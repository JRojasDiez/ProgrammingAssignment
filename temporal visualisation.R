library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

file_path <- "data.xlsx"
sheets <- c("22", "21", "20", "19", "18", "17")

read_and_filter_sheet <- function(sheet_name, fixed_gemeentes = NULL) {
  df <- read_excel(file_path, sheet = sheet_name) %>%
    mutate(
      `Regioaanduiding/Soort regio (omschrijving)` = str_trim(`Regioaanduiding/Soort regio (omschrijving)`),
      `Wijken en buurten` = str_trim(`Wijken en buurten`)
    ) %>%
    filter(`Regioaanduiding/Soort regio (omschrijving)` == "Gemeente")
  
  if (sheet_name == "22") {
    df <- df %>% filter(`Bevolking/Aantal inwoners (aantal)` > 150000)
  } else if (!is.null(fixed_gemeentes)) {
    df <- df %>% filter(`Wijken en buurten` %in% fixed_gemeentes)
  }
  
  year_num <- as.integer(paste0("20", sheet_name))
  df <- df %>% mutate(Year = year_num)
  
  return(df)
}

df_22 <- read_and_filter_sheet("22")
gemeentes_22 <- df_22$`Wijken en buurten`

all_data <- map_dfr(sheets, function(sheet) {
  if (sheet == "22") {
    read_and_filter_sheet(sheet)
  } else {
    read_and_filter_sheet(sheet, fixed_gemeentes = gemeentes_22)
  }
})

all_data <- all_data %>%
  mutate(
    pop_density_ratio = `Bevolking/Aantal inwoners (aantal)` / `Wonen/Woningvoorraad (aantal)`,
    woz_income_ratio = `Wonen/Gemiddelde WOZ-waarde van woningen (x 1 000 euro)` /
      `Inkomen/Inkomen van personen/Gemiddeld inkomen per inwoner  (x 1 000 euro)`
  ) %>%
  select(-starts_with("...")) %>%
  arrange(Year, `Wijken en buurten`)

student_data <- tibble(
  Year_label = c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23"),
  HBO = c(452.7, 455.7, 463.3, 489.3, 491.4, 476.8),
  WO = c(280, 294.7, 306.9, 331.4, 344.6, 344.1)
) %>%
  mutate(Year = as.integer(substr(Year_label, 1, 4))) %>%
  mutate(Total_Students = HBO + WO) %>%
  select(Year, Total_Students)

all_data_with_students <- all_data %>%
  left_join(student_data, by = "Year")

# Calculate scale factor for students to fit WOZ y-axis scale
range_ratio <- range(all_data_with_students$woz_income_ratio, na.rm = TRUE)
range_students <- range(all_data_with_students$Total_Students, na.rm = TRUE)
scale_factor <- diff(range_ratio) / diff(range_students)

# Prepare plotting dataframe with unified y column
all_data_with_students <- all_data_with_students %>%
  mutate(
    group = as.character(`Wijken en buurten`),
    group = ifelse(is.na(group), "Total Students", group),
    y_plot = ifelse(group == "Total Students", Total_Students * scale_factor, woz_income_ratio)
  )

num_gemeentes <- length(gemeentes_22)
my_colors <- colorRampPalette(brewer.pal(12, "Set3"))(num_gemeentes)
colors <- c(setNames(my_colors, gemeentes_22), "Total Students" = "black")

linetypes <- c(setNames(rep("solid", num_gemeentes), gemeentes_22), "Total Students" = "dashed")
shapes <- c(setNames(rep(16, num_gemeentes), gemeentes_22), "Total Students" = 17)

ggplot(all_data_with_students, aes(x = Year, y = y_plot, color = group, linetype = group, shape = group, group = group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    name = "WOZ Value / Average Income",
    sec.axis = sec_axis(~ . / scale_factor, name = "Total Number of Students (x1000)")
  ) +
  scale_color_manual(values = colors, breaks = names(colors)) +
  scale_linetype_manual(values = linetypes, breaks = names(colors)) +
  scale_shape_manual(values = shapes, breaks = names(colors)) +
  labs(
    title = "WOZ Value to Income Ratio Over Time + Total Students",
    color = "Gemeente",
    linetype = "Gemeente",
    shape = "Gemeente"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

library(ggplot2)
library(patchwork)

# Your original WOZ Income Ratio plot
plot1 <- ggplot(all_data, aes(x = Year, y = woz_income_ratio, color = `Wijken en buurten`)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "WOZ Value to Income Ratio Over Time",
    x = "Year",
    y = "WOZ Value / Average Income",
    color = "Gemeente"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Student data plot
plot2 <- ggplot(student_data, aes(x = Year, y = Total_Students)) +
  geom_line(color = "black", size = 1, linetype = "dashed") +
  geom_point(color = "black", size = 2) +
  labs(
    title = "Total Number of Students in HBO + WO Over Time",
    x = "Year",
    y = "Number of Students (x 1000)"
  ) +
  theme_minimal()

# Combine and print plots
plot1 + plot2 + plot_layout(ncol = 2, widths = c(2, 1))
