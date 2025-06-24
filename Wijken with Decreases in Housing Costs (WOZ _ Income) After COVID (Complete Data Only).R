library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

file_path <- "data.xlsx"
sheets <- c("22", "21", "20", "19", "18", "17")

# --- Step 1: Read data with cleaned column names ---
read_data <- function(sheet_name) {
  df <- read_excel(file_path, sheet = sheet_name)
  
  names(df) <- stringr::str_squish(names(df))
  
  df <- df %>%
    mutate(
      `Regioaanduiding/Soort regio (omschrijving)` = str_trim(`Regioaanduiding/Soort regio (omschrijving)`),
      `Wijken en buurten` = str_trim(`Wijken en buurten`),
      Year = as.integer(paste0("20", sheet_name))
    ) %>%
    filter(
      `Regioaanduiding/Soort regio (omschrijving)` == "Wijk"
    ) %>%
    rename(
      inwoners = `Bevolking/Aantal inwoners (aantal)`,
      woz_waarde = `Wonen/Gemiddelde WOZ-waarde van woningen (x 1 000 euro)`,
      inkomen = `Inkomen/Inkomen van personen/Gemiddeld inkomen per inwoner (x 1 000 euro)`
    ) %>%
    select(
      Year,
      `Wijken en buurten`,
      inwoners,
      woz_waarde,
      inkomen
    )
  return(df)
}

# --- Step 2: Combine all sheets ---
all_data <- map_dfr(sheets, read_data)

# --- Step 3: Calculate housing affordability ratio ---
all_data <- all_data %>%
  mutate(
    woz_income_ratio = woz_waarde / inkomen
  )

# --- Step 4: Find wijken with decrease in WOZ/Income after COVID ---
wijk_changes <- all_data %>%
  filter(Year %in% c(2017:2022)) %>%
  group_by(`Wijken en buurten`, period = if_else(Year < 2020, "before", "after")) %>%
  summarise(avg_ratio = mean(woz_income_ratio, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = avg_ratio) %>%
  mutate(change = after - before) %>%
  filter(!is.na(change)) %>%
  arrange(change)

wijken_decreasing <- wijk_changes %>%
  filter(change < 0) %>%
  arrange(change) %>%
  slice_head(n = 15) %>%
  pull(`Wijken en buurten`)

# --- Step 5: Ensure Amsterdam wijk is included ---
if(!"Amsterdam" %in% wijken_decreasing){
  wijken_decreasing <- c("Amsterdam", wijken_decreasing)
}

# --- Step 6: Filter data for selected wijken ---
plot_data <- all_data %>%
  filter(`Wijken en buurten` %in% wijken_decreasing) %>%
  mutate(
    label = `Wijken en buurten`,
    is_amsterdam = (label == "Amsterdam")
  )

# --- Step 7: Filter to keep only wijken with complete data ---
complete_wijken <- plot_data %>%
  group_by(label) %>%
  filter(!any(is.na(woz_income_ratio) | !is.finite(woz_income_ratio))) %>%
  ungroup() %>%
  distinct(label) %>%
  pull(label)

plot_data_complete <- plot_data %>%
  filter(label %in% complete_wijken)

# Get the exact unique labels present in the filtered data
unique_labels <- sort(unique(plot_data_complete$label))

# Create color palette for exactly these labels
wijk_colors <- setNames(
  colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(unique_labels)),
  unique_labels
)

# Set Amsterdam color only if it's in the final labels
if ("Amsterdam" %in% unique_labels) {
  wijk_colors["Amsterdam"] <- "black"
}

# Line types
line_types <- rep("solid", length(unique_labels))
names(line_types) <- unique_labels
if ("Amsterdam" %in% unique_labels) {
  line_types["Amsterdam"] <- "dashed"
}

# Line sizes
line_sizes <- rep(0.8, length(unique_labels))
names(line_sizes) <- unique_labels
if ("Amsterdam" %in% unique_labels) {
  line_sizes["Amsterdam"] <- 1.5
}


# --- Step 10: Plot ---
plot_housing_wijken <- ggplot(plot_data_complete, aes(x = Year, y = woz_income_ratio, group = label, color = label)) +
  geom_line(aes(linetype = label, size = label)) +
  geom_point(size = 2) +
  scale_color_manual(values = wijk_colors) +
  scale_linetype_manual(values = line_types) +
  scale_size_manual(values = line_sizes, guide = "none") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 2020, y = max(plot_data_complete$woz_income_ratio, na.rm = TRUE), label = "COVID Start", color = "red", vjust = -0.5, hjust = 1) +
  labs(
    title = "Wijken with Decreases in Housing Costs (WOZ / Income) After COVID (Complete Data Only)",
    x = "Year",
    y = "WOZ Value / Average Income",
    color = "Wijken",
    linetype = "Wijken"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(plot_housing_wijken)

ggsave(
  filename = "wijken_housing_affordability_decrease.png",
  plot = plot_housing_wijken,
  width = 10,      # width in inches
  height = 7,      # height in inches
  dpi = 300        # resolution
)