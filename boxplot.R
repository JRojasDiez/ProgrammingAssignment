library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)

file_path <- "data.xlsx"
sheets <- c("22", "21", "20", "19", "18", "17")

read_and_filter_sheet <- function(sheet_name, fixed_gemeentes = NULL) {
  df <- read_excel(file_path, sheet = sheet_name) %>%
    mutate(
      `Regioaanduiding/Soort regio (omschrijving)` = str_trim(`Regioaanduiding/Soort regio (omschrijving)`),
      `Wijken en buurten` = str_trim(`Wijken en buurten`)
    ) %>%
    filter(`Regioaanduiding/Soort regio (omschrijving)` == "Gemeente")
  
  # For this boxplot, don't filter on population, keep all gemeentes
  if (!is.null(fixed_gemeentes)) {
    df <- df %>% filter(`Wijken en buurten` %in% fixed_gemeentes)
  }
  
  year_num <- as.integer(paste0("20", sheet_name))
  df <- df %>% mutate(Year = year_num)
  
  return(df)
}

# Read all sheets without population filtering, keep all gemeenten
all_data <- map_dfr(sheets, ~ read_and_filter_sheet(.x))

# Calculate ratios
all_data <- all_data %>%
  mutate(
    pop_density_ratio = `Bevolking/Aantal inwoners (aantal)` / `Wonen/Woningvoorraad (aantal)`,
    woz_income_ratio = `Wonen/Gemiddelde WOZ-waarde van woningen (x 1 000 euro)` /
      `Inkomen/Inkomen van personen/Gemiddeld inkomen per inwoner  (x 1 000 euro)`
  ) %>%
  select(-starts_with("...")) %>%
  arrange(Year, `Wijken en buurten`)

# Calculate total population per gemeente
total_population <- all_data %>%
  group_by(`Wijken en buurten`) %>%
  summarise(total_pop = sum(`Bevolking/Aantal inwoners (aantal)`, na.rm = TRUE)) %>%
  arrange(desc(total_pop))

# Select top 5 gemeenten by population
top5_gemeentes <- total_population %>%
  slice_head(n = 5) %>%
  pull(`Wijken en buurten`)

# Filter data for top 5 gemeenten
top5_data <- all_data %>%
  filter(`Wijken en buurten` %in% top5_gemeentes)

# Plot boxplot of Housing Pressure (pop_density_ratio) for top 5 gemeenten
p <- ggplot(top5_data, aes(x = `Wijken en buurten`, y = pop_density_ratio, fill = `Wijken en buurten`)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Housing Pressure (Population per Housing Unit) - Top 5 Gemeentes by Population",
    x = "Gemeente",
    y = "Population per Housing Unit"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("housing_pressure_top5.png", plot = p, width = 10, height = 6, dpi = 300)

