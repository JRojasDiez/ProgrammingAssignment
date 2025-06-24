library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

file_path <- "data.xlsx"
sheets <- c("22", "21", "20", "19", "18", "17")

# Function to read data filtered for 'Gemeente' and specifically Amsterdam
read_gemeente_amsterdam <- function(sheet_name) {
  df <- read_excel(file_path, sheet = sheet_name)
  
  names(df) <- stringr::str_squish(names(df))
  
  df <- df %>%
    mutate(
      `Regioaanduiding/Soort regio (omschrijving)` = str_trim(`Regioaanduiding/Soort regio (omschrijving)`),
      `Regioaanduiding/Gemeentenaam (naam)` = str_trim(`Regioaanduiding/Gemeentenaam (naam)`),
      Year = as.integer(paste0("20", sheet_name))
    ) %>%
    filter(
      `Regioaanduiding/Soort regio (omschrijving)` == "Gemeente",
      `Regioaanduiding/Gemeentenaam (naam)` == "Amsterdam"
    ) %>%
    rename(
      inwoners = `Bevolking/Aantal inwoners (aantal)`,
      woz_waarde = `Wonen/Gemiddelde WOZ-waarde van woningen (x 1 000 euro)`,
      inkomen = `Inkomen/Inkomen van personen/Gemiddeld inkomen per inwoner (x 1 000 euro)`
    ) %>%
    select(
      Year,
      inwoners,
      woz_waarde,
      inkomen
    ) %>%
    mutate(
      woz_income_ratio = woz_waarde / inkomen,
      label = "Amsterdam (Gemeente)"
    )
  
  return(df)
}

# Read and combine all sheets for Amsterdam gemeente
amsterdam_data <- purrr::map_dfr(sheets, read_gemeente_amsterdam)

# Plot Amsterdam WOZ/Income trend
plot_amsterdam <- ggplot(amsterdam_data, aes(x = Year, y = woz_income_ratio)) +
  geom_line(color = "black", size = 1.5, linetype = "dashed") +
  geom_point(color = "black", size = 3) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 2020, y = max(amsterdam_data$woz_income_ratio, na.rm = TRUE), 
           label = "COVID Start", color = "red", vjust = -0.5, hjust = 1) +
  labs(
    title = "Housing Affordability (WOZ / Income) in Amsterdam (Gemeente)",
    x = "Year",
    y = "WOZ Value / Average Income"
  ) +
  theme_minimal()

print(plot_amsterdam)
ggsave(
  filename = "Amsterdam_Gemeente_WOZ_Income_Trend.png",
  plot = plot_amsterdam,
  width = 8,
  height = 6,
  dpi = 300
)