install.packages("viridis")

# Packages
library(readr)
library(dplyr)
library(stringr)
library(tidyverse)
library(scales)



# extracted from https://opendata.cbs.nl/#/CBS/nl/dataset/85618NED/table
# Load cleaned housing/student dataset
housing_data <- read_csv2("Echte kerncijfers.csv")

# Clean + combine columns

housing_data <- housing_data %>%
  filter(`Regioaanduiding/Soort regio (omschrijving)` == "Wijk") %>%
  mutate(
    wijk_code = str_extract(`Wijken en buurten`, "WK[0-9]+"),
    wijk_naam = str_trim(str_remove(`Wijken en buurten`, "\\s*\\(WK[0-9]+\\)")),
    aantal_inwoners = as.numeric(str_replace_all(`Bevolking/Aantal inwoners (aantal)`, "[^0-9]", "")),
    totaal_studenten = as.numeric(str_replace_all(`Onderwijs/Onderwijssoort/Studenten hbo (aantal)`, "[^0-9]", "")) +
      as.numeric(str_replace_all(`Onderwijs/Onderwijssoort/Studenten wo (aantal)`, "[^0-9]", ""))
  ) %>%
  select(wijk_code, wijk_naam, aantal_inwoners, totaal_studenten)

# implementing the geojson data
# from https://maps.amsterdam.nl/open_geodata/
# actual code : https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php?KAARTLAAG=INDELING_WIJK&THEMA=gebiedsindeling
library(sf)
# Joining datasets
geo_data <- st_read("geojson_lnglat.json")
map_data <- geo_data %>%
  left_join(housing_data, by = c("Wijk" = "wijk_naam"))

# Population density
map_data <- map_data %>%
  mutate(student_per_1000 = totaal_studenten / aantal_inwoners * 1000)

# Plot
student_quantiles <- quantile(map_data$student_per_1000, probs = c(0.05, 0.95), na.rm = TRUE)

student_map <- ggplot(map_data) +
  geom_sf(aes(fill = student_per_1000), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    limits = student_quantiles,
    oob = scales::squish,  # squish values outside limits to nearest color
    na.value = "grey90",
    name = "Stud/1000"
  ) +
  labs(
    title = "Studenten per 1.000 Inwoners per Wijk in Amsterdam",
    fill = "Stud/1000"
  ) +
  theme_minimal()
print(student_map)
ggsave("student_density_map_2023.png", plot = housing_map, width = 10, height = 8, dpi = 300)