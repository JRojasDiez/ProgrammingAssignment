library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load Excel data
df <- read_excel("Book3.xlsx")

# Check columns (optional)
print(colnames(df))

# Load GeoJSON spatial data
wijk_sf <- st_read("geojson_lnglat.json", quiet = TRUE)

# Clean wijk names in both dataframes for matching
df <- df %>%
  mutate(wijk_clean = tolower(`Wijken en buurten`) %>% trimws())

wijk_sf <- wijk_sf %>%
  mutate(wijk_clean = tolower(Wijk) %>% trimws())

# Join data on cleaned wijk names
wijk_sf_joined <- wijk_sf %>%
  left_join(df, by = "wijk_clean")

# Calculate housing scarcity ratio: population / number of houses
wijk_sf_joined <- wijk_sf_joined %>%
  mutate(housing_scarcity = `Bevolking/Aantal inwoners (aantal)` / `Wonen/Woningvoorraad (aantal)`)

# Plot choropleth map
library(scales)  # for squish()

# Calculate quantile limits for housing_scarcity, ignoring NAs
scarcity_limits <- quantile(wijk_sf_joined$housing_scarcity, probs = c(0.05, 0.95), na.rm = TRUE)

# Plot with clamped color scale
common_breaks <- pretty(scarcity_limits, n = 5)


housing_scarcity_map <- ggplot(wijk_sf_joined) +
  geom_sf(aes(fill = housing_scarcity), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "yellow",
    high = "red",
    na.value = "grey90",
    limits = scarcity_limits,
    breaks = common_breaks,
    oob = scales::squish  # squish values outside limits to nearest limit
  ) +
  labs(
    title = "Housing Scarcity Ratio per Wijk 2023",
    fill = "Population / Houses"
  ) +
  theme_minimal()
print(housing_scarcity_map)
ggsave("housing_stress_map_2017.png", plot = housing_scarcity_map, width = 10, height = 8, dpi = 300)
