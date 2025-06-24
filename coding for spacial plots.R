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
ggsave("student_density_map_2023.png", plot = housing_map, width = 10, height = 8, dpi = 300)

             
# Moving on to cost of housing in each wijk

affordability_data <- read_excel("vdf.xlsx")
colnames(affordability_data)

# merging old wijken with new ones 
# https://onderzoek.amsterdam.nl/artikel/nieuwe-indeling-naar-wijken-en-buurten-2022

library(sf)
library(dplyr)
library(readxl)
library(stringdist)
library(tidyr)
library(ggplot2)
library(viridis)  # for plasma palette

# Step 1: Load data
old_data <- readxl::read_excel("vdf.xlsx")
wijk_sf <- sf::st_read("geojson_lnglat.json", quiet = TRUE)

# Step 2: Clean wijk names (lowercase, trimmed)
old_data <- old_data %>%
  mutate(old_wijk_clean = tolower(trimws(`Wijken en buurten`)))

wijk_sf <- wijk_sf %>%
  mutate(new_wijk_clean = tolower(trimws(Wijk)))

# Step 3: Manual mapping for splits/renames with replication
manual_mapping <- tibble(
  old_wijk = c(
    "slotermeer zuidwest",
    "bijlmer centrum (d,f,h)",
    "bijlmer oost (e,g,k)",
    "holendrecht/reigersbos"
  ),
  new_wijken = list(
    c("slotermeer-zuidoost", "slotermeer-west"),
    c("venserpolder", "amsterdamse poort e.o.", "h-buurt"),
    c("ganzenhoef e.o.", "geerdinkhof/kantershof", "bijlmermuseum", "k-buurt"),
    c("holendrecht", "reigersbos")
  )
)

mapping_expanded <- manual_mapping %>%
  unnest(new_wijken) %>%
  rename(old_wijk_clean = old_wijk, new_wijk_clean = new_wijken)

# Step 4: Identify unmapped wijken for fuzzy matching
new_wijken_unmapped <- wijk_sf %>%
  filter(!(new_wijk_clean %in% mapping_expanded$new_wijk_clean)) %>%
  distinct(new_wijk_clean)

old_wijken_unmapped <- old_data %>%
  filter(!(old_wijk_clean %in% mapping_expanded$old_wijk_clean)) %>%
  distinct(old_wijk_clean)

# Step 5: Fuzzy matching
dist_mat <- stringdistmatrix(new_wijken_unmapped$new_wijk_clean, old_wijken_unmapped$old_wijk_clean, method = "jw")

best_matches <- apply(dist_mat, 1, function(x) {
  idx <- which.min(x)
  c(old_wijk_clean = old_wijken_unmapped$old_wijk_clean[idx], dist = x[idx])
})

fuzzy_mapping <- tibble(
  new_wijk_clean = new_wijken_unmapped$new_wijk_clean,
  old_wijk_clean = unlist(best_matches["old_wijk_clean", ]),
  dist = as.numeric(unlist(best_matches["dist", ]))
) %>%
  filter(dist < 0.15) %>%
  select(old_wijk_clean, new_wijk_clean)

# Step 6: Combine manual and fuzzy mappings
combined_mapping <- bind_rows(mapping_expanded %>% select(old_wijk_clean, new_wijk_clean), fuzzy_mapping)

# Step 7: Replicate old wijk data for each new wijk
old_data_mapped <- old_data %>%
  inner_join(combined_mapping, by = "old_wijk_clean") %>%
  select(-old_wijk_clean)

# Step 8: Join spatial data with mapped data
wijk_sf_joined <- wijk_sf %>%
  inner_join(old_data_mapped, by = "new_wijk_clean")

# Step 9: Calculate housing scarcity ratio
wijk_sf_joined <- wijk_sf_joined %>%
  mutate(
    WOZ_value = `WOZ waarde`,
    Income = Income,
    housing_scarcity = WOZ_value / Income
  )

# Step 10: Plot with viridis plasma palette, white borders, thin lines
# Combine both housing scarcity vectors (ignoring NAs)
all_values <- c(wijk_sf_joined$housing_scarcity, wijk_sf_joined_b4$housing_scarcity_b4)

# Calculate 5th and 95th percentiles to clamp the color scale (adjust if needed)
combined_limits <- quantile(all_values, probs = c(0.05, 0.95), na.rm = TRUE)

# Recent data plot with clamped limits
# Combine both housing scarcity vectors (ignoring NAs)
all_values <- c(wijk_sf_joined$housing_scarcity, wijk_sf_joined_b4$housing_scarcity_b4)

# Calculate 5th and 95th percentiles to clamp the color scale (adjust if needed)
combined_limits <- quantile(all_values, probs = c(0.05, 0.95), na.rm = TRUE)

# Recent data plot with inverted gradient and clamped limits
housing_map <- ggplot(wijk_sf_joined) +
  geom_sf(aes(fill = housing_scarcity), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "yellow",    # low scarcity = yellow
    high = "blue",     # high scarcity = blue
    limits = combined_limits,
    oob = scales::squish,
    na.value = "grey90",
    name = "Housing Scarcity\n(WOZ / Income)"
  ) +
  labs(
    title = "Housing Cost/Income Heatmap of Amsterdam Wijken",
    fill = "WOZ / Income"
  ) +
  theme_minimal()

ggsave("housing_scarcity_map_2023.png", plot = housing_map, width = 10, height = 8, dpi = 300)

#Calculate combined limits based on both datasets
all_values <- c(wijk_sf_joined$housing_scarcity, wijk_sf_joined_b4$housing_scarcity_b4)
combined_limits <- quantile(all_values, probs = c(0.05, 0.95), na.rm = TRUE)

##2017
# Load data from book4.xlsx
old_data_b4 <- readxl::read_excel("book4.xlsx")
wijk_sf_b4 <- sf::st_read("geojson_lnglat.json", quiet = TRUE)

# Clean wijk names
old_data_b4 <- old_data_b4 %>%
  mutate(old_wijk_clean_b4 = tolower(trimws(`Wijken en buurten`)))

wijk_sf_b4 <- wijk_sf_b4 %>%
  mutate(new_wijk_clean_b4 = tolower(trimws(Wijk)))

# Manual mapping for splits/renames with replication
manual_mapping_b4 <- tibble(
  old_wijk_b4 = c(
    "slotermeer zuidwest",
    "bijlmer centrum (d,f,h)",
    "bijlmer oost (e,g,k)",
    "holendrecht/reigersbos"
  ),
  new_wijken_b4 = list(
    c("slotermeer-zuidoost", "slotermeer-west"),
    c("venserpolder", "amsterdamse poort e.o.", "h-buurt"),
    c("ganzenhoef e.o.", "geerdinkhof/kantershof", "bijlmermuseum", "k-buurt"),
    c("holendrecht", "reigersbos")
  )
)

mapping_expanded_b4 <- manual_mapping_b4 %>%
  unnest(new_wijken_b4) %>%
  rename(old_wijk_clean_b4 = old_wijk_b4, new_wijk_clean_b4 = new_wijken_b4)

# Identify unmapped wijken for fuzzy matching
new_wijken_unmapped_b4 <- wijk_sf_b4 %>%
  filter(!(new_wijk_clean_b4 %in% mapping_expanded_b4$new_wijk_clean_b4)) %>%
  distinct(new_wijk_clean_b4)

old_wijken_unmapped_b4 <- old_data_b4 %>%
  filter(!(old_wijk_clean_b4 %in% mapping_expanded_b4$old_wijk_clean_b4)) %>%
  distinct(old_wijk_clean_b4)

# Fuzzy matching
dist_mat_b4 <- stringdistmatrix(new_wijken_unmapped_b4$new_wijk_clean_b4, old_wijken_unmapped_b4$old_wijk_clean_b4, method = "jw")

best_matches_b4 <- apply(dist_mat_b4, 1, function(x) {
  idx <- which.min(x)
  c(old_wijk_clean_b4 = old_wijken_unmapped_b4$old_wijk_clean_b4[idx], dist = x[idx])
})

fuzzy_mapping_b4 <- tibble(
  new_wijk_clean_b4 = new_wijken_unmapped_b4$new_wijk_clean_b4,
  old_wijk_clean_b4 = unlist(best_matches_b4["old_wijk_clean_b4", ]),
  dist = as.numeric(unlist(best_matches_b4["dist", ]))
) %>%
  filter(dist < 0.15) %>%
  select(old_wijk_clean_b4, new_wijk_clean_b4)

# Combine manual and fuzzy mappings
combined_mapping_b4 <- bind_rows(mapping_expanded_b4 %>% select(old_wijk_clean_b4, new_wijk_clean_b4), fuzzy_mapping_b4)

# Replicate old wijk data for each new wijk
old_data_mapped_b4 <- old_data_b4 %>%
  inner_join(combined_mapping_b4, by = "old_wijk_clean_b4") %>%
  select(-old_wijk_clean_b4)

# Join spatial data with mapped data
wijk_sf_joined_b4 <- wijk_sf_b4 %>%
  inner_join(old_data_mapped_b4, by = "new_wijk_clean_b4")

# Calculate housing scarcity ratio
wijk_sf_joined_b4 <- wijk_sf_joined_b4 %>%
  mutate(
    WOZ_value_b4 = `WOZ waarde`,
    Income_b4 = Income,
    housing_scarcity_b4 = WOZ_value_b4 / Income_b4
  )

# Calculate quantile limits for color scaling
scarcity_limits_b4 <- quantile(wijk_sf_joined_b4$housing_scarcity_b4, probs = c(0.05, 0.95), na.rm = TRUE)

# Plot heatmap with viridis plasma palette and clamped limits
housing_map_b4 <- ggplot(wijk_sf_joined_b4) +
  geom_sf(aes(fill = housing_scarcity_b4), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "yellow",    # low scarcity = yellow
    high = "blue",     # high scarcity = blue
    limits = combined_limits,
    oob = scales::squish,
    na.value = "grey90",
    name = "Housing Scarcity\n(WOZ / Income)"
  ) +
  labs(
    title = "Housing Cost/Income Heatmap 2017",
    fill = "WOZ / Income"
  ) +
  theme_minimal()


# Display plots
print(housing_map)
print(housing_map_b4)


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

##2017
library(sf)
library(dplyr)
library(readxl)
library(stringdist)
library(tidyr)
library(ggplot2)
library(viridis)  # for plasma palette

# Step 1: Load data
hm_old_data <- readxl::read_excel("book4.xlsx")
hm_wijk_sf <- sf::st_read("geojson_lnglat.json", quiet = TRUE)

# Step 2: Clean wijk names (lowercase, trimmed)
hm_old_data <- hm_old_data %>%
  mutate(hm_old_wijk_clean = tolower(trimws(`Wijken en buurten`)))

hm_wijk_sf <- hm_wijk_sf %>%
  mutate(hm_new_wijk_clean = tolower(trimws(Wijk)))

# Step 3: Manual mapping for splits/renames with replication
hm_manual_mapping <- tibble(
  hm_old_wijk = c(
    "slotermeer zuidwest",
    "bijlmer centrum (d,f,h)",
    "bijlmer oost (e,g,k)",
    "holendrecht/reigersbos"
  ),
  hm_new_wijken = list(
    c("slotermeer-zuidoost", "slotermeer-west"),
    c("venserpolder", "amsterdamse poort e.o.", "h-buurt"),
    c("ganzenhoef e.o.", "geerdinkhof/kantershof", "bijlmermuseum", "k-buurt"),
    c("holendrecht", "reigersbos")
  )
)

hm_mapping_expanded <- hm_manual_mapping %>%
  unnest(hm_new_wijken) %>%
  rename(hm_old_wijk_clean = hm_old_wijk, hm_new_wijk_clean = hm_new_wijken)

# Step 4: Identify unmapped wijken for fuzzy matching
hm_new_wijken_unmapped <- hm_wijk_sf %>%
  filter(!(hm_new_wijk_clean %in% hm_mapping_expanded$hm_new_wijk_clean)) %>%
  distinct(hm_new_wijk_clean)

hm_old_wijken_unmapped <- hm_old_data %>%
  filter(!(hm_old_wijk_clean %in% hm_mapping_expanded$hm_old_wijk_clean)) %>%
  distinct(hm_old_wijk_clean)

# Step 5: Fuzzy matching
hm_dist_mat <- stringdistmatrix(hm_new_wijken_unmapped$hm_new_wijk_clean, hm_old_wijken_unmapped$hm_old_wijk_clean, method = "jw")

hm_best_matches <- apply(hm_dist_mat, 1, function(x) {
  idx <- which.min(x)
  c(hm_old_wijk_clean = hm_old_wijken_unmapped$hm_old_wijk_clean[idx], dist = x[idx])
})

hm_fuzzy_mapping <- tibble(
  hm_new_wijk_clean = hm_new_wijken_unmapped$hm_new_wijk_clean,
  hm_old_wijk_clean = unlist(hm_best_matches["hm_old_wijk_clean", ]),
  dist = as.numeric(unlist(hm_best_matches["dist", ]))
) %>%
  filter(dist < 0.15) %>%
  select(hm_old_wijk_clean, hm_new_wijk_clean)

# Step 6: Combine manual and fuzzy mappings
hm_combined_mapping <- bind_rows(hm_mapping_expanded %>% select(hm_old_wijk_clean, hm_new_wijk_clean), hm_fuzzy_mapping)

# Step 7: Replicate old wijk data for each new wijk
hm_old_data_mapped <- hm_old_data %>%
  inner_join(hm_combined_mapping, by = c("hm_old_wijk_clean" = "hm_old_wijk_clean")) %>%
  select(-hm_old_wijk_clean)

# Step 8: Join spatial data with mapped data
hm_wijk_sf_joined <- hm_wijk_sf %>%
  inner_join(hm_old_data_mapped, by = c("hm_new_wijk_clean" = "hm_new_wijk_clean"))

# Step 9: Calculate housing scarcity ratio = population / number of houses
hm_wijk_sf_joined <- hm_wijk_sf_joined %>%
  mutate(
    Population = `Bevolking/Aantal inwoners (aantal)`,
    Houses = `Wonen/Woningvoorraad (aantal)`,
    housing_scarcity = Population / Houses
  )

library(scales)  # for squish()

# Calculate quantile limits for housing_scarcity, ignoring NAs
hm_scarcity_limits <- quantile(hm_wijk_sf_joined$housing_scarcity, probs = c(0.05, 0.95), na.rm = TRUE)

# Plot with clamped yellow-red gradient
hm_housing_map <- ggplot(hm_wijk_sf_joined) +
  geom_sf(aes(fill = housing_scarcity), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "yellow",
    high = "red",
    na.value = "grey90",
    limits = hm_scarcity_limits,
    breaks = common_breaks,
    oob = scales::squish  # squish values outside limits to nearest limit
  ) +
  labs(
    title = "Housing Scarcity Ratio per Wijk 2017",
    fill = "Population / Houses"
  ) +
  theme_minimal()

print(hm_housing_map)
ggsave("housing_stress_map_2017.png", plot = hm_housing_map, width = 10, height = 8, dpi = 300)

# Show the plot
print(hm_housing_map)

