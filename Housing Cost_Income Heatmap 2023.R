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
  geom_sf(aes(fill = housing_scarcity), color = "white", size = 0.2) +  # white thin borders
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    limits = c(6, 13),             # use same limits as first plot for comparability
    oob = scales::squish,
    na.value = "grey90",
    name = "Housing Scarcity\n(WOZ / Income)"
  ) +
  labs(
    title = "Housing Cost/Income Heatmap 2023",
    fill = "WOZ / Income"
  ) +
  theme_minimal()

print(housing_map)
ggsave("housing_scarcity_map_2023.png", plot = housing_map, width = 10, height = 8, dpi = 300)