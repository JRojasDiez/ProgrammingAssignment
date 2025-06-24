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