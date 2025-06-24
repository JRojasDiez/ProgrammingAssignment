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
# Calculate limits for current data color scale (5th and 95th percentiles)
combined_limits <- quantile(wijk_sf_joined$housing_scarcity, probs = c(0.05, 0.95), na.rm = TRUE)

# Plot current data heatmap with viridis plasma palette and clamped limits
housing_map <- ggplot(wijk_sf_joined) +
  geom_sf(aes(fill = housing_scarcity), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    limits = c(6, 13),
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
# Calculate limits for 2017 data color scale (5th and 95th percentiles)
scarcity_limits_b4 <- quantile(wijk_sf_joined_b4$housing_scarcity_b4, probs = c(0.05, 0.95), na.rm = TRUE)

# Plot 2017 data heatmap with viridis plasma palette and clamped limits
housing_map_b4 <- ggplot(wijk_sf_joined_b4) +
  geom_sf(aes(fill = housing_scarcity_b4), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    limits = c(6, 13),
    oob = scales::squish,
    
    na.value = "grey90",
    name = "Housing Scarcity\n(WOZ / Income)"
  ) +
  labs(
    title = "Housing Cost/Income Heatmap 2017",
    fill = "WOZ / Income"
  ) +
  theme_minimal()
print(housing_map_b4)
ggsave("Housing Cost Income Heatmap 2017.png", plot = housing_map_b4, width = 10, height = 8, dpi = 300)

ggsave("Housing Cost Income Heatmap Recent.png", plot = housing_map, width = 10, height = 8, dpi = 300)

