
show_col_types = FALSE

## make continental europe non nuclear countries df - canada excluded for spatial weights
df_sp_host <- df_sp %>% 
  filter(!(country_name %in% c("France"))) %>% 
  filter(!(country_name %in% c("United States"))) %>% 
  filter(!(country_name %in% c("United Kingdom"))) %>%
  filter(!(country_name %in% c("Canada"))) 
  #filter(!(country_name %in% c("Montenegro"))) %>%
  #filter(!(country_name %in% c("North Macedonia"))) 
  #filter(!(country_name %in% c("Turkiye"))) %>% 
  #filter(!(country_name %in% c("Greece")))

#bit of cleaning
df_sp_host <- df_sp_host %>% 
  mutate(dca = if_else(country_name == "Netherlands", 1, dca)) %>%
  mutate(dca = if_else(country_name == "Greece", 1, dca))  %>%
  mutate(dca = if_else(country_name == "Latvia", 0, dca))  %>%
  mutate(East = if_else(joined_nato >= 1999, 1, 0)) %>%
  mutate(ln_ME_GDP = log(ME_GDP),
         ln_Population = log(Population))  %>%
  filter(in_nato == 1) %>%
  filter(year >= 2014)

#create distance to moscow
coords <- cbind(df_sp_host$caplong, df_sp_host$caplat)
capitals_df <- st_drop_geometry(df_sp_host)
capitals_df <- st_as_sf(capitals_df, coords = c("caplong", "caplat"), crs = 4326)
moscow_coords <- st_sfc(st_point(c(37.6173, 55.7558)), crs = 4326)
capitals_df$distance_to_moscow <- st_distance(capitals_df, moscow_coords)
df_sp_host$distance_to_moscow <- capitals_df$distance_to_moscow 
df_sp_host$distance_to_moscow <- df_sp_host$distance_to_moscow / 1000 ##in km
df_sp_host$distance_to_moscow <- as.numeric(df_sp_host$distance_to_moscow)
df_sp_host$inverse_distance_to_moscow <- 1 / df_sp_host$distance_to_moscow


df_sp_host <- st_sf(df_sp_host)

#### Correlation 

#Host countries
correlation_results_host <- vector("numeric", length = length(unique(df_sp_host$year)))
p_values_host <- vector("numeric", length = length(unique(df_sp_host$year)))

for (i in seq_along(unique(df_sp_host$year))) {
  df_year <- df_sp_host[df_sp_host$year == unique(df_sp_host$year)[i], ]

  cor_test_result <- cor.test(df_year$ln_ME_GDP, df_year$dca, 
                              method = "pearson")
  
  correlation_results_host[i] <- cor_test_result$estimate
  p_values_host[i] <- cor_test_result$p.value
}

correlation_table_host <- data.frame(
  Year = unique(df_sp_host$year),
  Correlation_Coefficient = correlation_results_host,
  P_Value = p_values_host
)

correlation_table_host

####

df_sp_host_ols <- df_sp_host %>%
  filter(year >= 2014)

ols_models <- list()
for (year in unique(df_sp_host_ols$year)) {
  df_year <- df_sp_host_ols[df_sp_host_ols$year == year, ] %>%
    filter(!is.na(ln_ME_GDP))
    lm_model <- lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_year)
  robust_se <- sqrt(diag(vcovHC(lm_model, type = "HC1")))
  lm_model$robust_se <- robust_se
    ols_models[[as.character(year)]] <- lm_model
}

####

##### Contiguity & Contiguity + US weights

nb_contiguity <- poly2nb(df_sp_host$geom_sp, queen = TRUE)

nbw_continguity <- nb2listw(nb_contiguity, style = "B", zero.policy = T)

####

#### Moran's I Year Test
df_sp2020 <- df_sp %>% 
  filter(!is.na(ME_GDP)) %>% 
  filter(year == 2020) 

nb_contiguity2020 <- poly2nb(df_sp2020$geom_sp, queen = TRUE)
nbw_contiguity2020 <- nb2listw(nb_contiguity2020, style = "B", zero.policy = T)

moran_contiguity <- moran.test(df_sp2020$ME_GDP, nbw_contiguity2020, 
                               alternative = "greater")
moran_contiguity

####

#### Contiguity Moran's I iterate 
moran_contiguity <- list()

for (year in unique(df_sp_host$year)) {
  df_year <- df_sp_host[df_sp_host$year == year, ]
  
  df_year <- df_year %>%
    filter(!is.na(ME_GDP))

  nb <- poly2nb(df_year$geom_sp, queen = TRUE)
  
  nbw <- nb2listw(nb, style = "B", zero.policy = TRUE)
  
  moran_contiguity <- moran.test(df_year$ln_ME_GDP, nbw, 
                                 alternative = "greater")

  cat("Year:", year, "\n")
  print(moran_contiguity)
  cat("\n")
  
  moran_contiguity[[as.character(year)]] <- moran_contiguity
}

####

#### Moran's I Inverse Distance Iterate

moran_results_inverse <- list()

for (year in unique(df_sp_host$year)) {
  df_year <- df_sp_host[df_sp_host$year == year, ]
  df_year <- df_year %>%
    filter(!is.na(ln_ME_GDP))
  
  coords <- cbind(df_year$caplong, df_year$caplat)
  k1 <- knn2nb(knearneigh(coords))
  critical_threshold <- max(unlist(nbdists(k1, coords)))
  nb.dist.band <- dnearneigh(coords, 0, critical_threshold)
  distances <- nbdists(nb.dist.band, coords)
  inverted_dists <- lapply(distances, function(x){1/x})
  nbw_inverted <- nb2listw(nb.dist.band, glist = inverted_dists, style = "B")
  
  moran_results_inverse <- moran.test(df_year$ln_ME_GDP, nbw_inverted, alternative = "greater")
 
  cat("Year:", year, "\n")
  print(moran_results_inverse)
  cat("\n")
  
  moran_results_inverse[[as.character(year)]] <- moran_results_inverse
}


####

str(df_sp_host)
df_sp_host2019 <- df_sp_host %>% 
  filter(!is.na(ME_GDP)) %>% 
  filter(year == 2019) 

df_centroids_2019 <- st_drop_geometry(df_sp_host2019)
df_centroids_2019 <- st_as_sf(df_centroids_2019, coords = c("caplong", "caplat"), crs = 4326)

coords <- cbind(df_sp_host2019$caplong, df_sp_host2019$caplat)

k1 <- knn2nb(knearneigh(coords))
critical.threshold <- max(unlist(nbdists(k1,coords)))

nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
distances <- nbdists(nb.dist.band,coords)
inverted_dists <- lapply(distances, function(x){1/x})

nbw_inverted19 <- nb2listw(nb.dist.band, glist = inverted_dists, style = "B", zero.policy = TRUE)

plot(st_geometry(df_sp_host2019), border = "lightgray")
plot.nb(nb.dist.band, st_geometry(df_centroids_2019), add = TRUE)

sar_2019_test <- lagsarlm(
  formula = lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_sp_host2019),
  data = df_sp_host2019,
  listw = nbw_inverted19, 
  zero.policy = TRUE
)

summary(sar_2019_test, Nagelkerke = TRUE)

###


####


#### Iterate SAR per year Inverse Distance

sar_results <- list()

for (year in unique(df_sp_host$year)) {
  df_year <- df_sp_host[df_sp_host$year == year, ]
  df_year <- df_year %>%
    filter(!is.na(ln_ME_GDP))
  
  coords <- cbind(df_year$caplong, df_year$caplat)
  
  k1 <- knn2nb(knearneigh(coords))
  critical_threshold <- max(unlist(nbdists(k1, coords)))
  
  nb.dist.band <- dnearneigh(coords, 0, critical_threshold)
  distances <- nbdists(nb.dist.band, coords)
  inverted_dists <- lapply(distances, function(x){1/x})
  
  nbw_inverted <- nb2listw(nb.dist.band, glist = inverted_dists, style = "B")
  
  # Run spatial regression
  sar_year <- lagsarlm(
    formula = lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_year),
    data = df_year,
    listw = nbw_inverted, 
    zero.policy = TRUE
  )
  
  moran_result <- moran.test(sar_year$residuals, nbw_inverted)
  impacts_result <- impacts(sar_year, listw = nbw_inverted, R = 100)

  sar_results[[as.character(year)]] <- list(
    sar_summary = summary(sar_year, Nagelkerke = TRUE),
    impacts = impacts_result,
    imp_summary = summary(impacts_result, zstats = TRUE)$pzmat,
    moran_test_result = moran_result
  )
  
}

for (year in names(sar_results)) {
  cat("Year:", year, "\n")
  cat("Summary of SAR:", "\n")
  print(sar_results[[year]]$sar_summary)
  cat("Impacts:", "\n")
  print(sar_results[[year]]$impacts)
  cat("Summary of Impacts:", "\n")
  print(sar_results[[year]]$imp_summary)
  cat("Moran's I:", "\n")
  print(sar_results[[year]]$moran_test_result)
}

####   

#### SAR Model Contiguity

sar_results_contiguity <- list()

for (year in unique(df_sp_host$year)) {
  df_year <- df_sp_host[df_sp_host$year == year, ]
  df_year <- df_year %>%
    filter(!is.na(ln_ME_GDP))
  nb <- poly2nb(df_year$geom_sp, queen = TRUE)
  nbw <- nb2listw(nb, style = "B", zero.policy = TRUE)
  
  # Run spatial regression
  sar_year <- lagsarlm(
    formula = lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_year),
    data = df_year,
    listw = nbw, 
    zero.policy = TRUE
  )
  
  moran_result <- moran.test(sar_year$residuals, nbw)
  
  impacts_result <- impacts(sar_year, listw = nbw, R = 100)
  
  sar_results_contiguity[[as.character(year)]] <- list(
    sar_summary = summary(sar_year, Nagelkerke = TRUE),
    impacts = impacts_result,
    imp_summary = summary(impacts_result, zstats = TRUE)$pzmat,
    moran_test_result = moran_result
  )
}

for (year in names(sar_results_contiguity)) {
  cat("Year:", year, "\n")
  cat("Summary of SAR:", "\n")
  print(sar_results_contiguity[[year]]$sar_summary)
  cat("Impacts:", "\n")
  print(sar_results_contiguity[[year]]$impacts)
  cat("Summary of Impacts:", "\n")
  print(sar_results_contiguity[[year]]$imp_summary)
  cat("Moran's I:", "\n")
  print(sar_results[[year]]$moran_test_result)

}

##  cat("Morans I:", "\n")
##print(sar_results_contiguity[[year]]$moran_test_result)

####


### Sar impacts contiguity 

sar_impacts_contiguity <- list()

for (year in unique(df_sp_host$year)) {
  df_year <- df_sp_host[df_sp_host$year == year, ]
  df_year <- df_year %>%
    filter(!is.na(ln_ME_GDP))
  nb <- poly2nb(df_year$geom_sp, queen = TRUE)
  nbw <- nb2listw(nb, style = "B", zero.policy = TRUE)
  
  # Run spatial regression
  sar_year <- lagsarlm(
    formula = lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_year),
    data = df_year,
    listw = nbw, 
    zero.policy = TRUE
  )
  
  impacts_result <- impacts(sar_year, listw = nbw, R = 100)
  
  sar_results_contiguity[[as.character(year)]] <- list(
    sar_summary = summary(sar_year, Nagelkerke = TRUE),
    impacts = impacts_result,
    imp_summary = summary(impacts_result, zstats = TRUE)$pzmat,
    moran_test_result = moran_result
  )
}


#### compare models inverse distance

for (year in unique(df_sp_host$year)) {
  df_year <- df_sp_host[df_sp_host$year == year, ]
  df_year <- df_year %>%
    filter(!is.na(ln_ME_GDP))
  coords <- cbind(df_year$caplong, df_year$caplat)
  k1 <- knn2nb(knearneigh(coords))
  critical_threshold <- max(unlist(nbdists(k1, coords)))
  nb.dist.band <- dnearneigh(coords, 0, critical_threshold)
  distances <- nbdists(nb.dist.band, coords)
  inverted_dists <- lapply(distances, function(x){1/x})
  nbw_inverted <- nb2listw(nb.dist.band, glist = inverted_dists, style = "B")
  
  sar_model <- lagsarlm(
    formula = lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_year),
    data = df_year,
    listw = nbw_inverted, 
    zero.policy = TRUE
  )
  
  sem_model <- errorsarlm(
    formula = lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_year),
    data = df_year,
    listw = nbw_inverted, 
    zero.policy = TRUE
  )
  
  slx_model <- lmSLX(
    formula = lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_year),
    data = df_year,
    listw = nbw_inverted, 
    zero.policy = TRUE
  )
  
  assign(paste0("sar_", year), sar_model)
  assign(paste0("sem_", year), sem_model)
  assign(paste0("slx_", year), slx_model)
}

AIC(slx_2015, sem_2015, sar_2015)

model1 <- lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_sp_host2019)

lm.RStests(
  model1,
  nbw_inverted19,
  test = c("LMerr", "LMlag")
)

summary(sar_2015)

#### create SAR individual for contiguity

for (year in unique(df_sp_host$year)) {
  df_year <- df_sp_host[df_sp_host$year == year, ]
  df_year <- df_year %>%
    filter(!is.na(ln_ME_GDP))
  nb <- poly2nb(df_year$geom_sp, queen = TRUE)
  nbw <- nb2listw(nb, style = "B", zero.policy = TRUE)
  
  # Run spatial regression
  sar_year <- lagsarlm(
    formula = lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_year),
    data = df_year,
    listw = nbw, 
    zero.policy = TRUE
  )
  
  assign(paste0("sar_contiguity_", year), sar_year)
}



