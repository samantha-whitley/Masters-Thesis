df_sp <- df_sp %>% 
  mutate(dca = if_else(country_name == "Netherlands", 1, dca)) %>%
  mutate(dca = if_else(country_name == "Greece", 1, dca))  %>%
  mutate(dca = if_else(country_name == "Latvia", 0, dca))  %>%
  mutate(East = if_else(joined_nato >= 1999, 1, 0)) %>%
  mutate(ln_ME_GDP = log(ME_GDP),
         ln_Population = log(Population))  %>%
  filter(year >= 2014) %>%
  rename(democracy = 'Political regime')

## make continental europe non nuclear countries df - canada excluded for spatial weights
df_sp_host <- df_sp %>% 
  filter(!(country_name %in% c("France"))) %>% 
  filter(!(country_name %in% c("United States"))) %>% 
  filter(!(country_name %in% c("United Kingdom"))) %>%
  filter(!(country_name %in% c("Canada"))) 

#bit of cleaning

# create lagged ME
df_sp_host <- df_sp_host[order(df_sp_host$country_name, df_sp_host$year), ]
lag_by_country <- function(x) c(NA, head(x, -1))
df_sp_host$ln_ME_GDP_lag <- unlist(with(df_sp_host, ave(ln_ME_GDP, country_name, FUN = lag_by_country)))

df_sp_host <- df_sp_host %>%   
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

df_sp_host <- df_sp_host %>%
  mutate(log_inverse_distance_to_moscow = log(inverse_distance_to_moscow))
df_sp_host <- df_sp_host %>%
  mutate(log_distance_to_moscow = log(distance_to_moscow))

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
for (year in unique(df_sp_host$year)) {
  df_year <- df_sp_host[df_sp_host$year == year, ] %>%
    filter(!is.na(ln_ME_GDP))
    lm_model <- lm(ln_ME_GDP ~ dca + ln_Population + East, data = df_year)
  robust_se <- sqrt(diag(vcovHC(lm_model, type = "HC1")))
  lm_model$robust_se <- robust_se
    ols_models[[as.character(year)]] <- lm_model
}

modelsummary(ols_models,
             stars = TRUE)
####

##### Contiguity & Contiguity + US weights

nb_contiguity <- poly2nb(df_sp_host$geom_sp, queen = TRUE)

nbw_continguity <- nb2listw(nb_contiguity, style = "B", zero.policy = T)

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
  filter(year == 2016) 

df_centroids_2019 <- st_drop_geometry(df_sp_host2019)
df_centroids_2019 <- st_as_sf(df_centroids_2019, coords = c("caplong", "caplat"), crs = 4326)

coords <- cbind(df_sp_host2019$caplong, df_sp_host2019$caplat)

k1 <- knn2nb(knearneigh(coords))
critical.threshold <- max(unlist(nbdists(k1,coords)))

nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
distances <- nbdists(nb.dist.band,coords)
inverted_dists <- lapply(distances, function(x){1/x})

nbw_inverted19 <- nb2listw(nb.dist.band, glist = inverted_dists, style = "B", zero.policy = TRUE)

plot(st_geometry(df_sp), border = "lightgray")
plot.nb(nb.dist.band, st_geometry(df_centroids_2019), add = TRUE)

sar_2019_test <- lagsarlm(
  formula = lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_sp_host2019),
  data = df_sp_host2019,
  listw = nbw_inverted19, 
  zero.policy = TRUE
)

summary(sar_2019_test, Nagelkerke = TRUE)

modelsummary(sar_2019_test,
             stars = TRUE
             )
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
  nbw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Run spatial regression
  sar_year <- lagsarlm(
    formula = lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_year),
    data = df_year,
    listw = nbw, 
    zero.policy = TRUE
  )
  
  assign(paste0("sar_contiguity", year), sar_year)
}

#### spatial panel

panel <- pdata.frame(df_sp_host)
panel <- st_as_sf(panel)

df_sp_host <- df_sp_host %>%
  filter(year <= 2020)

df_sp_no_host1 <- df_sp_host %>%
  filter(!(country_name %in% c("Italy"))) |>
  filter(!(country_name %in% c("Germany"))) |>
  filter(!(country_name %in% c("Belgiun"))) |>
  filter(!(country_name %in% c("Greece"))) |>
  filter(!(country_name %in% c("Netherlands"))) |>
  filter(!(country_name %in% c("Turkey"))) |>
  filter(!(country_name %in% c("Norway"))) |>
  filter(year <= 2020)

df_sp_no_host <- df_sp_host %>%
  filter(!(country_name %in% c("Italy"))) |>
  filter(!(country_name %in% c("Germany"))) |>
  filter(!(country_name %in% c("Belgiun"))) |>
  filter(!(country_name %in% c("Greece"))) |>
  filter(!(country_name %in% c("Netherlands"))) |>
  filter(!(country_name %in% c("Turkey"))) |>
  filter(year <= 2020)

##weights
df_sp_host2019 <- df_sp_host %>% 
  filter(year == 2019) 

df_sp_no_host2019 <- df_sp_no_host %>% 
  filter(year == 2019) 

df_sp_host1_2019 <- df_sp_host %>% 
  filter(year == 2019) 

df_sp_no_host1_2019 <- df_sp_no_host1 %>% 
  filter(year == 2019) 
 
#contiguity weights w/ host
nb <- poly2nb(df_sp_host1_2019, queen = TRUE)
nbw <- nb2listw(nb, style = "B", zero.policy = TRUE)

# contiguity no hosts
nb_no_19 <- poly2nb(df_sp_no_host1_2019, queen = TRUE)
nbw_no_19 <- nb2listw(nb, style = "B", zero.policy = TRUE)

# inverse distance weights w/ host
df_centroids_2019 <- st_drop_geometry(df_sp_host2019)
df_centroids_2019 <- st_as_sf(df_centroids_2019, coords = c("caplong", "caplat"), crs = 4326)
coords <- cbind(df_sp_host2019$caplong, df_sp_host2019$caplat)
k1 <- knn2nb(knearneigh(coords))
critical.threshold <- max(unlist(nbdists(k1,coords)))
nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
distances <- nbdists(nb.dist.band,coords)
inverted_dists <- lapply(distances, function(x){1/x})
nbw_inverted19 <- nb2listw(nb.dist.band, glist = inverted_dists, style = "B", zero.policy = TRUE)


#inverse distance weights w/out host
df_centroids_2019_no <- st_drop_geometry(df_sp_no_host2019)
df_centroids_2019_no <- st_as_sf(df_centroids_2019_no, coords = c("caplong", "caplat"), crs = 4326)
coords_no <- cbind(df_sp_no_host2019$caplong, df_sp_no_host2019$caplat)
k1_no <- knn2nb(knearneigh(coords_no))
critical.threshold_no <- max(unlist(nbdists(k1_no,coords_no)))
nb.dist.band_no <- dnearneigh(coords_no, 0, critical.threshold_no)
distances_no <- nbdists(nb.dist.band_no,coords_no)
inverted_dists_no <- lapply(distances_no, function(x){1/x})
nbw_inverted19_no <- nb2listw(nb.dist.band_no, glist = inverted_dists_no, style = "B", zero.policy = TRUE)

###panel spatial lag

id_spgm_no_host <- spgm(ln_ME_GDP ~ democracy + ln_Population + terror_attacks, data=df_sp_no_host,
                        lag = TRUE, spatial.error = FALSE, 
                        listw = nbw_inverted19_no, method = "w2sls")

id_spgm_host <- spgm(ln_ME_GDP ~ democracy + ln_Population + terror_attacks, data=df_sp_host,
                     lag = TRUE, spatial.error = FALSE,
                     listw = nbw_inverted19, method = "w2sls")

spgm_id <- list(id_spgm_no_host, id_spgm_host)

Z <- (.535-.439)/(.096^2 +.092^2)^.5


