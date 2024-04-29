

#### summary statistics 

df_sp_host2019_summary <- df_sp_host %>%
  filter(year == 2019, !is.na(ME_GDP)) %>%
  select(ME_GDP, Population, ln_ME_GDP, ln_Population)
df_sp_host2019_summary <- st_drop_geometry(df_sp_host2019_summary)

summary_stats <- cbind(
  Mean = sapply(df_sp_host2019_summary, mean, na.rm = TRUE),
  Median = sapply(df_sp_host2019_summary, median, na.rm = TRUE),
  SD = sapply(df_sp_host2019_summary, sd, na.rm = TRUE),
  Min = sapply(df_sp_host2019_summary, min, na.rm = TRUE),
  Max = sapply(df_sp_host2019_summary, max, na.rm = TRUE),
  N = sapply(df_sp_host2019_summary, sum, na.rm = TRUE)
)

summary_stats <- as.data.frame(summary_stats)
summary_stats <- rownames_to_column(summary_stats, var = "Variable")
summary_stats[, sapply(summary_stats, is.numeric)] <- round(summary_stats[, sapply(summary_stats, is.numeric)], 3)

summary_stats_table <- flextable(summary_stats)

summary_stats_doc <- read_docx()

summary_stats_doc <- body_add_flextable(summary_stats_doc, value = summary_stats_table)

file_path <- "plots/summary_stats_doc.docx"
print(summary_stats_doc, target = file_path)


#### MAP
df_sp2019_plot <- df_sp %>%
  filter(year == 2018) 

qtm(df_sp2019_plot, fill="ME_GDP") 

tm_shape(df_sp2019_plot) + tm_fill(fill = "ME_GDP") + tm_borders() 

#### ME per country over time 

ME_plot <- ggplot(df_sp_host, aes(x = year, y = ME_GDP, color = factor(dca))) +
  geom_line() +
  facet_wrap(~country_name, nrow = 5) +  # Adjust the number of rows as needed
  labs(title = "NATO's Members' Military Expenditure per Year", x = "Year", y = "Military Expenditure per GDP", color = "Host Nuclear Weapins") +
  scale_color_manual(values = c("grey", "red"), labels = c("Do not Host", "Host")) +  # Customize color scale
  theme_bw()

#### OLS Table

ols_models_table <- modelsummary(ols_models, 
                                 output = 'flextable',
                                 coef_omit = "Intercept",
                                 fmt = 2,
                                 stars = c('*' = .1, '**' = .05, '***' = .01),
                                 coef_rename = c("dca" = "Host Nuclear Weapons", "East" = "Eastern Bloc", "ln_Population" = "Log of Poplation"),
                                 gof_map = c("nobs", "r.squared"),
                                 caption = "OLS Regression 2014 - 2022") |>
  autofit() %>% 
  add_footer_lines("Robust standard errors are in brackets.") 

ols_models_doc <- read_docx()

ols_models_doc <- body_add_flextable(ols_models_doc, value = ols_models_table)

file_path <- "plots/ols_models_doc.docx"
print(ols_models_doc, target = file_path)

#### Moran's I  

moran_contiguity <- data.frame(
  Year = 2014:2022,
  Moran_I_Statistic_Contigity = c(0.08571968, 0.07806536, 0.20384562, 0.23358990, 0.37789500, 
                                  0.58641818, 0.26694374, 0.20194814, 0.11290679),
  P_Value_Contiguity = c(0.2104, 0.228, 0.06125, 0.04713, 0.005951, 
                         9.293e-05, 0.02419, 0.04608, 0.1583)
)

moran_inverse <- data.frame(
  Year = 2014:2022,
  Moran_I_Statistic_Inverse = c(0.007339257, 0.05696222, 0.19670345, 0.23759770, 0.31995210, 
                                0.30201177, 0.21240021, 0.17378726, 0.14487530),
  P_Value_Inverse = c(0.3382, 0.2125, 0.02603, 0.01085, 0.001613, 
                      0.002638, 0.01178, 0.02073, 0.04732)
)

summary_stats <- merge(moran_contiguity, moran_inverse, by = "Year")

summary_stats <- summary_stats %>%
  mutate(across(where(is.numeric), round, digits = 3))

summary_stats$Contiguity_Significance <- ifelse(summary_stats$P_Value_Contiguity < 0.001, "***", 
                                                ifelse(summary_stats$P_Value_Contiguity < 0.01, "**",
                                                       ifelse(summary_stats$P_Value_Contiguity < 0.05, "*", "")))

summary_stats$Inverse_Significance <- ifelse(summary_stats$P_Value_Inverse < 0.01, "***", 
                                             ifelse(summary_stats$P_Value_Inverse < 0.05, "**",
                                                    ifelse(summary_stats$P_Value_Inverse < 0.1, "*", "")))

summary_stats$I_Statistic_Contiguity <- paste0(
  format(summary_stats$Moran_I_Statistic_Contigity, digits=4, nsmall=4), 
  summary_stats$Contiguity_Significance
)

summary_stats$I_Statistic_Inverse <- paste0(
  format(summary_stats$Moran_I_Statistic_Inverse, digits=4, nsmall=4), 
  summary_stats$Inverse_Significance
)

moran_I_df <- summary_stats %>%
  select(Year, I_Statistic_Contiguity, I_Statistic_Inverse)

moran_I_df$Year <- as.numeric(gsub(",", "", moran_I_df$Year))


Moran_I_Table <- flextable(moran_I_df) |> 
  colformat_num(
    big.mark = "", decimal.mark = "",
    na_str = "N/A"
  ) |> 
  add_header( Year = "Year",
              I_Statistic_Contiguity = "Weight", 
              I_Statistic_Inverse = "Weight", top = TRUE ) |>
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  set_header_labels( Year = "Year", I_Statistic_Contiguity = "Contiguity", I_Statistic_Inverse = "Inverse Distance") |>
  set_caption(caption = "Moran's I Statistics with Significance", align_with_table = FALSE) %>% 
  align(align = "center", part = "header") %>% 
  add_footer_lines("Standard errors are in brackets. \n * 10% Significance. ** 5% Significance. *** 1% Significance.\n  Each value is the estimate of rho from a single-year from model 2.") |>
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 12, part = "all")

Moran_I_Table

Moran_I_doc <- read_docx()

Moran_I_doc <- body_add_flextable(Moran_I_doc, value = Moran_I_Table)

file_path <- "plots/Word_Document_Moran_I.docx"
print(Moran_I_doc, target = file_path)

#### Sar Rho Table

models_sar_inverse <- list(
  `2014` = sar_2014, 
  `2015` = sar_2015, 
  `2016` = sar_2016, 
  `2017` = sar_2017, 
  `2018` = sar_2018, 
  `2019` = sar_2019, 
  `2020` = sar_2020, 
  `2021` = sar_2021, 
  `2022` = sar_2022
)

models_sar_contiguity <- list(
  `2014` = sar_contiguity_2014, 
  `2015` = sar_contiguity_2015, 
  `2016` = sar_contiguity_2016, 
  `2017` = sar_contiguity_2017, 
  `2018` = sar_contiguity_2018, 
  `2019` = sar_contiguity_2019, 
  `2020` = sar_contiguity_2020, 
  `2021` = sar_contiguity_2021, 
  `2022` = sar_contiguity_2022
)

rho_data <- lapply(names(models_sar_inverse), function(year) {
  model <- models_sar_inverse[[year]]
  tibble(
    Year = year,
    Rho = model$rho,
    SE = model$rho.se,
    P_value = ifelse(model$rho.se == 0, NA, 2 * pnorm(-abs(model$rho / model$rho.se)))  # Wald test for p-value
  )
}) %>% bind_rows() %>% 
  mutate(
    Significance = case_when(
      P_value < 0.01  ~ "***",
      P_value < 0.05  ~ "**",
      P_value < 0.1   ~ "*",
      TRUE            ~ ""
    ),
    Rho_Display = sprintf("%3.3f%s \n (%3.3f)", Rho, Significance, SE)  # Include HTML tag for line break
  )

rho_data_kable <- rho_data %>%
  select(Year, Rho_Display)

rho_table <- kable(rho_data_kable, format = "html", escape = FALSE, 
                   col.names = c("Year", "Inverse Distance"), 
                   caption = "Rho Values and Standard Errors by Year") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  add_header_above(c(" " = 1, "Rho Statistics" = 1))

rho_data_contiguity <- lapply(names(models_sar_contiguity), function(year) {
  model <- models_sar_contiguity[[year]]
  tibble(
    Year = year,
    Rho = model$rho,
    SE = model$rho.se,
    P_value = ifelse(model$rho.se == 0, NA, 2 * pnorm(-abs(model$rho / model$rho.se)))  # Wald test for p-value
  )
}) %>% bind_rows() %>% 
  mutate(
    Significance = case_when(
      P_value < 0.01  ~ "***",
      P_value < 0.05  ~ "**",
      P_value < 0.1   ~ "*",
      TRUE            ~ ""
    ),
    Rho_Display = sprintf("%3.3f%s \n (%3.3f)", Rho, Significance, SE)  # Include HTML tag for line break
  )

rho_data_kable_contiguity <- rho_data_contiguity %>%
  select(Year, Rho_Display)

rho_table_contiguity <- kable(rho_data_kable_contiguity, format = "html", escape = FALSE, 
                              col.names = c("Year", "Contiguity"), 
                              caption = "Rho Values and Standard Errors by Year") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  add_header_above(c(" " = 1, "Rho Statistics" = 1))

combined_rho_data <- rho_data_kable_contiguity %>%
  rename(Rho_Display_Contiguity = Rho_Display) %>%
  inner_join(rho_data_kable %>% rename(Rho_Display_Inverse = Rho_Display), by = "Year")

combined_rho_table <- combined_rho_data |> 
  flextable() |> 
  add_header(Year = "Year",
             Rho_Display_Contiguity = "Weight", 
             Rho_Display_Inverse = "Weight") |>
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  set_header_labels(Year = "Year", Rho_Display_Contiguity = "Contiguity", Rho_Display_Inverse = "Inverse Distance") |>
  align(align = "center", part = "header") %>% 
  set_caption(caption = "Defense Burden (logged)", align_with_table = FALSE ) %>%
  add_footer_lines("Standard errors are in brackets. \n * 10% Significance. ** 5% Significance. *** 1% Significance.\n  Each value is the estimate of rho from a single-year from model 2.") 


combined_rho_doc <- read_docx()

combined_rho_doc <- body_add_flextable(combined_rho_doc, value = combined_rho_table)
file_path <- "plots/combined_rho_doc.docx"
print(combined_rho_doc, target = file_path)



#### Direct Effects Table
convert_p_to_stars <- function(p_values) {
  sapply(p_values, function(x) {
    if (x < 0.001) {
      "***"
    } else if (x < 0.01) {
      "**"
    } else if (x < 0.05) {
      "*"
    } else {
      ""
    }
  })
}

combined_effects <- data.frame()

for(year in names(sar_results_contiguity)) {
  impacts_data <- sar_results_contiguity[[year]]$impacts$res
  p_values_data <- sar_results_contiguity[[year]]$imp_summary
  
  effects_df <- data.frame(
    Year = rep(year, 3),
    Variable = c("dca", "East", "ln_Population"),
    Direct_Effect = impacts_data$direct,
    Indirect_Effect = impacts_data$indirect,
    Total_Effect = impacts_data$total,
    Direct_pValue = convert_p_to_stars(p_values_data[,"Direct"]),
    Indirect_pValue = convert_p_to_stars(p_values_data[,"Indirect"]),
    Total_pValue = convert_p_to_stars(p_values_data[,"Total"])
  )
  
  combined_effects <- rbind(combined_effects, effects_df)
}

combined_effects <-  combined_effects %>%
  mutate(across(where(is.numeric), round, digits = 3))
combined_effects

combined_effects$Direct_Effect <- paste0(combined_effects$Direct_Effect, combined_effects$Direct_pValue)
combined_effects$Indirect_Effect <- paste0(combined_effects$Indirect_Effect, combined_effects$Indirect_pValue)
combined_effects$Total_Effect <- paste0(combined_effects$Total_Effect, combined_effects$Total_pValue)

combined_effects <- combined_effects %>%
  select(Year, Variable, Direct_Effect, Indirect_Effect, Total_Effect)

combined_effects <- combined_effects %>%
  mutate(Variable = case_when(
    Variable == "dca" ~ "Host Nuclear\nWeapons",
    Variable == "East" ~ "Eastern Bloc",
    Variable == "ln_Population" ~ "Log of Population",
    TRUE ~ Variable  # Fallback in case there are other unexpected values
  ))
str(combined_effects)

combined_effects <- pivot_longer(combined_effects, cols = c(Direct_Effect, Indirect_Effect, Total_Effect), names_to = "Effects", values_to = "Effect_Value")
combined_effects <- pivot_wider(combined_effects, names_from = "Variable", values_from = "Effect_Value")

combined_effects <- combined_effects %>%
  mutate(Effects = case_when(
    Effects == "Direct_Effect" ~ "Direct Effect",
    Effects == "Indirect_Effect" ~ "Indirect Effect",
    Effects == "Total_Effect" ~ "Total Effect",
    TRUE ~ Effects  # Fallback in case there are other unexpected values
  ))

col_names <- c("", "Host Nuclear<br/>Weapons", "Eastern Bloc", "Log of<br/>Population")


effects_table <- combined_effects |> 
  flextable() |> 
  separate_header() |> 
  merge_v(j = 1) |> 
  valign(j = 1, valign = "top") |>
  set_caption(caption = "Defense Burden (logged)", align_with_table = FALSE ) %>%
  autofit() |>
  add_footer_lines("Standard errors are in brackets. \n * 10% Significance. ** 5% Significance. *** 1% Significance.\n  Each value is the estimate of rho from a single-year from model 2.")

effects_table_doc <- read_docx()

effects_table_doc <- body_add_flextable(effects_table_doc, value = effects_table)

file_path <- "plots/effects_table.docx"
print(effects_table_doc, target = file_path)


#### Model Check / Appendix

AIC(slx_2015, sem_2015, sar_2015)

model1 <- lm(ln_ME_GDP ~ dca + East + ln_Population, data = df_sp_host2019)

lm.RStests(
  model1,
  nbw_inverted19,
  test = c("LMerr", "LMlag")
)

AICs<-c(AIC(slx_2019),AIC(sem_2019), AIC(sar_2019))
labels<-c("SLX", "SEM","SAR" )

flextable(data.frame(Models=labels, AIC=round(AICs, 2)))






