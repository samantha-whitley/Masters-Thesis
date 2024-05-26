
##SIPRI DATA
show_col_types = FALSE

sheet_names <- excel_sheets("SIPRI-Milex-data-2000-2023.xlsx")

## Read each sheet into a list of data frames
dfs_SIPRI <- lapply(sheet_names, function(sheet) {
  read_excel("SIPRI-Milex-data-2000-2023.xlsx", sheet = sheet)
})

## ME/GDP Variable
ME_GDP_SIPRI <- dfs_SIPRI[[4]]

ME_GDP_SIPRI <- ME_GDP_SIPRI %>%
  mutate_at(vars(starts_with("20")), as.numeric)

ME_GDP_SIPRI <- ME_GDP_SIPRI[-c(1:4, 6), ]
ME_GDP_SIPRI <- ME_GDP_SIPRI[, -2]
ME_GDP_SIPRI <- janitor::row_to_names(ME_GDP_SIPRI, 1)

ME_GDP_SIPRI <- ME_GDP_SIPRI %>% 
  mutate(Country = ifelse(Country == "Türkiye", "Turkiye", Country)) %>% 
  mutate(Country = ifelse(Country == "United States of America", "United States", Country))

## Population World Bank
df_pop <- read_csv("API_SP.POP.TOTL_DS2_en_csv_v2_79.csv")
nato_countries <- c("Albania", "Belgium", "Bulgaria", "Canada", "Croatia", "Czechia", "Denmark", "Estonia", 
                    "Finland", "France", "Germany", "Greece", "Hungary", "Italy", "Latvia", "Lithuania", "Luxembourg", 
                    "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania",
                    "Slovakia", "Slovenia", "Spain", "Turkiye", "United Kingdom", "United States")
df_pop <- df_pop %>% 
  rename(Country = `Country Name`) %>%
  mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))  %>%
  filter(Country %in% nato_countries) 
  
eastern_central_europe_countries <- c("Albania", "Bulgaria", "Croatia", "Czechia", "Estonia",
                                      "Hungary", "Latvia", "Lithuania", "Montenegro", "North Macedonia",
                                      "Poland", "Romania", "Slovakia", "Slovenia")


ccode <- df_pop[, c("Country Code","Country")]

## Country shapes
??cshapes

nato_countries1 <- c("Albania", "Belgium", "Bulgaria" ,"Canada", "Croatia", "Denmark", "Estonia", 
                     "Finland", "France", "Greece", "Hungary", "Latvia", "Lithuania", "Luxembourg", 
                     "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania",
                     "Slovak Republic", "Slovenia", "Spain", "United Kingdom", "United States of America",
                     "Turkey (Ottoman Empire)", "German Federal Republic", "Czech Republic", 
                     "Italy/Sardinia", "Macedonia (FYROM/North Macedonia)", "Rumania", "Slovakia")

cshapes <- cshp(date=as.Date("2019-1-1"), useGW = TRUE, dependencies = FALSE)

country_df <- cshapes %>% 
  filter(country_name %in% nato_countries1)

country_df <- country_df %>% 
  mutate(country_name = ifelse(country_name == "United States of America", "United States", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "Turkey (Ottoman Empire)", "Turkiye", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "German Federal Republic", "Germany", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "Czech Republic", "Czechia", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "Italy/Sardinia", "Italy", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "Macedonia (FYROM/North Macedonia)", "North Macedonia", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "Rumania", "Romania", country_name))  
  

gw_code <- country_df[, c("gwcode","country_name")]



## PIVOT

pop_long_df <- pivot_longer(df_pop, cols = -c(Country, "Country Code"), names_to = "Year", values_to = "Value")

ME_GDP_SIPRI[, -1]  <- sapply(ME_GDP_SIPRI[, -1], as.numeric)

ME_long_df <- pivot_longer(ME_GDP_SIPRI, cols = -Country, names_to = "Year", values_to = "Value")


## Join ME and Pop df 

main_df <- left_join(ME_long_df, pop_long_df, by = c("Country", "Year"))

russia_ME<- main_df[main_df$Country == "Russia", ]
main_df <- main_df[main_df$Country != "Russia", ]

main_df <- main_df %>%
  rename(`ME/GDP` = Value.x,
         Population = Value.y,
         year = Year)

NATO_countries <- ME_GDP_SIPRI[, 1] 

write.csv(NATO_countries, "nato_countries.csv", row.names = FALSE)

#Add geo data
country_df_merge <- country_df %>% select(-1, -3, -4, -5, -6, -10, -11)

main_df <- left_join(country_df_merge, main_df, by = c("country_name" = "Country"))

#Add terror
terror <- read.csv("terror.csv")

names(main_df)[names(main_df) == 'Country Code'] <- 'ccode'
main_df$year <- as.integer(main_df$year)
main_df <- full_join(terror, main_df, by = c('ccode', "year"))
main_df <- main_df[main_df$ccode!= "SWE", ]

##dummy

host_us_nukes <- read_excel("Host US Nukes.xlsx")
dca <-read_excel("DCA capable .xlsx")

names(host_us_nukes)[1] <- "country_name"

host_us_nukes_long <- pivot_longer(host_us_nukes, 
                                   cols = -country_name, 
                                   names_to = "year", 
                                   values_to = "host_us_nukes")

names(dca)[1] <- "country_name"

dca_long <- pivot_longer(dca, 
                                   cols = -country_name, 
                                   names_to = "year", 
                                   values_to = "dca")

main_df <- merge(main_df, host_us_nukes_long, by = c("country_name", "year"), all.x = TRUE)
main_df <- merge(main_df, dca_long, by = c("country_name", "year"), all.x = TRUE)


#country code

countries <- world[c("name_long")]

countries <- countries  %>%
  mutate(name_long = ifelse(name_long == "Czech Republic", "Czechia", name_long)) %>%
  mutate(name_long = ifelse(name_long == "Macedonia", "North Macedonia", name_long)) %>%
  mutate(name_long = ifelse(name_long == "Turkey", "Turkiye", name_long)) %>%
  filter(name_long %in% nato_countries) %>%
  rename('country_name' = 'name_long')

main_df <- merge(main_df, countries, by = "country_name")

main_df <- main_df %>%
  rename(geom_sp = geom) %>%
  rename(geom_cshapes = geometry) 

df_sp <- main_df %>%
  select(-geom_cshapes)

df_c <- main_df %>%
  select(-geom_sp)

df_sp <- df_sp %>%
  rename(ME_GDP = `ME/GDP`)

#add Russia ME 

russia_ME <- russia_ME %>%
  select(-c(`Country Code`, Value.y))

russia_ME <- russia_ME %>%
  rename(russia_ME = 'Value.x')

russia_ME <- russia_ME %>%
  rename('year' = 'Year')

russia_ME <- russia_ME %>%
  select(-Country)

df_sp <- merge(df_sp, russia_ME, by = "year")

### Create contiguity with Russia
nato_countries2 <- c("Albania", "Belgium", "Bulgaria" ,"Canada", "Croatia", "Denmark", "Estonia", 
                     "Finland", "France", "Greece", "Hungary", "Latvia", "Lithuania", "Luxembourg", 
                     "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania",
                     "Slovak Republic", "Slovenia", "Spain", "United Kingdom", "United States of America",
                     "Turkey (Ottoman Empire)", "German Federal Republic", "Czech Republic", 
                     "Italy/Sardinia", "Macedonia (FYROM/North Macedonia)", "Rumania", "Slovakia", "Russia (Soviet Union)")

cshapes <- cshp(date=as.Date("2019-1-1"), useGW = TRUE, dependencies = FALSE)

country_df_russia <- cshapes %>% 
  filter(country_name %in% nato_countries2)

country_df_russia <- country_df_russia %>% 
  mutate(country_name = ifelse(country_name == "United States of America", "United States", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "Turkey (Ottoman Empire)", "Turkiye", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "German Federal Republic", "Germany", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "Czech Republic", "Czechia", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "Italy/Sardinia", "Italy", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "Macedonia (FYROM/North Macedonia)", "North Macedonia", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "Rumania", "Romania", country_name))  %>% 
  mutate(country_name = ifelse(country_name == "Russia (Soviet Union)", "Russia", country_name))


#remove Finland
df_sp <- df_sp[df_sp$country_name != "Finland", ]

#create years of membership

nato_join_dates <- data.frame(
  country_name = c("Albania", "Belgium", "Bulgaria", "Canada", "Croatia", "Czechia", "Denmark", "Estonia", 
              "France", "Germany", "Greece", "Hungary", "Italy", "Latvia", "Lithuania", "Luxembourg", 
              "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania",
              "Slovakia", "Slovenia", "Spain", "Turkiye", "United Kingdom", "United States"),
  joined_nato = c(2009, 1949, 2004, 1949, 2009, 1999, 1949, 2004, 1949, 1955, 1952, 1999, 1949, 2004, 
                  2004, 1949, 2017, 1949, 2020, 1949, 1999, 1949, 2004, 2004, 2004, 1982, 1952, 1949, 1949)
)

df_sp <- merge(df_sp, nato_join_dates, by = "country_name", all.x = TRUE)

nato_membership <- function(year, country_name) {
  ifelse(year >= nato_join_dates$joined_nato[nato_join_dates$country_name == country_name], 1, 0)
}

df_sp_test <- expand.grid(year = 2000:2022, country_name = nato_join_dates$country_name) %>%
  mutate(in_nato = mapply(nato_membership, year, country_name))

df_sp <- merge(df_sp, df_sp_test, by = c("country_name", "year"))

polyarchy <- read_csv("polyarchy-lexical.csv")

polyarchy_df <- polyarchy %>%
  filter(Year >= 2013) %>%
  mutate(Entity = ifelse(Entity == "Turkey", "Turkiye", Entity)) %>%
  filter(Entity %in% nato_countries) %>%
  select(!Code) %>%
  rename(country_name = Entity,
         year = Year)

df_sp <- merge(df_sp, polyarchy_df, by = c("country_name", "year"))

df_sp <- st_sf(df_sp)
