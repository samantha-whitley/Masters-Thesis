if (!require("haven")) {
  install.packages("haven")
}
library(haven)

data <- read_dta("replication_data.dta")

ccode <-unique(data$ccode)
ccode 
Russia <- data %>%
  select(ccode, year, terror_attacks) %>%
  filter(year >= 2000)

file_name <- "terror.csv"

# Save the extracted dataframe as a CSV file in the current working directory
write.csv(terror, file = file_name, row.names = FALSE)

