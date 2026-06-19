library(tidyverse)
setwd("C:/Users/finky/Documents/IDDO/Code&Data")
()
novartis <- read_csv("novartis.csv")

#Extracting the Eloff data from Novartis
eloff_data <- novartis %>%
  filter(`Site Name` %in% c(
    "Kavango East",
    "Kavango West",
    "Zambezi", 
    "Ohangwena",
    "Omusati"
  ))

#Extracting data from Zambia as a comparison
zambia <- novartis %>%
  filter(Country == "Zambia")
summary(zambia)
