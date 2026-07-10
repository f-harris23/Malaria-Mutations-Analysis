library(tidyverse)
novartis <- read_csv("novartis.csv")

#Basic cleaning
novartis <- novartis %>% 
  mutate(
    `Start Year` = as.numeric(`Start Year`),
    `End Year` = as.numeric(`End Year`),
    `Year Published` = as.numeric(`Year Published`),
    Present = as.numeric(Present),
    Tested = as.numeric(Tested),
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    `Prevalence (%)` = as.numeric(`Prevalence (%)`),
    `Prevalence (%) incl mixed` = as.numeric(`Prevalence (%) incl mixed`)
  ) %>%
  filter(!is.na(`Start Year`) & !is.na(`End Year`)) %>%
  filter(`End Year` > 1960 & `End Year` < 2500) %>%
  filter(`Start Year` > 1960 & `Start Year` < 2500) %>%
  filter(Tested > 0) %>%
  filter(between(Latitude, -90, 90)) %>%
  filter(between(Longitude, -180, 180)) %>%
  mutate(year = round((`Start Year` + `End Year`) / 2, 0),
         year = case_when(is.na(year) & !is.na(`Start Year`) ~ `Start Year`,
                          is.na(year) & !is.na(`End Year`) ~ `End Year`,
                          TRUE ~ year)) %>%
  drop_na(Present, Tested)

#Fixing whitespace when seperating marker strings
novartis <- novartis %>%
  mutate(across(where(is.character), str_trim))

#Replace any possible empty strings with NA
novartis <- novartis %>%
  mutate(across(where(is.character), ~na_if(., "")))

#Removing all instances where 'null' appears and swapping it to 'NA'
novartis <- novartis %>%
  mutate(across(where(is.character), ~na_if(., "null")))

#Splitting entires in the 'Marker' column into 'Gene' and 'Mutation'
novartis <- novartis %>%
  mutate(
    Gene = str_extract(Marker, "^\\S+"),
    Mutation = str_extract(Marker, "\\S+$")
  )

#Checking what '1' is in the mutation column - maybe an error
#novartis %>%
#  filter(Mutation == "1") %>%
#  select(Marker, Gene, Mutation) %>%
#  head(20)

#Checking haplotypes
#novartis %>%
#  filter(str_detect(Mutation, "[A-Z]{3,}$")) %>%
#  count(Mutation, sort = TRUE) %>%
#  head(20)

#Here we have inconsistency with start and end dates, need to check
#novartis %>% 
#  filter(uniq_id_publication == 1579) %>%
#  View()

#Changing the end dates to 2023 for this specific study - consistent with the publication
novartis <- novartis %>%
  mutate(
    `End Year` = if_else(
      uniq_id_publication == 1579, 
      2023,
      `End Year`
    )
  )
  
#Entries with >100% prevalence, need to determine what to do
#novartis %>%
#  filter(Present > Tested) %>%
#  select(Country, `Site Name`, Marker, Present, Tested, `Prevalence (%)`) %>%
#  print(n = 12)

#I've decided to remove all 12 rows where Prev.>100%, won't have much effect on the data
#The publications do not give enough information to make changes
novartis <- novartis %>%
  filter(!(Present > Tested))

#Duplicate rows - 400 and I don't know what do do with them
novartis %>%
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) %>%
  View()

#Decided to remove all 400 duplicates, they are completely identical in every column
#By looking at some of the publications, it's difficult to distinguish duplicates from legitimate entries
#Thought it's best to treat all duplicates in the same way rather than keeping some and discarding others
novartis <- novartis %>%
  distinct()

#Entering missing info for publication 296 - found on publication url
novartis <- novartis %>%
  mutate(
    Continent = if_else(uniq_id_publication == 296, "Africa", Continent),
    Country = if_else(uniq_id_publication == 296, "Angola", Country),
    District = if_else(uniq_id_publication == 296, "Bengo", District),
    `Site Name` = if_else(uniq_id_publication == 296, "Dande", `Site Name`),
    Latitude = if_else(uniq_id_publication == 296, -8.5, Latitude),
    Longitude = if_else(uniq_id_publication == 296, 13.833, Longitude)
  )

#Standardizing some country names to align with the map packages on R
#Run after loading any map packages in case the names are older
novartis <- novartis %>%
  mutate(Country = case_when(
    Country == "Swaziland" ~ "Eswatini",
    Country == "Viet Nam" ~ "Vietnam", 
    Country == "Lao People's Democratic Republic" ~ "Laos", 
    Country == "Côte d'Ivoire" ~ "Ivory Coast",
    TRUE ~ Country
  ))

#Checking if there are any errors in calculating errors for Prevalence
#There are no rows when running this so Prevalence must be calculated correctly for each row
novartis %>%
  filter(abs(`Prevalence (%)` - (Present / Tested * 100)) > 0.01) %>%
  select(uniq_id_publication, Country, Marker, Present, Tested, `Prevalence (%)`) %>%
  print(n = 20)

#Missing value report
novartis %>%
  summarise(across(everything(), ~sum(is.na(.))))

#Creating a new .csv file of the cleaned data for analysis
write_csv(novartis, "novartis_clean.csv")




