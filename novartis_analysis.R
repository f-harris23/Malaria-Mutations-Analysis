library(tidyverse)
library(rnaturalearth)
library(viridis)
library(sf)
library(ggiraph)

novartis_clean <- read_csv("novartis_clean.csv")

#Some geometries from rnaturalearth are invalid - quick fix
sf_use_s2(FALSE)

#Creating a world data frame for plotting
world <- ne_countries(scale = "medium", returnclass = "sf")

#Starting with simple results 
#Looking at the countries with the most data entries 
novartis_clean %>%
  count(Country, sort = TRUE)
#Most common markers/genes/mutations
novartis_clean %>%
  count(Marker, sort = TRUE)
#Most common mutations for the common genes
novartis_clean %>%
  count(Gene, Mutation, sort = TRUE) %>%
  group_by(Gene) %>%
  slice_max(n, n = 1, with_ties = FALSE)
#Prevalence summary by marker
novartis_clean %>%
  group_by(Marker) %>%
  summarise(
    mean_prevalence = mean(`Prevalence (%)`, na.rm = TRUE),
    n_studies = n()
  ) %>%
  arrange(desc(mean_prevalence)) %>%
  head(20)
#Looking at P441L prevalence as part of the Eloff publication
novartis_clean %>%
  filter(Mutation == "P441L") %>%
  group_by(Country) %>%
  summarise(
    mean_prevalence = mean(`Prevalence (%)`, na.rm = TRUE), 
    n_studies = n()
  ) %>%
  arrange(desc(mean_prevalence))

#slice_max() not working so had to make a new dataframe for the bar chat 
result <- novartis_clean %>%
  filter(Mutation == "P441L", !is.na(`Prevalence (%)`)) %>%
  group_by(Country) %>%
  summarise(mean_prevalence = mean(`Prevalence (%)`, na.rm = TRUE)) %>%
  arrange(desc(mean_prevalence)) %>%
  head(8)

#Can represent P441L prevalence by the bar chart below - only includes countries with >0% prev.
result %>%
  ggplot(aes(x = reorder(Country, mean_prevalence), y = mean_prevalence)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  coord_flip() + 
  labs(title = "P441L Prevalence by Country", x = "Country", y = "Mean Prevalence (%)") 

#Now we can plot these on a heatmap which shows prevalence in African countries
africa <- world %>%
  filter(continent == "Africa") %>%
  left_join(result, by = c("name" = "Country")) %>%
  ggplot() +
  geom_sf(aes(fill = mean_prevalence)) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey90") +
  coord_sf(xlim = c(0, 50), ylim = c(-35, 15)) +
  theme_bw() +
  labs(title = "P441L Prevalence in Africa",
       fill = "Mean Prevalence (%)")

africa

#Since the Eloff paper focuses on Namibia - may be useful to make more detailed plots
#We can use river, lake and possibly elevation data to add some more context
lakes <- ne_download(
  scale = "large",
  type = "lakes",
  category = "physical",
  returnclass = "sf"
)
namibia <- ne_countries(
  country = "Namibia",
  returnclass = "sf"
)
namibia_lakes <- st_intersection(lakes, namibia)

rivers <- ne_download(
  scale = "large", 
  type = "rivers_lake_centerlines",
  category = "physical",
  returnclass = "sf",
)
namibia_rivers <- st_intersection(rivers, namibia)

#Creating a summarised data frame for the bubble map
namibia_p441l_summary <- novartis_clean %>%
  filter(Country == "Namibia", Mutation == "P441L") %>%
  group_by(`Site Name`, Latitude, Longitude) %>%
  summarise(
    mean_prevalence = mean(`Prevalence (%)`, na.rm = TRUE), 
    total_tested = sum(Tested, na.rm = TRUE)
  )

#Looking at pfk13 P441L prevalence in Namibia using a bubble map
ggplot() +
  geom_sf(data = namibia, fill = "grey95") +
  geom_sf(data = namibia_lakes, fill = "lightblue", colour = "blue") +
  geom_sf(data = namibia_rivers, colour = "dodgerblue", linewidth = 0.3) +
  geom_point(data = namibia_p441l_summary,
             aes(x = Longitude, y = Latitude, 
                 fill = mean_prevalence,
                 size = total_tested),
             shape = 21, colour = "black") +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_size_continuous(range = c(3, 10)) + 
  geom_text(data = namibia_p441l_summary, 
            aes(x = Longitude, y = Latitude, 
                label = `Site Name`),
            nudge_y = 0.55, size = 2.24) +
  labs(title = "P441L Prevalence in Namibia",
       fill = "Mean Prevalence (%)",
       size = "Samples Tested")

novartis_clean %>%
  filter(Country == "Namibia", Mutation == "P441L") %>%
  group_by(`Site Name`, Latitude, Longitude) %>%
  summarise(
    mean_prevalence = mean(`Prevalence (%)`, na.rm = TRUE),
    total_tested = sum(Tested, na.rm = TRUE),
    n_studies = n()
  ) %>%
  arrange(desc(mean_prevalence))

#The Eloff publication states that P441L increases West-to-East across Namibia
#We can see if this holds using Spearman's test
cor.test(~Longitude + mean_prevalence,
         data = namibia_p441l_summary,
         method = "spearman") #Rejects null (rho = 0), rho approx 0.772 which implies strong positive correlation

#Creating data frames for scatter plot
# define markers
k13_markers <- c("P441L", "P574L", "A675V", "R515K", "C580Y", 
                 "R561H", "C469Y", "C469F", "A675V")

sp_markers <- c("51I", "59R", "108N", "437G", "540E", "581G")

africa_art <- novartis_clean %>%
  filter(Continent == "Africa", Gene == "k13") %>%
  filter(!grepl("copy number", Marker)) %>%
  filter(Mutation != "wildtype") %>%
  group_by(Country) %>%
  summarise(art_prevalence = mean(`Prevalence (%)`, na.rm = TRUE))

africa_sp <- novartis_clean %>%
  filter(Continent == "Africa", Mutation %in% sp_markers) %>%
  group_by(Country) %>%
  summarise(sp_prevalence = mean(`Prevalence (%)`, na.rm = TRUE))

africa_combined <- africa_art %>%
  inner_join(africa_sp, by = "Country")

#Visualising the results using a scatter plot
africa_scatter <- ggplot(data = africa_combined, aes(x = art_prevalence, y = sp_prevalence)) +
  geom_point_interactive( 
    aes(colour = Country == "Namibia",
        tooltip = Country,
        data_id = Country),
    size = 3) +
  scale_colour_manual(values = c("grey50", "red"),
                      labels = c("Other", "Namibia")) +
  geom_smooth(method = "lm", se = TRUE, colour = "blue") +
  labs(title = "Artemisinin vs SP resistance in Africa (SNP only)",
       x = "Mean Artemisinin resistance prevalence (%)",
       y = "Mean SP resitance prevalence (%)",
       colour = "") +
  theme_minimal()

girafe(ggobj = africa_scatter)

#Producing a Spearman's correlation test to see the association between SP and Artemisinin resistance prevalence
#I chose this instead of Pearson's since ranking is more robust to outliers which we have as shown by the graph
cor.test(africa_combined$art_prevalence,
         africa_combined$sp_prevalence,
         method = "spearman") #Alternative hypothesis true with rho = 0.397 and p = 0.0076, moderate positive correlation

#Seeing if the graph matches up with the table
novartis_clean %>%
  filter(Continent == "Africa", Mutation %in% k13_markers) %>%
  group_by(Country) %>%
  summarise(
    mean_prevalence = mean(`Prevalence (%)`, na.rm = TRUE),
    n_studies = n()
  ) %>%
  arrange(desc(mean_prevalence)) %>%
  print(n = 40)

#Creating a data frame for surveillance gap plot
africa_surveillance <- novartis_clean %>%
  filter(Continent == "Africa") %>%
  count(Country)

surveillance <- world %>%
  filter(continent == "Africa") %>%
  left_join(africa_surveillance, by = c("name" = "Country"))

#Plotting surveillance data
ggplot(data = surveillance) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient(low = "white", high = "darkblue",
                      na.value = "grey") +
  coord_sf(xlim = c(-20, 55), ylim = c(-37, 37)) +
  theme_minimal() +
  labs(fill = "Number of\nobservations",
       caption = "Grey = no surveillance data")

#Quick check to see if numbers for each country match with heatmap
novartis_clean %>%
  filter(Country == "Uganda") %>%
  nrow()



