# PACKAGES
library(tidyverse)

######### FIXING ISSUES WITH "folketing_members.csv" ############
list_of_members <- read_delim("./data/metadata_for_scraping/folketing_members.csv", delim = ";", col_names = c("Name", "Parti", "Year"))

list <- list_of_members %>% mutate(
  # Hardcoding members with no parti
  Parti = ifelse(Name == "Helge Dohrmann", "Fremskridtspartiet", Parti),
  Parti = ifelse(Name == "Thomas Adelskov", "Socialdemokratiet", Parti),
  # Editing typos in paty names
  Parti = ifelse(Parti == "Socialdemorkatiet", "Socialdemokratiet", Parti),
  Parti = ifelse(Parti == "Socialdemokraterne", "Socialdemokratiet", Parti), # party changed name and reverted (2002)
  Parti = ifelse(Parti == "Slevigsk Parti", "Slesvigsk Parti", Parti),
  Parti = ifelse(Parti == "Tjódveldi", "Tjóðveldi", Parti),
  Parti = ifelse(Parti == "Atassut", "Atássut", Parti),
  Parti = ifelse(Parti == "Folkaflokkurin", "Fólkaflokkurin", Parti),
  Parti = ifelse(Parti == "Javnadarflokkurin", "Javnaðarflokkurin", Parti),
  Parti = ifelse(Parti == "Tjódveldisflokkurin", "Tjóðveldi", Parti),
  Parti = ifelse(str_detect(string = Parti, pattern = "Centrum-Demokraterne"), "Centrum-Demokraterne", Parti),
  Parti = ifelse(str_detect(string = Parti, pattern = "Socialdemokrat"), "Socialdemokratiet", Parti),
  Parti = ifelse(str_detect(string = Parti, pattern = "Konservative Folkeparti"), "Det Konservative Folkeparti", Parti),
  Parti = ifelse(str_detect(string = Parti, pattern = "Radikale Venstre"), "Radikale Venstre", Parti),
  Parti = ifelse(str_detect(string = Parti, pattern = "Socialistisk Folkeparti"), "Socialistisk Folkeparti", Parti),
  Parti = ifelse(str_detect(string = Parti, pattern = "Kristeligt Folkeparti"), "Kristeligt Folkeparti", Parti),
  Parti = str_replace(Parti, "x - ", "")
) 

hep <- list %>% distinct(Parti)

write_delim("./data/metadata_for_scraping/folketing_members.csv", col_names = FALSE, delim = ";", x = list)

