library(dplyr)

source("functions/world_map.R")

download.corona.data <- function(file.path = "corona.csv"){
  if (format(file.info(file.path)$ctime, '%Y-%m-%d') != Sys.Date()) {
    download.file(url = 'https://opendata.ecdc.europa.eu/covid19/casedistribution/csv', 
                  destfile = file.path)
  }
}

get.corona.data <- function(file.path = "corona.csv", world_map = create_world_map(), norm.size = 100000){
  corona <- read.csv('corona.csv') %>% 
    mutate_at(vars(dateRep), 
              funs(as.Date(., format='%d/%m/%Y'))) %>% 
    arrange(dateRep) %>%
    rename(country = countriesAndTerritories) %>%
    filter(countryterritoryCode %in% world_map$iso3) %>%
    group_by(country) %>%
    mutate(total_cases = cumsum(cases), 
           total_deaths = cumsum(deaths)) %>%
    mutate(relative_cases = cases / (popData2018 / norm.size),
           relative_deaths = deaths / (popData2018 / norm.size),
           relative_total_cases = total_cases / (popData2018 / norm.size),
           relative_total_deaths = total_deaths / (popData2018 / norm.size))
}