#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")




library(ggplot2)
library(tidyverse)
library(dplyr)
library(tseries)
library(forecast)
library(fpp2)
library(knitr)
library(stringr)
library(DT)
library(fpp)
library(lattice)
library(lubridate)
library(stringr)
library(sf)
library(leaflet)

# set mapping colour for each disease
breast_cancer_col = "#cc4c02"
cervical_cancer_col = "#662506"
colonandrectum_cancer_col = "#045a8d"
liver_cancer_col = "#4d004b"

# import data

breast_cancer_deaths_per_100000_women = read.csv("data/breast_cancer_deaths_per_100000_women.csv")
breast_cancer_new_cases_per_100000_women = read.csv("data/breast_cancer_new_cases_per_100000_women.csv")
breast_cancer_number_of_female_deaths = read.csv("data/breast_cancer_number_of_female_deaths.csv")
breast_cancer_number_of_new_female_cases = read.csv("data/breast_cancer_number_of_new_female_cases.csv")
cervical_cancer_deaths_per_100000_women = read.csv("data/cervical_cancer_deaths_per_100000_women.csv")
cervical_cancer_new_cases_per_100000_women = read.csv("data/cervical_cancer_new_cases_per_100000_women.csv")
cervical_cancer_number_of_female_deaths = read.csv("data/cervical_cancer_number_of_female_deaths.csv")
cervical_cancer_number_of_new_female_cases = read.csv("data/cervical_cancer_number_of_new_female_cases.csv")
colonandrectum_cancer_deaths_per_100000_men = read.csv("data/colonandrectum_cancer_deaths_per_100000_men.csv")
colonandrectum_cancer_new_cases_per_100000_men = read.csv("data/colonandrectum_cancer_new_cases_per_100000_men.csv")
colonandrectum_cancer_number_of_male_deaths = read.csv("data/colonandrectum_cancer_number_of_male_deaths.csv")
colonandrectum_cancer_number_of_new_male_cases = read.csv("data/colonandrectum_cancer_number_of_new_male_cases.csv")
liver_cancer_deaths_per_100000_men = read.csv("data/liver_cancer_deaths_per_100000_men.csv")
liver_cancer_new_cases_per_100000_men = read.csv("data/liver_cancer_new_cases_per_100000_men.csv")
liver_cancer_number_of_male_deaths = read.csv("data/liver_cancer_number_of_male_deaths.csv")
liver_cancer_number_of_new_male_cases = read.csv("data/liver_cancer_number_of_new_male_cases.csv")
countries_codes_and_coordinates = read.csv("data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("worldcountry.geojson", what = "sp")

### DATA PROCESSING###
prep.ztbl <- function(atbl){
  c.nondate = length(which(!str_detect(colnames(atbl), "^X")))
  c.nondate
  range.datecol = (c.nondate+1):length(colnames(atbl))
  range.datecol
  tbl = atbl %>% gather("year", "Value", range.datecol)
  tbl$year <- tbl$year %>%  str_replace('X', '') 
  tbl <- subset(tbl, year > 1989)
}

### DATA PROCESSING: breast_cancer###
breast_cancer_deaths_per_100000_women<-prep.ztbl(breast_cancer_deaths_per_100000_women)
breast_cancer_new_cases_per_100000_women<-prep.ztbl(breast_cancer_new_cases_per_100000_women)
breast_cancer_number_of_female_deaths<-prep.ztbl(breast_cancer_number_of_female_deaths)
breast_cancer_number_of_new_female_cases<-prep.ztbl(breast_cancer_number_of_new_female_cases)

colnames(breast_cancer_deaths_per_100000_women)[3] <- "breast_cancer_deaths_per_100000_women"
colnames(breast_cancer_new_cases_per_100000_women)[3] <- "breast_cancer_new_cases_per_100000_women"
colnames(breast_cancer_number_of_female_deaths)[3] <- "breast_cancer_number_of_female_deaths"
colnames(breast_cancer_number_of_new_female_cases)[3] <- "breast_cancer_number_of_new_female_cases"

breast_cancer <- 
  breast_cancer_deaths_per_100000_women %>% 
  merge(breast_cancer_new_cases_per_100000_women, by = c("country", "year")) %>% 
  merge(breast_cancer_number_of_female_deaths,by = c("country", "year")) %>% 
  merge(breast_cancer_number_of_new_female_cases, by = c("country", "year"))

breast_cancer[is.na(breast_cancer)] <- 0
breast_cancer$breast_cancer_number_of_female_deaths<- 
  as.numeric(sub("k", "e3", breast_cancer$breast_cancer_number_of_female_deaths, fixed = TRUE))
breast_cancer$breast_cancer_number_of_new_female_cases<- 
  as.numeric(sub("k", "e3", breast_cancer$breast_cancer_number_of_new_female_cases, fixed = TRUE))
breast_cancer[is.na(breast_cancer)] <- 0

breast_cancer$death_rate <- round(breast_cancer$breast_cancer_deaths_per_100000_women/
                                    breast_cancer$breast_cancer_new_cases_per_100000_women, 2)
breast_cancer[is.na(breast_cancer)] <- 0

# extract dates from breast_cancer data
min_year = min(breast_cancer$year)
current_year = max(breast_cancer$year)
max_year = max(breast_cancer$year)


# merge breast_cancer data with country data and extract key summary variables
breast_cancer = merge(breast_cancer, countries_codes_and_coordinates, by = "country")

# create subset of state data for latest data
breast_cancer_latest = subset(breast_cancer, year==max_year)

# aggregate at continent level
breast_cancer_continent = subset(breast_cancer, !is.na(continent_level)) %>% 
  select(c(breast_cancer_number_of_female_deaths, 
           breast_cancer_number_of_new_female_cases, 
           year,
           continent_level)) %>% 
  group_by(continent_level, year) %>% summarise_each(funs(sum)) %>% data.frame()

                                                  
# add continent populations
breast_cancer_continent$pop = NA	
breast_cancer_continent$pop[breast_cancer_continent$continent=="Africa"] = 1.2e9
breast_cancer_continent$pop[breast_cancer_continent$continent=="Asia"] = 4.5e9
breast_cancer_continent$pop[breast_cancer_continent$continent=="Europe"] = 7.4e8
breast_cancer_continent$pop[breast_cancer_continent$continent=="North America"] = 5.8e8
breast_cancer_continent$pop[breast_cancer_continent$continent=="Oceania"] = 3.8e7
breast_cancer_continent$pop[breast_cancer_continent$continent=="South America"] = 4.2e8

# add normalised counts
breast_cancer_continent$breast_cancer_deaths_per_100000_women =  
  as.numeric(format(round(breast_cancer_continent$breast_cancer_number_of_female_deaths/(breast_cancer_continent$pop/100000),1),nsmall=1))	
breast_cancer_continent$breast_cancer_new_cases_per_100000_women =  
  as.numeric(format(round(breast_cancer_continent$breast_cancer_number_of_new_female_cases/(breast_cancer_continent$pop/100000),1),nsmall=1))
breast_cancer_continent$death_rate = 
  as.numeric(format(round(breast_cancer_continent$breast_cancer_number_of_female_deaths/(breast_cancer_continent$breast_cancer_number_of_new_female_cases),2),nsmall=1))
  
# aggregate at global level
breast_cancer_global = breast_cancer %>% 
  select(c(breast_cancer_number_of_female_deaths, 
           breast_cancer_number_of_new_female_cases, 
           year,
           global_level)) %>% 
  group_by(global_level, year) %>% 
  summarise_each(funs(sum)) %>% data.frame()
breast_cancer_global$death_rate = round(breast_cancer_global$breast_cancer_number_of_female_deaths/
                                          breast_cancer_global$breast_cancer_number_of_new_female_cases,2)
  

# add normalised counts
breast_cancer_global$pop = 7.6e9
breast_cancer_global$breast_cancer_deaths_per_100000_women =  
  as.numeric(format(round(breast_cancer_global$breast_cancer_number_of_female_deaths/(breast_cancer_global$pop/100000),1),nsmall=1))
breast_cancer_global$breast_cancer_new_cases_per_100000_women =  
  as.numeric(format(round(breast_cancer_global$breast_cancer_number_of_new_female_cases/(breast_cancer_global$pop/100000),1),nsmall=1))

# create plotting parameters for map
bins = c(0, 10,20,30,40, 50,Inf)
breast_cancer_pal <- colorBin("Oranges", domain = breast_cancer$breast_cancer_number_of_new_female_cases, bins = bins)
worldcountry[worldcountry$ADM0_A3 %in% countries_codes_and_coordinates$alpha3, ]

# sum breast cancer case counts by date
breast_cancer_aggregated = aggregate(breast_cancer$breast_cancer_number_of_new_female_cases, by=list(Category=breast_cancer$year), FUN=sum)
names(breast_cancer_aggregated) = c("year", "cases")
breast_cancer_aggregated

# add plotting region
breast_cancer_aggregated$region = "Global"

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), 
            brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), 
            brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  
            brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)

cls_names = c(as.character(unique(breast_cancer$country)), 
              as.character(unique(breast_cancer_continent$continent)), 
              "Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names




### DATA PROCESSING: cervical_cancer###
cervical_cancer_deaths_per_100000_women<-prep.ztbl(cervical_cancer_deaths_per_100000_women)
cervical_cancer_new_cases_per_100000_women<-prep.ztbl(cervical_cancer_new_cases_per_100000_women)
cervical_cancer_number_of_female_deaths<-prep.ztbl(cervical_cancer_number_of_female_deaths)
cervical_cancer_number_of_new_female_cases<-prep.ztbl(cervical_cancer_number_of_new_female_cases)

colnames(cervical_cancer_deaths_per_100000_women)[3] <- "cervical_cancer_deaths_per_100000_women"
colnames(cervical_cancer_new_cases_per_100000_women)[3] <- "cervical_cancer_new_cases_per_100000_women"
colnames(cervical_cancer_number_of_female_deaths)[3] <- "cervical_cancer_number_of_female_deaths"
colnames(cervical_cancer_number_of_new_female_cases)[3] <- "cervical_cancer_number_of_new_female_cases"

cervical_cancer <- 
  cervical_cancer_deaths_per_100000_women %>% 
  merge(cervical_cancer_new_cases_per_100000_women, by = c("country", "year")) %>% 
  merge(cervical_cancer_number_of_female_deaths,by = c("country", "year")) %>% 
  merge(cervical_cancer_number_of_new_female_cases, by = c("country", "year"))

cervical_cancer[is.na(cervical_cancer)] <- 0
cervical_cancer$cervical_cancer_number_of_female_deaths<- 
  as.numeric(sub("k", "e3", cervical_cancer$cervical_cancer_number_of_female_deaths, fixed = TRUE))
cervical_cancer$cervical_cancer_number_of_new_female_cases<- 
  as.numeric(sub("k", "e3", cervical_cancer$cervical_cancer_number_of_new_female_cases, fixed = TRUE))
cervical_cancer[is.na(cervical_cancer)] <- 0


cervical_cancer$death_rate <- round(cervical_cancer$cervical_cancer_number_of_female_deaths/
                                      cervical_cancer$cervical_cancer_number_of_new_female_cases, 2)
cervical_cancer[is.na(cervical_cancer)] <- 0

# extract dates from cervical_cancer data
min_year = min(cervical_cancer$year)
current_year = max(cervical_cancer$year)
max_year = max(cervical_cancer$year)


# merge cervical_cancer data with country data and extract key summary variables
cervical_cancer = merge(cervical_cancer, countries_codes_and_coordinates, by = "country")

# create subset of state data for latest data
cervical_cancer_latest = subset(cervical_cancer, year==max_year)

# aggregate at continent level
cervical_cancer_continent = subset(cervical_cancer, !is.na(continent_level)) %>% 
  select(c(cervical_cancer_number_of_female_deaths, 
           cervical_cancer_number_of_new_female_cases, 
           year,
           continent_level)) %>% 
  group_by(continent_level, year) %>% summarise_each(funs(sum)) %>% data.frame()


# add continent populations
cervical_cancer_continent$pop = NA	
cervical_cancer_continent$pop[cervical_cancer_continent$continent=="Africa"] = 1.2e9
cervical_cancer_continent$pop[cervical_cancer_continent$continent=="Asia"] = 4.5e9
cervical_cancer_continent$pop[cervical_cancer_continent$continent=="Europe"] = 7.4e8
cervical_cancer_continent$pop[cervical_cancer_continent$continent=="North America"] = 5.8e8
cervical_cancer_continent$pop[cervical_cancer_continent$continent=="Oceania"] = 3.8e7
cervical_cancer_continent$pop[cervical_cancer_continent$continent=="South America"] = 4.2e8

# add normalised counts
cervical_cancer_continent$cervical_cancer_deaths_per_100000_women =  
  as.numeric(format(round(cervical_cancer_continent$cervical_cancer_number_of_female_deaths/(cervical_cancer_continent$pop/100000),1),nsmall=1))	
cervical_cancer_continent$cervical_cancer_new_cases_per_100000_women =  
  as.numeric(format(round(cervical_cancer_continent$cervical_cancer_number_of_new_female_cases/(cervical_cancer_continent$pop/100000),1),nsmall=1))
cervical_cancer_continent$death_rate = 
  as.numeric(format(round(cervical_cancer_continent$cervical_cancer_number_of_female_deaths/(cervical_cancer_continent$cervical_cancer_number_of_new_female_cases),2),nsmall=1))

# aggregate at global level
cervical_cancer_global = cervical_cancer %>% 
  select(c(cervical_cancer_number_of_female_deaths, 
           cervical_cancer_number_of_new_female_cases, 
           year,
           global_level)) %>% 
  group_by(global_level, year) %>% 
  summarise_each(funs(sum)) %>% data.frame()
cervical_cancer_global$death_rate = round(cervical_cancer_global$cervical_cancer_number_of_female_deaths/
                                            cervical_cancer_global$cervical_cancer_number_of_new_female_cases,2)


# add normalised counts
cervical_cancer_global$pop = 7.6e9
cervical_cancer_global$cervical_cancer_deaths_per_100000_women =  
  as.numeric(format(round(cervical_cancer_global$cervical_cancer_number_of_female_deaths/(cervical_cancer_global$pop/100000),1),nsmall=1))
cervical_cancer_global$cervical_cancer_new_cases_per_100000_women =  
  as.numeric(format(round(cervical_cancer_global$cervical_cancer_number_of_new_female_cases/(cervical_cancer_global$pop/100000),1),nsmall=1))

# create plotting parameters for map
bins = c(0, 10,20,30,40, 50,Inf)
cervical_cancer_pal <- colorBin("Blue", domain = cervical_cancer$cervical_cancer_number_of_new_female_cases, bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% countries_codes_and_coordinates$alpha3, ]

# sum cervical_cancer case counts by date
cervical_cancer_aggregated = aggregate(cervical_cancer$cervical_cancer_number_of_new_female_cases, by=list(Category=cervical_cancer$year), FUN=sum)
names(cervical_cancer_aggregated) = c("year", "cases")
cervical_cancer_aggregated

# add plotting region
cervical_cancer_aggregated$region = "Global"



### DATA PROCESSING: colonandrectum_cancer###
colonandrectum_cancer_deaths_per_100000_men<-prep.ztbl(colonandrectum_cancer_deaths_per_100000_men)
colonandrectum_cancer_new_cases_per_100000_men<-prep.ztbl(colonandrectum_cancer_new_cases_per_100000_men)
colonandrectum_cancer_number_of_male_deaths<-prep.ztbl(colonandrectum_cancer_number_of_male_deaths)
colonandrectum_cancer_number_of_new_male_cases<-prep.ztbl(colonandrectum_cancer_number_of_new_male_cases)

colnames(colonandrectum_cancer_deaths_per_100000_men)[3] <- "colonandrectum_cancer_deaths_per_100000_men"
colnames(colonandrectum_cancer_new_cases_per_100000_men)[3] <- "colonandrectum_cancer_new_cases_per_100000_men"
colnames(colonandrectum_cancer_number_of_male_deaths)[3] <- "colonandrectum_cancer_number_of_male_deaths"
colnames(colonandrectum_cancer_number_of_new_male_cases)[3] <- "colonandrectum_cancer_number_of_new_male_cases"

colonandrectum_cancer <- 
  colonandrectum_cancer_deaths_per_100000_men %>% 
  merge(colonandrectum_cancer_new_cases_per_100000_men, by = c("country", "year")) %>% 
  merge(colonandrectum_cancer_number_of_male_deaths,by = c("country", "year")) %>% 
  merge(colonandrectum_cancer_number_of_new_male_cases, by = c("country", "year"))

colonandrectum_cancer[is.na(colonandrectum_cancer)] <- 0
colonandrectum_cancer$colonandrectum_cancer_number_of_male_deaths<- 
  as.numeric(sub("k", "e3", colonandrectum_cancer$colonandrectum_cancer_number_of_male_deaths, fixed = TRUE))
colonandrectum_cancer$colonandrectum_cancer_number_of_new_male_cases<- 
  as.numeric(sub("k", "e3", colonandrectum_cancer$colonandrectum_cancer_number_of_new_male_cases, fixed = TRUE))
colonandrectum_cancer[is.na(colonandrectum_cancer)] <- 0


colonandrectum_cancer$death_rate <- round(colonandrectum_cancer$colonandrectum_cancer_number_of_male_deaths/
                                            colonandrectum_cancer$colonandrectum_cancer_number_of_new_male_cases, 2)
colonandrectum_cancer[is.na(colonandrectum_cancer)] <- 0

# extract dates from colonandrectum_cancer data
min_year = min(colonandrectum_cancer$year)
current_year = max(colonandrectum_cancer$year)
max_year = max(colonandrectum_cancer$year)


# merge colonandrectum_cancer data with country data and extract key summary variables
colonandrectum_cancer = merge(colonandrectum_cancer, countries_codes_and_coordinates, by = "country")

# create subset of state data for latest data
colonandrectum_cancer_latest = subset(colonandrectum_cancer, year==max_year)

# aggregate at continent level
colonandrectum_cancer_continent = subset(colonandrectum_cancer, !is.na(continent_level)) %>% 
  select(c(colonandrectum_cancer_number_of_male_deaths, 
           colonandrectum_cancer_number_of_new_male_cases, 
           year,
           continent_level)) %>% 
  group_by(continent_level, year) %>% summarise_each(funs(sum)) %>% data.frame()


# add continent populations
colonandrectum_cancer_continent$pop = NA	
colonandrectum_cancer_continent$pop[colonandrectum_cancer_continent$continent=="Africa"] = 1.2e9
colonandrectum_cancer_continent$pop[colonandrectum_cancer_continent$continent=="Asia"] = 4.5e9
colonandrectum_cancer_continent$pop[colonandrectum_cancer_continent$continent=="Europe"] = 7.4e8
colonandrectum_cancer_continent$pop[colonandrectum_cancer_continent$continent=="North America"] = 5.8e8
colonandrectum_cancer_continent$pop[colonandrectum_cancer_continent$continent=="Oceania"] = 3.8e7
colonandrectum_cancer_continent$pop[colonandrectum_cancer_continent$continent=="South America"] = 4.2e8

# add normalised counts
colonandrectum_cancer_continent$colonandrectum_cancer_deaths_per_100000_men =  
  as.numeric(format(round(colonandrectum_cancer_continent$colonandrectum_cancer_number_of_male_deaths/(colonandrectum_cancer_continent$pop/100000),1),nsmall=1))	
colonandrectum_cancer_continent$colonandrectum_cancer_new_cases_per_100000_men =  
  as.numeric(format(round(colonandrectum_cancer_continent$colonandrectum_cancer_number_of_new_male_cases/(colonandrectum_cancer_continent$pop/100000),1),nsmall=1))
colonandrectum_cancer_continent$death_rate = 
  as.numeric(format(round(colonandrectum_cancer_continent$colonandrectum_cancer_number_of_male_deaths/(colonandrectum_cancer_continent$colonandrectum_cancer_number_of_new_male_cases),2),nsmall=1))

# aggregate at global level
colonandrectum_cancer_global = colonandrectum_cancer %>% 
  select(c(colonandrectum_cancer_number_of_male_deaths, 
           colonandrectum_cancer_number_of_new_male_cases, 
           year,
           global_level)) %>% 
  group_by(global_level, year) %>% 
  summarise_each(funs(sum)) %>% data.frame()
colonandrectum_cancer_global$death_rate = round(colonandrectum_cancer_global$colonandrectum_cancer_number_of_male_deaths/
                                                  colonandrectum_cancer_global$colonandrectum_cancer_number_of_new_male_cases,2)


# add normalised counts
colonandrectum_cancer_global$pop = 7.6e9
colonandrectum_cancer_global$colonandrectum_cancer_deaths_per_100000_men =  
  as.numeric(format(round(colonandrectum_cancer_global$colonandrectum_cancer_number_of_male_deaths/(colonandrectum_cancer_global$pop/100000),1),nsmall=1))
colonandrectum_cancer_global$colonandrectum_cancer_new_cases_per_100000_men =  
  as.numeric(format(round(colonandrectum_cancer_global$colonandrectum_cancer_number_of_new_male_cases/(colonandrectum_cancer_global$pop/100000),1),nsmall=1))

# create plotting parameters for map
bins = c(0, 10,20,30,40, 50,Inf)
colonandrectum_cancer_pal <- colorBin("Blue", domain = colonandrectum_cancer$colonandrectum_cancer_number_of_new_male_cases, bins = bins)


# sum colonandrectum_cancer case counts by date
colonandrectum_cancer_aggregated = aggregate(colonandrectum_cancer$colonandrectum_cancer_number_of_new_male_cases, by=list(Category=colonandrectum_cancer$year), FUN=sum)
names(colonandrectum_cancer_aggregated) = c("year", "cases")

# add plotting region
colonandrectum_cancer_aggregated$region = "Global"


### DATA PROCESSING: liver_cancer###
liver_cancer_deaths_per_100000_men<-prep.ztbl(liver_cancer_deaths_per_100000_men)
liver_cancer_new_cases_per_100000_men<-prep.ztbl(liver_cancer_new_cases_per_100000_men)
liver_cancer_number_of_male_deaths<-prep.ztbl(liver_cancer_number_of_male_deaths)
liver_cancer_number_of_new_male_cases<-prep.ztbl(liver_cancer_number_of_new_male_cases)

colnames(liver_cancer_deaths_per_100000_men)[3] <- "liver_cancer_deaths_per_100000_men"
colnames(liver_cancer_new_cases_per_100000_men)[3] <- "liver_cancer_new_cases_per_100000_men"
colnames(liver_cancer_number_of_male_deaths)[3] <- "liver_cancer_number_of_male_deaths"
colnames(liver_cancer_number_of_new_male_cases)[3] <- "liver_cancer_number_of_new_male_cases"

liver_cancer <- 
  liver_cancer_deaths_per_100000_men %>% 
  merge(liver_cancer_new_cases_per_100000_men, by = c("country", "year")) %>% 
  merge(liver_cancer_number_of_male_deaths,by = c("country", "year")) %>% 
  merge(liver_cancer_number_of_new_male_cases, by = c("country", "year"))

liver_cancer[is.na(liver_cancer)] <- 0
liver_cancer$liver_cancer_number_of_male_deaths<- 
  as.numeric(sub("k", "e3", liver_cancer$liver_cancer_number_of_male_deaths, fixed = TRUE))
liver_cancer$liver_cancer_number_of_new_male_cases<- 
  as.numeric(sub("k", "e3", liver_cancer$liver_cancer_number_of_new_male_cases, fixed = TRUE))
liver_cancer[is.na(liver_cancer)] <- 0


liver_cancer$death_rate <- round(liver_cancer$liver_cancer_number_of_male_deaths/
                                   liver_cancer$liver_cancer_number_of_new_male_cases, 2)
liver_cancer[is.na(liver_cancer)] <- 0

# extract dates from liver_cancer data
min_year = min(liver_cancer$year)
current_year = max(liver_cancer$year)
max_year = max(liver_cancer$year)


# merge liver_cancer data with country data and extract key summary variables
liver_cancer = merge(liver_cancer, countries_codes_and_coordinates, by = "country")

# create subset of state data for latest data
liver_cancer_latest = subset(liver_cancer, year==max_year)

# aggregate at continent level
liver_cancer_continent = subset(liver_cancer, !is.na(continent_level)) %>% 
  select(c(liver_cancer_number_of_male_deaths, 
           liver_cancer_number_of_new_male_cases, 
           year,
           continent_level)) %>% 
  group_by(continent_level, year) %>% summarise_each(funs(sum)) %>% data.frame()


# add continent populations
liver_cancer_continent$pop = NA	
liver_cancer_continent$pop[liver_cancer_continent$continent=="Africa"] = 1.2e9
liver_cancer_continent$pop[liver_cancer_continent$continent=="Asia"] = 4.5e9
liver_cancer_continent$pop[liver_cancer_continent$continent=="Europe"] = 7.4e8
liver_cancer_continent$pop[liver_cancer_continent$continent=="North America"] = 5.8e8
liver_cancer_continent$pop[liver_cancer_continent$continent=="Oceania"] = 3.8e7
liver_cancer_continent$pop[liver_cancer_continent$continent=="South America"] = 4.2e8

# add normalised counts
liver_cancer_continent$liver_cancer_deaths_per_100000_men =  
  as.numeric(format(round(liver_cancer_continent$liver_cancer_number_of_male_deaths/(liver_cancer_continent$pop/100000),1),nsmall=1))	
liver_cancer_continent$liver_cancer_new_cases_per_100000_men =  
  as.numeric(format(round(liver_cancer_continent$liver_cancer_number_of_new_male_cases/(liver_cancer_continent$pop/100000),1),nsmall=1))
liver_cancer_continent$death_rate = 
  as.numeric(format(round(liver_cancer_continent$liver_cancer_number_of_male_deaths/(liver_cancer_continent$liver_cancer_number_of_new_male_cases),2),nsmall=1))

# aggregate at global level
liver_cancer_global = liver_cancer %>% 
  select(c(liver_cancer_number_of_male_deaths, 
           liver_cancer_number_of_new_male_cases, 
           year,
           global_level)) %>% 
  group_by(global_level, year) %>% 
  summarise_each(funs(sum)) %>% data.frame()
liver_cancer_global$death_rate = round(liver_cancer_global$liver_cancer_number_of_male_deaths/
                                         liver_cancer_global$liver_cancer_number_of_new_male_cases,2)


# add normalised counts
liver_cancer_global$pop = 7.6e9
liver_cancer_global$liver_cancer_deaths_per_100000_men =  
  as.numeric(format(round(liver_cancer_global$liver_cancer_number_of_male_deaths/(liver_cancer_global$pop/100000),1),nsmall=1))
liver_cancer_global$liver_cancer_new_cases_per_100000_men =  
  as.numeric(format(round(liver_cancer_global$liver_cancer_number_of_new_male_cases/(liver_cancer_global$pop/100000),1),nsmall=1))

# create plotting parameters for map
bins = c(0, 10,20,30,40, 50,Inf)
liver_cancer_pal <- colorBin("Blue", domain = liver_cancer$liver_cancer_number_of_new_male_cases, bins = bins)


# sum liver_cancer case counts by date
liver_cancer_aggregated = aggregate(liver_cancer$liver_cancer_number_of_new_male_cases, by=list(Category=liver_cancer$year), FUN=sum)
names(liver_cancer_aggregated) = c("year", "cases")

# add plotting region
liver_cancer_aggregated$region = "Global"


###Create plot

# create interactive_map_death 
# (needs to include polygons and circles as slider input not recognised upon initial loading)
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Breast Cancer", "Cervical Cancer", "Colon and Rectum Cancer", "Liver Cancer"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Cervical Cancer", "Colon and Rectum Cancer", "Liver Cancer")) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70)




# function to plot breast_cancer global cases by year
breast_cancer_aggregated_plot = function(breast_cancer_aggregated, plot_date) {
  plot_df = subset(breast_cancer_aggregated, year<=plot_date)
  g1 = ggplot(plot_df, aes(x = year, y = cases, group = 1)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("Breast cancer new cases") +  xlab("Year") + theme_bw() + 
    scale_colour_manual(values=c(breast_cancer_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_x_discrete(breaks = seq(0, 2200, by = 5))
  g1
}

# function to plot cervical_cancer global cases by year
cervical_cancer_aggregated_plot = function(cervical_cancer_aggregated, plot_date) {
  plot_df = subset(cervical_cancer_aggregated, year<=plot_date)
  g1 = ggplot(plot_df, aes(x = year, y = cases, group = 1)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("cervical_cancer new cases") +  xlab("Year") + theme_bw() + 
    scale_colour_manual(values=c(cervical_cancer_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_x_discrete(breaks = seq(0, 2200, by = 5))
  g1
}

# function to plot colonandrectum_cancer global cases by year
colonandrectum_cancer_aggregated_plot = function(colonandrectum_cancer_aggregated, plot_date) {
  plot_df = subset(colonandrectum_cancer_aggregated, year<=plot_date)
  g1 = ggplot(plot_df, aes(x = year, y = cases, group = 1)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("colonandrectum_cancer new cases") +  xlab("Year") + theme_bw() + 
    scale_colour_manual(values=c(colonandrectum_cancer_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_x_discrete(breaks = seq(0, 2200, by = 5))
  g1
}

# function to plot liver_cancer global cases by year
liver_cancer_aggregated_plot = function(liver_cancer_aggregated, plot_date) {
  plot_df = subset(liver_cancer_aggregated, year<=plot_date)
  g1 = ggplot(plot_df, aes(x = year, y = cases, group = 1)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("liver_cancer new cases") +  xlab("Year") + theme_bw() + 
    scale_colour_manual(values=c(liver_cancer_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_x_discrete(breaks = seq(0, 2200, by = 5))
  g1
}


# function to plot breast_cancer by region
breast_cancer_country_cases_plot = function(breast_cancer) {
    g = ggplot(breast_cancer, aes(x = year, y = new_outcome, color = region, group = 1,
                             text = paste0(format(year), "\n", region, ": ",new_outcome))) + 
       xlab("Date")
    g1 = g +
    geom_line(stat="identity") + 
      ylab("") + theme_bw()  + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      scale_x_discrete(breaks = seq(0, 2200, by = 5))+ 
    scale_fill_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cervical_cancer by region
cervical_cancer_country_cases_plot = function(cervical_cancer) {
  g = ggplot(cervical_cancer, aes(x = year, y = new_outcome, color = region, group = 1,
                                  text = paste0(format(year), "\n", region, ": ",new_outcome))) + 
    xlab("Date")
  g1 = g +
    geom_line( stat="identity") + 
    ylab("") + theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_x_discrete(breaks = seq(0, 2200, by = 5))+ 
    scale_fill_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot colonandrectum_cancer by region
colonandrectum_cancer_country_cases_plot = function(colonandrectum_cancer) {
  g = ggplot(colonandrectum_cancer, aes(x = year, y = new_outcome, color = region, group = 1,
                                        text = paste0(format(year), "\n", region, ": ",new_outcome))) + 
   xlab("Date")
  g1 = g +
    geom_line( stat="identity") + 
    ylab("") + theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_x_discrete(breaks = seq(0, 2200, by = 5))+ 
    scale_fill_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot liver_cancer by region
liver_cancer_country_cases_plot = function(liver_cancer) {
  g = ggplot(liver_cancer, aes(x = year, y = new_outcome, color = region, group = 1,
                               text = paste0(format(year), "\n", region, ": ",new_outcome))) + 
     xlab("Date")
  g1 = g +
    geom_line( stat="identity") + 
    ylab("") + theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_x_discrete(breaks = seq(0, 2200, by = 5))+ 
    scale_fill_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

##### Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" 
                  class="active" href="#">Datazoidsr</a>'), id="nav",
             windowTitle = "Datazoids",
             
             tabPanel("Cancer mapper",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                                        plotOutput("breast_cancer_aggregated_plot", height="130px", width="100%"),
                                        plotOutput("cervical_cancer_aggregated_plot", height="130px", width="100%"),
                                        plotOutput("colonandrectum_cancer_aggregated_plot", height="130px", width="100%"),
                                        plotOutput("liver_cancer_aggregated_plot", height="130px", width="100%"),

                                        sliderTextInput("plot_date",
                                                        label = h5("Select mapping date"),
                                                        choices = c(1990 : 2018),
                                                        selected = max_year,
                                                        grid = FALSE,
                                                        animate=animationOptions(interval = 3000, loop = FALSE))
                                        
                          ),
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                          
                          
                      )
             ),
             
             
             tabPanel("Region plots",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                          
                          pickerInput("level_select", "Level:",   
                                      choices = c("Global", "Continent", "Country"), 
                                      selected = c("Country"),
                                      multiple = FALSE),
                          pickerInput("region_select", "Country/Region:",   
                                      choices = as.character(breast_cancer_latest$country), 
                                      options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                      selected = as.character(breast_cancer_latest$country)[2:3],
                                      multiple = TRUE), 
                          pickerInput("outcome_select", "Outcome:",   
                                      choices = c("Deaths per 10K", "Cases per 10K", "New Cases", "Deaths", "Death Rate"), 
                                      selected = c("Deaths per 10K"),
                                      multiple = FALSE),
                          "Death rate was estimated by Death/New Cases."
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Breast Cancer", plotlyOutput("breast_cancer_country_cases_plot")),
                            tabPanel("Cervical Cancer", plotlyOutput("cervical_cancer_country_cases_plot")),
                            tabPanel("Colon and Rectum Cancer", plotlyOutput("colonandrectum_cancer_country_cases_plot")),
                            tabPanel("Liver Cancer", plotlyOutput("liver_cancer_country_cases_plot"))
                          )
                        )
                      )
             ),
             
             tabPanel("Milestones",
                      fluidRow(
                        column(1),
                        column(5,
                               shiny::HTML("<br><br><center> <h1>1990</h1> </center><br>"),
                               shiny::HTML("<br><br><center> <h2>Revolutionary bladder cancer treatment is developed</h2> </center><br>"),
                               shiny::HTML("<h5>The FDA approves BCG, a bacterial vaccine, for the treatment of bladder cancer,
                                           based on results of a clinical trial conducted by Memorial Sloan Kettering urologic 
                                           surgeon Harry Herr and tumor immunologist Herbert Oettgen (shown). BCG remains a primary 
                                           treatment for non-muscle invasive bladder cancer.</h5>"),
                               shiny::HTML("<br><br><center> <h7>Credit: Memorial Sloan Kettering Cancer Center</h7> </center><br>"),
                        ),
                        column(6,
                               img(src = "1990-BCG.jpeg", height = 500, width = 500)
                               )
                      ),
                      
                      fluidRow(
                        
                        style = "height:70px;"),
             
                      tags$hr(),
                      
                      fluidRow(
                        column(1),
                        column(5,
                               shiny::HTML("<br><br><center> <h1>1994</h1> </center><br>"),
                               shiny::HTML("<br><br><center> <h2>First cancer predisposition gene, BRCA1, is sequenced</h2> </center><br>"),
                               shiny::HTML("<h5>Based on genetic pedigree studies by Mary Claire King, a team from Myriad Genetics clones and sequences the cancer predisposition gene BRCA1. Specific inherited mutations in this gene greatly increase the risks of breast, ovarian, and other cancer types. The following year, the same group sequences the gene BRCA2. These breakthroughs confirm the genetic basis of some cancers..</h5>"),
                               shiny::HTML("<br><br><center> <h7>Credit: James King-Holmes/Icrf/Science Photo Library/Getty</h7> </center><br>"),
                        ),
                        column(6,
                               img(src = "1994-BRCA.webp", height = 500, width = 500)
                        )
                      ),
                      
                      fluidRow(
                        
                        style = "height:70px;"), 
                      tags$hr(),
                      
                               fluidRow(
                                 column(1),
                                 column(5,
                                        shiny::HTML("<br><br><center> <h1>1996</h1> </center><br>"),
                                        shiny::HTML("<br><br><center> <h2>The birth of immune checkpoint blockade therapy</h2> </center><br>"),
                                        shiny::HTML("<h5>TImmunologist James Allison is the first to show that blocking a molecule on immune cells 
                                        called CTLA-4 could cure cancer in mice. This opens a whole new approach to cancer treatment. The approach, called immune checkpoint blockade therapy, is geared toward “releasing the brakes” on the immune system. The 2018 Nobel Prize in Physiology or Medicine was awarded to Drs. Allison and Tasuku Honjo for their work on developing this approach as a treatment for cancer.</h5>"),
                                        shiny::HTML("<br><br><center> <h7>Credit: Steve Gschmeissner/Science Photo Library</h7> </center><br>"),
                                 ),
                                 column(6,
                                        img(src = "1996-Immune.webp", height = 500, width = 500)
                                 )
                               ),
                               
                               fluidRow(
                                 
                                 style = "height:70px;"),
                               
                               tags$hr(),
##
                      fluidRow(
                        column(1),
                        column(5,
                               shiny::HTML("<br><br><center> <h1>2001</h1> </center><br>"),
                               shiny::HTML("<br><br><center> <h2>FDA approves imatinib</h2> </center><br>"),
                               shiny::HTML("<h5>The FDA approves imatinib (Gleevec®) for the treatment of chronic myeloid leukemia. The drug, which interferes with the action of a mutant protein called BCR-ABL, is one of the first successful targeted cancer therapies, and it is now used to treat a number of cancer types. Scientists Brian Druker, Nicholas Lydon, and Charles Sawyers shared the 2009 Lasker~DeBakey Award for their work on the drug’s development.</h5>"),
                               shiny::HTML("<br><br><center> <h7>Credit: Burger/Phanie/Shutterstock</h7> </center><br>"),
                        ),
                        column(6,
                               img(src = "2001-Imatinib.webp", height = 500, width = 500)
                        )
                      ),
                      
                      fluidRow(
                        
                        style = "height:70px;"),
                      
                      tags$hr(),
##
fluidRow(
  column(1),
  column(5,
         shiny::HTML("<br><br><center> <h1>2003</h1> </center><br>"),
         shiny::HTML("<br><br><center> <h2>The complete human genome is sequenced</h2> </center><br>"),
         shiny::HTML("<h5>TThe full sequence of the human genome is completed and published jointly by the Human Genome Project— the international, collaborative research program whose goal was the complete mapping and understanding of all the genes of human beings—and Celera Genomics. The public effort was led by Francis Collins, now the director of the National Institutes of Health, and the private one was led by biochemist and geneticist Craig Venter.</h5>"),
         shiny::HTML("<br><br><center> <h7>Credit: Sebastian Kaulitzki/Getty</h7> </center><br>"),
  ),
  column(6,
         img(src = "2003-GenomeB.webp", height = 500, width = 500)
  )
),

fluidRow(
  
  style = "height:70px;"),

tags$hr(),

##
fluidRow(
  column(1),
  column(5,
         shiny::HTML("<br><br><center> <h1>2006</h1> </center><br>"),
         shiny::HTML("<br><br><center> <h2>FDA approves first HPV vaccine</h2> </center><br>"),
         shiny::HTML("<h5>In an effort to prevent HPV infection, which causes cervical cancer, the FDA approves the first HPV vaccine, Papillomavirus (9-Valent) Vaccine (Gardasil 9®).</h5>"),
         shiny::HTML("<br><br><center> <h7>Credit: Garo/Phanie/Science Photo Library</h7> </center><br>"),
  ),
  column(6,
         img(src = "2006-HPV.webp", height = 500, width = 500)
  )
),

fluidRow(
  
  style = "height:70px;"),

tags$hr(),

##
fluidRow(
  column(1),
  column(5,
         shiny::HTML("<br><br><center> <h1>2011</h1> </center><br>"),
         shiny::HTML("<br><br><center> <h2>The FDA approves first immune checkpoint inhibitor</h2> </center><br>"),
         shiny::HTML("<h5>Following clinical trials led by Memorial Sloan Kettering physician-scientist Jedd Wolchok, the FDA approves the first immune checkpoint inhibitor, ipilimumab (Yervoy®), for the treatment of advanced melanoma. The drug releases a brake on immune cells, allowing them to find and fight cancer. The brake is a protein called CTLA-4.</h5>"),
         shiny::HTML("<br><br><center> <h7>Credit: Mario Margado</h7> </center><br>"),
  ),
  column(6,
         img(src = "2011-MSK.webp", height = 500, width = 500)
  )
),

fluidRow(
  
  style = "height:70px;"),

tags$hr(),

##
fluidRow(
  column(1),
  column(5,
         shiny::HTML("<br><br><center> <h1>2014</h1> </center><br>"),
         shiny::HTML("<br><br><center> <h2>Next wave of checkpoint inhibitors arrive</h2> </center><br>"),
         shiny::HTML("<h5>TThe FDA approves two new immune therapies for melanoma, pembrolizumab (Keytruda®) and nivolumab (Opdivo®), which target a checkpoint protein called PD-1. In 2015, the FDA approves nivolumab and pembrolizumab for non-small cell lung cancer, and nivolumab for renal cell carcinoma.</h5>"),
         shiny::HTML("<br><br><center> <h7>Credit: Steve Gschmeissner/Science Photo Library/Getty</h7> </center><br>"),
  ),
  column(6,
         img(src = "2014-PD1.webp", height = 500, width = 500)
  )
),

fluidRow(
  
  style = "height:70px;"),

tags$hr(),
                      
                      
                      ),             
             
             tabPanel("About this site",
                      tags$div(
                        tags$h4("Copyright"), 
                        "This site is developed for NEU COE IE 6600 Computation and Visualization for Analytics Course Project (2022 fall term)", 
                        tags$br(),
                        "Copyright belongs to NEU COE IE 6600 Class(2022 fall term)",
                        
                        tags$br(),tags$br(),
                        tags$h4("Background"),
                        "Cancer is a leading cause of death worldwide, accounting for nearly 10 million deaths in 2020, or nearly one in six deaths.(WHO)", 
                        tags$br(),
                        "The goal of this project is to identify and analyze the allocation of the 4 most common cancers (breast, cervical, colon and rectum, and  liver cancers) in different con4nents and different countries.",
                        
                        tags$br(),tags$br(),tags$h4("Sources"),
                        tags$b("Cancer data: "), tags$a(href="https://www.gapminder.org/data/","GAPMinder"),
                        tags$br(),
                        tags$b("Country mapping coordinates: "), tags$a(href="https://github.com/martynafford/natural-earth-geojson", "Martyn Afford's Github repository"),
                        
                        tags$br(),tags$br(),tags$h4("Authors"),
                        "Pavan Kumar Reddy Sajjala",
                        tags$br(),
                        "Sainath Nayani",
                        tags$br(),
                        "Zheng Zheng",
                        tags$br(),
                        "Northeastern University, COE, Garduate School.",
                        tags$br(),
                        tags$img(src = "neu_log.png", width = "150px", height = "75px")
                      )
             )
             
  )          
)



### SHINY SERVER ###

server = function(input, output, session) {
  
## Cancer mapper tab 
  formatted_date = reactive({
    input$plot_date
  })
  output$clean_date_reactive <- renderText({
   formatted_date()
  })
  
  reactive_db_breast_cancer = reactive({
    breast_cancer %>% filter(year == formatted_date())
  })

  reactive_db_cervical_cancer= reactive({
    cervical_cancer %>% filter(year == formatted_date())
  })
  
  reactive_db_colonandrectum_cancer = reactive({
    colonandrectum_cancer %>% filter(year == formatted_date())
  })
  
  reactive_db_liver_cancer = reactive({
    liver_cancer %>% filter(year == formatted_date())
  })
  
  output$mymap <- renderLeaflet({
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      
      addCircleMarkers(data = reactive_db_breast_cancer(), lat = ~ latitude, lng = ~ longitude, weight = 1, 
                       radius = ~(breast_cancer_number_of_new_female_cases)^(1/4), 
                       fillOpacity = 0.1, color = breast_cancer_col, 
                       group = "Breast Cancer",
                       label = sprintf("<strong>%s</strong><br/>Bread Cancer Cases: %g<br/>Deaths: %g<br/>Cases per 100k: %g", 
                                       reactive_db_breast_cancer()$country, 
                                       reactive_db_breast_cancer()$breast_cancer_number_of_new_female_cases, 
                                       reactive_db_breast_cancer()$breast_cancer_number_of_female_deaths, 
                                       reactive_db_breast_cancer()$breast_cancer_new_cases_per_100000_women)%>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = breast_cancer_col),
                         textsize = "15px", direction = "auto")) %>%  
      

      addCircleMarkers(data = reactive_db_cervical_cancer(), lat = ~ latitude, lng = ~ longitude, weight = 1, 
                       radius = ~(cervical_cancer_number_of_new_female_cases)^(1/4), 
                       fillOpacity = 0.2, color = cervical_cancer_col, group = "Cervical Cancer",
                       label = sprintf("<strong>%s</strong><br/>Bread Cancer Cases: %g<br/>Deaths: %g<br/>Cases per 100k: %g", 
                                       reactive_db_cervical_cancer()$country, 
                                       reactive_db_cervical_cancer()$cervical_cancer_number_of_new_female_cases, 
                                       reactive_db_cervical_cancer()$cervical_cancer_number_of_female_deaths, 
                                       reactive_db_cervical_cancer()$cervical_cancer_new_cases_per_100000_women)%>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = cervical_cancer_col),
                         textsize = "15px", direction = "auto")) %>% 
      
  
      addCircleMarkers(data = reactive_db_colonandrectum_cancer(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(colonandrectum_cancer_number_of_new_male_cases)^(1/4), 
                       fillOpacity = 0.2, color = colonandrectum_cancer_col, group = "Colon and Rectum Cancer",
                       label = sprintf("<strong>%s</strong><br/>Bread Cancer Cases: %g<br/>Deaths: %g<br/>Cases per 100k: %g", 
                                       reactive_db_colonandrectum_cancer()$country, 
                                       reactive_db_colonandrectum_cancer()$colonandrectum_cancer_number_of_new_male_cases, 
                                       reactive_db_colonandrectum_cancer()$colonandrectum_cancer_number_of_male_deaths, 
                                       reactive_db_colonandrectum_cancer()$colonandrectum_cancer_new_cases_per_100000_men)%>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = colonandrectum_cancer_col),
                         textsize = "15px", direction = "auto")) %>%                 
                      
 
      addCircleMarkers(data = reactive_db_liver_cancer(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(liver_cancer_number_of_new_male_cases)^(1/4), 
                       fillOpacity = 0.2, color = liver_cancer_col, group = "Liver Cancer",
                       label = sprintf("<strong>%s</strong><br/>Bread Cancer Cases: %g<br/>Deaths: %g<br/>Cases per 100k: %g", 
                                       reactive_db_liver_cancer()$country, 
                                       reactive_db_liver_cancer()$liver_cancer_number_of_new_male_cases, 
                                       reactive_db_liver_cancer()$liver_cancer_number_of_male_deaths, 
                                       reactive_db_liver_cancer()$liver_cancer_new_cases_per_100000_men)%>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = liver_cancer_col),
                         textsize = "15px", direction = "auto"))                   

  })
  
  output$breast_cancer_aggregated_plot <- renderPlot({
    breast_cancer_aggregated_plot(breast_cancer_aggregated, formatted_date())
  })
  
  output$cervical_cancer_aggregated_plot <- renderPlot({
    cervical_cancer_aggregated_plot(breast_cancer_aggregated, formatted_date())
  })
  
  output$colonandrectum_cancer_aggregated_plot <- renderPlot({
    colonandrectum_cancer_aggregated_plot(breast_cancer_aggregated, formatted_date())
  })
  
  output$liver_cancer_aggregated_plot <- renderPlot({
    liver_cancer_aggregated_plot(breast_cancer_aggregated, formatted_date())
  })
      
## Region Plots
# update region selections
  
  observeEvent(input$level_select, {
    if (input$level_select=="Global") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = "Global", selected = "Global")
    }
    
    if (input$level_select=="Continent") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                        selected = c("Africa", "Asia", "Europe", "North America", "South America"))
    }
    
    
    if (input$level_select=="Country") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(breast_cancer_latest[order(breast_cancer_latest$breast_cancer_number_of_new_female_cases),]$country), 
                        selected = as.character(breast_cancer_latest[order(breast_cancer_latest$breast_cancer_number_of_new_female_cases),]$country)[2:3])
    }
  }, ignoreInit = TRUE)
  
  # create dataframe with selected countries
  country_reactive_db_breast_cancer = reactive({
    if (input$level_select=="Global") { 
      db = breast_cancer_global
      db$region = db$global_level
    }
    if (input$level_select=="Continent") { 
      db = breast_cancer_continent 
      db$region = db$continent
    }
    if (input$level_select=="Country") { 
      db = breast_cancer
      db$region = db$country
    }
    
    if (input$outcome_select=="Deaths per 10K") { 
      db$new_outcome = db$breast_cancer_deaths_per_100000_women 
    }
    
    if (input$outcome_select=="Cases per 10K") { 
      db$new_outcome = db$breast_cancer_new_cases_per_100000_women
    }
    
    if (input$outcome_select=="New Cases") { 
      db$new_outcome = db$breast_cancer_number_of_new_female_cases
    }
    
    if (input$outcome_select=="Deaths") { 
      db$new_outcome = db$breast_cancer_number_of_female_deaths
    }  
      
    if (input$outcome_select=="Death Rate") { 
      db$new_outcome = db$death_rate 
    }  
    
    db %>% filter(region %in% input$region_select)
  })
  
  country_reactive_db_cervical_cancer = reactive({
    if (input$level_select=="Global") { 
      db = cervical_cancer_global
      db$region = db$global_level
    }
    if (input$level_select=="Continent") { 
      db = cervical_cancer_continent 
      db$region = db$continent
    }
    if (input$level_select=="Country") { 
      db = cervical_cancer
      db$region = db$country
    }
    
    if (input$outcome_select=="Deaths per 10K") { 
      db$new_outcome = db$cervical_cancer_deaths_per_100000_women 
    }
    
    if (input$outcome_select=="Cases per 10K") { 
      db$new_outcome = db$cervical_cancer_new_cases_per_100000_women
    }
    
    if (input$outcome_select=="New Cases") { 
      db$new_outcome = db$cervical_cancer_number_of_new_female_cases
    }
    
    if (input$outcome_select=="Deaths") { 
      db$new_outcome = db$cervical_cancer_number_of_female_deaths
    }  
    
    if (input$outcome_select=="Death Rate") { 
      db$new_outcome = db$death_rate 
    }  
    
    db %>% filter(region %in% input$region_select)
  })
  
  country_reactive_db_colonandrectum_cancer = reactive({
    if (input$level_select=="Global") { 
      db = colonandrectum_cancer_global
      db$region = db$global_level
    }
    if (input$level_select=="Continent") { 
      db = colonandrectum_cancer_continent 
      db$region = db$continent
    }
    if (input$level_select=="Country") { 
      db = colonandrectum_cancer
      db$region = db$country
    }
    
    if (input$outcome_select=="Deaths per 10K") { 
      db$new_outcome = db$colonandrectum_cancer_deaths_per_100000_men
    }
    
    if (input$outcome_select=="Cases per 10K") { 
      db$new_outcome = db$colonandrectum_cancer_new_cases_per_100000_men
    }
    
    if (input$outcome_select=="New Cases") { 
      db$new_outcome = db$colonandrectum_cancer_number_of_new_male_cases
    }
    
    if (input$outcome_select=="Deaths") { 
      db$new_outcome = db$ccolonandrectum_cancer_number_of_male_deaths 
    }  
    
    if (input$outcome_select=="Death Rate") { 
      db$new_outcome = db$death_rate 
    }  
    
    db %>% filter(region %in% input$region_select)
  })
  
  country_reactive_db_liver_cancer = reactive({
    if (input$level_select=="Global") { 
      db = liver_cancer_global
      db$region = db$global_level
    }
    if (input$level_select=="Continent") { 
      db = liver_cancer_continent 
      db$region = db$continent
    }
    if (input$level_select=="Country") { 
      db = liver_cancer
      db$region = db$country
    }
    
    if (input$outcome_select=="Deaths per 10K") { 
      db$new_outcome = db$liver_cancer_deaths_per_100000_men
    }
    
    if (input$outcome_select=="Cases per 10K") { 
      db$new_outcome = db$liver_cancer_new_cases_per_100000_men
    }
    
    if (input$outcome_select=="New Cases") { 
      db$new_outcome = db$liver_cancer_number_of_new_male_cases
    }
    
    if (input$outcome_select=="Deaths") { 
      db$new_outcome = db$cliver_cancer_number_of_male_deaths 
    }  
    
    if (input$outcome_select=="Death Rate") { 
      db$new_outcome = db$death_rate 
    }  
    
    db %>% filter(region %in% input$region_select)
  })
  
  # breast_cancer_country_cases_plot
  output$breast_cancer_country_cases_plot <- renderPlotly({
    breast_cancer_country_cases_plot(country_reactive_db_breast_cancer())
  })
  
  # cervical_cancer_country_cases_plot
  output$cervical_cancer_country_cases_plot <- renderPlotly({
    cervical_cancer_country_cases_plot(country_reactive_db_cervical_cancer())
  })
  
  # colonandrectum_cancer_country_cases_plot
  output$colonandrectum_cancer_country_cases_plot <- renderPlotly({
    colonandrectum_cancer_country_cases_plot(country_reactive_db_colonandrectum_cancer())
  })
  
  # liver_cancer_country_cases_plot
  output$liver_cancer_country_cases_plot <- renderPlotly({
    liver_cancer_country_cases_plot(country_reactive_db_liver_cancer())
  })
  
}
 
                          
# Run the application 
shinyApp(ui = ui, server = server)
  
#library(rsconnect)
#deployApp(account="vac-lshtm")