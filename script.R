# this is a test

install.packages("tidyverse")
install.packages("readr")
# only needed the first time you use it
install.packages("magrittr") 
# alternative installation of the %>%
install.packages("dplyr")   
# need to run every time you start R and want to use %>%
library(magrittr)
# alternative, this also loads %>%
library(dplyr)    
library(tidyverse)
library(readr)
suppressPackageStartupMessages(library(tidyverse))
rme <- read_csv("/Users/stephanietran/Documents/school/sciences-po/semester-2/z_scpocodinweek/scpocodingweek/Repertoire-national-des-elus.csv")

rme %>% 
  group_by(`Libellé de la profession`) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# remove rows where year of birth is 1900
rme %>% 
  gather('mandat', 'value', 'Conseiller Municipal' :Maire) %>% # ':' means range to 
  filter(value %in% TRUE ) %>% 
  select(-value) %>% 
  group_by(mandat) %>% 
  filter(!('Date de naissance' %in% lubridate::ymd('1900-01-01'))) %>%  # removing rows where birthday is in 1900
  summarise(n=n())

# Finding average age in each office 
rme %>% 
  gather('mandat', 'value', 'Conseiller Municipal' :Maire) %>% # ':' means range to 
  filter(value %in% TRUE ) %>% 
  select(-value) %>%  
  group_by(mandat) %>% 
  summarise(age = mean(Age, na.rm = TRUE)) # remove missing values

# Finding missing values per type of office
rme %>% 
  gather('mandat', 'value', 'Conseiller Municipal' :Maire) %>% # ':' means range to 
  filter(value %in% TRUE ) %>% 
  select(-value) %>%  
  group_by(mandat) %>% 
  filter(is.na(Age)) %>% 
  summarize(n=n())  # show number of missing values

# find avg number of offices held by occupation and gender
rme %>% 
  gather('mandat', 'value', `Conseiller Municipal` :Maire) %>% # ':' means range to 
  filter(value %in% TRUE ) %>% 
  select(-value) %>%   
  # grouping by unique identifier
  group_by(Identifiant) %>% 
  # number of offices
  summarise(offices = n(), occupation = unique(`Libellé de la profession`), gender = unique(`Code sexe`)) %>% 
  # remove the group
   ungroup() %>% 
  # group by occupation and gender
  group_by(occupation, gender) %>% 
  summarise(offices = mean(offices)) %>% 
  arrange(desc(offices))

