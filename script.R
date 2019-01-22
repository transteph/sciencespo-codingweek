# this is a test
install.packages('car')
install.packages("tidyverse")
install.packages("readr")
# only needed the first time you use it
install.packages("magrittr") 
# alternative installation of the %>%
install.packages("dplyr")   
install.packages('fuzzyjoin')
install.packages('ggplot2')
install.packages('Cairo', dependencies = TRUE)
install.packages('gdtools', dependencies = TRUE)
install.packages('gcookbook', dependencies = TRUE)
install.packages('caret')
update.packages()
install.packages('hrbrthemes', dependencies = TRUE)
hrbrthemes::import_roboto_condensed() 
#     SETTING UP 
# 
#
library(car)
library(caret)
# need to run every time you start R and want to use %>%
library(magrittr)
# alternative, this also loads %>%
library(dplyr)    
library(tidyverse)
library(readr)
library(ggplot2)
library(fuzzyjoin)
library(hrbrthemes)
suppressPackageStartupMessages(library(tidyverse))
rme <- read_csv("/Users/stephanietran/Documents/school/sciences-po/semester-2/z_scpocodinweek/scpocodingweek/Repertoire-national-des-elus.csv")

rme %>% 
  group_by(`Libellé de la profession`) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# remove rows where year of birth is 1900
#
#
rme %>% 
  gather('mandat', 'value', 'Conseiller Municipal' :Maire) %>% # ':' means range to 
  filter(value %in% TRUE ) %>% 
  select(-value) %>% 
  group_by(mandat) %>% 
  filter(!('Date de naissance' %in% lubridate::ymd('1900-01-01'))) %>%  # removing rows where birthday is in 1900
  summarise(n=n())

# Finding average age in each office 
#
#
rme %>% 
  gather('mandat', 'value', 'Conseiller Municipal' :Maire) %>% # ':' means range to 
  filter(value %in% TRUE ) %>% 
  select(-value) %>%  
  group_by(mandat) %>% 
  summarise(age = mean(Age, na.rm = TRUE)) # remove missing values

# Finding missing values per type of office
#
#
rme %>% 
  gather('mandat', 'value', 'Conseiller Municipal' :Maire) %>% # ':' means range to 
  filter(value %in% TRUE ) %>% 
  select(-value) %>%  
  group_by(mandat) %>% 
  filter(is.na(Age)) %>% 
  summarize(n=n())  # show number of missing values

# find avg number of offices held by occupation and gender
#
#
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


# find second youngest woman in each occupation using slice()
#
#
rme %>% 
  filter(`Code sexe` %in% "F" ) %>% 
  group_by(`Libellé de la profession`) %>% 
  arrange(Age) %>% 
  # keeping only the 2nd row for each group 
  slice(2) %>% 
  View
  
# find second youngest woman in each occupation using filter
#
#
rme %>% 
  filter(`Code sexe` %in% "F" ) %>% 
  group_by(`Libellé de la profession`) %>% 
  arrange(desc(Age)) %>%
  # create new var named rank that shows ranking of person's age
  mutate(rank = 1:n()) %>% 
  filter(rank %in% 2) %>% 
  View


# case_when()
#
#
rme %>% 
  mutate(number= case_when(`Nombre de mandats` %in% 1 ~ "one",
                           `Nombre de mandats` %in% 2 ~ "two",
                           `Nombre de mandats` %in% 3 ~ "three",
                           `Nombre de mandats` %in% 4 ~ "four",
                           # if none of the conditions are true.
                           # NA needs to be character to be same data type as var
                           TRUE ~ NA_character_
                           )) %>% 
  View

# using aesthetic function to do the mapping
#
#
rme %>% 
  # '+' is an older way of using pipe in ggplot 
  ggplot(aes(x = `Code sexe`)) + 
  #adding layer, a bar chart
  geom_bar() +
  scale_y_continuous(labels = scales::comma) +
  # flipping coordinates
  coord_flip() 


# mapping in order by freq
#
#
# Making pdf file
# cairo_pdf(file = './plot1', width = 12, height = 7)

rme %>% 
  mutate(gender = recode(`Code sexe`, "M" = "Male", "F" = "Female")) %>% 
  count(`Libellé de la profession`,gender, sort = TRUE) %>% 
  filter(!is.na(`Libellé de la profession`)) %>% 
  ungroup %>% 
  arrange(gender, n) %>% 
  filter(n > 1000) %>% 
  mutate(order = row_number()) %>% 
  mutate(occupation = fct_inorder(`Libellé de la profession`)) %>% 
  # creating new var coord so that if n past a certain limit, the label will be 
  # inside the bar and white. otherwise will be black
  mutate(coord = if_else(n > 22000, n - 1000, n + 1000),
         colour = if_else(n > 22000, "white", "black")) %>% 
  ggplot(aes(x = order, y = n)) +
  geom_bar(aes(fill = gender), stat = "identity", width = 0.8) +
  scale_fill_discrete( guide = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
    # geom_text() = creating new geometry of type 'text' onto plot
  #     don't have to indicate x position as it was indicated in initial ggplot
  #     have to change y position because we want labels 
  #     y indicates the CENTER of the label 
  #    hjust = horizontal justification 
  geom_text(aes(label = occupation, y = coord, colour = colour), hjust = "inward", 
            vjust = "center", size = 2) +
  # set colours of text. remove legend
  scale_color_manual(values = c("black", "white"), guide = FALSE) +
  # facet wrap
  facet_wrap(facets = vars(gender), scales = "free_y") +
  # using xlab because coord_flip flipped x and y
  # removing x axis label
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = NULL) +
  theme(axis.ticks.y = element_blank()) +
  theme_ipsum(grid = "X") +
  labs(title = "Most elected officials are employees, farmers or retired.", subtitle = "Number of elected officials in France in 2018 by occupation.", caption = "Source: RNE (Ministère de l'intérieur), computation by Sciences Po students.")
# dev.off()
  


  
  

