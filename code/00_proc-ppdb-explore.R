# created 3/9/24
# purpose: look at ppdb values

library(tidyverse)
library(readxl)
library(lubridate)
library(tidytext)
library(naniar)

Sys.setenv(LANG = "en")
rm(list = ls())


# 0. raw data ----------------------------------------------------------------

rdat <- 
  read_excel("data/Requested-PPDB-format.xlsx") %>% 
  janitor::clean_names()

rdat.names <- names(rdat)

draw <- 
  read_excel("data/PPDB_Aarhus_University_24-07-15.xlsx") %>% 
  janitor::clean_names()

draw.names <- names(draw)

intersect(rdat.names, draw.names)


# 1. clean it up ----------------------------------------------------------

d1 <- 
  draw %>%
  mutate_all(as.character) %>% 
  pivot_longer(6:ncol(.)) %>% 
  filter(name %in% rdat.names) %>%  
  mutate(value2 = as.numeric(value)) %>% 
  #--these need to be dealt with
  mutate_if(is.character, str_to_lower)


# 2. look at it -----------------------------------------------------------

# start with just herbicides
d1 %>% 
  filter(grepl("herbicide", pesticide_type)) %>% 
  filter(!is.na(value2)) %>% 
  mutate(substance = reorder_within(substance, value2, name)) %>%
  ggplot(aes(substance, value2)) +
  geom_point() +
  geom_miss_point() +
  facet_wrap(~name, scales = "free")

#this is ugly
d1 %>% 
  filter(grepl("herbicide", pesticide_type)) %>% 
  filter(!is.na(value2)) %>% 
  mutate(substance = reorder_within(substance, value2, name)) %>%
  ggplot(aes(substance, value2)) +
  geom_point() +
  geom_miss_point() +
  facet_wrap(~name, scales = "free") +
  coord_flip() +
  theme(axis.text.y = element_blank())

  
ggplot(aes(name, n, fill = decade)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~decade, scales = "free_y") +
  