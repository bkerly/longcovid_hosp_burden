# The purpose of this script is to generate some nice tables and graphs for the 2023 long COVID burden estimate

# Load packages
library(tidyverse)
library(ggthemes)
library(lubridate)
library(readxl)
library(table1)

# load data, do dates--------------
denom <- read_excel("data_phi_ignore/cha_denom_2023.10.04.xlsx") %>%
  mutate(month = ymd(
    paste0(
      "20",
      dischargeyear,
      "/",
      dischargemonth,
      "/",
      "1"
    )
  )
  ) %>%
  select(-dischargeyear,-dischargemonth)

hosp <- read_excel("data_phi_ignore/cha_longcovid_2023.10.04.xlsx") %>%
  mutate(month = ymd(
    paste0(
      "20",
      dischargeyear,
      "/",
      dischargemonth,
      "/",
      "1"
    )
  )
  ) %>%
  select(-admityear,-admitmonth,
         -dischargeyear,-dischargemonth)

# Recode vars -------------

denom <- denom %>%
  transmute(
    month = month,
    type = factor(type),
    residence = case_when(
      residence == 1 ~ "Denver Metro Area",
      residence == 2 ~ "Outside of Denver"
    ),
    tot_enc = num
  )
  

hosp<- hosp %>%
  transmute(
    month = month,
    type = factor(type),
    LOS = lengthofstay,
    sex = factor(sex),
    age = ageinyears,
    re = case_when(
      ethnicity == 1 ~ "Hispanic",
      ethnicity == 2 ~ race
    ),
    residence = case_when(
      residence == 1 ~ "Denver Metro Area",
      residence == 2 ~ "Outside of Denver"
    ),
    age_group = case_when(
      ageinyears <= 19 ~ "Pediatric (0-19 Years)",
      ageinyears <= 39 ~ "Adult (20-39 Years)",
      ageinyears <= 59 ~ "Adult (40-59 Years)",
      TRUE ~ "Older adults (60+)"
    ),
    principal_diagnosis = case_when(
      PrincipleDiagnosis == "U071" ~ "Acute COVID (U07.1)",
      PrincipleDiagnosis == "U099" ~ "Long COVID (U09.9)",
      TRUE ~ "Other Principal Encounter Diagnosis"
    )
  )



# Make tables -------


  table1(~ principal_diagnosis + age + age_group + re + residence + LOS
           | type, data = hosp)


# Make graphs -------------------------------------------------------------

# Aggregate by many, many variables
hosp_num <- hosp %>%
  group_by(month,type,sex,re,residence,age_group,principal_diagnosis) %>%
  summarize(encounters = n()) 


# Enounters by encounter trype over month

hosp %>%
  group_by(month, type) %>%
  summarize(encounters = n()) %>%
  ggplot(aes(x=month,y=encounters,color = type)) +
  geom_line(linewidth = 2) +
  theme_fivethirtyeight() +
  theme(legend.title = element_blank()) +
  labs(title = "Long COVID Encounters",
       subtitle = "By Encounter Month")

# Encounters as a rate

hosp %>%
  group_by(month, type) %>%
  summarize(encounters = n()) %>%
  left_join(denom) %>%
  mutate(rate_per_1000_encounters = encounters / tot_enc)
