# The purpose of this script is to generate some nice tables and graphs for the 2023 long COVID burden estimate

# Load packages
library(tidyverse) # Good for data manipulation
library(ggthemes) # Good for making graphs cute
library(lubridate) # Makes dates easier
library(readxl) # Import excel files
library(table1) # Does table 1 without having to mess around a lot

# load data, do dates--------------
# Import denominator data
denom <- read_excel("data_phi_ignore/cha_denom_2023.10.04.xlsx") %>%
  # Redo the dates so R understands them as dates
  mutate(month = ymd(paste0(
    "20",
    dischargeyear,
    "/",
    dischargemonth,
    "/",
    "1"
  ))) %>%
  # Remove some unnecessary columns
  select(-dischargeyear, -dischargemonth)

#Import count data
hosp <-
  read_excel("data_phi_ignore/cha_longcovid_2023.10.04.xlsx") %>%
  # Redo dates (as above)
  mutate(month = ymd(paste0(
    "20",
    dischargeyear,
    "/",
    dischargemonth,
    "/",
    "1"
  ))) %>%
  # Remove some columns I don't care about
  select(-admityear, -admitmonth,-dischargeyear, -dischargemonth)

# Recode vars -------------

denom <- denom %>%
  # Transmute is like mutate and select all together!
  transmute(
    # Dates are good
    month = month,
    # Treat encounter type as a factor for easier graphin'
    type = factor(type),
    # Recode county of residence as plain text
    residence = case_when(
      residence == 1 ~ "Denver Metro Area",
      residence == 2 ~ "Outside of Denver"
    ),
    # Rename "num" to "tot_enc" because it's the total number of encounters
    tot_enc = num
  )


hosp <- hosp %>%
  # Transmute again!
  transmute(
    # Month was recoded above
    month = month,
    # As above
    type = factor(type),
    # Renaming it because knowing what LOS means makes you cool
    LOS = lengthofstay,
    # Make sex a factor for good graphin'
    sex = factor(sex),
    # Rename age, because years is the default, man
    age = ageinyears,
    # REcode race/ethnicity as either hispanic or nonhispanic race
    re = case_when(ethnicity == 1 ~ "Hispanic",
                   ethnicity == 2 ~ race),
    # Recode residence, as above
    residence = case_when(
      residence == 1 ~ "Denver Metro Area",
      residence == 2 ~ "Outside of Denver"
    ),
    # Recode age into age groups
    age_group = case_when(
      ageinyears <= 19 ~ "Pediatric (0-19 Years)",
      ageinyears <= 39 ~ "Adult (20-39 Years)",
      ageinyears <= 59 ~ "Adult (40-59 Years)",
      TRUE ~ "Older adults (60+)"
    ),
    # Convert principal diagnosis into either long covid, acute covid, or something else.
    principal_diagnosis = case_when(
      PrincipleDiagnosis == "U071" ~ "Acute COVID (U07.1)",
      PrincipleDiagnosis == "U099" ~ "Long COVID (U09.9)",
      TRUE ~ "Other Principal Encounter Diagnosis"
    )
  ) %>%
  mutate(age_group = factor(
    age_group,
    levels =
      c(
        "Pediatric (0-19 Years)",
        "Adult (20-39 Years)",
        "Adult (40-59 Years)",
        "Older adults (60+)"
      )
  ))



# Make tables -------
# The table1 package just makes table 1 of whatever you want, with good default stats!
# Everything before the | is rows, everything after is columns.
table1( ~ principal_diagnosis + age + age_group + re + residence + LOS
        | type,
        data = hosp) %>%
  # This just exports it as a CSV
  
  write.table(
    .,
    "table1.csv",
    col.names = T,
    row.names = F,
    append = T,
    sep = ','
  )


# Make graphs -------------------------------------------------------------


# Encounters by encounter type over month

hosp %>%
  # Group By + Summarize just aggregates line lists into counts
  group_by(month, type) %>%
  summarize(encounters = n()) %>%
  # Initialize the graph
  ggplot(aes(x = month, y = encounters, color = type)) +
  # Make it a line graph
  geom_line(linewidth = 2) +
  # Make it pretty, like Nate Silver would
  theme_fivethirtyeight() +
  # Remove the legend title
  theme(legend.title = element_blank()) +
  # Add in a title and subtitle
  labs(title = "Long COVID Encounters",
       subtitle = "By Encounter Type")

# Save it!
ggsave(
  "graphs/Long COVID Encounters by Encounter Type.png",
  width = 3.5,
  height = 3
)

# Encounters as a rate

hosp %>%
  # Do the same group by as above
  group_by(month, type) %>%
  summarize(encounters = n()) %>%
  # Add in denominator data! This is a nested thing, which is messy to read, sure.
  # But basically I'm just aggregating the denominator data by month and type too
  # And then you join it to the count data!
  left_join(denom %>%
              group_by(month, type) %>%
              summarize(tot_enc = sum(tot_enc))) %>%
  # Calculate rate per 1000 hospitalizations
  mutate(rate_per_1000_encounters = encounters / (tot_enc * 1000)) %>%
  # Plot the graph, same as above
  ggplot(aes(x = month, y = rate_per_1000_encounters, color = type)) +
  geom_line(linewidth = 2) +
  theme_fivethirtyeight() +
  theme(legend.title = element_blank()) +
  labs(title = "Long COVID Encounters per 1000 Hospital Encounters",
       subtitle = "By Encounter Type")


ggsave(
  "graphs/Long COVID Encounter Rate by Encounter Type.png",
  width = 3.5,
  height = 3
)

# Count by sex
hosp %>%
  # Just an extra aggregating variable here, sex. Then everything is the same until...
  group_by(month, type, sex) %>%
  summarize(encounters = n()) %>%
  ggplot(aes(x = month, y = encounters, color = sex)) +
  geom_line(linewidth = 2) +
  theme_fivethirtyeight() +
  theme(legend.title = element_blank()) +
  labs(title = "Long COVID Encounters",
       subtitle = "By Encounter Type and Sex") +
  # ... you get here, which gives you two stacked graphs.
  facet_wrap( ~ type, 2)

ggsave(filename = "graphs/Long COVID Encounter Rate by Encounter Type and Sex.png",
       width = 7,
       height = 4)

# Count by age group
hosp %>%
  # Same as the sex graph, but this time by age group.
  group_by(month, type, age_group) %>%
  summarize(encounters = n()) %>%
  ggplot(aes(x = month, y = encounters, color = age_group)) +
  geom_line(linewidth = 2) +
  theme_fivethirtyeight() +
  theme(legend.title = element_blank()) +
  labs(title = "Long COVID Encounters",
       subtitle = "By Encounter Type and Age Group") +
  facet_wrap( ~ type, 2)

ggsave(
  "graphs/Long COVID Encounter Rate by Encounter Type and Age Group.png",
  width = 7,
  height = 4
)
