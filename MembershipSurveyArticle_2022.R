#=== === === === === === === ===
# Started August 11, 2022
# Rebekah Stiling, rebekahstiling@gmail.com
# This script remakes figures from the WALPA membership survey
#=== === === === === === === ===


## load the relevant packages
library(googlesheets4) # To import data directly from google
library(tidyverse) #to tidy and plot the data


## specify URL - I converted the excel file to a googlesheet.
gs_url <- "https://docs.google.com/spreadsheets/d/1lAIe_L0U2ZqsYM1pPdAvXbbP4ravFZLOrr-FZmx88lo/edit#gid=2062504534"

## read from URL
survey.messy <-read_sheet(gs_url)

## The below row may be needed to authorize for reading non-public sheet
# gs4_auth(email = rebekahstiling@gmail.com) #Use your email address (assuming you have permission to access the WALPA google drive)

head(survey.messy)

#### Ages plot ####

#isolate ages data
ages <- survey.messy %>% 
  select('16) Which category below includes your age?') %>% #select only the age data
  rename (age_groups = '16) Which category below includes your age?') #rename the column to something reasonable

age.data <-ages %>% 
  group_by(age_groups) %>% #group results into bins
  summarise(n = n()) %>% #summarize the counts for each bin
  filter(age_groups != "Prefer not to answer") #remove no answer

#create plot
p1<-ggplot(age.data, aes(x= age_groups, y=n)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = c(2,4,6,8,10)) +
  theme_classic() +
  labs(x= "age group", y = "number of respondents")

ggsave("WALPA_survey_age.png", p1, width = 3.25, height = 3.25, units = "in")
ggsave("WALPA_survey_age.jpeg", p1, width = 3.25, height = 3.25, units = "in")

