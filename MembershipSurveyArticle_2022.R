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

## The below row may be needed to authorize for reading non-public sheet, then the previous lines can be tried again
gs4_auth(email = rebekahstiling@gmail.com) 
##If you are not Beka, use your email address (assuming you have permission to access the WALPA google drive)

head(survey.messy)

ages <- survey.messy %>% 
  select('16) Which category below includes your age?') %>% #select only the age data
  rename (age_groups = '16) Which category below includes your age?') #rename the column something reasonable

age.data <-ages %>% 
  group_by(age_groups) %>% #group results into bins
  summarise(n = n()) %>% #summarize the counts for each bin
  filter(age_groups != "Prefer not to answer")

#create plots
ggplot(age.data, aes(x= age_groups, y=n)) +
  geom_bar(stat = "identity")
