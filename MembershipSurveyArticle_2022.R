#=== === === === === === === ===
# Started August 11, 2022
# Rebekah Stiling, rebekahstiling@gmail.com
# This script remakes figures from the WALPA membership survey
#=== === === === === === === ===


## load the relevant packages
library(googlesheets4) # To import data directly from google
library(tidyverse) #to tidy and plot the data


## Color picker colors for WALPA  
#water: #008fc9
#shore: #009063
#littoral/shore: #00b88c
#sky: #a0d0ee
#mountain: #2d2929
#cattail: #f6d894
#Sun: #fae637


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
  geom_bar(stat = "identity", fill = "#008fc9") +
  scale_y_continuous(breaks = c(2,4,6,8,10)) +
  theme_classic() +
  labs(x= "age group", y = "number of respondents")
p1
ggsave("WALPA_survey_age.png", p1, width = 3.25, height = 3.25, units = "in")
ggsave("WALPA_survey_age.jpeg", p1, width = 3.25, height = 3.25, units = "in")

#### Belonging and Conf attendance ####

#isolate belonging and attendance data
belong.att <- survey.messy %>% 
  select('7) Do you feel like you are part of the WALPA community?','11) How often do you attend WALPA annual conferences?' ) %>% #select belonging and conf data
  rename (belong = '7) Do you feel like you are part of the WALPA community?', 
          conf = '11) How often do you attend WALPA annual conferences?' ) #rename the column to something reasonable

## now I'm going to update conf categories in order to combine them.
belong.att.grouped <-belong.att %>%
  mutate(conf = case_when( #change the wording during case matche 
    conf == "Almost every year" | conf == "Every year"  ~ "Every year or Almost every year" , #always or almost and combine
    conf == "Iâ€™ve never attended a WALPA annual meeting" | conf == "Only once before"  ~ "Never or only once", #never or once and combine
    conf == "I have attended one conference, but plan to attend annually when I can."  ~ "Consistently in the past, but not recently", #change this 1 to match the consistently in the past group
    TRUE ~ conf)) #don't change the others


bel.att.data <- belong.att.grouped %>% 
  group_by(belong, conf) %>% 
  summarise(n = n()) %>% 
  drop_na()
#convert to factors in order to sequence attence by frequence 
bel.att.data$conf <- factor(bel.att.data$conf, 
                            levels=c( 'Every year or Almost every year', 
                                      'Consistently in the past, but not recently', 
                                     'Typically once in a five-year span', 
                                     'Never or only once'))

#now I am going to combine responses to better reflect Angela's table.

p2<-ggplot(bel.att.data, aes(x = reorder(belong, -n), y = n, fill=conf)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = c("#009063", "#2d2929", "#008fc9", "#f6d894")) +
  labs(x= "Do you feel like you are\npart of the WALPA community?", 
       y = "number of respondents",
       fill='How often do you attend\nWALPA annual conferences?') 
p2
ggsave("WALPA_survey_belongConf.png", p2, width = 5, height = 3.25, units = "in")
ggsave("WALPA_survey_belongConf.jpeg", p2, width = 5, height = 3.25, units = "in")


