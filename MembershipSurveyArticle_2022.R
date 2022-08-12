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


## The below row may be needed to authorize for reading non-public sheet
# gs4_auth(email = rebekahstiling@gmail.com) #Use your email address (assuming you have permission to access the WALPA google drive)


## specify URL - I converted the excel file to a googlesheet.
gs_url <- "https://docs.google.com/spreadsheets/d/1lAIe_L0U2ZqsYM1pPdAvXbbP4ravFZLOrr-FZmx88lo/edit#gid=2062504534"

## read from URL
survey.messy <-read_sheet(gs_url)

#Glimpse at the data
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
ggsave("WALPA_survey_age.png", p1, width = 4, height = 3.25, units = "in")
ggsave("WALPA_survey_age.jpeg", p1, width = 4, height = 3.25, units = "in")

#### Belonging and Conf attendance plot ####

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

p2<-ggplot(bel.att.data, aes(x = reorder(belong, -n), y = n, fill=conf, label = n)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), color = "white") +  
  theme_classic() +
  scale_fill_manual(values = c("#009063", "#2d2929", "#008fc9", "#f6d894")) +
  labs(x= "Do you feel like you are\npart of the WALPA community?", 
       y = "number of respondents",
       fill='How often do you attend\nWALPA annual conferences?') 
p2
ggsave("WALPA_survey_belongConf.png", p2, width = 7, height = 3.5, units = "in")
ggsave("WALPA_survey_belongConf.jpeg", p2, width = 7, height = 3.5, units = "in")

#### Belonging and Membership plot ####

#isolate belonging and membership data
belong.mem <- survey.messy %>% 
  select('7) Do you feel like you are part of the WALPA community?','10) How long have you been a member of WALPA? Recall that a member includes anyone that received this survey.' ) %>% #select belonging and conf data
  rename (belong = '7) Do you feel like you are part of the WALPA community?', 
          mem = '10) How long have you been a member of WALPA? Recall that a member includes anyone that received this survey.' ) #rename the column to something reasonable

bel.mem.data <- belong.mem %>% 
  group_by(belong, mem) %>% 
  summarise(n = n()) %>% 
  drop_na()
#convert to factors in order to sequence attence by frequence 
bel.att.data$conf <- factor(bel.att.data$conf, 
                            levels=c( 'Every year or Almost every year', 
                                      'Consistently in the past, but not recently', 
                                      'Typically once in a five-year span', 
                                      'Never or only once'))

#now I am going to combine responses to better reflect Angela's table.
ggplot(bel.mem.data, aes(x = belong, y = n, fill = mem))+
  geom_bar(stat="identity")

#convert to factors in order to sequence membership by length of time
bel.mem.data$mem <- factor(bel.mem.data$mem , 
                            levels=c( '1-5 years', 
                                      '5-10 years', 
                                      '10-15 years', 
                                      'More than 15 years'))
#factor yes, maybe, no in order to determine order of x axis
bel.mem.data$belong <- factor(bel.mem.data$belong , 
                           levels=c( 'Yes', 
                                     'Maybe', 
                                     'No'))

p3<-ggplot(bel.mem.data, aes(x = belong, y = n, fill=mem, label = n)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), color = "white") +  
  theme_classic() +
  scale_fill_manual(values = c("#009063", "#2d2929", "#008fc9", "#f6d894")) +
  labs(x= "Do you feel like you are\npart of the WALPA community?", 
       y = "number of respondents",
       fill='How long have you been\na member of WALPA?') 
p3
ggsave("WALPA_survey_belongMember.png", p3, width = 7, height = 3.5, units = "in")
ggsave("WALPA_survey_belongMember.jpeg", p3, width = 7, height = 3.5, units = "in")

#### Focus for the Future ####

#isolate future focus data
outreach <- survey.messy %>% 
  select(c(10)) %>%  #I used column numbers
  rename ("Outreach" = 1)

#interesting, so each value is embedded in a list. I'm not sure what to do about that.
o.df <- as.data.frame(unlist(outreach)) #unlist is the solution

#isolate future focus data
conference <- survey.messy %>% 
  select(c(12)) %>%  #I used column numbers
  rename ("conf" = 1)

#interesting, so each value is embedded in a list. I'm not sure what to do about that.
con.df <- as.data.frame(unlist(conference)) #unlist is the solution

#put the two dfs back together (I'm sure there was a better way to do this!)
comp.data <-cbind(o.df, con.df)

Q3.data <-as_tibble(comp.data) %>% 
  rename("Public outreach/education" = "unlist(outreach)",
         "Host conference" = "unlist(conference)")

Q3.tidy <-Q3.data %>% pivot_longer(cols=c(1,2), names_to = "repondant", values_to = "score")

# Q3.tidy<-Q3.tidy %>% mutate(score = case_when( 
#   score == "1 (most important)"  ~ "1" , 
#   score == "7 (least important)"  ~ "7", 
#   TRUE ~ score))

Q3.sum<-Q3.tidy %>% 
  group_by(repondant, score) %>% 
  summarise(n = n()) %>% 
  drop_na()

p4<-ggplot(Q3.sum, aes(x = score, y = n, fill = score)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~fct_rev(repondant), ) +
  theme_classic() +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5", "6", "7")) + 
  scale_fill_manual(values = c("#009063",
                               "#00b88c", 
                               "#2d2929", 
                               "#008fc9",
                               "#a0d0ee",
                               "#f6d894",  
                               "#fae637")) +
  labs(y = "number of respondents",
       fill='Rank of importance') +
  theme(panel.spacing = unit(1, "lines"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        legend.position = "top") 
  
p4

ggsave("WALPA_survey_future.png", p4, width = 7, height = 3.5, units = "in")
ggsave("WALPA_survey_future.jpeg", p4, width = 7, height = 3.5, units = "in")


#water: #008fc9
#shore: #009063
#littoral/shore: #00b88c
#sky: #a0d0ee
#mountain: #2d2929
#cattail: #f6d894
#Sun: #fae637
