
## load the relevant packages
library(googlesheets4) # To import data directly from google
library(tidyr) #to tidy the data
library(dplyr) #to wrangle the data
library(ggplot2) #for plotting
library(patchwork) #to combine panels

## authorize for reading non-public sheet
gs4_auth(email = stilir@uw.edu)

## specify URL
gs_url <- "https://docs.google.com/spreadsheets/d/1Isfay93yZj1I3GAgr17EOfZMNvgqAqygzpIX3TSf1k4/edit#gid=1645739698"

## read from URL
WALPA.messy <-read_sheet(gs_url)

## Start tidying the data
#drop average column
WALPA.messy.1_8<-WALPA.messy[,1:8] 

#rename with year only for plotting
colnames(WALPA.messy.1_8) <- c("CATEGORY", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

#pivot to longform data structure
WALPA.tidy<-WALPA.messy.1_8 %>% pivot_longer(!CATEGORY, names_to = "year", values_to = "amount")

#to ID which columns I may want to plot.
options <-WALPA.tidy %>% distinct(CATEGORY)

# Select just a few things to plot for now
overview.plot<- c("TOTAL INCOME", "TOTAL EXPENSES","START BALANCE", "END BALANCE")

WALPA.overview <- WALPA.tidy %>% filter(CATEGORY %in% overview.plot)

p1<-ggplot(data = WALPA.overview , aes(x = year, y = abs(amount), group = CATEGORY, color = CATEGORY)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  ylab("amount ($)")

expenses.plot<- c("CONFERENCE-OUT TOTAL", "TOTAL ADMINISTRATIVE","TOTAL NEWSLETTER", "Scholarship")

WALPA.expenses <- WALPA.tidy %>% filter(CATEGORY %in% expenses.plot)

p2<-ggplot(data = WALPA.expenses , aes(x = year, y = abs(amount), group = CATEGORY, color = CATEGORY)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  ylab("amount ($)")

WALPA.summary <- p1/p2 + plot_annotation(tag_levels = 'A')

ggsave("WALPAsummary.png", dpi = 300)

  


