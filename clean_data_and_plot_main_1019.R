library(tidyverse)
library(dlookr)
library(dplyr)
library(hablar)
library(ggplot2)

# Load Dataset
ks_1 <- read.csv("dataset/ks-projects-201801.csv")

# Preview
head(ks_1)

# Column names
names(ks_1)

# 5 Number summary
summary(ks_1)

# Check for data issues using dlookr

diagnose(ks_1)

## Fix Data Types
#Notice that the USD columns are not in numerical data types.
#fix data type issues using retype function

ks_1_fix <- retype(ks_1)

diagnose(ks_1_fix)

# see country unique values
ks_1_fix$country %>% unique()

# problematic country rows
ks_1_fix %>% 
  filter(country == "N,0\"") %>%
  view()

## launched year issue
ks_1_fix %>%
  filter(launched < 2013-01-01) %>%
  view()


#########################
## START Data Cleaning ##
#########################

# Extract year from date
# check out date format strings for R https://www.r-bloggers.com/2013/08/date-formats-in-r/
format(ks_1_fix$launched, format = "%Y")
format(ks_1_fix$launched, format = "%m")
format(ks_1_fix$launched, format= "%H") #hour

#########################
# START Clean dataframe #
#########################
ks_cleaned <- ks_1_fix %>% 
  filter(state != 'live') %>% # Exclude live projects
  filter(country != "N,0\"") %>% # Remove those with weird country code
  filter(launched >= 2013-01-01) %>% # Remove those launched dates with just 1970
  mutate(restate = str_replace(state,"canceled","failed")) %>% # New state column for success and failed only
  mutate(restate = str_replace(restate,"suspended","failed")) %>%
  mutate(launched_year = as.numeric( format(launched, format="%Y"))) %>%
  mutate(launched_month = as.numeric(format(launched, format= "%m"))) %>%
  mutate(launched_day_of_week = as.numeric(format(launched, format = "%w"))) %>%
  mutate(dollar_per_backer = usd_pledged_real/backers) %>%
  mutate(launch_period = deadline - as.Date(launched)) %>%
  filter(launch_period <= 60) %>%
  mutate(difference_from_goal = usd_pledged_real - usd_goal_real) %>%
  mutate(project_name_length = nchar(name)) %>%
  mutate(dollar_per_backer = replace_na(dollar_per_backer,0)) %>%
  mutate(launch_period_10days = case_when(
    launch_period < 10 ~ "0-9",
    launch_period >= 10 & launch_period<20 ~ "10-19",
    launch_period >= 20 & launch_period<30 ~ "20-29",
    launch_period >= 30 & launch_period<40 ~ "30-39",
    launch_period >= 40 & launch_period<50 ~ "40-49",
    launch_period >= 50 & launch_period<60 ~ "50-59",
    launch_period >= 60 ~ ">=60"
  )) %>%
  mutate(launch_period_10days = factor(launch_period_10days,
                                       levels=c("0-9","10-19","20-29","30-39","40-49","50-59",">=60")))
    
    #launch_period >= 60 & launch_period<70 ~ "60-69",
    #launch_period >= 70 & launch_period<80 ~ "70-79",
    #launch_period >= 80 & launch_period<90 ~ "80-89",
    #launch_period >= 90 ~ ">=90"
  #)) %>%
  #mutate(launch_period_10days = factor(launch_period_10days,levels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89",">=90")))
  

## Failed with 0 backers (fyi)
# Check that all rows with 0 backers have 0 pledge
ks_cleaned %>%
  filter(is.na(ks_cleaned$dollar_per_backer )) %>% 
  #filter(usd_pledged_real != 0) %>% 
  view()


# Success when goal not met (fyi)
ks_cleaned %>%
  filter(difference_from_goal <= 0) %>% 
  group_by(restate) %>%
  ggplot() + geom_bar(aes(x=restate))

# State of projects which have goal amount met (fyi)
## Majority success when goal amount met
ks_cleaned %>%
  filter(difference_from_goal >= 0) %>% 
  group_by(restate) %>%
  ggplot() + geom_bar(aes(x=restate))


#############
#### END ####
#############


############################
# START Graph Plotting EDA #
############################

#### Overall success rate ####
ks_cleaned %>%
  rename(Project_Status = restate) %>%
  group_by(Project_Status) %>%
  summarise(count = n())%>%
  mutate(per=count/sum(count)) %>%
  ungroup %>%
  ggplot(aes(x="",y=per,  fill=Project_Status)) + geom_col() +
  geom_text(aes(label = round(per*100,1)), position = position_stack(vjust = 0.5))+
  coord_polar("y", start = 0) + theme_void() +
  ggtitle("Summary of Success (%)") + ylab("")


#### Launch period vs pledged amount by state of project ####
ks_cleaned %>%
  rename(Project_Status = restate) %>%
  ggplot(aes(x=launch_period, y=usd_pledged_real, shape=Project_Status, color=Project_Status)) + 
  geom_point()+
  ylab('Pledged amount USD')+
  xlab('Launch Period (Days)')+
  facet_wrap(~Project_Status,ncol=2)+
  scale_y_continuous(trans='log10')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


#### Launch period vs length of project name by state of project ####
ks_cleaned %>%
  rename(Project_Status = restate) %>%
  ggplot(aes(x=project_name_length, y=usd_pledged_real, shape=Project_Status, color=Project_Status)) + 
  geom_point()+
  ylab('Pledged amount USD')+
  xlab('Length of project name (characters)')+
  facet_wrap(~Project_Status,ncol=2)+
  scale_y_continuous(trans='log10')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#### No of dollar per backer by main category ####
# dollar per backer <= 1000
ks_cleaned %>%
  filter(dollar_per_backer <= 1000) %>%
  ggplot() + geom_histogram(aes(x=dollar_per_backer, color=main_category), binwidth=100, ) + 
  facet_wrap(~main_category) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# dollar per backer >1000
ks_cleaned %>%
  filter(dollar_per_backer > 1000) %>%
  ggplot() + geom_histogram(aes(x=dollar_per_backer, color=main_category), binwidth=500,) + 
  facet_wrap(~main_category)  + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#### Add new graph here ####

#############
#### END ####
#############

##############################
# START Correlation analysis #
##############################
# Overall table for correlation Analysis
cor1 <-  filter(ks_cleaned, restate != "failed") %>% select(launch_period, usd_pledged_real, project_name_length)

#### Launch period vs pledged amount for successful projects ####
cor.test(x=cor1$usd_pledged_real, y=as.numeric(cor1$launch_period))

#### Project name length vs pledged amount for successful projects ####
cor.test(cor1$project_name_length, cor1$usd_pledged_real)

#############
#### END ####
#############