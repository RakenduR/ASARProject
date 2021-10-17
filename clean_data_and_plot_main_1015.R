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
#fix data type issues using retype function from hablar package

ks_1_fix <- retype(ks_1)

diagnose(ks_1_fix)

# see country unique values
ks_1_fix$country %>% unique()

# problematic country rows
ks_1_fix %>% 
  filter(country == "N,0\"") %>%
  view()
#comment for test github
## launched year issue
ks_1_fix %>%
  filter(launched < 2013-01-01) %>%
  view()


###################
## Data Cleaning ##
###################

# Extract year from date
# check out date format strings for R https://www.r-bloggers.com/2013/08/date-formats-in-r/
format(ks_1_fix$launched, format = "%Y")
format(ks_1_fix$launched, format = "%m")
format(ks_1_fix$launched, format= "%H") #hour

###################
# Clean dataframe #
###################
ks_cleaned <- ks_1_fix %>% 
  filter(country != "N,0\"") %>% # Remove those with weird country code
  filter(launched >= 2013-01-01) %>%# Remove those launched dates with just 1970
  mutate(launched_year = as.numeric( format(launched, format="%Y"))) %>%
  mutate(launched_month = as.numeric(format(launched, format= "%m"))) %>%
  mutate(launched_day_of_week = as.numeric(format(launched, format = "%w"))) %>%
  mutate(dollar_per_backer = usd_pledged_real/backers) %>%
  mutate(launch_period = deadline - as.Date(launched)) %>%
  mutate(difference_from_goal = usd_pledged_real - usd_goal_real) %>%
  mutate(project_name_length = nchar(name)) %>%
  mutate(dollar_per_backer = replace_na(dollar_per_backer,0))
  

## Failed with 0 backers
# Rows with 0 backers have 0 pledge
ks_cleaned %>%
  filter(is.na(ks_cleaned$dollar_per_backer )) %>% 
  #filter(usd_pledged_real != 0) %>% 
  view()

### Plotting and Viewing
#############################
# START Overall success rate#
#############################
ks_cleaned %>%
  group_by(state) %>%
  summarise(count = n())%>%
  mutate(per=count/sum(count)) %>%
  ungroup %>%
  ggplot(aes(x="",y=per,  fill=state)) + geom_col() +
  coord_polar("y", start = 0) + ggtitle("Summary of Success") + ylab("")

ks_cleaned %>%
  group_by(state) %>%
  summarise(count = n())%>%
  mutate(per=count/sum(count)) %>%
  ungroup %>%
  ggplot(aes(x="",y=per*100,  fill=state, label=round(per*100,1))) + geom_col() +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
  ylab("Percent") + xlab("") 
#############
#### END ####
#############

# Success when goal not met (fyi)
ks_cleaned %>%
  filter(difference_from_goal <= 0) %>% 
  group_by(state) %>%
  ggplot() + geom_bar(aes(x=state))
