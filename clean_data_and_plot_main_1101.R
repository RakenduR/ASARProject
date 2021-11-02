library(tidyverse)
library(dlookr)
library(dplyr)
library(hablar)
library(ggplot2)

library(scales)

library(caret)

library(ggthemes)
library(scales)
library(treemapify)
library(treemap)

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


########JAMIE CHARTS###############################################
library('scales')
library(treemapify)
library('treemap')

#treemap of main category
ks_cleaned %>%
  group_by(main_category) %>%
  summarise(count = n()) %>%
  mutate(ct=count) %>%
  ungroup %>%
  #x = NULL %>%
  ggplot((aes (area = ct, fill = `main_category`, label = paste(main_category)))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 10)


#distribution of state for main cat - final
ks_cleaned %>%
  ggplot(aes (x = main_category, fill = restate))+
  geom_bar()+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


#boxplot for success/fail project pledged goal
ks_cleaned %>%
  ggplot(aes(y = usd_pledged_real, x = main_category)) +
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_y_continuous(labels = comma)


ks_cleaned %>%
  ggplot(aes(y = usd_pledged_real, x = main_category)) +
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_y_continuous(labels = comma)+
  coord_cartesian(ylim = c(0,10000))



#category success rate - 1 bar final 
state.pct <- ks_cleaned %>%
  filter(restate %in% c("successful", "failed")) %>%
  group_by(main_category, restate) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(desc(restate), pct)

state.pct$main_category <- factor(state.pct$main_category, 
                                  levels=state.pct$main_category[1:(nrow(state.pct)/2)])

ggplot(state.pct, aes(main_category, pct, fill=restate)) + geom_bar(stat="identity") + 
  ggtitle("Success vs. Failure Rate by Project Category") + 
  xlab("Project Category") + ylab("Percentage") + scale_y_continuous(labels=scales::percent) + 
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failure")) + 
  geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5), 
            colour="white", size=5)+ 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold")) + coord_flip()

#subcategory success rate - to incorporate in shiny
ks_cleaned %>%
  ggplot(aes(y = reorder(category,category,function(x) length(x)))) +
  geom_bar(aes(fill = restate), position = position_dodge())+
  facet_wrap(~ main_category, scales = "free")+
  labs(y="Sub Category", x = "Count")+
  guides(fill=guide_legend(title="Status"))


  
#project state by month
ks_cleaned %>%
  mutate(month = as.integer(format(launched, "%m"))) %>%
  group_by(month) %>%
  summarise(sucessfull = sum(restate == "successful"),
            failed = sum(restate == "failed"),
            success_rate = round(sum(state == "successful") / n(), digits = 2),
            failed_rate = round(sum(state == "failed") / n(), digits = 2)) %>%
  arrange(month)

plot1 <- ks_cleaned %>%
  mutate(month = as.integer(format(launched, "%m"))) %>%
  group_by(month, restate) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count), label = scales::percent(percentage)) %>%
  arrange(month)
ggplot(plot1, aes(x = month, y = percentage)) +
  geom_line(aes(color = restate), lwd = 1) + 
  scale_color_manual("Project Status",values = c("successful" = "green", "failed" = "red"), labels=c("Successful","Failed")) + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = paste(seq(0, 100, 20), "%", sep = "")) +
  labs(y = "Percent", fill = "State", x = "Month", title = "Project State by Month") + 
  scale_x_discrete(limits=month.abb) 

#boxplot for goal
ks_cleaned %>%
  ggplot(aes(y = usd_goal_real, x = main_category)) +
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_y_continuous(labels = comma)


ks_cleaned %>%
  ggplot(aes(y = usd_goal_real, x = main_category)) +
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_y_continuous(labels = comma)+
  coord_cartesian(ylim = c(0,50000))

#treemap of main category
ks_cleaned %>%
  group_by(main_category) %>%
  summarise(count = n()) %>%
  mutate(ct=count) %>%
  ungroup %>%
  #x = NULL %>%
  ggplot((aes (area = ct, fill = `main_category`, label = paste(main_category)))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 10)
#######################END-JAMIE CHARTS####################################
 
####################### RAKENDU'S CHARTS ##################################
#boxplot of pledged amount of successful projects
ks_cleaned %>%
  filter(state == "successful")%>%
  ggplot(aes(y = usd_pledged_real, x = main_category)) +
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_y_continuous(labels = comma)+
  coord_cartesian(ylim = c(0,80000))+ 
  ggtitle("Pledged Amount of Successful Projects")+
  labs(y="Pledged Amount", x = "Category")

#boxplot of pledged amount of successful projects with Technology boxplot highlighted in green
ColorTech <-ks_cleaned %>%
  filter(state == "successful")

ggplot(data = ColorTech)+
  geom_boxplot(aes(y = usd_pledged_real, x = main_category)) +
  geom_boxplot(data = ColorTech[ColorTech$main_category=="Technology",],
               aes(y = usd_pledged_real, x = main_category),fill="green3")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_y_continuous(labels = comma)+
  coord_cartesian(ylim = c(0,80000))+ 
  ggtitle("Pledged Amount of Successful Projects")+
  labs(y="Pledged Amount", x = "Category")

# bar chart of pledged amount of successful projects by main category
ks_cleaned %>%
  filter(state == "successful")%>%
  group_by(main_category) %>%
  summarise(Amount = sum(usd_pledged_real)) %>%
  mutate(Amount=Amount)%>%
  mutate( ToHighlight = ifelse( main_category == "Technology", "yes", "no" ) )%>%
  mutate(pct=Amount/sum(Amount)) %>%
  ggplot(aes (x = reorder(main_category,-Amount), y =Amount, fill = ToHighlight))+
  geom_bar(stat = "identity") +
  scale_fill_manual( values = c( "yes"="green4", "no"="gray42" ), guide = "none" )+
  scale_y_continuous(labels = comma)+
  ggtitle("Pledged Amount of Successful Projects")+
  labs(y="Total Pledged Amount (USD)", x = "Category")+
  geom_col(position = 'dodge') + 
  geom_text(aes(label = scales::percent(Amount/sum(Amount),accuracy = .1),
                y=Amount),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3)

# bardata <- ks_cleaned %>%s
#   filter(state == "successful")%>%
#   filter(main_category == "Technology")
# write_csv(bardata,"dataset/Techology.csv")

####################### END OF RAKENDU'S CHARTS ##################################