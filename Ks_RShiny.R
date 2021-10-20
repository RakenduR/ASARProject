library(shiny)
library(tidyverse)
library(dlookr)
library(dplyr)
library(hablar)
library(ggplot2)
library(scales)
library(shinydashboard)

# # Load Dataset
# ks_1 <- read.csv("dataset/ks-projects-201801.csv")
# 
# ks_1_fix <- retype(ks_1)
# 
# #Datacleaning, need to replace this with saved dataset
# ks_cleaned <- ks_1_fix %>% 
#     filter(country != "N,0\"") %>% # Remove those with weird country code
#     filter(launched >= 2013-01-01) %>%# Remove those launched dates with just 1970
#     mutate(launched_year = as.numeric( format(launched, format="%Y"))) %>%
#     mutate(launched_month = as.numeric(format(launched, format= "%m"))) %>%
#     mutate(launched_day_of_week = as.numeric(format(launched, format = "%w"))) %>%
#     mutate(dollar_per_backer = usd_pledged_real/backers) %>%
#     mutate(launch_period = deadline - as.Date(launched)) %>%
#     mutate(difference_from_goal = usd_pledged_real - usd_goal_real) %>%
#     mutate(project_name_length = nchar(name)) %>%
#     mutate(dollar_per_backer = replace_na(dollar_per_backer,0))

ks_cleaned <- readRDS(file = "dataset/ks_cleaned.rds")

#prepare the sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Tab1" ,tabName = "Tab1", icon = icon("dashboard")),
        menuItem("Kickstarter Planning",tabName = "Tab2", icon = icon("brain")),
        menuItem("Tab3", tabName = "Tab3", icon = icon("chart-line"))
        
    )
)

#prepare the body
body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = 'Tab1'
        ),
        tabItem(
            tabName = 'Tab2',
            fluidPage(
                titlePanel("Tell Us About Your Kickstarter Idea"),
                sidebarLayout(
                    sidebarPanel(
                        
                        textInput(
                            "tab2_project_name",
                            "Your Future Project Name"
                        ),
                        
                        selectInput(
                            "tab2_main_category",
                            "Main Category",
                            unique(ks_cleaned["main_category"])
                        ),
                        
                        uiOutput("tab2_sub_category_input"),
                        
                        dateInput("tab2_project_date_start", label = "Your Fundraiser Start Date", value = Sys.Date()),
                        
                        dateInput("tab2_project_date_end", label = "Your Fundraiser End Date", value = Sys.Date()+30),
                        
                        numericInput("tab2_goal_usd", label = "Your Fundraiser Goal in USD", value = 0, min = 0, step = 1)
                        
                    ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel("Success Rate Predictor"), 
                            tabPanel("Similar Projects: Stats",
                                     plotOutput("tab2_success_rate_plot")),
                            tabPanel("Similar Projects: List", DT::dataTableOutput("tab2_similar_projects"))
                        )
                    )
            )
            )
        ),
        tabItem(
            tabName = 'Tab3',
            fluidPage(
                titlePanel("Project Status Pie"),
                fluidRow(
                    column(
                        width = 12,
                        tabsetPanel(
                            tabPanel("Status",
                                     fluidRow(
                                         tableOutput(outputId = "status"),
                                         box(
                                             plotOutput(outputId = "pie_plot1",
                                                        click = "plot_click"),
                                             verbatimTextOutput('mouse')
                                         ),
                                         box(
                                             plotOutput(outputId = "bp_plot1",
                                                        click = "plot1_click"),
                                             verbatimTextOutput('mouse1')
                                         )
                                     )
                            )
                        )
                    )
                )
            )
        )
        
    )
)

# Put sidebar and body together into a dashboardPage
ui <- dashboardPage(
    dashboardHeader(title = "KickStarter"),
    sidebar,
    body
)

# create the server functions for the dashboard  
server <- function(input, output) {
    
    # Tab 3 - display pie chart
    output$pie_plot1 <- renderPlot({
        
        #prepare the bar graph
        bp <- ks_cleaned %>%
            group_by(state) %>%
            summarise(count = n())%>%
            mutate(per=count/sum(count)) %>%
            ungroup %>%
            ggplot(aes(x="",y=per*100,  fill=state, label=round(per*100,1))) + geom_col() +
            geom_text(size = 3, position = position_stack(vjust = 0.5)) +
            ylab("Percent") + xlab("")
        
        #change the bar graph into pie
        pie_plot1 <- bp + coord_polar("y", start=0)
        
        #build blank theme
        blank_theme <- theme_minimal()+
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid=element_blank(),
                axis.ticks = element_blank(),
                plot.title=element_text(size=14, face="bold")
            )
        
        #apply blank them to pie and apply blues theme
        pie_plot1 <- pie_plot1 + scale_fill_brewer("Blues") + blank_theme +
            theme(axis.text.x=element_blank())
        
        
        return(pie_plot1)
    })
    
    #display the return on pie click below the pie
    output$mouse <- renderPrint({
        text <- str(input$plot_click)
        return(text)
    })
    
    #Tab 3 - display bar plot
    output$bp_plot1 <- renderPlot({
        
        #prepare the bar
        bp <- ks_cleaned %>%
            group_by(state) %>%
            summarise(count = n())%>%
            mutate(per=count/sum(count)) %>%
            ungroup %>%
            ggplot(aes(x="",y=per*100,  fill=state, label=round(per*100,1))) + geom_col() +
            geom_text(size = 3, position = position_stack(vjust = 0.5)) +
            ylab("Percent") + xlab("")
        
        #build blank theme
        blank_theme <- theme_minimal()+
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid=element_blank(),
                axis.ticks = element_blank(),
                plot.title=element_text(size=14, face="bold")
            )
        
        #apply blank theme and blue theme to bar
        bp <- bp + scale_fill_brewer("Blues") + blank_theme
        
        #return bar to ui
        return(bp)
    })
    
    output$mouse1 <- renderPrint({
        text <- str(input$plot1_click)
        return(text)
    })
    
    #Tab2: Dynamic Input for Sub Category
    output$tab2_sub_category_input <- renderUI({
        selectInput(inputId="tab2_sub_category",
                    label="Sub Category",
                    choices=unique(ks_cleaned[ks_cleaned$main_category == input$tab2_main_category, "category"])
        )
    })
    
    #Tab2: List of Similar Projects
    output$tab2_similar_projects <- DT::renderDataTable(ks_cleaned[ks_cleaned$main_category == input$tab2_main_category
                                                                   & ks_cleaned$category == input$tab2_sub_category
                                                                   & if(input$tab2_goal_usd == 0){TRUE} else {abs(ks_cleaned$usd_goal_real - input$tab2_goal_usd) / input$tab2_goal_usd < 0.25}
                                                                   ,c("name", "main_category", "category", "launched", "deadline", "launch_period", "state", "backers", "country", "usd_goal_real", "usd_pledged_real", "dollar_per_backer")
                                                                   ], options = list(scrollX = TRUE)
    )
    
    #Tab2: Success Rate
    output$tab2_success_rate_plot <- renderPlot({
        bp <- 
        ks_cleaned[ks_cleaned$main_category == input$tab2_main_category
           & ks_cleaned$category == input$tab2_sub_category
           & if(input$tab2_goal_usd == 0){TRUE} else {abs(ks_cleaned$usd_goal_real - input$tab2_goal_usd) / input$tab2_goal_usd < 0.25}
           ,] %>% 
            mutate(state_2 = if_else_(state == "successful", "success", "fail")) %>%
            group_by(state_2) %>%
            summarise(count = n())%>%
            mutate(per=count/sum(count)) %>%
            ungroup %>%
            ggplot(aes(x="",y=per*100,  fill=state_2, label=round(per*100,1))) + geom_col() +
            geom_text(size = 3, position = position_stack(vjust = 0.5)) +
            ylab("Percent") + xlab("")
        
        #change the bar graph into pie
        pie_plot1 <- bp + coord_polar("y", start=0)
        
        #build blank theme
        blank_theme <- theme_minimal()+
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid=element_blank(),
                axis.ticks = element_blank(),
                plot.title=element_text(size=14, face="bold")
            )
        
        #apply blank them to pie and apply blues theme
        pie_plot1 <- pie_plot1 + scale_fill_brewer("Blues") + blank_theme +
            theme(axis.text.x=element_blank())
        
        return(pie_plot1)
    })
}

shinyApp(ui, server)
