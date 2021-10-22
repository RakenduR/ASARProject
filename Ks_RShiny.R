library(shiny)
library(tidyverse)
library(dlookr)
library(dplyr)
library(hablar)
library(ggplot2)
library(scales)
library(shinydashboard)

library(flexdashboard)
library(caret)

library(ggthemes)

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
tab2_glm <- readRDS(file = "model/tab2_glm_compressed.rds")
test_data <- readRDS(file = "model/test_data.rds")

#prepare the sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Tab1" ,tabName = "Tab1", icon = icon("dashboard")),
        menuItem("Kickstarter Planning",tabName = "Tab2", icon = icon("brain")),
        menuItem("Summary", tabName = "Summary", icon = icon("chart-line"))
        
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
                        
                        dateInput("tab2_project_date_start", label = "Your Fundraiser Start Date", value = Sys.Date(), min = Sys.Date()),
                        
                        uiOutput("tab2_project_date_end_input"),
                        
                        numericInput("tab2_goal_usd", label = "Your Fundraiser Goal in USD", value = 0, min = 0, step = 1),
                        
                        sliderInput("tab2_success_prediction_criteria", label = "How Sure Should the Model be to Predict a Success? (Higher = More Conservative about Predicting Success):", value = 50, min = 0, max = 100, post = "%")
                        
                    ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel("Success Rate Predictor",
                                     fluidRow(
                                         column(6,
                                               wellPanel(
                                                   gaugeOutput("tab2_glm_prediction_num_plot")
                                               ),
                                               wellPanel(
                                                   textOutput("tab2_glm_prediction_text"),
                                                   tags$head(tags$style("#tab2_glm_prediction_text{color: Black;
                                                             font-size: 40px;
                                                             font-style: bold;
                                                             }"
                                                   )
                                                   )
                                               )
                                               ),
                                         column(6,
                                                wellPanel(
                                                    verbatimTextOutput("confusion_matrix_text")
                                                )
                                         ),
                                     )
                                     ), 
                            tabPanel("Similar Projects: Stats",
                                     
                                     textOutput("tab2_similar_projects_text"),
                                     
                                     fluidRow(
                                         
                                         column(6,
                                                wellPanel(
                                                    textOutput("tab2_success_rate_text"),
                                                    
                                                    textOutput("tab2_mean_cnt_success_backers_text"),
                                                    
                                                    
                                                    textOutput("tab2_mean_cnt_failed_backers_text"),
                                                    
                                                    
                                                    textOutput("tab2_mean_success_usd_per_backer_text"),
                                                    
                                                    
                                                    textOutput("tab2_mean_failed_usd_per_backer_text")
                                                    
                                                    )
                                                ),
                                         
                                         column(6,
                                                wellPanel(
                                                    plotOutput("tab2_success_rate_plot")
                                                )
                                         ),
                                         
                                         ),
                                     
                                     fluidRow(
                                         
                                         column(6,
                                                wellPanel(
                                                    plotOutput("tab2_cnt_backers_plot")
                                                )
                                         ),
                                         
                                         column(6,
                                                wellPanel(
                                                    plotOutput("tab2_usd_per_backer_plot")
                                                )
                                         ),
                                         
                                     )
                                     
                                     ),
                                    
                            tabPanel("Similar Projects: List", DT::dataTableOutput("tab2_similar_projects"))
                        )
                    )
            )
            )
        ),
        tabItem(
            tabName = 'Summary',
            fluidPage(
                titlePanel("Summary of Past KickStarter Projects"),
                fluidRow(
                    column(
                        width = 12,
                        tabsetPanel(
                            tabPanel("",
                                     fluidRow(
                                         tableOutput(outputId = "status"),
                                         box(
                                             plotOutput(outputId = "pie_plot1",
                                                        click = "plot_click",
                                                        height = 600),

                                         ),
                                         box(
                                             verbatimTextOutput('result'),
                                             plotOutput(outputId = "box_plot1",
                                                        height = 600)
                                         )
                                     ),
                                     fluidRow(
                                         tableOutput(outputId = "summary"),
                                         box(
                                             plotOutput(outputId = "bar_plot1",
                                                        click = "bplot_click",
                                                        height = 600)
                                         ),
                                         box(
                                             verbatimTextOutput('barresult'),
                                             plotOutput(outputId = "box_plot2",
                                                        height = 600)

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
    
#############
####Tab 3####
#############
    
    bpdata <- ks_cleaned %>%
        rename(Project_Status = restate) %>%
        group_by(Project_Status) %>%
        summarise(count = n())%>%
        mutate(per=count/sum(count)) %>%
        ungroup
    
    bp_decode <- bpdata %>% arrange(desc(Project_Status)) %>%
        mutate(csum=cumsum(per),
               lagcsum=lag(csum,default=0))
    
    # Tab 3 - display pie chart
    output$pie_plot1 <- renderPlot({
        
        #prepare the bar graph
        bp <-  bpdata%>%
            ggplot(aes(x="",y=per,  fill=Project_Status)) + geom_col() +
            geom_text(aes(label = round(per*100,1)), position = position_stack(vjust = 0.5))+
            coord_polar("y", start = 0) + theme_void() +
            ggtitle("Summary of Success (%)") + ylab("")
        
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
        pie_plot1 <- pie_plot1 + scale_fill_brewer(palette= "Blues", name="Legend", labels=c("Failed","Successful")) + blank_theme +
            theme(axis.text.x=element_blank())
        
        
        return(pie_plot1)
    })
    
    click_result <- reactive({
        ipc <- req(input$plot_click)
        y_val <- ipc$y
        bp_decode %>% rowwise %>% filter(
            between(y_val,
                    lagcsum,
                    csum)) %>% pull(Project_Status)
    })
    
    # output$result <- renderText({
    #     if(req(click_result()) == "successful"){
    #         text = "Pledged Amount of Successful Projects"
    #     }else{
    #         text = "Pledged Amount of Failed Projects"
    #     }
    #     return(text)
    # 
    # })
    
    #Boxplot corresponding to pie chart
    output$box_plot1 <- renderPlot({
        
        box <- ks_cleaned %>%
            ggplot(aes(y = usd_pledged_real, x = main_category)) +
            geom_boxplot()+
            scale_x_discrete(guide = guide_axis(n.dodge=2))+
            scale_y_continuous(labels = comma)+
            coord_cartesian(ylim = c(0,10000))
        
        
        if (!is.null(req(click_result()))){
            
            if(req(click_result()) == "successful"){
                text = "Pledged Amount of Successful Projects"
            }else{
                text = "Pledged Amount of Failed Projects"
            }
            
            box <- ks_cleaned %>%
                filter(state == click_result()) %>%
                ggplot(aes(y = usd_pledged_real, x = main_category)) +
                geom_boxplot()+
                scale_x_discrete(guide = guide_axis(n.dodge=2))+
                scale_y_continuous(labels = comma)+
                coord_cartesian(ylim = c(0,10000)) +
                ggtitle(text)
            
        }
        
        return(box)
    })
    
    #Tab 3 - display bar plot
    output$bar_plot1 <- renderPlot({
        
        #prepare the bar
        bp <- bpdata%>%
            ggplot(aes(x="",y=per,  fill=Project_Status)) + geom_col() +
            geom_text(aes(label = round(per*100,1)), position = position_stack(vjust = 0.5))+
            theme_void() +
            ggtitle("Summary of Success (%)") + ylab("") +
            scale_color_brewer(palette= "Blues")
        
        #build blank theme
        blank_theme <- theme_minimal()+
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid=element_blank(),
                axis.ticks = element_blank(),
                plot.title=element_text(size=14, face="bold") +
                    scale_color_brewer(palette= "Blues")
            )
        
        #apply blank theme and blue theme to bar
        bp <- bp + scale_fill_brewer(palette= "Blues", name="Legend", labels=c("Failed","Successful")) + blank_theme
        
        #return bar to ui
        return(bp)
    })
    
    bclick_result <- reactive({
        ipc <- req(input$bplot_click)
        y_val <- ipc$y
        bp_decode %>% rowwise %>% filter(
            between(y_val,
                    lagcsum,
                    csum)) %>% pull(Project_Status)
    })
    
    # output$barresult <- renderText({
    #     if(req(bclick_result()) == "successful"){
    #         text = "Pledged Amount of Successful Projects"
    #     }else{
    #         text = "Pledged Amount of Failed Projects"
    #     }
    #     return(text)
    # })
    
    #Boxplot corresponding to bar chart
    output$box_plot2 <- renderPlot({
        
        if (!is.null(req(click_result()))){
            
            if(req(bclick_result()) == "successful"){
                text = "Pledged Amount of Successful Projects"
            }else{
                text = "Pledged Amount of Failed Projects"
            }
        }
        
        box <- ks_cleaned %>%
            ggplot(aes(y = usd_pledged_real, x = main_category)) +
            geom_boxplot()+
            scale_x_discrete(guide = guide_axis(n.dodge=2))+
            scale_y_continuous(labels = comma)+
            coord_cartesian(ylim = c(0,10000))+ 
            ggtitle(text)
            
        
        
        if (!is.null(req(bclick_result()))){
            box <- ks_cleaned %>%
                filter(state == bclick_result())%>%
                ggplot(aes(y = usd_pledged_real, x = main_category)) +
                geom_boxplot()+
                scale_x_discrete(guide = guide_axis(n.dodge=2))+
                scale_y_continuous(labels = comma)+
                coord_cartesian(ylim = c(0,10000)) +
                ggtitle(text)
            
        }
        
        return(box)
    })
    
####################
####End of Tab 3####
####################
    
#############
####Tab 2####
#############
    
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
                                                                   & (abs(as.numeric(ks_cleaned$launch_period) - as.numeric((input$tab2_project_date_end - input$tab2_project_date_start)))/as.numeric((input$tab2_project_date_end - input$tab2_project_date_start))) < 0.25
                                                                   ,c("name", "main_category", "category", "launched", "deadline", "launch_period", "state", "backers", "country", "usd_goal_real", "usd_pledged_real", "dollar_per_backer")
                                                                   ], options = list(scrollX = TRUE)
    )
    
    #Tab2: Dynamic Input for Launch Date End
    output$tab2_project_date_end_input <- renderUI({
        dateInput("tab2_project_date_end", label = "Your Fundraiser End Date", value = Sys.Date()+30, min = input$tab2_project_date_start+1)
    })
    
    #Tab2: Success Rate
    output$tab2_success_rate_plot <- renderPlot({
        bp <- 
        ks_cleaned[ks_cleaned$main_category == input$tab2_main_category
           & ks_cleaned$category == input$tab2_sub_category
           & if(input$tab2_goal_usd == 0){TRUE} else {abs(ks_cleaned$usd_goal_real - input$tab2_goal_usd) / input$tab2_goal_usd < 0.25}
           & (abs(as.numeric(ks_cleaned$launch_period) - as.numeric((input$tab2_project_date_end - input$tab2_project_date_start)))/as.numeric((input$tab2_project_date_end - input$tab2_project_date_start))) < 0.25
           ,] %>%
            mutate(project_status = if_else_(state == "successful", "success", "fail")) %>%
            group_by(project_status) %>%
            summarise(count = n())%>%
            mutate(per=count/sum(count)) %>%
            ungroup %>%
            ggplot(aes(x="",y=per,  fill=project_status)) + geom_col() +
            geom_text(aes(label = round(per*100,1)), position = position_stack(vjust = 0.5))+
            coord_polar("y", start = 0) + theme_void() +
            ggtitle("Summary of Success (%)") + ylab("") +
            theme(plot.title = element_text(hjust = 0.5))
        
        return(bp)
    })
    
    #Tab2: Filter Details
    output$tab2_similar_projects_text <- renderText(
                                             paste(
                                                 "Comparing ", input$tab2_main_category, ": ", input$tab2_sub_category,
                                                 " Projects ",
                                                 if_else_(input$tab2_goal_usd == 0, "Across All Goal Amounts", paste(
                                                     "with a Goal Amount in the Range of ",
                                                     input$tab2_goal_usd*(1-0.25),
                                                     " to ",
                                                     input$tab2_goal_usd*(1+0.25), " USD"
                                                 )
                                                 ),
                                                 ", and with a Fundraising Period of around ",
                                                 round((input$tab2_project_date_end - input$tab2_project_date_start)*(1-0.25)),
                                                 "-",
                                                 round((input$tab2_project_date_end - input$tab2_project_date_start)*(1+0.25)),
                                                 " days."
                                             )
    )
    
    #Tab2: Breakdown of Backers (USD per Backer)
    output$tab2_usd_per_backer_plot <- renderPlot({
        
        output_plot <-
            ks_cleaned[ks_cleaned$main_category == input$tab2_main_category
                       & ks_cleaned$category == input$tab2_sub_category
                       & if(input$tab2_goal_usd == 0){TRUE} else {abs(ks_cleaned$usd_goal_real - input$tab2_goal_usd) / input$tab2_goal_usd < 0.25}
                       & (abs(as.numeric(ks_cleaned$launch_period) - as.numeric((input$tab2_project_date_end - input$tab2_project_date_start)))/as.numeric((input$tab2_project_date_end - input$tab2_project_date_start))) < 0.25
                       ,] %>%
            ggplot() + geom_histogram(aes(x=dollar_per_backer), binwidth=100, ) + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        
        return(output_plot)
    })
    
    #Tab2: Breakdown of Backers (Count of Backers)
    output$tab2_cnt_backers_plot <- renderPlot ({
        output_plot <-
            ks_cleaned[ks_cleaned$main_category == input$tab2_main_category
                       & ks_cleaned$category == input$tab2_sub_category
                       & if(input$tab2_goal_usd == 0){TRUE} else {abs(ks_cleaned$usd_goal_real - input$tab2_goal_usd) / input$tab2_goal_usd < 0.25}
                       & (abs(as.numeric(ks_cleaned$launch_period) - as.numeric((input$tab2_project_date_end - input$tab2_project_date_start)))/as.numeric((input$tab2_project_date_end - input$tab2_project_date_start))) < 0.25
                       ,] %>%
            ggplot() + geom_histogram(aes(x=backers), binwidth=100, ) + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        
        return(output_plot)
    })
    
    #Tab2: Summary Text Stats
    
    output$tab2_success_rate_text <- renderText(
        paste("Historical Success Rate: ",
              round(ks_cleaned[ks_cleaned$main_category == input$tab2_main_category
                         & ks_cleaned$category == input$tab2_sub_category
                         & if(input$tab2_goal_usd == 0){TRUE} else {abs(ks_cleaned$usd_goal_real - input$tab2_goal_usd) / input$tab2_goal_usd < 0.25}
                         & (abs(as.numeric(ks_cleaned$launch_period) - as.numeric((input$tab2_project_date_end - input$tab2_project_date_start)))/as.numeric((input$tab2_project_date_end - input$tab2_project_date_start))) < 0.25
                         ,] %>%
                mutate(project_status = if_else_(state == "successful", "success", "fail")) %>%
                group_by(project_status) %>%
                summarise(count = n())%>%
                mutate(per=count/sum(count)) %>%
                filter(project_status == "success") %>%
                pull(per)*100, 2),
             "%"
             )
    )
    
    output$tab2_mean_cnt_success_backers_text <- renderText(
        paste("Average Count of Backers in Successful Projects: ",
              round(ks_cleaned[ks_cleaned$main_category == input$tab2_main_category
                         & ks_cleaned$category == input$tab2_sub_category
                         & if(input$tab2_goal_usd == 0){TRUE} else {abs(ks_cleaned$usd_goal_real - input$tab2_goal_usd) / input$tab2_goal_usd < 0.25}
                         & (abs(as.numeric(ks_cleaned$launch_period) - as.numeric((input$tab2_project_date_end - input$tab2_project_date_start)))/as.numeric((input$tab2_project_date_end - input$tab2_project_date_start))) < 0.25
                         ,] %>%
                  mutate(project_status = if_else_(state == "successful", "success", "fail")) %>%
                  group_by(project_status) %>%
                  summarise(avg_backers = mean(backers))%>%
                  filter(project_status == "success") %>%
                  pull(avg_backers), 2)
        )
    )
    
    output$tab2_mean_cnt_failed_backers_text <- renderText(
        paste("Average Count of Backers in Failed Projects: ",
              round(ks_cleaned[ks_cleaned$main_category == input$tab2_main_category
                         & ks_cleaned$category == input$tab2_sub_category
                         & if(input$tab2_goal_usd == 0){TRUE} else {abs(ks_cleaned$usd_goal_real - input$tab2_goal_usd) / input$tab2_goal_usd < 0.25}
                         & (abs(as.numeric(ks_cleaned$launch_period) - as.numeric((input$tab2_project_date_end - input$tab2_project_date_start)))/as.numeric((input$tab2_project_date_end - input$tab2_project_date_start))) < 0.25
                         ,] %>%
                  mutate(project_status = if_else_(state == "successful", "success", "fail")) %>%
                  group_by(project_status) %>%
                  summarise(avg_backers = mean(backers))%>%
                  filter(project_status == "fail") %>%
                  pull(avg_backers), 2)
        )
    )
    
    output$tab2_mean_success_usd_per_backer_text <- renderText(
        paste("Average USD Pledge per Backer in Successful Projects: ",
              round(ks_cleaned[ks_cleaned$main_category == input$tab2_main_category
                         & ks_cleaned$category == input$tab2_sub_category
                         & if(input$tab2_goal_usd == 0){TRUE} else {abs(ks_cleaned$usd_goal_real - input$tab2_goal_usd) / input$tab2_goal_usd < 0.25}
                         & (abs(as.numeric(ks_cleaned$launch_period) - as.numeric((input$tab2_project_date_end - input$tab2_project_date_start)))/as.numeric((input$tab2_project_date_end - input$tab2_project_date_start))) < 0.25
                         ,] %>%
                  mutate(project_status = if_else_(state == "successful", "success", "fail")) %>%
                  group_by(project_status) %>%
                  summarise(avg_usd = mean(dollar_per_backer))%>%
                  filter(project_status == "success") %>%
                  pull(avg_usd), 2),
              " USD"
        )
    )

    output$tab2_mean_failed_usd_per_backer_text <- renderText(
        paste("Average USD Pledge per Backer in Failed Projects: ",
              round(ks_cleaned[ks_cleaned$main_category == input$tab2_main_category
                         & ks_cleaned$category == input$tab2_sub_category
                         & if(input$tab2_goal_usd == 0){TRUE} else {abs(ks_cleaned$usd_goal_real - input$tab2_goal_usd) / input$tab2_goal_usd < 0.25}
                         & (abs(as.numeric(ks_cleaned$launch_period) - as.numeric((input$tab2_project_date_end - input$tab2_project_date_start)))/as.numeric((input$tab2_project_date_end - input$tab2_project_date_start))) < 0.25
                         ,] %>%
                  mutate(project_status = if_else_(state == "successful", "success", "fail")) %>%
                  group_by(project_status) %>%
                  summarise(avg_usd = mean(dollar_per_backer))%>%
                  filter(project_status == "fail") %>%
                  pull(avg_usd), 2),
              " USD"
        )
    )
####################
####End of Tab 2####
####################
    
    # Creating Global Data Frame
    data_frame_of_input <- reactiveValues(
        input_data = data.frame()
    )
    
    df_input <- reactive({
        data_frame_of_input$input_data = 
            data.frame(
            category = c(input$tab2_sub_category), 
            main_category = c(input$tab2_main_category), 
            usd_goal_real = c(input$tab2_goal_usd), 
            launch_period = c(input$tab2_project_date_end - input$tab2_project_date_start)
        )
        
        return(data_frame_of_input$input_data)
    })
    
    output$df <- renderDataTable(
        data.frame(
            main_category = c(input$tab2_main_category), 
            sub_category = c(input$tab2_sub_category), 
            goal_in_usd = c(input$tab2_goal_usd), 
            launch_period = c(input$tab2_project_date_end - input$tab2_project_date_start)
        )
    )
    
    output$tab2_glm_prediction_text <- renderText(
        paste("Logistic Model Prediction: ",
              if(input$tab2_project_name == "") {"Unnamed Project"} else {input$tab2_project_name},
              " is predicted to ",
        
        ifelse(
            tab2_glm %>% predict(
                data.frame(
                category = c(input$tab2_sub_category), 
                main_category = c(input$tab2_main_category), 
                usd_goal_real = c(input$tab2_goal_usd), 
                launch_period = c(input$tab2_project_date_end - input$tab2_project_date_start)
            )
            , type = "response")
            > input$tab2_success_prediction_criteria/100, "be successful", "fail")
        
        )
        
    )
    
    output$tab2_glm_prediction_num_text <- renderText(
        tab2_glm %>% predict(
            data.frame(
                category = c(input$tab2_sub_category), 
                main_category = c(input$tab2_main_category), 
                usd_goal_real = c(input$tab2_goal_usd), 
                launch_period = c(input$tab2_project_date_end - input$tab2_project_date_start)
            ), type = "response")
    )
    
    output$tab2_glm_prediction_num_plot <- renderGauge({
        
        predict_result <- tab2_glm %>% predict(
            data.frame(
                category = c(input$tab2_sub_category), 
                main_category = c(input$tab2_main_category), 
                usd_goal_real = c(input$tab2_goal_usd), 
                launch_period = c(input$tab2_project_date_end - input$tab2_project_date_start)
            ), type = "response")
        
        plot <- gauge(
                round(predict_result[[1]]*100),
                min = 0,
                max = 100,
                sectors = gaugeSectors(
                    success = c(input$tab2_success_prediction_criteria,100),
                    danger = c(0,input$tab2_success_prediction_criteria)
                )
        )
                
        return(plot)
    })
    
    output$confusion_matrix_text <- renderPrint({
        
        probabilities <- tab2_glm %>% predict(test_data[, c("category", "main_category", "usd_goal_real", "launch_period")])
        
        predicted.classes <- ifelse(probabilities > input$tab2_success_prediction_criteria/100, "successful", "failed")
        
        mean(predicted.classes == test_data$restate)
        
        cm <- confusionMatrix(data=as.factor(predicted.classes) , reference = as.factor(test_data$restate), positive = "successful")
        
        return(cm)
    })
    
}

shinyApp(ui, server)
