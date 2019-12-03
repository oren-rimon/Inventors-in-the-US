#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(ggplot2)
library(readr)
library(readxl)
library(shinyLP)
library(Matching)
library(shinythemes)
library(janitor)
library(tidyverse)

state <- read_csv("by_state.csv")
cz <- read_csv("by_cz.csv")
college <- read_csv("by_college.csv")

# matching 
cz_matching <- read_csv("cz_matching.csv")
col_matching <- read_csv("col_matching.csv")



# list of choices for commuting zone income quantile

quantiles_cz <- c("1st" = "par_q1",
                  "2nd" = "par_q2",
                  "3rd" = "par_q3",
                  "4th" = "par_q4",
                  "5th" = "par_q5")

# list of choices for college income quantile

quantiles_col <- c("1st" = "share_q1",
                  "2nd" = "share_q2",
                  "3rd" = "share_q3",
                  "4th" = "share_q4",
                  "5th" = "share_q5")


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("sandstone"),
    navbarPage("Inventors In The US", 
               tabPanel("About",
                        sidebarLayout(
                            sidebarPanel("",
                                         panel_div(
                                             class_type = "primary",
                                             panel_title = "About Me",
                                             content = (p("My name is Oren Rimon Or, and I am a second-year student at Harvard, studying Economics and Statistics.
                                                          Please feel free to reach out to me with any question about the project: Oren_rimonor@college.harvard.edu"))),
                                         panel_div(
                                             class_type = "primary",
                                             panel_title = "Source Code",
                                             content = "Interested in my code? Take a look at the GitHub repository found here."
                                         )),
                            mainPanel(                        
                                titlePanel("About The Project"),
                                           p(paste("Who is likely to become an inventor? Does the income of our parents impact our chances of being listed on a patent application? Is a higher economic mobility associated with a higher inventors rate, when comparing two places with a similar income level?

This app is based on the data collected by Opportunity Insights. From Opportunity Insights data collection and analysis, we can see how inventors rate is highly correlated with parent income. We can also see how children who grow up in places with a high invention rate,  are much more likely to become inventors. 

In the “Income” tab, you can interactively explore  the associated relationship between income level and invention rate. The data is divided into five income quintiles,  when the first quintile represents the bottom 20 percentile,  and the fifth quintile represents the top 20 percentile. 

In the “Gender”  you can interactively explore the associated relationship between income level and female inventors share. The female inventors share is calculated as the percentage of female inventors among the inventors in a specific state or commuting zone. This is different  from the female inventors rate. 

My question is why do we see a difference in inventors rate between places who share the same income level. In the “Economic Mobility” I have made three models,  using matching,  in order to see whether economic mobility explains why places with a similar income level have different inventors rate. Places that share the same income level might differ substantially by their economic mobility rate. Economic mobility rate is defined in this data set as the probability of someone to reach the top 20 percentile, given that her parents are at the lowest 20 percentile.

There are many variables that can explain the difference in inventors rate across places with similar income level. Teachers to students ratio is one small example,  but there are many more.   Therefore,  in my model I have matched  places on income level,  but also on other variables such as crime rate, gini coefficient and more. 

I have decided to use the model of matching since I wanted to see whether household income is a potential confounding variable that affects the relationship between invention rate and economic mobility. Therefore, I matched neighborhoods with similar income level and other relevant variables, and checked whether those with a higher invention rate have a higher economic mobility as well. 
"))
                                )
                        )),
               tabPanel("Income",
                   navlistPanel(
                       tabPanel("By State",
                        fluidPage(
                            titlePanel("By State"),
                            p(helpText("The x-axis represents the fraction of parents in a specific 
                                       income quintile. 1 represents the bottom quintile and 5 represents the top quintile.")),
                            selectInput(inputId = "in_q", label = "Select an Income Quntile", 
                                        choices = c("1st" = "par_q1",
                                                    "2nd" = "par_q2",
                                                    "3rd" = "par_q3",
                                                    "4th" = "par_q4",
                                                    "5th" = "par_q5")),
                        mainPanel(
                            plotOutput("plot1")))),
                       tabPanel("By Commuting Zone",
                                fluidPage(
                                    titlePanel("By Commuting Zone"),
                                    p(helpText("The x-axis represents the fraction of parents in a specific 
                                       income quintile. 1 represents the bottom quintile and 5 represents the top quintile.")),
                                    selectInput(inputId = "in_q2", label = "Select an Income Quntile", choices = quantiles_cz),
                                    mainPanel(
                                        plotOutput("plot2")))),
                       tabPanel("By College",
                                fluidPage(
                                    titlePanel("By College"),
                                    p(helpText("The x-axis represents the fraction of students with parents in a specific 
                                       income quintile. 1 represents the bottom quintile and 5 represents the top quintile.")),
                                    selectInput(inputId = "in_q3", label = "Select an Income Quntile", choices = quantiles_col),
                                    mainPanel(
                                        plotOutput("plot3")))))),
               tabPanel("Gender",
                        titlePanel("Is The Share of Female Inventors Related To Income Distribution?"),
                        navlistPanel(
                            tabPanel("By State",
                                     fluidPage(
                                         titlePanel("By State"),
                                         p(helpText("The x-axis represents the fraction of parents in a specific 
                                       income quintile. 1 represents the bottom quintile and 5 represents the top quintile.")),
                                         selectInput(inputId = "in_qen_state", label = "Select an Income Quntile", 
                                                     choices = c("1st" = "par_q1",
                                                                 "2nd" = "par_q2",
                                                                 "3rd" = "par_q3",
                                                                 "4th" = "par_q4",
                                                                 "5th" = "par_q5")),
                                                     mainPanel(
                                                         plotOutput("plot4"))
                                         )),
                            tabPanel("By Commuting Zone",
                                     fluidPage(
                                         titlePanel("By Commuting Zone"),
                                         p(helpText("The x-axis represents the fraction of parents in a specific 
                                       income quintile. 1 represents the bottom quintile and 5 represents the top quintile.")),
                                         selectInput(inputId = "in_qen_cz", label = "Select an Income Quntile", choices = quantiles_cz),
                                         mainPanel(
                                             plotOutput("plot5"))))
                            )),
              tabPanel("Economic Mobility",
                    navlistPanel(
                        tabPanel("Backgroung"),
                        tabPanel("By State",
                                 h2("My model:"),
                                 verbatimTextOutput("match_state"),
                                 br(),
                                 h2("Interactive model:"),
                                 p(helpText("Select control variables. States will be matched on income level and on the variables you select. Please consult the table below for more details about the control variables."),
                                   checkboxGroupInput(inputId = "control_1", label = "Select control variables", choices = c("")))),
                        tabPanel("By Commuting Zone",
                                 verbatimTextOutput("match_cz")),
                        tabPanel("By College",
                                 verbatimTextOutput("match_col"))
                    )
               )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
            

    # inventors vs income quantil by state
    output$plot1 <- renderPlot({
        
        colx = input$in_q
        gg <- state %>%
            ggplot(aes(x = get(colx), y =inventor)) +
            geom_point() +
            geom_smooth(method = "lm")
        gg
    })
        
    # inventors vs income quantil by cz
    output$plot2 <- renderPlot({
        colx1 = input$in_q2
        gg2 <- cz %>%
            ggplot(aes(x = get(colx1), y =inventor)) +
            geom_point() +
            geom_smooth(method = "lm")
        gg2 
    }) 
    # inventors vs income quantil by college
    output$plot3 <- renderPlot({
        colx3 = input$in_q3
        gg3 <- college %>%
            ggplot(aes(x = get(colx3), y = inventor)) +
            geom_point() +
            geom_smooth(method = "lm") +
            scale_y_continuous(limits=c(0, 0.05))
        gg3 
    }) 
    
    # gender tab
    # gender - state
    output$plot4 <- renderPlot({
        
        x_var = input$in_qen_state
        pl <- state %>%
            ggplot(aes(x = get(x_var), y = share_f)) +
            geom_point() +
            geom_smooth(method = "lm")
        pl
    })
    
    # gender - cz
    output$plot5 <- renderPlot({
        
        x_var1 = input$in_qen_cz
        p2 <- cz %>%
            ggplot(aes(x = get(x_var1), y = share_f)) +
            geom_point() +
            geom_smooth(method = "lm")
        p2
    })
    
    
    # matching on state level
    
    # define the treatment
    state <- state %>%
        mutate(treatment = ifelse(cohort_mean > 0.5, 1, 0))
    # set controls
    controls <-  state %>%
        select(par_q5, par_q4, par_q3, par_q2, par_q1)
    
    output$match_state <- renderPrint({
         summary(Match(Y = state$inventor, Tr = state$treatment, X = controls))
    })
   
    # matching on cz level
    
    controls_cz <-  cz_matching %>%
        select(par_q5, par_q4, par_q3, par_q2, par_q1, frac_black, violent_crime_rate, gini, migration_inflow_rate, migration_outlflow_rate, college_graduation_rate_income_adjusted)
    
    output$match_cz <- renderPrint({
         summary(Match(Y = cz_matching$inventor, Tr = cz_matching$treatment, X = controls_cz)) 
    }) 
    
    # matching on college level
    
    controls_col <- col_matching %>%
        select(share_q1, share_q2, share_q3, share_q4, share_q5)
    
    output$match_col <- renderPrint({
            summary(Match(Y = col_matching$inventor, Tr = col_matching$treatment, X = controls_col))
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
