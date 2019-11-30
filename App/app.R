#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(ggplot2)
library(readr)
library(readxl)
library(Matching)
library(janitor)
library(tidyverse)

state <- read_csv("by_state.csv")
cz <- read_csv("by_cz.csv")

quantiles <- c("1st" = state$par_q1,
               "2nd" = state$par_q2,
               "3rd" = state$par_q3,
               "4th" = state$par_q4,
               "5th" = state$par_q5)

quantiles_cz <- c("1st" = cz$par_q1,
                  "2nd" = cz$par_q2,
                  "3rd" = cz$par_q3,
                  "4th" = cz$par_q4,
                  "5th" = cz$par_q5)

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("Inventors In The US", 
               tabPanel("About"),
               tabPanel("Income",
                   navlistPanel(
                       tabPanel("By State",
                        fluidPage(
                            titlePanel("By State"),
                            p(helpText("The x-axis represents the fraction of parents in a specific 
                                       income quintile. 1 represents the bottom quintile and 5 represents the top quintile.")),
                            selectInput(inputId = "in_q", label = "Select an Income Quntile", 
                                        choices = quantiles),
                        mainPanel(
                            plotOutput("plot1")))),
                       tabPanel("By Commuting Zone",
                                fluidPage(
                                    titlePanel("By Commuting Zone"),
                                    p(helpText("The x-axis represents the fraction of parents in a specific 
                                       income quintile. 1 represents the bottom quintile and 5 represents the top quintile.")),
                                    selectInput(inputId = "in_q2", label = "Select an Income Quntile", choices = quantiles_cz),
                                    mainPanel(
                                        plotOutput("plot2"))
                                )),
                       tabPanel("By College")
                        )),
              tabPanel("Economic Mobility",
                    navlistPanel(
                        tabPanel("Backgroung"),
                        tabPanel("By State",
                                 tableOutput("match_state")),
                        tabPanel("By Commuting Zone")
                        
                    )
               )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
            

    # inventors vs income quantil by state
    output$plot1 <- renderPlot({
        
        ggplot(data = state, aes(x = input$in_q, y =inventor)) +
                   geom_point() 
    })
        
    # inventors vs income quantil by cz
    output$plot2 <- renderPlot({
            ggplot(data = cz, aes(x = input$in_q2, y =inventor)) +
                geom_point() 
    }) 
    
    # matching on state level
    
    # define the treatment
    state <- state %>%
        mutate(treatment = ifelse(cohort_mean > 0.5, 1, 0))
    # set controls
    controls <-  state %>%
        select(par_q5, par_q4, par_q3, par_q2, par_q1)
    
    output$match_state <- renderTable({
         match <- Match(Y = state$inventor, Tr = state$treatment, X = controls)
    summary(match)
    })
   
    
    
    # matching on cz level
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
