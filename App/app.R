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
    theme = shinytheme("flatly"),
    navbarPage("Inventors In The US", 
              
               tabPanel("Income",
                   navlistPanel(
                       tabPanel("Background",
                                fluidPage(
                                    titlePanel("Is Innovation Related to Income?"),
                                    br(),
                                    p("What is the economic background of inventors in the US? Is the income distribution of parents in a specific state related to the children inventors rate in that state? Is the relationship similar at  the commuting zone or at the college level?"),
                                    p("In this tab you can interactively explore the relationship between parents’ income distribution and inventors rate at the state, commuting zone and college level.
                                      Parents income is divided into five equal quintiles, where the fifth quintile represents the top 20th percentile."),
                                    p("In each graph, the vertical axis represents the inventors rate, and the horizontal axis represents the share of parents in the population that are part of the selected income quintile. Therefore, a higher value on the x axis means a 
                                    larger share of the population that are part of the selected quintile.")
                                )),
                       tabPanel("By State",
                        fluidPage(
                            titlePanel("By State"),
                            br(),
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
                                    br(),
                                    p(helpText("The x-axis represents the fraction of parents in a specific 
                                       income quintile. 1 represents the bottom quintile and 5 represents the top quintile.")),
                                    selectInput(inputId = "in_q2", label = "Select an Income Quntile", choices = quantiles_cz),
                                    mainPanel(
                                        plotOutput("plot2")))),
                       tabPanel("By College",
                                fluidPage(
                                    titlePanel("By College"),
                                    br(),
                                    p(helpText("The x-axis represents the fraction of students with parents in a specific 
                                       income quintile. 1 represents the bottom quintile and 5 represents the top quintile.")),
                                    selectInput(inputId = "in_q3", label = "Select an Income Quntile", choices = quantiles_col),
                                    mainPanel(
                                        plotOutput("plot3")))))),
               tabPanel("Gender",
                        navlistPanel(
                            tabPanel("Background",
                                     fluidPage(
                                         titlePanel("Is The Share of Female Inventors Related to Parents' Income?"),
                                         br(),
                                         p("In this tab you can interactively explore the associated relationship between income level and female inventors share. The female inventors share is calculated as the percentage of female inventors out of the inventors in a specific state or commuting zone. This is different from the female inventors rate itself, which represents the fraction of female inventors out of the whole population, and thus does not account for differences in magnitude of inventors rate across different places. For example, state X can have a higher female inventors rate than state B, but a lower fraction of female inventors out of the total inventors in that state."),
                                         p("The data shows that the relationship between parents’ income and female inventors share is weaker,  but we can still see that a higher share of the population in the upper quantile is associated with a higher female inventors share.")
                                               )),
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
                                         br(),
                                         p(helpText("The x-axis represents the fraction of parents in a specific 
                                       income quintile. 1 represents the bottom quintile and 5 represents the top quintile.")),
                                         selectInput(inputId = "in_qen_cz", label = "Select an Income Quntile", choices = quantiles_cz),
                                         mainPanel(
                                             plotOutput("plot5"))))
                            )),
              tabPanel("Economic Mobility",
                    navlistPanel(
                        tabPanel("Background",
                                 titlePanel("Background"),
                                 br(),
                                 p("In the ‘Income’ tab we saw that as the share of the population in the upper quintiles is higher, the inventors rate of a given place is relatively higher as well. However, we can still see Places who share a similar share of a population in a given quantile, but have a stark difference in their inventors rate. "),
                                 p("There are several reasons that can explain this. It might be, for example, that several commuting zones have a similar share of the population in the bottom quintile, but a different share of their population in the top quintile. This explanation, however, is not sufficient when we compare the inventors rate of commuting zones  that have a similar income distribution across the five quintiles.
"),
                                 p("Aside of income distribution, commuting zones and states differ by other variables such as  racial characteristics, education level, population size and etc. In this tab, I have used a matching model in order to see whether economic mobility can explain the difference in inventors rate between states, commuting zones and colleges that share similar characteristics. 
"),
                                 p("The matching models show different results for the different area levels - states, commuting zones and colleges. It is interesting to see that while the coefficient on economic mobility is not significant at the state level, it is significant at the commuting zone and college level.")),
                        tabPanel("By State",
                                 h2("My model:"),
                                 br(),
                                 p("In order to create the treatment variables, I have created the ‘Tr’ variable, which equals 1 for every state that has a mobility rate above the mean value, and equals 0 otherwise. The control variables I have used are the five income quintiles. 
"),
                                 verbatimTextOutput("match_state"),
                                 br(),
                                 h4("Interpretation:"),
                                 p("The treatment estimate at the state level is insignificant and is relatively small in magnitude, implying that economic mobility is not a variable that explains the difference in inventors rate across states that share similar income distribution. An important caveat that should we mention here, is that I have not control for other variables at the state level model.")),
                        tabPanel("By Commuting Zone",
                                 h2("My Model"),
                                 p("In order to create the treatment variables, I have created the ‘Tr’ variable, which equals 1 for every commuting zone that has a mobility rate above the mean value, and equals 0 otherwise. The control variables I have used are the five income quintiles, percent of black population, violent crime rate, gini index, migration inflow rate,  migration outflow rate, and college graduation rate (income adjusted)."),
                                 verbatimTextOutput("match_cz"),
                                br(),
                                h4("Interpretation:"),
                                p("The estimate is significant with a P-value of 0.006033. The estimated value is 0.0003321, and when comparing it to the mean value of inventors rate (0.00181) we can see that the magnitude is large."),
                                  p("This result implies that economic mobility might be a confounding variable that impacts the relationship between parents’ income and children inventors rate at the commuting zone level.
")),
                        tabPanel("By College",
                                 h2("My model:"),
                                 br(),
                                 p("In order to create the treatment variables, I have created the ‘Tr’ variable, which equals 1 for every college that has a mobility rate above the mean value, and equals 0 otherwise. The control variables I have used are the five income quintiles. 
"),
                                 verbatimTextOutput("match_col"),
                                 br(),
                                 h4("Interpretation:"),
                                 p("The estimated coefficient is 0.0093012, with a P-value of approximately 0.02. The mean value of inventors rate at the college level is 0.00881, which shows that the estimated coefficient is very large in magnitude."),
                                 p("This result implies that economic mobility might be a confounding variable that impacts the relationship between parents’ income and children inventors rate at the college level.
"))
                    )
               ),
              tabPanel("About",
                       sidebarLayout(
                           sidebarPanel("",
                                        tags$style(".well background-color:[#E9967A];"),
                                        panel_div(
                                            class_type = "primary",
                                            panel_title = "About Me",
                                            content = (p("My name is Oren Rimon Or, and I am a second-year student at Harvard University, studying Economics and Statistics.
                                                       Please feel free to reach out to me with any question: Oren_rimonor@college.harvard.edu"))),
                                        panel_div(
                                            class_type = "primary",
                                            panel_title = "Source Code",
                                            content = p("Interested in my code? Take a look at my GitHub repository found ", a("here.", href = "https://github.com/oren-rimon/Inventors-in-the-US"))
                                        )),
                           mainPanel(                        
                               titlePanel("About The Project"),
                               p(paste("Who is likely to become an inventor? Does the income of our parents impact our chances of being listed on a patent application? Is a higher 
                                       economic mobility associated with a higher inventors rate, when comparing two places with a similar income distribution?"),
                                 p(HTML(paste("This app is based on the data collected by ", tags$a("Opportunity Insights.", href = "https://opportunityinsights.org/data/"), 'From the data collection and analysis of Opportunity Insights, we can see how inventors rate is highly correlated with parents’ income. 
                                              We can also see how children who grow up in places with a high invention rate, are much more likely to become inventors.'))),
                                 p(paste("In the “Income” tab, you can interactively explore the associated relationship between income level and inventors rate. The data is divided into five income quintiles, where the fifth quintile represents the top 20% Off the income distribution, and the first represents the bottom 20% of the income distribution.")),
                                 p(paste("In the “Gender” tab you can interactively explore the associated relationship between income level and female inventors share. The female inventors share is calculated as the percentage of female inventors out of the inventors in a specific state or commuting zone. This is different from the female inventors rate itself, which represents the fraction of female inventors out of the whole population, and thus does not account for differences in magnitude of inventors rate across different places. For example, state X can have a higher female inventors rate than state B, but a lower fraction of female inventors out of the total inventors in that state.")),
                                 p(paste("My question is why do we see a difference in inventors rate between places who share the same income level. In the “Economic Mobility” I have made three models,  using matching,  in order to see whether economic mobility explains why places with a similar income level have different inventors rate. Places that share the same income level might differ substantially by their economic mobility rate. Economic mobility rate is defined in this data set as the probability of someone to reach the top 20 percentile, given that her parents are at the lowest 20 percentile.
")),
                                 p(paste("The main question of this project is why do we see a difference in inventors rate between places that share similar income distribution. In the “Economic Mobility” tab, I have made three matching models, in order to see whether economic mobility explains why places with a similar income level have different inventors rate. Places that share similar income distribution might differ substantially by their economic mobility rate. For commuting zones and colleges, economic mobility rate is defined in this data set as the probability of someone to reach the top 20% 
                                         percent of the income distribution, given that her parents are at the bottom 20% percent. For states it is defined as the probability of earning more than your parents.")),
                                 br(),
                                 tags$video(src = "introduction.mp4", type = "video/mp4", controls = "controls", width = "500px", height = "400px")
                               ))))))

# Define server logic required to draw a histogram
server <- function(input, output) {
            

    # inventors vs income quantil by state
    output$plot1 <- renderPlot({
        
        colx = input$in_q
        gg <- state %>%
            ggplot(aes(x = get(colx), y =inventor)) +
            geom_point() +
            geom_smooth(method = "lm") +
            ylab("Inventors Rate") +
            xlab("Share of Population in Income Quintile")
        gg
    })
        
    # inventors vs income quantil by cz
    output$plot2 <- renderPlot({
        colx1 = input$in_q2
        gg2 <- cz %>%
            ggplot(aes(x = get(colx1), y =inventor)) +
            geom_point() +
            geom_smooth(method = "lm") +
            ylab("Inventors Rate") +
            xlab("Share of Population in Income Quintile")
        
        gg2 
    }) 
    # inventors vs income quantil by college
    output$plot3 <- renderPlot({
        colx3 = input$in_q3
        gg3 <- college %>%
            ggplot(aes(x = get(colx3), y = inventor)) +
            geom_point() +
            geom_smooth(method = "lm") +
            scale_y_continuous(limits=c(0, 0.05)) +
            ylab("Inventors Rate") +
            xlab("Share of Population in Income Quintile")
        gg3 
    }) 
    
    # gender tab
    # gender - state
    output$plot4 <- renderPlot({
        
        x_var = input$in_qen_state
        pl <- state %>%
            ggplot(aes(x = get(x_var), y = share_f)) +
            geom_point() +
            geom_smooth(method = "lm") +
            ylab("Share of Female Inventors") +
            xlab("Share of Population in Income Quintile")
        pl
    })
    
    # gender - cz
    output$plot5 <- renderPlot({
        
        x_var1 = input$in_qen_cz
        p2 <- cz %>%
            ggplot(aes(x = get(x_var1), y = share_f)) +
            geom_point() +
            geom_smooth(method = "lm")+
            ylab("Share of Female Inventors") +
            xlab("Share of Population in Income Quintile")
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
