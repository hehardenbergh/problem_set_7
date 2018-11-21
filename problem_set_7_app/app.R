library(shiny)
library(tidyverse)
library(knitr)
library(scales)
library(shinyWidgets)
library(stargazer)
library(haven)
library(sjlabelled)
library(rsconnect)
library(shinythemes)
library(plotly)

shiny_data <- read_rds("final_shiny_data.rds")

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("2018 Midterm Election Results vs. Forecasts"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: drop-down menu to display selections for x axis, y axis, and state  ----
      selectInput("x",
                  "X-axis:",
                   choices = c("Percent Female" = "pct_female", 
                               "Percent White" = "pct_white", 
                               "Percent Likely to Vote" = "pct_likely", 
                               "Percent with a College Degree" = "pct_college", 
                               "Percent Undecided Voters" = "pct_und", 
                               "Percent Millenials" = "pct_mil")),
      selectInput("y",
                  "Y-axis:",
                  choices = c("Predicted Democratic Advantage" = "poll_dem_advantage",
                              "Actual Democratic Advantage" = "actual_dem_advantage")
                  ),
      pickerInput("state",
                  "State(s)",
                  choices = c("Arizona" = "AZ",
                              "California" = "CA",
                              "Colorado" = "CO",
                              "Florida" = "FL",
                              "Georgia" = "GA",
                              "Iowa" = "IA",
                              "Illinois" = "IL",
                              "Kansas" = "KS",
                              "Kentucky" = "KY",
                              "Maine" = "ME",
                              "Michigan" = "MI",
                              "Minnesota" = "MN", 
                              "North Carolina" = "NC",
                              "New Jersey" = "NJ",
                              "New Mexico" = "NM",
                              "New York" = "NY",
                              "Ohio" = "OH",
                              "Pennsylvania" = "PA",
                              "Texas" = "TX",
                              "Utah" = "UT",
                              "Virginia" = "VA",
                              "Washington" = "WA",
                              "West Virginia" = "WV"),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),
      
      # create checkbox for linear model
      checkboxInput("line", label = "Add linear model")
  
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plotand two tables ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Polled Democratic Advantage", htmlOutput("polled")),
                  tabPanel("Actual Democratic Advantage", htmlOutput("actual"))
      )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    
    # filter data for selected states
    plot_data <- shiny_data %>% filter(state %in% input$state)
    
    # use if statement to create different plots for with and without linear model selection
    if(input$line == TRUE) {
        ggplot(plot_data, aes_string(x = input$x, y = input$y)) +
        geom_point() +
        geom_smooth(method="lm", se=FALSE) +
        labs(x = "Selected X-Value (Percentage)",
             y = "Selected Y-Value",
             title = "Predicted vs. Actual Republican Advantage in U.S. 2018 Midterm Election",
             subtitle = "Calculated by dividing the difference between Republican and Democrat votes by total votes cast")
    }
    else{
      ggplot(plot_data, aes_string(x = input$x, y = input$y)) +
        geom_point() +
        labs(x = "Selected X-Value (Percentage)",
             y = "Selected Y-Value",
             title = "Predicted vs. Actual Republican Advantage in U.S. 2018 Midterm Election",
             subtitle = "Calculated by dividing the difference between Republican and Democrat votes by total votes cast")
    }
    })
   
  # display regression table
  output$polled <- renderUI({
     
    HTML(
      stargazer(lm(data = shiny_data, poll_dem_advantage ~ pct_female + pct_white + pct_likely + pct_college + pct_und + pct_mil), 
                type = "html",
                column.labels = "Polled Democratic Advantage",
                covariate.labels = c("Percent Female", "Percent White", "Percent Likely to Vote", "Percent with College Degree +", "Percent Undecided", "Percent Millenial"),
                notes = "Covariates calculated using Upshot data: Percent Female calculated by dividing number of female respondents in each district by total respondents, Percent White calculated by dividing number of white respondents in each district by total respondents, Percent Likely to Vote calculated by dividing number of respondents in \"Already voted\", \"Almost certain\", and \"Very likely\" by total respondents, Percent with College Degree or More	calculated by dividing number of respondents with a 4-year college degree or more by total respondents, Percent Undecided calculated by dividing number of respondents who were Undecided by total respondents, and Percent Millenial calculated by dividing number of respondents below 34 by total respondents.",
                notes.align = "c")
    )
  })
  
  
  # display regression table
  output$actual <- renderUI({
  
    HTML(
      stargazer(lm(data = shiny_data, actual_dem_advantage ~ pct_female + pct_white + pct_likely + pct_college + pct_und + pct_mil), 
                type = "html",
                column.labels = "Actual Democratic Advantage",
                covariate.labels = c("Percent Female", "Percent White", "Percent Likely to Vote", "Percent with College Degree +", "Percent Undecided", "Percent Millenial"),
                notes = "Covariates calculated using Upshot data: Percent Female calculated by dividing number of female respondents in each district by total respondents, Percent White calculated by dividing number of white respondents in each district by total respondents, Percent Likely to Vote calculated by dividing number of respondents in \"Already voted\", \"Almost certain\", and \"Very likely\" by total respondents, Percent with College Degree or More	calculated by dividing number of respondents with a 4-year college degree or more by total respondents, Percent Undecided calculated by dividing number of respondents who were Undecided by total respondents, and Percent Millenial calculated by dividing number of respondents below 34 by total respondents.",
                notes.align = "c")
    )
   })
  
}

# Create Shiny app ----
shinyApp(ui, server)