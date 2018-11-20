library(shiny)
library(tidyverse)
library(knitr)
library(scales)

shiny_data <- read_rds("shiny_data.rds")

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("2018 Midterm Election Results vs. Forecastss"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: menu for the number of observations to generate ----
      selectInput("type",
                  "Race Type (for Plot Tab):",
                  choices = c("House of Representatives" = "rep",
                              "Senate" = "sen",
                              "Governor" = "gov"),
                  multiple = TRUE),
      
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Select the random distribution type ----
      radioButtons("data", 
                   "Information Source (for Summary Tab):",
                   c("Results" = "actual_rep_advantage",
                     "Polls" = "predicted_rep_advantage")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: menu for the number of observations to generate ----
      sliderInput("observations",
                  "Number of Observations Type (for Table Tab):",
                  min = 1,
                  max = 66,
                  value = 10)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
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
    
    shiny_data %>%
      filter(position %in% input$type) %>% 
      ggplot(aes(x = predicted_rep_advantage, y = actual_rep_advantage)) +
      geom_point() +
      geom_smooth(method="lm", se=FALSE) +
      labs(x = "Predicted",
           y = "Actual",
           title = "Predicted vs. Actual Republican Advantage in U.S. 2018 Midterm Election",
           subtitle = "Calculated by dividing the difference between Republican and Democrat votes by total votes cast")
    
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    
    shiny_data %>% 
      select(input$data) %>% 
      summary()
    
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    
    shiny_data %>% 
      mutate(actual_rep_advantage = percent(actual_rep_advantage),
             predicted_rep_advantage = percent(predicted_rep_advantage)) %>% 
      rename("Race" = "race", 
             "Results" = "actual_rep_advantage", 
             "Forecast" = "predicted_rep_advantage", 
             "Position" = "position") %>% 
      head(input$observations)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)