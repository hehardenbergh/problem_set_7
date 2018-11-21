library(shiny)
library(tidyverse)
library(knitr)
library(scales)

shiny_data <- read_rds("final_shiny_data.rds")
shiny_states <- read_rds("shiny_states.rds")
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("2018 Midterm Election Results vs. Forecastss"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: drop-down menu to display observations  ----
      selectInput("x",
                  "X-axis:",
                   choices = c(`Percent Female` = "pct_female", 
                              `Percent White` = "pct_white", 
                              `Percent Likely to Vote` = "pct_likely", 
                              `Percent with a College Degree` = "pct_college", 
                              `Percent Undecided Voters` = "pct_und", 
                              `Percent Millenials` = "pct_mil")),
      selectInput("y",
                  "Y-axis:",
                  choices = c(`Predicted Democratic Advantage` = "poll_dem_advantage",
                              `Actual Democratic Advantage` = "actual_dem_advantage"),
                  multiple = TRUE),
      selectInput("state",
                  "State(s)",
                  choices = names(shiny_states)),
      
      checkboxInput("line", label = "Add linear model"),
      htmlOutput("see_table"),
      htmlOutput("regression_table"),
      
      
      # br() element to introduce extra vertical spacing ----
    #  br(),
  
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Polled Democratic Advantage", verbatimTextOutput("summary")),
                  tabPanel("Actual Democratic Advantage", tableOutput("table"))
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
    if(input$line == TRUE) {
      shiny_data %>%
        filter(position %in% input$type) %>%
        filter(state %in% state.abb[match(input$state,state.name)]) %>%
        ggplot(aes(x = predicted_rep_advantage, y = actual_rep_advantage)) +
        geom_point() +
        geom_smooth(method="lm", se=FALSE) +
        labs(x = "Predicted",
             y = "Actual",
             title = "Predicted vs. Actual Republican Advantage in U.S. 2018 Midterm Election",
            subtitle = "Calculated by dividing the difference between Republican and Democrat votes by total votes cast")
    }
    else{
      shiny_data %>%
        filter(position %in% input$type) %>% 
        filter(state %in% state.abb[match(input$state,state.name)]) %>%
        ggplot(aes(x = predicted_rep_advantage, y = actual_rep_advantage)) +
        geom_point() +
        labs(x = "Predicted",
             y = "Actual",
             title = "Predicted vs. Actual Republican Advantage in U.S. 2018 Midterm Election",
             subtitle = "Calculated by dividing the difference between Republican and Democrat votes by total votes cast")
    }
  })
   
  # # display regression table
  # output$regression_table <- renderUI({
  #   filteredData <- reactive ({
  #     df <- app_data[app_data$state %in% input$state,]
  #   })
  #   
  #   
  # # Generate a summary of the data ----
  # output$summary <- renderPrint({
  #   
  #   shiny_data %>% 
  #     select(input$data) %>% 
  #     summary()
  #   
  # })
  
  # # Generate an HTML table view of the data ----
  # output$table <- renderTable({
  #   
  #   shiny_data %>% 
  #      
  #   
  # })
  
}

# Create Shiny app ----
shinyApp(ui, server)