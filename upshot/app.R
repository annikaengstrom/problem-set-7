library(shiny)
library(tidyverse)
library(rsconnect)

# Read in the necessary data
df <- read_rds("data.rds")
# Make a selection list
var_list <- c("Age", "Education", "Gender", "Likely", "Race")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("House Polling Population Characteristics and Upshot Error"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput(inputId = "char", 
                  label = "Select a characteristic:",
                  choices = var_list,
                  selected = "Age"),
    hr(),
    helpText("These charts show how the error in Upshot polling tends to vary with 
             different population characteristics within House districts. Error is calculated as
             the sum of the absolute discrepancy between Wave 3 Upshot Democratic and Republican vote shares
             and actual results in 49 House races. Green corresponds to a race that Upshot predicted
             correctly, red incorrectly."),
    fluidRow(column(width = 2, c("Races displayed:", unique(df$District))))),

    # Show a plot of the generated distribution
    mainPanel(plotOutput("Plot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({

    # Choose which percent to display
    Percent <- case_when(
      input$char == "Age" ~ "p_age",
      input$char == "Education" ~ "p_educ",
      input$char == "Gender" ~ "p_gender",
      input$char == "Likely" ~ "p_likely",
      input$char == "Race" ~ "p_ethn")
    
    ggplot(df, aes(get(Percent), Error, group = get(input$char))) +
      geom_point(aes(col = Prediction)) +
      scale_color_manual(values=c("#009E73", "#f45642")) +
      geom_smooth(method = lm, se = FALSE) +
      facet_wrap(~ get(input$char), ncol = 1) +
      labs(x = "Percent")

  }, height = 1400, width = 1000)
}

# Run the application 
shinyApp(ui = ui, server = server)
