#
# This Shiny app shows how predictive polling error varies with population characteristics
# (within the polled population and between the polled and actual populations).
#

library(shiny)
library(tidyverse)
library(rsconnect)

# Read in the necessary data
df <- read_rds("data.rds")
df2 <- read_rds("data2.rds")

# Make a selection list
var_list <- c("Age", "Education", "Gender", "Likely", "Race")
var_list2 <- c("Female error", "White error", "Black error", "Hispanic error",
               "65+ error", "Some college or less error")

# Race lists
race_list <- c(unique(df$District))
race_list2 <- c(unique(df2$race))

# Define UI for application that draws a histogram
ui <- navbarPage("2018 House Races & Predictive Error",

  tabPanel("Polling Demographics", fluidPage(
  
  # Application title
  titlePanel("Polling Population Characteristics and Predictive Error"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput(inputId = "char", 
                  label = "Select a characteristic of the polled population:",
                  choices = var_list,
                  selected = "Age"),
    hr(),
    helpText("These charts show how the predictive error in 2018 Upshot polling tends to vary with 
             different characteristics of the polled population within House districts. Error is calculated as
             the sum of the absolute percentage-point discrepancy between Upshot wave 3 Democratic and Republican vote shares
             and actual results. Each point is a voting district; green corresponds to a race that Upshot predicted
             correctly, red incorrectly.", br(), br(), "The charts show that error tends to be higher in polled populations with 
             a higher proportion of seniors, women, and Hispanics. Lower error tends to be associated with higher proportions of 
             18-29 year-olds, college-educated individuals, White voters, and those who already voted in the polled population.", br(), br(),
             "43 House races shown:", toString(race_list))),

    # Show a plot of the generated distribution
    mainPanel(plotOutput("Plot")))
  )),
  tabPanel("Population Demographics", fluidPage(
    
    # Application title
    titlePanel("District Population Demographics and Predictive Error"),
    
    # Select the x axis variable 
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput(inputId = "char2", 
                    label = "Select a district demographic:",
                    choices = var_list2,
                    selected = "Age2"),
        hr(),
        helpText("These charts show how the predictive error in 2018 Upshot polling tends to vary with 
                 the misrepresentativeness of different House district populations. Vote share error (y axis) here is calculated as
                 the absolute percentage-point discrepancy between Upshot wave 3 Democratic and Republican vote shares (not summed)
                 and actual results. The demographic error (x axis) is the absolute percentage-point discrepancy between the sample proportion (i.e., within the polled population) and the actual population proportion.
                 Each point is a voting district; green corresponds to a race that Upshot predicted
                 correctly, red incorrectly.", br(), br(), "The charts show that predictive error tends to be higher for the Republican vote when the polled population has a greater proportion of females than the actual population,
                 and lower for the Republican vote when the polled population has a greater share of Whites and Hispanics than the actual population. Democratic vote share error tends to be higher when the polled population has a greater proportion of
                 seniors (65+).", br(), br(), "43 House races shown:", toString(race_list2),
                 br(), br(), "Demographic data from https://github.com/MEDSL/2018-elections.")),
      
      mainPanel(fluidRow(
                column(6, plotOutput("Plot2")),
                column(6, plotOutput("Plot3")))))
    )
  )
)

# Server
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
      labs(x = "Percent of Polled Population", y = "Combined Dem/Rep Polling Error")

  }, height = 1400, width = 1000)
  
  output$Plot2 <- renderPlot({
    # Choose which percent to display
    Percent <- case_when(
      input$char2 == "Female error" ~ "gender_error",
      input$char2 == "Black error" ~ "black_error",
      input$char2 == "White error" ~ "white_error",
      input$char2 == "Hispanic error" ~ "hispanic_error",
      input$char2 == "65+ error" ~ "age_error",
      input$char2 == "Some college or less error" ~ "edu_error")
    
    ggplot(df2, aes(get(Percent), d_error)) +
      geom_point(aes(col = Prediction)) +
      scale_color_manual(values=c("#009E73", "#f45642")) +
      geom_smooth(method = lm, se = FALSE) +
      labs(x = input$char2, y = "Democratic Error") +
      guides(col = FALSE)
  })
  
  output$Plot3 <- renderPlot({
    # Choose which percent to display
    Percent <- case_when(
      input$char2 == "Female error" ~ "gender_error",
      input$char2 == "Black error" ~ "black_error",
      input$char2 == "White error" ~ "white_error",
      input$char2 == "Hispanic error" ~ "hispanic_error",
      input$char2 == "65+ error" ~ "age_error",
      input$char2 == "Some college or less error" ~ "edu_error")
    
    ggplot(df2, aes(get(Percent), r_error)) +
      geom_point(aes(col = Prediction)) +
      scale_color_manual(values=c("#009E73", "#f45642")) +
      geom_smooth(method = lm, se = FALSE) +
      labs(x = input$char2, y = "Republican Error")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
