# Define libraries ----
library(shiny)
library(tidyverse)
library(ggplot2)

# Helper functions ----
replace_space <- function(txt){
  str_replace_all(txt,'\\s','_')
}


# Load Data ----
whitewine <- read_delim('./data/winequality-white.csv',delim=';', col_names = TRUE)
whitewine <- rename_with(whitewine, replace_space)
whitewine$quality = as.factor(whitewine$quality)

# Define UI ----
ui <- fluidPage(

    # Application title
    titlePanel("Wine Quality"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = 'selected_var',
            label = 'Variable:',
            choices = names(spec(whitewine)[["cols"]]),
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server ----
server <- function(input, output) {

    output$distPlot <- renderPlot({
        #ggplot(whitewine,aes(quality,)) + geom_bar()
        ggplot(whitewine,aes(quality, whitewine[[input$selected_var]] )) + 
        geom_boxplot() + 
        labs(title = input$selected_var, x = "Quality", y = input$selected_var)
      })
}

# Run the application ----
shinyApp(ui = ui, server = server)