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
#whitewine <- rename_with(whitewine, replace_space)
whitewine$quality = as.factor(whitewine$quality)

# Define UI ----
ui <- fluidPage(

    # Application title
    titlePanel("Wine Quality"),

    fluidRow(
      column(width = 4,
             selectInput(
               inputId = 'selected_var',
               label = 'Variable:',
               choices = names(spec(whitewine)[["cols"]]),
               width = '100%',
              )
      )),
    fluidRow(
      column(width = 4,
             align = 'center',
             tableOutput('summarytable'),               
            )),
      fluidRow(
        column(width = 4,
               plotOutput("distPlot"),
        )),
)

# Define server ----
server <- function(input, output) {

    output$distPlot <- renderPlot({
        #ggplot(whitewine,aes(quality,)) + geom_bar()
        ggplot(whitewine,aes(quality, whitewine[[input$selected_var]])) + 
        geom_boxplot() + 
        geom_hline(yintercept = mean(whitewine[[input$selected_var]]),
                   color = 'red',
                   linetype = 2,
                   ) +
        theme(axis.title.x = element_text(size = 20),
              axis.text.x = element_text(size = 12),
              axis.title.y = element_text(size = 20),
              axis.text.y = element_text(size = 12),
              plot.background = element_rect(fill = 'white'),
              panel.background = element_rect(color = 'black', fill = 'white'),
              panel.grid.major = element_line(color = 'gray'),
              ) +
        labs(x = "Quality", y = input$selected_var)
      })

    output$summarytable <- renderTable(
      align = 'c',
      whitewine %>%
      summarise("Mean" = mean(whitewine[[input$selected_var]]), 
                "Median" = median(whitewine[[input$selected_var]]),
                "STDEV" = sd(whitewine[[input$selected_var]]), 
                "Min" = min(whitewine[[input$selected_var]]),
                "Max" = max(whitewine[[input$selected_var]]))
      )
    
    
    
}

# Run the application ----
shinyApp(ui = ui, server = server)