# Define libraries ----
library(shiny)
library(tidyverse)
library(ggplot2)

# Helper functions ----
replace_space <- function(txt){
  str_replace_all(txt,'\\s','_')
}

base_theme <- function() {
    theme(axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 12),
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 12),
          plot.background = element_rect(fill = 'white'),
          panel.background = element_rect(color = 'black', fill = 'white'),
          panel.grid.major = element_line(color = 'gray'),
    )
}

# Load Data ----
whitewine <- read_delim('./data/winequality-white.csv',delim=';', col_names = TRUE)
#whitewine <- rename_with(whitewine, replace_space)
whitewine$quality = as.factor(whitewine$quality)

var_names <- c()
for (v in names(spec(whitewine)[["cols"]])) {
  if(is.null(levels(whitewine[[v]]))){
    var_names <- c(var_names,v)
  }
}

# Define UI ----
ui <- fluidPage(
  
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "main.css")),

    # Title & Logo ----
    title = "Wine Quality",
    
    fluidRow(
      column(width = 12,
          img(src="logo.png", height = '100px', style = "float:left;"),
          tags$div(tags$span('Wine'),class="color1 logotext line1"),
          tags$div(tags$span('Quality'),class="color2 logotext line2"),
          tags$div(tags$span('Analyzer'),class="color3 logotext line3"),
      ),
    ),
    fluidRow(
      column(
        width = 12,
        img(src="bar.png",width = '100%', height = '2px')
      ),
    ),

    navlistPanel(
      widths = c(2, 10),
      well = FALSE,

      # Home ----
      tabPanel(
        "Home", 
        tags$p(
          class = "color1",
          style="width:100%; text-align:center; font-size:36px; font-weight: bold;",
          "Welcome to the Wine Quality Analyzer"
        ),
        tags$br(),
        tags$p(
          class = "",
          style="width:100%; text-align:justify; font-size:24px; font-weight: bold;",
          "The QUALITY of WINE matters."
        ),
        tags$br(),
        tags$p(
          class = "hometext",
          "We offer you a tool to evaluate wine quality based on physicochemical tests results for the following 11 factors:"
          
        ),
        tags$br(),
        tags$p(
          class = "hometext",
          "- Fixed acidity",  tags$br(),
          "- Volatile acidity", tags$br(),
          "- Citric acid", tags$br(),
          "- Residual sugar", tags$br(),
          "- Chlorides", tags$br(),
          "- Free sulfur dioxide", tags$br(),
          "- Total sulfur dioxide", tags$br(),
          "- Density", tags$br(),
          "- pH", tags$br(),
          "- Sulphates", tags$br(),
          "- Alcohol", tags$br(),
        ), 
        tags$br(),
        tags$p(
          class = "hometext",
          "In order to provide you a reliable prediction we have built a comprehensive machine learning model that we trained with ",tags$b("over 3,000")," observations and tested extensively.", tags$br(), 
          "We followed the standard framework for data mining (CRISP-DM) in the development of this application."
        ),
        tags$br(),
        tags$p(
          class = "hometext",
          "Enter your result in our ", tags$b("ANALYZER")," to see how your wine will score.", tags$br(),
          "If you want to get an overview on the data, in ",tags$b("DATA INSIGHTS")," your will find more details.", tags$br(),
          "For further information on the applied ",tags$b("MODELS")," feel free to have a look at them as well."
        ),
        
      ),
      
      # Statistical Models ----
      tabPanel(
        "Analyzer", 
        ""
      ),
      
      # Data Insights ----
      tabPanel(
        "Data Insights", 
        fluidRow(
          # Box Plot
          column(width = 6,
                 fluidRow(
                   column(width = 12,
                          selectInput(
                            inputId = 'selected_var',
                            label = 'Variable:',
                            choices = var_names,
                            width = '100%',
                          )
                   )),
                 fluidRow(
                   column(width = 12,
                          align = 'center',
                          tableOutput('summarytable'),               
                   )),
                 fluidRow(
                   column(width = 12,
                          plotOutput("frequency_plot"),
                   )),
                 fluidRow(
                   column(width = 12,
                          plotOutput("box_plot"),
                   )),                 
          ),
          
          # Point Plot
          column(width = 6,
            fluidRow(
              column(width = 12,
                  
              ),
            ),
            fluidRow(
              column(width = 12, align = 'center',     
                plotOutput("point_plot"),
                selectInput(
                  inputId = 'selected_var_pointplot',
                  label = NULL,
                  choices = var_names,
                  selected = var_names[length(var_names)],
                  width = '50%',
                )
                
              ),
            ),
          ),
        ),
      ),   
    
      # Statistical Models ----
      tabPanel(
        "Statistical Models", 
        ""
      ),
),
)



# Define server ----
server <- function(input, output) {

    output$box_plot <- renderPlot({
        ggplot(whitewine,aes(quality, whitewine[[input$selected_var]])) + 
        geom_boxplot() + 
        geom_hline(yintercept = mean(whitewine[[input$selected_var]]),
                   color = 'red',
                   linetype = 2,
                   ) +
        labs(title = "Box Plot", x = "Quality", y = input$selected_var) +
        base_theme()
      })

    output$frequency_plot <- renderPlot({
      q <- quantile(whitewine[[input$selected_var]], probs = c(0.25,0.50,0.75))

      ggplot(whitewine,aes(whitewine[[input$selected_var]])) + 
        geom_freqpoly() +  
        geom_vline(xintercept = q[1], color = 'blue',linetype = 3,) +
        geom_vline(xintercept = q[2], color = 'red', linetype = 2,) +
        geom_vline(xintercept = q[3], color = 'blue',linetype = 3,) +
        annotate(geom = "text", x = q[1], y = 0, label = "25%") +
        annotate(geom = "text", x = q[2], y = 0, label = "50%") +
        annotate(geom = "text", x = q[3], y = 0, label = "75%") +
        base_theme() +
        labs(title = "Frequency Plot",  x = input$selected_var, y="count" )
    })
    
    
    output$point_plot <- renderPlot({
      x <- whitewine[[input$selected_var_pointplot]]
      y <- whitewine[[input$selected_var]]
      c <- whitewine[["quality"]]
      
      ggplot(whitewine,aes(x,y)) + 
        geom_point(color = c) +
        labs(x = input$selected_var_pointplot, y = input$selected_var) +
        base_theme() 
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