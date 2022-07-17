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

quality_levels <- as.integer(levels(whitewine[["quality"]]))

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
      id = "nav_panel",
      widths = c(2, 10),
      well = FALSE,

      # Home ----
      tabPanel(
        title="Home", 
        value="home",
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
        tags$ul(
          class = "hometext",
          tags$li("Fixed acidity"),
          tags$li("Volatile acidity"),
          tags$li("Citric acid"),
          tags$li("Residual sugar"),
          tags$li("Chlorides"),
          tags$li("Free sulfur dioxide"),
          tags$li("Total sulfur dioxide"),
          tags$li("Density"),
          tags$li("pH"),
          tags$li("Sulphates"),
          tags$li("Alcohol"),
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
          "Enter your result in our ", tags$b(actionLink(inputId = 'switch_predict', label = "ANALYZER")), " to see how your wine will score.", 
        ),
        tags$br(),
        tags$p(
          class = "hometext",
          "If you want to get an overview on the data, in ",tags$b(actionLink(inputId = 'switch_explore', label = "DATA INSIGHTS"))," your will find more details.", tags$br(),
        ),
        tags$br(),
        tags$p(
          class = "hometext",
          "For further information on the applied ",tags$b(actionLink(inputId = 'switch_model', label = "MODELS"))," feel free to have a look at them as well."
        ),
        
      ),
      
      # Statistical Models ----
      tabPanel(
        title="Analyzer", 
        value="predict",
        ""
      ),
      
      # Data Insights ----
      tabPanel(
        title = "Data Insights", 
        value = "explore",
        
        fluidRow(
          column(width = 6,
            fluidRow(
              column(width = 6,
                selectInput(
                  inputId = 'selected_var',
                  label = 'Variable:',
                  choices = var_names,
                  width = '100%',
                ),
              ),
            ),
            )
          ),
        
          fluidRow(
            column(width = 6, align = "center",
                   tableOutput('summarytable'),                     
            ),
            column(width = 6, align = "center",
                   sliderInput(
                     inputId = "quality_minmax",
                     label = "Quality:",
                     value = c(quality_levels[1],quality_levels[length(quality_levels)]),
                     min = quality_levels[1],
                     max = quality_levels[length(quality_levels)],
                     step = 1,
                   ),
            ),
          ),
        
          fluidRow(
            column(width = 6,
                   plotOutput("frequency_plot"),                 
            ),
            column(width = 6, align = "center",
                   plotOutput("point_plot"),
                   selectInput(
                     inputId = 'selected_var_pointplot',
                     label = NULL,
                     choices = var_names,
                     selected = var_names[length(var_names)],
                     width = '50%',
                   ),
            ),
          ),
          fluidRow(
            column(width = 6,
                   plotOutput("box_plot"),						  
            ),
          ),
      ),   

      # Statistical Models ----
      tabPanel(
        title = "Statistical Models", 
        value = "model",
        ""
      ),
  ),
)

# Define server ----
server <- function(input, output, session) {

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
      data <- filter(whitewine,between(quality,input$quality_minmax[1],input$quality_minmax[2]))
      x <- data[[input$selected_var_pointplot]]
      y <- data[[input$selected_var]]
      c <- data[["quality"]]
      q <- c("4" = "red", "5" = "blue", "6" = "darkgreen")
      
      ggplot(data,aes(x,y)) + 
        geom_point(color = c) +
        #scale_color_manual(values = q) +
        labs(title = "Point Plot", x = input$selected_var_pointplot, y = input$selected_var,) +
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
    
    observeEvent(input$switch_home,    {updateTabsetPanel(session, "nav_panel", "home")})
    observeEvent(input$switch_predict, {updateTabsetPanel(session, "nav_panel", "predict")})
    observeEvent(input$switch_explore, {updateTabsetPanel(session, "nav_panel", "explore")})
    observeEvent(input$switch_model,   {updateTabsetPanel(session, "nav_panel", "model")})
    
}

# Run the application ----
shinyApp(ui = ui, server = server)