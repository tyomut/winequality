# Define libraries ----
library(shiny)
library(tidyverse)
library(ggplot2)
library(e1071)
library(caret)

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

mutate(whitewine,quality = as.character(quality))

whitewine[["quality"]] <- ordered(as.factor(whitewine[["quality"]]))

quality_levels <- levels(whitewine[["quality"]])

col_names <- spec(whitewine)[["cols"]]

var_names <- c()
for (v in names(col_names)) {
  if(!is.factor(whitewine[[v]])){
    var_names <- c(var_names,v)
  }
}

# Build Model ----

# SVM ----
partion <- createDataPartition(y = whitewine$quality, p= 0.8, list = FALSE)

data_train <- whitewine[partion,,drop=TRUE]
data_test <- whitewine[-partion,,drop=TRUE]

svm_kernel = "radial"

model_svm <- svm(data_train$quality ~ ., data = data_train, kernel = svm_kernel, cost = 10, scale = TRUE)

data_predict_svm <- predict(model_svm,data_test)

conf_matrix_svm <- confusionMatrix(data = data_predict_svm, reference = data_test$quality)

predict_svm <- function(fixed_acidity,
                        volatile_acidity,
                        citric_acid,
                        residual_sugar,
                        chlorides,
                        free_sulfur_dioxide,
                        total_sulfur_dioxide,
                        density,
                        pH,
                        sulphates,
                        alcohol){
  
  data_analyze <- tibble("fixed acidity" = fixed_acidity, 
                         "volatile acidity" = volatile_acidity,
                         "citric acid" = citric_acid,
                         "residual sugar" = residual_sugar,
                         "chlorides" = chlorides,
                         "free sulfur dioxide" = free_sulfur_dioxide,
                         "total sulfur dioxide" = total_sulfur_dioxide,
                         "density" = density,
                         "pH" = pH,
                         "sulphates" = sulphates,
                         "alcohol" = alcohol)  
  
  quality <- as.numeric(predict(model_svm,data_analyze))
  
  return(quality)
}

# Define UI ----
ui <- fluidPage(
  
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "main.css")),

    # Title & Logo ----
    title = "Wine Quality",
    
    fluidRow(
      column(width = 12,
          img(src="logo.png", height = '100px', style = "float:left;"),
          tags$div(tags$span("Wine"),class="color1 logotext line1"),
          tags$div(tags$span("Quality"),class="color2 logotext line2"),
          tags$div(tags$span("Analyzer"),class="color3 logotext line3"),
      ),
    ),
    fluidRow(
      column(
        width = 12,
        img(src="bar.png",width = "100%", height = "2px")
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
          "We offer you a tool to evaluate the quality of white wine based on physicochemical tests results for the following 11 factors:"
          
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
          "Enter your result in our ", tags$b(actionLink(inputId = "switch_predict", label = "ANALYZER")), " to see how your wine will score.", 
        ),
        tags$br(),
        tags$p(
          class = "hometext",
          "If you want to get an overview on the data, in ",tags$b(actionLink(inputId = "switch_explore", label = "DATA INSIGHTS"))," your will find more details.", tags$br(),
        ),
        tags$br(),
        tags$p(
          class = "hometext",
          "For further information on the applied ",tags$b(actionLink(inputId = "switch_model", label = "MODELS,"))," feel free to have a look at them as well."
        ),
        
      ),
      
      # Analyzer ----
      tabPanel(
        title="Analyzer", 
        value="predict",
        fluidRow(
          column(width = 2, 
                 textInput(
                   inputId = "input_fixed_acidity",
                   label = "Fixed Acidity",
                   value = 7.0,
                 ),
                 textInput(
                   inputId = "input_volatile_acidity",
                   label = "Volatile Acidity",
                   value = 0.270,
                 ),
                 textInput(
                   inputId = "input_citric_acid",
                   label = "Citric Acid",
                   value = 0.36,
                 ),
                 textInput(
                   inputId = "input_residual_sugar",
                   label = "Residual Sugar",
                   value = 20.70,
                 ),
                 textInput(
                   inputId = "input_chlorides",
                   label = "Chlorides",
                   value = 0.045,
                 ),
                 textInput(
                   inputId = "input_free_sulfur_dioxide",
                   label = "Free Sulfur Dioxide",
                   value = 45.0,
                 ),
          ),
          column(width = 2,
                 textInput(
                   inputId = "input_total_sulfur_dioxide",
                   label = "Total Sulfur Dioxide",
                   value = 0,
                 ),
                 textInput(
                   inputId = "input_density",
                   label = "Density",
                   value = 1.0010,
                 ),
                 textInput(
                   inputId = "input_pH",
                   label = "pH",
                   value = 3.00,
                 ),
                 textInput(
                   inputId = "input_sulphates",
                   label = "Sulphates",
                   value = 0.45,
                 ),
                 textInput(
                   inputId = "input_alcohol",
                   label = "Alcohol",
                   value = 8.8,
                 ),          
          ),
        ),
        fluidRow(
          column(width = 4,  align = "center",
            actionButton(
              inputId = "action_predict_quality",
              label = "Predict Quality",
              icon = icon (name = "wine-glass"),
              width = "50%",
              style = "font-size: 14pt; background-color: #92d050; border-radius: 20px;",
            ),
          ),
        ),
        fluidRow(
          column(width = 4,  align = "center",
            tags$div(
              class = "modeldetails",
              style = "margin-top: 30px",
              tags$p(
                class = "modelsubheader color1",
                "The predicted quality score:",
              ),
              textInput(
                inputId = "predicted_quality",
                label = NULL,
                value = NULL,
              ),
            ),
          ),
        ),
      ),
      
      # Data Insights ----
      tabPanel(
        title = "Data Insights", 
        value = "explore",
        
        fluidRow(
          column(width = 6, align = "center",
            selectInput(
              inputId = "selected_var",
              label = "Variable:",
              choices = var_names,
              width = '50%'
            ),
          ),
          column(width = 6, align = "center",
                 sliderInput(
                   inputId = "quality_minmax",
                   label = "Quality:",
                   value = c(as.integer(quality_levels[1]),as.integer(quality_levels[length(quality_levels)])),
                   min = as.integer(quality_levels[1]),
                   max = as.integer(quality_levels[length(quality_levels)]),
                   step = 1,
                 )
          ),
        ),

        fluidRow(
          column(width = 6, align = "center",
            tableOutput("summary_table"),                     
          ),
          column(width = 6, align = "center",
            tableOutput("quality_count_table"),                     
          ),
          
        ),
        
        fluidRow(
          column(width = 6,
            plotOutput("frequency_plot"),                 
          ),
          column(width = 6, align = "center",
            plotOutput("point_plot"),
            selectInput(
              inputId = "selected_var_pointplot",
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
          column(width = 6,
             plotOutput("quality_density_plot"),						  
          ),
          
        ),
      ),   

      # Statistical Models ----
      tabPanel(
        title = "Statistical Models", 
        value = "model",
        fluidRow(
          column(width = 4,
            fluidRow(
              column(width = 12,
                style = "line-height:1",
                tags$p(
                  class = "modelheader color1",
                  "SVM"
                ),
              ),
            ),
            fluidRow(
              column(width = 12,
                tags$div(
                  class = "modeldetails",
                  tags$p(
                    class = "modelsubheader color1",
                    "Parameters"
                  ),
                  tags$div(
                    class = "modelparams",
                    tableOutput("svm_params_table"),  
                  ),
                ),
              ),
            ),
            fluidRow(
              column(width = 12,
                tags$div(
                  class = "modeldetails",
                  tags$p(
                    class = "modelsubheader color1",
                    "Results"
                  ),
                  tags$div(
                    class = "modelparams",
                    tableOutput("svm_results_table"),  
                  ),
                  tags$div(
                    class = "modelparams",
                    tags$b("Confusion Matrix"),
                  ),
                  tags$div(
                    class = "modelparams",
                    verbatimTextOutput("svm_confusion_table"),  
                  ),                  
                ),
              ),
            ),
        ),
        column(width = 4,
               fluidRow(
                 column(width = 12,
                        style = "line-height:1",
                        tags$p(
                          class = "modelheader color1",
                          "RF"
                        ),
                 ),
               ),
               fluidRow(
                 column(width = 12,
                        tags$div(
                          class = "modeldetails",
                          tags$p(
                            class = "modelsubheader color1",
                            "Parameters"
                          ),
                          tags$div(
                            class = "modelparams",
                            tableOutput("rf_params_table"),  
                          ),
                        ),
                 ),
               ),
               fluidRow(
                 column(width = 12,
                        tags$div(
                          class = "modeldetails",
                          tags$p(
                            class = "modelsubheader color1",
                            "Results"
                          ),
                          tags$div(
                            class = "modelparams",
                            tableOutput("rf_results_table"),  
                          ),
                          tags$div(
                            class = "modelparams",
                            tags$b("Confusion Matrix"),
                          ),
                          tags$div(
                            class = "modelparams",
                            verbatimTextOutput("rf_confusion_table"),  
                          ),                  
                        ),
                 ),
               ),
        ),        
      ),
    ),
  )
)

# Define server ----
server <- function(input, output, session) {

    #box_plot
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

    #frequency_plot
    output$frequency_plot <- renderPlot({
      q <- quantile(whitewine[[input$selected_var]], probs = c(0.25,0.50,0.75))

      ggplot(whitewine,aes(whitewine[[input$selected_var]])) + 
        geom_density(kernel="gaussian") +  
        geom_vline(xintercept = q[1], color = 'blue',linetype = 3,) +
        geom_vline(xintercept = q[2], color = 'red', linetype = 2,) +
        geom_vline(xintercept = q[3], color = 'blue',linetype = 3,) +
        annotate(geom = "text", x = q[1], y = 0, label = "25%") +
        annotate(geom = "text", x = q[2], y = 0, label = "50%") +
        annotate(geom = "text", x = q[3], y = 0, label = "75%") +
        base_theme() +
        labs(title = "Density Plot (Gaussian)",  x = input$selected_var, y="Density" )
    })
    
    #point_plot
    output$point_plot <- renderPlot({
      d <- filter(whitewine,quality %in% c(input$quality_minmax[1]:input$quality_minmax[2]))
      x <- d[[input$selected_var_pointplot]]
      y <- d[[input$selected_var]]
      c <- d[["quality"]]
      
      ggplot(d,aes(x,y)) + 
        geom_point(color = c,) +
        labs(title = "Point Plot", x = input$selected_var_pointplot, y = input$selected_var) +
        base_theme()
    })
    
    #quality_density_plot
    output$quality_density_plot <- renderPlot({
      d <- filter(whitewine,quality %in% c(input$quality_minmax[1]:input$quality_minmax[2]))
      
      ggplot(d) + 
      geom_density(aes(x = d[[input$selected_var_pointplot]], group = d[["quality"]], fill = d[["quality"]],),alpha = 0.3) +
      labs(title = "Quality Density Plot", x = input$selected_var_pointplot, y = "Density",) +
      base_theme()
    })
    
    #summary_table
    output$summary_table <- renderTable(
      align = 'c',
      whitewine %>%
      summarise("Mean" = mean(whitewine[[input$selected_var]]), 
                "Median" = median(whitewine[[input$selected_var]]),
                "Std.Dev." = sd(whitewine[[input$selected_var]]), 
                "Min" = min(whitewine[[input$selected_var]]),
                "Max" = max(whitewine[[input$selected_var]]))
    )

    #quality_count_table
    output$quality_count_table <- renderTable(
      align = 'c',
      whitewine %>%
        group_by(quality) %>%
          count(name = "total_count") %>%
            #filter(between(quality,input$quality_minmax[1],input$quality_minmax[2])) %>%
            filter(quality %in% c(input$quality_minmax[1]:input$quality_minmax[2])) %>%
              pivot_wider(names_from = quality, values_from = total_count)
    )
    
    observeEvent(input$switch_home,    {updateTabsetPanel(session, "nav_panel", "home")})
    observeEvent(input$switch_predict, {updateTabsetPanel(session, "nav_panel", "predict")})
    observeEvent(input$switch_explore, {updateTabsetPanel(session, "nav_panel", "explore")})
    observeEvent(input$switch_model,   {updateTabsetPanel(session, "nav_panel", "model")})
    
    #svm_params_table
    output$svm_params_table <- renderTable(
      align = 'c',
      tibble(kernel = svm_kernel, cost = model_svm$cost, gamma = model_svm$gamma, "Number of classes" = model_svm$nclasses),
    )
    
    #svm_results_table
    output$svm_results_table <- renderTable(
      align = 'c',
      tibble("Overall Accuracy"=conf_matrix_svm$overall[1], "Number of Sup.Vec." = model_svm$tot.nSV),
    )    

    #svm_confusion_table
    output$svm_confusion_table <- renderPrint(
      conf_matrix_svm[["table"]],
    )    

    #rf_params_table
    output$rf_params_table <- renderTable(
      align = 'c',
      tibble(kernel = svm_kernel, cost = model_svm$cost, gamma = model_svm$gamma, "Number of classes" = model_svm$nclasses),
    )
    
    #rf_results_table
    output$rf_results_table <- renderTable(
      align = 'c',
      tibble("Overall Accuracy"=conf_matrix_svm$overall[1], "Number of Sup.Vec." = model_svm$tot.nSV),
    )    
    
    #rf_confusion_table
    output$rf_confusion_table <- renderPrint(
      conf_matrix_svm[["table"]],
    )    
    
    observeEvent(input$action_predict_quality, updateTextInput(session = session, 
                                                         inputId = "predicted_quality",
                                                         value = predict_svm(input$input_fixed_acidity,
                                                                             input$input_volatile_acidity,
                                                                             input$input_citric_acid,
                                                                             input$input_residual_sugar,
                                                                             input$input_chlorides,
                                                                             input$input_free_sulfur_dioxide, 
                                                                             input$input_total_sulfur_dioxide, 
                                                                             input$input_density, 
                                                                             input$input_pH, 
                                                                             input$input_sulphates,
                                                                             input$input_alcohol)
                                                         ))
}

# Run the application ----
shinyApp(ui = ui, server = server)