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

# Define UI ----
ui <- fluidPage(

    title = "Wine Quality",
    
    fluidRow(
      column(
        width = 4,
        tags$h1("Wine Quality")
      ),
      column(
        width = 4,
        offset = 4,
        img(src="hs-aalen-logo.png")
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

      # Einleitung ----
      tabPanel(
        "Einleitung", 
        img(src = "wine.jpg"),
        tags$p(
          "Dies ist ein Typoblindtext. An ihm kann man sehen, ob alle Buchstaben da sind und wie sie aussehen. Manchmal benutzt man Worte wie Hamburgefonts, Rafgenduks oder Handgloves, um Schriften zu testen. Manchmal Sätze, die alle Buchstaben des Alphabets enthalten - man nennt diese Sätze »Pangrams«."
        ),
        tags$p(
          "Sehr bekannt ist dieser: The quick brown fox jumps over the lazy old dog. Oft werden in Typoblindtexte auch fremdsprachige Satzteile eingebaut (AVAIL® and Wefox™ are testing aussi la Kerning), um die Wirkung in anderen Sprachen zu testen. In Lateinisch sieht zum Beispiel fast jede Schrift gut aus."
        ),
        tags$p(
          "Quod erat demonstrandum. Seit 1975 fehlen in den meisten Testtexten die Zahlen, weswegen nach TypoGb. 204 § ab dem Jahr 2034 Zahlen in 86 der Texte zur Pflicht werden. Nichteinhaltung wird mit bis zu 245 € oder 368 $ bestraft."
        ),
        tags$p(
          "Genauso wichtig in sind mittlerweile auch Âçcèñtë, die in neueren Schriften aber fast immer enthalten sind. Ein wichtiges aber schwierig zu integrierendes Feld sind OpenType-Funktionalitäten. Je nach Software und Voreinstellungen können eingebaute Kapitälchen, Kerning oder Ligaturen (sehr pfiffig) nicht richtig dargestellt werden. Dies ist ein Typoblindtext. An ihm kann man sehen, ob alle Buchstaben da sind und wie sie aussehen. Manchmal benutzt man Worte wie Hamburgefonts, Rafgenduks"
        ),
      ),
      
      # Explorative Analyse ----
      tabPanel(
        "Explorative Analyse", 
        fluidRow(
          # Box Plot----
          column(width = 6,
                 fluidRow(
                   column(width = 12,
                          selectInput(
                            inputId = 'selected_var',
                            label = 'Variable:',
                            choices = names(spec(whitewine)[["cols"]]),
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
                          plotOutput("distPlot"),
                   )),
          ),
          
          # Point Plot----
          column(width = 6,
            fluidRow(
              column(width = 12,
                  
              ),
            ),
            fluidRow(
              column(width = 12,
                plotOutput("pointPlot"),
              ),
            ),
          ),
        ),
      ),   
    
      # Statistisches Modell ----
      tabPanel(
        "Statistisches Modell", 
        ""
      ),
),
)

# Define server ----
server <- function(input, output) {

    output$distPlot <- renderPlot({
        ggplot(whitewine,aes(quality, whitewine[[input$selected_var]])) + 
        geom_boxplot() + 
        geom_hline(yintercept = mean(whitewine[[input$selected_var]]),
                   color = 'red',
                   linetype = 2,
                   ) +
        labs(x = "Quality", y = input$selected_var) +
        base_theme()
      })

    output$pointPlot <- renderPlot({
      x <- whitewine[["alcohol"]]
      y <- whitewine[["total sulfur dioxide"]]
      c <- whitewine[["quality"]]
      ggplot(whitewine,aes(x,y)) + 
        geom_point(color = c) +
        labs(x = "alcohol", y = "total sulfur dioxide") +
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