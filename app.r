library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)

# File paths mapped by class
file_map <- list(
  "Class 3" = list(
    chennai = "data/item_analysis_class_3_fixed_subjects.csv",
    cuddalore = "data/item_analysis_cuddalore_class_3_fixed_subjects.csv"
  ),
  "Class 5" = list(
    chennai = "data/item_analysis_class_5_fixed_subjects.csv",
    cuddalore = "data/item_analysis_cuddalore_class_5_fixed_subjects.csv"
  ),
  "Class 8" = list(
    chennai = "data/item_analysis_class_8_fixed_subjects.csv",
    cuddalore = "data/item_analysis_cuddalore_class_8_fixed_subjects.csv"
  )
)

ui <- fluidPage(
  titlePanel("SLAS 2025 - Item Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput("class_level", "Select Class:", choices = names(file_map), selected = "Class 3")
    ),

    mainPanel(
      h4("Difficulty vs Discrimination - Chennai"),
      plotlyOutput("chennai_plot"),
      
      h4("Difficulty vs Discrimination - Cuddalore"),
      plotlyOutput("cuddalore_plot"),
      
      h4("Item Count by Subject - Chennai"),
      plotlyOutput("chennai_bar"),
      
      h4("Item Count by Subject - Cuddalore"),
      plotlyOutput("cuddalore_bar")
    )
  )
)

server <- function(input, output) {
  
  load_data <- reactive({
    class_selected <- input$class_level
    list(
      chennai = read_csv(file_map[[class_selected]]$chennai),
      cuddalore = read_csv(file_map[[class_selected]]$cuddalore)
    )
  })

  plot_scatter <- function(df, title) {
    ggplot(df, aes(x = mean, y = r, color = subject,
                   text = paste("LO:", learning_outcome,
                                "<br>Mean:", round(mean, 2),
                                "<br>r:", round(r, 2)))) +
      geom_point(size = 2, alpha = 0.8) +
      geom_hline(yintercept = 0.3, linetype = "dashed", color = "gray40") +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray40") +
      labs(title = title, x = "Item Difficulty", y = "Item Discrimination", color = "Subject") +
      theme_minimal()
  }

  plot_bar <- function(df, title) {
    df %>%
      count(subject) %>%
      ggplot(aes(x = subject, y = n, fill = subject)) +
      geom_bar(stat = "identity") +
      labs(title = title, x = "Subject", y = "Number of Items") +
      theme_minimal()
  }

  output$chennai_plot <- renderPlotly({
    df <- load_data()$chennai
    ggplotly(plot_scatter(df, "Chennai: Difficulty vs Discrimination"), tooltip = "text")
  })

  output$cuddalore_plot <- renderPlotly({
    df <- load_data()$cuddalore
    ggplotly(plot_scatter(df, "Cuddalore: Difficulty vs Discrimination"), tooltip = "text")
  })

  output$chennai_bar <- renderPlotly({
    df <- load_data()$chennai
    ggplotly(plot_bar(df, "Chennai: Item Count by Subject"))
  })

  output$cuddalore_bar <- renderPlotly({
    df <- load_data()$cuddalore
    ggplotly(plot_bar(df, "Cuddalore: Item Count by Subject"))
  })
}

shinyApp(ui = ui, server = server)
