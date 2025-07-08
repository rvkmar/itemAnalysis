library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)

ui <- fluidPage(
  titlePanel("SLAS 2025 - Comparing Chennai and Cuddalore districts - Item Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv")
    ),
    
    mainPanel(
      h4("Summary Statistics by Subject"),
      tableOutput("summary_table"),
      
      h4("Difficulty vs Discrimination Plot"),
      plotlyOutput("item_plot")
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  output$summary_table <- renderTable({
    df <- data()
    df %>%
      group_by(subject) %>%
      summarise(
        n_items = n(),
        avg_difficulty = mean(mean, na.rm = TRUE),
        avg_discrimination = mean(r, na.rm = TRUE),
        sd_difficulty = sd(mean, na.rm = TRUE),
        sd_discrimination = sd(r, na.rm = TRUE)
      )
  })
  
  output$item_plot <- renderPlotly({
    df <- data()
    p <- ggplot(df, aes(x = mean, y = r, color = subject,
                        text = paste("LO:", learning_outcome,
                                     "<br>Mean:", round(mean, 2),
                                     "<br>r:", round(r, 2)))) +
      geom_point(size = 2, alpha = 0.8) +
      geom_hline(yintercept = 0.3, linetype = "dashed", color = "gray40") +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray40") +
      labs(
        title = "Item Analysis: Difficulty vs Discrimination",
        x = "Item Difficulty (Proportion Correct)",
        y = "Item Discrimination (r)",
        color = "Subject"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)
