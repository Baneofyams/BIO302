library(shiny)
library(bslib)
library(tidyverse)
source(here::here('R/sim_dat.R'))
set.seed(13)

# ---- SIMULATED DATA FUNCTION ----


# Example inputs


# ---- UI ----
ui <- page_sidebar(
  title = "Ecology Dashboard",
  
  sidebar = sidebar(
    
    radioButtons(
      "gradient1",
      "Gradient (Plot 1 & 2)",
      choices = c("Precipitation", "Elevation")
    ),
    
    checkboxGroupInput(
      "traits",
      "Traits",
      choices = c("trait1", "trait2", "trait3"),
      selected = "trait1"
    ),
    
    hr(),
    
    selectInput(
      "metric",
      "Plot 3 Metric",
      choices = c("Traits", "Diversity")
    ),
    
    selectInput(
      "gradient3",
      "Gradient Value (Plot 3)",
      choices = NULL
    )
  ),
  
  layout_column_wrap(
    width = 1/1,
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3")
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  df <- reactive({
    gradient_data(rain, masl, spp)
  })
  
  # ---- Update gradient3 choices ----
  observeEvent(df(), {
    updateSelectInput(
      session, "gradient3",
      choices = sort(unique(c(df()$precipitation, df()$elevation)))
    )
  })
  
  # Helper: choose gradient column safely
  get_gradient <- reactive({
    req(input$gradient1)
    
    if (input$gradient1 == "Precipitation") {
      "precipitation"
    } else {
      "elevation"
    }
  })
  
  # ---- Plot 1 ----
  output$plot1 <- renderPlot({
    req(input$traits)
    
    data <- df()
    gradient <- get_gradient()
    
    data_long <- data %>%
      pivot_longer(cols = all_of(input$traits),
                   names_to = "trait",
                   values_to = "value")
    
    summary_data <- data_long %>%
      group_by(.data[[gradient]], trait) %>%
      summarise(rel_abundance = mean(value), .groups = "drop")
    
    ggplot(summary_data,
           aes(x = .data[[gradient]],
               y = rel_abundance,
               color = trait)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", se = TRUE) +
      theme_minimal()
  })
  
  # ---- Plot 2 ----
  output$plot2 <- renderPlot({
    
    data <- df()
    gradient <- get_gradient()
    
    summary_data <- data %>%
      group_by(.data[[gradient]]) %>%
      summarise(diversity = n_distinct(species), .groups = "drop")
    
    ggplot(summary_data,
           aes(x = .data[[gradient]], y = diversity)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", se = TRUE) +
      theme_minimal()
  })
  
  # ---- Plot 3 ----
  output$plot3 <- renderPlot({
    
    req(input$gradient3)
    
    data <- df()
    
    filtered <- data %>%
      filter(precipitation == input$gradient3 |
               elevation == input$gradient3)
    
    req(nrow(filtered) > 0)
    
    if (input$metric == "Traits") {
      
      summary_data <- filtered %>%
        summarise(across(starts_with("trait"), mean)) %>%
        pivot_longer(everything(),
                     names_to = "trait",
                     values_to = "percentage")
      
      ggplot(summary_data,
             aes(x = trait, y = percentage)) +
        geom_col() +
        theme_minimal()
      
    } else {
      
      diversity <- n_distinct(filtered$species)
      
      ggplot(data.frame(x = "Diversity", y = diversity),
             aes(x = x, y = y)) +
        geom_col() +
        theme_minimal()
    }
  })
}

shinyApp(ui, server)