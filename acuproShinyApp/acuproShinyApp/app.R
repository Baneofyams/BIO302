library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
source(here::here('R/sim_dat.R'))
source(here::here('R/import_data.R'))
set.seed(13)

rain <- c(4000, 2750, 2125, 1900, 1650)
masl <- c(75, 425, 810)

ui <- page_sidebar(
  title = "Hardanger aculeata data exploration",
  
  sidebar = sidebar(
    
    # Controls for plot 1 and 2
    radioButtons(
      "gradient1",
      "Gradient (Plot 1 & 2)",
      choices = c("Precipitation" = "precipitation",
                  "Elevation" = "elevation")
    ),
    
    checkboxGroupInput(
      "traits",
      "Traits (Plot 1)",
      choices = c("trait1", "trait2", "trait3"),
      selected = "trait1"
    ),
    
    hr(),
    
    # Controls for plot 3
    selectInput(
      "metric",
      "Select trait or diversity",
      choices = c("Traits" = "traits",
                  "Taxonomic Diversity" = "diversity")
    ),
    
    radioButtons(
      "gradient3",
      "Gradient value",
      choices = 'Loading...'
    )
  ),
  
  layout_column_wrap(
    width = 1/1,
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3")
  )
)


server <- function(input, output, session) {
  
  # Example placeholder dataset
  df <- reactive({
    # Replace with your real data
    gradient_data(rain, masl, spp)
  })
  
  observe({
    updateRadioButtons(
      session, "gradient3",
      choices = c(df()$precipitation, df()$elevation) |> unique()
    )
  })
  
  
  # ---- Plot 1: Relative abundance of traits ----
  output$plot1 <- renderPlot({
    req(input$traits)
    
    data <- df()
    gradient <- input$gradient1
    
    data_long <- data %>%
      select(all_of(c(gradient, input$traits))) %>%
      pivot_longer(cols = all_of(input$traits),
                          names_to = "trait",
                          values_to = "value")
    
    summary_data <- data_long %>%
      group_by(.data[[gradient]], trait) %>%
      summarise(rel_abundance = mean(value, na.rm = TRUE),
                .groups = "drop")
    
    ggplot(summary_data,
           aes(x = .data[[gradient]],
               y = rel_abundance,
               color = trait)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      labs(
        x = gradient,
        y = "Relative Abundance",
        color = "Trait"
      ) +
      theme_minimal()
  })
  
  # ---- Plot 2: Taxonomic diversity ----
  output$plot2 <- renderPlot({
    
    data <- df()
    gradient <- input$gradient1
    
    summary_data <- data %>%
      group_by(.data[[gradient]]) %>%
      summarise(diversity = n_distinct(species),
                .groups = "drop")
    
    ggplot(summary_data,
           aes(x = .data[[gradient]],
               y = diversity)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      labs(
        x = gradient,
        y = "Taxonomic Diversity"
      ) +
      theme_minimal()
  })
  
  # ---- Plot 3: Percentage plot ----
  output$plot3 <- renderPlot({
    
    data <- df()
    
    # Determine gradient column
    filtered <- data %>%
      filter(
        precipitation == input$gradient3 |
          elevation == input$gradient3
      )
    
    # Extract selected level
    selected_level <- input$gradient3
    
    if (input$metric == "traits") {
      
      trait_cols <- c("trait1", "trait2", "trait3")  # update
      
      summary_data <- filtered %>%
        summarise(across(all_of(trait_cols), ~ mean(.x, na.rm = TRUE))) %>%
        tidyr::pivot_longer(everything(),
                            names_to = "trait",
                            values_to = "percentage")
      
      ggplot(summary_data,
             aes(x = trait, y = percentage)) +
        geom_col(fill = "darkgreen") +
        labs(
          x = "Trait",
          y = "Percentage"
        ) +
        theme_minimal()
      
    } else {
      
      diversity <- filtered %>%
        summarise(div = n_distinct(species)) %>%
        pull(div)
      
      df_plot <- data.frame(
        metric = "Diversity",
        value = diversity
      )
      
      ggplot(df_plot,
             aes(x = metric, y = value)) +
        geom_col(fill = "purple") +
        labs(y = "Taxonomic Diversity") +
        theme_minimal()
    }
  })
}

# ---- RUN APP ----
shinyApp(ui, server)