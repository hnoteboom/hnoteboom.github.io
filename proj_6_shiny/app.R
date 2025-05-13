library(shiny)
library(tidyverse)

# Define UI
ui <- fluidPage(
  titlePanel("Species Categories by National Park"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_park",
        label = "Choose a National Park:",
        choices = NULL,  # Start with an empty list
        selected = "ROMO"
      ), 
      radioButtons(
        inputId = "plot_type",
        label = "Select plot type:",
        choices = c("Bar Chart" = "bar", "Pie Chart" = "pie"),
        selected = "bar"
      ),
      selectInput(
        inputId = "exclude",
        label = "Exclude Species Categories:",
        choices = NULL,  # Start with an empty list
        multiple = TRUE,
        selectize = TRUE
      ), 
      sliderInput(
        inputId = "top_n",
        label = "Number of species categories to show:",
        min = 1,
        max = 10,
        value = 5
      )
    ),
    
    mainPanel(
      plotOutput("species_plot")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Load the data and park names reactively
  most_visited_nps_species_data <- reactive({
    readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-10-08/most_visited_nps_species_data.csv', show_col_types = FALSE)
  })
  
  # Get park choices reactively
  park_choices <- reactive({
    most_visited_nps_species_data() |> 
      select(ParkName, ParkCode) |> 
      distinct() |> 
      arrange(ParkName)
  })
  
  # Update the dropdown for parks dynamically
  observe({
    updateSelectInput(session, "selected_park", choices = setNames(park_choices()$ParkCode, park_choices()$ParkName), selected = "ROMO")
  })
  
  # Update the exclude categories dynamically
  observe({
    updateSelectInput(session, "exclude", choices = unique(most_visited_nps_species_data()$CategoryName))
  })
  
  # Filtering and creating the plot
  output$species_plot <- renderPlot({
    filtered_data <- most_visited_nps_species_data() |>
      filter(ParkCode == input$selected_park) |>
      filter(!CategoryName %in% input$exclude) |>
      group_by(CategoryName) |>
      summarize(count = n()) |>
      arrange(desc(count))|>
      head(input$top_n)
    
    park_name <- park_choices() |>
      filter(ParkCode == input$selected_park) |>
      pull(ParkName)
    
    if (input$plot_type == "bar") {
      ggplot(filtered_data, aes(x = reorder(CategoryName, -count), y = count, fill = CategoryName)) +
        geom_col() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        labs(
          x = "Species Category",
          y = "Number of Species",
          title = paste("Bar Graph of Species Categories in", park_name),
          fill = "Category"
        )
    } else if (input$plot_type == "pie") {
      ggplot(filtered_data, aes(x = "", y = count, fill = CategoryName)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y") +
        theme_void() +
        labs(
          title = paste("Pie Chart of Species Categories in", park_name),
          fill = "Category"
        )
    }
  })
}

# Run the App
shinyApp(ui = ui, server = server)
