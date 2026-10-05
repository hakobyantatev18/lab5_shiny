# helper functions
get_categories <- function() {
  res <- jsonlite::fromJSON(
    "https://api.nobelprize.org/2.1/nobelPrizes?limit=1000"
  )
  sort(unique(res$nobelPrizes$category$en))
}

get_categories()

get_years <- function(){
  res <- jsonlite::fromJSON(
    "https://api.nobelprize.org/2.1/nobelPrizes?limit=1000"
  )
  sort(unique(res$nobelPrizes$awardYear))
}
categories <- c(get_categories())
years <- as.integer(c(get_years()))


# UI

ui <- fluidPage(
  titlePanel("Nobel Prize laureates"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Category", choices = categories),
      conditionalPanel(
        condition = "input.tabs == 'Laureates by year'",
        sliderInput("year", "Year", min = min(years), max = max(years),
                    value = max(years), sep = "", step = 1)
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Laureates by year", tableOutput("laureates")),
                  tabPanel("Age over time", plotOutput("age_plot")),
                  tabPanel("Map of the birth countries", plotOutput("map_of_countries")),
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  laureates <- reactive({
    get_laureates(
      category = input$category,
      year = input$year
    )
  })
  
  output$laureates <- renderTable({
    
    res <- laureates()
    
    validate(
      need(
        nrow(res) > 0,
        "No prize awarded for this category and year."
      )
    )
    res
  })
  
  output$age_plot <- renderPlot({
    
    res <- laureates()
    plot_age_over_time(res)
  })
  
  output$map_of_countries <- renderPlot({
    
    res <- laureates()
    plot_birth_countries(res)
  })
}

# Shiny app
shinyApp(ui = ui, server = server)


