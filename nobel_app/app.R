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
        condition = "input.tabs == 'Laureates by category and year'",
        checkboxInput("use_year", "Filter by year", value = FALSE),
        conditionalPanel(
          condition = "input.use_year",
          sliderInput("year", "Year", min = min(years), max = max(years),
                      value = 2019, sep = "", step = 1)
        )
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Laureates by category and year", tableOutput("laureates")),
                  tabPanel("Age over time", plotOutput("age_plot")),
                  tabPanel("Map of the birth countries", plotOutput("map_of_countries")),
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  laureates <- reactive({
    year <- if (isTRUE(input$use_year)) input$year else NULL
    get_laureates(category = input$category, year = year)
  })
  
  laureates_all_years <- reactive({
    get_laureates(category = input$category, year = NULL)
  })
  
  output$laureates <- renderTable({
    res <- laureates()
    validate(need(nrow(res) > 0, "No laureates found for this selection."))
    res
  })
  
  output$age_plot <- renderPlot({
    plot_age_over_time(laureates_all_years())
  })
  
  output$map_of_countries <- renderPlot({
    plot_birth_countries(laureates_all_years())
  })
}

# Shiny app
shinyApp(ui = ui, server = server)

# shiny::runApp("nobel_app")
# Sys.setenv(TAR = "/usr/bin/tar")
# shiny::runGitHub("lab5_shiny", "hakobyantatev18", subdir = "nobel_app")