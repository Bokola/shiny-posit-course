
# week 1 ------------------------------------------------------------------



# Anatomy of an app: UI and server

library(shiny)
library(bslib) # for modern UI toolkit for shiny
library(tidyverse)


d <- readr::read_csv(file.path(here::here(), "data", "weather.csv"))
d <- d  |> mutate(
  year = lubridate::year(date))

# selecting a variable to plot 
d_vars <- c(
  "Average temp" = "temp_avg",
  "Min temp" = "temp_min",
  "Max temp" = "temp_max",
  "Total precip" = "precip",
  "Snow depth" = "snow",
  "Wind direction" = "wind_direction",
  "Wind speed" = "wind_speed",
  "Air pressure" = "air_press"
)


ui <- page_sidebar(
  title = "title",
  sidebar = "sidebar",
  "main content"
)

server <- function(input, output, session){
  
}

# The sidebar and main content area can hold any collection of UI elements.
# it's a good practice to keep inputs in a sidebar and outputs in the main
# content area
# Consider wrapping outputs in a card() and sidebar contents in a sidebar()



# shinyApp(ui, server)

# first shiny app

# page layouts organize UI components
# UI consits of title, sidebar & other UI elements in body
# sidebar layout puts componets side by side

# inputs: textInput(), numericInput(), sliderInput(), radioButtons(), 
# checkboxGroupInput(), selectInput() - for dropdown

# browseURL(' https://shiny.posit.co/r/gallery/widgets/widget-gallery/')

# outputs: plotOutput(), imageOutput(), tableOutput(), textOutput(),
# verbatimTextOutput(), htmlOutput(), uiOutput()

# output elements are created as part of app's UI. Act as placeholders for the 
# dynamic content displayed in response to an input



ui <- page_sidebar(
  title = "Temperature Forecasts"
  ,sidebar = sidebar(
    radioButtons(
      inputId = "name"
      ,label = "Select an airport"
      ,choices = c(
        "Raleigh-Durham",
        "Houston Intercontinental",
        "Denver",
        "Los Angeles",
        "John F. Kennedy"
      )
    )
  )
  ,plotOutput("plot")
)


# Reactive expressions: server function defines reative expressions
# common approach to create reactive functions is by render* functions

server <- function(input, output, session){
  output$plot <- renderPlot({
    d |> 
      filter(name %in% input$name) |>
      ggplot(aes(x = date, y = temp_avg, color = name)) +
      geom_line()
  })
}

# shinyApp(ui, server)


# Lesson 2 - Basic reactivity ---------------------------------------------


ui <- page_sidebar(
  title = "Weather Forecasts",
  sidebar = sidebar(
    radioButtons(
      "name", "Select an airport",
      choices = c(
        "Raleigh-Durham",
        "Houston Intercontinental",
        "Denver",
        "Los Angeles",
        "John F. Kennedy"
      )
    ),
    selectInput(
      "var", "Select a variable",
      choices = d_vars, selected = "temp_avg"
    )
  ),
  plotOutput("plot")
  ,tableOutput("minmax")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    d |>
      filter(name %in% input$name) |>
      # rlang way to reference columns in a dataframe
      ggplot(aes(x = date, y = .data[[input$var]], color = name)) +
      geom_line()
  })
  output$minmax <- renderTable({
      d |> filter(name %in% input$name) |> mutate(
      year = lubridate::year(date)) |>
        summarize(
          `min temp` = min(temp_min),
          `max temp` = max(temp_max),
          .by = year
        )
  })
}

shinyApp(ui = ui, server = server)

# Exwecise 1 - layout_columns()

# layout_columns() - create one or more rows of UI elements that are positioned
# based on a grid system - 12 column layout with each column equal to 1/12 of 
# total width

# example: a row of two colums - layout_columns(col_widths = c(6,6)).
# Negative values are treated as empty columns

# selecting a variable to plot 

ui <- page_sidebar(
  title = "Weather Forecasts",
  sidebar = sidebar(
    radioButtons(
      "name", "Select an airport",
      choices = c(
        "Raleigh-Durham",
        "Houston Intercontinental",
        "Denver",
        "Los Angeles",
        "John F. Kennedy"
      )
    ),
    selectInput(
      "var", "Select a variable",
      choices = d_vars, selected = "temp_avg"
    )
  ),
  plotOutput("plot"),
  layout_columns(
    tableOutput("minmax"), col_widths = c(-4,4,-4)
  )

)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    d |>
      filter(name %in% input$name) |>
      # rlang way to reference columns in a dataframe
      ggplot(aes(x = date, y = .data[[input$var]], color = name)) +
      geom_line()
  })
  output$minmax <- renderTable({
    d |> filter(name %in% input$name) |> mutate(
      year = lubridate::year(date)) |>
      summarize(
        `min temp` = min(temp_min),
        `max temp` = max(temp_max),
        .by = year
      )
  })
}

shinyApp(ui = ui, server = server)

# Exercise 2 -  Tabled interfaces with navset_card_tab()



ui <- page_sidebar(
  title = "Weather Forecasts",
  sidebar = sidebar(
    radioButtons(
      "name", "Select an airport",
      choices = c(
        "Raleigh-Durham",
        "Houston Intercontinental",
        "Denver",
        "Los Angeles",
        "John F. Kennedy"
      )
    ),
    selectInput(
      "var", "Select a variable",
      choices = d_vars, selected = "temp_avg"
    )
  ),
  # navset_card_tab()
  navset_card_tab(
    # nav_panel() takes id
    nav_panel(title = "plot",
      plotOutput("plot")
    ),
    nav_panel(title = "table",
      tableOutput("minmax")
    ),
    nav_panel(
      title = "summary",
      verbatimTextOutput("summ")
    )
  )
  
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    d |>
      filter(name %in% input$name) |>
      # rlang way to reference columns in a dataframe
      ggplot(aes(x = date, y = .data[[input$var]], color = name)) +
      geom_line()
  })
  output$minmax <- renderTable({
    d |> filter(name %in% input$name) |> 
      summarize(
        `min temp` = min(temp_min),
        `max temp` = max(temp_max),
        .by = year
      )
  })
  
  output$summ <- renderText({
    paste0("The currently selected airport is ", input$name, "\n",
  "The currently selected variable is ", input$var, "\n")
  })
  
}

shinyApp(ui = ui, server = server)

# Exercise 3 - page_navbar() in place of page_sidebar


ui <- page_navbar(
  title = "Weather Forecasts",
  sidebar = sidebar(
    radioButtons(
      "name", "Select an airport",
      choices = c(
        "Raleigh-Durham",
        "Houston Intercontinental",
        "Denver",
        "Los Angeles",
        "John F. Kennedy"
      )
    ),
    selectInput(
      "var", "Select a variable",
      choices = d_vars, selected = "temp_avg"
    )
  ),
    nav_spacer(),
  
    nav_panel(title = "plot",
              plotOutput("plot"),
              icon = bsicons::bs_icon("graph-up")
    ),
    nav_panel(title = "table",
              tableOutput("minmax"),
              icon = bsicons::bs_icon("table")
    ),
    nav_panel(
      title = "summary",
      verbatimTextOutput("summ"),
      icon = bsicons::bs_icon("text-center")
    )
  
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    d |>
      filter(name %in% input$name) |>
      # rlang way to reference columns in a dataframe
      ggplot(aes(x = date, y = .data[[input$var]], color = name)) +
      geom_line()
  })
  output$minmax <- renderTable({
    d |> filter(name %in% input$name) |> 
      summarize(
        `min temp` = min(temp_min),
        `max temp` = max(temp_max),
        .by = year
      )
  })
  
  output$summ <- renderText({
    paste0("The currently selected airport is ", input$name, "\n", "\n",
           "The currently selected variable is ", input$var, "\n")
  })
  
}

shinyApp(ui = ui, server = server)



# Week 2 - Reactivity -----------------------------------------------------

# Reactivity helps avoid repetitive code

# reactive({}) - return a value just like a function
# observe({}) - used for its side effect - sending data to client browser,
# writing to a file, printing a message etc.


ui <- page_sidebar(
  title = "Weather Forecasts",
  sidebar = sidebar(
    radioButtons(
      "name", "Select an airport",
      choices = c(
        "Raleigh-Durham",
        "Houston Intercontinental",
        "Denver",
        "Los Angeles",
        "John F. Kennedy"
      )
    ),
    selectInput(
      "var", "Select a variable",
      choices = d_vars, selected = "temp_avg"
    )
  ),
  plotOutput("plot"),
  tableOutput("minmax")
)

server <- function(input, output, session) {
  d_city <- reactive({
    d |>
      filter(name %in% input$name)
  })
  
  output$plot <- renderPlot({
    d_city() |>
      ggplot(aes(x = date, y = .data[[input$var]])) +
      ggtitle(names(d_vars)[d_vars == input$var]) +
      geom_line()
  })
  
  output$minmax <- renderTable({
    d_city() |>
      mutate(
        year = lubridate::year(date) |> as.integer()
      ) |>
      summarize(
        `min temp` = min(temp_min),
        `max temp` = max(temp_max),
        .by = year
      )
  })
}

shinyApp(ui = ui, server = server)

## filter by region and use updateSelectInput() to select airports in selected 
# region

ui <- page_sidebar(
  title = "Weather forecasts"
  ,sidebar = sidebar(
    selectInput(
      "region", "Select a region",
      choices = c("West", "Midwest", "Northeast", "South")
    )
    ,selectInput(
      "name", "Select an airport"
      # ,choices = c("Los Angeles")
      ,choices = 
    )
    ,selectInput(
      "var", "Select a variable",
      choices = d_vars, selected = "temp_avg"
    )
  )
  , plotOutput("plot")
  ,tableOutput("minmax")
)

server <- function(input, output, session){
  observe({
    updateSelectInput(
      inputId = "name"
      ,choices = d |>
        distinct(region, name) |>
        filter(region == input$region) |>
        pull(name)
    )
  })
  
  d_city <- reactive({
    d |> 
      filter(name %in% input$name)
  })
  
  output$plot <- renderPlot({
    d_city() |>
      ggplot(aes(x = date, y = .data[[input$var]], color = name)) +
      ggtitle(names(d_vars)[d_vars == input$var]) +
      geom_line()
      
  })
  
  output$minmax <- renderTable({
    d_city() |>
      summarize(
        `min temp` = min(temp_min),
        `max temp` = max(temp_max),
        .by = year
      )
  })
}

shinyApp(ui, server)

## using req()

# req() checks if a value is truthy i.e., not null - 
# think of it as bullet-proof against bad inputs

ui <- page_sidebar(
  title = "Weather forecasts"
  ,sidebar = sidebar(
    selectInput(
      "region", "Select a region",
      choices = c("West", "Midwest", "Northeast", "South")
    )
    ,selectInput(
      "name", "Select an airport"
      # ,choices = c("Los Angeles")
      ,choices = c()
    )
    ,selectInput(
      "var", "Select a variable",
      choices = d_vars, selected = "temp_avg"
    )
  )
  , plotOutput("plot")
  ,tableOutput("minmax")
)

server <- function(input, output, session){
  observe({
    updateSelectInput(
      inputId = "name"
      ,choices = d |>
        distinct(region, name) |>
        filter(region == input$region) |>
        pull(name)
    )
  })
  
  d_city <- reactive({
    d |> 
      filter(name %in% input$name)
  })
  
  output$plot <- renderPlot({
    # require name
    req(input$name)
    d_city() |>
      ggplot(aes(x = date, y = .data[[input$var]], color = name)) +
      ggtitle(names(d_vars)[d_vars == input$var]) +
      geom_line()
    
  })
  
  output$minmax <- renderTable({
    d_city() |>
      summarize(
        `min temp` = min(temp_min),
        `max temp` = max(temp_max),
        .by = year
      )
  })
}

shinyApp(ui, server)

# Using bindEvent() to control reactive curve

ui <- page_sidebar(
  title = "Weather forecasts"
  ,sidebar = sidebar(
    selectInput(
      "region", "Select a region",
      choices = c(
        "West",
        "Midwest",
        "Northeast",
        "South"
      )
    )
    ,selectInput(
      "name", "Select an airport",
      choices = c()
    )
    ,selectInput(
      "var", "Select a variable",
      choices = d_vars,
      selected = "temp_avg"
    )
    ,actionButton("draw", "Draw a plot")
    
  )
  ,plotOutput("plot")
)

server <- function(input, output){
  observe({
    updateSelectInput(
      inputId = "name",
      choices = d |> 
        distinct(region, name) |>
        filter(region == input$region) |>
        pull(name)
    )
  })
  d_city <- reactive({
    req(input$name)
    d |> filter(name %in% input$name)
  })
  output$plot <- renderPlot({
    d_city() |>
      ggplot(aes(x = date, y = .data[[input$var]], color = name)) +
      ggtitle(names(d_vars)[d_vars == input$var]) +
      geom_line()
    # bindEvent in use
  }) |> bindEvent(input$draw, d_city())
}

shinyApp(ui, server)

# using isolate() to control reactive curve

# isloate() prevents a reactive expression from becomng a dependency of the 
# calling reactive expression

# For a specific example: If we were to use isolate(input$name) instead of
# input$name within the d_city reactive, this would prevent the linking of
# input$name to d_city() in the reactive graph. Meaning that updating
# input$name now no longer causes d_city() to update, as well as all
# downstream reactives and observers.

ui <- page_sidebar(
  title = "Weather forecasts"
  ,sidebar = sidebar(
    selectInput(
      "region", "Select a region",
      choices = c(
        "West",
        "Midwest",
        "Northeast",
        "South"
      )
    )
    ,selectInput(
      "name", "Select an airport",
      choices = c()
    )
    ,selectInput(
      "var", "Select a variable",
      choices = d_vars,
      selected = "temp_avg"
    )
    ,actionButton("draw", "Draw a plot")
    
  )
  ,plotOutput("plot")
)

server <- function(input, output){
  observe({
    updateSelectInput(
      inputId = "name",
      choices = d |> 
        distinct(region, name) |>
        filter(region == input$region) |>
        pull(name)
    )
  })
  d_city <- reactive({
    req(input$name)
    # isolate input$name - updating it wont make the graph re-draw
    d |> filter(name %in% isolate(input$name))
  })
  output$plot <- renderPlot({
    d_city() |>
      ggplot(aes(x = date, y = .data[[input$var]], color = name)) +
      ggtitle(names(d_vars)[d_vars == input$var]) +
      geom_line()
    # bindEvent in use
  }) |> bindEvent(input$draw, d_city())
}

shinyApp(ui, server)

