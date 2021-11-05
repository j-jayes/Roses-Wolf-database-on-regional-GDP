library(shiny)
library(tidyverse)
library(glue)
library(leaflet)
library(sf)
library(shinyWidgets)

df <- read_rds("data.rds")
map_simple <- read_rds("map_simple.rds")

legend_tbl <- df %>% distinct(series) %>% 
  bind_cols(tibble(prefix = c("$", "$", "", "", "", ""),
                   suffix = c("m", "m", "", "%", "%", "%")))

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput("series_input",
        "Series:",
        choices = unique(df$series),
        selected = "Industry share of employment",
        multiple = FALSE
      ),
      sliderTextInput("year_input",
        "Year:",
        choices = c(1900, 1910, 1925, 1938, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2015),
        selected = 1950,
        animate = TRUE
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("leaflet_map"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  legend_prefix <- reactive({
    legend_tbl %>%
      filter(series == input$series_input) %>%
      select(prefix) %>%
      pull()
  })

  legend_suffix <- reactive({
    legend_tbl %>%
      filter(series == input$series_input) %>%
      select(suffix) %>%
      pull()
  })

  df_map <- reactive({
    df %>%
      filter(
        series == input$series_input,
        year == input$year_input
      ) %>%
      filter(!is.na(value)) %>%
      mutate(
        value_table = str_c(legend_prefix(), format(round(value, 0), big.mark = " "), legend_suffix()),
        region_table = region,
        year_table = year
      ) %>%
      gather(
        key, vt,
        region_table, year_table, value_table
      ) %>%
      mutate(
        key = case_when(
          key == "region_table" ~ "Region",
          key == "year_table" ~ "Year",
          TRUE ~ input$series_input
        ),
        key = paste0("<b>", key, "</b>")
      ) %>%
      replace_na(list(vt = "Unknown")) %>%
      nest(data = c(key, vt)) %>%
      mutate(html = map(data,
        knitr::kable,
        format = "html",
        escape = FALSE,
        col.names = c("", "")
      )) %>% 
      inner_join(map_simple) %>% 
      st_sf()
  })
  
  colorpal <- reactive({
    colorNumeric(
      palette = "Spectral",
      domain = df_map()$value
    )
  })
  
  output$leaflet_map <- renderLeaflet({
    
    leaflet() %>%
      fitBounds(lat1 = 29,
                lng1 = 16.0,
                lat2 = 70,
                lng2 = 31.1656) %>% 
      addProviderTiles("CartoDB.Positron")
  })
  
  observe({
    
    pal <- colorpal()
    
    leafletProxy("leaflet_map", data = df_map()) %>%
      clearShapes() %>%
      addPolygons(color = ~ pal(value),
                  fillOpacity = .3,
                  popup = ~ html)
    
  })
  
  observe({
    pal <- colorpal()

    leafletProxy("leaflet_map", data = df_map()) %>%
      clearControls() %>%
      addLegend(position = "bottomright",
                pal = pal, 
                values = ~value,
                title = glue(input$series_input),
                labFormat = labelFormat(
          prefix = glue(legend_prefix()),
          suffix = glue(legend_suffix())
        ))
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
