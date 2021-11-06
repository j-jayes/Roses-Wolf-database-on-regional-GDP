library(shiny)
library(tidyverse)
library(glue)
library(leaflet)
library(sf)
library(shinyWidgets)
library(ggiraph)
# library(bslib)
# library(thematic)


df <- read_rds("data.rds")
df_country <- read_rds("data_country.rds")
map_simple <- read_rds("map_simple.rds")

thematic_shiny(font = "auto")
# theme_update(text = element_text(size = 17))

legend_tbl <- df %>% distinct(series) %>% 
  bind_cols(tibble(prefix = c("$", "$", "", "", "", ""),
                   suffix = c("m", "m", "", "%", "%", "%")))

# Define UI for application that draws a histogram
ui <- fluidPage(
  # theme = bs_theme(bootswatch = "minty", font_scale = 1.5),
  
  # Application title
  titlePanel("Europe's regional development"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 3,
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
      fluidRow(
        column(width = 7,
               leafletOutput("leaflet_map", height = 800)),
        column(width = 5,
               ggiraphOutput("stacked_fill"),
               ggiraphOutput("facet_line")))

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
      setView(lng = 12,
              lat = 56,
              zoom = 4) %>% 
      addProviderTiles("CartoDB.Positron")
  })
  
  observe({
    
    pal <- colorpal()
    
    leafletProxy("leaflet_map", data = df_map()) %>%
      clearShapes() %>%
      addPolygons(color = ~ pal(value),
                  fillOpacity = .3,
                  popup = ~ html,
                  layerId = ~region)
    
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
  
  # output$click_test <- renderPrint({reactiveValuesToList(input)})

  output$stacked_fill <- renderggiraph({
    
    req(input$leaflet_map_shape_click$id)
    
    g <- df %>% 
      filter(series %in% c("Agric. share of employment",
                           "Industry share of employment",
                           "Services share of employment"),
             region == input$leaflet_map_shape_click$id) %>% 
      mutate(series = str_remove(series, "share of employment")) %>% 
      ggplot(aes(year, value, fill = series, tooltip = series)) +
      geom_area_interactive(position = "fill") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_brewer(palette = "Spectral") +
      theme(legend.position = "bottom") +
      labs(x = NULL,
           y = NULL,
           fill = NULL,
           title = "Employment composition")
    
    
    ggiraph(ggobj = g)
    
  })
  
  output$facet_line <- renderggiraph({
    
    req(input$leaflet_map_shape_click$id)
    
    country_name <- df %>% 
      filter(region == input$leaflet_map_shape_click$id) %>% 
      distinct(country_current_borders) %>% pull()
    
    f <- df %>% 
      filter(series %in% c("Population", "Regional GDP (2011 $m)"),
             region == input$leaflet_map_shape_click$id) %>% 
      inner_join(df_country, by = c("country_current_borders", "year", "series")) %>% 
      pivot_longer(c(value, country_avg), names_to = "stat") %>% 
      mutate(stat = case_when(
        stat == "country_avg" ~ str_c("Avg. for regions in ", country_name),
        TRUE ~ input$leaflet_map_shape_click$id
      )) %>% 
      mutate(value_disp = format(round(value), big.mark = " "),
             tooltip = str_c(stat, " ", value_disp)) %>% 
      ggplot(aes(year, value, colour = stat, tooltip = tooltip)) +
      geom_line(aes(year, value, colour = stat, group = stat), cex = 2, alpha = .8) +
      geom_point_interactive(cex = 3, alpha = .8) +
      facet_wrap(~ series, scales = "free_y", nrow = 2) +
      scale_y_continuous(labels = scales::number_format()) +
      scale_colour_manual(values = c("#D53E4F", "#66C2A5")) +
      theme(legend.position = "bottom") +
      labs(x = NULL,
           y = NULL,
           colour = NULL,
           title = "Population and GDP") 
    
    
    ggiraph(ggobj = f)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
