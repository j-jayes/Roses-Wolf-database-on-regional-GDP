library(shiny)
library(tidyverse)
library(glue)
library(leaflet)
library(sf)
library(shinyWidgets)
library(ggiraph)
library(bslib)
library(metathis)


df <- read_rds("data.rds")

df_country <- read_rds("data_country.rds")
map_simple <- read_rds("map_simple.rds")

# thematic_shiny(font = "auto")
theme_set(theme_light())
theme_update(text = element_text(size = 17))

legend_tbl <- df %>% distinct(series) %>% 
  bind_cols(tibble(prefix = c("", "", "", "", "", "", "", "", "", ""),
                   suffix = c("", "", "", "%", "%", "%", "", "", "", "")))

# Define UI for application that draws a histogram
ui <- fluidPage(
  # theme = bs_theme(bootswatch = "solar", font_scale = 1.5),
  
  # Application title
  titlePanel("Europe's regional development"),
  
  meta() %>%
    meta_description(
      "Europe's regional development Shiny App"
    ) %>% 
    meta_name("github-repo" = "j-jayes/Roses-Wolf-database-on-regional-GDP") %>% 
    meta_viewport() %>% 
    meta_social(
      title = "Roses Wolf database on regional GDP",
      url = "https://jonathan-jayes.shinyapps.io/Roses-Wolf-database-on-regional-GDP/",
      image = "https://github.com/j-jayes/Roses-Wolf-database-on-regional-GDP/blob/main/images/app.PNG",
      image_alt = "Roses Wolf database on regional GDP App Screenshot",
      og_type = "app",
      og_author = c("Jonathan Jayes"),
      twitter_card_type = "summary",
      twitter_creator = "@JonathanJayes"
    ),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 3,
      selectizeInput("series_input",
        "Series:",
        choices = unique(df$series),
        selected = "Regional GDP per capita (1990 $)",
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
  
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = df_map, { 
    showModal(modalDialog(
      title = "Rosés-Wolf database on regional GDP", 
      # h1('Landing Page'),
      p("The Rosés-Wolf database on regional GDP provides data on the economic development of European regions at the level of NUTS-2 regions for the years 1900 - 2015. It contains information of nominal GDP (in 1990 and 2011 PPP), population, area, and on sector-level employment shares."),
      p("Click on the 'Series' dropdown menu to change the variable displayed on the map."),
      p("Drag the slider to change the year shown on the map, or click the triangle to animate the map."),
      p("Click on a region to compare regional and national dynamics over time."),
      a(href="https://www.wiwi.hu-berlin.de/de/professuren/vwl/wg/roses-wolf-database-on-regional-gdp", "Link to the underlying data")
    ))
  })

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
      reverse = TRUE,
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
                  layerId = ~ region)
    
  })
  
  observe({
    leafletProxy("leaflet_map", data = df_map()) %>%
      clearControls() %>%
      addLegend(
        position = "bottomright",
        pal = colorNumeric(
          palette = "Spectral",
          domain = df_map()$value
        ),
        values = ~value,
        title = glue(input$series_input),
        labFormat = labelFormat(
          transform = function(value) sort(value, decreasing = TRUE),
          prefix = glue(legend_prefix()),
          suffix = glue(legend_suffix())
        )
      )
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

