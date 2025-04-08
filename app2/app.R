# risk_analysis/app.R

library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(readr)
library(leaflet.extras)

# Load data
raw_data <- read_csv("security_incidents.csv")

# Clean and preprocess
clean_data <- raw_data %>%
  filter(!is.na(Latitude), !is.na(Longitude), !is.na(Motive)) %>%
  mutate(
    Motive = trimws(Motive),
    Motive = ifelse(Motive %in% c("Unknown", "Incidental", "Political", "Economic"), Motive, "Other"),
    killed = as.numeric(`Total killed`),
    kidnapped = as.numeric(`Total kidnapped`),
    wounded = as.numeric(`Total wounded`),
    year = as.integer(Year),
    incident = Details,
    death_type = `Means of attack`,
    danger_score = killed + kidnapped + wounded  # Simple sum, not weighted
  )

# UI
ui <- fluidPage(
  titlePanel("🔍 Motive-Based Risk Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("motive_select", "Choose a Motive:", choices = unique(clean_data$Motive), selected = "Political")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("📊 Danger Index", plotOutput("barPlot")),
        tabPanel("🌍 Risk Map", leafletOutput("map", height = 600)),
        tabPanel("📌 Top Countries", tableOutput("countryTable"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered <- reactive({
    clean_data %>% filter(Motive == input$motive_select)
  })
  
  # Danger Score Bar Plot
  output$barPlot <- renderPlot({
    motive_summary <- clean_data %>%
      group_by(Motive) %>%
      summarise(
        total_incidents = n(),
        total_danger_score = sum(danger_score, na.rm = TRUE)
      ) %>%
      arrange(desc(total_danger_score))
    
    ggplot(motive_summary, aes(x = reorder(Motive, total_danger_score), y = total_danger_score, fill = Motive)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Total Danger Score by Motive",
        y = "Total Danger Score",
        x = "Motive"
      ) +
      theme_minimal()
  })
  
  # Leaflet Map
  output$map <- renderLeaflet({
    dat <- filtered()
    pal <- colorNumeric("YlOrRd", domain = dat$danger_score)
    
    top5 <- dat %>% arrange(desc(danger_score)) %>% head(5)
    
    leaflet(dat) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = ~pmin(danger_score, 10),
        color = ~pal(danger_score),
        stroke = TRUE,
        fillOpacity = 0.9,
        popup = ~paste(
          "<strong>Country:</strong>", Country, "<br>",
          "<strong>Year:</strong>", year, "<br>",
          "<strong>Killed:</strong>", killed, "<br>",
          "<strong>Kidnapped:</strong>", kidnapped, "<br>",
          "<strong>Wounded:</strong>", wounded, "<br>",
          "<strong>Danger Score:</strong>", danger_score, "<br>",
          "<strong>Incident:</strong>", incident, "<br>",
          "<strong>Death Type:</strong>", death_type, "<br>",
          "<strong>Motive:</strong>", Motive
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~danger_score, title = "Danger Score") %>%
      addAwesomeMarkers(
        data = top5,
        lng = ~Longitude, lat = ~Latitude,
        icon = awesomeIcons(
          icon = 'arrow-up',
          library = 'fa',
          markerColor = 'red',
          iconColor = 'white'
        ),
        popup = ~paste0(
          "<b>TOP INCIDENT 🔥</b><br>",
          "<b>Country:</b> ", Country, "<br>",
          "<b>Year:</b> ", year, "<br>",
          "<b>Killed:</b> ", killed, "<br>",
          "<b>Kidnapped:</b> ", kidnapped, "<br>",
          "<b>Wounded:</b> ", wounded, "<br>",
          "<b>Danger Score:</b> ", danger_score, "<br>",
          "<b>Incident:</b> ", incident, "<br>",
          "<b>Death Type:</b> ", death_type
        )
      )
  })
  
  # Table of top countries
  output$countryTable <- renderTable({
    filtered() %>%
      group_by(Country) %>%
      summarise(
        Incidents = n(),
        TotalKilled = sum(killed, na.rm = TRUE),
        TotalKidnapped = sum(kidnapped, na.rm = TRUE),
        TotalWounded = sum(wounded, na.rm = TRUE),
        TotalDangerScore = sum(danger_score, na.rm = TRUE)
      ) %>%
      arrange(desc(TotalDangerScore)) %>%
      head(10)
  })
}

# Run app
shinyApp(ui, server)
