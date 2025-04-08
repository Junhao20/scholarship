library(shiny)
library(dplyr)
library(leaflet)
library(stringr)
library(ggplot2)
library(plotly)
library(DT)

# Load and clean data
data <- read.csv("security_incidents.csv")

data <- data %>%
  filter(!is.na(Latitude), !is.na(Longitude), !is.na(Year)) %>%
  mutate(
    Details = tolower(Details),
    is_rape = str_detect(Details, "rape|sexual"),
    is_kidnap = Total.kidnapped > 0,
    is_fatal = Total.killed > 0,
    severity = case_when(
      is_fatal ~ "Fatal",
      is_rape ~ "Rape",
      is_kidnap ~ "Kidnapping",
      TRUE ~ "Other"
    ),
    color = case_when(
      severity == "Fatal" ~ "red",
      severity == "Rape" ~ "purple",
      severity == "Kidnapping" ~ "gold",
      TRUE ~ "blue"
    )
  )

# UI
ui <- fluidPage(
  titlePanel("🌍 Humanitarian Security Incidents Map"),
  
  tabsetPanel(
    
    # TAB 1: Global Overview
    tabPanel("Global Overview",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("yearRange", "Select Year Range:",
                             min = min(data$Year),
                             max = max(data$Year),
                             value = c(min(data$Year), max(data$Year)),
                             sep = ""),
                 textInput("searchText", "Search in Incident Details:", placeholder = "e.g. school, bomb, convoy")
               ),
               mainPanel(
                 leafletOutput("globalMap", height = "650px")
               )
             )
    ),
    
    # TAB 2: Explore by Filter
    tabPanel("Explore by Filter",
             sidebarLayout(
               sidebarPanel(
                 selectInput("year", "Year", choices = sort(unique(data$Year)), selected = max(data$Year)),
                 selectInput("country", "Country", choices = sort(unique(na.omit(data$Country))), selected = "Afghanistan"),
                 selectInput("attack", "Means of Attack", choices = sort(unique(na.omit(data$Means.of.attack))), selected = "Shooting")
               ),
               mainPanel(
                 leafletOutput("incidentMap", height = "650px")
               )
             )
    ),
    
    # TAB 3: Summary Stats
    tabPanel("📊 Summary Stats",
             fluidRow(
               column(6, plotlyOutput("barPlot")),
               column(6, dataTableOutput("summaryTable"))
             )
    ),
    
    # TAB 4: Risk Advisory
    tabPanel("🛑 Risk Advisory",
             sidebarLayout(
               sidebarPanel(
                 selectInput("advisoryYear", "Select Year", choices = sort(unique(data$Year)), selected = max(data$Year))
               ),
               mainPanel(
                 h3("Risk Ratings by Country"),
                 dataTableOutput("riskTable"),
                 br(),
                 plotlyOutput("riskBarPlot")
               )
             )
    )
  )
)

# SERVER
server <- function(input, output) {
  
  # GLOBAL FILTERED
  global_filtered <- reactive({
    df <- data %>%
      filter(Year >= input$yearRange[1],
             Year <= input$yearRange[2])
    
    if (input$searchText != "") {
      keyword <- tolower(input$searchText)
      df <- df %>% filter(str_detect(Details, keyword))
    }
    df
  })
  
  # FILTERED BY OPTIONS
  filtered_data <- reactive({
    data %>%
      filter(
        Year == input$year,
        Country == input$country,
        Means.of.attack == input$attack
      )
  })
  
  # GLOBAL MAP
  output$globalMap <- renderLeaflet({
    df <- global_filtered()
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(Total.affected + 1) * 2,
        color = ~color,
        fillColor = ~color,
        fillOpacity = 0.6,
        stroke = TRUE,
        popup = ~paste0(
          "<strong>Year:</strong> ", Year, "<br>",
          "<strong>Country:</strong> ", Country, "<br>",
          "<strong>Type:</strong> ", severity, "<br>",
          "<strong>Details:</strong> ", Details, "<br>",
          "<strong>Killed:</strong> ", Total.killed, "<br>",
          "<strong>Wounded:</strong> ", Total.wounded, "<br>",
          "<strong>Kidnapped:</strong> ", Total.kidnapped
        )
      ) %>%
      addLegend(
        "bottomright",
        colors = c("red", "purple", "gold", "blue"),
        labels = c("Fatal", "Rape-related", "Kidnapping", "Other"),
        title = "Incident Type",
        opacity = 0.9
      )
  })
  
  # FILTERED MAP
  output$incidentMap <- renderLeaflet({
    df <- filtered_data()
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(Total.affected + 1) * 2,
        color = "red",
        fillColor = "red",
        fillOpacity = 0.7,
        stroke = TRUE,
        popup = ~paste0(
          "<strong>Details:</strong> ", Details, "<br>",
          "<strong>City:</strong> ", City, "<br>",
          "<strong>Killed:</strong> ", Total.killed, "<br>",
          "<strong>Wounded:</strong> ", Total.wounded, "<br>",
          "<strong>Kidnapped:</strong> ", Total.kidnapped
        )
      )
  })
  
  # BAR PLOT
  output$barPlot <- renderPlotly({
    yearly <- data %>%
      count(Year) %>%
      arrange(Year)
    
    p <- ggplot(yearly, aes(x = Year, y = n)) +
      geom_col(fill = "#0073C2FF") +
      labs(title = "Number of Incidents Per Year", y = "Incidents", x = "Year") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # SUMMARY TABLE
  output$summaryTable <- renderDataTable({
    summary <- data %>%
      summarise(
        Total_Incidents = n(),
        Total_Killed = sum(Total.killed),
        Total_Wounded = sum(Total.wounded),
        Total_Kidnapped = sum(Total.kidnapped),
        Rape_Related = sum(is_rape, na.rm = TRUE)
      ) %>%
      t() %>%
      as.data.frame()
    
    colnames(summary) <- c("Count")
    summary$Metric <- rownames(summary)
    summary <- summary[, c("Metric", "Count")]
    
    datatable(summary, rownames = FALSE, options = list(dom = "t"))
  })
  
  # RISK ADVISORY TABLE
  output$riskTable <- renderDataTable({
    df <- data %>%
      filter(Year == input$advisoryYear) %>%
      group_by(Country) %>%
      summarise(
        Deaths = sum(Total.killed),
        Kidnaps = sum(Total.kidnapped),
        Rapes = sum(is_rape, na.rm = TRUE),
        RiskScore = 3 * Deaths + 2 * Kidnaps + Rapes
      ) %>%
      mutate(
        RiskLevel = case_when(
          RiskScore > 10 ~ "High",
          RiskScore >= 5 ~ "Moderate",
          TRUE ~ "Low"
        )
      ) %>%
      arrange(desc(RiskScore))
    
    datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # RISK BAR PLOT
  output$riskBarPlot <- renderPlotly({
    df <- data %>%
      filter(Year == input$advisoryYear) %>%
      group_by(Country) %>%
      summarise(
        Deaths = sum(Total.killed),
        Kidnaps = sum(Total.kidnapped),
        Rapes = sum(is_rape, na.rm = TRUE),
        RiskScore = 3 * Deaths + 2 * Kidnaps + Rapes
      ) %>%
      top_n(10, RiskScore)
    
    p <- ggplot(df, aes(x = reorder(Country, RiskScore), y = RiskScore, fill = RiskScore)) +
      geom_col() +
      coord_flip() +
      labs(title = "Top 10 Highest-Risk Countries", x = "Country", y = "Risk Score") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)