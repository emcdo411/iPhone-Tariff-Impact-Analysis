### 📱 iPhone Tariff Simulator

This Shiny app simulates the impact of tariffs and economic factors on the cost of manufacturing iPhones in China versus the U.S. It includes visualizations for cost comparisons, state-level manufacturing cost indexes, and a map of key Chinese factories.

---

#### 🛠️ Installation

Make sure you have **R** and **RStudio** installed. Then install the required R packages:

```r
install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "leaflet"))
```

---

#### 🚀 How to Run

Save the script below as `app.R` and run it in RStudio:

```r
# app.R

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)

# Base iPhone production costs (without tariffs)
base_china_cost <- 588
base_us_costs <- c(1500, 1800, 2000)

# Tariff range
tariff_range <- seq(0, 145, by = 5)

# Simulated dataset
iphone_data <- data.frame(
  Tariff = tariff_range,
  China_15 = base_china_cost * (1 + tariff_range / 100),
  US_15 = base_us_costs[1] * (1 + 0 / 100),
  US_15Pro = base_us_costs[2] * (1 + 0 / 100),
  US_15ProMax = base_us_costs[3] * (1 + 0 / 100)
)

state_costs <- data.frame(
  State = c("Texas", "New York", "North Carolina"),
  CostIndex = c(1.0, 1.3, 1.1)
)

factories <- data.frame(
  Name = c("Foxconn Zhengzhou", "Pegatron Shanghai"),
  Lat = c(34.7466, 31.2304),
  Lon = c(113.6254, 121.4737)
)

ui <- dashboardPage(
  dashboardHeader(title = "iPhone Cost Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cost Impact", tabName = "cost_impact", icon = icon("dollar-sign")),
      menuItem("China Tariff Impact", tabName = "china_tariff", icon = icon("chart-line")),
      menuItem("U.S. State Comparison", tabName = "us_states", icon = icon("map")),
      menuItem("China Factory Map", tabName = "china_map", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "cost_impact",
              fluidRow(
                box(width = 4,
                    sliderInput("tariff", "Tariff Percentage:", min = 0, max = 145, value = 0),
                    checkboxGroupInput("factors", "Additional Cost Factors:",
                                       choices = list("Domestic Inflation" = "inflation",
                                                      "Supply Chain Rebuild" = "supply",
                                                      "Policy Penalties" = "penalty"))
                ),
                box(width = 8, plotOutput("costPlot"))
              )
      ),
      tabItem(tabName = "china_tariff",
              h2("Impact of Tariffs on China's Production Costs"),
              plotOutput("chinaTariffPlot")
      ),
      tabItem(tabName = "us_states",
              h2("U.S. State Manufacturing Cost Comparison"),
              plotOutput("stateCostPlot")
      ),
      tabItem(tabName = "china_map",
              h2("iPhone Manufacturing Locations in China"),
              leafletOutput("chinaMap")
      )
    )
  )
)

server <- function(input, output) {
  reactive_data <- reactive({
    tariff_multiplier <- 1 + input$tariff / 100

    inflation <- ifelse("inflation" %in% input$factors, 0.05, 0)
    supply_rebuild <- ifelse("supply" %in% input$factors, 0.10, 0)
    penalty <- ifelse("penalty" %in% input$factors, 0.08, 0)
    total_penalty <- 1 + inflation + supply_rebuild + penalty

    data.frame(
      Tariff = tariff_range,
      China_15 = base_china_cost * (1 + tariff_range / 100),
      US_15 = base_us_costs[1] * tariff_multiplier * total_penalty,
      US_15Pro = base_us_costs[2] * tariff_multiplier * total_penalty,
      US_15ProMax = base_us_costs[3] * tariff_multiplier * total_penalty
    )
  })

  output$costPlot <- renderPlot({
    df <- reactive_data()
    ggplot(df, aes(x = Tariff)) +
      geom_line(aes(y = China_15, color = "China iPhone 15")) +
      geom_line(aes(y = US_15, color = "US iPhone 15")) +
      geom_line(aes(y = US_15Pro, color = "US iPhone 15 Pro")) +
      geom_line(aes(y = US_15ProMax, color = "US iPhone 15 Pro Max")) +
      labs(title = "iPhone Cost vs. Tariffs and U.S. Factors",
           x = "Tariff (%)",
           y = "Estimated Cost ($)",
           color = "Legend") +
      theme_minimal()
  })

  output$chinaTariffPlot <- renderPlot({
    china_costs <- base_china_cost * (1 + tariff_range / 100)
    plot(tariff_range, china_costs, type = "l",
         xlab = "Tariff (%)", ylab = "China Production Cost ($)",
         main = "China Production Cost vs. Tariff", col = "red", lwd = 2)
  })

  output$stateCostPlot <- renderPlot({
    barplot(state_costs$CostIndex, names.arg = state_costs$State,
            ylab = "Cost Index", main = "Manufacturing Cost by State",
            col = "steelblue")
  })

  output$chinaMap <- renderLeaflet({
    leaflet(factories) %>%
      addTiles() %>%
      addMarkers(~Lon, ~Lat, popup = ~Name)
  })
}

shinyApp(ui, server)
```

---

#### 📊 Features

- **Tariff Impact Simulation**  
  Analyze how varying tariff levels affect iPhone prices.

- **Cost Factor Adjustments**  
  Add inflation, supply chain rebuild, and policy penalties.

- **State-by-State Comparison**  
  Visualizes cost index for U.S. manufacturing states.

- **Factory Mapping**  
  Interactive map of major iPhone production sites in China.

---

Let me know if you'd like this saved as a `.md` file for GitHub!



