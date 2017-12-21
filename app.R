library(shiny)
library(readxl)  
library(lubridate, warn.conflicts = FALSE)
library(xtable, warn.conflicts = FALSE)
source("global.R")


ui <- pageWithSidebar(
  # Sidebar with controls to select the dataset and forecast ahead duration
  headerPanel('USD Revenue forecast'),
  sidebarPanel(
    h2("Forecasting time series"),
    p("This app forecasts USD revenues for the year 2017 up to year 2020.
        This allows informed decision by considering future predicted revenues. 
        The forecast is based on data in the time range 2014-01-01 to 2016-12-31.",
      style = "font-family: 'times'; font-si16pt"),   
    h4("User input"),
    p("Choose the parameters for your custom forecast."), 
    selectInput('lead_controlling_office', 'Lead Controlling Office', c("All"="All", unique(data_frame$`Lead Controlling Office`))),
    selectInput('mode_of_transport', 'Mode of transport', c("All"="All", unique(data_frame$`Mode of transport`))),
    selectInput('destination_country', 'Destination country', c("All"="All", unique(data_frame$`Destination country`))),
    selectInput('lead_owner', 'Lead Owner', c("All"="All", unique(data_frame$`Lead Owner`))),
    selectInput('default_channel_grouping', 'Default Channel Grouping', c("All"="All", unique(data_frame$`Default Channel Grouping`))),
    numericInput("ahead", "Months to Forecast Ahead:", 6, 36),
    submitButton("Update Forecast"),
    br(),
    p("Developed by "),
    span("Manniti Luca", style = "color:blue"),
    #br(),
    h6("Code: ", a(href = "https://github.com/luchetto/kalinax-forecast", "Github"))),
   
  mainPanel(
    h4("Time series forecast algorithm is based on ", a(href = "https://www.otexts.org/fpp/7", "exponential smoothing methods.")),
    dygraphOutput("etsForecastPlot"),
    br(),
    strong("Confidence level = 80%", align="center"),
    h4("Normal forecast point estimates (mean) and prediction intervals (upper and lower) if you anticipate typical market behaviour:"),
    tableOutput("summary_80"),
    br(),
    strong("Confidence level = 95%", align="center"),
    h4("Conservative forecast intervals if uncertainity is high:"),
    tableOutput("summary_95")
    ))



server <- function(input, output) {
  # Filter data based on selections
  filtered_data <- reactive({
    data <- data_frame
    if (input$lead_controlling_office != "All") 
      {data <- data[data$'Lead Controlling Office' == input$lead_controlling_office,]}
    if (input$mode_of_transport != "All") 
      {data <- data[data$'Mode of transport' == input$mode_of_transport,]}
    if (input$destination_country != "All") 
      {data <- data[data$'Destination country' == input$destination_country,]}
    if (input$lead_owner != "All") 
      {data <- data[data$'Lead Owner' == input$lead_owner,]}
     if (input$default_channel_grouping != "All") 
      {data <- data[data$'Default Channel Grouping' == input$default_channel_grouping,]}
    data
  })
  
plot_data <- reactive({
  data <- filtered_data()
  data <- aggregate(filtered_data()$`Revenue USD`, list(strftime(as.Date(filtered_data()$`Lead Creation Date`) , format="%Y-%m")), FUN = sum , na.rm=TRUE)
  # Filling missing dates
  st <- as.Date("2014-01-01")
  en <- as.Date("2016-12-31")
  s <- list(seq(st,en, "months"))
  s <- s[[1]]
  s <- sapply(s, function(x) strftime(as.Date(x), format="%Y-%m"))
  s <- as.data.frame(s)
  colnames(s)<-"Group.1"
  data <- merge(s,data,by="Group.1",all.x=TRUE)
  data[is.na(data)] <- 0.00
  data <- ts(data$x, frequency=12, start=c(2014,1))
  data_forecast <- HoltWinters(data)
  data_forecast2 <- forecast(data_forecast, h=input$ahead)
  plot_data <- funggcast(data, data_forecast2)
  plot_data <- plot_data/1000 #Scaling results
})

output$etsForecastPlot <- renderDygraph({
    dygraph(plot_data(), ylab = "Revenue (in thousand USD)")%>% 
    dyLegend(show = "onmouseover", hideOnMouseOut = TRUE, width = 500, labelsSeparateLines = FALSE) %>% 
    dyShading(from = "2015-1-1", to = "2015-1-15") %>% 
    dyEvent("2015-1-1", "Observed and fitted data", labelLoc = "top", color = "black") %>% 
    dyEvent("2016-12-31", "Forecasted values", labelLoc = "top", color = "black")  %>% 
    dySeries("observed", color="#cca300") %>%
    dySeries("fitted", color="#b30000")  %>%
    dySeries("forecast", label="mean-forecast") %>%
    dySeries("hi95", strokePattern = "dashed", color = "#1a6600") %>%
    dySeries("lo95", strokePattern = "dashed", color = "#1a6600") %>%
    dySeries("hi80", strokePattern = "dashed", color = "#0047b3") %>%
    dySeries("lo80", strokePattern = "dashed", color = "#0047b3") %>%
    dySeries(c("hi80","forecast", "lo80"), label="80-forecast") %>%
    dySeries(c("hi95","forecast","lo95"), label="95-forecast") %>%
    dyRangeSelector()
    })

output$summary_95 <- renderTable({
  data <- plot_data()
  forecast_only <- window(data, 2017)
  data_summary_95 <- cbind(forecast = forecast_only[,"forecast"],
                        hi95 = forecast_only[,"hi95"],
                        lo95 = forecast_only[,"lo95"])
  xtable(data_summary_95)
  })


output$summary_80 <- renderTable({
  data <- plot_data()
  forecast_only <- window(data, 2017)
  data_summary_80 <- cbind(forecast = forecast_only[,"forecast"],
                        hi80 = forecast_only[,"hi80"],
                        lo80 = forecast_only[,"lo80"])
  xtable(data_summary_80)
})
}
  
shinyApp(ui = ui, server = server)
  