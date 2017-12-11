library(leaflet) 
library(shiny) 
library(shinydashboard)
library(readr)
library(methods)
library(rgdal)  
library(plyr) 
library(highcharter) 
library(RCurl)
library(googleVis)　
library(openair) 
library(DT)
alldata_20072016B <- read_csv("alldata_20072016C.csv", 
                             col_types = cols(PM2.5 = col_number()))
alldata_20072016  <- alldata_20072016B
taiwan <- readOGR("COUNTY_MOI_1060525.shp",encoding="big5" ,
                  layer = "COUNTY_MOI_1060525", verbose = FALSE)

alldata_m_d <- read_csv("alldata_m_d_20072016.csv", 
                        col_types = cols(PM2.5 = col_number(), 
                                         date = col_datetime(format = "%Y-%m-%d")))
alldata_m_d_h <- alldata_m_d 
alldata_m_d_PM25 <- alldata_m_d 
alldata_m_d_PM10 <- alldata_m_d
alldata_m_d_YMD <- alldata_m_d 
alldata_m_d_o <- alldata_m_d
ui <- 
  dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
      fluidPage(
        tags$head(tags$script(src = "message-handler.js"))
      ),
      sidebarMenu(
        menuItem(
          "Maps", 
          tabName = "maps", 
          icon = icon("globe"),
          menuSubItem("TaiwanAQI", tabName = "m_AQI", icon = icon("map")),
          menuSubItem("PM2.5 hour map", tabName = "m_data", icon = icon("map")),
          menuSubItem("PM2.5 day map", tabName = "m_day25", icon = icon("map")),
          menuSubItem("PM2.5 anime", tabName = "m_anime25", icon = icon("map"))
        ),
        menuItem(
          "Charts", 
          tabName = "charts", 
          icon = icon("bar-chart"),
          menuSubItem("TaiwanAQI", tabName = "c_AQI", icon = icon("area-chart")),
          menuSubItem("PM10_Calendar", tabName = "PM10_Calendar", icon = icon("area-chart")),
          menuSubItem("Calendar openair", tabName = "Calendar", icon = icon("area-chart")),
          menuSubItem("timeVariation openair", tabName = "timeVariation", icon = icon("area-chart")),
          menuSubItem("highcharter day avg", tabName = "highcharterYMD", icon = icon("area-chart")),
          menuSubItem("highcharter month and year avg", tabName = "highcharterYM", icon = icon("area-chart"))
          
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "m_AQI",
          fluidPage(
            title = "AQI MAP",
            collapsible = TRUE,
            width = "100%",
            height = "100%",
            leafletOutput("AQImap", height = "900px")
          )
        ),
        tabItem(
          tabName = "m_data",
          
          fluidPage(
            column(2,
                   dateInput('date',
                           label = 'Date input:',
                           value  = "2010-03-21", min = '2007-01-01', max = '2016-12-31',
                           language = "zh-TW")
            ),
            column(2,
                   selectInput("hour",
                               "hour:",
                               c(unique(as.character(alldata_20072016$hour))))
            ),
            title = "data MAP",
            collapsible = TRUE,
            width = "100%",
            height = "100%",
            leafletOutput("datamap", height = "900px")
          )
        ),
        
        tabItem(
          tabName = "m_day25",
                    fluidPage(
           
                   dateInput('date2',
                             label = 'Date input:',
                             value  = "2010-03-21", min = '2007-01-01', max = '2016-12-31',
                             language = "zh-TW")  ,
            
            title = "day PM2.5",
            collapsible = TRUE,
            width = "100%",
            height = "100%",
            leafletOutput("day25map", height = "900px")
          )
        ),
        tabItem(
          tabName = "m_anime25",
          fluidPage(
            title = "anime PM2.5",
            collapsible = TRUE,
            width = "100%",
            height = "100%",
           
            sidebarPanel(
              sliderInput("date1", "Date:",
                          min=as.POSIXct("2007-01-01 00:00:00", tz = "Asia/Taipei"),
                          max=as.POSIXct("2016-12-31 23:00:00", tz = "Asia/Taipei"),
                          value=as.POSIXct("2010-03-22 00:00:00", tz = "Asia/Taipei"), 
                          animate = animationOptions(interval = 500, loop = FALSE, 
                          playButton = NULL,pauseButton = NULL),step = 3600)),
            
              leafletOutput("map", height = "800px")
            
          )
        ),
        
        tabItem(
          tabName = "c_AQI",
          dataTableOutput ("AQI_data_table")
          
        ),
        tabItem(
          tabName = "PM10_Calendar",
          fluidPage(
            collapsible = TRUE,
            width = "100%",
            height = "100%",
            column(2,
                   selectInput("pop",
                               "pop:",
                               c("PM2.5","PM10"))),
            column(2,
                   selectInput("sitegv",
                               "site:", 
                               unique(as.character(alldata_m_d_h$site)))),
            br(),
            br(),
            htmlOutput("gvisCalendarPM10")
          )),
        tabItem(
          tabName = "Calendar",
          fluidPage(
            collapsible = TRUE,
            width = "100%",
            height = "100%",
            column(2,
                   selectInput("sitecalendarPlot",
                               "site:", 
                               unique(as.character(alldata_m_d$site)))),
            column(2,
                   selectInput("yearcalendarPlot",
                               "year:", 
                               unique(as.character(alldata_m_d$year)))),
            
            br(),
            plotOutput("CalendarA"),
            br(),
            highchartOutput("highcharterCalendarA"),
            br(),
            plotOutput("CalendarB"),
            br(),
            highchartOutput("highcharterCalendarB")
          )),
        
        tabItem(
          tabName = "timeVariation",
          fluidPage(
            collapsible = TRUE,
            width = "100%",
            height = "100%",
            column(3,
                   dateRangeInput("date_range_timeVariation", ("Date Range"),
                                  start="2007-01-01", end="2016-12-31",
                                  min="2007-01-01", max="2016-12-31",
                                  language = "zh-TW")),
            column(2,
                   selectInput("sitetimeVariation",
                               "site:", 
                               unique(as.character(alldata_20072016$site)))),
            column(2,
            checkboxGroupInput("pollutantcheckGroup", label = ("pollutant"),
                               choices = c("PM2.5","PM10","O3","CO","NO","NO2","NOx","SO2"),
                               selected =c("PM2.5","PM10","O3"))),
            br(),
            plotOutput("timeVariation")
          )),
        
        
        tabItem(
          tabName = "highcharterYMD",
          fluidPage(
            collapsible = TRUE,
            width = "100%",
            height = "100%",
            column(3,
            dateRangeInput("date_range", ("Date Range"),
                           start="2007-01-01", end="2016-12-31",
                           min="2007-01-01", max="2016-12-31",
                           language = "zh-TW")),
            column(2,
            selectInput("hsite",
                        "site:", 
                        unique(as.character(alldata_m_d_YMD$site)))),
            br(),
            highchartOutput("highcharter25"),
            br(),
            br(),
            highchartOutput("highcharter10")
          )),
        tabItem(
          tabName = "highcharterYM",
          fluidPage(
            collapsible = TRUE,
            width = "100%",
            height = "100%",
            selectInput("HMsite",
                        "site:", 
                        unique(as.character(alldata_20072016$site))),
            highchartOutput("highcharterYM2510"),
            br(),
            br(),
            highchartOutput("highcharterY2510")
          ))
      )))
server <- function(input, output, session) {
  #      AQXSite <- read_csv("http://opendata.epa.gov.tw/webapi/api/rest/datastore/355000000I-000006/?format=csv&offset=0&sort=SiteName&token=xg1XRujOhUCu+ZcH8+A5sQ")
  #        AQI_data <- read_csv("http://opendata.epa.gov.tw/webapi/api/rest/datastore/355000000I-000259/?format=csv&offset=0&sort=SiteName&token=xg1XRujOhUCu+ZcH8+A5sQ")
  #     xmlfile=xmlParse("http://opendata.cwb.gov.tw/govdownload?dataid=F-D0047-089&authorizationkey=rdec-key-123-45678-011121314")
  #   UV_data <- read_csv("http://opendata.epa.gov.tw/webapi/api/rest/datastore/355000000I-000005/?format=csv&sort=PublishTime&offset=0&limit=1000&token=xg1XRujOhUCu+ZcH8+A5sQ")
  #        AQXSite <- read_csv("AQXSite.csv")
  #       AQI_data <- read_csv("AQI_data.csv")
  #      
  AQXSite <- read_csv("AQXSite_20171028164209.csv")
  #    
  AQI_data <- read_csv("AQI_data_20171028164210.csv")
  ######################################################################################
  
  getColorAQI <- function(AQI_data) {
    sapply(AQI_data$AQI, function(AQI) {
      if(AQI <= 50& (!is.na(AQI)) ) {
        "green"
      } else if(AQI <= 100& (!is.na(AQI)))  {
        "orange"
      } else if(AQI <= 150& (!is.na(AQI)))  {
        "red"
      } else {
        "black"
      } 
    }
    )
  }
  iconsAQI <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColorAQI(AQI_data)
  )
  
  popupAQI <- paste(
    "<strong><font color='red'>SiteName: </font></strong>", AQI_data$SiteName, "<br>", 
    "<strong><font color='red'>AQI:</font>  </strong>", AQI_data$AQI, 
    "<strong><font color='red'>Pollutant: </font></strong>", AQI_data$Pollutant, "<br>", 
    "<strong><font color='red'>Status: </font>  </strong>", AQI_data$Status, "<br>", 
    "<strong><font color='red'>SO2: </font>  </strong>", AQI_data$SO2, "<br>", 
    "<strong><font color='red'>CO: </font></strong>", AQI_data$CO, 
    "<strong><font color='red'>CO_8hr: </font>  </strong>", AQI_data$CO_8hr, "<br>", 
    "<strong><font color='red'>O3: </font></strong>", AQI_data$O3,
    "<strong><font color='red'>O3_8hr: </font>  </strong>", AQI_data$O3_8hr, "<br>", 
    "<strong><font color='red'>PM10: </font>  </strong>", AQI_data$PM10, 
    "<strong><font color='red'>PM2.5: </font>  </strong>", AQI_data$PM2.5,"<br>", 
    "<strong><font color='red'>NO2: </font>  </strong>", AQI_data$NO2, 
    "<strong><font color='red'>NOx: </font>  </strong>", AQI_data$NOx,
    "<strong><font color='red'>NO: </font>  </strong>", AQI_data$NO,  "<br>", 
    "<strong><font color='red'>PM2.5_AVG: </font>  </strong>", AQI_data$PM2.5_AVG, 
    "<strong><font color='red'>PM10_AVG: </font>  </strong>", AQI_data$PM10_AVG, "<br>", 
    "<strong><font color='red'>PublishTime: </font>  </strong>", AQI_data$PublishTime, "<br>"
  )
  
  
  output$AQImap <- renderLeaflet({
    AQImap <- 
      leaflet(taiwan) %>% addTiles() %>%
      addPolygons(color = "#444444", weight = 1.5, smoothFactor = 1.5,
                  opacity = 1.5, fillOpacity = 0.1,
                  highlightOptions = highlightOptions(bringToFront = TRUE))%>%
      addAwesomeMarkers(lng=AQXSite$TWD97Lon,lat=AQXSite$TWD97Lat, icon=iconsAQI, label=~as.character(AQXSite$SiteName),popup= ~popupAQI)
  }) 
  ###############################################################################
  
  output$datamap <- renderLeaflet({
    data <- alldata_20072016B
    
    data <- data[data$date2 ==input$date,]
    data <- data[data$hour == input$hour,]
    
    bins <- c(0, 35, 53, 70, Inf)
    cPal <- colorBin(palette = c("#00ff00","#FFA500","#963296"),domain = data$PM2.5, bins = bins)
    
    leaflet(taiwan) %>%addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(color = "#444444", weight = 1.5, smoothFactor = 1.5,
                  opacity = 1.5, fillOpacity = 0.1) %>% 
      addCircleMarkers(lng=data$TWD97Lon,lat=data$TWD97Lat, 
                       radius=13,stroke=FALSE, fillOpacity = 0.9,
                       fillColor = ~cPal(data$PM2.5), 
                       label =~as.character(data$PM2.5), 
                       popup = ~as.character(data$site))%>% 
      addLegend("bottomright", pal = cPal, values =data$PM2.5,title = "PM2.5",
                labFormat = labelFormat(suffix = " "),opacity = 1)
  }) 
  ######################################################################################

  
  output$day25map <- renderLeaflet({
    
    day25 <- alldata_m_d[alldata_m_d$date ==input$date2,]
    
    bins <- c(0, 15, 35, 54,150,250, Inf)
    cPal <- colorBin(palette = c("#00ff00","#ffff00","#FFA500","#ff0000","#963296","#7d0023"),domain = day25$PM2.5, bins = bins)
    
    leaflet(taiwan) %>%addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(color = "#444444", weight = 1.5, smoothFactor = 1.5,
                  opacity = 1.5, fillOpacity = 0.1) %>% 
      addCircleMarkers(lng=day25$TWD97Lon,lat=day25$TWD97Lat, 
                       radius=13,stroke=FALSE, fillOpacity = 0.9,
                       fillColor = ~cPal(day25$PM2.5), 
                       label =~as.character(day25$PM2.5), 
                       popup = ~as.character(day25$site))%>% 
      addLegend("bottomright", pal = cPal, values =day25$PM2.5,title = "PM2.5",
                labFormat = labelFormat(suffix = " "),opacity = 1)
  }) 
  ######################################################################################
  
  output$AQI_data_table  <- renderDataTable({
    datatable(AQI_data, options = list(pageLength = 100))
  })
  ######################################################################################
  
  
  output$gvisCalendarPM10 <- renderGvis({
    if (input$sitegv != "All") {
      alldata_m_d_YMD <- alldata_m_d_YMD[alldata_m_d_YMD$site == input$sitegv,]
    }
    
    gvisCalendar(alldata_m_d_YMD, 
                 datevar="date", 
                 numvar=input$pop,
                 
                 options=list(colors="['#00ff00', '#ffff00', '#ff0000']",width=1500, height=3500))
  })
  
  
  output$CalendarA <- renderPlot({
    if (input$sitecalendarPlot != "All") {
      alldata_m_d_1 <- alldata_m_d[alldata_m_d$site == input$sitecalendarPlot,]
    }
    if (input$yearcalendarPlot != "All") {
      alldata_m_d_2 <- alldata_m_d_1[alldata_m_d_1$year == input$yearcalendarPlot,]
    }
    
    pm25.breaks <- c(0, 12, 24, 35, 42, 47, 53, 59, 65, 70, 1000)
    labels <- c(". - Low", "12 - Low", "24 - Low", "35 - Moderate", "42 - Moderate",
                "47 - Moderate", "43 - High", "59 - High", "65 - High", "70 - Very High")
    
    calendarPlot(alldata_m_d_2,pollutant = "PM2.5", year =input$yearcalendarPlot, annotate = "value",
                 labels = labels,breaks = pm25.breaks,main=input$sitecalendarPlot)
    
  })
  output$CalendarB <- renderPlot({
    if (input$sitecalendarPlot != "All") {
      alldata_m_d_1 <- alldata_m_d[alldata_m_d$site == input$sitecalendarPlot,]
    }
    if (input$yearcalendarPlot != "All") {
      alldata_m_d_2 <- alldata_m_d_1[alldata_m_d_1$year == input$yearcalendarPlot,]
    }
    pm10.breaks <- c(0, 17, 34, 50, 59, 67, 75, 84, 92, 100, 1000)
    labels10 <- c("0 - Low", "17 - Low", "34 - Low", "50 - Moderate", "59 - Moderate",
                  "67 - Moderate", "75 - High", "84 - High", "92 - High", "100 - Very High")
    calendarPlot(alldata_m_d_2,pollutant = "PM10", labels = labels10,breaks = pm10.breaks, 
                 year =input$yearcalendarPlot, annotate = "value", main=input$sitecalendarPlot)
    
  })
  output$highcharterCalendarA <- renderHighchart({
    if (input$sitecalendarPlot != "All") {
      alldata_m_d_1 <- alldata_m_d[alldata_m_d$site == input$sitecalendarPlot,]
    }
    if (input$yearcalendarPlot != "All") {
      alldata_m_d_2 <- alldata_m_d_1[alldata_m_d_1$year == input$yearcalendarPlot,]
    }
    
    highchart() %>% 
      hc_chart(type = "line") %>% 
      hc_xAxis(categories=alldata_m_d_2$date)  %>%
      hc_yAxis(title = list(text = "PM2.5"),
               plotLines = list(
                 list(label = list(text = "PM2.5日平均標準值35μg／m3"),
                      value = 35,color = "red", width = 2)))  %>%
      hc_plotOptions(line = list(
        dataLabels = list(enabled = F),
        enableMouseTracking = T) ) %>% 
      hc_legend(align = "left", verticalAlign = "top",
                layout = "vertical", x = 0, y = 100) %>%
      hc_tooltip(crosshairs = F, backgroundColor = "#FCFFC5",
                 shared = F, borderWidth = 5) %>% 
      
      hc_add_series(name="PM2.5" ,data = alldata_m_d_2$PM2.5)
  })
  
  output$highcharterCalendarB <- renderHighchart({
    if (input$sitecalendarPlot != "All") {
      alldata_m_d_1 <- alldata_m_d[alldata_m_d$site == input$sitecalendarPlot,]
    }
    if (input$yearcalendarPlot != "All") {
      alldata_m_d_2 <- alldata_m_d_1[alldata_m_d_1$year == input$yearcalendarPlot,]
    }
    
    highchart() %>% 
      hc_chart(type = "line") %>% 
      hc_xAxis(categories=alldata_m_d_2$date)  %>%
      hc_yAxis(title = list(text = "PM10"),
               plotLines = list(
                 list(label = list(text = "PM10日平均標準值125μg／m3"),
                      value = 125, color = "red", width = 2)) )%>%
      hc_plotOptions(line = list(
        dataLabels = list(enabled = F),
        enableMouseTracking = T) ) %>% 
      hc_legend(align = "left", verticalAlign = "top",
                layout = "vertical", x = 0, y = 100) %>%
      hc_tooltip(crosshairs = F, backgroundColor = "#FCFFC5",
                 shared = F, borderWidth = 5) %>% 
      
      hc_add_series(name="PM10" ,data = alldata_m_d_2$PM10)
  })
  ############################################################################################
  output$timeVariation <- renderPlot({
    if (input$sitetimeVariation != "All") {
      timeVariationDATA <- alldata_20072016[alldata_20072016$site == input$sitetimeVariation,]
    }

    timeVariationDATA <- subset(timeVariationDATA, date2 >= input$date_range_timeVariation[1] & date2 <= input$date_range_timeVariation[2])
    
    timeVariation(timeVariationDATA, pollutant = input$pollutantcheckGroup, normalise = TRUE)
    
  })
  ###################################################################################
  output$highcharter25 <- renderHighchart({
    
    if (input$hsite != "All") {
      alldata_m_d_PM25 <- alldata_m_d_PM25[alldata_m_d_PM25$site == input$hsite,]
    }
    
    alldata_m_d_PM25 <- subset(alldata_m_d_PM25, date >= input$date_range[1] & date <= input$date_range[2])
    highchart() %>% 
      hc_chart(type = "line") %>% 
      hc_xAxis(categories=alldata_m_d_PM25$date)  %>%
      hc_yAxis(title = list(text = "PM2.5"),
               plotLines = list(
                 list(label = list(text = "PM2.5日平均標準值35μg／m3"),
                      value = 35,color = "red", width = 2))) %>%
      hc_plotOptions(line = list(
        dataLabels = list(enabled = F),
        enableMouseTracking = T) ) %>% 
      hc_legend(align = "left", verticalAlign = "top",
                layout = "vertical", x = 0, y = 100) %>%
      hc_tooltip(crosshairs = F, backgroundColor = "#FCFFC5",
                 shared = F, borderWidth = 5) %>% 
      
      hc_add_series(name="PM2.5" ,data = alldata_m_d_PM25$PM2.5)
  })
  
  output$highcharter10 <- renderHighchart({
    if (input$hsite != "All") {
      alldata_m_d_PM10 <- alldata_m_d_PM10[alldata_m_d_PM10$site == input$hsite,]
    }
    alldata_m_d_PM10 <- subset(alldata_m_d_PM10, date >= input$date_range[1] & date <= input$date_range[2])
    highchart() %>% 
      hc_chart(type = "line") %>% 
      hc_xAxis(categories=alldata_m_d_PM10$date )  %>%
      hc_yAxis(title = list(text = "PM10"),
               plotLines = list(
                 list(label = list(text = "PM10日平均標準值125μg／m3"),
                      value = 125, color = "red", width = 2)) ) %>%
      hc_plotOptions(line = list(
        dataLabels = list(enabled = F),
        enableMouseTracking = T) ) %>% 
      hc_legend(align = "left", verticalAlign = "top",
                layout = "vertical", x = 0, y = 100) %>%
      hc_tooltip(crosshairs = F, backgroundColor = "#FCFFC5",
                 shared = F, borderWidth = 5) %>% 
      
      hc_add_series(name="PM10" ,data = alldata_m_d_PM10$PM10)
  })
  ############################################################################################

  
  output$highcharterYM2510 <- renderHighchart({
    if (input$HMsite != "All") {
      alldata_20072016YMPM10 <- alldata_20072016[alldata_20072016$site == input$HMsite,]
    }
    alldata_20072016YMPM10 <- tapply(alldata_20072016YMPM10$PM10, format(alldata_20072016YMPM10$date,"%Y-%m"),mean, na.rm = TRUE)
    alldata_20072016YMPM10 <- data.frame(alldata_20072016YMPM10)
    alldata_20072016YMPM10 <- cbind(Row.Names = rownames(alldata_20072016YMPM10), alldata_20072016YMPM10)
    rownames(alldata_20072016YMPM10) <- NULL
    names(alldata_20072016YMPM10) <- c("date", "PM10")
    if (input$HMsite != "All") {
      alldata_20072016YMPM2.5 <- alldata_20072016[alldata_20072016$site == input$HMsite,]
    }
    alldata_20072016YMPM2.5 <- tapply(alldata_20072016YMPM2.5$PM2.5, format(alldata_20072016YMPM2.5$date,"%Y-%m"),mean, na.rm = TRUE)
    alldata_20072016YMPM2.5 <- data.frame(alldata_20072016YMPM2.5)
    alldata_20072016YMPM2.5 <- cbind(Row.Names = rownames(alldata_20072016YMPM2.5), alldata_20072016YMPM2.5)
    rownames(alldata_20072016YMPM2.5) <- NULL
    names(alldata_20072016YMPM2.5) <- c("date", "PM2.5")
    
    
    highchart() %>% 
      hc_chart(type = "line") %>% 
      hc_xAxis(categories=alldata_20072016YMPM10$date)  %>%
      hc_yAxis(title = list(text = "月平均")) %>%
      hc_plotOptions(line = list(
        dataLabels = list(enabled = F),
        enableMouseTracking = T) ) %>% 
      hc_legend(align = "left", verticalAlign = "top",
                layout = "vertical", x = 0, y = 100) %>%
      hc_tooltip(crosshairs = F, backgroundColor = "#FCFFC5",
                 shared = F, borderWidth = 5) %>% 
      
      hc_add_series(name="PM10" ,data = alldata_20072016YMPM10$PM10)%>%
      hc_add_series(name="PM2.5" ,data = alldata_20072016YMPM2.5$PM2.5)
  })
  ############################################################################################
  
  output$highcharterY2510 <- renderHighchart({
    if (input$HMsite != "All") {
      alldata_20072016YPM10 <- alldata_20072016[alldata_20072016$site == input$HMsite,]
    }
    if (input$HMsite != "All") {
      alldata_20072016YPM2.5 <- alldata_20072016[alldata_20072016$site == input$HMsite,]
    }
    alldata_20072016YPM10 <- tapply(alldata_20072016YPM10$PM10, format(alldata_20072016YPM10$date,"%Y"),mean, na.rm = TRUE)
    alldata_20072016YPM10 <- data.frame(alldata_20072016YPM10)
    alldata_20072016YPM10 <- cbind(Row.Names = rownames(alldata_20072016YPM10), alldata_20072016YPM10)
    rownames(alldata_20072016YPM10) <- NULL
    names(alldata_20072016YPM10) <- c("date", "PM10")
    
    alldata_20072016YPM2.5 <- tapply(alldata_20072016YPM2.5$PM2.5, format(alldata_20072016YPM2.5$date,"%Y"),mean, na.rm = TRUE)
    alldata_20072016YPM2.5 <- data.frame(alldata_20072016YPM2.5)
    alldata_20072016YPM2.5 <- cbind(Row.Names = rownames(alldata_20072016YPM2.5), alldata_20072016YPM2.5)
    rownames(alldata_20072016YPM2.5) <- NULL
    names(alldata_20072016YPM2.5) <- c("date", "PM2.5")
    
    highchart() %>% 
      hc_chart(type = "line") %>% 
      hc_xAxis(categories=alldata_20072016YPM10$date)  %>%
      hc_yAxis(title = list(text = "年平均"),
               plotLines = list(
                 list(label = list(text = "PM10年平均標準值65 μg／m3"),
                      value = 65, color = "red", width = 2)),
               
               plotBands = list(
                 list(label = list(text = "PM2.5年平均標準值15 μg／m3"),
                      value = 15, color = "red", width = 2)))%>%
               
      hc_plotOptions(line = list(
        dataLabels = list(enabled = F),
        enableMouseTracking = T) ) %>% 
      hc_legend(align = "left", verticalAlign = "top",
                layout = "vertical", x = 0, y = 100) %>%
      hc_tooltip(crosshairs = F, backgroundColor = "#FCFFC5",
                 shared = F, borderWidth = 5) %>% 
      hc_add_series(name="PM10" ,data = alldata_20072016YPM10$PM10)%>%
      hc_add_series(name="PM2.5" ,data = alldata_20072016YPM2.5$PM2.5)
  })
  ############################################################################################
  dataOutput <- reactive({
    subset(alldata_20072016B, date == input$date1 )
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() 
  })
  
  observeEvent(dataOutput(), {
    
    mapProxy <- leafletProxy("map", session)
    
    clearControls(mapProxy)
    clearMarkers(mapProxy)
    
    dataOutput <-  subset(alldata_20072016B,date == input$date1  )
    bins <- c(0, 15, 35, 54,150,250, Inf)
    cPal <- colorBin(palette = c("#00ff00","#ffff00","#FFA500","#ff0000","#963296","#7d0023"),
                     domain = alldata_20072016B$PM2.5, bins = bins)
    mapProxy %>%
      addControl(h1(input$date1), position = "topright") %>%
      addCircleMarkers(data=dataOutput,lng=dataOutput$TWD97Lon,lat=dataOutput$TWD97Lat, 
                       radius=13,stroke=FALSE, fillOpacity = 0.9,
                       fillColor = ~cPal(dataOutput$PM2.5), 
                       label =~as.character(dataOutput$PM2.5), 
                       popup = ~as.character(dataOutput$site))%>% 
      addLegend("bottomright", pal = cPal, values =alldata_20072016B$PM2.5,title = "PM2.5",
                labFormat = labelFormat(suffix = " "),opacity = 1)
    
  })
  
}
shinyApp(ui, server)
