# Import data
df <- read.csv(file = "UKCoronaVirusData.csv", stringsAsFactors = FALSE)
df2 <- read.csv(file="UKCOVID-19_test_deaths.csv", stringsAsFactors = FALSE)
df3 <- read.csv(file = "UKCOVID-19_deaths.csv", stringsAsFactors = FALSE)

# Change the column names 
if (length(colnames(df)[-1:-7])!=as.numeric(Sys.Date()-as.Date("2020-03-07"))){
  colnames(df)[-1:-6] <- as.character(seq(as.Date("2020-03-07"), Sys.Date()-1, by = 1))
}else{
  colnames(df)[-1:-6] <- as.character(seq(as.Date("2020-03-07"), Sys.Date(), by = 1))
}
#colnames(df)

# Change the rownames
df2$Date <- as.Date(df2$Date, format="%d/%m/%Y")
rownames(df2) <- df2$Date
df3$Date <- as.Date(df3$Date, format="%d/%m/%Y")
rownames(df3) <- df3$Date
# Factor NHS Region
df$NHSCategory <- factor(df$NHSCategory)

# Create a new dataframe (UKCov) without Mar5 to 7
UKCoV <- df[,-c(6)]

# Change from wide to long
library(tidyr)
UKCoVLong <- gather(UKCoV, Date, NewConfCases, "2020-03-07":colnames(df)[ncol(df)], factor_key = FALSE)

# Sort the data according to UTLA alphabets
UKCoVLong <- UKCoVLong[order(UKCoVLong$Source),]
rownames(UKCoVLong) <- c(1:dim(UKCoVLong)[1])
UKCoVLong$Date <- as.Date(UKCoVLong$Date)

# Interactive function for visualization ----------------------------------
library(shiny)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(xts)
library(dplyr)


server <- function(input, output, session) {

# UK ----------------------------------------------------------------------

  # Reactive dataframe based on input$date
  UKData <- reactive({
    UKCoVLong%>%
      filter(Date==input$date)
  })
  
  # Static Map
  output$UK <- renderLeaflet({
    leaflet(data = UKCoVLong)%>%
      addTiles() %>%
      addResetMapButton()%>%
      addProviderTiles(providers[["CartoDB.DarkMatter"]]) %>%
      setView(lng=1.5694517, lat=55.617083, zoom = 6)%>%
      clearShapes()%>%
      addCircles(data = UKCoVLong[UKCoVLong$Date=="2020-03-07",],lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Dynamic Map
  observe({
    leafletProxy("UK", data = UKData())%>%
      clearShapes()%>%
      addCircles(lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Cumulative Confirmed Totals
  output$Total <- renderText({
    sum(UKData()$NewConfCases, na.rm = TRUE)
  })
  
  # Daily Difference
  output$diff <- renderText({
    if (as.character(input$date)!="2020-03-07"){
      Diff <- sum(UKCoV[as.character(input$date)], na.rm = TRUE) - sum(UKCoV[as.character(input$date-1)], na.rm = TRUE)
      if (Diff > 0){
        paste("<font color=\"#FF0000\"><b>", "⬆", as.character(Diff),"</b></font>")
      }else if (Diff < 0){
        paste("<font color=\"#00FF00\"><b>", "⬇", as.character(abs(Diff)),"</b></font>")
      }else{
        ""
      }
    }else{
      ""
    }  
  })
  
  # Trend plot
  output$trend <- renderPlotly({
    
    if (input$date!="2020-03-07"){
      total <- colSums(UKCoV[,c(which(colnames(UKCoV)=='2020-03-07'):which(colnames(UKCoV)==as.character(input$date)))], na.rm = TRUE)
      
      data <- data.frame('date' = format(as.Date(names(total)), format="%Y-%m-%d"),
                         'total'=total, 
                         'TotalDeaths' = df2$TotalDeaths[10:which(rownames(df2)==as.character(input$date))])
      
      fig <- plot_ly(data = data, x = ~date, y = ~total, 
                     name = 'Total Confirmed Cases', 
                     type = 'scatter', 
                     mode = 'lines', 
                     line = list(color = 'rgb(255,0,0)'),
                     width = 450,
                     height = 300)%>%
        add_trace(data = data, x =~date, y=~TotalDeaths, name = 'Total Deaths', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22, 96, 167)'))
      
      fig <- fig %>% 
        layout(
               xaxis = list(title = "", tickangle=300),
               yaxis = list (title = ""),
               autosize = F)%>% 
        layout(legend = list(orientation = "h", xanchor = "center", x = 0.45, y = 1.2))
      fig
    }else{
    }
  })
  
  # Number of People Tested
  output$CumulativeOV <- renderPlotly({
    plot_ly(df2, x = ~Date, y = ~NumNegative, type = 'bar', name = 'Negative', marker = list(color = 'lime'))%>%
      add_trace(y= ~NumPositive, name = 'Positive', marker = list(color = 'red'))%>%
      layout(title = 'Cumulative UK Number of People Tested',
             yaxis = list(title = 'Number of People Tested'),
             barmode = 'stack',
             legend = list(orientation = "h", xanchor = "center", x = 0.48, y = 1.03)
             )
  })
  
  output$DailyOV <- renderPlotly({
    plot_ly(df2, x = ~Date, y = ~DiffNumNegative, type = 'bar', name = 'Negative', marker = list(color = 'lime'))%>%
      add_trace(y= ~DiffNumPositive, name = 'Positive', marker = list(color = 'red'))%>%
      layout(title = 'Daily UK Number of People Tested',
             yaxis = list(title = 'Number of People Tested'),
             barmode = 'stack',
             legend = list(orientation = "h", xanchor = "center", x = 0.48, y = 1.03)
      )
  })
  
  # Number of Deaths
  output$CumulativeDeaths <- renderPlotly({
    plot_ly(df3, x = ~Date, y = ~Wales, type = 'bar', name = 'Wales', marker = list(color = 'red'))%>%
      add_trace(y = ~Scotland, name = 'Scotland', marker = list(color = 'blue'))%>%
      add_trace(y = ~NI, name = 'Northern Ireland', marker = list(color = 'lime'))%>%
      add_trace(y = ~England, name = 'England', marker = list(color = 'cornsilk'))%>%
      layout(title = 'Cumulative Number of Deaths in UK due to Coronavirus',
             yaxis = list(title = 'Number of Deaths'),
             barmode = 'stack',
             legend = list(orientation = "h", xanchor = "center", x = 0.48, y = 1.03)
             )
  })
  
  output$DailyDeaths <- renderPlotly({
    plot_ly(df3, x = ~Date, y = ~DiffWales, type = 'bar', name = 'Wales', marker = list(color = 'red'))%>%
      add_trace(y = ~DiffScotland, name = 'Scotland', marker = list(color = 'blue'))%>%
      add_trace(y = ~DiffNI, name = 'Northern Ireland', marker = list(color = 'lime'))%>%
      add_trace(y = ~DiffEngland, name = 'England', marker = list(color = 'cornsilk'))%>%
      layout(title = 'Daily Number of Deaths in UK due to Coronavirus',
             yaxis = list(title = 'Number of Deaths'),
             barmode = 'stack',
             legend = list(orientation = "h", xanchor = "center", x = 0.48, y = 1.03)
             )
  })

# England -----------------------------------------------------------------

  England <- UKCoVLong%>%filter(Countries=="England")
  
  EngData <- reactive({
    England%>%
      filter(Date==input$date4England)
  })
  
  # Static map
  output$England <- renderLeaflet({
    leaflet(data = England)%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers[["CartoDB.DarkMatter"]])%>%
      setView(lng=1.5694517, lat=53.017083, zoom = 7)%>%
      clearShapes()%>%
      addCircles(data = England[England$Date=="2020-03-07",],lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })

  # Dynamic Map
  observe({
    leafletProxy("England", data = EngData())%>%
      clearShapes()%>%
      addCircles(lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Cumulative Confirmed Totals
  output$Eng <- renderText({
    Eng <- sum(EngData()$NewConfCases, na.rm = TRUE)
    Eng
  })
  
  # Daily Difference
  output$EngDiff <- renderText({
    if (input$date4England!="2020-03-07"){
      Diff <- sum(EngData()$NewConfCases, na.rm = TRUE) - sum(England[England$Date==as.character(as.Date(input$date4England)-1),]$NewConfCases, na.rm = TRUE)
      
      if (Diff > 0){
        paste("<font color=\"#FF0000\"><b>", "⬆", as.character(Diff),"</b></font>")
      }else if (Diff < 0){
        paste("<font color=\"#00FF00\"><b>", "⬇", as.character(abs(Diff)),"</b></font>")
      }else{
        ""
      }
    }else{
      ""
    }  
  })

  # Rank table
  output$EnglandTable <- DT::renderDataTable({
    DT::datatable(setNames(head(EngData()[order(EngData()$NewConfCases, decreasing = TRUE),c(-2:-6)],5), c("Local Authority", 'Cumulative Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))
  })
  
# Northern Ireland --------------------------------------------------------

  NI <- UKCoVLong%>%filter(Countries=="Northern Ireland")
  
  NIData <- reactive({
    NI%>%
      filter(Date==input$date4NI)
  })
  
  # Static map
  output$NI <- renderLeaflet({
    leaflet(data = NI)%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers[["CartoDB.DarkMatter"]])%>%
      setView(lng=-6.5625153, lat=54.6664821, zoom = 9)%>%
      clearShapes()%>%
      addCircles(data = NI[NI$Date=="2020-03-07",],lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Dynamic Map
  observe({
    leafletProxy("NI", data = NIData())%>%
      clearShapes()%>%
      addCircles(lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Cumulative Confirmed Totals
  output$NIreland <- renderText({
    NIreland <- sum(NIData()$NewConfCases, na.rm = TRUE)
    NIreland
  })
  
  # Daily Difference
  output$NIDiff <- renderText({
    if (input$date4NI!="2020-03-07"){
      Diff <- sum(NIData()$NewConfCases, na.rm = TRUE) - sum(NI[NI$Date==as.character(as.Date(input$date4NI)-1),]$NewConfCases, na.rm = TRUE)
      
      if (Diff > 0){
        paste("<font color=\"#FF0000\"><b>", "⬆", as.character(Diff),"</b></font>")
      }else if (Diff < 0){
        paste("<font color=\"#00FF00\"><b>", "⬇", as.character(abs(Diff)),"</b></font>")
      }else{
        ""
      }
    }else{
      ""
    }  
  })
  
  # Rank table
  output$NITable <- DT::renderDataTable({
    DT::datatable(setNames(head(NIData()[order(NIData()$NewConfCases, decreasing = TRUE),c(-2:-6)],5), c("Local Government District", 'Cumulative Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))
  })
  
# Scotland ----------------------------------------------------------------

  Scotland <- UKCoVLong%>%filter(Countries=="Scotland")
  
  ScotData <- reactive({
    Scotland%>%
      filter(Date==input$date4Scotland)
  })
  
  # Static map
  output$Scotland <- renderLeaflet({
    leaflet(data = Scotland)%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers[["CartoDB.DarkMatter"]])%>%
      setView(lng=0.188267, lat=57.853251, zoom = 6)%>%
      clearShapes()%>%
      addCircles(data = Scotland[Scotland$Date=="2020-03-07",],lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })

  # Dynamic Map
  observe({
    leafletProxy("Scotland", data = ScotData())%>%
      clearShapes()%>%
      addCircles(lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Cumulative Confirmed Totals
  output$SL <- renderText({
    SL <- sum(ScotData()$NewConfCases, na.rm = TRUE)
    SL
  })
  
  # Daily Difference
  output$SLDiff <- renderText({
    if (input$date4Scotland!="2020-03-07"){
      Diff <- sum(ScotData()$NewConfCases, na.rm = TRUE) - sum(Scotland[Scotland$Date==as.character(as.Date(input$date4Scotland)-1),]$NewConfCases, na.rm = TRUE)
      
      if (Diff > 0){
        paste("<font color=\"#FF0000\"><b>", "⬆", as.character(Diff),"</b></font>")
      }else if (Diff < 0){
        paste("<font color=\"#00FF00\"><b>", "⬇", as.character(abs(Diff)),"</b></font>")
      }else{
        ""
      }
    }else{
      ""
    }  
  })

  # Rank table
  output$ScotlandTable <- DT::renderDataTable({
    DT::datatable(setNames(head(ScotData()[order(ScotData()$NewConfCases, decreasing = TRUE),c(-2:-6)],5), c("Health Board", 'Cumulative Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))
  })
  
# Wales -------------------------------------------------------------------

  Wales <- UKCoVLong%>%filter(Countries=="Wales")
  
  WalesData <- reactive({
    Wales%>%
      filter(Date==input$date4Wales)
  })
  
  # Static map
  output$Wales <- renderLeaflet({
    leaflet(data = Wales)%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers[["CartoDB.DarkMatter"]])%>%
      setView(lng=-2.2203027, lat=52.4040065, zoom = 8)%>%
      clearShapes()%>%
      addCircles(data = Wales[Wales$Date=="2020-03-19",],lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Dynamic Map
  observe({
    leafletProxy("Wales", data = WalesData())%>%
      clearShapes()%>%
      addCircles(lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Cumulative Confirmed Totals
  output$wales <- renderText({
    wales <- sum(WalesData()$NewConfCases, na.rm = TRUE)
    wales
  })
  
  # Daily Difference
  output$WalesDiff <- renderText({
    if (input$date4Wales!="2020-03-07"){
      Diff <- sum(WalesData()$NewConfCases, na.rm = TRUE) - sum(Wales[Wales$Date==as.character(as.Date(input$date4Wales)-1),]$NewConfCases, na.rm = TRUE)
      
      if (Diff > 0){
        paste("<font color=\"#FF0000\"><b>", "⬆", as.character(Diff),"</b></font>")
      }else if (Diff < 0){
        paste("<font color=\"#00FF00\"><b>", "⬇", as.character(abs(Diff)),"</b></font>")
      }else{
        ""
      }
    }else{
      ""
    }  
  })

  # Rank table
  output$WalesTable <- DT::renderDataTable({
    DT::datatable(setNames(head(WalesData()[order(WalesData()$NewConfCases, decreasing = TRUE),c(-2:-6)],5), c("Health Board", 'Cumulative Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))
  })
  
# East of England ---------------------------------------------------------

  EastEngland <- UKCoVLong%>%filter(NHSCategory=="East of England")

  EastEnglandData <- reactive({
    EastEngland%>%
      filter(Date==input$date4EastEngland)
  })

  # Static map
  output$EastEngland <- renderLeaflet({
    leaflet(data = EastEngland)%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers[["CartoDB.DarkMatter"]])%>%
      setView(lng=1.6927, lat=52.1911, zoom = 8)%>%
      clearShapes()%>%
      addCircles(data = EastEngland[EastEngland$Date=="2020-03-07",],lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })

  # Dynamic Map
  observe({
    leafletProxy("EastEngland", data = EastEnglandData())%>%
      clearShapes()%>%
      addCircles(lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })

  # Cumulative Confirmed Totals
  output$eastengland <- renderText({
    eastengland <- sum(EastEnglandData()$NewConfCases, na.rm = TRUE)
    eastengland
  })

  # Daily Difference
  output$EastEnglandDiff <- renderText({
    if (input$date4EastEngland!="2020-03-07"){
      Diff <- sum(EastEnglandData()$NewConfCases, na.rm = TRUE) - sum(EastEngland[EastEngland$Date==as.character(as.Date(input$date4EastEngland)-1),]$NewConfCases, na.rm = TRUE)

      if (Diff > 0){
        paste("<font color=\"#FF0000\"><b>", "⬆", as.character(Diff),"</b></font>")
      }else if (Diff < 0){
        paste("<font color=\"#00FF00\"><b>", "⬇", as.character(abs(Diff)),"</b></font>")
      }else{
        ""
      }
    }else{
      ""
    }
  })

  # Rank table
  output$EastEngTable <- DT::renderDataTable({
    DT::datatable(setNames(head(EastEnglandData()[order(EastEnglandData()$NewConfCases, decreasing = TRUE),c(-2:-6)],5), c("Local Authority", 'Cumulative Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))
  })
  
# London ------------------------------------------------------------------

  London <- UKCoVLong%>%filter(NHSCategory=="London")

  LondonData <- reactive({
    London%>%
      filter(Date==input$date4London)
  })

  # Static map
  output$London <- renderLeaflet({
    leaflet(data = London)%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers[["CartoDB.DarkMatter"]])%>%
      setView(lng=0.181678, lat=51.4785582, zoom = 10)%>%
      clearShapes()%>%
      addCircles(data = London[London$Date=="2020-03-07",],lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })

  # Dynamic Map
  observe({
    leafletProxy("London", data = LondonData())%>%
      clearShapes()%>%
      addCircles(lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })

  # Cumulative Confirmed Totals
  output$london <- renderText({
    london <- sum(LondonData()$NewConfCases, na.rm = TRUE)
    london
  })

  # Daily Difference
  output$LondonDiff <- renderText({
    if (input$date4London!="2020-03-07"){
      Diff <- sum(LondonData()$NewConfCases, na.rm = TRUE) - sum(London[London$Date==as.character(as.Date(input$date4London)-1),]$NewConfCases, na.rm = TRUE)

      if (Diff > 0){
        paste("<font color=\"#FF0000\"><b>", "⬆", as.character(Diff),"</b></font>")
      }else if (Diff < 0){
        paste("<font color=\"#00FF00\"><b>", "⬇", as.character(abs(Diff)),"</b></font>")
      }else{
        ""
      }
    }else{
      ""
    }
  })

  # Rank table
  output$LondonTable <- DT::renderDataTable({
    DT::datatable(setNames(head(LondonData()[order(LondonData()$NewConfCases, decreasing = TRUE),c(-2:-6)],5), c("Local Authority", 'Cumulative Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))
  })
  
# Midlands ----------------------------------------------------------------
  
  Midlands <- UKCoVLong%>%filter(NHSCategory=="Midlands")

  MidlandsData <- reactive({
    Midlands%>%
      filter(Date==input$date4Midlands)
  })

  # Static map
  output$Midlands <- renderLeaflet({
    leaflet(data = Midlands)%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers[["CartoDB.DarkMatter"]])%>%
      setView(lng=-0.8718, lat=52.6449, zoom = 9)%>%
      clearShapes()%>%
      addCircles(data = Midlands[Midlands$Date=="2020-03-07",],lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })

  # Dynamic Map
  observe({
    leafletProxy("Midlands", data = MidlandsData())%>%
      clearShapes()%>%
      addCircles(lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })

  # Cumulative Confirmed Totals
  output$midlands <- renderText({
    midlands <- sum(MidlandsData()$NewConfCases, na.rm = TRUE)
    midlands
  })

  # Daily Difference
  output$MidlandsDiff <- renderText({
    if (input$date4Midlands!="2020-03-07"){
      Diff <- sum(MidlandsData()$NewConfCases, na.rm = TRUE) - sum(Midlands[Midlands$Date==as.character(as.Date(input$date4Midlands)-1),]$NewConfCases, na.rm = TRUE)

      if (Diff > 0){
        paste("<font color=\"#FF0000\"><b>", "⬆", as.character(Diff),"</b></font>")
      }else if (Diff < 0){
        paste("<font color=\"#00FF00\"><b>", "⬇", as.character(abs(Diff)),"</b></font>")
      }else{
        ""
      }
    }else{
      ""
    }
  })

  # Rank table
  output$MidlandsTable <- DT::renderDataTable({
    DT::datatable(setNames(head(MidlandsData()[order(MidlandsData()$NewConfCases, decreasing = TRUE),c(-2:-6)],5), c("Local Authority", 'Cumulative Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))
  })
  
# North East and Yorkshire ------------------------------------------------

  NorthEast <- UKCoVLong%>%filter(NHSCategory=="North East and Yorkshire")

  NorthEastData <- reactive({
    NorthEast%>%
      filter(Date==input$date4NorthEast)
  })

  # Static map
  output$NorthEast <- renderLeaflet({
    leaflet(data = NorthEast)%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers[["CartoDB.DarkMatter"]])%>%
      setView(lng=-0.6280, lat=54.3456, zoom = 8)%>%
      clearShapes()%>%
      addCircles(data = NorthEast[NorthEast$Date=="2020-03-07",],lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })

  # Dynamic Map
  observe({
    leafletProxy("NorthEast", data = NorthEastData())%>%
      clearShapes()%>%
      addCircles(lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })

  # Cumulative Confirmed Totals
  output$northeast <- renderText({
    northeast <- sum(NorthEastData()$NewConfCases, na.rm = TRUE)
    northeast
  })

  # Daily Difference
  output$NorthEastDiff <- renderText({
    if (input$date4NorthEast!="2020-03-07"){
      Diff <- sum(NorthEastData()$NewConfCases, na.rm = TRUE) - sum(NorthEast[NorthEast$Date==as.character(as.Date(input$date4NorthEast)-1),]$NewConfCases, na.rm = TRUE)

      if (Diff > 0){
        paste("<font color=\"#FF0000\"><b>", "⬆", as.character(Diff),"</b></font>")
      }else if (Diff < 0){
        paste("<font color=\"#00FF00\"><b>", "⬇", as.character(abs(Diff)),"</b></font>")
      }else{
        ""
      }
    }else{
      ""
    }
  })

  # Rank table
  output$NorthEastTable <- DT::renderDataTable({
    DT::datatable(setNames(head(NorthEastData()[order(NorthEastData()$NewConfCases, decreasing = TRUE),c(-2:-6)],5), c("Local Authority", 'Cumulative Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))
  })
  
# North West --------------------------------------------------------------
  
  NorthWest <- UKCoVLong%>%filter(NHSCategory=="North West")
  
  NorthWestData <- reactive({
    NorthWest%>%
      filter(Date==input$date4NorthWest)
  })
  
  # Static map
  output$NorthWest <- renderLeaflet({
    leaflet(data = NorthWest)%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers[["CartoDB.DarkMatter"]])%>%
      setView(lng=-1.6280, lat=53.5456, zoom = 9)%>%
      clearShapes()%>%
      addCircles(data = NorthWest[NorthWest$Date=="2020-03-07",],lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Dynamic Map
  observe({
    leafletProxy("NorthWest", data = NorthWestData())%>%
      clearShapes()%>%
      addCircles(lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Cumulative Confirmed Totals
  output$northwest <- renderText({
    northwest <- sum(NorthWestData()$NewConfCases, na.rm = TRUE)
    northwest
  })
  
  # Daily Difference
  output$NorthWestDiff <- renderText({
    if (input$date4NorthWest!="2020-03-07"){
      Diff <- sum(NorthWestData()$NewConfCases, na.rm = TRUE) - sum(NorthWest[NorthWest$Date==as.character(as.Date(input$date4NorthWest)-1),]$NewConfCases, na.rm = TRUE)
      
      if (Diff > 0){
        paste("<font color=\"#FF0000\"><b>", "⬆", as.character(Diff),"</b></font>")
      }else if (Diff < 0){
        paste("<font color=\"#00FF00\"><b>", "⬇", as.character(abs(Diff)),"</b></font>")
      }else{
        ""
      }
    }else{
      ""
    }
  })

  # Rank table
  output$NorthWestTable <- DT::renderDataTable({
    DT::datatable(setNames(head(NorthWestData()[order(NorthWestData()$NewConfCases, decreasing = TRUE),c(-2:-6)],5), c("Local Authority", 'Cumulative Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))
  })
  
# South East --------------------------------------------------------------

  SouthEast <- UKCoVLong%>%filter(NHSCategory=="South East")
  
  SouthEastData <- reactive({
    SouthEast%>%
      filter(Date==input$date4SouthEast)
  })
  
  # Static map
  output$SouthEast <- renderLeaflet({
    leaflet(data = SouthEast)%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers[["CartoDB.DarkMatter"]])%>%
      setView(lng=0.4280, lat=51.2456, zoom = 9)%>%
      clearShapes()%>%
      addCircles(data = SouthEast[SouthEast$Date=="2020-03-07",],lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Dynamic Map
  observe({
    leafletProxy("SouthEast", data = SouthEastData())%>%
      clearShapes()%>%
      addCircles(lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Cumulative Confirmed Totals
  output$southeast <- renderText({
    southeast <- sum(SouthEastData()$NewConfCases, na.rm = TRUE)
    southeast
  })
  
  # Daily Difference
  output$SouthEastDiff <- renderText({
    if (input$date4SouthEast!="2020-03-07"){
      Diff <- sum(SouthEastData()$NewConfCases, na.rm = TRUE) - sum(SouthEast[SouthEast$Date==as.character(as.Date(input$date4SouthEast)-1),]$NewConfCases, na.rm = TRUE)
      
      if (Diff > 0){
        paste("<font color=\"#FF0000\"><b>", "⬆", as.character(Diff),"</b></font>")
      }else if (Diff < 0){
        paste("<font color=\"#00FF00\"><b>", "⬇", as.character(abs(Diff)),"</b></font>")
      }else{
        ""
      }
    }else{
      ""
    }
  })

  # Rank table
  output$SouthEastTable <- DT::renderDataTable({
    DT::datatable(setNames(head(SouthEastData()[order(SouthEastData()$NewConfCases, decreasing = TRUE),c(-2:-6)],5), c("Local Authority", 'Cumulative Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))
  })
  
# South West --------------------------------------------------------------

  SouthWest <- UKCoVLong%>%filter(NHSCategory=="South West")
  
  SouthWestData <- reactive({
    SouthWest%>%
      filter(Date==input$date4SouthWest)
  })
  
  # Static map
  output$SouthWest <- renderLeaflet({
    leaflet(data = SouthWest)%>%
      addTiles()%>%
      addResetMapButton()%>%
      addProviderTiles(providers[["CartoDB.DarkMatter"]])%>%
      setView(lng=-1.8280, lat=51.0456, zoom = 8)%>%
      clearShapes()%>%
      addCircles(data = SouthWest[SouthWest$Date=="2020-03-07",],lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Dynamic Map
  observe({
    leafletProxy("SouthWest", data = SouthWestData())%>%
      clearShapes()%>%
      addCircles(lng=~long, lat=~lat, weight = 1,
                 radius = ~NewConfCases*10, popup = ~paste0(Source, ":", NewConfCases), color = "red")
  })
  
  # Cumulative Confirmed Totals
  output$southwest <- renderText({
    southwest <- sum(SouthWestData()$NewConfCases, na.rm = TRUE)
    southwest
  })
  
  # Daily Difference
  output$SouthWestDiff <- renderText({
    if (input$date4SouthWest!="2020-03-07"){
      Diff <- sum(SouthWestData()$NewConfCases, na.rm = TRUE) - sum(SouthWest[SouthWest$Date==as.character(as.Date(input$date4SouthWest)-1),]$NewConfCases, na.rm = TRUE)
      
      if (Diff > 0){
        paste("<font color=\"#FF0000\"><b>", "⬆", as.character(Diff),"</b></font>")
      }else if (Diff < 0){
        paste("<font color=\"#00FF00\"><b>", "⬇", as.character(abs(Diff)),"</b></font>")
      }else{
        ""
      }
    }else{
      ""
    }
  })
      
  # Rank table
  output$SouthWestTable <- DT::renderDataTable({
    DT::datatable(setNames(head(SouthWestData()[order(SouthWestData()$NewConfCases, decreasing = TRUE),c(-2:-6)],5), c("Local Authority", 'Cumulative Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))
  })
  
# Tables ------------------------------------------------------------------

  output$dataset <- DT::renderDataTable({
    df <- UKCoV[c(1:5,ncol(UKCoV):6)] %>%
      filter(
        is.null(input$source) | Source %in% input$source,
        is.null(input$nhscategory) | NHSCategory %in% input$nhscategory,
        is.null(input$countries) | Countries %in% input$countries
      )
    DT::datatable(df,
        extensions = 'Buttons',
        
        options = list(
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'tB',
          buttons = c('copy', 'csv', 'excel'),
          pageLength = 182, 
          info = FALSE,
          lengthMenu = list(c(15, -1), c("15", "All"))
          
        ),
        
        class = "display"
                  )
  })
  
  output$dataset2 <- DT::renderDataTable({
    DT::datatable(df2[order(df2$Date, decreasing = TRUE),],
                  rownames = FALSE,
                  extensions = 'Buttons',
                  
                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    buttons = c('copy', 'csv', 'excel'),
                    pageLength = 200, 
                    info = FALSE,
                    lengthMenu = list(c(15, -1), c("15", "All"))
                    ),
                  class = "display"
    )
  })
  
  output$dataset3 <- DT::renderDataTable({
    DT::datatable(df3[order(df3$Date, decreasing = TRUE),],
                  rownames = FALSE,
                  extensions = 'Buttons',
                  
                  options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    buttons = c('copy', 'csv', 'excel'),
                    pageLength = 200, 
                    info = FALSE,
                    lengthMenu = list(c(15, -1), c("15", "All"))
                    ),
                  class = "display"
    )
  })
  
}

