library(readxl)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(tidyr)
library(xts)
library(dplyr)
library(DT)

# Import data
df <- data.frame(read_excel('UKCOVID-19Dataset.xlsx', sheet = 'ConfirmedCases'),stringsAsFactors = FALSE)
df2 <- data.frame(read_excel('UKCOVID-19Dataset.xlsx', sheet = 'Tests&Deaths'),stringsAsFactors = FALSE)
df3 <- data.frame(read_excel('UKCOVID-19Dataset.xlsx', sheet = 'Deaths'),stringsAsFactors = FALSE)
df4 <- data.frame(read_excel('UKCOVID-19Dataset.xlsx', sheet = 'NHSTrustDeaths'),stringsAsFactors = FALSE)
df5 <- data.frame(read_excel('UKCOVID-19Dataset.xlsx', sheet = 'NHSRegionDeaths'),stringsAsFactors = FALSE)

# Change the column names 
colnames(df4)[4] <- 'Up to 2020-03-01'
if (length(colnames(df)[-1:-7])!=as.numeric(Sys.Date()-as.Date("2020-03-07"))){
  colnames(df)[-1:-6] <- as.character(seq(as.Date("2020-03-07"), Sys.Date()-1, by = 1))
  colnames(df4)[-1:-4] <- as.character(seq(as.Date("2020-03-01"), Sys.Date()-1, by = 1))
  #colnames(df5)[-1:-2] <- as.character(seq(as.Date("2020-03-01"), Sys.Date()-1, by = 1))
}else{
  colnames(df)[-1:-6] <- as.character(seq(as.Date("2020-03-07"), Sys.Date(), by = 1))
  colnames(df4)[-1:-4] <- as.character(seq(as.Date("2020-03-01"), Sys.Date(), by = 1))
  #colnames(df5)[-1:-2] <- as.character(seq(as.Date("2020-03-01"), Sys.Date()-1, by = 1))
}

# Factor NHS Region
df$NHSCategory <- factor(df$NHSCategory)

# Create a new dataframe (UKCov) without Mar5 to 7
UKCoV <- df[,-c(6)]

# Change from wide to long
UKCoVLong <- gather(UKCoV, Date, NewConfCases, "2020-03-07":colnames(df)[ncol(df)], factor_key = FALSE)

# Sort the data according to UTLA alphabets
UKCoVLong <- UKCoVLong[order(UKCoVLong$Source),]
rownames(UKCoVLong) <- c(1:dim(UKCoVLong)[1])
UKCoVLong$Date <- as.Date(UKCoVLong$Date)

# Change the rownames
df2$Date <- as.Date(df2$Date, format="%d/%m/%Y")
rownames(df2) <- df2$Date
df3$Date <- as.Date(df3$Date, format="%d/%m/%Y")
rownames(df3) <- df3$Date
df5$Date <- as.Date(df5$Date, format="%d/%m/%Y")
rownames(df5) <- df5$Date

UKCoVLong <- cbind(UKCoVLong,"DNewConfCases" = 0)

# Calculate the daily confirmed cases for each local authority
for (i in 1:length(unique(UKCoVLong$Source))){
  tmp <- UKCoVLong[UKCoVLong$Source==unique(UKCoVLong$Source)[i],]
  
  for (j in 1:dim(tmp)[1]){
    rowname <- rownames(tmp)[j]
    
    if (is.na(tmp$NewConfCases[j])==TRUE){
      UKCoVLong[as.numeric(rowname),]$DNewConfCases <- NA
    }else{
      if (j == 1){
        if (tmp$NewConfCases[j]==0){
          UKCoVLong[as.numeric(rowname),]$DNewConfCases <- 0
        }
      }else{
        UKCoVLong[as.numeric(rowname),]$DNewConfCases <- tmp$NewConfCases[j]-tmp$NewConfCases[j-1]
      }
    }
  }
}

# Function for obtaining info about confirmed cases and deaths
Calculate <- function(Type, Time, Data, Place, Date, FilteredData){
  if (Type =='Cases'){
    # Overall
    if (Place=='UK'){
      Total <- Data[Data$Date==Date,]$NumPositive
      Diff <- Data[Data$Date==Date,]$DiffNumPositive
      # Constituent Countries and English NHS Regions
    }else{
      Total <- sum(FilteredData$NewConfCases, na.rm = TRUE)
      Diff <- Total - sum(Data[Data$Date==as.character(Date-1),]$NewConfCases, na.rm = TRUE)
    }
    
    if (Time == 'Cumulative'){
      if (Diff > 0){
        color <- 'red'
      }else if (Diff < 0){
        color <- 'green'
      }else{
        color <- 'black'
      }
      value = Total
      subtitle = tags$p("Total Confirmed Cases", style = "font-size: 130%;")
    }else{
      if (Diff > 0){
        value = paste0("⬆",Diff)
        color = 'red'
      }else if (Diff < 0){
        value = paste0("⬇",abs(Diff))
        color = 'green'
      }else{
        value = '0'
        color = 'black'
      }
      subtitle = tags$p("Daily Confirmed Cases", style = "font-size: 130%;")
    }
  }
  
  if (Type =='Deaths'){
      if (Place == 'UK'){
        Total <- Data$TotalDeaths[which(Data$Date==Date)]
        Diff <- Total - Data$TotalDeaths[which(Data$Date==as.character(Date-1))]
      }else if (Place %in% c("England", "Northern Ireland", "Scotland","Wales")){
        Total <- Data[Place][which(Data$Date==Date),]
        if (as.character(Date)!='2020-03-07'){
          Diff <- Total - Data[as.character(Place)][which(Data$Date==as.character(Date-1)),]
        }else{
          Diff <- Total
        }
      }else{
        Total <- Data[Place][which(Data$Date==Date),]
        #Total <- sum(Data[Data$NHSRegion==Place, which(colnames(Data)=='2020-03-07'):which(colnames(Data)==as.character(Date))], na.rm = TRUE)
        if (as.character(Date)!='2020-03-07'){
          Diff <- Total - Data[as.character(Place)][which(Data$Date==as.character(Date-1)),]
          #Diff <- Total - Data[Data$NHSRegion==Place, which(colnames(Data)==as.character(Date-1))]
        }else{
          Total <- 0
          Diff <- Total
        }
      }
      
      if (Time=='Cumulative'){
        if (Diff > 0){
          color <- 'red'
        }else if (Diff < 0){
          color <- 'green'
        }else{
          color <- 'black'
        }
        value = Total
        subtitle = tags$p("Total Deaths", style = "font-size: 130%;")
      }else{
        if (Diff > 0){
          value = paste0("⬆",Diff)
          color = 'red'
        }else if (Diff < 0){
          value = paste0("⬇",abs(Diff))
          color = 'green'
        }else{
          value = '0'
          color = 'black'
        }
        subtitle = tags$p("Daily Deaths", style = "font-size: 130%;")
      }
  }
  
  valueBox(value = tags$p(value, style = "font-size: 150%;"), subtitle = subtitle, color = color, width = 2)
}

# Function for initial map
InitialMap <- function(data, long, lat, zoom){
  leaflet(data = data)%>%
    addTiles() %>%
    addResetMapButton()%>%
    addProviderTiles(providers[["CartoDB.DarkMatter"]]) %>%
    setView(lng=long, lat=lat, zoom = zoom)%>%
    clearShapes()%>%
    addCircles(data = data[data$Date=="2020-03-07",],lng=~long, lat=~lat, weight = 1,
               radius = ~NewConfCases, popup = ~paste0(Source, ":", NewConfCases), color = "red")
}

# Function for updating map
UpdateMap <- function(Place, Data){
  leafletProxy(Place, data = Data)%>%
    clearShapes()%>%
    addCircles(lng=~long, lat=~lat, weight = 1,
               radius = ~NewConfCases, popup = ~paste0(Source, ":", NewConfCases), color = "red")
}

Trend <- function(Date, Place, ExtData, Type){
  if (Date!='2020-03-07'){
    if (Place=='UK'){
      Data <- UKCoV
    }
    
    if (Place %in% c("England", "Northern Ireland", "Scotland", "Wales")){
      Data <- UKCoV[UKCoV$Countries==Place,]
    }
    
    if (Place %in% c('East of England', 'London', 'Midlands', 'North East and Yorkshire', 'North West', 'South East', 'South West')){
      Data <- UKCoV[UKCoV$NHSCategory==Place,]
    }
    
    total <- colSums(Data[,c(which(colnames(Data)=='2020-03-07'):which(colnames(Data)==as.character(Date)))], na.rm = TRUE)
    
    if (Type == "NORMAL"){
      if (Place=='UK'){
        data <- data.frame('date' = format(as.Date(names(total)), format="%Y-%m-%d"),
                           'total'=total,
                           'TotalDeaths' = ExtData$TotalDeaths[10:which(rownames(ExtData)==as.character(Date))])
      }
      
      if (Place %in% c("England", "Northern Ireland", "Scotland", "Wales")){
        if (Place=='Northern Ireland'){
          Place <- 'NI'
        }
        
        data <- data.frame('date' = format(as.Date(names(total)), format="%Y-%m-%d"),
                           'total'=total, 
                           'TotalDeaths' = ExtData[Place][which(rownames(ExtData)=='2020-03-07'):which(rownames(ExtData)==as.character(Date)),])
      }
      
      if (Place %in% c('East of England', 'London', 'Midlands', 'North East and Yorkshire', 'North West', 'South East', 'South West')){
        if(Place == 'East of England'){
          Place <- 'EastOfEngland'
        }
        
        if (Place == 'North East and Yorkshire'){
          Place <- 'NorthEastAndYorkshire'
        }
        
        if (Place == 'North West'){
          Place <- 'NorthWest'
        }
        
        if (Place == 'South East'){
          Place <- 'SouthEast'
        }
        
        if (Place == 'South West'){
          Place <- 'SouthWest'
        }
        
        data <- data.frame('date' = format(as.Date(names(total)), format="%Y-%m-%d"),
                           'total'=total, 
                           'TotalDeaths' = ExtData[Place][which(rownames(ExtData)=='2020-03-07'):which(rownames(ExtData)==as.character(Date)),])
      }
      
      fig <- plot_ly(data = data, x = ~date, y = ~total, 
                     name = 'Total Confirmed Cases', 
                     type = 'scatter', 
                     mode = 'lines', 
                     line = list(color = 'rgb(255,0,0)'),
                     height = 320
      )%>%
        add_trace(data = data, x =~date, y=~TotalDeaths, name = 'Total Deaths', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22, 96, 167)'))
    }

    if (Type == 'LOG'){
      if (Place=='UK'){
        data <- data.frame('date' = format(as.Date(names(total)), format="%Y-%m-%d"),
                           'total'=log10(total),
                           'TotalDeaths' = log10(ExtData$TotalDeaths[10:which(rownames(ExtData)==as.character(Date))]))
      }
      
      if (Place %in% c("England", "Northern Ireland", "Scotland", "Wales")){
        if (Place=='Northern Ireland'){
          Place <- 'NI'
        }
        
        data <- data.frame('date' = format(as.Date(names(total)), format="%Y-%m-%d"),
                           'total'=log10(total), 
                           'TotalDeaths' = log10(ExtData[Place][which(rownames(ExtData)=='2020-03-07'):which(rownames(ExtData)==as.character(Date)),]))
      }
      
      if (Place %in% unique(UKCoV$NHSCategory)){
        if(Place == 'East of England'){
          Place <- 'EastOfEngland'
        }
        
        if (Place == 'North East and Yorkshire'){
          Place <- 'NorthEastAndYorkshire'
        }
        
        if (Place == 'North West'){
          Place <- 'NorthWest'
        }
        
        if (Place == 'South East'){
          Place <- 'SouthEast'
        }
        
        if (Place == 'South West'){
          Place <- 'SouthWest'
        }
        
        data <- data.frame('date' = format(as.Date(names(total)), format="%Y-%m-%d"),
                           'total'=log10(total), 
                           'TotalDeaths' = log10(ExtData[Place][which(rownames(ExtData)=='2020-03-07'):which(rownames(ExtData)==as.character(Date)),]))
      }
      
      fig <- plot_ly(data = data, x = ~date, y = ~total, 
                     name = 'Log of Total Confirmed Cases', 
                     type = 'scatter', 
                     mode = 'lines', 
                     line = list(color = 'rgb(255,0,0)'),
                     height = 320
      )%>%
        add_trace(data = data, x =~date, y=~TotalDeaths, name = 'Log of Total Deaths', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22, 96, 167)'))
    }
    
    fig <- fig %>% 
      layout(
        xaxis = list(title = "", tickangle=300),
        yaxis = list (title = ""),
        autosize = T)%>% 
      layout(legend = list(orientation = "h", xanchor = "center", x = 0.45, y = 1.2))%>%
      layout(paper_bgcolor = 'black')%>%
      layout(plot_bgcolor='black'
             # ,
             # annotations = list(x = c(as.Date('2020-03-23')), 
             #                    y = c(df2[df2$Date=='2020-03-23',]$NumPositive),
             #                    text = c('UK Lockdown'), 
             #                    xref = "x", yref = "y", 
             #                    font = list(color='white'),
             #                    showarrow = TRUE, arrowhead = 7, 
             #                    ax = 0, ay = -40)
      )
    fig
    
  }else{
    
  }
}

# Function for plotting daily cases and deaths
pDailyTrend <- function(Type, Date, Data, Place){
  if(Date!='2020-03-07'){
    if (Type == 'DailyCases'){
      if (Place=='UK'){
        data <- Data[c(which(Data$Date=='2020-03-07'):which(Data$Date==as.character(Date))),]
        
        fig <- plot_ly(data = data, x = ~Date, y = ~DiffNumPositive, 
                       name = 'Daily Confirmed Cases', 
                       type = 'bar',
                       color = I("red"),
                       #width = 430,
                       height = 300)%>%
          add_trace(data = data, x =~Date, y=~FiveDayAvg, name = '5-Day Average', type = 'scatter', mode = 'lines', line = list(color = 'rgb(255,165,0)'))
      }
      
      if (Place %in% unique(Data$Countries)){
        RawData <- Data[Data$Countries==Place,]
        total <- colSums(RawData[,c(which(colnames(RawData)=='2020-03-07'):which(colnames(RawData)==as.character(Date)))], na.rm = TRUE)
        
        Diff <- {}
        
        for (i in 1:length(total)){
          if (i!=1){
            Diff[i] <- total[i]-total[i-1]
          }else{
            Diff[i] <- NA
          }
        }
        
        data <- data.frame('date'=seq(as.Date('2020-03-07'), Date, by = 1),'diff'=Diff)#, 'FiveDayAvg' = FiveDayAvg)
        
        fig <- plot_ly(data = data, x = ~date, y = ~diff, 
                       name = 'Daily Confirmed Cases', 
                       type = 'bar',
                       color = I("red"),
                       #width = 430,
                       height = 300)
      }
      
      if (Place %in% c('East of England', 'London', 'Midlands', 'North East and Yorkshire', 'North West', 'South East', 'South West')){
        RawData <- Data[Data$NHSCategory==Place,]
        total <- colSums(RawData[,c(which(colnames(RawData)=='2020-03-07'):which(colnames(RawData)==as.character(Date)))], na.rm = TRUE)
        
        Diff <- {}
        
        for (i in 1:length(total)){
          if (i!=1){
            Diff[i] <- total[i]-total[i-1]
          }else{
            Diff[i] <- NA
          }
        }
        
        data <- data.frame('date'=seq(as.Date('2020-03-07'), Date, by = 1),'diff'=Diff)#, 'FiveDayAvg' = FiveDayAvg)
        
        fig <- plot_ly(data = data, x = ~date, y = ~diff, 
                       name = 'Daily Confirmed Cases', 
                       type = 'bar',
                       color = I("red"),
                       #width = 430,
                       height = 300)
      }
    }
    
    if (Type == 'DailyDeaths'){
      data <- Data[c(which(Data$Date=='2020-03-07'):which(Data$Date==as.character(Date))),]
      
      if (Place == 'UK'){
        fig <- plot_ly(data = data, x = ~Date, y = ~DiffDeaths, 
                       name = 'Daily Deaths', 
                       type = 'bar',
                       #width = 430,
                       height = 300)
      }
      
      if (Place %in% unique(UKCoV$Countries)){
        if (Place == 'Northern Ireland'){
          Place <- 'NI'
        }
        
        fig <- plot_ly(data = data, x = ~Date, y = data[,paste0('Diff', Place)], 
                       name = 'Daily Deaths', 
                       type = 'bar',
                       #width = 430,
                       height = 300)
      }

      if (Place %in% c('East of England', 'London', 'Midlands', 'North East and Yorkshire', 'North West', 'South East', 'South West')){
        if(Place == 'East of England'){
          Place <- 'EE'
        }
        
        if (Place == 'North East and Yorkshire'){
          Place <- 'NE'
        }
        
        if (Place == 'North West'){
          Place <- 'NW'
        }
        
        if (Place == 'South East'){
          Place <- 'SE'
        }
        
        if (Place == 'South West'){
          Place <- 'SW'
        }
        
        fig <- plot_ly(data = data, x = ~Date, y = data[,paste0('Diff', Place)], 
                       name = 'Daily Deaths', 
                       type = 'bar',
                       #width = 430,
                       height = 300)
      }
    }
    
    fig <- fig %>% 
      layout(
        xaxis = list(title = "", tickangle=300),
        yaxis = list (title = ""),
        autosize = T)%>% 
      layout(legend = list(orientation = "h", xanchor = "center", x = 0.45, y = 1.2))%>%
      layout(paper_bgcolor = 'black')%>%
      layout(plot_bgcolor='black')
    fig
    
  }else{
    
  }
  
}

server <- function(input, output, session){

# UK ----------------------------------------------------------------------

  # Reactive dataframe based on input$date
  UKData <- reactive({
    UKCoVLong%>%filter(Date==input$date)
  })
  
  output$cases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Cumulative', Data=df2, Place='UK', Date=input$date)
  })
  
  output$dailycases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Daily', Data=df2, Place='UK', Date=input$date)
  })
  
  output$deaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Cumulative', Data=df2, Place='UK', Date=input$date)
  })
  
  output$dailydeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Daily', Data=df2, Place='UK', Date=input$date)
  })
  
  # Static Map
  output$UK <- renderLeaflet({
    InitialMap(data = UKCoVLong, long = -2.2694517, lat = 55.617083, zoom = 5)
  })
  
  # Dynamic Map
  observe({
    UpdateMap(Place = 'UK', Data = UKData())
  })
  
  output$trend <- renderPlotly({
    Trend(input$date, "UK", df2, "NORMAL")
  })
  
  output$logtrend <- renderPlotly({
    Trend(input$date, "UK", df2, 'LOG')
  })
  
  output$pdailycases <- renderPlotly({
    pDailyTrend(Type = 'DailyCases', Date=input$date, Data=df2, Place='UK')
  })
  
  output$pdailydeaths <- renderPlotly({
    pDailyTrend(Type = 'DailyDeaths', Date=input$date, Data=df2, Place='UK')
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(setNames(head(UKData()[order(UKData()$NewConfCases, decreasing = TRUE),c(-2:-6,-8)],5), c("Local Authority", 'Total Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))%>%
      formatStyle("Local Authority", backgroundColor = 'black')%>%
      formatStyle('Total Confirmed Cases', backgroundColor = 'black')
  })
  

# England -----------------------------------------------------------------

  England <- UKCoVLong%>%filter(Countries=="England")
  # Reactive dataframe based on input$date
  EnglandData <- reactive({
    England%>%
      filter(Date==input$date4England)
  })
  
  output$Englandcases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Cumulative', Data=England, Place='England', Date=input$date4England, FilteredData = EnglandData())
  })
  
  output$Englanddailycases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Daily', Data=England, Place='England', Date=input$date4England, FilteredData = EnglandData())
  })
  
  output$Englanddeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Cumulative', Data=df3, Place='England', Date=input$date4England)
  })
  
  output$Englanddailydeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Daily', Data=df3, Place='England', Date=input$date4England)
  })
  
  # Static Map
  output$England <- renderLeaflet({
    InitialMap(data = England, long = -1.5694517, lat = 52.617083, zoom = 6)
  })
  
  # Dynamic Map
  observe({
    UpdateMap(Place = 'England', Data = EnglandData())
  })
  
  output$Englandtrend <- renderPlotly({
    Trend(input$date4England, "England", df3, "NORMAL")
  })
  
  output$Englandlogtrend <- renderPlotly({
    Trend(input$date4England, "England", df3, "LOG")
  })
  
  output$Englandpdailycases <- renderPlotly({
    pDailyTrend(Type = 'DailyCases', Date=input$date4England, Data=UKCoV, Place='England')
  })
  
  output$Englandpdailydeaths <- renderPlotly({
    pDailyTrend(Type = 'DailyDeaths', Date=input$date4England, Data=df3, Place='England')
  })
  
  output$Englandtable <- DT::renderDataTable({
    DT::datatable(setNames(head(EnglandData()[order(EnglandData()$NewConfCases, decreasing = TRUE),c(-2:-6,-8)],5), c("Local Authority", 'Total Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))%>%
      formatStyle("Local Authority", backgroundColor = 'black')%>%
      formatStyle('Total Confirmed Cases', backgroundColor = 'black')
  })
  
  output$EnglandTCC <- renderPlotly({
    
  })

  output$EnglandDCC <- renderPlotly({
    
  })

# East of England ---------------------------------------------------------

  EastEngland <- UKCoVLong%>%filter(NHSCategory=="East of England")
  # Reactive dataframe based on input$date
  EastEnglandData <- reactive({
    EastEngland%>%
      filter(Date==input$date4EastEngland)
  })
  
  output$EastEnglandcases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Cumulative', Data=EastEngland, Place='East of England', Date=input$date4EastEngland, FilteredData = EastEnglandData())
  })
  
  output$EastEnglanddailycases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Daily', Data=EastEngland, Place='East of England', Date=input$date4EastEngland, FilteredData = EastEnglandData())
  })
  
  output$EastEnglanddeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Cumulative', Data=df5, Place='EastOfEngland', Date=input$date4EastEngland)
  })

  output$EastEnglanddailydeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Daily', Data=df5, Place='EastOfEngland', Date=input$date4EastEngland)
  })
  
  # Static Map
  output$EastEngland <- renderLeaflet({
    InitialMap(data = EastEngland, long = 0.6927, lat = 52.1911, zoom = 8)
  })
  
  # Dynamic Map
  observe({
    UpdateMap(Place = 'EastEngland', Data = EastEnglandData())
  })
  
  output$EastEnglandtrend <- renderPlotly({
    Trend(input$date4EastEngland, "East of England", df5, "NORMAL")
  })

  output$EastEnglandlogtrend <- renderPlotly({
    Trend(input$date4EastEngland, "East of England", df5, "LOG")
  })

  output$EastEnglandpdailycases <- renderPlotly({
    pDailyTrend(Type = 'DailyCases', Date=input$date4EastEngland, Data=UKCoV, Place='East of England')
  })
  
  output$EastEnglandpdailydeaths <- renderPlotly({
    pDailyTrend(Type = 'DailyDeaths', Date=input$date4EastEngland, Data=df5, Place='East of England')
  })

  output$EastEnglandtable <- DT::renderDataTable({
    DT::datatable(setNames(head(EastEnglandData()[order(EastEnglandData()$NewConfCases, decreasing = TRUE),c(-2:-6,-8)],5), c("Local Authority", 'Total Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))%>%
      formatStyle("Local Authority", backgroundColor = 'black')%>%
      formatStyle('Total Confirmed Cases', backgroundColor = 'black')
  })

# London ------------------------------------------------------------------

  London <- UKCoVLong%>%filter(NHSCategory=="London")
  # Reactive dataframe based on input$date
  LondonData <- reactive({
    London%>%
      filter(Date==input$date4London)
  })
  
  output$Londoncases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Cumulative', Data=London, Place='London', Date=input$date4London, FilteredData = LondonData())
  })
  
  output$Londondailycases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Daily', Data=London, Place='London', Date=input$date4London, FilteredData = LondonData())
  })
  
  output$Londondeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Cumulative', Data=df5, Place='London', Date=input$date4London)
  })
  
  output$Londondailydeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Daily', Data=df5, Place='London', Date=input$date4London)
  })
  
  # Static Map
  output$London <- renderLeaflet({
    InitialMap(data = London, long=-0.101678, lat=51.4785582, zoom = 10)
  })
  
  # Dynamic Map
  observe({
    UpdateMap(Place = 'London', Data = LondonData())
  })
  
  output$Londontrend <- renderPlotly({
    Trend(input$date4London, "London", df5, "NORMAL")
  })
  
  output$Londonlogtrend <- renderPlotly({
    Trend(input$date4London, "London", df5, "LOG")
  })
  
  output$Londonpdailycases <- renderPlotly({
    pDailyTrend(Type = 'DailyCases', Date=input$date4London, Data=UKCoV, Place='London')
  })
  
  output$Londonpdailydeaths <- renderPlotly({
    pDailyTrend(Type = 'DailyDeaths', Date=input$date4London, Data=df5, Place='London')
  })
  
  output$Londontable <- DT::renderDataTable({
    DT::datatable(setNames(head(LondonData()[order(LondonData()$NewConfCases, decreasing = TRUE),c(-2:-6,-8)],5), c("Local Authority", 'Total Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))%>%
      formatStyle("Local Authority", backgroundColor = 'black')%>%
      formatStyle('Total Confirmed Cases', backgroundColor = 'black')
  })

# Midlands ----------------------------------------------------------------

  Midlands <- UKCoVLong%>%filter(NHSCategory=="Midlands")
  # Reactive dataframe based on input$date
  MidlandsData <- reactive({
    Midlands%>%
      filter(Date==input$date4Midlands)
  })
  
  output$Midlandscases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Cumulative', Data=Midlands, Place='Midlands', Date=input$date4Midlands, FilteredData = MidlandsData())
  })
  
  output$Midlandsdailycases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Daily', Data=Midlands, Place='Midlands', Date=input$date4Midlands, FilteredData = MidlandsData())
  })
  
  output$Midlandsdeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Cumulative', Data=df5, Place='Midlands', Date=input$date4Midlands)
  })
  
  output$Midlandsdailydeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Daily', Data=df5, Place='Midlands', Date=input$date4Midlands)
  })
  
  # Static Map
  output$Midlands <- renderLeaflet({
    InitialMap(data = Midlands, long=-1.3718, lat=52.6449, zoom = 8)
  })
  
  # Dynamic Map
  observe({
    UpdateMap(Place = 'Midlands', Data = MidlandsData())
  })
  
  output$Midlandstrend <- renderPlotly({
    Trend(input$date4Midlands, "Midlands", df5, "NORMAL")
  })
  
  output$Midlandslogtrend <- renderPlotly({
    Trend(input$date4Midlands, "Midlands", df5, "LOG")
  })
  
  output$Midlandspdailycases <- renderPlotly({
    pDailyTrend(Type = 'DailyCases', Date=input$date4Midlands, Data=UKCoV, Place='Midlands')
  })
  
  output$Midlandspdailydeaths <- renderPlotly({
    pDailyTrend(Type = 'DailyDeaths', Date=input$date4Midlands, Data=df5, Place='Midlands')
  })
  
  output$Midlandstable <- DT::renderDataTable({
    DT::datatable(setNames(head(MidlandsData()[order(MidlandsData()$NewConfCases, decreasing = TRUE),c(-2:-6,-8)],5), c("Local Authority", 'Total Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))%>%
      formatStyle("Local Authority", backgroundColor = 'black')%>%
      formatStyle('Total Confirmed Cases', backgroundColor = 'black')
  })

# North East and Yorkshire ------------------------------------------------

  NorthEast <- UKCoVLong%>%filter(NHSCategory=="North East and Yorkshire")
  # Reactive dataframe based on input$date
  NorthEastData <- reactive({
    NorthEast%>%
      filter(Date==input$date4NE)
  })

  output$NEcases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Cumulative', Data=NorthEast, Place='North East and Yorkshire', Date=input$date4NE, FilteredData = NorthEastData())
  })

  output$NEdailycases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Daily', Data=NorthEast, Place='North East and Yorkshire', Date=input$date4NE, FilteredData = NorthEastData())
  })

  output$NEdeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Cumulative', Data=df5, Place='NorthEastAndYorkshire', Date=input$date4NE)
  })

  output$NEdailydeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Daily', Data=df5, Place='NorthEastAndYorkshire', Date=input$date4NE)
  })

  # Static Map
  output$NE <- renderLeaflet({
    InitialMap(data = NorthEast, long=-1.6280, lat=54.3456, zoom = 7)
  })

  # Dynamic Map
  observe({
    UpdateMap(Place = 'NE', Data = NorthEastData())
  })

  output$NEtrend <- renderPlotly({
    Trend(input$date4NE, "North East and Yorkshire", df5, "NORMAL")
  })

  output$NElogtrend <- renderPlotly({
    Trend(input$date4NE, "North East and Yorkshire", df5, "LOG")
  })

  output$NEpdailycases <- renderPlotly({
    pDailyTrend(Type = 'DailyCases', Date=input$date4NE, Data=UKCoV, Place='North East and Yorkshire')
  })

  output$NEpdailydeaths <- renderPlotly({
    pDailyTrend(Type = 'DailyDeaths', Date=input$date4NE, Data=df5, Place='North East and Yorkshire')
  })

  output$NEtable <- DT::renderDataTable({
    DT::datatable(setNames(head(NorthEastData()[order(NorthEastData()$NewConfCases, decreasing = TRUE),c(-2:-6,-8)],5), c("Local Authority", 'Total Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))%>%
      formatStyle("Local Authority", backgroundColor = 'black')%>%
      formatStyle('Total Confirmed Cases', backgroundColor = 'black')
  })

# North West --------------------------------------------------------------

  NorthWest <- UKCoVLong%>%filter(NHSCategory=="North West")
  # Reactive dataframe based on input$date
  NorthWestData <- reactive({
    NorthWest%>%
      filter(Date==input$date4NW)
  })
  
  output$NWcases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Cumulative', Data=NorthWest, Place='North West', Date=input$date4NW, FilteredData = NorthWestData())
  })
  
  output$NWdailycases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Daily', Data=NorthWest, Place='North West', Date=input$date4NW, FilteredData = NorthWestData())
  })
  
  output$NWdeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Cumulative', Data=df5, Place='NorthWest', Date=input$date4NW)
  })
  
  output$NWdailydeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Daily', Data=df5, Place='NorthWest', Date=input$date4NW)
  })
  
  # Static Map
  output$NW <- renderLeaflet({
    InitialMap(data = NorthWest, long=-2.5280, lat=53.5456, zoom = 9)
  })
  
  # Dynamic Map
  observe({
    UpdateMap(Place = 'NW', Data = NorthWestData())
  })
  
  output$NWtrend <- renderPlotly({
    Trend(input$date4NW, "North West", df5, "NORMAL")
  })
  
  output$NWlogtrend <- renderPlotly({
    Trend(input$date4NW, "North West", df5, "LOG")
  })
  
  output$NWpdailycases <- renderPlotly({
    pDailyTrend(Type = 'DailyCases', Date=input$date4NW, Data=UKCoV, Place='North West')
  })
  
  output$NWpdailydeaths <- renderPlotly({
    pDailyTrend(Type = 'DailyDeaths', Date=input$date4NW, Data=df5, Place='North West')
  })
  
  output$NWtable <- DT::renderDataTable({
    DT::datatable(setNames(head(NorthWestData()[order(NorthWestData()$NewConfCases, decreasing = TRUE),c(-2:-6,-8)],5), c("Local Authority", 'Total Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))%>%
      formatStyle("Local Authority", backgroundColor = 'black')%>%
      formatStyle('Total Confirmed Cases', backgroundColor = 'black')
  })  
  

# South East --------------------------------------------------------------

  SouthEast <- UKCoVLong%>%filter(NHSCategory=="South East")
  # Reactive dataframe based on input$date
  SouthEastData <- reactive({
    SouthEast%>%
      filter(Date==input$date4SE)
  })
  
  output$SEcases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Cumulative', Data=SouthEast, Place='South East', Date=input$date4SE, FilteredData = SouthEastData())
  })
  
  output$SEdailycases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Daily', Data=SouthEast, Place='South East', Date=input$date4SE, FilteredData = SouthEastData())
  })
  
  output$SEdeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Cumulative', Data=df5, Place='SouthEast', Date=input$date4SE)
  })
  
  output$SEdailydeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Daily', Data=df5, Place='SouthEast', Date=input$date4SE)
  })
  
  # Static Map
  output$SE <- renderLeaflet({
    InitialMap(data = SouthEast, long=0.2280, lat=51.2456, zoom = 8)
  })
  
  # Dynamic Map
  observe({
    UpdateMap(Place = 'SE', Data = SouthEastData())
  })
  
  output$SEtrend <- renderPlotly({
    Trend(input$date4SE, "South East", df5, "NORMAL")
  })
  
  output$SElogtrend <- renderPlotly({
    Trend(input$date4SE, "South East", df5, "LOG")
  })
  
  output$SEpdailycases <- renderPlotly({
    pDailyTrend(Type = 'DailyCases', Date=input$date4SE, Data=UKCoV, Place='South East')
  })
  
  output$SEpdailydeaths <- renderPlotly({
    pDailyTrend(Type = 'DailyDeaths', Date=input$date4SE, Data=df5, Place='South East')
  })
  
  output$SEtable <- DT::renderDataTable({
    DT::datatable(setNames(head(SouthEastData()[order(SouthEastData()$NewConfCases, decreasing = TRUE),c(-2:-6,-8)],5), c("Local Authority", 'Total Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))%>%
      formatStyle("Local Authority", backgroundColor = 'black')%>%
      formatStyle('Total Confirmed Cases', backgroundColor = 'black')
  })
  
# South West --------------------------------------------------------------

  SouthWest <- UKCoVLong%>%filter(NHSCategory=="South West")
  # Reactive dataframe based on input$date
  SouthWestData <- reactive({
    SouthWest%>%
      filter(Date==input$date4SW)
  })
  
  output$SWcases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Cumulative', Data=SouthWest, Place='South West', Date=input$date4SW, FilteredData = SouthWestData())
  })
  
  output$SWdailycases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Daily', Data=SouthWest, Place='South West', Date=input$date4SW, FilteredData = SouthWestData())
  })
  
  output$SWdeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Cumulative', Data=df5, Place='SouthWest', Date=input$date4SW)
  })
  
  output$SWdailydeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Daily', Data=df5, Place='SouthWest', Date=input$date4SW)
  })
  
  # Static Map
  output$SW <- renderLeaflet({
    InitialMap(data = SouthWest, long=-2.8280, lat=51.0456, zoom = 8)
  })
  
  # Dynamic Map
  observe({
    UpdateMap(Place = 'SW', Data = SouthWestData())
  })
  
  output$SWtrend <- renderPlotly({
    Trend(input$date4SW, "South West", df5, "NORMAL")
  })
  
  output$SWlogtrend <- renderPlotly({
    Trend(input$date4SW, "South West", df5, "LOG")
  })
  
  output$SWpdailycases <- renderPlotly({
    pDailyTrend(Type = 'DailyCases', Date=input$date4SW, Data=UKCoV, Place='South West')
  })
  
  output$SWpdailydeaths <- renderPlotly({
    pDailyTrend(Type = 'DailyDeaths', Date=input$date4SW, Data=df5, Place='South West')
  })
  
  output$SWtable <- DT::renderDataTable({
    DT::datatable(setNames(head(SouthWestData()[order(SouthWestData()$NewConfCases, decreasing = TRUE),c(-2:-6,-8)],5), c("Local Authority", 'Total Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))%>%
      formatStyle("Local Authority", backgroundColor = 'black')%>%
      formatStyle('Total Confirmed Cases', backgroundColor = 'black')
  })
    
# Northern Ireland --------------------------------------------------------

  NI <- UKCoVLong%>%filter(Countries=="Northern Ireland")
  # Reactive dataframe based on input$date
  NIData <- reactive({
    NI%>%filter(Date==input$date4NI)
  })
  
  output$NIcases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Cumulative', Data=NI, Place='NI', Date=input$date4NI, FilteredData = NIData())
  })
  
  output$NIdailycases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Daily', Data=NI, Place='NI', Date=input$date4NI, FilteredData = NIData())
  })
  
  output$NIdeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Cumulative', Data=df3, Place='NI', Date=input$date4NI)
  })
  
  output$NIdailydeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Daily', Data=df3, Place='NI', Date=input$date4NI)
  })
  
  # Static Map
  output$NI <- renderLeaflet({
    InitialMap(data = NI, long = -6.5625153, lat = 54.6664821, zoom = 9)
  })
  
  # Dynamic Map
  observe({
    UpdateMap(Place = 'NI', Data = NIData())
  })
  
  output$NItrend <- renderPlotly({
    Trend(input$date4NI, "Northern Ireland", df3, "NORMAL")
  })
  
  output$NIlogtrend <- renderPlotly({
    Trend(input$date4NI, "Northern Ireland", df3, "LOG")
  })
  
  output$NIpdailycases <- renderPlotly({
    pDailyTrend(Type = 'DailyCases', Date=input$date4NI, Data=UKCoV, Place='Northern Ireland')
  })
  
  output$NIpdailydeaths <- renderPlotly({
    pDailyTrend(Type = 'DailyDeaths', Date=input$date4NI, Data=df3, Place='Northern Ireland')
  })
  
  output$NItable <- DT::renderDataTable({
    DT::datatable(setNames(head(NIData()[order(NIData()$NewConfCases, decreasing = TRUE),c(-2:-6,-8)],5), c("Local Government District", 'Total Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))%>%
      formatStyle("Local Government District", backgroundColor = 'black')%>%
      formatStyle('Total Confirmed Cases', backgroundColor = 'black')
  })

# Scotland ----------------------------------------------------------------

  Scotland <- UKCoVLong%>%filter(Countries=="Scotland")
  # Reactive dataframe based on input$date
  ScotlandData <- reactive({
    Scotland%>%filter(Date==input$date4Scotland)
  })
  
  output$Scotlandcases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Cumulative', Data=Scotland, Place='Scotland', Date=input$date4Scotland, FilteredData = ScotlandData())
  })
  
  output$Scotlanddailycases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Daily', Data=Scotland, Place='Scotland', Date=input$date4Scotland, FilteredData = ScotlandData())
  })
  
  output$Scotlanddeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Cumulative', Data=df3, Place='Scotland', Date=input$date4Scotland)
  })
  
  output$Scotlanddailydeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Daily', Data=df3, Place='Scotland', Date=input$date4Scotland)
  })
  
  # Static Map
  output$Scotland <- renderLeaflet({
    InitialMap(data = Scotland, long = -3.188267, lat = 57.853251, zoom = 6)
  })
  
  # Dynamic Map
  observe({
    UpdateMap(Place = 'Scotland', Data = ScotlandData())
  })
  
  output$Scotlandtrend <- renderPlotly({
    Trend(input$date4Scotland, "Scotland", df3, "NORMAL")
  })
  
  output$Scotlandlogtrend <- renderPlotly({
    Trend(input$date4Scotland, "Scotland", df3, "LOG")
  })
  
  output$Scotlandpdailycases <- renderPlotly({
    pDailyTrend(Type = 'DailyCases', Date=input$date4Scotland, Data=UKCoV, Place='Scotland')
  })
  
  output$Scotlandpdailydeaths <- renderPlotly({
    pDailyTrend(Type = 'DailyDeaths', Date=input$date4Scotland, Data=df3, Place='Scotland')
  })
  
  output$Scotlandtable <- DT::renderDataTable({
    DT::datatable(setNames(head(ScotlandData()[order(ScotlandData()$NewConfCases, decreasing = TRUE),c(-2:-6,-8)],5), c("Health Board", 'Total Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))%>%
      formatStyle("Health Board", backgroundColor = 'black')%>%
      formatStyle('Total Confirmed Cases', backgroundColor = 'black')
  })
    

# Wales -------------------------------------------------------------------

  Wales <- UKCoVLong%>%filter(Countries=="Wales")
  # Reactive dataframe based on input$date
  WalesData <- reactive({
    Wales%>%filter(Date==input$date4Wales)
  })
  
  output$Walescases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Cumulative', Data=Wales, Place='Wales', Date=input$date4Wales, FilteredData = WalesData())
  })
  
  output$Walesdailycases <- renderValueBox({
    Calculate(Type = 'Cases', Time='Daily', Data=Wales, Place='Wales', Date=input$date4Wales, FilteredData = WalesData()) 
  })

  output$Walesdeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Cumulative', Data=df3, Place='Wales', Date=input$date4Wales)
  })
  
  output$Walesdailydeaths <- renderValueBox({
    Calculate(Type = 'Deaths', Time='Daily', Data=df3, Place='Wales', Date=input$date4Wales)
  })
  
  # Static Map
  output$Wales <- renderLeaflet({
    InitialMap(data = Wales, long = -3.2203027, lat = 52.4040065, zoom = 8)
  })
  
  # Dynamic Map
  observe({
    UpdateMap(Place = 'Wales', Data = WalesData())
  })
  
  output$Walestrend <- renderPlotly({
    Trend(input$date4Wales, "Wales", df3, "NORMAL")
  })
  
  output$Waleslogtrend <- renderPlotly({
    Trend(input$date4Wales, "Wales", df3, "LOG")
  })
  
  output$Walespdailycases <- renderPlotly({
    pDailyTrend(Type = 'DailyCases', Date=input$date4Wales, Data=UKCoV, Place='Wales')
  })
  
  output$Walespdailydeaths <- renderPlotly({
    pDailyTrend(Type = 'DailyDeaths', Date=input$date4Wales, Data=df3, Place='Wales')
  })
  
  output$Walestable <- DT::renderDataTable({
    DT::datatable(setNames(head(WalesData()[order(WalesData()$NewConfCases, decreasing = TRUE),c(-2:-6,-8)],5), c("Health Board", 'Total Confirmed Cases')), rownames = FALSE, options = list(dom = 't', ordering=F))%>%
      formatStyle("Health Board", backgroundColor = 'black')%>%
      formatStyle('Total Confirmed Cases', backgroundColor = 'black')
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
                  rownames= FALSE,
                  
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
    )%>%formatStyle(colnames(df), backgroundColor = 'black')
    #%>%
    #mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #action <- DT::dataTableAjax(session, df, outputId = "dataset")
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
    )%>%formatStyle(colnames(df2), backgroundColor = 'black')
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
    )%>%formatStyle(colnames(df3), backgroundColor = 'black')
  })
  
  output$dataset4 <- DT::renderDataTable({
    df <- df4[c(1:3,ncol(df4):4)] %>%
      filter(
        is.null(input$NHSRegion) | NHSRegion %in% input$NHSRegion,
        is.null(input$HospitalCode) | Code %in% input$HospitalCode,
        is.null(input$HospitalName) | Name %in% input$HospitalName
      )
    DT::datatable(df,
                  extensions = 'Buttons',
                  rownames= FALSE,
                  
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
    )%>%formatStyle(colnames(df), backgroundColor = 'black')
    #%>%
    #mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #action <- DT::dataTableAjax(session, df, outputId = "dataset")
  })

# Search ------------------------------------------------------------------

  SearchDF <- reactive({
    UKCoVLong%>%
      filter(Source %in% input$search)
  })  
  
  output$SearchTCC <- renderPlotly({
   
    fig <- plot_ly(data = SearchDF())
    
    for (i in 1: length(input$search)){
     subdf <- SearchDF()%>%filter(Source==input$search[i])
     
     fig <- add_trace(fig, x= ~Date, y= ~NewConfCases, data = subdf, name = input$search[i], type = 'bar')
    }
    fig <- fig %>% 
      layout(
        title = 'Total Confirmed Cases', xaxis = list(title = 'Date', tickangle=300), yaxis = list (title = 'Number of confirmed cases'),
        autosize = T, legend = list(orientation = "h", xanchor = "center", x = 0.45, y = 1),
        paper_bgcolor = 'black',plot_bgcolor='black', barmode = 'group'
        )
    fig
  })  
  
  SearchNDF <- reactive({
    
  })
  
  output$SearchDCC <- renderPlotly({
    
    fig <- plot_ly(data = SearchDF())
    
    for (i in 1: length(input$search)){
      subdf <- SearchDF()%>%filter(Source==input$search[i])
      
      fig <- add_trace(fig, x= ~Date, y= ~DNewConfCases, data = subdf, name = input$search[i], type = 'bar')
    }

    fig <- fig %>% 
      layout(
        title = 'Daily Confirmed Cases', xaxis = list(title = 'Date', tickangle=300), yaxis = list (title = 'Number of confirmed cases'),
        autosize = T, legend = list(orientation = "h", xanchor = "center", x = 0.45, y = 1),
        paper_bgcolor = 'black',plot_bgcolor='black', barmode = 'group'
      )
    fig
  })  
  
}
