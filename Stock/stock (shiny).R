# This script creates a web server on shiny about stock's performance

lapply(c("shiny","shinythemes","plotly","DT","quantmod"),require,character.only = TRUE)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  title="Stock Performance",
  
  hr(),
  
  fluidRow(
    column(3,
           textInput(
             inputId = "name",
             label = "Stock name",
             value = "GOOG" # the default stock is Google
           )
    ),
    column(4,
           dateRangeInput(
             inputId = "range",
             label = "Date Range Input: yyyy-mm-dd",
             start = Sys.Date()-365,
             end = Sys.Date()
           )
    ),
    
    hr(),
    
    hr(),
    
    hr(),
    
    plotlyOutput("candlestick"), # candlestick chart
    
    hr(),
    
    plotlyOutput("trend"), # trend
    
    hr(),
    
    dataTableOutput("table"),
    
    downloadButton("download","Download")
    
  )
)
  
server <- function(input,output){
  
  df <- reactive({
    data <- as.data.frame(getSymbols(Symbols = input$name,env = NULL,from = input$range)) # Extract data 
    df <- data.frame(rownames(data),data) # add back dates as the first column
    colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adj.Close")
    df <- na.omit(df) # Remove NA data
    df
  })
  
  output$candlestick <- renderPlotly({
    df <- df()
    candlestick <- df %>%
      plot_ly(x=~df$Date,type = "candlestick", # candlestick chart from plotly
              open=~df$Open,close=~df$Adj.Close,
              high=~df$High,low=df$Low)%>%
      layout(
        paper_bgcolor='transparent', 
        plot_bgcolor='transparent',
        title = sprintf("%s's Candlestick Chart",input$name),
        legend = list(x = 0.1, y = 0.9),
        xaxis=list(title="Date"),
        yaxis=list(title="Price ($)")
      )
  })
  
  output$trend <- renderPlotly({
    df <- df()
    trend <- df %>% 
      plot_ly(type = 'scatter', mode = 'lines')%>% # scatter plot with lines
      add_lines(x=~df$Date, y=~df$High, name="High",line=list(color="#33CFA5"))%>%
      add_lines(x=~df$Date, y=~df$Low, name="Low",line=list(color="#F06A6A"))%>%
      add_lines(x=~df$Date, y=~df$Adj.Close, name="Adj.Close",line=list(color="#3462d6"))%>%
      layout(
        paper_bgcolor='transparent', 
        plot_bgcolor='transparent',
        title = sprintf("%s's Trend",input$name),
        legend = list(x = 0.1, y = 0.9),
        xaxis=list(
          rangeslider = list(type = "date"),
          title="Date"),
        yaxis=list(title="Price ($)")
      )
  })
  
  output$table <- renderDataTable({
    datatable(df(),rownames = FALSE)
  })
  
  output$download <- downloadHandler(
    filename <- function(){
      paste(input$name,".csv",sep = "")
    },
    content <- function(file){
      write.csv(df(),file,row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)