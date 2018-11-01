library(shiny)
library(plotly)

library(scales) 
RFT = function(w,i){ # w = weighting, i = array of price distribution
  range = (i-min(i))/(max(i)-min(i)) # range principle
  freq = (rank(i)-1)/(length(i)-1) # rank principle
  RFT = w*range+(1-w)*freq # RFT calculation
  RFTP = rescale(-RFT, to = c(1,7)) # rescale between 1 and 7
  mat=t(data.frame(Price=i,RFT=RFT,RFTP=RFTP,Range=range,Freq=freq)) # Create a matrix
  return(mat)
}

ui <- fluidPage(
  titlePanel("Range Frequency Theory"),

  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons(
        inputId = "dist",
        label = "Price Distribution",
        choices = c("Unimodal",
                    "Bimodal",
                    "Positively Skewed",
                    "Negatively Skewed")
      ),
      sliderInput(
        inputId = "w",
        label = "Weighting Parameter",
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.1,
        animate = animationOptions(
          interval = 300,
          loop = TRUE)
      )

    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Table",tableOutput("table")),
        tabPanel("RFT",plotlyOutput("rft")),
        tabPanel("RFTP",plotlyOutput("rftp"))
      )
      )
  )
)

server <- function (input,output){
  
  dataset <- reactive(
    switch(input$dist,
           "Unimodal" = c(28,42,46,49,52,55,58,61,64,68,82), # Unimodal
           "Bimodal" = c(28,31,34,38,42,55,68,72,76,79,82), # Bimodal
           "Positively Skewed" = c(28,29,31,33,36,39,43,48,55,65,82), # Positively Skewed
           "Negatively Skewed" = c(28,45,55,62,67,71,74,77,79,81,82) # Negatively Skewed
    )
  )
  
  df <- reactive(
    data.frame(t(round(RFT(input$w,dataset()),4)))
  )
  
  output$table <- renderTable({
    df()
  })

  output$rft <- renderPlotly({
    df <- df()
    rft <- plot_ly(df,x=~df$Price,y=~df$RFT,
                   type = "scatter",mode="lines+markers")%>%
      layout(title="RFT ~ Prices (£)",
             xaxis=list(title="Prices (£)"),
             yaxis=list(title="RFT"),
             plot_bgcolor='transparent',
             paper_bgcolor='transparent')
  })
  
  output$rftp <- renderPlotly({
    df <- df()
    rftp <- plot_ly(df,x=~df$Price,y=~df$RFTP,
                   type = "scatter",mode="lines+markers")%>%
      layout(title="RFTP ~ Prices (£)",
             xaxis=list(title="Prices (£)"),
             yaxis=list(title="RFTP"),
             plot_bgcolor='transparent',
             paper_bgcolor='transparent')
  })
  
}

shinyApp(ui = ui, server = server)