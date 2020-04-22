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
Note <- data.frame(read_excel('UKCOVID-19Dataset.xlsx', sheet = 'Note4UK'),stringsAsFactors = FALSE)
Log <- data.frame(read_excel('UKCOVID-19Dataset.xlsx', sheet = 'Log'),stringsAsFactors = FALSE)

# Change the column names
colnames(df4)[4] <- 'Up to 2020-03-01'
if (length(colnames(df)[-1:-7])!=as.numeric(Sys.Date()-as.Date("2020-03-07"))){
  colnames(df)[-1:-6] <- as.character(seq(as.Date("2020-03-07"), Sys.Date()-1, by = 1))
  colnames(df4)[-1:-4] <- as.character(seq(as.Date("2020-03-01"), Sys.Date()-1, by = 1))
}else{
  colnames(df)[-1:-6] <- as.character(seq(as.Date("2020-03-07"), Sys.Date(), by = 1))
  colnames(df4)[-1:-4] <- as.character(seq(as.Date("2020-03-01"), Sys.Date(), by = 1))
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
# colnames(df4)[4] <- 'Up to 2020-03-01'
# colnames(df4)[5:ncol(df4)] <- as.Date(colnames(df4)[5:ncol(df4)], format="%d/%m/%Y")

UKCoVLong <- cbind(UKCoVLong,"DNewConfCases" = 0)

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

#Time <- Sys.time()
Time <- '2020-04-22 22:00:00'
VersionNo <- "1.0.0"

ui <- navbarPage(
  inverse = TRUE,
  title = "UK COVID-19",
  windowTitle = "UK COVID-19 Update by FT",
  theme = shinytheme("cyborg"),

# UK ----------------------------------------------------------------------

  navbarMenu("UK",
             tabPanel("Overview",
                      # Confirmed cases and death counts
                      fluidRow(
                        tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                        valueBoxOutput('cases', width = 2),
                        valueBoxOutput('dailycases', width = 2),
                        valueBoxOutput('deaths', width = 2),
                        valueBoxOutput('dailydeaths', width = 2),
                        box(sliderInput(inputId = 'date', label = tags$p("Date", style = "font-size: 150%;"), min = min(unique(UKCoVLong$Date)),
                                        max = max(unique(UKCoVLong$Date)), value = max(unique(UKCoVLong$Date)),step = 1,animate = T),
                            width = 4, height = 120, background = 'black')),
                      # Map
                      fluidRow(
                        box(leafletOutput("UK", height = 570), tags$p(""),
                            tags$p(paste0("Last updated at ", Time, " | version ", VersionNo)),
                            tags$p("Data sources: ",
                                   tags$a(href='https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public','Public Health England'),", ",
                                   tags$a(href='https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/','NHS England'),", ",
                                   tags$a(href='https://www.health-ni.gov.uk','Health NI'),", ",
                                   tags$a(href='https://www.publichealth.hscni.net','HSC Public Health Agency'),", ",
                                   tags$a(href='https://www.gov.scot/coronavirus-covid-19/','Scottish Government'),", ",
                                   tags$a(href='https://covid19-phwstatement.nhs.wales','NHS Wales')),
                            width = 8, height = 645, background = 'black',
                        ),
                        # Trend
                        box(tabsetPanel(
                          tabPanel(title = 'Confirmed', plotlyOutput("trend")),
                          tabPanel(title = 'Logarithmic', plotlyOutput("logtrend")),
                          tabPanel(title = 'Daily Cases', plotlyOutput("pdailycases")),
                          tabPanel(title = 'Daily Deaths', plotlyOutput("pdailydeaths"))
                        ),
                        width = 4, height = 380, background = 'black'
                        ),
                        box(DT::dataTableOutput("table"), width = 4, background = 'black')
                      )
             ),
             tabPanel("Note",
                      fluidRow(
                        tags$style(HTML(".box.box-solid.box-primary>.box-header {} .box.box-solid.box-primary{background:#222d32}")),
                        column(
                          width = 6,
                          tags$p('User Interface'),
                          lapply(1:dim(Note%>%filter(Place=='UK' & Symbol == 'UI'))[1], function(i){
                            
                            df <- Note%>%filter(Place=='UK' & Symbol == 'UI')
                            
                              if (i == 1){
                                box(
                                  title = df$Title[i],
                                  width = NULL,
                                  background = 'orange',
                                  df$Note[i],
                                  solidHeader = TRUE,
                                  collapsible = TRUE
                                )
                              }else{
                                box(
                                  title = df$Title[i],
                                  width = NULL,
                                  background = 'orange',
                                  df$Note[i],
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  collapsed = TRUE
                                )
                              }
                          })
                        ),
                        column(
                          width = 6,
                          tags$p('Data'),
                          lapply(1:dim(Note%>%filter(Place=='UK' & Symbol == 'Data'))[1], function(i){
                            
                            df <- Note%>%filter(Place=='UK' & Symbol == 'Data')
                            
                            if (i == 1){
                              box(
                                title = df$Date[i],
                                width = NULL,
                                background = 'purple',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE
                              )
                            }else{
                              box(
                                title = df$Date[i],
                                width = NULL,
                                background = 'purple',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE
                                )
                              }
                            })
                          )
                        )
                      ) 
  ),

# England -----------------------------------------------------------------

  navbarMenu("England",
             tabPanel("Overview",
                      # Confirmed cases and death counts
                      fluidRow(
                        tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                        valueBoxOutput('Englandcases', width = 2),
                        valueBoxOutput('Englanddailycases', width = 2),
                        valueBoxOutput('Englanddeaths', width = 2),
                        valueBoxOutput('Englanddailydeaths', width = 2),
                        box(sliderInput(inputId = 'date4England', label = tags$p("Date", style = "font-size: 150%;"), min = min(unique(UKCoVLong$Date)), 
                                        max = max(unique(UKCoVLong$Date)), value = min(unique(UKCoVLong$Date)),step = 1,animate = T),
                            width = 4, height = 120, background = 'black')),
                      # Map
                      fluidRow(
                        box(leafletOutput("England", height = 595), tags$p(""), 
                            tags$p(paste0("Last updated at ", Time, " | version ", VersionNo, " | Data sources: "), 
                                   tags$a(href='https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public','Public Health England'),", ",
                                   tags$a(href='https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/','NHS England')),
                            width = 8, height = 645, background = 'black',
                        ),
                        # Trend
                        box(tabsetPanel(
                          tabPanel(title = 'Confirmed', plotlyOutput("Englandtrend")),
                          tabPanel(title = 'Logarithmic', plotlyOutput("Englandlogtrend")),
                          tabPanel(title = 'Daily Cases', plotlyOutput("Englandpdailycases")),
                          tabPanel(title = 'Daily Deaths', plotlyOutput("Englandpdailydeaths"))
                        ),
                        width = 4, height = 380, background = 'black'
                        ),
                        box(DT::dataTableOutput("Englandtable"), width = 4, background = 'black')
                      )
                      ),
             tabPanel("NHS Regions",
                      tabsetPanel(type = 'tabs',
                                  # tabPanel("Overall",
                                  #          tags$br(),
                                  #            fluidRow(
                                  #              box(plotlyOutput("EnglandTCC"), width = 6, height = 350, background = "black"),
                                  #              box(plotlyOutput("EnglandDCC"), width = 6, height = 350, background = "black"),
                                  #              ),
                                  #            fluidRow(
                                  #              box(plotlyOutput("EnglandTD"), width = 6, height = 350, background = "black"),
                                  #              box(plotlyOutput("EnglandDD"), width = 6, height = 350, background = "black"),
                                  #              )
                                  #          ),
                                  tabPanel("East of England",
                                           # Confirmed cases and death counts
                                           fluidRow(
                                             tags$br(),
                                             tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                                             valueBoxOutput('EastEnglandcases', width = 2),
                                             valueBoxOutput('EastEnglanddailycases', width = 2),
                                             valueBoxOutput('EastEnglanddeaths', width = 2),
                                             valueBoxOutput('EastEnglanddailydeaths', width = 2),
                                             box(sliderInput(inputId = 'date4EastEngland', label = tags$p("Date", style = "font-size: 150%;"), min = min(unique(UKCoVLong$Date)),
                                                             max = max(unique(UKCoVLong$Date)), value = min(unique(UKCoVLong$Date)),step = 1,animate = T),
                                                 width = 4, height = 120, background = 'black')),
                                           # Map
                                           fluidRow(
                                             box(leafletOutput("EastEngland", height = 535), tags$p(""),
                                                 tags$p(paste0("Last updated at ", Time, " | version ", VersionNo, " | Data sources: "), 
                                                        tags$a(href='https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public','Public Health England'),", ",
                                                        tags$a(href='https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/','NHS England')),
                                                 width = 8, height = 580, background = 'black',
                                             ),
                                             # Trend
                                             box(tabsetPanel(
                                               tabPanel(title = 'Confirmed', plotlyOutput("EastEnglandtrend")),
                                               tabPanel(title = 'Logarithmic', plotlyOutput("EastEnglandlogtrend")),
                                               tabPanel(title = 'Daily Cases', plotlyOutput("EastEnglandpdailycases")),
                                               tabPanel(title = 'Daily Deaths', plotlyOutput("EastEnglandpdailydeaths"))
                                             ),
                                             width = 4, height = 310, background = 'black'
                                             ),
                                             box(DT::dataTableOutput("EastEnglandtable"), width = 4, background = 'black')
                                           )
                                           ),
                                  tabPanel("London",
                                           # Confirmed cases and death counts
                                           fluidRow(
                                             tags$br(),
                                             tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                                             valueBoxOutput('Londoncases', width = 2),
                                             valueBoxOutput('Londondailycases', width = 2),
                                             valueBoxOutput('Londondeaths', width = 2),
                                             valueBoxOutput('Londondailydeaths', width = 2),
                                             box(sliderInput(inputId = 'date4London', label = tags$p("Date", style = "font-size: 150%;"), min = min(unique(UKCoVLong$Date)),
                                                             max = max(unique(UKCoVLong$Date)), value = min(unique(UKCoVLong$Date)),step = 1,animate = T),
                                                 width = 4, height = 120, background = 'black')),
                                           # Map
                                           fluidRow(
                                             box(leafletOutput("London", height = 535), tags$p(""),
                                                 tags$p(paste0("Last updated at ", Time, " | version ", VersionNo, " | Data sources: "), 
                                                        tags$a(href='https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public','Public Health England'),", ",
                                                        tags$a(href='https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/','NHS England')),
                                                 width = 8, height = 580, background = 'black',
                                             ),
                                             # Trend
                                             box(tabsetPanel(
                                               tabPanel(title = 'Confirmed', plotlyOutput("Londontrend")),
                                               tabPanel(title = 'Logarithmic', plotlyOutput("Londonlogtrend")),
                                               tabPanel(title = 'Daily Cases', plotlyOutput("Londonpdailycases")),
                                               tabPanel(title = 'Daily Deaths', plotlyOutput("Londonpdailydeaths"))
                                             ),
                                             width = 4, height = 310, background = 'black'
                                             ),
                                             box(DT::dataTableOutput("Londontable"), width = 4, background = 'black')
                                           )),
                                  tabPanel("Midlands",
                                           # Confirmed cases and death counts
                                           fluidRow(
                                             tags$br(),
                                             tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                                             valueBoxOutput('Midlandscases', width = 2),
                                             valueBoxOutput('Midlandsdailycases', width = 2),
                                             valueBoxOutput('Midlandsdeaths', width = 2),
                                             valueBoxOutput('Midlandsdailydeaths', width = 2),
                                             box(sliderInput(inputId = 'date4Midlands', label = tags$p("Date", style = "font-size: 150%;"), min = min(unique(UKCoVLong$Date)),
                                                             max = max(unique(UKCoVLong$Date)), value = min(unique(UKCoVLong$Date)),step = 1,animate = T),
                                                 width = 4, height = 120, background = 'black')),
                                           # Map
                                           fluidRow(
                                             box(leafletOutput("Midlands", height = 535), tags$p(""),
                                                 tags$p(paste0("Last updated at ", Time, " | version ", VersionNo, " | Data sources: "), 
                                                        tags$a(href='https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public','Public Health England'),", ",
                                                        tags$a(href='https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/','NHS England')),
                                                 width = 8, height = 580, background = 'black',
                                             ),
                                             # Trend
                                             box(tabsetPanel(
                                               tabPanel(title = 'Confirmed', plotlyOutput("Midlandstrend")),
                                               tabPanel(title = 'Logarithmic', plotlyOutput("Midlandslogtrend")),
                                               tabPanel(title = 'Daily Cases', plotlyOutput("Midlandspdailycases")),
                                               tabPanel(title = 'Daily Deaths', plotlyOutput("Midlandspdailydeaths"))
                                             ),
                                             width = 4, height = 310, background = 'black'
                                             ),
                                             box(DT::dataTableOutput("Midlandstable"), width = 4, background = 'black')
                                           )),
                                  tabPanel("North East and Yorkshire",
                                           # Confirmed cases and death counts
                                           fluidRow(
                                             tags$br(),
                                             tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                                             valueBoxOutput('NEcases', width = 2),
                                             valueBoxOutput('NEdailycases', width = 2),
                                             valueBoxOutput('NEdeaths', width = 2),
                                             valueBoxOutput('NEdailydeaths', width = 2),
                                             box(sliderInput(inputId = 'date4NE', label = tags$p("Date", style = "font-size: 150%;"), min = min(unique(UKCoVLong$Date)),
                                                             max = max(unique(UKCoVLong$Date)), value = min(unique(UKCoVLong$Date)),step = 1,animate = T),
                                                 width = 4, height = 120, background = 'black')),
                                           # Map
                                           fluidRow(
                                             box(leafletOutput("NE", height = 535), tags$p(""),
                                                 tags$p(paste0("Last updated at ", Time, " | version ", VersionNo, " | Data sources: "), 
                                                        tags$a(href='https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public','Public Health England'),", ",
                                                        tags$a(href='https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/','NHS England')),
                                                 width = 8, height = 580, background = 'black',
                                             ),
                                             # Trend
                                             box(tabsetPanel(
                                               tabPanel(title = 'Confirmed', plotlyOutput("NEtrend")),
                                               tabPanel(title = 'Logarithmic', plotlyOutput("NElogtrend")),
                                               tabPanel(title = 'Daily Cases', plotlyOutput("NEpdailycases")),
                                               tabPanel(title = 'Daily Deaths', plotlyOutput("NEpdailydeaths"))
                                             ),
                                             width = 4, height = 310, background = 'black'
                                             ),
                                             box(DT::dataTableOutput("NEtable"), width = 4, background = 'black')
                                           )),
                                  tabPanel("North West",
                                           # Confirmed cases and death counts
                                           fluidRow(
                                             tags$br(),
                                             tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                                             valueBoxOutput('NWcases', width = 2),
                                             valueBoxOutput('NWdailycases', width = 2),
                                             valueBoxOutput('NWdeaths', width = 2),
                                             valueBoxOutput('NWdailydeaths', width = 2),
                                             box(sliderInput(inputId = 'date4NW', label = tags$p("Date", style = "font-size: 150%;"), min = min(unique(UKCoVLong$Date)),
                                                             max = max(unique(UKCoVLong$Date)), value = min(unique(UKCoVLong$Date)),step = 1,animate = T),
                                                 width = 4, height = 120, background = 'black')),
                                           # Map
                                           fluidRow(
                                             box(leafletOutput("NW", height = 535), tags$p(""),
                                                 tags$p(paste0("Last updated at ", Time, " | version ", VersionNo, " | Data sources: "), 
                                                        tags$a(href='https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public','Public Health England'),", ",
                                                        tags$a(href='https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/','NHS England')),
                                                 width = 8, height = 580, background = 'black',
                                             ),
                                             # Trend
                                             box(tabsetPanel(
                                               tabPanel(title = 'Confirmed', plotlyOutput("NWtrend")),
                                               tabPanel(title = 'Logarithmic', plotlyOutput("NWlogtrend")),
                                               tabPanel(title = 'Daily Cases', plotlyOutput("NWpdailycases")),
                                               tabPanel(title = 'Daily Deaths', plotlyOutput("NWpdailydeaths"))
                                             ),
                                             width = 4, height = 310, background = 'black'
                                             ),
                                             box(DT::dataTableOutput("NWtable"), width = 4, background = 'black')
                                           )),
                                  tabPanel("South East",
                                           # Confirmed cases and death counts
                                           fluidRow(
                                             tags$br(),
                                             tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                                             valueBoxOutput('SEcases', width = 2),
                                             valueBoxOutput('SEdailycases', width = 2),
                                             valueBoxOutput('SEdeaths', width = 2),
                                             valueBoxOutput('SEdailydeaths', width = 2),
                                             box(sliderInput(inputId = 'date4SE', label = tags$p("Date", style = "font-size: 150%;"), min = min(unique(UKCoVLong$Date)),
                                                             max = max(unique(UKCoVLong$Date)), value = min(unique(UKCoVLong$Date)),step = 1,animate = T),
                                                 width = 4, height = 120, background = 'black')),
                                           # Map
                                           fluidRow(
                                             box(leafletOutput("SE", height = 535), tags$p(""),
                                                 tags$p(paste0("Last updated at ", Time, " | version ", VersionNo, " | Data sources: "), 
                                                        tags$a(href='https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public','Public Health England'),", ",
                                                        tags$a(href='https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/','NHS England')),
                                                 width = 8, height = 580, background = 'black',
                                             ),
                                             # Trend
                                             box(tabsetPanel(
                                               tabPanel(title = 'Confirmed', plotlyOutput("SEtrend")),
                                               tabPanel(title = 'Logarithmic', plotlyOutput("SElogtrend")),
                                               tabPanel(title = 'Daily Cases', plotlyOutput("SEpdailycases")),
                                               tabPanel(title = 'Daily Deaths', plotlyOutput("SEpdailydeaths"))
                                             ),
                                             width = 4, height = 310, background = 'black'
                                             ),
                                             box(DT::dataTableOutput("SEtable"), width = 4, background = 'black')
                                           )),
                                  tabPanel("South West",
                                           # Confirmed cases and death counts
                                           fluidRow(
                                             tags$br(),
                                             tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                                             valueBoxOutput('SWcases', width = 2),
                                             valueBoxOutput('SWdailycases', width = 2),
                                             valueBoxOutput('SWdeaths', width = 2),
                                             valueBoxOutput('SWdailydeaths', width = 2),
                                             box(sliderInput(inputId = 'date4SW', label = tags$p("Date", style = "font-size: 150%;"), min = min(unique(UKCoVLong$Date)),
                                                             max = max(unique(UKCoVLong$Date)), value = min(unique(UKCoVLong$Date)),step = 1,animate = T),
                                                 width = 4, height = 120, background = 'black')),
                                           # Map
                                           fluidRow(
                                             box(leafletOutput("SW", height = 535), tags$p(""),
                                                 tags$p(paste0("Last updated at ", Time, " | version ", VersionNo, " | Data sources: "), 
                                                        tags$a(href='https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public','Public Health England'),", ",
                                                        tags$a(href='https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/','NHS England')),
                                                 width = 8, height = 580, background = 'black',
                                             ),
                                             # Trend
                                             box(tabsetPanel(
                                               tabPanel(title = 'Confirmed', plotlyOutput("SWtrend")),
                                               tabPanel(title = 'Logarithmic', plotlyOutput("SWlogtrend")),
                                               tabPanel(title = 'Daily Cases', plotlyOutput("SWpdailycases")),
                                               tabPanel(title = 'Daily Deaths', plotlyOutput("SWpdailydeaths"))
                                             ),
                                             width = 4, height = 310, background = 'black'
                                             ),
                                             box(DT::dataTableOutput("SWtable"), width = 4, background = 'black')
                                           ))
                                  )
                      ),
             tabPanel("Note",
                      fluidRow(
                        tags$style(HTML(".box.box-solid.box-primary>.box-header {} .box.box-solid.box-primary{background:#222d32}")),
                        column(
                          width = 6,
                          tags$p('User Interface'),
                          lapply(1:dim(Note%>%filter(Place=='England' & Symbol == 'UI'))[1], function(i){
                            
                            df <- Note%>%filter(Place=='England' & Symbol == 'UI')
                            
                            if (i == 1){
                              box(
                                title = df$Title[i],
                                width = NULL,
                                background = 'orange',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE
                              )
                            }else{
                              box(
                                title = df$Title[i],
                                width = NULL,
                                background = 'orange',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE
                              )
                            }
                          })
                        ),
                        column(
                          width = 6,
                          tags$p('Data'),
                          lapply(1:dim(Note%>%filter(Place=='England' & Symbol == 'Data'))[1], function(i){
                            
                            df <- Note%>%filter(Place=='England' & Symbol == 'Data')
                            
                            if (i == 1){
                              box(
                                title = df$Date[i],
                                width = NULL,
                                background = 'purple',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE
                              )
                            }else{
                              box(
                                title = df$Date[i],
                                width = NULL,
                                background = 'purple',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE
                              )
                            }
                            })
                          )
                        )
                      )
  ),

# Northern Ireland --------------------------------------------------------

  navbarMenu("Northern Ireland",
             tabPanel("Overview",
                      # Confirmed cases and death counts
                      fluidRow(
                        tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                        valueBoxOutput('NIcases', width = 2),
                        valueBoxOutput('NIdailycases', width = 2),
                        valueBoxOutput('NIdeaths', width = 2),
                        valueBoxOutput('NIdailydeaths', width = 2),
                        box(sliderInput(inputId = 'date4NI', label = tags$p("Date", style = "font-size: 150%;"), min = min(unique(UKCoVLong$Date)),
                                        max = max(unique(UKCoVLong$Date)), value = min(unique(UKCoVLong$Date)),step = 1,animate = T),
                            width = 4, height = 120, background = 'black')),
                      # Map
                      fluidRow(
                        box(leafletOutput("NI", height = 595), tags$p(""),
                            tags$p(paste0("Last updated at ", Time, " | version ", VersionNo, " | Data source: "),
                                   tags$a(href='https://www.health-ni.gov.uk','Health NI'),", ",
                                   tags$a(href='https://www.publichealth.hscni.net','HSC Public Health Agency')),
                            width = 8, height = 645, background = 'black',
                        ),
                        # Trend
                        box(tabsetPanel(
                          tabPanel(title = 'Confirmed', plotlyOutput("NItrend")),
                          tabPanel(title = 'Logarithmic', plotlyOutput("NIlogtrend")),
                          tabPanel(title = 'Daily Cases', plotlyOutput("NIpdailycases")),
                          tabPanel(title = 'Daily Deaths', plotlyOutput("NIpdailydeaths"))
                        ),
                        width = 4, height = 380, background = 'black'
                        ),
                        box(DT::dataTableOutput("NItable"), width = 4, background = 'black')
                      )
             ),
             tabPanel("Note",
                      fluidRow(
                        tags$style(HTML(".box.box-solid.box-primary>.box-header {} .box.box-solid.box-primary{background:#222d32}")),
                        column(
                          width = 6,
                          tags$p('User Interface'),
                          lapply(1:dim(Note%>%filter(Place=='Northern Ireland' & Symbol == 'UI'))[1], function(i){
                            
                            df <- Note%>%filter(Place=='Northern Ireland' & Symbol == 'UI')
                            
                            if (i == 1){
                              box(
                                title = df$Title[i],
                                width = NULL,
                                background = 'orange',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE
                              )
                            }else{
                              box(
                                title = df$Title[i],
                                width = NULL,
                                background = 'orange',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE
                              )
                            }
                          })
                        ),
                        column(
                          width = 6,
                          tags$p('Data'),
                          lapply(1:dim(Note%>%filter(Place=='Northern Ireland' & Symbol == 'Data'))[1], function(i){
                            
                            df <- Note%>%filter(Place=='Northern Ireland' & Symbol == 'Data')
                            
                            if (i == 1){
                              box(
                                title = df$Date[i],
                                width = NULL,
                                background = 'purple',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE
                              )
                            }else{
                              box(
                                title = df$Date[i],
                                width = NULL,
                                background = 'purple',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE
                              )
                            }
                          })
                        )
                      )
                      )
  ),

# Scotland ----------------------------------------------------------------

  navbarMenu("Scotland",
             tabPanel("Overview",
                      # Confirmed cases and death counts
                      fluidRow(
                        tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                        valueBoxOutput('Scotlandcases', width = 2),
                        valueBoxOutput('Scotlanddailycases', width = 2),
                        valueBoxOutput('Scotlanddeaths', width = 2),
                        valueBoxOutput('Scotlanddailydeaths', width = 2),
                        box(sliderInput(inputId = 'date4Scotland', label = tags$p("Date", style = "font-size: 150%;"), min = min(unique(UKCoVLong$Date)), 
                                        max = max(unique(UKCoVLong$Date)), value = min(unique(UKCoVLong$Date)),step = 1,animate = T),
                            width = 4, height = 120, background = 'black')),
                      # Map
                      fluidRow(
                        box(leafletOutput("Scotland", height = 595), tags$p(""), 
                            tags$p(paste0("Last updated at ", Time, " | version ", VersionNo, " | Data sources: "), 
                                   tags$a(href='https://www.gov.scot/coronavirus-covid-19/','Scottish Government')),
                            width = 8, height = 645, background = 'black',
                        ),
                        # Trend
                        box(tabsetPanel(
                          tabPanel(title = 'Confirmed', plotlyOutput("Scotlandtrend")),
                          tabPanel(title = 'Logarithmic', plotlyOutput("Scotlandlogtrend")),
                          tabPanel(title = 'Daily Cases', plotlyOutput("Scotlandpdailycases")),
                          tabPanel(title = 'Daily Deaths', plotlyOutput("Scotlandpdailydeaths"))
                        ),
                        width = 4, height = 380, background = 'black'
                        ),
                        box(DT::dataTableOutput("Scotlandtable"), width = 4, background = 'black')
                      )
             ),
             tabPanel("Note",
                      fluidRow(
                        tags$style(HTML(".box.box-solid.box-primary>.box-header {} .box.box-solid.box-primary{background:#222d32}")),
                        column(
                          width = 6,
                          tags$p('User Interface'),
                          lapply(1:dim(Note%>%filter(Place=='Scotland' & Symbol == 'UI'))[1], function(i){
                            
                            df <- Note%>%filter(Place=='Scotland' & Symbol == 'UI')
                            
                            if (i == 1){
                              box(
                                title = df$Title[i],
                                width = NULL,
                                background = 'orange',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE
                              )
                            }else{
                              box(
                                title = df$Title[i],
                                width = NULL,
                                background = 'orange',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE
                              )
                            }
                          })
                        ),
                        column(
                          width = 6,
                          tags$p('Data'),
                          lapply(1:dim(Note%>%filter(Place=='Scotland' & Symbol == 'Data'))[1], function(i){
                            
                            df <- Note%>%filter(Place=='Scotland' & Symbol == 'Data')
                            
                            if (i == 1){
                              box(
                                title = df$Date[i],
                                width = NULL,
                                background = 'purple',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE
                              )
                            }else{
                              box(
                                title = df$Date[i],
                                width = NULL,
                                background = 'purple',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE
                              )
                            }
                          })
                        )
                      )
                      )
  ),

# Wales -------------------------------------------------------------------

  navbarMenu("Wales",
             tabPanel("Overview",
                      # Confirmed cases and death counts
                      fluidRow(
                        tags$head(tags$style(HTML(".small-box {height: 120px}"))),
                        valueBoxOutput('Walescases', width = 2),
                        valueBoxOutput('Walesdailycases', width = 2),
                        valueBoxOutput('Walesdeaths', width = 2),
                        valueBoxOutput('Walesdailydeaths', width = 2),
                        box(sliderInput(inputId = 'date4Wales', label = tags$p("Date", style = "font-size: 150%;"), min = min(unique(UKCoVLong$Date)), 
                                        max = max(unique(UKCoVLong$Date)), value = as.Date('2020-03-19'),step = 1,animate = T),
                            width = 4, height = 120, background = 'black')),
                      # Map
                      fluidRow(
                        box(leafletOutput("Wales", height = 595), tags$p(""), 
                            tags$p(paste0("Last updated at ", Time, " | version ", VersionNo, " | Data source: "), 
                                   tags$a(href='https://covid19-phwstatement.nhs.wales','NHS Wales')),
                            width = 8, height = 645, background = 'black',
                        ),
                        # Trend
                        box(tabsetPanel(
                          tabPanel(title = 'Confirmed', plotlyOutput("Walestrend")),
                          tabPanel(title = 'Logarithmic', plotlyOutput("Waleslogtrend")),
                          tabPanel(title = 'Daily Cases', plotlyOutput("Walespdailycases")),
                          tabPanel(title = 'Daily Deaths', plotlyOutput("Walespdailydeaths"))
                        ),
                        width = 4, height = 380, background = 'black'
                        ),
                        box(DT::dataTableOutput("Walestable"), width = 4, background = 'black')
                      )
             ),
             tabPanel("Note",
                      fluidRow(
                        tags$style(HTML(".box.box-solid.box-primary>.box-header {} .box.box-solid.box-primary{background:#222d32}")),
                        column(
                          width = 6,
                          tags$p('User Interface'),
                          lapply(1:dim(Note%>%filter(Place=='Wales' & Symbol == 'UI'))[1], function(i){
                            
                            df <- Note%>%filter(Place=='Wales' & Symbol == 'UI')
                            
                            if (i == 1){
                              box(
                                title = df$Title[i],
                                width = NULL,
                                background = 'orange',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE
                              )
                            }else{
                              box(
                                title = df$Title[i],
                                width = NULL,
                                background = 'orange',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE
                              )
                            }
                          })
                        ),
                        column(
                          width = 6,
                          tags$p('Data'),
                          lapply(1:dim(Note%>%filter(Place=='Wales' & Symbol == 'Data'))[1], function(i){
                            
                            df <- Note%>%filter(Place=='Wales' & Symbol == 'Data')
                            
                            if (i == 1){
                              box(
                                title = df$Date[i],
                                width = NULL,
                                background = 'purple',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE
                              )
                            }else{
                              box(
                                title = df$Date[i],
                                width = NULL,
                                background = 'purple',
                                df$Note[i],
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                collapsed = TRUE
                              )
                            }
                          })
                        )
                      )
                      )
  ),

# Tables ------------------------------------------------------------------

  navbarMenu("Tables",
             tabPanel("Confirmed Cases",

                      fluidRow(
                        column(3, selectInput(inputId = "source", label = "ULTA / LGD / Health Board", choices = UKCoV$Source, multiple = TRUE)),
                        column(3, selectInput(inputId = "nhscategory", label = "NHS Regions / LGD / Health Board", choices = unique(UKCoV$NHSCategory), multiple = TRUE)),
                        column(3, selectInput(inputId = "countries", label = "Constituent Countries", choices = unique(UKCoV$Countries), selected = "England", multiple = TRUE)),
                        tags$p("Data source: ", tags$a(href='https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases','Public Health England'))
                        ),
                      hr(),
                      DT::dataTableOutput("dataset")
                      ),
             tabPanel("Number of People Tested & Deaths",
                      fluidPage(
                        div(class='body', 
                            tags$div(align='justify', 
                                     tags$p("Data source: ", tags$a(href='https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases','Public Health England'))
                                     )
                            ),
                        hr(),
                        DT::dataTableOutput("dataset2")
                        )
                      ),
             tabPanel("Number of Deaths in terms of Constituent Countries",
                      fluidPage(
                        div(class='body', 
                            tags$div(align='justify',
                                     tags$p("Data source: ",
                                            tags$a(href='https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/','NHS England')," for 'England and DiffEngland' (2pm each day), ",
                                            tags$a(href='https://www.publichealth.hscni.net/publications/covid-19-surveillance-reports','HSC Public Health Agency'), " for 'NI and DiffNI' (each day), ",
                                            tags$a(href='https://www.gov.scot/coronavirus-covid-19/','Scottish government'), " for 'Scotland and DiffScotland' (2pm each day), ",
                                            tags$a(href='https://covid19-phwstatement.nhs.wales','Public Health Wales'), " for 'Wales and DiffWales' (2pm each day)."
                                     )
                            )
                        ),
                        hr(),
                        DT::dataTableOutput("dataset3")
                        )
                      ),
             tabPanel("England Hospital Deaths",
                      
                      fluidRow(
                        column(3, selectInput(inputId = "NHSRegion", label = "England NHS Region", choices = df4$NHSRegion, multiple = TRUE)),
                        column(3, selectInput(inputId = "HospitalCode", label = "Hospital Code", choices = df4$Code, multiple = TRUE)),
                        column(3, selectInput(inputId = "HospitalName", label = "Hospital Name", choices = df4$Name, multiple = TRUE)),
                        tags$p("Data source: ", tags$a(href='https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/','NHS England'))
                      ),
                      hr(),
                      DT::dataTableOutput("dataset4")
                      )
             ),

# Search ------------------------------------------------------------------

  tabPanel("Search",
           fluidPage(
             div(class="body",
                 tags$div(align="justify",
                          selectInput(inputId='search',
                                      label='Area name(s)',
                                      choices = UKCoV$Source,
                                      selected = 'Coventry',
                                      multiple = T,
                                      width = '1600px'
                                      ),
                          #box(plotlyOutput("SearchCCC"))
                          
                          fluidRow(
                            box(plotlyOutput("SearchTCC"), width = 6, background = 'black'),
                            box(plotlyOutput("SearchDCC"), width = 6, background = 'black')
                          )
                          )
                 )
             )
           ),
  #tabPanel("News"),


# About -------------------------------------------------------------------

  navbarMenu("About",
             tabPanel("Introduction",
                      fluidPage(div(class="body",
                                    tags$div(align="justify",
                                             tags$h2("Welcome!"),
                                             tags$br(),
                                             tags$p("This is the website to provide the current confirmed cases update of COVID-19 in UK."),
                                             tags$p(paste0("The data ranges from 2020-03-07 to ", as.character(Sys.Date()),".")),
                                             tags$p("This website is the current project I am working on.")
                                    )
                      )
                      )
             ),
             tabPanel("Source",
                      fluidPage(div(class="body",
                                    tags$div(align="justify",
                                             tags$h2("Data Source"),
                                             tags$br(),
                                             tags$p("The following links are the data source of UK COVID-19 cases."),
                                             tags$br(),
                                             tags$ul(
                                               tags$li(
                                                 tags$a(href="https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases",
                                                        "Public Health England (PHE)")
                                               ),
                                               tags$li(
                                                 tags$a(href="https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/",
                                                        "NHS England")
                                               ),
                                               tags$li(
                                                 tags$a(href="https://www.publichealth.hscni.net/publications/covid-19-surveillance-reports",
                                                        "HSC Public Health Agency")
                                               ),
                                               tags$li(
                                                 tags$a(href="https://www.gov.scot/coronavirus-covid-19/",
                                                        "Scottish Government")
                                               ),
                                               tags$li(
                                                 tags$a(href="https://covid19-phwstatement.nhs.wales",
                                                        "Public Health Wales")
                                               )
                                             ),
                                             tags$p("The following links are the daily dashboard for each constituent country."),
                                             tags$br(),
                                             tags$ul(
                                               tags$li(
                                                 tags$a(href="https://coronavirus.data.gov.uk/#local-authorities",
                                                        "Public Health England (PHE)")
                                               ),
                                               tags$li(
                                                 tags$a(href="http://www.pha.site/cvdashboard",
                                                        "Health NI")
                                               ),
                                               tags$li(
                                                 tags$a(href="https://www.arcgis.com/apps/opsdashboard/index.html#/658feae0ab1d432f9fdb53aa082e4130",
                                                        "Public Health Scotland")
                                               ),
                                               tags$li(
                                                 tags$a(href="https://covid19-phwdata.nhs.wales",
                                                        "NHS Wales")
                                               )
                                             ),
                                             tags$br(),
                                             tags$h2("Resource"),
                                             tags$br(),
                                             tags$p("The following links are the resources related to COVID-19."),
                                             tags$br(),
                                             tags$ul(
                                               tags$li(
                                                 tags$a(href='https://www.nhs.uk/conditions/coronavirus-covid-19/symptoms-and-what-to-do/','Symptoms and what to do (NHS)')
                                               ),
                                               tags$li(
                                                 tags$a(href="https://www.gov.uk/government/publications/coronavirus-action-plan/coronavirus-action-plan-a-guide-to-what-you-can-expect-across-the-uk",
                                                        "Coronavirus action plan: a guide to what you can expect across the UK (Department of Health & Social Care, 3 March 2020)")
                                               ),
                                               tags$li(
                                                 tags$a(href="https://www.gov.uk/coronavirus",
                                                        "Coronavirus (COVID-19): what you need to do (GOV.UK)")
                                               )
                                             )
                                    )
                      )
                      )
             ),
             tabPanel("Q & A",
                      fluidPage(
                        div(class="body",
                            tags$div(align="justify",
                                     tags$h2("Question and Answers"),
                                     tags$br(),
                                     tags$p("Here are the potential questions you might want to ask for me. I provide the answers for each question :)"),
                                     tags$br(),
                                     tabsetPanel(
                                       type = 'tabs',
                                       tabPanel("Maps",
                                                tags$div(align='justify',
                                                         tags$br(),
                                                         tags$p("Q1: What do those red circles mean in 'Map' and 'Charts'? What does the size of a red circle refer to?"),
                                                         tags$p("A1: The red circles show the number of total confirmed cases. The bigger the red circle, the higher the number of total confirmed cases a local authority has. 
                                                                                      The radius of a red circle is equal to its corresponding number of confirmed cases times 10."),
                                                         tags$br(),
                                                         tags$p("Q2: Why there are two red numbers in the Map? What do they mean?"),
                                                         tags$p("A2: The bigger red number shows the number of confirmed cases on the day selected. The smaller refers to the difference of the confirmed cases between the day 
                                                                                      selected and the day before the day selected. If there was an increase in the difference, the number is red with an up arrow. Otherwise, the number is green 
                                                                                      with a down arrow."),
                                                         tags$br(),
                                                         tags$p("Q3: Why the circles' locations are not accurate?"),
                                                         tags$p("A3: The longitude and latitude of the circles are not retrieved from OpenStreetMap, which is used as an interaction map here. I will try to increase the 
                                                                                      accuracy in upcoming updates."),
                                                         tags$br(),
                                                         tags$p("Q4: Why there is a difference between the total confirmed cases in UK shown here and the one provided by Public Health England (PHE)?"),
                                                         tags$p("A4: The differnce is the unconfirmed cases or residents living outside the local authority or Health board. Here I provide the total confirmed cases in each 
                                                                                      local authority/ health board/ local government district only."),
                                                         tags$br(),
                                                         tags$p("Q5: Why there is only one circle in Northern Ireland in some dates?"),
                                                         tags$p("A5: In some COVID-19 Daily reports, HSC Public Health Agency only provides the total confirmed cases in Northern Ireland. Sometimes they provide the number 
                                                                                      in terms of Local Government Districts. I will have a look at other trusted sources for adding the missing information. Note that the aggregate number (one 
                                                                                      circle) does not exclude the number of 'Unknown'. Hence, interpret the situation with caution."),
                                                         tags$br(),
                                                         tags$p("Q6. Why there are no cases in Wales prior to 2020-03-19?"),
                                                         tags$p("A6: This is the earliest date based on the data I found for Wales."),
                                                         tags$br(),
                                                         tags$p("Q7: Why there is a difference between the sum of each NHS region shown here and the one in PHE Dashboard?"),
                                                         tags$p("A7: The reason was in twofold. On the one hand, it is the difference in time of recording and mentioning. On the other hand, there are some local authorities 
                                                                                      that have results in both NHS regions. For instance, Cumbria is in between North East and North West. I've checked each locations with the following links: ", 
                                                                tags$a(href='https://www.hee.nhs.uk/about/how-we-work/your-area','NHS Health Education England'), ', '
                                                                ,tags$a(href='https://digital.nhs.uk/services/organisation-data-service/2019-changes-to-regions','NHS Digital'),', ', 
                                                                tags$a(href='https://ago-item-storage.s3.us-east-1.amazonaws.com/944355ae7a11446cb030ed6313554a00/CTY_LAD_APR_2019_UK_MP.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEIv%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJHMEUCIQDoZmJxNYMRyomZu9tMcdmqwciw1TyXTEodyQkJi4m3cgIgPDefD6oHnV%2FD1pBQHYfF9ytxhhQVYCHTLgo0mpR2BuYqvQMIlP%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FARAAGgw2MDQ3NTgxMDI2NjUiDCfkcNfEHhJiXCaIRCqRA1cB2PXo4s6jjjOWR19x2ePifgh%2FC4aVFno5BJVQ0Z6TKoCd6Frz%2FajahCu4SEEXYrlbM6lsto8bf0a3NOPaBgj2XwPhQpouzzgsz%2FlaaxYvIK1nUmASjOqb4HDoC5h8mopLRSah0gSfooa7zw%2Btd6xlJ6k55rcebcgfVBlVuRcxwcfLdQJtZYxQW7AVmIAS2Nkxay02p5Cn2MLEmovnjn9sKl%2BGu1YaQPRzJHdu3yqnBNRSzD4oitsf342JJyLS8uaeJYbBC%2FB1f63YPoFLTiauGgRmcy%2B2UNm6yv1D0dO3vLSWYNhmf82NjmpjZY2TbrMWR804GnNrTRtuE%2B%2FPHFX%2F0yf%2BidziBENHAWkefb0TG8lJUDi1csYuslVqT3z%2B7PvSdSOdDifUmoFR5f1W9K7S%2FyTbzuzA1dDCj0xJTn9%2FTPbhAJb%2BldwJGcszlGgVBpGXYl7O1TwKE6BBuznLQ3lZA98TTA3TnSM1RbD6MPzpwYqOve5v5WGIecSoQfO7Lht%2BhIrLGQPv9%2BC4UySjPLvnMObLqPQFOusBaM4jppel4YVqA6hSH2XoL1WWdIVpGBe4xF1yf6KhKd4zpEGpfNGTLRsnrd03bJMoqT6XGIU1utK683q4j%2BGigZ3f%2B9pCrFlOjTP6uewYTJEGTZqzMyUaG0ytid7iuwkNWLl%2FQUMPY3kRaQhr7KcM%2FNUYuUopqsfjQUZlE1xD03m1HI%2Bd%2FjnD3fNi5U%2F8BgTaggcwLdIGal%2FIl%2BNQcVPQfSEE%2BK899OTCWITyp3flEZgE75gf5KZ3oP4gqjacchTkJ2J7jZzdqiSzanX8PY5mJLJpMm8XP2%2F4xJj%2F05FDj3QetFGhk5bmkhsZCg%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20200405T195148Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAYZTTEKKE3FPWUTG5%2F20200405%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=bbbc54d8f12120c6431b5db1db0fa93f74e6d13ec9f00e763ccfcadb78bffec7','OS Data '), 
                                                                ' and ', tags$a(href='http://geoportal.statistics.gov.uk/datasets/local-authority-district-to-region-april-2019-lookup-in-england/data','Office for National Statistics'), "."),
                                                         tags$br(),
                                                         tags$p("Q8: What do you mean 'the number for some health boards in Scotland are incorrect on 8 and 9 April 2020?'"),
                                                         tags$p("A8: Recently Scottish government updated their ways of reporting the COVID-19 situations in Scotland. One of the changes is that in their table of showing the 
                                                                                      confirmed cases in each health baord, some fo them are marked with '*' since the number is less than five. This happened in Orkney on 8 and 9 April, Eileanan 
                                                                                      Siar (Western Isles) on 8 April and Golden Jubilee National Hospital on 9 April. Hence, I modified their numbers since it would be easier to plot in the
                                                                                      Map while the daily sum matches with the total number of confirmed cases reported by Scottish government (It's 4,565 on 8 April, 4,957 on 9 April). However, 
                                                                                      please take these changes into the map interpretation."),
                                                )
                                       ),
                                       tabPanel("Charts",
                                                tags$div(align='justify',
                                                         tags$br(),
                                                         tags$p("Q1: Why there is a difference between number of deaths in 'Number of People Tested' and 'Number of Deaths in terms of Constituent Countries'?"),
                                                         tags$p("A1: It is because of the time difference of the information shown. PHE shows death counts of all Constituent Countries as of from yesterday, while I extracted 
                                                                                      the related information directly from the source provided. For instance, Scottish government and Public Health Wales publish the daily data at 2pm each day, while 
                                                                                      PHE provides the today's death counts from yesterday. For more information, please have a look at the description inside", 
                                                                tags$a(href='https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14','COVID-19 dashboard')," provided by PHE.")
                                                )
                                       ),
                                       tabPanel("Tables",
                                                tags$div(align='justify',
                                                         tags$br(),
                                                         tags$p("Q1: Why there is a difference between NumPositive under 'Number of People Tested and Deaths' and the aggregate number shown in 'Overview' under Maps?"),
                                                         tags$p("A1: Please refer to Q4 in 'Maps' :)"),
                                                         tags$br(),
                                                         tags$p("Q2: Is the table 'Number of People Tested and Deaths' based on UK?"),
                                                         tags$p("A2: Yes. I will try to create another table that shows the number of people tested deaths based on 4 constituent countries."),
                                                         tags$br(),
                                                         tags$p("Q3: What do the column names of table 'Number of People Tested and Deaths' refer to?"),
                                                         tags$p("A3: 'NumPeopleTested' refers to Number of people tested (note that some individuals are tested more than once for clinical reasons). 'NumPositive' and 'NumNegative' 
                                                                                      refer to number of positive and negative test results respectively. 'DiffNumPositive' refers to the difference between number of positive tests between two 
                                                                                      consecutive days. 'TotalDeaths' refers to number of total deaths in UK."),
                                                         tags$br(),
                                                         tags$p("Q4: Are the data tables shown in 'Tables' downloadable?"),
                                                         tags$p("A4: Yes, there are three options below each table: 'Copy', 'CSV', 'Excel'. 'Copy' allows you to copy the table, while 'CSV' and 'Excel' allows you download and 
                                                                                      save the table as '.csv' or '.xlsx'."),
                                                         tags$br(),
                                                         tags$p("Q5: Why there is a difference between number of deaths in 'Number of People Tested' and 'Number of Deaths in terms of Constituent Countries'?"),
                                                         tags$p("Please refer to Q1 in 'Charts' :)")
                                                )
                                       ),
                                       tabPanel("Others",
                                                tags$div(align='justify',
                                                         tags$br(),
                                                         tags$p("Q1: Will you update daily?"),
                                                         tags$p("A1: Yes. However, I am not sure when exactly during the day I need to get the data from various sources, which provide at different times during the day."),
                                                         tags$br(),
                                                         tags$p("Q2: What software do you use to create this website?"),
                                                         tags$p("A2: R with packages such as ", tags$a(href="https://shiny.rstudio.com","Shiny "), "and ",tags$a(href="https://rstudio.github.io/leaflet/","Leaflet"), ".")
                                                )
                                       )
                                     )
                            )
                        )
                      )
             ),
             tabPanel("Me",
                      fluidPage(div(class="body",
                                    tags$div(align="justify",
                                             tags$h2("About Me"),
                                             tags$br(),
                                             tags$p("Hi everyone! I am Fendi who's studying in UK at the moment. Below is my Github link. I will try to upload the code for this website as soon as possible :)"),
                                             tags$a(href="https://github.com/fendit?tab=repositories",
                                                    tags$img(src="https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png", width = "100px", height = "100px"))
                                             ),
                                    )
                                )
                      )
             ),
  tabPanel("Log",
           fluidRow(
             tags$style(HTML(".box.box-solid.box-primary>.box-header {} .box.box-solid.box-primary{background:#222d32}")),
             column(
               width = 12,
               lapply(1:dim(Log)[1], function(i){
                 if (i == 1){
                   box(
                     title = Log$Title[i],
                     width = NULL,
                     background = 'black',
                     Log$Log[i],
                     solidHeader = TRUE,
                     collapsible = TRUE
                   )
                 }else{
                   box(
                     title = Log$Title[i],
                     width = NULL,
                     background = 'black',
                     Log$Log[i],
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     collapsed = TRUE
                     )
                   }
                 })
               )
             )
           ),
  useShinydashboard(),
  collapsible = TRUE
)
