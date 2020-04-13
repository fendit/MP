# Import data
df <- read.csv(file = "UKCoronaVirusData.csv", stringsAsFactors = FALSE)

# Change the column names
if (length(colnames(df)[-1:-7])!=as.numeric(Sys.Date()-as.Date("2020-03-07"))){
  colnames(df)[-1:-6] <- as.character(seq(as.Date("2020-03-07"), Sys.Date()-1, by = 1))
}else{
  colnames(df)[-1:-6] <- as.character(seq(as.Date("2020-03-07"), Sys.Date(), by = 1))
}
#colnames(df)

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

# Updated time
time <- "07:30:00"


library(shiny)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(xts)
library(dplyr)

ui <- navbarPage(inverse = TRUE,
                 "UK COVID-19",
                 windowTitle = "UK COVID-19 Confirmed Cases Update by FT",
                 navbarMenu("Maps",
                            icon=icon("map-marked-alt", lib = "font-awesome"),

# Maps: Overview ----------------------------------------------------------

                            tabPanel("Overview",
                                     bootstrapPage(
                                       div(class="outer",
                                           tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                           tags$style(type="text/css", "UK.recalculating { opacity: 1.0; }"),
                                           leafletOutput(outputId = "UK", width = "100%", height = "100%"),
                                           absolutePanel(
                                             top = 30, right = 20, width = 450, height = 600,
                                             wellPanel(
                                               shiny::HTML("<h4 align='right'><span style='color: black'>UK COVID-19 Confirmed Cases Update</span><h4>"),
                                               shiny::HTML(paste0("<h4 align='right'><span style='color: black'>", paste0("Data as of ", weekdays(as.Date(max(unique(UKCoVLong$Date)))), " ", format(max(unique(UKCoVLong$Date)), format = "%d %B %Y")), "</span><h4>")),
                                               wellPanel(
                                                 shiny::HTML("<h5 align='center'><span style='color:black'>UK Cumulative Confirmed Cases<h5>"),
                                                 textOutput(outputId = "Total"),
                                                 tags$head(tags$style("#Total{color: red; font-size: 60px; font-style: bold; text-align:center;}")),
                                                 htmlOutput(outputId="diff"),
                                                 tags$head(tags$style("#diff{font-size: 30px; font-style: bold; text-align:center;}")),
                                                 style = "padding: 0px;"),
                                               sliderInput(inputId = "date", 
                                                           label = shiny::HTML("<p><span style='color: black'>Date</span></p>"),
                                                           min = min(unique(UKCoVLong$Date)),
                                                           max = max(unique(UKCoVLong$Date)),
                                                           step = 1,
                                                           animate = T,
                                                           value = max(unique(UKCoVLong$Date)))
                                             ),
                                             plotlyOutput("trend")
                                           ),
                                           absolutePanel(
                                             bottom = 10, right = 20,
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Source: Public Health England, HSC Public Health Agency, Scottish Goverment and Public Health Wales","</span><h5>")),
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>Updated: ", paste(as.character(Sys.Date()), time, "BST", sep = " "),"</span><h5>")),
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Note: The number for some health boards in Scotland are incorrect on 8 and 9 April 2020. Please check 'Q&A' (under 'About') for more information | version 0.2.7","</span><h5>"))
                                           )
                                       )
                                     )
                            ),

# Maps: Constituent Countries ---------------------------------------------

                            tabPanel("Constituent Countries",
                                     tabsetPanel(type = "tabs",
                                                 # England
                                                 tabPanel("England",
                                                          fluidPage(
                                                            div(class="outer1",
                                                                tags$style(type = "text/css", ".outer1 {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                                leafletOutput("England", width = "100%", height = "100%"),
                                                                tags$style(type="text/css", "England.recalculating { opacity: 1.0; }"),
                                                                absolutePanel(
                                                                  top = 10, right = 10, width = 450,
                                                                  wellPanel(
                                                                    shiny::HTML("<h4 align='center'><span style='color: black'>England Cumulative Confirmed Cases</span><h4>"),
                                                                    textOutput(outputId = "Eng"),
                                                                    tags$head(tags$style("#Eng{color: red; font-size: 60px; font-style: bold; text-align:center;}")),
                                                                    htmlOutput(outputId = "EngDiff"),
                                                                    tags$head(tags$style("#EngDiff{font-size: 30px; font-style: bold; text-align:center;}")),
                                                                    sliderInput(inputId = "date4England", 
                                                                                label = shiny::HTML("<p><span style='color: black'>Date</span></p>"),
                                                                                min = min(unique(UKCoVLong$Date)),
                                                                                max = max(unique(UKCoVLong$Date)),
                                                                                step = 1,
                                                                                animate = T,
                                                                                value = min(unique(UKCoVLong$Date))
                                                                                ),
                                                                    ),
                                                                  wellPanel(
                                                                    tags$style(type="text/css", "EnglandTable.recalculating { opacity: 1.0; }"),
                                                                    DT::dataTableOutput("EnglandTable")
                                                                  )
                                                                  ),
                                                                absolutePanel(
                                                                  bottom = 10, right = 20,
                                                                  shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Source: Public Health England","</span><h5>")),
                                                                  shiny::HTML("<h5 align='right'><span style='color: white'>Please check 'Q&A' (under 'About') for more information.</span><h5>"),
                                                                  shiny::HTML(paste0("<h5 align='right'><span style='color: white'>Updated: ", paste(as.character(Sys.Date()), time, "BST", sep = " "),"</span><h5>"))
                                                                  )
                                                                )
                                                            )
                                                          ),
                                                 # Northern Ireland
                                                 tabPanel("Northern Ireland",
                                                          fluidPage(
                                                            div(class="outer1",
                                                                tags$style(type = "text/css", ".outer1 {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                                tags$style(type="text/css", "NI.recalculating { opacity: 1.0; }"),
                                                                leafletOutput("NI", width = "100%", height = "100%"),
                                                                absolutePanel(
                                                                  top = 10, right = 10, width = 450,
                                                                  wellPanel(
                                                                    shiny::HTML("<h4 align='center'><span style='color: black'>Northern Ireland Cumulative Confirmed Cases</span><h4>"),
                                                                    textOutput(outputId = "NIreland"),
                                                                    tags$head(tags$style("#NIreland{color: red; font-size: 60px; font-style: bold; text-align:center;}")),
                                                                    htmlOutput(outputId = "NIDiff"),
                                                                    tags$head(tags$style("#NIDiff{font-size: 30px; font-style: bold; text-align:center; }")),
                                                                    sliderInput(inputId = "date4NI", 
                                                                                label = shiny::HTML("<p><span style='color: black'>Date</span></p>"),
                                                                                min = min(unique(UKCoVLong$Date)),
                                                                                max = max(unique(UKCoVLong$Date)),
                                                                                step = 1,
                                                                                animate = T,
                                                                                value = min(unique(UKCoVLong$Date))
                                                                                ),
                                                                    ),
                                                                  wellPanel(
                                                                    tags$style(type="text/css", "NITable.recalculating { opacity: 1.0; }"),
                                                                    DT::dataTableOutput("NITable")
                                                                  )
                                                                  ),
                                                                absolutePanel(
                                                                  bottom = 10, right = 20,
                                                                  shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Source: HSC Public Health Agency","</span><h5>")),
                                                                  shiny::HTML("<h5 align='right'><span style='color: white'>Please check 'Q&A' (under 'About') for more information.</span><h5>"),
                                                                  shiny::HTML(paste0("<h5 align='right'><span style='color: white'>Updated: ", paste(as.character(Sys.Date()), time, "BST", sep = " "),"</span><h5>")),
                                                                  )
                                                                )
                                                            )
                                                          ),
                                                 # Scotland
                                                 tabPanel("Scotland",
                                                          fluidPage(
                                                            div(class="outer1",
                                                                tags$style(type = "text/css", ".outer1 {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                                tags$style(type="text/css", "Scotland.recalculating { opacity: 1.0; }"),
                                                                leafletOutput("Scotland", width = "100%", height = "100%"),
                                                                absolutePanel(
                                                                  top = 10, right = 10, width = 450,
                                                                  wellPanel(
                                                                    shiny::HTML("<h4 align='center'><span style='color: black'>Scotland Cumulative Confirmed Cases</span><h4>"),
                                                                    textOutput(outputId = "SL"),
                                                                    tags$head(tags$style("#SL{color: red; font-size: 60px; font-style: bold; text-align:center;}")),
                                                                    htmlOutput(outputId = "SLDiff"),
                                                                    tags$head(tags$style("#SLDiff{font-size: 30px; font-style: bold; text-align:center;}")),
                                                                    sliderInput(inputId = "date4Scotland", 
                                                                                label = shiny::HTML("<p><span style='color: black'>Date</span></p>"),
                                                                                min = min(unique(UKCoVLong$Date)),
                                                                                max = max(unique(UKCoVLong$Date)),
                                                                                step = 1,
                                                                                animate = T,
                                                                                value = min(unique(UKCoVLong$Date))
                                                                                )
                                                                    ),
                                                                  wellPanel(
                                                                    tags$style(type="text/css", "ScotlandTable.recalculating { opacity: 1.0; }"),
                                                                    DT::dataTableOutput("ScotlandTable")
                                                                  )
                                                                  ),
                                                                absolutePanel(
                                                                  bottom = 10, right = 20,
                                                                  shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Source: Scottish Goverment","</span><h5>")),
                                                                  shiny::HTML("<h5 align='right'><span style='color: white'>Note: The number for some health boards in Scotland are incorrect on 8 and 9 April 2020. Please check 'Q&A' (under 'About') for more information.</span><h5>"),
                                                                  shiny::HTML(paste0("<h5 align='right'><span style='color: white'>Updated: ", paste(as.character(Sys.Date()), time, "BST", sep = " "),"</span><h5>")),
                                                                  )
                                                                )
                                                            )
                                                          ),
                                                 # Wales
                                                 tabPanel("Wales",
                                                          fluidPage(
                                                            div(class="outer1",
                                                                tags$style(type = "text/css", ".outer1 {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                                tags$style(type="text/css", "Wales.recalculating { opacity: 1.0; }"),
                                                                leafletOutput("Wales", width = "100%", height = "100%"),
                                                                absolutePanel(
                                                                  top = 10, right = 10, width = 450,
                                                                  wellPanel(
                                                                    shiny::HTML("<h4 align='center'><span style='color: black'>Wales Cumulative Confirmed Cases</span><h4>"),
                                                                    textOutput(outputId = "wales"),
                                                                    tags$head(tags$style("#wales{color: red; font-size: 60px; font-style: bold; text-align:center;}")),
                                                                    htmlOutput(outputId = "WalesDiff"),
                                                                    tags$head(tags$style("#WalesDiff{font-size: 30px; font-style: bold; text-align:center;}")),
                                                                    sliderInput(inputId = "date4Wales", 
                                                                                label = shiny::HTML("<p><span style='color: black'>Date</span></p>"),
                                                                                min = min(unique(UKCoVLong$Date)),
                                                                                max = max(unique(UKCoVLong$Date)),
                                                                                step = 1,
                                                                                animate = T,
                                                                                value = as.Date("2020-03-19")
                                                                                ),
                                                                    ),
                                                                  wellPanel(
                                                                    tags$style(type="text/css", "WalesTable.recalculating { opacity: 1.0; }"),
                                                                    DT::dataTableOutput("WalesTable")
                                                                  )
                                                                  ),
                                                                absolutePanel(
                                                                  bottom = 10, right = 20,
                                                                  shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Source: Public Health Wales","</span><h5>")),
                                                                  shiny::HTML("<h5 align='right'><span style='color: white'>Please check 'Q&A' (under 'About') for more information.</span><h5>"),
                                                                  shiny::HTML(paste0("<h5 align='right'><span style='color: white'>Updated: ", paste(as.character(Sys.Date()), time, "BST", sep = " "),"</span><h5>")),
                                                                  )
                                                                )
                                                            )
                                                          )
                                                 )
                                     ),

# Maps: NHS Regions -------------------------------------------------------

                            tabPanel("NHS Regions",
                            tabsetPanel(type = "tabs",
                            #East of England
                            tabPanel("East of England",
                                     fluidPage(
                                       div(class="outer2",
                                           tags$style(type = "text/css", ".outer2 {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                           tags$style(type="text/css", "EastEngland.recalculating { opacity: 1.0; }"),
                                           leafletOutput("EastEngland", width = "100%", height = "100%"),
                                           absolutePanel(
                                             top = 10, right = 10, width = 450,
                                             wellPanel(
                                               shiny::HTML("<h4 align='center'><span style='color: black'>Cumulative Confirmed Cases</span><h4>"),
                                               textOutput(outputId = "eastengland"),
                                               tags$head(tags$style("#eastengland{color: red; font-size: 60px; font-style: bold; text-align:center;}")),
                                               htmlOutput(outputId = "EastEnglandDiff"),
                                               tags$head(tags$style("#EastEnglandDiff{font-size: 30px; font-style: bold; text-align:center;}")),
                                               sliderInput(inputId = "date4EastEngland",
                                                           label = shiny::HTML("<p><span style='color: black'>Date</span></p>"),
                                                           min = min(unique(UKCoVLong$Date)),
                                                           max = max(unique(UKCoVLong$Date)),
                                                           step = 1,
                                                           animate = T,
                                                           value = min(unique(UKCoVLong$Date))
                                                           )
                                               ),
                                             wellPanel(
                                               tags$style(type="text/css", "EastEngTable.recalculating { opacity: 1.0; }"),
                                               DT::dataTableOutput("EastEngTable")
                                               )
                                             ),
                                           absolutePanel(
                                             bottom = 10, right = 20,
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Source: Public Health England","</span><h5>")),
                                             shiny::HTML("<h5 align='right'><span style='color: white'>Please check 'Q&A' (under 'About') for more information.</span><h5>"),
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>Updated: ", paste(as.character(Sys.Date()), time, "BST", sep = " "),"</span><h5>")),
                                             )
                                           )
                                       )
                                     ),
                            #London
                            tabPanel("London",
                                     fluidPage(
                                       div(class="outer2",
                                           tags$style(type = "text/css", ".outer2 {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                           tags$style(type="text/css", "London.recalculating { opacity: 1.0; }"),
                                           leafletOutput("London", width = "100%", height = "100%"),
                                           absolutePanel(
                                             top = 10, right = 10, width = 450,
                                             wellPanel(
                                               shiny::HTML("<h4 align='center'><span style='color: black'>Cumulative Confirmed Cases</span><h4>"),
                                               textOutput(outputId = "london"),
                                               tags$head(tags$style("#london{color: red; font-size: 60px; font-style: bold; text-align:center;}")),
                                               htmlOutput(outputId = "LondonDiff"),
                                               tags$head(tags$style("#LondonDiff{font-size: 30px; font-style: bold; text-align:center;}")),
                                               sliderInput(inputId = "date4London",
                                                           label = shiny::HTML("<p><span style='color: black'>Date</span></p>"),
                                                           min = min(unique(UKCoVLong$Date)),
                                                           max = max(unique(UKCoVLong$Date)),
                                                           step = 1,
                                                           animate = T,
                                                           value = min(unique(UKCoVLong$Date))
                                                           ),
                                               ),
                                             wellPanel(
                                               tags$style(type="text/css", "LondonTable.recalculating { opacity: 1.0; }"),
                                               DT::dataTableOutput("LondonTable")
                                             )
                                             ),
                                           absolutePanel(
                                             bottom = 10, right = 20,
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Source: Public Health England","</span><h5>")),
                                             shiny::HTML("<h5 align='right'><span style='color: white'>Please check 'Q&A' (under 'About') for more information.</span><h5>"),
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>Updated: ", paste(as.character(Sys.Date()), time, "BST", sep = " "),"</span><h5>")),
                                             )
                                           )
                                       )
                                     ),
                            #Midlands
                            tabPanel("Midlands",
                                     fluidPage(
                                       div(class="outer2",
                                           tags$style(type = "text/css", ".outer2 {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                           tags$style(type="text/css", "Midlands.recalculating { opacity: 1.0; }"),
                                           leafletOutput("Midlands", width = "100%", height = "100%"),
                                           absolutePanel(
                                             top = 10, right = 10, width = 450,
                                             wellPanel(
                                               shiny::HTML("<h4 align='center'><span style='color: black'>Cumulative Confirmed Cases</span><h4>"),
                                               textOutput(outputId = "midlands"),
                                               tags$head(tags$style("#midlands{color: red; font-size: 60px; font-style: bold; text-align:center;}")),
                                               htmlOutput(outputId = "MidlandsDiff"),
                                               tags$head(tags$style("#MidlandsDiff{font-size: 30px; font-style: bold; text-align:center;}")),
                                               sliderInput(inputId = "date4Midlands",
                                                           label = shiny::HTML("<p><span style='color: black'>Date</span></p>"),
                                                           min = min(unique(UKCoVLong$Date)),
                                                           max = max(unique(UKCoVLong$Date)),
                                                           step = 1,
                                                           animate = T,
                                                           value = min(unique(UKCoVLong$Date))
                                                           ),
                                               ),
                                             wellPanel(
                                               tags$style(type="text/css", "MidlandsTable.recalculating { opacity: 1.0; }"),
                                               DT::dataTableOutput("MidlandsTable")
                                             )
                                             ),
                                           absolutePanel(
                                             bottom = 10, right = 20,
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Source: Public Health England","</span><h5>")),
                                             shiny::HTML("<h5 align='right'><span style='color: white'>Please check 'Q&A' (under 'About') for more information.</span><h5>"),
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>Updated: ", paste(as.character(Sys.Date()), time, "BST", sep = " "),"</span><h5>")),
                                             )
                                           )
                                       )
                                     ),
                            #North East and Yorkshire
                            tabPanel("North East and Yorkshire",
                                     fluidPage(
                                       div(class="outer2",
                                           tags$style(type = "text/css", ".outer2 {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                           tags$style(type="text/css", "NorthEast.recalculating { opacity: 1.0; }"),
                                           leafletOutput("NorthEast", width = "100%", height = "100%"),
                                           absolutePanel(
                                             top = 10, right = 10, width = 450,
                                             wellPanel(
                                               shiny::HTML("<h4 align='center'><span style='color: black'>Cumulative Confirmed Cases</span><h4>"),
                                               textOutput(outputId = "northeast"),
                                               tags$head(tags$style("#northeast{color: red; font-size: 60px; font-style: bold; text-align:center;}")),
                                               htmlOutput(outputId = "NorthEastDiff"),
                                               tags$head(tags$style("#NorthEastDiff{font-size: 30px; font-style: bold; text-align:center;}")),
                                               sliderInput(inputId = "date4NorthEast",
                                                           label = shiny::HTML("<p><span style='color: black'>Date</span></p>"),
                                                           min = min(unique(UKCoVLong$Date)),
                                                           max = max(unique(UKCoVLong$Date)),
                                                           step = 1,
                                                           animate = T,
                                                           value = min(unique(UKCoVLong$Date))
                                                           ),
                                               ),
                                             wellPanel(
                                               tags$style(type="text/css", "NorthEastTable.recalculating { opacity: 1.0; }"),
                                               DT::dataTableOutput("NorthEastTable")
                                             )
                                             ),
                                           absolutePanel(
                                             bottom = 10, right = 20,
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Source: Public Health England","</span><h5>")),
                                             shiny::HTML("<h5 align='right'><span style='color: white'>Please check 'Q&A' (under 'About') for more information.</span><h5>"),
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>Updated: ", paste(as.character(Sys.Date()), time, "BST", sep = " "),"</span><h5>")),
                                             )
                                           )
                                       )
                                     ),
                            #North West
                            tabPanel("North West",
                                     fluidPage(
                                       div(class="outer2",
                                           tags$style(type = "text/css", ".outer2 {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                           tags$style(type="text/css", "NorthWest.recalculating { opacity: 1.0; }"),
                                           leafletOutput("NorthWest", width = "100%", height = "100%"),
                                           absolutePanel(
                                             top = 10, right = 10, width = 450,
                                             wellPanel(
                                               shiny::HTML("<h4 align='center'><span style='color: black'>Cumulative Confirmed Cases</span><h4>"),
                                               textOutput(outputId = "northwest"),
                                               tags$head(tags$style("#northwest{color: red; font-size: 60px; font-style: bold; text-align:center;}")),
                                               htmlOutput(outputId = "NorthWestDiff"),
                                               tags$head(tags$style("#NorthWestDiff{font-size: 30px; font-style: bold; text-align:center;}")),
                                               sliderInput(inputId = "date4NorthWest",
                                                           label = shiny::HTML("<p><span style='color: black'>Date</span></p>"),
                                                           min = min(unique(UKCoVLong$Date)),
                                                           max = max(unique(UKCoVLong$Date)),
                                                           step = 1,
                                                           animate = T,
                                                           value = min(unique(UKCoVLong$Date))
                                               ),
                                             ),
                                             wellPanel(
                                               tags$style(type="text/css", "NorthWestTable.recalculating { opacity: 1.0; }"),
                                               DT::dataTableOutput("NorthWestTable")
                                             )
                                           ),
                                           absolutePanel(
                                             bottom = 10, right = 20,
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Source: Public Health England","</span><h5>")),
                                             shiny::HTML("<h5 align='right'><span style='color: white'>Please check 'Q&A' (under 'About') for more information.</span><h5>"),
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>Updated: ", paste(as.character(Sys.Date()), time, "BST", sep = " "),"</span><h5>")),
                                             )
                                           )
                                       )
                                     ),
                            #South East
                            tabPanel("South East",
                                     fluidPage(
                                       div(class="outer2",
                                           tags$style(type = "text/css", ".outer2 {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                           tags$style(type="text/css", "SouthEast.recalculating { opacity: 1.0; }"),
                                           leafletOutput("SouthEast", width = "100%", height = "100%"),
                                           absolutePanel(
                                             top = 10, right = 10, width = 450,
                                             wellPanel(
                                               shiny::HTML("<h4 align='center'><span style='color: black'>Cumulative Confirmed Cases</span><h4>"),
                                               textOutput(outputId = "southeast"),
                                               tags$head(tags$style("#southeast{color: red; font-size: 60px; font-style: bold; text-align:center;}")),
                                               htmlOutput(outputId = "SouthEastDiff"),
                                               tags$head(tags$style("#SouthEastDiff{font-size: 30px; font-style: bold; text-align:center;}")),
                                               sliderInput(inputId = "date4SouthEast",
                                                           label = shiny::HTML("<p><span style='color: black'>Date</span></p>"),
                                                           min = min(unique(UKCoVLong$Date)),
                                                           max = max(unique(UKCoVLong$Date)),
                                                           step = 1,
                                                           animate = T,
                                                           value = min(unique(UKCoVLong$Date))
                                               ),
                                             ),
                                             wellPanel(
                                               tags$style(type="text/css", "SouthEastTable.recalculating { opacity: 1.0; }"),
                                               DT::dataTableOutput("SouthEastTable")
                                             )
                                           ),
                                           absolutePanel(
                                             bottom = 10, right = 20,
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Source: Public Health England","</span><h5>")),
                                             shiny::HTML("<h5 align='right'><span style='color: white'>Please check 'Q&A' (under 'About') for more information.</span><h5>"),
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>Updated: ", paste(as.character(Sys.Date()), time, "BST", sep = " "),"</span><h5>")),
                                             )
                                           )
                                       )
                                     ),
                            #South West
                            tabPanel("South West",
                                     fluidPage(
                                       div(class="outer2",
                                           tags$style(type = "text/css", ".outer2 {position: fixed; top: 120px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                           tags$style(type="text/css", "SouthWest.recalculating { opacity: 1.0; }"),
                                           leafletOutput("SouthWest", width = "100%", height = "100%"),
                                           absolutePanel(
                                             top = 10, right = 10, width = 450,
                                             wellPanel(
                                               shiny::HTML("<h4 align='center'><span style='color: black'>Cumulative Confirmed Cases</span><h4>"),
                                               textOutput(outputId = "southwest"),
                                               tags$head(tags$style("#southwest{color: red; font-size: 60px; font-style: bold; text-align:center;}")),
                                               htmlOutput(outputId = "SouthWestDiff"),
                                               tags$head(tags$style("#SouthWestDiff{font-size: 30px; font-style: bold; text-align:center;}")),
                                               sliderInput(inputId = "date4SouthWest",
                                                           label = shiny::HTML("<p><span style='color: black'>Date</span></p>"),
                                                           min = min(unique(UKCoVLong$Date)),
                                                           max = max(unique(UKCoVLong$Date)),
                                                           step = 1,
                                                           animate = T,
                                                           value = min(unique(UKCoVLong$Date))
                                               ),
                                             ),
                                             wellPanel(
                                               tags$style(type="text/css", "SouthWestTable.recalculating { opacity: 1.0; }"),
                                               DT::dataTableOutput("SouthWestTable")
                                             )
                                           ),
                                           absolutePanel(
                                             bottom = 10, right = 20,
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>", "Source: Public Health England","</span><h5>")),
                                             shiny::HTML("<h5 align='right'><span style='color: white'>Please check 'Q&A' (under 'About') for more information.</span><h5>"),
                                             shiny::HTML(paste0("<h5 align='right'><span style='color: white'>Updated: ", paste(as.character(Sys.Date()), time, "BST", sep = " "),"</span><h5>")),
                                             )
                                           )
                                       )
                                     )
                            )
                            )
),

# Charts ------------------------------------------------------------------

                 navbarMenu("Charts",
                            icon = icon("chart-pie", lib = "font-awesome"),
                            tabPanel("Number of People Tested",
                                     fluidRow(
                                       splitLayout(cellWidths = c("50%", "50%"), plotlyOutput('CumulativeOV'), plotlyOutput('DailyOV'))
                                       ),
                                     tags$br(),
                                     fluidPage(
                                       div(class='body',
                                           div(align='justify',
                                               tags$p("Data Source: ",
                                                      tags$a(href="https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases",
                                                             "Public Health England (PHE)")
                                                      ),
                                               tags$br(),
                                               tags$p(tags$u("Note")),
                                               tags$p("The graphs above show the cumulative and daily number of people tested respectively. User can remove the bars in the plot by clicking on the corresponding legends (under the title)."),
                                               tags$p("(11 April) According to Public Health England, the daily number of positive tests is 5,234. However, the difference between the cumulative number of positive tests from yesterday and today 
                                                      is greater than 5,234 (shown on the right graph). Perhaps the cumulative number of positive tests does not exclude the results of swab testing. However, this requires further explanation
                                                      PHE, though it does not explain today. Hence, be careful of interpreting the the above charts."),
                                               tags$p("(10 April) According to Public Health England, Today’s figures for positive tests have been adjusted to include positive case results from swab testing for key workers and their households.
                                                      These will be included in the daily figures from today, 10 April. If these results were excluded from the figures, as they have been previously, the daily increase in the number of people 
                                                      who tested positive would have been 5,195. Hence, there are some numerical differences in the daily number of people tested. To get the daily number of people tested of 13,543, I would adjust
                                                      the total number of people tested on 9 April. However, that figure is correct and in line with daily number of people tested on 8 April. Hence, be careful of interpreting the the above 
                                                      charts."),
                                               tags$p("(8 April) According to Public Health England, the daily number of people tested on 8 April is 12,959. Hence, the cumulative number of people tested on 7 April is updated to 219,749, 
                                               instead of 213,181. The daily number of people tested on 7 April should be 10,912 instead of 4,344. The daily number of people who have negative results on 7 April should be 164,507 instead of 
                                                      157,939."),
                                               tags$p("(8 April) According to Public Health England, the figure on 8 April 2020 for people does not include Charing Cross and Southampton due to a data processing delay. The tests concluded 
                                                      figure excludes data from Northern Ireland."),
                                               tags$p("(7 April) According to Public Health England, the figure on 7 April 2020 for people tested does not include Manchester and Leeds due to a data processing delay. The tests concluded 
                                                      figure excludes data from Northern Ireland.")
                                               )
                                           )
                                       )
                                     ),
                            tabPanel("Number of Deaths",
                                     fluidRow(
                                       splitLayout(cellWidths = c("50%", "50%"), plotlyOutput('CumulativeDeaths'), plotlyOutput('DailyDeaths'))
                                       ),
                                     tags$br(),
                                     fluidPage(
                                       div(class='body',
                                           div(align='justify',
                                               tags$p("Data Source: ",
                                                      tags$a(href="https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/",
                                                             "NHS England"), ", ",
                                                      tags$a(href="https://www.publichealth.hscni.net/publications/covid-19-surveillance-reports",
                                                             "HSC Public Health Agency"), ", ",
                                                      tags$a(href="https://www.gov.scot/coronavirus-covid-19/",
                                                             "Scottish Government"), " and ",
                                                      tags$a(href="https://phw.nhs.wales/topics/latest-information-on-novel-coronavirus-covid-19/",
                                                             "Public Health Wales")
                                               ),
                                               tags$br(),
                                               tags$p(tags$u("Note")),
                                               tags$p("The graphs above show the cumulative and daily number of deaths in all constituent countries respectively. User can remove the bars in the plot by clicking on the corresponding legends 
                                                      (under the title)."),
                                               tags$p("According to Public Health England, data on UK positive and negative tests and data on deaths is updated on this page daily at 2pm or shortly after."),
                                               tags$p(tags$u("The following is the excerpt from Public Health England regarding the death figures (retrieved on 7 April):")),
                                               tags$p("The figures on deaths relate in almost all cases to patients who have died in hospital and who have tested positive for COVID-19. Slight differences in reporting in devolved 
                                                      administrations may mean that they include a small number of deaths outside hospital. The figures are compiled from validated data provided by NHS England and Improvement, Health Protection 
                                                      Scotland, Public Health Wales and the Public Health Agency (Northern Ireland)."),
                                               tags$br(),
                                               tags$p("These figures do not include deaths outside hospital, such as those in care homes, except as indicated above. This approach allows us to compile deaths data on a daily basis using up-to-date 
                                                      figures. The data includes confirmed cases reported as at 5pm the previous day. The amount of time between occurrence of death and reporting in these figures may vary slightly and in some 
                                                      cases could be a few days, so figures at 5pm may not include all deaths for that day."),
                                               tags$br(),
                                               tags$p("In addition to these figures, the Office for National Statistics (ONS) publishes weekly counts of deaths in which COVID-19 was mentioned on the death certificate. This publication is issued 
                                                      every Tuesday, starting on 31 March. It includes cases outside hospital and also some cases where COVID-19 is suspected but no test has taken place. ONS data will initially cover England and 
                                                      Wales only and will report on deaths registered up to 11 days before publication, so up to 20 March for their first release."),
                                           )
                                       )
                                     )
                                     )
                            ),

                 navbarMenu("Tables",
                            icon=icon("table"),
                            tabPanel("Confirmed Cases",
                                     fluidRow(
                                       column(3,
                                              selectInput(inputId = "source",
                                                          label = "Source",
                                                          choices = UKCoV$Source,
                                                          #selected = "Coventry",
                                                          multiple = TRUE)),
                                       column(3,
                                              selectInput(inputId = "nhscategory",
                                                          label = "NHS Category",
                                                          choices = unique(UKCoV$NHSCategory),
                                                          selected = "Midlands",
                                                          multiple = TRUE)),
                                       column(3,
                                              selectInput(inputId = "countries",
                                                          label = "Countries",
                                                          choices = unique(UKCoV$Countries),
                                                          selected = "England",
                                                          multiple = TRUE)),
                                       tags$p("Data source: ",
                                              tags$a(href='https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases','Public Health England')
                                              )
                                     ),
                                     hr(),
                                     DT::dataTableOutput("dataset")
                            ),
                            tabPanel("Number of People Tested & Deaths",
                                     fluidPage(
                                       div(class='body', 
                                           tags$div(align='justify',
                                                    tags$p("Data source: ",
                                                           tags$a(href='https://www.gov.uk/government/publications/covid-19-track-coronavirus-cases','Public Health England')
                                                           )
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
                                     ),
                                     
                            )
                 ),
                navbarMenu("About",
                           icon = icon("question-circle", lib = "font-awesome"),
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
                                                           tags$br(),
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
                          icon = icon("book-open", lib = "font-awesome"),
                          fluidPage(
                            div(class="body",
                              tags$div(align="justify",
                                       tags$h2("Update History"),
                                       tags$br(),
                                       includeHTML("log.html"),
                                       )
                              )
                            )
                         )
)

