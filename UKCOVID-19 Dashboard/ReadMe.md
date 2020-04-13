# UK COVID-19 Dashboard

Hello everyone! In this mini project I am presenting you an interactive dashboard about current COVID-19 situation in UK with Shiny.

* [Purposes](https://github.com/fendit/MP/tree/master/UKCOVID-19%20Dashboard#purposes)
* [Objectives](https://github.com/fendit/MP/tree/master/UKCOVID-19%20Dashboard#objectives)
* [Pre-requisite](https://github.com/fendit/MP/tree/master/UKCOVID-19%20Dashboard#pre-requisite)
* [Features](https://github.com/fendit/MP/tree/master/UKCOVID-19%20Dashboard#features)
* [Suggestions](https://github.com/fendit/MP/tree/master/UKCOVID-19%20Dashboard#suggestions)
* [References](https://github.com/fendit/MP/tree/master/UKCOVID-19%20Dashboard#references)

### Purposes
These R scripts are to show that:
1. how data visualisation can be done with packages such as Shiny, leaflet and plotly in R
1. how to set up a webpage with two .R files, namely 'ui.R' and 'server.R'

### Objectives
1. Create an interactive dashboard showing the current COVID-19 situation in UK (data starts from 2020-03-07)
1. Create an interactive and automated map for showing the situation with plotly
1. Create a website that allows other users to explore

### Pre-requisite
1. Download 'ui.R', 'server.R' and 'log.html'. The last one is simply a log of the update history
1. Download the related csv file from the website (https://ft3pi.shinyapps.io/covid-19_uk_shiny/)
1. Install packages such as 'shiny', 'leaflet', 'leaflet.extras', 'plotly', 'xts', 'tidyr' and 'dplyr'
1. Create an account on shinyapps.io (https://www.shinyapps.io) if setting up the website in the server

### Features
1. Maps with red circles showing the confirmed cases on the selected date
1. A range slider for changing the date within a specific time range
1. Situations in terms of constituent countries (England, Northern Ireland, Scotland and Wales) and NHS regions in England (East of England, London, Midlands, North East and Yorkshire, North West, South East and South West) are shown specifically
1. Confirmed cases are shown on the map as well as the ranked table/graph
1. Interactive bar charts for the cumulative and daily UK number of people tested
1. Datatables that allow users to search for a specific local authority/local government district/ health board
1. Downloadable datatables for replications
1. Source for the data and resources that are related to COVID-19 in UK
1. Update history

### Suggestions
1. Smartphone-user-friendly. (Perhaps using flexdashboard?)
1. News section such as what the recent news about COVID-19 in UK is. Possible adding a page that shows the tweets from public health departments
1. Timeline section for viewing the timeline of COVID-19, such as when the first patient in UK was found, when the lockdown was implemented, what significant measures the government has made in what time etc

### References
Most of the lines are copied from various sources. Here are some useful references for the packages I am using

1. https://shiny.rstudio.com/gallery/ (Shiny: an excellent overview for beginners to explore the unlimited potentials of creating website for data visualisation and interaction)
1. https://rstudio.github.io/leaflet/ (Leaflet: an breath-taking R package for showing geospatial data which is one of the most popular open-source JavaScript libraries for interactive maps. It's straightforward and user-friendly. Definitely recommend)
1. https://leaflet-extras.github.io/leaflet-providers/preview/ (leaflet. extras: 164 layers for the map. Which one is your favourite?)
1. https://plotly.com/r/ (Plotly: a R package for creating interactive plots. Never bored with it since there are unlimited ways of showing the same set of the data; the sky is the limit, though)
1. Data sources are [Public Health England](https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public), [NHS England](https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/), [HSC Public Health Agency](https://www.publichealth.hscni.net), [Scottish Government](https://www.gov.scot/coronavirus-covid-19/) and [Public Health Wales](https://covid19-phwstatement.nhs.wales)
