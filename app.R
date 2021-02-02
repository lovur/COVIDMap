# This application is a project of mine
# through which I learned about Shiny
# and using the plotly library to create
# some interesting visualizations.
# Author: Logan Van Vuren
# Feel free to reach out to me at
# vanvurenl1@gmail.com if you have
# any questions!

library(shiny)
library(tidyverse)
library(readr)
library(albersusa)
library(plotly)
library(rsconnect)

rsconnect::deployApp()

states_covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
state_population <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv") %>% select(NAME, POPESTIMATE2019)
us_states <- usa_sf("laea")
states_covid$date <- as.Date(states_covid$date)


# Combine States Pop and COVID Cases
states_covid_pop <- left_join(states_covid, state_population, by=c("state" = "NAME"))
states_covid_pop <- rename(states_covid_pop, "population" = "POPESTIMATE2019")

states_covid_pop <- states_covid_pop %>% mutate(Deaths_Per_Capita = deaths/population*100000, Cases_Per_Capita = cases/population*100000)
states_covid_pop <- states_covid_pop %>% rename("Deaths" = "deaths")
states_covid_pop <- states_covid_pop %>% rename("Cases" = "cases")


# Most recent data
earliest <- min(states_covid_pop$date)
today <- max(states_covid_pop$date)

# Graph from function
filter_date <- function(stat, mydate){
    my_stat <- enquo(stat)
    
    covid_oneday <- states_covid_pop %>%
        filter(date==as.Date(mydate)) %>%
        select(plot_stat = !!my_stat, fips, state) %>% 
        mutate(text=paste("<b>", state,"</b>\nValue:", prettyNum(signif(plot_stat, 4), big.mark = ",")))
    mapdata <- left_join(us_states, covid_oneday, by=c("fips_state"="fips"))
    m <- ggplot(mapdata) +
        geom_sf(aes(fill=plot_stat, text=text)) +
        labs(fill = "Value") +
        scale_fill_continuous(low="yellow", high="red")
    ggplotly(m, tooltip="text") %>%
        style(hoveron = "fill")
}

# Define UI for application that draws the graph
ui <- fluidPage(

    # Application title
    titlePanel("COVID Deaths and Cases in the United States"),

    # Sidebar with a slider input for date to display
    sidebarLayout(
        sidebarPanel(
            sliderInput("mydate",
                        "Select a date to display:",
                        min = as.Date(min(states_covid_pop$date),"%Y-%m-%d"),
                        max = as.Date(max(states_covid_pop$date), "%Y-%m-%d"),
                        value = as.Date(today),
                        timeFormat="%m-%d"),
            selectInput("my_stat",
                        "Statistic to Plot:",
                        choices = list("Deaths Per Capita" = "Deaths_Per_Capita",
                                       "Cases Per Capita" = "Cases_Per_Capita",
                                       "Deaths" = "Deaths",
                                       "Cases" = "Cases"),
                        selected = "Cases_Per_Capita"
                        )
            
        ),

        # Show a plot of the United States for the selected date and stat
        mainPanel(
            h3(textOutput("TitleText")),
           plotlyOutput("statPlot"),
           h5("Per capita stats are calculated using 2019 U.S. Census estimates per 100,000 people"),
           h5("Data sourced from the New York Times"),
           h6(tags$a(href="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", "Source"))
        )
    )
)

# Define server logic required to draw our COVID map
server <- function(input, output) {
    
    output$TitleText = renderText(paste("Cumulative", str_replace_all(input$my_stat, "_", " "), "on", input$mydate))

    output$statPlot <- renderPlotly({
        filter_date(stat = input$my_stat, input$mydate)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
