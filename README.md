## COVID-19 Map of United States

[View Application](https://vanvurenl.shinyapps.io/covid_visualization/?_ga=2.267778314.581288874.1612146626-1614514016.1607710385)

**Project description:** This COVID-19 Map for the United States is a project that I undertook as part of a Data Science class. I aimed to make a Shiny app which made it simple for the user to generate a map with different variables. This was made possible by using the Shiny library in R and taking advantage of its page elements, such as drop-downs, sliders, and graph plots. I am grateful to the New York Times for making its current COVID data publically available which I use to automatically update the applications dataset. You can view the final application [here,](https://vanvurenl.shinyapps.io/covid_visualization/?_ga=2.267778314.581288874.1612146626-1614514016.1607710385) and the code [here](https://github.com/Prizm15/COVIDMap).

## Static_Plot.Rmd

This is the R Markdown document which I used to initially create my plots and functions that I would eventually use in the server.

## app.R

This is the final Shiny application that is currently being deployed to the server. 

**Skills:**
* R and Shiny
* Data analytics
* Statistical equations
* Real-time data retrieval

### 1. Initial plots with R

After cleaning the data, I started with a few quick static plots, including the following one using cases per capita. 

```r
# Graph of cases_per_cap
left_join(us_states, current, by=c("fips_state" = "fips")) %>% 
ggplot() +
  geom_sf(aes(fill=Cases_Per_Capita)) +
  scale_fill_continuous(low="yellow", high="red", labels = scales::percent)
```
<img src="images/casescap_covidmap.PNG?raw=true"/>

### 2. Transforming the plot into a plotly object

I then created a function which takes an user-inputted statistic as a parameter which creates a plotly object based on the static graph created by ggplot. 

```r
# Graph from function
COVIDPlot <- function(data) {
  p <- data %>% 
    ggplot() + 
    geom_sf(aes(fill=plot_stat, geometry=geometry, text=text)) +
    theme_minimal() +
    scale_fill_continuous(low="yellow", high="red")
  
  ggplotly(p, tooltip="text") %>% 
  style(hoveron="fills") 
}

COVIDPlot(filter_date(Deaths_Per_Capita, today))
```
<img src="images/plotly_covidmap.PNG?raw=true"/>

### 3. Migrating to a Shiny application

Finally, I migrated the code to a Shiny application then deployed it to a server. The final Shiny application can be viewed [here,](https://vanvurenl.shinyapps.io/covid_visualization/?_ga=2.267778314.581288874.1612146626-1614514016.1607710385) and the code [here!](https://github.com/Prizm15/COVIDMap)

<img src="images/shinyappdemo.PNG?raw=true"/>
