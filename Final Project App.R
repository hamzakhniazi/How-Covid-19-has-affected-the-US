library(shiny)
library(usmap) 
library(ggplot2)
library(dplyr)


counties1= read.csv("/Users/hamzasultankhanniazi/Downloads/us-counties.csv")
counties1 = na.omit(counties1)



ui = fluidPage(
  titlePanel("How Covid-19 has affected the US"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("You can choose from the different variables and date range to see how Covid-19 has affected each state since the outbreak."),
      
      selectInput("var", label = "Select Variable", choices = c("Total Deaths", "Total Positive Cases", "Percentage of people died after testing positive", "Percentage of people recovered after testing positive")),
      
      
      dateRangeInput("date", label = "Date Range", start = "2020-02-04", end = "2020-04-04", min = "2020-01-21", max = "2021-12-08")
    ),
    
    
    
    mainPanel(plotOutput("map"))
  )
)

server = function(input, output) {
  output$map = renderPlot({
    
    counties1 = counties1[counties1$date >= input$date[1] & counties1$date <= input$date[2],]
    counties1 = subset(counties1, select = -c(county,fips) )
    counties1=counties1[order(counties1$state),]
    un=unique(counties1$state)
    final=data.frame(un)
    names(final)[1] = "state"
    
    n=1
    total_deaths =0
    for (i in 1:length(counties1$state)){
      if (counties1$state[i] == un[n]){
        total_deaths = (total_deaths +  as.integer( counties1$deaths[i]))
        final$deaths[n] = total_deaths
      }
      else if (counties1$state[i+1] == un[n+1]){
        final$deaths[n] = total_deaths
        total_deaths=0
        n=n+1
      }
    }
    
    n=1
    total_cases =0
    for (i in 1:length(counties1$state)){
      if (counties1$state[i] == un[n]){
        total_cases = (total_cases +  as.integer( counties1$cases[i]))
        final$cases[n] = total_cases
      }
      else if (counties1$state[i+1] == un[n+1]){
        final$cases[n] = total_cases
        total_cases=0
        n=n+1
      }
    }
    
    final$perc_died = ((final$deaths/final$cases)*100)
    final$perc_surv = (100 - ((final$deaths/final$cases)*100))
    
    if (input$var == "Total Deaths"){
      plot_usmap(data = final, values = "deaths", regions = c("states")) + 
        scale_fill_continuous(low = "white", high = "red", name = "Deaths due to Covid-19", label = scales::comma) + 
        theme(panel.background=element_blank())
    } else if (input$var == "Total Positive Cases"){
      plot_usmap(data = final, values = "cases", regions = c("states")) + 
        scale_fill_continuous(low = "white", high = "blue", name = "Positive Covid-19 cases", label = scales::comma) + 
        theme(panel.background=element_blank())
    } else if (input$var == "Percentage of people died after testing positive"){
      plot_usmap(data = final, values = "perc_died", regions = c("states")) + 
        scale_fill_continuous(low = "white", high = "orange", name = "Percentage of people died after testing positive", label = scales::comma) + 
        theme(panel.background=element_blank())
    } else if (input$var == "Percentage of people recovered after testing positive"){
      plot_usmap(data = final, values = "perc_surv", regions = c("states")) + 
        scale_fill_continuous(low = "white", high = "green", name = "Percentage of people recovered after testing positive", label = scales::comma) + 
        theme(panel.background=element_blank())
    }
  })
}

shinyApp(ui, server)