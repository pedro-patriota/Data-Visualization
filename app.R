
#https://www.kaggle.com/datasets/sumanthvrao/daily-climate-time-series-data?resource=download
library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(lubridate)
library(psych)

master_df <- read.csv("DailyDelhiClimateTrain.csv")
data_types <- c('meantemp', 'humidity', 'wind_speed', 'meanpressure')
master_df <- drop_na(master_df)

master_df$date <- as.Date(master_df$date, format = '%Y-%m-%d')
# Define UI for application that draws a histogram

getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

get_mode<-function(x){
  which.max(tabulate(x)
            )}

ui <- dashboardPage(
  
  dashboardHeader(title = "Data Project"),
  dashboardSidebar(sidebarMenu(
    menuItem("Single data visualization", tabName = "single", icon = icon("chart-line")),
    menuItem('Comparing Classes', tabName = 'compare', icon = icon('chart-bar'))
  )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "single",
              fluidRow(
                box(title = "Inputs", width = (NULL),
                    box(title = "Choose your data",
                        selectInput(inputId = "user_choice", label = "Classes", data_types),
                        
                    ),
                    box(title = "Period of time",
                        
                        dateRangeInput(inputId = "range_date", label = "Choose your range",
                                       start = "2013-01-01", end = "2016-02-01",min = "2013-01-01", max = "2017-01-01",
                                       format = "yyyy-mm-dd", language = "pt")
                 
                    ),
                    actionButton(inputId = "go", label = "RUN")
                    
                )
                
              ),
              fluidRow(
                box(title = "Time series analysis - Line Graph",
                    width = (NULL),
                    plotOutput("line")),
                box(title = "Time series analysis - Histogram",
                    width = (NULL),
                    plotOutput("histo")),
                box(title = "Time series analysis - Boxplot",
                    width = (NULL),
                    plotOutput("box")),
                box(title = "Summary",
                    width = (NULL),
                    verbatimTextOutput("sum"),
                    h6("Mode:"),
                    textOutput("mode"))
              )
              
              
      )
      
    )
    
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  select_classes <- eventReactive(input$go, {
    class_name <- input$user_choice
    start <- input$range_date[1]
    end <- input$range_date[2]
    aux <- master_df %>%
      
      filter(date >= start & date <= end)
    
    aux <- select(aux, 1, class_name)
    return(aux)
    
  })
  
  only_the_data <- eventReactive(input$go, {
    class_name <- input$user_choice
    start <- input$range_date[1]
    end <- input$range_date[2]
    aux <- master_df %>%
      
      filter(date >= start & date <= end)
    aux <- select(aux, class_name)
    return(aux)
    
  })
  
  choose_value <- eventReactive(input$go, {
    class_name <- input$user_choice
    return(class_name)
  })
  num_breaks <- eventReactive(input$go, {
    start <- input$range_date[1]
    end <- input$range_date[2]
    
    difference <- as.numeric(difftime(end, start, unit = "days"))
    if (abs(difference) > 365){
      return ("8 months") 
    } else if (abs(difference) > 120){
      return ("1 month")
    } else if (abs(difference) > 80){
      return ("2 weeks")
    } else if (abs(difference) > 40){
      return ("1 weeks")
    } else if (abs(difference) > 10){
      return ("4 days")
    } else{
      return ("1 day")
    }
    
  })
  
  
  
  output$line <- renderPlot({
      
      df <- select_classes()
      value <- choose_value()
      names (df)[2] <- 'answer'   #as.character()
      
      k <- num_breaks()
      df$season <-getSeason(df$date)
      df$season <- as.factor(df$season)
    
      theme_set(theme_bw())
      ggplot(df) +
        geom_line(
          mapping = aes(x = date, y = answer, color = season, group = 1),
          size = 1
        )+
        labs(x = "Period of time", y = value)+
        theme(axis.title.x = element_text(vjust = 0, size = 15),
              axis.title.y = element_text(vjust = 2, size = 15))+
        scale_x_date(date_labels = "%Y-%m-%d")+
        scale_x_date(date_breaks = k)

   })
  
  output$histo <- renderPlot({
    df <- select_classes()
    value <- choose_value()
    
    names (df)[2] <- 'answer'   #as.character()
    
    max <- max(df$answer)
    min <- min(df$answer)
   
    
    theme_set(theme_bw())
    ggplot(df) +
      geom_histogram(
        mapping = aes(x = answer, group = 1),
        color = "black",
        fill = "gray"
      )+
      labs(x = value, y = "Occurencies")+
      theme(axis.title.y = element_text(vjust = 2, size = 15))


  })
  
  output$box <- renderPlot({
    df <- select_classes()
    
    value <- choose_value()
    names (df)[2] <- 'answer'   #as.character()
    
    
    df$season <-getSeason(df$date)
    df$season <- as.factor(df$season)
    
    theme_set(theme_bw())
    
    ggplot(df) +
      geom_boxplot(
        mapping = aes(x = season, y = answer, color = season)

      )+
      geom_dotplot(mapping = aes(x = season, y = answer), binaxis='y', stackdir='center', dotsize=0.1)+
      labs(x = "Seasons", y = value)
    
      
  })
  output$sum <- renderPrint({
    df <- only_the_data()
    describe(df, fast = TRUE)
  })
  output$mode <- renderText({
    df <- only_the_data()
    names (df)[1] <- 'answer'
    
    get_mode(df$answer)

  })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
