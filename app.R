
#https://www.kaggle.com/datasets/sumanthvrao/daily-climate-time-series-data?resource=download
#Libraries

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(lubridate)
library(psych)
library(reshape2)


master_df <- read.csv("DailyDelhiClimateTrain.csv") # Getting the file
data_types <- c('meantemp', 'humidity', 'wind_speed', 'meanpressure') # data types list
master_df <- drop_na(master_df) # deletes blank spaces

master_df$date <- as.Date(master_df$date, format = '%Y-%m-%d') # makes date readable

getSeason <- function(input.date){ # Gets the dates and transforms it to season
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

get_mode<-function(x){ # gets the mode
  which.max(tabulate(x)
            )}

ui <- dashboardPage(
  
  dashboardHeader(title = "Data Project"),
  dashboardSidebar(sidebarMenu( # Side Bar
    menuItem("Single data visualization", tabName = "single", icon = icon("chart-line")),
    menuItem('Comparing Classes', tabName = 'compare', icon = icon('chart-bar'))
  )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "single",
              fluidRow(
                box(title = "Inputs", width = (NULL), # gets the input
                    box(title = "Choose your data",
                        selectInput(inputId = "user_choice", label = "Classes", data_types),
                        
                    ),
                    box(title = "Period of time", # Gets the date input
                        
                        dateRangeInput(inputId = "range_date", label = "Choose your range",
                                       start = "2013-01-01", end = "2016-02-01",min = "2013-01-01", max = "2017-01-01",
                                       format = "yyyy-mm-dd", language = "pt")
                 
                    ),
                    actionButton(inputId = "go", label = "RUN") # Button
                    
                )
                
              ),
              fluidRow( # UI for the different types of graphs
                box(title = "Time series analysis - Line Graph", 
                    width = (NULL),
                    plotOutput("line")),
                box(title = "Time series analysis - Histogram",
                    width = (NULL),
                    plotOutput("histo")),
                box(title = "Time series analysis - Boxplot",
                    width = (NULL),
                    plotOutput("box")),
                box(title = "Summary", # A group of statistic data
                    width = (NULL),
                    verbatimTextOutput("sum"),
                    h6("Mode:"),
                    textOutput("mode"))
              )
              
              
      ),
      tabItem(tabName = "compare",
              fluidRow(
                box(title = "Inputs", width = (NULL), # Gets the input
                    box(title = "Choose your data",
                        selectInput(inputId = "user_choice1", label = "First Class", data_types, selected = 'wind_speed'),
                        selectInput(inputId = "user_choice2", label = "Second Class", data_types)
                    ),
                    box(title = "Period of time", # Gets the date
                        
                        dateRangeInput(inputId = "compare_range_date", label = "Choose your range",
                                       start = "2013-01-01", end = "2016-02-01",min = "2013-01-01", max = "2017-01-01",
                                       format = "yyyy-mm-dd", language = "pt")
                        
                    ),
                    actionButton(inputId = "compare_go", label = "RUN")
                    
                ), 
                fluidRow( # Ui for different types of graphs
                  box(title = "Time series analysis - Line Graph",
                      width = (NULL),
                      plotOutput("compare_line")),
                  box(title = "Time series analysis - Correlation",
                      width = (NULL),
                      plotOutput("compare_correlation")),
                  
                  box(title = "Correlation Matrix",
                      width = (NULL),
                      verbatimTextOutput("correlation")),
                  
                  box(title = "Time series analysis - Mean Bars",
                      width = (NULL),
                      plotOutput("bar")),
                 
                  box(title = "Time series analysis - Scatter plot",
                      width = (NULL),
                      plotOutput("compare_scatter")),
                  
                  box(title = "Summary",
                      width = (NULL),
                      verbatimTextOutput("compare_sum"))
                      
                )
                
              )
        
      )
    )
    
    
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  select_classes <- eventReactive(input$go, { # Filters the dataframe by the classes and time
    # Selected
    class_name <- input$user_choice
    start <- input$range_date[1]
    end <- input$range_date[2]
    aux <- master_df %>%
      
      filter(date >= start & date <= end)
    
    aux <- select(aux, 1, class_name)
    return(aux)
    
  })
  
  only_the_data <- eventReactive(input$go, { # Filters the dataframe only by the classes
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
  
  num_breaks <- eventReactive(input$go, { # Standard separation between periods of time
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
      value <- choose_value() # Gets the correct data frame and input
      names (df)[2] <- 'answer' # Changes the name of the column
      
      k <- num_breaks() 
      df$season <-getSeason(df$date) # Gets the season
      df$season <- as.factor(df$season)
    
      theme_set(theme_bw())
      ggplot(df) +
        geom_line(
          mapping = aes(x = date, y = answer, color = season, group = 1), # Plots the line graph
          size = 1
        )+
        labs(x = "Period of time", y = value)+
        theme(axis.title.x = element_text(vjust = 0, size = 15),
              axis.title.y = element_text(vjust = 2, size = 15))+
        scale_x_date(date_labels = "%Y-%m-%d")+ # Transforms the x axis to a date model
        scale_x_date(date_breaks = k)

   })
  
  output$histo <- renderPlot({
    df <- select_classes()
    value <- choose_value()  # Inputs
    
    names (df)[2] <- 'answer' # Changes the name of the column
    
    max <- max(df$answer) # Max and min values
    min <- min(df$answer)
   
    
    theme_set(theme_bw())
    ggplot(df) +
      geom_histogram( # Histogram graph
        mapping = aes(x = answer, group = 1),
        color = "black",
        fill = "gray" # palette = "Blues"
      )+
      labs(x = value, y = "Occurencies")+
      theme(axis.title.y = element_text(vjust = 2, size = 15))


  })
  
  output$box <- renderPlot({
    df <- select_classes()
    value <- choose_value()  # Inputs
    
    names (df)[2] <- 'answer' # Changes the name of the column
    
    
    df$season <-getSeason(df$date) # Gets the season
    df$season <- as.factor(df$season)
    
    theme_set(theme_bw())
    
    ggplot(df) +
      geom_boxplot(
        mapping = aes(x = season, y = answer, color = season) # Boxplot graph

      )+
      geom_dotplot(mapping = aes(x = season, y = answer), binaxis='y', stackdir='center', dotsize=0.1)+
      labs(x = "Seasons", y = value)
    
      
  })
  output$sum <- renderPrint({ # Summary
    df <- only_the_data()
    describe(df, fast = TRUE)
  })
  output$mode <- renderText({ # Mode
    df <- only_the_data()
    names (df)[1] <- 'answer'
    
    get_mode(df$answer)

  })
  
  
  select_classes_2 <- eventReactive(input$compare_go, { # Same logic as the first select_classes
    class_name1 <- input$user_choice1
    class_name2 <- input$user_choice2
    start <- input$compare_range_date[1]
    end <- input$compare_range_date[2]
    aux <- master_df %>%
      
      filter(date >= start & date <= end)
    
    aux <- select(aux, 1, class_name1, class_name2)
    return(aux)
    
  })
  
  num_breaks2 <- eventReactive(input$compare_go, { # Same logic as the num_breaks
    start <- input$compare_range_date[1]
    end <- input$compare_range_date[2]
    
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
  
  output$compare_line <- renderPlot({
    
    df <- select_classes_2()
    value1 <- input$user_choice1 # Inputs
    value2 <- input$user_choice2
    
    names (df)[2] <- 'answer1'   # Changes the name of the columns
    names (df)[3] <- 'answer2'
    
    k <- num_breaks2()
    df$season <-getSeason(df$date) # Gets season
    df$season <- as.factor(df$season)
    
    theme_set(theme_bw())
    ggplot(df, aes(x = date)) +
      geom_line(
        aes(y = answer1, colour = value1) # Multiple lines
      )+
      geom_line(
        aes(y = answer2, colour = value2)
      )+
      scale_y_continuous(sec.axis = sec_axis(~.*5, name = value1))+
      labs(x = "Time Serie", y = value2)+
      theme(axis.title.x = element_text(vjust = 0, size = 15),
            axis.title.y = element_text(vjust = 0, size = 15))+
      scale_x_date(date_labels = "%Y-%m-%d")+ # Date format
      scale_x_date(date_breaks = k)
    
  })
  
  compare_only_the_data <- eventReactive(input$compare_go, { # Filter the dataframe only by the classes
    class_name1 <- input$user_choice1
    class_name2 <- input$user_choice2
    start <- input$compare_range_date[1]
    end <- input$compare_range_date[2]
    aux <- master_df %>%
      
      filter(date >= start & date <= end)
    
    aux <- select(aux, class_name1, class_name2)
    return(aux)
  })
  
  
  output$correlation <- renderPrint({ # Correlation matrix
    df <- compare_only_the_data()
    cor(df)
    
  })
  output$compare_correlation <- renderPlot({
    df <- select_classes_2()
    value1 <- input$user_choice1 # Input
    value2 <- input$user_choice2
    
    names (df)[2] <- 'answer1'   # Changes the name of the columns
    names (df)[3] <- 'answer2'
    
    ggplot(df) +
      geom_line(
        mapping = aes(x = answer1, y = answer2), # Line graph
        colour = "cyan",
        size = 0.65
      )+
      labs(x = value1, y = value2)+
      theme(axis.title.x = element_text(vjust = 0, size = 15),
            axis.title.y = element_text(vjust = 2, size = 15))
    
  })
  


  output$compare_scatter <- renderPlot({
  
    df <- select_classes_2()  # Input
    value1 <- input$user_choice1
    value2 <- input$user_choice2
    
    names (df)[2] <- 'answer1'   # Changes the name of the columns
    names (df)[3] <- 'answer2'
    
    df$season <-getSeason(df$date)
    df$season <- as.factor(df$season)
    theme_set(theme_bw())
    
    ggplot(df, aes(x = answer1,y = answer2, color = season))+ # Scatter graph
      labs(x = value1, y = value2) +
      geom_point()+
      stat_smooth(method = "loess", formula = y ~ x, size = 1)

  })
  
  output$compare_sum <- renderPrint({ # Summary
    df <- compare_only_the_data()
    describe(df, fast = TRUE)
  })
  
  output$bar <- renderPlot({
      df <- select_classes_2()
      value1 <- input$user_choice1 # Input
      value2 <- input$user_choice2

      names (df)[2] <- 'answer1'   # Changes the name of the columns
      names (df)[3] <- 'answer2'
      df$season <-getSeason(df$date)
      df$season <- as.factor(df$season)
      theme_set(theme_bw())
      
      df2 <- melt(df [, c('season', 'answer1', 'answer2')], id.vars = 1)
      # Puts the class1 and class2 column together
      
      ggplot(df2, aes(x = season, y = value, fill = variable,))+ # Bar graph
      geom_bar(position = "dodge", stat = "summary", fun = "mean")+
      
      labs(y = "Means")+
      theme(axis.title.x = element_text(vjust = 0, size = 15),
            axis.title.y = element_text(vjust = 2, size = 15))+
        
      scale_fill_discrete(name = "Inputs", labels = c(value1, value2))
      
        
      
  })
  
}
# Runs the application 
shinyApp(ui = ui, server = server)
