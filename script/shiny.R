
#### 0.   INCLUDES / PREPARING DATA _______________________________________ #### 

#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}

pacman::p_load(rstudioapi,dplyr, ggplot2, lubridate, randomForest, caret,
               rpart,rpart.plot,tidyr, shiny, shinydashboard, rvest, DT,
               ggthemes)

# Setwd (set current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)

# Let's read the script with the own functions
# source("./script/functions_sara.R")

# Introduce a meteosat link for extracting information about the weather
get.weather<- function(link){
  
  # Extracting data
  data <- read_html(link) 
  data <- html_table(data, fill=TRUE)  
  data <- data[[1]]  
  
  # Renaming columns
  colnames(data)<- c("Date", "Time", "Empty", "Temperature", "Wind", "Speed", 
                     "Precipitation")
  
  # Removing useless and empty rows. Also the empty column
  data<-data %>% filter(!grepl("google|Fin de semana", Empty)) %>%
    filter(!(Temperature=="")) %>% select(-Empty)
  
  # Fixing date: removing characters
  # ^ start of string  \\D+  one or more chars other than digit
  data$Date<- sub("^\\D+", "", data$Date)
  data$Date<-dmy(noquote(data$Date))
  
  # Fixing date: filling in the empty cells
  for(i in 1:length(data$Date)){
    if(is.na(data$Date[i])){
      data$Date[i]<-lag(data$Date)[i]
    }
  }
  
  # Fixing Time,  temperature, speed, precipitation and speed
  data<- data %>% filter(!grepl("de", Time))
  data$Time<- as.numeric(sub("\\ .*", "", data$Time))
  data$Temperature<- as.numeric(gsub("([0-9]+).*$", "\\1", data$Temperature))
  data$Speed<- as.numeric(gsub("([0-9]+).*$", "\\1", data$Speed))
  data$Precipitation<- as.numeric(gsub("([0-9]+).*$", "\\1", data$Precipitation))
  data$Wind<- as.factor(data$Wind)
  
  # Creating datetime
  # data$DateTime<- ymd_h(paste(data$Date, data$Time))
  # data<- data %>% select(-Time, -Date)
  
  return(data)
  
}

# Extracting data
Coruna<-get.weather("https://www.meteosat.com/tiempo/a-coruna/tiempo-a-coruna-la-coruna.html")
Cadiz<-get.weather("https://www.meteosat.com/tiempo/cadiz/tiempo-cadiz.html")
Barcelona<-get.weather("https://www.meteosat.com/tiempo/barcelona/tiempo-barcelona.html")

#### 1.   SHINY ___________________________________________________________ #### 

ui <- dashboardPage(
  
  # Title
  dashboardHeader(title = "Where should I go this weekend?"),
  
  # Sidebar 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Datasets", tabName = "datasets", icon =icon("table")),
      menuItem("Graphs", tabName = "graphs", icon =icon("bar-chart-o"),
      selected = TRUE)),   
    
    selectInput(inputId = "variable", label = "What would you like to know?",
                choices=c("Temperature","Speed","Precipitation"))
  ),
  
  # Body
  dashboardBody(
    tabItems(
      tabItem(tabName = "datasets",
      
        fluidRow(
          
          # cadiz
          column(width=4,
                 box(width = NULL,
                   title = "Cádiz",DT::dataTableOutput("dataCadiz")
                 )),
          
          # Barcelona
          column(width=4,
                 box(width = NULL,
                   title = "Barcelona",DT::dataTableOutput("dataBcn")
                 )),
          
          # Coruña
          column(width=4,
                 box(width = NULL,
                   title = "Coruña",DT::dataTableOutput("dataCoruña")
                 ))          
        )
    # 
      ),
      tabItem(tabName = "graphs",
        
          # cadiz
          fluidRow(
                 box(width = NULL, 
                     title = "Cádiz",plotOutput("plotCadiz",width="100%", 
                                                height=150)
                 )),
          
          # Barcelona
          fluidRow(
                 box(width = NULL,
                     title = "Barcelona",plotOutput("plotBcn",width="100%", 
                                                    height=150)
                 )),
          
          # Coruña
          fluidRow(
                 box(width = NULL,
                     title = "Coruña",plotOutput("plotCoruna", width="100%", 
                                                 height=150)
                 ))          
        )
        
           
    )                 
  )
)

server <- function(input, output, session) {
  
  observe({
    # Re-execute this reactive expression after 1000 milliseconds
    invalidateLater(3600000, session)
    
    # Do something each time this is invalidated.
    # The isolate() makes this observer _not_ get invalidated and re-executed
    # when input$n changes.
    Coruna<-get.weather("https://www.meteosat.com/tiempo/a-coruna/tiempo-a-coruna-la-coruna.html")
    Cadiz<-get.weather("https://www.meteosat.com/tiempo/cadiz/tiempo-cadiz.html")
    Barcelona<-get.weather("https://www.meteosat.com/tiempo/barcelona/tiempo-barcelona.html")
    
  })
  
  # datasets
  output$dataCadiz <- renderDataTable ({
     Cadiz %>% select(input$variable)
  })  
  
  output$dataBcn <- renderDataTable ({
    Barcelona %>% select(input$variable)
  })  
  
  output$dataCoruña <- renderDataTable ({
    Coruna %>% select(input$variable)
  })
  
  # plots
  output$plotCadiz <- renderPlot ({
    Cadiz<-Cadiz %>% select(Date, Time,Variable=input$variable)
    
    ggplot(Cadiz,aes(x=Time, y=Variable)) + geom_line()+
      ylab(as.character(input$variable))  +
      facet_wrap(~Date,scales='free') + theme(axis.line=element_line()) +
      scale_y_continuous(limits=c(15,40))
  })  
  
  output$plotBcn <- renderPlot ({
    
    Barcelona<-Barcelona %>% select(Date, Time,Variable=input$variable)
    
    ggplot(Barcelona,aes(x=Time, y=Variable)) + geom_line()+
      ylab(as.character(input$variable))  +
      facet_wrap(~Date,scales='free') + theme(axis.line=element_line()) +
      scale_y_continuous(limits=c(15,40))
  })  
  
  output$plotCoruna <- renderPlot ({
    
    Coruna<-Coruna %>% select(Date, Time,Variable=input$variable)
    
    ggplot(Coruna,aes(x=Time, y=Variable)) + geom_line()+
      ylab(as.character(input$variable))  +
      facet_wrap(~Date,scales='free') + theme(axis.line=element_line()) +
      scale_y_continuous(limits=c(15,40))
  }) 
  
}

shinyApp(ui, server)

