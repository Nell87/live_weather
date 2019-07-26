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
