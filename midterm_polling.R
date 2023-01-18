library(rvest)
library(dplyr)

link_head <- 'https://www.270towin.com/2022-senate-polls/'
states <- c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maryland', 'Missouri', 'Nevada', 'New Hampshire', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma (1)', 'Oklahoma (2)', 'Oregon', 'Pennsylvania', 'South Carolina', 'South Dakota', 'Utah', 'Vermont', 'Washington', 'Wisconsin')
#states <- c('Hawaii', 'Oregon')

averageDifference = c()
party = c()

statePolls <- function(state){
  if (state == 'oklahoma(1)'){
    link = paste(link_head, 'oklahoma', sep='')
    page = read_html(link)
    
    demPolls = page %>% html_nodes('#polls_1113 .poll_sample+ .poll_data') %>% html_text()
    demPolls = as.numeric(gsub("[^0-9.-]", "", demPolls))
    
    repPolls = page%>% html_nodes('#polls_1113 .winner') %>% html_text()
    repPolls = as.numeric(gsub("[^0-9.-]", "", repPolls))
  }
  
  else if (state == 'oklahoma(2)'){
    link = paste(link_head, 'oklahoma', sep='')
    page = read_html(link)
    
    demPolls = page %>% html_nodes('#polls_1604 .poll_sample+ .poll_data') %>% html_text()
    demPolls = as.numeric(gsub("[^0-9.-]", "", demPolls))
    
    repPolls = page%>% html_nodes('#polls_1604 .winner') %>% html_text()
    repPolls = as.numeric(gsub("[^0-9.-]", "", repPolls))
  } 
  
  else{
    link = paste(link_head, state, sep='')
    page = read_html(link)
    
    demPolls = page %>% html_nodes('.poll_sample+ .poll_data') %>% html_text()
    demPolls = as.numeric(gsub("[^0-9.-]", "", demPolls))
    
    if (state == 'alaska'){
      repPolls = page %>% html_nodes('.poll_data:nth-child(7)') %>% html_text()
      repPolls = as.numeric(gsub("[^0-9.-]", "", repPolls))
    } 
    
    else{
      repPolls = page %>% html_nodes('.poll_data:nth-child(6)') %>% html_text()
      repPolls = as.numeric(gsub("[^0-9.-]", "", repPolls))
    }
  }
  difference = demPolls-repPolls
  avgDiff = sum(difference)/length(demPolls)
  avgDiff = round(avgDiff, digits = 2)
  return(avgDiff)
}

for (state in states){
  myState = tolower(gsub(' ', '', state))
  print(state)
  value = statePolls(myState)
  
  if (is.na(value)){
    states <- states[! states %in% c(state)]
  }
  else{
    if (value > 0){
      party = c(party, 'D')
    }
    else{
      party = c(party, 'R')
    }  
    averageDifference = c(averageDifference, abs(value))
  }
}

polls <- data.frame(
  states,
  party,
  averageDifference
)
colnames(polls) = c('States', 'Party', 'Average Poll Difference')
View(polls)
