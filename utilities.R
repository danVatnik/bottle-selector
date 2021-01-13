source('bottle.R')
library(shiny)

createBottles = function(bottlesData){
  bottles = list()
  for(i in 1:dim(bottlesData)[1]){
    bottles[[i]] = bottle(
      bottlesData$Bottle[i],
      bottlesData$Price[i],
      bottlesData$Weight[i],
      bottlesData$Cold.Temp[i],
      bottlesData$Heat.Loss[i],
      bottlesData$Heat.loss.ambient[i],
      bottlesData$Durability[i],
      bottlesData$link[i],
      bottlesData$imageLink[i])
  }
  
  return(bottles)
}


importanceRadioSelector = function(inputId, label, hr = TRUE){
  rb = radioButtons(
    inputId,
    label,
    c("Don't care" = 'dontCare',
      "Not Important" = 'notImportant',
      "Important" = 'important',
      "Critical" = 'critical'),
    selected = 'important',
    inline = TRUE
  )
  tgs = tags$div(
    rb,
    if(hr){tags$hr()}
  )
  
  return(tgs)
}


