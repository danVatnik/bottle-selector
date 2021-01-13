library(shiny)
source('utilities.R')
source('productTest.R')

bottleData <- read.csv("bottleData.csv", stringsAsFactors=FALSE)
bottles = createBottles(bottleData)

priceTest = productTest(lapply(bottles, function(b){return(b$price)}))
weightTest = productTest(lapply(bottles, function(b){return(b$weight)}))
coldRetentionTest = productTest(lapply(bottles, function(b){return(b$coldRetention)}))
heatRetentionHotTest = productTest(lapply(bottles, function(b){return(b$heatRetentionHot)}))
heatRetentionAmbientTest = productTest(lapply(bottles, function(b){return(b$heatRetentionAmbient)}))
durabilityTest = productTest(lapply(bottles, function(b){return(b$durability)}), lowerIsBetter = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Best Insulated Water Bottle"),
    "This app will help you decide what is the best insulated water bottle for you.
    Select the importance of each category and the app will rank the bottles from best to worst.
    The data is based on ",
    tags$a(target="_blank", href="https://www.youtube.com/watch?v=a6j1NJkNzwI&ab_channel=ProjectFarm", "Project Farm's Youtube video"),".",
    hr(),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(5,
            wellPanel(
                importanceRadioSelector("price", "Price"),
                importanceRadioSelector("weight", "Weight"),
                importanceRadioSelector("coldRetention", "Keeping Water Cold"),
                importanceRadioSelector("heatRetention", "Keeping Water Hot"),
                importanceRadioSelector("heatRetentionAmbient", "Keeping Water at Ambient Temperature When Cold"),
                importanceRadioSelector("durability", "Durability", hr = FALSE),
            ),
        ),
        # Show a plot of the generated distribution
        column(5,
            uiOutput("rankedBottles")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observe({
        
        weights = c(
            getWeight(input$price),
            getWeight(input$weight),
            getWeight(input$coldRetention),
            getWeight(input$heatRetention),
            getWeight(input$heatRetentionAmbient),
            getWeight(input$durability))
        
        totalWeight = getTotalWeight(weights)
        
        priceTest$setImportance(input$price, weights[1]/totalWeight)
        weightTest$setImportance(input$weight, weights[2]/totalWeight)
        coldRetentionTest$setImportance(input$coldRetention, weights[3]/totalWeight)
        heatRetentionHotTest$setImportance(input$heatRetention, weights[4]/totalWeight)
        heatRetentionAmbientTest$setImportance(input$heatRetentionAmbient, weights[5]/totalWeight)
        durabilityTest$setImportance(input$durability, weights[6]/totalWeight)
    
        setBottlesTotalValue()
        
        bottleOrder = getBottleOrder()
        
        output$rankedBottles <- renderUI({
        x = list()
        for(p in 1:length(bottles)){
            i = bottleOrder[p]
            x[[p]] <- 
            tags$div(class = "thumbnail", style = if(p == 1) {"border-color: green; border-width: medium;"},
                tags$div(class = "card-body", style = "padding-left: 15px; padding-right: 15px;",
                    fluidRow(style = "padding-bottom: 10px;",
                        column(10,
                            tags$h4(bottles[[i]]$name)),
                        column(2,
                            tags$h4(paste0('$',bottles[[i]]$price$value))),
                    ),
                    fluidRow(
                        column(4,
                            tags$img(src=paste0(bottles[[i]]$imageLink, '.jpg'), height = "250px", style = "display: block;
                                margin-left: auto;
                                margin-right: auto;"
                             )
                        ),
                        column(8,
                           tags$table(class="table",
                              tags$thead(
                                  tags$tr(
                                      tags$th(tags$h4('Score')),
                                      tags$th(tags$h4(propNum(bottles[[i]]$totalValue)))
                                  )
                              ),
                              tags$tbody(
                                  tags$tr(
                                      tags$td('Price'),
                                      tags$td(propNum(bottles[[i]]$price$scaledValue))
                                  ),
                                  tags$tr(
                                      tags$td('Weight'),
                                      tags$td(propNum(bottles[[i]]$weight$scaledValue))
                                  ),
                                  tags$tr(
                                      tags$td('Keeping Water Cold'),
                                      tags$td(propNum(bottles[[i]]$coldRetention$scaledValue))
                                  ),
                                  tags$tr(
                                      tags$td('Keeping Water Hot'),
                                      tags$td(propNum(bottles[[i]]$heatRetentionHot$scaledValue))
                                  ),
                                  tags$tr(
                                      tags$td('Keeping Water Ambient'),
                                      tags$td(propNum(bottles[[i]]$heatRetentionAmbient$scaledValue))
                                  ),
                                  tags$tr(
                                      tags$td("Durability"),
                                      tags$td(propNum(bottles[[i]]$durability$scaledValue))
                                  )
                              )
                           )
                        )
                    ),
                    fluidRow(
                        column(12,
                            tags$a(target="_blank", href=bottles[[i]]$link, "Amazon link")
                        )
                    )
                )
            )
        }
        return(x)
    })
    })
    
    setBottlesTotalValue = function(){
        lapply(bottles, function(b){b$calculateTotalValue()})
    }
    
    getTotalWeight = function(pWeights){
        total = sum(pWeights)
        if(total == 0){
            return(1)
        }else{
            return(total)
        }
    }
    
    getWeight = function(pImportance){
        if(pImportance == 'dontCare'){
            return(0)
        }else if(pImportance == 'notImportant'){
            return(0.5)
        }else if(pImportance == 'important'){
            return(1)
        }else if(pImportance == 'critical'){
            return(2)
        }else{
            stop("Unknown importance level")
        }
    }
    
    propNum = function(pValue){
        signif(pValue * 10, 2)
    }
    
    getBottleOrder = function(){
        return(order(sapply(bottles, function(b){return(b$totalValue)}), decreasing = TRUE))
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
