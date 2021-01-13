source('characteristic.R')

bottle = setRefClass(
  'bottle',
  fields = list(
    name = 'character',
    price = 'ANY',
    weight = 'ANY',
    coldRetention = 'ANY',
    heatRetentionHot = 'ANY',
    heatRetentionAmbient = 'ANY',
    durability = 'ANY',
    value = 'numeric',
    link = 'character',
    imageLink = 'character',
    totalValue = 'numeric'
  ),
  methods = list(
    initialize = function(pName, pPrice, pWeight, pColdRetention, pHeatRetentionHot, pHeatRetentionAmbient, pDurability, pLink, pImageLink){
      name <<- pName
      price <<- characteristic(pPrice)
      weight <<- characteristic(pWeight)
      coldRetention <<- characteristic(pColdRetention)
      heatRetentionHot <<- characteristic(pHeatRetentionHot)
      heatRetentionAmbient <<- characteristic(pHeatRetentionAmbient)
      durability <<- characteristic(pDurability)
      value <<- 1
      totalValue <<- 1
      link <<- pLink
      imageLink <<- pImageLink
    },
    calculateTotalValue = function(){
      totalValue <<-
        (price$weightedValue
        + weight$weightedValue
        + coldRetention$weightedValue
        + heatRetentionHot$weightedValue
        + heatRetentionAmbient$weightedValue
        + durability$weightedValue)
    }
  )
)