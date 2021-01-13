productTest = setRefClass(
  'productTest',
  fields = list(
    data = 'list',
    meanValue = 'numeric',
    stdValue = 'numeric',
    minValue = 'numeric',
    maxValue = 'numeric',
    weight = 'numeric',
    isLowerBetter = 'logical',
    subPrimeDataCutoff = 'numeric'
  ),
  methods = list(
    initialize = function(pData, lowerIsBetter = TRUE){
      data <<- pData
      currentData = getValues()
      meanValue <<- mean(currentData)
      stdValue <<- sd(currentData)
      minValue <<- min(currentData)
      maxValue <<- max(currentData)
      weight <<- 1
      subPrimeDataCutoff <<- 2
      isLowerBetter <<- lowerIsBetter
      scaleData()
      wightData()
    },
    getValues = function(){
      return(sapply(data, function(d){d$value}))
    },
    getScaledValues = function(){
      return(sapply(data, function(d){d$scaledValue}))
    },
    setScaledValues = function(pValues){
      for(i in 1: length(pValues)){
        data[[i]]$scaledValue <<- pValues[i]
      }
    },
    getWeightedValues = function(){
      return(sapply(data, function(d){d$weightedValue}))
    },
    setWeightedValues = function(pValues){
      for(i in 1: length(pValues)){
        data[[i]]$weightedValue <<- pValues[i]
      }
    },
    scaleData = function(){
      testValues  = getValues();
      scaledValues = sapply(testValues, function(tv){return(scale(tv))})
      setScaledValues(scaledValues)
    },
    scale = function(pValue){
      return(applyDirection(applyLimits(minMaxScale(pValue))))
    },
    minMaxScale = function(pValue){
      return((pValue - minValue)/(maxValue - minValue))
    },
    applyDirection = function(pValue){
      if(isLowerBetter){
        return(1-pValue)
      }
      return(pValue)
    },
    applyLimits = function(pValue){
      if(pValue > 1){
        return(1)
      }else if(pValue < 0){
        return(0)
      }else{
        return(pValue)
      }
    },
    setImportance = function(pImportance, pWeight){
      if(pImportance == 'critical'){
        currentData = getPremiumValues()
      } else{
        currentData = getValues()
      }
      weight <<- pWeight
      minValue <<- min(currentData)
      maxValue <<- max(currentData)
      scaleData()
      wightData()
    },
    getPremiumValues = function(){
      dataValues = getValues()
      
      if(isLowerBetter){
        subPrimeDataCutoffValue = meanValue + subPrimeDataCutoff * 2
        return(dataValues[dataValues < subPrimeDataCutoffValue ])
      }else{
        subPrimeDataCutoffValue = meanValue - subPrimeDataCutoff * 2
        return(dataValues[dataValues > subPrimeDataCutoffValue ])
      }
    },
    wightData = function(){
      scaledValues  = getScaledValues();
      weightedValues = sapply(scaledValues, function(tv){return(tv * weight)})
      setWeightedValues(weightedValues)
    }
  ))