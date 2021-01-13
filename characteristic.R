characteristic = setRefClass(
  'characteristic',
  fields = list(
    value = 'numeric',
    scaledValue = 'numeric',
    weightedValue = 'numeric'),
  methods = list(
    initialize = function(v){
      value <<- v
      scaledValue <<- v
      weightedValue <<- v
    },
    setScaledValue = function(sv){
      scaledValue <<- sv
    },
    setWieghtedValue = function(wv){
      weightedValue <<- wv
    }))