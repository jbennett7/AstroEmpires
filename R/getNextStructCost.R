setGeneric("getNextStructCost",
           function(.Object, structure){
               standardGeneric("getNextStructCost")})
setMethod("getNextStructCost",
          signature("Base"),
          function(.Object, structure){
    data(list=c(structure))
    lvl <- .Object@structures[structure,]+1
    return(eval(as.symbol(structure))[lvl,"Credits"])
})
