setGeneric("baseEnergyAnalysis",
           function(.Object){
               standardGeneric("baseEnergyAnalysis")})
setMethod("baseEnergyAnalysis",
          signature("Base"),
          function(.Object){
    data(Structures)
    plants <- c("Solar.Plants",
                "Gas.Plants",
                rownames(Structures[Structures$Energy>0,]))
    Energy <- c(.Object@resources[c("Solar","Gas"),],
        Structures[Structures$Energy>0,"Energy"])
    Next.Level.Cost <- c()
    for (plant in plants){
        lvl <- .Object@structures[plant,]+1
        data(list=c(plant))
        Next.Level.Cost <- c(
            Next.Level.Cost,
            eval(as.symbol(plant))[lvl,"Credits"])
    }
    Energy.per.Cost <- Energy/Next.Level.Cost
    energy <- data.frame(Energy,Next.Level.Cost,Energy.per.Cost)
    rownames(energy) <- plants
    return(energy[
        order(energy$Energy.per.Cost,decreasing=TRUE),])
})

