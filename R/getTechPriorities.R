setGeneric("getTechPriorities",
           function(.Object){
               standardGeneric("getTechPriorities")})
setMethod("getTechPriorities",
          signature("Empire"),
          function(.Object){
    techList <- .Object@techLevels
    table <- data.frame()
    for (tech in rownames(techList)){
        data(list=c(tech))
        tmp <- tail(eval(as.symbol(tech))[,1:2],-(techList[tech,]+1))
        tmp <- cbind(rep(tech,nrow(tmp)),tmp)
        colnames(tmp) <- c("Tech","Level","Credits")
        table <- rbind(table,tmp)
    }
    return(table[order(table$Credits,decreasing=FALSE),])
})
