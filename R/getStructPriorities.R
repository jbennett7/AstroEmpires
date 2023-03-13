setGeneric("getStructPriorities",
           function(.Object, structures){
               standardGeneric("getStructPriorities")})
setMethod("getStructPriorities",
          signature("Empire"),
          function(.Object, structures){
    structList <- .Object@structures[,structures]
    table <- data.frame()
    for(struct in structures){
        structLevel <- min(structList[,struct])
        data(list=c(struct))
        tmp <- tail(eval(as.symbol(struct))[,1:2],
                    -structLevel)
        tmp <- cbind(rep(struct,nrow(tmp)),tmp)
        colnames(tmp) <- c("Struct","Level","Credits")
        table <- rbind(table,tmp)
    }
    return(table[order(table$Credits,decreasing=FALSE),])

})
