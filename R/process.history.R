process.history <- function(.Object){
    history <- .Object@History
    construction <- {
        ledger <- history[
            grepl("Construction",history$Description),]
        Costs <- -sum(ledger$Credits)
        items <- data.frame(cbind(do.call(rbind,strsplit(ledger$Description,split="-"))))
        items$X3 = as.numeric(items$X3)
        #items[items$X2=="Research.Labs" && max(items$X3),]
        items[items$X2=="Research.Labs" & items$X3==max(items$X3),]
    }
    production <- history[
        grepl("Production",history$Description),]
    research <- history[
        grepl("Research",history$Description),]
    empire <- new(Class="Empire")
    data(Structures)
    data(Defenses)
    data(Technologies)
    structNames <- c(rownames(Structures),rownames(Defenses))
    techNames <- rownames(Technologies)
    return(construction)
}
