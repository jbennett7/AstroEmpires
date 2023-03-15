#' @title construction.history
#' @description Construction history
#' @param input credit history data frame
#' @return list of base dataframes
#' @import dplyr
#' @examples
#' # default
#'
library(dplyr)
activity.list <- function(df.credit.history) {
    tmp.df <- df.credit.history[
        grepl("Construction|Production|Research",
              df.credit.history$Description),]
    ret.df <- data.frame(cbind(tmp.df$Date,do.call(rbind,strsplit(
        tmp.df$Description,split="-")),tmp.df$Credits,tmp.df$Balance))
    colnames(ret.df) <- c("Date","Activity","Item","Level.Amount","Base","Credits","Balance")
    ret.df$Date <- tmp.df$Date
    ret.df$Level.Amount <- as.numeric(ret.df$Level.Amount)
    return(ret.df)
}
construction.history <- function(df.credit.history) {
        construction.activity <- df.credit.history[
            grepl("Construction",df.credit.history$Description),]

        construction.items <- data.frame(
            cbind(do.call(rbind,strsplit(
                construction.activity$Description,split="-"))))[,c("X2","X3","X4")]
        construction.items$X3 <- as.numeric(construction.items$X3)
        construction.df <- construction.items
        c.ret <- list()
        for(base in unique(construction.df$X4)){
            tmp.df <- construction.df[construction.df$X4==base,]
            base.construction <- data.frame(table(tmp.df$X2))
            colnames(base.construction) <- c("Structure","Num.Upgrades")
            Max.Level <- (tmp.df %>% group_by(X2) %>% summarise(max = max(X3)))$max
            c.ret[[base]] <- cbind(base.construction,Max.Level)
        }
        return(c.ret)
}

#   data(Structures)
#   data(Defenses)
#    structNames <- c(rownames(Structures),rownames(Defenses))
#    ledger <- df[
#        grepl("Construction|Production|Research",df$Description),]
#    TotalCost <- -sum(ledger$Credits)
#    items <- data.frame(cbind(do.call(rbind,strsplit(ledger$Description,split="-"))))
#    return(items)
    #items[items$X2=="Research.Labs" & items$X3==max(items$X3),]
#    production <- history[
#        grepl("Production",history$Description),]
#    research <- history[
#        grepl("Research",history$Description),]
#    empire <- new(Class="Empire")
    #return(items)
