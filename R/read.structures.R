#' @title read.structures
#' @description generates the structure table.
#' @param input string vector from the structure page.
#' @return data frame of base structure levels.
#' @examples
#' # default
#' bases <- read.structures(input=stdin())
#'
read.structures <- function(input) {
    input <- input[!input %in% c("","\t")]
    data(Structures)
    data(Defenses)
    structNames = rownames(Structures)
    structNames = c(structNames,rownames(Defenses))
    dat = strsplit(paste(input," ",sep=""),split="\t")
    ret = cbind(do.call(rbind,dat))
    baseNames = ret[,1]
    ret = cbind(ret[,2:24],ret[,26:35])
    ret = data.frame(as.numeric(ret),ncol=33)
    rownames(ret) = baseNames
    colnames(ret) = structNames
    ret[is.na(ret)] = 0
    return(ret)
}
