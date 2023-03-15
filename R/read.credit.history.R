#' @title read.credit.history
#' @description generates the credit history table.
#' @param input string vector from the credit history.
#' @examples
#' # default
#' bases <- read.credit.history(input=stdin())
#'
read.credit.history <- function(input) {
    ##PROCESS INPUTS
    input <- gsub("\t$","", input[!input %in% c("","\t")])
    heading <- strsplit(input[2],"\t")[[1]]
    data <- strsplit(tail(input,-2),"\t")
    data <- do.call(rbind,data)

    ##DATE FIELD
    Date <- strptime(ifelse(grepl(",",data[,1]),
                            data[,1],
                            paste0(data[,1], ", 00:00:00")),
                     format = "%d %B %Y, %H:%M:%S",
                     tz="GMT")

    ##DESCRIPTION FIELD
    regex.construction.and.research <- "^(Construction|Research) of (.*) lvl ([1-9][0-9]*) at (.*)$"
    regex.production <- "^(Production) of ([1-9][0-9]*) (.*) at (.*)$"
    regex.output <- "\\1-\\2-\\3-\\4"
    regex.production.output <- "\\1-\\3-\\2-\\4"
    Description <- {
        tmp <- gsub(regex.construction.and.research,regex.output,data[,2])
        tmp <- gsub(regex.production,regex.production.output, tmp)
        gsub(" ",".",tmp)
    }

    ##CREDITS AND BALANCE FIELDS
    Credits <- as.numeric(data[,3])
    Balance <- as.numeric(data[,4])

    return(data.frame(Date,Description,Credits,Balance))
}
