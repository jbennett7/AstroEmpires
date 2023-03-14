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
    # The internal as.POSIXct returns epoch time. Need to
    # convert to a readable version.
    #TODO: add feature to adjust date format
    Date <- as.POSIXct({
        ifelse(grepl(",", data[,1]),
            as.POSIXct(data[,1],
                       format = "%d %b %Y, %H:%M:%S"),
            # Some dates are "12 Mar 2023" instead of
            #   "12 Mar 2023 00:00:00"
            as.POSIXct(paste0(data[,1], " 00:00:00"),
                       format = "%d %b %Y %H:%M:%S"))
        }, origin="1970-01-01")

    ##DESCRIPTION FIELD
    regex.construction.and.research <- "^(Construction|Research) of (.*) lvl ([1-9][0-9]*) at (.*)$"
    regex.production <- "^(Production) of ([1-9][0-9]*) (.*) at (.*)$"
    regex.output <- "\\1-\\2-\\3-\\4"
    Description <- data[,2]
    Description <- gsub(regex.construction.and.research,regex.output,data[,2])
    Description <- gsub(regex.production,regex.output, Description)
    Description <- gsub(" ",".",Description)

    ##CREDITS AND BALANCE FIELDS
    Credits <- as.numeric(data[,3])
    Balance <- as.numeric(data[,4])

    return(data.frame(Date,Description,Credits,Balance))
}
