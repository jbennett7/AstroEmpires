#' @title read.credit.history
#' @description generates the credit history table.
#' @param input string vector from the credit history.
#' @examples
#' # default
#' bases <- read.credit.history(input=stdin())
#'
read.credit.history <- function(input) {
    input <- gsub("\t$","", input[!input %in% c("","\t")])
    heading <- strsplit(input[2],"\t")[[1]]
    data <- strsplit(tail(input,-2),"\t")
    data <- do.call(rbind,data)
    # The internal as.POSIXct returns epoch time. Need to
    # convert to a readable version.
    Date <- as.POSIXct({
        ifelse(grepl(",", data[,1]),
            as.POSIXct(data[,1],
                       format = "%d %b %Y, %H:%M:%S"),
            # Some dates are "12 Mar 2023" instead of
            #   "12 Mar 2023 00:00:00"
            as.POSIXct(paste0(data[,1], " 00:00:00"),
                       format = "%d %b %Y %H:%M:%S"))
        }, origin="1970-01-01")
    #TODO: Break up the descrption to particular actions.
    Description <- data[,2]
    #gsub("^(Construction|Research) of (.*) lvl ([1-9][0-9]*) at (.*)$","\\1 \\2 \\3 \\4",ich$Description)
    Credits <- as.numeric(data[,3])
    Balance <- as.numeric(data[,4])
    return(data.frame(Date,Description,Credits,Balance))
}
