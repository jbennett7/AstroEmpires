setGeneric("appendCreditHistory",
           function(.Object,input){
               standardGeneric("appendCreditHistory")})
setMethod("appendCreditHistory",
          signature("CreditHistory"),
          function(.Object,input){
              data <- read.credit.history(input)
              data <- rbind(.Object@History,data)
              data <- data[!duplicated(data$Date),]
              data <- data[order(data$Date,decreasing=TRUE),]
              rownames(data) <- seq(1:nrow(data))
              .Object@History <- data
              return(.Object)
          }
)
