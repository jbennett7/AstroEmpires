initialize <- function() {
    Data <- new.env()
    Data$Current <- list()
    Data$Technologies <- list()
    Data$Structures <- list()
    Data$Tables <- list()
    save(Data,file=data.file)
}

etl.num <- function(df) {
    for(col in colnames(df)){
        if(grepl("%", df[1,col])){
            tryCatch({
                df[,col] <- as.numeric(gsub("%","",df[,col]))*.01
            },warning=function(w){df[,col]},
            error=function(e){df[,col]})
        } else {
            tryCatch({
                df[,col] <- as.numeric(gsub(",","",df[,col]))
            },warning=function(w){df[,col]},
            error=function(e){df[,col]})
        }
    }
    return(df)
}

raw.input <- function() {
    return(readLines(con=stdin()))
}
