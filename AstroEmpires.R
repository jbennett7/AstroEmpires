files <- "Tables"
# TODOs
#
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
getInput <- function(){
    input = readLines(con=stdin())
    return(input[input!=""])
}

setClass(
    Class = "AE",
    representation = representation(
        path = "character",
        techLevels = "matrix",
        structureLevels = "matrix"
    )
)
setMethod(
    f = "initialize",
    signature = "AE",
    definition = function(.Object){
        .Object = setPath(.Object,files)
        cat("Set Tech levels\n")
        .Object = setTechLevels(.Object)
        return(.Object)
    }
)
setGeneric("setPath",
           function(object,path){standardGeneric("setPath")})
setMethod(
    f = "setPath",
    signature = "AE",
    definition = function(object,path){
        object@path = path
        return(object)
    }
)
setGeneric("setTechLevels",
           function(object,input=getInput()){standardGeneric("setTechLevels")})
setMethod(
    f = "setTechLevels",
    signature = "AE",
    definition = function(object,input=getInput()){
        rownames = gsub(" |-",".",input[seq(3,length(input),3)])
        data = do.call(rbind,
             strsplit(gsub("^\t","",input[seq(5,length(input),3)]),split="\t"))
        data = as.numeric(data[,4])
        data = matrix(data,ncol=1)
        rownames(data) = rownames
        colnames(data) = "Level"
        object@techLevels = data
        return(object)
    }
)
setGeneric("setStructureLevels",
           function(object,input=getInput()){standardGeneric("setStructureLevels")})
setMethod(
    f = "setStructureLevels",
    signature = "AE",
    definition = function(object,input=getInput()){
        tmp = rownames(read.table(paste(object@path,"/Structures.csv",sep="")))
        struct.names = c(tmp,rownames(read.table(paste(object@path,"/Defenses.csv",sep=""))))
        tmp = cbind(do.call(rbind,strsplit(paste(input," ",sep=""),split="\t")))
        base.names = tmp[,1]
        tmp = cbind(tmp[,2:24],tmp[,26:35])
        tmp = matrix(as.numeric(tmp),ncol=33)
        rownames(tmp) <- base.names
        colnames(tmp) <- struct.names
        tmp[is.na(tmp)] <- 0
    }
)

setGeneric("listTables",
           function(object){standardGeneric("listTables")})
setMethod(
    f = "listTables",
    signature = "AE",
    definition = function(object){
        technologies = gsub("\\.csv","",list.files(paste(object@path,"Technologies",sep="/")))
        structures = gsub("\\.csv","",list.files(paste(object@path,"Structures",sep="/")))
        ret = list()
        ret[["Technologies"]] <- technologies
        ret[["Structures"]] <- structures
        return(ret)
    }
)
setGeneric("getTable",
           function(object,name){standardGeneric("getTable")})
setMethod(
    f = "getTable",
    signature = "AE",
    definition = function(object, name){
        type = ""
        for(type in c("Technologies","Structures")){
            path = paste(object@path,type,paste(name,"csv",sep="."),sep="/")
            if(file.exists(path))
                return(read.table(path))
        }
        return(NULL)
    }
)
setGeneric("setTable",
           function(object,name){standardGeneric("setTable")})
setMethod(
    f = "setTable",
    signature = "AE",
    definition = function(object,name){
        type = ""
        for(type in c("Technologies","Structures")){
            path = paste(object@path,type,paste(name,"csv",sep="."),sep="/")
            if(file.exists(path)){
                input <- readLines(con=stdin())
                table.name <- gsub(" |-",".",input[1])
                level.data <- input[grepl("\t",input)]
                colnames <- strsplit(gsub(" |-",".",level.data[1]),split="\t")[[1]]
                df <- as.data.frame(cbind(do.call(
                     rbind,
                     strsplit(level.data[2:length(level.data)],split="\t"))))
                colnames(df) <- colnames
                df <- etl.num(df)
                filename <- paste("Tables/",type,"/",table.name,".csv",sep="")
                write.table(df,file=filename)
                return(1)
            }
        }
        return(-1)
    }
)
setGeneric("techList",
           function(object,name){standardGeneric("techList")})
setMethod(
    f = "techList",
    signature = "AE",
    definition = function(object){
        database <- data.frame()
        tech = object@techLevels
        for(technology in row.names(tech)){
            tech.level <-tech[technology,]+2
            path <- paste(object@path,"/Technologies/",technology,".csv",sep="")
            tmp <- read.table(path)[,1:2]
            tmp <- tmp[tech.level:nrow(tmp),]
            rr <- rep(technology,times=nrow(tmp))
            col.names <- c("Technology",colnames(tmp))
            tmp <- cbind(rr,tmp)
            colnames(tmp) <- col.names
            database <- rbind(database,tmp)
        }
        database <- database[order(database$Credits,decreasing=FALSE),]
        rownames(database) <- 1:nrow(database)
        return(database[order(database$Credits,decreasing=FALSE),])
    }
)
setGeneric("showNeededUpdates",
           function(object){standardGeneric("showNeededUpdates")})
setMethod(
    f = "showNeededUpdates",
    signature = "AE",
    definition = function(object){
        tech.path = paste(object@path,"/Technologies",sep="")
        update.list = c()
        for(tech in list.files(tech.path)){
            tech.level = object@techLevels[gsub(".csv","",tech),]
            tech.nrow = nrow(read.table(paste(tech.path,"/",tech,sep="")))
            if(diff(c(tech.level,tech.nrow))<9){
                update.list = c(update.list, gsub(".csv","",tech))
            }
        }
    }
)
setGeneric("updateTables",
           function(object){standardGeneric("updateTables")})
setMethod(
    f = "updateTables",
    signature = "AE",
    definition = function(object){
        for(tech in showNeededUpdates(object)){
            cat("Update",tech,"\n")
            if(setTable(object,tech)<0){stop("ERROR")}
        }
        return(0)
    }
)

