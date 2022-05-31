source("utils.R")
source("config")
Input <- new.env()

# NOTE: The home base needs to be marked
# Advantages for home base is +20 Construction (i.e. starts with +40 Construction, instead of initial +20)
Input$mark.base.home <- function(base.name) {
    load(data.file)
    Data$Home <- base.name
    save(Data,file=data.file)
}

#NOTE: The comment section in "Bases" needs to have the base type (i.e. "Rocky", "Arid", ...).
Input$bases <- function(input=Utils$raw.input()) {
    load(data.file)
    input <- gsub(" *$","",input)
    input <- do.call(rbind,strsplit(input,split="\t"))
    rownames <- input[,1]
    colnames <- c("Position","Type","Terrain")
    Position <- as.numeric(gsub(".*:(.).$","\\1",input[,2]))
    Type <- ifelse(gsub(".*:.(.)$","\\1",input[,2])==0,"Planet",ifelse(input[,5]=="Asteroid","Asteroid","Moon"))
    Terrain <- input[,5]
    Data$Current$Bases <- data.frame(Position,Type,Terrain)
    rownames(Data$Current$Bases) <- rownames
    colnames(Data$Current$Bases) <- colnames
    save(Data,file=data.file)
}

#TODO: Generic function to collect the tables in the Tables section. Does not work with Terrain.
#      How do we clean this up?
Input$table <- function(input=Utils$raw.input()) {
    if(!file.exists(data.file)){
        Utils$initialze()
    }
    load(data.file)
    input <- input[input!=""]
    table.name <- gsub(" ", ".", input[1])
    table.columns <- strsplit(input[2],split="\t")[[1]]
    table.columns <- table.columns[table.columns!=""]
    if(table.name == "Structures")
        table.columns <- c(table.columns[1],"Construction","Production",table.columns[2:(length(table.columns))])
    table.columns <- gsub(" ", ".", table.columns[table.columns!=""])
    table.rows <- gsub(" |-", ".", input[seq(3,length(input),3)])
    table.data <- do.call(rbind,strsplit(gsub("\t$", "\t ", gsub("^\t",
                                "",
                                input[seq(5,length(input),3)])), split="\t"))
    if(table.name == "Structures") {
        dummy = rep("",length(table.data[,1]))
        table.data <- cbind(table.data[,1],dummy,dummy,table.data[,(2:length(table.data[1,]))])
    }
    table <- as.data.frame(table.data)
    colnames(table) <- table.columns
    rownames(table) <- table.rows
    # This is necessary because the table does not provide this information
    if(table.name == "Structures") {
        construction <- c("Robotic.Factories","Nanite.Factories","Android.Factories")
        table[construction, "Construction"] <- c(2,4,6)
        production <- c("Robotic.Factories","Shipyards","Orbital.Shipyards","Nanite.Factories","Android.Factories")
        table[production, "Production"] <- c(2,2,8,4,6)
    }
    table <- Utils$etl.num(table)
    table[is.na(table)] = 0
    Data$Tables[[table.name]] <- table
    save(Data,file=data.file)
}

#TODO: How is the Astro.Position table created?
Input$terrain <- function(input=Utils$raw.input()) {
    load(data.file)
    table.name <- gsub(" ",".",input[1])
    print(table.name)
    table.columns <- strsplit(
        gsub("^\t","",gsub(" ",".", input[2])),split="\t")[[1]]
    table.data <- do.call(rbind,
        strsplit(input[3:length(input)], split="\t"))
    d.table <- table.data[,2:length(table.data[1,])]
    d.table <- gsub("\\(|\\)|-","",d.table)
    d.table <- sapply(as.data.frame(d.table),as.numeric)
    rownames(d.table) <- gsub(" ",".",table.data[,1])
    colnames(d.table) <- table.columns
    Data$Tables[[table.name]] <- data.frame(d.table)
    save(Data,file=data.file)
}

#TODO: Break this up into two input functions one for Technology one for Structures.
Input$levels <- function(input=Utils$raw.input(), type="Technologies") {
    load(data.file)
    table.name <- gsub(" |-", ".", input[1])
    level.data <- input[grepl("\t", input)]
    df <- as.data.frame(
        cbind(do.call(
            rbind,
            strsplit(level.data[2:length(level.data)], split="\t"))))
    colnames(df) <- strsplit(gsub(" ", ".", level.data[1]), split="\t")[[1]]
    df <- Utils$etl.num(df)
    if(type=="Technologies"){
        Data$Technologies[[table.name]] <- df
    } else {
        Data$Structures[[table.name]] <- df
    }
    save(Data,file=data.file)
}
Input$levels.structures <- function(input=Utils$raw.input()) {
    Input$levels(input,type="Structures")
}
Input$levels.technologies <- function(input=Utils$raw.input()) {
    Input$levels(input,type="Technologies")
}

#TODO: Is this needed?
Input$capacities <- function(input=Utils$raw.input()) {
    load(data.file)
    columns <- c("Name","Location","Type","Now.Economy","Max.Economy",
                 "Construction","Production","Shipyards",
                 "Orbital.Shipyards","Research","Labs","Commander")
    tmp <- do.call(rbind, strsplit(input, split="\t"))
    eco.expr <- "([0-9]+) / ([0-9]+)"
    eco.regs <- do.call(rbind,
                        regmatches(tmp[,4], regexec(eco.expr, tmp[,4])))
    prod.expr <- "([0-9]+) \\(([0-9]+)/([0-9]+)\\)"
    prod.regs <- do.call(rbind,
                         regmatches(tmp[,6], regexec(prod.expr,tmp[,6])))
    res.expr <- "([0-9]+) \\(([0-9]+)\\)"
    res.regs <- do.call(rbind,
                        regmatches(tmp[,7], regexec(res.expr,tmp[,7])))
    tmp <- data.frame(tmp[,1:3], eco.regs[,2:3], tmp[,5], prod.regs[,2:4],
                      res.regs[,2:3], tmp[,8])
    colnames(tmp) <- columns
    tmp <- process$util$Utils$etl.num(tmp)
    Data$Current[[Capacities]] <- tmp
    save(Data,file=data.file)
}

Input$structures <- function(input=Utils$raw.input()) {
    load(data.file)
    input <- input[input!=""]
    tmp <- cbind(do.call(rbind,strsplit(paste(input," ",sep=""),split="\t")))
    tmp <- cbind(tmp[,1:24],tmp[,26:35])
    Structures <- sapply(as.data.frame(tmp[,2:34]),as.numeric)
    rownames(Structures) <- tmp[,1]
    colnames(Structures) <- c(
        rownames(Data$Tables$Structures),
        rownames(Data$Tables$Defenses))
    Structures <- data.frame(Structures)
    Structures[is.na(Structures)] <- 0
    Data$Current[["Structures"]] <- Structures
    save(Data,file=data.file)
}

Input$current.technologies <- function(input=Utils$raw.input()) {
    load(data.file)
    rownames <- gsub(" ",".",input[seq(3,length(input),3)])
    colnames <- strsplit(
         gsub(" |-",".",gsub("^\t","",input[2])),split="\t")[[1]]
    data <- do.call(rbind,
         strsplit(gsub("^\t","",input[seq(5,length(input),3)]),split="\t"))
    rownames(data) <- rownames
    colnames(data) <- colnames
    Data$Current$Technologies <- Utils$etl.num(as.data.frame(data))
    save(Data,file=data.file)
}
