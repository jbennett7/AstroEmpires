source('utils.R')
source('config')
input <- new.env()

input$base <- function(base.name,s,f,m,g,c) {
    load(data.file)
    rownames <- c(rownames(Data$Current$Bases),base.name)
    Data$Current$Bases <- rbind(Data$Current$Bases,c(s,f,m,g,c))
    rownames(Data$Current$Bases) <- rownames
    save(Data,file=data.file)
}

input$table <- function(input=raw.input()) {
    if(!file.exists(data.file)){
        initialize()
    }
    load(data.file)
    input <- input[input!=""]
    table.name <- gsub(" ", ".", input[1])
    table.columns <- strsplit(input[2],split="\t")[[1]]
    table.columns <- table.columns[table.columns!=""]
    if(table.name == "Structures")
        table.columns <- c(table.columns[1],"Construction","Production",table.columns[2:(length(table.columns))])
    table.columns <- gsub(" ", ".", table.columns[table.columns!=""])
    table.rows <- gsub(" ", ".", input[seq(3,length(input),3)])
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
    if(table.name == "Structures") {
        construction <- c("Robotic.Factories","Nanite.Factories","Android.Factories")
        table[construction, "Construction"] <- c(2,4,6)
        production <- c("Robotic.Factories","Shipyards","Orbital.Shipyards","Nanite.Factories","Android.Factories")
        table[production, "Production"] <- c(2,2,8,4,6)
    }
    table <- etl.num(table)
    table[is.na(table)] = 0
    Data$Tables[[table.name]] <- table
    save(Data,file=data.file)
}

input$terrain <- function(input=raw.input()) {
    load(data.file)
    table.name <- gsub(" ",".",input[1])
    table.columns <- strsplit(
        gsub("^\t","",gsub(" ",".", input[2])),split="\t")
    table.data <- do.call(rbind,
        strsplit(input[3:length(input)], split="\t"))
    table <- sapply(ad.data.frame(table.data[,2:24],as.numeric))
    rownames(table) <- table.data[,1]
    colnames(table) <- table.columns
    Terrains <- data.frame(table)
    Data$Tables$Terrains <- Terrains
    save(Data,file=data.file)
}

input$levels <- function(input=raw.input(), type="Technologies") {
    if(type != "Technologies" && type != "Structures"){
        stop("type only can be Technologies or Structures")
    }
    if(!file.exists(data.file)){
        initialize()
    }
    load(data.file)
    table.name <- gsub(" ", ".", input[1])
    level.data <- input[grepl("\t", input)]
    df <- as.data.frame(
        cbind(do.call(
            rbind,
            strsplit(level.data[2:length(level.data)], split="\t"))))
    colnames(df) <- strsplit(gsub(" ", ".", level.data[1]), split="\t")[[1]]
    df <- etl.num(df)
    if(type=="Technologies"){
        Data$Technologies[[table.name]] <- df
    } else {
        Data$Structures[[table.name]] <- df
    }
    save(Data,file=data.file)
}

input$capacities <- function(input=raw.input()) {
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
    tmp <- process$util$etl.num(tmp)
    Data$Current[[Capacities]] <- tmp
    save(Data,file=data.file)
}

input$structures <- function(input=raw.input()) {
    load(data.file)
    input <- input[input!=""]
    tmp <- cbind(do.call(rbind,strsplit(paste(input," ",sep=""),split="\t")))
    tmp <- cbind(tmp[,1:24],tmp[,26:35])
    Structures <- sapply(as.data.frame(tmp[,2:34]),as.numeric)
    rownames(Structures) <- tmp[,1]
    colnames(Structures) <- c("Urban.Structures","Solar.Plants",
                              "Gas.Plants","Fusion.Plants",
                              "Antimatter.Plants","Orbital.Plants",
                              "Research.Labs","Metal.Refineries",
                              "Crystal.Mines","Robotic.Factories",
                              "Shipyards","Orbital.Shipyards",
                              "Spaceports","Command.Centers",
                              "Nanite.Factories","Android.Factories",
                              "Economic.Centers","Terraform",
                              "Multi.Level.Platforms","Orbital.Base",
                              "Jump.Gate","Biosphere.Modification","Capital",
                              "Barracks","Laser.Turrets","Missile.Turrets",
                              "Plasma.Turrets","Ion.Turrets",
                              "Photon.Turrets","Disruptor.Turrets",
                              "Deflection.Shields","Planetary.Shield",
                              "Planetary.Ring")
    Structures <- data.frame(Structures)
    Structures[is.na(Structures)] <- 0
    Data$Current[["Structures"]] <- Structures
    save(Data,file=data.file)
}

input$current.technologies <- function(input=raw.input()) {
    load(data.file)
    rownames <- gsub(" ",".",input[seq(3,length(input),3)])
    colnames <- strsplit(
         gsub(" ",".",gsub("^\t","",input[2])),split="\t")[[1]]
    data <- do.call(rbind,
         strsplit(gsub("^\t","",input[seq(5,length(input),3)]),split="\t"))
    rownames(data) <- rownames
    colnames(data) <- colnames
    Data$Current$Technologies <- etl.num(as.data.frame(data))
    save(Data,file=data.file)
}
