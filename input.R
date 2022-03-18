input <- new.env()

input$table <- function(input=raw.input()) {
    source('utils.R')
    source('config')
    if(!file.exists(data.file)){
        initialize()
    }
    load(data.file)
    input <- input[input!=""]
    table.name <- gsub(" ", ".", input[1])
    table.columns <- c(input[1],
                       "Description",
                       strsplit(input[2], split="\t")[[1]])
    table.columns <- gsub(" ", ".", table.columns[table.columns!=""])
    table.items <- gsub(" ", ".", input[seq(3,length(input),3)])
    table.description <- input[seq(4,length(input),3)]
    table.data <- strsplit(gsub("\t$", "\t ", gsub("^\t",
                                "",
                                input[seq(5,length(input),3)])), split="\t")
    table <- as.data.frame(cbind(table.items,
                                 table.description,
                                 do.call(rbind,table.data)))
    colnames(table) <- table.columns
    table <- etl.num(table)
    Data$Tables[[table.name]] <- table
    save(Data,file=data.file)
}

input$terrain <- function(input=raw.input()) {
    source('utils.R')
    source('config')
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
    source('utils.R')
    source('config')
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
    source('utils.R')
    source('config')
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
    source('utils.R')
    source('config')
    load(data.file)
    tmp <- strsplit(input,split="\t")
    tmp <- cbind(do.call(rbind,strsplit(input,split="\t")),rep("",10))
    Structures <- sapply(as.data.frame(tmp[,2:24]),as.numeric)
    Structures[is.na(Structures)] <- 0
    rownames(Structures) <- tmp[,1]
    Defenses <- sapply(as.data.frame(tmp[,c(26:35)]),as.numeric)
    Defenses[is.na(Defenses)] <- 0
    rownames(Defenses) <- tmp[,1]
    colnames(Structures) <- c("Urban.Structures","Solar.Plants",
                              "Gas.Plants","Fusion.Plants",
                              "AntiMatter.Plants","Orbital.Plants",
                              "Research.Labs","Metal.Refineries",
                              "Crystal.Mines","Robotic.Factories",
                              "Shipyards","Orbital.Shipyards",
                              "Spaceports","Command.Centers",
                              "Nanite.Factories","Android.Factories",
                              "Economic.Centers","Terraform",
                              "MultiLevel.Platforms","Orbital.Base",
                              "Jump.Gate","Biosphere.Mod","Capital")
    colnames(Defenses) <- c("Barracks","Laser.Turrets","Missile.Turrets",
                            "Plasma.Turrets","Ion.Turrets",
                            "Photon.Turrets","Disruptor.Turrets",
                            "Deflection.Shields","Planetary.Shield",
                            "Planetary.Ring")
    Structures <- data.frame(Structures)
    Defenses <- data.frame(Defenses)
    Data$Current[["Structures"]] <- Structures
    Data$Current[["Defenses"]] <- Defenses
    save(Data,file=data.file)
}

input$current.technologies <- function(input=raw.input()) {
    source('utils.R')
    source('config')
    load(data.file)
    rownames <- input[seq(3,length(input),3)]
    colnames <- strsplit(
         gsub(" ",".",gsub("^\t","",input[2])),split="\t")[[1]]
    data <- do.call(rbind,
         strsplit(gsub("^\t","",input[seq(5,length(data),3)]),split="\t"))
    rownames(data) <- rownames
    colnames(data) <- colnames
    Data$Current$Technologies <- etl.num(as.data.frame(data))
    save(Data,file=data.file)
}
