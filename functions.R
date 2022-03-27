source("config")
load(data.file)

home.base <- function(base.name) {
    ifelse(Data$Home==base.name,return(TRUE),return(FALSE))
}

get.bases <- function() {
    return(rownames(Data$Current$Bases))
}

get.base.solar <- function(base.name) {
    base.pos <- Data$Current$Bases[base.name,"Position"]
    return(Data$Tables$Astro.Position["Solar.Energy",base.pos])
}

get.base.fertility <- function(base.name) {
    base.info <- Data$Current$Bases[base.name,c("Position","Terrain")]
    return(Data$Tables$Terrain[base.info[,2],"Fertility"] +
           Data$Tables$Astro.Position["Fertility",base.info[,1]])
}

get.base.metal <- function(base.name) {
    base.terrain <- Data$Current$Bases[base.name,"Terrain"]
    return(Data$Tables$Terrain[base.terrain,"Metal"])
}

get.base.gas <- function(base.name) {
    base.info <- Data$Current$Bases[base.name,c("Position","Terrain")]
    return(Data$Tables$Terrain[base.info[,2],"Gas"] +
           Data$Tables$Astro.Position["Gas",base.info[,1]])
}

get.base.crystals <- function(base.name) {
    base.terrain <- Data$Current$Bases[base.name,"Terrain"]
    return(Data$Tables$Terrain[base.terrain,"Crystals"])
}

get.base.solar.gas <- function(base.name) {
    base.info <- Data$Current$Bases[base.name,c("Position","Terrain")]
    return(c(
        Data$Tables$Astro.Position["Solar.Energy",base.info[,1]],
        Data$Tables$Terrain[base.info[,2],"Gas"] +
           Data$Tables$Astro.Position["Gas",base.info[,1]]))
}

get.base.construction <- function(base.name) {
    con.bool <- Data$Tables$Structures$Construction>0
    base.starts <- c(get.base.metal(base.name),Data$Tables$Structures[con.bool,"Construction"])
    construction.levels <- as.numeric(Data$Current$Structures[base.name,
        c("Metal.Refineries", rownames(Data$Tables$Structures[con.bool,]))])
    cybernetics.technology <- Data$Current$Technologies["Cybernetics",c("Bonus")]
    return(round(sum(c(20,construction.levels*base.starts))*(1+cybernetics.technology)))
}

get.base.production <- function(base.name) {
    prod.bool <- Data$Tables$Structures$Production>0
    base.starts <- c(get.base.metal(base.name),Data$Tables$Structures[prod.bool,"Production"])
    production.levels <- as.numeric(Data$Current$Structures[base.name,
        c("Metal.Refineries", rownames(Data$Tables$Structures[prod.bool,]))])
    cybernetics.technology <- Data$Current$Technologies["Cybernetics",c("Bonus")]
    return(round(sum(production.levels*base.starts)*(1+cybernetics.technology)))
}

get.base.economy <- function(base.name) {
    eco.bool <- Data$Tables$Structures$Economy>0
    base.starts <- c(get.base.crystals(base.name),Data$Tables$Structures[eco.bool,"Economy"])
    economy.levels <- as.numeric(Data$Current$Structures[base.name,
        c("Crystal.Mines",rownames(Data$Tables$Structures[eco.bool,]))])
    return(round(sum(c(economy.levels*base.starts))))
}

get.base.research <- function(base.name) {
    base.starts <- 8
    ai.technology <- Data$Current$Technologies["Artificial.Intelligence",c("Bonus")]
    research.levels <- as.numeric(Data$Current$Structures[base.name,"Research.Labs"])
    return(round(base.starts*research.levels*(1+ai.technology)))
}

get.base.structure.current.level <- function(base.name,structure) {
    return(Data$Current$Structures[base.name,structure])
}

get.base.structure.next.level <- function(base.name,structure) {
    return(get.base.structure.current.level(base.name,structure)+1)
}

get.base.structure.next.cost <- function(base.name,structure) {
    structure.next.level <- get.base.structure.next.level(base.name,structure)
    return(Data$Structures[[structure]][structure.next.level,"Credits"])
}

get.base.structure.current.sum.cost <- function(base.name,structure) {
    structure.level <- get.base.structure.current.level(base.name,structure)
    if(structure.level==0) return(0)
    return(sum(Data$Structures[[structure]]$Credits[1:structure.level]))
}

get.energy.structures <- function() {
    return(c("Solar.Plants", "Gas.Plants", rownames(Data$Tables$Structures[Data$Tables$Structures$Energy>0,])))
}

get.structure.resource.dependencies <- function(structs) {
    return(Data$Tables$Structures[structs,
        colSums(Data$Tables$Structures[structs,unlist(lapply(Data$Tables$Structures,is.numeric))]<0)>0,drop=F])
}

get.structure.resource.dependency.cost <- function() {}

########################### Area ################################
get.area.structures <- function() {
    return (rownames(Data$Tables$Structures[Data$Tables$Structures$Area>0,]))
}

get.base.max.area <- function(base.name) {
    base.info <- Data$Current$Bases[base.name,c("Type","Terrain")]
    area <- Data$Tables$Terrain[base.info[,2],
        ifelse(base.info[,1]=="Planet","Area.Planet","Area.Moon")]
    area.levels <- as.numeric(Data$Current$Structures[base.name,get.area.structures()])
    area.capacities <- c(Data$Tables$Structures[get.area.structures(),"Area"])
    return(sum(c(area,area.capacities*area.levels)))
}

get.base.current.area <- function(base.name) {
    return(sum(Data$Current$Structures[base.name,
        rownames(Data$Tables$Structures[Data$Tables$Structures$Area<0,])]))
}

get.base.area.structures.cost <- function(base.name) {
    area.structures <- get.area.structures()
    base.area.structures <- Data$Current$Structures[base.name,area.structures]
    sum.cost <- 0
    for(structure in area.structures){
        if(base.area.structures[,structure]==0)
            next
        sum.cost <- sum(c(sum.cost,Data$Structures[[structure]]$Credits[1:base.area.structures[,structure]]))
    }
    return(sum.cost)
}

get.base.cost.per.area <- function(base.name) {
    return(round(get.base.area.structures.cost(base.name)/get.base.max.area(base.name)))
}

get.base.next.cost.per.area.structure <- function(base.name) {
    area.structures <- get.area.structures()
    area.values <- as.numeric(Data$Tables$Structures[area.structures,"Area"])
    area.costs <- c()
    for(structure in area.structures){
        area.costs <- c(area.costs, get.base.structure.next.cost(base.name,structure))
    }
    cpa <- round(area.costs/area.values)
    return(cpa)
}

get.base.next.area.structure <- function(base.name) {
    area.structures <- rownames(Data$Tables$Structures[Data$Tables$Structures$Area>0,])
    cpa <- get.base.next.cost.per.area.structure(base.name)
    return(area.structures[cpa==min(cpa)])
}
########################### Area ################################

########################## Population ###########################
get.population.structures <- function() {
    return(c("Urban.Structures","Orbital.Base","Biosphere.Modification"))
}

get.base.max.population <- function(base.name) {
    pop.bool <- Data$Tables$Structures$Population>0
    biosphere <- Data$Current$Structures[base.name,"Biosphere.Modification"]
    base.starts <- c(
        get.base.fertility(base.name)+biosphere,
        Data$Tables$Structures[pop.bool,"Population"])
    population.levels <- as.numeric(Data$Current$Structures[base.name,
        c("Urban.Structures",rownames(Data$Tables$Structures[pop.bool,]))])
    return(sum(population.levels*base.starts))
}

get.base.current.population <- function(base.name) {
    return(sum(Data$Current$Structures[base.name,
        rownames(Data$Tables$Structures[Data$Tables$Structures$Population<0,])]))
}

get.base.population.structure.cost.each <- function(base.name) {
    population.structures <- get.population.structures()
    base.population.structures <- Data$Current$Structures[base.name,population.structures]
    sum.cost <- c()
    for(structure in population.structures) {
        if(base.population.structures[,structure]==0)
            sum.structure <- 0
        else
            sum.structure <- sum(Data$Structures[[structure]]$Credits[
                1:base.population.structures[,structure]])
        sum.cost <- c(sum.cost,sum.structure)
    }
    return(sum.cost)
}

get.base.population.structure.total.cost <- function(base.name) {
    return(sum(get.base.population.structure.cost.each(base.name)))
}

get.base.cost.per.population <- function(base.name) {
    return(round(get.base.population.structure.total.cost(base.name)/get.base.max.population(base.name)))
}

get.base.next.cost.per.population.structure <- function(base.name) {
    pop.bool <- Data$Tables$Structures$Population>0
    biosphere <- Data$Current$Structures[base.name,"Biosphere.Modification"]
    base.starts <- c(
        get.base.fertility(base.name)+biosphere,
        Data$Tables$Structures["Orbital.Base","Population"],
        get.base.fertility(base.name)+biosphere+1*
            Data$Current$Structures[base.name,"Urban.Structures"])
    structure.modified.next.level.cost <- c(
        get.base.structure.next.cost(base.name,"Urban.Structures")+
            get.base.cost.per.area(base.name),
        get.base.structure.next.cost(base.name,"Orbital.Base"),
        get.base.structure.next.cost(base.name,"Biosphere.Modification")+
            get.base.cost.per.energy(base.name)*
                -Data$Tables$Structures["Biosphere.Modification","Energy"]+
            get.base.cost.per.area(base.name))
    cpp <- round(structure.modified.next.level.cost/base.starts)
    return(cpp)
}

get.base.next.population.structure <- function(base.name) {
    population.structures <- get.population.structures()
    cpp <- get.base.next.cost.per.population.structure(base.name)
    return(population.structures[cpp==min(cpp)])
}

get.base.current.population <- function(base.name) {
    pop.bool <- Data$Tables$Structures$Population<0
    return(sum(c(as.numeric(Data$Current$Structures[base.name,c(
        rownames(Data$Tables$Structures[pop.bool,]),
        rownames(Data$Tables$Defenses[,]))]))))
}
########################## Population ###########################

########################## Energy ###############################
get.energy.structures <- function() {
    return(c("Solar.Plants","Gas.Plants",
        rownames(Data$Tables$Structures[Data$Tables$Structures$Energy>0,])))
}

get.base.max.energy <- function(base.name) {
    base.starts <- c(get.base.solar.gas(base.name),
        Data$Tables$Structures[Data$Tables$Structures$Energy>0,"Energy"])
    energy.levels <- as.numeric(Data$Current$Structures[base.name,get.energy.structures()])
    energy.technology <- Data$Current$Technologies["Energy","Bonus"]+1
    return(sum(c(5,round(energy.levels*base.starts*energy.technology))))
}

get.base.current.energy <- function(base.name) {
    structures <- rownames(Data$Tables$Structures[Data$Tables$Structures$Energy<0,])
    defenses <- rownames(Data$Tables$Defenses[Data$Tables$Defenses$Energy<0,])
    base.structures <- Data$Current$Structures[base.name,c(structures,defenses)]
    structure.energy.consumption <- c(
        Data$Tables$Structures[structures,"Energy"],
        Data$Tables$Defenses[defenses,"Energy"])
    return(sum(-base.structures*structure.energy.consumption))
}

get.base.energy.structure.cost.each <- function(base.name) {
    energy.structures <- get.energy.structures()
    base.energy.structures <- Data$Current$Structures[base.name,energy.structures]
    sum.cost <- c()
    for(structure in energy.structures){
        if(base.energy.structures[,structure]==0)
            sum.structure <- 0
        else
            sum.structure <- sum(Data$Structures[[structure]]$Credits[
                1:base.energy.structures[,structure]])
        sum.cost <- c(sum.cost,sum.structure)
    }
    return(sum.cost)
}

get.base.energy.structure.total.cost <- function(base.name) {
    return(sum(get.base.energy.structure.cost.each(base.name)))
}

get.base.cost.per.energy <- function(base.name) {
    return(round(get.base.energy.structure.total.cost(base.name)/get.base.max.energy(base.name)))
}

get.base.next.cost.per.energy.structure <- function(base.name) {
    orbital.plants <- Data$Current$Structures[base.name,"Orbital.Plants"]
    base.starts <- c(get.base.solar.gas(base.name),
        Data$Tables$Structures[Data$Tables$Structures$Energy>0,"Energy"])
    structure.modified.next.level.cost <- c(
        get.base.structure.next.cost(base.name,"Solar.Plants")+
            get.base.cost.per.area(base.name)+
            get.base.cost.per.population(base.name),
        get.base.structure.next.cost(base.name,"Gas.Plants")+
            get.base.cost.per.area(base.name)+
            get.base.cost.per.population(base.name),
        get.base.structure.next.cost(base.name,"Fusion.Plants")+
            get.base.cost.per.area(base.name)+
            get.base.cost.per.population(base.name),
        get.base.structure.next.cost(base.name,"Antimatter.Plants")+
            get.base.cost.per.area(base.name)+
            get.base.cost.per.population(base.name),
        get.base.structure.next.cost(base.name,"Orbital.Plants")+
            get.base.cost.per.population(base.name))
    cpe <- round(structure.modified.next.level.cost/base.starts)
    return(cpe)
}

get.base.next.energy.structure <- function(base.name) {
    energy.structures <- get.energy.structures()
    cpe <- get.base.next.cost.per.energy.structure(base.name)
    return(energy.structures[cpe==min(cpe)])
}
########################## Energy ###############################

########################## Construction #########################
get.construction.structures <- function() {
    return(c("Metal.Refineries",rownames(Data$Tables$Structures[Data$Tables$Structures$Construction>0,])))
}

get.base.current.construction <- function(base.name) {
    initial.construction <- ifelse(home.base(base.name),40,20)
    base.starts <- c(get.base.metal(base.name),
        Data$Tables$Structures[Data$Tables$Structures$Construction>0,"Construction"])
    construction.levels <- as.numeric(Data$Current$Structures[base.name,get.construction.structures()])
    cybernetics <- Data$Current$Technologies["Cybernetics","Bonus"]+1
    return(round(sum(c(initial.construction,construction.levels*base.starts))*cybernetics))
}

get.base.construction.structure.cost.each <- function(base.name) {
    construction.structures <- get.construction.structures()
    base.construction.structures <- Data$Current$Structures[base.name,construction.structures]
    sum.cost <- c()
    for(structure in construction.structures) {
        if(base.construction.structures[,structure]==0)
            sum.structure <- 0
        else
            sum.structure <- sum(Data$Structures[[structure]]$Credits[
                1:base.construction.structures[,structure]])
        sum.cost <- c(sum.cost,sum.structure)
    }
    return(sum.cost)
}

get.base.construction.structure.total.cost <- function(base.name) {
    return(sum(get.base.construction.structure.cost.each(base.name)))
}

get.base.cost.per.construction <- function(base.name) {
    return(round(get.base.construction.structure.total.cost(base.name)/get.base.construction(base.name)))
}

get.base.next.cost.per.construction.structure <- function(base.name) {
    base.starts <- c(get.base.metal(base.name),
        Data$Tables$Structures[Data$Tables$Structures$Construction>0,"Construction"])
    structure.modified.next.level.cost <- c(
        get.base.structure.next.cost(base.name,"Metal.Refineries")+
            get.base.cost.per.area(base.name)+
            get.base.cost.per.population(base.name),
        get.base.structure.next.cost(base.name,"Robotic.Factories")+
            get.base.cost.per.area(base.name)+
            get.base.cost.per.population(base.name),
        get.base.structure.next.cost(base.name,"Nanite.Factories")+
            get.base.cost.per.area(base.name)+
            get.base.cost.per.population(base.name),
        get.base.structure.next.cost(base.name,"Android.Factories")+
            get.base.cost.per.area(base.name)+
            get.base.cost.per.population(base.name))
    cpc <- round(structure.modified.next.level.cost/base.starts)
    return(cpc)
}

get.base.next.construction.structure <- function(base.name) {
    construction.structures <- get.construction.structures()
    cpc <- get.base.next.cost.per.construction.structure(base.name)
    return(construction.structures[cpc==min(cpc)])
}
