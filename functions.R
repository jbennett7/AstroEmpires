source("config")
load(data.file)

get.bases <- function() {
    return(rownames(Data$Current$Bases))
}

get.base.solar <- function(base.name) {
    return(as.numeric(Data$Current$Bases[base.name,c("Solar")]))
}

get.base.fertility <- function(base.name) {
    return(as.numeric(Data$Current$Bases[base.name,c("Fertility")]))
}

get.base.metal <- function(base.name) {
    return(as.numeric(Data$Current$Bases[base.name,c("Metal")]))
}

get.base.gas <- function(base.name) {
    return(as.numeric(Data$Current$Bases[base.name,c("Gas")]))
}

get.base.crystals <- function(base.name) {
    return(as.numeric(Data$Current$Bases[base.name,c("Crystals")]))
}

get.base.construction <- function(base.name) {
    con.bool <- Data$Tables$Structures$Construction>0
    base.starts <- c(get.base.metal(base.name),Data$Tables$Structures[con.bool,"Construction"])
    construction.levels <- as.numeric(Data$Current$Structures[base.name,
        c("Metal.Refineries", rownames(Data$Tables$Structures[con.bool,]))])
    cybernetics.technology = Data$Current$Technologies["Cybernetics",c("Bonus")]
    return(round(sum(c(20,construction.levels*base.starts))*(1+cybernetics.technology)))
}

get.base.production <- function(base.name) {
    prod.bool <- Data$Tables$Structures$Production>0
    base.starts <- c(get.base.metal(base.name),Data$Tables$Structures[prod.bool,"Production"])
    production.levels <- as.numeric(Data$Current$Structures[base.name,
        c("Metal.Refineries", rownames(Data$Tables$Structures[prod.bool,]))])
    cybernetics.technology = Data$Current$Technologies["Cybernetics",c("Bonus")]
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

get.base.structure.next.level <- function(base.name,structure) {
    return(Data$Current$Structures[base.name,structure]+1)
}

get.base.structure.next.cost <- function(base.name,structure) {
    structure.next.level <- get.base.structure.next.level(base.name,structure)
    return(Data$Structures[[structure]][structure.next.level,"Credits"])
}

get.base.structure.current.sum.cost <- function(base.name,structure) {
    structure.level <- get.base.structure.next.level(base.name,structure)
    if(structure.level==0) return(0)
    credits <- Data$Structures[[structure]][,"Credits"]
    return(sum(credits[1:(structure.level)]))
}

############# Population ###########################
get.base.max.population <- function(base.name) {
    pop.bool <- Data$Tables$Structures$Population>0
    biosphere <- Data$Current$Structures[base.name,"Biosphere.Modification"]
    base.starts <- c(get.base.fertility(base.name)+biosphere,Data$Tables$Structures[pop.bool,"Population"])
    population.levels <- as.numeric(Data$Current$Structures[base.name,
        c("Urban.Structures",rownames(Data$Tables$Structures[pop.bool,]))])
    return(sum(population.levels*base.starts))
}
##
get.base.population.cost <- function(base.name) {
    return(sum(c(get.base.structure.current.sum.cost(base.name,"Urban.Structures"),
                 get.base.structure.current.sum.cost(base.name,"Orbital.Base"),
                 get.base.structure.current.sum.cost(base.name,"Biosphere.Mod"))))
}

get.base.cost.per.structure <- function(base.name) {
    return(round(get.base.population.cost(base.name)/get.base.max.population(base.name)))
}

build.next.population.cost.per.structure <- function(base.name) {
    biosphere <- Data$Current$Structures[base.name,"Biosphere.Mod"]
    base.starts <- c(get.base.fertility(base.name)+biosphere,10,
            get.base.structure.next.level(base.name,"Urban.Structures")-1)
    structure.modified.next.level.cost <- c(
        get.base.structure.next.cost(base.name,"Urban.Structures")+
            get.base.cost.per.area(base.name),
        get.base.structure.next.cost(base.name,"Orbital.Base"),
        get.base.structure.next.cost(base.name,"Biosphere.Modification")+
            get.base.cost.per.energy(base.name)*24+
            get.base.cost.per.area(base.name))
    cpp <- structure.modified.next.level.cost/base.starts
    return(min(cpp))
}
build.next.population.structure <- function(base.name) {
    population.structures <- c(
        "Urban.Structures",
        "Orbital.Base",
        "Biosphere.Mod")
    cpp <- build.next.population.cost.per.structure(base.name)
    return(population.structures[cpp==min(cpp)])
}

get.base.current.population <- function(base.name) {
    return(sum(c(as.numeric(Data$Current$Structures[base.name,c(
        "Solar.Plants",
        "Gas.Plants",
        "Fusion.Plants",
        "Antimatter.Plants",
        "Orbital.Plants",
        "Research.Labs",
        "Metal.Refineries",
        "Crystal.Mines",
        "Robotic.Factories",
        "Shipyards",
        "Orbital.Shipyards",
        "Spaceports",
        "Command.Centers",
        "Nanite.Factories",
        "Android.Factories",
        "Economic.Centers",
        "Jump.Gate",
        "Biosphere.Mod",
        "Barracks",
        "Laser.Turrets",
        "Missile.Turrets",
        "Plasma.Turrets",
        "Ion.Turrets",
        "Photon.Turrets",
        "Disruptor.Turrets",
        "Deflection.Shields",
        "Planetary.Shield",
        "Planetary.Ring")]))))
}
############# Population ###########################

############ Area #########################
get.base.max.area <- function(base.name) {
    planet.area <- 85
    area.starts <- c(5, 10)
    area.modifiers <- as.numeric(Data$Current$Structures[base.name,c(
        "Terraform","Multi.Level.Platforms")])
    return(sum(c(planet.area,area.starts*area.modifiers)))
}

get.base.area.cost <- function(base.name) {
    return(sum(c(get.base.structure.current.sum.cost(base.name,"Terraform"),
                 get.base.structure.current.sum.cost(base.name,"Multi.Level.Platforms"))))
}

get.base.cost.per.area <- function(base.name) {
    return(round(get.base.area.cost(base.name)/get.base.max.area(base.name)))
}

get.base.next.area.cost.per.area <- function(base.name) {
    cpa <- c(
        get.base.structure.next.cost(base.name,"Multi.Level.Platforms")/10,
        get.base.structure.next.cost(base.name,"Terraform")/5)
    return(min(cpa))
}

build.next.area <- function(base.name) {
    cpa <- get.base.next.area.cost.per.area(base.name)
    return(c("Multi.Level.Platforms","Terraform")[cpa==min(cpa)])
}
############ Area #########################

############ Energy ############################
get.base.max.energy <- function(base.name) {
    base.starts <- c(as.numeric(Data$Current$Bases[base.name,c("Solar","Gas")]),
                     4,10,24)
    energy.technology <- Data$Current$Technologies["Energy",c("Bonus")]
    energy.levels <- as.numeric(Data$Current$Structures[base.name,
         c("Solar.Plants",
           "Gas.Plants",
           "Fusion.Plants",
           "Antimatter.Plants",
           "Orbital.Plants")])
    return(4+round(sum(energy.levels*base.starts)*(1+energy.technology)))
}

get.base.energy.cost <- function(base.name) {
    s <- 0
    for(e in c("Solar.Plants",
               "Gas.Plants",
               "Fusion.Plants",
               "Antimatter.Plants",
               "Orbital.Plants")){
        s <- s + get.base.structure.current.sum.cost(base.name,e)
    }
    return(s)
}

get.base.cost.per.energy <- function(base.name) {
    return(round(get.base.energy.cost(base.name)/get.base.max.energy(base.name)))
}

get.base.modified.cost.per.energy <- function(base.name) {
    return(sum(c(
        get.base.cost.per.energy(base.name),
        get.base.next.area.cost.per.area(base.name),
        get.base.cost.per.population(base.name))))
}

get.base.next.energy.cost.per.structure <- function(base.name) {
    energy.structures <- c("Solar.Plants","Gas.Plants","Fustion.Plants",
                           "Antimatter.Plants","Orbital.Plants")
    base.starts <- c(as.numeric(Data$Current$Bases[base.name,c("Solar","Gas")]),
        4,10,24)
    structure.modified.next.level.cost <- c(
        get.base.structure.next.cost(base.name,"Solar.Plants"),
        get.base.structure.next.cost(base.name,"Gas.Plants"),
        get.base.structure.next.cost(base.name,"Fusion.Plants"),
        get.base.structure.next.cost(base.name,"Antimatter.Plants"),
        get.base.structure.next.cost(base.name,"Orbital.Plants")) +
            get.base.next.area.cost.per.area(base.name) +
            get.base.next.population.cost.per.structure(base.name)
    cpe <- base.starts/structure.modified.next.level.cost
    return(energy.structures[cpe==max(cpe)])
}

get.base.current.energy <- function(base.name) {}
############ Energy ############################
