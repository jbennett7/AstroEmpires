load(".datafile.Rdata")

Trade <- new.env()
Trade$income <- function(low.base,distance,players) {
    return(ceiling(
        sqrt(low.base)*(1+(sqrt(distance)/75)+(sqrt(players)/10))))
}

Fleet <- new.env()
Fleet$support <- function(eco) {
    return(eco^1.6+(eco/100)^3.2)
}

Fleet$mainenance <- function(fleet.above) {
    return((fleet.above/125)^.7)
}

Base <- new.env()
Base$resources <- function(bases) {
    return(Data$Tables$Terrain[Data$Current$Bases[bases,"Terrain"],])
}

Base$solar <- function(bases) {
    pos.solar <- Data$Tables$Astro.Position["Solar.Energy",
        Data$Current$Bases[bases,"Position"]]
    technology <- 1+Data$Current$Technologies["Energy","Bonus"]
    return(round(as.numeric(pos.solar*technology)))
}

Base$gas <- function(bases) {
    info <- Data$Current$Bases[bases,c("Position","Terrain")]
    ter.gas <- Data$Tables$Terrain[info[,2],"Gas"]
    pos.gas <- Data$Tables$Astro.Position["Gas",info[,1]]
    technology <- 1+Data$Current$Technologies["Energy","Bonus"]
    return(round(as.numeric(ter.gas+pos.gas)*technology))
}

Base$fertility <- function(bases) {
    biosphere <- Data$Current$Structures[bases,"Biosphere.Modification"]
    info <- Data$Current$Bases[bases,c("Position","Terrain")]
    ter.fertility <- Data$Tables$Terrain[info[,2],"Fertility"]
    pos.fertility <- Data$Tables$Astro.Position["Fertility",info[,1]]
    return(as.numeric(ter.fertility+pos.fertility+biosphere))
}

Base$metal <- function(bases) {
    info <- Data$Current$Bases[bases,c("Terrain")]
    ter.metal <- Data$Tables$Terrain[info,"Metal"]
    return(as.numeric(ter.metal))
}

Base$crystals <- function(bases) {
    info <- Data$Current$Bases[bases,c("Terrain")]
    ter.crystals <- Data$Tables$Terrain[info,"Crystals"]
    return(as.numeric(ter.crystals))
}

Base$economy <- function(bases) {
    crystals <- Base$crystals(bases)
    eco.struct.values <- Data$Tables$Structures[
        Data$Tables$Structures$Economy>0,"Economy"]
    eco.values <- cbind(crystals,
        matrix(rep(eco.struct.values,length(bases)),
            nrow=length(bases),byrow=TRUE))
    structure.levels <- Structures$economy(bases)
    initial.values <- rowSums(eco.values*structure.levels)
    return(initial.values)
}

Base$construction <- function(bases) {
    metals <- Base$metal(bases)
    const.struct.values <- Data$Tables$Structures[
        Data$Tables$Structures$Construction>0,"Construction"]
    const.values <- cbind(metals,
        matrix(rep(const.struct.values,length(bases)),
            nrow=length(bases),byrow=TRUE))
    structure.levels <- Structures$construction(bases)
    cybernetics <- Data$Current$Technologies["Cybernetics","Bonus"]
    initial.values <- rowSums(const.values*structure.levels)
    home <- Data$Current$Home
    if(home %in% bases)
        initial.values[home] <- initial.values[home] + 20
    return(round((initial.values+20)*(1+cybernetics)))
}

Base$production <- function(bases) {
    metals <- Base$metal(bases)
    prod.struct.values <- Data$Tables$Structures[
        Data$Tables$Structures$Production>0,"Production"]
    prod.values <- cbind(metals,
        matrix(rep(prod.struct.values,length(bases)),
            nrow=length(bases),byrow=TRUE))
    colnames(prod.values) <- NULL
    structure.levels <- Structures$production(bases)
    cybernetics <- Data$Current$Technologies["Cybernetics","Bonus"]
    initial.values <- rowSums(prod.values*structure.levels)
    return(round(initial.values*(1+cybernetics)))
}

Base$research <- function(bases) {
    ai <- Data$Current$Technologies["Artificial.Intelligence","Bonus"]
    research.labs <- Structures$research(bases)
    return(round((research.labs*8)*(1+ai)))
}

Base$dependencies <- function() {
    return(c("Area","Population","Energy"))
}


Structures <- new.env()
Structures$area <- function(bases) {
    area.structs <- rownames(Data$Tables$Structures[Data$Tables$Structures$Area>0,])
    return(Data$Current$Structures[bases,area.structs])
}

Structures$population <- function(bases) {
    pop.structs <- c("Urban.Structures",
        rownames(Data$Tables$Structures[
            Data$Tables$Structures$Population>0,]))
    return(Data$Current$Structures[bases,pop.structs])
}

Structures$energy <- function(bases) {
    energy.structs <- c("Solar.Plants","Gas.Plants",
        rownames(Data$Tables$Structures[Data$Tables$Structures$Energy>0,]))
    return(Data$Current$Structures[bases,energy.structs])
}

#TODO: Add the Capital influence to bases. (10 per level for the host base; 2 per level for all other bases)
Structures$economy <- function(bases) {
    econ.structs <- c("Crystal.Mines",
        rownames(Data$Tables$Structures[Data$Tables$Structures$Economy>0,]))
    return(Data$Current$Structures[bases,econ.structs])
} 

Structures$construction <- function(bases) {
    const.structs <- c("Metal.Refineries",
        rownames(Data$Tables$Structures[Data$Tables$Structures$Construction>0,]))
    return(Data$Current$Structures[bases,const.structs])
}

Structures$production <- function(bases) {
    prod.structs <- c("Metal.Refineries",
        rownames(Data$Tables$Structures[Data$Tables$Structures$Production>0,]))
    return(Data$Current$Structures[bases,prod.structs])
}

Structures$research <- function(bases) {
    return(Data$Current$Structures[bases,"Research.Labs"])
}

Structures$costs <- function(bases,structures,i=0) {
    current.structures <- Data$Current$Structures[bases,structures,drop=F]
    for(structure in structures)
        for(base in bases){
            lvl <- current.structures[base,structure]
            current.structures[base,structure] <- ifelse(lvl!=0,
                Data$Structures[[structure]]$Credits[lvl+i],0)
        }
    return(current.structures)
}

Structures$current.costs <- function(bases,structures) {
    return(Structures$costs(bases,structures))
}

Structures$next.costs <- function(bases,structures) {
    return(Structures$costs(bases,structures,1))
}

#
Planning <- new.env()
#Planning$base.construction(bases) {}
#Planning$structure.build.time(bases,structures) {

#    gas.values <- base.gas(bases)
#    energy.values <- Data$Tables$Structures[Data$Tables$Structures$Energy>0,"Energy"]
#    df <- cbind(solar.values,gas.values,matrix(rep(energy.values,length(bases)),nrow=length(bases),byrow=TRUE))
#    colnames(df) <- energy.structures()
#    rownames(df) <- bases
#    return(df)
#}
#
#population.structure.values <- function(bases) {
#    biosphere.level <- Data$Current$Structures[bases,"Biosphere.Modification"]
#    urban.structure.level <- Data$Current$Structures[bases,"Urban.Structures"]
#    bases.fertility <- base.fertility(bases)+biosphere.level
#    population.values <- Data$Tables$Structures[Data$Tables$Structures$Population>0,"Population"]
#    df <- cbind(bases.fertility,matrix(rep(population.values,length(bases)),nrow=length(bases),byrow=TRUE),
#        urban.structure.level)
#    colnames(df) <- population.structures()
#    rownames(df) <- bases
#    return(df)
#}
#
#construction.structure.values <- function(bases) {
#    metal.values <- base.metal(bases)
#    construction.values <- Data$Tables$Structures[Data$Tables$Structures$Construction>0,"Construction"]
#    df <- cbind(metal.values,matrix(rep(construction.values,length(bases)),nrow=length(bases),byrow=TRUE))
#    colnames(df) <- construction.structures()
#    rownames(df) <- bases
#    return(df)
#}
#
#production.structure.values <- function(bases) {
#    metal.values <- base.metal(bases)
#    production.values <- Data$Tables$Structures[Data$Tables$Structures$Production>0,"Production"]
#    df <- cbind(metal.values,matrix(rep(production.values,length(bases)),nrow=length(bases),byrow=TRUE))
#    colnames(df) <- production.structures()
#    rownames(df) <- bases
#    return(df)
#}
#
#economy.structure.values <- function(bases) {
#    crystal.values <- base.crystals(bases)
#    economy.values <- Data$Tables$Structures[Data$Tables$Structures$Economy>0,"Economy"]
#    df <- cbind(crystal.values,matrix(rep(economy.values,length(bases)),nrow=length(bases),byrow=TRUE))
#    colnames(df) <- economy.structures()
#    rownames(df) <- bases
#    return(df)
#}
#
#base.area <- function(bases) {
#    return(rowSums(area.structure.values(bases)*Data$Current$Structures[bases,area.structures()]))
#}
#base.energy <- function(bases) {
#    return(rowSums(energy.structure.values(bases)*Data$Current$Structures[bases,energy.structures()]))
#}
#base.population <- function(bases) {
#    return(rowSums(population.structure.values(bases)*Data$Current$Structures[bases,population.structures()]))
#}
#base.construction <- function(bases) {
#    return(rowSums(construction.structure.values(bases)*Data$Current$Structures[bases,construction.structures()]))
#}
#base.production <- function(bases) {
#    return(rowSums(production.structure.values(bases)*Data$Current$Structures[bases,production.structures()]))
#}
#base.economy <- function(bases) {
#    return(rowSums(economy.structure.values(bases)*Data$Current$Structures[bases,economy.structures()]))
#}
#
#base.structures.level.costs <- function(bases,structures) {
#}
#
#base.structure.dependencies <- function(structures) {
#    return(-Data$Tables$Structures[structures,
#           colSums(Data$Tables$Structures[structures,
#                   unlist(lapply(Data$Tables$Structures,is.numeric))]<0)>0,drop=F])
#}

