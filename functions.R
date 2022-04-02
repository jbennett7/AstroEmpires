load(".datafile.Rdata")

base.resources <- function(bases) {
    bases.info <- Data$Current$Bases[bases,]
    resources <- Data$Tables$Terrain[Data$Current$Bases[bases,"Terrain"]]
    return(resources)
}

base.solar <- function(bases) {
    return(Data$Tables$Astro.Position["Solar.Energy",
        Data$Current$Bases[bases,"Position"]])
}

base.gas <- function(bases) {
    info <- Data$Current$Bases[bases,c("Position","Terrain")]
    return(as.numeric(Data$Tables$Terrain[info[,2],"Gas"] +
        Data$Tables$Astro.Position["Gas",info[,1]]))
}

base.fertility <- function(bases) {
    info <- Data$Current$Bases[bases,c("Position","Terrain")]
    return(as.numeric(Data$Tables$Terrain[info[,2],"Fertility"] +
        Data$Tables$Astro.Position["Fertility",info[,1]]))
}

base.metal <- function(bases) {
    info <- Data$Current$Bases[bases,c("Terrain")]
    return(as.numeric(Data$Tables$Terrain[info,"Metal"]))
}

base.crystals <- function(bases) {
    info <- Data$Current$Bases[bases,c("Terrain")]
    return(as.numeric(Data$Tables$Terrain[info,"Crystals"]))
}

base.dependencies <- function() {
    return(c("Area","Population","Energy"))
}

dependency.structures <- function(dependencies) {
    l <- list(Area=area.structures(),Population=population.structures(),Energy=energy.structures())
    return(l[dependencies])
}

energy.structures <- function() {
    return(c("Solar.Plants","Gas.Plants",rownames(Data$Tables$Structures[Data$Tables$Structures$Energy>0,])))
}

area.structures <- function() {
    return(rownames(Data$Tables$Structures[Data$Tables$Structures$Area>0,]))
}

population.structures <- function() {
    return(c("Urban.Structures",rownames(Data$Tables$Structures[Data$Tables$Structures$Population>0,]),
        "Biosphere.Modification"))
}

economy.structures <- function() {
    return(c("Crystal.Mines",rownames(Data$Tables$Structures[Data$Tables$Structures$Economy>0,])))
}

construction.structures <- function() {
    return(c("Metal.Refineries",rownames(Data$Tables$Structures[Data$Tables$Structures$Construction>0,])))
}

production.structures <- function() {
    return(c("Metal.Refineries",rownames(Data$Tables$Structures[Data$Tables$Structures$Production>0,])))
}

area.structure.values <- function(bases) {
    df <- matrix(rep(Data$Tables$Structures[Data$Tables$Structures$Area>0,"Area"],
        length(bases)),nrow=length(bases),byrow=TRUE)
    colnames(df) <- area.structures()
    rownames(df) <- bases
    return(df)
}

energy.structure.values <- function(bases) {
    solar.values <- base.solar(bases)
    gas.values <- base.gas(bases)
    energy.values <- Data$Tables$Structures[Data$Tables$Structures$Energy>0,"Energy"]
    df <- cbind(solar.values,gas.values,matrix(rep(energy.values,length(bases)),nrow=length(bases),byrow=TRUE))
    colnames(df) <- energy.structures()
    rownames(df) <- bases
    return(df)
}

population.structure.values <- function(bases) {
    biosphere.level <- Data$Current$Structures[bases,"Biosphere.Modification"]
    urban.structure.level <- Data$Current$Structures[bases,"Urban.Structures"]
    bases.fertility <- base.fertility(bases)+biosphere.level
    population.values <- Data$Tables$Structures[Data$Tables$Structures$Population>0,"Population"]
    df <- cbind(bases.fertility,matrix(rep(population.values,length(bases)),nrow=length(bases),byrow=TRUE),
        urban.structure.level)
    colnames(df) <- population.structures()
    rownames(df) <- bases
    return(df)
}

construction.structure.values <- function(bases) {
    metal.values <- base.metal(bases)
    construction.values <- Data$Tables$Structures[Data$Tables$Structures$Construction>0,"Construction"]
    df <- cbind(metal.values,matrix(rep(construction.values,length(bases)),nrow=length(bases),byrow=TRUE))
    colnames(df) <- construction.structures()
    rownames(df) <- bases
    return(df)
}

production.structure.values <- function(bases) {
    metal.values <- base.metal(bases)
    production.values <- Data$Tables$Structures[Data$Tables$Structures$Production>0,"Production"]
    df <- cbind(metal.values,matrix(rep(production.values,length(bases)),nrow=length(bases),byrow=TRUE))
    colnames(df) <- production.structures()
    rownames(df) <- bases
    return(df)
}

economy.structure.values <- function(bases) {
    crystal.values <- base.crystals(bases)
    economy.values <- Data$Tables$Structures[Data$Tables$Structures$Economy>0,"Economy"]
    df <- cbind(crystal.values,matrix(rep(economy.values,length(bases)),nrow=length(bases),byrow=TRUE))
    colnames(df) <- economy.structures()
    rownames(df) <- bases
    return(df)
}

base.area <- function(bases) {
    return(rowSums(area.structure.values(bases)*Data$Current$Structures[bases,area.structures()]))
}
base.energy <- function(bases) {
    return(rowSums(energy.structure.values(bases)*Data$Current$Structures[bases,energy.structures()]))
}
base.population <- function(bases) {
    return(rowSums(population.structure.values(bases)*Data$Current$Structures[bases,population.structures()]))
}
base.construction <- function(bases) {
    return(rowSums(construction.structure.values(bases)*Data$Current$Structures[bases,construction.structures()]))
}
base.production <- function(bases) {
    return(rowSums(production.structure.values(bases)*Data$Current$Structures[bases,production.structures()]))
}
base.economy <- function(bases) {
    return(rowSums(economy.structure.values(bases)*Data$Current$Structures[bases,economy.structures()]))
}

base.structures.level.costs <- function(bases,structures) {
}

base.structure.dependencies <- function(structures) {
    return(-Data$Tables$Structures[structures,
           colSums(Data$Tables$Structures[structures,
                   unlist(lapply(Data$Tables$Structures,is.numeric))]<0)>0,drop=F])
}

