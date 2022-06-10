load(".datafile.Rdata")
initial.energy <- 2
struct.refs <- Data$Tables$Structures
defense.refs <- Data$Tables$Defenses
astro.pos <- Data$Tables$Astro.Position
energy.tech <- 1+Data$Current$Technologies["Energy","Bonus"]
ai.tech <- 1+Data$Current$Technologies["Artificial.Intelligence","Bonus"]
cyber.tech <- 1+Data$Current$Technologies["Cybernetics","Bonus"]

setClass(
    Class = "Base",
    representation = representation(
        name = "character",
        home = "logical",
        structures = "data.frame",
        defenses = "data.frame",
        resources = "matrix",
        construction = "numeric",
        production = "numeric",
        research = "numeric"
    ),
    prototype = prototype(
        resources = matrix(0),
        construction = 0,
        production = 0,
        research = 0
    )
)
setMethod(
    f = "initialize",
    signature = "Base",
    definition = function(.Object, name){
        .Object@name = name

        .Object@resources = {
            base.info <- Data$Current$Bases[name,]
            terrain <- Data$Tables$Terrains[base.info$Terrain,]
            area <- if(base.info$Type == "Planet") terrain$Area.Planet else terrain$Area.Moon
            tmp <- as.matrix( c(
                astro.pos["Solar.Energy", base.info$Position],
                astro.pos["Fertility", base.info$Position]+
                    terrain$Fertility+
                    as.numeric(Data$Current$Structures[.Object@name,"Biosphere.Modification"]),
                terrain$Metal,
                astro.pos["Gas", base.info$Position]+terrain$Gas,
                terrain$Crystals,
                area), ncol=1)
            row.names(tmp) <- c("Solar.Energy", "Fertility", "Metal", "Gas", "Crystals", "Area")
            tmp
        }

        .Object@structures = {
            tmp <- Data$Tables$Structures[
                !row.names(Data$Tables$Structures) %in% c("Crystal.Mines"),]
            tmp["Metal.Refineries", c("Production", "Construction")] <- .Object@resources["Metal",]
            tmp["Solar.Plants","Energy"] <- .Object@resources["Solar.Energy",]
            tmp["Gas.Plants","Energy"] <- .Object@resources["Gas",]
            tmp["Urban.Structures","Population"] <- .Object@resources["Fertility",]
            tmp <- tmp[, !names(tmp) %in% c("Credits", "Requires", "Advanced")]
            Level <- Data$Current$Structures[.Object@name,row.names(tmp)]
            Credits <- c()
            for(table.name in row.names(tmp)){
                Credits <- c(Credits,
                    Data$Structures[[table.name]][as.numeric(Level[table.name])+1,"Credits"])
            }
            Level <- as.numeric(Level)
            tmp <- cbind(Level, Credits, tmp)
        }

        .Object@defenses = {
            tmp <- Data$Tables$Defenses
            tmp <- tmp[,!names(tmp) %in% c("Credits", "Requires", "Weapon")]
            Level <- Data$Current$Structures[.Object@name,row.names(tmp)]
            Credits <- c()
            for(table.name in row.names(tmp)){
                Credits <- c(Credits,
                    Data$Structures[[table.name]][as.numeric(Level[table.name])+1,"Credits"])
            }
            Level <- as.numeric(Level)
            Population <- rep(-1, length(row.names(tmp)))
            tmp <- cbind(Level, Credits, tmp, Population)
            tmp
        }

        return(.Object)
    }
)
setGeneric("isHome", function(.Object){standardGeneric("isHome")})
setMethod(
    f = "isHome",
    signature = "Base",
    definition = function(.Object){
        return(Data$Current$Home == .Object@name)
    }
)
setGeneric("getPopulation", function(.Object){standardGeneric("getPopulation")})
setMethod(
    f = "getPopulation",
    signature = "Base",
    definition = function(.Object){
            c( sum(c(.Object@structures[.Object@structures$Population<0,"Level"],
                     .Object@defenses[.Object@defenses$Population<0,"Level"])*
                   -c(.Object@structures[.Object@structures$Population<0,"Population"],
                      .Object@defenses[.Object@defenses$Population<0,"Population"])),
               sum(.Object@structures[.Object@structures$Population>0,"Level"]*
                   .Object@structures[.Object@structures$Population>0,"Population"]))
    }
)
setGeneric("getEnergy", function(.Object){standardGeneric("getEnergy")})
setMethod(
    f = "getEnergy",
    signature = "Base",
    definition = function(.Object){
            energy.tech <- 1+Data$Current$Technologies["Energy","Bonus"]
            c( sum(c(.Object@structures[.Object@structures$Energy<0,"Level"],
                     .Object@defenses[.Object@defenses$Energy<0,"Level"])*
                  -c(.Object@structures[.Object@structures$Energy<0, "Energy"],
                     .Object@defenses[.Object@defenses$Energy<0, "Energy"])),
               round(sum(2,
                   .Object@structures[row.names(.Object@structures[.Object@structures$Energy>0,]), "Level"]*
                   c(.Object@structures[.Object@structures$Energy>0, "Energy"]))*
                   energy.tech))
    }
)
setGeneric("getArea", function(.Object){standardGeneric("getArea")})
setMethod(
    f = "getArea",
    signature = "Base",
    definition = function(.Object){
        c(
           sum(c(.Object@structures[.Object@structures$Area<0, "Level"],
                 .Object@defenses[.Object@defenses$Area<0, "Level"])*
              -c(.Object@structures[.Object@structures$Area<0, "Area"],
                 .Object@defenses[.Object@defenses$Area<0, "Area"])),
           sum(.Object@resources["Area",],
               .Object@structures[row.names(.Object@structures[.Object@structures$Area>0,]), "Level"]*
             c(.Object@structures[.Object@structures$Area>0, "Area"])))
    }
)
setGeneric("getEconomy", function(.Object){standardGeneric("getEconomy")})
setMethod(
    f = "getEconomy",
    signature = "Base",
    definition = function(.Object){
        sum(.Object@structures[row.names(.Object@structures[.Object@structures$Economy>0,]), "Level"]*
            .Object@structures[row.names(.Object@structures[.Object@structures$Economy>0,]), "Economy"])
    }
)

setGeneric("getConstruction", function(.Object){standardGeneric("getConstruction")})
setMethod(
    f = "getConstruction",
    signature = "Base",
    definition = function(.Object){
        cons.d<-if(isHome(.Object)) 40 else 20
        cyber.tech <- 1+Data$Current$Technologies["Cybernetics", "Bonus"]
        round(sum(cons.d,
            .Object@structures[row.names(.Object@structures[.Object@structures$Construction>0,]), "Level"]*
            .Object@structures[row.names(.Object@structures[.Object@structures$Construction>0,]), "Construction"])*
            cyber.tech)
    }
)

setGeneric("getProduction", function(.Object){standardGeneric("getProduction")})
setMethod(
    f = "getProduction",
    signature = "Base",
    definition = function(.Object){
        cyber.tech <- 1+Data$Current$Technologies["Cybernetics", "Bonus"]
        round(sum(
            .Object@structures[row.names(.Object@structures[.Object@structures$Production>0,]), "Level"]*
            .Object@structures[row.names(.Object@structures[.Object@structures$Production>0,]), "Production"])*
            cyber.tech)
    }
)

setGeneric("getResearch", function(.Object){standardGeneric("getResearch")})
setMethod(
    f = "getResearch",
    signature = "Base",
    definition = function(.Object){
        ai.tech <- 1+Data$Current$Technologies["Artificial.Intelligence", "Bonus"]
        round(as.numeric(.Object@structures["Research.Labs", "Level"]*8*ai.tech))
    }
)

{Primus<-new(Class="Base",name="Primus")}
getPopulation(Primus)
getEnergy(Primus)
getArea(Primus)
getEconomy(Primus)
getConstruction(Primus)
getProduction(Primus)
getResearch(Primus)
