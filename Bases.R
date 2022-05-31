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
        structures = "matrix",
        sfmgca = "matrix",
        population = "numeric",
        energy = "numeric",
        area = "numeric",
        economy = "numeric",
        construction = "numeric",
        production = "numeric",
        research = "numeric"
    ),
    prototype = prototype(
        sfmgca = matrix(0),
        population = c(0,0),
        energy = c(0,0),
        area = c(0,0),
        economy = 0,
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

        .Object@home = if(Data$Current$Home==name) TRUE else FALSE

        .Object@structures = {
            tmp <- as.matrix(as.numeric(Data$Current$Structures[name,]), ncol = 1)
            row.names(tmp) <- names(Data$Current$Structures)
            tmp
        }

        .Object@sfmgca = {
            base.info <- Data$Current$Bases[name,]
            terrain <- Data$Tables$Terrains[base.info$Terrain,]
            area <- if(base.info$Type == "Planet") terrain$Area.Planet else terrain$Area.Moon
            tmp <- as.matrix( c(
                astro.pos["Solar.Energy", base.info$Position],
                astro.pos["Fertility", base.info$Position]+terrain$Fertility,
                terrain$Metal,
                astro.pos["Gas", base.info$Position]+terrain$Gas,
                terrain$Crystals,
                area), ncol=1)
            row.names(tmp) <- c("Solar.Energy", "Fertility", "Metal", "Gas", "Crystals", "Area")
            tmp
        }

        .Object@population = {
            c( sum(.Object@structures[c(row.names(struct.refs[struct.refs$Population<0,]),
                   row.names(defense.refs)),]),
               sum(.Object@structures[c("Urban.Structures", "Orbital.Base"),]*
                   c(.Object@sfmgca["Fertility",], 10)))
        }

        .Object@energy = {
            c( sum(.Object@structures[c(row.names(struct.refs[struct.refs$Energy<0,]),
                   row.names(defense.refs[defense.refs$Energy<0,])),]*
                   -c(struct.refs[struct.refs$Energy<0,"Energy"],
                       defense.refs[defense.refs$Energy<0, "Energy"])),
               round(sum(2,
                   .Object@structures[c("Solar.Plants", "Gas.Plants",
                       row.names(struct.refs[struct.refs$Energy>0,])),]*
                   c(.Object@sfmgca[c("Solar.Energy", "Gas"),],
                       struct.refs[struct.refs$Energy>0, "Energy"]))*
                   energy.tech))
        }
 
        .Object@area = {
            c(
               sum(.Object@structures[c(row.names(struct.refs[struct.refs$Area<0,]),
                   row.names(defense.refs[defense.refs$Area<0,])),]*
                   -c(struct.refs[struct.refs$Area<0,"Area"],
                       defense.refs[defense.refs$Area<0,"Area"])),
               sum(.Object@sfmgca["Area",],
                   .Object@structures[row.names(struct.refs[struct.refs$Area>0,]),]*
                   struct.refs[struct.refs$Area>0,"Area"]))
        }

        .Object@economy=sum(.Object@structures[row.names(struct.refs[struct.refs$Economy>0,]),]*
                            struct.refs[struct.refs$Economy>0,"Economy"])

        .Object@construction={
            cons.d<-if(.Object@home) 40 else 20
            round(sum(cons.d,sum(
            .Object@structures[c("Metal.Refineries",
                row.names(struct.refs[struct.refs$Construction>0,])),]*
                c(.Object@sfmgca["Metal",],
                    struct.refs[struct.refs$Construction>0,"Construction"])))*
                cyber.tech)
        }

        .Object@production=round(sum(
            .Object@structures[c("Metal.Refineries",
                row.names(struct.refs[struct.refs$Production>0,])),]*
            c(.Object@sfmgca["Metal",],struct.refs[struct.refs$Production>0,"Production"]))*
            cyber.tech)

        .Object@research=round(as.numeric(.Object@structures["Research.Labs",]*8*ai.tech))

        return(.Object)
    }
)
new(Class="Base",name="Primus")
