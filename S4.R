load(".datafile.Rdata")
initial.energy<-5
setClass(
    Class="Base",
    representation=representation(
        name="character",
        home="logical",
        sfmgc="numeric",
        population="numeric",
        energy="numeric",
        area="numeric",
        economy="numeric",
        construction="numeric",
        production="numeric",
        research="numeric"
    ),
    prototype=prototype(
        sfmgc=c(0,0,0,0,0),
        population=c(0,0),
        energy=c(0,0),
        area=c(0,0),
        economy=0,
        construction=0,
        production=0,
        research=0
    )
)
setMethod(
    f="initialize",
    signature="Base",
    definition=function(.Object,name){
        .Object@name=name
        .Object@home=if(Data$Current$Home==name)TRUE else FALSE
        str.mat<-Data$Tables$Structures
        def.mat<-Data$Tables$Defenses
        str<-Data$Current$Structures[name,]
        b.info<-Data$Current$Bases[name,]
        ap<-Data$Tables$Astro.Position
        ter<-Data$Tables$Terrains[b.info$Terrain,]
        energy.tech<-1+Data$Current$Technologies["Energy","Bonus"]
        ai.tech<-1+Data$Current$Technologies["Artificial.Intelligence","Bonus"]
        cyber.tech<-1+Data$Current$Technologies["Cybernetics","Bonus"]
        area.d<-if(b.info$Type=="Planet") ter$Area.Planet else ter$Area.Moon
        cons.d<-if(.Object@home) 40 else 20

        s<-ap["Solar.Energy",b.info$Position]
        f<-ap["Fertility",b.info$Position]+ter$Fertility
        m<-ter$Metal
        g<-ap["Gas",b.info$Position]+ter$Gas
        c=ter$Crystals
        .Object@sfmgc=c(s,f,m,g,c)
        
        pop.max<-sum(str[,c("Urban.Structures","Orbital.Base")]*c(.Object@sfmgc[2],10))
        pop<-sum(str[,c(row.names(str.mat[str.mat$Population<0,]),row.names(def.mat))])
        .Object@population=c(pop,pop.max)

        energy.max<-round(sum(initial.energy,
            sum(str[,c("Solar.Plants","Gas.Plants",row.names(str.mat[str.mat$Energy>0,]))]*
            c(.Object@sfmgc[1],.Object@sfmgc[4],str.mat[str.mat$Energy>0,"Energy"]))*
            energy.tech))
        energy<-sum(str[,c(row.names(str.mat[str.mat$Energy<0,]),row.names(def.mat[def.mat$Energy<0,]))]*
            -c(str.mat[str.mat$Energy<0,"Energy"],def.mat[def.mat$Energy<0,"Energy"]))
        .Object@energy=c(energy,energy.max)

        area.max<-sum(area.d,
            sum(str[,row.names(str.mat[str.mat$Area>0,])]*str.mat[str.mat$Area>0,"Area"]))
        area<-sum(str[,c(row.names(str.mat[str.mat$Area<0,]),row.names(def.mat[def.mat$Area<0,]))]*
            -c(str.mat[str.mat$Area<0,"Area"],def.mat[def.mat$Area<0,"Area"]))
        .Object@area=c(area,area.max)

        .Object@economy=sum(str[,row.names(str.mat[str.mat$Economy>0,])]*str.mat[str.mat$Economy>0,"Economy"])

        .Object@construction=round(sum(cons.d,sum(
            str[,c("Metal.Refineries",row.names(str.mat[str.mat$Construction>0,]))]*
            c(.Object@sfmgc[3],str.mat[str.mat$Construction>0,"Construction"])))*
            cyber.tech)

        .Object@production=round(sum(
            str[,c("Metal.Refineries",row.names(str.mat[str.mat$Production>0,]))]*
            c(.Object@sfmgc[3],str.mat[str.mat$Production>0,"Production"]))*
            cyber.tech)

        .Object@research=round(str[,"Research.Labs"]*8*ai.tech)

        return(.Object)
    }
)
new(Class="Base",name="Secundus")
