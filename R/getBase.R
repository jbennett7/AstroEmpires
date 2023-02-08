setGeneric("getBase", function(.Object, base_name){standardGeneric("getBase")})
setMethod("getBase",signature("Empire"),function(.Object, base_name){
    bases = rownames(.Object@structures)
    if ( base_name %in% bases ){
        base = new(Class="Base")
        base@name = base_name
        base@terrain = .Object@bases[base_name, "Terrain"]
        base@type = .Object@bases[base_name,"Type"]
        base@position = .Object@bases[base_name, "Position"]
        base@resources = {
            data(Astro.Position)
            data(Terrains)
            base_resources <- Terrains[base@terrain,]
            if (base@type == "Planet") {
                base_resources <- base_resources[-6]
            } else {
                base_resources <- base_resources[-5]
            }
            colnames(base_resources)[5] <- "Area"
            Solar <- Astro.Position[1,base@position]
            base_resources[,"Fertility"] <- base_resources[,"Fertility"] + Astro.Position[2,base@position]
            base_resources[,"Gas"] <- base_resources[,"Gas"] + Astro.Position[3,base@position]
            base_resources <- cbind(Solar,base_resources)[,c(1,3,5,2,4,6)]
            rownames(base_resources) <- "Values"
            t(as.matrix(base_resources))
        }

        base@structures = {
            structs <- matrix(.Object@structures[base_name,],ncol=1)
            rownames(structs) <- colnames(.Object@structures)
            colnames(structs) <- "Levels"
            structs
        }

        base@population = {
            fertility <- base@resources["Fertility",] + base@structures["Biosphere.Modification",]
            max.pop <- sum(base@structures[c("Urban.Structures","Orbital.Base"),] * c(fertility,10))
            data(Structures)
            data(Defenses)
            pop <- sum(base@structures[c(rownames(Structures[Structures$Population<0,]),rownames(Defenses)),])
            c(pop,max.pop)
        }

        base@energy = {
            solar <- base@resources["Solar",]
            gas <- base@resources["Gas",]
            resource.plants <- data.frame(Energy=c(solar,gas))
            rownames(resource.plants) <- c("Solar.Plants","Gas.Plants")
            data(Structures)
            data(Defenses)
            data(Energy)
            energy.tech.bonus <- Energy[Energy$Level==.Object@techLevels["Energy",],"Base.Energy.Output"]
            econsume.structures <- rbind(Structures[Structures$Energy<0,"Energy",drop=FALSE],
                                         Defenses[Defenses$Energy<0,"Energy",drop=FALSE])
            energy.structures <- rbind(resource.plants,
                                       Structures[Structures$Energy>0,"Energy",drop=FALSE])
            c(-sum(econsume.structures$Energy*base@structures[rownames(econsume.structures),]),
              round(sum(2,energy.structures$Energy*base@structures[rownames(energy.structures),])*energy.tech.bonus))
        }

        base@area = {
            start <- base@resources["Area",]
            data(Structures)
            data(Defenses)
            area.structures <- rbind(Structures[Structures$Area>0,"Area",drop=FALSE])
            all.structures <-  rbind(Structures[Structures$Area<0,"Area",drop=FALSE],
                                     Defenses[Defenses$Area<0,"Area",drop=FALSE])
            c(-sum(all.structures$Area*base@structures[rownames(all.structures),]),
              sum(start,area.structures$Area*base@structures[rownames(area.structures),]))
        }

        base@economy = {
            data(Structures)
            crystals <- base@resources["Crystals",]*base@structures["Crystal.Mines",]
            if(base@structures["Capital",]>0){
                eco.mod <- base@structures["Capital",]*10
            } else {
                eco.mod <- sum(.Object@structures[,"Capital"])*2
            }

            eco.structures <- Structures[Structures$Economy>0,"Economy",drop=FALSE]
            sum(crystals,eco.structures$Economy*base@structures[rownames(eco.structures),],eco.mod)
        }

        base@construction = {
            data(Structures)
            data(Cybernetics)
            cybernetics <- Cybernetics[Cybernetics$Level==.Object@techLevels["Cybernetics",],3]
            metal <- base@resources["Metal",]*base@structures["Metal.Refineries",]
            if(.Object@homeBase==base_name){
                home <- 40
            } else {
                home <- 20
            }
            construction.structures <- Structures[Structures$Construction>0,"Construction",drop=FALSE]
            round(sum(home,metal,base@structures[rownames(construction.structures),]*
                                                       construction.structures$Construction)*cybernetics)
        }

        base@production = {
            data(Structures)
            data(Cybernetics)
            cybernetics <- Cybernetics[Cybernetics$Level==.Object@techLevels["Cybernetics",],3]
            metal <- base@resources["Metal",]*base@structures["Metal.Refineries",]
            production.structures <- Structures[Structures$Production>0,"Production",drop=FALSE]
            round(sum(metal,base@structures[rownames(production.structures),]*
                                                       production.structures$Production)*cybernetics)
        }

        base@research = {
            data(Structures)
            data(Artificial.Intelligence)
            ai <- Artificial.Intelligence[Artificial.Intelligence$Level==.Object@techLevels["Artificial.Intelligence",],3]
            round(sum(base@structures["Research.Labs",]*8)*ai)
        }
        return(base)
    } else {
        stop(paste(base_name, "is not a base in destiny server"))
    }
})
