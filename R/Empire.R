setClass(
    "Empire",
    slots = c(
        server = "character",
        homeBase = "character",
        structures = "matrix",
        techLevels = "matrix",
        bases = "list"
    ))
setMethod(
    f = "initialize",
    signature = "Empire",
    definition = function(.Object,
                          server,
                          input.techlevels,
                          input.bases,
                          input.structures
    ) {
        .Object@server = server
        .Object@homeBase = "Primus"
        .Object@techLevels = read.techlevels(input.techlevels)
        .Object@bases = read.bases(input.bases)
        .Object@structures = read.structures(input.structures)

        return(.Object)
    }
)
setGeneric("getBase", function(.Object, base_name){standardGeneric("getBase")})
setMethod("getBase",signature("Empire"),function(.Object, base_name){
    bases = rownames(.Object@structures)
    if ( base_name %in% bases ){
        base <- new(Class="Base")
        base@name = base_name
        base@home = base_name == .Object@homeBase
        base@terrain = .Object@bases[base_name, "Terrain"]
        base@type = .Object@bases[base_name,"Type"]
        base@position = .Object@bases[base_name, "Position"]
        base@resources = {
            data(Astro.Position)
            data(Terrains)
            base_resources = Terrains[base@terrain,]
            if (base@type == "Planet") {
                base_resources = base_resources[-6]
            } else {
                base_resources = base_resources[-5]
            }
            colnames(base_resources)[5] <- "Area"
            Solar = Astro.Position[1,base@position]
            base_resources[,"Fertility"] = base_resources[,"Fertility"] + Astro.Position[2,base@position]
            base_resources[,"Gas"] = base_resources[,"Gas"] + Astro.Position[3,base@position]
            base_resources = cbind(Solar,base_resources)[,c(1,3,5,2,4,6)]
            rownames(base_resources) = "Values"
            t(as.matrix(base_resources))
        }

        base@structures = {
            structs = matrix(.Object@structures[base_name,],ncol=1)
            rownames(structs) = colnames(.Object@structures)
            colnames(structs) = "Levels"
            structs
        }
        return(base)
    } else {
        stop(paste(base_name, "is not a base in destiny server"))
    }
})
