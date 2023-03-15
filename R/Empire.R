setClass(
    "Empire",
    slots = c(
        homeBase = "character",
        structures = "data.frame",
        techLevels = "data.frame",
        bases = "list"
    ))
#setMethod(
#    f = "initialize",
#    signature = "Empire",
#    definition = function(.Object,
#                          server,
#                          input.techlevels,
#                          input.bases,
#                          input.structures
#    ) {
#        .Object@server = server
#        .Object@homeBase = "Primus"
#        .Object@techLevels = read.techlevels(input.techlevels)
#        .Object@bases = read.bases(input.bases)
#        .Object@structures = read.structures(input.structures)
#
#        return(.Object)
#    }
#)
