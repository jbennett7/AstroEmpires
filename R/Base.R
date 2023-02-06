setClass(
    Class = "Base",
    slots = c(
        name = "character",
        home = "logical",
        terrain = "character",
        type = "character",
        position = "numeric",
        resources = "matrix",
        structures = "matrix"
    )
)
