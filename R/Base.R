setClass(
    Class = "Base",
    slots = c(
        name = "character",
        terrain = "character",
        type = "character",
        position = "numeric",
        resources = "matrix",
        structures = "matrix",
        population = "numeric",
        energy = "numeric",
        area = "numeric",
        economy = "numeric",
        construction = "numeric",
        production = "numeric",
        research = "numeric"
    )
)
