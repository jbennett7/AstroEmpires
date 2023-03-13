initialize.empire <- function(name="falcon") {
    empire <- new(Class="Empire")
    empire@server <- name
    empire@homeBase <- "Home Planet"
    data(Technologies)
    tech <- rownames(Technologies)
    Level <- rep(0,length(tech))
    empire@techLevels <- data.frame(Level)
    rownames(empire@techLevels) <- tech
    return(empire)
}
