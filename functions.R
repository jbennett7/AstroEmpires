structs <- new.env()

structs$construction <- function(name) {
    source('config')
    load(data.file)
    return(Data$Current$Structures[name,
        c('Metal.Refineries',
          'Robotic.Factories',
          'Nanite.Factories',
          'Android.Factories')])
}

structs$production <- function(name) {
    source('config')
    load(data.file)
    return(cbind(structs$construction(name),
                 Data$Current$Structures[name,
                     c('Shipyards','Orbital.Shipyards')]))
}
