#' @title read.bases
#' @description generates the bases table.
#' @param input string vector from the base page.
#' @return data frame of Terrain Type Position for bases
#' @examples
#' # default
#' bases <- read.bases(input=stdin())
#'
read.bases <- function(input) {
    dat = input[input!=""]
    dat = gsub(" *$","",dat)
    dat = do.call(rbind,strsplit(dat,split="\t"))
    Name = dat[,1]
    Position = as.numeric(gsub(".*:(.).$","\\1",dat[,2]))
    planet = as.numeric(gsub(".*:.(.)$","\\1",dat[,2]))==0
    Terrain = dat[,5]
    Type = {
        tmp = c()
        for(i in 1:length(Terrain)){
            if(Terrain[i] == "Asteriod"){
                tmp = c(tmp,"Asteriod")
            } else if (planet[i]) {
                tmp = c(tmp,"Planet")
            } else {
                tmp = c(tmp,"Moon")
            }
        }
        tmp
    }
    ret = data.frame(Terrain,Type,Position)
    rownames(ret) = Name
    return(ret)
}
