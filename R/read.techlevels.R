#' @title read.techlevels
#' @description generates the structure table.
#' @param input string vector from the technologies page.
#' @return matrix of technology levels
#' @examples
#' # default
#' bases <- read.techlevels(input=stdin())
#'
# The input from the technologies page.
read.techlevels <- function(input) {
    input <- input[!input %in% c("","\t")]
    rownames = gsub(" |-",".",input[seq(3,length(input),3)])
    dat = strsplit(gsub("^\t","",input[seq(5,length(input),3)]),split="\t")
    ret = do.call(rbind,dat)
    ret = as.numeric(ret[,4])
    ret = data.frame(ret)
    rownames(ret) = rownames
    colnames(ret) = "Level"
    return(as.matrix(ret))
}
