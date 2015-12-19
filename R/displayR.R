#' Display R
#'
#' Displays a capital R
#'
#' @export
#' @examples
#' displayR()
displayR <- function(){
    spacing <- getOption("width") %/% 2 - 5
    j <- rep("R", 77)
    jumps <- rep(5:9, 2)
    jumps[4:5] <- 7
    linebreaks <- cumsum(jumps+1)
    j[linebreaks] <- "\n"
    j[c(16:17,24:26,32:33,46:47,53:55,61:64,70:74)]= " "
    start = c(0,linebreaks[-10]) + 1
    end <- linebreaks
    ans <- ""
    for(i in 1:10){
            ans =c(ans, rep(" ", spacing), j[start[i]:end[i]])
    }
    cat("\n")
    cat(ans, sep= "")
}
