#' Display R
#'
#' Displays a capital R.
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


#' Display R
#'
#' Displays a capital R. Same outcome as displayR() but the code
#' has be obfuscated. That isn't to say that the original code
#' is very clear...
#'
#' @export
#' @examples
#' displayRobfuscated()
displayRobfuscated <- function(){
    h=character;r=rep;a=b=h(0);p=options()$width%/%2-5;n="
    ";j=r(toupper(substring(mode(a),4,4)),sum(r(5:9,2)+1)-3)
    k=r(5:9,2);k[4:5]=7;k=cumsum(k+1);j[k]=n;m=paste(h(1), 
                                                     h(1));s=c(0,k[-10])+1;j[c(16:17,24:26,32:33,46:47,53:55,
                                                                             61:64,70:74)]=m;for(i in 1:10)a=c(a,r(m,p),j[s[i]:k[i]])
    cat(c(n,a),sep=b)
}
