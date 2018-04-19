
#' Main perceptron algorithm with averaging
#'
#' Main perceptron algorithm with averaging
#'
#' @param y response variable
#' @param x predictors
#' @param E number of epochs
#' @param has_bias if the bias term has been included or not (defaults to FALSE)
#' @return tibble of the input file
#' @export
#' @examples
#' perceptron()
perceptron <- function(y,x,E=100,has_bias=FALSE,seed=Sys.time()){
    local_x <- x
    if( !has_bias ){
        local_x$B <- 1
    }
    
    w <- as.numeric(rep(0,ncol(local_x)))
    
    for( e in 1:E ){
        cat('starting epoch:',e,fill=TRUE)
        e_shuffle <- sample(seq_along(y),length(y))
        ye <- y[ e_shuffle ]
        xe <- local_x[ e_shuffle , ]
        
        for( i in seq_along(ye) ){
            s <- as.numeric(ye[i] * crossprod(w,as.numeric(xe[i,])))
            if( s <= 0 ){
                w <- w + ye[i]*as.numeric(xe[i,])
            }
        }
    }
    
    return(w)
}