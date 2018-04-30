
#' Main perceptron algorithm
#'
#' Main perceptron algorithm
#'
#' @param y response variable
#' @param x predictors
#' @param r number of epochs
#' @param mu margin for the margin perceptron
#' @param E number of epochs
#' @param dynamic boolean value to turn on dynamic learning rate
#' @param averaged boolean value to turn on averaging
#' @param has_bias if the bias term has been included or not (defaults to FALSE)
#' @param seed some value for setting the seed for random number generation
#' @return tibble of the input file
#' @export
#' @examples
#' perceptron()
perceptron <- function(y,x,params=list(r=1,mu=0,E=10,dynamic=FALSE,averaged=FALSE,has_bias=FALSE,seed=Sys.time())){
    if( is.null(params$has_bias) ) params$has_bias <- FALSE
    
    local_x <- x
    if( !params$has_bias ){
        local_x$B <- 1
    }
    
    y_ <- as.numeric(unlist(y)) #make sure y is of the right type before we start
    
    set.seed(params$seed)
    
    w <- runif(ncol(local_x),min=-0.01,max=0.01)
    a <- vector(length=ncol(local_x),mode='numeric')
    
    mistake_count <- vector(length=params$E,mode='numeric')
    
    for( e in 1:params$E ){
        cat('starting epoch:',e,fill=TRUE)
        e_shuffle <- sample(seq_along(y_),length(y_))
        ye <- y_[ e_shuffle ]
        xe <- local_x[ e_shuffle , ]
        my_r <- params$r
        if( params$dynamic || params$mu > 0 ){
            my_r <- params$r / e
        }
        
        for( i in seq_along(ye) ){
            s <- as.numeric(ye[i] * crossprod(w,as.numeric(xe[i,])))
            if( s < params$mu ){
                w <- w + my_r*ye[i]*as.numeric(xe[i,])
                mistake_count[e] <- mistake_count[e] + 1
            }
            if( params$averaged ){
                a <- a + w
            }
        }
    }
    
    # w_out <- list(w=matrix(w,ncol=1),m=mistake_count)
    w_out <- w
    if( params$averaged ){
        w_out <- a / (params$E * length(y_))
    }
    
    return(w_out)
}


#' Perceptron Fit Calc
#'
#' Perceptron Fit Calc
#'
#' @param x predictors variable
#' @param w weights
#' @return Numeric vector of responses
#' @export
#' @examples
#' perceptron_fit()
perceptron_fit <- function(x,w,has_bias=FALSE){
    lx <- x
    if( !has_bias ){
        lx$b <- 1
    }
    cprod <- as.numeric(as.matrix(lx) %*% w)
    out <- ifelse(cprod < 0,-1,1)
    return(out)
}



