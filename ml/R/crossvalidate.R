
#' Generalized Cross Validation
#'
#' Generalized Cross Validation
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
#' crossvalidate()
crossvalidate <- function(xv_data,h,...){
    n_fold <- length(xv_data)
    out <- vector(n_fold,mode='list')
    for( i in 1:n_fold ){
        my_y <- extract(xv_data,'y',i)
        my_x <- extract(xv_data,'x',i)
        out[[i]] <- h(y=my_y,x=my_x,...)
    }
    return(out)
}

extract <- function(xv,element,skip_block){
    subset <- xv[ - skip_block ]
    all_element <- dplyr::bind_rows(lapply(subset,function(x,el=element){ 
        x[[el]] 
    }))
    return(all_element)
}
