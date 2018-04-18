#' Read a file in LIBSVM format
#'
#' Read a file in LIBSVM format
#'
#' @param x string representing full or relative path of input file
#' @param prefix string or character value for the predictor columns
#' @return tibble of the input file
#' @examples
#' read_libsvm()
read_libsvm <- function(x,prefix='F'){
    a <- read_delim(x,delim=' ',col_names=FALSE)
    a$row <- seq_along(a$X1)
    
    b <- melt(a,id=c('X1','row'))
    b_values <- stringr::str_split(b$value,':',simplify=TRUE)
    b$var <- b_values[,1]
    b$val <- as.numeric(b_values[,2])
    
    var_length <- max(nchar(b$var))
    
    b$vp <- var_length - nchar(b$var)
    b$p <- rep('0',max(c(1,b$vp)))
    b$var_padded <- paste(prefix,ifelse(b$vp > 0,paste(b$p,b$var,sep=''),b$var),sep='')
    
    # return(b)
    
    out <- tibble::as.tibble(reshape2::dcast(b,X1 + row ~ var_padded,value.var='val',fill=0))
    return(out)
}
