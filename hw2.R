
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
    
    out <- as.tibble(reshape2::dcast(b,X1 + row ~ var_padded,value.var='val',fill=0))
    return(out)
}
train <- read_libsvm('Dataset/phishing.train')
test <- read_libsvm('Dataset/phishing.test')

 