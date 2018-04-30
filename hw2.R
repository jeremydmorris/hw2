
hd <- require(devtools)
if( !hd ){
    install.packages('devtools')
}

devtools::load_all('ml')
train <- read_libsvm('Dataset/phishing.train')
test <- read_libsvm('Dataset/phishing.test')
test$F68 <- 0 #need to add this since it is missing from the input 
dev <- read_libsvm('Dataset/phishing.dev')
dev$F68 <- 0 #likewise, missing from this data set

cv <- as.list(paste('Dataset/CVSplits/training0',c(0:4),'.data',sep=''))
cv_data <- lapply(cv,read_libsvm)
cv_data[[1]]$F68 <- 0 #for some reason left out of this also

test_params <- list(r=1,E=10,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1))
test_params <- list(r=1,E=10)
test_run <- perceptron(select(train,X1),select(train,-X1,-row),test_params)

#need to format data for crossvalidation
cv_fmt <- lapply(cv_data,function(x){
    list(y=select(x,X1),x=select(x,-X1,-row))
})

param_test <- list(
    simple1=list(r=1,mu=0,E=20,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    simple2=list(r=0.1,mu=0,E=20,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    simple3=list(r=0.01,mu=0,E=20,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    dynamic1=list(r=1,mu=0,E=20,dynamic=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    dynamic2=list(r=0.1,mu=0,E=20,dynamic=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    dynamic3=list(r=0.01,mu=0,E=20,dynamic=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    margin1=list(r=1,mu=1,E=20,dynamic=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    margin2=list(r=0.1,mu=1,E=20,dynamic=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    margin3=list(r=0.01,mu=1,E=20,dynamic=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    margin4=list(r=1,mu=0.1,E=20,dynamic=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    margin5=list(r=0.1,mu=0.1,E=20,dynamic=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    margin6=list(r=0.01,mu=0.1,E=20,dynamic=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    margin7=list(r=1,mu=0.01,E=20,dynamic=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    margin8=list(r=0.1,mu=0.01,E=20,dynamic=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    margin9=list(r=0.01,mu=0.01,E=20,dynamic=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    avg1=list(r=1,mu=0,E=20,averaged=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    avg2=list(r=0.1,mu=0,E=20,averaged=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1)),
    avg3=list(r=0.01,mu=0,E=20,averaged=TRUE,epoch_test_x=select(dev,-X1,-row),epoch_test_y=select(dev,X1))
)

cv_test <- lapply(param_test,function(x,dta=cv_fmt,f=perceptron){
    crossvalidate(dta,f,x)
})

result_test <- lapply(cv_test,function(x,tdta=test){
    cv_result <- lapply(x,function(y,ttdta=tdta){
        lttdta <- ttdta
        lttdta$fit <- perceptron_fit(select(ttdta,-X1,-row),y$w)
        outt <- lttdta %>% group_by(X1,fit) %>% summarise(n=n()) %>% ungroup() %>% mutate(t=sum(n),p=n/t) %>% filter(X1 == fit) %>% summarise(r=sum(p))
        return(as.numeric(outt[1,1]))
    })
    return(mean(as.numeric(unlist(cv_result))))
}) %>% unlist()


d <- data.frame(y=cv_test[['avg1']][[1]]$ea,x=c(1:20))
ggplot(d) + geom_line(aes(x=x,y=y)) + theme_ipsum() + 
    labs(x='Epoch',y='Dev set accuracy',title='Averaged Perceptron')
