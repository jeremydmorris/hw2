
devtools::load_all('ml')
train <- read_libsvm('Dataset/phishing.train')
test <- read_libsvm('Dataset/phishing.test')
test$F68 <- 0 #need to add this since it is missing from the input 
dev <- read_libsvm('Dataset/phishing.dev')
dev$F68 <- 0 #likewise, missing from this data set

cv <- as.list(paste('Dataset/CVSplits/training0',c(0:4),'.data',sep=''))
cv_data <- lapply(cv,read_libsvm)
cv_data[[1]]$F68 <- 0 #for some reason left out of this also

test_params <- list(r=1,mu=0,E=2,dynamic=FALSE,averaged=FALSE,seed=1234)
test_run <- perceptron(select(train,X1),select(train,-X1,-row),test_params)

#need to format data for crossvalidation
cv_fmt <- lapply(cv_data,function(x){
    list(y=select(x,X1),x=select(x,-X1,-row))
})

param_test <- list(
    simple1=list(r=1,mu=0,E=10,dynamic=FALSE,averaged=FALSE,has_bias=FALSE,seed=1234),
    simple2=list(r=0.1,mu=0,E=10,dynamic=FALSE,averaged=FALSE,has_bias=FALSE,seed=1234),
    simple3=list(r=0.01,mu=0,E=10,dynamic=FALSE,averaged=FALSE,has_bias=FALSE,seed=1234),
    dynamic1=list(r=1,mu=0,E=10,dynamic=TRUE,averaged=FALSE,has_bias=FALSE,seed=1234),
    dynamic2=list(r=0.1,mu=0,E=10,dynamic=TRUE,averaged=FALSE,has_bias=FALSE,seed=1234),
    dynamic3=list(r=0.01,mu=0,E=10,dynamic=TRUE,averaged=FALSE,has_bias=FALSE,seed=1234),
    margin1=list(r=1,mu=1,E=10,dynamic=TRUE,averaged=FALSE,has_bias=FALSE,seed=1234),
    margin2=list(r=0.1,mu=1,E=10,dynamic=TRUE,averaged=FALSE,has_bias=FALSE,seed=1234),
    margin3=list(r=0.01,mu=1,E=10,dynamic=TRUE,averaged=FALSE,has_bias=FALSE,seed=1234),
    margin4=list(r=1,mu=0.1,E=10,dynamic=TRUE,averaged=FALSE,has_bias=FALSE,seed=1234),
    margin5=list(r=0.1,mu=0.1,E=10,dynamic=TRUE,averaged=FALSE,has_bias=FALSE,seed=1234),
    margin6=list(r=0.01,mu=0.1,E=10,dynamic=TRUE,averaged=FALSE,has_bias=FALSE,seed=1234),
    margin7=list(r=1,mu=0.01,E=10,dynamic=TRUE,averaged=FALSE,has_bias=FALSE,seed=1234),
    margin8=list(r=0.1,mu=0.01,E=10,dynamic=TRUE,averaged=FALSE,has_bias=FALSE,seed=1234),
    margin9=list(r=0.01,mu=0.01,E=10,dynamic=TRUE,averaged=FALSE,has_bias=FALSE,seed=1234),
    avg1=list(r=1,mu=0,E=10,dynamic=FALSE,averaged=TRUE,has_bias=FALSE,seed=1234),
    avg2=list(r=0.1,mu=0,E=10,dynamic=FALSE,averaged=TRUE,has_bias=FALSE,seed=1234),
    avg3=list(r=0.01,mu=0,E=10,dynamic=FALSE,averaged=TRUE,has_bias=FALSE,seed=1234)
)

cv_test <- lapply(param_test,function(x,dta=cv_fmt,f=perceptron){
    crossvalidate(dta,f,x)
})

result_test <- lapply(cv_test,function(x,tdta=test){
    cv_result <- lapply(x,function(y,ttdta=tdta){
        lttdta <- ttdta
        lttdta$fit <- perceptron_fit(select(ttdta,-X1,-row),y)
        outt <- lttdta %>% group_by(X1,fit) %>% summarise(n=n()) %>% ungroup() %>% mutate(t=sum(n),p=n/t) %>% filter(X1 == fit) %>% summarise(r=sum(p))
        return(as.numeric(outt[1,1]))
    })
    return(mean(as.numeric(unlist(cv_result))))
}) %>% unlist()

p1_train <- perceptron(y=train$X1,x=select(train,-X1,-row),params=list(r=1,E=10,seed=1234))
p1_train <- perceptron(y=train$X1,x=select(train,-X1,-row))
# p1_train <- perceptron(y=train$X1,x=select(train,-X1,-row),r=1,dynamic=TRUE,E=10,seed=1234)
# p1_train <- perceptron(y=train$X1,x=select(train,-X1,-row),r=1,averaged=TRUE,E=10,seed=1234)
# p1_train <- perceptron(y=train$X1,x=select(train,-X1,-row),r=1,mu=0.01,E=10,seed=1234)

# p01_train <- perceptron(y=train$X1,x=select(train,-X1,-row),r=0.1,E=10,seed=1234)
# p001_train <- perceptron(y=train$X1,x=select(train,-X1,-row),r=0.01,E=10,seed=1234)

train_fit <- train
train_fit$fit1 <- perceptron_fit(select(train,-X1,-row),p1_train)
# train_fit$fit01 <- perceptron_fit(select(train,-X1,-row),p01_train)
train_fit %>% group_by(X1,fit1) %>% summarise(n=n()) %>% ungroup() %>% mutate(t=sum(n),p=n/t) %>% filter(X1 == fit1) %>% summarise(r=sum(p))
# train_fit %>% group_by(X1,fit01) %>% summarise(n=n()) %>% ungroup() %>% mutate(t=sum(n),p=n/t) %>% dcast(X1 ~ fit01,value.var='p')

# test_fit <- test
# test_fit$fit1 <- perceptron_fit(select(test,-X1,-row),p1_train)
# test_fit$fit01 <- perceptron_fit(select(test,-row,-X1),p01_train)
# test_fit %>% group_by(X1,fit1) %>% summarise(n=n()) %>% ungroup() %>% mutate(t=sum(n),p=n/t) %>% filter(X1 == fit1) %>% summarise(r=sum(p))
