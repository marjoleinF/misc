library(tidyverse)
library(randomForest)
library(gtools)
library(mlbench)
library(pre)
library(partykit)
library(glmnet)
library(permute)
library(broom)
library(DescTools)
library(doParallel)
registerDoParallel(4) # enable parallel lassoing

## the data generation function; samples with replacement per each column, and only keeps unique combinations
permutation_do <- function(data, rows_n = 10000){
  cols_n <- ncol(data)
  new_data <- data.frame(matrix(vector(), rows_n, cols_n,dimnames=list(c(), c(colnames(data)))), stringsAsFactors=F)
  for (a in 1:cols_n){
    new_data[a] = sample_n(data[a],rows_n, replace = 1)
  }
  new_data <- unique(new_data) # only keeping uniques might help remove some noise (i.e preferring splits due to repetition), and reduces computation - seems to actually help.
  return(new_data)
}


## Boosting model. Takes X, black-box model, and how many observations should be generated (that's n)

rules_maker_bin <- function(train, model0, learnrate = 0.01, ntrees = 500, maxdepth=3, cut = 0.2, n = 5000)
{
  generated_data <- permutation_do(train, n) # generates data
  value <-  predict(model0, newdata = generated_data) # gets predictions
  generated_data <- cbind(generated_data, value)
  rm(value)
  
  rules <- c()  
  
  # sub-sampler
  z <- sample(1:nrow(generated_data), size =nrow(generated_data)) # shuffle indexes
  indx <- z[1: ((cut)*nrow(generated_data))] # pick 50% of indexes
  
  # get initial eta as per PRE's algorithm
  surrogate <- generated_data
  y <- generated_data$value == levels(generated_data$value)[1]
  
  eta_0 <- pre:::get_intercept_logistic(y, rep(1,nrow(generated_data))) # get null model
  eta <- rep(eta_0, length(y))
  p_0 <- 1 / (1 + exp(-eta))
  
  surrogate[34] <- ifelse(y, log(p_0), log(1 - p_0)) # get residuals based on null model; log(prob) residual
  
  best_tree <- ctree(value~. , surrogate[indx,], control = ctree_control(maxdepth = maxdepth, alpha =  0.05))
  
  rules <- c(rules, pre:::list.rules(best_tree)) # that's for the first iteration
  
  for (a in 2:ntrees){  ## for the next iterations
    # re-sample indexes
    z <- sample(1:nrow(generated_data), size =nrow(generated_data)) # shuffle indexes
    indx <- z[1: ((cut)*nrow(generated_data))] # pick 50% of indexes
    
    ## Update eta and y_learn:
    eta <- eta + (learnrate * predict(best_tree, newdata = surrogate))
    surrogate[34] <- pre:::get_y_learn_logistic(eta, y)
    best_tree <- ctree(value~. , surrogate[indx,], control = ctree_control(maxdepth = maxdepth, alpha =  0.05))
    rules <- c(rules, pre:::list.rules(best_tree))
  }
  return(unique(rules[!rules==""])) # returns all unique rules (complements are dealt-with later)
}

## makes a dataset based on rules
make_mf <- function(data, rules){
  mf <- data.frame(matrix(NA, nrow = nrow(data), ncol = length(rules))) # makes empty dataset
  
  # goes over each rule,
  for (a in 1:length(rules)){
    name <- rules[a]
    rule <- parse(text=name)
    z = ifelse(eval(rule, envir = data), 1,0)
    colnames(mf)[a] <- name
    mf[[name]] <- z
  }
  return(mf)
}






### work with data starts here


data(Ionosphere)
data <- Ionosphere[-2] # all values are 0
data <- na.omit(data)
data[2:33] <- round(data[2:33],4)
data[34] <- factor(data$Class)


set.seed(1)
B = 200
reg_acc <- numeric(B)
pre1_acc <- numeric(B)
pre2_acc <- numeric(B)
pre3_acc <- numeric(B)

pre1_nrules <- numeric(B)
pre2_nrules <- numeric(B)
pre3_nrules <- numeric(B)
boost_nrules <- numeric(B)

pre1_fidel <- numeric(B)
pre2_fidel <- numeric(B)
pre3_fidel <- numeric(B)

pre1_vars <- vector(mode = "list", length = B)
pre2_vars <- vector(mode = "list", length = B)
pre3_vars <- vector(mode = "list", length = B)


### EXPERIMENT LOOP ###

for(i in 1:B){
  cut = 0.5 # how much test set gets
  z <- sample(1:nrow(data), size =nrow(data))
  train_indx <- z[1: ((1-cut)*nrow(data))]
  test_indx <- z[((1-cut)*nrow(data)): nrow(data)]
  train <- data[train_indx, ]
  test <- data[test_indx, ]
  rm(cut, z, train_indx, test_indx)
  
  model0 <- randomForest(Class~., data = train, mtry=5,  ntree= 1000)
  
  pred0 <- predict(model0, newdata = test)
  acc0 <- mean(pred0 == test$Class)

  ### generative PRE starts here
  
  rules <-  rules_maker_bin(train[-34], model0, ntrees = 500, n = 10000, cut = 0.5, learnrate = 0.05, maxdepth=3)
  z <- pre:::delete_duplicates_complements(data=train, rules=rules)[[1]] # I did it in each model, but not across models
  rules <- unique(z[!z==""]) # unique is not really needed, but might as well
  
  ## train-based lasso
  mf <- make_mf(train[-34], rules) # better when non-complements are used (same accuracy; much simpler)
  
  y <- as.matrix(predict(model0, newdata = train))

  xlin <- Winsorize(as.matrix(train[2:33]), probs = c(0.025, 0.975))
  x <- as.matrix(cbind((xlin/sd(xlin)*0.4), mf))
  
  cv.lasso <- cv.glmnet(x, y,nfolds = 5, standardize = 0, family = "binomial")
  
  ## now to test
  
  mf <- make_mf(test[-34],rules)
  xlin1 <- as.matrix(test[2:33])
  x <- as.matrix(cbind((xlin1/sd(xlin)*0.4), mf)) # note that I standardize by previous xlin
  
  newX <- model.matrix(~., data=as_tibble(x))
  
  pred12 <- predict(cv.lasso, newx = newX[,-1], s = "lambda.1se", type = 'class')
  acc12 = mean(pred12 == test$Class)
  
  coefs12 <- coefficients(cv.lasso, s = "lambda.1se")

  nrules12 <- length(coefs12[which(coefs12 != 0 ) ]) - 1 # correct for intercept
  
  fidelity12 <- mean(pred12 == pred0) 
  
  coefs <- data.frame(name = coefs12@Dimnames[[1]], coefficient = coefs12[1:length(coefs12)])
  coefs <- coefs[1][coefs[2] !=0]
  coefs <- coefs[2:length(coefs)]
 
  get_rules <- coefs
  a <- c()
  for (naam in colnames(data)){ 
    if(sum(str_detect(get_rules, paste(naam, ""))) > 0) 
      a <- c(a, naam)
  } 
  vars12 <- unique(a)

  
  #### SURROGATE SELECTOR
  
  ## 1st level is not used for prediction-making.
  n = 10000
  generated_data <- permutation_do(train[-34], n)
  y <-  predict(model0, newdata = generated_data)
  y <- as.matrix(y)
  
  mf <- make_mf(generated_data, rules)
  xlin <- Winsorize(as.matrix(generated_data[2:33]), probs = c(0.025, 0.975))
  x <- as.matrix(cbind((xlin/sd(xlin)*0.4), mf))
  
  cv.lasso <- cv.glmnet(x, y,nfolds = 3, standardize = 0, parallel = 1, dfmax = nrow(train), family = "binomial")

  coefs21 <- coefficients(cv.lasso, s = "lambda.1se")

  ### 2nd-lvl Surrogate Lasso
  
  select_rules <- coefs21[34:length(coefs21),]  != 0
  
  n = 10000
  generated_data <- permutation_do(train[-34], n)
  y <-  predict(model0, newdata = generated_data)
  y <- as.matrix(y)
  
  mf <- make_mf(generated_data, rules)[,select_rules]
  select_lin <- coefs21[2:33,]  != 0
  xlin <- Winsorize(as.matrix(generated_data[select_lin]), probs = c(0.025, 0.975))
  x <- as.matrix(cbind((xlin/sd(xlin)*0.4), mf))
  
  best_lasso <- cv.glmnet(x, y,nfolds = 5, standardize = 0, family = "binomial") # standardizing seems to do better? 
  
  # now to test
  
  mf <- make_mf(test, rules)[,select_rules]
  xlin1 <- test[2:33]
  xlin1 <- xlin1[select_lin]
  x <- as.data.frame(cbind((xlin1/sd(xlin)*0.4), mf))
  
  newX <- model.matrix(~.,data=x)
  
  pred2 <- predict(best_lasso, newx = newX[,-1], s = "lambda.1se", type = 'class')
  
  acc2 = mean(pred2 == test$Class)
  
  coefs2 <- coefficients(best_lasso, s="lambda.1se")
  nrules2 <- length(coefs2[which(coefs2 != 0 ) ]) - 1
  fidelity2 <- mean(pred2 == pred0) 
  
  # which vars are used
  coefs <- data.frame(name = coefs2@Dimnames[[1]], coefficient = coefs2[1:length(coefs2)])
  coefs <- coefs[1][coefs[2] !=0]
  coefs <- coefs[2:length(coefs)]
  
  get_rules <- coefs
  a <- c()
  for (naam in colnames(data)){ 
    if(sum(str_detect(get_rules, paste(naam, ""))) > 0) 
      a <- c(a, naam)
  }
  vars2 <- unique(a)


  ### 2nd-lvl regular lasso
  
  select_all <- coefs21[1:length(coefs21),]  != 0
  select_all <- coefs21[select_all,]
  select_all <- select_all[-1] # remove intercept
  select_all <- names(select_all)
  
  select_rules <- str_detect(select_all, ">") | str_detect(select_all, "<") | str_detect(select_all, "=")
  select_lin <- select_rules == 0
  select_lin <- select_all[select_lin]
  select_rules <- select_all[select_rules]
  mf <- make_mf(train, select_rules)
  
  lins <-  select(train, all_of(select_lin))
  xlin <- Winsorize(as.matrix(lins, probs = c(0.025, 0.975)))
  x <- as.matrix(cbind((xlin/sd(xlin)*0.4), mf))
  
  y <- as.matrix(predict(model0, newdata = train))
  
  best_lasso <- cv.glmnet(x, y,nfolds = 5, standardize = 0, family = "binomial") # standardizing seems to do better? 
  
  # now to test
  
  mf <- make_mf(test, select_rules)
  lins <-  select(test, select_lin)
  xlin <- Winsorize(as.matrix(lins, probs = c(0.025, 0.975)))
  x <- as.data.frame(cbind((xlin/sd(xlin)*0.4), mf))
  
  newX <- model.matrix(~.,data=x)

  pred3 <- predict(best_lasso, newx = newX[,-1], s = "lambda.1se", type = 'class')
  
  acc3 = mean(pred3 == test$Class)

  coefs3 <- coefficients(best_lasso, s="lambda.1se")
  nrules3 <- length(coefs3[which(coefs3 != 0 ) ]) - 1
  fidelity3 <- mean(pred3 == pred0) 

  # which vars are used
  coefs <- data.frame(name = coefs3@Dimnames[[1]], coefficient = coefs3[1:length(coefs3)])
  coefs <- coefs[1][coefs[2] !=0]
  coefs <- coefs[2:length(coefs)]
  
  get_rules <- coefs
  a <- c()
  for (naam in colnames(data)){ 
    if(sum(str_detect(get_rules, paste(naam, ""))) > 0) 
      a <- c(a, naam)
  }
  vars3 <- unique(a)

  
  reg_acc[i] <- acc0
  pre1_acc[i] <- acc12 
  pre2_acc[i] <- acc2 
  pre3_acc[i] <- acc3 
  
  pre1_nrules[i] <- nrules12 
  pre2_nrules[i] <- nrules2 
  pre3_nrules[i] <- nrules3 
  boost_nrules[i] <- length(rules)
  
  pre1_fidel[i] <- fidelity12
  pre2_fidel[i] <- fidelity2
  pre3_fidel[i] <- fidelity3
  
  pre1_vars[[i]] <- vars12
  pre2_vars[[i]] <- vars2
  pre3_vars[[i]] <- vars3
  
  print(c("iteration",i))
}



### RESULTS ###

indx <- pre1_nrules > 0 & pre3_nrules >0 # to avoid NAs

## var selection number & stability

# Jaccard
library(proxy)

jacc1 <- data.frame(matrix(vector(), B, 33, dimnames=list(c(), c(colnames(data)[-34]))), stringsAsFactors=F)

sum(sapply(pre1_vars, length))/B

sum(sapply(pre2_vars, length))/B

sum(sapply(pre3_vars, length))/B

t.test(sapply(pre1_vars, length), sapply(pre3_vars, length) , paired = 1)
t.test(sapply(pre1_vars, length)/ sapply(pre3_vars, length) )

rbind(sapply(jacc1, sum)/B, sapply(jacc3, sum)/B)


# Stability Measure Nogueira

library(stabm)

stabilityNogueira(pre1_vars, 33, impute.na = 1)


stabilityNogueira(pre2_vars, 33, impute.na = 1) # makes sense. The shit keeps changing; the good stuff remains.


stabilityNogueira(pre3_vars, 33, impute.na = 1)

## the rest

mean(boost_nrules[indx])

mean(pre1_nrules[indx]/boost_nrules[indx])
mean(pre2_nrules[indx]/boost_nrules[indx])
mean(pre3_nrules[indx]/boost_nrules[indx])


mean(pre1_nrules[indx])
mean(pre2_nrules[indx])
mean(pre3_nrules[indx])


mean(reg_acc[indx])
mean(pre1_acc[indx])
mean(pre2_acc[indx])
mean(pre3_acc[indx])


mean(pre1_acc[indx]/reg_acc[indx])
mean(pre2_acc[indx]/reg_acc[indx])
mean(pre3_acc[indx]/reg_acc[indx])

mean(pre1_acc[indx]/pre3_acc[indx])



mean(pre1_fidel[indx])
mean(pre2_fidel[indx])
mean(pre3_fidel[indx])


## 2nd-lvl surrogate has fewr rules, better acc and fidelity

t.test(pre1_fidel[indx], pre3_fidel[indx], paired = 1) # 

t.test(pre1_acc[indx], pre3_acc[indx], paired = 1) # 

t.test(pre1_nrules[indx], pre3_nrules[indx], paired = 1) # pre3 has sign. fewer
t.test(pre1_nrules[indx]/ pre3_nrules[indx])

t.test(pre3_acc[indx], reg_acc[indx], paired = 1)

## plots

boxplot(pre1_acc[indx] - reg_acc[indx], pre2_acc[indx] - reg_acc[indx], pre3_acc[indx] - reg_acc[indx], notch = TRUE, main="MSE relative to regression", xlab="procedure",
        names=c("Regular lasso", "Surrogate lasso", "2nd-level lasso"))

boxplot(pre1_acc[indx] / reg_acc[indx], pre3_acc[indx] / reg_acc[indx], notch = TRUE, main="MSE relative to regression", xlab="procedure",
        names=c("Regular lasso", "2nd-level lasso"))

boxplot(pre1_fidel[indx] , pre2_fidel[indx] , pre3_fidel[indx] , notch = TRUE, main="Fidelity MSE", xlab="procedure",
        names=c("Regular lasso", "Surrogate lasso", "2nd-level lasso"))



boxplot(reg_acc[indx], pre1_acc[indx], pre2_acc[indx], pre3_acc[indx])

boxplot(pre1_nrules[indx] / boost_nrules[indx], pre2_nrules[indx] / boost_nrules[indx], pre3_nrules[indx] / boost_nrules[indx], notch = TRUE, main="Nrules relative to boosting", xlab="procedure",
        names=c("Regular lasso", "Surrogate lasso", "2nd-level lasso"))

boxplot(pre1_nrules[indx] / boost_nrules[indx], pre3_nrules[indx] / boost_nrules[indx], notch = TRUE, main="Nrules relative to boosting", xlab="procedure",
        names=c("Regular lasso", "Nested lasso"))


### Does fidelity relate to MSE?

cor(pre1_fidel[indx]-pre3_fidel[indx], pre1_acc[indx]-pre3_acc[indx]) # yup. r = 0.29
plot(pre1_fidel[indx]-pre3_fidel[indx], pre1_acc[indx]-pre3_acc[indx])
cor.test(pre1_fidel[indx]-pre3_fidel[indx], pre1_acc[indx]-pre3_acc[indx])


gg_data <- as_tibble(cbind(pre1_fidel[indx]-pre3_fidel[indx], pre1_acc[indx]-pre3_acc[indx]))
colnames(gg_data) <- c("Regular fidelity minus Nested fidelity", "Regular accuracy minus Nested accuracy")

summary(lm(`Regular accuracy minus Nested accuracy` ~ `Regular fidelity minus Nested fidelity`, data = gg_data))



ggplot(gg_data, aes(x = `Regular fidelity minus Nested fidelity`, y = `Regular accuracy minus Nested accuracy`)) + 
  geom_smooth(se = FALSE)+
  geom_point()+
  ggtitle("Relationship between fidelity differences and accuracy differences (Loess line)")


# does fidelity mediate differences between conditions?

#newmed <- as_tibble(cbind(cbind(pre1_fidel, pre3_fidel), cbind(pre1_acc, pre3_acc)))

id <- sort(rbind(1:sum(indx), 1:sum(indx)))
x <- numeric(2*sum(indx))
x[seq(1,2*sum(indx),2)] <-  1
x[seq(2,2*sum(indx),2)] <-  3

m <- numeric(2*sum(indx))
m[seq(1,2*sum(indx),2)] <-  pre1_fidel[indx]
m[seq(2,2*sum(indx),2)] <-  pre3_fidel[indx]

y <- numeric(2*sum(indx))
y[seq(1,2*sum(indx),2)] <-  pre1_acc[indx]
y[seq(2,2*sum(indx),2)] <-  pre3_acc[indx]

newmed <- as_tibble(cbind(cbind(id, x), cbind(m, y)))


library(lme4)
library(mediation)


# 1st model
model.m <- lmer(m ~ x + (1 | id), data = newmed)
summary(model.m)

# 2nd model
model.y <- lmer(y ~ m+ x + (1 | id), data = newmed)
summary(model.y)

# mediation
multilevel <- mediate(model.m, model.y, treat = "x", control.value = 3, mediator = "m", sims=100)

summary(multilevel) # yup. Factors no longer sign. after controlling for fidelity.

plot(multilevel)

