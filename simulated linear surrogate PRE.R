library(tidyverse)
library(gtools)
library(mlbench)
library(pre)
library(partykit)
library(glmnet)
library(permute)
library(broom)
library(doParallel)
registerDoParallel(4) # enable parallel lassoing


permutation_do <- function(data, rows_n = 10000){
  cols_n <- ncol(data)
  new_data <- data.frame(matrix(vector(), rows_n, cols_n,dimnames=list(c(), c(colnames(data)))), stringsAsFactors=F)
  for (a in 1:cols_n){
    new_data[a] = sample_n(data[a],rows_n, replace = 1)
  }
  new_data <- unique(new_data) # only keeping uniques might help remove some noise (i.e preferring splits due to repetition), and reduces computation - seems to actually help.
  return(new_data)
} ## same data generator as before

## making Boosting model with continuous data
rules_maker_cont <- function(train, model0, learnrate = 0.01, ntrees = 500, maxdepth=3, cut = 0.5, n = 5000)
{
  generated_data <- permutation_do(train, n)
  value <-  predict(model0, newdata = generated_data)
  value <- value - mean(value)
  generated_data <- cbind(generated_data, value)
  rm(value)
  
  rules <- c()  
  
  # sub-sampler
  z <- sample(1:nrow(generated_data), size =nrow(generated_data)) # shuffle indexes
  indx <- z[1: ((cut)*nrow(generated_data))] # pick 50% of indexes
  
  # get initial eta
  surrogate <- generated_data
  y <- as.matrix(generated_data[10]) # it keeps the training data DV, and never lets it change
  
  eta_0 <- apply(y, 2, weighted.mean, weights = rep(1, nrow(y))) # mean-prediction is the base model
  eta <- (replicate(n = nrow(y), expr = eta_0))
  surrogate[10] <- as.numeric(y - as.numeric(eta))
  
  best_tree <- ctree(value~. , surrogate[indx,], control = ctree_control(maxdepth = maxdepth, alpha =  0.05))
  rules <- c(rules, pre:::list.rules(best_tree)) # that's without a boosting
  
  
  for (a in 2:ntrees){  ## added learn rate adjustments
    # re-sample indexes
    z <- sample(1:nrow(generated_data), size =nrow(generated_data)) # shuffle indexes
    indx <- z[1: ((cut)*nrow(generated_data))] # pick 50% of indexes
    
    ## Update eta and y_learn:
    eta <- eta + (learnrate * predict(best_tree, newdata = surrogate))
    surrogate[10] <- as.numeric(y - eta)
    best_tree <- ctree(value~. , surrogate[indx,], control = ctree_control(maxdepth = maxdepth, alpha =  0.05))
    rules <- c(rules, pre:::list.rules(best_tree))
  }
  return(unique(rules[!rules==""]))
}

## same feature-set maker as before
make_mf <- function(data, rules){
  mf <- data.frame(matrix(NA, nrow = nrow(data), ncol = length(rules)))
  
  for (a in 1:length(rules)){
    name <- rules[a]
    rule <- parse(text=rules[a])
    z = ifelse(eval(rule, envir = data), 1,0)
    colnames(mf)[a] <- name
    mf[[name]] <- z
  }
  return(mf)
}

## actually gets true positives rate and false positives rate.
get_discovery_rates <- function(coefs_o, keep, data){
  keep <- as.character(keep)
  
  coefs <- data.frame(name = coefs_o@Dimnames[[1]], coefficient = coefs_o[1:length(coefs_o)])
  coefs <- coefs[1][coefs[2] !=0]
  coefs <- coefs[2:length(coefs)]
  
  get_rules <- coefs
  a <- c()
  for (naam in colnames(data)){ 
    if(sum(str_detect(get_rules, paste(naam, ""))) > 0) 
      a <- c(a, naam)
  }
  
  TDR <- length(intersect(a, keep)) / length(keep)
  FDR <- length(setdiff(a,keep)) / (ncol(data) - length(keep))
  return(c(TDR, FDR))
}




### data work starts here

data(BreastCancer)
data <- BreastCancer[c(-1,-11)]
data <- as_tibble(sapply(data, as.numeric))
data <- na.omit(data) 


set.seed(1)
B = 200
reg_acc <- numeric(B)
pre0_acc <- numeric(B)
pre1_acc <- numeric(B)
pre2_acc <- numeric(B)
pre3_acc <- numeric(B)

pre0_nrules <- numeric(B)
pre1_nrules <- numeric(B)
pre2_nrules <- numeric(B)
pre3_nrules <- numeric(B)
boost_nrules <- numeric(B)

pre1_fidel <- numeric(B)
pre2_fidel <- numeric(B)
pre3_fidel <- numeric(B)

pre1_fidel_r2 <- numeric(B)
pre2_fidel_r2 <- numeric(B)
pre3_fidel_r2 <- numeric(B)

pre1_FDR <- numeric(B)
pre2_FDR <- numeric(B)
pre3_FDR <- numeric(B)

pre1_TDR <- numeric(B)
pre2_TDR <- numeric(B)
pre3_TDR <- numeric(B)



### Experiments LOOP FROM HERE

## NEEDS VAR IMPORTANCE (not ready yet)

for(i in 1:B){
  #k <- round(runif(1, 2, 6),0) # can randomly select the number of variables
  names <- colnames(data)[shuffle(1:9)][1:3] 
  names <- parse(text = names)
  par1 <- runif(1, 0.5, 2) * ifelse(rbinom(1,1,0.5)==1, 1, -1) 
  par2 <- runif(1, 0.5, 2) * ifelse(rbinom(1,1,0.5)==1, 1, -1) 
  par3 <- runif(1, 0.5, 2) * ifelse(rbinom(1,1,0.5)==1, 1, -1) 
  data <- data%>%
    mutate(y = round(100 + (par1 *eval(names[1])) + par2*eval(names[2]) + par3*eval(names[3]) + rnorm(nrow(data),0,5), 0))
  
  
  cut = 0.5 # how much test set gets
  z <- sample(1:nrow(data), size =nrow(data))
  train_indx <- z[1: ((1-cut)*nrow(data))]
  test_indx <- z[((1-cut)*nrow(data)): nrow(data)]
  train <- data[train_indx, ]
  test <- data[test_indx, ]
  rm(cut, z, train_indx, test_indx)
  
  model0 <- lm(y~., data = train)
  #summary(model0)
  #names
  
  pred0 <- predict(model0, newdata = test)
  acc0 <- mean((pred0 - test$y)^2)
  acc0_r2 = cor(pred0 , test$y)^2

  ### regular PRE
  
  rules <-  rules_maker_cont(train[-10], model0, n = 10000, cut = 0.5)
  z <- pre:::delete_duplicates_complements(data=train, rules=rules)[[1]] # I did it in each model, but not across models
  rules <- unique(z[!z==""])
  
  ### train-based lasso
  mf <- make_mf(train[-10], rules) # better when non-complements are used (same accuracy; much simpler)
  
  y <- as.matrix(predict(model0, newdata = train))
  #y <- train$y
  x <- as.matrix(mf)
  
  cv.lasso <- cv.glmnet(x, y,nfolds = 5, standardize = 0)
  
  ## now to test
  
  mf <- make_mf(test[-10],rules)
  
  newX <- model.matrix(~., data=mf)
  
  pred12 <- predict(cv.lasso, newx = newX[,-1], s = "lambda.1se")
  acc12 = mean((pred12 - test$y)^2)
  coefs12 <- coefficients(cv.lasso, s = "lambda.1se") 
  acc12_r2 = as.numeric(cor(pred12 , test$y)^2)
  nrules12 <- length(coefs12[which(coefs12 != 0 ) ]) - 1 # correct for intercept
  fidelity12 <- mean((pred12 - pred0)^2) 
  fidelity12_r2 <- as.numeric(cor(pred12 , pred0)^2)
  
  discover <- get_discovery_rates(coefs_o = coefs12, keep = names, data =train[-10])
  pre1_TDR[i] <- discover[1]
  pre1_FDR[i] <- discover[2]
  
  #### SURROGATE Lasso
  n = 10000
  generated_data <- permutation_do(train[-10], n)
  y <-  predict(model0, newdata = generated_data)
  
  mf <- make_mf(generated_data, rules)
  
  y <- as.matrix(y)
  mf <- as.matrix(mf)
  cv.lasso <- cv.glmnet(mf, y,nfolds = 3, standardize = 0, parallel = 1, dfmax = nrow(train))


  coefs21 <- coefficients(cv.lasso, s = "lambda.1se")
  
  ### 2nd-lvl Surrogate Lasso
  
  n = 10000
  generated_data <- permutation_do(train[-10], n)
  y <-  predict(model0, newdata = generated_data)
  y <- as.matrix(y)
  
  select_all <- coefs21[1:length(coefs21),]  != 0
  select_all <- coefs21[select_all,]
  select_all <- select_all[-1] # remove intercept
  select_all <- names(select_all)
  
  select_rules <- str_detect(select_all, ">") | str_detect(select_all, "<") | str_detect(select_all, "=")
  select_rules <- select_all[select_rules]
  mf <- as.matrix(make_mf(generated_data, select_rules))

  best_lasso <- cv.glmnet(mf, y, nfolds = 5, standardize = 0, parallel = 1) # standardizing seems to do better? 
  
  # now to test
  
  mf <- make_mf(test, select_rules)
  
  newX <- model.matrix(~.,data=as.data.frame(mf))
  
  pred2 <- predict(best_lasso, newx = newX[,-1], s = "lambda.1se")
  
  acc2 = mean((pred2 - test$y)^2)
  acc2_r2 = as.numeric(cor(pred2 , test$y)^2)
  coefs2 <- coefficients(best_lasso, s="lambda.1se")
  nrules2 <- length(coefs2[which(coefs2 != 0 ) ]) - 1
  fidelity2 <- mean((pred2 - pred0)^2) 
  fidelity2_r2 <- as.numeric(cor(pred2 , pred0)^2)
  
  discover <- get_discovery_rates(coefs2, keep = names, data =data[-10])
  pre2_TDR[i] <- discover[1]
  pre2_FDR[i] <- discover[2]
  
  
  ### 2nd-lvl regular lasso 
  
  select_rules <- as.data.frame(summary(coefs21))[-1,1] - 1 # get rid of intercept...
  
  mf <- make_mf(train, rules)[,select_rules]
  
  
  y <- as.matrix(predict(model0, newdata = train))
  #y <- train$y
  x <- as.matrix(mf)
  
  best_lasso <- cv.glmnet(x, y,nfolds = 5, standardize = 0) # standardizing seems to do better? 
  
  # now to test
  
  mf <- make_mf(test, rules)[,select_rules]
  
  newX <- model.matrix(~.,data=mf)
  pred3 <- predict(best_lasso, newx = newX[,-1], s = "lambda.1se")
  
  acc3 = mean((pred3 - test$y)^2)
  acc3_r2 = as.numeric(cor(pred3 , test$y)^2)
  coefs3 <- coefficients(best_lasso, s="lambda.1se")
  nrules3 <- length(coefs3[which(coefs3 != 0 ) ]) - 1
  fidelity3 <- mean((pred3 - pred0)^2) 
  fidelity3_r2 <- as.numeric(cor(pred3 , pred0)^2)
  
  discover <- get_discovery_rates(coefs3, keep = names, data =data[-10])
  pre3_TDR[i] <- discover[1]
  pre3_FDR[i] <- discover[2]
  
  
  reg_acc[i] <- acc0
  #pre0_acc[i] <- acc1 
  pre1_acc[i] <- acc12 
  pre2_acc[i] <- acc2 
  pre3_acc[i] <- acc3 
  
  pre0_nrules[i] <- length(rules)
  pre1_nrules[i] <- nrules12 
  pre2_nrules[i] <- nrules2 
  pre3_nrules[i] <- nrules3 
  boost_nrules[i] <- length(rules)
  
  pre1_fidel[i] <- fidelity12
  pre2_fidel[i] <- fidelity2
  pre3_fidel[i] <- fidelity3
  
  pre1_fidel_r2[i] <- fidelity12_r2
  pre2_fidel_r2[i] <- fidelity2_r2
  pre3_fidel_r2[i] <- fidelity3_r2
  
  print(c("iteration",i))
}


###  RESULTS from here

indx <- pre1_nrules > 0 & pre3_nrules >0

## discovery rates

mean(pre1_FDR)
mean(pre2_FDR)
mean(pre3_FDR)

mean(pre1_FDR-pre3_FDR)
plot(density(pre1_FDR-pre3_FDR)) # this is roughly normal
t.test(pre1_FDR, pre3_FDR, paired = 1) # highly sign.


mean(pre1_TDR)
mean(pre2_TDR)
mean(pre3_TDR)

mean(pre1_TDR - pre3_TDR)

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

mean(pre3_fidel[indx]/pre2_fidel[indx]) # 3.12-times more than pure surrogate...
mean(pre1_fidel[indx]/pre3_fidel[indx]) # fidelity mse 90% higher!

mean(pre1_fidel_r2[indx])#^0.5)
mean(pre2_fidel_r2[indx])#^0.5)
mean(pre3_fidel_r2[indx])#^0.5)

## 2nd-lvl surrogate has fewr rules, better acc and fidelity

mean(reg_acc[indx])
mean(pre1_acc[indx])
mean(pre2_acc[indx])
mean(pre3_acc[indx])

t.test(pre1_fidel[indx], pre3_fidel[indx], paired = 1) # pre3 sign. better

t.test(pre1_acc[indx], pre3_acc[indx], paired = 1) # pre3 sign. better

t.test(pre1_nrules[indx], pre3_nrules[indx], paired = 1) # pre3 has sign. fewer

t.test(pre3_acc[indx], reg_acc[indx], paired = 1)

## plots

boxplot(pre1_acc[indx] / reg_acc[indx], pre2_acc[indx] / reg_acc[indx], pre3_acc[indx] / reg_acc[indx], notch = TRUE, main="MSE relative to regression", xlab="procedure",
        names=c("Regular lasso", "Surrogate lasso", "2nd-level lasso"))

boxplot(pre1_acc[indx] / reg_acc[indx], pre3_acc[indx] / reg_acc[indx], notch = TRUE, main="MSE relative to regression", xlab="procedure",
        names=c("Regular lasso", "2nd-level lasso"))

boxplot(pre1_fidel[indx] , pre2_fidel[indx] , pre3_fidel[indx] , notch = TRUE, main="Fidelity MSE", xlab="procedure",
        names=c("Regular lasso", "Surrogate lasso", "2nd-level lasso"))



boxplot(reg_acc[indx], pre1_acc[indx], pre2_acc[indx], pre3_acc[indx])

boxplot(pre1_nrules[indx] / boost_nrules[indx], pre2_nrules[indx] / boost_nrules[indx], pre3_nrules[indx] / boost_nrules[indx], notch = TRUE, main="Nrules relative to boosting", xlab="procedure",
        names=c("Regular lasso", "Surrogate lasso", "2nd-level lasso"))

boxplot(pre1_nrules[indx] / boost_nrules[indx], pre3_nrules[indx] / boost_nrules[indx], notch = TRUE, main="Nrules relative to boosting", xlab="procedure",
        names=c("Regular lasso", "2nd-level lasso"))

### do boosting nrules explain differences in fidelity?
cor.test(pre1_fidel[indx]-pre3_fidel[indx], boost_nrules)
plot(boost_nrules, pre3_fidel[indx]/pre3_fidel[indx]) # an unclear relationship. Maybe because pre2 keeps too many rules at very high boosting nrules.



### Does fidelity relate to MSE?
cor(pre1_fidel[indx], pre1_acc[indx]-reg_acc[indx]) # yup
cor.test(pre1_fidel[indx], pre1_acc[indx]-reg_acc[indx])

cor(pre3_fidel[indx], pre3_acc[indx]-reg_acc[indx]) # yup
cor.test(pre3_fidel[indx], pre3_acc[indx]-reg_acc[indx])

cor(pre1_fidel[indx]-pre3_fidel[indx], pre1_acc[indx]-pre3_acc[indx]) # yup. r = 0.84
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

