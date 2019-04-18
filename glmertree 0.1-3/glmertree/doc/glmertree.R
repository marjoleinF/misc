### R code from vignette source 'glmertree.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
library("glmertree")
options(width = 70, prompt = "R> ", continue = "+  ")


###################################################
### code chunk number 2: glmertree.Rnw:61-62 (eval = FALSE)
###################################################
## install.packages("glmertree")


###################################################
### code chunk number 3: glmertree.Rnw:67-68 (eval = FALSE)
###################################################
## install.packages("glmertree", repos = "http://R-Forge.R-project.org")


###################################################
### code chunk number 4: glmertree.Rnw:73-74 (eval = FALSE)
###################################################
## library("glmertree")


###################################################
### code chunk number 5: glmertree.Rnw:85-87
###################################################
data("MHserviceDemo", package = "glmertree")
summary(MHserviceDemo)


###################################################
### code chunk number 6: glmertree.Rnw:92-94
###################################################
lmmt <- lmertree(outcome ~ 1 | cluster_id | age + gender + emotional + 
                 autism + impact + conduct, data = MHserviceDemo)


###################################################
### code chunk number 7: glmertree.Rnw:101-103 (eval = FALSE)
###################################################
## outcome ~ 1 | (1 + age | cluster_id) | age + gender + emotional + 
##   autism + impact + conduct


###################################################
### code chunk number 8: glmertree.Rnw:110-114 (eval = FALSE)
###################################################
## MHserviceDemo$outcome_bin <- factor(outcome > 0)
## glmmt <- glmertree(outcome_bin ~ 1 | cluster_id | age + gender + 
##                   emotional + autism + impact + conduct, 
##                 data = MHserviceDemo, family = "binomial")


###################################################
### code chunk number 9: glmertree.Rnw:119-120 (eval = FALSE)
###################################################
## plot(lmmt)


###################################################
### code chunk number 10: glmertree.Rnw:126-127
###################################################
plot(lmmt, which = "tree")


###################################################
### code chunk number 11: glmertree.Rnw:135-136
###################################################
plot(lmmt, which = "ranef")


###################################################
### code chunk number 12: glmertree.Rnw:151-156 (eval = FALSE)
###################################################
## print(lmmt)
## coef(lmmt)
## fixef(lmmt)
## ranef(lmmt)
## VarCorr(lmmt)


###################################################
### code chunk number 13: glmertree.Rnw:161-162
###################################################
predict(lmmt, newdata = MHserviceDemo[1:10,])


###################################################
### code chunk number 14: glmertree.Rnw:167-168
###################################################
predict(lmmt, newdata = MHserviceDemo[1:10, -7], re.form = NA)


###################################################
### code chunk number 15: glmertree.Rnw:176-180 (eval = FALSE)
###################################################
## resids <- residuals(lmmt)
## preds <- predict(lmmt)
## plot(MHserviceDemo$cluster_id, resids)
## scatter.smooth(preds, resids)


###################################################
### code chunk number 16: glmertree.Rnw:186-189
###################################################
resids <- residuals(lmmt)
preds <- predict(lmmt)
plot(MHserviceDemo$cluster_id, resids, xlab = "Cluster", ylab = "Residuals")


###################################################
### code chunk number 17: glmertree.Rnw:191-192
###################################################
scatter.smooth(preds, resids, xlab = "Predicted values", ylab = "Residuals")


###################################################
### code chunk number 18: glmertree.Rnw:206-208
###################################################
data("DepressionDemo", package = "glmertree")
summary(DepressionDemo)


###################################################
### code chunk number 19: glmertree.Rnw:215-217
###################################################
lmm_tree <- lmertree(depression ~ treatment | cluster |
  age + duration + anxiety, data = DepressionDemo)


###################################################
### code chunk number 20: glmertree.Rnw:222-223 (eval = FALSE)
###################################################
## depression ~ treatment | ( 1 + age | cluster) | age + duration + anxiety


###################################################
### code chunk number 21: glmertree.Rnw:230-232
###################################################
glmm_tree <- glmertree(depression_bin ~ treatment | cluster |
  age + duration + anxiety, data = DepressionDemo, family = binomial)


###################################################
### code chunk number 22: glmertree.Rnw:237-238 (eval = FALSE)
###################################################
## plot(lmm_tree)


###################################################
### code chunk number 23: glmertree.Rnw:244-245
###################################################
plot(lmm_tree, which = "tree")


###################################################
### code chunk number 24: glmertree.Rnw:253-254
###################################################
plot(lmm_tree, which = "ranef")


###################################################
### code chunk number 25: glmertree.Rnw:269-274 (eval = FALSE)
###################################################
## print(lmm_tree)
## coef(lmm_tree)
## fixef(lmm_tree)
## ranef(lmm_tree)
## VarCorr(lmm_tree)


###################################################
### code chunk number 26: glmertree.Rnw:279-280
###################################################
predict(lmm_tree, newdata = DepressionDemo[1:7,])


###################################################
### code chunk number 27: glmertree.Rnw:285-286
###################################################
predict(lmm_tree, newdata = DepressionDemo[1:7, -3], re.form = NA)


###################################################
### code chunk number 28: glmertree.Rnw:294-298 (eval = FALSE)
###################################################
## resids <- residuals(lmm_tree)
## preds <- predict(lmm_tree)
## plot(factor(DepressionDemo$cluster), resids)
## scatter.smooth(preds, resids)


###################################################
### code chunk number 29: glmertree.Rnw:304-307
###################################################
resids <- residuals(lmm_tree)
preds <- predict(lmm_tree)
plot(factor(DepressionDemo$cluster), resids, xlab = "Cluster", ylab = "Residuals")


###################################################
### code chunk number 30: glmertree.Rnw:309-310
###################################################
scatter.smooth(preds, resids, xlab = "Predicted values", ylab = "Residuals")


###################################################
### code chunk number 31: glmertree.Rnw:326-329
###################################################
data("GrowthCurveDemo", package = "glmertree")
dim(GrowthCurveDemo)
names(GrowthCurveDemo)


###################################################
### code chunk number 32: glmertree.Rnw:338-341
###################################################
form <- formula(paste0("y ~ time | person | ", 
                       paste0("x", 1:28, collapse = " + ")))
form


###################################################
### code chunk number 33: glmertree.Rnw:348-349
###################################################
gcm_tree <- lmertree(form, cluster = person, data = GrowthCurveDemo)


###################################################
### code chunk number 34: glmertree.Rnw:354-355
###################################################
width(gcm_tree$tree)


###################################################
### code chunk number 35: glmertree.Rnw:360-362
###################################################
gcm_obs_tree <- lmertree(form, data = GrowthCurveDemo)
width(gcm_obs_tree$tree)


###################################################
### code chunk number 36: glmertree.Rnw:367-368 (eval = FALSE)
###################################################
## plot(gcm_tree, which = "tree")


###################################################
### code chunk number 37: glmertree.Rnw:375-376
###################################################
plot(gcm_tree, which = "tree")


###################################################
### code chunk number 38: glmertree.Rnw:387-389
###################################################
varcor <- VarCorr(gcm_tree)
varcor


###################################################
### code chunk number 39: glmertree.Rnw:394-398
###################################################
res_var <- attr(varcor, "sc")^2
int_var <- as.numeric(varcor$person)
ICC <- int_var / (res_var + int_var)
ICC


###################################################
### code chunk number 40: glmertree.Rnw:406-409
###################################################
form_s <- formula(paste0("y ~ time | (1 + time | person) | ", 
                         paste0("x", 1:28, collapse = " + ")))
form_s


###################################################
### code chunk number 41: glmertree.Rnw:414-415
###################################################
gcm_tree_s <- lmertree(form_s, cluster = person, data = GrowthCurveDemo)


###################################################
### code chunk number 42: glmertree.Rnw:420-421
###################################################
VarCorr(gcm_tree_s)


###################################################
### code chunk number 43: appendix1
###################################################
set.seed(123)
treatment <- rbinom(n = 150, size = 1, prob = .5)
duration <- round(rnorm(150, mean = 7, sd = 3))
anxiety <- round(rnorm(150, mean = 10, sd = 3))
age <- round(rnorm(150, mean = 45, sd = 10))
error <- rnorm(150, 0, 2)


###################################################
### code chunk number 44: appendix2
###################################################
cluster <- error + rnorm(150, 0, 6)
rand_int <- sort(rep(rnorm(10, 0, 1), each = 15))
rand_int[order(cluster)] <- rand_int 
error <- error - rand_int
cluster[order(cluster)] <- rep(1:10, each = 15)


###################################################
### code chunk number 45: appendix3
###################################################
node3t1 <- ifelse(duration <= 8 & anxiety <= 10 & treatment == 0, -2, 0)
node3t2 <- ifelse(duration <= 8 & anxiety <= 10 & treatment == 1, 2, 0)
node5t1 <- ifelse(duration > 8 & treatment == 0, 2.5, 0)
node5t2 <- ifelse(duration > 8 & treatment == 1, -2.5, 0)


###################################################
### code chunk number 46: appendix4
###################################################
depression <- round(9 + node3t1 + node3t2 + node5t1 + node5t2 +
  .4 * treatment + error + rand_int)
depression_bin <- factor(as.numeric(depression > 9))


###################################################
### code chunk number 47: appendix5
###################################################
treatment <- factor(treatment, labels = c("Treatment 1", "Treatment 2"))
DepressionDemo <- data.frame(depression, treatment, cluster,
  age, anxiety, duration, depression_bin)


