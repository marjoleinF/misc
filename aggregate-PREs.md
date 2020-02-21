Load library **`pre`**:

``` r
library("pre")
```

Prepare original dataset:

``` r
airq <- airquality[complete.cases(airquality), ]
airq$Ozone <- factor(airq$Ozone > median(airq$Ozone))
```

Create a `list` with several different versions of the dataset:

``` r
set.seed(42)
datasets <- list()
for (i in 1:5) datasets[[i]] <- airq[sample(1:nrow(airq), nrow(airq)*.8),]
```

Create a function that fits PREs to several datasets contained in a
list:

``` r
pre.agg <- function(datasets, ...) {
  result <- list()
  for (i in 1:length(datasets)) {
    result[[i]] <- pre(datasets[[i]], ...)
  }
  result
}
set.seed(43)
airq.agg <- pre.agg(datasets, formula = Ozone ~ ., family = "binomial")
```

Create and illustrate `print` and `summary` methods (we can use ellipsis
(`...`) to pass arguments to `print.pre` and `summary.pre`):

``` r
print.agg <- function(object, ...) {
  result <- list()
  sink("NUL")
  for (i in 1:length(object)) {
    result[[i]] <- print(object[[i]], ...)
  }
  sink()
  print(result)
}
print.agg(airq.agg)
```

    ## [[1]]
    ##           rule coefficient description
    ## 62 (Intercept)   1.0659771           1
    ## 1        rule1  -0.7583351  Temp <= 80
    ## 2        rule2  -0.6683016  Temp <= 77
    ## 3        rule3  -0.5956491  Temp <= 82
    ## 
    ## [[2]]
    ##           rule coefficient                description
    ## 76 (Intercept)  1.40983290                          1
    ## 45      rule46 -1.12542864    Wind > 7.4 & Temp <= 82
    ## 5        rule5 -1.02132310                 Temp <= 77
    ## 6        rule6 -0.72477893                 Temp <= 82
    ## 8        rule8  0.42983680  Temp > 78 & Solar.R > 157
    ## 61      rule64  0.33538374 Solar.R > 148 & Month <= 8
    ## 30      rule30  0.17304579   Wind <= 10.3 & Temp > 76
    ## 22      rule22 -0.10733325    Wind > 7.4 & Temp <= 80
    ## 2        rule2 -0.04783606                 Temp <= 78
    ## 67      rule70 -0.04162906      Wind > 8 & Temp <= 76
    ## 
    ## [[3]]
    ##           rule coefficient               description
    ## 72 (Intercept)  0.10999535                         1
    ## 14      rule14  1.06035821 Temp > 78 & Solar.R > 148
    ## 2        rule2 -0.55207831                Temp <= 78
    ## 1        rule1 -0.34416744                Temp <= 82
    ## 3        rule3 -0.16654473                Temp <= 77
    ## 33      rule36  0.15287944 Temp > 77 & Solar.R > 148
    ## 8        rule8 -0.05862083                Temp <= 80
    ## 
    ## [[4]]
    ##           rule coefficient               description
    ## 77 (Intercept)  0.59725073                         1
    ## 58      rule59  0.91439182 Temp > 77 & Solar.R > 148
    ## 41      rule42 -0.72497066     Wind > 8 & Temp <= 80
    ## 6        rule6 -0.59529999                Temp <= 82
    ## 67      rule68 -0.39683663     Wind > 8 & Temp <= 82
    ## 1        rule1 -0.03881522                Temp <= 78
    ## 
    ## [[5]]
    ##           rule  coefficient                  description
    ## 98 (Intercept)  1.359001129                            1
    ## 66      rule67 -0.977975550       Temp <= 82 & Day <= 23
    ## 2        rule2 -0.888961378                   Temp <= 82
    ## 12      rule12  0.664921163    Solar.R > 149 & Temp > 77
    ## 17      rule17 -0.554631889      Temp <= 80 & Wind > 9.2
    ## 47      rule47 -0.446569958        Temp <= 77 & Day > 17
    ## 39      rule39  0.352016861   Solar.R > 175 & Month <= 8
    ## 86      rule88 -0.345194349  Solar.R <= 273 & Wind > 7.4
    ## 1        rule1 -0.239940247                   Temp <= 77
    ## 88      rule90  0.198040668 Wind <= 10.3 & Solar.R > 148
    ## 53      rule53 -0.142824604    Temp <= 77 & Wind <= 11.5
    ## 57      rule57 -0.038575297       Temp <= 80 & Month > 6
    ## 29      rule29  0.010655689    Temp > 78 & Solar.R > 201
    ## 25      rule25 -0.005047176        Temp <= 77 & Day > 13

``` r
summary.agg <- function(object, ...) {
  for (i in 1:length(object)) summary(object[[i]], ...)
}
summary.agg(airq.agg)
```

    ## 
    ## Final ensemble with cv error within 1se of minimum: 
    ##   lambda =  0.07568624
    ##   number of terms = 3
    ##   mean cv error (se) = 0.9909853 (0.06623608)
    ## 
    ##   cv error type : Binomial Deviance
    ## 
    ## 
    ## Final ensemble with cv error within 1se of minimum: 
    ##   lambda =  0.036382
    ##   number of terms = 9
    ##   mean cv error (se) = 0.8142007 (0.09033872)
    ## 
    ##   cv error type : Binomial Deviance
    ## 
    ## 
    ## Final ensemble with cv error within 1se of minimum: 
    ##   lambda =  0.07049046
    ##   number of terms = 6
    ##   mean cv error (se) = 0.9651525 (0.1009328)
    ## 
    ##   cv error type : Binomial Deviance
    ## 
    ## 
    ## Final ensemble with cv error within 1se of minimum: 
    ##   lambda =  0.05940188
    ##   number of terms = 5
    ##   mean cv error (se) = 0.9504962 (0.08500335)
    ## 
    ##   cv error type : Binomial Deviance
    ## 
    ## 
    ## Final ensemble with cv error within 1se of minimum: 
    ##   lambda =  0.03194007
    ##   number of terms = 13
    ##   mean cv error (se) = 0.8168138 (0.09947524)
    ## 
    ##   cv error type : Binomial Deviance

For averaging over predictions, there is only one option for continuous
outcomes. For non-continuous outcomes, we can average over the linear
predictor, or over the predicted values on the scale of the response. I
do not know which would be more appropriate. The resulting predicted
values will be highly correlated. Again, the ellipsis (`...`) can be
used to pass arguments to `predict.pre`:

``` r
predict.agg <- function(object, newdata, ...) {
  rowMeans(sapply(object, predict, newdata = newdata, ...))
}
```

Return predictions on the scale of the linear predictor (default in
`predict.pre`):

``` r
lin_preds <- predict.agg(airq.agg, newdata = airq)
lin_preds
```

    ##           1           2           3           4           7           8 
    ## -0.47879075 -0.97147025 -1.21944223 -1.10856491 -0.95803040 -1.28651898 
    ##           9          12          13          14          15          16 
    ## -1.28651898 -1.13799564 -0.95803040 -1.10957434 -1.28752841 -1.10957434 
    ##          17          18          19          20          21          22 
    ## -1.08100942 -1.37684240 -1.19888833 -1.40540732 -1.40540732 -1.17032341 
    ##          23          24          28          29          30          31 
    ## -1.40540732 -1.18124729 -1.18124729  0.68923689  0.97355492 -0.37351906 
    ##          38          40          41          44          47          48 
    ## -0.25583780  1.69250019  1.69250019 -0.17647048 -1.14172248 -1.08100942 
    ##          49          50          51          62          63          64 
    ## -1.29448095 -1.40540732 -1.40540732  1.76671748  1.69767862  0.56785908 
    ##          66          67          68          69          70          71 
    ##  1.69418297  1.69250019  1.76671748  1.76671748  1.76671748  1.69418297 
    ##          73          74          76          77          78          79 
    ## -1.15675392  0.42110727 -0.73894037  0.94135100  0.63689795  1.76671748 
    ##          80          81          82          85          86          87 
    ##  1.76458635  1.62346132 -0.75391749  1.76671748  1.69767862 -0.06024269 
    ##          88          89          90          91          92          93 
    ##  0.83937258  1.76671748  1.76671748  1.76671748  0.76345419  0.11765412 
    ##          94          95          99         100         101         104 
    ## -0.29044696  0.11765412  1.76671748  1.69767862  1.69767862  1.62133019 
    ##         105         106         108         109         110         111 
    ##  0.49364178 -0.03913620 -1.28087342 -0.05345213 -0.75391749 -0.38276768 
    ##         112         113         114         116         117         118 
    ## -0.30855039 -1.23875153 -1.38455746  0.31496078  1.13694611  1.69767862 
    ##         120         121         122         123         124         125 
    ##  1.69767862  1.76671748  1.76671748  1.76458635  1.62710623  1.62710623 
    ##         126         127         128         129         130         131 
    ##  1.62710623  1.62710623  0.94302061  0.83937258 -0.09233174 -0.44603051 
    ##         132         133         134         135         136         137 
    ## -1.32279896 -1.28319082  0.35616166 -1.29423404 -0.58937677 -1.32380839 
    ##         138         139         140         141         142         143 
    ## -1.32380839  0.20484857 -1.29524347 -1.41312238 -1.37351425  0.50761514 
    ##         144         145         146         147         148         149 
    ## -1.38455746 -1.30219601 -0.25583780 -1.21752727 -1.18896235 -0.51871424 
    ##         151         152         153 
    ## -1.18896235 -0.87391363 -1.21752727

Return predictions on the scale of the response:

``` r
prob_preds <- predict.agg(airq.agg, newdata = airq, type = "response")
prob_preds
```

    ##         1         2         3         4         7         8         9        12 
    ## 0.3877162 0.2851789 0.2309900 0.2489050 0.2818367 0.2209226 0.2209226 0.2435769 
    ##        13        14        15        16        17        18        19        20 
    ## 0.2818367 0.2487154 0.2207862 0.2487154 0.2542605 0.2104713 0.2338699 0.2078445 
    ##        21        22        23        24        28        29        30        31 
    ## 0.2078445 0.2382024 0.2078445 0.2377911 0.2377911 0.6619755 0.7076285 0.4138447 
    ##        38        40        41        44        47        48        49        50 
    ## 0.4384131 0.8333282 0.8333282 0.4580023 0.2429138 0.2542605 0.2198593 0.2078445 
    ##        51        62        63        64        66        67        68        69 
    ## 0.2078445 0.8391008 0.8338747 0.6366888 0.8335685 0.8333282 0.8391008 0.8391008 
    ##        70        71        73        74        76        77        78        79 
    ## 0.8391008 0.8335685 0.2403972 0.6006015 0.3317624 0.7128066 0.6526960 0.8391008 
    ##        80        81        82        85        86        87        88        89 
    ## 0.8389612 0.8271835 0.3282217 0.8391008 0.8338747 0.4848920 0.6907667 0.8391008 
    ##        90        91        92        93        94        95        99       100 
    ## 0.8391008 0.8391008 0.6771176 0.5271226 0.4300309 0.5271226 0.8391008 0.8338747 
    ##       101       104       105       106       108       109       110       111 
    ## 0.8338747 0.8269658 0.6186853 0.4881552 0.2223857 0.4850475 0.3282217 0.4061816 
    ##       112       113       114       116       117       118       120       121 
    ## 0.4243932 0.2288212 0.2097322 0.5739233 0.7473804 0.8338747 0.8338747 0.8391008 
    ##       122       123       124       125       126       127       128       129 
    ## 0.8391008 0.8389612 0.8274370 0.8274370 0.8274370 0.8274370 0.7083077 0.6907667 
    ##       130       131       132       133       134       135       136       137 
    ## 0.4758638 0.3910778 0.2163077 0.2213757 0.5845020 0.2198919 0.3614919 0.2161876 
    ##       138       139       140       141       142       143       144       145 
    ## 0.2161876 0.5478032 0.2197590 0.2071852 0.2107970 0.6199352 0.2097322 0.2188564 
    ##       146       147       148       149       151       152       153 
    ## 0.4384131 0.2313794 0.2363751 0.3778404 0.2363751 0.3043952 0.2313794

The two averaging methods yield highly correlated, but not identical,
predicted probabilities:

``` r
cor(exp(lin_preds) / (1 + exp(lin_preds)), prob_preds)
```

    ## [1] 0.9999314

Create and illustrate `coef` method:

``` r
coef.agg <- function(object, ...) {
  coefs <- coef(object[[1]], ...)
  coefs <- coefs[coefs$coefficient != 0,]
  for (i in 2:length(object)) {
    coefs_tmp <- coef(object[[i]], ...)
    coefs_tmp <- coefs_tmp[coefs_tmp$coefficient != 0,]
    ## Add intercepts:
    coefs[coefs$rule == "(Intercept)", "coefficient"] <- 
      coefs[coefs$rule == "(Intercept)", "coefficient"] + 
      coefs_tmp[coefs_tmp$rule == "(Intercept)", "coefficient"]
    ## Append other terms rest to coefs:
    coefs <- rbind(coefs, coefs_tmp[coefs_tmp$rule!= "(Intercept)", ])
  }
  ## Divide coefficients by the number of datasets:
  coefs$coefficient <- coefs$coefficient / length(object)
  ## Identify identical rules:
  duplicates <- which(duplicated(coefs$description))
  for (i in duplicates) {
    first_match <- which(coefs$description == coefs$description[i])[1]
    ## Add the coefficients:
    coefs$coefficient[first_match] <- 
      coefs$coefficient[first_match] + coefs$coefficient[i]
  }
  ## Remove duplicates:
  coefs <- coefs[-duplicates, ]
  ## Return results:
  coefs
}
coef.agg(airq.agg)
```

    ##            rule  coefficient                  description
    ## 62  (Intercept)  0.908411448                            1
    ## 1         rule1 -0.163391189                   Temp <= 80
    ## 2         rule2 -0.419221939                   Temp <= 77
    ## 3         rule3 -0.629771375                   Temp <= 82
    ## 45       rule46 -0.225085728      Wind > 7.4 & Temp <= 82
    ## 8         rule8  0.085967360    Temp > 78 & Solar.R > 157
    ## 61       rule64  0.067076748   Solar.R > 148 & Month <= 8
    ## 30       rule30  0.034609158     Wind <= 10.3 & Temp > 76
    ## 22       rule22 -0.021466650      Wind > 7.4 & Temp <= 80
    ## 21        rule2 -0.127745917                   Temp <= 78
    ## 67       rule70 -0.008325812        Wind > 8 & Temp <= 76
    ## 14       rule14  0.212071642    Temp > 78 & Solar.R > 148
    ## 33       rule36  0.213454252    Temp > 77 & Solar.R > 148
    ## 41       rule42 -0.144994131        Wind > 8 & Temp <= 80
    ## 671      rule68 -0.079367326        Wind > 8 & Temp <= 82
    ## 66       rule67 -0.195595110       Temp <= 82 & Day <= 23
    ## 121      rule12  0.132984233    Solar.R > 149 & Temp > 77
    ## 17       rule17 -0.110926378      Temp <= 80 & Wind > 9.2
    ## 47       rule47 -0.089313992        Temp <= 77 & Day > 17
    ## 39       rule39  0.070403372   Solar.R > 175 & Month <= 8
    ## 86       rule88 -0.069038870  Solar.R <= 273 & Wind > 7.4
    ## 88       rule90  0.039608134 Wind <= 10.3 & Solar.R > 148
    ## 53       rule53 -0.028564921    Temp <= 77 & Wind <= 11.5
    ## 57       rule57 -0.007715059       Temp <= 80 & Month > 6
    ## 29       rule29  0.002131138    Temp > 78 & Solar.R > 201
    ## 25       rule25 -0.001009435        Temp <= 77 & Day > 13
