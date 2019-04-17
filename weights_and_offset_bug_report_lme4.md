lmer() creates weights and offset variables in global environment
=================================================================

In a fresh session we're loading `lme4`, the `sleepstudy` data, and add a (nonsensical) column `wght`. After that only the data set is in the global environment:

``` r
library("lme4")
data("sleepstudy", package = "lme4")
sleepstudy$wght <- rep_len(1:2, length.out = nrow(sleepstudy))
ls()
```

    ## [1] "sleepstudy"

After fitting the `lmer()` model, there is somewhat surprisingly also a weights variable in the global environment, which is a copy of the weights generic from the stats package:

``` r
m <- lmer(Reaction ~ Days + (Days | Subject), weights = wght, data = sleepstudy)
ls()
```

    ## [1] "m"          "sleepstudy" "weights"

``` r
weights
```

    ## function (object, ...) 
    ## UseMethod("weights")
    ## <bytecode: 0x0000000007f9ef30>
    ## <environment: namespace:stats>

If weights variable exists in global environment:
-------------------------------------------------

The same does not happen when a weights variable already exists in the global environment. For example we can clean up and create a new variable:

``` r
rm(m, weights)
ls()
```

    ## [1] "sleepstudy"

``` r
weights <- "Hello World!"
```

Then, this remains untouched by `lmer()` fitting:

``` r
m <- lmer(Reaction ~ Days + (Days | Subject), weights = wght,
          data = sleepstudy)
ls()
```

    ## [1] "m"          "sleepstudy" "weights"

``` r
weights
```

    ## [1] "Hello World!"

If weights variable exists in global environment and lmer() is called within a function:
----------------------------------------------------------------------------------------

But when `lmer()` is wrapped inside a function, the weights variable does get replaced:

``` r
lmer.wrapper <- function(formula, weights, data) {
  lmer(formula, weights = weights, data = data)
}
m <- lmer.wrapper(Reaction ~ Days + (Days | Subject), weights = sleepstudy$wght,
          data = sleepstudy)
ls()
```

    ## [1] "lmer.wrapper" "m"            "sleepstudy"   "weights"

``` r
weights
```

    ##   [1] 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1
    ##  [36] 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2
    ##  [71] 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1
    ## [106] 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2
    ## [141] 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1
    ## [176] 2 1 2 1 2

The same behavior occurs in both 1.1-17 (CRAN) and 1.1-18 (GitHub SHA1 40f86be5).

The analogous problem occurs for specifying argument `offset` instead of `weights`.
