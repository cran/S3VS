
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/logo.png" align="right" style="float:right; height:140px;" />

# S3VS: Structured Screen-and-Select Variable Selection in Linear, Generalized Linear, and Survival Models

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/S3VS)](https://CRAN.R-project.org/package=S3VS)
[![R-CMD-check](https://github.com/nilotpalsanyal/S3VS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nilotpalsanyal/S3VS/actions/workflows/R-CMD-check.yaml)
[![CodeFactor](https://www.codefactor.io/repository/github/nilotpalsanyal/s3vs/badge)](https://www.codefactor.io/repository/github/nilotpalsanyal/s3vs)
[![](http://cranlogs.r-pkg.org/badges/grand-total/S3VS)](https://cran.r-project.org/package=S3VS)
<!-- badges: end -->

S3VS performs variable selection using the structured screen-and-select
(S3VS) framework in linear models, generalized linear models with binary
data, and survival models such as the Cox model and accelerated failure
time (AFT) model.

## Installation

### Install from CRAN

``` r
install.packages("S3VS")
```

### Install from GitHub

``` r
# install.packages("devtools")
devtools::install_github("nilotpalsanyal/S3VS")
```

## Description

`S3VS()` is the main and the top-level function of this package.
Depending on the family, `S3VS()` dispatches to `S3VS_LM()`,
`S3VS_GLM()`, or `S3VS_SURV()`, each of which calls the following
functions to perform the whole analysis:

- `get_leadvars*()` for choosing leading variables,
- `get_leadsets()` for construting leading sets,
- `VS_method*()` for within-set variable selection,
- `select_vars()` for aggregating selected variables across sets,
- `remove_vars()` for aggregating not-selected variables across sets,
- `update_y*()` for updating the working response, and
- `looprun()` to check when the algorithm should be terminated.

## Examples

Below, we provide three examples of the usage of `S3VS()` for linear,
generalized linear, and survival models, respectively. For a detailed
manual, see the package
<a href="https://CRAN.R-project.org/package=S3VS"
target="_blank">vignette</a>.

``` r
library(S3VS)
```

### Example 1: Linear model

We simulate a correlated design and a sparse linear signal.

``` r
set.seed(1)

n <- 200
p <- 300

rho <- 0.6
Sigma <- rho ^ abs(outer(1:p, 1:p, "-"))
X_lm <- matrix(rnorm(n * p), n, p) %*% chol(Sigma)
colnames(X_lm) <- paste0("X", seq_len(p))

beta <- rep(0, p)
active <- c(5, 17, 50, 120, 201)
beta[active] <- c(2.0, -1.5, 1.2, 1.8, -2.2)

y_lm <- as.numeric(X_lm %*% beta + rnorm(n, sd = 2))
```

Next, we run un S3VS with LASSO within leading sets

``` r
fit_lm <- S3VS(
  y = y_lm,
  X = X_lm,
  family = "normal",
  method_xy = "percthresh",
  param_xy = list(thresh = 95),
  method_xx = "topk",
  param_xx = list(k = 10),
  vsel_method = "LASSO",
  method_sel = "conservative",
  method_rem = "conservative_begin",
  verbose = FALSE,
  seed = 1
)
#> =================================
#> Number of selected variables: 33
#> Time taken: 0.5 sec
#> =================================
```

The selected variables are

``` r
fit_lm$selected
#>  [1] "X5"   "X4"   "X201" "X200" "X203" "X204" "X205" "X261" "X252" "X120"
#> [11] "X121" "X17"  "X6"   "X50"  "X49"  "X157" "X270" "X271" "X268" "X131"
#> [21] "X72"  "X264" "X263" "X266" "X113" "X22"  "X21"  "X135" "X137" "X296"
#> [31] "X30"  "X278" "X276"
```

Iteration-wise selected variables are

``` r
str(fit_lm$selected_iterwise)
#> List of 36
#>  $ : chr "X5"
#>  $ : chr "X4"
#>  $ : chr "X201"
#>  $ : chr "X200"
#>  $ : chr "X203"
#>  $ : chr "X204"
#>  $ : chr "X205"
#>  $ : chr "X261"
#>  $ : chr "X252"
#>  $ : chr "X120"
#>  $ : chr "X121"
#>  $ : chr "X17"
#>  $ : chr "X6"
#>  $ : chr "X50"
#>  $ : chr "X49"
#>  $ : chr "X157"
#>  $ : chr "X270"
#>  $ : chr "X271"
#>  $ : chr "X268"
#>  $ : chr "X131"
#>  $ : chr "X72"
#>  $ : chr "X264"
#>  $ : chr "X263"
#>  $ : chr "X266"
#>  $ : chr "X113"
#>  $ : chr "X22"
#>  $ : chr "X21"
#>  $ : chr "X135"
#>  $ : chr "X137"
#>  $ : chr "X296"
#>  $ : chr "X30"
#>  $ : chr ""
#>  $ : chr ""
#>  $ : chr "X278"
#>  $ : chr "X276"
#>  $ : chr ""
```

`pred_S3VS()` refits a model on the selected variables using the method
appropriate for the family.

``` r
Xsel <- X_lm[, fit_lm$selected, drop = FALSE]
pred_lm <- pred_S3VS(y = y_lm, X = Xsel, family = "normal", method = "LASSO")

head(pred_lm$y.pred)
#> [1]  2.2593236 -3.8540957 -4.5531521  2.2126372 -7.8408224  0.6618288
```

### Example 2: Binary classification model

We simulate a correlated design and a sparse binary signal.

``` r
set.seed(2)

n <- 250
p <- 400

rho <- 0.4
Sigma <- rho ^ abs(outer(1:p, 1:p, "-"))
X_glm <- matrix(rnorm(n * p), n, p) %*% chol(Sigma)
colnames(X_glm) <- paste0("X", seq_len(p))

beta <- rep(0, p)
active <- c(10, 25, 90, 200)
beta[active] <- c(1.2, -1.0, 0.9, -1.4)

eta <- as.numeric(X_glm %*% beta)
pr  <- 1 / (1 + exp(-eta))
y_glm   <- rbinom(n, 1, pr)
```

We run un S3VS with SCAD within leading sets

``` r
fit_glm <- S3VS(
  y = y_glm,
  X = X_glm,
  family = "binomial",
  method_xy = "topk",
  param_xy = list(k = 2),
  method_xx = "percthresh",
  param_xx = list(thresh = 90),
  vsel_method = "SCAD",
  sel_regout = TRUE,
  rem_regout = TRUE,
  update_y_thresh = 0.5,
  verbose = FALSE,
  seed = 1
)
#> =================================
#> Number of selected variables: 9
#> Time taken: 0.06 sec
#> =================================

fit_glm$selected
#> [1] "X200" "X25"  "X10"  "X277" "X101" "X195" "X90"  "X340" "X116"
```

### Example 3: Survival model

For survival models, `y` must be a list with components:

- `time`: observed times
- `status`: event indicator (1 = event, 0 = censored)

We generate a survival data as follows.

``` r
set.seed(3)

n <- 300
p <- 200
X_surv <- matrix(rnorm(n*p), n, p)
colnames(X_surv) <- paste0("X", seq_len(p))

beta <- rep(0, p)
active <- c(3, 30, 77, 150)
beta[active] <- c(0.6, -0.5, 0.7, -0.8)

linpred <- as.numeric(X_surv %*% beta)
base_rate <- 0.05

time <- rexp(n, rate = base_rate * exp(linpred))
cens <- rexp(n, rate = 0.02)
status <- as.integer(time <= cens)
time <- pmin(time, cens)

y_surv <- list(time = time, status = status)
```

First, we run S3VS assuming a Cox model.

``` r
fit_cox <- S3VS(
  y = y_surv,
  X = X_surv,
  family = "survival",
  surv_model = "COX",
  method_xy = "topk",
  param_xy = list(k = 2),
  method_xx = "percthresh",
  param_xx = list(thresh = 90),
  vsel_method = "LASSO",
  verbose = FALSE,
  seed = 1
)
#> =================================
#> Number of selected variables: 4
#> Time taken: 0.51 sec
#> =================================

fit_cox$selected
#> [1] "X150" "X77"  "X3"   "X30"
```

Next, we run S3VS assuming an AFT model. AFT screening is often more
expensive because adding a candidate can require repeated model fitting.
Start with smaller `k` values.

``` r
fit_aft <- S3VS(
  y = y_surv,
  X = X_surv,
  family = "survival",
  surv_model = "AFT",
  method_xy = "topk",
  param_xy = list(k = 2),
  method_xx = "topk",
  param_xx = list(k = 2),
  vsel_method = "AFTGEE",
  verbose = FALSE,
  seed = 1,
  parallel = FALSE
)
#> =================================
#> Number of selected variables: 8
#> Time taken: 7.09 sec
#> =================================

fit_aft$selected
#> [1] "X150" "X77"  "X3"   "X30"  "X148" "X74"  "X62"  "X116"
```

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->
