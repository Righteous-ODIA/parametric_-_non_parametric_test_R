Hypothesis Tests About Population Means
================
Odia
2026-08-12

# Hypothesis Tests About Population Means

This module organizes the mean tests around the type of population-mean
question.

## 1. One-Sample Population Mean

### Business question

Is the population mean different from a known benchmark?

### Hypotheses

Two-sided:

H0: mu = mu0

H1: mu != mu0

Right-tailed:

H0: mu = mu0

H1: mu \> mu0

Left-tailed:

H0: mu = mu0

H1: mu \< mu0

### One-sample t-test

Use when the population standard deviation is unknown.

``` r
marketing_data <- read.csv("one_sample_ttest.csv")
x <- marketing_data$Purchase_Amount
mu0 <- 15000

t_result <- t.test(
  x,
  mu = mu0,
  alternative = "two.sided",
  conf.level = 1 - alpha
)

t_result
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  x
    ## t = 2.4788, df = 24, p-value = 0.0206
    ## alternative hypothesis: true mean is not equal to 15000
    ## 95 percent confidence interval:
    ##  15015.73 15172.27
    ## sample estimates:
    ## mean of x 
    ##     15094

### Manual calculation

``` r
xbar <- mean(x)
s <- sd(x)
n <- length(x)

se <- s / sqrt(n)
t_calculated <- (xbar - mu0) / se
df <- n - 1

critical_t <- qt(
  c(alpha / 2, 1 - alpha / 2),
  df = df
)

p_value <- 2 * pt(
  abs(t_calculated),
  df = df,
  lower.tail = FALSE
)

t_calculated
```

    ## [1] 2.47884

``` r
critical_t
```

    ## [1] -2.063899  2.063899

``` r
p_value
```

    ## [1] 0.02060047

### Right-tailed version

``` r
t_result_right <- t.test(
  x,
  mu = mu0,
  alternative = "greater",
  conf.level = 1 - alpha
)

t_result_right
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  x
    ## t = 2.4788, df = 24, p-value = 0.0103
    ## alternative hypothesis: true mean is greater than 15000
    ## 95 percent confidence interval:
    ##  15029.12      Inf
    ## sample estimates:
    ## mean of x 
    ##     15094

### Left-tailed version

``` r
t_result_left <- t.test(
  x,
  mu = mu0,
  alternative = "less",
  conf.level = 1 - alpha
)

t_result_left
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  x
    ## t = 2.4788, df = 24, p-value = 0.9897
    ## alternative hypothesis: true mean is less than 15000
    ## 95 percent confidence interval:
    ##      -Inf 15158.88
    ## sample estimates:
    ## mean of x 
    ##     15094

------------------------------------------------------------------------

## 2. One-Sample Population z-Test

Use the classical z-test when the population standard deviation σ is
known.

``` r
sigma <- 4000

se <- sigma / sqrt(n)

z_calculated <- (
  xbar - mu0
) / se

critical_z <- qnorm(
  1 - alpha
)

p_value <- pnorm(
  z_calculated,
  lower.tail = FALSE
)

z_calculated
```

    ## [1] 0.1175

``` r
critical_z
```

    ## [1] 1.644854

``` r
p_value
```

    ## [1] 0.4532319

Two-sided z-test:

``` r
critical_z_two_sided <- qnorm(
  c(alpha / 2, 1 - alpha / 2)
)

p_value_two_sided <- 2 * pnorm(
  abs(z_calculated),
  lower.tail = FALSE
)
```

------------------------------------------------------------------------

## 3. Difference Between Two Independent Population Means

### Business question

Does one independent group’s mean differ from another?

``` r
marketing_data_2 <- read.csv("independent_ttest.csv")

group_a <- subset(
  marketing_data_2,
  Campaign == "Old"
)$Purchase_Amount

group_b <- subset(
  marketing_data_2,
  Campaign == "New"
)$Purchase_Amount
```

Two-sided:

``` r
t.test(
  group_b,
  group_a,
  alternative = "two.sided",
  var.equal = FALSE,
  conf.level = 1 - alpha
)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  group_b and group_a
    ## t = 11.457, df = 20.413, p-value = 2.4e-10
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  462.262 667.738
    ## sample estimates:
    ## mean of x mean of y 
    ##  15571.67  15006.67

Right-tailed business expectation:

``` r
t.test(
  group_b,
  group_a,
  alternative = "greater",
  var.equal = FALSE,
  conf.level = 1 - alpha
)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  group_b and group_a
    ## t = 11.457, df = 20.413, p-value = 1.2e-10
    ## alternative hypothesis: true difference in means is greater than 0
    ## 95 percent confidence interval:
    ##  480.0272      Inf
    ## sample estimates:
    ## mean of x mean of y 
    ##  15571.67  15006.67

------------------------------------------------------------------------

## 4. Difference Between Two Paired Population Means

When the same subjects or matched units are measured twice:

``` r
paired_data <- read.csv("paired_cashier_data.csv")

t.test(
  paired_data$After,
  paired_data$Before,
  paired = TRUE,
  alternative = "two.sided",
  conf.level = 1 - alpha
)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  paired_data$After and paired_data$Before
    ## t = 7.3485, df = 11, p-value = 1.451e-05
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  2.101452 3.898548
    ## sample estimates:
    ## mean difference 
    ##               3

The null difference is usually:

``` r
mu_difference <- 0
```

because zero means no average change.

------------------------------------------------------------------------

## 5. Decision Framework

### Critical-value method

Reject H0 when the test statistic falls in the rejection region.

### p-value method

Reject H0 when:

``` r
p_value < alpha
```

Otherwise:

``` r
"Fail to Reject H0"
```

### `alternative`

``` r
alternative = "two.sided"
```

means H1 is a difference.

``` r
alternative = "greater"
```

means H1 is greater than the benchmark.

``` r
alternative = "less"
```

means H1 is less than the benchmark.

The business question determines H1. We do not automatically choose H1
simply because it is management’s expectation; the claim must be
translated into a statistically appropriate directional or
non-directional hypothesis.

### Confidence level

If:

``` r
alpha <- 0.05
```

then:

``` r
conf.level <- 0.95
```

because:

``` text
1 - alpha = 0.95
```

## What I Learned

- One-sample, independent-samples, and paired tests answer different
  mean questions.
- `qt()`/`pt()` are used with the t-distribution.
- `qnorm()`/`pnorm()` are used with the standard normal distribution.
- The alternative hypothesis determines the test direction.
- A non-significant result means insufficient evidence to reject H0, not
  proof that H0 is true.
