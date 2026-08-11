Parametric Tests in R
================
Odia
2026-08-11

# Parametric Tests

Parametric tests make assumptions about the population distribution and
its parameters. This module follows my established pattern: business
scenario, EDA, descriptive statistics, hypotheses, assumptions, manual
calculation, critical-value decision, p-value decision, R verification,
interpretation, recommendation, and lessons learned.

**Important:** We normally say **Fail to Reject H0**, rather than
“accept H0”.

## Module 1: One-Sample t-Test

### Business Scenario

FreshMart reports that average customer spending is 15,000. A random
sample of 25 customers is selected and the population standard deviation
is unknown.

### Load and explore

``` r
marketing_data <- read.csv("one_sample_ttest.csv")
head(marketing_data)
```

    ##   Customer_ID Purchase_Amount
    ## 1        C001           14820
    ## 2        C002           15210
    ## 3        C003           14980
    ## 4        C004           15340
    ## 5        C005           15120
    ## 6        C006           14790

``` r
str(marketing_data)
```

    ## 'data.frame':    25 obs. of  2 variables:
    ##  $ Customer_ID    : chr  "C001" "C002" "C003" "C004" ...
    ##  $ Purchase_Amount: int  14820 15210 14980 15340 15120 14790 15410 15050 14890 15180 ...

``` r
summary(marketing_data)
```

    ##     Customer_ID Purchase_Amount
    ##  Length   :25   Min.   :14790  
    ##  N.unique :25   1st Qu.:14950  
    ##  N.blank  : 0   Median :15090  
    ##  Min.nchar: 4   Mean   :15094  
    ##  Max.nchar: 4   3rd Qu.:15230  
    ##                 Max.   :15450

``` r
nrow(marketing_data)
```

    ## [1] 25

``` r
ncol(marketing_data)
```

    ## [1] 2

### Descriptive statistics

``` r
x <- marketing_data$Purchase_Amount
mean(x)
```

    ## [1] 15094

``` r
median(x)
```

    ## [1] 15090

``` r
sd(x)
```

    ## [1] 189.6049

``` r
var(x)
```

    ## [1] 35950

``` r
min(x)
```

    ## [1] 14790

``` r
max(x)
```

    ## [1] 15450

``` r
quantile(x)
```

    ##    0%   25%   50%   75%  100% 
    ## 14790 14950 15090 15230 15450

### Hypotheses

H0: Average customer spending is 15,000

H0: mu = 15000

H1: mu != 15,000

`NOTE:` This means the population mean is different from 15,000.

A difference can occur in either direction:

mu \< 15,000 OR mu \> 15,000

Therefore, this is a **`two-tailed test`**.

``` r
mu <- 15000
alpha <- 0.05
```

### Manual calculation

``` r
sample_mean <- mean(x)
sample_sd <- sd(x)
sample_size <- length(x)
df <- sample_size - 1

# Calculate Standard Error
standard_error <- sample_sd / sqrt(sample_size)

# Calculate t
t_calculated <- (sample_mean - mu) / standard_error

# Calculate the critical t and p-value using t-statistic functions`(qt(), pt())`

# Divide alpha by 2 and beta = (1 - alpha)  by 2, subtract both to get a collection of both the lower and upper tail band to get the critical t (range).
critical_t <- qt(
  c(alpha / 2, 1 - alpha / 2),
  df = df
)

# Calculate p-value. Multiply by 2 if the test is two tailed and 1 or nothing is the test is one tail
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

### Decisions

``` r
if (t_calculated < critical_t[1] ||
    t_calculated > critical_t[2]) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

``` r
if (p_value < alpha) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

### Verification

``` r
t.test(
  x,
  mu = mu,
  alternative = "two.sided",
  conf.level = 1 - alpha
)
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

### Interpretation

If p-value \< 0.05, there is sufficient evidence that the population
mean differs from 15,000.

### Business Recommendation

Use the statistical result together with practical business
considerations before changing pricing or marketing strategy.

### What I Learned

- `qt()` gives critical t-values.
- `pt()` gives probabilities from the t-distribution.
- The t-test is used when population standard deviation is unknown.
- `alternative` describes the alternative hypothesis.

------------------------------------------------------------------------

## Module 2: Independent Samples t-Test

### Business Scenario

FreshMart wants to know whether customers exposed to a new campaign
spend more than customers exposed to the old campaign.

### Load and explore

``` r
marketing_data_2 <- read.csv("independent_ttest.csv")
head(marketing_data_2)
```

    ##   Customer_ID Campaign Purchase_Amount
    ## 1        C001      Old           14820
    ## 2        C002      Old           15110
    ## 3        C003      Old           14950
    ## 4        C004      Old           15200
    ## 5        C005      Old           15030
    ## 6        C006      Old           14780

``` r
str(marketing_data_2)
```

    ## 'data.frame':    24 obs. of  3 variables:
    ##  $ Customer_ID    : chr  "C001" "C002" "C003" "C004" ...
    ##  $ Campaign       : chr  "Old" "Old" "Old" "Old" ...
    ##  $ Purchase_Amount: int  14820 15110 14950 15200 15030 14780 15190 14980 15020 14890 ...

``` r
summary(marketing_data_2)
```

    ##     Customer_ID      Campaign  Purchase_Amount
    ##  Length   :24   Length   :24   Min.   :14780  
    ##  N.unique :24   N.unique : 2   1st Qu.:15010  
    ##  N.blank  : 0   N.blank  : 0   Median :15315  
    ##  Min.nchar: 4   Min.nchar: 3   Mean   :15289  
    ##  Max.nchar: 4   Max.nchar: 3   3rd Qu.:15545  
    ##                                Max.   :15750

``` r
table(marketing_data_2$Campaign)
```

    ## 
    ## New Old 
    ##  12  12

### Separate the groups

``` r
old <- subset(
  marketing_data_2,
  Campaign == "Old"
)$Purchase_Amount

new <- subset(
  marketing_data_2,
  Campaign == "New"
)$Purchase_Amount
```

### Descriptive statistics

``` r
mean(old)
```

    ## [1] 15006.67

``` r
mean(new)
```

    ## [1] 15571.67

``` r
sd(old)
```

    ## [1] 136.6038

``` r
sd(new)
```

    ## [1] 102.5877

``` r
median(old)
```

    ## [1] 15000

``` r
median(new)
```

    ## [1] 15550

``` r
var(old)
```

    ## [1] 18660.61

``` r
var(new)
```

    ## [1] 10524.24

### Assumptions

1.  SHAPIRO TEST:

The Shapiro-Wilk test checks whether your data is consistent with a
normal distribution.

Think:

`Shapiro = Shape`

You’re asking:

“Does this data approximately follow a normal, bell-shaped
distribution?”

2.  Variance test:

“Do the groups have similar spread?”

The F-test for variances compares the variances of two groups.

`Variance = Variation`

You’re asking:

“Is the amount of variation/spread similar between my two groups?”

``` r
# 
shapiro.test(old)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  old
    ## W = 0.95947, p-value = 0.7763

``` r
shapiro.test(new)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  new
    ## W = 0.94915, p-value = 0.6246

``` r
var.test(old, new)
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  old and new
    ## F = 1.7731, num df = 11, denom df = 11, p-value = 0.3563
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.5104377 6.1592395
    ## sample estimates:
    ## ratio of variances 
    ##           1.773107

If equal variances are not supported, Welch’s t-test is generally
preferred.

### Hypotheses

For the business expectation that the new campaign increases spending:

H0: muNew ≤ muOld

H1: muNew \> muOld

### Manual Welch calculation

``` r
mean_old <- mean(old)
mean_new <- mean(new)

var_old <- var(old)
var_new <- var(new)

n_old <- length(old)
n_new <- length(new)

standard_error <- sqrt(
  var_old / n_old +
  var_new / n_new
)

t_calculated <- (
  mean_new - mean_old
) / standard_error

df <- (
  var_old / n_old +
  var_new / n_new
)^2 /
(
  (var_old / n_old)^2 / (n_old - 1) +
  (var_new / n_new)^2 / (n_new - 1)
)

critical_t <- qt(
  1 - alpha,
  df = df
)

p_value <- pt(
  t_calculated,
  df = df,
  lower.tail = FALSE
)

t_calculated
```

    ## [1] 11.45672

``` r
df
```

    ## [1] 20.41342

``` r
critical_t
```

    ## [1] 1.723025

``` r
p_value
```

    ## [1] 1.199997e-10

### Decisions

``` r
if (t_calculated > critical_t) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

``` r
if (p_value < alpha) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

### Verification

``` r
t.test(
  new,
  old,
  alternative = "greater",
  var.equal = FALSE,
  conf.level = 1 - alpha
)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  new and old
    ## t = 11.457, df = 20.413, p-value = 1.2e-10
    ## alternative hypothesis: true difference in means is greater than 0
    ## 95 percent confidence interval:
    ##  480.0272      Inf
    ## sample estimates:
    ## mean of x mean of y 
    ##  15571.67  15006.67

### Interpretation

A significant result supports the claim that customers exposed to the
new campaign have a higher average purchase amount.

### Business Recommendation

If statistically significant and practically meaningful, management can
consider scaling the new campaign while monitoring its cost and return.

### What I Learned

- Independent t-tests compare two independent means.
- Welch’s t-test does not require equal population variances.
- `alternative = "greater"` corresponds to a directional business
  expectation.

------------------------------------------------------------------------

## Module 3: Paired t-Test

### Business Scenario

The same cashiers are measured before and after a new checkout system.

### Load and explore

``` r
paired_data <- read.csv("paired_cashier_data.csv")
head(paired_data)
```

    ##   Cashier_ID Before After
    ## 1       E001     72    75
    ## 2       E002     68    70
    ## 3       E003     75    79
    ## 4       E004     70    71
    ## 5       E005     73    78
    ## 6       E006     69    72

``` r
str(paired_data)
```

    ## 'data.frame':    12 obs. of  3 variables:
    ##  $ Cashier_ID: chr  "E001" "E002" "E003" "E004" ...
    ##  $ Before    : int  72 68 75 70 73 69 74 71 67 76 ...
    ##  $ After     : int  75 70 79 71 78 72 76 75 69 81 ...

``` r
summary(paired_data)
```

    ##      Cashier_ID     Before          After      
    ##  Length   :12   Min.   :67.00   Min.   :69.00  
    ##  N.unique :12   1st Qu.:69.75   1st Qu.:71.75  
    ##  N.blank  : 0   Median :71.50   Median :74.50  
    ##  Min.nchar: 4   Mean   :71.42   Mean   :74.42  
    ##  Max.nchar: 4   3rd Qu.:73.25   3rd Qu.:76.50  
    ##                 Max.   :76.00   Max.   :81.00

### Difference

``` r
paired_data$Difference <-
  paired_data$After -
  paired_data$Before

mean(paired_data$Difference)
```

    ## [1] 3

``` r
median(paired_data$Difference)
```

    ## [1] 3

``` r
sd(paired_data$Difference)
```

    ## [1] 1.414214

``` r
var(paired_data$Difference)
```

    ## [1] 2

``` r
min(paired_data$Difference)
```

    ## [1] 1

``` r
max(paired_data$Difference)
```

    ## [1] 5

``` r
quantile(paired_data$Difference)
```

    ##   0%  25%  50%  75% 100% 
    ##    1    2    3    4    5

### Normality of differences

``` r
shapiro.test(paired_data$Difference)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  paired_data$Difference
    ## W = 0.91705, p-value = 0.2624

### Hypotheses

H0: muDifference = 0

H1: muDifference \> 0

Zero represents **no average change**.

### Manual calculation

``` r
difference_mean <- mean(paired_data$Difference)
difference_sd <- sd(paired_data$Difference)
n <- length(paired_data$Difference)

standard_error <- difference_sd / sqrt(n)
t_calculated <- difference_mean / standard_error
df <- n - 1

critical_t <- qt(
  1 - alpha,
  df = df
)

p_value <- pt(
  t_calculated,
  df = df,
  lower.tail = FALSE
)

t_calculated
```

    ## [1] 7.348469

``` r
df
```

    ## [1] 11

``` r
critical_t
```

    ## [1] 1.795885

``` r
p_value
```

    ## [1] 7.255373e-06

### Decisions

``` r
if (t_calculated > critical_t) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

``` r
if (p_value < alpha) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

### Verification

``` r
t.test(
  paired_data$After,
  paired_data$Before,
  paired = TRUE,
  alternative = "greater",
  conf.level = 1 - alpha
)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  paired_data$After and paired_data$Before
    ## t = 7.3485, df = 11, p-value = 7.255e-06
    ## alternative hypothesis: true mean difference is greater than 0
    ## 95 percent confidence interval:
    ##  2.266833      Inf
    ## sample estimates:
    ## mean difference 
    ##               3

``` r
t.test(
  paired_data$Difference,
  alternative = "greater",
  conf.level = 1 - alpha
)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  paired_data$Difference
    ## t = 7.3485, df = 11, p-value = 7.255e-06
    ## alternative hypothesis: true mean is greater than 0
    ## 95 percent confidence interval:
    ##  2.266833      Inf
    ## sample estimates:
    ## mean of x 
    ##         3

### Interpretation

A significant result means the average paired difference is greater than
zero.

### Business Recommendation

Use the result to assess whether the intervention produced a measurable
change, together with practical significance.

### What I Learned

- Paired tests are used for matched observations.
- The analysis is performed on the differences.
- `mu = 0` represents no average change.

------------------------------------------------------------------------

## Module 4: One-Sample z-Test

### Business Scenario

FreshMart wants to know whether average customer spending is greater
than 15,000. The population standard deviation is known to be 4,000.

### Load and explore

``` r
marketing_data_4 <- read.csv("one_sample_ztest.csv")
x <- marketing_data_4$Purchase_Amount

head(marketing_data_4)
```

    ##   Customer_ID Purchase_Amount
    ## 1           1           16000
    ## 2           2           17200
    ## 3           3           15500
    ## 4           4           18100
    ## 5           5           14900
    ## 6           6           16700

``` r
str(marketing_data_4)
```

    ## 'data.frame':    40 obs. of  2 variables:
    ##  $ Customer_ID    : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Purchase_Amount: int  16000 17200 15500 18100 14900 16700 15800 19000 15200 17600 ...

``` r
summary(marketing_data_4)
```

    ##   Customer_ID    Purchase_Amount
    ##  Min.   : 1.00   Min.   :14900  
    ##  1st Qu.:10.75   1st Qu.:15800  
    ##  Median :20.50   Median :16650  
    ##  Mean   :20.50   Mean   :16760  
    ##  3rd Qu.:30.25   3rd Qu.:17625  
    ##  Max.   :40.00   Max.   :19100

``` r
mean(x)
```

    ## [1] 16760

``` r
median(x)
```

    ## [1] 16650

``` r
sd(x)
```

    ## [1] 1184.256

``` r
var(x)
```

    ## [1] 1402462

``` r
min(x)
```

    ## [1] 14900

``` r
max(x)
```

    ## [1] 19100

``` r
quantile(x)
```

    ##    0%   25%   50%   75%  100% 
    ## 14900 15800 16650 17625 19100

### Hypotheses

H0: mu = 15,000

H1: mu \> 15,000

``` r
mu <- 15000
sigma <- 4000
alpha <- 0.05
```

### Manual calculation

``` r
sample_mean <- mean(x)
sample_size <- length(x)

standard_error <- sigma / sqrt(sample_size)

z_calculated <- (
  sample_mean - mu
) / standard_error

# We use the standard normal (Z) distribution (qnorm(), pnorm())

# qnorm() calculates critical_z
critical_z <- qnorm(1 - alpha)

# pnorm() calculates p-value
p_value <- pnorm(
  z_calculated,
  lower.tail = FALSE
)

z_calculated
```

    ## [1] 2.782804

``` r
critical_z
```

    ## [1] 1.644854

``` r
p_value
```

    ## [1] 0.002694565

### Decisions

``` r
if (z_calculated > critical_z) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

``` r
if (p_value < alpha) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

### Interpretation

A significant result indicates evidence that the population mean is
greater than 15,000.

### Business Recommendation

Management can investigate whether the increase is sustainable and
whether the factors associated with it should be maintained.

### What I Learned

- The population standard deviation is known in the classical one-sample
  z-test.
- `qnorm()` gives critical z-values.
- `pnorm()` gives probabilities from the standard normal distribution.
- The normal distribution does not use degrees of freedom.

------------------------------------------------------------------------

## Module 5: One-Way ANOVA

### Business Scenario

FreshMart wants to determine whether average customer spending differs
among three or more cities.

### Load and explore

``` r
marketing_data_5 <- read.csv("one_way_anova.csv")

head(marketing_data_5)
```

    ##   Customer_ID  City Purchase_Amount
    ## 1           1 Abuja           12000
    ## 2           2 Abuja           13500
    ## 3           3 Abuja           14200
    ## 4           4 Abuja           12800
    ## 5           5 Abuja           15000
    ## 6           6 Abuja           13100

``` r
str(marketing_data_5)
```

    ## 'data.frame':    200 obs. of  3 variables:
    ##  $ Customer_ID    : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ City           : chr  "Abuja" "Abuja" "Abuja" "Abuja" ...
    ##  $ Purchase_Amount: int  12000 13500 14200 12800 15000 13100 14500 13800 12500 15200 ...

``` r
summary(marketing_data_5)
```

    ##   Customer_ID            City     Purchase_Amount
    ##  Min.   :  1.00   Length   :200   Min.   :12000  
    ##  1st Qu.: 50.75   N.unique :  3   1st Qu.:14100  
    ##  Median :100.50   N.blank  :  0   Median :15100  
    ##  Mean   :100.50   Min.nchar:  4   Mean   :15400  
    ##  3rd Qu.:150.25   Max.nchar:  5   3rd Qu.:16800  
    ##  Max.   :200.00                   Max.   :18900

``` r
table(marketing_data_5$City)
```

    ## 
    ## Abuja  Kano Lagos 
    ##    70    65    65

### Descriptive statistics

``` r
aggregate(
  Purchase_Amount ~ City,
  data = marketing_data_5,
  FUN = mean
)
```

    ##    City Purchase_Amount
    ## 1 Abuja        13860.00
    ## 2  Kano        14960.00
    ## 3 Lagos        17496.92

``` r
aggregate(
  Purchase_Amount ~ City,
  data = marketing_data_5,
  FUN = sd
)
```

    ##    City Purchase_Amount
    ## 1 Abuja        939.2257
    ## 2  Kano        730.1113
    ## 3 Lagos        770.1398

### Assumptions

``` r
by(
  marketing_data_5$Purchase_Amount,
  marketing_data_5$City,
  shapiro.test
)
```

    ## marketing_data_5$City: Abuja
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dd[x, ]
    ## W = 0.95957, p-value = 0.02372
    ## 
    ## ------------------------------------------------------------ 
    ## marketing_data_5$City: Kano
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dd[x, ]
    ## W = 0.96193, p-value = 0.04326
    ## 
    ## ------------------------------------------------------------ 
    ## marketing_data_5$City: Lagos
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  dd[x, ]
    ## W = 0.96766, p-value = 0.08679

``` r
bartlett.test(
  Purchase_Amount ~ City,
  data = marketing_data_5
)
```

    ## 
    ##  Bartlett test of homogeneity of variances
    ## 
    ## data:  Purchase_Amount by City
    ## Bartlett's K-squared = 4.8384, df = 2, p-value = 0.08899

### Hypotheses

H0: All population means are equal.

H1: At least one population mean differs.

### Manual ANOVA

``` r
grand_mean <- mean(
  marketing_data_5$Purchase_Amount
)

group_means <- tapply(
  marketing_data_5$Purchase_Amount,
  marketing_data_5$City,
  mean
)

group_sizes <- table(
  marketing_data_5$City
)

SSB <- sum(
  group_sizes *
  (group_means - grand_mean)^2
)

SSW <- sum(
  ave(
    marketing_data_5$Purchase_Amount,
    marketing_data_5$City,
    FUN = function(x) {
      (x - mean(x))^2
    }
  )
)

SST <- sum(
  (
    marketing_data_5$Purchase_Amount -
    grand_mean
  )^2
)

SSB + SSW
```

    ## [1] 597349950

``` r
SST
```

    ## [1] 597349950

### Degrees of freedom

``` r
k <- length(unique(marketing_data_5$City))
N <- nrow(marketing_data_5)

df_between <- k - 1
df_within <- N - k
df_total <- N - 1

df_between + df_within
```

    ## [1] 199

``` r
df_total
```

    ## [1] 199

### F-statistic

``` r
MSB <- SSB / df_between
MSW <- SSW / df_within

F_calculated <- MSB / MSW

critical_F <- qf(
  1 - alpha,
  df1 = df_between,
  df2 = df_within
)

p_value <- pf(
  F_calculated,
  df1 = df_between,
  df2 = df_within,
  lower.tail = FALSE
)

MSB
```

    ## [1] 232203283

``` r
MSW
```

    ## [1] 674839.5

``` r
F_calculated
```

    ## [1] 344.0867

``` r
critical_F
```

    ## [1] 3.041753

``` r
p_value
```

    ## [1] 5.27999e-65

### Decisions

``` r
if (F_calculated > critical_F) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

``` r
if (p_value < alpha) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

### Verification

``` r
anova_model <- aov(
  Purchase_Amount ~ City,
  data = marketing_data_5
)

summary(anova_model)
```

    ##              Df    Sum Sq   Mean Sq F value Pr(>F)    
    ## City          2 464406565 232203283   344.1 <2e-16 ***
    ## Residuals   197 132943385    674840                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Post hoc test

If ANOVA is significant:

``` r
TukeyHSD(anova_model)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Purchase_Amount ~ City, data = marketing_data_5)
    ## 
    ## $City
    ##                 diff       lwr      upr p adj
    ## Kano-Abuja  1100.000  765.8347 1434.165     0
    ## Lagos-Abuja 3636.923 3302.7578 3971.088     0
    ## Lagos-Kano  2536.923 2196.6258 2877.220     0

### Interpretation

A significant ANOVA result means at least one group mean differs. ANOVA
alone does not identify which groups differ.

### Business Recommendation

Investigate the specific group differences before applying a broad
business strategy.

### What I Learned

- ANOVA compares three or more independent means.
- Total variation is separated into between-group and within-group
  variation.
- The F-statistic is MSB divided by MSW.
- Tukey’s HSD can identify specific differences after a significant
  ANOVA.
