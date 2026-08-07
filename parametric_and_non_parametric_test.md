parametric_and_non_parametric_test
================
Odia
2026-08-04

Introduction to Hypothesis Testing

1.  Parametric Tests

    1.1 One-Sample t-test

    1.2 Independent Samples t-test 1.3 Paired t-test 1.4 One-Sample
    z-test 1.5 One-Way ANOVA

2.  Non-Parametric Tests 2.1 Mann-Whitney U Test 2.2 Wilcoxon
    Signed-Rank Test 2.3 Kruskal-Wallis Test 2.4 Chi-Square Test

3.  Summary and Key Takeaways

## Module 1: One-Sample t-Test in R

**Business Scenario**

FreshMart Supermarket has always reported that the average amount spent
by a customer per visit is ₦15,000.

Recently, management noticed that spending may have changed due to
changes in customer behavior.

Instead of surveying all customers, a random sample of 25 customers was
selected.

As the company’s data analyst, your task is to determine whether there
is enough statistical evidence to conclude that the average purchase
amount is different from ₦15,000.

**Variable of interest**: Purchase_Amount

**Why are we using a One-Sample t-Test?**

A one-sample t-test is appropriate because:

- We have one sample.
- We are comparing the sample mean to a known value (₦15,000).
- The population standard deviation is unknown.
- The sample size is 25, which is less than 30.

### Business Question

Is the average customer purchase amount different from ₦15,000?

The direction isn’t stated, this tells me this is a two-tailed test

### Hypotheses

H0: The population mean purchase amount is ₦15,000.

H1: The population mean purchase amount is not ₦15,000.

**significance level**: 0.05

``` r
alpha_ <- 0.05
```

### Read and store dataset in a variable

``` r
marketing_data <- read.csv("one_sample_ttest.csv")
```

### Exploration Data Analysis on the Dataset

**View the first 5 rows of the dataset using `head()`**

``` r
head(marketing_data)
```

    ##   Customer_ID Purchase_Amount
    ## 1        C001           14820
    ## 2        C002           15210
    ## 3        C003           14980
    ## 4        C004           15340
    ## 5        C005           15120
    ## 6        C006           14790

str

``` r
str(marketing_data)
```

    ## 'data.frame':    25 obs. of  2 variables:
    ##  $ Customer_ID    : chr  "C001" "C002" "C003" "C004" ...
    ##  $ Purchase_Amount: int  14820 15210 14980 15340 15120 14790 15410 15050 14890 15180 ...

**Summary of the dataset**

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

**Number of rows/observation available in the dataset**

``` r
nrow(marketing_data)
```

    ## [1] 25

**Number of columns in the dataset**

``` r
ncol(marketing_data)
```

    ## [1] 2

### Descriptive Analysis

``` r
sample_mean <- mean(marketing_data$Purchase_Amount)

sample_median <- median(marketing_data$Purchase_Amount)

sample_sd <- sd(marketing_data$Purchase_Amount)

sample_variance <- var(marketing_data$Purchase_Amount)

min_sample <- min(marketing_data$Purchase_Amount)

max_sample <- max(marketing_data$Purchase_Amount)

sample_quantiles <- quantile(marketing_data$Purchase_Amount)

cat(paste("Average Purchase Amount =", sample_mean))
```

    ## Average Purchase Amount = 15094

``` r
cat(paste("\nMedian Purchase Amount =", sample_median))
```

    ## 
    ## Median Purchase Amount = 15090

``` r
cat(paste("\nStandard Deviation of Purchase Amount =", sample_sd))
```

    ## 
    ## Standard Deviation of Purchase Amount = 189.604852258585

``` r
cat(paste("\nVariance =", sample_variance))
```

    ## 
    ## Variance = 35950

``` r
cat(paste("\nMinimum Purchase Amount =", min_sample))
```

    ## 
    ## Minimum Purchase Amount = 14790

``` r
cat(paste("\nMaximum Purchase Amount =", max_sample))
```

    ## 
    ## Maximum Purchase Amount = 15450

``` r
sample_quantiles
```

    ##    0%   25%   50%   75%  100% 
    ## 14790 14950 15090 15230 15450

### Step 1: Calculate the Sample Statistics for t-test

Define our values for t-test

``` r
sample_mean <- mean(marketing_data$Purchase_Amount)

# Measures how much individual customer purchases vary.
sample_sd <- sd(marketing_data$Purchase_Amount)

sample_size <- length(marketing_data$Purchase_Amount)

mu <- 15000

alpha_ <- 0.05

degrees_freedom <- sample_size - 1

cat(paste("Sample Mean :", sample_mean))
```

    ## Sample Mean : 15094

``` r
cat(paste("\nStandard Deviation :", sample_sd))
```

    ## 
    ## Standard Deviation : 189.604852258585

``` r
cat(paste("\nSample Size :", sample_size))
```

    ## 
    ## Sample Size : 25

``` r
cat(paste("\nmu :", mu))
```

    ## 
    ## mu : 15000

``` r
cat(paste("\nalpha :", alpha_))
```

    ## 
    ## alpha : 0.05

``` r
cat(paste("\nDegrees of Freedom :", degrees_freedom))
```

    ## 
    ## Degrees of Freedom : 24

Degrees of freedom `(n-1)` represent the number of independent pieces of
information available after estimating the sample mean.

### Calculate the Standard Error

Standard Error (SE)

The standard error estimates the variability of the sample mean. It
tells us how much the sample mean would be expected to change if many
random samples of the same size were collected.

``` r
standard_error <- sample_sd / sqrt(sample_size)
standard_error
```

    ## [1] 37.92097

### Calculate the t-statistic

How many standard errors away is our sample mean from ₦15,000?

``` r
t_calculated <- (sample_mean - mu) / standard_error
t_calculated
```

    ## [1] 2.47884

``` r
cat(
  paste(
    "The sample mean is",
    round(t_calculated,2),
    "standard errors away from the hypothesized mean."
  )
)
```

    ## The sample mean is 2.48 standard errors away from the hypothesized mean.

### Find the Critical Value

These are boundaries of the rejection region

``` r
critical_t <- qt(
  c(0.025, 0.975),
  df = degrees_freedom
)

cat(paste("These are the boundaries of the rejection region :\n"))
```

    ## These are the boundaries of the rejection region :

``` r
critical_t
```

    ## [1] -2.063899  2.063899

### Decision Using the Critical Value

``` r
if(
  t_calculated < critical_t[1] |
  t_calculated > critical_t[2]
){

  print("Reject H0")

}else{

  print("Fail to Reject H0")

}
```

    ## [1] "Reject H0"

### Calculate the p-value

The p-value is the probability of obtaining a test statistic at least as
extreme as the observed one, assuming that the null hypothesis is true.

Because this is a two-tailed test we include `2` in the formula:

``` r
p_value <- 2 * (
  1 - pt(
    abs(t_calculated),
    df = degrees_freedom
  )
)

p_value
```

    ## [1] 0.02060047

### Decision Using the p-value

``` r
if(p_value < alpha_){

  print("Reject H0")

}else{

  print("Fail to Reject H0")

}
```

    ## [1] "Reject H0"

We now have two independent methods:

- Critical value method.
- p-value method.

They must produce the same conclusion.

If they don’t, there’s a mistake somewhere in the calculations.

### Using a T-test function to verify

`t.test()`

``` r
t.test(
  marketing_data$Purchase_Amount,
  mu = 15000,
  alternative = "two.sided",
  conf.level = 0.95
)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  marketing_data$Purchase_Amount
    ## t = 2.4788, df = 24, p-value = 0.0206
    ## alternative hypothesis: true mean is not equal to 15000
    ## 95 percent confidence interval:
    ##  15015.73 15172.27
    ## sample estimates:
    ## mean of x 
    ##     15094

**Interpretation:**

``` r
cat(
  paste(
    "The calculated t-statistic (",
    round(t_calculated, 3),
    ") is compared with the critical values (",
    round(critical_t[1],3),
    " and ",
    round(critical_t[2],3),
    ").",
    sep=""
  )
)
```

    ## The calculated t-statistic (2.479) is compared with the critical values (-2.064 and 2.064).

### Test Summary

| Item                | Result                                        |
|---------------------|-----------------------------------------------|
| Test Used           | One-Sample t-test                             |
| Sample Size         | 25                                            |
| Significance Level  | 0.05                                          |
| Test Statistic      | 2.479                                         |
| Critical Value      | -2.064 and 2.064                              |
| p-value             | 0.0206                                        |
| Decision            | Reject H₀ / Fail to Reject H₀                 |
| Business Conclusion | Average purchase amount differs from ₦15,000. |

### Business Recommendation

FreshMart should investigate the factors influencing customer spending
because the average purchase amount appears to have changed from the
historical benchmark of ₦15,000.

## Module 2: Independent Samples t-Test

**Business Scenario**

FreshMart Supermarket recently tested two marketing campaigns to
increase customer spending.

Old Campaign: SMS discount messages New Campaign: Personalized email
offers with loyalty rewards

To evaluate which campaign performed better, the marketing team randomly
selected 25 customers who received the old campaign and 25 customers who
received the new campaign.

As the company’s data analyst, your task is to determine whether the new
marketing campaign resulted in a higher average purchase amount than the
old campaign.

**Variable of interest**: Purchase_Amount

**Why are we using an Independent Samples t-test t-Test?**

An Independent Samples t-Test is appropriate because:

We have two independent samples. We are comparing the sample mean to a
known value (₦15,000). The population standard deviation is unknown. The
sample size is 25, which is less than 30.

### Business Question

Did customers exposed to the new campaign spend more than customers
exposed to the old campaign?

Notice the wording:

“…spend more…”

That tells us this is a right-tailed test.

### Hypotheses

H0: There is no increase in average spending under the new campaign.

H1: Customers receiving the new campaign spend more on average.

**Significance Level:**

``` r
alpha_
```

    ## [1] 0.05

### Read the dataset

``` r
marketing_data_2 <- read.csv("independent_ttest.csv")
```

### Exploratory Data Analysis

``` r
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
nrow(marketing_data_2)
```

    ## [1] 24

``` r
ncol(marketing_data_2)
```

    ## [1] 3

### Descriptive Statistics

``` r
old_campaign <- subset(
  marketing_data_2,
  Campaign == "Old"
)

new_campaign <- subset(
  marketing_data_2,
  Campaign == "New"
)

old_campaign_mean <- mean(old_campaign$Purchase_Amount)
new_campaign_mean <- mean(new_campaign$Purchase_Amount)

old_campaign_median <- median(old_campaign$Purchase_Amount)
new_campaign_median <- median(new_campaign$Purchase_Amount)

old_campaign_sd <- sd(old_campaign$Purchase_Amount)
new_campaign_sd <- sd(new_campaign$Purchase_Amount)

old_campaign_variance <- var(old_campaign$Purchase_Amount)
new_campaign_variance <- var(new_campaign$Purchase_Amount)

old_campaign_minimum <- min(old_campaign$Purchase_Amount)
new_campaign_minimum <- min(new_campaign$Purchase_Amount)

old_campaign_maximum <- max(old_campaign$Purchase_Amount)
new_campaign_maximum <- max(new_campaign$Purchase_Amount)

cat(paste("Mean of Old Campaign Purchase Amount =", old_campaign_mean))
```

    ## Mean of Old Campaign Purchase Amount = 15006.6666666667

``` r
cat(paste("\nMean of New Campaign Purchase Amount =", new_campaign_mean))
```

    ## 
    ## Mean of New Campaign Purchase Amount = 15571.6666666667

``` r
cat(paste("\nMedian of Old Campaign Purchase Amount =", old_campaign_median))
```

    ## 
    ## Median of Old Campaign Purchase Amount = 15000

``` r
cat(paste("\nMedian of New Campaign Purchase Amount =", new_campaign_median))
```

    ## 
    ## Median of New Campaign Purchase Amount = 15550

``` r
cat(paste("\nStandard Deviation of Old Campaign Purchase Amount =", old_campaign_sd))
```

    ## 
    ## Standard Deviation of Old Campaign Purchase Amount = 136.603828865102

``` r
cat(paste("\nStandard Deviation of New Campaign Purchase Amount =", new_campaign_sd))
```

    ## 
    ## Standard Deviation of New Campaign Purchase Amount = 102.587730378649

``` r
cat(paste("\nVariance of Old Campaign Purchase Amount =", old_campaign_variance))
```

    ## 
    ## Variance of Old Campaign Purchase Amount = 18660.6060606061

``` r
cat(paste("\nVariance of New Campaign Purchase Amount =", new_campaign_variance))
```

    ## 
    ## Variance of New Campaign Purchase Amount = 10524.2424242424

``` r
cat(paste("\nMinimum Old Campaign Purchase Amount =", old_campaign_minimum))
```

    ## 
    ## Minimum Old Campaign Purchase Amount = 14780

``` r
cat(paste("\nMinimum New Campaign Purchase Amount =", new_campaign_maximum))
```

    ## 
    ## Minimum New Campaign Purchase Amount = 15750

``` r
cat(paste("\nMaximum Old Campaign Purchase Amount =", old_campaign_maximum))
```

    ## 
    ## Maximum Old Campaign Purchase Amount = 15200

``` r
cat(paste("\nMaximum New Campaign Purchase Amount =", new_campaign_maximum))
```

    ## 
    ## Maximum New Campaign Purchase Amount = 15750

### Check Assumptions

**Assumption 1: Normality**

``` r
shapiro.test(old_campaign$Purchase_Amount)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  old_campaign$Purchase_Amount
    ## W = 0.95947, p-value = 0.7763

``` r
shapiro.test(new_campaign$Purchase_Amount)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  new_campaign$Purchase_Amount
    ## W = 0.94915, p-value = 0.6246

Interpretation of Assumptions:

p \> 0.05 → no evidence against normality. p \< 0.05 → evidence that the
data are not normally distributed.

**Assumption 2: Equality of Variances**

``` r
var.test(
  old_campaign$Purchase_Amount,
  new_campaign$Purchase_Amount
)
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  old_campaign$Purchase_Amount and new_campaign$Purchase_Amount
    ## F = 1.7731, num df = 11, denom df = 11, p-value = 0.3563
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.5104377 6.1592395
    ## sample estimates:
    ## ratio of variances 
    ##           1.773107

### Required Values/Parameters needed from the t_test formula

first define the values we’ll use throughout the calculation

``` r
sample_size_old <- length(old_campaign$Purchase_Amount)

sample_size_new <- length(new_campaign$Purchase_Amount)

mean_old <- mean(old_campaign$Purchase_Amount)

mean_new <- mean(new_campaign$Purchase_Amount)

variance_old <- var(old_campaign$Purchase_Amount)

variance_new <- var(new_campaign$Purchase_Amount)

alpha <- 0.05

cat("Old Campaign Sample Size :", sample_size_old)
```

    ## Old Campaign Sample Size : 12

``` r
cat("\nNew Campaign Sample Size :", sample_size_new)
```

    ## 
    ## New Campaign Sample Size : 12

``` r
cat("\nOld Campaign Mean :", mean_old)
```

    ## 
    ## Old Campaign Mean : 15006.67

``` r
cat("\nNew Campaign Mean :", mean_new)
```

    ## 
    ## New Campaign Mean : 15571.67

``` r
cat("\nOld Campaign Variance :", variance_old)
```

    ## 
    ## Old Campaign Variance : 18660.61

``` r
cat("\nNew Campaign Variance :", variance_new)
```

    ## 
    ## New Campaign Variance : 10524.24

### Calculate the Pooled Variance

Since our F-test showed that the variances are equal, we combine them
into a single estimate called the pooled variance.

``` r
pooled_variance <-
(
  (sample_size_old - 1) * variance_old +
  (sample_size_new - 1) * variance_new
) /
(
  sample_size_old + sample_size_new - 2
)

pooled_variance
```

    ## [1] 14592.42

### Calculate the Pooled Standard Deviation

The pooled standard deviation is simply the square root of the pooled
variance.

``` r
pooled_sd <- sqrt(pooled_variance)

pooled_sd
```

    ## [1] 120.7991

### Calculate the Standard Error

Now we have two samples, so the standard error becomes calculated
differently.

``` r
standard_error <-
pooled_sd *
sqrt(
  (1 / sample_size_old) +
  (1 / sample_size_new)
)

standard_error
```

    ## [1] 49.31603

**Interpretation**

This standard error measures how much the difference between the two
sample means would vary if we repeatedly took random samples from the
same populations.

### Calculate the t-statistic

Now compare the two sample means.

Since our alternative hypothesis is:

H1:μNew \>μOld

``` r
t_calculated <-
(mean_new - mean_old) /
standard_error

t_calculated
```

    ## [1] 11.45672

**Interpretation**

The t-statistic tells us how many standard errors apart the two sample
means are.

The larger the positive value, the stronger the evidence that the new
campaign produced higher spending.

### Degrees of Freedom

For the pooled independent samples t-test:

``` r
degrees_freedom <-
sample_size_old +
sample_size_new - 2

degrees_freedom
```

    ## [1] 22

### Critical Value

Because this is a right-tailed test, we only need one critical value.

``` r
critical_t <-
qt(
  0.95,
  df = degrees_freedom
)

critical_t
```

    ## [1] 1.717144

Notice this is different from Module 1.

In Module 1 we used:

qt(c(0.025, 0.975), df)

because it was a two-tailed test.

Now we use:

qt(0.95, df)

because the rejection region is only in the right tail.

**A note on the formulas**

You might notice that the formulas are becoming more complex. Rather
than memorizing them, try to understand what each one is measuring:

Pooled variance combines the variability from both groups. Pooled
standard deviation is the common estimate of spread. Standard error
measures the expected variability in the difference between the two
sample means. t-statistic expresses that observed difference in units of
standard error.

If you understand those ideas, the formulas become much easier to
remember.

### Decision Using the Critical Value

``` r
if (t_calculated > critical_t) {

  print("Reject H0")

} else {

  print("Fail to Reject H0")

}
```

    ## [1] "Reject H0"

### Calculate the p-value

This is different from module 1, module 1 uses two tail. Module 2 is
right tailed, so only the right tail matters

``` r
p_value <- pt(
  t_calculated,
  df = degrees_freedom,
  lower.tail = FALSE
)

p_value
```

    ## [1] 4.813608e-11

### Decision Using the p-value

``` r
if (p_value < alpha) {

  print("Reject H0")

} else {

  print("Fail to Reject H0")

}
```

    ## [1] "Reject H0"

This should agree with the critical value method.

If it doesn’t, recheck your calculations.

### Verify Using t.test()

``` r
t.test(
  Purchase_Amount ~ Campaign,
  data = marketing_data_2,
  alternative = "greater",
  var.equal = TRUE
)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  Purchase_Amount by Campaign
    ## t = 11.457, df = 22, p-value = 4.814e-11
    ## alternative hypothesis: true difference in means between group New and group Old is greater than 0
    ## 95 percent confidence interval:
    ##  480.3173      Inf
    ## sample estimates:
    ## mean in group New mean in group Old 
    ##          15571.67          15006.67

`numerical_column ~ category_column`

This tells R:

Compare the numeric variable across the groups.

### Statistical Interpretation:

The calculated t-statistic is greater than the critical t-value, and the
p-value is less than the 5% significance level. Therefore, we reject the
null hypothesis. There is sufficient statistical evidence to conclude
that customers exposed to the new marketing campaign spent more on
average than those exposed to the old campaign.

### Business Recommendation:

The analysis indicates that the new marketing campaign is associated
with higher average customer spending. FreshMart Supermarket should
consider adopting the new campaign more broadly, while continuing to
monitor customer response and campaign costs to ensure the increase in
spending translates into higher overall profitability.

### End-of-Module Summary

| Item | Result |
|----|----|
| Test Used | Independent Samples t-test |
| Sample Size | 12 (Old), 12 (New) |
| Alternative Hypothesis | μ<sub>New</sub> \> μ<sub>Old</sub> |
| Equal Variances | Yes |
| Test Statistic | 11.457 |
| Degrees of Freedom | 22 |
| Critical Value | 1.717 |
| p-value | 4.814^{-11} |
| Decision | Reject H₀ / Fail to Reject H₀ |
| Business Conclusion | The new campaign resulted in a significantly higher average purchase amount. |

### What I Learned

- When to use an independent samples t-test.
- Why we check normality and equal variances.
- The difference between pooled variance and pooled standard deviation.
- Why a one-tailed test uses only one critical value.
- How to verify manual calculations with t.test().

## Module 3: Paired Samples t-Test in R

This is where I initially got confused because I see two sets of numbers
and assume it’s another independent samples t-test.

But It isn’t.

The key difference is **the observations are related.**

### Business Scenario

FreshMart Supermarket introduced a customer service training program for
its cashiers.

Management wants to know whether the training improved customer
satisfaction.

Instead of comparing two different groups of employees, the same 12
cashiers will be evaluated:

- Before the training.
- After the training.

Since each cashier has two scores, the observations are naturally
paired.

### Task

As the company’s data analyst, your task is to determine whether the
training significantly improved customer satisfaction scores.

### Variable of Interest

Customer Satisfaction Score

### Why are we using a Paired Samples t-Test?

A paired samples t-test is appropriate because:

- We have two measurements for the same individual.
- The observations are dependent (paired).
- We want to determine whether the average difference between the two
  measurements is significantly different from zero.
- The population standard deviation is unknown.
- The sample size is 12 paired observations, following our classroom
  convention for a t-test.

### Business Question

Did the customer service training improve cashier satisfaction scores?

The word improve tells us this is a one-tailed (right-tailed) test.

### Hypotheses

Let’s define the difference as:

Difference = After − Before

Then:

Null Hypothesis (H0): The training had no effect on the average
satisfaction score.

Alternative Hypothesis (H1): The average satisfaction score increased
after the training.

### Significance Level

``` r
alpha_ <- 0.05
```

### View Dataset using head()

### What Makes This Test Different?

Look at one row:

``` r
paired_data <- read.csv("paired_ttest_v1.csv")
head(paired_data)
```

    ##   Cashier_ID Before After
    ## 1       E001     72    75
    ## 2       E002     68    70
    ## 3       E003     75    79
    ## 4       E004     70    71
    ## 5       E005     73    78
    ## 6       E006     69    72

Those two numbers belong to the same cashier.

We should never separate them into independent groups.

Instead, we calculate the difference for each cashier.

And so on.

By the end, we’ll have 12 differences.

Here’s the important insight:

`A paired samples t-test is actually a one-sample t-test performed on the differences.`

This is why I intentionally placed it after the one-sample t-test and
the independent samples t-test. Once you see this connection, the
formulas become much easier to understand.

### Module Structure

1.  Load the dataset.
2.  Explore the dataset (`head(), str(), summary()`).
3.  Calculate descriptive statistics for the Before and After scores.
4.  Create a new Difference column (After - Before).
5.  Calculate descriptive statistics for the differences.
6.  Check the normality of the differences using `shapiro.test()`.
7.  Manually calculate the paired t-test.
8.  Find the critical value using `qt()`.
9.  Calculate the p-value using `pt()`.
10. Verify the result using
11. Write the statistical interpretation.
12. Write the business recommendation.

### Why this order matters

After this module, you’ll see that the three t-tests are closely
related: \| Test \| What is being compared? \| \| ————————– \|
——————————————- \| \| One-Sample t-test \| One sample mean vs. a known
population mean \| \| Independent Samples t-test \| Two independent
sample means \| \| Paired Samples t-test \| The mean of the paired
differences vs. zero \|

Once you understand this relationship, you’ll find it much easier to
decide which t-test to use in a real business problem. After Module 3,
moving to the one-sample z-test will feel like a small change rather
than learning an entirely new technique.

### Step 2: Exploratory Data Analysis

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

``` r
nrow(paired_data)
```

    ## [1] 12

``` r
ncol(paired_data)
```

    ## [1] 3

### Step 3: Descriptive Statistics

``` r
paired_data_before <- paired_data$Before

paired_data_after <- paired_data$After

mean_paired_data_before <- mean(paired_data_before)
mean_paired_data_after <- mean(paired_data_after)

median_paired_data_before <- median(paired_data_before)
median_paired_data_after <- median(paired_data_after)

sd_paired_data_before <- sd(paired_data_before)
sd_paired_data_after <- sd(paired_data_after)

var_paired_data_before <- var(paired_data_before)
var_paired_data_after <- var(paired_data_after)

min_paired_data_before <- min(paired_data_before)
min_paired_data_after <- min(paired_data_after)

max_paired_data_before <- max(paired_data_before)
max_paired_data_after <- max(paired_data_after)

quartile_paired_data_before <- quantile(paired_data_before)
quartile_paired_data_after <- quantile(paired_data_after)

cat(paste("Cashiers Average Before Training: ", mean_paired_data_before))
```

    ## Cashiers Average Before Training:  71.4166666666667

``` r
cat(paste("\nCashiers Average After Training: ", mean_paired_data_after))
```

    ## 
    ## Cashiers Average After Training:  74.4166666666667

``` r
cat(paste("\nMedian Before Training: ", median_paired_data_before))
```

    ## 
    ## Median Before Training:  71.5

``` r
cat(paste("\nMedian After Training: ", median_paired_data_after))
```

    ## 
    ## Median After Training:  74.5

``` r
cat(paste("\nStandard Deviation Before Training: ", sd_paired_data_before))
```

    ## 
    ## Standard Deviation Before Training:  2.77843426585856

``` r
cat(paste("\nStandard Deviation After Training: ", sd_paired_data_after))
```

    ## 
    ## Standard Deviation After Training:  3.67938565359474

``` r
cat(paste("\nVariance Before Training: ", var_paired_data_before))
```

    ## 
    ## Variance Before Training:  7.71969696969697

``` r
cat(paste("\nVariance After Training: ", var_paired_data_after))
```

    ## 
    ## Variance After Training:  13.5378787878788

``` r
cat(paste("\nMinimum Before Training: ", min_paired_data_before))
```

    ## 
    ## Minimum Before Training:  67

``` r
cat(paste("\nMinimum After Training: ", min_paired_data_after))
```

    ## 
    ## Minimum After Training:  69

``` r
cat(paste("\nMaximum Before Training: ", max_paired_data_before))
```

    ## 
    ## Maximum Before Training:  76

``` r
cat(paste("\nMaximum After Training: ", max_paired_data_after))
```

    ## 
    ## Maximum After Training:  81

``` r
cat(paste("\nQuartile Before Training: ", quartile_paired_data_before))
```

    ## 
    ## Quartile Before Training:  67 
    ## Quartile Before Training:  69.75 
    ## Quartile Before Training:  71.5 
    ## Quartile Before Training:  73.25 
    ## Quartile Before Training:  76

``` r
cat(paste("\nQuartile After Training: ", quartile_paired_data_after))
```

    ## 
    ## Quartile After Training:  69 
    ## Quartile After Training:  71.75 
    ## Quartile After Training:  74.5 
    ## Quartile After Training:  76.5 
    ## Quartile After Training:  81

### Step 4: Create the Difference Variable

This is the most important step in the entire module.

``` r
paired_data$Difference <-
paired_data$After -
paired_data$Before

paired_data
```

    ##    Cashier_ID Before After Difference
    ## 1        E001     72    75          3
    ## 2        E002     68    70          2
    ## 3        E003     75    79          4
    ## 4        E004     70    71          1
    ## 5        E005     73    78          5
    ## 6        E006     69    72          3
    ## 7        E007     74    76          2
    ## 8        E008     71    75          4
    ## 9        E009     67    69          2
    ## 10       E010     76    81          5
    ## 11       E011     72    73          1
    ## 12       E012     70    74          4

### Step 5: Descriptive Statistics of the Differences

``` r
difference_mean <- mean(paired_data$Difference)

difference_sd <- sd(paired_data$Difference)

difference_variance <- var(paired_data$Difference)

difference_median <- median(paired_data$Difference)

difference_min <- min(paired_data$Difference)

difference_max <- max(paired_data$Difference)

difference_quantiles <- quantile(
  paired_data$Difference
)

cat(paste("\nAverage Cashier Difference: ", difference_mean))
```

    ## 
    ## Average Cashier Difference:  3

``` r
cat(paste("\nStandard Deviation of Cashier Difference: ", difference_sd))
```

    ## 
    ## Standard Deviation of Cashier Difference:  1.4142135623731

``` r
cat(paste("\nVariance Cashier Difference: ", difference_variance))
```

    ## 
    ## Variance Cashier Difference:  2

``` r
cat(paste("\nMedian Cashier Difference: ", difference_median))
```

    ## 
    ## Median Cashier Difference:  3

``` r
cat(paste("\nMinimum Cashier Difference: ", difference_min))
```

    ## 
    ## Minimum Cashier Difference:  1

``` r
cat(paste("\nMaximum Cashier Difference: ", difference_max, "\n"))
```

    ## 
    ## Maximum Cashier Difference:  5

``` r
cat(paste("Quartile Cashier Difference\n"))
```

    ## Quartile Cashier Difference

``` r
difference_quantiles
```

    ##   0%  25%  50%  75% 100% 
    ##    1    2    3    4    5

**Interpretation**

Instead of asking

What is the average satisfaction score?

we’re asking

What is the average improvement?

If the mean difference is

3.2

that means

On average, customer satisfaction increased by 3.2 points after
training.

That already sounds like a business insight.

But… Is 3.2 large enough?

That’s what hypothesis testing tells us.

### Step 6: Check the Assumption

Unlike the independent t-test, we do NOT check normality separately for
Before and After. We only check the Difference.

The reason is because the paired t-test assumes the differences are
normally distributed

—not necessarily the original Before and After scores.

``` r
shapiro.test(
  paired_data$Difference
)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  paired_data$Difference
    ## W = 0.91705, p-value = 0.2624

**Interpretation of Assumptions:**

p \> 0.05 → no evidence against normality. p \< 0.05 → evidence that the
data are not normally distributed.

If p \> 0.05 then The differences appear to be normally distributed.

The assumption is satisfied.

If p \< 0.05 we reject the null hypothesis of the Shapiro-Wilk test.

There is sufficient evidence to conclude that the difference scores are
not normally distributed.

Therefore,

The normality assumption for the paired t-test is not satisfied.

If the p_value \> 0.05 (The normality assumption is satified) then
continue with the test, if otherwise(the normality assumption is
violated) then the alternative for testing will be
`Wilcoxon Signed-Rank Test`

### Interpretation

There is no sufficient evidence to conclude that the difference scores
are not normally distributed. Therefore, the normality assumption
required for the paired samples t-test is satisfied.

### Step 7: Define the Values

``` r
sample_mean <- mean(
  paired_data$Difference
)

sample_sd <- sd(
  paired_data$Difference
)

sample_size <- length(
  paired_data$Difference
)

mu <- 0

alpha_ <- 0.05

degrees_freedom <- sample_size - 1

cat("Mean Difference :", sample_mean)
```

    ## Mean Difference : 3

``` r
cat("\nStandard Deviation :", sample_sd)
```

    ## 
    ## Standard Deviation : 1.414214

``` r
cat("\nSample Size :", sample_size)
```

    ## 
    ## Sample Size : 12

``` r
cat("\nHypothesized Mean Difference :", mu)
```

    ## 
    ## Hypothesized Mean Difference : 0

``` r
cat("\nDegrees of Freedom :", degrees_freedom)
```

    ## 
    ## Degrees of Freedom : 11

### Step 9: Calculate the Standard Error

The formula is exactly the same as the one-sample t-test

``` r
standard_error <- sample_sd / sqrt(sample_size)

standard_error
```

    ## [1] 0.4082483

**Interpretation**

The standard error measures how much the average improvement score would
vary if we repeatedly selected random samples of 12 cashiers.

### Step 10: Calculate the t-statistic

``` r
t_calculated <- (sample_mean - mu) / standard_error

t_calculated
```

    ## [1] 7.348469

**Interpretation**

The t-statistic tells us how many standard errors the observed mean
improvement is above the hypothesized improvement of zero.

### Step 11: Find the Critical Value

Since this is a right-tailed test, we use one critical value.

``` r
critical_t <- qt(
  0.95,
  df = degrees_freedom
)

critical_t
```

    ## [1] 1.795885

### Step 12: Decision Using the Critical Value

``` r
if (t_calculated > critical_t) {

  print("Reject H0")

} else {

  print("Fail to Reject H0")

}
```

    ## [1] "Reject H0"

### Step 13: Calculate the p-value

Again, because this is a right-tailed test:

``` r
p_value <- pt(
  t_calculated,
  df = degrees_freedom,
  lower.tail = FALSE
)

p_value
```

    ## [1] 7.255373e-06

### Step 14: Decision Using the p-value

``` r
if (p_value < alpha_) {

  print("Reject H0")

} else {

  print("Fail to Reject H0")

}
```

    ## [1] "Reject H0"

The conclusion from the p-value method should match the critical value
method.

### Step 15: Verify with t.test()

Finally, let R confirm your manual calculations.

``` r
t.test(
  paired_data$After,
  paired_data$Before,
  paired = TRUE,
  alternative = "greater",
  conf.level = 0.95
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

### The Most Important Concept in This Module

The key idea from the paired samples t-test:

- One-Sample t-test: Tests whether one sample mean differs from a known
  value.
- Paired Samples t-test: First computes the difference for each pair,
  then performs a one-sample t-test on those differences.

That’s why almost every calculation in this module is identical to
Module 1. The only new step was creating the Difference column. Once I
did that, the rest of the analysis followed the same workflow.

### Statistical Conclusion

The paired samples t-test produced a t-statistic of 7.35 with 11 degrees
of freedom and a p-value of 0.0000073. Since the p-value is less than
the 5% significance level, we reject the null hypothesis. There is
sufficient statistical evidence to conclude that the customer service
training significantly improved customer satisfaction scores.

### Business Recommendation

The analysis shows that the customer service training program led to a
statistically significant improvement in customer satisfaction scores,
with an average increase of 3 points. FreshMart Supermarket should
continue investing in staff training, as the results suggest that the
program positively influences customer experience. Management may also
consider extending similar training to other branches while monitoring
long-term performance.

### What I Learned (Module 3)

What I Learned

- A paired samples t-test is used when the same subjects are measured
  twice or when observations are naturally paired.
- Instead of comparing two separate groups, we calculate the difference
  between each pair of observations.
- The paired samples t-test is mathematically equivalent to performing a
  one-sample t-test on the difference scores.
- The normality assumption applies to the difference scores, not the
  original “Before” and “After” measurements.
- A one-tailed paired t-test uses a one-sided confidence interval and a
  right-tail p-value when the alternative hypothesis states that the
  mean difference is greater than zero.

## Module 4: One-Sample z-Test in R

When I first learned hypothesis testing, I expected the z-test to be
completely different from the t-test. It isn’t.

In fact, you’ll notice that about 90% of the workflow is identical. The
only major differences are:

- We know the population standard deviation (σ).
- We use the standard normal (Z) distribution instead of the
  t-distribution.

This is also where you’ll finally use the functions you mentioned from
your class:

`qnorm()`

Why?

Because the z-test uses the standard normal distribution, which doesn’t
require degrees of freedom.

`pnorm()`

Why?

Because we’re working with the standard normal distribution rather than
the t-distribution.

### Module Structure

1.  Business Scenario
2.  Why a z-test?
3.  Load the dataset
4.  Exploratory Data Analysis
5.  Descriptive Statistics
6.  State the hypotheses
7.  Define the required values
8.  Calculate the standard error
9.  Calculate the z-statistic
10. Find the critical value using `qnorm()`
11. Decision using the critical value
12. Calculate the p-value using pnorm()
13. Decision using the p-value
14. Verify using a z-test function (or manual verification)
15. Statistical interpretation
16. Business recommendation
17. What I learned

### Business Scenario

FreshMart Supermarket has historically recorded an average customer
purchase amount of ₦15,000.

The finance department also knows, from years of historical data, that
the population standard deviation is ₦4,000.

Management recently launched several store improvements and wants to
determine whether the average customer purchase amount has increased.

Rather than surveying every customer, a random sample of 40 customers
was selected.

**Variable of Interest**

`Purchase_Amount`

### Why are we using a One-Sample z-Test?

A one-sample z-test is appropriate because:

- We have one sample.
- We are comparing the sample mean with a known population mean
  (₦15,000).
- The population standard deviation is known (₦4,000).
- The sample size is 40, matching the classroom convention for using a
  z-test.

**Business Question**

`Has the average customer purchase amount increased beyond ₦15,000?`

Notice the word increased.

That immediately tells us this is a right-tailed test.

**Hypotheses**

H0: The average purchase amount has not increased.

`H0 = 15000`

H1: The average purchase amount has increased.

`H1 > 15000`

**Significance Level**

``` r
alpha_ <- 0.05
```
