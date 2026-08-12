Non-Parametric Tests in R
================
Odia
2026-08-12

# Non-Parametric Tests

Non-parametric tests are useful when assumptions of parametric tests are
not satisfied, especially with non-normal data, ordinal data, outliers,
or small samples.

Our pattern remains: business scenario, EDA, descriptive analysis,
hypotheses, assumptions, test, decision, interpretation, recommendation,
and lessons learned.

## Module 1: Mann-Whitney U Test

### Business Scenario

FreshMart wants to know whether purchase amounts differ between two
independent campaigns when normality is not supported.

### Load and explore

``` r
marketing_data <- read.csv("mann_whitney_data.csv")

head(marketing_data)
```

    ##   Customer_ID Campaign Purchase_Amount
    ## 1           1      Old           11800
    ## 2           2      Old           13200
    ## 3           3      Old           14500
    ## 4           4      Old           12100
    ## 5           5      Old           13900
    ## 6           6      Old           12700

``` r
str(marketing_data)
```

    ## 'data.frame':    24 obs. of  3 variables:
    ##  $ Customer_ID    : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Campaign       : chr  "Old" "Old" "Old" "Old" ...
    ##  $ Purchase_Amount: int  11800 13200 14500 12100 13900 12700 15100 13400 14200 12500 ...

``` r
summary(marketing_data)
```

    ##   Customer_ID         Campaign  Purchase_Amount
    ##  Min.   : 1.00   Length   :24   Min.   :11800  
    ##  1st Qu.: 6.75   N.unique : 2   1st Qu.:13350  
    ##  Median :12.50   N.blank  : 0   Median :15450  
    ##  Mean   :12.50   Min.nchar: 3   Mean   :15296  
    ##  3rd Qu.:18.25   Max.nchar: 3   3rd Qu.:17175  
    ##  Max.   :24.00                  Max.   :18800

``` r
table(marketing_data$Campaign)
```

    ## 
    ## New Old 
    ##  12  12

### Groups

``` r
old <- subset(
  marketing_data,
  Campaign == "Old"
)$Purchase_Amount

new <- subset(
  marketing_data,
  Campaign == "New"
)$Purchase_Amount
```

### Descriptive statistics

``` r
mean(old)
```

    ## [1] 13333.33

``` r
median(old)
```

    ## [1] 13300

``` r
mean(new)
```

    ## [1] 17258.33

``` r
median(new)
```

    ## [1] 17250

``` r
sd(old)
```

    ## [1] 993.9209

``` r
sd(new)
```

    ## [1] 975.5729

### Normality

``` r
shapiro.test(old)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  old
    ## W = 0.98646, p-value = 0.9981

``` r
shapiro.test(new)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  new
    ## W = 0.96733, p-value = 0.8809

### Hypotheses

H0: The two groups have the same distribution.

H1: The two groups have different distributions.

### Test

``` r
mw_result <- wilcox.test(
  new,
  old,
  alternative = "two.sided",
  exact = FALSE,
  conf.int = TRUE
)

mw_result
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  new and old
    ## W = 144, p-value = 3.658e-05
    ## alternative hypothesis: true location shift is not equal to 0
    ## 95 percent confidence interval:
    ##  3100 4800
    ## sample estimates:
    ## difference in location 
    ##                   4000

### Decision

``` r
if (mw_result$p.value < alpha) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

### Interpretation

A significant result indicates evidence that the two independent groups
differ in distribution.

### Business Recommendation

Use the result to compare campaigns when the independent t-test
assumptions are not suitable.

### What I Learned

- Mann-Whitney is a non-parametric alternative to the independent
  t-test.
- It uses ranks.
- The observations must be independent.

------------------------------------------------------------------------

## Module 2: Wilcoxon Signed-Rank Test

### Business Scenario

FreshMart measures the same employees before and after training, but the
differences are not normally distributed.

### Load and explore

``` r
paired_data <- read.csv("wilcoxon_signed_rank_data.csv")

head(paired_data)
```

    ##   Employee_ID Before After
    ## 1           1     72    75
    ## 2           2     68    71
    ## 3           3     75    78
    ## 4           4     70    74
    ## 5           5     81    84
    ## 6           6     77    80

``` r
str(paired_data)
```

    ## 'data.frame':    12 obs. of  3 variables:
    ##  $ Employee_ID: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Before     : int  72 68 75 70 81 77 69 74 71 80 ...
    ##  $ After      : int  75 71 78 74 84 80 72 77 74 84 ...

``` r
summary(paired_data)
```

    ##   Employee_ID        Before          After      
    ##  Min.   : 1.00   Min.   :68.00   Min.   :71.00  
    ##  1st Qu.: 3.75   1st Qu.:70.75   1st Qu.:74.00  
    ##  Median : 6.50   Median :73.50   Median :76.50  
    ##  Mean   : 6.50   Mean   :73.83   Mean   :77.00  
    ##  3rd Qu.: 9.25   3rd Qu.:76.25   3rd Qu.:79.25  
    ##  Max.   :12.00   Max.   :81.00   Max.   :84.00

### Differences

``` r
paired_data$Difference <-
  paired_data$After -
  paired_data$Before

mean(paired_data$Difference)
```

    ## [1] 3.166667

``` r
median(paired_data$Difference)
```

    ## [1] 3

``` r
sd(paired_data$Difference)
```

    ## [1] 0.3892495

``` r
shapiro.test(paired_data$Difference)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  paired_data$Difference
    ## W = 0.46465, p-value = 9.811e-06

### Hypotheses

H0: The median paired difference is 0.

H1: The median paired difference is not 0.

### Test

``` r
wilcox_result <- wilcox.test(
  paired_data$After,
  paired_data$Before,
  paired = TRUE,
  alternative = "two.sided",
  exact = FALSE,
  conf.int = TRUE
)
```

    ## Warning in .wilcox_test_one_cint_asymp(x, n, alternative, conf.level, correct
    ## >= : requested conf.level not achievable

``` r
wilcox_result
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  paired_data$After and paired_data$Before
    ## V = 78, p-value = 0.001222
    ## alternative hypothesis: true location shift is not equal to 0
    ## 60 percent confidence interval:
    ##  3.000000 3.000086
    ## sample estimates:
    ## (pseudo)median 
    ##       3.000077

### Decision

``` r
if (wilcox_result$p.value < alpha) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

### Interpretation

A significant result indicates evidence of a systematic change between
paired measurements.

### Business Recommendation

Use the result to assess an intervention when normality of the paired
differences is not supported.

### What I Learned

- Wilcoxon Signed-Rank is the non-parametric counterpart to the paired
  t-test.
- It works with paired observations and signed ranks.
- Zero represents no paired change.

------------------------------------------------------------------------

## Module 3: Kruskal-Wallis Test

### Business Scenario

FreshMart wants to compare purchase amounts across three or more cities,
but the data are not suitable for one-way ANOVA.

### Load and explore

``` r
marketing_data <- read.csv("kruskal_wallis_data.csv")

head(marketing_data)
```

    ##   Customer_ID  City Purchase_Amount
    ## 1           1 Abuja           12000
    ## 2           2 Abuja           14500
    ## 3           3 Abuja           13200
    ## 4           4 Abuja           15100
    ## 5           5 Abuja           12800
    ## 6           6 Abuja           13900

``` r
str(marketing_data)
```

    ## 'data.frame':    30 obs. of  3 variables:
    ##  $ Customer_ID    : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ City           : chr  "Abuja" "Abuja" "Abuja" "Abuja" ...
    ##  $ Purchase_Amount: int  12000 14500 13200 15100 12800 13900 12500 14800 13400 14100 ...

``` r
summary(marketing_data)
```

    ##   Customer_ID           City    Purchase_Amount
    ##  Min.   : 1.00   Length   :30   Min.   :12000  
    ##  1st Qu.: 8.25   N.unique : 3   1st Qu.:13825  
    ##  Median :15.50   N.blank  : 0   Median :15000  
    ##  Mean   :15.50   Min.nchar: 4   Mean   :15330  
    ##  3rd Qu.:22.75   Max.nchar: 5   3rd Qu.:16900  
    ##  Max.   :30.00                  Max.   :19100

``` r
table(marketing_data$City)
```

    ## 
    ## Abuja  Kano Lagos 
    ##    10    10    10

### Hypotheses

H0: The groups come from the same distribution.

H1: At least one group differs.

### Test

``` r
kruskal_result <- kruskal.test(
  Purchase_Amount ~ City,
  data = marketing_data
)

kruskal_result
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  Purchase_Amount by City
    ## Kruskal-Wallis chi-squared = 21.236, df = 2, p-value = 2.447e-05

### Decision

``` r
if (kruskal_result$p.value < alpha) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

### Post hoc analysis

If significant:

``` r
pairwise.wilcox.test(
  marketing_data$Purchase_Amount,
  marketing_data$City,
  p.adjust.method = "bonferroni"
)
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum exact test 
    ## 
    ## data:  marketing_data$Purchase_Amount and marketing_data$City 
    ## 
    ##       Abuja   Kano   
    ## Kano  0.13    -      
    ## Lagos 3.2e-05 3.2e-05
    ## 
    ## P value adjustment method: bonferroni

### Interpretation

A significant Kruskal-Wallis result indicates that at least one group
differs in distribution.

### Business Recommendation

Investigate the specific groups identified by post hoc testing before
implementing location-wide decisions.

### What I Learned

- Kruskal-Wallis is the non-parametric counterpart to one-way ANOVA.
- It compares three or more independent groups.
- It uses ranks.
- A significant result requires post hoc comparisons.

------------------------------------------------------------------------

## Module 4: Chi-Square Test of Independence

### Business Scenario

FreshMart wants to know whether membership status is associated with
campaign response.

### Load and explore

``` r
marketing_data <- read.csv("chi_square_data.csv")

head(marketing_data)
```

    ##   Customer_ID Membership Response
    ## 1           1      Basic       No
    ## 2           2      Basic       No
    ## 3           3      Basic       No
    ## 4           4      Basic       No
    ## 5           5      Basic       No
    ## 6           6      Basic       No

``` r
str(marketing_data)
```

    ## 'data.frame':    60 obs. of  3 variables:
    ##  $ Customer_ID: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Membership : chr  "Basic" "Basic" "Basic" "Basic" ...
    ##  $ Response   : chr  "No" "No" "No" "No" ...

``` r
summary(marketing_data)
```

    ##   Customer_ID        Membership      Response 
    ##  Min.   : 1.00   Length   :60   Length   :60  
    ##  1st Qu.:15.75   N.unique : 3   N.unique : 2  
    ##  Median :30.50   N.blank  : 0   N.blank  : 0  
    ##  Mean   :30.50   Min.nchar: 4   Min.nchar: 2  
    ##  3rd Qu.:45.25   Max.nchar: 6   Max.nchar: 3  
    ##  Max.   :60.00

``` r
table(
  marketing_data$Membership,
  marketing_data$Response
)
```

    ##         
    ##          No Yes
    ##   Basic  14   6
    ##   Gold    6  14
    ##   Silver 10  10

### Observed frequencies

``` r
observed <- table(
  marketing_data$Membership,
  marketing_data$Response
)

observed
```

    ##         
    ##          No Yes
    ##   Basic  14   6
    ##   Gold    6  14
    ##   Silver 10  10

### Hypotheses

H0: Membership status and campaign response are independent.

H1: Membership status and campaign response are associated.

### Test

``` r
chi_result <- chisq.test(observed)
chi_result
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  observed
    ## X-squared = 6.4, df = 2, p-value = 0.04076

### Expected frequencies

``` r
chi_result$expected
```

    ##         
    ##          No Yes
    ##   Basic  10  10
    ##   Gold   10  10
    ##   Silver 10  10

Expected frequencies should generally be sufficiently large for the
chi-square approximation to be reliable. For small tables with small
expected counts, consider Fisher’s exact test.

### Decision

``` r
if (chi_result$p.value < alpha) {
  print("Reject H0")
} else {
  print("Fail to Reject H0")
}
```

    ## [1] "Reject H0"

### Interpretation

A significant result indicates evidence of an association between the
categorical variables. It does not prove causation.

### Business Recommendation

If an association exists, investigate which membership groups respond
differently and use the result to improve campaign targeting.

### What I Learned

- Chi-square tests categorical variables.
- It compares observed and expected frequencies.
- A significant result indicates association, not causation.
