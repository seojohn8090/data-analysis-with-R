Moments
================

### Using daily closing index data from 2005 to 2024, compute both simple and logarithmic returns. (a) Evaluate their distributional properties by calculating the mean, variance, skewness, and kurtosis. (b) Conduct statistical tests to assess the presence of asymmetry and fat tails in each return distribution. (c) Analyze whether there are statistically significant differences between the two types of returns.

### 1. SSE Composite Index

#### (a)

``` r
data <- read_csv("Data/Shanghai Composite Historical Data.csv")
```

    ## Rows: 4860 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Date, Vol., Change %
    ## num (4): Price, Open, High, Low
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data <- data %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  arrange(Date) %>%
  mutate(
    simple_return = Price / lag(Price) - 1,
    log_return = log(Price / lag(Price))
  ) %>%
  filter(!is.na(simple_return))

stats_simple <- basicStats(data$simple_return)
stats_simple
```

    ##             X..data.simple_return
    ## nobs                  4859.000000
    ## NAs                      0.000000
    ## Minimum                 -0.088407
    ## Maximum                  0.094549
    ## 1. Quartile             -0.006111
    ## 3. Quartile              0.007353
    ## Mean                     0.000317
    ## Median                   0.000581
    ## Sum                      1.541259
    ## SE Mean                  0.000215
    ## LCL Mean                -0.000105
    ## UCL Mean                 0.000739
    ## Variance                 0.000225
    ## Stdev                    0.015001
    ## Skewness                -0.390656
    ## Kurtosis                 5.154275

``` r
stats_log    <- basicStats(data$log_return)
stats_log
```

    ##             X..data.log_return
    ## nobs               4859.000000
    ## NAs                   0.000000
    ## Minimum              -0.092562
    ## Maximum               0.090343
    ## 1. Quartile          -0.006130
    ## 3. Quartile           0.007326
    ## Mean                  0.000204
    ## Median                0.000581
    ## Sum                   0.992143
    ## SE Mean               0.000216
    ## LCL Mean             -0.000219
    ## UCL Mean              0.000628
    ## Variance              0.000227
    ## Stdev                 0.015052
    ## Skewness             -0.548834
    ## Kurtosis              5.319291

#### (b)

``` r
# Simple return
# Skewness test
Z_skew_simple_return <- sqrt(4859/6) * (-0.390656)
Z_skew_simple_return
```

    ## [1] -11.11712

``` r
p_skew_simple_return <- 2 * (1 - pnorm(abs(Z_skew_simple_return)))
p_skew_simple_return
```

    ## [1] 0

``` r
# Kurtosis test
Z_kurt_simple_return <- sqrt(4859/24) * (5.154275)
Z_kurt_simple_return
```

    ## [1] 73.33907

``` r
p_kurt_simple_return <- 2 * (1 - pnorm(abs(Z_kurt_simple_return)))
p_kurt_simple_return
```

    ## [1] 0

``` r
# Log return
# Skewness test
Z_skew_log_return <- sqrt(4859/6) * (-0.548834)
Z_skew_log_return
```

    ## [1] -15.61848

``` r
p_skew_log_return <- 2 * (1 - pnorm(abs(Z_skew_log_return)))
p_skew_log_return
```

    ## [1] 0

``` r
# Kurtosis test
Z_kurt_log_return <- sqrt(4859/24) * (5.319291)
Z_kurt_log_return
```

    ## [1] 75.68705

``` r
p_kurt_log_return <- 2 * (1 - pnorm(abs(Z_kurt_log_return)))
p_kurt_log_return
```

    ## [1] 0

#### Both null hypotheses are rejected, indicating that the data exhibit statistically significant skewness and excess kurtosis. This implies that the distribution is not symmetric and deviates from normality, with either heavier or lighter tails than the normal distribution.

#### (c)

#### The mean of simple returns (0.000317) is slightly higher than that of log returns (0.000204). This is because log returns are continuously compounded and account for the effects of compounding more accurately, typically leading to slightly lower average values than simple returns.

------------------------------------------------------------------------

### 2. KOSPI

#### (a)

``` r
data_1 <- read_csv("Data/KOSPI Historical Data.csv")
```

    ## Rows: 4942 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Date, Vol., Change %
    ## num (4): Price, Open, High, Low
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data_1 <- data_1 %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  arrange(Date) %>%
  mutate(
    simple_return = Price / lag(Price) - 1,
    log_return = log(Price / lag(Price))
  ) %>%
  filter(!is.na(simple_return))

stats_simple_1 <- basicStats(data_1$simple_return)
stats_simple_1
```

    ##             X..data_1.simple_return
    ## nobs                    4941.000000
    ## NAs                        0.000000
    ## Minimum                   -0.105705
    ## Maximum                    0.119457
    ## 1. Quartile               -0.005310
    ## 3. Quartile                0.006540
    ## Mean                       0.000274
    ## Median                     0.000611
    ## Sum                        1.356157
    ## SE Mean                    0.000173
    ## LCL Mean                  -0.000066
    ## UCL Mean                   0.000615
    ## Variance                   0.000149
    ## Stdev                      0.012193
    ## Skewness                  -0.336250
    ## Kurtosis                   8.663526

``` r
stats_log_1    <- basicStats(data_1$log_return)
stats_log_1
```

    ##             X..data_1.log_return
    ## nobs                 4941.000000
    ## NAs                     0.000000
    ## Minimum                -0.111720
    ## Maximum                 0.112844
    ## 1. Quartile            -0.005324
    ## 3. Quartile             0.006519
    ## Mean                    0.000200
    ## Median                  0.000611
    ## Sum                     0.987630
    ## SE Mean                 0.000174
    ## LCL Mean               -0.000141
    ## UCL Mean                0.000541
    ## Variance                0.000149
    ## Stdev                   0.012224
    ## Skewness               -0.529591
    ## Kurtosis                8.851759

#### (b)

``` r
# Simple return
# Skewness test
Z_skew_simple_return_1 <- sqrt(4941/6) * (-0.336250)
Z_skew_simple_return_1
```

    ## [1] -9.649262

``` r
p_skew_simple_return_1 <- 2 * (1 - pnorm(abs(Z_skew_simple_return_1)))
p_skew_simple_return_1
```

    ## [1] 0

``` r
# Kurtosis test
Z_kurt_simple_return_1 <- sqrt(4941/24) * (8.663526)
Z_kurt_simple_return_1
```

    ## [1] 124.3073

``` r
p_kurt_simple_return_1 <- 2 * (1 - pnorm(abs(Z_kurt_simple_return_1)))
p_kurt_simple_return_1
```

    ## [1] 0

``` r
# Log return
# Skewness test
Z_skew_log_return_1 <- sqrt(4941/6) * (-0.529591)
Z_skew_log_return_1
```

    ## [1] -15.19751

``` r
p_skew_log_return_1 <- 2 * (1 - pnorm(abs(Z_skew_log_return_1)))
p_skew_log_return_1
```

    ## [1] 0

``` r
# Kurtosis test
Z_kurt_log_return_1 <- sqrt(4941/24) * (8.851759)
Z_kurt_log_return_1
```

    ## [1] 127.0081

``` r
p_kurt_log_return_1 <- 2 * (1 - pnorm(abs(Z_kurt_log_return_1)))
p_kurt_log_return_1
```

    ## [1] 0
