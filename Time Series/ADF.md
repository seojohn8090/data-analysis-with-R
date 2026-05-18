ADF
================

### 1. Test the stationarity of the daily closing index of the Shanghai Composite Index from 2006 to 2025 using the ADF method.

``` r
# 第0步：数据预处理
data <- read_excel("Data/000001.SH.xlsx")
head(data)
```

    ## # A tibble: 6 × 12
    ##   交易日期   开盘点位 最高点位 最低点位 收盘价  涨跌  `涨跌幅(%)` 开始日累计涨跌
    ##   <chr>      <chr>    <chr>    <chr>    <chr>   <chr> <chr>       <chr>         
    ## 1 2025-12-31 3,968.73 3,977.54 3,955.49 3,968.… 3.72  0.09        2,807.78      
    ## 2 2025-12-30 3,947.87 3,979.99 3,947.42 3,965.… -0.16 0.00        2,804.06      
    ## 3 2025-12-29 3,964.65 3,983.98 3,956.95 3,965.… 1.60  0.04        2,804.22      
    ## 4 2025-12-26 3,957.83 3,977.71 3,945.52 3,963.… 4.06  0.10        2,802.62      
    ## 5 2025-12-25 3,937.72 3,964.07 3,936.08 3,959.… 18.67 0.47        2,798.56      
    ## 6 2025-12-24 3,920.35 3,947.04 3,912.31 3,940.… 20.97 0.53        2,779.89      
    ## # ℹ 4 more variables: 开始日累计涨跌幅 <chr>, `成交量(万股)` <chr>,
    ## #   `成交额(万元)` <chr>, 持仓量 <lgl>

``` r
colnames(data) <- c(
  "date", "open", "high", "low", "close", 
  "change", "pct_change", "cum_change", "cum_pct",
  "volume", "amount", "position"
)
data <- data %>% select(date, close)
data <- data %>%
  mutate(
    date = as.Date(date),  
    close = str_remove_all(close, ",") %>% as.numeric() 
  )
data <- na.omit(data)
head(data)
```

    ## # A tibble: 6 × 2
    ##   date       close
    ##   <date>     <dbl>
    ## 1 2025-12-31 3969.
    ## 2 2025-12-30 3965.
    ## 3 2025-12-29 3965.
    ## 4 2025-12-26 3964.
    ## 5 2025-12-25 3960.
    ## 6 2025-12-24 3941.

``` r
str(data)
```

    ## tibble [4,859 × 2] (S3: tbl_df/tbl/data.frame)
    ##  $ date : Date[1:4859], format: "2025-12-31" "2025-12-30" ...
    ##  $ close: num [1:4859] 3969 3965 3965 3964 3960 ...

``` r
price <- data$close
lnprice <- log(price)
head(lnprice)
```

    ## [1] 8.286229 8.285291 8.285332 8.284928 8.283903 8.279177

``` r
# 第1步：包含截距项和时间趋势项，并用AIC自动选择差分滞后阶数
k <- trunc((length(lnprice) - 1)^(1/3))
df1 <- ur.df(lnprice, lags = k, type = "trend", selectlags = "AIC")
summary(df1)
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression trend 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.086081 -0.006978 -0.000462  0.006096  0.093832 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.323e-02  7.806e-03   1.695 0.090181 .  
    ## z.lag.1      -1.614e-03  9.588e-04  -1.684 0.092294 .  
    ## tt           -2.380e-07  1.649e-07  -1.443 0.149089    
    ## z.diff.lag1   2.531e-02  1.440e-02   1.758 0.078793 .  
    ## z.diff.lag2  -1.360e-02  1.439e-02  -0.945 0.344740    
    ## z.diff.lag3   2.454e-02  1.437e-02   1.707 0.087885 .  
    ## z.diff.lag4   4.952e-02  1.438e-02   3.444 0.000577 ***
    ## z.diff.lag5   1.548e-03  1.439e-02   0.108 0.914354    
    ## z.diff.lag6  -6.225e-02  1.439e-02  -4.325 1.56e-05 ***
    ## z.diff.lag7   2.384e-02  1.442e-02   1.653 0.098308 .  
    ## z.diff.lag8  -1.028e-03  1.443e-02  -0.071 0.943206    
    ## z.diff.lag9   1.309e-02  1.442e-02   0.907 0.364345    
    ## z.diff.lag10  2.841e-03  1.440e-02   0.197 0.843557    
    ## z.diff.lag11  1.645e-02  1.440e-02   1.143 0.253165    
    ## z.diff.lag12  1.288e-02  1.438e-02   0.895 0.370566    
    ## z.diff.lag13  5.071e-02  1.438e-02   3.526 0.000426 ***
    ## z.diff.lag14 -3.635e-02  1.440e-02  -2.524 0.011624 *  
    ## z.diff.lag15  4.267e-02  1.440e-02   2.963 0.003066 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0148 on 4824 degrees of freedom
    ## Multiple R-squared:  0.01487,    Adjusted R-squared:  0.0114 
    ## F-statistic: 4.284 on 17 and 4824 DF,  p-value: 8.292e-09
    ## 
    ## 
    ## Value of test-statistic is: -1.6838 1.5327 1.7913 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau3 -3.96 -3.41 -3.12
    ## phi2  6.09  4.68  4.03
    ## phi3  8.27  6.25  5.34

##### 从上述结果中可以看出，差分滞后阶数为15。

##### tau3统计量为-1.6838\>-3.41(5%)，不能拒绝单位根原假设，序列非平稳。

##### phi3统计量为1.7913\<6.25(5%)，不能拒绝趋势项系数为0，因此去掉趋势项。

``` r
# 第2步：只包含截距项，固定滞后阶数
df2 <- ur.df(lnprice, lags = 15, type = "drift", selectlags = "Fixed")
summary(df2)
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression drift 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.086641 -0.006917 -0.000443  0.006190  0.093759 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.0084137  0.0070615   1.191 0.233517    
    ## z.lag.1      -0.0010827  0.0008857  -1.222 0.221609    
    ## z.diff.lag1   0.0252168  0.0143959   1.752 0.079895 .  
    ## z.diff.lag2  -0.0137146  0.0143892  -0.953 0.340578    
    ## z.diff.lag3   0.0244337  0.0143742   1.700 0.089226 .  
    ## z.diff.lag4   0.0494217  0.0143771   3.438 0.000592 ***
    ## z.diff.lag5   0.0013797  0.0143931   0.096 0.923639    
    ## z.diff.lag6  -0.0624012  0.0143936  -4.335 1.49e-05 ***
    ## z.diff.lag7   0.0237132  0.0144213   1.644 0.100175    
    ## z.diff.lag8  -0.0011767  0.0144270  -0.082 0.934997    
    ## z.diff.lag9   0.0129458  0.0144241   0.898 0.369490    
    ## z.diff.lag10  0.0026964  0.0143960   0.187 0.851430    
    ## z.diff.lag11  0.0163035  0.0143967   1.132 0.257503    
    ## z.diff.lag12  0.0127209  0.0143833   0.884 0.376511    
    ## z.diff.lag13  0.0505374  0.0143812   3.514 0.000445 ***
    ## z.diff.lag14 -0.0365333  0.0143981  -2.537 0.011200 *  
    ## z.diff.lag15  0.0424985  0.0144043   2.950 0.003189 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0148 on 4826 degrees of freedom
    ## Multiple R-squared:  0.01445,    Adjusted R-squared:  0.01118 
    ## F-statistic: 4.422 on 16 and 4826 DF,  p-value: 8.674e-09
    ## 
    ## 
    ## Value of test-statistic is: -1.2224 1.2534 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau2 -3.43 -2.86 -2.57
    ## phi1  6.43  4.59  3.78

##### tau2统计量为-1.2224\>-2.861(5%)，不能拒绝单位根原假设，序列非平稳。

##### phi1统计量为1.2534\<4.59(5%)，不能拒绝滞后项系数和截距项系数同时为零的假说。

``` r
# 第3步：无截距、无趋势，固定滞后阶数
df3 <- ur.df(lnprice, lags = 15, type = "none", selectlags = "Fixed")
summary(df3)
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression none 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 - 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.086866 -0.006864 -0.000377  0.006198  0.093669 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## z.lag.1      -2.787e-05  2.673e-05  -1.043 0.297193    
    ## z.diff.lag1   2.443e-02  1.438e-02   1.699 0.089445 .  
    ## z.diff.lag2  -1.447e-02  1.438e-02  -1.006 0.314234    
    ## z.diff.lag3   2.364e-02  1.436e-02   1.646 0.099734 .  
    ## z.diff.lag4   4.861e-02  1.436e-02   3.385 0.000718 ***
    ## z.diff.lag5   5.411e-04  1.438e-02   0.038 0.969978    
    ## z.diff.lag6  -6.323e-02  1.438e-02  -4.398 1.12e-05 ***
    ## z.diff.lag7   2.289e-02  1.441e-02   1.589 0.112159    
    ## z.diff.lag8  -2.018e-03  1.441e-02  -0.140 0.888629    
    ## z.diff.lag9   1.209e-02  1.441e-02   0.839 0.401443    
    ## z.diff.lag10  1.909e-03  1.438e-02   0.133 0.894412    
    ## z.diff.lag11  1.553e-02  1.438e-02   1.080 0.280353    
    ## z.diff.lag12  1.190e-02  1.437e-02   0.828 0.407478    
    ## z.diff.lag13  4.969e-02  1.436e-02   3.460 0.000546 ***
    ## z.diff.lag14 -3.738e-02  1.438e-02  -2.600 0.009362 ** 
    ## z.diff.lag15  4.164e-02  1.439e-02   2.894 0.003817 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0148 on 4827 degrees of freedom
    ## Multiple R-squared:  0.01443,    Adjusted R-squared:  0.01116 
    ## F-statistic: 4.417 on 16 and 4827 DF,  p-value: 8.958e-09
    ## 
    ## 
    ## Value of test-statistic is: -1.0426 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau1 -2.58 -1.95 -1.62

##### tau1统计量为-1.0426\>-1.95(5%)，不能拒绝单位根原假设，序列非平稳。

##### 总结：检验结果显示，tau3、tau2、tau1统计量均大于5%显著性水平临界值。因此，在5%显著性水平下，不能拒绝单位根原假设，上证综指日度收盘价对数序列为非平稳序列。
