Seasonal Adjustment
================

### Using quarterly GDP data for China spanning 1992–2024, implement seasonal adjustment via deterministic seasonal filtering and the X‑11 procedure separately. Assess which method yields superior performance.

``` r
# 数据预处理
gdp <- read_excel("Data/GDP.xlsx", col_names = c("date", "gdp"))
gdp <- gdp %>%
  arrange(date)
gdp_ts <- ts(gdp$gdp,
             start = c(1992, 1),
             frequency = 4)
gdp_ts
```

    ##          Qtr1     Qtr2     Qtr3     Qtr4
    ## 1992   5284.9   6507.9   7218.4   8284.4
    ## 1993   6866.3   8390.4   9423.1  11139.9
    ## 1994   9424.1  11532.7  12925.4  14980.1
    ## 1995  12182.3  14685.2  16241.8  18540.1
    ## 1996  14720.6  17239.7  18705.5  21544.7
    ## 1997  16808.1  19280.8  20629.8  23506.4
    ## 1998  18205.4  20449.7  21945.6  25263.2
    ## 1999  19551.1  21751.3  23255.8  26820.7
    ## 2000  21569.1  24277.1  25973.8  29488.6
    ## 2001  24390.6  27017.8  28662.0  32086.9
    ## 2002  26666.0  29550.7  31664.4  35430.7
    ## 2003  30282.0  32978.2  35778.3  40338.8
    ## 2004  35107.8  39251.8  42448.0  47420.3
    ## 2005  41079.2  45399.8  48681.7  54746.8
    ## 2006  47831.7  53415.3  56820.5  64510.8
    ## 2007  58080.6  65751.0  70494.1  79854.0
    ## 2008  70609.6  79965.0  83668.3  90075.0
    ## 2009  75341.3  85355.6  91330.0 102494.7
    ## 2010  89125.0 101075.0 107792.9 121260.4
    ## 2011 106218.9 120776.8 128561.5 140150.3
    ## 2012 119377.5 133491.2 140384.0 154258.0
    ## 2013 131879.7 146113.9 154972.1 170694.7
    ## 2014 143539.4 159462.4 168601.7 184179.4
    ## 2015 154171.4 171887.0 180118.7 196334.4
    ## 2016 165735.3 185002.2 194837.5 215617.9
    ## 2017 185274.4 205665.6 216755.3 239687.6
    ## 2018 205735.3 228042.4 238796.8 263435.6
    ## 2019 221453.9 246193.9 256023.2 282201.4
    ## 2020 209671.1 253450.0 269910.2 301836.2
    ## 2021 255055.2 287979.2 297961.9 332826.7
    ## 2022 277175.8 299111.5 315399.6 342342.4
    ## 2023 292368.8 316237.5 328440.7 357224.8
    ## 2024 304525.2 328585.4 341443.2 373512.4

``` r
plot(gdp_ts,
     main = "China Quarterly GDP (1992-2024)",
     ylab = "GDP",
     xlab = "Year")
```

![](Seasonal-Adjustment_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
gdp_log <- log(gdp_ts)
plot(gdp_log)
```

![](Seasonal-Adjustment_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
# 确定性季节调整（一）
mth <- format(gdp$date, "%m")
d1 <- d2 <- d3 <- d4 <- rep(0, length(gdp_log))
d1[mth == "03"] <- 1
d2[mth == "06"] <- 1
d3[mth == "09"] <- 1
d4[mth == "12"] <- 1
# 以第四季度为基准组
d14 <- d1 - d4
d24 <- d2 - d4
d34 <- d3 - d4
out1 <- lm(gdp_log ~ d14 + d24 + d34)
summary(out1)
```

    ## 
    ## Call:
    ## lm(formula = gdp_log ~ d14 + d24 + d34)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4035 -1.0001  0.1796  1.0321  1.6504 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 11.12923    0.10209 109.013   <2e-16 ***
    ## d14         -0.15316    0.17683  -0.866    0.388    
    ## d24         -0.02992    0.17683  -0.169    0.866    
    ## d34          0.03508    0.17683   0.198    0.843    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.173 on 128 degrees of freedom
    ## Multiple R-squared:  0.008821,   Adjusted R-squared:  -0.01441 
    ## F-statistic: 0.3797 on 3 and 128 DF,  p-value: 0.7678

``` r
adj1 <- out1$coef[1] + residuals(out1)
adj1 <- exp(adj1)
adj1_ts <- ts(adj1,
              start = c(1992, 1),
              frequency = 4)
plot(gdp_ts,
     lty = 3,
     main = "Deterministic Seasonal Adjustment",
     ylab = "GDP",
     xlab = "Year")

lines(adj1_ts,
      lty = 1)

legend("topleft",
       legend = c("Original GDP", "Adjusted GDP"),
       lty = c(3, 1))
```

![](Seasonal-Adjustment_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

##### 首先采用固定季节效应的确定性季节模型进行调整。模型结果显示，季度虚拟变量 d14、d24、d34 的系数均不显著，整体F统计量对应的p值为0.7678，说明仅使用固定季度虚拟变量难以有效解释GDP序列中的季节波动。此外，该模型的调整后 R2 为负值，残差标准差达到1.173，说明模型拟合效果较差。

``` r
# 确定性季节调整（二）：季节效应随时间变化模型
t <- 1:length(gdp_log)
d14t <- d14 * t
d24t <- d24 * t
d34t <- d34 * t
out2 <- lm(gdp_log ~ t + d14 + d24 + d34 +
                     d14t + d24t + d34t)
summary(out2)
```

    ## 
    ## Call:
    ## lm(formula = gdp_log ~ t + d14 + d24 + d34 + d14t + d24t + d34t)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.43200 -0.08023  0.02532  0.10902  0.24209 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  9.1310613  0.0274753 332.337   <2e-16 ***
    ## t            0.0300568  0.0003585  83.844   <2e-16 ***
    ## d14         -0.1572565  0.0470496  -3.342   0.0011 ** 
    ## d24         -0.0291139  0.0474060  -0.614   0.5402    
    ## d34          0.0357768  0.0477652   0.749   0.4553    
    ## d14t         0.0007474  0.0006209   1.204   0.2310    
    ## d24t         0.0002063  0.0006209   0.332   0.7403    
    ## d34t        -0.0002437  0.0006209  -0.392   0.6954    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1569 on 124 degrees of freedom
    ## Multiple R-squared:  0.9828, Adjusted R-squared:  0.9819 
    ## F-statistic:  1014 on 7 and 124 DF,  p-value: < 2.2e-16

``` r
adj2 <- out2$coef[1] +
        out2$coef[2] * t +
        residuals(out2)

adj2 <- exp(adj2)

adj1_sub <- ts(adj1[73:length(adj1)],
               start = c(2010, 1),
               frequency = 4)

adj2_sub <- ts(adj2[73:length(adj2)],
               start = c(2010, 1),
               frequency = 4)

plot(adj1_sub,
     lty = 3,
     main = "Comparison of Deterministic Seasonal Adjustments",
     ylab = "GDP",
     xlab = "Year")

lines(adj2_sub,
      lty = 1)

legend("topleft",
       legend = c("Fixed Seasonal Effect",
                  "Time-varying Seasonal Effect"),
       lty = c(3, 1))
```

![](Seasonal-Adjustment_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

##### 进一步地，在模型中加入时间趋势项以及季度虚拟变量与时间的交互项后，模型拟合效果显著提升。结果显示，时间趋势项t高度显著，说明中国GDP长期呈现明显增长趋势；同时，第一季度虚拟变量d14在1%水平下显著，表明第一季度GDP相对于第四季度存在明显差异。改进模型的R2提高到0.9828，调整后R2达到0.9819，残差标准差下降至0.1569，远低于固定季节模型，说明加入趋势项和时间变化季节效应后，模型能够更好地刻画GDP的动态变化。

``` r
# X11
x11_fit <- seas(gdp_ts,
                x11 = "")
summary(x11_fit)
```

    ## 
    ## Call:
    ## seas(x = gdp_ts, x11 = "")
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## AO2020.1          -0.106732   0.006143 -17.376  < 2e-16 ***
    ## AO2021.2           0.032292   0.006143   5.257 1.46e-07 ***
    ## AR-Nonseasonal-01  0.428496   0.080019   5.355 8.56e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## X11 adj.  ARIMA: (1 1 0)(0 1 0)  Obs.: 132  Transform: log
    ## AICc:  2149, BIC:  2160  QS (no seasonality in final):7.296 *
    ## Box-Ljung (no autocorr.): 20.85   Shapiro (normality): 0.9847

``` r
x11_adj <- final(x11_fit)

plot(gdp_ts,
     lty = 3,
     main = "X11 Seasonal Adjustment",
     ylab = "GDP",
     xlab = "Year")

lines(x11_adj,
      lty = 1)

legend("topleft",
       legend = c("Original GDP", "X11 Adjusted GDP"),
       lty = c(3, 1))
```

![](Seasonal-Adjustment_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

##### 最后，采用X11方法对季度GDP进行季节调整。X11方法自动选择了对数变换以及ARIMA(1,1,0)(0,1,0)模型，并自动识别出2020年第一季度和2021年第二季度的异常值，这与疫情冲击带来的特殊经济波动相一致。季节调整后的序列基本消除了原始GDP中的季度波动，趋势更加清晰。
