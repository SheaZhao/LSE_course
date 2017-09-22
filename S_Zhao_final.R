---
    title: "ME415 Latent Variable Modelling and Structual Equation Modelling for Social Science Research"
author: "Shea Zhao"
date: "August 28, 2017"
output: pdf_document
---
    
    ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Description

This document is the final project for the LSE **ME415** Summer Methods Programme course. Below is the R I used as well as a step-by-step analysis of the output.

## Load data
```r
# import & explore data
library(foreign)
library(haven)

physfunc <- read_dta("~/Desktop/physfunc.dta")
View(physfunc)
```
## Explore the data
```r
str(physfunc)
colnames(physfunc)
# "id"   "occ"  "female" "grade"  "age50"  "phf"  
unique(physfunc$occ) # 6 measurement periods
sum(is.na(physfunc)) # there are 18189 NA's in this dataset
summary(physfunc) 
# females make-up about 31% of participants 
# mean age is 6.647 & median age is 6.621
# mean phf is 50.284 & median is 52. 620
```

## Tidy data before moving on to questions
The aim of this exercise is to investigate the following questions: How does women’s physical functioning change as they get older? To what extent does this vary between women? 

So we can create a new dataframe with just women in it.

```r
# create a new data frame with just women
library(magrittr)
library(tidyverse)
library(reshape2)

Women <- physfunc %>% 
    filter(female == 1) %>% 
    na.omit()

View(Women) # only 12451 objects in "Women" data frame

```

I no longer need the gender column, so I removed it and the NAs
```r
Women_2 <- Women %>%
    select(-female) %>% 
    na.omit()

head(Women_2)
# A tibble: 6 x 5
# id   occ     grade    age50      phf
# <dbl> <dbl> <dbl+lbl>    <dbl>    <dbl>
# 1     1     1         2  5.00000 39.59072
# 2     1     2         2  7.00000 38.61111
# 3     1     3         2 10.41615 39.92796
# 4     1     4         2 13.64844 21.91010
# 5     1     5         2 16.62012 25.65662
# 6     2     1         3  6.00000 29.31145


```

## Questions
###1) Obtain summary statistics for phf and age50 at each occasion. 

Wrote function to view only the summary statistics that I care about
```r
install.packages("lavaan")    
library(lavaan)
library(reshape2)

multi.fun <- function(x) {
    c(mean = mean(x), median = median(x), sd = sd(x), var = var(x), min = min(x), 
      max = max(x), range = range(x), quantile = quantile(x))
}

```
ADD SUMMARY HERE - there is probably a better way to do this
```r
occ_1 <- Women_2 %>% 
    filter(occ == "1") %>% 
    select(-id, -occ, - grade) %>% 
    sapply(multi.fun)

occ_1
# age50       phf
# mean            0.3386656 49.856628
# median          0.0000000 52.082150
# sd              6.1466605  8.897158
# var            37.7814357 79.159415
# min           -11.0000000  6.407695
# max            13.0000000 71.575737
# range1        -11.0000000  6.407695
# range2         13.0000000 71.575737
# quantile.0%   -11.0000000  6.407695
# quantile.25%   -5.0000000 45.881485
# quantile.50%    0.0000000 52.082150
# quantile.75%    6.0000000 56.098101
# quantile.100%  13.0000000 71.575737

occ_2 <- Women_2 %>% 
    filter(occ == "2") %>% 
    select(-id, -occ, - grade) %>% 
    sapply(multi.fun)

occ_2
# age50       phf
# mean           3.392399 48.529781
# median         3.000000 51.318520
# sd             6.165760  9.902452
# var           38.016600 98.058552
# min           -8.000000 11.804350
# max           15.000000 69.590912
# range1        -8.000000 11.804350
# range2        15.000000 69.590912
# quantile.0%   -8.000000 11.804350
# quantile.25%  -2.000000 43.620581
# quantile.50%   3.000000 51.318520
# quantile.75%   9.000000 55.685865
# quantile.100% 15.000000 69.590912

occ_3 <- Women_2 %>% 
    filter(occ == "3") %>% 
    select(-id, -occ, - grade) %>% 
    sapply(multi.fun)

occ_3
# age50       phf
# mean           6.2109378 48.512209
# median         5.7262149 51.035210
# sd             6.0263640  9.809208
# var           36.3170625 96.220564
# min           -4.7871318 11.473180
# max           18.4462700 69.627800
# range1        -4.7871318 11.473180
# range2        18.4462700 69.627800
# quantile.0%   -4.7871318 11.473180
# quantile.25%   0.9404526 43.701321
# quantile.50%   5.7262149 51.035210
# quantile.75%  11.6427097 55.430580
# quantile.100% 18.4462700 69.627800

occ_4 <- Women_2 %>% 
    filter(occ == "4") %>% 
    select(-id, -occ, - grade) %>% 
    sapply(multi.fun)

occ_4
# age50        phf
# mean           9.539228  47.856429
# median         9.226563  51.137680
# sd             6.175221  10.397208
# var           38.133359 108.101933
# min           -1.640625   9.355224
# max           21.078131  69.819252
# range1        -1.640625   9.355224
# range2        21.078131  69.819252
# quantile.0%   -1.640625   9.355224
# quantile.25%   4.054688  41.924149
# quantile.50%   9.226563  51.137680
# quantile.75%  15.140630  55.367470
# quantile.100% 21.078131  69.819252

occ_5 <- Women_2 %>% 
    filter(occ == "5") %>% 
    select(-id, -occ, - grade) %>% 
    sapply(multi.fun)

occ_5
# age50       phf
# mean          11.5588791  46.20331
# median        11.0362802  49.27971
# sd             6.1255685  10.64468
# var           37.5225888 113.30925
# min            0.4668045  11.32698
# max           24.0807590  71.85617
# range1         0.4668045  11.32698
# range2        24.0807590  71.85617
# quantile.0%    0.4668045  11.32698
# quantile.25%   6.2402475  40.15645
# quantile.50%  11.0362802  49.27971
# quantile.75%  17.0561218  54.04741
# quantile.100% 24.0807590  71.85617

occ_6 <- Women_2 %>% 
    filter(occ == "6") %>% 
    select(-id, -occ, - grade) %>% 
    sapply(multi.fun)

occ_6
# age50       phf
# mean          14.3613935  46.84055
# median        13.8984404  50.37339
# sd             6.0563290  10.59076
# var           36.6791210 112.16416
# min            0.3046875  11.38633
# max           26.5000000  70.09693
# range1         0.3046875  11.38633
# range2        26.5000000  70.09693
# quantile.0%    0.3046875  11.38633
# quantile.25%   9.1210942  40.46995
# quantile.50%  13.8984404  50.37339
# quantile.75%  19.7031307  54.71719
# quantile.100% 26.5000000  70.09693
# mean age increases and mean phf decreases over time

```
exploratory plots
as expected age increases by occ, and phf decreases slightly
TRY TO MAKE GRADE 3RD DIM

```r
library(ggplot2)
library(knitr)

occ_phf.plot <- ggplot(Women_2, aes(x = occ, y = phf, group = occ, fill = occ)) + geom_boxplot() # fit a line?
plot(occ_phf.plot)
```
```{r echo=FALSE}
library(magrittr)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)

physfunc <- read_dta("~/Desktop/physfunc.dta")

Women <- physfunc %>% 
    filter(female == 1) %>% 
    na.omit()


Women_2 <- Women %>%
    select(-female) %>% 
    na.omit()

occ_phf.plot <- ggplot(Women_2, aes(x = occ, y = phf, group = occ, fill = occ)) + geom_boxplot() # fit a line?
plot(occ_phf.plot)
```

```{r echo=FALSE}
library(magrittr)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)

physfunc <- read_dta("~/Desktop/physfunc.dta")

Women <- physfunc %>% 
    filter(female == 1) %>% 
    na.omit()


Women_2 <- Women %>%
    select(-female) %>% 
    na.omit()

occ_age.plot <- ggplot(Women_2, aes(x = occ, y = age50, group = occ, fill = occ)) + 
    geom_boxplot() # fit a line?
occ_age.plot
```

```{r echo=FALSE}
library(magrittr)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)

physfunc <- read_dta("~/Desktop/physfunc.dta")

Women <- physfunc %>% 
    filter(female == 1) %>% 
    na.omit()


Women_2 <- Women %>%
    select(-female) %>% 
    na.omit()

age_phf.plot <- ggplot(Women_2, aes(x = age50, y = phf, color = occ, fill = occ)) + 
    geom_point() # fit a line?
age_phf.plot
```

```{r echo=FALSE}
library(magrittr)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)

physfunc <- read_dta("~/Desktop/physfunc.dta")

Women <- physfunc %>% 
    filter(female == 1) %>% 
    na.omit()


Women_2 <- Women %>%
    select(-female) %>% 
    na.omit()

ggplot(Women_2, aes(phf)) + geom_histogram()
```

```{r echo=FALSE}
library(magrittr)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)

physfunc <- read_dta("~/Desktop/physfunc.dta")

Women <- physfunc %>% 
    filter(female == 1) %>% 
    na.omit()


Women_2 <- Women %>%
    select(-female) %>% 
    na.omit()

ggplot(Women_2, aes(age50)) + geom_histogram()
```

```{r echo=FALSE}
library(magrittr)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)

physfunc <- read_dta("~/Desktop/physfunc.dta")

Women <- physfunc %>% 
    filter(female == 1) %>% 
    na.omit()


Women_2 <- Women %>%
    select(-female) %>% 
    na.omit()

ggplot(Women_2, aes(x = occ, y = phf, fill = occ)) + geom_bar(stat = "identity")
```

```{r echo=FALSE}
library(magrittr)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)

physfunc <- read_dta("~/Desktop/physfunc.dta")

Women <- physfunc %>% 
    filter(female == 1) %>% 
    na.omit()


Women_2 <- Women %>%
    select(-female) %>% 
    na.omit()

ggplot(Women_2, aes(x = grade, y = phf)) + geom_violin()
```


### 2) Plot physical functioning trajectories by age for the first 5 individuals in the dataset

```r
install.packages("XQuartz", repos = "https://www.xquartz.org/", type="source")
install.packages("/Users/Shea/Downloads/R-3.4.1.tar.gz", repos = NULL, type="source")
install.packages('XQuartz', lib='/Applications/Utilities/XQuartz.app',repos = NULL)

library(Rcmdr)
install.packages("XQuartz")
library(XQuartz)
```

```{r echo=FALSE}
library(Rcmdr)
library(magrittr)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)
library(tidyverse)

physfunc <- read_dta("~/Desktop/physfunc.dta")

Women <- physfunc %>% 
    filter(female == 1) %>% 
    na.omit()

Women_2 <- Women %>%
    select(-female) %>% 
    na.omit()

first_5 <- Women_2[Women_2$id < 14, ]
View(first_5)

ggplot(first_5, aes(x=age50, y=phf, group = id, colour = id)) +
    geom_line()
# generally, as age increases physical function is very decreasing
```

### 3) Fit a simple random effects model for phf with no explanatory variables (i.e. only the constant term). What is the intra-individual correlation? 
```r
install.packages("lme4")
install.packages("arm")
library(lme4)
library(arm)

# think of individual as the random variable because phf and age varies 
# differently from person-to-person over time

# fit model with a random intercept (individual represented by id number in the dataset)
model_1 <- lmer(phf ~ (1 | id), data = Women_2, REML = FALSE)
summary(model_1)

# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: phf ~ (1 | id)
# Data: Women_2
# 
# AIC      BIC   logLik deviance df.resid 
# 87077.6  87099.9 -43535.8  87071.6    12448 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.6796 -0.4190  0.1098  0.5159  3.9693 
# 
# Random effects:
#     Groups   Name        Variance Std.Dev.
# id       (Intercept) 61.2     7.823   
# Residual             41.5     6.442   
# Number of obs: 12451, groups:  id, 2697
# 
# Fixed effects:
#     Estimate Std. Error t value
# (Intercept)  47.8880     0.1637   292.6

```

### 4) Fit a random intercepts growth model which allows an individual’s physical functioning to change linearly with age.
```r
#Fitting the  model with the random intercept
model_2 <- lmer(phf ~ age50+(1 | id), data = Women_2, REML = FALSE)
summary(model_2)

# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: phf ~ age50 + (1 | id)
# Data: Women_2
# 
# AIC      BIC   logLik deviance df.resid 
# 86467.5  86497.2 -43229.8  86459.5    12447 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.5153 -0.4393  0.1078  0.5267  3.7917 
# 
# Random effects:
#     Groups   Name        Variance Std.Dev.
# id       (Intercept) 58.81    7.669   
# Residual             39.43    6.279   
# Number of obs: 12451, groups:  id, 2697
# 
# Fixed effects:
#     Estimate Std. Error t value
# (Intercept)  49.6852     0.1757  282.86
# age50        -0.2655     0.0106  -25.05
# 
# Correlation of Fixed Effects:
#     (Intr)
# age50 -0.409
```
### 5)  Fit a random slopes model that allows the rate of change in physical functioning to vary among individuals. 
```r
Fitting the  model with the random intercept and random slope
model_3 <- lmer(phf ~ age50+(1 | id) + (0 + age50 | id), data = Women_2, REML = FALSE)
summary(model_3)

# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: phf ~ age50 + (1 | id) + (0 + age50 | id)
# Data: Women_2
# 
# AIC      BIC   logLik deviance df.resid 
# 86152.7  86189.8 -43071.3  86142.7    12446 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.3785 -0.4142  0.1063  0.5026  3.5006 
# 
# Random effects:
#     Groups   Name        Variance Std.Dev.
# id       (Intercept) 51.2462  7.1586  
# id.1     age50        0.1395  0.3735  
# Residual             34.8499  5.9034  
# Number of obs: 12451, groups:  id, 2697
# 
# Fixed effects:
#     Estimate Std. Error t value
# (Intercept) 49.83138    0.16725  297.95
# age50       -0.26557    0.01272  -20.88
# 
# Correlation of Fixed Effects:
#     (Intr)
# age50 -0.319

```

### 6) Carry out a likelihood ratio test of the null hypothesis that growth rate is fixed across individuals.
```r
# As I understand, to do a likelihood ratio test, I must compare two models
# I'm comparing two models - model_1 and model_2

anova(model_1, model_2)

# Data: Women_2
# Models:
#     model_1: phf ~ (1 | id)
# model_2: phf ~ age50 + (1 | id)
# Df   AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model_1  3 87078 87100 -43536    87072                             
# model_2  4 86468 86497 -43230    86460 612.09      1  < 2.2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

### 7) Produce a plot of that superimposes predicted phf trajectories from the random intercepts and random slopes models and the observed values of phf for the first 5 individuals.
```r
# initially I made this model, but I could not figure out how to plot it
model_3 <- lmer(phf ~ age50+(1 | id) + (0 + age50 | id), data = Women_2, REML = FALSE)
summary(model_3)

first_5 <- Women_2[Women_2$id < 14, ]
View(first_5)

# could not figure out how to plot only first five women without taking other women out the model - I'm sure this is not right, but the plot works

model_3.2 <- lmer(phf ~ age50+(1 | id) + (0 + age50 | id), data = first_5, REML = FALSE)
summary(model_3.2)

# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: phf ~ age50 + (1 | id) + (0 + age50 | id)
# Data: first_5
# 
# AIC      BIC   logLik deviance df.resid 
# 158.5    164.2    -74.3    148.5       18 
# 
# Scaled residuals: 
#     Min       1Q   Median       3Q      Max 
# -2.26059 -0.22275  0.07196  0.44317  1.41303 
# 
# Random effects:
#     Groups   Name        Variance Std.Dev.
# id       (Intercept) 64.99    8.061   
# id.1     age50        0.00    0.000   
# Residual             20.90    4.572   
# Number of obs: 23, groups:  id, 5
# 
# Fixed effects:
#     Estimate Std. Error t value
# (Intercept)  46.6377     4.0977  11.382
# age50        -0.7863     0.1881  -4.181
# 
# Correlation of Fixed Effects:
#     (Intr)
# age50 -0.404

```r
library(lattice)
library(lme4)

library(ggplot2)
F5_2 <- ggplot(first_5, aes(x = phf, y = age50, group = occ, color = occ)) +
    geom_point(size=3) +
    geom_line(aes(y = predict(model_3.2)),size = 1) 
print(F5_2)
```

```{r echo = FALSE}
library(lattice)
library(lme4)
library(ggplot2)

first_5 <- Women_2[Women_2$id < 14, ]

model_3.2 <- lmer(phf ~ age50+(1 | id) + (0 + age50 | id), data = first_5, REML = FALSE)
summary(model_3.2)


ggplot(first_5, aes(x = phf, y = age50, group = occ, color = occ)) +
    geom_point(size=3) +
    geom_line(aes(y = predict(model_3.2)),size = 1) 
```



### 8)  Fit a quadratic growth model with a fixed coefficient for age-squared. (A model with a random coefficient for age-squared fails to converge.)
```r
install.packages("nlme")
library(nlme)

model_4 <- lme(phf ~ age50 + I (age50^2), random =~ age50 | id, data = Women_2)
summary(model_4)

# Linear mixed-effects model fit by REML
# Data: Women_2 
# AIC      BIC    logLik
# 86134.25 86186.26 -43060.13
# 
# Random effects:
#     Formula: ~age50 | id
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr  
# (Intercept) 7.0762752 (Intr)
# age50       0.3576407 0.063 
# Residual    5.9142383       
# 
# Fixed effects: phf ~ age50 + I(age50^2) 
# Value  Std.Error   DF   t-value p-value
# (Intercept) 49.80392 0.16548872 9752 300.95055       0
# age50       -0.18133 0.01802704 9752 -10.05856       0
# I(age50^2)  -0.00643 0.00099026 9752  -6.48870       0
# Correlation: 
#     (Intr) age50 
# age50      -0.214       
# I(age50^2)  0.012 -0.719
# 
# Standardized Within-Group Residuals:
#     Min         Q1        Med         Q3        Max 
# -4.3644586 -0.4121647  0.1068419  0.5024621  3.4697630 
# 
# Number of Observations: 12451
# Number of Groups: 2697 



# Compare this model to the linear growth model using a likelihood ratio test. 

# in order to compare model_3 and model_4

model_3.2 <- lme(phf ~ age50, random =~ age50 | id, data = Women_2)
summary(model_3.2)

# Linear mixed-effects model fit by REML
# Data: Women_2 
# AIC      BIC    logLik
# 86162.1 86206.68 -43075.05
# 
# Random effects:
#     Formula: ~age50 | id
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev    Corr  
# (Intercept) 7.0854582 (Intr)
# age50       0.3650564 0.053 
# Residual    5.9161572       
# 
# Fixed effects: phf ~ age50 
# Value  Std.Error   DF   t-value p-value
# (Intercept) 49.82151 0.16577436 9753 300.53813       0
# age50       -0.26546 0.01262867 9753 -21.02015       0
# Correlation: 
#     (Intr)
# age50 -0.298
# 
# Standardized Within-Group Residuals:
#     Min         Q1        Med         Q3        Max 
# -4.3825737 -0.4152879  0.1058552  0.5025557  3.5242316 
# 
# Number of Observations: 12451
# Number of Groups: 2697 

anova(model_4, model_3.2)

# Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# model_4       1  7 86134.25 86186.26 -43060.13                        
# model_3.2     2  6 86162.10 86206.68 -43075.05 1 vs 2 29.84889  <.0001
# Warning message:
#     In anova.lme(model_4, model_3.2) :
#     fitted objects with different fixed effects. REML comparisons are not meaningful.

```
