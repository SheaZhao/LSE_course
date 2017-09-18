# import data
library(foreign)
library(haven)
physfunc <- read_dta("~/Desktop/physfunc.dta", header = TRUE, stringsAsFactors = FALSE)
View(physfunc)

# explore data
colnames(physfunc)
    # "id"     "occ"    "female" "grade"  "age50"  "phf"   
summary(physfunc)
# id            occ          female           grade           age50        
# Min.   :   1   Min.   :1.0   Min.   :0.0000   Min.   :1.000   Min.   :-11.000  
# 1st Qu.:2204   1st Qu.:2.0   1st Qu.:0.0000   1st Qu.:1.000   1st Qu.:  1.000  
# Median :4408   Median :3.5   Median :0.0000   Median :2.000   Median :  6.621  
# Mean   :4408   Mean   :3.5   Mean   :0.3129   Mean   :1.888   Mean   :  6.647  
# 3rd Qu.:6612   3rd Qu.:5.0   3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.: 12.000  
# Max.   :8815   Max.   :6.0   Max.   :1.0000   Max.   :3.000   Max.   : 26.734  
# NA's   :7257    

# phf        
# 
# Min.   : 6.408  
# 1st Qu.:47.144  
# Median :52.620  
# Mean   :50.284  
# 3rd Qu.:55.938  
# Max.   :71.856  
# NA's   :10932

# have NA's been dropped - has listwise deletion occured?
sum(is.na(physfunc)) # No - there are 18189 NA's in this dataset

# The aim of this exercise is to investigate the following questions: 
#   • How does women’s physical functioning change as they get older? 
#   • To what extent does this vary between women? 

# create a new data frame with just women in it
library(tidyverse)
library(reshape2)

Women <- physfunc %>%
    filter(female == 1) %>% 
    na.omit()

    View(Women) # only 12451 objects in "women" data frame
    
# Next create a new unique identifier from women, coded 1, 2, . . . n 
    # (where n is the total number of women in the sample). 

    # reshape data by spreading occ variable
    #unique(Women$occ) # 6 occurances
    
    # some variables seem like they should be factor variables instead of numeric
    #factor_variables <- c('occ', 'id', 'grade')
    # Women[factor_variables] <- lapply(Women[factor_variables], function(x) as.factor(x))
    # str(Women)
    
    Women_2 <- Women %>%
        select(-female) %>% 
        na.omit()
    View(Women_2)
    

    
install.packages("lavaan")    
library(lavaan)
library(reshape2)
library(ggplot2)

Women_3 <- dcast(Women_2, id + age50 + grade ~ occ, value.var = "phf")
View(Women_3)        

# 1) Obtain summary statistics for phf and age50 at each occasion. 
summary(Women_3)
    # id           grade           age50               1                2        
    # Min.   :   1   Min.   :1.000   Min.   :-11.000   Min.   : 6.408   Min.   :11.80  
    # 1st Qu.:2162   1st Qu.:2.000   1st Qu.:  1.185   1st Qu.:45.881   1st Qu.:43.62  
    # Median :4496   Median :2.000   Median :  7.000   Median :52.082   Median :51.32  
    # Mean   :4434   Mean   :2.273   Mean   :  7.028   Mean   :49.857   Mean   :48.53  
    # 3rd Qu.:6718   3rd Qu.:3.000   3rd Qu.: 12.579   3rd Qu.:56.098   3rd Qu.:55.69  
    # Max.   :8807   Max.   :3.000   Max.   : 26.500   Max.   :71.576   Max.   :69.59  
    # NA's   :9887     NA's   :10187  
    
    # 3               4                5               6        
    # Min.   :11.47   Min.   : 9.355   Min.   :11.33   Min.   :11.39  
    # 1st Qu.:43.70   1st Qu.:41.924   1st Qu.:40.16   1st Qu.:40.47  
    # Median :51.03   Median :51.138   Median :49.28   Median :50.37  
    # Mean   :48.51   Mean   :47.856   Mean   :46.20   Mean   :46.84  
    # 3rd Qu.:55.43   3rd Qu.:55.367   3rd Qu.:54.05   3rd Qu.:54.72  
    # Max.   :69.63   Max.   :69.819   Max.   :71.86   Max.   :70.10  
    # NA's   :10473   NA's   :10549    NA's   :10574   NA's   :10579 

summary(Women_2)

sapply(Women_3, sd, na.rm = TRUE) # could do this for each summary stat or write a function

multi.fun <- function(x) {
    c(mean = mean(x), median = median(x), sd = sd(x), var = var(x), min = min(x), 
      max = max(x), range = range(x), quantile = quantile(x))
}

multi.fun <- function() {
    c(mean = mean, sd = sd, var = var, min = min, 
      max = max, range = range, quantile = quantile)
}

sumStats <- sapply(Women_2, multi.fun)
sumStats


                        # id        occ     grade      age50        phf
    # mean             4433.884 3.311381 2.2732311   7.028570  48.092942
    # median           4496.000 3.000000 2.0000000   7.000000  50.995361
    # sd               2584.518 1.736723 0.6950466   7.828425  10.063388
    # var           6679734.834 3.016207 0.4830898  61.284243 101.271777
    # min                 1.000 1.000000 1.0000000 -11.000000   6.407695
    # max              8807.000 6.000000 3.0000000  26.500000  71.856171
    # range1              1.000 1.000000 1.0000000 -11.000000   6.407695
    # range2           8807.000 6.000000 3.0000000  26.500000  71.856171
    # quantile.0%         1.000 1.000000 1.0000000 -11.000000   6.407695
    # quantile.25%     2162.000 2.000000 2.0000000   1.185490  43.052280
    # quantile.50%     4496.000 3.000000 2.0000000   7.000000  50.995361
    # quantile.75%     6717.500 5.000000 3.0000000  12.579060  55.272360
    # quantile.100%    8807.000 6.000000 3.0000000  26.500000  71.856171



unique(Women$occ) # 6 occatoins

# as expected age increases by occ, and phf decreases slightly
# try to grade as a thrid dimension 
ggplot(Women_2, aes(x = occ, y = phf, group = occ, fill = occ)) + 
    geom_boxplot() # fit a line?

ggplot(Women_2, aes(x = occ, y = age50, group = occ, fill = occ)) + 
    geom_boxplot() # fit a line?

ggplot(Women_2, aes(x = age50, y = phf, color = "grade")) + geom_point()
    

library(psych)


#### https://www.zoology.ubc.ca/~schluter/R/fit-model/


# 2) Plot physical functioning trajectories by age for the first 5 individuals in the dataset.

first_5 <- head(Women_2, n = 5L)
first_5

ggplot(first_5, aes(x = age50, y =phf)) +
    geom_line() 

plot(first_5)    



# 3) Fit a simple random effects model for phf with no explanatory variables 
    # (i.e. only the constant term). What is the intra-individual correlation?

                # politeness.model = lmer(frequency ~ attitude +
                #    (1|subject) + (1|scenario), data=politeness)
                
                # summary(politeness.model)
                # 
                # library(lme4)
                # re.lm <- lmer(y ~ x + (1+x|unit), data = test.df) 
                # summary(re.lm)
                # 
                # 
                # REM_phf.model <- lmer(frequency ~ phf + (1|id) + (1|scenario), data = Women_2)
                # 
                # 
                # fit <- lm(HEDSCORE ~ 1, data = hedonismdata)
                # summary(fit)
    
    #Fitting the null model without the random intercept 
    fit <- lm(HEDSCORE ~ 1, data = hedonismdata)
    summary(fit)
    
    # write based on below

# 4) Fit a random intercepts growth model which allows an individual’s physical 
    # functioning to change linearly with age. 
   
    #Fitting the null model with a random intercept only (country effect) 
    nullmodel <- lmer(HEDSCORE ~ (1 | country), data = hedonismdata, REML = FALSE)
    summary(nullmodel)
    
# 5)  Fit a random slopes model that allows the rate of change in physical 
    # functioning to vary among individuals. 
    
    #Fitting the  model with the random intercept and random slope
    model2 <- lmer(HEDSCORE ~ AGE+(1| country)+(0+AGE|country) , data = hedonismdata, REML = FALSE)
    summary(model2)
    
# 6) Carry out a likelihood ratio test of the null hypothesis that growth rate 
    # is fixed across individuals. 
    
    # must compare two models
    fm1 <- lmer(y ~ 1 + (1 | subjects), data=data)
    fm2 <- lmer(y ~ 1 + (time | subjects), data=data)
    # also works with lme objects
    anova(fm1, fm2) 
    
# 7) Produce a plot of that superimposes predicted phf trajectories from the 
    # random intercepts and random slopes models and the observed values of phf 
    # for the first 5 individuals. 
    
    
  #  https://cran.r-project.org/web/packages/sjPlot/vignettes/sjplmer.html
    # https://www.youtube.com/watch?v=kCXN7CRYKVo
    
    
    
# 8)  Fit a quadratic growth model with a fixed coefficient for age-squared. 
    # (A model with a random coefficient for age-squared fails to converge.)
    
    # http://web.pdx.edu/~newsomj/mlrclass/ho_quadratic%20growth%20curve%20example.pdf
    # https://stackoverflow.com/questions/42764028/fitting-a-quadratic-curve-in-ggplot
    
    # Compare this model to the linear growth model using a likelihood ratio test. 
    
   # http://lavaan.ugent.be/tutorial/tutorial.pdf
