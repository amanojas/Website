---
title: "Example Codes"
author: ""
date: "2022-03-07"
output: 
  html_document: 
    toc: yes
    toc_float: yes
    theme: flatly
    highlight: tango
    keep_md: yes
---
<style type="text/css">
  body{
  font-size: 12pt;
}
</style>



 

```r
packages <- c("tidyverse","stargazer","AER","asbio","tigerstats","readxl","foreign","wooldridge") ## This is how you define an object (which is a vector here)
install.packages(packages, repos='http://cran.us.r-project.org') # Installing packages at once
lapply(packages, library, character.only = T) # Loading the packages
```

## Reading data files into R

Below are some ways to load the data files in different formats. To load excel and Stata data files, you will need to install the packages "readxl" and "foreign" respectively.


```r
bwsmoking_excel <- read_xlsx("S:\\Baruch\\ECO 4000\\Spring2022\\Datasets\\Birthweight and Smoking\\birthweight_smoking.xlsx")

## Read a csv file

bwsmoking_csv<- read_csv("S:\\Baruch\\ECO 4000\\Spring2022\\Datasets\\Birthweight and Smoking\\bwsmoking.csv")

## Read a STATA file (will require package named "foreign")

bwsmoking_stata <- read.dta("S:\\Baruch\\ECO 4000\\Spring2022\\Datasets\\Birthweight and Smoking\\birthweight_smoking.dta")

## Read an RDS file 

bwsmoking_R <- readRDS("S:\\Baruch\\ECO 4000\\Spring2022\\Datasets\\Birthweight and Smoking\\bwsmoking.rds")
```


## One Sample Hypothesis Testing
These examples are from the practice problem set provided to the students. 

### Problem 9
**A company claims that its soup machines deliver exactly 10.0 ounces of soup—no more, no less. A researcher samples 100 bowls of soup and finds that:**

$\overline{X}$= 10.28 ounces\
s = 1.20 ounces\
Test the company’s claim at 5% and 1% significance level. 

We denote null hypothesis as $H_{0}$ and alternative hypothesis as $H_{1}$.

$$H_{0} :\mu = 10$$
$$H_{1} :\mu \neq 10$$

We define z-statistic in terms of the sample mean, sample size, and the population standard deviation $\sigma$

$z = \frac{\overline{X} - \mu_{0}}{\bigg(\frac{\sigma}{\sqrt{n}}\bigg)}$ \

We reject the null hypothesis if $|z| \geq |z_{\frac{\alpha}{2}}|$, where $|z|$ is the absolute value of the z-statistic we calculated and $|z_{\frac{\alpha}{2}}|$ is the critical value we obtain from the table. Notice that we use $|z_{\frac{\alpha}{2}}|$ here because we are conducting a two-sided test. \

We reject the null hypothesis if p-value $\leq \frac{\alpha}{2}$, where $\alpha$ is a significance level.(we will often use 0.01,0.05,and 0.1 values for $\alpha$ for our purposes.)



```r
# A function to calculate z scores and compare them with critical values of z (ones we obtain from the table)
z_test <- function(mu,x_bar,s,n,alpha,alternative){
  sd = s/sqrt(n)
  z <- (x_bar - mu)/(sd)
  if(alternative == "two-sided"){
    z_alpha = qnorm((1-alpha/2)) 
  }
  else if(alternative == "less"){
  z_alpha = -qnorm((1-alpha))
  }
  else if(alternative == "greater"){
    z_alpha = qnorm((1-alpha))
  }
  else("NA")
  
  z_list = list("|z_calculated|" = abs(z), "|z_critical|" = abs(z_alpha))
  return(z_list)
}
```

```r
set.seed(21)
Q_9 <- tibble(X = rnorm(mean = 10.28, sd = 0.12, n = 100)) # Our simulated data
z_05 <- z_test(10,10.28,s = 1.2,100,0.05, alternative = "two-sided") ## a two-sided test at 5 % significance level
z_05
```

```
## $`|z_calculated|`
## [1] 2.333333
## 
## $`|z_critical|`
## [1] 1.959964
```

```r
t.test(x = Q_9$X, mu = 10, alternative = "two.sided")
```

```
## 
## 	One Sample t-test
## 
## data:  Q_9$X
## t = 23.422, df = 99, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 10
## 95 percent confidence interval:
##  10.26424 10.31316
## sample estimates:
## mean of x 
##   10.2887
```

As we notice here, the absolute value of z-calculated = 2.33 is greater than the absolute value of z-critical = 1.96. Therefore, we reject the null hypothesis being $\mu = 10$


```r
z_01 <- z_test(10,10.28,s = 1.2,100,0.01, alternative = "two-sided") ## a two-sided test at 1 % significance level
z_01
```

```
## $`|z_calculated|`
## [1] 2.333333
## 
## $`|z_critical|`
## [1] 2.575829
```

As we notice above, the absolute value of z-calculated = 2.33 is less than the absolute value of z-critical = 2.58. Therefore,we fail reject the null hypothesis being $\mu = 10$

### Problem - 6

**A manufacturer produces drill bits with an intended life of at least 580 hours and a standard deviation of 30 hours. A quality control scientist draws a sample of 100 bits and finds $\overline{X}=577$. Test at $\alpha =.05$ to see if the machinery needs adjusting.**


$$H_{0} :\mu \geq 580$$
$$H_{1} :\mu < 580$$

![](Example_Codes_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



```r
set.seed(121)
Q_6 <- tibble(X = rnorm(mean = 577, sd = 30, n = 100)) # dataset with N ~ (577,900) (it does not have to be normal though. Recall Central Limit Theorem!)
z_one_sided <- z_test(mu = 580, x_bar = 577, s = 30, n = 100, alpha = 0.05, alternative = "less")
z_one_sided
```

```
## $`|z_calculated|`
## [1] 1
## 
## $`|z_critical|`
## [1] 1.644854
```

As we notice above, the absolute value of z-calculated = 1 is less than the absolute value of z-critical = 1.64. Therefore,we fail reject the null hypothesis being $\mu = 580$.


### Problem - 4

**A drug company that manufactures a diet drug claims that those using the drug for 30 days will lose at least 15 pounds. You sample 30 people who have used the drug and find that the average weight loss was 12 pounds with a standard deviation of 5 pounds. (Hint : When sample is small enough i.e. $n \leq 30$ use a t-test). Test the claim at the .05 significance level. **

$$H_{0} :\mu \geq 15$$
$$H_{1} :\mu < 15$$


```r
t_test <- function(mu,x_bar,s,n,alpha,alternative){
  sd = s/sqrt(n)
  t <- (x_bar - mu)/(sd)
  if(alternative == "two-sided"){
    t_alpha = qt(alpha/2,df = n-1)
  }
  else if(alternative == "less"){
    t_alpha = - qt(1-alpha, df= n-1)
  }
  else if(alternative == "greater"){
    t_alpha = qt(1-alpha, df= n-1)
  }
  else("NA")
  
  t_list = list("|t_calculated|" = abs(t), "|t_critical|" = abs(t_alpha))
  return(t_list)
}


t_one_sided <- t_test(15,12,s = 5,n=30,0.05,alternative = "less")

t_one_sided
```

```
## $`|t_calculated|`
## [1] 3.286335
## 
## $`|t_critical|`
## [1] 1.699127
```

```r
z_one_sided <- z_test(15,12,s = 5,n=30,0.05,alternative = "less")
z_one_sided
```

```
## $`|z_calculated|`
## [1] 3.286335
## 
## $`|z_critical|`
## [1] 1.644854
```

```r
pvalt <- 2 * pt(-t_one_sided$`|t_calculated|`,29) # p value calculation
pvalt
```

```
## [1] 0.002658998
```
As we notice above, the absolute value of t-calculated = 3.29 is greater than the absolute value of t-critical = 1.69. Therefore,we reject the null hypothesis being $\mu = 15$.

p-value here is 0.002 which is less than 0.05. Therefore,we reject the null hypothesis being $\mu = 15$. 



## Monte Carlo Simulation to Confirm the Unbiasedness of Slope Parameter


```r
## POPULATION PARAMETERS
B0 = 2
B1 = 0.5




n = 10000

coeffs <- tibble(b_0 = rep(0,n),b_1 = rep(0,n))

for (i in 1:n) {
  
  dat1 <- tibble(X = 1:50, u = rnorm(50), Y = B0 + B1*X + u)
  reg <- lm(Y~X, data = dat1)
  model <- summary(reg)
  
  coeffs$b_0[i] = model$coefficients[1,1]
  coeffs$b_1[i] = model$coefficients[2,1]
  
}

### With increased error

coeffs_2 <- tibble(b_02 = rep(0,n),b_12 = rep(0,n))

for (i in 1:n) {
  
  dat2 <- tibble(X = 1:50, u = rnorm(50), Y = B0 + B1*X + 2*u)
  reg2 <- lm(Y~X, data = dat2)
  model2 <- summary(reg2)
  
  coeffs_2$b_02[i] = model2$coefficients[1,1]
  coeffs_2$b_12[i] = model2$coefficients[2,1]
  
}


##### Plot the data

ggplot(data = coeffs, aes(x = b_1), color = "blue")+
  geom_density()+
  geom_density(data = coeffs_2, aes(x = b_12), color = "green")+
  xlab("beta_hat")
```

![](Example_Codes_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Regression Analysis Interpretation Examples

For this exercise we will require to install r package "wooldridge". This package contains all the data that are used in [Wooldridge's Introductory Econometrics](https://www.cengage.com/c/introductory-econometrics-a-modern-approach-6e-wooldridge/9781305270107/) textbook. We will use those data sets for our purpose.

**E1. For the population of chief executive officers, let Y be annual salary (salary) in thousands of dollars.Thus, y = 856.3 indicates an annual salary of $856,300, and y = 1,452.6 indicates a salary of \$1,452,600. Let X be the average return on equity (roe) for the CEO’s firm for the previous three years. (Return on equity is defined in terms of net income as a percentage of common equity.) For example, if roe = 10, then average return on equity is 10%.**


```r
load("S:\\Baruch\\ECO 4000\\Spring2022\\Datasets\\ceosal1.RData")
ceo1 <- data

## Summary Statistics

stargazer(ceo1, type = "html",digits = 2)
```


<table style="text-align:center"><tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Statistic</td><td>N</td><td>Mean</td><td>St. Dev.</td><td>Min</td><td>Max</td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">salary</td><td>209</td><td>1,281.12</td><td>1,372.35</td><td>223</td><td>14,822</td></tr>
<tr><td style="text-align:left">pcsalary</td><td>209</td><td>13.28</td><td>32.63</td><td>-61</td><td>212</td></tr>
<tr><td style="text-align:left">sales</td><td>209</td><td>6,923.79</td><td>10,633.27</td><td>175.20</td><td>97,649.90</td></tr>
<tr><td style="text-align:left">roe</td><td>209</td><td>17.18</td><td>8.52</td><td>0.50</td><td>56.30</td></tr>
<tr><td style="text-align:left">pcroe</td><td>209</td><td>10.80</td><td>97.22</td><td>-98.90</td><td>977.00</td></tr>
<tr><td style="text-align:left">ros</td><td>209</td><td>61.80</td><td>68.18</td><td>-58</td><td>418</td></tr>
<tr><td style="text-align:left">indus</td><td>209</td><td>0.32</td><td>0.47</td><td>0</td><td>1</td></tr>
<tr><td style="text-align:left">finance</td><td>209</td><td>0.22</td><td>0.42</td><td>0</td><td>1</td></tr>
<tr><td style="text-align:left">consprod</td><td>209</td><td>0.29</td><td>0.45</td><td>0</td><td>1</td></tr>
<tr><td style="text-align:left">utility</td><td>209</td><td>0.17</td><td>0.38</td><td>0</td><td>1</td></tr>
<tr><td style="text-align:left">lsalary</td><td>209</td><td>6.95</td><td>0.57</td><td>5.41</td><td>9.60</td></tr>
<tr><td style="text-align:left">lsales</td><td>209</td><td>8.29</td><td>1.01</td><td>5.17</td><td>11.49</td></tr>
<tr><td colspan="6" style="border-bottom: 1px solid black"></td></tr></table>

```r
## plot the data 
p1 <- ggplot(data = ceo1, aes(x = roe, y = salary)) +
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Return on Equity")+
  ylab("Salary")

p2 <- ggplot(data = ceo1, aes(x = roe, y = lsalary)) +
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Return on Equity")+
  ylab("Log Salary")

gridExtra::grid.arrange(p1,p2)
```

![](Example_Codes_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
model_1 <- lm(data = ceo1, salary ~ roe)
stargazer(model_1, type = "html",notes.append = FALSE,notes = c("<sup>&sstarf;</sup>p<0.1; <sup>&sstarf;&sstarf;</sup>p<0.05; <sup>&sstarf;&sstarf;&sstarf;</sup>p<0.01"))
```


<table style="text-align:center"><tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>salary</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">roe</td><td>18.501<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(11.123)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>963.191<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(213.240)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>209</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.013</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.008</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>1,366.555 (df = 207)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>2.767<sup>*</sup> (df = 1; 207)</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>&sstarf;</sup>p<0.1; <sup>&sstarf;&sstarf;</sup>p<0.05; <sup>&sstarf;&sstarf;&sstarf;</sup>p<0.01</td></tr>
</table>

