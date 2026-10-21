# ECON7710-HW5
Access knit file here: file:///C:/Users/kateb/Downloads/1135_8_hw5.html

---
title: |
    | ECON 7710 
    | Homework 5: Instrumental Variables 
author:
- "Kate Bloom"
- "Kai Clancy"
- "Dan Mosu"
- "Priyanka Govani"
- "Ben An"
- "Triya Veeramani"
output:
 html_notebook
urlcolor: blue
---

```{r setup, include=FALSE}

# Attach packages
library(tidyverse)     
library(ggplot2)       
library(modelsummary)  
library(vtable)       
library(AER) 
library(estimatr)

# Set global options
knitr::opts_chunk$set(echo = TRUE, eval=TRUE)
```

```{asis, directions=TRUE}

**General instructions** 

The `.Rmd` source for this document will be the template for your homework submission. You must submit your completed assignment as a single html document saved as a PDF file, uploaded to eLC by **11:59p on October 22 ** using the filename `sectiontime_teamnumber_hw1.html` (e.g. `935_1_hw5.html`). 

*Notes*:
  
+ Include the name of each teammate under `author` in the `yaml`. 
+ For questions requiring analytical solutions, you can type them in using markdown math code.  Or, you can submit handwritten solutions, embedding them in the knitted document as clearly readable images.
+ For questions requiring computation, some or all of the required code is included in associated chunks. Modify chunks where and how you are directed.
+ For (almost) all questions about R Markdown, consult [The Definitive Guide](https://bookdown.org/yihui/rmarkdown/).
+ The `setup` chunk above indicates the packages required for this assignment.
+ You will find a description of the variables in the referenced dataset through the Help tab in the Plot pane of RStudio. 
- **Switch `eval` to `TRUE` in the global options command to execute code chunks**.

***
```

In this homework assignment, you will use data from Angrist and Krueger 1991 to estimate the effects of years of schooling on labor market outcomes. This paper uses an indicator for being born in the first quarter of the year as an instrument for years of schooling since students born in the first quarter of the year will have attended fewer years of school when they hit the compulsory schooling age. 


```{r}
data <- read.csv("hw5_data.csv")
str(data)
```
1. First calculate the OLS estimate of the relationship between years of schooling and log wages. Why might you not trust this estimate? Discuss the potential sign of the bias using the OVB formula. 

```{r}
ols <- lm_robust(log_wage ~ education, data = data)
summary(ols)
```

**Answer** 

The OLS estimate is about 0.059. Each additional year of schooling is associated with roughly 5.9% higher wages (robust SE ≈ 0.0003).

I wouldn't trust this as causal because education is not randomly assigned. Let the true model be $\log(wage_i) = \alpha + \beta\, educ_i + \gamma\, ability_i + u_i$. If we omit ability, the OVB formula gives

$$\hat\beta_{OLS} \rightarrow \beta + \gamma \cdot \frac{Cov(educ, ability)}{Var(educ)}.$$

Higher-ability (or more motivated, or better-connected) people tend to get more schooling, so $Cov(educ, ability) > 0$, and they also earn more even at a given level of schooling, so $\gamma > 0$. The bias is therefore **positive** and OLS likely overstates the return to schooling. (Measurement error in schooling would push the other way, via attenuation bias, so the net direction isn't certain, but omitted ability is the usual concern.)

2.	Now, you decide you use an instrumental variables approach and instrument for education using quarter of birth. What assumptions do you need for this to be a valid strategy?

**Answer** 

Let $Z_i$ = born in the first quarter.

1. **Relevance:** $Cov(Z, educ) \neq 0$. Being born in Q1 must actually change years of schooling (the first stage). Compulsory schooling laws make this plausible: Q1 births reach the legal dropout age having completed less schooling.
2. **Exogeneity / independence:** $Z$ is as good as randomly assigned, so it is uncorrelated with the unobserved determinants of wages (ability, family background, etc.): $Cov(Z, u) = 0$.
3. **Exclusion restriction:** $Z$ affects wages *only* through years of schooling, not through any other channel (e.g., age at labor market entry, health, or season-of-birth effects on development).
4. **Monotonicity** (for a LATE interpretation): being born in Q1 weakly lowers schooling for everyone, never raises it. The IV estimate is then the effect for "compliers," those whose schooling is changed by the instrument.

3. Write down and estimate the first stage equation. Report the main coefficient estimate in a sentence. Store your predicted values.   

```{r}
first_stage <- lm(education ~ first_quarter, data = data)
educ_hat <- predict(first_stage)
summary(first_stage)
```

**Answer** 

First stage: $educ_i = \pi_0 + \pi_1\, FirstQuarter_i + v_i$

People born in the first quarter have about 0.10 fewer years of schooling than those born in other quarters ($\hat\pi_1 \approx -0.0996$, SE ≈ 0.008 robust; highly significant).


4. Write down and estimate the reduced form equation. Report the main coefficient estimate in a sentence.  

```{r}
reduced_form <- lm_robust(log_wage ~ first_quarter, data = data)
summary(reduced_form)
```

**Answer** 

Reduced form: $\log(wage_i) = \delta_0 + \delta_1\, FirstQuarter_i + e_i$

People born in the first quarter earn about 0.3% lower wages ($\hat\delta_1 \approx -0.0032$, SE ≈ 0.0017). It is small and only marginally significant at conventional levels.


4. Using your answers in 3) and 4), report the Wald estimate of the relationship between education and wages. 

```{r}
wald <- coef(reduced_form)["first_quarter"] / coef(first_stage)["first_quarter"]
wald
```

**Answer** 

$$\hat\beta_{Wald} = \frac{\hat\delta_1}{\hat\pi_1} = \frac{-0.00323}{-0.09959} \approx 0.032$$

Each additional year of schooling raises wages by about 3.2%, for people whose schooling is affected by quarter of birth.

5. To estimate this using 2SLS, write down the second stage equation. Estimate this using your predicted values from 3).

```{r}
second_stage <- lm(log_wage ~ educ_hat, data = data)
summary(second_stage)
```

**Answer** 

Second stage: $\log(wage_i) = \beta_0 + \beta_1\, \widehat{educ}_i + \epsilon_i$

$\hat\beta_1 \approx 0.0324$, identical to the Wald estimate (with a single binary instrument and no controls, they are algebraically the same). Note the standard errors from this manual second stage (≈ 0.0172) are not correct because they ignore that `educ_hat` is itself estimated; use the `ivreg` standard errors in the next question.

6. Now, use the ivreg command to direct estimate this effect. Compare this estimate to your OLS estimate in 1).

```{r}
iv = ivreg(log_wage ~ education | first_quarter, data = data)
summary(iv)
```

**Answer** 

The 2SLS estimate is 0.0324 (SE ≈ 0.0166), matching the Wald and manual 2SLS point estimates. It is about 45% smaller than the OLS estimate of 0.059. This is consistent with OLS being biased upward by omitted ability, but the IV standard error is about 60 times larger. The 95% CI (roughly 0.000 to 0.065) is wide and includes the OLS estimate, so we can't statistically rule out that OLS is correct.

7. Now, add control variables to your IV regression. How do these change your results? 

```{r}
iv2 = ivreg(log_wage ~ education + married + mid_atlantic + mountain + new_england + year_of_birth |
              first_quarter + married + mid_atlantic + mountain + new_england + year_of_birth,
            data = data)
summary(iv2)
```


**Answer** 

With controls for marital status, region and birth year, the IV estimate rises to about 0.053 (SE ≈ 0.017), compared with 0.032 without controls. It is still below the OLS estimate with the same controls (≈ 0.063). The standard error is roughly unchanged, and the first stage is basically unaffected (≈ -0.093), so the instrument is still relevant. Controls such as year of birth soak up residual variation in wages and in schooling that is correlated with when people were born, which makes the exogeneity assumption more credible. The jump from 0.032 to 0.053 should be read with caution since the standard errors are large (the two estimates are not statistically different).

8. The relationship between quarter of birth and years of schooling should only hold for those who are affected by r compulsory school laws. Estimate a regression that tests whether there is a relationship between quarter of birth and years of schooling for those who have at least a college education (16 years of schooling). What do you find? 

```{r}
data_16 = subset(data, education >= 16)  # "at least" 16 years of schooling
placebo <- lm_robust(education ~ first_quarter, data = data_16)
summary(placebo)
```

**Answer** 

Among people with at least 16 years of schooling, the coefficient on first quarter is small and statistically insignificant (≈ +0.011, SE ≈ 0.007). Using `education > 16` as in the original template gives the same conclusion (≈ +0.009, SE ≈ 0.008). So there is no relationship between quarter of birth and schooling for people who stayed in school well beyond the compulsory schooling age. This is what the story predicts: the effect of quarter of birth on education should operate only through compulsory schooling laws, which don't bind for college-educated people. This "placebo" test supports the interpretation of the first stage and (weakly) the exclusion restriction, since it shows no general quarter-of-birth effect on schooling or related traits that would show up where the laws don't bind.

9.	Someone now tells you that quarter of birth is related to parental income. Does this change how you feel about your IV analysis? 

**Answer** 

Yes, this would undermine the analysis. If quarter of birth is correlated with parental income, the instrument is no longer as good as randomly assigned: $Cov(Z, u) \neq 0$, because parental income affects children's wages directly (through connections, quality of schooling, nutrition, and so on), not only through years of schooling. That violates the exogeneity / exclusion restriction, and the IV estimate would pick up part of the effect of family background, so it would be biased. The direction depends on the sign of the correlation: if Q1 births are from lower-income families, the reduced form would be too negative and the IV estimate would overstate the return to schooling.

Possible fixes: control for parental income if observed (the instrument then only needs to be exogenous conditional on it), find a different instrument, or run robustness checks such as comparing siblings with different birth quarters. The placebo test in 8) would not detect this problem on its own.
