# Assignment #2

library(tidyverse)
library(haven)


setwd('/Users/tuk12127/Google_Drive/courses/Multivariate/homework/assignment_02/')
getwd()


# I. Select 2 predictor variables and 1 criterion variable

hcp_behav <- read.csv("~/Google_Drive/courses/Multivariate/homework/assignment_01/unrestricted_hspopal_9_9_2019_22_40_57.csv", 
                      row.names = 1, header = TRUE)
hcp_behav$Age <- as.character(hcp_behav$Age)

library(psych)
describe(hcp_behav[c("AngAffect_Unadj", "Friendship_Unadj", "LifeSatisf_Unadj")])

## I.a. Simple bivariate correlation analysis
cor.mat <- cor(hcp_behav[c("AngAffect_Unadj", "Friendship_Unadj", "LifeSatisf_Unadj")], use = "complete.obs",  method = "pearson")
cor.mat
#cor_p <- rcorr(hcp_behav[c("AngAffect_Unadj", "Friendship_Unadj", "LifeSatisf_Unadj")], use = "complete.obs")
#cor_p


## I.b. Compute srs, prs, Bs, and bs by hand

sr_AngAffect = (-0.3497 - 0.3717*-0.2892) / sqrt(1 - (-0.2892)^2)
sr_Friendship = (0.3717 - -0.3497*-0.2892) / sqrt(1 - (-0.2892)^2)

pr_AngAffect = (-0.3497 - 0.3717*-0.2892) / (sqrt(1 - (-0.3497)^2)*sqrt(1 - (-0.2892)^2))
pr_Friendship = (0.3717 - -0.3497*-0.2892) / (sqrt(1 - (0.3717)^2)*sqrt(1 - (-0.2892)^2))

B_AngAffect = (-0.3497 - 0.3717*-0.2892) / (1 - (-0.2892)^2)
B_Friendship = (0.3717 - -0.3497*-0.2892) / (1 - (-0.2892)^2)

b_AngAffect = (B_AngAffect*9.21) / 8.4
b_friendship = (B_Friendship*9.21) / 9.11


## I.c. Compute with R

mr_LAF.1 <- lm(LifeSatisf_Unadj ~ AngAffect_Unadj + Friendship_Unadj, hcp_behav)
summary(mr_LAF.1)

library(olsrr)
ols_correlations(mr_LAF.1)


# II. Hierarchical Regression

cor(hcp_behav[c("AngAffect_Unadj", "Friendship_Unadj", "AngHostil_Unadj", "LifeSatisf_Unadj")], use = "complete.obs",  method = "pearson")

ls_hier_reg.mvs1 <- lm(LifeSatisf_Unadj ~ AngAffect_Unadj, hcp_behav)
ls_hier_reg.mvs2 <- lm(LifeSatisf_Unadj ~ AngAffect_Unadj + Friendship_Unadj, hcp_behav)
ls_hier_reg.mvs3 <- lm(LifeSatisf_Unadj ~ AngAffect_Unadj + Friendship_Unadj + AngHostil_Unadj, hcp_behav)

summary(ls_hier_reg.mvs1)
summary(ls_hier_reg.mvs2)
summary(ls_hier_reg.mvs3)


library(stargazer)

### Test assumptions

#### Residuals Mean of 0
mean(ls_hier_reg.mvs3$residuals)

#### Heteroscedasticity - simultaneous look at plots
par(mfrow=c(2, 2))
plot(ls_hier_reg.mvs3)

#### Predictor and Residuals of Y are not correlated
cor.test(ls_hier_reg.mvs3$model$AngAffect_Unadj, ls_hier_reg.mvs3$residuals)
cor.test(ls_hier_reg.mvs3$model$Friendship_Unadj, ls_hier_reg.mvs3$residuals)
cor.test(ls_hier_reg.mvs3$model$AngHostil_Unadj, ls_hier_reg.mvs3$residuals)

## Complement of LM Assumption Checking
## Most useful for looking at the heteroscedasticity
library(gvlma)
mvn_check <- gvlma(ls_hier_reg.mvs3, alphalevel = 0.000001)
mvn_check

# Create tables
stargazer(ls_hier_reg.mvs1, ls_hier_reg.mvs2, ls_hier_reg.mvs3, type = "text", title = "Life Satisfaction Hierarchical Regression", 
          single.row = T,  out="ls_HMRE.htm")

stargazer(ls_hier_reg.mvs1, ls_hier_reg.mvs2, ls_hier_reg.mvs3, type = "text", title = "Life Satisfaction Hierarchical Regression", 
          single.row = T, ci = T,  out="ls_HMRE-CI.htm")


# Are models significantly different

anova(ls_hier_reg.mvs1, ls_hier_reg.mvs2, ls_hier_reg.mvs3)

coefficients(summary(cl3.mvs1))
coefficients(summary(cl3.mvs2))







