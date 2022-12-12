#This is the R script used to reproduce the examples in the lecture.
#Varying intercept model under multi-level model framework.

#install.packages(lme4)
#install.packages(ggplot2)
library(lme4)
library(ggplot2)
library(MuMIn)

#setwd("")

#Read in the data
data = read.csv("Data/voting_2021.csv")

#TX state id: 48
#CA state id: 06
#MA state id: 25

for (j in c(8:22)){
    data[,j] = scale(data[,j])
}

#Re-run model after adding individual and group-level predictors for all the states
model_full <- lmer(formula = new_pct_dem ~ 1
                                + sex_ratio
                                + pct_black
                                + pct_hisp
                                + pct_bach
                                + median_income
                                + pct_65_over
                                + pct_age_18_29
                                + gini
                                + pct_manuf
                                + ln_pop_den
                                + pct_3rd_party
                                + turn_out
                                + pct_fb
                                + pct_uninsured
                                + (1 + sex_ratio
                                     + pct_black
                                     + pct_hisp
                                     + pct_bach
                                     + median_income
                                     + pct_65_over
                                     + pct_age_18_29
                                     + gini
                                     + pct_manuf
                                     + ln_pop_den
                                     + pct_3rd_party
                                     + turn_out
                                     + pct_fb
                                     + pct_uninsured | state),
                            data = data)


summary(model_full)

#Obtain random effects
ranef(model_full)$state
write.csv(ranef(model_full)$state,"re.csv")
write.csv(fixef(model_full),"fe.csv")

#Obtain residuals
residuals(model_full)
#Assign back to data
data$lme_residual = residuals(model_full)

#Export to csv to be imported in pandas
write.csv(data,"data_with_residual.csv")

#Model diagnostics
AIC(model_full)
AICc(model_full)
r.squaredGLMM(model_full)

#Confidence intervals
confint(model_full, oldNames=FALSE)


#Individual-level significance
library(merTools)
randomSims <- REsim(model_full, n.sims = 100000)
fixedSims <- FEsim(model_full, n.sims = 100000)

p = plotREsim(randomSims)$data
write.csv(p,"re_sim.csv")
write.csv(fixedSims,"fe_sim.csv")
