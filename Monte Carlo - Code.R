# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# Monte Carlo Simulation - Assignment 2: Part 2
# Brian Higgins
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# Sections:
# 0. Prep: Load data
# 1. Linear Model 1 : Reduced Model assigned from assignment.
# 2. Linear Model 2 : Using AIC to pick model
# 3. Monte Carlo Simulations : Simulation 1: Reduced Model
# 4. Monte Carlo Simulations : Simulation 2: AIC Model

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# 0. Prep: Load data
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# Load Data
data <- read.csv("STAT8010_2022_assignment2_2022.csv")
attach(data)

# Used to create tables for the report.
library(kableExtra)

# library to let me compare linear Regression models.
# install.packages("AICcmodavg")
library(AICcmodavg)

# Clear workspace
# rm(list=ls())

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# 1. Linear Model 1 : Reduced Model assigned from assignment.
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# 1.1. Linear regression for model 1: Reduced Model
#     1.1.1 Save values for later.
#     1.1.2 Is residuals normal?
#     1.1.3 Seperate out the Betas and the Error
# 1.2. Linear regresion line for model 2

# ------------------------------------------------------
# 1.1 Linear Model 1
# ------------------------------------------------------
# Model 1 is the Reduced Model given in the Assignment 
  
# Linear Model 1:: C02 = B0 + B1 Eng.size +B2 Cylinders + B3 Fuel.cons.comb + B4 Fuel.consmpg + e
lm_model_1 <- lm(CO2.Emissions.g.km.~+Engine.Size.L.+Cylinders+Fuel.Consumption.Comb..L.100.km.+Fuel.Consumption.Comb..mpg., data=data)

# Look at
summary(lm_model_1)

# -----------------------
# 1.1.1 Save values for later
#----------------------

# Save Residensls
lm_model_1$residuals

# Get Error
lm_model_1_mean <- mean(lm_model_1$residuals)
lm_model_1_mean # -3.412043e-15

lm_model_1_sd <- sd(lm_model_1$residuals)
lm_model_1_sd # 18.15186

?sd()

# Save Betas for later Monte Carlo Simulations
md_1_b0 <- coef(lm_model_1)[[1]] # 224.207
md_1_b1 <- coef(lm_model_1)[[2]] # 4.911002
md_1_b2 <- coef(lm_model_1)[[3]] # 6.911625
md_1_b3 <- coef(lm_model_1)[[4]] # 5.679959
md_1_b4 <- coef(lm_model_1)[[5]] # -3.285403

#Save values for Linear Model for comparison later
summary(lm_model_1)[[4]]

lm_model_1_df <- data.frame(summary(lm_model_1)[[4]])
lm_model_1_df <- lm_model_1_df[1:2]

# Save Coefficients and Standard Error in a table for comparing later.
lm_model_1_df %>%
  kbl(caption = "LM Model_1: Coefficent and Sandard Error Results") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# -----------------------
# 1.1.2 Are residuals Normal?
# -----------------------

summary(lm_model_1$residuals)
# Mean is 0 - So passes normality
plot(lm_model_1$residuals, col="black", ylab="", xlab="", main = "Scatter plot of Residuals Normality")
abline(h=0, col=2, lwd=2)

# New to this and this might be wrong but as the data was in in clumps below but slightly
# heavier on the top like, I explored normality with a shaprio Wilks test
# Put in a note: to ask Francisco is there a better way to make a formal decision on 
# normality. 
shapiro.test(lm_model_1$residuals[1:5000])
shapiro.test(lm_model_1$residuals[5001:7385])

# As I was looking it this I read about an issue with a Shapiro Wilks tests that
# as you give it large amounts of data, the chance that the H0 will be reacted
# get s larger. With larger data very small amounts of deviations from normality
# can be detected and the H0 is rejected even though the data can be normal enough
# for our purposes.

#-----------------------------
# Visual look at normality

# looks to be more groups on the bottom but top also looks heavier grouped.
hist(lm_model_1$residuals, xlab="", ylab="", main="Histogram of Residual Normality")
# looks normal, left side is higher but the right side has a small bump in 
# data that looks like it evens out.
# We can assume that the data is normal and contuine.

library(ggpubr)
ggqqplot(lm_model_1$residuals, xlab="", ylab="", main="QQplot to look at Normaility")
# qqplot shows heavily skewed tails on both sides of the data.

# ----------------------------
# I'm still learning and so will ask about this next semester as its not required 
# for this project so will ASSUME normality and make a note to ask next semester.

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# 2. Linear Model 2 : Using AIC to pick model
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# 2.1 Look for best Linear model with AIC
# 2.2 Run Linear Model and taken out results.
  
# -----------------------
# 2.1 Look for best Linear model with AIC
# -----------------------
# Use AIC to find a second model.
# How will it perform with the first model?
  
model1 <- lm(CO2.Emissions.g.km. ~ Fuel.Consumption.City..L.100.km.+Fuel.Consumption.Comb..L.100.km.+Fuel.Consumption.Comb..mpg., data=data)
model2 <- lm(CO2.Emissions.g.km. ~ Engine.Size.L.+Cylinders, data=data)
model3 <- lm(CO2.Emissions.g.km. ~ Engine.Size.L.+Fuel.Consumption.City..L.100.km., data=data)
model4 <- lm(CO2.Emissions.g.km. ~ Engine.Size.L.+Cylinders+Fuel.Consumption.City..L.100.km., data = data)
model5 <- lm(CO2.Emissions.g.km. ~ Engine.Size.L.+Cylinders+Fuel.Consumption.City..L.100.km.,
             Fuel.Consumption.Comb..L.100.km.+Fuel.Consumption.Comb..mpg.+Fuel.Consumption.Hwy..L.100.km., data = data)
model6 <- lm(CO2.Emissions.g.km. ~ Cylinders+Fuel.Consumption.Comb..mpg.+Fuel.Consumption.Hwy..L.100.km., data=data)
model7 <- lm(CO2.Emissions.g.km. ~ Cylinders+Fuel.Consumption.City..L.100.km.+Fuel.Consumption.Comb..L.100.km., data=data)
model8 <- lm(CO2.Emissions.g.km. ~ Cylinders+Fuel.Consumption.Comb..mpg.+Fuel.Consumption.Comb..L.100.km., data=data)
model9 <- lm(CO2.Emissions.g.km. ~ Engine.Size.L., data=data)
model10 <-lm(CO2.Emissions.g.km. ~ Fuel.Consumption.Comb..mpg.)
  
# Future: Look at creating a loop to test each variation, done in a nice way with labels and output to tables.
# Would take a long time and would need to store each value. mmm. Bigger than I just thought.

# Place the models together for comparison  
models_comparision <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)

# Give them names
model_names <- c("Fuel-city.Fuel-comb.Fuel-mpg", "Eng.Cyl", "Eng.Fuel-City","Eng.Cyl.Fuel-City", "All",
                "Cyl.Fuel-Hwy","Cyl.Fuel-mpg.Fuel-Hwy","Cyl.Fuel-City.Fuel-Comb","Eng","Fuel-mpg")

# Compare all models
aictab(cand.set = models_comparision, modnames = model_names)
aic_table <- as.data.frame(aictab(cand.set = models_comparision, modnames = model_names))

# output a table of results
# Save Coefficients and Standard Error in a table for comparing later.
aic_table %>%
  kbl(caption = "AIC Model results") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Reading results:
# k - parameters in models
# AIC - value - Lower is best fitting model and is always lowest first.
# Delta AIC - difference in value between AIC of best model and model being compared.

# It appears that the model with all the predictors is the best model.
# So will use this for second Linear Model

# -----------------------
# 2.2 Run Linear Model and taken out results.
# -----------------------
# Run Linear Regression on Full Model
lm_model_2 <- lm(CO2.Emissions.g.km.~+Engine.Size.L.+Cylinders+Fuel.Consumption.City..L.100.km.
                 + Fuel.Consumption.Hwy..L.100.km.+Fuel.Consumption.Comb..L.100.km.
                 + Fuel.Consumption.Comb..mpg. , data=data)
summary(lm_model_2)
# -----------------------
# 2.3 Save values for later
#----------------------

# Get Error
lm_model_2_mean <- mean(lm_model_2$residuals)
lm_model_2_mean # 2.230737e-16 

lm_model_2_sd <- sd(lm_model_2$residuals)
lm_model_2_sd # 18.13831 # Not much different from model_1

# Save Betas for each predicator
md_2_b0 <- coef(lm_model_2)[[1]]
md_2_b1 <- coef(lm_model_2)[[2]]
md_2_b2 <- coef(lm_model_2)[[3]]
md_2_b3 <- coef(lm_model_2)[[4]]
md_2_b4 <- coef(lm_model_2)[[5]]
md_2_b5 <- coef(lm_model_2)[[6]]
md_2_b6 <- coef(lm_model_2)[[7]]

#Save values for Linear Model for comparison later
summary(lm_model_2)[[4]]

lm_model_2_df <- data.frame(summary(lm_model_2)[[4]])
lm_model_2_df <- lm_model_2_df[1:2]

# Save Coefficients and Standard Error in a table for comparing later.
lm_model_2_df %>%
  kbl(caption = "LM Model_2: Coefficent and Sandard Error Results") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# 3. Monte Carlo Simulations : Simulation 1: Reduced Model
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# ***********Note****************
# simulation results will change each time
# My outputs are for my own investigating
# Results from report will be DIFFERENT and results discussed there.

# Liner Regression Model Given in Assignment Guidelines
# 2.1 lm_model_1: Run a single simulation
# 2.2 lm_model_1: Run 1,000 simulation
# 2.3 lm_model_1: Run 10,000 simulations
# 2.4 Compare results.

# -----------------------
# 3.1 Run a single simulation
# -----------------------
# Run a single Simulation to test.
  
# Size of data
n_data_size <- length(data$Make) # 7385 observations/rows
  
cO2_estimate <- md_1_b0 + md_1_b1* Engine.Size.L. + md_1_b2 * Cylinders +
                md_1_b3* Fuel.Consumption.Comb..L.100.km. + md_1_b4* data$Fuel.Consumption.Comb..mpg. +
                rnorm(n_data_size,0,lm_model_1_sd)
  
  
single_sim_lm = lm(cO2_estimate~Engine.Size.L.+Cylinders+Fuel.Consumption.Comb..L.100.km.+Fuel.Consumption.Comb..mpg., data=data)

# We now have a single observation
single_sim_lm # gives us the output for one Linear regression model

# compare model 1 and a single simulation  
summary(single_sim_lm)
summary(lm_model_1)

# Remove Coefficients 
coef(single_sim_lm) # gives the coefficients

# Remove Standard Error Values from summary
summary(single_sim_lm)$coefficients[, 2]

# -----------------------
# 3.2 Run 1,000 simulations
# -----------------------
# Next lets run 1,000 times.
sim_1_1k_c02_results <- NULL 
sim_1_1k_coef_results <- NULL 
sim_1_1k_std_err_results <- NULL

for (i in 1:1000){
  cO2_estimate <- md_1_b0 + 
    md_1_b1 * Engine.Size.L. + 
    md_1_b2 * Cylinders +
    md_1_b3* Fuel.Consumption.Comb..L.100.km. + 
    md_1_b4* data$Fuel.Consumption.Comb..mpg. +
    rnorm(n_data_size,0,lm_model_1_sd)
  
  single_sim_lm = lm(cO2_estimate~Engine.Size.L.+
                       Cylinders+
                       Fuel.Consumption.Comb..L.100.km.+
                       Fuel.Consumption.Comb..mpg., data=data)
  
  sim_1_1k_c02_results <- c(sim_1_1k_c02_results, mean(cO2_estimate))
  sim_1_1k_coef_results = rbind(sim_1_1k_coef_results, coef(single_sim_lm))
  sim_1_1k_std_err_results = rbind(sim_1_1k_std_err_results, summary(single_sim_lm)$coefficients[, 2])
}

# mean                
mean(sim_1_1k_c02_results) # 250.5761 # Not going to be the same each time obs

# get mean of coefficients
colMeans(sim_1_1k_coef_results)

# get mean of standard Error 
colMeans(sim_1_1k_std_err_results)

# Save results to a data frame and then output to a table
sim_1_df_table <- as.data.frame(cbind(colMeans(sim_1_1k_coef_results),colMeans(sim_1_1k_std_err_results)))
colnames(sim_1_df_table) <- c("Estimate", "Std.Error")

# Make an ouput table
# Save Coefficients and Standard Error in a table for comparing later.
sim_1_df_table %>%
  kbl(caption = "Simulation 1: 1,000 Runs: Coefficent and Sandard Error Results") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# -----------------------
# 3.3 Run 10,000 simulations
# -----------------------
# Next lets run 10,000 times.
sim_1_10k_c02_results <- NULL 
sim_1_10k_coef_results <- NULL 
sim_1_10k_std_err_results <- NULL

for (i in 1:10000){
  cO2_estimate <- md_1_b0 + 
    md_1_b1 * Engine.Size.L. + 
    md_1_b2 * Cylinders +
    md_1_b3* Fuel.Consumption.Comb..L.100.km. + 
    md_1_b4* Fuel.Consumption.Comb..mpg. +
    rnorm(n_data_size,0,lm_model_1_sd)
  
  single_sim_lm = lm(cO2_estimate~Engine.Size.L.+
                       Cylinders+
                       Fuel.Consumption.Comb..L.100.km.+
                       Fuel.Consumption.Comb..mpg., data=data)
  
  sim_1_10k_c02_results <- c(sim_1_10k_c02_results, mean(cO2_estimate))
  sim_1_10k_coef_results = rbind(sim_1_10k_coef_results, coef(single_sim_lm))
  sim_1_10k_std_err_results = rbind(sim_1_10k_std_err_results, summary(single_sim_lm)$coefficients[, 2])
}

# mean                
mean(sim_1_10k_c02_results) # 250.5761 # Not going to be the same each time obs

# get mean of coefficients
colMeans(sim_1_10k_coef_results)

# get mean of standard Error 
colMeans(sim_1_10k_std_err_results)

# Save results to a data frame and then output to a table
sim_1_2_10k_df_table <- as.data.frame(cbind(colMeans(sim_1_10k_coef_results),colMeans(sim_1_10k_std_err_results)))
colnames(sim_1_2_10k_df_table) <- c("Estimate", "Std.Error")

# Make an output table
# Save Coefficients and Standard Error in a table for comparing later.
sim_1_2_10k_df_table %>%
  kbl(caption = "Simulation 1: 10,000 Runs: Coefficent and Sandard Error Results") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# ----------------------------------------------------------------------------
# 3.4. Compare results
# ----------------------------------------------------------------------------
# Comparison of Linear Model and Simulations will be done in the Report.
# As each time the simulation is ran, different results are outputted.
# Please see attached Report.

# 3.4.1 Compare Linear Model 1 Actual Results and Estiamted Co2 values
# 3.4.2 Compare Actual and Estimate Co2 Mean values side by side.
# 3.4.3 Histogram to look at actual and Estiamted Distributed

# -----------------------
# 3.4.1 Compare Linear Model 1 Actual Results and Estiamted Co2 values
# -----------------------

# -----------------------
# Plot Actual Co2 Emissions and Monte Carlo Predication
par(mfrow=c(1, 2)) # side by side plots

# plot Actual Co2Emissions
plot(CO2.Emissions.g.km., type="l", ylim=c(0, 600), col="azure4",
     main="Actual Co2",
     ylab = "Emission values",
     xlab = "Observations")
abline(h=250.6, col=2) # added the mean line value

# Plot simulation 1's Co2Emissions
plot(cO2_estimate, type = "l",ylim=c(0, 600), col="azure4", 
     main="Estimated Co2",
     ylab = "Emission values",
     xlab = "Observations")
abline(h=250.24, col=4) # added the mean line value

par(mfrow=c(1, 1)) # return plots to 1

# -------------------------
# Interesting there is some variation in the min and max but the median and mean
# are almost the same.
summary(data$CO2.Emissions.g.km)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 96.0   208.0   246.0   250.6   288.0   522.0

summary(cO2_estimate)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 35.02  207.49  246.57  250.24  290.28  450.31

# -----------------------
# 3.4.2 Compare Actual and Estimate Co2 Mean values side by side.
# -----------------------

# -------------------
# Look at the mean values for Actual and Estimated Co2 Values

plot(density(data$CO2.Emissions.g.km), col="blue", ylab="", xlab="", main="Actual and Estiated Co2 values")
lines(density(cO2_estimate), col="red")
abline(v=mean(data$CO2.Emissions.g.km), col="blue", lty=2)
abline(v=mean(cO2_estimate), col="red", lty="twodash")
legend("topright", legend=c("Actual Co2", "Estimate Co2"), lty=1, col=c("blue", "red"), cex=0.75)

# Out of curious what if we look at 5 simualitons on the plot
line_1 <- md_1_b0 + md_1_b1 * Engine.Size.L. + md_1_b2 * Cylinders +md_1_b3* Fuel.Consumption.Comb..L.100.km. + md_1_b4* Fuel.Consumption.Comb..mpg. +rnorm(n_data_size,0,lm_model_1_sd)
line_2 <- md_1_b0 + md_1_b1 * Engine.Size.L. + md_1_b2 * Cylinders +md_1_b3* Fuel.Consumption.Comb..L.100.km. + md_1_b4* Fuel.Consumption.Comb..mpg. +rnorm(n_data_size,0,lm_model_1_sd)
line_3 <- md_1_b0 + md_1_b1 * Engine.Size.L. + md_1_b2 * Cylinders +md_1_b3* Fuel.Consumption.Comb..L.100.km. + md_1_b4* Fuel.Consumption.Comb..mpg. +rnorm(n_data_size,0,lm_model_1_sd)
line_4 <- md_1_b0 + md_1_b1 * Engine.Size.L. + md_1_b2 * Cylinders +md_1_b3* Fuel.Consumption.Comb..L.100.km. + md_1_b4* Fuel.Consumption.Comb..mpg. +rnorm(n_data_size,0,lm_model_1_sd)
line_5 <- md_1_b0 + md_1_b1 * Engine.Size.L. + md_1_b2 * Cylinders +md_1_b3* Fuel.Consumption.Comb..L.100.km. + md_1_b4* Fuel.Consumption.Comb..mpg. +rnorm(n_data_size,0,lm_model_1_sd)
line_6 <- md_1_b0 + md_1_b1 * Engine.Size.L. + md_1_b2 * Cylinders +md_1_b3* Fuel.Consumption.Comb..L.100.km. + md_1_b4* Fuel.Consumption.Comb..mpg. +rnorm(n_data_size,0,lm_model_1_sd)
line_7 <- md_1_b0 + md_1_b1 * Engine.Size.L. + md_1_b2 * Cylinders +md_1_b3* Fuel.Consumption.Comb..L.100.km. + md_1_b4* Fuel.Consumption.Comb..mpg. +rnorm(n_data_size,0,lm_model_1_sd)


plot(density(data$CO2.Emissions.g.km), col="blue", ylab="", xlab="", main="Multiple Estimate lines")
lines(density(line_1 ), col="red")
lines(density(line_2 ), col="red")
lines(density(line_3 ), col="red")
lines(density(line_4 ), col="red")
lines(density(line_5 ), col="red")
lines(density(line_6 ), col="red")
lines(density(line_7 ), col="red")

abline(v=mean(data$CO2.Emissions.g.km), col="blue", lty=2)
abline(v=mean(line_1), col="red", lty="twodash")
abline(v=mean(line_2), col="red", lty="twodash")
abline(v=mean(line_3), col="red", lty="twodash")
abline(v=mean(line_4), col="red", lty="twodash")
abline(v=mean(line_5), col="red", lty="twodash")
abline(v=mean(line_6), col="red", lty="twodash")
abline(v=mean(line_7), col="red", lty="twodash")
legend("topright", legend=c("Actual Co2", "Estimate Co2"), lty=1, col=c("blue", "red"), cex=0.75)


# -----------------------
# 3.4.3 Histogram to look at actual and Estimated Distributed
# -----------------------
# --------------------

hist(data$CO2.Emissions.g.km, prob=TRUE, ylab="", xlab="", 
     main="Histogram of Actual Co2 Emissions", col="lightgrey")
lines(density(data$CO2.Emissions.g.km),col="blue")
abline(v=mean(data$CO2.Emissions.g.km), col="blue", lty=2)

hist(cO2_estimate, prob=TRUE,ylab="", xlab="", 
     main="Histogram of Simulated Co2 Emissions", col="lightgrey")
lines(density(cO2_estimate), col="red")
abline(v=mean(cO2_estimate), col="red", lty="dotted")

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# 4. Monte Carlo Simulations : Simulation 2: AIC Model
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# Linear Regression Model created form AIC Results
# 4.1 lm_model_2: Run 1,000 simulation
# 4.2 lm_model_2: Run 10,000 simulations
# 4.3 Compare results.

# -----------------------
# 4.1 Model 2:  Run 1,000 simulations
# -----------------------

# Next lets run 1,000 times.
sim_2_1k_c02_results <- NULL 
sim_2_1k_coef_results <- NULL 
sim_2_1k_std_err_results <- NULL

for (i in 1:1000){
  cO2_estimate <- md_2_b0 + 
    md_2_b1 * Engine.Size.L. + 
    md_2_b2 * Cylinders +
    md_2_b3 * Fuel.Consumption.City..L.100.km. + 
    md_2_b4 * Fuel.Consumption.Hwy..L.100.km. +
    md_2_b5 * Fuel.Consumption.Comb..L.100.km.+
    md_2_b6 * Fuel.Consumption.Comb..mpg.+
    rnorm(n_data_size,0,lm_model_2_sd)
  
  single_sim_lm = lm(cO2_estimate~Engine.Size.L.+
                       Cylinders+
                       Fuel.Consumption.City..L.100.km. + 
                       Fuel.Consumption.Hwy..L.100.km. +
                       Fuel.Consumption.Comb..L.100.km.+
                       Fuel.Consumption.Comb..mpg.,
                       data=data)
  
  sim_2_1k_c02_results <- c(sim_2_1k_c02_results, mean(cO2_estimate))
  sim_2_1k_coef_results = rbind(sim_2_1k_coef_results, coef(single_sim_lm))
  sim_2_1k_std_err_results = rbind(sim_2_1k_std_err_results, summary(single_sim_lm)$coefficients[, 2])
}

# mean                
mean(sim_2_1k_c02_results) # 250.5866

# get mean of coefficients
colMeans(sim_2_1k_coef_results)

# get mean of standard Error 
colMeans(sim_2_1k_std_err_results)

# Save results to a data frame and then output to a table
sim_2_df_table <- as.data.frame(cbind(colMeans(sim_2_1k_coef_results),colMeans(sim_2_1k_std_err_results)))
colnames(sim_2_df_table) <- c("Estimate", "Std.Error")

# Make an output table
# Save Coefficients and Standard Error in a table for comparing later.
sim_2_df_table %>%
  kbl(caption = "Simulation 2: 1,000 Runs: Coefficent and Sandard Error Results") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# -----------------------
# 4.2 Run 10,000 simulations
# -----------------------
# separated out to us on results comparison
cO2_estimate_md_2 <- md_2_b0 + 
  md_2_b1 * Engine.Size.L. + 
  md_2_b2 * Cylinders +
  md_2_b3 * Fuel.Consumption.City..L.100.km. + 
  md_2_b4 * Fuel.Consumption.Hwy..L.100.km. +
  md_2_b5 * Fuel.Consumption.Comb..L.100.km.+
  md_2_b6 * Fuel.Consumption.Comb..mpg.+
  rnorm(n_data_size,0,lm_model_2_sd)


# Next lets run 10,000 times.
sim_2_10k_c02_results <- NULL 
sim_2_10k_coef_results <- NULL 
sim_2_10k_std_err_results <- NULL

for (i in 1:10000){
  cO2_estimate <- md_2_b0 + 
    md_2_b1 * Engine.Size.L. + 
    md_2_b2 * Cylinders +
    md_2_b3 * Fuel.Consumption.City..L.100.km. + 
    md_2_b4 * Fuel.Consumption.Hwy..L.100.km. +
    md_2_b5 * Fuel.Consumption.Comb..L.100.km.+
    md_2_b6 * Fuel.Consumption.Comb..mpg.+
    rnorm(n_data_size,0,lm_model_2_sd)
  
  single_sim_lm = lm(cO2_estimate~Engine.Size.L.+
                       Cylinders+
                       Fuel.Consumption.City..L.100.km. + 
                       Fuel.Consumption.Hwy..L.100.km. +
                       Fuel.Consumption.Comb..L.100.km.+
                       Fuel.Consumption.Comb..mpg.,
                     data=data)
  
  sim_2_10k_c02_results <- c(sim_2_10k_c02_results, mean(cO2_estimate))
  sim_2_10k_coef_results = rbind(sim_2_10k_coef_results, coef(single_sim_lm))
  sim_2_10k_std_err_results = rbind(sim_2_10k_std_err_results, summary(single_sim_lm)$coefficients[, 2])
}

# mean                
mean(sim_2_10k_c02_results) # 250.5761 # Not going to be the same each time obs

# get mean of coefficients
colMeans(sim_2_10k_coef_results)

# get mean of standard Error 
colMeans(sim_2_10k_std_err_results)

# Save results to a data frame and then output to a table
sim_2_2_10k_df_table <- as.data.frame(cbind(colMeans(sim_2_10k_coef_results),colMeans(sim_2_10k_std_err_results)))
colnames(sim_2_2_10k_df_table) <- c("Estimate", "Std.Error")

# output to table
sim_2_2_10k_df_table %>%
  kbl(caption = "Simulation 2: 10,000 Runs: Coefficent and Sandard Error Results") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# ----------------------------------------------------------------------------
# 4.3. Compare results
# ----------------------------------------------------------------------------
# 4.4.1 Compare Linear Model 1 Actual Results and Estimated Co2 values
# 4.4.2 Compare Actual and Estimate Co2 Mean values side by side.
# 4.4.3 Histogram to look at actual and Estimated Distributed

# -----------------------
# 4.4.1 Compare Linear Model 1 Actual Results and 1k Simulation Estimated Co2 values
# -----------------------

# -----------------------
# Plot Actual Co2 Emissions and Monte Carlo Predication
par(mfrow=c(1, 2)) # side by side plots

# plot Actual Co2Emissions
plot(CO2.Emissions.g.km., type="l", ylim=c(0, 600), col="azure4",
     main="Actual Co2",
     ylab = "Emission values",
     xlab = "Observations")
abline(h=250.6, col=2) # added the mean line value

# Plot simulation 1's Co2Emissions
plot(cO2_estimate_md_2, type = "l",ylim=c(0, 600), col="azure4", 
     main="Estiamted Co2",
     ylab = "Emission values",
     xlab = "Observations")
abline(h=250.24, col=4) # added the mean line value

par(mfrow=c(1, 1)) # return plots to 1

# -------------------------
# Interesting there is some variation in the min and max but the median and mean
# are almost the same.
summary(data$CO2.Emissions.g.km)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 96.0   208.0   246.0   250.6   288.0   522.0
summary(cO2_estimate_md_2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 40.37  208.47  247.58  250.58  290.97  464.58

# -----------------------
# 3.4.2 Compare Actual and 1k Simulation Estimate Co2 Mean values side by side.
# -----------------------

# -------------------
# Look at the mean values for Actual and Estimated Co2 Values

plot(density(data$CO2.Emissions.g.km), col="blue", ylab="", xlab="", main="Actual and Estimated Co2 values")
lines(density(cO2_estimate_md_2), col="red")
abline(v=mean(data$CO2.Emissions.g.km), col="blue", lty=2)
abline(v=mean(cO2_estimate_md_2), col="red", lty="dashed")
legend("topright", legend=c("Actual Co2", "Estimate Co2"), lty=1, col=c("blue", "red"), cex=0.75)

# -----------------------
# 3.4.3 Histogram to look at actual and 1k Simulaitons Estimated Distributed
# -----------------------


hist(data$CO2.Emissions.g.km, prob=TRUE, xlab="", ylab="", main = "Histogram of Actual Co2 Emissions")
lines(density(data$CO2.Emissions.g.km),col="blue")
abline(v=mean(data$CO2.Emissions.g.km), col="blue", lty=2)

hist(cO2_estimate_md_2, prob=TRUE, xlab="", ylab="", main = "Histogram of Estimated Co2 Emissions")
lines(density(cO2_estimate), col="red")
abline(v=mean(cO2_estimate), col="red", lty=2)


# -----------------------
# Code test
# -----------------------
print("Code has run to the end")


