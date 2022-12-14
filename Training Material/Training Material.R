# HARMONY CA18208 Training School 

# Introduction to Bayesian Latent Class models for health scientists


# 23 - 25 November 2022
# Valencia, Spain


# Set your working directory, with the setwd() function in the folder that is in the material you downloaded.

####### 
# Exercise 1 – True prevalence estimation model

# Before we apply the 2-test 2-population (Hui-Walter model) 
# let’s start with running a basic JAGS model in R. 
# This model is the so called “true prevalence estimation” model, 
# where we estimate the true prevalence of infection adjusting for test imperfection.


# 1.	Open a new .txt file in the R session (File -> New File -> Text File)
# 2.	Write the model in new text file and	save this file as “tp_model.bug”

# You can skip this step, because the tp_model.bug file is is already in your working directory.

# 3.	Attach the “runjags” package 

library(runjags)

# 4.	Define the required input data:
n <- 4072
y <- 1220

# 5.	Define the initial data for the MCMC chains:
tp <- list(chain1=0.05, chain2=0.95)
Se <- list(chain1=0.05, chain2=0.95)
Sp <- list(chain1=0.05, chain2=0.95)

# 6.	 Run the model
results <- run.jags('tp_model.bug', n.chains=2, burnin=5000, sample=10000)

# 7.	 Plot and view the results
plot(results) # click backwards to view all plots
results
summary(results)


####### 
# Exercise 2 – The Hui-Walter model

# In this session we will apply a Bayesian Latent class model to the data/observations 
# from the manuscript published back in 1980 (Table 1 in the Exercise 2 Word document). 

# 1.	Add population 1 of table 1 in your working directory, with the following lines of code. 
pop_1 = matrix(nrow=3,ncol=3)
rownames(pop_1) = c("Mantoux_Test_Pos", "Mantoux_Test_Neg", "Total")
colnames(pop_1) = c("Tine_Test_Pos", "Tine_Test_Neg", "Total")

pop_1[1,1] = 14
pop_1[1,2] = 4
pop_1[2,1] = 9
pop_1[2,2] = 528
#Total rows and columns
pop_1[1,3] = pop_1[1,1] + pop_1[1,2]
pop_1[2,3] = pop_1[2,1] + pop_1[2,2]
pop_1[3,1] = pop_1[1,1] + pop_1[2,1]
pop_1[3,2] = pop_1[1,2] + pop_1[2,2]
N_1 = sum(pop_1[1,1] + pop_1[1,2] + pop_1[2,1] + pop_1[2,2])
pop_1[3,3] = N_1
pop_1


# 2.	Do the same with pop_2
pop_2 = matrix(nrow=3,ncol=3)
rownames(pop_2) = c("Mantoux_Test_Pos", "Mantoux_Test_Neg", "Total")
colnames(pop_2) = c("Tine_Test_Pos", "Tine_Test_Neg", "Total")

pop_2[1,1] = 887
pop_2[1,2] = 31
pop_2[2,1] = 37
pop_2[2,2] = 367
#Total rows and columns
pop_2[1,3] = pop_2[1,1] + pop_2[1,2]
pop_2[2,3] = pop_2[2,1] + pop_2[2,2]
pop_2[3,1] = pop_2[1,1] + pop_2[2,1]
pop_2[3,2] = pop_2[1,2] + pop_2[2,2]
N_2 = sum(pop_2[1,1] + pop_2[1,2] + pop_2[2,1] + pop_2[2,2])
pop_2[3,3] = N_2
pop_2


# 3.	Write the 2_test_2_population model and save as “hw_blca.bug”.

# You can skip this step, because the hw_blca.bug file is is already in your working directory.

# 4.	Attach the “runjags” package
library(runjags)

# 5.	Specify the data in R environment

Population_1 <- as.numeric(pop_1[1:2,1:2])
Population_2 <- as.numeric(pop_2[1:2,1:2])

# 6.	Define the initial values data for the MCMC chains:
  
prev <- list(chain1=c(0.05,0.99), chain2=c(0.95,0.05))
se <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))
sp <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))

# 7.	Run the model
results <- run.jags("hw_blca.bug", n.chains=2, burnin=5000, sample=10000)

# 8.	Plot and view the results

summary(results)
pt <- plot(results) 
pt$`prev[1].plot1`
pt$`prev[1].plot3`

print(pt[["prev[1].plot1"]])
print(pt[["se[1].plot1"]])
print(pt[["sp[1].plot1"]])
print(pt[["sp[1].plot3"]])

####### 
# Exercise 3 – Multi-test Multi-populations Hui-Walter models

# Now let’s move to the 2-tests - 5-populations setting.

# 1.	Encode the table from the Exercise 3 Word document in your R working directory, as a matrix and save it as a variable named Cross_Classified.
# 2nd Option
# Load the ex3.csv file into the working directory
data = read.csv("ex3.csv", sep=";", header=TRUE)
Cross_Classified=array(,dim=c(4,5))
Cross_Classified[1:4,1] = data[1:4,2] 
Cross_Classified[1:4,2] = data[1:4,3] 
Cross_Classified[1:4,3] = data[1:4,4] 
Cross_Classified[1:4,4] = data[1:4,5] 
Cross_Classified[1:4,5] = data[1:4,6] 

# 2.	Write the 2_test_5_population model and save as “hw2_blca.bug”.

# You can skip this step, because the hw2_blca.bug file is is already in your working directory.

# 3.	Attach the “runjags” package
library(runjags)

# 4.	Specify the data in R environment
Populations <- 5
TotalTests <- array(,dim = c(1,5))
TotalTests[1] <- sum(Cross_Classified[1:4,1])
TotalTests[2] <- sum(Cross_Classified[1:4,2])
TotalTests[3] <- sum(Cross_Classified[1:4,3])
TotalTests[4] <- sum(Cross_Classified[1:4,4])
TotalTests[5] <- sum(Cross_Classified[1:4,5])
TotalTests = as.numeric(TotalTests)

# 5.	Define the initial values data for the MCMC chains:
  
prev <- list(chain1=c(0.1, 0.1, 0.1, 0.9, 0.9), chain2=c(0.9, 0.9, 0.9, 0.1, 0.1))
se <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))
sp <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))

# 6.	Run the model

results <- run.jags("hw2_blca.bug", n.chains=2, burnin=5000, sample=10000)

# 7.	Plot and view the results

summary(results)
pt <- plot(results) 
pt$`prev[1].plot1`
pt$`prev[1].plot3`

print(pt[["prev[1].plot1"]])
print(pt[["se[1].plot1"]])
print(pt[["sp[1].plot1"]])
print(pt[["sp[1].plot3"]])

# 8.	Apply the model to 3 populations instead of 5 and see if there is any change to the results.

# Let's apply the model to the first 3 populations - 2 test | 3 population setting
# The model is the same, we need to adjust the data only
Cross_Classified = Cross_Classified[1:4,1:3]

library(runjags)

# Specify the data in R environment
Populations <- 3
TotalTests <- array(,dim = c(1,3))
TotalTests[1] <- sum(Cross_Classified[1:4,1])
TotalTests[2] <- sum(Cross_Classified[1:4,2])
TotalTests[3] <- sum(Cross_Classified[1:4,3])
TotalTests = as.numeric(TotalTests)

# Define the initial values data for the MCMC chains:

prev <- list(chain1=c(0.1, 0.1, 0.9), chain2=c(0.9, 0.9, 0.1))
se <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))
sp <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))

# Run the model
results_3p <- run.jags("hw2_blca.bug", n.chains=2, burnin=5000, sample=10000)

# Plot and view the results

summary(results_3p)
pt <- plot(results_3p) 
pt$`prev[1].plot1`
pt$`prev[1].plot3`

print(pt[["prev[1].plot1"]])
print(pt[["se[1].plot1"]])
print(pt[["sp[1].plot1"]])
print(pt[["sp[1].plot3"]])

####### 
# Exercise 4 - Bayesian Latent Class for continuous tests

# Explain the question - Mixture Normal model
# YouTube video - https://www.youtube.com/watch?v=1gQ-lLKl5CQ&ab_channel=PolychronisKostoulas

# Point 1
# ▶ In the absence of a gold standard the true disease status of each individual is unknown.
# ▶ The available information we have is the continuous test output from each individual.

# Point 2
# ▶ Most studies dichotomize the continuous test output based on a pre-selected cut-off value and apply the BLCMs we discussed yesterday.
# ▶ But this results in loss of valuable information.
# ▶ All positive results are equal, no matter how near or far they are from the cutoff.

## Mixture Normal Model explained 
# Load the model in your working directory as continuous_test
continuous_test <- "model {
  for (i in 1:481) {
	    #S[i] diagnostic test value for ith individual
	    S[i] ~ dnorm(mu[i],tau[i])
  
	    #Value of mu & tau depending on the group (diseased or disease-free)
	    mu[i] <- lambda[T[i]]           
	    tau[i] <- gamma[T[i]]
	    #dcat <- categorical #D(-) if T[i]=1, D(+) if T[i]=2
	    T[i] ~ dcat(P[]) 
  }
	P[1:2] ~ ddirch(alpha[])

	# lambda[1]-gamma[1] mean-precision of non-disease group   
	lambda[1] ~ dnorm(0,0.001)
	lambda[2] ~ dnorm(0,0.001)T(lambda[1],)  # for identifiability we assume that infected individuals have higher scores than healthy individuals
	gamma[1] ~ dgamma(0.001,0.001)
	gamma[2] ~ dgamma(0.001,0.001)  

	# variance = 1/precision(tau)
	sigma[1] <- 1/gamma[1] 	
	sigma[2] <- 1/gamma[2]	

	# AUC
	AUC <- phi(-(lambda[1]-lambda[2])/sqrt(sigma[2]+sigma[1]))
	# ROC curve
	for(i in 1:111) {
	  c1[i] <-  ((-8.1+0.1*i)-lambda[2])/sqrt(sigma[2]) # grid is from -3 to 8
	  se[i] <- 1-phi(c1[i])
    c2[i] <-  ((-8.1+0.1*i)-lambda[1])/sqrt(sigma[1])
	  sp[i] <- phi(c2[i])
	  
	  Y[i] <- se[i] + sp[i] - 1
	}
	#data# alpha, S
	#inits# lambda, gamma
	#monitor# AUC, se, sp, P, lambda, gamma, sigma
}
"

# Data - Initial Values
alpha <- c(1,1) # Dirichlet parameter
S <- c(-1.09,-0.69,-1.26,-0.54,-2.09,-0.76,0.13,-1.86,-0.21,-1.48,-0.02,-0.17,-1.19,-1.51,-0.92,0.21,-1.01,-1.97,-1.02,-0.06,-1.26,-2.34,-0.29,-2.44,-1.03,-1.97,-0.22,-1.45,
       -2.53,-2.03,-1.90,-2.27,0.81,0.17,-1.45,-0.56,-0.32,-0.02,-2.03,-2.21,-1.29,-1.31, 0.22,0.17,0.37,-3.06,-0.33,-0.97,0.84,-0.03,-2.66,0.01,-1.39,0.81,-0.45,-0.87,
       0.21,0.46,-0.53,-0.29,0.51,-0.56,-0.67,-0.54,-0.51,-0.99,-0.21,-2.04,-1.97,-0.16,
       0.43,-1.77,0.19,-1.08,-2.30,-1.66,-0.78,-0.69,-2.41,-2.41,-1.51,-2.53,-0.33,0.20,
       -0.24,-2.53,-0.80,-2.12,-2.98,-2.90,-2.66,-2.75,-2.83,-2.83,-2.92,-2.98,-2.88,-2.55,-2.78,-2.80,-2.94,-2.98,-3.02,-2.83,-2.86,-2.98,-2.88,-2.63,-2.94,-2.48,-2.86,-2.88,-2.78,-2.98,-2.80,-2.94,-2.60,-2.63,-2.92,-2.81,-2.62,-2.86,-2.83,-2.81,-2.78,-2.76,-2.63,-1.98,-2.81,-2.28, -2.69,-2.60,-2.31,-2.63,-2.70,-2.88,-2.55,-2.60,-2.45,-2.73,-2.78,-1.81,-2.69,-2.51,-2.72,-2.67,-1.53,-2.78,-2.65,-2.17,-2.36,-2.75,-2.49,-2.50,-2.59,-2.69,-2.20,-2.40,-2.23,-2.12,-2.65,-2.44,-2.67,-2.55,-2.56,-2.23,-2.70,-2.24,-2.44,-1.93,-2.28,-2.72,-2.25,-2.44,-2.66,-2.65,-2.70,-2.55,-2.30,-2.67,-2.73,-2.40,-3.06,-2.92,-3.06,-3.06, -3.02,-2.86,-2.94,-3.00,-2.88,-2.88,-3.08,-2.92,-3.06,-2.90,-2.92,-3.04,-3.10,-2.96, -3.02,-3.08,-2.88,-2.92,-2.96,-3.08,-2.94,-3.00,-3.04,-2.67,-2.98,-3.02,-3.00,-3.04,
       -2.44,-3.08,-3.02,-2.98,-2.94,-2.90,-2.47,-3.06,-3.06,-3.04,-1.87,-2.14,-2.45,-2.62,
       -2.63,-2.19,-2.54,-2.67,-2.54,-2.27,-2.73,-2.50,-2.60,-2.62,-2.62,-2.40,-2.59,-2.35,
       -2.44,-2.48,-2.58,-2.55,-2.75,-2.73,-2.80,-2.49,-2.51,-2.66,-2.60,-2.60,-2.67,-2.43,
       -2.73,-2.59,-2.67,-2.49,-2.54,-2.23,-2.49,-2.11,-2.59,-2.41,-2.51,-2.75,-2.81,-2.38,
       -2.47,-2.45,-2.75,-2.96,-2.83,-2.98,-2.90,-2.90,-2.92,-2.98,-2.92,-2.60,-2.98,-2.96,
       -2.92,-2.94,-2.81,-3.00,-2.54,-2.67,-2.96,-2.83,-2.90,-2.02,-2.70,-2.81,-1.97,-2.69,
       -2.30,-2.80,-2.90,-2.67,-2.86,-2.88,-2.75,-2.65,-2.80,-2.73,-2.76,-2.73,-2.78,-2.76,
       -2.83,-2.53,-2.85,-2.78,-2.85,-3.08,-3.10,-3.08,-3.08,-3.02,-3.08,-2.80,-2.98,-3.04,
       -3.02,-3.04,-3.08,-3.02,-2.86,-2.98,-3.00,-3.08,-3.08,-2.98,-3.06,-3.04,-3.06,-3.02,
       -3.02,-3.08,-2.83,-3.08,-3.08,-2.90,-2.98,-3.06,-2.94,-2.98,-3.06,-2.85,-3.02,-3.00,
       -3.06,-3.06,-3.08,-2.92,-3.10,-3.04,-3.08,-2.94,-3.10,-3.00,-2.98,-3.06,-2.98,-3.06,
       -3.04,-3.08,-2.65,-2.85,-3.00,-3.04,-2.51,-2.85,-2.63,-2.90,-2.56,-2.67,-2.78,-2.65,
       -2.67,-2.76,-2.78,-2.50,-2.32,-2.54,-2.56,-2.47,-2.73,-2.44,-2.76,-2.76,-2.54,-2.62,
       -2.54,-2.83,-2.65,-2.55,-2.59,-2.75,-2.66,-2.86,-2.69,-2.75,-2.75,-2.72,-2.15,-2.63,
       -2.80,-2.63,-2.48,-2.47,-2.34,-2.63,-2.50,-2.36,-2.75,-2.51,-2.30,-2.70,-2.69,-2.65,
       -2.40,-2.81,-3.00,-2.83,-2.81,-2.90,-2.86,-2.81,-2.25,-2.88,-3.00,-2.90,-2.81,-2.88,
       -2.80,-2.85,-2.98,-2.80,-2.83,-2.63,-2.58,-2.85,-2.76,-2.75,-2.85,-2.86,-2.81,-2.78,
       -2.75,-2.69,-2.62,-2.81,-2.92,-2.88,-2.92,-2.85,-2.94,-2.86,-2.96,-2.67,-2.63,-2.83,
       -2.81,-2.70,-2.69,-2.90,-2.80,-2.85,-2.67,-2.43,-2.85,-2.86,-2.98,-2.86,-2.94,-2.94,
       -2.83)



summary(S)
hist(S)

# Initial values:
lambda <- list(chain1=c(-3, 0), chain2=c(-2,-2))
gamma <- list(chain1=c(10, 0.1), chain2=c(30, 5))


# Run the model
results <- run.jags(continuous_test, n.chains = 2)

# Results interpretation and visualization
plot(results, vars=c('AUC', 'P', 'lambda', 'gamma', 'sigma'))
results_summary <- add.summary(results, vars=c('AUC', 'P', 'lambda', 'gamma', 'sigma'))

# Estimate Se, Sp and plot ROC curve
se_est <- combine.mcmc(results, vars='se')
sp_est <- combine.mcmc(results, vars='sp')
ses_mu <- apply(se_est, 2, mean)
sps_mu <- apply(sp_est, 2, mean)
par(mfrow=c(1,1))
plot((1-sps_mu), ses_mu, type="l", col="darkblue", xlab = "1-Sp", ylab = "Se")

# AUC estimation
auc_est <- combine.mcmc(results, vars='AUC')
hist(auc_est, breaks=50, col="orange", main="AUC")

# Mean of infected and uninfected groups
lambda_est <- combine.mcmc(results, vars='lambda')
boxplot(as.matrix(lambda_est), col="red")

####### 
# Exercise 5 - Meta - Analysis with BLCMs

# Links to understand, run the model
# ▶ Bayesian Meta-Analysis of the Accuracy of a Test for Tuberculous Pleuritis in the Absence of a Gold Standard Reference (DOI: 10.1111/j.1541-0420.2012.01773.x)
# ▶ Bayesian Diagnostic Test Accuracy Meta-Analysis ShinyApp - https://bayesdta.shinyapps.io/meta-analysis/
# ▶ ShinyApp Demonstration: https://www.nandinidendukuri.com/bayesian-inference-for-hsroc-diagnostic-meta-analysis-model/



