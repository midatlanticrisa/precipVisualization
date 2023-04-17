#################################################################################
#
#  -file = "explanation_plots.R"   Code written January 2023
#  - Author: Kelsey Ruckert (klr324@psu.edu)
#
#  -This code generates plots that explain 3 common visual styles for presenting
#       climate information. These visual styles match those represented in the 
#       Outlooks. Explanation plots include box, bar, and area/line plots.
#
# THIS CODE IS PROVIDED AS-IS WITH NO WARRANTY (NEITHER EXPLICIT
# NOT IMPLICIT).  I SHARE THIS CODE IN HOPES THAT IT IS USEFUL, 
# BUT I AM NOT LIABLE FOR THE BEHAVIOR OF THIS CODE IN YOUR OWN
# APPLICATION.  YOU ARE FREE TO SHARE THIS CODE SO LONG AS THE
# AUTHOR(S) AND VERSION HISTORY REMAIN INTACT.
#
###################################################################################
rm(list =ls()) #Clear global environment
# install.packages("DEoptim")
library(DEoptim)
library(ggplot2)
library(compiler)
enableJIT(3)

# Functions ---------------------------------------------------------------
# Box plot explanation plot modified from the USGS's post by Laura DeCicco:
# "Exploring ggplot2 boxplots - Defining limits and adjusting style"
# https://waterdata.usgs.gov/blog/boxplots/
ggplot_box_legend_simple <- function(family = "serif"){
  
  # Create data to use in the boxplot legend:
  set.seed(100)
  
  sample_df <- data.frame(parameter = "test",
                          values = sample(500))
  
  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350
  
  # Function to calculate important values:
  ggplot2_boxplot <- function(x){
    
    quartiles <- as.numeric(quantile(x,
                                     probs = c(0.25, 0.5, 0.75)))
    
    names(quartiles) <- c("25th percentile",
                          "50th percentile\n(median)",
                          "75th percentile")
    
    IQR <- diff(quartiles[c(1,3)])
    
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }
  
  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)
  
  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  # Labels don't inherit text:
  update_geom_defaults("label",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  
  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +
    stat_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 geom ='errorbar', width = 0.3) +
    geom_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 width = 0.3, fill = "lightgrey", outlier.shape = NA) +
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(x = 2.3, xend = 2.3,
                     y = ggplot_output[["25th percentile"]],
                     yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3,
                     y = ggplot_output[["25th percentile"]],
                     yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3,
                     y = ggplot_output[["75th percentile"]],
                     yend = ggplot_output[["75th percentile"]])) +
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]),
              label = "Interquartile range\n(IQR = 75th - 25th\npercentile)", fontface = "bold",
              # label = "Interquartile\nrange\n(middle 50%\nof the data)", fontface = "bold",
              vjust = 0.4) +
    geom_text(aes(x = c(1.17,1.17),
                  y = c(ggplot_output[["upper_whisker"]],
                        ggplot_output[["lower_whisker"]]),
                  label = c("Largest value within\n75th percentile + 1.5 x IQR",
                            "Smallest value within\n25th percentile - 1.5 x IQR")),
                  # label = c("Largest value (1.5 times\nthe interquartile range above\n75th percentile)",
                  #           "Smallest value (1.5 times\nthe interquartile range below\n25th percentile)")),
              fontface = "bold", vjust = 0.9) +
  geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]],
                 label = names(ggplot_output[["quartiles"]])),
             vjust = c(0.4,0.85,0.4),
             fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-200, 900))
  
  return(explain_plot)
}

# For Polygon plot make dummy data using a model and bootstrapping
# This is the model: y = a+b*x
model<-function(parm,x){ # Inouts are parameters and length of data
  model.p=length(parm) # number of parameters in the model
  
  a=parm[1]
  b=parm[2]
  
  y.mod <- a+b*x #This linear equation represent the model
  #Return model observations and number of parameters
  return(list(mod.obs = y.mod, model.p = model.p))
}

# source the model
boot_model = function(p){ # p represents the parameters in a vector
  
  obser=p[1]+p[2]*x # estimate observations using a linear model
  #p[1] = a
  #p[2] = b
  return(obser)
}

# find the minimum residuals
min_res = function(p){
  sum(abs(obs - boot_model(p))) # returns the sum of the absolute values
  # from observations minus simulated model values
}

# Create explanation plots ------------------------------------------------
############################# Box plot #########################################
cairo_ps("plots/boxexplanation.eps", width=3, height=4)
par(mgp=c(1.5,.5,0), mar=c(3, 4, 2, 1), las=1)
ggplot_box_legend_simple()
dev.off()

############################# Bar plot #########################################
cairo_ps("plots/barexplanation.eps", width=5.72, height=4.04, family = "serif")
par(mfrow=c(1, 1), mgp=c(1.5,.5,0), mar=c(1, 3, 1, 1))
barplot(1:3, yaxt="n", xlim=c(0,5))
lines(c(0,3.5), rep(0,2))

lines(c(3.7,4.2), rep(3,2))
lines(c(3.7,4.2), rep(0,2))
lines(rep(4.2,2), c(0,3))

text(4.35, 1.5, labels="Average", pos=4, offset=0, cex=0.8, font=2)
dev.off()

########################### Polygon plot #######################################
# The bootstrap method is applied to a linear model as described in the appendix of Ruckert et al.
# (2016). For further description and references, please read the paper and appendix.
#----------------------- Step 1: Choose the number of observations ----------------------------#
datalength = 200
x = 1:datalength
################# Step 1: Set up the true model & Generate generate AR 1 errors
true_par = c(1,0.2) #a=1 b=0.2
true.obs= model(true_par,x)

rho=0.3  # define your correlation
sigma.ar1=0.3 # define your ar1 white noise inovation variance
sigma.inov = sigma.ar1/(1-0.3^2) # this is your stationary variance for the AR1 process. 
# i.e.the variance for time 0

t.s0 = rnorm(1,0,sd = sqrt(sigma.inov))
t.s<-c()
t.s[1]<-t.s0
for ( i in (2:datalength)){ # write a loop to simulate AR1 residuals
  t.s[i]<-rho*t.s[i-1]+rnorm(1,mean=0,sd=sqrt(sigma.ar1))
}

obs = true.obs$mod.obs + t.s #Add the observations and the residuals

#------------------------- Find Initial Parameters -------------------------------
#Run DEoptim in R to find good initial parameters
lower=c(-10,-10)  
upper=c(10,10)
iter=1000  # specify number of iterations
outDEoptim <- DEoptim(min_res, lower, upper, 
                      DEoptim.control(itermax=iter,
                                      trace=FALSE))
print(outDEoptim$optim$bestmem)# find best initial parameters
parms = c(outDEoptim$optim$bestmem[1], outDEoptim$optim$bestmem[2])

#Run the model with the initial parameters to create the best simulation of the observations
y.obs= model(parms,x)
# Project the linear model to 300
plength = 300
project = 1:plength
p.obs = model(parms,project)

#---------- Calculate the Residuals ------------------
### Calculate Residuals during observed time series (data - polynomial fit)  ###
res <- obs - y.obs$mod.obs

#---------- Bootstrap the Residuals & Save AR(1) Coefficient and Sigma ---------
### We need to retain the auto-correlated structure of the residuals ###
### determine AR coefficients of the original residuals (data - polynomial fit)  ###
# pdf(file="../ToyFigures/plot1a.pdf")  # write to pdf, define a pdf file to write to
rho=rep(NA,3)
ac <- acf(res, lag.max=5, plot=TRUE, main="Autocorrelated Residuals")  # apply auto-correlation to determine correlation coefficientsrho[1] <- ac$acf[ 1 ]
rho[2] <- ac$acf[ 2 ]
rho[3] <- ac$acf[ 3 ]
rho[4] <- ac$acf[ 4 ]
rho[5] <- ac$acf[ 5 ]
# dev.off()

# Find the standard deviation (sigma)
N=5000  # Number of bootstrap samples
white.boot = mat.or.vec(N, datalength) # create matrix (nr,nc)
white.boot_sd = rep(NA,N)
for(i in 1:N) {
  white.boot[i,1:datalength] = sample(res,size=datalength,replace=TRUE)
  white.boot_sd[i] = sd(white.boot[i,]) #this estimtes stationary variance
}

### Calculate the bootstrapped residuals with an lag-1 autocorrelation coefficient: rho[2]
# IMPORTANT: estimate the white noice variance (sigma) from the stationary variance to avoid accounting
# for autocorrelation twice
sigma = sqrt((white.boot_sd^2)*(1-(rho[2]^2))) #what is this?
res.boot=mat.or.vec(N, datalength) #(nr,nc)
for(n in 1:N) {
  for(i in 2:datalength) {
    res.boot[n,i] = rho[2]*res.boot[n,i-1] + rnorm(1,mean=0,sd=sigma[n])
  }
}
### Superimpose residuals on the hindcasts from the linear model to make N bootstrap samples###
lin.boot=mat.or.vec(N, datalength) #(nr,nc)
for(i in 1:N) {
  lin.boot[i,]=y.obs$mod.obs+res.boot[i,]
}

###IMPORTANT: calculate polynomial coefficients for the bootstrapped samples###
########## NOTE: THIS MAY TAKE A FEW MINUTES! ###
boot.fit_coef=mat.or.vec(N, 2)
for(i in 1:N) {
  # source("ToyScripts/de_boot.R") # source the model
  # source("ToyScripts/bootstrapped_min_res.R") # find the minimum residuals with the Many bootstrap samples
  lower=c(-10,-10)  
  upper=c(10,10)
  iter=100  # specify number of iterations
  outDEoptim <- DEoptim(min_res, lower, upper, 
                        DEoptim.control(itermax=iter,
                                        trace=FALSE))
  boot.fit_coef[i,1]=outDEoptim$optim$bestmem[1]
  boot.fit_coef[i,2]=outDEoptim$optim$bestmem[2]
}
#---------------------------- Hincast the model with uncertainty ------------------------------------
# Calculate the smooth hindcast fits
boot.fit=mat.or.vec(N, datalength)
for(n in 1:N) {
  for(i in 1:datalength) {
    boot.fit[n,i]=boot.fit_coef[n,1] + boot.fit_coef[n,2]*x[i]
  }
}
# Add the AR(1) residual noise to the smooth hindcast fits
boot.fit_prob=mat.or.vec(N, datalength) #(nr,nc)
for(i in 1:N) {
  boot.fit_prob[i,] = boot.fit[i,]+res.boot[i,]
}

#---------------------------- Project the model with uncertainty ------------------------------------
#Calculate smooth projections
boot.fit_proj=mat.or.vec(N, plength)
for(n in 1:N) {
  for(i in 1:plength) {
    boot.fit_proj[n,i]=boot.fit_coef[n,1] + boot.fit_coef[n,2]*project[i]
  }
}

#---------------------------------------------------------------------------
### calculate projected AR(1) residual noise ###
res.boot_proj=mat.or.vec(N, plength) #(nr,nc)
boot_proj=res.boot_proj
for(n in 1:N) {
  for(i in 2:plength) {
    res.boot_proj[n,i] = rho[2]*res.boot_proj[n,i-1] + rnorm(1,mean=0,sd=sigma[n])
  }
}

### superimpose residuals on polynomial smooth fits to 
### calculate Probabilistic bootstrap samples###
boot.proj=mat.or.vec(N, plength) #(nr,nc)
for(i in 1:N) {
  boot.proj[i,]=boot.fit_proj[i,]+res.boot_proj[i,]
}
#------------------------ Analyze the Results ------------------------------------
#Plot the projected fits with the added noise
### Plot the Min-Max hindcast ###
# Calculate the Min-Max confidence interval
boot_f <-
  boot_mean <-
  boot_nf <-rep(NA,plength)
for(i in 1:plength){
  boot_f[i] = min(boot.proj[,i])
  boot_nf[i] = max(boot.proj[,i])
  boot_mean[i] = mean(boot.proj[,i])
}
boot_x=c(boot_f[1:80], rev(boot_nf[1:80])); boot_y=c(project[1:80], rev(project[1:80]))

#Calculate the Surprise index by observation the ratio of outliers to the expected # of outliers
# The confidence intervals in the paper are:
# Percentages: 10%, 20%, 30%, 40%, 50%, 60%, 70%, 80%, 90%, 92%, 95%,  96%, 97%,  98%, 99%,  100%
# low numbers: 0.45,0.40,0.35,0.30,0.25,0.20,0.15,0.10,0.05,0.04,0.025,0.02,0.015,0.01,0.005,0
# high numbers:0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,0.96,0.975,0.98,0.985,0.99,0.995,100

cairo_ps("plots/polyexplanation.eps", width=5.72, height=4.04, family = "serif")
par(mfrow=c(1, 1), mgp=c(1.5,.5,0), mar=c(1, 1, 1, 1))
plot(0, type="n",xlab="",ylab="", xaxt="n", yaxt="n",frame.plot = FALSE,
     ylim=c(0,20), xlim=c(5,120))
#Plot the 90% confidence interval envelope
polygon(boot_y, boot_x, col="gray", border=NA)
points(x[1:80], obs[1:80], pch=20, cex=0.8, col="black")
points(42, 6.7, pch=20, cex=0.8, col="black")
lines(project[1:80], boot_mean[1:80], col="black", lwd=1.5)

text(81, boot_nf[80], labels="Maximum", pos=4, offset=0, cex=0.8)
text(81, boot_f[80], labels="Minimum", pos=4, offset=0, cex=0.8)
text(81, boot_mean[80], labels="Average", pos=4, offset=0, cex=0.8)
text(45, 6, labels="Historical measurement\n(observation)", pos=4, offset=0, 
     cex=0.8, font=2)

lines(c(95,100), rep(boot_nf[80],2))
lines(c(95,100), rep(boot_f[80],2))
lines(rep(100,2), c(boot_f[80],boot_nf[80]))
text(101, mean(c(boot_f[80],boot_nf[80]))-0.5, 
     labels="Max-Min\nrange\n(100% of\nthe model\nsimulation)", pos=4, offset=0, 
     cex=0.8, font=2)
dev.off()

#################### END ####################################
