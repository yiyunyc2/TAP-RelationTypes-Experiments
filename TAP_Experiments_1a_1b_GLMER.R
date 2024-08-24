#install.packages('ergm.ego')
#library(ergm.ego)
#install.packages('devtools',repos = "http://cran.us.r-project.org")
library(devtools)
#devtools::install_github("timonelmer/netglm")
library(netglm)
#install.packages('lme4',repos = "http://cran.us.r-project.org")
library(lme4)
#install.packages("fitdistrplus",repos = "http://cran.us.r-project.org") ## check distribution
library(fitdistrplus) 
#install.packages("goft",repos = "http://cran.us.r-project.org")
library(goft)
#install.packages("car",repos = "http://cran.us.r-project.org")
library(car)
#install.packages("actuar",repos = "http://cran.us.r-project.org")
library(actuar)
#install.packages("DHARMa")
library(DHARMa)
#install.packages("sjPlot")
library(sjPlot)
#install.packages("glmmTMB")
library(glmmTMB)
#install.packages("MuMIn")
require(MuMIn)
#install.packages("performance")
library(performance)
#install.packages("partR2")
library(partR2) # for part R2 values
#install.packages("ggpubr")
library(ggpubr)


setwd("/Users/lydinh/Downloads")

##############  Experiment 1a  ###################### 
TAP_mixedArtTypes = read.csv("Experiment-1a-output-summary.csv")
TAP_mixedArtTypes

equals_2 = TAP_mixedArtTypes$Equal
includes_2 = TAP_mixedArtTypes$Include
included_in_2 = TAP_mixedArtTypes$Included.in
disjoint_2 = TAP_mixedArtTypes$Disjoint
overlaps_2 = TAP_mixedArtTypes$Overlap
pw_2 = TAP_mixedArtTypes$Number.of.PWs
pw_2

#mvshapiro_test(pw_2)  ## p-value<0.05 = NOT normal

## histogram of raw data 
hist(pw_2)

##model 3

glm_model3 = glm(pw_2 ~ 0 + equals_2 + includes_2 + included_in_2 + disjoint_2 + overlaps_2, family="poisson"(link='log'))
#glm_model3 = glm(pw_2 ~ included_in_2 * includes_2 * disjoint_2 * overlaps_2, family="poisson"(link='log'))

#glm_model3 = glm(pw_2 ~ equals_2 * includes_2 * included_in_2 * disjoint_2 * overlaps_2, family="poisson"(link='log'))
summary(glm_model3)

plot_model(glm_model3, show.values = TRUE, value.offset = .3)

exp(coefficients(glm_model3))


pdf(file = "/Users/lydinh/Downloads/Exp1a_IRR_new.pdf",height=5, width=15)   # The directory you want to save the file in

set_theme(base = theme_classic(), #To remove the background color & grids
          #   theme.font = 'arial',   #To change the font type
          axis.title.size = 2.0,  #To change axis title size
          axis.textsize.x = 1.3,  #To change x axis text size
          axis.textsize.y = 1.3)  #To change y axis text size

model = plot_model(
  glm_model3,
  colors = "Paired",  ## change colors here
  show.values = TRUE,
  value.offset = .45,
  value.size = 8,
  dot.size = 10,
  line.size = 5,
  vline.color = "red",
  width = 0.7,
  title="",
  axis.title = c("Incidence Rate Ratio","hi", "hi")) + scale_x_discrete(labels=c("Equal","Include", "Included-In", "Disjoint", "Overlap")) + scale_y_continuous(limits = c(0.3,10))

model

dev.off()


##############  Experiment 1b  ###################### 
TAP_final = read.csv("Experiment-1b-output-summary.csv")
TAP_final

equals_5 = TAP_final$Equals
includes_5 = TAP_final$Includes
included_in_5 = TAP_final$Included.in
disjoint_5 = TAP_final$Disjoint
overlaps_5 = TAP_final$Overlaps
pw_5 = TAP_final$PWs
pw_5

## histogram of raw data 
hist(pw_5)

##model 3
#glm_model4 = glm(pw_3 ~ equals_3 + includes_3 + included_in_3 + disjoint_3 + overlaps_3, family="poisson"(link='log'))
glm_model5 = glm(pw_5 ~ 0 + (equals_5 + includes_5 + included_in_5 + disjoint_5 + disjoint_5 + overlaps_5)^2, family="poisson"(link='log'))

summary(glm_model5)

plot_model(glm_model5, show.values = TRUE, value.offset = .3)

exp(coefficients(glm_model5))
## we don't have confidence interval because If x and y are proportions, odds.ratio simply returns the value of the odds ratio, with no confidence interval.

pdf(file = "/Users/lydinh/Downloads/Exp1b_IRR_new.pdf",height=5, width=15)   # The directory you want to save the file in

set_theme(base = theme_classic(), #To remove the background color & grids
          #   theme.font = 'arial',   #To change the font type
          axis.title.size = 2.0,  #To change axis title size
          axis.textsize.x = 1.3,  #To change x axis text size
          axis.textsize.y = 1.3)  #To change y axis text size

model = plot_model(
  glm_model5,
  colors = "Paired",  ## change colors here
  show.values = TRUE,
  value.offset = .45,
  value.size = 8,
  dot.size = 10,
  line.size = 5,
  vline.color = "red",
  width = 0.7,
  title="",
axis.title = c("Incidence Rate Ratio","hi", "hi")) + scale_x_discrete(labels=c("Equal","Include", "Included-In", "Disjoint", "Overlap")) + scale_y_continuous(limits = c(0.3,30))

model

dev.off()
