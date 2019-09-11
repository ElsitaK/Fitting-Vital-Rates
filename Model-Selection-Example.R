library(lme4)
library(MuMIn)

setwd('~/Fitting-Vital-Rates')

#read in the data
Data1=read.csv("Warming.Field.FP4.5.Eggs.Results.1.8.18.csv")
Data2=read.csv("Warming.Field.FP2.3.Eggs.Results.1.8.18.csv")
Data3=read.csv("Warming.Field.FP3.1.Eggs.Results.1.8.18.csv")

Data1$FP <- "A" #summer 1 generation
Data2$FP <- "B" #summer 2 generation
Data3$FP <- "C" #winter generation

Data4 <- rbind(Data1, Data2)
Data <-rbind(Data4,Data3)

#change data into egg counts from 1 day survival
Data$dailySurvival <- sqrt(Data$TwoDaySurvival)
Data$Eggs.OutA <- round(Data$dailySurvival*Data$Eggs.In)

#add Dam Restoration treatment
Metadata=read.csv("Restoration.DAM.Treatments.csv")
Data <- merge(Data,Metadata,by=c("Site","Plot"))
str(Data)

#create survival variable
survariable <- cbind(Data$Eggs.OutA, Data$Eggs.In-Data$Eggs.OutA)

#scaling data
dfs <- Data #duplicate
numcols <- c(5,6,8) #all temperature variable data columns
attributes <- scale(Data[,numcols])
str(attributes) #quick look 
dfs[,numcols] <- scale(dfs[,numcols]) #rewrites and saves scaled variables to dataframe dfs

#list of models to be ranked
mod.1 = glmer(formula = survariable ~ (1 | Site/Plot) + Treatment + Dam + FP, data = dfs, family = binomial(logit))
mod.2 = glmer(formula = survariable ~ (1 | Site/Plot) + Treatment, data = dfs, family = binomial(logit))
mod.3 = glmer(formula = survariable ~ (1 | Site/Plot) + Dam, data = dfs, family = binomial(logit))
mod.4 = glmer(formula = survariable ~ (1 | Site/Plot) + FP, data = dfs, family = binomial(logit))
mod.5 = glmer(formula = survariable ~ (1 | Site/Plot) + Dam + FP, data = dfs, family = binomial(logit))
mod.6 = glmer(formula = survariable ~ (1 | Site/Plot) + Treatment + Dam, data = dfs, family = binomial(logit))
mod.7 = glmer(formula = survariable ~ (1 | Site/Plot) + Treatment + FP, data = dfs, family = binomial(logit))
mod.8 = glmer(formula = survariable ~ (1 | Site/Plot), data = dfs, family = binomial(logit))

mod.9 = glmer(formula = survariable ~ AVEmaxTemp + (1 | Site/Plot), data = dfs, family = binomial(logit))
mod.10 = glmer(formula = survariable ~ AVEmaxTemp + (1 | Site/Plot) + Treatment, data = dfs, family = binomial(logit))
mod.11 = glmer(formula = survariable ~ AVEmaxTemp + (1 | Site/Plot) + Dam, data = dfs, family = binomial(logit))
mod.12 = glmer(formula = survariable ~ AVEmaxTemp + (1 | Site/Plot) + Treatment + Dam, data = dfs, family = binomial(logit))
mod.13 = glmer(formula = survariable ~ AVEmaxTemp*FP + (1 | Site/Plot), data = dfs, family = binomial(logit))
mod.14 = glmer(formula = survariable ~ AVEmaxTemp*FP + (1 | Site/Plot) + Dam, data = dfs, family = binomial(logit))
mod.15 = glmer(formula = survariable ~ AVEmaxTemp*FP + (1 | Site/Plot) + Treatment, data = dfs, family = binomial(logit))
mod.16 = glmer(formula = survariable ~ AVEmaxTemp*FP + (1 | Site/Plot) + Treatment + Dam, data = dfs, family = binomial(logit))

mod.17 = glmer(formula = survariable ~ AVEminTemp + (1 | Site/Plot), data = dfs, family = binomial(logit))
mod.18 = glmer(formula = survariable ~ AVEminTemp + (1 | Site/Plot) + Treatment, data = dfs, family = binomial(logit))
mod.19 = glmer(formula = survariable ~ AVEminTemp + (1 | Site/Plot) + Dam, data = dfs, family = binomial(logit))
mod.20 = glmer(formula = survariable ~ AVEminTemp + (1 | Site/Plot) + Treatment + Dam, data = dfs, family = binomial(logit))
mod.21 = glmer(formula = survariable ~ AVEminTemp*FP + (1 | Site/Plot), data = dfs, family = binomial(logit))
mod.22 = glmer(formula = survariable ~ AVEminTemp*FP + (1 | Site/Plot) + Dam, data = dfs, family = binomial(logit))
mod.23 = glmer(formula = survariable ~ AVEminTemp*FP + (1 | Site/Plot) + Treatment, data = dfs, family = binomial(logit))
mod.24 = glmer(formula = survariable ~ AVEminTemp*FP + (1 | Site/Plot) + Treatment + Dam, data = dfs, family = binomial(logit))

mod.25 = glmer(formula = survariable ~ meanTemp + (1 | Site/Plot), data = dfs, family = binomial(logit))
mod.26 = glmer(formula = survariable ~ meanTemp + (1 | Site/Plot) + Treatment, data = dfs, family = binomial(logit))
mod.27 = glmer(formula = survariable ~ meanTemp + (1 | Site/Plot) + Dam, data = dfs, family = binomial(logit))
mod.28 = glmer(formula = survariable ~ meanTemp + (1 | Site/Plot) + Treatment + Dam, data = dfs, family = binomial(logit))
mod.29 = glmer(formula = survariable ~ meanTemp*FP + (1 | Site/Plot), data = dfs, family = binomial(logit))
mod.30 = glmer(formula = survariable ~ meanTemp*FP + (1 | Site/Plot) + Dam, data = dfs, family = binomial(logit))
mod.31 = glmer(formula = survariable ~ meanTemp*FP + (1 | Site/Plot) + Treatment, data = dfs, family = binomial(logit))
mod.32 = glmer(formula = survariable ~ meanTemp*FP + (1 | Site/Plot) + Treatment + Dam, data = dfs, family = binomial(logit))

#models with no interaction between FP and temperature variable
mod.33 = glmer(formula = survariable ~ AVEmaxTemp + FP + (1 | Site/Plot), data = dfs, family = binomial(logit))
mod.34 = glmer(formula = survariable ~ AVEmaxTemp + FP + (1 | Site/Plot) + Dam, data = dfs, family = binomial(logit))
mod.35 = glmer(formula = survariable ~ AVEmaxTemp + FP + (1 | Site/Plot) + Treatment, data = dfs, family = binomial(logit))
mod.36 = glmer(formula = survariable ~ AVEmaxTemp + FP + (1 | Site/Plot) + Treatment + Dam, data = dfs, family = binomial(logit))

mod.37 = glmer(formula = survariable ~ AVEminTemp + FP + (1 | Site/Plot), data = dfs, family = binomial(logit))
mod.38 = glmer(formula = survariable ~ AVEminTemp + FP + (1 | Site/Plot) + Treatment, data = dfs, family = binomial(logit))
mod.39 = glmer(formula = survariable ~ AVEminTemp + FP + (1 | Site/Plot) + Dam, data = dfs, family = binomial(logit))
mod.40 = glmer(formula = survariable ~ AVEminTemp + FP + (1 | Site/Plot) + Treatment + Dam, data = dfs, family = binomial(logit))

mod.41 = glmer(formula = survariable ~ meanTemp + FP + (1 | Site/Plot), data = dfs, family = binomial(logit))
mod.42 = glmer(formula = survariable ~ meanTemp + FP + (1 | Site/Plot) + Treatment, data = dfs, family = binomial(logit))
mod.43 = glmer(formula = survariable ~ meanTemp + FP + (1 | Site/Plot) + Dam, data = dfs, family = binomial(logit))
mod.44 = glmer(formula = survariable ~ meanTemp + FP + (1 | Site/Plot) + Treatment + Dam, data = dfs, family = binomial(logit))

#model convergence issue
#function to help models to converge
CH <- function(x){
  ss <- getME(x,c("theta","fixef")) 
  return(update(x,start=ss,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))))
}

#models that did not converge
#run this code a few times
mod.21 = CH(mod.21)
mod.31 = CH(mod.31) 
mod.32 = CH(mod.32) 

#all singular fits not converging properly, remove

#model selection table
model.sel(mod.1,	mod.2,	mod.3,	mod.4,	mod.5,	mod.6,	mod.7,	mod.8,	mod.9, mod.10,	mod.11, mod.12, mod.13, mod.14, mod.15, mod.16, mod.17, mod.18,	mod.19, mod.20,	mod.22, mod.23, mod.24, mod.25,	mod.26,	mod.27,	mod.28,	mod.29,	mod.30,	mod.31,	mod.32, mod.33, mod.34, mod.35, mod.36, mod.37, mod.38, mod.39, mod.40, mod.41, mod.42, mod.42, mod.43, mod.44)

#HIGHEST RANKED MODEL
mod.33 = glmer(formula = survariable ~ AVEmaxTemp + FP + (1 | Site/Plot), data = dfs, family = binomial(logit))
summary(mod.33)
