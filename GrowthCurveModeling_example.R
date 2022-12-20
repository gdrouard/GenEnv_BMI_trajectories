# Here is a script to show how lavaan has been used to model BMI/Alcohol 
# consumption trajectories. These are example codes, and can be easily customized.
# Additional resources include the lavaan webpage from which valuable examples
# can be found.


###############################
#### Growth Curve Modeling ####
###############################

# import data

Twin_bivar_finaldf <- read.csv("C:/.../Twin_bivar_finaldf.txt", sep="")
summary(Twin_bivar_finaldf)

# the data should be wide format

### rectify variable type before analysis
Twin_bivar_finaldf$PERSON_NB <- as.character(Twin_bivar_finaldf$PERSON_NB)
Twin_bivar_finaldf$FAMILY_NB <- as.character(Twin_bivar_finaldf$FAMILY_NB)
Twin_bivar_finaldf$SEX <- as.factor(Twin_bivar_finaldf$SEX)
Twin_bivar_finaldf$ZYG <- as.factor(Twin_bivar_finaldf$ZYG)
Twin_bivar_finaldf$Education <- as.factor(Twin_bivar_finaldf$Education)
Twin_bivar_finaldf$Mar75 <- as.factor(Twin_bivar_finaldf$Mar75)
Twin_bivar_finaldf$Mar81 <- as.factor(Twin_bivar_finaldf$Mar81)
Twin_bivar_finaldf$Mar90 <- as.factor(Twin_bivar_finaldf$Mar90)
Twin_bivar_finaldf$Mar11 <- as.factor(Twin_bivar_finaldf$Mar11)
Twin_bivar_finaldf$Smo75 <- as.factor(Twin_bivar_finaldf$Smo75)
Twin_bivar_finaldf$Smo81 <- as.factor(Twin_bivar_finaldf$Smo81)
Twin_bivar_finaldf$Smo90 <- as.factor(Twin_bivar_finaldf$Smo90)
Twin_bivar_finaldf$Smo11 <- as.factor(Twin_bivar_finaldf$Smo11)

# final summary

summary(Twin_bivar_finaldf) # to display the data

####################################
# The following loop computes variance/sd for descriptive statistics

#building variance and average variable
sd <- rep(NA,nrow(Twin_bivar_finaldf))
average <- rep(NA,nrow(Twin_bivar_finaldf))
BMI_order <- c("BMI75","BMI81","BMI90","BMI11")
for(i in 1:nrow(Twin_bivar_finaldf)){
  average[i] <- mean(as.numeric(Twin_bivar_finaldf[i,BMI_order]))
  sd[i] <- sd(Twin_bivar_finaldf[i,BMI_order])
}
head(average);head(sd)

####################################

library(lavaan) #package used for Growth Modeling

####################################
# additonal data transformation

library(mltools)
library(data.table)
df <- Twin_bivar_finaldf
# lavaan considers numeric covariates, so we one-hot encode
df$SEX <- as.numeric(as.numeric(df$SEX) - 1)
df$ZYG <- as.numeric(as.numeric(df$ZYG) - 1)

# change raw education classes into "known" classes
table(df$Education)
vartochange <- rep("other",nrow(df))
vartochange[which(df$Education=="High school 12 yrs")] <- "HS"
vartochange[which(df$Education=="High school and more")] <- "HS"
vartochange[which(df$Education=="9 yrs")] <- "Mid"
vartochange[which(df$Education=="7-8 yrs")] <- "Mid"
vartochange[which(df$Education=="10-11 yrs")] <- "Mid"
vartochange[which(df$Education=="6 yrs or less")] <- "Low"
vartochange[which(df$Education=="University")] <- "U"
table(vartochange)
df$Education <- as.factor(vartochange)

# we automatically one-hot encode the covariate variables
df <- one_hot(as.data.table(df)) #one-hot encoding
df <- as.data.frame(df)

# rename covariates
colnames(df)
colnames(df)[22:29] <- c("Mar75m","Mar75s","Mar81m","Mar81s","Mar90m",
                         "Mar90s","Mar11m","Mar11s")
colnames(df)[30:49] <- c("Smo75_1","Smo75_2","Smo75_3","Smo75_4","Smo75_5",
                         "Smo81_1","Smo81_2","Smo81_3","Smo81_4","Smo81_5",
                         "Smo90_1","Smo90_2","Smo90_3","Smo90_4","Smo90_5",
                         "Smo11_1","Smo11_2","Smo11_3","Smo11_4","Smo11_5")
rownames(df) <- rownames(Twin_bivar_finaldf)


# one can use log-values to reduce skewness of the originale variables
# ex:
# we consider log-values
df <- as.data.frame(df)
df$BMI75 <- log(df$BMI75)
df$BMI81 <- log(df$BMI81)
df$BMI90 <- log(df$BMI90)
df$BMI11 <- log(df$BMI11)
# same can be done for alcohol (alcohol variables were already log-values in
# the pre-loaded dataset)


#########################################
####### Design full control / Both sexes

### model specification

model <- '
  # intercept and slope with fixed coefficients
    i_bmi =~ 1*BMI75 + 1*BMI81 + 1*BMI90 + 1*BMI11
    s_bmi =~ 0*BMI75 + 6*BMI81 + 15*BMI90 + 36*BMI11
    i_alc =~ 1*Alc75 + 1*Alc81 + 1*Alc90 + 1*Alc11
    s_alc =~ 0*Alc75 + 6*Alc81 + 15*Alc90 + 36*Alc11
  
  # first control 
    i_bmi ~ SEX + Education_HS + Education_Low + Education_Mid + Education_U
    s_bmi ~ SEX + Education_HS + Education_Low + Education_Mid + Education_U
    i_alc ~ SEX + Education_HS + Education_Low + Education_Mid + Education_U
    s_alc ~ SEX + Education_HS + Education_Low + Education_Mid + Education_U
    
  # time-varying covariates
    BMI75 ~ Age75 + Smo75_1 + Smo75_2 + Smo75_3 + Mar75m 
    BMI81 ~ Age81 + Smo81_1 + Smo81_2 + Smo81_3 + Mar81m 
    BMI90 ~ Age90 + Smo90_1 + Smo90_2 + Smo90_3 + Mar90m 
    BMI11 ~ Age11 + Smo11_1 + Smo11_2 + Smo11_3 + Mar11m 
    Alc75 ~ Age75 + Smo75_1 + Smo75_2 + Smo75_3 + Mar75m 
    Alc81 ~ Age81 + Smo81_1 + Smo81_2 + Smo81_3 + Mar81m 
    Alc90 ~ Age90 + Smo90_1 + Smo90_2 + Smo90_3 + Mar90m 
    Alc11 ~ Age11 + Smo11_1 + Smo11_2 + Smo11_3 + Mar11m 
'

# fitting data
fit <- growth(model, data = df)

# checking model quality
lavInspect(fit)
summary(fit, fit.measures=T)
fit@Data

# save predictions
pred <- lavPredict(fit)
pred <- as.data.frame(pred)
rownames(pred)<-rownames(df)

# looking at correlations between growth factors
cor(pred)
cor.test(pred[,1],pred[,2])
cor.test(pred[,1],pred[,3])
cor.test(pred[,1],pred[,4])
cor.test(pred[,2],pred[,3])
cor.test(pred[,2],pred[,4])
cor.test(pred[,3],pred[,4])


#########################
#### without smoking covariate adjustment

model <- '
  # intercept and slope with fixed coefficients
    i_bmi =~ 1*BMI75 + 1*BMI81 + 1*BMI90 + 1*BMI11
    s_bmi =~ 0*BMI75 + 6*BMI81 + 15*BMI90 + 36*BMI11
    i_alc =~ 1*Alc75 + 1*Alc81 + 1*Alc90 + 1*Alc11
    s_alc =~ 0*Alc75 + 6*Alc81 + 15*Alc90 + 36*Alc11
  
  # first control 
    i_bmi ~ SEX + Education_HS + Education_Low + Education_Mid + Education_U
    s_bmi ~ SEX + Education_HS + Education_Low + Education_Mid + Education_U
    i_alc ~ SEX + Education_HS + Education_Low + Education_Mid + Education_U
    s_alc ~ SEX + Education_HS + Education_Low + Education_Mid + Education_U
    
  # time-varying covariates
    BMI75 ~ Age75 +  Mar75m 
    BMI81 ~ Age81 +  Mar81m 
    BMI90 ~ Age90 +  Mar90m 
    BMI11 ~ Age11 +  Mar11m 
    Alc75 ~ Age75 +  Mar75m 
    Alc81 ~ Age81 +  Mar81m 
    Alc90 ~ Age90 +  Mar90m 
    Alc11 ~ Age11 +  Mar11m 
'


fit <- growth(model, data = df)
lavInspect(fit)
summary(fit, fit.measures=T)
fit@Data
pred <- lavPredict(fit)
pred <- as.data.frame(pred)
rownames(pred)<-rownames(df)
cor(pred)
cor.test(pred[,1],pred[,4])


######################################
#### Stratification : we run the analysis for men/women seperately

# we made SEX-1 : originally men are 1 and female 2
df$SEX
dfm <- df[df$SEX==0,] #men
dff <- df[df$SEX==1,] #women

### MODEL MALE

model <- '
  # intercept and slope with fixed coefficients
    i_bmi =~ 1*BMI75 + 1*BMI81 + 1*BMI90 + 1*BMI11
    s_bmi =~ 0*BMI75 + 6*BMI81 + 15*BMI90 + 36*BMI11
    i_alc =~ 1*Alc75 + 1*Alc81 + 1*Alc90 + 1*Alc11
    s_alc =~ 0*Alc75 + 6*Alc81 + 15*Alc90 + 36*Alc11
  
  # first control 
    i_bmi ~ Education_HS + Education_Low + Education_Mid + Education_U
    s_bmi ~ Education_HS + Education_Low + Education_Mid + Education_U
    i_alc ~ Education_HS + Education_Low + Education_Mid + Education_U
    s_alc ~ Education_HS + Education_Low + Education_Mid + Education_U
    
  # time-varying covariates
    BMI75 ~ Age75 + Smo75_1 + Smo75_2 + Smo75_3 + Mar75m 
    BMI81 ~ Age81 + Smo81_1 + Smo81_2 + Smo81_3 + Mar81m 
    BMI90 ~ Age90 + Smo90_1 + Smo90_2 + Smo90_3 + Mar90m 
    BMI11 ~ Age11 + Smo11_1 + Smo11_2 + Smo11_3 + Mar11m 
    Alc75 ~ Age75 + Smo75_1 + Smo75_2 + Smo75_3 + Mar75m 
    Alc81 ~ Age81 + Smo81_1 + Smo81_2 + Smo81_3 + Mar81m 
    Alc90 ~ Age90 + Smo90_1 + Smo90_2 + Smo90_3 + Mar90m 
    Alc11 ~ Age11 + Smo11_1 + Smo11_2 + Smo11_3 + Mar11m 
'


fit <- growth(model, data = dfm)
lavInspect(fit)
summary(fit,fit.measures=T)
fit@Data
pred <- lavPredict(fit)
pred <- as.data.frame(pred)
rownames(pred)<-rownames(dfm)
cor(pred)
# testing correlations between GF
cor.test(pred[,1],pred[,2])
cor.test(pred[,1],pred[,3])
cor.test(pred[,1],pred[,4])
cor.test(pred[,2],pred[,3])
cor.test(pred[,2],pred[,4])
cor.test(pred[,3],pred[,4])

head(pred)
pred$person_nb <- rownames(pred)
#write.table(pred,"GrowthFactorsMales.txt")

### MODEL FEMALE

model <- '
  # intercept and slope with fixed coefficients
    i_bmi =~ 1*BMI75 + 1*BMI81 + 1*BMI90 + 1*BMI11
    s_bmi =~ 0*BMI75 + 6*BMI81 + 15*BMI90 + 36*BMI11
    i_alc =~ 1*Alc75 + 1*Alc81 + 1*Alc90 + 1*Alc11
    s_alc =~ 0*Alc75 + 6*Alc81 + 15*Alc90 + 36*Alc11
  
  # first control 
    i_bmi ~ Education_HS + Education_Low + Education_Mid + Education_U
    s_bmi ~ Education_HS + Education_Low + Education_Mid + Education_U
    i_alc ~ Education_HS + Education_Low + Education_Mid + Education_U
    s_alc ~ Education_HS + Education_Low + Education_Mid + Education_U
    
  # time-varying covariates
    BMI75 ~ Age75 + Smo75_1 + Smo75_2 + Smo75_3 + Mar75m 
    BMI81 ~ Age81 + Smo81_1 + Smo81_2 + Smo81_3 + Mar81m 
    BMI90 ~ Age90 + Smo90_1 + Smo90_2 + Smo90_3 + Mar90m 
    BMI11 ~ Age11 + Smo11_1 + Smo11_2 + Smo11_3 + Mar11m 
    Alc75 ~ Age75 + Smo75_1 + Smo75_2 + Smo75_3 + Mar75m 
    Alc81 ~ Age81 + Smo81_1 + Smo81_2 + Smo81_3 + Mar81m 
    Alc90 ~ Age90 + Smo90_1 + Smo90_2 + Smo90_3 + Mar90m 
    Alc11 ~ Age11 + Smo11_1 + Smo11_2 + Smo11_3 + Mar11m 
'


#same process:
fit <- growth(model, data = dff)
lavInspect(fit)
summary(fit, fit.measures=T)
temp <- summary(fit)
temp$PE
fit@Data
pred <- lavPredict(fit)
pred <- as.data.frame(pred)
rownames(pred)<-rownames(dff)
round(cor(pred),3)*100
# correlations structure 
cor.test(pred[,1],pred[,2])
cor.test(pred[,1],pred[,3])
cor.test(pred[,1],pred[,4])
cor.test(pred[,2],pred[,3])
cor.test(pred[,2],pred[,4])
cor.test(pred[,3],pred[,4])


plot(pred[,1],pred[,2]) #quick visualization
head(pred)
pred$person_nb <- rownames(pred)
#write.table(pred,"GrowthFactorsFemales.txt")



############################################################
##### CORRECT ONLY BY AGE FOR TWIN MODELING 


### MODEL MALE

model <- '
  # intercept and slope with fixed coefficients
    i_bmi =~ 1*BMI75 + 1*BMI81 + 1*BMI90 + 1*BMI11
    s_bmi =~ 0*BMI75 + 6*BMI81 + 15*BMI90 + 36*BMI11
    i_alc =~ 1*Alc75 + 1*Alc81 + 1*Alc90 + 1*Alc11
    s_alc =~ 0*Alc75 + 6*Alc81 + 15*Alc90 + 36*Alc11
  
    
  # time-varying covariates
    BMI75 ~ Age75
    BMI81 ~ Age81
    BMI90 ~ Age90 
    BMI11 ~ Age11 
    Alc75 ~ Age75  
    Alc81 ~ Age81 
    Alc90 ~ Age90 
    Alc11 ~ Age11 
'
# above are taken residuals of BMI/Alc
# it could be possible to correct for age differences.


fit <- growth(model, data = dfm)
lavInspect(fit)
summary(fit, fit.measures=T)
fit@Data
pred <- lavPredict(fit)
pred <- as.data.frame(pred)
rownames(pred)<-rownames(dfm)
cor(pred)

# correlations between GF
cor.test(pred[,1],pred[,2])
cor.test(pred[,1],pred[,3])
cor.test(pred[,1],pred[,4])
cor.test(pred[,2],pred[,3])
cor.test(pred[,2],pred[,4])
cor.test(pred[,3],pred[,4])

head(pred)
pred$person_nb <- rownames(pred)
#write.table(pred,"GrowthFactorsMales_wo_correction.txt")

### MODEL FEMALE

fit <- growth(model, data = dff)
lavInspect(fit)
summary(fit, fit.measures=T)
temp <- summary(fit)
temp$PE
fit@Data
pred <- lavPredict(fit)
pred <- as.data.frame(pred)
rownames(pred)<-rownames(dff)
cor(pred)
# correlations
cor.test(pred[,1],pred[,2])
cor.test(pred[,1],pred[,3])
cor.test(pred[,1],pred[,4])
cor.test(pred[,2],pred[,3])
cor.test(pred[,2],pred[,4])
cor.test(pred[,3],pred[,4])

cor.test(pred[,4],pred[,2])
head(pred)
pred$person_nb <- rownames(pred)
#write.table(pred,"GrowthFactorsFemales_wo_corrections.txt")



############################################################
##### MODEL WITHOUT ADJUSTMENT / model used in the ms  


### MODEL MALE

model <- '
  # intercept and slope with fixed coefficients
    i_bmi =~ 1*BMI75 + 1*BMI81 + 1*BMI90 + 1*BMI11
    s_bmi =~ 0*BMI75 + 6*BMI81 + 15*BMI90 + 36*BMI11
    i_alc =~ 1*Alc75 + 1*Alc81 + 1*Alc90 + 1*Alc11
    s_alc =~ 0*Alc75 + 6*Alc81 + 15*Alc90 + 36*Alc11
'

# in the ms, log-values were considered (as in the sensitivity analysis)

fit <- growth(model, data = dfm) # for men
lavInspect(fit)
summary(fit, fit.measures=T)
fit@Data
pred <- lavPredict(fit)
pred <- as.data.frame(pred)
rownames(pred)<-rownames(dfm)
pred$person_nb <- rownames(pred)
plot(pred[,1],pred[,2])
mean(pred[,1])
temp <- dfm$Age11 - dfm$Age75
summary(lm(pred[,1]~temp))
colnames(pred)

#write.table(pred,"gf_log_wo_m.txt")

model <- '
  # intercept and slope with fixed coefficients
    i_bmi =~ 1*BMI75 + 1*BMI81 + 1*BMI90 + 1*BMI11
    s_bmi =~ 0*BMI75 + 6*BMI81 + 15*BMI90 + 36*BMI11
    i_alc =~ 1*Alc75 + 1*Alc81 + 1*Alc90 + 1*Alc11
    s_alc =~ 0*Alc75 + 6*Alc81 + 15*Alc90 + 36*Alc11
'


fit <- growth(model, data = dff) #for women
lavInspect(fit)
summary(fit)
fit@Data
pred <- lavPredict(fit)
pred <- as.data.frame(pred)
rownames(pred)<-rownames(dff)
pred$person_nb <- rownames(pred)
plot(pred[,1],pred[,2])
mean(pred[,1])
summary(lm(pred[,1]~temp))
colnames(pred)

#write.table(pred,"gf_log_wo_f.txt")



#-------------------------
