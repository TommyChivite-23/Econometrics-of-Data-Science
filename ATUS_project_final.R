# Keenan Flynn
# Txomin Chivite
# Group 3

getwd()
setwd("C:/Users/Keenan/Documents/5590-Rstudio/Project")

library(car)
library(alr4)
library(faraway)
library(MASS)
library(corrr)
library(dplyr)
library(pastecs)
library(lmtest)

atus.raw <- read.delim('atussum_2021.dat', sep=",")
View(atus.raw)
dim(atus.raw)

#-------------------------------------------------------------------------------

# 1. 
# Exploratory Data Analysis

# Get a count of all non-zero/non-null values for our independent variable
sum(atus.sum$t010101!=0&atus.sum$t010101!=-1)

# Get a count for all non-zero and non-null variables (-1 is null)
# From these counts we can narrow our selection of independent variables 
colSums(atus.raw!=0 & atus.raw!=-1)

# Here we will initialize a new dataframe
# In this dataframe we will pick and choose our variables of interest
# Adding a variable to this clean dataframe does not mean that it will make it into the final model
atus.clean <- data.frame(matrix(nrow=9087, ncol=0))

# Rename variables for ease of use
atus.clean$age.youngest.child <- atus.earnings$TRYHHCHILD
atus.clean$age <- atus.earnings$TEAGE
atus.clean$sex <- atus.earnings$TESEX
atus.clean$schooling.level <- atus.earnings$PEEDUCA
atus.clean$employed <- atus.earnings$TELFS
atus.clean$full.time <- atus.earnings$TRDPFTPT
atus.clean$weekly.earnings <- atus.earnings$TRERNWA/100
atus.clean$num.children <- atus.earnings$TRCHILDNUM
atus.clean$weekly.hours.worked <- atus.earnings$TEHRUSLT
atus.clean$sleep <- atus.earnings$t010101


# Remove row if any column contains a zero or -1 (null)
row_sub = apply(atus.clean, 1, function(row) all(row > 0))
atus.clean <- atus.clean[row_sub,]

# Get columns sums for the refined dataset
colSums(atus.clean!=0 & atus.clean!=-1)

View(atus.clean)
dim(atus.clean)

# Get correlation matrix of selected variables
cor(atus.clean)

# Get summary statistics of selected variables
sapply(atus.clean, sd)

stat.desc(atus.clean)

table(atus.clean$full.time)
#-------------------------------------------------------------------------------

# 2. 
# Model experimentation

# Several combinations of variables were experimented with to try to find a meaningful model
# We experimented with scaling and log-transforming selected variables but found
# effect of the score of the model

# This is where a bulk of time was spent

    # atus.clean$sleep.log <- log(atus.clean$sleep)
    # atus.clean$weekly.earnings.log <- log(atus.clean$weekly.earnings)
    # atus.clean$sleep.scale <- scale(atus.clean$sleep)
    # atus.clean$weekly.earnings.scale <- scale(atus.clean$weekly.earnings)
    # atus.clean$weekly.hours.worked.scale <- scale(atus.clean$weekly.hours.worked)
    # 
    # 
    # naive.model = lm(sleep.log ~ age + weekly.hours.worked + weekly.earnings, data=atus.clean)
    # 
    # naive.model = lm(sleep ~ age + sex + weekly.earnings, data=atus.clean)
    # 
    # naive.model = lm(sleep.scale ~ schooling.level + employed + weekly.hours.worked.scale + weekly.earnings.scale, data=atus.clean)
    # 
    # summary(naive.model)
    # vif(naive.model)
    # residualPlots(naive.model)
    #
    # scatterplotMatrix(~ sleep + schooling.level  + employed + weekly.hours.worked.scale + weekly.earnings.log, data=atus.clean)
    # 
    # boxplot(atus.clean$sleep.log, horizontal=TRUE)
    #     
    #
    # mod_all <- lm(sleep ~ age.youngest.child + age + sex + schooling.level + 
    #                 employed + full.time + weekly.earnings + num.children + 
    #                 weekly.hours.worked, data = atus.clean)
    # summary(mod_all)
    # 
    # mod_red <- lm(sleep ~ age + full.time + weekly.earnings, data = atus.clean)
    # summary(mod_red)
    # 
    # mod_red2 <- lm(sleep ~ age + full.time + weekly.earnings + weekly.hours.worked, data = atus.clean)
    # summary(mod_red2)


#-------------------------------------------------------------------------------

# 3. 
# Model Refinement

# After several tests, we decided to split our model into 2.
# One model will be for full time workers, the other will be for part time workers

fulltime <- subset(atus.clean, full.time == 1)
parttime <- subset(atus.clean, full.time == 2)

dim(fulltime)
dim(parttime)


full_mod <- lm(sleep ~ age  + weekly.earnings + weekly.hours.worked, data = fulltime)
summary(full_mod)

part_mod <- lm(sleep ~ age + weekly.earnings + weekly.hours.worked, data = parttime)
summary(part_mod)

#-------------------------------------------------------------------------------

# 4. 
# Testing Linear Model Assumptions


# Linearity
predicted_sleep_full = predict(full_mod, fulltime)
error_full = fulltime$sleep - predicted_sleep_full
plot(x = predicted_sleep_full, y = error_full)

predicted_sleep_part = predict(part_mod, parttime)
error_part = parttime$sleep - predicted_sleep_part
plot(x = predicted_sleep_part, y = error_part)

# Normal residuals
hist(error_full, freq = FALSE)
dens_full <- density(error_full)
lines(dens_full)

hist(error_part, freq = FALSE)
dens_part <- density(error_part)
lines(dens_part)

# Homoskedasticity
bptest(full_mod, data = fulltime)
bptest(part_mod, data = parttime)

# Multicollinearity
vif(full_mod)
vif(part_mod)







