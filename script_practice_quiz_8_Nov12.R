###load data
library(tidyverse)
library(haven)
library(apaTables)

my.data <- read_spss("Lecture 7 regression_example_data.sav")
glimpse(my.data)

### get initial descriptives
apa.cor.table(my.data)
## use ac ratings, has a correlation of .37 with job performance

##check for curve. relationship
psych::pairs.panels(as.data.frame(my.data)) # no concerns 


##gma regression
gmaDV1.regression <- lm(jobperf~gma, data=my.data)
summary(gmaDV1.regression)

apa.reg.table(gmaDV1.regression)

#gma r = .51, sr2 = .26, 95% CI = .20, .31

##gma and con regression
gmacon.regression <- lm(jobperf~gma + con, data=my.data)
summary(gmaDV1.regression)

apa.reg.table(gmacon.regression)
summary(gmacon.regression)
# gma and con R2 = .37, 95% CI[.29,.41]
# consr2 = .10, 95% CI[.05,.14]

###as a hierarchical regression
head(my.data)
reg1 <- lm(jobperf ~ gma + con, data=my.data)
#run anova
anova(reg1)
apa.reg.table(reg1)
# Delta R2 = .36 - significant, adding con contriubted to model 
#95% CI[.29, .41]

##QUESTION 2
# gma and assessment centre ratings 
gmaac.regression <- lm(jobperf~gma + ac, data=my.data)
summary(gmaac.regression)

apa.reg.table(gmaac.regression)
summary(gmaac.regression)

##QUESTION 3
# gma and graph
gmagraph.regression <- lm(jobperf~gma + graph, data=my.data)
summary(gmagraph.regression)

apa.reg.table(gmagraph.regression)
summary(gmagraph.regression)

###BLOCK REGRESSIONS
# gma and con

block1 = lm(jobperf~gma,data=my.data)

block2 = lm(jobperf~gma+con,data=my.data)

apa.reg.table(block1, block2)

# gma and ac
block1 = lm(jobperf~gma,data=my.data)

block3 = lm(jobperf~gma+ac,data=my.data)

apa.reg.table(block1, block3)

# gma and graph
block1 = lm(jobperf~gma,data=my.data)

block4 = lm(jobperf~gma+graph,data=my.data)

apa.reg.table(block1, block4)

###CONFIDENCE INTERVALS
## find mean
apa.cor.table(my.data)
#gma mean = 100
#con mean = 120

mean_score_CIs <- data.frame(gma = c(100),con=c(120))
CI_data <- predict(my.regression,
                   newdata = mean_score_CIs, interval = "confidence", level = 0.95)
CI_data <- as.data.frame(cbind(mean_score_CIs, CI_data))
print(CI_data)

###PREDICTION INTERVALS

                    