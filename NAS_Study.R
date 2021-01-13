library(magrittr)
library(ggplot2)
suppressMessages(suppressWarnings(library(magrittr)))
suppressMessages(suppressWarnings(library(readxl)))


data = read.csv('/Users/liangchen/Desktop/BSTA630/final project/Data_NAS_Study.txt',sep='\t',header=T)


# Dimension of the whole data frame
dim(data)
# Show the first 10  rows of the whole data frame
head(x = data, n = 10)
# Dimension of a data frame
data_acu = data[(data$Acupressure)==1,]
data_acu_o = data[(data$Acupressure)==2,]
#sample variance and standard deviation of hospital stay
mean(data$Length_of_stay)
median(data$Length_of_stay)
var(x = data$Length_of_stay)
sd(x = data$Length_of_stay)

mean(data_acu$Length_of_stay)
median(data_acu$Length_of_stay)
var(x = data_acu$Length_of_stay)
sd(x = data_acu$Length_of_stay)

mean(data_acu_o$Length_of_stay)
median(data_acu_o$Length_of_stay)
var(x = data_acu_o$Length_of_stay)
sd(x = data_acu_o$Length_of_stay)

# 1st and 3rd interquatile range of hospital stay
quantile(x = data$Length_of_stay, probs = c(0.25,0.75))
# 1st and 3rd interquatile range of hospital stay with Acu
quantile(x = data_acu$Length_of_stay, probs = c(0.25,0.75))
# 1st and 3rd interquatile range of hospital stay w/o Acu
quantile(x = data_acu_o$Length_of_stay, probs = c(0.25,0.75))
# histogram of hospital stay
p = ggplot(data, aes(x = Length_of_stay))
p + geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(Length_of_stay)), 
             linetype = "dashed",color = 'red',size = 0.6)
# histogram of hospital stay with Acu
data_acu = data[(data$Acupressure)==1,]
p = ggplot(data_acu, aes(x = Length_of_stay))
p + geom_histogram(bins = 50, color = "black", fill = "gray") +
geom_vline(aes(xintercept = mean(Length_of_stay)), 
             linetype = "dashed", color = 'red',size = 0.6)

# histogram of hospital stay w/o Acu
data_acu_o = data[(data$Acupressure)==2,]
p = ggplot(data_acu_o, aes(x = Length_of_stay))
p + geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(Length_of_stay)), 
             linetype = "dashed", color = 'red',size = 0.6)


# density of hospital stay
p = ggplot(data, aes(x = Length_of_stay))
p + geom_density() +
  geom_vline(aes(xintercept = mean(Length_of_stay)), 
             linetype = "dashed", size = 0.6)
# Change y axis to count instead of density
p + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(Length_of_stay)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

# density of hospital stay with acu
p = ggplot(data_acu, aes(x = Length_of_stay))
p + geom_density() +
  geom_vline(aes(xintercept = mean(Length_of_stay)), 
             linetype = "dashed", size = 0.6)
# Change y axis to count instead of density
p + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(Length_of_stay)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

# density of hospital stay without acu
p = ggplot(data_acu_o, aes(x = Length_of_stay))
p + geom_density() +
  geom_vline(aes(xintercept = mean(Length_of_stay)), 
             linetype = "dashed", size = 0.6)
# Change y axis to count instead of density
p + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(Length_of_stay)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

# density function after log transformation 

# density of hospital stay
p = ggplot(data, aes(x = log(Length_of_stay)))
p + geom_density() +
  geom_vline(aes(xintercept = mean(log(Length_of_stay))), 
             linetype = "dashed", size = 0.6)
# Change y axis to count instead of density
p + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(log(Length_of_stay))), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

# density of hospital stay with acu
p = ggplot(data_acu, aes(x = log(Length_of_stay)))
p + geom_density() +
  geom_vline(aes(xintercept = mean(log(Length_of_stay))), 
             linetype = "dashed", size = 0.6)
# Change y axis to count instead of density
p + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(log(Length_of_stay))), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")

# density of hospital stay without acu
p = ggplot(data_acu_o, aes(x = log(Length_of_stay)))
p + geom_density() +
  geom_vline(aes(xintercept = mean(log(Length_of_stay))), 
             linetype = "dashed", size = 0.6)
# Change y axis to count instead of density
p + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(log(Length_of_stay))), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")
# binomial distribution for categorical variables
size  = 77
# heroin use
p = (nrow(data[data$Maternal_heroin_use == 1,]))/size
plot(0:size,dbinom(0:size,size=size,prob=p),
     type='h',
     main='Binomial Distribution for heroin use',
     ylab='Probability',
     xlab ='# of Heroin Use',
     lwd=3)
p
size*p*(1-p)
# methadone use
p = (nrow(data[data$Maternal_methadone_use == 1,]))/size
plot(0:size,dbinom(0:size,size=size,prob=p),
     type='h',
     main='Binomial Distribution for methadone use',
     ylab='Probability',
     xlab ='# of Methadone Use',
     lwd=3)
p
size*p*(1-p)
# cocaine use
p = (nrow(data[data$Maternal_cocaine_use == 1,]))/size
plot(0:size,dbinom(0:size,size=size,prob=p),
     type='h',
     main='Binomial Distribution for cocaine use',
     ylab='Probability',
     xlab ='# of Cocaine Use',
     lwd=3)
p
size*p*(1-p)
# tobacco use
p = (nrow(data[data$Maternal_tobacco_use == 1,]))/size
plot(0:size,dbinom(0:size,size=size,prob=p),
     type='h',
     main='Binomial Distribution for tobacco use',
     ylab='Probability',
     xlab ='# of Tobacco Use',
     lwd=3)
p
size*p*(1-p)
# alcohol use
p = (nrow(data[data$Maternal_alcohol_use == 1,]))/size
plot(0:size,dbinom(0:size,size=size,prob=p),
     type='h',
     main='Binomial Distribution for alcohol use',
     ylab='Probability',
     xlab ='# of Alcohol Use',
     lwd=3)
p
size*p*(1-p)
# Homeless
p = (nrow(data[data$Homeless == 1,]))/size
plot(0:size,dbinom(0:size,size=size,prob=p),
     type='h',
     main='Binomial Distribution for Homeless',
     ylab='Probability',
     xlab ='# of Homeless',
     lwd=3)
p
size*p*(1-p)
# Incarcerated
p = (nrow(data[data$Incarcerated == 1,]))/size
plot(0:size,dbinom(0:size,size=size,prob=p),
     type='h',
     main='Binomial Distribution for Incarcerated',
     ylab='Probability',
     xlab ='# of Incarcerated',
     lwd=3)
p
size*p*(1-p)
# Baby gender
p = (nrow(data[data$Baby_gender == 'M',]))/size
plot(0:size,dbinom(0:size,size=size,prob=p),
     type='h',
     main='Binomial Distribution for Baby gender',
     ylab='Probability',
     xlab ='# of Males',
     lwd=3)
p
size*p*(1-p)
# Acupressure
p = (nrow(data[data$Acupressure == 1,]))/size
plot(0:size,dbinom(0:size,size=size,prob=p),
     type='h',
     main='Binomial Distribution for Acupressure',
     ylab='Probability',
     xlab ='# Successes',
     lwd=3)
p
size*p*(1-p)

# MLE estimation for mean and variance (assume normality)
# total
n = length(data$Length_of_stay)
total.mean.MLE = mean(data$Length_of_stay)
total.var.MLE = ((n-1)/n)*var(data$Length_of_stay)
# with acu
n = length(data_acu$Length_of_stay)
Acu.mean.MLE = mean(data_acu$Length_of_stay)
Acu.var.MLE = ((n-1)/n)*var(data_acu$Length_of_stay)
#w/o acu
n = length(data_acu_o$Length_of_stay)
Acu_o.mean.MLE = mean(data_acu_o$Length_of_stay)
Acu_o.var.MLE = ((n-1)/n)*var(data_acu_o$Length_of_stay)
total.mean.MLE
total.var.MLE 
Acu.mean.MLE
Acu.var.MLE 
Acu_o.mean.MLE
Acu_o.var.MLE 

# CI 
# 95% confidence interval for mu assuming sigma is unknown
n = length(data$Length_of_stay)
mean.sample = mean(data$Length_of_stay)
var.sample = var(data$Length_of_stay)
sd.sample = sd(data$Length_of_stay)
# lower/upper bounds
# use t.test function to extract the 95% CI
t.test(x = data$Length_of_stay, conf.level = 0.95)$conf.int
t.test(x = data_acu$Length_of_stay, conf.level = 0.95)$conf.int
t.test(x = data_acu_o$Length_of_stay, conf.level = 0.95)$conf.int
# binomial variables
# heroin
x = nrow(data[data$Maternal_heroin_use == 1,])
prop.test(x = x, n = 77, correct = FALSE)
# methadone
x = nrow(data[data$Maternal_methadone_use == 1,])
prop.test(x = x, n = 77, correct = FALSE)
# cocaine
x = nrow(data[data$Maternal_cocaine_use == 1,])
prop.test(x = x, n = 77, correct = FALSE)
# tabacco
x = nrow(data[data$Maternal_tobacco_use == 1,])
prop.test(x = x, n = 77, correct = FALSE)
# alcohol
x = nrow(data[data$Maternal_alcohol_use == 1,])
prop.test(x = x, n = 77, correct = FALSE)
# Incarcerated
x = nrow(data[data$Incarcerated == 1,])
prop.test(x = x, n = 77, correct = FALSE)
# Baby gender
x = nrow(data[data$Baby_gender == 'M',])
prop.test(x = x, n = 77, correct = FALSE)
# Acupressure
x = nrow(data[data$Acupressure == 1,])
prop.test(x = x, n = 77, correct = FALSE)
# Homeless
x = nrow(data[data$Homeless == 1,])
binom.test(x,77,conf.level = 0.95)


# Two-sample hypothesis testing

# assume data is normal distribution
t.test(
  x = data_acu$Length_of_stay,
  y = data_acu_o$Length_of_stay,
  mu = 0,
  var.equal = TRUE,
  alternative = "two.sided",
  conf.level = 0.95
)
t.test(
  x = data_acu$Length_of_stay,
  y = data_acu_o$Length_of_stay,
  mu = 0,
  var.equal = FALSE,
  alternative = "two.sided",
  conf.level = 0.95
)

# F test equality of two variances
var.test(
  x = data_acu$Length_of_stay,
  y = data_acu_o$Length_of_stay,
  ratio = 1,
  alternative = "two.sided",
  conf.level = 0.95
)


# bootstrap
n_bootstrap = 25
sample.x = sapply(
  X = 1:2000,
  FUN = function(a){
      sample(
        x = data_acu$Length_of_stay,
        size = n_bootstrap,
        replace = TRUE
      )
  }
)
sample.y = sapply(
  X = 1:2000,
  FUN = function(a){
    sample(
      x = data_acu_o$Length_of_stay,
      size = n_bootstrap,
      replace = TRUE
    )
  }
)
bootstrap.p.value = 0
bootstrap.lower.CI = 0
bootstrap.upper.CI = 0
bootstrap.lower.CI.sec = 0
bootstrap.upper.CI.sec = 0
for (i in 1:2000){
  x = sample.x[((25*(i-1))+1):(25*i)]
  y = sample.y[((25*(i-1))+1):(25*i)]
  bootstrap.ttest = t.test(
    x = x,
    y = y,
    mu = 0,
    var.equal = TRUE,
    alternative = "two.sided",
    conf.level = 0.95
  )
  bootstrap.p.value = bootstrap.p.value + bootstrap.ttest[3]$p.value
  bootstrap.lower.CI = bootstrap.lower.CI + bootstrap.ttest[4]$conf.int[1]
  bootstrap.upper.CI = bootstrap.upper.CI + bootstrap.ttest[4]$conf.int[2]
  sample.difference = x - y
  # bootstrap 95% CI when assuming statistic mean(length.of.stay.bootstrap) is normal distribution
  # i.e., sampling distribution is normal (no assumption about the data)
  bootstrap.upper.CI.sec = bootstrap.upper.CI.sec + mean(sample.difference) + qnorm(0.975)*sd(sample.difference)
  bootstrap.lower.CI.sec = bootstrap.lower.CI.sec + mean(sample.difference) - qnorm(0.975)*sd(sample.difference)

}

bootstrap.p.value = bootstrap.p.value / 2000
bootstrap.lower.CI = bootstrap.lower.CI / 2000
bootstrap.upper.CI = bootstrap.upper.CI / 2000
bootstrap.lower.CI.sec = bootstrap.lower.CI.sec / 2000
bootstrap.upper.CI.sec = bootstrap.upper.CI.sec /2000
bootstrap.p.value 
bootstrap.lower.CI 
bootstrap.upper.CI
bootstrap.lower.CI.sec
bootstrap.upper.CI.sec 



# Wilcoxon rank-sum test (Approximation, Sample number D is larger than 16)
# without normality assumption
# one sample
wilcox.test(
  x = data_acu$Length_of_stay,
  y = data_acu_o$Length_of_stay,
  alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE
)
#bootstrap
n_bootstrap = 25
wilcox.bootstrap = sapply(
  X = 1:2000,
  FUN = function(a){
    wilcox.test(
      x = data_acu$Length_of_stay,
      y = data_acu_o$Length_of_stay,
      alternative = "two.sided", paired = FALSE, exact = FALSE, correct = TRUE
    )
  }
)
wilcox.bootstrap[3]
p.value = 0
for (i in 1 : length(wilcox.bootstrap)) {
  if ((i-1) %% 7 == 0) {
    p.value = p.value + as.numeric(wilcox.bootstrap[i+2])
  }
}
p.value
# bootstrap p value
mean.p.value = p.value/2000
mean.p.value


# Linear regression model
data$Race = as.factor(data$Race)
data$Maternal_heroin_use = as.factor(data$Maternal_heroin_use)
data$Maternal_methadone_use = as.factor(data$Maternal_methadone_use)
data$Maternal_cocaine_use = as.factor(data$Maternal_cocaine_use)
data$Maternal_tobacco_use = as.factor(data$Maternal_tobacco_use)
data$Incarcerated = as.factor(data$Incarcerated)
data$Homeless = as.factor(data$Homeless)
data$Baby_gender = as.factor(data$Baby_gender)
data$Size_for_GA = as.factor(data$Size_for_GA)
data$Acupressure = as.factor(data$Acupressure)

model1 = lm(Length_of_stay ~ Maternal_age + Race + Maternal_heroin_use +
            Maternal_methadone_use + Maternal_cocaine_use + Maternal_tobacco_use +
            Maternal_alcohol_use + Incarcerated + Homeless + Baby_gender + BW +
            GA + Size_for_GA + Hrs_to_1st_med + Av_Nas_score + Acupressure, 
            data = data, x = TRUE)
summary(model1)
model1
#normality
std.resi.hat = rstandard(model1)
qqnorm(y = std.resi.hat, pch = 16, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
qqline(y = std.resi.hat, col = "red")
title(main = "No Transformation", cex = 2, outer = TRUE)
hist(
  x = std.resi.hat, freq = FALSE,
  xlim = range(density(std.resi.hat)$x), ylim = c(0, max(density(std.resi.hat)$y)*1.1),
  cex.lab = 1.3, cex.axis = 1.5, cex.main = 1.5,
  main = "", xlab = "Studentized residuals"
)
lines(x = density(std.resi.hat), col = "red")
# log trans
model11 = lm(log(Length_of_stay) ~ Maternal_age + Race + Maternal_heroin_use +
               Maternal_methadone_use + Maternal_cocaine_use + Maternal_tobacco_use +
               Maternal_alcohol_use + Incarcerated + Homeless + Baby_gender + BW +
               GA + Size_for_GA + Hrs_to_1st_med + Av_Nas_score + Acupressure, 
             data = data, x = TRUE)
summary(model11)
std.resi.hat = rstandard(model11)
qqnorm(y = std.resi.hat, pch = 16, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
qqline(y = std.resi.hat, col = "red")
hist(
  x = std.resi.hat, freq = FALSE,
  xlim = range(density(std.resi.hat)$x), ylim = c(0, max(density(std.resi.hat)$y)*1.1),
  cex.lab = 1.3, cex.axis = 1.5, cex.main = 1.5,
  main = "", xlab = "Studentized residuals"
)
lines(x = density(std.resi.hat), col = "red")
# linearity
# Maternal_age
plot(
  x = data$Maternal_age, y = data$Length_of_stay,
  xlim = c(min(data$Maternal_age)-2,max(data$Maternal_age)+2), 
  ylim = c(min(data$Length_of_stay)-5,max(data$Length_of_stay)+5), axes = FALSE,
  xlab = "Maternal_age",
  ylab = "Length_of_stay",
  cex.lab = 1.5, cex.axis = 1.5, cex = 1.5
)
box()
axis(side = 1, at = seq(min(data$Maternal_age)-2,max(data$Maternal_age)+2,5), 
     cex.axis = 1.3, xpd = TRUE)
axis(side = 2, at = seq(min(data$Length_of_stay)-5,max(data$Length_of_stay)+5,15),
     cex.axis = 1.3, las = 1)
points(
  x = lowess(x = data$Maternal_age, y = data$Length_of_stay),
  type = "l", col = "blue", lwd = 2, lty = 1
)
# BW
plot(
  x = data$BW, y = data$Length_of_stay,
  xlim = c(min(data$BW)-2,max(data$BW)+2), 
  ylim = c(min(data$Length_of_stay)-5,max(data$Length_of_stay)+5), axes = FALSE,
  xlab = "BW",
  ylab = "Length_of_stay",
  cex.lab = 1.5, cex.axis = 1.5, cex = 1.5
)
box()
axis(side = 1, at = seq(min(data$BW)-2,max(data$BW)+2,800), 
     cex.axis = 1.3, xpd = TRUE)
axis(side = 2, at = seq(min(data$Length_of_stay)-5,max(data$Length_of_stay)+5,15),
     cex.axis = 1.3, las = 1)
points(
  x = lowess(x = data$BW, y = data$Length_of_stay),
  type = "l", col = "blue", lwd = 2, lty = 1
)
#GA
plot(
  x = data$GA, y = data$Length_of_stay,
  xlim = c(min(data$GA)-2,max(data$GA)+2), 
  ylim = c(min(data$Length_of_stay)-5,max(data$Length_of_stay)+5), axes = FALSE,
  xlab = "GA",
  ylab = "Length_of_stay",
  cex.lab = 1.5, cex.axis = 1.5, cex = 1.5
)
box()
axis(side = 1, at = seq(min(data$GA)-2,max(data$GA)+2,3), 
     cex.axis = 1.3, xpd = TRUE)
axis(side = 2, at = seq(min(data$Length_of_stay)-5,max(data$Length_of_stay)+5,15),
     cex.axis = 1.3, las = 1)
points(
  x = lowess(x = data$GA, y = data$Length_of_stay),
  type = "l", col = "blue", lwd = 2, lty = 1
)
# Hrs_to_1st_med 
plot(
  x = data$Hrs_to_1st_med, y = data$Length_of_stay,
  xlim = c(min(data$Hrs_to_1st_med)-2,max(data$Hrs_to_1st_med)+2), 
  ylim = c(min(data$Length_of_stay)-5,max(data$Length_of_stay)+5), axes = FALSE,
  xlab = "Hrs_to_1st_med",
  ylab = "Length_of_stay",
  cex.lab = 1.5, cex.axis = 1.5, cex = 1.5
)
box()
axis(side = 1, at = seq(min(data$Hrs_to_1st_med)-2,max(data$Hrs_to_1st_med)+2,15), 
     cex.axis = 1.3, xpd = TRUE)
axis(side = 2, at = seq(min(data$Length_of_stay)-5,max(data$Length_of_stay)+5,15),
     cex.axis = 1.3, las = 1)
points(
  x = lowess(x = data$Hrs_to_1st_med, y = data$Length_of_stay),
  type = "l", col = "blue", lwd = 2, lty = 1
)
#Av_Nas_score
plot(
  x = data$Av_Nas_score, y = data$Length_of_stay,
  xlim = c(min(data$Av_Nas_score)-2,max(data$Av_Nas_score)+2), 
  ylim = c(min(data$Length_of_stay)-5,max(data$Length_of_stay)+5), axes = FALSE,
  xlab = "Av_Nas_score",
  ylab = "Length_of_stay",
  cex.lab = 1.5, cex.axis = 1.5, cex = 1.5
)
box()
axis(side = 1, at = seq(min(data$Av_Nas_score)-2,max(data$Av_Nas_score)+2,2), 
     cex.axis = 1.3, xpd = TRUE)
axis(side = 2, at = seq(min(data$Length_of_stay)-5,max(data$Length_of_stay)+5,15),
     cex.axis = 1.3, las = 1)
points(
  x = lowess(x = data$Av_Nas_score, y = data$Length_of_stay),
  type = "l", col = "blue", lwd = 2, lty = 1
)

Maternal_age.sq = (data$Maternal_age)^2
BW.sq = (data$BW)^2
GA.sq = (data$GA)^2
Hrs_to_1st_med.sq = (data$Hrs_to_1st_med)^2
Av_Nas_score.sq = (data$Av_Nas_score)^2
Maternal_age.cu = (data$Maternal_age)^3
BW.cu = (data$BW)^3
GA.cu = (data$GA)^3
Hrs_to_1st_med.cu = (data$Hrs_to_1st_med)^3
Av_Nas_score.cu = (data$Av_Nas_score)^3
model1.quadra = lm(Length_of_stay ~  Race 
                   + Maternal_heroin_use + Maternal_methadone_use + Maternal_cocaine_use 
                   + Maternal_tobacco_use + Maternal_alcohol_use + Incarcerated + Homeless 
                   + Baby_gender + BW + BW.sq + GA + GA.sq  + Size_for_GA + 
                     Hrs_to_1st_med + Hrs_to_1st_med.sq + Av_Nas_score
                   + Av_Nas_score.sq + Acupressure, 
                   data = data, x = TRUE)
summary(model1.quadra)


model1.cub = lm(Length_of_stay ~  Race 
                   + Maternal_heroin_use + Maternal_methadone_use + Maternal_cocaine_use 
                   + Maternal_tobacco_use + Maternal_alcohol_use + Incarcerated + Homeless 
                   + Baby_gender + BW + BW.cu + GA + GA.cu + Size_for_GA + 
                     Hrs_to_1st_med + Hrs_to_1st_med.cu + Av_Nas_score
                   + Av_Nas_score.cu + Acupressure, 
                   data = data, x = TRUE)
summary(model1.cub)


model1.cub.log = lm(log(Length_of_stay) ~  Race 
                + Maternal_heroin_use + Maternal_methadone_use + Maternal_cocaine_use 
                + Maternal_tobacco_use + Maternal_alcohol_use + Incarcerated + Homeless 
                + Baby_gender + BW + BW.cu + GA + GA.cu + Size_for_GA + 
                  Hrs_to_1st_med + Hrs_to_1st_med.cu + Av_Nas_score
                + Av_Nas_score.cu + Acupressure, 
                data = data, x = TRUE)
summary(model1.cub.log)


model1.quacub = lm(Length_of_stay ~  Race 
                   + Maternal_heroin_use + Maternal_methadone_use + Maternal_cocaine_use 
                   + Maternal_tobacco_use + Maternal_alcohol_use + Incarcerated + Homeless 
                   + Baby_gender + BW + BW.sq + BW.cu + GA + GA.sq + GA.cu + Size_for_GA + 
                     Hrs_to_1st_med + Hrs_to_1st_med.sq + Hrs_to_1st_med.cu + Av_Nas_score
                   + Av_Nas_score.sq + Av_Nas_score.cu + Acupressure, 
            data = data, x = TRUE)
summary(model1.quacub)

model1.quacub.log = lm(log(Length_of_stay) ~  Race 
                   + Maternal_heroin_use + Maternal_methadone_use + Maternal_cocaine_use 
                   + Maternal_tobacco_use + Maternal_alcohol_use + Incarcerated + Homeless 
                   + Baby_gender + BW + BW.sq + BW.cu + GA + GA.sq + GA.cu + Size_for_GA + 
                     Hrs_to_1st_med + Hrs_to_1st_med.sq + Hrs_to_1st_med.cu + Av_Nas_score
                   + Av_Nas_score.sq + Av_Nas_score.cu + Acupressure, 
                   data = data, x = TRUE)
summary(model1.quacub.log)

model1.quadra.log = lm(log(Length_of_stay) ~  Race 
                   + Maternal_heroin_use + Maternal_methadone_use + Maternal_cocaine_use 
                   + Maternal_tobacco_use + Maternal_alcohol_use + Incarcerated + Homeless 
                   + Baby_gender + BW + BW.sq + GA + GA.sq  + Size_for_GA + 
                     Hrs_to_1st_med + Hrs_to_1st_med.sq + Av_Nas_score
                   + Av_Nas_score.sq + Acupressure, 
                   data = data, x = TRUE)
summary(model1.quadra.log)
# Diagnostic --------------------------------------------------------------
which(apply(influence.measures(model1.quadra.log)$is.inf, 1, any))









model2 = lm(Length_of_stay ~ Maternal_age + Race + Maternal_heroin_use +
              Maternal_methadone_use + Maternal_cocaine_use + Maternal_tobacco_use +
              Maternal_alcohol_use + Incarcerated + Homeless + Acupressure, 
            data = data, x = TRUE)
summary(model2)

model3 = lm(Length_of_stay ~  Baby_gender + BW +
              GA + Size_for_GA + Hrs_to_1st_med + Av_Nas_score + Acupressure, 
            data = data, x = TRUE)
summary(model3)


model4 = lm(Length_of_stay ~ Maternal_age + Race + 
              Maternal_methadone_use + Maternal_cocaine_use + Maternal_tobacco_use +
              Maternal_alcohol_use + Incarcerated + Homeless + Acupressure, 
            data = data, x = TRUE)
summary(model4)

model5 = lm(Length_of_stay ~  Baby_gender + BW +
              GA + Size_for_GA + Hrs_to_1st_med + Acupressure, 
            data = data, x = TRUE)
summary(model5)


model6 = lm(Length_of_stay ~  Baby_gender + BW +
              GA + Hrs_to_1st_med + Acupressure, 
            data = data, x = TRUE)
summary(model6)

rstandard(model6)

# race and incarcerated
x11 = sum(data$Race == 'A' & data$Incarcerated == 1)
x12 = sum(data$Race == 'A' & data$Incarcerated == 2)
x21 = sum(data$Race == 'C' & data$Incarcerated == 1)
x22 = sum(data$Race == 'C' & data$Incarcerated == 2)
x31 = sum(data$Race == 'H' & data$Incarcerated == 1)
x32 = sum(data$Race == 'H' & data$Incarcerated == 2)
n1 = sum(data$Race == 'A')
n2 = sum(data$Race == 'C')
n3 = sum(data$Race == 'H')
E.11 = (x11 + x21 + x31)*n1/(n1 + n2 + n3)
E.12 = (x12 + x22 + x32)*n1/(n1 + n2 + n3)
E.21 = (x11 + x21 + x31)*n2/(n1 + n2 + n3)
E.22 = (x12 + x22 + x32)*n2/(n1 + n2 + n3)
E.31 = (x11 + x21 + x31)*n3/(n1 + n2 + n3)
E.32 = (x12 + x22 + x32)*n3/(n1 + n2 + n3)
E.11 
E.12 
E.21 
E.22 
E.31 
E.32
chisq.test(
  matrix(
    data = c(x11,x21,x31,x12,x22,x32),
    nrow = 3
  )
)
# race and maternal use of heroin
x11 = sum(data$Race == 'A' & data$Maternal_heroin_use == 1)
x12 = sum(data$Race == 'A' & data$Maternal_heroin_use == 2)
x21 = sum(data$Race == 'C' & data$Maternal_heroin_use == 1)
x22 = sum(data$Race == 'C' & data$Maternal_heroin_use == 2)
x31 = sum(data$Race == 'H' & data$Maternal_heroin_use == 1)
x32 = sum(data$Race == 'H' & data$Maternal_heroin_use == 2)
n1 = sum(data$Race == 'A')
n2 = sum(data$Race == 'C')
n3 = sum(data$Race == 'H')
E.11 = (x11 + x21 + x31)*n1/(n1 + n2 + n3)
E.12 = (x12 + x22 + x32)*n1/(n1 + n2 + n3)
E.21 = (x11 + x21 + x31)*n2/(n1 + n2 + n3)
E.22 = (x12 + x22 + x32)*n2/(n1 + n2 + n3)
E.31 = (x11 + x21 + x31)*n3/(n1 + n2 + n3)
E.32 = (x12 + x22 + x32)*n3/(n1 + n2 + n3)
E.11 
E.12 
E.21 
E.22 
E.31 
E.32
# fisher's test
fisher.test(
  matrix(
    data = c(x11,x21,x31,x12,x22,x32),
    nrow = 3
  )
)

# race and maternal use of methadone
x11 = sum(data$Race == 'A' & data$Maternal_methadone_use == 1)
x12 = sum(data$Race == 'A' & data$Maternal_methadone_use == 2)
x21 = sum(data$Race == 'C' & data$Maternal_methadone_use == 1)
x22 = sum(data$Race == 'C' & data$Maternal_methadone_use == 2)
x31 = sum(data$Race == 'H' & data$Maternal_methadone_use == 1)
x32 = sum(data$Race == 'H' & data$Maternal_methadone_use == 2)
n1 = sum(data$Race == 'A')
n2 = sum(data$Race == 'C')
n3 = sum(data$Race == 'H')
E.11 = (x11 + x21 + x31)*n1/(n1 + n2 + n3)
E.12 = (x12 + x22 + x32)*n1/(n1 + n2 + n3)
E.21 = (x11 + x21 + x31)*n2/(n1 + n2 + n3)
E.22 = (x12 + x22 + x32)*n2/(n1 + n2 + n3)
E.31 = (x11 + x21 + x31)*n3/(n1 + n2 + n3)
E.32 = (x12 + x22 + x32)*n3/(n1 + n2 + n3)
E.11 
E.12 
E.21 
E.22 
E.31 
E.32

fisher.test(
  matrix(
    data = c(x11,x21,x31,x12,x22,x32),
    nrow = 3
  )
)

# race and maternal use of alcohol
x11 = sum(data$Race == 'A' & data$Maternal_alcohol_use == 1)
x12 = sum(data$Race == 'A' & data$Maternal_alcohol_use == 2)
x21 = sum(data$Race == 'C' & data$Maternal_alcohol_use == 1)
x22 = sum(data$Race == 'C' & data$Maternal_alcohol_use == 2)
x31 = sum(data$Race == 'H' & data$Maternal_alcohol_use == 1)
x32 = sum(data$Race == 'H' & data$Maternal_alcohol_use == 2)
x11
x12
x21
x22
x31
x32
n1 = sum(data$Race == 'A')
n2 = sum(data$Race == 'C')
n3 = sum(data$Race == 'H')
E.11 = (x11 + x21 + x31)*n1/(n1 + n2 + n3)
E.12 = (x12 + x22 + x32)*n1/(n1 + n2 + n3)
E.21 = (x11 + x21 + x31)*n2/(n1 + n2 + n3)
E.22 = (x12 + x22 + x32)*n2/(n1 + n2 + n3)
E.31 = (x11 + x21 + x31)*n3/(n1 + n2 + n3)
E.32 = (x12 + x22 + x32)*n3/(n1 + n2 + n3)
E.11 
E.12 
E.21 
E.22 
E.31 
E.32

chisq.test(
  matrix(
    data = c(x11,x21,x31,x12,x22,x32),
    nrow = 3
  )
)

# race and homeless
x11 = sum(data$Race == 'A' & data$Homeless == 1)
x12 = sum(data$Race == 'A' & data$Homeless == 2)
x21 = sum(data$Race == 'C' & data$Homeless == 1)
x22 = sum(data$Race == 'C' & data$Homeless == 2)
x31 = sum(data$Race == 'H' & data$Homeless == 1)
x32 = sum(data$Race == 'H' & data$Homeless == 2)
n1 = sum(data$Race == 'A')
n2 = sum(data$Race == 'C')
n3 = sum(data$Race == 'H')
E.11 = (x11 + x21 + x31)*n1/(n1 + n2 + n3)
E.12 = (x12 + x22 + x32)*n1/(n1 + n2 + n3)
E.21 = (x11 + x21 + x31)*n2/(n1 + n2 + n3)
E.22 = (x12 + x22 + x32)*n2/(n1 + n2 + n3)
E.31 = (x11 + x21 + x31)*n3/(n1 + n2 + n3)
E.32 = (x12 + x22 + x32)*n3/(n1 + n2 + n3)
E.11 
E.12 
E.21 
E.22 
E.31 
E.32

fisher.test(
  matrix(
    data = c(x11,x21,x31,x12,x22,x32),
    nrow = 3
  )
)





