## analisis de la data mundo
# lectura de la data
library(readxl)
mundo <- read_excel("D:/2021-II/mineria/TRABAJOS/DATA MUNDO/Data-mining/data/mundo.xlsx")
colSums(is.na(mundo)) #NA por variable
sum(is.na(mundo))
dim(mundo)
#(calories, lit_male, lit_fema) tienen datos "NA" mayor al 15%
#por lo que no se les considera para el estudio

#elimindando las variables (calories, lit_male, lit_fema)
names(mundo)
drops<- c("country", "calories", "lit_male", "lit_fema")
new.mundo<- mundo[,!(names(mundo) %in% drops)]
summary(new.mundo)
dim(new.mundo)      #nueva dimension de la data

#revisando los valores NA
library(VIM)
aggr(new.mundo, numbers=T, sortVar=T,cex.axis = 0.6)
colSums(is.na(new.mundo)) #valores NA por variables
sum(is.na(new.mundo))     #numero de NAs presentes en la nueva data

#imputacion de los valores NA
####imputacion por la media
library(mice)
columns <- c(names(new.mundo))
imputed_data <- mice(new.mundo[,names(new.mundo) %in% columns],m = 5,
                     maxit = 1, method = "mean",seed = 2018,print=F)
mean.imputation<- mice::complete(imputed_data)

#na.var<-c("urban", "esp_alfabet", "aids", "mortalidad", "aids_rt",
#          "lg_aidsr", "b_to_d", "fertilidad", "cropgrow", "climate")

par(mfrow=c(2,3))
plot(density(mundo$urban,na.rm = T),col=2,main="urban")
lines(density(mean.imputation$urban),col=3)

plot(density(mundo$alfabet,na.rm = T),col=2,main="alfabet")
lines(density(mean.imputation$alfabet),col=3)

plot(density(mundo$aids,na.rm = T),col=2,main="aids")
lines(density(mean.imputation$aids),col=3)

plot(density(mundo$mortalidad,na.rm = T),col=2,main="mortalidad")
lines(density(mean.imputation$mortalidad),col=3)

plot(density(mundo$aids_rt,na.rm = T),col=2,main="aids_rt")
lines(density(mean.imputation$aids_rt),col=3)

plot(density(mundo$lg_aidsr,na.rm = T),col=2,main="lg_aidsr")
lines(density(mean.imputation$lg_aidsr),col=3)

plot(density(mundo$b_to_d,na.rm = T),col=2,main="b_to_d")
lines(density(mean.imputation$b_to_d),col=3)

plot(density(mundo$fertilidad,na.rm = T),col=2,main="fertilidad")
lines(density(mean.imputation$fertilidad),col=3)

plot(density(mundo$cropgrow,na.rm = T),col=2,main="cropgrow")
lines(density(mean.imputation$cropgrow),col=3)

plot(density(mundo$climate,na.rm = T),col=2,main="climate")
lines(density(mean.imputation$climate),col=3)

## Analisis de valores outliers por Boxplot
par(mfrow=c(2,4))
boxplot(mean.imputation[,1],main="poblacion")
boxplot(mean.imputation[,2],main="densidad")
boxplot(mean.imputation[,3],main="urban")
boxplot(mean.imputation[,4],main="esp_vidaf")
boxplot(mean.imputation[,5],main="esp_vidam")
boxplot(mean.imputation[,6],main="alfabet")
boxplot(mean.imputation[,7],main="inc_pobl")
boxplot(mean.imputation[,8],main="mort_inf")
boxplot(mean.imputation[,9],main="gdp_cap")
boxplot(mean.imputation[,10],main="region")
boxplot(mean.imputation[,11],main="aids")
boxplot(mean.imputation[,12],main="natalidad")
boxplot(mean.imputation[,13],main="mortalidad")
boxplot(mean.imputation[,14],main="aids_rt")
boxplot(mean.imputation[,15],main="log_pib")
boxplot(mean.imputation[,16],main="log_aidsr")
par(mfrow=c(2,3))
boxplot(mean.imputation[,17],main="b_ti_d")
boxplot(mean.imputation[,18],main="fertilidad")
boxplot(mean.imputation[,19],main="log_pop")
boxplot(mean.imputation[,20],main="cropgrow")
boxplot(mean.imputation[,21],main="climate")
names(mean.imputation)

#ver los valores outliers por variables
boxplot.stats(mean.imputation$poblacion)$out
boxplot.stats(mean.imputation$densidad)$out
#boxplot.stats(mean.imputation$urban)$out
boxplot.stats(mean.imputation$esp_vidaf)$out
boxplot.stats(mean.imputation$esp_vidam)$out
#boxplot.stats(mean.imputation$alfabet)$out
#boxplot.stats(mean.imputation$inc_pobl)$out
boxplot.stats(mean.imputation$mort_inf)$out
boxplot.stats(mean.imputation$gdp_cap)$out
#boxplot.stats(mean.imputation$region)$out
boxplot.stats(mean.imputation$aids)$out
#boxplot.stats(mean.imputation$natalidad)$out
boxplot.stats(mean.imputation$mortalidad)$out
boxplot.stats(mean.imputation$aids_rt)$out
#boxplot.stats(mean.imputation$Log_pib)$out
#boxplot.stats(mean.imputation$lg_aidsr)$out
boxplot.stats(mean.imputation$b_to_d)$out
#boxplot.stats(mean.imputation$fertilidad)$out
boxplot.stats(mean.imputation$log_pop)$out
boxplot.stats(mean.imputation$cropgrow)$out
#boxplot.stats(mean.imputation$climate)$out

#imputacion de los valores outliers

replace_outliers <- function(x, removeNA = FALSE){
  qrts <- quantile(x, probs = c(0.25, 0.75))
  caps <- quantile(x, probs = c(0.25, 0.75))
  iqr <- qrts[2]-qrts[1]
  h <- 1.5 * iqr
  x[x<qrts[1]-h] <- caps[1]
  x[x>qrts[2]+h] <- caps[2]
  x
}
S.out.pob<- replace_outliers(mean.imputation$poblacion)
S.out.den<- replace_outliers(mean.imputation$densidad)
S.out.vidaf<- replace_outliers(mean.imputation$esp_vidaf)
S.out.vidam<- replace_outliers(mean.imputation$esp_vidam)
S.out.mort_inf<- replace_outliers(mean.imputation$mort_inf)
S.out.gdp<- replace_outliers(mean.imputation$gdp_cap)
S.out.aids<- replace_outliers(mean.imputation$aids)
S.out.mort<- replace_outliers(mean.imputation$mortalidad)
S.out.aids_rt<- replace_outliers(mean.imputation$aids_rt)
S.out.b_to<- replace_outliers(mean.imputation$b_to_d)
S.out.log_pop<- replace_outliers(mean.imputation$log_pop)
S.out.crop<- replace_outliers(mean.imputation$cropgrow)

#comparacion de boxplot para mean.imputation y los valores outliers imputados
par(mfrow=c(1,2))
boxplot(mean.imputation[,1],main="poblacion con outliers")
boxplot(S.out.pob, main="poblacion sin outliers")
boxplot(mean.imputation[,2],main="densidad con outliers")
boxplot(S.out.den, main="densidad sin outliers")
boxplot(mean.imputation[,4],main="esp_vidaf con outliers")
boxplot(S.out.vidaf, main="sp_vidaf sin outliers")
boxplot(mean.imputation[,5],main="esp_vidam con otliers")
boxplot(S.out.vidam,main="esp_vidam sin otliers")
boxplot(mean.imputation[,8],main="mort_inf con outliers")
boxplot(S.out.mort_inf,main="mort_inf sin outliers")
boxplot(mean.imputation[,9],main="gdp_cap con outliers")
boxplot(S.out.gdp,main="gdp_cap sin outliers")
boxplot(mean.imputation[,11],main="aids con outliers")
boxplot(S.out.aids,main="aids sin outliers")
boxplot(mean.imputation[,13],main="mortalidad con outliers")
boxplot(S.out.mort,main="mortalidad sin outliers")
boxplot(mean.imputation[,14],main="aids_rt con outliers")
boxplot(S.out.aids_rt,main="aids_rt sin outliers")
boxplot(mean.imputation[,17],main="b_to_d con outliers")
boxplot(S.out.b_to,main="b_to_d sin outliers")
boxplot(mean.imputation[,19],main="log_pop con outliers")
boxplot(S.out.log_pop,main="log_pop sin outliers")
boxplot(mean.imputation[,20],main="cropgrow con outliers")
boxplot(S.out.crop,main="cropgrow sin outliers")

#Reemplazando los valores sin outliers en una data
final.mundo<-mean.imputation
names(final.mundo)
final.mundo$poblacion <- S.out.pob
final.mundo$densidad <- S.out.den
final.mundo$esp_vidaf <- S.out.vidaf
final.mundo$esp_vidam <- S.out.vidam
final.mundo$mort_inf <- S.out.mort_inf
final.mundo$gdp_cap <- S.out.gdp
final.mundo$aids <- S.out.aids
final.mundo$mortalidad <- S.out.mort
final.mundo$aids_rt <- S.out.aids_rt
final.mundo$b_to_d <- S.out.b_to
final.mundo$log_pop <- S.out.log_pop
final.mundo$cropgrow <- S.out.crop

par(mfrow=c(1,1))
boxplot(final.mundo)

#estandarizacion de la data
library(reshape)
Z.final.mundo<-rescaler(x=final.mundo,type="sd")
Z.final.mundo
summary(Z.final.mundo)

#histograma de algunas variables
par(mfrow=c(1,2))
hist(final.mundo$poblacion)
hist(Z.final.mundo$poblacion)
hist(final.mundo$densidad)
hist(Z.final.mundo$densidad)
hist(final.mundo$urban)
hist(Z.final.mundo$urban)
hist(final.mundo$esp_vidaf)
hist(Z.final.mundo$esp_vidaf)

#plot para ver los efectos de la normalizacion
plot(sort(final.mundo$poblacion))
plot(sort(Z.final.mundo$poblacion))
plot(sort(final.mundo$densidad))
plot(sort(Z.final.mundo$densidad))
