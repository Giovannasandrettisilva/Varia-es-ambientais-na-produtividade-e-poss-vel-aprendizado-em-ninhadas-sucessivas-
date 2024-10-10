rm(list = ls())
#4.3.3 
#Packages####
install.packages("funspace")
install.packages("glmmTMB")
install.packages("LongituRF")
install.packages("Matrix")
install.packages('TMB', type = 'source')
install.packages("effects")
install.packages("sjPlot")
install.packages("sjmisc")

library(LongituRF)
library(funspace)
library(car)
library(corrplot)
library(lme4)
library(glmmTMB)
library(ggplot2)
library(effects)
library(sjmisc)
library(sjPlot)
library(ggplot2)


#Imputation climatic data####
dadosclimaticos <- read.csv("dados_climaticos_imputacao.csv", sep=";")

#first, testing the correlation between de databases (WorldClim and SIMEPAR) and adjusting some data
#precipitation
precipitation<-data.frame(dadosclimaticos$chuva_acumulada_simepar, dadosclimaticos$chuva_acumulada_worldclim)
precipitation<-na.omit(precipitation)
cor(precipitation$dadosclimaticos.chuva_acumulada_simepar, precipitation$dadosclimaticos.chuva_acumulada_worldclim)

#for precipitation, the correlation is 56.46% (moderate), however, there are two outliers:  1 (2006), 14 (2019)
#plot(precipitation$dadosclimaticos.chuva_acumulada_simepar, precipitation$dadosclimaticos.chuva_acumulada_worldclim)
#in these two outliers, the SIMEPAR had measured a high value of prec., so  I will exclude the WorldClim data for these years.

precipitation$dadosclimaticos.chuva_acumulada_worldclim[1]<-NA
precipitation$dadosclimaticos.chuva_acumulada_worldclim[12]<-NA

precipitation<-na.omit(precipitation)

cor(precipitation$dadosclimaticos.chuva_acumulada_simepar, precipitation$dadosclimaticos.chuva_acumulada_worldclim)
#the new correlation (>70%) is strong

dadosclimaticos$chuva_acumulada_worldclim[1]<-NA
dadosclimaticos$chuva_acumulada_worldclim[14]<-NA

#temperature
#max
temperature<-data.frame(dadosclimaticos$temperatura_maxima_simepar, dadosclimaticos$temperatura_maxima_worldclim)
temperature<-na.omit(temperature)
cor(temperature$dadosclimaticos.temperatura_maxima_simepar, temperature$dadosclimaticos.temperatura_maxima_worldclim)
#for temperature, the correlation is 58.73% (moderate)
#there are two outliers with a difference greater than 2ºC (2013 and 2014)

temperature$dadosclimaticos.temperatura_maxima_worldclim[6]<-NA
temperature$dadosclimaticos.temperatura_maxima_worldclim[7]<-NA

temperature<-na.omit(temperature)

cor(temperature$dadosclimaticos.temperatura_maxima_simepar, temperature$dadosclimaticos.temperatura_maxima_worldclim)
#the new correlation (>70%) is strong

dadosclimaticos$temperatura_maxima_worldclim[8]<-NA
dadosclimaticos$temperatura_maxima_worldclim[9]<-NA

#min
temperature<-data.frame(dadosclimaticos$temperatura_minima_simepar, dadosclimaticos$tempertura_minima_worldclim)

temperature<-na.omit(temperature)

cor(temperature$dadosclimaticos.temperatura_minima_simepar, temperature$dadosclimaticos.tempertura_minima_worldclim)
#the correlation (62.28%) is moderate
#plot(temperature$dadosclimaticos.temperatura_minima_simepar, temperature$dadosclimaticos.tempertura_minima_worldclim)
#there are two outliers with a difference greater than 2ºC: 2007 e 2009

temperature$dadosclimaticos.tempertura_minima_worldclim[2]<-NA
temperature$dadosclimaticos.tempertura_minima_worldclim[3]<-NA

temperature<-na.omit(temperature)

cor(temperature$dadosclimaticos.temperatura_minima_simepar, temperature$dadosclimaticos.tempertura_minima_worldclim)
#the new correlation (>70%) is strong

dadosclimaticos$tempertura_minima_worldclim[2]<-NA
dadosclimaticos$tempertura_minima_worldclim[4]<-NA

#now, the databases are with strong correlations, so we can impute the data

imputacao<-impute(traits = dadosclimaticos, addingSpecies = FALSE)

dadosclimaticosimputado<-imputacao$imputed
dadosclimaticosimputado$ano<-c(2006:2023)

#The imputed values were:
#chuva_acumulada 2008 and 2012
#dias_chuva 2008 and 2012
#chuva_forte_violenta 2008, 2012, 2018, 2019, 2020, 2021, 2022, 2023
#mare_maxima 2022 and 2023
#temperatura_media 2008 and 2012

write.csv(dadosclimaticosimputado, file = "dadosclimaticosimputados.csv", row.names = FALSE)

#Productivity####
#upstream####
rm(list = ls())

#importing upstream data
dados_montante<- read.csv("dados_montante.csv", sep=";")
#excluding non-important columns
dados_montante <- dados_montante[, -c(9, 24)]
dados_montante<-na.omit(dados_montante)

#transforming qualitative variables in factors
colnames(dados_montante)
dados_montante$territorio<-as.factor(dados_montante$territorio)
dados_montante$densidade_cebolama<-as.factor(dados_montante$densidade_cebolama)
dados_montante$densidade_cebolama<-factor(dados_montante$densidade_cebolama, levels = c("normal", "moderada", "densa"), ordered=T)
dados_montante$densidade_samambaia<-as.factor(dados_montante$densidade_samambaia)
dados_montante$qualidade_samambaia<-factor(dados_montante$qualidade_samambaia, levels=c("ausenteouruim", "moderada", "boa"), ordered=T)
dados_montante$densidade_capimserra<-as.factor(dados_montante$densidade_capimserra)
dados_montante$qualidade_capimserra<-factor(dados_montante$qualidade_capimserra, levels = c("ausenteouruim", "moderado", "denso"), ordered=T)#simplified
dados_montante$arboreas<-as.factor(dados_montante$arboreas)
dados_montante$aboreas_simplificado<-as.factor(dados_montante$aboreas_simplificado)
dados_montante$foquilhas<-as.factor(dados_montante$foquilhas)
dados_montante$femea<-as.factor(dados_montante$femea)
dados_montante$macho<-as.factor(dados_montante$macho)

dados_montante$area_territorio<-log(dados_montante$area_territorio+1)
dados_montante$perimetro<-log(dados_montante$perimetro+1)
dados_montante$razao_perimetro_area<-log(dados_montante$razao_perimetro_area+1)
dados_montante$numero_vizinhos<-log(dados_montante$numero_vizinhos+1)
dados_montante$altitude_mediana<-log(dados_montante$altitude_mediana+1)
dados_montante$area_cebolama<-log(dados_montante$area_cebolama+1)
dados_montante$proporcao_cebolama<-log(dados_montante$proporcao_cebolama+1)
dados_montante$area_samambaia<-log(dados_montante$area_samambaia+1)
dados_montante$proporcao_samambaia<-log(dados_montante$proporcao_samambaia+1)
dados_montante$area_capimserra<-log(dados_montante$area_capimserra+1)
dados_montante$proporcao_capimserra<-log(dados_montante$proporcao_capimserra+1)
dados_montante$idade_macho<-log(dados_montante$idade_macho+1)
dados_montante$idade_femea<-log(dados_montante$idade_femea+1)
dados_montante$chuva_acumulada_simepar<-log(dados_montante$chuva_acumulada_simepar+1)
dados_montante$chuva_forte_violenta_simepar<-log(dados_montante$chuva_forte_violenta_simepar+1)
dados_montante$dias_chuva_simepar<-log(dados_montante$dias_chuva_simepar+1)
dados_montante$mare_maxima<-log(dados_montante$mare_maxima+1)
dados_montante$temperatura_media_simepar<-log(dados_montante$temperatura_media_simepar+1)
dados_montante$temperatura_maxima_simepar<-log(dados_montante$temperatura_maxima_simepar+1)
dados_montante$temperatura_minima_simepar<-log(dados_montante$temperatura_minima_simepar+1)

#scaling quantitative variables
dados_montante$area_territorio<-scale(dados_montante$area_territorio)
dados_montante$perimetro<-scale(dados_montante$perimetro)
dados_montante$razao_perimetro_area<-scale(dados_montante$razao_perimetro_area)
dados_montante$numero_vizinhos<-scale(dados_montante$numero_vizinhos)
dados_montante$altitude_mediana<-scale(dados_montante$altitude_mediana)
dados_montante$area_cebolama<-scale(dados_montante$area_cebolama)
dados_montante$proporcao_cebolama<-scale(dados_montante$proporcao_cebolama)
dados_montante$area_samambaia<-scale(dados_montante$area_samambaia)
dados_montante$proporcao_samambaia<-scale(dados_montante$proporcao_samambaia)
dados_montante$area_capimserra<-scale(dados_montante$area_capimserra)
dados_montante$proporcao_capimserra<-scale(dados_montante$proporcao_capimserra)
dados_montante$idade_macho<-scale(dados_montante$idade_macho)
dados_montante$idade_femea<-scale(dados_montante$idade_femea)
dados_montante$chuva_acumulada_simepar<-scale(dados_montante$chuva_acumulada_simepar)
dados_montante$chuva_forte_violenta_simepar<-scale(dados_montante$chuva_forte_violenta_simepar)
dados_montante$dias_chuva_simepar<-scale(dados_montante$dias_chuva_simepar)
dados_montante$mare_maxima<-scale(dados_montante$mare_maxima)
dados_montante$temperatura_media_simepar<-scale(dados_montante$temperatura_media_simepar)
dados_montante$temperatura_maxima_simepar<-scale(dados_montante$temperatura_maxima_simepar)
dados_montante$temperatura_minima_simepar<-scale(dados_montante$temperatura_minima_simepar)

#transforming everything to the model of the function
#fixed effects
fixos_montante<-dados_montante[,c("area_territorio", "perimetro", "razao_perimetro_area", "numero_vizinhos", "altitude_mediana", "area_cebolama", "proporcao_cebolama", "densidade_cebolama", "area_samambaia", "proporcao_samambaia", "qualidade_samambaia", "area_capimserra", "proporcao_capimserra", "qualidade_capimserra", "arboreas", "aboreas_simplificado", "foquilhas", "chuva_acumulada_simepar", "chuva_forte_violenta_simepar", "dias_chuva_simepar", "mare_maxima", "temperatura_maxima_simepar", "temperatura_minima_simepar", "temperatura_media_simepar")]
 
fixos_montante<-data.matrix(fixos_montante)

dados_montante$territorio<-as.numeric(dados_montante$territorio)

identifier<-dados_montante$territorio
identifier<-as.integer(dados_montante$territorio)

dados_montante$filhotes_independentes<-as.vector(dados_montante$filhotes_independentes)
output<-dados_montante$filhotes_independentes

time<-dados_montante$ano_simples
time<-as.numeric(time)

#randomeffects
aleatorios_montante<-dados_montante[,c("femea", "macho", "idade_femea", "idade_macho")]

aleatorios_montante<-data.matrix((aleatorios_montante))

#Matrix to assess correlation between fixed varoables
correlation_matrix <- cor(fixos_montante, use = "complete.obs")
print(correlation_matrix)
corrplot(correlation_matrix, method = "circle")

vif_model <- lm(filhotes_independentes ~ ., data = data.frame(filhotes_independentes = dados_montante$filhotes_independentes, fixos_montante))
vif_values <- vif(vif_model)
high_vif_vars <- names(vif_values[vif_values > 5])
print(high_vif_vars)

#Improving the variables, i.e., removing those with high correlation 
fixos_montante<-dados_montante[,c( "perimetro", "razao_perimetro_area", "numero_vizinhos", "altitude_mediana", "area_cebolama", "densidade_cebolama", "area_samambaia",  "qualidade_samambaia", "area_capimserra","qualidade_capimserra", "arboreas",  "foquilhas", "chuva_acumulada_simepar","dias_chuva_simepar", "mare_maxima", "temperatura_maxima_simepar", "temperatura_minima_simepar")]

fixos_montante<-data.matrix(fixos_montante)

lista<-list(aleatorios_montante=aleatorios_montante, fixos_montante=fixos_montante, identifier=identifier, output=output, time=time)

modeloMERF<-MERF(X = lista$fixos_montante, Y = lista$output, Z = lista$aleatorios_montante, id = lista$identifier, time = lista$time, sto="OrnUhl", mtry = 5, ntree=1000, delta=0.001, iter=100)

#Assessing variable importance
#random
matriz_aleatorios<-modeloMERF$var_random_effects
intercepts<-modeloMERF$random_effects

matriz_aleatorios
intercepts

sum((sqrt(intercepts[,1]^2))) 
sum((sqrt(intercepts[,2]^2)))
sum((sqrt(intercepts[,3]^2)))
sum((sqrt(intercepts[,4]^2))) 

#fixed
importancia<-modeloMERF$forest$importance
importancia<-importancia[,1]
order(importancia)

rm(list = ls())
#importing upstream data
dados_montante<- read.csv("dados_montante.csv", sep=";")
#excluding non-important columns
dados_montante <- dados_montante[, -c(9, 24)]
dados_montante<-na.omit(dados_montante)

#transforming qualitative variables in factors
colnames(dados_montante)
dados_montante$ano<-as.factor(dados_montante$ano)
dados_montante$territorio<-as.factor(dados_montante$territorio)
dados_montante$densidade_cebolama<-factor(dados_montante$densidade_cebolama, levels = c("normal", "moderada", "densa"), ordered=T)
dados_montante$densidade_samambaia<-as.factor(dados_montante$densidade_samambaia)
dados_montante$qualidade_samambaia<-factor(dados_montante$qualidade_samambaia, levels=c("ausenteouruim", "moderada", "boa"), ordered=T)
dados_montante$densidade_capimserra<-as.factor(dados_montante$densidade_capimserra)
dados_montante$qualidade_capimserra<-factor(dados_montante$qualidade_capimserra, levels = c("ausenteouruim", "moderado", "denso"), ordered=T)#simplified
dados_montante$arboreas<-as.factor(dados_montante$arboreas)
dados_montante$aboreas_simplificado<-as.factor(dados_montante$aboreas_simplificado)
dados_montante$foquilhas<-as.factor(dados_montante$foquilhas)
dados_montante$femea<-as.factor(dados_montante$femea)
dados_montante$macho<-as.factor(dados_montante$macho)

dados_montante$area_territorio<-log(dados_montante$area_territorio+1)
dados_montante$perimetro<-log(dados_montante$perimetro+1)
dados_montante$razao_perimetro_area<-log(dados_montante$razao_perimetro_area+1)
dados_montante$numero_vizinhos<-log(dados_montante$numero_vizinhos+1)
dados_montante$altitude_mediana<-log(dados_montante$altitude_mediana+1)
dados_montante$area_cebolama<-log(dados_montante$area_cebolama+1)
dados_montante$proporcao_cebolama<-log(dados_montante$proporcao_cebolama+1)
dados_montante$area_samambaia<-log(dados_montante$area_samambaia+1)
dados_montante$proporcao_samambaia<-log(dados_montante$proporcao_samambaia+1)
dados_montante$area_capimserra<-log(dados_montante$area_capimserra+1)
dados_montante$proporcao_capimserra<-log(dados_montante$proporcao_capimserra+1)
dados_montante$idade_macho<-log(dados_montante$idade_macho+1)
dados_montante$idade_femea<-log(dados_montante$idade_femea+1)
dados_montante$chuva_acumulada_simepar<-log(dados_montante$chuva_acumulada_simepar+1)
dados_montante$chuva_forte_violenta_simepar<-log(dados_montante$chuva_forte_violenta_simepar+1)
dados_montante$dias_chuva_simepar<-log(dados_montante$dias_chuva_simepar+1)
dados_montante$mare_maxima<-log(dados_montante$mare_maxima+1)
dados_montante$temperatura_media_simepar<-log(dados_montante$temperatura_media_simepar+1)
dados_montante$temperatura_maxima_simepar<-log(dados_montante$temperatura_maxima_simepar+1)
dados_montante$temperatura_minima_simepar<-log(dados_montante$temperatura_minima_simepar+1)

#scaling quantitative variables
dados_montante$area_territorio<-scale(dados_montante$area_territorio)
dados_montante$perimetro<-scale(dados_montante$perimetro)
dados_montante$razao_perimetro_area<-scale(dados_montante$razao_perimetro_area)
dados_montante$numero_vizinhos<-scale(dados_montante$numero_vizinhos)
dados_montante$altitude_mediana<-scale(dados_montante$altitude_mediana)
dados_montante$area_cebolama<-scale(dados_montante$area_cebolama)
dados_montante$proporcao_cebolama<-scale(dados_montante$proporcao_cebolama)
dados_montante$area_samambaia<-scale(dados_montante$area_samambaia)
dados_montante$proporcao_samambaia<-scale(dados_montante$proporcao_samambaia)
dados_montante$area_capimserra<-scale(dados_montante$area_capimserra)
dados_montante$proporcao_capimserra<-scale(dados_montante$proporcao_capimserra)
dados_montante$idade_macho<-scale(dados_montante$idade_macho)
dados_montante$idade_femea<-scale(dados_montante$idade_femea)
dados_montante$chuva_acumulada_simepar<-scale(dados_montante$chuva_acumulada_simepar)
dados_montante$chuva_forte_violenta_simepar<-scale(dados_montante$chuva_forte_violenta_simepar)
dados_montante$dias_chuva_simepar<-scale(dados_montante$dias_chuva_simepar)
dados_montante$mare_maxima<-scale(dados_montante$mare_maxima)
dados_montante$temperatura_media_simepar<-scale(dados_montante$temperatura_media_simepar)
dados_montante$temperatura_maxima_simepar<-scale(dados_montante$temperatura_maxima_simepar)
dados_montante$temperatura_minima_simepar<-scale(dados_montante$temperatura_minima_simepar)

#Poisson
glmm_poisson <- glmer(filhotes_independentes ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|femea) + (1|macho) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson)
print(AIC(glmm_poisson))
print(BIC(glmm_poisson))


#Binomal negative
glmm_negbin <- glmer.nb(filhotes_independentes ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|femea) + (1|macho) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_negbin)
print(AIC(glmm_negbin))
print(BIC(glmm_negbin))


#### Simplifing the random
glmm_poisson1 <- glmer(filhotes_independentes ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|macho) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson1)
print(AIC(glmm_poisson1)) 
print(BIC(glmm_poisson1)) 

glmm_poisson2 <- glmer(filhotes_independentes ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra+ (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson2)
print(AIC(glmm_poisson2)) 
print(BIC(glmm_poisson2))

glmm_poisson3 <- glmer(filhotes_independentes ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra+ (1|idade_femea) + (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson3)
print(AIC(glmm_poisson3)) 
print(BIC(glmm_poisson3)) 

glmm_poisson4 <- glmer(filhotes_independentes ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson4)
print(AIC(glmm_poisson4))
print(BIC(glmm_poisson4))

#Now, simplifying the fixed effects
glmm_poisson5 <- glmer(filhotes_independentes ~ numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson5)
print(AIC(glmm_poisson5))
print(BIC(glmm_poisson5)) 

glmm_poisson6 <- glmer(filhotes_independentes ~ area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson6)
print(AIC(glmm_poisson6))
print(BIC(glmm_poisson6)) 

#downstream####
rm(list = ls())

dados_jusante<- read.csv("dados_jusante.csv", sep=";")
#excluding non-important columns
dados_jusante <- dados_jusante[, -c(8, 20)]
dados_jusante<-na.omit(dados_jusante)

#transforming qualitative variables in factors
colnames(dados_jusante)
dados_jusante$territorio<-as.factor(dados_jusante$territorio)
dados_jusante$qualidade_samambaia<-factor(dados_jusante$qualidade_samambaia, levels=c("ausenteouruim", "boa"), ordered=T)
dados_jusante$foquilhas<-as.factor(dados_jusante$foquilhas)
dados_jusante$femea<-as.factor(dados_jusante$femea)
dados_jusante$macho<-as.factor(dados_jusante$macho)

#log transforming the quantitative variables
dados_jusante$area_territorio<-log(dados_jusante$area_territorio+1)
dados_jusante$perimetro<-log(dados_jusante$perimetro+1)
dados_jusante$razao_perimetro_area<-log(dados_jusante$razao_perimetro_area+1)
dados_jusante$numero_vizinhos<-log(dados_jusante$numero_vizinhos+1)
dados_jusante$altitude_mediana<-log(dados_jusante$altitude_mediana+1)
dados_jusante$proporcao_mangue<-log(dados_jusante$proporcao_mangue+1)
dados_jusante$area_mangue<-log(dados_jusante$area_mangue+1)
dados_jusante$proporcao_cebolama<-log(dados_jusante$proporcao_cebolama+1)
dados_jusante$area_cebolama<-log(dados_jusante$area_cebolama+1)
dados_jusante$proporcao_samambaia<-log(dados_jusante$proporcao_samambaia+1)
dados_jusante$area_samambaia<-log(dados_jusante$area_samambaia+1)
dados_jusante$idade_femea<-log(dados_jusante$idade_femea+1)
dados_jusante$idade_macho<-log(dados_jusante$idade_macho+1)
dados_jusante$chuva_acumulada_simepar<-log(dados_jusante$chuva_acumulada_simepar+1)
dados_jusante$chuva_forte_violenta_simepar<-log(dados_jusante$chuva_forte_violenta_simepar+1)
dados_jusante$dias_chuva_simepar<-log(dados_jusante$dias_chuva_simepar+1)
dados_jusante$mare_maxima<-log(dados_jusante$mare_maxima+1)
dados_jusante$temperatura_media_simepar<-log(dados_jusante$temperatura_media_simepar+1)
dados_jusante$temperatura_maxima_simepar<-log(dados_jusante$temperatura_maxima_simepar+1)
dados_jusante$temperatura_minima_simepar<-log(dados_jusante$temperatura_minima_simepar+1)

#scaling transforming the quantitative variables
dados_jusante$area_territorio<-scale(dados_jusante$area_territorio)
dados_jusante$perimetro<-scale(dados_jusante$perimetro)
dados_jusante$razao_perimetro_area<-scale(dados_jusante$razao_perimetro_area)
dados_jusante$numero_vizinhos<-scale(dados_jusante$numero_vizinhos)
dados_jusante$altitude_mediana<-scale(dados_jusante$altitude_mediana)
dados_jusante$proporcao_mangue<-scale(dados_jusante$proporcao_mangue)
dados_jusante$area_mangue<-scale(dados_jusante$area_mangue)
dados_jusante$proporcao_cebolama<-scale(dados_jusante$proporcao_cebolama)
dados_jusante$area_cebolama<-scale(dados_jusante$area_cebolama)
dados_jusante$proporcao_samambaia<-scale(dados_jusante$proporcao_samambaia)
dados_jusante$area_samambaia<-scale(dados_jusante$area_samambaia)
dados_jusante$idade_femea<-scale(dados_jusante$idade_femea)
dados_jusante$idade_macho<-scale(dados_jusante$idade_macho)
dados_jusante$chuva_acumulada_simepar<-scale(dados_jusante$chuva_acumulada_simepar)
dados_jusante$chuva_forte_violenta_simepar<-scale(dados_jusante$chuva_forte_violenta_simepar)
dados_jusante$dias_chuva_simepar<-scale(dados_jusante$dias_chuva_simepar)
dados_jusante$mare_maxima<-scale(dados_jusante$mare_maxima)
dados_jusante$temperatura_media_simepar<-scale(dados_jusante$temperatura_media_simepar)
dados_jusante$temperatura_maxima_simepar<-scale(dados_jusante$temperatura_maxima_simepar)
dados_jusante$temperatura_minima_simepar<-scale(dados_jusante$temperatura_minima_simepar)


#transforming everything to the model of the function
#fixed effects
fixos_jusante<-dados_jusante[,c("area_territorio", "perimetro", "razao_perimetro_area", "numero_vizinhos", "altitude_mediana", "area_cebolama", "proporcao_cebolama",  "area_samambaia", "proporcao_samambaia", "qualidade_samambaia", "proporcao_mangue", "area_mangue", "foquilhas", "chuva_acumulada_simepar", "chuva_forte_violenta_simepar", "dias_chuva_simepar", "mare_maxima", "temperatura_maxima_simepar", "temperatura_minima_simepar", "temperatura_media_simepar")]

fixos_jusante<-data.matrix(fixos_jusante)

dados_jusante$territorio<-as.numeric(dados_jusante$territorio)

identifier<-dados_jusante$territorio
identifier<-as.integer(dados_jusante$territorio)

filhotes_independentes<-as.vector(dados_jusante$filhotes_independentes)
output<-filhotes_independentes
output<-as.numeric(output)

time<-dados_jusante$ano
time<-as.numeric(time)

#randomeffects
aleatorios_jusante<-dados_jusante[,c("femea", "macho", "idade_femea", "idade_macho")]

aleatorios_jusante<-data.matrix((aleatorios_jusante))

#Matrix to assess correlation between fixed varoables
correlation_matrix <- cor(fixos_jusante, use = "complete.obs")
print(correlation_matrix)
corrplot(correlation_matrix, method = "circle")

vif_model <- lm(filhotes_independentes ~ ., data = data.frame(filhotes_independentes = dados_jusante$filhotes_independentes, fixos_jusante))
vif_values <- vif(vif_model)
high_vif_vars <- names(vif_values[vif_values > 5])
print(high_vif_vars)

#Improving the variables, i.e., removing those with high correlation 
fixos_jusante<-dados_jusante[,c("area_territorio", "numero_vizinhos", "altitude_mediana", "proporcao_cebolama",  "proporcao_samambaia", "proporcao_mangue", "foquilhas", "chuva_acumulada_simepar", "dias_chuva_simepar", "mare_maxima", "temperatura_maxima_simepar", "temperatura_minima_simepar")]

fixos_jusante<-data.matrix(fixos_jusante)

lista<-list(aleatorios_jusante=aleatorios_jusante, fixos_jusante=fixos_jusante, identifier=identifier, output=output, time=time)

#Doing the MERF to assess the importance of predictors
modeloMERF<-MERF(X = lista$fixos_jusante, Y = lista$output, Z = lista$aleatorios_jusante, id = lista$identifier, time = lista$time, sto="OrnUhl", mtry = 5, ntree=1000, delta=0.001, iter=100)


#Assessing variable importance
#random
matriz_aleatorios<-modeloMERF$var_random_effects
intercepts<-modeloMERF$random_effects

matriz_aleatorios
intercepts

sum((sqrt(intercepts[,1]^2))) 
sum((sqrt(intercepts[,2]^2)))
sum((sqrt(intercepts[,3]^2)))
sum((sqrt(intercepts[,4]^2))) 

#fixed
importancia<-modeloMERF$forest$importance
importancia<-importancia[,1]
order(importancia)

rm(list = ls())

#importing data
dados_jusante<- read.csv("dados_jusante.csv", sep=";")
#excluding non-important columns
dados_jusante <- dados_jusante[, -c(8, 20)]
dados_jusante<-na.omit(dados_jusante)

#transforming qualitative variables in factors
colnames(dados_jusante)
dados_jusante$territorio<-as.factor(dados_jusante$territorio)
dados_jusante$qualidade_samambaia<-factor(dados_jusante$qualidade_samambaia, levels=c("ausenteouruim", "boa"), ordered=T)
dados_jusante$foquilhas<-as.factor(dados_jusante$foquilhas)
dados_jusante$femea<-as.factor(dados_jusante$femea)
dados_jusante$macho<-as.factor(dados_jusante$macho)

#log transforming the quantitative variables
dados_jusante$area_territorio<-log(dados_jusante$area_territorio+1)
dados_jusante$perimetro<-log(dados_jusante$perimetro+1)
dados_jusante$razao_perimetro_area<-log(dados_jusante$razao_perimetro_area+1)
dados_jusante$numero_vizinhos<-log(dados_jusante$numero_vizinhos+1)
dados_jusante$altitude_mediana<-log(dados_jusante$altitude_mediana+1)
dados_jusante$proporcao_mangue<-log(dados_jusante$proporcao_mangue+1)
dados_jusante$area_mangue<-log(dados_jusante$area_mangue+1)
dados_jusante$proporcao_cebolama<-log(dados_jusante$proporcao_cebolama+1)
dados_jusante$area_cebolama<-log(dados_jusante$area_cebolama+1)
dados_jusante$proporcao_samambaia<-log(dados_jusante$proporcao_samambaia+1)
dados_jusante$area_samambaia<-log(dados_jusante$area_samambaia+1)
dados_jusante$idade_femea<-log(dados_jusante$idade_femea+1)
dados_jusante$idade_macho<-log(dados_jusante$idade_macho+1)
dados_jusante$chuva_acumulada_simepar<-log(dados_jusante$chuva_acumulada_simepar+1)
dados_jusante$chuva_forte_violenta_simepar<-log(dados_jusante$chuva_forte_violenta_simepar+1)
dados_jusante$dias_chuva_simepar<-log(dados_jusante$dias_chuva_simepar+1)
dados_jusante$mare_maxima<-log(dados_jusante$mare_maxima+1)
dados_jusante$temperatura_media_simepar<-log(dados_jusante$temperatura_media_simepar+1)
dados_jusante$temperatura_maxima_simepar<-log(dados_jusante$temperatura_maxima_simepar+1)
dados_jusante$temperatura_minima_simepar<-log(dados_jusante$temperatura_minima_simepar+1)

#scaling transforming the quantitative variables
dados_jusante$ano<-scale(dados_jusante$ano)
dados_jusante$area_territorio<-scale(dados_jusante$area_territorio)
dados_jusante$perimetro<-scale(dados_jusante$perimetro)
dados_jusante$razao_perimetro_area<-scale(dados_jusante$razao_perimetro_area)
dados_jusante$numero_vizinhos<-scale(dados_jusante$numero_vizinhos)
dados_jusante$altitude_mediana<-scale(dados_jusante$altitude_mediana)
dados_jusante$proporcao_mangue<-scale(dados_jusante$proporcao_mangue)
dados_jusante$area_mangue<-scale(dados_jusante$area_mangue)
dados_jusante$proporcao_cebolama<-scale(dados_jusante$proporcao_cebolama)
dados_jusante$area_cebolama<-scale(dados_jusante$area_cebolama)
dados_jusante$proporcao_samambaia<-scale(dados_jusante$proporcao_samambaia)
dados_jusante$area_samambaia<-scale(dados_jusante$area_samambaia)
dados_jusante$idade_femea<-scale(dados_jusante$idade_femea)
dados_jusante$idade_macho<-scale(dados_jusante$idade_macho)
dados_jusante$chuva_acumulada_simepar<-scale(dados_jusante$chuva_acumulada_simepar)
dados_jusante$chuva_forte_violenta_simepar<-scale(dados_jusante$chuva_forte_violenta_simepar)
dados_jusante$dias_chuva_simepar<-scale(dados_jusante$dias_chuva_simepar)
dados_jusante$mare_maxima<-scale(dados_jusante$mare_maxima)
dados_jusante$temperatura_media_simepar<-scale(dados_jusante$temperatura_media_simepar)
dados_jusante$temperatura_maxima_simepar<-scale(dados_jusante$temperatura_maxima_simepar)
dados_jusante$temperatura_minima_simepar<-scale(dados_jusante$temperatura_minima_simepar)

#Poisson
glmm_poisson <- glmer(filhotes_independentes ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|femea) + (1|macho) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson)
print(AIC(glmm_poisson))
print(BIC(glmm_poisson))

#Binomal negative
glmm_negbin <- glmer.nb(filhotes_independentes ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama + (1|femea) + (1|macho) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), data = dados_jusante)
summary(glmm_negbin)
print(AIC(glmm_negbin))
print(BIC(glmm_negbin))

####
glmm_poisson1 <- glmer(filhotes_independentes ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|femea) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson1)
print(AIC(glmm_poisson1))
print(BIC(glmm_poisson1))

glmm_poisson2 <- glmer(filhotes_independentes ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson2)
print(AIC(glmm_poisson2)) 
print(BIC(glmm_poisson2))

glmm_poisson3 <- glmer(filhotes_independentes ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|idade_femea)+ (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson3)
print(AIC(glmm_poisson3)) 
print(BIC(glmm_poisson3)) 

glmm_poisson4 <- glmer(filhotes_independentes ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson4)
print(AIC(glmm_poisson4)) 
print(BIC(glmm_poisson4)) 

glmm_poisson5 <- glmer(filhotes_independentes ~  area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson5)
print(AIC(glmm_poisson5)) 
print(BIC(glmm_poisson5))
Anova(glmm_poisson5)

summary(glht(glmm_poisson5, linfct = mcp(foquilhas = 'Tukey')))

glmm_poisson6 <- glmer(filhotes_independentes ~  area_territorio + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson6)
print(AIC(glmm_poisson6)) 
print(BIC(glmm_poisson6)) 


#Mortality####
#upstream####
rm(list = ls())

#importing data
dados_montante<- read.csv("dados_montante.csv", sep=";")
#excluding non-important columns
dados_montante <- dados_montante[, -c(9, 24)]
dados_montante<-na.omit(dados_montante)

#transforming qualitative variables in factors
colnames(dados_montante)
dados_montante$territorio<-as.factor(dados_montante$territorio)
dados_montante$densidade_cebolama<-as.factor(dados_montante$densidade_cebolama)
dados_montante$densidade_cebolama<-factor(dados_montante$densidade_cebolama, levels = c("normal", "moderada", "densa"), ordered=T)
dados_montante$densidade_samambaia<-as.factor(dados_montante$densidade_samambaia)
dados_montante$qualidade_samambaia<-factor(dados_montante$qualidade_samambaia, levels=c("ausenteouruim", "moderada", "boa"), ordered=T)
dados_montante$densidade_capimserra<-as.factor(dados_montante$densidade_capimserra)
dados_montante$qualidade_capimserra<-factor(dados_montante$qualidade_capimserra, levels = c("ausenteouruim", "moderado", "denso"), ordered = T)#simplified
dados_montante$arboreas<-as.factor(dados_montante$arboreas)
dados_montante$aboreas_simplificado<-as.factor(dados_montante$aboreas_simplificado)
dados_montante$foquilhas<-as.factor(dados_montante$foquilhas)
dados_montante$femea<-as.factor(dados_montante$femea)
dados_montante$macho<-as.factor(dados_montante$macho)

dados_montante$area_territorio<-log(dados_montante$area_territorio+1)
dados_montante$perimetro<-log(dados_montante$perimetro+1)
dados_montante$razao_perimetro_area<-log(dados_montante$razao_perimetro_area+1)
dados_montante$numero_vizinhos<-log(dados_montante$numero_vizinhos+1)
dados_montante$altitude_mediana<-log(dados_montante$altitude_mediana+1)
dados_montante$area_cebolama<-log(dados_montante$area_cebolama+1)
dados_montante$proporcao_cebolama<-log(dados_montante$proporcao_cebolama+1)
dados_montante$area_samambaia<-log(dados_montante$area_samambaia+1)
dados_montante$proporcao_samambaia<-log(dados_montante$proporcao_samambaia+1)
dados_montante$area_capimserra<-log(dados_montante$area_capimserra+1)
dados_montante$proporcao_capimserra<-log(dados_montante$proporcao_capimserra+1)
dados_montante$idade_macho<-log(dados_montante$idade_macho+1)
dados_montante$idade_femea<-log(dados_montante$idade_femea+1)
dados_montante$chuva_acumulada_simepar<-log(dados_montante$chuva_acumulada_simepar+1)
dados_montante$chuva_forte_violenta_simepar<-log(dados_montante$chuva_forte_violenta_simepar+1)
dados_montante$dias_chuva_simepar<-log(dados_montante$dias_chuva_simepar+1)
dados_montante$mare_maxima<-log(dados_montante$mare_maxima+1)
dados_montante$temperatura_media_simepar<-log(dados_montante$temperatura_media_simepar+1)
dados_montante$temperatura_maxima_simepar<-log(dados_montante$temperatura_maxima_simepar+1)
dados_montante$temperatura_minima_simepar<-log(dados_montante$temperatura_minima_simepar+1)

#scaling quantitative variables
dados_montante$area_territorio<-scale(dados_montante$area_territorio)
dados_montante$perimetro<-scale(dados_montante$perimetro)
dados_montante$razao_perimetro_area<-scale(dados_montante$razao_perimetro_area)
dados_montante$numero_vizinhos<-scale(dados_montante$numero_vizinhos)
dados_montante$altitude_mediana<-scale(dados_montante$altitude_mediana)
dados_montante$area_cebolama<-scale(dados_montante$area_cebolama)
dados_montante$proporcao_cebolama<-scale(dados_montante$proporcao_cebolama)
dados_montante$area_samambaia<-scale(dados_montante$area_samambaia)
dados_montante$proporcao_samambaia<-scale(dados_montante$proporcao_samambaia)
dados_montante$area_capimserra<-scale(dados_montante$area_capimserra)
dados_montante$proporcao_capimserra<-scale(dados_montante$proporcao_capimserra)
dados_montante$idade_macho<-scale(dados_montante$idade_macho)
dados_montante$idade_femea<-scale(dados_montante$idade_femea)
dados_montante$chuva_acumulada_simepar<-scale(dados_montante$chuva_acumulada_simepar)
dados_montante$chuva_forte_violenta_simepar<-scale(dados_montante$chuva_forte_violenta_simepar)
dados_montante$dias_chuva_simepar<-scale(dados_montante$dias_chuva_simepar)
dados_montante$mare_maxima<-scale(dados_montante$mare_maxima)
dados_montante$temperatura_media_simepar<-scale(dados_montante$temperatura_media_simepar)
dados_montante$temperatura_maxima_simepar<-scale(dados_montante$temperatura_maxima_simepar)
dados_montante$temperatura_minima_simepar<-scale(dados_montante$temperatura_minima_simepar)
#fixed effects
fixos_montante<-dados_montante[,c("area_territorio", "perimetro", "razao_perimetro_area", "numero_vizinhos", "altitude_mediana", "area_cebolama", "proporcao_cebolama", "densidade_cebolama", "area_samambaia", "proporcao_samambaia", "qualidade_samambaia", "area_capimserra", "proporcao_capimserra", "qualidade_capimserra", "arboreas", "aboreas_simplificado", "foquilhas", "chuva_acumulada_simepar", "chuva_forte_violenta_simepar", "dias_chuva_simepar", "mare_maxima", "temperatura_maxima_simepar", "temperatura_minima_simepar", "temperatura_media_simepar")]

fixos_montante<-data.matrix(fixos_montante)

dados_montante$territorio<-as.numeric(dados_montante$territorio)

identifier<-dados_montante$territorio
identifier<-as.integer(dados_montante$territorio)

dados_montante$mortalidade<-as.vector(dados_montante$mortalidade)
output<-dados_montante$mortalidade

time<-dados_montante$ano_simples
time<-as.numeric(time)

#randomeffects
aleatorios_montante<-dados_montante[,c("femea", "macho", "idade_femea", "idade_macho")]

aleatorios_montante<-data.matrix((aleatorios_montante))

#Matrix to assess correlation between fixed varoables
correlation_matrix <- cor(fixos_montante, use = "complete.obs")
print(correlation_matrix)
corrplot(correlation_matrix, method = "circle")

vif_model <- lm(mortalidade ~ ., data = data.frame(mortalidade = dados_montante$mortalidade, fixos_montante))
vif_values <- vif(vif_model)
high_vif_vars <- names(vif_values[vif_values > 5])
print(high_vif_vars)

#Improving the variables, i.e., removing those with high correlation 
fixos_montante<-dados_montante[,c( "perimetro", "razao_perimetro_area", "numero_vizinhos", "altitude_mediana", "area_cebolama", "densidade_cebolama", "area_samambaia",  "qualidade_samambaia", "area_capimserra","qualidade_capimserra", "arboreas",  "foquilhas", "chuva_acumulada_simepar","dias_chuva_simepar", "mare_maxima", "temperatura_maxima_simepar", "temperatura_minima_simepar")]

fixos_montante<-data.matrix(fixos_montante)

lista<-list(aleatorios_montante=aleatorios_montante, fixos_montante=fixos_montante, identifier=identifier, output=output, time=time)

modeloMERF<-MERF(X = lista$fixos_montante, Y = lista$output, Z = lista$aleatorios_montante, id = lista$identifier, time = lista$time, sto="OrnUhl", mtry = 5, ntree=1000, delta=0.001, iter=100)

#assessing the importance of the variable
#random
matriz_aleatorios<-modeloMERF$var_random_effects
intercepts<-modeloMERF$random_effects

matriz_aleatorios
intercepts

sum((sqrt(intercepts[,1]^2))) 
sum((sqrt(intercepts[,2]^2)))
sum((sqrt(intercepts[,3]^2)))
sum((sqrt(intercepts[,4]^2))) 

#fixed
importancia<-modeloMERF$forest$importance
importancia<-importancia[,1]
order(importancia)

rm(list = ls())
#importing upstream data
dados_montante<- read.csv("dados_montante.csv", sep=";")
#excluding non-important columns
dados_montante <- dados_montante[, -c(9, 24)]
dados_montante<-na.omit(dados_montante)

#transforming qualitative variables in factors
colnames(dados_montante)
dados_montante$ano<-as.factor(dados_montante$ano)
dados_montante$territorio<-as.factor(dados_montante$territorio)
dados_montante$densidade_cebolama<-factor(dados_montante$densidade_cebolama, levels = c("normal", "moderada", "densa"), ordered=T)
dados_montante$densidade_samambaia<-as.factor(dados_montante$densidade_samambaia)
dados_montante$qualidade_samambaia<-factor(dados_montante$qualidade_samambaia, levels=c("ausenteouruim", "moderada", "boa"), ordered=T)
dados_montante$densidade_capimserra<-as.factor(dados_montante$densidade_capimserra)
dados_montante$qualidade_capimserra<-factor(dados_montante$qualidade_capimserra, levels = c("ausenteouruim", "moderado", "denso"), ordered = T)#simplified
dados_montante$arboreas<-as.factor(dados_montante$arboreas)
dados_montante$aboreas_simplificado<-as.factor(dados_montante$aboreas_simplificado)
dados_montante$foquilhas<-as.factor(dados_montante$foquilhas)
dados_montante$femea<-as.factor(dados_montante$femea)
dados_montante$macho<-as.factor(dados_montante$macho)

dados_montante$area_territorio<-log(dados_montante$area_territorio+1)
dados_montante$perimetro<-log(dados_montante$perimetro+1)
dados_montante$razao_perimetro_area<-log(dados_montante$razao_perimetro_area+1)
dados_montante$numero_vizinhos<-log(dados_montante$numero_vizinhos+1)
dados_montante$altitude_mediana<-log(dados_montante$altitude_mediana+1)
dados_montante$area_cebolama<-log(dados_montante$area_cebolama+1)
dados_montante$proporcao_cebolama<-log(dados_montante$proporcao_cebolama+1)
dados_montante$area_samambaia<-log(dados_montante$area_samambaia+1)
dados_montante$proporcao_samambaia<-log(dados_montante$proporcao_samambaia+1)
dados_montante$area_capimserra<-log(dados_montante$area_capimserra+1)
dados_montante$proporcao_capimserra<-log(dados_montante$proporcao_capimserra+1)
dados_montante$idade_macho<-log(dados_montante$idade_macho+1)
dados_montante$idade_femea<-log(dados_montante$idade_femea+1)
dados_montante$chuva_acumulada_simepar<-log(dados_montante$chuva_acumulada_simepar+1)
dados_montante$chuva_forte_violenta_simepar<-log(dados_montante$chuva_forte_violenta_simepar+1)
dados_montante$dias_chuva_simepar<-log(dados_montante$dias_chuva_simepar+1)
dados_montante$mare_maxima<-log(dados_montante$mare_maxima+1)
dados_montante$temperatura_media_simepar<-log(dados_montante$temperatura_media_simepar+1)
dados_montante$temperatura_maxima_simepar<-log(dados_montante$temperatura_maxima_simepar+1)
dados_montante$temperatura_minima_simepar<-log(dados_montante$temperatura_minima_simepar+1)

#scaling quantitative variables
dados_montante$area_territorio<-scale(dados_montante$area_territorio)
dados_montante$perimetro<-scale(dados_montante$perimetro)
dados_montante$razao_perimetro_area<-scale(dados_montante$razao_perimetro_area)
dados_montante$numero_vizinhos<-scale(dados_montante$numero_vizinhos)
dados_montante$altitude_mediana<-scale(dados_montante$altitude_mediana)
dados_montante$area_cebolama<-scale(dados_montante$area_cebolama)
dados_montante$proporcao_cebolama<-scale(dados_montante$proporcao_cebolama)
dados_montante$area_samambaia<-scale(dados_montante$area_samambaia)
dados_montante$proporcao_samambaia<-scale(dados_montante$proporcao_samambaia)
dados_montante$area_capimserra<-scale(dados_montante$area_capimserra)
dados_montante$proporcao_capimserra<-scale(dados_montante$proporcao_capimserra)
dados_montante$idade_macho<-scale(dados_montante$idade_macho)
dados_montante$idade_femea<-scale(dados_montante$idade_femea)
dados_montante$chuva_acumulada_simepar<-scale(dados_montante$chuva_acumulada_simepar)
dados_montante$chuva_forte_violenta_simepar<-scale(dados_montante$chuva_forte_violenta_simepar)
dados_montante$dias_chuva_simepar<-scale(dados_montante$dias_chuva_simepar)
dados_montante$mare_maxima<-scale(dados_montante$mare_maxima)
dados_montante$temperatura_media_simepar<-scale(dados_montante$temperatura_media_simepar)
dados_montante$temperatura_maxima_simepar<-scale(dados_montante$temperatura_maxima_simepar)
dados_montante$temperatura_minima_simepar<-scale(dados_montante$temperatura_minima_simepar)

#Poisson
glmm_poisson <- glmer(mortalidade ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|femea) + (1|macho) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson)
print(AIC(glmm_poisson))
print(BIC(glmm_poisson))

#Binomal negative
glmm_negbin <- glmer.nb(mortalidade ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|femea) + (1|macho) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_negbin)
print(AIC(glmm_negbin))
print(BIC(glmm_negbin))

glmm_poisson1 <- glmer(mortalidade ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|femea) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson1)
print(AIC(glmm_poisson1)) 
print(BIC(glmm_poisson1)) 

glmm_poisson2 <- glmer(mortalidade ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra+ (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson2)
print(AIC(glmm_poisson2)) 
print(BIC(glmm_poisson2))

glmm_poisson3 <- glmer(mortalidade ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra+ (1|idade_femea) + (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson3)
print(AIC(glmm_poisson3)) 
print(BIC(glmm_poisson3))

glmm_poisson4 <- glmer(mortalidade ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson4)
print(AIC(glmm_poisson4)) 
print(BIC(glmm_poisson4)) 

glmm_poisson5 <- glmer(mortalidade ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + area_capimserra+ (1|idade_femea) + (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson5)
print(AIC(glmm_poisson5)) 
print(BIC(glmm_poisson5))

glmm_poisson6 <- glmer(mortalidade ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + area_capimserra+ (1|idade_femea) + (1|territorio)+ (1|ano), family = poisson, data = dados_montante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson6)
print(AIC(glmm_poisson6)) 
print(BIC(glmm_poisson6))

#downstream####
rm(list = ls())

#importing data
dados_jusante<- read.csv("dados_jusante.csv", sep=";")
#excluding non-important columns
dados_jusante <- dados_jusante[, -c(8, 20)]
dados_jusante<-na.omit(dados_jusante)

#transforming qualitative variables in factors
colnames(dados_jusante)
dados_jusante$territorio<-as.factor(dados_jusante$territorio)
dados_jusante$qualidade_samambaia<-factor(dados_jusante$qualidade_samambaia, levels=c("ausenteouruim", "boa"), ordered=T)
dados_jusante$foquilhas<-as.factor(dados_jusante$foquilhas)
dados_jusante$femea<-as.factor(dados_jusante$femea)
dados_jusante$macho<-as.factor(dados_jusante$macho)

#log transforming the quantitative variables
dados_jusante$area_territorio<-log(dados_jusante$area_territorio+1)
dados_jusante$perimetro<-log(dados_jusante$perimetro+1)
dados_jusante$razao_perimetro_area<-log(dados_jusante$razao_perimetro_area+1)
dados_jusante$numero_vizinhos<-log(dados_jusante$numero_vizinhos+1)
dados_jusante$altitude_mediana<-log(dados_jusante$altitude_mediana+1)
dados_jusante$proporcao_mangue<-log(dados_jusante$proporcao_mangue+1)
dados_jusante$area_mangue<-log(dados_jusante$area_mangue+1)
dados_jusante$proporcao_cebolama<-log(dados_jusante$proporcao_cebolama+1)
dados_jusante$area_cebolama<-log(dados_jusante$area_cebolama+1)
dados_jusante$proporcao_samambaia<-log(dados_jusante$proporcao_samambaia+1)
dados_jusante$area_samambaia<-log(dados_jusante$area_samambaia+1)
dados_jusante$idade_femea<-log(dados_jusante$idade_femea+1)
dados_jusante$idade_macho<-log(dados_jusante$idade_macho+1)
dados_jusante$chuva_acumulada_simepar<-log(dados_jusante$chuva_acumulada_simepar+1)
dados_jusante$chuva_forte_violenta_simepar<-log(dados_jusante$chuva_forte_violenta_simepar+1)
dados_jusante$dias_chuva_simepar<-log(dados_jusante$dias_chuva_simepar+1)
dados_jusante$mare_maxima<-log(dados_jusante$mare_maxima+1)
dados_jusante$temperatura_media_simepar<-log(dados_jusante$temperatura_media_simepar+1)
dados_jusante$temperatura_maxima_simepar<-log(dados_jusante$temperatura_maxima_simepar+1)
dados_jusante$temperatura_minima_simepar<-log(dados_jusante$temperatura_minima_simepar+1)


#scaling transforming the quantitative variables
dados_jusante$area_territorio<-scale(dados_jusante$area_territorio)
dados_jusante$perimetro<-scale(dados_jusante$perimetro)
dados_jusante$razao_perimetro_area<-scale(dados_jusante$razao_perimetro_area)
dados_jusante$numero_vizinhos<-scale(dados_jusante$numero_vizinhos)
dados_jusante$altitude_mediana<-scale(dados_jusante$altitude_mediana)
dados_jusante$proporcao_mangue<-scale(dados_jusante$proporcao_mangue)
dados_jusante$area_mangue<-scale(dados_jusante$area_mangue)
dados_jusante$proporcao_cebolama<-scale(dados_jusante$proporcao_cebolama)
dados_jusante$area_cebolama<-scale(dados_jusante$area_cebolama)
dados_jusante$proporcao_samambaia<-scale(dados_jusante$proporcao_samambaia)
dados_jusante$area_samambaia<-scale(dados_jusante$area_samambaia)
dados_jusante$idade_femea<-scale(dados_jusante$idade_femea)
dados_jusante$idade_macho<-scale(dados_jusante$idade_macho)
dados_jusante$chuva_acumulada_simepar<-scale(dados_jusante$chuva_acumulada_simepar)
dados_jusante$chuva_forte_violenta_simepar<-scale(dados_jusante$chuva_forte_violenta_simepar)
dados_jusante$dias_chuva_simepar<-scale(dados_jusante$dias_chuva_simepar)
dados_jusante$mare_maxima<-scale(dados_jusante$mare_maxima)
dados_jusante$temperatura_media_simepar<-scale(dados_jusante$temperatura_media_simepar)
dados_jusante$temperatura_maxima_simepar<-scale(dados_jusante$temperatura_maxima_simepar)
dados_jusante$temperatura_minima_simepar<-scale(dados_jusante$temperatura_minima_simepar)

#transforming everything to the model of the function
#fixed effects
fixos_jusante<-dados_jusante[,c("area_territorio", "perimetro", "razao_perimetro_area", "numero_vizinhos", "altitude_mediana", "area_cebolama", "proporcao_cebolama",  "area_samambaia", "proporcao_samambaia", "qualidade_samambaia", "proporcao_mangue", "area_mangue", "foquilhas", "chuva_acumulada_simepar", "chuva_forte_violenta_simepar", "dias_chuva_simepar", "mare_maxima", "temperatura_maxima_simepar", "temperatura_minima_simepar", "temperatura_media_simepar")]

fixos_jusante<-data.matrix(fixos_jusante)

dados_jusante$territorio<-as.numeric(dados_jusante$territorio)

identifier<-dados_jusante$territorio
identifier<-as.integer(dados_jusante$territorio)

mortalidade<-as.vector(dados_jusante$mortalidade)
output<-mortalidade
output<-as.numeric(output)

time<-dados_jusante$ano
time<-as.numeric(time)

#randomeffects
aleatorios_jusante<-dados_jusante[,c("femea", "macho", "idade_femea", "idade_macho")]

aleatorios_jusante<-data.matrix((aleatorios_jusante))

#Matrix to assess correlation between fixed varoables
correlation_matrix <- cor(fixos_jusante, use = "complete.obs")
print(correlation_matrix)
corrplot(correlation_matrix, method = "circle")

vif_model <- lm(mortalidade ~ ., data = data.frame(mortalidade = dados_jusante$mortalidade, fixos_jusante))
vif_values <- vif(vif_model)
high_vif_vars <- names(vif_values[vif_values > 5])
print(high_vif_vars)

#Improving the variables, i.e., removing those with high correlation 

fixos_jusante<-dados_jusante[,c("area_territorio", "numero_vizinhos", "altitude_mediana", "proporcao_cebolama",  "proporcao_samambaia", "proporcao_mangue", "foquilhas", "chuva_acumulada_simepar", "dias_chuva_simepar", "mare_maxima", "temperatura_maxima_simepar", "temperatura_minima_simepar")]

fixos_jusante<-data.matrix(fixos_jusante)

lista<-list(aleatorios_jusante=aleatorios_jusante, fixos_jusante=fixos_jusante, identifier=identifier, output=output, time=time)

modeloMERF<-MERF(X = lista$fixos_jusante, Y = lista$output, Z = lista$aleatorios_jusante, id = lista$identifier, time = lista$time, sto="OrnUhl", mtry = 5, ntree=1000, delta=0.001, iter=100)

#Assessing variable importance
#random
matriz_aleatorios<-modeloMERF$var_random_effects
intercepts<-modeloMERF$random_effects

matriz_aleatorios
intercepts

sum((sqrt(intercepts[,1]^2))) 
sum((sqrt(intercepts[,2]^2)))
sum((sqrt(intercepts[,3]^2)))
sum((sqrt(intercepts[,4]^2))) 

#fixed
importancia<-modeloMERF$forest$importance
importancia<-importancia[,1]
order(importancia)

rm(list = ls())

#importing upstream data
dados_jusante<- read.csv("dados_jusante.csv", sep=";")
#excluding non-important columns
dados_jusante <- dados_jusante[, -c(8, 20)]
dados_jusante<-na.omit(dados_jusante)

#transforming qualitative variables in factors
colnames(dados_jusante)
dados_jusante$territorio<-as.factor(dados_jusante$territorio)
dados_jusante$qualidade_samambaia<-factor(dados_jusante$qualidade_samambaia, levels=c("ausenteouruim", "boa"), ordered=T)
dados_jusante$foquilhas<-as.factor(dados_jusante$foquilhas)
dados_jusante$femea<-as.factor(dados_jusante$femea)
dados_jusante$macho<-as.factor(dados_jusante$macho)

#log transforming the quantitative variables
dados_jusante$area_territorio<-log(dados_jusante$area_territorio+1)
dados_jusante$perimetro<-log(dados_jusante$perimetro+1)
dados_jusante$razao_perimetro_area<-log(dados_jusante$razao_perimetro_area+1)
dados_jusante$numero_vizinhos<-log(dados_jusante$numero_vizinhos+1)
dados_jusante$altitude_mediana<-log(dados_jusante$altitude_mediana+1)
dados_jusante$proporcao_mangue<-log(dados_jusante$proporcao_mangue+1)
dados_jusante$area_mangue<-log(dados_jusante$area_mangue+1)
dados_jusante$proporcao_cebolama<-log(dados_jusante$proporcao_cebolama+1)
dados_jusante$area_cebolama<-log(dados_jusante$area_cebolama+1)
dados_jusante$proporcao_samambaia<-log(dados_jusante$proporcao_samambaia+1)
dados_jusante$area_samambaia<-log(dados_jusante$area_samambaia+1)
dados_jusante$idade_femea<-log(dados_jusante$idade_femea+1)
dados_jusante$idade_macho<-log(dados_jusante$idade_macho+1)
dados_jusante$chuva_acumulada_simepar<-log(dados_jusante$chuva_acumulada_simepar+1)
dados_jusante$chuva_forte_violenta_simepar<-log(dados_jusante$chuva_forte_violenta_simepar+1)
dados_jusante$dias_chuva_simepar<-log(dados_jusante$dias_chuva_simepar+1)
dados_jusante$mare_maxima<-log(dados_jusante$mare_maxima+1)
dados_jusante$temperatura_media_simepar<-log(dados_jusante$temperatura_media_simepar+1)
dados_jusante$temperatura_maxima_simepar<-log(dados_jusante$temperatura_maxima_simepar+1)
dados_jusante$temperatura_minima_simepar<-log(dados_jusante$temperatura_minima_simepar+1)

#scaling transforming the quantitative variables
dados_jusante$ano<-scale(dados_jusante$ano)
dados_jusante$area_territorio<-scale(dados_jusante$area_territorio)
dados_jusante$perimetro<-scale(dados_jusante$perimetro)
dados_jusante$razao_perimetro_area<-scale(dados_jusante$razao_perimetro_area)
dados_jusante$numero_vizinhos<-scale(dados_jusante$numero_vizinhos)
dados_jusante$altitude_mediana<-scale(dados_jusante$altitude_mediana)
dados_jusante$proporcao_mangue<-scale(dados_jusante$proporcao_mangue)
dados_jusante$area_mangue<-scale(dados_jusante$area_mangue)
dados_jusante$proporcao_cebolama<-scale(dados_jusante$proporcao_cebolama)
dados_jusante$area_cebolama<-scale(dados_jusante$area_cebolama)
dados_jusante$proporcao_samambaia<-scale(dados_jusante$proporcao_samambaia)
dados_jusante$area_samambaia<-scale(dados_jusante$area_samambaia)
dados_jusante$idade_femea<-scale(dados_jusante$idade_femea)
dados_jusante$idade_macho<-scale(dados_jusante$idade_macho)
dados_jusante$chuva_acumulada_simepar<-scale(dados_jusante$chuva_acumulada_simepar)
dados_jusante$chuva_forte_violenta_simepar<-scale(dados_jusante$chuva_forte_violenta_simepar)
dados_jusante$dias_chuva_simepar<-scale(dados_jusante$dias_chuva_simepar)
dados_jusante$mare_maxima<-scale(dados_jusante$mare_maxima)
dados_jusante$temperatura_media_simepar<-scale(dados_jusante$temperatura_media_simepar)
dados_jusante$temperatura_maxima_simepar<-scale(dados_jusante$temperatura_maxima_simepar)
dados_jusante$temperatura_minima_simepar<-scale(dados_jusante$temperatura_minima_simepar)

#Poisson
glmm_poisson <- glmer(mortalidade ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|femea) + (1|macho) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson)
print(AIC(glmm_poisson))
print(BIC(glmm_poisson))

#Binomal negative
glmm_negbin <- glmer.nb(mortalidade ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama + (1|femea) + (1|macho) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), data = dados_jusante)
summary(glmm_negbin)
print(AIC(glmm_negbin))
print(BIC(glmm_negbin))

glmm_poisson1 <- glmer(mortalidade ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|femea) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson1)
print(AIC(glmm_poisson1))
print(BIC(glmm_poisson1))

glmm_poisson2 <- glmer(mortalidade ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  +  (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson2)
print(AIC(glmm_poisson2))
print(BIC(glmm_poisson2))

glmm_poisson3 <- glmer(mortalidade ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama + (1|idade_macho)+ (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson3)
print(AIC(glmm_poisson3))
print(BIC(glmm_poisson3))

glmm_poisson4 <- glmer(mortalidade ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson4)
print(AIC(glmm_poisson4))
print(BIC(glmm_poisson4))

glmm_poisson5 <- glmer(mortalidade ~ proporcao_samambaia + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson5)
print(AIC(glmm_poisson5))
print(BIC(glmm_poisson5))

glmm_poisson6 <- glmer(mortalidade ~ proporcao_samambaia + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson6)
print(AIC(glmm_poisson6))
print(BIC(glmm_poisson6))

glmm_poisson7 <- glmer(mortalidade ~ proporcao_samambaia + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson7)
print(AIC(glmm_poisson7))
print(BIC(glmm_poisson7))

glmm_poisson8 <- glmer(mortalidade ~ mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson8)
print(AIC(glmm_poisson8))
print(BIC(glmm_poisson8))

glmm_poisson9 <- glmer(mortalidade ~ numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson9)
print(AIC(glmm_poisson9))
print(BIC(glmm_poisson9))

glmm_poisson10 <- glmer(mortalidade ~ numero_vizinhos + dias_chuva_simepar + proporcao_mangue + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson10)
print(AIC(glmm_poisson10))
print(BIC(glmm_poisson10))

glmm_poisson11 <- glmer(mortalidade ~ dias_chuva_simepar + proporcao_mangue + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson11)
print(AIC(glmm_poisson11))
print(BIC(glmm_poisson11))

glmm_poisson12 <- glmer(mortalidade ~ dias_chuva_simepar + proporcao_mangue + temperatura_minima_simepar + proporcao_cebolama + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson12)
print(AIC(glmm_poisson12))
print(BIC(glmm_poisson12))

glmm_poisson13 <- glmer(mortalidade ~ proporcao_mangue + temperatura_minima_simepar + proporcao_cebolama + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson13)
print(AIC(glmm_poisson13))
print(BIC(glmm_poisson13))

glmm_poisson14 <- glmer(mortalidade ~ proporcao_mangue + proporcao_cebolama + (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson14)
print(AIC(glmm_poisson14))
print(BIC(glmm_poisson14))

glmm_poisson15 <- glmer(mortalidade ~ proporcao_mangue +  (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson15)
print(AIC(glmm_poisson15))
print(BIC(glmm_poisson15))

glmm_poisson16 <- glmer(mortalidade ~ 1 +  (1|territorio)+ (1|ano), family = poisson, data = dados_jusante, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_poisson16)
print(AIC(glmm_poisson16))
print(BIC(glmm_poisson16))

summary(glmm_poisson161)

#DP####
#upstream####
rm(list = ls())

#importing upstream data
dados_montante<- read.csv("dados_montante.csv", sep=";")
#excluding non-important columns
dados_montante <- dados_montante[, -c(9, 24)]
dados_montante<-na.omit(dados_montante)
dados_montante$balanco<-(dados_montante$produtividade_anual)-(dados_montante$mortalidade)

#transforming qualitative variables in factors
colnames(dados_montante)
dados_montante$territorio<-as.factor(dados_montante$territorio)
dados_montante$densidade_cebolama<-as.factor(dados_montante$densidade_cebolama)
dados_montante$densidade_cebolama<-factor(dados_montante$densidade_cebolama, levels = c("normal", "moderada", "densa"), ordered=T)
dados_montante$densidade_samambaia<-as.factor(dados_montante$densidade_samambaia)
dados_montante$qualidade_samambaia<-factor(dados_montante$qualidade_samambaia, levels=c("ausenteouruim", "moderada", "boa"), ordered=T)
dados_montante$densidade_capimserra<-as.factor(dados_montante$densidade_capimserra)
dados_montante$qualidade_capimserra<-factor(dados_montante$qualidade_capimserra, levels = c("ausenteouruim", "moderado", "denso"), ordered = T)#simplified
dados_montante$arboreas<-as.factor(dados_montante$arboreas)
dados_montante$aboreas_simplificado<-as.factor(dados_montante$aboreas_simplificado)
dados_montante$foquilhas<-as.factor(dados_montante$foquilhas)
dados_montante$femea<-as.factor(dados_montante$femea)
dados_montante$macho<-as.factor(dados_montante$macho)

dados_montante$area_territorio<-log(dados_montante$area_territorio+1)
dados_montante$perimetro<-log(dados_montante$perimetro+1)
dados_montante$razao_perimetro_area<-log(dados_montante$razao_perimetro_area+1)
dados_montante$numero_vizinhos<-log(dados_montante$numero_vizinhos+1)
dados_montante$altitude_mediana<-log(dados_montante$altitude_mediana+1)
dados_montante$area_cebolama<-log(dados_montante$area_cebolama+1)
dados_montante$proporcao_cebolama<-log(dados_montante$proporcao_cebolama+1)
dados_montante$area_samambaia<-log(dados_montante$area_samambaia+1)
dados_montante$proporcao_samambaia<-log(dados_montante$proporcao_samambaia+1)
dados_montante$area_capimserra<-log(dados_montante$area_capimserra+1)
dados_montante$proporcao_capimserra<-log(dados_montante$proporcao_capimserra+1)
dados_montante$idade_macho<-log(dados_montante$idade_macho+1)
dados_montante$idade_femea<-log(dados_montante$idade_femea+1)
dados_montante$chuva_acumulada_simepar<-log(dados_montante$chuva_acumulada_simepar+1)
dados_montante$chuva_forte_violenta_simepar<-log(dados_montante$chuva_forte_violenta_simepar+1)
dados_montante$dias_chuva_simepar<-log(dados_montante$dias_chuva_simepar+1)
dados_montante$mare_maxima<-log(dados_montante$mare_maxima+1)
dados_montante$temperatura_media_simepar<-log(dados_montante$temperatura_media_simepar+1)
dados_montante$temperatura_maxima_simepar<-log(dados_montante$temperatura_maxima_simepar+1)
dados_montante$temperatura_minima_simepar<-log(dados_montante$temperatura_minima_simepar+1)

#scaling quantitative variables
dados_montante$area_territorio<-scale(dados_montante$area_territorio)
dados_montante$perimetro<-scale(dados_montante$perimetro)
dados_montante$razao_perimetro_area<-scale(dados_montante$razao_perimetro_area)
dados_montante$numero_vizinhos<-scale(dados_montante$numero_vizinhos)
dados_montante$altitude_mediana<-scale(dados_montante$altitude_mediana)
dados_montante$area_cebolama<-scale(dados_montante$area_cebolama)
dados_montante$proporcao_cebolama<-scale(dados_montante$proporcao_cebolama)
dados_montante$area_samambaia<-scale(dados_montante$area_samambaia)
dados_montante$proporcao_samambaia<-scale(dados_montante$proporcao_samambaia)
dados_montante$area_capimserra<-scale(dados_montante$area_capimserra)
dados_montante$proporcao_capimserra<-scale(dados_montante$proporcao_capimserra)
dados_montante$idade_macho<-scale(dados_montante$idade_macho)
dados_montante$idade_femea<-scale(dados_montante$idade_femea)
dados_montante$chuva_acumulada_simepar<-scale(dados_montante$chuva_acumulada_simepar)
dados_montante$chuva_forte_violenta_simepar<-scale(dados_montante$chuva_forte_violenta_simepar)
dados_montante$dias_chuva_simepar<-scale(dados_montante$dias_chuva_simepar)
dados_montante$mare_maxima<-scale(dados_montante$mare_maxima)
dados_montante$temperatura_media_simepar<-scale(dados_montante$temperatura_media_simepar)
dados_montante$temperatura_maxima_simepar<-scale(dados_montante$temperatura_maxima_simepar)
dados_montante$temperatura_minima_simepar<-scale(dados_montante$temperatura_minima_simepar)

#transforming everything to the model of the function
#fixed effects
fixos_montante<-dados_montante[,c("area_territorio", "perimetro", "razao_perimetro_area", "numero_vizinhos", "altitude_mediana", "area_cebolama", "proporcao_cebolama", "densidade_cebolama", "area_samambaia", "proporcao_samambaia", "qualidade_samambaia", "area_capimserra", "proporcao_capimserra", "qualidade_capimserra", "arboreas", "aboreas_simplificado", "foquilhas", "chuva_acumulada_simepar", "chuva_forte_violenta_simepar", "dias_chuva_simepar", "mare_maxima", "temperatura_maxima_simepar", "temperatura_minima_simepar", "temperatura_media_simepar")]

fixos_montante<-data.matrix(fixos_montante)

dados_montante$territorio<-as.numeric(dados_montante$territorio)

identifier<-dados_montante$territorio
identifier<-as.integer(dados_montante$territorio)

dados_montante$balanco<-as.vector(dados_montante$balanco)
output<-dados_montante$balanco

time<-dados_montante$ano_simples
time<-as.numeric(time)

#randomeffects
aleatorios_montante<-dados_montante[,c("femea", "macho", "idade_femea", "idade_macho")]

aleatorios_montante<-data.matrix((aleatorios_montante))

#Matrix to assess correlation between fixed varoables
correlation_matrix <- cor(fixos_montante, use = "complete.obs")
print(correlation_matrix)
corrplot(correlation_matrix, method = "circle")

vif_model <- lm(balanco ~ ., data = data.frame(balanco = dados_montante$balanco, fixos_montante))
vif_values <- vif(vif_model)
high_vif_vars <- names(vif_values[vif_values > 5])
print(high_vif_vars)

#Improving the variables, i.e., removing those with high correlation 
fixos_montante<-dados_montante[,c( "perimetro", "razao_perimetro_area", "numero_vizinhos", "altitude_mediana", "area_cebolama", "densidade_cebolama", "area_samambaia",  "qualidade_samambaia", "area_capimserra","qualidade_capimserra", "arboreas",  "foquilhas", "chuva_acumulada_simepar","dias_chuva_simepar", "mare_maxima", "temperatura_maxima_simepar", "temperatura_minima_simepar")]

fixos_montante<-data.matrix(fixos_montante)

lista<-list(aleatorios_montante=aleatorios_montante, fixos_montante=fixos_montante, identifier=identifier, output=output, time=time)

modeloMERF<-MERF(X = lista$fixos_montante, Y = lista$output, Z = lista$aleatorios_montante, id = lista$identifier, time = lista$time, sto="OrnUhl", mtry = 5, ntree=1000, delta=0.001, iter=100)

#assessing the importance of the variable
#random
matriz_aleatorios<-modeloMERF$var_random_effects
intercepts<-modeloMERF$random_effects

matriz_aleatorios
intercepts

sum((sqrt(intercepts[,1]^2))) 
sum((sqrt(intercepts[,2]^2)))
sum((sqrt(intercepts[,3]^2)))
sum((sqrt(intercepts[,4]^2))) 

#fixed
importancia<-modeloMERF$forest$importance
importancia<-importancia[,1]
order(importancia)

rm(list = ls())
#importing  data
dados_montante<- read.csv("dados_montante.csv", sep=";")
#excluding non-important columns
dados_montante <- dados_montante[, -c(9, 24)]
dados_montante<-na.omit(dados_montante)
dados_montante$balanco<-(dados_montante$produtividade_anual)-(dados_montante$mortalidade)

#transforming qualitative variables in factors
colnames(dados_montante)
dados_montante$ano<-as.factor(dados_montante$ano)
dados_montante$territorio<-as.factor(dados_montante$territorio)
dados_montante$densidade_cebolama<-factor(dados_montante$densidade_cebolama, levels = c("normal", "moderada", "densa"), ordered=T)
dados_montante$densidade_samambaia<-as.factor(dados_montante$densidade_samambaia)
dados_montante$qualidade_samambaia<-factor(dados_montante$qualidade_samambaia, levels=c("ausenteouruim", "moderada", "boa"), ordered=T)
dados_montante$densidade_capimserra<-as.factor(dados_montante$densidade_capimserra)
dados_montante$qualidade_capimserra<-factor(dados_montante$qualidade_capimserra, levels = c("ausenteouruim", "moderado", "denso"), ordered = T)#simplified
dados_montante$arboreas<-as.factor(dados_montante$arboreas)
dados_montante$aboreas_simplificado<-as.factor(dados_montante$aboreas_simplificado)
dados_montante$foquilhas<-as.factor(dados_montante$foquilhas)
dados_montante$femea<-as.factor(dados_montante$femea)
dados_montante$macho<-as.factor(dados_montante$macho)

dados_montante$area_territorio<-log(dados_montante$area_territorio+1)
dados_montante$perimetro<-log(dados_montante$perimetro+1)
dados_montante$razao_perimetro_area<-log(dados_montante$razao_perimetro_area+1)
dados_montante$numero_vizinhos<-log(dados_montante$numero_vizinhos+1)
dados_montante$altitude_mediana<-log(dados_montante$altitude_mediana+1)
dados_montante$area_cebolama<-log(dados_montante$area_cebolama+1)
dados_montante$proporcao_cebolama<-log(dados_montante$proporcao_cebolama+1)
dados_montante$area_samambaia<-log(dados_montante$area_samambaia+1)
dados_montante$proporcao_samambaia<-log(dados_montante$proporcao_samambaia+1)
dados_montante$area_capimserra<-log(dados_montante$area_capimserra+1)
dados_montante$proporcao_capimserra<-log(dados_montante$proporcao_capimserra+1)
dados_montante$idade_macho<-log(dados_montante$idade_macho+1)
dados_montante$idade_femea<-log(dados_montante$idade_femea+1)
dados_montante$chuva_acumulada_simepar<-log(dados_montante$chuva_acumulada_simepar+1)
dados_montante$chuva_forte_violenta_simepar<-log(dados_montante$chuva_forte_violenta_simepar+1)
dados_montante$dias_chuva_simepar<-log(dados_montante$dias_chuva_simepar+1)
dados_montante$mare_maxima<-log(dados_montante$mare_maxima+1)
dados_montante$temperatura_media_simepar<-log(dados_montante$temperatura_media_simepar+1)
dados_montante$temperatura_maxima_simepar<-log(dados_montante$temperatura_maxima_simepar+1)
dados_montante$temperatura_minima_simepar<-log(dados_montante$temperatura_minima_simepar+1)

#scaling quantitative variables
dados_montante$area_territorio<-scale(dados_montante$area_territorio)
dados_montante$perimetro<-scale(dados_montante$perimetro)
dados_montante$razao_perimetro_area<-scale(dados_montante$razao_perimetro_area)
dados_montante$numero_vizinhos<-scale(dados_montante$numero_vizinhos)
dados_montante$altitude_mediana<-scale(dados_montante$altitude_mediana)
dados_montante$area_cebolama<-scale(dados_montante$area_cebolama)
dados_montante$proporcao_cebolama<-scale(dados_montante$proporcao_cebolama)
dados_montante$area_samambaia<-scale(dados_montante$area_samambaia)
dados_montante$proporcao_samambaia<-scale(dados_montante$proporcao_samambaia)
dados_montante$area_capimserra<-scale(dados_montante$area_capimserra)
dados_montante$proporcao_capimserra<-scale(dados_montante$proporcao_capimserra)
dados_montante$idade_macho<-scale(dados_montante$idade_macho)
dados_montante$idade_femea<-scale(dados_montante$idade_femea)
dados_montante$chuva_acumulada_simepar<-scale(dados_montante$chuva_acumulada_simepar)
dados_montante$chuva_forte_violenta_simepar<-scale(dados_montante$chuva_forte_violenta_simepar)
dados_montante$dias_chuva_simepar<-scale(dados_montante$dias_chuva_simepar)
dados_montante$mare_maxima<-scale(dados_montante$mare_maxima)
dados_montante$temperatura_media_simepar<-scale(dados_montante$temperatura_media_simepar)
dados_montante$temperatura_maxima_simepar<-scale(dados_montante$temperatura_maxima_simepar)
dados_montante$temperatura_minima_simepar<-scale(dados_montante$temperatura_minima_simepar)

####
glmm_gaussian <- lmer(balanco ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|femea) + (1|macho) + (1|idade_femea) + (1|idade_macho) + (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian)
print(AIC(glmm_gaussian))

glmm_gaussian1 <- lmer(balanco ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|macho) + (1|idade_femea) + (1|idade_macho) + (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian1)
print(AIC(glmm_gaussian1))

glmm_gaussian2 <- lmer(balanco ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|idade_femea) + (1|idade_macho) + (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian2)
print(AIC(glmm_gaussian2))

glmm_gaussian3 <- lmer(balanco ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian3)
print(AIC(glmm_gaussian3))

glmm_gaussian4 <- lmer(balanco ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + densidade_cebolama + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian4)
print(AIC(glmm_gaussian4))

glmm_gaussian4 <- lmer(balanco ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + mare_maxima + perimetro + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian4)
print(AIC(glmm_gaussian4))

glmm_gaussian5 <- lmer(balanco ~ altitude_mediana + numero_vizinhos + area_samambaia + razao_perimetro_area + foquilhas + perimetro + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian5)
print(AIC(glmm_gaussian5))

glmm_gaussian6 <- lmer(balanco ~ altitude_mediana + area_samambaia + razao_perimetro_area + foquilhas + perimetro + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian6)
print(AIC(glmm_gaussian6))

glmm_gaussian7 <- lmer(balanco ~ altitude_mediana + area_samambaia + razao_perimetro_area  + perimetro + qualidade_samambaia + arboreas + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian7)
print(AIC(glmm_gaussian7))

glmm_gaussian8 <- lmer(balanco ~ altitude_mediana + area_samambaia + razao_perimetro_area  + perimetro + qualidade_samambaia + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian8)
print(AIC(glmm_gaussian8))

glmm_gaussian9 <- lmer(balanco ~ altitude_mediana + area_samambaia + razao_perimetro_area  + qualidade_samambaia + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian9)
print(AIC(glmm_gaussian9))

glmm_gaussian10 <- lmer(balanco ~ altitude_mediana + area_samambaia + razao_perimetro_area + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + temperatura_minima_simepar + temperatura_maxima_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian10)
print(AIC(glmm_gaussian10))

glmm_gaussian11 <- lmer(balanco ~ altitude_mediana + area_samambaia + razao_perimetro_area + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar  + temperatura_maxima_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian11)
print(AIC(glmm_gaussian11))

glmm_gaussian12 <- lmer(balanco ~ altitude_mediana + area_samambaia + razao_perimetro_area + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian12)
print(AIC(glmm_gaussian12))

glmm_gaussian13 <- lmer(balanco ~ altitude_mediana + razao_perimetro_area + area_cebolama + qualidade_capimserra + chuva_acumulada_simepar + dias_chuva_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian13)
print(AIC(glmm_gaussian13))

glmm_gaussian14 <- lmer(balanco ~ altitude_mediana + razao_perimetro_area + area_cebolama + qualidade_capimserra + dias_chuva_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian14)
print(AIC(glmm_gaussian14))

glmm_gaussian15 <- lmer(balanco ~ razao_perimetro_area + area_cebolama + qualidade_capimserra + dias_chuva_simepar + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian15)
print(AIC(glmm_gaussian15))

glmm_gaussian16 <- lmer(balanco ~ razao_perimetro_area + area_cebolama + qualidade_capimserra + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian16)
print(AIC(glmm_gaussian16))

glmm_gaussian17 <- lmer(balanco ~ razao_perimetro_area + area_cebolama  + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian17)
print(AIC(glmm_gaussian17))

glmm_gaussian18 <- lmer(balanco ~ area_cebolama  + area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian18)
print(AIC(glmm_gaussian18))

glmm_gaussian19 <- lmer(balanco ~ area_capimserra + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian19)
print(AIC(glmm_gaussian19))

glmm_gaussian20 <- lmer(balanco ~ 1 + (1|idade_femea) +  (1|territorio) + (1|ano),   data = dados_montante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian20)
print(AIC(glmm_gaussian20))


#downstream#####
rm(list = ls())

#importing data
dados_jusante<- read.csv("dados_jusante.csv", sep=";")
#excluding non-important columns
dados_jusante <- dados_jusante[, -c(8, 20)]
dados_jusante<-na.omit(dados_jusante)
dados_jusante$balanco<-(dados_jusante$produtividade_anual)-(dados_jusante$mortalidade)

#transforming qualitative variables in factors
colnames(dados_jusante)
dados_jusante$territorio<-as.factor(dados_jusante$territorio)
dados_jusante$qualidade_samambaia<-factor(dados_jusante$qualidade_samambaia, levels=c("ausenteouruim", "boa"), ordered=T)
dados_jusante$foquilhas<-as.factor(dados_jusante$foquilhas)
dados_jusante$femea<-as.factor(dados_jusante$femea)
dados_jusante$macho<-as.factor(dados_jusante$macho)

#log transforming the quantitative variables
dados_jusante$area_territorio<-log(dados_jusante$area_territorio+1)
dados_jusante$perimetro<-log(dados_jusante$perimetro+1)
dados_jusante$razao_perimetro_area<-log(dados_jusante$razao_perimetro_area+1)
dados_jusante$numero_vizinhos<-log(dados_jusante$numero_vizinhos+1)
dados_jusante$altitude_mediana<-log(dados_jusante$altitude_mediana+1)
dados_jusante$proporcao_mangue<-log(dados_jusante$proporcao_mangue+1)
dados_jusante$area_mangue<-log(dados_jusante$area_mangue+1)
dados_jusante$proporcao_cebolama<-log(dados_jusante$proporcao_cebolama+1)
dados_jusante$area_cebolama<-log(dados_jusante$area_cebolama+1)
dados_jusante$proporcao_samambaia<-log(dados_jusante$proporcao_samambaia+1)
dados_jusante$area_samambaia<-log(dados_jusante$area_samambaia+1)
dados_jusante$idade_femea<-log(dados_jusante$idade_femea+1)
dados_jusante$idade_macho<-log(dados_jusante$idade_macho+1)
dados_jusante$chuva_acumulada_simepar<-log(dados_jusante$chuva_acumulada_simepar+1)
dados_jusante$chuva_forte_violenta_simepar<-log(dados_jusante$chuva_forte_violenta_simepar+1)
dados_jusante$dias_chuva_simepar<-log(dados_jusante$dias_chuva_simepar+1)
dados_jusante$mare_maxima<-log(dados_jusante$mare_maxima+1)
dados_jusante$temperatura_media_simepar<-log(dados_jusante$temperatura_media_simepar+1)
dados_jusante$temperatura_maxima_simepar<-log(dados_jusante$temperatura_maxima_simepar+1)
dados_jusante$temperatura_minima_simepar<-log(dados_jusante$temperatura_minima_simepar+1)

#scaling transforming the quantitative variables
dados_jusante$area_territorio<-scale(dados_jusante$area_territorio)
dados_jusante$perimetro<-scale(dados_jusante$perimetro)
dados_jusante$razao_perimetro_area<-scale(dados_jusante$razao_perimetro_area)
dados_jusante$numero_vizinhos<-scale(dados_jusante$numero_vizinhos)
dados_jusante$altitude_mediana<-scale(dados_jusante$altitude_mediana)
dados_jusante$proporcao_mangue<-scale(dados_jusante$proporcao_mangue)
dados_jusante$area_mangue<-scale(dados_jusante$area_mangue)
dados_jusante$proporcao_cebolama<-scale(dados_jusante$proporcao_cebolama)
dados_jusante$area_cebolama<-scale(dados_jusante$area_cebolama)
dados_jusante$proporcao_samambaia<-scale(dados_jusante$proporcao_samambaia)
dados_jusante$area_samambaia<-scale(dados_jusante$area_samambaia)
dados_jusante$idade_femea<-scale(dados_jusante$idade_femea)
dados_jusante$idade_macho<-scale(dados_jusante$idade_macho)
dados_jusante$chuva_acumulada_simepar<-scale(dados_jusante$chuva_acumulada_simepar)
dados_jusante$chuva_forte_violenta_simepar<-scale(dados_jusante$chuva_forte_violenta_simepar)
dados_jusante$dias_chuva_simepar<-scale(dados_jusante$dias_chuva_simepar)
dados_jusante$mare_maxima<-scale(dados_jusante$mare_maxima)
dados_jusante$temperatura_media_simepar<-scale(dados_jusante$temperatura_media_simepar)
dados_jusante$temperatura_maxima_simepar<-scale(dados_jusante$temperatura_maxima_simepar)
dados_jusante$temperatura_minima_simepar<-scale(dados_jusante$temperatura_minima_simepar)

#transforming everything to the model of the function
#fixed effects
fixos_jusante<-dados_jusante[,c("area_territorio", "perimetro", "razao_perimetro_area", "numero_vizinhos", "altitude_mediana", "area_cebolama", "proporcao_cebolama",  "area_samambaia", "proporcao_samambaia", "qualidade_samambaia", "proporcao_mangue", "area_mangue", "foquilhas", "chuva_acumulada_simepar", "chuva_forte_violenta_simepar", "dias_chuva_simepar", "mare_maxima", "temperatura_maxima_simepar", "temperatura_minima_simepar", "temperatura_media_simepar")]

fixos_jusante<-data.matrix(fixos_jusante)

dados_jusante$territorio<-as.numeric(dados_jusante$territorio)

identifier<-dados_jusante$territorio
identifier<-as.integer(dados_jusante$territorio)

balanco<-as.vector(dados_jusante$balanco)
output<-balanco
output<-as.numeric(output)

time<-dados_jusante$ano
time<-as.numeric(time)

#randomeffects
aleatorios_jusante<-dados_jusante[,c("femea", "macho", "idade_femea", "idade_macho")]

aleatorios_jusante<-data.matrix((aleatorios_jusante))

#Matrix to assess correlation between fixed varoables
correlation_matrix <- cor(fixos_jusante, use = "complete.obs")
print(correlation_matrix)
corrplot(correlation_matrix, method = "circle")

vif_model <- lm(mortalidade ~ ., data = data.frame(balanco = dados_jusante$balanco, fixos_jusante))
vif_values <- vif(vif_model)
high_vif_vars <- names(vif_values[vif_values > 5])
print(high_vif_vars)

#Improving the variables, i.e., removing those with high correlation 

fixos_jusante<-dados_jusante[,c("area_territorio", "numero_vizinhos", "altitude_mediana", "proporcao_cebolama",  "proporcao_samambaia", "proporcao_mangue", "foquilhas", "chuva_acumulada_simepar", "dias_chuva_simepar", "mare_maxima", "temperatura_maxima_simepar", "temperatura_minima_simepar")]

fixos_jusante<-data.matrix(fixos_jusante)

lista<-list(aleatorios_jusante=aleatorios_jusante, fixos_jusante=fixos_jusante, identifier=identifier, output=output, time=time)

modeloMERF<-MERF(X = lista$fixos_jusante, Y = lista$output, Z = lista$aleatorios_jusante, id = lista$identifier, time = lista$time, sto="OrnUhl", mtry = 5, ntree=1000, delta=0.001, iter=100)

#Assessing variable importance
#random
matriz_aleatorios<-modeloMERF$var_random_effects
intercepts<-modeloMERF$random_effects

matriz_aleatorios
intercepts

sum((sqrt(intercepts[,1]^2))) 
sum((sqrt(intercepts[,2]^2)))
sum((sqrt(intercepts[,3]^2)))
sum((sqrt(intercepts[,4]^2))) 

#fixed
importancia<-modeloMERF$forest$importance
importancia<-importancia[,1]
order(importancia)

rm(list = ls())

#importing upstream data
dados_jusante<- read.csv("dados_jusante.csv", sep=";")
#excluding non-important columns
dados_jusante <- dados_jusante[, -c(8, 20)]
dados_jusante<-na.omit(dados_jusante)
dados_jusante$balanco<-(dados_jusante$produtividade_anual)-(dados_jusante$mortalidade)

#transforming qualitative variables in factors
colnames(dados_jusante)
dados_jusante$territorio<-as.factor(dados_jusante$territorio)
dados_jusante$qualidade_samambaia<-factor(dados_jusante$qualidade_samambaia, levels=c("ausenteouruim", "boa"), ordered=T)
dados_jusante$foquilhas<-as.factor(dados_jusante$foquilhas)
dados_jusante$femea<-as.factor(dados_jusante$femea)
dados_jusante$macho<-as.factor(dados_jusante$macho)

#log transforming the quantitative variables
dados_jusante$area_territorio<-log(dados_jusante$area_territorio+1)
dados_jusante$perimetro<-log(dados_jusante$perimetro+1)
dados_jusante$razao_perimetro_area<-log(dados_jusante$razao_perimetro_area+1)
dados_jusante$numero_vizinhos<-log(dados_jusante$numero_vizinhos+1)
dados_jusante$altitude_mediana<-log(dados_jusante$altitude_mediana+1)
dados_jusante$proporcao_mangue<-log(dados_jusante$proporcao_mangue+1)
dados_jusante$area_mangue<-log(dados_jusante$area_mangue+1)
dados_jusante$proporcao_cebolama<-log(dados_jusante$proporcao_cebolama+1)
dados_jusante$area_cebolama<-log(dados_jusante$area_cebolama+1)
dados_jusante$proporcao_samambaia<-log(dados_jusante$proporcao_samambaia+1)
dados_jusante$area_samambaia<-log(dados_jusante$area_samambaia+1)
dados_jusante$idade_femea<-log(dados_jusante$idade_femea+1)
dados_jusante$idade_macho<-log(dados_jusante$idade_macho+1)
dados_jusante$chuva_acumulada_simepar<-log(dados_jusante$chuva_acumulada_simepar+1)
dados_jusante$chuva_forte_violenta_simepar<-log(dados_jusante$chuva_forte_violenta_simepar+1)
dados_jusante$dias_chuva_simepar<-log(dados_jusante$dias_chuva_simepar+1)
dados_jusante$mare_maxima<-log(dados_jusante$mare_maxima+1)
dados_jusante$temperatura_media_simepar<-log(dados_jusante$temperatura_media_simepar+1)
dados_jusante$temperatura_maxima_simepar<-log(dados_jusante$temperatura_maxima_simepar+1)
dados_jusante$temperatura_minima_simepar<-log(dados_jusante$temperatura_minima_simepar+1)

#scaling transforming the quantitative variables
dados_jusante$ano<-scale(dados_jusante$ano)
dados_jusante$area_territorio<-scale(dados_jusante$area_territorio)
dados_jusante$perimetro<-scale(dados_jusante$perimetro)
dados_jusante$razao_perimetro_area<-scale(dados_jusante$razao_perimetro_area)
dados_jusante$numero_vizinhos<-scale(dados_jusante$numero_vizinhos)
dados_jusante$altitude_mediana<-scale(dados_jusante$altitude_mediana)
dados_jusante$proporcao_mangue<-scale(dados_jusante$proporcao_mangue)
dados_jusante$area_mangue<-scale(dados_jusante$area_mangue)
dados_jusante$proporcao_cebolama<-scale(dados_jusante$proporcao_cebolama)
dados_jusante$area_cebolama<-scale(dados_jusante$area_cebolama)
dados_jusante$proporcao_samambaia<-scale(dados_jusante$proporcao_samambaia)
dados_jusante$area_samambaia<-scale(dados_jusante$area_samambaia)
dados_jusante$idade_femea<-scale(dados_jusante$idade_femea)
dados_jusante$idade_macho<-scale(dados_jusante$idade_macho)
dados_jusante$chuva_acumulada_simepar<-scale(dados_jusante$chuva_acumulada_simepar)
dados_jusante$chuva_forte_violenta_simepar<-scale(dados_jusante$chuva_forte_violenta_simepar)
dados_jusante$dias_chuva_simepar<-scale(dados_jusante$dias_chuva_simepar)
dados_jusante$mare_maxima<-scale(dados_jusante$mare_maxima)
dados_jusante$temperatura_media_simepar<-scale(dados_jusante$temperatura_media_simepar)
dados_jusante$temperatura_maxima_simepar<-scale(dados_jusante$temperatura_maxima_simepar)
dados_jusante$temperatura_minima_simepar<-scale(dados_jusante$temperatura_minima_simepar)

glmm_gaussian <- lmer(balanco ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|femea) + (1|macho) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano),   data = dados_jusante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian)
print(AIC(glmm_gaussian))

glmm_gaussian1 <- lmer(balanco ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|femea) + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano),   data = dados_jusante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian1)
print(AIC(glmm_gaussian1))

glmm_gaussian2 <- lmer(balanco ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|idade_femea) + (1|idade_macho)+ (1|territorio)+ (1|ano),   data = dados_jusante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian2)
print(AIC(glmm_gaussian2))

glmm_gaussian3 <- lmer(balanco ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  + (1|idade_femea)+ (1|territorio)+ (1|ano),   data = dados_jusante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian3)
print(AIC(glmm_gaussian3))

glmm_gaussian4 <- lmer(balanco ~ proporcao_samambaia + area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  +  (1|territorio)+ (1|ano),   data = dados_jusante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian4)
print(AIC(glmm_gaussian4))

glmm_gaussian5 <- lmer(balanco ~ area_territorio + foquilhas + mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  +  (1|territorio)+ (1|ano),   data = dados_jusante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian5)
print(AIC(glmm_gaussian5))

Anova(glmm_gaussian5)
summary(glht(glmm_gaussian5, linfct = mcp(foquilhas = 'Tukey')))


glmm_gaussian6 <- lmer(balanco ~ area_territorio +  mare_maxima + numero_vizinhos + dias_chuva_simepar + chuva_acumulada_simepar + proporcao_mangue + altitude_mediana + temperatura_maxima_simepar + temperatura_minima_simepar + proporcao_cebolama  +  (1|territorio)+ (1|ano),   data = dados_jusante,   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_gaussian6)
print(AIC(glmm_gaussian6))
