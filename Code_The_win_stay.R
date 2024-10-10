#####
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
library(lme4)
install.packages('afex')
library(afex)
library(nlme)
library(nlme)
library(bbmle)
library(lattice)
library(lme4)
library(MASS)
library(multcomp)
library(lmtest) 
library(lsmeans)
library(emmeans)
library(afex)
library(Design)
library(car)
library(binaryLogic)

#####
setwd("C:/Users/Usuario/Documents/Dissertacao_final/Capítulo_1")

#Análise 1 - Altura#####
rm(list = ls())
dadosefeitos<-read.csv("Dadosefeitos_ornit.csv", sep = ";")
dadosefeitos <- dadosefeitos[, !(names(dadosefeitos) %in% c("Expmacho", "Expfemea", "Casal", "Altura.total"))]


#transforming qualitative variables in factors
dadosefeitos$Ninho<-as.factor(dadosefeitos$Ninho)
dadosefeitos$Ambiente<-as.factor(dadosefeitos$Ambiente)
dadosefeitos$Territorio<-as.factor(dadosefeitos$Territorio)
dadosefeitos$Local<-as.factor(dadosefeitos$Local)
dadosefeitos$Femea<-as.factor(dadosefeitos$Femea)
dadosefeitos$Macho<-as.factor(dadosefeitos$Macho)
dadosefeitos$Destino<-as.factor(dadosefeitos$Destino)

#transforming qualitative variables in numeric
dadosefeitos$Ano<-as.numeric(dadosefeitos$Ano)
dadosefeitos$Altura<-as.numeric(dadosefeitos$Altura)
dadosefeitos$Altitude<-as.numeric(dadosefeitos$Altitude)
dadosefeitos$Espessura<-as.numeric(dadosefeitos$Espessura)
dadosefeitos$Idade_femea<-as.numeric(dadosefeitos$Idade_femea)
dadosefeitos$Idade_macho<-as.numeric(dadosefeitos$Idade_macho)


####
dadosefeitos_altura <- dadosefeitos[, !(names(dadosefeitos) %in% c("Altitude", "Espessura", "Esp"))]

dadosefeitos_altura<-na.omit(dadosefeitos_altura)

#N=149

#model construction
# Modelo Completo
mixed_efeitos_altura <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)

# Modelos com 5 Variáveis Aleatórias
mixed_efeitos_altura_s1 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s2 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s3 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s4 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s5 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s6 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)

# Modelos com 4 Variáveis Aleatórias
mixed_efeitos_altura_s7 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano)  + (1 | Femea) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s8 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s9 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s10 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s11 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) +  (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s12 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s13 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s14 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s15 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s16 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s17 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Macho) +  (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s18 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_femea) , data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s19 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s20 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s21 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Macho), data = dadosefeitos_altura, REML = TRUE)

#3
mixed_efeitos_altura_s22 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s23 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s24 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) , data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s25 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s26 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s27 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea)  + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s28 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s29 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + + (1 | Ambiente) + (1 | Femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s30 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s31 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s32 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s33 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s34 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s35 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s36 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s37 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s38 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s39 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s40 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s41 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)



#2
mixed_efeitos_altura_s42 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s43 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s44 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s45 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s46 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s47 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s48 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s49 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s50 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s51 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s52 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s53 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s54 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s55 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s56 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)

#1
mixed_efeitos_altura_s57 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s58 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s59 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s60 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Macho), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s61 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea), data = dadosefeitos_altura, REML = TRUE)
mixed_efeitos_altura_s62 <- lmer(Altura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho), data = dadosefeitos_altura, REML = TRUE)

selecao.alturaefeitoAIC<-AIC(mixed_efeitos_altura,mixed_efeitos_altura_s1, mixed_efeitos_altura_s2, mixed_efeitos_altura_s3, mixed_efeitos_altura_s4, mixed_efeitos_altura_s5, mixed_efeitos_altura_s6, mixed_efeitos_altura_s7, mixed_efeitos_altura_s8, mixed_efeitos_altura_s9, mixed_efeitos_altura_s10, mixed_efeitos_altura_s11, mixed_efeitos_altura_s12, mixed_efeitos_altura_s13, mixed_efeitos_altura_s14, mixed_efeitos_altura_s15, mixed_efeitos_altura_s16, mixed_efeitos_altura_s17, mixed_efeitos_altura_s18, mixed_efeitos_altura_s19, mixed_efeitos_altura_s20, mixed_efeitos_altura_s21, mixed_efeitos_altura_s22, mixed_efeitos_altura_s23, mixed_efeitos_altura_s24, mixed_efeitos_altura_s25, mixed_efeitos_altura_s26, mixed_efeitos_altura_s27, mixed_efeitos_altura_s28, mixed_efeitos_altura_s29, mixed_efeitos_altura_s30, mixed_efeitos_altura_s31, mixed_efeitos_altura_s32, mixed_efeitos_altura_s33, mixed_efeitos_altura_s34, mixed_efeitos_altura_s35, mixed_efeitos_altura_s36, mixed_efeitos_altura_s37, mixed_efeitos_altura_s38, mixed_efeitos_altura_s39, mixed_efeitos_altura_s40, mixed_efeitos_altura_s41, mixed_efeitos_altura_s42, mixed_efeitos_altura_s43, mixed_efeitos_altura_s44, mixed_efeitos_altura_s45, mixed_efeitos_altura_s46, mixed_efeitos_altura_s47, mixed_efeitos_altura_s48, mixed_efeitos_altura_s49, mixed_efeitos_altura_s50, mixed_efeitos_altura_s51, mixed_efeitos_altura_s52, mixed_efeitos_altura_s53, mixed_efeitos_altura_s54, mixed_efeitos_altura_s55, mixed_efeitos_altura_s56, mixed_efeitos_altura_s57, mixed_efeitos_altura_s58, mixed_efeitos_altura_s59, mixed_efeitos_altura_s60, mixed_efeitos_altura_s61, mixed_efeitos_altura_s62)

sort(selecao.alturaefeitoAIC$AIC)

summary(mixed_efeitos_altura_s58)

Anova(mixed_efeitos_altura_s58)

summary(glht(mixed_efeitos_altura_s58, linfct = mcp(Destino = "Predado - Alagado == 0")))
summary(glht(mixed_efeitos_altura_s58, linfct = mcp(Destino = "Sucesso - Alagado == 0")))
summary(glht(mixed_efeitos_altura_s58, linfct = mcp(Destino = "Tombado - Alagado == 0")))
summary(glht(mixed_efeitos_altura_s58, linfct = mcp(Destino = "Predado - Sucesso == 0")))
summary(glht(mixed_efeitos_altura_s58, linfct = mcp(Destino = "Tombado - Predado == 0")))
summary(glht(mixed_efeitos_altura_s58, linfct = mcp(Destino = "Tombado - Sucesso == 0")))


############################################################
rm(list = ls())
dadosefeitos<-read.csv("Dadosefeitos_ornit.csv", sep = ";")
dadosefeitos <- dadosefeitos[, !(names(dadosefeitos) %in% c("Expmacho", "Expfemea", "Casal", "Altura.total"))]


#transforming qualitative variables in factors
dadosefeitos$Ninho<-as.factor(dadosefeitos$Ninho)
dadosefeitos$Ambiente<-as.factor(dadosefeitos$Ambiente)
dadosefeitos$Territorio<-as.factor(dadosefeitos$Territorio)
dadosefeitos$Local<-as.factor(dadosefeitos$Local)
dadosefeitos$Femea<-as.factor(dadosefeitos$Femea)
dadosefeitos$Macho<-as.factor(dadosefeitos$Macho)
dadosefeitos$Destino<-as.factor(dadosefeitos$Destino)

#transforming qualitative variables in numeric
dadosefeitos$Ano<-as.numeric(dadosefeitos$Ano)
dadosefeitos$Altura<-as.numeric(dadosefeitos$Altura)
dadosefeitos$Altitude<-as.numeric(dadosefeitos$Altitude)
dadosefeitos$Espessura<-as.numeric(dadosefeitos$Espessura)
dadosefeitos$Idade_femea<-as.numeric(dadosefeitos$Idade_femea)
dadosefeitos$Idade_macho<-as.numeric(dadosefeitos$Idade_macho)


####Altitude####
rm(list = ls())
dadosefeitos<-read.csv("Dadosefeitos_ornit.csv", sep = ";")
dadosefeitos <- dadosefeitos[, !(names(dadosefeitos) %in% c("Expmacho", "Expfemea", "Casal", "Altura.total"))]

#transforming qualitative variables in factors
dadosefeitos$Ninho<-as.factor(dadosefeitos$Ninho)
dadosefeitos$Ambiente<-as.factor(dadosefeitos$Ambiente)
dadosefeitos$Territorio<-as.factor(dadosefeitos$Territorio)
dadosefeitos$Local<-as.factor(dadosefeitos$Local)
dadosefeitos$Femea<-as.factor(dadosefeitos$Femea)
dadosefeitos$Macho<-as.factor(dadosefeitos$Macho)
dadosefeitos$Destino<-as.factor(dadosefeitos$Destino)

#transforming qualitative variables in numeric
dadosefeitos$Ano<-as.numeric(dadosefeitos$Ano)
dadosefeitos$Altura<-as.numeric(dadosefeitos$Altura)
dadosefeitos$Altitude<-as.numeric(dadosefeitos$Altitude)
dadosefeitos$Espessura<-as.numeric(dadosefeitos$Espessura)
dadosefeitos$Idade_femea<-as.numeric(dadosefeitos$Idade_femea)
dadosefeitos$Idade_macho<-as.numeric(dadosefeitos$Idade_macho)


dadosefeitos_altitude <- dadosefeitos[, !(names(dadosefeitos) %in% c("Altura", "Espessura", "Esp"))]

dadosefeitos_altitude<-na.omit(dadosefeitos_altitude)

#N=105
#model construction
# Modelo Completo
mixed_efeitos_altitude <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)

# Modelos com 5 Variáveis Aleatórias
mixed_efeitos_altitude_s1 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s2 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s3 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s4 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s5 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s6 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)

# Modelos com 4 Variáveis Aleatórias
mixed_efeitos_altitude_s7 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano)  + (1 | Femea) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s8 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s9 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s10 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s11 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) +  (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s12 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s13 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s14 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s15 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s16 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s17 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Macho) +  (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s18 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_femea) , data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s19 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s20 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s21 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Macho), data = dadosefeitos_altitude, REML = TRUE)

#3
mixed_efeitos_altitude_s22 <- lmer(Altitude ~ Destino + (1 | Territorio)+ (1 | Ano) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s23 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s24 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) , data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s25 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s26 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s27 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea)  + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s28 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s29 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + + (1 | Ambiente) + (1 | Femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s30 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s31 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s32 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s33 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s34 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s35 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s36 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s37 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s38 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s39 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s40 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s41 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)



#2
mixed_efeitos_altitude_s42 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s43 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s44 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s45 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s46 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s47 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s48 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s49 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s50 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s51 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s52 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s53 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s54 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s55 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s56 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)

#1
mixed_efeitos_altitude_s57 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s58 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s59 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s60 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s61 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s62 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho), data = dadosefeitos_altitude, REML = TRUE)
mixed_efeitos_altitude_s63 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano), data = dadosefeitos_altitude, REML = TRUE)


selecao.altitudeefeitoAIC<-AIC(mixed_efeitos_altitude,mixed_efeitos_altitude_s1, mixed_efeitos_altitude_s2, mixed_efeitos_altitude_s3, mixed_efeitos_altitude_s4, mixed_efeitos_altitude_s5, mixed_efeitos_altitude_s6, mixed_efeitos_altitude_s7, mixed_efeitos_altitude_s8, mixed_efeitos_altitude_s9, mixed_efeitos_altitude_s10, mixed_efeitos_altitude_s11, mixed_efeitos_altitude_s12, mixed_efeitos_altitude_s13, mixed_efeitos_altitude_s14, mixed_efeitos_altitude_s15, mixed_efeitos_altitude_s16, mixed_efeitos_altitude_s17, mixed_efeitos_altitude_s18, mixed_efeitos_altitude_s19, mixed_efeitos_altitude_s20, mixed_efeitos_altitude_s21, mixed_efeitos_altitude_s22, mixed_efeitos_altitude_s23, mixed_efeitos_altitude_s24, mixed_efeitos_altitude_s25, mixed_efeitos_altitude_s26, mixed_efeitos_altitude_s27, mixed_efeitos_altitude_s28, mixed_efeitos_altitude_s29, mixed_efeitos_altitude_s30, mixed_efeitos_altitude_s31, mixed_efeitos_altitude_s32, mixed_efeitos_altitude_s33, mixed_efeitos_altitude_s34, mixed_efeitos_altitude_s35, mixed_efeitos_altitude_s36, mixed_efeitos_altitude_s37, mixed_efeitos_altitude_s38, mixed_efeitos_altitude_s39, mixed_efeitos_altitude_s40, mixed_efeitos_altitude_s41, mixed_efeitos_altitude_s42, mixed_efeitos_altitude_s43, mixed_efeitos_altitude_s44, mixed_efeitos_altitude_s45, mixed_efeitos_altitude_s46, mixed_efeitos_altitude_s47, mixed_efeitos_altitude_s48, mixed_efeitos_altitude_s49, mixed_efeitos_altitude_s50, mixed_efeitos_altitude_s51, mixed_efeitos_altitude_s52, mixed_efeitos_altitude_s53, mixed_efeitos_altitude_s54, mixed_efeitos_altitude_s55, mixed_efeitos_altitude_s56, mixed_efeitos_altitude_s57, mixed_efeitos_altitude_s58, mixed_efeitos_altitude_s59, mixed_efeitos_altitude_s60, mixed_efeitos_altitude_s61, mixed_efeitos_altitude_s62)

sort(selecao.altitudeefeitoAIC$AIC)


mixed_efeitos_altitude_s59 <- lmer(Altitude ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea), data = dadosefeitos_altitude, REML = TRUE)
summary(mixed_efeitos_altitude_s59)
Anova(mixed_efeitos_altitude_s59)



####Espessura####
rm(list = ls())
dadosefeitos<-read.csv("Dadosefeitos_ornit.csv", sep = ";")
dadosefeitos <- dadosefeitos[, !(names(dadosefeitos) %in% c("Expmacho", "Expfemea", "Casal", "Altura.total"))]


#transforming qualitative variables in factors
dadosefeitos$Ninho<-as.factor(dadosefeitos$Ninho)
dadosefeitos$Ambiente<-as.factor(dadosefeitos$Ambiente)
dadosefeitos$Territorio<-as.factor(dadosefeitos$Territorio)
dadosefeitos$Local<-as.factor(dadosefeitos$Local)
dadosefeitos$Femea<-as.factor(dadosefeitos$Femea)
dadosefeitos$Macho<-as.factor(dadosefeitos$Macho)
dadosefeitos$Destino<-as.factor(dadosefeitos$Destino)

#transforming qualitative variables in numeric
dadosefeitos$Ano<-as.numeric(dadosefeitos$Ano)
dadosefeitos$Altura<-as.numeric(dadosefeitos$Altura)
dadosefeitos$Altitude<-as.numeric(dadosefeitos$Altitude)
dadosefeitos$Espessura<-as.numeric(dadosefeitos$Espessura)
dadosefeitos$Idade_femea<-as.numeric(dadosefeitos$Idade_femea)
dadosefeitos$Idade_macho<-as.numeric(dadosefeitos$Idade_macho)

dadosefeitos_espessura <- dadosefeitos[, !(names(dadosefeitos) %in% c("Altura", "Altitude", "Esp"))]

dadosefeitos_espessura<-na.omit(dadosefeitos_espessura)

#N=105

#model construction
# Modelo Completo
mixed_efeitos_espessura <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)

# Modelos com 5 Variáveis Aleatórias
mixed_efeitos_espessura_s1 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s2 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s3 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s4 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s5 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s6 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)

# Modelos com 4 Variáveis Aleatórias
mixed_efeitos_espessura_s7 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano)  + (1 | Femea) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s8 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s9 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s10 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s11 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) +  (1 | Ambiente) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s12 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s13 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s14 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s15 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s16 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s17 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Macho) +  (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s18 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_femea) , data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s19 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s20 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s21 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) + (1 | Macho), data = dadosefeitos_espessura, REML = TRUE)

#3
mixed_efeitos_espessura_s22 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano)+ (1 | Macho) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s23 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s24 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Femea) , data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s25 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s26 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s27 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea)  + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s28 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s29 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + + (1 | Ambiente) + (1 | Femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s30 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s31 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s32 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s33 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s34 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s35 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s36 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s37 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s38 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s39 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s40 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s41 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)



#2
mixed_efeitos_espessura_s42 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Ambiente), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s43 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s44 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s45 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s46 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s47 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s48 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s49 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s50 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s51 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s52 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s53 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s54 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s55 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s56 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)

#1
mixed_efeitos_espessura_s57 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Local), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s58 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Ambiente), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s59 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s60 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Macho), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s61 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea), data = dadosefeitos_espessura, REML = TRUE)
mixed_efeitos_espessura_s62 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho), data = dadosefeitos_espessura, REML = TRUE)

mixed_efeitos_espessura_s63 <- lmer(Espessura ~ Destino + (1 | Territorio) + (1 | Ano), data = dadosefeitos_espessura, REML = TRUE)

selecao.espessuraefeitoAIC<-AIC(mixed_efeitos_espessura,mixed_efeitos_espessura_s1, mixed_efeitos_espessura_s2, mixed_efeitos_espessura_s3, mixed_efeitos_espessura_s4, mixed_efeitos_espessura_s5, mixed_efeitos_espessura_s6, mixed_efeitos_espessura_s7, mixed_efeitos_espessura_s8, mixed_efeitos_espessura_s9, mixed_efeitos_espessura_s10, mixed_efeitos_espessura_s11, mixed_efeitos_espessura_s12, mixed_efeitos_espessura_s13, mixed_efeitos_espessura_s14, mixed_efeitos_espessura_s15, mixed_efeitos_espessura_s16, mixed_efeitos_espessura_s17, mixed_efeitos_espessura_s18, mixed_efeitos_espessura_s19, mixed_efeitos_espessura_s20, mixed_efeitos_espessura_s21, mixed_efeitos_espessura_s22, mixed_efeitos_espessura_s23, mixed_efeitos_espessura_s24, mixed_efeitos_espessura_s25, mixed_efeitos_espessura_s26, mixed_efeitos_espessura_s27, mixed_efeitos_espessura_s28, mixed_efeitos_espessura_s29, mixed_efeitos_espessura_s30, mixed_efeitos_espessura_s31, mixed_efeitos_espessura_s32, mixed_efeitos_espessura_s33, mixed_efeitos_espessura_s34, mixed_efeitos_espessura_s35, mixed_efeitos_espessura_s36, mixed_efeitos_espessura_s37, mixed_efeitos_espessura_s38, mixed_efeitos_espessura_s39, mixed_efeitos_espessura_s40, mixed_efeitos_espessura_s41, mixed_efeitos_espessura_s42, mixed_efeitos_espessura_s43, mixed_efeitos_espessura_s44, mixed_efeitos_espessura_s45, mixed_efeitos_espessura_s46, mixed_efeitos_espessura_s47, mixed_efeitos_espessura_s48, mixed_efeitos_espessura_s49, mixed_efeitos_espessura_s50, mixed_efeitos_espessura_s51, mixed_efeitos_espessura_s52, mixed_efeitos_espessura_s53, mixed_efeitos_espessura_s54, mixed_efeitos_espessura_s55, mixed_efeitos_espessura_s56, mixed_efeitos_espessura_s57, mixed_efeitos_espessura_s58, mixed_efeitos_espessura_s59, mixed_efeitos_espessura_s60, mixed_efeitos_espessura_s61, mixed_efeitos_espessura_s62, mixed_efeitos_espessura_s63)

sort(selecao.espessuraefeitoAIC$AIC)
Anova(mixed_efeitos_espessura_s63)

summary(glht(mixed_efeitos_espessura_s63, linfct = mcp(Destino = "Predado - Alagado == 0")))
summary(glht(mixed_efeitos_espessura_s63, linfct = mcp(Destino = "Sucesso - Alagado == 0")))
summary(glht(mixed_efeitos_espessura_s63, linfct = mcp(Destino = "Tombado - Alagado == 0")))
summary(glht(mixed_efeitos_espessura_s63, linfct = mcp(Destino = "Predado - Sucesso == 0")))
summary(glht(mixed_efeitos_espessura_s63, linfct = mcp(Destino = "Tombado - Predado == 0")))
summary(glht(mixed_efeitos_espessura_s63, linfct = mcp(Destino = "Tombado - Sucesso == 0")))

####################################################################

########Análise 2 - MUDANÇA ALTURA ALAGADO#####
dadossucala<-read.csv("Dados_exatosonitsucala.csv", sep = ";")

dadossucala<-dadossucala[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altura", "Dest_N1", "Femea", "Macho")]

dadossucala <- dadossucala[, !(names(dadossucala) %in% c("Diferenca_altitude", "Diferenca_espessura", "Mudou_amb", "Distancia"))]

dadossucala<-na.omit(dadossucala)

#5
mixed_sucala <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)

#4
mixed_sucala_s1 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s2 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s3 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucala, REML = TRUE)
mixed_sucala_s4 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucala, REML = TRUE)
mixed_sucala_s5 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossucala, REML = TRUE)


#3
mixed_sucala_s6 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s7 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s8 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s9 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s10 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s11 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s12 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s12 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s13 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s14 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s15 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossucala, REML = TRUE)


#2 
mixed_sucala_s16 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s17 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s18 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s19 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s20 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s21 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s22 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s23 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s24 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s25 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucala, REML = TRUE)

#1
mixed_sucala_s26 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s27 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossucala, REML = TRUE)
mixed_sucala_s28 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s29 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s30 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossucala, REML = TRUE)

mixed_sucala_s31 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = TRUE)

mixed_sucala_AIC<-AIC(mixed_sucala, mixed_sucala_s1, mixed_sucala_s2, mixed_sucala_s3, mixed_sucala_s4, mixed_sucala_s5, mixed_sucala_s6, mixed_sucala_s7, mixed_sucala_s8, mixed_sucala_s9, mixed_sucala_s10, mixed_sucala_s11, mixed_sucala_s12, mixed_sucala_s13, mixed_sucala_s14, mixed_sucala_s15, mixed_sucala_s16, mixed_sucala_s17, mixed_sucala_s18, mixed_sucala_s19, mixed_sucala_s20, mixed_sucala_s21, mixed_sucala_s22, mixed_sucala_s23, mixed_sucala_s24, mixed_sucala_s25, mixed_sucala_s26, mixed_sucala_s27, mixed_sucala_s28, mixed_sucala_s29, mixed_sucala_s30, mixed_sucala_s31)


sort(mixed_sucala_AIC$AIC)
mixed_sucala_s32 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucala, REML = FALSE)
mixed_sucala_s33 <- lmer(Diferenca_altura ~ Dest_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucala, REML = FALSE)
mixed_sucala_s34 <- lmer(Diferenca_altura ~ Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucala, REML = FALSE)

mixed_sucala_AIC<-AIC(mixed_sucala_s32, mixed_sucala_s33, mixed_sucala_s34)

Anova(mixed_sucala_s33)

####Altitude####
dadossucala<-read.csv("Dados_exatosonitsucala.csv", sep = ";")

dadossucala<-dadossucala[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altitude", "Dest_N1", "Femea", "Macho")]

dadossucala <- dadossucala[, !(names(dadossucala) %in% c("Diferenca_altura", "Diferenca_espessura", "Mudou_amb", "Distancia"))]

dadossucala<-na.omit(dadossucala)

#5
mixed_sucala <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)

#4
mixed_sucala_s1 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s2 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s3 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucala, REML = TRUE)
mixed_sucala_s4 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucala, REML = TRUE)
mixed_sucala_s5 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossucala, REML = TRUE)


#3
mixed_sucala_s6 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s7 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s8 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s9 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s10 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s11 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s12 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s12 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s13 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s14 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s15 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossucala, REML = TRUE)


#2 
mixed_sucala_s16 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s17 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s18 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s19 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s20 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s21 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s22 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s23 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s24 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s25 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucala, REML = TRUE)

#1
mixed_sucala_s26 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s27 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossucala, REML = TRUE)
mixed_sucala_s28 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s29 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s30 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossucala, REML = TRUE)

mixed_sucala_s31 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = TRUE)

mixed_sucala_AIC<-AIC(mixed_sucala, mixed_sucala_s1, mixed_sucala_s2, mixed_sucala_s3, mixed_sucala_s4, mixed_sucala_s5, mixed_sucala_s6, mixed_sucala_s7, mixed_sucala_s8, mixed_sucala_s9, mixed_sucala_s10, mixed_sucala_s11, mixed_sucala_s12, mixed_sucala_s13, mixed_sucala_s14, mixed_sucala_s15, mixed_sucala_s16, mixed_sucala_s17, mixed_sucala_s18, mixed_sucala_s19, mixed_sucala_s20, mixed_sucala_s21, mixed_sucala_s22, mixed_sucala_s23, mixed_sucala_s24, mixed_sucala_s25, mixed_sucala_s26, mixed_sucala_s27, mixed_sucala_s28, mixed_sucala_s29, mixed_sucala_s30, mixed_sucala_s31)


sort(mixed_sucala_AIC$AIC)
mixed_sucala_s32 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = FALSE)
mixed_sucala_s33 <- lmer(Diferenca_altitude ~ Dest_N1  + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = FALSE)
mixed_sucala_s34 <- lmer(Diferenca_altitude ~  Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = FALSE)



mixed_sucala_AIC<-AIC(mixed_sucala_s32, mixed_sucala_s33, mixed_sucala_s34)

Anova(mixed_sucala_s33)

####Espessura####
dadossucala<-read.csv("Dados_exatosonitsucala.csv", sep = ";")

dadossucala<-dadossucala[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_espessura", "Dest_N1", "Femea", "Macho")]

dadossucala <- dadossucala[, !(names(dadossucala) %in% c("Diferenca_altitude", "Diferenca_altura", "Mudou_amb", "Distancia"))]

dadossucala<-na.omit(dadossucala)

#5
mixed_sucala <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)

#4
mixed_sucala_s1 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s2 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s3 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucala, REML = TRUE)
mixed_sucala_s4 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucala, REML = TRUE)
mixed_sucala_s5 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossucala, REML = TRUE)


#3
mixed_sucala_s6 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s7 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s8 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s9 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s10 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s11 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s12 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s12 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s13 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s14 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s15 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossucala, REML = TRUE)


#2 
mixed_sucala_s16 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s17 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s18 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s19 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s20 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s21 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s22 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s23 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s24 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s25 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucala, REML = TRUE)

#1
mixed_sucala_s26 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s27 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossucala, REML = TRUE)
mixed_sucala_s28 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s29 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s30 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossucala, REML = TRUE)

mixed_sucala_s31 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = TRUE)

mixed_sucala_AIC<-AIC(mixed_sucala, mixed_sucala_s1, mixed_sucala_s2, mixed_sucala_s3, mixed_sucala_s4, mixed_sucala_s5, mixed_sucala_s6, mixed_sucala_s7, mixed_sucala_s8, mixed_sucala_s9, mixed_sucala_s10, mixed_sucala_s11, mixed_sucala_s12, mixed_sucala_s13, mixed_sucala_s14, mixed_sucala_s15, mixed_sucala_s16, mixed_sucala_s17, mixed_sucala_s18, mixed_sucala_s19, mixed_sucala_s20, mixed_sucala_s21, mixed_sucala_s22, mixed_sucala_s23, mixed_sucala_s24, mixed_sucala_s25, mixed_sucala_s26, mixed_sucala_s27, mixed_sucala_s28, mixed_sucala_s29, mixed_sucala_s30, mixed_sucala_s31)


sort(mixed_sucala_AIC$AIC)

mixed_sucala_s32 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = FALSE)
mixed_sucala_s33 <- lmer(Diferenca_espessura ~ Dest_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = FALSE)
mixed_sucala_s34 <- lmer(Diferenca_espessura ~  Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = FALSE)

mixed_sucala_AIC<-AIC(mixed_sucala_s32, mixed_sucala_s33, mixed_sucala_s34)

Anova(mixed_sucala_s33)


#####Distancia####
dadossucala<-read.csv("Dados_exatosonitsucala.csv", sep = ";")

dadossucala<-dadossucala[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Distancia", "Dest_N1", "Femea", "Macho")]

dadossucala <- dadossucala[, !(names(dadossucala) %in% c("Diferenca_altitude", "Diferenca_espessura", "Mudou_amb", "Diferenca_altura"))]

dadossucala<-na.omit(dadossucala)

#5
mixed_sucala <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)

#4
mixed_sucala_s1 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s2 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s3 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucala, REML = TRUE)
mixed_sucala_s4 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucala, REML = TRUE)
mixed_sucala_s5 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossucala, REML = TRUE)


#3
mixed_sucala_s6 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s7 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s8 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s9 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s10 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s11 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s12 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s12 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s13 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s14 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s15 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossucala, REML = TRUE)


#2 
mixed_sucala_s16 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s17 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s18 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s19 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s20 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s21 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s22 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s23 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s24 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossucala, REML = TRUE)
mixed_sucala_s25 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucala, REML = TRUE)

#1
mixed_sucala_s26 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucala, REML = TRUE)
mixed_sucala_s27 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossucala, REML = TRUE)
mixed_sucala_s28 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossucala, REML = TRUE)
mixed_sucala_s29 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucala, REML = TRUE)
mixed_sucala_s30 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossucala, REML = TRUE)

mixed_sucala_s31 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = TRUE)

mixed_sucala_AIC<-AIC(mixed_sucala, mixed_sucala_s1, mixed_sucala_s2, mixed_sucala_s3, mixed_sucala_s4, mixed_sucala_s5, mixed_sucala_s6, mixed_sucala_s7, mixed_sucala_s8, mixed_sucala_s9, mixed_sucala_s10, mixed_sucala_s11, mixed_sucala_s12, mixed_sucala_s13, mixed_sucala_s14, mixed_sucala_s15, mixed_sucala_s16, mixed_sucala_s17, mixed_sucala_s18, mixed_sucala_s19, mixed_sucala_s20, mixed_sucala_s21, mixed_sucala_s22, mixed_sucala_s23, mixed_sucala_s24, mixed_sucala_s25, mixed_sucala_s26, mixed_sucala_s27, mixed_sucala_s28, mixed_sucala_s29, mixed_sucala_s30, mixed_sucala_s31)


sort(mixed_sucala_AIC$AIC)
mixed_sucala_s32 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = FALSE)
mixed_sucala_s33 <- lmer(Distancia ~ Dest_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = FALSE)
mixed_sucala_s34 <- lmer(Distancia ~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, REML = FALSE)

mixed_sucala_AIC<-AIC(mixed_sucala_s32, mixed_sucala_s33, mixed_sucala_s34)

Anova(mixed_sucala_s33)

####Ambiente####
dadossucala<-read.csv("Dados_exatosonitsucala.csv", sep = ";")

dadossucala<-dadossucala[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Mudou_amb", "Dest_N1", "Femea", "Macho")]

dadossucala <- dadossucala[, !(names(dadossucala) %in% c("Diferenca_altitude", "Diferenca_espessura", "Distancia", "Diferenca_altura"))]

dadossucala$Mudou_amb<-as.factor(dadossucala$Mudou_amb)

dadossucala<-na.omit(dadossucala)

#5
mixed_sucala <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucala, family = binomial)

#4
mixed_sucala_s1 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossucala, family = binomial)
mixed_sucala_s2 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossucala,  family = binomial)
mixed_sucala_s3 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucala, family = binomial)
mixed_sucala_s4 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucala, family = binomial)
mixed_sucala_s5 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossucala,  family = binomial)


#3
mixed_sucala_s6 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossucala, family = binomial)
mixed_sucala_s7 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossucala,  family = binomial)
mixed_sucala_s8 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossucala,  family = binomial)
mixed_sucala_s9 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucala, family = binomial)
mixed_sucala_s10 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossucala,  family = binomial)
mixed_sucala_s11 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossucala,  family = binomial)
mixed_sucala_s12 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossucala,  family = binomial)
mixed_sucala_s12 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucala,  family = binomial)
mixed_sucala_s13 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucala,  family = binomial)
mixed_sucala_s14 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucala, family = binomial)
mixed_sucala_s15 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossucala,  family = binomial)


#2 
mixed_sucala_s16 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossucala,  family = binomial)
mixed_sucala_s17 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossucala,  family = binomial)
mixed_sucala_s18 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossucala, family = binomial)
mixed_sucala_s19 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossucala,  family = binomial)
mixed_sucala_s20 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossucala,  family = binomial)
mixed_sucala_s21 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossucala,family = binomial)
mixed_sucala_s22 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossucala,  family = binomial)
mixed_sucala_s23 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossucala,  family = binomial)
mixed_sucala_s24 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossucala,  family = binomial)
mixed_sucala_s25 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucala, family = binomial)

#1
mixed_sucala_s26 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucala,  family = binomial)
mixed_sucala_s27 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossucala,  family = binomial)
mixed_sucala_s28 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossucala,  family = binomial)
mixed_sucala_s29 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucala,  family = binomial)
mixed_sucala_s30 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossucala,  family = binomial)

mixed_sucala_s31 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, family = binomial)

mixed_sucala_AIC<-AIC(mixed_sucala, mixed_sucala_s1, mixed_sucala_s2, mixed_sucala_s3, mixed_sucala_s5, mixed_sucala_s6, mixed_sucala_s7, mixed_sucala_s8, mixed_sucala_s9, mixed_sucala_s10, mixed_sucala_s11, mixed_sucala_s12, mixed_sucala_s13, mixed_sucala_s14, mixed_sucala_s15, mixed_sucala_s16, mixed_sucala_s17, mixed_sucala_s18, mixed_sucala_s19, mixed_sucala_s20, mixed_sucala_s21, mixed_sucala_s23, mixed_sucala_s24, mixed_sucala_s25, mixed_sucala_s26, mixed_sucala_s27, mixed_sucala_s28, mixed_sucala_s29, mixed_sucala_s30, mixed_sucala_s31)


sort(mixed_sucala_AIC$AIC)

mixed_sucala_s32 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, family = binomial)
mixed_sucala_s33 <- glmer(Mudou_amb~ Dest_N1+ (1 | Territorio) + (1 | Ano), data = dadossucala, family = binomial)
mixed_sucala_s34 <- glmer(Mudou_amb~  Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucala, family = binomial)

mixed_sucala_AIC<-AIC(mixed_sucala_s32, mixed_sucala_s33, mixed_sucala_s34)

Anova(mixed_sucala_s33)


#####PREDADOS####
dadossucpre<-read.csv("Dados_exatosonitsucpre.csv", sep = ";")

dadossucpre<-dadossucpre[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altura", "Dest_N1", "Femea", "Macho")]

dadossucpre <- dadossucpre[, !(names(dadossucpre) %in% c("Diferenca_altitude", "Diferenca_espessura", "Mudou_amb", "Distancia"))]

dadossucpre<-na.omit(dadossucpre)

#5
mixed_sucpre <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)

#4
mixed_sucpre_s1 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s2 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s3 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s4 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s5 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossucpre, REML = TRUE)


#3
mixed_sucpre_s6 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s7 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s8 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s9 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s10 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s11 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s12 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s12 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s13 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s14 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s15 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossucpre, REML = TRUE)


#2 
mixed_sucpre_s16 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s17 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s18 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s19 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s20 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s21 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s22 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s23 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s24 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s25 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)

#1
mixed_sucpre_s26 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s27 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s28 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s29 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s30 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossucpre, REML = TRUE)

mixed_sucpre_s31 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, REML = TRUE)

mixed_sucpre_AIC<-AIC(mixed_sucpre, mixed_sucpre_s1, mixed_sucpre_s2, mixed_sucpre_s3, mixed_sucpre_s4, mixed_sucpre_s5, mixed_sucpre_s6, mixed_sucpre_s7, mixed_sucpre_s8, mixed_sucpre_s9, mixed_sucpre_s10, mixed_sucpre_s11, mixed_sucpre_s12, mixed_sucpre_s13, mixed_sucpre_s14, mixed_sucpre_s15, mixed_sucpre_s16, mixed_sucpre_s17, mixed_sucpre_s18, mixed_sucpre_s19, mixed_sucpre_s20, mixed_sucpre_s21, mixed_sucpre_s22, mixed_sucpre_s23, mixed_sucpre_s24, mixed_sucpre_s25, mixed_sucpre_s26, mixed_sucpre_s27, mixed_sucpre_s28, mixed_sucpre_s29, mixed_sucpre_s30, mixed_sucpre_s31)


sort(mixed_sucpre_AIC$AIC)
mixed_sucpre_s32 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, REML = FALSE)
mixed_sucpre_s33 <- lmer(Diferenca_altura ~ Dest_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, REML = FALSE)

mixed_sucpre_AIC<-AIC(mixed_sucpre_s32, mixed_sucpre_s33)

Anova(mixed_sucpre_s32)


####Altitude####
dadossucpre<-read.csv("Dados_exatosonitsucpre.csv", sep = ";")

dadossucpre<-dadossucpre[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altitude", "Dest_N1", "Femea", "Macho")]

dadossucpre <- dadossucpre[, !(names(dadossucpre) %in% c("Diferenca_altura", "Diferenca_espessura", "Mudou_amb", "Distancia"))]

dadossucpre<-na.omit(dadossucpre)

#5
mixed_sucpre <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)

#4
mixed_sucpre_s1 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s2 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s3 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s4 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s5 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossucpre, REML = TRUE)


#3
mixed_sucpre_s6 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s7 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s8 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s9 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s10 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s11 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s12 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s12 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s13 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s14 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s15 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossucpre, REML = TRUE)


#2 
mixed_sucpre_s16 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s17 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s18 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s19 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s20 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s21 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s22 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s23 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s24 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s25 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)

#1
mixed_sucpre_s26 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s27 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s28 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s29 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s30 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossucpre, REML = TRUE)

mixed_sucpre_s31 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, REML = TRUE)

mixed_sucpre_AIC<-AIC(mixed_sucpre, mixed_sucpre_s1, mixed_sucpre_s2, mixed_sucpre_s3, mixed_sucpre_s4, mixed_sucpre_s5, mixed_sucpre_s6, mixed_sucpre_s7, mixed_sucpre_s8, mixed_sucpre_s9, mixed_sucpre_s10, mixed_sucpre_s11, mixed_sucpre_s12, mixed_sucpre_s13, mixed_sucpre_s14, mixed_sucpre_s15, mixed_sucpre_s16, mixed_sucpre_s17, mixed_sucpre_s18, mixed_sucpre_s19, mixed_sucpre_s20, mixed_sucpre_s21, mixed_sucpre_s22, mixed_sucpre_s23, mixed_sucpre_s24, mixed_sucpre_s25, mixed_sucpre_s26, mixed_sucpre_s27, mixed_sucpre_s28, mixed_sucpre_s29, mixed_sucpre_s30, mixed_sucpre_s31)


sort(mixed_sucpre_AIC$AIC)
mixed_sucpre_s32 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucpre, REML = FALSE)
mixed_sucpre_s33 <- lmer(Diferenca_altitude ~ Dest_N1  + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucpre, REML = FALSE)
mixed_sucpre_s34 <- lmer(Diferenca_altitude ~  Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucpre, REML = FALSE)



mixed_sucpre_AIC<-AIC(mixed_sucpre_s32, mixed_sucpre_s33, mixed_sucpre_s34)

Anova(mixed_sucpre_s33)



mixed_sucpre_s33 <- lmer(Diferenca_altitude ~ Dest_N1  + Idade_femea+ (1 | Territorio) + (1 | Ano) , data = dadossucpre, REML = FALSE)


####Espessura####
dadossucpre<-read.csv("Dados_exatosonitsucpre.csv", sep = ";")

dadossucpre<-dadossucpre[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_espessura", "Dest_N1", "Femea", "Macho")]

dadossucpre <- dadossucpre[, !(names(dadossucpre) %in% c("Diferenca_altitude", "Diferenca_altura", "Mudou_amb", "Distancia"))]

dadossucpre<-na.omit(dadossucpre)

#5
mixed_sucpre <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)

#4
mixed_sucpre_s1 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s2 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s3 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s4 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s5 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossucpre, REML = TRUE)


#3
mixed_sucpre_s6 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s7 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s8 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s9 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s10 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s11 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s12 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s12 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s13 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s14 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s15 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossucpre, REML = TRUE)


#2 
mixed_sucpre_s16 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s17 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s18 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s19 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s20 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s21 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s22 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s23 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s24 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s25 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)

#1
mixed_sucpre_s26 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s27 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s28 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s29 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s30 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossucpre, REML = TRUE)

mixed_sucpre_s31 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, REML = TRUE)

mixed_sucpre_AIC<-AIC(mixed_sucpre, mixed_sucpre_s1, mixed_sucpre_s2, mixed_sucpre_s3, mixed_sucpre_s4, mixed_sucpre_s5, mixed_sucpre_s6, mixed_sucpre_s7, mixed_sucpre_s8, mixed_sucpre_s9, mixed_sucpre_s10, mixed_sucpre_s11, mixed_sucpre_s12, mixed_sucpre_s13, mixed_sucpre_s14, mixed_sucpre_s15, mixed_sucpre_s16, mixed_sucpre_s17, mixed_sucpre_s18, mixed_sucpre_s19, mixed_sucpre_s20, mixed_sucpre_s21, mixed_sucpre_s22, mixed_sucpre_s23, mixed_sucpre_s24, mixed_sucpre_s25, mixed_sucpre_s26, mixed_sucpre_s27, mixed_sucpre_s28, mixed_sucpre_s29, mixed_sucpre_s30, mixed_sucpre_s31)


sort(mixed_sucpre_AIC$AIC)

mixed_sucpre_s32 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, REML = FALSE)
mixed_sucpre_s33 <- lmer(Diferenca_espessura ~ Dest_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, REML = FALSE)
mixed_sucpre_s34 <- lmer(Diferenca_espessura ~  Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, REML = FALSE)

mixed_sucpre_AIC<-AIC(mixed_sucpre_s32, mixed_sucpre_s33, mixed_sucpre_s34)

Anova(mixed_sucpre_s33)


#####Distancia####
dadossucpre<-read.csv("Dados_exatosonitsucpre.csv", sep = ";")

dadossucpre<-dadossucpre[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Distancia", "Dest_N1", "Femea", "Macho")]

dadossucpre <- dadossucpre[, !(names(dadossucpre) %in% c("Diferenca_altitude", "Diferenca_espessura", "Mudou_amb", "Diferenca_altura"))]

dadossucpre<-na.omit(dadossucpre)

#5
mixed_sucpre <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)

#4
mixed_sucpre_s1 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s2 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s3 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s4 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s5 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossucpre, REML = TRUE)


#3
mixed_sucpre_s6 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s7 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s8 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s9 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s10 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s11 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s12 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s12 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s13 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s14 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s15 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossucpre, REML = TRUE)


#2 
mixed_sucpre_s16 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s17 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s18 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s19 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s20 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s21 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s22 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s23 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s24 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s25 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucpre, REML = TRUE)

#1
mixed_sucpre_s26 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucpre, REML = TRUE)
mixed_sucpre_s27 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s28 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossucpre, REML = TRUE)
mixed_sucpre_s29 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucpre, REML = TRUE)
mixed_sucpre_s30 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossucpre, REML = TRUE)

mixed_sucpre_s31 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, REML = TRUE)

mixed_sucpre_AIC<-AIC(mixed_sucpre, mixed_sucpre_s1, mixed_sucpre_s2, mixed_sucpre_s3, mixed_sucpre_s4, mixed_sucpre_s5, mixed_sucpre_s6, mixed_sucpre_s7, mixed_sucpre_s8, mixed_sucpre_s9, mixed_sucpre_s10, mixed_sucpre_s11, mixed_sucpre_s12, mixed_sucpre_s13, mixed_sucpre_s14, mixed_sucpre_s15, mixed_sucpre_s16, mixed_sucpre_s17, mixed_sucpre_s18, mixed_sucpre_s19, mixed_sucpre_s20, mixed_sucpre_s21, mixed_sucpre_s22, mixed_sucpre_s23, mixed_sucpre_s24, mixed_sucpre_s25, mixed_sucpre_s26, mixed_sucpre_s27, mixed_sucpre_s28, mixed_sucpre_s29, mixed_sucpre_s30, mixed_sucpre_s31)


sort(mixed_sucpre_AIC$AIC)
mixed_sucpre_s32 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, REML = FALSE)
mixed_sucpre_s33 <- lmer(Distancia ~ Dest_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, REML = FALSE)
mixed_sucpre_s34 <- lmer(Distancia ~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, REML = FALSE)

mixed_sucpre_AIC<-AIC(mixed_sucpre_s32, mixed_sucpre_s33, mixed_sucpre_s34)

Anova(mixed_sucpre_s33)

#####Ambiente####
dadossucpre<-read.csv("Dados_exatosonitsucpre.csv", sep = ";")

dadossucpre<-dadossucpre[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Mudou_amb", "Dest_N1", "Femea", "Macho")]

dadossucpre <- dadossucpre[, !(names(dadossucpre) %in% c("Diferenca_altitude", "Diferenca_espessura", "Distancia", "Diferenca_altura"))]

dadossucpre$Mudou_amb<-as.factor(dadossucpre$Mudou_amb)

dadossucpre<-na.omit(dadossucpre)

#5
mixed_sucpre <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucpre, family = binomial)

#4
mixed_sucpre_s1 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossucpre, family = binomial)
mixed_sucpre_s2 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossucpre,  family = binomial)
mixed_sucpre_s3 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucpre, family = binomial)
mixed_sucpre_s4 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossucpre, family = binomial)
mixed_sucpre_s5 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossucpre,  family = binomial)


#3
mixed_sucpre_s6 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossucpre, family = binomial)
mixed_sucpre_s7 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossucpre,  family = binomial)
mixed_sucpre_s8 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossucpre,  family = binomial)
mixed_sucpre_s9 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossucpre, family = binomial)
mixed_sucpre_s10 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossucpre,  family = binomial)
mixed_sucpre_s11 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossucpre,  family = binomial)
mixed_sucpre_s12 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossucpre,  family = binomial)
mixed_sucpre_s12 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre,  family = binomial)
mixed_sucpre_s13 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre,  family = binomial)
mixed_sucpre_s14 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucpre, family = binomial)
mixed_sucpre_s15 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossucpre,  family = binomial)


#2 
mixed_sucpre_s16 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossucpre,  family = binomial)
mixed_sucpre_s17 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossucpre,  family = binomial)
mixed_sucpre_s18 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossucpre, family = binomial)
mixed_sucpre_s19 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossucpre,  family = binomial)
mixed_sucpre_s20 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossucpre,  family = binomial)
mixed_sucpre_s21 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossucpre,family = binomial)
mixed_sucpre_s22 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossucpre,  family = binomial)
mixed_sucpre_s23 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossucpre,  family = binomial)
mixed_sucpre_s24 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossucpre,  family = binomial)
mixed_sucpre_s25 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossucpre, family = binomial)

#1
mixed_sucpre_s26 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossucpre,  family = binomial)
mixed_sucpre_s27 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossucpre,  family = binomial)
mixed_sucpre_s28 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossucpre,  family = binomial)
mixed_sucpre_s29 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossucpre,  family = binomial)
mixed_sucpre_s30 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossucpre,  family = binomial)

mixed_sucpre_s31 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, family = binomial)

mixed_sucpre_AIC<-AIC(mixed_sucpre, mixed_sucpre_s1, mixed_sucpre_s2, mixed_sucpre_s3,mixed_sucpre_s4, mixed_sucpre_s5, mixed_sucpre_s6, mixed_sucpre_s7, mixed_sucpre_s8, mixed_sucpre_s9, mixed_sucpre_s10, mixed_sucpre_s11, mixed_sucpre_s12, mixed_sucpre_s13, mixed_sucpre_s14, mixed_sucpre_s15, mixed_sucpre_s16, mixed_sucpre_s17, mixed_sucpre_s18, mixed_sucpre_s19, mixed_sucpre_s20, mixed_sucpre_s21, mixed_sucpre_s22, mixed_sucpre_s23, mixed_sucpre_s24, mixed_sucpre_s25, mixed_sucpre_s26, mixed_sucpre_s27, mixed_sucpre_s28, mixed_sucpre_s29, mixed_sucpre_s30, mixed_sucpre_s31)


sort(mixed_sucpre_AIC$AIC)

mixed_sucpre_s32 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, family = binomial)
mixed_sucpre_s33 <- glmer(Mudou_amb~ Dest_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, family = binomial)
mixed_sucpre_s34 <- glmer(Mudou_amb~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossucpre, family = binomial)

mixed_sucpre_AIC<-AIC(mixed_sucpre_s32,mixed_sucpre_s33,mixed_sucpre_s34)

Anova(mixed_sucpre_s33)

####TOMBADOS####
dadossuctom<-read.csv("Dados_exatosonitsuctom.csv", sep = ";")

dadossuctom<-dadossuctom[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altura", "Dest_N1", "Femea", "Macho")]

dadossuctom <- dadossuctom[, !(names(dadossuctom) %in% c("Diferenca_altitude", "Diferenca_espessura", "Mudou_amb", "Distancia"))]

dadossuctom<-na.omit(dadossuctom)

#5
mixed_suctom <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)

#4
mixed_suctom_s1 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s2 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s3 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s4 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s5 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossuctom, REML = TRUE)


#3
mixed_suctom_s6 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s7 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s8 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s9 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s10 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s11 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s12 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s12 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s13 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s14 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s15 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossuctom, REML = TRUE)


#2 
mixed_suctom_s16 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s17 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s18 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s19 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s20 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s21 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s22 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s23 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s24 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s25 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)

#1
mixed_suctom_s26 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s27 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s28 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s29 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s30 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossuctom, REML = TRUE)

mixed_suctom_s31 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = TRUE)

mixed_suctom_AIC<-AIC(mixed_suctom, mixed_suctom_s1, mixed_suctom_s2, mixed_suctom_s3, mixed_suctom_s4, mixed_suctom_s5, mixed_suctom_s6, mixed_suctom_s7, mixed_suctom_s8, mixed_suctom_s9, mixed_suctom_s10, mixed_suctom_s11, mixed_suctom_s12, mixed_suctom_s13, mixed_suctom_s14, mixed_suctom_s15, mixed_suctom_s16, mixed_suctom_s17, mixed_suctom_s18, mixed_suctom_s19, mixed_suctom_s20, mixed_suctom_s21, mixed_suctom_s22, mixed_suctom_s23, mixed_suctom_s24, mixed_suctom_s25, mixed_suctom_s26, mixed_suctom_s27, mixed_suctom_s28, mixed_suctom_s29, mixed_suctom_s30, mixed_suctom_s31)


sort(mixed_suctom_AIC$AIC)
mixed_suctom_s32 <- lmer(Diferenca_altura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = FALSE)
mixed_suctom_s33 <- lmer(Diferenca_altura ~ Dest_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = FALSE)
mixed_suctom_s34 <- lmer(Diferenca_altura ~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = FALSE)

mixed_suctom_AIC<-AIC(mixed_suctom_s32, mixed_suctom_s33, mixed_suctom_s34)

Anova(mixed_suctom_s33)

####Altitude####
dadossuctom<-read.csv("Dados_exatosonitsuctom.csv", sep = ";")

dadossuctom<-dadossuctom[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altitude", "Dest_N1", "Femea", "Macho")]

dadossuctom <- dadossuctom[, !(names(dadossuctom) %in% c("Diferenca_altura", "Diferenca_espessura", "Mudou_amb", "Distancia"))]

dadossuctom<-na.omit(dadossuctom)

#5
mixed_suctom <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)

#4
mixed_suctom_s1 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s2 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s3 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s4 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s5 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossuctom, REML = TRUE)


#3
mixed_suctom_s6 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s7 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s8 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s9 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s10 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s11 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s12 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s12 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s13 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s14 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s15 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossuctom, REML = TRUE)


#2 
mixed_suctom_s16 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s17 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s18 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s19 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s20 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s21 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s22 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s23 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s24 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s25 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)

#1
mixed_suctom_s26 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s27 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s28 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s29 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s30 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossuctom, REML = TRUE)

mixed_suctom_s31 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = TRUE)

mixed_suctom_AIC<-AIC(mixed_suctom, mixed_suctom_s1, mixed_suctom_s2, mixed_suctom_s3, mixed_suctom_s4, mixed_suctom_s5, mixed_suctom_s6, mixed_suctom_s7, mixed_suctom_s8, mixed_suctom_s9, mixed_suctom_s10, mixed_suctom_s11, mixed_suctom_s12, mixed_suctom_s13, mixed_suctom_s14, mixed_suctom_s15, mixed_suctom_s16, mixed_suctom_s17, mixed_suctom_s18, mixed_suctom_s19, mixed_suctom_s20, mixed_suctom_s21, mixed_suctom_s22, mixed_suctom_s23, mixed_suctom_s24, mixed_suctom_s25, mixed_suctom_s26, mixed_suctom_s27, mixed_suctom_s28, mixed_suctom_s29, mixed_suctom_s30, mixed_suctom_s31)


sort(mixed_suctom_AIC$AIC)
mixed_suctom_s32 <- lmer(Diferenca_altitude ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = FALSE)
mixed_suctom_s33 <- lmer(Diferenca_altitude ~ Dest_N1  + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = FALSE)
mixed_suctom_s34 <- lmer(Diferenca_altitude ~  Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = FALSE)



mixed_suctom_AIC<-AIC(mixed_suctom_s32, mixed_suctom_s33, mixed_suctom_s34)

Anova(mixed_suctom_s32)

####Espessura####
dadossuctom<-read.csv("Dados_exatosonitsuctom.csv", sep = ";")

dadossuctom<-dadossuctom[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_espessura", "Dest_N1", "Femea", "Macho")]

dadossuctom <- dadossuctom[, !(names(dadossuctom) %in% c("Diferenca_altitude", "Diferenca_altura", "Mudou_amb", "Distancia"))]

dadossuctom<-na.omit(dadossuctom)

#5
mixed_suctom <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)

#4
mixed_suctom_s1 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s2 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s3 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s4 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s5 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossuctom, REML = TRUE)


#3
mixed_suctom_s6 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s7 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s8 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s9 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s10 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s11 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s12 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s12 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s13 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s14 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s15 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossuctom, REML = TRUE)


#2 
mixed_suctom_s16 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s17 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s18 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s19 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s20 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s21 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s22 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s23 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s24 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s25 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)

#1
mixed_suctom_s26 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s27 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s28 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s29 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s30 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossuctom, REML = TRUE)

mixed_suctom_s31 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = TRUE)

mixed_suctom_AIC<-AIC(mixed_suctom, mixed_suctom_s1, mixed_suctom_s2, mixed_suctom_s3, mixed_suctom_s4, mixed_suctom_s5, mixed_suctom_s6, mixed_suctom_s7, mixed_suctom_s8, mixed_suctom_s9, mixed_suctom_s10, mixed_suctom_s11, mixed_suctom_s12, mixed_suctom_s13, mixed_suctom_s14, mixed_suctom_s15, mixed_suctom_s16, mixed_suctom_s17, mixed_suctom_s18, mixed_suctom_s19, mixed_suctom_s20, mixed_suctom_s21, mixed_suctom_s22, mixed_suctom_s23, mixed_suctom_s24, mixed_suctom_s25, mixed_suctom_s26, mixed_suctom_s27, mixed_suctom_s28, mixed_suctom_s29, mixed_suctom_s30, mixed_suctom_s31)


sort(mixed_suctom_AIC$AIC)

mixed_suctom_s32 <- lmer(Diferenca_espessura ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = FALSE)
mixed_suctom_s33 <- lmer(Diferenca_espessura ~ Dest_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = FALSE)
mixed_suctom_s34 <- lmer(Diferenca_espessura ~  Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = FALSE)

mixed_suctom_AIC<-AIC(mixed_suctom_s32, mixed_suctom_s33, mixed_suctom_s34)

Anova(mixed_suctom_s33)


#####Distancia####
dadossuctom<-read.csv("Dados_exatosonitsuctom.csv", sep = ";")

dadossuctom<-dadossuctom[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Distancia", "Dest_N1", "Femea", "Macho")]

dadossuctom <- dadossuctom[, !(names(dadossuctom) %in% c("Diferenca_altitude", "Diferenca_espessura", "Mudou_amb", "Diferenca_altura"))]

dadossuctom<-na.omit(dadossuctom)

#5
mixed_suctom <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)

#4
mixed_suctom_s1 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s2 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s3 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s4 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s5 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossuctom, REML = TRUE)


#3
mixed_suctom_s6 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s7 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s8 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s9 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s10 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s11 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s12 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s12 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s13 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s14 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s15 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossuctom, REML = TRUE)


#2 
mixed_suctom_s16 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s17 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s18 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s19 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s20 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s21 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s22 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s23 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s24 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s25 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossuctom, REML = TRUE)

#1
mixed_suctom_s26 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossuctom, REML = TRUE)
mixed_suctom_s27 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s28 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossuctom, REML = TRUE)
mixed_suctom_s29 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossuctom, REML = TRUE)
mixed_suctom_s30 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossuctom, REML = TRUE)

mixed_suctom_s31 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = TRUE)

mixed_suctom_AIC<-AIC(mixed_suctom, mixed_suctom_s1, mixed_suctom_s2, mixed_suctom_s3, mixed_suctom_s4, mixed_suctom_s5, mixed_suctom_s6, mixed_suctom_s7, mixed_suctom_s8, mixed_suctom_s9, mixed_suctom_s10, mixed_suctom_s11, mixed_suctom_s12, mixed_suctom_s13, mixed_suctom_s14, mixed_suctom_s15, mixed_suctom_s16, mixed_suctom_s17, mixed_suctom_s18, mixed_suctom_s19, mixed_suctom_s20, mixed_suctom_s21, mixed_suctom_s22, mixed_suctom_s23, mixed_suctom_s24, mixed_suctom_s25, mixed_suctom_s26, mixed_suctom_s27, mixed_suctom_s28, mixed_suctom_s29, mixed_suctom_s30, mixed_suctom_s31)


sort(mixed_suctom_AIC$AIC)
mixed_suctom_s32 <- lmer(Distancia ~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = FALSE)
mixed_suctom_s33 <- lmer(Distancia ~ Dest_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = FALSE)
mixed_suctom_s34 <- lmer(Distancia ~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, REML = FALSE)

mixed_suctom_AIC<-AIC(mixed_suctom_s32, mixed_suctom_s33, mixed_suctom_s34)

Anova(mixed_suctom_s33)

#####Ambiente####
dadossuctom<-read.csv("Dados_exatosonitsuctom.csv", sep = ";")

dadossuctom<-dadossuctom[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Mudou_amb", "Dest_N1", "Femea", "Macho")]

dadossuctom <- dadossuctom[, !(names(dadossuctom) %in% c("Diferenca_altitude", "Diferenca_espessura", "Distancia", "Diferenca_altura"))]

dadossuctom$Mudou_amb<-as.factor(dadossuctom$Mudou_amb)

dadossuctom<-na.omit(dadossuctom)

#5
mixed_suctom <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossuctom, family = binomial)

#4
mixed_suctom_s1 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadossuctom, family = binomial)
mixed_suctom_s2 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadossuctom,  family = binomial)
mixed_suctom_s3 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossuctom, family = binomial)
mixed_suctom_s4 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadossuctom, family = binomial)
mixed_suctom_s5 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadossuctom,  family = binomial)


#3
mixed_suctom_s6 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadossuctom, family = binomial)
mixed_suctom_s7 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadossuctom,  family = binomial)
mixed_suctom_s8 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadossuctom,  family = binomial)
mixed_suctom_s9 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadossuctom, family = binomial)
mixed_suctom_s10 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadossuctom,  family = binomial)
mixed_suctom_s11 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadossuctom,  family = binomial)
mixed_suctom_s12 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadossuctom,  family = binomial)
mixed_suctom_s12 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom,  family = binomial)
mixed_suctom_s13 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom,  family = binomial)
mixed_suctom_s14 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossuctom, family = binomial)
mixed_suctom_s15 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadossuctom,  family = binomial)


#2 
mixed_suctom_s16 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadossuctom,  family = binomial)
mixed_suctom_s17 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadossuctom,  family = binomial)
mixed_suctom_s18 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadossuctom, family = binomial)
mixed_suctom_s19 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadossuctom,  family = binomial)
mixed_suctom_s20 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadossuctom,  family = binomial)
mixed_suctom_s21 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadossuctom,family = binomial)
mixed_suctom_s22 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadossuctom,  family = binomial)
mixed_suctom_s23 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadossuctom,  family = binomial)
mixed_suctom_s24 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadossuctom,  family = binomial)
mixed_suctom_s25 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadossuctom, family = binomial)

#1
mixed_suctom_s26 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadossuctom,  family = binomial)
mixed_suctom_s27 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadossuctom,  family = binomial)
mixed_suctom_s28 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadossuctom,  family = binomial)
mixed_suctom_s29 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadossuctom,  family = binomial)
mixed_suctom_s30 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadossuctom,  family = binomial)

mixed_suctom_s31 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, family = binomial)

mixed_suctom_AIC<-AIC(mixed_suctom, mixed_suctom_s1, mixed_suctom_s2, mixed_suctom_s3, mixed_suctom_s5, mixed_suctom_s6, mixed_suctom_s7, mixed_suctom_s8, mixed_suctom_s9, mixed_suctom_s10, mixed_suctom_s11, mixed_suctom_s12, mixed_suctom_s13, mixed_suctom_s14, mixed_suctom_s15, mixed_suctom_s16, mixed_suctom_s17, mixed_suctom_s18, mixed_suctom_s19, mixed_suctom_s20, mixed_suctom_s21, mixed_suctom_s23, mixed_suctom_s24, mixed_suctom_s25, mixed_suctom_s26, mixed_suctom_s27, mixed_suctom_s28, mixed_suctom_s29, mixed_suctom_s30, mixed_suctom_s31)


sort(mixed_suctom_AIC$AIC)
mixed_suctom_s32 <- glmer(Mudou_amb~ Dest_N1 + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, family = binomial)
mixed_suctom_s33 <- glmer(Mudou_amb~ Dest_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, family = binomial)
mixed_suctom_s34 <- glmer(Mudou_amb~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadossuctom, family = binomial)

mixed_suctom_AIC<-AIC(mixed_suctom_s32, mixed_suctom_s33, mixed_suctom_s34)

Anova(mixed_suctom_s33)

#####ANÁLISE 4####
##Alagados####
####Altura####
dadosdestinoalagamento<-read.csv("Dados_destinoalagamento.csv", sep = ";")

dadosdestinoalagamento<-dadosdestinoalagamento[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altura", "Destino2", "Femea", "Macho")]

dadosdestinoalagamento$ Destino2_amb<-as.factor(dadosdestinoalagamento$ Destino2)
dadosdestinoalagamento<-read.csv("Dados_destinoalagamento.csv", sep = ";")

dadosdestinoalagamento<-dadosdestinoalagamento[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altura", "Destino2", "Femea", "Macho")]

dadosdestinoalagamento$Destino2_amb<-as.factor(dadosdestinoalagamento$Destino2)

dadosdestinoalagamento$Femea<-as.factor(dadosdestinoalagamento$Femea)
dadosdestinoalagamento$Macho<-as.factor(dadosdestinoalagamento$Macho)
dadosdestinoalagamento$Local<-as.factor(dadosdestinoalagamento$Local)
dadosdestinoalagamento$Ambiente_N1<-as.factor(dadosdestinoalagamento$Ambiente_N1)

dadosdestinoalagamento<-na.omit(dadosdestinoalagamento)

#5
mixed_sucala <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento, family = binomial)

#4
mixed_sucala_s1 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s2 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s3 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s4 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s5 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinoalagamento,  family = binomial)


#3
mixed_sucala_s6 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s7 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s8 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s9 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s10 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s11 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s12 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s12 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s13 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s14 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s15 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)


#2 
mixed_sucala_s16 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s17 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s18 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s19 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s20 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s21 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinoalagamento,family = binomial)
mixed_sucala_s22 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s23 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s24 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s25 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinoalagamento, family = binomial)

#1
mixed_sucala_s26 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s27 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s28 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s29 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s30 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)

mixed_sucala_s31 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)

mixed_sucala_AIC<-AIC(mixed_sucala, mixed_sucala_s1, mixed_sucala_s3, mixed_sucala_s5, mixed_sucala_s6, mixed_sucala_s7, mixed_sucala_s8, mixed_sucala_s9, mixed_sucala_s10, mixed_sucala_s11, mixed_sucala_s12, mixed_sucala_s13, mixed_sucala_s14, mixed_sucala_s15, mixed_sucala_s16, mixed_sucala_s17, mixed_sucala_s18, mixed_sucala_s19, mixed_sucala_s20, mixed_sucala_s21, mixed_sucala_s23, mixed_sucala_s24, mixed_sucala_s25, mixed_sucala_s26, mixed_sucala_s27, mixed_sucala_s28, mixed_sucala_s29, mixed_sucala_s30, mixed_sucala_s31)


sort(mixed_sucala_AIC$AIC)

mixed_sucala_s32 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s33 <- glmer(Destino2~ Diferenca_altura + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)




mixed_sucala_AIC<-AIC(mixed_sucala_s32, mixed_sucala_s33, mixed_sucala_s34)

Anova(mixed_sucala_s33)

####Altitude####
dadosdestinoalagamento<-read.csv("Dados_destinoalagamento.csv", sep = ";")

dadosdestinoalagamento<-dadosdestinoalagamento[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altitude", "Destino2", "Femea", "Macho")]

dadosdestinoalagamento$ Destino2_amb<-as.factor(dadosdestinoalagamento$ Destino2)
dadosdestinoalagamento<-read.csv("Dados_destinoalagamento.csv", sep = ";")

dadosdestinoalagamento<-dadosdestinoalagamento[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altitude", "Destino2", "Femea", "Macho")]

dadosdestinoalagamento$Destino2_amb<-as.factor(dadosdestinoalagamento$Destino2)

dadosdestinoalagamento$Femea<-as.factor(dadosdestinoalagamento$Femea)
dadosdestinoalagamento$Macho<-as.factor(dadosdestinoalagamento$Macho)
dadosdestinoalagamento$Local<-as.factor(dadosdestinoalagamento$Local)
dadosdestinoalagamento$Ambiente_N1<-as.factor(dadosdestinoalagamento$Ambiente_N1)

dadosdestinoalagamento<-na.omit(dadosdestinoalagamento)

#5
mixed_sucala <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento, family = binomial)

#4
mixed_sucala_s1 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s2 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s3 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s4 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s5 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinoalagamento,  family = binomial)


#3
mixed_sucala_s6 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s7 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s8 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s9 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s10 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s11 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s12 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s12 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s13 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s14 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s15 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)


#2 
mixed_sucala_s16 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s17 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s18 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s19 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s20 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s21 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinoalagamento,family = binomial)
mixed_sucala_s22 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s23 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s24 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s25 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinoalagamento, family = binomial)

#1
mixed_sucala_s26 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s27 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s28 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s29 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s30 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)

mixed_sucala_s31 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)

mixed_sucala_AIC<-AIC(mixed_sucala, mixed_sucala_s1, mixed_sucala_s3, mixed_sucala_s5, mixed_sucala_s6, mixed_sucala_s7, mixed_sucala_s8, mixed_sucala_s9, mixed_sucala_s10, mixed_sucala_s11, mixed_sucala_s12, mixed_sucala_s13, mixed_sucala_s14, mixed_sucala_s15, mixed_sucala_s16, mixed_sucala_s17, mixed_sucala_s18, mixed_sucala_s19, mixed_sucala_s20, mixed_sucala_s21, mixed_sucala_s23, mixed_sucala_s24, mixed_sucala_s25, mixed_sucala_s26, mixed_sucala_s27, mixed_sucala_s28, mixed_sucala_s29, mixed_sucala_s30, mixed_sucala_s31)


sort(mixed_sucala_AIC$AIC)

mixed_sucala_s32 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s33 <- glmer(Destino2~ Diferenca_altitude + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)




mixed_sucala_AIC<-AIC(mixed_sucala_s32, mixed_sucala_s33, mixed_sucala_s34)

Anova(mixed_sucala_s33)


####Distancia####
dadosdestinoalagamento<-read.csv("Dados_destinoalagamento.csv", sep = ";")

dadosdestinoalagamento<-dadosdestinoalagamento[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Distancia", "Destino2", "Femea", "Macho")]

dadosdestinoalagamento$Destino2_amb<-as.factor(dadosdestinoalagamento$Destino2)

dadosdestinoalagamento$Femea<-as.factor(dadosdestinoalagamento$Femea)
dadosdestinoalagamento$Macho<-as.factor(dadosdestinoalagamento$Macho)
dadosdestinoalagamento$Local<-as.factor(dadosdestinoalagamento$Local)
dadosdestinoalagamento$Ambiente_N1<-as.factor(dadosdestinoalagamento$Ambiente_N1)

dadosdestinoalagamento<-na.omit(dadosdestinoalagamento)

#5
mixed_sucala <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento, family = binomial)

#4
mixed_sucala_s1 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s2 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s3 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s4 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s5 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinoalagamento,  family = binomial)


#3
mixed_sucala_s6 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s7 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s8 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s9 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s10 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s11 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s12 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s12 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s13 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s14 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s15 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)


#2 
mixed_sucala_s16 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s17 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s18 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s19 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s20 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s21 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinoalagamento,family = binomial)
mixed_sucala_s22 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s23 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s24 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s25 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinoalagamento, family = binomial)

#1
mixed_sucala_s26 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s27 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s28 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s29 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s30 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)

mixed_sucala_s31 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)

mixed_sucala_AIC<-AIC(mixed_sucala, mixed_sucala_s1, mixed_sucala_s3, mixed_sucala_s5, mixed_sucala_s6, mixed_sucala_s7, mixed_sucala_s8, mixed_sucala_s9, mixed_sucala_s10, mixed_sucala_s11, mixed_sucala_s12, mixed_sucala_s13, mixed_sucala_s14, mixed_sucala_s15, mixed_sucala_s16, mixed_sucala_s17, mixed_sucala_s18, mixed_sucala_s20, mixed_sucala_s21, mixed_sucala_s23, mixed_sucala_s24, mixed_sucala_s25, mixed_sucala_s26, mixed_sucala_s27, mixed_sucala_s28, mixed_sucala_s29, mixed_sucala_s30, mixed_sucala_s31)


sort(mixed_sucala_AIC$AIC)

mixed_sucala_s32 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s33 <- glmer(Destino2~ Distancia + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)




mixed_sucala_AIC<-AIC(mixed_sucala_s32, mixed_sucala_s33, mixed_sucala_s34)

Anova(mixed_sucala_s33)

####Ambiente####
dadosdestinoalagamento<-read.csv("Dados_destinoalagamento.csv", sep = ";")

dadosdestinoalagamento<-dadosdestinoalagamento[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Mudou_amb", "Destino2", "Femea", "Macho")]

dadosdestinoalagamento$Destino2_amb<-as.factor(dadosdestinoalagamento$Destino2)

dadosdestinoalagamento$Femea<-as.factor(dadosdestinoalagamento$Femea)
dadosdestinoalagamento$Macho<-as.factor(dadosdestinoalagamento$Macho)
dadosdestinoalagamento$Local<-as.factor(dadosdestinoalagamento$Local)
dadosdestinoalagamento$Ambiente_N1<-as.factor(dadosdestinoalagamento$Ambiente_N1)

dadosdestinoalagamento<-na.omit(dadosdestinoalagamento)

#5
mixed_sucala <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento, family = binomial)

#4
mixed_sucala_s1 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s2 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s3 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s4 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s5 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinoalagamento,  family = binomial)


#3
mixed_sucala_s6 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s7 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s8 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s9 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s10 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s11 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s12 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s12 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s13 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s14 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s15 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)


#2 
mixed_sucala_s16 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s17 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s18 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s19 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s20 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s21 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinoalagamento,family = binomial)
mixed_sucala_s22 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s23 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s24 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s25 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinoalagamento, family = binomial)

#1
mixed_sucala_s26 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s27 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s28 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s29 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinoalagamento,  family = binomial)
mixed_sucala_s30 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinoalagamento,  family = binomial)

mixed_sucala_s31 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)

mixed_sucala_AIC<-AIC(mixed_sucala, mixed_sucala_s1, mixed_sucala_s3, mixed_sucala_s5, mixed_sucala_s6, mixed_sucala_s7, mixed_sucala_s8, mixed_sucala_s9, mixed_sucala_s10, mixed_sucala_s11, mixed_sucala_s12, mixed_sucala_s13, mixed_sucala_s14, mixed_sucala_s15, mixed_sucala_s16, mixed_sucala_s17, mixed_sucala_s18, mixed_sucala_s19, mixed_sucala_s20, mixed_sucala_s21, mixed_sucala_s23, mixed_sucala_s24, mixed_sucala_s25, mixed_sucala_s26, mixed_sucala_s27, mixed_sucala_s28, mixed_sucala_s29, mixed_sucala_s30, mixed_sucala_s31)


sort(mixed_sucala_AIC$AIC)

mixed_sucala_s32 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s33 <- glmer(Destino2~ Mudou_amb + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)
mixed_sucala_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinoalagamento, family = binomial)




mixed_sucala_AIC<-AIC(mixed_sucala_s32, mixed_sucala_s33, mixed_sucala_s34)

Anova(mixed_sucala_s33)

##Predados####
####Altura####
dadosdestinopredacao<-read.csv("Dados_destinopredacao.csv", sep = ";")

dadosdestinopredacao<-dadosdestinopredacao[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altura", "Destino.2", "Femea", "Macho")]
names(dadosdestinopredacao)[names(dadosdestinopredacao) == "Destino.2"] <- "Destino2"


dadosdestinopredacao$Destino2<-as.factor(dadosdestinopredacao$Destino2)

dadosdestinopredacao$Femea<-as.factor(dadosdestinopredacao$Femea)
dadosdestinopredacao$Macho<-as.factor(dadosdestinopredacao$Macho)
dadosdestinopredacao$Local<-as.factor(dadosdestinopredacao$Local)
dadosdestinopredacao$Ambiente_N1<-as.factor(dadosdestinopredacao$Ambiente_N1)

dadosdestinopredacao<-na.omit(dadosdestinopredacao)

#5
mixed_sucpre <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao, family = binomial)

#4
mixed_sucpre_s1 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s2 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s3 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s4 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s5 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao,  family = binomial)


#3
mixed_sucpre_s6 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s7 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s8 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s9 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s10 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s11 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s12 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s12 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s13 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s14 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s15 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinopredacao,  family = binomial)


#2 
mixed_sucpre_s16 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s17 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s18 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s19 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s20 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s21 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinopredacao,family = binomial)
mixed_sucpre_s22 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s23 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s24 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s25 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinopredacao, family = binomial)

#1
mixed_sucpre_s26 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s27 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s28 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s29 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s30 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)

mixed_sucpre_s31 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)

mixed_sucpre_AIC<-AIC(mixed_sucpre, mixed_sucpre_s3, mixed_sucpre_s6, mixed_sucpre_s7, mixed_sucpre_s8, mixed_sucpre_s10, mixed_sucpre_s11, mixed_sucpre_s12, mixed_sucpre_s14, mixed_sucpre_s15, mixed_sucpre_s17, mixed_sucpre_s18, mixed_sucpre_s19, mixed_sucpre_s20, mixed_sucpre_s21, mixed_sucpre_s23, mixed_sucpre_s24, mixed_sucpre_s25, mixed_sucpre_s26, mixed_sucpre_s27, mixed_sucpre_s28, mixed_sucpre_s30, mixed_sucpre_s31)


sort(mixed_sucpre_AIC$AIC)

mixed_sucpre_s32 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s33 <- glmer(Destino2~ Diferenca_altura + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)




mixed_sucpre_AIC<-AIC(mixed_sucpre_s32, mixed_sucpre_s33, mixed_sucpre_s34)

Anova(mixed_sucpre_s33)

####Altitude####
dadosdestinopredacao<-read.csv("Dados_destinopredacao.csv", sep = ";")

dadosdestinopredacao<-dadosdestinopredacao[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altitude", "Destino.2", "Femea", "Macho")]

names(dadosdestinopredacao)[names(dadosdestinopredacao) == "Destino.2"] <- "Destino2"


dadosdestinopredacao$Destino2<-as.factor(dadosdestinopredacao$Destino2)

dadosdestinopredacao$Femea<-as.factor(dadosdestinopredacao$Femea)
dadosdestinopredacao$Macho<-as.factor(dadosdestinopredacao$Macho)
dadosdestinopredacao$Local<-as.factor(dadosdestinopredacao$Local)
dadosdestinopredacao$Ambiente_N1<-as.factor(dadosdestinopredacao$Ambiente_N1)

dadosdestinopredacao<-na.omit(dadosdestinopredacao)

#5
mixed_sucpre <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao, family = binomial)

#4
mixed_sucpre_s1 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s2 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s3 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s4 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s5 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao,  family = binomial)


#3
mixed_sucpre_s6 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s7 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s8 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s9 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s10 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s11 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s12 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s12 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s13 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s14 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s15 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinopredacao,  family = binomial)


#2 
mixed_sucpre_s16 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s17 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s18 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s19 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s20 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s21 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinopredacao,family = binomial)
mixed_sucpre_s22 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s23 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s24 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s25 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinopredacao, family = binomial)

#1
mixed_sucpre_s26 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s27 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s28 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s29 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s30 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)

mixed_sucpre_s31 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)

mixed_sucpre_AIC<-AIC(mixed_sucpre, mixed_sucpre_s1, mixed_sucpre_s2, mixed_sucpre_s3, mixed_sucpre_s4, mixed_sucpre_s5, mixed_sucpre_s6, mixed_sucpre_s7, mixed_sucpre_s8, mixed_sucpre_s9, mixed_sucpre_s10, mixed_sucpre_s11, mixed_sucpre_s12, mixed_sucpre_s13, mixed_sucpre_s14, mixed_sucpre_s15, mixed_sucpre_s16, mixed_sucpre_s17, mixed_sucpre_s18, mixed_sucpre_s19, mixed_sucpre_s20, mixed_sucpre_s21, mixed_sucpre_s23, mixed_sucpre_s24, mixed_sucpre_s25, mixed_sucpre_s26, mixed_sucpre_s27, mixed_sucpre_s28, mixed_sucpre_s29, mixed_sucpre_s30, mixed_sucpre_s31)


sort(mixed_sucpre_AIC$AIC)

mixed_sucpre_s32 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s33 <- glmer(Destino2~ Diferenca_altitude + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)




mixed_sucpre_AIC<-AIC(mixed_sucpre_s32, mixed_sucpre_s33, mixed_sucpre_s34)

Anova(mixed_sucpre_s32)


####Distancia####
dadosdestinopredacao<-read.csv("Dados_destinopredacao.csv", sep = ";")

dadosdestinopredacao<-dadosdestinopredacao[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Distancia", "Destino.2", "Femea", "Macho")]

names(dadosdestinopredacao)[names(dadosdestinopredacao) == "Destino.2"] <- "Destino2"


dadosdestinopredacao$Destino2<-as.factor(dadosdestinopredacao$Destino2)

dadosdestinopredacao$Femea<-as.factor(dadosdestinopredacao$Femea)
dadosdestinopredacao$Macho<-as.factor(dadosdestinopredacao$Macho)
dadosdestinopredacao$Local<-as.factor(dadosdestinopredacao$Local)
dadosdestinopredacao$Ambiente_N1<-as.factor(dadosdestinopredacao$Ambiente_N1)

dadosdestinopredacao<-na.omit(dadosdestinopredacao)

#5
mixed_sucpre <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao, family = binomial)

#4
mixed_sucpre_s1 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s2 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s3 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s4 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s5 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao,  family = binomial)


#3
mixed_sucpre_s6 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s7 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s8 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s9 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s10 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s11 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s12 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s12 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s13 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s14 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s15 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinopredacao,  family = binomial)


#2 
mixed_sucpre_s16 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s17 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s18 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s19 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s20 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s21 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinopredacao,family = binomial)
mixed_sucpre_s22 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s23 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s24 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s25 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinopredacao, family = binomial)

#1
mixed_sucpre_s26 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s27 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s28 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s29 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s30 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)

mixed_sucpre_s31 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)

mixed_sucpre_AIC<-AIC( mixed_sucpre_s3, mixed_sucpre_s5, mixed_sucpre_s6, mixed_sucpre_s8, mixed_sucpre_s10, mixed_sucpre_s11, mixed_sucpre_s12, mixed_sucpre_s13, mixed_sucpre_s16, mixed_sucpre_s17, mixed_sucpre_s18, mixed_sucpre_s20, mixed_sucpre_s21, mixed_sucpre_s23, mixed_sucpre_s24, mixed_sucpre_s25, mixed_sucpre_s26, mixed_sucpre_s27, mixed_sucpre_s28, mixed_sucpre_s29, mixed_sucpre_s31)


sort(mixed_sucpre_AIC$AIC)

mixed_sucpre_s32 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s33 <- glmer(Destino2~ Distancia + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)




mixed_sucpre_AIC<-AIC(mixed_sucpre_s32, mixed_sucpre_s33, mixed_sucpre_s34)

Anova(mixed_sucpre_s33)


####Ambiente####
dadosdestinopredacao<-read.csv("Dados_destinopredacao.csv", sep = ";")

dadosdestinopredacao<-dadosdestinopredacao[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Mudou_amb", "Destino.2", "Femea", "Macho")]

names(dadosdestinopredacao)[names(dadosdestinopredacao) == "Destino.2"] <- "Destino2"


dadosdestinopredacao$Destino2<-as.factor(dadosdestinopredacao$Destino2)

dadosdestinopredacao$Femea<-as.factor(dadosdestinopredacao$Femea)
dadosdestinopredacao$Macho<-as.factor(dadosdestinopredacao$Macho)
dadosdestinopredacao$Local<-as.factor(dadosdestinopredacao$Local)
dadosdestinopredacao$Ambiente_N1<-as.factor(dadosdestinopredacao$Ambiente_N1)

dadosdestinopredacao<-na.omit(dadosdestinopredacao)

#5
mixed_sucpre <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao, family = binomial)

#4
mixed_sucpre_s1 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s2 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s3 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s4 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s5 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao,  family = binomial)


#3
mixed_sucpre_s6 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s7 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s8 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s9 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s10 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s11 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s12 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s12 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s13 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s14 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s15 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinopredacao,  family = binomial)


#2 
mixed_sucpre_s16 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s17 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s18 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s19 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s20 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s21 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinopredacao,family = binomial)
mixed_sucpre_s22 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s23 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s24 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s25 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinopredacao, family = binomial)

#1
mixed_sucpre_s26 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s27 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s28 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s29 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s30 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)

mixed_sucpre_s31 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)

mixed_sucpre_AIC<-AIC(mixed_sucpre, mixed_sucpre_s3, mixed_sucpre_s5, mixed_sucpre_s7, mixed_sucpre_s8, mixed_sucpre_s9, mixed_sucpre_s10, mixed_sucpre_s11, mixed_sucpre_s13, mixed_sucpre_s14, mixed_sucpre_s15, mixed_sucpre_s16, mixed_sucpre_s17, mixed_sucpre_s18, mixed_sucpre_s20, mixed_sucpre_s24, mixed_sucpre_s25, mixed_sucpre_s27, mixed_sucpre_s28, mixed_sucpre_s29, mixed_sucpre_s30, mixed_sucpre_s31)


sort(mixed_sucpre_AIC$AIC)

mixed_sucpre_s32 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s33 <- glmer(Destino2~ Mudou_amb + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)




mixed_sucpre_AIC<-AIC(mixed_sucpre_s32, mixed_sucpre_s33, mixed_sucpre_s34)

Anova(mixed_sucpre_s33)

####Espessura####
dadosdestinopredacao<-read.csv("Dados_destinopredacao.csv", sep = ";")

dadosdestinopredacao<-dadosdestinopredacao[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_espessura", "Destino.2", "Femea", "Macho")]

names(dadosdestinopredacao)[names(dadosdestinopredacao) == "Destino.2"] <- "Destino2"


dadosdestinopredacao$Destino2<-as.factor(dadosdestinopredacao$Destino2)

dadosdestinopredacao$Femea<-as.factor(dadosdestinopredacao$Femea)
dadosdestinopredacao$Macho<-as.factor(dadosdestinopredacao$Macho)
dadosdestinopredacao$Local<-as.factor(dadosdestinopredacao$Local)
dadosdestinopredacao$Ambiente_N1<-as.factor(dadosdestinopredacao$Ambiente_N1)

dadosdestinopredacao<-na.omit(dadosdestinopredacao)

#5
mixed_sucpre <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao, family = binomial)

#4
mixed_sucpre_s1 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s2 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s3 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s4 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s5 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinopredacao,  family = binomial)


#3
mixed_sucpre_s6 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s7 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s8 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s9 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s10 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s11 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s12 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s12 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s13 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s14 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s15 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinopredacao,  family = binomial)


#2 
mixed_sucpre_s16 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s17 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s18 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s19 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s20 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s21 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinopredacao,family = binomial)
mixed_sucpre_s22 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s23 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s24 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s25 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinopredacao, family = binomial)

#1
mixed_sucpre_s26 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s27 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s28 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s29 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinopredacao,  family = binomial)
mixed_sucpre_s30 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinopredacao,  family = binomial)

mixed_sucpre_s31 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)

mixed_sucpre_AIC<-AIC(mixed_sucpre, mixed_sucpre_s5, mixed_sucpre_s7, mixed_sucpre_s8, mixed_sucpre_s9, mixed_sucpre_s10, mixed_sucpre_s11, mixed_sucpre_s13, mixed_sucpre_s14, mixed_sucpre_s15, mixed_sucpre_s16, mixed_sucpre_s17, mixed_sucpre_s18, mixed_sucpre_s20, mixed_sucpre_s24, mixed_sucpre_s25, mixed_sucpre_s27, mixed_sucpre_s28, mixed_sucpre_s29, mixed_sucpre_s30, mixed_sucpre_s31)


sort(mixed_sucpre_AIC$AIC)

mixed_sucpre_s32 <- glmer(Destino2~ Diferenca_espessura + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s33 <- glmer(Destino2~ Diferenca_espessura + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)
mixed_sucpre_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinopredacao, family = binomial)




mixed_sucpre_AIC<-AIC(mixed_sucpre_s32, mixed_sucpre_s33, mixed_sucpre_s34)

Anova(mixed_sucpre_s33)


##Tombados####
####Altura####
dadosdestinotombamento<-read.csv("Dados_destinotombamento.csv", sep = ";")

dadosdestinotombamento<-dadosdestinotombamento[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altura", "Destino2", "Femea", "Macho")]

dadosdestinotombamento$Destino2_amb<-as.factor(dadosdestinotombamento$Destino2)

dadosdestinotombamento$Femea<-as.factor(dadosdestinotombamento$Femea)
dadosdestinotombamento$Macho<-as.factor(dadosdestinotombamento$Macho)
dadosdestinotombamento$Local<-as.factor(dadosdestinotombamento$Local)
dadosdestinotombamento$Ambiente_N1<-as.factor(dadosdestinotombamento$Ambiente_N1)

dadosdestinotombamento<-na.omit(dadosdestinotombamento)

#5
mixed_suctom <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento, family = binomial)

#4
mixed_suctom_s1 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s2 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s3 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s4 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s5 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinotombamento,  family = binomial)


#3
mixed_suctom_s6 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s7 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s8 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s9 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s10 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s11 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s12 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s12 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s13 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s14 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s15 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinotombamento,  family = binomial)


#2 
mixed_suctom_s16 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s17 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s18 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s19 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s20 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s21 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinotombamento,family = binomial)
mixed_suctom_s22 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s23 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s24 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s25 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinotombamento, family = binomial)

#1
mixed_suctom_s26 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s27 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s28 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s29 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s30 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)

mixed_suctom_s31 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinotombamento, family = binomial)

mixed_suctom_AIC<-AIC(mixed_suctom, mixed_suctom_s1, mixed_suctom_s3, mixed_suctom_s6, mixed_suctom_s7, mixed_suctom_s8, mixed_suctom_s9, mixed_suctom_s10, mixed_suctom_s11, mixed_suctom_s12, mixed_suctom_s13, mixed_suctom_s14, mixed_suctom_s15, mixed_suctom_s16, mixed_suctom_s17, mixed_suctom_s18, mixed_suctom_s19, mixed_suctom_s20, mixed_suctom_s21, mixed_suctom_s23, mixed_suctom_s24, mixed_suctom_s25, mixed_suctom_s26, mixed_suctom_s27, mixed_suctom_s28, mixed_suctom_s29, mixed_suctom_s30, mixed_suctom_s31)


sort(mixed_suctom_AIC$AIC)

mixed_suctom_s32 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s33 <- glmer(Destino2~ Diferenca_altura + (1 | Territorio) + (1 | Ano), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinotombamento, family = binomial)




mixed_suctom_AIC<-AIC(mixed_suctom_s32, mixed_suctom_s33, mixed_suctom_s34)

Anova(mixed_suctom_s33)



####Distancia####
dadosdestinotombamento<-read.csv("Dados_destinotombamento.csv", sep = ";")

dadosdestinotombamento<-dadosdestinotombamento[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Distancia", "Destino2", "Femea", "Macho")]

dadosdestinotombamento$Destino2_amb<-as.factor(dadosdestinotombamento$Destino2)

dadosdestinotombamento$Femea<-as.factor(dadosdestinotombamento$Femea)
dadosdestinotombamento$Macho<-as.factor(dadosdestinotombamento$Macho)
dadosdestinotombamento$Local<-as.factor(dadosdestinotombamento$Local)
dadosdestinotombamento$Ambiente_N1<-as.factor(dadosdestinotombamento$Ambiente_N1)

dadosdestinotombamento<-na.omit(dadosdestinotombamento)

#5
mixed_suctom <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento, family = binomial)

#4
mixed_suctom_s1 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s2 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s3 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s4 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s5 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinotombamento,  family = binomial)


#3
mixed_suctom_s6 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s7 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s8 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s9 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s10 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s11 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s12 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s12 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s13 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s14 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s15 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinotombamento,  family = binomial)


#2 
mixed_suctom_s16 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s17 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s18 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s19 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s20 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s21 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinotombamento,family = binomial)
mixed_suctom_s22 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s23 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s24 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s25 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinotombamento, family = binomial)

#1
mixed_suctom_s26 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s27 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s28 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s29 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s30 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)

mixed_suctom_s31 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinotombamento, family = binomial)

mixed_suctom_AIC<-AIC(mixed_suctom, mixed_suctom_s1, mixed_suctom_s3, mixed_suctom_s5, mixed_suctom_s6, mixed_suctom_s7, mixed_suctom_s8, mixed_suctom_s9, mixed_suctom_s10, mixed_suctom_s11, mixed_suctom_s12, mixed_suctom_s13, mixed_suctom_s14, mixed_suctom_s15, mixed_suctom_s16, mixed_suctom_s17, mixed_suctom_s18, mixed_suctom_s20, mixed_suctom_s21, mixed_suctom_s23, mixed_suctom_s24, mixed_suctom_s25, mixed_suctom_s26, mixed_suctom_s27, mixed_suctom_s28, mixed_suctom_s29, mixed_suctom_s30, mixed_suctom_s31)


sort(mixed_suctom_AIC$AIC)

mixed_suctom_s32 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s33 <- glmer(Destino2~ Distancia + (1 | Territorio) + (1 | Ano), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinotombamento, family = binomial)




mixed_suctom_AIC<-AIC(mixed_suctom_s32, mixed_suctom_s33, mixed_suctom_s34)

Anova(mixed_suctom_s33)

####Ambiente####
dadosdestinotombamento<-read.csv("Dados_destinotombamento.csv", sep = ";")

dadosdestinotombamento<-dadosdestinotombamento[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Mudou_amb", "Destino2", "Femea", "Macho")]

dadosdestinotombamento$Destino2_amb<-as.factor(dadosdestinotombamento$Destino2)

dadosdestinotombamento$Femea<-as.factor(dadosdestinotombamento$Femea)
dadosdestinotombamento$Macho<-as.factor(dadosdestinotombamento$Macho)
dadosdestinotombamento$Local<-as.factor(dadosdestinotombamento$Local)
dadosdestinotombamento$Ambiente_N1<-as.factor(dadosdestinotombamento$Ambiente_N1)

dadosdestinotombamento<-na.omit(dadosdestinotombamento)

#5
mixed_suctom <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento, family = binomial)

#4
mixed_suctom_s1 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s2 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s3 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s4 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s5 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinotombamento,  family = binomial)


#3
mixed_suctom_s6 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s7 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s8 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s9 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s10 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s11 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s12 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s12 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s13 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s14 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s15 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinotombamento,  family = binomial)


#2 
mixed_suctom_s16 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s17 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s18 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s19 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s20 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s21 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinotombamento,family = binomial)
mixed_suctom_s22 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s23 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s24 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s25 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinotombamento, family = binomial)

#1
mixed_suctom_s26 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s27 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s28 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s29 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinotombamento,  family = binomial)
mixed_suctom_s30 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinotombamento,  family = binomial)

mixed_suctom_s31 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinotombamento, family = binomial)

mixed_suctom_AIC<-AIC(mixed_suctom_s1, mixed_suctom_s3, mixed_suctom_s5, mixed_suctom_s7, mixed_suctom_s8, mixed_suctom_s9, mixed_suctom_s10, mixed_suctom_s11, mixed_suctom_s12, mixed_suctom_s14, mixed_suctom_s15, mixed_suctom_s16, mixed_suctom_s17, mixed_suctom_s18, mixed_suctom_s19, mixed_suctom_s20, mixed_suctom_s21, mixed_suctom_s23, mixed_suctom_s24, mixed_suctom_s25, mixed_suctom_s26, mixed_suctom_s27, mixed_suctom_s28, mixed_suctom_s29, mixed_suctom_s30, mixed_suctom_s31)


sort(mixed_suctom_AIC$AIC)

mixed_suctom_s32 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s33 <- glmer(Destino2~ Mudou_amb + (1 | Territorio) + (1 | Ano), data = dadosdestinotombamento, family = binomial)
mixed_suctom_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinotombamento, family = binomial)




mixed_suctom_AIC<-AIC(mixed_suctom_s32, mixed_suctom_s33, mixed_suctom_s34)

Anova(mixed_suctom_s33)


#####SUCESSO#####
####Altura####
dadosdestinosucesso<-read.csv("Dados_destinosucesso.csv", sep = ";")

dadosdestinosucesso<-dadosdestinosucesso[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altura", "Destino2", "Femea", "Macho")]

dadosdestinosucesso$Destino2_amb<-as.factor(dadosdestinosucesso$Destino2)

dadosdestinosucesso$Femea<-as.factor(dadosdestinosucesso$Femea)
dadosdestinosucesso$Macho<-as.factor(dadosdestinosucesso$Macho)
dadosdestinosucesso$Local<-as.factor(dadosdestinosucesso$Local)
dadosdestinosucesso$Ambiente_N1<-as.factor(dadosdestinosucesso$Ambiente_N1)

dadosdestinosucesso<-na.omit(dadosdestinosucesso)

#5
mixed_sucsuc <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso, family = binomial)

#4
mixed_sucsuc_s1 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s2 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s3 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s4 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s5 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinosucesso,  family = binomial)


#3
mixed_sucsuc_s6 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s7 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s8 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s9 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s10 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s11 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s12 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s12 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s13 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s14 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s15 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinosucesso,  family = binomial)


#2 
mixed_sucsuc_s16 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s17 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s18 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s19 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s20 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s21 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinosucesso,family = binomial)
mixed_sucsuc_s22 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s23 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s24 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s25 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinosucesso, family = binomial)

#1
mixed_sucsuc_s26 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s27 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s28 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s29 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s30 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)

mixed_sucsuc_s31 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)

mixed_sucsuc_AIC<-AIC(mixed_sucsuc, mixed_sucsuc_s1, mixed_sucsuc_s3, mixed_sucsuc_s5, mixed_sucsuc_s6, mixed_sucsuc_s7, mixed_sucsuc_s8, mixed_sucsuc_s9, mixed_sucsuc_s10, mixed_sucsuc_s11, mixed_sucsuc_s12, mixed_sucsuc_s13, mixed_sucsuc_s14, mixed_sucsuc_s15, mixed_sucsuc_s16, mixed_sucsuc_s17, mixed_sucsuc_s18, mixed_sucsuc_s20, mixed_sucsuc_s21, mixed_sucsuc_s23, mixed_sucsuc_s24, mixed_sucsuc_s25, mixed_sucsuc_s26, mixed_sucsuc_s27, mixed_sucsuc_s28, mixed_sucsuc_s29, mixed_sucsuc_s30, mixed_sucsuc_s31)


sort(mixed_sucsuc_AIC$AIC)

mixed_sucsuc_s32 <- glmer(Destino2~ Diferenca_altura + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s33 <- glmer(Destino2~ Diferenca_altura + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)




mixed_sucsuc_AIC<-AIC(mixed_sucsuc_s32, mixed_sucsuc_s33, mixed_sucsuc_s34)

Anova(mixed_sucsuc_s33)

####Altitude####
dadosdestinosucesso<-read.csv("Dados_destinosucesso.csv", sep = ";")

dadosdestinosucesso<-dadosdestinosucesso[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altitude", "Destino2", "Femea", "Macho")]

dadosdestinosucesso$ Destino2_amb<-as.factor(dadosdestinosucesso$ Destino2)
dadosdestinosucesso<-read.csv("Dados_destinosucesso.csv", sep = ";")

dadosdestinosucesso<-dadosdestinosucesso[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Diferenca_altitude", "Destino2", "Femea", "Macho")]

dadosdestinosucesso$Destino2_amb<-as.factor(dadosdestinosucesso$Destino2)

dadosdestinosucesso$Femea<-as.factor(dadosdestinosucesso$Femea)
dadosdestinosucesso$Macho<-as.factor(dadosdestinosucesso$Macho)
dadosdestinosucesso$Local<-as.factor(dadosdestinosucesso$Local)
dadosdestinosucesso$Ambiente_N1<-as.factor(dadosdestinosucesso$Ambiente_N1)

dadosdestinosucesso<-na.omit(dadosdestinosucesso)

#5
mixed_sucsuc <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso, family = binomial)

#4
mixed_sucsuc_s1 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s2 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s3 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s4 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s5 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinosucesso,  family = binomial)


#3
mixed_sucsuc_s6 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s7 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s8 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s9 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s10 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s11 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s12 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s12 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s13 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s14 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s15 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinosucesso,  family = binomial)


#2 
mixed_sucsuc_s16 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s17 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s18 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s19 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s20 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s21 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinosucesso,family = binomial)
mixed_sucsuc_s22 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s23 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s24 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s25 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinosucesso, family = binomial)

#1
mixed_sucsuc_s26 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s27 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s28 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s29 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s30 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)

mixed_sucsuc_s31 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)

mixed_sucsuc_AIC<-AIC(mixed_sucsuc, mixed_sucsuc_s1, mixed_sucsuc_s3, mixed_sucsuc_s5, mixed_sucsuc_s6, mixed_sucsuc_s7, mixed_sucsuc_s8, mixed_sucsuc_s9, mixed_sucsuc_s10, mixed_sucsuc_s11, mixed_sucsuc_s12, mixed_sucsuc_s13, mixed_sucsuc_s14, mixed_sucsuc_s15, mixed_sucsuc_s16, mixed_sucsuc_s17, mixed_sucsuc_s18, mixed_sucsuc_s19, mixed_sucsuc_s20, mixed_sucsuc_s21, mixed_sucsuc_s23, mixed_sucsuc_s24, mixed_sucsuc_s25, mixed_sucsuc_s26, mixed_sucsuc_s27, mixed_sucsuc_s28, mixed_sucsuc_s29, mixed_sucsuc_s30, mixed_sucsuc_s31)


sort(mixed_sucsuc_AIC$AIC)

mixed_sucsuc_s32 <- glmer(Destino2~ Diferenca_altitude + Ambiente_N1 + (1 | Territorio) + (1 | Ano)+  (1 | Idade_femea), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s33 <- glmer(Destino2~ Diferenca_altitude + (1 | Territorio) + (1 | Ano)+  (1 | Idade_femea), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano)+  (1 | Idade_femea), data = dadosdestinosucesso, family = binomial)




mixed_sucsuc_AIC<-AIC(mixed_sucsuc_s32, mixed_sucsuc_s33, mixed_sucsuc_s34)

Anova(mixed_sucsuc_s32)


####Distancia####
dadosdestinosucesso<-read.csv("Dados_destinosucesso.csv", sep = ";")

dadosdestinosucesso<-dadosdestinosucesso[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Distancia", "Destino2", "Femea", "Macho")]

dadosdestinosucesso$Destino2_amb<-as.factor(dadosdestinosucesso$Destino2)

dadosdestinosucesso$Femea<-as.factor(dadosdestinosucesso$Femea)
dadosdestinosucesso$Macho<-as.factor(dadosdestinosucesso$Macho)
dadosdestinosucesso$Local<-as.factor(dadosdestinosucesso$Local)
dadosdestinosucesso$Ambiente_N1<-as.factor(dadosdestinosucesso$Ambiente_N1)

dadosdestinosucesso<-na.omit(dadosdestinosucesso)

#5
mixed_sucsuc <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso, family = binomial)

#4
mixed_sucsuc_s1 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s2 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s3 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s4 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s5 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinosucesso,  family = binomial)


#3
mixed_sucsuc_s6 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s7 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s8 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s9 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s10 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s11 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s12 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s12 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s13 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s14 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s15 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinosucesso,  family = binomial)


#2 
mixed_sucsuc_s16 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s17 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s18 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s19 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s20 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s21 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinosucesso,family = binomial)
mixed_sucsuc_s22 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s23 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s24 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s25 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinosucesso, family = binomial)

#1
mixed_sucsuc_s26 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s27 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s28 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s29 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s30 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)

mixed_sucsuc_s31 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)

mixed_sucsuc_AIC<-AIC(mixed_sucsuc, mixed_sucsuc_s1, mixed_sucsuc_s3, mixed_sucsuc_s5, mixed_sucsuc_s6, mixed_sucsuc_s7, mixed_sucsuc_s8, mixed_sucsuc_s9, mixed_sucsuc_s10, mixed_sucsuc_s11, mixed_sucsuc_s14, mixed_sucsuc_s15, mixed_sucsuc_s16, mixed_sucsuc_s17, mixed_sucsuc_s18, mixed_sucsuc_s20, mixed_sucsuc_s21, mixed_sucsuc_s23, mixed_sucsuc_s24, mixed_sucsuc_s25, mixed_sucsuc_s26, mixed_sucsuc_s27, mixed_sucsuc_s28, mixed_sucsuc_s29, mixed_sucsuc_s30, mixed_sucsuc_s31)


sort(mixed_sucsuc_AIC$AIC)

mixed_sucsuc_s32 <- glmer(Destino2~ Distancia + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s33 <- glmer(Destino2~ Distancia + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)




mixed_sucsuc_AIC<-AIC(mixed_sucsuc_s32, mixed_sucsuc_s33, mixed_sucsuc_s34)

Anova(mixed_sucsuc_s33)

####Ambiente####
dadosdestinosucesso<-read.csv("Dados_destinosucesso.csv", sep = ";")

dadosdestinosucesso<-dadosdestinosucesso[,c("Territorio", "Ano", "Ambiente_N1", "Local", "Idade_femea", "Idade_macho", "Mudou_amb", "Destino2", "Femea", "Macho")]

dadosdestinosucesso$Destino2_amb<-as.factor(dadosdestinosucesso$Destino2)

dadosdestinosucesso$Femea<-as.factor(dadosdestinosucesso$Femea)
dadosdestinosucesso$Macho<-as.factor(dadosdestinosucesso$Macho)
dadosdestinosucesso$Local<-as.factor(dadosdestinosucesso$Local)
dadosdestinosucesso$Ambiente_N1<-as.factor(dadosdestinosucesso$Ambiente_N1)

dadosdestinosucesso<-na.omit(dadosdestinosucesso)

#5
mixed_sucsuc <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso, family = binomial)

#4
mixed_sucsuc_s1 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Femea) + (1 | Macho), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s2 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s3 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s4 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_macho) + (1 | Local) + (1 | Femea), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s5 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) +  (1 | Idade_femea) + (1 | Local) + (1 | Femea), data = dadosdestinosucesso,  family = binomial)


#3
mixed_sucsuc_s6 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Macho), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s7 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s8 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s9 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s10 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s11 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s12 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s12 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s13 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s14 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s15 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea) + (1 | Local), data = dadosdestinosucesso,  family = binomial)


#2 
mixed_sucsuc_s16 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s17 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s18 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Local), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s19 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_macho) + (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s20 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s21 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_femea), data = dadosdestinosucesso,family = binomial)
mixed_sucsuc_s22 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Femea) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s23 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s24 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Macho) + (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s25 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) + (1 | Idade_femea) + (1 | Idade_macho), data = dadosdestinosucesso, family = binomial)

#1
mixed_sucsuc_s26 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Local), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s27 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s28 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Macho), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s29 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_femea), data = dadosdestinosucesso,  family = binomial)
mixed_sucsuc_s30 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano) +  (1 | Idade_macho), data = dadosdestinosucesso,  family = binomial)

mixed_sucsuc_s31 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)

mixed_sucsuc_AIC<-AIC(mixed_sucsuc, mixed_sucsuc_s1, mixed_sucsuc_s3, mixed_sucsuc_s5, mixed_sucsuc_s6, mixed_sucsuc_s7, mixed_sucsuc_s8, mixed_sucsuc_s9, mixed_sucsuc_s10, mixed_sucsuc_s11, mixed_sucsuc_s12, mixed_sucsuc_s13, mixed_sucsuc_s14, mixed_sucsuc_s15, mixed_sucsuc_s16, mixed_sucsuc_s17, mixed_sucsuc_s18, mixed_sucsuc_s19, mixed_sucsuc_s20, mixed_sucsuc_s21, mixed_sucsuc_s23, mixed_sucsuc_s24, mixed_sucsuc_s25, mixed_sucsuc_s26, mixed_sucsuc_s29, mixed_sucsuc_s31)


sort(mixed_sucsuc_AIC$AIC)

mixed_sucsuc_s32 <- glmer(Destino2~ Mudou_amb + Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s33 <- glmer(Destino2~ Mudou_amb + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)
mixed_sucsuc_s34 <- glmer(Destino2~ Ambiente_N1 + (1 | Territorio) + (1 | Ano), data = dadosdestinosucesso, family = binomial)




mixed_sucsuc_AIC<-AIC(mixed_sucsuc_s32, mixed_sucsuc_s33, mixed_sucsuc_s34)

Anova(mixed_sucsuc_s33)


