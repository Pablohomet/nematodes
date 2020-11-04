#### SEM NEMATODES####

#library(dendextend)
#library(vegan)
#library(corrgram)
#library(bestNormalize)
#library(nlme)
#library(tidyverse)
library(ggplot2)
#library(plotly)
#library(sjPlot)
#library(MuMIn)
#library(vegan)
#library(dplyr)
library(corrgram)
#library(vegan)
#library(varhandle)
library(semPlot)
library(lavaan)
library(reshape2)
#library(grid)
getwd()
setwd("C:/Users/Usuario/Documents/nematodes")
#nematodes <- read.table("nematodes_intercapa.txt", header=T , sep="\t")
nematodes <- read.table("nematodes_18.txt", header=T , sep="\t")
nematodes <- nematodes[-which(nematodes$site=='Puechabon'),]

nematodes$tree<-as.factor(nematodes$tree)
nematodes$tree_sp<-as.factor(nematodes$tree_sp)
nematodes$treatment<-as.factor(nematodes$treatment)
nematodes$site<-as.factor(nematodes$site)
nematodes$block<-as.factor(nematodes$block)

compuestos<- read.table("compuestosecundarios.txt", header=T , sep="\t")
suelo<- read.table("datosuelo18.txt", header=T , sep="\t")
str(nematodes)

### Select only the compounds sampled in 20018

compuestosh <- (compuestos[,c(22,23,34,35,46,47)])
compuestosr <- (compuestos[,c(16,17,28,29,40,41,52,53)])
compuestos<-cbind(compuestosh,compuestosr)

suelo <- (suelo[,c(9:27)])### Select only the compounds sampled in 20018



corrgram (compuestosh, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
          diag.panel=panel.density, pch=16, lty=1, main="Environmental correlations")

corrgram (compuestosr, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
          diag.panel=panel.density, pch=16, lty=1, main="Environmental correlations")

corrgram (compuestos, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
          diag.panel=panel.density, pch=16, lty=1, main="Environmental correlations")

corrgram (suelo, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
          diag.panel=panel.density, pch=16, lty=1, main="Environmental correlations")


suelo <- (suelo[,c(1,2,5,6,8,9,11:15,17:19)])




factors <- (nematodes[,c(2,3,7:11)])
genus <- (nematodes[,c(13:147)])

##creo la variable suma de porcentage de omnivoros y predadores
omnpredpercen<-apply(nematodes[, c(171,173)],1,sum)
###creo la variable suma de abundancias de omnivoros y predadores
omnpred<-apply(nematodes[, c(192,194)],1,sum)
nematodes<-cbind(nematodes,omnpredpercen)
nematodes<-cbind(nematodes,omnpred)

###saco los porcentages y abundancias de los distintos grupos troficos
trophic_n<-(nematodes[,c(190:194,211)])
trophic_p<-(nematodes[,c(166,167,169,171,173,210)])


semd<-cbind(factors,suelo)
semd<-cbind(semd,compuestos)
semd<-cbind(semd,trophic_n)


####grafico para ver como varian las variables en funcion del tratamiento####


prueba <- (semd[,c(3:15)])



prueba2 <- (semd[,c(3,16:25)])

prueba3 <- (semd[,c(3,26:33)])
#gg<-melt(trophic,id.vars=c("site","treatment"))




gg<-melt(prueba3,id.vars=c("treatment"))

indexs_plot2_alcor<-  ggplot(gg, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
  scale_color_discrete("treatment")+
  
  stat_summary(fun.data = mean_se,geom="errorbar",
               color="black",position=position_dodge(0.9), width=.2)+
  theme(        axis.text.x=element_text(size=14))+
  
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = NULL, y= NULL,size=30)+
  scale_y_continuous(limits=c(0,500), breaks=seq(0,500, by = 40), expand=c(0,0))+
  coord_cartesian(ylim = c(0,500), expand=c(0,0))

indexs_plot2_alcor 




####comprobar y transformar distribucion de las variables######

hist(semd$LAI)
hist(sqrt(semd$basal_area))
hist(semd$X.Sand)
hist(log(semd$mo))
hist(log(semd$moisture_content))


hist(semd$Cmic18)

hist(sqrt(semd$pH18))
hist(sqrt(semd$Nmic18))
hist(sqrt(semd$Cmic.Nmic18))
hist(sqrt(semd$Cmic.Pmic18))
hist(semd$NO3_18)
hist(semd$NH4_18)
hist(semd$B.Glucosidasa18)
hist(semd$LeafTa_M18g)
hist(semd$LeafSu_M18g)
hist(semd$LeafSu_S18g)
hist(semd$LeafSt_M18g)
hist(semd$RootPh_M18g)
hist(semd$RootSu_M18g)
hist(semd$RootTa_M18g)


hist(log(semd$PBray18))

hist(sqrt(semd$Ureasa18))

hist(log(semd$pathogen2016))
hist(log(semd$pathogen2017))
hist(log(semd$pathogen2018))

hist(log(semd$LeafTa_S18g))

hist(log(semd$LeafSt_S18g))

hist(sqrt(semd$RootPh_S18g))
hist(log(semd$RootSt_M18g))
hist(log10(semd$RootSt_S18g))

hist(sqrt(semd$RootSu_S18g))

hist(sqrt(semd$RootTa_S18g))
hist(log(semd$bacterivores_number))
hist(log(semd$fungivores_number))
hist(log(semd$hervibores_number))
hist(log(semd$omnivores_number))
hist(sqrt(semd$predators_number))
hist(log(omnpred))
###transformamos variables
semd$moisture_content <- log(semd$moisture_content)
semd$mo<-(log(semd$mo))
semd$basal_area<-(sqrt(semd$basal_area))
semd$ph18<-(sqrt(semd$pH18))
semd$Nmic18<-(sqrt(semd$Nmic18))
semd$Cmic.Nmic18<-(sqrt(semd$Cmic.Nmic18))
semd$Cmic.Pmic18<-(sqrt(semd$Cmic.Pmic18))


semd$PBray18<-(log(semd$PBray18))

semd$Ureasa18<-(sqrt(semd$Ureasa18))

semd$pathogen2016<-(log(semd$pathogen2016))
semd$pathogen2017<-(log(semd$pathogen2017))
semd$pathogen2018<-(log(semd$pathogen2018))

semd$LeafTa_S18g<-(log(semd$LeafTa_S18g))

semd$LeafSt_S18g<-(log(semd$LeafSt_S18g))

semd$RootPh_S18g<-(sqrt(semd$RootPh_S18g))
semd$RootSt_M18g<-(log(semd$RootSt_M18g))
semd$RootSt_S18g<-(log10(semd$RootSt_S18g))

semd$RootSu_S18g<-(sqrt(semd$RootSu_S18g))

semd$RootTa_S18g<-(sqrt(semd$RootTa_S18g))
semd$bacterivores_number<-(log(semd$bacterivores_number))
semd$fungivores_number<-(log(semd$fungivores_number))
semd$hervibores_number<-(log(semd$hervibores_number))
semd$omnivores_number<-(log(semd$omnivores_number))
semd$predators_number<-(sqrt(semd$predators_number))
semd$omnpred<-(log(semd$omnpred))

semd$Cmic18<-(sqrt(semd$Cmic18))#### lo transformamos para que no haya tanta diferencia en magnitud con las otras variables
semd$Cmic18<-(log(semd$Cmic18))###primero raiz cuadrada y luego logaritmo
semd$NH4_18<-(sqrt(semd$NH4_18))
semd$Nmic18<-(log(semd$Nmic18))
semd$RootTa_M18g<-(log(semd$RootTa_M18g))
semd$RootSu_M18g<-(sqrt(semd$RootSu_M18g))


hist((semd$Cmic18))
hist((semd$NH4_18))
hist((semd$Nmic18))
hist((semd$RootTa_M18g))
hist((semd$RootSu_M18g))

###sem simple#####

semnormal <- '
#regressions
#litter_quality = ~initial_cn #+ sla+thickness## esto seria para crear una variable latente?
#hervibores_number~Cmic18


hervibores_number~moisture_content
hervibores_number~mo
hervibores_number~RootSu_M18g
hervibores_number~RootTa_M18g

#omnivores_number~moisture_content
omnivores_number~RootTa_M18g
#omnivores_number~pathogen2018

bacterivores_number~moisture_content
bacterivores_number~RootTa_M18g
bacterivores_number~Nmic18

#fungivores_number~Nmic18
fungivores_number~pathogen2018 
#fungivores_number~RootSt_M18g
fungivores_number~moisture_content

predators_number~~fungivores_number
predators_number~pathogen2018

RootTa_M18g~pathogen2018
RootSt_M18g~pathogen2018




pathogen2018~moisture_content
#correlation
 


#intercept
'
fit <- sem(semnormal, data=semd, fixed.x=FALSE)
summary(fit, standardized=TRUE)
#this function is to obtain the fit form the model see http://lavaan.ugent.be/tutorial/tutorial.pdf
fitMeasures(fit, c("cfi","rmsea","srmr", "pvalue"))

corrgram(semd[,1:16], order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="Environmental correlations")

semPaths(fit, what = "std", whatLabels = 'std', style= "ram", layout= "spring", residuals= FALSE, 
         sizeInt =500, edge.label.cex = 1.5, node.width=2, node.height=2, label.cex= 3, curvePivot = TRUE)
##Multicomparison SEM----


nematodesc<-semd[which(semd$treatment=='control'), ]
group <- rep("control", times=length(nematodesc$treatment))
nematodesc <- cbind(nematodesc, group)

nematodesre<-semd[which(semd$treatment=='drought'), ]
group <- rep("RE", times=length(nematodesre$treatment))
nematodesre <- cbind(nematodesre, group)





nematodes_sem <- rbind (nematodesc, nematodesre)



###abndancia grupos troficos nematodos

### sem1. parece que si metemos soil moisture a fungivore los fitmeasures mejoran  un pelin

multigroup.model <- '
#regressions
#litter_quality = ~initial_cn #+ sla+thickness## esto seria para crear una variable latente?
#hervibores_number~Cmic18


hervibores_number~moisture_content
hervibores_number~mo
hervibores_number~RootSu_M18g
#hervibores_number~RootTa_M18g
#hervibores_number~pathogen2018



omnivores_number~moisture_content
omnivores_number~RootTa_M18g # si ponemos este y quitamos el de herbívoros sale mejor
omnivores_number~pathogen2018

bacterivores_number~moisture_content
#bacterivores_number~RootTa_M18g
bacterivores_number~Nmic18
bacterivores_number~pathogen2018


fungivores_number~Nmic18
fungivores_number~pathogen2018 
#fungivores_number~RootSt_M18g
#fungivores_number~moisture_content


predators_number~~fungivores_number
predators_number~pathogen2018
#predators_number~Nmic18

RootTa_M18g~pathogen2018
RootSu_M18g~pathogen2018
#RootSt_M18g~pathogen2018

#pathogen2018~moisture_content

#correlation
 


#intercept
'
multigroup <- sem(multigroup.model, nematodes_sem, group = "group") 

summary(multigroup, standardize=T)
varTable(multigroup)

fitMeasures(multigroup, c("cfi","rmsea","srmr", "pvalue"))


#this is to check that allowing coefficient to vary among groups improve our statistical fit

multigroup.constrained <- sem(multigroup.model, nematodes_sem, group = "group", group.equal = c("intercepts", "regressions"))

summary(multigroup.constrained)

anova(multigroup, multigroup.constrained)

semPaths(multigroup)


####sem2



multigroup.model <- '
#regressions
#litter_quality = ~initial_cn #+ sla+thickness## esto seria para crear una variable latente?
#hervibores_number~Cmic18



hervibores_number~moisture_content
hervibores_number~mo
hervibores_number~RootSu_M18g
hervibores_number~RootTa_M18g
hervibores_number~pathogen2018



omnivores_number~moisture_content
#omnivores_number~RootTa_M18g # si ponemos este y quitamos el de herbívoros sale mejor
#omnivores_number~pathogen2018

bacterivores_number~moisture_content
#bacterivores_number~RootTa_M18g
bacterivores_number~Nmic18
bacterivores_number~pathogen2018


fungivores_number~Nmic18
#fungivores_number~pathogen2018 
#fungivores_number~RootSt_M18g
#fungivores_number~moisture_content

predators_number~~fungivores_number
predators_number~pathogen2018
#predators_number~Nmic18

RootTa_M18g~pathogen2018
RootSu_M18g~pathogen2018
#RootSt_M18g~pathogen2018

#pathogen2018~moisture_content

#correlation
 

#intercept
'
multigroup <- sem(multigroup.model, nematodes_sem, group = "group") 

summary(multigroup, standardize=T)
varTable(multigroup)

fitMeasures(multigroup, c("cfi","rmsea","srmr", "pvalue"))


#this is to check that allowing coefficient to vary among groups improve our statistical fit

multigroup.constrained <- sem(multigroup.model, nematodes_sem, group = "group", group.equal = c("intercepts", "regressions"))

summary(multigroup.constrained)

anova(multigroup, multigroup.constrained)






###pongo el sem inicial en multigrupo####
semnormalmulti <- '
#regressions
#litter_quality = ~initial_cn #+ sla+thickness## esto seria para crear una variable latente?
#hervibores_number~Cmic18


hervibores_number~moisture_content
hervibores_number~mo
hervibores_number~RootSu_M18g
hervibores_number~RootTa_M18g

#omnivores_number~moisture_content
omnivores_number~RootTa_M18g
#omnivores_number~pathogen2018

#bacterivores_number~moisture_content
bacterivores_number~RootTa_M18g



fungivores_number~pathogen2018 
fungivores_number~RootSt_M18g
fungivores_number~moisture_content

predators_number~~fungivores_number
predators_number~pathogen2018

RootTa_M18g~pathogen2018
RootSt_M18g~pathogen2018

#correlation
 


#intercept
'


multigroup1 <- sem(semnormalmulti, nematodes_sem, group = "group") 

summary(multigroup1, standardize=T)
varTable(multigroup1)

fitMeasures(multigroup1, c("cfi","rmsea","srmr", "pvalue"))


#this is to check that allowing coefficient to vary among groups improve our statistical fit

multigroup.constrained1 <- sem(semnormalmulti, nematodes_sem, group = "group", group.equal = c("intercepts", "regressions"))

summary(multigroup.constrained1)

anova(multigroup1, multigroup.constrained1)

semPaths(multigroup1)

 ###sale peor que el modelo que habia hecho originalmente para el multigroup.  ###sale peor que el que creado directamente en multigrupo
####lo hacemos con porcentajes##### sale peor que con abundancia######


#library(grid)
getwd()
setwd("C:/Users/Usuario/Documents/nematodes")
#nematodes <- read.table("nematodes_intercapa.txt", header=T , sep="\t")
nematodes <- read.table("nematodes_18.txt", header=T , sep="\t")
nematodes <- nematodes[-which(nematodes$site=='Puechabon'),]

nematodes$tree<-as.factor(nematodes$tree)
nematodes$tree_sp<-as.factor(nematodes$tree_sp)
nematodes$treatment<-as.factor(nematodes$treatment)
nematodes$site<-as.factor(nematodes$site)
nematodes$block<-as.factor(nematodes$block)

compuestos<- read.table("compuestosecundarios.txt", header=T , sep="\t")
suelo<- read.table("datosuelo18.txt", header=T , sep="\t")
str(nematodes)
compuestosh <- (compuestos[,c(22,23,34,35,46,47)])### Select only the compounds sampled in 20018

compuestosr <- (compuestos[,c(16,17,28,29,40,41,52,53)])### Select only the compounds sampled in 20018

compuestos<-cbind(compuestosh,compuestosr)

suelo <- (suelo[,c(9:27)])### Select only the compounds sampled in 20018



corrgram (compuestosh, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
          diag.panel=panel.density, pch=16, lty=1, main="Environmental correlations")

corrgram (compuestosr, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
          diag.panel=panel.density, pch=16, lty=1, main="Environmental correlations")

corrgram (compuestos, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
          diag.panel=panel.density, pch=16, lty=1, main="Environmental correlations")

corrgram (suelo, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
          diag.panel=panel.density, pch=16, lty=1, main="Environmental correlations")


suelo <- (suelo[,c(1,2,5,6,8,9,11:15,17:19)])




factors <- (nematodes[,c(2,3,7:11)])
genus <- (nematodes[,c(13:147)])

omnpredpercen<-apply(nematodes[, c(171,173)],1,sum)
omnpred<-apply(nematodes[, c(192,194)],1,sum)
nematodes<-cbind(nematodes,omnpredpercen)
nematodes<-cbind(nematodes,omnpred)

trophic_n<-(nematodes[,c(190:194,211)])
trophic_p<-(nematodes[,c(166,167,169,171,173,210)])


semd<-cbind(factors,suelo)
semd<-cbind(semd,compuestos)
semd<-cbind(semd,trophic_p)

###transformamos variables
semd$moisture_content <- log(semd$moisture_content)
semd$mo<-(log(semd$mo))
semd$basal_area<-(sqrt(semd$basal_area))
semd$ph18<-(sqrt(semd$pH18))
semd$Nmic18<-(sqrt(semd$Nmic18))
semd$Cmic.Nmic18<-(sqrt(semd$Cmic.Nmic18))
semd$Cmic.Pmic18<-(sqrt(semd$Cmic.Pmic18))


semd$PBray18<-(log(semd$PBray18))

semd$Ureasa18<-(sqrt(semd$Ureasa18))

semd$pathogen2016<-(log(semd$pathogen2016))
semd$pathogen2017<-(log(semd$pathogen2017))
semd$pathogen2018<-(log(semd$pathogen2018))

semd$LeafTa_S18g<-(log(semd$LeafTa_S18g))

semd$LeafSt_S18g<-(log(semd$LeafSt_S18g))

semd$RootPh_S18g<-(sqrt(semd$RootPh_S18g))
semd$RootSt_M18g<-(log(semd$RootSt_M18g))
semd$RootSt_S18g<-(log10(semd$RootSt_S18g))

semd$RootSu_S18g<-(sqrt(semd$RootSu_S18g))

semd$RootTa_S18g<-(sqrt(semd$RootTa_S18g))

hist(log(semd$bacterivores_percentage))
hist(log(semd$fungivores_percentage))
hist(log(semd$herbivores_percentage))
hist(sqrt(semd$omnivores_percentage))
hist(sqrt(semd$predators_percen))
hist(sqrt(semd$omnpredpercen))

semd$bacterivores_percentage<-(log(semd$bacterivores_percentage))
semd$fungivores_percentage<-(log(semd$fungivores_percentage))
semd$herbivores_percentage<-(log(semd$herbivores_percentage))
semd$omnivores_percentage<-(sqrt(semd$omnivores_percentage))
semd$predators_percen<-(sqrt(semd$predators_percen))
semd$omnpredpercen<-(sqrt(semd$omnpredpercen))


semd$Cmic18<-(sqrt(semd$Cmic18))#### lo transformamos para que no haya tanta diferencia en magnitud con las otras variables
semd$Cmic18<-(log(semd$Cmic18))###primero raiz cuadrada y luego logaritmo
semd$NH4_18<-(sqrt(semd$NH4_18))
semd$Nmic18<-(log(semd$Nmic18))
semd$RootTa_M18g<-(log(semd$RootTa_M18g))
semd$RootSu_M18g<-(sqrt(semd$RootSu_M18g))


hist((semd$Cmic18))
hist((semd$NH4_18))
hist((semd$Nmic18))
hist((semd$RootTa_M18g))
hist((semd$RootSu_M18g))

nematodesc<-semd[which(semd$treatment=='control'), ]
group <- rep("control", times=length(nematodesc$treatment))
nematodesc <- cbind(nematodesc, group)

nematodesre<-semd[which(semd$treatment=='drought'), ]
group <- rep("RE", times=length(nematodesre$treatment))
nematodesre <- cbind(nematodesre, group)





nematodes_sem <- rbind (nematodesc, nematodesre)


###C Marrufo

multigroup.model <- '
#regressions
#litter_quality = ~initial_cn #+ sla+thickness## esto seria para crear una variable latente?
#hervibores_number~Cmic18


herbivores_percentage~moisture_content
#herbivores_percentage~mo
herbivores_percentage~RootSu_M18g
herbivores_percentage~RootTa_M18g

omnivores_percentage~moisture_content
#omnivores_percentage~RootTa_M18g
#omnivores_percentage~pathogen2018

bacterivores_percentage~moisture_content
bacterivores_percentage~RootTa_M18g



fungivores_percentage~pathogen2018 
fungivores_percentage~RootSt_M18g
#fungivores_percentage~moisture_content


#predators_percen~moisture_content

RootTa_M18g~pathogen2018
RootSu_M18g~pathogen2018

#correlation
 


#intercept
'
multigroup <- sem(multigroup.model, nematodes_sem, group = "group") 

summary(multigroup, standardize=T)
varTable(multigroup)

fitMeasures(multigroup, c("cfi","rmsea","srmr", "pvalue"))


#this is to check that allowing coefficient to vary among groups improve our statistical fit

multigroup.constrained <- sem(multigroup.model, nematodes_sem, group = "group", group.equal = c("intercepts", "regressions"))

summary(multigroup.constrained)

anova(multigroup, multigroup.constrained)

semPaths(multigroup)






 ##tambien sale peor ###sale peor que con abundancia. tendria que cambiar parámetros
####sem multigrupo usando filogenias. para ello metemos las abundancias por generos creando variable latente #####
##cargo los datos que voy a tener que cargar todos los generos

nematodes <- read.table("nematodes_18.txt", header=T , sep="\t")
nematodes <- nematodes[-which(nematodes$site=='Puechabon'),]

nematodes$tree<-as.factor(nematodes$tree)
nematodes$tree_sp<-as.factor(nematodes$tree_sp)
nematodes$treatment<-as.factor(nematodes$treatment)
nematodes$site<-as.factor(nematodes$site)
nematodes$block<-as.factor(nematodes$block)

compuestos<- read.table("compuestosecundarios.txt", header=T , sep="\t")
suelo<- read.table("datosuelo18.txt", header=T , sep="\t")
str(nematodes)
compuestosh <- (compuestos[,c(22,23,34,35,46,47)])### Select only the compounds sampled in 20018

compuestosr <- (compuestos[,c(16,17,28,29,40,41,52,53)])### Select only the compounds sampled in 20018

compuestos<-cbind(compuestosh,compuestosr)

suelo <- (suelo[,c(9:27)])### Select only the compounds sampled in 20018



suelo <- (suelo[,c(1,2,5,6,8,9,11:15,17:19)])




factors <- (nematodes[,c(2,3,7:11)])
genus <- (nematodes[,c(13:147)])




semd<-cbind(factors,suelo)
semd<-cbind(semd,compuestos)
semd<-cbind(semd,genus)

semd <- semd[, -c(40,75,77,80:82,92,93,95,97,105,125,126,130,136,144,149,160,164,168,170)]

semd$moisture_content <- log(semd$moisture_content)
semd$mo<-(log(semd$mo))
semd$basal_area<-(sqrt(semd$basal_area))
semd$ph18<-(sqrt(semd$pH18))
semd$Nmic18<-(sqrt(semd$Nmic18))
semd$Cmic.Nmic18<-(sqrt(semd$Cmic.Nmic18))
semd$Cmic.Pmic18<-(sqrt(semd$Cmic.Pmic18))


semd$PBray18<-(log(semd$PBray18))

semd$Ureasa18<-(sqrt(semd$Ureasa18))

semd$pathogen2016<-(log(semd$pathogen2016))
semd$pathogen2017<-(log(semd$pathogen2017))
semd$pathogen2018<-(log(semd$pathogen2018))

semd$LeafTa_S18g<-(log(semd$LeafTa_S18g))

semd$LeafSt_S18g<-(log(semd$LeafSt_S18g))

semd$RootPh_S18g<-(sqrt(semd$RootPh_S18g))
semd$RootSt_M18g<-(log(semd$RootSt_M18g))
semd$RootSt_S18g<-(log10(semd$RootSt_S18g))

semd$RootSu_S18g<-(sqrt(semd$RootSu_S18g))

semd$RootTa_S18g<-(sqrt(semd$RootTa_S18g))


semd$Cmic18<-(sqrt(semd$Cmic18))#### lo transformamos para que no haya tanta diferencia en magnitud con las otras variables
semd$Cmic18<-(log(semd$Cmic18))###primero raiz cuadrada y luego logaritmo
semd$NH4_18<-(sqrt(semd$NH4_18))
semd$Nmic18<-(log(semd$Nmic18))
semd$RootTa_M18g<-(log(semd$RootTa_M18g))
semd$RootSu_M18g<-(sqrt(semd$RootSu_M18g))


hist((semd$Cmic18))
hist((semd$NH4_18))
hist((semd$Nmic18))
hist((semd$RootTa_M18g))
hist((semd$RootSu_M18g))

nematodesc<-semd[which(semd$treatment=='control'), ]
group <- rep("control", times=length(nematodesc$treatment))
nematodesc <- cbind(nematodesc, group)

nematodesre<-semd[which(semd$treatment=='drought'), ]
group <- rep("RE", times=length(nematodesre$treatment))
nematodesre <- cbind(nematodesre, group)





nematodes_sem <- rbind (nematodesc, nematodesre)


multigroup.model <- '

hervibores_number = ~ Achromadora+Anguina+Axonchium+Basiria+Boleodorus+Cephalenchus+Coslenchus+Criconemella+Criconemoides+ecphyadophora+ Lelenchus+Longidorella+Longidorus+Malenchus+
                       Miculenchus+Neopsilenchus+Neothada+Ogma+Paratylenchus+Pratylenchus+Psilenchus+Tenunemellus+Trichodorus+Tylenchus
                       
                       
bacterivores_number= ~Acrobeles+ Acrobeloides+ Acrolobus+ Alaimus+Anaplectus+Bastiania+Bunonema+Bursilla+Cephalobus+Cervidellus+Chiloplacus+Chronogaster+Dauer.larva+Drilocephalobus+ 
                      Eucephalobus+Eumonhystera.+Geomonhystera+Heterocephalobus+Leptolaimus+Mesorhabditis+Metateratocephalus+Microlaimus+Monhystera+ Monhystrella+Odontolaymus+Panagrolaimus+Paramphidelus+Pellioditis+Plectus+Plectonchus+
                      Prismatolaimus+Prodesmodora+Rhabdolaimus+ Sterneinema+Teratocephalus+Theristus+Turbatrix+Tylocephalus+Wilsonema

fungivores_number=~ Aphelenchoides+ Aphelenchus+Aprutides+Bursaphelenchus+Diphtherophora+Ditylenchus+Filenchus+Fungtionchun+Leptonchus+Nothotylenchus+Paraphelenchus+Pseudhalenchus+Tylencholaimellus+Tylencholaimus+tylolaimophorus

omnivores_number=~  Aporcelaimellus+Aporcelaimium+ Aporcelaimus+ Chromadorita+ Epidorylaimus+ Eudorylaimus+Microdorylaimus+Paraxonchium+Pungentus+Sectonema+Thornia+Thonus

#predators_number=~ Anatonchus+Clarkus+Coomansus+Discolaimium+Miconchus+Mylonchulus+Nygolaimus+Prionchulus+Seinura+Stenonchulus+Tobrilus+Trischistoma+Tripyla

#regressions

hervibores_number~moisture_content
hervibores_number~mo
hervibores_number~RootSu_M18g
hervibores_number~RootTa_M18g

omnivores_number~moisture_content
omnivores_number~RootTa_M18g
omnivores_number~pathogen2018

bacterivores_number~moisture_content
bacterivores_number~RootTa_M18g



fungivores_number~pathogen2018 
fungivores_number~RootSt_M18g
fungivores_number~moisture_content


RootTa_M18g~pathogen2018
RootSu_M18g~pathogen2018
RootSt_M18g~pathogen2018

#correlation
 


#intercept
'




multigroup <- sem(multigroup.model, nematodes_sem, group = "group") 

summary(multigroup, standardize=T)
varTable(multigroup)

fitMeasures(multigroup, c("cfi","rmsea","srmr", "pvalue"))


#this is to check that allowing coefficient to vary among groups improve our statistical fit

multigroup.constrained <- sem(multigroup.model, nematodes_sem, group = "group", group.equal = c("intercepts", "regressions"))

summary(multigroup.constrained)

anova(multigroup, multigroup.constrained)

semPaths(multigroup)


#####Con familias en vez de generos#####


nematodes <- read.table("nematodes_18.txt", header=T , sep="\t")
nematodes <- nematodes[-which(nematodes$site=='Puechabon'),]

nematodes$tree<-as.factor(nematodes$tree)
nematodes$tree_sp<-as.factor(nematodes$tree_sp)
nematodes$treatment<-as.factor(nematodes$treatment)
nematodes$site<-as.factor(nematodes$site)
nematodes$block<-as.factor(nematodes$block)

compuestos<- read.table("compuestosecundarios.txt", header=T , sep="\t")
suelo<- read.table("datosuelo18.txt", header=T , sep="\t")
str(nematodes)
compuestosh <- (compuestos[,c(22,23,34,35,46,47)])### Select only the compounds sampled in 20018

compuestosr <- (compuestos[,c(16,17,28,29,40,41,52,53)])### Select only the compounds sampled in 20018

compuestos<-cbind(compuestosh,compuestosr)

suelo <- (suelo[,c(9:27)])### Select only the compounds sampled in 20018



suelo <- (suelo[,c(1,2,5,6,8,9,11:15,17:19)])




factors <- (nematodes[,c(2,3,7:11)])
genus <- (nematodes[,c(13:147)])

Achromadoridae <- (genus[,c(1)])
Alaimidae<-apply(genus[, c(6,88)],1,sum)
Anatonchidae<-apply(genus[, c(9,75)],1,sum)
Anguinidae<-apply(genus[, c(10,44,82,104)],1,sum)

Bathyodontidae<-(genus[, c(36)])

Aphelenchidae<-apply(genus[, c(12,89)],1,sum)
Aphelenchoididae<-apply(genus[, c(11,16,22,111)],1,sum)
Seinura<-(genus[, c(111)])###sseinura 111 es predator aunque es de aphelenchoididae


Aporcelaimidae<-apply(genus[, c(13,14,15,93,110)],1,sum)
Bastianiidae<-apply(genus[, c(19,40)],1,sum)
Bathyodontidae<-(genus[, c(36)])
Belondiridae<-(genus[, c(17)])
Brevibuccidae<-(genus[, c(97)])
Bunonematidae<-(genus[, c(21)])


Cephalobidae<-apply(genus[, c(2,3,4,25,26,27,51,59)],1,sum)
Criconematidae<-apply(genus[, c(33,34,35,84)],1,sum)

Desmodoridae<-(genus[, c(102)])
Diphtherophoridae<-apply(genus[, c(41,132)],1,sum)

Diplopeltidae<-(genus[, c(37)])
Discolaimidae<-(genus[, c(43)])
Dolichodoridae<-(genus[, c(69)])

Ecphyadophoridae<-apply(genus[, c(49,115)],1,sum)

Hoplolaimidae<-(genus[, c(90)])
Hypodontolaimidae<-(genus[, c(28)])
Iotonchidae<-(genus[, c(55)])
Ironidae<-(genus[, c(61)])


Leptolaimidae<-apply(genus[, c(29,64)],1,sum)
Leptonchidae<-apply(genus[, c(65,127,128)],1,sum)

Longidoridae<-(genus[, c(67)])
Microlaimidae<-(genus[, c(74)])


Monhysteridae<-apply(genus[, c(53,56,77,78)],1,sum)
Mononchidae<-apply(genus[, c(30,31,79,99)],1,sum)
Nordiidae<-apply(genus[, c(106,119)],1,sum)## omnivoros
Longidorella<-(genus[, c(66)])##longidorella es nordidae pero es herrviboro

Nygolaimidae<-(genus[, c(83)])
Odontolaimidae<-(genus[, c(85)])

Onchulidae<-apply(genus[, c(86,112)],1,sum)

Ostellidae<-(genus[, c(48)])

Panagrolaimidae<-apply(genus[, c(87,126)],1,sum)

Paratylenchidae<-(genus[, c(92)])

Plectidae<-apply(genus[, c(8,96,131,134)],1,sum)

Pratylenchidae<-(genus[, c(98)])
Prismatolaimidae<-(genus[, c(100)])
Psilenchidae<-(genus[, c(105)])

Quadsinematidae<-apply(genus[, c(7,50,52,73,120)],1,sum)
Rhabditidae<-apply(genus[, c(23,39,71,94,107)],1,sum)

Rhabdolaimidae<-(genus[, c(108)])
Steinernematidae<-(genus[, c(113)])

Teratocephalidae<-apply(genus[, c(72,116)],1,sum)

Thornematidae<-(genus[, c(103)])
Tobrilidae<-(genus[, c(121)])
Trichodoridae<-(genus[, c(123)])

Tripylidae<-apply(genus[, c(124,122)],1,sum)
Tylenchidae<-apply(genus[, c(18,20,32,63,68,76,80,81,130)],1,sum) 

Filenchus<-(genus[, c(54)])##filenchus (54) es fungivoro, tylenchidae herbivoros

Tylodoridae<-(genus[, c(24)])

Xyalidae<-apply(genus[, c(38,118)],1,sum)




semd<-cbind(factors,suelo)
semd<-cbind(semd,compuestos)



###familias que saco por tener muy pocos individuos Psilenchidae,Trichodoridae,Discolaimidae,Hypodontolaimidae,Diplopeltidae,Ironidae,Bathyodontidae,Hoplolaimidae
#familia que tambien saco aunque son un pelin mas numerosas Tylodoridae,Panagrolaimidae,Iotonchidae,Ostellidae


semd<-cbind(semd,Achromadoridae,Alaimidae,Anatonchidae,Anguinidae,Aphelenchidae,Aphelenchoididae,
            Aporcelaimidae,Bastianiidae,Belondiridae,Brevibuccidae,Bunonematidae,Cephalobidae,
            Criconematidae,Desmodoridae,Diphtherophoridae,Dolichodoridae,
            Ecphyadophoridae,Leptolaimidae,Leptonchidae,
            Longidoridae,Microlaimidae,Monhysteridae,Mononchidae,Nordiidae,Nygolaimidae,Odontolaimidae,Onchulidae,
            Paratylenchidae,Plectidae,Pratylenchidae,Prismatolaimidae,
            Quadsinematidae,Rhabditidae,Rhabdolaimidae,Steinernematidae,Teratocephalidae,Thornematidae,
            Tobrilidae,Tripylidae,Tylenchidae,Xyalidae, Filenchus,Longidorella)





semd$moisture_content <- log(semd$moisture_content)
semd$mo<-(log(semd$mo))
semd$basal_area<-(sqrt(semd$basal_area))
semd$ph18<-(sqrt(semd$pH18))
semd$Nmic18<-(sqrt(semd$Nmic18))
semd$Cmic.Nmic18<-(sqrt(semd$Cmic.Nmic18))
semd$Cmic.Pmic18<-(sqrt(semd$Cmic.Pmic18))


semd$PBray18<-(log(semd$PBray18))

semd$Ureasa18<-(sqrt(semd$Ureasa18))

semd$pathogen2016<-(log(semd$pathogen2016))
semd$pathogen2017<-(log(semd$pathogen2017))
semd$pathogen2018<-(log(semd$pathogen2018))

semd$LeafTa_S18g<-(log(semd$LeafTa_S18g))

semd$LeafSt_S18g<-(log(semd$LeafSt_S18g))

semd$RootPh_S18g<-(sqrt(semd$RootPh_S18g))
semd$RootSt_M18g<-(log(semd$RootSt_M18g))
semd$RootSt_S18g<-(log10(semd$RootSt_S18g))

semd$RootSu_S18g<-(sqrt(semd$RootSu_S18g))

semd$RootTa_S18g<-(sqrt(semd$RootTa_S18g))


semd$Cmic18<-(sqrt(semd$Cmic18))#### lo transformamos para que no haya tanta diferencia en magnitud con las otras variables
semd$Cmic18<-(log(semd$Cmic18))###primero raiz cuadrada y luego logaritmo
semd$NH4_18<-(sqrt(semd$NH4_18))
semd$Nmic18<-(log(semd$Nmic18))
semd$RootTa_M18g<-(log(semd$RootTa_M18g))
semd$RootSu_M18g<-(sqrt(semd$RootSu_M18g))


hist((semd$Cmic18))
hist((semd$NH4_18))
hist((semd$Nmic18))
hist((semd$RootTa_M18g))
hist((semd$RootSu_M18g))

nematodesc<-semd[which(semd$treatment=='control'), ]
group <- rep("control", times=length(nematodesc$treatment))
nematodesc <- cbind(nematodesc, group)

nematodesre<-semd[which(semd$treatment=='drought'), ]
group <- rep("RE", times=length(nematodesre$treatment))
nematodesre <- cbind(nematodesre, group)

nematodes_sem <- rbind (nematodesc, nematodesre)




multigroup.model <- '

hervibores_number = ~ Achromadoridae+Belondiridae+Criconematidae+Dolichodoridae+Ecphyadophoridae++Longidoridae+Paratylenchidae+Pratylenchidae+Tylenchidae
              
              ##familias de herviboros eliminadaspor ser escasas Hoplolaimidae+Tylodoridae+Trichodoridae+Psilenchidae
                       
bacterivores_number= ~Alaimidae+ Bastianiidae+Brevibuccidae+Bunonematidae+Cephalobidae+Desmodoridae+Leptolaimidae+Microlaimidae+Monhysteridae+
                       Odontolaimidae+Plectidae+Prismatolaimidae+Rhabditidae+Steinernematidae+Teratocephalidae+Xyalidae+Longidorella

  ##familias de bacterivoros eliminadas por escasas Panagrolaimidae+ Ostellidae+Bathyodontidae +Diplopeltidae+


fungivores_number= ~Anguinidae+Aphelenchidae+Aphelenchoididae+Diphtherophoridae+Leptonchidae+Filenchus

##fungivoros eliminados por escasos Iotonchidae+


omnivores_number= ~Aporcelaimidae+Nordiidae+Quadsinematidae+Thornematidae  

##familias eliminadas de herbivoros por ser poco abundantes Hypodontolaimidae


#predators_number=  ~Anatonchidae+Monochidae+Nygolaimidae+Onchulidae+Tobrilidae+Tripylidae
  
   ###eliminados por escasos Ironidae+Discolaimidae

#regressions



hervibores_number~moisture_content
hervibores_number~mo
hervibores_number~RootSu_M18g
hervibores_number~RootTa_M18g

omnivores_number~moisture_content
omnivores_number~RootTa_M18g
omnivores_number~pathogen2018

bacterivores_number~moisture_content
bacterivores_number~RootTa_M18g



fungivores_number~pathogen2018 
fungivores_number~RootSt_M18g
fungivores_number~moisture_content


RootTa_M18g~pathogen2018
RootSu_M18g~pathogen2018
RootSt_M18g~pathogen2018

#correlation
 


#intercept
'


#fit <- sem(multigroup.model, data=semd, fixed.x=FALSE)
#summary(fit, standardized=TRUE)
#this function is to obtain the fit form the model see http://lavaan.ugent.be/tutorial/tutorial.pdf
#fitMeasures(fit, c("cfi","rmsea","srmr", "pvalue"))

corrgram(semd[,66:93], order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="Environmental correlations")


multigroup <- sem(multigroup.model, nematodes_sem, group = "group") 

summary(multigroup, standardize=T)
varTable(multigroup)

fitMeasures(multigroup, c("cfi","rmsea","srmr", "pvalue"))


#this is to check that allowing coefficient to vary among groups improve our statistical fit

multigroup.constrained <- sem(multigroup.model, nematodes_sem, group = "group", group.equal = c("intercepts", "regressions"))

summary(multigroup.constrained)

anova(multigroup, multigroup.constrained)

semPaths(multigroup)


