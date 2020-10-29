library(bestNormalize)
library(nlme)
library(tidyverse)
library(ggplot2)
library(plotly)
library(sjPlot)
library(MuMIn)
library(vegan)
library(Rmisc)

library (lme4)
getwd()
setwd("C:/Users/Usuario/Documents/nematodes")
#nematodes <- read.table("nematodes_intercapa.txt", header=T , sep="\t")
nematodes <- read.table("nematodes_18.txt", header=T , sep="\t")

str(nematodes)




nematodes$block<-as.factor(nematodes$block)
nematodes$block2<-as.factor(nematodes$block2)
nematodes$tree<-as.factor(nematodes$tree)
nematodes$tree_sp<-as.factor(nematodes$tree_sp)
nematodes$treatment<-as.factor(nematodes$treatment)
nematodes$site<-as.factor(nematodes$site)
nematodes$Dintheria<-as.numeric(nematodes$Dintheria)
nematodes$Hemicycliophora<-as.numeric(nematodes$Hemicycliophora)

factors <- (nematodes[,c(1:11)])
sp <- (nematodes[,c(13:147)])



richness<-apply(sp[,]>0,1,sum)


shannong<- diversity(sp, index = "shannon", MARGIN = 1, base = exp(1))

sp<-cbind(sp,shannong)
sp<-cbind(sp,richness) 



spg<-cbind(factors,sp)

hist((spg$richness))
hist((spg$shannong))

#omnpred<-apply(nematodes[c(,170,172)]>0,1,sum)

omnpredpercen<-apply(nematodes[, c(171,173)],1,sum)
omnpred<-apply(nematodes[, c(192,194)],1,sum)
nematodes<-cbind(nematodes,omnpredpercen)
nematodes<-cbind(nematodes,omnpred)

hist((nematodes$mo))
hist(log(nematodes$moisture_content))
hist(nematodes$moisture_content)
hist(log(nematodes$nematodes_gram))
hist(log(nematodes$plant_parasitic_index))
hist(nematodes$structure_index)
hist(log(nematodes$f_b_ratio))
hist(log(nematodes$maturity_index))
hist(nematodes$enrichment_index)
hist(log(nematodes$bacterivores_number))
hist((nematodes$bacterivores_percentage))
hist(log(nematodes$fungivores_number))
hist(nematodes$fungivores_percentage)
hist(log(nematodes$herbivores_percentage))
hist(log(nematodes$hervibores_number))
hist((nematodes$predators_number))
hist((nematodes$predators_percen))
hist(log(nematodes$omnivores_number))
hist(sqrt(nematodes$omnivores_percentage))
hist(log(nematodes$mo))
hist(log(nematodes$prey_predator))
hist(log(nematodes$omnpredpercen))
hist(log(nematodes$omnpred))
hist((nematodes$moisture_content))

nematodes$nematodes_gram<-log(nematodes$nematodes_gram)
nematodes$maturity_index<-log(nematodes$maturity_index)
nematodes$plant_parasitic_index<-log(nematodes$plant_parasitic_index)
nematodes$f_b_ratio<-log(nematodes$f_b_ratio)
nematodes$bacterivores_number<-log(nematodes$bacterivores_number)
nematodes$fungivores_number<-log(nematodes$fungivores_number)
nematodes$hervibores_number<-log(nematodes$hervibores_number)
nematodes$herbivores_percentage<-log(nematodes$herbivores_percentage)
#nematodes$predators_number<-sqrt(nematodes$predators_number)
#nematodes$predators_percen<-sqrt(nematodes$predators_percen)
nematodes$omnivores_number<-log(nematodes$omnivores_number)
nematodes$omnivores_percentage<-sqrt(nematodes$omnivores_percentage)
nematodes$prey_predator<-log(nematodes$prey_predator)
nematodes$mo<-log(nematodes$mo)
nematodes$omnpredpercen<-log(nematodes$omnpredpercen)
nematodes$omnpred<-log(nematodes$omnpred)

#nematodes <- nematodes[-which(nematodes$site=="Puechabon"),]
#nematodes <- nematodes[-which(nematodes$site=="Alcornocales"),]
###abundance####
hist((nematodes$nematodes_gram))

tablaabundance<- summarySE(nematodes, measurevar="nematodes_gram", groupvars=c("treatment", "site"))

nematodes_abundance<-ggplot(tablaabundance, aes(x=site, y=nematodes_gram, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=nematodes_gram-se, ymax=nematodes_gram+se),
                width=.2,position=position_dodge(.9))+
  
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "Nematodes abundance",size=30)
#+
  theme(legend.position="none")



ggplotly(nematodes_abundance)
options(na.action = "na.fail")


abundance_model2 <- lme(nematodes_gram ~site*treatment*mo*moisture_content+block%in%site ,
                      data = nematodes, random=~ 1|tree)

#ms2 <- dredge(abundance_model2, trace = TRUE, rank = "AICc", REML = FALSE)
#(attr(ms2, "rank.call"))
#par(mar = c(3,5,6,4))
#plot(ms2, labAsExpr = TRUE)
#model.avg(ms2, subset = delta < 4)
#confset.95p <- get.models(ms2, cumsum(weight) <= .95)
#avgmod.95p <- model.avg(confset.95p)
#summary(avgmod.95p)
#confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
#fmList <- get.models(ms2, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
#summary(model.avg(fmList))
#model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))



ms2 <- dredge(abundance_model2)
par(mar = c(3,5,6,4))
plot(ms2, labAsExpr = TRUE)
model.avg(ms2, subset = delta < 4)
confset.95p <- get.models(ms2, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

abundance_model <- lme(nematodes_gram ~site*treatment+block %in% site ,
                       data = nematodes, random=~ 1|tree)


#abundance_model2 <- lm(nematodes_gram ~site*treatment+block %in% site ,
 #                      data = nematodes)

summary(abundance_model)

summary(abundance_model2)


AIC( abundance_model)
r.squaredGLMM(abundance_model)

r.squaredGLMM(abundance_model)

anova(abundance_model)
anova(abundance_model2)


b <- ggplot(nematodes, aes(x = mo, y = nematodes_gram,colour=site))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 


####richness#####
hist(spg$richness)
tablarichness<- summarySE(spg, measurevar="richness", groupvars=c("treatment", "site"))


richness<-ggplot(tablarichness, aes(x=site, y=richness, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=richness-se, ymax=richness+se),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Treatment", y= "richness",size=30)+
  theme(legend.position = "none")
ggplotly(richness)


richness_model2 <- lme(richness ~site*treatment*mo*moisture_content+block%in% site ,
                        data = spg, random=~ 1|tree)
ms3 <- dredge(richness_model2)
par(mar = c(3,5,6,4))
plot(ms3, labAsExpr = TRUE)
model.avg(ms2, subset = delta < 4)
confset.95p <- get.models(ms3, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

richness_model <- lme(richness ~site*treatment+block%in%site ,
                       data = spg, random=~ 1|tree)

summary(richness_model)
AIC(richness_model2)
r.squaredGLMM(richness_model)
anova(richness_model)

richness_model <- lm(richness ~site*treatment+block%in%site ,
                      data = spg)


####diversity########  
###genus
hist(sqrt(spg$shannong))
tabladiversity<- summarySE(spg, measurevar="shannong", groupvars=c("treatment", "site"))


shannong<-ggplot(tabladiversity, aes(x=site, y=shannong, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=shannong-sd, ymax=shannong+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Treatment", y= "Shannon index",size=30)+
  theme(legend.position = "none")

ggplotly(shannong)



shannon_model2 <- lme(sqrt(shannong) ~site*treatment*mo*moisture_content+block%in%site ,
                       data = spg, random=~ 1|tree)
ms3 <- dredge(shannon_model2)
par(mar = c(3,5,6,4))
plot(ms3, labAsExpr = TRUE)
model.avg(ms3, subset = delta < 4)
confset.95p <- get.models(ms3, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

shannon_model <- lme(sqrt(shannong) ~site*treatment+block%in%site ,
                      data = spg, random=~ 1|tree)

summary(shannon_model)
AIC(shannon_model)
r.squaredGLMM(shannon_model)
anova(shannon_model)

b <- ggplot(spg, aes(x = moisture_content, y = shannong, colour=treatment))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 

####maturity index#####
hist(nematodes$maturity_index)
tablamaturity<- summarySE(nematodes, measurevar="maturity_index", groupvars=c("treatment", "site"))


maturity_index<-ggplot(tablamaturity, aes(x=site, y=maturity_index, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=maturity_index-sd, ymax=maturity_index+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "Maturity index",size=30)+
  theme(legend.position="none")

ggplotly(maturity_index)

MI2model <- lme(maturity_index ~site*treatment*mo*moisture_content+block%in%site ,
                      data = nematodes, random=~ 1|tree)
ms4 <- dredge(MI2model)
par(mar = c(3,5,6,4))
plot(ms4, labAsExpr = TRUE)
model.avg(ms4, subset = delta < 4)
confset.95p <- get.models(ms4, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

MImodel <- lme(maturity_index ~site*treatment+block%in%site ,
                     data = nematodes, random=~ 1|tree)

summary(MImodel)
AIC(MImodel)
r.squaredGLMM(MImodel)
anova(MImodel)


###plant parasitic index####
hist(nematodes$plant_parasitic_index)
tablappi<- summarySE(nematodes, measurevar="plant_parasitic_index", groupvars=c("treatment", "site"))


plant_parasitic_index<-ggplot(tablappi, aes(x=site, y=plant_parasitic_index, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=plant_parasitic_index-sd, ymax=plant_parasitic_index+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "Plant parasitic index",size=30)+
  theme(legend.position="none")

ggplotly(plant_parasitic_index)

ppi2model <- lme(plant_parasitic_index ~site*treatment*mo*moisture_content+block%in%site ,
                data = nematodes, random=~ 1|tree)
ms5 <- dredge(ppi2model)
par(mar = c(3,5,6,4))
plot(ms5, labAsExpr = TRUE)
model.avg(ms5, subset = delta < 4)
confset.95p <- get.models(ms5, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

ppimodel <- lme(plant_parasitic_index ~site*treatment+block%in%site ,
               data = nematodes, random=~ 1|tree)

summary(ppimodel)
AIC(ppimodel)
r.squaredGLMM(ppimodel)
anova(ppimodel)
####structure index#####
hist((nematodes$structure_index))
tablasi<- summarySE(nematodes, measurevar="structure_index", groupvars=c("treatment", "site"))



structure_index<-ggplot(tablasi, aes(x=site, y=structure_index, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=structure_index-sd, ymax=structure_index+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "Structure Index",size=30)+
  theme(legend.position="none")



ggplotly(structure_index)
options(na.action = "na.fail")


structure_idex_modell2 <- lme(structure_index ~site*treatment*mo*moisture_content+block%in%site ,
                        data = nematodes, random=~ 1|tree)

ms6 <- dredge(structure_idex_modell2)
par(mar = c(3,5,6,4))
plot(ms6, labAsExpr = TRUE)
model.avg(ms6, subset = delta < 4)
confset.95p <- get.models(ms6, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

structure_idex_model <- lme(structure_index ~site*treatment*mo+block%in%site ,
                       data = nematodes, random=~ 1|tree)

summary(structure_idex_model)
AIC(structure_idex_model)
r.squaredGLMM(structure_idex_model)
anova(structure_idex_model)

b <- ggplot(nematodes, aes(x = structure_index, y = mo, colours=site))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 


#####enrichment index#####
hist((nematodes$enrichment_index))
tablaenrichment<- summarySE(nematodes, measurevar="enrichment_index", groupvars=c("treatment", "site"))


enrichment_index<-ggplot(tablaenrichment, aes(x=site, y=enrichment_index, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=enrichment_index-sd, ymax=enrichment_index+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "Enrichment Index",size=30)+
  theme(legend.position="none")



ggplotly(enrichment_index)
options(na.action = "na.fail")


enrichment_idex_modell2 <- lme(enrichment_index ~site*treatment*mo*moisture_content+block%in%site ,
                              data = nematodes, random=~ 1|tree)

ms7 <- dredge(enrichment_idex_modell2)
par(mar = c(3,5,6,4))
plot(ms7, labAsExpr = TRUE)
model.avg(ms7, subset = delta < 4)
confset.95p <- get.models(ms7, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

enrichment_idex_model <- lme(enrichment_index ~site*treatment*mo+block%in%site ,
                            data = nematodes, random=~ 1|tree)

summary(enrichment_idex_model)
AIC(enrichment_idex_model)
r.squaredGLMM(enrichment_idex_model)
anova(enrichment_idex_model)

b <- ggplot(nematodes, aes(x = enrichment_index, y = mo))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 




####f/b ratio######
hist((nematodes$f_b_ratio))

tablafb<- summarySE(nematodes, measurevar="f_b_ratio", groupvars=c("treatment", "site"))



f_b_ratio<-ggplot(tablafb, aes(x=site, y=f_b_ratio, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=f_b_ratio-sd, ymax=f_b_ratio+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "Fungiv./Bacter.",size=30)+
  theme(legend.position = "none")

ggplotly(f_b_ratio)


hist((nematodes$f_b_ratio))




#fbmodel <- lme(f_b_ratio ~treatment*mo*moisture_content+block ,
 #              data = nematodes, random=~ 1|tree)


fbmodel <- lme(f_b_ratio ~treatment*site*mo*moisture_content+block%in%site ,
                        data = nematodes, random=~ 1|tree)


ms8 <- dredge(fbmodel)
par(mar = c(3,5,6,4))
plot(ms8, labAsExpr = TRUE)
model.avg(ms8, subset = delta < 4)
confset.95p <- get.models(ms8, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

fbmodel1 <- lme(f_b_ratio ~site*treatment*moisture_content+block%in%site ,
                               data = nematodes, random=~ 1|tree)

#fbmodel1 <- lme(f_b_ratio ~treatment*mo+block ,
 #               data = nematodes, random=~ 1|tree)

summary(fbmodel1)
AIC(fbmodel1)
r.squaredGLMM(fbmodel1)
anova(fbmodel1)

b <- ggplot(nematodes, aes(x = f_b_ratio, y = moisture_content, colour=treatment))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 



####???bacterivores####
hist(nematodes$bacterivores_number)
hist(nematodes$bacterivores_percentage)


tablab<- summarySE(nematodes, measurevar="bacterivores_number", groupvars=c("treatment", "site"))



bacterivores_number<-ggplot(tablab, aes(x=site, y=bacterivores_number, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=bacterivores_number-sd, ymax=bacterivores_number+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "bacterivores number",size=30)+
  theme(legend.position = "none")+geom_boxplot()
ggplotly(bacterivores_number)


bacterivoresmodel <- lme(bacterivores_number ~treatment*site*mo*moisture_content+block%in%site ,
               data = nematodes, random=~ 1|tree)


ms9 <- dredge(bacterivoresmodel)
par(mar = c(3,5,6,4))
plot(ms9, labAsExpr = TRUE)
model.avg(ms9, subset = delta < 4)
confset.95p <- get.models(ms9, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

bacterivoresmodel1 <- lme(bacterivores_number ~site*treatment+block%in%site ,
                data = nematodes, random=~ 1|tree)

bacterivoresmodel1 <- lme(bacterivores_number ~treatment+block ,
                          data = nematodes, random=~ 1|tree)



summary(bacterivoresmodel1)
AIC(bacterivoresmodel1)
r.squaredGLMM(bacterivoresmodel1)
anova(bacterivoresmodel1)

b <- ggplot(nematodes, aes(x = bacterivores_number, y = moisture_content, colour=treatment))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 


tablabper<- summarySE(nematodes, measurevar="bacterivores_percentage", groupvars=c("treatment", "site"))


bacterivores_percen<-ggplot(tablabper, aes(x=site, y=bacterivores_percentage, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=bacterivores_percentage-se, ymax=bacterivores_percentage+se),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "bacterivores %",size=30)+
  theme(legend.position = "none")



bacterivoresmodelperce <- lme(bacterivores_percentage ~treatment*site*mo*moisture_content+block%in%site ,
                         data = nematodes, random=~ 1|tree)

#bacterivoresmodelperce <- lme(bacterivores_percentage ~treatment*mo*moisture_content+block ,
 #                             data = nematodes, random=~ 1|tree)

msbacte <- dredge(bacterivoresmodelperce, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(msbacte, "rank.call"))
par(mar = c(3,5,6,4))
plot(msbacte, labAsExpr = TRUE)
model.avg(msbacte, subset = delta < 4)
confset.95p <- get.models(msbacte, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
fmList <- get.models(msfungi, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
summary(model.avg(fmList))
model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))


bacterivoresmodel1perce <- lme(bacterivores_percentage ~site*treatment+mo*moisture_content+moisture_content*treatment+block%in%site ,
                          data = nematodes,random=~ 1|tree)

#bacterivoresmodel1perce <- lme(bacterivores_percentage ~treatment*mo*moisture_content+block ,
 #                             data = nematodes,random=~ 1|tree)

summary(bacterivoresmodel1perce)
AIC(bacterivoresmodel1perce)
r.squaredGLMM(bacterivoresmodel1perce)
anova(bacterivoresmodel1perce)

b <- ggplot(nematodes, aes(x = bacterivores_percentage, y = moisture_content, colour=treatment))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 

hist(nematodes$bacterivores_percentage)

#####fungivores####
tablaf<- summarySE(nematodes, measurevar="fungivores_number", groupvars=c("treatment", "site"))

fungivores_number<-ggplot(tablaf, aes(x=site, y=fungivores_number, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=fungivores_number-sd, ymax=fungivores_number+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "fungivores number",size=30)+
  theme(legend.position = "none")

ggplotly(fungivores_number)

hist((nematodes$fungivores_number))
hist((nematodes$fungivores_percentage))




fungivoresmodel <- lm((fungivores_number) ~treatment*site*mo*moisture_content+block%in%site ,
               data = nematodes)



msfungi <- dredge(fungivoresmodel, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(msfungi, "rank.call"))
par(mar = c(3,5,6,4))
plot(msfungi, labAsExpr = TRUE)
model.avg(msfungi, subset = delta < 4)
confset.95p <- get.models(msfungi, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)

confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
fmList <- get.models(msfungi, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
summary(model.avg(fmList))
model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))



fungivoresmodel <- lm((fungivores_number) ~treatment*site+block%in%site ,
                       data = nematodes)

#fungivoresmodel <- lm((fungivores_number) ~treatment*mo*moisture_content+block ,
 #                     data = nematodes)

summary(fungivoresmodel)
AIC(fungivoresmodel)
r.squaredGLMM(fungivoresmodel)
anova(fungivoresmodel)
b <- ggplot(nematodes, aes(x = fungivores_number, y = moisture_content, colour=site))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 

#####fungivores percentage#####
tablafp<- summarySE(nematodes, measurevar="fungivores_percentage", groupvars=c("treatment", "site"))


fungivores_percen<-ggplot(tablafp, aes(x=site, y=fungivores_percentage, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=fungivores_percentage-sd, ymax=fungivores_percentage+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "fungivores %",size=30)+
  theme(legend.position = "none")

ggplotly(fungivores_percen)


hist((nematodes$fungivores_percentage))





fungivoresmodelp1 <- lme((fungivores_percentage) ~treatment*site*mo*moisture_content+block%in%site ,
                       data = nematodes, random=~ 1|tree)

msfungip <- dredge(fungivoresmodelp1, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(msfungip, "rank.call"))
par(mar = c(3,5,6,4))
plot(msfungip, labAsExpr = TRUE)
model.avg(msfungip, subset = delta < 4)
confset.95p <- get.models(msfungip, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
fmList <- get.models(msfungi, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
summary(model.avg(fmList))
model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))


fungivoresmodelp <- lme((fungivores_percentage) ~treatment*site+block %in% site ,
                       data = nematodes, random=~ 1|tree)
AIC(fungivoresmodelp)
r.squaredGLMM(fungivoresmodelp)
summary(fungivoresmodelp)
anova(fungivoresmodelp)


#####herbivores#####
tablafh<- summarySE(nematodes, measurevar="hervibores_number", groupvars=c("treatment", "site"))


herbivores_number<-ggplot(tablafh, aes(x=site, y=hervibores_number, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=hervibores_number-sd, ymax=hervibores_number+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "herbivores number",size=30)+
  theme(legend.position = "none")



herbivoresmodel <- lm((hervibores_number) ~treatment*site*mo*moisture_content+block%in%site ,
                       data = nematodes)

mshervi <- dredge(herbivoresmodel, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(mshervi, "rank.call"))
par(mar = c(3,5,6,4))
plot(msfungi, labAsExpr = TRUE)
model.avg(mshervi, subset = delta < 4)
confset.95p <- get.models(mshervi, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
fmList <- get.models(mshervi, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
summary(model.avg(fmList))
model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))

ggplotly(herbivores_number)


herbivoresmodel <- lme((hervibores_number) ~treatment*site+moisture_content+mo+block%in%site ,
                       data = nematodes,random=~ 1|tree)

herbivoresmodel <- lme((hervibores_number) ~treatment+moisture_content+mo+block,
                       data = nematodes,random=~ 1|tree)

AIC(herbivoresmodel)
r.squaredGLMM(herbivoresmodel)
summary(herbivoresmodel)
anova(herbivoresmodel)

b <- ggplot(nematodes, aes(x = hervibores_number, y = mo))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 

#####herbivores percentage######
tablahp<- summarySE(nematodes, measurevar="herbivores_percentage", groupvars=c("treatment", "site"))

herbivores_percen<-ggplot(tablahp, aes(x=site, y=herbivores_percentage, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=herbivores_percentage-se, ymax=herbivores_percentage+se),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "herbivores %",size=30)+
  theme(legend.position = "none")

ggplotly(herbivores_percen)

hist((nematodes$hervibores_number))
hist((nematodes$herbivores_percentage))





herbivoresmodelp <- lme((herbivores_percentage) ~treatment*site*mo*moisture_content+block%in%site ,
                       data = nematodes,random=~ 1|tree)

mshervip <- dredge(herbivoresmodelp, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(mshervip, "rank.call"))
par(mar = c(3,5,6,4))
plot(mshervip, labAsExpr = TRUE)
model.avg(mshervip, subset = delta < 4)
confset.95p <- get.models(mshervip, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
fmList <- get.models(mshervi, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
summary(model.avg(fmList))
model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))


herbivoresmodelp <- lme((herbivores_percentage) ~treatment*site+moisture_content+mo+block%in% site ,
                       data = nematodes, random=~ 1|tree)
AIC(herbivoresmodelp)
r.squaredGLMM(herbivoresmodelp)
summary(herbivoresmodelp)
anova(herbivoresmodelp)


b <- ggplot(nematodes, aes(x = herbivores_percentage, y = moisture_content, colour=treatment))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 




#### predators##### hacer sin trasformar los datos#####

tablap<- summarySE(nematodes, measurevar="predators_number", groupvars=c("treatment", "site"))


hist(log10(nematodes$predators_number))
hist(nematodes$predators_number)


hist(log(nematodes$predators_number))
orderpredator<- orderNorm(nematodes$predators_number)
hist(orderpredator$x.t)
predators_number1<-predict(orderpredator)
nematodes<-cbind(predators_number1,nematodes)


predator_number<-ggplot(tablap, aes(x=site, y=predators_number, fill=treatment)) +
  
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=predators_number-sd, ymax=predators_number+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "predators number",size=30)+
  theme(legend.position = "none")



predatormodel <- lme((predators_number) ~treatment*site*mo*moisture_content+block%in%site ,
                       data = nematodes, random=~ 1|tree)

mspred <- dredge(predatormodel, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(mshervi, "rank.call"))
par(mar = c(3,5,6,4))
plot(mspred, labAsExpr = TRUE)
model.avg(mspred, subset = delta < 4)
confset.95p <- get.models(mspred, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
fmList <- get.models(mspred, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
summary(model.avg(fmList))
model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))



predatorsmodel <- lme((predators_number1) ~treatment*site+block%in%site ,
                       data = nematodes, random=~ 1|tree)


AIC(predatorsmodel)
r.squaredGLMM(predatorsmodel)
summary(predatorsmodel)
anova(predatorsmodel)

b <- ggplot(nematodes, aes(x = predators_number1, y = moisture_content))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 

#####predators percentage######
tablapp<- summarySE(nematodes, measurevar="predators_percen", groupvars=c("treatment", "site"))


predators_percen<-ggplot(tablapp, aes(x=site, y=predators_percen, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=predators_percen-sd, ymax=predators_percen+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "predators %",size=30)+
  theme(legend.position = "none")

hist(log(nematodes$predators_percen))
orderpredatorpercen<- orderNorm(nematodes$predators_percen)
hist(orderpredatorpercen$x.t)
predators_percen1<-predict(orderpredatorpercen)
nematodes<-cbind(predators_percen1,nematodes)
hist((nematodes$predators_percen))
hist((nematodes$predators_percen1))



predatormodelp <- lme((predators_percen1) ~treatment*site*mo*moisture_content+block%in%site ,
                        data = nematodes,
                      random=~ 1|tree)

mspredp <- dredge(predatormodelp, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(mspredp, "rank.call"))
par(mar = c(3,5,6,4))
plot(mspredp, labAsExpr = TRUE)
model.avg(mspredp, subset = delta < 4)
confset.95p <- get.models(mspredp, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
fmList <- get.models(mspredp, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
summary(model.avg(fmList))
model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))


predatorsmodelp <- lme((predators_percen1) ~treatment*site+moisture_content+block%in%site ,
                        data = nematodes,       random=~ 1|tree)
AIC(predatorsmodelp)
r.squaredGLMM(predatorsmodelp)
summary(predatorsmodelp)
anova(predatorsmodelp)


b <- ggplot(nematodes, aes(x = predators_percen1, y = moisture_content))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 

#####omnivores number#####
tablao<- summarySE(nematodes, measurevar="omnivores_number", groupvars=c("treatment", "site"))

hist(nematodes$omnivores_number)
omni_number<-ggplot(tablao, aes(x=site, y=omnivores_number, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=omnivores_number-sd, ymax=omnivores_number+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "omnivores number",size=30)+
  theme(legend.position = "none")



omivoresmodel <- lme((omnivores_number) ~treatment*site*mo*moisture_content+block%in%site ,
                       data = nematodes,random=~ 1|tree)

msomni <- dredge(omivoresmodel, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(msomni, "rank.call"))
par(mar = c(3,5,6,4))
plot(msomni, labAsExpr = TRUE)
model.avg(msomni, subset = delta < 4)
confset.95p <- get.models(msomni, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
fmList <- get.models(msomni, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
summary(model.avg(fmList))
model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))


hist(nematodes$moisture_content)

omnivoresmodel <- lme((omnivores_number) ~treatment*site+block%in%site ,
                       data = nematodes, random=~ 1|tree)
omnivoresmodel <- lme((omnivores_number) ~mo*moisture_content+site*moisture_content+block%in%site ,
                      data = nematodes, random=~ 1|tree)
omnivoresmodel <- lme((omnivores_number) ~treatment+mo*moisture_content+site*moisture_content+block%in%site ,
                      data = nematodes, random=~ 1|tree)

AIC(omnivoresmodel)
r.squaredGLMM(omnivoresmodel)
summary(omnivoresmodel)
anova(omnivoresmodel)

b <- ggplot(nematodes, aes(x = omnivores_number, y = moisture_content, colour=site))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 

######omnivores percentage####

tablaop<- summarySE(nematodes, measurevar="omnivores_percentage", groupvars=c("treatment", "site"))



hist(nematodes$omnivores_percentage)
omni_percentage<-ggplot(tablaop, aes(x=site, y=omnivores_percentage, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=omnivores_percentage-sd, ymax=omnivores_percentage+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "omnivores %",size=30)+
  theme(legend.position = "none")



omivoresmodelp <- lme((omnivores_percentage) ~treatment*site*mo*moisture_content+block%in%site ,
                     data = nematodes, random=~ 1|tree)

msomnip <- dredge(omivoresmodelp, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(msomnip, "rank.call"))
par(mar = c(3,5,6,4))
plot(msomnip, labAsExpr = TRUE)
model.avg(msomnip, subset = delta < 4)
confset.95p <- get.models(msomnip, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
fmList <- get.models(msomnip, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
summary(model.avg(fmList))
model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))


hist(nematodes$moisture_content)

omnivoresmodelp <- lme((omnivores_percentage) ~treatment*site+block%in%site,
                      data = nematodes, random=~ 1|tree)

#omnivoresmodelp <- lme((omnivores_percentage) ~treatment*mo*moisture_content+block,
 #                      data = nematodes, random=~ 1|tree)

AIC(omnivoresmodelp)
r.squaredGLMM(omnivoresmodelp)
summary(omnivoresmodelp)
anova(omnivoresmodelp)

b <- ggplot(nematodes, aes(x = omnivores_percentage, y = moisture_content, colour=site))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 

#####omnivores+predatos number#####
hist(nematodes$omnpred)

tablaop<- summarySE(nematodes, measurevar="omnpred", groupvars=c("treatment", "site"))



omnpred_number<-ggplot(tablaop, aes(x=site, y=omnpred, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=omnpred-sd, ymax=omnpred+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "omni+pred nº",size=30)+
  theme(legend.position = "none")



omnpredmodel <- lm((omnpred) ~treatment*site*mo*moisture_content+block%in%site ,
                     data = nematodes)

msomnpred <- dredge(omnpredmodel, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(msomnpred, "rank.call"))
par(mar = c(3,5,6,4))
plot(msomnpred, labAsExpr = TRUE)
model.avg(msomnpred, subset = delta < 4)
confset.95p <- get.models(msomnpred, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
fmList <- get.models(msomnpred, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
summary(model.avg(fmList))
model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))




omnipredmodel <- lm((omnpred) ~treatment*site*moisture_content+mo+ block%in%site ,
                      data = nematodes)
AIC(omnipredmodel)
r.squaredGLMM(omnipredmodel)
summary(omnipredmodel)
anova(omnipredmodel)

b <- ggplot(nematodes, aes(x = omnpred, y = moisture_content))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 

hist(nematodes$omnpredpercen)

tablaopp<- summarySE(nematodes, measurevar="omnpredpercen", groupvars=c("treatment", "site"))

omnpred_percen<-ggplot(tablaopp, aes(x=site, y=omnpredpercen, fill=treatment)) +
  
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=omnpredpercen-sd, ymax=omnpredpercen+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "omni+pred %",size=30)+
  theme(legend.position = "none")



omnpredmodelp <- lm((omnpredpercen) ~treatment*site*mo*moisture_content+block%in%site ,
                    data = nematodes)

msomnpredp <- dredge(omnpredmodelp, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(msomnpredp, "rank.call"))
par(mar = c(3,5,6,4))
plot(msomnpredp, labAsExpr = TRUE)
model.avg(msomnpredp, subset = delta < 4)
confset.95p <- get.models(msomnpredp, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
fmList <- get.models(msomnpredp, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
summary(model.avg(fmList))
model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))




omnipredmodelp <- lm((omnpredpercen) ~treatment*site+ block%in%site ,
                     data = nematodes)
AIC(omnipredmodelp)
r.squaredGLMM(omnipredmodelp)
summary(omnipredmodelp)
anova(omnipredmodelp)




####prey to predator####
tablaptp<- summarySE(nematodes, measurevar="prey_predator", groupvars=c("treatment", "site"))

hist(nematodes$prey_predator)

prey_to_predator<-ggplot(tablaptp, aes(x=site, y=prey_predator, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  
  geom_errorbar(aes(ymin=prey_predator-sd, ymax=prey_predator+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "prey/predator",size=30)+
  theme(legend.position = "none")



preypredatormodel <- lme((prey_predator) ~treatment*site*mo*moisture_content+block%in%site ,
                     data = nematodes, random=~ 1|tree)

msmprey <- dredge(preypredatormodel, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(msmprey, "rank.call"))
par(mar = c(3,5,6,4))
plot(msmprey, labAsExpr = TRUE)
model.avg(msmprey, subset = delta < 4)
confset.95p <- get.models(msmprey, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
# Get the models (fitted by REML, as in the global model)
fmList <- get.models(msmprey, 1:4)
# Because the models originate from 'dredge(..., rank = AICc, REML = FALSE)',
# the default weights in 'model.avg' are ML based:
summary(model.avg(fmList))
model.avg(fmList, rank = "AICc", rank.args = list(REML = FALSE))




preypredatormodel <- lme((prey_predator) ~treatment*site+ block%in%site ,
                      data = nematodes, random=~ 1|tree)
AIC(preypredatormodel)
r.squaredGLMM(preypredatormodel)
summary(preypredatormodel)
anova(preypredatormodel)



####graficos###

library(gridExtra)

grid.arrange(nematodes_abundance,richness,shannong, maturity_index, plant_parasitic_index, structure_index, enrichment_index, f_b_ratio,bacterivores_number,bacterivores_percen, fungivores_number,fungivores_percen,herbivores_number,herbivores_percen, predator_number, predators_percen, omni_number,omni_percentage,omnpred_number,omnpred_percen, prey_to_predator, ncol = 2, widths=c(2.1, 2.1), heights=c(1.4,1.4,1.4,1.4,1.4,1.4,1.4,1.4,1.4,1.4,1.4))



hist(nematodes$omnpred)
moisture<-ggplot(nematodes, aes(x=site, y=moisture_content, fill=treatment)) +
  geom_boxplot(trim=FALSE)+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "soil moisture content",size=30)+
  theme(legend.position = "true")


preypredatormodel <- lme((Prismatolaimus) ~treatment*site+ block ,
                         data = nematodes, random=~ 1|tree)
AIC(preypredatormodel)
r.squaredGLMM(preypredatormodel)
summary(preypredatormodel)
anova(preypredatormodel)



####generos filenchus prodesmodora y prismatolaimus####
hist(log(nematodes$Filenchus))
hist(sqrt(nematodes$Prismatolaimus))
hist(sqrt(nematodes$Prodesmodora))

tablafilenchus<- summarySE(nematodes, measurevar="Filenchus", groupvars=c("treatment", "site"))

filenchus<-ggplot(tablafilenchus, aes(x=site, y=Filenchus, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=Filenchus-sd, ymax=Filenchus+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "treatmentt",size=30)+
  theme(legend.position = "true")

preypredatormodel <- lme(sqrt(Prodesmodora) ~treatment*site+ block ,
                         data = nematodes, random=~ 1|tree)
AIC(preypredatormodel)
r.squaredGLMM(preypredatormodel)
summary(preypredatormodel)
anova(preypredatormodel)


####families#####

setwd("C:/Users/Usuario/Documents/nematodes")
#nematodes <- read.table("nematodes_intercapa.txt", header=T , sep="\t")
nematodes <- read.table("nematodes_18.txt", header=T , sep="\t")

str(nematodes)




nematodes$block<-as.factor(nematodes$block)

nematodes$tree<-as.factor(nematodes$tree)
nematodes$tree_sp<-as.factor(nematodes$tree_sp)
nematodes$treatment<-as.factor(nematodes$treatment)
nematodes$site<-as.factor(nematodes$site)
nematodes$Dintheria<-as.numeric(nematodes$Dintheria)
nematodes$Hemicycliophora<-as.numeric(nematodes$Hemicycliophora)

factors <- (nematodes[,c(1:10)])
sp <- (nematodes[,c(12:146)])






cephalobidae<-apply(sp[, c(2,3,4,25,26,27,51,59)],1,sum)
plectidae<-apply(sp[, c(8,96,131,134)],1,sum)
tylenchidae<-apply(sp[, c(5,18,20,32,54,63,68,76,80,81,131)],1,sum)
tylenchidae2<-apply(sp[, c(5,18,20,32,63,68,76,80,81,131)],1,sum)
criconematidae<-apply(sp[, c(33,34,35,70,84)],1,sum)
rhabditidae<-apply(sp[, c(23,39,71,94,95,107,117)],1,sum)
aphelenchidae<-apply(sp[, c(12,89)],1,sum)
aphelenchoididae<-apply(sp[, c(11,16,22,111)],1,sum)
aporcelaimidae<-apply(sp[, c(13,14,15,93,110)],1,sum)
prismatolaimidae<-apply(sp[, c(100)],1,sum)
qudsianematidae<-apply(sp[, c(7,50,52,62,73,120)],1,sum)
monhysteridae<-apply(sp[, c(53,57,77,78)],1,sum)
desmodoridae<-apply(sp[, c(102)],1,sum)


nematodes<-cbind(factors,cephalobidae)
nematodes<-cbind(nematodes,plectidae)
nematodes<-cbind(nematodes,tylenchidae)
nematodes<-cbind(nematodes,tylenchidae2)
nematodes<-cbind(nematodes,criconematidae)
nematodes<-cbind(nematodes,rhabditidae)
nematodes<-cbind(nematodes,aphelenchidae)
nematodes<-cbind(nematodes,aphelenchoididae)

nematodes<-cbind(nematodes,aporcelaimidae)
nematodes<-cbind(nematodes,qudsianematidae)

nematodes<-cbind(nematodes,monhysteridae)


hist(log(nematodes$mo))
hist(log(nematodes$moisture_content))
hist(nematodes$moisture_content)
hist(log(nematodes$cephalobidae))
hist(sqrt(nematodes$plectidae))
hist(log(nematodes$tylenchidae))
hist(sqrt(nematodes$tylenchidae2))

hist((nematodes$criconematidae))

hist(sqrt(nematodes$rhabditidae))
hist(sqrt(nematodes$aphelenchoididae))

hist(sqrt(nematodes$aphelenchidae))

hist(sqrt(nematodes$aporcelaimidae))
hist(sqrt(nematodes$qudsianematidae))
hist(sqrt(nematodes$monhysteridae))





nematodes$mo<-log(nematodes$mo)
nematodes$cephalobidae<-log(nematodes$cephalobidae)
nematodes$plectidae<-sqrt(nematodes$plectidae)
nematodes$tylenchidae<-log(nematodes$tylenchidae)
nematodes$tylenchidae2<-sqrt(nematodes$tylenchidae2)

nematodes$criconematidae<-sqrt(nematodes$criconematidae)

nematodes$rhabditidae<-sqrt(nematodes$rhabditidae)
nematodes$aphelenchoididae<-sqrt(nematodes$aphelenchoididae)
nematodes$aphelenchidae<-sqrt(nematodes$aphelenchidae)
nematodes$aporcelaimidae<-sqrt(nematodes$aporcelaimidae)
nematodes$qudsianematidae<-sqrt(nematodes$qudsianematidae)
nematodes$monhysteridae<-sqrt(nematodes$monhysteridae)

###Cehalobidae####
hist(nematodes$cephalobidae)

tablacephalobidae<- summarySE(nematodes, measurevar="cephalobidae", groupvars=c("treatment", "site"))

cephalobidae<-ggplot(tablacephalobidae, aes(x=site, y=cephalobidae, fill=treatment)) +
  
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=cephalobidae-sd, ymax=cephalobidae+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "cephalobidaer",size=30)+
  theme(legend.position = "none")


cephalobidaemodel <- lme((cephalobidae) ~treatment*site+block%in%site,
                         data = nematodes, random=~ 1|tree)
AIC(cephalobidaemodel)
r.squaredGLMM(cephalobidaemodel)
summary(cephalobidaemodel)
anova(cephalobidaemodel)

####plectidae####
hist(nematodes$plectidae)

tablaplectidae<- summarySE(nematodes, measurevar="plectidae", groupvars=c("treatment", "site"))

plectidae<-ggplot(tablaplectidae, aes(x=site, y=plectidae, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=plectidae-sd, ymax=plectidae+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "prey/predator",size=30)+
  theme(legend.position = "none")


plectidaemodel <- lme((plectidae) ~treatment*site+ block%in%site ,
                         data = nematodes, random=~ 1|tree)
AIC(plectidaemodel)
r.squaredGLMM(plectidaemodel)
summary(plectidaemodel)
anova(plectidaemodel)


####tylenchidae####
hist(nematodes$tylenchidae)

tablatylenchidae<- summarySE(nematodes, measurevar="tylenchidae2", groupvars=c("treatment", "site"))

tylenchidae<-ggplot(tablatylenchidae, aes(x=site, y=tylenchidae2, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=tylenchidae2-sd, ymax=tylenchidae2+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "filenchus",size=30)+
  theme(legend.position = "none")

tylenchidaemodel <- lm((tylenchidae) ~treatment*site+block%in%site ,
                      data = nematodes)
AIC(tylenchidaemodel)
r.squaredGLMM(tylenchidaemodel)
summary(tylenchidaemodel)
anova(tylenchidaemodel)

####aporcelaimidae####
hist(nematodes$aporcelaimidae)
tablaaporcelaimidae<- summarySE(nematodes, measurevar="aporcelaimidae", groupvars=c("treatment", "site"))

aporcelaimidae<-ggplot(tablaaporcelaimidae, aes(x=site, y=aporcelaimidae, fill=treatment)) +
  
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=aporcelaimidae-sd, ymax=aporcelaimidae+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "prey/predator",size=30)+
  theme(legend.position = "none")

aporcelaimidaemodel <- lme((aporcelaimidae) ~treatment*site+block%in%site ,
                        data = nematodes, random=~ 1|tree)
AIC(aporcelaimidaemodel)
r.squaredGLMM(aporcelaimidaemodel)
summary(aporcelaimidaemodel)
anova(aporcelaimidaemodel)


####criconematidae####
hist(nematodes$criconematidae)
tablacriconematidae<- summarySE(nematodes, measurevar="criconematidae", groupvars=c("treatment", "site"))

criconematidae<-ggplot(tablacriconematidae, aes(x=site, y=criconematidae, fill=treatment)) +
  
  geom_bar(stat="identity",position=position_dodge())+
geom_errorbar(aes(ymin=criconematidae-se, ymax=criconematidae+se),
              width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "prey/predator",size=30)+
  theme(legend.position = "none")

ordercrico<- orderNorm(nematodes$criconematidae)
hist(ordercrico$x.t)
criconematidae1<-predict(ordercrico)
nematodes<-cbind(criconematidae1,nematodes)


b <- ggplot(nematodes, aes(x = site, y = criconematidae,colour=treatment))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 

criconematidae1model <- lme((criconematidae1) ~treatment*site+block%in%site ,
                           data = nematodes, random=~ 1|tree)
AIC(criconematidae1model)
r.squaredGLMM(criconematidae1model)
summary(criconematidae1model)
anova(criconematidae1model)


####rhabditidae#####

hist(nematodes$rhabditidae)

tablarhabditidae<- summarySE(nematodes, measurevar="rhabditidae", groupvars=c("treatment", "site"))

rhabditidae<-ggplot(tablarhabditidae, aes(x=site, y=rhabditidae, fill=treatment)) +
  
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=rhabditidae-sd, ymax=rhabditidae+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "prey/predator",size=30)+
  theme(legend.position = "none")

rhabditidaemodel <- lme((rhabditidae) ~treatment*site+ block%in%site ,
                           data = nematodes, random=~ 1|tree)
AIC(rhabditidaemodel)
r.squaredGLMM(rhabditidaemodel)
summary(rhabditidaemodel)
anova(rhabditidaemodel)


####apjelenchoididae####
hist(nematodes$aphelenchoididae)
tablaaphelenchoididae<- summarySE(nematodes, measurevar="aphelenchoididae", groupvars=c("treatment", "site"))

aphelenchoididae<-ggplot(tablaaphelenchoididae, aes(x=site, y=aphelenchoididae, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=aphelenchoididae-sd, ymax=aphelenchoididae+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "prey/predator",size=30)+
  theme(legend.position = "none")

aphelenchoididaemodel <- lme((aphelenchoididae) ~treatment*site+block%in%site ,
                        data = nematodes, random=~ 1|tree)
AIC(aphelenchoididaemodel)
r.squaredGLMM(aphelenchoididaemodel)
summary(aphelenchoididaemodel)
anova(aphelenchoididaemodel)


####apjelenchidae####
hist(nematodes$aphelenchidae)

tablaaphelenchidae<- summarySE(nematodes, measurevar="aphelenchidae", groupvars=c("treatment", "site"))

aphelenchidae<-ggplot(tablaaphelenchidae, aes(x=site, y=aphelenchidae, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=aphelenchidae-sd, ymax=aphelenchidae+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "prey/predator",size=30)+
  theme(legend.position = "none")

aphelenchidaemodel <- lme((aphelenchidae) ~treatment*site+block%in%site ,
                             data = nematodes, random=~ 1|tree)
AIC(aphelenchidaemodel)
r.squaredGLMM(aphelenchidaemodel)
summary(aphelenchidaemodel)
anova(aphelenchidaemodel)


####qudsianematida####
hist(nematodes$qudsianematidae)
tablaqudsianematidae<- summarySE(nematodes, measurevar="qudsianematidae", groupvars=c("treatment", "site"))

qudsianematidae<-ggplot(tablaqudsianematidae, aes(x=site, y=qudsianematidae, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=qudsianematidae-sd, ymax=qudsianematidae+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "prey/predator",size=30)+
  theme(legend.position = "none")

qudsianematidaemodel <- lm((qudsianematidae) ~treatment*site+block%in%site ,
                          data = nematodes)
AIC(qudsianematidaemodel)
r.squaredGLMM(qudsianematidaemodel)
summary(qudsianematidaemodel)
anova(qudsianematidaemodel)

####monhysteridae####
hist(nematodes$monhysteridae)
tablamonhysteridae<- summarySE(nematodes, measurevar="monhysteridae", groupvars=c("treatment", "site"))

monhysteridae<-ggplot(tablamonhysteridae, aes(x=site, y=monhysteridae, fill=treatment)) +
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin=monhysteridae-sd, ymax=monhysteridae+sd),
                width=.2,position=position_dodge(.9))+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = "Year", y= "prey/predator",size=30)+
  theme(legend.position = "none")

monhysteridaeemodel <- lme((monhysteridae) ~treatment*site +block%in%site ,
                            data = nematodes, random=~ 1|tree)
AIC(monhysteridaeemodel)
r.squaredGLMM(monhysteridaeemodel)
summary(monhysteridaeemodel)
anova(monhysteridaeemodel)
#### Pitar % de grupos troficos y abundancia de familias por tr.####



setwd("C:/Users/Usuario/Documents/nematodes")
#nematodes <- read.table("nematodes_intercapa.txt", header=T , sep="\t")
nematodes <- read.table("nematodes_181.txt", header=T , sep="\t")

str(nematodes)




nematodes$block<-as.factor(nematodes$block)

nematodes$tree<-as.factor(nematodes$tree)
nematodes$tree_sp<-as.factor(nematodes$tree_sp)
nematodes$treatment<-as.factor(nematodes$treatment)
nematodes$site<-as.factor(nematodes$site)
nematodes$Dintheria<-as.numeric(nematodes$Dintheria)
nematodes$Hemicycliophora<-as.numeric(nematodes$Hemicycliophora)

factors <- (nematodes[,c(1:11)])



genus <- (nematodes[,c(13:147)])

Achromadoridae <- (genus[,c(1)])
Alaimidae<-apply(genus[, c(6,88)],1,sum)
Anatonchidae<-apply(genus[, c(9,75)],1,sum)
Anguinidae<-apply(genus[, c(10,44,82,104,114)],1,sum)

Bathyodontidae<-(genus[, c(36)])

Aphelenchidae<-apply(genus[, c(12,89)],1,sum)
Aphelenchoididae<-apply(genus[, c(11,16,22,111)],1,sum)
Seinura<-(genus[, c(111)])###sseinura 111 es predator aunque es de aphelenchoididae


Aporcelaimidae<-apply(genus[, c(13,14,15,93,110)],1,sum)
Bastianiidae<-apply(genus[, c(19,40)],1,sum)
Bathyodontidae<-(genus[, c(36)])
Belondiridae<-apply(genus[, c(17,46)],1,sum)
Brevibuccidae<-(genus[, c(97)])
Bunonematidae<-(genus[, c(21)])


Cephalobidae<-apply(genus[, c(2,3,4,25,26,27,51,59)],1,sum)
Criconematidae<-apply(genus[, c(33,34,35,84,70)],1,sum)

Desmodoridae<-(genus[, c(102)])
Diphtherophoridae<-apply(genus[, c(41,132)],1,sum)

Diplopeltidae<--apply(genus[, c(37,45)],1,sum)
Discolaimidae<-(genus[, c(43)])
Dolichodoridae<--apply(genus[, c(69,129)],1,sum)

Ecphyadophoridae<-apply(genus[, c(49,115)],1,sum)

Hoplolaimidae<--apply(genus[, c(57,90)],1,sum)
Hypodontolaimidae<-(genus[, c(28)])
Iotonchidae<-(genus[, c(55)])
Ironidae<-(genus[, c(61)])


Leptolaimidae<-apply(genus[, c(29,64)],1,sum)
Leptonchidae<-apply(genus[, c(65,127,128,47)],1,sum)

Longidoridae<--apply(genus[, c(67,135)],1,sum)
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

Quadsinematidae<-apply(genus[, c(7,50,52,73,120,62)],1,sum)
Rhabditidae<-apply(genus[, c(23,39,71,94,107,95)],1,sum)

Rhabdolaimidae<-(genus[, c(108)])
Steinernematidae<-(genus[, c(113)])

Teratocephalidae<-apply(genus[, c(72,116)],1,sum)

Thornematidae<-(genus[, c(103)])
Tobrilidae<-(genus[, c(121)])
Trichodoridae<--apply(genus[, c(91,123)],1,sum)

Tripylidae<-apply(genus[, c(124,122)],1,sum)
Tylenchidae<-apply(genus[, c(5,18,20,32,63,68,76,80,81,130)],1,sum) 

Filenchus<-(genus[, c(54)])##filenchus (54) es fungivoro, tylenchidae herbivoros

Tylodoridae<-(genus[, c(24)])

Xyalidae<-apply(genus[, c(38,118)],1,sum)


families<-cbind(factors,Anatonchidae,Discolaimidae,Ironidae,Mononchidae,Nygolaimidae,Onchulidae, Tobrilidae,Tripylidae,
                Aporcelaimidae, Hypodontolaimidae,Nordiidae,Quadsinematidae,Thornematidae,
                Achromadoridae,Belondiridae,Criconematidae,Ecphyadophoridae,Longidoridae,Longidorella,Paratylenchidae,Pratylenchidae,Psilenchidae,Trichodoridae,Tylenchidae,Tylodoridae,
                Anguinidae,Aphelenchidae,Aphelenchoididae,Diphtherophoridae,Iotonchidae,Leptonchidae,Filenchus,
                Alaimidae,Bastianiidae,Bathyodontidae,Brevibuccidae,Bunonematidae,Cephalobidae,Desmodoridae,Leptolaimidae,Microlaimidae,Monhysteridae,Odontolaimidae,Ostellidae,Panagrolaimidae,Plectidae,Prismatolaimidae,Rhabditidae,Rhabdolaimidae,Steinernematidae,Teratocephalidae,Xyalidae)
                
                


familiesa <- families[-which(families$site=="Puechabon"),]
familiesp<- families[-which(families$site=="Alcornocales"),]

familiesa <- (familiesa[,c(7,12:63)])
familiesp <- (familiesp[,c(7,12:63)])



library(reshape2)
gg<-melt(familiesa,id="treatment")

familiesplotalcornocales<- ggplot(gg, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
 scale_color_discrete("treatment")+
theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = NULL, y= "Abundance (indv/g)",size=30)+
  scale_y_continuous(expand = c(0, 0))+
  stat_summary(fun.data = mean_se,geom="errorbar",
               color="black",position=position_dodge(0.9), width=.2)+
   theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))+
ggtitle("Spain.2 years of rainfall exclusion")+
  theme(plot.title=element_text(size=25))+ theme(
    plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")
  

gg<-melt(familiesp,id="treatment")

familiesplotpuechabon<- ggplot(gg, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
  scale_color_discrete("treatment")+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = NULL, y= "Abundance (indv/g)",size=30)+
  scale_y_continuous(expand = c(0, 0))+
  stat_summary(fun.data = mean_se,geom="errorbar",
               color="black",position=position_dodge(0.9), width=.2)+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))+
  ggtitle("France. 15 years of rainfall exclusion")+
  theme(plot.title=element_text(size=25))+ theme(
    plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")


#library(grid)
#library(gridExtra)
grid.arrange(familiesplotalcornocales,familiesplotpuechabon,ncol=2, bottom=textGrob("Families",gp=gpar(fontsize=25)))

####percentages of families#####
nematodes <- read.table("nematodes_181.txt", header=T , sep="\t")

str(nematodes)




nematodes$block<-as.factor(nematodes$block)

nematodes$tree<-as.factor(nematodes$tree)
nematodes$tree_sp<-as.factor(nematodes$tree_sp)
nematodes$treatment<-as.factor(nematodes$treatment)
nematodes$site<-as.factor(nematodes$site)
nematodes$Dintheria<-as.numeric(nematodes$Dintheria)
nematodes$Hemicycliophora<-as.numeric(nematodes$Hemicycliophora)

factors <- (nematodes[,c(1:11,165)])



genus <- (nematodes[,c(13:147)])

Achromadoridae <- (genus[,c(1)])
Alaimidae<-apply(genus[, c(6,88)],1,sum)
Anatonchidae<-apply(genus[, c(9,75)],1,sum)
Anguinidae<-apply(genus[, c(10,44,82,104,114)],1,sum)

Bathyodontidae<-(genus[, c(36)])

Aphelenchidae<-apply(genus[, c(12,89)],1,sum)
Aphelenchoididae<-apply(genus[, c(11,16,22,111)],1,sum)
Seinura<-(genus[, c(111)])###sseinura 111 es predator aunque es de aphelenchoididae


Aporcelaimidae<-apply(genus[, c(13,14,15,93,110)],1,sum)
Bastianiidae<-apply(genus[, c(19,40)],1,sum)
Bathyodontidae<-(genus[, c(36)])
Belondiridae<-apply(genus[, c(17,46)],1,sum)
Brevibuccidae<-(genus[, c(97)])
Bunonematidae<-(genus[, c(21)])


Cephalobidae<-apply(genus[, c(2,3,4,25,26,27,51,59)],1,sum)
Criconematidae<-apply(genus[, c(33,34,35,84,70)],1,sum)

Desmodoridae<-(genus[, c(102)])
Diphtherophoridae<-apply(genus[, c(41,132)],1,sum)

Diplopeltidae<--apply(genus[, c(37,45)],1,sum)
Discolaimidae<-(genus[, c(43)])
Dolichodoridae<--apply(genus[, c(69,129)],1,sum)

Ecphyadophoridae<-apply(genus[, c(49,115)],1,sum)

Hoplolaimidae<--apply(genus[, c(57,90)],1,sum)
Hypodontolaimidae<-(genus[, c(28)])
Iotonchidae<-(genus[, c(55)])
Ironidae<-(genus[, c(61)])


Leptolaimidae<-apply(genus[, c(29,64)],1,sum)
Leptonchidae<-apply(genus[, c(65,127,128,47)],1,sum)

Longidoridae<--apply(genus[, c(67,135)],1,sum)
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

Quadsinematidae<-apply(genus[, c(7,50,52,73,120,62)],1,sum)
Rhabditidae<-apply(genus[, c(23,39,71,94,107,95)],1,sum)

Rhabdolaimidae<-(genus[, c(108)])
Steinernematidae<-(genus[, c(113)])

Teratocephalidae<-apply(genus[, c(72,116)],1,sum)

Thornematidae<-(genus[, c(103)])
Tobrilidae<-(genus[, c(121)])
Trichodoridae<--apply(genus[, c(91,123)],1,sum)

Tripylidae<-apply(genus[, c(124,122)],1,sum)
Tylenchidae<-apply(genus[, c(5,18,20,32,63,68,76,80,81,130)],1,sum) 

Filenchus<-(genus[, c(54)])##filenchus (54) es fungivoro, tylenchidae herbivoros

Tylodoridae<-(genus[, c(24)])

Xyalidae<-apply(genus[, c(38,118)],1,sum)


families<-cbind(factors,Anatonchidae,Discolaimidae,Ironidae,Mononchidae,Nygolaimidae,Onchulidae, Tobrilidae,Tripylidae,
                Aporcelaimidae, Hypodontolaimidae,Nordiidae,Quadsinematidae,Thornematidae,
                Achromadoridae,Belondiridae,Criconematidae,Ecphyadophoridae,Longidoridae,Longidorella,Paratylenchidae,Pratylenchidae,Psilenchidae,Trichodoridae,Tylenchidae,Tylodoridae,
                Anguinidae,Aphelenchidae,Aphelenchoididae,Diphtherophoridae,Iotonchidae,Leptonchidae,Filenchus,
                Alaimidae,Bastianiidae,Bathyodontidae,Brevibuccidae,Bunonematidae,Cephalobidae,Desmodoridae,Leptolaimidae,Microlaimidae,Monhysteridae,Odontolaimidae,Ostellidae,Panagrolaimidae,Plectidae,Prismatolaimidae,Rhabditidae,Rhabdolaimidae,Steinernematidae,Teratocephalidae,Xyalidae)


familiesa <- families[-which(families$site=="Puechabon"),]
familiesp<- families[-which(families$site=="Alcornocales"),]

familiesa <- (familiesa[,c(7,12:64)])
familiesp <- (familiesp[,c(7,12:64)])
familiesapercen <- 100*((familiesa[,3:54])/(familiesa[,2]))###calculate percentages
familiesapercen<-cbind(familiesapercen,familiesa$treatment)
colnames(familiesapercen)[53]<-"treatment"
familiespuechpercen <- 100*((familiesp[,3:54])/(familiesp[,2]))###calculate percentages
familiespuechpercen<-cbind(familiespuechpercen,familiesa$treatment)
colnames(familiespuechpercen)[53]<-"treatment"




gg<-melt(familiesapercen,id="treatment")

familiesplotalcornocales<- ggplot(gg, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
  scale_color_discrete("treatment")+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = NULL, y= "Abundance (% of total)",size=30)+
  scale_y_continuous(expand = c(0, 0))+
  stat_summary(fun.data = mean_se,geom="errorbar",
               color="black",position=position_dodge(0.9), width=.2)+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))+
  ggtitle("Spain.2 years of rainfall exclusion")+
  theme(plot.title=element_text(size=25))+ theme(
    plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")


gg<-melt(familiespuechpercen,id="treatment")

familiesplotpuechabon<- ggplot(gg, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
  scale_color_discrete("treatment")+
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = NULL, y= "Abundance (% of total)",size=30)+
  scale_y_continuous(expand = c(0, 0))+
  stat_summary(fun.data = mean_se,geom="errorbar",
               color="black",position=position_dodge(0.9), width=.2)+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))+
  ggtitle("France. 15 years of rainfall exclusion")+
  theme(plot.title=element_text(size=25))+ theme(
    plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")


grid.arrange(familiesplotalcornocales,familiesplotpuechabon,ncol=2, bottom=textGrob("Families",gp=gpar(fontsize=25)))

####trophic groups plot %####

nematodes <- read.table("nematodes_181.txt", header=T , sep="\t")
nematodes1 <- nematodes[-which(nematodes$site=="Puechabon"),]
nematodes2 <- nematodes[-which(nematodes$site=="Alcornocales"),]

trophic <- (nematodes1[,c(7,166,167,169,171,173)])

trophic2 <- (nematodes2[,c(7,166,167,169,171,173)])

library(reshape2)


#gg<-melt(trophic,id.vars=c("site","treatment"))




gg<-melt(trophic,id.vars=c("treatment"))

trophic_plot_alcor<- ggplot(gg, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
  scale_color_discrete("tr_site")+

    stat_summary(fun.data = mean_se,geom="errorbar",
             color="black",position=position_dodge(0.9), width=.2)+
  
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(        axis.text.x=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = NULL, y= "Percentege of total abundance",size=30)+
  scale_y_continuous(limits=c(0,70), breaks=seq(0,70, by = 10), expand=c(0,0))+
  coord_cartesian(ylim = c(0, 50), expand=c(0,0))

trophic_plot_alcor <-trophic_plot_alcor +
  scale_x_discrete(limits=c("predators_percen","omnivores_percentage","herbivores_percentage", "fungivores_percentage","bacterivores_percentage"))+
  theme(legend.position = "none")+
  theme( panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  ggtitle("Spain. 2 years of rainfall exclusion")+
  theme(plot.title=element_text(size=25))+
  scale_x_discrete(limits=c("predators_percen","omnivores_percentage","herbivores_percentage", "fungivores_percentage","bacterivores_percentage"),
                   labels=c("predators_percen" = "Predators", "omnivores_percentage" = "Omnivores",
                            "herbivores_percentage" = "Hervibores", "fungivores_percentage" = "Fungivores", "bacterivores_percentage" = "Bacterivores") )+
  theme(
    plot.title = element_text(hjust = 0.5))+
  annotate(geom="text", x=2, y=45, label="*",  color="black", size=8, fontface="bold")


#library(grid)


gg2<-melt(trophic2,id.vars=c("treatment"))

trophic_plot_puec<- ggplot(gg2, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
  scale_color_discrete("tr_site")+
  
  stat_summary(fun.data = mean_se,geom="errorbar",
               color="black",position=position_dodge(0.9), width=.2)+
  
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = NULL,
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  theme(        axis.text.x=element_text(size=14))+
  labs(x = NULL, y= NULL,size=30)+
  scale_y_continuous(limits=c(0,70), breaks=seq(0,70, by = 10), expand=c(0,0),position="right")+
  coord_cartesian(ylim = c(0, 50), expand=c(0,0))+
  ggtitle("France.15 years of rainfall exclusion")+
  theme(plot.title=element_text(size=25))+
  theme(
    plot.title = element_text(hjust = 0.5))+
annotate(geom="text", x=2, y=45, label="*",
        color="black", size=8, fontface="bold")+
  annotate(geom="text", x=4, y=45, label="*",
           color="black", size=8, fontface="bold")






trophic_plot_puec<-trophic_plot_puec + 
 #  scale_x_discrete(limits=c("predators_percen","omnivores_percentage","herbivores_percentage", "fungivores_percentage","bacterivores_percentage"))+
  theme(legend.position = "none")+
 theme( panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  scale_x_discrete(limits=c("predators_percen","omnivores_percentage","herbivores_percentage", "fungivores_percentage","bacterivores_percentage"),
                   labels=c("predators_percen" = "Predators", "omnivores_percentage" = "Omnivores",
                            "herbivores_percentage" = "Hervibores", "fungivores_percentage" = "Fungivores", "bacterivores_percentage" = "Bacterivores") )


#library(grid)
#library(gridExtra)
grid.arrange(trophic_plot_alcor,trophic_plot_puec,ncol=2, bottom=textGrob(" Trophic groups",gp=gpar(fontsize=25)))



###graph indexes 1####

nematodes <- read.table("nematodes_181.txt", header=T , sep="\t")

factors <- (nematodes[,c(1:11)])
sp <- (nematodes[,c(13:147)])



richness<-apply(sp[,]>0,1,sum)
nematodes<-cbind(nematodes,richness) 
shannong<- diversity(sp, index = "shannon", MARGIN = 1, base = exp(1))
nematodes<-cbind(nematodes,shannong)

nematodes1 <- nematodes[-which(nematodes$site=="Puechabon"),]
nematodes2 <- nematodes[-which(nematodes$site=="Alcornocales"),]




trophic <- (nematodes1[,c(7,12,155,154,199,201)])

trophic2 <- (nematodes2[,c(7,12,155,154,199,201)])

library(reshape2)


#gg<-melt(trophic,id.vars=c("site","treatment"))




gg<-melt(trophic,id.vars=c("treatment"))

indexs_plot1_alcor<- ggplot(gg, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
  scale_color_discrete("tr_site")+
  
  stat_summary(fun.data = mean_se,geom="errorbar",
               color="black",position=position_dodge(0.9), width=.2)+
  theme(        axis.text.x=element_text(size=14))+
  
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = NULL, y= NULL,size=30)+
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100, by = 10), expand=c(0,0))+
coord_cartesian(ylim = c(0, 80), expand=c(0,0))


indexs_plot1_alcor <-indexs_plot1_alcor +
  scale_x_discrete(limits=c("nematodes_gram", "prey_predator","richness","enrichment_index", "structure_index"))+
  theme(legend.position = "none")+
  theme( panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  ggtitle("Spain. 2 years of rainfall exclusion")+
  theme(plot.title=element_text(size=25))+
  scale_x_discrete(limits=c( "nematodes_gram","prey_predator","richness","enrichment_index", "structure_index"),
                   labels=c("nematodes_gram"="Abundance","prey_predator" = "Prey/Predators", "richness" = "Richness",
                            "enrichment_index" = "E.I", "structure_index" = "S.I") )+
  theme(
    plot.title = element_text(hjust = 0.5))+
  annotate(geom="text", x=1, y=75, label="*",
           color="black", size=8, fontface="bold")+
  annotate(geom="text", x=5, y=75, label="*",
           color="black", size=8, fontface="bold")+
  annotate(geom="text", x=2, y=75, label="*",
           color="black", size=8, fontface="bold")
 

#library(grid)


gg2<-melt(trophic2,id.vars=c("treatment"))

indexs_plot1_puec<- ggplot(gg2, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
  scale_color_discrete("tr_site")+
  
  stat_summary(fun.data = mean_se,geom="errorbar",
               color="black",position=position_dodge(0.9), width=.2)+
  theme(        axis.text.x=element_text(size=14))+
  
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
   labs(x = NULL, y= NULL,size=30)+
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100, by = 10), expand=c(0,0), position="right")+
coord_cartesian(ylim = c(0, 80), expand=c(0,0))


indexs_plot1_puec <-indexs_plot1_puec +
  scale_x_discrete(limits=c("nematodes_gram", "prey_predator","richness","enrichment_index", "structure_index"))+
  theme(legend.position = "none")+
  theme( panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  ggtitle("France. 15 years of rainfall exclusion")+
  theme(plot.title=element_text(size=25))+
  scale_x_discrete(limits=c( "nematodes_gram","prey_predator","richness","enrichment_index", "structure_index"),
                   labels=c("nematodes_gram"="Abundance","prey_predator" = "Prey/Predators", "richness" = "Richness",
                            "enrichment_index" = "E.I", "structure_index" = "S.I") )+
  theme(
    plot.title = element_text(hjust = 0.5))+

  annotate(geom="text", x=5, y=75, label="*",
           color="black", size=8, fontface="bold")+
  annotate(geom="text", x=2, y=75, label="*",
           color="black", size=8, fontface="bold")


  
library(grid)
library(gridExtra)
grid.arrange(indexs_plot1_alcor,indexs_plot1_puec,ncol=2, bottom=textGrob(" Soil foodweb Indices",gp=gpar(fontsize=25)))


###graph indexes 2####

nematodes <- read.table("nematodes_181.txt", header=T , sep="\t")

factors <- (nematodes[,c(1:11)])
sp <- (nematodes[,c(13:147)])



richness<-apply(sp[,]>0,1,sum)
nematodes<-cbind(nematodes,richness) 
shannong<- diversity(sp, index = "shannon", MARGIN = 1, base = exp(1))
nematodes<-cbind(nematodes,shannong)

nematodes1 <- nematodes[-which(nematodes$site=="Puechabon"),]
nematodes2 <- nematodes[-which(nematodes$site=="Alcornocales"),]




trophic <- (nematodes1[,c(7,148,151,196,202)])

trophic2 <- (nematodes2[,c(7,148,151,196,202)])

library(reshape2)


#gg<-melt(trophic,id.vars=c("site","treatment"))




gg<-melt(trophic,id.vars=c("treatment"))

indexs_plot2_alcor<-  ggplot(gg, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
  scale_color_discrete("tr_site")+
  
  stat_summary(fun.data = mean_se,geom="errorbar",
               color="black",position=position_dodge(0.9), width=.2)+
  theme(        axis.text.x=element_text(size=14))+
  
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
   labs(x = NULL, y= NULL,size=30)+
  scale_y_continuous(limits=c(0,4), breaks=seq(0,4, by = 1), expand=c(0,0))

indexs_plot2_alcor <-indexs_plot2_alcor +
  scale_x_discrete(limits=c("f_b_ratio","plant_parasitic_index","maturity_index", "shannong"))+
  theme(legend.position = "none")+
  theme( panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  ggtitle("Spain. 2 years of rainfall exclusion")+
  theme(plot.title=element_text(size=25))+
  scale_x_discrete(limits=c("f_b_ratio","plant_parasitic_index","maturity_index", "shannong"),
                   labels=c("f_b_ratio" = "F/B", "plant_parasitic_index" = "P.P.I",
                            "maturity_index" = "M.I", "shannong" = "Diversity") )+
  theme(
    plot.title = element_text(hjust = 0.5))+
 
  annotate(geom="text", x=3, y=3.5, label="*",
           color="black", size=8, fontface="bold")



#library(grid)


gg2<-melt(trophic2,id.vars=c("treatment"))

indexs_plot2_puec<- ggplot(gg2, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
  scale_color_discrete("tr_site")+
  
  stat_summary(fun.data = mean_se,geom="errorbar",
               color="black",position=position_dodge(0.9), width=.2)+
  theme(        axis.text.x=element_text(size=14))+
  
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = NULL, y= NULL,size=30)+
  scale_y_continuous(limits=c(0,4), breaks=seq(0,4, by = 1), expand=c(0,0),position="right")



indexs_plot2_puec <-indexs_plot2_puec +
  scale_x_discrete(limits=c("f_b_ratio","plant_parasitic_index","maturity_index", "shannong"))+
  theme(legend.position = "none")+
  theme( panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  ggtitle("France. 15 years of rainfall exclusion")+
  theme(plot.title=element_text(size=25))+
  scale_x_discrete(limits=c("f_b_ratio","plant_parasitic_index","maturity_index", "shannong"),
                   labels=c("f_b_ratio" = "F/B", "plant_parasitic_index" = "P.P.I",
                            "maturity_index" = "M.I", "shannong" = "Diversity") )+
  theme(
    plot.title = element_text(hjust = 0.5))+
  annotate(geom="text", x=1, y=3.5, label="*",
           color="black", size=8, fontface="bold")+
  annotate(geom="text", x=3, y=3.5, label="*",
           color="black", size=8, fontface="bold")


#library(grid)

library(grid)
library(gridExtra)
grid.arrange(indexs_plot2_alcor,indexs_plot2_puec,ncol=2, bottom=textGrob("Soil foodweb Indices",gp=gpar(fontsize=25)))




####trophic groups plot abundance####

nematodes <- read.table("nematodes_181.txt", header=T , sep="\t")
nematodes1 <- nematodes[-which(nematodes$site=="Puechabon"),]
nematodes2 <- nematodes[-which(nematodes$site=="Alcornocales"),]

trophic <- (nematodes1[,c(7,190:194)])

trophic2 <- (nematodes2[,c(7,190:194)])

library(reshape2)


#gg<-melt(trophic,id.vars=c("site","treatment"))




gg<-melt(trophic,id.vars=c("treatment"))

trophic_plot_alcor<- ggplot(gg, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
  scale_color_discrete("tr_site")+
  
  stat_summary(fun.data = mean_se,geom="errorbar",
               color="black",position=position_dodge(0.9), width=.2)+
  theme(        axis.text.x=element_text(size=14))+
  
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = NULL, y= "Total abundance (ind/g)",size=30)+
#  scale_y_continuous(limits=c(0,20), breaks=seq(0,20, by = 1), expand=c(0,0))
  coord_cartesian(ylim = c(0,4), expand=c(0,0))
  
  
trophic_plot_alcor <-trophic_plot_alcor +
  scale_x_discrete(limits=c("predators_number","omnivores_number","hervibores_number", "fungivores_number","bacterivores_number"))+
  theme(legend.position = "none")+
  theme( panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  ggtitle("Spain. 2 years of rainfall exclusion")+
  theme(plot.title=element_text(size=25))+
  scale_x_discrete(limits=c("predators_number","omnivores_number","hervibores_number", "fungivores_number","bacterivores_number"),
                   labels=c("predators_number" = "Predators", "omnivores_number" = "Omnivores",
                            "hervibores_number" = "Hervibores", "fungivores_number" = "Fungivores", "bacterivores_number" = "Bacterivores") )+
  theme(
    plot.title = element_text(hjust = 0.5))+
  annotate(geom="text", x=5, y=3.8, label="*",  color="black", size=8, fontface="bold")+
annotate(geom="text", x=4, y=3.8, label="*",  color="black", size=8, fontface="bold")+
  annotate(geom="text", x=3, y=3.8, label="*",  color="black", size=8, fontface="bold")

#library(grid)


gg2<-melt(trophic2,id.vars=c("treatment"))

trophic_plot_puec<- ggplot(gg2, aes(x=variable, y=value, fill=factor(treatment))) + 
  stat_summary(fun.y=mean, geom="bar",position="dodge") + 
  scale_color_discrete("tr_site")+
  
  stat_summary(fun.data = mean_se,geom="errorbar",
               color="black",position=position_dodge(0.9), width=.2)+
  theme(        axis.text.x=element_text(size=14))+
  
  theme( panel.background  = element_blank())+
  scale_fill_manual(values=c("powderblue" ,"#CC0C00FF"))+
  theme(axis.title.y = element_text(size=18),
        axis.text.y=element_text(size=14))+
  theme(axis.line = element_line(colour="black"))+
  labs(x = NULL, y= "Total abundance (ind/g)",size=30)+
  scale_y_continuous(limits=c(0,20), breaks=seq(0,20, by = 1), expand=c(0,0),position="right")+
  coord_cartesian(ylim = c(0, 8.5), expand=c(0,0))



trophic_plot_puec <-trophic_plot_puec +
  scale_x_discrete(limits=c("predators_number","omnivores_number","hervibores_number", "fungivores_number","bacterivores_number"))+
  theme(legend.position = "none")+
  theme( panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  ggtitle("France. 15 years of rainfall exclusion")+
  theme(plot.title=element_text(size=25))+
  scale_x_discrete(limits=c("predators_number","omnivores_number","hervibores_number", "fungivores_number","bacterivores_number"),
                   labels=c("predators_number" = "Predators", "omnivores_number" = "Omnivores",
                            "hervibores_number" = "Hervibores", "fungivores_number" = "Fungivores", "bacterivores_number" = "Bacterivores") )+
  theme(
    plot.title = element_text(hjust = 0.5))+
  annotate(geom="text", x=2, y=8, label="*",  color="black", size=8, fontface="bold")



#library(grid)
#library(gridExtra)
grid.arrange(trophic_plot_alcor,trophic_plot_puec,ncol=2, bottom=textGrob(" Trophic groups",gp=gpar(fontsize=25)))

###1400*600