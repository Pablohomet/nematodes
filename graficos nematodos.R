#### Graficos % de grupos troficos y abundancia de familias por tr.####

library(ggplot2)



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



library(gridExtra)
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
familiespuechpercen<-cbind(familiespuechpercen,familiesp$treatment)
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