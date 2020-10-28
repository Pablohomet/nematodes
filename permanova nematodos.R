library(bestNormalize)
library(nlme)
library(tidyverse)
library(ggplot2)
library(plotly)
library(sjPlot)
library(MuMIn)
library(vegan)
getwd()
setwd("C:/Users/Usuario/Documents/nematodes")
#nematodes <- read.table("nematodes_intercapa.txt", header=T , sep="\t")
nematodes <- read.table("nematodes_181.txt", header=T , sep="\t")

str(nematodes)




nematodes$block<-as.factor(nematodes$block)
nematodes$tr_site<-as.factor(nematodes$tr_site)
nematodes$tree<-as.factor(nematodes$tree)
nematodes$tree_sp<-as.factor(nematodes$tree_sp)
nematodes$treatment<-as.factor(nematodes$treatment)
nematodes$site<-as.factor(nematodes$site)
nematodes$Dintheria<-as.numeric(nematodes$Dintheria)
nematodes$Hemicycliophora<-as.numeric(nematodes$Hemicycliophora)

factors <- (nematodes[,c(1:11)])
sp <- (nematodes[,c(13:147)])##para 181







adonis(sp ~ tr_site,strata = factors$block, data=factors, perm=1e3, method="bray")
#adonis(sp ~ tree_sp/plot, data=env, perm=1e3, method="bray")
#adonis(sp ~ tree_sp*plot, data=env, perm=1e3, method="bray")


#PERMDISP:homogeneidad de varianza

## Bray-Curtis distances between samples
dis <- vegdist(sp)

## First 16 sites grazed, remaining 8 sites ungrazed
groups <- factor(factors$tr_site)

## Calculate multivariate dispersions
mod <- betadisper(dis, groups)
mod

## Perform test
anova(mod)

#NO SIGNIFICATIVO: existe homogeneidad en la dispersiC3n entre los grupos
#Podemos afirmar que hay diferencias en la composiciC3n de las comunidades


## Permutation test for F

permutest(mod, pairwise = TRUE)
#No SIGNIFICATIVO: existe homogeneidad en la dispersiC3n entre los grupos
#Podemos afirmar que hay diferencias en la composiciC3n de las comunidades




## Tukey's Honest Significant Differences(no se que muestra)
mod.HSD <- TukeyHSD(mod)
plot(mod.HSD)
## Plot the groups and distances to centroids on the
## first two PCoA axes
plot(mod)

## with data ellipses instead of hulls
plot(mod, ellipse = TRUE, hull = FALSE) # 1 sd data ellipse
plot(mod, ellipse = TRUE, hull = FALSE, conf = 0.90) # 90% data ellipse

## can also specify which axes to plot, ordering respected
plot(mod, axes = c(3,1), seg.col = "forestgreen", seg.lty = "dashed")

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)

## `scores` and `eigenvals` also work
scrs <- scores(mod)
str(scrs)
head(scores(mod, 1:4, display = "sites"))
# group centroids/medians 
scores(mod, 1:4, display = "centroids")
# eigenvalues from the underlying principal coordinates analysis
eigenvals(mod) 
## try out bias correction; compare with mod3
(mod3B <- betadisper(dis, groups, type = "median", bias.adjust=TRUE))
anova(mod3B)
permutest(mod3B, permutations = 99)

## should always work for a single group
group <- factor(rep("grazed", NROW(sp)))
(tmp <- betadisper(dis, group, type = "median"))
(tmp <- betadisper(dis, group, type = "centroid"))

## simulate missing values in 'd' and 'group'
## using spatial medians
groups[c(2,20)] <- NA
dis[c(2, 20)] <- NA
mod2 <- betadisper(dis, groups) ## messages
mod2
permutest(mod2, permutations = 99)
anova(mod2)
plot(mod2)
boxplot(mod2)
plot(TukeyHSD(mod2))

## Using group centroids
mod3 <- betadisper(dis, groups, type = "centroid")
mod3
permutest(mod3, permutations = 99)
anova(mod3)
plot(mod3)
boxplot(mod3)
plot(TukeyHSD(mod3))





###probamos
mod <- rda(sp, scale = TRUE)

## plot the PCA
plot(mod, scaling = 3)


 with(factors, levels(tr_site))

 scl <- 3 ## scaling = 3
 colvec <- c("red2", "green4", "mediumblue","orange")
 plot(mod, type = "n", scaling = scl)
 
 with(factors, points(mod, display = "sites", col = colvec[tr_site],
                       scaling = scl, pch = 21, bg = colvec[tr_site]))
 
 head(with(factors, colvec[tr_site]))
 text(mod, display = "species", scaling = scl, cex = 0.5, col = "darkcyan")
 with(factors, legend("topright", legend = levels(tr_site), bty = "n",
                       col = colvec, pch = 21, pt.bg = colvec))
 
 ####most influencial species
 (sim <- with(factors, simper(sp, tr_site)))
 summary(sim)
 
 ####3
 str(scrs, max = 1)
 scrs <- scores(mod, display = c("sites", "species"), scaling = scl)
 xlim <- with(scrs, range(2.5,-2.5))
 ylim <- with(scrs, range(2, -2))
 plot.new()
 plot.window(xlim = xlim, ylim = ylim, asp = 1)
 abline(h = 0, lty = "dotted")
 abline(v = 0, lty = "dotted")
 with(factors, points(scrs$sites, col = colvec[tr_site],
                       pch = 21, bg = colvec[tr_site]))
 with(scrs, text(species, labels = rownames(species),
                 col = "darkcyan", cex = 0.8))
 with(factors, legend("topright", legend = levels(tr_site), bty = "n",
                       col = colvec, pch = 21, pt.bg = colvec))
 axis(side = 1)
 axis(side = 2)
 title(xlab = "PC 1", ylab = "PC 2")
 box()
 

 ###solo site
 
 ## plot the PCA
 plot(mod, scaling = 3)
 
 
 with(factors, levels(site))
 
 scl <- 3 ## scaling = 3
 colvec <- c("red2", "green4")
 plot(mod, type = "n", scaling = scl)
 
 with(factors, points(mod, display = "sites", col = colvec[site],
                      scaling = scl, pch = 21, bg = colvec[site]))
 
 head(with(factors, colvec[site]))
 text(mod, display = "species", scaling = scl, cex = 0.5, col = "darkcyan")
 with(factors, legend("topright", legend = levels(site), bty = "n",
                      col = colvec, pch = 21, pt.bg = colvec))
 
 
 (sim <- with(factors, simper(sp, site)))
 summary(sim)
 
 ####3
 str(scrs, max = 1)
 scrs <- scores(mod, display = c("sites", "species"), scaling = scl)
 xlim <- with(scrs, range(2.5,-2.5))
 ylim <- with(scrs, range(2, -2))
 plot.new()
 plot.window(xlim = xlim, ylim = ylim, asp = 1)
 abline(h = 0, lty = "dotted")
 abline(v = 0, lty = "dotted")
 with(factors, points(scrs$sites, col = colvec[tr_site],
                      pch = 21, bg = colvec[tr_site]))
 with(scrs, text(species, labels = rownames(species),
                 col = "darkcyan", cex = 0.8))
 with(factors, legend("topright", legend = levels(tr_site), bty = "n",
                      col = colvec, pch = 21, pt.bg = colvec))
 axis(side = 1)
 axis(side = 2)
 title(xlab = "PC 1", ylab = "PC 2")
 box()
 
 
 