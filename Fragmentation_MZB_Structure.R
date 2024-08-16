##################################### Fragmentation ~ MZB Structure ###
##################################### Rebecca Oester ##################
######################################## 2023 #########################
## Leaf-associated macroinvertebrate assemblage and leaf litter breakdown in headwater streams depend on local riparian vegetation ##


rm(list=ls())


##### Load packages ####
library(ggplot2)
library(ggpubr)
library(nlme)
library(dplyr)
library(stringr)
library(reshape2)
library(vegan)
library(ggtext)
library(sjPlot)
library(lmerTest)
library(MuMIn)
library(lme4)
library(predictmeans)
library(MASS)
library(dplyr)
library(patchwork)

##### color vectors #####
bw=c("black", "grey")
Landscol<- c("#d86365", "#008837")
Landscol <- (c("#008837", "#d86365"))

Leafcol <- c("#d7191c", "#fdae61", "916fdb")

#################### 1. Shredder abundance ##################

###### A) Data preparation ####
dat.tot <- read.delim("datTOT.txt")
dat.tot$SiteLeaf<- paste(dat.tot$Site, dat.tot$Leaf, sep="")
dat.tot<- subset(dat.tot, !is.na(dat.tot$Stream))
dat.tot$Region<- as.factor(dat.tot$Region)
levels(dat.tot$Region)[levels(dat.tot$Region)=="TG"] <- "North"
levels(dat.tot$Region)[levels(dat.tot$Region)=="TI"] <- "South"
dat.tot$Landscape<- as.factor(dat.tot$Landscape)
levels(dat.tot$Landscape)[levels(dat.tot$Landscape)=="A"] <- "non-forested"
levels(dat.tot$Landscape)[levels(dat.tot$Landscape)=="F"] <- "forested"
dat.tot$Landscape<- factor(dat.tot$Landscape, levels= c("forested", "non-forested"))


hist(log(dat.tot$lambda.correct)) ### transform with natural logarithm
dat.tot$log.lambda.correct <- log10(dat.tot$lambda.correct) ### NANs because of negative values do not work for log
dat.tot$RegionLandscape <- paste(dat.tot$Region, dat.tot$Landscape)
dat.tot$RegionLandscape<- as.factor(dat.tot$RegionLandscape)

dat.tot <- subset(dat.tot, dat.tot$Leaf!= "Mix")



dat.tot<- dat.tot %>%
  mutate(Position=(ifelse(Site=="TG1F" | Site=="TG2A" | Site=="TG3A" | Site=="TG4F" | Site=="TI1F" | Site=="TI2F" | Site=="TI3A" | Site=="TI4A", "upstream","downstream")))


dat.tot$LandscapePosition <- paste(dat.tot$Landscape, dat.tot$Position, sep="_")

### Shred abundance 
shred <- dat.tot %>%
  dplyr::group_by(SiteLeaf) %>%
  dplyr::mutate(SumTotalShred = sum(Total[ActiveShredder=="YES"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)

shred <- distinct(shred, SiteLeaf, .keep_all = TRUE) #1 row per bag

hist(log10(shred$SumTotalShred+1))# as good as it gets

 
 dat.tot.shorta<- dat.tot[!duplicated(dat.tot$SiteLeaf),] ### shorten the dataset to 32, 16 sites * 2 leaves = 32 rows


 dat.tot.shorta <- dat.tot.shorta[complete.cases(dat.tot.shorta$log.lambda.correct), ] ## 28 rows left

###### B) Check Lambda ####
lam<- aggregate((lambda.correct*1000)~Region*Landscape*Leaf, data=dat.tot.shorta, mean) ### lambda.f 3 times bigger in F than A [from g dd-1 to ==> mg dd-1]
lam.sd<- aggregate((lambda.correct*1000)~Region*Landscape*Leaf, data=dat.tot.shorta, sd)

ggplot(data=dat.tot.shorta) + theme_bw()+
  geom_point(aes(Sample_ID, log.lambda.correct, colour=Leaf, shape=Region))+
  theme(axis.text.x = element_text(angle=-90))+facet_wrap(~Landscape+Region) # between 10-12 datapoints per subplot in same ranges


###### C) Plots #####

as<- ggplot(data=shred) +
  labs (x= expression(paste(log[10]," shredder abundance")), y = expression(paste(log[10]," ", lambda [F]," [mg ", dd^-1, "]")))+
  theme_bw() +
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  geom_point(aes(log10(SumTotalShred+1), log10(lambda.correct*1000), fill=Leaf, shape=Region, alpha=Landscape, size=3))+
  scale_fill_manual(values=Leafcol)+
  scale_alpha_manual(values = c(0.4, 1))+
  scale_shape_manual(values = c(21, 24))+
  theme(legend.position="none")



###### D) Models #####

shred <- shred[complete.cases(shred$log.lambda.correct), ] ## 31 rows left

#d <- lme(log.lambda.correct ~ log10(SumTotalShred+1)+Landscape+Region+Leaf+Position, data=shred, random = (~ 1|Stream))
d <- lme(log.lambda.correct ~ log10(SumTotalShred+1)+Landscape+Region+Leaf, data=shred, random = (~ 1|Stream))
d <- lme(log.lambda.correct ~ log10(SumTotalShred+1), data=shred, random = (~ 1|Stream))

summary((d))
car::Anova(d, type=3, icontrasts=c("contr.sum", "contr.poly"))
r.squaredGLMM(d)
qqnorm(resid(d))
qqline(resid(d))


#### 2. Shredder diversity ##########################################

############# first we need to calculate richness and shannon per site and leaf

shred <- dat.tot %>%
  dplyr::group_by(Sample_ID) %>%
  dplyr::mutate(SumTotalShred = sum(Total[ActiveShredder=="YES"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)



dat.long<-dcast(shred, Sample_ID~Taxon, value.var = "Total", sum) # transpose table based on Taxon and Site, summing up the Total per Taxon
row.names(dat.long) <- dat.long$Sample_ID # make row names the Sample ID
dat.long.l.shredding<- dat.long[,-1] # delete first column with Sample ID 

### delete columns that are not shredders
s <-subset(dat.tot, dat.tot$ActiveShredder=="YES")
s$Taxon <- as.factor(s$Taxon)
levels(s$Taxon)
l <- (levels(s$Taxon))

dat.long.l.shredding<- subset(dat.long, select=l)


#### shannon 
div.l.shred<- diversity(dat.long.l.shredding)
div.l.shred<- as.data.frame(div.l.shred)
hist((div.l.shred$div.l.shred), breaks=20)
div.l.shred["Sample_ID"] <- rownames(div.l.shred)
#dat.tot.l.shredding <- merge (dat.tot.l.shredding, div.l.shred, by="Sample_ID")
dat.tot.l.shredding <- merge (shred, div.l.shred, by="Sample_ID")


#### species richness
#spec.l.shredding <- specnumber(dat.long.l.shredding) # Z?hlt wieviele verschiedene Taxa es pro Standort hat
#spec1.l.shredding <-as.data.frame(spec.l.shredding)
#spec1.l.shredding["SiteLeaf"] <- rownames(spec1.l.shredding)
#dat.tot.l.shredding <- merge (dat.tot.l.shredding, spec1.l.shredding, by="SiteLeaf")

dat.tot.shortd<- dat.tot.l.shredding[!duplicated(dat.tot.l.shredding$Sample_ID),] ### shorten the dataset to 170

dat.tot.shortd<- dat.tot.shortd %>% group_by(SiteLeaf)%>%
  mutate(MeanDiv = mean(div.l.shred))

dat.tot.shortd<- dat.tot.shortd[!duplicated(dat.tot.shortd$SiteLeaf),] ### shorten the dataset to 32

dat.tot.shortd <- dat.tot.shortd[complete.cases(dat.tot.shortd$log.lambda.correct), ] ## 31

dat.tot.shortd$LandscapeRegion<- paste(dat.tot.shortd$Landscape, dat.tot.shortd$Region, sep="")


###### A) Plots #############

ss<-ggplot(data=dat.tot.shortd) +
  labs (x="shredder diversity", y = expression(paste(log[10]," ", lambda [F]," [mg ", dd^-1, "]")))+
  theme_bw() +
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  geom_point(aes((MeanDiv), log10(lambda.correct*1000), fill=Leaf, shape=Region, alpha=Landscape, size=3))+
  #geom_smooth(aes(log(MeanTotal), log.lambda.correct), color="black", method=lm, se=FALSE)+
  scale_fill_manual(values=Leafcol)+
  scale_alpha_manual(values = c(0.4, 1))+
  scale_shape_manual(values = c(21, 24))+
  theme(legend.position="none")





###### B) Models ####
m <- lme(log.lambda.correct ~ div.l.shred+Landscape+Region+Leaf+Position, data=dat.tot.shortd, random = (~ 1|Stream))
#m <- lme(log.lambda.correct ~ div.l.shred+Landscape+Region+Leaf, data=dat.tot.shortd, random = (~ 1|Stream))
m <- lme(log.lambda.correct ~ div.l.shred, data=dat.tot.shortd, random = (~ 1|Stream))


summary((m))
car::Anova(m, type=3, icontrasts=c("contr.sum", "contr.poly"))
qqnorm(resid(m))
qqline(resid(m))
r.squaredGLMM(m)


##### 3. Shredder Biomass ####

shred <- dat.tot %>%
  dplyr::group_by(Sample_ID) %>%
  dplyr::mutate(Sum_BiomassShred = sum(TotalDryMass[ActiveShredder=="YES"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)

dat.bm.test<- shred[!duplicated(shred$Sample_ID),] ### shorten the dataset so that only one value of Sum_Biomass exist per bag

dat.bm.test<- dat.bm.test %>% dplyr::group_by(SiteLeaf) %>%
  dplyr::mutate(Mean_Sum_Biomass = mean(Sum_Biomass, na.rm=TRUE)) #### calculate the mean of the Total Dry Mass in SiteLeaf treatment combination

dat.bm.test<- dat.bm.test[!duplicated(dat.bm.test$SiteLeaf),] ### shorten the dataset to 32

dat.tot.shortb<- dat.bm.test[complete.cases(dat.bm.test$log.lambda.correct) , ] ### 46 rows left
dat.tot.shortb$LandscapeRegion<- paste(dat.tot.shortb$Landscape, dat.tot.shortb$Region, sep="")



#### How much Gammarid biomass? ####
gamm<- subset(dat.tot, dat.tot$Genus=="Gammarus")

dat.gamm<- gamm %>% dplyr::group_by(Sample_ID) %>%
  dplyr::mutate(Sum_Biomass = sum(TotalDryMass, na.rm=TRUE)) #### calculate the sum of the Total Dry Mass in each bag


min(dat.gamm$Sum_Biomass)
max(dat.gamm$Sum_Biomass)

###### A) Plots #####

bs<-ggplot(data=dat.tot.shortb) +
  labs (x= expression(paste(log[10]," shredder biomass [mg]")), y = expression(paste(log[10]," ", lambda [F]," [mg ", dd^-1, "]")))+
  theme_bw() +
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  geom_point(aes(log10(Mean_Sum_Biomass), log10(lambda.correct*1000), fill=Leaf, shape=Region, alpha=Landscape), size=4.5)+
  #geom_smooth(aes(log(MeanTotal), log.lambda.correct), color="black", method=lm, se=FALSE)+
  scale_fill_manual(values=Leafcol)+
  scale_alpha_manual(values = c(0.4, 1))+
  scale_shape_manual(values = c(21, 24))+
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) ,
         color= guide_legend(override.aes = list(color= bw )))+
  theme(legend.position = c(0.3, 0.80), legend.title = element_text(size=12), legend.text = element_text(size=12), legend.direction="horizontal")+
  labs(fill="Leaf:", shape="Region:" ,alpha="Riparian vegetation:")

###### B) Models ####

#p <- lme(log.lambda.correct ~ log10(Mean_Sum_Biomass+1)+Landscape+Region+Leaf+Position, data=dat.tot.shortb, random = (~ 1|Stream))
p <- lme(log.lambda.correct ~ log10(Mean_Sum_Biomass+1)+Landscape+Region+Leaf, data=dat.tot.shortb, random = (~ 1|Stream))
summary(p)
r.squaredGLMM(p) ## m=marginal (only fixed effects), c=conditional (fixed+random)
qqnorm(resid(p))
qqline(resid(p))
car::Anova(p, type=3, icontrasts=c("contr.sum", "contr.poly"))


#### 4. Figure making ##################

(as)/(ss)/(bs)+ plot_annotation(tag_levels ="A") 

