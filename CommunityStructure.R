##################################### Community composition ###########
##################################### Rebecca Oester ##################
######################################## 2023 #########################
## Leaf-associated macroinvertebrate assemblage and leaf litter breakdown in headwater streams depend on local riparian vegetation ##


rm(list=ls())

#### packages ####
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
library(MASS)
library(patchwork)

rm(list=ls())

#### Load Data ####
dat.tot <- read.delim("datTOT.txt")
dat.tot$SiteLeaf<- paste(dat.tot$Site, dat.tot$Leaf, sep="")
dat.tot$Leaf<- as.factor(dat.tot$Leaf)
dat.tot$Stream<- as.factor(dat.tot$Stream)
dat.tot$Region<- as.factor(dat.tot$Region)
levels(dat.tot$Region)[levels(dat.tot$Region)=="TG"] <- "North"
levels(dat.tot$Region)[levels(dat.tot$Region)=="TI"] <- "South"
dat.tot$Region<- factor(dat.tot$Region, levels= c("North", "South"))

dat.tot$Landscape<- as.factor(dat.tot$Landscape)
levels(dat.tot$Landscape)[levels(dat.tot$Landscape)=="F"] <- "forested"
levels(dat.tot$Landscape)[levels(dat.tot$Landscape)=="A"] <- "non-forested"
dat.tot$Landscape<- factor(dat.tot$Landscape, levels= c("forested", "non-forested"))


### define all Gammarus fossarum and pulex to Gammarus sp. to make the taxonomic resolution equal
dat.tot$Taxon[dat.tot$Taxon=="Gammarus fossarum"]<- "Gammarus sp."
dat.tot$Taxon[dat.tot$Taxon=="Gammarus pulex"]<- "Gammarus sp."


dat.tot$Sample_ID<- as.factor(dat.tot$Sample_ID)
levels(dat.tot$Sample_ID)

dat.tot <- subset(dat.tot, dat.tot$Leaf!= "Mix")
dat.tot <- as.data.frame(droplevels(dat.tot))

dat.tot<- dat.tot %>%
  mutate(Position=(ifelse(Site=="TG1F" | Site=="TG2A" | Site=="TG3A" | Site=="TG4F" | Site=="TI1F" | Site=="TI2F" | Site=="TI3A" | Site=="TI4A", "upstream","downstream")))
                         

dat.tot$LandscapePosition <- paste(dat.tot$Landscape, dat.tot$Position, sep="_")

Leafcol <- c("#d7191c", "#fdae61", "916fdb")


########### 1. Abundance ##############

##### 1.1 Shredders ####
shred <- dat.tot %>%
  dplyr::group_by(Sample_ID) %>%
  dplyr::mutate(SumTotalShred = sum(Total[ActiveShredder=="YES"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)

shred <- distinct(shred, Sample_ID, .keep_all = TRUE) #1 row per bag

hist(log10(shred$SumTotalShred+1))# as good as it gets

meanAbundanceShred<- aggregate(SumTotalShred~Region+Landscape, shred, mean) # [ shannon diversity per bag]
meanAbundanceShred.sd<- aggregate(SumTotalShred~Region+Landscape, shred, "sd")



###### 1.1.1 model ####
shred$Region <- factor(shred$Region, levels=c("North", "South"))
shredmod <- lme(log10(SumTotalShred+1) ~Landscape*Region, random = (~1|Stream), data=shred)
summary(shredmod)
shred$Region <- factor(shred$Region, levels=c("South", "North"))
shred$Landscape <- factor(shred$Landscape, levels=c("forested", "non-forested"))
summary(shredmod)
shred$Region <- factor(shred$Region, levels=c("North", "South"))

#shredmod <- lme(log10(SumTotalShred+1) ~Landscape*Region+Position, random = (~1|Stream), data=shred)
qqnorm(resid(shredmod)) # ok
qqline(resid(shredmod))
plot(shredmod) #ok
levels(shred$Landscape)
summary(shredmod)
car::Anova(shredmod, type=3, icontrasts=c("contr.sum", "contr.poly"))
r.squaredGLMM(shredmod) ## m=marginal (only fixed effects), c=conditional (fixed+random)

###### 1.1.2 figure #####
sa<-ggplot(shred)+theme_bw()+
  geom_boxplot(aes(y=log10(SumTotalShred+1), x=Landscape), outlier.shape = NA)+
  scale_color_manual(values=Leafcol)+
  labs (x= "", y = "")+
  #labs(fill="Leaf:", shape="Region:")+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+scale_shape_discrete(breaks=c("North","South"))+
  geom_point(aes(y=log10(SumTotalShred+1), x=Landscape, color=Leaf, shape=Region), position = position_dodge(width = 0.75))+ facet_wrap(~Region)+
  ggtitle("Shredder")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-0.5,2.6)

p<-ggplot(shred)+theme_bw()+
  geom_boxplot(aes(y=log10(SumTotalShred+1), x=Landscape), outlier.shape = NA)+
  scale_color_manual(values=Leafcol)+
  labs (x= "", y = "")+
  #labs(fill="Leaf:", shape="Region:")+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+scale_shape_discrete(breaks=c("North","South"))+
  geom_point(aes(y=log10(SumTotalShred+1), x=Landscape, color=Leaf, shape=Region), position = position_dodge(width = 0.75))+ facet_wrap(~Region)+
  ggtitle("Shredder")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-0.5,2.6)

dat_text <- data.frame(
  label = c("*** ", ""),
  Region   = c("North", "South"))


sa<-p + geom_text(
  data    = dat_text,
  mapping = aes(x = -Inf, y = -Inf, label = label,
                hjust   = -2.5,
                vjust   = -14), size=10)

##### 1.2 EPT  #############################

EPT <- dat.tot %>%
  dplyr::group_by(Sample_ID) %>%
  dplyr::mutate(SumTotalEPT = sum(Total[Order=="Ephemeroptera" | Order=="Plecoptera" | Order=="Trichoptera"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)

EPT <- distinct(EPT, Sample_ID, .keep_all = TRUE) #1 row per bag

hist(log(EPT$SumTotalEPT+1))# as good as it gets


###### 1.2.1 model ####
EPT$Region <- factor(EPT$Region, levels=c("North", "South"))
EPTmod <- lme(log10(SumTotalEPT+1) ~Landscape*Region, random = (~1|Stream), data=EPT)
EPT$Region <- factor(EPT$Region, levels=c("South", "North"))

#EPTmod <- lme(log10(SumTotalEPT+1) ~Landscape*Region+Position, random = (~1|Stream), data=EPT)
EPT$Region <- factor(EPT$Region, levels=c("North", "South"))
qqnorm(resid(EPTmod)) # ok
qqline(resid(EPTmod))
plot(EPTmod) #ok
summary(EPTmod)
car::Anova(EPTmod, type=3, icontrasts=c("contr.sum", "contr.poly"))
r.squaredGLMM(EPTmod) ## m=marginal (only fixed effects), c=conditional (fixed+random)


###### 1.2.2 figure ####
p<- ggplot(EPT)+theme_bw()+
  geom_boxplot(aes(y=log10(SumTotalEPT+1), x=Landscape), outlier.shape = NA)+
  scale_color_manual(values=Leafcol)+
  labs (x= "", y=expression(paste(log[10]," Abundance")))+
  labs(fill="Leaf:", shape="Region:")+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+scale_shape_discrete(breaks=c("North","South"))+
  geom_point(aes(y=log10(SumTotalEPT+1), x=Landscape, color=Leaf, shape=Region), position = position_dodge(width = 0.75))+ facet_wrap(~Region)+
  ggtitle("EPT")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-0.5,2.6)


dat_text <- data.frame(
  label = c("*** ", ""),
  Region   = c("North", "South"))


ea<-p + geom_text(
  data    = dat_text,
  mapping = aes(x = -Inf, y = -Inf, label = label,
                hjust   = -2.5,
                vjust   = -14), size=10)+
  labs (x= "", y=expression(paste(log[10]," Abundance")))+ggtitle("EPT")



##### 1.3 Shredder ~ EPT ####
abund <- left_join(shred, EPT, by="Sample_ID") ## only take rows that have values for both 

plot(log(abund$SumTotalEPT+1), log(abund$SumTotalShred+1))
hist(log(abund$SumTotalEPT+1))
qqnorm(log(abund$SumTotalEPT+1))
shapiro.test(log(abund$SumTotalEPT+1)) ## not signiciantly different from normal
hist(log(abund$SumTotalShred+1))
qqnorm(log(abund$SumTotalShred+1))
shapiro.test(log(abund$SumTotalShred+1)) ## signiciantly different from normal

cor.test(log10(abund$SumTotalEPT+1), log10(abund$SumTotalShred+1), method="kendall") ## significant and strong correlation 



#### 2. Diversity #############

##### 2.1 Shredders ####
shred <- dat.tot %>%
  dplyr::group_by(Sample_ID) %>%
  dplyr::mutate(SumTotalShred = sum(Total[ActiveShredder=="YES"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)



dat.long<-dcast(shred, Sample_ID~Taxon, value.var = "Total", sum) # transpose table based on Taxon and Site, summing up the Total per Taxon
row.names(dat.long) <- dat.long$Sample_ID # make row names the Sample ID
dat.long<- dat.long[,-1] # delete first column with Sample ID 


### delete columns that are not shredders
s <-subset(dat.tot, dat.tot$ActiveShredder=="YES")
s$Taxon <- as.factor(s$Taxon)
levels(s$Taxon)
l <- (levels(s$Taxon))

dat.long<- subset(dat.long, select=l)


#### species richness
spec <- specnumber(dat.long) 
spec1 <-as.data.frame(spec)
spec1["Sample_ID"] <- rownames(spec1)
spec2<- merge(spec1, shred, by="Sample_ID")

meanRichShred<- aggregate(spec~Region+Landscape, spec2, mean) # [ shannon diversity per bag]
meanRichShred.sd<- aggregate(spec~Region+Landscape, spec2, "sd")


#### shannon
div<- diversity(dat.long)
div<- as.data.frame(div)
div["Sample_ID"] <- rownames(div)
div2<- merge(div, shred, by="Sample_ID")
div2<- distinct(div2, Sample_ID, .keep_all = TRUE) ## one row per bag

meanDivShred<- aggregate(div~Region+Landscape, div2, mean) # [ shannon diversity per bag]
meanDivShred.sd<- aggregate(div~Region+Landscape, div2, "sd")



###### 2.1.1 model #####
hist(log(div2$div+1))
hist((div2$div))

od<- lme(div~Landscape*Region, random =(~ 1|Stream), data=div2)
#od<- lme(div~Landscape*Region+Position, random =(~ 1|Stream), data=div2)
div2$Region <- factor(div2$Region, levels=c("South", "North"))

summary(od)

div2$Region <- factor(div2$Region, levels=c("North", "South"))
qqnorm(resid(od))
qqline(resid(od))
plot(od)
plot(ranef(od))
car::Anova(od, type=3, icontrasts=c("contr.sum", "contr.poly"))
r.squaredGLMM(od) ## m=marginal (only fixed effects), c=conditional (fixed+random)

###### 2.1.2 figure ####
p<- ggplot(div2)+theme_bw()+
  geom_boxplot(aes(y=div, x=Landscape), outlier.shape = NA)+
  scale_color_manual(values=Leafcol)+
  labs (x= "", y="")+
  labs(fill="Leaf:", shape="Region:")+
  theme(legend.position = "none")+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank())+
  theme(strip.background = element_blank(), strip.text.x = element_blank())+
  geom_point(aes(y=div, x=Landscape, color=Leaf, shape=Region), position = position_dodge(width = 0.75))+ facet_wrap(~Region)+
  ylim(-0.5,2.6)

dat_text <- data.frame(
  label = c("*** ", ""),
  Region   = c("North", "South"))


sd<-p + geom_text(
  data    = dat_text,
  mapping = aes(x = -Inf, y = -Inf, label = label,
                hjust   = -2.5,
                vjust   = -14), size=10)

 
##### 2.2 EPT ##################

EPT <- dat.tot %>%
  dplyr::group_by(Sample_ID) %>%
  dplyr::mutate(SumTotalEPT = sum(Total[Order=="Ephemeroptera" | Order=="Plecoptera" | Order=="Trichoptera"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)

dat.long<-dcast(EPT, Sample_ID~Taxon, value.var = "Total", sum) # transpose table based on Taxon and Site, summing up the Total per Taxon
row.names(dat.long) <- dat.long$Sample_ID # make row names the Sample ID
dat.long<- dat.long[,-1] # delete first column with Sample ID 


### delete columns that are not shredders
s <-subset(dat.tot, Order=="Ephemeroptera" | Order=="Plecoptera" | Order=="Trichoptera")
s$Taxon <- as.factor(s$Taxon)
levels(s$Taxon)
l <- (levels(s$Taxon))

dat.long<- subset(dat.long, select=l)

#### species richness
spec <- specnumber(dat.long) 
spec1 <-as.data.frame(spec)
spec1["Sample_ID"] <- rownames(spec1)
spec2<- merge(spec1, EPT, by="Sample_ID")

min(spec2$spec)
max(spec2$spec)

meanRichEPT<- aggregate(spec~Region+Landscape, spec2, mean) # [ shannon diversity per bag]
meanRichEPT.sd<- aggregate(spec~Region+Landscape, spec2, FUN="sd")


#### shannon 
dive<- diversity(dat.long)
dive<- as.data.frame(dive)
dive["Sample_ID"] <- rownames(dive)
div2e<- merge(dive, EPT, by="Sample_ID")
div2e<- distinct(div2e, Sample_ID, .keep_all = TRUE) ## one row per bag

min(div2e$dive)
max(div2e$dive)

meanDivEPT<- aggregate(dive~Region+Landscape, div2e, mean) # [ shannon diversity per bag]
meanDivEPT.sd<- aggregate(dive~Region+Landscape, div2e, "sd")


min(div2e$dive)
max(div2e$dive)
mean(div2e$dive)
sd(div2e$dive)


###### 2.2.1 model ####
hist(log10(div2e$dive+1))
hist((div2e$dive))

ode<- lme(dive~Landscape*Region, random =(~ 1|Stream), data=div2e)
div2e$Region <- factor(div2e$Region, levels=c("South", "North"))
div2e$Region <- factor(div2e$Region, levels=c("North", "South"))


#ode<- lme(dive~Landscape*Region+Position, random =(~ 1|Stream), data=div2e)
summary(ode)
qqnorm(resid(ode))
qqline(resid(ode))
plot(ode)
plot(ranef(ode))
car::Anova(ode, type=3, icontrasts=c("contr.sum", "contr.poly"))
r.squaredGLMM(ode) ## m=marginal (only fixed effects), c=conditional (fixed+random)
div2e$Region <- factor(div2e$Region, levels=c("North", "South"))

###### 2.2.2 figure #####
p<- ggplot(div2e)+theme_bw()+
  geom_boxplot(aes(y=dive, x=Landscape), outlier.shape = NA)+
  scale_color_manual(values=Leafcol)+
  labs (x= "", y=expression(paste(" Diversity")))+
  labs(fill="Leaf:", shape="Region:")+
  theme(legend.position = "none")+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  theme(strip.background = element_blank(), strip.text.x = element_blank())+
  geom_point(aes(y=dive, x=Landscape, color=Leaf, shape=Region), position = position_dodge(width = 0.75))+ facet_wrap(~Region)+
  ylim(-0.5,2.6)


dat_text <- data.frame(
  label = c("  *  ", "  *  "),
  Region   = c("North", "South"))


ed<-p + geom_text(
  data    = dat_text,
  mapping = aes(x = -Inf, y = -Inf, label = label,
                hjust   = -2.5,
                vjust   = -14), size=10)

##### 2.3 Shredder ~EPT ####
diversity <- left_join(div, dive, by="Sample_ID") ## only take rows that have values for both 

plot((diversity$div), (diversity$dive))
hist((diversity$div))
qqnorm((diversity$div))
shapiro.test((diversity$div)) ## signiciantly different from normal
hist((diversity$dive))
qqnorm((diversity$dive))
shapiro.test((diversity$dive)) ## signiciantly different from normal

cor.test((diversity$div), (diversity$dive), method="kendall") ## significant and strong correlation 


#### 3.Biomass #############

###### 3.1 Shredders ############
shred <- dat.tot %>%
  dplyr::group_by(Sample_ID) %>%
  dplyr::mutate(Sum_BiomassShred = sum(TotalDryMass[ActiveShredder=="YES"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)

dat.bm.s.test<- shred[!duplicated(shred$Sample_ID),] ### shorten the dataset so that only one value of Sum_Biomass exist per bag
hist(log(dat.bm.s.test$Sum_BiomassShred))

min(dat.bm.s.test$Sum_BiomassShred)
mean(dat.bm.s.test$Sum_BiomassShred)
max(dat.bm.s.test$Sum_BiomassShred)
sd(dat.bm.s.test$Sum_BiomassShred)


meanMassShred<- aggregate(Sum_BiomassShred~Region+Landscape, dat.bm.s.test, mean) # [ shannon diversity per bag]
meanMassShred.sd<- aggregate(Sum_BiomassShred~Region+Landscape, dat.bm.s.test, "sd")


####### 3.1.1 model ####
dat.bm.s.test$Region <- factor(dat.bm.s.test$Region, levels=c("North", "South"))
obs0<- lme(log10(Sum_BiomassShred+1)~Landscape*Region, random =(~ 1|Stream), data=dat.bm.s.test)
dat.bm.s.test$Region <- factor(dat.bm.s.test$Region, levels=c("South", "North"))
obs1<- lme(log10(Sum_BiomassShred+1)~Landscape*Region, random =(~ 1|Stream), data=dat.bm.s.test)
#obs<- lme(log10(Sum_BiomassShred+1)~Landscape*Region+Position, random =(~ 1|Stream), data=dat.bm.s.test)
dat.bm.s.test$Region <- factor(dat.bm.s.test$Region, levels=c("North", "South"))
summary(obs0)
summary(obs1)
qqnorm(resid(obs1))
qqline(resid(obs1))
plot(obs1)
plot(ranef(obs1))
car::Anova(obs1, type=3, icontrasts=c("contr.sum", "contr.poly"))
car::Anova(obs0, type=3, icontrasts=c("contr.sum", "contr.poly"))
anova.lme(obs0)
r.squaredGLMM(obs1)

##### 3.1.2 figure ####
p<- ggplot(dat.bm.s.test)+theme_bw()+
  geom_boxplot(aes(y=log10(Sum_BiomassShred+1), x=Landscape), outlier.shape = NA)+
  scale_color_manual(values=Leafcol)+
  labs (x= "", y="")+
  labs(fill="Leaf:", shape="Region:")+
  theme(legend.position = "none")+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  theme(strip.background = element_blank(), strip.text.x = element_blank())+
  geom_point(aes(y=log10(Sum_BiomassShred+1), x=Landscape, color=Leaf, shape=Region), position = position_dodge(width = 0.75))+ facet_wrap(~Region)+
  ylim(-0.5,2.6)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


dat_text <- data.frame(
  label = c("  *  ", "  .  "),
  Region   = c("North", "South"))


sb<-p + geom_text(
  data    = dat_text,
  mapping = aes(x = -Inf, y = -Inf, label = label,
                hjust   = -2.5,
                vjust   = -14), size=10)


##### 3.2 EPT ##############

dat.bm.s<- subset(dat.tot, dat.tot$Order =="Ephemeroptera" | dat.tot$Order=="Plecoptera" | dat.tot$Order == "Trichoptera")

dat.bm.s.1<- dat.bm.s %>% dplyr::group_by(Sample_ID) %>%
  dplyr::mutate(Sum_BiomassEPT = sum(TotalDryMass, na.rm=TRUE)) #### calculate the sum of the Total Dry Mass in each bag

dat.bm.s.teste<- dat.bm.s.1[!duplicated(dat.bm.s.1$Sample_ID),] ### shorten the dataset so that only one value of Sum_Biomass exist per bag
hist(log(dat.bm.s.teste$Sum_BiomassEPT))

meanMassEPT<- aggregate(Sum_BiomassEPT~Region+Landscape, dat.bm.s.teste, mean) # [ shannon diversity per bag]
meanMassEPT.sd<- aggregate(Sum_BiomassEPT~Region+Landscape, dat.bm.s.teste, "sd")


min(dat.bm.s.teste$Sum_BiomassEPT)
max(dat.bm.s.teste$Sum_BiomassEPT)
mean(dat.bm.s.teste$Sum_BiomassEPT)
sd(dat.bm.s.teste$Sum_BiomassEPT)

###### 3.2.1 model ####
obsept<- lme(log10(Sum_BiomassEPT+1)~Landscape*Region, random =(~ 1|Stream), data=dat.bm.s.teste)
#obsept<- lme(log10(Sum_BiomassEPT+1)~Landscape*Region+Position, random =(~ 1|Stream), data=dat.bm.s.teste)
dat.bm.s.teste$Region <- factor(dat.bm.s.teste$Region, levels=c("South", "North"))


summary(obsept)
qqnorm(resid(obsept))
qqline(resid(obsept))
plot(obsept)
plot(ranef(obsept))
car::Anova(obsept, type=3, icontrasts=c("contr.sum", "contr.poly"))
r.squaredGLMM(obsept)

dat.bm.s.teste$Region <- factor(dat.bm.s.teste$Region, levels=c("North", "South"))

##### 3.2.2 figure ####
p<- ggplot(dat.bm.s.teste)+theme_bw()+
  geom_boxplot(aes(y=log10(Sum_BiomassEPT+1), x=Landscape), outlier.shape = NA)+
  scale_color_manual(values=Leafcol)+
  labs (x= "", y=expression(paste(log[10]," Biomass [mg]")))+
  labs(color="Leaf:", shape="Region:")+
  theme(legend.position = c(0.25, 0.15), legend.title = element_text(size=14), legend.text = element_text(size=14), legend.direction="horizontal")+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  theme(strip.background = element_blank(), strip.text.x = element_blank())+
  geom_point(aes(y=log10(Sum_BiomassEPT+1), x=Landscape, color=Leaf, shape=Region), position = position_dodge(width = 0.75))+ facet_wrap(~Region)+
  ylim(-0.5,2.6)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))

dat_text <- data.frame(
  label = c("*** ", " ** "),
  Region   = c("North", "South"))


eb<-p + geom_text(
  data    = dat_text,
  mapping = aes(x = -Inf, y = -Inf, label = label,
                hjust   = -2.5,
                vjust   = -14), size=10)

##### 3.4 Shredder ~ EPT ####
biomass <- left_join(dat.bm.s.test, dat.bm.s.teste, by="Sample_ID") ## only take rows that have values for both 

plot(log(biomass$Sum_BiomassShred), log(biomass$Sum_BiomassEPT))

hist(log(biomass$Sum_BiomassShred))
qqnorm(log(biomass$Sum_BiomassShred+1))
shapiro.test(log(biomass$Sum_BiomassShred)) ## signiciantly different from normal

hist(log(biomass$Sum_BiomassEPT+1))
qqnorm(log(biomass$Sum_BiomassEPT))
shapiro.test(log(biomass$Sum_BiomassEPT)) ## signiciantly different from normal

cor.test(log(biomass$Sum_BiomassShred+1), log(biomass$Sum_BiomassEPT+1), method="kendall") ## significant and strong correlation 


#### 4. Figure making ####

(ea+sa)/(ed+sd)/(eb+sb)+ plot_annotation(tag_levels ="A") 

