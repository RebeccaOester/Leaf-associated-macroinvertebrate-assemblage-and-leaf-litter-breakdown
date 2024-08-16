##################################### Basic Results ###################
##################################### Rebecca Oester ##################
######################################## 2023 #########################
## Leaf-associated macroinvertebrate assemblage and leaf litter breakdown in headwater streams depend on local riparian vegetation ##


#### Results to report #####
rm(list=ls())
setwd("")

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

#### Data ####
dat.tot <- read.delim("datTOT.txt")
dat.tot$SiteLeaf<- paste(dat.tot$Site, dat.tot$Leaf, sep="")
dat.tot$Region<- as.factor(dat.tot$Region)
levels(dat.tot$Region)[levels(dat.tot$Region)=="TG"] <- "North"
levels(dat.tot$Region)[levels(dat.tot$Region)=="TI"] <- "South"
dat.tot$Landscape<- as.factor(dat.tot$Landscape)
levels(dat.tot$Landscape)[levels(dat.tot$Landscape)=="A"] <- "Non-Forest"
levels(dat.tot$Landscape)[levels(dat.tot$Landscape)=="F"] <- "Forest"

dat.tot <- subset(dat.tot, dat.tot$Leaf!= "Mix") ## 1631 remaining rows with only Alder and Ash as leaf treatments

#### Community ####
########### 1. Abundance ##############
plot(dat.tot$Total) ### outlier detected but no reason to remove it as this is a true value

#### How many individuals did we count? 
sum(dat.tot$Total) #### 31560 individuals
plot((dat.tot$Total))
hist(log((dat.tot$Total)))

#### How many Taxa could we distinguish? 
dat.tot$Taxon<- as.factor(dat.tot$Taxon)
levels(dat.tot$Taxon) ##### 93

##### How many of these Taxa are on Genus level? 
dat.tot <- dat.tot %>% mutate_all(na_if,"")
dat.tot.g<- subset(dat.tot, !is.na(dat.tot$Species))
dat.tot.g$Taxon <- factor(dat.tot.g$Taxon)
levels(dat.tot.g$Taxon) #### 63 Taxa are on Genus or species level
sum(dat.tot.g$Total) #### which corresponds to 12154 individuals 

##### How many of these Taxa are on Subfamily level? 
dat.tot.sf<- subset(dat.tot, is.na(dat.tot$Species))
dat.tot.sf <- subset(dat.tot.sf, !is.na(dat.tot.sf$Genus))
dat.tot.sf$Taxon <- factor(dat.tot.sf$Taxon)
levels(dat.tot.sf$Taxon) #### 18 Taxa are on Subfamily level
sum(dat.tot.sf$Total) #### which corresponds to 16735 individuals 

31560-16735-12154 ## = 2671 on family or order level

##### overall number of individuals on a single leaf bag 

dat.tot.lb<- dat.tot %>% group_by(Sample_ID) %>%
  mutate(SumTotal = sum(Total))

dat.tot.lb$Sample_ID<- as.factor(dat.tot.lb$Sample_ID)
dat.tot.lb.short<- distinct(dat.tot.lb, Sample_ID, .keep_all = TRUE) ## one row per bag

shapiro.test(log10(dat.tot.lb.short$SumTotal)) # normal
qqnorm(log10(dat.tot.lb.short$SumTotal), datax = TRUE) ## looks ok

hist(dat.tot.lb.short$SumTotal)
plot(dat.tot.lb.short$SumTotal)
min(dat.tot.lb.short$SumTotal) ## 17 individuals on leaf bag
max(dat.tot.lb.short$SumTotal) ## 992 individuals on leaf bag
mean(dat.tot.lb.short$SumTotal)## 166
sd(dat.tot.lb.short$SumTotal) ##  137.0282
hist(dat.tot.lb.short$SumTotal, breaks =50)


ggplot(data=dat.tot.lb.short) +
  labs (x="Site", y = "Abundance")+
  theme_bw() +
  geom_boxplot(aes(Landscape,SumTotal, fill=Leaf))+ 
  geom_point(aes(Landscape,SumTotal))+
  facet_wrap(~Region)
#### Abundances are higher in the North, in the Forest, and on Ash


##### shredder number of individuals on a single leaf bag 
shred<- subset(dat.tot, dat.tot$ActiveShredder=="YES")
shred$Taxon<- as.factor(shred$Taxon)
shred$Taxon<- factor(shred$Taxon)
levels(shred$Taxon) ### 15 Taxa belong to Active Shredders!!!

shred <- shred %>%
  group_by(Sample_ID) %>%
  mutate(SumTotalShred = sum(Total))

shred <- distinct(shred, Sample_ID, .keep_all = TRUE) #171 bags had >=1 shredder
hist(log10(shred$SumTotalShred))
shapiro.test(log(shred$SumTotalShred)) # not normal
qqnorm(log10(shred$SumTotalShred), datax = TRUE) ## looks weird

min(shred$SumTotalShred) ## 0 individuals on leaf bag for 189-171 = 18 but for the 171 at least 1 shredder
max(shred$SumTotalShred) ## 660 individuals on leaf bag
mean(shred$SumTotalShred)## 27.35673 of the subset of only shredders
sd(shred$SumTotalShred) ## 60.84654
hist(shred$SumTotalShred, breaks =50)
shapiro.test(log(shred$SumTotalShred)) # not normal
qqnorm(log(shred$SumTotalShred), datax = TRUE) ## 



##### EPT number of individuals on a single leaf bag 
EPT<- subset(dat.tot, dat.tot$Order =="Ephemeroptera" | dat.tot$Order=="Plecoptera" | dat.tot$Order == "Trichoptera")
EPT$Taxon<- as.factor(EPT$Taxon)
EPT$Taxon<- factor(EPT$Taxon)
levels(EPT$Taxon) #

EPT <- EPT %>%
  group_by(Sample_ID) %>%
  mutate(SumTotalEPT = sum(Total))

EPT <- distinct(EPT, Sample_ID, .keep_all = TRUE) #171 bags had >=1 shredder
hist(log10(EPT$SumTotalEPT))
shapiro.test(log(EPT$SumTotalEPT)) # not normal
qqnorm(log10(EPT$SumTotalEPT), datax = TRUE) ## looks weird

min(EPT$SumTotalEPT) ## 1
max(EPT$SumTotalEPT) ## 676 individuals on leaf bag
mean(EPT$SumTotalEPT)## 27.35673 of the subset of only shredders
sd(EPT$SumTotalEPT) ## 60.84654
hist(EPT$SumTotalEPT, breaks =50)
shapiro.test(log(EPT$SumTotalEPT)) # not normal
qqnorm(log(EPT$SumTotalEPT), datax = TRUE) ## 





#### how many Nemouridae? 
ggplot(data=shred)+theme_bw()+
  labs (x="", y="Taxonomic composition", fill="Taxonomic group")+
  geom_bar(stat="identity", position="fill", aes(x=Site, y=Total, fill=Family))+
  theme(text = element_text(size=34), axis.text.x = element_text(size=34))+
  guides(fill=guide_legend(title="All"))

ggplot(data=shred)+theme_bw()+
  labs (x="", y="Taxonomic composition", fill="Taxonomic group")+
  geom_bar(stat="identity", aes(x=Site, y=Total, fill=Family))+
  theme(text = element_text(size=34), axis.text.x = element_text(size=34))+
  guides(fill=guide_legend(title="All"))

nem <-aggregate(Total~Family+Site, dat=shred, sum) ### abs# nemouridae 3-900 ind/site
nem1<- nem %>% dplyr::group_by(Site) %>%
  dplyr::mutate(Sum_Site = sum(Total, na.rm=TRUE)) #### calculate the sum of the Total Dry Mass in each bag
nem1<- nem1 %>% dplyr::group_by(Site)%>%
  dplyr::mutate(Prop_Site = Total/Sum_Site*100) #### calculate the sum of the Total Dry Mass in each bag
aggregate(Prop_Site~Family, mean, data=nem1)
## ranging from 14-100% with an average of 78.54%


############ 2. Diversity #############

### we are taking the dat.tot.lb.short, so we only look at one bag per row

min(dat.tot.lb.short$SpeciesRichness) ## 4
max(dat.tot.lb.short$SpeciesRichness) ## 25
mean(dat.tot.lb.short$SpeciesRichness) ## 12.50
sd(dat.tot.lb.short$SpeciesRichness) ## 4.18

min(dat.tot.lb.short$ShannonIndex) # 0.60
max(dat.tot.lb.short$ShannonIndex) # 2.45
mean(dat.tot.lb.short$ShannonIndex) #1.53
sd(dat.tot.lb.short$ShannonIndex) #0.42


#### only looking at shredders

dat.s<- subset(dat.tot, dat.tot$ActiveShredder=="YES") # select only taxa that are real shredders so only 463 rows left

dat.long<-dcast(dat.s, Sample_ID~Taxon, value.var = "Total", sum) # transpose table based on Taxon and Site, summing up the Total per Taxon
row.names(dat.long) <- dat.long$Sample_ID # make row names the Sample ID
dat.long<- dat.long[,-1] # delete first column with Sample ID 

#### species richness 
spec <- specnumber(dat.long) 
spec1 <-as.data.frame(spec)
spec1["Sample_ID"] <- rownames(spec1)
min(spec1$spec) #1
max(spec1$spec) #7
mean(spec1$spec)#2.70
sd(spec1$spec)#1.36

#### shannon 
div<- diversity(dat.long)
div<- as.data.frame(div)
div["Sample_ID"] <- rownames(div)
min(div$div) #0
max(div$div) #1.58
mean(div$div) #0.66
sd(div$div)#0.46


############ 3. Biomass #############
hist(log(dat.tot.lb.short$Sum_Biomass), breaks=50) # plot a histogram to see the frequency distribution of log(Sum_Biomass) (logged as this looks more normal)

min(dat.tot.lb.short$Sum_Biomass) #2.0177 mg/bag
max(dat.tot.lb.short$Sum_Biomass) #687.9588 mg/bag 
mean(dat.tot.lb.short$Sum_Biomass) #76.28353 mg/bag 
sd(dat.tot.lb.short$Sum_Biomass) #93.78914


dat.bm.s<- subset(dat.tot, dat.tot$ActiveShredder=="YES")
dat.bm.s.1<- dat.bm.s %>% dplyr::group_by(Sample_ID) %>%
  dplyr::mutate(Sum_BiomassShred = sum(TotalDryMass, na.rm=TRUE)) #### calculate the sum of the Total Dry Mass in each bag

dat.bm.s.test<- dat.bm.s.1[!duplicated(dat.bm.s.1$Sample_ID),] ### shorten the dataset so that only one value of Sum_Biomass exist per bag
hist(log(dat.bm.s.test$Sum_BiomassShred), breaks=50) # plot a histogram to see the frequency distribution of log(Sum_Biomass) (logged as this looks more normal)

min(dat.bm.s.test$Sum_BiomassShred) #0.01647848 mg/bag
max(dat.bm.s.test$Sum_BiomassShred) #681.64 mg/bag
mean(dat.bm.s.test$Sum_BiomassShred) #55.26327 mg/bag 
sd(dat.bm.s.test$Sum_BiomassShred) #90.4552



#### 4. Decomposition ####
## Table 3

dat.ML.new <- read.delim ("datML.new.txt") ### 384 rows one for each mesh bag

dat.ML.new <- subset(dat.ML.new, dat.ML.new$Leaf!= "Mix") ## 256 remaining rows with only Alder and Ash as leaf treatments
dat.ML.new$MassLossAlder <- dat.ML.new$DW_beforeGrammAlder-dat.ML.new$DW_gramAlder_Adj
dat.ML.new$MassLossAsh <- dat.ML.new$DW_beforeGrammAsh-dat.ML.new$DW_gramAsh_Adjusted

plot(dat.ML.new$MassLossAlder~dat.ML.new$Loss_AbsolutAlder)
plot(dat.ML.new$MassLossAsh~dat.ML.new$Loss_AbsolutAsh)



k<- aggregate(k~Region+Landscape+Treatment, dat.ML.new, mean) # k means in g dd-1, 24 rows
k$k<- k$k*1000 # [mg dd-1]
k$k <- round(k$k, digits=2)
k.sd<- aggregate(k~Region+Landscape+Treatment, dat.ML.new, sd) # k sds in g dd-1, 24 rows
k.sd$k <- k.sd$k*1000# [mg dd-1]
k.sd$k <- round(k.sd$k, digits=2)



m<- aggregate(MassLossAlder~Region+Landscape+Treatment, dat.ML.new, mean) # mean of 4 replicate bags ==> Loss of dry weight from initial 5g 
m$loss<- m$MassLossAlder ## grams lost during the course of the experiment
m$loss <- round(m$loss, digits=2)
m.sd<- aggregate(MassLossAlder~Region+Landscape+Treatment, dat.ML.new, sd) # in
m.sd$sd <- m.sd$MassLossAlder
m.sd$sd <- round(m.sd$sd, digits=2)


ma<- aggregate(MassLossAsh~Region+Landscape+Treatment, dat.ML.new, mean) # mean of 4 replicate bags ==> Loss of dry weight from initial 5g 
ma$loss<- ma$MassLossAsh ## grams lost during the course of the experiment
ma$loss <- round(ma$loss, digits=2)
ma.sd<- aggregate(MassLossAsh~Region+Landscape+Treatment, dat.ML.new, sd) # in
ma.sd$sd <- ma.sd$MassLossAsh
ma.sd$sd <- round(ma.sd$sd, digits=2)


dat.ML.new.short <-  dat.ML.new[!duplicated(dat.ML.new$lambda.correct),] ### shorten the dataset so that only one per lambda.correct ==> 48 rows


hist(dat.ML.new.short$lambda.correct)

lambda<- aggregate(lambda.correct~Region+Landscape+Treatment, dat.ML.new.short, mean) # lambdaF means, 12 rows
lambda$lambda.correct<- lambda$lambda.correct*1000 # [mg dd-1]
lambda$lambda.correct <- round(lambda$lambda.correct, digits=2)

lambda.sd<- aggregate(lambda.correct~Region+Landscape+Treatment, dat.ML.new.short, sd) # lambdaF sds
lambda.sd$lambda.correct<- lambda.sd$lambda.correct*1000 # [mg dd-1]
lambda.sd$lambda.correct <- round(lambda.sd$lambda.correct, digits=2)

## check
mean(dat.ML.new.short$lambda.correct[dat.ML.new.short$Region=="TG" & dat.ML.new.short$Leaf=="Alder" & dat.ML.new.short$Landscape=="F"])*1000

mean(dat.ML.new.short$lambda.correct[dat.ML.new.short$Region=="TG" & dat.ML.new.short$Leaf=="Alder" & dat.ML.new.short$Landscape=="F"])/
  mean(dat.ML.new.short$lambda.correct[dat.ML.new.short$Region=="TG" & dat.ML.new.short$Leaf=="Alder" & dat.ML.new.short$Landscape=="A"])

mean(dat.ML.new.short$lambda.correct[dat.ML.new.short$Region=="TG" & dat.ML.new.short$Leaf=="Ash" & dat.ML.new.short$Landscape=="F"])/
  mean(dat.ML.new.short$lambda.correct[dat.ML.new.short$Region=="TG" & dat.ML.new.short$Leaf=="Ash" & dat.ML.new.short$Landscape=="A"])


mean(dat.ML.new.short$lambda.correct[dat.ML.new.short$Region=="TI" & dat.ML.new.short$Leaf=="Alder" & dat.ML.new.short$Landscape=="F"])/
  mean(dat.ML.new.short$lambda.correct[dat.ML.new.short$Region=="TI" & dat.ML.new.short$Leaf=="Alder" & dat.ML.new.short$Landscape=="A"])

mean(dat.ML.new.short$lambda.correct[dat.ML.new.short$Region=="TI" & dat.ML.new.short$Leaf=="Ash" & dat.ML.new.short$Landscape=="F"])/
  mean(dat.ML.new.short$lambda.correct[dat.ML.new.short$Region=="TI" & dat.ML.new.short$Leaf=="Ash" & dat.ML.new.short$Landscape=="A"])

mean(dat.ML.new.short$lambda.correct[ dat.ML.new.short$Landscape=="F"])/
  mean(dat.ML.new.short$lambda.correct[ dat.ML.new.short$Landscape=="A"])



