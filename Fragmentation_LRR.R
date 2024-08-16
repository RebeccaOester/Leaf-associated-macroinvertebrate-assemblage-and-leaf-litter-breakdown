##################################### log response ratio  LRR##########
##################################### Rebecca Oester ##################
######################################## 2023 #########################
## Leaf-associated macroinvertebrate assemblage and leaf litter breakdown in headwater streams depend on local riparian vegetation ##

rm(list=ls())
##### Load Packages ####
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(car)
library(nlme)
library(metafor)

##### Load Data ####
setwd("")
dattot <- read.delim("datTOT.txt")
dattot<- subset(dattot, !is.na(dattot$Stream))
dattot$Region<- as.factor(dattot$Region)
levels(dattot$Region)[levels(dattot$Region)=="TG"] <- "North"
levels(dattot$Region)[levels(dattot$Region)=="TI"] <- "South"

dattot <- subset(dattot, dattot$Leaf!= "Mix")


##### 1. Restructure Dataset ####
dat.short<- dplyr::select(dattot, Stream, TreatmentLandscape, lambda.correct) ## only look at columns of interest
dat.short$TreatmentLandscapeStream<- paste(dat.short$TreatmentLandscape, dat.short$Stream)

ggplot(dat.short)+geom_boxplot(aes(x=Stream, y=lambda.correct))+
  geom_point(aes(x=Stream, y=lambda.correct, color=TreatmentLandscape))
  

dat.short<- dat.short[!duplicated(dat.short$TreatmentLandscapeStream),] ## should be 32 (8 streams*4 treatment combinations)
dat.short$TreatmentLandscapeStream<-NULL ## delete this column

dat<- dcast(dat.short, Stream~TreatmentLandscape, id.var="lambda.correct" ) #### 8 rows, 5 columns

dat$LRR_Alder<- log(dat$CoarseAlderA/dat$CoarseAlderF)
dat$LRR_Ash<- log(dat$CoarseAshA/dat$CoarseAshF) ### NANs got created because one cannot take log from negative values!
dat<- dplyr::select(dat, Stream, LRR_Alder, LRR_Ash) ### select only columns of interest


dat.long<- melt(dat, id="Stream") ## from horizontal to vertical data structure
dat.long<- dplyr::rename(dat.long, Leaf=variable, LRR = value) ## rename columns
dat.long$LRR[is.nan(dat.long$LRR)]<-NA ## replace NANs with NAs

dat.long<- dat.long %>%
  mutate(Region= ifelse(str_detect(Stream, "TI"), "South", "North")) ### Add a column Region

dat.long<- dat.long %>% group_by(Stream)%>%
  mutate(MeanLRR= mean(LRR, na.rm=TRUE)) ### Add a column MeanLRR 

######################### 2. Stats #######################

dat.long$unweighted<-1

dat.new<- dat.long[!duplicated(dat.long$Stream),]# 8 streams

## random effects model, unweighted
Mod4<-rma(yi=MeanLRR,vi=unweighted,data=dat.new)
summary(Mod4) ### pval= 0.0016, estimate = -1.4175



##### 3. Graphs ####

Leafcol <- c("#d7191c", "#fdae61", "916fdb")
levels(dat.long$Leaf) <- c("Alder", "Ash", "Mix")

ggplot(dat.long)+theme_bw()+
  geom_boxplot(aes(x=reorder(Stream, MeanLRR), y=LRR))+
  geom_point(aes(x=Stream, y=LRR, color=Leaf))+
  scale_color_manual(values=Leafcol)+
  geom_hline(yintercept=0, colour="grey")+
  labs (x="Streams", y=expression(paste("LRR", lambda [F])))

ggplot(dat.long)+theme_bw()+
  geom_boxplot(aes(x=reorder(Stream, MeanLRR), y=LRR))+
  geom_point(aes(x=Stream, y=LRR, color=Leaf, shape=Region))+
  scale_color_manual(values=Leafcol)+
  geom_hline(yintercept=0, colour="grey")+
  labs (x="Streams", y=expression(paste("LRR ", lambda [F])))+
  theme(axis.text.x = element_blank(),
        legend.position = c(0.7, 0.2), legend.title = element_text(size=8), legend.text = element_text(size=8), legend.direction="horizontal")


LRR<- ggplot(dat.long)+theme_bw()+
  geom_boxplot(aes(x=reorder(Stream, MeanLRR), y=LRR))+
  #geom_point(aes(x=Stream, y=LRR, color=Leaf, shape=Region))+
  geom_hline(yintercept=0, colour="darkgrey")+
  scale_color_manual(values=Leafcol)+
  geom_hline(yintercept=-1.4175, color="red", linetype= "dashed")+
  labs (x="", y=expression(paste("Mean LRR ", lambda [F])))+
  theme()+
  theme(legend.position = c(0.2, 0.9), legend.title = element_text(size=8), legend.text = element_text(size=8), legend.direction="horizontal")

ggplot(dat.long)+theme_bw()+
  geom_boxplot(aes(y=LRR, x=Region, fill=Leaf))+
  labs (x="", 
        y = expression(paste("LRR ", lambda [F])))+
  #geom_point(aes(y=LRR, x=Region), position=position_dodge(width=0.75))+
  scale_fill_manual(values=Leafcol)+
  geom_hline(yintercept=-0.8187, color="black", linetype= "dashed")+
  geom_hline(yintercept=0, colour="grey")+
  theme(legend.position = c(0.35, 0.80), legend.title = element_text(size=14), legend.text = element_text(size=14), legend.direction="horizontal")+
  labs(fill="Leaf:", shape="Region:")+
  #geom_point(aes(y=LRR, x=Region, shape=Region, fill=Leaf), position = position_dodge(width = 0.75))+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )


#### A EPT abundance ####
EPT <- dattot %>%
  dplyr::group_by(Site) %>%
  dplyr::mutate(SumTotalEPT = sum(Total[Order=="Ephemeroptera" | Order=="Plecoptera" | Order=="Trichoptera"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)

EPT <- distinct(EPT, Site, .keep_all = TRUE) #1 row per site


dat.short<- dplyr::select(EPT, Stream, Region, Landscape, SumTotalEPT, Site) ## only look at columns of interest
dat.short$RegionLandscape<- paste(dat.short$Region, dat.short$Landscape)



dat<- dcast(dat.short, Stream~RegionLandscape, value.var="SumTotalEPT" ) #### 8 rows, 5 columns

dat$LRR_North<- log(dat$`North A` /dat$`North F`)
dat$LRR_South<- log(dat$`South A`/dat$`South F`) ### NANs got created because one cannot take log from negative values!
#dat<- dplyr::select(dat, Stream, LRR_North, LRR_South) ### select only columns of interest
dat$LRR <- rowSums(dat[,c("LRR_North", "LRR_South")], na.rm=TRUE)

#dat<- dplyr::select(dat, Stream, LRR) ### select only columns of interest

dat<- dat %>%
  mutate(Region= ifelse(str_detect(Stream, "TI"), "South", "North")) ### Add a column Region



dat$unweighted<-1

## random effects model, unweighted
Mod5<-rma(yi=LRR,vi=unweighted,data=dat, mods=~factor(Region))
summary(Mod5) ### pval= 0.0016, estimate = -1.4175

ggplot(dat)+theme_bw()+
  geom_boxplot(aes(y=LRR, x=Region))+
  labs (x="", 
        y = expression(paste("LRR")))+
  geom_point(aes(y=LRR, x=Region), position=position_dodge(width=0.75))+
  #scale_fill_manual(values=Leafcol)+
  #geom_hline(yintercept=-0.8187, color="black", linetype= "dashed")+
  geom_hline(yintercept=0, colour="grey")+
  theme(legend.position = c(0.35, 0.80), legend.title = element_text(size=14), legend.text = element_text(size=14), legend.direction="horizontal")+
  #labs(fill="Leaf:", shape="Region:")+
  #geom_point(aes(y=LRR, x=Region, shape=Region, fill=Leaf), position = position_dodge(width = 0.75))+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )







#### B Shredder abundance ####
shred <- dattot %>%
  dplyr::group_by(Site) %>%
  dplyr::mutate(SumTotalShred = sum(Total[ActiveShredder=="YES"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)

shred <- distinct(shred, Site, .keep_all = TRUE) #1 row per site


dat.short<- dplyr::select(shred, Stream, Region, Landscape, SumTotalShred, Site) ## only look at columns of interest
dat.short$RegionLandscape<- paste(dat.short$Region, dat.short$Landscape)



dat<- dcast(dat.short, Stream~RegionLandscape, value.var="SumTotalShred" ) #### 8 rows, 5 columns

dat$LRR_North<- log(dat$`North A` /dat$`North F`)
dat$LRR_South<- log(dat$`South A`/dat$`South F`) ### NANs got created because one cannot take log from negative values!
#dat<- dplyr::select(dat, Stream, LRR_North, LRR_South) ### select only columns of interest
dat$LRR <- rowSums(dat[,c("LRR_North", "LRR_South")], na.rm=TRUE)

#dat<- dplyr::select(dat, Stream, LRR) ### select only columns of interest

dat<- dat %>%
  mutate(Region= ifelse(str_detect(Stream, "TI"), "South", "North")) ### Add a column Region



dat$unweighted<-1

## random effects model, unweighted
Mod5<-rma(yi=LRR,vi=unweighted,data=dat, mods=~factor(Region))
summary(Mod5) ### pval= 0.0016, estimate = -1.4175

ggplot(dat)+theme_bw()+
  geom_boxplot(aes(y=LRR, x=Region))+
  labs (x="", 
        y = expression(paste("LRR")))+
  geom_point(aes(y=LRR, x=Region), position=position_dodge(width=0.75))+
  #scale_fill_manual(values=Leafcol)+
  #geom_hline(yintercept=-0.8187, color="black", linetype= "dashed")+
  geom_hline(yintercept=0, colour="grey")+
  theme(legend.position = c(0.35, 0.80), legend.title = element_text(size=14), legend.text = element_text(size=14), legend.direction="horizontal")+
  #labs(fill="Leaf:", shape="Region:")+
  #geom_point(aes(y=LRR, x=Region, shape=Region, fill=Leaf), position = position_dodge(width = 0.75))+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )


#### C EPT Diversity ####
EPT <- dattot %>%
  dplyr::group_by(Site) %>%
  dplyr::mutate(SumTotalEPT = sum(Total[Order=="Ephemeroptera" | Order=="Plecoptera" | Order=="Trichoptera"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)

dat.long<-dcast(EPT, Site~Taxon, value.var = "Total", sum) # transpose table based on Taxon and Site, summing up the Total per Taxon
row.names(dat.long) <- dat.long$Site # make row names the Sample ID
dat.long<- dat.long[,-1] # delete first column with Sample ID 


### delete columns that are EPT
s <-subset(dattot, Order=="Ephemeroptera" | Order=="Plecoptera" | Order=="Trichoptera")
s$Taxon <- as.factor(s$Taxon)
levels(s$Taxon)
l <- (levels(s$Taxon))

dat.long<- subset(dat.long, select=l)

#### shannon 
dive<- diversity(dat.long)
dive<- as.data.frame(dive)
dive["Site"] <- rownames(dive)
div2e<- merge(dive, EPT, by="Site")
div2e<- distinct(div2e, Site, .keep_all = TRUE) ## one row per bag



dat.short<- dplyr::select(div2e, Stream, Region, Landscape, dive, Site) ## only look at columns of interest
dat.short$RegionLandscape<- paste(dat.short$Region, dat.short$Landscape)



dat<- dcast(dat.short, Stream~RegionLandscape, value.var="dive" ) #### 8 rows, 5 columns

dat$LRR_North<- log(dat$`North A` /dat$`North F`)
dat$LRR_South<- log(dat$`South A`/dat$`South F`) ### NANs got created because one cannot take log from negative values!
#dat<- dplyr::select(dat, Stream, LRR_North, LRR_South) ### select only columns of interest
dat$LRR <- rowSums(dat[,c("LRR_North", "LRR_South")], na.rm=TRUE)

#dat<- dplyr::select(dat, Stream, LRR) ### select only columns of interest

dat<- dat %>%
  mutate(Region= ifelse(str_detect(Stream, "TI"), "South", "North")) ### Add a column Region



dat$unweighted<-1

## random effects model, unweighted
Mod5<-rma(yi=LRR,vi=unweighted,data=dat, mods=~factor(Region))
summary(Mod5) ### pval= 0.0016, estimate = -1.4175

ggplot(dat)+theme_bw()+
  geom_boxplot(aes(y=LRR, x=Region))+
  labs (x="", 
        y = expression(paste("LRR")))+
  geom_point(aes(y=LRR, x=Region), position=position_dodge(width=0.75))+
  #scale_fill_manual(values=Leafcol)+
  #geom_hline(yintercept=-0.8187, color="black", linetype= "dashed")+
  geom_hline(yintercept=0, colour="grey")+
  theme(legend.position = c(0.35, 0.80), legend.title = element_text(size=14), legend.text = element_text(size=14), legend.direction="horizontal")+
  #labs(fill="Leaf:", shape="Region:")+
  #geom_point(aes(y=LRR, x=Region, shape=Region, fill=Leaf), position = position_dodge(width = 0.75))+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )




#### D Shredder Diversity ####

shred <- dattot %>%
  dplyr::group_by(Site) %>%
  dplyr::mutate(SumTotalShred = sum(Total[ActiveShredder=="YES"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)

dat.long<-dcast(shred, Site~Taxon, value.var = "Total", sum) # transpose table based on Taxon and Site, summing up the Total per Taxon
row.names(dat.long) <- dat.long$Site # make row names the Sample ID
dat.long<- dat.long[,-1] # delete first column with Sample ID 


### delete columns that are not shredders
s <-subset(dattot, dattot$ActiveShredder=="YES")
s$Taxon <- as.factor(s$Taxon)
levels(s$Taxon)
l <- (levels(s$Taxon))

dat.long<- subset(dat.long, select=l)

#### shannon 
dive<- diversity(dat.long)
dive<- as.data.frame(dive)
dive["Site"] <- rownames(dive)
div2e<- merge(dive, shred, by="Site")
div2e<- distinct(div2e, Site, .keep_all = TRUE) ## one row per bag



dat.short<- dplyr::select(div2e, Stream, Region, Landscape, dive, Site) ## only look at columns of interest
dat.short$RegionLandscape<- paste(dat.short$Region, dat.short$Landscape)



dat<- dcast(dat.short, Stream~RegionLandscape, value.var="dive" ) #### 8 rows, 5 columns

dat$LRR_North<- log(dat$`North A` /dat$`North F`)
dat$LRR_South<- log(dat$`South A`/dat$`South F`) ### NANs got created because one cannot take log from negative values!
#dat<- dplyr::select(dat, Stream, LRR_North, LRR_South) ### select only columns of interest
dat$LRR <- rowSums(dat[,c("LRR_North", "LRR_South")], na.rm=TRUE)

#dat<- dplyr::select(dat, Stream, LRR) ### select only columns of interest

dat<- dat %>%
  mutate(Region= ifelse(str_detect(Stream, "TI"), "South", "North")) ### Add a column Region



dat$unweighted<-1

## random effects model, unweighted
Mod5<-rma(yi=LRR,vi=unweighted,data=dat, mods=~factor(Region))
summary(Mod5) ### pval= 0.0016, estimate = -1.4175

ggplot(dat)+theme_bw()+
  geom_boxplot(aes(y=LRR, x=Region))+
  labs (x="", 
        y = expression(paste("LRR")))+
  geom_point(aes(y=LRR, x=Region), position=position_dodge(width=0.75))+
  #scale_fill_manual(values=Leafcol)+
  #geom_hline(yintercept=-0.8187, color="black", linetype= "dashed")+
  geom_hline(yintercept=0, colour="grey")+
  theme(legend.position = c(0.35, 0.80), legend.title = element_text(size=14), legend.text = element_text(size=14), legend.direction="horizontal")+
  #labs(fill="Leaf:", shape="Region:")+
  #geom_point(aes(y=LRR, x=Region, shape=Region, fill=Leaf), position = position_dodge(width = 0.75))+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )





#### E EPT Biomass ####
EPT <- dattot %>%
  dplyr::group_by(Site) %>%
  dplyr::mutate(SumBEPT = sum(TotalDryMass[Order=="Ephemeroptera" | Order=="Plecoptera" | Order=="Trichoptera"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)

EPT <- distinct(EPT, Site, .keep_all = TRUE) #1 row per site


dat.short<- dplyr::select(EPT, Stream, Region, Landscape, SumBEPT, Site) ## only look at columns of interest
dat.short$RegionLandscape<- paste(dat.short$Region, dat.short$Landscape)



dat<- dcast(dat.short, Stream~RegionLandscape, value.var="SumBEPT" ) #### 8 rows, 5 columns

dat$LRR_North<- log(dat$`North A` /dat$`North F`)
dat$LRR_South<- log(dat$`South A`/dat$`South F`) ### NANs got created because one cannot take log from negative values!
#dat<- dplyr::select(dat, Stream, LRR_North, LRR_South) ### select only columns of interest
dat$LRR <- rowSums(dat[,c("LRR_North", "LRR_South")], na.rm=TRUE)

#dat<- dplyr::select(dat, Stream, LRR) ### select only columns of interest

dat<- dat %>%
  mutate(Region= ifelse(str_detect(Stream, "TI"), "South", "North")) ### Add a column Region



dat$unweighted<-1

## random effects model, unweighted
Mod5<-rma(yi=LRR,vi=unweighted,data=dat, mods=~factor(Region))
summary(Mod5) ### pval= 0.0016, estimate = -1.4175

ggplot(dat)+theme_bw()+
  geom_boxplot(aes(y=LRR, x=Region))+
  labs (x="", 
        y = expression(paste("LRR")))+
  geom_point(aes(y=LRR, x=Region), position=position_dodge(width=0.75))+
  #scale_fill_manual(values=Leafcol)+
  #geom_hline(yintercept=-0.8187, color="black", linetype= "dashed")+
  geom_hline(yintercept=0, colour="grey")+
  theme(legend.position = c(0.35, 0.80), legend.title = element_text(size=14), legend.text = element_text(size=14), legend.direction="horizontal")+
  #labs(fill="Leaf:", shape="Region:")+
  #geom_point(aes(y=LRR, x=Region, shape=Region, fill=Leaf), position = position_dodge(width = 0.75))+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )










#### F Shredder Biomass ####
shred <- dattot %>%
  dplyr::group_by(Site) %>%
  dplyr::mutate(SumBshred = sum(TotalDryMass[ActiveShredder=="YES"])) ## new column with the abundances per taxa summed up per bag(Sample_ID)

shred <- distinct(shred, Site, .keep_all = TRUE) #1 row per site


dat.short<- dplyr::select(shred, Stream, Region, Landscape, SumBshred, Site) ## only look at columns of interest
dat.short$RegionLandscape<- paste(dat.short$Region, dat.short$Landscape)



dat<- dcast(dat.short, Stream~RegionLandscape, value.var="SumBshred" ) #### 8 rows, 5 columns

dat$LRR_North<- log(dat$`North A` /dat$`North F`)
dat$LRR_South<- log(dat$`South A`/dat$`South F`) ### NANs got created because one cannot take log from negative values!
#dat<- dplyr::select(dat, Stream, LRR_North, LRR_South) ### select only columns of interest
dat$LRR <- rowSums(dat[,c("LRR_North", "LRR_South")], na.rm=TRUE)

#dat<- dplyr::select(dat, Stream, LRR) ### select only columns of interest

dat<- dat %>%
  mutate(Region= ifelse(str_detect(Stream, "TI"), "South", "North")) ### Add a column Region



dat$unweighted<-1

## random effects model, unweighted
Mod5<-rma(yi=LRR,vi=unweighted,data=dat, mods=~factor(Region))
summary(Mod5) ### pval= 0.0016, estimate = -1.4175

ggplot(dat)+theme_bw()+
  geom_boxplot(aes(y=LRR, x=Region))+
  labs (x="", 
        y = expression(paste("LRR")))+
  geom_point(aes(y=LRR, x=Region), position=position_dodge(width=0.75))+
  #scale_fill_manual(values=Leafcol)+
  #geom_hline(yintercept=-0.8187, color="black", linetype= "dashed")+
  geom_hline(yintercept=0, colour="grey")+
  theme(legend.position = c(0.35, 0.80), legend.title = element_text(size=14), legend.text = element_text(size=14), legend.direction="horizontal")+
  #labs(fill="Leaf:", shape="Region:")+
  #geom_point(aes(y=LRR, x=Region, shape=Region, fill=Leaf), position = position_dodge(width = 0.75))+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )

