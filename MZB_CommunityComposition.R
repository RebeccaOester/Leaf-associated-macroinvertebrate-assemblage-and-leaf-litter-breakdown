##################################### Ordinations #####################
##################################### Rebecca Oester ##################
######################################## 2023 #########################
## Leaf-associated macroinvertebrate assemblage and leaf litter breakdown in headwater streams depend on local riparian vegetation ##


rm(list=ls())

### load packages####
library(dplyr)
library(ggplot2)
library(ggpubr)
library(vegan)
library(reshape2)
library(stringr)
library(ggrepel)
library(nlme)
library(MuMIn)
library(patchwork)
library(ggvegan)



set.seed(3) ### so it's reproducible

################################# 1. Taxonomy ############################

#### load data
dat <- read.delim("datTOT.txt") #### macroinvertebrate taxa list
dat<- subset(dat, !is.na(dat$Stream))
dat$Region<- as.factor(dat$Region)
dat$Family<- as.factor(dat$Family)
levels(dat$Family)[levels(dat$Family)=="Cucurlionidae"] <- "Curculionidae"
levels(dat$Family)[levels(dat$Family)=="Chrysamelidae"] <- "Chrysomelidae"
levels(dat$Family) ## 53 different families
dat$SiteLeaf<- paste(dat$Site, dat$Leaf, sep="")

### define all Gammarus fossarum and pulex to Gammarus sp. to make the taxonomic resolution equal
dat$Taxon[dat$Taxon=="Gammarus fossarum"]<- "Gammarus sp."
dat$Taxon[dat$Taxon=="Gammarus pulex"]<- "Gammarus sp."


levels(dat$Region)[levels(dat$Region)=="TG"] <- "North"
levels(dat$Region)[levels(dat$Region)=="TI"] <- "South"
dat$Landscape<- as.factor(dat$Landscape)
levels(dat$Landscape)[levels(dat$Landscape)=="F"] <- "forested"
levels(dat$Landscape)[levels(dat$Landscape)=="A"] <- "non-forested"

dat$Landscape<- factor(dat$Landscape, levels= c("forested", "non-forested"))


dat <- subset(dat, dat$Leaf!= "Mix")

## create a new column with NewOrder classifications
dat<- dat %>%
  mutate(NewOrder=(ifelse(Order=="Bivalvia" | Order=="Coleoptera" | Order=="Gastropoda" | Order =="Hirudinea" | Order=="Hydracarina" | Order=="Nematoda" | Order=="Odonata" | Order=="Oligochaeta" | Order=="Turbellaria", "Other",
                          ifelse(Order=="Crustacea", "Crustacea",
                                 ifelse(Family=="Chironomidae", "Chironomidae", 
                                        ifelse(Family=="Simuliidae", "Simuliidae",
                                               ifelse(Order=="Diptera", "Diptera", 
                                                      ifelse(Order=="Ephemeroptera", "Ephemeroptera", 
                                                             ifelse(Order=="Plecoptera", "Plecoptera", "Trichoptera")))))))))



#### A) relative barplots ####
dat$NewOrder <- factor(dat$NewOrder, levels = c("Ephemeroptera", "Plecoptera", "Trichoptera", "Diptera","Chironomidae", "Simuliidae", "Crustacea", "Other"))
levels(dat$NewOrder)
ordercol<- c("#a6bddb", "#5ab4ac", "#addd8e", "#f6e8c3", "#d8b365", "#8c510a", "#de77ae", "#762a83")

t<-ggplot(dat)+theme_bw()+
  labs (x="", y="Taxonomic composition", fill="Taxonomic group")+
  geom_bar(stat="identity", position="fill", aes(x=Landscape, y=Total, fill=NewOrder))+
  theme(text = element_text(size=34), axis.text.x = element_text(size=34))+
  scale_fill_manual(values=ordercol)+
  guides(fill=guide_legend(title="All"))
  

legt <- get_legend(t)
leg.ggt<- as_ggplot(legt)


tg<-ggplot(subset(dat, dat$Region=="North"))+theme_bw()+
  labs (x="", y="", fill="Taxonomic group")+
  geom_bar(stat="identity", position="fill", aes(x=Landscape, y=Total, fill=NewOrder))+
  scale_fill_manual(values=ordercol)+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


tg<-ggplot(subset(dat, dat$Region=="North"))+theme_bw()+
  labs (x="", y="", fill="Taxonomic group")+
  labs (x="", y="Total number of individuals", fill="Taxonomic group")+
  geom_bar(stat="identity",  aes(x=Landscape, y=Total, fill=NewOrder))+
  scale_fill_manual(values=ordercol)+
  theme(text = element_text(size=24), axis.text.x = element_text(size=24))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  ggtitle("North")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(dat, dat$Region=="North"))+theme_bw()+
  labs (x="", y="", fill="Taxonomic group")+
  geom_bar(stat="identity", position="fill", aes(x=Site_ID, y=Total, fill=NewOrder))+
  scale_fill_manual(values=ordercol)+
  theme(text = element_text(size=18), axis.text.x = element_text(size=18))+
  theme(legend.position="none")

ggplot(subset(dat, dat$Region=="North"))+theme_bw()+
  labs (x="", y="", fill="Taxonomic group")+
  geom_bar(stat="identity", aes(x=Site_ID, y=Total, fill=NewOrder))+
  scale_fill_manual(values=ordercol)+
  theme(text = element_text(size=18), axis.text.x = element_text(size=18))+
  theme(legend.position="none")


ordercol<- c("#a6bddb", "#5ab4ac", "#addd8e",  "#f6e8c3", "#d8b365", "#8c510a", "#762a83")

ti<-ggplot(subset(dat, dat$Region=="South"))+theme_bw()+
  labs (x="", y="Total number of individuals", fill="Taxonomic group")+
  geom_bar(stat="identity", position="fill", aes(x=Landscape, y=Total, fill=NewOrder))+
  scale_fill_manual(values=ordercol)+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


ti<-ggplot(subset(dat, dat$Region=="South"))+theme_bw()+
  labs (x="", y="")+
  geom_bar(stat="identity", aes(x=Landscape, y=Total, fill=NewOrder))+
  scale_fill_manual(values=ordercol)+
  theme(text = element_text(size=24), axis.text.x = element_text(size=24))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  ggtitle("South")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(dat, dat$Region=="South"))+theme_bw()+
  labs (x="", y="Taxonomic composition", fill="Taxonomic group")+
  geom_bar(stat="identity", position="fill", aes(x=Site_ID, y=Total, fill=NewOrder))+
  scale_fill_manual(values=ordercol)+
  theme(text = element_text(size=18), axis.text.x = element_text(size=18))+
  theme(legend.position="none")

ggplot(subset(dat, dat$Region=="South"))+theme_bw()+
  labs (x="", y="Taxonomic composition", fill="Taxonomic group")+
  geom_bar(stat="identity",  aes(x=Site_ID, y=Total, fill=NewOrder))+
  scale_fill_manual(values=ordercol)+
  theme(text = element_text(size=18), axis.text.x = element_text(size=18))+
  theme(legend.position="none")


dat <- dat %>% group_by(Site_ID) %>%
  mutate(Sum_Total   = sum(Total),
         Sum_EPT = sum(Total[Order=="Ephemeroptera" | Order=="Plecoptera" | Order == "Trichoptera"]),
         EPT_Total_Percent = Sum_EPT/Sum_Total*100) ## 16 different values for the 16 different sampling sites

dat <- dat %>% group_by(Landscape, Region) %>%
  mutate(Mean_EPT_Percent = mean(EPT_Total_Percent),
         sd_EPT_Percent = sd(EPT_Total_Percent),
         Mean_EPT_abs = mean(Sum_EPT),
         sd_EPT_abs = sd(Sum_EPT),
         n_16= 2*4*4) %>% ## 2 leaves, 4 replicates within a stream, 4 replicate sites within region of the same vegetation
  mutate(se_EPT_Percent = sd_EPT_Percent/sqrt(n_16),
         lower.ci.EPT_Percent = Mean_EPT_Percent - qt(1 - (0.05 / 2), n_16 - 1) * se_EPT_Percent,
         upper.ci.EPT_Percent = Mean_EPT_Percent + qt(1 - (0.05 / 2), n_16 - 1) * se_EPT_Percent,
         
         se_EPT_abs = sd_EPT_abs/sqrt(n_16),
         lower.ci.EPT_abs = Mean_EPT_abs - qt(1 - (0.05 / 2), n_16 - 1) * se_EPT_abs,
         upper.ci.EPT_abs = Mean_EPT_abs + qt(1 - (0.05 / 2), n_16 - 1) * se_EPT_abs)
              

dat.Mean_Percent<- dat[!duplicated(dat$Mean_EPT_Percent),] ## 4 values for each Region, Landscape combination ==> report in MS
dat.Mean_Absolut<- dat[!duplicated(dat$Mean_EPT_abs),] ## 4 values for each Region, Landscape combination ==> report in MS



#### make species matrix with dcast 
dat.long<-dcast(dat, Site~Taxon, value.var = "Total", sum) # transpose table based on Taxon and SiteLeaf, summing up the Total per Taxon ==> 16*2==>32

row.names(dat.long) <- dat.long$Sample_ID # make row names the Sample ID
dat.long<- dat.long[,-1] # delete first column with Sample ID 

#### transform the species community data using hellinger transformation
## hellinger is useful when few abundant species dominate community
dat.long.hel<- decostand(dat.long, method="hellinger") ### decostand standardises abundances to balance distributions

#### B) NMDS ####
nmds1<- metaMDS(dat.long.hel, autotransform=F) ### stress indicates how much data needs to be distortet

#### use basic plottic function
ordiplot(nmds1, type="t") # points are sampling sites and crosses are taxa
autoplot(nmds1)

## full control with fortified ordination output
#fort<- fortify(nmds1.1)
fort<- fortify(nmds1)
ggplot()+
  geom_point(data=subset(fort, Score=="sites"),
             mapping = aes (x=NMDS1, y=NMDS2),
             color="black",
             alpha=0.5)+
  geom_segment(data=subset(fort, Score == "species"),
               mapping= aes(x=0, y=0, xend= NMDS1, yend=NMDS2), 
               arrow= arrow (length=unit (0.015, "npc"),
                             type="closed"),
               color="darkgray",
               size=0.8)+
  geom_text(data= subset(fort, Score="species"),
            mapping = aes(label = Label, x = NMDS1*1.1, y= NMDS2*1.1))+
  geom_abline (intercept = 0, slope = 0, linetype="dashed", size=0.8, color="grey")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, color="grey")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color= "black"))

#### C) PERMANOVA ####
dat1<- dat%>% dplyr::group_by(Site) %>%
  dplyr::mutate(Sum_Total = sum(Total, na.rm=TRUE)) #### calculate the sum 
dat1<- dat1[!duplicated(dat1$Site),]

data_1<- dplyr::select(dat1, Stream, Region, Landscape, Leaf) ## make a dataframe for groups
data_1$Landscape<- as.factor(data_1$Landscape)


set.seed(999) ### so it's reproducible
adonis(dat.long.hel ~Landscape*Region, data=data_1, strata= data_1$Stream)



#### Check variances ####
dat.longR<-dcast(dat, Site~Taxon, value.var = "Total", sum) # transpose table based on Taxon and Sample Id, summing up the Total per Taxon

row.names(dat.longR) <- dat.longR$Sample_ID # make row names the Sample ID
dat.longR<- dat.longR[,-1] # delete first column with Sample ID 

#### transform the species community data using hellinger transformation
## hellinger is useful when few abundant species dominate community
dat.long.helR<- decostand(dat.longR, method="hellinger") ### decostand standardises abundances to balance distributions

disR <- vegdist(dat.long.helR)


## 1. Region
groups <- factor(c("N", "N", 
                   "N", "N",
                   "N", "N",
                   "N", "N",
                   "S", "S",
                   "S", "S",
                   "S", "S",
                   "S", "S"))
mod <- betadisper(disR, groups)
mod
anova(mod)

## 2. Landscape
groups <- factor(c("non-forested", "forested", 
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested"))



mod <- betadisper(disR, groups)
mod
anova(mod)



#### D) NMDS Plot ####

#### make species matrix with dcast 
dat.long<-dcast(dat, SiteLeaf~NewOrder, value.var = "Total", sum) # transpose table based on Taxon and Sample Id, summing up the Total per Taxon
row.names(dat.long) <- dat.long$Sample_ID # make row names the Sample ID
dat.long<- dat.long[,-1] # delete first column with Sample ID 

#### transform the species community data using hellinger transformation
## hellinger is useful when few abundant species dominate community
dat.long.hel<- decostand(dat.long, method="hellinger") ### decostand standardises abundances to balance distributions

nmds1<- metaMDS(dat.long.hel, autotransform=F) ### stress indicates how much data needs to be distortet
fort<- fortify(nmds1)


## create new dataframe NMDS
NMDS=data.frame(x=nmds1$point[,1],y=nmds1$point[,2],Landscape=as.factor(data_1$Landscape), Region = as.factor(data_1$Region))


ord<-ordiellipse(nmds1, data_1$Landscape ,display = "sites", kind ="sd", conf = 0.95, label = T)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#Generate ellipse points
df_ell <- data.frame()
for(g in levels(NMDS$Landscape)){
  if(g!="" && (g %in% names(ord))){
    
    df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$Landscape==g,],
                                                     veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,ord[[g]]$scale)))
                                  ,Landscape=g))
  }
}

head(df_ell)
df_ell$Landscape<-as.factor(df_ell$Landscape)
df_ell$Landscape <- factor(df_ell$Landscape, levels = c("forested", "non-forested"))

NMDS.mean=aggregate(NMDS[,1:2],list(group=NMDS$Landscape),mean)
Landscol<- c("#d8b365","#1b9e77")
Landscol<- c("#1b9e77","#d8b365")


fort <- fort %>%
  mutate(label = ifelse(fort$Score=="species", Label, NA))


tax<-ggplot()+
  geom_point(data=NMDS,aes(x,y, color=Landscape,shape=Region), size=5)+theme_bw()+
  scale_color_manual(values=Landscol)+
  annotate("text",x=-0.9,y=0.5,label="stress = 0.19" ,size=8)+ ### this indicates where the centre would be of each group
  geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2, linetype=Landscape), size=1)+
  geom_segment(data=subset(fort, Score == "species"),
               mapping= aes(x=0, y=0, xend= NMDS1, yend=NMDS2), 
               arrow= arrow (length=unit (0.015, "npc"),
                             type="closed"),
               color="darkgray",
               size=0.8)+
  geom_text_repel(data= subset(fort, Score="species"),
                  mapping = aes(label = label, x = NMDS1*1.2, y= NMDS2*1.2), max.overlaps = 12, size=10)+
  xlab("NMDS1")+ ylab("NMDS2")+ theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22))+
  theme(legend.position = c(0.35, 0.2), legend.title = element_text(size=24), legend.text = element_text(size=24), legend.direction="horizontal")+
  labs(col="Riparian vegetation:", shape="Region:" ,linetype="Riparian vegetation:")

ggplot()+
  geom_point(data=NMDS,aes(x,y, color=Landscape,shape=Region), size=5)+theme_bw()+
  scale_color_manual(values=Landscol)+
  annotate("text",x=-0.9,y=0.5,label="stress = 0.19" ,size=8)+ ### this indicates where the centre would be of each group
  geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2, linetype=Landscape), size=1)+
  geom_segment(data=subset(fort, Score == "species"),
               mapping= aes(x=0, y=0, xend= NMDS1, yend=NMDS2), 
               arrow= arrow (length=unit (0.015, "npc"),
                             type="closed"),
               color="darkgray",
               size=0.8)+
  geom_text_repel(data= subset(fort, Score="species"),
                  mapping = aes(label = label, x = NMDS1*1.2, y= NMDS2*1.2), max.overlaps = 12, size=10)+
  xlab("NMDS1")+ ylab("NMDS2")+ theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22))+
  #theme(legend.position = c(0.35, 0.2), legend.title = element_text(size=24), legend.text = element_text(size=24), legend.direction="horizontal")+
  labs(col="Riparian vegetation:", shape="Region:" ,linetype="Riparian vegetation:")



################ 2. Feeding Types CWMT ################
#### load data 
dat <- read.delim("datTOT.txt") #### macroinvertebrate taxa list
dat<- subset(dat, !is.na(dat$Stream))
dat$Region<- as.factor(dat$Region)
levels(dat$Region)[levels(dat$Region)=="TG"] <- "North"
levels(dat$Region)[levels(dat$Region)=="TI"] <- "South"
dat$Landscape<- as.factor(dat$Landscape)
levels(dat$Landscape)[levels(dat$Landscape)=="A"] <- "non-forested"
levels(dat$Landscape)[levels(dat$Landscape)=="F"] <- "forested"
dat$Landscape<- factor(dat$Landscape, levels= c("forested", "non-forested"))

dat$Family<- as.factor(dat$Family)
levels(dat$Family)[levels(dat$Family)=="Cucurlionidae"] <- "Curculionidae"
levels(dat$Family)[levels(dat$Family)=="Chrysamelidae"] <- "Chrysomelidae"
levels(dat$Family)

dat <- subset(dat, dat$Leaf!= "Mix")


dat$SiteLeaf <- paste(dat$Site,dat$Leaf)


##### multiply total number of taxa with the fuzzy coded feeding type
dat$grazer2<- dat$Total*dat$grazer
dat$miner2<- dat$Total*dat$miner
dat$xylobiont2<- dat$Total*dat$xylobiont
dat$shredder2<- dat$Total*dat$shredder
dat$gatherer2<- dat$Total*dat$gatherer
dat$activefilterfeeder2<- dat$Total*dat$activefilterfeeder
dat$passivefilterfeeder2<- dat$Total*dat$passivefilterfeeder
dat$predator2<- dat$Total*dat$predator
dat$parasite2<- dat$Total*dat$parasite
dat$other2<- dat$Total*dat$other


#dat.cwmt<- dplyr::select(dat ,Site, grazer2, miner2, xylobiont2, shredder2, gatherer2, activefilterfeeder2, passivefilterfeeder2, predator2, parasite2, other2)
dat.cwmt<- dplyr::select(dat, Sample_ID, grazer2, miner2, xylobiont2, shredder2, gatherer2, activefilterfeeder2, passivefilterfeeder2, predator2, parasite2, other2)

dat.cwmt<- subset(dat.cwmt, !is.na(dat.cwmt$grazer2)) ### delete some empty rows where I couldn't find feeding type info


dat.cwmt<-dplyr::rename(dat.cwmt, Grazer = grazer2,
                        Miner = miner2,
                        Xylobiont = xylobiont2,
                        Shredder = shredder2,
                        Gatherer = gatherer2,
                        ActiveFilterFeeder = activefilterfeeder2,
                        PassiveFilterFeeder = passivefilterfeeder2,
                        Predator = predator2,
                        Parasite = parasite2,
                        Other = other2)

#### A) relative barplots ####
#dat.cwmt.long<- melt(dat.cwmt, id= c("Site") )
dat.cwmt.long<- melt(dat.cwmt, id= c("Sample_ID") )
dat.cwmt.long<- dat.cwmt.long%>%
  mutate(Landscape= ifelse(str_detect(Site, "A"), "non-forested", "forested"))
dat.cwmt.long<- dat.cwmt.long%>%
  mutate(Landscape= ifelse(str_detect(Sample_ID, "A"), "non-forested", "forested"))

# dat.cwmt.long<- dat.cwmt.long%>%
#   mutate(Landscape= ifelse(str_detect(Sample_ID, "A"), "non-forested", "forested"))


dat.cwmt.long$Landscape<- as.factor(dat.cwmt.long$Landscape)
dat.cwmt.long$Landscape <- factor(dat.cwmt.long$Landscape, levels = c("forested", "non-forested"))
dat.cwmt.long<- dat.cwmt.long%>%
  mutate(Region= ifelse(str_detect(Site, "TG"), "North", "South"))
dat.cwmt.long<- dat.cwmt.long%>%
  mutate(Region= ifelse(str_detect(Sample_ID, "TG"), "North", "South"))


# dat.cwmt.long<- dat.cwmt.long%>%
#   mutate(Region= ifelse(str_detect(Sample_ID, "TG"), "North", "South"))


fedcol<- c("#d9f0a3", "#78c679", "#8c510a", "#bf812d", "#7fcdbb", "#41b6c4","#1d91c0", "#225ea8", "#253494", "#081d58")

f<-ggplot(dat.cwmt.long, aes(x=Landscape, y=value, fill=variable))+theme_bw()+
  labs (x="", y="CWMT", fill="Functional Feeding Groups")+
  geom_bar(stat="identity", position = "fill")+
  theme(text = element_text(size=30), axis.text.x = element_text(size=30))+
  scale_fill_manual(values=fedcol, labels=c("Grazer/Scraper", "Miner", "Xylobiont", "Shredder", "Gatherer/Collector", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  #scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.title=element_blank())

legft <- get_legend(f)
leg.gft<- as_ggplot(legft)


ftg<-ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="North"), aes(x=Landscape, y=value, fill=variable))+theme_bw()+
  labs (x="", y="", fill="Functional Feeding Groups")+
  geom_bar(stat="identity", position = "fill")+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  #scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


ftg<-ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="North"), aes(x=Landscape, y=value, fill=variable))+theme_bw()+
  labs (x="", y="ACWMT", fill="Functional Feeding Groups")+
  geom_bar(stat="identity")+
  theme(text = element_text(size=24), axis.text.x = element_text(size=24))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  #scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  ggtitle("North")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="North"), aes(x=Sample_ID, y=value, fill=variable))+theme_bw()+
  labs (x="", y="", fill="Functional Feeding Groups")+
  geom_bar(stat="identity", position = "fill")+
  theme(text = element_text(size=18), axis.text.x = element_text(size=18))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")

ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="North"), aes(x=Site, y=value, fill=variable))+theme_bw()+
  labs (x="", y="", fill="Functional Feeding Groups")+
  geom_bar(stat="identity",)+
  theme(text = element_text(size=18), axis.text.x = element_text(size=18))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")


fti<-ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="South"), aes(x=Landscape, y=value, fill=variable))+theme_bw()+
  labs (x="", y="CWMT", fill="Functional Feeding Groups")+
  geom_bar(stat="identity", position = "fill")+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  #scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


fti<-ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="South"), aes(x=Landscape, y=value, fill=variable))+theme_bw()+
  labs (x="", y="", fill="Functional Feeding Groups")+
  geom_bar(stat="identity")+
  theme(text = element_text(size=24), axis.text.x = element_text(size=24))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  #scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  ggtitle("South")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="South"), aes(x=Sample_ID, y=value, fill=variable))+theme_bw()+
  labs (x="", y="CWMT", fill="Functional Feeding Groups")+
  geom_bar(stat="identity", position = "fill")+
  theme(text = element_text(size=18), axis.text.x = element_text(size=18))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")


ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="South"), aes(x=Site, y=value, fill=variable))+theme_bw()+
  labs (x="", y="CWMT", fill="Functional Feeding Groups")+
  geom_bar(stat="identity")+
  theme(text = element_text(size=18), axis.text.x = element_text(size=18))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")


#### for every sample_ID, calculate the acwmt percentage of shredder feeding type to the other feeding types

dat.cwmt.long1 <- dat.cwmt.long %>% group_by(Sample_ID) %>%
  mutate(Sum_value_FT   = sum(value),
         Sum_shred = sum(value[variable=="Shredder"]),
         Shred_FT_Percent = Sum_shred/Sum_value_FT*100) ## 16 different values for the 16 different sampling sites

#### now reduce dataset so we only have one row per site
dat.cwmt.long2<- dat.cwmt.long1[!duplicated(dat.cwmt.long1$Sample_ID),] ## 16 Sites, 127 Bags

hist(dat.cwmt.long2$Shred_FT_Percent)

dat.cwmt.long3 <- dat.cwmt.long2 %>% group_by(Landscape, Region) %>%
  mutate(Mean_shred_Percent = mean(Shred_FT_Percent),
         sd_shred_Percent = sd(Shred_FT_Percent),
         Mean_shred_abs = mean(Sum_shred),
         sd_shred_abs = sd(Sum_shred))

min(dat.cwmt.long3$Shred_FT_Percent)
max(dat.cwmt.long3$Shred_FT_Percent)

min(dat.cwmt.long3$Sum_shred)
max(dat.cwmt.long3$Sum_shred)


#### sum up values per site 

dat.cwmt<- dplyr::select(dat ,Site, grazer2, miner2, xylobiont2, shredder2, gatherer2, activefilterfeeder2, passivefilterfeeder2, predator2, parasite2, other2)
dat.cwmt<- subset(dat.cwmt, !is.na(dat.cwmt$grazer2)) ### delete some empty rows where I couldn't find feeding type info
dat.cwmt<-dplyr::rename(dat.cwmt, Grazer = grazer2,
                        Miner = miner2,
                        Xylobiont = xylobiont2,
                        Shredder = shredder2,
                        Gatherer = gatherer2,
                        ActiveFilterFeeder = activefilterfeeder2,
                        PassiveFilterFeeder = passivefilterfeeder2,
                        Predator = predator2,
                        Parasite = parasite2,
                        Other = other2)


dat.c<- dat.cwmt%>% dplyr::group_by(Site) %>%
  dplyr::summarise(across(everything(), ~ sum(., is.na(.), 0))) ### column sum per site of each feeding type so now we only have 16 columns

row.names(dat.c) <- dat.c$Site # make row names the Site
dat.c<- dat.c[,-1] # delete first column with Site

#### B) NMDS #####
dat.c.hel<- decostand(dat.c, method="hellinger") ### decostand standardises abundances to balance distributions

#### do the actual NMDS
nmds2<- metaMDS(dat.c.hel, autotransform=F) ### stress 0.07

fort2<- fortify(nmds2)
ggplot()+
  geom_point(data=subset(fort2, Score=="sites"),
             mapping = aes (x=NMDS1, y=NMDS2),
             color="black",
             alpha=0.5)+
  geom_segment(data=subset(fort2, Score == "species"),
               mapping= aes(x=0, y=0, xend= NMDS1, yend=NMDS2), 
               arrow= arrow (length=unit (0.015, "npc"),
                             type="closed"),
               color="darkgray",
               size=0.8)+
  geom_text(data= subset(fort2, Score="species"),
            mapping = aes(label = Label, x = NMDS1*1.1, y= NMDS2*1.1))+
  geom_abline (intercept = 0, slope = 0, linetype="dashed", size=0.8, color="grey")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, color="grey")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color= "black"))


#### C) PERMANOVA ####
set.seed(3)
adonis(dat.c.hel ~Landscape*Region, data=data_1, strata= data_1$Stream)

disR <- vegdist(dat.c.hel)

groups <- factor(c("N", "N", 
                   "N", "N",
                   "N", "N",
                   "N", "N",
                   "S", "S",
                   "S", "S",
                   "S", "S",
                   "S", "S"))
mod <- betadisper(disR, groups)
mod
anova(mod)



groups <- factor(c("non-forested", "forested", 
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested"))

mod <- betadisper(disR, groups)
mod
anova(mod)



#### D) NMDS Plot ####
#### show results in plot version 1 with ellipses
## create new dataframe NMDS
NMDS2=data.frame(x=nmds2$point[,1],y=nmds2$point[,2],Landscape=as.factor(data_1$Landscape), Region = as.factor(data_1$Region))

ord2<-ordiellipse(nmds2, as.factor(data_1$Landscape) ,display = "sites", kind ="sd", conf = 0.95, label = T)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#Generate ellipse points
df_ell2 <- data.frame()
for(g in levels(NMDS2$Landscape)){
  if(g!="" && (g %in% names(ord2))){
    
    df_ell2 <- rbind(df_ell2, cbind(as.data.frame(with(NMDS2[NMDS2$Landscape==g,],
                                                       veganCovEllipse(ord2[[g]]$cov,ord2[[g]]$center,ord2[[g]]$scale)))
                                    ,Landscape=g))
  }
}

head(df_ell2)
df_ell2$Landscape<-as.factor(df_ell2$Landscape)
df_ell2$Landscape <- factor(df_ell2$Landscape, levels = c("forested", "non-forested"))

NMDS.mean2=aggregate(NMDS2[,1:2],list(group=NMDS2$Landscape),mean)

fort2 <- fort2 %>%
  mutate(label = ifelse(fort2$Score=="species", Label, NA))


ft<-ggplot()+
  geom_point(data=NMDS2,aes(x,y, color=Landscape,, shape=Region), size=5)+theme_bw()+
  scale_color_manual(values=Landscol)+
  annotate("text",x=-0.35,y=0.35,,label="stress = 0.06",size=8)+ ### this indicates where the centre would be of each group
  geom_path(data=df_ell2, aes(x=NMDS1, y=NMDS2, linetype=Landscape), size=1)+
  geom_segment(data=subset(fort2, Score == "species"),
               mapping= aes(x=0, y=0, xend= NMDS1, yend=NMDS2), 
               arrow= arrow (length=unit (0.015, "npc"),
                             type="closed"),
               color="darkgray",
               size=0.8)+
  geom_text_repel(data= subset(fort2, Score="species"),
                  mapping = aes(label = label, x = NMDS1*1.2, y= NMDS2*1.2), max.overlaps = 12, size=10)+
  xlab("NMDS1")+ ylab("NMDS2")+theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  theme(legend.position = c(0.7, 0.85), legend.title = element_text(size=20), legend.text = element_text(size=20), legend.direction="horizontal")+
  labs(col="Riparian vegetation:", shape="Region:" ,linetype="Riparian vegetation:")

ggplot()+
  geom_point(data=NMDS2,aes(x,y, color=Landscape,, shape=Region), size=5)+theme_bw()+
  scale_color_manual(values=Landscol)+
  annotate("text",x=-0.35,y=0.35,,label="stress = 0.06",size=8)+ ### this indicates where the centre would be of each group
  geom_path(data=df_ell2, aes(x=NMDS1, y=NMDS2, linetype=Landscape), size=1)+
  geom_segment(data=subset(fort2, Score == "species"),
               mapping= aes(x=0, y=0, xend= NMDS1, yend=NMDS2), 
               arrow= arrow (length=unit (0.015, "npc"),
                             type="closed"),
               color="darkgray",
               size=0.8)+
  geom_text_repel(data= subset(fort2, Score="species"),
                  mapping = aes(label = label, x = NMDS1*1.2, y= NMDS2*1.2), max.overlaps = 12, size=10)+
  xlab("NMDS1")+ ylab("NMDS2")+theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  #theme(legend.position = c(0.7, 0.85), legend.title = element_text(size=20), legend.text = element_text(size=20), legend.direction="horizontal")+
  labs(col="Riparian vegetation:", shape="Region:" ,linetype="Riparian vegetation:")



################ 2. Feeding Types BWMT ################
#### load data 
dat <- read.delim("datTOT.txt") #### macroinvertebrate taxa list
dat<- subset(dat, !is.na(dat$Stream))
dat$Region<- as.factor(dat$Region)
levels(dat$Region)[levels(dat$Region)=="TG"] <- "North"
levels(dat$Region)[levels(dat$Region)=="TI"] <- "South"
dat$Landscape<- as.factor(dat$Landscape)
levels(dat$Landscape)[levels(dat$Landscape)=="A"] <- "non-forested"
levels(dat$Landscape)[levels(dat$Landscape)=="F"] <- "forested"
dat$Landscape<- factor(dat$Landscape, levels= c("forested", "non-forested"))

dat$Family<- as.factor(dat$Family)
levels(dat$Family)[levels(dat$Family)=="Cucurlionidae"] <- "Curculionidae"
levels(dat$Family)[levels(dat$Family)=="Chrysamelidae"] <- "Chrysomelidae"
levels(dat$Family)

dat$SiteLeaf <- paste(dat$Site,dat$Leaf)

dat <- subset(dat, dat$Leaf!= "Mix")

##### multiply TotalDryMass number of taxa with the fuzzy coded feeding type
dat$grazer2<- dat$TotalDryMass*dat$grazer
dat$miner2<- dat$TotalDryMass*dat$miner
dat$xylobiont2<- dat$TotalDryMass*dat$xylobiont
dat$shredder2<- dat$TotalDryMass*dat$shredder
dat$gatherer2<- dat$TotalDryMass*dat$gatherer
dat$activefilterfeeder2<- dat$TotalDryMass*dat$activefilterfeeder
dat$passivefilterfeeder2<- dat$TotalDryMass*dat$passivefilterfeeder
dat$predator2<- dat$TotalDryMass*dat$predator
dat$parasite2<- dat$TotalDryMass*dat$parasite
dat$other2<- dat$TotalDryMass*dat$other


#dat.cwmt<- dplyr::select(dat, Site, grazer2, miner2, xylobiont2, shredder2, gatherer2, activefilterfeeder2, passivefilterfeeder2, predator2, parasite2, other2)
dat.cwmt<- dplyr::select(dat, Sample_ID, grazer2, miner2, xylobiont2, shredder2, gatherer2, activefilterfeeder2, passivefilterfeeder2, predator2, parasite2, other2)


dat.cwmt<- subset(dat.cwmt, !is.na(dat.cwmt$grazer2)) ### delete some empty rows where I couldn't find feeding type info


dat.cwmt<-dplyr::rename(dat.cwmt, Grazer = grazer2,
                        Miner = miner2,
                        Xylobiont = xylobiont2,
                        Shredder = shredder2,
                        Gatherer = gatherer2,
                        ActiveFilterFeeder = activefilterfeeder2,
                        PassiveFilterFeeder = passivefilterfeeder2,
                        Predator = predator2,
                        Parasite = parasite2,
                        Other = other2)

#### A) relative barplots ####
#dat.cwmt.long<- melt(dat.cwmt, id= c("Site") )
dat.cwmt.long<- melt(dat.cwmt, id= c("Sample_ID") )


# dat.cwmt.long<- dat.cwmt.long%>%
#   mutate(Landscape= ifelse(str_detect(Site, "A"), "non-forested", "forested"))
dat.cwmt.long<- dat.cwmt.long%>%
  mutate(Landscape= ifelse(str_detect(Sample_ID, "A"), "non-forested", "forested"))


dat.cwmt.long$Landscape<- as.factor(dat.cwmt.long$Landscape)
dat.cwmt.long$Landscape <- factor(dat.cwmt.long$Landscape, levels = c("non-forested", "forested"))
dat.cwmt.long$Landscape<- factor(dat.cwmt.long$Landscape, levels= c("forested", "non-forested"))

# dat.cwmt.long<- dat.cwmt.long%>%
#   mutate(Region= ifelse(str_detect(Site, "TG"), "North", "South"))

dat.cwmt.long<- dat.cwmt.long%>%
  mutate(Region= ifelse(str_detect(Sample_ID, "TG"), "North", "South"))


fedcol<- c("#d9f0a3", "#78c679", "#8c510a", "#bf812d", "#7fcdbb", "#41b6c4","#1d91c0", "#225ea8", "#253494", "#081d58")

f<-ggplot(dat.cwmt.long, aes(x=Landscape, y=value, fill=variable))+theme_bw()+
  labs (x="", y="CWMT", fill="Functional Feeding Groups")+
  geom_bar(stat="identity", position = "fill")+
  theme(text = element_text(size=34), axis.text.x = element_text(size=34))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  #scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.title=element_blank())

legft <- get_legend(f)
leg.gft<- as_ggplot(legft)

bftg<-ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="North"), aes(x=Landscape, y=value, fill=variable))+theme_bw()+
  labs (x="", y="", fill="Functional Feeding Groups")+
  geom_bar(stat="identity", position = "fill")+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  #scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


bftg<-ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="North"), aes(x=Landscape, y=value, fill=variable))+theme_bw()+
  labs (x="", y="BCWMT", fill="Functional Feeding Groups")+
  geom_bar(stat="identity")+
  theme(text = element_text(size=24), axis.text.x = element_text(size=24))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
 # scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))



ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="North"), aes(x=Sample_ID, y=value, fill=variable))+theme_bw()+
  labs (x="", y="", fill="Functional Feeding Groups")+
  geom_bar(stat="identity", position = "fill")+
  theme(text = element_text(size=18), axis.text.x = element_text(size=18))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")

ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="North"), aes(x=Site, y=value, fill=variable))+theme_bw()+
  labs (x="", y="", fill="Functional Feeding Groups")+
  geom_bar(stat="identity",)+
  theme(text = element_text(size=18), axis.text.x = element_text(size=18))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")


bfti<-ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="South"), aes(x=Landscape, y=value, fill=variable))+theme_bw()+
  labs (x="", y="CWMT", fill="Functional Feeding Groups")+
  geom_bar(stat="identity", position = "fill")+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  #scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


bfti<-ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="South"), aes(x=Landscape, y=value, fill=variable))+theme_bw()+
  labs (x="", y="", fill="Functional Feeding Groups")+
  geom_bar(stat="identity")+
  theme(text = element_text(size=24), axis.text.x = element_text(size=24))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  #scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="South"), aes(x=Sample_ID, y=value, fill=variable))+theme_bw()+
  labs (x="", y="CWMT", fill="Functional Feeding Groups")+
  geom_bar(stat="identity", position = "fill")+
  theme(text = element_text(size=18), axis.text.x = element_text(size=18))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")


ggplot(subset(dat.cwmt.long, dat.cwmt.long$Region=="South"), aes(x=Site, y=value, fill=variable))+theme_bw()+
  labs (x="", y="CWMT", fill="Functional Feeding Groups")+
  geom_bar(stat="identity")+
  theme(text = element_text(size=18), axis.text.x = element_text(size=18))+
  scale_fill_manual(values=fedcol, labels=c("Grazer", "Miner", "Xylobiont", "Shredder", "Gatherer", "Active Filter Feeder", "Passive Filter Feeder", "Predator", "Parasite", "Other"))+
  scale_x_discrete(labels=c("A" = "non-forested", "F" = "forested"))+
  theme(legend.position="none")


dat.cwmt.long4 <- dat.cwmt.long %>% group_by(Sample_ID) %>%
  mutate(Sum_value_FT   = sum(value),
         Sum_shred = sum(value[variable=="Shredder"]),
         Shred_FT_Percent = Sum_shred/Sum_value_FT*100) ## 16 different values for the 16 different sampling sites



#### now reduce dataset so we only have one row per site
dat.cwmt.long4<- dat.cwmt.long4[!duplicated(dat.cwmt.long4$Sample_ID),] ## 16 Sites, 127 Bags

hist(dat.cwmt.long4$Shred_FT_Percent)



dat.cwmt.long5 <- dat.cwmt.long4 %>% group_by(Landscape, Region) %>%
  mutate(Mean_shred_Percent = mean(Shred_FT_Percent),
         sd_shred_Percent = sd(Shred_FT_Percent),
         Mean_shred_abs = mean(Sum_shred),
         sd_shred_abs = sd(Sum_shred))





min(dat.cwmt.long5$Shred_FT_Percent)
max(dat.cwmt.long5$Shred_FT_Percent)

min(dat.cwmt.long5$Sum_shred)
max(dat.cwmt.long5$Sum_shred)


#### sum up values per site 
dat.cwmt<- dplyr::select(dat, Site, grazer2, miner2, xylobiont2, shredder2, gatherer2, activefilterfeeder2, passivefilterfeeder2, predator2, parasite2, other2)

dat.c<- dat.cwmt%>% dplyr::group_by(Site) %>%
  dplyr::summarise(across(everything(), ~ sum(., is.na(.), 0))) ### column sum per site of each feeding type so now we only have 16 columns

row.names(dat.c) <- dat.c$Site # make row names the Site
dat.c<- dat.c[,-1] # delete first column with Site

#### B) NMDS #####
dat.c.hel<- decostand(dat.c, method="hellinger") ### decostand standardises abundances to balance distributions

#### do the actual NMDS
nmds2<- metaMDS(dat.c.hel, autotransform=F) ### stress 0.07

fort2<- fortify(nmds2)
ggplot()+
  geom_point(data=subset(fort2, Score=="sites"),
             mapping = aes (x=NMDS1, y=NMDS2),
             color="black",
             alpha=0.5)+
  geom_segment(data=subset(fort2, Score == "species"),
               mapping= aes(x=0, y=0, xend= NMDS1, yend=NMDS2), 
               arrow= arrow (length=unit (0.015, "npc"),
                             type="closed"),
               color="darkgray",
               size=0.8)+
  geom_text(data= subset(fort2, Score="species"),
            mapping = aes(label = Label, x = NMDS1*1.1, y= NMDS2*1.1))+
  geom_abline (intercept = 0, slope = 0, linetype="dashed", size=0.8, color="grey")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, color="grey")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color= "black"))


#### C) PERMANOVA ####
set.seed(3)
adonis(dat.c.hel ~Landscape*Region, data=data_1, strata= data_1$Stream)

disR <- vegdist(dat.c.hel)

groups <- factor(c("N", "N", 
                   "N", "N",
                   "N", "N",
                   "N", "N",
                   "S", "S",
                   "S", "S",
                   "S", "S",
                   "S", "S"))
mod <- betadisper(disR, groups)
mod
anova(mod)



groups <- factor(c("non-forested", "forested", 
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested"))

mod <- betadisper(disR, groups)
mod
anova(mod)





#### D) NMDS Plot ####
#### show results in plot version 1 with ellipses
## create new dataframe NMDS
NMDS2=data.frame(x=nmds2$point[,1],y=nmds2$point[,2],Landscape=as.factor(data_1$Landscape), Region = as.factor(data_1$Region))

ord2<-ordiellipse(nmds2, as.factor(data_1$Landscape) ,display = "sites", kind ="sd", conf = 0.95, label = T)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#Generate ellipse points
df_ell2 <- data.frame()
for(g in levels(NMDS2$Landscape)){
  if(g!="" && (g %in% names(ord2))){
    
    df_ell2 <- rbind(df_ell2, cbind(as.data.frame(with(NMDS2[NMDS2$Landscape==g,],
                                                       veganCovEllipse(ord2[[g]]$cov,ord2[[g]]$center,ord2[[g]]$scale)))
                                    ,Landscape=g))
  }
}

head(df_ell2)
df_ell2$Landscape<-as.factor(df_ell2$Landscape)
df_ell2$Landscape <- factor(df_ell2$Landscape, levels = c("forested", "non-forested"))

NMDS.mean2=aggregate(NMDS2[,1:2],list(group=NMDS2$Landscape),mean)

fort2 <- fort2 %>%
  mutate(label = ifelse(fort2$Score=="species", Label, NA))


bft<-ggplot()+
  geom_point(data=NMDS2,aes(x,y, color=Landscape,, shape=Region), size=5)+theme_bw()+
  scale_color_manual(values=Landscol)+
  annotate("text",x=-0.45,y=0.35,,label="stress = 0.07",size=8)+ ### this indicates where the centre would be of each group
  geom_path(data=df_ell2, aes(x=NMDS1, y=NMDS2, linetype=Landscape), size=1)+
  geom_segment(data=subset(fort2, Score == "species"),
               mapping= aes(x=0, y=0, xend= NMDS1, yend=NMDS2), 
               arrow= arrow (length=unit (0.015, "npc"),
                             type="closed"),
               color="darkgray",
               size=0.8)+
  geom_text_repel(data= subset(fort2, Score="species"),
                  mapping = aes(label = label, x = NMDS1*1.2, y= NMDS2*1.2), max.overlaps = 12, size=10)+
  xlab("NMDS1")+ ylab("NMDS2")+theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  theme(legend.position = "none")



############################################## 3. Shredder #####################################################

#### first we need to subset dataset to only include real shredders
dats<- subset(dat, dat$ActiveShredder=="YES") ## 463

#### A) relative barplots ####
dats$Family <- factor(dats$Family, levels = c("Capniidae", "Nemouridae","Brachycentridae", "Leptoceridae","Limnephilidae","Sericostomatidae", "Gammaridae", "Limoniidae"))
famcol<- c("#003c30", "#01665e", "#e6f5d0", "#b8e186", "#7fbc41","#4d9221", "#fde0ef", "#f1b6da", "#de77ae")

s<-ggplot((dats))+theme_bw()+
  labs (x="", y="", fill="Taxonomic group")+
  geom_bar(stat="identity", position="fill", aes(x=Landscape, y=Total, fill=Family))+
  theme(text = element_text(size=34), axis.text.x = element_text(size=34))+
  scale_fill_manual(values=famcol)+
  guides(fill=guide_legend(title="Shredder"))


leg <- get_legend(s)
leg.gg<- as_ggplot(leg)

famcol1<- c("#003c30", "#01665e", "#b8e186", "#7fbc41", "#fde0ef", "#f1b6da", "#de77ae", "#8e0152")

tgs<-ggplot(subset(dats, dats$Region=="North"))+theme_bw()+
  labs (x="", y="", fill="Taxonomic group")+
  geom_bar(stat="identity", position="fill", aes(x=Landscape, y=Total, fill=Family))+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22))+
  scale_fill_manual(values=famcol1)+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


tgs<-ggplot(subset(dats, dats$Region=="North"))+theme_bw()+
  labs (x="", y="Total number of individuals", fill="Taxonomic group")+
  geom_bar(stat="identity", aes(x=Landscape, y=Total, fill=Family))+
  theme(text = element_text(size=24), axis.text.x = element_text(size=24))+
  scale_fill_manual(values=famcol1)+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))

famcol2<- c("#01665e", "#e6f5d0", "#b8e186", "#7fbc41","#4d9221", "#8e0152")

tis<-ggplot(subset(dats, dats$Region=="South"))+theme_bw()+
  labs (x="", y="Total number of individuals", fill="Taxonomic group")+
  geom_bar(stat="identity", position="fill", aes(x=Landscape, y=Total, fill=Family))+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22))+
  scale_fill_manual(values=famcol2)+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


tis<-ggplot(subset(dats, dats$Region=="South"))+theme_bw()+
  labs (x="", y="", fill="Taxonomic group")+
  geom_bar(stat="identity", aes(x=Landscape, y=Total, fill=Family))+
  theme(text = element_text(size=24), axis.text.x = element_text(size=24))+
  scale_fill_manual(values=famcol2)+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


#### B) NMDS ####
#### make species matrix with dcast
dat.long<-dcast(dats, Site~Taxon, value.var = "Total", sum) # transpose table based on Taxon and Sample Id, summing up the Total per Taxon
row.names(dat.long) <- dat.long$Sample_ID # make row names the Sample ID
dat.long<- dat.long[,-1] # delete first column with Sample ID 

#### transform the species community data using hellinger transformation
## hellinger is useful when few abundant species dominate community
dat.long.hel<- decostand(dat.long, method="hellinger") ### decostand standardises abundances to balance distributions

#### do the actual NMDS
nmds1<- metaMDS(dat.long.hel, autotransform=F) ### 20 stress: 0.067

#### use basic plottic function
ordiplot(nmds1, type="t") # points are sampling sites and crosses are taxa
autoplot(nmds1)

## full control with fortified ordination output
#fort<- fortify(nmds1.1)
fort<- fortify(nmds1)
ggplot()+
  geom_point(data=subset(fort, Score=="sites"),
             mapping = aes (x=NMDS1, y=NMDS2),
             color="black",
             alpha=0.5)+
  geom_segment(data=subset(fort, Score == "species"),
               mapping= aes(x=0, y=0, xend= NMDS1, yend=NMDS2), 
               arrow= arrow (length=unit (0.015, "npc"),
                             type="closed"),
               color="darkgray",
               size=0.8)+
  geom_text(data= subset(fort, Score="species"),
            mapping = aes(label = Label, x = NMDS1*1.1, y= NMDS2*1.1))+
  geom_abline (intercept = 0, slope = 0, linetype="dashed", size=0.8, color="grey")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, color="grey")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color= "black"))

#### C) PERMANOVA ####
dat1<- dat%>% dplyr::group_by(Site) %>%
  dplyr::mutate(Sum_Total = sum(Total, na.rm=TRUE)) #### calculate the sum of the Total Dry Mass in site
dat1<- dat1[!duplicated(dat1$Site),]
data_1<- dplyr::select(dat1, Stream, Region, Landscape) ## make a dataframe for groups
data_1$Landscape<- as.factor(data_1$Landscape)

set.seed(3)
#adonis(dat.long.hel ~Landscape+Region, data=data_1) ### nothing significant
adonis(dat.long.hel ~Landscape*Region, data=data_1, strata= data_1$Stream)

#adonis2(dat.long.hel ~Landscape+Region, data=data_1) ### nothing significant
#anosim(dat.long.hel, data_1$Region, distance = "bray", permutations = 999)

disR <- vegdist(dat.long.hel)

groups <- factor(c("N", "N", 
                   "N", "N",
                   "N", "N",
                   "N", "N",
                   "S", "S",
                   "S", "S",
                   "S", "S",
                   "S", "S"))
mod <- betadisper(disR, groups)
mod
anova(mod)



groups <- factor(c("non-forested", "forested", 
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested"))

mod <- betadisper(disR, groups)
mod
anova(mod)



#### D) NMDS Plot ####
## create new dataframe NMDS
NMDS=data.frame(x=nmds1$point[,1],y=nmds1$point[,2],Landscape=as.factor(data_1$Landscape), Region = as.factor(data_1$Region))


ord<-ordiellipse(nmds1, as.factor(data_1$Landscape) ,display = "sites", kind ="sd", conf = 0.95, label = T)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#Generate ellipse points
df_ell <- data.frame()
for(g in levels(NMDS$Landscape)){
  if(g!="" && (g %in% names(ord))){
    
    df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$Landscape==g,],
                                                     veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,ord[[g]]$scale)))
                                  ,Landscape=g))
  }
}

head(df_ell)
df_ell$Landscape<-as.factor(df_ell$Landscape)
df_ell$Landscape <- factor(df_ell$Landscape, levels = c("non-forested", "forested"))

NMDS.mean=aggregate(NMDS[,1:2],list(group=NMDS$Landscape),mean)
Landscol<- c("#d8b365","#1b9e77")

fort <- fort %>%
  mutate(label = ifelse(fort$Score=="species", Label, NA))


taxS<-ggplot()+
  geom_point(data=NMDS,aes(x,y, color=Landscape,, shape=Region), size=3)+theme_bw()+
  scale_color_manual(values=Landscol)+
  annotate("text",x=-1.5,y=1.1,,label="stress = 0.11",size=8)+ 
  #annotate("text",x=NMDS.mean$x,y=NMDS.mean$y,label=NMDS.mean$group,size=4)+ ### this indicates where the centre would be of each group
  geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2, linetype=Landscape), size=1)+
  geom_segment(data=subset(fort, Score == "species"),
               mapping= aes(x=0, y=0, xend= NMDS1, yend=NMDS2), 
               arrow= arrow (length=unit (0.015, "npc"),
                             type="closed"),
               color="darkgray",
               size=0.8)+
  geom_text_repel(data= subset(fort, Score="species"),
                  mapping = aes(label = label, x = NMDS1*1.2, y= NMDS2*1.2), max.overlaps = 12, size=10)+
  xlab("NMDS1")+ ylab("NMDS2")+theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  theme(legend.position = c(0.3, 0.25), legend.title = element_text(size=20), legend.text = element_text(size=20), legend.direction="horizontal")+
  theme(legend.position = "none")


############################# 4. EPT ########################################

#### first we need to subset dataset to only include real shredders
EPT<- subset(dat, dat$Order =="Ephemeroptera" | dat$Order=="Plecoptera" | dat$Order == "Trichoptera")


#### A) relative barplots ####

EPT$Family <- factor(EPT$Family, levels = c("Baetidae", "Ephemerellidae","Ephemeridae", "Heptageniidae","Leptophlebiidae","Capniidae", "Leuctridae", "Nemouridae", "Perlodidae", "Taeniopterygidae", "Brachycentridae", "Glossosomatidae", "Hydropsychidae", "Hydroptilidae", "Leptoceridae", "Limnephilidae", "Odontoceridae", "Philopotamidae", "Polycentropodidae", "Rhyacophilidae", "Sericostomatidae"))

famcol<- c("#ece2f0", "#d0d1e6", "#a6bddb", "#67a9cf", "#3690c0", "#003c30", "#02818a", "#01665e", "#55ab9b", "#014636","#e6f5d0", "#f7fcb9", "#d9f0a3", "#addd8e", "#b8e186", "#7fbc41", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#4d9221")

es<-ggplot((EPT))+theme_bw()+
  labs (x="", y="", fill="Taxonomic group")+
  geom_bar(stat="identity", position="fill", aes(x=Landscape, y=Total, fill=Family))+
  scale_fill_manual(values=famcol)+
  theme(text = element_text(size=34), axis.text.x = element_text(size=34))+
  guides(fill=guide_legend(ncol=1))+
  guides(fill=guide_legend(title="EPT"))

eleg <- get_legend(es)
eleg.gg<- as_ggplot(eleg)



famcol1<- c("#ece2f0", "#d0d1e6", "#a6bddb", "#67a9cf", "#3690c0", "#003c30", "#02818a", "#01665e", "#55ab9b", "#014636", "#d9f0a3", "#addd8e", "#b8e186", "#7fbc41", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d")

etgs<-ggplot(subset(EPT, EPT$Region=="North"))+theme_bw()+
  labs (x="", y="", fill="Taxonomic group")+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22))+
  geom_bar(stat="identity", position="fill", aes(x=Landscape, y=Total, fill=Family))+
  scale_fill_manual(values=famcol1)+theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


etgs<-ggplot(subset(EPT, EPT$Region=="North"))+theme_bw()+
  labs (x="", y="Total number of individuals", fill="Taxonomic group")+
  theme(text = element_text(size=24), axis.text.x = element_text(size=24))+
  geom_bar(stat="identity", aes(x=Landscape, y=Total, fill=Family))+
  scale_fill_manual(values=famcol1)+theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))



famcol2<- c("#ece2f0", "#67a9cf", "#3690c0", "#02818a", "#01665e", "#55ab9b", "#014636","#e6f5d0", "#f7fcb9", "#d9f0a3", "#b8e186", "#7fbc41", "#c7e9c0", "#41ab5d", "#4d9221")

etis<-ggplot(subset(EPT, EPT$Region=="South"))+theme_bw()+
  labs (x="", y="Total number of individuals", fill="Taxonomic group")+
  geom_bar(stat="identity", position="fill", aes(x=Landscape, y=Total, fill=Family))+
  scale_fill_manual(values=famcol2)+
  theme(text = element_text(size=22), axis.text.x = element_text(size=22))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))


etis<-ggplot(subset(EPT, EPT$Region=="South"))+theme_bw()+
  labs (x="", y="", fill="Taxonomic group")+
  geom_bar(stat="identity", aes(x=Landscape, y=Total, fill=Family))+
  scale_fill_manual(values=famcol2)+
  theme(text = element_text(size=24), axis.text.x = element_text(size=24))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))

#### B) NMDS ####

#### make species matrix with dcast
dat.long<-dcast(EPT, Site~Taxon, value.var = "Total", sum) # transpose table based on Taxon and Sample Id, summing up the Total per Taxon
row.names(dat.long) <- dat.long$Sample_ID # make row names the Sample ID
dat.long<- dat.long[,-1] # delete first column with Sample ID 

#### transform the species community data using hellinger transformation
## hellinger is useful when few abundant species dominate community
dat.long.hel<- decostand(dat.long, method="hellinger") ### decostand standardises abundances to balance distributions

#### do the actual NMDS
nmds1<- metaMDS(dat.long.hel, autotransform=F) ### 20 stress: 0.067

#### use basic plottic function
ordiplot(nmds1, type="t") # points are sampling sites and crosses are taxa
autoplot(nmds1)

## full control with fortified ordination output
#fort<- fortify(nmds1.1)
fort<- fortify(nmds1)
ggplot()+
  geom_point(data=subset(fort, Score=="sites"),
             mapping = aes (x=NMDS1, y=NMDS2),
             color="black",
             alpha=0.5)+
  geom_segment(data=subset(fort, Score == "species"),
               mapping= aes(x=0, y=0, xend= NMDS1, yend=NMDS2), 
               arrow= arrow (length=unit (0.015, "npc"),
                             type="closed"),
               color="darkgray",
               size=0.8)+
  geom_text(data= subset(fort, Score="species"),
            mapping = aes(label = Label, x = NMDS1*1.1, y= NMDS2*1.1))+
  geom_abline (intercept = 0, slope = 0, linetype="dashed", size=0.8, color="grey")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, color="grey")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color= "black"))

#### C) PERMANOVA ####
dat1<- dat%>% dplyr::group_by(Site) %>%
  dplyr::mutate(Sum_Total = sum(Total, na.rm=TRUE)) #### calculate the sum of the Total Dry Mass in site
dat1<- dat1[!duplicated(dat1$Site),]
data_1<- dplyr::select(dat1, Stream, Region, Landscape) ## make a dataframe for groups
data_1$Landscape<- as.factor(data_1$Landscape)

set.seed(3)
#adonis(dat.long.hel ~Landscape+Region, data=data_1) ### nothing significant
adonis(dat.long.hel ~Landscape*Region, data=data_1, strata= data_1$Stream)

#adonis2(dat.long.hel ~Landscape+Region, data=data_1) ### nothing significant
#anosim(dat.long.hel, data_1$Region, distance = "bray", permutations = 999)

disR <- vegdist(dat.long.hel)

groups <- factor(c("N", "N", 
                   "N", "N",
                   "N", "N",
                   "N", "N",
                   "S", "S",
                   "S", "S",
                   "S", "S",
                   "S", "S"))
mod <- betadisper(disR, groups)
mod
anova(mod)



groups <- factor(c("non-forested", "forested", 
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested",
                   "non-forested", "forested"))

mod <- betadisper(disR, groups)
mod
anova(mod)





#### D) NMDS Plot ####
#### make species matrix with dcast
dat.long<-dcast(EPT, Site~Family, value.var = "Total", sum) # transpose table based on Taxon and Sample Id, summing up the Total per Taxon
row.names(dat.long) <- dat.long$Sample_ID # make row names the Sample ID
dat.long<- dat.long[,-1] # delete first column with Sample ID 

#### transform the species community data using hellinger transformation
## hellinger is useful when few abundant species dominate community
dat.long.hel<- decostand(dat.long, method="hellinger") ### decostand standardises abundances to balance distributions

#### do the actual NMDS
nmds1<- metaMDS(dat.long.hel, autotransform=F) ### 20 stress: 0.067

## full control with fortified ordination output
#fort<- fortify(nmds1.1)
fort<- fortify(nmds1)


## create new dataframe NMDS
NMDS=data.frame(x=nmds1$point[,1],y=nmds1$point[,2],Landscape=as.factor(data_1$Landscape), Region = as.factor(data_1$Region))


ord<-ordiellipse(nmds1, as.factor(data_1$Landscape) ,display = "sites", kind ="sd", conf = 0.95, label = T)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#Generate ellipse points
df_ell <- data.frame()
for(g in levels(NMDS$Landscape)){
  if(g!="" && (g %in% names(ord))){
    
    df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$Landscape==g,],
                                                     veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,ord[[g]]$scale)))
                                  ,Landscape=g))
  }
}

head(df_ell)
df_ell$Landscape<-as.factor(df_ell$Landscape)
df_ell$Landscape <- factor(df_ell$Landscape, levels = c("non-forested", "forested"))

NMDS.mean=aggregate(NMDS[,1:2],list(group=NMDS$Landscape),mean)
Landscol<- c("#d8b365","#1b9e77")

fort <- fort %>%
  mutate(label = ifelse(fort$Score=="species", Label, NA))


EPT<-ggplot()+
  geom_point(data=NMDS,aes(x,y, color=Landscape,, shape=Region), size=3)+theme_bw()+
  scale_color_manual(values=Landscol)+
  annotate("text",x=-1,y=1.2,,label="stress = 0.16",size=8)+ 
  #annotate("text",x=NMDS.mean$x,y=NMDS.mean$y,label=NMDS.mean$group,size=4)+ ### this indicates where the centre would be of each group
  geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2, linetype=Landscape), size=1)+
  geom_segment(data=subset(fort, Score == "species"),
               mapping= aes(x=0, y=0, xend= NMDS1, yend=NMDS2), 
               arrow= arrow (length=unit (0.015, "npc"),
                             type="closed"),
               color="darkgray",
               size=0.8)+
  geom_text_repel(data= subset(fort, Score="species"),
                  mapping = aes(label = label, x = NMDS1*1.2, y= NMDS2*1.2), max.overlaps = 12, size=10)+
  xlab("NMDS1")+ ylab("NMDS2")+theme(text = element_text(size=22), axis.text.x = element_text(size=22), axis.text.y = element_text(size=22) )+
  theme(legend.position = "None")
#theme(legend.position = c(0.25, 0.1), legend.title = element_text(size=6), legend.text = element_text(size=6), legend.direction="horizontal")


#### 5. Merge Plots ####

###### Figure 2 ####

tax+tg+ti+leg.ggt+EPT+etgs+etis+eleg.gg+taxS+tgs+tis+leg.gg+ 
  plot_layout(ncol=4, widths = c(4,1.5,1.5,1.5,4,1.5,1.5,1.5,4,1.5,1.5,1.5))+
  plot_annotation(tag_levels = list(c('A', '', '', '', 'B', '', '', '', "C", "", "", ""))) &  theme(plot.tag.position = c(0, 0.95), plot.tag = element_text(size = 20, hjust = 0, vjust = 0))


set.seed(3)  

##### Figure 3 #####
ft+ftg+fti+leg.gft+bft+bftg+bfti+ 
  plot_layout(ncol=4, widths = c(4,1.5,1.5,1.5,4,1.5,1.5,1.5))+
  plot_annotation(tag_levels = list(c('A', '', '', '', 'B', '', ''))) &  theme(plot.tag.position = c(0, 0.95), plot.tag = element_text(size = 20, hjust = 0, vjust = 0))

