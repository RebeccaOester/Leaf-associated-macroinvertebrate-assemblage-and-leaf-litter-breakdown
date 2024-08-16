READ ME

Leaf-associated macroinvertebrate assemblage and leaf litter breakdown in headwater streams depend on local riparian vegetation

Rebecca Oester, Paula C. dos Reis Oliveira, Marcelo S. Moretti, Florian Altermatt, Andreas Bruder

rebecca.oester@eawag.ch


This READ ME file contains information on the r-scripts and variables in the datasets used for the analyses. 


Datafiles:

1) datTOT

Main dataset with information on the macroinvertebrate community in each leaf litter bag with the following variables:
Sample_ID: unique identifier for each leaf litter bag (composed of the Region (TG or TI), Stream ID (1-4), Landscape (F = Forested; A= Non-Forested))
Type: indication that These communities stem from leaf litter bags
Site_ID: Unique identifier for the stream sites
Order: taxonomic order
Family: taxonomic family
Genus: taxonomic genus or subfamily
Species: species Name or sp. indication if not further determined
Larvae: number of larvae found
Adult: number of adults found
Nymphe: number of nymphs found
Total: the total number of this taxon in this leaf litter bag (sum of number of larvae, adult and nymphe)
Length1: the total body length of individual 1 of this taxa in this leaf litter bag in mm
Length2: the total body length of individual 2 of this taxa in this leaf litter bag in mm
Length3: the total body length of individual 3 of this taxa in this leaf litter bag in mm
Length4: the total body length of individual 4 of this taxa in this leaf litter bag in mm
Length5: the total body length of individual 5 of this taxa in this leaf litter bag in mm
Length6: the total body length of individual 6 of this taxa in this leaf litter bag in mm
Length7: the total body length of individual 7 of this taxa in this leaf litter bag in mm
Length8: the total body length of individual 8 of this taxa in this leaf litter bag in mm
Length9: the total body length of individual 9 of this taxa in this leaf litter bag in mm
Length10: the total body length of individual 10 of this taxa in this leaf litter bag
NumberExtra: in case of more than 10 individuals, number of exceeding abundances
AverageLength: the mean of values from Length1-Length10 in mm
TotalLength: the sum of values from Length1-Length10 in mm
Taxon: Taxon name (lowest taxonomic deterimnation usually genus and species epithet)
SpeciesRichness: number of taxa in leaf litter bag
ShannonIndex: shannon diversity indext for each leaf litter bag
grazer: indication for functional feeding group from 0 (no preference) to 10 (strong preference) according to the freshwaterecology.info database
miner: indication for functional feeding group from 0 (no preference) to 10 (strong preference) according to the freshwaterecology.info database
xylobiont: indication for functional feeding group from 0 (no preference) to 10 (strong preference) according to the freshwaterecology.info database
shredder: indication for functional feeding group from 0 (no preference) to 10 (strong preference) according to the freshwaterecology.info database
gatherer: indication for functional feeding group from 0 (no preference) to 10 (strong preference) according to the freshwaterecology.info database
activefilterfeeder: indication for functional feeding group from 0 (no preference) to 10 (strong preference) according to the freshwaterecology.info database
passivefilterfeeder: indication for functional feeding group from 0 (no preference) to 10 (strong preference) according to the freshwaterecology.info database
predator: indication for functional feeding group from 0 (no preference) to 10 (strong preference) according to the freshwaterecology.info database
parasite: indication for functional feeding group from 0 (no preference) to 10 (strong preference) according to the freshwaterecology.info database
other: indication for functional feeding group from 0 (no preference) to 10 (strong preference) according to the freshwaterecology.info database
ActiveShredder: indication for functional feeding being a shredder (YES or NO)
lna: from taxon-specific allometric length-weight regressions of the form W = aL^b, ln of a
b: from taxon-specific allometric length-weight regressions of the form W = aL^b, b
a: from taxon-specific allometric length-weight regressions of the form W = aL^b, a
DryMass: based on the taxon-specific length-weight regressions from the AverageLength, the estimated dry mass in mg
TotalDryMass: the DryMass multiplied by the total number of individuals of this taxon in this leaf litter bag
Sum_Biomass: the total biomass of macroinvertebrates in this leaf litter bag
Stream: unique identifier for streams
Site: unique identifier for sites
Region: unique identifier for regions
Treatment: indication that the bags were made of coarse mesh bag and filled with either alder, ash or a mix of both
Landscape: forested (F) or non-forested (A)
TreatmentLandscape: string combination of Treatment and Landscape
Leaf: Leaf litter species used in the leaf litter bag
lambda.correct: The fragmentation rates associated with this leaf litter bag (for mixed leaf litter bags, it's the average of alder and ash)



2) dat.ML.new
Dataset with additional information of each leaf litter species in each leaf litter bag with the following variables:

ID: unique identifier for each leaf litter bag (composed of the Region (TG or TI), Stream ID (1-4), Landscape (F = Forested; A= Non-Forested))
Sample_ID: unique identifier for each leaf litter bag (composed of the Region (TG or TI), Stream ID (1-4), Landscape (F = Forested; A= Non-Forested))
Stream: unique identifier for each stream
Site: unique identifier for each site
Region: unique identifier for each reagion
Bag: indication on the mesh size of the leaf litter bag
Treatment: indication of the combined mesh size and leaf litter species
Landscape: forested (F) or non-forested (A)
TreatmentLandscape: tring combination of Treatment and Landscape
DW_beforeGrammAlder: dry weight (g) of alder at the start
DW_beforeGrammAsh: dry weight (g) of ash at the start
DW_Total: Sum of DW_beforeGrammAlder and DW_beforeGrammAsh only relevant for mixed leaf litter bags
Mesh: fine or coarse mesh bag excluding or allowing macroinvertebrate consumers to the leaf litter bags, respectively.
Leave1: Leaf litter species #1
Leave2: Leaf litter species #2
Leaf: Leaf litter species or Mix
DW_afterGrammAlder: dry weight (g) for alder at the end
DW_afterGrammAsh: dry weight (g) for ash at the end
Loss_AbsolutAlder: number of grams lost for alder
Loss_AbsolutAsh: number of grams lost for ash
Loss_Absolut_Sum: total number of grams lsot
Loss_RelativeAlder: % loss for alder
Loss_RelativeAsh: % loss for ash
Loss_Relative_Sum: the total of % loss for alder and ash
Leaf_Disk_Weight_Alder: additional weight (g) from leaf disks of this alder used for fungal biomass determination for another project
Leaf_Disk_Weight_Ash: additional weight (g) from leaf disks of this ash used for fungal biomass determination for another project
DW_gramAlder_Adj: adjusted dry weight (g) of alder 
DW_gramAsh_Adj: adjusted dry weight (g) of ash
DW_gramAlder_Adjusted: adjusted dry weight (g) of alder 
DW_gramAsh_Adjusted: adjusted dry weight (g) of ash
Site_ID: unique identifier for site
AvGDegreeDays: average degree days accumulated over the course of the field experiment
kAlder: decomposition rate for alder
kAsh: decomposition rate for ash
k: average decomposition rate
kcAlder: coarse mesh decomposition rate for alder
kcAsh: coarse mesh decomposition rate for ash
kcMix: coarse mesh decomposition rate for mixed leaf litter bags
kfAlder: fine mesh decomposition rate for alder
kfAsh: fine mesh decomposition rate for ash
kfMix: fine mesh decomposition rate for mixed leaf litter bags
lambdaFAlder: fragmentation rates for alder
lambdaFAsh: fragmentation rates for ash
lambdaFMix: fragmentation rates for mixed leaf litter bags
lambda.correct: fragmentation rates for each Treatment combination



R-scripts:
Run in R (version version 4.0.3) on Windows. 


1) Basic_Results

requires datTOT and datML.new

Calculations on 1) abundance, 2) diversity, 3) biomass and 4) decomposition rates to describe the effects of riparian forests on leaf-associated macroinvertebrate communities


2) CommunityStructure

requires datTOT

Calculations on 1) abundance, 2) diversity and 3) biomass of shredders and EPT separately plus figure making


3) MZB_CommunityComposition

requires datTOT

Ordinations on 1) Taxonomy, 2) Abundance-CWMT Feeding Types, 3) Biomass-CWMT Feeding Types, 4) Shredders, and 5) EPT plut figure making


4)Fragmentation_LRR

requires datTOT

Calculations on how fragmentation rates and the LRR thereof changes in forested and non-forested sites



5) Fragmentation_MZB_Structure

requires datTOT

Supplementary calculations on the relationshipts between fragmentation rates and different abundance, diversity and biomass of macroinvertebrates and specifically on shredders



