
#The following file is an investigation of M. domestica and M. serverseii
#the data analyzed was gathered by the Myles lab in 2016 and 2017 under the file
#names 20200306_supp_table_1 and 20191017_supp_table_2
#The object of this investigation is to investigate differences and similarities in
#wild and domestic apples.
install.packages("tidyverse")
library(tidyverse)
install.packages("openxlsx")
library("openxlsx")

#######################################################################
#DATA CURATION# #!#
#######################################################################


#setting working directory
setwd("~/Desktop/DALMSc/Biostats/abc_project")

#Read in the raw data. This is the only table used in this analysis.
abc_data = read.csv("Watts_et_al_2021_supp_data.csv")

#This supplementary table is from Watts et al 2021 *LINK*.

# reduce table to only useful phenotypes 
# make a vector of the column numbers that we wish to keep, this will exclude phenotypes from other years that is not usable in the current analysis
c_to_k = c(1,2,5,6,7,9,21,22,35:39,41,44,46)
abc_data = abc_data[,c_to_k]
#this analysis will only look at dessert and wild apples
#trim data to contain only dessert and wild apples. 
index = which(abc_data$use == 'dessert' | abc_data$use == 'wild')
abc_data=abc_data[index,]
#removing NA data from the 'species' , and rewriting the index to analyze only dessert and wild apples with defined species attributes
abc_data=abc_data[!is.na(abc_data[,'species']),]

#This table now only includes dessert and wild apples 

# the following loop will check each row and sum the "NA" from that row
#Then it will put it into "na_sum" for each apple sample
#The result is a value with 1 row of 879 values and each entry represents the number of missing data points for that sample 
na_sum = c()
for (i in 1:nrow(abc_data)) {
  na_sum[i] = sum(is.na(abc_data[i,7:16]))
}
#lets remove the samples that are missing all phenotype data 
table(na_sum)
#0   1   2   3   4   5   6   7   8   9  10 
#220 205 141 131   5   1   1  45  99  25   6 
# 6 samples are missing all phenotype data and must be removed
index_keep = !(na_sum == 10)
abc_data = abc_data[index_keep,]
#get a count of the number of each species in the data 
sum(abc_data$species == "domestica", na.rm = T)
#[1] 797. There are 797 dessert 

sum(abc_data$species == "sieversii", na.rm = T)
#[1] 76. there are 76 sieversii in the dataset
total_wild = sum(abc_data$species == "sieversii", na.rm = T)
total_dessert = sum(abc_data$use == 'dessert', na.rm = T)
#how many phenotypes?
# there are 10 phenotypes 
n_pheno = 10
#The first 6 columns are meta data 

### EXAMINE THE DATA IN DETAIL ###
#produce figures that describes the amount of missing data 
#this will be missing data by sample
#this loop records how much data is missing for each apple
na_sum = c()
for (i in 1:nrow(abc_data)) {
  na_sum[i] = sum(is.na(abc_data[i,7:16]))
}

#this will calculate what proportion of data is missing from each sample, and then plot the number of samples missing what proportion of data
missing_percent_by_sample = (na_sum/n_pheno)*100
barplot(table(missing_percent_by_sample))
#This is a base plot 
#Missing data by phenotype is important 

#Make a figure to show the missing data by phenotype 

index_wild = which(abc_data$use == 'wild')
index_dess = which(abc_data$use == 'dessert')
na_prop_pheno_wild = c()
na_prop_pheno_dess = c()
for (i in 7:16) {#only loop through columns that have phenotype data (not metadata)
  na_prop_pheno_wild[i] = (sum(is.na(abc_data[index_wild,i]))/length(index_wild))*100
  na_prop_pheno_dess[i] = (sum(is.na(abc_data[index_dess,i]))/length(index_dess))*100
}
na_prop_pheno_dess
na_prop_pheno_wild
#this is the proportion of missing data for each pheno, but the first 6 entries are NAs. Need to remove the NAs. 
na_prop_pheno_dess = na_prop_pheno_dess[7:16]
na_prop_pheno_dess
na_prop_pheno_wild = na_prop_pheno_wild[7:16]
na_prop_pheno_wild
#NAs removed

#Make dataframe with the prop_pheno missing that will be used to make a barplot in ggplot
pheno_dat = c(rbind(na_prop_pheno_dess, na_prop_pheno_wild))
group_names = rep(c("dessert", "wild"), 10)
pheno_names = colnames(abc_data)[7:16]
pheno_names_alt = c('Flowering Date', 'Precocity', 'Soluble Solids', 'Acidity', 'Harvest Date', 'Firmness', 'Weight', 'Change in acidity during storage', 'Change in firmness during storage', 'Phenolic Content')
pheno_names_alt = c(rbind(pheno_names_alt, pheno_names_alt))
dataframe2plot = data.frame(pheno_dat, group_names, pheno_names_alt)

#the following is transformations of the columns into factors so they can have levels.

dataframe2plot$pheno_names_alt = factor(pheno_names_alt, levels = rev(c('Precocity', 'Flowering Date', 'Harvest Date', 'Firmness', 'Weight', 'Acidity', 'Soluble Solids', 'Phenolic Content', 'Change in acidity during storage', 'Change in firmness during storage')))
dataframe2plot$group_names = factor(group_names, levels = c("wild","dessert"))
dataframe2plot[order(dataframe2plot$pheno_names_alt, dataframe2plot$group_names),]

#plot
percent_missing_data_chart = ggplot(dataframe2plot, aes(fill=group_names, y=pheno_names_alt, x= pheno_dat))+
  geom_bar(position="dodge", stat="identity") +
  labs(title= "% Data Missing by Phenotype", x='% Data Missing',y= 'Phenotype', fill = "Apple Type") +
  scale_fill_manual(values = c('#FFDB6D','#00AFBB'))+theme_bw() +guides(fill= guide_legend(reverse= TRUE)) + scale_x_continuous( limits = c(0,100))+theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 10,))

percent_missing_data_chart

###Writing above plot to file ###

pdf(file= "supp_1_plot.pdf", width = 7, height = 4)

 ggplot(dataframe2plot, aes(fill=group_names, y=pheno_names_alt, x= pheno_dat))+
  geom_bar(position="dodge", stat="identity") +
  labs(title= "% Data Missing by Phenotype", x='% Data Missing',y= 'Phenotype', fill = "Apple Type") +
  scale_fill_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+theme_bw() +guides(fill= guide_legend(reverse= TRUE)) + scale_x_continuous( limits = c(0,100))+theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 10,))

dev.off()

#After looking at this plot, I decided to keep all of the 10 phenotypes because, even with low sample sizes I feel that the analyses may be informative.

#Create a new data frame with the column headers named like in the figure above and sort it as it will be plotted for all following figures. Then write this table to file - it is the final phenotype file that will read in for the remainder of the analyses in this script.
#I will make a dara frame that has the useful metadata and the ten phenotypes


#DATA SORTING

abc_data = read.csv("Watts_et_al_2021_supp_data.csv")

index = which(abc_data$use == 'dessert' | abc_data$use == 'wild')
abc_data=abc_data[index,]
#removing NA data from the 'species' , and rewriting the index so i can analyse only dessert and wild apples with defined species attributes
abc_data=abc_data[!is.na(abc_data[,'species']),]

index_wild = which(abc_data$use == 'wild')
index_dess = which(abc_data$use == 'dessert')
#Modifying the labels on Wild and Dessert so that they are capitalized for graph labelling. 
abc_data$use[index_wild] = "Wild"
abc_data$use[index_dess] = "Dessert"

c_to_k = c(1,2,5:9,21,22,35:39,41,44,46 )
abc_data = abc_data[,c_to_k]

#Theses are the columns I need, now I need to rename the col names 
#Ive added 'use' to the pheno_names_alt 
pheno_names_alt = c('Plant ID','Apple ID', 'Species','Use','Country','World','Release Year', 'Flowering Date', 'Precocity', 'Soluble Solids', 'Acidity', 'Harvest Date', 'Firmness', 'Weight', 'Change in acidity during storage', 'Change in firmness during storage', 'Phenolic Content')
for (i in 1:ncol(abc_data)){
  names(abc_data)[i] = pheno_names_alt[i]
}

abc_data
# This has renamed them. Now they need to be reoredered. 
# I am putting USE at the right-most column of meta-data for ease of trimming 'abc_data' later on
#Ive added 'Use' to pheno_names_order
pheno_names_order = c('Plant ID','Apple ID', 'Species','Country','World','Release Year','Use','Precocity', 'Flowering Date', 'Harvest Date', 'Firmness', 'Weight', 'Acidity', 'Soluble Solids', 'Phenolic Content', 'Change in acidity during storage', 'Change in firmness during storage')

abc_data = abc_data[,pheno_names_order]
######ADD NEEDED META DATA TO THIS FILE
#Okay. Now this table is ordered and named properly. 
#writing to a final file. 

write.csv(abc_data,file = '~/Desktop/DALMSc/Biostats/abc_project/final_phenotype_table.csv',row.names = FALSE)

#File written

#######################################################################
#PCA#  #!#
#######################################################################
#In this section, I will analyse the data using a principal components analysis in order to determine if there are differences between wild and dessert apples. 

setwd("~/Desktop/DALMSc/Biostats/abc_project")
abc_data = read.csv('~/Desktop/DALMSc/Biostats/abc_project/final_phenotype_table.csv')
#remove metadata for PCA
pca_data = abc_data[8:17]
#create indices  
index_wild = which(abc_data$Use == 'Wild')
index_dess = which(abc_data$Use == 'Dessert')


layout(matrix(1:10, 2, 5, byrow = T))
#this will visualize each phenotype as a histogram
for (i in 1:ncol(pca_data)) {
  hist(pca_data[,i], xlab = colnames(pca_data)[i])
}
#this data will need to be scaled in order to appropriately interpret it in the PCA. 
#Scaling the data 
pca_data_scaled = scale(pca_data)

#visualizing scaled data as histograms
layout(matrix(1:10, 2, 5, byrow = T))
for (i in 1:ncol(pca_data_scaled)) {
  hist(pca_data_scaled[,i], xlab = colnames(pca_data[i]))
}
dev.off()

#Set NA values to 0
#removal of NAs by setting them to 0, which is now the mean of each phenotype
pca_data_scaled_noNAs = apply(pca_data_scaled, 2, function (x) {ifelse(is.na(x), 0, x)})

#running the PCA
pca1 = prcomp(pca_data_scaled_noNAs) 
scores = pca1$x
#reporting the proportion variance explained by each PC
prop_var_tab = summary(pca1) 
prop_var_tab


#Plotting the PCA 
###
install.packages(ggplot2)
library(ggfortify)
###
#preliminary plot. 
plot(pca1$x[,1],pca1$x[,3])

### PCA PLOTTING ### 

#Plot PC1 and PC2
pdf(file= "PCA1_PC2_plot.pdf", width = 7, height = 4)
pc1pc2_plot = autoplot(pca1, data = abc_data, colour = 'Use')+xlab('PC1 (22.3%)')+ylab('PC2 (13.7%)')+scale_colour_manual(name = "Apple Type", labels = c('Cultivated','Wild'),values = c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.9, 0.9),legend.title.align = 0.5,legend.background = element_rect(size = 0.5,linetype = 'solid', colour = 'black'),legend.title = element_text(size = 10,face = "bold"), legend.text = element_text(size = 10),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))
pc1pc2_plot
dev.off()

#PLOT PC1 AND PC3
pc1pc3_plot= autoplot(pca1, x = 1, y = 3, data = abc_data, colour = 'Use')+xlab('PC1 (22.3%)')+ylab('PC3 (13.4%)')+scale_colour_manual(name = "Apple Type", labels = c('Cultivated','Wild'),values = c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.9, 0.9),legend.title.align = 0.5,legend.background = element_rect(size = 0.5,linetype = 'solid', colour = 'black'),legend.title = element_text(size = 10,face = "bold"), legend.text = element_text(size = 10),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))
pc1pc3_plot

#PLOT PC1 and PC4
pc1pc4_plot = autoplot(pca1, x = 1, y = 4, data = abc_data, colour = 'Use')+
  labs(title= "Principal Components Analysis of Wild and Dessert Apples")+
  scale_colour_manual(values = c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.9, 0.9),legend.title.align = 0.5,legend.background = element_rect(size = 0.5,linetype = 'solid', colour = 'black'),legend.title = element_text(size = 9,face = "bold"), legend.text = element_text(size = 8),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))
pc1pc4_plot

#PLOT PC2 and PC3
pc2pc3_plot = autoplot(pca1, x = 2, y = 3, data = abc_data, colour = 'Use')+
  labs(title= "Principal Components Analysis of Wild and Dessert Apples")+
  scale_colour_manual(values = c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.9, 0.9),legend.title.align = 0.5,legend.background = element_rect(size = 0.5,linetype = 'solid', colour = 'black'),legend.title = element_text(size = 9, face = "bold"), legend.text = element_text(size = 8),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))
pc2pc3_plot

#significance testing

pc1_scores = pca1$x[,1]
pc2_scores = pca1$x[,2]
pc3_scores = pca1$x[,3]
pc4_scores = pca1$x[,4]
pc1_sig = wilcox.test(pc1_scores[index_dess],pc1_scores[index_wild], exact = F)
pc1_sig
# W = 53893
#exact p value 
pc1_sig$p.value

pc2_sig = wilcox.test(pc2_scores[index_dess],pc2_scores[index_wild], exact = F)
pc2_sig
# W = 13066
#exact p value 
pc2_sig$p.value
pc3_sig =wilcox.test(pc3_scores[index_dess],pc3_scores[index_wild], exact = F)
pc3_sig
# W = 39203
#exact p value 
pc3_sig$p.value

#PC1, PC2 and PC3 are all significant. PC4 is not. 

wilcox.test(pc4_scores[index_dess],pc4_scores[index_wild], exact = F)
wilcox.test(pca1$x[index_dess,5],pca1$x[index_wild,5], exact = F)
wilcox.test(pca1$x[index_dess,6],pca1$x[index_wild,6], exact = F)
wilcox.test(pca1$x[index_dess,7],pca1$x[index_wild,7], exact = F)
wilcox.test(pca1$x[index_dess,8],pca1$x[index_wild,8], exact = F)

 
### CREATE VIOLIN PLOTS FOR PC SCORES ###
# Make a dataframe with PC scores and use. 
pca_scores_df = data.frame(abc_data$Use, pc1_scores,pc2_scores, pc3_scores  )
names(pca_scores_df)[1] = "Use"
names(pca_scores_df)[2] = "PC1 Score"
names(pca_scores_df)[3] = "PC2 Score"
names(pca_scores_df)[4] = "PC3 Score"

# PCA 1 scores

#pdf(file= "PCA1_scores_plot.pdf", width = 7, height = 4)

pc1scores = ggplot(pca_scores_df, aes(x= Use, y= pca_scores_df[,2], fill = Use))+ geom_violin(colour = 'black', alpha = 0.75,show.legend = FALSE )+ geom_boxplot(width=0.1, fill = "white") +theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(colour = "black", size=10))+labs(x = " ",y= names((pca_scores_df)[2]))+scale_fill_manual(values=c('#00AFBB','#FFDB6D'))+geom_signif(comparisons= list(c('Dessert','Wild')))+ scale_x_discrete(labels=c("Dessert" = "Cultivated", "Wild" = "Wild"))
print(pc1scores)
#dev.off()

# PCA 2 scores

#pdf(file= "PCA2_scores_plot.pdf", width = 7, height = 4)

pc2scores = ggplot(pca_scores_df, aes(x= Use, y= pca_scores_df[,3], fill = Use))+ geom_violin(colour = 'black', alpha = 0.75,show.legend = FALSE )+ geom_boxplot(width=0.1, fill = "white") +theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(colour = "black", size=10))+labs(x = " ", y= names((pca_scores_df)[3]))+scale_fill_manual(values=c('#00AFBB','#FFDB6D'))+geom_signif(comparisons= list(c('Dessert','Wild')))+ scale_x_discrete(labels=c("Dessert" = "Cultivated", "Wild" = "Wild"))
print(pc2scores)
#dev.off()

#PCA 3 scores
#pdf(file= "PCA3_scores_plot.pdf", width = 7, height = 4)

pc3scores = ggplot(pca_scores_df, aes(x= Use, y= pca_scores_df[,4], fill = Use))+ geom_violin(colour = 'black', alpha = 0.75,show.legend = FALSE )+ geom_boxplot(width=0.1, fill = "white") +theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(colour = "black", size=10))+labs(x = " ",y= names((pca_scores_df)[4]))+scale_fill_manual(values=c('#00AFBB','#FFDB6D'))+ geom_signif(comparisons= list(c('Dessert','Wild'))) + scale_x_discrete(labels=c("Dessert" = "Cultivated", "Wild" = "Wild"))
print(pc3scores)
#dev.off()

#Use ggarange to make the figure 
#Layout should be as follows: 
#PC1 v 2 -  PC1 v 3
#PC1, PC2, PC3


library("cowplot")
pdf(file= "figure_1_PCA.pdf", width = 11, height = 8.5)

figure_1_PCA_experiment = ggdraw() +draw_plot(pc1scores, x = 0, y = 0, width = 0.33, height = 0.5) +draw_plot(pc2scores, x = 0.33, y = 0, width = 0.33, height = 0.5) + draw_plot(pc3scores, x = 0.66, y = 0, width = 0.33, height = 0.5) + draw_plot(pc1pc2_plot, x = 0, y = 0.5, width = 0.5, height= 0.5) + draw_plot(pc1pc3_plot, x = 0.5,y=0.5, width=0.5,height = 0.5)
figure_1_PCA_experiment
dev.off()

#######################################################################
#DENSITY DISTRIBUTIONS FOR EACH PHENOTYPE#  #!#
#######################################################################
#In this section I will investigate the differences between dessert and wild apples for each of the phenotypes

setwd("~/Desktop/DALMSc/Biostats/abc_project")
abc_data = read.csv('~/Desktop/DALMSc/Biostats/abc_project/final_phenotype_table.csv')
install.packages(ggplot2)
library(ggfortify)
install.packages('devtools')
library(devtools)
install.packages('ggpubr')
library(ggpubr)
#create indicies
index_wild = which(abc_data$Use == 'Wild')
index_dess = which(abc_data$Use == 'Dessert')
#trim out uneeded meta data
abc_data = abc_data[7:17]

layout(matrix(1:10, 2, 5, byrow = T))

#The following loop will print out each phenotype comparison between wild and dessert apples USING DENSITY PLOTS
#pdf(file= "density_plots.pdf")
#par(mfrow = c(5, 2))

for (i in 2:ncol(abc_data)){
  gg=ggplot(abc_data, aes(x= abc_data[,i], fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(values=c('#00AFBB','#FFDB6D'))+scale_colour_manual(values=c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.85, 0.85),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( x=names((abc_data)[i]),y= 'Density')
  print(gg)
  
}
#dev.off()

#####
#the next section is purely for PDF printing purposes, for the DENSITY PLOTS 
#####


#PRECOCITY
#pdf(file= "Precocity_densityplot.pdf", width = 7, height = 4)
d1=ggplot(abc_data, aes(x= abc_data[,2], fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+scale_colour_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.85, 0.85),legend.title = element_blank(),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( x= ' Precocity (years)',y= 'Density')
print(d1)
#dev.off()

#FLOWERING DATE
#pdf(file= "floweringdate_densityplot.pdf", width = 7, height = 4)
d2=ggplot(abc_data, aes(x= abc_data[,3], fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+scale_colour_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.85, 0.85),legend.title = element_blank(),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( x='Flowering date (julian days)',y= 'Density')
print(d2)
#dev.off()

#HARVEST DATE
#pdf(file= "Harvestdate_densityplot.pdf", width = 7, height = 4)
d3=ggplot(abc_data, aes(x= abc_data[,4], fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+scale_colour_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.85, 0.85),legend.title = element_blank(),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( x='Harvest date (julian days)',y= 'Density')
print(d3)
#dev.off()

#FIRMNESS
#pdf(file= "Firmness_densityplot.pdf", width = 7, height = 4)
d4=ggplot(abc_data, aes(x= abc_data[,5], fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+scale_colour_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.85, 0.85),legend.title = element_blank(),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( x='Firmness (kg/cm2)',y= 'Density')
print(d4)
#dev.off()


#WEIGHT
#pdf(file= "Weight_densityplot.pdf", width = 7, height = 4)
d5=ggplot(abc_data, aes(x= abc_data[,6], fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+scale_colour_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.85, 0.85),legend.title = element_blank(),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( x='Weight (g)',y= 'Density')
print(d5)
#dev.off()

#ACIDITY
#pdf(file= "Acidity_densityplot.pdf", width = 7, height = 4)
d6=ggplot(abc_data, aes(x= abc_data[,7], fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+scale_colour_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.85, 0.85),legend.title = element_blank(),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( x='Acidity (malic acid mg/ml)',y= 'Density')
print(d6)
#dev.off()

#SOLUBLE SOLIDS
#pdf(file= "solublesolids_densityplot.pdf", width = 7, height = 4)
d7=ggplot(abc_data, aes(x= abc_data[,8], fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+scale_colour_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.85, 0.85),legend.title = element_blank(),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( x='Soluble solids (Brix)',y= 'Density')
print(d7)
#dev.off()

#PHENOLIC CONTENT
#pdf(file= "Phenoliccontent_densityplot.pdf", width = 7, height = 4)
d8=ggplot(abc_data, aes(x= abc_data[,9], fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+scale_colour_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.85, 0.85),legend.title = element_blank(),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( x= 'Phenolic content (umol GAE/g)',y= 'Density')
print(d8)
#dev.off()

#CHANGE IN ACIDITY
#pdf(file= "aciditycahnge_densityplot.pdf", width = 7, height = 4)
d9=ggplot(abc_data, aes(x= abc_data[,10], fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+scale_colour_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.85, 0.85),legend.title = element_blank(),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( x='% change in acidity during storage',y= 'Density')
print(d9)
#dev.off()

#CHANGE IN FIRMNESS
#pdf(file= "Changeinfirmness_densityplot.pdf", width = 7, height = 4)
d10=ggplot(abc_data, aes(x= abc_data[,11], fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+scale_colour_manual(labels = c('Cultivated','Wild'),values=c('#00AFBB','#FFDB6D'))+theme_bw() + theme(legend.position = c(0.85, 0.85),legend.title = element_blank(),panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( x='% change in firmness during storage',y= 'Density') 
print(d10)
#dev.off()

library(ggpubr)
density_figure = ggarrange(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,ncol = 2, nrow = 5)
pdf(file= "density_figure.pdf", width = 8.5, height = 11)
density_figure
dev.off()

test_df = data.frame(1,2,3,4,5)

#WILCOXON TESTS 
#loop for wilcox tests
for (i in 2:ncol(abc_data)){
  c = abc_data[,i]
  print(names(abc_data)[i])
  wt = (wilcox.test(c[index_dess],c[index_wild]))
  print(wt)
  print(wt$p.value)
  if ((wt$p.value) < 0.05){print('SIGNIFICANT')} 
  else {print('NOT SINGNIFICANT')}
  
}

#double check that this is the case 

ff = wilcox.test(abc_data$Firmness.loss.during.storage[index_wild],abc_data$Firmness.loss.during.storage[index_dess])


lol = wilcox.test(abc_data$Firmness[index_wild],abc_data$Firmness[index_dess])

#It would seem that my loop of tests worked. 

#Next, I would like to calculate the % difference in the medians for each of the significant phenotypes: 
#Precocity, flowering date, harvest date, weight, acidity, phenolic content #I need to take the median of the wild, subtract median of the cultivated and then divide by the median of the wild. 

# trim data 
sig_columns = c(1,2,3,4,6,7,9)
abc_data_percent_change = abc_data[,sig_columns]

#Precocity
precocity_wild_median = median(as.vector(abc_data$Precocity[index_wild]), na.rm = TRUE)
precocity_cult_median = median(as.vector(abc_data$Precocity[index_dess]), na.rm = TRUE)
precocity_change = (precocity_wild_median - precocity_cult_median)/precocity_wild_median
precocity_change
#Wild apples are 21% higher in precocity
index_wild = which(abc_data_percent_change$Use == "Wild")
index_dess = which(abc_data_percent_change$Use == "Dessert")
#the following is a loop to compare all of the phenotypes and present the change in a vector
change_vector = c()
for (i in 2:ncol(abc_data_percent_change)){
  wild_median = median(as.vector(abc_data_percent_change[,i][index_wild]), na.rm = TRUE)
  cult_median = median(as.vector(abc_data_percent_change[,i][index_dess]), na.rm = TRUE)
  change = (wild_median - cult_median)/wild_median
  change = change*100
  print(change)
  change_vector[i-1]= change
}
change_vector

# Cultivated apples are 21% less precocious than wild
# Cultivated apples flower 2% later - 3 days than wild (Flowering season from 143 - 161 days = 18 days) 3/18 = 16.6% later 
# Cultivated apples are harvested 5.9% later - 15 days than wild (Harvest season from 224 - 290 days = 66 days) 15/66 = 22.7% 
# Cultivated apples are 262% heavier than wild
# Cultivated apples are 43% less acidic than wild
# Cultivated apples are 68% lower in phenolic content than wild


# for flowering date and harvest date, # of days may be more understandable 
#Harvest date
wild_median_harv_date = median(as.vector(abc_data_percent_change$Harvest.Date[index_wild]), na.rm = TRUE)
cult_median_harv_date = median(as.vector(abc_data_percent_change$Harvest.Date[index_dess]),na.rm = TRUE)
harv_date_difference = wild_median_harv_date - cult_median_harv_date
harv_date_difference
#Flowering date
wild_median_flow_date = median(as.vector(abc_data_percent_change$Flowering.Date[index_wild]), na.rm = TRUE)
cult_median_flow_date = median(as.vector(abc_data_percent_change$Flowering.Date[index_dess]),na.rm = TRUE)
flow_date_difference = wild_median_flow_date - cult_median_flow_date
flow_date_difference

#Wild apples have a harvest date that is 15 days earlier than cultivated
#Wild apples have a flowering date that is 3 days earlier than cultivated


#######################################################################
#PHENOTYPIC DIFFERENCES AND RELEASE YEAR ANALYSIS#  #!#
#######################################################################
# I would like to make 10, 2-panel figures (all in one) that shows the phenotypic differences (or lack thereof) as well as a time line for release year of dessert apples side by side. It will have the violin plot on the left and the year of release plotted against phenotypic value on the right #In this next section I will plot the phenotypes against the year of release 

setwd("~/Desktop/DALMSc/Biostats/abc_project")
abc_data = read.csv('~/Desktop/DALMSc/Biostats/abc_project/final_phenotype_table.csv')

install.packages("ggpubr")
library("ggpubr")
index_wild = which(abc_data$Use == 'Wild')
index_dess = which(abc_data$Use == 'Dessert')

#Make a loop to graph them all 
#remove meta data, keep release year 
c_to_k = c(6,8:17)
abc_data_time = abc_data[c_to_k]

for (i in 2:ncol(abc_data_time)){
  
  gg=ggplot(abc_data_time, aes(x= Release.Year, y= abc_data_time[,i] ))+ geom_point(colour ='#00AFBB')+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))+labs( x='Release Year',y= names(abc_data_time)[i])+stat_cor(method="pearson",digits=3)
  print(gg)
}

# From prior knowledge from Watts, I want to change the scale on TPC graph to take a closer look at the trend 

ggplot(abc_data_time, aes(x= Release.Year, y= Phenolic.Content ))+ geom_point(colour ='#00AFBB')+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))+labs( x='Release Year',y= 'Phenolic Content') + ylim(0,15)+ xlim (1800,2018)+stat_cor(method="pearson",digits=3)


#PEARSONS CORRELATION TEST 

test = cor.test(abc_data_time$Release.Year, abc_data_time$Phenolic.Content, method = c("pearson"))
test$estimate

#PRINTING OUT SCATTER PLOTS VISUALIZED ABOVE 

#PRECOCITY
s1=ggplot(abc_data_time, aes(x= Release.Year, y= abc_data_time[,2] ))+ geom_point(colour ='#00AFBB')+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))+labs( x='Release Year',y= names(abc_data_time)[2])+stat_cor(method="pearson",digits=3)+xlim(1800,2015)+geom_smooth(method = "lm", colour = "black", se = FALSE)
print(s1)

#FLOWERING DATE 
s2=ggplot(abc_data_time, aes(x= Release.Year, y= abc_data_time[,3] ))+ geom_point(colour ='#00AFBB')+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))+labs( x='Release Year',y= 'Flowering Date')+stat_cor(method="pearson",digits=3)+xlim(1800,2015)+geom_smooth(method = "lm", colour = "black", se = FALSE)
print(s2)

# I scaled the X axis, however I am not sure if the datapoint that includes one apple from 1700 is considered still. 

#HARVEST DATE 
s3=ggplot(abc_data_time, aes(x= Release.Year, y= abc_data_time[,4] ))+ geom_point(colour ='#00AFBB')+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))+labs( x='Release Year',y= 'Harvest Date')+stat_cor(method="pearson",digits=3)+xlim(1800,2015)+geom_smooth(method = "lm", colour = "black", se = FALSE)
print(s3)

#FIRMNESS
s4=ggplot(abc_data_time, aes(x= Release.Year, y= abc_data_time[,5] ))+ geom_point(colour ='#00AFBB')+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))+labs( x='Release Year',y= names(abc_data_time)[5])+stat_cor(method="pearson",digits=3)+xlim(1800,2015)+geom_smooth(method = "lm", colour = "black", se = FALSE)
print(s4)

#WEIGHT
s5=ggplot(abc_data_time, aes(x= Release.Year, y= abc_data_time[,6] ))+ geom_point(colour ='#00AFBB')+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))+labs( x='Release Year',y= names(abc_data_time)[6])+stat_cor(method="pearson",digits=3)+xlim(1800,2015)+geom_smooth(method = "lm", colour = "black", se = FALSE)
print(s5)

#ACIDITY
s6=ggplot(abc_data_time, aes(x= Release.Year, y= abc_data_time[,7] ))+ geom_point(colour ='#00AFBB')+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))+labs( x='Release Year',y= names(abc_data_time)[7])+stat_cor(method="pearson",digits=3)+xlim(1800,2015)+geom_smooth(method = "lm", colour = "black", se = FALSE)
print(s6)

#SOLUBLE SOLIDS
s7=ggplot(abc_data_time, aes(x= Release.Year, y= abc_data_time[,8] ))+ geom_point(colour ='#00AFBB')+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))+labs( x='Release Year',y= 'Soluble Solids')+stat_cor(method="pearson",digits=3)+xlim(1800,2015)+geom_smooth(method = "lm", colour = "black", se = FALSE)
print(s7)

#PHENOLIC CONTENT
s8=ggplot(abc_data_time, aes(x= Release.Year, y= abc_data_time[,9] ))+ geom_point(colour ='#00AFBB')+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))+labs( x='Release Year',y= 'Phenolic Content')+stat_cor(method="pearson",digits=3)+geom_smooth(method = "lm", colour = "black", se = FALSE)+xlim(1800,2015)+scale_y_continuous(limits = c(0.3,19.3))
print(s8)
##
#Use +xlim(1800,2015) to scale the x axis if needed
##

#CHANGE IN ACIDITY DURING STORAGE 
s9=ggplot(abc_data_time, aes(x= Release.Year, y= abc_data_time[,10] ))+ geom_point(colour ='#00AFBB')+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))+xlim(1800,2015)+labs( x='Release Year',y= 'Change in acidity during storage')+stat_cor(method="pearson",digits=3)+geom_smooth(method = "lm", colour = "black", se = FALSE)
print(s9)

#CHANGE IN FIRMNESS DURING STORAGE
s10=ggplot(abc_data_time, aes(x= Release.Year, y= abc_data_time[,11] ))+ geom_point(colour ='#00AFBB')+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2), plot.title = element_text(size = 9))+xlim(1800,2015)+labs( x='Release Year',y= 'Change in firmness during storage')+stat_cor(method="pearson",digits=3)+geom_smooth(method = "lm", colour = "black", se = FALSE)
print(s10)




# in order to get the two types of plots to be visually comparable, I will need to make them have the same scales and I would like to make the desity plots have a smaller x axis 
#To do this I think I will need to do all of the density plots individually to make them match 
#included below are the plots for only the traits with significant trends 

#START WITH PRECOCITY 
#the following section is purely for printing purposes. 
abc_data_wild = abc_data[index_wild,]

g1=ggplot(abc_data_wild, aes(y=abc_data_wild$Precocity, fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(values=c('#FFDB6D'))+scale_colour_manual(values=c('#FFDB6D'))+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( y='Precocity',x= 'Density') + scale_y_continuous(limits = c(0,4))
print(g1)
#VIOLIN VARIATION
g1=ggplot(abc_data_wild, aes(x = Use, y= abc_data_wild$Precocity))+ geom_violin(colour = 'black', fill ='#FFDB68', alpha = 0.75 )+geom_boxplot(width = 0.1) + labs( x='Wild Apples',y= element_blank(), fill = "Apple Type")+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2)) + scale_y_continuous(limits = c(0,4))
print(g1)


#FLOWERING DATE 
g2=ggplot(abc_data_wild, aes(y=abc_data_wild$Flowering.Date, fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(values=c('#FFDB6D'))+scale_colour_manual(values=c('#FFDB6D'))+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( y='Flowering Date',x= 'Density') + scale_y_continuous(limits = c(143,161.5))
print(g2)
#VIOLIN VARIATION
#pdf(file= "flowering_date_violing.pdf", width = 3, height = 4)
g2=ggplot(abc_data_wild, aes(x = Use, y= abc_data_wild$Flowering.Date))+ geom_violin(colour = 'black', fill ='#FFDB68', alpha = 0.75 )+geom_boxplot(width = 0.1) + labs( x='Wild Apples',y= element_blank(), fill = "Apple Type")+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2)) + scale_y_continuous(limits = c(143,161.5))
print(g2)
#dev.off()

#HARVEST DATE
g3=ggplot(abc_data_wild, aes(x = Use, y= abc_data_wild$Harvest.Date))+ geom_violin(colour = 'black', fill ='#FFDB68', alpha = 0.75 )+geom_boxplot(width = 0.1) + labs( x='Wild Apples',y= element_blank(), fill = "Apple Type")+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2)) + scale_y_continuous(limits = c(225,290))
print(g3)

#FIRMNESS
g4=ggplot(abc_data_wild, aes(x = Use, y= abc_data_wild$Firmness))+ geom_violin(colour = 'black', fill ='#FFDB68', alpha = 0.75 )+geom_boxplot(width = 0.1) + labs( x='Wild Apples',y= element_blank(), fill = "Apple Type")+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2)) + scale_y_continuous(limits = c(2.5,14))
print(g4)

#WEIGHT 
g5=ggplot(abc_data_wild, aes(x = Use, y= abc_data_wild$Weight))+ geom_violin(colour = 'black', fill ='#FFDB68', alpha = 0.75 )+geom_boxplot(width = 0.1) + labs( x='Wild Apples',y= element_blank(), fill = "Apple Type")+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2)) + scale_y_continuous(limits = c(20,460))
print(g5)

#ACIDITY
g6=ggplot(abc_data_wild, aes(x = Use, y= abc_data_wild$Acidity))+ geom_violin(colour = 'black', fill ='#FFDB68', alpha = 0.75 )+geom_boxplot(width = 0.1) + labs( x='Wild Apples',y= element_blank(), fill = "Apple Type")+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2)) + scale_y_continuous(limits = c(2,26))
print(g6)


# SOLUBLE SOLIDS 
g7=ggplot(abc_data_wild, aes(y=abc_data_wild$Soluble.Solids, fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(values=c('#FFDB6D'))+scale_colour_manual(values=c('#FFDB6D'))+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( y='Soluble Solids',x= 'Density') + scale_y_continuous(limits = c(7,16.5))
print(g7)
#VIOLIN VARIATION
#pdf(file= "soluble_solids_violin.pdf", width = 3, height = 4)
g7=ggplot(abc_data_wild, aes(x = Use, y= abc_data_wild$Soluble.Solids))+ geom_violin(colour = 'black', fill ='#FFDB68', alpha = 0.75 )+geom_boxplot(width = 0.1) + labs( x='Wild Apples',y= element_blank(), fill = "Apple Type")+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2)) + scale_y_continuous(limits = c(7,16.5))
print(g7)
#dev.off()

#PHENOLIC CONTENT 
g8=ggplot(abc_data_wild, aes(y=abc_data_wild$Phenolic.Content, fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(values=c('#FFDB6D'))+scale_colour_manual(values=c('#FFDB6D'))+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( y='Phenolic Content',x= 'Density') + scale_y_continuous(limits = c(0.3,19.3))
print(g8)
#VIOLIN VARIATION
#pdf(file= "phenolic_content_violin.pdf", width = 3, height = 4)
g8=ggplot(abc_data_wild, aes(x = Use, y= abc_data_wild$Phenolic.Content))+ geom_violin(colour = 'black', fill ='#FFDB68', alpha = 0.75 )+geom_boxplot(width = 0.1) + labs( x='Wild Apples',y= element_blank(), fill = "Apple Type")+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2)) + ylim(0.3,19.3)
print(g8)
#dev.off()

#CHANGE IN ACIDITY DURING STORAGE

g9=ggplot(abc_data_wild, aes(x = Use, y= abc_data_wild$Change.in.acidity.during.storage))+ geom_violin(colour = 'black', fill ='#FFDB68', alpha = 0.75 )+geom_boxplot(width = 0.1) + labs( x='Wild Apples',y= element_blank(), fill = "Apple Type")+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2)) + scale_y_continuous(limits = c(-77,29))
print(g9)

#CHANGE IN FIRMNESS DURING STORAGE
g10=ggplot(abc_data_wild, aes(y=abc_data_wild$Change.in.firmness.during.storage, fill = Use, colour= Use))+ geom_density(alpha = 0.4)+scale_fill_manual(values=c('#FFDB6D'))+scale_colour_manual(values=c('#FFDB6D'))+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))+labs( y='Change in firmness during storage',x= 'Density') + scale_y_continuous(limits = c(-68,15))
print(g10)
#VIOLIN VARIATION
#pdf(file= "change_in_firmness_violin.pdf", width = 3, height = 4)
g10=ggplot(abc_data_wild, aes(x = Use, y= abc_data_wild$Change.in.firmness.during.storage))+ geom_violin(colour = 'black', fill ='#FFDB68', alpha = 0.75 )+geom_boxplot(width = 0.1) + labs( x='Wild Apples', y = element_blank(), fill = "Apple Type")+theme_bw() + theme(panel.border = element_blank(), axis.text=element_text(colour = "black", size=10), axis.title=element_text(size = 10, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.2))  + scale_y_continuous(limits = c(-68,15))
print(g10)
#dev.off()

#Using ggarrange to make the figure 
#For this figure I will just be looking at the significant scatter plots paired with thier violin plots. This will be: 
#Flowering Date -- s2,g2
#Soluble Solids -- s7,g3
#Phenolic Content -- s8,g4
#Change in firmness -- s10,g5

pdf(file= "figure_3_timeline.pdf", width = 7, height = 3)
figure_3_experiment = ggdraw() +draw_plot(s8,x=0,y=0,width = 0.66,height = 1) +draw_plot(g8,x=0.66,y=0,width = 0.33,height = 1)
figure_3_experiment
dev.off()

pdf(file= "Supp_figure_2.pdf", width = 8.5, height = 11)
ggarrange(s1,g1,s2,g2,s3,g3,s4,g4,s5,g5,ncol = 2, nrow = 5, common.legend = TRUE)
dev.off()

pdf(file= "Supp_figure_3.pdf", width = 8.5, height = 11)
ggarrange(s6,g6,s7,g7,s8,g8,s9,g9,s10,g10, ncol = 2, nrow = 5, common.legend = TRUE)
dev.off()


####################################################################
# % Change in Phenolics? #!#
####################################################################
setwd("~/Desktop/DALMSc/Biostats/abc_project")
abc_data = read.csv('~/Desktop/DALMSc/Biostats/abc_project/final_phenotype_table.csv')
#creating an index of rows that have release year data 
# index so that there are no NAs in release year or phenolics 
index_release = which(!is.na(abc_data$Release.Year))
abc_data_phenols = abc_data[index_release,]
index_phenol = which(!is.na(abc_data_phenols$Phenolic.Content))
abc_data_phenols = abc_data_phenols[index_phenol,]
abc_data_phenols$Release.Year = as.numeric(abc_data_phenols$Release.Year)
data_pre_1930 = c()
data_post_1930 = c()
#this loop will separate phenolic data based on the year of the released apple 
for (i in 1:nrow(abc_data_phenols))
  #print(abc_data_phenols$Release.Year[i])
  if ((abc_data_phenols$Release.Year[i]) <= 1930){data_pre_1930[i] = abc_data_phenols$Phenolic.Content[i]}else{data_post_1930[i] = abc_data_phenols$Phenolic.Content[i]}
    
data_post_1930 = data_post_1930[!is.na(data_post_1930)]
data_pre_1930 = data_pre_1930[!is.na(data_pre_1930)]

sum(data_post_1930)/118
#3.58
sum(data_pre_1930)/41
#4.76

abc_data = read.csv('~/Desktop/DALMSc/Biostats/abc_project/final_phenotype_table.csv')
# this will index for only wild apples with phenolic data and then calculate the median phenolics 

wild_abc_data = abc_data[index_wild,]
wild_phenolic_index = which(!is.na(wild_abc_data$Phenolic.Content))
wild_abc_data = wild_abc_data[wild_phenolic_index,]
sum(wild_abc_data$Phenolic.Content)/9
#10.36






