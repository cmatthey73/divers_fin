library(dplyr)
library(igraph)
library(entropy)
library(openxlsx)
library(lubridate)
library(stringr)
library(reshape2)
library(ggplot2)

#### essai selon doc SouthAfrica

rm(list=ls())
memory.size()
gc() # libération mémoire

options(stringsAsFactors = F,scipen=10) # pour permettre travail avec textes

setwd("W:/04_EM/01_Christophe/00_Méthodes/Divers/")

## Importation données

source<-"./data_kmap.xlsx"

lst_sh<-getSheetNames(file = source)

data_q<-lapply(lst_sh, function(x){
  tmp<-read.xlsx(xlsxFile = source, sheet = x, check.names = F)
  if (x=="PIB"){tmp$Jour<-NA ; tmp$Mois<-NA 
                tmp$Trim<-as.numeric(unlist(lapply(str_split(tmp$Trim, "-Q"), paste, collapse="." )))} 
  else if (x=="MM") {tmp$Jour<-as.Date.character(paste(tmp$Mois, "01", sep="-"), format="%Y-%m-%d") 
                     tmp$Trim<-quarter(tmp$Jour, with_year = T)} 
  else {tmp$Jour<-as.Date(tmp$Jour, format="%Y-%m-%d")
        tmp$Trim<-quarter(tmp$Jour, with_year = T)}
  tmp[,-grep("Jour|Mois", colnames(tmp))] %>% group_by(Trim) %>% summarise_all(.funs = function(x) mean(x, na.rm=T))
})

names(data_q)<-lst_sh

data_q_all<-data_q[[1]]
for (i in 2:length(data_q)){
  data_q_all<-merge(data_q_all, data_q[[i]], by="Trim")
}

data_q_all<-data_q_all[complete.cases(data_q_all),]
data_q_all_var<-data_q_all
data_q_all_var[,-1]<-lapply(data_q_all[,-1], function(x) {x/lag(x)-1})
data_q_all_var<-data_q_all_var[-1,]

## Calculs

# corrélations (variations)

cor_all<-as.data.frame(cor(data_q_all_var[,-1]))
for (i in 1:ncol(cor_all)){
  for (j in 1:ncol(cor_all)){
    if (i>=j) {cor_all[i,j]<-NA}
  }
}

summary(unlist(cor_all))

# entropy (variations)
# For entropy and mutual information calculations, the statistical package R is used 
# with the natural logarithm and bin size based on the square root of the number of samples (n=11)[4,21].
nbin<-sqrt(nrow(data_q_all_var))
var_discr<-lapply(data_q_all_var[,-1], function(x) {discretize(x , numBins = nbin)})
lapply(var_discr, entropy.empirical)

# mutual info (variations)
var_discr2d<-list() 
mi<-list()
n<-1
for (i in 2:(ncol(data_q_all)-1)){
  for (j in (i+1):(ncol(data_q_all))){
    # print(i)
    # print(j)
    var_discr2d[[n]]<-discretize2d(data_q_all_var[[i]], data_q_all_var[[j]], numBins1 = nbin, numBins2 = nbin )
    mi[[n]]<-c(colnames(data_q_all_var)[i], colnames(data_q_all_var)[j], mi.empirical(var_discr2d[[n]]))
    n<-n+1
  }
}  

mi<-as.data.frame(do.call(rbind, mi))
colnames(mi)<-c("edge1","edge2","weight")
mi$weight[mi$weight<0.2]<-0

a<-graph.data.frame(mi, directed=F)
a<-simplify(a, remove.multiple = F, remove.loops = T)
E(a)$width<-as.numeric(mi$weight)*20
plot(a, edge.arrow.size=0.4)

dens<-length(mi$edge1[mi$weight!=0])/(length(unique(mi$edge1))*(length(unique(mi$edge1))-1))

