#### Testing the MFD-es ####
## Author: Cristian Rodriguez (c.rodriguez@uci.edu)

### I. Preparing libraries ###

pacman::p_load_gh("kbenoit/quanteda.dictionaries", dependencies = TRUE)
library(reshape)
library(quanteda)
library(ggplot2)
library(readtext)
library(lubridate)
library(tidyverse)

### II. Getting the texts: 
## CM - Communist Manifesto - Karl Marx (Chapter 1 - Bourgeois and Proletarians) ##
## HV - Encyclical "Humanae Vitae" - Pope VI (complete) ##
## UN - Declaration of Human Rights - United Nations (complete) ##
## TEU - Treatise of the European Union (complete) ##
## ICL - Declaration of Principles International Communist League (complete) ##
## DL - Message "Compassion and the Individual" - 14th Dalai Lama ##

Tests <- readtext(paste0("*test/*.txt"),
                        docvarsfrom = "filenames", # indicates that the meta information is coming from the file names
                        dvsep = "_", # indicates the separator that separates the information
                        docvarnames = c("document","language")) # indicates the name of the meta variables
## Transforming the texts..

# ...into corpora
ES_Corpus <- corpus(Tests[c(2,4,6,8,10,12),])
EN_Corpus <- corpus(Tests[c(1,3,5,7,9,11),])

# ... into document-feature-matrices
ES_Mat<-dfm(ES_Corpus, remove = stopwords("es"))
EN_Mat<-dfm(EN_Corpus, remove = stopwords("en"))

#... into data frames
ES_df<-convert(ES_Mat, "data.frame")
EN_df<-convert(EN_Mat, "data.frame")

### III. Analyses by language
dict_analysis_ES <- liwcalike(ES_Corpus, dictionary = dict_ES)
dict_analysis_EN <- liwcalike(EN_Corpus, dictionary = dict_EN)

### Matching the analyses by document
data_CM<-rbind(dict_analysis_EN[1,],dict_analysis_ES[1,])
data_DL<-rbind(dict_analysis_EN[2,],dict_analysis_ES[2,])
data_HU<-rbind(dict_analysis_EN[3,],dict_analysis_ES[3,])
data_ICL<-rbind(dict_analysis_EN[4,],dict_analysis_ES[4,])
data_TEU<-rbind(dict_analysis_EN[5,],dict_analysis_ES[5,])
data_UN<-rbind(dict_analysis_EN[6,],dict_analysis_ES[6,])


lang<-rep(c("EN", "ES"), 10)
library(ggplot2)
library(reshape)

## IV. Comparative graphs 

## Communist Manifesto
meltedCM<-melt(data_CM[,c(1, 7:16)], id.vars="docname")
meltedCM<-cbind(meltedCM, lang)
ggplot(meltedCM, aes(x = variable, y = value,  fill = lang)) + 
  geom_col(position = "dodge") +
  labs(x = "Moral Foundation", fill = "Language", title = "Moral Foundations - Communist Manifesto, chp. 1")

## International Communist League
meltedICL<-melt(data_ICL[,c(1, 7:16)], id.vars="docname")
meltedICL<-cbind(meltedICL, lang)
ggplot(meltedICL, aes(x = variable, y = value,  fill = lang)) + 
  geom_col(position = "dodge") +
  labs(x = "Moral Foundation", fill = "Language", title = "Moral Foundations - Declaration of the International Communist League")

##Humanae Vitae
meltedHV<-melt(data_HV[,c(1, 7:16)], id.vars="docname")
meltedHV<-cbind(meltedHV, lang)
ggplot(meltedHV, aes(x = variable, y = value,  fill = lang)) + 
  geom_col(position = "dodge") +
  labs(x = "Moral Foundation", fill = "Language", title = "Moral Foundations - Humanae Vitae (Paul VI)")

## Compassion Message
meltedDL<-melt(data_DL[,c(1, 7:16)], id.vars="docname")
meltedDL<-cbind(meltedDL, lang)
ggplot(meltedDL, aes(x = variable, y = value,  fill = lang)) + 
  geom_col(position = "dodge") +
  labs(x = "Moral Foundation", fill = "Language", title = "Moral Foundations - Compassion Message (14th Dalai Lama)")

## Human Rights declaration
meltedUN<-melt(data_UN[,c(1, 7:16)], id.vars="docname")
meltedUN<-cbind(meltedUN, lang)
ggplot(meltedUN, aes(x = variable, y = value,  fill = lang)) + 
  geom_col(position = "dodge") +
  labs(x = "Moral Foundation", fill = "Language", title = "Moral Foundations - Human Rights Declaration")

## Treatise of the European Union
meltedTEU<-melt(data_TEU[,c(1, 7:16)], id.vars="docname")
meltedTEU<-cbind(meltedTEU, lang)
ggplot(meltedTEU, aes(x = variable, y = value,  fill = lang)) + 
  geom_col(position = "dodge") +
  labs(x = "Moral Foundation", fill = "Language", title = "Moral Foundations - Treatise of the European Union")


meltAll<-rbind(meltedCM, meltedICL, meltedDL, meltedHV, meltedTEU, meltedUN)
type<-vector(mode = "character", length = nrow(meltAll))
meltAll<-cbind(meltAll, type)
meltAll[1:20,1]<-"Communist Manifesto"
meltAll[21:40,1]<- "Principles International Communist Leage"
meltAll[41:60,1]<- "Compassion (Dalai Lama)"
meltAll[61:80,1]<- "Humanae Vitae (Pope Paul VI)"
meltAll[81:100,1]<- "Treatise of Maastrich (EU)"
meltAll[101:120,1]<- "Human Rights (UN)"
meltAll[,5]<-factor(meltAll[,5], levels = c("Communists","Religious Leaders","International Declarations"))
meltAll[1:40, 5]<- "Communists"
meltAll[41:80, 5]<- "Religious Leaders"
meltAll[81:120, 5]<- "International Declarations"


## Plot All texts ##

ggplot(meltAll, aes(x = variable, y = value,  fill = lang)) + 
  geom_col(position = "dodge")

## Plot by type of text
ggplot(meltAll[which(meltAll$type == "Communists"),], aes(x = variable, y = value,  fill = lang)) + 
  geom_col(position = "dodge")

ggplot(meltAll[which(meltAll$type == "Religious Leaders"),], aes(x = variable, y = value,  fill = lang)) + 
  geom_col(position = "dodge")

ggplot(meltAll[which(meltAll$type == "International Declarations"),], aes(x = variable, y = value,  fill = lang)) + 
  geom_col(position = "dodge")
