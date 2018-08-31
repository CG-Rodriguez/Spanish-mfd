#### Testing the MFD-es ####
## Author: Cristian Rodriguez (c.rodriguez@uci.edu)

### I. Preparing libraries ###

pacman::p_load("tidyverse", "quanteda", "lubridate", "readtext")
pacman::p_load_gh("kbenoit/quanteda.dictionaries", dependencies = TRUE)

library(quanteda)
library(ggplot2)
library(readtext)
library(lubridate)
library(tidyverse)

### II. Getting the texts: 
## CM - Communist Manifesto - Karl Marx (Chapter 1 - Bourgeois and Proletarians) ##
## HV - Encyclical "Humanae Vitae" - Pope VI (complete) ##
## UN - Declaration of Human Rights - United Nations (complete) ##

Tests <- readtext(paste0("test/*.txt"),
                        docvarsfrom = "filenames", # indicates that the meta information is coming from the file names
                        dvsep = "_", # indicates the separator that separates the information
                        docvarnames = c("source","language")) # indicates the name of the meta variables
## Transforming the texts..

# ...into corpora
ES_Corpus <- corpus(Tests[c(2,4,6),])
EN_Corpus <- corpus(Tests[c(1,3,5),])

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
data_HV<-rbind(dict_analysis_EN[2,],dict_analysis_ES[2,])
data_UN<-rbind(dict_analysis_EN[3,],dict_analysis_ES[3,])
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

##Humanae Vitae
meltedHV<-melt(data_HV[,c(1, 7:16)], id.vars="docname")
meltedHV<-cbind(meltedHV, lang)
ggplot(meltedHV, aes(x = variable, y = value,  fill = lang)) + 
  geom_col(position = "dodge") +
  labs(x = "Moral Foundation", fill = "Language", title = "Moral Foundations - Humanae Vitae (Paul VI)")

## Human Rights declaration
meltedUN<-melt(data_UN[,c(1, 7:16)], id.vars="docname")
meltedUN<-cbind(meltedUN, lang)
ggplot(meltedUN, aes(x = variable, y = value,  fill = lang)) + 
  geom_col(position = "dodge") +
  labs(x = "Moral Foundation", fill = "Language", title = "Moral Foundations - Human Rights Declaration")
