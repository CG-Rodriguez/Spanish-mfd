library(openxlsx)
library(gutenbergr)
library(tidyverse)
library(reshape)
library(quanteda)
library(quanteda.dictionaries)
library(readtext)
library(lubridate)
options(stringsAsFactors = F)

# find authors who were published in both English and Spanish
en <- gutenberg_metadata %>% filter(language=="en")
es <- gutenberg_metadata %>% filter(language=="es") 
en %>% distinct(gutenberg_author_id) -> en_dist
es_dist <- es %>% distinct(gutenberg_author_id)
en_dist$gutenberg_author_id[en_dist$gutenberg_author_id %in% es_dist$gutenberg_author_id] -> authors_d

en_es_authors <- gutenberg_metadata %>% 
  filter(gutenberg_author_id %in% authors_d) %>%
  filter(!is.na(gutenberg_author_id )) %>% 
  filter(language == "en" | language == "es") %>%
  filter(author != "Anonymous") %>%
  filter(has_text) %>%
  arrange(author)

#Save list in Excel
write.xlsx(en_es_authors, "autores.xlsx")
#Set dictionaries
dict_Test <- dictionary(file = "dicts/dict_es_test.dic",
                        format = "LIWC")
dict_EN <- dictionary(file = "dicts/mfd2.0.dic",
                      format = "LIWC")

## Authors and books both in Spanish and English in Gutenberg
Dataset<-read.xlsx("autores.xlsx", sheet = "Sheet4")

#Get the raw text from Gutenberg for all books
allTexts<-vector(mode = "character", length = nrow(Dataset))
for (i in 1:nrow(Dataset)) {
  
  Gut <- gutenberg_download(Dataset[i,2])

  Encoding(Gut$text) <- 'latin1'
  Vector<-paste(Gut$text, collapse=" ")
  allTexts[i]<-Vector
  }

## Appending the texts to the metadata
allTexts<-cbind(Dataset, allTexts)
names(allTexts)[6]<-"text"
# Creating the corpus
allCorpora<-corpus(allTexts, docid_field = "ID", text_field = "text")
#Analyzing the corpora, by language
Analyses_EN<-liwcalike(allCorpora[which(Dataset$Language == "en")], dict_EN)
Analyses_ES<-liwcalike(allCorpora[which(Dataset$Language == "es")], dict_Test)

#Restructuring the data
Analyses_ES<-cbind(Analyses_ES, "ES")
Analyses_EN<-cbind(Analyses_EN, "EN")
names(Analyses_ES)[29]<- "lang"
names(Analyses_EN)[29]<- "lang"
allAnalyses <- as.data.frame(rbind(Analyses_EN[,c(1,7:16, 29)], Analyses_ES[,c(1,7:16, 29)]))
allAnalyses<-arrange(allAnalyses,  docname)

allAnalyses[,1]<-gsub("\\.1", "", allAnalyses[,1])

### CALCULATING CORRELATIONS
##Wide format by book

wideall<-reshape(allAnalyses, idvar = "docname", 
                 timevar = "lang", direction = "wide")

allCorrs<-data.frame(
  T.statistic = vector(mode = "numeric", length = 10), 
  p.value = vector(mode = "numeric", length = 10), 
  R = vector(mode = "numeric", length = 10),
  LL = vector(mode = "numeric", length = 10),
  UL =  vector(mode = "numeric", length = 10),
  row.names = gsub("\\.EN","",names(wideall[,2:11])))


for (i in 2:11) {
  PH<-cor.test(wideall[,i], wideall[,i+10])
  round(as.numeric(PH$statistic), 3)->allCorrs[i-1,1]
  round(as.numeric(PH$p.value), 3)->allCorrs[i-1,2]
  round(as.numeric(PH$estimate), 3)->allCorrs[i-1,3]
  round(as.numeric(PH$conf.int), 3)->allCorrs[i-1,4:5]
}  

## plot correlations
allCorrs %>%
  mutate(sig = if_else(p.value < .05, "Significant", "Non-significant")) %>%
  ggplot(  aes(x = row.names(allCorrs), y = R)) +
  geom_col(aes(fill = as.factor(sig))) +
  ylim(c(-.25,1)) +
  geom_errorbar(aes(ymin = LL, ymax = UL))+
  labs(fill = "Significant Correlation", x = "Moral Foundation", 
       y = "Pearson's R", title = "Correlations MFD-en vs. MFD-es",
       subtitle = "Analyses of 42 books in both languages from Project Gutenberg")

## Correlations for both dimensions of each foundation

fiveDim<-data.frame(docname = wideall$docname,
                    Care.en = rowMeans(wideall[,2:3]), 
                    Fairness.en = rowMeans(wideall[,4:5]), 
                    Loyalty.en = rowMeans(wideall[,6:7]),
                    Authority.en = rowMeans(wideall[,8:9]),
                    Sanctity.en = rowMeans(wideall[,10:11]),
                    Care.es = rowMeans(wideall[,12:13]), 
                    Fairness.es = rowMeans(wideall[,14:15]), 
                    Loyalty.es = rowMeans(wideall[,16:17]),
                    Authority.es = rowMeans(wideall[,18:19]),
                    Sanctity.es = rowMeans(wideall[,2:3])
)


allCorrs5<-data.frame(
  T.statistic = vector(mode = "numeric", length = 5), 
  p.value = vector(mode = "numeric", length = 5), 
  R = vector(mode = "numeric", length = 5),
  LL = vector(mode = "numeric", length = 5),
  UL =  vector(mode = "numeric", length = 5),
  row.names = gsub("\\.en","",names(fiveDim[,2:6])))


for (i in 2:6) {
  PH<-cor.test(fiveDim[,i], fiveDim[,i+  5])
  round(as.numeric(PH$statistic), 3)->allCorrs5[i-1,1]
  round(as.numeric(PH$p.value), 3)->allCorrs5[i-1,2]
  round(as.numeric(PH$estimate), 3)->allCorrs5[i-1,3]
  round(as.numeric(PH$conf.int), 3)->allCorrs5[i-1,4:5]
}  

allCorrs5 %>%
  mutate(sig = if_else(p.value < .05, "Significant", "Non-significant")) %>%
  ggplot(  aes(x = row.names(allCorrs5), y = R)) +
  geom_col(aes(fill = as.factor(sig))) +
  ylim(c(-.15,1)) +
  geom_errorbar(aes(ymin = LL, ymax = UL))+
  labs(fill = "Significant Correlation", x = "Moral Foundation", 
       y = "Pearson's R", title = "Correlations MFD-en vs. MFD-es",
       subtitle = "Analyses of 42 books in both languages from Project Gutenberg")

Corrs<-list("All" = allCorrs, "Five Dimensions" = allCorrs5)
write.xlsx(Corrs, "Correlations_Gutenberg.xlsx", rowNames = TRUE)


### PROPORTIONS
##Plotting the proportions overall
meltAll<-melt(allAnalyses, measure.vars = 3:12, id.vars = c(1,13))
ggplot(meltAll, aes(x = variable, y = value,  fill = lang)) + 
  stat_summary(fun.y = mean, geom = "col", position = position_dodge(width = .95)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = .95), width = .5)

meltFive<-melt(fiveDim, measure.vars = 2:11, id.vars = 1)
meltFive %>%
  mutate(lang = c(rep("EN", 210), rep("ES", 210))) %>%
  ggplot(aes(x = variable, y = value,  fill = lang)) + 
  stat_summary(fun.y = mean, geom = "col", position = position_dodge(width = .95)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = .95), width = .5)

