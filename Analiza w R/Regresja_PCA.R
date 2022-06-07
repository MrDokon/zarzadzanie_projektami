library(dplyr)
library(corrplot)
library(psych)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(GGally)
library(tidyr)
library(fpc)
library(cowplot)


#wczytywanie danych i poprawa danych
df <- readxl::read_excel(path = "./Data.xlsx")
df <- df %>% janitor::clean_names()
df <- df %>% select(-working_pop_pct)
df$joined_eu <- as.factor(df$joined_eu)

#sprawdzenie kolumn
str(df)

#dalsza manipulacja danych
df_cor <- df %>% select(where(is.double))
df_cor<-df_cor %>% select(c(-sea_access,-nuclear_electricity,-is_euro_currency))

##Standaryzacja zmiennych
df_standarized <-  scale(df_cor[,-1]) %>% as.data.frame()
rownames(df_standarized) <- df$country

#PCA - 2 wymiary
pca_object <- PCA(df_standarized, graph = F, ncp = 2)

#do analizy wymiarów
PCA_object1 <- principal(df_standarized,2,rotate="none", nfactors = 2)
PCA_df <-  PCA_object1$scores %>% as.data.frame()

#ramka do regresji z PCA
PCA_Regr <-  PCA_df %>% cbind.data.frame(.,gdp_pc = df$gdp_pc)

############## budowa modelu regresji liniowej
regr_object <- lm(gdp_pc ~ ., data = PCA_Regr)
regr_object %>% summary()

