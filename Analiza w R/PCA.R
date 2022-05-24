library(dplyr)
library(corrplot)
library(psych)
library(FactoMineR)
library(factoextra)


#wczytywanie danych i poprawa danych
df <- readxl::read_excel(path = "./Data.xlsx")
df <- df %>% janitor::clean_names()
df <- df %>% select(-working_pop_pct)
df$joined_eu <- as.factor(df$joined_eu)

#sprawdzenie kolumn
str(df)
df %>% glimpse()
df %>% as_tibble()

##Sprawdzenie korelaji i ich istotność
df_cor <- df %>% select(where(is.double))
df_cor<-df_cor %>% select(c(-sea_access,-nuclear_electricity,-is_euro_currency))

corrplot::corrplot(cor(df_cor),
                   method = "color",
                   type="lower",
                   addCoef.col = "black") 

##Test sferyczności Barletta
cortest.bartlett(cor(df_cor), n = nrow(df_cor)) #ist gut

#Kryterium KMO
KMO(cor(df_cor))

##Standaryzacja zmiennych
df_standarized <-  scale(df_cor) %>% as.data.frame()
rownames(df_standarized) <- df$country

##Analiza PCA
pca_object <- PCA(df_standarized, graph = F, ncp = 8)
pca_object

#Zbadanie wymiarów
pca_object$var$coord

#Pomiar rozbieżności pomiędzy modelem a rzeczywistymi danymi - wykres
fviz_screeplot(pca_object, addlabels = TRUE)

#PCA - 2 wymiary
pca_object <- PCA(df_standarized, graph = F, ncp = 2)

#Diagram ordynacyjny ze względu na udział w PCA - biplot
fviz_pca_var(pca_object,
             repel = TRUE,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             labelsize = 5, col.circle = "grey40")

#Wykres obserwacji indywidualnych ze względu na udział w PCA - wykres
fviz_pca_ind(pca_object,
             repel = TRUE,
             pointsize = "contrib",
             pointshape = 21, fill = "#4b86b4", col.point = "white")

#do analizy wymiarów
principal(df_cor,2,rotate="none", nfactors = 8)
principal(df_cor,2,rotate="none", nfactors = 2)
