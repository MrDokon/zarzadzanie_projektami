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

df %>% glimpse()
#PCA zmienić - gdp_pc w piewszym wymiarze
#wczytywanie danych i poprawa danych
df <- readxl::read_excel(path = "./Data.xlsx")
df <- df %>% janitor::clean_names()
df <- df %>% select(-working_pop_pct)
df$joined_eu <- as.factor(df$joined_eu)
df$sea_access <- as.factor(df$sea_access)
df$joined_eu <- as.factor(df$joined_eu)
df$is_euro_currency <- as.factor(df$is_euro_currency)
df$nuclear_electricity <- as.factor(df$nuclear_electricity)

df <- df %>% as.data.frame()
rownames(df) <- df$country
#       Właściwa Regresja #
#--------------------#
str(Rcmd)
Rcmd %>% mutate(country=1:nrow(Rcmd))
Regresja_dane<-Rcmd %>% select(-joined_EU,-sea_access,-nuclear_electricity,-is_euro_currency,-country)
macierz_korelacji<-cor(Regresja_dane)
library(PerformanceAnalytics)
chart.Correlation(macierz_korelacji,histogram = TRUE, pch=20)
colnames(Regresja_dane)

macierz_korelacji





Zadanie<-TaskRegr$new(id="efekt_7",backend=df[,-1], target="gdp_pc")
learner_regresja<-lrn("regr.lm")
learner_regresja$train(Zadanie)
summary(learner_regresja$model)


lrn.rf <- lrn("regr.ranger", id = "rf", importance="impurity")


lrn.rf$train(Zadanie)

imp <- lrn.rf$importance()

barplot(imp)

df_rf <- df %>% select(-c(sea_access,nuclear_electricity,is_euro_currency,phycisians_per_1000))

df_rf %>% glimpse()

task.rf <- TaskRegr$new(id = "RF2",backend = df_rf[,-1], target = "gdp_pc" )
task.rf2 <- TaskRegr$new(id = "RF3",backend = df_rf[,-1], target = "gdp_pc" )
learner_regresja$train(task = task.rf2)
learner_regresja$model %>% summary()


df_rf %>% select(-country,-joined_eu)->dane
colnames(dane)
ao.hellwig(7,dane,"pearson")
str(dane)
## 2,3,5,6
DaneAnaliza<-dane %>% select(gdp_pc,high_tech_trade_pc,r_d_gdp_pct,use_cloud_pct,weeknd_work_pct)
summary(lm(gdp_pc~.,DaneAnaliza))

##------------------------------------------------------------------------------##




