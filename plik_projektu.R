library(dplyr)
#wczytywanie danych
df <- readxl::read_excel(path = "./Data.xlsx")

#poprawa kolumn
df <- df %>% janitor::clean_names()
str(df)
#sprawdzenie kolumn
df %>% glimpse()
df %>% as_tibble()

df <- df %>% select(-working_pop_pct)
#zmiennosc
cv <- function(x){(sd(x)/mean(x))*100}
sapply(as.list(df[,-1]), cv)

corrplot::corrplot(
  cor(df[,-c(1,9,10)]), method = "number")
##DO ZMIENNOSCI SI EXCELA LICZY JAK NA ADR

cor(DaneAnaliza)
korelacje<-as.numeric(cor(DaneAnaliza))
korelacje<-korelacje %>% as.data.frame()
korelacje %>% ggplot(aes(x=.,fill=2))+geom_density()+labs(x="Korelacje",y="Gęstość")
corrplot(cor(DaneAnaliza))

##geom col
Rcmd %>%   group_by(joined_EU)  %>% summarise(liczba_krajów=n()) %>% ggplot(aes(x=joined_EU,y=liczba_krajów,la,label=liczba_krajów))+geom_col()