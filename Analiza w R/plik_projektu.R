#NIE ODWOLUJEMY SIE DO OBIEKTOW, KTORE NIE SA WIDOCZNE Z POZIOMU TEGO SKRYPTU
#KW
#https://www.youtube.com/watch?v=U-hS2zoRPOY

#mann - witney
#https://www.youtube.com/watch?v=2sZp51etL3c

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
df$joined_eu <- as.factor(df$joined_eu)
#zmiennosc
#cv <- function(x){(sd(x)/mean(x))*100}
#sapply(as.list(df[,-1]), cv)

#corrplot::corrplot(
 # cor(df[,-c(1,9,10)]), method = "number")
##DO ZMIENNOSCI SI EXCELA LICZY JAK NA ADR

#cor(DaneAnaliza)
#korelacje<-as.numeric(cor(DaneAnaliza))
#korelacje<-korelacje %>% as.data.frame()
#korelacje %>% ggplot(aes(x=.,fill=2))+geom_density()+labs(x="Korelacje",y="Gęstość")
#corrplot(cor(DaneAnaliza))

##geom col
#co to jest? - obiekt Rcmd nie jest widoczny z poziomu skryptu
#Rcmd %>%   group_by(joined_EU)  %>% summarise(liczba_krajów=n()) %>% ggplot(aes(x=joined_EU,y=liczba_krajów,la,label=liczba_krajów))+geom_col()

to_geom_col <- df %>% select(joined_eu) %>% group_by(joined_eu) %>%
  summarise(`liczba krajów` = n())

ggplot(to_geom_col,
       aes(x = `liczba krajów`, y = joined_eu)) +
  geom_col() +
  theme_minimal()+
  theme(axis.text.x  = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.title.y = element_blank()) +
  geom_label(aes(label = `liczba krajów`),size = 6 )

        