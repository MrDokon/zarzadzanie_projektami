library(dplyr)
#wczytywanie danych
df <- readxl::read_excel(path = "./Data.xlsx")
getwd()
#poprawa kolumn
df <- df %>% janitor::clean_names()

#sprawdzenie kolumn
df %>% glimpse()
df %>% as_tibble()

#zmiennosc
cv <- function(x){(sd(x)/mean(x))*100}
sapply(as.list(df[,-1]), cv)

#elo

#damian - elo

#elo mordelo

##Siema co tam
12+4
is.numeric(df)
scale(df)
