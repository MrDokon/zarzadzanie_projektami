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


#elo

#damian - elo

#elo mordelo

##Siema co tamdasdasdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
##DDFAASFFAFAFSasffssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
12+4
is.numeric(df)
asfassdddddddddddddddd;
scale(df)
##affaaaaaaaaaaaaaaaaaaaaaaaaaaaa
1+1
1+2
