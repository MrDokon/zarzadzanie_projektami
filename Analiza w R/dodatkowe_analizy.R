#pakiety
library(tidyverse)
library(dlookr) #funkcja normality - sprawdzenie normalnosci
library(ggstatsplot) #funkcja ggbetweenstats
library(tidyquant)
#wczytywanie danych
df <- readxl::read_excel(path = "./Data.xlsx")

#poprawa kolumn
df <- df %>% janitor::clean_names()

#sprawdzenie kolumn
df %>% glimpse()
df %>% as_tibble()

#wyrzucenie zmiennej quasi stalej
df <- df %>%
  select(-working_pop_pct)

#zamiana zmiennych kategorycznych na factory
df <- df %>%
  mutate_if(colnames(df)
            %in%
              c("sea_access", "joined_eu",
                "is_euro_currency", "nuclear_electricity"),
            as.factor)

#porownuje nuclear electricity pod wzgledem GPD_pc

#p < 0.05 - data is not-normally distributed
df %>% 
  group_by(nuclear_electricity) %>% 
  normality(gdp_pc)

#rozkład nie jest normally distributed

#MANN-WHITNEY test

ggbetweenstats(
  data = df,
  x = nuclear_electricity, #zmienna kategoryczna
  y = gdp_pc, #zmienna numeryczna
  type = "nonparametric" 
)
#p >= 0.1 - Absence of evidence against the null hypothesis in favour of the alternative, grupy są podobne, dwie grupy maja rowna srednia. 
effectsize::interpret_rank_biserial(-0.10)

#czy panstwa posiadajace elektrownie jadrawa roznia sie od panstw, ktore nie posiadaja elektrowni atomowych ze wzgledu na gdp_pc. 
df %>% 
  select(sea_access, is_euro_currency, nuclear_electricity) %>% 
  count(sea_access, is_euro_currency, nuclear_electricity) %>% 
  pivot_longer(sea_access:nuclear_electricity) %>% 
  mutate_if(is.double, as.factor) %>% 
  ggplot(aes(value, n, fill = value)) +
  geom_col(show.legend = F)+
  facet_wrap(vars(name)) +
  scale_y_continuous(limits = c(0,25)) +
  theme_ggstatsplot() +
  scale_fill_grey() +
  ylab("liczba państw")+
  xlab("etykieta")
  

