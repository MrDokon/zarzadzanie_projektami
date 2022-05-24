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
df$sea_access <- as.factor(df$sea_access)
df$joined_eu <- as.factor(df$joined_eu)
df$is_euro_currency <- as.factor(df$is_euro_currency)
df$nuclear_electricity <- as.factor(df$nuclear_electricity)

#sprawdzenie kolumn
str(df)
df %>% glimpse()
df %>% as_tibble()

##Sprawdzenie korelaji i ich istotność
df_cor <- df %>% select(where(is.double))

##Standaryzacja zmiennych
df_standarized <-  scale(df_cor) %>% as.data.frame()
rownames(df_standarized) <- df$country

#Wykresy punktowe danych
ggpairs(df_standarized) +
  theme_light() +
  theme(strip.text = element_text(size=10, color = "black"))

#Macierz dystansu
d <- dist(df_standarized, method = "euclidean")

#Grupowanie hclust - przed podziałem
hc1 <- hclust(d, method = "ward.D2")

#Dendogram przed podziałem - wykres
fviz_dend(hc1)

#Piękny dendogram, podział na 3 grupy - wykres
fviz_dend(hc1,
          k = 3, rect = TRUE,
          rect_border = "gray80",         
          k_colors = c("#000a14", "#9a0707", "#0052a3"),
          color_labels_by_k = TRUE, lwd = 1.1) + ylab("")

#Aktualizacja danych
df$cluster.w <- cutree(hc1, k = 3) %>% as.factor()

#Zamiana na postać długą
df_long <- df %>% 
  pivot_longer(gdp_pc:phycisians_per_1000,
               values_to = "value",
               names_to = "zmienna")

#Rozkład ze względu na klasteryzację - boxplot
ggplot(df_long,
       aes(value,cluster.w, col = cluster.w, fill = cluster.w))+
  geom_boxplot(alpha = .5)+
  facet_wrap(vars(zmienna),
             scales = "free",
             ncol = 4) +
  ylab("") +
  xlab("") +
  theme_light() + 
  theme(strip.text = element_text(size=11, color = "black")) +
  ggtitle("Charakterystyka grup według hclust") +
  scale_fill_manual(values = c("#000a14", "#9a0707", "#0052a3")) +
  scale_color_manual(values = c("#000a14", "#9a0707", "#0052a3"))

#sprawdzenie
df_long %>% group_by(zmienna, cluster.w) %>% summarise(median(value)) %>% arrange(desc(zmienna, cluster.w))
df_long %>% filter(cluster.w == 1) %>% group_by( zmienna) %>% summarise(median(value)) %>% arrange(zmienna)
df_long %>% filter(cluster.w == 2) %>% group_by( zmienna) %>% summarise(median(value)) %>% arrange(zmienna)
df_long %>% filter(cluster.w == 3) %>% group_by( zmienna) %>% summarise(median(value)) %>% arrange(zmienna)

### Grupowanie Kmeans ###
###Przygotowywanie danych do wizualizacji ###
x <- rep(0, 10)
for(i in 1:10)
  x[i] <- kmeans(df_standarized, centers = i, nstart = 10)$tot.withinss

wss <- ggplot(x %>% as.data.frame(),
              aes(row_number(-x),x)) +
  geom_line(size = 1.5, alpha = 0.4, col = "#000a14") +
  geom_point(col = "#0052a3", size = 4, alpha = 0.9) +
  xlab("") +
  ylab("") +
  theme_minimal() + 
  ggtitle("Total Within Sum of Squares") +
  theme(title = element_text(size = 13))

#Kryterium Average Silhouette i Calinskiego - Harabasza
km.ch <- kmeansruns(df_standarized, criterion = "ch", runs = 10)
km.asw <- kmeansruns(df_standarized, criterion = "asw", runs = 10)

#Przygotowanie danych do wizualizacji
kryteria <- cbind.data.frame(`Caliński - Harabasz` = km.ch$crit,
                             `Average Silhouette` = km.asw$crit)
kryteria_pivot <- kryteria %>%
  mutate(index = 1:10) %>% 
  pivot_longer(`Caliński - Harabasz`:`Average Silhouette`,
               names_to = "kryterium",
               values_to = "Wartość")

aw_ch <- ggplot(kryteria_pivot,
                aes(index, `Wartość`)) +
  geom_line(size = 1.5,
            alpha = 0.4, col = "#000a14") +
  geom_point(col = "#0052a3", size = 4, alpha = 0.9) +
  xlab("") +
  ylab("") +
  facet_wrap(vars(kryterium), scales = "free") +
  theme_minimal() +
  theme(strip.text = element_text(size=15))

#Kryteria - wykres   
plot_grid(aw_ch, wss, nrow = 3, rel_heights = c(6, 8, 1))

### Kmeans ###
df$cluster.km <-  kmeans(df_standarized,
                         centers = 2,
                         nstart = 10)$cluster %>% as.factor()

#Zamiana na postać długą v2
df_long <-  df %>% 
  pivot_longer(gdp_pc:phycisians_per_1000,
               values_to = "value",
               names_to = "zmienna")

#Kmeans - wykres
ggplot(df_long,
       aes(value,cluster.km, col = cluster.km, fill = cluster.km))+
  geom_boxplot(alpha = .5)+
  facet_wrap(vars(zmienna),
             scales = "free",
             ncol = 4) +
  ylab("") +
  xlab("") +
  theme_light() + 
  theme(strip.text = element_text(size=11, color = "black")) +
  ggtitle("Charakterystyka grup według kmeans") +
  scale_fill_manual(values = c("#000a14","#9a0707")) +
  scale_color_manual(values = c("#000a14","#9a0707"))

#Kmeans vs Hclust GDP per Capita - wykres
ggplot(df_long %>% filter(zmienna == "gdp_pc") %>% 
         pivot_longer(cluster.w:cluster.km,
                      names_to = "cluster",
                      values_to = "cluster_value"),
       aes(value,cluster_value, col = cluster_value,
           fill = cluster_value))+
  geom_boxplot(alpha = .5)+
  facet_wrap(vars(cluster),
             scales = "free_y",
             ncol = 4) +
  ylab("") +
  xlab("") +
  theme_light() + 
  theme(strip.text = element_text(size=11, color = "black")) +
  ggtitle("GDP per capita kmeans vs hclust") +
  scale_fill_manual(values = c("#000a14","#9a0707","#0052a3")) +
  scale_color_manual(values = c("#000a14","#9a0707","#0052a3")) + 
  geom_curve(data = data.frame(x = 13228.3476837539,
                               y = 0.73680235500068,
                               xend = 13943.0469670678,
                               yend = 0.993691405408211,
                               cluster = "cluster.km"),
             mapping = aes(x = x,
                           y = y,
                           xend = xend,
                           yend = yend),
             arrow = arrow(30L, unit(0.1, "inches"), "last", "closed"),
             inherit.aes = FALSE) + 
  geom_text(data = data.frame(x = 9064.855754389,
                              y = 0.72946266784618,
                              label = "Estonia",
                              cluster = "cluster.km"),
            mapping = aes(x = x,
                          y = y,
                          label = label),
            colour = "#9a0707", inherit.aes = FALSE)

#Kmeans vs Hclust PCA

#Przygotowanie danych
pca_object <- PCA(df_standarized, graph = F, ncp = 2)

dflong_last_plot <-  pca_object$ind$coord %>%
  as.data.frame() %>%
  mutate(`kmeans` = df$cluster.km,
         `hclust` = df$cluster.w,
         country = rownames(.)) %>% 
  pivot_longer(kmeans:hclust,
               values_to = "cluster_value",
               names_to = "cluster_name")

#Przygotowanie danych - postać długa
colnames(dflong_last_plot)
#Kmeans vs Hclust - ostatni wykres
ggplot(dflong_last_plot,
       aes(Dim.1,Dim.2, col = cluster_value)) + 
  geom_point() +
  ggrepel::geom_text_repel(aes(label = country)) +
  facet_wrap(vars(cluster_name)) +
  xlab("") +
  ylab("") +
  theme_light() + 
  theme(strip.text = element_text(size=11, color = "black")) +
  ggtitle("GDP per capita kmeans vs hclust") +
  scale_fill_manual(values = c("#000a14","#9a0707","#0052a3")) +
  scale_color_manual(values = c("#000a14","#9a0707","#0052a3"))

#wizualizacja danych kateogrycznych


  
  