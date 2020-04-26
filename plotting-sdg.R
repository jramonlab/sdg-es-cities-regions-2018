library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(ggflags)
library(countrycode)
library(colorspace)
library(colorblindr)

options(digits = 3)

load("./rda/sdg2018es.rda")


boxplot(sdg01~region_id, data = sdg2018es)
boxplot(sdg_avg~region_id, data = sdg2018es)


#
# Adds region ranking to master set
ranking <- sdg2018es %>%  
  group_by(Reg = .$'Region name') %>%
  summarize(CitiesInRegion = n(), region_avg = mean(sdg_avg), id = unique(region_id)) %>% 
  mutate(region_avg = round(region_avg, 1)) %>%
  mutate(region_rank = rank(desc(region_avg))) %>%
  select(id, region_rank, region_avg)

sdg2018es <- left_join(sdg2018es, ranking, by = c("region_id" = "id"))


#
# Cities index across regions
sdg2018es %>%   
  ggplot(aes(x = reorder(.$'Region name', desc(region_rank)), sdg_avg)) +
  geom_boxplot() + 
  geom_point() + 
  coord_flip()


#
# Cities index across regions (GOOD)
sdg2018es %>%   
  ggplot(aes(x = reorder(.$'Region name', desc(region_rank)), sdg_avg)) +
  geom_boxplot() + 
  coord_flip() +
  scale_x_discrete(labels = c("Andalucía" = expression(bold("*ANDALUCÍA")), parse=TRUE)) + 
  ggtitle("Distribución Índice ODS de las ciudades por CCAA. 2018") + 
  xlab("") + 
  ylab("Índice ODS. (0-100)") +
  #annotate(geom="text", x=2.5, y=86, label = "Fuente: REDS.SDSN 2018",color="darkred", size = 4.5)+
  #annotate(geom="text", x=1.5, y=84, label = "Gráficos: JRLAB. 2020",color="darkblue", size = 4.5) +
  #theme_classic() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        aspect.ratio = 1/1)

ggsave("figs/ods_2018_cities_distribution.png", width = 15, height = 10)


##
# Convert to tidy format
library(tidyr)
sdg2018es_tidy <- sdg2018es %>% 
  gather(key = SDG_NUM, value = SDG_VAL, 
                                sdg01, sdg02, sdg03, sdg04, sdg05, sdg06, sdg07, sdg08,
                                sdg09, sdg10, sdg11, sdg12, sdg13, sdg14, sdg15, sdg16, sdg17)

#
# SDGs index across different SDGs in ANDALUCIA
sdg2018es_tidy %>% 
  filter(region_id == "ES-AN") %>%
  ggplot(aes(x = reorder(SDG_NUM, SDG_VAL), SDG_VAL)) +
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  coord_flip()


sdg2018es_tidy %>% 
  filter(region_id == "ES-AN") %>%
  ggplot(aes(x = reorder(SDG_NUM, SDG_VAL), SDG_VAL)) +
  geom_boxplot() +
  geom_point(size = 3, aes(color = (City == "Granada"), alpha = (City == "Granada"))) +
  coord_flip() + 
  ggtitle("Distribución Índice ODS ANDALUCÍA. 2018") + 
  xlab("") + 
  ylab("Índice ODS. (0-100)") +
  annotate(geom="text", x=2.5, y=86, label = "Fuente: REDS.SDSN 2018",color="darkred", size = 4.5)+
  annotate(geom="text", x=1.5, y=84, label = "Gráficos: JRLAB. 2020",color="darkblue", size = 4.5) +
  theme_bw() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        aspect.ratio = 1/1)



#
# SDGs index across different SDGs in GRANADA

sdg2018es_tidy %>% 
  filter(City == "Granada") %>%
  ggplot(aes(x = reorder(SDG_NUM, SDG_VAL),
             y = SDG_VAL,
             label = round(SDG_VAL, 1))) +
  geom_text(nudge_y = 5, color = "black", size = 5) +
  geom_col(alpha = 0.5, fill = "lightblue") +
  coord_flip() +
  scale_x_discrete(labels = c("Andalucía" = expression(bold("*ANDALUCÍA")), parse=TRUE)) + 
  scale_y_continuous(limits = c(0,100)) +
  ggtitle("Índice ODS GRANADA.2018") + 
  xlab("") + 
  ylab("Índice ODS. (0-100)") +
  annotate(geom="text", x=2.5, y=86, label = "Fuente: REDS.SDSN 2018",color="darkred", size = 4.5)+
  annotate(geom="text", x=1.5, y=84, label = "Gráficos: JRLAB. 2020",color="darkblue", size = 4.5) +
  theme_classic() +
  theme(title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        aspect.ratio = 1/1)


