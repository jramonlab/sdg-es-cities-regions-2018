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


# BLUE and GREY
simple_pallete <- c("grey", "#023048")
#
#A colorblind-friendly palette
# The palette with grey:
cbPalette_wgrey <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbPalette_wblack <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#000000", "#D55E00", "#CC79A7")
# To use for fills, add
#scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)
#
# color for different regions
color_values = c("#999999", "#d8b365", "#f5f5f5", "#5ab4ac", "#E69F00")
#


#
# simple chart of regions
sdg2018es %>% group_by(Reg = .$'Region name') %>%
              summarize(CitiesInRegion = n(), region_avg = mean(sdg_avg), id = unique(region_id)) %>% 
              mutate(Reg = reorder(Reg, region_avg)) %>%
              mutate(region_avg = round(region_avg, 1)) %>%
              ggplot(aes(Reg, region_avg, label = region_avg)) + 
              #scale_color_OkabeIto() +  
              geom_text(nudge_y = 5, color = "black", size = 4.5) +
              geom_col(alpha = 0.3) + 
              coord_flip() +
              scale_y_continuous(limits = c(0,100)) +
              ggtitle("Índice ODS conseguido por Comunidad") + 
              xlab("Índice ODS. (0-100)")

#
# 100 cities check
sdg2018es %>% nrow()

#
# GOOD chart of regions
sdg2018es %>% group_by(Reg = .$'Region name') %>%
  summarize(CitiesInRegion = n(), region_avg = mean(sdg_avg), id = unique(region_id)) %>% 
  mutate(Reg = reorder(Reg, region_avg)) %>%
  mutate(region_avg = round(region_avg, 1)) %>%
  mutate(toHighlight = ifelse( Reg == "Andalucía", "yes", "no" ))  %>%
  ggplot(aes(Reg, region_avg, 
             label = region_avg,
             fill   = toHighlight == "yes",
             text = CitiesInRegion)) + 
  geom_text(nudge_y = 5, color = "black", size = 5) +
  #geom_col(alpha = 0.4, fill = "lightblue") +
  geom_col() +
  coord_flip() +
  scale_x_discrete(labels = c("Andalucía" = expression(bold("ANDALUCÍA")), parse=TRUE)) + 
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_manual(values = simple_pallete) +
  ggtitle("Índice ODS conseguido por Comunidad. 2018") + 
  xlab("") + 
  ylab("Índice ODS. (0-100)") +
  annotate(geom="text", x=2.5, y=86, label = "Fuente: REDS.SDSN 2018",color="darkred", size = 4.5)+
  annotate(geom="text", x=1.5, y=84, label = "Gráficos: JRLAB. 2020",color="darkblue", size = 4.5) +
  #theme_classic() +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        aspect.ratio = 1/1)

ggsave("figs/ods_2018_index_1_1.png", width = 15, height = 10)


#
# GOOD chart of regions + cities
sdg2018es %>% group_by(Reg = .$'Region name') %>%
  summarize(CitiesInRegion = n(), region_avg = mean(sdg_avg), id = unique(region_id)) %>% 
  mutate(Reg = reorder(Reg, region_avg)) %>%
  mutate(toHighlight = ifelse( Reg == "Andalucía", "yes", "no" ))  %>%
  mutate(region_avg = round(region_avg, 1)) %>%
  ggplot(aes(Reg, region_avg, 
             label = region_avg, 
             fill   = toHighlight == "yes",
             text = CitiesInRegion)) + 
  
  geom_text(nudge_y = 5, color = "black", size = 5) +
  #geom_col(alpha = 0.2, fill = "lightblue") +
  geom_col(alpha = 0.4) +
  
  geom_col(aes(Reg, CitiesInRegion), alpha = 0.2, fill = "red", width = 0.2) + 
  geom_text(aes(label = CitiesInRegion), y = 10 ,color = "black", size = 4) +
  
  coord_flip() +
  scale_fill_manual(values = simple_pallete) +
  scale_x_discrete(labels = c("Andalucía" = expression(bold("ANDALUCÍA")), parse=TRUE)) + 
  scale_y_continuous(limits = c(0,100)) +
  ggtitle("Índice ODS conseguido por Comunidad. 2018") + 
  xlab("") + 
  ylab("Ciudades analizadas por CCAA / Índice ODS. (0-100)") +
  annotate(geom="text", x=2.5, y=86, label = "Fuente: REDS.SDSN 2018",color="darkred", size = 4.5)+
  annotate(geom="text", x=1.5, y=84, label = "Gráficos: JRLAB. 2020",color="darkblue", size = 4.5) +
  #theme_classic() +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        aspect.ratio = 1/1)

ggsave("figs/ods_2018_index_region_1_1.png", width = 15, height = 10)



#
# GOOD chart of ANDAÑLUCIA + cities + GRANADA (accent)
sdg2018es %>% filter(region_id == "ES-AN") %>%
  mutate(toHighlight = ifelse( City == "Granada", "yes", "no" ))  %>%
  mutate(sdg_avg = round(sdg_avg,1)) %>%
  ggplot(aes(x = reorder(City, sdg_avg), 
             y = sdg_avg, 
             label = sdg_avg, 
             fill   = toHighlight == "yes")) + 
  geom_text(nudge_y = 5, color = "black", size = 5) +
  #geom_col(alpha = 0.2, fill = "lightblue") +
  geom_col(alpha = 0.4) +
  #geom_text(aes(label = CitiesInRegion), y = 10 ,color = "black", size = 4) +
  
  coord_flip() +
  scale_fill_manual(values = simple_pallete) +
  scale_y_continuous(limits = c(0,100)) +
  ggtitle("Índice ODS conseguido en ciudades de Andalucía. 2018") + 
  xlab("") + 
  ylab("Índice ODS. (0-100)") +
  annotate(geom="text", x=2.5, y=86, label = "Fuente: REDS.SDSN 2018",color="darkred", size = 4.5)+
  annotate(geom="text", x=1.5, y=84, label = "Gráficos: JRLAB. 2020",color="darkblue", size = 4.5) +
  #theme_classic() +
  theme(legend.position = "none",
        title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        aspect.ratio = 1/1)

ggsave("figs/ods_2018_index_ANDALUCIA_GR_1_1.png", width = 15, height = 10)


