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

# .$ is need to incoke column name between quotes inside AES




#dat <- dat %>% mutate(index = .$'2019 EU SDG Index Score (0-100)')
#
# index + region (OLD)

sdg2019eu %>% select(Country, id, 'Sub-region' , '2019 EU SDG Index Score (0-100)' ) %>%
  mutate('2019 EU SDG Index Score (0-100)' = round(.$'2019 EU SDG Index Score (0-100)', 1)) %>%
  mutate(Country = reorder(Country, .$'2019 EU SDG Index Score (0-100)')) %>%
  mutate(toHighlight = ifelse( Country == "European Union", 0.5, 0.4 ))  %>%
  ggplot(aes(Country, .$'2019 EU SDG Index Score (0-100)',
             label  = .$'2019 EU SDG Index Score (0-100)', 
             fill   = .$'Sub-region',
             linetype  = factor(toHighlight))) + 
  geom_bar(stat = "identity", color = "black", linetype = "blank") + 
  scale_x_discrete(labels = c("Spain" = expression(bold(SPAIN)),"European Union" = expression(bold(EUROPEAN_UNION)), parse=TRUE)) + 
  geom_text(nudge_y = 5, color = "black", size = 4.5) + 
  coord_flip() +
  xlab("") + 
  ylab("Índice ODS en EU (0-100)")+ 
  labs(fill = "Regiones de Europa") + 
  scale_fill_manual(values = cbPalette_wblack) + 
  scale_y_continuous(limits = c(0,100))+
  ggtitle("Índices consecución de los ODS en la Unión europea. Hasta 2019")+
  annotate(geom="text", x=1.8, y=86, label = "Source: SDSN & IEEP. 2019",color="darkred", size = 5.5)+
  annotate(geom="text", x=0.8, y=84, label = "Graphics: JRLAB. 2020",color="darkblue", size = 5.5) + 
  theme(axis.text.y = element_text(size = 17),
        legend.text = element_text(size = 19)) 

ggsave("figs/SDG_2019_index_region.png", width = 15, height = 15)


#
# index + region (TFM)

sdg2019eu %>% select(Country, id, 'Sub-region' , '2019 EU SDG Index Score (0-100)' ) %>%
  mutate('2019 EU SDG Index Score (0-100)' = round(.$'2019 EU SDG Index Score (0-100)', 1)) %>%
  mutate(Country = reorder(Country, .$'2019 EU SDG Index Score (0-100)')) %>%
  mutate(toHighlight = ifelse( Country == "European Union", 0.5, 0.4 ))  %>%
  ggplot(aes(Country, .$'2019 EU SDG Index Score (0-100)',
             label  = .$'2019 EU SDG Index Score (0-100)', 
             fill   = .$'Sub-region',
             linetype  = factor(toHighlight))) + 
  geom_bar(width = 0.90, size = 15, stat = "identity", color = "black", linetype = "blank") + 
  scale_x_discrete(labels = c("Spain" = expression(bold("*SPAIN")),"European Union" = expression(bold("*EUROPEAN UNION")), parse=TRUE)) + 
  geom_text(nudge_y = 5, color = "black", size = 5) + 
  coord_flip() +
  xlab("") + 
  ylab("Índice ODS en EU (0-100)")+ 
  labs(fill = "Regiones de Europa") + 
  scale_fill_manual(values = cbPalette_wblack) + 
  scale_y_continuous(limits = c(0,100))+
  ggtitle("Índices consecución de los ODS en la Unión europea. Hasta 2019")+
  annotate(geom="text", x=2.5, y=86, label = "Source: SDSN & IEEP. 2019",color="darkred", size = 5.5)+
  annotate(geom="text", x=1.5, y=84, label = "Graphics: JRLAB. 2020",color="darkblue", size = 5.5) + 
  theme(axis.text.y = element_text(size = 19),
        legend.text = element_text(size = 17),
        title = element_text(size = 20),
        aspect.ratio = 1/1)

ggsave("figs/SDG_2019_index_region_TFM_1_1.png", width = 15, height = 15)


#
# index + region (by ID and flags)
#
#
devtools::install_github("rensa/ggflags")
library(ggflags)
library(countrycode)
library(ggthemes)

ccode = c("at", "be", "bg", "cy", "cz", "de", "dk", "es", "ee",
          "eu", "fi", "fr", "gb", "gr", "hr", "hu", "ie", "it",
          "lt", "lu", "lv", "mt", "nl", "pl", "pt", "ro", "sk", "si", "se")


sdg2019eu %>% select(Country, id, 'Sub-region' , '2019 EU SDG Index Score (0-100)' ) %>%
  mutate('2019 EU SDG Index Score (0-100)' = round(.$'2019 EU SDG Index Score (0-100)', 1)) %>%
  mutate(id = reorder(id, desc(.$'2019 EU SDG Index Score (0-100)'))) %>%
  mutate(toHighlight = ifelse( Country == "European Union", 0.5, 0.4 ))  %>%
  ggplot(aes(id, .$'2019 EU SDG Index Score (0-100)',
             label  = .$'2019 EU SDG Index Score (0-100)', 
             fill   = .$'Sub-region',
             linetype  = factor(toHighlight))) + 
  geom_bar(stat = "identity", color = "black", linetype = "blank") + 
  scale_x_discrete(labels = c("ESP" = expression(bold("*ESP")),"EUU" = expression(bold("*EUU")), parse=TRUE)) + 
  geom_text(nudge_y = 2, color = "black", size = 4.5) + 
  xlab("") + 
  ylab("Índice ODS en EU (0-100)")+ 
  labs(fill = "Regiones de Europa") + 
  scale_fill_manual(values = cbPalette_wblack) + 
  scale_y_continuous(limits = c(0,100))+
  ggtitle("Índices consecución de los ODS en la Unión europea. Hasta 2019")+
  annotate(geom="text", x=26, y=99, label = "Source: SDSN & IEEP. 2019",color="darkred", size = 5.5)+
  annotate(geom="text", x=26, y=97, label = "Graphics: JRLAB. 2020",color="darkblue", size = 5.5) + 
  geom_flag(y = -2.5, aes(country = ccode), size = 8) +
  theme_fivethirtyeight() +
  #theme_excel() +
  theme(axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = 0.5))

  # theme(axis.text.y = element_text(size = 15),
  #      axis.text.x = element_blank(),
  #      legend.text = element_text(size = 19)) 
  # 

ggsave("figs/SDG_2019_index_region_flags_538.png", width = 15, height = 15)



#
# index + region + population

sdg2019eu %>% select(Country, id,'2019 Population', 'Sub-region' , '2019 EU SDG Index Score (0-100)' ) %>%
  mutate('2019 EU SDG Index Score (0-100)' = round(.$'2019 EU SDG Index Score (0-100)', 1)) %>%
  mutate(Country = reorder(Country, .$'2019 EU SDG Index Score (0-100)')) %>%
  ggplot(aes(Country, .$'2019 EU SDG Index Score (0-100)', 
             label = .$'2019 EU SDG Index Score (0-100)', 
             color = .$'Sub-region',
             size = .$'2019 Population')) + 
  geom_point() + 
  coord_flip() +
  geom_text(nudge_y = 5, color = "black", size = 3) + 
  theme(axis.text.y = element_text(size = 8)) +
  xlab("") + 
  ylab("EU SD Index Score")


#
# index + region + GDP per capita

sdg2019eu %>% select(Country, id,'GDP per capita, PPP in 2017', 'Sub-region' , '2019 EU SDG Index Score (0-100)' ) %>%
  mutate('2019 EU SDG Index Score (0-100)' = round(.$'2019 EU SDG Index Score (0-100)', 1)) %>%
  mutate(Country = reorder(Country, .$'2019 EU SDG Index Score (0-100)')) %>%
  ggplot(aes(Country, .$'2019 EU SDG Index Score (0-100)', 
             label = .$'2019 EU SDG Index Score (0-100)', 
             color = .$'Sub-region',
             size = .$'GDP per capita, PPP in 2017')) + 
  geom_point() + 
  coord_flip() +
  geom_text(nudge_y = 5, color = "black", size = 3) + 
  theme(axis.text.y = element_text(size = 8)) +
  xlab("") + 
  ylab("EU SD Index Score")


#
# index + region + GDP per capita + population

library(ggrepel)

# Clorblind simulation package
#devtools::install_github("wilkelab/cowplot")
#install.packages("colorspace", repos = "http://R-Forge.R-project.org")
#devtools::install_github("clauswilke/colorblindr")
library(colorblindr)
# 
# To use a color scale that works somewhat better for people with color-vision deficiency, we recommend scale_color_OkabeIto and scale_fill_OkabeIto:
#   
#   fig2 <- ggplot(iris, aes(Sepal.Length, fill = Species)) + 
#   geom_density(alpha = 0.7) + scale_fill_OkabeIto()
# fig2


fig <- sdg2019eu %>% select(Country, id, '2019 Population', 'GDP per capita, PPP in 2017', 'Sub-region' , '2019 EU SDG Index Score (0-100)' ) %>%
  mutate('2019 EU SDG Index Score (0-100)' = round(.$'2019 EU SDG Index Score (0-100)', 1)) %>%
  mutate(Country = reorder(Country, .$'2019 EU SDG Index Score (0-100)')) %>%
  ggplot(aes(.$'GDP per capita, PPP in 2017',
             .$'2019 EU SDG Index Score (0-100)',
             label = Country,
             color = .$'Sub-region',
             size = .$'2019 Population'/10^6)) + 
  geom_point() + 
  scale_size_area(max_size = 15) + 
  scale_color_OkabeIto() +
  scale_x_log10() + 
  # scale_y_log10() +
  scale_y_continuous(limits = c(0,100))+
  #geom_text_repel(color = "black", size = 4, nudge_y = 0.3) +
  geom_label_repel(size = 4) +
  ylab("EU SDG Index Score") +
  xlab("GDP per capita, PPP in 2017")


fig 

# 
# index + region + GDP per capita + population (with flags)

fig <- sdg2019eu %>% select(Country, id, '2019 Population', 'GDP per capita, PPP in 2017', 'Sub-region' , '2019 EU SDG Index Score (0-100)' ) %>%
  mutate('2019 EU SDG Index Score (0-100)' = round(.$'2019 EU SDG Index Score (0-100)', 1)) %>%
  mutate(Country = reorder(Country, .$'2019 EU SDG Index Score (0-100)')) %>%
  ggplot(aes(.$'GDP per capita, PPP in 2017',
             .$'2019 EU SDG Index Score (0-100)',
             label = Country,
             color = .$'Sub-region')) + 
  #           size = .$'2019 Population'/10^6)) + 
  geom_point(size = 22, alpha = 0.5) + 
  scale_color_OkabeIto() +
  #scale_x_log10() + 
  #scale_y_log10() +
  #scale_y_continuous(limits = c(0,100))+
  #geom_text_repel(color = "black", size = 4, nudge_y = 0.3) +
  #geom_label_repel(size = 4) +
  ylab("Índice de consecución de los objetivos (ODS). [De 0 a 100]") +
  xlab("PIB per capita, PPP en 2017 [en Euros]") +
  geom_flag(country = ccode, size = 14) + 
  scale_country() +
  ggtitle("Índices ODS 2019 Vs. Renta per capita (ppp) 2017") + 
  labs(color = "Regiones de Europa") + 
  theme(legend.position="bottom",
        legend.box = "horizontal",
        title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15))

fig 

ggsave("figs/SDG_index_Vs_GDP_region_flags.png", width = 15, height = 15)




####

# CORRELATION

####

library(GGally)

sdg2019eu %>% 
  filter(Country != "European Union" ) %>%
  select('2019 Population', 'GDP per capita, PPP in 2017', '2019 EU SDG Index Score (0-100)') %>%
  ggpairs(columns = 1:3, title="Country data (28 countries)")

ggsave("figs/Correlation_28_countries.png", width = 15, height = 15)


sdg2019eu %>% 
  filter(Country != "European Union" ) %>%
  select('2019 Population', 'GDP per capita, PPP in 2017', '2019 EU SDG Index Score (0-100)' ) %>%
  filter(.$'GDP per capita, PPP in 2017' < 60000) %>%
  ggpairs(columns = 1:3, title = "Country data (26 poorest countries)")

ggsave("figs/Correlation_26_poorest_countries.png", width = 15, height = 15)
