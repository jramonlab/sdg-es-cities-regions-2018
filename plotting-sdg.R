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

ggsave("figs/ods_2018_distribucion_andalucia.png", width = 15, height = 10)


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

ggsave("figs/ods_2018_Granada.png", width = 15, height = 10)

#
# SDGs index across different SDGs in GRANADA (with icons)
p <- sdg2018es_tidy %>% 
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



## Other approach  (HAND mapping)
# https://drmowinckels.io/blog/adding-external-images-to-plots/
library(png)
library(grid)

dat <- sdg2018es_tidy %>% 
  filter(City == "Granada") 

PLOT <- dat %>%
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
#
# Creat pic file array
sdg1_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-01.png"
sdg2_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-02.png"
sdg3_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-03.png"
sdg4_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-04.png"
sdg5_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-05.png"
sdg6_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-06.png"
sdg7_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-07.png"
sdg8_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-08.png"
sdg9_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-09.png"
sdg10_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-10.png"
sdg11_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-11.png"
sdg12_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-12.png"
sdg13_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-13.png"
sdg14_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-14.png"
sdg15_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-15.png"
sdg16_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-16.png"
sdg17_file <- "resources/SDG Icons 2019_WEB/E-WEB-Goal-17.png"


sdg_pic_file <- c(sdg1_file, sdg2_file, sdg3_file, sdg4_file, sdg5_file,
                  sdg6_file, sdg7_file, sdg8_file, sdg9_file, sdg10_file,
                  sdg11_file, sdg12_file, sdg13_file, sdg14_file, sdg15_file,
                  sdg16_file, sdg17_file)
#
# Reorder pics according to the chart 
sdg_pic_fil_ordered <- dat %>% cbind(sdg_pic_file, stringsAsFactors = FALSE) %>%
  arrange(SDG_VAL) %>%
  select(sdg_pic_file)

sdg_pic_file <- sdg_pic_fil_ordered$sdg_pic_file

g <- list()
for(i in 1:nrow(dat)){
  
  img = readPNG(sdg_pic_file[i])
  g[[i]] =  rasterGrob(img, interpolate=TRUE)
  
  PLOT = PLOT +
    annotation_custom(grob = g[[i]], xmin=i-.5, xmax=i+.5, ymin=0, ymax=10)
}
PLOT

ggsave("figs/ods_2018_Granada_pics.png", width = 15, height = 10)

