library(raster)
library(lubridate)
library(tidyverse)

setwd("H:\\Water_sink_source\\2015_2100_compare")

effect <- stack("trend_var_total_1SSP585.tif")
climate = effect$climate

effect <- stack("trend_var_total_1SSP585.tif")
vege = effect$vege

LAI <- stack("LAI_mean_SSP585.tif")
all = stack("SSP585_all_effects.tif")

multi_frame <-
  as.data.frame(LAI , xy = TRUE) %>%
  na.omit() %>% mutate(across(c(x, y), round, digits = 3))

multi_frame$climate = raster::extract(climate ,multi_frame[,1:2])
multi_frame$vege = raster::extract(vege ,multi_frame[,1:2])
multi_frame$all = raster::extract(all ,multi_frame[,1:2])
multi_frame = multi_frame %>% na.omit()
colnames(multi_frame) = c("x","y","LAI","climate","vege","all")


se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

statis_plot_1 = multi_frame %>% na.omit() %>%
  mutate(bin_LAI = cut(LAI, breaks = c(seq(0,5,by=0.5),Inf))) %>% 
  group_by(bin_LAI) %>% 
  summarise(median = median(vege, na.rm = TRUE)*100,
            Q25 = quantile(vege,probs = 0.25, na.rm = TRUE)*100,
            Q75 = quantile(vege,probs = 0.75, na.rm = TRUE)*100 ) %>%
  filter( !bin_LAI %in% c("(5,Inf]") ) %>% na.omit() %>% transform(ID = "LAI")

statis_plot_2 = multi_frame %>% na.omit() %>%
  mutate(bin_LAI = cut(LAI, breaks = c(seq(0,5,by=0.5),Inf))) %>% 
  group_by(bin_LAI) %>% 
  summarise(median = median(climate, na.rm = TRUE)*100,
            Q25 = quantile(climate,probs = 0.25, na.rm = TRUE)*100,
            Q75 = quantile(climate,probs = 0.75, na.rm = TRUE)*100 ) %>%
  filter( !bin_LAI %in% c("(5,Inf]") ) %>% na.omit() %>% transform(ID = "Climate")

statis_plot_3 = multi_frame %>% na.omit() %>%
  mutate(bin_LAI = cut(LAI, breaks = c(seq(0,5,by=0.5),Inf))) %>% 
  group_by(bin_LAI) %>% 
  summarise(median = median(all, na.rm = TRUE)*100,
            Q25 = quantile(all,probs = 0.25, na.rm = TRUE)*100,
            Q75 = quantile(all,probs = 0.75, na.rm = TRUE)*100 ) %>%
  filter( !bin_LAI %in% c("(5,Inf]") ) %>% na.omit() %>% transform(ID = "ALL")

statis_plot = rbind(statis_plot_1,statis_plot_2)

ggplot(statis_plot, aes(x = bin_LAI, y = median,group = 2,color = ID)) +geom_point(size = 2)+
  geom_errorbar(data = statis_plot,aes(x= bin_LAI, y= median, ymin=Q25, ymax=Q75), width=0,size=1, 
                position=position_dodge(0.8)) +labs(x="LAI", y = "LAI or Climate effect") +
  theme(axis.title=element_text(size=14,face="bold",color="black"),
        axis.text = element_text(size=14,face="plain",color="black"),
        axis.text.x = element_text(size=14,angle = 60),
        panel.background=element_rect(colour="black",fill=NA),
        panel.grid.minor=element_blank(),
        text=element_text(size=14),
        legend.position="right",
        legend.text = element_text(size=14
        ),legend.background=element_rect(colour=NA,fill=NA),
        axis.ticks=element_line(colour="black"))


ggplot(statis_plot_2, aes(x = bin_LAI, y = median,group = 1)) +geom_point(size = 2)+
  geom_errorbar(data = statis_plot_2,aes(x= bin_LAI, y= median, ymin=Q25, ymax=Q75), width=0,size=1, 
                position=position_dodge(0.8))+ geom_line(size = 0.5) +labs(x="P/PET", y = "Climate effect") +
  theme(axis.title=element_text(size=14,face="bold",color="black"),
        axis.text = element_text(size=14,face="plain",color="black"),
        axis.text.x = element_text(size=14,angle = 60),
        panel.background=element_rect(colour="black",fill=NA),
        panel.grid.minor=element_blank(),
        text=element_text(size=14),
        legend.position="right",
        legend.text = element_text(size=14
        ),legend.background=element_rect(colour=NA,fill=NA),
        axis.ticks=element_line(colour="black"))

###########################################

statis_plot_3 = multi_frame %>% na.omit() %>%
  mutate(bin_LAI = cut(LAI, breaks = c(seq(0,5,by=0.5),Inf))) %>% 
  group_by(bin_LAI) %>% count()






