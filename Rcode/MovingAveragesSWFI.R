
library(rworldmap)

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(gdalUtils)
library(devtools)
library(rasterVis)

library(RColorBrewer)

library(ggplot2)
library(rgdal)           
library(zoo)
library(snow)
library(scales)

library(dplyr)

library(raster)
library(sp)
library(ggplot2)
library(RColorBrewer)
library(reshape2)

library("ggsci")
library("ggplot2")
library("gridExtra")
library(tidyverse)




theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold"),
            # legend.key.width = unit(1.75,'cm'),
            # legend.position = "bottom",
            # legend.direction = "horizontal"
    ))
  
}




DataSwedenFinland <- read.csv('Data/FigS3.csv')


# https://www.rdocumentation.org/packages/caTools/versions/1.1/topics/runmean
# https://stackoverflow.com/questions/55163699/what-does-the-align-parameter-do-in-rollapply

DataSwedenFinland <- DataSwedenFinland %>%
  select(Year, Country ,variable  ,     value) %>%
  mutate(variable = recode(variable, "National Statistics (Clearcut+shelterwood)"= "National Statistics (Final Fellings)"))%>%
  tidyr::spread(variable,value) %>%
# rename(NationalStats = "National Statistics (Clearcut+shelterwood)") %>%
  rename(NationalStatistics = "National Statistics (Final Fellings)" ) %>%
  rename(RemoteSensing = "Remote Sensing" )




DataSwedenFinland2 <- DataSwedenFinland %>%
  tidyr::gather(metric, value, NationalStatistics:RemoteSensing)%>% 
  filter(Country == 'Sweden') 





Increase1 <- DataSwedenFinland2 %>%
  group_by(metric) %>% 
  filter(Year >=2011)%>% 
  filter(Year <=2015)%>% 
    summarize(Mean1=mean(value,na.rm = T))

Increase2 <- DataSwedenFinland2 %>%
  group_by(metric) %>% 
  filter(Year >=2016)%>% 
  summarize(Mean1=mean(value,na.rm = T))

Increase <- round(((Increase2$Mean1-Increase1$Mean1)/Increase1$Mean1)*100)

Increase



ggplot(data= DataSwedenFinland2,aes(Year, value, fill = metric)) +
  # facet_wrap(Country ~ ., scales = "free")+
  geom_bar(position = "dodge", stat = "identity")+
  theme_minimal()+ #### , scales="free"
  scale_x_continuous(limits=c(2010.4,2018.6),breaks = c(2011, 2013, 2015,2018))+##,labels=c("2004","2017"))+
  # facet_grid( product~Product_f )+
  # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
  scale_fill_manual(values = c( "#E64B35FF","#00A087FF"))+
  # scale_fill_npg(palette = rev("nrc"))+
  # ggtitle('Sweden 3-year Moving Average \nMAPE = 9')+ 
  xlab("Year ") +
  ylab(" Harvested Forest Area [1000 ha]") +##["~km^2~"]") +
  labs(fill=expression(atop("Source")))+ 
  theme_Publication()+
  theme(panel.grid.minor=element_blank(), 
        strip.text = element_text(size = 10),
        # legend.key.height = unit(1.75,'cm'),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size = 20),
        axis.title.x =  element_text(size = 18),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        # legend.key.width = unit(1.75,'cm'),
        # legend.position = "bottom",
        # legend.direction = "horizontal"  ,
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )
ggsave("SwedenArea_annual.png", width = 23, height = 15, units = "cm",dpi=300,path = 'figures' )



DataSweden <- DataSwedenFinland  %>%
group_by(Country)  %>% 
  mutate(NationalStatisticsMA = rollmean(NationalStatistics, k = 3, fill = NA ,align = "right")) %>% 
  # mutate(NationalStatistics2MA = rollmean(NationalStats2, k = 5, fill = NA)) %>% 
  mutate(RS_MA = rollmean(RemoteSensing, k =3, fill = NA, align = "right"))%>%
  select(Year, Country,RS_MA ,NationalStatisticsMA) %>% 
  filter(Country == 'Sweden')%>% 
  drop_na()







t.test(DataSweden$RS_MA,DataSweden$NationalStatisticsMA)

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}
mape(DataSweden$RS_MA,DataSweden$NationalStatisticsMA)


DataSweden2 <- DataSweden %>%
  tidyr::gather(metric, value, RS_MA:NationalStatisticsMA)



Increase1 <- DataSweden2 %>%
  group_by(metric) %>% 
  filter(Year >=2011)%>% 
  filter(Year <=2015)%>% 
  summarize(Mean1=mean(value,na.rm = T))

Increase2 <- DataSweden2 %>%
  group_by(metric) %>% 
  filter(Year >=2016)%>% 
  summarize(Mean1=mean(value,na.rm = T))

Increase <- round(((Increase2$Mean1-Increase1$Mean1)/Increase1$Mean1)*100)

Increase

  ggplot(data= DataSweden2,aes(Year, value, fill = metric)) +
  facet_wrap(Country ~ ., scales = "free")+
    geom_bar(position = "dodge", stat = "identity")+
    theme_minimal()+ #### , scales="free"
    scale_x_continuous(limits=c(2010.4,2018.6),breaks = c(2011, 2013, 2015,2018))+##,labels=c("2004","2017"))+
    # facet_grid( product~Product_f )+
    # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
    scale_fill_manual(values = c( "#E64B35FF","#00A087FF"))+
    # scale_fill_npg(palette = rev("nrc"))+
    ggtitle('Sweden 3-year Moving Average \nMAPE = 9')+ 
    xlab("Year ") +
    ylab(" Harvested Forest Area [1000 ha]") +##["~km^2~"]") +
    labs(fill=expression(atop("Source")))+ 
    theme_Publication()+
    theme(panel.grid.minor=element_blank(), 
          strip.text = element_text(size = 10),
          # legend.key.height = unit(1.75,'cm'),
          axis.line.y = element_line(),
          axis.line.x = element_line(),
          axis.text.x = element_text(size = 20),
          axis.title.x =  element_text(size = 18),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          # legend.key.width = unit(1.75,'cm'),
          # legend.position = "bottom",
          # legend.direction = "horizontal"  ,
          legend.title=element_text(size=20), 
          legend.text=element_text(size=20),
          legend.key.width = unit(1.75,'cm'),
          legend.position = "bottom",
          legend.direction = "horizontal"   )
  ggsave("SwedenArea3year.png", width = 23, height = 15, units = "cm",dpi=300,path = 'figures' )
  