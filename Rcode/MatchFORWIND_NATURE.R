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

# library(tidyverse)

library(sf)



a <- read.csv('Data/WindGiovanniAndNature.csv')
b <- read.csv('Data/WindGiovanniAndNature_year1.csv')

c <- read.csv('Data/WindGiovanni.csv')

a$Forest.Loss.Total[1:14] <- a$Forest.Loss.Total[1:14] + b$Forest.Loss.Total[1:14]

Omissions = cbind(a,c)

Omissions <- Omissions[, c(3,2,6)]

Omissions[,2:3] <- Omissions[,2:3] / 10000000

colnames(Omissions)[2] <- "Loss Both"
colnames(Omissions)[3] <- "Forest Only Forzieri"

Omissions$Ratio <- Omissions$`Loss Both` / Omissions$`Forest Only Forzieri`

Omissions$Ratio[Omissions$Ratio>1] <- 1
Omissions$Ratio[!is.finite(Omissions$Ratio)] <- NA

Omissions$Ratio <- Omissions$Ratio *100



theme_Publication <- function(base_size=14) {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
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

library(wesanderson)

ggplot(Omissions, aes(x=year, y=Ratio))+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
  geom_bar(stat="identity", position=position_dodge())+##, position=position_dodge())+ ##, position=position_dodge())
  theme_minimal()+ #### , scales="free"
  # scale_x_continuous(limits=c(2010.4,2019.6),breaks = c(2011, 2013, 2015,2019))+##,labels=c("2004","2017"))+
  # facet_grid( product~Product_f )+
  # scale_fill_manual(values = (wes_palette("GrandBudapest1",3)))+ ##("Royal1")
  # scale_fill_manual(values = c( "#E64B35FF","#00A087FF",'green'))+
  # scale_y_continuous(trans='log2')+
  # scale_fill_npg(palette = rev("nrc"))+
  # ggtitle('Mean LAI')+ 
  xlab("Year ") +
  ylab(" WindThrown Captured by our method") +##["~km^2~"]") +
  labs(fill=expression(atop("Wind Forest Loss")))+ 
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
ggsave("Omissions.png", width = 23, height = 15, units = "cm",dpi=300 ,path = 'figures' )



