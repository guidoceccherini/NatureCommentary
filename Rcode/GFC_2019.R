
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



Data_EU = list()



list_countries <- c('AU','HU','PL','IT','FR', 'LO','GM','EZ','RO','HR','SW','FI','UK','EI','SP','PO',
                    'LH','LG','EN','GR','BU','BE', 'LU','SI','DA','NL')
list_countries_REAL <- c('Austria','Hungary','Poland','Italy','France', 'Slovakia','Germany',
                         'Czech Republic','Romania','Croatia','Sweden','Finland','UK','Ireland','Spain','Portugal',
                         'Lithuania','Latvia','Estonia','Greece','Bulgaria','Belgium', 
                         'Luxembourg','Slovenia', 'Denmark','Netherlands')
for (i in 1: 26){
  
  
  
  Forest_L <- read.csv(
    paste0('Data/NatureLetter/Country_Forest_Change_loss_F_',list_countries[i],'.csv' ))
  ### TOTAL FOREST LOSS
  Forest_TOTAL_LOSS_TOT <- read.csv(paste0('Data/NatureLetter/Country_Forest_Change_loss_TOT_',list_countries[i],'.csv' ))
      Forest_TOTAL_LOSS_WF <- read.csv(paste0('Data/NatureLetter/Country_Forest_Change_loss_WF_',list_countries[i],'.csv' ))
  Forest_TOTAL_LOSS_F <- read.csv(paste0('Data/NatureLetter/Country_Forest_Change_loss_F_',list_countries[i],'.csv' ))
  Forest_TOTAL_LOSS_W <- read.csv(paste0('Data/NatureLetter/Country_Forest_Change_loss_W_',list_countries[i],'.csv' ))
  
  
  
  
  
  
  
  Data_EU_DB<- data.frame(Year =  rep(NA, 19))
  Data_EU_DB$Year <-  Forest_TOTAL_LOSS_WF$year
  Data_EU_DB$TotalLoss <-  Forest_TOTAL_LOSS_TOT$Forest.Loss.Total / 10000000
  Data_EU_DB$PartialLoss <-  Forest_TOTAL_LOSS_WF$Forest.Loss.Total / 10000000
  Data_EU_DB$Delta <- Data_EU_DB$TotalLoss - Data_EU_DB$PartialLoss
  
  Data_EU_DB$WIND <-  Data_EU_DB$TotalLoss - Forest_TOTAL_LOSS_W$Forest.Loss.Total/ 10000000
  
  
  Data_EU_DB$FIRES <-  Data_EU_DB$Delta  - Data_EU_DB$WIND
  
  
#   
#   
  Data_EU_DB$Country <- as.character(list_countries_REAL[i])
#   
  
  
  Data_EU[[i]] <- Data_EU_DB # add it to your list
  
}

big_data_EU = do.call(rbind, Data_EU)

big_data_EU <- big_data_EU %>% select(-TotalLoss) %>% select(-Delta)

colnames(big_data_EU)[2] <- 'Harvest'
colnames(big_data_EU)[3] <- 'ExtremeEvents'
colnames(big_data_EU)[4] <- 'Fires'


big_data_EU2 <-  big_data_EU %>% group_by(Year,Country) %>%
  summarise(Harvest = sum(Harvest, na.rm = TRUE),ExtremeEvents = sum(ExtremeEvents, na.rm = TRUE),Fires = sum(Fires, na.rm = TRUE))




FA2000 <- read_csv('Data/ForestArea2000.csv')

FA2000$Country[FA2000$Country == 'United Kingdom'] <- 'UK'
FA2000$Country[FA2000$Country == 'Czechia'] <- 'Czech Republic'

big_data_EU2bis = do.call(rbind, Data_EU)

big_data_EU2bis <- big_data_EU2bis %>% select(Year,Country,TotalLoss)

big_data_EU2Summarized <- big_data_EU2bis %>% 
 group_by(Country ) %>% 
  summarise( MaxH = max(TotalLoss, na.rm =T) )


FA2000J <- dplyr::left_join(FA2000,big_data_EU2Summarized, by =c('Country'))
FA2000J$ratio <- (FA2000J$MaxH / FA2000J$treecover2000)*100
FA2000J <- FA2000J %>% select(Country,ratio)



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

library(wesanderson)
# pal <- wes_palette("Darjeeling1")

### EU STATS


library(tidyverse)

counts_EU2 <- big_data_EU2
counts_EU_2 = reshape2::melt(counts_EU2, id.vars = c(1,2))##melt(r_df, id.vars = c('x','y'))
# 
counts_EU_2 <- counts_EU_2 %>%  filter(Year >= 2004)


# Biomass <- read_csv('/data/cecchgu/Forest_management_EU/Figure_NC/Fig_Biomass_S1/Data/Biomass.csv')
# Biomass <- as.data.frame(Biomass)


ggplot(counts_EU_2)+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
  geom_bar(aes(x=Year, y=value, fill = forcats::fct_rev(variable)),stat="identity")+
  facet_wrap(Country ~ ., scales = "free",ncol=5)+
  ##, position=position_dodge())+ ##, position=position_dodge())
  theme_bw()+ 
  # theme_void() +
  scale_x_continuous(limits=c(2003.4,2019.6),breaks = c(2004, 2009, 2014,2019))+##,labels=c("2004","2017"))+
  # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
  # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
  # scale_fill_manual(values = rev(colorblind_pal()(6)))+
  
  # scale_fill_npg(palette = rev("nrc"))+
  # ggtitle('Mean LAI')+ 
  xlab("Year") +
  ylab(" Harvested Area [1,000 ha]") +##["~km^2~"]") +
  labs( linetype = "Biomass",
        # fill=expression('  '))+
        fill=expression(atop("Forest Loss Area from:")))+
  theme_Publication()+
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 10),
        # legend.key.height = unit(1.75,'cm'),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size = 10),
        axis.title.x =  element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        # legend.key.width = unit(1.75,'cm'),
        # legend.position = "bottom",
        # legend.direction = "horizontal"  ,
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )+
  # guides(fill=guide_legend(nrow=3, title.vjust =-1))+ 
  # # labs(tag ='a')+
  # # theme(plot.tag = element_text(size = 24, face = "bold"))+
  # # theme(plot.tag.position = c(0.01, 1.05))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))


# ggsave("Plot2019.png", width = 23, height = 23, units = "cm",dpi=300,path = 'figures' )




counts_EU_2bis <- left_join(counts_EU_2,FA2000J,'Country')

counts_EU_2bis$Country <- paste0(counts_EU_2bis$Country,'  (', round(counts_EU_2bis$ratio,2),'%)')


ggplot(counts_EU_2bis)+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
  geom_bar(aes(x=Year, y=value, fill = forcats::fct_rev(variable)),stat="identity")+
  facet_wrap(Country ~ ., scales = "free",ncol=4)+
  ##, position=position_dodge())+ ##, position=position_dodge())
  theme_bw()+ 
  # theme_void() +
  scale_x_continuous(limits=c(2003.4,2019.6),breaks = c(2004, 2009, 2014,2019))+##,labels=c("2004","2017"))+
  # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
  # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
  # scale_fill_manual(values = rev(colorblind_pal()(6)))+
  
  # scale_fill_npg(palette = rev("nrc"))+
  # ggtitle('Mean LAI')+ 
  xlab("Year") +
  ylab(" Harvested Area [1,000 ha]") +##["~km^2~"]") +
  labs( linetype = "Biomass",
        # fill=expression('  '))+
        fill=expression(atop("Forest Loss Area from:")))+
  theme_Publication()+
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 10),
        # legend.key.height = unit(1.75,'cm'),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size = 10),
        axis.title.x =  element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        # legend.key.width = unit(1.75,'cm'),
        # legend.position = "bottom",
        # legend.direction = "horizontal"  ,
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )+
  # guides(fill=guide_legend(nrow=3, title.vjust =-1))+ 
  labs(tag ='a')+
  theme(plot.tag = element_text(size = 24, face = "bold"))+
  theme(plot.tag.position = c(0.01, .99))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))


ggsave("Plot2019bis.png", width = 23, height = 23, units = "cm",dpi=300,path = 'figures' )


#####aggregate



big_data_EU3 <-  big_data_EU2 %>% group_by(Year) %>%
  summarise(Harvest = sum(Harvest, na.rm = TRUE),
            ExtremeEvents = sum(ExtremeEvents, na.rm = TRUE),
 Fires = sum(Fires, na.rm = TRUE))

counts_EU_3 <- big_data_EU3
counts_EU_3 = reshape2::melt(counts_EU_3, id.vars = c(1))##melt(r_df, id.vars = c('x','y'))
# 
counts_EU_3 <- counts_EU_3 %>%  filter(Year >= 2004)



ggplot(counts_EU_3)+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
  geom_bar(aes(x=Year, y=value, fill = forcats::fct_rev(variable)),stat="identity")+
  # facet_wrap(Country ~ ., scales = "free",ncol=4)+
  ##, position=position_dodge())+ ##, position=position_dodge())
  theme_bw()+ 
  # theme_void() +
  scale_x_continuous(limits=c(2003.4,2019.6),breaks = c(2004, 2009, 2014,2019))+##,labels=c("2004","2017"))+
  # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
  # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
  # scale_fill_manual(values = rev(colorblind_pal()(6)))+
  
  # scale_fill_npg(palette = rev("nrc"))+
  # ggtitle('Mean LAI')+ 
  xlab("Year") +
  ylab(" Harvested Area [1,000 ha]") +##["~km^2~"]") +
  labs( linetype = "Biomass",
        # fill=expression('  '))+
        fill=expression(atop("Forest Loss Area from:")))+
  theme_Publication()+
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 10),
        # legend.key.height = unit(1.75,'cm'),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size = 10),
        axis.title.x =  element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        # legend.key.width = unit(1.75,'cm'),
        # legend.position = "bottom",
        # legend.direction = "horizontal"  ,
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )+
  # guides(fill=guide_legend(nrow=3, title.vjust =-1))+ 
  # labs(tag ='a')+
  theme(plot.tag = element_text(size = 24, face = "bold"))+
  theme(plot.tag.position = c(0.01, .99))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))


ggsave("Plot2019Tot.png", width = 23, height = 15, units = "cm",dpi=300,path = 'figures' )


##### style paper


counts_EU2_summ <- big_data_EU2%>% group_by(Year)
counts_EUD <- summarize(counts_EU2_summ, Harvest = sum(Harvest, na.rm =T), ExtremeEvents = sum(ExtremeEvents, na.rm =T), Fires = sum(Fires, na.rm =T))
head(counts_EUD)

# write_csv(counts_EUD, 'EU_agg.csv')

counts_EU_2 = reshape2::melt(counts_EUD, id.vars = c(1))##melt(r_df, id.vars = c('x','y'))
# 
counts_EU_2 <- counts_EU_2 %>%  filter(Year >= 2004)



ggplot(counts_EU_2)+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
  geom_bar(aes(x=Year, y=value, fill = forcats::fct_rev(variable)),stat="identity")+
  # facet_wrap(Country ~ ., scales = "free",ncol=5)+
  ##, position=position_dodge())+ ##, position=position_dodge())
  theme_bw()+ 
  # theme_void() +
  scale_x_continuous(limits=c(2003.4,2019.6),breaks = c(2004, 2009, 2014,2019))+##,labels=c("2004","2017"))+
  # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
  # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
  # scale_fill_manual(values = rev(colorblind_pal()(6)))+
  
  # scale_fill_npg(palette = rev("nrc"))+
  # ggtitle('Mean LAI')+ 
  xlab("Year") +
  ylab(" Harvested Area [1,000 ha]") +##["~km^2~"]") +
  labs( linetype = "Biomass",
        # fill=expression('  '))+
        fill=expression(atop("Forest Loss:")))+
  theme_Publication()+
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 16),
        # legend.key.height = unit(1.75,'cm'),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size = 16),
        axis.title.x =  element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        # legend.key.width = unit(1.75,'cm'),
        # legend.position = "bottom",
        # legend.direction = "horizontal"  ,
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )+
  # guides(fill=guide_legend(nrow=3, title.vjust =-1))+ 
  # # labs(tag ='a')+
  # # theme(plot.tag = element_text(size = 24, face = "bold"))+
  # # theme(plot.tag.position = c(0.01, 1.05))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))


ggsave("Plot2019aggregated.png", width = 20, height = 14, units = "cm",dpi=300,path = 'figures' )


##### Remove Management

counts_EU2_summ <- big_data_EU2%>% group_by(Year)
counts_EUD <- summarize(counts_EU2_summ, ExtremeEvents = sum(ExtremeEvents, na.rm =T), Fires = sum(Fires, na.rm =T))
head(counts_EUD)



counts_EU_2 = reshape2::melt(counts_EUD, id.vars = c(1))##melt(r_df, id.vars = c('x','y'))
# 
counts_EU_2 <- counts_EU_2 %>%  filter(Year >= 2004)



ggplot(counts_EU_2)+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
  geom_bar(aes(x=Year, y=value, fill = forcats::fct_rev(variable)),stat="identity")+
  # facet_wrap(Country ~ ., scales = "free",ncol=5)+
  ##, position=position_dodge())+ ##, position=position_dodge())
  theme_bw()+ 
  # theme_void() +
  scale_x_continuous(limits=c(2003.4,2019.6),breaks = c(2004, 2009, 2014,2019))+##,labels=c("2004","2017"))+
  # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
  # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
  # scale_fill_manual(values = rev(colorblind_pal()(6)))+
  
  # scale_fill_npg(palette = rev("nrc"))+
  # ggtitle('Mean LAI')+ 
  xlab("Year") +
  ylab(" Loss Area [1,000 ha]") +##["~km^2~"]") +
  labs( linetype = "Biomass",
        # fill=expression('  '))+
        fill=expression(atop("Forest Loss:")))+
  theme_Publication()+
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 16),
        # legend.key.height = unit(1.75,'cm'),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size = 16),
        axis.title.x =  element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        # legend.key.width = unit(1.75,'cm'),
        # legend.position = "bottom",
        # legend.direction = "horizontal"  ,
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )+
  # guides(fill=guide_legend(nrow=3, title.vjust =-1))+ 
  # # labs(tag ='a')+
  # # theme(plot.tag = element_text(size = 24, face = "bold"))+
  # # theme(plot.tag.position = c(0.01, 1.05))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))


ggsave("Plot2019WithoutBlue.png", width = 20, height = 14, units = "cm",dpi=300,path = 'figures' )



Biomass = 96.6

ggplot(counts_EU_2)+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
  geom_bar(aes(x=Year, y=value*Biomass, fill = forcats::fct_rev(variable)),stat="identity")+
  # facet_wrap(Country ~ ., scales = "free",ncol=5)+
  ##, position=position_dodge())+ ##, position=position_dodge())
  theme_bw()+ 
  # theme_void() +
  scale_x_continuous(limits=c(2003.4,2018.6),breaks = c(2004, 2009, 2014,2018))+##,labels=c("2004","2017"))+
  # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
  # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
  # scale_fill_manual(values = rev(colorblind_pal()(6)))+
  
  # scale_fill_npg(palette = rev("nrc"))+
  # ggtitle('Mean LAI')+ 
  xlab("Year") +
  ylab(" Biomass Loss [1,000 t]") +##["~km^2~"]") +
  labs( linetype = "Biomass",
        # fill=expression('  '))+
        fill=expression(atop("Forest Loss:")))+
  theme_Publication()+
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 16),
        # legend.key.height = unit(1.75,'cm'),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        axis.text.x = element_text(size = 16),
        axis.title.x =  element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        # legend.key.width = unit(1.75,'cm'),
        # legend.position = "bottom",
        # legend.direction = "horizontal"  ,
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        legend.key.width = unit(1.75,'cm'),
        legend.position = "bottom",
        legend.direction = "horizontal"   )+
  # guides(fill=guide_legend(nrow=3, title.vjust =-1))+ 
  # # labs(tag ='a')+
  # # theme(plot.tag = element_text(size = 24, face = "bold"))+
  # # theme(plot.tag.position = c(0.01, 1.05))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))


ggsave("Plot2019WithoutBlueBiomass.png", width = 20, height = 14, units = "cm",dpi=300,path = 'figures' )

