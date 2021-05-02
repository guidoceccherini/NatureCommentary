library(readxl)
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

big_data_EU2 <- big_data_EU2 %>% select(Year,Country,ExtremeEvents)

big_data_EU2SL <- big_data_EU2 %>% filter(Country %in% c('Estonia',
                                                         'Finland','Hungary','Latvia','Lithuania',
                                                         'Romania','Sweden'))




DataSarah <- read_excel("Data/UPDATED MS-salvage-logging-rates_workingfile - Estimates.xlsx")

DataSarah <- DataSarah   %>% rename(Country = MS)  %>%
  rename(TotalNFI =  `Total logging ha`)  %>%
  rename(SalvageNFI = `Salvage logging ha`)%>%
  select(Country,Year,SalvageNFI)

DataSarah <- DataSarah %>% filter(Country %in% c('Estonia',
                                                         'Finland','Hungary','Latvia','Lithuania',
                                                         'Romania','Sweden'))



# DataSarah$TotalNFI <-  as.numeric(as.character(DataSarah$TotalNFI))
DataSarah$SalvageNFI <-  as.numeric(as.character(DataSarah$SalvageNFI))

DataSarah <- DataSarah   %>%
  # mutate (TotalNFI =  TotalNFI /1000000)%>% 
    mutate (SalvageNFI =  SalvageNFI/1000 )

# big_data_EU2SL_M3 <- big_data_EU2SL_M3   %>% rename(SalvageRS = ExtremeEvents)   %>%
#   mutate (SalvageRS =  SalvageRS /1000)%>% 
#   mutate (TotalRS =  TotalRS /1000)


NewDF <- dplyr::left_join(big_data_EU2SL,DataSarah, by =c('Country','Year'))%>% 
  rename(SalvageRS= ExtremeEvents)
  # select(-Harvest) %>%
  # select(Year,Country  , SalvageRS,SalvageNFI)
# %>% select(- Fires)


NewDF2 <- NewDF
NewDF2 = reshape2::melt(NewDF2, id.vars = c(1,2))##melt(r_df, id.vars = c('x','y'))
# 
NewDF2 <- NewDF2 %>%  filter(Year >= 2004)


# Biomass <- read_csv('/data/cecchgu/Forest_management_EU/Figure_NC/Fig_Biomass_S1/Data/Biomass.csv')
# Biomass <- as.data.frame(Biomass)
# Default order
levels(NewDF2$variable)
# Or specify the factor levels in the order you want
# NewDF2$variable <- factor(NewDF2$variable, levels = c("SalvageNFI" , "SalvageRS","TotalNFI","TotalRS"))


ggplot(NewDF2,aes(x=Year, y=value, fill = forcats::fct_rev(variable)))+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
  geom_bar(position = "dodge", stat = "identity")+
  facet_wrap(Country ~ ., scales = "free",ncol=3)+
  ##, position=position_dodge())+ ##, position=position_dodge())
  theme_bw()+ 
  # theme_void() +
  scale_x_continuous(limits=c(2010.4,2019.6),breaks = c(2011, 2015,2019))+##,labels=c("2004","2017"))+
  # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
  # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
  # scale_fill_manual(values = rev(colorblind_pal()(6)))+
  
  # scale_fill_npg(palette = rev("nrc"))+
  ggtitle('Salvage Logging Area')+ 
  xlab("Year") +
  ylab(" Area Loss [1,000 ha]") +##["~km^2~"]") +
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

ggsave("PlotAreaSarah.png", width = 23, height = 23, units = "cm",dpi=300,path = 'figures' )



###aggregate


NewDFAgg <-  NewDF %>% group_by(Year) %>%
  summarise(
    RemoteSensing = sum(SalvageRS, na.rm = TRUE),
    # TotalRS = sum(TotalRS, na.rm = TRUE),
    # TotalNFI = sum(TotalNFI, na.rm = TRUE),
    CountryReports = sum(SalvageNFI, na.rm = TRUE))%>%
  select(Year,RemoteSensing,CountryReports)%>%
  rename('Remote Sensing (CECCHERINI)' =  RemoteSensing)  
# Fires = sum(Fires, na.rm = TRUE))

NewDFAgg <- NewDFAgg
NewDFAgg2 = reshape2::melt(NewDFAgg, id.vars = c(1))##melt(r_df, id.vars = c('x','y'))
# 
NewDFAgg2 <- NewDFAgg2 %>%  filter(Year >= 2004)



# Biomass <- read_csv('/data/cecchgu/Forest_management_EU/Figure_NC/Fig_Biomass_S1/Data/Biomass.csv')
# Biomass <- as.data.frame(Biomass)
# Default order
levels(NewDFAgg2$variable)
# Or specify the factor levels in the order you want
NewDFAgg2$variable <- factor(NewDFAgg2$variable, levels = c("CountryReports" , "Remote Sensing (CECCHERINI)"))


ggplot(NewDFAgg2,aes(x=Year, y=value, fill = forcats::fct_rev(variable)))+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
  geom_bar(position = "dodge", stat = "identity")+
  # facet_wrap(Country ~ ., scales = "free",ncol=3)+
  ##, position=position_dodge())+ ##, position=position_dodge())
  theme_bw()+ 
  # theme_void() +
  scale_x_continuous(limits=c(2010.4,2019.6),breaks = c(2011, 2013,2015,2017,2019))+##,labels=c("2004","2017"))+
  # scale_fill_manual(values = rev(wes_palette("Royal1",2)))+ ##("Royal1")
  # scale_fill_manual(values = c( "#4DBBD5FF","#E64B35FF","#00A087FF"))+
  scale_fill_manual(values = (colorblind_pal()(6)))+
  
  # scale_fill_npg(palette = rev("nrc"))+
  ggtitle('Salvage Logging')+ 
  xlab("Year") +
  ylab(" Forest Loss [1000 ha]") +##["~km^2~"]") +
  labs( linetype = "Biomass",
        # fill=expression('  '))+
        fill=expression(atop(" ")))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
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
        legend.direction = "horizontal"   )+ 
  # guides(fill=guide_legend(nrow=3, title.vjust =-1))+ 
  # # labs(tag ='a')+
  # # theme(plot.tag = element_text(size = 24, face = "bold"))+
  # # theme(plot.tag.position = c(0.01, 1.05))+
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))+
  labs( tag = 'b')+
  theme(plot.tag = element_text(size = 18, face = "bold"))

ggsave("PlotAreaSalvage.png", width = 23, height = 15, units = "cm",dpi=300,path = 'figures' )

