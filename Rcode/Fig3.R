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
  
  
  
  a <- read.csv('Data/WindGiovanniHessd.csv')
  b <- read.csv('Data/WindOURMETHOD.csv')
  c <- read.csv('Data/WindGiovanniHessdM1.csv')
  # d <- read.csv('Data/WindGiovanniHessdArea.csv')
  
  a$Forest.Loss.Total <- a$Forest.Loss.Total + c$Forest.Loss.Total
  Wind = cbind(a,b)
  
  Wind <- Wind[, c(3,2,6)]
  
  Wind[,2:3] <- Wind[,2:3] / 10000000
  
  colnames(Wind)[2] <- "Loss FORWIND"
  
  colnames(Wind)[3] <- "Loss CECCHERINI"
  # colnames(Wind)[4] <- "Forest FORWIND"
  
  
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
  
  Wind2 = reshape2::melt(Wind, id.vars = c(1))##melt(r_df, id.vars = c('x','y'))
  
  
  
  library(wesanderson)
  
  ggplot(Wind2, aes(x=year, y=value, fill = forcats::fct_rev(variable)))+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
    geom_bar(stat="identity", position=position_dodge())+##, position=position_dodge())+ ##, position=position_dodge())
    theme_minimal()+ #### , scales="free"
    # scale_x_continuous(limits=c(2010.4,2019.6),breaks = c(2011, 2013, 2015,2019))+##,labels=c("2004","2017"))+
    # facet_grid( product~Product_f )+
    scale_fill_manual(values = (wes_palette("Royal1",2)))+ ##("Royal1")
    # scale_fill_manual(values = c( "#E64B35FF","#00A087FF",'green'))+
    # scale_y_continuous(trans='log2')+
    # scale_fill_npg(palette = rev("nrc"))+
    # ggtitle('Mean LAI')+ 
    xlab("Year ") +
    ylab(" Area WindThrow [1000 ha]") +##["~km^2~"]") +
    labs(fill=expression(atop("")))+ 
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
  # ggsave("AreaWind2018_2.png", width = 23, height = 15, units = "cm",dpi=300 ,path = 'figures' )
  
  
  
  Wind3 <- data.frame(year= seq(2004,2018), Accuracy = c(100.00000,47.07818,NA ,100.00000 ,100.00000, 100.00000,87.77628,NA,NA , 95.45431,27.95369,0.00000,NA, 100.00000,35.78488))
  
  Wind2bis <- left_join(Wind2%>%  filter(variable == 'Loss CECCHERINI'),Wind3,'year')%>% filter(!is.na(Accuracy)) 
  
  library(ggrepel)
  
  p1 <- ggplot(Wind2, aes(x=year, y=value, fill = forcats::fct_rev(variable)))+##//forcats::fct_rev(variable)))+##, color=CompLoss))+
    geom_bar(stat="identity", position=position_dodge())+##, position=position_dodge())+ ##, position=position_dodge())
    theme_minimal()+ #### , scales="free"
    # scale_x_continuous(limits=c(2010.4,2019.6),breaks = c(2011, 2013, 2015,2019))+##,labels=c("2004","2017"))+
    # facet_grid( product~Product_f )+
    scale_fill_manual(values = (wes_palette("Royal1",3)))+ ##("Royal1")
    # scale_fill_manual(values = c( "#E64B35FF","#00A087FF",'green'))+
    # scale_y_continuous(trans='log2')+
    # scale_fill_npg(palette = rev("nrc"))+
    # ggtitle('Mean LAI')+ 
    xlab("Year ") +
    ylab("Forest Area Loss [1000 ha]") +##["~km^2~"]") +
    labs(fill=expression(atop("")))+ 
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
    geom_label_repel(data= Wind2bis, aes(x=year,value= value, label = paste0(round(Accuracy,0),'%')) ,    segment.colour = "black",
                     fill = 'white', color = 'black',size = 3.5,nudge_y = 85,nudge_x = .1)
  # labs( tag = 'a')+
    # theme(plot.tag = element_text(size = 18, face = "bold"))
  p1
  ggsave("AreaWind2018_3.png", width = 23, height = 15, units = "cm",dpi=300 ,path = 'figures' )
  
  
  
  
  library(grid)
  fig.name <- 'MA_FIG_3'
  fig.width <- 14; fig.height <- 9; 
  fig.fmt <- 'eps' ##'png'
  fig.fullfname <- paste0('figures', '/',  fig.name, '.', fig.fmt)
  if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "cm", res= 300)}
  if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}
  if(fig.fmt == 'eps'){pdf(fig.fullfname, width = fig.width, height = fig.height)}
  
  grid.newpage()
  # Create layout : nrow = 3, ncol = 2
  pushViewport(viewport(layout = grid.layout(nrow = 1, ncol =1)))
  # A helper function to define a region on the layout
  define_region <- function(row, col){
    viewport(layout.pos.row = row, layout.pos.col = col)
  } 
  # Arrange the plots
  print(p1, vp = define_region(row = 1, col = 1))   # Span over two columns

  
  # print(p1, vp = viewport(width = 1, height = 1, x = 0, y = 0, just=c(0,0)))
  # print(p2, vp = viewport(width = 1, height = 1, x = 0.5, y = 0, just=c(0,0)))
  # 
  # grid.text(expression(bold("a")), x = unit(0.02, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
  # grid.text(expression(bold("b")), x = unit(0.52, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
  # grid.text(expression(bold("c")), x = unit(0.76, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
  
  dev.off()
  
  
  
  