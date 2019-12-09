myPackages <- c("rio","tidyverse","gridExtra")
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[, 1])) 
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(myPackages, usePackage)

# remove the comment symbol (#), and change "path" on the line below to the directory where 'Figure 5 data.csv' is saved

# setwd("path")

data <- import("Figure 5 data.csv", setclass = 'tibble')

data <- data[which(data$Total>=75),]

dataSouthern <- data[which(data$Location=="Southern Shivwits Plateau"),]
dataHidden <- data[which(data$Location=="Hidden Hills"),]

pointSize <- 1.5
axisLabelSize <- 10
tickLabelSize <- 8
lineWidth <- .5
sp = .9

#create plot of percent corrugated versus percent upland pottery 

CvU <- ggplot(data, aes(x=PercentCorrugated, y = PercentUpland), width = 5, height = 3) +
  labs(x="Percent Corrugated", y = "Percent Upland") +
  geom_point(size=pointSize, shape=data$PointType, color = "black") +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60)) +
  scale_y_continuous(breaks=c(40,50,60,70,80,90),limits=c(40,100)) +
  theme(axis.title.x = element_text(color="black",size=axisLabelSize),
        axis.title.y = element_text(color="black", size=axisLabelSize),
        axis.text.x = element_text(color = "black", size=tickLabelSize),
        axis.text.y = element_text(color = "black", size=tickLabelSize))+
  geom_rect(xmin=-5,xmax=32,ymin=95,ymax=103,color = "black",size=0.2,fill="transparent") +
  geom_point(aes(x=0,y=100),size=pointSize, shape=16, color = "black") +
  geom_point(aes(x=0, y=97),size=pointSize, shape=17, color = "black")+
  geom_text(x=16.65, y=100.2, label="Southern Shivwits Plateau", size = 2, aes(fontface=1,family="sans")) +
  geom_text(x=9, y=97.2, label="Hidden Hills", size =2, aes(fontface=1,family="sans")) +
  stat_smooth(method = "loess",
              col = "black",
              se = FALSE,
              size = lineWidth,
              span = sp, linetype=6)


# create plot of percent corrugated versus percent Shivwits Ware

CvS <- ggplot(data, aes(x=PercentCorrugated, y = PercentShivwits),width = 5, height = 3) +
  labs(x="Percent Corrugated", y = "Percent Shivwits Ware") +
  geom_point(size=pointSize, shape=data$PointType, color = "black") +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,65)) +
  scale_y_continuous(breaks=c(0,20,40,60,80,100)) +
  theme(axis.title.x = element_text(color="black",size=axisLabelSize),
        axis.title.y = element_text(color="black", size=axisLabelSize),
        axis.text.x = element_text(color = "black", size=tickLabelSize),
        axis.text.y = element_text(color = "black", size=tickLabelSize)) +
  geom_rect(xmin=29,xmax=68,ymin=92,ymax=105,color = "black",size = 0.2,fill="transparent") +
  geom_point(aes(x=31.5,y=100),size=pointSize, shape=16, color = "black") +
  geom_point(aes(x=31.5, y=95),size=pointSize, shape=17, color = "black")+
  geom_text(x=50.15, y=100.5, label="Southern Shivwits Plateau", size = 2, aes(fontface=1,family="sans")) +
  geom_text(x=28.5, y=68, label="Southern", size = 2, aes(fontface=1, angle=-40, family="sans")) +
  geom_text(x=41, y=95.5, label="Hidden Hills", size =2, aes(fontface=1,family="sans")) +
  geom_text(x=38, y=35, label="Hidden Hills", size = 2, aes(fontface=1, angle=47, family="sans")) +
  stat_smooth(dataHidden,mapping=aes(x=PercentCorrugated, y=PercentShivwits),method = "loess",
              col = "black",
              se = FALSE,
              size = lineWidth,
              linetype=2,
              span = sp) +
  stat_smooth(dataSouthern,mapping=aes(x=PercentCorrugated, y=PercentShivwits),method = "loess",
              col = "black",
              se = FALSE,
              size = lineWidth,
              linetype=2,
              span = sp)


# create plot of percent corrugated versus percent Moapa Gray Ware

CvM <- ggplot(data, aes(x=PercentCorrugated, y = PercentMoapa),width = 5, height = 3) +
  labs(x="Percent Corrugated", y = "Percent Moapa Gray Ware") +
  geom_point(size=pointSize, shape=data$PointType, color = "black") +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60)) +
  scale_y_continuous(breaks=c(0,20,40,60,80,100), limits=c(0,90)) +
  theme(axis.title.x = element_text(color="black",size=axisLabelSize),
        axis.title.y = element_text(color="black", size=axisLabelSize),
        axis.text.x = element_text(color = "black", size=tickLabelSize),
        axis.text.y = element_text(color = "black", size=tickLabelSize),) +
  geom_rect(xmin=-5,xmax=32,ymin=82,ymax=95,color = "black",size=0.2,fill="transparent") +
  geom_point(aes(x=0,y=90),size=pointSize, shape=16, color = "black") +
  geom_point(aes(x=0, y=85),size=pointSize, shape=17, color = "black")+
  geom_text(x=16.65, y=90.5, label="Southern Shivwits Plateau", size = 2, aes(fontface=1,family="sans")) +
  geom_text(x=9, y=85.5, label="Hidden Hills", size =2, aes(fontface=1,family="sans")) +
  stat_smooth(method = "loess",
              col = "black",
              se = FALSE,
              size = lineWidth,
              span = sp)


# create plot of LOESS lines

LoS <- ggplot(data,width = 5, height = 3) +
  labs(x="Percent Corrugated", y = "Percent") +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60)) +
  scale_y_continuous(breaks=c(0,20,40,60,80,100), limits=c(0,100)) +
  geom_smooth(method = "loess", 
              aes(PercentCorrugated, PercentUpland),
              col = "black",
              se = FALSE,
              size = lineWidth,
              linetype=6,
              span = sp) +
  geom_smooth(method = "loess", 
              aes(PercentCorrugated, PercentMoapa),
              col = "black",
              se = FALSE,
              size = lineWidth,
              span = sp) +
  stat_smooth(dataHidden,mapping=aes(x=PercentCorrugated, y=PercentShivwits),method = "loess",
              col = "black",
              se = FALSE,
              size = lineWidth,
              linetype=2,
              span = sp) +
  stat_smooth(dataSouthern,mapping=aes(x=PercentCorrugated, y=PercentShivwits),method = "loess",
              col = "black",
              se = FALSE,
              size = lineWidth,
              linetype=2,
              span = sp)+

  geom_text(x=21, y=43, label="Moapa Gray Ware", size = 2, aes(fontface=1, angle=-50, family="sans")) +
  geom_text(x=43, y=49, label="Shivwits Ware", size = 2, aes(fontface=1, angle=44, family="sans")) +
  geom_text(x=46, y=46, label="(Hidden Hills)", size = 2, aes(fontface=1, angle=44, family="sans")) +
  geom_text(x=45, y=67, label="Shivwits Ware", size = 2, aes(fontface=1, angle=0, family="sans")) +
  geom_text(x=45, y=62, label="(Southern)", size = 2, aes(fontface=1, angle=0, family="sans")) +
  geom_text(x=25, y=83.5, label="Upland Pottery", size = 2, aes(fontface=1, angle=33, family="sans")) +
  theme(axis.title.x = element_text(color="black",size=axisLabelSize),
        axis.title.y = element_text(color="black", size=axisLabelSize),
        axis.text.x = element_text(color = "black", size=tickLabelSize),
        axis.text.y = element_text(color = "black", size=tickLabelSize))


# draw the plots (labeling of the LOESS lines may look wrong on the screen, but it should be correct in the saved files)

ShivwitsPlots <- grid.arrange(CvM,CvS,CvU,LoS)

# remove the comment symbol from the lines below to save

# ggsave(ShivwitsPlots, filename = "Figure 5.tiff", dpi = 600, width = 5, height = 6)
# ggsave(ShivwitsPlots, filename = "Figure 5.jpg", dpi = 300, width = 5, height = 6)

