# load packages
myPackages <- c("ggplot2","rio","tidyverse","gridExtra")
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[, 1])) 
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(myPackages, usePackage)

# change "path" on the line below to the directory where 'Figure 6 through 8 data.csv' is saved

# setwd("path")

data <- import("Figure 6 through 8 data.csv", setclass = 'tibble')


pointSize <- 1.5
axisLabelSize <- 10
tickLabelSize <- 8
lineWidth <- .5
sp = .6

#create plot of percent corrugated versus percent upland pottery 

CvU <- ggplot(data, aes(x=PercentCorrugated, y = PercentUpland), width = 5, height = 3) +
  labs(x="Percent Corrugated", y = "Percent Upland") +
  geom_point(size=pointSize, shape=17, color = "black") +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20,30,40,50)) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70)) +
  theme(axis.title.x = element_text(color="black",size=axisLabelSize),
        axis.title.y = element_text(color="black", size=axisLabelSize),
        axis.text.x = element_text(color = "black", size=tickLabelSize),
        axis.text.y = element_text(color = "black", size=tickLabelSize)) +
  stat_smooth(method = "loess",
              col = "black",
              se = FALSE,
              size = lineWidth,
              linetype=6,
              span = sp)


# create plot of percent corrugated versus percent Shivwits Ware

CvS <- ggplot(data, aes(x=PercentCorrugated, y = PercentShivwits),width = 5, height = 3) +
  labs(x="Percent Corrugated", y = "Percent Shivwits Ware") +
  geom_point(size=pointSize, shape=17, color = "black") +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20,30,40,50)) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50)) +
  theme(axis.title.x = element_text(color="black",size=axisLabelSize),
        axis.title.y = element_text(color="black", size=axisLabelSize),
        axis.text.x = element_text(color = "black", size=tickLabelSize),
        axis.text.y = element_text(color = "black", size=tickLabelSize)) +
  stat_smooth(method = "loess",
              col = "black",
              se = FALSE,
              size = lineWidth,
              linetype=2,
              span = sp)


# create plot of percent corrugated versus percent Moapa Gray Ware

CvM <- ggplot(data, aes(x=PercentCorrugated, y = PercentMoapa),width = 5, height = 3) +
  labs(x="Percent Corrugated", y = "Percent Moapa Gray Ware") +
  geom_point(size=pointSize, shape=17, color = "black") +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20,30,40,50)) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50)) +
  theme(axis.title.x = element_text(color="black",size=axisLabelSize),
        axis.title.y = element_text(color="black", size=axisLabelSize),
        axis.text.x = element_text(color = "black", size=tickLabelSize),
        axis.text.y = element_text(color = "black", size=tickLabelSize)) +
  stat_smooth(method = "loess",
              col = "black",
              se = FALSE,
              size = lineWidth,
              span = sp)

# create plot of LOESS lines

LoS <- ggplot(data,width = 5, height = 3) +
  labs(x="Percent Corrugated", y = "Percent") +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20,30,40,50)) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50)) +
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
  geom_smooth(method = "loess", 
            aes(PercentCorrugated, PercentShivwits),
            col = "black",
            se = FALSE,
            size = lineWidth,
            linetype=2,
            span = sp)+
  geom_text(x=30, y=18.7, label="Moapa Gray Ware", size = 2, aes(fontface=1, angle =-37, family="sans")) +
  geom_text(x=35, y=8, label="Shivwits Ware", size = 2, aes(fontface=1, angle =-30, family="sans")) +
  geom_text(x=34.3, y=24, label="Upland Pottery", size = 2, aes(fontface=1, angle =-53, family="sans")) +
  theme(axis.title.x = element_text(color="black",size=axisLabelSize),
        axis.title.y = element_text(color="black", size=axisLabelSize),
        axis.text.x = element_text(color = "black", size=tickLabelSize),
        axis.text.y = element_text(color = "black", size=tickLabelSize))


# draw the plots (labeling of the LOESS lines may look wrong on the screen, but it should be correct in the saved files)

MoapaPlots <- grid.arrange(CvM,CvS,CvU,LoS)

# remove the comment symbol from the lines below to save

# ggsave(MoapaPlots, filename = "Figure 6.jpg", dpi = 600, width = 5, height = 6)
# ggsave(MoapaPlots, filename = "Figure 6.tiff", dpi = 600, width = 5, height = 6)

