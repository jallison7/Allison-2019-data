# load packages
myPackages <- c("car","ggplot2","rio","tidyverse","gridExtra")
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[, 1])) 
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(myPackages, usePackage)


# change "path" on the line below to the directory where 'Figure 6 through 8 data.csv' is saved

# setwd("path")

data <- import("Figure 6 through 8 data.csv", setclass = 'tibble')
data <- data[which(data$PercentCorrugated<20),]

pUpland <- mean(data$PercentUpland)/100
pMoapa <- mean(data$PercentMoapa)/100
pShivwits <- mean(data$PercentShivwits)/100
 
qUpland <- 1 - pUpland
qMoapa <- 1 - pMoapa
qShivwits <- 1 - pShivwits


#calculate the width of a one s.d. confidence interval (really calculating the +/-, i.e., half the width)
widthUpland <- sqrt((pUpland*qUpland)/data$Total)
widthMoapa <- sqrt((pMoapa*qMoapa)/data$Total)
widthShivwits <- sqrt((pShivwits*qShivwits)/data$Total)


z = 2.576 #creates a 99% confidence band


intervalUpperUpland <- 100* (pUpland + (z*widthUpland))
intervalLowerUpland <- 100 *(pUpland - (z*widthUpland))
intervalUpperMoapa <- 100* (pMoapa + (z*widthMoapa))
intervalLowerMoapa <- 100 *(pMoapa - (z*widthMoapa))
intervalUpperShivwits <- 100* (pShivwits + (z*widthMoapa))
intervalLowerShivwits <- 100 *(pShivwits - (z*widthMoapa))


data <- cbind(data,intervalLowerUpland, intervalUpperUpland,intervalLowerMoapa, intervalUpperMoapa,
              intervalLowerShivwits, intervalUpperShivwits)

#setconstants to help keep plots consistent and make it easy to change font sizes, point sizes, etc.
pointSize <- 1.5
axisLabelSize <- 10
tickLabelSize <- 8
lineWidth <- .5
pointType = 17


# create plot for total upland pottery

NvU <- ggplot(data, width = 5, height = 3,) +
  geom_point(aes(x=Total, y = PercentUpland), size=pointSize, shape=pointType, color = "black") +
  geom_line (aes(x=Total, y = intervalLowerUpland), size = lineWidth) +
  geom_line (aes(x = Total, y = intervalUpperUpland), size = lineWidth) +
   theme_bw() +
   labs(x="Assemblage Size", y = "Percent Upland") +
   scale_x_continuous(breaks = c(200,400,600,800,1000,1200,1400,1600,1800), limits=c(0,1800)) +
   scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70)) +
   theme(axis.title.x = element_text(color="black",size=axisLabelSize),
         axis.title.y = element_text(color="black", size=axisLabelSize),
         axis.text.x = element_text(color = "black", size=tickLabelSize),
         axis.text.y = element_text(color = "black", size=tickLabelSize))



# create plot for Moapa Gray Ware

NvM <- ggplot(data, width = 5, height = 3) +
  geom_point(aes(x=Total, y = PercentMoapa), size=pointSize, shape=pointType, color = "black") +
  geom_line (aes(x=Total, y = intervalLowerMoapa), size = lineWidth) +
  geom_line (aes(x = Total, y = intervalUpperMoapa),size = lineWidth) +
  theme_bw() +
  labs(x="Assemblage Size", y = "Percent Moapa") +
  scale_x_continuous(breaks = c(200,400,600,800,1000,1200,1400,1600,1800), limits=c(0,1800)) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70)) +
  theme(axis.title.x = element_text(color="black",size=axisLabelSize),
        axis.title.y = element_text(color="black", size=axisLabelSize),
        axis.text.x = element_text(color = "black", size=tickLabelSize),
        axis.text.y = element_text(color = "black", size=tickLabelSize))


# create plot for Shivwits Ware

NvS <- ggplot(data, width = 5, height = 3) +
  geom_point(aes(x=Total, y = PercentShivwits), size=pointSize, shape=pointType, color = "black") +
  geom_line (aes(x=Total, y = intervalLowerShivwits), size = lineWidth) +
  geom_line (aes(x = Total, y = intervalUpperShivwits),size = lineWidth) +
  theme_bw() +
  labs(x="Assemblage Size", y = "Percent Shivwits") +
  scale_x_continuous(breaks = c(200,400,600,800,1000,1200,1400,1600,1800), limits=c(0,1800)) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70)) +
  theme(axis.title.x = element_text(color="black",size=axisLabelSize),
        axis.title.y = element_text(color="black", size=axisLabelSize),
        axis.text.x = element_text(color = "black", size=tickLabelSize),
        axis.text.y = element_text(color = "black", size=tickLabelSize))

# draw the plots

IntervalPlot <- grid.arrange(NvU,NvM,NvS)



# ggsave("Figure 7.tiff", IntervalPlot, dpi=600, width = 5, height = 9)
# ggsave("Figure 7.jpg", IntervalPlot, dpi=600, width = 5, height = 9)