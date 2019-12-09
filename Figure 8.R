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

#setconstants to help keep plots consistent and make it easy to change font sizes, point sizes, etc.
pointSize <- 2
axisLabelSize <- 10
tickLabelSize <- 8
lineWidth <- .5
pointType = 17

line <- as.data.frame(cbind(c(0,50),c(50,0)))
names(line) <- c("x","y")

MvS <- ggplot(data, width = 5, height = 3,) +
  geom_point(aes(x=PercentMoapa, y = PercentShivwits), size=pointSize, shape=pointType, color = "black") +
  geom_line (line, mapping = aes(x=x, y=y), size = lineWidth) +
  geom_text(x=10, y=42, label="50 Percent Upland", size = 2, aes(fontface=1, angle =-28, family="sans")) +
  theme_bw() +
  labs(x="Percent Moapa Gray Ware", y = "Percent Shivwits Ware") +
  theme(axis.title.x = element_text(color="black",size=axisLabelSize),
        axis.title.y = element_text(color="black", size=axisLabelSize),
        axis.text.x = element_text(color = "black", size=tickLabelSize),
        axis.text.y = element_text(color = "black", size=tickLabelSize))



MvS

# ggsave("Figure 8.tiff", MvS, dpi=600, width = 5, height = 3)
# ggsave("Figure 8.jpg", MvS, dpi=600, width = 5, height = 3)
