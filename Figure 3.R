# load packages
myPackages <- c("rio", "tidyverse", "ggtern")
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[, 1])) 
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
lapply(myPackages, usePackage)


# remove the comment symbol (#), and change "path" on the line below to the directory where 'Figure 3 data.csv' is saved

# setwd("path")

data <- import("Figure 3 data.csv")
data <- data[which(data$Location!="Plateaus"),]
data <- data[which(data$Location!="Saint George Basin"),]

ternary <- ggtern(data, mapping = aes(x=PercentMoapa, y=PercentShivwits, z=PercentTusayan)) +
  geom_point(size=2,shape=data$SymbolShape) +
  labs(x="Moapa", y = "Shivwits", z = "Tusayan") +
  theme_custom(base_size = 12, base_family = "",
               tern.plot.background = NULL, tern.panel.background = "NA",
               col.T = "black", col.L = "black",col.R = "black",
               col.grid.minor = NA) +
  theme_showarrows() +
  theme(tern.panel.grid.major = element_line(size=.5,linetype=3),
        axis.title = element_text(size=10))

#draw the plot

ternary


#this does not exactly produce the final version, 
#which was saved as a pdf and edited in Adobe Acrobat to add a key and make minor aesthetic changes
# remove the comment symbol from the lines below to save

# ggsave(ternary,filename="Figure 3.pdf", dpi=600)
# ggsave(ternary,filename="Figure 3.jpg", dpi=600)
# ggsave(ternary,filename="Figure 3.tiff", dpi=600)
 

  