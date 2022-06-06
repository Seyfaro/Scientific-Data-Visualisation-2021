if(!require("lubridate")) install.packages("lubridate")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(readxl) 
library(ggplot2)




trilobites <- read_excel("./trilobites.xlsx", sheet = "trilobites")
View(trilobites)


#plot the chart. 
p <- ggplot(trilobites, aes(x=order, y=approx.age))+
  geom_jitter(aes(colour = order), alpha=0.05) + #transparency to allow display of density
  geom_boxplot(width=0.6, fill="white", alpha=0.7)+ #plots the boxplot
  scale_y_reverse(lim=c())+
  coord_flip()+
  labs(title="Trilobite fossil occurrences", x="Trilobite order", y="Approx. age of occurrence (ma)", fill="Order (1 point = singular occurance)")
  
#custom theme

p+theme(panel.background = element_rect(fill="lightgrey", colour="lightgrey"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.y = element_text(size=20, color="black"),
        axis.text.x = element_text(size=20, color="black"),
        plot.title=element_text(size=20, face="bold", hjust = 0.5, color = "black"),
        axis.title.x = element_text(colour = "black", face="bold", hjust = 0.5, size = 16),
        axis.title.y = element_text(colour = "black", face = "bold", size = 16),
        axis.ticks.length=unit(.25, "cm"),
        legend.title = element_blank(),
        axis.ticks=element_line(size=2, colour="black"),
        axis.line = element_line(size = 2, linetype = "solid",colour = "black"))









