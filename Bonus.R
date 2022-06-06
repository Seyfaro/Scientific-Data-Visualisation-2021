
if(!require("lubridate")) install.packages("lubridate")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("readxl")) install.packages("readxl")
if(!require("ggplot2")) install.packages("ggplot2")
#libraries
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(readxl)

#import GATdata data
GATdata <- read_excel("./GATdata.xlsx")
View(GATdata)


#summary for the min/max temp values - used to find the year to label on the chart
summary(GATdata)


#number is max colours and RdBu is the colour scheme
col_strip <- brewer.pal(11,"RdBu")

#brings up a chart for colour schemes
brewer.pal.info

#plot the chart 
p<-ggplot(GATdata,aes(x=Year,y=Annual, fill=Annual))+
  geom_bar(stat="identity")+
  ylim(NA,1)+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn(colors=rev(col_strip))+
  guides(fill=guide_colorbar(barwidth = 1))+
  geom_vline(xintercept= 1862, linetype=5, color="purple",)+ #Vline annotation and labels for maximum and minimum values
  annotate(geom="text", x=1860, y=0.38, label="Min.(1862)", size=5, color="white",angle=90, face="bold")+
  geom_vline(xintercept= 2016, linetype=5, color="purple",)+ #Vline annotation and labels for maximum and minimum values
  annotate(geom="text", x=2014, y=-0.38, label="Max.(2016)", size=5, color="white",angle=90, face="bold")+
  labs(title="Global Annual Temperature 1850-2020", x="Year", y="Temperature anomalies (°C)\n w.r.t 1961-1990 average")            

#custom theme
  p+theme(panel.background = element_rect(fill="black", colour="black"),
          panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "dark grey"), 
          panel.grid.minor = element_line(size = 0.1, linetype = 'solid',colour = "dark grey"),
          plot.background = element_rect(fill = "black"),
          axis.text.y = element_text(size=20, color="white"),
          axis.text.x = element_text(size=20, color="white"),
          plot.title=element_text(size=35, face="bold", hjust = 0.5, color = "white"),
          axis.title.x = element_text(colour = "white", face="bold", hjust = 0.5, color = "white", size = 26),
          axis.title.y = element_text(colour = "white", face = "bold", size = 26),
          legend.title = element_blank(),
          legend.position = "none",
          axis.ticks.length=unit(.25, "cm"),
          axis.ticks=element_line(size=2, colour="white"),
          axis.line = element_line(size = 2, linetype = "solid",colour = "White"))

