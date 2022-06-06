#need to install lubridate for the date creation
if(!require("lubridate")) install.packages("lubridate")
if(!require("tidyverse")) install.packages("tidyverse")
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


#Turns the year column into a date object for plotting. Creates a new column in the GATdata table. Next lines define a theme to be used in the ggplot. Centers the title, hides unwanted elements.
GATdata <- mutate(GATdata,date=str_c(Year,"01-01",sep="-")%>%ymd())
theme_GAT <- theme_minimal()+
  theme(axis.text.y = element_blank(), #Y axis has no values 
        legend.title = element_text(),
        panel.grid.minor=element_blank(),
        plot.title=element_text(size=16, face="bold", hjust = 0.5))

#number is max colours and RdBu is the colour scheme
col_strip <- brewer.pal(15,"RdBu")

#brings up a chart for colour schemes
brewer.pal.info

#plot the chart - It is a tile chart but where the Y value is 1, turning it into bars
ws<-ggplot(GATdata,aes(x=date,y=1,fill=Annual))+
  geom_tile()+
  scale_x_date(date_breaks = "10 years",
               date_labels = "%Y",
  expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_gradientn(colors=rev(col_strip))+
  guides(fill=guide_colorbar(barwidth = 1))+
  labs(title="Global Annual Temperature 1850-2020", x="Year", y=element_blank(), fill="Temperature anomalies (°C)\n w.r.t 1961-1990 average")+            #The \n forces a line break- the title was too long for one line
  geom_vline(xintercept = as.numeric(as.Date("2016-01-01")), linetype=5, color="purple",)+ #Vline annotation and labels for maximum and minimum values
  annotate(geom="text", x=as.Date("1860-01-01"), y=1, label="Min.(1862)", size=5, color="white",angle=90, face="bold")+
  theme_GAT
  

ws+annotate(geom="text", x=as.Date("2014-01-01"), y=1, label="Max.(2016)", size=5, color="white",angle=90, face="bold")+
  geom_vline(xintercept = as.numeric(as.Date("1862-01-01")), linetype=5, color="purple")


