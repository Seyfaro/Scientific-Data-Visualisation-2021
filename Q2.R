if(!require("ggpubr")) install.packages("ggpubr")
if(!require("readxl")) install.packages("readxl")
if(!require("nortest")) install.packages("nortest")
library(readxl)
library(ggpubr)
library(nortest)

input_data <- read_excel('./GATdata.xlsx') #Make sure data is in the same folder as the rscript to use this format.
View(input_data)



colnames(input_data)[2]  <- "MYDATA"


#the Freedman-Diaconis rule to decide the bin width since this code must work with any dataset
bw <- 2 * (IQR(input_data$MYDATA) / length(input_data$MYDATA)^(1/3))

#histogram 
h <- ggplot(input_data, aes(MYDATA)) + 
  geom_histogram(col="black", aes(fill = ..count..), binwidth = bw) + 
  labs(title="Histogram", y = "Count")+
  theme(axis.title.x = element_blank(),
  axis.text.y = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1),
  axis.ticks=element_blank(),
  axis.title.y = element_blank())
print(h)

#QQ plot 
qq <- ggplot(input_data, aes(sample = MYDATA)) +
  stat_qq(size=4, color="black", fill="blue", shape=23) + 
  stat_qq_line(size = 1, color = "red") +
  labs(title="QQ plot", x="Theoretical Quantiles", y="Sample Quantiles")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
print(qq) #NOTE - none of the plots would show until print was added, if it was set to be a named object.

#Stats Summary
s <- summary(input_data[c(2)])

#plot table of summary stats
tsum <- ggtexttable(s, rows = NULL, cols = NULL,
  theme = ttheme(colnames.style = colnames_style(color = "Black", fill = "grey"),
  tbody.style = tbody_style(color = "black")))
print(tsum)



bxp <- ggplot(data = input_data, aes_string(y = "MYDATA", x = 1)) + 
  geom_boxplot(fill='light blue')+
  coord_flip()+
  labs(title="Data distribution")+
  theme(aspect.ratio=1/14,
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.x = element_blank())
print(bxp)


ADtest<-ad.test(input_data$MYDATA) 
Anderson.Darling.squared<-round(ADtest$statistic, 2) #AD test to two decimal places
Pvalue<-round(ADtest$p.value, 3) #pvalue to 3 decimal places with caveat of next IF statement
if (Pvalue<0.05) {Pvalue = "<0.05"} 

AD<-data.frame(mget(c('Anderson.Darling.squared', 'Pvalue')))

ADtable <- ggtexttable(AD, rows = NULL,
                    theme = ttheme(colnames.style = colnames_style(color = "Black", fill = "grey"),
                                   tbody.style = tbody_style(color = "black")))
print(ADtable)
#Arrange the summary
plot<-ggarrange(h, tsum, bxp, ADtable, qq, ncol = 2, nrow = 3, align= "v", widths = c(2, 1))

annotate_figure(plot, top = text_grob("Summary report for TITLE", 
                                      color = "black", face = "bold", size = 18))