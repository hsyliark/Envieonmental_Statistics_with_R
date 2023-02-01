CJ1 <- read.csv("C:/Users/User/Desktop/논문데이터/J1 cell.csv",sep=",",header=T)
CJ2 <- read.csv("C:/Users/User/Desktop/논문데이터/J2 cell.csv",sep=",",header=T)
CT1 <- read.csv("C:/Users/User/Desktop/논문데이터/T1 cell.csv",sep=",",header=T)
CT2 <- read.csv("C:/Users/User/Desktop/논문데이터/T2 cell.csv",sep=",",header=T)

library(dplyr)
CT2_reshape <- CT2 %>%
  group_by(yyyymm) %>%
  summarise_at(vars(c(Cyanophytes,Diatoms,Chlorophytes,Others)), list(name = mean))
colnames(CT2_reshape) <- c("month","Cyanophytes","Diatoms","Chlorophytes","Others")

library(reshape2)
CT2_melt <- melt(CT2_reshape,
                 id.vars = 'month',
                 variable.name = "algae",
                 value.name = "cells") 
CT2_melt$month <- as.factor(CT2_melt$month)

library(ggplot2)
ggplot(CT2_melt, aes(x=month, y=cells, group=algae, color=algae)) +
  geom_line(size=1.5) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(x ="month", y = "mean of cell count(cells/mL)") +
  theme(axis.text.x = element_text(size = 12, face='bold'),
        axis.text.y = element_text(size = 12, face='bold'),
        axis.title.x = element_text(size=20,face='bold'),
        axis.title.y = element_text(size=20,face='bold'),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"))
