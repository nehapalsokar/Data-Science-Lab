library(ggplot2)
library(magrittr)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(gridExtra)

data <- read.csv("MentalHealthCleanedDataset.csv", header = TRUE, stringsAsFactors = TRUE)

#mental health coverage
No_cov<-subset(data, data$mental.health.coverage == "No"|data$mental.health.coverage == "Yes"|data$mental.health.coverage == "Not eligible for coverage / N/A"|data$mental.health.coverage == "I don't know")
ggplot(No_cov, aes(x = mental.health.coverage,y=percentage, fill=mental.health.coverage)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  #scale_fill_brewer(palette = "Reds")+
  #scale_colour_manual(values = rev(palette("Reds")))+
  scale_fill_brewer(palette="Reds")+
  #ggtitle("% of respondents unsure discussing mental health issues if their anonymity was not protected at previous workplace")+
  xlab("Mental Health Coverage")+
  theme(legend.position= "bottom" )

      
      
#Anonymity protected
anom.data<-subset(data, data$prev.anonymity.protected=='I don\'t know' | data$prev.anonymity.protected=='No' | data$prev.anonymity.protected== 'Sometimes' | data$prev.anonymity.protected=='Yes, always')
ggplot(anom.data, aes(x = prev.anonymity.protected,y=percentage, fill=prev.anonymity.protected)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  #scale_fill_brewer(palette = "Reds")+
  #scale_colour_manual(values = rev(palette("Reds")))+
  scale_fill_brewer(palette="Reds", direction=-1)+
  ggtitle("% of respondents unsure discussing mental health issues if their anonymity was not protected at previous workplace")+
  xlab("Previous anonymity protected")+
  theme(legend.position= "bottom" )

#current mental disorder
ggplot(data, aes(x = current.mental.disorder,y=percentage, fill=current.mental.disorder)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  #scale_fill_brewer(palette = "Reds")+
  #scale_colour_manual(values = rev(palette("Reds")))+
  scale_fill_brewer(palette="Reds")+
  #ggtitle("% of respondents unsure discussing mental health issues if their anonymity was not protected at previous workplace")+
  xlab("Current Mental Disorder")+
  theme(legend.position= "bottom")
