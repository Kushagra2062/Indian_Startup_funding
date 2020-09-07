
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plyr)
library(DT)
library(scales)
options(scipen=10000)


funding <- read.csv("D:/RMIT Semester 2/Data Visualization/Assignment 3/startup_funding.csv")
summary(funding)
sum(is.na(funding$Remarks))
startup_funding <- select(funding,-1)

# Visualisation by city Locations

a <- table(startup_funding$CityLocation)
a2 <- as.data.frame(a)

sum(is.na(a2$Var1))


a2$Var1 <- as.character(a2$Var1)
a2$Var1[a2$Var1==""] <- "CitynotPublished"


a2$Var1[is.na(a2$Var1)] <- "CitynotPublished"

a2 <- a2 %>% arrange(desc(Freq))
a2$Percentage=(a2$Freq/sum(a2$Freq))*100



city_location <- ggplot(data = head(a2,10),aes(x=reorder(Var1,Freq),y=Freq))+ geom_bar(stat = "identity",fill="yellow",color="black")+
  geom_text(aes(label=round(Percentage,3)), vjust=0.5, color="black", size=4.5,hjust=0.5)+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+coord_flip()

city_location+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+scale_y_continuous(breaks = c(0,100,200,300,400,500,600))+
  labs(title = "Citywise Preferences of Startups",x="City Locations",y="Total number of startups",subtitle = "Barplot of Locations versus number of startups alongside their percentage")+
  theme(plot.title = element_text(size=20,colour = "black",face = "bold"))+
  theme(axis.text = element_text(size = 12,face="italic",color="#595c61"))+
  theme(axis.title = element_text(size=14),panel.background = element_rect(color = "white",fill="#a4e7ed"))

# Visualize by their Industry Vertical

iv <- table(startup_funding$IndustryVertical)
iv2 <- as.data.frame(iv)

sum(is.na(iv2$Freq))

iv2 <- iv2 %>% arrange(desc(Freq))
iv2$Var1[iv2$Var1=="ECommerce"] <- "eCommerce"

iv2[4,] <- iv2[4,]+iv2[5,]
iv2 <- iv2[rownames(iv2) != 5,]
iv2$Var1 <- as.character(iv2$Var1)

iv2$Var1[is.na(iv2$Var1)] <- "E Commerce"
iv2$Var1[iv2$Var1==""] <- "Not Published"

iv2$Percentage=(iv2$Freq/sum(iv2$Freq))*100
iv2 <- head(iv2,10)

ggplot(data = iv2,aes(x=reorder(Var1,Freq),y=Freq))+ geom_bar(stat = "identity",fill="green")+
  geom_text(aes(label=round(Percentage,3)),hjust=0.5, vjust=0.5, color="black", size=4.5)+theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+coord_flip()+
  labs(title = "Number of Startups and their primary Industry type",x="Industry type",y="Number of startups",subtitle = "Percentage proportion shown in each bar")+ theme(plot.title = element_text(size=16,colour = "black",face = "bold"))+
  theme(axis.text = element_text(size = 12,face="italic",color="#595c61"))+
  theme(axis.title = element_text(size=13,face = "bold"),panel.background = element_rect(color = "white"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  


# Most Funding received

startup_funding2 <- startup_funding %>% drop_na(AmountInUSD)

startup_funding2$StartupName <- sapply(strsplit(startup_funding2$StartupName, split='.com', fixed=TRUE), function(x) (x[1]))
startup_funding2$StartupName <- sapply(strsplit(startup_funding2$StartupName, split='.in', fixed=TRUE), function(x) (x[1]))
startup_funding2$StartupName <- sapply(strsplit(startup_funding2$StartupName, split='.co', fixed=TRUE), function(x) (x[1]))
startup_funding2$StartupName <- tolower(startup_funding2$StartupName)


startup_funding2$StartupName[startup_funding2$StartupName == "olacabs"] <- "ola"
startup_funding2$StartupName[startup_funding2$StartupName == "oyo"] <- "oyo rooms"
startup_funding2$StartupName[startup_funding2$StartupName == "oyorooms"] <- "oyo rooms"
startup_funding2$StartupName[startup_funding2$StartupName == "Flipkart.com"] <- "Flipkart"


startup_funding$AmountInUSD=as.numeric(gsub(",","",startup_funding$AmountInUSD))

x2 <- aggregate(startup_funding$AmountInUSD,by=list(Startups=startup_funding$StartupName),FUN=sum)
x2 <- as.data.frame(x2)
x2$Startups[x2$Startups=="Flipkart.com"] <- "Flipkart"
x2$x[568] <- x2$x[568]+x2$x[569]
x2 <- x2[rownames(x2)!=569,]

x2$Startups[x2$Startups=="Olacabs"] <- "Ola"
x2$x[1239] <- x2$x[1239]+x2$x[1241]
x2 <- x2[rownames(x2)!=1241,]

x3 <- na.omit(x2)

x3 <- x3 %>% arrange(desc(x))

tot1 <- ggplot(head(x3,15),aes(x=reorder(Startups,-x),y=x))+geom_bar(stat = "identity",fill="#bc5edb")+
  scale_y_continuous(labels = unit_format(unit = "B",scale=1e-9))+labs(title = "Highest total Investments received by startups",x="Startup names",y="Investment received(US Dollars)")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

 tot1+ theme(plot.title = element_text(size=18,color="black",face = "bold"),axis.title = element_text(size = 14),
             axis.text = element_text(size = 12,face = "italic"))
# Number of times that the startup received funding

startups_received <- table(startup_funding$StartupName)
f_received <- as.data.frame(startups_received)

f_received <- f_received %>% arrange(desc(Freq))

plt1 <- ggplot(head(f_received,15),aes(x=reorder(Var1,-Freq),y=Freq,fill=Freq))+geom_bar(stat = "identity",color="black")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+labs(title = "Most Fundings received by Startups",x="Startups"
                                                                           ,y="Number of fundings received")

plt1+theme(panel.grid.major = element_blank())+scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8))+
  theme(axis.text = element_text(size=12,face = "italic"))+ theme(plot.title = element_text(size = 20,color = "black"))
