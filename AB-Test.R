library(dplyr)
library(ggplot2)
Data<-read.csv("C:/Users/thiru/OneDrive/Desktop/AB_Test/ab_data.csv")
head(Data)
str(Data)
table(Data$group)
table(Data$landing_page)

Summary_Data<-Data %>% 
  group_by(group) %>% 
  summarise(Conversions=sum(converted),Visitors=n())
Summary_Data

Conversion_Rate<-Data %>% 
  group_by(group) %>%
  summarise(
    Visitors=n(),
    Conversion=sum(converted),
    Conversion_Rate=(Conversion/Visitors)*100
  )
Conversion_Rate  

prop.test(x=Summary_Data$Conversions,n=Summary_Data$Visitors)

CR_Control<-Summary_Data$Conversions[Summary_Data$group=="control"]/
  Summary_Data$Visitors[Summary_Data$group=="control"]
CR_Treatment<-Summary_Data$Conversions[Summary_Data$group=="treatment"]/
  Summary_Data$Visitors[Summary_Data$group=="treatment"]

lift<-(CR_Treatment - CR_Control)/CR_Control *100
lift

ggplot(Conversion_Rate,aes(x=group,y=Conversion_Rate,fill = group))+
  geom_col(width=0.6) +
  geom_text(aes(label = paste0(round(Conversion_Rate,2),"%")),vjust=-0.5,size=3)+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title="Conversion Rate by Group", y="Conversion Rate") +
  theme_bw()
