# comcast Telecom Consumer Complaints


cm <- read.csv(file.choose(),header=T)

View(cm)
head(cm)
str(cm)

library(lubridate)
library(ggplot2)
library(stringi)
library(ggpubr)
library(dplyr)


na_vector <- is.na(cm)
length(na_vector[na_vector==T])

cm$Date <- dmy(cm$Date)

months<- summarise(group_by(cm,Month =as.integer(month(Date))),Count = n())
daily<- summarise(group_by(cm,Date),Count =n())

ggplot(data = months,aes(Month,Count,label = Count))+
  geom_line()+
  geom_point(size = 1, color= "Red")+
  scale_x_continuous(breaks = months$Month)+
  labs(title = "Monthly Complains Count",x= "Months",y ="No. of Complains")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = daily,aes(as.POSIXct(Date),Count))+
  geom_line()+
  geom_point(size = 1)+
  scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
  labs(title = "Daily Complains Count",x= "Days",y ="No. of complains")+
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))



network<- contains(cm$Customer.Complaint,match = 'network',ignore.case = T)
internet<- contains(cm$Customer.Complaint,match = 'internet',ignore.case = T)
billing<- contains(cm$Customer.Complaint,match = 'bill',ignore.case = T)
email<- contains(cm$Customer.Complaint,match = 'email',ignore.case = T)
charges<- contains(cm$Customer.Complaint,match = 'charge',ignore.case = T)



cm$ComplaintType[internet]<- "Internet"
cm$ComplaintType[network]<- "Network"
cm$ComplaintType[billing]<- "Billing"
cm$ComplaintType[email]<- "Email"
cm$ComplaintType[charges]<- "Charges"

cm$ComplaintType[-c(internet,network,billing,charges,email)]<- "Others"

table(cm$ComplaintType)

open_complaints<- (cm$Status == "Open"| cm$Status =="Pending")
closed_complaints<-(cm$Status == "Closed"| cm$Status =="Solved")
cm$Category[ open_complaints]<-"Open" 
cm$Category[closed_complaints]<- "Closed" 

table(cm$Category)


cm<- group_by(cm,State,Category)
chart_data<- summarise(cm,Count = n())
ggplot(as.data.frame(chart_data) ,mapping = aes(State,Count))+
  geom_col(aes(fill = Category),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "#0073C2FF"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "Complains Stacked Bar Chart ",
       x = "States",y = "No of Complains",
       fill= "Status")


chart_data%>%
  filter(Category == "Open")->
  open_complaints
open_complaints[open_complaints$Count == max(open_complaints$Count),c(1,3)]




resolved_data <- group_by(cm,Category)
total_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data))) 
resolved <- group_by(cm,Received.Via,Category)
Category_resloved<- summarise(resolved,percentage =(n()/nrow(resolved))) 




par(mfrow = c(1,2))
total<-ggplot(total_resloved,
              aes(x= "",y =percentage,fill = Category))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())

cat<-ggplot(Category_resloved,
                 aes(x= "",y =percentage,fill = Category))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(Received.Via,"-",round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
ggarrange(total,cat,nrow = 1, ncol = 2)
