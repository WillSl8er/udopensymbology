#index
#install packages(only need to intsall them the first time use it)
#call library(first step everytime)
#read in csv data(second step everytime)

#Q1:analysis the data by qrt&yrCompleted
#Q1(1)Line Chart-completed for each quarter
#Q1(2)Bar chart-complete and not Completed

#Q2:analysis the data by region,numbers of client per region
#Q2(1)Bar Chart
#Q2(2)Pie Chart

#Q3:Each country in one Region
#Q3(1)Bar chart(Asia Pacific)
#Q3(2)Bar chart(Eastern Europe)
#Q3(3)Bar chart(Latin America & Caribbean)
#Q3(4)Bar chart(Middle East & Africa)
#Q3(5)Bar chart(North America)
#Q3(6)Bar chart(Western Europe)

#Q4 State in USA
#Q4(1)Bar chart

#Q5 Accepted FIGI?
#Q5(1)Bar chart
#Q5(2)Pie Chart

#Q5(3)Accepted FIGI per Region-bar chart-1
#Q5(3)Accepted FIGI per Region-bar chart-2
#Q5(3)Accepted FIGI per Region-pie chart-1
#Q5(3)Accepted FIGI per Region-pie chart-2

#Q5(4)Accepted FIGI per Client Type-bar chart-1
#Q5(4)Accepted FIGI per Client Type-bar chart-2
#Q5(4)Accepted FIGI per Client Type-pie chart-1
#Q5(4)Accepted FIGI per Client Type-pie chart-2

#Q5(5)Accepted FIGI per Using FIGI category-bar chart-1
#Q5(5)Accepted FIGI per Using FIGI category-bar chart-2
#Q5(5)Accepted FIGI per Using FIGI category-pie chart-1
#Q5(5)Accepted FIGI per Using FIGI category-pie chart-2

#Q5(6)Relationship of Accepted FIGI and Using FIGI(numerical)-plot chart

#6 Using FIGI?
#Q6(1)Using FIGI-bar chart
#Q6(1)Using FIGI-pie chart

#Q6(2)Using FIGI per Region-bar chart-1
#Q6(2)Using FIGI per Region-bar chart-2
#Q6(2)Using FIGI per Region-pie chart-1
#Q6(2)Using FIGI per Region-pie chart-2

#Q6(3)Using FIGI per Client Type-bar chart-1
#Q6(3)Using FIGI per Client Type-bar chart-2
#Q6(3)Using FIGI per Client Type-pie chart-1
#Q6(3)Using FIGI per Client Type-pie chart-2

#Q7 Bloomberg Product
#Q7(1)Bloomberg Product-bar chart
#Q7(1)Bloomberg Product-pie chart
#Q7(1)Bloomberg Product-pie chart(except "--")
#Q7(2)Bloomberg Product per Client Type-plot chart
#Q7(2)Bloomberg Product per Client Type-bar chart-1
#Q7(2)Bloomberg Product per Client Type-bar chart-2
#Q7(2)Bloomberg Product per Client Type-pie chart-1
#Q7(2)Bloomberg Product per Client Type-pie chart-1(Except"--")
#Q7(2)Bloomberg Product per Client Type-pie chart-2
#Q7(2)Bloomberg Product per Client Type-pie chart-2(Except"--")

#Q8 Use Case
#Q8(1)Use case-bar chart
#Q8(1)Use case-bar chart(Except"--")
#Q8(2)Use case-pie chart
#Q8(2)Use case-pie chart(Except"--")
#Q8(3)Use Case per Client Type-bar chart-1
#Q8(3)Use Case per Client Type-bar chart-1(Except ClientType="--")
#Q8(3)Use Case per Client Type-bar chart-1(Except UseCase1="--")
#Q8(3)Use Case per Client Type-bar chart-2
#Q8(3)Use Case per Client Type-bar chart-2(Except UseCase="--")
#Q8(3)Use Case per Client Type-pie chart-1
#Q8(3)Use Case per Client Type-pie chart-1(except usecase="--")
#Q8(3)Use Case per Client Type-pie chart-2
#Q8(3)Use Case per Client Type-pie chart-2(Except usecase="--")














#install packages(only need to intsall them the first time use it)
install.packages("dplyr",dependencies=TRUE)
install.packages("tidyr",dependencies=TRUE)
install.packages("ggplot2",dependencies=TRUE)
install.packages("nycflights13",dependencies=TRUE)
install.packages("lubridate",dependencies=TRUE)
install.packages("ggthemes",dependencies=TRUE)
install.packages("gridExtra",dependencies=TRUE)
install.packages("e1071",dependencies=TRUE)
install.packages("pROC",dependencies=TRUE)
install.packages("fitdistrplus",dependencies=TRUE)
install.packages("png",dependencies=TRUE)
install.packages("scales",dependencies=TRUE)





#call library(first step everytime)
library("dplyr")
library("ggplot2")
library("tidyr")
library("ggthemes")
library("gridExtra")
library("nycflights13")
library("lubridate")

#read in csv data(second step everytime)
#change the name of the data file to "clientdata.csv"
#and put the data file in the same file with this R file
DataDF = read.csv("clientdata.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "NULL") %>% 
  tbl_df()




#Q1:analysis the data by qrt&yrCompleted
DataDF1=DataDF%>%group_by(QtrYrcompleted)%>%summarise(Number=n())%>%
  separate(QtrYrcompleted, c("Qtr", "Year"), " ")%>%unite(YrQrt,Year:Qtr, sep='')


#Q1(1)Line Chart-completed for each quarter
DataDF1=DataDF%>%group_by(QtrYrcompleted)%>%summarise(Number=n())%>%
  separate(QtrYrcompleted, c("Qtr", "Year"), " ")%>%filter(Qtr!="Not")%>%
   unite(YrQrt,Year:Qtr, sep='')
ggplot(DataDF1, aes(x=YrQrt,y=Number,group="")) + 
  geom_point(stat="identity")+geom_line()+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("Year and Quarter Completed") + ylab("Number of client") + ggtitle("Number of client completed per Quarter") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))
#Q1(2)Bar chart-complete and not Completed
DataDF1=DataDF%>%group_by(QtrYrcompleted)%>%summarise(Number=n())%>%
  separate(QtrYrcompleted, c("Qtr", "Year"), " ")%>%
  unite(YrQrt,Year:Qtr, sep='')
ggplot(DataDF1, aes(x=YrQrt,y=Number,group="")) + 
  geom_bar(stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("Year and Quarter Completed") + ylab("Number of client") + ggtitle("Number of client completed per Quarter") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q2:analysis the data by region,numbers of client per region
DataDF2=DataDF%>%group_by(Region)%>%summarise(Number=n())


#Q2(1)Bar chart
DataDF2=DataDF%>%group_by(Region)%>%summarise(Number=n())
ggplot(DataDF2, aes(x=reorder(Region,Number),y=Number)) + 
      geom_bar(position ="dodge",stat="identity")+
      geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
      xlab("Region") + ylab("Number per Region") + ggtitle("Number of client per Region") +
      theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q2(2)Pie Chart
DataDF2=DataDF%>%select(Region)%>%group_by(Region)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF2, aes(x=factor(1),y=Number,fill=factor(Region))) + 
  geom_bar(stat="identity")+
  xlab("") + ylab("Number per Region") + labs(fill='Region')+
  geom_text(aes(x= factor(1), y=sum(Number)-pos, label = Number), size=6)+
  ggtitle("Portion of client per Region") +coord_polar(theta = "y")+
  theme(text= element_text(size = 20),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q3:Each country in one Region
DataDF3=DataDF%>%select(Region,Country)%>%group_by(Region,Country)%>%summarise(Number=n())

#Q3(1)Bar chart(Asia Pacific)
DataDF3=DataDF%>%select(Region,Country)%>%group_by(Region,Country)%>%summarise(Number=n())%>%
filter(Region=="Asia Pacific")
ggplot(DataDF3, aes(x=reorder(Country,Number),y=Number)) + 
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("Country") + ylab("Number per Country for Asia Pacific") + ggtitle("Number of client per Country of Asian Pacific") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q3(2)Bar chart(Eastern Europe)
DataDF3=DataDF%>%select(Region,Country)%>%group_by(Region,Country)%>%summarise(Number=n())%>%
  filter(Region=="Eastern Europe")
ggplot(DataDF3, aes(x=reorder(Country,Number),y=Number)) + 
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("Country") + ylab("Number per Country for Eastern Europe") + ggtitle("Number of client per Country of Eastern Europe") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q3(3)Bar chart(Latin America & Caribbean)
DataDF3=DataDF%>%select(Region,Country)%>%group_by(Region,Country)%>%summarise(Number=n())%>%
  filter(Region=="Latin America & Caribbean")
ggplot(DataDF3, aes(x=reorder(Country,Number),y=Number)) + 
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("Country") + ylab("Number per Country for Latin America & Caribbean") + ggtitle("Number of client per Country of Latin America & Caribbean") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q3(4)Bar chart(Middle East & Africa)
DataDF3=DataDF%>%select(Region,Country)%>%group_by(Region,Country)%>%summarise(Number=n())%>%
  filter(Region=="Middle East & Africa")
ggplot(DataDF3, aes(x=reorder(Country,Number),y=Number)) + 
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("Country") + ylab("Number per Country for Middle East & Africa") + ggtitle("Number of client per Country of Middle East & Africa") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q3(5)Bar chart(North America)
DataDF3=DataDF%>%select(Region,Country)%>%group_by(Region,Country)%>%summarise(Number=n())%>%
  filter(Region=="North America")
ggplot(DataDF3, aes(x=reorder(Country,Number),y=Number)) + 
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("Country") + ylab("Number per Country for North America") + ggtitle("Number of client per Country of North America") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q3(6)Bar chart(Western Europe)
DataDF3=DataDF%>%select(Region,Country)%>%group_by(Region,Country)%>%summarise(Number=n())%>%
  filter(Region=="Western Europe")
ggplot(DataDF3, aes(x=reorder(Country,Number),y=Number)) + 
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("Country") + ylab("Number per Country for Western Europe") + ggtitle("Number of client per Country of Western Europe") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q4 State in USA
DataDF4=DataDF%>%select(State,Country)%>%filter(Country=="United States of America (USA)")%>%
       group_by(State)%>%summarise(Number=n())
#Q4(1)Bar chart
DataDF4=DataDF%>%select(State,Country)%>%filter(Country=="United States of America (USA)")%>%
  group_by(State)%>%summarise(Number=n())
ggplot(DataDF4, aes(x=reorder(State,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("State") + ylab("Number per State") + ggtitle("Number of client per State in U.S.A") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5 Accepted FIGI?
DataDF5=DataDF%>%select(AcceptedFIGI)%>%group_by(AcceptedFIGI)%>%summarise(Number=n())

#Q5(1)Bar chart
DataDF5=DataDF%>%select(AcceptedFIGI)%>%group_by(AcceptedFIGI)%>%summarise(Number=n())
ggplot(DataDF5, aes(x=reorder(AcceptedFIGI,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("Accepted FIGI?") + ylab("Number per category") + ggtitle("Number of client per Category of whether accepted FIGI") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q5(2)Pie Chart
DataDF5=DataDF%>%select(AcceptedFIGI)%>%group_by(AcceptedFIGI)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=factor(AcceptedFIGI))) + 
  geom_bar(stat="identity")+
  xlab("") + ylab("Number per category") + labs(fill='Accepted FIGI')+
  geom_text(aes(x= factor(1), y=sum(Number)-pos, label = Number), size=6)+
  ggtitle("Portion of client per Category of whether accepted FIGI") +coord_polar(theta = "y")+
  theme(text= element_text(size = 20),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q5(3)Accepted FIGI per Region-bar chart-1
DataDF5=DataDF%>%select(Region,AcceptedFIGI)%>%group_by(Region,AcceptedFIGI)%>%summarise(Number=n())
ggplot(DataDF5, aes(x=reorder(Region,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=AcceptedFIGI))+
  xlab("Region") + ylab("Number per category") + ggtitle("Number of client per Category of whether accepted FIGI for each Region") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(3)Accepted FIGI per Region-bar chart-2
DataDF5=DataDF%>%select(AcceptedFIGI,Region)%>%group_by(AcceptedFIGI,Region)%>%summarise(Number=n())
ggplot(DataDF5, aes(x=reorder(AcceptedFIGI,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=Region))+
  xlab("Accepted FIGI?") + ylab("Number per category") + ggtitle("Number of client per Category of whether accepted FIGI for each Region") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q5(3)Accepted FIGI per Region-pie chart-1
DataDF5=DataDF%>%select(Region,AcceptedFIGI)%>%group_by(Region,AcceptedFIGI)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=AcceptedFIGI))+ 
  geom_bar(position="stack",stat="identity")+facet_wrap(~Region)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per region") + ggtitle("Number of client per Category of whether accepted FIGI for each Region") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(3)Accepted FIGI per Region-pie chart-2
DataDF5=DataDF%>%select(AcceptedFIGI,Region)%>%group_by(AcceptedFIGI,Region)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=Region))+ 
  geom_bar(position="stack",stat="identity")+facet_wrap(~AcceptedFIGI)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of whether accepted FIGI for each Region") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))



#Q5(4)Accepted FIGI per Client Type-bar chart-1
DataDF5=DataDF%>%select(ClientType,AcceptedFIGI)%>%group_by(ClientType,AcceptedFIGI)%>%summarise(Number=n())
ggplot(DataDF5, aes(x=reorder(ClientType,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=AcceptedFIGI))+
  xlab("Client Type") + ylab("Number per category") + ggtitle("Number of client per Category of whether accepted FIGI for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(4)Accepted FIGI per Client Type-bar chart-2
DataDF5=DataDF%>%select(AcceptedFIGI,ClientType)%>%group_by(AcceptedFIGI,ClientType)%>%summarise(Number=n())
ggplot(DataDF5, aes(x=reorder(AcceptedFIGI,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=ClientType))+
  xlab("Accepted FIGI?") + ylab("Number per category") + ggtitle("Number of client per Category of whether accepted FIGI for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(4)Accepted FIGI per Client Type-pie chart-1
DataDF5=DataDF%>%select(ClientType,AcceptedFIGI)%>%group_by(ClientType,AcceptedFIGI)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=AcceptedFIGI)) + 
  geom_bar(position="stack",stat="identity")+facet_wrap(~ClientType)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of whether accepted FIGI for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(4)Accepted FIGI per Client Type-pie chart-2
DataDF5=DataDF%>%select(AcceptedFIGI,ClientType)%>%group_by(AcceptedFIGI,ClientType)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=ClientType)) + 
  geom_bar(position="stack",stat="identity")+facet_wrap(~AcceptedFIGI)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of whether accepted FIGI for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q5(5)Accepted FIGI per Using FIGI category-bar chart-1
DataDF5=DataDF%>%select(AcceptedFIGI,UsingFIGI)%>%
  group_by(AcceptedFIGI,UsingFIGI)%>%summarise(Number=n())
ggplot(DataDF5,aes(x=AcceptedFIGI,y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+facet_grid(.~UsingFIGI)+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("") + ylab("Number of each category") + ggtitle("Accepted FIGI per Using FIGI") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(5)Accepted FIGI per Using FIGI category-bar chart-2
DataDF5=DataDF%>%select(UsingFIGI,AcceptedFIGI)%>%
  group_by(UsingFIGI,AcceptedFIGI)%>%summarise(Number=n())
ggplot(DataDF5,aes(x=UsingFIGI,y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+facet_grid(.~AcceptedFIGI)+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("") + ylab("Number of each category") + ggtitle("Using FIGI per Accepted FIGI") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(5)Accepted FIGI per Using FIGI category-pie chart-1
DataDF5=DataDF%>%select(AcceptedFIGI,UsingFIGI)%>%
  group_by(AcceptedFIGI,UsingFIGI)%>%summarise(Number=n())
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=AcceptedFIGI)) + 
  geom_bar(position="stack",stat="identity")+facet_grid(.~UsingFIGI)+coord_polar(theta = "y")+
  xlab("") + ylab("Number of each category") + ggtitle("Accepted FIGI per Using FIGI") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(5)Accepted FIGI per Using FIGI category-pie chart-2
DataDF5=DataDF%>%select(UsingFIGI,AcceptedFIGI)%>%
  group_by(UsingFIGI,AcceptedFIGI)%>%summarise(Number=n())
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=UsingFIGI)) + 
  geom_bar(position="stack",stat="identity")+facet_grid(.~AcceptedFIGI)+coord_polar(theta = "y")+
  xlab("") + ylab("Number of each category") + ggtitle("Using FIGI per Accepted FIGI") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(6)Relationship of Accepted FIGI and Using FIGI?(numerical)-plot chart
DataDF5=DataDF%>%select(UsingFIGI,AcceptedFIGI)%>%
  group_by(UsingFIGI,AcceptedFIGI)%>%summarise(Number=n())
ggplot(DataDF5,aes(x=UsingFIGI,y=AcceptedFIGI)) + 
  geom_point(aes(size=Number),stat="identity")+
  geom_text(aes(label=Number),hjust=3, vjust=-0.5 )+scale_size(range = c(4, 40))+
  xlab("Using FIGI?") + ylab("Accepted FIGI?") + ggtitle("Number of client according to Using FIGI and Accepted FIGI") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6Using FIGI
DataDF6=DataDF%>%select(UsingFIGI)%>%
  group_by(UsingFIGI)%>%summarise(Number=n())

#Q6(1)Using FIGI-bar chart
DataDF6=DataDF%>%select(UsingFIGI)%>%
  group_by(UsingFIGI)%>%summarise(Number=n())
ggplot(DataDF6,aes(x=UsingFIGI,y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("Using FIGI?") + ylab("Number of each category") + ggtitle("Number of client whether Using FIGI") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(1)Using FIGI-pie chart
DataDF6=DataDF%>%select(UsingFIGI)%>%
  group_by(UsingFIGI)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF6,aes(x=factor(1),y=Number,fill=UsingFIGI)) + 
  geom_bar(position ="stack",stat="identity")+coord_polar(theta = "y")+
  geom_text(aes(x= factor(1), y=sum(Number)-pos, label = Number),size=6 )+
  xlab("") + ylab("Number of each category") + ggtitle("Number of client whether Using FIGI") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(2)Using FIGI per Region-bar chart-1
DataDF6=DataDF%>%select(Region,UsingFIGI)%>%group_by(Region,UsingFIGI)%>%summarise(Number=n())
ggplot(DataDF6, aes(x=reorder(Region,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=UsingFIGI))+
  xlab("Region") + ylab("Number per category") + ggtitle("Number of client per Category of whether using FIGI for each Region") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(2)Using FIGI per Region-bar chart-2
DataDF6=DataDF%>%select(UsingFIGI,Region)%>%group_by(UsingFIGI,Region)%>%summarise(Number=n())
ggplot(DataDF6, aes(x=reorder(UsingFIGI,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=Region))+
  xlab("Using FIGI?") + ylab("Number per category") + ggtitle("Number of client per Category of whether using FIGI for each Region") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q6(2)Using FIGI per Region-pie chart-1
DataDF6=DataDF%>%select(Region,UsingFIGI)%>%group_by(Region,UsingFIGI)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF6, aes(x=factor(1),y=Number,fill=UsingFIGI))+ 
  geom_bar(position="stack",stat="identity")+facet_wrap(~Region)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per region") + ggtitle("Number of client per Category of whether using FIGI for each Region") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(2)Using FIGI per Region-pie chart-2
DataDF6=DataDF%>%select(UsingFIGI,Region)%>%group_by(UsingFIGI,Region)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF6, aes(x=factor(1),y=Number,fill=Region))+ 
  geom_bar(position="stack",stat="identity")+facet_wrap(~UsingFIGI)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of whether using FIGI for each Region") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q6(3)Using FIGI per Client Type-bar chart-1
DataDF6=DataDF%>%select(ClientType,UsingFIGI)%>%group_by(ClientType,UsingFIGI)%>%summarise(Number=n())
ggplot(DataDF6, aes(x=reorder(ClientType,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=UsingFIGI))+
  xlab("Client Type") + ylab("Number per category") + ggtitle("Number of client per Category of whether using FIGI for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(3)Using FIGI per Client Type-bar chart-2
DataDF6=DataDF%>%select(UsingFIGI,ClientType)%>%group_by(UsingFIGI,ClientType)%>%summarise(Number=n())
ggplot(DataDF6, aes(x=reorder(UsingFIGI,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=ClientType))+
  xlab("Using FIGI?") + ylab("Number per category") + ggtitle("Number of client per Category of whether using FIGI for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(3)Using FIGI per Client Type-pie chart-1
DataDF6=DataDF%>%select(ClientType,UsingFIGI)%>%group_by(ClientType,UsingFIGI)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF6, aes(x=factor(1),y=Number,fill=UsingFIGI)) + 
  geom_bar(position="stack",stat="identity")+facet_wrap(~ClientType)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of whether using FIGI for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(3)Using FIGI per Client Type-pie chart-2
DataDF6=DataDF%>%select(UsingFIGI,ClientType)%>%group_by(UsingFIGI,ClientType)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF6, aes(x=factor(1),y=Number,fill=ClientType)) + 
  geom_bar(position="stack",stat="identity")+facet_wrap(~UsingFIGI)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of whether using FIGI for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q7 Bloomberg Product
DataDF7=DataDF%>%select(BloombergProduct)%>%group_by(BloombergProduct)%>%summarise(Number=n())

#Q7(1)Bloomberg Product-bar chart
DataDF7=DataDF%>%select(BloombergProduct)%>%group_by(BloombergProduct)%>%summarise(Number=n())
ggplot(DataDF7, aes(x=reorder(BloombergProduct,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("Bloomberg Product") + ylab("Number per category") + ggtitle("Number of client per Category of Bloomberg Product") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q7(1)Bloomberg Product-pie chart
DataDF7=DataDF%>%select(BloombergProduct)%>%group_by(BloombergProduct)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF7, aes(x=factor(1),y=Number,fill=factor(BloombergProduct))) + 
  geom_bar(position="stack",stat="identity")+
  xlab("") + ylab("Number per Bloomberg Product") + labs(fill='Bloomberg Product')+
  geom_text(aes(x= factor(1), y=sum(Number)-pos, label = Number), size=3)+
  ggtitle("Portion of client per Bloomberg Product") +coord_polar(theta = "y")+
  theme(text= element_text(size = 20),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q7(1)Bloomberg Product-pie chart(except "--")
DataDF7=DataDF%>%select(BloombergProduct)%>%filter(BloombergProduct!="--")%>%group_by(BloombergProduct)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF7, aes(x=factor(1),y=Number,fill=factor(BloombergProduct))) + 
  geom_bar(position="stack",stat="identity")+
  xlab("") + ylab("Number per Bloomberg Product") + labs(fill='Bloomberg Product')+
  geom_text(aes(x= factor(1), y=sum(Number)-pos, label = Number), size=3)+
  ggtitle("Portion of client per Bloomberg Product") +coord_polar(theta = "y")+
  theme(text= element_text(size = 20),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q7(2)Bloomberg Product per Client Type-plot chart
DataDF7=DataDF%>%select(BloombergProduct,ClientType)%>%group_by(BloombergProduct,ClientType)%>%summarise(Number=n())
ggplot(DataDF7,aes(x=BloombergProduct,y=ClientType)) + 
  geom_point(aes(size=Number),stat="identity")+
  geom_text(aes(label=Number),hjust=0, vjust=3.5 )+scale_size(range = c(4, 20))+
  xlab("Bloomberg Product") + ylab("Client Type") + ggtitle("Number of client according to Bloomberg Product and Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q7(2)Bloomberg Product per Client Type-bar chart-1
DataDF7=DataDF%>%select(BloombergProduct,ClientType)%>%
  group_by(BloombergProduct,ClientType)%>%summarise(Number=n())
ggplot(DataDF7,aes(x=BloombergProduct,y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+facet_grid(.~ClientType)+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("") + ylab("Number of each category") + ggtitle("Bloomberg Product per Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q7(2)Bloomberg Product per Client Type-bar chart-2
DataDF7=DataDF%>%select(ClientType,BloombergProduct)%>%
  group_by(ClientType,BloombergProduct)%>%summarise(Number=n())
ggplot(DataDF7,aes(x=ClientType,y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+facet_wrap(~BloombergProduct)+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("") + ylab("Number of each category") + ggtitle("Bloomberg Product per Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q7(2)Bloomberg Product per Client Type-pie chart-1
DataDF7=DataDF%>%select(BloombergProduct,ClientType)%>%
  group_by(BloombergProduct,ClientType)%>%summarise(Number=n())%>%mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF7, aes(x=factor(1),y=Number,fill=ClientType))+ 
  geom_bar(position="stack",stat="identity")+facet_wrap(~BloombergProduct)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per Bloomberg Product") + ggtitle("Number of client per Bloomberg Product of each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q7(2)Bloomberg Product per Client Type-pie chart-1(Except"--")
DataDF7=DataDF%>%select(BloombergProduct,ClientType)%>%filter(ClientType!="--",BloombergProduct!="--")%>%
  group_by(BloombergProduct,ClientType)%>%summarise(Number=n())%>%mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF7, aes(x=factor(1),y=Number,fill=ClientType))+ 
  geom_text(aes(x= factor(1), y=pos, label = Number), size=5)+
  geom_bar(position="stack",stat="identity")+facet_wrap(~BloombergProduct)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per Bloomberg Product") + ggtitle("Number of client per Bloomberg Product of each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q7(2)Bloomberg Product per Client Type-pie chart-2
DataDF7=DataDF%>%select(ClientType,BloombergProduct)%>%
  group_by(ClientType,BloombergProduct)%>%summarise(Number=n())%>%mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF7, aes(x=factor(1),y=Number,fill=BloombergProduct))+ 
  geom_bar(position="stack",stat="identity")+facet_wrap(~ClientType)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per Bloomberg Product") + ggtitle("Number of client per Bloomberg Product of each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q7(2)Bloomberg Product per Client Type-pie chart-2(Except"--")
DataDF7=DataDF%>%select(ClientType,BloombergProduct)%>%filter(ClientType!="--",BloombergProduct!="--")%>%
  group_by(ClientType,BloombergProduct)%>%summarise(Number=n())%>%mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF7, aes(x=factor(1),y=Number,fill=BloombergProduct))+ 
  geom_text(aes(x= factor(1), y=pos, label = Number), size=5)+
  geom_bar(position="stack",stat="identity")+facet_wrap(~ClientType)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per Bloomberg Product") + ggtitle("Number of client per Bloomberg Product of each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q8 Use Case
DataDF8=DataDF%>%select(UseCase1)%>%group_by(UseCase1)%>%summarise(Number=n())

#Q8(1)Use case-bar chart
DataDF8=DataDF%>%select(UseCase1)%>%group_by(UseCase1)%>%summarise(Number=n())
ggplot(DataDF8,aes(x=UseCase1,y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("") + ylab("Number of each category") + ggtitle("Number of client per Use Case") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q8(1)Use case-bar chart(Except"--")
DataDF8=DataDF%>%select(UseCase1)%>%filter(UseCase1!="--")%>%group_by(UseCase1)%>%summarise(Number=n())
ggplot(DataDF8,aes(x=reorder(UseCase1,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("") + ylab("Number of each category") + ggtitle("Number of client per Use Case") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q8(2)Use case-pie chart
DataDF8=DataDF%>%select(UseCase1)%>%group_by(UseCase1)%>%summarise(Number=n())%>%
mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF8, aes(x=factor(1),y=Number,fill=factor(UseCase1))) + 
  geom_bar(position="stack",stat="identity")+
  xlab("") + ylab("Number per Use Case") + labs(fill='Use Case')+
  geom_text(aes(x= factor(1), y=sum(Number)-pos, label = Number), size=6)+
  ggtitle("Portion of client per Use Case") +coord_polar(theta = "y")+
  theme(text= element_text(size = 20),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q8(2)Use case-pie chart(Except"--")
DataDF8=DataDF%>%select(UseCase1)%>%filter(UseCase1!="--")%>%group_by(UseCase1)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF8, aes(x=factor(1),y=Number,fill=factor(UseCase1))) + 
  geom_bar(position="stack",stat="identity")+
  xlab("") + ylab("Number per Use Case") + labs(fill='Use Case')+
  geom_text(aes(x= factor(1), y=sum(Number)-pos, label = Number), size=6)+
  ggtitle("Portion of client per Use Case") +coord_polar(theta = "y")+
  theme(text= element_text(size = 20),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q8(3)Use Case per Client Type-bar chart-1
DataDF8=DataDF%>%select(ClientType,UseCase1)%>%group_by(ClientType,UseCase1)%>%summarise(Number=n())
ggplot(DataDF8, aes(x=reorder(ClientType,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=UseCase1))+
  xlab("Client Type") + ylab("Number per category") + ggtitle("Number of client per Category of Use Case for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q8(3)Use Case per Client Type-bar chart-1(Except ClientType="--")
DataDF8=DataDF%>%select(ClientType,UseCase1)%>%filter(ClientType!="--")%>%group_by(ClientType,UseCase1)%>%summarise(Number=n())
ggplot(DataDF8, aes(x=reorder(ClientType,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=UseCase1))+
  xlab("Client Type") + ylab("Number per category") + ggtitle("Number of client per Category of Use Case for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q8(3)Use Case per Client Type-bar chart-1(Except UseCase1="--")
DataDF8=DataDF%>%select(ClientType,UseCase1)%>%filter(UseCase1!="--")%>%group_by(ClientType,UseCase1)%>%summarise(Number=n())
ggplot(DataDF8, aes(x=reorder(ClientType,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=UseCase1))+
  xlab("Client Type") + ylab("Number per category") + ggtitle("Number of client per Category of Use Case for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q8(3)Use Case per Client Type-bar chart-2
DataDF8=DataDF%>%select(UseCase1,ClientType)%>%group_by(UseCase1,ClientType)%>%summarise(Number=n())
ggplot(DataDF8, aes(x=reorder(UseCase1,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=ClientType))+
  xlab("Use Case") + ylab("Number per category") + ggtitle("Number of client per Category of Use Case for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q8(3)Use Case per Client Type-bar chart-2(Except UseCase="--")
DataDF8=DataDF%>%select(UseCase1,ClientType)%>%filter(UseCase1!="--")%>%group_by(UseCase1,ClientType)%>%summarise(Number=n())
ggplot(DataDF8, aes(x=reorder(UseCase1,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=ClientType))+
  xlab("Use Case") + ylab("Number per category") + ggtitle("Number of client per Category of Use Case for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q8(3)Use Case per Client Type-pie chart-1
DataDF8=DataDF%>%select(ClientType,UseCase1)%>%group_by(ClientType,UseCase1)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF8, aes(x=factor(1),y=Number,fill=UseCase1)) + 
  geom_bar(position="stack",stat="identity")+facet_wrap(~ClientType)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of Use Case for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q8(3)Use Case per Client Type-pie chart-1(except usecase="--")
DataDF8=DataDF%>%select(ClientType,UseCase1)%>%filter(UseCase1!="--")%>%group_by(ClientType,UseCase1)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF8, aes(x=factor(1),y=Number,fill=UseCase1)) + 
  geom_bar(position="stack",stat="identity")+facet_wrap(~ClientType)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of Use Case for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q8(3)Use Case per Client Type-pie chart-2
DataDF8=DataDF%>%select(UseCase1,ClientType)%>%group_by(UseCase1,ClientType)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF8, aes(x=factor(1),y=Number,fill=ClientType)) + 
  geom_bar(position="stack",stat="identity")+facet_wrap(~UseCase1)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of Use Case for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q8(3)Use Case per Client Type-pie chart-2(Except usecase="--")
DataDF8=DataDF%>%select(UseCase1,ClientType)%>%filter(UseCase1!="--")%>%group_by(UseCase1,ClientType)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF8, aes(x=factor(1),y=Number,fill=ClientType)) + 
  geom_bar(position="stack",stat="identity")+facet_wrap(~UseCase1)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of Use Case for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

