#index
#install packages(only need to intsall them the first time use it)
#call library(first step everytime)
#read in csv data(second step everytime)

#Q1:analysis the data by qtr_Yrcompleted
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


#Q5 User Type
#Q5(1)User Type-bar chart
#Q5(1)User Type-pie chart
#Q5(1)User Type-pie chart(except "--")
#Q5(2)User Type per Client Type-plot chart
#Q5(2)User Type per Client Type-bar chart-1
#Q5(2)User Type per Client Type-bar chart-2
#Q5(2)User Type per Client Type-pie chart-1
#Q5(2)User Type per Client Type-pie chart-1(Except"--")
#Q5(2)User Type per Client Type-pie chart-2
#Q5(2)User Type per Client Type-pie chart-2(Except"--")

#Q6 Data Usage
#Q6(1)Data Usage-bar chart
#Q6(1)Data Usage-bar chart(Except"--")
#Q6(2)Data Usage-pie chart
#Q6(2)Data Usage-pie chart(Except"--")
#Q6(3)Data Usage per Client Type-bar chart-1
#Q6(3)Data Usage per Client Type-bar chart-1(Except clienttypeName="--")
#Q6(3)Data Usage per Client Type-bar chart-1(Except datausageName="--")
#Q6(3)Data Usage per Client Type-bar chart-2
#Q6(3)Data Usage per Client Type-bar chart-2(Except datausageName="--")
#Q6(3)Data Usage per Client Type-pie chart-1
#Q6(3)Data Usage per Client Type-pie chart-1(except datausageName="--")
#Q6(3)Data Usage per Client Type-pie chart-2
#Q6(3)Data Usage per Client Type-pie chart-2(Except datausageName="--")














#install packages(only need to intsall them the first time use it)
install.packages("dplyr",dependencies=TRUE)
install.packages("tidyr",dependencies=TRUE)
install.packages("ggplot2",dependencies=TRUE)
install.packages("nycflights13",dependencies=TRUE)
install.packages("lubridate",dependencies=TRUE)
install.packages("ggthemes",dependencies=TRUE)
install.packages("gridExtra",dependencies=TRUE)
install.packages("e1051",dependencies=TRUE)
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



#Q1:analysis the data by qtr_Yrcompleted
DataDF1=DataDF%>%group_by(qtr_Yrcompleted)%>%summarise(Number=n())%>%
  separate(qtr_Yrcompleted, c("Qtr", "Year"), " ")%>%unite(YrQrt,Year:Qtr, sep='')


#Q1(1)Line Chart-completed for each quarter
DataDF1=DataDF%>%group_by(qtr_Yrcompleted)%>%summarise(Number=n())%>%
  separate(qtr_Yrcompleted, c("Qtr", "Year"), " ")%>%filter(Qtr!="Not")%>%
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
DataDF1=DataDF%>%group_by(qtr_Yrcompleted)%>%summarise(Number=n())%>%
  separate(qtr_Yrcompleted, c("Qtr", "Year"), " ")%>%
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
DataDF2=DataDF%>%group_by(regionName)%>%summarise(Number=n())


#Q2(1)Bar chart
DataDF2=DataDF%>%group_by(regionName)%>%summarise(Number=n())
ggplot(DataDF2, aes(x=reorder(regionName,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("regionName") + ylab("Number per regionName") + ggtitle("Number of client per regionName") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q2(2)Pie Chart
DataDF2=DataDF%>%select(regionName)%>%group_by(regionName)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF2, aes(x=factor(1),y=Number,fill=factor(regionName))) + 
  geom_bar(stat="identity")+
  xlab("") + ylab("Number per regionName") + labs(fill='regionName')+
  geom_text(aes(x= factor(1), y=sum(Number)-pos, label = Number), size=6)+
  ggtitle("Portion of client per regionName") +coord_polar(theta = "y")+
  theme(text= element_text(size = 20),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q3:Each country in one regionName
DataDF3=DataDF%>%select(regionName,countryName)%>%group_by(regionName,countryName)%>%summarise(Number=n())

#Q3(1)Bar chart(Asia Pacific)
DataDF3=DataDF%>%select(regionName,countryName)%>%group_by(regionName,countryName)%>%summarise(Number=n())%>%
  filter(regionName=="Asia Pacific")
ggplot(DataDF3, aes(x=reorder(countryName,Number),y=Number)) + 
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("countryName") + ylab("Number per countryName for Asia Pacific") + ggtitle("Number of client per countryName of Asian Pacific") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q3(2)Bar chart(Eastern Europe)
DataDF3=DataDF%>%select(regionName,countryName)%>%group_by(regionName,countryName)%>%summarise(Number=n())%>%
  filter(regionName=="Eastern Europe")
ggplot(DataDF3, aes(x=reorder(countryName,Number),y=Number)) + 
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("countryName") + ylab("Number per countryName for Eastern Europe") + ggtitle("Number of client per countryName of Eastern Europe") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q3(3)Bar chart(Latin America & Caribbean)
DataDF3=DataDF%>%select(regionName,countryName)%>%group_by(regionName,countryName)%>%summarise(Number=n())%>%
  filter(regionName=="Latin America & Caribbean")
ggplot(DataDF3, aes(x=reorder(countryName,Number),y=Number)) + 
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("countryName") + ylab("Number per countryName for Latin America & Caribbean") + ggtitle("Number of client per countryName of Latin America & Caribbean") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q3(4)Bar chart(Middle East & Africa)
DataDF3=DataDF%>%select(regionName,countryName)%>%group_by(regionName,countryName)%>%summarise(Number=n())%>%
  filter(regionName=="Middle East & Africa")
ggplot(DataDF3, aes(x=reorder(countryName,Number),y=Number)) + 
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("countryName") + ylab("Number per countryName for Middle East & Africa") + ggtitle("Number of client per countryName of Middle East & Africa") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q3(5)Bar chart(North America)
DataDF3=DataDF%>%select(regionName,countryName)%>%group_by(regionName,countryName)%>%summarise(Number=n())%>%
  filter(regionName=="North America")
ggplot(DataDF3, aes(x=reorder(countryName,Number),y=Number)) + 
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("countryName") + ylab("Number per countryName for North America") + ggtitle("Number of client per countryName of North America") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q3(6)Bar chart(Western Europe)
DataDF3=DataDF%>%select(regionName,countryName)%>%group_by(regionName,countryName)%>%summarise(Number=n())%>%
  filter(regionName=="Western Europe")
ggplot(DataDF3, aes(x=reorder(countryName,Number),y=Number)) + 
  geom_bar(position="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("countryName") + ylab("Number per countryName for Western Europe") + ggtitle("Number of client per countryName of Western Europe") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q4 State in USA
DataDF4=DataDF%>%select(State,countryName)%>%filter(countryName=="United States of America (USA)")%>%
  group_by(State)%>%summarise(Number=n())
#Q4(1)Bar chart
DataDF4=DataDF%>%select(State,countryName)%>%filter(countryName=="United States of America (USA)")%>%
  group_by(State)%>%summarise(Number=n())
ggplot(DataDF4, aes(x=reorder(State,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("State") + ylab("Number per State") + ggtitle("Number of client per State in U.S.A") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q5 User Type
DataDF5=DataDF%>%select(usertypeName)%>%group_by(usertypeName)%>%summarise(Number=n())

#Q5(1)User Type-bar chart
DataDF5=DataDF%>%select(usertypeName)%>%group_by(usertypeName)%>%summarise(Number=n())
ggplot(DataDF5, aes(x=reorder(usertypeName,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("User Type") + ylab("Number per category") + ggtitle("Number of client per Category of User Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(1)User Type-pie chart
DataDF5=DataDF%>%select(usertypeName)%>%group_by(usertypeName)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=factor(usertypeName))) + 
  geom_bar(position="stack",stat="identity")+
  xlab("") + ylab("Number per User Type") + labs(fill='User Type')+
  geom_text(aes(x= factor(1), y=sum(Number)-pos, label = Number), size=3)+
  ggtitle("Portion of client per User Type") +coord_polar(theta = "y")+
  theme(text= element_text(size = 20),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q5(1)User Type-pie chart(except "--")
DataDF5=DataDF%>%select(usertypeName)%>%filter(usertypeName!="--")%>%group_by(usertypeName)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=factor(usertypeName))) + 
  geom_bar(position="stack",stat="identity")+
  xlab("") + ylab("Number per User Type") + labs(fill='User Type')+
  geom_text(aes(x= factor(1), y=sum(Number)-pos, label = Number), size=3)+
  ggtitle("Portion of client per User Type") +coord_polar(theta = "y")+
  theme(text= element_text(size = 20),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(2)User Type per Client Type-plot chart
DataDF5=DataDF%>%select(usertypeName,clienttypeName)%>%group_by(usertypeName,clienttypeName)%>%summarise(Number=n())
ggplot(DataDF5,aes(x=usertypeName,y=clienttypeName)) + 
  geom_point(aes(size=Number),stat="identity")+
  geom_text(aes(label=Number),hjust=0, vjust=3.5 )+scale_size(range = c(4, 20))+
  xlab("User Type") + ylab("Client Type") + ggtitle("Number of client according to User Type and Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(2)User Type per Client Type-bar chart-1
DataDF5=DataDF%>%select(usertypeName,clienttypeName)%>%
  group_by(usertypeName,clienttypeName)%>%summarise(Number=n())
ggplot(DataDF5,aes(x=usertypeName,y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+facet_grid(.~clienttypeName)+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("") + ylab("Number of each category") + ggtitle("User Type per Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(2)User Type per Client Type-bar chart-2
DataDF5=DataDF%>%select(clienttypeName,usertypeName)%>%
  group_by(clienttypeName,usertypeName)%>%summarise(Number=n())
ggplot(DataDF5,aes(x=clienttypeName,y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+facet_wrap(~usertypeName)+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("") + ylab("Number of each category") + ggtitle("User Type per Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(2)User Type per Client Type-pie chart-1
DataDF5=DataDF%>%select(usertypeName,clienttypeName)%>%
  group_by(usertypeName,clienttypeName)%>%summarise(Number=n())%>%mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=clienttypeName))+ 
  geom_bar(position="stack",stat="identity")+facet_wrap(~usertypeName)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per User Type") + ggtitle("Number of client per User Type of each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q5(2)User Type per Client Type-pie chart-1(Except"--")
DataDF5=DataDF%>%select(usertypeName,clienttypeName)%>%filter(clienttypeName!="--",usertypeName!="--")%>%
  group_by(usertypeName,clienttypeName)%>%summarise(Number=n())%>%mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=clienttypeName))+ 
  geom_text(aes(x= factor(1), y=pos, label = Number), size=5)+
  geom_bar(position="stack",stat="identity")+facet_wrap(~usertypeName)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per User Type") + ggtitle("Number of client per User Type of each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(2)User Type per Client Type-pie chart-2
DataDF5=DataDF%>%select(clienttypeName,usertypeName)%>%
  group_by(clienttypeName,usertypeName)%>%summarise(Number=n())%>%mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=usertypeName))+ 
  geom_bar(position="stack",stat="identity")+facet_wrap(~clienttypeName)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per User Type") + ggtitle("Number of client per User Type of each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q5(2)User Type per Client Type-pie chart-2(Except"--")
DataDF5=DataDF%>%select(clienttypeName,usertypeName)%>%filter(clienttypeName!="--",usertypeName!="--")%>%
  group_by(clienttypeName,usertypeName)%>%summarise(Number=n())%>%mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF5, aes(x=factor(1),y=Number,fill=usertypeName))+ 
  geom_text(aes(x= factor(1), y=pos, label = Number), size=5)+
  geom_bar(position="stack",stat="identity")+facet_wrap(~clienttypeName)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per User Type") + ggtitle("Number of client per User Type of each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6 Data Usage
DataDF6=DataDF%>%select(datausageName)%>%group_by(datausageName)%>%summarise(Number=n())

#Q6(1)Data Usage-bar chart
DataDF6=DataDF%>%select(datausageName)%>%group_by(datausageName)%>%summarise(Number=n())
ggplot(DataDF6,aes(x=datausageName,y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("") + ylab("Number of each category") + ggtitle("Number of client per Data Usage") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(1)Data Usage-bar chart(Except"--")
DataDF6=DataDF%>%select(datausageName)%>%filter(datausageName!="--")%>%group_by(datausageName)%>%summarise(Number=n())
ggplot(DataDF6,aes(x=reorder(datausageName,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity")+
  geom_text(aes(label=Number),hjust=0.5, vjust=-0.5 )+
  xlab("") + ylab("Number of each category") + ggtitle("Number of client per Data Usage") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q6(2)Data Usage-pie chart
DataDF6=DataDF%>%select(datausageName)%>%group_by(datausageName)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF6, aes(x=factor(1),y=Number,fill=factor(datausageName))) + 
  geom_bar(position="stack",stat="identity")+
  xlab("") + ylab("Number per Data Usage") + labs(fill='Data Usage')+
  geom_text(aes(x= factor(1), y=sum(Number)-pos, label = Number), size=6)+
  ggtitle("Portion of client per Data Usage") +coord_polar(theta = "y")+
  theme(text= element_text(size = 20),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q6(2)Data Usage-pie chart(Except"--")
DataDF6=DataDF%>%select(datausageName)%>%filter(datausageName!="--")%>%group_by(datausageName)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF6, aes(x=factor(1),y=Number,fill=factor(datausageName))) + 
  geom_bar(position="stack",stat="identity")+
  xlab("") + ylab("Number per Data Usage") + labs(fill='Data Usage')+
  geom_text(aes(x= factor(1), y=sum(Number)-pos, label = Number), size=6)+
  ggtitle("Portion of client per Data Usage") +coord_polar(theta = "y")+
  theme(text= element_text(size = 20),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(3)Data Usage per Client Type-bar chart-1
DataDF6=DataDF%>%select(clienttypeName,datausageName)%>%group_by(clienttypeName,datausageName)%>%summarise(Number=n())
ggplot(DataDF6, aes(x=reorder(clienttypeName,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=datausageName))+
  xlab("Client Type") + ylab("Number per category") + ggtitle("Number of client per Category of Data Usage for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(3)Data Usage per Client Type-bar chart-1(Except clienttypeName="--")
DataDF6=DataDF%>%select(clienttypeName,datausageName)%>%filter(clienttypeName!="--")%>%group_by(clienttypeName,datausageName)%>%summarise(Number=n())
ggplot(DataDF6, aes(x=reorder(clienttypeName,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=datausageName))+
  xlab("Client Type") + ylab("Number per category") + ggtitle("Number of client per Category of Data Usage for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(3)Data Usage per Client Type-bar chart-1(Except datausageName="--")
DataDF6=DataDF%>%select(clienttypeName,datausageName)%>%filter(datausageName!="--")%>%group_by(clienttypeName,datausageName)%>%summarise(Number=n())
ggplot(DataDF6, aes(x=reorder(clienttypeName,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=datausageName))+
  xlab("Client Type") + ylab("Number per category") + ggtitle("Number of client per Category of Data Usage for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(3)Data Usage per Client Type-bar chart-2
DataDF6=DataDF%>%select(datausageName,clienttypeName)%>%group_by(datausageName,clienttypeName)%>%summarise(Number=n())
ggplot(DataDF6, aes(x=reorder(datausageName,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=clienttypeName))+
  xlab("Data Usage") + ylab("Number per category") + ggtitle("Number of client per Category of Data Usage for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(3)Data Usage per Client Type-bar chart-2(Except datausageName="--")
DataDF6=DataDF%>%select(datausageName,clienttypeName)%>%filter(datausageName!="--")%>%group_by(datausageName,clienttypeName)%>%summarise(Number=n())
ggplot(DataDF6, aes(x=reorder(datausageName,Number),y=Number)) + 
  geom_bar(position ="dodge",stat="identity",aes(fill=clienttypeName))+
  xlab("Data Usage") + ylab("Number per category") + ggtitle("Number of client per Category of Data Usage for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))


#Q6(3)Data Usage per Client Type-pie chart-1
DataDF6=DataDF%>%select(clienttypeName,datausageName)%>%group_by(clienttypeName,datausageName)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF6, aes(x=factor(1),y=Number,fill=datausageName)) + 
  geom_bar(position="stack",stat="identity")+facet_wrap(~clienttypeName)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of Data Usage for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(3)Data Usage per Client Type-pie chart-1(except datausageName="--")
DataDF6=DataDF%>%select(clienttypeName,datausageName)%>%filter(datausageName!="--")%>%group_by(clienttypeName,datausageName)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF6, aes(x=factor(1),y=Number,fill=datausageName)) + 
  geom_bar(position="stack",stat="identity")+facet_wrap(~clienttypeName)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of Data Usage for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(3)Data Usage per Client Type-pie chart-2
DataDF6=DataDF%>%select(datausageName,clienttypeName)%>%group_by(datausageName,clienttypeName)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF6, aes(x=factor(1),y=Number,fill=clienttypeName)) + 
  geom_bar(position="stack",stat="identity")+facet_wrap(~datausageName)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of Data Usage for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

#Q6(3)Data Usage per Client Type-pie chart-2(Except datausageName="--")
DataDF6=DataDF%>%select(datausageName,clienttypeName)%>%filter(datausageName!="--")%>%group_by(datausageName,clienttypeName)%>%summarise(Number=n())%>%
  mutate(pos =cumsum(Number)-Number/2)
ggplot(DataDF6, aes(x=factor(1),y=Number,fill=clienttypeName)) + 
  geom_bar(position="stack",stat="identity")+facet_wrap(~datausageName)+coord_polar(theta = "y")+
  xlab("") + ylab("Number per category") + ggtitle("Number of client per Category of Data Usage for each Client Type") +
  theme(text= element_text(size = 20),
        axis.text.x=element_text(hjust=1),
        axis.title.x = element_text (size = 30),
        axis.title.y = element_text (size = 30))

