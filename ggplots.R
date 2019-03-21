library("ggplot2")

setwd("-------------")
data=read.csv("--------.csv",stringsAsFactors=FALSE)

#Average salaries by Race
df=data.frame(data$Race.Description,data$Annual.Salary)
df=df[order(df$data.Race.Description),]
names(df)=c("Race","Salary")
df1=aggregate(df$Salary~df$Race, df, mean)
names(df1)=c("Race","Salary")
position=c("American Indian or Alaska Native","Native Hawaiian or Other Pacific Islander",
           "White","Black or African American","Asian","Hispanic or Latino","Two or more races")
df1$Race=factor(df1$Race,levels=position)
p=ggplot(data=df1,aes(x=Race,Salary,fill= Race))+geom_bar(stat="identity")+scale_y_continuous(name="Salary", labels = scales::comma)
p=p+theme_classic()+geom_text(aes(label=scales::dollar(Salary)),color="Black",vjust=-0.3,size=3.5)
p=p+geom_vline(xintercept=c(-2.5,2.5), linetype="dotted")
plo1=p
ggsave("test.1.pdf", plot1+ggtitle("Average Salaries by Race 2018-2019")+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(plot.title = element_text(hjust = 0.5)), width = 20,height=10)

#Average Salaries by Gender
df=data.frame(data$Gender,data$Annual.Salary)
df=df[order(df[,1]),]
names(df)=c("Gender","Salary")
df1=aggregate(df$Salary~df$Gender, df, mean)
names(df1)=c("Gender","Salary")
p=ggplot(data=df1,aes(Gender,Salary,fill= Gender))+geom_bar(width=0.5,stat="identity")+scale_y_continuous(name="Salary", labels = scales::comma)
p=p+theme_classic()+geom_text(aes(label=scales::dollar(Salary)),color="Black",vjust=-0.3,size=3.5)+geom_col(position = "dodge")
plot2=p
ggsave("test.2.pdf", plot2+ggtitle("Unadjusted Gender Wage Gap")+theme(plot.title = element_text(hjust = 0.5)), width = 10,height=10)
