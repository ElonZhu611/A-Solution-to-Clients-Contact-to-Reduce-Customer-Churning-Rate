# Title: Analyse [Balance, Products, Has Credit Card, Tenure] for churning dataset
# Author: Zhu Haoxiang & Ritika Jain
# Date: 7 Sep 2020
# Reference: https://www.kaggle.com/kmalit/bank-customer-churn-prediction
#            https://www.r-graph-gallery.com/index.html
#            https://ggplot2.tidyverse.org/index.html

# Import Libraries and Datasets
library(data.table)
library(ggplot2)
#setwd("E:/OneDrive - Nanyang Technological University/NTU/Trimester 1/AN6003 Analytics Strategy/Team Assignment and Project/Data")
dt = fread("Churn_Modelling.csv")
dt$Gender = factor(dt$Gender)
dt$Exited = factor(dt$Exited)

apply(dt, 2, function(x) any(is.na(dt)))
# Analyse Balance
dt$Exited = factor(dt$Exited)
# Calculate Balance/Estimated Salary
dt[, Balanceratio := Balance / EstimatedSalary]
# Sort Balanceratio to check extreme values
sort(dt$Balanceratio, decreasing = TRUE)
# Define Loyal Customer, General Customer and Exited Customer
loyal_cus = dt[dt$Balanceratio >= 5 & dt$Balanceratio < 20, ]
gen_cus = dt[dt$Balanceratio > 0 & dt$Balanceratio < 5, ]
nobal_cus = dt[dt$Balanceratio == 0, ]

# Barplot and Boxplot Balanceratio
prop_exit = data.table(prop = c(sum(loyal_cus$Exited[loyal_cus$Gender == "Male"] == 1) / nrow(loyal_cus[loyal_cus$Gender == "Male"]),
                                sum(loyal_cus$Exited[loyal_cus$Gender == "Female"] == 1) / nrow(loyal_cus[loyal_cus$Gender == "Female"]),
                                gen = sum(gen_cus$Exited[gen_cus$Gender == "Male"] == 1) / nrow(gen_cus[gen_cus$Gender == "Male"]),
                                gen = sum(gen_cus$Exited[gen_cus$Gender == "Female"] == 1) / nrow(gen_cus[gen_cus$Gender == "Female"]),
                                nobal = sum(nobal_cus$Exited[nobal_cus$Gender == "Male"] == 1) / nrow(nobal_cus[nobal_cus$Gender == "Male"]),
                                nobal = sum(nobal_cus$Exited[nobal_cus$Gender == "Female"] == 1) / nrow(nobal_cus[nobal_cus$Gender == "Female"])),
                       category = c("High", "High", "Medium", "Medium", "Low", "Low"),
                       Gender = c("Male", "Female", "Male", "Female", "Male", "Female"))
ggplot(prop_exit, aes(category, prop, fill = Gender)) + geom_bar(stat="identity", position="dodge")

p_loyal_cus = ggplot(loyal_cus, aes(Exited, Balanceratio, fill = Gender))
p_loyal_cus + geom_boxplot() + ggtitle("High bal_ratio Customers")
p_gen_cus = ggplot(gen_cus, aes(Exited, Balanceratio, fill = Gender))
p_gen_cus + geom_boxplot() + ggtitle("Medium bal_ratio Customers")


# Analyse Salary
p_hv = ggplot(dt, aes(Exited, EstimatedSalary, fill = Gender))
p_hv + geom_boxplot() + ggtitle("All Customers")
# High-Net-Value Customers
High_value = dt[dt$EstimatedSalary >= 150000]
p_hv = ggplot(High_value, aes(Exited, EstimatedSalary, fill = Gender))
p_hv + geom_boxplot() + ggtitle("High Net Value Customers")

# Analyse Number of products
# Density of Number of products
p_np_d = ggplot(dt, aes(NumOfProducts, fill = Gender))
p_np_d + geom_histogram(binwidth = 0.5, position="dodge")
# Relationship between #products and exit
p_np_e = ggplot(dt, aes(x = Exited,y =  NumOfProducts, fill = Gender))
p_np_e + geom_boxplot()
# Average #products who exited
p_np_e + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=20, size=3, color = "black")


# Analyse HasCreditCard
dt$HasCrCard = factor(dt$HasCrCard)
# Relationship between having credit card and exit
prop_CrCard = data.table(prop = c(sum(dt$Exited[dt$Gender == "Male" & dt$HasCrCard == 1] == 1) / nrow(dt[dt$Gender == "Male" & dt$HasCrCard == 1]),
                                sum(dt$Exited[dt$Gender == "Female" & dt$HasCrCard == 1] == 1) / nrow(dt[dt$Gender == "Female" & dt$HasCrCard == 1]),
                                sum(dt$Exited[dt$Gender == "Male" & dt$HasCrCard == 0] == 1) / nrow(dt[dt$Gender == "Male" & dt$HasCrCard == 0]),
                                sum(dt$Exited[dt$Gender == "Female" & dt$HasCrCard == 0] == 1) / nrow(dt[dt$Gender == "Female" & dt$HasCrCard == 0])),
                         CrCard = c("HasCard", "HasCard", "NoCard", "NoCard"),
                         Gender = c("Male", "Female", "Male", "Female"))
ggplot(prop_CrCard, aes(CrCard, prop, fill = Gender)) + geom_bar(stat="identity", width = 0.5, position = "dodge")

# Analyse Tenure
p_ten = ggplot(dt, aes(x = Exited,y =  Tenure, fill = Gender))
p_ten + geom_boxplot()

# Second Part
data = read.csv("Churn_Modelling.csv")
head(data)

# Code for Active member as an indicator
data_exited = data[data$Exited == 1,]
head(data_exited)

print(nrow(data_exited$IsActiveMember == 1))

data_exited_active = data_exited[data_exited$IsActiveMember == 1,]

data_exited_nonActive = data_exited[data_exited$IsActiveMember == 0,]

pie_data = c(nrow(data_exited_active),nrow(data_exited_nonActive))
pie_percent = round(100*pie_data/sum(pie_data),1)

pie(pie_data,
    pie_percent,
    main = "Active or non active customer who exited",
    col = rainbow(length(pie_data))
    
)
legend("topleft",c("Active Member","Non Active Member"),fill = rainbow(length(pie_data)),cex = 0.7)

# Divivding according to gender

data_exited_active_males = data_exited_active[data_exited_active$Gender == "Male",]
data_exited_active_females = data_exited_active[data_exited_active$Gender == "Female",]
data_exited_nonActive_males = data_exited_nonActive[data_exited_nonActive$Gender == "Male",]
data_exited_nonActive_females = data_exited_nonActive[data_exited_nonActive$Gender == "Female",]

GenderBased = data.frame(GenderGroup = c("Active male members","Active Female Numbers",
                                         "Non Active Male Members","Non Active Female members"),
                         Count = c(nrow(data_exited_active_males),nrow(data_exited_active_females),
                                   nrow(data_exited_nonActive_males),nrow(data_exited_nonActive_females)
                         ))
GenderBased
bp = ggplot(GenderBased,aes(x="", y=Count, fill=GenderGroup))+geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)+geom_text(aes(label = paste0(round(Count*100/sum(Count),2),"%")),position = position_stack(vjust = 0.5)) + ggtitle("Active status group by Gender")
pie


library(plotrix)


# Code for Credit Score

customer_exited_badScore = data_exited[data$CreditScore<580,]
customer_exited_fairScore = data_exited[data$CreditScore>=580 & data$CreditScore <669,]
customer_exited_goodScore = data_exited[data$CreditScore>=670 & data$CreditScore <739,]
customer_exited_veryGoodScore = data_exited[data$CreditScore>=740 & data$CreditScore <=799,]
customer_exited_excellentScore = data_exited[data$CreditScore>=800,]

plot_data = c(nrow(customer_exited_badScore),nrow(customer_exited_fairScore),
              nrow(customer_exited_goodScore),nrow(customer_exited_veryGoodScore),
              nrow(customer_exited_excellentScore))
piePercent = round(100*plot_data/sum(plot_data),2)

pie(plot_data,
    piePercent,
    main = "Customer exited based on credit score",
    col = rainbow(length(plot_data))
    
)
segment_name = c("Bad Score","Fair Score","Good Score","Very Good Score","Excellent")
legend("topleft",segment_name,fill = rainbow(length(plot_data)),cex = 0.6)



customer_exited_badScore_male = customer_exited_badScore[customer_exited_badScore$Gender == "Male",]
customer_exited_badScore_female = customer_exited_badScore[customer_exited_badScore$Gender == "Female",]
customer_exited_fairScore_male = customer_exited_fairScore[customer_exited_fairScore$Gender == "Male",]
customer_exited_fairScore_female = customer_exited_fairScore[customer_exited_badScore$Gender == "Female",]
customer_exited_goodScore_male = customer_exited_goodScore[customer_exited_goodScore$Gender == "Male",]
customer_exited_goodScore_female = customer_exited_goodScore[customer_exited_goodScore$Gender == "Female",]

CreditBased = data.frame(CreditGroup = c("Bad Credit Score-Males","Bad Credit Score-Females",
                                         "Fair Credit Score-Males","Fair Credit Score-Female",
                                         "Good Credit Score-Males","Good Credit Score-Females"),
                         Count = c(nrow(customer_exited_badScore_male),nrow(customer_exited_badScore_female),
                                   nrow(customer_exited_fairScore_male),nrow(customer_exited_fairScore_female),
                                   nrow(customer_exited_goodScore_male),nrow(customer_exited_goodScore_female)
                         ))
print(CreditBased)

bp = ggplot(CreditBased,aes(x="", y=Count, fill=CreditGroup))+geom_bar(width = 1, stat = "identity") 
bp

pie <- bp + coord_polar("y", start=0)+geom_text(aes(label = paste0(round(Count*100/sum(Count),2),"%")),position = position_stack(vjust = 0.5))+ ggtitle("Credit Score group by Gender")
pie


# Code for age group
Age16To20 = data_exited[data_exited$Age>=16 & data_exited$Age<20,]
Age20To30 = data_exited[data_exited$Age>=20 & data_exited$Age<30,]
Age30To40 = data_exited[data_exited$Age>=30 & data_exited$Age<40,]
Age40To50 = data_exited[data_exited$Age>=40 & data_exited$Age<50,]
Age50To60 = data_exited[data_exited$Age>=50 & data_exited$Age<60,]
Age60To70 = data_exited[data_exited$Age>=60 & data_exited$Age<70,]
Age70To80 = data_exited[data_exited$Age>=70 & data_exited$Age<80,]
Age80To90 = data_exited[data_exited$Age>=80 & data_exited$Age<90,]


plot_Age_Data = c(nrow(Age16To20),nrow(Age20To30),nrow(Age30To40),nrow(Age40To50),
                  nrow(Age50To60),nrow(Age60To70),nrow(Age70To80),
                  nrow(Age80To90))



df <- data.frame(
  AgeGroup = c("Age 16 to 20", "Age 20 to 30", "Age 30 to 40","Age 40 to 50",
               "Age 50 to 60","Age 60 to 70","Age 70 to 80","Age 80 to 90"),
  count = c(nrow(Age16To20),nrow(Age20To30),nrow(Age30To40),nrow(Age40To50),
            nrow(Age50To60),nrow(Age60To70),nrow(Age70To80),
            nrow(Age80To90))
)
bp<- ggplot(df, aes(x="", y=count, fill=AgeGroup))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie <- pie+geom_text(aes(label = paste0(round(count*100/sum(count),2),"%")),position = position_stack(vjust = 0.5))
pie


Age30To40_male = Age30To40[Age30To40$Gender=='Male',]
Age30To40_female = Age30To40[Age30To40$Gender=='Female',]
Age40To50_male= Age40To50[Age40To50$Gender == 'Male',]
Age40To50_female= Age40To50[Age40To50$Gender=='Female',]
Age50To60_male= Age50To60[Age50To60$Gender=='Male',]
Age50To60_female= Age50To60[Age50To60$Gender=='Female',]

df = data.frame(AgeGroup_BasedOnGender = c("Age 30 to 40-Males","Age 30 to 40-Females",
                                           "Age 40 to 50-Males","Age 40 to 50-Females",
                                           "Age 50 to 60-Males","Age 50 to 60-Females"),
                Count = c(nrow(Age30To40_male),nrow(Age30To40_female),
                          nrow(Age40To50_male),nrow(Age40To50_female),
                          nrow(Age50To60_male),nrow(Age50To60_female)))

bp<- ggplot(df, aes(x="", y=Count, fill=AgeGroup_BasedOnGender))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)+geom_text(aes(label = paste0(round(Count*100/sum(Count),2),"%")),position = position_stack(vjust = 0.5))
pie

# Plot Age vs. Exited
Age16To20 = data[data$Age>=16 & data$Age<20,]
Age20To30 = data[data$Age>=20 & data$Age<30,]
Age30To40 = data[data$Age>=30 & data$Age<40,]
Age40To50 = data[data$Age>=40 & data$Age<50,]
Age50To60 = data[data$Age>=50 & data$Age<60,]
Age60To70 = data[data$Age>=60 & data$Age<70,]
Age70To80 = data[data$Age>=70 & data$Age<80,]
Age80To90 = data[data$Age>=80 & data$Age<90,]

Age16To20_exit = sum(Age16To20["Exited"]) / nrow(Age16To20)
Age20To30_exit = sum(Age20To30["Exited"]) / nrow(Age20To30)
Age30To40_exit = sum(Age30To40["Exited"]) / nrow(Age30To40)
Age40To50_exit = sum(Age40To50["Exited"]) / nrow(Age40To50)
Age50To60_exit = sum(Age50To60["Exited"]) / nrow(Age50To60)
Age60To70_exit = sum(Age60To70["Exited"]) / nrow(Age60To70)
Age70To80_exit = sum(Age70To80["Exited"]) / nrow(Age70To80)
Age80To90_exit = sum(Age80To90["Exited"]) / nrow(Age80To90)

agee <- data.frame(age=c("16-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90"),
                   exit_prop=c(Age16To20_exit, Age20To30_exit, Age30To40_exit, Age40To50_exit, Age50To60_exit, Age60To70_exit, Age70To80_exit, Age80To90_exit))
p <- ggplot(data=agee, aes(x=age, y=exit_prop, fill=age)) +
  geom_bar(stat="identity")
p

