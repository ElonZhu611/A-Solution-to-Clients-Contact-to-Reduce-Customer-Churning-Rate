library(ggplot2)
library(data.table)
df = read.csv("contact_short_drop.csv")
#summary(df$job)
#education
job1 = df[df$job == "admin.",]
job2 = df[df$job == "blue-collar",]
job3 = df[df$job == "entrepreneur",]
job4 = df[df$job == "housemaid",]
job5 = df[df$job == "management",]
job6 = df[df$job == "retired",]
job7 = df[df$job == "self-employed",]
job8 = df[df$job == "services",]
job9 = df[df$job == "student",]
job10 = df[df$job == "technician",]
job11 = df[df$job == "unemployed",]

prop_job = data.table(prop = c(sum(job1$y == "yes") / nrow(job1), 
                               sum(job2$y == "yes") / nrow(job2), 
                               sum(job3$y == "yes") / nrow(job3),
                               sum(job4$y == "yes") / nrow(job4),
                               sum(job5$y == "yes") / nrow(job5),
                               sum(job6$y == "yes") / nrow(job6),
                               sum(job7$y == "yes") / nrow(job7),
                               sum(job8$y == "yes") / nrow(job8),
                               sum(job9$y == "yes") / nrow(job9),
                               sum(job10$y == "yes") / nrow(job10),
                               sum(job11$y == "yes") / nrow(job11)),
                      compared_with_avg = c("over","below","below","below","over","over","below","below","over","below","over"),
                      job = c("admin.","blue-collar", "entrepreneur","housemaid", "management","retired","self-employed","services","student","technician","unemployed")
)
prop_job
#prop_job$job = factor(prop_job$job, levels=c("admin.","blue-collar", "entrepreneur","housemaid", "management","retired","self-employed","services","student","technician","unemployed"))
plot_prop_job = ggplot(prop_job, aes(job, prop, fill = compared_with_avg)) + ggtitle("Success Propability per job group")
plot_prop_job+ geom_bar(stat="identity") + geom_hline(yintercept=mean(sum(df$y=="yes")/nrow(df)))

library(ggplot2)
library(data.table)
df = read.csv("contact_short_drop.csv")
#summary(df$job)
#education
job1 = df[df$job == "basic.4y",]
job2 = df[df$job == "basic.6y",]
job3 = df[df$job == "basic.9y",]
job4 = df[df$job == "high.school",]
job5 = df[df$job == "illiterate",]
job6 = df[df$job == "professional.course",]
job7 = df[df$job == "university.degree",]


df3= data.frame(JobGroup = c("basic.4y","basic.6y","basic.9y","high.school","illiterate","professional.course","university.degree"), 
                Count = c(nrow(job1),nrow(job2),nrow(job3),nrow(job4), nrow(job5),nrow(job6),nrow(job7)))
bp<- ggplot(df3, aes(x="", y=Count, fill=JobGroup))+
  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0)+geom_text(aes(label = paste0(round(Count*100/sum(Count),2),"%")),position = position_stack(vjust = 0.5))
pie

library(data.table)
library(ggplot2)

dt = read.csv("contact_short_drop.csv")
summary(dt)
summary(dt$month)
# Separate customer contacted in different months
dt_mar = dt[dt$month == "mar", ] 
dt_apr = dt[dt$month == "apr", ] 
dt_may = dt[dt$month == "may", ] 
dt_jun = dt[dt$month == "jun", ] 
dt_jul = dt[dt$month == "jul", ] 
dt_aug = dt[dt$month == "aug", ] 
dt_sep = dt[dt$month == "sep", ] 
dt_oct = dt[dt$month == "oct", ] 
dt_nov = dt[dt$month == "nov", ] 
dt_dec = dt[dt$month == "dec", ] 

# Calculate the probability of campaign success when contact in different months
prop_month = data.table(prop = c(sum(dt_mar$y == "yes") / nrow(dt_mar), 
                                 sum(dt_apr$y == "yes") / nrow(dt_apr), 
                                 sum(dt_may$y == "yes") / nrow(dt_may), 
                                 sum(dt_jun$y == "yes") / nrow(dt_jun), 
                                 sum(dt_jul$y == "yes") / nrow(dt_jul), 
                                 sum(dt_aug$y == "yes") / nrow(dt_aug), 
                                 sum(dt_sep$y == "yes") / nrow(dt_sep), 
                                 sum(dt_oct$y == "yes") / nrow(dt_oct), 
                                 sum(dt_nov$y == "yes") / nrow(dt_nov), 
                                 sum(dt_dec$y == "yes") / nrow(dt_dec)), 
                        month = c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),
                        compared_with_avg = c("over","over","below","below","below","below","over","over","below","over"))
prop_month$month = factor(prop_month$month, levels=c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

# Plot Success Propability in Different Months
plot_prop_month = ggplot(prop_month, aes(month, prop, fill=compared_with_avg)) + ggtitle("Success Propability in Different Months")
plot_prop_month + geom_bar(stat="identity") + geom_hline(yintercept=mean(sum(dt$y=="yes")/nrow(dt)))

summary(dt$day_of_week)
# Separate customer contacted in different days of week
dt_mon = dt[dt$day_of_week == "mon", ] 
dt_tue = dt[dt$day_of_week == "tue", ] 
dt_wed = dt[dt$day_of_week == "wed", ] 
dt_thu = dt[dt$day_of_week == "thu", ] 
dt_fri = dt[dt$day_of_week == "fri", ] 

# Calculate the probability of campaign success when contact in different months
prop_weekday = data.table(prop = c(sum(dt_mon$y == "yes") / nrow(dt_mon), 
                                   sum(dt_tue$y == "yes") / nrow(dt_tue), 
                                   sum(dt_wed$y == "yes") / nrow(dt_wed), 
                                   sum(dt_thu$y == "yes") / nrow(dt_thu), 
                                   sum(dt_fri$y == "yes") / nrow(dt_fri)),
                          weekday = c("mon","tue","wed","thu","fri"),
                          compared_with_avg = c("below","over","over","over","below"))
prop_weekday$weekday = factor(prop_weekday$weekday, levels=c("mon","tue","wed","thu","fri"))

# Plot Success Propability in Different Weekdays
plot_prop_weekday = ggplot(prop_weekday, aes(weekday, prop, fill=compared_with_avg)) + ggtitle("Success Propability in Different Weekdays")
plot_prop_weekday + geom_bar(stat="identity") + geom_hline(yintercept=mean(sum(dt$y=="yes")/nrow(dt)))

summary(dt$duration)
# Look into the relationship between contact duration and whether the client subscribed a term deposit
plot_duration = ggplot(dt, aes(y, duration, fill=y)) 
plot_duration + geom_boxplot()

summary(dt$campaign)
hist(dt$campaign)
# Classify clients based on contact times
dt_rare = dt[dt$campaign == 1, ] 
dt_norm = dt[dt$campaign == 2|dt$campaign == 3, ] 
dt_freq = dt[dt$campaign > 3, ] 

# Calculate the probability of campaign success with different contact frequency
prop_times = data.table(prop = c(sum(dt_rare$y == "yes") / nrow(dt_rare), 
                                 sum(dt_norm$y == "yes") / nrow(dt_norm), 
                                 sum(dt_freq$y == "yes") / nrow(dt_freq)), 
                        times = c("rarely","normally","frequently"),
                        compared_with_avg = c("over","over","below"))
prop_times$times = factor(prop_times$times, levels=c("rarely","normally","frequently"))

# Plot Success Propability with Different Contact Frequency
plot_prop_times = ggplot(prop_times, aes(times, prop, fill=compared_with_avg)) + ggtitle("Success Propability with Different Contact Frequency")
plot_prop_times + geom_bar(stat="identity") + geom_hline(yintercept=mean(sum(dt$y=="yes")/nrow(dt)))

summary(dt$poutcome)
# Classify clients based on whether they have attended last campaign and the result
dt_psuc = dt[dt$poutcome == "success", ] 
dt_pfai = dt[dt$poutcome == "failure", ] 
dt_pnon = dt[dt$poutcome == "nonexistent", ] 

# Calculate the probability of campaign success with different contact frequency
prop_poutcome = data.table(prop = c(sum(dt_psuc$y == "yes") / nrow(dt_psuc), 
                                    sum(dt_pfai$y == "yes") / nrow(dt_pfai), 
                                    sum(dt_pnon$y == "yes") / nrow(dt_pnon)), 
                            poutcome = c("success","failure","nonexistent"),
                            compared_with_avg = c("over","over","below"))
prop_poutcome$poutcome = factor(prop_poutcome$poutcome, levels=c("success","failure","nonexistent"))

# Plot Success Propability with Different Previous Outcome
plot_prop_poutcome = ggplot(prop_poutcome, aes(poutcome, prop, fill=compared_with_avg)) + ggtitle("Success Propability with Different Previous Outcome")
plot_prop_poutcome + geom_bar(stat="identity") + geom_hline(yintercept=mean(sum(dt$y=="yes")/nrow(dt)))