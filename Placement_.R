library(dplyr)
library(ggplot2)
library(scales)

data = read.csv("C:/Users/SABAREESH/Downloads/Placement_Data_Full_Class.csv")

View(data)
head(data)
str(data)
summary(data)

ggplot(data, aes(hsc_b)) + geom_bar()
ggplot(data, aes(hsc_s)) + geom_bar() 

# 1. Commerce , 2. Science, 3. Arts

ggplot(data, aes(degree_t)) + geom_bar() 

# 1. Commerce and Managment , 2. Science and Tech , 3. Others

ggplot(data, aes(workex)) + geom_bar() 

# 1. NO, 2. Yes

ggplot(data, aes(salary)) +
  geom_histogram(fill="pink",colour="black",alpha=0.3) + 
  scale_x_continuous(labels = comma)

#The salary range from 200000 to 940000 where the most people get around 260000 to 280000


data$salary[is.na(data$salary)] = mean(data$salary)


?prop.table


prop.table(table(data$hsc_b,data$specialisation),2)*100

# People who get specialized are from Other standards in HSC ( 12th Standard )

prop.table(table(data$ssc_b,data$specialisation),2)*100

# People who get specialized are from Central in SSC ( 10th Standard )

par(mfrow=c(1,2))

ggplot(data,aes(x = gender,y= salary)) + geom_point(size=1,colour='red') + scale_y_continuous(labels = comma)

# Male gets somewhat high salary than female but this does not affect placement

ggplot(data,aes(x=gender,y=salary,colour=workex)) + geom_point() + 
  geom_smooth(method=lm) + scale_y_continuous(labels = comma) + 
  facet_wrap(~specialisation) + theme_bw()

# Male in Market and finance gets the high salary where also in HR ( Commerce and Management )

ggplot(data,aes(x=etest_p,y=salary)) + geom_boxplot() + 
  geom_smooth(method=lm) + scale_y_continuous(labels = comma) +  facet_wrap(~specialisation) +
    theme_bw()

# As employability test percentage increases the salary increases a little where Marketing and Finance team gets more salary than HR team.

ggplot(data,aes(x=mba_p,y=salary)) + geom_line() + 
  geom_smooth(method=lm) + scale_y_continuous(labels = comma) + facet_wrap(~specialisation) +
  theme_bw()

# Commerce and Management students who studied Marketing and Finance got more salary


ggplot(data,aes(x=workex,y=salary)) + geom_boxplot() + 
  geom_smooth(method=lm) + scale_y_continuous(labels = comma) + 
  theme_bw()

data$salary = as.factor(data$salary)

ggplot(data,aes(x=ssc_p,y=status)) + geom_point()  + 
  theme_bw() + facet_wrap(~ssc_b)

# Students with high SSC percentage get more places. 

ggplot(data,aes(x=hsc_p,y=status)) + geom_point()   + 
  theme_bw() + facet_wrap(~hsc_b)

# Students with high HSC percentage get more places. 

ggplot(data,aes(x=degree_p,y=status)) + geom_point()   + 
  theme_bw() + facet_wrap(~degree_t)

# Students with high HSC percentage get more places. 

ggplot(data,aes(x=etest_p,y=status)) + geom_point()   + 
  theme_bw() + facet_wrap(~data$specialisation)

# etest percentage Does not affect much

ggplot(data,aes(x=mba_p,y=status)) + geom_point()   + 
  theme_bw() + facet_wrap(~data$specialisation)

# MBA percentage does not affect the placement

prop.table(table(data$gender,data$status),1) *100

# 70 per male placed while 63 per female only placed

prop.table(table(data$gender,data$workex),1) *100

#  37 per male , 28 per female have work experience

fil_data = subset(data, workex = "Yes")
