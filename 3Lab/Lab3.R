library(data.table)
mark = copy(Ch3_marketing)
setDT(mark)

str(mark)
unique(mark$pop_density)

#Nominal scale is a naming scale, where variables are simply “named” 
#or labeled, with no specific order. Ordinal scale has all its variables 
#in a specific order, beyond just naming them. eg pop_density low med high

mark[,pop_density:=factor(pop_density,levels = c('Low','Medium','High'), ordered = T)]

#NA values in the table
grep('NA',mark)

#stats summary
summary(mark$google_adwords)

mark[,summary(google_adwords)]
mark[,summary(pop_density)]

library(ggplot2)
ggplot(mark, aes(x=pop_density)) + geom_bar()
ggplot(mark, aes(x=pop_density)) + geom_bar(fill = 'green', color='blue')

#x axis
ggplot(mark, aes(x=google_adwords)) + geom_boxplot(fill = 'green', color='blue')

ggplot(mark, aes(x=google_adwords)) + geom_histogram(fill = 'pink', color='red')

#bins or binwidth 
ggplot(mark, aes(x=google_adwords)) + geom_histogram(fill = 'pink', color='red', bins = 10)
ggplot(mark, aes(x=google_adwords)) + geom_histogram(fill = 'pink', color='red', binwidth = 50)

#Transforming data into categories cut function, breaks into group sizes defined by you
x=1:10
y=cut(x,2,labels = c('BM','SM'))
y
dy = data.table(y)
dy

mark[,empFactor:=cut(employees,2)]
table(mark$empFactor,mark$pop_density)

ggplot(mark,aes(x=pop_density,fill=empFactor)) + geom_bar()
ggplot(mark, aes(x=pop_density,y=marketing_total)) + geom_boxplot(fill = 'orange', color='red')

#positive correlation between -1 to 1/ Scatter plot
ggplot(mark, aes(x=revenues,y=google_adwords)) + geom_point( color='salmon')

cor(mark$google_adwords,mark$revenues)
cor.test(mark$google_adwords,mark$revenues)
cor.test(mark$twitter,mark$revenues)
cor.test(mark$facebook,mark$revenues)
mark[,.(cor.test(mark$twitter,mark$revenues),cor.test(mark$facebook,mark$revenues))]

#scatter plot every col against every
pairs(mark)

#correlations between all columns
cor(mark[,1:6])


