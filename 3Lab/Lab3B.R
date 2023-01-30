library(data.table)
setDT(titanic)
library(ggplot2)
str(titanic)

unique(titanic$Pclass)

titanic$Pclass
titanic[,Survived:= factor(Survived)]
titanic[,Pclass:= factor(Pclass,ordered = T)]
titanic[,Sex:= factor(Sex)]

#Change int to char
titanic$Survived <- as.character(titanic$Survived)

ggplot(titanic, aes(x=Sex)) + geom_bar()
ggplot(titanic, aes(x=Survived)) + geom_bar() + 
theme_bw() + labs(y="Passenger Count", title="Titanic survival rates")

#divide bar into survived 0 1
ggplot(titanic, aes(x=Sex, fill=Survived)) + geom_bar() + 
theme_bw() + labs(y="Passenger Count", title="Titanic survival rates")


ggplot(titanic , aes(x=Sex , fill=Survived)) + geom_bar() + facet_wrap(~Pclass) +
theme_bw() + labs(y= "Passenger Count",title = "Titanice Survival Rates")

# age increment of 5
ggplot(titanic , aes(x=Age)) + geom_histogram(binwidth= 5,fill='yellow',color='black') + theme_bw() + 
labs(y= "Passenger Count",x="Age (5)", title = "Titanice Survival Rates")

ggplot(titanic , aes(x=Age , fill=Survived)) + geom_histogram(binwidth = 5,color='black') + theme_bw() + labs(y= "
Passenger Count",x="Age (5)", title = "Titanice
Survival Rates")

ggplot(titanic , aes(y=Age , x=Survived)) + geom_boxplot() + theme_bw() +
labs(x= "Survived",y="Age", title = "Titanice Survival Rates")

#density curve
ggplot(titanic , aes(x=Age , fill=Survived)) + geom_density(alpha = 0.5) +
  theme_bw() + facet_wrap(Sex ~ Pclass) + labs(y= "Passenger Count",x="Age (5)",
                                               title = "Titanice Survival Rates")
#histogram
ggplot(titanic , aes(x=Age , fill=Survived)) + geom_histogram(binwidth = 5) +
  theme_bw() + facet_wrap(Sex ~ Pclass) + labs(y= "Passenger Count",x="Age (5)",
                                               title = "Titanice Survival Rates")





