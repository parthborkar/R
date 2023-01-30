library(data.table)
library(ggplot2)

advert = copy(Ch4_marketing)
setDT(advert)
str(advert)

grep('NA',advert)
advert[,.(summary(google_adwords),summary (facebook),summary (twitter))]
mark[,summary(google_adwords)]

#data is normally distributed if mean and median is similar
summary(advert)

g=advert[,google_adwords]
f=advert[,facebook]
tw=advert[,twitter]
#stack all the values in one table
nv=c(g,f,tw)
nv

#match the values to corresponding company, NROW gives the number of g to be added to the table
nc=c(rep('g',NROW (g)),rep('f',NROW (f)),rep('tw',NROW (tw))) 
nc
advert2 = data.table(nv ,nc)
advert2

ggplot(advert2,aes(y=nv,x=nc)) + geom_boxplot(fill='pink')

#visually looks like a line
pairs(advert)

ggplot (advert , aes (x= google_adwords ,y= marketing_total)) + 
geom_point(color ='blue')

ggplot (advert , aes (x= revenues ,y= marketing_total)) +
geom_point(color ='purple')


#regression
#b0 is the estimated average value of Y when the value of X is zero
#b1 is the estimated change in the average value of Y as a result of a one-unit 
#increase in X
model1 =lm(revenues~marketing_total,data=advert)
model1
str(model1)
model1$residuals

#unit of data is in thousands therefore Revenue increases by $51.93 for every 
#$1,000 increase in total marketing Revenue is $32,007 when total marketing 
#expenditure is $0
#geom_smooth() just a line

ggplot (advert , aes (x= revenues ,y= marketing_total)) +
geom_point(color ='purple') + geom_smooth(method = "lm")

resdf = data.table('res'= model1$residuals)
ggplot(resdf,aes(x=res )) + geom_histogram(bins=10,fill='purple',color='black')

#mean is virtually 0
mean(model1$residuals)
sd(model1$residuals)

#x or y is not defined, the residuals are plotted
#stat_qq z scores -ve to +ve with an avg of 0
#qq plot, qq_line is the ideal line
ggplot(resdf,aes(sample=res)) + stat_qq(color='blue') + stat_qq_line()

#equal variance test
#residuals compared to the predicted values
resdf[,pred:= model1$fitted.values ]
ggplot(resdf,aes(x=pred ,y=res)) + geom_point(color ='purple') + 
geom_smooth(method ='lm')

summary(model1)
#t value
#                 Estimate Std.     Error     t value Pr(>|t|)    
#(Intercept)        32.006696       0.635590   50.36   <2e-16 ***
# marketing_total   0.051929        0.002437   21.31   <2e-16 ***

#t value shows how random is the value based on its error
0.051929/0.002437

#use values only in the range of min and max to predict
summary(advert$marketing_total)
advert[marketing_total>430,marketing_total]

newrev = data.table(marketing_total=seq(460,470,5))
predict.lm(model1,newrev,interval = 'predict')
#inference: for a spend of $460000 estimate revenue is $55894

predict.lm(model1,newrev,level=.99,interval = 'predict')

vv=5:15
sample(vv,5)

#to get the same set of random numbers
set.seed(7)

#30 percent of all rows
liladvert = advert[sample(.N,.3*.N)]
liladvert

samp_model = lm(revenues ~ marketing_total,data = liladvert )
samp_model
confint(samp_model)

#refining data for SLR
x =1:10
y=c(1 ,1.41 ,1.73 ,2 ,2.24 ,2.45 ,2.65 ,2.83 ,3 ,3.16)
fit = lm(y~x)
sampdt = data.table(x,y)
ggplot(sampdt ,aes(x=x,y=y)) + geom_point(color='purple') + 
  geom_smooth(method="lm") + labs(title="Linearity?")

sampdt[,res:= fit$residuals ]
ggplot(sampdt,aes(x=res)) + geom_histogram(bins=10,fill ='blue',color='white') 
+ labs(title = "Normality?")

sampdt[,pred := fit$fitted.values ]
ggplot(sampdt,aes(x=pred ,y=res)) + geom_point(color ='purple') + 
geom_smooth(method = 'lm') + labs ( title ="Equal Variance ?")

#multiple regression
model2 = lm(revenues ~ google_adwords + facebook + twitter,data = advert )
plot(advert )

resdf2 = data.table(res = model2$residuals,pred=model2$fitted.values)

ggplot(resdf2,aes(x=res)) + geom_histogram(bins =10 ,fill ='blue',color ='white')
ggplot(resdf2,aes(sample=res)) + stat_qq(color =" blue") + stat_qq_line()
ggplot(resdf2,aes(x=pred,y=res)) + geom_point (color ='purple') 
+ geom_smooth(method = 'lm')

summary(model2)

# Residual standard error: 2.214 on 168 degrees of freedom
# Multiple R-squared:  0.8585,	Adjusted R-squared:  0.856 
# F-statistic: 339.8 on 3 and 168 DF,  p-value: < 2.2e-16
qf(.95,df1=3, df2=168)
# gives value 2.658399



