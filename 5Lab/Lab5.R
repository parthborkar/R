library ( data.table )
bike = copy (Ch5_bike_station_locations)
setDT ( bike )
str ( bike )

#check NA
grep ('NA',bike )

library(ggplot2)
ggplot(bike ,aes (x= longitude ,y= latitude )) + geom_point()

#set.seed will give us the same ans
set.seed (123)
k3= kmeans (bike ,3)
k3

#(between_SS / total_SS =  63.7 %)
# Error Rate = Between Cluster Sum of Squares / All Sum of Squares
#                    points
# Sum of Squares = summation (xi - meanx )^2 + (yi - meany )^2
#                     i=1
#If each individual point was its own cluster, then the error rate would be 100%.

#visualize the clusters after adding a new column
bike [, clus3 := k3$ cluster ]
ggplot (bike ,aes (x= longitude ,y= latitude , color = clus3 )) + geom_point()

# since it is numerical column we need to get distinct colors by factoring
bike [, clus3 := factor ( clus3 )]
ggplot (bike ,aes (x= longitude ,y= latitude , color = clus3 )) + geom_point ()

#visualize the stations center of the clusters which we will store in data container
# called matrix/ array, since there are 2 values
class(k3$ centers)

#gives the value of 1 row 1 column
k3$ centers [1 ,1]

#conv matrix to dt
centdt = data.table (k3$ centers )
centdt

#adding the center points to the cluster graph graph, we get
ggplot(bike,aes(x=longitude ,y=latitude ,color=clus3)) + 
  geom_point() +
  geom_point(data=centdt ,aes(x=longitude ,y=latitude), colour ="purple",
             shape =11,size =2)

# now well try for 2 bike stations
k2 = kmeans ( bike [ ,.( latitude , longitude )] ,2)
k2
bike [, clus2 := k2$ cluster ]

bike [, clus2 := factor ( clus2 )]
centdt2 = data.table (k2$ centers )
ggplot (bike ,aes (x= longitude ,y= latitude , color = clus2 )) +
  geom_point () +
  geom_point ( data = centdt2 ,aes (x= longitude , y= latitude ),
                   colour =" black ", shape =19 , size =2) 

#combining 3 and 2 stations to get center location
ggplot(bike ,aes (x= longitude ,y= latitude , color = clus3 )) +
geom_point()+ geom_point(data =centdt ,aes (x=longitude,y= latitude),colour="purple", 
                         shape =11,size =2) +
geom_point(data=centdt2 ,aes (x=longitude , y=latitude),colour ="black", 
           shape =19 , size =2)

#using geosphere to get the actual distances in Meters, we have to divide by 1690
install.packages("geosphere")
library ( geosphere )

#eg
dd= distm(c(38.88838, -76.97846),c(38.93855, -77.03975),fun=distHaversine) / 1690
dd
class(dd)

#actual distance of bike from all 3 kiosk
res_matrix = distm ( bike [ ,.( latitude , longitude )],centdt ,
                       fun= distHaversine )/ 1609
head(res_matrix)
res_matrix

# load into bike as.data.table matrix to dt
bike [,c('k31 ','k32 ','k33 '):= as.data.table (res_matrix )]

# min distance when you have 3 clusters
bike[clus3 ==1 , c3dist := k31 ]
bike[clus3 ==2 , c3dist := k32 ]
bike[clus3 ==3 , c3dist := k33 ]

# 2 matrix
res_matrix2 = distm ( bike [ ,.( latitude , longitude )], centdt2
                        ,fun= distHaversine )/ 1609
head(res_matrix2 )

bike [,c('k21 ','k22 '):= as.data.table (res_matrix2 )]
bike [ clus2 ==1 , c2dist := k21 ]
bike [ clus2 ==2 , c2dist := k22 ]

#delete col
bike [,c('k21 ','k22 '):= NULL ]

#distances
StaDist = bike [ ,.( clus3 ,k31 ,k32 ,k33 )]
# We now want to nd 3 scenarios:
# For kiosk 1 bike stations, nd the smaller distance between kiosk 2 or 3
# For kiosk 2 bike stations, nd the smaller distance between kiosk 1 or 3
# For kiosk 3 bike stations, nd the smaller distance between kiosk 1 or 2
StaDist [, head (.SD ,5) ,by = clus3 ]

# gives the mean
StaDist [, rowMeans (.SD) ,.SDcols =2:4]
StaDist [, rowMeans (.SD) ,.SDcols =c('k31 ','k32 ','k33 ')]

# min dis without one of the kiosk mentioned
StaDist [, pmin (k31 ,k32 ,k33 )]
StaDist [,c('noK1 ','noK2 ','noK3 '):= pmin (k31 ,k32 ,k33)]
StaDist [ clus3 ==1 , noK1 := pmin (k32 ,k33 )]
StaDist [ clus3 ==2 , noK2 := pmin (k31 ,k33 )]
StaDist [ clus3 ==3 , noK3 := pmin (k31 ,k32 )]
StaDist [ ,.( mean ( noK1 ),mean ( noK2 ),mean ( noK3 ))]
















