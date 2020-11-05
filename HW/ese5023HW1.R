## 1. Flow Chart
Print_values<-function(a,b,c){
  if (a>b){
    if(b>c){
      print(paste(a,',',b,',',c))
    }
    else{
      if(a>c){
        print(paste(a,',',c,',',b))
      }
      else{
        print(paste(c,',',a,',',b))
      }
    }
  }
  else{
    if(b>c){
      if(a>c){
        print(paste(c,',',a,',',b))
      }
    }else{
      print(paste(c,',',b,',',a))
    }
  }
}
A<-runif(3,min=1,max=100)
Print_values(A[1],A[2],A[3])







##2. Matrix multiplication
#2.1 
M1<-matrix(runif(50,min = 0,max = 50),nrow = 5,ncol = 10)
M2<-matrix(runif(50,min = 0,max = 50),nrow = 10,ncol = 5)
#2.2
Matrix_multip<-function(a,b){
  rown<-dim(a)[1]
  coln<-dim(b)[2]

  mat_re<-matrix(nrow=rown,ncol = rown)
  for (k in 1:rown) {
    for (i in 1:rown) {
      mat_cal<-0
      for (j in 1:coln) {
      mat_cal<-mat_cal+a[k,j]*b[j,i]  
      }
      mat_re[k,i]<-mat_cal
      
    }
  }
  print(mat_re)
}
Matrix_multip(M1,M2)








## 3. Pascal Triangle
Pascal_triangle<-function(k){
  tri_met<-array(0,dim = c(k,k))
  
  tri_met[,1]<-1
  for (i in 1:k){
    tri_met[i,i]<-1
  }

  for (Xth_row in 3:k) {
    for (Xth_col in 2:(k-1)){
      tri_met[Xth_row,Xth_col]<-tri_met[Xth_row-1,Xth_col-1]+
      tri_met[Xth_row-1,Xth_col]
     }
      
   }
  print(tri_met[k,])  
}
 
Pascal_triangle(10)







## 4.Add or Double 
Least_moves<-function(m){
  count=0
  while ( m > 1) {
    if(m%%2==1){
      m=m-1
      count=count+1
    }
    else{
      m=m/2
      count=count+1
    }
  }
  print(count)
}



## 5.Dynamic Programming
#5.1
library(gtools)
Find_expression<-function(number){
  operations<-permutations(3,8,v=c('+','-',''),set=FALSE,repeats.allowed=TRUE)
  numbers<-matrix(nrow = 6561,ncol = 9)
  solutions<-array()
  for (i in 1:nrow(operations)) {
    numbers[i,]<-c('1','2','3','4','5','6','7','8','9')
  }
  
  solcoun<-1
  count<-1
  expre<-matrix(nrow = 6561,ncol = 1)
  cal<-matrix(nrow = 6561,ncol = 1)
  
  for (op in 1:nrow(operations)) {
    expre[count,1]<-'1'
    
    for (j in 1:8) {
      expre[count,1]<-paste(expre[count,1],operations[op,j],numbers[op,(j+1)],sep = '')
      
    }
    cal[count,1]<-eval(parse(text = expre[count,1]))
    if(cal[count,1]==number) {
      solutions[solcoun]<-paste(expre[count,1],'=',number,sep = '')
      solcoun<-solcoun+1
    }
    count=count+1
  }
  print(solutions)
  return(solcoun)
}

Find_expression(50)
#5.2
Total_solutions<-c()
for (i in 1:100) {
  Total_solutions[i]<-Find_expression(i)
}

plot(c(1:100),Total_solutions,type = 'l',xlab = 'The number taken',
     ylab = 'The number of operation',col = 'blue',
     main = 'The possible methods for solutions from 1 to 100',
     cex = 1.5, lwd = 1.5,pch = 16, lty = 1,
     cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)

NumOfMax<-which(Total_solutions==max(Total_solutions))
NumOfMin<-which(Total_solutions==min(Total_solutions))
print(paste('The max amount of methods happened when number is',NumOfMax))
print(paste('The min amount of methods happened when number is',NumOfMin))

# ## 5. Dynamic Programming
# # 5.1
# Find_expression<-function(number){
#   sign_place<-list()
#   PorM_seq<-list()
#   opera<-c()
#   savepart<-matrix()
#   savepart_num<-matrix()
#   nums<-'123456789'
#   count=1
#   for (sign_num in 1:8) {
#     sign_place[[sign_num]]<-combinations(8,sign_num)
#     PorM_seq[[sign_num]]<-combinations(2,sign_num,v=c('P','M'),
#                            set=FALSE,repeats.allowed=TRUE)
# 
# 
# 
# 
#     for (i in 1:nrow(sign_place[[sign_num]])) {
#       for (j in 1:sign_num) {
#         if(j==1){
#           savepart[i,1]<-substr(nums,1,sign_place[[sign_num]][i,1])
# 
#         }
# 
#         else if(j==sign_num){
#           savepart[i,(sign_num+1)]<-substr(nums,sign_place[[sign_num]][i,sign_num],9)
# 
#         }
# 
#         else{
#         savepart[i,j]<-substr(nums,((sign_place[[sign_num]][i,(j-1)]+1)),
#                 sign_place[[sign_num]][i,j])
#         }
#       }
# 
#     }
#    savepart_num<-as.integer(savepart)
# 
# 
# 
# 
#    for (i in 1:nrow(savepart_num)) {
# 
#      for (k in 1:nrow(PorM_seq[[sign_num]])) {
#       opera[count]<-savepart_num[i,1]
#       for (j in 2:ncol(savepart_num)) {
# 
# 
#         if(PorM_seq[[sign_num]][k,j]=='P'){
#             opera[count]<-savepart_num[i,j]+opera[count]
#           }
#         else if(PorM_seq[[sign_num]][k,j]=='M'){
#             opera[count]<-opera[count]-savepart_num[i,j]
#           }
#       count=count+1
# 
# 
#       }
#       if(opera[count]==number){
#         print(c(savepart_num[i,],PorM_seq[[sign_num]][k,]))
#       }
#       }
# 
#    }
# 
# 
#   }
# 
# 
# }







## 6.  Visibility in Shenzhen during the past 10 years
Station_data <- read.csv(file = "2281305.csv", header = T)
Vis<-Station_data$VIS
Vis_data<-substr(Vis,1,6)
Vis_con<-substr(Vis,8,12)
Vis_data[which(Vis>=160000)]<-NA

Vis_data2<-as.numeric(Vis_data)

Vis_con2<-Vis_data2[which(Vis_con=='1,N,1')]
time_hour<-Station_data$DATE[which(Vis_con=='1,N,1')]
time_hour_fac<-as.factor(time_hour)
plot(time_hour_fac,Vis_con2,type = 'l',xaxt='n',xlab = 'Year',
     ylab = 'Visibility(km)',col = 'blue',
     main = 'The visibility of Shenzhen during year 2010-2013',
     cex = 1.5, lwd = 1.5,pch = 16, lty = 1,
     cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)
axis(side=1,at=c(1,8712,17443,26203), 
     labels=c("2010","2011","2012","2013"))

times<-substr(time_hour,1,4)
dates<-substr(time_hour,1,10)
Vis_div<-c()
for (y in 2010:2013) {
  y2<-as.character(y)

  Vis_div1<-Vis_con2[which(times==y2)]
  Oneyear<-dates[which(times==y2)]
  for (Thisday in unique(dates)) {
    Vis_div<-c(Vis_div,max(Vis_div1[which(Oneyear==Thisday)]))
    }
  int1<-length(which(Vis_div<5000))
  int2<-length(which(Vis_div>=5000& Vis_div<10000))
  int3<-length(which(Vis_div>=10000& Vis_div<15000))
  int4<-length(which(Vis_div>=15000& Vis_div<20000))
  int5<-length(which(Vis_div>=20000& Vis_div<25000))
  int6<-length(which(Vis_div>=25000& Vis_div<30000))
  int7<-length(which(Vis_div>=30000))
  
  print(c('In the year ',as.character(i2)))
  print(c(int1,int2,int3,int4,int5,int6,int7))
  Vis_div_tokm<-Vis_div/1000
  dev.new()
  hist(Vis_div_tokm, breaks =8, xlab='Visibility(km)',
       ylab = 'Days',
       main = paste("Hist of visibility in",y2))
  Vis_div<-c()
  }






## 7. Explore a data set

#Downloaded one of the Madrid / Barajas stations data from 1991-1995
Madrid<-read.csv('c82210-1.csv', header = T)
Years<-Madrid$Y
Months<-Madrid$M
Days<-Madrid$D
AvgTemp<-Madrid$AT
MaxT<-Madrid$TM
MinT<-Madrid$Tm
YandM<-c()
Diff<-MaxT-MinT

Dates<-c()
ThisMonthTemp<-c()
ThisYearTempMax<-c()
ThisYearTempMin<-c()
ThisMonthTempDiff<-c()
# plot daily average temperature
for (i in 1:nrow(Madrid)) {
  Dates[i]<-paste(Years[i],Months[i],Days[i],sep = '-')
  YandM[i]<-paste(Years[i],Months[i],sep = '-')
}

Dates2<-as.Date(Dates,)
plot(Dates2,AvgTemp,type = 'l',xlab = 'Years',
     ylab = 'Daily average temperature(¡æ)',col = 'blue',
     main = 'Time series of daily average temperature',
     cex = 1.5, lwd = 1.5,pch = 16, lty = 1,
     cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)

#calculate monthly mean temperature and plot
count1<-1
for (year in unique(Years)) {
  for (month in unique(Months)) {
  ThisMonthTemp[count1]<-mean(AvgTemp[which(YandM==paste(year,month,sep = '-'))])  
  count1<-count1+1  
  }
}
plot(c(1:60),ThisMonthTemp,type = 'l',xlab = 'The nth months',
     ylab = 'Monthly mean temperature(¡æ)',col = 'blue',
     main = 'Time series of monthly mean temperature',
     cex = 1.5, lwd = 1.5,pch = 16, lty = 1,
     cex.axis = 0.8, cex.lab = 0.8, cex.main = 1) 


#calculate yearly max temperature and plot
count2<-1
for (year in unique(Years)) {
     ThisYearTempMax[count2]<-max(MaxT[which(Years==year)])  
     count2<-count2+1  
     
  
}
plot(c(1991:1995),ThisYearTempMax,type = 'l',xlab = 'Years',
     ylab = 'Yearly max temperature(¡æ)',col = 'blue',
     main = 'Time series of yearly max temperature',
     cex = 1.5, lwd = 1.5,pch = 16, lty = 1,
     cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)




#calculate yearly min temperature and plot
count2<-1
for (year in unique(Years)) {
  ThisYearTempMin[count2]<-min(MaxT[which(Years==year)])  
  count2<-count2+1  
  
  
}
plot(c(1991:1995),ThisYearTempMin,type = 'l',xlab = 'Years',
     ylab = 'Yearly min temperature(¡æ)',col = 'blue',
     main = 'Time series of yearly min temperature',
     cex = 1.5, lwd = 1.5,pch = 16, lty = 1,
     cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)


#calculate difference of monthly temperature 
count1<-1
for (year in unique(Years)) {
  for (month in unique(Months)) {
    ThisMonthTempDiff[count1]<-mean(Diff[which(YandM==paste(year,month,sep = '-'))])  
    count1<-count1+1  
  }
}  
  plot(c(1:60),ThisMonthTempDiff,type = 'l',xlab = 'The nth month',
       ylab = 'Temperature(¡æ)',col = 'blue',
       main = 'Time series of monthly difference of max and min temperature',
       cex = 1.5, lwd = 1.5,pch = 16, lty = 1,
       cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)
# try find statistically meanings of monthly 
  #temperature differences
  AutoCo<-acf(ThisMonthTempDiff)# It seems that there is
  #correlations within the monthly differences temperature
  library(lmtest)
  dw<-dwtest(ThisMonthTempDiff)

  
  
  