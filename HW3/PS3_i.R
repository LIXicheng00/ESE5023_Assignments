library(tidyr)
library(dplyr)
library(ggplot2)

#1 Cloud Seeding
##1.1
seedday<-read.csv(file = 'ese5023hw3_1.csv',header = T)
class(seedday)

seedday_tbl<-as_tibble(seedday) %>% 
  mutate(character=factor(character,ordered = T)) 

seedday_tbl %>%
  group_by(character) %>%
  summarise(
    count = n(),
    mean_rainfall = mean(rainfall, na.rm = TRUE),
    sd_rainfall = sd(rainfall, na.rm = TRUE)
  )
    
seedday_tbl%>% 
  ggplot(aes(x=character,y=rainfall,fill=character))+
  geom_boxplot() +
  theme_classic()#Seems there is difference between two dataset.
##1.2
#Normality 
uns<-seedday_tbl %>%
  filter(character=='1') %>% 
  pull(rainfall)
s<-seedday_tbl %>%
  filter(character=='2') %>% 
  pull(rainfall)

shapiro.test(uns)#p-value = 3.134e-07
shapiro.test(s)#p-value = 1.411e-06
#The two datasets do not obey normal distribution

#Homogeneity of variance
bartlett.test(rainfall~character,data=seedday_tbl)#p-value = 6.754e-05
#Reject the hypothesis

anova_one_way<-aov(rainfall ~ character,data = seedday_tbl)
summary(anova_one_way)
#The Pr() is 0.0511, which can just reject the hypothesis and have
#little significance.But from the boxplot, it seems there are differences 
#between the two sets.

#2. Was Tyrannosaurus Rex Warm-Blooded?
bone<-read.csv(file = 'ese5023hw3_2.csv',header = T)

bone_tbl<-as_tibble(bone) %>% 
  mutate(names=factor(names,ordered = T)) 
glimpse(bone_tbl)

bone_tbl%>% 
  ggplot(aes(x=names,y=comp,fill=names))+
  geom_boxplot() +
  theme_classic()

anova_one_way2<-aov(comp ~ names,data = bone_tbl)
summary(anova_one_way2)#The Pr(>F)=9.73e-07 ***, which means the variances are  
#significantly differences.
#Thus the conclusion is that Tyrannosaurus Rex is not warm-blooded.
TukeyHSD(anova_one_way2)


#3. Vegetarians and Zinc
veg<-read.csv(file = 'ese5023hw3_3.csv',header = T)

veg_tbl<-as_tibble(veg) %>% 
  mutate(team=factor(team,ordered = T)) 
glimpse(veg_tbl)

veg_tbl%>% 
  ggplot(aes(x=team,y=level,fill=team))+
  geom_boxplot() +
  theme_classic()

veg_tbl %>%
  group_by(team) %>%
  summarise(
    count = n(),
    mean_level = mean(level, na.rm = TRUE),
    sd_level = sd(level, na.rm = TRUE)
  )#On simple mean aspect, The zinc level in Pregnant Vegetarians is
#slightly less than that in nonpregnant vegetarians.
#Normality
name3<-unique(veg$team)
p_nv<-veg_tbl %>%
  filter(team==name3[1]) %>% 
  pull(level)
p_v<-veg_tbl %>%
  filter(team==name3[2]) %>% 
  pull(level)
np_v<-veg_tbl %>%
  filter(team==name3[3]) %>% 
  pull(level)

shapiro.test(p_nv)#p-value = 0.03533 reject
shapiro.test(p_v) #p-value = 0.1418
shapiro.test(np_v)#p-value = 0.8142

#Homogeneity of variance
bartlett.test(level~team,data=veg_tbl)#p-value = 0.445

anova_one_way3<-aov(level~team,data=veg_tbl)
summary(anova_one_way3)#Pr(>F)=0.982, accept the hypothesis.
#There is no distinct differences in variance among the three different groups.
#Although the means have slight differences.
TukeyHSD(anova_one_way3)

#4. Atmospheric Lapse Rate
Huawei<-read.csv(file = 'ese5023hw3_4.csv',header = T)

linearr4 <- lm(Temperature.degrees.C.~Elevation.m., data = Huawei)
summary(linearr4)
coef(linearr4)

plot4<-ggplot(Huawei, aes(x=Elevation.m., y=Temperature.degrees.C.))+
  geom_point()+
  geom_smooth(method = "lm")

l<- list(a = as.numeric(format(coef(linearr4)[1], digits = 4)),
          b = as.numeric(format(coef(linearr4)[2], digits = 4)),
          r2 = format(summary(linearr4)$r.squared, digits = 4),
          p = format(summary(linearr4)$coefficients[2,4], digits = 4))
eq <- substitute(italic(y) == a + b %.% italic(x)~","~
                   italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
#Methods of adding text on the plot
#refer to:https://blog.csdn.net/weixin_43948357/article/details/105336901
plot4 + geom_text(aes(x = 500, y = 20, 
                  label = as.character(as.expression(eq))),
                  parse = TRUE,size = 2.5)
#The lapse rate here is 9.312 degrees C km-1.

#5. The Big Bang Theory
bbt<-read.csv(file = 'ese5023hw3_5.csv',header = T)
##5.1&5.2
plot5<-ggplot(bbt, aes(x=Velocity, y=Distance))+
  geom_point()+
  geom_smooth(method = "lm")
#It seems there is a trend in point distribution but more scattered.
linearr5 <- lm(Distance~Velocity, data = bbt)
summary(linearr5)
coef(linearr5)
l5<- list(a5 = as.numeric(format(coef(linearr5)[1], digits = 4)),
         b5 = as.numeric(format(coef(linearr5)[2], digits = 4)),
         r25 = format(summary(linearr5)$r.squared, digits = 4),
         p5 = format(summary(linearr5)$coefficients[2,4], digits = 4))
eq5 <- substitute(italic(y) == a5 + b5 %.% italic(x)~","~
                   italic(R)^2~"="~r25~","~italic(P)~"="~p5, l5)
plot5 + geom_text(aes(x = 400, y = 2.5, 
                      label = as.character(as.expression(eq5))),
                  parse = TRUE,size = 2.5)
##5.3
###The explanation to the two assumptions:
###a.The intercept should be zero:
###If the BBT is correct, the fact is that the outer celestial body
###has a larger velocity compared to the inner celestial bodies derided from
###red shift. Thus, celestial bodies close to each other should have similar 
###velocity. This leads to if the velocity(relative velocity) is 0,and thus 
###the distance should be very close to 0.
###b.The slope is the age of the universe:
###The slope is Distance/Velocity and the unit should be time. If the Theory 
###is correct, the universe continues to expand since singular point. 
###The expand time is the age of the universe.

###The intercept is 0.3991 because the detection is not particularly accurate.
###From the handwriting calculation, the age of universe 
###is only 1.345*10^(9) years.


##5.4
###As explained in 5.3,the improvement of distance measurement will probably 
###enhance the R^2 as well as the fitting slope and intercept.


#6. CPU Performance
library(MASS)
data(cpus)
##6.1
hist(cpus$perf)
hist(log(cpus$perf))#The log data is better normally but I did not get
#the idea of whether should be logarithmetics because other parameters are
#all skewed.
# hist(cpus$syct)
# cpus<-cpus %>% 
#   mutate(perf_log = log(perf))

sample_index <- sample(nrow(cpus),nrow(cpus)*0.80)
train <- cpus[sample_index,]
test  <- cpus[-sample_index,]
model6 <- lm(perf~syct+mmin+mmax+cach+chmin+chmax, data=train)
summary(model6)
##6.2
coef(model6)
perf_predict <- predict(model6,test)
plot(test$perf, perf_predict)
cor(test$perf, perf_predict)
mean(perf_predict)
mean(test$perf)
(mean(perf_predict) - mean(test$perf))/
  mean(test$perf)*100

#7. Analysis of Data Sets from Your Group
satisfying<-read.csv(file = 'ese5023hw3_7.csv',header = T)
satisfying_tbl<-as_tibble(satisfying) %>% 
  mutate(group=factor(group,ordered = T)) 
glimpse(satisfying_tbl)

satisfying_tbl%>% 
  ggplot(aes(x=group,y=satisfying.level,fill=group))+
  geom_boxplot() +
  theme_classic()

##7.1
###Question:Whether the satisfaction are difference between service and delivery.
#Normality
name7<-unique(satisfying$group)
service<-satisfying_tbl %>%
  filter(group==name7[1]) %>% 
  pull(satisfying.level)
deliver<-satisfying_tbl %>%
  filter(group==name7[2]) %>% 
  pull(satisfying.level)
price<-satisfying_tbl %>%
  filter(group==name7[3]) %>% 
  pull(satisfying.level)

shapiro.test(service)#p-value = 0.7004
shapiro.test(deliver) #p-value = 0.6326
shapiro.test(price)#p-value = 0.6845
#All be accepted.

#Homogeneity of variance
bartlett.test(satisfying.level~group,data=satisfying_tbl)#p-value = 0.7182
#The data are homogeneity

#Do t_test to service satisfaction and deliver satisfaction.
t.test(service, deliver)
#Showing that for different types, the differences of variance are significant.

##7.2
###Question:Do the three satisfactions have significant differences?
#We have exam the normality and the Homogeneity
anova_one_way7<-aov(satisfying.level~group,data=satisfying_tbl)
summary(anova_one_way7)#Pr(>F)=0.0109, reject the hypothesis.
#The three satisfactions have significant differences among them.

##7.3 
###Question:Is there some relationship between delivery satisfaction and 
###service satisfaction?
data7<-data.frame(service,deliver)
plot7<-ggplot(data7,aes(x=deliver, y=service))+
  geom_point()+
  geom_smooth(method = "lm")
#It seems there is a trend in point distribution.
linearr7 <- lm(service~deliver, data = data7)
summary(linearr7)
coef(linearr7)
l7<- list(a7 = as.numeric(format(coef(linearr7)[1], digits = 4)),
          b7 = as.numeric(format(coef(linearr7)[2], digits = 4)),
          r27 = format(summary(linearr7)$r.squared, digits = 4),
          p7 = format(summary(linearr7)$coefficients[2,4], digits = 4))
eq7 <- substitute(italic(y) == a7 + b7 %.% italic(x)~","~
                    italic(R)^2~"="~r27~","~italic(P)~"="~p7, l7)
plot7 + geom_text(aes(x = 4.2, y = 4.5, 
                      label = as.character(as.expression(eq7))),
                  parse = TRUE,size = 2.5)
##From the graph, the R_square is 0.4549.
##There is limited relationship between deliver satisfaction 
##and service satisfaction.


