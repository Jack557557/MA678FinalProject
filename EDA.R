data<-read.csv("data.csv")

# data cleaning
data$waterfront<-NULL
data$date<-NULL
data$sqft_above<-NULL
data$sqft_basement<-NULL
data$statezip7<-NULL
data$yr_built<-NULL
data$yr_renovated<-NULL
data$street<-NULL
data$city<-NULL
data$country<-NULL

#EDA
#2.2.1  House Price Distribution
library(gglot2)
ggplot(data=data, aes(x=price)) +
  geom_histogram(fill="blue", binwidth = 10000) +xlim(0,2500000)

#2.2.2	Correlations visualization
library(corrplot)
cor_numVar <- cor(data, use="pairwise.complete.obs")
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#2.2.3 sqft_living
ggplot(data=data, aes(x=sqft_living, y=price))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+ylim(0,2500000)

#2.2.4 Elementary School Rate
ggplot(data=data, aes(x=factor(ES), y=price))+
  geom_boxplot(col='blue') + labs(x='Elementary School Rate') +
  ylim(0,2500000)

#2.2.5 Extra EDA

#bedrooms

ggplot(data=data, aes(x=as.factor(data$bedrooms), y=price))+
  geom_point(col='blue') + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +ylim(0,1000000)

  ggplot(data=data, aes(x=as.factor(bedrooms))) +
  geom_histogram(stat='count')

#bathrooms  
  
  ggplot(data=data, aes(x=as.factor(data$bathrooms), y=price))+
    geom_point(col='blue') + 
    geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +ylim(0,1000000)
  
  ggplot(data=data, aes(x=as.factor(bathrooms))) +
    geom_histogram(stat='count')
 
# sqft_lot
  ggplot(data=data, aes(x=sqft_lot, y=price))+
    geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+xlim(0,200000)+ylim(0,2500000)

# floors 
  ggplot(data=data, aes(x=as.factor(data$floors), y=price))+
    geom_point(col='blue') + 
    geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +ylim(0,1000000)
  
  ggplot(data=data, aes(x=as.factor(floors))) +
    geom_histogram(stat='count')

# view
  ggplot(data=data, aes(x=as.factor(data$view), y=price))+
    geom_point(col='blue') + 
    geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +ylim(0,1000000)
  
  ggplot(data=data, aes(x=as.factor(view))) +
    geom_histogram(stat='count')
  
# condition
  ggplot(data=data, aes(x=as.factor(data$condition), y=price))+
    geom_point(col='blue') + 
    geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +ylim(0,1000000)
  
  ggplot(data=data, aes(x=as.factor(condition))) +
    geom_histogram(stat='count')
  
# MS
  ggplot(data=data, aes(x=factor(MS), y=price))+
    geom_boxplot(col='blue') + labs(x='Elementary School Rate') +
    ylim(0,2500000)
  
# HS
  ggplot(data=data, aes(x=factor(HS), y=price))+
    geom_boxplot(col='blue') + labs(x='Elementary School Rate') +
    ylim(0,2500000)
  
# Age
  ggplot(data=data, aes(x=age, y=price))+
    geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+xlim(0,120)+ylim(0,2500000)

# Income 
  ggplot(data=data, aes(x=income, y=price))+
    geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+xlim(20000,140000)+ylim(0,2500000)
  