outliers = function(v){
  q3 = quantile(v,0.75)
  q1 = quantile(v, 0.25)
  iqr = IQR(v)
  upper = q3 + 1.5*iqr
  lower = q1 - 1.5*iqr
  greaters = which(v>upper)
  #print(greaters)
  lessers = which(v<lower)
  #print(lessers)
  result = list("greaters" = greaters, "lessers" = lessers, "Uppper_limit" = upper, "lower_limit"= lower)
  return(result)
}


d = read.csv("OnlineNewsPopularity.csv")
d$Popularity = as.numeric(d$shares > 2500)
d$url = NULL
d$Popularity = factor(d$Popularity)
library(dplyr)
popularity = d %>%
  group_by(Popularity) %>%
  summarise(count = n()/nrow(d)[1])




categoricals = c( "data_channel_is_lifestyle",
                  "data_channel_is_entertainment",
                  "data_channel_is_bus",
                  "data_channel_is_socmed",
                  "data_channel_is_tech",
                  "data_channel_is_world",
                  "weekday_is_monday",
                  "weekday_is_tuesday",
                  "weekday_is_wednesday",
                  "weekday_is_thursday",
                  "weekday_is_friday",
                  "weekday_is_saturday")


d$is_weekend = NULL
d$weekday_is_sunday = NULL

#Convert to factor
d.backup = d
for(col in categoricals){
  d[,col] = factor(d[,col])
}


numericals = c("timedelta",
               "n_tokens_title",
               "n_tokens_content",
               "n_unique_tokens",
               "n_non_stop_words",
               "n_non_stop_unique_tokens",
               "num_hrefs",
               "num_self_hrefs",
               "num_imgs",
               'num_videos',
               "average_token_length",
               "num_keywords",
               "kw_min_min",
               'kw_max_min',
               'kw_avg_min',
               "kw_min_max",
               "kw_max_max",
               "kw_avg_max",
               "kw_min_avg",
               "kw_max_avg",
               "kw_avg_avg",
               "self_reference_min_shares",
               "self_reference_max_shares",
               "self_reference_avg_sharess",
               "LDA_00",
               "LDA_01",
               "LDA_02",
               "LDA_03",
               "LDA_04",
               "global_subjectivity",
               "global_sentiment_polarity",
               "global_rate_positive_words",
               "global_rate_negative_words",
               "rate_positive_words",
               "rate_negative_words",
               "avg_positive_polarity",
               "min_positive_polarity",
               'max_positive_polarity',
               "avg_negative_polarity",
               "min_negative_polarity",
               "max_negative_polarity",
               "title_subjectivity",
               "title_sentiment_polarity",
               "abs_title_subjectivity",
               "abs_title_sentiment_polarity"
)


#Impute missing with mean
#Clamp outliers
#Normalize
outs = outliers(d$timedelta)
for(col in numericals){
  mu = mean(d[,col], na.rm = T)
  missing = which(is.na(d[,col]))
  d[missing,col] = mu #Imputation over
  
  outs = outliers(d[,col])
  d[outs$greaters, col] = outs$Uppper_limit
  d[outs$lessers, col] = outs$lower_limit #Clamping over
  
  d[,col] = as.vector(scale(d[,col])) #Normalization over
}
str(d)
temp = as.vector(scale(d.backup$kw_max_max))
d$kw_max_max = temp #For some reason this showed up NAN, so I fixed this manually

#Step 2---------------------------------------------------Remove multicollinearity---------------------------------
library(car)
cor.model = lm(shares~.-Popularity, data=d)
cor.model = step(cor.model)
formula(cor.model)

variables.without.cor = c("timedelta" , "n_tokens_title" , "n_unique_tokens" , "n_non_stop_words" , 
                          "n_non_stop_unique_tokens" , "num_hrefs" , "num_self_hrefs" , "num_imgs", 
                          "num_videos" , "data_channel_is_lifestyle" , "data_channel_is_entertainment" , 
                          "kw_min_avg" , "kw_avg_avg" , "self_reference_avg_sharess" , "weekday_is_monday" , 
                          "weekday_is_saturday" , "LDA_02" , "global_subjectivity" , "global_rate_positive_words" , 
                          "avg_positive_polarity" , "min_positive_polarity" , "max_positive_polarity" , 
                          "min_negative_polarity" , "max_negative_polarity" , "abs_title_subjectivity" , 
                          "abs_title_sentiment_polarity","Popularity")

dataset.cleaned = d[,variables.without.cor]

dim(dataset.cleaned)
remaining.numericals = intersect(numericals, variables.without.cor)
remaining.categoricals = setdiff(variables.without.cor, remaining.numericals)


#Step 3 Fit a Decision Tree to it---------------------------------------------------------------
library(rpart)
library(rpart.plot)
0.01*nrow(dataset.cleaned)
View(dataset.cleaned[1:10,])


n = ceiling(0.7*nrow(dataset.cleaned))
all = 1:nrow(dataset.cleaned)
ind = sample(all, n)
others = setdiff(all, ind)
training.data = dataset.cleaned[ind,]
testing.data = dataset.cleaned[others,]
dim(training.data)
dim(testing.data)


default.tree = rpart(data = training.data,
                     Popularity~.,
                     control = rpart.control(xval = 10, cp = 0.00000001,
                                             maxdepth = 5)) #400 is roughly 1% of the number of records
prp(default.tree, varlen = 30)



#Step 4 Prediction and Evaluation_________________________________________

y_hat = predict(default.tree, newdata = testing.data, method='class')
y_hat = y_hat[,2]
head(y_hat)
library(pROC)
table(testing.data$Popularity)
class(testing.data$Popularity)

r=roc(testing.data$Popularity, y_hat)
plot(r)

library(caret)
hist(y_hat)
cutoff = 0.4
c=confusionMatrix(testing.data$Popularity, factor(as.numeric(y_hat>0.5)))
var(default.tree)
library(ggplot2)
temp=data.frame(names = names(default.tree$variable.importance),
                values = as.numeric(default.tree$variable.importance) )

temp = temp[order(temp$values),]
temp$names = factor(temp$names, levels = temp$names)

variable.importance.plot = ggplot(temp) + geom_col(aes(y=names, x=values))




# Some Visualization about these important variables


#To get the original data
for(col in categoricals){
  d.backup[,col] = factor(d.backup[,col])
}

for(col in numericals){
  mu = mean(d.backup[,col], na.rm = T)
  missing = which(is.na(d.backup[,col]))
  d.backup[missing,col] = mu #Imputation over
  
  outs = outliers(d.backup[,col])
  d.backup[outs$greaters, col] = outs$Uppper_limit
  d.backup[outs$lessers, col] = outs$lower_limit #Clamping over
}



str(d.backup)
timedelda.plot = ggplot(d.backup) +
  geom_histogram(aes(x=timedelta), bins = 10, color="gray") +
  ggtitle('Timedelta Distribution') 
ggsave("C:\\USA\\UTD college\\Semesters\\Sem 1 Srping 22\\Ba with R Buan 6356\\Projects\\OnlineNewsPopularity\\OnlineNewsPopularity\\Stage 2 Code\\Images\\Timedelta2.png")


write.csv(x = dataset.cleaned, file = "Cleaned Dataset.csv")
