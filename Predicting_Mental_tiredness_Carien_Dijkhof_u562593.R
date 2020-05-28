#------------------------------------------------------------------------------#
#---------- PREDICTING MENTAL TIREDNESS ON SOCIAL MEDIA USAGE -----------------#
#------------------------------------------------------------------------------#

# Carien Dijkhof
# U562593 // 2029730
# A. Hendrickson & B. Nicenboim
# Tilburg University

#------------------------------------------------------------------------------#
#------------------------------ USED LIBRARIES --------------------------------#
#------------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(factoextra)
library(corrplot)
library(forcats)
#library(MASS)

#------------------------------------------------------------------------------#
#------------------------------ ClEANED MOOD ----------------------------------#
#------------------------------------------------------------------------------#

# make mood dataset 
mood <- read.table("mood_sampling_data.csv", header = TRUE, sep = ",")
mood_sampling_data <- read.table("mood_sampling_data.csv", 
                                 header = TRUE, sep = ",")

# filter out the NAs abd errorred user-IDs.
mood <- mood %>%
  filter(duration != "Expired",
         duration != "Canceled",
         duration != "Blocked",
         duration != "Unknown") %>%
  filter(tired == 0 | tired == 1 | tired == 2 | 
           tired == 3 | tired == 4 | tired == 5) %>%
  filter(user_id != 10175, user_id != 10194, 
         user_id != 10199, user_id != 10217, 
         user_id != 10218, user_id != 10249, 
         user_id != 10252, user_id != 10271,
         user_id != 10293, user_id != 10592,
         user_id != 10602, user_id != 10603, user_id != 10607) 

# create hour and date variable out of response_time 
mood$hour <- format(as.POSIXct(strptime(mood$response_time,
                             "%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
mood$date <- format(as.POSIXct(strptime(mood$response_time,
                            "%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y-%m-%d")

#select timeframe of phone-dataset 
mood <- mood[mood$date > '2019-02-21' & mood$date <= '2019-03-26',]
mood <- droplevels.data.frame(mood)

#select exact variables which are used
mood <- select(mood, user_id, date, hour, tired)

#create timeframes of 4 hours
attach(mood)
mood$time[hour >= '06:00' & hour <= '9:59'] <- "Morning"
mood$time[hour >= '10:00' & hour <= '13:59'] <- "Noon"
mood$time[hour >= '14:00' & hour <= '17:59'] <- "Afternoon"
mood$time[hour >= '18:00' & hour <= '21:59'] <- "Evening"
mood$time[hour >= '22:00' & hour <= '23:59'] <- "Night"
mood$time[hour  >= '00:00' & hour <= '01:59'] <- "Night"
mood$time[hour  >= '02:00' & hour <= '05:59'] <- "Late-night"

#From 16,016 entries to 8,377 entries 
dim(mood_sampling_data)
dim(mood)

#------------------------------------------------------------------------------#
#create tired variable
# 5 multiclass
mood.2 <- mood %>% group_by(user_id, date, time)
mood_all.2 <- mood.2 %>%
  summarise(
    median_tired = round(median(as.numeric(as.character(tired))),0),
    tired = ifelse(median_tired == 0, "0", "1")
  )

#From 8,377 to 6970 entries 
dim(mood_sampling_data)
dim(mood)
dim(mood_all.2)

#------------------------------------------------------------------------------#
#------------------------------ ClEANED PHONE ---------------------------------#
#------------------------------------------------------------------------------#

#make phone dataset 
phone_use_data <- read.table("phone_use_data.csv", 
                             header = TRUE,
                             sep = ",")
phone <- phone_use_data

# create binary of notification and time_spend variable
phone <- phone %>%
  mutate(notification = ifelse(notification == "True",
                               1, 0)) %>%
  mutate(time_spend = ((endTimeMillis - startTimeMillis) / 1000))

# make character variable of application
phone$application <- as.character(phone$application)

# create time variables out of endTime and startTime
phone$endTime <- gsub("T", "", phone$endTime)

phone$hour <- format(as.POSIXct(strptime(phone$endTime,
                              "%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
phone$date <- format(as.POSIXct(strptime(phone$endTime,
                              "%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y-%m-%d")

#select used variables
phone <- select(phone, user_id, application, notification, time_spend, hour, date, weekend) 


attach(phone)
phone$time[hour >= '06:00' & hour <= '9:59'] <- "Morning"
phone$time[hour >= '10:00' & hour <= '13:59'] <- "Noon"
phone$time[hour >= '14:00' & hour <= '17:59'] <- "Afternoon"
phone$time[hour >= '18:00' & hour <= '21:59'] <- "Evening"
phone$time[hour >= '22:00' & hour <= '23:59'] <- "Night"
phone$time[hour  >= '00:00' & hour <= '01:59'] <- "Night"
phone$time[hour  >= '02:00' & hour <= '05:59'] <- "Late-night"

#------------------------------------------------------------------------------#
# Creating total phone dataset
#Removing Ethica
phone_tot <- filter(phone, application != "com.ethica.logger")

total_group <- phone_tot %>% group_by(user_id, date, time)
phone_total <- total_group %>%
  summarise(
    total_time = round(sum(time_spend)/60,2),
    total_sessions = n(),
    total_notifications = sum(as.numeric(notification))
  )

#From 16,016 entries to 12,668 entries 
dim(phone_use_data)
dim(phone_total)

#------------------------------------------------------------------------------#
# Creating social media dataset
sns_phone <- phone %>%
  filter(application == "com.facebook.katana" 
         | application == "com.facebook.lite" 
         | application == "com.google.android.youtube"
         | application == "com.instagram.android" 
         | application == "com.snapchat.android"
         | application == "com.twitter.android"
         | application == "com.linkedin.android"
         | application == "com.pinterest"
         | application == "com.zhiliaoapp.musically")

# creating new variable names

sns_phone$application[sns_phone$application == "com.facebook.katana"] <-"Facebook"
sns_phone$application[sns_phone$application == "com.facebook.lite"] <-"Facebook"
sns_phone$application[sns_phone$application == "com.google.android.youtube"] <-"YouTube"
sns_phone$application[sns_phone$application == "com.instagram.android"] <-"Instagram"
sns_phone$application[sns_phone$application == "com.snapchat.android"] <-"Snapchat"
sns_phone$application[sns_phone$application == "com.twitter.android"] <-"Twitter"
sns_phone$application[sns_phone$application == "com.linkedin.android"] <-"LinkedIn"
sns_phone$application[sns_phone$application == "com.pinterest"] <-"Pinterest"
sns_phone$application[sns_phone$application == "com.zhiliaoapp.musically"] <-"TikTok"

sns_phone <- mutate(sns_phone, insta_used = ifelse(application == "Instagram", 1,0))
sns_phone <- mutate(sns_phone, fb_used = ifelse(application == "Facebook", 1,0))
sns_phone <- mutate(sns_phone, yt_used = ifelse(application == "YouTube", 1,0))

#grouping social network sites data
sns_group <- sns_phone %>% group_by(user_id, date, time)
phone_sns <- sns_group %>%
  summarise(
    sns_time = round(sum(time_spend)/60,2),
    sns_med_time = round(median(time_spend)/60,2),
    sns_max_time = round(max(time_spend)/60,2),
    sns_min_time = round(min(time_spend)/60,2),
    sns_sessions = n(),
    sns_notification = sum(as.numeric(notification)),
    unique_sns = length(unique(application)),
    insta_sessions = sum(insta_used),
    fb_sessions = sum(fb_used),
    yt_sessions = sum(yt_used))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Create one phone dataset 
phone.2 <- merge(phone_sns, phone_total, by=c("user_id","date", "time"),all.y = TRUE)
phone.2[is.na(phone.2)] <- 0

#create combined dataset
mood_phone <- merge(phone.2, mood_all.2, by=c("user_id","date", "time"))

mood_phone$day <- weekdays(as.Date(mood_phone$date))
mood_phone <- mood_phone %>%
  mutate(weekend = ifelse(mood_phone$day == "vrijdag" | 
                            mood_phone$day == "zaterdag", 1,0))

#selecting features models
head(mood_phone,1)
dim(mood_phone)
mood_phone <- select(mood_phone,c(1:3,20,17,18,4:16))


#selecting users with more than 20 data-entries
mp.grouped <- group_by(mood_phone, user_id) %>%
  summarise(
    entries = n())

#remove entries with less than 10 data-entries 
mp.select <- filter(mp.grouped, entries >= 10)
mood_phone2 <- merge(mood_phone, mp.select, by = "user_id")

#removing entries with less than 1 session and less than 1 minute spend
mood_phone3 <- filter(mood_phone2, total_sessions > 1 & total_time > 1 & sns_sessions > 1)
mood_phone3a <- filter(mood_phone2, total_sessions > 1 & total_time > 1)
mood_phone3$entries <- NULL



#4.182 entries, 19 variables
#------------------------------------------------------------------------------#
#-------------------------Exploratory Data Analysis----------------------------#
#------------------------------------------------------------------------------#
head(mood_phone3,1)
#Correlation Matrix
correlations <- cor(mood_phone3[,7:19])

corrplot(correlations, method="circle", type = c("full"),bg = "white",tl.col = "black")
highlyCorrelated <- findCorrelation(correlations, cutoff=0.8)
print(highlyCorrelated)


model.lvq <- mood_phone3[,6:19]
model.lvq$tired <- as.factor(model.lvq$tired)
control <- trainControl(method="repeatedcv", number=10, repeats=10)
model <- train(tired~., data=model.lvq, method="lvq", trControl=control)

importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance, ylab = "Features", main = "Feature importance with LVQ ")

write.table(correlations_used_features, file = "corrtable.txt", sep = "\t",
            row.names = TRUE, col.names = TRUE)

# removing least important feature: sns_min_time and yt_sessions

eda.mp <- mood_phone3[,-c(10,16)] 
eda.mp$tired <- as.factor(eda.mp$tired)
head(eda.mp,1)
dim(eda.mp)

#------------------------------------------------------------------------------#

# Step aic
mp <- eda.mp[,6:17]
mp$tired <- as.factor(mp$tired)
full <- glm(tired~., family = binomial, data = mp)
summary(full)
#sns_sessions,sns_notification,unique_sns,insta_used,total_sessions 

library(MASS)
step <- stepAIC(full, trace=TRUE, direction = "both")
step$anova

detach("package:MASS", unload = TRUE)
library(dplyr)
#Final Model:
#  tired ~ sns_time + sns_sessions + sns_notification + unique_sns + 
#  insta_sessions + total_time + total_sessions + total_notifications

# removing sns_med_time, fb_sessions, sns_max_time
final.model <- eda.mp[,c(1,5:7,10:13,15:17)]
full.model <-  eda.mp[,c(1,3:7,10:13,15:17)]


#only classes 0 versus 4/5
mp.class <- filter(full.model, median_tired == 0 | median_tired == 4 |  median_tired == 5)

#------------------------------------------------------------------------------#
#PCA
x <- final.model[,-c(1,2,3)]
res.pca <- prcomp(x, scale = TRUE, center = TRUE)
summary(res.pca)
fviz_eig(res.pca, addlabels=TRUE, color=rgb(0,0,0), 
         main = "Principal Component Analysis")+ theme_classic()

fviz_pca_ind(res.pca, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.6)

#------------------------------------------------------------------------------#
#-------------------------Tables and Graphs------------------------------------#
#------------------------------------------------------------------------------#
mood.phone <- full.model

mood.phone$tired <- as.factor(mood.phone$tired)

# multiclass variability total
mood_phone_groups <- group_by(mood.phone, tired)%>%
  summarise(
    count = n())

ggplot(mood_phone_groups, aes(x=tired, y= count)) + 
  ggtitle("Distribution mental tiredness \n in total") +
  geom_bar(stat = "identity",color=rgb(0,0,0), fill = "#3f78b5")+ 
  geom_text(aes(label = count), vjust = -0.5) +
  theme_classic()+
  scale_y_continuous(name = "Number of surveys (n=5338)") +
  scale_x_discrete(name = "Mental tiredness by scale")

ggplot(full.model, aes(x=total_notifications, y=tired, fill = tired)) + 
  geom_boxplot()+
  ggtitle("Notifications and mental tiredness")+
  coord_flip()+
  scale_fill_manual(values=c("#3f78b5","#5eacff"))+
  scale_x_continuous(name = "Number of notifications") +
  scale_y_discrete(name = "Mental tiredness") 

boxplot(total_notifications ~ tired, full.model, main = "Total notifications and mental tiredness", xlab = "Mental Tiredness", ylab= "Amount of notifications")

##-----------------------------------------------------------------------------#
# multiclass variability per person
btired_per_id <- group_by(eda.mp, user_id) %>%
  summarise(
    range_tired = n_distinct(tired),
    count = n())

btired2 <- group_by(btired_per_id, range_tired) %>%
  summarise(
    count2 = n())
btired2$range_tired <- as.factor(btired2$range_tired)

ggplot(btired2, aes(x=range_tired, y =count2)) + 
  ggtitle("Distribution mental tiredness \n by user id")+
  geom_bar(stat = "identity",color=rgb(0,0,0), fill = "#3f78b5")+
  geom_text(aes(label = count2), vjust = -0.5) +
  theme_classic()+
  scale_y_continuous(name = "Number of respondents (n=102)") +
  scale_x_discrete(name = "Variability in mental tiredness") 

##-----------------------------------------------------------------------------#

ggplot(mood.phone, aes(x = weekend, fill = tired)) +
  ggtitle("Distribution mental tiredness \n by weekend")+
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("#3f78b5","#5eacff"))+
  theme_classic()+
  scale_y_continuous( name = "Distribution")+
  scale_x_discrete(name = "Non-weekend versus weekend ")

mood.phone$time <- factor(mood.phone$time ,levels = c("Morning","Noon","Afternoon","Evening", "Night"))

ggplot(mood.phone, aes(x = time, fill = tired)) +
  ggtitle("Distribution mental tiredness \n by timeframe") +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("#5eacff","#3f78b5"))+
  theme_classic()+
  scale_y_continuous( name = "Distribution")+
  scale_x_discrete(name = "Mental tiredness by timeframe")+
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

ggplot(sns_phone, aes(x = fct_infreq(application))) +
  ggtitle("Social media networks \n by sessions") +
  geom_bar(stat = "count",color=rgb(0,0,0), fill = "#3f78b5")+
  scale_y_continuous( name = "Number of sessions")+
  theme_classic()+
  scale_x_discrete(name = "Used applications")+
  theme(axis.text.x = element_text(angle=90, vjust=0.5))



#------------------------------------------------------------------------------#
#-------------------------MODELS-----------------------------------------------#
#------------------------------------------------------------------------------#

#model 1: full model
#model 1a: full model only o versus 4/5
#model 2: top 20 users with most social media usage
#model 3: top 25 users with most entries
#model 3a: top 25 users with most entries only o versus 4/5
#model 4: model based on full days in stead of time windows
#model 4a: model based on full days in stead of time windows only o versus 4/5
#model 5: pca model

model.1 <- select(full.model, c(2,3,5:13))
model.1a <- select(mp.class, c(2,3,5:13))

##-----------------------------------------------------------------------------#
#selecting top 20 users with most social media usage
users <- group_by(full.model, user_id)
most_social <- users %>%
  summarise(
    t0 = sum(sns_sessions),
    t1 = sum(sns_time),
    counts = n(),
  )
most.social <- filter(most_social, counts > 1, t0 > 1000, t1 > 1500)
model2  <- merge(full.model, most.social, by = "user_id")


model.2 <- select(model2, -c(1,4,14,15,16))

##-----------------------------------------------------------------------------#
# selecting top 25 users with most entries 

mp.grouped <- group_by(full.model, user_id) %>%
  summarise(
    entries = n())

mp.select2 <- filter(mp.grouped, entries > 60, user_id != 	10186)
mp.model3 <- merge(full.model, mp.select2, by = "user_id")
mp.model3 <- filter(mp.model3, total_sessions > 1 & total_time > 1 & sns_sessions > 1)
mp.model3$entries <- NULL
head(mp.model3)

model.3 <- select(mp.model3, -c(1,4))

mp.select2
##-----------------------------------------------------------------------------#
#model 3a
mp.model.3a <- filter(mp.model3, median_tired == 0 | median_tired == 4 |  median_tired == 5)
View(model.3a)
model.3a <- select(mp.model.3a, -c(1,4))

##-----------------------------------------------------------------------------#
# Creating model for day prediction 


#per day
mood2 <- mood
mood2 <- mood2[,-c(5)]

#create tired variable
mood.3 <- mood2 %>% group_by(user_id, date)
mood_all.3 <- mood.3 %>%
  summarise(
    median_tired = round(median(as.numeric(as.character(tired))),0),
    tired = ifelse(median_tired == 0, "0", "1")
  )
phone_total2 <- filter(phone, application != "com.ethica.logger")
total_group2 <- phone_total2 %>% group_by(user_id, date)
phone_total2 <- total_group2 %>%
  summarise(
    total_time = round(sum(time_spend)/60,2),
    total_sessions = n(),
    total_notifications = sum(as.numeric(notification))
  )
sns_group2 <- sns_phone %>% group_by(user_id, date)
phone_sns2 <- sns_group2 %>%
  summarise(
    sns_time = round(sum(time_spend)/60,2),
    sns_sessions = n(),
    sns_notification = sum(as.numeric(notification)),
    unique_sns = length(unique(application)),
    insta_sessions = sum(insta_used))

#Create one phone dataset 
phone.3 <- merge(phone_sns2, phone_total2, by=c("user_id","date"), all.y = TRUE)
phone.3$day <- weekdays(as.Date(phone.3$date))

#creating dummy coding weekend + days
phone.3 <- phone.3 %>%
  mutate(weekend = ifelse(phone.3$day == "vrijdag" | 
                            phone.3$day == "zaterdag", 1,0))

phone.3$day <- NULL
View(phone.3)
phone.3[is.na(phone.3)] <- 0

#create combined dataset
mp.model4 <- merge(phone.3, mood_all.3, by=c("user_id","date"))

model.4 <- select(mp.model4, -c(1,2,12))


mp.grouped <- group_by(mp.model4, user_id) %>%
  summarise(
    entries = n())

View(mp.grouped)
#----------------------------------------------------------------------------------------#
mp.model.4a <- filter(mp.model4, median_tired == 0 | median_tired == 4 |  median_tired == 5)
View(mp.model.4a)
model.4a <- select(mp.model.4a, -c(1,2,12))

#-----------------------------------------------------------------------------------------#
#PCA-model
head(pca.model,1)


#creating pca dataframe
x <- final.model[,-c(1,2,3)]
scaled_mp <- apply(x, 2, scale)
head(scaled_mp)

mp.cov <- cov(scaled_mp)
mp.eigen <- eigen(mp.cov)
str(mp.eigen)
phi <- mp.eigen$vectors[,1:5]
phi <- -phi
row.names(phi) <- c("sns_time","sns_session", "sns_notification","unique_sns","insta_sessions",
                    "total_time","total_sessions", "total_notifications")
colnames(phi) <- c("PC1", "PC2","PC3","PC4", "PC5") 

PC1 <- as.matrix(scaled_mp) %*% phi[,1]
PC2 <- as.matrix(scaled_mp) %*% phi[,2]
PC3 <- as.matrix(scaled_mp) %*% phi[,3]
PC4 <- as.matrix(scaled_mp) %*% phi[,4]
PC5 <- as.matrix(scaled_mp) %*% phi[,5]
tired <- as.matrix(final.model[,3])

pca.model <- data.frame(session = row.names(x), PC1, PC2, PC3, PC4, PC5, tired)

model.5 <- select(pca.model, -c(1)) 

#-----------------------------------------------------------------------------------------#
prop.table(table(final.model$tired))
prop.table(table(mp.class$tired))
table(full.model$time)
table(full.model$weekend)


#median features 0 versus 4/5
tired_class <- group_by(mp.class, tired)

testing <- tired_class %>%
  summarise(
    time.sns = median(sns_time),
    noti.sns = median(sns_notification),
    sessions.sns = median(sns_sessions),
    time.total = median(total_time),
    noti.total = median(total_notifications),
    m.noti = mean(total_notifications),
    s.noti = mean(sns_notification),
    sessions.total = median(total_sessions),
    sess = median(unique_sns),
    insta = median(insta_sessions),
    time.dis = (time.sns/time.total),
    sess.dis = (sessions.sns/sessions.total)
  )
View(testing)


tired_class2 <- group_by(full.model, tired)

#median features 0 versus all
testing2 <- tired_class2 %>%
  summarise(
    time.sns = median(sns_time),
    noti.sns = median(sns_notification),
    sessions.sns = median(sns_sessions),
    time.total = median(total_time),
    noti.total = median(total_notifications),
    m.noti = mean(total_notifications),
    s.noti = mean(sns_notification),
    sessions.total = median(total_sessions),
    sess = median(unique_sns),
    insta = median(insta_sessions),
    time.dis = (time.sns/time.total),
    sess.dis = (sessions.sns/sessions.total)
  )
View(testing2)
#-----------------------------------#
citation("tidyr")
citation("dplyr")
citation("ggplot2")
citation("caret")
citation("corrplot")
citation("forcats")
citation("factoextra")
citation("caret")
citation("MASS")
?corrplot
citation()




