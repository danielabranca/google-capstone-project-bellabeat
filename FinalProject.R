#Working on weight. 
##Data cleaning

n_distinct(weight$Id)
### removing duplicates seems difficult in this case.

###This will change the data type from dbl to chr. 
weight$LogId <- as.character(weight$LogId)

###This will change the Date datetime format from "%m/%d/%Y %I:%M:%S %p" to "%m/%d/%Y %H:%M:%S"
> DateTime <- as.POSIXct(weight$Date, format= "%m/%d/%Y %I:%M:%S %p")
> weight$Date <- as.Date(DateTime)
##AND - transform function doesn't save the work in a new column, so I have to attribute it a dataframe so it saves the progress. Also, attributing it only a column name won't work, since it saves all columns present. Has to be done this way
weight <- transform(weight, DateTime = format(as.POSIXct(Date, format = "%m/%d/%Y %I:%M:%S %p"), format = "%m/%d/%Y %H:%M:%S"))

###This splits Date column into two (date and time), having space as a separator (phenomenal!)
weight[c('date', 'time')] <- str_split_fixed(weight$Date, ' ', 2)


#This graph will show us how many (in percentage) weight logs have been introduced manually.
ggplot(weight)+
  geom_bar(mapping = aes(x = IsManualReport, fill = IsManualReport), show.legend = FALSE)+
  scale_x_discrete(labels = c("FALSE" = "Automatic", "TRUE" = "Manually Entered"))+
  scale_y_continuous(name = "Percentage", sec.axis = sec_axis( trans = , name = "Second Axis"))+ #Idk how to do this, I'll move on.
  labs(title = "Weight Logs: Data Entry Method", 
       subtitle = "sampling period: 04-12-2016/05-12-2016", 
       caption ="Data from https://www.kaggle.com/datasets/arashnic/fitbit", 
       x="Entry Method", y="Number of Entries")
##The graph indicates that the majority of weightlogs were manually inserted, meaning the company has to work on a way to better connect bellabeat app and smart scales. 
##41/67*100 = 61% is the percentage of manual entries

#Working on Steps, Kcal and Distance 

##First, lets prepare and process the data

###This is how you change column names
colnames(merged_dataset)[1] <- "date"

###Now lets merge data
merging_stuff <- merge(merged_dataset, weight, by=c ("Id", "date"))

##The analysis on the merged dataset will allow to identify trends that can better help to direct marketing strategies. > n_distinct(merged_dataset$Id)
> n_distinct(merged_dataset$Id)
##We have 33 distinct users.

##Plotting daily steps vs calorie visualization - Beautiful! courtesy of Roc Data Analyst through Github

ggplot(data=merged_dataset, aes(x=TotalSteps, y=Calories))+
  geom_point(aes(colour = TotalDistance))+
  geom_smooth()+
  xlab("Total Steps") + ylab("Calories Burned")+
  scale_color_gradientn(colours = rainbow(3))+
  theme(plot.title = element_text(face = "bold", vjust = 0.5, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.caption = element_text(face = "italic"))+
  labs(x = "Daily Steps", y = "Calories Burned", 
       title = "Daily Steps vs Calories Burned",
       subtitle = "sampling period: 04-12-2016/05-12-2016", 
       caption ="Data from https://www.kaggle.com/datasets/arashnic/fitbit")
##Here we see a positive correlation between steps given and calories burned! 
##How to remove the three outliers, though? 
##The more exercise/steps made/given, the more kcal burned, which is fundamental for weight loss. The company create ways for costumers to costumize its apps to go in accordance to their weight objectives, so the app can notify them if they have succeeded in their weight loss/kcal burned goals daily.


#Plotting time asleep throughout the days of the week
sleep_v2 <- sleep %>% 
mutate(WeekDay = case_when(
        SleepDay == "2016-04-17" | SleepDay == "2016-04-24" | SleepDay == "2016-05-01" | SleepDay == "2016-05-08" | SleepDay == "2016-05-10" ~ "Sunday", 
        SleepDay == "2016-04-18" | SleepDay == "2016-04-25" | SleepDay == "2016-05-02" | SleepDay == "2016-05-09" | SleepDay == "2016-05-11" ~ "Monday", 
        SleepDay == "2016-04-12" | SleepDay == "2016-04-19" | SleepDay == "2016-04-26" | SleepDay == "2016-05-03" | SleepDay == "2016-05-12" ~ "Tuesday", 
        SleepDay == "2016-04-13" | SleepDay == "2016-04-20" | SleepDay == "2016-04-27" | SleepDay == "2016-05-04" | SleepDay == "2016-05-13" ~ "Wednesday",
        SleepDay == "2016-04-14" | SleepDay == "2016-04-21" | SleepDay == "2016-04-28" | SleepDay == "2016-05-05" | SleepDay == "2016-05-14" ~ "Thursday",
        SleepDay == "2016-04-15" | SleepDay == "2016-04-22" | SleepDay == "2016-04-29" | SleepDay == "2016-05-06" | SleepDay == "2016-05-15" ~ "Friday",
        SleepDay == "2016-04-16" | SleepDay == "2016-04-23" | SleepDay == "2016-04-30" | SleepDay == "2016-05-07" | SleepDay == "2016-05-16" ~ "Saturday"))
View(sleep_v2)
save(sleep_v2, file = "sleep_v2.csv")

##Here, the plot:
ggplot(data = sleep_v2) + 
  geom_col(mapping = aes(x = WeekDay, y = TotalMinutesAsleep, fill = "TotalMinutesAsleep"), show.legend = FALSE) + 
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold", angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.caption = element_text(face = "italic", vjust = 0.5, hjust = 0.5)) +
  labs(title = "Average Minutes Slept over the Week", 
       subtitle = "sampling period: 04-12-2016/05-12-2016", 
       caption ="Data from https://www.kaggle.com/datasets/arashnic/fitbit",
       x = "Days of the Week", y = "Minutes Asleep, Average")

###Questions: how can I connect minutes to fall asleep and minutes slept in the same plot, with minutes asleep as geom_col and minutes to fall asleep as a geom_smooth? How can I transform these minutes into duration (hours and minutes display)?

##Data tells us that the day people sleep more Sunday and less on Thursday. 
##
##PROBLEM: Instead of the average, it made the sum of values! How to correct it? Can't make further analysis until I correct problem. 
###Solution:

###creating a data frame with the averages
sleep_v3 <- sleep_v2 %>% 
  group_by(WeekDay) %>% 
  summarize(mean_TotalMinutesAsleep = mean(TotalMinutesAsleep), mean_TotalTimeInBed = mean(TotalTimeInBed))

###now plotting it:

ggplot(data = sleep_v3) + 
  geom_col(mapping = aes(x = WeekDay, y = mean_TotalMinutesAsleep), show.legend = FALSE) +
  geom_line(mapping = aes(x = WeekDay, y = mean_TotalTimeInBed), show.legend = FALSE) +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold", angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.caption = element_text(face = "italic", vjust = 0.5, hjust = 0.5)) +
  labs(title = "Average Minutes Slept over the Week", 
       subtitle = "sampling period: 04-12-2016/05-12-2016", 
       caption ="Data from https://www.kaggle.com/datasets/arashnic/fitbit",
       x = "Days of the Week", y = "Minutes Asleep, Average")

###Question: now that I solved the problem of sum/avg, how can I change the y axis so it shows as h:m instead of only minutes?
###Question: why doesn't the geom_line show up? I was trying to relate time asleep with time in bed. 

##On hourly steps : activity throughout the day 
##courtesy of Roc Analyst - github 
###

hourly_steps <- read_csv("Data Analysis by Google/Capstone/fitness_data/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")


###This is a(nother) way of converting date time from a cylce of 12hours AM/PM to 24h. And it is simpler! 
> hourly_steps$Time <- format(strptime(hourly_steps$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p"))

###removing a column I did by mistake
hourly_steps = subset(hourly_steps, select = -c(activity_hour))

###converting the data column from chr to datetime
hourly_steps$Hour <- as.POSIXct(strptime(hourly_steps$Time, format = "%Y-%m-%d %H:%M:%S"))
###separating datetime column into date and time
hourly_steps$Date <- as.Date(hourly_steps$Hour) #add date column
hourly_steps$Hour <- format(as.POSIXct(hourly_steps$Hour), format = "%H:%M:%S") #add time column

##Cleaning data so I can plot it
hourly_steps_plot <- hourly_steps %>% 
  group_by(Hour) %>% 
  drop_na() %>% 
  summarize(mean_steps_total = mean(StepTotal))

#plotting this *$%#!
hourly_steps_plot  %>% 
  mutate(Hour = as.POSIXct(hms::parse_hm(Hour))) %>% 
  ggplot(aes(Hour, mean_StepTotal))+
  geom_col()+
  geom_smooth()+
  scale_x_datetime(breaks = scales::date_breaks("120 mins"), date_labels = "%H:%M")+
  theme(plot.title = element_text(face = "bold", vjust = 0.5, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.caption = element_text(face = "italic"))+
  labs(x = "Time of Day", y = "Steps", title = "Average Steps Per Hour",
       caption = "Data Source: Crowd-sourced Fitbit datasets")
print(hourly_steps_plot)

##From 6 to 12 there's an increase in activity. from 13 to 14 is +/- constant. 15 has a decrease, from 16 to 18 it goes up again and from 19 to 00 it decreases. It makes sense, since lunch times are usually around 13 to 16. 
##Nevertheless, specially for costumers who are aiming to loose weight/higher their daily activity, the company could personalize notifications from 19 onwards to motivate more exercise making.


#Finding relationship between Average Steps Given with Average Time to Fall Asleep per Id

###This will transform Id variables from num to chr; important for plotting
sleep_steps$Id <- as.character(sleep_steps$Id)  

##Plotting
ggplot(data= sleep_steps) +
  geom_col(mapping = aes(x=Id, y=AVGSteps)) +
  geom_smooth(mapping = aes(x=Id,y=AVGMinToFallAsleep)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Relationship between Steps and Time to Fall Asleep",
       subtitle = "Participants tracked between 4/12/16 and 5/12/16",
       caption = "data source: crowd-sourced fitbit datasets") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 0.003, name = "Minutes to Fall Asleep"))

###Problems: how to make the secondary axis work properly? 
###I was expecting to find higher times to fall asleep with less steps given. if true, suggest notifications for mindfulness meditations and for more exercise when user has long periods of being in bed but not asleep.
###I need help to do this right! I'll put it on github and kaggle and hope and wait until someone helps.

#Relationship between heart rate and intensity - finding outliers 

##Preparing the data
###This is a confusing way of splitting date and time but I didn't know better before and it worked.
heartrate_v2 <- heartrate %>% 
  mutate(Date = substr(Time,1,9)) %>% 
  mutate(time = substr(Time,11,21)) %>% 
  rename(OldTime = Time)

###Intensity is coded, scale is as follows: 0=Sedentary, 1 Light, 2=Moderate, 3=VeryActive
heartrate_v2 <- heartrate_v2 %>% mutate(DateTime = mdy_hms(OldTime)) %>% rename(Time = time) 

Intensities <- MinuteIntensities %>% 
  mutate(Date = substr(ActivityMinute,1,9)) %>% 
  mutate(time = substr(ActivityMinute,10,21)) %>% 
  mutate(DateTime = mdy_hms(ActivityMinute)) %>% 
  mutate(IntensityLevel = case_when(Intensity == "0" ~ "Sedentary", 
                                    Intensity == "1" ~ "Light", 
                                    Intensity == "2" ~ "Moderate", 
                                    Intensity == "3" ~ "Very Active"))

#merging datasets
HeartrateVSIntensities <- merge(Intensities, heartrate_v2, by = c("Id", "DateTime"))

##Plotting the data
ggplot(data = HeartrateVSIntensities)+
  geom_point(mapping = aes(x = DateTime, y = Value, color = Intensity))+
  labs(title="Activity Intensity and Heart Rate",
       subtitle="Participants tracked 4/12/16-5/12/16",
       caption = "Data from https://www.kaggle.com/datasets/arashnic/fitbit",
       x="Date", y="Heart Rate, bpm")

### It's clearly seenable that there are some intensity zero moments where the heart rate is high, which might indicate high stress. It would be interesting to send notifications to users when moments like these are happening that invite to a mindfulness medidation or breathwork practice. 


#Diving deeper into stress : high heart rate and very low activity level
###Now I wanted to dive deeper into the HRxActivity data to see if I can identify the percentage of high HR in sedentary activity. 
##According to Mayo Clinic, normal resting HR range for adults is between 60-100 bpm.(NOTE:I am making assumption survey participants were adults). 

##creating a subset of data, for users who have a high heartrate and very low activity. 
High_HR_while_Sedentary <- subset(HeartrateVSIntensities, Value > 100 & Intensity < 1)
##counting stuff
> nrow(High_HR_while_Sedentary)
[1] 935
> nrow(subset(HeartrateVSIntensities, Value > 100))
[1] 14573
##Changing Id data type from dbl to character
High_HR_while_Sedentary$Id <- as.character(High_HR_while_Sedentary$Id)

##plotting stuff
ggplot(data = High_HR_while_Sedentary) + 
  geom_point(mapping = aes(x = DateTime, y = Value, color = Id)) + 
  labs(title="High HR during Sedentary periods", 
       subtitle="Participants tracked 4/12/16-5/12/16", 
       caption = "Data from https://www.kaggle.com/datasets/arashnic/fitbit", 
       x="Date", y="Heart Rate, bpm")

#Counting stuff
unique(High_HR_while_Sedentary$Id)
unique(HeartrateVSIntensities$Id)

> 935 / 14573
[1] 0.06415975
> 0.06415975 * 100
[1] 6.415975

#So, about 6.42% of the time an individual's HR was >100, they were sedentary - this isn't a huge occurrence, but may be worth looking at this population for marketing opportunities.

#Frequency of logs throughout the month

##Let's see how the frequency of logs is throughout the sampling period
###Firstly lets count the #logs per date
daily_activity_v2 <- daily_activity %>%
  group_by(ActivityDate) %>%  
  count(ActivityDate)

###Now lets plot this:
ggplot(data = daily_activity_v2) + 
  geom_col(mapping = aes(x = ActivityDate, y = n, show.legend = FALSE)) + 
  labs(title = "Frequency of logs throughout the month", 
       subtitle = "sampling period between 12thApril and  12thMay 2016", 
       caption = "Data from https://www.kaggle.com/datasets/arashnic/fitbit", 
       x = "Dates", y = "#logs per day in a max of 33 users")

##There was a decrease in log activity as the sample period proceeded. Despite 30 days is not a so much significant period, it is advisable that the company's app notifies users as a reminder to log their activity/use the watch, so their daily track doesn't have gaps.


#Still unanswered questions: relation between resting periods and heartrate? activity periods and heartrate? kcal spent and time to fall asleep? sleep and metabolic rate? sleep and minutes of activity?
#kcal spent and intensity of activity? kcal and metabolic rate? 
