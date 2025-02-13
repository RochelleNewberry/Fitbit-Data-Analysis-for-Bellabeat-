# Fitbit-Data-Analysis-for-Bellabeat-
Capstone project, Bellabeat product for advertisement
---
title: "Bellabeat Product Analysis Project"
author: "Rochelle Newberry"
date: "February 9th, 2025"
output:
  pdf_document: default
  html_document:
    df_print: paged
editor_options:
  markdown:
    wrap: sentence
---


#### **Bellabeat is a company ahead of its time. We make and sell health monitoring devices with smart technology tailored to the needs of women. We are a successful company that is looking to grow its market share. The goal of this report is to provide a marketing strategy to grow our customer base and our opportunities for growth.**

# **The Process**

### **Ask** 
**Analysis Task: identify which attributes of the competitor's products are used most and use that information to choose which Bellabeat product to advertise.**\

**Answer the 3 main questions:**\

**1. What are some trends in smart device usage?**\
**2. How could these trends apply to Bellabeat customers?**\
**3. How could these trends help influence Bellabeat marketing strategy. **\

**Stakeholders: Urška Sršen: Bellabeat’s cofounder and Chief Creative Officer, Sando Mur: Mathematician and Bellabeat’s cofounder; key member of the Bellabeat executive team**\

**People who can help answer questions: Bellabeat marketing analytics team: A team of data analysts responsible for collecting, analyzing, and repoing data that helps guide Bellabeat’s marketing strategy**\

**Business Task: Use data to choose which Bellabeat product would be the most appealing to potential customers for advertisement.**\

### **2. Prepare**

#### **Data sources:**

        
**1. Fitbit Fitness Tracker Data made available on kaggle.com via Mobius at https://www.kaggle.com/datasets/arashnic/fitbit**\


**Data integrity using the ROCCC method: Reliability: This is raw user data but it is a very small sample size making it relatively unreliable. Original: This is a third party data set and therefore completely unoriginal. Comprehensive: The sample size is small and demographics or method of choosing participants is not explained. Participants could all have the same health problem or all be the same age for example. Current: It is almost 9 years old and newer data sets are available, This data set is not current. Cited: This data set explains where it came from and who collected it. It's not cited like a peer reviewed journal but the method of citing used is acceptable.**\

**This data set overall is not very reliable but it has relevant and necessary data for the purpose of this project and is the set that was suggested by the stakeholders.**\

**“Consumer Perceptions of Wearable Technology Devices: Retrospective Review and Analysis” at https://pmc.ncbi.nlm.nih.gov/articles/PMC7199133/ by the National Library of Medicine**\

**Data integrity according to ROCCC method: Reliable: This data set is from a very reputable source (The National Library of Medicine) and is peer reviewed. Original: This data was not directly collected by us but is a second party data set from a trustworthy source. Comprehensive: It has a very large sample size. The collection method was safe and unbiased. Many different aspects of the products were taken into account. It is satisfactorily comprehensive. Current: This set is now almost 5 years old but that is the most recent data set available and I am not able to collect a new data set at this time. Cited: The sources have been professionally cited at the bottom of the report.**\

**This is a very reliable data set. It is from a reputable and peer reviewed source that explains its collection method and is unbiased. It has a very large sample size and is the most recent data set available on this topic. The source also cites all references at the end of the analysis explanation.**\

### **Process and Prepare**

#### **Documentation of Cleaning and Manipulation of Data **

**It is necessary before beginning to process any data in RStudio to update and call up all libraries I will be using for the project.**
```
install.packages("tidyverse")
install.packages("here")
install.packages("dplyr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("RColorBrewer")
```

```{r warnings = FALSE}
library(tidyverse)
library(here)
library(dplyr)
library(janitor)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
```

**Now that the libraries are ready, it is time to upload the data frames. These data frames were  downloaded to my internal computer memory and uploaded from a set working directory. They are checked to make sure they uploaded properly.**

```{r}

daily_activity_1 <- read.csv("dailyActivity_merged.csv")
glimpse(daily_activity_1)

hourly_calories_1 <- read.csv("hourlyCalories_merged.csv")
glimpse(hourly_calories_1)

hourly_intensities_1 <- read.csv("hourlyIntensities_merged.csv")
glimpse(hourly_intensities_1)

sleep_day_2 <- read.csv("2sleepDay_merged.csv")
glimpse(sleep_day_2)

```

**Now with all the data frames that I will be analyzing uploaded, it is time to start studying and inspecting the data.**

```{r}
n_distinct(daily_activity_1$Id) #35
n_distinct(hourly_calories_1$Id) #34
n_distinct(hourly_intensities_1$Id) #34
n_distinct(sleep_day_2$Id) #24

sum(duplicated(daily_activity_1)) #checking for duplicates
sum(duplicated(hourly_calories_1))
sum(duplicated(hourly_intensities_1))
sum(duplicated(sleep_day_2)) #shows 3 duplicates

sapply(daily_activity_1, class) # date in character format
sapply(hourly_calories_1, class)
sapply(hourly_intensities_1, class)
sapply(sleep_day_2, class) #date in character format 
```

**With that code, I discovered that the Daily Activity data frame has 35 distinct Ids listed. Hourly calories and hourly intensities have 34, and the sleep (measured by day) data frame has 24.**\

**Then each data frame is checked for duplicates. The sleep data frame shows 3 duplicates and the others have none.**\

**Then the form (or class) of data in each column in each data frame is checked. The daily activity and the Sleep data frames have *date* columns in *character* format.**\

**Now it's time to clean up the errors.**\

```{r}
sleep_day_2 <- sleep_day_2 %>%  #duplicate correction
  distinct() %>% 
  drop_na()

sum(duplicated(sleep_day_2)) #duplicate recheck

sleep_day_2 <- sleep_day_2 %>% #converting the dates from character format to date format
  mutate(SleepDay = mdy_hms(SleepDay, tz="EST"))

daily_activity <- daily_activity_1 %>% 
  mutate(ActivityDate = mdy(ActivityDate))

sapply(sleep_day_2, class) # Date rechecks
sapply(daily_activity, class)
```


**This cleaning was successful. Now to start processing the data into something we want to use to analyze and visualize.**\

**Not all customers wore the watch all day when they wore it. To filter out incomplete information, let's filter out the partial days by leaving out the days with less than 2000 steps.**

```{r}
applied_daily_activity <- daily_activity %>% 
  filter(TotalSteps > 2000) # filter out the days where the watch wasn't worn all day

n_distinct(applied_daily_activity$Id) #still 34 unique Ids

```

**There are still 34 unique Ids (one Id was dropped but 34 is still plenty to get accurate results).**\

### **Process **

**This is where I began processing the data to get it ready for analysis.**
**I grouped the data by Ids and grouped the days together by so we can see how many days total each person wore the watch. Then I plotted it so we can visualize how popular this feature is.**

```{r}
distinct_daily_activity <- applied_daily_activity %>% #this will show how many days each user wore the watch for
  group_by(Id)%>%
  summarize(Days = n_distinct(ActivityDate))

distinct_daily_activity %>%  
  ggplot(aes(x=Id, y= Days))+
  geom_point()+
  labs( title = "How Many Days Each Person Wore It (to check work)", x= "Id", y = "Number of Days")
```

**Then I did the same for the data collected for sleep. One individual wore it every singe night that data was collected!**

```{r}
distinct_sleep_day_2 <- sleep_day_2 %>% #this will show how many days each user wore the watch for
  group_by(Id) %>% 
  summarize(Days = n_distinct(SleepDay))

distinct_sleep_day_2 %>% 
  ggplot(aes(x=Id, y= Days))+
  geom_point()+
  labs( title = "How Many Days Each Person Wore It (to check work)", x= "Id", y = "Number of Days")
```

**It sounds interesting to see if activity or other variables affect how long it takes a person to fall asleep so I made a new column in the sleep table called "TimeAwakeInBed" by subtracting the time asleep from the total time in bed.**

```{r}
sleep_day_2 <- transform(sleep_day_2, TimeAwakeInBed = TotalTimeInBed-TotalMinutesAsleep) 
#that made a new column to look at how long each person was laying awake in bed
```

**To continue to gather the data into a format that is easy to plot and study, I made a table out of the averages of each attribute in the daily activity and sleep data frames. I grouped the averages by Id so I didn't just get an average of each column at the top but instead got an average for each individual using the product. These specific columns were chosen after an analysis of the data points available.**

```{r}
average_activity_columns <-applied_daily_activity %>%
  group_by(Id) %>%
  summarize(AvgSteps = mean(TotalSteps), AvgTotalDistance =mean(TotalDistance),
            AvgVeryActiveDistance = mean(VeryActiveDistance),
            AvgModeratelyActiveDistance = mean(ModeratelyActiveDistance),
            AvgLightActiveDistance = mean(LightActiveDistance),
            AvgVeryActiveMinutes = mean(VeryActiveMinutes),
            AvgFairlyActiveMinutes = mean(FairlyActiveMinutes),
            AvgLightlyActiveMinutes = mean(LightlyActiveMinutes), 
            AvgCalories =mean(Calories)
            )

average_sleep_columns <-sleep_day_2 %>%
  group_by(Id) %>%
  summarize(AvgTotalSleepRecords = mean(TotalSleepRecords), 
            AvgTotalMinutesAsleep =mean(TotalMinutesAsleep),
            AvgTotalTimeInBed = mean(TotalTimeInBed),
            AvgTimeAwakeInBed = mean(TimeAwakeInBed)
            )
```

**Next I'm going to combine the averages tables with the tables that contain the total days each person wore the product and check the integrity of the data so far.**

```{r}
averages_daily_activity_final <- merge(distinct_daily_activity,average_activity_columns, by = "Id")

averages_sleep_activity_final <- merge(distinct_sleep_day_2,average_sleep_columns, by = "Id")

glimpse(averages_daily_activity_final)
glimpse(averages_sleep_activity_final)
```

**Then I combined the two tables containing all of the average daily activity with all of the average sleep data to have a complete health information table based on Id.Then another data integrity check.**

```{r}
complete_health_info <- merge(averages_daily_activity_final, averages_sleep_activity_final, by = "Id")
glimpse(complete_health_info)
```

**The other two data frames that were loaded, hourly calories and hourly intensities, were already clean so I combined them. Then I checked the data integrity again.**

```{r}
calories_vs_intensities <- merge(hourly_calories_1, hourly_intensities_1)
glimpse(calories_vs_intensities) #it was done right
n_distinct(calories_vs_intensities$Id) #there are still 34 distinct Ids
```

### **Analyze**

#### **Visualizations and Correlations**

**Now that the data has been cleaned and put into an easy to use form, it's time to look at some visuals and search for correlations between data points.**\

**The first visualization is to see if there is a link between how many calories a person burns in a day and how long they lay awake in bed at night. The graph suggests that while burning more calories does not guarantee falling asleep faster, the individuals that laid awake in bed the longest burned fewer than fewer than average calories.**\

### **Time Awake in Bed vs Calories Burned**
```{r}
complete_health_info %>% 
  ggplot(aes(x=AvgTimeAwakeInBed, y=AvgCalories))+
  geom_point(aes(color = AvgCalories))+
  labs(title = "Does Exercise Help You Rest", x= "Time Awake In Bed", y = "Calories burned")+
  scale_colour_distiller(palette = "Paired")
```

**The next graph shows the link between how intense the physical activity was in each hour and how many calories were burned that hour by each person who was logging their activity.**

### **Average Hourly Intensity vs Calories Burned**
```{r}
calories_vs_intensities %>% 
  ggplot(aes(x=AverageIntensity, y=Calories, color = Calories))+
  geom_point()+
  facet_wrap(~Id)+
  labs( title = "Calories versus Intenseness for Each Id", x= "Average Intensity", y="Calories")+
  scale_color_gradientn(colours = terrain.colors(10))
```

**The last graph shows the link for each person between how many steps they took and how many calories were burned. This seems like an easy association to make. The point of graphing this is to show that people who bought this product took advantage of this feature and used it to motivate themselves to live a healthier lifestyle**.

### **Total Steps vs Calories Burned**
```{r}
applied_daily_activity %>%
  ggplot(aes(x = TotalSteps, y = Calories)) +
  geom_point(aes(color = Calories)) +
  facet_wrap(~ Id) +
  labs(title = "Relationship between Total Steps and Calories for each Id",
       x = "Total Steps",
       y = "Calories")+
  scale_colour_distiller(palette = "Set1")
```

**The last step is to look for number correlations between some of the gathered health information.**\

**This number list shows how strong of a correlation there is between the number of calories burned and; total steps, total distance, how long they were very active for, and how long they were lightly active for. It shows that total steps, distance, and very active minutes were moderately correlated with calories burned and lightly active minutes was weakly correlated.**\

### **Daily Activity Correlations**
```{r}
correlation_daily_activity <- applied_daily_activity %>% 
  summarize(cor(TotalSteps, Calories),
            cor(TotalDistance, Calories),
            cor(VeryActiveMinutes, Calories),
            cor(LightlyActiveMinutes, Calories)
            )
 head(correlation_daily_activity)
```

**Then just for fun let's look at some sleep correlation numbers. This shows a negative correlation between How long people laid awake in bed and how many calories were burned (we saw this on a graph earlier too). It also shows a negative correlation between the total amount of time people spent in bed and how many calories were burned and a very strong correlation between how long people were in bed and how long they were asleep. Those numbers really just show that people are using the product to track their sleeping hours accurately.**

### **Sleep Correlations**
```{r}
correlation_sleep_exercise <- complete_health_info %>% 
   summarize(cor(AvgTimeAwakeInBed, AvgCalories),
             cor(AvgTotalTimeInBed, AvgCalories),
             cor(AvgTotalMinutesAsleep, AvgTotalTimeInBed)
             )

 head(correlation_sleep_exercise) 
```

### **Top High Level Content Recommendation**

**According to the data analysis by the National Library of Medicine, people who wear fitness tracking smart devices purchase based on how it can be used to motivate them, it's ability to track sleep, its ability to hold them accountable, and its discretion in wearing it. The report quoted certain customers as having said**\

**"I wondered why I was so tired when I got up in the morning. The Fitbit really does track my sleep patterns. I found that I was awake numerous times (it even tell you exactly what times you are awake) and it shows when you are restless..." (Sleep tracking)**\

**and "…this works! It's easy to check during the day to keep you on target. In fact I actually WANT to check it, to see my progress. My goal, of course is 10,000 steps a day. Thanks to my Fitbit One, I know I'm going to get there on a regular basis. It gets me out and walking and keeps me moving. I am constantly challenging myself. I finally found something that motivates me to exercise." (Motivation)**\

**and "…because I didn't want to wear a wristband all day since I am constantly typing I knew it would drive me nuts… I really like this little guy I clip it to my bra… don't even notice it is there throughout the day.” (Discretion)**\

**We can see from the Analysis of the Fitbit data available on Kaggle that their customers really did take advantage of the features that track movement and intensity of activity and the sleep tracking feature.**\

**The Bellabeat product that has all of those features available is the Leaf. It can be worn in different ways to be kept discrete and tracks activity information and sleep patterns.That information holds people accountable and motivates them to exercise. I recommend Advertising the Leaf as the beginner product for people coming to Bellabeat then suggesting the app and the membership as the next step.**\

**Advertising The Leaf and the Bellabeat app together will almost guarantee an increase in new customers.**
