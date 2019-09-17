library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways


####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('data',full.names=T)


#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')


ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')

# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))

##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

?spread
full_wide<- spread(full_long, key = "data", value = "value")%>%
  filter_if(is.numeric,all_vars(!is.na(.))) %>%
  mutate(month = month(DateTime),
         year = year(DateTime))
  
head(full_wide) 
summer_only<- filter(full_wide,month %in% c(6,7,8,9)) #filter out months w snow


summary(full_wide)



ggplot(summer_only,aes(x=ndmi, y=ndvi, color=site))+
  geom_point()+
  theme_few()+
  scale_color_few()+
  xlab('NDMI')+
  ylab('NDVI')

  

## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 


## Your code here

#Filter winter months
winter_months<- filter(full_wide,month %in% c(1,2,3,4))%>%
  group_by(year, site)%>%
  summarize(ndsi_mean= mean(ndsi))

#Filter summer months
summer_months<- filter(full_wide,month %in% c(6,7,8))%>%
  group_by(year, site)%>%
  summarize(ndvi_mean = mean(ndvi))
?merge
summer_winter<- inner_join(winter_months,summer_months, by=c("year", "site"))



ggplot(summer_winter, aes(x=ndsi_mean, y=ndvi_mean, color=site))+
  geom_point()+
  theme_few()+
  theme(legend.position = c(0.85,0.3))+
  xlab('Mean NDSI')+
  ylab('Mean NDVI')
  
  
  ## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 



## Your code here
pre_post<- mutate(summer_winter, condition = ifelse(year<2002,"pre","post"))

ggplot(pre_post, aes(x=ndsi_mean, y=ndvi_mean, color=site))+
  geom_point()+
  theme_few()+
  facet_wrap(~condition)+
  xlab('Mean NDSI')+
  ylab('Mean NDVI')

## End code for question 3


###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 


summer2<- ndvi %>%
  filter(!is.na(burned),!is.na(unburned))%>%
  mutate(year= year(DateTime))%>%
  mutate(month= month(DateTime))%>%
  gather(key='site',
         value='value',
         -DateTime,-month,-year,-data)%>%
  group_by(month)%>%
  summarize("monthly_ndvi" = mean(value))

ggplot(summer2, aes(x=month, y=monthly_ndvi))+
         geom_point()+
         theme_few()+
         ylab('Monthly NDVI')
        

# Personal Note - "Gather" combined burned and unburned into one column


                                
#Question 4 Answer: August was the greenest month

ndvi_pre_fire_means <- ndvi%>%
  filter(!is.na(burned), !is.na(unburned))%>%
  mutate(month=month(DateTime))%>%
  mutate(year=year(DateTime))%>%
  filter(year <= 2002)%>%
  group_by(month)%>%
  summarise('mean_ndvi_burned'=mean(burned),
            unburned = mean(unburned))%>%
  mutate(treatment="preburned")

ndvi_post_fire_means <- ndvi%>%
  filter(!is.na(burned), !is.na(unburned))%>%
  mutate(month=month(DateTime))%>%
  mutate(year=year(DateTime))%>%
  filter(year > 2002)%>%
  group_by(month)%>%
  summarise('mean_ndvi_burned'=mean(burned),
            unburned= mean(unburned))%>%
  mutate(treatment="postburned")

ndvi_both<- bind_rows(ndvi_pre_fire_means, ndvi_post_fire_means)

ggplot(ndvi_both, aes(x=month, y=mean_ndvi_burned, color= treatment))+
  geom_point()+
  theme_few()+
  xlab('Month')+
  ylab('Mean NDVI in Burned Plots')

  

  
         

##### Question 5 ####
#What month is the snowiest on average?

winter2<- ndsi %>%
  filter(!is.na(burned),!is.na(unburned))%>%
  mutate(year= year(DateTime))%>%
  mutate(month= month(DateTime))%>%
  gather(key='site',
         value='value',
         -DateTime,-month,-year,-data)%>%
  group_by(month)%>%
  summarize("monthly_ndsi" = mean(value))

ggplot(winter2, aes(x=month, y=monthly_ndsi))+
  geom_point()+
  theme_few()+
  xlab('Month')+
  ylab('Monthly NDSI')
##January is snowiest month on average