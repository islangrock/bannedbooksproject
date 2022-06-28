# Google Trends Interest Analysis 
library(lubridate)
library(tidyverse)

pen_books <- read.csv("~/Desktop/bannedbooksproject/data/PEN_booklist.csv")

books_dates <- pen_books  %>% select(Author, Title, Date.of.Challenge.Removal) %>%
  rename(Date="Date.of.Challenge.Removal")

books_dates$Date 

books_dates$Date <- gsub(" 2021", "", books_dates$Date)
books_dates$Date <- gsub(" 2022", "", books_dates$Date)


quarter <- data.frame(Month= c("Fall", "October", "November", "December",
                               "Winter", "January", "February", "March",
                               "Spring", "April", "May", "June",
                               "Summer", "July", "August", "September"), 
                      Quarter=c("Fall", "Fall", "Fall", "Fall", 
                                "Winter", "Winter", "Winter", "Winter",
                                "Spring", "Spring","Spring","Spring",
                                "Summer", "Summer", "Summer", "Summer"))


books_dates <- books_dates %>% left_join(quarter, by=c("Date"="Month"))

books_dates <- books_dates %>%
  mutate(time_frame = if_else(Quarter== "Fall", "2021-07-01 2022-03-31", 
                              if_else(Quarter=="Winter", "2021-10-01 2022-06-30", 
                                      if_else(Quarter=="Summer", "2021-04-01 2021-12-31", 
                                              "2022-01-01 2022-06-27"))))

books_dates <- books_dates %>%
  mutate(ID=row_number())


write.csv(books_dates, file="~/Desktop/bannedbooksproject/data/booksbandates.csv")

## now we run the python script for the Google API. 

interest1 <- read.csv("~/Desktop/bannedbooksproject/data/interest_data/test.csv")

spring_old <- interval(ymd("2021-04-01"), ymd("2021-06-30"))
summer<- interval(ymd("2021-07-01"), ymd("2021-09-30"))
fall <- interval(ymd("2021-10-01"), ymd("2021-12-31"))
winter <- interval(ymd("2022-01-01"), ymd("2022-03-31"))
spring <- interval(ymd("2022-04-01"), ymd("2022-06-27"))


interest <- interest1 %>%
  mutate(season = if_else(ymd(date) %within% fall =="TRUE", "fall", 
                          if_else(ymd(date) %within% winter =="TRUE", "winter",
                                  if_else(ymd(date) %within% spring == "TRUE", "spring",
                                          if_else(ymd(date) %within% spring_old ==TRUE, "spring_old", "summer"))))) %>%
  group_by(ID, season, book) %>%
  summarize(av_interest=mean(interest))

ban_interest<-left_join(interest, books_dates, by="ID") %>%
  rename(ban_date= "Quarter")


# if ban_date= Fall, ban = Fall, ban-1 = Summer, ban + 1 = Winter 
# if ban_date = Winter, ban = Winter, ban-1= Fall , ban +1 = Spring  

ban_interest <- ban_interest %>%
  mutate(ban_date=tolower(ban_date)) %>%
  mutate(ban_period = if_else(ban_date == season, "ban", "0")) %>%
  mutate(ban_period = if_else(ban_date == "fall" & season == "winter", "ban +1", ban_period)) %>%
  mutate(ban_period = if_else(ban_date == "fall" & season == "summer", "ban -1", ban_period)) %>%
  mutate(ban_period = if_else(ban_date == "winter" & season == "spring", "ban +1", ban_period)) %>%
  mutate(ban_period = if_else(ban_date == "winter" & season == "fall", "ban -1", ban_period)) %>%
  mutate(ban_period = if_else(ban_date == "summer" & season == "spring_old", "ban -1", ban_period)) %>%
  mutate(ban_period = if_else(ban_date == "summer" & season == "fall", "ban +1", ban_period))


ban_interest %>%
  filter(!ban_period=="0") %>%
  ggplot(aes(x=factor(ban_period, level=c("ban -1", "ban", "ban +1")), y=av_interest, group=ID, color=ID))+
  geom_point()+
  geom_line()


aes(x = factor(Species, level = c('virginica', 'versicolor', 'setosa')), y = Petal.Width))

  