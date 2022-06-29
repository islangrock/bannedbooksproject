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

interest1 <- read.csv("~/Desktop/bannedbooksproject/data/interest_data/ids_done.csv")

spring_old <- interval(ymd("2021-04-01"), ymd("2021-06-30"))
summer<- interval(ymd("2021-07-01"), ymd("2021-09-30"))
fall <- interval(ymd("2021-10-01"), ymd("2021-12-31"))
winter <- interval(ymd("2022-01-01"), ymd("2022-03-31"))
spring <- interval(ymd("2022-04-01"), ymd("2022-06-27"))


interest1 <- interest1 %>%
  mutate(season = if_else(ymd(date) %within% fall =="TRUE", "fall", 
                          if_else(ymd(date) %within% winter =="TRUE", "winter",
                                  if_else(ymd(date) %within% spring == "TRUE", "spring",
                                          if_else(ymd(date) %within% spring_old ==TRUE, "spring_old", "summer"))))) 


ban_interest<-left_join(interest1, books_dates, by="ID") %>%
  dplyr::rename(ban_date= "Quarter")


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


# Produce first Plot 

ban_interest %>%
  group_by(ID, ban_period, book) %>%
  summarize(av_interest=mean(interest))%>%
  ggplot(aes(x=factor(ban_period, level=c("ban -1", "ban", "ban +1")), y=av_interest, group=ID, color=ID))+
  geom_boxplot()+
  geom_line()


df<-ban_interest %>%
  group_by(ID, ban_period, book) %>%
  summarize(av_interest=mean(interest)) %>%
  pivot_wider(names_from=ban_period, values_from=av_interest)%>%
  dplyr::rename(banpre="ban -1", banpost="ban +1")



  mutate(delta1= (ban - banpre), delta2 = banpost-banpre, delta_combo=(ban+banpost-banpre))



# Produce significance table 

sig_test<- ban_interest %>%
  select(ID, ban_period, interest, date, book)

sig_test1<- sig_test %>%
  filter(ban_period != "ban +1")

test1<-lapply(split(sig_test1, factor(sig_test1$ID)), function(x)t.test(data=x, interest~ban_period, paired=FALSE))

sig_test2<- sig_test %>%
  filter(ban_period != "ban -1")

test2<-lapply(split(sig_test2, factor(sig_test2$ID)), function(x)t.test(data=x, interest~ban_period, paired=FALSE))


library(plyr)
detach(package:plyr)

sig_df <- data.frame(matrix(unlist(test1), nrow=length(test1), byrow=TRUE)) %>%
  rowid_to_column() %>%
  select(rowid, X1, X3)


sig_df<- sig_df %>%
  mutate(p_value = if_else(X3<.05, "sig", "0"), 
         coef_sign = if_else(X1<0, "neg", "pos"), 
         sig_sign = paste(p_value, coef_sign, sep=" "))

table(sig_df$sig_sign)



# ID, test 1 coef, test 1 p value, test 2 coef, test 2 p-value, interest at each ban_period 


sig_vis <- left_join(df, sig_df, by=c("ID" = "rowid")) %>%
  pivot_longer(3:4, names_to= "ban_period", values_to = "av_interest") %>%
  na.omit()

ggplot(sig_vis, aes(x=factor(ban_period, level=c("banpre", "ban")), y=av_interest, 
                             color= sig_sign, alpha=sig_sign))+
  geom_point()+
  geom_jitter()+
  scale_color_manual(values=c("grey", "grey", "green", "red", "grey"))+
  scale_alpha_manual()
  

  