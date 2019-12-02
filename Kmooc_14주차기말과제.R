load("C:/Users/com/Documents/R/asset-v1_SejonguniversityK+SJMOOC10K+2018_03SJ10+type@asset+block@bts_rtweet (1).RData")
getwd()
install.packages("tidytext")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("ggplot2")

library("tidytext")
library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")
library("ggplot2")
library(janeaustenr)
# tidytext, dplyr, tidyr, stringr, lubridate, ggplot2 패키지 설치

#위 처리 과정을 거친 데이터를 bts_text라는 새로운 객체에 할당
bts_text <- bts_rtweet %>% select(status_id, created_at, text)
#새로운 데이터 프레임 구조 확인 
dim(bts_text)

#위 처리 과정을 거친 데이터를 bts_text_unique라는 새로운 객체에 할당 
bts_text_unique <-bts_text %>% filter(!duplicated(text))

remove_regex = c("trump", "great")
# dt_rtweet 객체명의 데이터를 unnest_tokens( ) 함수를 이용하여 tidy 데이터로 만듬
bts_tidy <- bts_text_unique %>%
  mutate(text = str_replace_all(text, remove_regex, "")) %>%
  mutate(text = str_replace_all(text, "[#@]?[^[:ascii:]]", "")) %>%
  unnest_tokens(word, text, token = "regex") %>%
  filter(!word %in% stop_words$word)
bts_tidy

# 시간의 흐름에 따른 감정 변화를 살펴볼 수 있는 바 그래프를 ggplot( ) 함수를 이용하여 만듬
bts_tidy %>%
  filter(!word %in% c("trump", "great")) %>%
  mutate(time_floor = floor_date(created_at, unit = "hour")) %>%
  inner_join(get_sentiments("bing")) %>%
  count(time_floor, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(x=time_floor,y=sentiment)) +
  geom_col() +
  scale_x_datetime(date_breaks = "1 day", date_labels = " %b %d ")
