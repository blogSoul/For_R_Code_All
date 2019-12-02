install.packages("pdftools")
install.packages("wordcloud")
library(pdftools)
library(stringr)
library(wordcloud)
korea_text <- pdf_text("R/korea.pdf")

#하나의 문자열로 연결
korea_tidy_word <- str_c(korea_text, collapse =  "") %>%
  #앞뒤 공백 제거
  str_trunc(31828, side="right") %>%
  #공백 제거
  str_replace_all("[[:space:]]{1,}", " ") %>%
  #비알파벳 문자 제거 
  str_replace_all("[^[:ascii:]]+", " ") %>%
  #소문자로 표준화
  tolower() %>%
  #줄임말 표시 제거 
  str_replace_all("['][sS]", " ")%>%
  #구두점 표함한 어휘처리 
  str_replace_all(" u\\.s\\. ", " usa ")%>%
  str_replace_all(" r\\&b "," rnb ")%>%
  #인용 표시 처리
  str_replace_all("\\[[[:digit:]]+\\]|\\([[:digit:]]+\\)", "")%>%
  #구두점 제거
  str_replace_all("[[:punct:]^]+", "")%>%
  #숫자 제거
  str_replace_all("[[:digit:]]+", " ")%>%
  #전처리 과정에서 파생된 공백 제거 
  str_replace_all("[[:space:]]{1,}", " ")%>%
  #빈칸을 기준으로 분할해서(쪼개서) 토큰화
  str_split(" ")%>%
  #리스트를 단어들의 벡터로 변환
  unlist()

#각 단어 출현 빈도 수 세서, 내림차순 정렬
korea_tidy_word_freq <- korea_tidy_word %>%
  table() %>% sort(decreasing = TRUE)

pal <- brewer.pal(8, "Dark2")# "Dark2"에서 8가지 색상 검색
set.seed(405)
wordcloud(words = names(korea_tidy_word_freq),# 고유 단어의 열
          freq = korea_tidy_word_freq, #단어의 빈도 
          min.freq = 5, #표시된 단어의 최소 빈도 
          max.words = 500,# 빈도 순서로 표시된 500개의 단어
          random.order = FALSE,# 중앙에 위치한 최다 빈도 단어들
          rot.per = 0.1,# 플롯에서 회전하는 단어의 비율
          scale = c(4, 0.3),# 단어의 크기 범위
          colors = pal)# 단어 색상
