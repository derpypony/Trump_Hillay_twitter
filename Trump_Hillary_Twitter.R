options(stringsAsFactors = F)
library(tidyverse)
library(tidytext)
library(NLP)
library(openNLP)
# We need a NLP model installed, please run the following code
library("openNLPmodels.en")
# If you are doing all your work in your local machine, please run
# library("openNLPmodels.en", lib.loc="~/R/win-library/3.5") instead. 3.5 is the version of R 
# I am currently using, you can choose to change that to the version of R you are using.
library(wordcloud)
US_election <- read_csv('US_election.csv')
sent.token.annotator <- Maxent_Sent_Token_Annotator(language = "en")
word.token.annotator <- Maxent_Word_Token_Annotator(language = "en")
pos.tag.annotator <- Maxent_POS_Tag_Annotator(language = "en")
# We creat a function called word_cloud_plot, which has three parameter, 
# you put in candidate, topic(words like 'gun', 'violence', 'border', etc), and
# type(JJ:adjective, VB:verb, NN:noun), it will plot the word cloud
word_cloud_plot <- function(candidate = c('realDonaldTrump', 'HillaryClinton'), topic, type = c('JJ', 'NN', 'VB')){
  candidate_about_topic <- US_election %>% 
    filter(str_detect(text, regex(topic, ignore_case = T))) %>% 
    filter(handle == candidate) %>% 
    select(1,2,3) %>% 
    mutate(clean_text = str_remove_all(text, 'https://[\\w\\d/.]+'))
  # we use NLP::annotate instead of annotate because there is another annotate in tidyverse
  annotations <- NLP::annotate(candidate_about_topic$clean_text,list(sent.token.annotator,
                                                                     word.token.annotator,
                                                                     pos.tag.annotator))
  ann.df <- as.data.frame(annotations) %>% filter(type == 'word') %>% 
    select(3,4,5)
  ann.df$features <- unlist(ann.df$features) %>% unname()
  new.ann.df <- ann.df %>% filter(str_detect(features,'NN|JJ|VB'))
  new.ann.df$word <- ''
  
  content <- paste(candidate_about_topic$clean_text, collapse = ' ')
  for(i in 1:nrow(new.ann.df)){
    new.ann.df$word[i] <- substr(content,new.ann.df[i,1],new.ann.df[i,2])
  }
  new.ann.df$word <- tolower(new.ann.df$word)
  my.new.ann.df <- new.ann.df %>% anti_join(stop_words,by = "word") %>% 
    filter(str_detect(word, '^\\w+$'))
  
  word_count <- my.new.ann.df %>% 
    filter(str_detect(features, type)) %>% 
    count(word, sort = T)
  par(mar=c(0,0,0,0))
  wordcloud(word_count$word, 
            word_count$n, 
            max.words = 100,
            scale = c(2.5,.5), 
            random.color = TRUE, 
            colors = brewer.pal(9, "Set1"))
}
word_cloud_plot(candidate = 'HillaryClinton', topic = 'trump', type = 'JJ')
word_cloud_plot(candidate = 'realDonaldTrump', topic = 'hillary', type = 'JJ')
word_cloud_plot(candidate = 'HillaryClinton', topic = 'hillary', type = 'JJ')
word_cloud_plot(candidate = 'realDonaldTrump', topic = 'trump', type = 'JJ')
word_cloud_plot(candidate = 'HillaryClinton', topic = 'tax', type = 'JJ')
word_cloud_plot(candidate = 'realDonaldTrump', topic = 'tax', type = 'JJ')
word_cloud_plot(candidate = 'HillaryClinton', topic = 'education', type = 'JJ')
word_cloud_plot(candidate = 'realDonaldTrump', topic = 'education', type = 'JJ')
# we need to modify word_cloud_plot a bit to make a perfct worldcloud on topic immigrant
word_cloud_plot_2 <- function(candidate = c('realDonaldTrump', 'HillaryClinton'), topic){
  candidate_about_topic <- US_election %>% 
    filter(str_detect(text, regex(topic, ignore_case = T))) %>% 
    filter(handle == candidate) %>% 
    select(1,2,3) %>% 
    mutate(clean_text = str_remove_all(text, 'https://[\\w\\d/.]+'))
  # we use NLP::annotate instead of annotate because there is another annotate in tidyverse
  annotations <- NLP::annotate(candidate_about_topic$clean_text,list(sent.token.annotator,
                                                                     word.token.annotator,
                                                                     pos.tag.annotator))
  ann.df <- as.data.frame(annotations) %>% filter(type == 'word') %>% 
    select(3,4,5)
  ann.df$features <- unlist(ann.df$features) %>% unname()
  new.ann.df <- ann.df %>% filter(str_detect(features,'NN|JJ|VB'))
  new.ann.df$word <- ''
  
  content <- paste(candidate_about_topic$clean_text, collapse = ' ')
  for(i in 1:nrow(new.ann.df)){
    new.ann.df$word[i] <- substr(content,new.ann.df[i,1],new.ann.df[i,2])
  }
  new.ann.df$word <- tolower(new.ann.df$word)
  my.new.ann.df <- new.ann.df %>% anti_join(stop_words,by = "word") %>% 
    filter(str_detect(word, '^\\w+$'))
  
  word_count <- my.new.ann.df %>%
    count(word, sort = T) %>% 
    filter(!str_detect(word, topic))
  par(mar=c(0,0,0,0))
  wordcloud(word_count$word, 
            word_count$n, 
            max.words = 100,
            min.freq = 2,
            scale = c(2.5,.5), 
            random.color = TRUE, 
            colors = brewer.pal(9, "Set1"))
}
word_cloud_plot_2(candidate = 'realDonaldTrump', topic = 'immigrant')
word_cloud_plot_2(candidate = 'HillaryClinton', topic = 'immigrant')
# we modify word_cloud_plot_2 a bit to fit topic "jobs"
word_cloud_plot_3 <- function(candidate = c('realDonaldTrump', 'HillaryClinton'), topic){
  candidate_about_topic <- US_election %>% 
    filter(str_detect(text, regex(topic, ignore_case = T))) %>% 
    filter(handle == candidate) %>% 
    select(1,2,3) %>% 
    mutate(clean_text = str_remove_all(text, 'https://[\\w\\d/.]+'))
  # we use NLP::annotate instead of annotate because there is another annotate in tidyverse
  annotations <- NLP::annotate(candidate_about_topic$clean_text,list(sent.token.annotator,
                                                                     word.token.annotator,
                                                                     pos.tag.annotator))
  ann.df <- as.data.frame(annotations) %>% filter(type == 'word') %>% 
    select(3,4,5)
  ann.df$features <- unlist(ann.df$features) %>% unname()
  new.ann.df <- ann.df %>% filter(str_detect(features,'NN|JJ|VB'))
  new.ann.df$word <- ''
  
  content <- paste(candidate_about_topic$clean_text, collapse = ' ')
  for(i in 1:nrow(new.ann.df)){
    new.ann.df$word[i] <- substr(content,new.ann.df[i,1],new.ann.df[i,2])
  }
  new.ann.df$word <- tolower(new.ann.df$word)
  my.new.ann.df <- new.ann.df %>% anti_join(stop_words,by = "word") %>% 
    filter(str_detect(word, '^\\w+$'))
  
  word_count <- my.new.ann.df %>%
    count(word, sort = T) %>% 
    filter(!str_detect(word, topic))
  par(mar=c(0,0,0,0))
  wordcloud(word_count$word, 
            word_count$n, 
            max.words = 100,
            min.freq = 4,
            scale = c(2.5,.5), 
            random.color = TRUE, 
            colors = brewer.pal(9, "Set2"))
}

word_cloud_plot_3(candidate = 'realDonaldTrump', topic = 'jobs')
word_cloud_plot_3(candidate = 'HillaryClinton', topic = 'jobs')

tidy_US_2 <- US_election %>% 
  mutate(clean_text = str_remove_all(text, 'https://[\\w\\d/.]+')) %>%
  select(2,29) %>% 
  unnest_tokens(two_gram, clean_text, token = 'ngrams', n = 2) %>%
  mutate(word_1 = str_split(two_gram, ' ') %>% lapply('[', 1) %>% unlist,
         word_2 = str_split(two_gram, ' ') %>% lapply('[', 2) %>% unlist) %>% 
  anti_join(stop_words, by = c('word_1'  = 'word')) %>% 
  anti_join(stop_words, by = c('word_2'  = 'word')) %>% 
  count(two_gram, handle, sort = T) %>% filter(n > 1)

fav_retw_count <- function(candidate = c('realDonaldTrump', 'HillaryClinton'), topic){
  candidate_about_topic <- US_election %>% 
    filter(str_detect(text, regex(topic, ignore_case = T))) %>% 
    filter(handle == candidate)
  retweet_ave = mean(candidate_about_topic$retweet_count, na.rm = T)
  favorite_ave = mean(candidate_about_topic$favorite_count, na.rm = T)
  return(tibble(retweet_ave = retweet_ave,
                favorite_ave = favorite_ave,
                topic = topic,
                candidate = candidate))
}


political_list_2 <- list()
pb <- txtProgressBar(min = 1, max = nrow(tidy_US_2), style = 3)
for(i in 1:nrow(tidy_US_2)){
  political_list_2[[i]] <- fav_retw_count(tidy_US_2$handle[i], tidy_US_2$two_gram[i])
  setTxtProgressBar(pb, i)
}
political_list_2 <- do.call(rbind,political_list_2)
close(pb)

top_word_2 <- political_list_2 %>% 
  group_by(candidate) %>% 
  top_n(50, wt = retweet_ave) %>% 
  ungroup()


top_word_2 %>% 
  ggplot(aes(retweet_ave, favorite_ave, color = candidate, label = topic)) +
  geom_text(check_overlap = T, hjust = 0.6, vjust = 0.6) +
  geom_point() +
  scale_x_log10()+
  scale_y_log10()+
  labs(x = 'average retweet', y = 'average favorit retweet', 
       title = 'Topics That Supporters for Both Candidates Like the Most')

library(plotly)
top_word_2 %>% plot_ly(x = ~ retweet_ave, y = ~ favorite_ave, 
                       color = ~ candidate, text = ~ topic, colors = c('blue', 'red'), 
                       type = 'scatter', mode = 'markers', hoverinfo = 'text') %>% 
  layout(title = 'Top 50 Liked Topics (2grams) for Each Candidate',
         xaxis = list(title = '# Retweet Average', type = 'log'),
         yaxis = list(title = '# favorite Average', type = 'log'))
