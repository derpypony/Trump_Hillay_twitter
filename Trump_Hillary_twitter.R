US_election <- read_csv("../input/tweets.csv", 
                        col_types = cols(id = col_character()))
head(US_election,2)
sent.token.annotator <- Maxent_Sent_Token_Annotator(language = "en")
word.token.annotator <- Maxent_Word_Token_Annotator(language = "en")
pos.tag.annotator <- Maxent_POS_Tag_Annotator(language = "en")
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
library(plotly)
library(htmlwidgets)
library(IRdisplay)
p1 <- US_election %>% 
  filter(retweet_count < 5e4, retweet_count < 1e5) %>%
  plot_ly(x = ~ retweet_count, y = ~ favorite_count,
          color = ~ handle, colors = c('blue', 'red'), opacity = 0.4)
saveWidget(p1, 'demo1.html', selfcontained = FALSE)
display_html('<iframe src="demo1.html",width = 1000, height = 400></iframe>')
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
fav_retw_count('realDonaldTrump', 'china')
tidy_US <- US_election %>% 
  mutate(clean_text = str_remove_all(text, 'https://[\\w\\d/.]+')) %>%
  select(2,29) %>% 
  unnest_tokens(word, clean_text) %>% 
  anti_join(stop_words, by = 'word') %>% 
  count(word, sort = T)

head(tidy_US)
trump_list <- list()
pb <- txtProgressBar(min = 1, max = nrow(tidy_US), style = 3)
for(i in 1:nrow(tidy_US)){
  trump_list[[i]] <- fav_retw_count('realDonaldTrump', tidy_US$word[i])
  setTxtProgressBar(pb, i)
}
trump_list <- do.call(rbind,trump_list)
head(trump_list)
hillary_list <- list()
pb <- txtProgressBar(min = 1, max = nrow(tidy_US), style = 3)
for(i in 1:nrow(tidy_US)){
  hillary_list[[i]] <- fav_retw_count('HillaryClinton', tidy_US$word[i])
  setTxtProgressBar(pb, i)
}
hillary_list <- do.call(rbind,hillary_list)
close(pb)
head(hillary_list)
political_list <- rbind(trump_list,hillary_list)

top_word <- political_list %>% 
  group_by(candidate) %>% 
  top_n(50, wt = retweet_ave) %>% 
  ungroup()
head(top_word)
p2 <- top_word %>% plot_ly(x = ~ retweet_ave, y = ~ favorite_ave, 
                           color = ~ candidate, text = ~ topic, colors = c('blue', 'red'), 
                           type = 'scatter', mode = 'markers', hoverinfo = 'text') %>% 
  layout(title = 'Top 50 Liked Topics (1gram) for Each Candidate',
         xaxis = list(title = '# Retweet Average', type = 'log'),
         yaxis = list(title = '# favorite Average', type = 'log'))
saveWidget(p2, 'demo2.html', selfcontained = FALSE)
display_html('<iframe src="demo2.html", width = 1000, height = 500></iframe>')
# Put your hover to dots and see text
# we can plot the same top_word plot with 2grams
tidy_US_2 <- US_election %>% 
  mutate(clean_text = str_remove_all(text, 'https://[\\w\\d/.]+')) %>%
  select(2,29) %>% 
  unnest_tokens(two_gram, clean_text, token = 'ngrams', n = 2) %>%
  mutate(word_1 = str_split(two_gram, ' ') %>% lapply('[', 1) %>% unlist,
         word_2 = str_split(two_gram, ' ') %>% lapply('[', 2) %>% unlist) %>% 
  anti_join(stop_words, by = c('word_1'  = 'word')) %>% 
  anti_join(stop_words, by = c('word_2'  = 'word')) %>% 
  count(two_gram, handle, sort = T) %>% filter(n > 1)
political_list_2 <- list()
pb <- txtProgressBar(min = 1, max = nrow(tidy_US_2), style = 3)
for(i in 1:nrow(tidy_US_2)){
  political_list_2[[i]] <- fav_retw_count(tidy_US_2$handle[i], tidy_US_2$two_gram[i])
  setTxtProgressBar(pb, i)
}
political_list_2 <- do.call(rbind,political_list_2)
close(pb)
head(political_list_2)
top_word_2 <- political_list_2 %>% 
  group_by(candidate) %>% 
  top_n(50, wt = retweet_ave) %>% 
  ungroup()
p3 <- top_word_2 %>% plot_ly(x = ~ retweet_ave, y = ~ favorite_ave, 
                             color = ~ candidate, text = ~ topic, colors = c('blue', 'red'), 
                             type = 'scatter', mode = 'markers', hoverinfo = 'text') %>% 
  layout(title = 'Top 50 Liked Topics (2grams) for Each Candidate',
         xaxis = list(title = '# Retweet Average', type = 'log'),
         yaxis = list(title = '# favorite Average', type = 'log'))
saveWidget(p3, 'demo3.html', selfcontained = FALSE)
display_html('<iframe src="demo3.html", width = 1000, height = 500></iframe>')
# Put your hover to dots and see text