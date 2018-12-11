#Simran Batra
#August 27, 2018

library(tidyverse)
library(tidytext)
library(knitr)

drake <- read.csv("astroworld.csv")
audio <- read.csv("astroworld_happy_sad.csv")


drake <- mutate(drake, lyrics=as.character(lyrics))

#unnesttokens
unnest_scorpion <- drake %>%
  unnest_tokens(word, lyrics)

#count how many times each word appears in each song
count_scorpion <- unnest_scorpion %>%
  count(song, word, sort = TRUE) %>%
  ungroup()

#count the total words in each album
totalwords_scorpion <- count_scorpion %>% 
  group_by(song) %>% 
  summarize(totalwords = sum(n)) %>%
  ungroup()

#merge to one data set
words_scorpion <- left_join(count_scorpion, totalwords_scorpion, by="song")

#get bing sentiments and merge
bing <- filter(sentiments, lexicon=="bing")
merge_lexicon <- merge(unnest_scorpion, bing)

#count total positive and negative words
test <-  merge_lexicon %>%
  count(song, sentiment, sort=TRUE)

#join that count with total number of words in each album
ratio_scorpion <- left_join(test, totalwords_scorpion, by="song")

#create new variable with proportion
ratio_scorpion <- mutate(ratio_scorpion, proportion = n/totalwords)
negative_scorpion <- filter(ratio_scorpion, sentiment == "negative")
positive_scorpion <- filter(ratio_scorpion, sentiment == "positive")
negative_scorpion <- mutate(negative_scorpion, "negative proportion" = proportion)
negative_scorpion <- select(negative_scorpion, -proportion, -sentiment, -n, -totalwords)
positive_scorpion <- mutate(positive_scorpion, "positive proportion" = proportion)
positive_scorpion <- select(positive_scorpion, -proportion, -sentiment, -n, -totalwords)
ratio_scorpion1 <- left_join(negative_scorpion, positive_scorpion, by="song")
ratio_scorpion1 <- mutate(ratio_scorpion1, "difference" = `positive proportion` - `negative proportion`)

kable(head(arrange(select(ratio_scorpion1, song, difference), difference), 3))

# top 3 most lyrically positive astroworld songs

kable(head(arrange(select(ratio_scorpion1, song, difference), -difference), 3))

# a lyrical emotional analysis of astroworld

#unnest tokens
unnest_drake <- drake %>%
  unnest_tokens(word, lyrics)

#count how many times each word appears in each album
count_drake <- unnest_drake %>%
  count(word, sort = TRUE) %>%
  ungroup()

#count the total words in each album
totalwords_drake <- count_drake %>% 
  summarize(totalwords = sum(n)) %>%
  ungroup()

#remove stop words
data(stop_words)
nostopwords_drake <- unnest_drake %>% 
  anti_join(stop_words, by="word")

#get afinn sentiment, merge with tokenized data, remove NA
afinn <- filter(sentiments, lexicon=="AFINN")
afinnmerge_drake <- left_join(unnest_drake, afinn, by="word")
afinnmerge_drake <- mutate(afinnmerge_drake, score = ifelse(is.na(score), 0, score))

#remove certain cuss words that are not coded correctly because of lack of context
word <- c("ass", "fucking", "fuck", "shit", "damn", "hell", "bitches", "bitch", "nigga", "niggas")
word_vector <- word
word <- data.frame(word)
afinnmerge_drake1 <- anti_join(afinnmerge_drake, word, by="word")

#count the total score in each song
afinn_count <- afinnmerge_drake1 %>%
  group_by(song) %>%
  summarize(total=sum(score)) %>%
  ungroup()

#merge with original untokenized data
afinn_tidy <- left_join(drake, afinn_count, by="song")

#time series
afinn_scorpion <- mutate(afinn_tidy, track = 1:17)


g <- ggplot(afinn_scorpion, aes(x=track, y=total)) +
  geom_line() +
  xlab("Song") +
  ylab("AFINN Sentiment Score") +
  ggtitle("An Emotional Analysis of Astroworld") +
  geom_hline(aes(yintercept=0), colour="#990000", linetype="dotted")
g

#top 3 astroworld songs by sentiment


nrc <- filter(sentiments, lexicon=="nrc")
nrc_drake <- inner_join(unnest_drake, nrc, by="word")
join_drake <- left_join(words_scorpion, nrc, by="word")
join_drake <- rename(join_drake, "wordcount"="n")

song_drake <- join_drake %>%
  group_by(song, sentiment) %>%
  count(sentiment, sort=FALSE, wt=1/totalwords) %>%
  ungroup()
song_drake <- filter(song_drake, sentiment!=is.na(sentiment))
song_drake <- filter(song_drake, sentiment!="positive")
song_drake <- filter(song_drake, sentiment!="negative")

song_drake %>% 
  group_by(sentiment) %>%  
  top_n(3) %>% 
  ungroup %>%
  ggplot(aes(song, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Proportion") +
  facet_wrap(~sentiment, ncol = 2, scales = "free") +
  ggtitle("Top Songs of Each Sentiment") + 
  coord_flip()


#songs ranked by lyrical sadness

sad_songs <- select(song_drake, song, sentiment, n)
sad_songs <- filter(sad_songs, sentiment=="sadness")
sad_songs <- arrange(sad_songs, -n)
kable(head(select(sad_songs, song, n), 5))

#songs ranked by audio sadness

audio <- read.csv("astroworld_happy_sad.csv")
audio_sad <- select(audio, title, predictions)
sad_songs <- arrange(audio_sad, -predictions)
kable(head(sad_songs, 5))


#songs ranked by lyrical disgust
disgust_songs <- select(song_drake, song, sentiment, n)
disgust_songs <- filter(disgust_songs, sentiment=="disgust")
disgust_songs <- arrange(disgust_songs, -n)
kable(head(select(disgust_songs, song, n), 5))

#songs ranked by lyrical joy

joy_songs <- select(song_drake, song, sentiment, n)
joy_songs <- filter(joy_songs, sentiment=="joy")
joy_songs <- arrange(joy_songs, -n)
kable(head(select(joy_songs, song, n), 5))


#songs ranked by audio happiness
audio <- read.csv("astroworld_happy_sad.csv")
audio_sad <- select(audio, title, predictions)
sad_songs <- arrange(audio_sad, predictions)
kable(head(sad_songs, 5))

#songs ranked by audio 'danceability'
audio <- read.csv("astroworld_happy_sad.csv")
audio_sad <- select(audio, title, danceability)
sad_songs <- arrange(audio_sad, -danceability)
#kable(head(sad_songs, 5))
ggplot(data=sad_songs, aes(x=title, y=danceability)) +
  geom_bar(stat="identity", width=0.5, fill="darkseagreen") +
  ggtitle("Danceability of Each Song in Astroworld") +
  theme(axis.text.x=element_text(angle=45,hjust=1))

#songs ranked by audio 'energy'
audio <- read.csv("astroworld_happy_sad.csv")
audio_sad <- select(audio, title, energy)
sad_songs <- arrange(audio_sad, -energy)
#kable(head(sad_songs, 5))
ggplot(data=sad_songs, aes(x=title, y=energy)) +
  geom_bar(stat="identity", width=0.5, fill="steelblue") +
  ggtitle("Energy of Each Song in Astroworld") +
  theme(axis.text.x=element_text(angle=45,hjust=1))