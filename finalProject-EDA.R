library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tinytex)
library(knitr)


## read data
dataf = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
str(dataf)
skimr::skim(dataf)

## check for duplicates
dataf_nodup = distinct(dataf)
# there were no EXACT duplicates, so I will continue using the original dataf.

## visualize missing data
visdat::vis_miss(dataf)
#only .3% missing, and it was from release date. Only 5.74% of release dates were missing. 

## In this EDA, we will be exploring the relationship between multiple dimensions, with the main investigation being on Which variables are most correlated with song popularity. We will also be looking into the relationship between variables such as tempo, energy, danceability, speechiness, instrumentalness, valence, and date.


## Looking at the data we hypothesize that danceability and valence will be most correlated with song popularity.


## create a data frame that have the top 10 correlations to track popularity.
dataf_smol = select(dataf, where(is.numeric), where(is.factor))

cor_matrix = dataf_smol %>%
  mutate(track_popularity = as.integer(track_popularity))%>%
  cor(method = 'spearman')

cor_df = as_tibble(cor_matrix, rownames = 'covar')

top_10 = cor_df%>%
  select(covar, track_popularity)%>%
  arrange(desc(track_popularity))%>%
  top_n(10)

## The top 5 correlated are acousticness, loudness, danceability, valence, and mode.


## PLOTS

# tempo vs energy 
ggplot(dataf, aes(x = tempo, y = energy, color = loudness)) + geom_point(size =.1) + geom_smooth()
ggplot(dataf, aes(x = tempo, y = energy, color = loudness)) + geom_point(size =.1) + geom_smooth() + facet_wrap(~playlist_genre)

# tempo vs danceability
ggplot(dataf, aes(x = tempo, y = danceability, color = loudness)) + geom_point(size =.1) + geom_smooth()
ggplot(dataf, aes(x = tempo, y = danceability, color = loudness)) + geom_point(size =.1) + geom_smooth() + facet_wrap(~playlist_genre)

# energy vs danceability
ggplot(dataf, aes(x = energy, y = danceability, color = tempo)) + geom_point(size =.1) + geom_smooth()
ggplot(dataf, aes(x = energy, y = danceability, color = tempo)) + geom_point(size =.1) + geom_smooth() + facet_wrap(~playlist_genre)


# playlist genre vs track popularity
ggplot(dataf, aes(x = playlist_genre, y = track_popularity)) + geom_boxplot()

# energy vs track popularity
ggplot(dataf, aes(x = energy, y = track_popularity)) + geom_point(size =.1) + geom_smooth()
ggplot(dataf, aes(x = energy, y = track_popularity)) + geom_point(size =.1) + geom_smooth() + facet_wrap(~playlist_genre)

# tempo vs track popularity
ggplot(dataf, aes(x = tempo, y = track_popularity)) + geom_point(size =.1) + geom_smooth()
ggplot(dataf, aes(x = tempo, y = track_popularity)) + geom_point(size =.1) + geom_smooth() + facet_wrap(~playlist_genre)

# danceability vs track popularity
ggplot(dataf, aes(x = danceability, y = track_popularity)) + geom_point(size =.1) + geom_smooth()
ggplot(dataf, aes(x = danceability, y = track_popularity)) + geom_point(size =.1) + geom_smooth() + facet_wrap(~playlist_genre)

# valence vs track popularity
ggplot(dataf, aes(x = valence, y = track_popularity)) + geom_point(size =.1) + geom_smooth()
ggplot(dataf, aes(x = valence, y = track_popularity)) + geom_point(size =.1) + geom_smooth() + facet_wrap(~playlist_genre)

# instrumentalness vs track popularity
ggplot(dataf, aes(x = instrumentalness, y = track_popularity)) + geom_point(size =.1) + geom_smooth()
ggplot(dataf, aes(x = instrumentalness, y = track_popularity)) + geom_point(size =.1) + geom_smooth() + facet_wrap(~playlist_genre)

# energy vs valence
ggplot(dataf, aes(x = energy, y = valence)) + geom_point(size =.1) + geom_smooth()
ggplot(dataf, aes(x = energy, y = valence)) + geom_point(size =.1) + geom_smooth() + facet_wrap(~playlist_genre)

# danceability vs valence
ggplot(dataf, aes(x = danceability, y = valence)) + geom_point(size =.1) + geom_smooth()
ggplot(dataf, aes(x = danceability, y = valence)) + geom_point(size =.1) + geom_smooth() + facet_wrap(~playlist_genre)

# genre vs danceability
ggplot(dataf, aes(x = playlist_genre, y = danceability)) + geom_boxplot()

# playlist genre vs valence
ggplot(dataf, aes(x = playlist_genre, y = valence)) + geom_boxplot()

# mutate data to filter buy date/year
dataf$track_album_release_date = as.Date(dataf$track_album_release_date)
datafdates = dataf %>% mutate(year = year(track_album_release_date), month = month(track_album_release_date), day = day(track_album_release_date))
ggplot(dataf, aes(x = year)) + geom_histogram()

# very few songs before 1980 which could skew data, so will filter songs to be after 1979

# valence vs release date
ggplot(dataf %>% filter(year>1979) %>% group_by(year) %>% summarise(y = mean(valence)), aes(x = year, y = y)) + geom_line() + ylab("valence")
ggplot(dataf %>% filter(year>1979) %>% group_by(year, playlist_genre) %>% summarise(y = mean(valence)), aes(x = year, y = y)) + geom_line() + facet_wrap(~playlist_genre) + ylab("valence")

# loudness vs release date
ggplot(datafdates %>% filter(year>1979) %>% group_by(year) %>% summarise(y = mean(loudness)), aes(x = year, y = y)) + geom_line() + ylab("loudness")
ggplot(datafdates %>% filter(year>1979) %>% group_by(year, playlist_genre) %>% summarise(y = mean(loudness)), aes(x = year, y = y)) + geom_line() + facet_wrap(~playlist_genre) + ylab("loudness")

# danceability vs release date
ggplot(datafdates %>% filter(year>1979) %>% group_by(year) %>% summarise(y = mean(danceability)), aes(x = year, y = y)) + geom_line() + ylab("danceability")
ggplot(datafdates %>% filter(year>1979) %>% group_by(year, playlist_genre) %>% summarise(y = mean(danceability)), aes(x = year, y = y)) + geom_line() + facet_wrap(~playlist_genre) + ylab("danceability")

# tempo vs release date
ggplot(datafdates %>% filter(year>1979) %>% group_by(year) %>% summarise(y = mean(tempo)), aes(x = year, y = y)) + geom_line() + ylab("tempo")
ggplot(datafdates %>% filter(year>1979) %>% group_by(year, playlist_genre) %>% summarise(y = mean(tempo)), aes(x = year, y = y)) + geom_line() + facet_wrap(~playlist_genre) + ylab("tempo")

# track popularity vs release date
ggplot(datafdates %>% filter(year>1979) %>% group_by(year) %>% summarise(y = mean(track_popularity)), aes(x = year, y = y)) + geom_line() + ylab("track_pop")
ggplot(datafdates %>% filter(year>1979) %>% group_by(year, playlist_genre) %>% summarise(y = mean(track_popularity)), aes(x = year, y = y)) + geom_line() + facet_wrap(~playlist_genre) + ylab("track_pop")

# track popularity vs release date after 1990
ggplot(datafdates %>% filter(year>1989) %>% group_by(year) %>% summarise(y = mean(track_popularity)), aes(x = year, y = y)) + geom_line() + ylab("track_pop")
ggplot(datafdates %>% filter(year>1989) %>% group_by(year, playlist_genre) %>% summarise(y = mean(track_popularity)), aes(x = year, y = y)) + geom_line() + facet_wrap(~playlist_genre) + ylab("track_pop")

