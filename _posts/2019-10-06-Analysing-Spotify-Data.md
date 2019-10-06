---
layout: post
title: "Spotify Top50 Charts Analysis"
---

Setup
-----

In order to access Spotify data, first register yourself a "Spotify for Developer" account and get your API key by selecting the Dashboard then click on "Create a Client ID" and fill in the required questions. Go to your new app page, click on "Show Client Secret" to retrieve your Client Secret, copy it and your Client ID to replace the following. The you will have your Spotify access token. *Note: make sure "<http://localhost:8888>" and "<http://localhost:8888/callback>" are added under Redirect URIs from the settings*

``` r
library(spotifyr)
library(dplyr) # To use piping
```

``` r
library(stringr) # string manipulation
id <- "8d280370d04c4cda9c0a463cfa8636e3"
secret <- "b91b6996ad194150b415313ab7b8db80"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()
```

Get Metadata from Spotify
-------------------------

In this analysis, I am comparing the Top50 tracks from all countries so I am retrieving the playlist from the Top50 Charts by country in Spotify. The easiest way to pull the song metadata from Spotify is accessing through the playlist in your Spotify account. To do this, first add all the charts list manually to your playlist and then using get\_user\_playlists, get\_playlist\_tracks and get\_track\_audio\_features functions from the spotifyr package to retrieve the metadata of those songs. To use get\_user\_playlists function, give your Spotify ID as input.

``` r
# only select the Top50 charts playlist, rename playlist name as respective country
playlist <- get_user_playlists('gorpublicusage') %>% filter(grepl("^Top 50.*", name)) %>% select(id, country = name) 
```

``` r
playlist$country <- lapply(playlist$country, str_remove, "Top 50 ") 

topTrack <-data.frame()
remaining_countries <- c()
for (i in 1:dim(playlist)[1]) {
    print(str_c("Processing Country: ", playlist$country[i]))
    succeed = FALSE
    tryCatch({
      # get the audio features of each track in the top50 charts by country using the track.id
      df <- get_playlist_tracks(playlist$id[i]) %>%
        select(track.id,track.name,track.artists,track.popularity) 
      topTrack <- get_track_audio_features(df$track.id) %>% 
        select(-track_href, -uri, -id, -analysis_url, -type, -time_signature) %>%
        mutate(Track = as.character(df$track.name), Track.Id = as.character(df$track.id), 
               Artists = as.list(lapply(df$track.artists , "[[" , "name" )), 
               Artists.Id = as.list(lapply(df$track.artists , "[[" , "id" )),
               Popularity = as.numeric(df$track.popularity), 
               Country = as.character(playlist$country[i])) %>% rbind(topTrack)
      succeed = TRUE
      print("Succeed!")
    }, error = function(e) { print(e) })
    
    # If fail, add the country to the remaining countries list
    if (!succeed) {
      remaining_countries <- c(remaining_countries, playlist$country[i])
    }
}
```

Data Cleansing
--------------

Since data accessed is in German, so I have to map the respective countries name to its English name and add an additional column of Continent

``` r
library(plyr)
```

``` r
library(magrittr) # to us %<>% piping
topTrack$Country <- as.character(mapvalues(topTrack$Country,from = c('Australien', 'Österreich', 
'Belgien', 'Bolivien', 'Kanada', 'Bulgarien', 'Brasilien', 'Dänemark', 'Tschechische', 'Estland', 'Deutschland', 'Frankreich', 'Finnland', 'Griechenland', 'Ungarn', 'Island', 'Irland', 'Israel', 
'Italien', 'Luxemburg','Litauen', 'Lettland', 'Mexiko', 'Neuseeland', 'Niederlande', 'Norwegen', 
'Polen', 'Rumänien', 'Portugal', 'Slowakei', 'Südafrika', 'Spanien', 'Schweden', 'Schweiz', 
'Großbritannien', 'USA', 'Uruguay', 'Peru', 'Panama', 'Paraguay','Nicaragua', 'Guatemala', 
'Honduras', 'Ecuador', 'El', 'Argentinien', 'Costa', 'Chile', 'Kolumbien', 'Dominikanische', 
'Türkei', 'Hong Kong', 'Indonesien', 'Malaysia', 'Malta', 'Philippinen', 'Singapur', 'Taiwan', 
'Thailand', 'Japan','Indien','Vietnam'),
to=c('Australia', 'Austria', 'Belgium', 'Bolivia', 'Canada', 'Bulgaria', 'Brazil', 'Denmark', 
     'Czech Republic', 'Estonia', 'Germany', 'France', 'Finland', 'Greece', 'Hungary', 'Iceland', 
     'Ireland', 'Israel', 'Italy', 'Luxembourg','Lithuania', 'Latvia', 'Mexico', 'New Zealand', 
     'Netherlands', 'Norway', 'Poland', 'Romania', 'Portugal', 'Slovakia', 'South Africa', 'Spain', 
     'Sweden', 'Switzerland', 'UK', 'USA', 'Uruguay', 'Peru', 'Panama', 'Paraguay', 'Nicaragua', 
     'Guatemala', 'Honduras', 'Ecuador', 'El Salvador', 'Argentina', 'Costa Rica', 'Chile', 
     'Colombia', 'Dominican  Republic', 'Turkey','China','Indonesia','Malaysia', 'Malta' ,
     'Philippines', 'Singapore', 'Taiwan', 'Thailand', 'Japan','India', 'Vietnam')))
```

``` r
topTrack$Continent <- ""
topTrack %<>% 
  mutate(Continent=replace(Continent, Country %in% c('Turkey','China','Indonesia', 'Malaysia', 'Philippines', 'Singapore', 'Taiwan', 'Thailand', 'Japan','India', 'Vietnam','Israel'), "AS")) %>%  mutate(Continent=replace(Continent, Country %in% c('Austria', 'Belgium', 'Bolivia', 'Bulgaria', 'Denmark', 'Czech Republic', 'Estonia', 'Germany', 'France', 'Finland', 'Greece', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Luxembourg','Lithuania', 'Latvia', 'Netherlands', 'Norway', 'Poland', 'Romania', 'Portugal', 'Slovakia', 'Spain', 'Sweden', 'Switzerland','Malta','UK'), "EU")) %>%  
  mutate(Continent=replace(Continent, Country %in% c('Canada', 'USA', 'Mexico', 'Panama', 'Nicaragua', 'Guatemala', 'Honduras', 'El Salvador', 'Costa Rica', 'Dominican Republic'), "NA")) %>% mutate(Continent=replace(Continent, Country %in% c('Brazil', 'Uruguay', 'Peru', 'Paraguay', 'Ecuador', 'Argentina', 'Chile', 'Colombia'), "SA")) %>% 
  mutate(Continent=replace(Continent, Country %in% c('Australia', 'New Zealand'), "OC")) %>%
  mutate(Continent=replace(Continent, Country %in% c('South Africa'), "AF"))
```

Exploratory Data Analysis
-------------------------

``` r
library(ggplot2)
library(gameofthrones)
library(scales) #rescale values 
library(wrapr) # for piping with ggplot
library(ggjoy) # to plot joyplot
```

```

``` r
# Allow piping into ggplot2
apply_left.gg <- function(pipe_left_arg, pipe_right_arg, pipe_environment, left_arg_name,
                          pipe_string, right_arg_name) { pipe_right_arg <- eval(pipe_right_arg,
                         envir = pipe_environment, enclos = pipe_environment)
  pipe_left_arg + pipe_right_arg 
}

# rescale the valence & dancebility score to make it more interpretable 
topTrack$valence <- rescale(topTrack$valence, to = c(-10, 10))
topTrack$danceability <- rescale(topTrack$danceability, to = c(-10, 10))
```

``` r
ggplot(topTrack, aes(x=valence, y=Continent, fill=Continent)) %.>% 
  geom_joy(scale=3, rel_min_height=0.01) %.>% 
  scale_fill_got(discrete = TRUE, option = "Margaery") %.>%  
  scale_y_discrete(expand = c(0.01, 0)) %.>% 
  ggtitle("Valence distributions of Top Songs Across Continents", 
          subtitle = "Based on valence pulled from Spotify's Web API with spotifyr") %.>%
  theme_joy() %.>% theme(axis.title.y = element_blank(), legend.position='none')
```

    ## Picking joint bandwidth of 1.27
![_config.yml]({{ site.baseurl }}/images/valence_Joyplot.png)

``` r
ggplot(topTrack, aes(x=danceability, y=Continent, fill=Continent)) +
  geom_joy(scale=3, rel_min_height=0.01) +
  scale_fill_got(discrete = TRUE, option = "Daenerys") +
  scale_y_discrete(expand = c(0.01, 0)) +
  ggtitle("Danceability distributions of Top Songs Across Continents", subtitle = "Based on danceability pulled from Spotify's Web API with spotifyr") +
  theme_joy() +
  theme(axis.title.y = element_blank(),
        legend.position='none')
```

    ## Picking joint bandwidth of 0.978
![_config.yml]({{ site.baseurl }}/images/danceability_Joyplot.png)

Valence World Map

``` r
left_join(map_data('world'), topTrack, by = c('region' = 'Country')) %.>% 
  ggplot(., aes(x = long, y = lat, group = group)) %.>% geom_polygon(aes(fill = valence)) %.>%
  scale_fill_got(option = "Tully") %.>% 
  labs(title = "How 'happy' the Top50 songs across 62 countries are?", 
       subtitle = "using audio feature of valence score", 
       caption = "Data Source:Spotify Top50 Chart by Country") 
```
![_config.yml]({{ site.baseurl }}/images/valence_Map.png)

![_config.yml]({{ site.baseurl }}/images/pressure-1.png)
