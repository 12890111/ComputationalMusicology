library(ggplot2)
library(tidyverse)
library(spotifyr)
library(compmus)

Sys.setenv(SPOTIFY_CLIENT_ID = 'bc10a0aeb17b470c8f8e96c0113af186')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b430deb2e5f3496aab656f91ab7a6ff9')

access_token <- get_spotify_access_token()

Top_50_US <- get_playlist_audio_features("", "5HPHhT6kRtFGjtjA5J3lqW")
Top_50_NL <- get_playlist_audio_features("", "0jHMZUikBLKj4Qv6ZjIo5L")
Top_100_USNL <- rbind(Top_50_US, Top_50_NL)

Top_100_USNL$key <- Top_100_USNL$key + 1
Top_100_USNL <- Top_100_USNL %>% rename(country = playlist_name)
Top_100_USNL$country[Top_100_USNL$country == 'Corpus Top 50 US'] <- c('US')
Top_100_USNL$country[Top_100_USNL$country == 'Corpus Top 50 NL'] <- c('NL')

x_range <- c('C', 'C#', "D", 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B')

USvsNL <- ggplot(Top_100_USNL, aes(x = key, fill = country)) +
  geom_bar(position = position_dodge(width = 0.5), alpha = (0.8))+ 
  labs(title = "The distibution of Key's Americans and the Dutch listen to", x = 'Key', y = 'Frequency', fill = 'Nationality')+
  xlim(x_range)

MasQueNada <-
  get_tidy_audio_analysis("0Vl9aGb0dmeiCQ2ATgNK2B") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

MasQueNada_Chroma <- MasQueNada |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value)) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

Corcovado <-
  get_tidy_audio_analysis("3ZhOHtPZ1JhDVFz4YaFqBn") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

Corcovado_Chroma <- Corcovado |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

SoDancoSamba <-
  get_tidy_audio_analysis("5je0CDv1Z1hT82Nek1Fjhl") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

SoDancoSamba_Chroma <- SoDancoSamba |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

FarMoreBlue <-
  get_tidy_audio_analysis("2djpFaL4yqTH3fvDKijsMv") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

FarMoreBlue_Chroma <- FarMoreBlue |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()


TooCloseForComfort <-
  get_tidy_audio_analysis("0ZU39CemSAamm1dtiSAchp") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

TooCloseForComfort_Chroma <- TooCloseForComfort |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

WithALittleBitOfLuck <-
  get_tidy_audio_analysis("2sYInNWcg7ZL9cLobvs0oo") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

WithALittleBitOfLuck_Chroma <- WithALittleBitOfLuck |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

saveRDS(Corcovado, 'Data/Corcovado.RDS')

saveRDS(MasQueNada_Chroma, 'Data/MasQueNada_Chroma.RDS')
saveRDS(Corcovado_Chroma, 'Data/Corcovado_Chroma.RDS')
saveRDS(SoDancoSamba_Chroma, 'Data/SoDancoSamba_Chroma.RDS')

saveRDS(FarMoreBlue_Chroma, 'Data/FarMoreBlue_Chroma.RDS')
saveRDS(TooCloseForComfort_Chroma, 'Data/TTCFC_Chroma.RDS')
saveRDS(WithALittleBitOfLuck_Chroma , 'Data/WALBOL_Chroma .RDS')


This_Is_ArrDee <- get_playlist_audio_features("", "37i9dQZF1DZ06evO4lUn2o")
This_Is_Digga_D <- get_playlist_audio_features("", "5xe6lc2UVJFDZ6RYOaTciq")
This_Is_Central_C <- get_playlist_audio_features("", "2ezSblsEEK0920wrIVXpfZ")

Drillers <- rbind(This_Is_Digga_D, This_Is_ArrDee, This_Is_Central_C)
Drillers <- Drillers %>% drop_na(playlist_id)
Drillers <- Drillers %>% rename(
  Artist = playlist_id
)

Drillers$Artist[Drillers$Artist == 'This is Digga D (2)'] <- 'Digga D'
Drillers$Artist[Drillers$Artist == 'This Is ArrDee'] <- 'ArrDee'
Drillers$Artist[Drillers$Artist == 'This is Central Cee (2)'] <- 'Central C'


Val_Enrg_Drill_Plt <- ggplot(Drillers, (aes(x = valence, y =  energy, color = Artist)))+
  geom_point(aes(alpha = track.popularity))+
  scale_alpha(range = c(0.2, 1.0))+
  theme_minimal()



Avg_Drillers <- Drillers %>% group_by(Artist)%>%summarise(Avg_Enrg = mean(energy), 
      Avg_Val = mean(valence), Avg_Pop = mean(track.popularity), Avg_Danc = mean(danceability), 
      Avg_Loud = mean(loudness), Avg_speechiness = mean(speechiness), Avg_Acous = mean(acousticness), 
      Avg_Instr = mean(instrumentalness), Avg_Lively = mean(liveness), Avg_tempo = mean(tempo))


Avg_Drillers_Plot <- ggplot(Avg_Drillers)+
  geom_point(aes(x = Avg_Val, y = Avg_Enrg, color = Artist, size =     (Avg_Pop), alpha = 0.75))+
  xlim(0,1)+
  ylim(0,1)+
  theme_bw()+
  scale_size_continuous(Avg_Drillers$Avg_Pop, range = c(10, 20))+
  geom_point(aes(x = Avg_Val, y = Avg_Enrg, color = Artist))

saveRDS(Avg_Drillers_Plot, 'Data/Avg_Drillers_Plot.RDS')
saveRDS(Val_Enrg_Drill_Plt, 'Data/Val_Enrg_Drill_Plt.RDS')
saveRDS(MasQueNada_chgram, 'Data/MasQueNada_Chordogram.RDS')
saveRDS(USvsNL, 'Data/USvsNL.RDS')




