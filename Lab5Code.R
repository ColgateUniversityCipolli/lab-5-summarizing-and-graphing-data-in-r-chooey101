library(tidyverse)

allentown_data <- read_csv("data/essentia.data.allentown.csv")
song_data <- read_csv("data/essentia.data.csv") 
song_stats <- function(feature){
song_data|>
group_by(artist)|>
  summarize( Min = min(get(feature), na.rm = TRUE),
             Q1 = quantile(get(feature), 0.25, na.rm = TRUE),
             Q3 = quantile(get(feature), 0.75, na.rm = TRUE),
             Max = max(get(feature), na.rm = TRUE)) |>
  mutate( 
    IQR = Q3 - Q1,
    Lower_Fence = Q1 - 1.5 * IQR,
    Upper_Fence = Q3 + 1.5 * IQR 
    ) |>
  mutate(
    out.of.range = if_else((allentown_data$overall_loudness < Min | allentown_data$overall_loudness > Max), 
                          TRUE, FALSE),
    unusual = if_else((allentown_data$overall_loudness < Lower_Fence | allentown_data$overall_loudness > Upper_Fence), 
                     TRUE, FALSE),
    description = case_when(
      out.of.range ~ "Out of Range",
      unusual ~ "Outlying",
      TRUE ~ "Within Range"

  )
  )
}

song_stats("spectral_energy")
  
