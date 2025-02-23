library(tidyverse)

allentown_data <- read_csv("data/essentia.data.allentown.csv")
song_data <- read_csv("data/essentia.data.csv") 
song_stats <- function(feature){
allentown_value <- allentown_data[[feature]]
song_data|>
group_by(artist)|>
  summarize( Min = min(get(feature), na.rm = TRUE),
             Q1 = quantile(get(feature), 0.25, na.rm = TRUE),
             Q3 = quantile(get(feature), 0.75, na.rm = TRUE),
             Max = max(get(feature), na.rm = TRUE)) |>
  mutate( 
    feature = feature,
    IQR = Q3 - Q1,
    Lower_Fence = Q1 - 1.5 * IQR,
    Upper_Fence = Q3 + 1.5 * IQR 
    ) |>
  mutate(
    out.of.range = if_else((allentown_value < Min | allentown_value > Max), 
                          TRUE, FALSE),
    unusual = if_else((allentown_value < Lower_Fence | allentown_value > Upper_Fence), 
                     TRUE, FALSE),
    description = case_when(
      out.of.range ~ "Out of Range",
      unusual ~ "Outlying",
      TRUE ~ "Within Range"

  )
  ) |>
  ungroup()

}
numerical_features <- song_data |>
  select(where(is.numeric)) |>
  names() #makes sure this is the character value of each feature
all_features_stats <- tibble()
for (feature in numerical_features) {
  all_features_stats <- bind_rows(all_features_stats, song_stats(feature))
}

  
