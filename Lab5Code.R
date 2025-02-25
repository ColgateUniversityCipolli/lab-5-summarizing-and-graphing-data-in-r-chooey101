library(tidyverse)

#########Creation of Statistical Data########

allentown_data <- read_csv("data/essentia.data.allentown.csv")
song_data <- read_csv("data/essentia.data.csv") 
song_stats <- function(feature){ #Takes an essentia feature as input 
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
    description = case_when(          #Tells us if the band is in range or unusual
      out.of.range ~ "Out of Range",
      unusual ~ "Outlying",
      TRUE ~ "Within Range"
  )
  ) |>
  ungroup()
}

#######Data Summarization#########

numerical_features <- song_data |>
  select(where(is.numeric)) |> #Pulls only numerical features
  names() #makes sure this is the character value of each feature
all_features_stats <- tibble()
for (feature in numerical_features) {
  all_features_stats <- bind_rows(all_features_stats, song_stats(feature)) #Statistics of all numerical features
}
##########Part 3--Create Table for Data###########

summarized_features <- all_features_stats |>
filter(str_detect(feature, "spectral_skewness|spectral_rolloff|spectral_energyband_middle_high|spectral_complexity|spectral_centroid|melbands_spread|melbands_flatness_db|erbbands_skewness|erbbands_flatness_db|dissonance"))
#Above I hadd to include all features on one line b/c spacing them vertically broke my code
table_data <- summarized_features |>
select(artist, feature, description)#Only include values that are easy to interpret
table_final <- xtable(table_data)
print(table_final)
#######Eventually move this data to Sweave#########
##write_csv(summarized_features, "summarized_features.csv") #Write csv file to use in shiny app

#########STEP 4--Create Plots for Data##########

#Chose to go with a barplot
#Inserted code from ggplot which generates bar plot representing the relative frequency of the amt of times each artist is in or out of range

library(tidyverse)
####################################
# Load Data
####################################
dat <- read_csv("summarized_features.csv")
####################################
# Mutate data for plot
####################################
df <- dat %>%
  dplyr::select("artist", "description") %>%
  drop_na() %>%
  group_by(!!sym("artist"), !!sym("description")) %>%
  summarise(Observations = sum(!is.na(!!sym("artist"))), .groups = "drop") %>%
  tidyr::complete(!!sym("artist"), !!sym("description")) %>%
  replace_na(list(Observations = 0)) %>%
  group_by(!!sym("description")) %>%
  mutate(Proportion = Observations / sum(Observations)) %>%
  arrange(desc(!!sym("artist"))) %>%
  mutate(Percent = Proportion * 100) %>%
  mutate(denoted.group = paste("description", " = ", !!sym("description"), sep = ""))
####################################
# Create Plot
####################################
p <- ggplot(df, aes(x = !!sym("artist"), y = Proportion)) +
  geom_bar(stat = "identity", width = 0.5, fill = "lightblue") +
  get("theme_bw")() +
  xlab("Artist") +
  ylab(ifelse("Frequency" == "", "Proportion", "Frequency")) +
  ggtitle("Relative Frequency of Bands Within Range of \"Allentown\"", "") +
  geom_hline(yintercept = 0) +
  facet_wrap(~denoted.group)
####################################
# Print Plot
####################################
p
####################################
# Summarize Data
####################################
dat.summary <- dat %>%
  dplyr::select("description", "artist") %>%
  group_by(!!sym("description"), !!sym("artist")) %>%
  summarize(Observations = sum(!is.na(!!sym("artist"))), .groups = "drop") %>%
  tidyr::complete(!!sym("description"), !!sym("artist")) %>%
  replace_na(list(Observations = 0))
missing.obs <- dat %>%
  summarize(missing = sum(is.na(!!sym("artist")) | is.na(!!sym("description")))) %>%
  pull(missing)
dat.summary <- dat.summary %>%
  filter(!(is.na(!!sym("artist")) | is.na(!!sym("description")))) %>%
  group_by(!!sym("description")) %>%
  mutate(Proportion = Observations / sum(Observations)) %>%
  arrange(desc(!!sym("description"))) %>%
  arrange(!!sym("artist"), .by_group = TRUE) %>%
  mutate(Percent = Proportion * 100) %>%
  mutate_if(is.numeric, round, 4) %>%
  ungroup() %>%
  add_row(`:=`(!!sym("description"), "Rows with Missing Data"), `:=`(!!sym("artist"), NA), Observations = missing.obs, Proportion = NA, Percent = NA)
####################################
# Print Data Summary
####################################
dat.summary

