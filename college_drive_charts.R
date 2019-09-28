
# Code to produce drive charts for a particular college football game 

# Load packages
library(tidyverse)
library(reshape2)
library(httr)
library(jsonlite)

# Function to format data from API nicely as data frame
import_from_college_api = function(url) {
  
  from_api = GET(url) %>%
              content(as = "text") %>%
              fromJSON(flatten = TRUE)
  
  return(from_api)
  
}

# Inputs for plots
season_type = "regular"
game_season = 2019
game_week = 2
offense_team = "LSU"
defense_team = "Texas"

# Stitch together the URL's to pull the data from
api_conditions = paste("?seasonType=", season_type, "&year=", game_season, "&week=", game_week, "&offense=", offense_team, "&defense=", defense_team, sep = "")
drives_url = paste("api.collegefootballdata.com/drives", api_conditions, sep = "")
plays_url = paste("api.collegefootballdata.com/plays/", api_conditions, sep = "")

# Import data from api.collegefootballdata.com
drives_info = import_from_college_api(drives_url)
plays_info = import_from_college_api(plays_url)

# Convert NA's to 0 (these are created where the time is 0) 
drives_info[is.na(drives_info)] = 0
drives_info[drives_info == "Uncategorized"] = "KNEEL" 

# Build features for plotting
drives_df = drives_info %>% 
                mutate(quarter_seconds_remaining_start = start_time.minutes * 60 + start_time.seconds,
                       quarter_seconds_remaining_end = end_time.minutes * 60 + end_time.seconds,
                       drive_elapsed_seconds = elapsed.minutes * 60 + elapsed.seconds,
                       quarter_seconds_start = as.character(start_time.seconds),
                       new_quarter_seconds_start = ifelse(nchar(quarter_seconds_start) == 1, paste("0", quarter_seconds_start, sep = ""), quarter_seconds_start),
                       quarter_seconds_elapsed = as.character(elapsed.seconds),
                       new_quarter_seconds_elapsed = ifelse(nchar(quarter_seconds_elapsed) == 1, paste("0", quarter_seconds_elapsed, sep = ""), quarter_seconds_elapsed),
                       drive_start_time = paste(start_time.minutes, new_quarter_seconds_start, sep = ":"),
                       drive_elapsed_time = paste(elapsed.minutes, new_quarter_seconds_elapsed, sep = ":"),
                       drive_result = as.character(drive_result),
                       start_yardline = 100 - start_yardline,
                       end_yardline = 100 - end_yardline) %>%
                filter(plays != 0) %>%
                mutate(drive_number = nrow(drives_info) - row_number())

# Get the score differences at the time of each drive from the play by play
plays_df = plays_info %>% 
                arrange(drive_id, period, desc(clock.minutes), desc(clock.seconds)) %>%
                mutate(score_difference = offense_score - defense_score,
                       score_teams = paste(offense_team, ": ", offense_score,  ", ", defense_team, ": ", defense_score, ".", sep = ""),
                       score_only = paste(offense_score, defense_score, sep = "-"))

drive_scores = plays_df %>% 
                group_by(drive_id) %>% 
                filter(row_number() == 1) %>% 
                select(drive_id, score_only)

# Merge drive and plays data frames together for the plot
drive_chart_plot_df = drives_df %>%
                        left_join(drive_scores, by = c("id" = "drive_id")) %>% 
                        select(id, drive_number, start_period, drive_start_time,  start_yardline, end_yardline, drive_elapsed_time, plays, yards, drive_result, score_only) %>%
                        melt(id = c("id", "drive_number", "start_period", "drive_start_time", "drive_elapsed_time", "plays", "yards", "drive_result", "score_only")) %>%
                        mutate(row_name = paste("Q", start_period, ", Time ", drive_start_time, ", Score ", score_only, sep = ""),
                               end_result = drive_result,
                               end_label = case_when(variable == "start_yardline" ~ "",
                                                     variable == "end_yardline" ~ paste(end_result, " (", plays, " plays, ", drive_elapsed_time, ")", sep = "")))

# Create wrapper for the theme
drive_chart_theme = function() {
  # Theme
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(5, 10, 5, 10), units = "mm"),
        panel.background = element_blank(),
  ) 
}

endzone_colour = "#CC5500"
grass_colour = "#93DB70"

# Create plot
ggplot(drive_chart_plot_df, aes(x = value, y = drive_number, group = drive_number)) +
  
  # Cut the grass and paint the end zone
  geom_rect(mapping = aes(xmin = -10, xmax = 0, ymin = 13, ymax = -2), fill = endzone_colour) + 
  geom_rect(mapping = aes(xmin = 0, xmax = 100, ymin = 13, ymax = -2), fill = grass_colour) +
  geom_rect(mapping = aes(xmin = 100, xmax = 110, ymin = 13, ymax = -2), fill = endzone_colour) + 

  # Paint yardlines
  geom_vline(xintercept = 0, linetype = "solid", colour = "white", size = 1) + 
  geom_vline(xintercept = 10, linetype = "solid", colour = "white", size = 1) +
  geom_vline(xintercept = 20, linetype = "solid", colour = "white", size = 1) + 
  geom_vline(xintercept = 30, linetype = "solid", colour = "white", size = 1) +
  geom_vline(xintercept = 40, linetype = "solid", colour = "white", size = 1) +
  geom_vline(xintercept = 50, linetype = "solid", colour = "white", size = 1) +
  geom_vline(xintercept = 60, linetype = "solid", colour = "white", size = 1) +
  geom_vline(xintercept = 70, linetype = "solid", colour = "white", size = 1) +
  geom_vline(xintercept = 80, linetype = "solid", colour = "white", size = 1) +
  geom_vline(xintercept = 90, linetype = "solid", colour = "white", size = 1) +
  geom_vline(xintercept = 100, linetype = "solid", colour = "white", size = 1) +
  
  # Place pylons in the corners of the end zone
  geom_point(x = -10, y = -2, shape = 22, colour = "#FFD700", fill = "#FFD700") + 
  geom_point(x = -10, y = 13, shape = 22, colour = "#FFD700", fill = "#FFD700") + 
  geom_point(x = 0, y = -2, shape = 22, colour = "#FFD700", fill = "#FFD700") + 
  geom_point(x = 0, y = 13, shape = 22, colour = "#FFD700", fill = "#FFD700") + 
  geom_point(x = 110, y = -2, shape = 22, colour = "#FFD700", fill = "#FFD700") + 
  geom_point(x = 110, y = 13, shape = 22, colour = "#FFD700", fill = "#FFD700") + 
  geom_point(x = 100, y = -2, shape = 22, colour = "#FFD700", fill = "#FFD700") + 
  geom_point(x = 100, y = 13, shape = 22, colour = "#FFD700", fill = "#FFD700") + 
  
  # Paint the field numbers
  geom_text(label = "2", x = 18.5, y = -1, colour = "white", size = 4) + 
  geom_text(label = "0", x = 22, y = -1, colour = "white", size = 4) + 
  geom_text(label = "4", x = 38.5, y = -1, colour = "white", size = 4) + 
  geom_text(label = "0", x = 42, y = -1, colour = "white", size = 4) + 
  geom_text(label = "4", x = 58.5, y = -1, colour = "white", size = 4) + 
  geom_text(label = "0", x = 62, y = -1, colour = "white", size = 4) + 
  geom_text(label = "2", x = 78.5, y = -1, colour = "white", size = 4) + 
  geom_text(label = "0", x = 82, y = -1, colour = "white", size = 4) + 
  
  geom_text(x = 16, y = -1, label = "▶", size = 2.5, family = "HiraKakuPro-W3", colour = "white", angle = 180)  + 
  geom_text(x = 36, y = -1, label = "▶", size = 2.5, family = "HiraKakuPro-W3", colour = "white", angle = 180)  + 
  geom_text(x = 64.5, y = -1, label = "▶", size = 2.5, family = "HiraKakuPro-W3", colour = "white")  + 
  geom_text(x = 84.5, y = -1, label = "▶", size = 2.5, family = "HiraKakuPro-W3", colour = "white")  + 
  
  # Add the points
  drive_chart_theme() + 
  xlim(c(-10, 140)) + 
  geom_point() + 
  geom_path(aes(), arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  scale_y_continuous(labels = unique(rev(drive_chart_plot_df$row_name)), breaks = 1:(nrow(drive_chart_plot_df)/2)) + 
  geom_text(aes(label = end_label), hjust = -0.1, vjust = -0.4, size = 3, fontface = "bold") + 
  
  # Add title and axes labels
  labs(title = "Geauxing Up Tempeaux", 
       subtitle = "LSU Offensive Drives vs Texas",
       x = "Data courtesy of api.collegefootballdata.com. By @ianasta23")

