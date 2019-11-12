# Building a College Football Win Probability Model

get_conference_plays_url = function(season_type, season, offensive_conference) {
  
  base_url = "api.collegefootballdata.com/plays?"
  api_conditions = paste("seasonType=", season_type, "&year=", season, "&offenseConference=", offensive_conference, sep = "")
  
  api_url = paste(base_url, api_conditions, sep = "")
  return(api_url)
  
}

import_from_college_api = function(url) {
  
  from_api = GET(url) %>%
              content(as = "text") %>%
              fromJSON(flatten = TRUE)
  
  return(from_api)
  
}

# Get play by play data for all SEC teams
sec_plays_2014 = import_from_college_api(get_conference_plays_url("regular", 2014, "SEC")) %>% mutate(season = 2014)
sec_plays_2015 = import_from_college_api(get_conference_plays_url("regular", 2015, "SEC")) %>% mutate(season = 2015)
sec_plays_2016 = import_from_college_api(get_conference_plays_url("regular", 2016, "SEC")) %>% mutate(season = 2016)
sec_plays_2017 = import_from_college_api(get_conference_plays_url("regular", 2017, "SEC")) %>% mutate(season = 2017)
sec_plays_2018 = import_from_college_api(get_conference_plays_url("regular", 2018, "SEC")) %>% mutate(season = 2018)
sec_plays_2019 = import_from_college_api(get_conference_plays_url("regular", 2019, "SEC")) %>% mutate(season = 2019)

all_sec_plays = bind_rows(sec_plays_2014, sec_plays_2015, sec_plays_2016, sec_plays_2017, sec_plays_2018, sec_plays_2019) %>%
                  mutate(game_id = substr(id, start = 1, stop = 8),
                         raw_play_type = case_when(str_detect(play_type, regex("field", ignore_case = TRUE)) ~ "field_goal",
                                                   str_detect(play_type, regex("punt", ignore_case = TRUE)) ~ "punt",
                                                   str_detect(play_type, regex("pass", ignore_case = TRUE)) ~ "pass",
                                                   str_detect(play_type, regex("rush", ignore_case = TRUE)) ~ "rush",
                                                   str_detect(play_type, regex("kickoff", ignore_case = TRUE)) ~ "kickoff",
                                                   play_type == "Sack" ~ "pass",
                                                   TRUE ~ "Other"),
                         quarter_seconds_remaining = clock.minutes * 60 + clock.seconds,
                         half_seconds_remaining = ifelse(period %in% c(1,3), 900 + quarter_seconds_remaining, quarter_seconds_remaining),
                         score_differential = offense_score - defense_score) %>%
                          arrange(id, period, desc(clock.minutes), desc(clock.seconds)) %>%
                  group_by(game_id) %>% 
                    mutate(left_to_right = case_when(raw_play_type == "kickoff" & yard_line == 35 ~ TRUE,
                                                     raw_play_type == "kickoff" & yard_line == 65 ~ FALSE,
                                                     TRUE ~ NA),
                           left_to_right_final = ifelse(sum(left_to_right == TRUE) > 0, TRUE, FALSE),
                           yardline_100 = ifelse(left_to_right == TRUE, 100 - yard_line, yard_line)) 
                  
                  

play_prediction_features = c("season", "period", "half_seconds_remaining", "yardline_100", "down", "distance", "score_differential", "raw_play_type")

play_prediction = all_sec_plays %>% 
                    filter(raw_play_type %in% c("field_goal", "pass", "punt", "rush")) %>%
                    select(play_prediction_features) %>%
                    mutate(play_codes = as.numeric(as.factor(raw_play_type)) - 1)

play_code_id = play_prediction %>% 
                select(raw_play_type, play_codes) %>% 
                unique() %>%
                arrange(play_codes)

play_prediction_train_x = play_prediction %>% filter(season <= 2017) %>% select(-raw_play_type, -play_codes) %>% as.matrix()
play_prediction_train_y = play_prediction %>% filter(season <= 2017) %>% select(play_codes) %>% as.matrix()

play_prediction_test_x = play_prediction %>% filter(season == 2018) %>% select(-raw_play_type, -play_codes) %>% as.matrix()
play_prediction_test_y = play_prediction %>% filter(season == 2018) %>% select(play_codes) %>% as.matrix()

train_dmat = xgb.DMatrix(data = play_prediction_train_x, label = play_prediction_train_y)
test_dmat = xgb.DMatrix(data = play_prediction_test_x, label = play_prediction_test_y)

param = list(max_depth = 5,
              eta = 0.15,
              objective = "multi:softprob",
              eval_metric = "mlogloss",
              num_class = nrow(play_code_id))

play_prediction_model = xgb.train(params = param,
                                  data = train_dmat,
                                  early_stopping_rounds = 10,
                                  nrounds = 1000,
                                  watchlist = list(train = train_dmat, val = test_dmat))

test_mat = data.frame(season = 2019,
                      period = 4,
                      half_seconds_remaining = 120,
                      yardline_100 = 35,
                      down = 4,
                      distance = 10,
                      score_differential = -10) %>% 
            as.matrix()

# Probabilities are "field_goal", "pass", "punt", "rush"
predict(play_prediction_model, test_mat)



## Test using 2019
xgb_test_2019 = all_sec_plays %>% 
                  filter(season == 2019, raw_play_type %in% c("field_goal", "pass", "punt", "rush")) %>%
                  select(play_prediction_features, -raw_play_type) %>%
                  as.matrix()

predictions = matrix(predict(play_prediction_model, xgb_test_2019), ncol = 4, byrow = TRUE, 
                     dimnames = list(x = NULL, y = c("field_goal", "pass", "punt", "rush"))) %>%
                as.data.frame() %>%
                mutate(prediction = case_when(field_goal > pass & field_goal > punt & field_goal > rush ~ "field_goal",
                                              pass > punt & pass > rush & pass > field_goal ~ "pass",
                                              punt > rush & punt > field_goal & punt > pass ~ "punt",
                                              rush > field_goal & rush > pass & rush > punt ~ "rush"))
                

predictions_and_results = bind_cols(all_sec_plays %>% filter(season == 2019, raw_play_type %in% c("field_goal", "pass", "punt", "rush")), predictions)

