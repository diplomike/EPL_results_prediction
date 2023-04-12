library(tidyverse)
library(readxl)
library(caret)
library(randomForest)
options(timeout = 120, digits=4, max.print=1000, width = 85, warn = 0)

# This project is to predict the correct scores of the remaining matches of the English Premier League for the 2022-23 Season as of 10 Apr 2023.

# The original database is obtained via https://www.kaggle.com/datasets/irkaal/english-premier-league-results, with results up to 10 Apr 2022.
# Additional scores were manually entered by referring to the EPL official website and uploaded as xlsx file to the github link below. 

# Download the file from github
if(!file.exists("EPL_results.xlsx")) download.file(
  "https://github.com/diplomike/EPL_results_prediction/raw/9580aadcef53662dc7c54e6a9025b53504644b0a/EPL_results_2023-04-10.xlsx", "EPL_results.xlsx", mode="wb")

# Load the Excel file into the database and inspect its content
read_xlsx("EPL_results.xlsx")

# Prune the data frame variables
epl_results <- read_xlsx("EPL_results.xlsx") %>% select(Season, DateTime, HomeTeam, AwayTeam, FTHG, FTAG) %>% mutate(DateTime=date(DateTime))

# Check the number of matches played
epl_results %>% group_by(Season) %>% summarize(matches=n()) %>% print(n = 30)

# Most up-to-date league table
epl_results %>% filter(Season=="2022-23") %>% select(HomeTeam, AwayTeam, FTHG, FTAG) %>%  
  pivot_longer(HomeTeam:AwayTeam, names_to= "venue", values_to = "Team") %>%
  mutate(Won = ifelse((FTHG - FTAG) * (venue =="HomeTeam") + (FTAG - FTHG) * (venue =="AwayTeam")>0,1,0), 
         Drawn = ifelse(FTHG == FTAG, 1, 0),
         Lost = ifelse((FTHG - FTAG) * (venue =="HomeTeam") + (FTAG - FTHG) * (venue =="AwayTeam")<0,1,0),
         GF = ifelse(venue=="HomeTeam", FTHG, FTAG), 
         GA = ifelse(venue=="AwayTeam", FTHG, FTAG),
         GD = GF-GA, Pts = Won * 3 + Drawn) %>% 
  group_by(Team) %>% summarize(Played = n(), Won=sum(Won), Drawn=sum(Drawn), Lost=sum(Lost), GF=sum(GF),GA=sum(GA), GD=sum(GD), Pts=sum(Pts)) %>%
  arrange(desc(Pts), desc(GD),desc(GF))

# Home and away goal distributions
epl_results %>% mutate(HomeGoals= FTHG, AwayGoals = FTAG) %>% 
  pivot_longer(HomeGoals:AwayGoals, values_to = "Goals", names_to = "Type") %>% 
  ggplot(aes(Goals, fill = Type)) + geom_bar(position = "dodge") + 
  scale_x_discrete(breaks = 0:8,limits=(0:8))

# Home away goal difference across seasons
epl_results %>% group_by(Season) %>% 
  summarize("Avg Home Goals" = mean(FTHG), "Avg Away Goals"=mean(FTAG), 
            "Avg Goal Diff" = mean(FTHG)- mean(FTAG)) %>% 
  pivot_longer("Avg Home Goals":"Avg Goal Diff", names_to = "type", values_to = "score") %>%
  ggplot(aes(Season, score, color=type)) + geom_point() + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + ylim(0,2)

# Filter old and abnormal seasons, assign corresponding seasons for training and testing data
epl_results <- epl_results %>% filter(Season >="2000-01" & Season != "2020-21")
TrainSeasons <- filter(epl_results, Season!="2022-23") %>% group_by(Season) %>% summarize %>% pull(Season)
TestSeason <- "2022-23"

# Train set home and away mean scores
epl_results %>% filter(Season %in% TrainSeasons) %>% 
  summarize (AvgHomeGoal = mean(FTHG), AvgAwayGoal = mean(FTAG))

# Train set home and away median scores
epl_results %>% filter(Season %in% TrainSeasons) %>% 
  summarize (MedianHomeGoal = median(FTHG), MedianAwayGoal = median(FTAG))

# Home Goals distribution
epl_results %>% filter(Season %in% TrainSeasons) %>% .$FTHG %>% table

# Aawy Goals distribution
epl_results %>% filter(Season %in% TrainSeasons) %>% .$FTAG %>% table

# Train set home and away score standard deviations
epl_results %>% filter(Season %in% TrainSeasons) %>% 
  summarize (HomeGoalSD = sd(FTHG), AwayGoalSD = sd(FTAG))

# Naive prediction accuracy
(epl_results %>% filter(Season == TestSeason) %>% 
    rename (HomeGoalAccurary=FTHG, AwayGoalAccurary=FTAG) %>% 
    select(HomeGoalAccurary, AwayGoalAccurary) == 1) %>% colMeans %>% print %>% prod

# Naive prediction RMSE
(epl_results %>% filter(Season == TestSeason) %>% 
    rename (HomeGoalRMSE=FTHG, AwayGoalRMSE=FTAG) %>% 
    select(HomeGoalRMSE, AwayGoalRMSE) - 1)^2 %>% 
  colMeans(na.rm=T) %>% sqrt %>% print %>% sum
# ___________________________________________________________________________

# number of recent matches to include under the same home/away condition
n=20

# H = Home team (1st place) /home match (2nd place)
# A = Away team (1st place) /away match (2nd place)
# G = Goals scored
# g = Goals conceded
# X = head-to-head encounters
# Number suffix = order of previous matches under the same conditions


# Past results of the teams under the same home/away condition
RcntHA <- epl_results %>% 
  rename(TmpSeason = Season, MatchDate=DateTime, TmpAwayTeam=AwayTeam, 
         HG=FTHG, AG=FTAG) %>% # Change the conflicting colnames
  inner_join(epl_results, c("HomeTeam")) %>% filter(MatchDate>DateTime) %>% 
  group_by(HomeTeam, MatchDate) %>% slice_max(DateTime, n=n) %>% 
  mutate(Match=str_pad(rank(-rank(DateTime)), 2, pad=0)) %>% ungroup %>%
  select(-Season, -DateTime,-AwayTeam) %>% # Create home team home match results
  rename(AwayTeam = TmpAwayTeam, TmpHomeTeam = HomeTeam, HHG = FTHG, HHg = FTAG) %>% 
  pivot_wider(names_from = Match, names_sep = "", values_from = c(HHG, HHg)) %>%
  inner_join(epl_results, c("AwayTeam")) %>% filter(MatchDate>DateTime) %>% 
  group_by(AwayTeam, MatchDate) %>% slice_max(DateTime, n=n) %>% 
  mutate(Match=str_pad(rank(-rank(DateTime)), 2, pad=0)) %>% ungroup %>% 
  select(-Season, -DateTime, -HomeTeam) %>% # Create away team away match results
  rename(AAG = FTAG, AAg = FTHG) %>%
  pivot_wider(names_from = Match, names_sep = "", values_from = c(AAG, AAg)) %>%
  rename_at(vars(starts_with("Tmp")),~str_replace(.,"Tmp","")) %>% # Restore the conflicting colnames
  arrange(Season, HomeTeam, MatchDate)

# Determine the maximum size of the RcntHA dataset for cross-validation
set_sze <- RcntHA %>% filter(Season %in% TrainSeasons)%>% select(matches("HHG|AAG")) %>% 
  is.na %>% rowSums %>% {.==0} %>% sum

# Create linear models from RcntHA and compute RMSE
RcntHA_RMSE <- function(i) rowMeans( # average the RMSE 
  sapply (1:100, function (repetitions) { # Perform Monte Carlo simulations
    set_idx <- RcntHA %>% filter(Season %in% TrainSeasons) %>%    
      select(matches(paste0("(HHG|AAG)",str_pad(1:i, 2, pad=0)))) %>% 
      is.na %>% rowSums %>% {which(.==0)} %>% sample(set_sze) # Exclude rows with missing data
    val_idx <- sample(set_idx, set_sze * 0.1) # Create the validation set 
    train_idx <- setdiff(set_idx, val_idx) # Create the training set
    linear_model <- lm(cbind(HG,AG) ~ ., data = RcntHA %>% # Build the linear model 
                         filter(Season %in% TrainSeasons) %>% .[train_idx,] %>%
                         select(matches(paste0("(HHG|AAG)", str_pad(1:i, 2, pad=0))), HG, AG)) 
    (predict(linear_model, RcntHA %>% filter(Season %in% TrainSeasons) %>% .[val_idx,]) 
      - RcntHA %>% filter(Season %in% TrainSeasons) %>% .[val_idx,] %>% select(HG,AG)) ^2 %>% 
      colMeans %>%sqrt})) # Predict scores with the model & compute its RMSE 

# Pick the optimal number of past matches from RcntHA
sapply(1:n, RcntHA_RMSE) %>% as.data.frame() %>% rename_all(~paste(1:n, "match(es)")) %>%
  t %>% as.data.frame() %>% mutate(Total = HG + AG) 

# Create linear regression model from RcntHA
linear_model <- lm(cbind(HG,AG) ~ ., data = RcntHA %>% filter(Season %in% TrainSeasons) %>%
                     select(matches(paste0("(HHG|AAG)",str_pad(1:17, 2, pad=0))), HG, AG))

# Evaluate RMSE from RcntHA with the test set
(predict(linear_model, RcntHA %>% filter(Season==TestSeason)) 
  - RcntHA %>% filter(Season==TestSeason) %>% select(HG,AG))^2 %>%
  colMeans(na.rm = T) %>% sqrt %>% print %>% sum

# ___________________________________________________________________________

# Include counter home/away of past match results for both teams
RcntHAAH <- RcntHA %>%
  rename_all(~paste0("Tmp",.)) %>%  # Change the conflicting colnames
  inner_join(RcntHA, c("TmpHomeTeam"="AwayTeam")) %>% filter(TmpMatchDate>MatchDate) %>% 
  group_by(TmpHomeTeam, TmpMatchDate) %>% slice_max(MatchDate) %>% ungroup %>%
  rename(HAG01 = AG, HAg01 = HG) %>% select(matches("Tmp|HA|AA")) %>%
  rename_at(vars(starts_with("AA")), # Create home team away match results
            ~paste0("HA", str_sub(.,3,3), str_pad(as.numeric(str_sub(.,4))+1, 2, pad=0))) %>%
  inner_join(RcntHA, c("TmpAwayTeam"="HomeTeam")) %>% filter(TmpMatchDate>MatchDate) %>% 
  group_by(TmpAwayTeam, TmpMatchDate) %>% slice_max(MatchDate) %>% ungroup %>%
  rename(AHG01 = HG, AHg01 = AG) %>% select(matches("Tmp|HA|AH|HH")) %>%
  rename_at(vars(starts_with("HH")), # Create away team home match results
            ~paste0("AH", str_sub(.,3,3), str_pad(as.numeric(str_sub(.,4))+1, 2, pad=0))) %>%
  rename_at(vars(starts_with("Tmp")),~str_replace(.,"Tmp","")) %>% # Restore the conflicting colnames
  select(Season, MatchDate, HomeTeam, AwayTeam, HG, AG, matches("HH|AA|HA|AH")) %>%
  arrange(HomeTeam, MatchDate) # Sort the variables and records

# Determine the maximum size of the RcntHAAH dataset for cross-validation 
set_sze <- RcntHAAH %>%filter(Season %in% TrainSeasons)%>% select(matches("HHG|AAG|HAG|AHG")) %>% 
  is.na %>% rowSums %>% {.==0} %>% sum

# Create linear models from RcntHAAH and compute its RMSE
RcntHAAH_RMSE <- function(i) rowMeans( # average the RMSE 
  sapply (1:100, function (repetitions) { # Perform Monte Carlo simulations
    set_idx <- RcntHAAH %>% filter(Season %in% TrainSeasons) %>%
      select(matches(paste0("(HHG|AAG)",str_pad(1:17, 2, pad=0))),
             matches(paste0("(HAG|AHG)",str_pad(1:i, 2, pad=0)))) %>%
      is.na %>% rowSums %>% {which(.==0)} %>% sample(set_sze) # Exclude rows with missing data
    val_idx <- sample(set_idx, set_sze * 0.1) # Create the validation set 
    train_idx <- setdiff(set_idx, val_idx) # Create the training set
    linear_model <- lm(cbind(HG,AG) ~ ., data = RcntHAAH %>% # Build linear model
                         filter(Season %in% TrainSeasons) %>% .[train_idx,] %>%
                         select(matches(paste0("(HHG|AAG)",str_pad(1:17, 2, pad=0))),
                                matches(paste0("(HAG|AHG)",str_pad(1:i, 2, pad=0))), HG, AG))
    (predict(linear_model, RcntHAAH %>% filter(Season %in% TrainSeasons) %>% .[val_idx,])
      - RcntHAAH %>% filter(Season %in% TrainSeasons) %>% .[val_idx,] %>% select(HG,AG)) ^2 %>% 
      colMeans %>%sqrt})) # Predict scores with the model & compute its RMSE 

# Pick the optimal number of past matches from RcntHAAH
sapply(1:n, RcntHAAH_RMSE) %>% as.data.frame() %>% rename_all(~paste(1:n, "match(es)")) %>% 
  t %>% as.data.frame() %>% mutate(Total = HG + AG)

# Update linear regression model from RcntHAAH
linear_model <- lm(cbind(HG,AG) ~ ., data = RcntHAAH %>% filter(Season %in% TrainSeasons) %>% 
                     select(matches(paste0("(HHG|AAG)",str_pad(1:17, 2, pad=0))),
                            matches(paste0("(HAG|AHG)",str_pad(1:13, 2, pad=0))), HG, AG))

# Evaluate RMSE from RcntHAAH with the test set
(predict(linear_model, RcntHAAH %>% filter(Season==TestSeason)) - RcntHAAH %>% filter(Season==TestSeason) %>% select(HG,AG)) ^2 %>% 
  colMeans(na.rm = T) %>%  sqrt %>% print %>% sum

# ___________________________________________________________________________

# Number of available training records with increasing no. of head-to-head encounters
epl_results %>% select(DateTime, HomeTeam, AwayTeam) %>% rename(MatchDate = DateTime) %>%
  inner_join(epl_results, c("HomeTeam", "AwayTeam")) %>% filter(MatchDate > DateTime) %>%
  group_by(MatchDate, HomeTeam, AwayTeam) %>% slice_max(DateTime, n=5) %>% mutate(HHX = rank(-rank(DateTime))) %>% ungroup %>%
  group_by(HHX) %>% summarize(count=n())
  
# Append last head-to-head results of the teams to the data frame 
RcntHAAHX <- epl_results %>% select(DateTime, HomeTeam, AwayTeam) %>% rename(MatchDate = DateTime) %>%
  inner_join(epl_results, c("HomeTeam", "AwayTeam")) %>% 
  filter(MatchDate>DateTime) %>% group_by(MatchDate, HomeTeam, AwayTeam) %>% 
  slice_max(DateTime, n=1) %>% mutate(Match=1) %>% ungroup %>% # Create same home/away results
  select(-Season, -DateTime) %>% rename (HHXG = FTHG, HHXg = FTAG) %>%
  pivot_wider(names_from = Match,  names_sep = "", values_from = c(HHXG, HHXg)) %>%
  inner_join(epl_results, c("AwayTeam"="HomeTeam", "HomeTeam"="AwayTeam")) %>% 
  filter(MatchDate>DateTime) %>% group_by(MatchDate, HomeTeam, AwayTeam) %>% 
  slice_max(DateTime, n=1) %>% mutate(Match=1) %>% ungroup %>% # Create counter home/away results
  select(-Season, -DateTime) %>% rename (AHXG = FTHG, AHXg = FTAG) %>%
  pivot_wider(names_from = Match,  names_sep = "", values_from = c(AHXG, AHXg)) %>%
  right_join(RcntHAAH, c("MatchDate","HomeTeam", "AwayTeam")) %>%
  select(Season, MatchDate, HomeTeam, AwayTeam, HG, AG, 
         starts_with(c("HHXG","AHXG","HHG","AAG","HAG","AHG")))

# Update linear regression model to include head-to-head encounters
linear_model <- lm(cbind(HG,AG) ~ ., data = RcntHAAHX %>% filter(Season %in% TrainSeasons) %>% 
                     select(matches(paste0("(HHG|AAG)",str_pad(1:17, 2, pad=0))),
                            matches(paste0("(HAG|AHG)",str_pad(1:13, 2, pad=0))), 
                            matches("X"), HG, AG))

# Evaluate RMSE from RcntHAAHX with the test set
(predict(linear_model, RcntHAAHX %>% filter(Season==TestSeason)) 
  - RcntHAAHX %>% filter(Season==TestSeason) %>% select(HG,AG)) ^2 %>% 
  colMeans(na.rm = T) %>% sqrt %>% print %>% sum

# Restore linear regression model back to RcntHAAH}
linear_model <- lm(cbind(HG,AG) ~ ., data = RcntHAAH %>% filter(Season %in% TrainSeasons) %>% 
                     select(matches(paste0("(HHG|AAG)",str_pad(1:17, 2, pad=0))),
                            matches(paste0("(HAG|AHG)",str_pad(1:13, 2, pad=0))), HG, AG))

# Evaluate prediction accuracy with the linear model
(predict(linear_model, RcntHAAH %>% filter(Season=="2022-23")) %>% round 
  == RcntHAAH %>% filter(Season=="2022-23") %>% select(HG,AG)) %>% 
  colMeans(na.rm = T) %>%  print %>% prod

# ___________________________________________________________________________

# Filter the dataset for random forest
RF_all <- RcntHAAHX %>% filter(Season %in% TrainSeasons) %>%
  select(HG, AG, matches("(HHG|AAG)"), matches("(HAG|AHG)"), matches("X")) %>% 
  filter(rowSums(is.na(.))==0)

# Tune the mtry parameter with an increment of 20
tuneRF(select(RF_all, -HG, -AG), RF_all$HG, ntreeTry = 50, stepFactor = 20)
tuneRF(select(RF_all, -HG, -AG), RF_all$AG, ntreeTry = 50, stepFactor = 20)

# Create random forest models from all the variables
HG_rf <- randomForest(HG ~ .,  data = RF_all %>% select(-AG), mtry=168)
AG_rf <- randomForest(AG ~ .,  data = RF_all %>% select(-HG), mtry=168)

# Check ntree performance
plot(HG_rf, main = "Home Goal prediction error by tree no.")
plot(AG_rf, main = "Away Goal prediction error by tree no.")

# Evaluate RMSE with the full test set
(predict(HG_rf, RcntHAAHX %>% filter(Season==TestSeason)) 
  - RcntHAAHX %>% filter(Season==TestSeason) %>% select(HG)) ^2 %>% 
  colMeans(na.rm = T) %>%  sqrt

(predict(AG_rf, RcntHAAHX %>% filter(Season==TestSeason)) 
  - RcntHAAHX %>% filter(Season==TestSeason) %>% select(AG)) ^2 %>% 
  colMeans(na.rm = T) %>%  sqrt

# ___________________________________________________________________________

# Plot the importance of the variables
varImpPlot(HG_rf, main = "Top Variable Importance for Home Goal", cex = 0.6)
varImpPlot(AG_rf, main = "Top Variable Importance for Away Goal", cex = 0.6)

# Filter the datasets with variables deemed most important in the corresponding models
RF_shtlst_HG <- select(RF_all, HG, importance(HG_rf) %>% as.data.frame %>% slice_max(order_by = IncNodePurity, n=100) %>% rownames)
RF_shtlst_AG <- select(RF_all, AG, importance(AG_rf) %>% as.data.frame %>% slice_max(order_by = IncNodePurity, n=100) %>% rownames)

# Create random forest models from the shortlisted variables
HG_rf <- randomForest(HG ~ .,  data = RF_shtlst_HG)
AG_rf <- randomForest(AG ~ .,  data = RF_shtlst_AG)

# Evaluate RMSE with the shotlisted test set
(predict(HG_rf, RcntHAAHX %>% filter(Season==TestSeason)) 
  - RcntHAAHX %>% filter(Season==TestSeason) %>% select(HG)) ^2 %>% 
  colMeans(na.rm = T) %>%  sqrt

(predict(AG_rf, RcntHAAHX %>% filter(Season==TestSeason)) 
  - RcntHAAHX %>% filter(Season==TestSeason) %>% select(AG)) ^2 %>% 
  colMeans(na.rm = T) %>%  sqrt

# Evaluate prediction accuracy with the random forest model
HG_Accy <- (predict(HG_rf, RcntHAAHX %>% filter(Season==TestSeason)) %>% round == 
              RcntHAAHX %>% filter(Season==TestSeason) %>% select(HG)) %>% mean(na.rm = T)
AG_Accy <- (predict(AG_rf, RcntHAAHX %>% filter(Season==TestSeason)) %>% round == 
              RcntHAAHX %>% filter(Season==TestSeason) %>% select(AG)) %>% mean(na.rm = T)
data.frame(Category = c("Home Goal", "Away Goal", "Overall"), Accuracy = c(HG_Accy, AG_Accy, HG_Accy * AG_Accy))

# Confusion matrix for HG random forest model
confusionMatrix(predict(HG_rf, RcntHAAHX %>% filter(Season==TestSeason)) %>% round %>% factor,
                RcntHAAHX %>% filter(Season==TestSeason) %>% .$HG %>% factor)

# Confusion matrix for AG random forest model
confusionMatrix(predict(AG_rf, RcntHAAHX %>% filter(Season==TestSeason)) %>% round %>% factor,
                RcntHAAHX %>% filter(Season==TestSeason) %>% .$AG %>% factor)
