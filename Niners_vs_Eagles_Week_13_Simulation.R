library(tidyverse)
library(ggthemes)

### Code For Eagles


eagles_drives <- 11

set.seed(123)
eagles <- function (drives){
  eagles_points <- 0 
  #eagles_score_pct determined by taking team scoring percentage (45.4%; adjusted manually to 40.4% after Dallas Goedert got injured)
  #Adjusted Eagle's scoring percentage by comparing Average Opponent's ELO compared to 49ers ELO
  #final calculation: 0.404 * (1503.18182/1693) = 0.358703753
  eagles_score_pct <- 0.358703753
  brown_rec_td <- 0
  smith_rec_td <- 0
  swift_rec_td <- 0
  stoll_rec_td <- 0
  zacc_rec_td <- 0
  jones_rec_td <- 0
  watkins_rec_td <- 0
  hurts_rushing_td <- 0
  swift_rushing_td <- 0
  gainwell_rushing_td <- 0
  eagles_passing_td <- 0
  eagles_rushing_td <- 0
  eagles_fg_made <- 0
  while (drives > 0){
    #determines if eagles get any points
    if (runif(1) <= eagles_score_pct) {
      #determined by taking total TDs/(Total TDs + FG)
      # 35/(35+19) = 0.648149148
      eagles_td_pct <- 0.648149148
      #determines if eagles score a TD or a FG
      if(runif(1) <= eagles_td_pct){
        #determines whether the eagles score a Passing TD or a Rushing TD
        #On the season, the Eagles had 18 Passing TDs and 17 Rushing TDs
        #18/(18+17) = 0.514285714
        eagles_passing_td_pct <- 0.514285714
        if(runif(1) <= eagles_passing_td_pct){
          eagles_points <- eagles_points + 7
          eagles_passing_td <- eagles_passing_td + 1
          #each player's Redzone target% from the season
          #these numbers sum up to one
          brown_td_pct <- 0.44
          smith_td_pct <- 0.16
          swift_td_pct <- 0.16
          stoll_td_pct <- 0.08
          zacc_td_pct <- 0.08
          jones_td_pct <- 0.04
          watkins_td_pct <- 0.04
          u <- runif(1)
          if(u <= brown_td_pct){
            brown_rec_td <- brown_rec_td + 1
            #given that AJ Brown doesn't score, we assume that smith's chance to score
            #is the sum of AJ Browns TD% and Devonta Smith's TD%
            #this assumption is made for all receivers
          } else if(u <= sum(brown_td_pct,smith_td_pct)){
            smith_rec_td <- smith_rec_td + 1
          } else if(u <= sum(brown_td_pct,smith_td_pct,swift_td_pct)){
            swift_rec_td <- swift_rec_td + 1
          } else if(u <= sum(brown_td_pct,smith_td_pct,swift_td_pct,stoll_td_pct)){
            stoll_rec_td <- stoll_rec_td + 1
          } else if(u <= sum(brown_td_pct,smith_td_pct,swift_td_pct,stoll_td_pct,zacc_td_pct)){
            zacc_rec_td <- zacc_rec_td + 1
          } else if(u <= sum(brown_td_pct,smith_td_pct,swift_td_pct,stoll_td_pct,zacc_td_pct,jones_td_pct)){
            jones_rec_td <- jones_rec_td + 1
          } else{
            watkins_rec_td <- watkins_rec_td + 1
          }
        } else {
          #Rushing TD
          eagles_points <- eagles_points + 7
          eagles_rushing_td <- eagles_rushing_td + 1
          #numbers gotten by taking the proportion of Rushing TDs on the Season for the team
          hurts_td_pct <- (12/18)
          swift_td_pct <- (4/18)
          gainwell_td_pct <- (2/18)
          u <- runif(1)
          if(u <= hurts_td_pct){
            hurts_rushing_td <- hurts_rushing_td + 1
            #follows the same logic as receivers
          } else if(u <= sum(hurts_td_pct,swift_td_pct)){
            swift_rushing_td <- swift_rushing_td + 1
          }else {
            gainwell_rushing_td <- gainwell_rushing_td + 1
          }
        }
      } else{
        #FG
        eagles_points <- eagles_points + 3
        eagles_fg_made <- eagles_fg_made + 1
      }
    } else {
      eagles_points <- eagles_points
    }
    drives <- drives - 1
  }
  return(list(eagles_points = as.numeric(eagles_points),
              eagles_passing_td = as.numeric(eagles_passing_td),
              eagles_rushing_td = as.numeric(eagles_rushing_td),
              eagles_fg_made = as.numeric(eagles_fg_made),
              brown_rec_td = as.numeric(brown_rec_td),
              smith_rec_td = as.numeric(smith_rec_td),
              swift_rec_td = as.numeric(swift_rec_td),
              stoll_rec_td = as.numeric(stoll_rec_td),
              zacc_rec_td = as.numeric(zacc_rec_td),
              jones_rec_td = as.numeric(jones_rec_td),
              watkins_rec_td = as.numeric(watkins_rec_td),
              hurts_rushing_td = as.numeric(hurts_rushing_td),
              swift_rushing_td = as.numeric(swift_rushing_td),
              gainwell_rushing_td = as.numeric(gainwell_rushing_td)))
}

do.call(rbind, replicate(100000, eagles(eagles_drives), simplify = FALSE)) -> EaglesTotals
EaglesTotals <- as.data.frame(EaglesTotals)
EaglesTotals <- EaglesTotals %>% mutate(game_number = 1:n())
### 49ers simulation code follows the same structure as the Eagles simulation code

### Code for 49ers
niners_drives <- 11

set.seed(234)
niners <- function (drives){
  niners_points <- 0 
  niners_score_pct <- 0.397844506
  mccaffrey_rec_td <- 0
  kittle_rec_td <- 0
  samuel_rec_td <- 0
  aiyuk_rec_td <- 0
  juszczyk_rec_td <- 0
  bell_rec_td <- 0
  dwelley_rec_td <- 0
  jennings_rec_td <- 0
  mccloud_rec_td <- 0
  mitchell_rec_td <- 0
  mccaffrey_rushing_td <- 0
  samuel_rushing_td <- 0
  mason_rushing_td <- 0
  purdy_rushing_td <- 0
  niners_passing_td <- 0
  niners_rushing_td <- 0
  niners_fg_made <- 0
  while (drives > 0){
    if (runif(1) <= niners_score_pct) {
      niners_td_pct <- 0.685185185
      if(runif(1) <= niners_td_pct){
        niners_passing_td_pct <- 0.513513514
        if(runif(1) <= niners_passing_td_pct){
          niners_points <- niners_points + 7
          niners_passing_td <- niners_passing_td + 1
          mccaffrey_td_pct <- 0.2766
          kittle_td_pct <- 0.2128
          samuel_td_pct <- 0.1915
          aiyuk_td_pct <- 0.1064
          juszczyk_td_pct <- 0.0851
          mitchell_td_pct <- 0.0426
          bell_td_pct <- 0.0213
          dwelley_td_pct <- 0.0213
          jennings_td_pct <- 0.0213
          mccloud_td_pct <- 0.0213
          u <- runif(1)
          if(u <= mccaffrey_td_pct){
            mccaffrey_rec_td <- mccaffrey_rec_td + 1
          } else if(u <= sum(mccaffrey_td_pct,kittle_td_pct)){
            kittle_rec_td <- kittle_rec_td + 1
          } else if(u <= sum(mccaffrey_td_pct,kittle_td_pct,samuel_td_pct)){
            samuel_rec_td <- samuel_rec_td + 1
          } else if(u <= sum(mccaffrey_td_pct,kittle_td_pct,samuel_td_pct,aiyuk_td_pct)){
            aiyuk_rec_td <- aiyuk_rec_td + 1
          } else if(u <= sum(mccaffrey_td_pct,kittle_td_pct,samuel_td_pct,aiyuk_td_pct,juszczyk_td_pct)){
            juszczyk_rec_td <- juszczyk_rec_td + 1
          } else if(u <= sum(mccaffrey_td_pct,kittle_td_pct,samuel_td_pct,aiyuk_td_pct,juszczyk_td_pct,mitchell_td_pct)){
            mitchell_rec_td <- mitchell_rec_td + 1
          } else if(u <= sum(mccaffrey_td_pct,kittle_td_pct,samuel_td_pct,aiyuk_td_pct,juszczyk_td_pct,mitchell_td_pct,bell_td_pct)){
            bell_rec_td <- bell_rec_td + 1
          } else if(u <= sum(mccaffrey_td_pct,kittle_td_pct,samuel_td_pct,aiyuk_td_pct,juszczyk_td_pct,mitchell_td_pct,bell_td_pct,dwelley_td_pct)){
            dwelley_rec_td <- dwelley_rec_td + 1
          } else if(u <= sum(mccaffrey_td_pct,kittle_td_pct,samuel_td_pct,aiyuk_td_pct,juszczyk_td_pct,mitchell_td_pct,bell_td_pct,dwelley_td_pct,jennings_td_pct)){
            jennings_rec_td <- jennings_rec_td + 1
          } else{
            mccloud_rec_td <- mccloud_rec_td + 1
          }
        } else {
          #Rushing TD
          niners_points <- niners_points + 7
          niners_rushing_td <- niners_rushing_td + 1
          mccaffrey_td_pct <- (12/20)
          samuel_td_pct <- (4/20)
          mason_td_pct <- (2/20)
          purdy_td_pct <- (2/20)
          u <- runif(1)
          if(u <= mccaffrey_td_pct){
            mccaffrey_rushing_td <- mccaffrey_rushing_td + 1
          }else if(u <= sum(mccaffrey_td_pct,samuel_td_pct)){
            samuel_rushing_td <- samuel_rushing_td + 1
          } else if(u <= sum(mccaffrey_td_pct,samuel_td_pct,mason_td_pct)){
            mason_rushing_td <- mason_rushing_td + 1
          } else{
            purdy_rushing_td <- purdy_rushing_td + 1
          }
        }
      } else{
        #FG
        niners_points <- niners_points + 3
        niners_fg_made <- niners_fg_made + 1
      }
    } else {
      niners_points <- niners_points
    }
    drives <- drives - 1
  }
  return(list(niners_points = as.numeric(niners_points), 
              niners_passing_td = as.numeric(niners_passing_td),
              niners_rushing_td = as.numeric(niners_rushing_td),
              niners_fg_made = as.numeric(niners_fg_made),
              mccaffrey_rec_td = as.numeric(mccaffrey_rec_td),
              kittle_rec_td = as.numeric(kittle_rec_td),
              samuel_rec_td = as.numeric(samuel_rec_td),
              aiyuk_rec_td = as.numeric(aiyuk_rec_td),
              juszczyk_rec_td = as.numeric(juszczyk_rec_td),
              bell_rec_td = as.numeric(bell_rec_td),
              dwelley_rec_td = as.numeric(dwelley_rec_td),
              jennings_rec_td = as.numeric(jennings_rec_td),
              mccloud_rec_td = mccloud_rec_td,
              mitchell_rec_td = mitchell_rec_td,
              mccaffrey_rushing_td = mccaffrey_rushing_td,
              samuel_rushing_td = samuel_rushing_td,
              mason_rushing_td = mason_rushing_td,
              purdy_rushing_td = purdy_rushing_td))
}

do.call(rbind, replicate(100000, niners(niners_drives), simplify = FALSE)) -> NinersTotals
NinersTotals <- as.data.frame(NinersTotals)
NinersTotals <- NinersTotals %>% mutate(game_number = 1:n())

SimulationResults <- left_join(EaglesTotals, NinersTotals, by = "game_number")
SimulationResults <- SimulationResults %>% mutate(eagles_win = ifelse(as.numeric(eagles_points) > as.numeric(niners_points),1,0))
SimulationResults <- SimulationResults %>% mutate(niners_win = ifelse(as.numeric(eagles_points) < as.numeric(niners_points),1,0))
SimulationResults <- SimulationResults %>% mutate(overtime = ifelse(as.numeric(eagles_points) == as.numeric(niners_points),1,0))

eagles_win_prob <- mean(SimulationResults$eagles_win)
niners_win_prob <- mean(SimulationResults$niners_win)
overtime <- mean(SimulationResults$overtime)
eagles_expected_points <- mean(as.numeric(SimulationResults$eagles_points))
niners_expected_points <- mean(as.numeric(SimulationResults$niners_points))

brown_2plus_Td <- SimulationResults %>% filter(brown_rec_td > 1) %>% summarize(aj_games = n(),aj_pct_games = round((n()/100000)*100,2))
smith_2plus_Td <- SimulationResults %>% filter(smith_rec_td > 1) %>% summarize(smith_games = n(), smith_pct_games = round((n()/100000)*100,2))
swift_2plus_Td <- SimulationResults %>% filter(swift_rec_td > 1) %>% summarize(swift_games = n(), swift_pct_games = round((n()/100000)*100,2))
stoll_2plus_Td <- SimulationResults %>% filter(stoll_rec_td > 1) %>% summarize(stoll_games = n(), stoll_pct_games = round((n()/100000)*100,2))
zacc_2plus_Td <- SimulationResults %>% filter(zacc_rec_td > 1) %>% summarize(zacc_games = n(), zacc_pct_games = round((n()/100000)*100,2))
jones_2plus_Td <- SimulationResults %>% filter(jones_rec_td > 1) %>% summarize(jones_games = n(), jones_pct_games = round((n()/100000)*100,2))
watkins_2plus_Td <- SimulationResults %>% filter(watkins_rec_td > 1) %>% summarize(watkins_games = n(), watkins_pct_games = round((n()/100000)*100,2))
mccaffrey_2plus_Td <- SimulationResults %>% filter(mccaffrey_rec_td > 1) %>% summarize(mccaffrey_games = n(), mccaffrey_pct_games = round((n()/100000)*100,2))
kittle_2plus_Td <- SimulationResults %>% filter(kittle_rec_td > 1) %>% summarize(kittle_games = n(), kittle_pct_games = round((n()/100000)*100,2))
samuel_2plus_Td <- SimulationResults %>% filter(samuel_rec_td > 1) %>% summarize(samuel_games = n(), samuel_pct_games = round((n()/100000)*100,2))
aiyuk_2plus_Td <- SimulationResults %>% filter(aiyuk_rec_td > 1) %>% summarize(aiyuk_games = n(), aiyuk_pct_games = round((n()/100000)*100,2))
juszczyk_2plus_Td <- SimulationResults %>% filter(juszczyk_rec_td > 1) %>% summarize(juszczyk_games = n(), juszczyk_pct_games = round((n()/100000)*100,2))
bell_2plus_Td <- SimulationResults %>% filter(bell_rec_td > 1) %>% summarize(bell_games = n(), bell_pct_games = round((n()/100000)*100,2))
dwelley_2plus_Td <- SimulationResults %>% filter(dwelley_rec_td > 1) %>% summarize(dwelley_games = n(), dwelley_pct_games = round((n()/100000)*100,2))
jennings_2plus_Td <- SimulationResults %>% filter(jennings_rec_td > 1) %>% summarize(jennings_games = n(), jennings_pct_games = round((n()/100000)*100,2))
mccloud_2Plus_Td <- SimulationResults %>% filter(mccloud_rec_td > 1) %>% summarize(mccloud_games = n(), mccloud_pct_games = round((n()/100000)*100,2))
mitchell_2Plus_Td <- SimulationResults %>% filter(mitchell_rec_td > 1) %>% summarize(mitchell_games = n(), mitchell_pct_games = round((n()/100000)*100,2))

X2plus_TD <- bind_cols(brown_2plus_Td,smith_2plus_Td,swift_2plus_Td,stoll_2plus_Td,zacc_2plus_Td,jones_2plus_Td,watkins_2plus_Td,mccaffrey_2plus_Td,kittle_2plus_Td,
                       samuel_2plus_Td,aiyuk_2plus_Td,juszczyk_2plus_Td,bell_2plus_Td,dwelley_2plus_Td,jennings_2plus_Td,mccloud_2Plus_Td,mitchell_2Plus_Td)

write_csv(X2plus_TD,file = "2+ TDs.csv")


### 49ers points histogram
SimulationResults %>% ggplot(aes(as.numeric(niners_points))) +
  geom_histogram(binwidth = 7, color = "#B3995D",fill = "#AA0000",breaks = seq(-0.1,77,7)) +
  scale_x_continuous(breaks = seq(0,77,7)) +
  scale_y_continuous(breaks = seq(0,30000,5000)) +
  ggtitle("49ers Points Scored Histogram")+
  xlab("49ers Points Scored")+
  theme_clean()
  

## Eagles points histogram
SimulationResults %>% ggplot(aes(as.numeric(eagles_points))) +
  geom_histogram(binwidth = 7, color = "#A5ACAF",fill = "#004C54",breaks = seq(-0.1,77,7)) +
  scale_x_continuous(breaks = seq(0,77,7)) +
  scale_y_continuous(breaks = seq(0,30000,5000)) +
  ggtitle("Eagles Points Scored Histogram")+
  xlab("Eagles Points Scored")+
  theme_clean()

## Eagles Passing TD Bar Graph
SimulationResults %>% ggplot(aes(as.numeric(eagles_passing_td))) +
  geom_bar(color = "#A5ACAF",fill = "#004C54") +
  scale_x_continuous(breaks = seq(0,11,1)) +
  xlab("Eagles Passing TD")+
  theme_clean()


## 49ers Passing TD Bar Graph
SimulationResults %>% ggplot(aes(as.numeric(niners_passing_td))) +
  geom_bar(color = "#B3995D",fill = "#AA0000") +
  scale_x_continuous(breaks = seq(0,11,1)) +
  xlab("Niners Passing TD")+
  theme_clean()

## Eagles Rushing TD Bar Graph
SimulationResults %>% ggplot(aes(as.numeric(eagles_rushing_td))) +
  geom_bar(color = "#A5ACAF",fill = "#004C54") +
  scale_x_continuous(breaks = seq(0,11,1)) +
  xlab("Eagles Rushing TD")+
  theme_clean()

## 49ers Rushing TD Bar Graph
SimulationResults %>% ggplot(aes(as.numeric(niners_rushing_td))) +
  geom_bar(color = "#B3995D",fill = "#AA0000") +
  scale_x_continuous(breaks = seq(0,11,1)) +
  xlab("Niners Rushing TD")+
  theme_clean()

## AJ Brown TD Bar Chart
SimulationResults %>% ggplot(aes(as.numeric(brown_rec_td))) +
  geom_bar(color = "#A5ACAF",fill = "#004C54") +
  scale_x_continuous(breaks = seq(0,11,1)) +
  ggtitle("AJ Brown Expected Receiving TDs") +
  xlab("AJ Brown Receiving TD")+
  theme_clean()

