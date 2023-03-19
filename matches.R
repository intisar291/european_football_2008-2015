
# connect to database
db_file="file_path/to/EuropeanFootball.sqlite"
con <- dbConnect(SQLite(), dbname = db_file)

# table queries

match <- tibble::as_tibble(dbGetQuery(con,"SELECT * FROM Match"))
league <-tibble::as_tibble(dbGetQuery(con,"SELECT * FROM League"))
head(match[,1:3],n=3)
colnames(match)
#top 6 leagues goal comparison

##top 6 team joining with league table & match table

top_leagues= filter(league,name %in% list("England Premier League",
                                    "Spain LIGA BBVA", 
                                    "Italy Serie A",
                                    "France Ligue 1",
                                    "Portugal Liga ZON Sagres",
                                    "Germany 1. Bundesliga"))

top_leagues_match=inner_join(match, top_leagues %>% select(league_id = id,league_name=name), by = "league_id")

## Checking the avg goals
avg_goal_league=top_leagues_match%>%
  group_by(league_name)%>%
  filter(!is.na(away_team_goal) | !is.na(home_team_goal))%>%
  summarise(avg_goals=mean(home_team_goal+away_team_goal),
            avg_home_goal=mean(home_team_goal),
            avg_away_goal=mean(away_team_goal))

avg_goal_league%>%              
  pivot_longer(cols = c(avg_goals, avg_home_goal, avg_away_goal),
              names_to = "columns_names",
              values_to = "columns_values")%>%
  ggplot(aes(x = league_name, y = columns_values, fill = columns_names)) +
  geom_col(position = "dodge")+
  scale_fill_manual(values=heat.colors(3))

#check the previously made pivot table meta structure in R
avg_goal_league%>%              
  pivot_longer(cols = c(avg_goals, avg_home_goal, avg_away_goal),
               names_to = "columns_names",
               values_to = "columns_values")



# Comparing to otherleagues
new_l=league %>% 
  mutate(group_name=fct_collapse(name,top_5=c("England Premier League",
                                   "Spain LIGA BBVA", 
                                   "Italy Serie A",
                                   "France Ligue 1",
                                   "Germany 1. Bundesliga")))%>%
  mutate(group_name=fct_other(group_name,keep="top_5",other_level="Other Leagues"))


new_l_match=inner_join(match,new_l
                       %>%select(league_id=id, group_name=group_name),by="league_id")

new_l_match%>%
  group_by(group_name)%>%
  filter(!is.na(home_team_goal|!is.na(away_team_goal)))%>%
  summarise(avg_goal=mean(home_team_goal+away_team_goal),
            sd_goal=sd(home_team_goal+away_team_goal))%>%
  pivot_longer(cols = c(avg_goal,sd_goal),
               names_to = "Matrics",
               values_to = "Values")%>%
  ggplot(aes(x=group_name,y=Values,fill=Matrics))+
  geom_col(position="dodge")


# Home advantage



match %>%
  pivot_longer(cols=c(home_team_goal,away_team_goal),
               names_to = "playing_as",
               values_to = "number_of_goals")%>%
  ggplot(aes(x=playing_as,y=number_of_goals))+
  geom_violin(position = "identity",adjust=0.9
              ,alpha=0.5,size=0.8)+
  stat_summary(fun=median,geom="crossbar",alpha=0.25,width=0.2,)

# more goal scoring months

match%>%
  mutate(match_month=month(as_date(date),label=TRUE))%>%
  group_by(match_month)%>%
  filter(!is.na(home_team_goal|!is.na(away_team_goal)))%>%
  summarize(avg_goal=mean(home_team_goal+away_team_goal))%>%
  ggplot(aes(x=match_month,y=avg_goal,group=1))+
  geom_line(color="black")+
  geom_point(color="red")+
  scale_y_continuous(limits=c(0,5))

dbDisconnect(con)

