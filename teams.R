
db_file="F:/Study/ORBA/Visual Analytics/Exercise 2022 (Ex. 5 & 7 not given)/database/EuropeanFootball.sqlite"
con <- dbConnect(SQLite(), dbname = db_file)

team<- tibble::as_tibble(dbGetQuery(con,"SELECT * FROM Team"))
team_attributes<- tibble::as_tibble(dbGetQuery(con,"SELECT * FROM Team_Attributes"))

colnames(team_attributes)
unique(team_attributes$chanceCreationPassingClass)

#Teams with high chance creation
join_team = team%>%filter(!is.na(team_fifa_api_id))%>%
  inner_join(team_attributes,team %>% select(team_name=team_long_name), by="team_fifa_api_id")
  
colnames(join_team)
colnames(team_attributes)

# Risky passing and lot of crossing teams build up play and defensive aggression
join_team %>%
  group_by(team_long_name) %>%
  filter(chanceCreationPassingClass == "Risky" & chanceCreationCrossingClass == "Lots") %>%
  select(team_long_name, defenceAggression, buildUpPlaySpeed, chanceCreationPassing, chanceCreationCrossing, chanceCreationShooting) %>%
  arrange(desc(buildUpPlaySpeed)) %>%
  summarise_at(vars(defenceAggression, buildUpPlaySpeed), mean, na.rm = TRUE) %>%
  rename(mean_defenceAggression = defenceAggression, mean_buildUpPlaySpeed = buildUpPlaySpeed)%>%
  ggplot(aes(x = mean_defenceAggression, y = mean_buildUpPlaySpeed)) +
  geom_jitter()+geom_smooth()


unique(join_team$buildUpPlayPassingClass)


# Defensive pressure and chance creation by passing with build up play speed 
join_team %>%
  group_by(team_long_name) %>%
  #filter(chanceCreationShootingClass == "Lots") %>%
  select(team_long_name, defencePressure,defencePressureClass,
         chanceCreationPassingClass, chanceCreationPassing,
         chanceCreationCrossingClass,chanceCreationCrossing,
         chanceCreationShootingClass,chanceCreationShooting,
         chanceCreationPositioningClass, buildUpPlaySpeed) %>%
  arrange(desc(defencePressureClass)) %>%
  ggplot(aes(y = chanceCreationPassingClass, x = defencePressureClass,color=buildUpPlaySpeed)) +
  geom_jitter()+geom_smooth()+ 
  scale_color_gradient(low="yellow", high="red")


# Position class vs defensive width
join_team%>%
  group_by(team_long_name) %>%
  select(team_long_name, defenceTeamWidth,defenceTeamWidthClass,
         chanceCreationPositioningClass, buildUpPlaySpeed)%>%
  ggplot(aes(x = chanceCreationPositioningClass, y = defenceTeamWidth)) +
  geom_boxplot()+geom_dotplot(binaxis='y', 
                              stackdir='center', 
                              dotsize = 0.12, 
                              fill="red")




# Chance from pass and type of pass on build up and play speed for the team with most shooting
join_team %>%
  group_by(team_long_name) %>%
  filter(chanceCreationShootingClass == "Lots") %>%
  select(team_long_name, buildUpPlayPassingClass, buildUpPlayPassing,
         buildUpPlayDribblingClass,buildUpPlayDribbling,
         chanceCreationPassingClass, chanceCreationPassing,
         chanceCreationCrossingClass,chanceCreationCrossing,
         #chanceCreationShootingClass,chanceCreationShooting,
         chanceCreationPositioningClass, buildUpPlaySpeed,) %>%
  #arrange(desc(defencePressureClass)) %>%
  ggplot(aes(x = chanceCreationPassing, y = buildUpPlayPassingClass,color=buildUpPlaySpeed)) +
  geom_point(size=3)+
  scale_color_gradient(low="skyblue", high="black")

colnames(join_team)
