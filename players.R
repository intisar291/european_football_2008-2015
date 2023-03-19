
# connect to database
db_file="file_path/to/EuropeanFootball.sqlite"
con <- dbConnect(SQLite(), dbname = db_file)

player<- tibble::as_tibble(dbGetQuery(con,"SELECT * FROM Player"))
player_attributes<- tibble::as_tibble(dbGetQuery(con,"SELECT * FROM Player_Attributes"))

team<- tibble::as_tibble(dbGetQuery(con,"SELECT * FROM Team"))
team_attributes<- tibble::as_tibble(dbGetQuery(con,"SELECT * FROM Team_Attributes"))

# Agility vs height
player_table= player_attributes%>%
  mutate(playing_years=year(as_date(date)))%>%
  inner_join(player,player_attributes,by="player_fifa_api_id")

player_table%>%filter(playing_years==2015)%>%
  ggplot(aes(x=agility,y=height))+geom_line(color="blue",size=1,alpha=0.5)+
  scale_x_continuous(limits = c(15,100),breaks=seq(15,100,by=20))
    
# reaction and volly based on preferred foot  


player_table%>%filter(playing_years==2015 & !is.na(preferred_foot))%>%
  ggplot(aes(x=reactions,y=volleys,color=preferred_foot))+geom_point(
                                               alpha=0.5)+
  
  scale_x_continuous(limits = c(30,100),breaks=seq(30,100,by=10))+
  scale_y_continuous(limits = c(15,100),breaks=seq(15,100,by=20))


# long pass and aggression with defensive workrate

## Change the string 0-9 in to category in defensive_work_rate 
####(some values are already in categorical form in defensive_work_rate)

player_table$defensive_work_rate<-
  ifelse(player_table$defensive_work_rate %in% c("0","1","2","3"),"low",
         ifelse(player_table$defensive_work_rate %in% c("4","5","6","7"),"medium",
                ifelse(player_table$defensive_work_rate %in% c("8","9"),"high",
                       player_table$defensive_work_rate)))


player_table%>%
  filter(playing_years==2015 & !is.na(defensive_work_rate))%>%
  ggplot(aes(x=long_passing,y=aggression,color=defensive_work_rate))+
  geom_point(shape=15,alpha=0.5,size=1)

