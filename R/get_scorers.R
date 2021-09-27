

#goal scorers from top 5 leagues

get_scorers<-function(league_name){

  leagues=list(EPL = 2021, BundesLiga = 2002,
               LaLiga = 2014, Ligue1 = 2015, SerieA = 2019)

  my_url<-sprintf("https://api.football-data.org/v2/competitions/%s/scorers",
                  leagues[[league_name]])

  comp2 <- GET(url=my_url,
               add_headers("X-Auth-Token"="ad55814c6db4453d8d7c63b828eb08a2")
  )

  df<-fromJSON(rawToChar(comp2$content))

  t2<-df$scorers

  new_t<-data.frame(player=t2[,'player']$name,position=t2[,'player']$position,
                    team=t2[,'team']$name, goals=t2[,'numberOfGoals'])

  return(new_t)

}
