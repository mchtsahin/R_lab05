

#fixtures from top 5 leagues

get_fixtures<-function(league_name){

  leagues=list(EPL = 2021, BundesLiga = 2002,
               LaLiga =  2014, Ligue1 = 2015, SerieA = 2019)

  my_url<-sprintf("https://api.football-data.org/v2/competitions/%s/matches?status=SCHEDULED",
                  leagues[[league_name]])

  comp2 <- GET(url=my_url,
               add_headers("X-Auth-Token"="ad55814c6db4453d8d7c63b828eb08a2")
  )

  df<-fromJSON(rawToChar(comp2$content))

  ndf<-df$matches


  ref<-cbind(ndf$homeTeam$name,ndf$awayTeam$name,ndf$matchday,ndf$utcDate)

  colnames(ref)<-c("Home Team","Away Team",
                   "Matchday","Date")

  ref<-as_tibble(ref)


  ref$Matchday<-as.numeric(ref$Matchday)

  ref$Date<-as.Date(ref$Date)

  ref<-as.data.frame(ref)

  return(ref)

}
