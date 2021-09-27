

#standings from top 5 leagues

get_standings<-function(league_name='EPL'){

  leagues=list(EPL = 2021, BundesLiga = 2002,
               LaLiga = 2014, Ligue1 = 2015, SerieA = 2019)

  my_url<-sprintf("https://api.football-data.org/v2/competitions/%s/standings",
                  leagues[[league_name]])

  comp2 <- GET(url=my_url,
               add_headers("X-Auth-Token"="ad55814c6db4453d8d7c63b828eb08a2")
  )

  df<-fromJSON(rawToChar(comp2$content))

  standings<-df$standings[[4]][[1]]

  standings[,2]<-standings[,2]$name

  standings<-standings[,-4]

  colnames(standings)<-c("Position", "Team", "MP",
                         "Wins","Draws", "Losses","Points", "GF",
                         "GA", "GD")

  col_order<-c("Position","Team", "MP",
               "Wins","Draws", "Losses", "GF",
               "GA", "GD", "Points")

  standings<-standings[,col_order]

  return(standings)

}
