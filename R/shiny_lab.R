



shiny_lab<-function(){

  #Functions for parsing data from Web API

  #EPL = 2021, BundesLiga = 2002, LaLiga = 2014, Ligue1 = 2015, SerieA = 2019

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

  #results from top 5 leagues

  get_results<-function(league_name){

    leagues=list(EPL = 2021, BundesLiga = 2002,
                 LaLiga = 2014, Ligue1 = 2015, SerieA = 2019)

    my_url<-sprintf("https://api.football-data.org/v2/competitions/%s/matches?status=FINISHED",
                    leagues[[league_name]])

    comp2 <- GET(url=my_url,
                 add_headers("X-Auth-Token"="ad55814c6db4453d8d7c63b828eb08a2")
    )

    df<-fromJSON(rawToChar(comp2$content))

    ndf<-df$matches


    ref<-cbind(ndf$homeTeam$name, ndf$score$fullTime$homeTeam, ndf$score$fullTime$awayTeam,
               ndf$awayTeam$name,ndf$matchday,ndf$utcDate)

    colnames(ref)<-c("Home Team","Home Team Score","Away Team Score","Away Team",
                     "Matchday","Date")

    ref<-as_tibble(ref)


    ref$Matchday<-as.numeric(ref$Matchday)

    ref$Date<-as.Date(ref$Date)

    ref<-as.data.frame(ref)

    return(ref)

  }

  #fixtures from top 5 leagues

  get_fixtures<-function(league_name,matchday){

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

  #Shiny Implementation

  require(shiny)

  shinyApp(


    ui <- fluidPage(


      selectInput(inputId = "ln",label="Top 5 Leagues",
                  choices = list("EPL", "BundesLiga", "LaLiga", "Ligue1", "SerieA"),
                  selected="EPL"),

      fluidRow(column(width=6, tableOutput(outputId = "t1")),
               column(width=6, dateRangeInput(inputId = "resdates",
                                              "Results",
                                              min = as.Date("2021-08-13"),
                                              max= as.Date("2022-05-22"),
                                              start = "2021-09-11",
                                              end = "2021-10-11"
               )),
               column(width=6, dataTableOutput(outputId='t4')),
               column(width=6, dateRangeInput(inputId = "fixdates",
                                              "Fixtures",
                                              min = as.Date("2021-08-13"),
                                              max= as.Date("2022-05-22"),
                                              start = "2021-09-11",
                                              end = "2021-10-11"
               )),
               column(width=6, dataTableOutput(outputId='t3'))),

      fluidRow(
        column(width=5, tableOutput(outputId='t2')),
        column(width=3, plotOutput("pl1",width="400px")),
        column(width=3,  plotOutput("pl2",width="400px"))),

    ),

    server <- function(input, output, session) {

      fixtures<-reactive({

        fix<-get_fixtures(league_name = input$ln)

        m<-as.Date(as.character(input$fixdates[1]))

        n<-as.Date(as.character(input$fixdates[2]))

        fix[fix$Date>m & fix$Date<n,]

      })

      results<-reactive({

        res<-get_results(league_name = input$ln)

        m<-as.Date(as.character(input$resdates[1]))

        n<-as.Date(as.character(input$resdates[2]))

        res[res$Date>m & res$Date<n,]

      })

      scor<-reactive({

        st1<-get_standings(league_name = input$ln)

        top<-st1[order(st1[,"GF"],decreasing = TRUE),]

        top$Team <- factor(top$Team, levels = top$Team[order(top$GF)])

        toppl<-ggplot(data = top)+
          geom_bar(mapping = aes(y = Team, x = GF), stat = "identity",color='black',
                   fill="blue", alpha=0.1)+
          xlab("Goals Scored")+
          ggtitle("Goals Scored by Teams")
      })


      conc<-reactive({

        st2<-get_standings(league_name = input$ln)

        worst<-st2[order(st2[,"GA"],decreasing = F),]

        worst$Team <- factor(worst$Team, levels = worst$Team[order(worst$GA,decreasing=TRUE)])

        worstpl<-ggplot(data = worst)+
          geom_bar(mapping = aes(y = Team, x = GA), stat = "identity",color='black',
                   fill='red',alpha=0.1 )+
          xlab("Goals Conceded")+
          ggtitle("Goals Conceded by Teams")

      })

      output$t1 <- renderTable(get_standings(league_name = input$ln)[,c(1,2,3,4,5,6,10)],
                               spacing = "m")

      output$t2 <- renderTable({get_scorers(league_name = input$ln)})

      output$t3 <- renderDataTable(fixtures(), options = list(pageLength = 5, autoWidth = TRUE))

      output$t4 <- renderDataTable(results(), options = list(pageLength = 4, autoWidth = TRUE))

      output$pl1<-renderPlot(print(scor()),res=96)

      output$pl2<-renderPlot(print(conc()),res=96)


    }

  )

}

