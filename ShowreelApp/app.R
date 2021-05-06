####Showreel App####
####Mark A Reuter####
library(shiny)
library(shinythemes)
library(tuneR)
library(RJSONIO)

#colour 1B5714
#font https://www.fontspace.com/fogle-hunter-font-f23730


print(Sys.Date())
print(Sys.time())
cols <- c("forestgreen","darkgreen","firebrick","firebrick4")
wd <- getwd()
print(wd)

metadata <- read.csv(paste0(wd,"/www/showreel_metadata.txt"),
                     sep = "\t",
                     stringsAsFactors = FALSE)
print(metadata)
reimagined <- list.files(paste0(wd,"/www"),
                         pattern = "Reimagined",
                         full.names = FALSE,
                         include.dirs = FALSE)
print(reimagined)

ui <- navbarPage(theme = shinytheme("darkly"),
                 "abominable | composer | showreel",
                 tabPanel("Film Scores Reimagined",
                          #titlePanel("abominable | composer | showreel"),
                          img(src='/images/circle-cropped.png', align = "right",width = 90),
                          #ui <- fluidPage(theme = shinytheme("darkly"),
                          ## Application title
                          #titlePanel("abominable | composer | showreel"),
                          #img(src='/images/circle-cropped.png', align = "right",width = 90),
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         selectInput(
                                           inputId = "fileChooser",
                                           label = "Tracks",
                                           choices = reimagined,
                                           selected = NULL,
                                           multiple = FALSE,
                                           selectize = TRUE,
                                           width = NULL,
                                           size = NULL
                                         ),
                                         actionButton(inputId = "zgo",
                                                      label = "load",
                                                      icon = NULL,
                                                      width = NULL),
                                         br(),
                                         br(),
                                         htmlOutput("zplayer"),
                                         br(),
                                         verbatimTextOutput("finfo"),
                                         hr(),
                                         fluidRow(
                                           column(width = 2,
                                                  uiOutput("emailLink"),
                                                  br(),
                                                  uiOutput("instagram")
                                           ),
                                           column(width = 1,
                                                  uiOutput("youtubeLink"),
                                                  br(),
                                                  uiOutput("kofiLink")
                                           )
                                         ),
                                         hr()
                            ),
                            ####mainpanel 1 ####
                            mainPanel(width = 9,
                                      fluidRow(
                                        column(width = 9,
                                               uiOutput("trackDesc"),
                                               plotOutput("wavPlot"),
                                               hr()
                                        )
                                      ),
                                      fluidRow(
                                        column(width = 3,
                                               uiOutput("trackImage")
                                        ),
                                        column(width = 6,
                                               uiOutput("trackText")
                                        )
                                      ),
                                      hr(),
                                      br(),
                                      uiOutput("yt"),
                                      hr(),
                                      uiOutput("abText"),
                                      tags$h6("Last Updated May 2021"),
                                      uiOutput("stick")
                                      
                            )#main panel end
                          )#sidebar layout end
                 ),#tab panel end
                 tabPanel("Army of Mice",
                          img(src='/images/aom-circle-cropped.png', align = "right",width = 90),
                          sidebarLayout(
                            sidebarPanel(width = 4,
                                         #HTML('<iframe style="border: 0; width: 350px; height: 470px;" src="https://bandcamp.com/EmbeddedPlayer/album=1982307016/size=large/bgcol=ffffff/linkcol=7137dc/tracklist=false/transparent=true/" seamless><a href="https://armyofmice.bandcamp.com/album/tracing-raindrops-album">Tracing Raindrops - Album by Army of Mice</a></iframe>'),
                                         uiOutput("bandcamp"),
                                         hr(),
                                         fluidRow(
                                           column(width = 2,
                                                  uiOutput("emailLink2"),
                                                  br(),
                                                  uiOutput("instagram2")
                                           ),
                                           column(width = 1,
                                                  uiOutput("youtubeLink2"),
                                                  br(),
                                                  uiOutput("kofiLink2")
                                           )
                                         ),
                                         hr()
                            ),
                            ####mainpanel 2 ####
                            mainPanel(width = 8,
                                      fluidRow(
                                        column(width = 4,
                                               uiOutput("tracingRain")
                                               ),
                                        column(width = 4,
                                               tags$h2("Tracing Raindrops album available on Bandcamp"))
                                      ),
                                      hr(),
                                      fluidRow(
                                        column(width = 4,
                                               uiOutput("wildFlower")
                                               ),
                                        column(width = 4,
                                               tags$h2("Wildflower EP available on Bandcamp"))
                                        ),
                                      hr(),
                                      uiOutput("abText2"),
                                      tags$h6("Last Updated May 2021"),
                                      uiOutput("stick2")
                                      
                            )#main panel end
                          )#sidebar layout end
                 )#tab panel end
)#navbar end

server <- function(input, output) {
    
    currentWav <- reactiveValues()
    currentWav$wav <- list()
    ##slot 1 - the stereo wav
    ##slot 2 - left channel
    ##slot 3 - righ channel
    
    processedWav <- reactiveValues()
    processedWav$leftRev <- vector()
    processedWav$RightRev <- vector()
    
    plotTitle <- reactiveValues()
    plotTitle$title <- vector()
    
    observeEvent(input$zgo,{
        plotTitle$title <- input$fileChooser
        infile <- input$fileChooser
        print(infile)
        fullpath <- paste0(wd,"/www/",infile)
        print(paste0("Full path is ",fullpath))
        finfo <- file.size(fullpath)
        finfo <- as.character(round(finfo/1000000,2)) #sizec in mb
        print(finfo)
        
        #read wav using Tuner
        testwav <- readMP3(paste0(wd,"/www/",infile))
        fn <- unlist(strsplit(input$fileChooser,"[.]"))
        print(paste0("Filename: ",fn[1]))
        #look-up file in metadata table
        metadata.index <- grep(infile,metadata[,1])
        print(paste0("metadata index: ",metadata.index))
        
        output$trackImage <- renderUI({
            print(metadata[metadata.index,2])
            i <- HTML(paste0('<img src="images/',metadata[metadata.index,2],'" alt="showreelImage">'))
            i
        })
        
        output$trackText <- renderUI({
            #HTML(paste0('<h3>',metadata[metadata.index,3],'</h3>'))
            j <- paste0("http://www.omdbapi.com/?i=",metadata[metadata.index,4],"&apikey=3dc38b3")
            print(j)
            film <- fromJSON(j)
            film.plot <- film$Plot
            film.director <- film$Director
            film.actors <- film$Actors
            HTML(paste0('<h3>',
                        film.plot,
                        '<br><hr>',
                        'Director: ',
                        film.director,
                        '<br><hr>',
                        'Actors: ',
                        film.actors,
                        '</h3>'))
        })
        
        output$trackDesc <- renderUI({
            t <- metadata[metadata.index,3]
            HTML(paste0('<h3>',t,'</h3>'))
            
        })
        
        output$zplayer <- renderUI({
            
            z <- HTML(paste0('<audio controls autoplay>
                       <source src="',infile,'" type="audio/mpeg">
                       Your browser does not support the audio element. </audio>'))
            z
        })
        
        output$finfo <- renderText({
            sr <- testwav@samp.rate
            bitdep <- testwav@bit
            timeLength <- round(length(testwav)/sr,1)
            paste0(infile," = ",
                   finfo,
                   " mb",
                   "\n",
                   "sample rate = ",
                   sr," Hz","\n",
                   "bit depth = ",bitdep," bits","\n",
                   "length = ",timeLength," s")
        })
        
        output$wavPlot <- renderPlot({
            plot(testwav,
                 col = cols[3],
                 main = plotTitle$title)
        })
        
        output$yt <- renderUI({
            yt <- metadata[metadata.index,5]
            HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/',yt,'" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
        })
        
        
    })
    
    output$emailLink <- renderUI({
        HTML('<a href="mailto:mark.reuter@googlemail.com"><img src="/images/gmail.png" alt="email" width=50></a>')
    })
    
    output$instagram <- renderUI({
        HTML('<a href="https://www.instagram.com/abominablemusic/" target="_blank"><img src="/images/instagram.png" alt="instagram" width=50></a>')
    })
    
    output$youtubeLink <- renderUI({
        HTML('<a href="https://www.youtube.com/c/abominablemusic" target="_blank"><img src="/images/youtube.png" alt="youtube" width=50></a>')
    })
    
    output$kofiLink <- renderUI({
        HTML('<a href="https://ko-fi.com/abominablemusic" target="_blank"><img src="/images/kofi.png" alt="kofi" width=50></a>')
    })
    
    output$emailLink2 <- renderUI({
      HTML('<a href="mailto:mark.reuter@googlemail.com"><img src="/images/gmail.png" alt="email" width=50></a>')
    })
    
    output$instagram2 <- renderUI({
      HTML('<a href="https://www.instagram.com/abominablemusic/" target="_blank"><img src="/images/instagram.png" alt="instagram" width=50></a>')
    })
    
    output$youtubeLink2 <- renderUI({
      HTML('<a href="https://www.youtube.com/c/abominablemusic" target="_blank"><img src="/images/youtube.png" alt="youtube" width=50></a>')
    })
    
    output$kofiLink2 <- renderUI({
      HTML('<a href="https://ko-fi.com/abominablemusic" target="_blank"><img src="/images/kofi.png" alt="kofi" width=50></a>')
    })
    
    output$abText <- renderUI({
        HTML('<img src="/images/abominable-fogle.png" alt="abominable" width=300>')
    })
    
    output$stick <- renderUI({
        HTML('<img src="/images/StickStack1.gif" alt="stick" width=30>')
    })
    
    output$bandcamp <- renderUI({
      w <- 350
      HTML(paste0('<iframe style="border: 0; width: ',w,'px; height: 470px;" src="https://bandcamp.com/EmbeddedPlayer/album=1982307016/size=large/bgcol=ffffff/linkcol=7137dc/tracklist=false/transparent=true/" seamless><a href="https://armyofmice.bandcamp.com/album/tracing-raindrops-album">Tracing Raindrops - Album by Army of Mice</a></iframe>'))
    })
    
    output$tracingRain <- renderUI({
      HTML('<a href="https://armyofmice.bandcamp.com/album/tracing-raindrops-album" target="_blank"><img src="/images/tracingRain.jpg" alt="Tracing Raindrops" width=300></a>')
    })
    
    output$wildFlower <- renderUI({
      HTML('<a href="https://armyofmice.bandcamp.com/album/wildflower-ep" target="_blank"><img src="/images/wildFlower.jpg" alt="Wildflower" width=300></a>')
    })
  
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)