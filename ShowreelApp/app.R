####Showreel App####
####Mark A Reuter####
library(shiny)
library(shinythemes)
library(tuneR)
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
                     full.names = FALSE)
print(reimagined)

ui <- fluidPage(theme = shinytheme("darkly"),
    ## Application title
    titlePanel("abominable | composer | showreel"),
    img(src='/images/circle-cropped.png', align = "right",width = 200),
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
                                  label = "play",
                                  icon = NULL,
                                  width = NULL),
                     br(),
                     br(),
                     htmlOutput("zplayer"),
                     verbatimTextOutput("finfo"),
                     hr()
        ),
        
        # Show a plot of the generated distribution
        mainPanel(width = 9,
                  fluidRow(
                      column(width = 9,
                             plotOutput("wavPlot")
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
        )
    )
)

server <- function(input, output) {
    
    currentWav <- reactiveValues()
    currentWav$wav <- list()
    ##slot 1 - the stereo wav
    ##slot 2 - left channel
    ##slot 3 - righ channel
    
    processedWav <- reactiveValues()
    processedWav$leftRev <- vector()
    processedWav$RightRev <- vector()
    
    observeEvent(input$zgo,{
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
            HTML(paste0('<h3>',metadata[metadata.index,3],'</h3>'))
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
                 main = input$fileChooser)
        })
        
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
