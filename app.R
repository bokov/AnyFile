library(shiny); library(shinyjs); library(shinyalert); library(dplyr); 
library(DT); library(rio); library(rio.db);

# global settings
shinyapps <- file.exists('.shinyapps');
source('www/docs/helptext.R');
hcol <- '#008c99';

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
   ,includeCSS('df.css')
   ,useShinyjs()
   ,useShinyalert()
   ,fluidRow(
     column(1,img(src='sitelogo_color.png',width='45px'))
     ,column(2,h3("AnyFile",id='apptitle')
             # ,"A resource for researchers")
     ,column(4
             ,fileInput("infile"
                        ,div("Choose a file to upload and convert to a format"
                             ," of your choice")
                        ,multiple = FALSE,width = '400px'
                        )
             ,id='infile_ui')
     ,column(1,id='helpDebug'
             ,span(id='hInfile',icon('question-circle'))
             ,if(!shinyapps) actionButton('debug','Debug') else c()
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  rv <- reactiveValues(disclaimerAgreed=F);
  # user agreement
  shinyalert('User Agreement',text=helptext$disclaimer
             # user agreement ----
             ,html=T,confirmButtonText = 'I agree',confirmButtonCol = hcol
             ,className = 'dfDisclaimer',closeOnEsc = F
             ,animation = 'slide-from-top'
             ,callbackR = function() {
               rv[['disclaimerAgreed']] <- T;
               show('infile')});

  # record file info
  observeEvent(c(input$infile,rv$disclaimerAgreed),{
    req(input$infile$datapath,rv$disclaimerAgreed);
    rv$infile <- input$infile$datapath;
    rv$infilename <- input$infile$name;});
  
  # try reading the file with rio
  
  # if error, shinyalert
  
  # display save widgets
  
  # render datatable
  
  # Testing ----
  observeEvent(input$debug,{
    browser();
  });
  
}

# Run the application 
shinyApp(ui = ui, server = server)

