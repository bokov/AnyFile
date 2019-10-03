library(shiny); library(shinyjs); library(shinyalert); library(dplyr); 
library(DT); library(rio); library(rio.db);

# global settings
shinyapps <- file.exists('.shinyapps');
source('www/docs/helptext.R');
hcol <- '#008c99';
formats <- gsub('.import.rio_','',grep('^\\.import\\.rio_'
                                       ,methods(.import),value=TRUE));
tryfirst <- intersect(c('xlsx','ods','xls','xml','rdata','r','json'
                        ,'html','yml','csvy'),formats);
trylast <- 'dat';
nevertry <- c('clipboard','fortran','csv','csv2','psv','fwf','txt',trylast);
tryother <- setdiff(formats,c(tryfirst,nevertry));
tryformats <- c(tryfirst,tryother,trylast);

exportformats <- gsub('.export.rio_','',grep('^\\.export\\.rio_'
                                             ,methods(.export),value=TRUE));

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
             ,"A resource for researchers")
     ,column(4
             ,fileInput("infile"
                        ,div("Choose a file to upload and convert to a format"
                             ," of your choice")
                        ,multiple = FALSE,width = '400px'
                        )
             ,hidden(span(actionButton('import','Interpret File')
                          ,'Which sheet or table?'
                          ,numericInput('which','',min=1,max=20,value=1
                                        ,width = '5vw'),id='importspan'))
             ,id='infile_ui')
     ,column(1,id='helpDebug'
             ,span(id='hInfile',icon('question-circle'))
             ,if(!shinyapps) actionButton('debug','Debug') else c()
     )
   )
  ,fluidRow(column(3,' ')
            ,column(4,hidden(downloadButton('download','Download As...'))
                    ,hidden(selectInput('saveas','Format:',choices = exportformats
                                        ,selected = 'csv')))
            )
)

# Define server logic 
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
    rv$infilename <- input$infile$name;
    show('importspan');
    });
  
  # try reading the file with rio
  observeEvent(input$import,{
    readfile <- try(rio::import(rv$infile,which=input$which));
    if(is(readfile,'try-error')){
      for(ii in tryformats) readfile <- try(rio::import(rv$infile,format=ii
                                                        ,which=input$which));
      if(!is(readfile,'try-error')) break;
    }
    if(is(readfile,'try-error')){
      shinyalert('You have discovered an (as yet) unsupported file',
                 'We would appreciate it if you would submit a bug 
                  report to https://github.com/bokov/AnyFile/issues/new
                  so we can figure out a way to make this app work for
                  your file as well.
                 ',type='warning')
    } else rv$readfile <- readfile;
  })
  
  # display save widgets
  observeEvent(rv$readfile,{
    show('saveas');
    hide('download');
  })
  
  observeEvent(input$saveas, if(!is.null(rv$readfile)){
    out <- try(export(rv$readfile
                  ,file = tempfile(fileext = paste0('.',input$saveas))
                  ,format=input$saveas));
    if(is(out,'try-error')) shinyalert('Error converting file',as.character(out))
    else {
      fnicename <- paste0(tools::file_path_sans_ext(rv$infilename)
                          ,'.',input$saveas);
      output$outdownload <- downloadHandler(filename=fnicename
                                            ,content=function(con) {
                                              file.copy(out,con)});
      show('download');
    }
  })
  
  # render datatable
  
  # Testing ----
  observeEvent(input$debug,{
    browser();
  });
  
}

# Run the application 
shinyApp(ui = ui, server = server)

