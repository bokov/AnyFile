##### libraries ####
library(shiny); library(shinyjs); library(shinyalert); library(dplyr); 
library(DT); library(rio); library(rio.db); library(csvy);

##### global settings ####
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

neverexport <- c('clipboard','sas7bdat')
exportformats <- setdiff(gsub('.export.rio_'
                              ,'',grep('^\\.export\\.rio_'
                                       ,methods(.export),value=TRUE))
                         ,neverexport);
# UI ####
ui <- fluidPage(
   # + Head ####
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
   ,includeCSS('df.css')
   ,useShinyjs()
   ,useShinyalert()
   ,fluidRow(
     # + Title etc. ####
     column(1,img(src='sitelogo_color.png',width='45px'),br()
            ,if(!shinyapps) actionButton('debug','Debug') else c())
     ,column(2,h3("AnyFile",id='apptitle')
             ,"A resource for researchers")
     ,column(5
             # + File Upload ####
             ,fileInput("infile"
                        ,div("Choose a file to upload and convert to a format"
                             ," of your choice")
                        ,multiple = FALSE,width = '400px'
                        )
             # + File Convert ####
             ,hidden(div(hr()
                         ,numericInput('which','Which sheet or table?'
                                       ,min=1,max=20,value=1)
                         ,br()
                         ,actionButton('import','Interpret File')
                         ,id='importdiv'))
             ,id='infile_ui')
   )
  ,fluidRow(column(3,' ')
            # + File Download ####
            ,column(5,hidden(div(hr()
                                 ,selectInput('saveas','Convert to:'
                                              ,choices = exportformats
                                              ,selected = 'csv')
                                  ,br()
                                  ,actionButton('convert','Convert File')
                                  ,id='convertdiv'))
                    ,hidden(div(hr()
                                ,downloadButton('download'
                                               ,'Download Converted File')
                                ,id='downloaddiv'))
                    )
            )
)

# Server ####
server <- function(input, output) {
  # reactive values ####
  rv <- reactiveValues(disclaimerAgreed=F);
  # user agreement ####
  shinyalert('User Agreement',text=helptext$disclaimer
             ,html=T,confirmButtonText = 'I agree',confirmButtonCol = hcol
             ,className = 'dfDisclaimer',closeOnEsc = F
             ,animation = 'slide-from-top'
             ,callbackR = function() {
               rv[['disclaimerAgreed']] <- T;
               show('infile')});

  # record file info ####
  observeEvent(c(input$infile,rv$disclaimerAgreed,input$which),{
    req(input$infile$datapath,rv$disclaimerAgreed);
    rv$infile <- input$infile$datapath;
    rv$infilename <- input$infile$name;
    show('importdiv');
    hide('convertdiv');hide('downloaddiv');
    });
  
  # change in output format ####
  observeEvent(input$which,hide('downloaddiv'));

  # read with rio ####
  observeEvent(input$import,{
    readfile <- try(rio::import(rv$infile,which=input$which));
    if(is(readfile,'try-error')){
      for(ii in tryformats){
        readfile <- try(rio::import(rv$infile,format=ii,which=input$which));
      if(is(readfile,'try-error')){
        readfile <- try(rio::import(rv$infile,format=ii,which=1));
        if(!is(readfile,'try-error')){
          warning('Specified table does not exist in file, '
                  ,'extracting first available table instead');
          break;
          }
      } else break;
      }
      }
    if(is(readfile,'try-error')){
      shinyalert('You have discovered an (as yet) unsupported file',
                'We would appreciate it if you would submit a bug 
                report to https://github.com/bokov/AnyFile/issues/new
                so we can figure out a way to make this app work for
                your file as well.
               ',type='warning')
      } else {
        rv$readfile <- readfile;
        show('convertdiv');
        hide('downloaddiv')
        }
    });
  
  # convert with rio ####
  observeEvent(input$convert,{
    out <- try(export(rv$readfile
                      ,file = tempfile(fileext = paste0('.',input$saveas))
                      ,format=input$saveas));
    if(is(out,'try-error')) shinyalert('Error converting file',as.character(out))
    else {
      fnicename <- paste0(tools::file_path_sans_ext(rv$infilename)
                          ,'.',input$saveas);
      output$download <- downloadHandler(filename=fnicename
                                         ,content=function(con) {
                                           file.copy(out,con)});
      show('downloaddiv');
    }
  })
  
  # render datatable #### 
  
  # debug ####
  observeEvent(input$debug,{
    browser();
  });
  
}

# Run the application ####
shinyApp(ui = ui, server = server)

