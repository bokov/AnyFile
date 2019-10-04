##### libraries ####
library(shiny); library(shinyjs); library(shinyalert); library(dplyr); 
library(DT); library(shinyBS); library(data.table); library(markdown);
library(rio);

requireNamespace('readxl');
requireNamespace('feather');
requireNamespace('fst');
requireNamespace('rmatio');
requireNamespace('jsonlite');
requireNamespace('readODS');
requireNamespace('xml2');
requireNamespace('yaml');
requireNamespace('pzfx');
requireNamespace('csvy');
##### global settings ####
shinyapps <- file.exists('.shinyapps');
source('www/docs/helptext.R');
hcol <- '#008c99';
formats <- gsub('.import.rio_','',grep('^\\.import\\.rio_'
                                       ,methods(.import),value=TRUE));
tryfirst <- intersect(c('xlsx','ods','xls','xml','rdata','r','json'
                        ,'html'),formats);
trylast <- c('dat','csvy','yml');
nevertry <- c('clipboard','fortran','csv','csv2','psv','fwf','txt','eviews',trylast);
tryother <- setdiff(formats,c(tryfirst,nevertry));
tryformats <- c(tryfirst,tryother,trylast);

neverexport <- c('clipboard','sqlite');
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
  ,hidden(fluidRow(column(12,bsCollapsePanel(span("Preview"
                                          ,icon('angle-down'))
                                     ,dataTableOutput('preview')))
            ,id='previewrow'))
)

# Server ####
server <- function(input, output, session) {
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
    hide('convertdiv');hide('downloaddiv');hide('previewrow');
    });
  
  # change in output format ####
  observeEvent(input$saveas, hide('downloaddiv'));

  # read with rio ####
  observeEvent(input$import,{
    readfile <- try(import(rv$infile,which=input$which),silent=TRUE);
    if(is(readfile,'try-error')){
      for(ii in tryformats){
        message('Trying format: ',ii);
        readfile <- try(import(rv$infile,format=ii,which=input$which)
                        ,silent=TRUE);
        if(is(readfile,'try-error')){
          readfile <- try(import(rv$infile,format=ii,which=1),silent=TRUE);
          if(!is(readfile,'try-error')){
            warning('Specified table does not exist in file, '
                    ,'extracting first available table instead');
            updateNumericInput(session,inputId = 'which',value=1)
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
        show('convertdiv'); show('previewrow')
        hide('downloaddiv');
        }
    });
  
  # convert with rio ####
  observeEvent(input$convert,{
    out <- setNames(rv$readfile,nm=gsub('\\.','_'
                                            ,make.names(names(rv$readfile)
                                                        ,unique = TRUE)));
    # hack to avoid errors on pzfx
    if(input$saveas == 'pzfx') for(ii in names(out)){
      if(inherits(out[[ii]],c('character','factor'))){
        out[[ii]] <- as.numeric(factor(out[[ii]]));
        showNotification(sprintf('Column %s converted to numeric',ii)
                         ,type='warning');
      }
    }
    result <- try(export(out
                      ,file = tempfile(fileext = paste0('.',input$saveas))
                      ,format=input$saveas));
    if(is(result,'try-error')) shinyalert('Error converting file'
                                          ,as.character(result))
    else {
      fnicename <- paste0(tools::file_path_sans_ext(rv$infilename)
                          ,'.',input$saveas);
      output$download <- downloadHandler(filename=fnicename
                                         ,content=function(con) {
                                           file.copy(result,con)});
      show('downloaddiv');
    }
  })
  
  # render datatable #### 
  output$preview <- renderDataTable({
    DT::datatable(rv$readfile,extensions = 'Scroller'
                  ,autoHideNavigation=T,rownames=F,fillContainer=T
                  ,options=list(processing=T,searching=F,scroller=T
                                ,scrollx='100%',scrolly='20vh'
                                ,dom='Bfrtip'
                                ))
    },server=FALSE);
  # debug ####
  observeEvent(input$debug,{
    browser();
  });
  
}

# Run the application ####
shinyApp(ui = ui, server = server)

