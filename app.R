##### libraries ####
library(shiny); library(shinyjs); library(shinyalert); library(dplyr); 
library(DT); library(shinyBS); library(data.table); library(markdown);
library(rio);
source('functions.R');

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
debug <- file.exists('.debug');
gitlink <- 'https://github.com/bokov/anyfile'
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
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico")
              ,includeScript("ga.js"))
   ,includeCSS('df.css')
   ,useShinyjs()
   ,useShinyalert()
   ,fluidRow(
     # + Title etc. ####
     column(1,img(src='sitelogo_color.png',width='45px'),br()
            ,if(debug) actionButton('debug','Debug') else c())
     ,column(2,h3("AnyFile",id='apptitle')
             ,"A resource for researchers")
     ,column(8,em('A free, open-source webapp by Alex Bokov, PhD'
                  ,'made possible by support from'
                  ,'NIH/NCATS UL1TR001120 (IIMS) and the'
                  ,'Long School of Medicine KL2 Award.'
                  ,'Makes use of the',a('rio library'
                                ,href='https://github.com/leeper/rio')
                  ,' by Thomas J. Leeper, PhD.'
                  ,' Source code available on',a('GitHub',href=gitlink
                                               ,target='_blank')))
     ,column(1))
     ,fluidRow(# + File Upload ####
               column(1)
               ,column(10,hr()
                      ,p("Sometimes you are provided data in an unfamiliar"
                         ," format, or in a format that needs software"
                         ," that you do not own, or even a format that is"
                         ," completely unknown. ", tags$b('AnyFile')
                         ," supports over a "
                         ," dozen of the most common data formats and will"
                         ," do its level best to find a way to read your"
                         ," data, then give you a choice of formats"
                         ," into which you can convert it.")
                      ,fileInput("infile"
                                 ,div("Choose a file to upload and convert to a"
                                      ," format of your choice" #,HTML('&nbsp;')
                                      )
                                 ,multiple = FALSE,width = '100%'
                                 ))
               ,column(1))
   # + File Convert ####
   ,hidden(fluidRow(column(1)
                    ,column(10,hr()
                           ,p("Some data formats (e.g. Excel and OpenOffice)"
                              ," may contain multiple tables of data. Here you"
                              ," are being asked which one to import in such a"
                              ," case. If the one you specify is not available "
                              ,tags$b('AnyFile')," will go back to importing"
                              ," the first one it finds.")
                           ,numericInput('which',span('Which sheet or table? '
                                         ,'(if in doubt, you can leave it as-is'
                                         ,' and just click the button below) '
                                         #,HTML('&nbsp;')
                                         )
                                         ,min=1,max=20,value=1,width='100%')
                           ,br()
                           ,actionButton('import','Interpret File')
                           ,hidden(div(hr()
                                      ,selectInput('saveas','Convert to:'
                                                   ,choices = exportformats
                                                   ,selected = 'csv')
                                      #,HTML('&nbsp;')
                                      ,actionButton('convert','Convert File')
                                      ,hidden(
                                        span(downloadButton(
                                          'download','Download Converted File')
                                          ,id='downloaddiv')),id='convertdiv'))
                           )
                    ,column(1),id='importdiv'))
   ,hidden(fluidRow(column(1),column(10,hr(),bsCollapsePanel(span("Preview"
                                                       ,icon('angle-down'))
                                                  ,dataTableOutput('preview')))
                    ,column(1),id='previewrow'))
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
    readfile <- try(try_import(rv$infile,which=input$which),silent=TRUE);
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

