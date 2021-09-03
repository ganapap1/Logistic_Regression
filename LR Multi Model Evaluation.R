# Logistic Regression - Generate Multiple model and Model evaluation

#First load required library
library(shiny)
library(shinydashboard)    # for Dashboard
library(shinyWidgets)      # for radio button widgets
library(shinyalert)        # for alert message very nice format
library(dplyr)             # select functions are covered in the library
library(plyr)              # empty() function is from this package
library(DT)                # for using %>% which works as a pipe in R code for filter etc
library(shinyjs)           # to perform common useful JavaScript operations in Shiny apps
library(plotly)            # to prepare interactive graphs/ charts
library(tidyverse)         # used to make pivot from dataset
library(ggplot2)           # to draw or take advantage of ggplot functions
library(ggrepel)           # to draws a rectangle underneath the text, making it easier to read
library(caret)             # logistic regression related package, 
library(InformationValue)  # is to generate KS plot and ks stat



#################################################################################
#style function for Action button default 50px and width 180px; you can change
#################################################################################
styleButtonBlue<- function(xheight="50px",xwidth="180px",xcolor='#4682B4'){
  paste("white-space: normal;
                        text-align:center;
                        color: #ffffff; 
                        background-color:",xcolor,";",
                        "border-color: #ffffff;
                        border-width:2px;
                        height:",xheight,";
                        width:",xwidth,";
                        font-size: 13px;")
}



##################################################################
#shinyInput function is used to add action button in DT::datatable
##################################################################
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}


##################################################################
#URL links are defined here and used on the dashboard
##################################################################
urlref5 <- a("Thanks to Cybernetic and Breck, Data Scientist at UH Cancer Center for the
             valuable contribution at https://stackoverflow.com/ on
             'R how to visualize confusion matrix using the caret package'.  I made marginal improvement by adding totals all round matrix and column names",
    href = "https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package/42940553#42940553"
  )


urlvif <- a(HTML(paste('<h5><b>',"Reference: Statistics How To, 'Statistics for the rest of us!'",'</b><br><h5>')),
            href="https://www.statisticshowto.com/variance-inflation-factor/")



##################################################################
#These two functions are formatting percentage 
##################################################################
fnpercentZeroDigits <- function(x, digits = 0, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

fnpercent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}



##################################################################
#Dashboard header starts here
##################################################################
  header<- dashboardHeader(
    title = "Logistic Regression Multiple Model Statistics",titleWidth = '500px',
    tags$li(tags$style(".leftAlign{float:left;}"),class = "dropdown"),
    tags$li(
      a(
        splitLayout(
          cellWidths = c("50%", "50%"),
          actionButton(inputId = "Previous", label = icon("arrow-left")),
          actionButton(inputId = "Next", label = icon("arrow-right"))
        ),
        href = NULL,
        style = "cursor: pointer;height:15px;"
      ),
      class = "dropdown"
    )
  )
  
##################################################################
#Dashboard sidebar starts here
##################################################################
sidebar <- dashboardSidebar(
  # Remove the sidebar toggle element
  tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
  minified = TRUE,  #this option will completely close the side bar  and expand the header text
  id = 'msidebarid',
  collapsed = TRUE,
  useShinyjs(),
  sidebarMenu(id = "tabs",
              menuItem('Overview',
                       tabName = 'taboverview',
                       icon = icon('line-chart')),
              menuItem('Data Processing',
                       tabName = 'tabdata',
                       icon = icon('line-chart')),
              menuItem('Model Evaluation',
                       tabName = 'tabModelEval',
                       icon = icon('line-chart')),
              menuItem('Multi Model Table)',
                       tabName = 'tabsmodelstbl',
                       icon = icon('line-chart')),
              menuItem('Multi Model Plotly',
                       tabName = 'tabsmodelsplotly',
                       icon = icon('line-chart'))
  )#sidebar menu
)
  

##################################################################
#Dashboard Body starts here
##################################################################
  body <- dashboardBody(
    useShinyalert(),
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(
        tabName = "taboverview",
        box(
          id="box_overview",
          width = 12,
          height = '450px',
          #background = 'light-blue',
          title = 'Logistic Regression Overview, Evaluation and Reporting',
          #title = "Variable Importance Table",
          #HTML(paste('<h5><b><left>',"Select ID and other predictors to delete, if required")),
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          uiOutput("mybox455")
        )#box closure
      ),#tabitem closure
      tabItem(
        tabName = "tabdata",
        column(
          width = 12,
          height = '425px',
          align ='center',
          box(
            id = "mprocBox0",
            width = 4,
            height = '450px',
            align = "center",
            title = 'Data Process Menu',
            status = "warning",
            solidHeader = TRUE,
            collapsible = FALSE,
            br(), br(),
            actionButton(inputId = 'mDataUploadBtn',label = "Data Upload!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
            br(),
            actionButton(inputId = 'mshowtableBtn',label = "Review Dataset!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
            br(),
            actionButton(inputId = 'mFixDependentVarBtn',label = "Choose Dependent Variable!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
            br(),
            actionButton(inputId = 'mCleanseDataBtn',label = "Data Cleansing!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
            br(),
            actionButton(inputId = 'mFinalProcessBtn',label = "Build Models!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
            br(),
            downloadButton("mDownloadmydataBtn", "Download Dataset CSV", style = styleButtonBlue(xheight = '35px',xwidth = '200px'))
          ),#box closure
          tags$div(id = 'placeholder_MultiPurpose')
        )#column closure
        #) # box closure
      ),#tabitem closure
      tabItem(
        tabName = "tabModelEval",
        column(
          width = 3,
          height = '425px',
          align ='center',
          background ='black',
          HTML(paste('<h4><b>',"LR Model Evaluation",'<h5></b>')),
          actionBttn(inputId = "mLrModelRevise",label = "Model Revision",style = "pill",color = "warning"),
          
          HTML(paste('<h5><b>',"Goodness of Fit",'<h5></b>')),
          actionButton(inputId = 'mLikelihoodBtn',label = "Likelihood Ratio!",style = styleButtonBlue(xheight = '32px',xwidth = '200px')),
          actionButton(inputId = 'mPseudoRsqBtn',label = "Pseudo R2 Test!",style = styleButtonBlue(xheight = '32px',xwidth = '200px')),
          actionButton(inputId = 'mHosmerLBtn',label = "Hosmer & Lemeshow Test!",style = styleButtonBlue(xheight = '32px',xwidth = '200px')),
          
          HTML(paste('<h5><b>',"Statistical Test of Individual Predictors",'<h5></b>')),
          actionButton(inputId = 'mWaldTestBtn',label = "Wald Test!",style = styleButtonBlue(xheight = '32px',xwidth = '200px')),
          actionButton(inputId = 'mAnovaTestBtn',label = "ANOVA Test!",style = styleButtonBlue(xheight = '32px',xwidth = '200px')),
          actionButton(inputId = 'mVariableImpBtn',label = "Variable Importance!",style = styleButtonBlue(xheight = '32px',xwidth = '200px')),
          
          HTML(paste('<h5><b>',"Model Result Validation",'<h5></b>')),
          actionButton(inputId = 'mConfusionMtrxBtn',label = "Confusion Matrix!",style = styleButtonBlue(xheight = '32px',xwidth = '200px')),
          actionButton(inputId = 'mKSROCPlotBtn',label = "KS-Plot & ROC (AUC)!",style = styleButtonBlue(xheight = '32px',xwidth = '200px')),
          actionButton(inputId = 'mKFoldValidBtn',label = "K-fold Cross Validation !",style = styleButtonBlue(xheight = '32px',xwidth = '200px')),
          actionButton(inputId = 'mAICDevianceBtn',label = "AIC / Deviance!",style = styleButtonBlue(xheight = '32px',xwidth = '200px')),
          actionButton(inputId = 'mVarInflationBtn',label = "Variance Inflation Factor!",style = styleButtonBlue(xheight = '32px',xwidth = '200px'))
        ),#column closure
        column(
          width = 9,
          height = '425px',
          align ='center',
          tags$div(id = 'placeholder_ModelEval')
        )#column closure
      ),
      tabItem(
        tabName = "tabsmodelstbl",
        box(
          width = 12,
          height = '1000px',
          align = "center",
          box(
            id = "slidebarbox206",
            width = NULL,
            height = '100%',
            title = 'Filter Models using Sliderbar and Table',
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            tags$head(
              tags$style(
                "#muimultisliderplay{color:black; font-size:12px; font-style:normal;height: 450px;
overflow-y:scroll; max-height: 450px; background: light-blue;text-align: left;}"
              )
            ),
            uiOutput(outputId = "muimultisliderplay"),
            # tags$head(tags$style(
            #   HTML(
            #     "#muimultisliderplay {
            #     height: 350px; overflow-x: auto; overflow-y: auto; # ::I got from this site; Thank you: this scrolling http://www.itgo.me/a/4608602556927470679/r-shiny-dashboard-how-to-add-vertical-scrollbar-to-dashboard-sidebar
            #   }"
            #   ) # close HTML
            # ) # close tags$style
            # ) # close tags#Head
          ), #box closure slider input
          box(
            width = 12,
            height = '450px',
            title = HTML(paste0('Multiple Model Matrix',downloadButton("mDownloaddtdfBtn", "Download CSV", style = styleButtonBlue(xheight = '35px',xwidth = '200px')))),
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            uiOutput(outputId = "tblmultiID206")
          )#box closure
        ) # column closure
      ),#tabitem closure
      tabItem(
        tabName = "tabsmodelsplotly",
        box(
          width = 12,
          height = '500px',
          closable = FALSE,
          title = "Interactive Plotly - Multiple Model Review",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          plotlyOutput('plotmodelcompare', height = '385px')
        )#box closure
      ) #tabitem closure
    )#tabItems Closure
  ) # dashboardBody closure

  ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
  )
  
  
  


server <- function(input, output, session) {
  inserted <- c()
  slidercolrange <- -2
  
  disable("mshowtableBtn")
  disable("mFixDependentVarBtn")
  disable("mCleanseDataBtn")
  disable("mFinalProcessBtn")
  disable("mDownloadmydataBtn")
  

  #################################################
  #right left arrow for the next and previous tab  ::I got from this site; Thank you: https://stackoverflow.com/questions/44309328/generic-button-for-go-to-next-and-previous-tabitem-shiny
  #################################################
  global <- reactiveValues(tab_id = "")
  tab_id <- c( 'taboverview','tabdata','tabModelEval','tabsmodelstbl','tabsmodelsplotly')
  
  Current <- reactiveValues(
    Tab = "taboverview"
  )
  
  observeEvent(
    input[["tabs"]],
    {
      Current$Tab <- input[["tabs"]]
    }
  )
  
  observeEvent(
    input[["Previous"]],
    {
      tab_id_position <- match(Current$Tab, tab_id) - 1
      if (isTRUE(tab_id_position == 0)) tab_id_position <- length(tab_id)
      Current$Tab <- tab_id[tab_id_position]
      updateTabItems(session, "tabs", tab_id[tab_id_position]) 
    }
  )
  
  observeEvent(
    input[["Next"]],
    {
      tab_id_position <- match(Current$Tab, tab_id) + 1
      if (isTRUE(tab_id_position > length(tab_id))) tab_id_position <- 1
      Current$Tab <- tab_id[tab_id_position]
      updateTabItems(session, "tabs", tab_id[tab_id_position]) 
    }
  )
  
  ###### end of code for go next and previous
  
  
  
##########################################################################
#Function to remove box or column present on the screen at the placeholder
##########################################################################
  removeRightBox <- function(x){
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted[length(inserted)])
    )
    inserted <<- inserted[-length(inserted)]
  }
  
  
  ###################################################################
  #There is a placeholder_MultiPurpose on UI, 
  #from here, every step or action will have 3 steps; 
  #1- Insert UI with observeEvent; 
  #2- Output in that inserted UI; 
  #3- Function if any required to calculate output 
  ###################################################################
  vmy <- reactiveValues(mydata=NULL,lr_models=NULL,down_train=NULL,up_train=NULL,
                        trainData = NULL, testData = NULL,data_1=NULL,
                        frame2 = NULL,frame2_ = NULL)
  
  observeEvent(input$mDataUploadBtn,{
    enable("mgetfileclick")
    removeRightBox()
    btn <- input$mDataUploadBtn
    id <- paste0('txt', btn)
    
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        box(
          id = "mprocBox1",
          width = 4,
          height = '450px',
          align = "center",
          title = "Uplaod Dataset",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          
          # Input: Select a file ----
          fileInput("file",
                    label = "Select: csv,txt, xls, xlsx, rds ",
                    multiple = FALSE,
                    accept = c("text/csv/txt/Excel",
                               "text/comma-separated-values,text/plain/excel",
                               ".csv",".txt",".xls",".xlsx",".rds")),
          
          # Horizontal line ----
          #tags$hr(),
          column(
            width = 5,
            offset = 1,
            align = "left",
            fluidRow(
              # Input: Checkbox if file has header ----
              checkboxInput("header", "Header", TRUE),
              
              # Input: Select separator ----
              radioButtons("sep", "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ","),
            )
          ),
          column(
            width = 5,
            offset = 0,
            align = "left",
            fluidRow(
              br(),
              br(),
              # Input: Select quotes ----
              radioButtons("quote", "Quote",
                           choices = c(None = "",
                                       "Double Qot." = '"',
                                       "Single Qot." = "'"),
                           selected = '"')
            )
          ),
          
          tags$hr(),
          column(
            width = 12,
            align="center",
            useShinyjs(),
            br(),
            actionButton(inputId = "mgetfileclick",label = "Get Data!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
            textOutput(outputId = 'mfileimportmsg')
          )
        )
      )
    )
    inserted <<- c(id, inserted)
  })
  

  observeEvent(input$mgetfileclick,{
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse, select and import ...!", type = "error")
      return()
    }
    enable("mshowtableBtn")
    enable("mDownloadmydataBtn")
    disable("mFixDependentVarBtn")
    disable("mFinalProcessBtn")
    
    #### file import code start
    ext <- tools::file_ext(input$file$name)
    
    if (ext != "csv" & ext !='rds' & ext != 'xlsx' & ext != 'xlsx'){
      shinyalert("Oops!", "valid files are csv, txt, excel and rds only", type = "error")
      return()
    }
    else if (ext == "rds"){
      vmy$mydata <- as.data.frame(readRDS(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "xls" || ext == 'xlsx'){
      vmy$mydata <- as.data.frame(readxl::read_excel(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "csv" || ext == 'txt'){
      tryCatch({            #avoid rscript showing error initially before execution,Skipping error in for-loop ::I got from this site; Thank you:https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop
        
        vmy$mydata <- as.data.frame(
          read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote
                   )
        )
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
    }
    #### file import code End
    
    #### delete NA rows code start
    vmy$mydata <- na.omit(vmy$mydata)
    vmy$mydata <- vmy$mydata[complete.cases(vmy$mydata), ]
    
    #### create datatype df to use in cleansing phase
    fncreatedftype()
    output$mfileimportmsg<-renderText({
      "Done - uploaded"
    })
    disable("mgetfileclick")
    
    updateNumericInput(session,inputId = "mVarCountFrom",min = 2,max = ncol(vmy$mydata)-1)
    disable("maddmodelYorN")
  })
  
  
  fncreatedftype <- function(){
    vmy$df_types <- data.frame("col_types" = unlist(lapply(vmy$mydata, typeof)))
    vmy$df_types$Var_name <- rownames(vmy$df_types)
    row.names(vmy$df_types) <- NULL
    vmy$df_types <-vmy$df_types %>% dplyr::select(-col_types, everything())
  }
  
  
  
  
  #####################################################################################
  # Show data table as such
  #####################################################################################
  observeEvent(input$mshowtableBtn,{
    removeRightBox()
    btn <- input$mshowtableBtn
    id <- paste0('txt', btn)
    disable("maddmodelYorN")
    enable("mFixDependentVarBtn")
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        box(
          width = 8,
          height = '450px', 
          title ="Dataset", 
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          autoWidth = FALSE,
          DT::dataTableOutput('mdatatable', height = '375px',width = '100%'),
          tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}'))
        ) #box closure
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
  output$mdatatable <- DT::renderDataTable({
    DT::datatable(vmy$mydata,  
                  rownames = FALSE,
                  editable = FALSE,
                  selection = list(mode = "single", selected = c(1), target = 'row'),
                  fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(
                    lengthMenu = list(c(10, 25, 50,-1), c('10', '25','50' ,'All')),
                    paging = TRUE,
                    lenthChange=TRUE,
                    searching = FALSE,
                    fixedColumns = FALSE,
                    ordering = FALSE,
                    initComplete = htmlwidgets::JS(
                      "function(settings, json) {",
                      paste0("$(this.api().table().container()).css({'font-size': '", "12px", "'});"),
                      "}")
                  ),
                  class ='cell-border stripe compact white-space: nowrap' #::I got from this site; Thank you this multiple classes: https://rstudio.github.io/DT/
    ) 
  })
  
  
  
#############################################################################
# here you get option to delete columns that are not required, for example ID  
#############################################################################
  observeEvent(input$mCleanseDataBtn,{
    enable("mFinalProcessBtn")
    removeRightBox()
    btn <- input$mCleanseDataBtn
    id <- paste0('txt', btn)
    
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        box(
          id = "mprocBox2",
          width = 4,
          height = '450px',
          align = "center",
          title = "Data Cleansing",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          DT::dataTableOutput("dt",height = '275px'),
          tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}')),
          useShinyjs(),
          extendShinyjs(text = paste0("shinyjs.resetDTClick = function() { Shiny.onInputChange('dt_cell_clicked', null); }"),functions = c('foo','bar')),
          textOutput("mselectedvariable"),
          actionButton(inputId = 'mbtndelete',label = "Delete Selected Variable!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
          br(),
          HTML(paste("Multi model table might be having this variable as part of predictors.", 
          "If so all those rows will also be deleted. Better you reprocess multiple models..!"))
         )
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
  
  
  output$dt <- DT::renderDataTable({
    DT::datatable(vmy$df_types,
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = FALSE,
                  selection = list(mode = "single", target = 'row'),
                  fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(dom = 't',ordering=F, pageLength = -1,class="compact",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#808080', 'color': '#fff'});",   #::I got from this site; Thank you: JS from https://stackoverflow.com/questions/34009922/how-to-format-a-shiny-rendertable
                                   "}")
                  ) 
    )
    
  })
  
  ### delete selected column
  ### this is warning messge for deleting
  observeEvent(input$mbtndelete,{
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete variable:",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    temp <- select(vmy$mydata,-vmy$df_types[input$dt_cell_clicked$row,1])
    vmy$mydata <- temp
    removeModal()
    temp2 <- subset(vmy$df_types, Var_name!=vmy$df_types[input$dt_cell_clicked$row,1] )
    vmy$df_types <- temp2
    fnTestTrainData(vmy$mydata,input$mdependvar)
    
    xxx <- paste0("vmy$down_train$",input$mdependvar)
    f2 <- as.formula(paste(text=xxx,"~", "."))
    vmy$fullMod <- glm(f2, family = binomial(link = logit), data=vmy$down_train)  #maxit default is 1000 , hence you can mention or not , no problem,integer giving the maximal number of IWLS iterations.https://rdrr.io/cran/SLEMI/man/capacity_logreg_algorithm.html
    fullMod2 <<-vmy$fullMod
    fndeletefromlr_modelstbl()   #once you delete column, all rows in multi model table having that column name will also be deleted
  })
  
  
  fndeletefromlr_modelstbl <- function(){
    req(vmy$lr_models)
    vmy$lr_models <- subset (vmy$lr_models %>%dplyr::filter(grepl(vmy$df_types[input$dt_cell_clicked$row,1], vmy$lr_models$Predictor_, fixed = TRUE)== FALSE))
    
  }
  
  
  output$mselectedvariable <-  renderText({
    if(length(input$dt_cell_clicked) != 0){
      clicked_list <- input$dt_cell_clicked
      HTML(paste("selected:",vmy$df_types[clicked_list$row,1],"Type:",vmy$df_types[clicked_list$row,2]))
      
    }
    else{
      HTML("select variable")
    }
  })
  
  
  
  
  #####################################################################################
  # select dependent variable and value to be considered as one and another one as zero 
  #####################################################################################
  observeEvent(input$mFixDependentVarBtn,{
    removeRightBox()
    btn <- input$mFixDependentVarBtn
    id <- paste0('txt', btn)
    disable("mFixDependentVarBtn")
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        box(
          br(),
          id = "mprocBox3",
          width = 4,
          height = '450px',
          align = "center",
          title = "Variable Selection",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          selectInput(inputId = 'mdependvar',label = 'Select Binary Dependent Variable',
                      choices =colnames(vmy$mydata),selected = rev(colnames(vmy$mydata))[1] ),
          actionButton(inputId = "AB1", label = "Click me to confirm..!",style=styleButtonBlue(xheight = '35px',xwidth = '200px')),
          br(),br(),
          selectInput(inputId = 'mbinaryOne',label = "Which element of dependent variable \nshould be ONE",
                      choices = NULL,selected = NULL),
          actionButton(inputId = "AB2", label = "Click me to confirm..!",style=styleButtonBlue(xheight = '35px',xwidth = '200px')),
          selectInput(inputId = 'mbinaryZero',label = "which should be ZERO",
                      choices = NULL,selected = NULL)
          
        )
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
  
  observeEvent(input$AB1, {
    if (length(input$file)!=0){
      if (nrow(dplyr::distinct(vmy$mydata,vmy$mydata[input$mdependvar])) != 2){
        useShinyjs()
        alert(HTML(paste("Dependent variable is not binary please check!,The following distinct values are in the selected column"),"<br>",
                   unique(vmy$mydata[,input$mdependvar])))
        return()
      }
      else{
        updateSelectInput(session,inputId = "mdependvar",choices = input$mdependvar,selected = input$mdependvar)
        updateSelectInput(session,inputId = "mbinaryOne",choices = unique(vmy$mydata[,input$mdependvar]))
        disable("AB1")
        disable('mbinaryZero')
        disable("mdependvar")
        
      }
    } 
  })
  
  
  observeEvent(input$AB2, {
    req(input$mbinaryOne) 
    aa<- unique(vmy$mydata[,input$mdependvar])
    bb <- ifelse(aa[1]==input$mbinaryOne,aa[2],aa[1])
    updateSelectInput(session,inputId = "mbinaryZero",choices = bb,selected = bb)
    vmy$mydata <-vmy$mydata %>% dplyr::select(-input$mdependvar, everything())
    vmy$mydata[input$mdependvar] <- factor(ifelse(vmy$mydata[input$mdependvar] == input$mbinaryOne, 1, 0), levels = c(0, 1))
    tempdf1 <- dplyr::select_if(vmy$mydata,is.numeric)
    tempdf1 <- cbind(tempdf1, dplyr::select_if(vmy$mydata,is.factor))
    vmy$mydata <- tempdf1
    fncreatedftype()
    fnTestTrainData(vmy$mydata,input$mdependvar)
    disable("AB2")
    disable("mbinaryOne")
    enable("mCleanseDataBtn")
  })
  
  
  fnTestTrainData <- function(xdatadf,xcolname){
    # Prep Training and Test data.
    
    n=which( colnames(xdatadf)==xcolname )
    set.seed(100)
    trainDataIndex <- createDataPartition(xdatadf[,n], p=0.70, list = F)  # 70% training data
    
    vmy$trainData <- xdatadf[trainDataIndex, ]
    vmy$testData <- xdatadf[-trainDataIndex, ]
    
    
    #There is approximately 2 times more benign samples. So lets downsample it using the downSample function from caret package.
    #To do this you just need to provide the X and Y variables as arguments.
    # Down Sample
    
    '%!in%' <- Negate('%in%')  # define 'not in' func
    options(scipen=999)  # prevents printing scientific notatio
    
    set.seed(100)
    n=which( colnames(xdatadf)==xcolname )
    assign('xcolname',xcolname)
    vmy$down_train <- downSample(x = vmy$trainData[, colnames(vmy$trainData) %!in% xcolname],
                                 y = vmy$trainData[,xcolname],yname = xcolname)
    #Benign and malignant are now in the same ratio.
    
    # Up Sample.
    set.seed(100)
    vmy$up_train <- upSample(x = vmy$trainData[, colnames(vmy$trainData) %!in% xcolname],
                             y = vmy$trainData[,xcolname],yname = xcolname)
  } 
  
  
  
  
  
  #####################################################################################
  # Building Multiple Model 
  #####################################################################################
  observeEvent(input$mFinalProcessBtn,{
    removeRightBox()
    btn <- input$mFinalProcessBtn
    id <- paste0('txt', btn)
    disable("maddmodelYorN")
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        box(
          id = "mprocBox4",
          width = 4,
          height = '450px',
          align = "center",
          title = "Multiple Model Building",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          br(),
          HTML(paste('<h5>',"Select the combination of predictors that you want to use in building multiple models.",'</br>','</br>',
                     "If you select from 2 to 4; all combinations of 2, 3 and 4 predictors will be used to generate multiple modules;",'</br>','</br>',
                     "Alternatively from 4 to 4 will use only combination of 4 predictors",'<h5>')),
          splitLayout(cellWidths = c('50%','50%'),
                      numericInput(inputId = "mVarCountFrom",label = "From",value = NULL,min = 2,max = 100),
                      numericInput(inputId = "mVarCountTo",label = "To",value = NULL,min = 2,max = 100)
                      ),
          br(),
          actionButton(inputId = "mGenerateLRModelsBtn", label = "Generate LR Models",style=styleButtonBlue(xheight = '35px',xwidth = '200px'))

        ),
        box(
          id = "mprocBox4",
          width = 4,
          height = '450px',
          align = "left",
          title = "Combinations of Predictors",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          br(),
          HTML(paste('<h5>',"Here is the number of combinations of LR models that you get when you process for each set of predictors",'<h5>')),
          verbatimTextOutput("mCombinationTxt")
        )
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
  observeEvent(input$mVarCountFrom,{
    updateNumericInput(session,inputId = "mVarCountTo",min = input$mVarCountFrom,max = ncol(vmy$mydata)-1,value = input$mVarCountFrom)
  })
  
  observeEvent(input$mVarCountTo,{
    req(input$mVarCountFrom)
    req(input$mVarCountTo)
    n<-ncol(vmy$down_train)
    mlist <- c()
    ncount <- 0
    for (i in (input$mVarCountFrom:input$mVarCountTo)){
      mlist <- c(mlist,paste("Number of models for ",i,"predictors :",length(combn(c(colnames(vmy$down_train[-n])), i))/i,'\n'))
      ncount <- ncount + length(combn(c(colnames(vmy$down_train[-n])), i))/i
    }
    mlist <- c(mlist,paste("\n","Total models:",ncount))
    output$mCombinationTxt <- renderPrint({
      HTML(mlist)
    })
    down_train <<-vmy$down_train
  })
  
  observeEvent(input$mGenerateLRModelsBtn,{
    req(input$mVarCountFrom)
    req(input$mVarCountTo)
    

    vmy$lr_models <- data.frame(
      Act           = character(),
      Model_        = character(),
      Predictor_    = character(),
      Count_        = numeric(),
      Null_Deviance = numeric(),
      Resd_Deviance = numeric(),
      Diff_Deviance = numeric(),
      AIC_          = numeric(),
      BIC_          = numeric(),
      KSStat_       = numeric(),
      ROCAUC_       = numeric(),
      SomersD_      = numeric(),
      Misclass_     = numeric(),
      Concordance_  = numeric(),
      Accuracy_     = numeric(),
      Sensitivity_  = numeric(), 
      Specificity_  = numeric(),
      LrPVal_       = numeric(),
      PseudoR2_     = numeric(),
      Hosmer_       = numeric()
    )
    

    for (i in input$mVarCountFrom:input$mVarCountTo){
      fnMultiModelGenerator1(
        xmdependvar=input$mdependvar,
        xmFixVarCount= i)
    }
  })
  
  
  
  fnMultiModelGenerator1 <- function(xmdependvar,xmFixVarCount){
    mprogcounter <- 100
    if (nrow(vmy$lr_models)==0){
      midcounter = 10000
    }
    else{
      mmodelid <- vmy$lr_models[nrow(vmy$lr_models),2]
      midcounter = as.numeric(paste0("1",substr(as.character(mmodelid ), start = 2, stop = 5)))        
    }
    
    ################################## full model calculation
    xxx <- paste0("vmy$down_train$",xmdependvar)
    f2 <- as.formula(paste(text=xxx,"~", "."))
    vmy$fullMod <- glm(f2, family = binomial(link = logit), data=vmy$down_train)  #maxit default is 1000 , hence you can mention or not , no problem,integer giving the maximal number of IWLS iterations.https://rdrr.io/cran/SLEMI/man/capacity_logreg_algorithm.html
    fnFitDFTable()
    fullMod1 <<-vmy$fullMod
    ##################################
    vmy$lr_waldtest <- fnWaldTestTbl()
    n<-ncol(vmy$down_train)
    m <- xmFixVarCount
    
    aa<- combn(c(colnames(vmy$down_train[-n])), m)
    
    ##- progress bar starting code before loop
    
    # Create a Progress object
    ##########################
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing Model", value = 0)
    
    # Number of times we'll go through the loop
    n <- 10
    ##- progress bar ending  code after loop
    
    nn<-ncol(aa)
    for (i in 1:nn){
      ##- progress bar starting code within loop
      ##########################################
      # Increment the progress bar, and update the detail text.
      progress$inc(1/nn, detail = paste("Predictors", m,"Combination:",i,"of",nn,";",round(i/nn*100,0),"%"))
      
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
      
      ##########################- progress bar end code code
      
      msimulationpredictors <- c(aa[,i])
      tt <- paste (msimulationpredictors, sep = " ", collapse = "+")
      
      xxx <- paste0("vmy$down_train$",xmdependvar)
      f1 <- as.formula(paste(paste( text=xxx,"~"), tt))
      #
      # f1 <- as.formula(paste(paste( text=xxx,"~"), tt))
      vmy$model2 <- glm(f1, family = binomial(link = logit), data=vmy$down_train, maxit = 100)
      
      pred1 <- predict(vmy$model2, newdata = vmy$testData, type = "response")
      y_pred1 <- as.numeric(ifelse( pred1  > 0.5, 1, 0))
      y_pred1  <- factor( y_pred1 , levels = c(0, 1))
      
      y_act1 <- vmy$testData[,which(colnames(vmy$testData)==xmdependvar)]
      
      actual1 <- as.numeric(as.character(y_act1))
      predscore1 <- as.numeric(as.character(y_pred1))
      
      results11 <- caret::confusionMatrix(y_pred1, y_act1, positive="1", mode="everything")
      df11 <- broom::tidy(results11)
      dfforShinyApp1 <- subset.data.frame(x = df11,select = c(1,3))
      if (isTRUE(nrow(vmy$testData)>=10)){
        ks_statval1 <- ks_stat(actuals=actual1, predictedScores=predscore1)
      }
      somersDval1 <- somersD(actuals=actual1, predictedScores=predscore1)
      rocaucval1 <-InformationValue::AUROC(actuals = actual1, predictedScores = predscore1)
      miscalerval1 <- misClassError(actuals=actual1, predictedScores=predscore1, threshold=0.5)    #Hi 0.5 is correct The default cutoff prediction probability score is 0.5 or the ratio of 1's and 0's in the training data. ::I got from this site; Thank you:http://r-statistics.co/Logistic-Regression-With-R.html
      
      library(lmtest)
      mlr<- lmtest::lrtest(vmy$model1,vmy$model2)
      mlrpval <-round(mlr[2,5],4)
      
      library(pscl)
      mpseudorval <- round(pR2(vmy$model2)[4],4)
      
      library(ResourceSelection)
      mhslm <- hoslem.test(vmy$model2$y, fitted(vmy$model2), g=10)
      mhoslem <- round(mhslm$p.value,4)
      
      #Akaike's Information Criterion and Bayesian Information Criterion are measures of the quality of the fit of statistical models. They can also be used as criteria for the selection of a model.
      #The model with the lowest BIC is considered the best, and can be written BIC* (or SIC* if you use that name and abbreviation).
      # For a perfect model, this will be 100%. So, the higher the concordance, the better is the quality of model.
      
      zz <- summary(vmy$model2)
      midcounter = midcounter +1
      mmodelid <-paste0("M",substr(as.character(midcounter), start = 2, stop = 5))


      vmy$lr_models <- vmy$lr_models %>% add_row(Act = "Text",
                                                 Model_= mmodelid,
                                                 Predictor_  = tt,
                                                 Count_ = m,
                                                 Null_Deviance=zz$null.deviance,
                                                 Resd_Deviance =zz$deviance,
                                                 Diff_Deviance = (zz$null.deviance-zz$deviance),
                                                 AIC_=zz$aic,
                                                 BIC_ = BIC(vmy$model2),
                                                 KSStat_ = round(ks_statval1*100,2),
                                                 ROCAUC_ = round(rocaucval1*100,2),
                                                 SomersD_ = round(somersDval1*100,2),
                                                 Misclass_ = round(miscalerval1*100,2),
                                                 Concordance_ =round((Concordance(actuals = actual1,predictedScores = predscore1)$Concordance)*100,2),
                                                 Accuracy_ = round((results11$table[1]+results11$table[4])/(results11$table[1]+results11$table[2]+results11$table[3]+results11$table[4])*100,2),
                                                 Sensitivity_ =  round(results11$byClass["Sensitivity"]*100,2),
                                                 Specificity_ =  round(results11$byClass["Specificity"]*100,2),
                                                 LrPVal_  = round(mlrpval,3),
                                                 PseudoR2_ = round(mpseudorval,3),
                                                 Hosmer_ = round(mhoslem,3)
      ) #add row closure
      
    } #second for closure
    
    vmy$lr_models[4:9] <- round(vmy$lr_models[4:9], 0)
    lr_models <<- vmy$lr_models
  }
  
  #######- above multimodel datatable end
  
  
  
  
  ########## Plotly multimodel chart start
  
  
  output$plotmodelcompare <- renderPlotly({
    d <- event_data("plotly_selected")
    if (!is.null(d)) {
      ddyu <- vmy$frame2()
    }
    else {
      #ddyu <- vmy$lr_models
      ddyu <- vmy$data_1()
    }
    
    f <- list(                       #plotly chart axis names /naming: ::I got from this site; Thank you: https://plotly.com/r/figure-labels/
      family = "Courier New, monospace",
      size = 15,
      weight="bold",
      color = "#000000"
    )
    xname <- list(
      title = "Logistic Regression Models",
      titlefont = f
    )
    yname <- list(
      title = "Statistics - values",
      titlefont = f
    )
    
    
    Diff_Deviance <- ddyu$Diff_Deviance
    AIC_ <- ddyu$AIC_
    BIC_ <- ddyu$BIC_
    KSStat_ <- ddyu$KSStat_
    ROCAUC_ <- ddyu$ROCAUC_
    SomersD_ <- ddyu$SomersD_
    Misclass_ <- ddyu$Misclass_
    Concordance_ <- ddyu$Concordance_
    Accuracy_ <- ddyu$Accuracy_
    Sensitivity_ <- ddyu$Sensitivity_
    Specificity_ <- ddyu$Specificity_
    LrPVal_ <- ddyu$LrPVal_
    PseudoR2_ <- ddyu$PseudoR2_
    Hosmer_ <- ddyu$Hosmer_
    Count_ <- ddyu$Count_
    
    x <- c(ddyu$Model_)
    
    data <- data.frame(x,Diff_Deviance, AIC_, BIC_, KSStat_, ROCAUC_, SomersD_, Misclass_, Concordance_,Accuracy_, Sensitivity_, Specificity_,LrPVal_,PseudoR2_,Hosmer_,Count_)
    fig <- plot_ly(data, x = ~x, y = ~Diff_Deviance, name = 'Diff_Deviance', type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~AIC_, name ='AIC', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~BIC_, name ='BIC', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~KSStat_, name ='KS Stat', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~ROCAUC_, name ='ROC AUC', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~SomersD_, name ='SomersD', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~Misclass_, name ='Misclass', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~Concordance_, name ='Concordance', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~Accuracy_, name ='Accuracy', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~Sensitivity_, name ='Sensitivity', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~Specificity_, name ='Specificity', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~LrPVal_, name ='LRT Pval', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~PseudoR2_, name ='Pseudo Rsqr', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~Hosmer_, name ='Hosmer Test', mode = 'lines+markers')
    fig <- fig %>% add_trace(y = ~Count_, name ='Count', mode = 'lines+markers')%>%
      layout(xaxis = xname, yaxis = yname)%>% event_register("plotly_selected")%>% layout(dragmode = "select")
    
    fig

  })
  
  
  frame2 <- data.frame()
  frame2_ <- data.frame()
  vmy$frame2<-function(){
    d <- event_data("plotly_selected")
    if (!is.null(d)) {
      frame2_ <- frame2_[is.null(frame2_$pointNumber), ] # Optional line to remove the previous selections 
      frame2_ <- as.data.frame(unique(rbind(frame2_, d)[3]))
      names(frame2_)[1]<-"Model_"
    }
    
    frame2_ <- inner_join(vmy$lr_models, frame2_, by = "Model_")  #::I got from this site; Thank you: https://stackoverflow.com/questions/46432574/filter-a-dataframe-by-another-dataframe
    return(frame2_)
    
  }

  
  ########## Plotly multimodel chart end
  
  
  observeEvent(input$select_button,{
    s = input$tblModel206_cell_clicked
    if (length(s)) {
      
      xRevindependent <- vmy$data_1()[s$row,3]
      
      if (length(xRevindependent )==0 ){
        alert("variables not selected plese check...!")
        return()
      }
      removeModal()
      pb = winProgressBar("Progress Bar", "Some information in %",0, 100, 0)
      u = c(0, sort(runif(20, 0, 100)), 100)
      for(i in u) {
        Sys.sleep(0.1)
        info <- sprintf("%d%% done", round(i))
        setWinProgressBar(pb, i, "Progress Bar", info)
      }
      Sys.sleep(2)
      close(pb)
      
      xxx <- paste0("vmy$down_train$",input$mdependvar)
      f1 <- as.formula(paste(paste( text=xxx,"~"), paste("+",paste (xRevindependent , sep = " ", collapse = "+"))))
      vmy$model2 <- glm(f1, family = binomial(link = logit), data=vmy$down_train)
      fnlrReprocessing()   
      
      updateTabItems(session,inputId = "tabs", selected = "tabModelEval")
      click(id = "mKSROCPlotBtn")
    }
  })
  
  urlintro <- a("A Review of the Logistic Regression Model with Emphasis on Medical Research", href="https://www.scirp.org/pdf/JDAIP_2019101114465046.pdf#page=1")
  
  urlintro2 <- a("Ref:For deeper understanding please refer to the blog by ANAMIKA THANDA on Logistic Regression ", href="https://careerfoundry.com/en/blog/data-analytics/what-is-logistic-regression/#:~:text=In%20logistic%20regression%2C%20every%20probability%20or%20possible%20outcome,uses%20a%20certain%20formula%20to%20make%20the%20conversion.")
  
  
  output$mybox455 <- renderUI({
    tags$div(
      tags$p(
        useShinyjs(),
        HTML(paste('<h5><b>',"Overview:",'</b><br><h5>',
                   paste("Logistic regression is a classification algorithm. 
                         It is used to predict a binary outcome based on a set of independent variables.",
                         urlintro2))),
        
        HTML(paste('<h5><b>',"Model Evaluation & Interpreting LR Results:",'</b><br><h5>')),
        HTML(paste('<h5>',
                   "1. An overall evaluation of the logistic model; ",'<br>',
                   "2. Statistical tests of individual predictors; ",'<br>',
                   "3. Goodness-of-fit statistics; and ",'<br>',
                   "4. Assessing the predictive ability of the model.",'<br>')),
        br(),
        HTML(paste('<h5><b>',"Reference:",'</b><br><h5>')),
        HTML(paste('<h5>',"Boateng, E. and Abaye, D. (2019) A Review of the Logistic ",
                   "Regression Model with Emphasis on Medical Research.Journal of Data ",
                   "Analysis and Information Processing, 7, 190-207. doi: 10.4236/jdaip.2019.74012.",urlintro))
      )
    )
  })
  
  

  output$muimultisliderplay <- renderUI({
    ncc<-ncol(vmy$lr_models)  # I am omitting the last text column for this slider
    slider_options <- names(vmy$lr_models[c(4:ncc)])

    # First, create a list of sliders each with a different name
    sliders <- lapply(1:length(slider_options), function(i) {
      if (slidercolrange==12){
        slidercolrange <- 1
      }
      else{
        slidercolrange <- slidercolrange ++ 1
      }
      inputName1A <- slider_options[i]

        column(slidercolrange+4,sliderInput(inputId = inputName1A, label = inputName1A,
                                            min=floor(min(vmy$lr_models[,inputName1A],na.rm = TRUE)),
                                            max=ceiling(max(vmy$lr_models[,inputName1A],na.rm = TRUE)),
                                            value=c(floor(min(vmy$lr_models[[inputName1A]],na.rm = TRUE)),ceiling(max(vmy$lr_models[[inputName1A]],na.rm = TRUE))),
                                            width = "200px"))
  
    })
    # Create a tagList of sliders (this is important)
    do.call(tagList, sliders)
    
  })
  
 

  vmy$data_1 <-function(){
    vmy$lr_models['Act'] <- shinyInput(actionButton, nrow(vmy$lr_models), 'button_', label = "",style=styleButtonBlue(xheight = '15px',xwidth = '20px'), onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )    #,icon = icon("arrow-alt-circle-right")
    vmy$lr_models <-vmy$lr_models %>% dplyr::select(Act, everything())
    data_ <- vmy$lr_models
    slider_options <- names(vmy$lr_models[c(4:20)]) 
    
    # this is how you fetch the input variables from ui component
    for(i in slider_options) {
      xxtt<-as.double(eval(parse(text=paste0("input$",i))))
      data_ <- data_[data_[[i]] <= xxtt[2] &                       # ::I got from this site; Thank you this data_: https://stackoverflow.com/questions/35202318/datatable-dt-shiny-steps-for-the-slider-range-in-filters
                      data_[[i]] >= xxtt[1],]

    }
    data_[order(data_$Model_, decreasing = FALSE),] 
  }
  
  
  output$tblmultiID206 <- renderUI({
    DT::dataTableOutput("tblModel206",height = '350px')
    
  })
  
  
  ##### - from her applied to multimodel table with filter option
  # if it is dataframe range slider filter will not work#lr_modelsminmax <-format.data.frame(lr_modelsminmax,digits=3) #note if you put 3 digits, you get 4 decimals
  output$tblModel206 <- DT::renderDataTable({
    DT::datatable(vmy$data_1(), 
                  class ='cell-border stripe compact white-space: nowrap', #::I got from this site; Thank you this multiple classes: https://rstudio.github.io/DT/
                  escape=F,
                  editable = F,
                  
                  options = list(lengthMenu = list(c(10, 25, 50,-1), c('10', '25','50' ,'All')),
                                 pageLength = 10,
                                 autoWidth = TRUE,
                                 columnDefs = list(list(width ='5px', targets = c(0)),
                                                   list(width ='5px', targets = c(1)),
                                                   list(width ='15px', targets = c(2)),
                                                   list(width ='40px', targets = c(3)),
                                                   list(width ='15px',className = "dt-center", targets = c(4:ncol(vmy$data_1()))))
                                 
                  ) ,
                  selection = list(mode = "single", selected = c(1), target = 'row'),
                  fillContainer = getOption("DT.fillContainer", TRUE)
                  
    ) %>% my.styleallrows()
  })
  
  my.styleallrows <- function(.) formatStyle(., columns=0, target= 'row',color = 'black', 
                                             backgroundColor = '#ffffed',
                                             fontWeight ='normal',lineHeight='75%')
  my.styleonecolumn <- function(.) formatStyle(., columns=c("AIC_"), target= 'cell',color = 'black', 
                                               backgroundColor = '#ffffed',
                                               fontWeight ='bold',lineHeight='70%')
  
  
  output$dtvartbl <- DT::renderDataTable({
    DT::datatable(vmy$df_types,
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = FALSE,
                  selection = list(mode = "multiple", target = 'row'),
                  fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(dom = 't',ordering=F, pageLength = -1,class="compact",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#808080', 'color': '#fff'});",   #::I got from this site; Thank you: JS from https://stackoverflow.com/questions/34009922/how-to-format-a-shiny-rendertable
                                   "}")
                  ) 
    )
    
  })
  
  output$mselectedvariable <-  renderText({
    if(length(input$dtvartbl_cell_clicked) != 0){
      clicked_list <- input$dtvartbl_cell_clicked
      HTML(paste("selected:",vmy$df_types[clicked_list$row,1],"Type:",vmy$df_types[clicked_list$row,2]))
    }
    else{
      HTML("select variable")
    }
  })
 
  
  
  
  
  ###########################################################################################
  # Data download actions for the dataset as well for the multiple model statistics dataframe
  ###########################################################################################
  
  output$mDownloaddtdfBtn<- downloadHandler(
    filename = function() {
      paste("multimodeldf", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vmy$data_1()[input[["tblModel206_rows_all"]],2:20 ]), file, row.names = FALSE)
    }
  )
  
  output$mDownloadmydataBtn<- downloadHandler(
    filename = function() {
      paste("mydatadf", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vmy$mydata[input[["mdatatable_rows_all"]], ]), file, row.names = FALSE)
    }
  )
  
  
  
  ################################################################################
  ## Likelihood Ratio Test
  ################################################################################
  library(lmtest) # for Likelihood Ratio Test 
  observeEvent(input$mLikelihoodBtn,{
    removeRightBox()
    btn <- input$mLikelihoodBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_ModelEval',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        HTML(paste('<h4><b>','Likelihood Ratio Test','</b><h5>')),
        tags$head(
          tags$style(
            "#mLikelihoodtxt{color:black; font-size:14.0px; font-style:normal;
                                            max-height: 400px; background: light-blue;text-align: left;}"
          )
        ),
        verbatimTextOutput("mLikelihoodtxt"),
        br(),
        uiOutput("mLikelihoodcomments")
      )
    )
    inserted <<- c(id, inserted)
  })
  
  output$mLikelihoodtxt <- renderPrint({
    lmtest::lrtest(vmy$model1,vmy$model2)
  })
  
  output$mLikelihoodcomments <- renderText({
    mpr<- lmtest::lrtest(vmy$model1,vmy$model2)
    mpr <-round(mpr[2,5],3)
    
    if (mpr <= 0.05){
      HTML(paste('<br>',"While comparing two models",',', "if the difference in log likelihoods of 
                    those two models is statistically significant (p value <= 0.05);",'<br>'," then the model1  
                    is of a better fit.",'<br><b><h4><FONT COLOR="#E51616">', "Here the p value of ",mpr, "being <=0.05",
                 "provides evidence",'<br>'," in favor of Model-1" ,'</b><h5>'))
      
    }
    else {
      HTML(paste('<br>',"While comparing two models",',', "if the difference in log likelihoods of 
                    those two models is statistically significant (p value <= 0.05);",'<br>'," then the model1  
                    is of a better fit.",'<br><b><h4><FONT COLOR="#E51616">', "Here the p value of ",mpr, "being > 0.05",
                 "provides evidence",'<br>',"AGAINST Model-1",'</b><h5>' )) 
    }
    
  })
  
  
  
  
  
  ################################################################################
  ## Pseudo Ratio Test
  ################################################################################
  library(lmtest) # for Pseudo Ratio Test 
  observeEvent(input$mPseudoRsqBtn,{
    removeRightBox()
    btn <- input$mPseudoRsqBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_ModelEval',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        HTML(paste('<h4><b>','Pseudo R2 Test','</b><h5>')),
        tags$head(
          tags$style(
            "#mPseudoRsqtxt{color:black; font-size:14.0px; font-style:normal;
                                            max-height: 400px; background: light-blue;text-align: left;}"
          )
        ),
        verbatimTextOutput("mPseudoRsqtxt"),
        br(),
        uiOutput("mPseudoRsqcomments")
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
  
  output$mPseudoRsqtxt <- renderPrint({
    library(pscl)
    pR2(vmy$model2)
  })
  
  output$mPseudoRsqcomments <- renderPrint({
    library(pscl)
    mpr<- pR2(vmy$model2)[4]
    HTML(paste('<b><h4><FONT COLOR="#E51616">',"Most important is McFadden's R2.","The measure ranges from 0 to just under 1;",
               "with value closer to 1 indicates higher predictive power.",'<br><b>',
               "In our proposed model McFadden's R2 is", round(mpr,4),'</b><h5>'))
    
  })
  
  
  
  
  ################################################################################
  ## Hosmer and Lemeshow Test
  ################################################################################
  library(lmtest) # for Pseudo Ratio Test 
  observeEvent(input$mHosmerLBtn,{
    removeRightBox()
    btn <- input$mHosmerLBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_ModelEval',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        HTML(paste('<h4><b>','Hosmer and Lemeshow Test','</b><h5>')),
        tags$head(
          tags$style(
            "#mHosmerLtxt{color:black; font-size:14.0px; font-style:normal;
                                            max-height: 400px; background: light-blue;text-align: left;}"
          )
        ),
        verbatimTextOutput("mHosmerLtxt"),
        br(),
        uiOutput("mHosmerLcomments")
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
  output$mHosmerLtxt <- renderPrint({
    library(ResourceSelection)
    hoslem.test(vmy$model2$y, fitted(vmy$model2), g=10)
  })
  
  output$mHosmerLcomments <- renderPrint({
    library(ResourceSelection)
    mpr<- hoslem.test(vmy$model2$y, fitted(vmy$model2), g=10)
    if (isTRUE(mpr$p.value>.05)){
      mprtext <- HTML(paste("is >= 0.05; not statistically significant;",'<br>',"hence there is no evidence against the NULL hypothesis;",
                            "and proposed model is a good fit"))
    }
    else{
      mprtext <- HTML(paste("is <= 0.05; statistically significant; hence we reject the NULL hypothesis;",
                            "indicate a poor fit"))
    }
    
    HTML(paste('<br>',"The NULL hypothesis is that the model fits to the data; and large p-value", 
               "indicates a good fit.",'<br>',
               '<b><h4><FONT COLOR="#E51616">',"Here the p value of",
               round(mpr$p.value,4),mprtext,'</b><h5>'))
    
  })
  
  
  
  ################################################################################
  ## Wald Test
  ################################################################################
  library(survey)  #The package survey provided function for the Wald test as "regTermTest". F statistic and P-value
  observeEvent(input$mWaldTestBtn,{
    removeRightBox()
    btn <- input$mWaldTestBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_ModelEval',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        HTML(paste('<b>',"Wald Test / Wald Chi-Squared Test",'</b>')),
        tags$head(
          tags$style(
            "#txtwaldtest{color:black; font-size:14.0px; font-style:normal;
                                            max-height: 400px; background: light-blue;text-align: left;}"
          )
        ),
        verbatimTextOutput("txtwaldtest"),
        htmlOutput(outputId ="mWaldTestKblTbl")
    )
    )
    inserted <<- c(id, inserted)
  })
  
 
  library(aod) # this is for Wald test
  output$txtwaldtest <- renderPrint({
    wald.test(b = coef(vmy$model2), Sigma = vcov(vmy$model2), Terms = 2:length(vmy$model2$terms))
  })

  
  library(kableExtra)      # to handle kbl tables and scroll_box function
  output$mWaldTestKblTbl <- renderUI({
    vmy$lr_waldtest <- fnWaldTestTbl()
    n<-ncol(vmy$lr_waldtest)
    HTML( kbl(vmy$lr_waldtest, escape = FALSE,align=c(rep('c',times=n)),   #align center all columns including header in table align=c(rep('c',times=n)) https://stackoverflow.com/questions/41365502/aligning-columns-with-knitr-kable-function
              caption =NULL,linesep = "\\addlinespace",
              table.attr = "style='width:30%;'")%>%
            kable_styling(font_size = 16, position = "center", html_font = "Cambria",fixed_thead = TRUE) %>%
            kable_paper("striped", full_width = FALSE) %>%
            column_spec(1, color = "red",background = '#ffffed')%>%  #for formatting first column in table
            row_spec(0:nrow(vmy$lr_waldtest), angle = 360,bold=TRUE, color = "red",background = '#ffffed',font_size = 16)%>%  #for formatting table header
            column_spec(1, bold = FALSE,width = "1.3in",color='red') %>%
            column_spec((1:n), bold = FALSE,border_left = TRUE,border_right = TRUE,width = "1.3in",color='black') %>%
            scroll_box(width = "100%", height = "375px")
    )

  })
  
  fnWaldTestTbl <- function(){
    lr_waldtest <- data.frame (Predictor="predictorname",
                               F_Test = 0,
                               DF_Total = 0,
                               P_Value = 0,
                               Sign_Stars ="stars"
    )[-1,]
    
    for (iw in names(vmy$model2$model[-1])) {
      ddc<-regTermTest(model = vmy$model2, test.terms = iw)
      if (isTRUE(ddc$p <= 0.001)) {
        mysig<-"***"
      }
      else if (isTRUE(ddc$p <= 0.01) == TRUE){
        mysig<-"**"
      }
      else if (isTRUE(ddc$p <= 0.05) == TRUE){
        mysig<-"*"
      }
      else if (!is.na(ddc$p <= 0.1) == TRUE){
        mysig<-"."
      }
      else {
        mysig<-""
      }
      lr_waldtest <- lr_waldtest %>% add_row(Predictor=ddc$test.terms,
                                             F_Test = as.numeric(sprintf(round(ddc$Ftest,3), fmt="%#.3f")),
                                             DF_Total = ddc$ddf,
                                             P_Value = as.numeric(sprintf(round(ddc$p,4), fmt="%#.4f")),
                                             Sign_Stars = mysig
      )
    }
    
    return(lr_waldtest)
  }
  
  
  ################################################################################
  ## ANOVA Test
  ################################################################################
  observeEvent(input$mAnovaTestBtn,{
    removeRightBox()
    btn <- input$mAnovaTestBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_ModelEval',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        HTML(paste('<b>',"ANOVA Test",'</b>')),
        tags$head(
          tags$style(
            "#txtAnovaTest{color:black; font-size:13.0px; font-style:normal;
overflow-y:scroll; max-height: 450px; background: light-blue;text-align: left;}"
          )
        ),
        tabsetPanel(
          tabPanel("Model1 Vs Model2",
                   verbatimTextOutput("txtAnovaTest1")
          ),  #tabpanel closure
          tabPanel("Model fitting",
                   verbatimTextOutput("txtAnovaTest2"),
          ),  #tabpanel closure
          tabPanel(
            "Waterfalls Chart",
            plotlyOutput("mAnovaWaterfall", height = '450px')
          )#tabpanel closure
        )#tabsetpanel closure
      )
    )
    inserted <<- c(id, inserted)
  })

  

  output$txtAnovaTest1 <- renderPrint({
    anova(vmy$model1,vmy$model2,test = "Chisq")
  })
  
  

  output$txtAnovaTest2 <- renderPrint({
    #anova(modl1, test="Chisq")
    anova(vmy$model2, test="Chisq")
    
    ## If we want to avoid auto naming
    # tab_model(modl1, auto.label = FALSE)
  })
  
  
  
  output$mAnovaWaterfall <- renderPlotly({
    
    xx <-  anova(vmy$model2, test="Chisq")
    aa<- as.list(rownames(xx))
    n<-length(aa)
    #variable 1
    x <- as.list(aa[1:n])
    x <- c(x, 'Residual')
    #Variable 2 
    l<-c()
    i=1
    while(i<=n) {
      if (i==1){
        b<-"relative"
      }
      else{
        b<-"relative"   
      }
      l<-c(l,b)
      i=i+1
    }
    b<-"total"
    l<-c(l,b)
    wfmeasure <- as.list(l)
    #Variable 3 
    l <-c()
    i=1
    totResidDev <- round(xx$"Resid. Dev"[1],3)
    balResidDev <- totResidDev
    while(i<=n) {
      if (i==1){
        b<-paste(as.character(sprintf(round(xx$"Resid. Dev"[1]/totResidDev*100,3), fmt="%#.2f")),"%")
      }
      else{
        b<-paste(as.character(sprintf(round(xx$Deviance[i]*-1/totResidDev*100,3), fmt="%#.2f")),"%")
        balResidDev <- balResidDev - xx$Deviance[i]
      }
      
      l<-c(l,b)
      i=i+1
    }
    b<-paste(as.character(sprintf(round(balResidDev/totResidDev*100,3), fmt="%#.2f")),"%")
    l<-c(l,b)
    
    text <- as.list(l)
    l <-c()
    i=1
    while(i<=n) {
      if (i==1){
        b<-sprintf(round(xx$"Resid. Dev"[1],3), fmt="%#.3f")
      }
      else{
        b<-sprintf(round(xx$Deviance[i]*-1,3), fmt="%#.3f")
      }
      
      l<-c(l,b)
      i=i+1
    }
    b<-sprintf(round(balResidDev,3), fmt="%#.3f")
    l<-c(l,b)
    y <- as.list(l)
    datawaterfall = data.frame(x=factor(x,levels=x),wfmeasure,text,y)
    
    f <- list(                       #plotly chart axis names /naming: ::I got from this site; Thank you: https://plotly.com/r/figure-labels/
      family = "Courier New, monospace",
      size = 15,
      color = "#000000"
    )
    xname <- list(
      title = "Predictors",
      titlefont = f
    )
    yname <- list(
      title = "% Deviance from Intercept",
      titlefont = f
    )
    
    plot_ly(
      datawaterfall, name = "20", type = "waterfall", measure = ~wfmeasure,
      x = ~x, textposition = "outside", y= ~y, text =~text,
      connector = list(line = list(color= "rgb(63, 63, 63)")))%>% 
      layout(xaxis = xname, yaxis = yname)%>%
      layout(xaxis = list(tickfont = list(size = 15)), 
             yaxis = list(tickfont = list(size = 5)))
    
  })
  
  
  
  ################################################################################
  ## Variable Importance
  ################################################################################
  observeEvent(input$mVariableImpBtn,{
    removeRightBox()
    btn <- input$mVariableImpBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_ModelEval',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        HTML(paste('<h4><b>',"Assess the variable importance of individual predicotrs",'</b><h4>')),
        plotOutput("VariableImpPlot", height = '450px', width = '100%'),
        uiOutput("VariableImpPlotcomment")
      )
    )
    inserted <<- c(id, inserted)
  })
  

  library(randomForest)
  output$VariableImpPlot <-renderPlot({
    fnFitDFTable()
    varImpPlot(vmy$fit,pch=18,col="blue",cex=1.25,
               main = NULL)
    
  })

  output$VariableImpPlotcomment <- renderPrint({
    HTML(paste('<h4><FONT COLOR="#E51616">',"The mean decrease accuracy is how much the model fit decreases when you drop a variable.", 
               "Higher the plotted position, the more significant the variable.",'<br>',
               "Similarly a higher Mean Decrease in Gini indicates higher variable importance.",'<h5>'))
    
  })
  
 fnFitDFTable <- function(){
   #here is the processing to get fit_df for variable importance by considering all independent variables
   n <- which(names(vmy$down_train)==input$mdependvar)
   xxx <- paste0("vmy$down_train$",input$mdependvar)
   f2 <- as.formula(paste(paste( text=xxx,"~"), paste("+",paste (names(vmy$down_train[-n]), sep = " ", collapse = "+"))))
   vmy$fit <- randomForest(f2 ,data=vmy$down_train,family=binomial(),importance=TRUE, nodesize=5,ntree=1000)
   
   options(scipen=999)
   
   fit_df<-as.data.frame(importance(vmy$fit)[ ,3:4])
   #Convert Row Names to Column with dplyr Package
   fit_df <- tibble::rownames_to_column(fit_df, "Independent_var") 
   
   
   #filter only top 3 independent variables based on MeanDecreaseGani for initial logit function
   hyr<- round(ncol(vmy$down_train)*0.40,0)
   f4 <- head(as.data.frame(fit_df[order(-fit_df$MeanDecreaseGini),]),hyr)[1]
   mfitselectedvar <- f4[,1]

   tt <- paste (mfitselectedvar, sep = " ", collapse = "+")
   
   xxx <- paste0("vmy$down_train$",input$mdependvar)
   f1 <- as.formula(paste(paste( text=xxx,"~"), tt))
   # 
   # f1 <- as.formula(paste(paste( text=xxx,"~"), tt))
   vmy$model1 <-  glm(f1, family = binomial(link = logit), data=vmy$down_train, maxit = 100)
 }
  
  
  
  
  ################################################################################
  ## Confusion Matrix or Contingency Matrix
  ################################################################################
  observeEvent(input$mConfusionMtrxBtn,{
    removeRightBox()
    btn <- input$mConfusionMtrxBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_ModelEval',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        HTML(paste('<h4><b>',"Confusion Matrix & Classification Rate.",'</b><h4>')),
        tags$head(
          tags$style(
            "#plotconfmatrix{color:white; font-size:18px; font-style:normal;
                                max-height: 400px;max-width:800px; background: light-blue;text-align: left;}"
          )
        ),
        plotOutput(outputId = "mConfusionMtrxPlot", height = '400px', width = '600px'),
        uiOutput(outputId = 'mConfusionMtrxcomments'),
        HTML(paste('<h5>',urlref5))
      )
    )
    inserted <<- c(id, inserted)
  })
  
  output$mConfusionMtrxPlot <-renderPlot({
    fnLRConfusionMtrx()
  })
  
  
output$mConfusionMtrxcomments <- renderPrint({
  xdep <- colnames( vmy$model2$model[1]) 
  xdep <- sub('.*\\$', '', xdep)
  HTML(paste( paste('<h5>',"Dependent Variable:",xdep,";",
                    "Independent variables:"),
              paste("+",paste (colnames( vmy$model2$model[-1]), sep = " ", collapse = " + "))))
})

  
  
  fnLRConfusionMtrx <- function(){
    pred <- predict(vmy$model2, newdata = vmy$testData, type = "response")
    
    y_pred <- as.numeric(ifelse( pred  > 0.5, 1, 0))
    y_pred  <- factor( y_pred , levels = c(0, 1))
    y_act <- vmy$testData[,which(colnames(vmy$testData)==input$mdependvar)]
    
    actual <- as.numeric(as.character(y_act))
    predscore <- as.numeric(as.character(y_pred))

    results1 <- caret::confusionMatrix(y_pred, y_act, positive="1", mode="everything")
    rownames(results1$table)<-ifelse(rownames(results1$table)=="0",input$mbinaryZero,input$mbinaryOne)
    colnames(results1$table)<-ifelse(colnames(results1$table)=="0",input$mbinaryZero,input$mbinaryOne)

    total <- sum(results1$table)
    res <- as.numeric(results1$table)
    
    # Generate color gradients. Palettes come from RColorBrewer.
    greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
    redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
    getColor <- function (greenOrRed = "green", amount = 0) {
      if (amount == 0)
        return("#FFFFFF")
      palette <- greenPalette
      if (greenOrRed == "red")
        palette <- redPalette
      colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
    }
    
    # set the basic layout
    layout(matrix(c(1,1,2)))
    par(mar=c(2,2,2,2))
    plot(c(125, 355), c(270, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
    #title('CONFUSION MATRIX & CLASSIFICATION RATE', cex.main=2.0)
    # create the matrix
    classes = colnames(results1$table)
    rect(150, 430, 240, 370, col=getColor("green", res[1]))
    text(195, 445, classes[1], cex=2.0)
    rect(250, 430, 340, 370, col=getColor("red", res[3]))
    text(295, 445, classes[2], cex=2.0)
    text(130, 370, 'Predicted', cex=1.7, srt=90, font=2)
    text(245, 445, 'Actual', cex=1.7, font=2)
    rect(150, 305, 240, 365, col=getColor("red", res[2]))
    rect(250, 305, 340, 365, col=getColor("green", res[4]))
    text(140, 400, classes[1], cex=2.0, srt=90)
    text(140, 335, classes[2], cex=2.0, srt=90)
    
    # add in the results1 results
    text(195, 400, res[1], cex=2.0, font=2.3, col='black')
    text(195, 335, res[2], cex=2.0, font=2.3, col='black')
    text(295, 400, res[3], cex=2.0, font=2.3, col='black')
    text(295, 335, res[4], cex=2.0, font=2.3, col='black')
    
    text(195, 285, (res[1]+res[2]), cex=2.0, font=2.3, col='black')
    text(295, 285, (res[3]+res[4]), cex=2.0, font=2.3, col='black')
    text(350, 400, (res[1]+res[3]), cex=2.0, font=2.3, col='black')
    text(350, 335, (res[2]+res[4]), cex=2.0, font=2.3, col='black')
    text(350, 285, (res[1]+res[2]+res[3]+res[4]), cex=2.0, font=2.3, col='black')
    
    # add in the specifics
    plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "Stats", xaxt='n', yaxt='n',cex.main=2.0)
    text(10, 85, names(results1$byClass[1]), cex=1.6, font=2)
    text(10, 67, fnpercent(as.numeric(results1$byClass[1]), 1), cex=1.6)
    text(30, 85, names(results1$byClass[2]), cex=1.6, font=2)
    text(30, 67, fnpercent(as.numeric(results1$byClass[2]), 1), cex=1.6)
    text(50, 85, names(results1$byClass[5]), cex=1.6, font=2)
    text(50, 67, fnpercent(as.numeric(results1$byClass[5]), 1), cex=1.6)
    text(70, 85, names(results1$byClass[6]), cex=1.6, font=2)
    text(70, 67, fnpercent(as.numeric(results1$byClass[6]), 1), cex=1.6)
    text(90, 85, names(results1$byClass[7]), cex=1.6, font=2)
    text(90, 67, fnpercent(as.numeric(results1$byClass[7]), 1), cex=1.6)
    # add in the accuracy information
    text(15, 35, names(results1$overall[1]),cex=1.6, font=2)
    text(15, 17, fnpercent(as.numeric(results1$overall[1]), 1), cex=1.6)
    text(85, 35, names(results1$overall[2]), cex=1.6, font=2)
    text(50, 35, "Miss-Classification",cex=1.6, font=2)
    text(50, 17, fnpercent(1-as.numeric(results1$overall[1]), 1), cex=1.6)
    text(85, 17, fnpercent(as.numeric(results1$overall[2]), 1), cex=1.6)
  } 
  
  
  
  
  
  ################################################################################
  ## KS and ROC-AUC Plot
  ################################################################################
  observeEvent(input$mKSROCPlotBtn,{
    fnlrReprocessing()
    removeRightBox()
    btn <- input$mKSROCPlotBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_ModelEval',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        HTML(paste('<h4><b>',"KS Plot and ROC-AUC Plot",'</b><h4>')),
        column(width = 6,
               plotOutput('mksplot',width = NULL,height = '350px')
               ),
        column(width = 6,
               plotOutput('mrocplot', width = NULL, height = '350px')
        ),
        column(
          width = 12,
          align='l',
          uiOutput(outputId = "mksplotcriteria")
        )


      )
    )
    inserted <<- c(id, inserted)
  })
  
  output$mksplot <-renderPlot({
    if (nrow(vmy$testData)>=10){
      InformationValue::ks_plot(actuals = vmy$actual,predictedScores =  vmy$predscore)
    }
    
  })
  
  output$mrocplot <-renderPlot({
    InformationValue::plotROC(actuals = vmy$actual, predictedScores =vmy$predscore)
  })
  
  output$mksplotcriteria <- renderPrint({
    
    xdep <- colnames( vmy$model2$model[1]) 
    xdep <- sub('.*\\$', '', xdep)
    
    
    HTML(paste('<h4><b>',"KS Statistics:",'</b>',vmy$mKsStatistics),
         paste('<h4><b>',"Dependent Variable:",'</b>',xdep),
         '<h4><b>',"Independent variables:",'</b>',paste("+",paste (colnames( vmy$model2$model[-1]), sep = " ", collapse = " + ")))
    })
  

  fnlrReprocessing <- function(){
    pred <- predict(vmy$model2, newdata = vmy$testData, type = "response")
    
    vmy$y_pred <- as.numeric(ifelse( pred  > 0.5, 1, 0))
    vmy$y_pred  <- factor( vmy$y_pred , levels = c(0, 1))
    vmy$y_act <- vmy$testData[,which(colnames(vmy$testData)==input$mdependvar)]
    
    vmy$actual <- as.numeric(as.character(vmy$y_act))
    vmy$predscore <- as.numeric(as.character(vmy$y_pred))
    vmy$results1 <- caret::confusionMatrix(vmy$y_pred, vmy$y_act, positive="1", mode="everything")
    
    if (isTRUE(nrow(vmy$testData)>=10)){
      vmy$mKsStatistics <- round(ks_stat(actuals=vmy$actual, predictedScores=vmy$predscore),3)
    }
    else{
      vmy$mKsStatistics <- "Observations are less than 10, KS Statistics cannot be calculated..!"
    }
    
  }
  
  observeEvent(input$mLrModelRevise,{  
    showFirstModal()
  })
  
  
  showFirstModal <- function() { 
    showModal(modalDialog(
      size = 's',
      easyClose = TRUE,
      footer = NULL,
      fluidRow(
        column(
          width = 12,
          align="center",
          HTML(paste('<p text-align ="center"><h4><i>',
                     paste('<span style="color:#ff0000;"> ',"You have an option here to select predictors and re-process!",'</span>'),'</i></p><h4>')),
          
          box(
            id="box_modelreprocess",
            width = 12,
            height = 375,
            #background = 'white',
            title = NULL,
            #title = "Variable Importance Table",
            status = "warning",
            solidHeader = TRUE,
            collapsible = FALSE,
            HTML(paste("Dependent Variable:",input$mdependvar)),
            br(),
            pickerInput(
              inputId = "mvarindep",
              label = tags$h5(HTML(paste("Select only Independent Variable"
              ))),
              multiple = TRUE,
              choices = colnames(vmy$down_train)[- which(colnames(vmy$down_train)==input$mdependvar)],
              ## this is an interesting option to filter,donot delete
              #choices = rownames(table_str%>%dplyr::filter( table_str$col_type=="factor")),
              selected = NULL,
              options = list('actions-box' = TRUE)
              )
            ),
            actionButton(inputId = "clean", label = "Re-process",style=styleButtonBlue(xheight = '35px',xwidth = '200px')),
            br(),
            tags$h5(HTML(paste("Click outside to close")))
          ) # box closure
        ) #column closure
      ) #fluidrow closure
    ) # modal dialogue closure
    
  }
  
  observeEvent(input$clean, {
    if (length(input$mvarindep)==0 ){
      alert("variables not selected plese check...!")
      return()
    }
    removeModal()
    pb = winProgressBar("Progress Bar", "Some information in %",0, 100, 0)
    u = c(0, sort(runif(20, 0, 100)), 100)
    for(i in u) {
      Sys.sleep(0.1)
      info <- sprintf("%d%% done", round(i))
      setWinProgressBar(pb, i, "Progress Bar", info)
    }
    Sys.sleep(2)
    close(pb)
    
    
    xxx <- paste0("vmy$down_train$",input$mdependvar)
    f1 <- as.formula(paste(paste( text=xxx,"~"), paste("+",paste (input$mvarindep, sep = " ", collapse = "+"))))
    vmy$model2 <- glm(f1, family = binomial(link = logit), data=vmy$down_train)
    fnlrReprocessing()   
  }) 
  

  
  ################################################################################
  ## K-fold cross validation
  ################################################################################
  observeEvent(input$mKFoldValidBtn,{
    removeRightBox()
    btn <- input$mKFoldValidBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_ModelEval',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        column(
          width = 6,
          HTML(paste('<h5><b>',"k-fold Cross Validation",'</b><h5>')),
          verbatimTextOutput("mKFoldValidTxt1A"),
          verbatimTextOutput("mKFoldValidTxt1B")
        ),
        column(
          width = 6,
          HTML(paste('<h5><b>',"Repeated k-fold Cross Validation",'</b><h5>')),
                     verbatimTextOutput("mKFoldValidTxt2A"),
                     verbatimTextOutput("mKFoldValidTxt2B")
          )
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
  output$mKFoldValidTxt1A <- renderPrint({
    set.seed(100)
    ctrl <- trainControl(method = "cv", number = 5,savePredictions = TRUE)
    f5 <- as.formula(paste(paste( text=input$mdependvar,"~"), paste(vmy$model2$terms)[3], sep = " ", collapse = "+"))
    mod_fit <- train(f5, data=vmy$testData, method="glm", family="binomial", trControl = ctrl, tuneLength = 7)
    caret::confusionMatrix((mod_fit$pred)$pred, (mod_fit$pred)$obs, positive="1", mode="everything")$table
  })
  
  
  
  output$mKFoldValidTxt1B <- renderPrint({
    set.seed(100)
    ctrl <- trainControl(method = "cv", number = 5,savePredictions = TRUE)
    f5 <- as.formula(paste(paste( text=input$mdependvar,"~"), paste(vmy$model2$terms)[3], sep = " ", collapse = "+"))
    mod_fit <- train(f5, data=vmy$testData, method="glm", family="binomial", trControl = ctrl, tuneLength = 7)
    mod_fit
  })
  
  output$mKFoldValidTxt2A <- renderPrint({
    set.seed(100)
    ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 4,savePredictions = TRUE)
    f5 <- as.formula(paste(paste( text=input$mdependvar,"~"), paste(vmy$model2$terms)[3], sep = " ", collapse = "+"))
    mod_fit2A <- train(f5, data=vmy$testData, method="glm", family="binomial", trControl = ctrl, tuneLength = 7)
    caret::confusionMatrix((mod_fit2A$pred)$pred, (mod_fit2A$pred)$obs, positive="1", mode="everything")$table
   })
  
  
  
  
  output$mKFoldValidTxt2B <- renderPrint({
    set.seed(100)
    ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 4,savePredictions = TRUE)
    f5 <- as.formula(paste(paste( text=input$mdependvar,"~"), paste(vmy$model2$terms)[3], sep = " ", collapse = "+"))
    mod_fit <- train(f5, data=vmy$testData, method="glm", family="binomial", trControl = ctrl, tuneLength = 7)
    mod_fit
  })
  
 
  
  
  
  
  ################################################################################
  ## Akaike Information Criteria (AIC)
  ################################################################################
  observeEvent(input$mAICDevianceBtn,{
    removeRightBox()
    btn <- input$mAICDevianceBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_ModelEval',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        HTML(paste('<b>',"Akaike Information Criteria (AIC)",'</b>')),
        tags$head(
          tags$style(
            "#mAICDevianceTxt{color:black; font-size:13.0px; font-style:normal;
overflow-y:scroll; max-height: 450px; background: light-blue;text-align: left;}"
          )
        ),
        tabsetPanel(
          tabPanel("Model fitting",
                   verbatimTextOutput("mAICDevianceTxt")
                   ), #tabpanel closure
          tabPanel("Bar Chart",
                   plotlyOutput("mAICDeviancePlot", height = '450px')
          ), #tabpanel closure
          tabPanel("Outliers",
                   plotOutput("mPlotOutlier", height = '450px')
          ), #tabpanel closure
          tabPanel("Odds Ratio & CI",
                   plotOutput('OddsRatioCIPlot', height = '450px')
          )#tabpanel closure
        )#tabsetpanel closure
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
  output$mAICDevianceTxt <- renderPrint({
    #summary(modl1)
    summary(vmy$model2)
  })
  
 
  output$mAICDeviancePlot <- renderPlotly({
    #zz<-summary(modl1)
    zz<-summary(vmy$model2)
    n<-nrow(zz$coefficients)
    xx <- as.data.frame(zz$coefficients[2:n,1])
    aa<- as.list(rownames(xx[1]))
    n<-length(aa)
    #variable 1
    x <- as.list( aa[1:n])
    
    #Variable 3 
    text <- as.list(as.character(sprintf(round(as.numeric(xx[,1]),3), fmt="%#.3f")))
    #Variable 4 
    y <- as.list(as.numeric(sprintf(round(as.numeric(xx[,1]),3), fmt="%#.3f")))
    databarchart = data.frame(x=factor(x,levels=x),text,y)
    
    #Variable 5 Color
    n<-length(aa)
    l<-c()
    i=1
    while(i<=n) {
      if (xx[i,1]<0){
        b<-"#ff0000"
      }
      else{
        b<-"#2e8b57"   
      }
      l<-c(l,b)
      i=i+1
    }
    mbarcolor <- as.list(l)
    
    
    f <- list(                       #plotly chart axis names /naming: ::I got from this site; Thank you: https://plotly.com/r/figure-labels/
      family = "Courier New, monospace",
      size = 15,
      color = "#000000"
    )
    xname <- list(
      title = "Predictors",
      titlefont = f
    )
    yname <- list(
      title = "Coefficient/Log-odds",
      titlefont = f
    )
    
    
    isolate(plot_ly(
      databarchart, type = "bar", x = ~x, textposition = "outside", 
      y= ~y, text =~text,marker = list(color = ~mbarcolor))%>%
        layout(xaxis = xname, yaxis = yname)
    )
    
  }) 
  
  
  output$mPlotOutlier <-renderPlot({
    # # It's extremely hard to see, but most of the variables show a Gaussian or double Gaussian distribution.
    # # You can look at the distribution of the data a different way using box and whisker plots. The box captures the middle 50% of the data, the line shows the median and the whiskers of the plots show the reasonable extent of data.
    # # Any dots outside the whiskers are good candidates for outliers.
    
    n <- ncol(vmy$mydata)-1
    
    if (n>13){
      n <- ncol(vmy$mydata)-1
      c <- ceiling(n/2)
      r <- 2
      par(mfrow=c(r,c))    #get in 3 rows and 3 columns (first one is row)
      
      # #par("mar") (bottom, left, top, right) in lines
      # #par(mar=rep(0,4))
      
      
      
      for(i in 1:n) {
        boxplot(vmy$mydata[,i],main = colnames(vmy$mydata)[i],
                at = c(i),
                col = "#87CEFA",
                border = "black",
                las=2,    #if you want x axis name vertically put las =2 otherwise by default las=1  ::I got from this site; Thank you: https://www.r-bloggers.com/box-plot-with-r-tutorial/
                horizontal = FALSE,
                notch = TRUE)
      }
    }
    else{
      
      # This is fancy box plot with notch in the middle ,
      # you can use this option when there is fixed number of variables upto 10 should give good visual
      # if you enable this function, above definishing of column and row number should not be there
      # ::I got from this site; Thank you: https://www.datamentor.io/r-programming/box-plot/#:~:text=R%20Box%20Plot.%20You%20will%20also%20learn%20to,data%20frame%29%20with%20numeric%20vectors%20as%20its%20components.
      
      n<-ncol(vmy$mydata)
      isolate(boxplot(vmy$mydata[-n],
                      # main = "Multiple boxplots for comparision",
                      at = c(seq(seq(1, n-1, by=1))),
                      names = c(colnames(vmy$mydata[-n])),
                      las = 2,
                      col = c("orange","red"),
                      border = "brown",
                      horizontal = FALSE,
                      notch = TRUE
                      
      )
      )
    }
    
    
  })
  
  
  
  
  
  output$OddsRatioCIPlot <- renderPlot({
    library(questionr)
    dfbb<-odds.ratio(vmy$model2)
    dfbb<-dfbb[-1,]
    Predictors=c(rownames(dfbb))
    # Create labels
    df <- data.frame(
      terms=rownames(dfbb),
      yAxis = 1:length(Predictors),
      Odds_Ratio=round(dfbb$OR,3),
      CI_Low=round(dfbb$'2.5 %',3),
      CI_High=round(dfbb$'97.5 %',3),
      p_value=round(dfbb$p,3)
    )
    
    
    (p <- ggplot(df, aes(x = Odds_Ratio, y = Predictors)) + 
        geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        geom_errorbarh(aes(xmax = CI_High, xmin = CI_Low), size = .5, height = 
                         .2, color = "gray50") +
        geom_point(size = 3.5, color = "orange") +
        theme(panel.grid.minor = element_blank()) +
        ylab("") +
        xlab("Odds ratio")  + 
        ggtitle(paste("Odds Ratio, CI and p-value on the risk of outcome be:",input$mbinaryOne ))+
      ### geom_label_repel
        geom_label_repel(aes(label = paste("OR:",round(Odds_Ratio,2),"(CI:",round(CI_Low,2),"-",round(CI_High,2),")","p-val:",round(p_value,2))),
                         box.padding   = 0.35, 
                         point.padding = 0.5,
                         segment.color = 'grey50') 
    ) 
    
  })  
  
  

  output$OddsRatioCIPlot1 <- renderPlot({
    ## odds ratios and 95% CI
    moddscidf <- data.frame(exp(cbind(OR = coef(vmy$model2), suppressMessages(confint(vmy$model2,level = 0.95)))))[-1,]
    colnames(moddscidf)<-c("OR","L_CI","U_CI")
    moddscidf$Conf_Intrl <- (moddscidf$U_CI-moddscidf$L_CI)/2

    #x = 1:nrow(moddscidf)*2-1
    moddscidf<<-moddscidf
    myplot<-ggplot(moddscidf, aes(y =paste(rownames(moddscidf),":\n","OR:",round(OR,2),"CI:","+-",round(Conf_Intrl,2)), x =OR )) +
      geom_point(size = 1,color="blue")+
      geom_errorbarh(aes(xmin = L_CI, xmax = U_CI,colour="red",height = .4,),show.legend = FALSE)+   #you can mention color as colour=group
      geom_line(linetype = 2)
    
    myplot$labels$x="Odds ratio"
    myplot$labels$y="Variable"
    
    myplot + theme_gray()+theme(text=element_text(face="bold",size=14,  family="sans"),
                                axis.title=element_text(face="bold",size="12", color="brown"))
  })
 
  
  
  
  ################################################################################
  ## Variance Inflation Factor (VIF)
  ################################################################################
  observeEvent(input$mVarInflationBtn,{
    removeRightBox()
    btn <- input$mVarInflationBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_ModelEval',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        HTML(paste('<b>',"Variance Inflation Factor (VIF)",'</b>')),
        tags$head(
          tags$style(
            "#mVarInflationTxt{color:black; font-size:13.0px; font-style:normal;
overflow-y:scroll; max-height: 450px; background: light-blue;text-align: left;}"
          )
        ),
        verbatimTextOutput("predictorcorrelation"),
        uiOutput("correlationtxt")
      )
    )
    inserted <<- c(id, inserted)
  })
  
 

  output$predictorcorrelation <- renderPrint({
    if (ncol(vmy$fullMod$model)>2){
      car::vif(vmy$fullMod)
    }
    else{
      HTML("variable influence cannot be calculated with one predictor..!")
    }
    
  })
  
  output$correlationtxt <- renderUI({
    ccdf <- as.data.frame(car::vif(vmy$fullMod))
    m5_0corrvar <- rownames(ccdf%>% dplyr::filter(ccdf[1]>5.0))
    m2_5corrvar <- rownames(ccdf%>% dplyr::filter(ccdf[1]>2.5))
    
    
    tags$div(
      tags$p(
        useShinyjs(),
        HTML(paste('<h5><b>',"Interpreting the Variance Inflation Factor (VIF):",'</b>',"as explained in the article referred here",'<br><h5>')),
        HTML(paste('&nbsp','&nbsp','&nbsp',"Variables having correlation > 5.0:")),
        
        if (length(m5_0corrvar)!=0){
          HTML(paste(rownames(as.data.frame(car::vif(vmy$fullMod))[1]%>% dplyr::filter(as.data.frame(car::vif(vmy$fullMod))[1]>5.0))))
        }
        else{
          HTML("NULL")
        },
        
        HTML(paste('<br>','&nbsp','&nbsp','&nbsp',"Variables having correlation > 2.5:")),
        if (length(m2_5corrvar)!=0){
          HTML(paste(rownames(as.data.frame(car::vif(vmy$fullMod))[1]%>% dplyr::filter(as.data.frame(car::vif(vmy$fullMod))[1]>2.5))))
        }
        else{
          HTML("NULL")
        },
        urlvif,
        HTML(paste('<h5>',"Variance inflation factors range from 1 upwards. The numerical value for VIF tells you (in decimal form) what",
                   "percentage the variance (i.e. the standard error squared) is inflated for each coefficient. For example, a VIF of 1.9",
                   "tells you that the variance of a particular coefficient is 90% bigger than what you would expect if there was no",
                   "multicollinearity - if there was no correlation with other predictors.")),
        
        HTML(paste('<h5><b>',"A rule of thumb for interpreting the variance inflation factor:",'</b><br><h5>')),
        HTML(paste('<h5>','&nbsp','&nbsp','&nbsp',"1 = not correlated. ",
                   '<br>','&nbsp','&nbsp','&nbsp',"Between 1 and 5 = moderately correlated.",
                   '<br>','&nbsp','&nbsp','&nbsp',"Greater than 5 = highly correlated.")),
        br(),
        HTML(paste('<h5>',"Exactly how large a VIF has to be before it causes issues is a subject of debate. What is known is that the more your",
                   "VIF increases, the less reliable your regression results are going to be. In general, a VIF above 10 indicates high",
                   "correlation and is cause for concern. Some authors suggest a more conservative level of 2.5 or above."))
      )
    )
  })
  
 
  
  } #server closure
shinyApp(ui, server)

