#!/usr/bin/env Rscript

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com
#

#### options regarding error and warning
options(shiny.trace=TRUE)
#options(shiny.error=browser)
#options(error=NULL)
#options( warn=0, error=recover)
#options( warn=2, error=recover)
options( warn=0, error=NULL)
#options( warn=2, error=NULL) # use this setting for formal testing

options(stringsAsFactors = FALSE) # hurray!!!



graphics.off()

fnLogSr <- function(...) { cat( file=stderr(), "SERVER.R:", ..., "\n" ); utils::flush.console() }

cat(file=stderr(), "\n\n\n\n")
fnLogSr( "starting shiny app server.R, in -", getwd(), '\n' )

#### if a local library directory exists, prepend it to the library paths
sLibDir <- "./library"
bLocalLib <- isTRUE( file.info(sLibDir)$isdir )
if(bLocalLib){
  fnLogSr("Prepend local directory to library paths")
  fnLogSr( ".libpaths()=", .libPaths() )
  if( ! sLibDir %in% .libPaths() ) .libPaths( c( sLibDir, .libPaths()) )
}
fnLogSr( ".libpaths()=", .libPaths() )

#### load and attach packages
fnLogSr("load and attach packages")
fnLogSr(".packages()=",.packages())
library(Rcpp)
library(diseaseRiskPredictor)
fnRep <- function( ..., file=stderr() ) { cat( "REPORT:", ..., "\n", file=file ); utils::flush.console() }
if(T){
  fnRep( "patch kinship2::plot.pedigree()")
  assignInNamespace( "plot.pedigree", diseaseRiskPredictor:::plot.pedigree.FIXED, ns="kinship2")
}
library(shiny)
library(shinysky)
library(shinyAce)
fnLogSr(".packages()=",.packages())



# examples code common to client and server
fnRep("create example pedigrees and disease models")
source("examplesSrc.R")



# creates a per process file for any plots to go into
# rely on process death to close the plot file
bPlotToFile <- !interactive()
bPlotToFile <- T
if(bPlotToFile) {
  sFilePlot <- paste0(tempfile(),".pdf")
  fnRep("Create a temp file for any plots to be written into -", sFilePlot )
  pdf(sFilePlot, paper="a4")
  fnStr(sFilePlot)
  iDevFilePlot <- dev.cur()
  iDevFilePlot
}



# report on setup
cat("\n")
fnRep("START OF setup diagnostic info")
fnRep("Below is some diagnostic information regarding the setup")
fnRep("sessionInfo() reports")
print( sessionInfo())
cat("\n")
fnRep("system('g++ -v') reports")
print(system('g++ -v'))
cat("\n")
fnRep("system('uname -a') reports")
print(system('uname -a'))
cat("\n")
fnRep("Sys.getenv('PATH') reports")
print(Sys.getenv('PATH'))
print(strsplit( split=";",Sys.getenv('PATH'))[[1]])
cat("\n")
if( 'devtools' %in% installed.packages()[,"Package"] ){
  fnRep("devtools::find_rtools(debug=TRUE) reports")
  print(devtools::find_rtools(debug=TRUE))
  cat("\n")
}
fnRep("END OF setup diagnostic info\n")



# make ped diagram and save to file
fnCreatePedDiagramInFile <- function( oPedigree )
{
  sFileImage <- tempfile()
  tryCatch( {
    png(sFileImage)
    plot(oPedigree)
  }
  , finally={ dev.off() }
  )
  sFileImage
}


fnGetTimeStamp <- function()
{
  op <- options(digits.secs = 6)
  oTime <- Sys.time()
  options(op)
  oTime
}



suppressPackageStartupMessages(library(abind))
fnCovForRenderHotable <- function(lDis,sPart)
{
  fnRep("fnCovForRenderHotable()")
  fnStr(sPart)
  aCov <- lDis$aCov
  if(is.null(aCov)) return(NULL)
  mTmp <- adrop(aCov[,,sPart,drop=F],drop=3)
  mCov <- as.data.frame( mTmp)
  dfCov <- cbind( data.frame(variables=rownames(mCov)), mCov)
  fnStr(dfCov)
  dfCov
}



fnAddHtml <- function( LL, lHtml )
{
  if ( !is.null(lHtml)) LL[[ length(LL) + 1 ]] <- HTML( as.character(lHtml))
  LL
}
fnAddText <- function( LL, sValue )
{
  if ( !is.null(sValue)) LL[[ length(LL) + 1 ]] <- renderText(sValue)
  LL
}
fnAddTable <- function( LL, sValue )
{
  if ( !is.null(sValue)) LL[[ length(LL) + 1 ]] <- renderTable(sValue,readOnly=T)
  LL
}

fnGetDataTableFromString <- function( sTbl )
{
  dfTbl <- NULL
  if( nchar(sTbl)>1 ) {
    # convert text to table
    txtCon <- textConnection(sTbl)
    dfTbl <- tryCatch( 
      read.table( file=txtCon, sep="", header=T, na.strings=c("NA",".") )
      , finally={ close(txtCon) }
    ) # end of tryCatch
  }
  dfTbl
}



# this code is for running the app on local machine
if(F){
  # XXXX copy and paste below to R terminal to launch in browser on local machine
  rm(list=ls())
  sAppDir <- "diseaseriskpredictor"
  sWD <- file.path("c:/Desmond/HKU/EuGei/riskApp/16dec15",sAppDir)
  setwd(sWD)
  
  runApp()
}


#### SSSS ############################################

shinyServer(function(input, output, session) {
fnRep( "START shinyServer()", getwd(), "\n") 
 
  #### risk calc impl control
  # state info regarding risk calc impl
  m_bGibbsSamplerImplCpp <- T
  
  # get Gibbs Sampler impl
  # XXXX not implemented - aim here was to have a button for choosing between R and C++ Gibbs samplers
  fnGetGibbsSampler <- reactive({
    fnRep("fnGetGibbsSampler()")
    
    a <- input$radioGSImpl
    
    if( input$radioGSImpl == 1 ) {
      m_bGibbsSamplerImplCpp <<- T
    } else if ( input$radioGSImpl == 3 ) {
      m_bGibbsSamplerImplCpp <<- F
    } else {
      stop("Unexpected value encountered for - input$radioGSImpl")
    }
    NULL
  })
  
  
  
  #### PPPP Ped control
  
  # ped file info can come from two sources
  # this common set of variables is when either of these events happens 
  m_dfFileInfoPed <- NULL
  m_oTimeFileInfoPed <- NULL
  
  fnSet_m_dfFileInfoPed <- function( dfFileInfo )
  {
    m_dfFileInfoPed <<- dfFileInfo
    m_oTimeFileInfoPed <<- fnGetTimeStamp()
    return()
  }
  
  
  
  # get file info for uploaded Ped file
  fnGetFileInfoPedUp <- reactive({
    fnRep("fnGetFileInfoPedUp()")	
    dfFileInfoPed <- input$dfFileInfoPed
    fnSet_m_dfFileInfoPed( dfFileInfoPed )
  })
  
  # get file info for selected Ped example file
  fnGetFileInfoPedEx <- reactive({
    fnRep("fnGetFileInfoPedEx() BEGIN")
    
    sFilePed <- input$sFilePedEx
    fnStr(sFilePed)
    vbxPedEx <- dfPedExInfo$title == sFilePed
    if( !sum(vbxPedEx) == 1) stop( paste("Expected exactly one match for sFilePed =", sFilePed))
    
    # construct file info
    dfFileInfo <- NULL
    if( "None Selected" != sFilePed ) {
      # create required data frame of file info as per fnGetFileInfoPedUp()
      dfFileInfo <- data.frame( 
        name=dfPedExInfo$path[vbxPedEx], size=NA, type="", datapath=dfPedExInfo$path[vbxPedEx] )
    }
    
    fnSet_m_dfFileInfoPed( dfFileInfo )
    
    fnRep("fnGetFileInfoPedEx() END")
  })
  
  # get Ped file info
  fnGetFileInfoPed <- reactive({
    fnRep("fnGetFileInfoPed()")	
    fnGetFileInfoPedUp()
    fnGetFileInfoPedEx()
    return(m_dfFileInfoPed)
  })
  
  # update Ped data
  fnUpdatePed <- reactive({ fnUpdateFromAcePed() })
  
  
  
  output$reportPed <- renderUI({
    fnRep("reportPed() BEGIN")
    lCheckPed <- fnCheckPed()
    if( is.null(lCheckPed) ) return()
    dfPed <- lCheckPed$dfPed
    
    if( is.null(dfPed) & is.null(lCheckPed$sErrMsg) ) return() # empty case or not a table
    
    LL <- list()
    # NB assignment of value to the rendering function needs to be via a uniquely named variable (or via a function). 
    # Reuse of a variable (example on line below) won't work; a variable cannot be reused for multiple assignments
    # This won't work: sText <- "aaa"; LL[[ length(LL)+1]] <- renderText(sValue); sText <- "bbb"; LL[[ length(LL)+1]] <- renderText(sValue)
    
    bValid <- !is.null(dfPed) & is.null(lCheckPed$sErrMsg)
    if (bValid) {
      # report valid ped
      
      # make ped diagram
      lImage <- fnMakePedDia()
      fnStr(lImage)
      if( is.null(lImage) ) {
        lImage = list()
        lImage$src = "blank.png"
      }
      # display ped diagram or report problem
      if ( is.null(lImage$sErrMsg) ) {
        LL <- fnAddHtml (LL, h4("Pedigree Diagram"))
        LL[[ length(LL) + 1 ]] <- renderImage( lImage, deleteFile=T )
        LL <- fnAddHtml (LL, br())
        LL <- fnAddHtml (LL, br())
        LL <- fnAddHtml (LL, br())
      } else {
        LL <- fnAddHtml (LL, h4("Pedigree apparently fine but drawing pedigree diagram failed"))
        LL <- fnAddHtml (LL, h4("Image Generation Error Message"))
        LL <- fnAddText (LL, lImage$sErrMsg)
      }
      
    } else {
      # report invalid ped
      LL <- fnAddHtml (LL, h4("Invalid Pedigree"))
      sText <- lCheckPed$sErrMsg
      if ( !is.null(sText)) LL <- fnAddText (LL, sText)
    }
    
    # display pedigree info table
    if(!is.null(dfPed)){
      fnRender <- renderTable( expr={ dfPed }, readOnly=F )
      #fnStr(fnRender)
      #print(fnRender)
      LL <- fnAddHtml (LL, hr())
      LL <- fnAddHtml (LL, h4("Extracted pedigree information"))
      LL[[ length(LL) + 1 ]] <- fnRender
    }
    
    fnRep("reportPed() END")
    LL
  })
  
  # read Ped data from Ace
  fnUpdateFromAcePed <- reactive( {
    fnRep("fnUpdateFromAcePed XXXX ()")
    
    sPed <- input$txtareaPed
    fnStr(sPed)
    
    # get data.table from string
    dfPed <- fnGetDataTableFromString( sPed )
    
    return( list( dfPed=dfPed, oTimePed=fnGetTimeStamp() ) )
  })
  
  
  fnCheckPed <- reactive({
    fnRep("fnCheckPed()")
    
    lOut <- list() # needed because otherwise lOut will not exist if an exception is thrown in fnUpdatePed()
    lOut <- tryCatch({
      lOut <- fnUpdatePed()
      if( !is.null(lOut$dfPed) ) {
        lOut$dfPed <- fnValidatePedigree( lOut$dfPed )
        oPedigree <- fnConstructPedigree( lOut$dfPed )
        lOut$oPedigree <- oPedigree
      }
      lOut
    }
    , warning=function(w) { 
      lOut$sWrnMsg <- w$message
      lOut$w <- w; fnStr(w)
      lOut
    }
    , error=function(e) { 
      lOut$sErrMsg <- e$message
      lOut$e <- e; fnStr(e)
      lOut
    }
    ) # end of tryCatch
    
    fnStr(lOut)
    #if(is.null(lOut$dfPed)) return(NULL)
    lOut
  })
  
  
  
  fnMakePedDia <- reactive({
    fnRep("FNMAKEPEDDIA()")
    
    oPedigree <- fnCheckPed()$oPedigree
    if( is.null(oPedigree) ) return()
    fnStr(oPedigree)
    
    lOut <- tryCatch({
      
      lOut <- list()
      
      # create pedigree diagram and write to file
      fnRep("create pedigree diagram and write to file")
      sFileImage <- tryCatch( 
        fnCreatePedDiagramInFile( oPedigree )
        ,warning=function(w){ return(w) }
      )
      if( "warning" %in% class(sFileImage)) {
        oWarning <- sFileImage; sFileImage <- NA
        lOut$sWrnMsg <- oWarning$message
        lOut$oWarning <- oWarning
        
        # redo, this time ignoring warnings
        sFileImage <- suppressWarnings( fnCreatePedDiagramInFile( oPedigree ) )
      }		
      lOut$src = sFileImage
      
      lOut
    }
    , warning=function(w) { 
      lOut$sWrnMsg <- w$message
      lOut$w <- w; fnStr(w)
      lOut
    }
    , error=function(e) { 
      lOut$sErrMsg <- e$message
      lOut$e <- e; fnStr(e)
      lOut
    }
    ) # end of tryCatch
    
    fnStr(lOut)
    fnRep("FNMAKEPEDDIA()")
    lOut
  })
  
  
  
  #### DDDD Dis control
  
  # state info regarding Dis
  
  # dis file info can come from two sources
  # this common set of variables is when either of these events happens 
  m_dfFileInfoDis <- NULL
  m_oTimeFileInfoDis <- NULL
  
  fnSet_m_dfFileInfoDis <- function( dfFileInfo )
  {
    m_dfFileInfoDis <<- dfFileInfo
    m_oTimeFileInfoDis <<- fnGetTimeStamp()
    return()
  }
  
  
  
  # get file info for uploaded Dis file
  fnGetFileInfoDisUp <- reactive({
    fnRep("fnGetFileInfoDisUp()")
    
    fnSet_m_dfFileInfoDis( input$dfFileInfoDis )
  })
  
  # get file info for selected Dis example file
  fnGetFileInfoDisEx <- reactive({
    fnRep("fnGetFileInfoDisEx() BEGIN")
    
    sFileDis <- input$sFileDisEx
    fnStr(sFileDis)
    vbxDisEx <- dfDisExInfo$title == sFileDis
    if( !sum(vbxDisEx) == 1) stop( paste("Expected exactly one match for sFileDis =", sFileDis))
    
    dfFileInfo <- NULL
    if( "None Selected" != sFileDis ) {
      # create required data frame of file info as per fnGetFileInfoDisUp()
      dfFileInfo <- data.frame( 
        name=dfDisExInfo$path[vbxDisEx], size=NA, type="", datapath=dfDisExInfo$path[vbxDisEx] )
    }
    fnSet_m_dfFileInfoDis( dfFileInfo )
    
    fnRep("fnGetFileInfoDisEx() END")
    
    return()
  })
  
  # get Dis file info
  fnGetFileInfoDis <- reactive({
    fnRep("fnGetFileInfoDis()")
    fnGetFileInfoDisUp()
    fnGetFileInfoDisEx()
    return(m_dfFileInfoDis)
  })
  
  output$reportDis <- renderUI({
    
    lCheckDis <- fnCheckDis()
    if( is.null(lCheckDis) ) return()
    lDis <- lCheckDis$lDis
    
    if( is.null(lDis) & is.null(lCheckDis$sErrMsg) ) return() # empty case
    
    LL <- list()
    # NB assignment of value to the rendering function needs to be via a uniquely named variable (or via a function). 
    # Reuse of a variable (example on line below) won't work; a variable cannot be reused for multiple assignments
    # This won't work: sText <- "aaa"; LL[[ length(LL)+1]] <- renderText(sValue); sText <- "bbb"; LL[[ length(LL)+1]] <- renderText(sValue)
    
    # report disease model validity
    bValid <- is.null(lCheckDis$sErrMsg)
    if (bValid) {
      LL <- fnAddText(LL, "The Disease Model Text above specifies a valid disease model which is reported below")
      
    } else {
      LL <- fnAddText(LL, "The Disease Model Text above does not specifies a valid disease model. Below is reported the model as specified up until a problem was detected")
      LL <- fnAddHtml (LL, h4("Error Message(s)"))
      for( sText in lCheckDis$lDis$sErrMsg ) LL <- fnAddText (LL, sText)
    }
    
    if (bValid) {
      LL <- fnAddHtml (LL, h4("Valid Disease Model"))
    } else {
      LL <- fnAddHtml (LL, h4("Invalid Disease Model"))
    }
    
    # report disease model
    sText <- lDis$title
    if ( !is.null(sText)) {
      LL <- fnAddHtml (LL, h4("Title"))
      LL <- fnAddText (LL, sText)
    }
    sText <- lDis$description
    if ( !is.null(sText)) {
      LL <- fnAddHtml (LL, h4("Description"))
      LL <- fnAddText (LL, sText)
    }
    sText <- lDis$lifetimeRisk
    if ( !is.null(sText)) {
      LL <- fnAddHtml (LL, h4("Lifetime Risk"))
      LL <- fnAddText (LL, sText)
    }
    dfCatRF <- lDis$categoricalRiskFactors
    if ( !is.null(dfCatRF)) {
      LL <- fnAddHtml (LL, h4("Categorical Risk Factors"))
      LL <- fnAddTable (LL, dfCatRF)
    }
    dfCov <- fnCovForRenderHotable(lDis,"Tot")
    if ( !is.null(dfCov)) {
      LL <- fnAddHtml (LL, h4("Covariance Total"))
      LL <- fnAddTable (LL, dfCov)
    }
    dfCov <- fnCovForRenderHotable(lDis,"Asq")
    if ( !is.null(dfCov)) {
      LL <- fnAddHtml (LL, h4("Covariance Asq"))
      LL <- fnAddTable (LL, dfCov)
    }
    dfCov <- fnCovForRenderHotable(lDis,"Csq")
    if ( !is.null(dfCov)) {
      LL <- fnAddHtml (LL, h4("Covariance Csq"))
      LL <- fnAddTable (LL, dfCov)
    }
    dfCov <- fnCovForRenderHotable(lDis,"Esq")
    if ( !is.null(dfCov)) {
      LL <- fnAddHtml (LL, h4("Covariance Esq"))
      LL <- fnAddTable (LL, dfCov)
    }
    dfAOO <- lDis$ageOfOnset
    if ( !is.null(dfAOO)) {
      LL <- fnAddHtml (LL, h4("Age Of Onset"))
      LL <- fnAddTable (LL, dfAOO)
    }
    
    LL
  })
  
  
  
  # read Dis data from Ace
  fnUpdateFromAceDis <- reactive( {
    fnRep("fnUpdateFromAceDis XXXX ()")
    
    sDis <- input$txtareaDis
    fnStr(sDis)
    
    lDis <- NULL
    if( nchar(sDis)>1 ) {
      # convert into a vector of lines
      vsLines <- strsplit( sDis, split="\n" )[[1]]
      
      # read object from lines
      lDis <- fnReadLinesDis( vsLines )
    }
    fnStr(lDis)
    
    return( list( lDis=lDis, oTimeDis=fnGetTimeStamp() ) )
  })
  
  
  
  # update Dis data
  fnUpdateDis <- reactive({ fnUpdateFromAceDis() })
  
  
  
  fnCheckDis <- reactive({
    fnRep("fnCheckDis()")
    
    lOut <- list() # needed because otherwise lOut will not exist if an exception is thrown in fnUpdateDis()
    lOut <- tryCatch({
      lOut <- fnUpdateDis()
      if(!is.null(lOut$lDis$sErrMsg)) lOut$sErrMsg <- "Problems with disease model"
      lOut
    }
    , warning=function(w) { 
      lOut$sWrnMsg <- w$message
      lOut$w <- w; fnStr(w)
      lOut
    }
    , error=function(e) { 
      lOut$sErrMsg <- e$message
      lOut$e <- e; fnStr(e)
      lOut
    }
    ) # end of tryCatch
    
    fnStr(lOut)
    lOut
  })
  
  
  
  #### Calc Risk
  
  fnCalcRiskEst <- reactive({
    fnRep("fnCalcRiskEst()")
    
    if( is.null(input$calcRskBtn) || input$calcRskBtn==0 ) return()
    fnStr(input$calcRskBtn)
    
    isolate({
      lCheckPed <- fnCheckPed()
      lCheckDis <- fnCheckDis()
    })
    
    if( is.null(lCheckPed) | !is.null(lCheckPed$sErrMsg) ) return()
    dfPed <- lCheckPed$dfPed
    if( is.null(dfPed) ) return()
    
    if( is.null(lCheckDis) || !is.null(lCheckDis$sErrMsg) ) return()
    lDis <- lCheckDis$lDis
    if( is.null(lDis) ) return()
    
    lOut <- tryCatch({
      
      fnRep('calc pedigree prior liability distribution')
      lPedDis <- fnPrepareForRiskPrediction( dfPed, lDis )
      
      fnRep("predict pedigree members' risks")
      lOut <- fnPredictRisk( lPedDis, bVerbose=T )
      print(names(lOut))
      lOut$lCheckPed <- lCheckPed
      lOut$lCheckDis <- lCheckDis
      
      lOut
    }
    
    , warning=function(w) { 
      lOut$sWrnMsg <- w$message
      lOut$w <- w; fnStr(w)
      lOut
    }
    , error=function(e) { 
      lOut$sErrMsg <- e$message
      e$traceback <- traceback()
      lOut$e <- e; fnStr(e)
      lOut
    }
    ) # end of tryCatch
    
    lOut$oTime <- fnGetTimeStamp()
    fnStr(lOut)
    lOut
  })
  
  
  
  #### Calc Risk
  

  
  # determines whrther the Calc Risk buttin is available
  fnValidPedAndDis <- reactive({
    lCheckPed <- fnCheckPed()
    lCheckDis <- fnCheckDis()
    
    if( is.null(lCheckPed) | is.null(lCheckPed$dfPed) ) return(FALSE)
    if( is.null(lCheckDis) | is.null(lCheckDis$lDis) ) return(FALSE)
    
    is.null(lCheckPed$sErrMsg) & is.null(lCheckDis$sErrMsg)
  })
  
  output$bValidPedAndDis <- reactive({ fnValidPedAndDis() })
  outputOptions(output, 'bValidPedAndDis', suspendWhenHidden=FALSE)
  
  output$bNotValidPedAndDis <- reactive({	!fnValidPedAndDis() })
  outputOptions(output, 'bNotValidPedAndDis', suspendWhenHidden=FALSE)
  
  
  
  # indicates whether the risk is out of date
  output$bRiskEstsCurrent <- reactive({
    fnRep("output$bRiskEstsCurrent")
    
    lRiskEst <- fnCalcRiskEst()
    if(is.null(lRiskEst)) return(FALSE)
    
    if( ! fnValidPedAndDis() ) return(FALSE)
    
    lCheckPed <- fnCheckPed()
    lCheckDis <- fnCheckDis()
    
    lRiskEst$oTime > lCheckPed$oTimePed && lRiskEst$oTime > lCheckDis$oTimeDis
  })
  outputOptions(output, 'bRiskEstsCurrent', suspendWhenHidden=FALSE)
  
  
  
  output$reportRisk <- renderUI({
    
    lRiskEst <- fnCalcRiskEst()
    if(is.null(lRiskEst)) return()
    
    LL <- list()
    # NB assignment of value to the rendering function needs to be via a uniquely named variable (or via a function). 
    # Reuse of a variable (example on line below) won't work; a variable cannot be reused for multiple assignments
    # This won't work: sText <- "aaa"; LL[[ length(LL)+1]] <- renderText(sValue); sText <- "bbb"; LL[[ length(LL)+1]] <- renderText(sValue)
    
    bValid <- is.null(lRiskEst$sErrMsg)
    if (bValid) {
      LL <- fnAddHtml (LL, h3("Disease Risk Predictions"))
      
      dfPedRisk <- lRiskEst$dfPedRisk
      # prune columns to ids, risk factors and risk related
      vsRF <- dimnames(lRiskEst$lDis$aCov)[[1]]
      vsRF <- vsRF[ vsRF != "liability" ]
      fnStr(vsRF)
      vbxCols <- colnames(dfPedRisk) %in% c("id","affected","age","expressedProportionOfLifetimeRisk","risk","nofYears","nYearRisk",vsRF)
      dfPedRisk <- dfPedRisk[,vbxCols,drop=F]	
      fnStr(dfPedRisk)
      
      LL[[ length(LL) + 1 ]] <- renderPlot( 
        fnBarPlotRisk( dfPedRisk )
      )
        
      LL <- fnAddTable ( LL, dfPedRisk)
      
    } else {
      LL <- fnAddHtml (LL, h3("Problem with Disease Risk Prediction"))
      LL <- fnAddText (LL, lRiskEst$sErrMsg)
    }
    
    LL
  })
  
  
  
  #### Details
  
  output$tblCalcRiskEst_mPopCov <- renderTable({ fnCalcRiskEst()$mPopCov })
  output$tblCalcRiskEst_mPopMean <- renderTable({ fnCalcRiskEst()$mPopMean })
  output$tblCalcRiskEst_mPriorCov <- renderTable({ fnCalcRiskEst()$mPriorCov })
  output$tblCalcRiskEst_mPriorMean <- renderTable({ fnCalcRiskEst()$mPriorMean })
  output$tblCalcRiskEst_mPostCov <- renderTable({ fnCalcRiskEst()$mPostCov })
  output$tblCalcRiskEst_mPostMean <- renderTable({ fnCalcRiskEst()$mPostMean })
  output$tblCalcRiskEstPed <- renderTable({ fnCalcRiskEst()$dfPedRisk })
  output$CalcRiskEst_error <- renderPrint({ fnCalcRiskEst()$e })
  output$CalcRiskEst_warning <- renderPrint({ fnCalcRiskEst()$w })
  
  
  
  observe({
    fnRep("OBSERVE DIS - UPDATEACEEDITOR BEGIN")
    
    dfFileInfoDis <- fnGetFileInfoDis()
    fnStr(dfFileInfoDis)
    
    vsLines <- " "
    if( !is.null(dfFileInfoDis) ){
      
      # read file into a vector of lines
      sFile <- dfFileInfoDis$datapath
      fnStr(sFile)
      vsLines <- tryCatch( {
        readLines(sFile, n = -1) # read file into a vector of lines
      }
      , warning=function(w) w
      , error=function(e) e
      ) # end of tryCatch
      if( "warning" %in% class(vsLines)) vsLines <- c( "Problem with reading Dis file: ", vsLines )
      if( "error"   %in% class(vsLines)) vsLines <- c( "Problem with reading Dis file: ", vsLines )
    }
    fnStr(vsLines)
    
    # paste lines into one string
    sDis <- paste( vsLines, collapse="\n")
    fnStr(sDis)
    
    updateAceEditor( session, editorId="txtareaDis", value=sDis, readOnly=F, mode="asciidoc" )
    fnRep("OBSERVE DIS - UPDATEACEEDITOR END")
  })
  
  observe({
    fnRep("OBSERVE PED - UPDATEACEEDITOR BEGIN")
    
    dfFileInfoPed <- fnGetFileInfoPed()
    fnStr(dfFileInfoPed)
    
    vsLines <- " "
    if( !is.null(dfFileInfoPed) ){	
      
      # read file into a vector of lines
      sFile <- dfFileInfoPed$datapath
      fnStr(sFile)
      vsLines <- tryCatch( {
        readLines(sFile, n = -1) # read file into a vector of lines
      }
      , warning=function(w) w
      , error=function(e) e
      ) # end of tryCatch
      if( "warning" %in% class(vsLines)) vsLines <- c( "Problem with reading Ped file: ", vsLines )
      if( "error"   %in% class(vsLines)) vsLines <- c( "Problem with reading Ped file: ", vsLines )
    }
    fnStr(vsLines)
    
    # paste lines into one string
    sPed <- paste( vsLines, collapse="\n")
    fnStr(sPed)
    
    updateAceEditor( session, editorId="txtareaPed", value=sPed, readOnly=F, mode="asciidoc" )
    fnRep("OBSERVE PED - UPDATEACEEDITOR END")
  })
  
  
  
  # download zip file name
  m_sFileDownloadZip <- "diseaseRiskPredictionTestHarness.zip"
  
  output$downloadZip <- downloadHandler(
    filename = m_sFileDownloadZip,
    content = function(file) file.copy( from=m_sFileDownloadZip, to=file )
  )
  
})
