options(shiny.trace=TRUE)

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com

fnLogUi <- function(...) { cat( file=stderr(), "UI.R:", ..., "\n" ); utils::flush.console() }

cat(file=stderr(), "\n\n\n\n")
fnLogUi( "starting shiny app ui.R, in -", getwd(), '\n' )
fnLogUi( installed.packages()["Rcpp",] )

sCmd <- "env | grep '^R_' | sort 1>&2"
fnLogUi("system(",sCmd,")")
print(file=stderr(),system(sCmd))

sCmd <- "env | grep '^SHINY_' | sort 1>&2"
fnLogUi("system(",sCmd,")")
print(file=stderr(),system(sCmd))



#### if a local library directory exists, prepend it to the library paths
sLibDir <- "./library"
bLocalLib <- isTRUE( file.info(sLibDir)$isdir )
if(bLocalLib){
  fnLogUi("Prepend local directory to library paths")
  fnLogUi(".libPaths()=", .libPaths(),"\n")
  if( ! sLibDir %in% .libPaths() ) .libPaths( c( sLibDir, .libPaths()) )
}
fnLogUi(".libPaths()=", .libPaths(),"\n")



#### load and attach packages
fnLogUi("load and attach packages")
fnLogUi(".packages()=",.packages())
fnLogUi("getNamespaceVersion()=", getNamespaceVersion("Rcpp"))
library(Rcpp)
fnLogUi(".packages()=",.packages())
suppressPackageStartupMessages(library(diseaseRiskPredictor))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinysky))
suppressPackageStartupMessages(library(shinyAce))
fnLogUi(".packages()=",.packages())



# examples code common to client and server
fnRep("create example pedigrees and disease models")
source("examplesSrc.R")




shinyUI( navbarPage("Disease Risk Predictor"
    
    , tabPanel("Intro"
        , h1("Introduction")
        , h2("Purpose")
        , p( "This website allows you to predict disease risk for the individuals of a family")
        , p( "To do this you need to supply")
        , HTML( "<OL>")
        , HTML( "<LI>information on the family members and how they are related")
        , HTML( "<LI>A model for the disease for which you want to obtain risks")
        , HTML( "</OL>")
        , p( "Both the family (pedigree) information and the disease model can be specified in files and uploaded to this app. ")
        , p( "Once the Pedigree and Disease Model are specified, the disease risk for family members can be calculated.")
        , p( "As well as being able to upload your own pedigrees and disease models, you can also select from some example pedigrees and disease models. ")
        , p( "Risks can be calculated for a wide range of multifactorial diseases, Mendelian diseases are not covered. ")
        
        , h2("Getting Started")
        , HTML( "<OL>")
        , HTML( "<LI>Go to the Pedigree tab. Select the first example from the Example Pedigrees drop down (on left hand side). A diagram of the pedigree should be displayed")
        , HTML( "<LI>Go to the Disease Model tab. Select the first example from the Example Disease Models drop down (on left hand side). It should report the model is valid")
        , HTML( "<LI>Go to the Risk Prediction tab. Click the Calc Risk button (on left hand side)")
        , HTML( "</OL>")
        , p( "If successful, a figure and a table will appear on the right hand side of the Risk Prediction page. ")
        , p( "The figure shows the predicted risk per pedigree member. ")
        , p( "The table is an abbreviated version of the pedigree information, each row representing a family member. ")
        , p( "To this table has been added the predicted risk (risk column) for each family member, and also the 5 year risk (nYearRisk column). ")
        , p( "This process of specifying the pedigree information and the disese model, then calculating disease risks is the general procedure. ")
        , p( "Help on how to specify pedigree information is available in the Help on Pedigrees section of the Pedigrees tab. ")
        , p( "Similarly, help on disease model specification is available in the Help on Disease Models section of the Disease Models tab. ")		
    )

        
    , tabPanel("Pedigree"
        ,sidebarLayout(
         sidebarPanel( 
           h1("Pedigree")	
           , p( "This tab concerns specifying pedigree information")
           , h2("File Upload")	
           , fileInput( "dfFileInfoPed", "Upload a Pedigree:" )
           
           , selectInput( inputId="sFilePedEx", label="Select an example pedigree:" 
                          , choices = dfPedExInfo$title
                          , selected = "None Selected"
                          , selectize=F
           )
           
           ,hr()
           
           ,h2("Help on Pedigrees")
           ,p("Family information is specifed in linkage/ped file format.")
           ,p("Briefly, the family is represented in a table, in which each row relates to one individual. The first 6 columns of the table are -")
           , HTML( "<UL>")
           , HTML( "<LI>famid - family id")
           , HTML( "<LI>id - person id")
           , HTML( "<LI>fatherid - father id")
           , HTML( "<LI>motherid - mother id")
           , HTML( "<LI>sex - sex")
           , HTML( "<LI>affected - affection status")
           , HTML( "</UL>")
           ,p("How this information specifies a family tree should become apparent after looking at some example ped files and their accompanying pedigree diagrams.")
           , h4("Example")
           , em("Example pedigree = Nuclear family.") ,p(),p()
           , h3("Other Columns")
           ,p("Other columns are optional. ")
           ,p("The column headings are not allowed to contain underscore characters.") # XXXX should test for this
           ,p("The following columns if they appear have special meanings - ")
           , HTML( "<UL>")
           , HTML( "<LI>deceased - indicates whether the person is deceased or living (this affects pedigree diagram generation)")
           , HTML( "<LI>age - person's age (this is used in risk estimation for non-congenital diseases)")
           , HTML( "<LI>relation.MZ, relation.DZ - indicates twins (see below)")
           , HTML( "</UL>")
           
           ,h3("Twins")
           ,p("Twins can be specified by including the following columns in the ped file")
           , HTML( "<UL>")
           , HTML( "<LI>relation.MZ - indicates identical twin groupings")
           , HTML( "<LI>relation.DZ - indicates non-identical twin groupings")
           , HTML( "</UL>")
           ,p("Each twin grouping is given a non-zero number. How this works should become apparent after looking at an example.")
           , h4("Example")
           , em("Example pedigree = Family with MZ twins.") ,p(),p()
           , em("Example pedigree = Family with MZ twins and DZ triplets.") ,p(),p()
           , em("Example pedigree = Family with quints containing 2 MZ pairs.") ,p(),p()
           
           ,h3("Editting Pedigree Information")
           ,p("When a pedigree file is uploaded the file contents are copied into the Pedigree Information text area (displayed right).")
           ,p("When this is in the correct format, a pedigree diagram is generated and the pedigree information is displayed in a table.")
           ,p("When there is a problem with the pedigree information, the problem is reported and no pedigree diagram is generated.")
           ,p("The pedigree information text area is constantly monitored to see whether its contents are correctly formatted.")
           ,p("As a result it can well be the case that errors are reported when you are only half way through making a change to the pedigree information.")
         ),
         
         mainPanel(
           h2("Pedigree Info (editable)")
           , p("This was going to be an editable table but none of the table widgets worked very well")
           , aceEditor("txtareaPed", value="", mode="asciidoc", height="200px")
           ,hr()
           ,uiOutput("reportPed")
         )
        )
    )
    
    
    , tabPanel("Disease Model"
        , sidebarLayout(
         sidebarPanel( 
           h1("Disease Model")	
           , p( "This tab concerns specifying the disease model")
           , h2("File Upload")	
           , fileInput( "dfFileInfoDis", "Upload a Disease Model:" )
           
           , selectInput( inputId="sFileDisEx", label="Select an example disease model:" 
                          , choices = dfDisExInfo$title
                          , selected = "None Selected"
                          , selectize=F
           )
           
           ,hr()
           ,h2("Help on Disease Models")
           ,p("Disease models for multifactorial diseases are currently supported, those for Mendelian diseases are not supported")
           ,p("The simplest such disease model requires only the specifying of")
           , HTML( "<UL>")
           , HTML( "<LI>the disease's lifetime risk")
           , HTML( "<LI>the disease's heritability")
           , HTML( "</UL>")
           , p("Such a disease model is appropriate for a congenital disease, i.e. one where individuals are affected from birth.")
           , h4("Example")
           , em("Example model = Single Stratum Congenital Disease.") ,p(),p() 
           , em("Example model = Single Stratum Congenital Disease (with Csq).") ,p(),p() 
           , p("Upon loading an example, the contents of the file are loaded into the editable Disease Model text area (displayed right).")
           , p("Below the Disease Model text area, a report will appear regarding whether there is a problem with the disease model as specified in the text.")
           , p("If not, the Disease Model extracted from the Disease Model text is reported.")
           , p("This shows the disease liability variance/convariance matrix (heading Covariance Total) and partitions of it due to")
           , HTML( "<UL>")
           , HTML( "<LI>additive genetic effects (heading Covariance Asq) ")
           , HTML( "<LI>shared environmental effects (heading Covariance Csq)") 
           , HTML( "<LI>unique environmental effects (heading Covariance Esq) ")
           , HTML( "</UL>")
           , p("As currently modelled, shared environmental effects means effects shared across siblings.")
           , p("This disease model is for a congenital disease, that is a disease where affection status is obvious from birth.")
           , p("Non-congenital diseases can also be modelled, see below.")
           
           ,h3("Continuous Risk Factors")
           , p("Many risk factors are continuous variables. For instance, BMI and blood-pressure (continuous variables) are risk factors for several diseases, e.g. stroke")
           , p("The effects of continuous **mean centred** risk factors on lifetime risk can be modelled.")
           , p("To do this one needs to specify")
           , HTML( "<UL>")
           , HTML( "<LI>the covariance of disease liability with the risk factor.")
           , HTML( "<LI>the risk factor variance")
           , HTML( "</UL>")
           , p("These are specified in the disease model text in respectively table elements covMatrixTot[liability,RF] and covMatrixTot[RF,RF], where RF is the risk factor's name.")
           , p("One also needs to specify how the risk factor impacts the partitions of the disease liability variance.")
           , p("This is specified in the disease model text by the table elements covMatrixAsq[RF,RF], covMatrixCsq[RF,RF] and covMatrixEsq[RF,RF]. ")
           , p("These variances should sum to equal the variance of the risk factor, i.e. covMatrixTot[RF,RF]")
           
           , p("Risk factor's effect size may be expressed as a regression of disease liability on the risk factor, or the proportion of disease liability the risk factor explains. ")
           , p("These can be converted into the required covariance using the following formulae")
           , HTML( "<UL>")
           , HTML( "<LI>The regression of liability on RF is <br>regCoeff(liability,RF) = cov(liability,RF) / variance(RF)")
           , HTML( "<LI>The disease liability variance explained by the RF is <br>regCoeff(liability,RF)^2 * variance(RF)")
           , HTML( "</UL>")
           
           , h4("Example")
           , p("One particular class of continuous risk factor are polygenic scores. These are in effect estimates of an individual's genetic liability based on their genome, typically assayed via GWAS chip genotyping.")
           , p("The following example disease model includes a polygenic score (called pgs) as an example of a continuous risk factor.")
           , em("Example model = Congenital disease with Quantitative Risk Factor.") ,p(),p() 
           , p("This polygenic score risk factor is wholely genetic which is specified in the disease model text by covMatrixAsq[pgs,pgs] = 1, covMatrixCsq[pgs,pgs] =0 and covMatrixEsq[pgs,pgs] = 0. ")
           , p("In this case the risk factor pgs explains 10% of disease liability = (0.316/1)^2 * 1.")
           
           ,h3("Categorical Risk Factors")
           , p("Known categorical risk factors for the disease can be modelled.")
           , p("For each category of the risk factor the user supplies ")
           , HTML( "<UL>")
           , HTML( "<LI>the population frequency of category")
           , HTML( "<LI>the relative risk of the category relative to an arbitrarily chosen reference category")
           , HTML( "</UL>")
           , p("The multifactorial disease is modelled by a liability threshold model where disease liability distribution (mean=0, variance=1) is a mixture distribution of risk factor strata liability distributions.")
           , p("The risk factor strata liability distributions are Gaussian and have the same variance, but differ by mean. These means and the variance they share are calculated from the frequency and relative risk information the user provided.")
           , p("One also needs to specify how the risk factor liability is split up over the partitions of the disease liability variance.")
           , p("This is specified in the disease model text by covMatrixAsq[RF,RF], covMatrixCsq[RF,RF] and covMatrixEsq[RF,RF]. ")
           , p("These are proportions and must sum to 1. ")
           , h4("Example")
           , em("Example model = Congenital Disease with Sex-Dependent Lifetime Risk.") ,p(),p() 
           , p("In this example, being female doubles your risk for the disease. ")
           , p("This is specified in the disease model text by the table headed - categoricalRiskFactors. ")
           , p("The sex risk factor is wholely attributed to unique enivonmental effects.  ")
           , p("This is specified in the disease model text by covMatrixAsq[sex,sex] = 0, covMatrixCsq[sex,sex] = 0 and covMatrixEsq[sex,sex] = 1 ")
           , p("It may seem odd to attribute the impact of sex wholely to unique environment however this is correct. ")
           , p("Although one's sex is genetically determined by inheritance of an X or Y chromosome from your father, which of those you get is a random event. ")
           , p("An alternative way to think about this is to ask whether your sex predicts that of your sibling. It doesn't. ")
           , p("One other thing to note about this example is that covMatrixAsq[liability,sex], covMatrixCsq[liability,sex] and covMatrixEsq[liability,sex] are not specified in the disease model text. ")
           , p("This is because the program works these parameters out for you from the risk factor information provided. ")
           , p("To see how this specification converts into a disease model see the Validated Disease Model report and the output headed")
           , HTML( "<UL>")
           , HTML( "<LI>Categorical Risk Factors")
           , HTML( "<LI>Covariance Total, Covariance Asq, Covariance Csq, Covariance Esq ")
           , HTML( "</UL>")
           
           ,h3("Age of Onset Curves")
           , p("Many diseases are not congenital, with people only becoming affected later in life.")
           , p("Such non-congenital diseases can also be modelled by specifying what proportion of those who will ultimately be affected, have become affected by a certain age.")
           , p("How this proportion increases with age is a characteristic of the disease, and is known as the age of onset curve. Such age of onset curves are known for many diseases.")
           , h4("Example")
           , em("Example model = Non-Congenital Disease.") ,p(),p() 
           
           , p("Risk factors may also impact on age of onset. ")
           , p("This can be modelled for categorical risk factors by specifying an Age of onset curve per categorical risk factor stratum.")
           , h4("Example")
           , em("Example model = Non-Congenital Disease with Sex-Dependent Age of Onset.") ,p(),p() 
           
           ,h3("Age")
           , p("When applying a disease model to a pedigree the first thing done is to assign disease model parameters to each individual based on their personal attributes, one of which is age.")
           , p("In the disease model, the age of onset curve parameter expressedProportionOfLifetimeRisk is specified at only a few ages.")
           , p("An individual is assigned an expressedProportionOfLifetimeRisk value by interpolating from the expressedProportionOfLifetimeRisk values given in the disease model ageOfOnset table, according to the individual's age.")
           
           ,h3("Multiple Risk Factors")
           , p("You can specify disease models containing multiple categorical and/or continuous risk factors.")
           , p("In addition to the specification required for each risk factor, the user must also specify how the risk factors covary, for each partition of the disease liability.")
           , p("This is specified in the disease model text by covMatrixAsq[RF1,RF2], covMatrixCsq[RF1,RF2] and covMatrixEsq[RF1,RF2]. ")
           , h4("Example")
           , em("Example model = Non-Congenital Disease with Lifetime Risk dependent on sex and pgs.") ,p(),p() 
           , p("In this example specify covMatrixAsq[RF1,RF2], covMatrixCsq[RF1,RF2] and covMatrixEsq[RF1,RF2] is unproblematic.")
           , p("The risk factor sex is wholey unique environment, while pgs is whole genetic, therefore covMatrixAsq[sex,pgs] = covMatrixCsq[sex,pgs] = covMatrixEsq[sex,pgs] = 0.")
           , h4("Example")
           , em("Example model = Non-Congenital Disease with 3 risk factors.") ,p(),p() 
           , p("This example extends the previous example by the addition of a 3rd risk factor cannabis consumption.")
           , p("Cannabis consumption is modelled as being caused partly by genetic, shared and unique environmental effects.")
           , p("It is also modelled as being more prevalent in males than females.")
           , p("While specifying such a model is straightforward, justifying it might be difficult.")
           
           ,h3("Dropping Model Components")
           , p("The previous example includes a heading - droppedRiskFactors, which has been commented out by starting the line with a #.")
           , p("By uncommenting the droppedRiskFactors heading you will cause the risk factors listed below the droppedRiskFactors heading to be dropped from the model.")
           , p("The droppedRiskFactors feature was deactivated by commenting it out. The sample applies to any feature of the disease model.")
           , p("For instance the disease can be made congenital by commenting out the heading ageOfOnset")
           
           ,h3("Editable Disease Model")
           ,p("When a Disease Model file is uploaded, the file contents are copied into the Disease Model text area (displayed right).")
           ,p("When this is in the correct format, the disease model extracted from text is reported.")
           ,p("When there is a problem with the disease model, the problem is reported.")
           ,p("The Disease Model text area is constantly monitored to see whether its contents are correctly formatted.")
           ,p("As a result it can well be the case that errors are reported when you are only half way through making a change to the Disease Model.")		
         ),
         mainPanel(
           h2("Disease Model (editable)")
           , p("Disease Model Parameter values are delineated by whitespace and new lines, and are identified by specific names/headings.")
           , aceEditor("txtareaDis", value="", mode="asciidoc")
           , hr()
           , uiOutput("reportDis")
         )
        )
    )
    
    
    , tabPanel("Risk Prediction"
        , sidebarLayout(
         sidebarPanel(
           h1("Risk Prediction")
           , conditionalPanel( condition = "output.bNotValidPedAndDis"
                               , p("Disease Risk Prediction is not currently available.")
                               , p("It is only possible when a valid pedigree and disease model have been specified")
           )
           , conditionalPanel( condition = "output.bValidPedAndDis"
                               , h2("Disease Risk Calculator")
                               , busyIndicator( "Risk Estimation In progress", wait = 0)
                               , actionButton( inputId="calcRskBtn", label="Calc Risk" )
           )
           #, radioButtons( inputId="radioGSImpl", label = h3("Implementation"), choices = list("Gibbs Sampler C++" = 1, "Gibbs Sampler R implementation (V SLOW)" = 2), selected = 1)
         ),
         mainPanel(
           conditionalPanel( condition = "output.bRiskEstsCurrent"
                             , uiOutput("reportRisk")
           )
         )
        )
    )
    
    
    , tabPanel("Details"
        , h1("Details")
        , p("This tab reports output generated during risk calculation. It is really just for the developer's user and can be ignored.")
        
        , p("Upon successful risk estimation the following will be displayed here:")
        , HTML( "<UL>")
        , HTML( "<LI>the pedigree's various joint distributions")
        , HTML( "<LI>the disease model parameters assigned to each pedigree member")
        , HTML( "<LI>pedigree member's risks")
        , HTML( "</UL>")
        
        , p( paste("Working directory = ", getwd() ) )
        , p( ".libPaths() = ", paste( .libPaths(), collapse=", ") )
        
        , strong("Error")
        , verbatimTextOutput("CalcRiskEst_error")
        , strong("Warning")
        , verbatimTextOutput("CalcRiskEst_warning")
        , strong("Pedigree's Joint tblCalcRiskEst_mPopCov")
        , tableOutput("tblCalcRiskEst_mPopCov")
        , strong("Pedigree's Joint tblCalcRiskEst_mPopMean")
        , tableOutput("tblCalcRiskEst_mPopMean")
        , strong("Pedigree's Joint tblCalcRiskEst_mPriorCov")
        , tableOutput("tblCalcRiskEst_mPriorCov")
        , strong("Pedigree's Joint tblCalcRiskEst_mPriorMean")
        , tableOutput("tblCalcRiskEst_mPriorMean")
        , strong("Pedigree's Joint tblCalcRiskEst_mPostCov")
        , tableOutput("tblCalcRiskEst_mPostCov")
        , strong("Pedigree's Joint tblCalcRiskEst_mPostMean")
        , tableOutput("tblCalcRiskEst_mPostMean")
        , strong("Pedigree's Joint risk")
        , tableOutput("tblCalcRiskEstPed")
    )
    
    
    , tabPanel("Documentation"
        , h1("Documentation")
        , h2("Methodology")
        , HTML( "<p>This work implements, via a web inteface, the methodology described in Campbell et al. 2010")
        , HTML("(also described in cartoon form <a href=\"http://sgdp.iop.kcl.ac.uk/ddc/gele/current/example.php\" target=\"_blank\">here</a>).")
        , p("This work extends that methodology in a number of ways:")
        , HTML( "<UL>")
        , HTML( "<LI>shared environmental effect can now be modelled. Previous only diseases following an additive genetics and non-shared environment model were modelled.")
        , HTML( "<LI>the effect of quantitative risk factors can now be modelled. Examples of quantitative risk factors include BMI, blood pressure and polygenic score.")
        , HTML( "<LI>simplifies modelling of risk factors. Previous the heritability residual to any risk factors had to be calculated.")
        , HTML( "<LI>Pedigrees that include MZ twins can now be properly modelled.")
        , HTML( "</UL>")		
        , h2("Implementation")
        , p( "The website was written using the R 'shiny' package (RStudio 2014).")
        , p( "Extensive use is made of the 'kinship2' R package (Sinnwell et al. 2014) for pedigree validation and the generation of pedigree diagrams.")
        , p( "To improve the risk calculation response time, the Gibbs Sampler used is written in C++ and integrated into the R program using the 'Rcpp' library (Eddelbuettel & Romain 2011).")
        
        , h2("References")
        , HTML( "<UL>")
        , HTML( "<LI>Campbell, Desmond D., Pak C. Sham, Jo Knight, Harvey Wickham, and Sabine Landau. 'Software for Generating Liability Distributions for Pedigrees Conditional on Their Observed Disease States and Covariates'. Genetic Epidemiology 34, no. 2 (1 February 2010): 159-70. <a target=\"_blank\" href=\"http://dx.doi.org/10.1002/gepi.20446\">doi:10.1002/gepi.20446</a>.")
        , HTML( "<LI>Sinnwell, Jason P., Terry M. Therneau, and Daniel J. Schaid. 'The kinship2 R Package for Pedigree Data'. Human Heredity 78, no. 2 (2014): 91-93. doi:10.1159/000363105.")
        , HTML( "<LI>RStudio and Inc. (2014). shiny: Web Application Framework for R. R package version 0.10.2.2. http://CRAN.R-project.org/package=shiny.")
        , HTML( "<LI>Eddelbuettel, Dirk, and Romain, Fran&ccedilois. 'Rcpp: Seamless R and C++ Integration'. Journal of Statistical Software 40, no. 8 (2011): 1-18.")
        , HTML( "</UL>")
        
        , h2("Citation")
        , p("If you have used this website for risk calculation and wish to cite it, please use the following")
        , HTML( "<UL>")
        , HTML( "<LI>Campbell, Desmond D., Yiming Li, Pak C. Sham, Stacey Cherny ...")
        , HTML( "</UL>")
        
        , h2("Contacts")
        , p( "The writer and main contact regarding this work is")
        , HTML("<address>Desmond Campbell<br>Email: ddc123 'at' hku.hk</><br>Tel: +852 28315156<br>")
        , HTML("L10-64 Lab Block, Faculty of Medicine, 21 Sassoon Road, Pokfulam Road, Central & Western District, Hong Kong<br>")
        , HTML("</address>")
        , HTML("<address>Yiming Li<br>Email: liym1018 'at' hku.hk</><br>Tel: +852 53451983<br>")
        , HTML("L1-05, HKJCBIR, 5 Sassoon Road, Pokfulam, Hong Kong<br>")
        , HTML("</address>")
        , p("Failing that, try contacting any of the other authors at the University of Hong Kong, via either the Dept of Psychiatry or the Centre for Genomic Sciences.")
    )
    
    
    , tabPanel("Download"
        , sidebarLayout(
         sidebarPanel(
           h1("Download")
           , p( "Download command line program and documentation")
           , downloadButton('downloadZip', "Download")
         ),
         mainPanel(
           h2("Download Details")
           , h3("Command Line Program")
           , p("A command line program that does the same job as this website is available for download. This has some extra functionality over that provided by the website")
           , HTML( "<UL>")
           , HTML( "<LI>It can estimate the precision of risk estimates generated")
           , HTML( "<LI>It generates a file containing 1000s of draws from the pedigree's joint liability distribution after conditioning on all disease risk relevant information")
           , HTML( "</UL>")
           , p("A word of warning, the installation involves installing some R packages and Rtools, so is not worth it unless you want the extra functionality.")
           , h3("Validiation Testing")
           , p("The download also contains the testing done to validate that the command line program and the website correctly estimate risk.")
           , h3("Instructions for Installation")
           , HTML( "<UL>")
           , HTML( "<LI>Download the zip file containing the program, documentation, example files, etc.")
           , HTML( "<LI>Unzip the zip file on your local machine")
           , HTML( "<LI>Read the README.html")
           , HTML( "</UL>")
         )
        )
    )
    
    
    , tabPanel("Acknowledgements"
        , h1("Acknowledgements")
        , HTML("<p>Construction of this website was supported by the following.")
        
        , h2("EU-GEI")
        , HTML("<p>The aim of the <a href=\"http://www.eu-gei.eu\" target=\"_blank\">EU-GEI project</a> (EUropean Network of national schizophrenia networks studying Gene-Environment Interactions)")
        , HTML("is to identify the interactive genetic, clinical and environmental determinants, involved in the development, severity and outcome of schizophrenia.")
        , p( "The EU-GEI project is funded by the European Community's Seventh Framework Programme grant HEALTH-F2-2010-241909 (Project EU-GEI).")
        
        , h2("Research Grants Council, Hong Kong")
        , HTML("<p>The funding obtained from the <a href=\"http://www.ugc.edu.hk/eng/rgc/index.htm\" target=\"_blank\">RGC</a> was via the General Research Fund, specifically")
        , HTML( "<UL>")
        , HTML( "<LI>Project Title: Method and Software for Personal Risk Profiling of Complex Diseases")
        , HTML( "<LI>Applied for 2011/12")
        , HTML( "<LI>RGC Ref No.777511")
        , HTML( "</UL>")
    )
    
))
