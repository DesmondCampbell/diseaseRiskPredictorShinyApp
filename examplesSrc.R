#### This file contains code that create data frames containing info on examples 
#### used in the disease risk predictor shiny app. 
#### 



#### create ped file examples info
fnRep("create ped file examples info")

sDirPedEx <- "pedigrees" # directory containing sample disease pedigree files
vsPathPedEx <- dir(sDirPedEx, full=T)
# discard directories
vbx <- !file.info(vsPathPedEx)$isdir
vsPathPedEx <- vsPathPedEx[vbx]

dfPedExInfo <- data.frame( file=basename(vsPathPedEx), title=NA, path=vsPathPedEx, stringsAsFactors=F )
# get title for each pedigree from file first line
sRegExpTitleSignifier <- "^#### "
for( rr in 1:nrow(dfPedExInfo)){
	sFirstLine <- readLines(dfPedExInfo$path[rr], n=1)
	if(!grepl( sRegExpTitleSignifier, sFirstLine ) ) next
	sTitle <- sub( sRegExpTitleSignifier, "", sFirstLine )
	dfPedExInfo$title[rr] <- sTitle
}

# reorder
dfPedExInfo <- dfPedExInfo[order(dfPedExInfo$title),]

# add none selected
dfPedExInfo <- rbind( data.frame( file=NA, title="None Selected", path=NA, stringsAsFactors=F ), dfPedExInfo )

# validate
if(length(unique(dfPedExInfo$title)) != nrow(dfPedExInfo)) stop("The set of Ped file titles contains one or more duplicates")

fnStr(dfPedExInfo)



#### create disease model file examples info
fnRep("create disease model file examples info")

sDirDisEx <- "diseaseModels" # directory containing sample disease model files
vsFileDisEx <- dir(sDirDisEx)
dfDisExInfo <- data.frame( file=vsFileDisEx, title=NA, stringsAsFactors=F )
dfDisExInfo$path <- file.path(sDirDisEx,dfDisExInfo$file)
for( rr in 1:nrow(dfDisExInfo)) {
  lDisFile <- fnReadFile( dfDisExInfo$path[rr], NULL, "title", NULL )
  sTitle <- NA; sTitle <- ifelse( is.null(lDisFile$title), dfDisExInfo$file[rr], lDisFile$title )
  dfDisExInfo$title[rr] <- sTitle
}
# sort
dfDisExInfo$bNumbered <- grepl("^[0-9]", dfDisExInfo$title )
dfDisExInfo <- dfDisExInfo[order(dfDisExInfo$bNumbered,dfDisExInfo$title),]
dfDisExInfo$bNumbered <- NULL

# add none selected
dfDisExInfo <- rbind( data.frame( file=NA, title="None Selected", path=NA, stringsAsFactors=F ), dfDisExInfo )

# validate
if(length(unique(dfDisExInfo$title)) != nrow(dfDisExInfo)) stop("The set of Dis file titles contains one or more duplicates")

fnStr(dfDisExInfo)
