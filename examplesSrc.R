
library(diseaseRiskPredictor)
library(hellno)
str(data.frame(letters)[,1])

# create ped file examples info
sDirPed <- "pedigrees"
sDirPedEx <- file.path(sDirPed)
vsFilePedEx <- dir(sDirPedEx)
dfPedExInfo <- data.frame( file=c(NA,vsFilePedEx), title=NA, stringsAsFactors=F )
dfPedExInfo$title[ is.na(dfPedExInfo$file) ] <- "None Selected"
dfPedExInfo$title[ dfPedExInfo$file=="ped.p2c2.tsv" ] <- "Nuclear family"
dfPedExInfo$title[ dfPedExInfo$file=="ped.g4p2c3.tsv" ] <- "Nuclear family with grandparents"
dfPedExInfo$title[ dfPedExInfo$file=="kinship2.sample.ped.2.mzdz.tsv" ] <- "Twins example"
dfPedExInfo$path <- file.path(sDirPedEx,dfPedExInfo$file)
dfPedExInfo$path[ is.na(dfPedExInfo$file) ] <- NA
vbx <- is.na(dfPedExInfo$title)
dfPedExInfo$title[vbx] <- dfPedExInfo$file[vbx]

# validate
if(length(unique(dfPedExInfo$title)) != nrow(dfPedExInfo)) stop("The set of Ped file titles contains one or more duplicates")
fnStr(dfPedExInfo)
print(dfPedExInfo)


# create dis file examples info
sDirDis <- "diseaseModels"
sDirDisEx <- file.path(sDirDis)
vsFileDisEx <- dir(sDirDisEx)
dfDisExInfo <- data.frame( file=vsFileDisEx, title=NA, stringsAsFactors=F )
dfDisExInfo$path <- file.path(sDirDisEx,dfDisExInfo$file)
for( rr in 1:nrow(dfDisExInfo)) {
  sFileDis <- dfDisExInfo$path[rr]
  sTitle <- NA
  lDisFile <- fnReadFile( sFileDis, NULL, "title", NULL )
  sTitle <- ifelse( is.null(lDisFile$title), dfDisExInfo$file[rr], lDisFile$title )
  dfDisExInfo$title[rr] <- sTitle
}
# sort
dfDisExInfo <- dfDisExInfo[order(dfDisExInfo$title),]
# add none selected
dfDisExInfo <- rbind( data.frame( file=NA, title="None Selected", path=NA, stringsAsFactors=F ), dfDisExInfo )
# validate
if(length(unique(dfDisExInfo$title)) != nrow(dfDisExInfo)) stop("The set of Dis file titles contains one or more duplicates")

fnStr(dfDisExInfo)
print(dfDisExInfo)
