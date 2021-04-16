library(plyr)
library(tableHTML)
source("CI-Functions-Bonferroni.R")

#set working directory to current directory

data_multiclass <- read.csv("data_multiclass.csv")
mydata<-data_multiclass
easydata <- mydata[mydata$difficulty == 'Facil',]
mediumdata <- mydata[mydata$difficulty == 'Media',]
harddata <- mydata[mydata$difficulty == 'Difícil',]
easydataone <- easydata[easydata$ntocompare == 1,]
easydatatwo <- easydata[easydata$ntocompare == 2,]
mediumdataone <- mediumdata[mediumdata$ntocompare == 1,]
mediumdatatwo <- mediumdata[mediumdata$ntocompare == 2,]
harddataone <- harddata[harddata$ntocompare == 1,]
harddatatwo <- harddata[harddata$ntocompare == 2,]



#############################
# analysis of all questions #
#############################

#function to generate all graphs and tables used in the paper and in the supplemental material webpage

saveTablesAndGraphs <- function(mydata, name) {
  myvars <- c("user", "technique", "time")
  elements <- mydata
  
  elements <- elements [ order(elements$user, elements$technique), ]
  
  elements <- elements [myvars]
  
  
  statstable_time <- ddply(elements,
                           c("user","technique"),
                           summarise,
                           time=mean(time)
  )
  elements <- statstable_time
  
  
  elements <- reshape(elements, timevar="technique", idvar=c("user"), direction="wide")
  colnames(elements) <- gsub("time.", "", colnames(elements))
  
  
  # drop columns with N/A
  elements <- na.omit(elements)
  
  
  #########
  # stats #
  #########
  
  data <- elements
  
  
  techniqueA <- bootstrapMeanCI(data$JX)
  techniqueB <- bootstrapMeanCI(data$ML)
  techniqueC <- bootstrapMeanCI(data$SW)
  techniqueD <- bootstrapMeanCI(data$OV)
  
  #
  # Calculating means per condition
  #
  analysisData <- c()
  analysisData$name <- c("JX","ML","SW","OV")
  analysisData$pointEstimate <- c(techniqueA[1], techniqueB[1], techniqueC[1], techniqueD[1])
  analysisData$ci.max <- c(techniqueA[3], techniqueB[3], techniqueC[3],techniqueD[3])
  analysisData$ci.min <- c(techniqueA[2], techniqueB[2], techniqueC[2],techniqueD[2])
  
  datatoprint <- data.frame(factor(analysisData$name),analysisData$pointEstimate, analysisData$ci.min, analysisData$ci.max)
  colnames(datatoprint) <- c("technique", "mean_time", "lowerBound_CI", "upperBound_CI ") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot
  
  pathPlots = paste("plots/")
  pathTables = paste("tables/")
  filenameTime = paste("time_", name, sep="")
  
  
  barChart(datatoprint, analysisData$name, nbTechs = 4, ymin = 0, ymax = 70, mycolor = c("#fc8d59"), xAxisLabel="", yAxisLabel="Mean time (seconds)")
  ggsave(paste0(pathPlots,"plot_",filenameTime,".png",seq=""),device = "png", width=5, height=3)
  colnames(datatoprint) <- c("tech", "time", "lb_CI", "ub_CI") 
  datatoprint[] <- lapply(datatoprint, format, digits = 2)
  write_tableHTML(tableHTML(datatoprint), file = paste(pathTables, filenameTime,".html", sep=""))
  
  
  #
  # Calculating differences of means 
  #
  
  # CIs with adapted alpha value for multiple comparisons not needed here
  diffJXML = bootstrapMeanCI_corr(data$JX - data$ML, 1)
  diffJXOV = bootstrapMeanCI_corr(data$JX - data$OV, 1)
  diffJXSW = bootstrapMeanCI_corr(data$JX - data$SW, 1)
  diffMLOV = bootstrapMeanCI_corr(data$ML - data$OV, 1)
  diffMLSW = bootstrapMeanCI_corr(data$ML - data$SW, 1)
  diffOVSW = bootstrapMeanCI_corr(data$OV - data$SW, 1)
  
  analysisData <- c()
  analysisData$name <- c("JX-ML","JX-OV","JX-SW", "ML-OV", "ML-SW", "OV-SW") # Symbol name has been changed in paper to Glyph
  analysisData$pointEstimate <- c(diffJXML[1], diffJXOV[1], diffJXSW[1],diffMLOV[1], diffMLSW[1],diffOVSW[1])
  analysisData$ci.max <- c(diffJXML[3], diffJXOV[3], diffJXSW[3],diffMLOV[3], diffMLSW[3],diffOVSW[3])
  analysisData$ci.min <- c(diffJXML[2], diffJXOV[2], diffJXSW[2],diffMLOV[2], diffMLSW[2],diffOVSW[2])

  datatoprint <- data.frame(factor(analysisData$name), analysisData$pointEstimate, analysisData$ci.min, analysisData$ci.max)
  colnames(datatoprint) <- c("technique", "mean_time", "lowerBound_CI", "upperBound_CI") #We use the name mean_time for the value of the mean even though it's not a time, it's just to parse the data for the plot
  
  
  filename = paste("time_diffs_", name, sep="")
  
  barChart(datatoprint, analysisData$name, nbTechs = 6, ymin = -35, ymax = 35, mycolor = "dodgerblue2", "", "")
  colnames(datatoprint) <- c("tech", "time", "lb_CI", "ub_CI") 
  datatoprint[] <- lapply(datatoprint, format, digits = 2)
  write_tableHTML(tableHTML(datatoprint), file = paste(pathTables, filename,".html", sep=""))
 
  ggsave(paste0(pathPlots,"plot_",filename,".png",seq=""),device = "png", width=5, height=3)
  
}


saveTablesAndGraphs(mydata, "all")
saveTablesAndGraphs(easydataone, "easy_one")
saveTablesAndGraphs(easydatatwo, "easy_two")
saveTablesAndGraphs(mediumdataone, "medium_one")
saveTablesAndGraphs(mediumdatatwo, "medium_two")
saveTablesAndGraphs(harddataone, "hard_one")
saveTablesAndGraphs(harddatatwo, "hard_two")
