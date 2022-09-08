##set input directory##
setwd("C:/Users/Heather Patterson/Downloads/R Data Rough/GEEDriveDownload")

##Install
install.packages("ggpubr")
install.packages("rsBayes")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("dplyr") 
install.packages("multcomp") 
install.packages("extrafont")
install.packages('nortest')
install.packages("FSA")
install.packages("ARTool")
install.packages("rcompanion")
install.packages("extrafont")




##Import Libararies##
library(rsBayes)
library(magrittr)
library(tidyverse)
library(dplyr)
library(multcomp)
library(extrafont)
library(nortest)
library(FSA)
library(ARTool)
library(rcompanion)
library(extrafont)


### Create Column and Assign.csv File Name to Column and Combine .csv's into one file by Site History Code###

C1F1XXXXXXXXXXT1XXXXXXR2XX <- list.files(path = "C:/Users/Heather Patterson/Downloads/R Data Rough/GEEDriveDownload/C1F1XXXXXXXXXXT1XXXXXXR2XX_3to510to12",
                                         pattern = "*.csv", full.names = TRUE) %>% 
  set_names() %>% 
  map_dfr(
    ~ read_csv(.x, col_types = cols(), col_names = FALSE),
    .id = "File_Path"
  )
lapply(read_csv) %>%  
  bind_rows
C1F1XXXXXXXXXXT1XXXXXXR2XX
write.csv(C1F1XXXXXXXXXXT1XXXXXXR2XX,'C:/Users/Heather Patterson/Downloads/R Data Rough/SiteCodes/C1F1XXXXXXXXXXT1XXXXXXR2XX.csv')


C1F1XXXXXXXXXXT1XXXXXXXXR3 <- list.files(path = "C:/Users/Heather Patterson/Downloads/R Data Rough/GEEDriveDownload/C1F1XXXXXXXXXXT1XXXXXXXXR3_3to510to12",    
                                         pattern = "*.csv", full.names = TRUE) %>% 
  set_names() %>% 
  map_dfr(
    ~ read_csv(.x, col_types = cols(), col_names = FALSE),
    .id = "File_Path"
  )
lapply(read_csv) %>%  
  bind_rows
C1F1XXXXXXXXXXT1XXXXXXXXR3
write.csv(C1F1XXXXXXXXXXT1XXXXXXXXR3,'C:/Users/Heather Patterson/Downloads/R Data Rough/SiteCodes/C1F1XXXXXXXXXXT1XXXXXXXXR3.csv')


C1F1XXXXXXXXXXXXXXXXXXR2XX <- list.files(path = "C:/Users/Heather Patterson/Downloads/R Data Rough/GEEDriveDownload/C1F1XXXXXXXXXXXXXXXXXXR2XX_10to12",    
                                         pattern = "*.csv", full.names = TRUE) %>% 
  set_names() %>% 
  map_dfr(
    ~ read_csv(.x, col_types = cols(), col_names = FALSE),
    .id = "File_Path"
  )
lapply(read_csv) %>%  
  bind_rows
C1F1XXXXXXXXXXXXXXXXXXR2XX
write.csv(C1F1XXXXXXXXXXXXXXXXXXR2XX,'C:/Users/Heather Patterson/Downloads/R Data Rough/SiteCodes/C1F1XXXXXXXXXXXXXXXXXXR2XX.csv')


C1F1XXXXXXXXXXXXXXXXXXXXR3 <- list.files(path = "C:/Users/Heather Patterson/Downloads/R Data Rough/GEEDriveDownload/C1F1XXXXXXXXXXXXXXXXXXXXR3_10to12",    
                                         pattern = "*.csv", full.names = TRUE) %>% 
  set_names() %>% 
  map_dfr(
    ~ read_csv(.x, col_types = cols(), col_names = FALSE),
    .id = "File_Path"
  )
lapply(read_csv) %>%  
  bind_rows
C1F1XXXXXXXXXXXXXXXXXXXXR3
write.csv(C1F1XXXXXXXXXXXXXXXXXXXXR3,'C:/Users/Heather Patterson/Downloads/R Data Rough/SiteCodes/C1F1XXXXXXXXXXXXXXXXXXXXR3.csv')

NoHerbicide <- bind_rows(C1F1XXXXXXXXXXXXXXXXXXXXR3, C1F1XXXXXXXXXXXXXXXXXXR2XX)
write.csv(NoHerbicide,'C:/Users/Heather Patterson/Downloads/R Data Rough/SiteCodes/NoHerbicide.csv')

Herbicide <- bind_rows(C1F1XXXXXXXXXXT1XXXXXXR2XX, C1F1XXXXXXXXXXT1XXXXXXXXR3)
write.csv(Herbicide,'C:/Users/Heather Patterson/Downloads/R Data Rough/SiteCodes/Herbicide.csv')

## set your input and output file paths
input_file <- 'C:/Users/Heather Patterson/Downloads/R Data Rough/SiteCodes/NoHerbicide.csv'
output_file <- 'C:/Users/Heather Patterson/Downloads/R Data Rough/SiteCodes/SpringInflection/NoHerbicide_SpringInflectionPosteriors.csv'

## read and clean input data
dataset <- read.csv(input_file, stringsAsFactors = FALSE, skip = 1, row.names = 1)
names(dataset) <- c('File_Path','SystemIndex', 'B','G','NDMI','NDVI','NIR','R','SWIR1','SWIR2','DATE','SATELLITE','GEOG')
dataset$NDVI <- as.numeric(dataset$NDVI)
dataset <- na.omit(dataset)

## function to extract the FID from a file path
getFID <- function(file_path) {
  substrings <- basename(file_path) %>%
    strsplit("_")
  fid <- strsplit(substrings[[1]][3], "\\.")[[1]][1]
  return(fid)
}

# apply the function to each file path in the data frame
# assign result to a new "FID" column
dataset$FID <- sapply(dataset$File_Path, getFID)

# get and format dates
dataset$DATE <- as.Date(dataset$DATE)
dataset$DOY <- format(dataset$DATE, "%j") %>% as.numeric()
dataset$YEAR <- format(dataset$DATE, "%Y") %>% as.numeric()

# rescale NDVI back to it's original [-1,1] scale
dataset$NDVI <- dataset$NDVI / 10000

# extract unique FIDs
unique_fids <- unique(dataset$FID)

## Look-Up Table (LUT) for plot info
LUT <- read.csv('C:/Users/Heather Patterson/Downloads/R Data Rough/HarvestInformation.csv')

### Model tuning parameters
# these don't change, so they can be set outside of the for-loop
init_params <- list(
  alpha.1 = 0.3,
  alpha.2 = 0.5,
  alpha.3 = 0.25,
  alpha.4 = 120,
  alpha.5 = 0.0001,
  alpha.6 = 0.25,
  alpha.7 = 200,
  sigma.sq = 0.01
)
tuning_params <- list(
  alpha.1 = 0.01,
  alpha.2 = 0.01,
  alpha.3 = 0.001,
  alpha.4 = 2,
  alpha.5 = 0.00005,
  alpha.6 = 0.001,
  alpha.7 = 1,
  sigma.sq = 0.1
)

priors <- list(
  alpha = list(alpha.5 = c(-1, 1)),
  sigma.sq.IG = c(2, 0.01)
)

# number of M-H iterations
N <- 100000

# burn-in length: The number of initial "guesses" to throw out before computing our median and credible intervals
burn_in = 5000



## Create a new data.frame with one row per FID
## Alternatively, you can use the main look-up data frame, but I don't have that on hand
## I am also creating blank columns for each parameter I want to extract inside the for-loop below
## PD = pre-disturbance; IN = intermediate; PT = post-treatment
## CILo = lower bounds of credible interval; MED = median posterior estimate; CIHi = upperbounds of credible interval
plot_FIDS <- data.frame(
  FID = unique_fids,
  CUT_YEAR = NA,
  SPRAYED = NA,
  SPRAY_YEAR = NA,
  PD_CILo = NA,
  PD_MED = NA,
  PD_CIHi = NA,
  IN_CILo = NA,
  IN_MED = NA,
  IN_CIHi = NA,
  PT_CILo = NA,
  PT_MED = NA,
  PT_CIHi = NA
)
## we'll fill these data in row by row (by FID)


# loop over each FID
for(i in c(1:length(unique_fids))) {
  cat("---\n")
  cat(unique_fids[i])
  cat("\n---\n\n")
  
  # time series for just this FID
  plot_data <- subset(dataset, FID == unique_fids[i])
  
  ## find row of LUT corresponding to this FID
  ## extract the cut year (distyear) and spray year (treatyear)
  ## assign distyear + 5 to treatyear if no treatment applied
  row <- which(LUT$TARGET_FID == unique_fids[i])
  distyear <- LUT$AR_YEAR_13[row]
  treatyear <- LUT$AR_YEAR_12[row]
  plot_FIDS$SPRAYED[i] <- TRUE
  if(treatyear == 0) {
    treatyear <- distyear + 5
    plot_FIDS$SPRAYED[i] <- FALSE
  }
  plot_FIDS$CUT_YEAR[i] <- distyear
  plot_FIDS$SPRAY_YEAR[i] <- treatyear
  
  
  ## PRECUT MODEL
  PC <- subset(plot_data, YEAR < distyear)
  PCModel <- pheno(NDVI ~ DOY, data = PC, gamma = c(-1, 1), family = "normal", starting = init_params, tuning = tuning_params, n.samples = N, n.report = 25000, priors = priors, fitted = TRUE, sub.sample = list("start" = 5000, "thin" = 2))
  PCalpha4 <- PCModel$p.theta.samples[,4] %>% as.vector()
  PCalpha4 <- PCalpha4[burn_in:length(PCalpha4)]
  PCalpha4_q <- quantile(PCalpha4, probs = c(0.05, 0.5, 0.95))
  plot_FIDS[i,'PD_CILo'] = PCalpha4_q[1]
  plot_FIDS[i,'PD_MED'] = PCalpha4_q[2]
  plot_FIDS[i,'PD_CIHi'] = PCalpha4_q[3]
  
  ## INTERMEDIATE MODEL
  IN <- subset(plot_data, YEAR >= distyear & YEAR < treatyear)
  INModel <- pheno(NDVI ~ DOY, data = IN, gamma = c(-1, 1), family = "normal", starting = init_params, tuning = tuning_params, n.samples = N, n.report = 25000, priors = priors, fitted = TRUE, sub.sample = list("start" = 5000, "thin" = 2))
  INalpha4 <- INModel$p.theta.samples[,4] %>% as.vector()
  INalpha4 <- INalpha4[burn_in:length(INalpha4)]
  INalpha4_q <- quantile(INalpha4, probs = c(0.05, 0.5, 0.95))
  plot_FIDS[i,'IN_CILo'] = INalpha4_q[1]
  plot_FIDS[i,'IN_MED'] = INalpha4_q[2]
  plot_FIDS[i,'IN_CIHi'] = INalpha4_q[3] 
  
  
  ## POST-TREATMENT MODEL
  PT <- subset(plot_data, YEAR >= treatyear)
  PTModel <- pheno(NDVI ~ DOY, data = PT, gamma = c(-1, 1), family = "normal", starting = init_params, tuning = tuning_params, n.samples = N, n.report = 25000, priors = priors, fitted = TRUE, sub.sample = list("start" = 5000, "thin" = 2))
  PTalpha4 <- PTModel$p.theta.samples[,4] %>% as.vector()
  PTalpha4 <- PTalpha4[burn_in:length(PTalpha4)]
  PTalpha4_q <- quantile(PTalpha4, probs = c(0.05, 0.5, 0.95))
  plot_FIDS[i,'PT_CILo'] = PTalpha4_q[1]
  plot_FIDS[i,'PT_MED'] = PTalpha4_q[2]
  plot_FIDS[i,'PT_CIHi'] = PTalpha4_q[3] 
}  


## write results to output file
write.csv(plot_FIDS, output_file)


## set your input and output file paths
input_file1 <- 'C:/Users/Heather Patterson/Downloads/R Data Rough/SiteCodes/Herbicide.csv'
output_file1 <- 'C:/Users/Heather Patterson/Downloads/R Data Rough/SiteCodes/SpringInflection/Herbicide_SpringInflectionPosteriors.csv'

## read and clean input data
dataset1 <- read.csv(input_file1, stringsAsFactors = FALSE, skip = 1, row.names = 1)
names(dataset1) <- c('File_Path','SystemIndex', 'B','G','NDMI','NDVI','NIR','R','SWIR1','SWIR2','DATE','SATELLITE','GEOG')
dataset1$NDVI <- as.numeric(dataset1$NDVI)
dataset1 <- na.omit(dataset1)

## function to extract the FID from a file path
getFID <- function(file_path) {
  substrings <- basename(file_path) %>%
    strsplit("_")
  fid <- strsplit(substrings[[1]][3], "\\.")[[1]][1]
  return(fid)
}

# apply the function to each file path in the data frame
# assign result to a new "FID" column
dataset1$FID <- sapply(dataset1$File_Path, getFID)

# get and format dates
dataset1$DATE <- as.Date(dataset1$DATE)
dataset1$DOY <- format(dataset1$DATE, "%j") %>% as.numeric()
dataset1$YEAR <- format(dataset1$DATE, "%Y") %>% as.numeric()

# rescale NDVI back to it's original [-1,1] scale
dataset1$NDVI <- dataset1$NDVI / 10000

# extract unique FIDs
unique_fids <- unique(dataset1$FID)

## Look-Up Table (LUT) for plot info
LUT <- read.csv('C:/Users/Heather Patterson/Downloads/R Data Rough/HarvestInformation.csv')

### Model tuning parameters
# these don't change, so they can be set outside of the for-loop
init_params <- list(
  alpha.1 = 0.3,
  alpha.2 = 0.5,
  alpha.3 = 0.25,
  alpha.4 = 120,
  alpha.5 = 0.0001,
  alpha.6 = 0.25,
  alpha.7 = 200,
  sigma.sq = 0.01
)
tuning_params <- list(
  alpha.1 = 0.01,
  alpha.2 = 0.01,
  alpha.3 = 0.001,
  alpha.4 = 2,
  alpha.5 = 0.00005,
  alpha.6 = 0.001,
  alpha.7 = 1,
  sigma.sq = 0.1
)

priors <- list(
  alpha = list(alpha.5 = c(-1, 1)),
  sigma.sq.IG = c(2, 0.01)
)

# number of M-H iterations
N <- 100000

# burn-in length: The number of initial "guesses" to throw out before computing our median and credible intervals
burn_in = 5000



## Create a new data.frame with one row per FID
## Alternatively, you can use the main look-up data frame, but I don't have that on hand
## I am also creating blank columns for each parameter I want to extract inside the for-loop below
## PD = pre-disturbance; IN = intermediate; PT = post-treatment
## CILo = lower bounds of credible interval; MED = median posterior estimate; CIHi = upperbounds of credible interval
plot_FIDS <- data.frame(
  FID = unique_fids,
  CUT_YEAR = NA,
  SPRAYED = NA,
  SPRAY_YEAR = NA,
  PD_CILo = NA,
  PD_MED = NA,
  PD_CIHi = NA,
  IN_CILo = NA,
  IN_MED = NA,
  IN_CIHi = NA,
  PT_CILo = NA,
  PT_MED = NA,
  PT_CIHi = NA
)
## we'll fill these data in row by row (by FID)


# loop over each FID
for(i in c(1:length(unique_fids))) {
  cat("---\n")
  cat(unique_fids[i])
  cat("\n---\n\n")
  
  # time series for just this FID
  plot_data <- subset(dataset1, FID == unique_fids[i])
  
  ## find row of LUT corresponding to this FID
  ## extract the cut year (distyear) and spray year (treatyear)
  ## assign distyear + 5 to treatyear if no treatment applied
  row <- which(LUT$TARGET_FID == unique_fids[i])
  distyear <- LUT$AR_YEAR_13[row]
  treatyear <- LUT$AR_YEAR_12[row]
  plot_FIDS$SPRAYED[i] <- TRUE
  if(treatyear == 0) {
    treatyear <- distyear + 5
    plot_FIDS$SPRAYED[i] <- FALSE
  }
  plot_FIDS$CUT_YEAR[i] <- distyear
  plot_FIDS$SPRAY_YEAR[i] <- treatyear
  
  
  ## PRECUT MODEL
  PC <- subset(plot_data, YEAR < distyear)
  PCModel <- pheno(NDVI ~ DOY, data = PC, gamma = c(-1, 1), family = "normal", starting = init_params, tuning = tuning_params, n.samples = N, n.report = 25000, priors = priors, fitted = TRUE, sub.sample = list("start" = 5000, "thin" = 2))
  PCalpha4 <- PCModel$p.theta.samples[,4] %>% as.vector()
  PCalpha4 <- PCalpha4[burn_in:length(PCalpha4)]
  PCalpha4_q <- quantile(PCalpha4, probs = c(0.05, 0.5, 0.95))
  plot_FIDS[i,'PD_CILo'] = PCalpha4_q[1]
  plot_FIDS[i,'PD_MED'] = PCalpha4_q[2]
  plot_FIDS[i,'PD_CIHi'] = PCalpha4_q[3]
  
  ## INTERMEDIATE MODEL
  IN <- subset(plot_data, YEAR >= distyear & YEAR < treatyear)
  INModel <- pheno(NDVI ~ DOY, data = IN, gamma = c(-1, 1), family = "normal", starting = init_params, tuning = tuning_params, n.samples = N, n.report = 25000, priors = priors, fitted = TRUE, sub.sample = list("start" = 5000, "thin" = 2))
  INalpha4 <- INModel$p.theta.samples[,4] %>% as.vector()
  INalpha4 <- INalpha4[burn_in:length(INalpha4)]
  INalpha4_q <- quantile(INalpha4, probs = c(0.05, 0.5, 0.95))
  plot_FIDS[i,'IN_CILo'] = INalpha4_q[1]
  plot_FIDS[i,'IN_MED'] = INalpha4_q[2]
  plot_FIDS[i,'IN_CIHi'] = INalpha4_q[3] 
  
  
  ## POST-TREATMENT MODEL
  PT <- subset(plot_data, YEAR >= treatyear)
  PTModel <- pheno(NDVI ~ DOY, data = PT, gamma = c(-1, 1), family = "normal", starting = init_params, tuning = tuning_params, n.samples = N, n.report = 25000, priors = priors, fitted = TRUE, sub.sample = list("start" = 5000, "thin" = 2))
  PTalpha4 <- PTModel$p.theta.samples[,4] %>% as.vector()
  PTalpha4 <- PTalpha4[burn_in:length(PTalpha4)]
  PTalpha4_q <- quantile(PTalpha4, probs = c(0.05, 0.5, 0.95))
  plot_FIDS[i,'PT_CILo'] = PTalpha4_q[1]
  plot_FIDS[i,'PT_MED'] = PTalpha4_q[2]
  plot_FIDS[i,'PT_CIHi'] = PTalpha4_q[3] 
}  


## write results to output file
write.csv(plot_FIDS, output_file1)









#########################################################STATISTICAL ANALYSIS########################################################################################
###ANOVA
#Import File and re-format
ANOVADATA <- list.files(path = "C:/Users/Heather Patterson/Downloads/R Data Rough/SiteCodes/SpringInflection",
                        pattern = "*.csv", full.names = TRUE) %>% 
  set_names() %>% 
  map_dfr(
    ~ read_csv(.x, col_types = cols(), col_names = FALSE),
    .id = "FID"
  )
lapply(read_csv) %>%  
  bind_rows
write.csv(ANOVADATA, 'C:/Users/Heather Patterson/Downloads/R Data Rough/ANOVADATA.csv')
ANOVADATA <- read.csv('C:/Users/Heather Patterson/Downloads/R Data Rough/ANOVADATA.csv', stringsAsFactors = FALSE, skip = 1, row.names = 1)
names(ANOVADATA) <- c('File_Path','NUM', 'FID', 'CUT_YEAR'	,'SPRAYED',	'SPRAY_YEAR',	'PD_CILo',	'PD_MED',	'PD_CIHi',	'IN_CILo',	'IN_MED',	'IN_CIHi',	'PT_CILo',	'PT_MED',	'PT_CIHi')
ANOVADATACLEAN <- na.omit(ANOVADATA)
ANOVADATACLEAN

##reclass
ANOVADATACLEAN$SPRAYED <- as.factor(ANOVADATACLEAN$SPRAYED)
ANOVADATACLEAN$PD_MED <- as.factor(ANOVADATACLEAN$PD_MED)
ANOVADATACLEAN$IN_MED <- as.factor(ANOVADATACLEAN$IN_MED)
ANOVADATACLEAN$PT_MED <- as.factor(ANOVADATACLEAN$PT_MED)
str(ANOVADATACLEAN)

##Create data frames for each period
ANOVADATACLEANMEDPD <- data.frame (TARGET_FID = c(ANOVADATACLEAN$FID), SPRAYED = c(ANOVADATACLEAN$SPRAYED), TIME = "PD", MED = c(ANOVADATACLEAN$PD_MED), Hi = c(ANOVADATACLEAN$PD_CIHi), Lo = c(ANOVADATACLEAN$PD_CILo))
ANOVADATACLEANMEDIN <- data.frame (TARGET_FID = c(ANOVADATACLEAN$FID), SPRAYED = c(ANOVADATACLEAN$SPRAYED), TIME = "IN", MED = c(ANOVADATACLEAN$IN_MED), Hi = c(ANOVADATACLEAN$IN_CIHi), Lo = c(ANOVADATACLEAN$IN_CILo))
ANOVADATACLEANMEDPT <- data.frame (TARGET_FID = c(ANOVADATACLEAN$FID), SPRAYED = c(ANOVADATACLEAN$SPRAYED), TIME = "PT", MED = c(ANOVADATACLEAN$PT_MED), Hi = c(ANOVADATACLEAN$PT_CIHi), Lo = c(ANOVADATACLEAN$PT_CILo))

##Merge Dataframes
ANOVAReshapedMED <- bind_rows(ANOVADATACLEANMEDIN, ANOVADATACLEANMEDPD, ANOVADATACLEANMEDPT)


ANOVAReshapedMED$SPRAYED <- as.factor(ANOVAReshapedMED$SPRAYED)
ANOVAReshapedMED$TIME <- factor(ANOVAReshapedMED$TIME, levels=c("PD","IN","PT"))
ANOVAReshapedMED$MED <- as.character(ANOVAReshapedMED$MED)
ANOVAReshapedMED$MED <- as.numeric(ANOVAReshapedMED$MED)
str(ANOVAReshapedMED)
write.csv(ANOVAReshapedMED, 'C:/Users/Heather Patterson/Downloads/R Data Rough/ANOVAReshapedMED.csv')

table(ANOVAReshapedMED$TIME, ANOVAReshapedMED$SPRAYED)



###EcoRegion
## Add Look-Up Table (LUT) for FMU and Ecoregion info

MasterLUT <-read.csv('C:/Users/Heather Patterson/Downloads/R Data Rough/EcoDistrict5.csv')
LUT <- read.csv('C:/Users/Heather Patterson/Downloads/R Data Rough/HarvestInformation.csv')
ANOVAReshapedMEDECO<-merge(x=ANOVAReshapedMED,y=MasterLUT,by="TARGET_FID",all.x=TRUE)
ANOVAReshapedMEDECO2<-merge(x=ANOVAReshapedMEDECO,y=LUT,by="TARGET_FID",all.x=TRUE)
ANOVAReshapedMEDECOREGION <- subset(ANOVAReshapedMEDECO2, select = c("TARGET_FID", "FMU_NAME", "SPRAYED","TIME","MED","Lo","Hi","ECOREGION_", "ECODISTRIC"))

# Reclass
library(tidyverse)
ANOVAReshapedMEDECOREGION$SPRAYED <- as.factor(ANOVAReshapedMEDECOREGION$SPRAYED)
ANOVAReshapedMEDECOREGION$TIME <- factor(ANOVAReshapedMEDECOREGION$TIME, levels=c("PD","IN","PT"))
ANOVAReshapedMEDECOREGION$ECOREGION_ <- as.factor(ANOVAReshapedMEDECOREGION$ECOREGION_)
ANOVAReshapedMEDECOREGION$MED <- as.character(ANOVAReshapedMEDECOREGION$MED)
ANOVAReshapedMEDECOREGION$MED <- as.numeric(ANOVAReshapedMEDECOREGION$MED)

str(ANOVAReshapedMEDECOREGION)

## Visualize Data 
# Box plot with multiple groups
library("ggpubr")
ggboxplot(ANOVAReshapedMED, x = "TIME", y = "MED", color = "SPRAYED",
          palette = c("#00AFBB", "#E7B800"))

###HISTOGRAMS
attach(mtcars)
par(mfcol=c(2,3))
PDNS <- ANOVAReshapedMEDECOREGION[ which(ANOVAReshapedMEDECOREGION$TIME=='PD'
                                         & ANOVAReshapedMEDECOREGION$SPRAYED == "FALSE"), ]
hist(PDNS$MED, col = 'blue', main = 'Distribution of MED for PD Not Sprayed',xlab = 'MED')

PDSP <- ANOVAReshapedMEDECOREGION[ which(ANOVAReshapedMEDECOREGION$TIME=='PD'
                                         & ANOVAReshapedMEDECOREGION$SPRAYED == "TRUE"),]
hist(PDSP$MED, col = 'blue', main = 'Distribution of MED for PD Sprayed',xlab = 'MED')

INNS <- ANOVAReshapedMEDECOREGION[ which(ANOVAReshapedMEDECOREGION$TIME=='IN'
                                         & ANOVAReshapedMEDECOREGION$SPRAYED == "FALSE"), ]
hist(INNS$MED, col = 'green', main = 'Distribution of MED for IN Not Sprayed',xlab = 'MED')

INSP <- ANOVAReshapedMEDECOREGION[ which(ANOVAReshapedMEDECOREGION$TIME=='IN'
                                         & ANOVAReshapedMEDECOREGION$SPRAYED == "TRUE"),]
hist(INSP$MED, col = 'green', main = 'Distribution of MED for IN Sprayed',xlab = 'MED')

PTNS <- ANOVAReshapedMEDECOREGION[ which(ANOVAReshapedMEDECOREGION$TIME=='PT'
                                         & ANOVAReshapedMEDECOREGION$SPRAYED == "FALSE"), ]
hist(PTNS$MED, col = 'purple', main = 'Distribution of MED for PT Not Sprayed',xlab = 'MED')

PTSP <- ANOVAReshapedMEDECOREGION[ which(ANOVAReshapedMEDECOREGION$TIME=='PT'
                                         & ANOVAReshapedMEDECOREGION$SPRAYED == "TRUE"),]
hist(PTSP$MED, col = 'purple', main = 'Distribution of MED for PT Sprayed',xlab = 'MED')

mean(PDSP$MED)
mean(PDNS$MED)
mean(INSP$MED)
mean(INNS$MED)
mean(PTSP$MED)
mean(PTNS$MED)

str(ANOVAReshapedMEDECOREGION)  
table(ANOVAReshapedMEDECOREGION$TIME, ANOVAReshapedMEDECOREGION$SPRAYED)

###DATACLEANING with Credible Intervals
### Subset 172>MED<80
ANOVAReshapedMEDECOREGIONEQ80172 <- ANOVAReshapedMEDECOREGION %>%   filter(MED >80 &  MED < 172)

### Subset Hi-Lo<92
ANOVAReshapedMEDECOREGIONEQ80172$Hi <- as.numeric(ANOVAReshapedMEDECOREGIONEQ80172$Hi)
ANOVAReshapedMEDECOREGIONEQ80172$Lo <- as.numeric(ANOVAReshapedMEDECOREGIONEQ80172$Lo)
ANOVAReshapedMEDECOREGIONEQ80172$CredInt <- ANOVAReshapedMEDECOREGIONEQ80172$Hi - ANOVAReshapedMEDECOREGIONEQ80172$Lo
ANOVAReshapedMEDECOREGIONEQ8017292 <- ANOVAReshapedMEDECOREGIONEQ80172 %>%   filter(CredInt < 92)

table(ANOVAReshapedMEDECOREGIONEQ8017292$TIME, ANOVAReshapedMEDECOREGIONEQ8017292$SPRAYED)

##Visualize Data
# Box plot with multiple groups
library("ggpubr")
ggboxplot(ANOVAReshapedMEDECOREGIONEQ8017292, x = "TIME", y = "MED", color = "SPRAYED",
          palette = c("#00AFBB", "#E7B800"))
###Histograms
attach(mtcars)
par(mfcol=c(2,3))
par(family = "times", font = 2, font.lab = 2, font.axis = 2)
PDNS <- ANOVAReshapedMEDECOREGIONEQ8017292[ which(ANOVAReshapedMEDECOREGIONEQ8017292$TIME=='PD'
                                                  & ANOVAReshapedMEDECOREGIONEQ8017292$SPRAYED == "FALSE"), ]
hist(PDNS$MED, col = 'black', main = 'Pre-Disturbance, Not Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

PDSP <- ANOVAReshapedMEDECOREGIONEQ8017292[ which(ANOVAReshapedMEDECOREGIONEQ8017292$TIME=='PD'
                                                  & ANOVAReshapedMEDECOREGIONEQ8017292$SPRAYED == "TRUE"),]
hist(PDSP$MED, col = 'black', main = 'Pre-Disturbance, Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

INNS <- ANOVAReshapedMEDECOREGIONEQ8017292[ which(ANOVAReshapedMEDECOREGIONEQ8017292$TIME=='IN'
                                                  & ANOVAReshapedMEDECOREGIONEQ8017292$SPRAYED == "FALSE"), ]
hist(INNS$MED, col = 'dark grey', main = 'Intermediate, Not Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

INSP <- ANOVAReshapedMEDECOREGIONEQ8017292[ which(ANOVAReshapedMEDECOREGIONEQ8017292$TIME=='IN'
                                                  & ANOVAReshapedMEDECOREGIONEQ8017292$SPRAYED == "TRUE"),]
hist(INSP$MED, col = 'dark grey', main = 'Intermediate, Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

PTNS <- ANOVAReshapedMEDECOREGIONEQ8017292[ which(ANOVAReshapedMEDECOREGIONEQ8017292$TIME=='PT'
                                                  & ANOVAReshapedMEDECOREGIONEQ8017292$SPRAYED == "FALSE"), ]
hist(PTNS$MED, col = 'light grey', main = 'Post-Treatment/Recovery, Not Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

PTSP <- ANOVAReshapedMEDECOREGIONEQ8017292[ which(ANOVAReshapedMEDECOREGIONEQ8017292$TIME=='PT'
                                                  & ANOVAReshapedMEDECOREGIONEQ8017292$SPRAYED == "TRUE"),]
hist(PTSP$MED, col = 'light grey', main = 'Post-Treatment/Recovery, Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')
mtext("Distribution of Spring Inflection Point Estimates for All Ontario Data",                   # Add main title
      side = 3,
      line = - 1,
      outer = TRUE)

#Normality
shapiro.test(PDSP$MED)
shapiro.test(PDNS$MED)
shapiro.test(INSP$MED)
shapiro.test(INNS$MED)
shapiro.test(PTSP$MED)
shapiro.test(PTNS$MED)
##All not normal

mean(PDSP$MED)
mean(PDNS$MED)
mean(INSP$MED)
mean(INNS$MED)
mean(PTSP$MED)
mean(PTNS$MED)

var.test(PDSP$MED, PDNS$MED)
var.test(INSP$MED, INNS$MED)
var.test(PTSP$MED, PTNS$MED)

### Variences are only equal for IN and PT Period

#Reclass
str(ANOVAReshapedMEDECOREGIONEQ8017292)
ANOVAReshapedMEDECOREGIONEQ8017292$Hi <- as.numeric(ANOVAReshapedMEDECOREGIONEQ8017292$Hi)
ANOVAReshapedMEDECOREGIONEQ8017292$Lo <- as.numeric(ANOVAReshapedMEDECOREGIONEQ8017292$Lo)

ANOVAReshapedMEDECOREGIONEQ8017292$CredInt <- ANOVAReshapedMEDECOREGIONEQ8017292$Hi - ANOVAReshapedMEDECOREGIONEQ8017292$Lo

##Subset different time periods
PD <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$TIME=='PD')
IN <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$TIME=='IN')
PT <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$TIME=='PT')


# Box plot with multiple groups
library("ggpubr")
Initial <- ggboxplot(ANOVAReshapedMEDECOREGIONEQ8017292, x = "TIME", y = "MED", color = "SPRAYED",
                     palette = c("black", "dark grey"))
Initial + theme(text = element_text(family = "serif")) + ggtitle("Median Spring Inflection Point Estimates by Period of Time (All of Ontario)") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Time Period", y="Median Spring Inflection Point Estimates (MED)", color='Herbicide-Treated') + theme(legend.position="bottom")                                                                                                                                                                                                                                 


ad.test(ANOVAReshapedMEDECOREGIONEQ8017292$MED)
##Not normal

###Need non-parametric version of two-way repeated measures ANOVA
##Aligned Ranks Transformation ANOVA https://rcompanion.org/handbook/F_16.html
str(ANOVAReshapedMEDECOREGIONEQ8017292)
table(ANOVAReshapedMEDECOREGIONEQ8017292$SPRAYED,ANOVAReshapedMEDECOREGIONEQ8017292$TIME)
ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME<- as.factor(ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME)
modelALL = art(MED ~ SPRAYED + TIME + SPRAYED:TIME, data = ANOVAReshapedMEDECOREGIONEQ8017292)
modelALL
anova(modelALL)
## All significant
##Tukey post-hoc
marginalALL = art.con(modelALL, "SPRAYED:TIME", adjust="tukey")
marginalALL
write.table(marginalALL, file="C:/Users/Heather Patterson/Downloads/MarginalALL.csv", sep=",", col.names=NA)


###Pre-Disturbance Differences indicated by significantly different PD values
###FMU Differences?

table (PDALL$FMU_NAME, PDALL$SPRAYED)
PDALL6 <- subset(PDALL, PDALL$FMU_NAME!='Algoma Forest'& PDALL$FMU_NAME!='Algonquin Park Forest'& PDALL$FMU_NAME!='Bancroft-Minden Forest'& PDALL$FMU_NAME!='Black Spruce Forest'& PDALL$FMU_NAME!='Dryden Forest'& PDALL$FMU_NAME!='Hearst Forest'& PDALL$FMU_NAME!='Kenogami Forest'& PDALL$FMU_NAME!='Kenora Forest'& PDALL$FMU_NAME!='Lac Seul Forest'& PDALL$FMU_NAME!='Lakehead Forest'& PDALL$FMU_NAME!='Nipissing Forest'& PDALL$FMU_NAME!='Red Lake Forest'& PDALL$FMU_NAME!='Romeo Malette Forest'& PDALL$FMU_NAME!='Sudbury Forest'& PDALL$FMU_NAME!='Temagami Forest'& PDALL$FMU_NAME!='Trout Lake Forest'& PDALL$FMU_NAME!='Whiskey Jack Forest'& PDALL$FMU_NAME!='White River Forest')
PDALL6$FMU_NAME <- as.factor(PDALL$FMU_NAME)


Initial <- ggboxplot(PDALL6, x = "FMU_NAME", y = "MED", color = "SPRAYED",
                     palette = c("black", "dark grey")) + coord_flip()
Initial + theme(text = element_text(family = "serif")) + ggtitle("Median Spring Inflection Point Estimates by FMU") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="FMU", y="Median Spring Inflection Point Estimates (Day of Year)", color='Herbicide-Treated') + theme(legend.position="bottom")                                                                                                                                                                                                                                 

PDALL6$FMU_NAME<- as.factor(PDALL6$FMU_NAME)
modelALL3 = art(MED ~ SPRAYED + FMU_NAME + SPRAYED:FMU_NAME, data = PDALL6)
modelALL3
anova(modelALL3)


## All significant
##Tukey post-hoc
marginalALL3 = art.con(modelALL3, "SPRAYED:FMU_NAME", adjust="tukey")
marginalALL3
write.table(marginalALL3, file="C:/Users/Heather Patterson/Downloads/MarginalALLFMU.csv", sep=",", col.names=NA)

##English River, Missinaibi, Dog River-Matawin and Lake Nipigon show significant difference between T and F in PD
###Re-reun without FMUs with significant PD differences
##Aligned Ranks Transformation ANOVA https://rcompanion.org/handbook/F_16.html

ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME<- as.factor(ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME)
str(ANOVAReshapedMEDECOREGIONEQ8017292)

RemoveN10<- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Algoma Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Algonquin Park Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Bancroft-Minden Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Black Spruce Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Dryden Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Hearst Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Kenogami Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Kenora Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Lac Seul Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Lakehead Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Nipissing Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Red Lake Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Romeo Malette Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Sudbury Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Temagami Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Trout Lake Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='Whiskey Jack Forest'& ANOVAReshapedMEDECOREGIONEQ8017292$FMU_NAME!='White River Forest')
ANOVAReshapedMEDECOREGIONEQ8017292DIFFPD <- subset(RemoveN10, RemoveN10$FMU_NAME!='Dog River-Matawin Forest' & RemoveN10$FMU_NAME!='English River Forest' & RemoveN10$FMU_NAME!='Missinaibi Forest' &RemoveN10$FMU_NAME!='Lake Nipigon Forest')

str(ANOVAReshapedMEDECOREGIONEQ8017292DIFFPD)
table(ANOVAReshapedMEDECOREGIONEQ8017292DIFFPD$FMU_NAME,ANOVAReshapedMEDECOREGIONEQ8017292DIFFPD$TIME)
library("ggpubr")
Initial <- ggboxplot(ANOVAReshapedMEDECOREGIONEQ8017292DIFFPD, x = "TIME", y = "MED", color = "SPRAYED",
                     palette = c("black", "dark grey"))
Initial + theme(text = element_text(family = "serif")) + ggtitle("Median Spring Inflection Point Estimates by Period of Time with Potential Problem FMUs Removed") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Time Period", y="Median Spring Inflection Point Estimates (Day of Year)", color='Herbicide-Treated') + theme(legend.position="bottom")                                                                                                                                                                                                                                 

table(ANOVAReshapedMEDECOREGIONEQ8017292DIFFPD$SPRAYED,ANOVAReshapedMEDECOREGIONEQ8017292DIFFPD$TIME)

modelALL12 = art(MED ~ SPRAYED + TIME + SPRAYED:TIME, data = ANOVAReshapedMEDECOREGIONEQ8017292DIFFPD)
modelALL12
anova(modelALL12)
## All significant
##Tukey post-hoc
marginalALL12 = art.con(modelALL12, "SPRAYED:TIME", adjust="tukey")
marginalALL12
write.table(marginalALL12, file="C:/Users/Heather Patterson/Downloads/MarginalALLProblemFMUsRemoved.csv", sep=",", col.names=NA)
##PD still significantly different p=0.000213148


###Subset by Ecoregion
names(ANOVAReshapedMEDECOREGIONEQ8017292)[names(ANOVAReshapedMEDECOREGIONEQ8017292) == "ECOREGION_"] <- "ECOREGION"
levels(factor(ANOVAReshapedMEDECOREGIONEQ8017292$ECOREGION)) 
PDALL2 <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$TIME=='PD')
PDALL3 <- subset(PDALL2, PDALL2$ECOREGION!='Agassiz Clay Plain' & PDALL2$ECOREGION!='Big Trout Lake')
str(PDALL3)
MEDECOREGIONPDALL <- qplot(ECOREGION, MED, data = PDALL3, geom = "boxplot", main = "MED by Ecoregion, All Sites PD") + coord_flip()
loadfonts(device = "win")
MEDECOREGIONPDALL + theme(text = element_text(family = "serif")) + ggtitle("MED by Ecoregion, All Sites PD") + theme(plot.title = element_text(hjust = 0.5)) + aes(x=reorder(ECOREGION,-MED, na.rm = TRUE, y=Ecoregion)) + labs(x="EcoRegion", y="Median Spring Inflection Point Estimates (MED)") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))



Initial <- ggboxplot(PDALL3, x = "ECOREGION", y = "MED", color = "SPRAYED",
                     palette = c("black", "dark grey")) + coord_flip()
Initial + theme(text = element_text(family = "serif")) + ggtitle("Median Spring Inflection Point Estimates by EcoRegion") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="EcoRegion", y="Median Spring Inflection Point Estimates (Day of Year)", color='Herbicide-Treated') + theme(legend.position="bottom")                                                                                                                                                                                                                                 

table(PDALL3$ECOREGION, PDALL3$SPRAYED)


##Aligned Ranks Transformation ANOVA https://rcompanion.org/handbook/F_16.html
str(PDALL3)
PDALL3$ECODISTRICT <- as.factor(PDALL3$ECOREGION)
table(PDALL3$SPRAYED,PDALL3$ECOREGION)
str(PDALL3)


modelALL2 = art(MED ~ SPRAYED + ECOREGION + SPRAYED:ECOREGION, data = PDALL3)
modelALL2
anova(modelALL2)


## All significant
##Tukey post-hoc
marginalALL2 = art.con(modelALL2, "SPRAYED:ECOREGION", adjust="tukey")
marginalALL2
write.table(marginalALL2, file="C:/Users/Heather Patterson/Downloads/MarginalALL2.csv", sep=",", col.names=NA)







kruskal.test(MED ~ ECOREGION, data = PDALL)
###As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.

##Multiple pairwise-comparison between groups Dunn

DUNNTESTRESULTSPDALL<- data.frame

DUNNTESTRESULTSPDALL<-dunnTest(MED ~ ECOREGION, data = PDALL, method = "holm")
write.table(DUNNTESTRESULTSPDALL[["res"]], file="C:/Users/Heather Patterson/Downloads/DUNNTESTRESULTSPDALLECO.csv", sep=",", col.names=NA)
library(plyr)
FACTORMEANSECOREG<-ddply(PDALL, .(ECOREGION), summarize, mean=mean(MED))
HerbNoHerbCount <- table(PDALL$ECOREGION,PDALL$SPRAYED)
HerbNoHerbCountDF <- as.data.frame(HerbNoHerbCount)

ECOAgassizClayPlain <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$ECOREGION=='Agassiz Clay Plain')
ECOBigTroutLake <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$ECOREGION=='Big Trout Lake')
ECOGeorgianBay <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$ECOREGION=='Georgian Bay')
ECOLakeAbitibi <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$ECOREGION=='Lake Abitibi')
ECOLakeNipigon <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$ECOREGION=='Lake Nipigon')
ECOLakeSt.Joseph <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$ECOREGION=='Lake St. Joseph')
ECOLakeTemagami <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$ECOREGION=='Lake Temagami')
ECOLakeWabigoon <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$ECOREGION=='Lake Wabigoon')
ECOPigeonRiver <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$ECOREGION=='Pigeon River')

LowMED <- rbind(ECOLakeNipigon, ECOLakeSt.Joseph, ECOLakeWabigoon, ECOPigeonRiver)
table(LowMED$SPRAYED, LowMED$TIME)

HiMED <- rbind(ECOGeorgianBay, ECOLakeAbitibi, ECOLakeTemagami)
table(HiMED$SPRAYED, HiMED$TIME)

NoDiffMED <- rbind(ECOAgassizClayPlain, ECOBigTroutLake)
table(NoDiffMED$SPRAYED, NoDiffMED$TIME)

###Eliminate ECOAgassizClayPlain and ECOBigTroutLake due to low sample size. 
HiMEDAmalg <- rbind(ECOGeorgianBay, ECOLakeAbitibi, ECOLakeTemagami)
table(HiMED$SPRAYED, HiMED$TIME)

LowMEDAmalg <- rbind(ECOLakeNipigon, ECOLakeSt.Joseph, ECOLakeWabigoon, ECOPigeonRiver)
table(LowMED$SPRAYED, LowMED$TIME)

### Test for significant difference
HiMEDAmalg$GROUP <- c("HIGH")
LowMEDAmalg$GROUP <- c("LOW")
MergeHighLowWilcox <- rbind(HiMEDAmalg, LowMEDAmalg)
wilcox.test(MED ~ GROUP, data=MergeHighLowWilcox) 

##Low and Hi are still significantly different  after amalgamation p-value = 9.641e-10


###Anova model by region
##HI

library("ggpubr")
InitialHi <- ggboxplot(HiMEDAmalg, x = "TIME", y = "MED", color = "SPRAYED", palette = c("black", "dark grey"))

InitialHi + theme(text = element_text(family = "serif")) + ggtitle("Median Spring Inflection Point Estimates by Period of Time (Late Spring Inflection Point Group)") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Time Period", y="Median Spring Inflection Point Estimates (Day of Year)", color='Herbicide-Treated') + theme(legend.position="bottom")

ad.test(HiMEDAmalg$MED)
##Not normal

modelHi = art(MED ~ SPRAYED + TIME + SPRAYED:TIME, data = HiMEDAmalg)
modelHi
anova(modelHi)
## All significant
##Tukey post-hoc
marginalHi = art.con(modelHi, "SPRAYED:TIME", adjust="tukey")
marginalHi
write.table(marginalHi, file="C:/Users/Heather Patterson/Downloads/MarginalHigh.csv", sep=",", col.names=NA)

##PD is significantly different between SPRAYED T and F 

Initial <- ggboxplot(PDALL3, x = "ECOREGION", y = "MED", color = "SPRAYED",
                     palette = c("black", "dark grey")) + coord_flip()
Initial + theme(text = element_text(family = "serif")) + ggtitle("Median Spring Inflection Point Estimates by EcoRegion") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="EcoRegion", y="Median Spring Inflection Point Estimates (Day of Year)", color='Herbicide-Treated') + theme(legend.position="bottom")                                                                                                                                                                                                                                 




###Histograms Hi
attach(mtcars)
par(mfcol=c(2,3))
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 2, font.lab = 2, font.axis = 2)
PDNSHi <- HiMEDAmalg[ which(HiMEDAmalg$TIME=='PD'
                            & HiMEDAmalg$SPRAYED == "FALSE"), ]
hist(PDNSHi$MED, col = 'black', main = 'Pre-Disturbance, Not Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

PDSPHi <- HiMEDAmalg[ which(HiMEDAmalg$TIME=='PD'
                            & HiMEDAmalg$SPRAYED == "TRUE"),]
hist(PDSPHi$MED, col = 'black', main = 'Pre-Disturbance, Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

INNSHi <- HiMEDAmalg[ which(HiMEDAmalg$TIME=='IN'
                            & HiMEDAmalg$SPRAYED == "FALSE"), ]
hist(INNSHi$MED, col = 'dark grey', main = 'Intermediate, Not Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

INSPHi <- HiMEDAmalg[ which(HiMEDAmalg$TIME=='IN'
                            & HiMEDAmalg$SPRAYED == "TRUE"),]
hist(INSPHi$MED, col = 'dark grey', main = 'Intermediate, Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

PTNSHi <- HiMEDAmalg[ which(HiMEDAmalg$TIME=='PT'
                            & HiMEDAmalg$SPRAYED == "FALSE"), ]
hist(PTNSHi$MED, col = 'light grey', main = 'Post-Treatment/Recovery, Not Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

PTSPHi <- HiMEDAmalg[ which(HiMEDAmalg$TIME=='PT'
                            & HiMEDAmalg$SPRAYED == "TRUE"),]
hist(PTSP$MED, col = 'light grey', main = 'Post-Treatment/Recovery, Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

mtext("Distribution of Spring Inflection Point Estimates for the High MED Group",                   # Add main title
      side = 3,
      line = - 1,
      outer = TRUE)

median(PDSPHi$MED)
median(PDNSHi$MED)
median(INSPHi$MED)
median(INNSHi$MED)
median(PTSPHi$MED)
median(PTNSHi$MED)

##LO
library("ggpubr")
Initial <- ggboxplot(LowMEDAmalg, x = "TIME", y = "MED", color = "SPRAYED",
                     palette = c("black", "dark grey"))
Initial + theme(text = element_text(family = "serif")) + ggtitle("Median Spring Inflection Point Estimates by Period of Time (Early Spring Inflection Point Group)") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Time Period", y="Median Spring Inflection Point Estimates (Day of Year)", color='Herbicide-Treated') + theme(legend.position="bottom")                                                                                                                                                                                                                                 


ad.test(LowMEDAmalg$MED)
##Not normal


modelLo = art(MED ~ SPRAYED + TIME + SPRAYED:TIME, data = LowMEDAmalg)
modelLo
anova(modelLo)
## All significant
##Tukey post-hoc
marginalLo = art.con(modelLo, "SPRAYED:TIME", adjust="tukey")
marginalLo
write.table(marginalLo, file="C:/Users/Heather Patterson/Downloads/MarginalLow.csv", sep=",", col.names=NA)

##PD is significantly different between SPRAYED T and F 

###Histograms Lo
attach(mtcars)
par(mfcol=c(2,3))
par(family = "times", font = 2, font.lab = 2, font.axis = 2)
PDNSLo <- LowMEDAmalg[ which(LowMEDAmalg$TIME=='PD'
                             & LowMEDAmalg$SPRAYED == "FALSE"), ]
hist(PDNSLo$MED, col = 'black', main = 'Pre-Disturbance, Not Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

PDSPLo <- LowMEDAmalg[ which(LowMEDAmalg$TIME=='PD'
                             & LowMEDAmalg$SPRAYED == "TRUE"),]
hist(PDSPLo$MED, col = 'black', main = 'Pre-Disturbance, Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

INNSLo <- LowMEDAmalg[ which(LowMEDAmalg$TIME=='IN'
                             & LowMEDAmalg$SPRAYED == "FALSE"), ]
hist(INNSLo$MED, col = 'dark grey', main = 'Intermediate, Not Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

INSPLo <- LowMEDAmalg[ which(LowMEDAmalg$TIME=='IN'
                             & LowMEDAmalg$SPRAYED == "TRUE"),]
hist(INSPLo$MED, col = 'dark grey', main = 'Intermediate, Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

PTNSLo <- LowMEDAmalg[ which(LowMEDAmalg$TIME=='PT'
                             & LowMEDAmalg$SPRAYED == "FALSE"), ]
hist(PTNSLo$MED, col = 'light grey', main = 'Post-Treatment/Recovery, Not Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')

PTSPLo <- LowMEDAmalg[ which(LowMEDAmalg$TIME=='PT'
                             & LowMEDAmalg$SPRAYED == "TRUE"),]
hist(PTSPLo$MED, col = 'light grey', main = 'Post-Treatment/Recovery, Herbicide Treated',xlab = 'Spring Inflection Point Estimate (Day of Year)')
mtext("Distribution of Spring Inflection Point Estimates for the Low MED Group",                   # Add main title
      side = 3,
      line = - 1,
      outer = TRUE)
median(PDSPLo$MED)
median(PDNSLo$MED)
median(INSPLo$MED)
median(INNSLo$MED)
median(PTSPLo$MED)
median(PTNSLo$MED)

write.table(LowMEDAmalg, file="C:/Users/Heather Patterson/Downloads/LowMEDAmalg.csv", sep=",", col.names=NA)
write.table(HiMEDAmalg, file="C:/Users/Heather Patterson/Downloads/HiMEDAmalg.csv", sep=",", col.names=NA)


shapiro.test(PDSPLo$MED)
shapiro.test(PDNSLo$MED)
shapiro.test(INSPLo$MED)
shapiro.test(INNSLo$MED)
shapiro.test(PTSPLo$MED)
shapiro.test(PTNSLo$MED)

##Not Normal

shapiro.test(PDSPHi$MED)
shapiro.test(PDNSHi$MED)
shapiro.test(INSPHi$MED)
shapiro.test(INNSHi$MED)
shapiro.test(PTSPHi$MED)
shapiro.test(PTNSHi$MED)

##Not Normal




### Ecodistrict
###Subset by EcoDistrict
names(ANOVAReshapedMEDECOREGIONEQ8017292)[names(ANOVAReshapedMEDECOREGIONEQ8017292) == "ECODISTRIC"] <- "ECODISTRICT"
levels(factor(ANOVAReshapedMEDECOREGIONEQ8017292$ECODISTRICT)) 
PDALL <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$TIME=='PD')
HerbNoHerbCount2 <- table(PDALL$ECODISTRICT,PDALL$SPRAYED)
HerbNoHerbCount2DF <- as.data.frame(HerbNoHerbCount2)

### Remove all Ecodistricts N<10
ECODN10 <-subset(PDALL, ECODISTRICT=="Clay Belt"| ECODISTRICT=="English River"| ECODISTRICT=="Foleyet"| ECODISTRICT=="Geraldton"| ECODISTRICT=="Manitou"| ECODISTRICT=="Quetico"| ECODISTRICT=="Savanne"| ECODISTRICT=="Sioux Narrows"| ECODISTRICT=="St. Raphael Lake"| ECODISTRICT=="Temagami"|ECODISTRICT=="Whitewater Lake")
str(ECODN10)
ECODN10$ECODISTRICT <- as.factor(ECODN10$ECODISTRICT)
str(ECODN10)
##Plot
ECODN10plot <- qplot(ECOREGION, MED, data = ECODN10, geom = "boxplot", main = "Median Spring Inflection Point Estimates by EcoDistrict, All Sites in the Pre-Disturbance Period") + coord_flip()
loadfonts(device = "win")
ECODN10plot + theme(text = element_text(family = "serif")) + ggtitle("Spring Inflection Point Estimates by EcoDistrict, All Sites PD") + theme(plot.title = element_text(hjust = 0.5)) + aes(x=reorder(ECODISTRICT,-MED, na.rm = TRUE, y=ECODISTRICT)) + labs(x="EcoDistrict", y="Median Spring Inflection Point Estimates (MED)") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


kruskal.test(MED ~ ECODISTRICT, data = ECODN10)
###As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.

##Multiple pairwise-comparison between groups Dunn

DUNNTESTRESULTSPDALL<- data.frame

DUNNTESTRESULTSPDALL<-dunnTest(MED ~ ECODISTRICT, data = ECODN10, method = "holm")
write.table(DUNNTESTRESULTSPDALL[["res"]], file="C:/Users/Heather Patterson/Downloads/DUNNTESTRESULTSPDALLECODISTRICT.csv", sep=",", col.names=NA)

###Summary
library(plyr)
FACTORMEANSECOD<-ddply(ECODN10, .(ECODISTRICT), summarize, mean=mean(MED))
library(dplyr)
ECODCount <- ECODN10 %>%
  group_by(ECODISTRICT) %>%
  dplyr::summarize(count = n())

ECODSumm <- as.data.frame(merge(ECODCount, FACTORMEANSECOD, on="ECODISTRICT", how="left"))

write.table(ECODSumm, file="C:/Users/Heather Patterson/Downloads/ECODSumm.csv", sep=",", col.names=NA)


##Aligned Ranks Transformation ANOVA https://rcompanion.org/handbook/F_16.html
PDALL2 <- subset(ANOVAReshapedMEDECOREGIONEQ8017292, ANOVAReshapedMEDECOREGIONEQ8017292$TIME=='PD')
PDALL4 <- subset(PDALL2, PDALL2$ECODISTRICT=='Clay Belt' | PDALL2$ECODISTRICT=='English River'| PDALL2$ECODISTRICT=='Foleyet'| PDALL2$ECODISTRICT=='Geraldton'| PDALL2$ECODISTRICT=='Manitou'| PDALL2$ECODISTRICT=='Quetico'| PDALL2$ECODISTRICT=='Savanne'| PDALL2$ECODISTRICT=='Sioux Narrows'| PDALL2$ECODISTRICT=='St. Raphael Lake'| PDALL2$ECODISTRICT=='Temagami'| PDALL2$ECODISTRICT=='Whitewater Lake')
str(PDALL4)
PDALL4$ECODISTRICT <- as.factor(PDALL4$ECODISTRICT)
str(PDALL4)
MEDECOREGIONPDALL4 <- qplot(ECODISTRICT, MED, data = PDALL4, geom = "boxplot", main = "MED by EcoDistrict, All Sites PD") + coord_flip()
loadfonts(device = "win")
MEDECOREGIONPDALL4 + theme(text = element_text(family = "serif")) + ggtitle("MED by EcoDistrict, All Sites PD") + theme(plot.title = element_text(hjust = 0.5)) + aes(x=reorder(ECODISTRICT,-MED, na.rm = TRUE, y=EcoDistrict)) + labs(x="EcoDistrict", y="Median Spring Inflection Point Estimates (MED)") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

Initial <- ggboxplot(PDALL4, x = "ECODISTRICT", y = "MED", color = "SPRAYED",
                     palette = c("black", "dark grey")) + coord_flip()
Initial + theme(text = element_text(family = "serif")) + ggtitle("Median Spring Inflection Point Estimates by EcoDistrict") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="EcoDistrict", y="Median Spring Inflection Point Estimates (Day of Year)", color='Herbicide-Treated') + theme(legend.position="bottom")                                                                                                                                                                                                                                 





modelALL3 = art(MED ~ SPRAYED + ECODISTRICT + SPRAYED:ECODISTRICT, data = PDALL4)
modelALL3
anova(modelALL3)


## All significant
##Tukey post-hoc
marginalALL3 = art.con(modelALL3, "SPRAYED:ECODISTRICT", adjust="tukey")
marginalALL3
write.table(marginalALL3, file="C:/Users/Heather Patterson/Downloads/MarginalALL3.csv", sep=",", col.names=NA)







###Re-reun without EcoDistricts with significant PD differences
##Aligned Ranks Transformation ANOVA https://rcompanion.org/handbook/F_16.html

Minus<- as.data.frame(ANOVAReshapedMEDECOREGIONEQ8017292)
levels(factor(Minus$ECODISTRICT)) 
MinusProblemEcoDistricts <-subset(Minus, ECODISTRICT=="Clay Belt"| ECODISTRICT=="English River"| ECODISTRICT=="Geraldton"| ECODISTRICT=="Manitou"| ECODISTRICT=="Sioux Narrows"| ECODISTRICT=="St. Raphael Lake"| ECODISTRICT=="Temagami"|ECODISTRICT=="Whitewater Lake")
str(MinusProblemEcoDistricts)
table(MinusProblemEcoDistricts$ECODISTRICT,MinusProblemEcoDistricts$TIME)
library("ggpubr")
Initial <- ggboxplot(MinusProblemEcoDistricts, x = "TIME", y = "MED", color = "SPRAYED",
                     palette = c("black", "dark grey"))
Initial + theme(text = element_text(family = "serif")) + ggtitle("Median Spring Inflection Point Estimates by Period of Time with Potential Problem EcoDistricts Removed") + theme(plot.title = element_text(hjust = 0.5)) + labs(x="Time Period", y="Median Spring Inflection Point Estimates (Day of Year)", color='Herbicide-Treated') + theme(legend.position="bottom")                                                                                                                                                                                                                                 

table(MinusProblemEcoDistricts$SPRAYED,MinusProblemEcoDistricts$TIME)

modelALL5 = art(MED ~ SPRAYED + TIME + SPRAYED:TIME, data = MinusProblemEcoDistricts)
modelALL5
anova(modelALL5)
## All significant
##Tukey post-hoc
marginalALL5 = art.con(modelALL5, "SPRAYED:TIME", adjust="tukey")
marginalALL5
write.table(marginalALL5, file="C:/Users/Heather Patterson/Downloads/MarginalALLPotentialProblemEcodistrictsRemoved.csv", sep=",", col.names=NA)
##PD still significantly different p=0.00767821



