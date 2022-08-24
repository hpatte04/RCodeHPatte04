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


##Import Libararies##
library(rsBayes)
library(magrittr)
library(tidyverse)
library(dplyr)
library(multcomp)
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




###ANOVA
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

ANOVADATACLEAN$SPRAYED <- as.factor(ANOVADATACLEAN$SPRAYED)
ANOVADATACLEAN$PD_MED <- as.factor(ANOVADATACLEAN$PD_MED)
ANOVADATACLEAN$IN_MED <- as.factor(ANOVADATACLEAN$IN_MED)
ANOVADATACLEAN$PT_MED <- as.factor(ANOVADATACLEAN$PT_MED)
str(ANOVADATACLEAN)

ANOVADATACLEANMEDPD <- data.frame (TARGET_FID = c(ANOVADATACLEAN$FID), SPRAYED = c(ANOVADATACLEAN$SPRAYED), TIME = "PD", MED = c(ANOVADATACLEAN$PD_MED))
ANOVADATACLEANMEDIN <- data.frame (TARGET_FID = c(ANOVADATACLEAN$FID), SPRAYED = c(ANOVADATACLEAN$SPRAYED), TIME = "IN", MED = c(ANOVADATACLEAN$IN_MED))
ANOVADATACLEANMEDPT <- data.frame (TARGET_FID = c(ANOVADATACLEAN$FID), SPRAYED = c(ANOVADATACLEAN$SPRAYED), TIME = "PT", MED = c(ANOVADATACLEAN$PT_MED))

ANOVAReshapedMED <- bind_rows(ANOVADATACLEANMEDIN, ANOVADATACLEANMEDPD, ANOVADATACLEANMEDPT)


ANOVAReshapedMED$SPRAYED <- as.factor(ANOVAReshapedMED$SPRAYED)
ANOVAReshapedMED$TIME <- factor(ANOVAReshapedMED$TIME, levels=c("PD","IN","PT"))

ANOVAReshapedMED$MED <- as.character(ANOVAReshapedMED$MED)
ANOVAReshapedMED$MED <- as.numeric(ANOVAReshapedMED$MED)


str(ANOVAReshapedMED)
write.csv(ANOVAReshapedMED, 'C:/Users/Heather Patterson/Downloads/R Data Rough/ANOVAReshapedMED.csv')

table(ANOVAReshapedMED$TIME, ANOVAReshapedMED$SPRAYED)
###Uneven design, use Type III Sums of Squares



# Box plot with multiple groups
library("ggpubr")
ggboxplot(ANOVAReshapedMED, x = "TIME", y = "MED", color = "SPRAYED",
          palette = c("#00AFBB", "#E7B800"))

# Line plot with multiple groups DOESNT WORK
ggline(ANOVAReshapedMED, x = "TIME", y = "MED", color = "SPRAYED",
       palette = c("#00AFBB", "#E7B800"))

##SUMMARY STATS
group_by(ANOVAReshapedMED, SPRAYED, TIME) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(MED, na.rm = TRUE),
    sd = sd(MED, na.rm = TRUE)
  )

### ANOVA ALL
library("car")
Anova(lm(MED ~ SPRAYED * TIME, ANOVAReshapedMED), type = '3')

### Both TIME and SPRAYED and their interactions are significant



###EcoRegion
## Look-Up Table (LUT) for Ecoregion info
LUT2 <- read.csv('C:/Users/Heather Patterson/Downloads/R Data Rough/HarvestInformationwithEcoregion.csv')

ANOVAReshapedMEDECO<-merge(x=ANOVAReshapedMED,y=LUT2,by="TARGET_FID",all.x=TRUE)
ANOVAReshapedMEDECOREGION <- subset(ANOVAReshapedMEDECO, select = c("SPRAYED","TIME","MED","SITE_REG_O"))

# group by EcoRegion
library(tidyverse)
ANOVAReshapedMEDECOREGION$SPRAYED <- as.factor(ANOVAReshapedMEDECOREGION$SPRAYED)
ANOVAReshapedMEDECOREGION$TIME <- factor(ANOVAReshapedMEDECOREGION$TIME, levels=c("PD","IN","PT"))
ANOVAReshapedMEDECOREGION$SITE_REG_O <- as.factor(ANOVAReshapedMEDECOREGION$SITE_REG_O)

ANOVAReshapedMEDECOREGION$MED <- as.character(ANOVAReshapedMEDECOREGION$MED)
ANOVAReshapedMEDECOREGION$MED <- as.numeric(ANOVAReshapedMEDECOREGION$MED)

str(ANOVAReshapedMEDECOREGION)

ANOVAReshapedMEDECOREGION %>%
  group_by(SITE_REG_O, TIME, SPRAYED)
  summarize(mean_MED=mean(MED))







##Tukey

TukeyHSD(res.aov2, which = "TIME")



library(multcomp)
summary(glht(res.aov2, linfct = mcp(TIME = "Tukey")))

##Check Normality
hist(ANOVAReshapedMED$MED, col = 'blue', main = 'Distribution of MED',xlab = 'MED')
hist(res.aov2, col = 'blue', main = 'Distribution of MED Residuals',xlab = 'MED')

# Extract the residuals
aov_residuals <- residuals(object = res.aov2)
install.packages("nortest")
library(nortest)
ad.test(x = aov_residuals)
###Residuals are not normal p<0.05

