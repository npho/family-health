# preprocessing.R
# loads linked infant death data from 2012 from NVSS of NCHS
# https://www.nber.org/data/lbid.html
# preprocesses for outcome variable and features of interest 

library(data.table)
library(tidyverse)

###
# load data
ddat2012 <- fread("~/Downloads/linkco2012us_den.csv", integer64="character")
ddat2012 <- Filter(function(x) { n_distinct(x) > 1 }, ddat2012) # removes features with no distinct value
ddat2012 <- ddat2012 %>% select(-starts_with("f_")) # removes flag columns

###
# subset and clean data for use
N <- ncol(ddat2012)
ddat.features <- data.frame(x=rep("", N), class=rep("", N), desc=rep("", N), stringsAsFactors=F)

# Temporal Data
ddat.features[ 1,] <- list("DOB_MM", "factor", "Birth Month")
ddat.features[ 2,] <- list("DOB_WK", "factor", "Birth Day of Week")
ddat.features[ 3,] <- list("BFACIL", "factor", "Birth Place")

# Maternal Demographic
ddat.features[ 4,] <- list("MAGER41", "numeric", "Mother's Single Years of Age")
ddat.features[ 5,] <- list("RESTATUS", "factor", "Residence Status")
ddat.features[ 6,] <- list("MEDUC", "factor", "Mother's Education")
ddat.features[ 7,] <- list("MRACEHISP", "factor", "Mother's Race/Hispanic Origin")

# Paternal Demographic
ddat.features[ 8,] <- list("FAGECOMB", "numeric", "Father's Combined Age")
ddat.features[ 9,] <- list("FRACEHISP", "factor", "Father's Race/Hispanic Origin")

# Maternal History
ddat.features[10,] <- list("LBO", "numeric", "Live Birth Order Recode")
ddat.features[11,] <- list("TBO", "numeric", "Total Birth Order Recode")

# Pre-delivery Maternal Care
ddat.features[12,] <- list("PRECARE", "numeric", "Month Prenatal Care Began")
ddat.features[13,] <- list("UPREVIS", "numeric", "Number of Prenatal Visits")
ddat.features[14,] <- list("WTGAIN", "numeric", "Weight Gain")

# Fetal Nutrition and Environment
ddat.features[15,] <- list("CIG_1", "numeric", "Cigarettes 1st Trimester")
ddat.features[16,] <- list("CIG_2", "numeric", "Cigarettes 2nd Trimester")
ddat.features[17,] <- list("CIG_3", "numeric", "Cigarettes 3rd Trimester")

# Risk Factors
ddat.features[18,] <- list("RF_DIAB", "factor", "Pre-pregnancy Diabetes")
ddat.features[19,] <- list("RF_GEST", "factor", "Gestational Diabetes")
ddat.features[20,] <- list("RF_PHYP", "factor", "Pre-pregnancy Hypertension")
ddat.features[21,] <- list("RF_GHYP", "factor", "Gestational Hypertension")
ddat.features[22,] <- list("RF_ECLAM", "factor", "Hypertension Eclampsia")
ddat.features[23,] <- list("RF_PPTERM", "factor", "Previous Preterm Birth")
ddat.features[24,] <- list("RF_PPOUTC", "factor", "Poor Pregnancy Outcome")
ddat.features[25,] <- list("RF_CESAR", "factor", "Previous Cesareans")
ddat.features[26,] <- list("RF_NCESAR", "numeric", "Number of Previous Cesareans")
ddat.features[27,] <- list("URF_DIAB", "factor", "Diabetes")
ddat.features[28,] <- list("URF_CHYPER", "factor", "Chronic Hypertension")
ddat.features[29,] <- list("URF_PHYPER", "factor", "Pre-pregnancy Associated Hypertension")
ddat.features[30,] <- list("URF_ECLAM", "factor", "Eclampsia")

# Obstetric Procedures
ddat.features[31,] <- list("OP_CERV", "factor", "Cervical Cerclage")
ddat.features[32,] <- list("OP_TOCOL", "factor", "Tocolysis")
ddat.features[33,] <- list("OP_ECVS", "factor", "Successful External Cephalic Version")
ddat.features[34,] <- list("OP_ECVF", "factor", "Failed External Cephalic Version")
ddat.features[35,] <- list("UOP_INDUC", "factor", "Induction of Labor")
ddat.features[36,] <- list("UOP_TOCOL", "factor", "Tocolysis")

# Onset of Labor
ddat.features[37,] <- list("ON_RUPTR", "factor", "Pre-mature Rupture of Membrane")
ddat.features[38,] <- list("ON_PROLG", "factor", "Prolonged Labor")

# Characteristics of Labor and Delivery
ddat.features[39,] <- list("LD_INDUCT", "factor", "Induction of Labor")
ddat.features[40,] <- list("ld_augment", "factor", "Augmentation of Labor")
ddat.features[41,] <- list("ld_steroids", "factor", "Steroids")
ddat.features[42,] <- list("ld_antibio", "factor", "Antibiotics")
ddat.features[43,] <- list("ld_chorio", "factor", "Chorioamnionitis")
ddat.features[44,] <- list("ld_mecon", "factor", "Meconium Staining")
ddat.features[45,] <- list("ld_fintol", "factor", "Fetal Intolerance")
ddat.features[46,] <- list("ld_anesth", "factor", "Anesthesia")

# Labor Delivery Complications
ddat.features[47,] <- list("uld_meco", "factor", "Meconium")
ddat.features[48,] <- list("uld_precip", "factor", "Precipitous Labor")
ddat.features[49,] <- list("uld_breech", "factor", "Breech")

ddat.features[50,] <- list("md_present", "factor", "Fetal Presentation")
ddat.features[51,] <- list("md_route", "factor", "Final Route and Method of Delivery")
ddat.features[52,] <- list("md_trial", "factor", "Trial of Labor Attempted")

# Method of Delivery
ddat.features[53,] <- list("ume_forcp", "factor", "Forceps")
ddat.features[54,] <- list("ume_vac", "factor", "Vacuum")
ddat.features[55,] <- list("rdmeth_rec", "factor", "Delivery Method Recode")

ddat.features[56,] <- list("ATTEND", "factor", "Attendant at Birth")

ddat.features[57,] <- list("apgar5", "numeric", "Five Minute APGAR Score")
ddat.features[58,] <- list("DPLURAL", "numeric", "Birth Plurality")
ddat.features[59,] <- list("SEX", "factor", "Sex of Baby")
ddat.features[60,] <- list("dlmp_mm", "factor", "Last Normal Menses Month")
ddat.features[61,] <- list("dlmp_yy", "numeric", "Year of Last Normal Menses Began")
ddat.features[62,] <- list("estgest", "numeric", "Obstetric/Clinical Gestation Est")
ddat.features[63,] <- list("dbwt", "numeric", "Delivery Birth Weight in Grams")

# Abnormal Conditions of the Newborn 517-536
ddat.features[64,] <- list("ab_vent", "factor", "Assisted Ventilation")
ddat.features[65,] <- list("ab_vent6", "factor", "Assisted Ventilation > 6 hrs")
ddat.features[66,] <- list("ab_nicu", "factor", "Admission to NICU")
ddat.features[67,] <- list("ab_surfac", "factor", "Surfactant")
ddat.features[68,] <- list("ab_antibio", "factor", "Antibiotics")

# Congenital Anomalies of the Newborn 537-566
ddat.features[69,] <- list("CA_ANEN", "factor", "Anencephaly")
ddat.features[70,] <- list("ca_menin", "factor", "Meningomyelocele / Spina Bifida")
ddat.features[71,] <- list("ca_heart", "factor", "Cyanotic Congenital Heart Disease")
ddat.features[72,] <- list("ca_hernia", "factor", "Congenital Diaphragmatic Hernia")
ddat.features[73,] <- list("ca_ompha", "factor", "Omphalocele")
ddat.features[74,] <- list("ca_gastro", "factor", "Gastroschisis")
ddat.features[75,] <- list("CA_LIMB", "factor", "Limb Reduction Defect")
ddat.features[76,] <- list("ca_cleftlp", "factor", "Cleft Lip w/ or w/o Cleft Palate")
ddat.features[77,] <- list("ca_cleft", "factor", "Cleft Palate alone")
ddat.features[78,] <- list("CA_DOWNS", "factor", "Down Syndrome")
ddat.features[79,] <- list("ca_chrom", "factor", "Suspected Chromosomal Disorder")
ddat.features[80,] <- list("ca_hypos", "factor", "Hypospadias")

ddat.features[81,] <- list("uca_anen", "factor", "Anencephalus")
ddat.features[82,] <- list("uca_spina", "factor", "Spina Bifida")
ddat.features[83,] <- list("uca_ompha", "factor", "Omphalocele / Gastroschisis")
ddat.features[84,] <- list("uca_cleftlp", "factor", "Cleft Lip/Palate ")
ddat.features[85,] <- list("uca_downs", "factor", "Downs Syndrome")

ddat.features[86,] <- list("aged", "numeric", "Age at Death in Days")
#ddat.features[87,] <- list("place", "factor", "Place of injury")

ddat.features <- ddat.features[ddat.features$x!="", ] # drop empty rows
ddat.features$x <- ddat.features$x %>% tolower()

# just get the features and correct mutated columns
ddat <- ddat2012 %>% select(ddat.features$x) %>% as.data.frame()

# collapse into outcome variable, alive after 1 year? yes (1) or no (0)
ddat$alive <- ddat$aged %>% replace_na(-1)
ddat$alive <- ifelse(ddat$alive > -1, 0, -1)
ddat$alive <- -1 * ddat$alive
ddat$alive <- factor(ddat$alive, levels=c(0, 1), labels=c('N', 'Y'))
ddat <- ddat %>% select(-aged) # no longer needed, codified as "alive" categorical
ddat.features <- ddat.features %>% filter(x != "aged") # also remove from features since no longer there

# get colindex by data type
mask.numeric <- colnames(ddat) %in% subset(ddat.features, class=="numeric")$x
mask.factor  <- colnames(ddat) %in% subset(ddat.features, class=="factor")$x

ddat[, mask.numeric] <- lapply(ddat[, mask.numeric], function(x) { na_if(x, max(unique(x))) %>% as.numeric() })
ddat[, mask.factor]  <- lapply(ddat[, mask.factor],  function(x) { na_if(x, 'U') })

ddat <- ddat[complete.cases(ddat),] # grab only complete cases 

ddat[, mask.factor]  <- lapply(ddat[, mask.factor],  function(x) { make.names(x) %>% as.factor() }) # SLOW!

###
# split data into training and testing subsets

# ddat2012$aged %>% is.na() %>% table() %>% prop.table() # complications to mom or child occur in 0.59% 23401/(23401+3937396)

# sub-sample for smaller data sets, faster iterations (80-20 for test-train)
i <- base::sample(1:nrow(ddat), replace=FALSE, size=1e4) # 10,000
ddat.test  <- ddat %>% slice(i) 

ddat.train1 <- ddat %>% slice(-i) %>% filter(alive=='Y') %>% sample_n(1e4) # YES alive w/10,000
ddat.train2 <- ddat %>% slice(-i) %>% filter(alive=='N') %>% sample_n(1e4) # NO alive w/10,000
ddat.train <- rbind(ddat.train1, ddat.train2)

###
# data ready to be processed by different supervised learning algorithms
cat("Writing processed death data...\n")
save(ddat2012, ddat.features, file=paste0("~/src/family-health/dat/ddat-all.RData"))
save(ddat, ddat.test, ddat.train, ddat.features, file=paste0("~/src/family-health/dat/ddat-", as.integer(as.POSIXct(Sys.time())), ".RData"))
