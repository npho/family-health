# preprocessing-natality.R
# loads natality data from 2017 from NVSS of NCHS
# https://www.nber.org/data/vital-statistics-natality-data.html 
# preprocesses for outcome variable and features of interest 

library(data.table)
library(tidyverse)

###
# load data
natl2017 <- fread("~/Downloads/natl2017.csv", integer64="character")
natl2017 <- Filter(function(x) { n_distinct(x) > 1 }, natl2017) # removes features with no distinct value

###
# subset and clean data for use
N <- ncol(natl2017)
nicu.features <- data.frame(x=rep("", N), class=rep("", N), desc=rep("", N), stringsAsFactors=F)

# Temporal Data
nicu.features[ 1,] <- list("DOB_MM", "factor", "Birth Month")
nicu.features[ 2,] <- list("DOB_WK", "factor", "Birth Day of Week")
nicu.features[ 3,] <- list("BFACIL", "factor", "Birth Place")

# Maternal Demographic
nicu.features[ 4,] <- list("MAGER", "numeric", "Mother's Single Years of Age")
nicu.features[ 5,] <- list("MRACEHISP", "factor", "Mother's Race/Hispanic Origin")
nicu.features[ 6,] <- list("DMAR", "factor", "Marital Status")
nicu.features[ 7,] <- list("MEDUC", "factor", "Mother's Education")

# Paternal Demographic
nicu.features[ 8,] <- list("FAGECOMB", "numeric", "Father's Combined Age")
nicu.features[ 9,] <- list("FRACEHISP", "factor", "Father's Race/Hispanic Origin")
nicu.features[10,] <- list("FEDUC", "factor", "Father's Education")

# Maternal History
nicu.features[11,] <- list("PRIORLIVE", "numeric", "Prior Births Now Living")
nicu.features[12,] <- list("PRIORDEAD", "numeric", "Prior Births Now Dead")
nicu.features[13,] <- list("PRIORTERM", "numeric", "Prior Other Terminations")
nicu.features[14,] <- list("LBO_REC", "numeric", "Live Birth Order Recode")
nicu.features[15,] <- list("TBO_REC", "numeric", "Total Birth Order Recode")
nicu.features[16,] <- list("ILLB_R", "numeric", "Interval Since Last Live Birth Recode")
nicu.features[17,] <- list("ILOP_R", "numeric", "Interval Since Last Other Pregnancy Recode")

# Pre-delivery Maternal Care
nicu.features[18,] <- list("PRECARE", "numeric", "Month Prenatal Care Began")
nicu.features[19,] <- list("PREVIS", "numeric", "Number of Prenatal Visits")

# Fetal Nutrition and Environment
nicu.features[20,] <- list("WIC", "factor", "Special Supplemental Nutrition Program for Women, Infants, and Children")
nicu.features[21,] <- list("CIG_0", "numeric", "Cigarettes Before Pregnancy")
nicu.features[22,] <- list("CIG_1", "numeric", "Cigarettes 1st Trimester")
nicu.features[22,] <- list("CIG_2", "numeric", "Cigarettes 2nd Trimester")
nicu.features[23,] <- list("CIG_3", "numeric", "Cigarettes 3rd Trimester")

# Maternal Features
nicu.features[24,] <- list("M_Ht_In", "numeric", "Mother's Height in Total Inches")
nicu.features[25,] <- list("BMI", "numeric", "Mother's BMI")
nicu.features[26,] <- list("PWgt_R", "numeric", "Pre-pregnancy Weight Recode")
#nicu.features[27,] <- list("DBWT", "numeric", "Delivery Weight")
nicu.features[28,] <- list("WTGAIN", "numeric", "Weight Gain")

# Risk Factors 313-342
nicu.features[29,] <- list("RF_PDIAB", "factor", "Pre-pregnancy Diabetes")
nicu.features[30,] <- list("RF_GDIAB", "factor", "Gestational Diabetes")
nicu.features[31,] <- list("RF_PHYPE", "factor", "Pre-pregnancy Hypertension")
nicu.features[32,] <- list("RF_GHYPE", "factor", "Gestational Hypertension")
nicu.features[33,] <- list("RF_EHYPE", "factor", "Hypertension Eclampsia")
nicu.features[34,] <- list("RF_PPTERM", "factor", "Previous Preterm Birth")
nicu.features[35,] <- list("RF_INFTR", "factor", "Infertility Treatment Used")
nicu.features[36,] <- list("RF_FEDRG", "factor", "Fertility Enhancing Drugs")
nicu.features[37,] <- list("RF_ARTEC", "factor", "Asst. Reproductive Technology")
nicu.features[38,] <- list("RF_CESAR", "factor", "Prevoius Cesarean")
nicu.features[39,] <- list("RF_CESARN", "numeric", "Number of Previous Cesareans")
#nicu.features[40,] <- list("NO_RISKS", "factor", "No Risk Factors Reported")

# Infections Present 343-358 
nicu.features[41,] <- list("IP_GON", "factor", "Gonorrhea")
nicu.features[42,] <- list("IP_SYPH", "factor", "Syphilis")
nicu.features[43,] <- list("IP_CHLAM", "factor", "Chlamydia")
nicu.features[44,] <- list("IP_HEPatB", "factor", "Hepatitis B")
nicu.features[45,] <- list("IP_HEPatC", "factor", "Hepatitis C")
#nicu.features[46,] <- list("NO_INFEC", "factor", "No Infections Reported")

# Obstetric Procedures 359-370
nicu.features[47,] <- list("OB_ECVS", "factor" , "Successful External Cephalic Version")
nicu.features[48,] <- list("OB_ECVF", "factor", "Failed External Cephalic Version")

# Characteristics of Labor and Delivery 383-400
nicu.features[49,] <- list("LD_INDL", "factor", "Induction of Labor")
nicu.features[50,] <- list("LD_AUGM", "factor", "Augmentation of Labor")
nicu.features[51,] <- list("LD_STER", "factor", "Steroids")
nicu.features[52,] <- list("LD_ANTB", "factor", "Antibiotics")
nicu.features[53,] <- list("LD_CHOR", "factor", "Chorioamnionitis")
nicu.features[54,] <- list("LD_ANES", "factor", "Anesthesia")

# Maternal Morbidity 415-432 
# NO_MMORB, No Maternal Morbidity, outcome variable

# Administrative
nicu.features[55,] <- list("ATTEND", "factor", "Attendant at Birth")
nicu.features[56,] <- list("PAY", "factor", "Payment Source for Delivery")

# Other Baby Info
# Skip APGAR
nicu.features[57,] <- list("DPLURAL", "numeric", "Birth Plurality")
nicu.features[58,] <- list("SETORDER_R", "numeric", "Set Order Recode")
nicu.features[59,] <- list("SEX", "factor", "Sex of Baby")
#nicu.features[60,] <- list("COMBGEST", "numeric", "Combined Gestation")

# Abnormal Conditions of the Newborn 517-536
# NO_ABNORM, No Abnormal Conditions Checked, outcome variable

# Congenital Anomalies of the Newborn 537-566
nicu.features[61,] <- list("CA_ANEN", "factor", "Anencephaly")
nicu.features[62,] <- list("CA_MNSB", "factor", "Meningomyelocele / Spina Bifida")
nicu.features[63,] <- list("CA_CCHD", "factor", "Cyanotic Congenital Heart Disease")
nicu.features[64,] <- list("CA_CDH", "factor", "Congenital Diaphragmatic Hernia")
nicu.features[65,] <- list("CA_OMPH", "factor", "Omphalocele")
nicu.features[66,] <- list("CA_GAST", "factor", "Gastroschisis")
nicu.features[67,] <- list("CA_LIMB", "factor", "Limb Reduction Defect")
nicu.features[68,] <- list("CA_CLEFT", "factor", "Cleft Lip w/ or w/o Cleft Palate")
nicu.features[69,] <- list("CA_CLPAL", "factor", "Cleft Palate alone")
nicu.features[70,] <- list("CA_DOWNs", "factor", "Down Syndrome")
nicu.features[71,] <- list("CA_DISOR", "factor", "Suspected Chromosomal Disorder")
nicu.features[72,] <- list("CA_HYPO", "factor", "Hypospadias")
#nicu.features[73,] <- list("NO_CONGEN", "factor", "No Congenital Anomalies Checked")

nicu.features <- nicu.features[nicu.features$x!="", ] # drop empty rows
nicu.features$x <- nicu.features$x %>% tolower()

# collapse all maternal complications into yes (1) or no (0)
natl2017 <- natl2017 %>% 
            mutate(m_comp=ifelse(mm_mtr=='Y'|mm_plac=='Y'|mm_rupt=='Y'|mm_uhyst=='Y'|mm_aicu=='Y', 1, 0))

# collapse all baby complications into yes (1) or no (0)
natl2017 <- natl2017 %>% 
            mutate(b_comp=ifelse(ab_aven1=='Y'|ab_aven6=='Y'|ab_nicu=='Y'|ab_surf=='Y'|ab_anti=='Y'|ab_seiz=='Y', 1, 0))

# collapse either maternal or baby complications into yes (1) or no (0)
natl2017 <- natl2017 %>% mutate(e_comp=ifelse(m_comp==1|b_comp==1, 1, 0))

# just get the features and correct mutated columns
nicu <- natl2017 %>% select(m_comp, b_comp, e_comp, nicu.features$x) %>% as.data.frame()
nicu$m_comp <- factor(nicu$m_comp, levels=c(0, 1), labels=c('N', 'Y'))
nicu$b_comp <- factor(nicu$b_comp, levels=c(0, 1), labels=c('N', 'Y'))
nicu$e_comp <- factor(nicu$e_comp, levels=c(0, 1), labels=c('N', 'Y'))

# get colindex by data type
mask.numeric <- colnames(nicu) %in% subset(nicu.features, class=="numeric")$x
mask.factor  <- colnames(nicu) %in% subset(nicu.features, class=="factor")$x

# set to NA all unknown values
nicu[, mask.numeric] <- lapply(nicu[, mask.numeric], function(x) { na_if(x, max(unique(x))) %>% as.numeric() })
nicu[, mask.factor]  <- lapply(nicu[, mask.factor],  function(x) { na_if(x, 'U') })

nicu <- nicu[complete.cases(nicu),] # grab only complete cases 

nicu[, mask.factor]  <- lapply(nicu[, mask.factor],  function(x) { make.names(x) %>% as.factor() })

###
# split data into training and testing subsets

# table(natl2017$e_comp) %>% prop.table() # complications to mom or child occur in 11.9% 459221/(459221+3405533)

# sub-sample for smaller data sets, faster iterations (80-20 for test-train)
i <- base::sample(1:nrow(nicu), replace=FALSE, size=1e4) # 10,000
nicu.test  <- nicu %>% slice(i) 

nicu.train1 <- nicu %>% slice(-i) %>% filter(e_comp=='Y') %>% sample_n(2e4) # YES complications w/20,000
nicu.train2 <- nicu %>% slice(-i) %>% filter(e_comp=='N') %>% sample_n(2e4) # NO complications w/20,000
nicu.train <- rbind(nicu.train1, nicu.train2)

###
# data ready to be processed by different supervised learning algorithms
cat("Writing processed natality data...\n")
save(natl2017, nicu.features, file=paste0("~/src/family-health/nicu-all.RData"))
save(nicu, nicu.test, nicu.train, nicu.features, file=paste0("~/src/family-health/nicu-", as.integer(as.POSIXct(Sys.time())), ".RData"))
