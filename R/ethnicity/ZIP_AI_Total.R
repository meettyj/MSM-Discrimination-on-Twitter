require(MASS)
require(xlsx)
require(pscl)
# install.packages("MASS")
# install.packages("xlsx")

# Set the working environment here
setwd("C:/Users/eddie/Desktop/Prof. Rumi Chunara/2019_Fall/msm-discrimination-on-twitter/")

survey_data_path <- "data/"
survey_data_file <- "P18_Final_Data_07162019.csv"
discrimination_data_path <- "data/discrimination/"
discrimination_data_file <- "P18_GPS_AWM_Twitter_data_2_1_2020.xlsx"

survey_data <- read.csv(paste(survey_data_path,survey_data_file,sep=""),header=TRUE)
# survey_data_covariates <- survey_data[c("a_pid","agef1y","racefa1_b","eduf3","eduf1","bornusf1_b","income_su")]
# survey_data_outcomes <- survey_data[c("AI_Total", "AI_Condom", "AI_Totalless")]

discrimination_data <- read.xlsx(paste(discrimination_data_path,discrimination_data_file,sep=""),sheetIndex=1,colNames = TRUE)
discrimination_data
#test_data <- survey_data[c('a_pid','agef1y','racefa1_b')]
#test_data

merged_survey_discrimination_data <- merge(survey_data,discrimination_data,by.x = 'a_pid', by.y = 'PID')
merged_survey_discrimination_data

# remove NA value in income_su
merged_survey_discrimination_data$income_su
which(merged_survey_discrimination_data$income_su =="NA")
merged_survey_discrimination_data <- merged_survey_discrimination_data[which(merged_survey_discrimination_data$income_su !="NA"), ]
merged_survey_discrimination_data$income_su


# convert age
merged_survey_discrimination_data['agef1y'] <- 2019 - merged_survey_discrimination_data$agef1y
merged_survey_discrimination_data['agef1y']

# add dummy variable column for race: racefa1_b
merged_survey_discrimination_data$race_hispanic <- merged_survey_discrimination_data$racefa1_b
merged_survey_discrimination_data$race_black <- merged_survey_discrimination_data$racefa1_b
merged_survey_discrimination_data$race_asian <- merged_survey_discrimination_data$racefa1_b
merged_survey_discrimination_data$race_mixed_or_other <- merged_survey_discrimination_data$racefa1_b
merged_survey_discrimination_data$race_white <- merged_survey_discrimination_data$racefa1_b
# merged_survey_discrimination_data$race_asian <- merged_survey_discrimination_data$racefa1_b

merged_survey_discrimination_data$race_hispanic[merged_survey_discrimination_data$race_hispanic != 1]<-0

merged_survey_discrimination_data$race_black[merged_survey_discrimination_data$race_black != 2]<-0
merged_survey_discrimination_data$race_black[merged_survey_discrimination_data$race_black == 2]<-1

merged_survey_discrimination_data$race_asian[merged_survey_discrimination_data$race_asian != 3]<-0
merged_survey_discrimination_data$race_asian[merged_survey_discrimination_data$race_asian == 3]<-1

merged_survey_discrimination_data$race_mixed_or_other[merged_survey_discrimination_data$race_mixed_or_other != 4 & merged_survey_discrimination_data$race_mixed_or_other != 5]<-0
merged_survey_discrimination_data$race_mixed_or_other[merged_survey_discrimination_data$race_mixed_or_other == 4|merged_survey_discrimination_data$race_mixed_or_other == 5]<-1

merged_survey_discrimination_data$race_white[merged_survey_discrimination_data$race_white != 6]<-0
merged_survey_discrimination_data$race_white[merged_survey_discrimination_data$race_white == 6]<-1

# merged_survey_discrimination_data$race_other[merged_survey_discrimination_data$race_other != 5]<-0
# merged_survey_discrimination_data$race_other[merged_survey_discrimination_data$race_other == 5]<-1
# merged_survey_discrimination_data$race_asian[merged_survey_discrimination_data$race_asian != 6]<-0
# merged_survey_discrimination_data$race_asian[merged_survey_discrimination_data$race_asian == 6]<-1


# merged_survey_discrimination_data$racefa1_b
# merged_survey_discrimination_data$race_hispanic
# merged_survey_discrimination_data$race_black
# merged_survey_discrimination_data$race_asian
# merged_survey_discrimination_data$race_mixed_or_other
# merged_survey_discrimination_data$race_other
# merged_survey_discrimination_data$race_asian
# merged_survey_discrimination_data$race_white


# add dummy variable column for income: income_su
merged_survey_discrimination_data$income_su


merged_survey_discrimination_data$income_low <- merged_survey_discrimination_data$income_su
merged_survey_discrimination_data$income_medium <- merged_survey_discrimination_data$income_su
merged_survey_discrimination_data$income_high <- merged_survey_discrimination_data$income_su

merged_survey_discrimination_data$income_low[merged_survey_discrimination_data$income_low != 1]<-0

merged_survey_discrimination_data$income_medium[merged_survey_discrimination_data$income_medium != 2]<-0
merged_survey_discrimination_data$income_medium[merged_survey_discrimination_data$income_medium == 2]<-1

merged_survey_discrimination_data$income_high[merged_survey_discrimination_data$income_high != 3]<-0
merged_survey_discrimination_data$income_high[merged_survey_discrimination_data$income_high == 3]<-1

merged_survey_discrimination_data$income_low
merged_survey_discrimination_data$income_medium
merged_survey_discrimination_data$income_high


# remove NA value in AI_Total
merged_survey_discrimination_data$AI_Total
merged_survey_discrimination_data <- merged_survey_discrimination_data[which(merged_survey_discrimination_data$AI_Total !="NA"), ]
merged_survey_discrimination_data$AI_Total

# remove NA value in AWM_Rac_grid
merged_survey_discrimination_data$AWM_Rac_grid
merged_survey_discrimination_data <- merged_survey_discrimination_data[which(merged_survey_discrimination_data$AWM_Rac_grid !="NA"), ]
merged_survey_discrimination_data$AWM_Rac_grid

# remove NA value in AWM_Hom_grid
merged_survey_discrimination_data$AWM_Hom_grid
merged_survey_discrimination_data <- merged_survey_discrimination_data[which(merged_survey_discrimination_data$AWM_Hom_grid !="NA"), ]
merged_survey_discrimination_data$AWM_Hom_grid



# seperate the merged data by ethnicity
ethnicity_asian<-merged_survey_discrimination_data[merged_survey_discrimination_data$race_asian == 1,]
ethnicity_black<-merged_survey_discrimination_data[merged_survey_discrimination_data$race_black == 1,]
ethnicity_white<-merged_survey_discrimination_data[merged_survey_discrimination_data$race_white == 1,]
ethnicity_hispanic<-merged_survey_discrimination_data[merged_survey_discrimination_data$race_hispanic == 1,]
ethnicity_mixed_or_other<-merged_survey_discrimination_data[merged_survey_discrimination_data$race_mixed_or_other == 1,]


# ---- asian ---- Zero Inflated Poisson ----
ZIP_AI_Total_asian <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium, dist = "poisson", data=ethnicity_asian)
summary(ZIP_AI_Total_asian)

# discrimination variables
ZIP_AI_Total_AWM_SSSOM_Rac_asian <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_SSSOM_Rac, dist = "poisson", data=ethnicity_asian)
summary(ZIP_AI_Total_AWM_SSSOM_Rac_asian)

ZIP_AI_Total_AWM_SSSOM_Hom_asian <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_SSSOM_Hom, dist = "poisson", data=ethnicity_asian)
summary(ZIP_AI_Total_AWM_SSSOM_Hom_asian)

ZIP_AI_Total_AWM_Rac_grid_asian <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Rac_grid, dist = "poisson", data=ethnicity_asian)
summary(ZIP_AI_Total_AWM_Rac_grid_asian)

ZIP_AI_Total_AWM_Hom_grid_asian <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Hom_grid, dist = "poisson", data=ethnicity_asian)
summary(ZIP_AI_Total_AWM_Hom_grid_asian)

ZIP_AI_Total_AWM_Zip_Rac_asian <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Zip_Rac, dist = "poisson", data=ethnicity_asian)
summary(ZIP_AI_Total_AWM_Zip_Rac_asian)

ZIP_AI_Total_AWM_Zip_Hom_asian <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Zip_Hom, dist = "poisson", data=ethnicity_asian)
summary(ZIP_AI_Total_AWM_Zip_Hom_asian)



# ---- Black ---- Zero Inflated Poisson ----
ZIP_AI_Total_black <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium, dist = "poisson", data=ethnicity_black)
summary(ZIP_AI_Total_black)

# discrimination variables
ZIP_AI_Total_AWM_SSSOM_Rac_black <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_SSSOM_Rac, dist = "poisson", data=ethnicity_black)
summary(ZIP_AI_Total_AWM_SSSOM_Rac_black)

ZIP_AI_Total_AWM_SSSOM_Hom_black <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_SSSOM_Hom, dist = "poisson", data=ethnicity_black)
summary(ZIP_AI_Total_AWM_SSSOM_Hom_black)

ZIP_AI_Total_AWM_Rac_grid_black <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Rac_grid, dist = "poisson", data=ethnicity_black)
summary(ZIP_AI_Total_AWM_Rac_grid_black)

ZIP_AI_Total_AWM_Hom_grid_black <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Hom_grid, dist = "poisson", data=ethnicity_black)
summary(ZIP_AI_Total_AWM_Hom_grid_black)

ZIP_AI_Total_AWM_Zip_Rac_black <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Zip_Rac, dist = "poisson", data=ethnicity_black)
summary(ZIP_AI_Total_AWM_Zip_Rac_black)

ZIP_AI_Total_AWM_Zip_Hom_black <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Zip_Hom, dist = "poisson", data=ethnicity_black)
summary(ZIP_AI_Total_AWM_Zip_Hom_black)


# ---- white ---- Zero Inflated Poisson ----
ZIP_AI_Total_white <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium, dist = "poisson", data=ethnicity_white)
summary(ZIP_AI_Total_white)

# discrimination variables
ZIP_AI_Total_AWM_SSSOM_Rac_white <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_SSSOM_Rac, dist = "poisson", data=ethnicity_white)
summary(ZIP_AI_Total_AWM_SSSOM_Rac_white)

ZIP_AI_Total_AWM_SSSOM_Hom_white <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_SSSOM_Hom, dist = "poisson", data=ethnicity_white)
summary(ZIP_AI_Total_AWM_SSSOM_Hom_white)

ZIP_AI_Total_AWM_Rac_grid_white <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Rac_grid, dist = "poisson", data=ethnicity_white)
summary(ZIP_AI_Total_AWM_Rac_grid_white)

ZIP_AI_Total_AWM_Hom_grid_white <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Hom_grid, dist = "poisson", data=ethnicity_white)
summary(ZIP_AI_Total_AWM_Hom_grid_white)

ZIP_AI_Total_AWM_Zip_Rac_white <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Zip_Rac, dist = "poisson", data=ethnicity_white)
summary(ZIP_AI_Total_AWM_Zip_Rac_white)

ZIP_AI_Total_AWM_Zip_Hom_white <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Zip_Hom, dist = "poisson", data=ethnicity_white)
summary(ZIP_AI_Total_AWM_Zip_Hom_white)



# ---- hispanic ---- Zero Inflated Poisson ----
ZIP_AI_Total_hispanic <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium, dist = "poisson", data=ethnicity_hispanic)
summary(ZIP_AI_Total_hispanic)

# discrimination variables
ZIP_AI_Total_AWM_SSSOM_Rac_hispanic <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_SSSOM_Rac, dist = "poisson", data=ethnicity_hispanic)
summary(ZIP_AI_Total_AWM_SSSOM_Rac_hispanic)

ZIP_AI_Total_AWM_SSSOM_Hom_hispanic <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_SSSOM_Hom, dist = "poisson", data=ethnicity_hispanic)
summary(ZIP_AI_Total_AWM_SSSOM_Hom_hispanic)

ZIP_AI_Total_AWM_Rac_grid_hispanic <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Rac_grid, dist = "poisson", data=ethnicity_hispanic)
summary(ZIP_AI_Total_AWM_Rac_grid_hispanic)

ZIP_AI_Total_AWM_Hom_grid_hispanic <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Hom_grid, dist = "poisson", data=ethnicity_hispanic)
summary(ZIP_AI_Total_AWM_Hom_grid_hispanic)

ZIP_AI_Total_AWM_Zip_Rac_hispanic <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Zip_Rac, dist = "poisson", data=ethnicity_hispanic)
summary(ZIP_AI_Total_AWM_Zip_Rac_hispanic)

ZIP_AI_Total_AWM_Zip_Hom_hispanic <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Zip_Hom, dist = "poisson", data=ethnicity_hispanic)
summary(ZIP_AI_Total_AWM_Zip_Hom_hispanic)



# ---- mixed_or_other ---- Zero Inflated Poisson ----
ZIP_AI_Total_mixed_or_other <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium, dist = "poisson", data=ethnicity_mixed_or_other)
summary(ZIP_AI_Total_mixed_or_other)

# discrimination variables
ZIP_AI_Total_AWM_SSSOM_Rac_mixed_or_other <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_SSSOM_Rac, dist = "poisson", data=ethnicity_mixed_or_other)
summary(ZIP_AI_Total_AWM_SSSOM_Rac_mixed_or_other)

ZIP_AI_Total_AWM_SSSOM_Hom_mixed_or_other <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_SSSOM_Hom, dist = "poisson", data=ethnicity_mixed_or_other)
summary(ZIP_AI_Total_AWM_SSSOM_Hom_mixed_or_other)

ZIP_AI_Total_AWM_Rac_grid_mixed_or_other <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Rac_grid, dist = "poisson", data=ethnicity_mixed_or_other)
summary(ZIP_AI_Total_AWM_Rac_grid_mixed_or_other)

ZIP_AI_Total_AWM_Hom_grid_mixed_or_other <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Hom_grid, dist = "poisson", data=ethnicity_mixed_or_other)
summary(ZIP_AI_Total_AWM_Hom_grid_mixed_or_other)

ZIP_AI_Total_AWM_Zip_Rac_mixed_or_other <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Zip_Rac, dist = "poisson", data=ethnicity_mixed_or_other)
summary(ZIP_AI_Total_AWM_Zip_Rac_mixed_or_other)

ZIP_AI_Total_AWM_Zip_Hom_mixed_or_other <- zeroinfl(AI_Total ~ agef1y+eduf1+income_high+income_medium+AWM_Zip_Hom, dist = "poisson", data=ethnicity_mixed_or_other)
summary(ZIP_AI_Total_AWM_Zip_Hom_mixed_or_other)


