require(MASS)
require(xlsx)
require(pscl)
# install.packages("MASS")
# install.packages("xlsx")
# install.packages("pscl")

# Set the working environment here
setwd("C:/Users/eddie/Desktop/Prof. Rumi Chunara/2019_Fall/msm-discrimination-on-twitter/")

survey_data_path <- "data/"
survey_data_file <- "P18_Final_Data_07162019.csv"
discrimination_data_path <- "data/discrimination/"
discrimination_data_file <- "P18_GPS_AWM_Twitter_data_summarystats.xlsx"

survey_data <- read.csv(paste(survey_data_path,survey_data_file,sep=""),header=TRUE)
# survey_data_covariates <- survey_data[c("a_pid","agef1y","racefa1_b","eduf3","eduf1","bornusf1_b","income_su")]
# survey_data_outcomes <- survey_data[c("AI_Total", "AI_Condom", "AI_Condomless")]

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
# merged_survey_discrimination_data$race_other <- merged_survey_discrimination_data$racefa1_b
# merged_survey_discrimination_data$race_white <- merged_survey_discrimination_data$racefa1_b

merged_survey_discrimination_data$race_hispanic[merged_survey_discrimination_data$race_hispanic != 1]<-0

merged_survey_discrimination_data$race_black[merged_survey_discrimination_data$race_black != 2]<-0
merged_survey_discrimination_data$race_black[merged_survey_discrimination_data$race_black == 2]<-1

merged_survey_discrimination_data$race_asian[merged_survey_discrimination_data$race_asian != 3]<-0
merged_survey_discrimination_data$race_asian[merged_survey_discrimination_data$race_asian == 3]<-1

merged_survey_discrimination_data$race_mixed_or_other[merged_survey_discrimination_data$race_mixed_or_other != 4 & merged_survey_discrimination_data$race_mixed_or_other != 5]<-0
merged_survey_discrimination_data$race_mixed_or_other[merged_survey_discrimination_data$race_mixed_or_other == 4|merged_survey_discrimination_data$race_mixed_or_other == 5]<-1

# merged_survey_discrimination_data$race_other[merged_survey_discrimination_data$race_other != 5]<-0
# merged_survey_discrimination_data$race_other[merged_survey_discrimination_data$race_other == 5]<-1
# merged_survey_discrimination_data$race_white[merged_survey_discrimination_data$race_white != 6]<-0
# merged_survey_discrimination_data$race_white[merged_survey_discrimination_data$race_white == 6]<-1

# merged_survey_discrimination_data$agef1y
# merged_survey_discrimination_data$eduf1
# merged_survey_discrimination_data$race_hispanic
# merged_survey_discrimination_data$race_black
# merged_survey_discrimination_data$race_asian
# merged_survey_discrimination_data$race_mixed_or_other
# merged_survey_discrimination_data$race_other
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


# remove NA value in alcohf7
merged_survey_discrimination_data$alcohf7
merged_survey_discrimination_data <- merged_survey_discrimination_data[which(merged_survey_discrimination_data$alcohf7 !="NA"), ]
merged_survey_discrimination_data$alcohf7

# remove NA value in AWM_Rac_grid
merged_survey_discrimination_data$AWM_Rac_grid
merged_survey_discrimination_data <- merged_survey_discrimination_data[which(merged_survey_discrimination_data$AWM_Rac_grid !="NA"), ]
merged_survey_discrimination_data$AWM_Rac_grid


# merged_survey_discrimination_data$AWM_SSSOM_Hom
# merged_survey_discrimination_data$race_asian


# ---- zero inflated poisson ----
zeroinfl_ZIP_alcohf7 <- zeroinfl(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium, dist = "poisson", data=merged_survey_discrimination_data)
summary(zeroinfl_ZIP_alcohf7)

# discrimination variables
zeroinfl_ZIP_alcohf7_AWM_Rac_grid <- zeroinfl(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_Rac_grid, dist = "poisson", data=merged_survey_discrimination_data)
summary(zeroinfl_ZIP_alcohf7_AWM_Rac_grid)

zeroinfl_ZIP_alcohf7_AWM_SSSOM_Hom <- zeroinfl(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_SSSOM_Hom, dist = "poisson", data=merged_survey_discrimination_data)
summary(zeroinfl_ZIP_alcohf7_AWM_SSSOM_Hom)

zeroinfl_ZIP_alcohf7_AWM_SSSOM_Rac <- zeroinfl(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_SSSOM_Rac, dist = "poisson", data=merged_survey_discrimination_data)
summary(zeroinfl_ZIP_alcohf7_AWM_SSSOM_Rac)

zeroinfl_ZIP_alcohf7_AWM_Zip_Hom <- zeroinfl(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_Zip_Hom, dist = "poisson", data=merged_survey_discrimination_data)
summary(zeroinfl_ZIP_alcohf7_AWM_Zip_Hom)

zeroinfl_ZIP_alcohf7_AWM_Zip_Rac <- zeroinfl(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_Zip_Rac, dist = "poisson", data=merged_survey_discrimination_data)
summary(zeroinfl_ZIP_alcohf7_AWM_Zip_Rac)



# ---- zero inflated negative binomial ----
zeroinfl_ZINB_alcohf7 <- zeroinfl(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium, dist = "negbin", data=merged_survey_discrimination_data)
summary(zeroinfl_ZINB_alcohf7)

# discrimination variables
zeroinfl_ZINB_alcohf7_AWM_Rac_grid <- zeroinfl(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_Rac_grid, dist = "negbin",data=merged_survey_discrimination_data)
summary(zeroinfl_ZINB_alcohf7_AWM_Rac_grid)

zeroinfl_ZINB_alcohf7_AWM_SSSOM_Hom <- zeroinfl(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_SSSOM_Hom, dist = "negbin",data=merged_survey_discrimination_data)
summary(zeroinfl_ZINB_alcohf7_AWM_SSSOM_Hom)

zeroinfl_ZINB_alcohf7_AWM_SSSOM_Rac <- zeroinfl(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_SSSOM_Rac, dist = "negbin",data=merged_survey_discrimination_data)
summary(zeroinfl_ZINB_alcohf7_AWM_SSSOM_Rac)

zeroinfl_ZINB_alcohf7_AWM_Zip_Hom <- zeroinfl(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_Zip_Hom, dist = "negbin",data=merged_survey_discrimination_data)
summary(zeroinfl_ZINB_alcohf7_AWM_Zip_Hom)

zeroinfl_ZINB_alcohf7_AWM_Zip_Rac <- zeroinfl(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_Zip_Rac, dist = "negbin",data=merged_survey_discrimination_data)
summary(zeroinfl_ZINB_alcohf7_AWM_Zip_Rac)




# ---- Negative Binomial ----
glm_nb_alcohf7 <- glm.nb(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium, data=merged_survey_discrimination_data)
summary(glm_nb_alcohf7)

# discrimination variables
glm_nb_alcohf7_AWM_Rac_grid <- glm.nb(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_Rac_grid, data=merged_survey_discrimination_data)
summary(glm_nb_alcohf7_AWM_Rac_grid)

glm_nb_alcohf7_AWM_SSSOM_Hom <- glm.nb(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_SSSOM_Hom, data=merged_survey_discrimination_data)
summary(glm_nb_alcohf7_AWM_SSSOM_Hom)

glm_nb_alcohf7_AWM_SSSOM_Rac <- glm.nb(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_SSSOM_Rac, data=merged_survey_discrimination_data)
summary(glm_nb_alcohf7_AWM_SSSOM_Rac)

glm_nb_alcohf7_AWM_Zip_Hom <- glm.nb(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_Zip_Hom, data=merged_survey_discrimination_data)
summary(glm_nb_alcohf7_AWM_Zip_Hom)

glm_nb_alcohf7_AWM_Zip_Rac <- glm.nb(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_Zip_Rac, data=merged_survey_discrimination_data)
summary(glm_nb_alcohf7_AWM_Zip_Rac)



# ---- Quasi Poisson Regression ----
glm_quasi_poisson_alcohf7 <- glm(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium, family=quasipoisson(link="log"),data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_alcohf7)

# discrimination variables
glm_quasi_poisson_alcohf7_AWM_Rac_grid <- glm(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_Rac_grid, family=quasipoisson(link="log"),data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_alcohf7_AWM_Rac_grid)

glm_quasi_poisson_alcohf7_AWM_SSSOM_Hom <- glm(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_SSSOM_Hom, family=quasipoisson(link="log"),data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_alcohf7_AWM_SSSOM_Hom)

glm_quasi_poisson_alcohf7_AWM_SSSOM_Rac <- glm(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_SSSOM_Rac, family=quasipoisson(link="log"),data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_alcohf7_AWM_SSSOM_Rac)

glm_quasi_poisson_alcohf7_AWM_Zip_Hom <- glm(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_Zip_Hom, family=quasipoisson(link="log"),data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_alcohf7_AWM_Zip_Hom)

glm_quasi_poisson_alcohf7_AWM_Zip_Rac <- glm(alcohf7 ~ agef1y+eduf1+race_hispanic+race_black+race_asian+race_mixed_or_other+income_high+income_medium+AWM_Zip_Rac, family=quasipoisson(link="log"),data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_alcohf7_AWM_Zip_Rac)




