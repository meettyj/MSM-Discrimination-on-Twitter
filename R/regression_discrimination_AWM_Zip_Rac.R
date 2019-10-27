require(MASS)
require(xlsx)
# install.packages("MASS")
# install.packages("xlsx")

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

# ---- Poisson ----
# AI
glm_poisson_AI_Total <- glm(AI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family="poisson",data=merged_survey_discrimination_data)
summary(glm_poisson_AI_Total)

glm_poisson_AI_Condom <- glm(AI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family="poisson",data=merged_survey_discrimination_data)
summary(glm_poisson_AI_Condom)

glm_poisson_AI_Condomless <- glm(AI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family="poisson",data=merged_survey_discrimination_data)
summary(glm_poisson_AI_Condomless)

# IAI
glm_poisson_IAI_Total <- glm(IAI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family="poisson",data=merged_survey_discrimination_data)
summary(glm_poisson_IAI_Total)

glm_poisson_IAI_Condom <- glm(IAI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family="poisson",data=merged_survey_discrimination_data)
summary(glm_poisson_IAI_Condom)

glm_poisson_IAI_Condomless <- glm(IAI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family="poisson",data=merged_survey_discrimination_data)
summary(glm_poisson_IAI_Condomless)

# RAI
glm_poisson_RAI_Total <- glm(RAI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family="poisson",data=merged_survey_discrimination_data)
summary(glm_poisson_RAI_Total)

glm_poisson_RAI_Condom <- glm(RAI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family="poisson",data=merged_survey_discrimination_data)
summary(glm_poisson_RAI_Condom)

glm_poisson_RAI_Condomless <- glm(RAI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family="poisson",data=merged_survey_discrimination_data)
summary(glm_poisson_RAI_Condomless)

# Drug use
glm_poisson_drug_use <- glm(drug_use ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family="poisson",data=merged_survey_discrimination_data)
summary(glm_poisson_drug_use)



# ---- Negative Binomial ----
# AI
glm_nb_AI_Total <- glm.nb(AI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, data=merged_survey_discrimination_data)
summary(glm_nb_AI_Total)

glm_nb_AI_Condom <- glm.nb(AI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, data=merged_survey_discrimination_data)
summary(glm_nb_AI_Condom)

glm_nb_AI_Condomless <- glm.nb(AI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, data=merged_survey_discrimination_data)
summary(glm_nb_AI_Condomless)

# IAI
glm_nb_IAI_Total <- glm.nb(IAI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, data=merged_survey_discrimination_data)
summary(glm_nb_IAI_Total)

glm_nb_IAI_Condom <- glm.nb(IAI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, data=merged_survey_discrimination_data)
summary(glm_nb_IAI_Condom)

glm_nb_IAI_Condomless <- glm.nb(IAI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, data=merged_survey_discrimination_data)
summary(glm_nb_IAI_Condomless)

# RAI
glm_nb_RAI_Total <- glm.nb(RAI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, data=merged_survey_discrimination_data)
summary(glm_nb_RAI_Total)

glm_nb_RAI_Condom <- glm.nb(RAI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, data=merged_survey_discrimination_data)
summary(glm_nb_RAI_Condom)

glm_nb_RAI_Condomless <- glm.nb(RAI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, data=merged_survey_discrimination_data)
summary(glm_nb_RAI_Condomless)

# Drug use
glm_nb_drug_use <- glm.nb(drug_use ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, data=merged_survey_discrimination_data)
summary(glm_nb_drug_use)


# ---- Quasi Poisson ----
# AI
glm_quasi_poisson_AI_Total <- glm(AI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family=quasipoisson,data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_AI_Total)

glm_quasi_poisson_AI_Condom <- glm(AI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family=quasipoisson,data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_AI_Condom)

glm_quasi_poisson_AI_Condomless <- glm(AI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family=quasipoisson,data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_AI_Condomless)

# IAI
glm_quasi_poisson_IAI_Total <- glm(IAI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family=quasipoisson,data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_IAI_Total)

glm_quasi_poisson_IAI_Condom <- glm(IAI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family=quasipoisson,data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_IAI_Condom)

glm_quasi_poisson_IAI_Condomless <- glm(IAI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family=quasipoisson,data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_IAI_Condomless)

# RAI
glm_quasi_poisson_RAI_Total <- glm(RAI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family=quasipoisson,data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_RAI_Total)

glm_quasi_poisson_RAI_Condom <- glm(RAI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family=quasipoisson,data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_RAI_Condom)

glm_quasi_poisson_RAI_Condomless <- glm(RAI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family=quasipoisson,data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_RAI_Condomless)

# Drug use
glm_quasi_poisson_drug_use <- glm(drug_use ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su+AWM_Zip_Hom, family=quasipoisson,data=merged_survey_discrimination_data)
summary(glm_quasi_poisson_drug_use)









