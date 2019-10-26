require(MASS)
install.packages("MASS")

# Set the working environment here
setwd("C:/Users/eddie/Desktop/Prof. Rumi Chunara/2019_Fall/msm-discrimination-on-twitter/")

survey_data_path <- "data/"
survey_data_file <- "P18_Final_Data_07162019.csv"

survey_data <- read.csv(paste(survey_data_path,survey_data_file,sep=""),header=TRUE)
survey_data_covariates <- survey_data[c("a_pid","agef1y","racefa1_b","eduf3","eduf1","bornusf1_b","income_su")]

survey_data_outcomes <- survey_data[c("AI_Total", "AI_Condom", "AI_Condomless")]


# ---- Possion ----
# AI
glm_possion_AI_Total <- glm(AI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family="poisson",data=survey_data)
summary(glm_possion_AI_Total)

glm_possion_AI_Condom <- glm(AI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family="poisson",data=survey_data)
summary(glm_possion_AI_Condom)

glm_possion_AI_Condomless <- glm(AI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family="poisson",data=survey_data)
summary(glm_possion_AI_Condomless)

# IAI
glm_possion_IAI_Total <- glm(IAI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family="poisson",data=survey_data)
summary(glm_possion_IAI_Total)

glm_possion_IAI_Condom <- glm(IAI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family="poisson",data=survey_data)
summary(glm_possion_IAI_Condom)

glm_possion_IAI_Condomless <- glm(IAI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family="poisson",data=survey_data)
summary(glm_possion_IAI_Condomless)

# RAI
glm_possion_RAI_Total <- glm(RAI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family="poisson",data=survey_data)
summary(glm_possion_RAI_Total)

glm_possion_RAI_Condom <- glm(RAI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family="poisson",data=survey_data)
summary(glm_possion_RAI_Condom)

glm_possion_RAI_Condomless <- glm(RAI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family="poisson",data=survey_data)
summary(glm_possion_RAI_Condomless)

# Drug use
glm_possion_drug_use <- glm(drug_use ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family="poisson",data=survey_data)
summary(glm_possion_drug_use)



# ---- Negative Binomial ----
# AI
glm_nb_AI_Total <- glm.nb(AI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, data=survey_data)
summary(glm_nb_AI_Total)

glm_nb_AI_Condom <- glm.nb(AI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, data=survey_data)
summary(glm_nb_AI_Condom)

glm_nb_AI_Condomless <- glm.nb(AI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, data=survey_data)
summary(glm_nb_AI_Condomless)

# IAI
glm_nb_IAI_Total <- glm.nb(IAI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, data=survey_data)
summary(glm_nb_IAI_Total)

glm_nb_IAI_Condom <- glm.nb(IAI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, data=survey_data)
summary(glm_nb_IAI_Condom)

glm_nb_IAI_Condomless <- glm.nb(IAI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, data=survey_data)
summary(glm_nb_IAI_Condomless)

# RAI
glm_nb_RAI_Total <- glm.nb(RAI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, data=survey_data)
summary(glm_nb_RAI_Total)

glm_nb_RAI_Condom <- glm.nb(RAI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, data=survey_data)
summary(glm_nb_RAI_Condom)

glm_nb_RAI_Condomless <- glm.nb(RAI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, data=survey_data)
summary(glm_nb_RAI_Condomless)

# Drug use
glm_nb_drug_use <- glm.nb(drug_use ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, data=survey_data)
summary(glm_nb_drug_use)


# ---- Quasi Possion ----
# AI
glm_quasi_possion_AI_Total <- glm(AI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family=quasipoisson,data=survey_data)
summary(glm_quasi_possion_AI_Total)

glm_quasi_possion_AI_Condom <- glm(AI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family=quasipoisson,data=survey_data)
summary(glm_quasi_possion_AI_Condom)

glm_quasi_possion_AI_Condomless <- glm(AI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family=quasipoisson,data=survey_data)
summary(glm_quasi_possion_AI_Condomless)

# IAI
glm_quasi_possion_IAI_Total <- glm(IAI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family=quasipoisson,data=survey_data)
summary(glm_quasi_possion_IAI_Total)

glm_quasi_possion_IAI_Condom <- glm(IAI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family=quasipoisson,data=survey_data)
summary(glm_quasi_possion_IAI_Condom)

glm_quasi_possion_IAI_Condomless <- glm(IAI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family=quasipoisson,data=survey_data)
summary(glm_quasi_possion_IAI_Condomless)

# RAI
glm_quasi_possion_RAI_Total <- glm(RAI_Total ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family=quasipoisson,data=survey_data)
summary(glm_quasi_possion_RAI_Total)

glm_quasi_possion_RAI_Condom <- glm(RAI_Condom ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family=quasipoisson,data=survey_data)
summary(glm_quasi_possion_RAI_Condom)

glm_quasi_possion_RAI_Condomless <- glm(RAI_Condomless ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family=quasipoisson,data=survey_data)
summary(glm_quasi_possion_RAI_Condomless)

# Drug use
glm_quasi_possion_drug_use <- glm(drug_use ~ agef1y+racefa1_b+eduf3+eduf1+bornusf1_b+income_su, family=quasipoisson,data=survey_data)
summary(glm_quasi_possion_drug_use)







