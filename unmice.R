load(file="./data/Weight/ALL_Weight.Rdata")
ALL_Weight.omit<-ALL_Weight
save(ALL_Weight.omit,file="./data/Survey/ALL_Weight.omit.Rdata")
ALL_Weight$Periodontitis<-factor(ALL_Weight$Periodontitis,levels = c("No/Mild periodontitis","Moderate/Severe periodontitis"))
table( ALL_Weight$Periodontitis,ALL_Weight$T2D)
#T2D
ALL_Weight$T2D<-factor( ALL_Weight$T2D,levels =c("T2D","No T2D"))
table( ALL_Weight$T2D,ALL_Weight$Periodontitis,ALL_Weight$MORT_stat)

ALL_Weight$HbA1c[ALL_Weight$T2D=="No T2D"&ALL_Weight$HbA1c>=6.5]<-6.4
table( ALL_Weight$T2D)
#MORT_stat
ALL_Weight$MORT_stat[ALL_Weight$MORT_stat=="Alive"]<-0
ALL_Weight$MORT_stat[ALL_Weight$MORT_stat=="Deceased"]<-1
ALL_Weight$MORT_stat<-as.numeric(ALL_Weight$MORT_stat)
table(ALL_Weight$MORT_stat)
#MORT_CVD
ALL_Weight$ucod_leading[is.na(ALL_Weight$ucod_leading)]<-0
ALL_Weight$ucod_leading[ALL_Weight$ucod_leading!="CVD"]<-0
ALL_Weight$ucod_leading[ALL_Weight$ucod_leading=="CVD"]<-1
table(ALL_Weight$ucod_leading)
ALL_Weight$ucod_leading<-as.numeric(ALL_Weight$ucod_leading)
#MORT_diabetes
ALL_Weight$diabetes[is.na(ALL_Weight$diabetes)]<-0
ALL_Weight$diabetes[ALL_Weight$diabetes=="NO"]<-0
ALL_Weight$diabetes[ALL_Weight$diabetes=="YES"]<-1
table(ALL_Weight$diabetes)
#peryear
ALL_Weight$peryear<-ALL_Weight$permth/12
summary(ALL_Weight$peryear)
#age
ALL_Weight$Age_status[ALL_Weight$Age<45]<-"<45"
ALL_Weight$Age_status[ALL_Weight$Age<65&ALL_Weight$Age>=45]<-"[45,65)"
ALL_Weight$Age_status[ALL_Weight$Age>65]<-">=65"
ALL_Weight$Age_status<-factor(ALL_Weight$Age_status,levels = c("<45","[45,65)",">=65"))
table(ALL_Weight$Age_status)
#gender

table( ALL_Weight$Gender)

#RACE

table(ALL_Weight$Race_ethnicity)
#Education_levels

ALL_Weight$Education_levels<-factor(ALL_Weight$Education_levels,
                                    levels = c("Less than high school","High school or Equivalent","College or above"))
table( ALL_Weight$Education_levels)
#PIR
ALL_Weight$PIR[ALL_Weight$PIR_raw>=3]<-">=3"
ALL_Weight$PIR[ALL_Weight$PIR_raw>=1.3&ALL_Weight$PIR_raw<3]<-"[1.3,3)"
ALL_Weight$PIR[ALL_Weight$PIR_raw<1.3]<-"<1.3"
ALL_Weight$PIR<-factor(ALL_Weight$PIR,levels = c("<1.3","[1.3,3)",">=3"))
table(ALL_Weight$PIR)
#Health_insurance
ALL_Weight$Health_insurance<-as.numeric(ALL_Weight$Health_insurance)
ALL_Weight$Health_insurance[ALL_Weight$Health_insurance==1]<-"No insurance"
ALL_Weight$Health_insurance[ALL_Weight$Health_insurance==2]<-"Public insurance"
ALL_Weight$Health_insurance[ALL_Weight$Health_insurance==3]<-"Private insurance"
ALL_Weight$Health_insurance<-factor(ALL_Weight$Health_insurance,
                                    levels = c("No insurance","Public insurance","Private insurance"))
table( ALL_Weight$Health_insurance)
#Smoking_status
ALL_Weight$Smoking_status<-as.numeric(ALL_Weight$Smoking_status)
ALL_Weight$Smoking_status[ALL_Weight$Smoking_status==1]<-"Never smoker"
ALL_Weight$Smoking_status[ALL_Weight$Smoking_status==2]<-"Former smoker"
ALL_Weight$Smoking_status[ALL_Weight$Smoking_status==3]<-"Current smoker"
ALL_Weight$Smoking_status<-factor(ALL_Weight$Smoking_status,
                                  levels = c("Never smoker","Former smoker","Current smoker"))
table( ALL_Weight$Smoking_status)
#Drinking_status
ALL_Weight$Drinking_status<-as.numeric(ALL_Weight$Drinking_status)
ALL_Weight$Drinking_status[ALL_Weight$Drinking_status==1]<-"Nondrinker"
ALL_Weight$Drinking_status[ALL_Weight$Drinking_status==2]<-"Light/moderate drinker"
ALL_Weight$Drinking_status[ALL_Weight$Drinking_status==3]<-"Heavier drinker"
ALL_Weight$Drinking_status<-factor(ALL_Weight$Drinking_status,
                                   levels = c("Nondrinker","Light/moderate drinker","Heavier drinker"))
table( ALL_Weight$Drinking_status)
#Physical_status
ALL_Weight$Physical_status<-as.numeric(ALL_Weight$Physical_status)
ALL_Weight$Physical_status[ALL_Weight$Physical_status==1]<-"Inactive"
ALL_Weight$Physical_status[ALL_Weight$Physical_status==2]<-"Insufficient"
ALL_Weight$Physical_status[ALL_Weight$Physical_status==3]<-"Recommended"
ALL_Weight$Physical_status<-factor(ALL_Weight$Physical_status,levels=c("Inactive","Insufficient","Recommended"))
table(ALL_Weight$Physical_status)
#HEI
ALL_Weight$HEI<-as.numeric(ALL_Weight$HEI)
ALL_Weight$HEI[ALL_Weight$HEI==1]<-"Quintile 1"
ALL_Weight$HEI[ALL_Weight$HEI==2]<-"Quintile 2"
ALL_Weight$HEI[ALL_Weight$HEI==3]<-"Quintile 3"
ALL_Weight$HEI[ALL_Weight$HEI==4]<-"Quintile 4"
ALL_Weight$HEI[ALL_Weight$HEI==5]<-"Quintile 5"
ALL_Weight$HEI<-as.factor(ALL_Weight$HEI)
table(ALL_Weight$HEI)
#HTN_status
ALL_Weight$HTN_status<-as.numeric(ALL_Weight$HTN_status)
ALL_Weight$HTN_status[ALL_Weight$HTN_status==2]<-"YES"
ALL_Weight$HTN_status[ALL_Weight$HTN_status==1]<-"NO"
ALL_Weight$HTN_status<-factor(ALL_Weight$HTN_status,levels = c("YES","NO"))
table(ALL_Weight$HTN_status)
#HPL_status
ALL_Weight$HPL_status<-as.numeric(ALL_Weight$HPL_status)
ALL_Weight$HPL_status[ALL_Weight$HPL_status==1]<-"NO"
ALL_Weight$HPL_status[ALL_Weight$HPL_status==2]<-"YES"
ALL_Weight$HPL_status<-factor(ALL_Weight$HPL_status,levels = c("YES","NO"))
table(ALL_Weight$HPL_status)
#CVD
ALL_Weight$CVD<-as.numeric(ALL_Weight$CVD)
ALL_Weight$CVD[ALL_Weight$CVD==1]<-"NO"
ALL_Weight$CVD[ALL_Weight$CVD==2]<-"YES"
ALL_Weight$CVD<-factor(ALL_Weight$CVD,levels = c("YES","NO"))
table(ALL_Weight$CVD)
#CKD
ALL_Weight$CKD<-as.numeric(ALL_Weight$CKD)
ALL_Weight$CKD[ALL_Weight$CKD==1]<-"NO"
ALL_Weight$CKD[ALL_Weight$CKD==2]<-"YES"
ALL_Weight$CKD<-factor(ALL_Weight$CKD,levels = c("YES","NO"))
table(ALL_Weight$CKD)
#CANCER
ALL_Weight$Cancer<-as.numeric(ALL_Weight$Cancer)
ALL_Weight$Cancer[ALL_Weight$Cancer==1]<-"NO"
ALL_Weight$Cancer[ALL_Weight$Cancer==2]<-"YES"
ALL_Weight$Cancer<-factor(ALL_Weight$Cancer,levels = c("YES","NO"))
table(ALL_Weight$Cancer)
#Br_diabetes_status
ALL_Weight$Br_diabetes_status<-as.numeric(ALL_Weight$Br_diabetes_status)
ALL_Weight$Br_diabetes_status[ALL_Weight$Br_diabetes_status==1]<-"NO"
ALL_Weight$Br_diabetes_status[ALL_Weight$Br_diabetes_status==2]<-"YES"
ALL_Weight$Br_diabetes_status<-factor(ALL_Weight$Br_diabetes_status,levels = c("YES","NO"))
table(ALL_Weight$Br_diabetes_status)
#HbA1c>=7
ALL_Weight$HbA1c_status[ALL_Weight$HbA1c>=7]<-"HbA1c>=7"
ALL_Weight$HbA1c_status[ALL_Weight$HbA1c<7]<-"HbA1c<7"
ALL_Weight$HbA1c_status<-factor(ALL_Weight$HbA1c_status,levels = c("HbA1c>=7","HbA1c<7"))
table(ALL_Weight$HbA1c_status)
#BMI_status
ALL_Weight$BMI_status[ALL_Weight$BMI<25]<-"<25"
ALL_Weight$BMI_status[ALL_Weight$BMI<30&ALL_Weight$BMI>=25]<-"[25.0-30)"
ALL_Weight$BMI_status[ALL_Weight$BMI>=30]<-">=30"
ALL_Weight$BMI_status<-factor(ALL_Weight$BMI_status,levels = c("<25","[25.0-30)",">=30"))
table(ALL_Weight$BMI_status)

ALL_Weight$T2D<-factor(ALL_Weight$T2D,levels=c("No T2D","T2D"))
ALL_Weight$Periodontitis<-factor(ALL_Weight$Periodontitis,levels=c("No/Mild periodontitis","Moderate/Severe periodontitis"))
ALL_Weight$ucod_leading<-as.factor(ALL_Weight$ucod_leading)
ALL_Weight$inter <- ifelse(ALL_Weight$T2D == 'No T2D' & ALL_Weight$Periodontitis =='No/Mild periodontitis', 'S0H0',
                           ifelse(ALL_Weight$T2D  == 'No T2D'&ALL_Weight$Periodontitis  == 'Moderate/Severe periodontitis', 'S0H1',                      
                                  ifelse(ALL_Weight$T2D == 'T2D' & ALL_Weight$Periodontitis  == 'No/Mild periodontitis', 'S1H0', 'S1H1')))

table(ALL_Weight$inter)
library(survey)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight,strata=~sdmvstra,weights = ~ weight)

table(ALL_Weight$T2D,ALL_Weight$Gender)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~ Periodontitis+T2D+Periodontitis*T2D+Age_status+Race_ethnicity+Health_insurance+
                                 Education_levels+PIR+Smoking_status+Drinking_status+Physical_status+HEI+
                                 BMI_status+HTN_status+HPL_status+CVD+CKD+Cancer+Br_diabetes_status, design =rhcSvy)
summary(MODEL_ALL_inter)
class(MODEL_ALL_inter)<-"coxph"
library(epiR)
epi.interaction(model = MODEL_ALL_inter, coef = c(1,2,length(MODEL_ALL_inter[["coefficients"]])),param = "product",conf.level = 0.95)

3
length(MODEL_ALL_inter[["coefficients"]])




load(file="./data/covariates/CON_covariates_DM.Rdata")
load(file="./data/covariates/CON_covariates_DM_09.Rdata")
load(file="./data/covariates/III_covariates_DM.Rdata")
III_covariates<-III_covariates_DM[,c("ID","duration","medicine")]
CON_covariates<-CON_covariates_DM[,c("ID","duration","medicine")]
CON_covariates_09<-CON_covariates_DM_09[,c("ID","duration","medicine")]
covariates_DM.O<-rbind(III_covariates,CON_covariates,CON_covariates_09)
load(file="./data/Survey/ALL_Weight.Rdata")
ALL_Weight_DM<-subset(ALL_Weight,T2D=="T2D")
ALL_Weight_plus<-merge(ALL_Weight,covariates_DM.O,by = "ID",all.x = T)
ALL_Weight_plus$duration_status[ALL_Weight_plus$duration<3]<-"duration<3"
ALL_Weight_plus$duration_status[ALL_Weight_plus$duration>=3]<-"duration>=3"
ALL_Weight_plus$medicine<-factor(ALL_Weight_plus$medicine,levels = c("YES","NO"))
table(ALL_Weight_plus$duration_status)
save(ALL_Weight_plus,file="./data/Survey/ALL_Weight_plus.Rdata")
save(covariates_DM,file="./data/Survey/DM_Weight.Rdata")
ALL_Weight_PLUS<-merge(ALL_Weight,covariates_DM[,c("ID","duration_status","medicine","duration")],by = "ID",all.x = T)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_PLUS,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D"&CVD=="NO")
MODEL_ALL_inter<-svycoxph(Surv(peryear, ucod_leading==1) ~ Periodontitis+Age_status+Race_ethnicity+
                            Education_levels+PIR+Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+CKD+Cancer+Br_diabetes_status+HbA1c_status+duration_status+medicine, design =rhcSvy_DM)
summary(MODEL_ALL_inter)










rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~ Periodontitis+Age_status+Race_ethnicity+Health_insurance+
                            Education_levels+PIR+Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+CVD+CKD+Cancer+Br_diabetes_status, design =rhcSvy_DM)
summary(MODEL_ALL_inter)

MODEL_ALL_inter<-svycoxph(Surv(peryear, ucod_leading==1) ~ Periodontitis+Age_status+Race_ethnicity+Health_insurance+
                            Education_levels+PIR+Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+CVD+CKD+Cancer+Br_diabetes_status+HbA1c_status, design =rhcSvy_DM)






rhcSvy_PD<-subset(rhcSvy ,Periodontitis=="Moderate/Severe periodontitis")

#DM&gener
rhcSvy_PD<-subset(rhcSvy,Periodontitis=="Moderate/Severe periodontitis")

MODEL_ALL_inter_PDGE<-svycoxph(Surv(peryear, MORT_stat==1) ~ inter+Age_status+Race_ethnicity+Health_insurance+
                                 Education_levels+PIR+Smoking_status+Drinking_status+Physical_status+HEI+
                                 BMI_status+HTN_status+HPL_status+CVD+Cancer+Br_diabetes_status, design =rhcSvy_DM11)
summary(MODEL_ALL_inter_PDGE)
class(MODEL_ALL_inter_PDGE)<-"coxph"

epi.interaction(model = MODEL_ALL_inter, coef = c(1,2,32),param = "product",conf.level = 0.95)
MODEL_ALL_inter_PDGE<-svycoxph(Surv(peryear, MORT_stat==1) ~ Gender+Periodontitis+Age_status+Race_ethnicity+Health_insurance+
                                 Education_levels+PIR+Smoking_status+Drinking_status+Physical_status+HEI+
                                 BMI_status+HTN_status+HPL_status+CVD+Cancer+Br_diabetes_status, design =rhcSvy_DM11)
ALL_Weight_PLUS$MORT_stat<-as.factor(ALL_Weight_PLUS$MORT_stat)
ALL_Weight_PLUS$ucod_leading<-as.factor(ALL_Weight_PLUS$ucod_leading)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight,strata=~sdmvstra,weights = ~ weight)

ALL_Weight$Gender<-factor(ALL_Weight$Gender,levels=c("Female","Male"))
table(ALL_Weight_PLUS$Gender)
table(ALL_Weight$Gender)
table(ALL_Weight_PLUS$MORT_stat)
table(ALL_Weight_PLUS$T2D)
table(ALL_Weight_PLUS$Periodontitis)
rhcSvy_DM11<-subset(rhcSvy,T2D=="T2D")
rhcSvy_PD<-subset(rhcSvy ,Periodontitis=="Moderate/Severe periodontitis")
ALL_Weight$T2D<-factor(ALL_Weight$T2D,levels=c("No T2D","T2D"))
table(ALL_Weight$inter)

ALL_Weight$inter <- ifelse(ALL_Weight$Gender == 'Male' & ALL_Weight$Periodontitis =='No/Mild periodontitis', 'S0H0',
                           ifelse(ALL_Weight$Gender  == 'Male'&ALL_Weight$Periodontitis  == 'Moderate/Severe periodontitis', 'S0H1',                      
                                  ifelse(ALL_Weight$Gender == 'Female' & ALL_Weight$Periodontitis  == 'No/Mild periodontitis', 'S1H0', 'S1H1')))
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat) ~ inter+Age_status+Race_ethnicity+Health_insurance+
                            Education_levels+PIR+Smoking_status+Health_insurance+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+CVD+Cancer+Br_diabetes_status, design =rhcSvy_DM11)
summary(MODEL_ALL_inter)
class(MODEL_ALL_inter)<-"coxph"
class(MODEL_ALL_inter)
colnames(ALL_Weight)
epi.interaction(model = MODEL_ALL_inter, coef = c(1,2,3),param = "dummy",conf.level = 0.95)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~ inter+T2D+Age_status+Race_ethnicity+Health_insurance+
                            Education_levels+PIR+Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+CVD+Cancer+Br_diabetes_status, design =rhcSvy)
summary(MODEL_ALL_inter)
class(MODEL_ALL_inter)<-"coxph"

epi.interaction(model = MODEL_ALL_inter, coef = c(1,2,32),param = "product",conf.level = 0.95)







table(ALL_Weight$CVD)
colnames(ALL_Weight)
ALL_Weight1<-ALL_Weight[-which(ALL_Weight$CVD=="YES"),]
ALL_Weight2<-ALL_Weight1[-which(ALL_Weight1$Cancer=="YES"),]
load(file="./data/Survey/ALL_Weight.Rdata")
ALL_Weight2$T2D<-factor(ALL_Weight2$T2D,levels=c("No T2D","T2D"))
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight2,strata=~sdmvstra,weights = ~ weight)
library(survey)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
table(ALL_Weight$T2D)
table(ALL_Weight$Periodontitis)
table(ALL_Weight$MORT_stat)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_stat==1) ~Periodontitis+Age_status
                          +BMI_status+HTN_status+HPL_status+CKD+Br_diabetes_status+Smoking_status
                          +Race_ethnicity+Drinking_status+PIR+Health_insurance+Physical_status+HEI+Education_levels, design =rhcSvy_DM)
MODEL_ALL_inter<-svycoxph(Surv(peryear, uncood==1) ~Periodontitis+Age_status
                          +BMI_status+HTN_status+HPL_status+CKD+Br_diabetes_status+Smoking_status
                          +Race_ethnicity+Drinking_status+PIR+Health_insurance+Physical_status+HEI+Education_levels, design =rhcSvy_DM)
summary(MODEL_ALL_inter)
library(epiR)
class(MODEL_ALL_inter)<-"coxph"
epi.interaction(model = MODEL_ALL_inter, coef = c(1,2,length(MODEL_ALL_inter[["coefficients"]])),param = "product",conf.level = 0.95)

