#diabetes========================================================================
lab=read.table("./lab.dat",sep=",",fill=T)
#HbA1c(%) > 6.5
lab$SEQN<-as.numeric(substring(lab$V1,1,5))
DM_data<-as.data.frame(lab$SEQN)
colnames(DM_data)<-c("SEQN")
DM_data$Diabete<-as.numeric(substring(lab$V1,1861,1864))
DM_data$Diabete[DM_data$Diabete==8888]<-NA
DM_data$HbA1c[DM_data$Diabete>=6.5]<-"T2D"
DM_data$HbA1c[DM_data$Diabete<6.5]<-"No T2D"
table(DM_data$HbA1c)

#Ate_time
DM_data$Ate_time<-as.numeric(substring(lab$V1,1255,1255))
#1263-1267
DM_data$Ate_duration<-as.numeric(substring(lab$V1,1263,1267))
#TIME
DM_data$time[DM_data$Ate_time==1|DM_data$Ate_time==3|DM_data$Ate_duration>=8]<-"Satisfied"
DM_data$time[is.na(DM_data$time)] <- "Unsatisfied"
#morning weight
DM_data$Plasma_glu_morning<-as.numeric(substring(lab$V1,113,121))
#Plasma_glu_1(mmol/L)
DM_data$Plasma_glu_1<-as.numeric(substring(lab$V1,1871,1876))
DM_data$Plasma_glu_1[DM_data$Plasma_glu_1==888888]<-NA
#Plasma_glu_2(mmol/L)
DM_data$Plasma_glu_2<-as.numeric(substring(lab$V1, 1890,1895))
DM_data$Plasma_glu_2[DM_data$Plasma_glu_2==888888]<-NA
#time duration
# DM_data$Plasma_glu_duration<-as.numeric(substring(lab$V1, 1882,1884))
#  DM_data$Plasma_glu_duration[DM_data$Plasma_glu_duration==888]<-NA
#fasting glucose (mmol/l) >= 7.0
DM_data$fast_glu[(DM_data$time=="Satisfied")&(DM_data$Plasma_glu_1>=7.0)]<-"T2D"
DM_data$fast_glu[(DM_data$time=="Satisfied")&(DM_data$Plasma_glu_1<7.0)]<-"No T2D"
#random blood glucose (mmol/l) >= 11.1
DM_data$rand_glu[(DM_data$time=="Unsatisfied")&(DM_data$Plasma_glu_1>=11.1)]<-"T2D"
DM_data$rand_glu[(DM_data$time=="Unsatisfied")&(DM_data$Plasma_glu_1<11.1)]<-"No T2D"
#two-hour OGTT blood glucose (mmol/l) >= 11.1
DM_data$OGTT2[(DM_data$time=="Satisfied")&(DM_data$Plasma_glu_morning>0)
             &(DM_data$Plasma_glu_2>=11.1)]<-"T2D"
DM_data$OGTT2[(DM_data$time=="Satisfied")&(DM_data$Plasma_glu_morning>0)
             &(DM_data$Plasma_glu_2<11.1)]<-"No T2D"


adult=read.table("./adult.dat",sep=",",fill=T)
adult$SEQN<-as.numeric(substring(adult$V1,1,5))
DM_adult<-as.data.frame(adult$SEQN)
colnames(DM_adult)<-c("SEQN")
DM_adult$told<-as.numeric(substring(adult$V1,1561,1561))
DM_adult$told[DM_adult$told==1]<-"T2D"
DM_adult$told[DM_adult$told==2]<-"No T2D"
DM_adult$told[DM_adult$told==8]<-NA
DM_adult$told[DM_adult$told==9]<-NA
#22->Diabetic on insulin
DM_data$Plasma_glu_reason<-as.numeric(substring(lab$V1,1249,1249))
DM_data$Plasma_glu_reason[DM_data$Plasma_glu_reason!=1]<-NA
DM_data$Plasma_glu_reason[DM_data$Plasma_glu_reason==1]<-"T2D"
#insulin
DM_adult$insulin<-as.numeric(substring(adult$V1,1568,1568))
DM_adult$insulin[DM_adult$insulin==1]<-"T2D"
DM_adult$insulin[DM_adult$insulin==2]<-NA
DM_adult$insulin[DM_adult$insulin==8]<-NA
#medication
DM_adult$medication<-as.numeric(substring(adult$V1,1578,1578))
DM_adult$medication[DM_adult$medication==1]<-"T2D"
DM_adult$medication[DM_adult$medication==2]<-NA
DM_adult$medication[DM_adult$medication==8]<-NA
DM_adult$medication[DM_adult$medication==9]<-NA
DM_dat<-merge(DM_data,DM_adult,by = "SEQN",all=T)
DM_dat$drug[DM_dat$Plasma_glu_reason=="T2D"|DM_dat$insulin=="T2D"|
              DM_dat$medication=="T2D"]<-"T2D"
DM_dat$T2D<-NA
colnames(DM_dat)
DM_dat$T2D[DM_dat$told=="No T2D"|DM_dat$HbA1c=="No T2D"|DM_dat$fast_glu=="No T2D"|
             DM_dat$OGTT2=="No T2D"|DM_dat$rand_glu=="No T2D"]<-"No T2D"
DM_dat$T2D[DM_dat$told=="T2D"|DM_dat$HbA1c=="T2D"|DM_dat$fast_glu=="T2D"|
             DM_dat$OGTT2=="T2D"|DM_dat$rand_glu=="T2D"|DM_dat$drug=="T2D"]<-"T2D"
DM_III<-DM_dat[,c("SEQN","T2D")]
table(DM_III$T2D)

#Mortality
load(file="./MORT_CON.Rdata")
MORT_III=read.table("G:/NHANES/mort/nhanes_iii_mort_2019_public.tsv",header = T,sep = "\t",fill = F)
library(dplyr)
MORT_III <- MORT_III %>%
  mutate(permth = ifelse(is.na(permth_exm),permth_int, permth_exm))
MORT_III<-MORT_III[,c("seqn","mortstat","ucod_leading","diabetes","permth")]
table(MORT_III$mortstat, useNA="ifany")
# 0 = Assumed alive
# 1 = Assumed deceased
MORT_III$mortstat[MORT_III$mortstat==0]<-"Assumed alive"
MORT_III$mortstat[MORT_III$mortstat==1]<-"Assumed deceased"
# 1 = Diseases of heart (I00-I09, I11, I13, I20-I51)
# 2 = Malignant neoplasms (C00-C97)
# 3 = Chronic lower respiratory diseases (J40-J47)
# 4 = Accidents (unintentional injuries) (V01-X59, Y85-Y86)
# 5 = Cerebrovascular diseases (I60-I69)
# 6 = Alzheimer's disease (G30)
# 7 = Diabetes mellitus (E10-E14)
# 8 = Influenza and pneumonia (J09-J18)
# 9 = Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)
# 10 = All other causes (residual)
MORT_III$ucod_leading[MORT_III$ucod_leading==1]<-"Diseases of heart (I00-I09, I11, I13, I20-I51)"
MORT_III$ucod_leading[MORT_III$ucod_leading==2]<-"Malignant neoplasms (C00-C97)"
MORT_III$ucod_leading[MORT_III$ucod_leading==3]<-"Chronic lower respiratory diseases (J40-J47)"
MORT_III$ucod_leading[MORT_III$ucod_leading==4]<-"Accidents (unintentional injuries) (V01-X59, Y85-Y86)"
MORT_III$ucod_leading[MORT_III$ucod_leading==5]<-"Cerebrovascular diseases (I60-I69)"
MORT_III$ucod_leading[MORT_III$ucod_leading==6]<-"Alzheimer's disease (G30)"
MORT_III$ucod_leading[MORT_III$ucod_leading==7]<-"Diabetes mellitus (E10-E14)"
MORT_III$ucod_leading[MORT_III$ucod_leading==8]<-"Influenza and pneumonia (J09-J18)"
MORT_III$ucod_leading[MORT_III$ucod_leading==9]<-"Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)"
MORT_III$ucod_leading[MORT_III$ucod_leading==10]<-"All other causes (residual)"
table(MORT_III$ucod_leading, useNA="ifany")
table(MORT_III$diabetes, useNA="ifany")
# 0 = No - Condition not listed as a multiple cause of death
# 1 = Yes - Condition listed as a multiple cause of death
MORT_III$diabetes[MORT_III$diabetes==0]<-"No - Condition not listed as a multiple cause of death"
MORT_III$diabetes[MORT_III$diabetes==1]<-"Yes - Condition listed as a multiple cause of death"
table(MORT_III$diabetes, useNA="ifany")
colnames(MORT_III)<-c("SEQN","MORT_stat","ucod_leading","diabetes","permth")
rownames(MORT_III)<-MORT_III$SEQN




#Exclusion Data
adult=read.table("./adult.dat",sep=",",fill=T)
adult$SEQN<-as.numeric(substring(adult$V1,1,5))
Ex_adult<-as.data.frame(adult$SEQN)
exam=read.table("./exam.dat",sep=",",fill=T)
exam$SEQN<-as.numeric(substring(exam$V1,1,5))
Ex_exam<-as.data.frame(exam$SEQN)
# skin Cancer
Ex_adult$skin_ca<-as.numeric(substring(adult$V1,1478,1478))
Ex_adult$skin_ca[Ex_adult$skin_ca==1]<-"YES"
Ex_adult$skin_ca[Ex_adult$skin_ca==2]<-"NO"
Ex_adult$skin_ca[Ex_adult$skin_ca==8]<-NA
Ex_adult$skin_ca[Ex_adult$skin_ca==9]<-NA
# other_Cancer
Ex_adult$other_ca<-as.numeric(substring(adult$V1,1479,1479))
Ex_adult$other_ca[Ex_adult$other_ca==1]<-"YES"
Ex_adult$other_ca[Ex_adult$other_ca==2]<-"NO"
Ex_adult$other_ca[Ex_adult$other_ca==8]<-NA
Ex_adult$other_ca[Ex_adult$other_ca==9]<-NA
Ex_adult$Cancer[Ex_adult$skin_ca=="YES"|Ex_adult$other_ca=="YES"]<-"YES"
Ex_adult$Cancer[Ex_adult$skin_ca=="NO"&Ex_adult$other_ca=="NO"]<-"NO"
table(Ex_adult$Cancer)
#CVD
#Heart disease
Ex_adult$Heart_disease<-as.numeric(substring(adult$V1,1972,1972))
Ex_adult$Heart_disease[Ex_adult$Heart_disease==1]<-"YES"
Ex_adult$Heart_disease[Ex_adult$Heart_disease==2]<-"NO"
Ex_adult$Heart_disease[Ex_adult$Heart_disease==8]<-NA
Ex_adult$Heart_disease[Ex_adult$Heart_disease==9]<-NA
#congestive heart failure
Ex_adult$heart_failure<-as.numeric(substring(adult$V1,1467,1467))
Ex_adult$heart_failure[Ex_adult$heart_failure==1]<-"YES"
Ex_adult$heart_failure[Ex_adult$heart_failure==2]<-"NO"
Ex_adult$heart_failure[Ex_adult$heart_failure==8]<-NA
Ex_adult$heart_failure[Ex_adult$heart_failure==9]<-NA
#heart attack
Ex_adult$heart_attack<-as.numeric(substring(adult$V1,1648,1648))
Ex_adult$heart_attack[Ex_adult$heart_attack==1]<-"YES"
Ex_adult$heart_attack[Ex_adult$heart_attack==2]<-"NO"
Ex_adult$heart_attack[Ex_adult$heart_attack==8]<-NA
Ex_adult$heart_attack[Ex_adult$heart_attack==9]<-NA
# stroke
Ex_adult$stroke<-as.numeric(substring(adult$V1,1468,1468))
Ex_adult$stroke[Ex_adult$stroke==1]<-"YES"
Ex_adult$stroke[Ex_adult$stroke==2]<-"NO"
Ex_adult$stroke[Ex_adult$stroke==8]<-NA
Ex_adult$stroke[Ex_adult$stroke==9]<-NA
Ex_adult$CVD[Ex_adult$Heart_disease=="NO"|Ex_adult$heart_failure=="NO"|
               Ex_adult$heart_attack=="NO"|Ex_adult$stroke=="NO"]<-"NO"
Ex_adult$CVD[Ex_adult$Heart_disease=="YES"|Ex_adult$heart_failure=="YES"|
               Ex_adult$heart_attack=="YES"|Ex_adult$stroke=="YES"]<-"YES"
colnames(Ex_adult)[1]<-"SEQN"
#Pregnancy
exam=read.table("./exam.dat",sep=",",fill=T)
exam$SEQN<-as.numeric(substring(exam$V1,1,5))
Ex_exam<-as.data.frame(exam$SEQN)
Ex_exam$Pregnancy<-as.numeric(substring(exam$V1,5151,5151))
Ex_exam$Pregnancy[Ex_exam$Pregnancy==1]<-"YES"
Ex_exam$Pregnancy[Ex_exam$Pregnancy==2]<-"NO"
Ex_exam$Pregnancy[Ex_exam$Pregnancy==8]<-NA
Ex_exam$Pregnancy[Ex_exam$Pregnancy==9]<-NA
colnames(Ex_exam)[1]<-"SEQN"
EX_III<-merge(Ex_adult[,c("SEQN","CVD","Cancer")],Ex_exam[,c("SEQN","Pregnancy")],by = "SEQN",all=T)
record<-ls()
rm(list=record[-which(record=='EX_III'|record=='EX_CON')])


       