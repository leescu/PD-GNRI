load(file="./Survey/ALL_Weight.Rdata")
ALL_Weight_plus<-ALL_Weight
#data arangement
colnames(ALL_Weight_plus)
ALL_Weight_plus$peryear<-ALL_Weight_plus
load(file="./Weight/ALL_Weight.Rdata")
ALL_Weight_plus<-merge(ALL_Weight_plus,ALL_Weight[,c("ID","ucod_leading")],by = "ID",all= T)
colnames(ALL_Weight_plus)<-c("ID","Periodontitis","T2D","MORT_status","CVD_MORT_status","diabetes","permth","Year","Age","Gender",
                             "Race_ethnicity","Education_levels","PIR","Health_insurance","Smoking_status",
                             "Drinking_status","Physical_status","HEI","BMI","HTN_status",
                             "HPL_status","CVD_status","CKD_status","Cancer_status","HbA1c",
                             "family_history","diabetes_duration","medicine_use","sdmvpsu","sdmvstra",
                             "weight","Age_status","peryear","PIR2","HbA1c_status",
                             "BMI_status","Mortality_causes","Cancer_MORT_status")
table(ALL_Weight_plus$Mortality_causes)
ALL_Weight_plus$Mortality_causes<-as.factor(ALL_Weight_plus$Mortality_causes)
table(ALL_Weight_plus$MORT_status)
ALL_Weight_plus$CVD_MORT_status[ALL_Weight_plus$MORT_status==1]<-2
ALL_Weight_plus$CVD_MORT_status[ALL_Weight_plus$MORT_status==1&ALL_Weight_plus$Mortality_causes=="CVD"]<-1
ALL_Weight_plus$CVD_MORT_status[ALL_Weight_plus$MORT_status==0]<-0
table(ALL_Weight_plus$CVD_MORT_status,useNA = "ifany")
ALL_Weight_plus$Year<-as.factor(ALL_Weight_plus$Year)
colnames(ALL_Weight_plus)
ALL_Weight_plus$MORT_status<-as.factor(ALL_Weight_plus$MORT_status)
ALL_Weight_plus$duration_status[ALL_Weight_plus$diabetes_duration>=3]<-"duration>=3"
ALL_Weight_plus$duration_status[ALL_Weight_plus$diabetes_duration<3]<-"duration<3"
table(ALL_Weight_plus$duration_status)
ALL_Weight_plus$duration_status<-as.factor(ALL_Weight_plus$duration_status)
ALL_Weight_plus$Cancer_MORT_status[ALL_Weight_plus$MORT_status==1]<-2
ALL_Weight_plus$Cancer_MORT_status[ALL_Weight_plus$MORT_status==1&ALL_Weight_plus$Mortality_causes=="Cancer"]<-1
ALL_Weight_plus$Cancer_MORT_status[ALL_Weight_plus$MORT_status==0]<-0
table(ALL_Weight_plus$Cancer_MORT_status,useNA = "ifany")
table(ALL_Weight_plus$Age_status,useNA = "ifany")
ALL_Weight_plus$Age_status[ALL_Weight_plus$Age<45]<-"<45"
ALL_Weight_plus$Age_status[ALL_Weight_plus$Age<65&ALL_Weight_plus$Age>=45]<-"[45,65)"
ALL_Weight_plus$Age_status[ALL_Weight_plus$Age>=65]<-">=65"
ALL_Weight_plus$DM_MORT_status<-2
ALL_Weight_plus$DM_MORT_status[ALL_Weight_plus$MORT_status==1&ALL_Weight_plus$Mortality_causes=="DM"]<-1
ALL_Weight_plus$DM_MORT_status[ALL_Weight_plus$MORT_status==0]<-0
table(ALL_Weight_plus$DM_MORT_status,useNA = "ifany")
table(ALL_Weight_plus$Mortality_causes)
ALL_Weight_plus$duration_status<-as.factor(ALL_Weight_plus$duration_status)
ALL_Weight_plus$PIR<-as.factor(ALL_Weight_plus$PIR)
summary(ALL_Weight_plus)
ALL_Weight_plus<-ALL_Weight_plus[,c("ID","Periodontitis","T2D","MORT_status","CVD_MORT_status","Cancer_MORT_status","DM_MORT_status","peryear",
                                    "Year","Age","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
                                    "Health_insurance","Smoking_status","Drinking_status","Physical_status","HEI",
                                    "BMI","BMI_status","HTN_status","HPL_status","CVD_status","CKD_status","Cancer_status","family_history",
                                    "HbA1c","HbA1c_status","diabetes_duration","duration_status","medicine_use",
                                    "sdmvpsu", "sdmvstra" ,"weight")]
save(ALL_Weight_plus,file="./Survey/ALL_Weight_plus_CA.Rdata")
#=data calculate==========================================================
library(plyr)
load(file="./Survey/ALL_Weight_plus_CA.Rdata")
DMdata<-subset(ALL_Weight_plus,T2D=="T2D")
table(DMdata$Periodontitis)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
rhcSvy_DM_CVD<-subset(rhcSvy,T2D=="T2D"&CVD_status=="NO")

#Categorical_variables
var<-c("Age_status","Gender","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","Smoking_status","Drinking_status","Physical_status","HEI",
       "BMI_status","HTN_status","HPL_status","CVD_status","CKD_status","Cancer_status","family_history",
       "HbA1c_status","duration_status","medicine_use")
model<- function(x){
  
  if( x %in% var ) {
    Covariates<-as.formula(paste0("~",x))
    unwtd_count<-svyby(Covariates,Covariates,rhcSvy_DM,unwtd.count) 
    svymean<-as.data.frame(svymean(Covariates,rhcSvy_DM, na.rm = TRUE))
    model <- data.frame('Covariates'=x,
                        'grade' = gsub(x,"",rownames(svymean)),
                        'counts'=unwtd_count[2],
                        'Mean' = round(svymean$mean*100,2),
                        'SE' = round(svymean$SE*100,2) )
    return(model)
  } else {
    
    Covariates<-as.formula(paste0("~",x))
    svymean<-as.data.frame(svymean(Covariates,rhcSvy_DM, na.rm = TRUE))
    colnames(svymean)[2]<-"SE"
    model <- data.frame('Covariates'=x,
                        'grade' ="Mean ± SE",
                        'counts'=' ',
                        'Mean' =round(svymean$mean,2),
                        'SE' = round(svymean$SE,2))
    return(model)
  }
}  

VAR_ALL<-c("Age","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
           "Health_insurance","Smoking_status","Drinking_status","Physical_status","HEI",
           "BMI","BMI_status","HTN_status","HPL_status","CVD_status","CKD_status","Cancer_status","family_history",
           "HbA1c","HbA1c_status","diabetes_duration","duration_status","medicine_use")
all_data<- ldply(lapply(VAR_ALL, model))
#=DM_PD===========================
rhcSvy_DM_noPD<-subset(rhcSvy,T2D=="T2D"&Periodontitis=="No/Mild periodontitis")
model<- function(x){
  
  if( x %in% var ) {
    Covariates<-as.formula(paste0("~",x))
    unwtd_count<-svyby(Covariates,Covariates,rhcSvy_DM_noPD,unwtd.count) 
    svymean<-as.data.frame(svymean(Covariates,rhcSvy_DM_noPD, na.rm = TRUE))
    model <- data.frame('Covariates'=x,
                        'grade' = gsub(x,"",rownames(svymean)),
                        'counts'=unwtd_count[2],
                        'Mean' = round(svymean$mean*100,2),
                        'SE' = round(svymean$SE*100,2) )
    return(model)
  } else {
    
    Covariates<-as.formula(paste0("~",x))
    svymean<-as.data.frame(svymean(Covariates,rhcSvy_DM_noPD, na.rm = TRUE))
    colnames(svymean)[2]<-"SE"
    model <- data.frame('Covariates'=x,
                        'grade' ="Mean ± SE",
                        'counts'=' ',
                        'Mean' =round(svymean$mean,2),
                        'SE' = round(svymean$SE,2))
    return(model)
  }
}  
noPD_data<- ldply(lapply(VAR_ALL, model))

rhcSvy_DM_PD<-subset(rhcSvy,T2D=="T2D"&Periodontitis=="Moderate/Severe periodontitis")
model<- function(x){
  
  if( x %in% var ) {
    Covariates<-as.formula(paste0("~",x))
    unwtd_count<-svyby(Covariates,Covariates,rhcSvy_DM_PD,unwtd.count) 
    svymean<-as.data.frame(svymean(Covariates,rhcSvy_DM_PD, na.rm = TRUE))
    model <- data.frame('Covariates'=x,
                        'grade' = gsub(x,"",rownames(svymean)),
                        'counts'=unwtd_count[2],
                        'Mean' = round(svymean$mean*100,2),
                        'SE' = round(svymean$SE*100,2) )
    return(model)
  } else {
    
    Covariates<-as.formula(paste0("~",x))
    svymean<-as.data.frame(svymean(Covariates,rhcSvy_DM_PD, na.rm = TRUE))
    colnames(svymean)[2]<-"SE"
    model <- data.frame('Covariates'=x,
                        'grade' ="Mean ± SE",
                        'counts'=' ',
                        'Mean' =round(svymean$mean,2),
                        'SE' = round(svymean$SE,2))
    return(model)
  }
}  
PD_data<- ldply(lapply(VAR_ALL, model))
total_data<-cbind(all_data,noPD_data[,c("counts","Mean","SE")],PD_data[,c("counts","Mean","SE")])
save(total_data,file = "./TABLE/Table1_Rdata")

#t-test and chi-test
model<- function(x){
  
  if( x %in% var ) {
    formula<-as.formula(paste0("~",x,"+Periodontitis"))
    chi_test<-svychisq(formula,rhcSvy_DM)
    model <- data.frame('Covariates'=x,
                        'P value' =chi_test[["p.value"]])
    return(model)
  } else {
    formula<-as.formula(paste0(x,"~Periodontitis"))
    t_test<-svyttest(formula,rhcSvy_DM)
    model <- data.frame('Covariates'=x,
                        'P value' =t_test[["p.value"]])
    return(model)
  }
}  
test_data<- ldply(lapply(VAR_ALL, model))
test_data$P.value<-round(test_data$P.value,3)
test_data$P.value[test_data$P.value==0]<-"<0.001"
new.function <- function(x){
  while(nchar(x)<5){
    temp <- paste(x,0)
    x <- temp
    x <- gsub(" ","",x)
  }
  return(x)
}
test_data$P.value<-lapply(test_data$P.value,new.function)
test_data$P.value<-as.character(test_data$P.value)

load(file = "./TABLE/Table1_Rdata")
data.all<-merge(total_data,test_data,by="Covariates",all = T)
write.table(data.all,sep = ",",file ="./TABLE/Table1.csv")

#=km曲线=================================
library(survminer)
library(survival)
library(ggplot2)
library(ggsci)
#ALL
library(survey)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
FIT<-svykm(Surv(peryear,MORT_status==1)~Periodontitis, design=rhcSvy_DM)

FIT_ALL_PD<-survfit(Surv(peryear, MORT_status==1) ~ Periodontitis, ALL_Weight_plus, weights = ALL_Weight_plus$weight)
summary(FIT_ALL_PD)    
PLOT<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#5F97D2", "#F0988C"), 
      
           break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
           xlab ="Time in Year",  pval = F, legend.title = "",ggtheme = theme(panel.background = element_rect(fill="white"),panel.border = element_rect(fill=NA,color="black", size=1.1)))
PLOT$plot = PLOT$plot + ggplot2::annotate("text",x =4.5, y = 0.2,label = "Median survival time (years):",size=5) +
  ggplot2::annotate("text",x =6.5, y = 0.15,label = "No/Mild periodontitis: 25.8 (23.7-30.3)",size=5)+
  ggplot2::annotate("text",x =8.5, y = 0.1,label = "Moderate/Severe periodontitis: 17.2 (16.4-18.6)",size=5)+
  ggplot2::annotate("text",x =0.8, y = 0.25,label = "P < 0.001",size=5)
PLOT
record<-ls()
rm(list=record[which(record!='ALL_Weight_plus')])

#CVD
ALL_Weight_plus_DM<-subset(ALL_Weight_plus,T2D=="T2D"&CVD_status=="NO")


FIT_ALL_PD<-survfit(Surv(peryear, CVD_MORT_status==1) ~ Periodontitis, ALL_Weight_plus_DM, weights = ALL_Weight_plus_DM$weight)
FIT_ALL_PD
PLOT<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#5F97D2", "#F0988C"), 
                 
                 break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                 xlab ="Time in Year", pval = T, legend.title = "",ggtheme = theme(panel.background = element_rect(fill="white"),panel.border = element_rect(fill=NA,color="black", size=1.1)))


PLOT
#6*7.8
#CANCER
ALL_Weight_plus_DM<-subset(ALL_Weight_plus,T2D=="T2D"&Cancer_status=="NO")


FIT_ALL_PD<-survfit(Surv(peryear, Cancer_MORT_status==1) ~ Periodontitis, ALL_Weight_plus_DM, weights = ALL_Weight_plus_DM$weight)
FIT_ALL_PD
PLOT<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#5F97D2", "#F0988C"), 
                 
                 break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                 xlab ="Time in Year", pval = T, legend.title = "",ggtheme = theme(panel.background = element_rect(fill="white"),panel.border = element_rect(fill=NA,color="black", size=1.1)))


PLOT
ALL_Weight_plus_DM<-subset(ALL_Weight_plus,T2D=="T2D")


FIT_ALL_PD<-survfit(Surv(peryear, DM_MORT_status==1) ~ Periodontitis, ALL_Weight_plus_DM, weights = ALL_Weight_plus_DM$weight)
FIT_ALL_PD
PLOT<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#5F97D2", "#F0988C"), 
                 
                 break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                 xlab ="Time in Year", pval = T, legend.title = "",ggtheme = theme(panel.background = element_rect(fill="white"),panel.border = element_rect(fill=NA,color="black", size=1.1)))


PLOT
#====CVD KM==============================
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D"&CVD_status=='NO')
ALL_Weight_plus_DM<-subset(ALL_Weight_plus,CVD_status=='NO')
FIT_ALL_PD<-survfit(Surv(peryear, CVD_MORT_status==1) ~ Periodontitis, ALL_Weight_plus_DM)
print(FIT_ALL_PD)  
PLOT<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, tables.theme = theme_cleantable(), palette=c("#5F97D2", "#F0988C"), 
                 
                 break.x.by = 5,font.tickslab = c(12, "plain", "black"),font.x = c(14, "bold", "black"),
                 font.y = c(14, "bold", "black"),
                 xlab ="Time in Year",  pval = F, legend.title = "",ggtheme = theme(panel.background = element_rect(fill="white"),panel.border = element_rect(fill=NA,color="black", size=1.1)))
PLOT$plot = PLOT$plot + ggplot2::annotate("text",x =4.5, y = 0.2,label = "Median survival time (years):",size=5) +
  ggplot2::annotate("text",x =6.5, y = 0.15,label = "No/Mild periodontitis: 25.8 (23.7-30.3)",size=5)+
  ggplot2::annotate("text",x =8.5, y = 0.1,label = "Moderate/Severe periodontitis: 17.2 (16.4-18.6)",size=5)+
  ggplot2::annotate("text",x =0.8, y = 0.25,label = "P < 0.001",size=5)
PLOT


#6x7.6
#COX回归
load(file="./Survey/ALL_Weight_plus_CA.Rdata")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
#all model1
model1_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI_status, design =rhcSvy_DM)
model1_all_result<-summary(model1_all)
P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'model'="model1",'status'="All cause")
result
#all model2
model2_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+CVD_status+Cancer_status, design =rhcSvy_DM)
model2_all_result<-summary(model2_all)
P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="model2",'status'="All cause")
result.all<-rbind(result,result2)
#all model3
model3_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+CVD_status+
                       HbA1c_status+duration_status+medicine_use+family_history, design =rhcSvy_DM)
model3_all_result<-summary(model3_all)

P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model3",'status'="All cause")
result.all<-rbind(result.all,result3)
result.all
#CVD model
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM_CVD<-subset(rhcSvy,T2D=="T2D"&CVD_status=="NO")
svytable(~CVD_status+CKD_status,rhcSvy_DM_CVD)
#CVD model1
colnames(ALL_Weight_plus)
model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status, design =rhcSvy_DM_CVD)
model1_CVD_result<-summary(model1_CVD)
P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="model1",'status'="CVD cause")
result.CVD
#CVD model2
model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status, design =rhcSvy_DM_CVD)
model2_CVD_result<-summary(model2_CVD)
P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model2",'status'="CVD cause")
result.CVD<-rbind(result.CVD,result2.CVD)
result.CVD
#CVD model3
model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+
                       HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM_CVD)
model3_CVD_result<-summary(model3_CVD)
P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model3",'status'="CVD cause")
result.CVD<-rbind(result.CVD,result3.CVD)
result.CVD
#Cancer model
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM_Cancer<-subset(rhcSvy,T2D=="T2D"&Cancer_status=="NO")
svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
#Cancer model1

model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status, design =rhcSvy_DM_Cancer)
model1_Cancer_result<-summary(model1_Cancer)
P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'model'="model1",'status'="Cancer cause")
result.Cancer
#Cancer model2
model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+CVD_status, design =rhcSvy_DM_Cancer)
model2_Cancer_result<-summary(model2_Cancer)
P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model2",'status'="Cancer cause")
result.Cancer<-rbind(result.Cancer,result2.Cancer)
result.Cancer
#Cancer model3
model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+CVD_status+
                       HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM_Cancer)
model3_Cancer_result<-summary(model3_Cancer)
P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="Cancer cause")
result.Cancer<-rbind(result.Cancer,result3.Cancer)
result.Cancer
#DM model
#DM model1
model1_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status, design =rhcSvy_DM)
model1_DM_result<-summary(model1_DM)
P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="model1",'status'="DM cause")
result
#all model2
model2_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+CVD_status+Cancer_status, design =rhcSvy_DM)
model2_DM_result<-summary(model2_DM)
P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model2",'status'="DM cause")
result.DM<-rbind(result,result2)
#DM model3
model3_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+CVD_status+
                       HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM)
model3_DM_result<-summary(model3_DM)

P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model3",'status'="DM cause")
result.DM<-rbind(result.DM,result3)
result.DM



#COMBINE

result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
result.all.cause$HR<-round(result.all.cause$HR,2)
result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
result.all.cause$P.value<-round(result.all.cause$P.value,3)
result.all.cause
write.table(result.all.cause,sep = ",",file ="./TABLE/Table2.csv")
PD.counts<-table(ALL_Weight_plus$Periodontitis,useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$MORT_status,useNA = "ifany")
all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
PD.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$CVD_status=="NO",useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$CVD_status=="NO",ALL_Weight_plus$Cancer_MORT_status,useNA = "ifany")
CVD.cause<-c(paste0(PD.M.counts[1,2,2],"/",PD.counts[1,2]),paste0(PD.M.counts[2,2,2],"/",PD.counts[2,2]))
PD.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$Cancer_status=="NO",useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$Cancer_status=="NO",ALL_Weight_plus$Cancer_MORT_status,useNA = "ifany")
Cancer.cause<-c(paste0(PD.M.counts[1,2,2],"/",PD.counts[1,2]),paste0(PD.M.counts[2,2,2],"/",PD.counts[2,2]))
PD.counts<-table(ALL_Weight_plus$Periodontitis,useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$DM_MORT_status,useNA = "ifany")
DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
total.counts<-rbind(all.cause,CVD.cause,Cancer.cause,DM.cause)
write.table(total.counts,sep = ",",file ="./TABLE/Table2_counts.csv")

#=subgroup==========================================
load(file="./Survey/ALL_Weight_plus_CA.Rdata")
#age
ALL_Weight_plus_DM<-ALL_Weight_plus
table(ALL_Weight_plus_DM$Age_status,useNA = "ifany")
ALL_Weight_plus$Age_sub[ALL_Weight_plus$Age<45]<-"<45"
ALL_Weight_plus$Age_sub[ALL_Weight_plus$Age>=45]<-">=45"
table(ALL_Weight_plus$Age_sub)
ALL_Weight_plus$Age_sub<-as.factor(ALL_Weight_plus$Age_sub)
#RACE
ALL_Weight_plus$Race_sub<-"Other_Race"
ALL_Weight_plus$Race_sub[ALL_Weight_plus$Race_ethnicity=="Non-Hispanic white"]<-"Non-Hispanic white"
table(ALL_Weight_plus$Race_sub)
ALL_Weight_plus$Race_sub<-as.factor(ALL_Weight_plus$Race_sub)
#BMI
ALL_Weight_plus$BMI_sub[ALL_Weight_plus$BMI<30]<-"<30"
ALL_Weight_plus$BMI_sub[ALL_Weight_plus$BMI>=30]<-">=30"
table(ALL_Weight_plus$BMI_sub)
ALL_Weight_plus$BMI_sub<-as.factor(ALL_Weight_plus$BMI_sub)

#smoke
ALL_Weight_plus$Smoking_sub<-"Former/Current smoker"
ALL_Weight_plus$Smoking_sub[ALL_Weight_plus$Smoking_status=="Never smoker"]<-"Never smoker"
table(ALL_Weight_plus$Smoking_sub)
ALL_Weight_plus$Smoking_sub<-as.factor(ALL_Weight_plus$Smoking_sub)

rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
rhcSvy_DM_CVD<-subset(rhcSvy,T2D=="T2D"&CVD_status=="NO")
rhcSvy_DM_45<-subset(rhcSvy,T2D=="T2D"&Age_sub=="<45")
rhcSvy_DM_45_CVD<-subset(rhcSvy,T2D=="T2D"&Age_sub=="<45"&CVD_status=="NO")
rhcSvy_DM_80<-subset(rhcSvy,T2D=="T2D"&Age_sub==">=45")
rhcSvy_DM_80_CVD<-subset(rhcSvy,T2D=="T2D"&Age_sub==">=45"&CVD_status=="NO")
svytable(~CVD_status+Age_status,rhcSvy_DM_45)
#AGE
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Gender+
                            Race_ethnicity+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+medicine_use, design =rhcSvy_DM_45)
Age45<-summary(MODEL_ALL_inter)
P<-Age45[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Age45[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Age1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'subgroup'="Age","group"="<45",'status'="all cause")
Age1
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Gender+
                            Race_ethnicity+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_45_CVD)
Age45<-summary(MODEL_ALL_inter)
P<-Age45[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Age45[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Age2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Age","group"="<45",'status'="CVD cause")
Age.all<-rbind(Age1,Age2)
Age.all
#45-65
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Gender+
                            Race_ethnicity+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+medicine_use, design =rhcSvy_DM_80)
Age80<-summary(MODEL_ALL_inter)
P<-Age80[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Age80[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Age3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Age","group"=">=45",'status'="all cause")
Age3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Gender+
                            Race_ethnicity+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_80_CVD)
Age80<-summary(MODEL_ALL_inter)
P<-Age80[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Age80[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Age4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Age","group"=">=45",'status'="CVD cause")
Age.all<-rbind(Age.all,Age3,Age4)
Age.all

#epiR
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_sub+Periodontitis*Age_sub+Gender+
                            Race_ethnicity+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+CVD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM)
Age<-summary(MODEL_ALL_inter)
P<-Age[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(Age[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Age5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Age","group"="interaction",'status'="all cause")
Age.all<-rbind(Age.all,Age5)
Age.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_sub+Periodontitis*Age_sub+Gender+
                            Race_ethnicity+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_CVD)
Age_c<-summary(MODEL_ALL_inter)
P<-Age_c[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(Age_c[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Age6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Age","group"="interaction",'status'="CVD cause")
Age.all<-rbind(Age.all,Age6)
Age.all

#RACE
table(ALL_Weight_plus$Race_sub)
rhcSvy_DM_white<-subset(rhcSvy,T2D=="T2D"&Race_sub=="Non-Hispanic white")
rhcSvy_DM_white_CVD<-subset(rhcSvy,T2D=="T2D"&Race_sub=="Non-Hispanic white"&CVD_status=="NO")
rhcSvy_DM_Other<-subset(rhcSvy,T2D=="T2D"&Race_sub=="Other_Race")
rhcSvy_DM_Other_CVD<-subset(rhcSvy,T2D=="T2D"&Race_sub=="Other_Race"&CVD_status=="NO")

MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                            Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+medicine_use, design =rhcSvy_DM_white)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Race1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Race","group"="white",'status'="all cause")
Race1
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                            Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_white_CVD)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Race2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="Race","group"="white",'status'="CVD cause")
Race2
Race.all<-rbind(Race1,Race2)
Race.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                            Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+medicine_use, design =rhcSvy_DM_Other)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Race3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="Race","group"="Other",'status'="all cause")
Race3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                            Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_Other_CVD)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Race4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Race","group"="Other",'status'="CVD cause")
Race4
Race.all<-rbind(Race.all,Race3,Race4)
Race.all
#epiR
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Race_sub+Periodontitis*Race_sub+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+CVD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM)
Age<-summary(MODEL_ALL_inter)
P<-Age[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(Age[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Race5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Race","group"="interaction",'status'="all cause")
Race.all<-rbind(Race.all,Race5)
Race.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Race_sub+Periodontitis*Race_sub+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_CVD)
Age_c<-summary(MODEL_ALL_inter)
P<-Age_c[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(Age_c[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Race6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Race","group"="interaction",'status'="CVD cause")
Race.all<-rbind(Race.all,Race6)
Race.all

#gender
table(ALL_Weight_plus$Gender)
rhcSvy_DM_Female<-subset(rhcSvy,T2D=="T2D"&Gender=="Female")
rhcSvy_DM_Female_CVD<-subset(rhcSvy,T2D=="T2D"&Gender=="Female"&CVD_status=="NO")
rhcSvy_DM_Male<-subset(rhcSvy,T2D=="T2D"&Gender=="Male")
rhcSvy_DM_Male_CVD<-subset(rhcSvy,T2D=="T2D"&Gender=="Male"&CVD_status=="NO")

MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+
                            Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+medicine_use, design =rhcSvy_DM_Female)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Gender1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="Gender","group"="Female",'status'="all cause")
Gender1
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+
                            Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_Female_CVD)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Gender2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Gender","group"="Female",'status'="CVD cause")
Gender2
Gender.all<-rbind(Gender1,Gender2)
Gender.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+
                            Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+medicine_use, design =rhcSvy_DM_Male)
Racewhite<-summary(MODEL_ALL_inter)
P<-Racewhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Racewhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Gender3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="Gender","group"="Male",'status'="all cause")
Gender3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+
                            Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_Male_CVD)
Genderwhite<-summary(MODEL_ALL_inter)
P<-Genderwhite[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(Genderwhite[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Gender4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Gender","group"="Male",'status'="CVD cause")
Gender4
Gender.all<-rbind(Gender.all,Gender3,Gender4)
Gender.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Gender+Periodontitis*Gender+Race_ethnicity+
                            Age_status+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+CVD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM)
Age<-summary(MODEL_ALL_inter)
P<-Age[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(Age[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Gender5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Gender","group"="interaction",'status'="all cause")
Gender.all<-rbind(Gender.all,Gender5)
Gender.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Gender+Periodontitis*Gender+Race_ethnicity+
                            Age_status+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_CVD)
Age_c<-summary(MODEL_ALL_inter)
P<-Age_c[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(Age_c[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Gender6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Gender","group"="interaction",'status'="CVD cause")
Gender.all<-rbind(Gender.all,Gender6)
Gender.all
#BMI
colnames(ALL_Weight_plus)
table(ALL_Weight_plus$BMI_sub)
rhcSvy_DM_30<-subset(rhcSvy,T2D=="T2D"&BMI_sub=="<30")
rhcSvy_DM_30_CVD<-subset(rhcSvy,T2D=="T2D"&BMI_sub=="<30"&CVD_status=="NO")
rhcSvy_DM_60<-subset(rhcSvy,T2D=="T2D"&BMI_sub==">=30")
rhcSvy_DM_60_CVD<-subset(rhcSvy,T2D=="T2D"&BMI_sub==">=30"&CVD_status=="NO")
svytable(~CVD_MORT_status+BMI_sub,rhcSvy_DM_30_CVD)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+medicine_use, design =rhcSvy_DM_30)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
BMI1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="BMI","group"="<30",'status'="all cause")
BMI1
svytable(~CVD_MORT_status+BMI_sub,rhcSvy_DM_30_CVD)
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_30_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
BMI2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="BMI","group"="<30",'status'="CVD cause")
BMI2
BMI.all<-rbind(BMI1,BMI2)
BMI.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+medicine_use, design =rhcSvy_DM_60)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
BMI3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'subgroup'="BMI","group"=">=30",'status'="all cause")
BMI3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_60_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
BMI4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="BMI","group"=">=30",'status'="CVD cause")
BMI4
BMI.all<-rbind(BMI.all,BMI3,BMI4)
BMI.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+BMI_sub+Periodontitis*BMI_sub+Race_ethnicity+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+CVD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
BMI5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="BMI","group"="interaction",'status'="all cause")
BMI.all<-rbind(BMI.all,BMI5)
BMI.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+BMI_sub+Periodontitis*BMI_sub+Race_ethnicity+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
BMI6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="BMI","group"="interaction",'status'="CVD cause")
BMI.all<-rbind(BMI.all,BMI6)
BMI.all
# Smoking_sub
colnames(ALL_Weight_plus)
table(ALL_Weight_plus$Smoking_sub)
rhcSvy_DM_Current<-subset(rhcSvy,T2D=="T2D"&Smoking_sub=="Former/Current smoker")
rhcSvy_DM_Current_CVD<-subset(rhcSvy,T2D=="T2D"&Smoking_sub=="Former/Current smoker"&CVD_status=="NO")
rhcSvy_DM_Never<-subset(rhcSvy,T2D=="T2D"&Smoking_sub=="Never smoker")
rhcSvy_DM_Never_CVD<-subset(rhcSvy,T2D=="T2D"&Smoking_sub=="Never smoker"&CVD_status=="NO")
svytable(~CVD_MORT_status+Smoking_sub,rhcSvy_DM_3_CVD)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+medicine_use, design =rhcSvy_DM_Current)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Smoke1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Smoke","group"="Current",'status'="all cause")
Smoke1

MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_Current_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Smoke2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Smoke","group"="Current",'status'="CVD cause")
Smoke2
Smoke.all<-rbind(Smoke1,Smoke2)
Smoke.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+medicine_use, design =rhcSvy_DM_Never)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Smoke3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'subgroup'="Smoke","group"="Never",'status'="all cause")
Smoke3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_Never_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
Smoke4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Smoke","group"="Never",'status'="CVD cause")
Smoke4
Smoke.all<-rbind(Smoke.all,Smoke3,Smoke4)
Smoke.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Smoking_sub+Periodontitis*Smoking_sub+Race_ethnicity+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+CVD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Smoke5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Smoke","group"="interaction",'status'="all cause")
Smoke.all<-rbind(Smoke.all,Smoke5)
Smoke.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Smoking_sub+Periodontitis*Smoking_sub+Race_ethnicity+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+medicine_use, design =rhcSvy_DM_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
Smoke6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                  'P value' =P,'subgroup'="Smoke","group"="interaction",'status'="CVD cause")
Smoke.all<-rbind(Smoke.all,Smoke6)
Smoke.all
# Diabetes duration
colnames(ALL_Weight_plus)
table(ALL_Weight_plus$duration_status)
rhcSvy_DM_3<-subset(rhcSvy,T2D=="T2D"&duration_status=="duration<3")
rhcSvy_DM_3_CVD<-subset(rhcSvy,T2D=="T2D"&duration_status=="duration<3"&CVD_status=="NO")
rhcSvy_DM_6<-subset(rhcSvy,T2D=="T2D"&duration_status=="duration>=3")
rhcSvy_DM_6_CVD<-subset(rhcSvy,T2D=="T2D"&duration_status=="duration>=3"&CVD_status=="NO")
svytable(~CVD_MORT_status+duration_status,rhcSvy_DM_3_CVD)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+Smoking_status+medicine_use, design =rhcSvy_DM_3)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
duration1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="duration","group"="duration<3",'status'="all cause")
duration1
svytable(~CVD_MORT_status+duration_status,rhcSvy_DM_3_CVD)
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+Smoking_status+medicine_use, design =rhcSvy_DM_3_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
duration2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="duration","group"="duration<3",'status'="CVD cause")
duration2
duration.all<-rbind(duration1,duration2)
duration.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+Smoking_status+medicine_use, design =rhcSvy_DM_6)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
duration3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="duration","group"="duration>=3",'status'="all cause")
duration3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+Smoking_status+medicine_use, design =rhcSvy_DM_6_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
duration4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="duration","group"="duration>=3",'status'="CVD cause")
duration4
duration.all<-rbind(duration.all,duration3,duration4)
duration.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+duration_status+Periodontitis*duration_status+Race_ethnicity+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+CVD_status+Cancer_status+Smoking_status+medicine_use, design =rhcSvy_DM)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
duration5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="duration","group"="interaction",'status'="all cause")
duration.all<-rbind(duration.all,duration5)
duration.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+duration_status+Periodontitis*duration_status+Race_ethnicity+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+Smoking_status+medicine_use, design =rhcSvy_DM_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
duration6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="duration","group"="interaction",'status'="CVD cause")
duration.all<-rbind(duration.all,duration6)
duration.all
#medicine_use
colnames(ALL_Weight_plus)
table(ALL_Weight_plus$medicine_use)
rhcSvy_DM_YES<-subset(rhcSvy,T2D=="T2D"&medicine_use==1)
rhcSvy_DM_YES_CVD<-subset(rhcSvy,T2D=="T2D"&medicine_use==1&CVD_status=="NO")
rhcSvy_DM_NO<-subset(rhcSvy,T2D=="T2D"&medicine_use==0)
rhcSvy_DM_NO_CVD<-subset(rhcSvy,T2D=="T2D"&medicine_use==0&CVD_status=="NO")
svytable(~CVD_MORT_status+medicine_use,rhcSvy_DM_YES_CVD)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+Smoking_status, design =rhcSvy_DM_YES)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
medicine_use1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="medicine_use","group"="YES",'status'="all cause")
medicine_use1
svytable(~CVD_MORT_status+medicine_use,rhcSvy_DM_3_CVD)
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+Smoking_status, design =rhcSvy_DM_YES_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
medicine_use2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="medicine_use","group"="YES",'status'="CVD cause")
medicine_use2
medicine_use.all<-rbind(medicine_use1,medicine_use2)
medicine_use.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+Smoking_status, design =rhcSvy_DM_NO)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
medicine_use3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'subgroup'="medicine_use","group"="NO",'status'="all cause")
medicine_use3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+Smoking_status, design =rhcSvy_DM_NO_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
medicine_use4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="medicine_use","group"="NO",'status'="CVD cause")
medicine_use4
medicine_use.all<-rbind(medicine_use.all,medicine_use3,medicine_use4)
medicine_use.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+medicine_use+Periodontitis*medicine_use+Race_ethnicity+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+CVD_status+Cancer_status+duration_status+Smoking_status, design =rhcSvy_DM)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
medicine_use5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="medicine_use","group"="interaction",'status'="all cause")
medicine_use.all<-rbind(medicine_use.all,medicine_use5)
medicine_use.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+medicine_use+Periodontitis*medicine_use+Race_ethnicity+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+Smoking_status, design =rhcSvy_DM_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
medicine_use6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                    'P value' =P,'subgroup'="medicine_use","group"="interaction",'status'="CVD cause")
medicine_use.all<-rbind(medicine_use.all,medicine_use6)
medicine_use.all

#HbA1c
colnames(ALL_Weight_plus)
ALL_Weight_plus_DM<-subset(ALL_Weight_plus,T2D=="T2D")
table(ALL_Weight_plus_DM$HbA1c_status)
rhcSvy_DM_YES<-subset(rhcSvy,T2D=="T2D"&HbA1c_status=="HbA1c>=7")
rhcSvy_DM_YES_CVD<-subset(rhcSvy,T2D=="T2D"&HbA1c_status=="HbA1c>=7"&CVD_status=="NO")
rhcSvy_DM_NO<-subset(rhcSvy,T2D=="T2D"&HbA1c_status=="HbA1c<7")
rhcSvy_DM_NO_CVD<-subset(rhcSvy,T2D=="T2D"&HbA1c_status=="HbA1c<7"&CVD_status=="NO")
svytable(~CVD_MORT_status+HbA1c_status,rhcSvy_DM_YES_CVD)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+medicine_use+
                            CKD_status+Cancer_status+CVD_status+duration_status+Smoking_status, design =rhcSvy_DM_YES)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
HbA1c_status1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'subgroup'="HbA1c_status","group"="YES",'status'="all cause")
HbA1c_status1
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+medicine_use+
                            CKD_status+Cancer_status+duration_status+Smoking_status, design =rhcSvy_DM_YES_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
HbA1c_status2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="HbA1c_status","group"="YES",'status'="CVD cause")
HbA1c_status2
HbA1c_status.all<-rbind(HbA1c_status1,HbA1c_status2)
HbA1c_status.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+medicine_use+
                            CKD_status+Cancer_status+CVD_status+duration_status+Smoking_status, design =rhcSvy_DM_NO)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
HbA1c_status3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'subgroup'="HbA1c_status","group"="NO",'status'="all cause")
HbA1c_status3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+medicine_use+
                            CKD_status+Cancer_status+duration_status+Smoking_status, design =rhcSvy_DM_NO_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
HbA1c_status4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="HbA1c_status","group"="NO",'status'="CVD cause")
HbA1c_status4
HbA1c_status.all<-rbind(HbA1c_status.all,HbA1c_status3,HbA1c_status4)
HbA1c_status.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+HbA1c_status+Periodontitis*HbA1c_status+Race_ethnicity+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+medicine_use+
                            CKD_status+CVD_status+Cancer_status+duration_status+Smoking_status, design =rhcSvy_DM)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
HbA1c_status5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="HbA1c_status","group"="interaction",'status'="all cause")
HbA1c_status.all<-rbind(HbA1c_status.all,HbA1c_status5)
HbA1c_status.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+HbA1c_status+Periodontitis*HbA1c_status+Race_ethnicity+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+medicine_use+
                            CKD_status+Cancer_status+duration_status+Smoking_status, design =rhcSvy_DM_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
HbA1c_status6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="HbA1c_status","group"="interaction",'status'="CVD cause")
HbA1c_status.all<-rbind(HbA1c_status.all,HbA1c_status6)
HbA1c_status.all

#FAMILY
colnames(ALL_Weight_plus)
ALL_Weight_plus_DM<-subset(ALL_Weight_plus,T2D=="T2D")
table(ALL_Weight_plus_DM$family_history)
rhcSvy_DM_YES<-subset(rhcSvy,T2D=="T2D"&family_history=="YES")
rhcSvy_DM_YES_CVD<-subset(rhcSvy,T2D=="T2D"&family_history=="YES"&CVD_status=="NO")
rhcSvy_DM_NO<-subset(rhcSvy,T2D=="T2D"&family_history=="NO")
rhcSvy_DM_NO_CVD<-subset(rhcSvy,T2D=="T2D"&family_history=="NO"&CVD_status=="NO")
svytable(~CVD_MORT_status+family_history,rhcSvy_DM_YES_CVD)
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+medicine_use+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+Smoking_status, design =rhcSvy_DM_YES)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
family_history1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'subgroup'="family_history","group"="YES",'status'="all cause")
family_history1
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+medicine_use+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+Smoking_status, design =rhcSvy_DM_YES_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
family_history2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="family_history","group"="YES",'status'="CVD cause")
family_history2
family_history.all<-rbind(family_history1,family_history2)
family_history.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+medicine_use+HbA1c_status+
                            CKD_status+Cancer_status+CVD_status+duration_status+Smoking_status, design =rhcSvy_DM_NO)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
family_history3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'subgroup'="family_history","group"="NO",'status'="all cause")
family_history3
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Race_ethnicity+Gender+
                            Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+medicine_use+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+Smoking_status, design =rhcSvy_DM_NO_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
family_history4<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="family_history","group"="NO",'status'="CVD cause")
family_history4
family_history.all<-rbind(family_history.all,family_history3,family_history4)
family_history.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+family_history+Periodontitis*family_history+Race_ethnicity+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+medicine_use+HbA1c_status+
                            CKD_status+CVD_status+Cancer_status+duration_status+Smoking_status, design =rhcSvy_DM)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
family_history5<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="family_history","group"="interaction",'status'="all cause")
family_history.all<-rbind(family_history.all,family_history5)
family_history.all
MODEL_ALL_inter<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+family_history+Periodontitis*family_history+Race_ethnicity+Gender+
                            Age_status+Education_levels+PIR+Health_insurance+
                            BMI_status+Drinking_status+Physical_status+HEI+
                            HTN_status+HPL_status+medicine_use+HbA1c_status+
                            CKD_status+Cancer_status+duration_status+Smoking_status, design =rhcSvy_DM_CVD)
MODEL<-summary(MODEL_ALL_inter)
P<-MODEL[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(MODEL[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
family_history6<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                           'P value' =P,'subgroup'="family_history","group"="interaction",'status'="CVD cause")
family_history.all<-rbind(family_history.all,family_history6)
family_history.all


all.subgroup<-rbind(Age.all,Race.all,Gender.all,BMI.all,Smoke.all,duration.all,medicine_use.all,HbA1c_status.all,family_history.all)
all.subgroup$HR<-round(all.subgroup$HR,2)
all.subgroup$lower..95<-round(all.subgroup$lower..95,2)
all.subgroup$upper..95<-round(all.subgroup$upper..95,2)
all.subgroup$P.value<-round(all.subgroup$P.value,3)
write.table(all.subgroup,sep = ",",file ="./TABLE/Table3.csv")



#====敏感性分析===================
#===1.去掉2年内死亡==========================
load(file="./Survey/ALL_Weight_plus_CA.Rdata")
ALL_Weight_plus<-subset(ALL_Weight_plus,peryear>=2)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
#all model1
model1_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status, design =rhcSvy_DM)
model1_all_result<-summary(model1_all)
P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="model1",'status'="All cause")
result
#all model2
model2_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+CVD_status+Cancer_status, design =rhcSvy_DM)
model2_all_result<-summary(model2_all)
P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model2",'status'="All cause")
result.all<-rbind(result,result2)
#all model3
model3_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+CVD_status+
                       HbA1c_status+duration_status+medicine_use+family_history, design =rhcSvy_DM)
model3_all_result<-summary(model3_all)

P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model3",'status'="All cause")
result.all<-rbind(result.all,result3)
result.all
#CVD model
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM_CVD<-subset(rhcSvy,T2D=="T2D"&CVD_status=="NO")
svytable(~CVD_status+CKD_status,rhcSvy_DM_CVD)
#CVD model1
colnames(ALL_Weight_plus)
model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status, design =rhcSvy_DM_CVD)
model1_CVD_result<-summary(model1_CVD)
P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'model'="model1",'status'="CVD cause")
result.CVD
#CVD model2
model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status, design =rhcSvy_DM_CVD)
model2_CVD_result<-summary(model2_CVD)
P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model2",'status'="CVD cause")
result.CVD<-rbind(result.CVD,result2.CVD)
result.CVD
#CVD model3
model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+
                       HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM_CVD)
model3_CVD_result<-summary(model3_CVD)
P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="CVD cause")
result.CVD<-rbind(result.CVD,result3.CVD)
result.CVD
#Cancer model
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM_Cancer<-subset(rhcSvy,T2D=="T2D"&Cancer_status=="NO")
svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
#Cancer model1

model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI_status, design =rhcSvy_DM_Cancer)
model1_Cancer_result<-summary(model1_Cancer)
P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model1",'status'="Cancer cause")
result.Cancer
#Cancer model2
model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI_status+HTN_status+HPL_status+CKD_status+CVD_status, design =rhcSvy_DM_Cancer)
model2_Cancer_result<-summary(model2_Cancer)
P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'model'="model2",'status'="Cancer cause")
result.Cancer<-rbind(result.Cancer,result2.Cancer)
result.Cancer
#Cancer model3
model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI_status+HTN_status+HPL_status+CKD_status+CVD_status+
                          HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM_Cancer)
model3_Cancer_result<-summary(model3_Cancer)
P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'model'="model3",'status'="Cancer cause")
result.Cancer<-rbind(result.Cancer,result3.Cancer)
result.Cancer
#DM model
#DM model1
model1_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                      Race_ethnicity+Education_levels+PIR+Health_insurance+
                      Smoking_status+Drinking_status+Physical_status+HEI+
                      BMI_status, design =rhcSvy_DM)
model1_DM_result<-summary(model1_DM)
P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="model1",'status'="DM cause")
result
#all model2
model2_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                      Race_ethnicity+Education_levels+PIR+Health_insurance+
                      Smoking_status+Drinking_status+Physical_status+HEI+
                      BMI_status+HTN_status+HPL_status+CKD_status+CVD_status+Cancer_status, design =rhcSvy_DM)
model2_DM_result<-summary(model2_DM)
P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model2",'status'="DM cause")
result.DM<-rbind(result,result2)
#DM model3
model3_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                      Race_ethnicity+Education_levels+PIR+Health_insurance+
                      Smoking_status+Drinking_status+Physical_status+HEI+
                      BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+CVD_status+
                      HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM)
model3_DM_result<-summary(model3_DM)

P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model3",'status'="DM cause")
result.DM<-rbind(result.DM,result3)
result.DM



#COMBINE

result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
result.all.cause$HR<-round(result.all.cause$HR,2)
result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
result.all.cause$P.value<-round(result.all.cause$P.value,3)
result.all.cause
write.table(result.all.cause,sep = ",",file ="./TABLE/Table2s.csv")
PD.counts<-table(ALL_Weight_plus$Periodontitis,useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$MORT_status,useNA = "ifany")
all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
PD.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$CVD_status=="NO",useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$CVD_status=="NO",ALL_Weight_plus$Cancer_MORT_status,useNA = "ifany")
CVD.cause<-c(paste0(PD.M.counts[1,2,2],"/",PD.counts[1,2]),paste0(PD.M.counts[2,2,2],"/",PD.counts[2,2]))
PD.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$Cancer_status=="NO",useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$Cancer_status=="NO",ALL_Weight_plus$Cancer_MORT_status,useNA = "ifany")
Cancer.cause<-c(paste0(PD.M.counts[1,2,2],"/",PD.counts[1,2]),paste0(PD.M.counts[2,2,2],"/",PD.counts[2,2]))
PD.counts<-table(ALL_Weight_plus$Periodontitis,useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$DM_MORT_status,useNA = "ifany")
DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
total.counts<-rbind(all.cause,CVD.cause,Cancer.cause,DM.cause)
write.table(total.counts,sep = ",",file ="./TABLE/Table2s_counts.csv")
table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$CVD_status,ALL_Weight_plus$CVD_MORT_status)


#===2.去掉癌症CVD==========================
load(file="./Survey/ALL_Weight_plus_CA.Rdata")
ALL_Weight_plus<-subset(ALL_Weight_plus,CVD_status=='NO'&Cancer_status=='NO')
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
#all model1
model1_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status, design =rhcSvy_DM)
model1_all_result<-summary(model1_all)
P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="model1",'status'="All cause")
result
#all model2
model2_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status, design =rhcSvy_DM)
model2_all_result<-summary(model2_all)
P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model2",'status'="All cause")
result.all<-rbind(result,result2)
#all model3
model3_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+
                       HbA1c_status+duration_status+medicine_use+family_history, design =rhcSvy_DM)
model3_all_result<-summary(model3_all)

P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model3",'status'="All cause")
result.all<-rbind(result.all,result3)
result.all
#CVD model
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM_CVD<-subset(rhcSvy,T2D=="T2D"&CVD_status=="NO")
svytable(~CVD_status+CKD_status,rhcSvy_DM_CVD)
#CVD model1
colnames(ALL_Weight_plus)
model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status, design =rhcSvy_DM_CVD)
model1_CVD_result<-summary(model1_CVD)
P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'model'="model1",'status'="CVD cause")
result.CVD
#CVD model2
model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status, design =rhcSvy_DM_CVD)
model2_CVD_result<-summary(model2_CVD)
P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model2",'status'="CVD cause")
result.CVD<-rbind(result.CVD,result2.CVD)
result.CVD
#CVD model3
model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+
                       HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM_CVD)
model3_CVD_result<-summary(model3_CVD)
P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="CVD cause")
result.CVD<-rbind(result.CVD,result3.CVD)
result.CVD
#Cancer model
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM_Cancer<-subset(rhcSvy,T2D=="T2D"&Cancer_status=="NO")
svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
#Cancer model1

model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI_status, design =rhcSvy_DM_Cancer)
model1_Cancer_result<-summary(model1_Cancer)
P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model1",'status'="Cancer cause")
result.Cancer
#Cancer model2
model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI_status+HTN_status+HPL_status+CKD_status, design =rhcSvy_DM_Cancer)
model2_Cancer_result<-summary(model2_Cancer)
P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'model'="model2",'status'="Cancer cause")
result.Cancer<-rbind(result.Cancer,result2.Cancer)
result.Cancer
#Cancer model3
model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI_status+HTN_status+HPL_status+CKD_status+
                          HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM_Cancer)
model3_Cancer_result<-summary(model3_Cancer)
P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'model'="model3",'status'="Cancer cause")
result.Cancer<-rbind(result.Cancer,result3.Cancer)
result.Cancer
#DM model
#DM model1
model1_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                      Race_ethnicity+Education_levels+PIR+Health_insurance+
                      Smoking_status+Drinking_status+Physical_status+HEI+
                      BMI_status, design =rhcSvy_DM)
model1_DM_result<-summary(model1_DM)
P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="model1",'status'="DM cause")
result
#all model2
model2_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                      Race_ethnicity+Education_levels+PIR+Health_insurance+
                      Smoking_status+Drinking_status+Physical_status+HEI+
                      BMI_status+HTN_status+HPL_status+CKD_status, design =rhcSvy_DM)
model2_DM_result<-summary(model2_DM)
P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model2",'status'="DM cause")
result.DM<-rbind(result,result2)
#DM model3
model3_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                      Race_ethnicity+Education_levels+PIR+Health_insurance+
                      Smoking_status+Drinking_status+Physical_status+HEI+
                      BMI_status+HTN_status+HPL_status+CKD_status+
                      HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM)
model3_DM_result<-summary(model3_DM)

P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model3",'status'="DM cause")
result.DM<-rbind(result.DM,result3)
result.DM



#COMBINE

result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
result.all.cause$HR<-round(result.all.cause$HR,2)
result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
result.all.cause$P.value<-round(result.all.cause$P.value,3)
result.all.cause
write.table(result.all.cause,sep = ",",file ="./TABLE/Table3s.csv")
PD.counts<-table(ALL_Weight_plus$Periodontitis,useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$MORT_status,useNA = "ifany")
all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
PD.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$CVD_status=="NO",useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$CVD_status=="NO",ALL_Weight_plus$Cancer_MORT_status,useNA = "ifany")
CVD.cause<-c(paste0(PD.M.counts[1,2,2],"/",PD.counts[1,2]),paste0(PD.M.counts[2,2,2],"/",PD.counts[2,2]))
PD.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$Cancer_status=="NO",useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$Cancer_status=="NO",ALL_Weight_plus$Cancer_MORT_status,useNA = "ifany")
Cancer.cause<-c(paste0(PD.M.counts[1,2,2],"/",PD.counts[1,2]),paste0(PD.M.counts[2,2,2],"/",PD.counts[2,2]))
PD.counts<-table(ALL_Weight_plus$Periodontitis,useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$DM_MORT_status,useNA = "ifany")
DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
total.counts<-rbind(all.cause,CVD.cause,Cancer.cause,DM.cause)
write.table(total.counts,sep = ",",file ="./TABLE/Table3s_counts.csv")

#===3.不填补====

load(file="./Weight/ALL_Weight.Rdata")
ALL_Weight$Periodontitis<-factor(ALL_Weight$Periodontitis,levels = c("No/Mild periodontitis","Moderate/Severe periodontitis"))
table( ALL_Weight$Periodontitis)
#T2D
ALL_Weight$T2D<-factor( ALL_Weight$T2D,levels =c("T2D","No T2D"))
table( ALL_Weight$T2D,ALL_Weight$MORT_stat,ALL_Weight$Periodontitis)

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

table(ALL_Weight$Age_status)
ALL_Weight$Age_status[ALL_Weight$Age<45]<-"<45"
ALL_Weight$Age_status[ALL_Weight$Age<65&ALL_Weight$Age>=45]<-"[45,65)"
ALL_Weight$Age_status[ALL_Weight$Age>65]<-">=65"
ALL_Weight$Age_status<-factor(ALL_Weight$Age_status,levels = c("<45","[45,65)",">=65"))
table(ALL_Weight$Age_status)
ALL_Weight$Age_status<-as.character(ALL_Weight$Age_status)
#gender

table( ALL_Weight$Gender,useNA = "ifany")

#RACE
ALL_Weight$Race_ethnicity[ALL_Weight$Race_ethnicity=="Non-Hispanic White"]<-"Non-Hispanic white"
ALL_Weight$Race_ethnicity[ALL_Weight$Race_ethnicity=="Non-Hispanic Black"]<-"Non-Hispanic black"

table(ALL_Weight$Race_ethnicity,useNA = "ifany")
#Education_levels
table( ALL_Weight$Education_levels,useNA = "ifany")
#PIR
table(ALL_Weight$PIR2,useNA = "ifany")
#Health_insurance
table( ALL_Weight$Health_insurance)
#Smoking_status

table( ALL_Weight$Smoking_status)
#Drinking_status
table( ALL_Weight$Drinking_status)
#Physical_status
table(ALL_Weight$Physical_status)
#HEI

ALL_Weight$HEI<-as.factor(ALL_Weight$HEI)
table(ALL_Weight$HEI)
#HTN_status

ALL_Weight$HTN_status<-factor(ALL_Weight$HTN_status,levels = c("YES","NO"))
table(ALL_Weight$HTN_status)
#HPL_status

table(ALL_Weight$HPL_status)
#CVD

table(ALL_Weight$CVD)
#CKD

table(ALL_Weight$CKD)
#CANCER

table(ALL_Weight$Cancer)
#Br_diabetes_status

table(ALL_Weight$Br_diabetes_status)
#HbA1c>=7

ALL_Weight$HbA1c_status[ALL_Weight$HbA1c>=7]<-"HbA1c>=7"
ALL_Weight$HbA1c_status[ALL_Weight$HbA1c<7]<-"HbA1c<7"
ALL_Weight$HbA1c_status<-factor(ALL_Weight$HbA1c_status,levels = c("HbA1c>=7","HbA1c<7"))
table(ALL_Weight$HbA1c_status,ALL_Weight$T2D)
#BMI_status
ALL_Weight$BMI_status[ALL_Weight$BMI<25]<-"<25"
ALL_Weight$BMI_status[ALL_Weight$BMI<30&ALL_Weight$BMI>=25]<-"[25.0-30)"
ALL_Weight$BMI_status[ALL_Weight$BMI>=30]<-">=30"
ALL_Weight$BMI_status<-factor(ALL_Weight$BMI_status,levels = c("<25","[25.0-30)",">=30"))


ALL_Weight_plus<-ALL_Weight
colnames(ALL_Weight_plus)
load(file="./Weight/ALL_Weight.Rdata")
ALL_Weight_plus<-merge(ALL_Weight_plus,ALL_Weight[,c("ID","ucod_leading")],by = "ID",all= T)
ALL_Weight_plus$SEQN<-NULL
ALL_Weight_plus$PIR_raw<-NULL
ALL_Weight_plus$HEI_Score<-NULL
ALL_Weight_plus$BMI_Grade<-NULL
ALL_Weight_plus$HEI_Score<-NULL
ALL_Weight_plus$HEI_Score<-NULL
table(ALL_Weight_plus$CVD_status)
ALL_Weight_plus$CVD_status<-factor(ALL_Weight_plus$CVD_status,levels = c("NO","YES"))
ALL_Weight_plus$Cancer_status<-factor(ALL_Weight_plus$Cancer_status,levels = c("NO","YES"))
colnames(ALL_Weight_plus)<-c("ID","Periodontitis","T2D","MORT_status","CVD_MORT_status","diabetes","permth","Year","Age","Gender",
                             "Race_ethnicity","Education_levels","PIR","Health_insurance","Smoking_status",
                             "Drinking_status","Physical_status","HEI","BMI","HTN_status",
                             "HPL_status","CVD_status","CKD_status","Cancer_status","HbA1c",
                             "family_history","diabetes_duration","medicine_use","sdmvpsu","sdmvstra",
                             "weight","peryear","Age_status","HbA1c_status",
                             "BMI_status","Mortality_causes")
table(ALL_Weight_plus$Mortality_causes)
ALL_Weight_plus$Mortality_causes<-as.factor(ALL_Weight_plus$Mortality_causes)
table(ALL_Weight_plus$MORT_status)
ALL_Weight_plus$CVD_MORT_status[ALL_Weight_plus$MORT_status==1]<-2
ALL_Weight_plus$CVD_MORT_status[ALL_Weight_plus$MORT_status==1&ALL_Weight_plus$Mortality_causes=="CVD"]<-1
ALL_Weight_plus$CVD_MORT_status[ALL_Weight_plus$MORT_status==0]<-0
table(ALL_Weight_plus$CVD_MORT_status,useNA = "ifany")
ALL_Weight_plus$Year<-as.factor(ALL_Weight_plus$Year)
colnames(ALL_Weight_plus)
ALL_Weight_plus$MORT_status<-as.factor(ALL_Weight_plus$MORT_status)
ALL_Weight_plus$duration_status[ALL_Weight_plus$diabetes_duration>=3]<-"duration>=3"
ALL_Weight_plus$duration_status[ALL_Weight_plus$diabetes_duration<3]<-"duration<3"
table(ALL_Weight_plus$duration_status)
ALL_Weight_plus$duration_status<-as.factor(ALL_Weight_plus$duration_status)
ALL_Weight_plus$Cancer_MORT_status[ALL_Weight_plus$MORT_status==1]<-2
ALL_Weight_plus$Cancer_MORT_status[ALL_Weight_plus$MORT_status==1&ALL_Weight_plus$Mortality_causes=="Cancer"]<-1
ALL_Weight_plus$Cancer_MORT_status[ALL_Weight_plus$MORT_status==0]<-0
table(ALL_Weight_plus$Cancer_MORT_status,useNA = "ifany")
table(ALL_Weight_plus$Age_status,useNA = "ifany")
ALL_Weight_plus$Age_status[ALL_Weight_plus$Age<45]<-"<45"
ALL_Weight_plus$Age_status[ALL_Weight_plus$Age<65&ALL_Weight_plus$Age>=45]<-"[45,65)"
ALL_Weight_plus$Age_status[ALL_Weight_plus$Age>=65]<-">=65"
ALL_Weight_plus$DM_MORT_status<-2
ALL_Weight_plus$DM_MORT_status[ALL_Weight_plus$MORT_status==1&ALL_Weight_plus$Mortality_causes=="DM"]<-1
ALL_Weight_plus$DM_MORT_status[ALL_Weight_plus$MORT_status==0]<-0
table(ALL_Weight_plus$DM_MORT_status,useNA = "ifany")
table(ALL_Weight_plus$Mortality_causes)
ALL_Weight_plus$duration_status<-as.factor(ALL_Weight_plus$duration_status)
ALL_Weight_plus$PIR<-as.factor(ALL_Weight_plus$PIR)
ALL_Weight_plus$Age_status<-as.factor(ALL_Weight_plus$Age_status)
ALL_Weight_plus$Gender
ALL_Weight_plus$Race_ethnicity 
ALL_Weight_plus$ Education_levels
ALL_Weight_plus$Health_insurance
ALL_Weight_plus$Smoking_statusPhysical_status 
ALL_Weight_plus$Drinking_status
ALL_Weight_plus$Physical_status 
colnames(ALL_Weight_plus)[c(7,9,19,25,27)]
colnames(ALL_Weight_plus)

colApply <- function(dat, cols = colnames(dat), func = as.factor) {
  dat[cols] <- lapply(dat[cols], func)
  return(dat)
}
ALL_Weight_plus<-colApply(ALL_Weight_plus,-c(7,9,19,25,27,31,32), as.factor)

ALL_Weight_plus<-ALL_Weight_plus[,c("ID","Periodontitis","T2D","MORT_status","CVD_MORT_status","Cancer_MORT_status","DM_MORT_status","peryear",
                                    "Year","Age","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
                                    "Health_insurance","Smoking_status","Drinking_status","Physical_status","HEI",
                                    "BMI","BMI_status","HTN_status","HPL_status","CVD_status","CKD_status","Cancer_status","family_history",
                                    "HbA1c","HbA1c_status","diabetes_duration","duration_status","medicine_use",
                                    "sdmvpsu", "sdmvstra" ,"weight")]
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
#all model1
model1_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status, design =rhcSvy_DM)
model1_all_result<-summary(model1_all)
P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="model1",'status'="All cause")
result
#all model2
model2_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+CVD_status+Cancer_status, design =rhcSvy_DM)
model2_all_result<-summary(model2_all)
P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model2",'status'="All cause")
result.all<-rbind(result,result2)
#all model3
model3_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+CVD_status+
                       HbA1c_status+duration_status+medicine_use+family_history, design =rhcSvy_DM)
model3_all_result<-summary(model3_all)

P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model3",'status'="All cause")
result.all<-rbind(result.all,result3)
result.all
#CVD model
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM_CVD<-subset(rhcSvy,T2D=="T2D"&CVD_status=="NO")
svytable(~CVD_status+CKD_status,rhcSvy_DM_CVD)
#CVD model1
colnames(ALL_Weight_plus)
model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status, design =rhcSvy_DM_CVD)
model1_CVD_result<-summary(model1_CVD)
P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'model'="model1",'status'="CVD cause")
result.CVD
#CVD model2
model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status, design =rhcSvy_DM_CVD)
model2_CVD_result<-summary(model2_CVD)
P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model2",'status'="CVD cause")
result.CVD<-rbind(result.CVD,result2.CVD)
result.CVD
#CVD model3
model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+
                       HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM_CVD)
model3_CVD_result<-summary(model3_CVD)
P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="CVD cause")
result.CVD<-rbind(result.CVD,result3.CVD)
result.CVD
#Cancer model
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM_Cancer<-subset(rhcSvy,T2D=="T2D"&Cancer_status=="NO")
svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
#Cancer model1

model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI_status, design =rhcSvy_DM_Cancer)
model1_Cancer_result<-summary(model1_Cancer)
P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model1",'status'="Cancer cause")
result.Cancer
#Cancer model2
model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI_status+HTN_status+HPL_status+CKD_status+CVD_status, design =rhcSvy_DM_Cancer)
model2_Cancer_result<-summary(model2_Cancer)
P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'model'="model2",'status'="Cancer cause")
result.Cancer<-rbind(result.Cancer,result2.Cancer)
result.Cancer
#Cancer model3
model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI_status+HTN_status+HPL_status+CKD_status+CVD_status+
                          HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM_Cancer)
model3_Cancer_result<-summary(model3_Cancer)
P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'model'="model3",'status'="Cancer cause")
result.Cancer<-rbind(result.Cancer,result3.Cancer)
result.Cancer
#DM model
#DM model1
model1_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                      Race_ethnicity+Education_levels+PIR+Health_insurance+
                      Smoking_status+Drinking_status+Physical_status+HEI+
                      BMI_status, design =rhcSvy_DM)
model1_DM_result<-summary(model1_DM)
P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="model1",'status'="DM cause")
result
#all model2
model2_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                      Race_ethnicity+Education_levels+PIR+Health_insurance+
                      Smoking_status+Drinking_status+Physical_status+HEI+
                      BMI_status+HTN_status+HPL_status+CKD_status+CVD_status+Cancer_status, design =rhcSvy_DM)
model2_DM_result<-summary(model2_DM)
P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model2",'status'="DM cause")
result.DM<-rbind(result,result2)
#DM model3
model3_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                      Race_ethnicity+Education_levels+PIR+Health_insurance+
                      Smoking_status+Drinking_status+Physical_status+HEI+
                      BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+CVD_status+
                      HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM)
model3_DM_result<-summary(model3_DM)

P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model3",'status'="DM cause")
result.DM<-rbind(result.DM,result3)
result.DM



#COMBINE

result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
result.all.cause$HR<-round(result.all.cause$HR,2)
result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
result.all.cause$P.value<-round(result.all.cause$P.value,3)
result.all.cause
write.table(result.all.cause,sep = ",",file ="./TABLE/Table4s.csv")
ALL_Weight_plus$Periodontitis
table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$Cancer_status)
table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$Cancer_MORT_status,ALL_Weight_plus$Cancer_status)
table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$CVD_status)
table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$CVD_MORT_status,ALL_Weight_plus$CVD_status)


#====4.连续变量======
load(file="./Survey/ALL_Weight_plus_CA.Rdata")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
#all model1
model1_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI, design =rhcSvy_DM)
model1_all_result<-summary(model1_all)
P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="model1",'status'="All cause")
result
#all model2
model2_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI+HTN_status+HPL_status+CKD_status+CVD_status+Cancer_status, design =rhcSvy_DM)
model2_all_result<-summary(model2_all)
P<-model2_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model2",'status'="All cause")
result.all<-rbind(result,result2)
#all model3
model3_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI+HTN_status+HPL_status+CKD_status+Cancer_status+CVD_status+
                       HbA1c+duration_status+medicine_use+family_history, design =rhcSvy_DM)
model3_all_result<-summary(model3_all)

P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model3",'status'="All cause")
result.all<-rbind(result.all,result3)
result.all
#CVD model
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM_CVD<-subset(rhcSvy,T2D=="T2D"&CVD_status=="NO")
svytable(~CVD_status+CKD_status,rhcSvy_DM_CVD)
#CVD model1
colnames(ALL_Weight_plus)
model1_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI, design =rhcSvy_DM_CVD)
model1_CVD_result<-summary(model1_CVD)
P<-model1_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'model'="model1",'status'="CVD cause")
result.CVD
#CVD model2
model2_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI+HTN_status+HPL_status+CKD_status+Cancer_status, design =rhcSvy_DM_CVD)
model2_CVD_result<-summary(model2_CVD)
P<-model2_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model2",'status'="CVD cause")
result.CVD<-rbind(result.CVD,result2.CVD)
result.CVD
#CVD model3
model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI+HTN_status+HPL_status+CKD_status+Cancer_status+
                       HbA1c+duration_status+medicine_use, design =rhcSvy_DM_CVD)
model3_CVD_result<-summary(model3_CVD)
P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3.CVD <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="CVD cause")
result.CVD<-rbind(result.CVD,result3.CVD)
result.CVD
#Cancer model
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM_Cancer<-subset(rhcSvy,T2D=="T2D"&Cancer_status=="NO")
svytable(~CVD_status+Cancer_status,rhcSvy_DM_Cancer)
#Cancer model1

model1_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI, design =rhcSvy_DM_Cancer)
model1_Cancer_result<-summary(model1_Cancer)
P<-model1_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                            'P value' =P,'model'="model1",'status'="Cancer cause")
result.Cancer
#Cancer model2
model2_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI+HTN_status+HPL_status+CKD_status+CVD_status, design =rhcSvy_DM_Cancer)
model2_Cancer_result<-summary(model2_Cancer)
P<-model2_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'model'="model2",'status'="Cancer cause")
result.Cancer<-rbind(result.Cancer,result2.Cancer)
result.Cancer
#Cancer model3
model3_Cancer<-svycoxph(Surv(peryear, Cancer_MORT_status==1) ~Periodontitis+Age_status+Gender+
                          Race_ethnicity+Education_levels+PIR+Health_insurance+
                          Smoking_status+Drinking_status+Physical_status+HEI+
                          BMI+HTN_status+HPL_status+CKD_status+CVD_status+
                          HbA1c+duration_status+medicine_use, design =rhcSvy_DM_Cancer)
model3_Cancer_result<-summary(model3_Cancer)
P<-model3_Cancer_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_Cancer_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3.Cancer <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                             'P value' =P,'model'="model3",'status'="Cancer cause")
result.Cancer<-rbind(result.Cancer,result3.Cancer)
result.Cancer
#DM model
#DM model1
model1_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                      Race_ethnicity+Education_levels+PIR+Health_insurance+
                      Smoking_status+Drinking_status+Physical_status+HEI+
                      BMI, design =rhcSvy_DM)
model1_DM_result<-summary(model1_DM)
P<-model1_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="model1",'status'="DM cause")
result
#all model2
model2_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                      Race_ethnicity+Education_levels+PIR+Health_insurance+
                      Smoking_status+Drinking_status+Physical_status+HEI+
                      BMI+HTN_status+HPL_status+CKD_status+CVD_status+Cancer_status, design =rhcSvy_DM)
model2_DM_result<-summary(model2_DM)
P<-model2_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model2_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result2 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model2",'status'="DM cause")
result.DM<-rbind(result,result2)
#DM model3
model3_DM<-svycoxph(Surv(peryear, DM_MORT_status==1) ~Periodontitis+Age_status+Gender+
                      Race_ethnicity+Education_levels+PIR+Health_insurance+
                      Smoking_status+Drinking_status+Physical_status+HEI+
                      BMI+HTN_status+HPL_status+CKD_status+Cancer_status+CVD_status+
                      HbA1c+duration_status+medicine_use, design =rhcSvy_DM)
model3_DM_result<-summary(model3_DM)

P<-model3_DM_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_DM_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result3 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="model3",'status'="DM cause")
result.DM<-rbind(result.DM,result3)
result.DM



#COMBINE

result.all.cause<-rbind(result.all,result.CVD,result.Cancer,result.DM)
result.all.cause$HR<-round(result.all.cause$HR,2)
result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
result.all.cause$P.value<-round(result.all.cause$P.value,3)
result.all.cause
write.table(result.all.cause,sep = ",",file ="./TABLE/Table5s.csv")
PD.counts<-table(ALL_Weight_plus$Periodontitis,useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$MORT_status,useNA = "ifany")
all.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
PD.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$CVD_status=="NO",useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$CVD_status=="NO",ALL_Weight_plus$Cancer_MORT_status,useNA = "ifany")
CVD.cause<-c(paste0(PD.M.counts[1,2,2],"/",PD.counts[1,2]),paste0(PD.M.counts[2,2,2],"/",PD.counts[2,2]))
PD.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$Cancer_status=="NO",useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$Cancer_status=="NO",ALL_Weight_plus$Cancer_MORT_status,useNA = "ifany")
Cancer.cause<-c(paste0(PD.M.counts[1,2,2],"/",PD.counts[1,2]),paste0(PD.M.counts[2,2,2],"/",PD.counts[2,2]))
PD.counts<-table(ALL_Weight_plus$Periodontitis,useNA = "ifany")
PD.M.counts<-table(ALL_Weight_plus$Periodontitis,ALL_Weight_plus$DM_MORT_status,useNA = "ifany")
DM.cause<-c(paste0(PD.M.counts[1,2],"/",PD.counts[1]),paste0(PD.M.counts[2,2],"/",PD.counts[2]))
total.counts<-rbind(all.cause,CVD.cause,Cancer.cause,DM.cause)
write.table(total.counts,sep = ",",file ="./TABLE/Table5s_counts.csv")

#====5.model4=======
load(file="./Survey/ALL_Weight_plus_CA.Rdata")
table(ALL_Weight_plus$Year)
ALL_Weight_plus$PD_measure[ALL_Weight_plus$Year=="1988-1991"|ALL_Weight_plus$Year=="1991-1994"|ALL_Weight_plus$Year=="1999-2000"|
                     ALL_Weight_plus$Year=="2001-2002"|ALL_Weight_plus$Year=="2003-2004"]<-"half"
ALL_Weight_plus$PD_measure[ALL_Weight_plus$Year=="2009-2010"|ALL_Weight_plus$Year=="2011-2012"|
                             ALL_Weight_plus$Year=="2013-2014"]<-"whole"
table(ALL_Weight_plus$PD_measure)
ALL_Weight_plus$PD_measure<-as.factor(ALL_Weight_plus$PD_measure)
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
#all model3
model3_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+CVD_status+
                       HbA1c_status+duration_status+medicine_use+PD_measure, design =rhcSvy_DM)
model3_all_result<-summary(model3_all)
summary(model3_all)
P<-model3_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result.all1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                      'P value' =P,'model'="PD_measure",'status'="All cause")
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+PD_measure+Periodontitis*PD_measure+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+CVD_status+
                       HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM)
result<-summary(MODEL_ALL_inter)
P<-result[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(result[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
result.all2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                   'P value' =P,'model'="interaction",'status'="all cause")
result.all<-rbind(result.all1,result.all2)
#CVD model
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM_CVD<-subset(rhcSvy,T2D=="T2D"&CVD_status=="NO")
#CVD model3
model3_CVD<-svycoxph(Surv(peryear, CVD_MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+
                       HbA1c_status+duration_status+medicine_use+PD_measure, design =rhcSvy_DM_CVD)
model3_CVD_result<-summary(model3_CVD)
P<-model3_CVD_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model3_CVD_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result.CVD1 <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                          'P value' =P,'model'="model3",'status'="CVD cause")
MODEL_ALL_inter<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+PD_measure+Periodontitis*PD_measure+Age_status+Gender+
                            Race_ethnicity+Education_levels+PIR+Health_insurance+
                            Smoking_status+Drinking_status+Physical_status+HEI+
                            BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+
                            HbA1c_status+duration_status+medicine_use, design =rhcSvy_DM_CVD)
result<-summary(MODEL_ALL_inter)
P<-result[["coefficients"]][length(MODEL_ALL_inter[["coefficients"]]),"Pr(>|z|)"]
HR<-as.numeric(result[["conf.int"]][length(MODEL_ALL_inter[["coefficients"]]),c("exp(coef)","lower .95","upper .95")])
result.CVD2<- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                         'P value' =P,'model'="interaction",'status'="CVD cause")
result.CVD<-rbind(result.CVD1,result.CVD2)
result.CVD 




#COMBINE

result.all.cause<-rbind(result.all,result.CVD)
result.all.cause$HR<-round(result.all.cause$HR,2)
result.all.cause$lower..95<-round(result.all.cause$lower..95,2)
result.all.cause$upper..95<-round(result.all.cause$upper..95,2)
result.all.cause$P.value<-round(result.all.cause$P.value,3)
result.all.cause
write.table(result.all.cause,sep = ",",file ="./TABLE/Table6s.csv")



#=S1特征表=====================
load(file="./Weight/ALL_Weight.Rdata")
load(file="./Survey/ALL_Weight_plus_CA.Rdata")
OMIT.ALL_Weight<-na.omit(ALL_Weight[,-c(6,7)])
rownames(OMIT.ALL_Weight)<-OMIT.ALL_Weight$ID
rownames(ALL_Weight_plus)<-ALL_Weight_plus$ID
missing<-ALL_Weight_plus[!rownames(ALL_Weight_plus) %in% rownames(OMIT.ALL_Weight),]
rhcSvy <- svydesign(id = ~1,nest = TRUE, data =missing,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
rhcSvy_DM_CVD<-subset(rhcSvy,T2D=="T2D"&CVD_status=="NO")

#Categorical_variables
var<-c("Age_status","Gender","Race_ethnicity","Education_levels","PIR",
       "Health_insurance","Smoking_status","Drinking_status","Physical_status","HEI",
       "BMI_status","HTN_status","HPL_status","CVD_status","CKD_status","Cancer_status","family_history",
       "HbA1c_status","duration_status","medicine_use")
model<- function(x){
  
  if( x %in% var ) {
    Covariates<-as.formula(paste0("~",x))
    unwtd_count<-svyby(Covariates,Covariates,rhcSvy_DM,unwtd.count,drop.empty.groups=F) 
    svymean<-as.data.frame(svymean(Covariates,rhcSvy_DM, na.rm =F))
    model <- data.frame('Covariates'=x,
                        'grade' = gsub(x,"",rownames(svymean)),
                        'counts'=unwtd_count[2],
                        'Mean' = round(svymean$mean*100,2),
                        'SE' = round(svymean$SE*100,2) )
    return(model)
  } else {
    
    Covariates<-as.formula(paste0("~",x))
    svymean<-as.data.frame(svymean(Covariates,rhcSvy_DM, na.rm = F))
    colnames(svymean)[2]<-"SE"
    model <- data.frame('Covariates'=x,
                        'grade' ="Mean ± SE",
                        'counts'=' ',
                        'Mean' =round(svymean$mean,2),
                        'SE' = round(svymean$SE,2))
    return(model)
  }
}  

VAR_ALL<-c("Age","Age_status","Gender","Race_ethnicity","Education_levels","PIR",
           "Health_insurance","Smoking_status","Drinking_status","Physical_status","HEI",
           "BMI","BMI_status","HTN_status","HPL_status","CVD_status","CKD_status","Cancer_status","family_history",
           "HbA1c","HbA1c_status","diabetes_duration","duration_status","medicine_use")
all_data<- ldply(lapply(VAR_ALL, model))
#=DM_PD===========================
rhcSvy_DM_noPD<-subset(rhcSvy,T2D=="T2D"&Periodontitis=="No/Mild periodontitis")
model<- function(x){
  
  if( x %in% var ) {
    Covariates<-as.formula(paste0("~",x))
    unwtd_count<-svyby(Covariates,Covariates,rhcSvy_DM_noPD,unwtd.count,drop.empty.groups=F) 
    svymean<-as.data.frame(svymean(Covariates,rhcSvy_DM_noPD, na.rm = F))
    model <- data.frame('Covariates'=x,
                        'grade' = gsub(x,"",rownames(svymean)),
                        'counts'=unwtd_count[2],
                        'Mean' = round(svymean$mean*100,2),
                        'SE' = round(svymean$SE*100,2) )
    return(model)
  } else {
    
    Covariates<-as.formula(paste0("~",x))
    svymean<-as.data.frame(svymean(Covariates,rhcSvy_DM_noPD, na.rm = F))
    colnames(svymean)[2]<-"SE"
    model <- data.frame('Covariates'=x,
                        'grade' ="Mean ± SE",
                        'counts'=' ',
                        'Mean' =round(svymean$mean,2),
                        'SE' = round(svymean$SE,2))
    return(model)
  }
}  
noPD_data<- ldply(lapply(VAR_ALL, model))

rhcSvy_DM_PD<-subset(rhcSvy,T2D=="T2D"&Periodontitis=="Moderate/Severe periodontitis")
model<- function(x){
  
  if( x %in% var ) {
    Covariates<-as.formula(paste0("~",x))
    unwtd_count<-svyby(Covariates,Covariates,rhcSvy_DM_PD,unwtd.count) 
    svymean<-as.data.frame(svymean(Covariates,rhcSvy_DM_PD, na.rm = TRUE))
    model <- data.frame('Covariates'=x,
                        'grade' = gsub(x,"",rownames(svymean)),
                        'counts'=unwtd_count[2],
                        'Mean' = round(svymean$mean*100,2),
                        'SE' = round(svymean$SE*100,2) )
    return(model)
  } else {
    
    Covariates<-as.formula(paste0("~",x))
    svymean<-as.data.frame(svymean(Covariates,rhcSvy_DM_PD, na.rm = TRUE))
    colnames(svymean)[2]<-"SE"
    model <- data.frame('Covariates'=x,
                        'grade' ="Mean ± SE",
                        'counts'=' ',
                        'Mean' =round(svymean$mean,2),
                        'SE' = round(svymean$SE,2))
    return(model)
  }
}  
PD_data<- ldply(lapply(VAR_ALL, model))
total_data<-cbind(all_data,noPD_data[,c("counts","Mean","SE")],PD_data[,c("counts","Mean","SE")])
save(total_data,file = "./TABLE/Table1S_Rdata")

#t-test and chi-test
model<- function(x){
  
  if( x %in% var ) {
    formula<-as.formula(paste0("~",x,"+Periodontitis"))
    chi_test<-svychisq(formula,rhcSvy_DM)
    model <- data.frame('Covariates'=x,
                        'P value' =chi_test[["p.value"]])
    return(model)
  } else {
    formula<-as.formula(paste0(x,"~Periodontitis"))
    t_test<-svyttest(formula,rhcSvy_DM)
    model <- data.frame('Covariates'=x,
                        'P value' =t_test[["p.value"]])
    return(model)
  }
}  
test_data<- ldply(lapply(VAR_ALL, model))
test_data$P.value<-round(test_data$P.value,3)
test_data$P.value[test_data$P.value==0]<-"<0.001"
new.function <- function(x){
  while(nchar(x)<5){
    temp <- paste(x,0)
    x <- temp
    x <- gsub(" ","",x)
  }
  return(x)
}
test_data$P.value<-lapply(test_data$P.value,new.function)
test_data$P.value<-as.character(test_data$P.value)

load(file = "./TABLE/Table1S_Rdata")
data.all<-merge(total_data,test_data,by="Covariates",all = T)
write.table(data.all,sep = ",",file ="./TABLE/Table1S.csv")


#RCS
load(file="./Survey/ALL_Weight_plus_CA.Rdata")
library(foreign)
library(rms)
dd <- datadist(ALL_Weight_plus) #为后续程序设定数据环境
options(datadist='dd') #为
colnames(ALL_Weight_plus)
table(ALL_Weight_plus$Periodontitis)
ALL_Weight_plus_pd<-subset(ALL_Weight_plus,Periodontitis=="No/Mild periodontitis")
fit<- cph(Surv(peryear,CVD_MORT_status==1) ~ rcs(HbA1c,5)+Age_status+Gender+
            Race_ethnicity+Education_levels+PIR+Health_insurance+
            Smoking_status+Drinking_status+Physical_status+HEI+
            BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+duration_status+medicine_use ,data=ALL_Weight_plus_pd) 
an<-anova(fit)
plot(Predict(fit, HbA1c,fun=exp), anova=an, pval=T)
HR<-Predict(fit, HbA1c,fun=exp,ref.zero = TRUE) 
ggplot(HR)
ggplot()+geom_line(data=HR, aes(HbA1c,yhat),linetype=1,size=1,alpha = 0.9,colour="red")+
  geom_ribbon(data=HR, aes(HbA1c,ymin = lower, ymax = upper),alpha = 0.3,fill="red")+
  geom_hline(yintercept=1, linetype=2,size=1)+theme_classic()+ 
  labs(title = "RCS", x="HbA1c", y="HR (95%CI)")
ALL_Weight_plus_pd<-subset(ALL_Weight_plus,Periodontitis=="Moderate/Severe periodontitis")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,strata=~sdmvstra,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
fit<-svycoxph(Surv(peryear,CVD_MORT_status==1) ~ rcs(HbA1c,3)+Age_status+Gender+
            Race_ethnicity+Education_levels+PIR+Health_insurance+
            Smoking_status+Drinking_status+Physical_status+HEI+
            BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+duration_status+medicine_use ,design=rhcSvy_DM) 
class(fit)<-"coxph"
an<-anova(fit)
Predict(fit)
HR1<-Predict(fit, HbA1c,fun=exp,ref.zero = TRUE) 
ggplot()+geom_line(data=HR1, aes(HbA1c,yhat),linetype=1,size=1,alpha = 0.9,colour="red")+
  geom_ribbon(data=HR1, aes(HbA1c,ymin = lower, ymax = upper),alpha = 0.3,fill="red")+
  geom_hline(yintercept=1, linetype=2,size=1)+theme_classic()+ 
  labs(title = "RCS", x="HbA1c", y="HR (95%CI)")



#
colnames(ALL_Weight_plus)
load(file="./Survey/ALL_Weight_plus_CA.Rdata")
table(ALL_Weight_plus$Year)
ALL_Weight_plus<-subset(ALL_Weight_plus,Year=='1988-1991'|Year=="1991-1994")
rhcSvy <- svydesign(id = ~sdmvpsu,nest = TRUE, data =ALL_Weight_plus,weights = ~ weight)
rhcSvy_DM<-subset(rhcSvy,T2D=="T2D")
#all model1
library(survey)
model3_all<-svycoxph(Surv(peryear, MORT_status==1) ~Periodontitis+Age_status+Gender+
                       Race_ethnicity+Education_levels+PIR+Health_insurance+
                       Smoking_status+Drinking_status+Physical_status+HEI+
                       BMI_status+HTN_status+HPL_status+CKD_status+Cancer_status+CVD_status+
                       HbA1c_status+duration_status+medicine_use+family_history, design =rhcSvy_DM)
summary(model1_all)
P<-model1_all_result[["coefficients"]][1,"Pr(>|z|)"]
HR<-as.numeric(model1_all_result[["conf.int"]][1,c("exp(coef)","lower .95","upper .95")])
result <- data.frame('HR'=HR[1],'lower .95'=HR[2],'upper .95'=HR[3],
                     'P value' =P,'model'="model1",'status'="All cause")
summary(ALL_Weight_plus$peryear)
table(ALL_Weight_plus$DM_MORT_status)
sum(ALL_Weight_plus$peryear)
