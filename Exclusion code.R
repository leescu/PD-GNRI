library(nhanesR)
DM_CON<-diag_DM(told = T,HbA1c = T,fast_glu = T,OGTT2 = T,
                rand_glu = T,drug = T,DM1 = F,cat = T,
                years = 1999:2004,join = "left")
DM_CON$DM[DM_CON$DM=="IFG"]<-'No T2D'
DM_CON$DM[DM_CON$DM=="no"]<-'No T2D'
DM_CON$DM[DM_CON$DM=="DM"]<-'T2D'
colnames(DM_CON)<-c("SEQN","T2D")


#CVD
CVD_CON<-diag_CVD(years = 1999:2004,join = "left")
table(CVD_CON$CVD)
CVD_CON$CVD[CVD_CON$CVD=="no"]<-"NO"
CVD_CON$CVD[CVD_CON$CVD=="yes"]<-"YES"
#Cancer
tsv<-nhs_tsv('mcq',years = 1999:2004)
Cancer_CON<-nhs_read(tsv,'mcq220:Cancer')
Cancer_CON$Cancer[Cancer_CON$Cancer=="Yes"]<-"YES"
Cancer_CON$Cancer[Cancer_CON$Cancer=="No"]<-"NO"
table(Cancer_CON$Cancer)
#Pregnancy
tsv<-nhs_tsv('demo',years = 1999:2004)
Pregnancy_CON<-nhs_read(tsv,'ridexprg:Pregnancy')
table(Pregnancy_CON$Pregnancy)
Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy=="Cannot ascertain if SP is pregnant at exam"]<-NA
Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy=="Yes, positive lab pregnancy test or self-reported pregnant at exam"]<-"YES"
Pregnancy_CON$Pregnancy[Pregnancy_CON$Pregnancy=="SP not pregnant at exam"]<-"NO"
EX_CON<-list(CVD_CON, Cancer_CON, Pregnancy_CON) %>% reduce(full_join, by = "seqn")
colnames(EX_CON)
EX_CON<-EX_CON[,c("seqn","CVD","Cancer","Pregnancy")]
colnames(EX_CON)[1]<-"SEQN"
record<-ls()
rm(list=record[-which(record==c('EX_CON'))])
