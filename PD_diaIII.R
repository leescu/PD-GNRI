#===================PERIODONTAL data==============================================
PD_dia=read.table("./exam.dat",sep=",",fill=T)
PD_dia$SEQN<-substring(PD_dia$V1,1,5)
PD_dia$PPD_01_M<-as.numeric(substring(PD_dia$V1,2445,2446))
PD_dia$PPD_01_B<-as.numeric(substring(PD_dia$V1,2449,2450))
PD_dia$PPD_02_M<-as.numeric(substring(PD_dia$V1,2453,2454))
PD_dia$PPD_02_B<-as.numeric(substring(PD_dia$V1,2457,2458))
PD_dia$PPD_03_M<-as.numeric(substring(PD_dia$V1,2462,2463))
PD_dia$PPD_03_B<-as.numeric(substring(PD_dia$V1,2467,2468))
PD_dia$PPD_04_M<-as.numeric(substring(PD_dia$V1,2471,2472))
PD_dia$PPD_04_B<-as.numeric(substring(PD_dia$V1,2475,2476))
PD_dia$PPD_05_M<-as.numeric(substring(PD_dia$V1,2479,2480))
PD_dia$PPD_05_B<-as.numeric(substring(PD_dia$V1,2483,2484))
PD_dia$PPD_06_M<-as.numeric(substring(PD_dia$V1,2488,2489))
PD_dia$PPD_06_B<-as.numeric(substring(PD_dia$V1,2493,2494))
PD_dia$PPD_07_M<-as.numeric(substring(PD_dia$V1,2498,2499))
PD_dia$PPD_07_B<-as.numeric(substring(PD_dia$V1,2503,2504))

PD_dia$PPD_11_M<-as.numeric(substring(PD_dia$V1,2507,2508))
PD_dia$PPD_11_B<-as.numeric(substring(PD_dia$V1,2512,2513))
PD_dia$PPD_12_M<-as.numeric(substring(PD_dia$V1,2516,2517))
PD_dia$PPD_12_B<-as.numeric(substring(PD_dia$V1,2521,2522))
PD_dia$PPD_13_M<-as.numeric(substring(PD_dia$V1,2526,2527))
PD_dia$PPD_13_B<-as.numeric(substring(PD_dia$V1,2531,2532))
PD_dia$PPD_14_M<-as.numeric(substring(PD_dia$V1,2536,2537))
PD_dia$PPD_14_B<-as.numeric(substring(PD_dia$V1,2541,2542))
PD_dia$PPD_15_M<-as.numeric(substring(PD_dia$V1,2545,2546))
PD_dia$PPD_15_B<-as.numeric(substring(PD_dia$V1,2549,2550))
PD_dia$PPD_16_M<-as.numeric(substring(PD_dia$V1,2554,2555))
PD_dia$PPD_16_B<-as.numeric(substring(PD_dia$V1,2559,2560))
PD_dia$PPD_17_M<-as.numeric(substring(PD_dia$V1,2564,2565))
PD_dia$PPD_17_B<-as.numeric(substring(PD_dia$V1,2569,2570))

#CAL
PD_dia$CAL_01_M<-as.numeric(substring(PD_dia$V1,2571,2572))
PD_dia$CAL_01_B<-as.numeric(substring(PD_dia$V1,2573,2574))
PD_dia$CAL_02_M<-as.numeric(substring(PD_dia$V1,2575,2576))
PD_dia$CAL_02_B<-as.numeric(substring(PD_dia$V1,2577,2578))
PD_dia$CAL_03_M<-as.numeric(substring(PD_dia$V1,2579,2580))
PD_dia$CAL_03_B<-as.numeric(substring(PD_dia$V1,2581,2582))
PD_dia$CAL_04_M<-as.numeric(substring(PD_dia$V1,2583,2584))
PD_dia$CAL_04_B<-as.numeric(substring(PD_dia$V1,2585,2586))
PD_dia$CAL_05_M<-as.numeric(substring(PD_dia$V1,2587,2588))
PD_dia$CAL_05_B<-as.numeric(substring(PD_dia$V1,2589,2590))
PD_dia$CAL_06_M<-as.numeric(substring(PD_dia$V1,2591,2592))
PD_dia$CAL_06_B<-as.numeric(substring(PD_dia$V1,2593,2594))
PD_dia$CAL_07_M<-as.numeric(substring(PD_dia$V1,2595,2596))
PD_dia$CAL_07_B<-as.numeric(substring(PD_dia$V1,2597,2598))

PD_dia$CAL_11_M<-as.numeric(substring(PD_dia$V1,2599,2600))
PD_dia$CAL_11_B<-as.numeric(substring(PD_dia$V1,2601,2602))
PD_dia$CAL_12_M<-as.numeric(substring(PD_dia$V1,2603,2604))
PD_dia$CAL_12_B<-as.numeric(substring(PD_dia$V1,2605,2606))
PD_dia$CAL_13_M<-as.numeric(substring(PD_dia$V1,2607,2608))
PD_dia$CAL_13_B<-as.numeric(substring(PD_dia$V1,2609,2610))
PD_dia$CAL_14_M<-as.numeric(substring(PD_dia$V1,2611,2612))
PD_dia$CAL_14_B<-as.numeric(substring(PD_dia$V1,2613,2614))
PD_dia$CAL_15_M<-as.numeric(substring(PD_dia$V1,2615,2616))
PD_dia$CAL_15_B<-as.numeric(substring(PD_dia$V1,2617,2618))
PD_dia$CAL_16_M<-as.numeric(substring(PD_dia$V1,2619,2620))
PD_dia$CAL_16_B<-as.numeric(substring(PD_dia$V1,2621,2622))
PD_dia$CAL_17_M<-as.numeric(substring(PD_dia$V1,2623,2624))
PD_dia$CAL_17_B<-as.numeric(substring(PD_dia$V1,2625,2626))


# #tooth condition
# PD_dia$tooth_11<-as.numeric(substring(PD_dia$V1,2187,2188))
# PD_dia$tooth_12<-as.numeric(substring(PD_dia$V1,2189,2190))
# PD_dia$tooth_13<-as.numeric(substring(PD_dia$V1,2191,2192))
# PD_dia$tooth_14<-as.numeric(substring(PD_dia$V1,2193,2194))
# PD_dia$tooth_15<-as.numeric(substring(PD_dia$V1,2195,2196))
# PD_dia$tooth_16<-as.numeric(substring(PD_dia$V1,2197,2198))
# PD_dia$tooth_17<-as.numeric(substring(PD_dia$V1,2199,2200))
# 
# PD_dia$tooth_21<-as.numeric(substring(PD_dia$V1,2202,2203))
# PD_dia$tooth_22<-as.numeric(substring(PD_dia$V1,2204,2205))
# PD_dia$tooth_23<-as.numeric(substring(PD_dia$V1,2206,2207))
# PD_dia$tooth_24<-as.numeric(substring(PD_dia$V1,2208,2209))
# PD_dia$tooth_25<-as.numeric(substring(PD_dia$V1,2210,2211))
# PD_dia$tooth_26<-as.numeric(substring(PD_dia$V1,2212,2213))
# PD_dia$tooth_27<-as.numeric(substring(PD_dia$V1,2214,2215))
# 
# PD_dia$tooth_31<-as.numeric(substring(PD_dia$V1,2217,2218))
# PD_dia$tooth_32<-as.numeric(substring(PD_dia$V1,2219,2220))
# PD_dia$tooth_33<-as.numeric(substring(PD_dia$V1,2221,2222))
# PD_dia$tooth_34<-as.numeric(substring(PD_dia$V1,2223,2224))
# PD_dia$tooth_35<-as.numeric(substring(PD_dia$V1,2225,2226))
# PD_dia$tooth_36<-as.numeric(substring(PD_dia$V1,2227,2228))
# PD_dia$tooth_37<-as.numeric(substring(PD_dia$V1,2229,2230))
# 
# 
# PD_dia$tooth_41<-as.numeric(substring(PD_dia$V1,2232,2233))
# PD_dia$tooth_42<-as.numeric(substring(PD_dia$V1,2234,2235))
# PD_dia$tooth_43<-as.numeric(substring(PD_dia$V1,2236,2237))
# PD_dia$tooth_44<-as.numeric(substring(PD_dia$V1,2238,2239))
# PD_dia$tooth_45<-as.numeric(substring(PD_dia$V1,2240,2241))
# PD_dia$tooth_46<-as.numeric(substring(PD_dia$V1,2242,2243))
# PD_dia$tooth_47<-as.numeric(substring(PD_dia$V1,2244,2245))

PD_dia<-PD_dia[,-1]
PD_dia[PD_dia=="  "]<-NA

colnames<-colnames(PD_dia)
rownames(PD_dia)<-PD_dia$SEQN
CAL<-grep('CAL.', colnames, value = T)
PPD<-grep('PPD.', colnames, value = T)
tooth<-grep('tooth.', colnames, value = T)
CAL<-PD_dia[,CAL]
PPD<-PD_dia[,PPD]

CAL_99<-CAL
CAL_99[CAL_99!=99]<-0
CAL_99[is.na(CAL_99)] <- 1
CAL_99[CAL_99=="NA"]<-1
CAL_99[CAL_99==99]<-1
colnames<-colnames(CAL_99)
#Sum the points equal to 99 in each tooth position. If it is greater than 1,
#it indicates that there are points that cannot be measured
#Group according to tooth position (extract the number of tooth position)
tooth<-colnames(CAL)
toothnumber<-substr(tooth,5,6)
CAL99<-t(CAL_99)
#Sum according to tooth position

CAL_TOOTH_99<-rowsum(CAL99, toothnumber,na.rm = F)
#According to the tooth position, the teeth with missing data
#are recorded as 1 and those without are recorded as 0
CAL_TOOTH_99[CAL_TOOTH_99<2]<-0
CAL_TOOTH_99[CAL_TOOTH_99==2]<-1
CALTOOTH_99<-t(CAL_TOOTH_99)
#Sum the number of missing teeth in each record
CAL_99number<-rowSums(CALTOOTH_99,na.rm = T)
CAL_99number<-data.frame(CAL_99number)

#Merge the missing teeth data into the summary table
tooth_count<-data.frame(rownames(CAL_99),CAL_99number)
colnames(tooth_count)<-c("SEQN","tooth_count")

CAL_4<-CAL
CAL_4[CAL_4==99]<-NA
CAL_4[CAL_4<4]<-0
CAL_4[CAL_4>=4]<-1
colnames<-colnames(CAL_4)
#Sum the sites of each tooth position CAL>=4. 
#If it is greater than 1, there are point CAL>=4
CAL4<-t(CAL_4)
CAL_TOOTH4<-rowsum(CAL4, toothnumber,na.rm = T)
#According to the tooth position, the teeth with CAL>=4are recorded as 1 
#and those without CAL>=4 are recorded as 0
CAL_TOOTH4[CAL_TOOTH4<1]<-0
CAL_TOOTH4[CAL_TOOTH4>=1]<-1
CAL_TOOTH41<-t(CAL_TOOTH4)
#For teeth with CAL>=4, if it is greater than 2, 
#there are more than 2 teeth with CAL>=4
CAL_4number<-rowSums(CAL_TOOTH41,na.rm = T)
CAL_4number<-data.frame(CAL_4number)
CAL_4number$SEQN<-rownames(CAL)
#Merge the CAL>=4 data into the summary table
OHXPER.CAL_4<-merge(tooth_count,CAL_4number,by="SEQN",all = T)
PPD_5<-PPD
PPD_5[PPD_5==99]<-NA
#Set the point with ppd>=5-> 1 and the point ppd<5->0
PPD_5[PPD_5<5]<-0
PPD_5[PPD_5>=5]<-1
colnames<-colnames(PPD_5)
#Sum the sites of each tooth position ppd>=5. 
#If it is greater than 1, there are point ppd>=5
tooth_PPD<-colnames(PPD)
toothnumber_PPD<-substr(tooth_PPD,5,6)
PPD51<-t(PPD_5)
PPD_TOOTH5<-rowsum(PPD51, toothnumber_PPD,na.rm = T)
#According to the tooth position, the teeth with PPD>=5are recorded as 1 
#and those without PPD>=5 are recorded as 0
PPD_TOOTH5[PPD_TOOTH5<1]<-0
PPD_TOOTH5[PPD_TOOTH5>=1]<-1
PPD_TOOTH51<-t(PPD_TOOTH5)
#For teeth with PPD>=5, if it is greater than 2, 
#there are more than 2 teeth with PPD>=5
PPD_5number<-rowSums(PPD_TOOTH51,na.rm = T)
PPD_5number<-data.frame(PPD_5number)
#Merge the PPD>=5 data into the summary table
PPD_5number$SEQN<-rownames(PPD_5)
PD_data<-merge(OHXPER.CAL_4,PPD_5number,by="SEQN",all = T)
PD_data$PD_dia<-NA
PD_data$PD_dia[PD_data$tooth_count<14&(PD_data$PPD_5number>=1|PD_data$CAL_4number>=1)]<-"Moderate/Severe periodontitis"
PD_data$PD_dia[PD_data$tooth_count<14&(PD_data$PPD_5number<1&PD_data$CAL_4number<1)]<-"No/Mild periodontitis"
PD_data$PD_dia[PD_data$tooth_count==14]<-NA
PD_data<-na.omit(PD_data)
table(PD_data$PD_dia)

PD_data$SEQN<-as.numeric(PD_data$SEQN)
adult=read.table("./adult.dat",sep=",",fill=T)
adult$SEQN<-as.numeric(substring(adult$V1,1,5))
age<-as.data.frame(adult$SEQN)
colnames(age)<-c("SEQN")
age$ageyr<-as.numeric(substring(adult$V1,18,19))
age$ageyr[age$ageyr<20]<-NA
age<-na.omit(age)
PD_dia_20<-merge(PD_data,age,by="SEQN",all = F)
PD_dia_20$SEQN<-as.numeric(PD_dia_20$SEQN)
PD_dia_20<-PD_dia_20[order(PD_dia_20$SEQN),]
PD_III<-PD_dia_20[,c("SEQN","PD_dia")]
PD_III$chort<-"NHANES_III"
PD_III$ID<-paste(PD_III$chort,PD_III$SEQN,sep="_")
PD_III<-PD_III[,c("ID","SEQN","PD_dia")]
colnames(PD_III)[3]<-"Periodontitis"
record<-ls()
rm(list=record[which(record!='PD_III')])
table(PD_III$Periodontitis)
