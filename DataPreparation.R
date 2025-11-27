##########################################################
## STUDY 3: Family-based redistribution and preferences ##
##########################################################

## 2016 DATA

####
## SCRIPT 1: DATA PREPARATION
####

library(readr)
library(tidyr)
library(dplyr)
library(foreign)
library(readxl)
library(forcats)

setwd("D:/BIGSSS/Dissertation/Study 3/Scripts/2016Data")

ESS16<-read.spss("ESS8e02_2.sav") # Main questionnaire variables
ESS16<-rbind.data.frame(ESS16)

## Preparing key variables

# Country
ESS16$cntry<-as.factor(ESS16$cntry)

# Remove countries from analysis
ESS16<-ESS16[c(which(ESS16$cntry=="Austria"),which(ESS16$cntry=="Belgium"),
               which(ESS16$cntry=="France"),which(ESS16$cntry=="Germany"),which(ESS16$cntry=="Netherlands"),
               which(ESS16$cntry=="Ireland"),which(ESS16$cntry=="United Kingdom"),
               which(ESS16$cntry=="Finland"),which(ESS16$cntry=="Norway"),which(ESS16$cntry=="Sweden"),
               which(ESS16$cntry=="Spain"),which(ESS16$cntry=="Italy"),which(ESS16$cntry=="Portugal"),
               which(ESS16$cntry=="Czechia"),which(ESS16$cntry=="Poland"),which(ESS16$cntry=="Hungary")),]
               
ESS16$cntry<-droplevels(ESS16$cntry)

# Regime
ESS16$regime<-factor(nrow(ESS16),levels=c("Conservative","Liberal","Social-democratic","Southern","Eastern"))
ESS16$regime[which(ESS16$cntry=="Austria" | ESS16$cntry=="Belgium" | ESS16$cntry=="France" |
                   ESS16$cntry=="Germany" | ESS16$cntry=="Netherlands")]="Conservative"
ESS16$regime[which(ESS16$cntry=="Ireland" | ESS16$cntry=="United Kingdom")]="Liberal"
ESS16$regime[which(ESS16$cntry=="Finland" | ESS16$cntry=="Norway" | ESS16$cntry=="Sweden")]="Social-democratic"
ESS16$regime[which(ESS16$cntry=="Spain" | ESS16$cntry=="Italy" | ESS16$cntry=="Portugal")]="Southern"
ESS16$regime[which(ESS16$cntry=="Czechia" | ESS16$cntry=="Poland" | ESS16$cntry=="Hungary")]="Eastern"

# Age
ESS16$agea<-as.numeric(ESS16$agea)
ESS16$agea_10<-ESS16$agea/10 # Here each unit indicates a change in 10 years (it will make estimates easier to see)

ESS16<-ESS16[-which(is.na(ESS16$agea)),]

ESS16$agea_grp<-factor(nrow(ESS16),levels=c("15-24","25-49","50-64","65-79","80 or more"))
ESS16$agea_grp[which(ESS16$agea<25)]="15-24"
ESS16$agea_grp[which(ESS16$agea>=25 & ESS16$agea<=49)]="25-49"
ESS16$agea_grp[which(ESS16$agea>=50 & ESS16$agea<=64)]="50-64"
ESS16$agea_grp[which(ESS16$agea>=65 & ESS16$agea<=79)]="65-79"
ESS16$agea_grp[which(ESS16$agea>=80)]="80 or more"

ESS16$agea_grp2<-factor(nrow(ESS16),levels=c("<30","30-39","40-49","50-59",">=60"))
ESS16$agea_grp2[which(ESS16$agea<30)]="<30"
ESS16$agea_grp2[which(ESS16$agea>=30 & ESS16$agea<40)]="30-39"
ESS16$agea_grp2[which(ESS16$agea>=40 & ESS16$agea<50)]="40-49"
ESS16$agea_grp2[which(ESS16$agea>=50 & ESS16$agea<60)]="50-59"
ESS16$agea_grp2[which(ESS16$agea>=60)]=">=60"

# Gender
ESS16$gndr<-as.factor(ESS16$gndr)

###############################################

# Preferences for redistribution
ESS16$gincdif<-factor(ESS16$gincdif,levels=c("Disagree strongly","Disagree","Neither agree nor disagree",
                                             "Agree","Agree strongly",
                                             "Don't know","No answer","Refusal"))
ESS16$gincdif[which(ESS16$gincdif=="Don't know" | ESS16$gincdif=="No answer" | ESS16$gincdif=="Refusal")]=NA
ESS16$gincdif<-as.ordered(droplevels(ESS16$gincdif))

ESS16$gincdif_num<-as.numeric(ESS16$gincdif)

# Preferences for family-work conciliation policies for working parents
ESS16$wrkprbf<-factor(ESS16$wrkprbf,levels=c("Strongly against","Against","In favour","Strongly in favour",
                                             "Refusal","Don't know","No answer"))
ESS16$wrkprbf[which(ESS16$wrkprbf=="Refusal" | ESS16$wrkprbf=="Don't know" | ESS16$wrkprbf=="No answer")]=NA
ESS16$wrkprbf<-as.ordered(droplevels(ESS16$wrkprbf))

ESS16$wrkprbf_num<-as.numeric(ESS16$wrkprbf)

# Preferences for public child-care services
ESS16$gvcldcr<-factor(ESS16$gvcldcr,levels=c("Not governments' responsibility at all","1","2","3","4",
                                             "5","6","7","8","9","Entirely governments' responsibility",
                                             "No answer","Don't know","Refusal"))
ESS16$gvcldcr[which(ESS16$gvcldcr=="No answer" | ESS16$gvcldcr=="Don't know" | ESS16$gvcldcr=="Refusal")]=NA
ESS16$gvcldcr<-as.ordered(droplevels(ESS16$gvcldcr))
ESS16$gvcldcr_simp<-recode_factor(ESS16$gvcldcr,"Not governments' responsibility at all"="1","1"="1",
                                  "2"="1","3"="2","4"="2","5"="3","6"="3","7"="4","8"="4",
                                  "9"="5","Entirely governments' responsibility"="5")

ESS16$gvcldcr_num<-as.numeric(ESS16$gvcldcr)
ESS16$gvcldcr_simp_num<-as.numeric(ESS16$gvcldcr_simp)

##############################################

# Marital status
ESS16$maritalb<-as.factor(ESS16$maritalb)
ESS16$maritalb[which(ESS16$maritalb=="Don't know")]=NA
ESS16$maritalb[which(ESS16$maritalb=="No answer")]=NA
ESS16$maritalb[which(ESS16$maritalb=="Refusal")]=NA
ESS16$maritalstatus<-droplevels(ESS16$maritalb)

ESS16$maritalstatus_grp<-recode_factor(ESS16$maritalstatus,
                                   "Legally married"="Legally married or in civil union",
                                   "In a legally registered civil union"="Legally married or in civil union",
                                   "Legally separated"="Legally separated or divorced",
                                   "Legally divorced/Civil union dissolved"="Legally separated or divorced",
                                   "Widowed/Civil partner died"="Widowed/Civil partner died",
                                   "None of these (NEVER married or in legally registered civil union)"="Never married or in civil union")

ESS16$maritalstatus_bin<-recode_factor(ESS16$maritalstatus,
                                    "Legally separated"="Not married or in civil union",
                                    "Legally divorced/Civil union dissolved"="Not married or in civil union",
                                    "Widowed/Civil partner died"="Not married or in civil union",
                                    "None of these (NEVER married or in legally registered civil union)"="Not married or in civil union",
                                    "Legally married"="Married or in civil union",
                                    "In a legally registered civil union"="Married or in civil union")

ESS16$divorced_married<-numeric(nrow(ESS16)) # Dummy to compare divorced with married, other categories are NAs
ESS16$divorced_married[which(ESS16$maritalstatus_grp=="Legally married or in civil union")]=0
ESS16$divorced_married[which(ESS16$maritalstatus_grp=="Legally separated or divorced")]=1
ESS16$divorced_married[which(ESS16$maritalstatus_grp=="Widowed/Civil partner died")]=NA
ESS16$divorced_married[which(ESS16$maritalstatus_grp=="Never married or in civil union")]=NA

ESS16$widowed_married<-numeric(nrow(ESS16)) # Dummy to compare widowed with married, other categories are NAs
ESS16$widowed_married[which(ESS16$maritalstatus_grp=="Legally married or in civil union")]=0
ESS16$widowed_married[which(ESS16$maritalstatus_grp=="Legally separated or divorced")]=NA
ESS16$widowed_married[which(ESS16$maritalstatus_grp=="Widowed/Civil partner died")]=1
ESS16$widowed_married[which(ESS16$maritalstatus_grp=="Never married or in civil union")]=NA

ESS16$single_married<-numeric(nrow(ESS16)) # Dummy to compare never married with married, other categories are NAs
ESS16$single_married[which(ESS16$maritalstatus_grp=="Legally married or in civil union")]=0
ESS16$single_married[which(ESS16$maritalstatus_grp=="Legally separated or divorced")]=NA
ESS16$single_married[which(ESS16$maritalstatus_grp=="Widowed/Civil partner died")]=NA
ESS16$single_married[which(ESS16$maritalstatus_grp=="Never married or in civil union")]=1

# Cohabiting partner
ESS16$lwpartner<-factor(nrow(ESS16),levels=c("no","yes"))

for (i in 1:length(ESS16$lwpartner)){
  if ("Husband/wife/partner" %in% c(ESS16$rshipa2[i],ESS16$rshipa3[i],ESS16$rshipa4[i],
               ESS16$rshipa5[i],ESS16$rshipa6[i],ESS16$rshipa7[i],
               ESS16$rshipa8[i],ESS16$rshipa9[i],ESS16$rshipa10[i],
               ESS16$rshipa11[i],ESS16$rshipa12[i])){
    ESS16$lwpartner[i]="yes"
  }
  else{
    ESS16$lwpartner[i]="no"
  }
}                       

shipaindex<-which(colnames(ESS16) %in% c("rshipa2","rshipa3","rshipa4","rshipa5","rshipa6","rshipa7",
                                         "rshipa8","rshipa9","rshipa10","rshipa11","rshipa12"))
gndrindex<-which(colnames(ESS16) %in% c("gndr2","gndr3","gndr4","gndr5","gndr6","gndr7",
                                        "gndr8","gndr9","gndr10","gndr11","gndr12"))
yrbrnindex<-which(colnames(ESS16) %in% c("yrbrn2","yrbrn3","yrbrn4","yrbrn5","yrbrn6","yrbrn7",
                                         "yrbrn8","yrbrn9","yrbrn10","yrbrn11","yrbrn12"))


# Gender of cohabiting partner
ESS16$gndrpartner<-factor(nrow(ESS16),levels=c("Female","Male"))
for (i in 1:nrow(ESS16)){
  if (ESS16$lwpartner[i]=="no"){
    ESS16$gndrpartner[i]=NA
  }
  else{
    ESS16$gndrpartner[i]=ESS16[i,gndrindex[which(ESS16[i,shipaindex]=="Husband/wife/partner")]]
  }
} # There are 10 cases in which respondent has partner but did not give the gender. Remove from sample

ESS16<-ESS16[-which(ESS16$lwpartner=="yes" & is.na(ESS16$gndrpartner)),]

# Age of cohabiting partner (estimated through the year of interview - year of partner's birth)
ESS16$agepartner<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$lwpartner[i]=="no"){
    ESS16$agepartner[i]=NA
  }
  else{
    ESS16$agepartner[i]=as.numeric(ESS16$inwyys[i])-as.numeric(paste(ESS16[i,yrbrnindex[which(ESS16[i,shipaindex]=="Husband/wife/partner")]]))
  }
}

ESS16$agepartner[which(ESS16$agepartner<0)]=NA
# There are 116 cases in which respondent has partner but did not give the age. Remove from sample
ESS16<-ESS16[-which(ESS16$lwpartner=="yes" & is.na(ESS16$agepartner)),]

ESS16$agepartner_grp2<-factor(nrow(ESS16),levels=c("<30","30-39","40-49","50-59",">=60"))
ESS16$agepartner_grp2[which(ESS16$agepartner<30)]="<30"
ESS16$agepartner_grp2[which(ESS16$agepartner>=30 & ESS16$agepartner<40)]="30-39"
ESS16$agepartner_grp2[which(ESS16$agepartner>=40 & ESS16$agepartner<50)]="40-49"
ESS16$agepartner_grp2[which(ESS16$agepartner>=50 & ESS16$agepartner<60)]="50-59"
ESS16$agepartner_grp2[which(ESS16$agepartner>=60)]=">=60"

# Cohabiting (0) vs married (1) couples
ESS16$married_cohabiting<-numeric(nrow(ESS16))
ESS16$married_cohabiting[which(ESS16$lwpartner=="yes" & ESS16$maritalstatus_bin=="Married or in civil union")]=1
ESS16$married_cohabiting[which(ESS16$lwpartner=="yes" & ESS16$maritalstatus_bin=="Not married or in civil union")]=0
ESS16$married_cohabiting[which(ESS16$lwpartner=="no")]=NA
ESS16$married_cohabiting[which(is.na(ESS16$maritalstatus_bin))]=NA

# Single vs Cohabitating vs Married
ESS16$relstatus<-factor(nrow(ESS16),levels=c("Single","Cohabitating","Married"))
ESS16$relstatus[which(ESS16$lwpartner=="no")]="Single"
ESS16$relstatus[which(ESS16$lwpartner=="yes" & ESS16$maritalstatus_bin=="Not married or in civil union")]="Cohabitating"
ESS16$relstatus[which(ESS16$lwpartner=="yes" & ESS16$maritalstatus_bin=="Married or in civil union")]="Married"
ESS16$relstatus[which(ESS16$lwpartner=="yes" & is.na(ESS16$maritalstatus_bin))]=NA

# Number of children (in HH)
ESS16$nchildren<-numeric(nrow(ESS16))

for (i in 1:length(ESS16$nchildren)){
  ESS16$nchildren[i]=length(which(ESS16[i,c("rshipa2","rshipa3","rshipa4","rshipa5","rshipa6","rshipa7","rshipa8",
                                     "rshipa9","rshipa10","rshipa11","rshipa12")]=="Son/daughter/step/adopted/foster"))
}  

ESS16$nchildren_grp<-factor(nrow(ESS16),levels=c("no children","1 child","2 children","3 children or more"),ordered=T)

ESS16$nchildren_grp[which(ESS16$nchildren==0)]="no children"
ESS16$nchildren_grp[which(ESS16$nchildren==1)]="1 child"
ESS16$nchildren_grp[which(ESS16$nchildren==2)]="2 children"
ESS16$nchildren_grp[which(ESS16$nchildren>=3)]="3 children or more"

ESS16$nchildren_grp2<-recode_factor(ESS16$nchildren_grp,"1 child"="1-2 children","2 children"="1-2 children")

ESS16$inwyys<-as.numeric(ESS16$inwyys)
ESS16$inwyys[which(is.na(ESS16$inwyys))]<-2016
ESS16$yrbrn2<-as.numeric(ESS16$yrbrn2)
ESS16$yrbrn2[which(is.na(ESS16$yrbrn2))]<-99999
ESS16$yrbrn3<-as.numeric(ESS16$yrbrn3)
ESS16$yrbrn3[which(is.na(ESS16$yrbrn3))]<-99999
ESS16$yrbrn4<-as.numeric(ESS16$yrbrn4)
ESS16$yrbrn4[which(is.na(ESS16$yrbrn4))]<-99999
ESS16$yrbrn5<-as.numeric(ESS16$yrbrn5)
ESS16$yrbrn5[which(is.na(ESS16$yrbrn5))]<-99999
ESS16$yrbrn6<-as.numeric(ESS16$yrbrn6)
ESS16$yrbrn6[which(is.na(ESS16$yrbrn6))]<-99999
ESS16$yrbrn7<-as.numeric(ESS16$yrbrn7)
ESS16$yrbrn7[which(is.na(ESS16$yrbrn7))]<-99999
ESS16$yrbrn8<-as.numeric(ESS16$yrbrn8)
ESS16$yrbrn8[which(is.na(ESS16$yrbrn8))]<-99999
ESS16$yrbrn9<-as.numeric(ESS16$yrbrn9)
ESS16$yrbrn9[which(is.na(ESS16$yrbrn9))]<-99999
ESS16$yrbrn10<-as.numeric(ESS16$yrbrn10)
ESS16$yrbrn10[which(is.na(ESS16$yrbrn10))]<-99999
ESS16$yrbrn11<-as.numeric(ESS16$yrbrn11)
ESS16$yrbrn11[which(is.na(ESS16$yrbrn11))]<-99999
ESS16$yrbrn12<-as.numeric(ESS16$yrbrn12)
ESS16$yrbrn12[which(is.na(ESS16$yrbrn12))]<-99999


# Ages of minor children in HH
ESS16$hhchildage2<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
if (ESS16$rshipa2[i]=="Son/daughter/step/adopted/foster" & (ESS16$inwyys[i]-ESS16$yrbrn2[i])<18){
  ESS16$hhchildage2[i]=(ESS16$inwyys[i]-ESS16$yrbrn2[i])}
else{
  ESS16$hhchildage2[i]=9999
}
}
ESS16$hhchildage2[which(ESS16$hhchildage2<0 | ESS16$hhchildage2>=18)]=NA

ESS16$hhchildage3<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$rshipa3[i]=="Son/daughter/step/adopted/foster" & (ESS16$inwyys[i]-ESS16$yrbrn3[i])<18){
    ESS16$hhchildage3[i]=(ESS16$inwyys[i]-ESS16$yrbrn3[i])}
  else{
    ESS16$hhchildage3[i]=9999
  }
}
ESS16$hhchildage3[which(ESS16$hhchildage3<0 | ESS16$hhchildage3>=18)]=NA

ESS16$hhchildage4<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$rshipa4[i]=="Son/daughter/step/adopted/foster" & (ESS16$inwyys[i]-ESS16$yrbrn4[i])<18){
    ESS16$hhchildage4[i]=(ESS16$inwyys[i]-ESS16$yrbrn4[i])}
  else{
    ESS16$hhchildage4[i]=9999
  }
}
ESS16$hhchildage4[which(ESS16$hhchildage4<0 | ESS16$hhchildage4>=18)]=NA

ESS16$hhchildage5<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$rshipa5[i]=="Son/daughter/step/adopted/foster" & (ESS16$inwyys[i]-ESS16$yrbrn5[i])<18){
    ESS16$hhchildage5[i]=(ESS16$inwyys[i]-ESS16$yrbrn5[i])}
  else{
    ESS16$hhchildage5[i]=9999
  }
}
ESS16$hhchildage5[which(ESS16$hhchildage5<0 | ESS16$hhchildage5>=18)]=NA

ESS16$hhchildage6<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$rshipa6[i]=="Son/daughter/step/adopted/foster" & (ESS16$inwyys[i]-ESS16$yrbrn6[i])<18){
    ESS16$hhchildage6[i]=(ESS16$inwyys[i]-ESS16$yrbrn6[i])}
  else{
    ESS16$hhchildage6[i]=9999
  }
}
ESS16$hhchildage6[which(ESS16$hhchildage6<0 | ESS16$hhchildage6>=18)]=NA

ESS16$rshipa7[which(is.na(ESS16$rshipa7))]=9999
ESS16$yrbrn7[which(is.na(ESS16$yrbrn7))]=9999
ESS16$hhchildage7<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$rshipa7[i]=="Son/daughter/step/adopted/foster" & (ESS16$inwyys[i]-ESS16$yrbrn7[i])<18){
    ESS16$hhchildage7[i]=(ESS16$inwyys[i]-ESS16$yrbrn7[i])}
  else{
    ESS16$hhchildage7[i]=9999
  }
}
ESS16$hhchildage7[which(ESS16$hhchildage7<0 | ESS16$hhchildage7>=18)]=NA

ESS16$rshipa8[which(is.na(ESS16$rshipa8))]=9999
ESS16$yrbrn8[which(is.na(ESS16$yrbrn8))]=9999
ESS16$hhchildage8<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$rshipa8[i]=="Son/daughter/step/adopted/foster" & (ESS16$inwyys[i]-ESS16$yrbrn8[i])<18){
    ESS16$hhchildage8[i]=(ESS16$inwyys[i]-ESS16$yrbrn8[i])}
  else{
    ESS16$hhchildage8[i]=9999
  }
}
ESS16$hhchildage8[which(ESS16$hhchildage8<0 | ESS16$hhchildage8>=18)]=NA

ESS16$rshipa9[which(is.na(ESS16$rshipa9))]=9999
ESS16$yrbrn9[which(is.na(ESS16$yrbrn9))]=9999
ESS16$hhchildage9<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$rshipa9[i]=="Son/daughter/step/adopted/foster" & (ESS16$inwyys[i]-ESS16$yrbrn9[i])<18){
    ESS16$hhchildage9[i]=(ESS16$inwyys[i]-ESS16$yrbrn9[i])}
  else{
    ESS16$hhchildage9[i]=9999
  }
}
ESS16$hhchildage9[which(ESS16$hhchildage9<0 | ESS16$hhchildage9>=18)]=NA

ESS16$rshipa10[which(is.na(ESS16$rshipa10))]=9999
ESS16$yrbrn10[which(is.na(ESS16$yrbrn10))]=9999
ESS16$hhchildage10<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$rshipa10[i]=="Son/daughter/step/adopted/foster" & (ESS16$inwyys[i]-ESS16$yrbrn10[i])<18){
    ESS16$hhchildage10[i]=(ESS16$inwyys[i]-ESS16$yrbrn10[i])}
  else{
    ESS16$hhchildage10[i]=9999
  }
}
ESS16$hhchildage10[which(ESS16$hhchildage10<0 | ESS16$hhchildage10>=18)]=NA

ESS16$rshipa11[which(is.na(ESS16$rshipa11))]=9999
ESS16$yrbrn11[which(is.na(ESS16$yrbrn11))]=9999
ESS16$hhchildage11<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$rshipa11[i]=="Son/daughter/step/adopted/foster" & (ESS16$inwyys[i]-ESS16$yrbrn11[i])<18){
    ESS16$hhchildage11[i]=(ESS16$inwyys[i]-ESS16$yrbrn11[i])}
  else{
    ESS16$hhchildage11[i]=9999
  }
}
ESS16$hhchildage11[which(ESS16$hhchildage11<0 | ESS16$hhchildage11>=18)]=NA

ESS16$rshipa12[which(is.na(ESS16$rshipa12))]=9999
ESS16$yrbrn12[which(is.na(ESS16$yrbrn12))]=9999
ESS16$hhchildage12<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$rshipa12[i]=="Son/daughter/step/adopted/foster" & (ESS16$inwyys[i]-ESS16$yrbrn12[i])<18){
    ESS16$hhchildage12[i]=(ESS16$inwyys[i]-ESS16$yrbrn12[i])}
  else{
    ESS16$hhchildage12[i]=9999
  }
}
ESS16$hhchildage12[which(ESS16$hhchildage12<0 | ESS16$hhchildage12>=18)]=NA

# Number of minor children in HH
ESS16$nmchildren<-numeric(nrow(ESS16))

for (i in 1:nrow(ESS16)){
  ESS16$nmchildren[i]<-11-length(which(is.na(c(ESS16$hhchildage2[i],ESS16$hhchildage3[i],ESS16$hhchildage4[i],
                                               ESS16$hhchildage5[i],ESS16$hhchildage6[i],ESS16$hhchildage7[i],
                                               ESS16$hhchildage8[i],ESS16$hhchildage9[i],ESS16$hhchildage10[i],
                                               ESS16$hhchildage11[i],ESS16$hhchildage12[i]))))
}

ESS16$nmchildren_grp<-factor(nrow(ESS16),levels=c("no children","1 child","2 children","3 children or more"),ordered=T)

ESS16$nmchildren_grp[which(ESS16$nmchildren==0)]="no children"
ESS16$nmchildren_grp[which(ESS16$nmchildren==1)]="1 child"
ESS16$nmchildren_grp[which(ESS16$nmchildren==2)]="2 children"
ESS16$nmchildren_grp[which(ESS16$nmchildren>=3)]="3 children or more"

ESS16$nmchildren_grp2<-recode_factor(ESS16$nmchildren_grp,"1 child"="1-2 children","2 children"="1-2 children")
ESS16$nmchildren_grp2<-factor(ESS16$nmchildren_grp2,levels=c("no children","1-2 children","3 children or more"))

ESS16$hasmkids_bin<-recode_factor(ESS16$nmchildren_grp,"no children"="no","1 child"="yes","2 children"="yes","3 children or more"="yes")

# Age of youngest child
ESS16$age_ychild_num<-numeric(nrow(ESS16))

for (i in 1:nrow(ESS16)){
  if (ESS16$nmchildren_grp[i]=="no children"){
    ESS16$age_ychild_num[i]=9999}
  if (ESS16$nmchildren_grp[i]!="no children"){
    ESS16$age_ychild_num[i]=min(na.omit(c(ESS16$hhchildage2[i],
                                    ESS16$hhchildage3[i],
                                    ESS16$hhchildage4[i],
                                    ESS16$hhchildage5[i],
                                    ESS16$hhchildage6[i],
                                    ESS16$hhchildage7[i],
                                    ESS16$hhchildage8[i],
                                    ESS16$hhchildage9[i],
                                    ESS16$hhchildage10[i],
                                    ESS16$hhchildage11[i],
                                    ESS16$hhchildage12[i])))
  }}

ESS16$age_ychild<-factor(nrow(ESS16),levels=c("none","youngest under 3","youngest 3 to 5","youngest 6 to 11","youngest 12 to 17"))
ESS16$age_ychild[which(ESS16$age_ychild_num==9999)]="none"
ESS16$age_ychild[which(ESS16$age_ychild_num<3)]="youngest under 3"
ESS16$age_ychild[which(ESS16$age_ychild_num>=3 & ESS16$age_ychild_num<=5)]="youngest 3 to 5"
ESS16$age_ychild[which(ESS16$age_ychild_num>=6 & ESS16$age_ychild_num<=11)]="youngest 6 to 11"
ESS16$age_ychild[which(ESS16$age_ychild_num>=12 & ESS16$age_ychild_num<=17)]="youngest 12 to 17"

# Combinations of parenthood and partnership
ESS16$parentpartner<-factor(nrow(ESS16),levels=c("Single no children","Single parent","Couple no children","Couple parents"))
ESS16$parentpartner[which(ESS16$lwpartner=="no" & ESS16$nmchildren==0)]="Single no children"
ESS16$parentpartner[which(ESS16$lwpartner=="no" & ESS16$nmchildren>0)]="Single parent"
ESS16$parentpartner[which(ESS16$lwpartner=="yes" & ESS16$nmchildren==0)]="Couple no children"
ESS16$parentpartner[which(ESS16$lwpartner=="yes" & ESS16$nmchildren>0)]="Couple parents"

# A dummy to compare single parents with parents in partnership (NAs for respondents without children)
ESS16$singleparent<-numeric(nrow(ESS16))
ESS16$singleparent[which(ESS16$lwpartner=="no" & ESS16$nmchildren==0)]=NA
ESS16$singleparent[which(ESS16$lwpartner=="no" & ESS16$nmchildren>0)]=1
ESS16$singleparent[which(ESS16$lwpartner=="yes" & ESS16$nmchildren==0)]=NA
ESS16$singleparent[which(ESS16$lwpartner=="yes" & ESS16$nmchildren>0)]=0

# HH income
ESS16$hinctnta_factor<-factor(ESS16$hinctnta,levels=c("J - 1st decile",
                                                      "R - 2nd decile",
                                                      "C - 3rd decile",
                                                      "M - 4th decile",
                                                      "F - 5th decile",
                                                      "S - 6th decile",
                                                      "K - 7th decile",
                                                      "P - 8th decile",
                                                      "D - 9th decile",
                                                      "H - 10th decile",
                                                      "Don't know","No answer","Refusal"))
ESS16$hinctnta_factor[which(ESS16$hinctnta_factor=="Don't know")]=NA
ESS16$hinctnta_factor[which(ESS16$hinctnta_factor=="No answer")]=NA
ESS16$hinctnta_factor[which(ESS16$hinctnta_factor=="Refusal")]=NA
ESS16$hinctnta_factor<-ordered(droplevels(ESS16$hinctnta_factor))
ESS16$hinctnta_num<-as.numeric(ESS16$hinctnta_factor)

# Dealing with high volume of missings
ESS16$hinctnta_num_na<-ESS16$hinctnta_num
ESS16$hinctnta_num_na[which(is.na(ESS16$hinctnta_num_na))]=0
ESS16$hinctnta_missing<-factor(nrow(ESS16),levels=c("Not missing","Missing"))
ESS16$hinctnta_missing[which(is.na(ESS16$hinctnta_num))]="Missing"
ESS16$hinctnta_missing[-which(is.na(ESS16$hinctnta_num))]="Not missing"

ESS16$hinctnta_grp<-factor(nrow(ESS16),levels=c("Low","Middle","High"))
ESS16$hinctnta_grp[which(ESS16$hinctnta_num<=3)]="Low"
ESS16$hinctnta_grp[which(ESS16$hinctnta_num>=4 & ESS16$hinctnta_num<=7)]="Middle"
ESS16$hinctnta_grp[which(ESS16$hinctnta_num>=8)]="High"

# HH category (distribution of paid work among partners -> single-earner, main-earner, double-earner, no-earner)
# Not needed (or should we model it?)

# Occupational status
ESS16$ocustat<-factor(nrow(ESS16),levels=c("Paid employment","NEET","In education","Retired"))

ESS16$ocustat[which(ESS16$mnactic=="Paid work")]="Paid employment"
ESS16$ocustat[which(ESS16$mnactic=="Community or military service")]="NEET"
ESS16$ocustat[which(ESS16$mnactic=="Housework, looking after children, others")]="NEET"
ESS16$ocustat[which(ESS16$mnactic=="Other")]="NEET"
ESS16$ocustat[which(ESS16$mnactic=="Permanently sick or disabled")]="NEET"
ESS16$ocustat[which(ESS16$mnactic=="Retired")]="Retired"
ESS16$ocustat[which(ESS16$mnactic=="Unemployed, looking for job")]="NEET"
ESS16$ocustat[which(ESS16$mnactic=="Unemployed, not looking for job")]="NEET"
ESS16$ocustat[which(ESS16$mnactic=="Education")]="In education"
ESS16$ocustat[which(ESS16$mnactic=="Don't know")]=NA
ESS16$ocustat[which(ESS16$mnactic=="Refusal")]=NA
ESS16$ocustat[which(ESS16$mnactic=="No answer")]=NA

# Single individual (0) vs individuals with family (1)
ESS16$single_family<-factor(nrow(ESS16),levels=c("Single","With family"))
ESS16$single_family[which(ESS16$lwpartner=="yes")]="With family"
ESS16$single_family[which(ESS16$nchildren>=1)]="With family"
ESS16$single_family[which(ESS16$lwpartner=="no" & ESS16$nchildren==0)]="Single"

ESS16$single_family_num<-as.numeric(ESS16$single_family)-1

############################################

#
# ESTIMATING INCOME FOR EACH PARTNER: NECESSARY VARIABLES
#

## EUROSTAT data for the imputation of average earnings

Earnings<-read.csv2("earn_ses_monthly__custom.csv",sep=",")

Earnings<-Earnings[,-c(1:4,13)]

Earnings$indic_se<-as.factor(Earnings$indic_se)
Earnings<-Earnings[which(Earnings$indic_se=="MEAN_E_PPS"),]
Earnings$indic_se<-droplevels(Earnings$indic_se)
Earnings$geo<-as.factor(Earnings$geo)
Earnings<-Earnings[which(Earnings$geo %in% c("AT","BE","CZ","DE","ES","FI","FR","HU",
                                             "IE","IT","NL","NO","PL","PT","SE","UK")),]
Earnings$geo<-droplevels(Earnings$geo)
Earnings$geo<-recode_factor(Earnings$geo,
                            "AT"="Austria","BE"="Belgium","CZ"="Czechia","DE"="Germany","ES"="Spain",
                            "FI"="Finland","FR"="France","HU"="Hungary","IE"="Ireland","IT"="Italy",
                            "NL"="Netherlands","NO"="Norway","PL"="Poland","PT"="Portugal",
                            "SE"="Sweden","UK"="United Kingdom")
                           
Earnings$TIME_PERIOD<-as.factor(Earnings$TIME_PERIOD)
Earnings$isco08<-as.factor(Earnings$isco08)
Earnings$worktime<-as.factor(Earnings$worktime)
Earnings$age<-as.factor(Earnings$age)
Earnings$age<-recode_factor(Earnings$age,"Y_GE60"=">=60","Y_LT30"="<30","Y30-39"="30-39",
                            "Y40-49"="40-49","Y50-59"="50-59")
Earnings$sex<-factor(Earnings$sex)
Earnings$sex<-recode_factor(Earnings$sex,"F"="Female","M"="Male","T"="Total")

Earnings$OBS_VALUE[which(Earnings$OBS_VALUE=="")]=NA
Earnings$OBS_VALUE<-as.numeric(Earnings$OBS_VALUE)

Earnings<-Earnings %>%
  pivot_wider(
    names_from = TIME_PERIOD,
    values_from = OBS_VALUE,
    names_prefix = "OBS_"
  )

Earnings<-as.data.frame(Earnings)

Earnings$OBS_MEAN<-numeric(nrow(Earnings))
for (i in 1:nrow(Earnings)){
  Earnings$OBS_MEAN[i]<-mean(c(Earnings$OBS_2014[i],Earnings$OBS_2018[i]),na.rm=T)
}

Earnings$OBS_TOTAL<-numeric(nrow(Earnings))
for (i in 1:nrow(Earnings)){
  Earnings$OBS_TOTAL[i]<-Earnings$OBS_MEAN[which(Earnings$geo==Earnings$geo[i] & Earnings$isco08=="TOTAL" &
                                                 Earnings$worktime=="TOTAL" & Earnings$age=="TOTAL" &
                                                 Earnings$sex=="Total")]
}

# Retrieve missings in OBS_MEAN:
# 1) If age is not TOTAL, use the TOTAL age group (recovers a high amount of missings)
for (i in which(is.na(Earnings$OBS_MEAN))){
  if (Earnings$age[i]!="TOTAL"){
    Earnings$OBS_MEAN[i]=Earnings$OBS_MEAN[which(Earnings$isco08==Earnings$isco08[i] & 
                                                   Earnings$worktime==Earnings$worktime[i] &
                                                   Earnings$age=="TOTAL" & Earnings$sex==Earnings$sex[i] &
                                                   Earnings$geo==Earnings$geo[i])]}
}

# 2) If gender is not TOTAL, use the TOTAL gender group
for (i in which(is.na(Earnings$OBS_MEAN))){
  if (Earnings$sex[i]!="Total"){
    Earnings$OBS_MEAN[i]=Earnings$OBS_MEAN[which(Earnings$isco08==Earnings$isco08[i] & 
                                                   Earnings$worktime==Earnings$worktime[i] &
                                                   Earnings$age==Earnings$age[i] & Earnings$sex=="Total" &
                                                   Earnings$geo==Earnings$geo[i])]}
}

# 3) There are missings for Portugal in OC6. Use instead OC6-8 just for Portugal
for (i in which(is.na(Earnings$OBS_MEAN) & Earnings$geo=="Portugal" & Earnings$isco08=="OC6")){
  Earnings$OBS_MEAN[i]=Earnings$OBS_MEAN[which(Earnings$isco08=="OC6-8" & 
                                                 Earnings$worktime==Earnings$worktime[i] &
                                                 Earnings$age==Earnings$age[i] & Earnings$sex==Earnings$sex[i] &
                                                 Earnings$geo==Earnings$geo[i])]
}
# The remaining missings all belong to OC0, a category that is hardly present in the ESS data.

table(Earnings$geo,Earnings$isco08) # I'm missing 0C6 for Austria and Belgium, use OC6-8 instead
Earnings$isco08[which(Earnings$geo %in% c("Austria","Belgium") & Earnings$isco08=="OC6-8")]="OC6" # This solves the problem

# There are still some missing groups, all of them for women. I will imput the totals group by group.
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="France" & Earnings$age==">=60" & 
                     Earnings$sex=="Total" & Earnings$worktime=="FT")]="Female" # Remove the total group and instead put a female group is the fastest way to go
##
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="Ireland" & Earnings$age==">=60" & 
                     Earnings$sex=="Total" & Earnings$worktime=="FT")]="Female"
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="Ireland" & Earnings$age=="<30" & 
                     Earnings$sex=="Total" & Earnings$worktime=="FT")]="Female"
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="Ireland" & Earnings$age=="50-59" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="Ireland" & Earnings$age=="<30" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="Ireland" & Earnings$age=="<30" & 
                     Earnings$sex=="Total" & Earnings$worktime=="TOTAL")]="Female"
##
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="Italy" & Earnings$age==">=60" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
##
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="Poland" & Earnings$age=="30-39" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="Poland" & Earnings$age==">=60" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
##
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="Portugal" & Earnings$age=="30-39" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="Portugal" & Earnings$age=="40-49" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="Portugal" & Earnings$age=="50-59" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="Portugal" & Earnings$age=="<30" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
Earnings$sex[which(Earnings$isco08=="OC1" & Earnings$geo=="Portugal" & Earnings$age==">=60" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
Earnings$sex[which(Earnings$isco08=="OC7" & Earnings$geo=="Portugal" & Earnings$age==">=60" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
Earnings$sex[which(Earnings$isco08=="OC7" & Earnings$geo=="Portugal" & Earnings$age=="<30" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
Earnings$sex[which(Earnings$isco08=="OC8" & Earnings$geo=="Portugal" & Earnings$age==">=60" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"
##
Earnings$sex[which(Earnings$isco08=="OC6" & Earnings$geo=="United Kingdom" & Earnings$age=="30-39" & 
                     Earnings$sex=="Total" & Earnings$worktime=="PT")]="Female"


Earnings<-Earnings[-which(Earnings$isco08 %in% c("OC1-5","OC6-8","OC7-9","TOTAL")),]
Earnings$isco08<-droplevels(Earnings$isco08)
Earnings<-Earnings[-which(Earnings$sex=="Total"),];Earnings$sex=droplevels(Earnings$sex)
Earnings<-Earnings[-which(Earnings$age=="TOTAL"),];Earnings$age=droplevels(Earnings$age)

table(Earnings$geo,Earnings$isco08) # Now we have a perfect table with 30 cases for category (except for OC0, which will be ommited)

### Obtaining the same factor in the ESS data

# 1) for respondent:

# ISCO08
ESS16$isco08<-as.factor(ESS16$isco08)

# Recoding isco08's categories into the numeric codes
ESS16$isco08codes<-recode_factor(ESS16$isco08,
                               "Armed forces occupations"="0",
                               "Commissioned armed forces officers"="100",
                               "Commissioned armed forces officers_duplicated_110"="110",
                               "Non-commissioned armed forces officers"="200",
                               "Non-commissioned armed forces officers_duplicated_210"="210",
                               "Armed forces occupations, other ranks"="300",
                               "Armed forces occupations, other ranks_duplicated_310"="310",
                               "Managers"="1000",
                               "Chief executives, senior officials and legislators"="1100",
                               "Legislators and senior officials"="1110",
                               "Legislators"="1111",
                               "Senior government officials"="1112",
                               "Traditional chiefs and heads of village"="1113",
                               "Senior officials of special-interest organizations"="1114",
                               "Managing directors and chief executives"="1120",
                               "Administrative and commercial managers"="1200",
                               "Business services and administration managers"="1210",
                               "Finance managers"="1211",
                               "Human resource managers"="1212",
                               "Policy and planning managers"="1213",
                               "Business services and administration managers not elsewhere classified"="1219",
                               "Sales, marketing and development managers"="1220",
                               "Sales and marketing managers"="1221",
                               "Advertising and public relations managers"="1222",
                               "Research and development managers"="1223",
                               "Production and specialised services managers"="1300",
                               "Production managers in agriculture, forestry and fisheries"="1310",
                               "Agricultural and forestry production managers"="1311",
                               "Aquaculture and fisheries production managers"="1312",
                               "Manufacturing, mining, construction, and distribution managers"="1320",
                               "Manufacturing managers"="1321",
                               "Mining managers"="1322",
                               "Construction managers"="1323",
                               "Supply, distribution and related managers"="1324",
                               "Information and communications technology service managers"="1330",
                               "Professional services managers"="1340",
                               "Child care services managers"="1341",
                               "Health services managers"="1342",
                               "Aged care services managers"="1343",
                               "Social welfare managers"="1344",
                               "Education managers"="1345",
                               "Financial and insurance services branch managers"="1346",
                               "Professional services managers not elsewhere classified"="1349",
                               "Hospitality, retail and other services managers"="1400",
                               "Hotel and restaurant managers"="1410",
                               "Hotel managers"="1411",
                               "Restaurant managers"="1412",
                               "Retail and wholesale trade managers"="1420",
                               "Other services managers"="1430",
                               "Sports, recreation and cultural centre managers"="1431",
                               "Services managers not elsewhere classified"="1439",
                               "Professionals"="2000",
                               "Science and engineering professionals"="2100",
                               "Physical and earth science professionals"="2110",
                               "Physicists and astronomers"="2111",
                               "Meteorologists"="2112",
                               "Chemists"="2113",
                               "Geologists and geophysicists"="2114",
                               "Mathematicians, actuaries and statisticians"="2120",
                               "Life science professionals"="2130",
                               "Biologists, botanists, zoologists and related professionals"="2131",
                               "Farming, forestry and fisheries advisers"="2132",
                               "Environmental protection professionals"="2133",
                               "Engineering professionals (excluding electrotechnology)"="2140",
                               "Industrial and production engineers"="2141",
                               "Civil engineers"="2142",
                               "Environmental engineers"="2143",
                               "Mechanical engineers"="2144",
                               "Chemical engineers"="2145",
                               "Mining engineers, metallurgists and related professionals"="2146",
                               "Engineering professionals not elsewhere classified"="2149",
                               "Electrotechnology engineers"="2150",
                               "Electrical engineers"="2151",
                               "Electronics engineers"="2152",
                               "Telecommunications engineers"="2153",
                               "Architects, planners, surveyors and designers"="2160",
                               "Building architects"="2161",
                               "Landscape architects"="2162",
                               "Product and garment designers"="2163",
                               "Town and traffic planners"="2164",
                               "Cartographers and surveyors"="2165",
                               "Graphic and multimedia designers"="2166",
                               "Health professionals"="2200",
                               "Medical doctors"="2210",
                               "Generalist medical practitioners"="2211",
                               "Specialist medical practitioners"="2212",
                               "Nursing and midwifery professionals"="2220",
                               "Nursing professionals"="2221",
                               "Midwifery professionals"="2222",
                               "Traditional and complementary medicine professionals"="2230",
                               "Paramedical practitioners"="2240",
                               "Veterinarians"="2250",
                               "Other health professionals"="2260",
                               "Dentists"="2261",
                               "Pharmacists"="2262",
                               "Environmental and occupational health and hygiene professionals"="2263",
                               "Physiotherapists"="2264",
                               "Dieticians and nutritionists"="2265",
                               "Audiologists and speech therapists"="2266",
                               "Optometrists and ophthalmic opticians"="2267",
                               "Health professionals not elsewhere classified"="2269",
                               "Teaching professionals"="2300",
                               "University and higher education teachers"="2310",
                               "Vocational education teachers"="2320",
                               "Secondary education teachers"="2330",
                               "Primary school and early childhood teachers"="2340",
                               "Primary school teachers"="2341",
                               "Early childhood educators"="2342",
                               "Other teaching professionals"="2350",
                               "Education methods specialists"="2351",
                               "Special needs teachers"="2352",
                               "Other language teachers"="2353",
                               "Other music teachers"="2354",
                               "Other arts teachers"="2355",
                               "Information technology trainers"="2356",
                               "Teaching professionals not elsewhere classified"="2359",
                               "Business and administration professionals"="2400",
                               "Finance professionals"="2410",
                               "Accountants"="2411",
                               "Financial and investment advisers"="2412",
                               "Financial analysts"="2413",
                               "Administration professionals"="2420",
                               "Management and organization analysts"="2421",
                               "Policy administration professionals"="2422",
                               "Personnel and careers professionals"="2423",
                               "Training and staff development professionals"="2424",
                               "Sales, marketing and public relations professionals"="2430",
                               "Advertising and marketing professionals"="2431",
                               "Public relations professionals"="2432",
                               "Technical and medical sales professionals (excluding ICT)"="2433",
                               "Information and communications technology sales professionals"="2434",
                               "Information and communications technology professionals"="2500",
                               "Software and applications developers and analysts"="2510",
                               "Systems analysts"="2511",
                               "Software developers"="2512",
                               "Web and multimedia developers"="2513",
                               "Applications programmers"="2514",
                               "Software and applications developers and analysts not elsewhere classified"="2519",
                               "Database and network professionals"="2520",
                               "Database designers and administrators"="2521",
                               "Systems administrators"="2522",
                               "Computer network professionals"="2523",
                               "Database and network professionals not elsewhere classified"="2529",
                               "Legal, social and cultural professionals"="2600",
                               "Legal professionals"="2610",
                               "Lawyers"="2611",
                               "Judges"="2612",
                               "Legal professionals not elsewhere classified"="2619",
                               "Librarians, archivists and curators"="2620",
                               "Archivists and curators"="2621",
                               "Librarians and related information professionals"="2622",
                               "Social and religious professionals"="2630",
                               "Economists"="2631",
                               "Sociologists, anthropologists and related professionals"="2632",
                               "Philosophers, historians and political scientists"="2633",
                               "Psychologists"="2634",
                               "Social work and counselling professionals"="2635",
                               "Religious professionals"="2636",
                               "Authors, journalists and linguists"="2640",
                               "Authors and related writers"="2641",
                               "Journalists"="2642",
                               "Translators, interpreters and other linguists"="2643",
                               "Creative and performing artists"="2650",
                               "Visual artists"="2651",
                               "Musicians, singers and composers"="2652",
                               "Dancers and choreographers"="2653",
                               "Film, stage and related directors and producers"="2654",
                               "Actors"="2655",
                               "Announcers on radio, television and other media"="2656",
                               "Creative and performing artists not elsewhere classified"="2659",
                               "Technicians and associate professionals"="3000",
                               "Science and engineering associate professionals"="3100",
                               "Physical and engineering science technicians"="3110",
                               "Chemical and physical science technicians"="3111",
                               "Civil engineering technicians"="3112",
                               "Electrical engineering technicians"="3113",
                               "Electronics engineering technicians"="3114",
                               "Mechanical engineering technicians"="3115",
                               "Chemical engineering technicians"="3116",
                               "Mining and metallurgical technicians"="3117",
                               "Draughtspersons"="3118",
                               "Physical and engineering science technicians not elsewhere classified"="3119",
                               "Mining, manufacturing and construction supervisors"="3120",
                               "Mining supervisors"="3121",
                               "Manufacturing supervisors"="3122",
                               "Construction supervisors"="3123",
                               "Process control technicians"="3130",
                               "Power production plant operators"="3131",
                               "Incinerator and water treatment plant operators"="3132",
                               "Chemical processing plant controllers"="3133",
                               "Petroleum and natural gas refining plant operators"="3134",
                               "Metal production process controllers"="3135",
                               "Process control technicians not elsewhere classified"="3139",
                               "Life science technicians and related associate professionals"="3140",
                               "Life science technicians (excluding medical)"="3141",
                               "Agricultural technicians"="3142",
                               "Forestry technicians"="3143",
                               "Ship and aircraft controllers and technicians"="3150",
                               "Ships' engineers"="3151",
                               "Ships' deck officers and pilots"="3152",
                               "Aircraft pilots and related associate professionals"="3153",
                               "Air traffic controllers"="3154",
                               "Air traffic safety electronics technicians"="3155",
                               "Health associate professionals"="3200",
                               "Medical and pharmaceutical technicians"="3210",
                               "Medical imaging and therapeutic equipment technicians"="3211",
                               "Medical and pathology laboratory technicians"="3212",
                               "Pharmaceutical technicians and assistants"="3213",
                               "Medical and dental prosthetic technicians"="3214",
                               "Nursing and midwifery associate professionals"="3220",
                               "Nursing associate professionals"="3221",
                               "Midwifery associate professionals"="3222",
                               "Traditional and complementary medicine associate professionals"="3230",
                               "Veterinary technicians and assistants"="3240",
                               "Other health associate professionals"="3250",
                               "Dental assistants and therapists"="3251",
                               "Medical records and health information technicians"="3252",
                               "Community health workers"="3253",
                               "Dispensing opticians"="3254",
                               "Physiotherapy technicians and assistants"="3255",
                               "Medical assistants"="3256",
                               "Environmental and occupational health inspectors and associates"="3257",
                               "Ambulance workers"="3258",
                               "Health associate professionals not elsewhere classified"="3259",
                               "Business and administration associate professionals"="3300",
                               "Financial and mathematical associate professionals"="3310",
                               "Securities and finance dealers and brokers"="3311",
                               "Credit and loans officers"="3312",
                               "Accounting associate professionals"="3313",
                               "Statistical, mathematical and related associate professionals"="3314",
                               "Valuers and loss assessors"="3315",
                               "Sales and purchasing agents and brokers"="3320",
                               "Insurance representatives"="3321",
                               "Commercial sales representatives"="3322",
                               "Buyers"="3323",
                               "Trade brokers"="3324",
                               "Business services agents"="3330",
                               "Clearing and forwarding agents"="3331",
                               "Conference and event planners"="3332",
                               "Employment agents and contractors"="3333",
                               "Real estate agents and property managers"="3334",
                               "Business services agents not elsewhere classified"="3339",
                               "Administrative and specialised secretaries"="3340",
                               "Office supervisors"="3341",
                               "Legal secretaries"="3342",
                               "Administrative and executive secretaries"="3343",
                               "Medical secretaries"="3344",
                               "Regulatory government associate professionals"="3350",
                               "Customs and border inspectors"="3351",
                               "Government tax and excise officials"="3352",
                               "Government social benefits officials"="3353",
                               "Government licensing officials"="3354",
                               "Police inspectors and detectives"="3355",
                               "Regulatory government associate professionals not elsewhere classified"="3359",
                               "Legal, social, cultural and related associate professionals"="3400",
                               "Legal, social and religious associate professionals"="3410",
                               "Police inspectors and detectives"="3411",
                               "Police inspectors and detectives_duplicated_3411"="3411",
                               "Social work associate professionals"="3412",
                               "Religious associate professionals"="3413",
                               "Sports and fitness workers"="3420",
                               "Athletes and sports players"="3421",
                               "Sports coaches, instructors and officials"="3422",
                               "Fitness and recreation instructors and program leaders"="3423",
                               "Artistic, cultural and culinary associate professionals"="3430",
                               "Photographers"="3431",
                               "Interior designers and decorators"="3432",
                               "Gallery, museum and library technicians"="3433",
                               "Chefs"="3434",
                               "Other artistic and cultural associate professionals"="3435",
                               "Information and communications technicians"="3500",
                               "Information and communications technology operations and user support technicians"="3510",
                               "Information and communications technology operations technicians"="3511",
                               "Information and communications technology user support technicians"="3512",
                               "Computer network and systems technicians"="3513",
                               "Web technicians"="3514",
                               "Telecommunications and broadcasting technicians"="3520",
                               "Broadcasting and audio-visual technicians"="3521",
                               "Telecommunications engineering technicians"="3522",
                               "Clerical support workers"="4000",
                               "General and keyboard clerks"="4100",
                               "General office clerks"="4110",
                               "Secretaries (general)"="4120",
                               "Keyboard operators"="4130",
                               "Typists and word processing operators"="4131",
                               "Data entry clerks"="4132",
                               "Customer services clerks"="4200",
                               "Tellers, money collectors and related clerks"="4210",
                               "Bank tellers and related clerks"="4211",
                               "Bookmakers, croupiers and related gaming workers"="4212",
                               "Pawnbrokers and money-lenders"="4213",
                               "Debt-collectors and related workers"="4214",
                               "Client information workers"="4220",
                               "Travel consultants and clerks"="4221",
                               "Contact centre information clerks"="4222",
                               "Telephone switchboard operators"="4223",
                               "Hotel receptionists"="4224",
                               "Enquiry clerks"="4225",
                               "Receptionists (general)"="4226",
                               "Survey and market research interviewers"="4227",
                               "Client information workers not elsewhere classified"="4229",
                               "Numerical and material recording clerks"="4300",
                               "Numerical clerks"="4310",
                               "Accounting and bookkeeping clerks"="4311",
                               "Statistical, finance and insurance clerks"="4312",
                               "Payroll clerks"="4313",
                               "Material-recording and transport clerks"="4320",
                               "Stock clerks"="4321",
                               "Production clerks"="4322",
                               "Transport clerks"="4323",
                               "Other clerical support workers"="4400",
                               "Other clerical support workers_duplicated_4410"="4410",
                               "Library clerks"="4411",
                               "Mail carriers and sorting clerks"="4412",
                               "Coding, proof-reading and related clerks"="4413",
                               "Scribes and related workers"="4414",
                               "Filing and copying clerks"="4415",
                               "Personnel clerks"="4416",
                               "Clerical support workers not elsewhere classified"="4419",
                               "Service and sales workers"="5000",
                               "Personal service workers"="5100",
                               "Travel attendants, conductors and guides"="5110",
                               "Travel attendants and travel stewards"="5111",
                               "Transport conductors"="5112",
                               "Travel guides"="5113",
                               "Cooks"="5120",
                               "Waiters and bartenders"="5130",
                               "Waiters"="5131",
                               "Bartenders"="5132",
                               "Hairdressers, beauticians and related workers"="5140",
                               "Hairdressers"="5141",
                               "Beauticians and related workers"="5142",
                               "Building and housekeeping supervisors"="5150",
                               "Cleaning and housekeeping supervisors in offices, hotels and other establishments"="5151",
                               "Domestic housekeepers"="5152",
                               "Building caretakers"="5153",
                               "Other personal services workers"="5160",
                               "Astrologers, fortune-tellers and related workers"="5161",
                               "Companions and valets"="5162",
                               "Undertakers and embalmers"="5163",
                               "Pet groomers and animal care workers"="5164",
                               "Driving instructors"="5165",
                               "Personal services workers not elsewhere classified"="5169",
                               "Sales workers"="5200",
                               "Street and market salespersons"="5210",
                               "Stall and market salespersons"="5211",
                               "Street food salespersons"="5212",
                               "Shop salespersons"="5220",
                               "Shop keepers"="5221",
                               "Shop supervisors"="5222",
                               "Shop sales assistants"="5223",
                               "Cashiers and ticket clerks"="5230",
                               "Other sales workers"="5240",
                               "Fashion and other models"="5241",
                               "Sales demonstrators"="5242",
                               "Door to door salespersons"="5243",
                               "Contact centre salespersons"="5244",
                               "Service station attendants"="5245",
                               "Food service counter attendants"="5246",
                               "Sales workers not elsewhere classified"="5249",
                               "Personal care workers"="5300",
                               "Child care workers and teachers' aides"="5310",
                               "Child care workers"="5311",
                               "Teachers' aides"="5312",
                               "Personal care workers in health services"="5320",
                               "Health care assistants"="5321",
                               "Home-based personal care workers"="5322",
                               "Personal care workers in health services not elsewhere classified"="5329",
                               "Protective services workers"="5400",
                               "Protective services workers_duplicated_5410"="5410",
                               "Fire-fighters"="5411",
                               "Police officers"="5412",
                               "Prison guards"="5413",
                               "Security guards"="5414",
                               "Protective services workers not elsewhere classified"="5419",
                               "Skilled agricultural, forestry and fishery workers"="6000",
                               "Market-oriented skilled agricultural workers"="6100",
                               "Market gardeners and crop growers"="6110",
                               "Field crop and vegetable growers"="6111",
                               "Tree and shrub crop growers"="6112",
                               "Gardeners, horticultural and nursery growers"="6113",
                               "Mixed crop growers"="6114",
                               "Animal producers"="6120",
                               "Livestock and dairy producers"="6121",
                               "Poultry producers"="6122",
                               "Apiarists and sericulturists"="6123",
                               "Animal producers not elsewhere classified"="6129",
                               "Mixed crop and animal producers"="6130",
                               "Market-oriented skilled forestry, fishery and hunting workers"="6200",
                               "Forestry and related workers"="6210",
                               "Fishery workers, hunters and trappers"="6220",
                               "Aquaculture workers"="6221",
                               "Inland and coastal waters fishery workers"="6222",
                               "Deep-sea fishery workers"="6223",
                               "Hunters and trappers"="6224",
                               "Subsistence farmers, fishers, hunters and gatherers"="6300",
                               "Subsistence crop farmers"="6310",
                               "Subsistence livestock farmers"="6320",
                               "Subsistence mixed crop and livestock farmers"="6330",
                               "Subsistence fishers, hunters, trappers and gatherers"="6340",
                               "Craft and related trades workers"="7000",
                               "Building and related trades workers, excluding electricians"="7100",
                               "Building frame and related trades workers"="7110",
                               "House builders"="7111",
                               "Bricklayers and related workers"="7112",
                               "Stonemasons, stone cutters, splitters and carvers"="7113",
                               "Concrete placers, concrete finishers and related workers"="7114",
                               "Carpenters and joiners"="7115",
                               "Building frame and related trades workers not elsewhere classified"="7119",
                               "Building finishers and related trades workers"="7120",
                               "Roofers"="7121",
                               "Floor layers and tile setters"="7122",
                               "Plasterers"="7123",
                               "Insulation workers"="7124",
                               "Glaziers"="7125",
                               "Plumbers and pipe fitters"="7126",
                               "Air conditioning and refrigeration mechanics"="7127",
                               "Painters, building structure cleaners and related trades workers"="7130",
                               "Painters and related workers"="7131",
                               "Spray painters and varnishers"="7132",
                               "Building structure cleaners"="7133",
                               "Metal, machinery and related trades workers"="7200",
                               "Sheet and structural metal workers, moulders and welders, and related workers"="7210",
                               "Metal moulders and coremakers"="7211",
                               "Welders and flamecutters"="7212",
                               "Sheet-metal workers"="7213",
                               "Structural-metal preparers and erectors"="7214",
                               "Riggers and cable splicers"="7215",
                               "Blacksmiths, toolmakers and related trades workers"="7220",
                               "Blacksmiths, hammersmiths and forging press workers"="7221",
                               "Toolmakers and related workers"="7222",
                               "Metal working machine tool setters and operators"="7223",
                               "Metal polishers, wheel grinders and tool sharpeners"="7224",
                               "Machinery mechanics and repairers"="7230",
                               "Motor vehicle mechanics and repairers"="7231",
                               "Aircraft engine mechanics and repairers"="7232",
                               "Agricultural and industrial machinery mechanics and repairers"="7233",
                               "Bicycle and related repairers"="7234",
                               "Handicraft and printing workers"="7300",
                               "Handicraft workers"="7310",
                               "Precision-instrument makers and repairers"="7311",
                               "Musical instrument makers and tuners"="7312",
                               "Jewellery and precious-metal workers"="7313",
                               "Potters and related workers"="7314",
                               "Glass makers, cutters, grinders and finishers"="7315",
                               "Sign writers, decorative painters, engravers and etchers"="7316",
                               "Handicraft workers in wood, basketry and related materials"="7317",
                               "Handicraft workers in textile, leather and related materials"="7318",
                               "Handicraft workers not elsewhere classified"="7319",
                               "Printing trades workers"="7320",
                               "Pre-press technicians"="7321",
                               "Printers"="7322",
                               "Print finishing and binding workers"="7323",
                               "Electrical and electronic trades workers"="7400",
                               "Electrical equipment installers and repairers"="7410",
                               "Building and related electricians"="7411",
                               "Electrical mechanics and fitters"="7412",
                               "Electrical line installers and repairers"="7413",
                               "Electronics and telecommunications installers and repairers"="7420",
                               "Electronics mechanics and servicers"="7421",
                               "Information and communications technology installers and servicers"="7422",
                               "Food processing, wood working, garment and other craft and related trades workers"="7500",
                               "Food processing and related trades workers"="7510",
                               "Butchers, fishmongers and related food preparers"="7511",
                               "Bakers, pastry-cooks and confectionery makers"="7512",
                               "Dairy-products makers"="7513",
                               "Fruit, vegetable and related preservers"="7514",
                               "Food and beverage tasters and graders"="7515",
                               "Tobacco preparers and tobacco products makers"="7516",
                               "Wood treaters, cabinet-makers and related trades workers"="7520",
                               "Wood treaters"="7521",
                               "Cabinet-makers and related workers"="7522",
                               "Woodworking-machine tool setters and operators"="7523",
                               "Garment and related trades workers"="7530",
                               "Tailors, dressmakers, furriers and hatters"="7531",
                               "Garment and related pattern-makers and cutters"="7532",
                               "Sewing, embroidery and related workers"="7533",
                               "Upholsterers and related workers"="7534",
                               "Pelt dressers, tanners and fellmongers"="7535",
                               "Shoemakers and related workers"="7536",
                               "Other craft and related workers"="7540",
                               "Underwater divers"="7541",
                               "Shotfirers and blasters"="7542",
                               "Product graders and testers (excluding foods and beverages)"="7543",
                               "Fumigators and other pest and weed controllers"="7544",
                               "Craft and related workers not elsewhere classified"="7549",
                               "Plant and machine operators, and assemblers"="8000",
                               "Stationary plant and machine operators"="8100",
                               "Mining and mineral processing plant operators"="8110",
                               "Miners and quarriers"="8111",
                               "Mineral and stone processing plant operators"="8112",
                               "Well drillers and borers and related workers"="8113",
                               "Cement, stone and other mineral products machine operators"="8114",
                               "Metal processing and finishing plant operators"="8120",
                               "Metal processing plant operators"="8121",
                               "Metal finishing, plating and coating machine operators"="8122",
                               "Chemical and photographic products plant and machine operators"="8130",
                               "Chemical products plant and machine operators"="8131",
                               "Photographic products machine operators"="8132",
                               "Rubber, plastic and paper products machine operators"="8140",
                               "Rubber products machine operators"="8141",
                               "Plastic products machine operators"="8142",
                               "Paper products machine operators"="8143",
                               "Textile, fur and leather products machine operators"="8150",
                               "Fibre preparing, spinning and winding machine operators"="8151",
                               "Weaving and knitting machine operators"="8152",
                               "Sewing machine operators"="8153",
                               "Bleaching, dyeing and fabric cleaning machine operators"="8154",
                               "Fur and leather preparing machine operators"="8155",
                               "Shoemaking and related machine operators"="8156",
                               "Laundry machine operators"="8157",
                               "Textile, fur and leather products machine operators not elsewhere classified"="8159",
                               "Food and related products machine operators"="8160",
                               "Wood processing and papermaking plant operators"="8170",
                               "Pulp and papermaking plant operators"="8171",
                               "Wood processing plant operators"="8172",
                               "Other stationary plant and machine operators"="8180",
                               "Glass and ceramics plant operators"="8181",
                               "Steam engine and boiler operators"="8182",
                               "Packing, bottling and labelling machine operators"="8183",
                               "Stationary plant and machine operators not elsewhere classified"="8189",
                               "Assemblers"="8200",
                               "Assemblers_duplicated_8210"="8210",
                               "Mechanical machinery assemblers"="8211",
                               "Electrical and electronic equipment assemblers"="8212",
                               "Assemblers not elsewhere classified"="8219",
                               "Drivers and mobile plant operators"="8300",
                               "Locomotive engine drivers and related workers"="8310",
                               "Locomotive engine drivers"="8311",
                               "Railway brake, signal and switch operators"="8312",
                               "Car, van and motorcycle drivers"="8320",
                               "Motorcycle drivers"="8321",
                               "Car, taxi and van drivers"="8322",
                               "Heavy truck and bus drivers"="8330",
                               "Bus and tram drivers"="8331",
                               "Heavy truck and lorry drivers"="8332",
                               "Mobile plant operators"="8340",
                               "Mobile farm and forestry plant operators"="8341",
                               "Earthmoving and related plant operators"="8342",
                               "Crane, hoist and related plant operators"="8343",
                               "Lifting truck operators"="8344",
                               "Ships' deck crews and related workers"="8350",
                               "Elementary occupations"="9000",
                               "Cleaners and helpers"="9100",
                               "Domestic, hotel and office cleaners and helpers"="9110",
                               "Domestic cleaners and helpers"="9111",
                               "Cleaners and helpers in offices, hotels and other establishments"="9112",
                               "Vehicle, window, laundry and other hand cleaning workers"="9120",
                               "Hand launderers and pressers"="9121",
                               "Vehicle cleaners"="9122",
                               "Window cleaners"="9123",
                               "Other cleaning workers"="9129",
                               "Agricultural, forestry and fishery labourers"="9200",
                               "Agricultural, forestry and fishery labourers_duplicated_9210"="9210",
                               "Crop farm labourers"="9211",
                               "Livestock farm labourers"="9212",
                               "Mixed crop and livestock farm labourers"="9213",
                               "Garden and horticultural labourers"="9214",
                               "Forestry labourers"="9215",
                               "Fishery and aquaculture labourers"="9216",
                               "Labourers in mining, construction, manufacturing and transport"="9300",
                               "Mining and construction labourers"="9310",
                               "Mining and quarrying labourers"="9311",
                               "Civil engineering labourers"="9312",
                               "Building construction labourers"="9313",
                               "Manufacturing labourers"="9320",
                               "Hand packers"="9321",
                               "Manufacturing labourers not elsewhere classified"="9329",
                               "Transport and storage labourers"="9330",
                               "Hand and pedal vehicle drivers"="9331",
                               "Drivers of animal-drawn vehicles and machinery"="9332",
                               "Freight handlers"="9333",
                               "Shelf fillers"="9334",
                               "Food preparation assistants"="9400",
                               "Food preparation assistants_duplicated_9410"="9410",
                               "Fast food preparers"="9411",
                               "Kitchen helpers"="9412",
                               "Street and related sales and service workers"="9500",
                               "Street and related service workers"="9510",
                               "Street vendors (excluding food)"="9520",
                               "Refuse workers and other elementary workers"="9600",
                               "Refuse workers"="9610",
                               "Garbage and recycling collectors"="9611",
                               "Refuse sorters"="9612",
                               "Sweepers and related labourers"="9613",
                               "Other elementary workers"="9620",
                               "Messengers, package deliverers and luggage porters"="9621",
                               "Odd job persons"="9622",
                               "Meter readers and vending-machine collectors"="9623",
                               "Water and firewood collectors"="9624",
                               "Elementary workers not elsewhere classified"="9629",
                               "Not applicable"="66666",
                               "Refusal"="77777",
                               "Don't know"="88888",
                               "No answer"="99999") # Numeric ISCO-08 codes as a factor

ESS16$isco08codes<-as.numeric(paste(ESS16$isco08codes)) # Numeric ISCO-08 codes
ESS16$isco08codes[which(ESS16$isco08codes>=66666)]=NA

ESS16$isco08_postcoded<-factor(nrow(ESS16),levels=c("OC0","OC1","OC2","OC3","OC4",
                                                    "OC5","OC6","OC7","OC8","OC9"))

ESS16$isco08_postcoded[which(ESS16$isco08codes<=999)]="OC0"
ESS16$isco08_postcoded[which(ESS16$isco08codes>=1000 & ESS16$isco08codes<2000)]="OC1"
ESS16$isco08_postcoded[which(ESS16$isco08codes>=2000 & ESS16$isco08codes<3000)]="OC2"
ESS16$isco08_postcoded[which(ESS16$isco08codes>=3000 & ESS16$isco08codes<4000)]="OC3"
ESS16$isco08_postcoded[which(ESS16$isco08codes>=4000 & ESS16$isco08codes<5000)]="OC4"
ESS16$isco08_postcoded[which(ESS16$isco08codes>=5000 & ESS16$isco08codes<6000)]="OC5"
ESS16$isco08_postcoded[which(ESS16$isco08codes>=6000 & ESS16$isco08codes<7000)]="OC6"
ESS16$isco08_postcoded[which(ESS16$isco08codes>=7000 & ESS16$isco08codes<8000)]="OC7"
ESS16$isco08_postcoded[which(ESS16$isco08codes>=8000 & ESS16$isco08codes<9000)]="OC8"
ESS16$isco08_postcoded[which(ESS16$isco08codes>=9000 & ESS16$isco08codes<9999)]="OC9"

# Full-time or part-time
ESS16$wkhtot[which(ESS16$wktot>168)]=NA # Note that 168 is the highest acceptable value according to the codebook
ESS16$wkhtot_grp<-factor(nrow(ESS16),levels=c("FT","PT","TOTAL"))
ESS16$wkhtot_grp[which(ESS16$wkhtot<35)]="PT"
ESS16$wkhtot_grp[which(ESS16$wkhtot>=35)]="FT"
ESS16$wkhtot_grp[which(is.na(ESS16$wkhtot))]="TOTAL" # When there is no info I am just using the total (for analyses code as NA)

# Estimate salary
ESS16$salary_est<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$pdwrk[i]=="Not marked"){
    ESS16$salary_est[i]=0} 
  if (ESS16$pdwrk[i]=="Marked" & is.na(ESS16$isco08_postcoded[i])){
      ESS16$salary_est[i]=NA}
    else{
    if (ESS16$pdwrk[i]=="Marked" & ESS16$isco08_postcoded[i]=="OC0"){
    ESS16$salary_est[i]=NA}
    if (ESS16$pdwrk[i]=="Marked" & ESS16$isco08_postcoded[i]!="OC0"){  
    ESS16$salary_est[i]=Earnings$OBS_MEAN[which(Earnings$isco08==ESS16$isco08_postcoded[i] & Earnings$worktime==ESS16$wkhtot_grp[i] &
                                                   Earnings$sex==ESS16$gndr[i] & Earnings$age==ESS16$agea_grp2[i] &
                                                   Earnings$geo==ESS16$cntry[i])]}
    }} 

# Estimate average salary in the country
ESS16$average_salary<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  ESS16$average_salary[i]=Earnings$OBS_TOTAL[which(Earnings$geo==ESS16$cntry[i])][1]
}

#######################################
# Calculate salary group as a function of respondent's salary and country average
ESS16$salary_grp<-factor(nrow(ESS16),levels=c("No income","Half the average","Average","Double the average"))
for (i in 1:nrow(ESS16)){
  if (is.na(ESS16$salary_est[i])){
    ESS16$salary_grp[i]=NA
  }
  else{
    if (ESS16$salary_est[i]<=(ESS16$average_salary[i]*0.5)){
      ESS16$salary_grp[i]="Half the average"
    }
    if (ESS16$salary_est[i]>(ESS16$average_salary[i]*0.5) & ESS16$salary_est[i]<(ESS16$average_salary[i]*2)){
      ESS16$salary_grp[i]="Average"
    }
    if (ESS16$salary_est[i]>=(ESS16$average_salary[i]*2)){
      ESS16$salary_grp[i]="Double the average"
    }
    if (ESS16$salary_est[i]==0){
      ESS16$salary_grp[i]="No income"
    }
  }
}

#############################
# Do the same for the partner:

# ISCO08
ESS16$isco08pcodes<-recode_factor(ESS16$isco08p,
                                  "Armed forces occupations"="0",
                                  "Commissioned armed forces officers"="100",
                                  "Commissioned armed forces officers_duplicated_110"="110",
                                  "Non-commissioned armed forces officers"="200",
                                  "Non-commissioned armed forces officers_duplicated_210"="210",
                                  "Armed forces occupations, other ranks"="300",
                                  "Armed forces occupations, other ranks_duplicated_310"="310",
                                  "Managers"="1000",
                                  "Chief executives, senior officials and legislators"="1100",
                                  "Legislators and senior officials"="1110",
                                  "Legislators"="1111",
                                  "Senior government officials"="1112",
                                  "Traditional chiefs and heads of village"="1113",
                                  "Senior officials of special-interest organizations"="1114",
                                  "Managing directors and chief executives"="1120",
                                  "Administrative and commercial managers"="1200",
                                  "Business services and administration managers"="1210",
                                  "Finance managers"="1211",
                                  "Human resource managers"="1212",
                                  "Policy and planning managers"="1213",
                                  "Business services and administration managers not elsewhere classified"="1219",
                                  "Sales, marketing and development managers"="1220",
                                  "Sales and marketing managers"="1221",
                                  "Advertising and public relations managers"="1222",
                                  "Research and development managers"="1223",
                                  "Production and specialised services managers"="1300",
                                  "Production managers in agriculture, forestry and fisheries"="1310",
                                  "Agricultural and forestry production managers"="1311",
                                  "Aquaculture and fisheries production managers"="1312",
                                  "Manufacturing, mining, construction, and distribution managers"="1320",
                                  "Manufacturing managers"="1321",
                                  "Mining managers"="1322",
                                  "Construction managers"="1323",
                                  "Supply, distribution and related managers"="1324",
                                  "Information and communications technology service managers"="1330",
                                  "Professional services managers"="1340",
                                  "Child care services managers"="1341",
                                  "Health services managers"="1342",
                                  "Aged care services managers"="1343",
                                  "Social welfare managers"="1344",
                                  "Education managers"="1345",
                                  "Financial and insurance services branch managers"="1346",
                                  "Professional services managers not elsewhere classified"="1349",
                                  "Hospitality, retail and other services managers"="1400",
                                  "Hotel and restaurant managers"="1410",
                                  "Hotel managers"="1411",
                                  "Restaurant managers"="1412",
                                  "Retail and wholesale trade managers"="1420",
                                  "Other services managers"="1430",
                                  "Sports, recreation and cultural centre managers"="1431",
                                  "Services managers not elsewhere classified"="1439",
                                  "Professionals"="2000",
                                  "Science and engineering professionals"="2100",
                                  "Physical and earth science professionals"="2110",
                                  "Physicists and astronomers"="2111",
                                  "Meteorologists"="2112",
                                  "Chemists"="2113",
                                  "Geologists and geophysicists"="2114",
                                  "Mathematicians, actuaries and statisticians"="2120",
                                  "Life science professionals"="2130",
                                  "Biologists, botanists, zoologists and related professionals"="2131",
                                  "Farming, forestry and fisheries advisers"="2132",
                                  "Environmental protection professionals"="2133",
                                  "Engineering professionals (excluding electrotechnology)"="2140",
                                  "Industrial and production engineers"="2141",
                                  "Civil engineers"="2142",
                                  "Environmental engineers"="2143",
                                  "Mechanical engineers"="2144",
                                  "Chemical engineers"="2145",
                                  "Mining engineers, metallurgists and related professionals"="2146",
                                  "Engineering professionals not elsewhere classified"="2149",
                                  "Electrotechnology engineers"="2150",
                                  "Electrical engineers"="2151",
                                  "Electronics engineers"="2152",
                                  "Telecommunications engineers"="2153",
                                  "Architects, planners, surveyors and designers"="2160",
                                  "Building architects"="2161",
                                  "Landscape architects"="2162",
                                  "Product and garment designers"="2163",
                                  "Town and traffic planners"="2164",
                                  "Cartographers and surveyors"="2165",
                                  "Graphic and multimedia designers"="2166",
                                  "Health professionals"="2200",
                                  "Medical doctors"="2210",
                                  "Generalist medical practitioners"="2211",
                                  "Specialist medical practitioners"="2212",
                                  "Nursing and midwifery professionals"="2220",
                                  "Nursing professionals"="2221",
                                  "Midwifery professionals"="2222",
                                  "Traditional and complementary medicine professionals"="2230",
                                  "Paramedical practitioners"="2240",
                                  "Veterinarians"="2250",
                                  "Other health professionals"="2260",
                                  "Dentists"="2261",
                                  "Pharmacists"="2262",
                                  "Environmental and occupational health and hygiene professionals"="2263",
                                  "Physiotherapists"="2264",
                                  "Dieticians and nutritionists"="2265",
                                  "Audiologists and speech therapists"="2266",
                                  "Optometrists and ophthalmic opticians"="2267",
                                  "Health professionals not elsewhere classified"="2269",
                                  "Teaching professionals"="2300",
                                  "University and higher education teachers"="2310",
                                  "Vocational education teachers"="2320",
                                  "Secondary education teachers"="2330",
                                  "Primary school and early childhood teachers"="2340",
                                  "Primary school teachers"="2341",
                                  "Early childhood educators"="2342",
                                  "Other teaching professionals"="2350",
                                  "Education methods specialists"="2351",
                                  "Special needs teachers"="2352",
                                  "Other language teachers"="2353",
                                  "Other music teachers"="2354",
                                  "Other arts teachers"="2355",
                                  "Information technology trainers"="2356",
                                  "Teaching professionals not elsewhere classified"="2359",
                                  "Business and administration professionals"="2400",
                                  "Finance professionals"="2410",
                                  "Accountants"="2411",
                                  "Financial and investment advisers"="2412",
                                  "Financial analysts"="2413",
                                  "Administration professionals"="2420",
                                  "Management and organization analysts"="2421",
                                  "Policy administration professionals"="2422",
                                  "Personnel and careers professionals"="2423",
                                  "Training and staff development professionals"="2424",
                                  "Sales, marketing and public relations professionals"="2430",
                                  "Advertising and marketing professionals"="2431",
                                  "Public relations professionals"="2432",
                                  "Technical and medical sales professionals (excluding ICT)"="2433",
                                  "Information and communications technology sales professionals"="2434",
                                  "Information and communications technology professionals"="2500",
                                  "Software and applications developers and analysts"="2510",
                                  "Systems analysts"="2511",
                                  "Software developers"="2512",
                                  "Web and multimedia developers"="2513",
                                  "Applications programmers"="2514",
                                  "Software and applications developers and analysts not elsewhere classified"="2519",
                                  "Database and network professionals"="2520",
                                  "Database designers and administrators"="2521",
                                  "Systems administrators"="2522",
                                  "Computer network professionals"="2523",
                                  "Database and network professionals not elsewhere classified"="2529",
                                  "Legal, social and cultural professionals"="2600",
                                  "Legal professionals"="2610",
                                  "Lawyers"="2611",
                                  "Judges"="2612",
                                  "Legal professionals not elsewhere classified"="2619",
                                  "Librarians, archivists and curators"="2620",
                                  "Archivists and curators"="2621",
                                  "Librarians and related information professionals"="2622",
                                  "Social and religious professionals"="2630",
                                  "Economists"="2631",
                                  "Sociologists, anthropologists and related professionals"="2632",
                                  "Philosophers, historians and political scientists"="2633",
                                  "Psychologists"="2634",
                                  "Social work and counselling professionals"="2635",
                                  "Religious professionals"="2636",
                                  "Authors, journalists and linguists"="2640",
                                  "Authors and related writers"="2641",
                                  "Journalists"="2642",
                                  "Translators, interpreters and other linguists"="2643",
                                  "Creative and performing artists"="2650",
                                  "Visual artists"="2651",
                                  "Musicians, singers and composers"="2652",
                                  "Dancers and choreographers"="2653",
                                  "Film, stage and related directors and producers"="2654",
                                  "Actors"="2655",
                                  "Announcers on radio, television and other media"="2656",
                                  "Creative and performing artists not elsewhere classified"="2659",
                                  "Technicians and associate professionals"="3000",
                                  "Science and engineering associate professionals"="3100",
                                  "Physical and engineering science technicians"="3110",
                                  "Chemical and physical science technicians"="3111",
                                  "Civil engineering technicians"="3112",
                                  "Electrical engineering technicians"="3113",
                                  "Electronics engineering technicians"="3114",
                                  "Mechanical engineering technicians"="3115",
                                  "Chemical engineering technicians"="3116",
                                  "Mining and metallurgical technicians"="3117",
                                  "Draughtspersons"="3118",
                                  "Physical and engineering science technicians not elsewhere classified"="3119",
                                  "Mining, manufacturing and construction supervisors"="3120",
                                  "Mining supervisors"="3121",
                                  "Manufacturing supervisors"="3122",
                                  "Construction supervisors"="3123",
                                  "Process control technicians"="3130",
                                  "Power production plant operators"="3131",
                                  "Incinerator and water treatment plant operators"="3132",
                                  "Chemical processing plant controllers"="3133",
                                  "Petroleum and natural gas refining plant operators"="3134",
                                  "Metal production process controllers"="3135",
                                  "Process control technicians not elsewhere classified"="3139",
                                  "Life science technicians and related associate professionals"="3140",
                                  "Life science technicians (excluding medical)"="3141",
                                  "Agricultural technicians"="3142",
                                  "Forestry technicians"="3143",
                                  "Ship and aircraft controllers and technicians"="3150",
                                  "Ships' engineers"="3151",
                                  "Ships' deck officers and pilots"="3152",
                                  "Aircraft pilots and related associate professionals"="3153",
                                  "Air traffic controllers"="3154",
                                  "Air traffic safety electronics technicians"="3155",
                                  "Health associate professionals"="3200",
                                  "Medical and pharmaceutical technicians"="3210",
                                  "Medical imaging and therapeutic equipment technicians"="3211",
                                  "Medical and pathology laboratory technicians"="3212",
                                  "Pharmaceutical technicians and assistants"="3213",
                                  "Medical and dental prosthetic technicians"="3214",
                                  "Nursing and midwifery associate professionals"="3220",
                                  "Nursing associate professionals"="3221",
                                  "Midwifery associate professionals"="3222",
                                  "Traditional and complementary medicine associate professionals"="3230",
                                  "Veterinary technicians and assistants"="3240",
                                  "Other health associate professionals"="3250",
                                  "Dental assistants and therapists"="3251",
                                  "Medical records and health information technicians"="3252",
                                  "Community health workers"="3253",
                                  "Dispensing opticians"="3254",
                                  "Physiotherapy technicians and assistants"="3255",
                                  "Medical assistants"="3256",
                                  "Environmental and occupational health inspectors and associates"="3257",
                                  "Ambulance workers"="3258",
                                  "Health associate professionals not elsewhere classified"="3259",
                                  "Business and administration associate professionals"="3300",
                                  "Financial and mathematical associate professionals"="3310",
                                  "Securities and finance dealers and brokers"="3311",
                                  "Credit and loans officers"="3312",
                                  "Accounting associate professionals"="3313",
                                  "Statistical, mathematical and related associate professionals"="3314",
                                  "Valuers and loss assessors"="3315",
                                  "Sales and purchasing agents and brokers"="3320",
                                  "Insurance representatives"="3321",
                                  "Commercial sales representatives"="3322",
                                  "Buyers"="3323",
                                  "Trade brokers"="3324",
                                  "Business services agents"="3330",
                                  "Clearing and forwarding agents"="3331",
                                  "Conference and event planners"="3332",
                                  "Employment agents and contractors"="3333",
                                  "Real estate agents and property managers"="3334",
                                  "Business services agents not elsewhere classified"="3339",
                                  "Administrative and specialised secretaries"="3340",
                                  "Office supervisors"="3341",
                                  "Legal secretaries"="3342",
                                  "Administrative and executive secretaries"="3343",
                                  "Medical secretaries"="3344",
                                  "Regulatory government associate professionals"="3350",
                                  "Customs and border inspectors"="3351",
                                  "Government tax and excise officials"="3352",
                                  "Government social benefits officials"="3353",
                                  "Government licensing officials"="3354",
                                  "Police inspectors and detectives"="3355",
                                  "Regulatory government associate professionals not elsewhere classified"="3359",
                                  "Legal, social, cultural and related associate professionals"="3400",
                                  "Legal, social and religious associate professionals"="3410",
                                  "Police inspectors and detectives"="3411",
                                  "Police inspectors and detectives_duplicated_3411"="3411",
                                  "Social work associate professionals"="3412",
                                  "Religious associate professionals"="3413",
                                  "Sports and fitness workers"="3420",
                                  "Athletes and sports players"="3421",
                                  "Sports coaches, instructors and officials"="3422",
                                  "Fitness and recreation instructors and program leaders"="3423",
                                  "Artistic, cultural and culinary associate professionals"="3430",
                                  "Photographers"="3431",
                                  "Interior designers and decorators"="3432",
                                  "Gallery, museum and library technicians"="3433",
                                  "Chefs"="3434",
                                  "Other artistic and cultural associate professionals"="3435",
                                  "Information and communications technicians"="3500",
                                  "Information and communications technology operations and user support technicians"="3510",
                                  "Information and communications technology operations technicians"="3511",
                                  "Information and communications technology user support technicians"="3512",
                                  "Computer network and systems technicians"="3513",
                                  "Web technicians"="3514",
                                  "Telecommunications and broadcasting technicians"="3520",
                                  "Broadcasting and audio-visual technicians"="3521",
                                  "Telecommunications engineering technicians"="3522",
                                  "Clerical support workers"="4000",
                                  "General and keyboard clerks"="4100",
                                  "General office clerks"="4110",
                                  "Secretaries (general)"="4120",
                                  "Keyboard operators"="4130",
                                  "Typists and word processing operators"="4131",
                                  "Data entry clerks"="4132",
                                  "Customer services clerks"="4200",
                                  "Tellers, money collectors and related clerks"="4210",
                                  "Bank tellers and related clerks"="4211",
                                  "Bookmakers, croupiers and related gaming workers"="4212",
                                  "Pawnbrokers and money-lenders"="4213",
                                  "Debt-collectors and related workers"="4214",
                                  "Client information workers"="4220",
                                  "Travel consultants and clerks"="4221",
                                  "Contact centre information clerks"="4222",
                                  "Telephone switchboard operators"="4223",
                                  "Hotel receptionists"="4224",
                                  "Enquiry clerks"="4225",
                                  "Receptionists (general)"="4226",
                                  "Survey and market research interviewers"="4227",
                                  "Client information workers not elsewhere classified"="4229",
                                  "Numerical and material recording clerks"="4300",
                                  "Numerical clerks"="4310",
                                  "Accounting and bookkeeping clerks"="4311",
                                  "Statistical, finance and insurance clerks"="4312",
                                  "Payroll clerks"="4313",
                                  "Material-recording and transport clerks"="4320",
                                  "Stock clerks"="4321",
                                  "Production clerks"="4322",
                                  "Transport clerks"="4323",
                                  "Other clerical support workers"="4400",
                                  "Other clerical support workers_duplicated_4410"="4410",
                                  "Library clerks"="4411",
                                  "Mail carriers and sorting clerks"="4412",
                                  "Coding, proof-reading and related clerks"="4413",
                                  "Scribes and related workers"="4414",
                                  "Filing and copying clerks"="4415",
                                  "Personnel clerks"="4416",
                                  "Clerical support workers not elsewhere classified"="4419",
                                  "Service and sales workers"="5000",
                                  "Personal service workers"="5100",
                                  "Travel attendants, conductors and guides"="5110",
                                  "Travel attendants and travel stewards"="5111",
                                  "Transport conductors"="5112",
                                  "Travel guides"="5113",
                                  "Cooks"="5120",
                                  "Waiters and bartenders"="5130",
                                  "Waiters"="5131",
                                  "Bartenders"="5132",
                                  "Hairdressers, beauticians and related workers"="5140",
                                  "Hairdressers"="5141",
                                  "Beauticians and related workers"="5142",
                                  "Building and housekeeping supervisors"="5150",
                                  "Cleaning and housekeeping supervisors in offices, hotels and other establishments"="5151",
                                  "Domestic housekeepers"="5152",
                                  "Building caretakers"="5153",
                                  "Other personal services workers"="5160",
                                  "Astrologers, fortune-tellers and related workers"="5161",
                                  "Companions and valets"="5162",
                                  "Undertakers and embalmers"="5163",
                                  "Pet groomers and animal care workers"="5164",
                                  "Driving instructors"="5165",
                                  "Personal services workers not elsewhere classified"="5169",
                                  "Sales workers"="5200",
                                  "Street and market salespersons"="5210",
                                  "Stall and market salespersons"="5211",
                                  "Street food salespersons"="5212",
                                  "Shop salespersons"="5220",
                                  "Shop keepers"="5221",
                                  "Shop supervisors"="5222",
                                  "Shop sales assistants"="5223",
                                  "Cashiers and ticket clerks"="5230",
                                  "Other sales workers"="5240",
                                  "Fashion and other models"="5241",
                                  "Sales demonstrators"="5242",
                                  "Door to door salespersons"="5243",
                                  "Contact centre salespersons"="5244",
                                  "Service station attendants"="5245",
                                  "Food service counter attendants"="5246",
                                  "Sales workers not elsewhere classified"="5249",
                                  "Personal care workers"="5300",
                                  "Child care workers and teachers' aides"="5310",
                                  "Child care workers"="5311",
                                  "Teachers' aides"="5312",
                                  "Personal care workers in health services"="5320",
                                  "Health care assistants"="5321",
                                  "Home-based personal care workers"="5322",
                                  "Personal care workers in health services not elsewhere classified"="5329",
                                  "Protective services workers"="5400",
                                  "Protective services workers_duplicated_5410"="5410",
                                  "Fire-fighters"="5411",
                                  "Police officers"="5412",
                                  "Prison guards"="5413",
                                  "Security guards"="5414",
                                  "Protective services workers not elsewhere classified"="5419",
                                  "Skilled agricultural, forestry and fishery workers"="6000",
                                  "Market-oriented skilled agricultural workers"="6100",
                                  "Market gardeners and crop growers"="6110",
                                  "Field crop and vegetable growers"="6111",
                                  "Tree and shrub crop growers"="6112",
                                  "Gardeners, horticultural and nursery growers"="6113",
                                  "Mixed crop growers"="6114",
                                  "Animal producers"="6120",
                                  "Livestock and dairy producers"="6121",
                                  "Poultry producers"="6122",
                                  "Apiarists and sericulturists"="6123",
                                  "Animal producers not elsewhere classified"="6129",
                                  "Mixed crop and animal producers"="6130",
                                  "Market-oriented skilled forestry, fishery and hunting workers"="6200",
                                  "Forestry and related workers"="6210",
                                  "Fishery workers, hunters and trappers"="6220",
                                  "Aquaculture workers"="6221",
                                  "Inland and coastal waters fishery workers"="6222",
                                  "Deep-sea fishery workers"="6223",
                                  "Hunters and trappers"="6224",
                                  "Subsistence farmers, fishers, hunters and gatherers"="6300",
                                  "Subsistence crop farmers"="6310",
                                  "Subsistence livestock farmers"="6320",
                                  "Subsistence mixed crop and livestock farmers"="6330",
                                  "Subsistence fishers, hunters, trappers and gatherers"="6340",
                                  "Craft and related trades workers"="7000",
                                  "Building and related trades workers, excluding electricians"="7100",
                                  "Building frame and related trades workers"="7110",
                                  "House builders"="7111",
                                  "Bricklayers and related workers"="7112",
                                  "Stonemasons, stone cutters, splitters and carvers"="7113",
                                  "Concrete placers, concrete finishers and related workers"="7114",
                                  "Carpenters and joiners"="7115",
                                  "Building frame and related trades workers not elsewhere classified"="7119",
                                  "Building finishers and related trades workers"="7120",
                                  "Roofers"="7121",
                                  "Floor layers and tile setters"="7122",
                                  "Plasterers"="7123",
                                  "Insulation workers"="7124",
                                  "Glaziers"="7125",
                                  "Plumbers and pipe fitters"="7126",
                                  "Air conditioning and refrigeration mechanics"="7127",
                                  "Painters, building structure cleaners and related trades workers"="7130",
                                  "Painters and related workers"="7131",
                                  "Spray painters and varnishers"="7132",
                                  "Building structure cleaners"="7133",
                                  "Metal, machinery and related trades workers"="7200",
                                  "Sheet and structural metal workers, moulders and welders, and related workers"="7210",
                                  "Metal moulders and coremakers"="7211",
                                  "Welders and flamecutters"="7212",
                                  "Sheet-metal workers"="7213",
                                  "Structural-metal preparers and erectors"="7214",
                                  "Riggers and cable splicers"="7215",
                                  "Blacksmiths, toolmakers and related trades workers"="7220",
                                  "Blacksmiths, hammersmiths and forging press workers"="7221",
                                  "Toolmakers and related workers"="7222",
                                  "Metal working machine tool setters and operators"="7223",
                                  "Metal polishers, wheel grinders and tool sharpeners"="7224",
                                  "Machinery mechanics and repairers"="7230",
                                  "Motor vehicle mechanics and repairers"="7231",
                                  "Aircraft engine mechanics and repairers"="7232",
                                  "Agricultural and industrial machinery mechanics and repairers"="7233",
                                  "Bicycle and related repairers"="7234",
                                  "Handicraft and printing workers"="7300",
                                  "Handicraft workers"="7310",
                                  "Precision-instrument makers and repairers"="7311",
                                  "Musical instrument makers and tuners"="7312",
                                  "Jewellery and precious-metal workers"="7313",
                                  "Potters and related workers"="7314",
                                  "Glass makers, cutters, grinders and finishers"="7315",
                                  "Sign writers, decorative painters, engravers and etchers"="7316",
                                  "Handicraft workers in wood, basketry and related materials"="7317",
                                  "Handicraft workers in textile, leather and related materials"="7318",
                                  "Handicraft workers not elsewhere classified"="7319",
                                  "Printing trades workers"="7320",
                                  "Pre-press technicians"="7321",
                                  "Printers"="7322",
                                  "Print finishing and binding workers"="7323",
                                  "Electrical and electronic trades workers"="7400",
                                  "Electrical equipment installers and repairers"="7410",
                                  "Building and related electricians"="7411",
                                  "Electrical mechanics and fitters"="7412",
                                  "Electrical line installers and repairers"="7413",
                                  "Electronics and telecommunications installers and repairers"="7420",
                                  "Electronics mechanics and servicers"="7421",
                                  "Information and communications technology installers and servicers"="7422",
                                  "Food processing, wood working, garment and other craft and related trades workers"="7500",
                                  "Food processing and related trades workers"="7510",
                                  "Butchers, fishmongers and related food preparers"="7511",
                                  "Bakers, pastry-cooks and confectionery makers"="7512",
                                  "Dairy-products makers"="7513",
                                  "Fruit, vegetable and related preservers"="7514",
                                  "Food and beverage tasters and graders"="7515",
                                  "Tobacco preparers and tobacco products makers"="7516",
                                  "Wood treaters, cabinet-makers and related trades workers"="7520",
                                  "Wood treaters"="7521",
                                  "Cabinet-makers and related workers"="7522",
                                  "Woodworking-machine tool setters and operators"="7523",
                                  "Garment and related trades workers"="7530",
                                  "Tailors, dressmakers, furriers and hatters"="7531",
                                  "Garment and related pattern-makers and cutters"="7532",
                                  "Sewing, embroidery and related workers"="7533",
                                  "Upholsterers and related workers"="7534",
                                  "Pelt dressers, tanners and fellmongers"="7535",
                                  "Shoemakers and related workers"="7536",
                                  "Other craft and related workers"="7540",
                                  "Underwater divers"="7541",
                                  "Shotfirers and blasters"="7542",
                                  "Product graders and testers (excluding foods and beverages)"="7543",
                                  "Fumigators and other pest and weed controllers"="7544",
                                  "Craft and related workers not elsewhere classified"="7549",
                                  "Plant and machine operators, and assemblers"="8000",
                                  "Stationary plant and machine operators"="8100",
                                  "Mining and mineral processing plant operators"="8110",
                                  "Miners and quarriers"="8111",
                                  "Mineral and stone processing plant operators"="8112",
                                  "Well drillers and borers and related workers"="8113",
                                  "Cement, stone and other mineral products machine operators"="8114",
                                  "Metal processing and finishing plant operators"="8120",
                                  "Metal processing plant operators"="8121",
                                  "Metal finishing, plating and coating machine operators"="8122",
                                  "Chemical and photographic products plant and machine operators"="8130",
                                  "Chemical products plant and machine operators"="8131",
                                  "Photographic products machine operators"="8132",
                                  "Rubber, plastic and paper products machine operators"="8140",
                                  "Rubber products machine operators"="8141",
                                  "Plastic products machine operators"="8142",
                                  "Paper products machine operators"="8143",
                                  "Textile, fur and leather products machine operators"="8150",
                                  "Fibre preparing, spinning and winding machine operators"="8151",
                                  "Weaving and knitting machine operators"="8152",
                                  "Sewing machine operators"="8153",
                                  "Bleaching, dyeing and fabric cleaning machine operators"="8154",
                                  "Fur and leather preparing machine operators"="8155",
                                  "Shoemaking and related machine operators"="8156",
                                  "Laundry machine operators"="8157",
                                  "Textile, fur and leather products machine operators not elsewhere classified"="8159",
                                  "Food and related products machine operators"="8160",
                                  "Wood processing and papermaking plant operators"="8170",
                                  "Pulp and papermaking plant operators"="8171",
                                  "Wood processing plant operators"="8172",
                                  "Other stationary plant and machine operators"="8180",
                                  "Glass and ceramics plant operators"="8181",
                                  "Steam engine and boiler operators"="8182",
                                  "Packing, bottling and labelling machine operators"="8183",
                                  "Stationary plant and machine operators not elsewhere classified"="8189",
                                  "Assemblers"="8200",
                                  "Assemblers_duplicated_8210"="8210",
                                  "Mechanical machinery assemblers"="8211",
                                  "Electrical and electronic equipment assemblers"="8212",
                                  "Assemblers not elsewhere classified"="8219",
                                  "Drivers and mobile plant operators"="8300",
                                  "Locomotive engine drivers and related workers"="8310",
                                  "Locomotive engine drivers"="8311",
                                  "Railway brake, signal and switch operators"="8312",
                                  "Car, van and motorcycle drivers"="8320",
                                  "Motorcycle drivers"="8321",
                                  "Car, taxi and van drivers"="8322",
                                  "Heavy truck and bus drivers"="8330",
                                  "Bus and tram drivers"="8331",
                                  "Heavy truck and lorry drivers"="8332",
                                  "Mobile plant operators"="8340",
                                  "Mobile farm and forestry plant operators"="8341",
                                  "Earthmoving and related plant operators"="8342",
                                  "Crane, hoist and related plant operators"="8343",
                                  "Lifting truck operators"="8344",
                                  "Ships' deck crews and related workers"="8350",
                                  "Elementary occupations"="9000",
                                  "Cleaners and helpers"="9100",
                                  "Domestic, hotel and office cleaners and helpers"="9110",
                                  "Domestic cleaners and helpers"="9111",
                                  "Cleaners and helpers in offices, hotels and other establishments"="9112",
                                  "Vehicle, window, laundry and other hand cleaning workers"="9120",
                                  "Hand launderers and pressers"="9121",
                                  "Vehicle cleaners"="9122",
                                  "Window cleaners"="9123",
                                  "Other cleaning workers"="9129",
                                  "Agricultural, forestry and fishery labourers"="9200",
                                  "Agricultural, forestry and fishery labourers_duplicated_9210"="9210",
                                  "Crop farm labourers"="9211",
                                  "Livestock farm labourers"="9212",
                                  "Mixed crop and livestock farm labourers"="9213",
                                  "Garden and horticultural labourers"="9214",
                                  "Forestry labourers"="9215",
                                  "Fishery and aquaculture labourers"="9216",
                                  "Labourers in mining, construction, manufacturing and transport"="9300",
                                  "Mining and construction labourers"="9310",
                                  "Mining and quarrying labourers"="9311",
                                  "Civil engineering labourers"="9312",
                                  "Building construction labourers"="9313",
                                  "Manufacturing labourers"="9320",
                                  "Hand packers"="9321",
                                  "Manufacturing labourers not elsewhere classified"="9329",
                                  "Transport and storage labourers"="9330",
                                  "Hand and pedal vehicle drivers"="9331",
                                  "Drivers of animal-drawn vehicles and machinery"="9332",
                                  "Freight handlers"="9333",
                                  "Shelf fillers"="9334",
                                  "Food preparation assistants"="9400",
                                  "Food preparation assistants_duplicated_9410"="9410",
                                  "Fast food preparers"="9411",
                                  "Kitchen helpers"="9412",
                                  "Street and related sales and service workers"="9500",
                                  "Street and related service workers"="9510",
                                  "Street vendors (excluding food)"="9520",
                                  "Refuse workers and other elementary workers"="9600",
                                  "Refuse workers"="9610",
                                  "Garbage and recycling collectors"="9611",
                                  "Refuse sorters"="9612",
                                  "Sweepers and related labourers"="9613",
                                  "Other elementary workers"="9620",
                                  "Messengers, package deliverers and luggage porters"="9621",
                                  "Odd job persons"="9622",
                                  "Meter readers and vending-machine collectors"="9623",
                                  "Water and firewood collectors"="9624",
                                  "Elementary workers not elsewhere classified"="9629",
                                  "Not applicable"="66666",
                                  "Refusal"="77777",
                                  "Don't know"="88888",
                                  "No answer"="99999") # Numeric ISCO-08 codes as a factor

ESS16$isco08pcodes<-as.numeric(paste(ESS16$isco08pcodes)) # Numeric ISCO-08 codes
ESS16$isco08pcodes[which(ESS16$isco08pcodes>=66666)]=NA

ESS16$isco08p_postcoded<-factor(nrow(ESS16),levels=c("OC0","OC1","OC2","OC3","OC4",
                                                     "OC5","OC6","OC7","OC8","OC9"))

ESS16$isco08p_postcoded[which(ESS16$isco08pcodes<=999)]="OC0"
ESS16$isco08p_postcoded[which(ESS16$isco08pcodes>=1000 & ESS16$isco08pcodes<2000)]="OC1"
ESS16$isco08p_postcoded[which(ESS16$isco08pcodes>=2000 & ESS16$isco08pcodes<3000)]="OC2"
ESS16$isco08p_postcoded[which(ESS16$isco08pcodes>=3000 & ESS16$isco08pcodes<4000)]="OC3"
ESS16$isco08p_postcoded[which(ESS16$isco08pcodes>=4000 & ESS16$isco08pcodes<5000)]="OC4"
ESS16$isco08p_postcoded[which(ESS16$isco08pcodes>=5000 & ESS16$isco08pcodes<6000)]="OC5"
ESS16$isco08p_postcoded[which(ESS16$isco08pcodes>=6000 & ESS16$isco08pcodes<7000)]="OC6"
ESS16$isco08p_postcoded[which(ESS16$isco08pcodes>=7000 & ESS16$isco08pcodes<8000)]="OC7"
ESS16$isco08p_postcoded[which(ESS16$isco08pcodes>=8000 & ESS16$isco08pcodes<9000)]="OC8"
ESS16$isco08p_postcoded[which(ESS16$isco08pcodes>=9000 & ESS16$isco08pcodes<9999)]="OC9"

# Full-time or part-time
ESS16$wkhtotp[which(ESS16$wktotp>168)]=NA # Note that 168 is the highest acceptable value according to the codebook
ESS16$wkhtotp_grp<-factor(nrow(ESS16),levels=c("FT","PT","TOTAL"))
ESS16$wkhtotp_grp[which(ESS16$wkhtotp<35)]="PT"
ESS16$wkhtotp_grp[which(ESS16$wkhtotp>=35)]="FT"
ESS16$wkhtotp_grp[which(is.na(ESS16$wkhtotp))]="TOTAL" # When there is no info I am just using the total (for analyses code as NA)

#######

# Estimate partner's salary
ESS16$salaryp_est<-numeric(nrow(ESS16))
for (i in 1:nrow(ESS16)){
  if (ESS16$pdwrkp[i]=="Not marked"){
    ESS16$salaryp_est[i]=0} 
  if (ESS16$pdwrkp[i]=="Marked" & is.na(ESS16$isco08p_postcoded[i])){
    ESS16$salaryp_est[i]=NA}
  else{
    if (ESS16$pdwrkp[i]=="Marked" & ESS16$isco08p_postcoded[i]=="OC0"){
      ESS16$salaryp_est[i]=NA}
    if (ESS16$pdwrkp[i]=="Marked" & ESS16$isco08p_postcoded[i]!="OC0"){  
      ESS16$salaryp_est[i]=Earnings$OBS_MEAN[which(Earnings$isco08==ESS16$isco08p_postcoded[i] & Earnings$worktime==ESS16$wkhtotp_grp[i] &
                                                     Earnings$sex==ESS16$gndrpartner[i] & Earnings$age==ESS16$agepartner_grp2[i] &
                                                     Earnings$geo==ESS16$cntry[i])]}
  }} 

#######################################
# Calculate partner's salary group as a function of partner's salary and country average
ESS16$salaryp_grp<-factor(nrow(ESS16),levels=c("No income","Half the average","Average","Double the average"))
for (i in 1:nrow(ESS16)){
  if (is.na(ESS16$salaryp_est[i])){
    ESS16$salaryp_grp[i]=NA
  }
  else{
    if (ESS16$salaryp_est[i]<=(ESS16$average_salary[i]*0.5)){
      ESS16$salaryp_grp[i]="Half the average"
    }
    if (ESS16$salaryp_est[i]>(ESS16$average_salary[i]*0.5) & ESS16$salaryp_est[i]<(ESS16$average_salary[i]*2)){
      ESS16$salaryp_grp[i]="Average"
    }
    if (ESS16$salaryp_est[i]>=(ESS16$average_salary[i]*2)){
      ESS16$salaryp_grp[i]="Double the average"
    }
    if (ESS16$salaryp_est[i]==0){
      ESS16$salaryp_grp[i]="No income"
    }
  }
}

# Distribution of market earnings among partners
ESS16$earnings_dist<-factor(nrow(ESS16),levels=c("No earner","Single earner","Supplementary earner","Double earner"))

ESS16$earnings_dist[which(ESS16$lwpartner=="no" & is.na(ESS16$msalary_est))]=NA
ESS16$earnings_dist[which(ESS16$lwpartner=="no" & ESS16$salary_est==0)]="No earner"
ESS16$earnings_dist[which(ESS16$lwpartner=="no" & ESS16$salary_est>0)]="Single earner"

ESS16$earnings_dist[which(ESS16$lwpartner=="yes" & is.na(ESS16$salary_est))]=NA
ESS16$earnings_dist[which(ESS16$lwpartner=="yes" & is.na(ESS16$salaryp_est))]=NA

ESS16$earnings_dist[which(ESS16$lwpartner=="yes" & ESS16$salary_grp=="No income" & ESS16$salaryp_grp=="No income")]="No earner"

ESS16$earnings_dist[which(ESS16$lwpartner=="yes" & ESS16$salary_grp!="No income" & ESS16$salaryp_grp=="No income")]="Single earner"
ESS16$earnings_dist[which(ESS16$lwpartner=="yes" & ESS16$salary_grp=="No income" & ESS16$salaryp_grp!="No income")]="Single earner"

ESS16$earnings_dist[which(ESS16$lwpartner=="yes" & ESS16$salary_grp!="No income" & !is.na(ESS16$salary_grp) & 
                            ESS16$salaryp_grp!="No income" & !is.na(ESS16$salaryp_grp) & 
                            ESS16$salary_grp!=ESS16$salaryp_grp)]="Supplementary earner"

ESS16$earnings_dist[which(ESS16$lwpartner=="yes" & ESS16$salary_grp!="No income" & !is.na(ESS16$salary_grp) & 
                          ESS16$salary_grp==ESS16$salaryp_grp)]="Double earner"


# Double-earner (0) vs single-earner (1) couples
ESS16$double_single<-numeric(nrow(ESS16))
ESS16$double_single[which(ESS16$earnings_dist=="Single-earner")]=1
ESS16$double_single[which(ESS16$earnings_dist=="Supplementary-earner")]=NA
ESS16$double_single[which(ESS16$earnings_dist=="No earner")]=NA
ESS16$double_single[which(ESS16$earnings_dist=="Double-earner")]=0
ESS16$double_single[which(is.na(ESS16$earnings_dist))]=NA

# Double-earner (0) vs Supplementary-earner (1) couples
ESS16$double_sup<-numeric(nrow(ESS16))
ESS16$double_sup[which(ESS16$earnings_dist=="Single-earner")]=NA
ESS16$double_sup[which(ESS16$earnings_dist=="No earner")]=NA
ESS16$double_sup[which(ESS16$earnings_dist=="Supplementary-earner")]=1
ESS16$double_sup[which(ESS16$earnings_dist=="Double-earner")]=0
ESS16$double_sup[which(is.na(ESS16$earnings_dist))]=NA

## Alternative operationalisation of earnings distribution (simply based on LMS)
ESS16$earnings_dist2<-factor(nrow(ESS16),levels=c("No earner","Single earner","Double earner"))
ESS16$earnings_dist2[which(ESS16$lwpartner=="no" & ESS16$pdwrk=="Not marked")]="No earner"
ESS16$earnings_dist2[which(ESS16$lwpartner=="no" & ESS16$pdwrk=="Marked")]="Single earner"
ESS16$earnings_dist2[which(ESS16$lwpartner=="yes" & ESS16$pdwrk=="Not marked" & ESS16$pdwrkp=="Not marked")]="No earner"
ESS16$earnings_dist2[which(ESS16$lwpartner=="yes" & ESS16$pdwrk=="Not marked" & ESS16$pdwrkp=="Not marked")]="No earner"
ESS16$earnings_dist2[which(ESS16$lwpartner=="yes" & ESS16$pdwrk=="Marked" & ESS16$pdwrkp=="Not marked")]="Single earner"
ESS16$earnings_dist2[which(ESS16$lwpartner=="yes" & ESS16$pdwrk=="Not marked" & ESS16$pdwrkp=="Marked")]="Single earner"
ESS16$earnings_dist2[which(ESS16$lwpartner=="yes" & ESS16$pdwrk=="Marked" & ESS16$pdwrkp=="Marked")]="Double earner"

############################################

## Other relevant variables:

# Education
ESS16$high_second_edu<-factor(nrow(ESS16),levels=c("No","Yes"))

ESS16$high_second_edu[which(ESS16$eisced=="ES-ISCED I , less than lower secondary")]="No"
ESS16$high_second_edu[which(ESS16$eisced=="ES-ISCED II, lower secondary")]="No"
ESS16$high_second_edu[which(ESS16$eisced=="ES-ISCED IIIb, lower tier upper secondary")]="Yes"
ESS16$high_second_edu[which(ESS16$eisced=="ES-ISCED IIIa, upper tier upper secondary")]="Yes"
ESS16$high_second_edu[which(ESS16$eisced=="ES-ISCED IV, advanced vocational, sub-degree")]="Yes"
ESS16$high_second_edu[which(ESS16$eisced=="ES-ISCED V1, lower tertiary education, BA level")]="Yes"
ESS16$high_second_edu[which(ESS16$eisced=="ES-ISCED V2, higher tertiary education, >= MA level")]="Yes"
ESS16$high_second_edu[which(ESS16$eisced=="Don't know")]=NA
ESS16$high_second_edu[which(ESS16$eisced=="No answer")]=NA
ESS16$high_second_edu[which(ESS16$eisced=="Other")]=NA
ESS16$high_second_edu[which(ESS16$eisced=="Refusal")]=NA

# Religiosity
ESS16$rlgdgr<-factor(ESS16$rlgdgr,levels=c("Not at all religious","1","2","3","4","5","6","7","8","9",
                                           "Very religious","Don't know","No answer","Refusal"))
ESS16$rlgdgr[which(ESS16$rlgdgr=="Don't know")]=NA
ESS16$rlgdgr[which(ESS16$rlgdgr=="No answer")]=NA
ESS16$rlgdgr[which(ESS16$rlgdgr=="Refusal")]=NA
ESS16$rlgdgr<-ordered(droplevels(ESS16$rlgdgr))

ESS16$rlgdgr_num<-as.numeric(ESS16$rlgdgr)-1

# Support for gay rights
ESS16$freehms<-factor(ESS16$freehms,levels=c("Disagree strongly","Disagree","Neither agree nor disagree",
                                             "Agree","Agree strongly","Don't know","No answer","Refusal"))
ESS16$freehms[which(ESS16$freehms=="Don't know")]=NA
ESS16$freehms[which(ESS16$freehms=="No answer")]=NA
ESS16$freehms[which(ESS16$freehms=="Refusal")]=NA
ESS16$freehms<-ordered(droplevels(ESS16$freehms))

ESS16$freehms_num<-as.numeric(ESS16$freehms)


# Weights
ESS16$dweight<-as.numeric(ESS16$dweight)
ESS16$anweight<-as.numeric(ESS16$anweight)

############################################

#
# HHoT DATA
#

file_path<-"D:/BIGSSS/Dissertation/Study 3 with Martin Gurin/Data/EUROMOD_2016_data.xlsx"

HHoT_list<-lapply(excel_sheets(file_path),function(sheet){
  read_excel(file_path,sheet=sheet)
})

names(HHoT_list)<-excel_sheets(file_path)

HHoT_list<-lapply(HHoT_list,function(df){
  df[,1:(ncol(df)-3)]
})

HHoT_list<-lapply(HHoT_list,function(df){
  colnames(df)<-c("rshipa","earn_dist","nchild","NDI_A1","NDI_RP1","Diff","Diff_percent","NDI_A2","NDI_FF","NDI_RP2",
                  "Total_RP","Diff_T","Diff_T_percent")
  return(df)
})

HHoT_list <- lapply(HHoT_list, function(df) {
  df[[1]] <- zoo::na.locf(df[[1]], na.rm = FALSE)  # Fill NAs in the first column
  return(df)
})

# Distribution of earnings
HHoT_list <- lapply(HHoT_list, function(df) {
  df$earn_dist2<-recode_factor(df$earn_dist,
                            "0"="No earner","100"="Single earner","100-0"="Single earner","100-100"="Double earner",
                            "100-50"="Supplementary earner","200"="Single earner","200-0"="Single earner",
                            "200-100"="Supplementary earner","200-200"="Double earner","50"="Single earner",
                            "50-0"="Single earner","50-25"="Supplementary earner","50-50"="Double earner"
                            )
  return(df)
})

# Household income
HHoT_list <- lapply(HHoT_list, function(df) {
  df$hhinc<-recode_factor(df$earn_dist,
                               "0"="Low","50"="Low","100"="Middle","200-0"="High",
                               "50-0"="Low","50-25"="Low","50-50"="Low",
                               "100-0"="Middle","100-50"="Middle","100-100"="Middle",
                               "200"="High", "200-100"="High","200-200"="High"
  )
  return(df)
})

# Adding rows for the reference points (Single Individuals, changing only household income and earnings distribution)
for (i in 1:length(HHoT_list)){
HHoT_list[[i]]<-rbind(HHoT_list[[i]],c("Single individual",NA, 0,HHoT_list[[i]]$NDI_RP1[1],HHoT_list[[i]]$NDI_RP1[1],
                                       0,0,0,HHoT_list[[i]]$NDI_RP1[1],0,0,0,0,"No earner","Low"),
                                     c("Single individual",NA, 0,HHoT_list[[i]]$NDI_RP1[3],HHoT_list[[i]]$NDI_RP1[3],
                                       0,0,0,HHoT_list[[i]]$NDI_RP1[3],0,0,0,0,"Single earner","Low"),
                                     c("Single individual",NA, 0,HHoT_list[[i]]$NDI_RP1[5],HHoT_list[[i]]$NDI_RP1[5],
                                       0,0,0,HHoT_list[[i]]$NDI_RP1[5],0,0,0,0,"Single earner","Middle"),
                                     c("Single individual",NA, 0,HHoT_list[[i]]$NDI_RP1[7],HHoT_list[[i]]$NDI_RP1[7],
                                       0,0,0,HHoT_list[[i]]$NDI_RP1[7],0,0,0,0,"Single earner","High")
                      )}
                                       

HHoT_list[[13]]<-HHoT_list[[13]][c(1:68,84:87),] # For some reasons the country 13 has 87 rows, but the extra rows are empty

# Turn the list into a single data frame                         
HHoT<-as.data.frame(do.call(rbind,HHoT_list))
rown<-nrow(HHoT_list[[1]])
HHoT$Country<-c(rep("AT",rown),rep("BE",rown),rep("CZ",rown),
                rep("DE",rown),rep("ES",rown),rep("FI",rown),
                rep("FR",rown),rep("HU",rown),rep("IE",rown),
                rep("IT",rown),rep("NL",rown),rep("PL",rown),
                rep("PT",rown),rep("SE",rown))

# Variables to link: rshipa: single parent, couples or single individual
HHoT$rshipa<-factor(HHoT$rshipa,levels=c("Single parent","Couples - cohabitating","Couples - married", "Single individual"))
ESS16$rshipa<-factor(nrow(ESS16),levels=c("Single parent","Couples - cohabitating","Couples - married", "Single individual"))
ESS16$rshipa[which(ESS16$lwpartner=="no" & ESS16$hasmkids_bin=="no")]="Single individual"
ESS16$rshipa[which(ESS16$lwpartner=="no" & ESS16$hasmkids_bin=="yes")]="Single parent"
ESS16$rshipa[which(ESS16$lwpartner=="yes" & ESS16$maritalstatus_bin=="Not married or in civil union")]="Couples - cohabitating"
ESS16$rshipa[which(ESS16$lwpartner=="yes" & ESS16$maritalstatus_bin=="Married or in civil union")]="Couples - married"
ESS16<-ESS16[-which(is.na(ESS16$rshipa)),]
summary(HHoT$rshipa);summary(ESS16$rshipa)

# Variables to link: number of children
HHoT$nchild<-as.factor(HHoT$nchild)
HHoT$nchild<-recode_factor(HHoT$nchild,"0"="no children","2"="1-2 children","3"="3 children or more")
ESS16$nchild<-ESS16$nmchildren_grp2
summary(HHoT$nchild);summary(ESS16$nchild)

# Variables to link: distribution of earnings
ESS16<-ESS16[-which(is.na(ESS16$earnings_dist)),]
ESS16$earn_dist2<-ESS16$earnings_dist
summary(HHoT$earn_dist2);summary(ESS16$earn_dist2)

# Variables to link: household income
ESS16$hhinc<-ESS16$hinctnta_grp
summary(HHoT$hhinc);summary(ESS16$hhinc) # There are lots of missings at the HH income variable in ESS16 (too many to remove)

# Variables to link: country
HHoT$Country<-as.factor(HHoT$Country)
HHoT$Country<-recode_factor(HHoT$Country,"AT"="Austria","BE"="Belgium","CZ"="Czechia","DE"="Germany",
                            "ES"="Spain","FI"="Finland",
                            "FR"="France","HU"="Hungary","IE"="Ireland","IT"="Italy","NL"="Netherlands",
                            "PL"="Poland","PT"="Portugal","SE"="Sweden")
ESS16<-ESS16[-which(ESS16$cntry=="Norway" | ESS16$cntry=="United Kingdom"),]
ESS16$cntry<-droplevels(ESS16$cntry)
summary(HHoT$Country);summary(ESS16$cntry)

#######

# Variables to impute (NDIs)
HHoT$NDI_A1<-as.numeric(HHoT$NDI_A1)
HHoT$NDI_RP1<-as.numeric(HHoT$NDI_RP1)
HHoT$NDI_FF<-as.numeric(HHoT$NDI_FF)
HHoT$NDI_A2<-as.numeric(HHoT$NDI_A2)
HHoT$Diff<-as.numeric(HHoT$Diff)
HHoT$Diff_percent<-as.numeric(HHoT$Diff_percent)
HHoT$Diff_T<-as.numeric(HHoT$Diff_T)
HHoT$Diff_T_percent<-as.numeric(HHoT$Diff_T_percent)


# Turn to Euro (using conversion rates for the year of study)

# Exchange rates in 2016 (from Eurostat https://ec.europa.eu/eurostat/databrowser/view/ERT_BIL_EUR_A/default/table?lang=en)
ER_CZ=27.034; ER_HU=311.44; ER_PL=4.3632; ER_SE=9.4689

HHoT$Diff_Eur<-HHoT$Diff
HHoT$Diff_Eur[which(HHoT$Country=="Czechia")]=HHoT$Diff_Eur[which(HHoT$Country=="Czechia")]/ER_CZ
HHoT$Diff_Eur[which(HHoT$Country=="Hungary")]=HHoT$Diff_Eur[which(HHoT$Country=="Hungary")]/ER_HU
HHoT$Diff_Eur[which(HHoT$Country=="Poland")]=HHoT$Diff_Eur[which(HHoT$Country=="Poland")]/ER_PL
HHoT$Diff_Eur[which(HHoT$Country=="Sweden")]=HHoT$Diff_Eur[which(HHoT$Country=="Sweden")]/ER_SE

HHoT$Diff_T_Eur<-HHoT$Diff_T
HHoT$Diff_T_Eur[which(HHoT$Country=="Czechia")]=HHoT$Diff_T_Eur[which(HHoT$Country=="Czechia")]/ER_CZ
HHoT$Diff_T_Eur[which(HHoT$Country=="Hungary")]=HHoT$Diff_T_Eur[which(HHoT$Country=="Hungary")]/ER_HU
HHoT$Diff_T_Eur[which(HHoT$Country=="Poland")]=HHoT$Diff_T_Eur[which(HHoT$Country=="Poland")]/ER_PL
HHoT$Diff_T_Eur[which(HHoT$Country=="Sweden")]=HHoT$Diff_T_Eur[which(HHoT$Country=="Sweden")]/ER_SE

HHoT$NDI_A1_Eur<-HHoT$NDI_A1
HHoT$NDI_A1_Eur[which(HHoT$Country=="Czechia")]=HHoT$NDI_A1_Eur[which(HHoT$Country=="Czechia")]/ER_CZ
HHoT$NDI_A1_Eur[which(HHoT$Country=="Hungary")]=HHoT$NDI_A1_Eur[which(HHoT$Country=="Hungary")]/ER_HU
HHoT$NDI_A1_Eur[which(HHoT$Country=="Poland")]=HHoT$NDI_A1_Eur[which(HHoT$Country=="Poland")]/ER_PL
HHoT$NDI_A1_Eur[which(HHoT$Country=="Sweden")]=HHoT$NDI_A1_Eur[which(HHoT$Country=="Sweden")]/ER_SE

HHoT$NDI_RP1_Eur<-HHoT$NDI_RP1
HHoT$NDI_RP1_Eur[which(HHoT$Country=="Czechia")]=HHoT$NDI_RP1_Eur[which(HHoT$Country=="Czechia")]/ER_CZ
HHoT$NDI_RP1_Eur[which(HHoT$Country=="Hungary")]=HHoT$NDI_RP1_Eur[which(HHoT$Country=="Hungary")]/ER_HU
HHoT$NDI_RP1_Eur[which(HHoT$Country=="Poland")]=HHoT$NDI_RP1_Eur[which(HHoT$Country=="Poland")]/ER_PL
HHoT$NDI_RP1_Eur[which(HHoT$Country=="Sweden")]=HHoT$NDI_RP1_Eur[which(HHoT$Country=="Sweden")]/ER_SE

HHoT$NDI_FF_Eur<-HHoT$NDI_FF
HHoT$NDI_FF_Eur[which(HHoT$Country=="Czechia")]=HHoT$NDI_FF_Eur[which(HHoT$Country=="Czechia")]/ER_CZ
HHoT$NDI_FF_Eur[which(HHoT$Country=="Hungary")]=HHoT$NDI_FF_Eur[which(HHoT$Country=="Hungary")]/ER_HU
HHoT$NDI_FF_Eur[which(HHoT$Country=="Poland")]=HHoT$NDI_FF_Eur[which(HHoT$Country=="Poland")]/ER_PL
HHoT$NDI_FF_Eur[which(HHoT$Country=="Sweden")]=HHoT$NDI_FF_Eur[which(HHoT$Country=="Sweden")]/ER_SE

HHoT$NDI_A2_Eur<-HHoT$NDI_A2
HHoT$NDI_A2_Eur[which(HHoT$Country=="Czechia")]=HHoT$NDI_A2_Eur[which(HHoT$Country=="Czechia")]/ER_CZ
HHoT$NDI_A2_Eur[which(HHoT$Country=="Hungary")]=HHoT$NDI_A2_Eur[which(HHoT$Country=="Hungary")]/ER_HU
HHoT$NDI_A2_Eur[which(HHoT$Country=="Poland")]=HHoT$NDI_A2_Eur[which(HHoT$Country=="Poland")]/ER_PL
HHoT$NDI_A2_Eur[which(HHoT$Country=="Sweden")]=HHoT$NDI_A2_Eur[which(HHoT$Country=="Sweden")]/ER_SE



# Turn to PPS
HHoT$PPP<-numeric(nrow(HHoT))
HHoT$PPP[which(HHoT$Country=="Austria")]=1.122
HHoT$PPP[which(HHoT$Country=="Belgium")]=1.147
HHoT$PPP[which(HHoT$Country=="Czechia")]=0.74
HHoT$PPP[which(HHoT$Country=="Germany")]=1.058
HHoT$PPP[which(HHoT$Country=="Ireland")]=1.342
HHoT$PPP[which(HHoT$Country=="Spain")]=0.964
HHoT$PPP[which(HHoT$Country=="France")]=1.139
HHoT$PPP[which(HHoT$Country=="Italy")]=1.033
HHoT$PPP[which(HHoT$Country=="Hungary")]=0.66
HHoT$PPP[which(HHoT$Country=="Netherlands")]=1.144
HHoT$PPP[which(HHoT$Country=="Poland")]=0.596
HHoT$PPP[which(HHoT$Country=="Portugal")]=0.882
HHoT$PPP[which(HHoT$Country=="Finland")]=1.257
HHoT$PPP[which(HHoT$Country=="Sweden")]=1.251

HHoT$Diff_PPS<-HHoT$Diff_Eur/HHoT$PPP
HHoT$Diff_T_PPS<-HHoT$Diff_T_Eur/HHoT$PPP
HHoT$NDI_A1_PPS<-HHoT$NDI_A1_Eur/HHoT$PPP
HHoT$NDI_RP1_PPS<-HHoT$NDI_RP1_Eur/HHoT$PPP
HHoT$NDI_FF_PPS<-HHoT$NDI_FF_Eur/HHoT$PPP
HHoT$NDI_A2_PPS<-HHoT$NDI_A2_Eur/HHoT$PPP

####
# Imputing HHoT data in the ESS dataset 
####

ESS16$GROUPS<-fct_cross(ESS16$cntry,ESS16$rshipa,ESS16$nchild,ESS16$earn_dist2,ESS16$hhinc,
                        keep_empty=T) 
HHoT$GROUPS<-fct_cross(HHoT$Country,HHoT$rshipa,HHoT$nchild,HHoT$earn_dist2,HHoT$hhinc,
                        keep_empty=T)

data_p3<-merge(ESS16,HHoT[,names(HHoT)[c(4:13,17:30)]],by="GROUPS",all.x=T)


data_p3$Diff_ths<-data_p3$Diff/1000
data_p3$Diff_Eur_ths<-data_p3$Diff_Eur/1000
data_p3$Diff_PPS_ths<-data_p3$Diff_PPS/1000
data_p3$NDI_RP1_PPS_ths<-data_p3$NDI_RP1_PPS/1000

# Creating an indicator of missings for the FRR and NDI_RP1 variables
data_p3$Diff_missing<-factor(nrow(data_p3),levels=c("Not missing","Missing"))
data_p3$Diff_missing[which(is.na(data_p3$Diff_Eur_ths))]="Missing"
data_p3$Diff_missing[-which(is.na(data_p3$Diff_Eur_ths))]="Not missing"

data_p3$NDI_RP1_missing<-factor(nrow(data_p3),levels=c("Not missing","Missing"))
data_p3$NDI_RP1_missing[which(is.na(data_p3$NDI_RP1_PPS_ths))]="Missing"
data_p3$NDI_RP1_missing[-which(is.na(data_p3$NDI_RP1_PPS_ths))]="Not missing"

data_p3$Diff_PPS_ths_na<-data_p3$Diff_PPS_ths
data_p3$Diff_PPS_ths_na[which(is.na(data_p3$Diff_PPS))]=0

data_p3$NDI_RP1_PPS_ths_na<-data_p3$NDI_RP1_PPS_ths
data_p3$NDI_RP1_PPS_ths_na[which(is.na(data_p3$NDI_RP1_PPS_ths))]=0

##########

# Average FRR in country excluding single individuals:
data_p3$average_FRR<-numeric(nrow(data_p3))
data_p3$average_FRR[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$rshipa!="Single individual")])
data_p3$average_FRR<-data_p3$average_FRR/1000

# Average FRR for single parents in country:
data_p3$average_FRR_sp<-numeric(nrow(data_p3))
data_p3$average_FRR_sp[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$rshipa=="Single parent")])
data_p3$average_FRR_sp<-data_p3$average_FRR_sp/1000

# Average FRR for couples with children in country:
data_p3$average_FRR_cwc<-numeric(nrow(data_p3))
data_p3$average_FRR_cwc[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild!="no children")])
data_p3$average_FRR_cwc<-data_p3$average_FRR_cwc/1000

# Average FRR for couples without children in country:
data_p3$average_FRR_cnc<-numeric(nrow(data_p3))
data_p3$average_FRR_cnc[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married") & HHoT$nchild=="no children")])
data_p3$average_FRR_cnc<-data_p3$average_FRR_cnc/1000

# Average FRR for families with kids in country:
data_p3$average_FRR_hk<-numeric(nrow(data_p3))
data_p3$average_FRR_hk[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$nchild!="no children")])
data_p3$average_FRR_hk<-data_p3$average_FRR_hk/1000

# Average FRR for families with 1-2 kids in country:
data_p3$average_FRR_child12<-numeric(nrow(data_p3))
data_p3$average_FRR_child12[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$nchild=="1-2 children")])
data_p3$average_FRR_child12<-data_p3$average_FRR_child12/1000

# Average FRR for families with 3 or more kids in country:
data_p3$average_FRR_child3<-numeric(nrow(data_p3))
data_p3$average_FRR_child3[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$nchild=="3 children or more")])
data_p3$average_FRR_child3<-data_p3$average_FRR_child3/1000

# Average FRR for married couples in country:
data_p3$average_FRR_married<-numeric(nrow(data_p3))
data_p3$average_FRR_married[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$rshipa=="Couples - married")])
data_p3$average_FRR_married<-data_p3$average_FRR_married/1000

# Average FRR for cohabitating couples in country:
data_p3$average_FRR_cohabitating<-numeric(nrow(data_p3))
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$rshipa=="Couples - cohabitating")])
data_p3$average_FRR_cohabitating<-data_p3$average_FRR_cohabitating/1000

# Average FRR for couples in country:
data_p3$average_FRR_lwp<-numeric(nrow(data_p3))
data_p3$average_FRR_lwp[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$rshipa %in% c("Couples - cohabitating","Couples - married"))])
data_p3$average_FRR_lwp<-data_p3$average_FRR_lwp/1000

# Average FRR for working parents in country:
data_p3$average_FRR_workp<-numeric(nrow(data_p3))
data_p3$average_FRR_workp[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$nchild!="no children" & HHoT$earn_dist2!="No earner")])
data_p3$average_FRR_workp<-data_p3$average_FRR_workp/1000

# Average FRR for single earner families in country:
data_p3$average_FRR_singleearner<-numeric(nrow(data_p3))
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$earn_dist2=="Single earner")])
data_p3$average_FRR_singleearner<-data_p3$average_FRR_singleearner/1000

# Average FRR for supplementary earner families in country:
data_p3$average_FRR_supearner<-numeric(nrow(data_p3))
data_p3$average_FRR_supearner[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$earn_dist2=="Supplementary earner")])
data_p3$average_FRR_supearner<-data_p3$average_FRR_supearner/1000


# Average FRR for double earner families in country:
data_p3$average_FRR_doubearner<-numeric(nrow(data_p3))
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Austria")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Austria" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Belgium")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Belgium" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Czechia")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Czechia" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Finland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Finland" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="France")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="France" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Germany")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Germany" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Hungary")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Hungary" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Ireland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Ireland" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Italy")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Italy" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Netherlands")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Netherlands" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Poland")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Poland" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Portugal")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Portugal" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Spain")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Spain" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner[which(data_p3$cntry=="Sweden")]=mean(HHoT$Diff_PPS[which(HHoT$Country=="Sweden" & HHoT$earn_dist2=="Double earner")])
data_p3$average_FRR_doubearner<-data_p3$average_FRR_doubearner/1000

############################################

# WFBP indicator https://ec.europa.eu/eurostat/databrowser/view/spr_exp_ffa/default/table?lang=en

data_p3$PPP<-numeric(nrow(data_p3))
data_p3$PPP[which(data_p3$cntry=="Austria")]=1.122
data_p3$PPP[which(data_p3$cntry=="Belgium")]=1.147
data_p3$PPP[which(data_p3$cntry=="Czechia")]=0.74
data_p3$PPP[which(data_p3$cntry=="Germany")]=1.058
data_p3$PPP[which(data_p3$cntry=="Ireland")]=1.342
data_p3$PPP[which(data_p3$cntry=="Spain")]=0.964
data_p3$PPP[which(data_p3$cntry=="France")]=1.139
data_p3$PPP[which(data_p3$cntry=="Italy")]=1.033
data_p3$PPP[which(data_p3$cntry=="Hungary")]=0.66
data_p3$PPP[which(data_p3$cntry=="Netherlands")]=1.144
data_p3$PPP[which(data_p3$cntry=="Poland")]=0.596
data_p3$PPP[which(data_p3$cntry=="Portugal")]=0.882
data_p3$PPP[which(data_p3$cntry=="Finland")]=1.257
data_p3$PPP[which(data_p3$cntry=="Sweden")]=1.251

data_p3$WFBP<-numeric(nrow(data_p3))
data_p3$WFBP[which(data_p3$cntry=="Austria")]=801.53
data_p3$WFBP[which(data_p3$cntry=="Belgium")]=643.9
data_p3$WFBP[which(data_p3$cntry=="Czechia")]=242.26
data_p3$WFBP[which(data_p3$cntry=="Finland")]=558.73
data_p3$WFBP[which(data_p3$cntry=="France")]=493.68
data_p3$WFBP[which(data_p3$cntry=="Germany")]=743.39
data_p3$WFBP[which(data_p3$cntry=="Hungary")]=183.37
data_p3$WFBP[which(data_p3$cntry=="Ireland")]=792.36
data_p3$WFBP[which(data_p3$cntry=="Italy")]=261.5
data_p3$WFBP[which(data_p3$cntry=="Netherlands")]=310.07
data_p3$WFBP[which(data_p3$cntry=="Poland")]=211.77
data_p3$WFBP[which(data_p3$cntry=="Portugal")]=137.94
data_p3$WFBP[which(data_p3$cntry=="Spain")]=128.2
data_p3$WFBP[which(data_p3$cntry=="Sweden")]=629.54

data_p3$WFBP_PPS<-data_p3$WFBP/data_p3$PPP

##

# Alternative WFBP indicator (from )
data_p3$WFBP2<-numeric(nrow(data_p3))
data_p3$WFBP2[which(data_p3$cntry=="Austria")]=20.4
data_p3$WFBP2[which(data_p3$cntry=="Belgium")]=41.4
data_p3$WFBP2[which(data_p3$cntry=="Czechia")]=20.6
data_p3$WFBP2[which(data_p3$cntry=="Finland")]=47.5
data_p3$WFBP2[which(data_p3$cntry=="France")]=36.8
data_p3$WFBP2[which(data_p3$cntry=="Germany")]=28.1
data_p3$WFBP2[which(data_p3$cntry=="Hungary")]=35.8
data_p3$WFBP2[which(data_p3$cntry=="Ireland")]=15.3
data_p3$WFBP2[which(data_p3$cntry=="Italy")]=44.3
data_p3$WFBP2[which(data_p3$cntry=="Netherlands")]=40.6
data_p3$WFBP2[which(data_p3$cntry=="Poland")]=18.6
data_p3$WFBP2[which(data_p3$cntry=="Portugal")]=46.3
data_p3$WFBP2[which(data_p3$cntry=="Spain")]=35.5
data_p3$WFBP2[which(data_p3$cntry=="Sweden")]=75.4

  
############################################

# Some other indicators for robustness (after feedback at PEW)

# GDP pc (data from the World Bank: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?end=2016&locations=EU&most_recent_year_desc=false&start=2015)
data_p3$GDP_pc<-numeric(nrow(data_p3))

data_p3$GDP_pc[which(data_p3$cntry=="Austria")]=45061.5
data_p3$GDP_pc[which(data_p3$cntry=="Belgium")]=41854.5
data_p3$GDP_pc[which(data_p3$cntry=="Czechia")]=18754.0
data_p3$GDP_pc[which(data_p3$cntry=="Finland")]=43451.3
data_p3$GDP_pc[which(data_p3$cntry=="France")]=37024.2
data_p3$GDP_pc[which(data_p3$cntry=="Germany")]=42961.0
data_p3$GDP_pc[which(data_p3$cntry=="Hungary")]=13104.7
data_p3$GDP_pc[which(data_p3$cntry=="Ireland")]=64292.7
data_p3$GDP_pc[which(data_p3$cntry=="Italy")]=31126.3
data_p3$GDP_pc[which(data_p3$cntry=="Netherlands")]=46808.5
data_p3$GDP_pc[which(data_p3$cntry=="Poland")]=12464.0
data_p3$GDP_pc[which(data_p3$cntry=="Portugal")]=19980.3
data_p3$GDP_pc[which(data_p3$cntry=="Spain")]=26740.7
data_p3$GDP_pc[which(data_p3$cntry=="Sweden")]=51820.4

# Gini index (data from the World Bank: https://data.worldbank.org/indicator/SI.POV.GINI?end=2021&locations=EU&most_recent_year_desc=false&start=2021&view=bar&year=2016)
data_p3$Gini<-numeric(nrow(data_p3))

data_p3$Gini[which(data_p3$cntry=="Austria")]=30.8
data_p3$Gini[which(data_p3$cntry=="Belgium")]=27.6
data_p3$Gini[which(data_p3$cntry=="Czechia")]=25.4
data_p3$Gini[which(data_p3$cntry=="Finland")]=27.1
data_p3$Gini[which(data_p3$cntry=="France")]=31.9
data_p3$Gini[which(data_p3$cntry=="Germany")]=31.4
data_p3$Gini[which(data_p3$cntry=="Hungary")]=30.3
data_p3$Gini[which(data_p3$cntry=="Ireland")]=32.8
data_p3$Gini[which(data_p3$cntry=="Italy")]=35.2
data_p3$Gini[which(data_p3$cntry=="Netherlands")]=28.2
data_p3$Gini[which(data_p3$cntry=="Poland")]=31.2
data_p3$Gini[which(data_p3$cntry=="Portugal")]=35.2
data_p3$Gini[which(data_p3$cntry=="Spain")]=35.8
data_p3$Gini[which(data_p3$cntry=="Sweden")]=29.6

# Cohort (grouping of ages to see if respondents are likely to have a child)
data_p3$cohort<-factor(nrow(data_p3),levels=c("<20","20-45",">45"))
data_p3$cohort[which(data_p3$agea<20)]="<20"
data_p3$cohort[which(data_p3$agea>=20 & data_p3$agea<=45)]="20-45"
data_p3$cohort[which(data_p3$agea>45)]=">45"

# Labour market status (4 category like in paper 1):
data_p3$lms<-factor(nrow(data_p3),levels=c("Employed","NEET","In education"))

data_p3$lms[which(data_p3$dngdk=="Marked")]=NA
data_p3$lms[which(data_p3$dngna=="Marked")]=NA
data_p3$lms[which(data_p3$dngref=="Marked")]=NA
data_p3$lms[which(data_p3$pdwrk=="Marked")]="Employed"
data_p3$lms[which(data_p3$uempla=="Marked")]="NEET"
data_p3$lms[which(data_p3$uempli=="Marked")]="NEET"
data_p3$lms[which(data_p3$cmsrv=="Marked")]="NEET"
data_p3$lms[which(data_p3$hswrk=="Marked")]="NEET"
data_p3$lms[which(data_p3$dsbld=="Marked")]="NEET"
data_p3$lms[which(data_p3$rtrd=="Marked")]="NEET"
data_p3$lms[which(data_p3$dngoth=="Marked")]="NEET"
data_p3$lms[which(data_p3$edctn=="Marked")]="In education"  

# Age of youngest kid in household
data_p3$age_ychild_num<-numeric(nrow(data_p3))

for (i in 1:nrow(data_p3)){
  if (data_p3$nmchildren_grp[i]=="no children"){
    data_p3$age_ychild_num[i]=9999}
  if (data_p3$nmchildren_grp[i]!="no children"){
    data_p3$age_ychild_num[i]=min(na.omit(c(data_p3$hhchildage2[i],
                                          data_p3$hhchildage3[i],
                                          data_p3$hhchildage4[i],
                                          data_p3$hhchildage5[i],
                                          data_p3$hhchildage6[i],
                                          data_p3$hhchildage7[i],
                                          data_p3$hhchildage8[i],
                                          data_p3$hhchildage9[i],
                                          data_p3$hhchildage10[i],
                                          data_p3$hhchildage11[i],
                                          data_p3$hhchildage12[i])))
  }}

data_p3$age_ychild<-factor(nrow(data_p3),levels=c("none","youngest under 3","youngest 3 to 5","youngest 6 to 11","youngest 12 to 17"))
data_p3$age_ychild[which(data_p3$age_ychild_num==9999)]="none"
data_p3$age_ychild[which(data_p3$age_ychild_num<3)]="youngest under 3"
data_p3$age_ychild[which(data_p3$age_ychild_num>=3 & data_p3$age_ychild_num<=5)]="youngest 3 to 5"
data_p3$age_ychild[which(data_p3$age_ychild_num>=6 & data_p3$age_ychild_num<=11)]="youngest 6 to 11"
data_p3$age_ychild[which(data_p3$age_ychild_num>=12 & data_p3$age_ychild_num<=17)]="youngest 12 to 17"


############################################

save(ESS16,file="ESS16_Analysis.RData")
save(list=ls(),file="DataStudy3_2016.RData")


load("DataStudy3_2016.RData")
