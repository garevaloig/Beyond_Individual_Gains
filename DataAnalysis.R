##########################################################
## STUDY 3: Family-based redistribution and preferences ##
##########################################################

####
## SCRIPT 3: REGRESSION MODELS
####

library(lme4)
library(lmerTest)
library(ggplot2)
library(ordinal)
library(texreg)
library(sjPlot)
library(ragg)
library(mice)
library(viridis)
library(broom.mixed)
library(lmerTest)
library(dplyr)
library(MASS)
library(brant)

setwd("D:/BIGSSS/Dissertation/Study 3/Scripts/2016Data")
load("DataStudy3_2016.RData")

#load("MICEModels_P3.RData") # Instead of fitting the models every time, given computation times

#M1_1<-MICEModels[[1]]
#M1_2<-MICEModels[[2]]
#M1_3<-MICEModels[[3]]
#M2_1<-MICEModels[[4]]
#M2_2<-MICEModels[[5]]
#M2_3<-MICEModels[[6]]
#M3_1<-MICEModels[[7]]
#M3_2<-MICEModels[[8]]
#M3_3<-MICEModels[[9]]
#M3_4<-MICEModels[[10]]
#M3_5<-MICEModels[[11]]
#M3_6<-MICEModels[[12]]
#M3_7<-MICEModels[[13]]
#M3_8<-MICEModels[[14]]
#M4_1<-MICEModels[[15]]
#M4_2<-MICEModels[[16]]
#M4_3<-MICEModels[[17]]
#M4_4<-MICEModels[[18]]
#M4_5<-MICEModels[[19]]

#############################################################

# Creating a function to get the matrixes to plot each model
#plot_matrix<-function(x){
#  fixed_effect<-fixef(x)[2]
#  coef_table<-coef(x)[[1]]
#  var_slopes<-attr(ranef(x)[[1]],"postVar")[2,2,]
#  se_slopes<-sqrt(var_slopes)
#  ci_lower<-coef_table[,2]-1.96*se_slopes
#  ci_upper<-coef_table[,2]+1.96*se_slopes
#  plot_data <- data.frame(
#    country = rownames(ranef(x)[[1]]),
#    total_effect = coef_table[,2],
#    ci_lower = ci_lower,
#    ci_upper = ci_upper
#  )
#  return(plot_data)
#}

#############################################################

###
# CREATE A SAMPLE FOR ANALYSIS (excludes all missings in variables except Household Income and Redistributive Outputs)
###

data_p3$workingparents<-numeric(nrow(data_p3))
data_p3$workingparents[which(data_p3$hasmkids_bin=="yes" & data_p3$earn_dist2!="No earner")]=1

# We explore all variables in the analysis
summary(data_p3$gincdif) # Preferences for redistribution: 435NAs
summary(data_p3$wrkprbf) # Preferences for work-family balance: 1903 NAs
summary(data_p3$Diff_PPS_ths) # Family-related redistribution: 9537 NAs (add also full version and NA indicator)
summary(data_p3$NDI_RP1_PPS_ths) # Individual-related redistribution: 9537 NAs (add also full version and NA indicator)
summary(data_p3$agea) # Age: 0 NAs
summary(data_p3$gndr) # Gender: 0 NAs
summary(data_p3$hinctnta_num) # Household income: 5106 NAs (add also full version and NA indicator)
summary(data_p3$cohort) # Cohort: 0 NAs
summary(data_p3$lms) # Labour market status: 51 NAs
summary(data_p3$high_second_edu) # Education: 102 NAs
summary(data_p3$rlgdgr_num) # Religiosity: 190 NAs
summary(data_p3$freehms_num) # Attitudes queer rights: 556 NAs
summary(data_p3$single_family) # Single individual or with family: 0 NAs
summary(data_p3$parentpartner) # Family type (children + partnership): 0 NAs
summary(data_p3$relstatus) # Relationship status (single/cohabitating/married): 0 NAs
summary(data_p3$nchild) # Number of children: 0 NAs
summary(data_p3$workingparents) # Working parents indicator: 0 NAs
summary(data_p3$earnings_dist) # Distribution of earnings: 0 NAs
summary(data_p3$regime) # Welfare regime: 0 NAs
summary(data_p3$GDP_pc) # GDP pc: 0 NAs
summary(data_p3$Gini) # Gini Index: 0 NAs
summary(data_p3$cntry) # Country: 0 NAs
summary(data_p3$average_FRR) # Average FRR in country: 0 NAs
summary(data_p3$average_FRR_sp) # Average FRR for single parents in country: 0 NAs
summary(data_p3$average_FRR_cwc) # Average FRR for couples with children in country: 0 NAs
summary(data_p3$average_FRR_cnc) # Average FRR for couples without children in country: 0 NAs
summary(data_p3$average_FRR_lwp) # Average FRR for those who live with their partner in country: 0 NAs
summary(data_p3$average_FRR_married) # Average FRR for married couples in country: 0 NAs
summary(data_p3$average_FRR_hk) # Average FRR for those who have children in country: 0 NAs
summary(data_p3$average_FRR_workp) # Average FRR for working parents in country: 0 NAs
summary(data_p3$WFBP_PPS) # Family-Policies Work-Family Balance Sub-Index (FPI): 0 NAs

data_full<-data_p3[,c("gincdif","gincdif_num","wrkprbf","wrkprbf_num",
                               "Diff_PPS_ths",
                               "NDI_RP1_PPS_ths",
                               "hinctnta_num","hinctnta_num_na","hinctnta_missing",
                               "agea","gndr","cohort","lms","high_second_edu",
                               "rlgdgr_num","freehms_num",
                               "single_family","workingparents","age_ychild",
                               "parentpartner","relstatus","nchild","earnings_dist","earnings_dist2",
                               "cntry","regime","GDP_pc","Gini",
                               "average_FRR","average_FRR_workp","average_FRR_sp","average_FRR_cwc",
                               "average_FRR_cnc","average_FRR_lwp","average_FRR_married",
                               "average_FRR_hk","average_FRR_cohabitating",
                               "average_FRR_child12","average_FRR_child3",
                               "average_FRR_singleearner","average_FRR_supearner","average_FRR_doubearner","WFBP_PPS", 
                               "anweight")]


# Remove the missings from all variables except household income, FRR and IRR
data_full<-data_full[-which(is.na(data_full$gincdif) | is.na(data_full$wrkprbf) | is.na(data_full$lms) |
                 is.na(data_full$high_second_edu) | is.na(data_full$rlgdgr) | is.na(data_full$freehms)),] 

# I also create a grouped HH income variable so that we can use the full sample without imputation in
# those models that do not use the simulated measures
data_full$hhincome<-factor(nrow(data_full),levels=c("Low","Middle","High","Missing"))
data_full$hhincome[which(data_full$hinctnta_num_na<=3)]="Low"
data_full$hhincome[which(data_full$hinctnta_num_na>3 & data_full$hinctnta_num_na<=7)]="Middle"
data_full$hhincome[which(data_full$hinctnta_num_na>=8)]="High"
data_full$hhincome[which(data_full$hinctnta_missing=="Missing")]="Missing"

data_full$hhincome2<-factor(nrow(data_full),levels=c("Low","Middle","High")) # This is the one for imputations
data_full$hhincome2[which(data_full$hinctnta_num_na<=3)]="Low"
data_full$hhincome2[which(data_full$hinctnta_num_na>3 & data_full$hinctnta_num_na<=7)]="Middle"
data_full$hhincome2[which(data_full$hinctnta_num_na>=8)]="High"
data_full$hhincome2[which(data_full$hinctnta_missing=="Missing")]=NA

data_full<-data_full[-which(data_full$agea<18),]

save(ESS16,file="ESS16_Analysis.RData")
save(list=ls(),file="DataStudy3_2016.RData")


###############################################################################
###############################################################################

#####
# MAIN MODELS
#####

set.seed(1234) # Seed for replicability

md.pattern(data_full) # We only have missings in the household income and imputed variables

# Specify the general predictor matrix
pred_matrix<-matrix(rep(0,ncol(data_full)^2),nrow=ncol(data_full),ncol=ncol(data_full))
colnames(pred_matrix)=colnames(data_full);rownames(pred_matrix)=colnames(data_full)

imp_ind<-which(rownames(pred_matrix) %in% c("hhincome2","NDI_RP1_PPS_ths","Diff_PPS_ths"))

#pred_ind_gincdif<-which(colnames(pred_matrix) %in% c("gincdif_num","wrkprbf_num","agea","gndr","lms","high_second_edu","rlgdgr_num",
                                             #"freehms_num","relstatus","nchild","earnings_dist","cntry","GDP_pc","Gini",
                                             #"average_FRR","age_ychild"))
#pred_ind_wfb<-which(colnames(pred_matrix) %in% c("gincdif_num","wrkprbf_num","agea","gndr","lms","high_second_edu","rlgdgr_num",
                                                #"freehms_num","relstatus","nchild","earnings_dist","cntry","GDP_pc","Gini",
                                                #"WFBP_PPS","age_ychild"))

#pred_matrix_gincdif<-pred_matrix
#pred_matrix_gincdif[imp_ind,pred_ind_gincdif]=1

#pred_matrix_wfb<-pred_matrix
#pred_matrix_wfb[imp_ind,pred_ind_wfb]=1

# Generate multiple imputations:
#imputation_gincdif<-mice(data_full,pred=pred_matrix_gincdif,m=10,method=c(rep("",4),"pmm",
#                                             "","","pmm","","","pmm",rep("",40)))
#imputation_wfb<-mice(data_full,pred=pred_matrix_wfb,m=10,method=c(rep("",4),"pmm",
#                                                                          "","","pmm","","","pmm",rep("",40)))

#xyplot(imputation,Diff_PPS_ths~gincdif_num,pch=20,cex=1.4)

#save(ESS16,file="ESS16_Analysis.RData")
#save(list=ls(),file="DataStudy3_2016.RData")

#########################################################

##
# Hypothesis 1
##

# M1: y~single_family+controls

cca_M1_1<-clmm(gincdif~single_family+
               hhincome+agea+gndr+lms+high_second_edu+(1|cntry),
               data=data_full,weights=anweight,link="logit")
summary(cca_M1_1) 

##

cca_M1_2<-clmm(gincdif~single_family+average_FRR+
           hhincome+agea+gndr+lms+high_second_edu+(1|cntry),
           data=data_full,weights=anweight,link="logit")

summary(cca_M1_2)


##

cca_M1_3<-clmm(gincdif~single_family*average_FRR+
               hhincome+agea+gndr+lms+high_second_edu+(1+single_family|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M1_3)

##

wordreg(list(cca_M1_1,cca_M1_2,cca_M1_3),
        #custom.coef.names=c("Working parents","Individual NDI",
        #"Household NDI","Age","Gender: Male","NEET","In education",
        #"Higher secondary education",
        #"Intercept 1|2","Intercept 2|3","Intercept 3|4",
        #"Family-Related Redistribution","Country-Average FRR","Working parents:Country-Average FRR working parents",
        #"Cohort 20-45","Cohort >45","GDP pc","Gini"),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="H1_CCAModels.doc")

#########


##
# Hypothesis 2
##

pred_ind_M2_1<-which(colnames(pred_matrix) %in% c("gincdif","Diff_PPS_ths",
                                                    "hhincome2","agea","gndr","lms","high_second_edu","cntry"))
pred_matrix_M2_1<-pred_matrix
pred_matrix_M2_1[imp_ind,pred_ind_M2_1]=1
imputation_M2_1<-mice(data_full,pred=pred_matrix_M2_1,m=10,method=c(rep("",4),"pmm","pmm",rep("",39),"pmm"))

M2_1<-with(imputation_M2_1,clmm(gincdif~Diff_PPS_ths+hhincome2+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(M2_1)) 

mean(sapply(M2_1$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(M2_1$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

pred_ind_M2_2<-which(colnames(pred_matrix) %in% c("gincdif","Diff_PPS_ths","single_family",
                                                  "hhincome2","agea","gndr","lms","high_second_edu","cntry"))
pred_matrix_M2_2<-pred_matrix
pred_matrix_M2_2[imp_ind,pred_ind_M2_2]=1
imputation_M2_2<-mice(data_full,pred=pred_matrix_M2_2,m=10,method=c(rep("",4),"pmm","pmm",rep("",39),"pmm"))

M2_2<-with(imputation_M2_2,clmm(gincdif~Diff_PPS_ths+single_family+
                                     hhincome2+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(M2_2)) 

mean(sapply(M2_2$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(M2_2$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

pred_ind_M2_3<-which(colnames(pred_matrix) %in% c("gincdif","Diff_PPS_ths","relstatus","nchild",
                                                  "hhincome2","agea","gndr","lms","high_second_edu","cntry"))
pred_matrix_M2_3<-pred_matrix
pred_matrix_M2_3[imp_ind,pred_ind_M2_3]=1
imputation_M2_3<-mice(data_full,pred=pred_matrix_M2_3,m=10,method=c(rep("",4),"pmm","pmm",rep("",39),"pmm"))


M2_3<-with(imputation_M2_3,clmm(gincdif~Diff_PPS_ths+relstatus+nchild+hhincome2+
                                     #NDI_RP1_PPS_ths+hinctnta_num+
                                     agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(M2_3)) 

mean(sapply(M2_3$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(M2_3$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

#wordreg(list(M2_1,M2_2,M2_3,M2_4),
        ##custom.coef.names=c("Working parents","Individual NDI",
        ##"Household NDI","Age","Gender: Male","NEET","In education",
        ##"Higher secondary education",
        ##"Intercept 1|2","Intercept 2|3","Intercept 3|4",
        ##"Family-Related Redistribution","Country-Average FRR","Working parents:Country-Average FRR working parents",
        ##"Cohort 20-45","Cohort >45","GDP pc","Gini"),
        #stars = c(0.001, 0.01, 0.05, 0.1),
        #file="H2_Models.doc")

##
# Hypothesis 3
##

cca_M3_1<-clmm(wrkprbf~workingparents+hhincome+
               #NDI_RP1_PPS_ths+hinctnta_num+
                 agea+gndr+lms+high_second_edu+(1|cntry),
               data=data_full,weights=anweight,link="logit")
            
summary(cca_M3_1)


##

cca_M3_2<-clmm(wrkprbf~workingparents+WFBP_PPS+hhincome+
               #NDI_RP1_PPS_ths+hinctnta_num+
               agea+gndr+lms+high_second_edu+(1|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M3_2)

##


cca_M3_3<-clmm(wrkprbf~workingparents*WFBP_PPS+hhincome+
                 #NDI_RP1_PPS_ths+hinctnta_num+
                 agea+gndr+lms+high_second_edu+(1+workingparents|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M3_3)

##

cca_M3_4<-clmm(wrkprbf~workingparents+average_FRR_workp+hhincome+
              #NDI_RP1_PPS_ths+hinctnta_num+
              agea+gndr+lms+high_second_edu+(1|cntry),
              data=data_full,weights=anweight,link="logit")

summary(cca_M3_4)

##

cca_M3_5<-clmm(wrkprbf~workingparents*average_FRR_workp+hhincome+
                 #NDI_RP1_PPS_ths+hinctnta_num+
                 agea+gndr+lms+high_second_edu+(1|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M3_5)

## The previous model but with a quadratic cross-level interaction

data_full$average_FRR_workp_q <- data_full$average_FRR_workp^2

cca_M3_5_q<-clmm(wrkprbf~workingparents*average_FRR_workp+
                   workingparents*average_FRR_workp_q+
                   hhincome+
                 #NDI_RP1_PPS_ths+hinctnta_num+
                 agea+gndr+lms+high_second_edu+(1+workingparents|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M3_5_q)

cca_M3_6_q<-clmm(wrkprbf~workingparents+average_FRR_workp+
                   average_FRR_workp_q+
                   hhincome+
                   #NDI_RP1_PPS_ths+hinctnta_num+
                   agea+gndr+lms+high_second_edu+(1|cntry),
                 data=data_full,weights=anweight,link="logit")

##

cca_M3_6<-clmm(wrkprbf~age_ychild+hhincome+
           #NDI_RP1_PPS_ths+hinctnta_num+
           agea+gndr+lms+high_second_edu+(1|cntry),
           data=data_full,weights=anweight,link="logit")

summary(cca_M3_6) 


##


cca_M3_7<-clmm(wrkprbf~age_ychild+average_FRR_workp+hhincome+
      #NDI_RP1_PPS_ths+hinctnta_num+
      agea+gndr+lms+high_second_edu+(1|cntry),
      data=data_full,weights=anweight,link="logit")

summary(cca_M3_7) 


##
cca_M3_8<-clmm(wrkprbf~age_ychild*average_FRR_workp+hhincome+ 
               #NDI_RP1_PPS_ths+hinctnta_num+
               agea+gndr+lms+high_second_edu+(1+age_ychild|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M3_8)

##
cca_M3_9<-clmm(wrkprbf~workingparents+average_FRR_workp+age_ychild+relstatus+hhincome+ 
                 #NDI_RP1_PPS_ths+hinctnta_num+
                 agea+gndr+lms+high_second_edu+(1+workingparents|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M3_9)

########################

wordreg(list(cca_M3_1,cca_M3_4,cca_M3_5,cca_M3_6,cca_M3_7,cca_M3_8),
        #custom.coef.names=c("Working parents","Individual NDI",
        #"Household NDI","Age","Gender: Male","NEET","In education",
        #"Higher secondary education",
        #"Intercept 1|2","Intercept 2|3","Intercept 3|4",
        #"Family-Related Redistribution","Country-Average FRR","Working parents:Country-Average FRR working parents",
        #"Cohort 20-45","Cohort >45","GDP pc","Gini"),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="H3_Models.doc")

##
# Hypothesis 4
##

pred_ind_M4_1<-which(colnames(pred_matrix) %in% c("wrkprbf","Diff_PPS_ths",
                                                  "hhincome2","agea","gndr","lms","high_second_edu","cntry"))
pred_matrix_M4_1<-pred_matrix
pred_matrix_M4_1[imp_ind,pred_ind_M4_1]=1
imputation_M4_1<-mice(data_full,pred=pred_matrix_M4_1,m=10,method=c(rep("",4),"pmm","pmm",rep("",39),"pmm"))

M4_1<-with(imputation_M4_1,clmm(wrkprbf~Diff_PPS_ths+
                                     hhincome2+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(M4_1)) 

mean(sapply(M4_1$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(M4_1$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

pred_ind_M4_2<-which(colnames(pred_matrix) %in% c("wrkprbf","Diff_PPS_ths","workingparents","relstatus",
                                                    "hhincome2","agea","gndr","lms","high_second_edu","cntry"))
pred_matrix_M4_2<-pred_matrix
pred_matrix_M4_2[imp_ind,pred_ind_M4_2]=1
imputation_M4_2<-mice(data_full,pred=pred_matrix_M4_2,m=10,method=c(rep("",4),"pmm","pmm",rep("",39),"pmm"))

M4_2<-with(imputation_M4_2,clmm(wrkprbf~Diff_PPS_ths+workingparents+relstatus+hhincome2+
                                  #NDI_RP1_PPS_ths+hinctnta_num+
                                  agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(M4_2)) 

mean(sapply(M4_2$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(M4_2$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

pred_ind_M4_3<-which(colnames(pred_matrix) %in% c("wrkprbf","Diff_PPS_ths","relstatus","nchild",
                                                  "hhincome2","agea","gndr","lms","high_second_edu","cntry"))
pred_matrix_M4_3<-pred_matrix
pred_matrix_M4_3[imp_ind,pred_ind_M4_3]=1
imputation_M4_3<-mice(data_full,pred=pred_matrix_M4_3,m=10,method=c(rep("",4),"pmm","pmm",rep("",39),"pmm"))

M4_3<-with(imputation_M4_3,clmm(wrkprbf~Diff_PPS_ths+relstatus+nchild+hhincome2+
                                     #NDI_RP1_PPS_ths+hinctnta_num+
                                    agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(M4_3)) 

mean(sapply(M4_3$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(M4_3$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

pred_ind_M4_4<-which(colnames(pred_matrix) %in% c("wrkprbf","Diff_PPS_ths","relstatus","age_ychild",
                                                  "hhincome2","agea","gndr","lms","high_second_edu","cntry"))
pred_matrix_M4_4<-pred_matrix
pred_matrix_M4_4[imp_ind,pred_ind_M4_4]=1
imputation_M4_4<-mice(data_full,pred=pred_matrix_M4_4,m=10,method=c(rep("",4),"pmm","pmm",rep("",39),"pmm"))

M4_4<-with(imputation_M4_4,clmm(wrkprbf~Diff_PPS_ths+age_ychild+relstatus+hhincome2+
                                     #NDI_RP1_PPS_ths+hinctnta_num+
                                     agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(M4_4)) 

mean(sapply(M4_4$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(M4_4$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

pred_ind_M4_5<-which(colnames(pred_matrix) %in% c("wrkprbf","Diff_PPS_ths","relstatus","nchild",
                                                  "hhincome2","agea","gndr","lms","high_second_edu","cntry"))
pred_matrix_M4_5<-pred_matrix
pred_matrix_M4_5[imp_ind,pred_ind_M4_4]=1
imputation_M4_5<-mice(data_full,pred=pred_matrix_M4_5,m=10,method=c(rep("",4),"pmm","pmm",rep("",39),"pmm"))

M4_5<-with(imputation_M4_5,clmm(wrkprbf~Diff_PPS_ths*age_ychild+relstatus+
                                  NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(M4_5)) 

mean(sapply(M4_5$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(M4_5$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

#####
# Save all models
#####

save(list=ls(),file="ResultsMay25.RData")

#MICEModels<-list(M1_1,M1_2,M1_3,M2_1,M2_2,M2_3,M3_1,M3_2,M3_3,M3_4,M3_5,M3_6,M3_7,M3_8,M4_1,M4_2,M4_3,M4_4,M4_5)

#save(MICEModels,file="MICEModels_P3.RData")
#load("MICEModels_P3.RData")


################################################################################
################################################################################

#####
# EXTRA MODELS
#####

##
# Hypothesis 1
##

cca_M1_1<-clmm(gincdif~single_family+#hhincome+
          NDI_RP1_PPS_ths+hinctnta_num+
          agea+gndr+lms+high_second_edu+(1|cntry),
          data=data_full,weights=anweight,link="logit")

summary(cca_M1_1) 

##

cca_M1_2<-clmm(gincdif~single_family+average_FRR+#hhincome+
              NDI_RP1_PPS_ths+hinctnta_num+
              agea+gndr+lms+high_second_edu+(1|cntry),
              data=data_full,weights=anweight,link="logit")

summary(cca_M1_2)

##

cca_M1_3<-clmm(gincdif~single_family*average_FRR+#hhincome+
          NDI_RP1_PPS_ths+hinctnta_num+
          agea+gndr+lms+high_second_edu+(1+single_family|cntry),
          data=data_full,weights=anweight,link="logit")

summary(cca_M1_3)

wordreg(list(cca_M1_1,cca_M1_2,cca_M1_3),
        #custom.coef.names=c("Working parents","Individual NDI",
        #"Household NDI","Age","Gender: Male","NEET","In education",
        #"Higher secondary education",
        #"Intercept 1|2","Intercept 2|3","Intercept 3|4",
        #"Family-Related Redistribution","Country-Average FRR","Working parents:Country-Average FRR working parents",
        #"Cohort 20-45","Cohort >45","GDP pc","Gini"),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="H1_CCAModels.doc")

##
# Hypothesis 2
##

cca_M2_1<-clmm(gincdif~Diff_PPS_ths+
          NDI_RP1_PPS_ths+hinctnta_num+
          agea+gndr+lms+high_second_edu+(1|cntry),
          data=data_full,weights=anweight,link="logit")

summary(cca_M2_1)

cca_M2_2<-clmm(gincdif~Diff_PPS_ths+relstatus+nchild+
          NDI_RP1_PPS_ths+hinctnta_num+
          agea+gndr+lms+high_second_edu+(1|cntry),
          data=data_full,weights=anweight,link="logit")

summary(cca_M2_2)

cca_M2_3<-clmm(gincdif~Diff_PPS_ths+single_family+
          NDI_RP1_PPS_ths+hinctnta_num+
          agea+gndr+lms+high_second_edu+(1|cntry),
          data=data_full,weights=anweight,link="logit")

summary(cca_M2_3) 

wordreg(list(cca_M2_1,cca_M2_2,cca_M2_3),
        #custom.coef.names=c("Working parents","Individual NDI",
        #"Household NDI","Age","Gender: Male","NEET","In education",
        #"Higher secondary education",
        #"Intercept 1|2","Intercept 2|3","Intercept 3|4",
        #"Family-Related Redistribution","Country-Average FRR","Working parents:Country-Average FRR working parents",
        #"Cohort 20-45","Cohort >45","GDP pc","Gini"),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="H2_CCAModels.doc")

##
# Hypothesis 3
##

cca_M3_1<-clmm(wrkprbf~workingparents+
          NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry),
          data=data_full,weights=anweight,link="logit")

summary(cca_M3_1) 

cca_M3_2<-clmm(wrkprbf~workingparents+WFBP_PPS+
          NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry),
          data=data_full,weights=anweight,link="logit")

summary(cca_M3_2)

cca_M3_3<-clmm(wrkprbf~workingparents*WFBP_PPS+
          NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1+workingparents|cntry),
          data=data_full,weights=anweight,link="logit")

summary(cca_M3_3)

cca_M3_4<-clmm(wrkprbf~workingparents+average_FRR_workp+
          NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry),
          data=data_full,weights=anweight,link="logit")

summary(cca_M3_4) 

cca_M3_5<-clmm(wrkprbf~workingparents*average_FRR_workp+
          NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1+workingparents|cntry),
          data=data_full,weights=anweight,link="logit")

summary(cca_M3_5)


cca_M3_6<-clmm(wrkprbf~age_ychild*average_FRR_workp+hhincome+ ### This is it!!!
                 #NDI_RP1_PPS_ths+hinctnta_num+
                 agea+gndr+lms+high_second_edu+(1+age_ychild|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M3_6)

# Plot this:
lm_M3_6<-lmer(wrkprbf_num~age_ychild*average_FRR_workp+hhincome+
                         #NDI_RP1_PPS_ths+hinctnta_num+
                         agea+gndr+lms+high_second_edu+(1+age_ychild|cntry),
                       data=data_full,weights=anweight)
#,link="logit"))
library(ggeffects)

# Compute marginal effects for the interaction term
marginal_effects <- ggpredict(lm_M3_6, terms = c("age_ychild", "average_FRR_workp"))

# Plot
plot(marginal_effects) + 
  theme_minimal() +
  labs(title = "Marginal Effects of Age of Youngest Child and FRR",
       x = "Age of Youngest Child",
       y = "Predicted Work Preference")

# Get predicted values: outcome vs moderator, for each level of the focal variable
mfx_lines <- ggpredict(lm_M3_6, terms = c("average_FRR_workp", "age_ychild"))

# Plot regression lines (with CI ribbon if you want)
ggplot(mfx_lines, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
  labs(
    title = "Interaction Effect: average_FRR_workp × age_ychild",
    x = "Average FRR for working parents",
    y = "Preferences for Work Family Balance Policies",
    color = "Youngest Child's Age",
    fill = "Youngest Child's Age"
  ) +
 # xlim(0,1)+
  theme_minimal()



wordreg(list(cca_M3_1,cca_M3_2,cca_M3_3,cca_M3_4,cca_M3_5,cca_M3_6),
        #custom.coef.names=c("Working parents","Individual NDI",
        #"Household NDI","Age","Gender: Male","NEET","In education",
        #"Higher secondary education",
        #"Intercept 1|2","Intercept 2|3","Intercept 3|4",
        #"Family-Related Redistribution","Country-Average FRR","Working parents:Country-Average FRR working parents",
        #"Cohort 20-45","Cohort >45","GDP pc","Gini"),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="H3_CCAModels.doc")

##
# Hypothesis 4
##

cca_M4_1<-clmm(wrkprbf~Diff_PPS_ths+
               NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M4_1)



##

cca_M4_2<-clmm(wrkprbf~Diff_PPS_ths+relstatus+nchild+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M4_2)

##

cca_M4_3<-clmm(wrkprbf~Diff_PPS_ths+workingparents+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M4_3)

cca_M4_4<-clmm(wrkprbf~Diff_PPS_ths*workingparents+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M4_4)

cca_M4_5<-clmm(wrkprbf~Diff_PPS_ths*age_ychild+
                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry),
               data=data_full,weights=anweight,link="logit")

summary(cca_M4_5)

##

wordreg(list(cca_M4_1,cca_M4_2,cca_M4_3,cca_M4_4),
        #custom.coef.names=c("Working parents","Individual NDI",
        #"Household NDI","Age","Gender: Male","NEET","In education",
        #"Higher secondary education",
        #"Intercept 1|2","Intercept 2|3","Intercept 3|4",
        #"Family-Related Redistribution","Country-Average FRR","Working parents:Country-Average FRR working parents",
        #"Cohort 20-45","Cohort >45","GDP pc","Gini"),
        stars = c(0.001, 0.01, 0.05, 0.1),
        file="H4_CCAModels.doc")

####################

# Trying the alternative WFBP indicator

data_full2<-data_p3[,c("gincdif","gincdif_num","wrkprbf","wrkprbf_num",
                               "Diff_PPS_ths",
                               "NDI_RP1_PPS_ths",
                               "hinctnta_num","hinctnta_num_na","hinctnta_missing",
                               "agea","gndr","cohort","lms","high_second_edu",
                               "rlgdgr_num","freehms_num",
                               "single_family","workingparents","age_ychild",
                               "parentpartner","relstatus","nchild","earnings_dist","earnings_dist2",
                               "cntry","regime","GDP_pc","Gini",
                               "average_FRR","average_FRR_workp","average_FRR_sp","average_FRR_cwc",
                               "average_FRR_cnc","average_FRR_lwp","average_FRR_married",
                               "average_FRR_hk","average_FRR_cohabitating",
                               "average_FRR_child12","average_FRR_child3",
                               "average_FRR_singleearner","average_FRR_supearner","average_FRR_doubearner","WFBP2", 
                               "anweight")]

# Remove the missings from all variables except household income, FRR and IRR
data_full2<-data_full2[-which(is.na(data_full2$gincdif) | is.na(data_full2$wrkprbf) | is.na(data_full2$lms) |
                              is.na(data_full2$high_second_edu) | is.na(data_full2$rlgdgr) | is.na(data_full2$freehms)),] 

# I also create a grouped HH income variable so that we can use the full sample without imputation in
# those models that do not use the simulated measures
data_full2$hhincome<-factor(nrow(data_full2),levels=c("Low","Middle","High","Missing"))
data_full2$hhincome[which(data_full2$hinctnta_num_na<=3)]="Low"
data_full2$hhincome[which(data_full2$hinctnta_num_na>3 & data_full2$hinctnta_num_na<=7)]="Middle"
data_full2$hhincome[which(data_full2$hinctnta_num_na>=8)]="High"
data_full2$hhincome[which(data_full2$hinctnta_missing=="Missing")]="Missing"

data_full2$hhincome2<-factor(nrow(data_full2),levels=c("Low","Middle","High")) # This is the one for imputations
data_full2$hhincome2[which(data_full2$hinctnta_num_na<=3)]="Low"
data_full2$hhincome2[which(data_full2$hinctnta_num_na>3 & data_full2$hinctnta_num_na<=7)]="Middle"
data_full2$hhincome2[which(data_full2$hinctnta_num_na>=8)]="High"
data_full2$hhincome2[which(data_full2$hinctnta_missing=="Missing")]=NA

data_full2<-data_full2[-which(data_full2$agea<18),]

cca_M3_2_WFBP<-clmm(wrkprbf~workingparents+WFBP2+hhincome+
                 #NDI_RP1_PPS_ths+hinctnta_num+
                 agea+gndr+lms+high_second_edu+(1|cntry),
               data=data_full2,weights=anweight,link="logit")

summary(cca_M3_2)

##


cca_M3_3_WFBP<-clmm(wrkprbf~workingparents*WFBP2+hhincome+
                 #NDI_RP1_PPS_ths+hinctnta_num+
                 agea+gndr+lms+high_second_edu+(1+workingparents|cntry),
               data=data_full2,weights=anweight,link="logit")

summary(cca_M3_3_WFBP)

####################

save(ESS16,file="ESS16_Analysis.RData")
save(list=ls(),file="DataStudy3_2016.RData")

####################

# SOME PLOTS:

##
# Marginal effects for the interaction between having a family and country-level average FRR
##

# Coefficients
b_family <- summary(pool(M1_3))$estimate[5]
b_interaction <- summary(pool(M1_3))$estimate[14]
se_b_family <- summary(pool(M1_3))$std.error[5]
se_b_interaction <- summary(pool(M1_3))$std.error[14]

# Values of average_FRR
FRR_vals <- seq(0, 0.5, length.out = 100)

# Marginal effects and SE
marginal_effect <- b_family + b_interaction * FRR_vals
se_marginal <- sqrt(se_b_family^2 + (FRR_vals^2 * se_b_interaction^2))

# Confidence intervals
upper <- marginal_effect + 1.96 * se_marginal
lower <- marginal_effect - 1.96 * se_marginal

# Data frame
marginal_df <- data.frame(
  FRR = FRR_vals,
  marginal_effect = marginal_effect,
  lower = lower,
  upper = upper
)

# Plot
plot1.3<-ggplot(marginal_df, aes(x = FRR, y = marginal_effect)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Marginal Effect of Single-Parent Family by average_FRR",
       x = "average_FRR",
       y = "Marginal Effect of Single-Parent Family") +
  theme_minimal()

##########

##
# Marginal effects for the interaction between being a working-parent and average FRR for this family type
##

# Coefficients
b_wparents <- summary(pool(M3_5))$estimate[4]
b_interaction <- summary(pool(M3_5))$estimate[13]
se_b_wparents <- summary(pool(M3_5))$std.error[4]
se_b_interaction <- summary(pool(M3_5))$std.error[13]

# Values of average_FRR
FRR_vals <- seq(0, 0.7, length.out = 100)

# Marginal effects and SE
marginal_effect <- b_wparents + b_interaction * FRR_vals
se_marginal <- sqrt(se_b_wparents^2 + (FRR_vals^2 * se_b_interaction^2))

# Confidence intervals
upper <- marginal_effect + 1.96 * se_marginal
lower <- marginal_effect - 1.96 * se_marginal

# Data frame
marginal_df <- data.frame(
  FRR = FRR_vals,
  marginal_effect = marginal_effect,
  lower = lower,
  upper = upper
)

# Plot
plot3.5<-ggplot(marginal_df, aes(x = FRR, y = marginal_effect)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Marginal Effect of Single-Parent Family by average_FRR",
       x = "average_FRR",
       y = "Marginal Effect of Single-Parent Family") +
  theme_minimal() 

###
# EXTRA ANALYSES: For WFB policies, try to divide the number of children depending on the age of those,
# and use Eurostat data for policy context (see Table 1 in Doblyte and Tejero, 2021).
# Take also into account single parents?
###

####################

# ROBUSTNESS:

### 
# Parallel slopes assumption:
###

# Gincdif models
ps_gincdif<-polr(gincdif~single_family*average_FRR+NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                   high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths,
                 data=data_full,weights=anweight,Hess=T)
brant(ps_gincdif)
Para
stargazer(brant(ps_gincdif)[,c(1,3)],type="latex")

# Wrkprbf models
ps_wrkprbf<-polr(wrkprbf~workingparents*average_FRR_workp+
                   NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+
                   high_second_edu+rlgdgr_num+freehms_num+Diff_PPS_ths,
                 data=data_full,weights=anweight,Hess=T)
brant(ps_wrkprbf)

stargazer(brant(ps_wrkprbf)[,c(1,3)],type="latex")

#########

### 
# Linear models with multiple imputations
###

##
# Hypothesis 1
##

# M1: y~single_family+controls

lm1_1<-with(imputation_gincdif,lmer(gincdif_num~single_family+
                                     NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(lm1_1)) 

mean(sapply(lm1_1$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm1_1$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

lm1_2<-with(imputation_gincdif,lmer(gincdif_num~single_family+average_FRR+
                                     NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(lm1_2)) 

mean(sapply(lm1_2$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm1_2$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

lm1_3<-with(imputation_gincdif,lmer(gincdif_num~single_family*average_FRR+
                                     NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+
                                     (1+single_family|cntry)))

summary(pool(lm1_3)) 

mean(sapply(lm1_3$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm1_3$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

#########


##
# Hypothesis 2
##

lm2_1<-with(imputation_gincdif,lmer(gincdif_num~Diff_PPS_ths+
                                     NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(lm2_1)) 

mean(sapply(lm2_1$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm2_1$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

lm2_2<-with(imputation_gincdif,lmer(gincdif_num~Diff_PPS_ths+relstatus+nchild+
                                     NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(lm2_2)) 

mean(sapply(lm2_2$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm2_2$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

lm2_3<-with(imputation_gincdif,lmer(gincdif_num~Diff_PPS_ths+single_family+
                                     NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(lm2_3)) 

mean(sapply(lm2_3$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm2_3$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##
# Hypothesis 3
##

lm3_1<-with(imputation_wfb,lmer(wrkprbf_num~workingparents+
                                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(lm3_1)) 

mean(sapply(lm3_1$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm3_1$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

lm3_2<-with(imputation_wfb,lmer(wrkprbf_num~workingparents+WFBP_PPS+
                                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(lm3_2)) 

mean(sapply(lm3_2$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm3_2$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

lm3_3<-with(imputation_wfb,lmer(wrkprbf_num~workingparents*WFBP_PPS+
                                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1+workingparents|cntry)))

summary(pool(lm3_3))

mean(sapply(lm3_3$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm3_3$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

lm3_4<-with(imputation_wfb,lmer(wrkprbf_num~workingparents+average_FRR_workp+
                                 NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(lm3_4)) 

mean(sapply(lm3_4$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm3_4$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##
lm3_5<-with(imputation_gincdif,lmer(wrkprbf_num~workingparents*average_FRR_workp+
                                     NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1+workingparents|cntry)))

summary(pool(lm3_5))

mean(sapply(lm3_5$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm3_5$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)


##
# Hypothesis 4
##

lm4_1<-with(imputation_gincdif,lmer(wrkprbf_num~Diff_PPS_ths+
                                     NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(lm4_1)) 

mean(sapply(lm4_1$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm4_1$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

lm4_2<-with(imputation_gincdif,lmer(wrkprbf_num~Diff_PPS_ths+relstatus+nchild+
                                     NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(lm4_2)) 

mean(sapply(lm4_2$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm4_2$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

lm4_3<-with(imputation_gincdif,lmer(wrkprbf_num~Diff_PPS_ths+workingparents+
                                     NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(lm4_3)) 

mean(sapply(lm4_3$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm4_3$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

##

lm4_4<-with(imputation_gincdif,lmer(wrkprbf_num~Diff_PPS_ths*workingparents+
                                      NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+(1|cntry)))

summary(pool(lm4_4)) 

mean(sapply(lm4_4$analyses,AIC)) # Obtain mean of the models' AIC
mean(sapply(lm4_4$analyses, function(model) {
  as.numeric(VarCorr(model))  # Extract mean variance of random intercept
})
)

