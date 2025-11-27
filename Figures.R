####
## SCRIPT 5: PLOTS OF RANDOM SLOPES MODELS
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
library(tidyr)

setwd("D:/BIGSSS/Dissertation/Study 3/Scripts/2016Data")
load("DataStudy3_2016.RData")

load("RSModels_P3.RData")

#############################

# GENERATE IMPUTATIONS

set.seed(1234) # Seed for replicability

md.pattern(data_full) # We only have missings in the household income and imputed variables

# Specify the general predictor matrix
pred_matrix<-matrix(rep(0,ncol(data_full)^2),nrow=ncol(data_full),ncol=ncol(data_full))
colnames(pred_matrix)=colnames(data_full);rownames(pred_matrix)=colnames(data_full)

imp_ind<-which(rownames(pred_matrix) %in% c("hhincome2","NDI_RP1_PPS_ths","Diff_PPS_ths"))

pred_ind_gincdif<-which(colnames(pred_matrix) %in% c("gincdif_num","wrkprbf_num","agea","gndr","lms","high_second_edu","rlgdgr_num",
"freehms_num","relstatus","nchild","earnings_dist","cntry","GDP_pc","Gini",
"average_FRR","age_ychild"))
pred_ind_wfb<-which(colnames(pred_matrix) %in% c("gincdif_num","wrkprbf_num","agea","gndr","lms","high_second_edu","rlgdgr_num",
"freehms_num","relstatus","nchild","earnings_dist","cntry","GDP_pc","Gini",
"WFBP_PPS","age_ychild"))

pred_matrix_gincdif<-pred_matrix
pred_matrix_gincdif[imp_ind,pred_ind_gincdif]=1

pred_matrix_wfb<-pred_matrix
pred_matrix_wfb[imp_ind,pred_ind_wfb]=1

 Generate multiple imputations:
imputation_gincdif<-mice(data_full,pred=pred_matrix_gincdif,m=10,method=c(rep("",4),"pmm",
                                             "","","pmm","","","pmm",rep("",40)))
imputation_wfb<-mice(data_full,pred=pred_matrix_wfb,m=10,method=c(rep("",4),"pmm",
                                                                          "","","pmm","","","pmm",rep("",40)))

xyplot(imputation,Diff_PPS_ths~gincdif_num,pch=20,cex=1.4)

#############################

###
# PREFERENCES FOR REDISTRIBUTION
###

## Plot 1.1: Effect of having a family on preferences and association with average FRR

rsl_M1.1<-with(imputation_gincdif,clmm(gincdif~single_family+hinctnta_num+agea+gndr+lms+
                                       high_second_edu+average_FRR+(1+single_family|cntry),
                                     weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M1.1))

fixed_effect <- pooled_results$estimate[which(pooled_results$term == "average_FRR")]

random_slopes_list <- lapply(rsl_M1.1$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect<-pooled_results$estimate[5]

plot_H1<-data.frame(cbind(levels(data_full$cntry),numeric(14)))
colnames(plot_H1)<-c("country","total_effect")

for (i in 1:nrow(plot_H1)){
  plot_H1$total_effect[i]=fixed_effect+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                            random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                            random_slopes_list[[5]][i,2])
}
plot_H1$total_effect<-as.numeric(plot_H1$total_effect)

plot_H1$average_FRR<-numeric(nrow(plot_H1))
for (i in 1:nrow(plot_H1)){
  plot_H1$average_FRR[i]=data_full$average_FRR[which(data_full$cntry==plot_H1$country[i])[1]]
}

## ADdd CIs

# Extract random slopes per country from each imputed model
random_slopes_list <- lapply(rsl_M1$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes
})

# Extract fixed effect
pooled_results <- summary(pool(rsl_M1))
fixed_effect <- pooled_results$estimate[5]  # Adjust index if needed

# Create a data frame for plotting
plot_H1 <- data.frame(country = levels(data_full$cntry))

# Compute mean random slopes and standard error
random_slopes_matrix <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes)
plot_H1$random_slope_mean <- rowMeans(random_slopes_matrix)
plot_H1$random_slope_se <- apply(random_slopes_matrix, 1, function(x) sd(x) / sqrt(length(x)))

# Compute total effect and confidence intervals
plot_H1$total_effect <- fixed_effect + plot_H1$random_slope_mean
plot_H1$lower_CI <- plot_H1$total_effect - 1.96 * plot_H1$random_slope_se
plot_H1$upper_CI <- plot_H1$total_effect + 1.96 * plot_H1$random_slope_se

# Add average_FRR_workp for each country
plot_H1$average_FRR <- sapply(plot_H1$country, function(cntry) {
  data_full$average_FRR[which(data_full$cntry == cntry)[1]]
})

# Plot with confidence intervals
agg_png("rslM1.1.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(data = plot_H1, aes(x = average_FRR, y = total_effect, label = country)) +
  geom_point(color = "#21908C", size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Regression line
  geom_text(vjust = -1.6, size = 3) +  # Country labels
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.02, color = "#21908C") +  # Confidence intervals
  scale_color_viridis(option = "magma", direction = -1) +  # Color scheme
  labs(title = "",
       x = "Average FRR",
       y = "Effect of having a family on preferences") +
  theme_minimal()
dev.off()

############################################################

## Plot 2: Effect of family types (combinations partnership and parentship) and association with average FRR

rsl_M1.2<-with(imputation_gincdif,clmm(gincdif~parentpartner+hinctnta_num+agea+gndr+lms+
                                       high_second_edu+average_FRR+(1+parentpartner|cntry),
                                     weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M1.2))

random_slopes_list <- lapply(rsl_M1.2$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect_1<-pooled_results$estimate[5]
fixed_effect_2<-pooled_results$estimate[6]
fixed_effect_3<-pooled_results$estimate[7]

plot_pp<-data.frame(cbind(levels(data_full$cntry),numeric(14),numeric(14),numeric(14)))
colnames(plot_pp)<-c("country","total_effect_1","total_effect_2","total_effect_3")

####

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_1[i]=fixed_effect_1+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                            random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                            random_slopes_list[[5]][i,2],random_slopes_list[[6]][i,2],
                                            random_slopes_list[[7]][i,2],random_slopes_list[[8]][i,2],
                                            random_slopes_list[[9]][i,2],random_slopes_list[[10]][i,2])
}
plot_pp$total_effect_1<-as.numeric(plot_pp$total_effect_1)

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_2[i]=fixed_effect_2+mean(random_slopes_list[[1]][i,3],random_slopes_list[[2]][i,3],
                                                 random_slopes_list[[3]][i,3],random_slopes_list[[4]][i,3],
                                                 random_slopes_list[[5]][i,3],random_slopes_list[[6]][i,3],
                                                 random_slopes_list[[7]][i,3],random_slopes_list[[8]][i,3],
                                                 random_slopes_list[[9]][i,3],random_slopes_list[[10]][i,3])
}
plot_pp$total_effect_2<-as.numeric(plot_pp$total_effect_2)

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_3[i]=fixed_effect_3+mean(random_slopes_list[[1]][i,4],random_slopes_list[[2]][i,4],
                                                 random_slopes_list[[3]][i,4],random_slopes_list[[4]][i,4],
                                                 random_slopes_list[[5]][i,4],random_slopes_list[[6]][i,4],
                                                 random_slopes_list[[5]][i,7],random_slopes_list[[8]][i,4],
                                                 random_slopes_list[[5]][i,9],random_slopes_list[[10]][i,4])
}
plot_pp$total_effect_3<-as.numeric(plot_pp$total_effect_3)


plot_pp$average_FRR<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR[i]=data_full$average_FRR[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_1<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_1[i]=data_full$average_FRR_sp[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_2<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_2[i]=data_full$average_FRR_cnc[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_3<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_3[i]=data_full$average_FRR_cwc[which(data_full$cntry==plot_pp$country[i])[1]]
}

## ADdd CIs

# Compute mean random slopes and standard error
rsm_1 <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes single parents)
plot_pp$random_slope_mean_1 <- rowMeans(rsm_1)
plot_pp$random_slope_se_1 <- apply(rsm_1, 1, function(x) sd(x) / sqrt(length(x)))

rsm_2 <- sapply(random_slopes_list, function(mat) mat[, 3])  # Extract column 3 (random slopes couples without children)
plot_pp$random_slope_mean_2 <- rowMeans(rsm_2)
plot_pp$random_slope_se_2 <- apply(rsm_2, 1, function(x) sd(x) / sqrt(length(x)))

# Compute mean random slopes and standard error
rsm_3 <- sapply(random_slopes_list, function(mat) mat[, 4])  # Extract column 2 (random slopes couples parents)
plot_pp$random_slope_mean_3 <- rowMeans(rsm_3)
plot_pp$random_slope_se_3 <- apply(rsm_3, 1, function(x) sd(x) / sqrt(length(x)))

# Compute confidence intervals
plot_pp$lower_CI_1 <- plot_pp$total_effect_1 - 1.96 * plot_pp$random_slope_se_1
plot_pp$upper_CI_1 <- plot_pp$total_effect_1 + 1.96 * plot_pp$random_slope_se_1
plot_pp$lower_CI_2 <- plot_pp$total_effect_2 - 1.96 * plot_pp$random_slope_se_2
plot_pp$upper_CI_2 <- plot_pp$total_effect_2 + 1.96 * plot_pp$random_slope_se_2
plot_pp$lower_CI_3 <- plot_pp$total_effect_3 - 1.96 * plot_pp$random_slope_se_3
plot_pp$upper_CI_3 <- plot_pp$total_effect_3 + 1.96 * plot_pp$random_slope_se_3

plot_pp_long <- plot_pp %>%
  pivot_longer(
    cols = starts_with("total_effect"),
    names_to = "family_type",
    names_prefix = "total_effect_",
    values_to = "total_effect"
  ) %>%
  mutate(
    lower_CI = case_when(
      family_type == "1" ~ lower_CI_1,
      family_type == "3" ~ lower_CI_3,
      family_type == "2" ~ lower_CI_2
    ),
    upper_CI = case_when(
      family_type == "1" ~ upper_CI_1,
      family_type == "3" ~ upper_CI_3,
      family_type == "2" ~ upper_CI_2
    ),
    average_FRR_ft = case_when(
      family_type == "1" ~ average_FRR_1,
      family_type == "3" ~ average_FRR_3,
      family_type == "2" ~ average_FRR_2
    ),
    family_type = recode(family_type,
                         "1" = "Single parent",
                         "3" = "Couple w/ children",
                         "2" = "Couple w/o children")
  )

# Plot it: 

agg_png("rslM1.2.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_pp_long, aes(x = average_FRR_ft, y = total_effect, color = family_type, label = country)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.02, alpha = 0.6) +  # CI bars
  geom_text(vjust = -1, size = 1.5) +  # Country names
  #geom_smooth(method = "lm", se = FALSE) +  # Regression line
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.85) +
  labs(
    x = "Redistribution to family type (in 1000 PPS)",
    y = "Association with preferences for redistribution",
    color = "Family type"
  ) +
  theme_minimal()
dev.off()


###############################################################################

## Plot 3: Effect of partnership type and association with average FRR

rsl_M1.3<-with(imputation_gincdif,clmm(gincdif~relstatus+nchild+hinctnta_num+agea+gndr+lms+
                                        high_second_edu+average_FRR+(1+relstatus|cntry),
                                      weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M1.3))

random_slopes_list <- lapply(rsl_M1.3$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect_1<-pooled_results$estimate[5]
fixed_effect_2<-pooled_results$estimate[6]

plot_data<-data.frame(cbind(levels(data_full$cntry),numeric(14),numeric(14)))
colnames(plot_data)<-c("country","total_effect_1","total_effect_2")

####

for (i in 1:nrow(plot_data)){
  plot_data$total_effect_1[i]=fixed_effect_1+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                                  random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                                  random_slopes_list[[5]][i,2],random_slopes_list[[6]][i,2],
                                                  random_slopes_list[[7]][i,2],random_slopes_list[[8]][i,2],
                                                  random_slopes_list[[9]][i,2],random_slopes_list[[10]][i,2])
}
plot_data$total_effect_1<-as.numeric(plot_data$total_effect_1)

for (i in 1:nrow(plot_data)){
  plot_data$total_effect_2[i]=fixed_effect_2+mean(random_slopes_list[[1]][i,3],random_slopes_list[[2]][i,3],
                                                    random_slopes_list[[3]][i,3],random_slopes_list[[4]][i,3],
                                                    random_slopes_list[[5]][i,3],random_slopes_list[[6]][i,3],
                                                    random_slopes_list[[7]][i,3],random_slopes_list[[8]][i,3],
                                                    random_slopes_list[[9]][i,3],random_slopes_list[[10]][i,3])
}
plot_data$total_effect_2<-as.numeric(plot_data$total_effect_2)

plot_data$average_FRR<-numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)){
  plot_data$average_FRR[i]=data_full$average_FRR[which(data_full$cntry==plot_data$country[i])[1]]
}

plot_data$average_FRR_1<-numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)){
  plot_data$average_FRR_1[i]=data_full$average_FRR_cohabitating[which(data_full$cntry==plot_data$country[i])[1]]
}

plot_data$average_FRR_2<-numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)){
  plot_data$average_FRR_2[i]=data_full$average_FRR_married[which(data_full$cntry==plot_data$country[i])[1]]
}

## ADdd CIs

# Compute mean random slopes and standard error
rsm_1 <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes cohabitating)
plot_data$random_slope_mean_1 <- rowMeans(rsm_1)
plot_data$random_slope_se_1 <- apply(rsm_1, 1, function(x) sd(x) / sqrt(length(x)))

rsm_2 <- sapply(random_slopes_list, function(mat) mat[, 3])  # Extract column 3 (random slopes married)
plot_data$random_slope_mean_2 <- rowMeans(rsm_2)
plot_data$random_slope_se_2 <- apply(rsm_2, 1, function(x) sd(x) / sqrt(length(x)))


# Compute confidence intervals
plot_data$lower_CI_1 <- plot_data$total_effect_1 - 1.96 * plot_data$random_slope_se_1
plot_data$upper_CI_1 <- plot_data$total_effect_1 + 1.96 * plot_data$random_slope_se_1
plot_data$lower_CI_2 <- plot_data$total_effect_2 - 1.96 * plot_data$random_slope_se_2
plot_data$upper_CI_2 <- plot_data$total_effect_2 + 1.96 * plot_data$random_slope_se_2

plot_data_long <- plot_data %>%
  pivot_longer(
    cols = starts_with("total_effect"),
    names_to = "family_type",
    names_prefix = "total_effect_",
    values_to = "total_effect"
  ) %>%
  mutate(
    lower_CI = case_when(
      family_type == "1" ~ lower_CI_1,
      family_type == "2" ~ lower_CI_2
    ),
    upper_CI = case_when(
      family_type == "1" ~ upper_CI_1,
      family_type == "2" ~ upper_CI_2
    ),
    average_FRR_ft = case_when(
      family_type == "1" ~ average_FRR_1,
      family_type == "2" ~ average_FRR_2
    ),
    family_type = recode(family_type,
                         "1" = "Cohabitating",
                         "2" = "Married")
  )

# Plot it: 

agg_png("rslM1.3.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_data_long, aes(x = average_FRR_ft, y = total_effect, color = family_type, label = country)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.01, alpha = 0.6) +  # CI bars
  geom_text(vjust = -1, size = 3) +  # Country names
  #geom_smooth(method = "lm", se = FALSE) +  # Regression line
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.85) +
  labs(
    x = "Redistribution to family type (in 1000 PPS)",
    y = "Association with preferences for redistribution",
    color = "Family type"
  ) +
  theme_minimal()
dev.off()

###############################################################################

## Plot 4: Effect of number of children and association with average FRR

rsl_M1.4<-with(imputation_gincdif,clmm(gincdif~relstatus+nchild+hinctnta_num+agea+gndr+lms+
                                       high_second_edu+average_FRR+(1+nchild|cntry),
                                     weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M1.4))

random_slopes_list <- lapply(rsl_M1.4$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect_1<-pooled_results$estimate[5]
fixed_effect_2<-pooled_results$estimate[6]

plot_data<-data.frame(cbind(levels(data_full$cntry),numeric(14),numeric(14)))
colnames(plot_data)<-c("country","total_effect_1","total_effect_2")

####

for (i in 1:nrow(plot_data)){
  plot_data$total_effect_1[i]=fixed_effect_1+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                                  random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                                  random_slopes_list[[5]][i,2],random_slopes_list[[6]][i,2],
                                                  random_slopes_list[[7]][i,2],random_slopes_list[[8]][i,2],
                                                  random_slopes_list[[9]][i,2],random_slopes_list[[10]][i,2])
}
plot_data$total_effect_1<-as.numeric(plot_data$total_effect_1)

for (i in 1:nrow(plot_data)){
  plot_data$total_effect_2[i]=fixed_effect_2+mean(random_slopes_list[[1]][i,3],random_slopes_list[[2]][i,3],
                                                  random_slopes_list[[3]][i,3],random_slopes_list[[4]][i,3],
                                                  random_slopes_list[[5]][i,3],random_slopes_list[[6]][i,3],
                                                  random_slopes_list[[7]][i,3],random_slopes_list[[8]][i,3],
                                                  random_slopes_list[[9]][i,3],random_slopes_list[[10]][i,3])
}
plot_data$total_effect_2<-as.numeric(plot_data$total_effect_2)

plot_data$average_FRR<-numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)){
  plot_data$average_FRR[i]=data_full$average_FRR[which(data_full$cntry==plot_data$country[i])[1]]
}

plot_data$average_FRR_1<-numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)){
  plot_data$average_FRR_1[i]=data_full$average_FRR_child12[which(data_full$cntry==plot_data$country[i])[1]]
}

plot_data$average_FRR_2<-numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)){
  plot_data$average_FRR_2[i]=data_full$average_FRR_child3[which(data_full$cntry==plot_data$country[i])[1]]
}

## ADdd CIs

# Compute mean random slopes and standard error
rsm_1 <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes cohabitating)
plot_data$random_slope_mean_1 <- rowMeans(rsm_1)
plot_data$random_slope_se_1 <- apply(rsm_1, 1, function(x) sd(x) / sqrt(length(x)))

rsm_2 <- sapply(random_slopes_list, function(mat) mat[, 3])  # Extract column 3 (random slopes married)
plot_data$random_slope_mean_2 <- rowMeans(rsm_2)
plot_data$random_slope_se_2 <- apply(rsm_2, 1, function(x) sd(x) / sqrt(length(x)))


# Compute confidence intervals
plot_data$lower_CI_1 <- plot_data$total_effect_1 - 1.96 * plot_data$random_slope_se_1
plot_data$upper_CI_1 <- plot_data$total_effect_1 + 1.96 * plot_data$random_slope_se_1
plot_data$lower_CI_2 <- plot_data$total_effect_2 - 1.96 * plot_data$random_slope_se_2
plot_data$upper_CI_2 <- plot_data$total_effect_2 + 1.96 * plot_data$random_slope_se_2

plot_data_long <- plot_data %>%
  pivot_longer(
    cols = starts_with("total_effect"),
    names_to = "family_type",
    names_prefix = "total_effect_",
    values_to = "total_effect"
  ) %>%
  mutate(
    lower_CI = case_when(
      family_type == "1" ~ lower_CI_1,
      family_type == "2" ~ lower_CI_2
    ),
    upper_CI = case_when(
      family_type == "1" ~ upper_CI_1,
      family_type == "2" ~ upper_CI_2
    ),
    average_FRR_ft = case_when(
      family_type == "1" ~ average_FRR_1,
      family_type == "2" ~ average_FRR_2
    ),
    family_type = recode(family_type,
                         "1" = "1-2 children",
                         "2" = "3 or more children")
  )

# Plot it: 

agg_png("rslM1.4.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_data_long, aes(x = average_FRR_ft, y = total_effect, color = family_type, label = country)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.03, alpha = 0.6) +  # CI bars
  geom_text(vjust = -1, size = 3) +  # Country names
  #geom_smooth(method = "lm", se = FALSE) +  # Regression line
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.85) +
  labs(
    x = "Redistribution to family type (in 1000 PPS)",
    y = "Association with preferences for redistribution",
    color = "Family type"
  ) +
  theme_minimal()
dev.off()

############################################################

## Plot 5: Effect of earnings distribution in the household and association with average FRR

rsl_M1.5<-with(imputation_gincdif,clmm(gincdif~earnings_dist+parentpartner+hinctnta_num+agea+gndr+lms+
                                       high_second_edu+average_FRR+(1+earnings_dist|cntry),
                                     weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M1.5))

random_slopes_list <- lapply(rsl_M1.5$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect_1<-pooled_results$estimate[5]
fixed_effect_2<-pooled_results$estimate[6]
fixed_effect_3<-pooled_results$estimate[7]

plot_pp<-data.frame(cbind(levels(data_full$cntry),numeric(14),numeric(14),numeric(14)))
colnames(plot_pp)<-c("country","total_effect_1","total_effect_2","total_effect_3")

####

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_1[i]=fixed_effect_1+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                                random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                                random_slopes_list[[5]][i,2],random_slopes_list[[6]][i,2],
                                                random_slopes_list[[7]][i,2],random_slopes_list[[8]][i,2],
                                                random_slopes_list[[9]][i,2],random_slopes_list[[10]][i,2])
}
plot_pp$total_effect_1<-as.numeric(plot_pp$total_effect_1)

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_2[i]=fixed_effect_2+mean(random_slopes_list[[1]][i,3],random_slopes_list[[2]][i,3],
                                                random_slopes_list[[3]][i,3],random_slopes_list[[4]][i,3],
                                                random_slopes_list[[5]][i,3],random_slopes_list[[6]][i,3],
                                                random_slopes_list[[7]][i,3],random_slopes_list[[8]][i,3],
                                                random_slopes_list[[9]][i,3],random_slopes_list[[10]][i,3])
}
plot_pp$total_effect_2<-as.numeric(plot_pp$total_effect_2)

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_3[i]=fixed_effect_3+mean(random_slopes_list[[1]][i,4],random_slopes_list[[2]][i,4],
                                                random_slopes_list[[3]][i,4],random_slopes_list[[4]][i,4],
                                                random_slopes_list[[5]][i,4],random_slopes_list[[6]][i,4],
                                                random_slopes_list[[5]][i,7],random_slopes_list[[8]][i,4],
                                                random_slopes_list[[5]][i,9],random_slopes_list[[10]][i,4])
}
plot_pp$total_effect_3<-as.numeric(plot_pp$total_effect_3)

plot_pp$average_FRR<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR[i]=data_full$average_FRR[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_1<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_1[i]=data_full$average_FRR_singleearner[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_2<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_2[i]=data_full$average_FRR_supearner[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_3<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_3[i]=data_full$average_FRR_doubearner[which(data_full$cntry==plot_pp$country[i])[1]]
}

## ADdd CIs

# Compute mean random slopes and standard error
rsm_1 <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes single parents)
plot_pp$random_slope_mean_1 <- rowMeans(rsm_1)
plot_pp$random_slope_se_1 <- apply(rsm_1, 1, function(x) sd(x) / sqrt(length(x)))

rsm_2 <- sapply(random_slopes_list, function(mat) mat[, 3])  # Extract column 3 (random slopes couples without children)
plot_pp$random_slope_mean_2 <- rowMeans(rsm_2)
plot_pp$random_slope_se_2 <- apply(rsm_2, 1, function(x) sd(x) / sqrt(length(x)))

# Compute mean random slopes and standard error
rsm_3 <- sapply(random_slopes_list, function(mat) mat[, 4])  # Extract column 2 (random slopes couples parents)
plot_pp$random_slope_mean_3 <- rowMeans(rsm_3)
plot_pp$random_slope_se_3 <- apply(rsm_3, 1, function(x) sd(x) / sqrt(length(x)))

# Compute confidence intervals
plot_pp$lower_CI_1 <- plot_pp$total_effect_1 - 1.96 * plot_pp$random_slope_se_1
plot_pp$upper_CI_1 <- plot_pp$total_effect_1 + 1.96 * plot_pp$random_slope_se_1
plot_pp$lower_CI_2 <- plot_pp$total_effect_2 - 1.96 * plot_pp$random_slope_se_2
plot_pp$upper_CI_2 <- plot_pp$total_effect_2 + 1.96 * plot_pp$random_slope_se_2
plot_pp$lower_CI_3 <- plot_pp$total_effect_3 - 1.96 * plot_pp$random_slope_se_3
plot_pp$upper_CI_3 <- plot_pp$total_effect_3 + 1.96 * plot_pp$random_slope_se_3

plot_pp_long <- plot_pp %>%
  pivot_longer(
    cols = starts_with("total_effect"),
    names_to = "family_type",
    names_prefix = "total_effect_",
    values_to = "total_effect"
  ) %>%
  mutate(
    lower_CI = case_when(
      family_type == "1" ~ lower_CI_1,
      family_type == "3" ~ lower_CI_3,
      family_type == "2" ~ lower_CI_2
    ),
    upper_CI = case_when(
      family_type == "1" ~ upper_CI_1,
      family_type == "3" ~ upper_CI_3,
      family_type == "2" ~ upper_CI_2
    ),
    average_FRR_ft = case_when(
      family_type == "1" ~ average_FRR_1,
      family_type == "3" ~ average_FRR_2,
      family_type == "2" ~ average_FRR_3
    ),
    family_type = recode(family_type,
                         "1" = "Single earner",
                         "3" = "Supplementary earner",
                         "2" = "Double earner") # Check that this is the correct ordering of the categories
  )

# Plot it: 

agg_png("rslM1.5.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_pp_long, aes(x = average_FRR_ft, y = total_effect, color = family_type, label = country)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.03, alpha = 0.6) +  # CI bars
  geom_text(vjust = -1, size = 1.5) +  # Country names
  #geom_smooth(method = "lm", se = FALSE) +  # Regression line
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.85) +
  labs(
    x = "Redistribution to family type (in 1000 PPS)",
    y = "Association with preferences for redistribution",
    color = "Family type"
  ) +
  theme_minimal()
dev.off()

RSModels<-list(rsl_M1.1,rsl_M1.2,rsl_M1.3,rsl_M1.4,rsl_M1.5)

save(RSModels,file="RSModels_P3.RData")

################################################################################
################################################################################

###
# PREFERENCES FOR WORK-FAMILY BALANCE
###

## Plot 2.1: Effect of being a working parent on preferences and association with average FRR

rsl_M2.1<-with(imputation_gincdif,clmm(wrkprbf~workingparents+
                                  NDI_RP1_PPS_ths+hinctnta_num+agea+gndr+lms+high_second_edu+
                                  rlgdgr_num+freehms_num+Diff_PPS_ths+average_FRR+(1+workingparents|cntry),
                                weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M2.1))

random_slopes_list <- lapply(rsl_M2.1$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect<-pooled_results$estimate[4]

plot_H3<-data.frame(cbind(levels(data_full$cntry),numeric(14)))
colnames(plot_H3)<-c("country","total_effect")

for (i in 1:nrow(plot_H3)){
  plot_H3$total_effect[i]=fixed_effect+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                            random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                            random_slopes_list[[5]][i,2])
}
plot_H3$total_effect<-as.numeric(plot_H3$total_effect)

plot_H3$average_FRR<-numeric(nrow(plot_H3))
for (i in 1:nrow(plot_H3)){
  plot_H3$average_FRR[i]=data_full$average_FRR_workp[which(data_full$cntry==plot_H3$country[i])[1]]
}

## ADdd CIs

# Compute mean random slopes and standard error
random_slopes_matrix <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes)
plot_H3$random_slope_mean <- rowMeans(random_slopes_matrix)
plot_H3$random_slope_se <- apply(random_slopes_matrix, 1, function(x) sd(x) / sqrt(length(x)))

# Compute total effect and confidence intervals
plot_H3$total_effect <- fixed_effect + plot_H3$random_slope_mean
plot_H3$lower_CI <- plot_H3$total_effect - 1.96 * plot_H3$random_slope_se
plot_H3$upper_CI <- plot_H3$total_effect + 1.96 * plot_H3$random_slope_se

# Plot with confidence intervals (linear)
agg_png("rslM3.1.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(data = plot_H3, aes(x = average_FRR, y = total_effect, label = country)) +
  geom_point(color = "#21908C", size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Regression line
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black")+
  geom_text(vjust = -1.6, size = 3) +  # Country labels
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.02, color = "#21908C") +  # Confidence intervals
  scale_color_viridis(option = "magma", direction = -1) +  # Color scheme
  labs(title = "",
       x = "Average FRR",
       y = "Effect of being a working parent on preferences") +
  theme_minimal()
dev.off()

# Plot with confidence intervals (quadratic)
agg_png("rslM3.1_q.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(data = plot_H3, aes(x = average_FRR, y = total_effect, label = country)) +
  geom_point(color = "#21908C", size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Regression line
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red")+
  geom_text(vjust = -1.6, size = 3) +  # Country labels
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.02, color = "#21908C") +  # Confidence intervals
  scale_color_viridis(option = "magma", direction = -1) +  # Color scheme
  labs(title = "",
       x = "Average FRR",
       y = "Effect of being a working parent on preferences") +
  theme_minimal()
dev.off()

############################################################

## Plot 2: Effect of family types (combinations partnership and parentship) and association with average FRR

rsl_M2.2<-with(imputation_gincdif,clmm(wrkprbf~parentpartner+hinctnta_num+agea+gndr+lms+
                                         high_second_edu+average_FRR+(1+parentpartner|cntry),
                                       weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M2.2))

random_slopes_list <- lapply(rsl_M2.2$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect_1<-pooled_results$estimate[5]
fixed_effect_2<-pooled_results$estimate[6]
fixed_effect_3<-pooled_results$estimate[7]

plot_pp<-data.frame(cbind(levels(data_full$cntry),numeric(14),numeric(14),numeric(14)))
colnames(plot_pp)<-c("country","total_effect_1","total_effect_2","total_effect_3")

####

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_1[i]=fixed_effect_1+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                                random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                                random_slopes_list[[5]][i,2],random_slopes_list[[6]][i,2],
                                                random_slopes_list[[7]][i,2],random_slopes_list[[8]][i,2],
                                                random_slopes_list[[9]][i,2],random_slopes_list[[10]][i,2])
}
plot_pp$total_effect_1<-as.numeric(plot_pp$total_effect_1)

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_2[i]=fixed_effect_2+mean(random_slopes_list[[1]][i,3],random_slopes_list[[2]][i,3],
                                                random_slopes_list[[3]][i,3],random_slopes_list[[4]][i,3],
                                                random_slopes_list[[5]][i,3],random_slopes_list[[6]][i,3],
                                                random_slopes_list[[7]][i,3],random_slopes_list[[8]][i,3],
                                                random_slopes_list[[9]][i,3],random_slopes_list[[10]][i,3])
}
plot_pp$total_effect_2<-as.numeric(plot_pp$total_effect_2)

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_3[i]=fixed_effect_3+mean(random_slopes_list[[1]][i,4],random_slopes_list[[2]][i,4],
                                                random_slopes_list[[3]][i,4],random_slopes_list[[4]][i,4],
                                                random_slopes_list[[5]][i,4],random_slopes_list[[6]][i,4],
                                                random_slopes_list[[5]][i,7],random_slopes_list[[8]][i,4],
                                                random_slopes_list[[5]][i,9],random_slopes_list[[10]][i,4])
}
plot_pp$total_effect_3<-as.numeric(plot_pp$total_effect_3)


plot_pp$average_FRR<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR[i]=data_full$average_FRR[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_1<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_1[i]=data_full$average_FRR_sp[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_2<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_2[i]=data_full$average_FRR_cnc[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_3<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_3[i]=data_full$average_FRR_cwc[which(data_full$cntry==plot_pp$country[i])[1]]
}

## ADdd CIs

# Compute mean random slopes and standard error
rsm_1 <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes single parents)
plot_pp$random_slope_mean_1 <- rowMeans(rsm_1)
plot_pp$random_slope_se_1 <- apply(rsm_1, 1, function(x) sd(x) / sqrt(length(x)))

rsm_2 <- sapply(random_slopes_list, function(mat) mat[, 3])  # Extract column 3 (random slopes couples without children)
plot_pp$random_slope_mean_2 <- rowMeans(rsm_2)
plot_pp$random_slope_se_2 <- apply(rsm_2, 1, function(x) sd(x) / sqrt(length(x)))

# Compute mean random slopes and standard error
rsm_3 <- sapply(random_slopes_list, function(mat) mat[, 4])  # Extract column 2 (random slopes couples parents)
plot_pp$random_slope_mean_3 <- rowMeans(rsm_3)
plot_pp$random_slope_se_3 <- apply(rsm_3, 1, function(x) sd(x) / sqrt(length(x)))

# Compute confidence intervals
plot_pp$lower_CI_1 <- plot_pp$total_effect_1 - 1.96 * plot_pp$random_slope_se_1
plot_pp$upper_CI_1 <- plot_pp$total_effect_1 + 1.96 * plot_pp$random_slope_se_1
plot_pp$lower_CI_2 <- plot_pp$total_effect_2 - 1.96 * plot_pp$random_slope_se_2
plot_pp$upper_CI_2 <- plot_pp$total_effect_2 + 1.96 * plot_pp$random_slope_se_2
plot_pp$lower_CI_3 <- plot_pp$total_effect_3 - 1.96 * plot_pp$random_slope_se_3
plot_pp$upper_CI_3 <- plot_pp$total_effect_3 + 1.96 * plot_pp$random_slope_se_3

plot_pp_long <- plot_pp %>%
  pivot_longer(
    cols = starts_with("total_effect"),
    names_to = "family_type",
    names_prefix = "total_effect_",
    values_to = "total_effect"
  ) %>%
  mutate(
    lower_CI = case_when(
      family_type == "1" ~ lower_CI_1,
      family_type == "3" ~ lower_CI_3,
      family_type == "2" ~ lower_CI_2
    ),
    upper_CI = case_when(
      family_type == "1" ~ upper_CI_1,
      family_type == "3" ~ upper_CI_3,
      family_type == "2" ~ upper_CI_2
    ),
    average_FRR_ft = case_when(
      family_type == "1" ~ average_FRR_1,
      family_type == "3" ~ average_FRR_3,
      family_type == "2" ~ average_FRR_2
    ),
    family_type = recode(family_type,
                         "1" = "Single parent",
                         "3" = "Couple w/ children",
                         "2" = "Couple w/o children")
  )

# Plot it: 

agg_png("rslM2.2.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_pp_long, aes(x = average_FRR_ft, y = total_effect, color = family_type, label = country)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.02, alpha = 0.6) +  # CI bars
  geom_text(vjust = -1, size = 1.5) +  # Country names
  #geom_smooth(method = "lm", se = FALSE) +  # Regression line
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.85) +
  labs(
    x = "Redistribution to family type (in 1000 PPS)",
    y = "Association with preferences for work-family balance policies",
    color = "Family type"
  ) +
  theme_minimal()
dev.off()


###############################################################################

## Plot 3: Effect of partnership type and association with average FRR

rsl_M2.3<-with(imputation_gincdif,clmm(wrkprbf~relstatus+nchild+hinctnta_num+agea+gndr+lms+
                                         high_second_edu+average_FRR+(1+relstatus|cntry),
                                       weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M2.3))

random_slopes_list <- lapply(rsl_M2.3$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect_1<-pooled_results$estimate[4]
fixed_effect_2<-pooled_results$estimate[5]

plot_data<-data.frame(cbind(levels(data_full$cntry),numeric(14),numeric(14)))
colnames(plot_data)<-c("country","total_effect_1","total_effect_2")

####

for (i in 1:nrow(plot_data)){
  plot_data$total_effect_1[i]=fixed_effect_1+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                                  random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                                  random_slopes_list[[5]][i,2],random_slopes_list[[6]][i,2],
                                                  random_slopes_list[[7]][i,2],random_slopes_list[[8]][i,2],
                                                  random_slopes_list[[9]][i,2],random_slopes_list[[10]][i,2])
}
plot_data$total_effect_1<-as.numeric(plot_data$total_effect_1)

for (i in 1:nrow(plot_data)){
  plot_data$total_effect_2[i]=fixed_effect_2+mean(random_slopes_list[[1]][i,3],random_slopes_list[[2]][i,3],
                                                  random_slopes_list[[3]][i,3],random_slopes_list[[4]][i,3],
                                                  random_slopes_list[[5]][i,3],random_slopes_list[[6]][i,3],
                                                  random_slopes_list[[7]][i,3],random_slopes_list[[8]][i,3],
                                                  random_slopes_list[[9]][i,3],random_slopes_list[[10]][i,3])
}
plot_data$total_effect_2<-as.numeric(plot_data$total_effect_2)

plot_data$average_FRR<-numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)){
  plot_data$average_FRR[i]=data_full$average_FRR[which(data_full$cntry==plot_data$country[i])[1]]
}

plot_data$average_FRR_1<-numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)){
  plot_data$average_FRR_1[i]=data_full$average_FRR_cohabitating[which(data_full$cntry==plot_data$country[i])[1]]
}

plot_data$average_FRR_2<-numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)){
  plot_data$average_FRR_2[i]=data_full$average_FRR_married[which(data_full$cntry==plot_data$country[i])[1]]
}

## ADdd CIs

# Compute mean random slopes and standard error
rsm_1 <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes cohabitating)
plot_data$random_slope_mean_1 <- rowMeans(rsm_1)
plot_data$random_slope_se_1 <- apply(rsm_1, 1, function(x) sd(x) / sqrt(length(x)))

rsm_2 <- sapply(random_slopes_list, function(mat) mat[, 3])  # Extract column 3 (random slopes married)
plot_data$random_slope_mean_2 <- rowMeans(rsm_2)
plot_data$random_slope_se_2 <- apply(rsm_2, 1, function(x) sd(x) / sqrt(length(x)))


# Compute confidence intervals
plot_data$lower_CI_1 <- plot_data$total_effect_1 - 1.96 * plot_data$random_slope_se_1
plot_data$upper_CI_1 <- plot_data$total_effect_1 + 1.96 * plot_data$random_slope_se_1
plot_data$lower_CI_2 <- plot_data$total_effect_2 - 1.96 * plot_data$random_slope_se_2
plot_data$upper_CI_2 <- plot_data$total_effect_2 + 1.96 * plot_data$random_slope_se_2

plot_data_long <- plot_data %>%
  pivot_longer(
    cols = starts_with("total_effect"),
    names_to = "family_type",
    names_prefix = "total_effect_",
    values_to = "total_effect"
  ) %>%
  mutate(
    lower_CI = case_when(
      family_type == "1" ~ lower_CI_1,
      family_type == "2" ~ lower_CI_2
    ),
    upper_CI = case_when(
      family_type == "1" ~ upper_CI_1,
      family_type == "2" ~ upper_CI_2
    ),
    average_FRR_ft = case_when(
      family_type == "1" ~ average_FRR_1,
      family_type == "2" ~ average_FRR_2
    ),
    family_type = recode(family_type,
                         "1" = "Cohabitating",
                         "2" = "Married")
  )

# Plot it: 

agg_png("rslM2.3.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_data_long, aes(x = average_FRR_ft, y = total_effect, color = family_type, label = country)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.01, alpha = 0.6) +  # CI bars
  geom_text(vjust = -1, size = 3) +  # Country names
  #geom_smooth(method = "lm", se = FALSE) +  # Regression line
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.85) +
  labs(
    x = "Redistribution to family type (in 1000 PPS)",
    y = "Association with preferences for work-family balance policies",
    color = "Family type"
  ) +
  theme_minimal()
dev.off()

###############################################################################

## Plot 4: Effect of number of children and association with average FRR

rsl_M2.4<-with(imputation_gincdif,clmm(wrkprbf~relstatus+nchild+hinctnta_num+agea+gndr+lms+
                                         high_second_edu+average_FRR+(1+nchild|cntry),
                                       weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M2.4))

random_slopes_list <- lapply(rsl_M2.4$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect_1<-pooled_results$estimate[5]
fixed_effect_2<-pooled_results$estimate[6]

plot_data<-data.frame(cbind(levels(data_full$cntry),numeric(14),numeric(14)))
colnames(plot_data)<-c("country","total_effect_1","total_effect_2")

####

for (i in 1:nrow(plot_data)){
  plot_data$total_effect_1[i]=fixed_effect_1+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                                  random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                                  random_slopes_list[[5]][i,2],random_slopes_list[[6]][i,2],
                                                  random_slopes_list[[7]][i,2],random_slopes_list[[8]][i,2],
                                                  random_slopes_list[[9]][i,2],random_slopes_list[[10]][i,2])
}
plot_data$total_effect_1<-as.numeric(plot_data$total_effect_1)

for (i in 1:nrow(plot_data)){
  plot_data$total_effect_2[i]=fixed_effect_2+mean(random_slopes_list[[1]][i,3],random_slopes_list[[2]][i,3],
                                                  random_slopes_list[[3]][i,3],random_slopes_list[[4]][i,3],
                                                  random_slopes_list[[5]][i,3],random_slopes_list[[6]][i,3],
                                                  random_slopes_list[[7]][i,3],random_slopes_list[[8]][i,3],
                                                  random_slopes_list[[9]][i,3],random_slopes_list[[10]][i,3])
}
plot_data$total_effect_2<-as.numeric(plot_data$total_effect_2)

plot_data$average_FRR<-numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)){
  plot_data$average_FRR[i]=data_full$average_FRR[which(data_full$cntry==plot_data$country[i])[1]]
}

plot_data$average_FRR_1<-numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)){
  plot_data$average_FRR_1[i]=data_full$average_FRR_child12[which(data_full$cntry==plot_data$country[i])[1]]
}

plot_data$average_FRR_2<-numeric(nrow(plot_data))
for (i in 1:nrow(plot_data)){
  plot_data$average_FRR_2[i]=data_full$average_FRR_child3[which(data_full$cntry==plot_data$country[i])[1]]
}

## ADdd CIs

# Compute mean random slopes and standard error
rsm_1 <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes cohabitating)
plot_data$random_slope_mean_1 <- rowMeans(rsm_1)
plot_data$random_slope_se_1 <- apply(rsm_1, 1, function(x) sd(x) / sqrt(length(x)))

rsm_2 <- sapply(random_slopes_list, function(mat) mat[, 3])  # Extract column 3 (random slopes married)
plot_data$random_slope_mean_2 <- rowMeans(rsm_2)
plot_data$random_slope_se_2 <- apply(rsm_2, 1, function(x) sd(x) / sqrt(length(x)))


# Compute confidence intervals
plot_data$lower_CI_1 <- plot_data$total_effect_1 - 1.96 * plot_data$random_slope_se_1
plot_data$upper_CI_1 <- plot_data$total_effect_1 + 1.96 * plot_data$random_slope_se_1
plot_data$lower_CI_2 <- plot_data$total_effect_2 - 1.96 * plot_data$random_slope_se_2
plot_data$upper_CI_2 <- plot_data$total_effect_2 + 1.96 * plot_data$random_slope_se_2

plot_data_long <- plot_data %>%
  pivot_longer(
    cols = starts_with("total_effect"),
    names_to = "family_type",
    names_prefix = "total_effect_",
    values_to = "total_effect"
  ) %>%
  mutate(
    lower_CI = case_when(
      family_type == "1" ~ lower_CI_1,
      family_type == "2" ~ lower_CI_2
    ),
    upper_CI = case_when(
      family_type == "1" ~ upper_CI_1,
      family_type == "2" ~ upper_CI_2
    ),
    average_FRR_ft = case_when(
      family_type == "1" ~ average_FRR_1,
      family_type == "2" ~ average_FRR_2
    ),
    family_type = recode(family_type,
                         "1" = "1-2 children",
                         "2" = "3 or more children")
  )

# Plot it: 

agg_png("rslM2.4.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_data_long, aes(x = average_FRR_ft, y = total_effect, color = family_type, label = country)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.03, alpha = 0.6) +  # CI bars
  geom_text(vjust = -1, size = 3) +  # Country names
  #geom_smooth(method = "lm", se = FALSE) +  # Regression line
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.85) +
  labs(
    x = "Redistribution to family type (in 1000 PPS)",
    y = "Association with preferences for work-family balance policies",
    color = "Family type"
  ) +
  theme_minimal()
dev.off()

############################################################

## Plot 5: Effect of earnings distribution in the household and association with average FRR

rsl_M2.5<-with(imputation_gincdif,clmm(wrkprbf~earnings_dist+parentpartner+hinctnta_num+agea+gndr+lms+
                                         high_second_edu+average_FRR+(1+earnings_dist|cntry),
                                       weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M2.5))

random_slopes_list <- lapply(rsl_M2.5$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect_1<-pooled_results$estimate[5]
fixed_effect_2<-pooled_results$estimate[6]
fixed_effect_3<-pooled_results$estimate[7]

plot_pp<-data.frame(cbind(levels(data_full$cntry),numeric(14),numeric(14),numeric(14)))
colnames(plot_pp)<-c("country","total_effect_1","total_effect_2","total_effect_3")

####

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_1[i]=fixed_effect_1+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                                random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                                random_slopes_list[[5]][i,2],random_slopes_list[[6]][i,2],
                                                random_slopes_list[[7]][i,2],random_slopes_list[[8]][i,2],
                                                random_slopes_list[[9]][i,2],random_slopes_list[[10]][i,2])
}
plot_pp$total_effect_1<-as.numeric(plot_pp$total_effect_1)

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_2[i]=fixed_effect_2+mean(random_slopes_list[[1]][i,3],random_slopes_list[[2]][i,3],
                                                random_slopes_list[[3]][i,3],random_slopes_list[[4]][i,3],
                                                random_slopes_list[[5]][i,3],random_slopes_list[[6]][i,3],
                                                random_slopes_list[[7]][i,3],random_slopes_list[[8]][i,3],
                                                random_slopes_list[[9]][i,3],random_slopes_list[[10]][i,3])
}
plot_pp$total_effect_2<-as.numeric(plot_pp$total_effect_2)

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_3[i]=fixed_effect_3+mean(random_slopes_list[[1]][i,4],random_slopes_list[[2]][i,4],
                                                random_slopes_list[[3]][i,4],random_slopes_list[[4]][i,4],
                                                random_slopes_list[[5]][i,4],random_slopes_list[[6]][i,4],
                                                random_slopes_list[[7]][i,4],random_slopes_list[[8]][i,4],
                                                random_slopes_list[[9]][i,4],random_slopes_list[[10]][i,4])
}
plot_pp$total_effect_3<-as.numeric(plot_pp$total_effect_3)

plot_pp$average_FRR<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR[i]=data_full$average_FRR[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_1<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_1[i]=data_full$average_FRR_singleearner[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_2<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_2[i]=data_full$average_FRR_supearner[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_3<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_3[i]=data_full$average_FRR_doubearner[which(data_full$cntry==plot_pp$country[i])[1]]
}

## ADdd CIs

# Compute mean random slopes and standard error
rsm_1 <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes single parents)
plot_pp$random_slope_mean_1 <- rowMeans(rsm_1)
plot_pp$random_slope_se_1 <- apply(rsm_1, 1, function(x) sd(x) / sqrt(length(x)))

rsm_2 <- sapply(random_slopes_list, function(mat) mat[, 3])  # Extract column 3 (random slopes couples without children)
plot_pp$random_slope_mean_2 <- rowMeans(rsm_2)
plot_pp$random_slope_se_2 <- apply(rsm_2, 1, function(x) sd(x) / sqrt(length(x)))

# Compute mean random slopes and standard error
rsm_3 <- sapply(random_slopes_list, function(mat) mat[, 4])  # Extract column 2 (random slopes couples parents)
plot_pp$random_slope_mean_3 <- rowMeans(rsm_3)
plot_pp$random_slope_se_3 <- apply(rsm_3, 1, function(x) sd(x) / sqrt(length(x)))

# Compute confidence intervals
plot_pp$lower_CI_1 <- plot_pp$total_effect_1 - 1.96 * plot_pp$random_slope_se_1
plot_pp$upper_CI_1 <- plot_pp$total_effect_1 + 1.96 * plot_pp$random_slope_se_1
plot_pp$lower_CI_2 <- plot_pp$total_effect_2 - 1.96 * plot_pp$random_slope_se_2
plot_pp$upper_CI_2 <- plot_pp$total_effect_2 + 1.96 * plot_pp$random_slope_se_2
plot_pp$lower_CI_3 <- plot_pp$total_effect_3 - 1.96 * plot_pp$random_slope_se_3
plot_pp$upper_CI_3 <- plot_pp$total_effect_3 + 1.96 * plot_pp$random_slope_se_3

plot_pp_long <- plot_pp %>%
  pivot_longer(
    cols = starts_with("total_effect"),
    names_to = "family_type",
    names_prefix = "total_effect_",
    values_to = "total_effect"
  ) %>%
  mutate(
    lower_CI = case_when(
      family_type == "1" ~ lower_CI_1,
      family_type == "3" ~ lower_CI_3,
      family_type == "2" ~ lower_CI_2
    ),
    upper_CI = case_when(
      family_type == "1" ~ upper_CI_1,
      family_type == "3" ~ upper_CI_3,
      family_type == "2" ~ upper_CI_2
    ),
    average_FRR_ft = case_when(
      family_type == "1" ~ average_FRR_1,
      family_type == "3" ~ average_FRR_2,
      family_type == "2" ~ average_FRR_3
    ),
    family_type = recode(family_type,
                         "1" = "Single earner",
                         "3" = "Supplementary earner",
                         "2" = "Double earner") # Check that this is the correct ordering of the categories
  )

# Plot it: 

agg_png("rslM2.5.png", width = 1000, height = 625, units = "px", res = 144)
ggplot(plot_pp_long, aes(x = average_FRR_ft, y = total_effect, color = family_type, label = country)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.03, alpha = 0.6) +  # CI bars
  geom_text(vjust = -1, size = 1.5) +  # Country names
  geom_smooth(method = "lm", se = FALSE) +  # Regression line
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.85) +
  labs(
    x = "Redistribution to family type (in 1000 PPS)",
    y = "Association with preferences for work-family balance policies",
    color = "Family type"
  ) +
  theme_minimal()
dev.off()

############################################################

## Plot 6: Effect of age of minor children

rsl_M2.6<-with(imputation_gincdif,clmm(wrkprbf~age_ychild+earnings_dist+hinctnta_num+agea+gndr+lms+
                                         high_second_edu+average_FRR+(1+age_ychild|cntry),
                                       weights=anweight,link="logit"))
pooled_results<-summary(pool(rsl_M2.6))

random_slopes_list <- lapply(rsl_M2.6$analyses, function(model) {
  ranef(model)$cntry  # Extract random slopes per country
})

fixed_effect_1<-pooled_results$estimate[5]
fixed_effect_2<-pooled_results$estimate[6]
fixed_effect_3<-pooled_results$estimate[7]
fixed_effect_4<-pooled_results$estimate[8]

plot_pp<-data.frame(cbind(levels(data_full$cntry),numeric(14),numeric(14),numeric(14),numeric(14)))
colnames(plot_pp)<-c("country","total_effect_1","total_effect_2","total_effect_3","total_effect_4")

####

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_1[i]=fixed_effect_1+mean(random_slopes_list[[1]][i,2],random_slopes_list[[2]][i,2],
                                                random_slopes_list[[3]][i,2],random_slopes_list[[4]][i,2],
                                                random_slopes_list[[5]][i,2],random_slopes_list[[6]][i,2],
                                                random_slopes_list[[7]][i,2],random_slopes_list[[8]][i,2],
                                                random_slopes_list[[9]][i,2],random_slopes_list[[10]][i,2])
}
plot_pp$total_effect_1<-as.numeric(plot_pp$total_effect_1)

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_2[i]=fixed_effect_2+mean(random_slopes_list[[1]][i,3],random_slopes_list[[2]][i,3],
                                                random_slopes_list[[3]][i,3],random_slopes_list[[4]][i,3],
                                                random_slopes_list[[5]][i,3],random_slopes_list[[6]][i,3],
                                                random_slopes_list[[7]][i,3],random_slopes_list[[8]][i,3],
                                                random_slopes_list[[9]][i,3],random_slopes_list[[10]][i,3])
}
plot_pp$total_effect_2<-as.numeric(plot_pp$total_effect_2)

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_3[i]=fixed_effect_3+mean(random_slopes_list[[1]][i,4],random_slopes_list[[2]][i,4],
                                                random_slopes_list[[3]][i,4],random_slopes_list[[4]][i,4],
                                                random_slopes_list[[5]][i,4],random_slopes_list[[6]][i,4],
                                                random_slopes_list[[7]][i,4],random_slopes_list[[8]][i,4],
                                                random_slopes_list[[9]][i,4],random_slopes_list[[10]][i,4])
}
plot_pp$total_effect_3<-as.numeric(plot_pp$total_effect_3)

for (i in 1:nrow(plot_pp)){
  plot_pp$total_effect_4[i]=fixed_effect_3+mean(random_slopes_list[[1]][i,5],random_slopes_list[[2]][i,5],
                                                random_slopes_list[[3]][i,5],random_slopes_list[[4]][i,5],
                                                random_slopes_list[[5]][i,5],random_slopes_list[[6]][i,5],
                                                random_slopes_list[[7]][i,5],random_slopes_list[[8]][i,5],
                                                random_slopes_list[[9]][i,5],random_slopes_list[[10]][i,5])
}
plot_pp$total_effect_4<-as.numeric(plot_pp$total_effect_4)

plot_pp$average_FRR<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR[i]=data_full$average_FRR[which(data_full$cntry==plot_pp$country[i])[1]]
}

plot_pp$average_FRR_workp<-numeric(nrow(plot_pp))
for (i in 1:nrow(plot_pp)){
  plot_pp$average_FRR_workp[i]=data_full$average_FRR_workp[which(data_full$cntry==plot_pp$country[i])[1]]
}

## ADdd CIs

# Compute mean random slopes and standard error
rsm_1 <- sapply(random_slopes_list, function(mat) mat[, 2])  # Extract column 2 (random slopes single parents)
plot_pp$random_slope_mean_1 <- rowMeans(rsm_1)
plot_pp$random_slope_se_1 <- apply(rsm_1, 1, function(x) sd(x) / sqrt(length(x)))

rsm_2 <- sapply(random_slopes_list, function(mat) mat[, 3])  # Extract column 3 (random slopes couples without children)
plot_pp$random_slope_mean_2 <- rowMeans(rsm_2)
plot_pp$random_slope_se_2 <- apply(rsm_2, 1, function(x) sd(x) / sqrt(length(x)))

# Compute mean random slopes and standard error
rsm_3 <- sapply(random_slopes_list, function(mat) mat[, 4])  # Extract column 4 (random slopes couples parents)
plot_pp$random_slope_mean_3 <- rowMeans(rsm_3)
plot_pp$random_slope_se_3 <- apply(rsm_3, 1, function(x) sd(x) / sqrt(length(x)))

# Compute mean random slopes and standard error
rsm_4 <- sapply(random_slopes_list, function(mat) mat[, 5])  # Extract column 5 (random slopes couples parents)
plot_pp$random_slope_mean_4 <- rowMeans(rsm_4)
plot_pp$random_slope_se_4 <- apply(rsm_4, 1, function(x) sd(x) / sqrt(length(x)))

# Compute confidence intervals
plot_pp$lower_CI_1 <- plot_pp$total_effect_1 - 1.96 * plot_pp$random_slope_se_1
plot_pp$upper_CI_1 <- plot_pp$total_effect_1 + 1.96 * plot_pp$random_slope_se_1
plot_pp$lower_CI_2 <- plot_pp$total_effect_2 - 1.96 * plot_pp$random_slope_se_2
plot_pp$upper_CI_2 <- plot_pp$total_effect_2 + 1.96 * plot_pp$random_slope_se_2
plot_pp$lower_CI_3 <- plot_pp$total_effect_3 - 1.96 * plot_pp$random_slope_se_3
plot_pp$upper_CI_3 <- plot_pp$total_effect_3 + 1.96 * plot_pp$random_slope_se_3
plot_pp$lower_CI_4 <- plot_pp$total_effect_4 - 1.96 * plot_pp$random_slope_se_4
plot_pp$upper_CI_4 <- plot_pp$total_effect_4 + 1.96 * plot_pp$random_slope_se_4

plot_pp_long <- plot_pp %>%
  pivot_longer(
    cols = starts_with("total_effect"),
    names_to = "family_type",
    names_prefix = "total_effect_",
    values_to = "total_effect"
  ) %>%
  mutate(
    lower_CI = case_when(
      family_type == "1" ~ lower_CI_1,
      family_type == "2" ~ lower_CI_2,
      family_type == "3" ~ lower_CI_3,
      family_type == "4" ~ lower_CI_4
    ),
    upper_CI = case_when(
      family_type == "1" ~ upper_CI_1,
      family_type == "2" ~ upper_CI_2,
      family_type == "3" ~ upper_CI_3,
      family_type == "4" ~ upper_CI_4
    ),
    average_FRR_ft = case_when(
      family_type == "1" ~ average_FRR_workp,
      family_type == "2" ~ average_FRR_workp,
      family_type == "3" ~ average_FRR_workp,
      family_type == "4" ~ average_FRR_workp,
    ),
    family_type = recode(family_type,
                         "1" = "<3",
                         "2" = "3-5",
                         "3" = "6-11",
                         "4" = "12-17") # Check that this is the correct ordering of the categories
  )

# Plot it: 

agg_png("rslM2.6.png", width = 1000, height = 62, units = "px", res = 144)
ggplot(plot_pp_long, aes(x = average_FRR_ft, y = total_effect, color = family_type, label = country)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.03, alpha = 0.6) +  # CI bars
  geom_text(vjust = -1, size = 1.5) +  # Country names
  geom_smooth(method = "lm", se = FALSE) +  # Regression line
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.85) +
  labs(
    x = "Redistribution to family type (in 1000 PPS)",
    y = "Association with preferences for work-family balance policies",
    color = "Family type"
  ) +
  theme_minimal()
dev.off()

###################

RSModels<-list(rsl_M1.1,rsl_M1.2,rsl_M1.3,rsl_M1.4,rsl_M1.5,
               rsl_M2.1,rsl_M2.2,rsl_M2.3,rsl_M2.4,rsl_M2.5,rsl_M2.6)

save(RSModels,file="RSModels_P3.RData")

rsl_M1.1<-RSModels[[1]]
rsl_M1.2<-RSModels[[2]]
rsl_M1.3<-RSModels[[3]]
rsl_M1.4<-RSModels[[4]]
rsl_M1.5<-RSModels[[5]]
rsl_M2.1<-RSModels[[6]]
rsl_M2.2<-RSModels[[7]]
rsl_M2.3<-RSModels[[8]]
rsl_M2.4<-RSModels[[9]]
rsl_M2.5<-RSModels[[10]]

rsl_M2.6<-RSModels[[11]]
