##########################################################
## STUDY 3: Family-based redistribution and preferences ##
##########################################################

####
## SCRIPT 2: EXPLORATORY ANALYSES
####

library(lme4)
library(lmerTest)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(reshape2)
library(tidyr)
library(extrafont)
library(patchwork)
library(ie2misc)
library(forcats)
library(ragg)
library(fastDummies)
library(corrplot)
library(viridis)

setwd("D:/BIGSSS/Dissertation/Study 3/Scripts/2016Data")
load("DataStudy3_2016.RData")

#############################

# 1) DISTRIBUTION OF THE DEPENDENT VARIABLES

#########################################
# Preferences for General Redistribution
data_summary <- data_p3 %>%
  group_by(cntry, gincdif) %>%
  summarise(count = n()) %>%
  ungroup()

total_summary <-data_p3 %>%
  group_by(gincdif) %>%
  summarise(count = n()) %>%
  mutate(cntry = "Total")

data_summary<-bind_rows(data_summary,total_summary)

data_summary<-data_summary[-which(is.na(data_summary$gincdif)),]

data_summary$prop<-numeric(nrow(data_summary))
for (i in 1:nrow(data_summary)){
  data_summary$prop[i]=data_summary$count[i]/sum(data_summary$count[which(data_summary$cntry==data_summary$cntry[i])])
}
data_summary$prop<-round(data_summary$prop,2)

gincdif_plot<-ggplot(data_summary, aes(x = cntry, y = gincdif, fill = prop)) +
  geom_tile() +
  geom_text(aes(label = prop), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #theme(text = element_text(family = "Times New Roman"))+
  labs(title = "",
       x="Country",y="Government should reduce differences in income levels",fill="Proportion")

agg_png("Exploratory_gincdif.png", width = 1500, height = 975, units = "px", res = 144)
gincdif_plot
dev.off()

###############################################
# Preferences for Work-Family Balance Policies
data_summary <- data_p3 %>%
  group_by(cntry, wrkprbf) %>%
  summarise(count = n()) %>%
  ungroup()

total_summary <-data_p3 %>%
  group_by(wrkprbf) %>%
  summarise(count = n()) %>%
  mutate(cntry = "Total")

data_summary<-bind_rows(data_summary,total_summary)

data_summary<-data_summary[-which(is.na(data_summary$wrkprbf)),]

data_summary$prop<-numeric(nrow(data_summary))
for (i in 1:nrow(data_summary)){
  data_summary$prop[i]=data_summary$count[i]/sum(data_summary$count[which(data_summary$cntry==data_summary$cntry[i])])
}
data_summary$prop<-round(data_summary$prop,2)

wrkprbf_plot<-ggplot(data_summary, aes(x = cntry, y = wrkprbf, fill = prop)) +
  geom_tile() +
  geom_text(aes(label = prop), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Introduction of work-family balance policies",fill="Proportion")

agg_png("Exploratory_WFB.png", width = 1500, height = 975, units = "px", res = 144)
wrkprbf_plot
dev.off()

#############################

# 2) DISTRIBUTION OF FAMILY TYPES

#############################
# Family types in general (too many categories, it is not a clear plot)
data_p3$FamilyType2<-fct_cross(data_p3$rshipa,data_p3$nchild,data_p3$earn_dist2,
                               keep_empty=F) 
summary(data_p3$FamilyType2) # Here we see all 84 family types considered (plus the single individuals)

levels.ft2<-levels(data_p3$FamilyType2)[c(3,12,4,7,13,16,
                                          1,10,19,25,5,14,21,27,8,17,23,29,
                                          2,11,20,26,6,15,22,28,9,18,24,30)]

data_p3$FamilyType2<-factor(data_p3$FamilyType2,
                            levels=levels.ft2)

data_summary <- data_p3 %>%
  group_by(cntry, FamilyType2) %>%
  summarise(count = n()) %>%
  ungroup()

data_summary<-data_summary[-which(data_summary$FamilyType2 %in% c("Single individual:no children:Single earner",
                                                                  "Single individual:no children:No earner")),]

ggplot(data_summary, aes(x = cntry, y = FamilyType2, fill = count)) +
  geom_tile() +
  geom_text(aes(label = count), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Family Type",Fill="Count")

total_summary <-data_p3 %>%
  group_by(FamilyType2) %>%
  summarise(count = n()) %>%
  mutate(cntry = "Total")

data_summary<-bind_rows(data_summary,total_summary)
data_summary<-data_summary[-which(data_summary$FamilyType2 %in% c("Single individual:no children:Single earner",
                                                                  "Single individual:no children:No earner")),]

data_summary$prop<-numeric(nrow(data_summary))
for (i in 1:nrow(data_summary)){
  data_summary$prop[i]=data_summary$count[i]/sum(data_summary$count[which(data_summary$cntry==data_summary$cntry[i])])
}
data_summary$prop<-round(data_summary$prop,2)

ggplot(data_summary, aes(x = cntry, y = FamilyType2, fill = prop)) +
  geom_tile() +
  geom_text(aes(label = prop), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Family Type",Fill="Proportion")

#############################
# Partnership and parenthood
data_summary <- data_p3 %>%
  group_by(cntry, parentpartner) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(data_summary, aes(x = cntry, y = parentpartner, fill = count)) +
  geom_tile() +
  geom_text(aes(label = count), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Partnership and Parenthood",fill="Count")

total_summary <-data_p3 %>%
  group_by(parentpartner) %>%
  summarise(count = n()) %>%
  mutate(cntry = "Total")

data_summary<-bind_rows(data_summary,total_summary)

data_summary$prop<-numeric(nrow(data_summary))
for (i in 1:nrow(data_summary)){
  data_summary$prop[i]=data_summary$count[i]/sum(data_summary$count[which(data_summary$cntry==data_summary$cntry[i])])
}
data_summary$prop<-round(data_summary$prop,2)

agg_png("Exploratory_ParentPartner.png", width = 1500, height = 975, units = "px", res = 144)
ggplot(data_summary, aes(x = cntry, y = parentpartner, fill = prop)) +
  geom_tile() +
  geom_text(aes(label = prop), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Partnership and Parenthood",fill="Proportion")
dev.off()

#############################
# Number of children
data_summary <- data_p3 %>%
  group_by(cntry, nchild) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(data_summary, aes(x = cntry, y = nchild, fill = count)) +
  geom_tile() +
  geom_text(aes(label = count), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Number of children",Fill="Count")

total_summary <-data_p3 %>%
  group_by(nchild) %>%
  summarise(count = n()) %>%
  mutate(cntry = "Total")

data_summary<-bind_rows(data_summary,total_summary)

data_summary$prop<-numeric(nrow(data_summary))
for (i in 1:nrow(data_summary)){
  data_summary$prop[i]=data_summary$count[i]/sum(data_summary$count[which(data_summary$cntry==data_summary$cntry[i])])
}
data_summary$prop<-round(data_summary$prop,2)

agg_png("Exploratory_nchild.png", width = 1500, height = 975, units = "px", res = 144)
ggplot(data_summary, aes(x = cntry, y = nchild, fill = prop)) +
  geom_tile() +
  geom_text(aes(label = prop), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Number of children",fill="Proportion")
dev.off()

#############################
# Number of marital status
data_summary <- data_p3 %>%
  group_by(cntry, maritalstatus_bin) %>%
  summarise(count = n()) %>%
  ungroup()

data_summary<-data_summary[-which(is.na(data_summary$maritalstatus_bin)),]

ggplot(data_summary, aes(x = cntry, y = maritalstatus_bin, fill = count)) +
  geom_tile() +
  geom_text(aes(label = count), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Marital status",fill="Count")

total_summary <-data_p3 %>%
  group_by(maritalstatus_bin) %>%
  summarise(count = n()) %>%
  mutate(cntry = "Total")

data_summary<-bind_rows(data_summary,total_summary)

data_summary<-data_summary[-which(is.na(data_summary$maritalstatus_bin)),]

data_summary$prop<-numeric(nrow(data_summary))
for (i in 1:nrow(data_summary)){
  data_summary$prop[i]=data_summary$count[i]/sum(data_summary$count[which(data_summary$cntry==data_summary$cntry[i])])
}
data_summary$prop<-round(data_summary$prop,2)

agg_png("Exploratory_maritalstatus.png", width = 1500, height = 975, units = "px", res = 144)
ggplot(data_summary, aes(x = cntry, y = maritalstatus_bin, fill = prop)) +
  geom_tile() +
  geom_text(aes(label = prop), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Marital status",fill="Proportion")
dev.off()

#############################
# Distribution of market earnings
data_summary <- data_p3 %>%
  group_by(cntry, earnings_dist) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(data_summary, aes(x = cntry, y = earnings_dist, fill = count)) +
  geom_tile() +
  geom_text(aes(label = count), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Distribution of market earnings",Fill="Count")

total_summary <-data_p3 %>%
  group_by(earnings_dist) %>%
  summarise(count = n()) %>%
  mutate(cntry = "Total")

data_summary<-bind_rows(data_summary,total_summary)

data_summary$prop<-numeric(nrow(data_summary))
for (i in 1:nrow(data_summary)){
  data_summary$prop[i]=data_summary$count[i]/sum(data_summary$count[which(data_summary$cntry==data_summary$cntry[i])])
}
data_summary$prop<-round(data_summary$prop,2)

agg_png("Exploratory_Earnings.png", width = 1500, height = 975, units = "px", res = 144)
ggplot(data_summary, aes(x = cntry, y = earnings_dist, fill = prop)) +
  geom_tile() +
  geom_text(aes(label = prop), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Distribution of market earnings",fill="Proportion")
dev.off()

#############################
# Distribution of household income
data_summary <- data_p3 %>%
  group_by(cntry, hhinc) %>%
  summarise(count = n()) %>%
  ungroup()

data_summary<-data_summary[-which(is.na(data_summary$hhinc)),]

ggplot(data_summary, aes(x = cntry, y = hhinc, fill = count)) +
  geom_tile() +
  geom_text(aes(label = count), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Household Income",Fill="Count")

total_summary <-data_p3 %>%
  group_by(hhinc) %>%
  summarise(count = n()) %>%
  mutate(cntry = "Total")

data_summary<-bind_rows(data_summary,total_summary)
data_summary<-data_summary[-which(is.na(data_summary$hhinc)),]

data_summary$prop<-numeric(nrow(data_summary))
for (i in 1:nrow(data_summary)){
  data_summary$prop[i]=data_summary$count[i]/sum(data_summary$count[which(data_summary$cntry==data_summary$cntry[i])])
}
data_summary$prop<-round(data_summary$prop,2)

agg_png("Exploratory_HH_Income.png", width = 1500, height = 975, units = "px", res = 144)
ggplot(data_summary, aes(x = cntry, y = hhinc, fill = prop)) +
  geom_tile() +
  geom_text(aes(label = prop), color = "white") +
  scale_fill_viridis_c() + # Better color scale for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x="Country",y="Household Income",fill="Proportion")
dev.off()

#############################

# 3) DISTRIBUTION OF FAMILY-RELATED REDISTRIBUTION

#############################

# At individual level:
# Boxplot
ggplot(data_p3, aes(x = cntry, y = Diff_PPS)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "",
       x = "Country",
       y = "Family-Related Redistribution (in PPS)")

# Violin plot
country_means <- data_p3 %>%
  group_by(cntry) %>%
  summarize(mean_diff = mean(Diff_PPS, na.rm = TRUE))
order(country_means$mean_diff)

data_p3$cntry2<-factor(data_p3$cntry,levels=country_means$cntry[order(country_means$mean_diff)])

agg_png("Exploratory_FRR.png", width = 1500, height = 975, units = "px", res = 144)
ggplot(data_p3, aes(x = cntry2, y = Diff_PPS)) +
  geom_violin(fill = "#21908C", alpha = 0.7) +
  stat_summary(fun = mean, geom = "crossbar", color = "black", width=0.4) +
  theme_minimal() +
  labs(title = "",
       x = "Country",
       y = "Family-Related Redistribution (in PPS)")
dev.off()

# Density plot
ggplot(data_p3, aes(x = Diff_PPS, fill = cntry)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  facet_wrap(~ cntry) +
  labs(title = "",
       x = "Family-Related Redistribution (in PPS)",
       y = "Density")

# At aggregated level:

country_means <- data_p3 %>%
  group_by(cntry) %>%
  summarize(average_FRR = mean(average_FRR, na.rm = TRUE))

country_means$cntry<-factor(country_means$cntry,levels=country_means$cntry[order(country_means$average_FRR)])
country_means$average_FRR<-country_means$average_FRR*1000

agg_png("Exploratory_FRR_cntry.png", width = 1500, height = 975, units = "px", res = 144)
ggplot(country_means, aes(x = cntry, y = average_FRR)) +
  geom_bar(stat = "identity",fill = "#21908C") +
  #scale_fill_viridis_c(option = "viridis") +  # Apply the viridis color scale
  theme_minimal() +
  labs(title = "",
       x = "Country",
       y = "Average Family-Related Redistribution (in PPS)",
       fill = "") +  # Legend title
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed
dev.off()

### Combine violin plot with average FRR

# Compute average_FRR values from the barplot (used for ordering)
country_means_bar <- data_p3 %>%
  group_by(cntry) %>%
  summarize(average_FRR = mean(average_FRR, na.rm = TRUE))

country_means_bar <- country_means_bar %>%
  arrange(average_FRR)  # Order by average_FRR

country_means_bar$cntry <- factor(country_means_bar$cntry, levels = country_means_bar$cntry)  # Reorder factor
country_means_bar$average_FRR <- country_means_bar$average_FRR * 1000  # Convert as in the original barplot

# Apply the same order to data_p3
data_p3$cntry2 <- factor(data_p3$cntry, levels = levels(country_means_bar$cntry))

# Plot combined violin + overlayed bar values
agg_png("Exploratory_FRR_combined.png", width = 1500, height = 975, units = "px", res = 144)
ggplot(data_p3, aes(x = cntry2, y = Diff_PPS)) +
  geom_violin(fill = "#21908C", alpha = 0.7) +
  geom_errorbar(data = country_means_bar, aes(x = cntry, y = average_FRR, ymin = average_FRR, ymax = average_FRR),
                color = "black", width = 0.4, size = 1.2) +
  #stat_summary(fun = mean, geom = "crossbar", color = "red", width=0.4) + #(add also sample mean?)
  theme_minimal() +
  labs(title = "",
       x = "Country",
       y = "Family-Related Redistribution (in PPS)")
dev.off()
#############################

# 4) CORRELATIONS BETWEEN ALL RELEVANT VARIABLES

#########################

data_full<-data_p3[,c("gincdif","gincdif_num","wrkprbf","wrkprbf_num",
                      "Diff_PPS_ths","Diff_PPS_ths_na","Diff_missing",
                      "NDI_RP1_PPS_ths","NDI_RP1_PPS_ths_na","NDI_RP1_missing",
                      "hinctnta_num","hinctnta_num_na","hinctnta_missing",
                      "agea","gndr","cohort","lms","high_second_edu",
                      "rlgdgr","rlgdgr_num","freehms","freehms_num",
                      "single_family","single_family_num",
                      "parentpartner","nchild","workingparents","earnings_dist","earnings_dist2",
                      "cntry","regime","GDP_pc","Gini",
                      "average_FRR","average_FRR_sp","average_FRR_cwc",
                      "average_FRR_cnc","average_FRR_lwp","average_FRR_married",
                      "average_FRR_hk","average_FRR_workp","anweight")]

data_full_dum<-dummy_cols(data_full,select_columns=c("gndr","cohort","lms","high_second_edu",
                                                     "parentpartner","nchild","earnings_dist",
                                                     "cntry","regime"))http://127.0.0.1:20861/graphics/plot_zoom_png?width=2115&height=1112

#http://127.0.0.1:34845/graphics/plot_zoom_png?width=2055&height=1112

data_full_dum<-data_full_dum[,c("gincdif_num","wrkprbf_num",
                                "Diff_PPS_ths","NDI_RP1_PPS_ths",
                                "single_family_num","parentpartner_Single no children",
                                "parentpartner_Single parent","parentpartner_Couple no children",  
                                "parentpartner_Couple parents","nchild_no children",
                                "nchild_1-2 children","nchild_3 children or more",
                                "workingparents","earnings_dist_No earner","earnings_dist_Single earner",
                                "earnings_dist_Supplementary earner","earnings_dist_Double earner",
                                "hinctnta_num","agea","gndr_Male","cohort_<20","cohort_20-45","cohort_>45",
                                "lms_Employed","lms_NEET","lms_In education","high_second_edu_Yes")]

colnames(data_full_dum)=c("Pref. Red.","Pref. WFB","FRR","Individual NDI","Single no children",
                          "Single parent","Couple no children","Couple parents","0 children",
                          "1-2 children","3 children or more","Working parents","No earner","Single earner",
                          "Sup. Earner","Double Earner","HH Income","Age","Gender: male",
                          "<20","20-45",">45","Employed","NEET","In education","High Sec. Edu: Yes")

agg_png("Exploratory_correlations.png", width = 1500, height = 1500, units = "px", res = 144)
corrplot(cor(data_full_dum,use="complete.obs"),method="col",
         #col=viridis(50),
         tl.col="black")
dev.off()

#############################

# 5) AVERAGE PREFERENCES FOR FAMILY TYPES

#############################

## 5.1) Preferences for Redistribution

# Individuals with Family vs Single Individuals:
singleind_av<- data_p3 %>%
  filter(single_family == "Single") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for individuals with family (with children and/or partner)
indwfam_av<- data_p3 %>%
  filter(single_family == "With family") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

si_iwm<-data.frame(Country=as.factor(levels(data_p3$cntry)), Single_Individual=singleind_av$mean_gincdif_num,
                   With_Family=indwfam_av$mean_gincdif_num,Difference=indwfam_av$mean_gincdif_num-singleind_av$mean_gincdif_num)
si_iwm<-si_iwm[order(si_iwm$Difference),]
si_iwm$Country<-factor(si_iwm$Country,levels=si_iwm$Country[order(si_iwm$Difference)])
si_iwm<-melt(si_iwm,id.vars="Country")

p1<-ggplot(si_iwm[1:28,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Single_Individual" = "#21908C", "With_Family" = "#FDE725FF"),
                     labels = c("Single_Individual" = "Single Individuals", "With_Family" = "Individuals with Family"),
                     limits = c("With_Family", "Single_Individual")) + # Change the order here 
  scale_shape_manual(values = c("Single_Individual" = 16, "With_Family" = 17),
                     labels = c("Single_Individual" = "Single Individuals", "With_Family" = "Individuals with Family"),
                     limits = c("With_Family", "Single_Individual")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group", title = "") +
  theme_minimal(base_family="Times New Roman")+theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(si_iwm[29:nrow(si_iwm),], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  #scale_fill_viridis_c(option = "viridis")+
  labs(#title="Differences (Individuals with family - Single individuals)",
    x = "Country", y = "Difference") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_SI<-p1/p2+plot_layout(heights=c(2,1));print(cp_SI)

agg_png("Exploratory_AvPref_WithFamily.png", width = 1500, height = 1000, units = "px", res = 144)
cp_SI
dev.off()

##

cor_sf <- data_p3 %>%
  group_by(cntry) %>%
  summarise(correlation = cor(gincdif_num, as.numeric(single_family), use = "complete.obs")) %>%
  pull(correlation)

p_sf <- data_p3 %>%
  group_by(cntry) %>%
  summarise(p.value = cor.test(gincdif_num, as.numeric(single_family), use = "complete.obs")$p.value) %>%
  pull(p.value)

singlefamily_cor<-data.frame(rbind(cor_sf,round(p_sf,3)))
colnames(singlefamily_cor)<-c(levels(data_p3$cntry))
rownames(singlefamily_cor)<-c("Estimate","p.value")

plotdata_sf<-data.frame(Country=colnames(singlefamily_cor),
                        Estimate=cor_sf,
                        p.value=p_sf)

plotdata_sf$Country <- reorder(plotdata_sf$Country, plotdata_sf$Estimate)

p1<-ggplot(plotdata_sf, aes(x = Country, y = Estimate))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Estimate") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

p2<-ggplot(plotdata_sf, aes(x = Country, y = p.value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0.05,linetype="dashed",color="red",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "p-value") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

corsf_plot<-p1/p2+plot_layout(heights=c(2,1));print(corsf_plot)

agg_png("Exploratory_Cor_WithFamily.png", width = 1500, height = 1000, units = "px", res = 144)
corsf_plot
dev.off()

###########

# Living with a partner vs not living with a partner

# Average preference for individuals living with a partner
lwp_av<- data_p3 %>%
  filter(lwpartner == "yes") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for individuals not living with a partner
nlwp_av<- data_p3 %>%
  filter(lwpartner == "no") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

lwp_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), Alone=nlwp_av$mean_gincdif_num,
                     With_Partner=lwp_av$mean_gincdif_num,Difference=lwp_av$mean_gincdif_num-nlwp_av$mean_gincdif_num)
lwp_plot<-lwp_plot[order(lwp_plot$Difference),]
lwp_plot$Country<-factor(lwp_plot$Country,levels=lwp_plot$Country[order(lwp_plot$Difference)])
lwp_plot <- melt(lwp_plot, id.vars = "Country")

p1<-ggplot(lwp_plot[1:28,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Alone" = "#21908C", "With_Partner" = "#FDE725FF"),
                     labels = c("Alone" = "Does not live with partner", "With_Partner" = "Lives with partner"),
                     limits = c("With_Partner", "Alone")) + # Change the order here 
  scale_shape_manual(values = c("Alone" = 16, "With_Partner" = 17),
                     labels = c("Alone" = "Does not live with partner", "With_Partner" = "Lives with partner"),
                     limits = c("With_Partner", "Alone")) + # Change the order here 
  # Customize labels and theme
  labs(#title="Average preference for redistribution",
    x = "", y = "Average Preference", color = "Group", shape = "Group") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(lwp_plot[29:nrow(lwp_plot),], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Difference") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_lwp<-p1/p2+plot_layout(heights=c(2,1));print(cp_lwp)


agg_png("Exploratory_AvPref_LWP.png", width = 1500, height = 1000, units = "px", res = 144)
cp_lwp
dev.off()

##

cor_lwp <- data_p3 %>%
  group_by(cntry) %>%
  summarise(correlation = cor(gincdif_num, as.numeric(lwpartner), use = "complete.obs")) %>%
  pull(correlation)

p_lwp <- data_p3 %>%
  group_by(cntry) %>%
  summarise(p.value = cor.test(gincdif_num, as.numeric(lwpartner), use = "complete.obs")$p.value) %>%
  pull(p.value)

lwp_cor<-data.frame(rbind(cor_lwp,round(p_lwp,3)))
colnames(lwp_cor)<-c(levels(data_p3$cntry))
rownames(lwp_cor)<-c("Estimate","p.value")

plotdata_lwp<-data.frame(Country=colnames(lwp_cor),
                        Estimate=cor_lwp,
                        p.value=p_lwp)

plotdata_lwp$Country <- reorder(plotdata_lwp$Country, plotdata_lwp$Estimate)

p1<-ggplot(plotdata_lwp, aes(x = Country, y = Estimate))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Estimate") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

p2<-ggplot(plotdata_lwp, aes(x = Country, y = p.value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0.05,linetype="dashed",color="red",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "p-value") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

corlwp_plot<-p1/p2+plot_layout(heights=c(2,1));print(corlwp_plot)

agg_png("Exploratory_Cor_lwp.png", width = 1500, height = 1000, units = "px", res = 144)
corlwp_plot
dev.off()

#################################

## Married vs cohabiting couples

# Average preference for married couples
marriedcohab_av<- data_p3 %>%
  filter(lwpartner=="yes") %>%
  filter(maritalstatus_bin== "Married or in civil union") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

nmarriedcohab_av<- data_p3 %>%
  filter(lwpartner=="yes") %>%
  filter(maritalstatus_bin== "Not married or in civil union") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

marcoh_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), Not_Married=nmarriedcohab_av$mean_gincdif_num,
                        Married=marriedcohab_av$mean_gincdif_num)
marcoh_plot$Difference<-marcoh_plot$Married-marcoh_plot$Not_Married
marcoh_plot<-marcoh_plot[order(marcoh_plot$Difference),]
marcoh_plot$Country<-factor(marcoh_plot$Country,levels=marcoh_plot$Country[order(marcoh_plot$Difference)])
marcoh_plot <- melt(marcoh_plot, id.vars = "Country")

p1<-ggplot(marcoh_plot[1:28,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Not_Married" = "#21908C", "Married" = "#FDE725FF"),
                     labels = c("Married" = "Married", "Not_Married" = "Not Married"),
                     limits = c("Married","Not_Married")) + # Change the order here 
  scale_shape_manual(values = c("Married" = 16, "Not_Married" = 17),
                     labels = c("Married" = "Married", "Not_Married" = "Not Married"),
                     limits = c("Married","Not_Married")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group", title = "") +
  theme_minimal(base_family="Times New Roman")+theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(marcoh_plot[29:nrow(marcoh_plot),], aes(x = Country, y = value))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (Individuals with family - Single individuals)",
    x = "Country", y = "Difference") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_marcoh<-p1/p2+plot_layout(heights=c(2,1));print(cp_marcoh)

agg_png("Exploratory_AvPref_married.png", width = 1500, height = 1000, units = "px", res = 144)
cp_marcoh
dev.off()

##

cor_married <- data_p3 %>%
  group_by(cntry) %>%
  summarise(correlation = cor(gincdif_num, as.numeric(maritalstatus_bin), use = "complete.obs")) %>%
  pull(correlation)

p_married <- data_p3 %>%
  group_by(cntry) %>%
  summarise(p.value = cor.test(gincdif_num, as.numeric(maritalstatus_bin), use = "complete.obs")$p.value) %>%
  pull(p.value)

married_cor<-data.frame(rbind(cor_married,round(p_married,3)))
colnames(married_cor)<-c(levels(data_p3$cntry))
rownames(married_cor)<-c("Estimate","p.value")

plotdata_married<-data.frame(Country=colnames(married_cor),
                         Estimate=cor_married,
                         p.value=p_married)

plotdata_married$Country <- reorder(plotdata_married$Country, plotdata_married$Estimate)

p1<-ggplot(plotdata_married, aes(x = Country, y = Estimate))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Estimate") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

p2<-ggplot(plotdata_married, aes(x = Country, y = p.value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0.05,linetype="dashed",color="red",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "p-value") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cormarried_plot<-p1/p2+plot_layout(heights=c(2,1));print(cormarried_plot)

agg_png("Exploratory_Cor_married.png", width = 1500, height = 1000, units = "px", res = 144)
cormarried_plot
dev.off()

#########

# Number of children

# Average preference for individuals without children
nochild_av<- data_p3 %>%
  filter(nmchildren_grp2 == "no children") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for individuals with 1-2 children
child12_av<- data_p3 %>%
  filter(nmchildren_grp2 == "1-2 children") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for individuals with 3 or more children
child3_av<- data_p3 %>%
  filter(nmchildren_grp2 == "3 children or more") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

nchild_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), no_child=nochild_av$mean_gincdif_num,
                        one_or_two=child12_av$mean_gincdif_num,three_or_more=child3_av$mean_gincdif_num)
for (i in 1:nrow(nchild_plot)){
  nchild_plot$Difference[i]<-madstat(c(nchild_plot$no_child[i],nchild_plot$one_or_two[i],nchild_plot$three_or_more[i]))
}
nchild_plot<-nchild_plot[order(nchild_plot$Difference),]
nchild_plot$Country<-factor(nchild_plot$Country,levels=nchild_plot$Country[order(nchild_plot$Difference)])
nchild_plot <- melt(nchild_plot, id.vars = "Country")

p1<-ggplot(nchild_plot[1:42,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("no_child" = "#440154FF", "one_or_two" = "#21908C", "three_or_more"="#FDE725FF"),
                     labels = c("no_child" = "0 children", "one_or_two"="1-2 children","three_or_more"="3 children or more"),
                     limits = c("no_child","one_or_two","three_or_more")) + # Change the order here 
  scale_shape_manual(values = c("no_child" = 16, "one_or_two" = 17, "three_or_more"=18),
                     labels = c("no_child" = "0 children", "one_or_two"="1-2 children","three_or_more"="3 children or more"),
                     limits = c("no_child","one_or_two","three_or_more")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(nchild_plot[43:56,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  #geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_child<-p1/p2+plot_layout(heights=c(2,1));print(cp_child)

agg_png("Exploratory_AvPref_nchild.png", width = 1500, height = 1000, units = "px", res = 144)
cp_child
dev.off()

##

cor_nchild <- data_p3 %>%
  group_by(cntry) %>%
  summarise(correlation = cor(gincdif_num, nchildren, use = "complete.obs")) %>%
  pull(correlation)

p_nchild <- data_p3 %>%
  group_by(cntry) %>%
  summarise(p.value = cor.test(gincdif_num, nchildren, use = "complete.obs")$p.value) %>%
  pull(p.value)

nchild_cor<-data.frame(rbind(cor_nchild,round(p_nchild,3)))
colnames(nchild_cor)<-c(levels(data_p3$cntry))
rownames(nchild_cor)<-c("Estimate","p.value")

plotdata_nchild<-data.frame(Country=colnames(nchild_cor),
                             Estimate=cor_nchild,
                             p.value=p_nchild)

plotdata_nchild$Country <- reorder(plotdata_nchild$Country, plotdata_nchild$Estimate)

p1<-ggplot(plotdata_nchild, aes(x = Country, y = Estimate))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Estimate") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

p2<-ggplot(plotdata_nchild, aes(x = Country, y = p.value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0.05,linetype="dashed",color="red",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "p-value") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cornchild_plot<-p1/p2+plot_layout(heights=c(2,1));print(cornchild_plot)

agg_png("Exploratory_Cor_nchild.png", width = 1500, height = 1000, units = "px", res = 144)
cornchild_plot
dev.off()

############

# Combinations of partnership and parenthood

# Average preference for single parents
sp_av<- data_p3 %>%
  filter(parentpartner == "Single parent") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for couples with children
cwc_av<- data_p3 %>%
  filter(parentpartner == "Couple parents") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for couples without children
cnc_av<- data_p3 %>%
  filter(parentpartner == "Couple no children") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

ft_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), Single_Individual=singleind_av$mean_gincdif_num,
                    Single_Parent=sp_av$mean_gincdif_num,Couple_w_Children=cwc_av$mean_gincdif_num,Couple_no_Children=cnc_av$mean_gincdif_num)

for (i in 1:nrow(ft_plot)){ # Calculate mean absolute deviation among the 4 groups
  ft_plot$Difference[i]<-madstat(c(ft_plot$Single_Individual[i],ft_plot$Single_Parent[i],
                                   ft_plot$Couple_w_Children[i],ft_plot$Couple_no_Children[i]))
}
ft_plot<-ft_plot[order(ft_plot$Difference),]
ft_plot$Country<-factor(ft_plot$Country,levels=ft_plot$Country[order(ft_plot$Difference)])
ft_plot <- melt(ft_plot, id.vars = "Country")

p1<-ggplot(ft_plot[1:56,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Single_Individual" = "#440154FF", "Single_Parent" = "#21908C", "Couple_w_Children"="#93D741FF",
                                "Couple_no_Children"="#F7E620FF"),
                     labels = c("Single_Individual" = "Single individuals", "Single_Parent" = "Single parents", 
                                "Couple_w_Children"="Couples with children",
                                "Couple_no_Children"="Couples without children"),
                     limits = c("Single_Individual", "Single_Parent", "Couple_w_Children","Couple_no_Children")) + 
  scale_shape_manual(values = c("Single_Individual" = 16, "Single_Parent" = 17, "Couple_w_Children"=15,
                                "Couple_no_Children"=18),
                     labels = c("Single_Individual" = "Single individuals", "Single_Parent" = "Single parents", 
                                "Couple_w_Children"="Couples with children",
                                "Couple_no_Children"="Couples without children"),
                     limits = c("Single_Individual", "Single_Parent", "Couple_w_Children","Couple_no_Children")) + 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(ft_plot[57:70,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  #geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

ftp_plot<-p1/p2+plot_layout(heights=c(2,1));print(ftp_plot)

agg_png("Exploratory_AvPref_parentpartner.png", width = 1500, height = 1000, units = "px", res = 144)
ftp_plot
dev.off()

#######

## No earner vs Single earner vs Dual earner vs Supplementary earner

# Average preference for no earner HHs
noearn_av<- data_p3 %>%
  filter(earnings_dist == "No earner") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for single earner HHs
singlearn_av<- data_p3 %>%
  filter(earnings_dist == "Single earner") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for supplementary earner HHs
supearn_av<- data_p3 %>%
  filter(earnings_dist == "Supplementary earner") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for double earner HHs
doubearn_av<- data_p3 %>%
  filter(earnings_dist == "Double earner") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

earn_plot<-merge(merge(singlearn_av,supearn_av,by="cntry",all=T),doubearn_av,by="cntry",all=T)
colnames(earn_plot)=c("Country","Single_Earner","Supplementary_Earner","Dual_Earner")
earn_plot<-cbind(earn_plot,noearn_av$mean_gincdif_num)
colnames(earn_plot)=c("Country","Single_Earner","Supplementary_Earner","Dual_Earner","No_Earner")

for (i in 1:nrow(earn_plot)){ # Calculate mean absolute deviation among the 4 groups
  earn_plot$Difference[i]<-madstat(c(earn_plot$Single_Earner[i],earn_plot$Supplementary_Earner[i],
                                     earn_plot$Dual_Earner[i],earn_plot$No_Earner[i]),na.rm=T)
}

earn_plot<-earn_plot[order(earn_plot$Difference),]
earn_plot$Country<-factor(earn_plot$Country,levels=earn_plot$Country[order(earn_plot$Difference)])
earn_plot <- melt(earn_plot, id.vars = "Country")

p1<-ggplot(earn_plot[1:56,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Single_Earner" = "#440154FF", "Supplementary_Earner" = "#21908C", "Dual_Earner"="#93D741FF","No_Earner"="#F7E620FF"),
                     labels = c("Single_Earner"="Single Earner","Supplementary_Earner"="Supplementary Earner",
                                "Dual_Earner"="Dual Earner","No_Earner"="No Earner"),
                     limits = c("No_Earner","Single_Earner","Supplementary_Earner","Dual_Earner")) + # Change the order here 
  scale_shape_manual(values = c("Single_Earner" = 16, "Supplementary_Earner" = 17, "Dual_Earner"=18,"No_Earner"=19),
                     labels = c("Single_Earner"="Single Earner","Supplementary_Earner"="Supplementary Earner",
                                "Dual_Earner"="Dual Earner","No_Earner"="No Earner"),
                     limits = c("No_Earner","Single_Earner","Supplementary_Earner","Dual_Earner")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(earn_plot[57:70,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

earnings_plot<-p1/p2+plot_layout(heights=c(2,1));print(earnings_plot)

agg_png("Exploratory_AvPref_earnings.png", width = 1500, height = 1000, units = "px", res = 144)
earnings_plot
dev.off()

######

## HH income

# Average preference for low HH income
lhhi_av<- data_p3 %>%
  filter(hinctnta_grp == "Low") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for medium HH income
mhhi_av<- data_p3 %>%
  filter(hinctnta_grp == "Middle") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

# Average preference for high HH income
hhhi_av<- data_p3 %>%
  filter(hinctnta_grp == "High") %>%
  group_by(cntry) %>%
  summarize(mean_gincdif_num = mean(gincdif_num, na.rm = TRUE))

hh_plot<-merge(merge(lhhi_av,mhhi_av,by="cntry",all=T),hhhi_av,by="cntry",all=T)
colnames(hh_plot)=c("Country","Low","Middle","High")
for (i in 1:nrow(hh_plot)){ # Calculate mean absolute deviation among the 4 groups
  hh_plot$Difference[i]<-madstat(c(hh_plot$Low[i],hh_plot$Middle[i],hh_plot$High[i]),na.rm=T)
}
hh_plot<-hh_plot[order(hh_plot$Difference),]
hh_plot$Country<-factor(hh_plot$Country,levels=hh_plot$Country[order(hh_plot$Difference)])
hh_plot <- melt(hh_plot, id.vars = "Country")

p1<-ggplot(hh_plot[1:42,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Low" = "#440154FF", "Middle" = "#21908C", "High"="#F7E620FF"),
                     labels = c("Low"="Low HH Income","Middle"="Middle HH Income","High"="High HH Income"),
                     limits = c("Low","Middle","High")) + # Change the order here 
  scale_shape_manual(values = c("Low"=16,"Middle"=17,"High"=18),
                     labels = c("Low"="Low HH Income","Middle"="Middle HH Income","High"="High HH Income"),
                     limits = c("Low","Middle","High")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(hh_plot[43:56,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

hhinc_plot<-p1/p2+plot_layout(heights=c(2,1));print(hhinc_plot)

agg_png("Exploratory_AvPref_hhinc.png", width = 1500, height = 1000, units = "px", res = 144)
hhinc_plot
dev.off()

######

cor_hhinc <- data_p3 %>%
  group_by(cntry) %>%
  summarise(correlation = cor(gincdif_num, hinctnta_num, use = "complete.obs")) %>%
  pull(correlation)

p_hhinc <- data_p3 %>%
  group_by(cntry) %>%
  summarise(p.value = cor.test(gincdif_num, hinctnta_num, use = "complete.obs")$p.value) %>%
  pull(p.value)

hhinc_cor<-data.frame(rbind(cor_hhinc,round(p_hhinc,3)))
colnames(hhinc_cor)<-c(levels(data_p3$cntry))
rownames(hhinc_cor)<-c("Estimate","p.value")

plotdata_hhinc<-data.frame(Country=colnames(hhinc_cor),
                            Estimate=cor_hhinc,
                            p.value=p_hhinc)

plotdata_hhinc$Country <- reorder(plotdata_hhinc$Country, plotdata_hhinc$Estimate)

p1<-ggplot(plotdata_hhinc, aes(x = Country, y = Estimate))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Estimate") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

p2<-ggplot(plotdata_hhinc, aes(x = Country, y = p.value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0.05,linetype="dashed",color="red",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "p-value") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

corhhinc_plot<-p1/p2+plot_layout(heights=c(2,1));print(corhhinc_plot)

agg_png("Exploratory_Cor_hhinc.png", width = 1500, height = 1000, units = "px", res = 144)
corhhinc_plot
dev.off()

##########################################################

## 5.2) Preferences for Work-Family Balance Policies

# Individuals with Family vs Single Individuals:
singleind_av<- data_p3 %>%
  filter(single_family == "Single") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

# Average preference for individuals with family (with children and/or partner)
indwfam_av<- data_p3 %>%
  filter(single_family == "With family") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

si_iwm<-data.frame(Country=as.factor(levels(data_p3$cntry)), Single_Individual=singleind_av$mean_wrkprbf_num,
                   With_Family=indwfam_av$mean_wrkprbf_num,Difference=indwfam_av$mean_wrkprbf_num-singleind_av$mean_wrkprbf_num)
si_iwm<-si_iwm[order(si_iwm$Difference),]
si_iwm$Country<-factor(si_iwm$Country,levels=si_iwm$Country[order(si_iwm$Difference)])
si_iwm<-melt(si_iwm,id.vars="Country")

p1<-ggplot(si_iwm[1:28,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Single_Individual" = "#21908C", "With_Family" = "#FDE725FF"),
                     labels = c("Single_Individual" = "Single Individuals", "With_Family" = "Individuals with Family"),
                     limits = c("With_Family", "Single_Individual")) + # Change the order here 
  scale_shape_manual(values = c("Single_Individual" = 16, "With_Family" = 17),
                     labels = c("Single_Individual" = "Single Individuals", "With_Family" = "Individuals with Family"),
                     limits = c("With_Family", "Single_Individual")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group", title = "") +
  theme_minimal(base_family="Times New Roman")+theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(si_iwm[29:nrow(si_iwm),], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  #scale_fill_viridis_c(option = "viridis")+
  labs(#title="Differences (Individuals with family - Single individuals)",
    x = "Country", y = "Difference") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_SI<-p1/p2+plot_layout(heights=c(2,1));print(cp_SI)

agg_png("Exploratory_AvPref_WithFamily.png", width = 1500, height = 1000, units = "px", res = 144)
cp_SI
dev.off()

##

cor_sf <- data_p3 %>%
  group_by(cntry) %>%
  summarise(correlation = cor(wrkprbf_num, as.numeric(single_family), use = "complete.obs")) %>%
  pull(correlation)

p_sf <- data_p3 %>%
  group_by(cntry) %>%
  summarise(p.value = cor.test(wrkprbf_num, as.numeric(single_family), use = "complete.obs")$p.value) %>%
  pull(p.value)

singlefamily_cor<-data.frame(rbind(cor_sf,round(p_sf,3)))
colnames(singlefamily_cor)<-c(levels(data_p3$cntry))
rownames(singlefamily_cor)<-c("Estimate","p.value")

plotdata_sf<-data.frame(Country=colnames(singlefamily_cor),
                        Estimate=cor_sf,
                        p.value=p_sf)

plotdata_sf$Country <- reorder(plotdata_sf$Country, plotdata_sf$Estimate)

p1<-ggplot(plotdata_sf, aes(x = Country, y = Estimate))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Estimate") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

p2<-ggplot(plotdata_sf, aes(x = Country, y = p.value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0.05,linetype="dashed",color="red",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "p-value") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

corsf_plot<-p1/p2+plot_layout(heights=c(2,1));print(corsf_plot)

agg_png("Exploratory_Cor_WithFamily.png", width = 1500, height = 1000, units = "px", res = 144)
corsf_plot
dev.off()

###########

# Living with a partner vs not living with a partner

# Average preference for individuals living with a partner
lwp_av<- data_p3 %>%
  filter(lwpartner == "yes") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

# Average preference for individuals not living with a partner
nlwp_av<- data_p3 %>%
  filter(lwpartner == "no") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

lwp_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), Alone=nlwp_av$mean_wrkprbf_num,
                     With_Partner=lwp_av$mean_wrkprbf_num,Difference=lwp_av$mean_wrkprbf_num-nlwp_av$mean_wrkprbf_num)
lwp_plot<-lwp_plot[order(lwp_plot$Difference),]
lwp_plot$Country<-factor(lwp_plot$Country,levels=lwp_plot$Country[order(lwp_plot$Difference)])
lwp_plot <- melt(lwp_plot, id.vars = "Country")

p1<-ggplot(lwp_plot[1:28,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Alone" = "#21908C", "With_Partner" = "#FDE725FF"),
                     labels = c("Alone" = "Does not live with partner", "With_Partner" = "Lives with partner"),
                     limits = c("With_Partner", "Alone")) + # Change the order here 
  scale_shape_manual(values = c("Alone" = 16, "With_Partner" = 17),
                     labels = c("Alone" = "Does not live with partner", "With_Partner" = "Lives with partner"),
                     limits = c("With_Partner", "Alone")) + # Change the order here 
  # Customize labels and theme
  labs(#title="Average preference for redistribution",
    x = "", y = "Average Preference", color = "Group", shape = "Group") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(lwp_plot[29:nrow(lwp_plot),], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Difference") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_lwp<-p1/p2+plot_layout(heights=c(2,1));print(cp_lwp)


agg_png("Exploratory_AvPref_LWP.png", width = 1500, height = 1000, units = "px", res = 144)
cp_lwp
dev.off()

##

cor_lwp <- data_p3 %>%
  group_by(cntry) %>%
  summarise(correlation = cor(wrkprbf_num, as.numeric(lwpartner), use = "complete.obs")) %>%
  pull(correlation)

p_lwp <- data_p3 %>%
  group_by(cntry) %>%
  summarise(p.value = cor.test(wrkprbf_num, as.numeric(lwpartner), use = "complete.obs")$p.value) %>%
  pull(p.value)

lwp_cor<-data.frame(rbind(cor_lwp,round(p_lwp,3)))
colnames(lwp_cor)<-c(levels(data_p3$cntry))
rownames(lwp_cor)<-c("Estimate","p.value")

plotdata_lwp<-data.frame(Country=colnames(lwp_cor),
                         Estimate=cor_lwp,
                         p.value=p_lwp)

plotdata_lwp$Country <- reorder(plotdata_lwp$Country, plotdata_lwp$Estimate)

p1<-ggplot(plotdata_lwp, aes(x = Country, y = Estimate))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Estimate") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

p2<-ggplot(plotdata_lwp, aes(x = Country, y = p.value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0.05,linetype="dashed",color="red",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "p-value") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

corlwp_plot<-p1/p2+plot_layout(heights=c(2,1));print(corlwp_plot)

agg_png("Exploratory_Cor_lwp.png", width = 1500, height = 1000, units = "px", res = 144)
corlwp_plot
dev.off()

#################################

## Married vs cohabiting couples

# Average preference for married couples
marriedcohab_av<- data_p3 %>%
  filter(lwpartner=="yes") %>%
  filter(maritalstatus_bin== "Married or in civil union") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

nmarriedcohab_av<- data_p3 %>%
  filter(lwpartner=="yes") %>%
  filter(maritalstatus_bin== "Not married or in civil union") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

marcoh_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), Not_Married=nmarriedcohab_av$mean_wrkprbf_num,
                        Married=marriedcohab_av$mean_wrkprbf_num)
marcoh_plot$Difference<-marcoh_plot$Married-marcoh_plot$Not_Married
marcoh_plot<-marcoh_plot[order(marcoh_plot$Difference),]
marcoh_plot$Country<-factor(marcoh_plot$Country,levels=marcoh_plot$Country[order(marcoh_plot$Difference)])
marcoh_plot <- melt(marcoh_plot, id.vars = "Country")

p1<-ggplot(marcoh_plot[1:28,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Not_Married" = "#21908C", "Married" = "#FDE725FF"),
                     labels = c("Married" = "Married", "Not_Married" = "Not Married"),
                     limits = c("Married","Not_Married")) + # Change the order here 
  scale_shape_manual(values = c("Married" = 16, "Not_Married" = 17),
                     labels = c("Married" = "Married", "Not_Married" = "Not Married"),
                     limits = c("Married","Not_Married")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group", title = "") +
  theme_minimal(base_family="Times New Roman")+theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(marcoh_plot[29:nrow(marcoh_plot),], aes(x = Country, y = value))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (Individuals with family - Single individuals)",
    x = "Country", y = "Difference") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_marcoh<-p1/p2+plot_layout(heights=c(2,1));print(cp_marcoh)

agg_png("Exploratory_AvPref_married.png", width = 1500, height = 1000, units = "px", res = 144)
cp_marcoh
dev.off()

##

cor_married <- data_p3 %>%
  group_by(cntry) %>%
  summarise(correlation = cor(wrkprbf_num, as.numeric(maritalstatus_bin), use = "complete.obs")) %>%
  pull(correlation)

p_married <- data_p3 %>%
  group_by(cntry) %>%
  summarise(p.value = cor.test(wrkprbf_num, as.numeric(maritalstatus_bin), use = "complete.obs")$p.value) %>%
  pull(p.value)

married_cor<-data.frame(rbind(cor_married,round(p_married,3)))
colnames(married_cor)<-c(levels(data_p3$cntry))
rownames(married_cor)<-c("Estimate","p.value")

plotdata_married<-data.frame(Country=colnames(married_cor),
                             Estimate=cor_married,
                             p.value=p_married)

plotdata_married$Country <- reorder(plotdata_married$Country, plotdata_married$Estimate)

p1<-ggplot(plotdata_married, aes(x = Country, y = Estimate))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Estimate") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

p2<-ggplot(plotdata_married, aes(x = Country, y = p.value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0.05,linetype="dashed",color="red",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "p-value") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cormarried_plot<-p1/p2+plot_layout(heights=c(2,1));print(cormarried_plot)

agg_png("Exploratory_Cor_married.png", width = 1500, height = 1000, units = "px", res = 144)
cormarried_plot
dev.off()

#########

# Number of children

# Average preference for individuals without children
nochild_av<- data_p3 %>%
  filter(nmchildren_grp2 == "no children") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

# Average preference for individuals with 1-2 children
child12_av<- data_p3 %>%
  filter(nmchildren_grp2 == "1-2 children") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

# Average preference for individuals with 3 or more children
child3_av<- data_p3 %>%
  filter(nmchildren_grp2 == "3 children or more") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

nchild_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), no_child=nochild_av$mean_wrkprbf_num,
                        one_or_two=child12_av$mean_wrkprbf_num,three_or_more=child3_av$mean_wrkprbf_num)
for (i in 1:nrow(nchild_plot)){
  nchild_plot$Difference[i]<-madstat(c(nchild_plot$no_child[i],nchild_plot$one_or_two[i],nchild_plot$three_or_more[i]))
}
nchild_plot<-nchild_plot[order(nchild_plot$Difference),]
nchild_plot$Country<-factor(nchild_plot$Country,levels=nchild_plot$Country[order(nchild_plot$Difference)])
nchild_plot <- melt(nchild_plot, id.vars = "Country")

p1<-ggplot(nchild_plot[1:42,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("no_child" = "#440154FF", "one_or_two" = "#21908C", "three_or_more"="#FDE725FF"),
                     labels = c("no_child" = "0 children", "one_or_two"="1-2 children","three_or_more"="3 children or more"),
                     limits = c("no_child","one_or_two","three_or_more")) + # Change the order here 
  scale_shape_manual(values = c("no_child" = 16, "one_or_two" = 17, "three_or_more"=18),
                     labels = c("no_child" = "0 children", "one_or_two"="1-2 children","three_or_more"="3 children or more"),
                     limits = c("no_child","one_or_two","three_or_more")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(nchild_plot[43:56,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  #geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cp_child<-p1/p2+plot_layout(heights=c(2,1));print(cp_child)

agg_png("Exploratory_AvPref_nchild.png", width = 1500, height = 1000, units = "px", res = 144)
cp_child
dev.off()

##

cor_nchild <- data_p3 %>%
  group_by(cntry) %>%
  summarise(correlation = cor(wrkprbf_num, nchildren, use = "complete.obs")) %>%
  pull(correlation)

p_nchild <- data_p3 %>%
  group_by(cntry) %>%
  summarise(p.value = cor.test(wrkprbf_num, nchildren, use = "complete.obs")$p.value) %>%
  pull(p.value)

nchild_cor<-data.frame(rbind(cor_nchild,round(p_nchild,3)))
colnames(nchild_cor)<-c(levels(data_p3$cntry))
rownames(nchild_cor)<-c("Estimate","p.value")

plotdata_nchild<-data.frame(Country=colnames(nchild_cor),
                            Estimate=cor_nchild,
                            p.value=p_nchild)

plotdata_nchild$Country <- reorder(plotdata_nchild$Country, plotdata_nchild$Estimate)

p1<-ggplot(plotdata_nchild, aes(x = Country, y = Estimate))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Estimate") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

p2<-ggplot(plotdata_nchild, aes(x = Country, y = p.value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0.05,linetype="dashed",color="red",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "p-value") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

cornchild_plot<-p1/p2+plot_layout(heights=c(2,1));print(cornchild_plot)

agg_png("Exploratory_Cor_nchild.png", width = 1500, height = 1000, units = "px", res = 144)
cornchild_plot
dev.off()

############

# Combinations of partnership and parenthood

# Average preference for single parents
sp_av<- data_p3 %>%
  filter(parentpartner == "Single parent") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

# Average preference for couples with children
cwc_av<- data_p3 %>%
  filter(parentpartner == "Couple parents") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

# Average preference for couples without children
cnc_av<- data_p3 %>%
  filter(parentpartner == "Couple no children") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

ft_plot<-data.frame(Country=as.factor(levels(data_p3$cntry)), Single_Individual=singleind_av$mean_wrkprbf_num,
                    Single_Parent=sp_av$mean_wrkprbf_num,Couple_w_Children=cwc_av$mean_wrkprbf_num,Couple_no_Children=cnc_av$mean_wrkprbf_num)

for (i in 1:nrow(ft_plot)){ # Calculate mean absolute deviation among the 4 groups
  ft_plot$Difference[i]<-madstat(c(ft_plot$Single_Individual[i],ft_plot$Single_Parent[i],
                                   ft_plot$Couple_w_Children[i],ft_plot$Couple_no_Children[i]))
}
ft_plot<-ft_plot[order(ft_plot$Difference),]
ft_plot$Country<-factor(ft_plot$Country,levels=ft_plot$Country[order(ft_plot$Difference)])
ft_plot <- melt(ft_plot, id.vars = "Country")

p1<-ggplot(ft_plot[1:56,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Single_Individual" = "#440154FF", "Single_Parent" = "#21908C", "Couple_w_Children"="#93D741FF",
                                "Couple_no_Children"="#F7E620FF"),
                     labels = c("Single_Individual" = "Single individuals", "Single_Parent" = "Single parents", 
                                "Couple_w_Children"="Couples with children",
                                "Couple_no_Children"="Couples without children"),
                     limits = c("Single_Individual", "Single_Parent", "Couple_w_Children","Couple_no_Children")) + 
  scale_shape_manual(values = c("Single_Individual" = 16, "Single_Parent" = 17, "Couple_w_Children"=15,
                                "Couple_no_Children"=18),
                     labels = c("Single_Individual" = "Single individuals", "Single_Parent" = "Single parents", 
                                "Couple_w_Children"="Couples with children",
                                "Couple_no_Children"="Couples without children"),
                     limits = c("Single_Individual", "Single_Parent", "Couple_w_Children","Couple_no_Children")) + 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(ft_plot[57:70,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  #geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

ftp_plot<-p1/p2+plot_layout(heights=c(2,1));print(ftp_plot)

agg_png("Exploratory_AvPref_parentpartner.png", width = 1500, height = 1000, units = "px", res = 144)
ftp_plot
dev.off()

#######

## No earner vs Single earner vs Dual earner vs Supplementary earner

# Average preference for no earner HHs
noearn_av<- data_p3 %>%
  filter(earnings_dist == "No earner") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

# Average preference for single earner HHs
singlearn_av<- data_p3 %>%
  filter(earnings_dist == "Single earner") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

# Average preference for supplementary earner HHs
supearn_av<- data_p3 %>%
  filter(earnings_dist == "Supplementary earner") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

# Average preference for double earner HHs
doubearn_av<- data_p3 %>%
  filter(earnings_dist == "Double earner") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

earn_plot<-merge(merge(singlearn_av,supearn_av,by="cntry",all=T),doubearn_av,by="cntry",all=T)
colnames(earn_plot)=c("Country","Single_Earner","Supplementary_Earner","Dual_Earner")
earn_plot<-cbind(earn_plot,noearn_av$mean_wrkprbf_num)
colnames(earn_plot)=c("Country","Single_Earner","Supplementary_Earner","Dual_Earner","No_Earner")

for (i in 1:nrow(earn_plot)){ # Calculate mean absolute deviation among the 4 groups
  earn_plot$Difference[i]<-madstat(c(earn_plot$Single_Earner[i],earn_plot$Supplementary_Earner[i],
                                     earn_plot$Dual_Earner[i],earn_plot$No_Earner[i]),na.rm=T)
}

earn_plot<-earn_plot[order(earn_plot$Difference),]
earn_plot$Country<-factor(earn_plot$Country,levels=earn_plot$Country[order(earn_plot$Difference)])
earn_plot <- melt(earn_plot, id.vars = "Country")

p1<-ggplot(earn_plot[1:56,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Single_Earner" = "#440154FF", "Supplementary_Earner" = "#21908C", "Dual_Earner"="#93D741FF","No_Earner"="#F7E620FF"),
                     labels = c("Single_Earner"="Single Earner","Supplementary_Earner"="Supplementary Earner",
                                "Dual_Earner"="Dual Earner","No_Earner"="No Earner"),
                     limits = c("No_Earner","Single_Earner","Supplementary_Earner","Dual_Earner")) + # Change the order here 
  scale_shape_manual(values = c("Single_Earner" = 16, "Supplementary_Earner" = 17, "Dual_Earner"=18,"No_Earner"=19),
                     labels = c("Single_Earner"="Single Earner","Supplementary_Earner"="Supplementary Earner",
                                "Dual_Earner"="Dual Earner","No_Earner"="No Earner"),
                     limits = c("No_Earner","Single_Earner","Supplementary_Earner","Dual_Earner")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(earn_plot[57:70,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

earnings_plot<-p1/p2+plot_layout(heights=c(2,1));print(earnings_plot)

agg_png("Exploratory_AvPref_earnings.png", width = 1500, height = 1000, units = "px", res = 144)
earnings_plot
dev.off()

######

## HH income

# Average preference for low HH income
lhhi_av<- data_p3 %>%
  filter(hinctnta_grp == "Low") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

# Average preference for medium HH income
mhhi_av<- data_p3 %>%
  filter(hinctnta_grp == "Middle") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

# Average preference for high HH income
hhhi_av<- data_p3 %>%
  filter(hinctnta_grp == "High") %>%
  group_by(cntry) %>%
  summarize(mean_wrkprbf_num = mean(wrkprbf_num, na.rm = TRUE))

hh_plot<-merge(merge(lhhi_av,mhhi_av,by="cntry",all=T),hhhi_av,by="cntry",all=T)
colnames(hh_plot)=c("Country","Low","Middle","High")
for (i in 1:nrow(hh_plot)){ # Calculate mean absolute deviation among the 4 groups
  hh_plot$Difference[i]<-madstat(c(hh_plot$Low[i],hh_plot$Middle[i],hh_plot$High[i]),na.rm=T)
}
hh_plot<-hh_plot[order(hh_plot$Difference),]
hh_plot$Country<-factor(hh_plot$Country,levels=hh_plot$Country[order(hh_plot$Difference)])
hh_plot <- melt(hh_plot, id.vars = "Country")

p1<-ggplot(hh_plot[1:42,], aes(x = Country, y = value, color = variable, shape = variable)) +
  # Plot points with appropriate shapes
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5) +
  # Set custom color and shape scales with updated labels
  scale_color_manual(values = c("Low" = "#440154FF", "Middle" = "#21908C", "High"="#F7E620FF"),
                     labels = c("Low"="Low HH Income","Middle"="Middle HH Income","High"="High HH Income"),
                     limits = c("Low","Middle","High")) + # Change the order here 
  scale_shape_manual(values = c("Low"=16,"Middle"=17,"High"=18),
                     labels = c("Low"="Low HH Income","Middle"="Middle HH Income","High"="High HH Income"),
                     limits = c("Low","Middle","High")) + # Change the order here 
  # Customize labels and theme
  labs(x = "", y = "Average Preference", color = "Group", shape = "Group") +
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.text=element_text(size=12),
    legend.title = element_blank()
  ) 

# Plot also the differences of means
p2<-ggplot(hh_plot[43:56,], aes(x = Country, y = value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = variable), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Mean-absolute deviation") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

hhinc_plot<-p1/p2+plot_layout(heights=c(2,1));print(hhinc_plot)

agg_png("Exploratory_AvPref_hhinc.png", width = 1500, height = 1000, units = "px", res = 144)
hhinc_plot
dev.off()

######

cor_hhinc <- data_p3 %>%
  group_by(cntry) %>%
  summarise(correlation = cor(wrkprbf_num, hinctnta_num, use = "complete.obs")) %>%
  pull(correlation)

p_hhinc <- data_p3 %>%
  group_by(cntry) %>%
  summarise(p.value = cor.test(wrkprbf_num, hinctnta_num, use = "complete.obs")$p.value) %>%
  pull(p.value)

hhinc_cor<-data.frame(rbind(cor_hhinc,round(p_hhinc,3)))
colnames(hhinc_cor)<-c(levels(data_p3$cntry))
rownames(hhinc_cor)<-c("Estimate","p.value")

plotdata_hhinc<-data.frame(Country=colnames(hhinc_cor),
                           Estimate=cor_hhinc,
                           p.value=p_hhinc)

plotdata_hhinc$Country <- reorder(plotdata_hhinc$Country, plotdata_hhinc$Estimate)

p1<-ggplot(plotdata_hhinc, aes(x = Country, y = Estimate))+
  geom_point(size = 3) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0,linetype="dashed",color="black",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "Estimate") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

p2<-ggplot(plotdata_hhinc, aes(x = Country, y = p.value))+
  geom_point(size = 3,shape=4) +
  # Add lines connecting points of the same group
  geom_line(aes(group = 1), linewidth = 0.5)+
  geom_hline(yintercept=0.05,linetype="dashed",color="red",linewidth=0.25)+
  labs(#title="Differences (lives with partner - does not live with partner)",
    x = "Country", y = "p-value") +
  #theme_minimal()+
  theme_minimal(base_family="Times New Roman")+
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) 

corhhinc_plot<-p1/p2+plot_layout(heights=c(2,1));print(corhhinc_plot)

agg_png("Exploratory_Cor_hhinc.png", width = 1500, height = 1000, units = "px", res = 144)
corhhinc_plot
dev.off()


