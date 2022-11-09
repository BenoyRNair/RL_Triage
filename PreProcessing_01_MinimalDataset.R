load("5v_cleandf.rdata")

sum(is.na(df$esi))
# [1] 2457

df_01_non_null_esi <- df[!(is.na(df$esi) | df$esi==""),]
# df_01_non_null_esi : 558029 obs. of 972 variables

sum(is.na(df_01_non_null_esi$esi))
# [1] 0

str(df_01_non_null_esi$esi)
# Factor w/ 5 levels "1","2","3","4",..: 4 4 2 2 3 3 3 4 2 2 ...
# esi appears to be factor (and not integer)
# needs to be coverted to numeric, as we are going to drop all the non-numeric features (which are all in Demographics section)
df_01_non_null_esi$esi <- as.numeric(as.character(df_01_non_null_esi$esi))

str(df_01_non_null_esi$esi)
# num [1:558029] 4 4 2 2 3 3 3 4 2 2 ...

sum(is.na(df_01_non_null_esi$cc_abdominalcramping))
# [1] 2576

df_02_non_null_cc <- df_01_non_null_esi[!(is.na(df_01_non_null_esi$cc_abdominalcramping) | df_01_non_null_esi$cc_abdominalcramping==""),]
# df_02_non_null_cc : 555453 obs. of 972 variables

sum(is.na(df_02_non_null_cc$cc_abdominalcramping))
# [1] 0

# dropping all non-numeric/ demographics text attributes
df_03_only_numeric <- df_02_non_null_cc[, sapply(df_02_non_null_cc, is.numeric)]
# df_03_only_numeric   : 555453 obs. of 957 variables

sum(is.na(df_03_only_numeric$age))
# [1] 11

df_04_non_null_age <- df_03_only_numeric[!(is.na(df_03_only_numeric$age) | df_03_only_numeric$age==""),]
# df_04_non_null_age : 555442 obs. of 957 variables

sum(is.na( df_04_non_null_age$age))
# [1] 0

# Applying median to NA Triage Variables
df_05_median_to_NA_triage_variables_df_04 <- data.frame(df_04_non_null_age)
# df_05_median_to_NA_triage_variables_df_04 : 555442 obs. of 957 variables

df_05_median_to_NA_triage_variables_df_04$triage_vital_hr[is.na(df_05_median_to_NA_triage_variables_df_04$triage_vital_hr)] <- median(df_05_median_to_NA_triage_variables_df_04$triage_vital_hr, na.rm = T)
df_05_median_to_NA_triage_variables_df_04$triage_vital_sbp[is.na(df_05_median_to_NA_triage_variables_df_04$triage_vital_sbp)] <- median(df_05_median_to_NA_triage_variables_df_04$triage_vital_sbp, na.rm = T)
df_05_median_to_NA_triage_variables_df_04$triage_vital_dbp[is.na(df_05_median_to_NA_triage_variables_df_04$triage_vital_dbp)] <- median(df_05_median_to_NA_triage_variables_df_04$triage_vital_dbp, na.rm = T)
df_05_median_to_NA_triage_variables_df_04$triage_vital_rr[is.na(df_05_median_to_NA_triage_variables_df_04$triage_vital_rr)] <- median(df_05_median_to_NA_triage_variables_df_04$triage_vital_rr, na.rm = T)
df_05_median_to_NA_triage_variables_df_04$triage_vital_temp[is.na(df_05_median_to_NA_triage_variables_df_04$triage_vital_temp)] <- median(df_05_median_to_NA_triage_variables_df_04$triage_vital_temp, na.rm = T)
df_05_median_to_NA_triage_variables_df_04$triage_vital_o2_device[is.na(df_05_median_to_NA_triage_variables_df_04$triage_vital_o2_device)] <- median(df_05_median_to_NA_triage_variables_df_04$triage_vital_o2_device, na.rm = T)
df_05_median_to_NA_triage_variables_df_04$triage_vital_o2[is.na(df_05_median_to_NA_triage_variables_df_04$triage_vital_o2)] <- median(df_05_median_to_NA_triage_variables_df_04$triage_vital_o2, na.rm = T)

install.packages('arsenal')
library('arsenal')

# Quick comparison
sink("summary_comparedf_df_04_and_df_05_median_to_NA_triage_columns.txt")
summary(comparedf(df_04_non_null_age, df_05_median_to_NA_triage_variables_df_04))
# Refer to "summary_comparedf_df_04_and_df_05_median_to_NA_triage_columns.txt"
sink()

tmp_03_cols_nulls_count_df_05<-colSums(is.na(df_05_median_to_NA_triage_variables_df_04))
write.csv(tmp_03_cols_nulls_count_df_05,"tmp_03_cols_nulls_count_df_05.csv")
# "REFER TO tmp_03_cols_nulls_count_df_05.csv"
# print(tmp_03_cols_nulls_count_df_05)

df_06_remove_NA_columns <- df_05_median_to_NA_triage_variables_df_04[, colSums(is.na(df_05_median_to_NA_triage_variables_df_04))==0]
# df_06_remove_NA_columns: 555442 obs. of 577 variables

# Quick correlation check
cor_stats_01 <- cor(df_06_remove_NA_columns)
write.csv(cor_stats_01, "cor_stats_01.csv")

# ecodesmachinary still NA in above cor_stats_01.csv file!!!
df_08_drop_ecodesmachinery <- subset(df_06_remove_NA_columns, select=-c(ecodesmachinery))
# df_08_drop_ecodesmachinery : 555442 obs. of 576 variables

cor_stats_02 <- cor(df_08_drop_ecodesmachinery)
write.csv(cor_stats_02  ,"CorrelationAnalysis.csv")

df_minimal_01 <- data.frame(df_08_drop_ecodesmachinery)
#add row id
row_id <- 1:nrow(df_minimal_01)
df_minimal_01 <- cbind(row_id, df_minimal_01)

save(df_minimal_01,file="df_minimal_01.RData")
write.csv(df_minimal_01,file="df_minimal_01.csv")

# Split: Train & Test
set.seed(1)

#Use 80% of dataset as training set and remaining 20% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df_minimal_01), replace=TRUE, prob=c(0.8,0.2))
df_minimal_01_train  <- df_minimal_01[sample, ]
df_minimal_01_test   <- df_minimal_01[!sample, ]

