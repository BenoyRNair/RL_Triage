load("df_minimal_01.RData")
# 555,442 obs of 577 variables

df_minimal_01 <- subset(df_minimal_01, select=-c(row_id))
# 555,442 obs of 576 variables

table (df_minimal_01$esi)
#     1      2      3      4      5 
#  5242 162803 235114 124435  27848

# ESI 1 & ESI 2 --> CESI 1
# CESI 1: 5242 + 162803 = 168045
df_minimal_01_esi_1_esi_2 <- subset (df_minimal_01, esi == 1 | esi == 2)
# 168,045 of 576 variables
colnames(df_minimal_01_esi_1_esi_2)[which(names(df_minimal_01_esi_1_esi_2) == "esi")] <- "cesi"
df_minimal_01_esi_1_esi_2$cesi <- 1

# ESI 3 --> CESI 2
# CESI 2: 235114
df_minimal_01_esi_3 <- subset (df_minimal_01, esi == 3)
# 235,114 obs of 576 variables
colnames(df_minimal_01_esi_3)[which(names(df_minimal_01_esi_3) == "esi")] <- "cesi"
df_minimal_01_esi_3$cesi <- 2

# ESI 4 & ESI 5 --> CESI 3
# CESI 3: 124435 + 27848 = 152283
df_minimal_01_esi_4_esi_5 <- subset (df_minimal_01, esi == 4 | esi == 5)
# 152,283 obs of 576 variables
colnames(df_minimal_01_esi_4_esi_5)[which(names(df_minimal_01_esi_4_esi_5) == "esi")] <- "cesi"
df_minimal_01_esi_4_esi_5$cesi <- 3

library(dplyr)

# Considering the number of records available for CESI 1, 2 & 3, Combined CESI dataset will have 450K records (150K of each CESI level)
df_minimal_01_esi_1_esi_2_sample_150k <- sample_n(df_minimal_01_esi_1_esi_2, 150000)
# 150,000 obs of 576 variables

df_minimal_01_esi_3_sample_150k <- sample_n (df_minimal_01_esi_3, 150000)
# 150,000 obs of 576 variables

df_minimal_01_esi_4_esi_5_sample_150k <- sample_n (df_minimal_01_esi_4_esi_5, 150000)
# 150,000 obs of 576 variables

# Merge
df_minimal_01_cesi_merged_temp <- rbind(df_minimal_01_esi_1_esi_2_sample_150k, df_minimal_01_esi_3_sample_150k, df_minimal_01_esi_4_esi_5_sample_150k)
# 450,000 obs of 576 variables

table(df_minimal_01_cesi_merged_temp$cesi)
#      1      2      3
# 150000 150000 150000

# Shuffle and finalize
df_minimal_01_cesi_merged  = data.frame(df_minimal_01_cesi_merged_temp [sample(1:nrow(df_minimal_01_cesi_merged_temp )), ])
# 450,000 obs of 576 variables

rownames(df_minimal_01_cesi_merged) <- NULL

#Add row_id column
row_id <- 1:nrow(df_minimal_01_cesi_merged)
df_minimal_01_cesi_merged <- cbind(row_id, df_minimal_01_cesi_merged)
# 450,000 of 577 variables

save(df_minimal_01_cesi_merged,file="df_minimal_01_cesi_merged.RData")
write.csv(df_minimal_01_cesi_merged,file="df_minimal_01_cesi_merged.csv")

# Split: Train & Test
set.seed(1)

#Use 80% of dataset as training set and remaining 20% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df_minimal_01_cesi_merged), replace=TRUE, prob=c(0.8,0.2))
df_minimal_01_train_cesi_merged <- df_minimal_01_cesi_merged[sample, ]
df_minimal_01_test_cesi_merged <- df_minimal_01_cesi_merged[!sample, ]

#write csv
write.csv(df_minimal_01_train_cesi_merged, 'df_minimal_01_train_cesi_merged.csv',row.names=FALSE)
write.csv(df_minimal_01_test_cesi_merged, 'df_minimal_01_test_cesi_merged.csv',row.names=FALSE)