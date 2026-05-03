# =====================================================
# PROBLEM SET 4 (USING patients.csv ONLY)
# =====================================================

# =====================================================
# 1. LOAD DATASET
# =====================================================

df <- read.csv("patients.csv", stringsAsFactors = FALSE)

names(df) <- trimws(names(df))

cat("ORIGINAL DATA:\n")
print(head(df))

# =====================================================
# 2. CHECK MISSING VALUES
# =====================================================

cat("\nMISSING VALUES:\n")
print(colSums(is.na(df)))

cat("\nINTERPRETATION:\n")
cat("Missing values exist in Age, Sex, Blood Type, and BP.\n")
cat("These may result from incomplete patient records.\n")

# =====================================================
# 3. IMPUTATION
# =====================================================

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# ---- AGE (mean) ----
df$Age <- as.numeric(df$Age)
mean_age <- mean(df$Age, na.rm = TRUE)
df$Age[is.na(df$Age)] <- mean_age

# ---- SEX (mode) ----
mode_sex <- getmode(na.omit(df$Sex))
df$Sex[is.na(df$Sex) | df$Sex == ""] <- mode_sex

# ---- BLOOD TYPE (mode) ----
mode_blood <- getmode(na.omit(df$Blood.Type))
df$Blood.Type[is.na(df$Blood.Type) | df$Blood.Type == ""] <- mode_blood

# ---- SYSTOLIC BP (mean) ----
df$Systolic.BP..mm.Hg. <- as.numeric(df$Systolic.BP..mm.Hg.)
mean_bp <- mean(df$Systolic.BP..mm.Hg., na.rm = TRUE)
df$Systolic.BP..mm.Hg.[is.na(df$Systolic.BP..mm.Hg.)] <- mean_bp

cat("\nIMPUTATION COMPLETE\n")

# =====================================================
# 4. SUMMARY
# =====================================================

summary(df)

cat("\nINTERPRETATION:\n")
cat("Mean imputation was used for numeric variables (Age, BP).\n")
cat("Mode imputation was used for categorical variables (Sex, Blood Type).\n")

# =====================================================
# 5. VISUALIZATION
# =====================================================

hist(df$Age, main="Age Distribution", xlab="Age")
hist(df$Systolic.BP..mm.Hg., main="Blood Pressure", xlab="BP")

boxplot(df$Age, main="Age Boxplot")
boxplot(df$Systolic.BP..mm.Hg., main="BP Boxplot")

# =====================================================
# 6. SYNTHETIC DATA
# =====================================================

set.seed(123)

synthetic_data <- data.frame(
  Age = rnorm(100, mean(df$Age), sd(df$Age)),
  SystolicBP = rnorm(100, mean(df$Systolic.BP..mm.Hg.), sd(df$Systolic.BP..mm.Hg.)),
  Sex = sample(unique(df$Sex), 100, replace = TRUE),
  BloodType = sample(unique(df$Blood.Type), 100, replace = TRUE)
)

cat("\nSYNTHETIC DATA GENERATED\n")

# =====================================================
# 7. REGRESSION ANALYSIS
# =====================================================

synthetic_data$Sex <- as.factor(synthetic_data$Sex)
synthetic_data$BloodType <- as.factor(synthetic_data$BloodType)

model <- lm(SystolicBP ~ Age + Sex + BloodType, data = synthetic_data)

summary(model)

cat("\nINTERPRETATION:\n")
cat("Age, Sex, and Blood Type are used to predict systolic blood pressure.\n")
cat("The model evaluates how demographic and biological factors influence BP.\n")

# =====================================================
# END
# =====================================================
