# Load required packages
library(shiny)
library(dplyr)
library(readxl)
library(EGAnet)
library(igraph)
library(qgraph)
library(DT)
library(haven)
library(ggplot2)
library(jtools)
library(imputeTS)
library(shadowtext)

# Define file paths
data_GSS <- "C:/Users/Pei-Chin/Dropbox (1)/shinyapp/recoded_GSS_data.sav"
data_dir <- "C:/Users/Pei-Chin/Documents/GitHub/World-Mobility/data"
output_dir <- "C:/Users/Pei-Chin/Documents/GitHub/World-Mobility/output"

# Load data
gss_data <- read_sav(data_GSS)
labels_data <- read_excel(file.path(data_dir, "Revised_info_modified0708 - youngjae suggestions_modified.xlsx"))

# Prepare data for GDP and Immigration
usImmigrationData <- read.csv(file.path(data_dir, "USImmigration.csv"))
usGdpData <- read.csv(file.path(data_dir, "FREDGDP.csv"))
usGdpData$DATE <- as.Date(usGdpData$DATE, format = "%Y-%m-%d")
usGdpData$Year <- as.integer(format(usGdpData$DATE, "%Y"))
usGdpData <- usGdpData %>%
  group_by(Year) %>%
  summarise(GDP = mean(GDP, na.rm = TRUE))

# Load and merge mobility data
mobility_data <- read.csv(file.path(data_dir, "GSS level 2e.csv"))
mobility_data <- mobility_data[, c("year", "Mobility", "Mobilitystate")] 
colnames(gss_data)[1] <- "year" 
gss_data <- merge(mobility_data, gss_data, by = "year", all.x = TRUE)
colnames(usGdpData)[1] <- "year"
colnames(usImmigrationData)[1] <- "year"

# Merge and process gss_data with mobility, GDP, and immigration data
gss_data <- gss_data %>%
  arrange(year) %>%
  mutate(
    FAIR = na_interpolation(FAIR, option = "spline"),
    TRUST = na_interpolation(TRUST, option = "spline"),
    HAPPY = na_interpolation(HAPPY, option = "spline"),
    HELPFUL = na_interpolation(HELPFUL, option = "spline"),
    Mobility = na_interpolation(Mobility, option = "spline"),
    Mobilitystate = na_interpolation(Mobilitystate, option = "spline"),
    AGED = na_interpolation(AGED, option = "spline"),
    ATTEND = na_interpolation(ATTEND, option = "spline"),
    CONARMY = na_interpolation(CONARMY, option = "spline"),
    CONBUS = na_interpolation(CONBUS, option = "spline"),
    CONCLERG = na_interpolation(CONCLERG, option = "spline"),
    CONEDUC = na_interpolation(CONEDUC, option = "spline"),
    CONFED = na_interpolation(CONFED, option = "spline"),
    CONFINAN = na_interpolation(CONFINAN, option = "spline"),
    CONJUDGE = na_interpolation(CONJUDGE, option = "spline"),
    CONLABOR = na_interpolation(CONLABOR, option = "spline"),
    CONLEGIS = na_interpolation(CONLEGIS, option = "spline"),
    CONMEDIC = na_interpolation(CONMEDIC, option = "spline"),
    CONPRESS = na_interpolation(CONPRESS, option = "spline"),
    CONSCI = na_interpolation(CONSCI, option = "spline"),
    CONTV = na_interpolation(CONTV, option = "spline"),
    COURTS = na_interpolation(COURTS, option = "spline"),
    DIVLAW = na_interpolation(DIVLAW, option = "spline"),
    FINALTER = na_interpolation(FINALTER, option = "spline"),
    FINRELA = na_interpolation(FINRELA, option = "spline"),
    GETAHEAD = na_interpolation(GETAHEAD, option = "spline"),
    HAPMAR = na_interpolation(HAPMAR, option = "spline"),
    HEALTH = na_interpolation(HEALTH, option = "spline"),
    HOMOSEX = na_interpolation(HOMOSEX, option = "spline"),
    LIFE = na_interpolation(LIFE, option = "spline"),
    NATAID = na_interpolation(NATAID, option = "spline"),
    NATARMS = na_interpolation(NATARMS, option = "spline"),
    NATCITY = na_interpolation(NATCITY, option = "spline"),
    NATCRIME = na_interpolation(NATCRIME, option = "spline"),
    NATDRUG = na_interpolation(NATDRUG, option = "spline"),
    NATEDUC = na_interpolation(NATEDUC, option = "spline"),
    NATENVIR = na_interpolation(NATENVIR, option = "spline"),
    NATFARE = na_interpolation(NATFARE, option = "spline"),
    NATHEAL = na_interpolation(NATHEAL, option = "spline"),
    NATRACE = na_interpolation(NATRACE, option = "spline"),
    NEWS = na_interpolation(NEWS, option = "spline"),
    PORNLAW = na_interpolation(PORNLAW, option = "spline"),
    PREMARSX = na_interpolation(PREMARSX, option = "spline"),
    RELITEN = na_interpolation(RELITEN, option = "spline"),
    SATJOB = na_interpolation(SATJOB, option = "spline"),
    XMARSEX = na_interpolation(XMARSEX, option = "spline")
  ) %>%
  mutate(
    FAIRLAG = lag(FAIR),
    TRUSTLAG = lag(TRUST),
    HAPPYLAG = lag(HAPPY),
    HELPFULLAG = lag(HELPFUL),
    MobilityLAG = lag(Mobility),
    MobilitystateLAG = lag(Mobilitystate),
    AGEDLAG = lag(AGED),
    ATTENDLAG = lag(ATTEND),
    CONARMYLAG = lag(CONARMY),
    CONBUSLAG = lag(CONBUS),
    CONCLERGLAG = lag(CONCLERG),
    CONEDUCLAG = lag(CONEDUC),
    CONFEDLAG = lag(CONFED),
    CONFINANLAG = lag(CONFINAN),
    CONJUDGELAG = lag(CONJUDGE),
    CONLABORLAG = lag(CONLABOR),
    CONLEGISLAG = lag(CONLEGIS),
    CONMEDICLAG = lag(CONMEDIC),
    CONPRESSLAG = lag(CONPRESS),
    CONSCILAG = lag(CONSCI),
    CONTVLAG = lag(CONTV),
    COURTSLAG = lag(COURTS),
    DIVLAWLAG = lag(DIVLAW),
    FINALTERLAG = lag(FINALTER),
    FINRELALAG = lag(FINRELA),
    GETAHEADLAG = lag(GETAHEAD),
    HAPMARLAG = lag(HAPMAR),
    HEALTHLAG = lag(HEALTH),
    HOMOSEXLAG = lag(HOMOSEX),
    LIFELAG = lag(LIFE),
    NATAIDLAG = lag(NATAID),
    NATARMSLAG = lag(NATARMS),
    NATCITYLAG = lag(NATCITY),
    NATCRIMELAG = lag(NATCRIME),
    NATDRUGLAG = lag(NATDRUG),
    NATEDUCLAG = lag(NATEDUC),
    NATENVIRLAG = lag(NATENVIR),
    NATFARELAG = lag(NATFARE),
    NATHEALLAG = lag(NATHEAL),
    NATRACELAG = lag(NATRACE),
    NEWSLAG = lag(NEWS),
    PORNLAWLAG = lag(PORNLAW),
    PREMARSXLAG = lag(PREMARSX),
    RELITENLAG = lag(RELITEN),
    SATJOBLAG = lag(SATJOB),
    XMARSEXLAG = lag(XMARSEX)
  )

# Basic Model Analysis
basic_model <- lm(HAPPY ~ MobilityLAG + HAPPYLAG, data = gss_data)
print(summary(basic_model))
print(confint(basic_model))

# Calculate partial R² for MobilityLAG
partial_r2 <- (sqrt(summary(basic_model)$coefficients["MobilityLAG", "t value"]^2 / 
                      (summary(basic_model)$coefficients["MobilityLAG", "t value"]^2 + basic_model$df.residual)))^2
print(paste("Partial R² for MobilityLAG:", partial_r2))

# List of target variables
target_column_names <- c(
  "NATENVIR", "NATHEAL", "NATCITY", "NATCRIME", "NATDRUG", "NATEDUC", 
  "NATRACE", "NATARMS", "NATAID", "NATFARE", "COURTS", "ATTEND", 
  "RELITEN", "HAPPY", "HAPMAR", "HEALTH", "LIFE", "HELPFUL", 
  "FAIR", "TRUST", "CONFINAN", "CONBUS", "CONCLERG", "CONEDUC", 
  "CONFED", "CONLABOR", "CONPRESS", "CONMEDIC", "CONTV", "CONJUDGE", 
  "CONSCI", "CONLEGIS", "CONARMY", "AGED", "SATJOB", "FINALTER", 
  "FINRELA", "GETAHEAD", "DIVLAW", "PREMARSX", "XMARSEX", 
  "HOMOSEX", "PORNLAW", "NEWS"
)

# Initialize results data frame
results <- data.frame(Column_Name = target_column_names, Partial_R2 = NA, beta_value = NA, se_value = NA, t_value = NA)

# Loop through each variable
for (i in seq_along(target_column_names)) {
  current_column <- target_column_names[i]
  current_LAG_column <- paste0(current_column, "LAG")
  
  # Check if both the lag column and MobilityLAG exist in the data
  if (current_LAG_column %in% colnames(gss_data) && "MobilityLAG" %in% colnames(gss_data)) {
    # Build and run model
    formula <- as.formula(paste(current_column, "~ MobilityLAG +", current_LAG_column))
    model <- lm(formula, data = gss_data)
    
    # Extract statistics
    t_value <- summary(model)$coefficients["MobilityLAG", "t value"]
    partial_r2 <- (sqrt(t_value^2 / (t_value^2 + model$df.residual)))^2
    beta_value <- summary(model)$coefficients["MobilityLAG", "Estimate"]
    se_value <- summary(model)$coefficients["MobilityLAG", "Std. Error"]
    
    # Store results
    results$Partial_R2[i] <- partial_r2
    results$beta_value[i] <- beta_value
    results$se_value[i] <- se_value
    results$t_value[i] <- t_value
  } else {
    # Print message for missing lag column
    cat("Skipping", current_column, "because", current_LAG_column, "or MobilityLAG doesn't exist.\n")
  }
}

# Display results
print(results)


















