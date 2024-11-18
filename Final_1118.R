# Load required packages

library(shiny)
library(dplyr)
library(readxl)
library(haven)
library(imputeTS)
library(tidyverse)
library(EGAnet)
library(igraph)
library(qgraph)
library(DT)
library(ggplot2)
library(jtools)
library(shadowtext)
library(wesanderson)
library(psych)
library(ggsci)
library(ggpubr)
library(dlookr)
library(rstatix)
library(sjstats)
library(here)
library(gtsummary)
library(report)
library(tseries)
library(forecast)
library(lubridate)
library(huxtable)
library(officer)
library(flextable)


# Define file paths

# Define file paths
data_GSS <- "C:/Users/Pei-Chin/Dropbox (1)/shinyapp/recoded_GSS_data.sav"
data_dir <- "C:/Users/Pei-Chin/Documents/GitHub/World-Mobility/data"
output_dir <- "C:/Users/Pei-Chin/Documents/GitHub/World-Mobility/output"

# Load data
gss_data <- read_sav("C:\\Users\\Pei-Chin\\Dropbox (1)\\shinyapp\\GSS7218_R3.sav")
labels_data <- read_excel(file.path(data_dir, "Revised_info_modified0708 - youngjae suggestions_modified.xlsx"))


###There mighht be soome variables which are not encoding because they are considered non-liker scale in previous processing.
column_recode_3to2 <- c("COURTS", "RELITEN", "HELPFUL", "FAIR", "TRUST", "AGED", "FINALTER", "DIVLAW")
column_recode_4othertomissing <- c("GETAHEAD")
column_recode_5othertomissing <- c("PREMARSX", "XMARSEX", "HOMOSEX")


gss_data[, column_recode_4othertomissing][gss_data[, column_recode_4othertomissing] == 4] <- NA
gss_data[, column_recode_5othertomissing][gss_data[, column_recode_5othertomissing] == 5] <- NA
gss_data[, column_recode_3to2][gss_data[, column_recode_3to2] == 3] <- 9992
gss_data[, column_recode_3to2][gss_data[, column_recode_3to2] == 2] <- 9993
gss_data[, column_recode_3to2][gss_data[, column_recode_3to2] == 9992] <- 2
gss_data[, column_recode_3to2][gss_data[, column_recode_3to2] == 9993] <- 3

# Calculate missing values per column
data_by_year <- gss_data %>%
  group_by(YEAR) %>%
  summarise_all(mean, na.rm = TRUE)
nan_count_per_column <- sapply(data_by_year, function(x) sum(is.na(x)))

labels_data <- labels_data %>%
  mutate(missing_count = nan_count_per_column[match(variable, names(nan_count_per_column))])



library(tools)
# Get the current column names
current_colnames <- colnames(gss_data)
# Convert column names to lowercase and then capitalize the first letter
new_colnames <- toTitleCase(tolower(current_colnames))
#Prepare for EGA
gss_data_plot <- gss_data
# Assign the new column names to the data frame
colnames(gss_data_plot) <- new_colnames
colnames(gss_data_plot)[1] <- "year"


## Prep for analyses ---- 

### Calculating the average score for each year; with the re-coded variables ----
data_mean_by_year <- gss_data[,-c(2)] %>%  ## remove ID
  group_by(YEAR) %>%
  summarise_all(mean, na.rm=TRUE)

# Load the toTitleCase function from the tools package
library(tools)
# Get the current column names
current_colnames <- colnames(data_mean_by_year)
# Convert column names to lowercase and then capitalize the first letter
new_colnames <- toTitleCase(tolower(current_colnames))
# Assign the new column names to the data frame
colnames(data_mean_by_year) <- new_colnames

# 2. Running the main analytic code with all the variables ----

gss_data<-data_mean_by_year
usImmigrationData <- read.csv(file.path(data_dir, "USImmigration.csv"))
usGdpData <- read.csv(file.path(data_dir, "FREDGDP.csv"))

## Adding Residential Mobility (predictor) data ----
mobility_data<- read.csv(file.path(data_dir, "GSS level 2e.csv")) ## mobility data
mobility_data<-mobility_data[,c("year", "Mobility", "Mobilitystate")] 
colnames(gss_data)[1] <- "year" 
gss_data <- merge(mobility_data, gss_data, by = "year", all.x = TRUE)

colnames(gss_data)[1] <- "year" 



usGdpData <-usGdpData %>%
  mutate(Date = as.Date(DATE, format="%Y-%m-%d"),
         Year = year(Date),
         Month = month(Date)) %>%
  filter(Month==10)


# Merge and process gss_data with mobility, GDP, and immigration data
gss_data <- gss_data %>%
  arrange(year) %>%
  mutate(
    Fair = na_interpolation(Fair, option = "spline"),
    Trust = na_interpolation(Trust, option = "spline"),
    Happy = na_interpolation(Happy, option = "spline"),
    Helpful = na_interpolation(Helpful, option = "spline"),
    Mobility = na_interpolation(Mobility, option = "spline"),
    Mobilitystate = na_interpolation(Mobilitystate, option = "spline"),
    Aged = na_interpolation(Aged, option = "spline"),
    Attend = na_interpolation(Attend, option = "spline"),
    Conarmy = na_interpolation(Conarmy, option = "spline"),
    Conbus = na_interpolation(Conbus, option = "spline"),
    Conclerg = na_interpolation(Conclerg, option = "spline"),
    Coneduc = na_interpolation(Coneduc, option = "spline"),
    Confed = na_interpolation(Confed, option = "spline"),
    Confinan = na_interpolation(Confinan, option = "spline"),
    Conjudge = na_interpolation(Conjudge, option = "spline"),
    Conlabor = na_interpolation(Conlabor, option = "spline"),
    Conlegis = na_interpolation(Conlegis, option = "spline"),
    Conmedic = na_interpolation(Conmedic, option = "spline"),
    Conpress = na_interpolation(Conpress, option = "spline"),
    Consci = na_interpolation(Consci, option = "spline"),
    Contv = na_interpolation(Contv, option = "spline"),
    Courts = na_interpolation(Courts, option = "spline"),
    Divlaw = na_interpolation(Divlaw, option = "spline"),
    Finalter = na_interpolation(Finalter, option = "spline"),
    Finrela = na_interpolation(Finrela, option = "spline"),
    Getahead = na_interpolation(Getahead, option = "spline"),
    Hapmar = na_interpolation(Hapmar, option = "spline"),
    Health = na_interpolation(Health, option = "spline"),
    Homosex = na_interpolation(Homosex, option = "spline"),
    Life = na_interpolation(Life, option = "spline"),
    Nataid = na_interpolation(Nataid, option = "spline"),
    Natarms = na_interpolation(Natarms, option = "spline"),
    Natcity = na_interpolation(Natcity, option = "spline"),
    Natcrime = na_interpolation(Natcrime, option = "spline"),
    Natdrug = na_interpolation(Natdrug, option = "spline"),
    Nateduc = na_interpolation(Nateduc, option = "spline"),
    Natenvir = na_interpolation(Natenvir, option = "spline"),
    Natfare = na_interpolation(Natfare, option = "spline"),
    Natheal = na_interpolation(Natheal, option = "spline"),
    Natrace = na_interpolation(Natrace, option = "spline"),
    News = na_interpolation(News, option = "spline"),
    Pornlaw = na_interpolation(Pornlaw, option = "spline"),
    Premarsx = na_interpolation(Premarsx, option = "spline"),
    Reliten = na_interpolation(Reliten, option = "spline"),
    Satjob = na_interpolation(Satjob, option = "spline"),
    Incom16 = na_interpolation(Incom16, option = "spline"),
    Income = na_interpolation(Income, option = "spline"),
    Rincome = na_interpolation(Rincome, option = "spline"),
    Partyid = na_interpolation(Partyid, option = "spline"),
    Polviews = na_interpolation(Polviews, option = "spline"),
    Natspac = na_interpolation(Natspac, option = "spline"),
    Fund = na_interpolation(Fund, option = "spline"),
    Fund16 = na_interpolation(Fund16, option = "spline"),
    Spfund = na_interpolation(Spfund, option = "spline"),
    Class = na_interpolation(Class, option = "spline"),
    Satfin = na_interpolation(Satfin, option = "spline"),
    Coop = na_interpolation(Coop, option = "spline"),
    Comprend = na_interpolation(Comprend, option = "spline"),
    Xmarsex = na_interpolation(Xmarsex, option = "spline")
    
  ) %>%
  mutate(
    zFair = scale(Fair),
    zTrust = scale(Trust),
    zHappy = scale(Happy),
    zHelpful = scale(Helpful),
    zMobility = scale(Mobility),
    zMobilitystate = scale(Mobilitystate),
    zAged = scale(Aged),
    zAttend = scale(Attend),
    zConarmy = scale(Conarmy),
    zConbus = scale(Conbus),
    zConclerg = scale(Conclerg),
    zConeduc = scale(Coneduc),
    zConfed = scale(Confed),
    zConfinan = scale(Confinan),
    zConjudge = scale(Conjudge),
    zConlabor = scale(Conlabor),
    zConlegis = scale(Conlegis),
    zConmedic = scale(Conmedic),
    zConpress = scale(Conpress),
    zConsci = scale(Consci),
    zContv = scale(Contv),
    zCourts = scale(Courts),
    zDivlaw = scale(Divlaw),
    zFinalter = scale(Finalter),
    zFinrela = scale(Finrela),
    zGetahead = scale(Getahead),
    zHapmar = scale(Hapmar),
    zHealth = scale(Health),
    zHomosex = scale(Homosex),
    zLife = scale(Life),
    zNataid = scale(Nataid),
    zNatarms = scale(Natarms),
    zNatcity = scale(Natcity),
    zNatcrime = scale(Natcrime),
    zNatdrug = scale(Natdrug),
    zNateduc = scale(Nateduc),
    zNatenvir = scale(Natenvir),
    zNatfare = scale(Natfare),
    zNatheal = scale(Natheal),
    zNatrace = scale(Natrace),
    zNews = scale(News),
    zPornlaw = scale(Pornlaw),
    zPremarsx = scale(Premarsx),
    zReliten = scale(Reliten),
    zSatjob = scale(Satjob),
    zIncom16 = scale(Incom16),
    zIncome = scale(Income),
    zRincome = scale(Rincome),
    zPartyid = scale(Partyid),
    zPolviews = scale(Polviews),
    zNatspac = scale(Natspac),
    zFund = scale(Fund),
    zFund16 = scale(Fund16),
    zSpfund = scale(Spfund),
    zClass = scale(Class),
    zSatfin = scale(Satfin),
    zCoop = scale(Coop),
    zComprend = scale(Comprend),
    zXmarsex = scale(Xmarsex)
  )

gss_data<-gss_data %>%
  left_join(usImmigrationData, by=c("year"="Year")) %>%
  rename("Immigration"="Number") %>%
  left_join(usGdpData, by=c("year"="Year")) %>%
  mutate(FairLag = lag(Fair),
         TrustLag = lag(Trust),
         HappyLag = lag(Happy),
         HelpfulLag = lag(Helpful),
         MobilityLag = lag(Mobility),
         MobilitystateLag = lag(Mobilitystate),
         AgedLag = lag(Aged),
         AttendLag = lag(Attend),
         ConarmyLag = lag(Conarmy),
         ConbusLag = lag(Conbus),
         ConclergLag = lag(Conclerg),
         ConeducLag = lag(Coneduc),
         ConfedLag = lag(Confed),
         ConfinanLag = lag(Confinan),
         ConjudgeLag = lag(Conjudge),
         ConlaborLag = lag(Conlabor),
         ConlegisLag = lag(Conlegis),
         ConmedicLag = lag(Conmedic),
         ConpressLag = lag(Conpress),
         ConsciLag = lag(Consci),
         ContvLag = lag(Contv),
         CourtsLag = lag(Courts),
         DivlawLag = lag(Divlaw),
         FinalterLag = lag(Finalter),
         FinrelaLag = lag(Finrela),
         GetaheadLag = lag(Getahead),
         HapmarLag = lag(Hapmar),
         HealthLag = lag(Health),
         HomosexLag = lag(Homosex),
         LifeLag = lag(Life),
         NataidLag = lag(Nataid),
         NatarmsLag = lag(Natarms),
         NatcityLag = lag(Natcity),
         NatcrimeLag = lag(Natcrime),
         NatdrugLag = lag(Natdrug),
         NateducLag = lag(Nateduc),
         NatenvirLag = lag(Natenvir),
         NatfareLag = lag(Natfare),
         NathealLag = lag(Natheal),
         NatraceLag = lag(Natrace),
         NewsLag = lag(News),
         PornlawLag = lag(Pornlaw),
         PremarsxLag = lag(Premarsx),
         RelitenLag = lag(Reliten),
         SatjobLag = lag(Satjob),
         Incom16Lag = lag(Incom16),
         IncomeLag = lag(Income),
         RincomeLag = lag(Rincome),
         PartyidLag = lag(Partyid),
         PolviewsLag = lag(Polviews),
         NatspacLag = lag(Natspac),
         FundLag = lag(Fund),
         Fund16Lag = lag(Fund16),
         SpfundLag = lag(Spfund),
         ClassLag = lag(Class),
         SatfinLag = lag(Satfin),
         CoopLag = lag(Coop),
         ComprendLag = lag(Comprend),
         XmarsexLag = lag(Xmarsex))


calculateStatistics_data <-gss_data


# Shiny app UI
# Shiny app UI
# Shiny app UI
ui <- fluidPage(
  titlePanel("GSS Data Classification, EGA Analysis, and Cluster Selection"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Step 1: Choose Variable Types"),
      checkboxGroupInput("filterType", "Choose Variable Types:",
                         choices = c("Likert Scale" = "Likert", 
                                     "Binary" = "Binary", 
                                     "Continuous" = "Continuous", 
                                     "Multichoice" = "Multichoice")),
      sliderInput("missingThreshold", "Select Missing Values Threshold:", min = 0, max = max(nan_count_per_column, na.rm = TRUE), value = 5),
      actionButton("initialFilterButton", "Initial Filter"),
      
      h4("Step 2: Refine Variable Selection"),
      selectizeInput("variableSearch", "Search and Add Variables:", choices = labels_data$variable, multiple = TRUE),
      actionButton("addVariable", "Add Selected Variables"),
      uiOutput("variableSelectUI"),
      actionButton("filterFinalButton", "Filter Final Data"),
      
      h4("Step 3: Run Analysis"),
      actionButton("runEGA", "Run EGA Analysis"),
      uiOutput("clusterSelectUI"),
      uiOutput("interestedVariableSelectUI"),
      actionButton("plotButton", "Generate Plots"),
      downloadButton("downloadStructuredData", "Download Structured Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Filtered Data", DTOutput("filteredTable")),
        tabPanel("EGA Network", plotOutput("egaPlot")),
        tabPanel("Clustered Variables", DTOutput("clusteredTable")),
        tabPanel("Histogram Plot", plotOutput("histogramPlot")),
        tabPanel("Raincloud Plot", plotOutput("raincloudPlot"))
      )
    )
  )
)


# Shiny app server logic
server <- function(input, output, session) {
  
  # Helper function to format variable names
  formatVariableNames <- function(variables) {
    variables <- tolower(variables) # Convert to lowercase
    variables <- tools::toTitleCase(variables) # Capitalize the first letter
    return(variables)
  }
  
  # Format variable names in labels_data on load
  labels_data <- labels_data %>%
    mutate(variable = formatVariableNames(variable))
  
  # Calculate statistics function
  calculateStatistics <- function(grouped_data) {
    # Initialize vectors to store values
    partial_r2_values <- numeric(ncol(grouped_data) - 1) # Exclude "year"
    beta_values <- numeric(ncol(grouped_data) - 1)
    se_values <- numeric(ncol(grouped_data) - 1)
    t_values <- numeric(ncol(grouped_data) - 1)
    
    # Iterate over columns (excluding "year")
    for (i in 2:ncol(grouped_data)) { 
      variable_name <- names(grouped_data)[i]
      
      # Debug: Print the current variable being processed
      print(paste("Processing variable:", variable_name))
      
      # Construct formula
      formula_text <- paste(variable_name, "~ MobilityLag +", paste0(variable_name, "Lag"))
      print(as.formula(formula_text))  # Debug: Print formula
      
      # Ensure the lagged variable exists in the data
      lagged_var <- paste0(variable_name, "Lag")
      if (!lagged_var %in% colnames(calculateStatistics_data)) {
        warning(paste("Lagged variable", lagged_var, "is missing in the data. Skipping."))
        next
      }
      
      # Run the linear regression model
      model <- lm(as.formula(formula_text), data = calculateStatistics_data)
      
      # Ensure "MobilityLag" exists in coefficients
      if (!"MobilityLag" %in% rownames(summary(model)$coefficients)) {
        warning(paste("MobilityLag is not in the model for", variable_name, ". Skipping."))
        next
      }
      
      # Calculate partial R2 for the current column
      t_value <- summary(model)$coefficients["MobilityLag", "t value"]
      partial_r2 <- (sqrt(t_value^2 / (t_value^2 + model$df.residual)))^2
      
      # Store values
      partial_r2_values[i - 1] <- partial_r2
      beta_values[i - 1] <- summary(model)$coefficients["MobilityLag", "Estimate"]
      se_values[i - 1] <- summary(model)$coefficients["MobilityLag", "Std. Error"]
      t_values[i - 1] <- t_value
    }
    
    # Combine into a data frame
    stats_result <- data.frame(
      variable = names(grouped_data)[-1], # Exclude "year"
      Partial_R2 = partial_r2_values,
      beta_value = beta_values,
      se_value = se_values,
      t_value = t_values
    )
    
    return(stats_result)
  }
  
  
  # Initialize selectedVars as a reactive value
  selectedVars <- reactiveVal(character(0))
  
  # Initial filtered data based on variable types and missing values threshold
  initialFilteredData <- eventReactive(input$initialFilterButton, {
    req(input$filterType)  # Ensure filterType input is selected
    
    data <- labels_data %>%
      filter((`Likert Scale Variables` == 1 & "Likert" %in% input$filterType) |
               (`Binary Variables` == 1 & "Binary" %in% input$filterType) |
               (`Continuous Variables` == 1 & "Continuous" %in% input$filterType) |
               (`Multichoice variables` == 1 & "Multichoice" %in% input$filterType)) %>%
      filter(missing_count <= input$missingThreshold)
    
    # Set initial selectedVars to the initially filtered variables
    selectedVars(data$variable)
    data
  })
  
  # Final filtered data with added or removed variables by the user
  finalFilteredData <- reactive({
    req(input$selectedVars)  # Ensure selectedVars input is not empty
    
    # Get the variables selected by the user in the checkbox
    selected_vars <- input$selectedVars
    selectedVars(selected_vars)
    
    # Filter labels_data to only include the selected variables
    labels_data %>% filter(variable %in% selected_vars)
  })
  
  # Display initial filtered variables and allow user selection
  output$variableSelectUI <- renderUI({
    req(initialFilteredData())
    
    # Convert variable names to a list for checkboxGroupInput
    checkboxGroupInput("selectedVars", "Select Variables for Final Analysis:", 
                       choices = as.list(initialFilteredData()$variable), 
                       selected = as.list(initialFilteredData()$variable))
  })
  
  # Display final filtered data in the table
  output$filteredTable <- renderDT({
    finalFilteredData()
  })
  
  # Add variable(s) to selection list based on search input
  observeEvent(input$addVariable, {
    req(input$variableSearch)  # Ensure variableSearch input is not empty
    
    # Format search input variables and update selectedVars
    search_vars <- formatVariableNames(input$variableSearch)
    updated_vars <- unique(c(selectedVars(), search_vars))
    selectedVars(updated_vars)
    
    # Update checkboxGroupInput with formatted variable names
    updateCheckboxGroupInput(session, "selectedVars", 
                             choices = as.list(updated_vars), 
                             selected = as.list(updated_vars))
  })
  
  # Run EGA analysis based on final selected variables
  egaResults <- eventReactive(input$runEGA, {
    final_vars <- selectedVars()
    req(length(final_vars) > 0)  # Ensure variables are selected
    
    GSS_Processed <- gss_data_plot %>% select(all_of(final_vars))
    # Keep the first two column names unchanged, convert the rest to uppercase
    colnames(GSS_Processed)[-c(1, 2)] <- toupper(colnames(GSS_Processed)[-c(1, 2)])
    standardized_data <- scale(GSS_Processed)
    ega.tmfg <- EGA(standardized_data, model = "TMFG")
    cluster_info <- data.frame(variable = final_vars, cluster = ega.tmfg$wc)
    
    output$clusterSelectUI <- renderUI({
      selectInput("selectedCluster", "Select Cluster:", choices = unique(cluster_info$cluster))
    })
    
    output$interestedVariableSelectUI <- renderUI({
      req(input$selectedCluster)
      interested_vars <- cluster_info$variable[cluster_info$cluster == input$selectedCluster]
      checkboxGroupInput("interestedVars", "Select Interested Variables:", choices = as.list(interested_vars))
    })
    
    list(ega = ega.tmfg, cluster_info = cluster_info)
  })
  
  # Classification of Variables with calculateStatistics function using selected variables
  classifiedVariables <- reactive({
    req(input$selectedCluster, input$interestedVars)
    selected_cluster <- input$selectedCluster
    interested_vars <- input$interestedVars
    cluster_info <- egaResults()$cluster_info
    
    # Identify proximate and distal variables
    proximate_vars <- setdiff(cluster_info %>% filter(cluster == selected_cluster) %>% pull(variable), interested_vars)
    distal_vars <- cluster_info %>% filter(cluster != selected_cluster) %>% pull(variable)
    mobility_vars <- c("MobilityLag", paste0(interested_vars, "Lag"))
    
    # Group calculateStatistics_data by year and calculate means
    data_mean_by_year <- calculateStatistics_data %>%
      group_by(year) %>%
      summarise(across(all_of(c(interested_vars, proximate_vars, distal_vars, mobility_vars)), mean, na.rm = TRUE)) %>%
      ungroup()
    
    # Debug: Print grouped data
    print("=== Debug: data_mean_by_year ===")
    print(data_mean_by_year)
    
    # Apply statistical calculations to grouped data
    stats_data <- calculateStatistics(data_mean_by_year)
    
    # Add category column
    stats_data <- stats_data %>%
      mutate(
        category = case_when(
          variable %in% interested_vars ~ "Interested",
          variable %in% proximate_vars ~ "Proximate",
          variable %in% distal_vars ~ "Distal",
          TRUE ~ NA_character_
        )
      )
    
    return(stats_data)
  })
  
  
  # Render the table in the UI
  output$clusteredTable <- renderDT({
    classifiedVariables()
  })
  
  
  output$egaPlot <- renderPlot({
    req(egaResults())
    ega.tmfg <- egaResults()$ega
    
    # Dynamically generate colors based on the number of clusters
    num_clusters <- length(unique(ega.tmfg$wc))
    custom_colors <- wes_palette("Darjeeling1", num_clusters, type = "continuous")
    
    qgraph(
      ega.tmfg$network, 
      layout = "spring", 
      groups = as.factor(ega.tmfg$wc), 
      legend = TRUE, 
      nodeNames = ega.tmfg$items, 
      legend.cex = 0.4, 
      color = custom_colors
    )
  })
  
  # Render Histogram Plot
  output$histogramPlot <- renderPlot({
    plot_data <- classifiedVariables() %>% 
      filter(!is.na(category))  # 去除 category 为 NA 的行
    
    ggplot(data = plot_data, aes(x = abs(t_value), y = reorder(variable, abs(t_value)), fill = category)) +
      geom_col(color = "black") +
      scale_fill_manual(values = c("Interested" = "#83CA55", "Proximate" = "#F36F61", "Distal" = "#34548B")) + 
      theme_minimal() +
      theme(axis.text.y = element_text(angle = 0, hjust = 1))
  })
  
  
  # Render raincloud Plot
  output$raincloudPlot <- renderPlot({
    plot_data <- classifiedVariables() %>% 
      filter(!is.na(category))  # 去除 category 为 NA 的行
    
    ggplot(plot_data, aes(x = category, y = abs(t_value), fill = category)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.2, position = position_dodge(0.75), alpha = 0.6) +
      geom_jitter(width = 0.2, alpha = 0.3) +
      labs(y = "Absolute t-value for regression coefficient", x = "Category") +
      theme_classic(base_size = 15) +
      scale_fill_manual(values = c("Interested" = "#83CA55", "Proximate" = "#F36F61", "Distal" = "#34548B")) +
      theme(legend.position = "none", axis.text = element_text(size = 15))
  })
  
}


# Run Shiny app
shinyApp(ui = ui, server = server)


