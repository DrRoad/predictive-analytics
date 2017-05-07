# Check first if the needed packages are install in the current environment
# before using them below. 
packages <- c("ggplot2", "ggvis", "corrplot", "survival", "survminer")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# Libraries to be used
library(ggplot2)
library(ggvis)
library(corrplot)
library(survival)
library(survminer)
library(data.table)
library(readr)

UpperOutlier <- 2160
LowerOutlier <- -1440

# Load IoT Sensor Captured - Equipment Performance Dataset for Statistical Analysis
iotData <- read_csv("equipment_IoT_sensor_data.csv")

# Do some renaming to shorten column names in graphs
iotPolishedData <- rename(iotData, c("equipment_category"="Category",
                  "engineer_at_time_of_reading"="Technician",
                  "equipment_code"="EQP_Code",
                  "running_hrs"="Running_Hrs",
                  "pressureInd_kgf_cm2"="Pressure","temperature_celsius"="Temp",
                  "vibrationInd_kHz"="Vibration",
                  "has_failure"="Is_Faulty",
                  "total_failure_count"="Total_Faults",
                  "downtime_min"="Downtime",
                  "failure_category"="Fault_Category",
                  "mtbf"="MTBF","mttr"="MTTR"))

# Check if data is in original shape...
if(!hasName(iotPolishedData,"MTBF")){
  
  # ... yes, data is in original shape, calculating MTBF / MTTR now...
  # Calculate Mean Time Between Failures (MTBF) = (Total up time) / (number of breakdowns)
  # Calculate Mean Time To Recover (MTTR) = (Total down time) / (number of breakdowns)
  iotPolishedData$Running_Hrs <- round(iotPolishedData$Running_Hrs / iotPolishedData$Total_Faults,0)
  iotPolishedData$Downtime <- round(iotPolishedData$Downtime / iotPolishedData$Total_Faults,0)
  
  # Optional: If you wish you can persist your changes through following call
  write_csv(iotPolishedData, "equipment_IoT_sensor_data.csv")
  
}

filteredDataSet <- iotPolishedData # %>% filter(Category %in% c("Materials Protection and Isolation","Air Gas and Pneumatic Equipment","Hydraulics Equipment"))

# Select survival regression model dependent variables.
depVariables = Surv(filteredDataSet$Running_Hrs, filteredDataSet$Is_Faulty)

# Create model (use the gaussian method)
survreg = survreg(depVariables~Pressure+Temp, dist="gaussian",data=filteredDataSet)
print(survreg)

# Fitting the Model now
fit <- survfit(Surv(Running_Hrs,Is_Faulty) ~ Category , data = filteredDataSet)

# Show the Survival Analysis Graph
ggsurvplot(fit, filteredDataSet, risk.table = FALSE, xlab = "Running Hours")

# Perform Prediction & Run the Forecast
Prediction <- predict(survreg, newdata = filteredDataSet, type="quantile", p=.5)
Forecast <- data.frame(Prediction)
Forecast$running_hrs <- filteredDataSet$Running_Hrs
Forecast$has_failure <- filteredDataSet$Is_Faulty
Forecast$Equipment <- filteredDataSet$EQP_Code

# Calculate Remaining Time to Failure (Time_To_Failure)
Forecast$RTime_To_Failure <- Forecast$Prediction - filteredDataSet$Running_Hrs

# Filtering out these outliers which should have failed 2 months ago. 
# Check with engineering, they might have overhault these but not updated the running hours counter
FilteredForecastOutliers <- Forecast %>% filter(RTime_To_Failure < LowerOutlier | RTime_To_Failure > UpperOutlier)
data.table(FilteredForecastOutliers[order(FilteredForecastOutliers$RTime_To_Failure),])

# Selected records ignoring major outliers and time to failures well in future e.g. 90days = 2160hrs
FilteredForecast <- Forecast %>% filter(RTime_To_Failure > LowerOutlier & RTime_To_Failure < UpperOutlier)

# Order the elements by Expected Remaining Time to Failure
FilteredForecast <- FilteredForecast[order(FilteredForecast$RTime_To_Failure),]
data.table(FilteredForecast)

# Classify and return the maintenance / replacement candidates sorted by urgency
FilteredForecast$class <- cut(FilteredForecast$RTime_To_Failure, c(LowerOutlier,1,4,UpperOutlier))
levels(FilteredForecast$class) <- c('Urgent', 'Medium', 'good')
summary(FilteredForecast)

