# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO EVENT 
# SHALL THE COPYRIGHT HOLDERS OR ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE 
# FOR ANY DAMAGES OR OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE, 
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
# DEALINGS IN THE SOFTWARE.

#
# Check first if the needed packages are install in the current environment
# before using them below. 
packages <- c("ggplot2", "ggvis", "corrplot", "survival", "survminer","data.table","SPREDA","boot","lattice", "readr")

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
library(SPREDA)
library(boot)
library(lattice)
library(readr)


#source("predictive_maintenance_helper.R")

# Constants to filter out outliers 
UpperOutlier <- 2160
LowerOutlier <- -1440

# Load IoT Sensor Captured - Equipment Performance Dataset for Statistical Analysis
iotData <- read_csv("equipment_IoT_sensor_data.csv")

# Do some renaming to shorten column names in graphs
colnames(iotData) <- c("Date","Time","Technician","EQP_Code","Category",
                       "Running_Hrs", "Pressure","Temp","Vibration",
                       "Is_Faulty","Total_Faults","Downtime","Fault_Category",
                       "MTBF","MTTR")

# Do some renaming to shorten column names in graphs
iotPolishedData <- iotData

# Check if data is in original shape...
if(!hasName(iotPolishedData,"MTBF")){
  
  # ... yes, data is in original shape, calculating MTBF / MTTR now...
  # Calculate Mean Time Between Failures (MTBF) = (Total up time) / (number of breakdowns)
  iotPolishedData$Running_Hrs <- round(iotPolishedData$Running_Hrs / iotPolishedData$Total_Faults,0)
  # Calculate Mean Time To Recover (MTTR) = (Total down time) / (number of breakdowns)
  iotPolishedData$Downtime <- round(iotPolishedData$Downtime / iotPolishedData$Total_Faults,0)
  # Optional: If you wish you can persist your changes through following call
  write_csv(iotPolishedData, "equipment_IoT_sensor_data.csv")
  
}

# Following are the possible Equipment Categories to choose from, just uncomment
# the code below and remove the categories not of interest for your analysis
filteredDataSet <- iotPolishedData %>% filter(Category %in% 
  c(
    "Materials Protection and Isolation",
    "Management Services",
    "Air Gas and Pneumatic Equipment",
    "Cooling Equipment"
    # "Structures and Outfitting Details",
    # "Automation and Control",
    # "Construction and Operation Services",
    # "Tanks and Heating",
    # "Electrical",
    # "Propulsion and Manouvering",
    # "Hydraulics Equipment",
    # "Power Generation",
    # "Firefighting Equipment",
    # "Fluid Transfer and Treatment",
    # "Fabrication Equipment and Software"
  )
)

# Show all Equipment Categories instead of above
# filteredDataSet <- iotPolishedData

# Select survival regression model dependent variables.
depVariables = Surv(filteredDataSet$Running_Hrs, filteredDataSet$Is_Faulty)

# Create model (use the gaussian method)
survreg = survreg(depVariables~Pressure+Temp, dist="gaussian",data=filteredDataSet)
print(survreg)

# Fitting the Model now
fit <- survfit(Surv(Running_Hrs,Is_Faulty) ~ Category , data = filteredDataSet)

# Perform Prediction & Run the Forecast
Prediction <- predict(survreg, newdata = filteredDataSet, type="quantile", p=.5)
Forecast <- data.frame(Prediction)
Forecast$Running_Hrs <- filteredDataSet$Running_Hrs
Forecast$Is_Faulty <- filteredDataSet$Is_Faulty
Forecast$Equipment <- filteredDataSet$EQP_Code
Forecast$MTBF <- filteredDataSet$MTBF
Forecast$MTTR <- filteredDataSet$MTTR
Forecast$Fault_Category <- filteredDataSet$Fault_Category
Forecast$Technician <- filteredDataSet$Technician
Forecast$Date <- filteredDataSet$Date
Forecast$Time <- filteredDataSet$Time

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
FilteredForecast$Priority <- cut(FilteredForecast$RTime_To_Failure, c(LowerOutlier,1,4,UpperOutlier))
levels(FilteredForecast$Priority) <- c('Urgent', 'Medium', 'good')

# Show the Survival Analysis Graph
ggsurvplot(fit, 
           Forecast, 
           xlab = "Running Hours",
           title = "Predictive Failure Analysis",
           legend.labs = c("Air & Gas", "Cooling", "Management", "Material"),
           legend.title = "Equipment Categories",
           risk.table.title = "Number of equipment at risk",
           risk.table = T, 
           risk.table.y.text = T,
           risk.table.y.text.col = T)

# Show the Final Statistics
summary(FilteredForecast)

