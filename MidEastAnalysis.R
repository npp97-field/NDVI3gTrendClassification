library(parallel)
library(zoo)
library(sp)
library(raster)

set.seed(1980)

# Path to GIMMS NDVI3g dataset
data.path <- "/media/Data/LUND/LUMA_GIS/GISM/DATA/ndvi3g_81_11"

# Path to output the results
output.path <- "/media/Data/LUND/LUMA_GIS/GISM/DATA/ProcessedNDVI3GData/"

# Define dataset start and end date 
# Note: try with at least 5 years difference (full-years)
data.date.start <- as.yearmon('2006-01')
data.date.end <-  as.yearmon('2010-12')

# Number of processor cores that are going to be used during the analysis.
# Note: Bear in mind, that by increasing this option, you will need more RAM
nCores <- 2

# Define your region, in this case MidEast
# Note: Run getData('ISO3') the codes
ME.Countries <- list (  "BHR", ## Bahrain
                        "CYP", ## Cyprus
                        "EGY", ## Egypt
                        "IRN", ## Iran
                        "IRQ", ## Iraq
                        "ISR", ## Israel
                        "JOR", ## Jordan
                        "KWT", ## Kuwait
                        "LBN", ## Lebanon
                        "OMN", ## Oman
                        "PSE", ## Palestine
                        "QAT", ## Qatar
                        "SAU", ## Saudi Arabia
                        "SYR", ## Syria
                        "TUR", ## Turkey
                        "ARE", ## United Arab Emirates
                        "YEM") ## Yemen


# Load the variable from above and run the analysis
TrendAnalyzer(region.countries = ME.Countries, 
                                 data.path = data.path,
                                 output.path = output.path, 
                                 output.format = "GTiff",
                                 data.date.start = data.date.start,
                                 data.date.end = data.date.end, 
                                 nCores = nCores)


