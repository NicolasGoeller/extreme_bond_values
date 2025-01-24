library(data.table)
library(ecb)

library(ismev)
library(evir)
library(stringr)

# Define base SDW key for EA triple-A gov bonds
base_key <- "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_"
# Define list of standard maturities of interest
mat <- c("3M","6M","9M","1Y","2Y","3Y","5Y","7Y","10Y","20Y","30Y")

# Loop over maturities, call API and put in list
bond_list <- list()
for (m in mat){
  key <- paste0(base_key,m)
  print(key)
  # Call API
  iter <- get_data(key, filter=list(detail="dataonly"))
  # Save in list
  bond_list[[length(bond_list)+1]] <- data.table(iter)
}

# Combine list to data.table
gov_bonds <- do.call(rbind, bond_list)

# Modify important indicator columns
gov_bonds$ref_area <- ifelse(gov_bonds$provider_fm == "4F","EA",gov_bonds$provider_fm)
gov_bonds$obstime<- as.Date(gov_bonds$obstime, format = "%Y-%m-%d")

# Rename columns appropriately
setnames(gov_bonds, old = c("data_type_fm","obsvalue","obstime"), 
         new = c("maturity", "spot_rate", "obs_time"))

# Keep only relevant columns
gov_bonds$ttm <- as.numeric(str_extract(gov_bonds$maturity,"[0-9]+"))
gov_bonds$mat_unit <- str_extract(gov_bonds$maturity,"[M,Y]{1}")
gov_bonds[mat_unit == "M", ttm := ttm * 1/12]
gov_bonds <- gov_bonds[,c("ref_area","maturity","ttm","obs_time","spot_rate")]

# Calculate daily bond prices from spot rates
#gov_bonds$price <- 100*exp(-gov_bonds$spot_rate * gov_bonds$ttm)
gov_bonds$price <- 100*(1+gov_bonds$spot_rate)^(-gov_bonds$ttm)

# Cast into wide form to get individual time series
gov_bonds <- dcast(gov_bonds, ref_area+obs_time~maturity, value.var = "price")
setcolorder(gov_bonds, c("ref_area","obs_time",paste0("SR_",mat)))

# Calculate daily bond returns from prices
returns <- copy(gov_bonds)
returns <- returns[, c(3:13) := log(.SD / shift(.SD)), .SDcols = c(3:13)]
returns <- na.omit(returns)

library(ggplot2)
library(modelsummary)

# Produce summary stats tables

test <- returns %>% 
  select(-ref_area, -obs_time)

Skew <- function(x){skewness(x)}
Kurt <- function(x){kurtosis(x)}

datasummary(All(test)~Mean+SD+Median+Min+Max+Skew+Kurt, data=test)

# Run basic EVA analysis on 2 series

ggplot(data = test, aes(x=obs_time,y=SR_5Y)) +
  geom_point()
