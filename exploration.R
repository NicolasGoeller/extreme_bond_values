library(ecb)
library(data.table)

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
gov_bonds$maturity <- substr(gov_bonds$data_type_fm,4,stop=10)
gov_bonds$ref_area <- ifelse(gov_bonds$provider_fm == "4F","EA",gov_bonds$provider_fm)
gov_bonds$obs_time<- as.Date(gov_bonds$obstime, format = "%Y-%m-%d")

# Keep only relevant columns
gov_bonds <- gov_bonds[,c("ref_area","maturity","obs_time","obsvalue")]

# Cast into wide form to get individual time series
gov_bonds <- dcast(gov_bonds, ref_area+obs_time~maturity, value.var = "obsvalue")
setcolorder(gov_bonds, c("ref_area","obs_time",mat))

# Compute daily price changes
test <- gov_bonds[, c(3:13) := .SD - shift(.SD), .SDcols = c(3:13)]

# Create alternative dataset of log changes
#test_log <- test[, c(3:13) := lapply(.SD,log), .SDcols = c(3:13)]

