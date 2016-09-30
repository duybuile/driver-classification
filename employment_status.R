

source("P:/Duy/R/Chillidrive Analysis/connect_to_MySQL.R")
setwd("C:/Users/duy.bui/Documents/GitHub/driver-classification/")

sql = "SELECT DISTINCT policy_id, employment_status_ft
FROM drivers
INNER JOIN customers ON drivers.cust_policy_id = customers.id
INNER JOIN current_insurers ON customers.current_insurer_id = current_insurers.id
INNER JOIN insurers ON current_insurers.insurer_id = insurers.id 
WHERE (code = 'AVT' OR code = 'AXT' OR code = 'HIT')
AND licence_years < 10;"

employment_status <- getDataFromHubSync(sql)

employment_status <- subset(employment_status, !duplicated(employment_status$policy_id))

save(employment_status, file = "employment.Rdata")
