# This code generates the wage at the county level of Californian Residential Electrical Contractor, a proxy for Solar Panel Installer

library(arrow)
library(data.table)
library(glue)
library(ggplot2)

# Data -ggplot2# Data --------------------------------------------------------------------

data = c("qcew_2008-2011.csv","qcew_2012-2015.csv","qcew_2016-2019.csv","qcew-2020-2022.csv")
wage = data.table()
for (d in data){ 
  w = fread(data_raw(glue("electricity_contractor_wage/{d}")))
  if (is.null(wage)) {wage = d} else {wage = rbind(wage, w, fill = TRUE)}
  rm(w)
  gc()
}

setnames(wage, colnames(wage), c("area_type", "area_name", "year", "quarter", "ownership", "naics_level", "naics_code",
                                 "industry_name", "establishments", "mean_month_emp", "1st_month_emp", "2nd_month_emp", "3rd_month_emp",
                                 "total_wage", "mean_week_wage", "time_period"))

keep = c("area_type", "area_name", "year", "quarter", "naics_code",
         "industry_name", "establishments", "mean_month_emp",
         "total_wage", "mean_week_wage")
wage = wage[, ..keep]
wage = wage[naics_code == "238211",]
wage = wage[area_type == "County",]

setnames(wage, c("area_name"), c("county"))
wage[, mean_week_wage := mean(mean_week_wage, na.rm = TRUE), by = .(year, county)]
wage[, mean_month_emp := mean(mean_month_emp, na.rm = TRUE), by = .(year, county)]

keep = c("county", "year", "industry_name", "mean_month_emp", "mean_week_wage")
wage = unique(wage[, ..keep])

fwrite(wage, data_temp("elec_contractor_wage_emp.csv"))
