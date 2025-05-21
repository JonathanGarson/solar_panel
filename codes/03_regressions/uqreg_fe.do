// Unconditional Quantile Regression
cd "../../data/3_final/"
clear
import delimited "tts_final.csv"

//Export Path
local output_graph "../output/regression/uqe/"

// Preping the Data
gen log_tariff = log(tariff)
gen log_efficiency = log(efficiency_module)
label variable log_tariff "Log Tariff Rate"
gen pv_system_size_dc_sq = pv_system_size_dc^2
egen quarter_origin = group(year_quarter origin)
encode installer_name, gen(fe_installer)
encode origin, gen(fe_origin)
encode county, gen(fe_county)
encode year_quarter, gen(fe_yearq)
encode module_manufacturer, gen(fe_firm)
gen post_tariff = year >= 2018

local controls pv_system_size_dc pv_system_size_dc_sq elec_price ///
    mean_week_wage population_density pct_bachelor_estimate ///
    median_home_value median_household_income

//AD1
	 
bootstrap, reps(100): rqr log_efficiency log_tariff if inrange(year, 2010, 2013), ///
    quantile(0.10(0.10)0.9) ///
    controls($controls) ///
    absorb(fe_yearq fe_firm fe_installer fe_county)
rqrplot
graph export "`output_graph'uqr_all_ad1.png", replace
	
//AD2
	 
bootstrap, reps(100): rqr log_efficiency log_tariff if inrange(year, 2014, 2016), ///
    quantile(0.10(0.1)0.90) ///
    controls($controls) ///
    absorb(fe_installer fe_yearq fe_firm fe_county)
rqrplot
graph export "`output_graph'uqr_all_ad2.png", replace

//ST
	 
bootstrap, reps(100): rqr log_efficiency log_tariff if inrange(year, 2017, 2018), ///
    quantile(0.10(0.1).90) ///
    controls($controls) ///
    absorb(fe_installer fe_yearq fe_firm fe_origin fe_county)
rqrplot
graph export "`output_graph'uqr_all_st.png", replace


//QTE
rifhdreg log_efficiency treated $controls if inrange(year, 2017, 2018), rif(q(10)) over(treated) rwlogit($controls) abs(fe_installer fe_yearq fe_origin fe_county) 
rifhdreg log_efficiency treated $controls if inrange(year, 2017, 2018), rif(q(20)) over(treated) rwlogit($controls) abs(fe_installer fe_yearq fe_origin fe_county)
rifhdreg log_efficiency treated $controls if inrange(year, 2017, 2018), rif(q(30)) over(treated) rwlogit($controls) abs(fe_installer fe_yearq fe_origin fe_county)
rifhdreg log_efficiency treated $controls if inrange(year, 2017, 2018), rif(q(40)) over(treated) rwlogit($controls) abs(fe_installer fe_yearq fe_origin fe_county)
rifhdreg log_efficiency treated $controls if inrange(year, 2017, 2018), rif(q(50)) over(treated) rwlogit($controls) abs(fe_installer fe_yearq fe_origin fe_county)
rifhdreg log_efficiency treated $controls if inrange(year, 2017, 2018), rif(q(60)) over(treated) rwlogit($controls) abs(fe_installer fe_yearq fe_origin fe_county)
rifhdreg log_efficiency treated $controls if inrange(year, 2017, 2018), rif(q(70)) over(treated) rwlogit($controls) abs(fe_installer fe_yearq fe_origin fe_county)
