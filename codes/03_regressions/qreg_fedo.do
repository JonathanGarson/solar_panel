// This code implement the package developped by Rios-Avila and Machado and which allow fo QR with FE

clear
import delimited "tts_final.csv"

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
gen post_tariff = year >= 2018

// AD1
	
foreach q in  20 40 60 {
		mmqreg log_efficiency log_tariff pv_system_size_dc pv_system_size_dc_sq elec_price ///
        mean_week_wage population_density pct_bachelor_estimate median_home_value median_household_income ///
        if year >= 2010 & year <= 2013, ///
        q(`q') abs(fe_installer fe_yearq fe_origin fe_county) ///
        cluster(zip_code) 
    estimates store mmqreg_q`q'
}
 
esttab  mmqreg_q20  mmqreg_q40 ///
       mmqreg_q60  ///
       using "../../output/regression/quality_shift/quantile_fe_ad1.tex", ///
       keep(log_tariff) star(* 0.1 ** 0.05 *** 0.01) ///
       se label replace
	  
foreach q in 10 20 30 40 50 60 {
		quietly mmqreg log_efficiency log_tariff pv_system_size_dc pv_system_size_dc_sq elec_price ///
        mean_week_wage population_density pct_bachelor_estimate median_home_value median_household_income ///
        if year >= 2010 & year <= 2013, ///
        q(`q') abs(fe_installer quarter_origin fe_county) ///
        cluster(zip_code) 
    estimates store mmqreg_q`q'
}
 
esttab mmqreg_q10 mmqreg_q20 mmqreg_q30 mmqreg_q40 mmqreg_q50 ///
       mmqreg_q60  ///
       using "../../output/regression/quality_shift/quantile_fe_ad1_t2.tex", ///
       keep(log_tariff) star(* 0.1 ** 0.05 *** 0.01) ///
       se label replace
	   	   
qregplot log_tariff, q(5(5)95) seed(100) label xsize(10) ysize(7)
// AD2

// mmqreg log_efficiency log_tariff pv_system_size_dc pv_system_size_dc_sq elec_price ///
//        mean_week_wage population_density rebate_w pct_bachelor_estimate median_home_value median_household_income ///
//        if year >= 2014 & year <= 2016, ///
//        q(10 20 30 40 50 60 70 80 90) ///
//        abs(fe_installer fe_yearq fe_origin fe_county) ///
//        cluster(zip_code)
	  

foreach q in 20 40 60 80 {
		mmqreg log_efficiency log_tariff pv_system_size_dc pv_system_size_dc_sq elec_price ///
        mean_week_wage population_density rebate_w pct_bachelor_estimate median_home_value median_household_income ///
        if year >= 2014 & year <= 2016, ///
        q(`q') abs(fe_installer fe_yearq fe_origin fe_county) ///
        cluster(zip_code) 
    estimates store mmqreg_q`q'
}
  
esttab mmqreg_q10 mmqreg_q20 mmqreg_q30 mmqreg_q40 mmqreg_q50 ///
       mmqreg_q60 mmqreg_q70 mmqreg_q80  ///
       using "../../output/regression/quality_shift/quantile_fe_ad2.tex", ///
       keep(log_tariff) star(* 0.1 ** 0.05 *** 0.01) ///
       se label replace
	   
foreach q in 10 20 30 40 50 60 70 80 {
    quietly mmqreg log_efficiency log_tariff pv_system_size_dc pv_system_size_dc_sq elec_price ///
        mean_week_wage population_density rebate_w pct_bachelor_estimate median_home_value median_household_income ///
        if year >= 2014 & year <= 2016, ///
        q(`q') abs(fe_installer quarter_origin fe_county) ///
        cluster(zip_code) 
    estimates store mmqreg_q`q'
}
  
esttab mmqreg_q10 mmqreg_q20 mmqreg_q30 mmqreg_q40 mmqreg_q50 ///
       mmqreg_q60 mmqreg_q70 mmqreg_q80  ///
       using "../../output/regression/quality_shift/quantile_fe_ad2_t2.tex", ///
       keep(log_tariff) star(* 0.1 ** 0.05 *** 0.01) ///
       se label replace
	   
// ST ***********************************
foreach q in 20 40 60 80 90{
mmqreg log_efficiency log_tariff pv_system_size_dc pv_system_size_dc_sq elec_price ///
        mean_week_wage population_density pct_bachelor_estimate median_home_value median_household_income ///
        if year >= 2017 & year <= 2018, ///
        q(`q') abs(fe_installer fe_yearq fe_origin fe_county) ///
        cluster(zip_code) 
    estimates store mmqreg_q`q'
}
esttab mmqreg_q10 mmqreg_q20 mmqreg_q30 mmqreg_q40 mmqreg_q50 ///
       mmqreg_q60 mmqreg_q70 mmqreg_q80 mmqreg_q90 ///
       using "../../output/regression/quality_shift/quantile_fe_st.tex", ///
       keep(log_tariff) star(* 0.1 ** 0.05 *** 0.01) ///
       se label replace

foreach q in 20 40 60 80 100 {
	mmqreg log_efficiency log_tariff pv_system_size_dc pv_system_size_dc_sq elec_price ///
        mean_week_wage population_density pct_bachelor_estimate median_home_value median_household_income ///
        if year >= 2017 & year <= 2018 & origin == "china", ///
        q(`q') abs(fe_installer fe_origin fe_yearq fe_county) ///
        cluster(zip_code) 
    estimates store mmqreg_q`q'
}
esttab mmqreg_q10 mmqreg_q20 mmqreg_q30 mmqreg_q40 mmqreg_q50 ///
       mmqreg_q60 mmqreg_q70 mmqreg_q80 mmqreg_q90 ///
       using "../../output/regression/quality_shift/quantile_fe_st_china.tex", ///
       keep(log_tariff) star(* 0.1 ** 0.05 *** 0.01) ///
       se label replace
	   	   	  
foreach q in 10 20 30 40 50 60 70 80 90 {
    quietly mmqreg log_efficiency log_tariff pv_system_size_dc pv_system_size_dc_sq elec_price ///
        mean_week_wage population_density pct_bachelor_estimate median_home_value median_household_income ///
        if year >= 2017 & year <= 2018, ///
        q(`q') abs(fe_installer fe_yearq fe_origin fe_county) ///
        cluster(zip_code) 
    estimates store mmqreg_q`q'
}	   
esttab mmqreg_q10 mmqreg_q20 mmqreg_q30 mmqreg_q40 mmqreg_q50 ///
       mmqreg_q60 mmqreg_q70 mmqreg_q80 mmqreg_q90 ///
       using "../../output/regression/quality_shift/quantile_fe_st_t2.tex", ///
       keep(log_tariff) star(* 0.1 ** 0.05 *** 0.01) ///
       se label replace
	   	   
		   
//Exploratory
mmqreg log_efficiency c.treated##i.fe_origin pv_system_size_dc pv_system_size_dc_sq elec_price ///
	mean_week_wage population_density pct_bachelor_estimate median_home_value median_household_income ///
	if year >= 2017 & year <= 2018 & origin != "china", ///
	q(10 20 30 40 50 60 70 80 90) abs(fe_installer  fe_yearq fe_county) ///
	cluster(zip_code) 

