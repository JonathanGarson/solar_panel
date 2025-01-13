if (Sys.getenv("USERNAME") == "jonat") {
  setwd("C:/Users/jonat/sciencespo/solar_panel_project")
  data_raw_path = "C:\\Users\\jonat\\Dropbox\\recherche\\master_thesis\\data\\raw\\"
  data_temp_path = "C:\\Users\\jonat\\Dropbox\\recherche\\master_thesis\\data\\temp\\"
  data_final_path = "C:\\Users\\jonat\\Dropbox\\recherche\\master_thesis\\data\\final\\"
} else if (Sys.getenv("USERNAME") == "GARSON") {
  setwd("/Users/GARSON/sciencespo/thesis/solar_panel_project")
  data_raw_path = "/Users/GARSON/Dropbox/recherche/master_thesis/data/raw"
  data_temp_path = "/Users/GARSON/Dropbox/recherche/master_thesis/data/temp"
  data_final_path = "/Users/GARSON/Dropbox/recherche/master_thesis/data/final"
} 

data_raw = function(filename){
  paste0(data_raw_path, filename)
}
data_temp = function(filename){
  paste0(data_temp_path, filename)
}
data_final = function(filename){
  paste0(data_final_path, filename)
}
