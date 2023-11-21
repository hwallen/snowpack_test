packages <- c("tidyverse", "here", "lubridate", "janitor", "sarp.snowprofile", "ggthemes", "openxlsx")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)
rm(packages)


# Profile files -----------------------------------------------------------

tiedostot <- list.files(here("snowpack", "output_snow"),
												full.names = T,
												pattern = ".pro")

pro_tahtelaheight_2 <- snowprofilePro(tiedostot[24])

df_pro <- rbind(pro_tahtelaheight_2) %>% 
	as_tibble() %>% 
	mutate(station = "Sodankylä Tähtelä") %>% 
	select(-station_id) %>% 
	group_by(datetime) %>% 
	mutate(layer_num = seq_along(datetime)) %>% 
	ungroup() %>% 
	relocate(station, datetime, date, layer_num)

write.xlsx(df_pro,
					 here("output", "profile_example.xlsx"))
