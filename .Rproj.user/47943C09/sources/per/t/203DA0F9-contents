packages <- c("tidyverse", "here", "terra", "lubridate", "sf", "janitor", "missForest", "ggthemes")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)
rm(packages)

## Data preparation 2022-2023

files_irradiance <- list.files(here("data", "weather", "tahtela", "irradiance"),
															 full.names = T,
															 pattern = "rad")

irra2023_hourly <- bind_rows(read_delim(files_irradiance[1],
																 delim = ";",
																 na = c("", "NA")) %>% 
													select(-`Observation station`) %>% 
												mutate_if(is.character, as.double),
											read_delim(files_irradiance[2],
																 delim = ";",
																 na = c("", "NA")) %>% 
												select(-`Observation station`) %>% 
												mutate_if(is.character, as.double))  %>% 
	rename("ILWR" = "Long wave solar radiation mean [kJ/m2]",
				 "ISWR" = "Global radiation mean [W/m2]",
				 "RSWR" = "Reflected radiation mean [W/m2]",
				 "OLWR" = "Long wave outgoing solar radiation mean [kJ/m2]") %>% 
	mutate(timestamp = ymd_hms(paste0(Year, "-", Month, "-", Day,"-", `Time [Local time]`)),
				 ISWR = case_when(ISWR < 0 ~ 0, TRUE ~ ISWR),
				 RSWR = case_when(RSWR < 0 ~ 0, TRUE ~ RSWR)) %>% # Negative values to 0
	select(timestamp, ILWR, RSWR, ISWR, OLWR)
	
# Impute missing irradiance data ------------------------------------------

df_rad_impute <- irra2023_hourly %>% 
	select(-timestamp) %>% 
	as.data.frame()

rad_imputed <- missForest(df_rad_impute,
													maxiter = 10,
													verbose = T)$ximp %>% 
	as_tibble()

dfrad <- rad_imputed
dfrad$timestamp <- irra2023_hourly$timestamp

df_rad <- dfrad %>% 
	relocate(timestamp)


# Meteo -data -------------------------------------------------------------

files_meteo <- list.files(here("data", "weather", "tahtela", "meteo"),
															 full.names = T,
															 pattern = "tah")

meteo2023_hourly <- bind_rows(read_delim(files_meteo[2],
																				delim = ";",
																				na = c("", "NA", "-")) %>% 
														 	select(-`Observation station`) %>% 
														 	mutate_if(is.character, as.double),
														 read_delim(files_meteo[1],
														 					 delim = ";",
														 					 na = c("", "NA", "-")) %>% 
														 	select(-`Observation station`) %>% 
														 	mutate_if(is.character, as.double),
															read_delim(files_meteo[3],
																				 delim = ";",
																				 na = c("", "NA", "-")) %>% 
																select(-`Observation station`) %>% 
																mutate_if(is.character, as.double)) %>% 
	rename("TA" = "Air temperature mean [°C]",
				 "RH" = "Relative humidity mean [%]",
				 "HS" = "Snow depth mean [cm]",
				 "VW" = "Wind speed mean [m/s]") %>% 
	mutate(timestamp = ymd_hms(paste0(Year, "-", Month, "-", Day,"-", `Time [Local time]`))) %>% 
	select(timestamp, TA, RH, HS, VW) %>% 
	mutate(TA = TA + 273.15,
				 HS = HS/100,
				 RH = RH/100)

# Impute missing meteo data ------------------------------------------

df_meteo_impute <- meteo2023_hourly %>% 
	select(-timestamp) %>% 
	as.data.frame()

meteo_imputed <- missForest(df_meteo_impute,
													maxiter = 10,
													verbose = T)$ximp %>% 
	as_tibble()

dfmeteo <- meteo_imputed
dfmeteo$timestamp <- meteo2023_hourly$timestamp

dfmeteo <- dfmeteo %>% 
	relocate(timestamp)

df_final <- left_join(dfrad,
											dfmeteo,
											by = "timestamp") %>% 
	relocate(timestamp, TA, RH, VW, ISWR, RSWR, ILWR, HS) 

write_delim(df_final, 
						delim = ";",
						file = here("snowpack", "meteo","meteo_tahtela.csv"))


# Huhtikuun analyysi ------------------------------------------------------

p1 <- df_final %>% 
	filter(timestamp > "2023-04-05")  %>% 
	ggplot(aes(x=timestamp, y = TA-273.15)) + 
	geom_line() +
	labs(x = NULL,
			 y = "Mean temperature",
			 subtitle = "Mean temperature - April 6 ->") +
	theme_clean() +
	geom_hline(yintercept = 0, color = "orange", linetype = "dashed", linewidth = 1.25)

p2 <- df_final %>% 
	filter(timestamp > "2023-04-05")  %>% 
	ggplot(aes(x=timestamp, y = HS)) + 
	geom_line() +
	labs(x = NULL,
			 y = "Mean snow depth",
			 subtitle = "Mean temperature - April 6 ->") +
	theme_clean() 
