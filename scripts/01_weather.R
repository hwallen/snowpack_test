packages <- c("tidyverse", "here", "terra", "lubridate", "fmi2", "sf", "geofi")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)
rm(packages)

df_radiation <- read_csv(here("data", "weather", "tahtela_irradiance.csv"),
													na = "-") %>% 
	rename("ILWR" = "Long wave solar radiation (W/m2)",
				 "ISWR" = "Global radiation (W/m2)",
				 "RSWR" = "Reflected radiation (W/m2)",
				 "OLWR" = "Long wave outgoing solar radiation (W/m2)") %>% 
	mutate(timestamp = ymd_hms(paste0(Year, "-", m, "-", d,"-", Time)),
				 ISWR = case_when(ISWR < 0 ~ 0, TRUE ~ ISWR),
				 RSWR = case_when(RSWR < 0 ~ 0, TRUE ~ RSWR)) %>% # Negative values to 0
	select(timestamp, ILWR, RSWR, ISWR, OLWR)
	


df_meteo <- read_csv(here("data", "weather", "tahtela_meteo.csv"),
										 na = "-") %>% 
	rename("TA" = "Air temperature (degC)",
				 "RH" = "Relative humidity (%)",
				 "HS" = "Snow depth (cm)",
				 "VW" = "Wind speed (m/s)") %>% 
	mutate(timestamp = ymd_hms(paste0(Year, "-", m, "-", d,"-", Time))) %>% 
	select(timestamp, TA, RH, HS, VW)

df_final <- left_join(df_radiation,
					df_meteo,
					by = "timestamp") %>% 
	relocate(timestamp, TA, RH, VW, ISWR, RSWR, ILWR, HS) %>% 
	mutate(TA = TA + 273.15,
				 HS = HS/100,
				 RH = RH/100) %>% 
	mutate(RSWR = replace_na(RSWR, mean(RSWR, na.rm = TRUE)),
				 ILWR = replace_na(ILWR, mean(ILWR, na.rm = TRUE)),
				 OLWR = replace_na(OLWR, mean(OLWR, na.rm = TRUE))) # replace missing values with mean values. Needs better imputation for this

write_delim(df_final, 
						delim = ";",
						file = here("snowpack", "meteo","meteo_tahtela.csv"))



# dem: tiff to cdf ----------------------------------------------------

dem_tahtela <- terra::rast(here("data", "dem", "U4344C.tif"))
writeCDF(dem_tahtela, here("output", "dem", "tahteladem2x2.cdf"))
