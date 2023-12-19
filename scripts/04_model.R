packages <- c("tidyverse", "here", "lubridate", "janitor", "ggthemes", "brms", "bayesplot", "cmdstanr", "mice")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)
rm(packages)


# Load data ---------------------------------------------------------------

df_main <- read_delim(here("data", "weather", "tahtela", "meteo", "tahtela_daily2000_2023.csv"),
											delim = ";",
											na = c("", "NA", "-")) %>% 
	clean_names() %>% 
	filter(month %in% c("11","12", "01", "02", "03", "04")) %>% 
	mutate(timestamp = ymd_hms(paste0(year, "-", month, "-", day,"-", time_local_time)),
				 snow_depth_cm = case_when(snow_depth_cm == -1 ~ 0,
				 													.default = snow_depth_cm)/100,
				 precipitation_amount_mm = case_when(precipitation_amount_mm == -1 ~ 0,
				 																		.default = precipitation_amount_mm),
				 observation_station = "Tähtelä",
				 year = factor(year),
				 month = factor(as.double(month)),
				 day = factor(as.double(day)),
				 .keep = "unused") %>% 
	rename("snowdepth" = "snow_depth_cm",
				 "precipitation" = "precipitation_amount_mm",
				 "temp_avg" = "average_temperature_c",
				 "station" = "observation_station") %>% 
	relocate(station,timestamp)

df2023 <- read.xlsx(here("data", "weather", "tahtela", "meteo", "tahtela2023_10min.xlsx")) %>% 
	as_tibble() %>% 
	clean_names() %>% 
	select(-1) %>% 
	mutate(timestamp = ymd_hm(paste0(year, "-", month, "-", day,"-", time_local_time)),
				 snow_depth_m = as.double(case_when(snow_depth_mean_cm == "-" ~ NA_character_,
				 													.default = snow_depth_mean_cm))/100,
				 air_temperature_mean = as.double(case_when(air_temperature_mean_c == "-" ~ NA_character_,
				 																					 .default = air_temperature_mean_c)),
				 rel_hum = round(as.double(case_when(relative_humidity_mean_percent == "-" ~ NA_character_,
				 										.default = relative_humidity_mean_percent))/100,2),
				 observation_station = factor("Tähtelä"),
				 year = factor(year),
				 month = factor(as.double(month)),
				 day = factor(as.double(day)),
				 .keep = "unused") %>%
	relocate(observation_station, timestamp)
	

p_snow <- df_main %>% 
	filter(snowdepth > 0) %>% 
	ggplot(aes(x = snowdepth)) +
	geom_histogram(binwidth = 1, color = "snow") +
	theme_clean()

p_prec <- df_main %>% 
	filter(precipitation > 0) %>% 
	ggplot(aes(x = precipitation)) +
	geom_histogram(color = "snow", bins = 30) +
	theme_clean()


# Descriptives ------------------------------------------------------------

df_main %>% 
	filter(precipitation > 0) %>% 
	group_by(year) %>% 
	summarise(prec_mean = mean(precipitation),
						prec_median = median(precipitation),
						prec_sd = sd(precipitation))


# Simple model: precipitation ---------------------------------------------

dfmodel <- df_main %>% 
	filter(precipitation > 0)

fsimple_prec_gamma <- bf(precipitation ~ 1) + Gamma(link = "log")




mod.gamma <- brm(formula = fsimple_prec_gamma,
								data = dfmodel,
								backend = "cmdstanr",
								iter = 3000, warmup = 1500,
								chains = 4, cores = 4)

fran_prec_gamma <- bf(precipitation ~ 1 + (1 | year)) + Gamma(link = "log")




modr.gamma <- brm(formula = fran_prec_gamma,
								 data = dfmodel,
								 backend = "cmdstanr",
								 iter = 3000, warmup = 1500,
								 chains = 4, cores = 4,
								 silent = 0)

fran_prec_wei <- bf(precipitation ~ 1 + (1 | year)) + weibull(link = "log")

modr.wei <- brm(formula = fran_prec_wei,
									data = dfmodel,
									backend = "cmdstanr",
									iter = 3000, warmup = 1500,
									chains = 4, cores = 4,
									silent = 0)

fran_prec_ext <- bf(precipitation ~ 1 + (1 | year)) + gen_extreme_value()

modr.ext <- brm(formula = fran_prec_ext,
								data = dfmodel,
								backend = "cmdstanr",
								iter = 3000, warmup = 1500,
								chains = 4, cores = 4,
								silent = 0)


# Posterior ---------------------------------------------------------------

modr.gamma %>% 
	spread_draws(b_Intercept) %>% 
	summarise(prec_mean = mean(b_Intercept),
						prec_sd = sd(b_Intercept))

modr.gamma %>% 
	spread_draws(b_Intercept, r_year[year,]) %>% 
	mutate(prec_mean = exp(b_Intercept) + exp(r_year)) %>% 
	ggplot(aes(x = prec_mean)) +
	stat_slabinterval(.width = c(0.8, 0.975),
										slab_fill ="#F0E442") +
	theme_clean() +
	labs(y = NULL,
			 x = "Precipitation",
			 title = )
	
modr.wei %>% 
	spread_draws(b_Intercept) %>% 
	summarise(prec_mean = mean(b_Intercept),
						prec_sd = sd(b_Intercept))

modr.wei %>% 
	spread_draws(b_Intercept, r_year[year,]) %>% 
	mutate(prec_mean = exp(b_Intercept) + exp(r_year)) %>% 
	ggplot(aes(x = prec_mean)) +
	stat_slabinterval(.width = c(0.8, 0.975),
										slab_fill ="#F0E442") +
	theme_clean() +
	labs(y = NULL,
			 x = "Precipitation",
			 title = "gmm: Weibull - Precipitation")
	

modr.wei %>% 
	spread_draws(b_Intercept) %>% 
	mutate(b_Intercept = exp(b_Intercept)) %>% 
	summarise(prec_mean = mean(b_Intercept),
						prec_sd = sd(b_Intercept),
						prec_max = max(b_Intercept))

start_dates <- c("2000-11-01",
								 "2001-11-01",
								 "2002-11-01",
								 "2003-11-01",
								 "2004-11-01",
								 "2005-11-01",
								 "2006-11-01",
								 "2007-11-01",
								 "2008-11-01",
								 "2009-11-01",
								 "2010-11-01",
								 "2011-11-01",
								 "2012-11-01",
								 "2013-11-01",
								 "2014-11-01",
								 "2015-11-01",
								 "2016-11-01",
								 "2017-11-01",
								 "2018-11-01",
								 "2019-11-01",
								 "2020-11-01",
								 "2021-11-01",
								 "2022-11-01")

end_dates <- c("2001-04-30",
							 "2002-04-30",
							 "2003-04-30",
							 "2004-04-30",
							 "2005-04-30",
							 "2006-04-30",
							 "2007-04-30",
							 "2008-04-30",
							 "2009-04-30",
							 "2010-04-30",
							 "2011-04-30",
							 "2012-04-30",
							 "2013-04-30",
							 "2014-04-30",
							 "2015-04-30",
							 "2016-04-30",
							 "2017-04-30",
							 "2018-04-30",
							 "2019-04-30",
							 "2020-04-30",
							 "2021-04-30",
							 "2022-04-30",
							 "2023-04-30")

vuodet <- vector(mode = "list",
								 length = length(end_dates))

for(i in 1:length(end_dates)){
	vuodet[[i]] <- df_main %>% 
		filter(timestamp >= start_dates[i] & timestamp <= end_dates[i]) %>% 
		mutate(delta_snow = c(NA, diff(snowdepth)),
					 winter_num = factor(paste0("winter", 
					 										str_sub(start_dates[i],1,4),
					 										str_sub(end_dates[i],1,4))),
					 year = droplevels(year))
}

df_deltasnow <- bind_rows(vuodet) %>% 
	select(2:7,9,10) %>% 
	group_by(winter_num) %>% 
	mutate(timestep = seq_along(winter_num),
				 anom = if_else(delta_snow > 2*sd(delta_snow, na.rm = T), T, F))

p_delta <- df_deltasnow %>% 
	drop_na() %>% 
	ggplot(aes(x = timestep,
						 y = delta_snow,
						 group = winter_num,
						 color = anom)) +
	geom_line() +
	scale_color_manual(values = c("black", "orange")) +
	facet_wrap(~winter_num) +
	theme_clean() + 
	labs(title = "Daily Snow Depth Change",
			 subtitle = "Anomaly delta > 2*SD",
			 color = "High delta")

ggsave(p_delta,
			 filename = here("dailysnowdepthchange.png"),
				 units = "px",
				 width = 4000,
				 height = 3000,
				 dpi = 300)

summary_delta <- df_deltasnow %>% 
	drop_na() %>% 
	group_by(winter_num) %>% 
	summarise(mean_delta = mean(delta_snow),
						median_delta = median(delta_snow),
						sd_delta = sd(delta_snow),
						max_delta = max(delta_snow))

p_sd <- summary_delta %>% 
	ggplot(aes(x = as.integer(winter_num),
						 y = sd_delta )) +
	geom_point() +
	geom_line() +
	geom_smooth(method = "lm") +
	theme_clean() +
	labs(x = "Winter number",
			 y = "sd",
			 subtitle = "SD delta snow")

ggsave(p_sd,
			 filename = here("output","delta_sd.png"),
			 units = "px",
			 width = 3000,
			 height = 2000,
			 dpi = 300)

p_max <- summary_delta %>% 
	ggplot(aes(x = as.integer(winter_num),
						 y = max_delta )) +
	geom_point() +
	geom_line() +
	geom_smooth(method = "lm") +
	theme_clean() +
	labs(x = "Winter number",
			 y = "delta_snow",
			 subtitle = "Maximum delta snow")

ggsave(p_max,
			 filename = here("output","max_delta.png"),
			 units = "px",
			 width = 3000,
			 height = 2000,
			 dpi = 300)

# Model -------------------------------------------------------------------


dfdeltamodel <- df_deltasnow %>% 
	drop_na() 

fran_ds_t <- bf(delta_snow ~ 1 + (1 | winter_num)) + student(link = "log")

modr.t <- brm(formula = fran_ds_t,
								data = dfdeltamodel,
								backend = "cmdstanr",
								iter = 3000, warmup = 1500,
								chains = 4, cores = 4,
								control = list(adapt_delta = 0.98),
								silent = 0)



# Q1 2023 model -----------------------------------------------------------

## Impute missing data

df2023_imp <- mice(df2023 %>% 
									 	select(6:8),
									 m = 5,
									 method = "pmm",
									 seed = 4863) %>% 
	complete() %>% 
	as_tibble() %>% 
	rename_with(toupper) %>% 
	bind_cols(df2023) %>% 
	relocate(observation_station, timestamp) %>% 
	mutate(delta_snow = c(0, diff(SNOW_DEPTH_M)),
				 time = seq_along(timestamp)) %>% 
	relocate(observation_station, time)

p_deltasnow <- df2023_imp %>% 
	ggplot(aes(timestamp, delta_snow)) +
	geom_col()

f.delta1 <- bf(delta_snow ~ ar()) 

df_deltasnow <- df2023_imp %>% 
	filter(delta_snow != 0)

fit.deltasnow1 <- brm(delta_snow ~ ar(),
										 data = df2023_imp,
										 backend = "cmdstanr",
										 cores = 8, chains = 8,
										 iter = 4000, warmup = 1500)

fit.deltasnow2 <- brm(delta_snow ~ 1 + AIR_TEMPERATURE_MEAN + REL_HUM,
											family = student(),
											data = df_deltasnow,
											backend = "cmdstanr",
											cores = 8, chains = 8,
											iter = 4000, warmup = 1500,
											silent = 0)

priors1 <- c(prior(normal(0, 0.25), class = b))

fit.deltasnow3 <- brm(delta_snow ~ 1 + AIR_TEMPERATURE_MEAN + REL_HUM + AIR_TEMPERATURE_MEAN*REL_HUM,
											family = student(),
											data = df_deltasnow,
											prior = priors1,
											backend = "cmdstanr",
											cores = 8, chains = 8,
											iter = 4000, warmup = 1500,
											silent = 0)

fit.deltasnow4 <- brm(delta_snow ~ 1 + s(REL_HUM)  + s(AIR_TEMPERATURE_MEAN),
											family = student(),
											data = df_deltasnow,
											prior = priors1,
											backend = "cmdstanr",
											cores = 8, chains = 8,
											iter = 4000, warmup = 1500,
											silent = 0)


# Delta snow -main --------------------------------------------------------------

tiedostot <- list.files(here("data", "weather", "tahtela", "meteo", "snow"),
												full.names = T)

aineisto <- vector(mode = "list", 
									 length = length(tiedostot))

deltasnow_func <- function(x = 1){
	df <- read_delim(tiedostot[x],
									 delim = ",",
									 na = c("", "NA", "-")) %>% 
		clean_names() %>% 
		filter(month %in% c("11","12", "01", "02", "03", "04")) %>% 
		mutate(timestamp = ymd_hms(paste0(year, "-", month, "-", day,"-", time_local_time)),
					 snow_depth = case_when(snow_depth_mean_cm == -1 ~ 0,
					 													.default = snow_depth_mean_cm)*10,
					 year = factor(year),
					 month = factor(as.double(month)),
					 day = factor(as.double(day)),
					 time30min = cut(timestamp,
					 								breaks = "30 min"),
					 .keep = "unused") %>% 
		rename("station" = "observation_station") %>% 
		relocate(station,timestamp) %>% 
		slice(4:nrow(.)) %>% 
		mutate(snowdepth = snow_depth,
					 .keep = "unused") %>% 
		fill(snowdepth, .direction = "down") %>% 
		group_by(time30min) %>% 
		summarise(snowdepth30min = mean(snowdepth)) %>% 
		ungroup() %>% 
		mutate(delta_snow = c(0, diff(snowdepth30min)),
					 time_id = seq_along(time30min),
					 winter_id = x,
					 timestamp = as.character(time30min),
					 timestamp = ymd_hms(timestamp))
	return(df)
}

for(i in 1:length(aineisto)){
	aineisto[[i]] <- deltasnow_func(i)
}

df_deltasnow_main <- bind_rows(aineisto)


### kuvat

for(i in 1:13){
	p1 <- df_deltasnow_main  %>% 
		filter(winter_id == i & delta_snow != 0) %>% 
		ggplot(aes(x = factor(winter_id), y = delta_snow)) +
		geom_boxplot() +
		theme_clean() +
		labs(x = paste0(2009+i,"-", 2010+i),
				 y = "Delta",
				 title = paste0("Lumen syvyyden muutos", " ", 2009+i,"-", 2010+i),
				 subtitle = "Delta != 0") +
		ylim(-45,45) +
		geom_hline(yintercept = 0, color = "orange", linetype = "dashed", size = 1) +
		theme(axis.text.x = element_blank())
	
	ggsave(p1,
				 filename = here("output", "plots", paste0("deltasnow-",2009+i, "-", 2010+i,".png")),
				 units = "px",
				 width = 1800,
				 height = 1200)
}

# Malli

dfmain <- df_deltasnow_main %>% 
	filter(delta_snow != 0) 


deltasnow_fit1 <- brm(
	formula = delta_snow ~ 1,
	data = dfmain,
	family = student(),
	chains = 8, cores = 8,
	iter = 4000, warmup = 1000,
	backend = "cmdstanr",
	silent =0
)

dfmain <- df_deltasnow_main %>% 
	filter(delta_snow != 0) %>% 
	mutate( delta_snow_abs = abs(delta_snow)) 

deltasnow_fit2 <- brm(
	formula = delta_snow_abs ~ 1,
	data = dfmain,
	family = Gamma(link = "log"),
	chains = 8, cores = 8,
	iter = 4000, warmup = 1000,
	backend = "cmdstanr",
	silent =0
)

 df_deltasnow_main %>% 
	filter(delta_snow != 0) 


deltasnow_fitnega <- brm(
	formula = delta_snow ~ 1,
	data = df_deltasnow_main %>% 
		filter(delta_snow != 0) ,
	family = student(),
	chains = 8, cores = 8,
	iter = 4000, warmup = 1000,
	backend = "cmdstanr",
	silent =0
)

# Erityistapaus 10.2.2023 -------------------------------------------------

df_main <- read_delim(here("data", "weather", "tahtela", "meteo", "case", "tahtala100220230-11022023.csv"),
											delim = ",",
											na = c("", "NA", "-")) %>% 
	clean_names() %>% 
	mutate(timestamp = ymd_hms(paste0(year, "-", month, "-", day,"-", time_local_time)),
				 .keep = "unused") %>% 
	relocate(timestamp)

df_snower <-  read_excel("data/weather/tahtela/meteo/case/Sodankylä_suodattamaton.xlsx", 
												 col_types = c("date", "numeric", "numeric", 
												 							"numeric", "text", "text")) %>% 
	select(1:2) %>% 
	clean_names()

p_snowdepth <- df_main %>% 
	filter(timestamp > '2023-02-10 11:00:00', timestamp < '2023-02-11 18:00:00') %>% 
	ggplot(aes(x = timestamp, y = snow_depth_cm)) +
	geom_line() +
	theme_clean()

p_wind_speed_m_s <- df_main %>% 
	filter(timestamp > '2023-02-10 11:00:00', timestamp < '2023-02-11 18:00:00') %>% 
	ggplot(aes(x = timestamp, y = wind_speed_m_s)) +
	geom_line() +
	theme_clean()

p_air_temperature_c <- df_main %>% 
	filter(timestamp > '2023-02-10 11:00:00', timestamp < '2023-02-11 18:00:00') %>% 
	ggplot(aes(x = timestamp, y = air_temperature_c)) +
	geom_line() +
	theme_clean()

p_relative_humidity_percent <- df_main %>% 
	filter(timestamp > '2023-02-10 11:00:00', timestamp < '2023-02-11 18:00:00') %>% 
	ggplot(aes(x = timestamp, y = relative_humidity_percent)) +
	geom_line() +
	theme_clean()

p_dew_point_temperature_c <- df_main %>% 
	filter(timestamp > '2023-02-10 11:00:00', timestamp < '2023-02-11 18:00:00') %>% 
	ggplot(aes(x = timestamp, y = dew_point_temperature_c)) +
	geom_line() +
	theme_clean()

p_gust_speed_m_s <- df_main %>% 
	filter(timestamp > '2023-02-10 11:00:00', timestamp < '2023-02-11 18:00:00') %>% 
	ggplot(aes(x = timestamp, y = gust_speed_m_s)) +
	geom_line() +
	theme_clean()

p_snower <- df_snower %>% 
	filter(timestamp > '2023-02-10 11:00:00', timestamp < '2023-02-11 18:00:00') %>% 
	ggplot(aes(x = timestamp, y = snow_depth)) +
	geom_line() +
	geom_hline(yintercept = 65, linetype = "dashed", color = "orange") +
	theme_clean()