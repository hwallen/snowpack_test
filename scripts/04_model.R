packages <- c("tidyverse", "here", "lubridate", "janitor", "ggthemes", "brms", "bayesplot", "cmdstanr")
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
				 													.default = snow_depth_cm),
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
