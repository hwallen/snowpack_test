packages <- c("tidyverse", "here", "sarp.snowprofile", "sf")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = T)
rm(packages)


# Profile files -----------------------------------------------------------

tiedostot <- list.files(here("snowpack", "output_snow"), # here-funktiolla on helppo rakentaa tiedostopolku-viittaus
												full.names = T,
												pattern = ".pro") ## .pro tiedostot

pro_example <- snowprofilePro(tiedostot[1]) # Ladataan ensimmäinen tiedosto

df_pro <- rbind(pro_example) %>% ## Yhdistää listan allekkain
	as_tibble() %>% ## muuttaa dataframen kätevämmin käsiteltäväksi
	mutate(station = "Sodankylä Tähtelä") %>% 
	select(-station_id) %>% 
	group_by(datetime) %>% ## Ryhmittelee aineiston datetime-muuttujan mukaan
	mutate(layer_num = seq_along(datetime)) %>% # Luo muuttujan "layer_num" samaa ajankohtaa koskevien rivien perusteella
	ungroup() %>% ## Poistaa ryhmittelyn
	relocate(station, datetime, date, layer_num) %>% 
	st_as_sf(coords = c("lon", "lat"), 
					 crs = 4326) %>% ## Muuttaa dataframen paikkatiedoksi; crs (WGS84)
	st_transform(crs = 3067) ## Muuttaa koordinaattijärjestelmän ETRS89/TM35FIN -muotoon.


st_write(df_pro,
				 "tahtela_esimerkki.geojson")
