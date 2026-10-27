
#
# IMPORT ALL DATA ---------------------------------------------------------
import_all_data = function(FILEPATH, PATTERN){
  
  filePaths <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
  dat <- do.call(bind_rows, lapply(filePaths, function(path) {
    df <- read.csv(path, na = c("", "NA", " ", " "))
    df <- df %>% mutate(source = basename(path)) %>% 
      dplyr::select(source, everything())
    
    df[df == " "] <- NA
    df[df == ""] <- NA
    
    df}))
  
  
}

data = 
  import_all_data(FILEPATH = "1-data", PATTERN = ".csv") %>% 
  janitor::clean_names() %>% 
  fill(name, .direction = "down")

data_columns = 
  data %>% 
  dplyr::select(source, name, 
                distribution_g_g_w, total_water_stable_aggregates_53um_6mm_g_g, water_stable_aggregates_mwd, # aggregates
                cole_from_length_cm_cm, #linear extensibility
                quartz, albite, microcline, mica, chlorite, kaolinite, # mineralogy
                p_h, # pH
                s_m2_g, #SSA
                c, n # tctn
  ) %>% 
  mutate(name = str_remove(name, "DLG_")) %>% 
  separate(name, sep = "_", into = c("plot", "BD", "sample"), remove = F) %>% 
  mutate(sample = str_remove(sample, "-"),
         sample = str_extract(sample, "A[0-9]|B[0-9]"),
         horizon = str_extract(sample, "A|B"))


data_long = 
  data_columns %>% 
  dplyr::select(-name) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = -c(source, plot, BD, sample, horizon)) %>% 
  drop_na() %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(source, plot, BD, sample, horizon, name) %>% 
  dplyr::summarise(value = mean(value)) 


data_wide = 
  data_long %>% 
  ungroup() %>% 
  dplyr::select(-source) %>% 
  pivot_wider()

data_long %>% 
  ggplot(aes(x = plot, y = value, color = sample))+
  geom_point(size = 3, width = 0.1)+
  facet_wrap(~source + name, scale = "free_y")+
  scale_color_manual(values = c("pink", "red2", "red4", "lightblue1", "lightblue3", "darkblue"))


#

# IMPORT KSAT -------------------------------------------------------------

import_ksat = function(FILEPATH, PATTERN){
  
  ## CONVERTING ALL XLSX TO CSV FILES
  ## 
  ##S  library(readxl)
  ##S  library(readr)
  ##S  
  ##S  # Find all xlsx files
  ##S  xlsx_files <- list.files(path = "1-data/KSAT", pattern = "\\.xlsx$", full.names = T)
  ##S  
  ##S  # Loop through and save the first sheet as CSV
  ##S  for (file in xlsx_files) {
  ##S    # Read the first sheet
  ##S    df <- read_excel(file, sheet = 1)
  ##S    
  ##S    # Create a new CSV filename
  ##S    csv_name <- gsub("\\.xlsx$", ".csv", file)
  ##S    
  ##S    # Write to CSV
  ##S    write_csv(df, csv_name)
  ##S  }
  ##
  ##

  filePaths <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
  dat <- do.call(bind_rows, lapply(filePaths, function(path) {
    df <- readxl::read_xlsx(path, col_names = F)
    #df <- read.csv(path)
    df <- df %>% 
      dplyr::select(1, 2) %>% 
      mutate(source = basename(path))
    
    df}))
  
  
}
ksat = import_ksat(FILEPATH = "1-data/KSAT/KSAT_converted", PATTERN = ".xlsx")

process_ksat = function(ksat){
  
  ksat_processed = 
    ksat %>% 
    janitor::clean_names() %>% 
    filter(x1 == "Ks Soil [cm/d]") %>% 
    dplyr::select(-x1) %>% 
    rename(Ks_cmd = x2) %>% 
    mutate(source = str_remove(source, "KSAT_measurement_"),
           source = str_remove(source, ".xlsx"),
           source = str_replace(source, "_BD", "-BD"),
           source = str_replace(source, "BD_", "BD-")) %>% 
    separate(source, sep = "-", into = c("plot", "BD", "sample"), remove = F) %>% 
    mutate(sample = str_remove(sample, "_[0-9]{3}"),
           Ks_cmd = as.numeric(Ks_cmd)) %>% 
    mutate(horizon = str_extract(sample, "A|B"))
  
    
}

ksat_processed = process_ksat(ksat)
ksat_processed %>% 
  ggplot(aes(x = plot, y = Ks_cmd, color = horizon))+
  geom_point(size = 5, width = 0.1)+
  facet_wrap(~horizon)+
  labs(y = "Ksat, cm/d")+
  scale_fill_manual(values = c("pink", "red2", "red4", "lightblue2", "lightblue3", "darkblue"))


#
# HYPROP ------------------------------------------------------------------

import_wrc_data = function(FILEPATH){
  
  filePaths_wrc <- list.files(path = FILEPATH, pattern = "xlsx", full.names = TRUE, recursive = FALSE)
  wrc_data <- do.call(bind_rows, lapply(filePaths_wrc, function(path) {
    
    # importing both, the evaluated values and the fitted values
    df_eval <- readxl::read_excel(path, sheet = "Evaluation-Retention Θ(pF)") %>% mutate_all(as.character) %>% janitor::clean_names()
    df_eval = df_eval %>% mutate(source = basename(path)) %>% dplyr::select(h_pa, water_content_vol_percent, source) %>% rename(hPa_eval = h_pa)
    
    df_fit <- readxl::read_excel(path, sheet = "Fitting-Retention Θ(pF)") %>% mutate_all(as.character) %>% janitor::clean_names()
    df_fit = df_fit %>% mutate(source = basename(path)) %>% dplyr::select(h_pa, water_content_vol_percent, source) %>% rename(hPa_fit = h_pa)
    
    df <- full_join(df_eval, df_fit)
    df
  }
  
  ))
  
}
wrc_data = import_wrc_data(FILEPATH = "1-data/HYPROP")
process_wrc = function(wrc_data){
  
  wrc_processed <- 
    wrc_data %>% 
    mutate(source = str_remove(source, ".xlsx")) %>% 
    separate(source, sep = "_", into = c("DLG", "plot", "BD", "sample"), remove = F) %>% 
   # dplyr::select(site, transect, water_content_vol_percent, starts_with("pf")) %>% 
    mutate_at(vars(starts_with("hPa")), as.numeric) %>% 
    mutate_at(vars(starts_with("water")), as.numeric) %>% 
    mutate(horizon = str_extract(sample, "A|B"))

}
import_wrc_parameters = function(FILEPATH){
  
  filePaths_wrc <- list.files(path = FILEPATH, pattern = "xlsx", full.names = TRUE, recursive = FALSE)
  wrc_parameters <- do.call(bind_rows, lapply(filePaths_wrc, function(path) {
    
    df_parameters <- readxl::read_excel(path, sheet = "Fitting-Parameter value") %>% mutate_all(as.character) %>% janitor::clean_names()
    df_parameters = df_parameters %>% mutate(source = basename(path)) %>% dplyr::select(parameter, value, source)
    
    
  }
  ))
  
  parameters_processed <- 
    wrc_parameters %>% 
    mutate(source = str_remove(source, ".xlsx"),
           value = parse_number(value)) %>% 
    separate(source, sep = "_", into = c("DLG", "plot", "BD", "sample"), remove = F) %>% 
    filter(parameter %in% c("alpha", "n", "th_r", "th_s"))  %>% 
    pivot_wider(names_from = "parameter", values_from = "value") %>% 
    mutate(m = 1 - (1/n),
           #transect = case_match(transect, "upland" ~ "UP", "transition" ~ "TR", "wetland" ~ "W")
    ) %>% 
    rename(
      theta_r = th_r,
      theta_s = th_s) 
  
  #parameters_processed %>% write.csv("1-data/wrc/wrc_msm_mcdowell/wrc_parameters_all_sites.csv", row.names = F, na = "")
  
}
wrc_parameters = import_wrc_parameters(FILEPATH = "1-data/HYPROP")

wrc_processed = process_wrc(wrc_data)
wrc_processed %>% 
  filter(hPa_fit >= 0 | hPa_eval >= 0) %>% 
  ggplot(aes(y = water_content_vol_percent, color = sample))+
  geom_line(aes(x = hPa_fit))+
  geom_point(aes(x = hPa_eval), shape = 1)+
  scale_x_log10(labels = scales::comma)+
 # scale_color_manual(values = rev(soilpalettes::soil_palette("redox2", 3)))+
  labs(x = "Water potential, hPa",
       y = "Water content, % v/v",
       color = "")+
  #    facet_wrap(~region + site)+
  facet_wrap( ~ plot + horizon)+
  scale_color_manual(values = c("pink", "red2", "red4", "lightblue2", "lightblue3", "darkblue"))+

  theme_kp()


# plot mineralogy ----
mineralogy = 
  data_long %>% 
  filter(source == "Mineralogy.csv")

mineralogy_summary = 
  mineralogy %>% 
  group_by(plot, horizon, name) %>% 
  dplyr::summarise(mean = mean(value))


mineralogy %>% 
  ggplot(aes(x = sample, y = value, fill = name))+
  geom_bar(stat = "identity")+
  labs(x = NULL,
       y = "fraction")+
  facet_wrap(~plot)

mineralogy_summary %>% 
  ggplot(aes(x = plot, y = mean, fill = name))+
  geom_bar(stat = "identity")+
  labs(x = NULL,
       y = "fraction")+
  facet_wrap(~horizon)



data_long %>% 
  filter(source %in% "pH.csv") %>% 
  ggplot(aes(x = plot, y = value, color = horizon))+
  geom_point(size = 4)+
  labs(y = "pH")+
  facet_wrap(~horizon)

data_long %>% 
  filter(source %in% "TCTN.csv" & name == "c") %>% 
  ggplot(aes(x = plot, y = value, color = horizon))+
  geom_point(size = 4)+
  labs(y = "Total Carbon, %")+
  facet_wrap(~horizon)

data_long %>% 
  filter(source %in% "TCTN.csv" & name == "n") %>% 
  ggplot(aes(x = plot, y = value, color = horizon))+
  geom_point(size = 4)+
  labs(y = "Total Nitrogen, %")+
  facet_wrap(~horizon)

data_long %>% 
  filter(source %in% "Specific_surface_area.csv") %>% 
  filter(value < 500000) %>% 
  ggplot(aes(x = plot, y = value, color = horizon))+
  geom_point(size = 4)+
  labs(y = "SSA, m2/g")+
  facet_wrap(~horizon)

data_long %>% 
  filter(source %in% "Linear_extensibility.csv") %>% 
  ggplot(aes(x = plot, y = value, color = horizon))+
  geom_point(size = 4)+
  labs(y = "COLE, cm/cm")+
  facet_wrap(~horizon)

