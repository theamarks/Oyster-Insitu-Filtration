library(tidyverse)
library(magrittr)
library(hms)
library(wesanderson)

######### Read in test file - sub in for list of files ##########
# aTimeSeriesFile <- Insitu_Filter_NPD_41519 <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_NPD_41519.csv")
#aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_MB_72718.csv")
#aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_NPD_51019.csv")
aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_NPD_6919.csv")

################### Test Correcting graph labels & consistent color asignment ###################
aFile_Mod = aTimeSeriesFile %<>%
  dplyr::mutate(Time = as.hms(Time),
                Experiment = ifelse(Experiment %in% c("sbs_after", "sbs_before", "Filtration"),
                                    Experiment, "Filtration"),
                Experiment_Title = paste0('aFileName', " - ", Experiment))

legend_info <- aFile_Mod %>%
  dplyr::select(Sonde, Position) %>%
  dplyr::distinct() %>%
  dplyr::mutate(legend = paste0(Position, " ", Sonde)) 

(one_plot = ggplot(data = aFile_Mod, aes(x = Time, y = Chl_ug_L, group = Position, color = Position)) +
                                                                # ^^^ Changed Sonde to Position ^^^
  geom_line(size = .5) +
  scale_color_manual(values = ifelse(legend_info$Position == 'Down',
                                     wes_palette("Cavalcanti1")[3], # specify color asigned to 'Down'
                                     wes_palette("Cavalcanti1")[2]), # specify color asigned to 'Up"
                     labels = ifelse(legend_info$Position == 'Down',
                                     legend_info[legend_info$Position == 'Down',3], # read cell in 3rd column -"legend'
                                     legend_info[legend_info$Position == 'Up',3])) +
  facet_wrap(~Experiment_Title, nrow = 1, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank()) +
  labs(x = ""))

#################### Test function guts - Graph Filtraiton after time adjustment #################
aTimeSeriesFile <- read_csv("output/4_Filtration_Calculations/Insitu_Filter_NPD_41519.csv")

aFile_Mod = aTimeSeriesFile %<>%
  dplyr::select(Time, Date, Site, Experiment, Sonde_Up, Chl_ug_L_Up,
                Sonde_Down, Chl_ug_L_Down, pcnt_Chl_rmvd, L_hr_m2) %>% 
  dplyr::mutate(Time = as.hms(Time),
                Experiment = ifelse(Experiment %in% c("sbs_after", "sbs_before", "Filtration"),
                                    Experiment, "Negative Control"),
                Experiment_Title = paste0('aFileName', " - ", Experiment)) %>% 
  gather(Chl_ug_L_Up, Chl_ug_L_Down, key = Position, value = Chl_ug_L)

legend_info <- aFile_Mod %>%
  dplyr::select(Sonde_Up, Sonde_Down) %>%
  dplyr::distinct() %>%
  gather(Sonde_Up, Sonde_Down, key = Position, value = Sonde) %>% 
  dplyr::mutate(legend = ifelse(Position == "Sonde_Up", paste0('Up', ' ', Sonde),
                                ifelse(Position == "Sonde_Down", paste0('Down', ' ', Sonde), NA)))

(one_plot = ggplot(data = aFile_Mod, aes(x = Time, y = Chl_ug_L, group = Position, color = Position)) +
    # ^^^ Changed Sonde to Position ^^^
    geom_line(size = .5) +
    scale_color_manual(values = ifelse(legend_info$Position == 'Down',
                                       wes_palette("Cavalcanti1")[3], # specify color asigned to 'Down'
                                       wes_palette("Cavalcanti1")[2]), # specify color asigned to 'Up"
                       labels = ifelse(legend_info$Position == 'Down',
                                       legend_info[legend_info$Position == 'Down',3], # read cell in 3rd column -"legend'
                                       legend_info[legend_info$Position == 'Up',3])) +
    facet_wrap(~Experiment_Title, nrow = 1, scales = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank()) +
    labs(x = ""))
