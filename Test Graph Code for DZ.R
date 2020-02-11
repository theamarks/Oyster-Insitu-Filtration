library(tidyverse)
library(magrittr)
library(hms)
library(wesanderson)
library(stringr)
library(ggthemes)

######## Test reordering graphs by time ########
aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_NPD_41519.csv")

aFile_Mod = aTimeSeriesFile %<>%
  dplyr::mutate(Time = as.hms(Time),
                Experiment = ifelse(Experiment %in% c("sbs_after", "sbs_before", "Filtration"), Experiment, "Neg_Control"),
               # Experiment_Title = paste0("NPD_41519", " - ", Experiment),
               legend_title = paste0(Position, ' ', Sonde)) %>%  # combine columns for title
      transform(Experiment = factor(Experiment, levels = c("sbs_before", "Filtration","Neg_Control", "sbs_after")))

trial_names <- c("sbs_before" = paste0("sbs_before", ' - ', "NPD"), # change "NPD" to aFileName in function
                 "Filtration" = paste0("Filtration", ' - ', "NPD"),
                 "sbs_after" = paste0("sbs_after", ' - ', "NPD"),
                 "Neg_Control" = paste0("Neg_Control", ' - ', (aFileName %>% str_replace("Insitu_FIlter_", "") %>% 
                                                                 str_replace(".csv", ""))))

one_plot = ggplot(data = aFile_Mod, aes(x = Time, y = Chl_ug_L, group = Position, color = legend_title)) +
  geom_line(size = 1) +
  geom_point() +
  scale_color_manual(values = c("Down G" = wes_palette("Cavalcanti1")[3], # assign Down to dark green
                                "Down H" = wes_palette("Cavalcanti1")[3],
                                "Up G" = wes_palette("Cavalcanti1")[2], # assign Up to light green
                                "Up H" = wes_palette("Cavalcanti1")[2])) +
  facet_wrap(.~Experiment, nrow = 1, scales = "free_x", labeller = as_labeller(trial_names)) + # scales free - independent x axis on each graph
  theme_gdocs() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank()) +
  labs(x = "")

one_plot

######## Test sbs time match ########

aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_NPD_41519.csv")

#aTimeSeriesFile <- as.data.frame(aTimeSeriesFile)

correction_check = aTimeSeriesFile %>%
  dplyr::filter(Experiment %in% c("sbs_before", "sbs_after")) %>% # all sbs values used in correction
  select(Time, Chl_ug_L, Position) %>% 
  tidyr::pivot_wider(names_from = Position, values_from = Chl_ug_L) %>%  # shows implicit missing data as NA (missing Chl measurements / time stamps)
 # replacement for tidyr::spread() - takes Chl data in single column and spreads it to two columns based on position (UP and DOwn)
# This pairs measurements by Time
  filter(!is.na(Up) & !is.na(Down)) %>% # select rows with data in Up and Down
  dplyr::summarise(Avg_Chl_Up = mean(Up),
                   Avg_Chl_Down = mean(Down))

down_chl = correction_check$Avg_Chl_Down
up_chl = correction_check$Avg_Chl_Up

################# Test Ploting Chl Diff, time adjusted #################
aTimeSeriesFile <- read_csv("output/4_Filtration_Calculations/Insitu_Filter_NPD_2019_4_15.csv")

  aFile_Mod = aTimeSeriesFile %<>%
    select(Time, Date, Site, Experiment, Turbidity_NTU_Up, Chl_ug_L_Up, Chl_ug_L_Down) %>% 
    mutate(Chl_diff = Chl_ug_L_Up - Chl_ug_L_Down,
           Time = as_hms(Time))
  
  (one_plot = ggplot(data = aFile_Mod, aes(x = Time, y = Chl_diff)) +
    geom_path(size = 1, color = wes_palette("GrandBudapest1")[3]) +
    geom_point(color = wes_palette("GrandBudapest1")[3]) +
   # scale_x_continuous(breaks = seq(min(aFile_Mod$Time), max(aFile_Mod$Time), 5)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank()) +
    geom_hline(yintercept = 0, size = 1, color = "grey50", linetype = "dashed")+
    labs(x = "", y = "Chl Difference (ug/L)", title = "title") )


  ######### Read in test file - sub in for list of files ##########
  aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_NPD_41519.csv")
  aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_MB_72718.csv")
  aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_NPD_51019.csv")
  aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_NPD_6919.csv")
  
################### Test Correcting graph labels & consistent color asignment ###################
aFile_Mod = aTimeSeriesFile %<>%
  dplyr::mutate(Time = as.hms(Time),
                Experiment = ifelse(Experiment %in% c("sbs_after", "sbs_before", "Filtration"),
                                    Experiment, "Filtration"),
                Experiment_Title = paste0('aFileName', " - ", Experiment),
                legend_title = paste0(Position, ' ', Sonde))

#legend_info <- aFile_Mod %>%
 # dplyr::select(Sonde, Position) %>%
  #dplyr::distinct() %>%
#  dplyr::mutate(legend = paste0(Position, " ", Sonde)) 

#graph_colors <- c("Down" = wes_palette("Cavalcanti1")[3],
                #  "Up" = wes_palette("Cavalcanti1")[2])

####### Posted question on StackOverflow about how to assing color values - This is the most concise answer
(one_plot = ggplot(data = aFile_Mod, aes(x = Time, y = Chl_ug_L, color = legend_title)) +
  geom_line(size = 1) +
  geom_point() +
  #scale_color_manual(values = c((grep("^Down", legend_title, value = TRUE)) = wes_palette("Cavalcanti1")[3],
                               # (grep("^UP", legend_title, value = TRUE)) = wes_palette("Cavalcanti1")[2])) +
  #scale_color_manual(values = c("Down[ ]" == wes_palette("Cavalcanti1")[3], "Up[ ]" == wes_palette("Cavalcanti1")[2])) +
  scale_color_manual(values = c("Down G" = wes_palette("Cavalcanti1")[3],
                                "Down H" = wes_palette("Cavalcanti1")[3],
                                "Up G" = wes_palette("Cavalcanti1")[2],
                                "Up H" = wes_palette("Cavalcanti1")[2])) +
    
  #scale_color_manual(values = unique(aFile_Mod$color_position), labels = unique(aFile_Mod$legend_title)) +
  #scale_color_identity(guide = "legend") +
  
  #scale_color_manual(values = ifelse(Position == 'Down',
                                    # wes_palette("Cavalcanti1")[3], # specify color asigned to 'Down'
                                    # wes_palette("Cavalcanti1")[2])) + # specify color asigned to 'Up"
                  #   labels = ifelse(lePosition == 'Down',
                   #                  legend_info[legend_info$Position == 'Down',3], # read cell in 3rd column -"legend'
                    #                 legend_info[legend_info$Position == 'Up',3])) +
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
  tidyr::gather(Chl_ug_L_Up, Chl_ug_L_Down, key = Position, value = Chl_ug_L)

legend_info <- aFile_Mod %>%
  dplyr::select(Sonde_Up, Sonde_Down) %>%
  dplyr::distinct() %>%
  tidyr::gather(Sonde_Up, Sonde_Down, key = data_name, value = Sonde) %>% 
  dplyr::mutate(legend = ifelse(data_name == "Sonde_Up", paste0('Up', ' ', Sonde),
                                ifelse(data_name == "Sonde_Down", paste0('Down', ' ', Sonde), NA)))

(one_plot = ggplot(data = aFile_Mod, aes(x = Time, y = Chl_ug_L, group = Position, color = Position)) +
    geom_line(size = .5) +
    scale_color_manual(values = ifelse(Position == "Chl_ug_L_Down",
                                       wes_palette("Cavalcanti1")[3], # specify color asigned to 'Down'
                                       wes_palette("Cavalcanti1")[2])) + # specify color asigned to 'Up"
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank()) +
    labs(x = ""))
