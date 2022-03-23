library(tidyverse)
library(magrittr)
library(hms)
library(wesanderson)
library(stringr)
library(ggthemes)
library(ggpubr) # ggtexttable() to transform dataframe to text table for graphing
library(cowplot) # layering multiple plots into single plot
library(broom) # turning t test output into a table

# viridis package - extra palatte Cividis

### replace negative chl values with 0s
neg_chl_to_zero = function(aTimeSeriesFile)
{
  neg_chl_fixed = aTimeSeriesFile %>% 
    mutate(Chl_ug_L = ifelse(Chl_ug_L < 0, 0, Chl_ug_L))
  return(neg_chl_fixed)
}

test <- read_csv("output/1_Cleaned_Data/Insitu_Filter_NPD_2019_4_17.csv")
#test$Chl_ug_L[test$Chl_ug_L < 0] <-0


test1 = neg_chl_to_zero(test)


Master_Filtration_analysis_table <- read_csv("output/4_Filtration_Calculations/Filtration_Summary/Master_Filtration_analysis_table.csv")


# Negative selection water summary table ----------------------------------
aFiltrationFile <- read_csv("output/4_Filtration_Calculations/Insitu_Filter_MB_2019_5_19.csv")

one_water_vel_summary = water_vel_summary %>%
  dplyr::filter(Date %in% unique(aFiltrationFile$Date)) %>%
  dplyr::select(-c("reliable_data", "data_possible_influence"))

# T-test results in table -------------------------------------------------
aFiltrationFile <- read_csv("output/4_Filtration_Calculations/Insitu_Filter_MB_2019_5_19.csv")

data_only_numeric = dplyr::select_if(aFiltrationFile, is.numeric)

filtration_sub_df =  aFiltrationFile %>% 
  dplyr::select(c(names(data_only_numeric), "Experiment", "Date", "Site"))

#up_down_PairedTtest = filtration_sub_df %>% # Error: Error: C stack usage  7969552 is too close to the limit
 # tidy(t.test(Chl_ug_L_Up, Chl_ug_L_Down, paired = T)) # Paired T-test, output is a 1x8 table

up_down_PairedTtest <- tidy(t.test(filtration_sub_df$Chl_ug_L_Up, filtration_sub_df$Chl_ug_L_Down, paired = T))
up_down_onetailT <- tidy(t.test(filtration_sub_df$Chl_ug_L_Up, filtration_sub_df$Chl_ug_L_Down, alternative = "greater"))
Chl_diff_t <- tidy(t.test(filtration_sub_df$Chl_diff, alternative = "greater"))

filtration_sub_df_sum = filtration_sub_df %>%
  dplyr::filter_if(~is.numeric(.), all_vars(!is.infinite(.))) %>%
  dplyr::group_by(Experiment, Date, Site) %>%
  dplyr::summarise(Temp_C_Up = mean(Temp_C_Up),
                   SpCond_mS_cm_Up = mean(SpCond_mS_cm_Up),
                   Cond_mS_cm_Up = mean(Cond_mS_cm_Up),
                   TDS_g_L_Up = mean(TDS_g_L_Up),
                   Sal_ppt_Up = mean(Sal_ppt_Up),
                   Turbidity_NTU_Up = mean(Turbidity_NTU_Up),
                   Chl_ug_L_Up = mean(Chl_ug_L_Up),
                   # Chl_ug_L_Up_SD = sd(Chl_ug_L_Up), # Not sure SD is appropriate
                   Temp_C_Down = mean(Temp_C_Down),
                   SpCond_mS_cm_Down = mean(SpCond_mS_cm_Down),
                   Cond_mS_cm_Down = mean(Cond_mS_cm_Down),
                   TDS_g_L_Down = mean(TDS_g_L_Down),
                   Sal_ppt_Down = mean(Sal_ppt_Down),
                   Turbidity_NTU_Down = mean(Turbidity_NTU_Down),
                   Chl_ug_L_Down = mean(Chl_ug_L_Down),
                   # Chl_ug_L_Down_SD = sd(Chl_ug_L_Down), # snot sure SD is appropriate
                   avg_depth_cm = mean(avg_depth_cm),
                   d_bw_sondes_m = mean(d_bw_sondes_m),
                   avg_m_hr = mean(avg_m_hr),
                   pcnt_Chl_rmvd = mean(pcnt_Chl_rmvd),
                   L_hr_m2 = mean(L_hr_m2)
  ) %>%
  data.frame() %>%
  # dplyr::mutate_if(is.numeric, round, 3) %>% # Tried to match excel numbers
  dplyr::mutate(File_Name = aFileName) %>%
  dplyr::select(File_Name, Experiment, everything())

filtration_df_Ttest = cbind(filtration_sub_df_sum, up_down_PairedTtest)

return(filtration_df_Ttest)



# Water Quality Graphs ----------------------------------------------------
aTimeSeriesFile <- read_csv("output/4_Filtration_Calculations/Insitu_Filter_SR_2018_11_8.csv")
aTimeSeriesFile <- read_csv("output/4_Filtration_Calculations/Insitu_Filter_NPD_2019_5_9.csv")
# Chl - from Sbs corrected data??
# Chl y-axis bounds
Chl_ymax <- ifelse(max(aTimeSeriesFile$Chl_ug_L_Up) > max(aTimeSeriesFile$Chl_ug_L_Down), 
                   max(aTimeSeriesFile$Chl_ug_L_Up), max(aTimeSeriesFile$Chl_ug_L_Down))
Chl_ymin <- ifelse(min(aTimeSeriesFile$Chl_ug_L_Up) < min(aTimeSeriesFile$Chl_ug_L_Down), 
                   min(aTimeSeriesFile$Chl_ug_L_Up), min(aTimeSeriesFile$Chl_ug_L_Down))
# Turbidity y-axis bounds
Turb_ymax <- ifelse(max(aTimeSeriesFile$Turbidity_NTU_Up) > max(aTimeSeriesFile$Turbidity_NTU_Down), 
                    max(aTimeSeriesFile$Turbidity_NTU_Up), max(aTimeSeriesFile$Turbidity_NTU_Down))
Turb_ymin <- ifelse(min(aTimeSeriesFile$Turbidity_NTU_Up) < min(aTimeSeriesFile$Turbidity_NTU_Down), 
                    min(aTimeSeriesFile$Turbidity_NTU_Up), min(aTimeSeriesFile$Turbidity_NTU_Down))
# Temp y-axis bounds
Temp_ymax <- ifelse(max(aTimeSeriesFile$Temp_C_Up) > max(aTimeSeriesFile$Temp_C_Down), 
                    max(aTimeSeriesFile$Temp_C_Up), max(aTimeSeriesFile$Temp_C_Down))
Temp_ymin <- ifelse(min(aTimeSeriesFile$Temp_C_Up) < min(aTimeSeriesFile$Temp_C_Down), 
                    min(aTimeSeriesFile$Temp_C_Up), min(aTimeSeriesFile$Temp_C_Down))
# Salinity y-axis bounds
Sal_ymax <- ifelse(max(aTimeSeriesFile$Sal_ppt_Up) > max(aTimeSeriesFile$Sal_ppt_Down), 
                   max(aTimeSeriesFile$Sal_ppt_Up), max(aTimeSeriesFile$Sal_ppt_Down))
Sal_ymin <- ifelse(min(aTimeSeriesFile$Sal_ppt_Up) < min(aTimeSeriesFile$Sal_ppt_Down), 
                   min(aTimeSeriesFile$Sal_ppt_Up), min(aTimeSeriesFile$Sal_ppt_Down))
                    
(Chl_plot_Up <- ggplot(data = aTimeSeriesFile, aes(x = Time, y = Chl_ug_L_Up)) +
  geom_path(size = 1, color = wes_palette("Cavalcanti1")[2]) +
  geom_point(color = wes_palette("Cavalcanti1")[2]) +
  theme_gdocs() +
  coord_cartesian(ylim = c(Chl_ymin, Chl_ymax)) +
  theme(axis.text.x = element_blank(), # remove x axis text
        axis.title.x = element_blank(), # removed x axis title
        legend.title = element_blank(), # remove legend
        rect = element_blank()) + # removed black boarder rectangle 
  theme(plot.title = element_text(hjust = 0.5)) + # Center title
 # geom_hline(yintercept = 0, size = 1, color = "grey50", linetype = "dashed") + # adds diff line at y = 0
  labs(title = "Upstream",
       y = expression(paste("Chlorophyll ", alpha, " (", mu, "g/L) ")))
)

# Turbidity Up 
(Turb_plot_Up <- ggplot(data = aTimeSeriesFile, aes(x = Time, y = Turbidity_NTU_Up)) +
    geom_path(size = 1, color = wes_palette("Royal1")[4]) +
    geom_point(color = wes_palette("Royal1")[4]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Turb_ymin, Turb_ymax)) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          rect = element_blank(),
          legend.title = element_blank()) +
  #  geom_hline(yintercept = 0, size = 1, color = "grey50", linetype = "dashed") + # adds diff line at y = 0
    labs(y = "Turbidity NTU")
)

(Temp_plot_Up <- ggplot(data = aTimeSeriesFile, aes(x = Time, y = Temp_C_Up)) +
    geom_path(size = 1, color = wes_palette("Zissou1")[1]) +
    geom_point(color = wes_palette("Zissou1")[1]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Temp_ymin, Temp_ymax)) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          rect = element_blank(),
          legend.title = element_blank()) +
    #  geom_hline(yintercept = 0, size = 1, color = "grey50", linetype = "dashed") + # adds diff line at y = 0
    labs(y = paste("Temperature ", "(", intToUtf8(176), "C)"))
)

(Sal_plot_Up <- ggplot(data = aTimeSeriesFile, aes(x = Time, y = Sal_ppt_Up)) +
    geom_path(size = 1, color = wes_palette("GrandBudapest1")[2]) +
    geom_point(color = wes_palette("GrandBudapest1")[2]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Sal_ymin, Sal_ymax)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          rect = element_blank(),
          legend.title = element_blank()) +
    #  geom_hline(yintercept = 0, size = 1, color = "grey50", linetype = "dashed") + # adds diff line at y = 0
    labs(y = "Salinity (ppt)")
)

Up_WQ <- plot_grid(Chl_plot_Up, Turb_plot_Up, Temp_plot_Up, Sal_plot_Up,
          nrow = 4)

###### WQ Downstream
(Chl_plot_Down <- ggplot(data = aTimeSeriesFile, aes(x = Time, y = Chl_ug_L_Down)) +
    geom_path(size = 1, color = wes_palette("Cavalcanti1")[3]) +
    geom_point(color = wes_palette("Cavalcanti1")[3]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Chl_ymin, Chl_ymax)) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          rect = element_blank()) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Downstream", title = "Test")
)

#labs(title = paste0(unique(aTimeSeriesFile$Experiment)), #, ' - ', aFileName %>% 
     # str_replace("Insitu_Filter_", "") %>%
     #  str_replace(".csv", "")),

# Turbidity Down 
(Turb_plot_Down <- ggplot(data = aTimeSeriesFile, aes(x = Time, y = Turbidity_NTU_Down)) +
    geom_path(size = 1, color = wes_palette("Royal1")[4]) +
    geom_point(color = wes_palette("Royal1")[4]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Turb_ymin, Turb_ymax)) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          rect = element_blank(),
          legend.title = element_blank()) 
    #  geom_hline(yintercept = 0, size = 1, color = "grey50", linetype = "dashed") + # adds diff line at y = 0
    #labs(y = "Turbidity NTU")
)

(Temp_plot_Down <- ggplot(data = aTimeSeriesFile, aes(x = Time, y = Temp_C_Down)) +
    geom_path(size = 1, color = wes_palette("Zissou1")[1]) +
    geom_point(color = wes_palette("Zissou1")[1]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Temp_ymin, Temp_ymax)) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          rect = element_blank(),
          legend.title = element_blank()) 
    #  geom_hline(yintercept = 0, size = 1, color = "grey50", linetype = "dashed") + # adds diff line at y = 0
    #labs(y = paste("Temperature ", "(", intToUtf8(176), "C)"))
)

(Sal_plot_Down <- ggplot(data = aTimeSeriesFile, aes(x = Time, y = Sal_ppt_Down)) +
    geom_path(size = 1, color = wes_palette("GrandBudapest1")[2]) +
    geom_point(color = wes_palette("GrandBudapest1")[2]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Sal_ymin, Sal_ymax)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          rect = element_blank(),
          legend.title = element_blank()) 
    #  geom_hline(yintercept = 0, size = 1, color = "grey50", linetype = "dashed") + # adds diff line at y = 0
    #labs(y = "Salinity (ppt)")
)

# combine all downstream graphs in vertical order
Down_WQ <- plot_grid(Chl_plot_Down, Turb_plot_Down, Temp_plot_Down, Sal_plot_Down,
          nrow = 4)
# combine upstream and downstream WQ graphs in two columns
(All_WQ <- plot_grid(Up_WQ, Down_WQ, ncol = 2)
)

# Confidence Intervals ( Not Finished )----------------------------------------------------
(distrubution = aTimeSeriesFile %>% 
   dplyr::filter(!grepl('sbs_before|sbs_after', Experiment)) %>% # selects FIltraiton or Neg Control
   select(Time, Chl_ug_L, Position, Experiment) %>% 
   tidyr::pivot_wider(names_from = Position, values_from = Chl_ug_L) %>% # spread data to remove NAs
   filter(!is.na(Up) & !is.na(Down)) %>% # remove NAs
   pivot_longer(c("Up", "Down"), names_to = "Position", values_to = "Chl_ug_L") # make data longer again for graphs
)

(graph_things <- distrubution %>% 
  group_by(Position) %>% 
  summarise(Avg_Chl = mean(Chl_ug_L))
)

(afileFilterStats = aTimeSeriesFile %>% 
    dplyr::filter(!grepl('sbs_before|sbs_after', Experiment)) %>% # # selects FIltraiton or Neg Control
    select(Time, Chl_ug_L, Position) %>% # select data relevent to sbs 
    tidyr::pivot_wider(names_from = Position, values_from = Chl_ug_L) %>% # create two new columns Up & Down fill with Chl values, paired by time
    filter(!is.na(Up) & !is.na(Down)) %>%  # select rows with data in Up and Down
  #  group_by(Position) %>% 
    summarise(n = length(Time),
              Mean_Up = mean(Up),
              Mean_Down = mean(Down),
              SD_Up = sd(Up), 
              SD_Down = sd(Down),
              CI_Up = qnorm(0.975)*(Mean_Up/sqrt(n)),
              CI_Down = qnorm(0.975)*(Mean_Down/sqrt(n)),
              Upper_CI_Up = Mean_Up + CI_Up,
              Lower_CI_Up = Mean_Up - CI_Up,
              Upper_CI_Down = Mean_Down + CI_Down,
              Lower_CI_Down = Mean_Down - CI_Down)
)

#correction_check[which(correction_check$Position == "Down"), "Avg_Chl"]$Avg_Chl

(distrubution_plot <- ggplot(data = distrubution, aes(x = Chl_ug_L)) +
    geom_freqpoly(aes(color = Position), binwidth = 0.1, size = 1) +
    scale_color_manual(values = c("Down" = wes_palette("Cavalcanti1")[3],
                                  "Up" = wes_palette("Cavalcanti1")[2])) +
    theme_gdocs() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_vline(xintercept =  graph_things[which(graph_things$Position == "Down"), "Avg_Chl"]$Avg_Chl, 
               color = wes_palette("Cavalcanti1")[3], linetype = "dashed", size = .75) +
    geom_vline(xintercept = graph_things[which(graph_things$Position == "Up"), "Avg_Chl"]$Avg_Chl,
               color = wes_palette("Cavalcanti1")[2], linetype = "dashed", size = .75) +
    geom_rect(xmin = afileFilterStats$Lower_CI_Up, xmax = afileFilterStats$Upper_CI_Up,
              ymin = 0, ymax = count(distrubution[which(distrubution$Position == "Up"), "Chl_ug_L"])) 
    # add Mean Down next to dashed line
  #  annotate("text", x = Sbs_stat_summary$Mean_Chl_Down - 0.05, 
   #          y = 0, label = bquote(bar(x) ~ .(round(Sbs_stat_summary$Mean_Chl_Down, 2))),
    #         vjust = "top", hjust = "right", color = wes_palette("Cavalcanti1")[3]) +
    # add mean Up next to dashed line
  #  annotate("text", x = Sbs_stat_summary$Mean_Chl_Up - 0.05, 
   #          y = Inf, label = bquote(bar(x) ~ .(round(Sbs_stat_summary$Mean_Chl_Up, 2))),
    #         vjust = "top", hjust = "right", color = wes_palette("Cavalcanti1")[2]) +
  #  labs(title = paste0(unique(aFile_Mod$Experiment), ' - ',
   #                     aFileName %>% str_replace("Insitu_Filter_", "") %>%
    #                      str_replace(".csv", ""))))
)
  
  aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_NPD_2019_4_15.csv")
  aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_MB_2018_7_27.csv")
  aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_NPD_2019_5_10.csv")
  aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_NPD_2019_6_9.csv") # Neg Control
  
# Test Upstream and Downstream Correlated? --------------------------------
(distrubution = aTimeSeriesFile %>% 
   dplyr::filter(!grepl('sbs_before|sbs_after', Experiment)) %>% # selects FIltraiton or Neg Control
   select(Time, Chl_ug_L, Position, Experiment) %>% 
   tidyr::pivot_wider(names_from = Position, values_from = Chl_ug_L) %>% # spread data to remove NAs
   filter(!is.na(Up) & !is.na(Down))# remove NAs
)

(corr_plot <- distrubution %>% 
  ggplot(aes(x = Up, y = Down)) +
  geom_point() +
  geom_smooth(method = lm) +
  geom_vline(xintercept = mean(distrubution$Up), 
               color = wes_palette("Cavalcanti1")[2] , linetype = "dashed", size = .75) +
  geom_hline(yintercept = mean(distrubution$Down),
             color = wes_palette("Cavalcanti1")[3] , linetype = "dashed", size = .75)
)

corr_test <- lm(distrubution$Down ~ distrubution$Up)
summary(corr_test)
var(distrubution$Up, distrubution$Down) # varience - why two variables?
cov(distrubution$Up, distrubution$Down) # covarrience 
cor(distrubution$Up, distrubution$Down) # Correlation range -1 (perfect inverse), 0 no relation, +1 (perfect positive)


# Calculate Uncertainty  --------------------------------------------------
(distrubution = aTimeSeriesFile %>% 
    dplyr::filter(!grepl('sbs_before|sbs_after', Experiment)) %>% # selects FIltraiton or Neg Control
    select(Time, Chl_ug_L, Position, Experiment) %>% 
    tidyr::pivot_wider(names_from = Position, values_from = Chl_ug_L) %>% # spread data to remove NAs
    mutate(Abs_UC_ug_L = 0.1,
           Up_Down_ug_L = Up - Down, # Upstream minus Downstream
           Abs_UC_Up_Down = Abs_UC_ug_L + Abs_UC_ug_L, # Absolute uncertainty of Upstream minus Downstream
           Rel_UC_Up_Down = ((Abs_UC_Up_Down/abs(Up_Down_ug_L)) * 100), # Relative uncertainty of Up - Down
           Rel_UC_Up = ((Abs_UC_ug_L / Up) * 100), # Relative uncertainty of Upstream ug/L
           ChlRmd_ug_L = ((Up - Down) / Up), # Chlorophyll removed 
           Rel_UC_ChlRmd = Rel_UC_Up_Down + Rel_UC_Up, # Relative uncertainty for Chl removed
           Per_ChlRmd = ChlRmd_ug_L *100, # Percent Chl removed
           Rel_UC_Per_ChlRmd = Rel_UC_ChlRmd # Relative uncertainty for percent chl rmoved - think this is same because multiplying by 100 isn't a constant
           ) %>% 
   filter(!is.na(Up) & !is.na(Down)) #, 
)

(UC_summary <- distrubution %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.))) %>%
  summarise(Avg_Down = mean(Down),
            Avg_Up = mean(Up),
            Avg_Diff = mean(Up_Down_ug_L),
            Avg_Rel_UC_Diff = mean(Rel_UC_Up_Down),
            Avg_Per_ChlRmd = mean(Per_ChlRmd),
            Avg_Rel_UC_ChlRmd = mean(Rel_UC_ChlRmd))
)



# Test Error Funciton -------------------------------------------------------------------------------
# SD and SE of sbs_Chl_diff
(afileSbsStats = aTimeSeriesFile %>% 
  dplyr::filter(Experiment %in% c("sbs_before", "sbs_after")) %>% # all sbs values used in correction
  select(Time, Chl_ug_L, Position) %>% # select data relevent to sbs 
  tidyr::pivot_wider(names_from = Position, values_from = Chl_ug_L) %>% # create two new columns Up & Down fill with Chl values, paired by time
  filter(!is.na(Up) & !is.na(Down)) %>%  # select rows with data in Up and Down
  mutate(sbs_Chl_diff = Up - Down) %>% 
  summarise(Mean_sbs_Chl_diff = mean(sbs_Chl_diff),
            Median_sbs_Chl_diff = median(sbs_Chl_diff),
            SD_sbs_Chl_diff = sd(sbs_Chl_diff), # calc sample standard deviation 
            SE_sbs_Chl_diff = SD_sbs_Chl_diff/sqrt(length(sbs_Chl_diff)), # calc sample standard error
            Sample_count = length(Time))
  )

# dataframe for graphing & individual sonde stats
(distrubution = aTimeSeriesFile %>% 
  dplyr::filter(Experiment %in% c("sbs_before", "sbs_after")) %>% # all sbs values used in correction
  select(Time, Chl_ug_L, Position, Experiment) %>% 
  tidyr::pivot_wider(names_from = Position, values_from = Chl_ug_L) %>% # spread data to remove NAs
  filter(!is.na(Up) & !is.na(Down)) %>% # remove NAs
  pivot_longer(c("Up", "Down"), names_to = "Position", values_to = "Chl_ug_L")) # make data longer again for graphs

# downstream summary stats - to populate graphs
(distrubution_down <- distrubution %>%
    filter(Position %in% "Down") %>% 
    summarise(mean_chl_down = mean(Chl_ug_L),
              median_chl_down = median(Chl_ug_L),
              sd_chl_down = sd(Chl_ug_L),
              se_chl_down = sd_chl_down/sqrt(length(Chl_ug_L))))

# upstream summary stats - to populate graphs
(distrubution_up <- distrubution %>%
    filter(Position %in% "Up") %>% 
    summarise(mean_chl_up = mean(Chl_ug_L),
              median_chl_up = median(Chl_ug_L),
              sd_chl_up = sd(Chl_ug_L),
              se_chl_up = sd_chl_up/sqrt(length(Chl_ug_L))))

# Create Side by Side summary data frame that will build larger dataframe in loop
(Sbs_stat_summary = data.frame(# File_Name = aFileName,
  Mean_sbs_Chl_diff = afileSbsStats$Mean_sbs_Chl_diff,
  Median_sbs_Chl_diff = afileSbsStats$Median_sbs_Chl_diff,
  SD_sbs_Chl_diff = afileSbsStats$SD_sbs_Chl_diff,
  SE_sbs_Chl_diff = afileSbsStats$SE_sbs_Chl_diff,
  Mean_Chl_Up = distrubution_up$mean_chl_up,
  Median_Chl_Up = distrubution_up$median_chl_up,
  SD_Chl_Up = distrubution_up$sd_chl_up,
  SE_CHl_UP = distrubution_up$se_chl_up,
  Mean_Chl_Down = distrubution_down$mean_chl_down,
  Median_Chl_Down = distrubution_down$median_chl_down,
  SD_Chl_Down = distrubution_down$sd_chl_down,
  SE_Chl_Down = distrubution_down$se_chl_down,
  Sample_Count = afileSbsStats$Sample_count)
)

##############################################
# Dataframe to add to plot
Sbs_stats_plot = Sbs_stat_summary %>% 
  select(Sample_Count, Mean_sbs_Chl_diff, SD_sbs_Chl_diff, SE_sbs_Chl_diff) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  rename("n" = Sample_Count,  
         "Mean Chl Diff" = Mean_sbs_Chl_diff,  
         "StDev Chl Diff" = SD_sbs_Chl_diff,  
         "St Error Chl Diff" = SE_sbs_Chl_diff)
# try to round Sampe_Count to whole number?
  
Sbs_stats_plot <- t(Sbs_stats_plot) # transpose columns and rows 
Sbs_stats_plot_text <- ggpubr::ggtexttable(Sbs_stats_plot, theme = ttheme("blank")) # dataframe to text table
# theme = ttheme("mOrange")

# Frequency polygon of sbs measurements
(distrubution_plot <- ggplot(data = distrubution, aes(x = Chl_ug_L)) +
  geom_freqpoly(aes(color = Position), binwidth = 0.1, size = 1) +
  scale_color_manual(values = c("Down" = wes_palette("Cavalcanti1")[3],
                              "Up" = wes_palette("Cavalcanti1")[2])) +
  theme_gdocs() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
    # add dashed line to indicate means
  geom_vline(xintercept = distrubution_down$mean_chl_down, 
             color = wes_palette("Cavalcanti1")[3] , linetype = "dashed", size = .75) +
  geom_vline(xintercept = distrubution_up$mean_chl_up,
             color = wes_palette("Cavalcanti1")[2], linetype = "dashed", size = .75) +
    # add Mean Down next to dashed line
  annotate("text", x = Sbs_stat_summary$Mean_Chl_Down - 0.05, 
           y = 0, label = bquote(bar(x) ~ .(round(Sbs_stat_summary$Mean_Chl_Down, 2))),
           vjust = "top", hjust = "right", color = wes_palette("Cavalcanti1")[3]) +
    # add mean Up next to dashed line
  annotate("text", x = Sbs_stat_summary$Mean_Chl_Up - 0.05, 
           y = Inf, label = bquote(bar(x) ~ .(round(Sbs_stat_summary$Mean_Chl_Up, 2))),
           vjust = "top", hjust = "right", color = wes_palette("Cavalcanti1")[2]) +
  labs(title = paste0(unique(aFile_Mod$Experiment), ' - ',
                      aFileName %>% str_replace("Insitu_Filter_", "") %>%
                        str_replace(".csv", ""))))
  
    
  #annotate("text", x = Inf, y = Inf, label = paste("n = ", sep = "", afileSbsStats$Sample_count),
   #        vjust = "top", hjust = "right") + # label plot with number of samples
)

ggdraw(distrubution_plot) +
  draw_plot(Sbs_stats_plot_text, 
            vjust = 0,
            hjust = 0,
            x = 0.3,
            y = 0.35
      )


# Box plot of sbs measurements ---> Don't think this is as visually useful
(distrubution_boxplot <- distrubution %>% 
    ggplot(aes(x = Position, y = Chl_ug_L, fill = Position)) +
    scale_fill_manual(values = c("Down" = wes_palette("Cavalcanti1")[3],
                                 "Up" = wes_palette("Cavalcanti1")[2])) +
    geom_boxplot() +
    theme_clean() +
    guides(fill = "none"))


### Round SD and SE ???

# Test reordering graphs by time ------------------------------------------------------------------------
aTimeSeriesFile <- read_csv("output/2_Manual_Corrections_Applied/Insitu_Filter_NPD_41519.csv")

aFile_Mod = aTimeSeriesFile %<>%
  dplyr::mutate(Time = as_hms(Time),
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


# Test sbs time match --------------------------------------------------------------------------------

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

# Test Ploting Chl Diff, time adjusted -------------------------------------------------------------

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
  

# Test Correcting graph labels & consistent color asignment ----------------------------------------------

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

# Test function guts - Graph Filtraiton after time adjustment ----------------------------------
  
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
