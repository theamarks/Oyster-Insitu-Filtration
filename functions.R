######################################################################################
## This function creates all the required output directories
######################################################################################
createOutputDirectories = function()
{
  # create an output directory where the results after initial cleaning will be stored
  output_dir_file_cleaning <<- file.path(output_directory, "1_Cleaned_Data")
  if(!dir.exists(output_dir_file_cleaning))
  {
    dir.create(output_dir_file_cleaning)
  }
  
  # create an output directory where the results after manual corrections will be stored
  output_dir_manual_corrections <<- file.path(output_directory, "2_Manual_Corrections_Applied")
  if(!dir.exists(output_dir_manual_corrections))
  {
    dir.create(output_dir_manual_corrections)
  }
  
  # Create an output directory to store time series files after correction
  sbs_correction_output_directory <<- file.path(output_directory,"3_Sbs_Corrections_Applied")
  if(!dir.exists(sbs_correction_output_directory))
  {
    dir.create(sbs_correction_output_directory)
  }
  
  filtration_calc_output_directory <<- file.path(output_directory,"4_Filtration_Calculations")
  if(!dir.exists(filtration_calc_output_directory))
  {
    dir.create(filtration_calc_output_directory)
  }
  
  filtration_summary_output_directory <<- file.path(filtration_calc_output_directory,"Filtration_Summary")
  if(!dir.exists(filtration_summary_output_directory))
  {
    dir.create(filtration_summary_output_directory)
  }
  
  graph_output_directory <<- file.path(output_directory,"0_Graph_Output")
  if(!dir.exists(graph_output_directory))
  {
    dir.create(graph_output_directory)
  }
  
  orig_graphs_directory <<- file.path(graph_output_directory,"Graphs_Before_Corrections")
  if(!dir.exists(orig_graphs_directory))
  {
    dir.create(orig_graphs_directory)
  } 
  
  corrected_graphs_directory <<- file.path(graph_output_directory,"Graphs_After_Corrections")
  if(!dir.exists(corrected_graphs_directory))
  {
    dir.create(corrected_graphs_directory)
  } 
  # directory for velocity paired Chl diff time series graphs
  pairedChlDiff_graphs_directory <<- file.path(graph_output_directory,"Graphs_Paired_Chl_Diff")
  if(!dir.exists(pairedChlDiff_graphs_directory))
  {
    dir.create(pairedChlDiff_graphs_directory)
  }
  
  # Create plot output directory
  Bivlave_density_graph_directory <<- file.path(graph_output_directory,"Graphs_Bivalve_Density")
  if(!dir.exists(Bivlave_density_graph_directory))
  {
    dir.create(Bivlave_density_graph_directory)
  }
  
  TPM_output_directory <<- file.path(output_directory,"5_TPM_OC_Summary")
  if(!dir.exists(TPM_output_directory))
  {
    dir.create(TPM_output_directory)
  }
  
  # Create an output directory for sbs corrections summary file 
  sbs_summary_output_directory <<- file.path(output_directory,"6_Sbs_Corrections_Summary")
  if(!dir.exists(sbs_summary_output_directory))
  {
    dir.create(sbs_summary_output_directory)
  }
  
  # Create an output directory for sbs corrections summary file 
  water_velocity_summary_directory <<- file.path(output_directory,"7_Water_Velocity_Summary")
  if(!dir.exists(water_velocity_summary_directory))
  {
    dir.create(water_velocity_summary_directory)
  }

  # Create an output directory for error statistics
  error_directory <<- file.path(output_directory,"8_Error_Calculatios")
  if(!dir.exists(error_directory))
  {
    dir.create(error_directory)
  }
}
######################################################################################
## This function standardizes the column names and values (for site, experiment and position)
######################################################################################
standardizeNamesAndColumns = function(aTimeSeriesFile)
{
  names(aTimeSeriesFile) = c("Time","Temp_C", "SpCond_mS_cm", "Cond_mS_cm", "TDS_g_L",   
                   "Sal_ppt", "Turbidity_NTU", "Chl_ug_L", "Experiment", "Date", 
                   "Sonde", "Position", "Site")
  
  aTimeSeriesFile %<>%
    dplyr::mutate(Site = tolower(Site),
                  Site = ifelse(str_detect(Site, "shell"), "Shellmaker",
                                    ifelse(str_detect(Site, "rafael") , "San Rafael",
                                           ifelse(str_detect(Site, "diego"), "San Diego",
                                                  ifelse(str_detect(Site, "bay"), "Morro Bay",
                                                         ifelse(str_detect(Site, "deanza"), "Deanza", "Pattern not found")))))) 
  
  ## 3.b) Column Position
  aTimeSeriesFile %<>%
    dplyr::mutate(Position = tolower(Position),
                  Position = ifelse(str_detect(Position, "up"), "Up", 
                                        ifelse(str_detect(Position, "down"), "Down", 
                                               "Pattern not found")))
  
  ## 3.c) Column Experiment
  aTimeSeriesFile %<>%
    dplyr::mutate(Experiment = ifelse(str_detect(tolower(Experiment), "before"), "sbs_before",
                                          ifelse(str_detect(tolower(Experiment), "afte"), "sbs_after", 
                                                 ifelse(str_detect(tolower(Experiment), "filt"), "Filtration", Experiment))))
  
  
 return(aTimeSeriesFile)
}

######################################################################################
## This function creates time series plot 
######################################################################################
createTimeSeriesPlot = function(aTimeSeriesFile, aFileName, aGraphOutputDirectory, aType)
{  
  aFile_Mod = aTimeSeriesFile %<>%
    dplyr::mutate(Time = as_hms(Time),
                  Experiment = ifelse(Experiment %in% c("sbs_after", "sbs_before", "Filtration"), Experiment, "Neg_Control"),
                  legend_title = paste0(Position, ' ', Sonde)) %>%  # combine columns for title
    transform(Experiment = factor(Experiment, levels = c("sbs_before", "Filtration","Neg_Control", "sbs_after")))
  # create list of names for facet headers 
  trial_names <- c("sbs_before" = paste0(aFileName %>% str_replace("Insitu_Filter_", "") %>%
                                           str_replace(".csv", ""),' - ',"sbs_before"), 
                   
                   "Filtration" = paste0(aFileName %>% str_replace("Insitu_Filter_", "") %>%
                                           str_replace(".csv", ""), ' - ',"Filtration"),
                   
                   "sbs_after" = paste0(aFileName %>% str_replace("Insitu_Filter_", "") %>%
                                          str_replace(".csv", ""), ' - ',"sbs_after"),
                   
                   "Neg_Control" = paste0(aFileName %>% str_replace("Insitu_Filter_", "") %>%
                                            str_replace(".csv", ""), ' - ', "Neg_Control"))
  
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
  
  one_graph_name = paste0(gsub(".csv", "", aFileName), "_", aType, ".pdf")
  ggsave(one_graph_name, one_plot, dpi = 600, width = 11.5, height = 7, units = "in", device = "pdf", aGraphOutputDirectory)
  return(one_plot)

}


######################################################################################
## This function applies manual corrections
######################################################################################
applyManualCorrections =  function(aTimeSeriesFile, aFileName, aManualCorrectionsFile,  aOutputDirectory)
{
  
  if(aFileName %in% aManualCorrectionsFile$File_Name)
  {
    one_file_corrections = aManualCorrectionsFile %>%
      dplyr::filter(File_Name == aFileName)
    
    for(correction in 1 : nrow(one_file_corrections))
    {  
      
      one_correction = one_file_corrections[correction, ]
      
      ## find which instrument to correct
      if(one_correction$Correction_Applied_to_Both_Instruments)
      {
        instruments_to_correct = c("G", "H")
      } else {
        instruments_to_correct = one_file_corrections$Instrument_with_Spike
      }
      
      ## find which eperiment to correct 
      # get the list of experiments 
      all_exp = unique(aTimeSeriesFile$Experiment)
      
      if(one_correction$Experiment == ""|is.na(one_correction$Experiment))
      {
        experiment_to_correct = all_exp[!(all_exp %in% c("sbs_before", "sbs_after"))]
        
      } else {
        
        experiment_to_correct = one_correction$Experiment
      }
      
      aTimeSeriesFile %<>%
        dplyr::mutate(Time = as_hms(Time)) %>%
        dplyr::filter(!(Experiment %in% experiment_to_correct & 
                          Sonde %in% instruments_to_correct & 
                          Time >= one_correction$Correction_Start_Time & 
                          Time <=   one_correction$Correction_End_Time)) %>%
        dplyr::mutate(Time = as.character(Time)) 
      
    }
    
    return(aTimeSeriesFile)
    
  } else {
    
    ## if there are no corrections return the same input file back
    return(aTimeSeriesFile)
    
  }
}

######################################################################################
## Creates time series plot - Difference b/w sondes Chl, velocity time adjusted
######################################################################################

createChlDiffPlot = function(aTimeSeriesFile, aFileName, aGraphOutputDirectory, aType)
{  
  aFile_Mod = aTimeSeriesFile %<>%
    select(Time, Date, Site, Experiment, Chl_ug_L_Up, Chl_ug_L_Down) %>% 
    mutate(Chl_diff = Chl_ug_L_Up - Chl_ug_L_Down,
           Time = as_hms(Time))
  
  one_plot = ggplot(data = aFile_Mod, aes(x = Time, y = Chl_diff))+
    geom_path(size = 1, color = wes_palette("GrandBudapest1")[3]) +
    geom_point(color = wes_palette("GrandBudapest1")[3]) +
    theme_gdocs() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank()) +
    geom_hline(yintercept = 0, size = 1, color = "grey50", linetype = "dashed") + # adds diff line at y = 0
    labs(x = "", y = "Chl Difference (ug/L)", title = paste0(aFileName %>% str_replace("Insitu_Filter_", "") %>%
                                                               str_replace(".csv", ""), ' - ',unique(aFile_Mod$Experiment)))
  
  one_graph_name = paste0(gsub(".csv", "", aFileName), "_", aType, ".pdf")
  ggsave(one_graph_name, one_plot, dpi = 600, width = 7, height = 5, units = "in", device = "pdf", aGraphOutputDirectory)
  return(one_plot)
  
}

######################################################################################
## This function calculates and summarizes sensor systematic error based on sbs trials
######################################################################################

calculateErrorStats = function(aTimeSeriesFile, aFileName, aManualCorrectionsFile,  aOutputDirectory)
{
  afileSbsStats = aTimeSeriesFile %>% 
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
  
  
  # dataframe for graphing & individual sonde stats
  adistrubution = aTimeSeriesFile %>% 
      dplyr::filter(Experiment %in% c("sbs_before", "sbs_after")) %>% # all sbs values used in correction
      select(Time, Chl_ug_L, Position, Experiment) %>% 
      tidyr::pivot_wider(names_from = Position, values_from = Chl_ug_L) %>% # spread data to remove NAs
      filter(!is.na(Up) & !is.na(Down)) %>% # remove NAs
      pivot_longer(c("Up", "Down"), names_to = "Position", values_to = "Chl_ug_L") # make data longer again for graphs
  
  # downstream summary stats - to populate graphs
  distrubution_down <- adistrubution %>%
      filter(Position %in% "Down") %>% 
      summarise(mean_chl_down = mean(Chl_ug_L),
                median_chl_down = median(Chl_ug_L),
                sd_chl_down = sd(Chl_ug_L),
                se_chl_down = sd_chl_down/sqrt(length(Chl_ug_L)))
  
  # upstream summary stats - to populate graphs
  distrubution_up <- adistrubution %>%
      filter(Position %in% "Up") %>% 
      summarise(mean_chl_up = mean(Chl_ug_L),
                median_chl_up = median(Chl_ug_L),
                sd_chl_up = sd(Chl_ug_L),
                se_chl_up = sd_chl_up/sqrt(length(Chl_ug_L)))
  
  # Create Side by Side summary data frame that will build larger dataframe in loop
  aSbs_stat_summary = data.frame(File_Name = aFileName,
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
  
  return(aSbs_stat_summary)
  
}
######################################################################################
## This function Sbs distrubution data set for Sbs Density Graphs
######################################################################################
calculateSbsGraphData = function(aTimeSeriesFile)
{
  
   # dataframe for graphing & individual sonde stats
  adistrubution = aTimeSeriesFile %>% 
    dplyr::filter(Experiment %in% c("sbs_before", "sbs_after")) %>% # all sbs values used in correction
    select(Time, Chl_ug_L, Position, Experiment) %>% 
    tidyr::pivot_wider(names_from = Position, values_from = Chl_ug_L) %>% # spread data to remove NAs
    filter(!is.na(Up) & !is.na(Down)) %>% # remove NAs
    pivot_longer(c("Up", "Down"), names_to = "Position", values_to = "Chl_ug_L") # make data longer again for graphs
  
   return(adistrubution)
  
}


######################################################################################
## This function Graphs Density Plot of SBS Trails 
######################################################################################
createSbsDensityPlot = function(adistrubution, aSbs_stat_summary, aFileName)
{
  # dataframe for graphing & individual sonde stats
  distrubution = adistrubution %>% 
    dplyr::filter(Experiment %in% c("sbs_before", "sbs_after")) %>% # all sbs values used in correction
    select(Time, Chl_ug_L, Position, Experiment) %>% 
    tidyr::pivot_wider(names_from = Position, values_from = Chl_ug_L) %>% # spread data to remove NAs
    filter(!is.na(Up) & !is.na(Down)) %>% # remove NAs
    pivot_longer(c("Up", "Down"), names_to = "Position", values_to = "Chl_ug_L") # make data longer again for graphs
  
  # downstream summary stats - to populate graphs
  distrubution_down <- distrubution %>%
    filter(Position %in% "Down") %>% 
    summarise(mean_chl_down = mean(Chl_ug_L),
              median_chl_down = median(Chl_ug_L),
              sd_chl_down = sd(Chl_ug_L),
              se_chl_down = sd_chl_down/sqrt(length(Chl_ug_L)))
  
  # upstream summary stats - to populate graphs
  distrubution_up <- distrubution %>%
    filter(Position %in% "Up") %>% 
    summarise(mean_chl_up = mean(Chl_ug_L),
              median_chl_up = median(Chl_ug_L),
              sd_chl_up = sd(Chl_ug_L),
              se_chl_up = sd_chl_up/sqrt(length(Chl_ug_L)))
  
  Sbs_stats_plot = aSbs_stat_summary %>% 
   select(Sample_Count, Mean_sbs_Chl_diff, SD_sbs_Chl_diff, SE_sbs_Chl_diff) %>% 
   mutate_if(is.numeric, round, 3) %>% 
   rename("n" = Sample_Count,  
         "Mean Chl Diff" = Mean_sbs_Chl_diff,  
         "StDev Chl Diff" = SD_sbs_Chl_diff,  
         "St Error Chl Diff" = SE_sbs_Chl_diff)

  Sbs_stats_plot <- t(Sbs_stats_plot) # transpose columns and rows 
  Sbs_stats_plot_text <- ggpubr::ggtexttable(Sbs_stats_plot, theme = ttheme("blank")) # dataframe to text table

  # Frequency polygon of sbs measurements
  distrubution_plot <- ggplot(data = adistrubution, aes(x = Chl_ug_L)) +
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
      annotate("text", x = aSbs_stat_summary$Mean_Chl_Down - 0.05, 
               y = 0, label = bquote(bar(x) ~ .(round(aSbs_stat_summary$Mean_Chl_Down, 2))),
              vjust = "top", hjust = "right", color = wes_palette("Cavalcanti1")[3]) +
    # add mean Up next to dashed line
      annotate("text", x = aSbs_stat_summary$Mean_Chl_Up - 0.05, 
              y = Inf, label = bquote(bar(x) ~ .(round(aSbs_stat_summary$Mean_Chl_Up, 2))),
              vjust = "top", hjust = "right", color = wes_palette("Cavalcanti1")[2]) +
      labs(title = aFileName %>% str_replace("Insitu_Filter_", "") %>%
                            str_replace(".csv", ""))
  
  distrubution_plot_stats <- ggdraw(distrubution_plot) +
      draw_plot(Sbs_stats_plot_text, 
                vjust = 0,
                hjust = 0,
                x = 0.3,
                y = 0.35)

  return(distrubution_plot_stats)
}

######################################################################################
## This function summarizes the sbs correction requirements 
######################################################################################
summarizeSbsCorrectionValues = function(aTimeSeriesFile, aFileName)
{
  # check if the correction need to be applied
  correction_check = aTimeSeriesFile %>%
    dplyr::filter(Experiment %in% c("sbs_before", "sbs_after")) %>% # all sbs values used in correction
    select(Time, Chl_ug_L, Position) %>% # select data relevent to sbs correction
    tidyr::pivot_wider(names_from = Position, values_from = Chl_ug_L) %>% # create two new columns Up & Down fill with Chl values, paired by time
    filter(!is.na(Up) & !is.na(Down)) %>% # select rows with data in Up and Down
    dplyr::summarise(Avg_Chl_Up = mean(Up),
                     Avg_Chl_Down = mean(Down))
     #  dplyr::group_by(Position) %>%
  #  dplyr::summarise(Avg_Chl = mean(Chl_ug_L))
  
  # extacts value from dataframe. without $Avg_Chl at the end, object would remain data.frame -AM
 # down_chl = correction_check[which(correction_check$Position == "Down"), "Avg_Chl"]$Avg_Chl
  down_chl = correction_check$Avg_Chl_Down
  #up_chl = correction_check[which(correction_check$Position == "Up"), "Avg_Chl"]$Avg_Chl 
  up_chl = correction_check$Avg_Chl_Up
  abs_difference = abs(up_chl - down_chl) # get rid of absolute value
  
  # Chl sensor error +- 0.1 ug/L, 2 sensors, need correction if chl difference > 0.2 ug/L
 # if(abs_difference > 0.2) # Remove conditional - DZ & AM decided to correct all measurements
 # {
    correction_factor = abs_difference/2
    
    ## determine if the correction is be applied to sbs before or sbs after
    instrument_check = aTimeSeriesFile %>%
      dplyr::filter(Experiment %in% c("sbs_before", "sbs_after")) %>%
      dplyr::group_by(Sonde) %>%
      dplyr::summarise(Avg_Chl = mean(Chl_ug_L))
    
    G_avg_chl = instrument_check[which(instrument_check$Sonde == "G"), "Avg_Chl"]$Avg_Chl
    H_avg_chl = instrument_check[which(instrument_check$Sonde == "H"), "Avg_Chl"]$Avg_Chl
    
    sonde_higher_avg = ifelse(G_avg_chl > H_avg_chl, "G", "H")
    sonde_lower_avg = ifelse(G_avg_chl < H_avg_chl, "G", "H")
    
    correction_summary = data.frame(File_Name = aFileName,
                                    Down_Chl_Avg = down_chl,
                                    Up_Chl_Avg = up_chl,
                                    Abs_Avg_Diff = abs_difference,
                                    Correction_Req = TRUE,
                                    Correction_Factor = correction_factor,
                                    G_Avg_Chl = G_avg_chl,
                                    H_Avg_Chl = H_avg_chl,
                                    Sonde_Read_Higher_Avg = sonde_higher_avg,
                                    Sonde_Read_Lower_Avg = sonde_lower_avg,
                                    stringsAsFactors = FALSE)
 # } else {
    
#    correction_summary = data.frame(File_Name = aFileName,
 #                                   Down_Chl_Avg = down_chl,
  #                                  Up_Chl_Avg = up_chl,
   #                                 Abs_Diff = abs_difference,
    #                                Correction_Req = FALSE,
     #                               Correction_Factor = NA,
      #                              G_Avg_Chl = NA,
       #                             H_Avg_Chl = NA,
        #                            Sonde_Higher_Avg = NA,
         #                           Sonde_Lower_Avg = NA,
          #                          stringsAsFactors = FALSE)
#  }
  
  return(correction_summary)

}

######################################################################################
## This function applies the side by side correction if required 
######################################################################################
applySbsCorrections = function(aTimeSeriesFile, aSbsCorrectionSummary)
{
  if(aSbsCorrectionSummary$Correction_Req) 
  {  
    one_file_sbs_correction_applied = aTimeSeriesFile %>%
            dplyr::mutate(Chl_ug_L_Corrected = ifelse(# !Experiment %in% c("sbs_before", "sbs_after") & # Select Filtraiton Or Negative Control 
                                             Sonde==aSbsCorrectionSummary$Sonde_Read_Higher_Avg, # AND Select sonde in summary reading higher
                                            Chl_ug_L - aSbsCorrectionSummary$Correction_Factor, # Then Subtract correction factor from higher sonde
                                              ifelse(# !Experiment %in% c("sbs_before", "sbs_after") & # Select Filtration Or Negative Control
                                                      Sonde==aSbsCorrectionSummary$Sonde_Read_Lower_Avg, # AND Select sonde in summary reading lower
                                                     Chl_ug_L + aSbsCorrectionSummary$Correction_Factor, Chl_ug_L))) # Then Add correction factor to lower sonde
    return(one_file_sbs_correction_applied)
         
  } else {
    
    return(aTimeSeriesFile)
  }
  
}  

######################################################################################
## This function calculate the time takes for water to travel from the upstream 
## instrument to the downstream instrument
######################################################################################
calculateTravelTimeBySiteAndDate =  function(aVelocityData, aFRVariableData)
{  
  
  vel_summary_df = aVelocityData %>%
    dplyr::filter(measure_position %in% c("Mid", "Down")) %>%
    dplyr::group_by(Date, Site, Experiment) %>%
    dplyr::summarise(avg_m_s = mean(m_s),
                     avg_m_hr = avg_m_s*3600)
  
  final_results = aFRVariableData %>%
    dplyr::left_join(vel_summary_df , by = c("Date", "Site", "Experiment")) %>%
    dplyr::mutate(t_travel_s = round(d_bw_sondes_m/avg_m_s, 0),
                  t_travel_mm_ss = hms(t_travel_s))
  
  return(final_results)
  
}

######################################################################################
## This function adjusts the time stamp for down instrument in filtration experiment by water travel time
######################################################################################
adjustDownSondeTimeStamp = function(aWaterVelSummaryFile, aTimeSeriesFile)
{
  one_correction = aWaterVelSummaryFile %>%
    dplyr::filter(Date %in% unique(aTimeSeriesFile$Date)) # select water row with same date as time series file going through 
  
  for(i in 1 : nrow(one_correction))
  {  
    aTimeSeriesFile %<>%
      data.frame() %>%
      dplyr::mutate(Time = paste0(Date, " ", Time),
                    Time = lubridate::parse_date_time(Time, orders = "mdy HMS"),
                    Time = ifelse(toupper(Experiment) == toupper(one_correction$Experiment)[[i]] 
                                  & Position == "Down" 
                                  & toupper(Site) == toupper(one_correction$Site)[[i]] ,  Time -one_correction$t_travel_s[[i]], Time),
                    Time = as.POSIXct(Time, origin="1970-01-01", tz = "UTC"),
                    Date = paste0(month(Time), "/", day(Time), "/", substr(year(Time), 3, 4)), 
                    Time = strftime(Time, format="%H:%M:%S", tz = "UTC")) 
  }
  return(aTimeSeriesFile)
}

######################################################################################
## This function calculates filtration after pairing the data by timestamp
######################################################################################
calculateFiltrationForPairedData = function(aTimeSeriesFile, aWaterVelSummary)
{
  one_water_vel_summary = aWaterVelSummary %>%
    dplyr::filter(Date %in% unique(aTimeSeriesFile$Date)) %>%
    dplyr::select(Date, Site, Experiment, avg_depth_cm, d_bw_sondes_m, avg_m_hr)
  
  up_sonde_df = aTimeSeriesFile %>%
    dplyr::filter(Position == "Up" & !Experiment %in% c("sbs_before", "sbs_after"))%>%
    dplyr::select(Time, Date, Experiment, Sonde, Site, Temp_C, SpCond_mS_cm, Cond_mS_cm, TDS_g_L, Sal_ppt, Turbidity_NTU, Chl_ug_L_Corrected)
  
  down_sonde_df = aTimeSeriesFile %>%
    dplyr::filter(Position == "Down" & !Experiment %in% c("sbs_before", "sbs_after"))%>%
    dplyr::select(Time, Date, Experiment, Sonde, Site, Temp_C, SpCond_mS_cm, Cond_mS_cm, TDS_g_L, Sal_ppt, Turbidity_NTU, Chl_ug_L_Corrected)
  
  
  combined_water_quality_df = up_sonde_df %>%
    dplyr::inner_join(down_sonde_df, by = c("Time", "Date", "Site", "Experiment")) %>%
    dplyr::select(Time, Date, Site, Experiment,
                  
                  Sonde_Up = Sonde.x,
                  Temp_C_Up = Temp_C.x, 
                  SpCond_mS_cm_Up = SpCond_mS_cm.x, 
                  Cond_mS_cm_Up = Cond_mS_cm.x, 
                  TDS_g_L_Up = TDS_g_L.x, 
                  Sal_ppt_Up = Sal_ppt.x, 
                  Turbidity_NTU_Up = Turbidity_NTU.x, 
                  Chl_ug_L_Up = Chl_ug_L_Corrected.x,
                  
                  Sonde_Down = Sonde.y,
                  Temp_C_Down = Temp_C.y, 
                  SpCond_mS_cm_Down = SpCond_mS_cm.y, 
                  Cond_mS_cm_Down = Cond_mS_cm.y, 
                  TDS_g_L_Down = TDS_g_L.y, 
                  Sal_ppt_Down = Sal_ppt.y, 
                  Turbidity_NTU_Down = Turbidity_NTU.y, 
                  Chl_ug_L_Down = Chl_ug_L_Corrected.y) %>%
    
    dplyr::inner_join(one_water_vel_summary, by = c("Date", "Site", "Experiment")) %>%
    
    dplyr::mutate(pcnt_Chl_rmvd = ((Chl_ug_L_Up - Chl_ug_L_Down) / Chl_ug_L_Up) * 100,
                  L_hr_m2 = (((avg_depth_cm/100) * avg_m_hr * 1000) / d_bw_sondes_m) * ((Chl_ug_L_Up - Chl_ug_L_Down) / Chl_ug_L_Up))
  
  return(combined_water_quality_df)

}

######################################################################################
## This function summarizes the filtration
######################################################################################
createFiltraationSummary = function(aFiltrationFile, aFileName)
{
  data_only_numeric = dplyr::select_if(aFiltrationFile, is.numeric)
  filtration_sub_df =  aFiltrationFile %>% 
    dplyr::select(c(names(data_only_numeric), "Experiment", "Date", "Site"))
  
  filtration_sub_df = filtration_sub_df %>%
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
  
  return(filtration_sub_df)
}

######################################################################################
## Create Water Quality compound Graphs 
######################################################################################

createWQgraphs = function(aFiltrationFile, aFileName)
{
  # Convert Time from difftime to hms variable for x-axis formatting
  aFiltrationFile$Time <- as_hms(aFiltrationFile$Time)
  
  # Make y-axis limits the same for both upstream and downstream graphs
  # Chl y-axis bounds
  Chl_ymax <- ifelse(max(aFiltrationFile$Chl_ug_L_Up) > max(aFiltrationFile$Chl_ug_L_Down), 
                     max(aFiltrationFile$Chl_ug_L_Up), max(aFiltrationFile$Chl_ug_L_Down))
  Chl_ymin <- ifelse(min(aFiltrationFile$Chl_ug_L_Up) < min(aFiltrationFile$Chl_ug_L_Down), 
                     min(aFiltrationFile$Chl_ug_L_Up), min(aFiltrationFile$Chl_ug_L_Down))
  # Turbidity y-axis bounds
  Turb_ymax <- ifelse(max(aFiltrationFile$Turbidity_NTU_Up) > max(aFiltrationFile$Turbidity_NTU_Down), 
                      max(aFiltrationFile$Turbidity_NTU_Up), max(aFiltrationFile$Turbidity_NTU_Down))
  Turb_ymin <- ifelse(min(aFiltrationFile$Turbidity_NTU_Up) < min(aFiltrationFile$Turbidity_NTU_Down), 
                      min(aFiltrationFile$Turbidity_NTU_Up), min(aFiltrationFile$Turbidity_NTU_Down))
  # Temp y-axis bounds
  Temp_ymax <- ifelse(max(aFiltrationFile$Temp_C_Up) > max(aFiltrationFile$Temp_C_Down), 
                      max(aFiltrationFile$Temp_C_Up), max(aFiltrationFile$Temp_C_Down))
  Temp_ymin <- ifelse(min(aFiltrationFile$Temp_C_Up) < min(aFiltrationFile$Temp_C_Down), 
                      min(aFiltrationFile$Temp_C_Up), min(aFiltrationFile$Temp_C_Down))
  # Salinity y-axis bounds
  Sal_ymax <- ifelse(max(aFiltrationFile$Sal_ppt_Up) > max(aFiltrationFile$Sal_ppt_Down), 
                     max(aFiltrationFile$Sal_ppt_Up), max(aFiltrationFile$Sal_ppt_Down))
  Sal_ymin <- ifelse(min(aFiltrationFile$Sal_ppt_Up) < min(aFiltrationFile$Sal_ppt_Down), 
                     min(aFiltrationFile$Sal_ppt_Up), min(aFiltrationFile$Sal_ppt_Down))
  
  # Chlorophyll Up
  Chl_plot_Up <- ggplot(data = aFiltrationFile, aes(x = Time, y = Chl_ug_L_Up)) +
      #geom_path(size = 1, color = wes_palette("Cavalcanti1")[2]) +
      geom_point(color = wes_palette("Cavalcanti1")[2]) +
      theme_gdocs() +
      coord_cartesian(ylim = c(Chl_ymin, Chl_ymax)) +
      geom_hline(yintercept = mean(aFiltrationFile$Chl_ug_L_Up),
                 color = wes_palette("Cavalcanti1")[2], linetype = "dashed", size = .75) +
      theme(axis.text.x = element_blank(), # remove x axis text
            axis.title.x = element_blank(), # removed x axis title
            legend.title = element_blank(), # remove legend
            rect = element_blank()) + # removed black boarder rectangle 
      theme(plot.subtitle = element_text(hjust = 0.5)) + # Center title
      labs(title = paste0(aFileName %>% str_replace("Insitu_Filter_", "") %>% str_replace(".csv", ""),
                          ' - ', unique(aFiltrationFile$Experiment)),
          subtitle = "Upstream")
  
  # Turbidity Up 
  Turb_plot_Up <- ggplot(data = aFiltrationFile, aes(x = Time, y = Turbidity_NTU_Up)) +
      #geom_path(size = 1, color = wes_palette("Royal1")[4]) +
      geom_point(color = wes_palette("Royal1")[4]) +
      theme_gdocs() +
      coord_cartesian(ylim = c(Turb_ymin, Turb_ymax)) +
      geom_hline(yintercept = mean(aFiltrationFile$Turbidity_NTU_Up),
                 color = wes_palette("Royal1")[4], linetype = "dashed", size = .75) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            rect = element_blank(),
            legend.title = element_blank()) +
      labs(y = "Turbidity NTU")
  
  # Temperature Up
  Temp_plot_Up <- ggplot(data = aFiltrationFile, aes(x = Time, y = Temp_C_Up)) +
     # geom_path(size = 1, color = wes_palette("Zissou1")[1]) +
      geom_point(color = wes_palette("Zissou1")[1]) +
      theme_gdocs() +
      coord_cartesian(ylim = c(Temp_ymin, Temp_ymax)) +
      geom_hline(yintercept = mean(aFiltrationFile$Temp_C_Up),
                 color = wes_palette("Zissou1")[1], linetype = "dashed", size = .75) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            rect = element_blank(),
            legend.title = element_blank()) +
      labs(y = paste("Temperature ", "(", intToUtf8(176), "C)"))
  
  # Slainity Up
  Sal_plot_Up <- ggplot(data = aFiltrationFile, aes(x = Time, y = Sal_ppt_Up)) +
      #geom_path(size = 1, color = wes_palette("GrandBudapest1")[2]) +
      geom_point(color = wes_palette("GrandBudapest1")[2]) +
      theme_gdocs() +
      coord_cartesian(ylim = c(Sal_ymin, Sal_ymax)) +
      geom_hline(yintercept = mean(aFiltrationFile$Sal_ppt_Up),
                 color = wes_palette("GrandBudapest1")[2], linetype = "dashed", size = .75) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            rect = element_blank(),
            legend.title = element_blank()) +
      labs(y = "Salinity (ppt)")
  
  # All Up WQ Graphs in a single column cowplot::plot_grid()
  Up_WQ <- plot_grid(Chl_plot_Up, Turb_plot_Up, Temp_plot_Up, Sal_plot_Up,
                     nrow = 4)
  
  # Chlorophyll Down
  Chl_plot_Down <- ggplot(data = aFiltrationFile, aes(x = Time, y = Chl_ug_L_Down)) +
      #geom_path(size = 1, color = wes_palette("Cavalcanti1")[3]) +
      geom_point(color = wes_palette("Cavalcanti1")[3]) +
      theme_gdocs() +
      coord_cartesian(ylim = c(Chl_ymin, Chl_ymax)) +
      geom_hline(yintercept = mean(aFiltrationFile$Chl_ug_L_Down),
                 color = wes_palette("Cavalcanti1")[3], linetype = "dashed", size = .75) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            rect = element_blank()) +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      labs(title = "",
          subtitle = "Downstream")
  
  # Turbidity Down 
  Turb_plot_Down <- ggplot(data = aFiltrationFile, aes(x = Time, y = Turbidity_NTU_Down)) +
      #geom_path(size = 1, color = wes_palette("Royal1")[4]) +
      geom_point(color = wes_palette("Royal1")[4]) +
      theme_gdocs() +
      coord_cartesian(ylim = c(Turb_ymin, Turb_ymax)) +
      geom_hline(yintercept = mean(aFiltrationFile$Turbidity_NTU_Down),
                 color = wes_palette("Royal1")[4], linetype = "dashed", size = .75) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            rect = element_blank(),
            legend.title = element_blank()) 
  
  # Temperature Down
  Temp_plot_Down <- ggplot(data = aFiltrationFile, aes(x = Time, y = Temp_C_Down)) +
      #geom_path(size = 1, color = wes_palette("Zissou1")[1]) +
      geom_point(color = wes_palette("Zissou1")[1]) +
      theme_gdocs() +
      coord_cartesian(ylim = c(Temp_ymin, Temp_ymax)) +
      geom_hline(yintercept = mean(aFiltrationFile$Temp_C_Down),
                 color = wes_palette("Zissou1")[1], linetype = "dashed", size = .75) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            rect = element_blank(),
            legend.title = element_blank()) 
  
  # Salinity Down
  Sal_plot_Down <- ggplot(data = aFiltrationFile, aes(x = Time, y = Sal_ppt_Down)) +
      #geom_path(size = 1, color = wes_palette("GrandBudapest1")[2]) +
      geom_point(color = wes_palette("GrandBudapest1")[2]) +
      theme_gdocs() +
      coord_cartesian(ylim = c(Sal_ymin, Sal_ymax)) +
      geom_hline(yintercept = mean(aFiltrationFile$Sal_ppt_Down),
                 color = wes_palette("GrandBudapest1")[2], linetype = "dashed", size = .75) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            rect = element_blank(),
            legend.title = element_blank()) 
  
  # combine all downstream graphs in a single column
  Down_WQ <- plot_grid(Chl_plot_Down, Turb_plot_Down, Temp_plot_Down, Sal_plot_Down,
                       nrow = 4)
  # combine upstream and downstream WQ graphs in two columns
  All_WQ <- plot_grid(Up_WQ, Down_WQ, ncol = 2)
  
  return(All_WQ)
  
}
######################################################################################
## Rearrange Sbs Corrected data for graphing water quality
######################################################################################
matchSbsCorrectedByTime = function(aSbsCorrectionFile)
{
  up_sonde_df = aSbsCorrectionFile %>%
    dplyr::filter(Position == "Up" & Experiment %in% c("sbs_before", "sbs_after"))%>%
    dplyr::select(Time, Date, Experiment, Sonde, Position, Site, Temp_C, SpCond_mS_cm,
                  Cond_mS_cm, TDS_g_L, Sal_ppt, Turbidity_NTU, Chl_ug_L_Corrected)
  
  down_sonde_df = aSbsCorrectionFile %>%
    dplyr::filter(Position == "Down" & Experiment %in% c("sbs_before", "sbs_after"))%>%
    dplyr::select(Time, Date, Experiment, Sonde, Position, Site, Temp_C, SpCond_mS_cm, 
                  Cond_mS_cm, TDS_g_L, Sal_ppt, Turbidity_NTU, Chl_ug_L_Corrected)
  
  combined_Sbs_WQ_df = up_sonde_df %>%
    dplyr::inner_join(down_sonde_df, by = c("Time", "Date", "Site", "Experiment")) %>%
    dplyr::select(Time, Date, Site, Experiment,
                  
                  Sonde_Up = Sonde.x,
                  Temp_C_Up = Temp_C.x, 
                  SpCond_mS_cm_Up = SpCond_mS_cm.x, 
                  Cond_mS_cm_Up = Cond_mS_cm.x, 
                  TDS_g_L_Up = TDS_g_L.x, 
                  Sal_ppt_Up = Sal_ppt.x, 
                  Turbidity_NTU_Up = Turbidity_NTU.x, 
                  Chl_ug_L_Corr_Up = Chl_ug_L_Corrected.x,
                  
                  Sonde_Down = Sonde.y,
                  Temp_C_Down = Temp_C.y, 
                  SpCond_mS_cm_Down = SpCond_mS_cm.y, 
                  Cond_mS_cm_Down = Cond_mS_cm.y, 
                  TDS_g_L_Down = TDS_g_L.y, 
                  Sal_ppt_Down = Sal_ppt.y, 
                  Turbidity_NTU_Down = Turbidity_NTU.y, 
                  Chl_ug_L_Corr_Down = Chl_ug_L_Corrected.y)
  
  return(combined_Sbs_WQ_df)
  
}
######################################################################################
## Create Water Quality compound Graphs - SBS
######################################################################################
createWQgraphsSBS = function(aSbsCorrMatchedFile, aFileName)
{
  # Convert Time from difftime to hms variable for x-axis formatting
  aSbsCorrMatchedFile$Time <- as_hms(aSbsCorrMatchedFile$Time)
  
  # Make y-axis limits the same for both upstream and downstream graphs
  # Chl y-axis bounds
  Chl_ymax <- ifelse(max(aSbsCorrMatchedFile$Chl_ug_L_Corr_Up) > max(aSbsCorrMatchedFile$Chl_ug_L_Corr_Down), 
                     max(aSbsCorrMatchedFile$Chl_ug_L_Corr_Up), max(aSbsCorrMatchedFile$Chl_ug_L_Corr_Down))
  Chl_ymin <- ifelse(min(aSbsCorrMatchedFile$Chl_ug_L_Corr_Up) < min(aSbsCorrMatchedFile$Chl_ug_L_Corr_Down), 
                     min(aSbsCorrMatchedFile$Chl_ug_L_Corr_Up), min(aSbsCorrMatchedFile$Chl_ug_L_Corr_Down))
  # Turbidity y-axis bounds
  Turb_ymax <- ifelse(max(aSbsCorrMatchedFile$Turbidity_NTU_Up) > max(aSbsCorrMatchedFile$Turbidity_NTU_Down), 
                      max(aSbsCorrMatchedFile$Turbidity_NTU_Up), max(aSbsCorrMatchedFile$Turbidity_NTU_Down))
  Turb_ymin <- ifelse(min(aSbsCorrMatchedFile$Turbidity_NTU_Up) < min(aSbsCorrMatchedFile$Turbidity_NTU_Down), 
                      min(aSbsCorrMatchedFile$Turbidity_NTU_Up), min(aSbsCorrMatchedFile$Turbidity_NTU_Down))
  # Temp y-axis bounds
  Temp_ymax <- ifelse(max(aSbsCorrMatchedFile$Temp_C_Up) > max(aSbsCorrMatchedFile$Temp_C_Down), 
                      max(aSbsCorrMatchedFile$Temp_C_Up), max(aSbsCorrMatchedFile$Temp_C_Down))
  Temp_ymin <- ifelse(min(aSbsCorrMatchedFile$Temp_C_Up) < min(aSbsCorrMatchedFile$Temp_C_Down), 
                      min(aSbsCorrMatchedFile$Temp_C_Up), min(aSbsCorrMatchedFile$Temp_C_Down))
  # Salinity y-axis bounds
  Sal_ymax <- ifelse(max(aSbsCorrMatchedFile$Sal_ppt_Up) > max(aSbsCorrMatchedFile$Sal_ppt_Down), 
                     max(aSbsCorrMatchedFile$Sal_ppt_Up), max(aSbsCorrMatchedFile$Sal_ppt_Down))
  Sal_ymin <- ifelse(min(aSbsCorrMatchedFile$Sal_ppt_Up) < min(aSbsCorrMatchedFile$Sal_ppt_Down), 
                     min(aSbsCorrMatchedFile$Sal_ppt_Up), min(aSbsCorrMatchedFile$Sal_ppt_Down))
  
  # Chlorophyll Up
  Chl_plot_Up <- ggplot(data = aSbsCorrMatchedFile, aes(x = Time, y = Chl_ug_L_Corr_Up)) +
    #geom_path(size = 1, color = wes_palette("Cavalcanti1")[2]) +
    geom_point(color = wes_palette("Cavalcanti1")[2]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Chl_ymin, Chl_ymax)) +
    geom_hline(yintercept = mean(aSbsCorrMatchedFile$Chl_ug_L_Corr_Up),
               color = wes_palette("Cavalcanti1")[2], linetype = "dashed", size = .75) +
    theme(axis.text.x = element_blank(), # remove x axis text
          axis.title.x = element_blank(), # removed x axis title
          legend.title = element_blank(), # remove legend
          rect = element_blank()) + # removed black boarder rectangle 
    theme(plot.subtitle = element_text(hjust = 0.5)) + # Center title
    labs(title = paste0(aFileName %>% str_replace("Insitu_Filter_", "") %>% str_replace(".csv", ""),
                        ' - ', "Side by Side"),
         subtitle = "Upstream")
  
  # Turbidity Up 
  Turb_plot_Up <- ggplot(data = aSbsCorrMatchedFile, aes(x = Time, y = Turbidity_NTU_Up)) +
    #geom_path(size = 1, color = wes_palette("Royal1")[4]) +
    geom_point(color = wes_palette("Royal1")[4]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Turb_ymin, Turb_ymax)) +
    geom_hline(yintercept = mean(aSbsCorrMatchedFile$Turbidity_NTU_Up),
               color = wes_palette("Royal1")[4], linetype = "dashed", size = .75) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          rect = element_blank(),
          legend.title = element_blank()) +
    labs(y = "Turbidity NTU")
  
  # Temperature Up
  Temp_plot_Up <- ggplot(data = aSbsCorrMatchedFile, aes(x = Time, y = Temp_C_Up)) +
    # geom_path(size = 1, color = wes_palette("Zissou1")[1]) +
    geom_point(color = wes_palette("Zissou1")[1]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Temp_ymin, Temp_ymax)) +
    geom_hline(yintercept = mean(aSbsCorrMatchedFile$Temp_C_Up),
               color = wes_palette("Zissou1")[1], linetype = "dashed", size = .75) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          rect = element_blank(),
          legend.title = element_blank()) +
    labs(y = paste("Temperature ", "(", intToUtf8(176), "C)"))
  
  # Slainity Up
  Sal_plot_Up <- ggplot(data = aSbsCorrMatchedFile, aes(x = Time, y = Sal_ppt_Up)) +
    #geom_path(size = 1, color = wes_palette("GrandBudapest1")[2]) +
    geom_point(color = wes_palette("GrandBudapest1")[2]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Sal_ymin, Sal_ymax)) +
    geom_hline(yintercept = mean(aSbsCorrMatchedFile$Sal_ppt_Up),
               color = wes_palette("GrandBudapest1")[2], linetype = "dashed", size = .75) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          rect = element_blank(),
          legend.title = element_blank()) +
    labs(y = "Salinity (ppt)")
  
  # All Up WQ Graphs in a single column cowplot::plot_grid()
  Up_WQ <- plot_grid(Chl_plot_Up, Turb_plot_Up, Temp_plot_Up, Sal_plot_Up,
                     nrow = 4)
  
  # Chlorophyll Down
  Chl_plot_Down <- ggplot(data = aSbsCorrMatchedFile, aes(x = Time, y = Chl_ug_L_Corr_Down)) +
    #geom_path(size = 1, color = wes_palette("Cavalcanti1")[3]) +
    geom_point(color = wes_palette("Cavalcanti1")[3]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Chl_ymin, Chl_ymax)) +
    geom_hline(yintercept = mean(aSbsCorrMatchedFile$Chl_ug_L_Corr_Down),
               color = wes_palette("Cavalcanti1")[3], linetype = "dashed", size = .75) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          rect = element_blank()) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    labs(title = "",
         subtitle = "Downstream")
  
  # Turbidity Down 
  Turb_plot_Down <- ggplot(data = aSbsCorrMatchedFile, aes(x = Time, y = Turbidity_NTU_Down)) +
    #geom_path(size = 1, color = wes_palette("Royal1")[4]) +
    geom_point(color = wes_palette("Royal1")[4]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Turb_ymin, Turb_ymax)) +
    geom_hline(yintercept = mean(aSbsCorrMatchedFile$Turbidity_NTU_Down),
               color = wes_palette("Royal1")[4], linetype = "dashed", size = .75) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          rect = element_blank(),
          legend.title = element_blank()) 
  
  # Temperature Down
  Temp_plot_Down <- ggplot(data = aSbsCorrMatchedFile, aes(x = Time, y = Temp_C_Down)) +
    #geom_path(size = 1, color = wes_palette("Zissou1")[1]) +
    geom_point(color = wes_palette("Zissou1")[1]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Temp_ymin, Temp_ymax)) +
    geom_hline(yintercept = mean(aSbsCorrMatchedFile$Temp_C_Down),
               color = wes_palette("Zissou1")[1], linetype = "dashed", size = .75) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          rect = element_blank(),
          legend.title = element_blank()) 
  
  # Salinity Down
  Sal_plot_Down <- ggplot(data = aSbsCorrMatchedFile, aes(x = Time, y = Sal_ppt_Down)) +
    #geom_path(size = 1, color = wes_palette("GrandBudapest1")[2]) +
    geom_point(color = wes_palette("GrandBudapest1")[2]) +
    theme_gdocs() +
    coord_cartesian(ylim = c(Sal_ymin, Sal_ymax)) +
    geom_hline(yintercept = mean(aSbsCorrMatchedFile$Sal_ppt_Down),
               color = wes_palette("GrandBudapest1")[2], linetype = "dashed", size = .75) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          rect = element_blank(),
          legend.title = element_blank()) 
  
  # combine all downstream graphs in a single column
  Down_WQ <- plot_grid(Chl_plot_Down, Turb_plot_Down, Temp_plot_Down, Sal_plot_Down,
                       nrow = 4)
  # combine upstream and downstream WQ graphs in two columns
  All_SBS_WQ <- plot_grid(Up_WQ, Down_WQ, ncol = 2)
  
  return(All_SBS_WQ)
  
}
