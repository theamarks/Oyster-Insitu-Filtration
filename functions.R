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
  trial_names <- c("sbs_before" = paste0("sbs_before", ' - ', aFileName %>% str_replace("Insitu_Filter_", "") %>%
                                                                            str_replace(".csv", "")), 
                   "Filtration" = paste0("Filtration", ' - ', aFileName %>% str_replace("Insitu_Filter_", "") %>%
                                                                            str_replace(".csv", "")),
                   "sbs_after" = paste0("sbs_after", ' - ', aFileName %>% str_replace("Insitu_Filter_", "") %>%
                                                                          str_replace(".csv", "")),
                   "Neg_Control" = paste0("Neg_Control", ' - ', aFileName %>% str_replace("Insitu_Filter_", "") %>%
                                                                          str_replace(".csv", "")))
  
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
      labs(x = "", y = "Chl Difference (ug/L)", title = paste0(unique(aFile_Mod$Experiment), ' - ', aFileName %>% str_replace("Insitu_Filter_", "") %>%
                                                                 str_replace(".csv", "")))
  
  one_graph_name = paste0(gsub(".csv", "", aFileName), "_", aType, ".pdf")
  ggsave(one_graph_name, one_plot, dpi = 600, width = 7, height = 5, units = "in", device = "pdf", aGraphOutputDirectory)
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
            dplyr::mutate(Chl_ug_L_Corrected = ifelse(!Experiment %in% c("sbs_before", "sbs_after") # Select Filtraiton Or Negative Control 
                                            & Sonde==aSbsCorrectionSummary$Sonde_Read_Higher_Avg, # AND Select sonde in summary reading higher
                                            Chl_ug_L - aSbsCorrectionSummary$Correction_Factor, # Then Subtract correction factor from higher sonde
                                              ifelse(!Experiment %in% c("sbs_before", "sbs_after") # Select Filtration Or Negative Control
                                                     & Sonde==aSbsCorrectionSummary$Sonde_Read_Lower_Avg, # AND Select sonde in summary reading lower
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
    dplyr::summarise(avg_m_s = round(mean(m_s), 2),
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
calculateFilterationForPairedData = function(aTimeSeriesFile, aWaterVelSummary)
{
  one_water_vel_summary = aWaterVelSummary %>%
    dplyr::filter(Date %in% unique(aTimeSeriesFile$Date)) %>%
    dplyr::select(Date, Site, Experiment, avg_depth_cm, d_bw_sondes_m, avg_m_hr)
  
  up_sonde_df = aTimeSeriesFile %>%
    dplyr::filter(Position == "Up" & !Experiment %in% c("sbs_before", "sbs_after"))%>%
    dplyr::select(Time, Date, Experiment, Sonde, Site, Temp_C, SpCond_mS_cm, Cond_mS_cm, TDS_g_L, Sal_ppt, Turbidity_NTU, Chl_ug_L)
  
  down_sonde_df = aTimeSeriesFile %>%
    dplyr::filter(Position == "Down" & !Experiment %in% c("sbs_before", "sbs_after"))%>%
    dplyr::select(Time, Date, Experiment, Sonde, Site, Temp_C, SpCond_mS_cm, Cond_mS_cm, TDS_g_L, Sal_ppt, Turbidity_NTU, Chl_ug_L)
  
  
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
                  Chl_ug_L_Up = Chl_ug_L.x,
                  
                  Sonde_Down = Sonde.y,
                  Temp_C_Down = Temp_C.y, 
                  SpCond_mS_cm_Down = SpCond_mS_cm.y, 
                  Cond_mS_cm_Down = Cond_mS_cm.y, 
                  TDS_g_L_Down = TDS_g_L.y, 
                  Sal_ppt_Down = Sal_ppt.y, 
                  Turbidity_NTU_Down = Turbidity_NTU.y, 
                  Chl_ug_L_Down = Chl_ug_L.y) %>%
    
    dplyr::inner_join(one_water_vel_summary, by = c("Date", "Site", "Experiment")) %>%
    
    dplyr::mutate(pcnt_Chl_rmvd = ((Chl_ug_L_Up - Chl_ug_L_Down) / Chl_ug_L_Up) * 100,
                  L_hr_m2 = (((avg_depth_cm/100) * avg_m_hr * 1000) / d_bw_sondes_m) * ((Chl_ug_L_Up - Chl_ug_L_Down) / Chl_ug_L_Up))
  
  return(combined_water_quality_df)

}

######################################################################################
## This function summarizes the filtration
######################################################################################
createFilterationSummary = function(aFilterationFile, aFileName)
{
  data_only_numeric = dplyr::select_if(aFilterationFile, is.numeric)
  filtration_sub_df =  aFilterationFile %>% 
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
    dplyr::mutate_if(is.numeric, round, 3) %>%
    dplyr::mutate(File_Name = aFileName) %>%
    dplyr::select(File_Name, Experiment, everything())
  
  return(filtration_sub_df)
}

