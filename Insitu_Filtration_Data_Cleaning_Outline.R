# Insitu Filtration Directions 

# 1) Import ------------------------------------------------------------------
# Import (~30) .csv time series that will be run through code. Each time series contains data for two water quality instruments
# That have identical data columns. The instruments are identified by their position and their 'name'. The combination of 
# Site & Date are unique identifers for a single time series among the entire collection of .csvs. 


# 2) Clean ----------------------------------------------------------------
# Clean up column names so they are standarized across all data sets. The order is always the same in every .csv
# 
# "Time","Temp_C", "SpCond_mS_cm", "Cond_mS_cm", "TDS_g_L", "Sal_ppt", "Turbidity_NTU", "Chl_ug_L", "Experiment", 
# "Date", "Sonde", "Position", "Site"


# 3) Standardize Text --------------------------------------------------------
# Standarized text descriptions in Site, Experiment, and Position columns.
#
# Sites - "Morro Bay", "Deanza", "Shellmaker", "San Rafael", "San Diego"
# Experiment - "sbs_before", "Filtration", "sbs_after"
# Position - "Up", "Down"


# 4) Plot -----------------------------------------------------------------
# Plot each of the three experiments in each time series (sbs_before, Filtration, sbs_after) & save .pdf
# y-axis - Chl_ug_L
# x-axis - Time
# title - .csv name, Experiment (i.e. "Insitu_Filter_MB_72818.csv - Filtration")


# 5) Identify Chl Spikes & remove -----------------------------------------
# Manually scan resulting plots to identify Chl_ug_L spikes and remove data if field notes validate. 
# Remove entire rows of data based on "Time" 
# This could mean removing the first 'x' number of rows in the Experiment, or 'y' rows in the middle of the Experiment, 
# or 'z' rows at the end of the Experiment

# I understand that some kind of outlier detection function could be put in here, but I think I want to keep this 
# somewhat manual as it will also allow me to cut the time series based on other factors in my field notes. 


# 6) Side-by-side correction ----------------------------------------------
# Calculate sbs (side-by-side) correction value
# For each set of data I need to determine if I need to correct the Chl_ug_L data in the Filtration Experiment. To do this 
# I need to isolate Chl_ug_L data from only the "sbs_before" and "sbs_after" Experiments and calculate the absolute average difference 
# of Chl_ug_L values in the "Down" position from Chl_ug_L values in the "Up" position. If the absolute average difference is >= 0.2 
# for a dataset, then I need to apply a correction value to the Chl_ug_L data of the Filtration Experiment. 

# The correction: If the absolute average difference in Chl_ug_L is >= 0.2, then divide the absolute average difference by 2, 
# this is the sbs correction unique to the Date & Site. In the combined sbs Experiment ("sbs_before" & "sbs_after") determine which 
# instrument has a higher average Chl_ug_L reading. Subtract the sbs correction value from each Chl_ug_L data point of the higher 
# reading instrument in the Filtration Experiment. Add the sbs correction value to each Chl_ug_L data point of the lower instrument 
# in the Filtration Experiment. 


# 7) Pair Filtration Time stamps by Velocity ------------------------------
# water velocity measurements are contained in a separate .csv (Insitu_Filter_velocity_all_data.csv). Calculate the average m/s 
# ("m_s") from only measurements in the "Mid" and "Down" positions. Calculate m/hr ("m_h"). The .csv 
# "Insitu_Filter_sbs_FR_variables_all_data.csv" contains other information about each Site & Date time series. Calculate the time 
# (seconds) it takes for water to travel from the upstream instrument to the downstream instrument ("t_travel_s") by dividing 
# "d_bw_sondes" by "avg_m_s" for each Site & Date combination. 

# In each time series, in the "Filtration" Experiment, in the "Down" Position - subtract t_travel_s from each Time value. This adjusts
# the "Down" instrument time stamp to be the same as the second "Up" instrument based on how fast the water flows from the upstream
# instrument to the downstream instrument. 

# Next, I need to join or match water quality measurements ("Temp_C", "SpCond_mS_cm", "Cond_mS_cm", "TDS_g_L", "Sal_ppt", "Turbidity_NTU",
# "Chl_ug_L") from each instrument by time stamps. This shows the change in water quality measurements from the "Up" instrument
# to the "Down" instrument. Keep only exact time stamp matches, if a time stamp for one instrument doesn't have a match with the 
# second instrument, these rows can be deleted. 


# 8) Calculate Filtration  ------------------------------------------------
# Calculate percent chlorophyll removed (pcnt_Chl_rmvd) for each paired data in a new column -->
# ((Chl_ug_L "Up" - Chl_ug_L "Down") / Chl_ug_L "Up") * 100

# Calculate Filtration Rate (L_hr_m2) for each paried data in a new column -->  
# [((avg_depth_cm*100)(m_h)(1000)) / d_bw_sondes_m] * [(Chl_ug_L "Up" - Chl_ug_L "Down") / Chl_ug_L "Up")]

# Finished 