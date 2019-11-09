
# Code to processes TSS or TPM filter samples

# TSS = Total Suspended Solids
# TPM = Total Particulate Matter
# POM = Particulate Organic Matter
# PIM + Particulate Inorganic Matter

######################################################################
# Load Packages
######################################################################
library(tidyverse)
library(lubridate)

######################################################################
# Set Up Directory and read in TPM Data
######################################################################

TPM_directory = "./TPM_POM_PIM"

TPM_data = fread(file.path(TPM_directory, "Insitu_Filter_POM_PIM_Data.csv"))

######################################################################
# Summary Table & Average Table per field day
######################################################################

TPM_summary_table <- TPM_data %>% 
  mutate(TPM = ((dry_weight_corrected_g - filter_weight_corrected_g) * 1000 / sample_volume_ml),
         PIM = TPM - ((ash_weight_corrected_g - filter_weight_corrected_g) * 1000 / sample_volume_ml),
         POM = TPM - PIM,
         Organic_Content_Ratio = POM / TPM) %>% 
  as_tibble()

Avg_TPM_summary_table <- TPM_summary_table %>% 
  group_by(Date, Site, sample_type) %>% 
  summarize(Avg_TPM = mean(TPM),
            Avg_PIM = mean(PIM),
            Avg_POM = mean(POM),
            Avg_OC_Ratio = mean(Organic_Content_Ratio)) 

######################################################################
# Create TPM Output folder
######################################################################

TPM_output_directory <<- file.path(output_directory,"6_TPM_OC_Summary")
if(!dir.exists(TPM_output_directory))
{
  dir.create(TPM_output_directory)
}
TPM_output_directory

#####################################################################
# Export Summary Tables to project Output folder
######################################################################

write_csv(TPM_summary_table, file.path(TPM_output_directory, "TPM_summary_table.csv"))
write_csv(Avg_TPM_summary_table, file.path(TPM_output_directory, "Avg_TPM_summary_table.csv"))

          