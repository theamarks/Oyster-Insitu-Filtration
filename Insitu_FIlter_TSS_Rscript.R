
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

# Graph Color Palette: Color blind palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

######################################################################
# Set Up Directory and read in TPM Data
######################################################################

TPM_directory = "./Data"

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

TPM_output_directory <<- file.path(output_directory,"5_TPM_OC_Summary")
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

#####################################################################
# Total Bivalve Density
#######################################################################
# San Rafael Bivalve Density - only oyster found in survey
SR_Density_directory = "./Data/bivalve_density_community"
SR_density_data = fread(file.path(SR_Density_directory, "Insitu_Filter_SR_oyster_density.csv"))

SR_nov17_olympia_m2 <- mean(SR_density_data$density_m2)      

# Morro Bay total bivalve density
MB_gigas_m2 = 600

# Newport Deanza total bivalve Density
NP_Density_directory = "./Data/bivalve_density_community"
NP_density_data = fread(file.path(NPD_Density_directory, "Insitu_filter_NP_may18_survey_counts.csv"))
NP_excavation_quad_area = 0.0625

NPD_may18_bivalve_den <- NP_density_data %>% 
  filter(Site == 'Deanza') %>% 
  select(1:10, 12) %>% 
  gather('O. lurida', 'C. gigas', 'Mytilus', 'Musculista', 'Geukensia', 'Adula', 'Speckled_scallop',
         key = 'Species',
         value = 'n_individuals') %>% 
  group_by(Quadrat) %>% 
  summarise(n_bivalves = sum(n_individuals),
            quad_den = n_bivalves / NP_excavation_quad_area)
  
NPD_may18_avg_m2 <- mean(NPD_may18_bivalve_den$quad_den) # Total Average bivalve density

# Newport Shellmaker total bivalve density

NPSM_may18_bivalve_den <- NP_density_data %>% 
  filter(Site == 'Shellmaker') %>% 
  select(1:10, 12) %>% 
  gather('O. lurida', 'C. gigas', 'Mytilus', 'Musculista', 'Geukensia', 'Adula', 'Speckled_scallop',
         key = 'Species',
         value = 'n_individuals') %>% 
  group_by(Quadrat) %>% 
  summarise(n_bivalves = sum(n_individuals),
            quad_den = n_bivalves / NP_excavation_quad_area)

NPSM_may18_avg_m2 <- mean(NPSM_may18_bivalve_den$quad_den) # Total Average bivalve density
  
#####################################################################
# Densities by Bivalve Species
######################################################################
# dataframe density by species - Deanza
NPD_may18_species_den <- NP_density_data %>% 
  filter(Site == 'Deanza') %>% 
  select(1:10, 12) %>% 
  gather('O. lurida', 'C. gigas', 'Mytilus', 'Musculista', 'Geukensia', 'Adula', 'Speckled_scallop',
         key = 'Species',
         value = 'n_individuals') %>% 
  group_by(Species, Site) %>% 
  summarise(species_den_m2 = (sum(n_individuals) / n()) / NP_excavation_quad_area)

# dataframe density by species - Shellmaker
NPSM_may18_species_den <- NP_density_data %>% 
  filter(Site == 'Shellmaker') %>% 
  select(1:10, 12) %>% 
  gather('O. lurida', 'C. gigas', 'Mytilus', 'Musculista', 'Geukensia', 'Adula', 'Speckled_scallop',
         key = 'Species',
         value = 'n_individuals') %>% 
  group_by(Species, Site) %>% 
  summarise(species_den_m2 = (sum(n_individuals) / n()) / NP_excavation_quad_area)

# dataframe density by species - San Rafael
SR_nov18_species_den <- SR_density_data %>% 
  mutate(Species = "O. lurida") %>% 
  group_by(Species, Site) %>% 
  summarise(species_den_m2 = mean(density_m2))
    
  
# dataframe density by species - numbers from Morro Bay Oyster Company
MB_species_den <- tibble(Species = "C. gigas", 
                         Site = "Morro Bay",
                         species_den_m2 = 600)

# combine species density dataframes - All sites
Species_density_summary <- bind_rows(NPD_may18_species_den,
                                           NPSM_may18_species_den,
                                           SR_nov18_species_den,
                                           MB_species_den, .id = NULL)
#####################################################################
# Graph Bivalve Species Densities
######################################################################
# Graph Density by Species, exclude absent species

# Newport Deanza
(Graph_NPD_species_den <- ggplot(NPD_may18_species_den[which(NPD_may18_species_den$species_den_m2>0),], 
                               aes(Species, species_den_m2, fill = Species)) + 
  geom_col() + # columns
  theme_classic() + 
  labs (x = 'Bivlave Species', 
        y = 'Density (individuals / m^2)', 
        title = 'Newport Deanza Bivalve Density',
        subtitle = 'May 2018 Survey') +
  guides(fill = FALSE) + # removes color legend
  geom_text(aes(label = species_den_m2), vjust = -0.5)) # able to round values? 
                                                                      
# Newport Shellmaker
(Graph_NPSM_species_den <- ggplot(NPSM_may18_species_den[which(NPD_may18_species_den$species_den_m2>0),], 
                                 aes(Species, species_den_m2, fill = Species)) + 
  geom_col() + # columns
  theme_classic() + 
  labs (x = 'Bivlave Species', 
        y = 'Density (individuals / m^2)', 
        title = 'Newport Shellmaker Bivalve Density',
        subtitle = 'May 2018 Survey') +
  guides(fill = FALSE) + # removes color legend
  geom_text(aes(label = species_den_m2), vjust = -0.5)) # able to round values? 

# Create plot output directory
Bivlave_density_graph_directory <<- file.path(graph_output_directory,"Graphs_Bivalve_Density")
if(!dir.exists(Bivlave_density_graph_directory))
{
  dir.create(Bivlave_density_graph_directory)
}
# Save graph to "./0_Graph_Output/Graphs_Bivalve_Density"
######### May need to reformat .pdf  ###########
ggsave(file.path(Bivlave_density_graph_directory, "NPD_may18_bivalve_species_density.pdf"), Graph_NPD_species_den )
ggsave(file.path(Bivlave_density_graph_directory, "NPSM_may18_bivalve_species_density.pdf"), Graph_NPSM_species_den )

##### FIgure ####### -->> biomass comparison among species
  
