#-----------------------------------------------------------------
# Author: Zhi Lin Zhou
# Last updated: 2023-09-18
# What: Making Figure Comparing maternal mortality 2000/2017
#
#----------------------------------------------------------------

here()

data <- read.csv(here('Output/finaldata.csv'))

df_inc_matmor <- data %>%
  select(country_name, ISO, Year, mat.mor) %>%
  dplyr::filter(Year < 2018) %>%
  arrange(ISO, Year) %>%
  group_by(ISO) %>%
  mutate(diffmatmor = mat.mor - mat.mor[1L]) %>%
  ungroup() %>%
  
  dplyr::filter(Year == 2017) %>%
  dplyr::filter(diffmatmor > 0)

poscountries <- df_inc_matmor$ISO # make the list of countries into a list 

# Make Line Graph 
data %>%
  dplyr::filter(ISO %in% poscountries) %>%
  ggplot(aes(x = Year, y = mat.mor, color = ISO, group = ISO))+
  geom_line()+
  scale_y_log10()+
  ggtitle('Maternal mortality')+
  ylab("Sum Maternal Mortality")
