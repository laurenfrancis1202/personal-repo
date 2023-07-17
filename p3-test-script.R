library(tidyverse)
library(readxl)
library(openxlsx)
library(janitor)  
library(here)
library(stringr)

# Statistics Specialist (Immunization), P3, Test

wd <- "C:/Users/XXXX/Test"

# read input data
maternal <- read_excel(paste0(wd, "/input-data/DHIS2_data_Ghana_v1.xlsx"), sheet = "Service_data_1") 
immunization <- read_excel(paste0(wd, "/input-data/DHIS2_data_Ghana_v1.xlsx"), sheet = "Service_data_2") 
mortality <- read_excel(paste0(wd, "/input-data/DHIS2_data_Ghana_v1.xlsx"), sheet = "Service_data_3")


### PART 1

# mortality has 10 extra rows

# checks
# look for missing districts
miss_maternal <- maternal %>% filter(is.na(District))
miss_immunization <- immunization %>% filter(is.na(District))
miss_mortality <- mortality %>% filter(is.na(District))  # 70 missing observations

# mortality has 70 missing districts. See what is missing
d_maternal <- maternal %>% select(District) %>% distinct() %>% mutate(mat='mat')
d_immunization <- immunization %>% select(District) %>% distinct()  %>% mutate(imm='imm')
d_mortality <- mortality %>% select(District) %>% distinct() %>% mutate(mort='mort')

check1 <- anti_join(d_maternal, d_mortality)  # mortality data is missing guan and month and year

# check for accented letters
accent_maternal <- maternal %>% filter(str_detect(District, "[^ -~]")) 
accent_immunization <- immunization %>% filter(str_detect(District, "[^ -~]"))
accent_mortality <- mortality %>% filter(str_detect(District, "[^ -~]"))  # 4 obs with accents (Keta)

# fill in missing district name and join on month and year
mortality_v2 <- mortality %>%
  mutate(District = ifelse(is.na(District), 'Guan', District)) %>%
  filter(!is.na(maternal_deaths)) %>%  # remove true missing rows
  mutate(new = ifelse(District=='Guan', 1, NA_real_))
  
month <- maternal %>% select(Year, Month) %>% distinct() %>% mutate(new=1) %>% rename(year=Year, month=Month)

mortality_v2 <- left_join(mortality_v2, month, by=c('new')) %>%
  distinct() %>%
  mutate(Year = ifelse(District=='Guan', year, Year),
         Month = ifelse(District=='Guan', month, Month)) %>%
  select(-c(month, year, new))

# fix Keta (remove accent)
mortality_v2 <- mortality_v2 %>%
  mutate(District = ifelse(District=='Kéta', 'Keta', District))

# function to trim leading and trailing white space
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

maternal_v2 <- maternal %>%
  mutate(District = trim(District))

immunization_v2 <- immunization %>%
  mutate(District = trim(District))

mortality_v2 <- mortality_v2 %>%
  mutate(District = trim(District))


# merge data files
merge <- left_join(maternal_v2, immunization_v2, by = join_by('District', 'Year', 'Month')) %>%
  left_join(., mortality_v2, by = join_by('District', 'Year', 'Month')) 

check2 <- merge %>% filter(!is.na(ANC4))
check2 <- merge %>% filter(!is.na(BCG))
check2 <- merge %>% filter(!is.na(Stillbirth_total))


# export
write.xlsx(merge, paste0(wd, '/p3-test-part1-output.xlsx'))


### PART 2

part2 <- read_excel(paste0(wd, "/input-data/Test_ghp3_2.xlsx"))

# 2a. number of regions

part2_2a <- part2 %>%
  arrange(country, year, admin1, admin2) %>%
  select(country, year, admin1) %>%
  group_by(country, year) %>%
  mutate(n_admin1 = n_distinct(admin1)) %>%
  ungroup() %>%
  select(-admin1) %>%
  distinct()

# part 2b. number of districts
part2_2b <- part2 %>%
  arrange(country, year, admin1, admin2) %>%
  select(country, year, admin2) %>%
  group_by(country, year) %>%
  mutate(n_admin2 = n_distinct(admin2)) %>%
  ungroup() %>%
  select(-admin2) %>%
  distinct()

# part 2c. region with most districts

part2_2c <- part2 %>%
  arrange(country, year, admin1, admin2) %>%
  select(country, year, admin1, admin2) %>%
  group_by(country, year, admin1) %>%
  mutate(n_admin2 = n_distinct(admin2)) %>%
  ungroup() %>%
  select(-admin2) %>%
  distinct() %>%
  pivot_wider(., names_from = year, values_from = n_admin2)


# part 2d. data quality

# first, calculate number of doses, target and weighted coverage at admin1 level 

part2_region <- part2 %>%
  arrange(country, year, admin1, vaccine_name) %>%
  group_by(country, year, admin1, vaccine_name) %>%
  mutate(n_doses = sum(doses),
         n_target_number = sum(target_number)) %>%
  ungroup() %>%
  mutate(coverage = (n_doses/n_target_number)*100,
         coverage=round(coverage,0)) %>%
  select(country:admin1, vaccine_name, target_define, n_doses, n_target_number, coverage) %>%
  distinct()

#check the target population are the same across related antigens
si <- part2_region %>%
  select(country:admin1, target_define, n_target_number) %>%
  distinct() %>%
  group_by(country, year, admin1, target_define) %>%
  mutate(n = n_distinct(n_target_number)) %>%
  ungroup()

# check there is one obs per region for number of births and four obs for number of surviving infants.
si2 <- part2_region %>%
  select(country:admin1, target_define, vaccine_name) %>%
  distinct() %>%
  group_by(country, year, admin1, target_define) %>%
  mutate(n = n_distinct(vaccine_name)) %>%
  ungroup() %>%
  select(-vaccine_name) %>%
  distinct()

# check related indicators
part2_dtp <- part2_region %>%
  select(country:admin1, vaccine_name, coverage) %>%  # compare dtp coverage
  filter(vaccine_name %in% c('DTP1','DTP3')) %>%
  pivot_wider(., names_from = vaccine_name, values_from = coverage) %>%
  mutate(dtp3_gt_dtp1 = ifelse(DTP3 > DTP1, 1, 0))

part2_dtp <- part2_region %>%
  select(country:admin1, vaccine_name, n_doses) %>%  # compare dtp doses
  filter(vaccine_name %in% c('DTP1','DTP3')) %>%
  pivot_wider(., names_from = vaccine_name, values_from = n_doses) %>%
  mutate(dtp3_gt_dtp1 = ifelse(DTP3 > DTP1, 1, 0))

part2_mcv_yfv <- part2_region %>%
  select(country:admin1, vaccine_name, coverage) %>%  # compare mcv and yfv coverage
  filter(vaccine_name %in% c('MCV1','YFV')) %>%
  pivot_wider(., names_from = vaccine_name, values_from = coverage) %>%
  mutate(dtp3_gt_dtp1 = ifelse(MCV1 != YFV, 1, 0)) %>%
  mutate(diff = MCV1-YFV)  # difference between MCV1 and YFV coverage

# 3a. 

# list the top 2 regions with highest coverage per vaccine-year
part3a <- part2_region %>%
  arrange(vaccine_name, year, desc(coverage)) %>%
  group_by(country, vaccine_name, year) %>%
  mutate(n=seq(n())) %>%
  ungroup() %>% filter(n<=2)








