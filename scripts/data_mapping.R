library(tidyverse)

for(type in c("FISH", "PHYTOPLANKTON", "INVERTEBRATES", "MACROALGAE", "BIOTA", "WATER", "METADATA", "SEDIMENTS")) {
    assign(type, read_csv(paste0("data/", type, ".csv")))
}

WATER %>% print(width=Inf)
length(unique(METADATA$siteid))
METADATA %>% group_by(siteid) %>% summarise(n=n()) %>% arrange(desc(n))
METADATA %>% filter(siteid == "E-A10") %>% print(width=Inf)
METADATA %>% select(-c(taxagroup, startyearcollected:mail3)) %>% distinct() %>% group_by(siteid) %>% summarise(n=n()) %>% arrange(desc(n))
METADATA %>% select(-c(taxagroup, startyearcollected:mail3)) %>% distinct()

WATER_METADATA <- METADATA %>% select(-c(taxagroup, startyearcollected:mail3)) %>% distinct() %>% 
                    right_join(WATER %>% select(-parameter_standardunit) %>% pivot_wider(values_from=c(parameter_value), names_from=parameter)) 
WATER_METADATA %>% print(width=Inf)

write_csv(WATER_METADATA, file="data/WATER_METADATA.csv")   # A tibble: 17,876 × 64

colSums(is.na(WATER_METADATA))

METADATA_WATER <- METADATA %>% filter(taxagroup=="Water")
METADATA_FISH <- METADATA %>% filter(taxagroup=="Fish")

METADATA_FISH_WATER <- METADATA_FISH %>% mutate(siteid_FISH=siteid) %>% 
    mutate(decimallatitude=round(decimallatitude, digits=4), decimallongitude=round(decimallongitude, digits=2)) %>% 
    select(siteid_FISH, decimallatitude, decimallongitude) %>% 
    left_join(METADATA_WATER %>% mutate(decimallatitude=round(decimallatitude, digits=4), decimallongitude=round(decimallongitude, digits=2)) %>% 
    select(siteid, decimallatitude, decimallongitude))
METADATA_FISH_WATER %>% filter(!is.na(siteid))

colnames(WATER_METADATA)
colnames(PHYTOPLANKTON)     # A tibble: 57,945 × 12
PHYTOPLANKTON %>% print(width=Inf)

WATER_METADATA_PHYTOPLANKTON <- WATER_METADATA %>% mutate(siteid_n = gsub("S$", "", gsub("HS$", "", siteid))) %>% 
                                    select(-c(samplingeffort, sampleid,basisofrecord,minimumdepthinmeters,maximumdepthinmeters)) %>% right_join(PHYTOPLANKTON, by=c("siteid_n"="siteid", "datecollected"))
colSums(is.na(WATER_METADATA_PHYTOPLANKTON))
write_csv(WATER_METADATA_PHYTOPLANKTON, file="data/WATER_METADATA_PHYTOPLANKTON.csv")

WATER_METADATA_PHYTOPLANKTON %>% filter(is.na(siteid)) %>% select(siteid, siteid_n, decimallatitude, decimallongitude,  parameter) %>% distinct()
WATER_METADATA_PHYTOPLANKTON %>% filter(!is.na(siteid)) %>% select(siteid, siteid_n, decimallatitude, decimallongitude,  parameter) 
# A tibble: 57,596 × 5

WATER_METADATA_PHYTOPLANKTON %>% mutate(corr = minimumdepthinmeters.x== minimumdepthinmeters.y) %>% select(corr) %>% table()
WATER_METADATA_PHYTOPLANKTON %>% mutate(corr = basisofrecord.x== basisofrecord.y) %>% select(corr) %>% table()
WATER_METADATA_PHYTOPLANKTON %>% mutate(corr = sampleid.x== sampleid.y) %>% select(corr) %>% table()

WATER_METADATA_PHYTOPLANKTON %>% select(taxaname) %>% distinct()

WATER_METADATA_INVERTEBRATE <- WATER_METADATA %>% mutate(siteid_n = gsub("S$", "", gsub("HS$", "", siteid))) %>% 
                                    select(-c(samplingeffort, sampleid,basisofrecord,minimumdepthinmeters,maximumdepthinmeters)) %>% 
                                    right_join(INVERTEBRATES, by=c("siteid_n"="siteid", "datecollected"))
colSums(is.na(WATER_METADATA_INVERTEBRATE))
write_csv(WATER_METADATA_INVERTEBRATE, file="data/WATER_METADATA_INVERTEBRATES.csv")
WATER_METADATA_INVERTEBRATE %>% filter(is.na(siteid)) %>% select(siteid, siteid_n, datecollected, decimallatitude, decimallongitude,  parameter) %>% distinct()


WATER_METADATA <- read_csv("data/WATER_METADATA.csv")
METADATA %>% print(width=Inf)
METADATA_water <- METADATA %>% mutate(siteid_n = gsub("B$", "", gsub("S$", "", gsub("LB$", "", gsub("HB$", "", gsub("LS$", "", gsub("HS$", "", siteid))))))) %>% filter(taxagroup=="Water")
METADATA_phytoplankton <- METADATA %>% mutate(siteid_n = gsub("S$", "", gsub("HS$", "", siteid))) %>% filter(taxagroup=="Phytoplankton")

METADATA_merge <- METADATA_water %>% full_join(METADATA_phytoplankton, by="siteid_n")
METADATA_merge %>% filter(!is.na(siteid.x)) %>% mutate(matched_longitude = decimallongitude.x==decimallongitude.y, matched_latitude= decimallatitude.x==decimallatitude.y) %>% select(siteid.x, siteid.y, siteid_n, matched_latitude, matched_longitude) 
METADATA_merge %>% filter(is.na(siteid.x)) %>% mutate(matched_longitude = decimallongitude.x==decimallongitude.y, matched_latitude= decimallatitude.x==decimallatitude.y) %>% select(siteid.x, siteid.y, siteid_n, matched_latitude, matched_longitude) 
METADATA_merge %>% filter(is.na(siteid.y)) %>% mutate(matched_longitude = decimallongitude.x==decimallongitude.y, matched_latitude= decimallatitude.x==decimallatitude.y) %>% select(siteid.x, siteid.y, siteid_n, matched_latitude, matched_longitude) 
