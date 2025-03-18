library(tidyverse)
library(ggplot)

WATER <- read_csv("data/WATER.csv")
METADATA <- read_csv("data/METADATA.csv")
PHYTOPLANKTON <- read_csv("data/PHYTOPLANKTON.csv")
FISH <- read_csv("data/FISH.csv")

for(type in c("FISH", "PHYTOPLANKTON", "INVERTEBRATES", "MACROALGAE", "BIOTA")) {
    assign(type, read_csv(paste0("data/", type, ".csv")))
}

for(type in c("FISH", "PHYTOPLANKTON", "INVERTEBRATES", "MACROALGAE", "BIOTA")) {
    print(type)
    print(dim(get(type)))
}

WATER %>% select(parameter, parameter_standardunit) %>% distinct() %>% print(n=Inf)     # 53 variables

length(unique(WATER$siteid))    # 166
length(unique(PHYTOPLANKTON$siteid))    # 53

PHYTOPLANKTON %>% group_by(siteid) %>% summarise(n=n()) %>% arrange(desc(n))
PHYTOPLANKTON %>% group_by(taxaname) %>% summarise(n=n()) %>% arrange(desc(n))
sample_sites <- PHYTOPLANKTON %>% group_by(siteid) %>% summarise(n=n()) %>% arrange(desc(n)) %>% slice(1:10) %>% pull(siteid)

plot <- PHYTOPLANKTON %>% filter(siteid %in% sample_sites, taxaname=="Plagioselmis") %>% mutate(datecollected=as.Date(datecollected)) %>% ggplot() +
            geom_point(aes(x=datecollected, y=parameter_value), size=0.5, alpha=0.5) +
            ggtitle("Phytoplanktons") + 
            facet_wrap(~siteid, nrow=2)
ggsave(plot, file="plots/phytoplankton_Plagioselmis.png", width=20, height=8)


