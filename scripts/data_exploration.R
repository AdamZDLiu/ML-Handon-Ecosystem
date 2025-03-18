library(tidyverse)
library(ggplot)
library(ggpmisc)  


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


#################################################
##### Plotting parameter values by environmental variables #####
WATER_METADATA_PHYTOPLANKTON <- read_csv("data/WATER_METADATA_PHYTOPLANKTON.csv")

sample_species <- WATER_METADATA_PHYTOPLANKTON %>% group_by(taxaname) %>% summarise(n=n()) %>% arrange(desc(n)) %>% slice(1:10) %>% pull(taxaname)
WATER_METADATA_PHYTOPLANKTON %>% group_by(taxaname) %>% summarise(n=n()) %>% arrange(desc(n)) %>% slice(1) %>% pull(taxaname)

plot <- WATER_METADATA_PHYTOPLANKTON %>% filter(taxaname %in% sample_species) %>% 
            select(taxaname, pH, parameter_value) %>% mutate(pH=as.numeric(pH), taxaname=factor(taxaname)) %>% 
            ggplot(aes(x=pH, y=parameter_value)) +
            geom_point(size=0.5, alpha=0.5) +
            geom_smooth(method = "lm", se = FALSE) +
            stat_poly_eq(
                aes(label = paste("p-value = ", signif(..p.value.., digits = 3), 
                                "\nR^2 = ", signif(..adj.r.squared.., digits = 3), sep = "")),
                formula = y ~ x,
                parse = FALSE
            ) +
            facet_wrap(~taxaname, nrow=2) + 
            ggtitle("Phytoplankton abundance by pH") + theme_bw()
ggsave(plot, file="plots/phytoplankton_by_ph.png", width=20, height=8)

plot <- WATER_METADATA_PHYTOPLANKTON %>% filter(taxaname %in% sample_species) %>% 
            select(taxaname, Temperature, parameter_value) %>% mutate(Temperature=as.numeric(Temperature), taxaname=factor(taxaname)) %>% 
            ggplot(aes(x=Temperature, y=parameter_value)) +
            geom_point(size=0.5, alpha=0.5) +
            geom_smooth(method = "lm", se = FALSE, alpha=0.2, linewidth=0.5) +
            stat_poly_eq(
                aes(label = paste("p-value = ", signif(..p.value.., digits = 3), 
                                "\nR^2 = ", signif(..adj.r.squared.., digits = 3), sep = "")),
                formula = y ~ x,
                parse = FALSE
            ) +
            facet_wrap(~taxaname, nrow=2, scales="free_y") + 
            ggtitle("Phytoplankton abundance by Temperature") + theme_bw()
ggsave(plot, file="plots/phytoplankton_by_temperature.png", width=20, height=8)

plot <- WATER_METADATA_PHYTOPLANKTON %>% filter(taxaname %in% sample_species) %>% 
            select(taxaname, Salinity, parameter_value) %>% mutate(Salinity=as.numeric(Salinity), taxaname=factor(taxaname)) %>% 
            ggplot(aes(x=Salinity, y=parameter_value)) +
            geom_point(size=0.5, alpha=0.5) +
            geom_smooth(method = "lm", se = FALSE, alpha=0.2, linewidth=0.5) +
            stat_poly_eq(
                aes(label = paste("p-value = ", signif(..p.value.., digits = 3), 
                                "\nR^2 = ", signif(..adj.r.squared.., digits = 3), sep = "")),
                formula = y ~ x,
                parse = FALSE
            ) +
            facet_wrap(~taxaname, nrow=2, scales="free_y") + 
            ggtitle("Phytoplankton abundance by Salinity") + theme_bw()
ggsave(plot, file="plots/phytoplankton_by_salinity.png", width=20, height=8)

plot <- WATER_METADATA_PHYTOPLANKTON %>% filter(taxaname %in% sample_species) %>% 
            select(taxaname, `Oxygen saturation %`, parameter_value) %>% mutate(`Oxygen saturation %`=as.numeric(`Oxygen saturation %`), taxaname=factor(taxaname)) %>% 
            ggplot(aes(x=`Oxygen saturation %`, y=parameter_value)) +
            geom_point(size=0.5, alpha=0.5) +
            geom_smooth(method = "lm", se = FALSE, alpha=0.2, linewidth=0.5) +
            stat_poly_eq(
                aes(label = paste("p-value = ", signif(..p.value.., digits = 3), 
                                "\nR^2 = ", signif(..adj.r.squared.., digits = 3), sep = "")),
                formula = y ~ x,
                parse = FALSE
            ) +
            facet_wrap(~taxaname, nrow=2, scales="free_y") + 
            ggtitle("Phytoplankton abundance by Oxygen saturation %") + theme_bw()
ggsave(plot, file="plots/phytoplankton_by_o2_pct.png", width=20, height=8)

plot <- WATER_METADATA_PHYTOPLANKTON %>% filter(taxaname %in% sample_species) %>% 
            select(taxaname, datecollected, parameter_value) %>% mutate(datecollected=as.Date(datecollected), taxaname=factor(taxaname)) %>% 
            ggplot(aes(x=datecollected, y=parameter_value)) +
            geom_point(size=0.5, alpha=0.5) +
            geom_smooth(method = "lm", se = FALSE, alpha=0.2, linewidth=0.5) +
            stat_poly_eq(
                aes(label = paste("p-value = ", signif(..p.value.., digits = 3), 
                                "\nR^2 = ", signif(..adj.r.squared.., digits = 3), sep = "")),
                formula = y ~ x,
                parse = FALSE
            ) +
            facet_wrap(~taxaname, nrow=2, scales="free_y") + 
            ggtitle("Phytoplankton abundance by date") + theme_bw()
ggsave(plot, file="plots/phytoplankton_by_date.png", width=20, height=8)


lm_model <- lm(parameter_value ~ Temperature + pH + `Dissolved oxygen` + `Oxygen saturation %` + Salinity , data = WATER_METADATA_PHYTOPLANKTON %>% filter(taxaname=="Plagioselmis"))
summary(lm_model)

summary(lm(parameter_value ~ `Oxygen saturation %` , data = WATER_METADATA_PHYTOPLANKTON %>% filter(taxaname=="Plagioselmis")))
summary(lm(parameter_value ~ Salinity , data = WATER_METADATA_PHYTOPLANKTON %>% filter(taxaname=="Plagioselmis")))

plot_value_by_var <- function(variable) {
    plot <- WATER_METADATA_PHYTOPLANKTON %>% filter(taxaname %in% sample_species) %>% 
                select(taxaname, !!sym(variable), parameter_value) %>% mutate(var=as.numeric(!!sym(variable)), taxaname=factor(taxaname)) %>% 
                ggplot(aes(x=!!sym(variable), y=parameter_value)) +
                geom_point(size=0.5, alpha=0.5) +
                geom_smooth(method = "lm", se = FALSE, alpha=0.2, linewidth=0.5) +
                stat_poly_eq(
                    aes(label = paste("p-value = ", signif(..p.value.., digits = 3), 
                                    "\nR^2 = ", signif(..adj.r.squared.., digits = 3), sep = "")),
                    formula = y ~ x,
                    parse = FALSE
                ) +
                facet_wrap(~taxaname, nrow=2, scales="free_y") + 
                ggtitle(paste0("Phytoplankton abundance by ", variable)) + theme_bw()
    ggsave(plot, file=paste0("plots/phytoplankton_by_", gsub(" ", "", variable), ".png"), width=20, height=8)
}

plot_value_by_var("decimallongitude")
lapply(c("decimallatitude", "Suspended solids", "Secchi disk", "Silicate", "Nitrite", "Ammonium", "Turbidity", "Nitrate"), plot_value_by_var)

names(which(colSums(is.na(WATER_METADATA_PHYTOPLANKTON)) < 1000))
#################################################
##### General plots #####
WATER_METADATA_PHYTOPLANKTON %>% group_by(siteid) %>% summarise(n=n()) %>% arrange(desc(n))
plot <- WATER_METADATA_PHYTOPLANKTON %>% ggplot() + geom_histogram(aes(x=taxaname), stat="count") 
ggsave(plot, file="plots/phytoplankton_histogram.png", width=10, height=4)

#################################################
#################################################
