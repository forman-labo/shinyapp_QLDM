#install.packages(c("shiny", "shinythemes", "leaflet", "sf", "plotly", "scales", "shinyjs", "markdown"))
library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(plotly)
library(scales)
library(shinyjs)
library(markdown)
library(ggplot2)


#setwd("C:/Users/FRPOI91/OneDrive - Université Laval/Documents/GitHub/shinyapp_QLDM")

scencuts<-read.table("graph_recolte/cuts.txt", header = T)

scensppcuts<-read.table("graph_recolte/sppcut.txt", header = T)
scensppcuts$spp <- factor(scensppcuts$spp, levels = c("Résineux", "Feuillus tolérants", "Feuillus intolérants", "Plantations résineuses"),
                          labels = c("Résineux", "Feuillus\ntolérants", "Feuillus\nintolérants", "Plantations\nrésineuses"))
names(scensppcuts)[names(scensppcuts) == "spp"] <- "Type de\npeuplement"

scenspp<-read.table("graph_compo/spp.txt", header = T)
scenspp$spp <- factor(scenspp$spp, levels = c("Résineux", "Feuillus tolérants", "Feuillus intolérants", "Plantations résineuses"),
                      labels = c("Résineux", "Feuillus\ntolérants", "Feuillus\nintolérants", "Plantations\nrésineuses"))
names(scenspp)[names(scenspp) == "spp"] <- "Type de\npeuplement"

scenage<-read.table("graph_compo/age.txt", header = T)
scenage$age.class <- factor(scenage$age.class, levels = c("C10", "C30", "C50", "C70", "C90", "OLD"))
levels(scenage$age.class) <- c("10 ans", "30 ans", "50 ans", "70 ans", "90 ans", "plus de\n100 ans")
names(scenage)[names(scenage) == "age.class"] <- "Classe d'âge"

scenbr<-read.table("graph_feux/br.txt", header = T)
names(scenbr)[names(scenbr) == "Climat.Végétation"] <- "Climat et Végétation"

scenfire<-read.table("graph_feux/fire.txt", header = T)

scencarbone <- read.table("graph_carbone/carbone.txt", header = T)
colnames(scencarbone)[4:20] <- gsub("\\.", " ", colnames(scencarbone)[4:20])

scenrege <- read.table("graph_rege_PL/regen.txt", header = T)
names(scenrege)[names(scenrege) == "area.per"] <- "Superficie périodique"
names(scenrege)[names(scenrege) == "area.total"] <- "Superficie totale"

scenpl <- read.table("graph_rege_PL/pl.txt", header = T)
names(scenpl)[names(scenpl) == "area.acc.mean"] <- "Accident reboisé"
names(scenpl)[names(scenpl) == "area.CT.mean"] <- "Coupe totale reboisée"
names(scenpl)[names(scenpl) == "area.salv.mean"] <- "Coupe de récupération reboisée"
names(scenpl)[names(scenpl) == "area.total.mean"] <- "Total reboisé"

UA.pub <- read_sf("shp/UApub.shp")
UA.centre <- read_sf("shp/UAcentre.shp")

####AREA
plot_area <- function(data, mgmt.unit, scenario) {
  p<-ggplot() +
    geom_line(data = subset(data[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==1, ]),
              aes(x=Année, y=Superficie, group = run, colour="Sans\nfeu"), linewidth = 1) +
    geom_line(data = subset(data[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0,]),
              aes(x=Année, y=Superficie, group = run, colour="Avec\nfeux"), linewidth = 0.2) +
    geom_hline(yintercept = min(data$min_area[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0 & data$Année>2020]),
               linetype = "dashed", linewidth = 0.1) +
    geom_hline(yintercept = max(data$min_area[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0 & data$Année>2020]),
               linetype = "dashed", linewidth = 0.1) +
    geom_hline(yintercept = median(data$min_area[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0 & data$Année>2020]),
               linetype = "dashed", linewidth = 0.1) +
    annotate("text", size=3.4, x=2030, y = (min(data$Superficie[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==1 & data$Année>2020])/100)*18,
             label = paste0("min: ", round(min(data$min_area[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0 & data$Année>2020]),2), " km²"))+
    annotate("text", size=3.4, x=2030, y = (min(data$Superficie[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==1 & data$Année>2020])/100)*30,
             label = paste0("med:", round(median(data$min_area[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0 & data$Année>2020]),2), " km²"))+
    annotate("text", size=3.4, x=2030, y = (min(data$Superficie[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==1 & data$Année>2020])/100)*42,
             label = paste0("max: ", round(max(data$min_area[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0 & data$Année>2020]),2), " km²"))+
    theme_minimal()+
    scale_y_continuous(breaks = pretty_breaks(n=6))+scale_x_continuous(breaks = pretty_breaks(n=6))+
    scale_color_manual(name="Scénario", values = c("Avec\nfeux" = "darkgrey", "Sans\nfeu" = "red"), breaks = c("Avec\nfeux", "Sans\nfeu"))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    ggtitle(paste0("<b>Impact des feux sur le taux de récolte</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scencuts$Superficie[scencuts$mgmt.unit==mgmt.unit]))) +
    labs(x = "Année", y = "Superficie récoltée (km² ∙ per⁻¹)") +
    expand_limits(x = 2020, y = 0)
  
   ggplotly(p, tooltip = c("Année", "Superficie", "run")) %>% layout(margin = list(t=50), legend = list(x=0.97)) 
     
}

plot_area_nofire <- function(data, mgmt.unit, scenario) {
  p <- ggplot() +
    geom_line(data = subset(data[data$mgmt.unit == mgmt.unit & data$scen == scenario,]),
              aes(x=Année, y=Superficie,), linewidth = 1, color="red") +
    geom_hline(yintercept = max(data$min_area[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$Année>2020]),
               linetype = "dashed", linewidth = 0.1) +
    annotate("text", size=3.4, x=2030, y = (min(data$Superficie[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$Année>2020])/100)*50,
             label = paste0("min: ", round(min(data$min_area[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$Année>2020]),2), " km²"))+
    theme_minimal() +
    scale_y_continuous(breaks = pretty_breaks(n=6))+scale_x_continuous(breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    ggtitle(paste0("<b>Taux de récolte sans feu</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scencuts$Superficie[scencuts$mgmt.unit==mgmt.unit]))) +
    labs(x = "Année", y = "Superficie récoltée (km² ∙ per⁻¹)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Année", "Superficie")) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97)) %>%
    highlight(on = "plotly_click", off = "plotly_doubleclick", opacityDim = 0.1)
}

plot_area_nocc <- function(data, mgmt.unit, scenario) {
  p <- ggplot(data = subset(data[data$mgmt.unit == mgmt.unit & data$scen == scenario,])) +
    geom_blank()+
    theme_minimal() +
    annotate("text", size=7, x=2060, y = 0, label = "Aucune récolte")+
    scale_y_continuous(breaks = pretty_breaks(n=6))+scale_x_continuous(breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    ggtitle(paste0("<b>Taux de récolte</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100)) +
    labs(x = "Année", y = "Superficie récoltée (km² ∙ per⁻¹)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Année", "Superficie")) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97)) %>%
    highlight(on = "plotly_click", off = "plotly_doubleclick", opacityDim = 0.1)
}

color_spp <-c("Résineux"="#F8766D","Feuillus tolérants"="#C49A00", "Feuillus intolérants"="#00BA38", "Plantations résineuses"="#619CFF")

plot_sppcut <- function(data, mgmt.unit, scenario) {
p <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année, y = Proportion, color=`Type de\npeuplement`)) +
             geom_line(linewidth = 0.5)+
             theme_minimal() +
             scale_y_continuous(labels= scales::percent_format(scale=100), breaks = pretty_breaks(n=6))+
             theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11), legend.title = element_text(size = 10)) +
             ggtitle(paste0("<b>Taux de récolte par type de peuplement</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
             coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scensppcuts$Proportion[scensppcuts$mgmt.unit==mgmt.unit]))) +
             labs(x = "Année", y = "Taux de récolte", color="Type de\npeuplement") +
             scale_color_manual(values = color_spp)+
             expand_limits(x = 2020, y = 0)
             
ggplotly(p) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
}

plot_sppcut_nocc <- function(data, mgmt.unit, scenario) {
  p <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année, y = Proportion, color=`Type de\npeuplement`)) +
    geom_blank()+
    theme_minimal() +
    scale_y_continuous(labels= scales::percent_format(scale=100), breaks = pretty_breaks(n=6))+
    annotate("text", size=7, x=2060, y = 0, label = "Aucune récolte")+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11), legend.title = element_text(size = 10)) +
    ggtitle(paste0("<b>Taux de récolte par type de peuplement</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100)) +
    labs(x = "Année", y = "Taux de récolte", color="Type de\npeuplement") +
    expand_limits(x = 2020, y = 0)

  ggplotly(p) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
}

####VOLUME
plot_vol <- function(data, mgmt.unit, scenario) {
  p<-ggplot() +
    geom_line(data = subset(data[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==1, ]),
              aes(x=Année, y=Volume, group = run, colour="Sans\nfeu"), linewidth = 1) +
    geom_line(data = subset(data[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0,]),
              aes(x=Année, y=Volume, group = run, colour="Avec\nfeux"), linewidth = 0.2) +
    geom_hline(yintercept = min(data$min_vol[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0 & data$Année>2020]),
               linetype = "dashed", linewidth = 0.1) +
    geom_hline(yintercept = max(data$min_vol[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0 & data$Année>2020]),
               linetype = "dashed", linewidth = 0.1) +
    geom_hline(yintercept = median(data$min_vol[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0 & data$Année>2020]),
               linetype = "dashed", linewidth = 0.1) +
    annotate("text", size=3.4, x=2030, y = (min(data$Volume[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==1 & data$Année>2020])/100)*18,
             label = paste0("min: ", format(round(min(data$min_vol[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0 & data$Année>2020]),-3), big.mark=" "), " m³"))+
    annotate("text", size=3.4, x=2030, y = (min(data$Volume[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==1 & data$Année>2020])/100)*30,
             label = paste0("med: ", format(round(median(data$min_vol[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0 & data$Année>2020]),-3), big.mark=" "), " m³"))+
    annotate("text", size=3.4, x=2030, y = (min(data$Volume[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==1 & data$Année>2020])/100)*42,
             label = paste0("max: ", format(round(max(data$min_vol[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$baseline==0 & data$Année>2020]),-3), big.mark=" "), " m³"))+
    theme_minimal()+
    scale_y_continuous(labels = function(x) x / 1000, breaks = pretty_breaks(n=6))+
    scale_x_continuous(breaks = pretty_breaks(n=6))+
    scale_color_manual(name="Scénario", values = c("Avec\nfeux" = "darkgrey", "Sans\nfeu" = "red"), breaks = c("Avec\nfeux", "Sans\nfeu"))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    ggtitle(paste0("<b>Impact des feux sur le taux de récolte</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scencuts$Volume[scencuts$mgmt.unit==mgmt.unit]))) +
    labs(x = "Année", y = "Volume récolté (1000 m³ ∙ per⁻¹)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Année", "Volume", "run")) %>% layout(margin = list(t=50), legend = list(x=0.97)) 
  
}

plot_vol_nofire <- function(data, mgmt.unit, scenario) {
  p <- ggplot() +
    geom_line(data = subset(data[data$mgmt.unit == mgmt.unit & data$scen == scenario,]),
              aes(x=Année, y=Volume,), linewidth = 1, color="red") +
    geom_hline(yintercept = max(data$min_vol[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$Année>2020]),
               linetype = "dashed", linewidth = 0.1) +
    annotate("text", size=3.4, x=2030, y = (min(data$Volume[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$Année>2020])/75)*50,
             label = paste0("min: ", format(round(min(data$min_vol[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$Année>2020]),-3), big.mark=" "), " m³"))+
    theme_minimal() +
    scale_y_continuous(labels = function(x) x / 1000, breaks = pretty_breaks(n=6))+
    scale_x_continuous(breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    ggtitle(paste0("<b>Taux de récolte sans feu</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scencuts$Volume[scencuts$mgmt.unit==mgmt.unit]))) +
    labs(x = "Année", y = "Volume récolté (1000 m³ ∙ per⁻¹)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Année", "Volume")) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97)) %>%
    highlight(on = "plotly_click", off = "plotly_doubleclick", opacityDim = 0.1)
}

plot_vol_nocc <- function(data, mgmt.unit, scenario) {
  p <- ggplot(data = subset(data[data$mgmt.unit == mgmt.unit & data$scen == scenario,])) +
    geom_blank()+
    theme_minimal() +
    annotate("text", size=7, x=2060, y = 0, label = "Aucune récolte")+
    scale_y_continuous(breaks = pretty_breaks(n=6))+scale_x_continuous(breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    ggtitle(paste0("<b>Taux de récolte</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100)) +
    labs(x = "Année", y = "Volume récolté (m³ ∙ per⁻¹)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Année", "Volume")) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97)) %>%
    highlight(on = "plotly_click", off = "plotly_doubleclick", opacityDim = 0.1)
}

color_spp <-c("Résineux"="#F8766D","Feuillus\ntolérants"="#C49A00", "Feuillus\nintolérants"="#00BA38", "Plantations\nrésineuses"="#619CFF")

plot_sppcut_vol <- function(data, mgmt.unit, scenario) {
  p <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année, y = Proportion.volume, color=`Type de\npeuplement`)) +
    geom_line(linewidth = 0.5)+
    theme_minimal() +
    scale_y_continuous(labels= scales::percent_format(scale=100), breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11), legend.title = element_text(size = 10)) +
    ggtitle(paste0("<b>Taux de récolte par type de peuplement</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scensppcuts$Proportion.volume[scensppcuts$mgmt.unit==mgmt.unit]))) +
    labs(x = "Année", y = "Taux de récolte", color="Type de\npeuplement") +
    scale_color_manual(values = color_spp)+
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
}

plot_sppcut_nocc_vol <- function(data, mgmt.unit, scenario) {
  p <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année, y = Proportion.volume, color=`Type de\npeuplement`)) +
    geom_blank()+
    theme_minimal() +
    scale_y_continuous(labels= scales::percent_format(scale=100), breaks = pretty_breaks(n=6))+
    annotate("text", size=7, x=2060, y = 0, label = "Aucune récolte")+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11), legend.title = element_text(size = 10)) +
    ggtitle(paste0("<b>Taux de récolte par type de peuplement</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100)) +
    labs(x = "Année", y = "Taux de récolte", color="Type de\npeuplement") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
}


plot_spp <- function(data, mgmt.unit, scenario) {
p <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année, y = Proportion, color=`Type de\npeuplement`)) +
             geom_line(linewidth = 0.5)+
             theme_minimal() +
             scale_y_continuous(labels= scales::percent_format(scale=100), breaks = pretty_breaks(n=6))+
             theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11), legend.title = element_text(size = 10)) +
             ggtitle(paste0("<b>Taux d'occupation par type de peuplement</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
             coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scenspp$Proportion[scenspp$mgmt.unit==mgmt.unit]))) +
             labs(x = "Année", y = "Taux d'occupation", color="Type de\npeuplement") +
             scale_color_manual(values = color_spp)+
             expand_limits(x = 2020, y = 0)

ggplotly(p) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))

}

color_age <-c("plus de\n100 ans"="#F8766D","90 ans"="#C49A00","70 ans"="#93AA00", "50 ans"="#00BA38", "30 ans"="#00B9E3","10 ans"="#DB72FB")

plot_age <- function(data, mgmt.unit, scenario) {
  p <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année, y = Proportion, color=`Classe d'âge`)) +
    geom_line(linewidth = 0.5)+
    theme_minimal() +
    scale_y_continuous(labels= scales::percent_format(scale=100), breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11), legend.title = element_text(size = 10)) +
    ggtitle(paste0("<b>Taux d'occupation par classe d'âge</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scenage$Proportion[scenage$mgmt.unit==mgmt.unit]))) +
    labs(x = "Année", y = "Taux d'occupation", color="Classe d'âge") +
    scale_color_manual(values = color_age)+
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
  
}

plot_fire <- function(data, mgmt.unit, scenario) {
p  <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$Année>2025, ], aes(x = Année)) +
             geom_line(linewidth = 1, aes(y=Superficie), color="red") +
             geom_hline(yintercept = scenfire$med_area[scenfire$mgmt.unit==mgmt.unit & scenfire$Année>2025 & scenfire$scen==scenario],
                        linetype = "dashed", linewidth = 0.1) +
             annotate("text", size=3.4, x = 2025, y = 0.9*scenfire$med_area[scenfire$mgmt.unit==mgmt.unit & scenfire$Année>2025 & scenfire$scen==scenario],
                      label = paste0("médiane: ", round(scenfire$med_area[scenfire$mgmt.unit==mgmt.unit & scenfire$Année>2025 & scenfire$scen==scenario],0), " km²"))+
             theme_minimal() +
             scale_y_continuous(breaks = pretty_breaks(n=6))+
             theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
             ggtitle(paste0("<b>Superficie brûlée</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
             coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scenfire$Superficie[scenfire$mgmt.unit==mgmt.unit & scenfire$Année>2025]))) +
             labs(x = "Année", y = "Superficie brûlée (km² ∙ per⁻¹)") +
             expand_limits(x = 2020, y = 0)
  
ggplotly(p, tooltip = c("Année", "Superficie")) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
  
}
# 
# plot_fire <- function(data, mgmt.unit, scenario) {
#   p  <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$Année>2025, ], aes(x = Année)) +
#     geom_line(linewidth = 1, aes(y=Superficie), color="red") +
#     theme_minimal() +
#     scale_y_continuous(breaks = pretty_breaks(n=6))+
#     theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
#     ggtitle(paste0("<b>Superficie brûlée</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
#     coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scenfire$Superficie[scenfire$mgmt.unit==mgmt.unit & scenfire$Année>2025]))) +
#     labs(x = "Année", y = "Superficie brûlée (km² ∙ per⁻¹)") +
#     expand_limits(x = 2020, y = 0)
#   
#   ggplotly(p, tooltip = c("Année", "Superficie")) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
#   
# }


plot_fire_nofire <- function(data, mgmt.unit, scenario) {
  p  <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année)) +
    geom_blank() +
    theme_minimal() +
    scale_y_continuous(breaks = pretty_breaks(n=6))+
    annotate("text", size=7, x=2060, y = 0, label = "Aucune superficie brûlée")+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    ggtitle(paste0("<b>Superficie brûlée</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100)) +
    labs(x = "Année", y = "Superficie brûlée (km² ∙ per⁻¹)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Année", "Superficie")) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
  
}

plot_br <- function(data, mgmt.unit, scenario) {
  p  <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année)) +
    geom_line(linewidth = 1, aes(y=Historique, color="Historique\n(sans modificateur)"), show.legend = T) +
    geom_line(linewidth = 0.8, aes(y=Climat, color="Climat"), show.legend = T) +
    geom_line(linewidth = 0.8, aes(y=`Climat et Végétation`, color="Climat et\nCombustibles"), show.legend = T) +
    theme_minimal() +
    scale_y_continuous(labels= scales::percent_format(scale=100), breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    scale_color_manual(name="Modificateur\ndu taux de brûlage", values = c("Historique\n(sans modificateur)" = "black", "Climat" = "#00B9E3", "Climat et\nCombustibles"="red"),
                       breaks = c("Historique\n(sans modificateur)", "Climat", "Climat et\nCombustibles"))+
    ggtitle(paste0("<b>Taux de brûlage potentiel en fonction du climat et des combustibles</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scenbr$`Climat et Végétation`[scenbr$mgmt.unit==mgmt.unit],
                                                          scenbr$`Climat`[scenbr$mgmt.unit==mgmt.unit]))) +
    labs(x = "Année", y = "Taux de brûlage (% ∙ année⁻¹)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Année", "Historique", "Climat", "Climat et Végétation")) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
  
}

plot_rege <- function(data, mgmt.unit, scenario) {
  p  <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année)) +
    geom_line(linewidth = 1, aes(y=`Superficie totale`, color="Superficie\ntotale"), show.legend = T) +
    geom_line(linewidth = 1, aes(y=`Superficie périodique`, color="Superficie\npériodique"), show.legend = T) +
    theme_minimal() +
    scale_y_continuous(breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    scale_color_manual(name="",values = c("Superficie\npériodique"= "#00B9E3", "Superficie\ntotale"= "red"))+
    ggtitle(paste0("<b>Superficie affectée par des échecs de régénération</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scenrege$`Superficie totale`[scenrege$mgmt.unit==mgmt.unit]))) +
    labs(x = "Année", y = "Superficie affectée (km²)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Année", "Superficie périodique", "Superficie totale")) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
  
}

plot_rege_nofire <- function(data, mgmt.unit, scenario) {
  p  <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année)) +
    geom_blank() +
    theme_minimal() +
    scale_y_continuous(breaks = pretty_breaks(n=6))+
    annotate("text", size=7, x=2060, y = 0, label = "Aucune superficie brûlée")+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    ggtitle(paste0("<b>Superficie affectée par des échecs de régénération</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100)) +
    labs(x = "Année", y = "Superficie affectée (km²)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Année", "Superficie périodique", "Superficie totale")) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
  
}

plot_pl <- function(data, mgmt.unit, scenario) {
  p  <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année)) +
    geom_line(linewidth = 0.5, aes(y=`Coupe totale reboisée`, color="Coupe totale reboisée"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=`Accident reboisé`, color="Accident de\nrégénération reboisé"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=`Coupe de récupération reboisée`, color="Coupe de\nrécupération reboisée"), show.legend = T) +
    geom_line(linewidth = 1, aes(y=`Total reboisé`, color="Total reboisé"), show.legend = T) +
    theme_minimal() +
    scale_y_continuous(breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    scale_color_manual(name="",values = c("Total reboisé" = "red", "Accident de\nrégénération reboisé"= "#00BA38",
                                          "Coupe totale reboisée"= "#00B9E3", "Coupe de\nrécupération reboisée"="#DB72FB"))+
    ggtitle(paste0("<b>Superficie reboisée</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scenpl$`Total reboisé`[scenpl$mgmt.unit==mgmt.unit]))) +
    labs(x = "Année", y = "Superficie reboisée (km²)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Année", "Total reboisé", "Accident reboisé", "Coupe totale reboisée", "Coupe de récupération reboisée")) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
  
}

plot_pl_nocc <- function(data, mgmt.unit, scenario) {
  p  <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année)) +
    geom_blank() +
    theme_minimal() +
    scale_y_continuous(breaks = pretty_breaks(n=6))+
    annotate("text", size=7, x=2060, y = 0, label = "Aucune superficie reboisée")+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    ggtitle(paste0("<b>Superficie reboisée</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100)) +
    labs(x = "Année", y = "Superficie reboisée (km²)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Année", "Total reboisé", "Accident reboisé", "Coupe totale reboisée", "Coupe de récupération reboisée")) %>% layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
  
}


plot_biomasse <- function(data, mgmt.unit, scenario) {
  p  <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année)) +
    geom_line(linewidth = 0.5, aes(y=`Bois marchand`, color="Bois\nmarchand"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=`Bois autre`, color="Bois autre"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=Feuillage, color="Feuillage"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=`Racines grossières`, color="Racines\ngrossières"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=`Racines fines`, color="Racines\nfines"), show.legend = T) +
    theme_minimal() +
    scale_y_continuous(breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    scale_color_manual(name="Réservoirs de\nbiomasse vivante", values = c("Bois\nmarchand" = "#00B9E3", "Bois autre" = "#F8766D",
                                                                                   "Feuillage"="#00BA38", "Racines\ngrossières"="#C49A00",
                                                                                   "Racines\nfines" ="#DB72FB"))+
    ggtitle(paste0("<b>Stocks de carbone dans la biomasse vivante</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scencarbone$`Bois marchand`[scencarbone$mgmt.unit==mgmt.unit]))) +
    labs(x = "Année", y = "Contenu en carbone (tC ∙ ha⁻¹)") +
    expand_limits(x = 2020, y = 0)

  ggplotly(p, tooltip = c("Bois marchand", "Bois autre", "Feuillage", "Racines grossières", "Racines fines")) %>%
    layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))

}

plot_reservoir <- function(data, mgmt.unit, scenario) {
  p  <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario, ], aes(x = Année)) +
    geom_line(linewidth = 0.5, aes(y=`Bois mort`, color="Bois mort\n(marchand et autre)"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=`très rapide`, color="Décomposition\ntrès rapide"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=`rapide`, color="Décomposition\nrapide"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=`moyenne`, color="Décomposition\nmoyenne"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=`lente aérien`, color="Décomposition\nlente (aérien)"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=`lente souterrain`, color="Décomposition\nlente (souterrain)"), show.legend = T) +
    theme_minimal() +
    scale_y_continuous(breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    scale_color_manual(name="Bois mort et réservoirs\nde carbone du sol\n<i><span style='font-size:9pt;'>(par vitesse de décomposition)</span><i>",
                       values = c("Bois mort\n(marchand et autre)" = "#00B9E3",
                                  "Décomposition\ntrès rapide" = "#F8766D",
                                  "Décomposition\nrapide" = "#00BA38",
                                  "Décomposition\nrapide" = "#C49A00",
                                  "Décomposition\nlente (aérien)" ="#DB72FB",
                                  "Décomposition\nlente (souterrain)" ="black"))+
    ggtitle(paste0("<b>Stocks de carbone dans le bois mort et les réservoirs du sol</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scencarbone$`lente souterrain`[scencarbone$mgmt.unit==mgmt.unit]))) +
    labs(x = "Année", y = "Contenu en carbone (tC ∙ ha⁻¹)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Bois mort", "très rapide", "rapide", "moyenne", "lente aérien",  "lente souterrain")) %>%
    layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
  
}

plot_transfert <- function(data, mgmt.unit, scenario) {
  p  <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$Année >2020, ], aes(x = Année)) +
    geom_line(linewidth = 0.5, aes(y=`Atmosphère décomposition`, color="Vers l'atmosphère\n(décomposition)"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=`Atmosphère perturbation`, color="Vers l'atmosphère\n(perturbation)"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=`Produits du bois`, color="Vers les produits\ndu bois"), show.legend = T) +
    theme_minimal() +
    scale_y_continuous(breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    scale_color_manual(name="Transferts de carbone",
                       values = c("Vers l'atmosphère\n(décomposition)" = "#00B9E3", "Vers l'atmosphère\n(perturbation)" = "#00BA38",
                                  "Vers les produits\ndu bois" = "#F8766D"))+
    ggtitle(paste0("<b>Transferts de carbone vers l'atmosphère et les produits du bois</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(0, max(scencarbone$`Atmosphère décomposition`[scencarbone$mgmt.unit==mgmt.unit],
                                                          scencarbone$`Atmosphère perturbation`[scencarbone$mgmt.unit==mgmt.unit]))) +
    labs(x = "Année", y = "Flux de carbone (tC ∙ ha⁻¹ ∙ an⁻¹)") +
    expand_limits(x = 2020, y = 0)
  
  ggplotly(p, tooltip = c("Atmosphère décomposition", "Atmosphère perturbation", "Produits du bois")) %>%
    layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
  
}

plot_NPP <- function(data, mgmt.unit, scenario) {
  p  <- ggplot(data[data$mgmt.unit == mgmt.unit & data$scen == scenario & data$Année >2020, ], aes(x = Année)) +
    geom_line(linewidth = 0.5, aes(y=PPN, color="Productivité\nprimaire nette\n(PPN)"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=PEN, color="Productivité\nde l'écosystème\nnette (PEN)"), show.legend = T) +
    geom_line(linewidth = 0.5, aes(y=PBN, color="Productivité\ndu biome nette\n(PBN)"), show.legend = T) +
    geom_hline(yintercept=0, linetype="solid", color="darkgrey", linewidth=0.3)+
    theme_minimal() +
    scale_y_continuous(breaks = pretty_breaks(n=6))+
    theme(axis.text.y = element_text(angle = 90), plot.title = element_text(size = 11)) +
    scale_color_manual(name="Indicateurs", values = c("Productivité\nprimaire nette\n(PPN)" = "#00B9E3",
                                                      "Productivité\nde l'écosystème\nnette (PEN)" = "#F8766D",
                                                      "Productivité\ndu biome nette\n(PBN)" = "#00BA38"))+
    ggtitle(paste0("<b>Indicateurs d'écosystème</b>", ifelse(mgmt.unit == "Forêt publique", "\n", "\nUA "), mgmt.unit, " - ", scenario)) +
    coord_cartesian(xlim = c(2020, 2100), ylim = c(min(0,
                                                       scencarbone$PBN[scencarbone$mgmt.unit==mgmt.unit & scencarbone$Année >2020]),
                                                   max(scencarbone$PPN[scencarbone$mgmt.unit==mgmt.unit & scencarbone$Année >2020]))) +
    labs(x = "Année", y = "Flux de carbone (tC ∙ ha⁻¹ ∙ an⁻)¹") +
    expand_limits(x = 2020)
  
  ggplotly(p, tooltip = c("PPN", "PEN", "PBN")) %>%
    layout(margin = list(t=50)) %>% layout(legend=list(x=0.97))
  
}

##UI###
ui<-fluidPage(
  tags$head(
    tags$style(HTML("
  .navbar { margin-bottom: 0px; }
    .main-container { 
      margin-left: 0px !important;  /* Force left alignment */
      padding-left: 0px !important;
      max-width: 100% !important;  /* Prevent excessive width */
    }  #content { max-width: 1000px; padding-left: 60px; }
"))
),
  useShinyjs(),
  navbarPage("",
             collapsible=F,
             tabPanel("À propos",
                      titlePanel(h2("Effet des changements climatiques sur la forêt"),
                                 tags$head(tags$style(HTML('#h2 {margin-top: 0px}')))),
                      fluidRow(
                        column(2,
                               navlistPanel(widths = c(12,12),
                                 id = "menu",
                                 tabPanel("Description générale"),
                                 tabPanel("Options du modèle"),
                                 tabPanel("Graphiques")
                               )
                        ),
                        column(10,  # This column will contain the mainPanel (content area)
                               div(id = "content", 
                                   uiOutput("html_display")
                               )
                        )
                      )
             ),
             tabPanel("Résultats",
  titlePanel(h2("Effet des changements climatiques sur la forêt"),
             tags$head(tags$style(HTML('#h2 {margin-top: 0px}')))),
  sidebarLayout(
    sidebarPanel(width = 3,
                 radioButtons("clim",
                              "Scénario climatique",
                              choices = c("climat stable", "SSP2-4.5", "SSP3-7.0"),
                                          selected = "climat stable"
                 ),
                 checkboxGroupInput("perturb",
                                    "Perturbations",
                                    choices = c("feux", "récolte"),
                                    inline = T
                 ),
                 radioButtons("bonifie",
                                "Scénario",
                              choices = list("Scénario de référence" = "reference",
                                             "Taux de récupération rehaussé" = "recup70%",
                                             "Cible de reboisement rehaussée" = "reboisement",
                                             "Mise en place d'un fonds de réserve" = "reserve"),
                              selected = "reference"
                 ),
                 tags$style(HTML("
        .disabled input[type='radio'] {
          pointer-events: none !important;  /* Disable interaction */
          opacity: 0.3 !important;  /* Reduce opacity for a gray effect */

        }
        .disabled {
          cursor: not-allowed !important;
        }
        .disabled label {
          color: #999 !important;  /* Gray out the label */
        }
      "))
    ),
    mainPanel(leafletOutput("carte", height = "100%"), width = 9, style = "height: 40vh")),
  tabsetPanel(
    tabPanel("Feux",
          conditionalPanel(
            condition = "input.perturb.includes('feux')",
              mainPanel(
                width = 12,
                div(
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  div(
                    style = "flex: 1; padding-right: 10px;",
                    plotlyOutput("plotfire", height = "44vh")
                  ),
                  div(
                    style = "flex: 1; padding-left: 10px;",
                    plotlyOutput("plotbr", height = "44vh")
                  )
                )
              )
          ),
          conditionalPanel(
            condition = "!input.perturb.includes('feux')",
            mainPanel(
              width = 12,
              div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                div(
                  style = "flex: 1; padding-right: 10px;",
                  plotlyOutput("plotfirenofire", height = "44vh")
                ),
                div(
                  style = "flex: 1; padding-left: 10px;",
                  plotlyOutput("plotbrnofire", height = "44vh")
                )
              )
            )
          )
),
tabPanel("Échecs et plantation",
         conditionalPanel(
           condition = "input.perturb.includes('feux') && input.perturb.includes('récolte')",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plotechec", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotpl", height = "44vh")
               )
             )
           )
         ),
         conditionalPanel(
           condition = "!input.perturb.includes('feux') && input.perturb.includes('récolte')",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plotechecnofire", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotplnofire", height = "44vh")
               )
             )
           )
         ),
         conditionalPanel(
           condition = "input.perturb.includes('feux') && !input.perturb.includes('récolte')",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plotechecnocc", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotplnocc", height = "44vh")
               )
             )
           )
         ),
         conditionalPanel(
           condition = "!input.perturb.includes('feux') && !input.perturb.includes('récolte')",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plotechecnopert", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotplnopert", height = "44vh")
               )
             )
           )
         )
),
tabPanel("Récolte (Superficie)",
         conditionalPanel(
           condition = "input.perturb.includes('feux') && input.perturb.includes('récolte')",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plotcut", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotsppcut", height = "44vh")
               )
             )
           )
         ),
         conditionalPanel(
           condition = "!input.perturb.includes('feux') && input.perturb.includes('récolte')",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plotcutnofire", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotsppcutnofire", height = "44vh")
               )
             )
           )
         ),
         conditionalPanel(
           condition = "!input.perturb.includes('récolte')",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plotcutnocc", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotsppcutnocc", height = "44vh")
               )
             )
           )
         )
),
tabPanel("Récolte (Volume)",
         conditionalPanel(
           condition = "input.perturb.includes('feux') && input.perturb.includes('récolte')",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plotcutvol", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotsppcutvol", height = "44vh")
               )
             )
           )
         ),
         conditionalPanel(
           condition = "!input.perturb.includes('feux') && input.perturb.includes('récolte')",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plotcutvolnofire", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotsppcutvolnofire", height = "44vh")
               )
             )
           )
         ),
         conditionalPanel(
           condition = "!input.perturb.includes('récolte')",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plotcutvolnocc", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotsppcutvolnocc", height = "44vh")
               )
             )
           )
         )
),
tabPanel("Composition et âge",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plotspp", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotage", height = "44vh")
               )
             )
           )
         ),
tabPanel("Carbone (Stocks)",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plotbiomasse", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotreservoir", height = "44vh")
               )
             )
           )
         ),
tabPanel("Carbone (Transferts et indicateurs)",
           mainPanel(
             width = 12,
             div(
               style = "display: flex; justify-content: space-between; align-items: center;",
               div(
                 style = "flex: 1; padding-right: 10px;",
                 plotlyOutput("plottransfert", height = "44vh")
               ),
               div(
                 style = "flex: 1; padding-left: 10px;",
                 plotlyOutput("plotNPP", height = "44vh")
               )
             )
           )
         )
)
)
)
)

server <- function(input, output, session){

###a propos
  output$html_display <- renderUI({
    file <- switch(input$menu,
                   "Description générale" = "markdown_info/info.md",
                   "Options du modèle" = "markdown_info/options.md",
                   "Graphiques" = "markdown_info/graph.md")
    
    includeMarkdown(file)
  })

###resultats
  disable("bonifie")
  
  
  map.scen <- reactive({
    if (all(c("feux", "récolte") %in% input$perturb)) {
      # Cas 1 : feux ET Récolte cochés
      scenspp %>%
        filter(ssp == input$clim, bonif == input$bonifie)
      
    } else if ("feux" %in% input$perturb) {
      # seulement feux
      scenspp %>%
        filter(ssp == input$clim, bonif == "sans coupe")
      
    } else if ("récolte" %in% input$perturb) {
      # seulement récole
      scenspp %>%
        filter(ssp == input$clim, bonif == "sans feu")
      
    } else {
      # aucun
      scenspp %>%
        filter(ssp == input$clim, bonif == "sans pert")
    }
  })
  
  output$carte <- renderLeaflet({
    UA.pub  %>%
      leaflet() %>%
      addTiles() %>% 
      addPolygons(stroke = T,
                  weight = 0.6,
                  smoothFactor = 1,
                  group="polygons",
                  layerId = ~mgmt_unit,
                  fillOpacity = 0.6,
                  fillColor = "green",
                  color = "white",
                  highlightOptions = highlightOptions(weight = 2,
                                                      fillOpacity = 1,
                                                      opacity=1,
                                                      bringToFront = T))%>%
      addLabelOnlyMarkers(data=UA.centre, label = ~mgmt_unit,
                          labelOptions = labelOptions(noHide = T, textOnly = T))
    })
  observe({

    # Vérifie si les 2 options sont cochées
    if (!is.null(input$perturb) && all(c("feux", "récolte") %in% input$perturb)) {
      enable("bonifie")
    } else {
      disable("bonifie")
    }
    
    
    event <- input$carte_shape_click
    if(is.null(event))
      return()
    unit <- paste(UA.pub$mgmt_unit[UA.pub$mgmt_unit == event$id])
    
    ##NFIRE&BR##
      output$plotfire <- renderPlotly({
      plot_fire(scenfire, unit, unique(map.scen()$scen))
    })
    output$plotbr <- renderPlotly({
      plot_br(scenbr, unit, unique(map.scen()$scen))
    })
    output$plotfirenofire <- renderPlotly({
      plot_fire_nofire(scenfire, unit, unique(map.scen()$scen))
    })
    output$plotbrnofire <- renderPlotly({
      plot_br(scenbr, unit, unique(map.scen()$scen))
    })
    
    ##REGE&PL##
    output$plotechec <- renderPlotly({
      plot_rege(scenrege, unit, unique(map.scen()$scen))
    })
    output$plotpl <- renderPlotly({
      plot_pl(scenpl, unit, unique(map.scen()$scen))
    })
    output$plotechecnofire <- renderPlotly({
      plot_rege_nofire(scenrege, unit, unique(map.scen()$scen))
    })
    output$plotplnofire <- renderPlotly({
      plot_pl(scenpl, unit, unique(map.scen()$scen))
    })
    output$plotechecnocc <- renderPlotly({
      plot_rege(scenrege, unit, unique(map.scen()$scen))
    })
    output$plotplnocc <- renderPlotly({
      plot_pl_nocc(scenpl, unit, unique(map.scen()$scen))
    })
    output$plotechecnopert <- renderPlotly({
      plot_rege_nofire(scenrege, unit, unique(map.scen()$scen))
    })
    output$plotplnopert <- renderPlotly({
      plot_pl_nocc(scenpl, unit, unique(map.scen()$scen))
    })
    
    ##CUTS###
    output$plotcut <- renderPlotly({
      plot_area(scencuts, unit, unique(map.scen()$scen))
    })
    output$plotsppcut <- renderPlotly({
      plot_sppcut(scensppcuts, unit, unique(map.scen()$scen))
    })
    output$plotcutvol <- renderPlotly({
      plot_vol(scencuts, unit, unique(map.scen()$scen))
    })
    output$plotsppcutvol <- renderPlotly({
      plot_sppcut_vol(scensppcuts, unit, unique(map.scen()$scen))
    })
    output$plotcutnofire <- renderPlotly({
      plot_area_nofire(scencuts, unit, unique(map.scen()$scen))
    })
    output$plotsppcutnofire <- renderPlotly({
      plot_sppcut(scensppcuts, unit, unique(map.scen()$scen))
    })
    output$plotcutvolnofire <- renderPlotly({
      plot_vol_nofire(scencuts, unit, unique(map.scen()$scen))
    })
    output$plotsppcutvolnofire <- renderPlotly({
      plot_sppcut_vol(scensppcuts, unit, unique(map.scen()$scen))
    })
    output$plotcutnocc <- renderPlotly({
      plot_area_nocc(scencuts, unit, unique(map.scen()$scen))
    })
    output$plotsppcutnocc <- renderPlotly({
      plot_sppcut_nocc(scensppcuts, unit, unique(map.scen()$scen))
    })
    output$plotcutvolnocc <- renderPlotly({
      plot_vol_nocc(scencuts, unit, unique(map.scen()$scen))
    })
    output$plotsppcutvolnocc <- renderPlotly({
      plot_sppcut_nocc_vol(scensppcuts, unit, unique(map.scen()$scen))
    })
    
    ##SPP##
    output$plotspp <- renderPlotly({
      plot_spp(scenspp, unit, unique(map.scen()$scen))
    })
    output$plotage <- renderPlotly({
      plot_age(scenage, unit, unique(map.scen()$scen))
    })
    
    ##CARBON##
    output$plotbiomasse <- renderPlotly({
      plot_biomasse(scencarbone, unit, unique(map.scen()$scen))
    })
    output$plotreservoir <- renderPlotly({
      plot_reservoir(scencarbone, unit, unique(map.scen()$scen))
    })
    output$plottransfert <- renderPlotly({
      plot_transfert(scencarbone, unit, unique(map.scen()$scen))
    })
    output$plotNPP <- renderPlotly({
      plot_NPP(scencarbone, unit, unique(map.scen()$scen))
    })
  })
    
}


shinyApp(ui=ui, server=server)


