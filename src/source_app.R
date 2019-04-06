
# -------------------------------------------------------------------------
# EduScope ----------------------------------------------------------------
# Code source -------------------------------------------------------------
# -------------------------------------------------------------------------
# Auteurs : 
# Romain Avouac
# Hugo Botton
# Alexandre Lim
# Pierre-Emmanuel Morant
# Emmanuel Mourtada
# -------------------------------------------------------------------------

library(tidyverse)
library(sf)


# Import des données ------------------------------------------------------

# Contours

iris <- st_read("data/shp/iris_acad_caen.shp", stringsAsFactors = F)
dep <- st_read("data/shp/dep_acad_caen.shp", stringsAsFactors = F)

# Données fiscales aux IRIS

filo <- read_delim("data/filo_iris_acad_caen_2014.csv", delim = "|")

# Données simulées sur les collèges de l'académie de Caen

data <- read_delim("data/data_simul_acad_caen.csv", delim = "|")


# Fonctions pour le calcul des statistiques sur les collèges --------------

ips_distrib <- function(data, echelle, point) {
  
  if (echelle != "etab") {
    df_ips <- data %>%
      filter(annee == 2017,
             !is.na(effectif_1er_cycle)) %>%
      group_by(!!sym(echelle)) %>%
      summarise(ips_total = weighted.mean(x = ips_total, 
                                          w = effectif_1er_cycle,
                                          na.rm = T)) %>%
      mutate(highlight = ifelse(!!sym(echelle) == point, 1, 0)) %>%
      na.omit()
    
  } else {
    
    acad_etab <- data$acad[data$nom_norme == point][1]
    
    df_ips <- data %>%
      filter(annee == 2017,
             acad == acad_etab) %>%
      select(etab, nom_norme, ips_total) %>%
      mutate(highlight = ifelse(nom_norme == point, 1, 0)) %>%
      distinct(nom_norme, .keep_all= TRUE)
    
    df_ips$highlight[df_ips$highlight == 1] <- c(1, rep(0, 
                                                        length(df_ips$highlight[df_ips$highlight == 1]) -1))
    
    ips_etab <- df_ips$ips_total[df_ips$highlight == 1]
  }
  
  min_ips <- min(data$ips_total, na.rm = T)
  max_ips <- max(data$ips_total, na.rm = T)
  
  elem_titre <- ifelse(echelle == "acad", "de l'académie",
                       ifelse(echelle == "dep", "du département",
                              ifelse(echelle == "bassin", "du bassin",
                                     ifelse(echelle == "etab", "de l'établissement", NA))))
  
  df_ips <- df_ips %>%
    mutate(nom_norme = reorder(nom_norme, ips_total))
  
  res_plot <- ggplot(data = df_ips, aes(x = nom_norme, 
                                        y = ips_total,
                                        fill = ips_total)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(min_ips, max_ips), oob = rescale_none) +
    scale_fill_gradient(low = "yellow", 
                        high = "red") +
    geom_vline(xintercept = which(levels(df_ips$nom_norme) == point), 
               color = "#3d6eb9", size = 1.5) +
    labs(y = "Indice de position sociale moyen") +
    theme(axis.text.x = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y = element_text(size = 14),
          legend.position = "none",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(color = "grey"))
  
  # axis.text.x = element_text(angle = 45, hjust = 1, 
  #                            size=11)
  
  return(list(ips_etab, res_plot))
}

gauge_moy_dnb <- function(data, echelle, point) {
  
  if (echelle != "etab") {
    df_ips <- data %>%
      filter(annee == 2017,
             !is.na(effectif_1er_cycle)) %>%
      group_by(!!sym(echelle)) %>%
      summarise(note_ecrit_dnb = weighted.mean(x = note_ecrit_dnb, 
                                               w = effectif_1er_cycle,
                                               na.rm = T)) %>%
      na.omit() %>%
      filter(!!sym(echelle) == point)
    
    note <- round(df_ips$note_ecrit_dnb, 1)
    
  } else {
    
    note <- round(data$note_ecrit_dnb[data$annee == 2017 & data$nom_norme == point], 1)[1]
    
  }
  
  elem_titre <- ifelse(echelle == "acad", "dans l'académie",
                       ifelse(echelle == "dep", "dans le département",
                              ifelse(echelle == "bassin", "dans le bassin",
                                     ifelse(echelle == "etab", "dans l'établissement", NA))))
  
  df_empty <- tibble("x" = 1)
  
  res_plot <- ggplot(data = df_empty, aes(ymax = note / 20, ymin = 0, xmax = 2, xmin = 1)) +
    geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
    geom_rect() + 
    coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
    geom_text(aes(x = 0, y = 0, label = paste0(note, " / 20"), colour="red"), size=6.5, family="Poppins SemiBold") +
    #geom_text(aes(x=0.5, y=1.5, label=paste("Note moyenne au DNB", sep = " ")), family="Poppins Light", size=4.2) + 
    theme_void() +
    scale_fill_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
    scale_colour_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank()) +
    guides(fill=FALSE) +
    guides(colour=FALSE)
  
  return(res_plot)
  
}

effectif_tot <- function(data, echelle, point) {
  
  if (echelle != "etab") {
    df_ips <- data %>%
      filter(annee == 2017,
             !is.na(effectif_1er_cycle)) %>%
      group_by(!!sym(echelle)) %>%
      summarise(effectif_1er_cycle = sum(effectif_1er_cycle)) %>%
      na.omit() %>%
      filter(!!sym(echelle) == point)
    
    eff_tot <- df_ips$effectif_1er_cycle
    
  } else {
    
    eff_tot <- data$effectif_1er_cycle[data$annee == 2017 & data$nom_norme == point][1]
    
  }
  
  return(eff_tot)
  
}

ep <- function(data, echelle, point) {
  
  elem_titre <- ifelse(echelle == "acad", "dans l'académie",
                       ifelse(echelle == "dep", "dans le département",
                              ifelse(echelle == "bassin", "dans le bassin",
                                     ifelse(echelle == "etab", "dans l'établissement", NA))))
  
  if (echelle != "etab") {
    
    data$hors_ep <- (data$ep == "Hors EP")
    data$rep <- (data$ep == "REP")
    data$rep_plus <- (data$ep == "REP+")
    
    df_ips <- data %>%
      filter(annee == 2017) %>%
      group_by(!!sym(echelle)) %>%
      summarise(hors_ep = mean(hors_ep),
                rep = mean(rep),
                rep_plus = mean(rep_plus)) %>%
      na.omit() %>%
      filter(!!sym(echelle) == point) %>%
      select(-!!sym(echelle)) %>%
      gather("ep", "part") %>%
      mutate(lab.ypos = cumsum(part) - 0.5*part) %>%
      filter(part != 0)
    
    ggplot(df_ips, 
           aes(x = "", 
               y = part, 
               fill = ep)) +
      geom_bar(width = 1, 
               stat = "identity", 
               color = "black") +
      coord_polar("y", 
                  start = 0, 
                  direction = -1) +
      theme_void() +
      geom_text(aes(y = lab.ypos, label = round(part*100, 1)), 
                color = "black") +
      labs(title = "Participants by race")
    
    plot_res <- ggplot(df_ips, aes(x="", y=part, fill=ep)) + 
      geom_bar(stat="identity", width=1) + 
      coord_polar("y", start=0) +
      geom_text(aes(label = paste0(round(part*100), "%")), 
                position = position_stack(vjust = 0.5)) + 
      scale_fill_discrete(labels = c("Hors EP", "REP", "REP+")) +
      labs(x = NULL, y = NULL, fill = NULL, 
           title = paste("Appartenance à l'éducation prioritaire",
                         elem_titre, sep = " ")) + 
      theme_classic() + 
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5,
                                      color = "#666666"))
    
    return(plot_res)
    
  } else {
    
    ep <- data$ep[data$nom_norme == point & data$annee == 2017][1]
    return(ep)
  }
}

plot_dynamique <- function(data, etab, variable) {
  
  data[,variable] <- as.numeric(data[,variable])
  
  acad_etab <- data$acad[data$nom_norme == etab][1]
  
  mean_nat <- data %>%
    group_by(annee) %>%
    summarise(avg = mean(!!sym(variable), na.rm = T)) %>%
    mutate(Echelon = "National")
  
  mean_acad <- data %>%
    filter(acad == acad_etab) %>%
    group_by(annee) %>%
    summarise(avg = mean(!!sym(variable), na.rm = T)) %>%
    mutate(Echelon = "Académie")
  
  mean_etab <- data[data$nom_norme == etab, ][1:4,c("annee", variable)] %>%
    rename(avg = variable) %>%
    mutate(Echelon = "Etablissement")
  
  df_mean_agg <- rbind(mean_etab, mean_acad, mean_nat)
  
  min <- min(unlist(data[,variable]), na.rm = T)
  max <- max(unlist(data[,variable]), na.rm = T)
  
  res_plot <- df_mean_agg %>%
    ggplot(aes(x = annee, y = avg, color = Echelon)) +
    geom_line(size = 1.5) +
    # scale_y_continuous(limits = c(min, max)) +
    scale_color_manual(values = c("red", "black", "blue")) +
    theme_bw() +
    labs(x = NULL, y = NULL)
  
  return(res_plot)
  
}











