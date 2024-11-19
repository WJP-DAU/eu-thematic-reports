gen_catMap <- function(dta, break_legend = F, static = F) {
  
  # Define the colors for categories
  unique_categories <- c(sort(setdiff(unique(dta$value2plot), "Other")), "Other")
  category_colors <- cat_map_palette[1:length(unique_categories)] %>%
    setNames(unique_categories)
  
  # Defining unique colors for map regions
  inner_regions <- dta %>%
    ungroup() %>%
    distinct(nuts_id) %>%
    pull(nuts_id)
  outer_regions <- base_map %>%
    filter(!polID %in% inner_regions) %>%
    pull(polID)
  border_color        <- c(region_names$unique_border,
                           rep("#ABA1A7", length((outer_regions))))
  names(border_color) <- c(region_names$nuts_id,
                           outer_regions)
  label_color         <- c(region_names$unique_label,
                           rep("#212429", length((outer_regions))))
  names(label_color)  <- c(region_names$nuts_id,
                           outer_regions)
  
  # Merge data with base map
  data4map <- base_map %>%
    left_join(dta, 
              by = c("polID" = "nuts_id")) %>%
    left_join(region_names %>% select(nuts_id, nameSHORT),
              by = c("polID" = "nuts_id"))
  
  inset_dimensions <- list(
    "Canaries/Madeira" = list(
      "x" = c(1491672, 2091880.2),
      "y" = c(941748.3, 1541956.5)
    ),
    "Azores"  = list(
      "x" = c(864542.6, 1407344),
      "y" = c(2250319.3, 2793120.7)
    ),
    "Cyprus" = list(
      "x" = c(6332001, 6525814),
      "y" = c(1585147, 1767640)
    )
  )
  
  # Convert insets to grob objects using ggplotGrob
  inset_grobs <- imap(insets, function(inset, inset_name) {
    
    region_data <- data4map %>%
      filter(polID %in% inset$polID)
    
    centroids <- region_data %>%
      filter(polID %in% inset$polID) %>%
      st_centroid() %>%
      mutate(
        lon = st_coordinates(.)[,1],
        lat = st_coordinates(.)[,2],
        lat = if_else(polID == "MT00", lat + 5000, lat),
        nameSHORT = str_trim(nameSHORT),
        nameSHORT = case_when(
          nameSHORT == "Auvergne-Rhône-Alpes"   ~ "Auvergne-Rhône- Alpes",
          nameSHORT == "Burgundy-Franche-Comté" ~ "Burgundy- Franche-Comté",
          nameSHORT == "Mecklenburg-Vorpommern" ~ "Mecklenburg- Vorpommern",
          nameSHORT == "South -West/-Central"   ~ "South -West/ -Central",
          TRUE ~ nameSHORT
        ),
        tooltip = paste0(
          "**",nameSHORT,"**<br>",
          "_",str_trim(country_name_ltn),"_<br>",
          value2plot
        ),
        hjust = 0,
        vjust = 1
      ) 
    
    # ggplot object for insets
    gg_inset <- ggplot() +
      geom_sf(
        data = region_data,
        aes(
          fill    = value2plot,
          colour  = polID
        ),
        size = 0.5,
        show.legend = c(fill = TRUE)
      ) +
      scale_colour_manual(values     = border_color,
                          guide      = "none") +
      scale_fill_manual(values       = category_colors, 
                        na.value     = "#d8d8d8") +
      new_scale_colour() 
    if (static == FALSE){
      gg_inset <- gg_inset + geom_textbox(
        data = centroids,
        aes(
          y      = lat,
          x      = lon,
          label  = tooltip,
          colour = polID,
          hjust  = hjust,
          vjust  = vjust
        ),
        family   = "Lato Full",
        fontface = "plain",
        width    = unit(1.25, "inch"),
        size     = 3,
        fill     = "white"
      )+ scale_colour_manual(
        values     = label_color,
        guide      = "none")} 
    
    gg_inset <- gg_inset +
      scale_y_continuous(limits  = inset_dimensions[[inset_name]][["y"]]) + 
      scale_x_continuous(limits  = inset_dimensions[[inset_name]][["x"]]) +
      coord_sf(clip = "off") +
      theme_minimal() +
      theme(
        axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        panel.background = element_blank(),
        legend.title     = element_blank(),
        legend.position  = "none",
        panel.border     = element_rect(color = "#e6e7e8", 
                                        fill  = NA, 
                                        linewidth = 0.5)
      ) +
      labs(fill = "Category") +
      ggtitle(inset_name) +
      theme(plot.title = element_text(size = 8, hjust = 0.5))
    
    # Convert to grob
    ggplotGrob(gg_inset)
  })
  
  country_level <- data4map %>%
    group_by(CNTR_CODE) %>%
    summarise()
  
  # Main map
  centroids <- data4map %>%
    filter(polID %in% inner_regions) %>%
    st_centroid() %>%
    mutate(
      lon = st_coordinates(.)[,1],
      lat = st_coordinates(.)[,2],
      lat = if_else(polID == "MT00", lat + 5000, lat),
      nameSHORT = str_trim(nameSHORT),
      nameSHORT = case_when(
        nameSHORT == "Auvergne-Rhône-Alpes"   ~ "Auvergne-Rhône- Alpes",
        nameSHORT == "Burgundy-Franche-Comté" ~ "Burgundy- Franche-Comté",
        nameSHORT == "Mecklenburg-Vorpommern" ~ "Mecklenburg- Vorpommern",
        nameSHORT == "South -West/-Central"   ~ "South -West/ -Central",
        TRUE ~ nameSHORT
      ),
      tooltip = paste0(
        "**",nameSHORT,"**<br>",
        "_",str_trim(country_name_ltn),"_<br>",
        value2plot
      ),
      hjust = case_when(
        lon <  4299365 ~ 0,
        lon >= 4299365 ~ 1,
      ),
      vjust = case_when(
        lat <  3383059 ~ 0,
        lat >= 3383059 ~ 1
      )
    ) 
  
  p <- ggplot() +
    geom_sf(
      data = data4map,
      aes(
        fill   = value2plot,
        colour = polID
      ),
      size = 0.5,
      show.legend = c(fill = TRUE)
    ) +
    geom_sf(data  = country_level,
            fill  = NA,
            color = "grey25") +
    scale_colour_manual(values     = border_color,
                        guide      = "none") +
    scale_fill_manual("",
                      values       = category_colors, 
                      na.value     = "#d8d8d8",
                      drop         = F,
                      na.translate = F) +
    new_scale_colour() 
  if (static == FALSE){
    p <- p + geom_textbox(
      data = centroids,
      aes(
        y      = lat,
        x      = lon,
        label  = tooltip,
        colour = polID,
        hjust  = hjust,
        vjust  = vjust
      ),
      family   = "Lato Full",
      fontface = "plain",
      width    = unit(1.25, "inch"),
      size     = 3,
      fill     = "white"
    ) +
      # coord_sf(crs = st_crs(base_map)) +
      scale_colour_manual(values = label_color,
                          guide  = "none")} 
  p <- p + scale_y_continuous(limits  = c(1442631, 5323487)) +
    scale_x_continuous(limits  = c(2581570, 6017160)) +
    theme_minimal() +
    theme(
      axis.title.x     = element_blank(),
      axis.title.y     = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      panel.grid       = element_blank(),
      panel.border     = element_rect(color     = "#e6e7e8", 
                                      fill      = NA, 
                                      linewidth = 0.55),
      panel.background   = element_blank(),
      plot.margin        = margin(0, 0, 0, 0),
      legend.text        = element_text(family = "Lato Full",
                                        face   = "bold", 
                                        size   = 3 * .pt,
                                        color  = "#222221",
                                        hjust  = 0.5,
                                        margin = margin(0,5,0,12)),
      legend.position      = "top",
      legend.byrow         = TRUE, 
      legend.key.size      = unit(0.15, "inches"), 
      # legend.key.spacing   = unit(0.25, "inches"),
      legend.justification = "left",
      legend.margin        = margin(2,0,0,0),
      
    ) + 
    guides(
      fill = guide_legend(
        nrow  = ifelse(break_legend == T, 2, 1),
        byrow = TRUE,
        override.aes = list(
          colour = NA
        )
      )
    )
  
  # Convert the main plot to a grob
  # p_grob <- ggplotGrob(p)
  
  
  # Inserting inset map boxes
  main_map <- p
  inset_grobs <- inset_grobs
  
  main_map_with_insets <- main_map +
    annotation_custom(grob = inset_grobs[[1]], ymin = 4.5e6, ymax = 5.5e6, xmin = 2.5e6, xmax = 3e6) +
    annotation_custom(grob = inset_grobs[[2]], ymin = 4.5e6, ymax = 5.5e6, xmin = 3e6,   xmax = 3.5e6) + 
    annotation_custom(grob = inset_grobs[[3]], ymin = 4.5e6, ymax = 5.5e6, xmin = 3.5e6, xmax = 4e6)
  
  patch <- plot_grid(
    main_map_with_insets
  )
  
  return(patch)
}
