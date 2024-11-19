## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Thematic Report - Main Insights
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    
##
## Dependencies:      World Justice Project
##
## Creation date:     November 18th, 2024
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

outline_GPP <- outline %>%
  select(thematic_reports, target_var_1, direction, description) %>%
  filter(thematic_reports == T) %>%
  filter(description %in% "GPP") %>%
  select(target_var = target_var_1, direction)
  
outline_QRQ <- outline %>%
  select(thematic_reports, target_var_1, direction, description) %>%
  filter(thematic_reports == T) %>%
  filter(description %in% "QRQ") %>%
  select(target_var = target_var_1, direction)

# Insight 1: Development and Rule of Law ----------------------------------

"%!in%" <- compose("!", "%in%")
data2plot <- GPP_QRQ_TPS_database %>%
  select(country, nuts_id, starts_with("GPP_"), development) %>%
  pivot_longer(cols = !c(country, nuts_id, development), names_to = "target_var", values_to = "value2plot") %>%
  mutate(target_var = 
           sub("GPP_", "", 
               target_var)) %>%
  left_join(outline_GPP, by = "target_var", relationship = "many-to-many") %>%
  drop_na(direction) %>%
  #filter(direction == "positive") %>%
  mutate(
    direction = 
      if_else(direction %in% "mixed", "positive", direction)
  ) %>%
  mutate(value2plot =
           if_else(
             direction %in% "negative",
             1-value2plot,
             value2plot
           )
  ) %>%
  filter(
    target_var %!in% c("prevalence2", "vulnerability1", "vulnerability2", 
                       "access2info","access2rep", "access2drm", "rp_time", 
                       "rp_cost", "rp_fair", "rp_outcome")
  ) %>%
  group_by(country, target_var, development) %>%
  summarise(
    value2plot = mean(value2plot, na.rm = T)
  ) %>%
  mutate(
    development = 
      case_when(
        development == 1 ~ "Less Developed Regions",
        development == 2 ~ "More Developed and Transition Regions",
        development == 3 ~ "More Developed and Transition Regions"
      )
  ) %>%
  drop_na(development) %>%
  group_by(development, target_var) %>%
  summarise(
    value2plot = mean(value2plot, na.rm = T)
  )


# Supongamos que tu data frame original se llama df en R y tiene las mismas columnas que la base que cargaste

# Filtra las filas para cada tipo de región
less_developed <- data2plot %>% filter(development == "Less Developed Regions")
more_developed <- data2plot %>% filter(development == "More Developed and Transition Regions")

# Une las dos bases de datos por la variable 'target_var'
merged_data <- merge(less_developed, more_developed, by = "target_var", suffixes = c("_less", "_more")) %>%
  mutate(
    GAP = value2plot_more - value2plot_less
  )

# Create the scatter plot with italicized "EUROVOICES"
plot <- ggplot(merged_data, aes(y = value2plot_more, x = value2plot_less)) +
  geom_point(aes(
    y = value2plot_more, 
    x = value2plot_less),
    size   = 2,
    stroke = .025,
    show.legend = c(fill = TRUE)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#ef4b4b") +
  labs(
    x = "Less Developed Regions",
    y = "More Developed and Transition Regions",
    title = "Public perceptions on the rule of law across economic development levels*",
    subtitle = "Each data point represents a proportion based on aggregated responses to perception-based questions from the *EUROVOICES* household survey.",
    caption = "*The classification of economic development is based on information sourced from DG REGIO.\n\nNote: the red line represents a 45-degree line, indicating points where the proportions among the regions are equal. Points above the line show cases \nwhere the proportion in the more developed and transition regions is higher than in the less developed regions, while points below the line indicate the \nopposite, with less developed regions having a higher proportion."
  ) +
  scale_y_continuous(breaks   = seq(0.2, 1, 0.20),
                     labels   = paste0(seq(20, 100, 20), "%"),
                     position = "left",  
                     expand   = c(0,0),
                     limits = c(0, 1.05)) +
  scale_x_continuous(breaks   = seq(0.2, 1, 0.20),
                     labels   = paste0(seq(20, 100, 20), "%"),
                     position = "bottom", 
                     expand   = c(0,0),
                     limits = c(0, 1.05)) +
  theme_minimal() +
  WJP_theme() +
  theme(
    axis.line            = element_line(color = "#5e5c5a", linetype = "solid"),
    plot.title           = element_text(family = "Lato Full",
                                        face = "bold",
                                        size = 4.920437 * .pt,
                                        color = "black",
                                        margin = margin(10, 0, 0, 0)),
    # Use element_markdown for the subtitle to enable italics
    plot.subtitle        = element_markdown(family = "Lato Full",
                                            face = "plain",
                                            size = 3.514598 * .pt,
                                            color = "#524F4C",
                                            margin = margin(10, 0, 10, 0)),
    plot.caption         = element_text(family = "Lato Full",
                                        face = "plain",
                                        size = 3.514598 * .pt,
                                        color = "#524F4C",
                                        hjust = 0,
                                        margin = margin(20, 0, 0, 0))
  );plot



ggsave(plot = plot, filename = "Economic_development_GPP.svg", width = 10, height = 7)

# Insight 2: Trust ----------------------------------

data2plot <- master_data_gpp %>%
  mutate(
    TRT_govt_national = 
      case_when(
        TRT_govt_national <= 2  ~ 1,
        TRT_govt_national <= 4  ~ 0,
        TRT_govt_national == 98 ~ 0
      ), 
    TRT_govt_local =
      case_when(
        TRT_govt_local <= 2  ~ 1,
        TRT_govt_local <= 4  ~ 0,
        TRT_govt_local == 98 ~ 0
      )
  ) %>%
  group_by(country_name_ltn) %>%
  summarise(
    TRT_govt_local = 
      mean(TRT_govt_local, na.rm = T),
    TRT_govt_national =
      mean(TRT_govt_national, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(
    GAP = TRT_govt_local - TRT_govt_national,
    GAP = abs(GAP)
  )

# Corrigiendo y optimizando el código
data2plot <- data2plot %>%
  bind_rows(
    data.frame(
      country_name_ltn = "EU Average",
      TRT_govt_local = data2plot %>% summarise(mean(TRT_govt_local, na.rm = TRUE)) %>% pull(),
      TRT_govt_national = data2plot %>% summarise(mean(TRT_govt_national, na.rm = TRUE)) %>% pull(),
      GAP = TRT_govt_local - TRT_govt_national
    )
  )

# Reorder country_name_ltn to place "EU Average" first, followed by the rest alphabetically
data2plot <- data2plot %>%
  mutate(country_name_ltn = factor(
    country_name_ltn, 
    levels = c(sort(setdiff(country_name_ltn, "EU Average"), decreasing = TRUE), "EU Average")
  ))

# Create the dumbbell chart with bold "EU Average"
plot <-
  ggplot(data2plot, aes(y = country_name_ltn)) +
  geom_segment(aes(x = TRT_govt_local, xend = TRT_govt_national, 
                   y = country_name_ltn, yend = country_name_ltn),
               color = "#aeb6bf", alpha = 0.5, size = 3.5) +
  
  # Points for local and national government
  geom_point(aes(x = TRT_govt_local, color = "Local"), size = 3, show.legend = TRUE) +
  geom_point(aes(x = TRT_govt_national, color = "National"), size = 3, show.legend = TRUE) +
  
  # Adding values with switched hjust for Denmark
  geom_text(aes(
    x = TRT_govt_local, 
    label = scales::percent(TRT_govt_local, accuracy = 1),
    hjust = ifelse(country_name_ltn == "Denmark", 1.5, -0.5)), 
    vjust = 0.5, color = "#009AA9", size = 3) + # Adjusted hjust for Denmark
  
  geom_text(aes(
    x = TRT_govt_national, 
    label = scales::percent(TRT_govt_national, accuracy = 1),
    hjust = ifelse(country_name_ltn == "Denmark", -0.5, 1.5)), 
    vjust = 0.5, color = "#ef4b4b", size = 3) +  # Adjusted hjust for Denmark
  
  # Define colors for the legend
  scale_color_manual(
    name = "Trust in Government:", # Legend title
    values = c("National" = "#ef4b4b", "Local" = "#009AA9")
  ) +
  
  labs(
    title = "Public trust in local versus national government officials",
    subtitle = "Percentage of respondents who express a lot or some trust in local and national government officials."
  ) +
  
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1), position = "top") +
  
  # Convert y-axis labels to a formatted version
  scale_y_discrete(labels = function(x) ifelse(x == "EU Average", 
                                               paste0("<b>", x, "</b>"), x)) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Lato Full",
                              face = "bold",
                              size = 4.920437 * .pt,
                              color = "black",
                              margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(family = "Lato Full",
                                 face = "plain",
                                 size = 3.514598 * .pt,
                                 color = "#524F4C",
                                 margin = margin(10, 0, 10, 0)),
    legend.title = element_text(family = "Lato Full", 
                                face = "bold",
                                size = 2.914598 * .pt,
                                hjust = 0.5,
                                color = "black"),
    axis.line.x = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(size = 0.25,
                                      colour = "#a0a0a0",
                                      linetype = "dashed"),
    legend.position = "top",
    legend.text = element_text(family = "Lato Full",
                               face = "plain", 
                               size = 2.914598 * .pt,
                               color = "#222221",
                               hjust = 0.5),
    legend.key.size = unit(0.15, "inches"), 
    legend.justification = "center",
    legend.margin = margin(2, 0, 0, 0),
    
    # Use element_markdown to render the bold text
    axis.text.y = element_markdown(family = "Lato Full", size = 2.914598 * .pt, hjust = 0)
  )

plot




ggsave(plot = plot, filename = "Trust.svg", width = 7, height = 7)


# Insight 3: Variance ----------------------------------

data2plot <- variance_analysis %>%
  select(target_variable, between_countries_var, avg_between_region_variance)

# Create the scatter plot with italicized "EUROVOICES"
plot <- ggplot(variance_analysis, aes(y = between_countries_var, x = avg_between_region_variance)) +
  geom_point(aes(
    y = between_countries_var, 
    x = avg_between_region_variance),
    size   = 2,
    stroke = .025,
    show.legend = c(fill = TRUE)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#ef4b4b") +
  labs(
    x = "Intra-Country Variance**",
    y = "Inter-Country Variance*",
    title = "Variance in public perceptions of the rule of law within and across EU countries",
    subtitle = "Each data point represents an average based on variances in aggregated responses to perception-based questions from the *EUROVOICES* household survey.",
    caption = "*An inter-country average represents the variance of individual countries' values from the overall EU average, highlighting variations between EU countries.\n**An intra-country average represents the variance of individual regions' values from the overall national average, highlighting variations within a single country.\n\nNote: the red line represents a 45-degree line, indicating points where the variance across countries is equal to the variance within countries. Points above the \nline show cases where the variance between countries is greater than the variance within countries, while points below the line indicate the opposite, with \nregions within a single country having greater variance."
  ) +
  scale_y_continuous(
    breaks = seq(0.01, 0.05, 0.010),
    labels = paste0(seq(0.01, 0.05, 0.010)),
    position = "left", 
    expand = c(0, 0),
    limits = c(0, 0.055)  # Set limits for full visualization
  ) +
  scale_x_continuous(
    breaks = seq(0.01, 0.05, 0.010),
    labels = paste0(seq(0.01, 0.05, 0.010)),
    position = "bottom", 
    expand = c(0, 0),
    limits = c(0, 0.055)  # Set limits for full visualization
  ) +
  theme_minimal() +
  WJP_theme() +
  theme(
    axis.line            = element_line(color = "#5e5c5a", linetype = "solid"),
    plot.title           = element_text(family = "Lato Full",
                                        face = "bold",
                                        size = 4.920437 * .pt,
                                        color = "black",
                                        margin = margin(10, 0, 0, 0)),
    # Use element_markdown for the subtitle to enable italics
    plot.subtitle        = element_markdown(family = "Lato Full",
                                            face = "plain",
                                            size = 3.514598 * .pt,
                                            color = "#524F4C",
                                            margin = margin(10, 0, 10, 0)),
    plot.caption         = element_text(family = "Lato Full",
                                        face = "plain",
                                        size = 3.514598 * .pt,
                                        color = "#524F4C",
                                        hjust = 0,
                                        margin = margin(20, 0, 0, 0))
  ); plot

ggsave(plot = plot, filename = "Variance.svg", width = 10.5, height = 7)

# Insight 4: Rural vs Urban ----------------------------------

data2plot <- data4web_gpp %>%
  filter(demographic %in% c("Rural", "Urban")) %>%
  drop_na(value) %>%
  filter(
    id %!in% c("prevalence2", "vulnerability1", "vulnerability2", 
                       "access2info","access2rep", "access2drm", "rp_time", 
                       "rp_cost", "rp_fair", "rp_outcome")
  ) %>%
  mutate(value =
           if_else(
             direction %in% "negative",
             1-value,
             value
           )
  ) %>%
  group_by(demographic, id) %>%
  summarise(
    value = mean(value, na.rm = T)
  )

# Supongamos que tu data frame original se llama df en R y tiene las mismas columnas que la base que cargaste

# Filtra las filas para cada tipo de región
Urban <- data2plot %>% filter(demographic == "Urban")
Rural <- data2plot %>% filter(demographic == "Rural")

# Une las dos bases de datos por la variable 'target_var'
merged_data <- merge(Rural, Urban, by = "id", suffixes = c("_rural", "_urban")) %>%
  mutate(
    GAP = value_urban - value_rural
  )

# Create the scatter plot with italicized "EUROVOICES"
plot <- ggplot(merged_data, aes(y = value_urban, x = value_rural)) +
  geom_point(aes(
    y = value_urban, 
    x = value_rural),
    size   = 2,
    stroke = .025,
    show.legend = c(fill = TRUE)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#ef4b4b") +
  labs(
    x = "Rural Regions",
    y = "Urban Regions",
    title = "Public perceptions of the rule of law in urban versus rural regions",
    subtitle = "Each data point represents a proportion based on aggregated responses to perception-based questions from the *EUROVOICES* household survey.",
    caption = "Note: the red line represents a 45-degree line, indicating points where the proportions among the regions are equal. Points above the line show cases \nwhere the proportion in urban regions is higher than in rural regions, while points below the line indicate the opposite, with rural regions having a higher \nproportion."
  ) +
  scale_y_continuous(breaks   = seq(0.2, 1, 0.20),
                     labels   = paste0(seq(20, 100, 20), "%"),
                     position = "left", 
                     expand   = c(0,0),
                     limits = c(0, 1.05)) +
  scale_x_continuous(breaks   = seq(0.2, 1, 0.20),
                     labels   = paste0(seq(20, 100, 20), "%"),
                     position = "bottom", 
                     expand   = c(0,0),
                     limits = c(0, 1.05)) +
  theme_minimal() +
  WJP_theme() +
  theme(
    axis.line            = element_line(color = "#5e5c5a", linetype = "solid"),
    plot.title           = element_text(family = "Lato Full",
                                        face = "bold",
                                        size = 4.920437 * .pt,
                                        color = "black",
                                        margin = margin(10, 0, 0, 0)),
    # Use element_markdown for the subtitle to enable italics
    plot.subtitle        = element_markdown(family = "Lato Full",
                                            face = "plain",
                                            size = 3.514598 * .pt,
                                            color = "#524F4C",
                                            margin = margin(10, 0, 10, 0)),
    plot.caption         = element_text(family = "Lato Full",
                                        face = "plain",
                                        size = 3.514598 * .pt,
                                        color = "#524F4C",
                                        hjust = 0,
                                        margin = margin(20, 0, 0, 0))
  ); plot


ggsave(plot = plot, filename = "Regions.svg", width = 10, height = 7)

# Insight 5: Male vs Female ----------------------------------

data2plot <- data4web_gpp %>%
  filter(demographic %in% c("Female", "Male")) %>%
  drop_na(value) %>%
  filter(
    id %!in% c("prevalence2", "vulnerability1", "vulnerability2", 
               "access2info","access2rep", "access2drm", "rp_time", 
               "rp_cost", "rp_fair", "rp_outcome")
  ) %>%
  filter(
    section %in% c(
      "Fundamental Rights",
      "Safety",
      "Control of Corruption",
      "Transparency"
    )
  ) %>%
  mutate(value =
           if_else(
             direction %in% "negative",
             1-value,
             value
           )
  ) %>%
  group_by(demographic, id) %>%
  summarise(
    topic = first(section),
    value = mean(value, na.rm = T)
  )

# Supongamos que tu data frame original se llama df en R y tiene las mismas columnas que la base que cargaste

# Filtra las filas para cada tipo de región
Male <- data2plot %>% filter(demographic == "Male")
Female <- data2plot %>% filter(demographic == "Female")

# Une las dos bases de datos por la variable 'target_var'
merged_data <- merge(Female, Male, by = "id", suffixes = c("_female", "_male")) %>%
  mutate(
    GAP = value_male - value_female,
    main_topic = 
      case_when(
        topic_male %in% c( "Control of Corruption",
                           "Transparency") ~ "Control of Corruption and Transparency",
        topic_male %in% c( "Safety",
                           "Fundamental Rights") ~ "Safety and Fundamental Rights",
        
      )
  )

# Crea el scatter plot
plot <- ggplot(merged_data, aes(y = value_male, x = value_female)) +
  geom_point(aes(
    y = value_male, 
    x = value_female,
    color = main_topic),
    #shape  = 21,
    size   = 2,
    stroke = .025,
    show.legend = c(fill = TRUE)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#ef4b4b") +
  labs(
    x = "Female",
    y = "Male",
    title = "Public perceptions of the rule of law by gender",
    subtitle = "Each data point represents a proportion based on aggregated responses to perception-based questions from the EUROVOICES household survey.",
    caption = "Note: the red line represents a 45-degree line, indicating points where the proportions of females and males are equal. Points above the line show cases \nwhere the proportion of males is higher than that of females, while points below the line indicate the opposite, with females having a higher proportion.",
    color = "Topic:"
    ) +
  scale_color_manual(
    values = c("Control of Corruption and Transparency" = "#009AA9",
               "Safety and Fundamental Rights" = "#ef4b4b")
  ) +
  scale_y_continuous(breaks   = seq(0.2, 1, 0.20),
                     labels   = paste0(seq(20, 100, 20), "%"),
                     position = "left", 
                     expand   = c(0,0),
                     limits = c(0, 1.05)) +
  scale_x_continuous(breaks   = seq(0.2, 1, 0.20),
                     labels   = paste0(seq(20, 100, 20), "%"),
                     position = "bottom", 
                     expand   = c(0,0),
                     limits = c(0, 1.05)) +
  theme_minimal() +
  WJP_theme() +
  theme(
    axis.line            = 
      element_line(color    = "#5e5c5a",
                   linetype = "solid"
      ),
    plot.title          = element_text(family   = "Lato Full",
                                       face     = "bold",
                                       size     = 4.920437*.pt,
                                       color    = "black",
                                       margin   = margin(10, 0, 0, 0)
    ),
    plot.subtitle       = element_text(family   = "Lato Full",
                                       face     = "plain",
                                       size     = 3.514598*.pt,
                                       color    = "#524F4C",
                                       margin   = margin(10, 0, 10, 0)
    ),
    plot.caption        = element_text(family   = "Lato Full",
                                       face     = "plain",
                                       size     = 3.514598*.pt,
                                       color    = "#524F4C",
                                       hjust = 0,
                                       margin   = margin(20, 0, 0, 0)),
    ,
    legend.position = "top",
    legend.title = element_text(family = "Lato Full",
                                face = "bold", 
                                size = 2.914598 * .pt,
                                color = "#222221",
                                hjust = 0.5),
    legend.text = element_text(family = "Lato Full",
                               face = "plain", 
                               size = 2.914598 * .pt,
                               color = "#222221",
                               hjust = 0.5),
    legend.key.size = unit(0.15, "inches"), 
    legend.justification = "center",
    legend.margin = margin(2, 0, 0, 0)
  );plot

ggsave(plot = plot, filename = "Gender.svg", width = 10, height = 7)





# Insight 6: Development and Rule of Law ----------------------------------

"%!in%" <- compose("!", "%in%")
data2plot <- GPP_QRQ_TPS_database %>%
  select(country, nuts_id, starts_with("QRQ_"), development) %>%
  pivot_longer(cols = !c(country, nuts_id, development), names_to = "target_var", values_to = "value2plot") %>%
  mutate(target_var = 
           sub("QRQ_", "", 
               target_var)) %>%
  left_join(outline_QRQ, by = "target_var", relationship = "many-to-many") %>%
  drop_na(direction) %>%
  #filter(direction == "positive") %>%
  mutate(
    direction = 
      if_else(direction %in% "mixed", "positive", direction)
  ) %>%
  mutate(value2plot =
           if_else(
             direction %in% "negative",
             1-value2plot,
             value2plot
           )
  ) %>%
  filter(
    target_var %!in% c("prevalence2", "vulnerability1", "vulnerability2", 
                       "access2info","access2rep", "access2drm", "rp_time", 
                       "rp_cost", "rp_fair", "rp_outcome")
  ) %>%
  group_by(country, target_var, development) %>%
  summarise(
    value2plot = mean(value2plot, na.rm = T)
  ) %>%
  mutate(
    development = 
      case_when(
        development == 1 ~ "Less Developed Regions",
        development == 2 ~ "More Developed and Transition Regions",
        development == 3 ~ "More Developed and Transition Regions"
      )
  ) %>%
  drop_na(development) %>%
  group_by(development, target_var) %>%
  summarise(
    value2plot = mean(value2plot, na.rm = T)
  )


# Supongamos que tu data frame original se llama df en R y tiene las mismas columnas que la base que cargaste

# Filtra las filas para cada tipo de región
less_developed <- data2plot %>% filter(development == "Less Developed Regions")
more_developed <- data2plot %>% filter(development == "More Developed and Transition Regions")

# Une las dos bases de datos por la variable 'target_var'
merged_data <- merge(less_developed, more_developed, by = "target_var", suffixes = c("_less", "_more")) %>%
  mutate(
    GAP = value2plot_more - value2plot_less
  )

# Create the scatter plot with italicized "EUROVOICES"
plot <- ggplot(merged_data, aes(y = value2plot_more, x = value2plot_less)) +
  geom_point(aes(
    y = value2plot_more, 
    x = value2plot_less),
    size   = 2,
    stroke = .025,
    show.legend = c(fill = TRUE)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#ef4b4b") +
  labs(
    x = "Less Developed Regions",
    y = "More Developed and Transition Regions",
    title = "Expert scores on the rule of law across economic development levels*",
    subtitle = "Each data point represents an aggregated indicator derived from expert opinion questions in the *EUROVOICES* legal experts survey.",
    caption = "*The classification of economic development is based on information sourced from DG REGIO.\n\nNote: the red line represents a 45-degree line, indicating points where the scores among the regions are equal. Points above the line show cases where the \nscores in the more developed and transition regions is higher than in the less developed regions, while points below the line indicate the opposite, with less \ndeveloped regions having a higher scores."
  ) +
  scale_y_continuous(breaks   = seq(0.2, 1, 0.20),
                     labels   = paste0(seq(0.2, 1, 0.20)),
                     position = "left", 
                     expand   = c(0,0),
                     limits = c(0, 1.05))+  # Set limits for full visualization) 
  scale_x_continuous(breaks   = seq(0.2, 1, 0.20),
                     labels   = paste0(seq(0.2, 1, 0.20)),
                     position = "bottom", 
                     expand   = c(0,0),
                     limits = c(0, 1.05)  # Set limits for full visualization
                     ) +
  theme_minimal() +
  WJP_theme() +
  theme(
    axis.line            = element_line(color = "#5e5c5a", linetype = "solid"),
    plot.title           = element_text(family = "Lato Full",
                                        face = "bold",
                                        size = 4.920437 * .pt,
                                        color = "black",
                                        margin = margin(10, 0, 0, 0)),
    # Use element_markdown for the subtitle to enable italics
    plot.subtitle        = element_markdown(family = "Lato Full",
                                            face = "plain",
                                            size = 3.514598 * .pt,
                                            color = "#524F4C",
                                            margin = margin(10, 0, 10, 0)),
    plot.caption         = element_text(family = "Lato Full",
                                        face = "plain",
                                        size = 3.514598 * .pt,
                                        color = "#524F4C",
                                        hjust = 0,
                                        margin = margin(20, 0, 0, 0))
  );plot

ggsave(plot = plot, filename = "Economic_development_QRQ.svg", width = 10, height = 7)
