# SCRIPT TO SELECT THE VARIABLES FROM THE DESCRIPTIVE ANALYSIS, CREATE THE 
# INDEXES FOR THE RANDOM EFFECTS AND MAKE THE  SENSITIVITY ANALYSIS FOR THE 
# ACADEMIC LEVEL AND 
library(tidyverse)
library(magrittr)

# Load the cleaned data
icfes_2022 <- readRDS("Data/icfes_2022_cleaned.Rds")

# Filter for the selected variables to modeling
icfes_2022_mod <- icfes_2022 %>% 
  select(
    # RESPONSE
    PUNT_MATEMATICAS,
    
    # COVARIATES
    COLE_JORNADA_IMPUTED, COLE_NATURALEZA, FAMI_ESTRATOVIVIENDA_IMPUTED,
    FAMI_TIENECOMPUTADOR, FAMI_EDUCACIONPADRE_IMPUTED,
    FAMI_EDUCACIONMADRE_IMPUTED, ACADEMIC_LEVEL,
    
    # CODES FOR THE SPATIAL AND SCHOOL RANDOM EFFECTS
    COLE_COD_MCPIO_UBICACION, # SPATIAL
    COLE_COD_DANE_ESTABLECIMIENTO, # SCHOOL
  )

################################################################################
# CONVERTING THE VARIABLES TO FACTORS
################################################################################
icfes_2022_mod %<>% 
  mutate(
    FAMI_EDUCACIONMADRE_IMPUTED = factor(
      FAMI_EDUCACIONMADRE_IMPUTED,
      levels = c("Primary incomplete", "Primary", "High school",
                 "Technical", "Graduate", "Postgraduate"),
    ),
    FAMI_EDUCACIONPADRE_IMPUTED = factor(
      FAMI_EDUCACIONPADRE_IMPUTED,
      levels = c("Primary incomplete", "Primary", "High school",
                 "Technical", "Graduate", "Postgraduate"),
    ),
    ACADEMIC_LEVEL = factor(
      ACADEMIC_LEVEL,
      levels = c("Primary incomplete", "Primary", "High school",
                 "Technical", "Graduate", "Postgraduate"),
    ),
    FAMI_ESTRATOVIVIENDA_IMPUTED = factor(
      FAMI_ESTRATOVIVIENDA_IMPUTED,
      levels = 1:6,
    ),
    COLE_JORNADA_IMPUTED = factor(
      COLE_JORNADA_IMPUTED,
      levels = c("Saturdays", "Nights", "Afternoons", "Mornings", "Complete")
    ),
    FAMI_TIENECOMPUTADOR = factor(
      FAMI_TIENECOMPUTADOR,
      levels = c("No", "Yes")
    ),
    COLE_NATURALEZA = factor(
      COLE_NATURALEZA,
      levels = c("Unofficial", "Official")
    )
  )

################################################################################
# CREATE THE INDEXES FOR THE SPATIAL AND SCHOOL RANDOM EFFECTS
################################################################################
mcpals <- readRDS("Data/col_mpio_geospatial.Rds")

# SPATIAL
# Creation of municipalities index in order to match with the one obtained by 
# poly2nb function (for the spatial fit)
icfes_2022_mod$idx_area <- 
  match(icfes_2022_mod$COLE_COD_MCPIO_UBICACION, 
        mcpals$codigo_municipio 
  )

# Sanity check 
# Min should be 1, Max should be the number of municipalities in the spatial 
# data
summary(icfes_2022_mod$idx_area) 

# SCHOOLS
icfes_2022_mod$idx_school <- 
  match(icfes_2022_mod$COLE_COD_DANE_ESTABLECIMIENTO, 
        unique(icfes_2022_mod$COLE_COD_DANE_ESTABLECIMIENTO) 
  )

# Min should be 1, Max should be the number of schools 
summary(icfes_2022_mod$idx_school) 

################################################################################
# CREATE THE NEIGHBORHOOD MATRIX
################################################################################

# Neighbors matrix
nb <- spdep::poly2nb(mcpals)

# Adjacency matrix (It is saved as a raw file)
# spdep::nb2INLA(file = "Data/graph_municipals", nb = nb)

# ADJACENCY GRAPH PLOT
# Centroids or representative points of each polygon
mcpals_aux <- st_set_crs(mcpals, 4326)
coords <- st_coordinates(st_point_on_surface(mcpals_aux))

# Convert neighbor object to line segments
adjacency_lines <- spdep::nb2lines(
  nb,
  coords = coords,
  as_sf = TRUE
)

# IMPORTANT: assign CRS to the adjacency lines
adjacency_lines <- st_as_sf(adjacency_lines)
st_crs(adjacency_lines) <- st_crs(mcpals_aux)

# Plot map + adjacencies
adjacency_map <- ggplot() +
  geom_sf(data = mcpals_aux, fill = "gray95", color = "gray60", linewidth = 0.2) +
  geom_sf(data = adjacency_lines, color = "red", linewidth = 0.3, alpha = 0.7) +
  labs(
    title = " ",
  ) +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = .5)
  ) 

adjacency_map

# ggsave(
#   filename = "article_plots/adjacency_map.png",
#   plot = adjacency_map,
#   width = 8,
#   height = 10,
#   units = "in"
# )


################################################################################
# SENSITIVITY ANALYSIS
################################################################################
library(INLA)
# AUX DATAFRAME WITH BOTH ACADEMIC LEVELS
icfes_2022_mod_no_na <- icfes_2022_mod %>% 
  drop_na()
# The number of observations is reduced from 758333 to 656474

# Auxiliary formula
aux_formula <- PUNT_MATEMATICAS ~ COLE_JORNADA_IMPUTED + FAMI_ESTRATOVIVIENDA_IMPUTED + 
  FAMI_TIENECOMPUTADOR + COLE_NATURALEZA + 
  FAMI_ESTRATOVIVIENDA_IMPUTED * FAMI_TIENECOMPUTADOR + 
  FAMI_ESTRATOVIVIENDA_IMPUTED * COLE_NATURALEZA


# MODEL WITH ONLY THE ACADEMIC LEVEL OF THE MOTHER
mom_formula <- update(aux_formula, . ~ . + FAMI_EDUCACIONMADRE_IMPUTED)

# Fit the model
mod_mom <- inla(
  formula = mom_formula,
  data = icfes_2022_mod_no_na,
  verbose = FALSE,
  control.compute = list(dic = T, waic = T, cpo = T)
)

# SUMMARY FOR THE MODEL WITH ONLY THE ACADEMIC LEVEL OF THE MOTHER
mod_mom$summary.fixed


# MODEL WITH ONLY THE ACADEMIC LEVEL OF THE FATHER
dad_formula <- update(aux_formula, . ~ . + FAMI_EDUCACIONPADRE_IMPUTED)

# Fit the model
mod_dad <- inla(
  formula = dad_formula,
  data = icfes_2022_mod_no_na,
  verbose = FALSE,
  control.compute = list(dic = T, waic = T, cpo = T)
)

# SUMMARY FOR THE MODEL WITH ONLY THE ACADEMIC LEVEL OF THE FATHER
mod_dad$summary.fixed


# COMPARE THE VALUES FOR THE FIXED EFFECTS
abs(mod_mom$summary.fixed - mod_dad$summary.fixed)

# The difference in the values is small, which means that the posterior 
# approximations are similar.

# MODEL WITH THE HIGHEST ACADEMIC LEVEL
max_academic_formula <- update(aux_formula, . ~ . + ACADEMIC_LEVEL)

# Fit the model
mod_max_academic <- inla(
  formula = max_academic_formula,
  data = icfes_2022_mod_no_na,
  verbose = FALSE,
  control.compute = list(dic = T, waic = T, cpo = T)
)

# SUMMARY OF THE MODEL WITH BOTH ACADEMIC LEVELS
mod_max_academic$summary.fixed

# SINCE THE INDIVIDUAL MODELS GIVE SIMILAR RESULTS, WE COMPARE ONLY WITH 
# ONE OF THEM BECAUSE THE DIFFERENCE WITH THE OTHER WOULD BE SMALL AS WELL
abs(mod_mom$summary.fixed - mod_max_academic$summary.fixed)

# Again, the difference among the posterior approximations are similar

# ACTIONS 
# 1. FROM NOW ON, WE USE THE MAX ACADEMIC LEVEL OF EITHER PARENT
# 2. We drop the academic levels of the mother and the father and we preserve
# only the column ACADEMIC_LEVEL (the maximum) in order to drop only 2% of
# the data from the object "icfes_2022_mod".
icfes_2022_mod %<>% 
  select(-c(FAMI_EDUCACIONMADRE_IMPUTED, FAMI_EDUCACIONPADRE_IMPUTED)) %>% 
  drop_na()

# With this, we move from 75833 observations to 739211. Droping NAs this way
# helps to loss less data. Also, if the model is fitted in this way, the
# fixed effects posterior approximations remain, in practice, in the same
# values

# SANITY CHECK
# Fit the model
linear_fit <- inla(
  formula = max_academic_formula,
  data = icfes_2022_mod,
  verbose = FALSE,
  control.compute = list(dic = T, waic = T, cpo = T)
)

# COMPARE SUMMARIES WITH THE PREVIOUS MODEL WITH LESS OBSERVATIONS
abs(linear_fit$summary.fixed - mod_max_academic$summary.fixed)

# CONCLUSION: "SAME" MODELS


# REPLICATION OF THE PLOT OF ACADEMIC LEVELS BUILT IN THE DESCRIPTIVE ANALYSIS
# Descriptive/EDA_ICFES_2022.R (To put in the paper)
academic_order <- c("Primary incomplete", "Primary", "High school",
                    "Technical", "Graduate", "Postgraduate")

academic_lvl <- ggplot(
  icfes_2022_mod %>%
    mutate(
      ACADEMIC_LEVEL =
        factor(ACADEMIC_LEVEL, levels = academic_order)
    ),
  aes(x = ACADEMIC_LEVEL, y = PUNT_MATEMATICAS)
) +
  geom_boxplot(aes(fill = ACADEMIC_LEVEL), notch = T) +
  labs(x = "", y = "Math score", fill = "Academic level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5), axis.text.x = element_blank())

academic_freq <- icfes_2022_mod %>% 
  select(ACADEMIC_LEVEL) %>% 
  group_by(ACADEMIC_LEVEL) %>% 
  summarise(Frequency = n()) %>% 
  ungroup() %>% 
  mutate(Proportion = Frequency/sum(Frequency)) %>% 
  ggplot(aes(ACADEMIC_LEVEL, Frequency)) +
  geom_col(aes(fill = ACADEMIC_LEVEL), col = "black") +
  geom_text(aes(label = scales::percent(Proportion)), vjust = -0.35) +
  labs(y = "Frequency", x = "", fill = "Academic level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())

# GRID WITH THE PLOTS
academic_lvl_plots <- ggpubr::ggarrange(academic_freq,
                                        academic_lvl,
                                        common.legend = T,
                                        legend = "bottom") %>%
  ggpubr::annotate_figure(ggpubr::text_grob("Max academic level of either parent"))

academic_lvl_plots

# ggsave(
#   filename = "article_plots/Academic_Level.png",
#   plot = academic_lvl_plots,
#   width = 7.5,
#   height = 4.2,
#   units = "in",
#   dpi = 300
# )

################################################################################
# SAVE THE DATA
################################################################################
# Note that the dataframe saved here has only the maximum academic level
# of either parent as described above.
# 739211 observations
# saveRDS(icfes_2022_mod, "Data/icfes_2022_mod.Rds")
















