################################################################################
# 1. PACKAGES AND DATA CONFIGURATION
################################################################################ 
# PUT YOUR LOCAL PATH HERE  

library(INLA)

# icfes data in 2022 ready for modeling
icfes_2022_mod <- readRDS("Data/icfes_2022_mod.Rds")
mcpals <- readRDS("Data/col_mpio_geospatial.Rds")
dpto <- readRDS("Data/col_dpto_geospatial.Rds")

################################################################################
# 1. MODELING
# "Best" model from the linear fits
################################################################################
lm_formula <- PUNT_MATEMATICAS ~ COLE_JORNADA_IMPUTED + FAMI_ESTRATOVIVIENDA_IMPUTED + 
  FAMI_TIENECOMPUTADOR + COLE_NATURALEZA + ACADEMIC_LEVEL + 
  FAMI_ESTRATOVIVIENDA_IMPUTED * FAMI_TIENECOMPUTADOR + FAMI_ESTRATOVIVIENDA_IMPUTED * 
  COLE_NATURALEZA

# ------------------------------------------------------------------------------
# MODELS FORMULA
# PC PRIORR FOR MODEL COMPONENTS
# ------------------------------------------------------------------------------

## error, school and spatial standard devation prior
precprior <- list(prec = list(prior = "pc.prec", param = c(10, 0.01)))

# ONLY BESAG FORMULA
besag_formula <- update(
  lm_formula, . ~ . +
    f(idx_area, model = "besag", graph = "Data/graph_municipals",
      scale.model = TRUE,
      hyper = precprior)
)

# BESAG AND SCHOOLS FORMULA
besag_schools_formula <- update(
  lm_formula, . ~ . +
    # Besag component
    f(idx_area, model = "besag", graph = "Data/graph_municipals",
      scale.model = TRUE,
      hyper = precprior) +
    # schools component
    f(idx_school, model = "iid",
      hyper = precprior)
)

# ------------------------------------------------------------------------------
# MODEL FITING
# ------------------------------------------------------------------------------

ccomp <- list(
  dic = TRUE,
  waic = TRUE,
  cpo = TRUE
)

# Simple linear fit
# This is to compare the models with LGOCV
linear_fit <- inla(
  formula = lm_formula,
  data = icfes_2022_mod,
  control.family = list(hyper = precprior),
  verbose = FALSE,
  control.fixed = list(prec = 0.1),
  control.compute = ccomp
)

# Fit besag model
besag_model <- inla(
  formula = besag_formula,
  data = icfes_2022_mod,
  control.family = list(hyper = precprior),
  verbose = FALSE,
  control.fixed = list(prec = 0.1),
  control.compute = ccomp
)

# Optional rerun for stability
besag_model <- inla.rerun(besag_model)

# Fit besag and schools model
besag_schools_model <- inla(
  formula = besag_schools_formula,
  data = icfes_2022_mod,
  control.family = list(hyper = precprior),
  verbose = FALSE,
  control.fixed = list(prec = 0.1),
  control.compute = ccomp
)

# Optional rerun for stability
besag_schools_model <- inla.rerun(besag_schools_model)

# SAVE THE MODELS
saveRDS(linear_fit, "linear_fit.Rds")
saveRDS(besag_model, "besag_model.Rds")
saveRDS(besag_schools_model, "besag_schools_model.Rds")

# LOAD MODELS
linear_fit <- readRDS("Models/linear_fit.Rds")
besag_model <- readRDS("Models/besag_model.Rds")
besag_schools_model <- readRDS("Models/besag_schools_model.Rds")

################################################################################
# 2. COMPARISON WITH LGOCV
################################################################################
# LEAVE GROUP OUT CROSS VALIDATION
# Function to compute the CV metric
extract_groupcv <- function(inla_group_cv_object){
  # Remove NA for the object with the cv values
  cv_metrics <- inla_group_cv_object$cv[!is.na(inla_group_cv_object$cv)]
  # model group cv
  group_cv <- -mean(log(cv_metrics))
  return(group_cv)
}

# Indices for the LGOCV
set.seed(3435)
groupcv_idx <- sample(1:dim(icfes_2022_mod)[1], 10000)
# groupcv_idx <- seq(1, dim(icfes_2022_mod)[1], by = 50)

# LGOCV object for linear model
lgocv_lm <- inla.group.cv(
  linear_fit,
  num.level.sets = 1,
  verbose = TRUE,
  size.max = 8,
  selection = groupcv_idx)

# LGOCV object for Besag 
lgocv_besag <- inla.group.cv(
  besag_model,
  num.level.sets = 1,
  verbose = TRUE,
  size.max = 8,
  selection = groupcv_idx)

# LGOCV BESAG AND SCHOOLS
lgocv_besag_schools <- inla.group.cv(
  besag_schools_model,
  num.level.sets = 1,
  verbose = TRUE,
  size.max = 8,
  selection = groupcv_idx)

# LGOCV COMPUTATION
extract_groupcv(lgocv_lm) # Linear model
extract_groupcv(lgocv_besag) # Besag
extract_groupcv(lgocv_besag_schools) # Besag and Schools


################################################################################
# 3. POSTERIOR DENSITY OF HYPER-PARAMS
################################################################################
library(ggplot2)
library(dplyr)

# Plotting the density of the hyperparameters
# Transformation of internal parameterization to parameters original scale
post_sd_gaussian_obs <- inla.tmarginal(function(x) exp(-x/2), besag_schools_model$internal.marginals.hyperpar[[1]])

post_sd_area <- inla.tmarginal(function(x) exp(-x/2), besag_schools_model$internal.marginals.hyperpar[[2]])

post_sd_school <- inla.tmarginal(function(x) exp(-x/2), besag_schools_model$internal.marginals.hyperpar[[3]])

# PLOTS OF THE POSTERIOR OF HYPER-PARAMS
post_sd_gaussian_obs_plot <- ggplot(post_sd_gaussian_obs, aes(x, y)) + 
  labs(title = "Gaussian observations", x = expression(sigma[y]), y = "") +
  geom_line() +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5))

# sigma_s
post_sd_area_plot <- ggplot(post_sd_area, aes(x, y)) + 
  labs(title = "Spatial standard deviation", x = expression(sigma[u]), y = " ") +
  geom_line() +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5))

# sigma_b
post_sd_school_plot <- ggplot(post_sd_school, aes(x, y)) + 
  labs(title = "School's standard deviation", x = expression(sigma[b]), y = " ") +
  geom_line() +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5))

# Grid with the plots
hyper_params_plot <- ggpubr::ggarrange(post_sd_gaussian_obs_plot, post_sd_area_plot,
                                       post_sd_school_plot, ncol = 2, nrow = 2)

hyper_params_plot

# ggsave(
#   filename = "article_plots/hyperparam_post.png",
#   plot = hyper_params_plot,
#   width = 7.5,
#   height = 4.2,
#   units = "in",
#   dpi = 300
# )


# TABLE WITH THE MARGINAL SUMMARIES AND UNCERTAINTY QUANTIFICATION
# Function to summarize one marginal
summ_marginal_hpd <- function(marg, prob = 0.95) {
  hpd <- inla.hpdmarginal(prob, marg)
  
  c(
    mean = inla.emarginal(function(x) x, marg),
    sd = sqrt(inla.emarginal(function(x) x^2, marg) -
                inla.emarginal(function(x) x, marg)^2),
    hpd_lower = hpd[1],
    hpd_upper = hpd[2]
  )
}

# Hyperparameter summary table
hyper_hpd_table <- rbind(
  sigma_y = summ_marginal_hpd(post_sd_gaussian_obs),
  sigma_u = summ_marginal_hpd(post_sd_area),
  sigma_b = summ_marginal_hpd(post_sd_school)
)

hyper_hpd_table <- as.data.frame(hyper_hpd_table)

# Round for report
hyper_hpd_table_round <- hyper_hpd_table
hyper_hpd_table_round[] <- round(hyper_hpd_table_round, 3)

hyper_hpd_table_round




# ------------------------------------------------------------------------------
# TOP MUNICIPALS AND SCHOOLS WITH UNCERTAINTY QUANTIFICATION
# ------------------------------------------------------------------------------

# Coerce to numerical values (the identifier)
mcpals <- mcpals %>% 
  mutate(codigo_departamento = as.numeric(codigo_departamento))

# Spatial random-effect posterior summaries
spatial_summary <- besag_schools_model$summary.random$idx_area
spatial_marginals <- besag_schools_model$marginals.random$idx_area

# Compute 95% HPD intervals for each municipality effect
spatial_hpd <- t(
  sapply(spatial_marginals, function(marg) {
    inla.hpdmarginal(0.95, marg)
  })
)

spatial_effects_table <- data.frame(
  idx_area = spatial_summary$ID,
  mean = spatial_summary$mean,
  sd = spatial_summary$sd,
  hpd_lower = spatial_hpd[, 1],
  hpd_upper = spatial_hpd[, 2]
)

# Add municipality information
# codigo_municipio is the original code in mcpals data.frame
spatial_effects_table$codigo_municipio <- mcpals$codigo_municipio[
  spatial_effects_table$idx_area
]

# Add municipality
# municipio is the name of the municiopal in mcpals data.frame
spatial_effects_table$nombre_municipio <- mcpals$municipio[
  spatial_effects_table$idx_area
]

# Add department code from municipality code
spatial_effects_table$codigo_departamento <- floor(
  spatial_effects_table$codigo_municipio / 1000
)

# Add department name from dpto
spatial_effects_table <- spatial_effects_table %>%
  left_join(
    dpto %>%
      sf::st_drop_geometry() %>%
      select(codigo_departamento, departamento),
    by = "codigo_departamento"
  )


# Top best municipals
spatial_effects_table %>% 
  select(departamento, nombre_municipio, mean, sd, 
         hpd_lower, hpd_upper) %>% 
  arrange(desc(mean)) %>% 
  head(5)

# Top worse
spatial_effects_table %>% 
  select(departamento, nombre_municipio, mean, sd, 
         hpd_lower, hpd_upper) %>%
  arrange(mean) %>% 
  head(5)

# ------------------------------------------------------------------------------
# PLOT OF RANDOM EFFECTS WITH UNCERTAINTY
# ------------------------------------------------------------------------------

# 1. Municipality spatial effects
spatial_hpd <- t(
  sapply(spatial_marginals, function(marg) {
    inla.hpdmarginal(0.95, marg)
  })
)

# Auxiliar data.frame to plot the ranking
spatial_effects_plot_df <- spatial_effects_table %>%
  arrange(mean) %>%
  mutate(order_id = row_number())

spatial_effects_plot <- ggplot(
  spatial_effects_plot_df,
  aes(x = order_id, y = mean)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
  geom_errorbar(
    aes(ymin = hpd_lower, ymax = hpd_upper),
    width = 0,
    alpha = 0.35
  ) +
  geom_point(size = 0.7) +
  labs(
    x = "Municipalities ordered by posterior mean",
    y = "Spatial effect",
    title = "Municipality-level spatial effects"
  ) +
  theme_bw()


# 2. School random effects
school_summary <- besag_schools_model$summary.random$idx_school
school_marginals <- besag_schools_model$marginals.random$idx_school

school_hpd <- t(
  sapply(school_marginals, function(marg) {
    inla.hpdmarginal(0.95, marg)
  })
)

school_effects_table <- data.frame(
  idx_school = school_summary$ID,
  mean = school_summary$mean,
  sd = school_summary$sd,
  hpd_lower = school_hpd[, 1],
  hpd_upper = school_hpd[, 2]
)

school_effects_plot_df <- school_effects_table %>%
  arrange(mean) %>%
  mutate(order_id = row_number())

school_effects_plot <- ggplot(
  school_effects_plot_df,
  aes(x = order_id, y = mean)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
  geom_errorbar(
    aes(ymin = hpd_lower, ymax = hpd_upper),
    width = 0,
    alpha = 0.25
  ) +
  geom_point(size = 0.35) +
  labs(
    x = "Schools ordered by posterior mean",
    y = "School effect",
    title = "School-level random effects"
  ) +
  theme_bw()

# 3. Grid plot
random_effects_grid <- ggpubr::ggarrange(spatial_effects_plot, 
                                         school_effects_plot,
                                         nrow = 2, ncol = 1) 
random_effects_grid

# ggsave(
#   filename = "article_plots/random_effects_grid.png",
#   plot = random_effects_grid,
#   width = 7.5,
#   height = 4.2,
#   units = "in",
#   dpi = 300
# )


################################################################################
# 4. MODEL DIAGNOSTICS
################################################################################

cpo_final_mod <- besag_schools_model$cpo

pit_df <- data.frame(
  pit = cpo_final_mod$pit
)

# Choose fixed bins over [0, 1]
pit_breaks <- seq(0, 1, by = 0.05)

pit_histogram <- ggplot(pit_df, aes(x = pit)) +
  geom_histogram(
    aes(y = after_stat(density)),
    breaks = pit_breaks,
    fill = "lightgray",
    col = "black",
    closed = "right"
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    expand = c(0, 0)
  ) +
  labs(
    x = "PIT values",
    y = "Density",
    title = " "
  ) +
  theme_bw()

pit_histogram

# ggsave(
#   filename = "article_plots/pit_histogram.png",
#   plot = pit_histogram,
#   width = 7.5,
#   height = 4.2,
#   units = "in",
#   dpi = 300
# )


################################################################################
# 5. RESULTS FOR THE REPORT
################################################################################

# ------------------------------------------------------------------------------
# FIXED EFFECTS ANALYSIS
# ------------------------------------------------------------------------------
# Posterior mean, SD, and 95% HPD credible intervals for fixed effects

fixed_summary <- besag_schools_model$summary.fixed
fixed_marginals <- besag_schools_model$marginals.fixed

hpd_fixed <- t(
  sapply(fixed_marginals, function(marg) {
    inla.hpdmarginal(0.95, marg)
  })
)

fixed_hpd_table <- data.frame(
  term = rownames(fixed_summary),
  mean = fixed_summary[, "mean"],
  sd = fixed_summary[, "sd"],
  hpd_lower = hpd_fixed[, 1],
  hpd_upper = hpd_fixed[, 2],
  row.names = NULL
)

# Rounded version
fixed_hpd_table_round <- fixed_hpd_table

fixed_hpd_table_round[, c("mean", "sd", "hpd_lower", "hpd_upper")] <- round(
  fixed_hpd_table_round[, c("mean", "sd", "hpd_lower", "hpd_upper")],
  4
)

fixed_hpd_table_round




# ------------------------------------------------------------------------------
# SPATIAL EFFECTS MAPS
# ------------------------------------------------------------------------------
# Add posterior summaries to the spatial dataframe
# This assumes idx_area matches the row order of mcpals

# Add posterior summaries to the spatial dataframe
# This assumes idx_area matches the row order of mcpals
mcpals_plot <- mcpals %>%
  mutate(idx_area = row_number()) %>%
  left_join(
    spatial_effects_table %>%
      select(
        idx_area,
        spatial_mean = mean,
        spatial_sd = sd,
        hpd_lower,
        hpd_upper
      ),
    by = "idx_area"
  )

# Posterior mean map
map_spatial_mean <- ggplot(mcpals_plot) +
  geom_sf(aes(fill = spatial_mean), color = NA) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "gray90"
  ) +
  labs(
    title = "Expected value",
    fill = NULL
  ) +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Posterior standard deviation map
map_spatial_sd <- ggplot(mcpals_plot) +
  geom_sf(aes(fill = spatial_sd), color = NA) +
  labs(title = "Standard deviation") +
  scale_fill_distiller(
    palette = "BuPu",
    name = " ",
    trans = "sqrt",
    breaks = c(0.2, 0.5, 1, 2, 4),
    labels = function(x) format(x, digits = 2, scientific = FALSE)
  ) +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Grid plot
spatial_maps_grid <- ggpubr::ggarrange(
  map_spatial_mean,
  map_spatial_sd,
  nrow = 1,
  ncol = 2
)

spatial_maps_grid

# ggsave(
#   filename = "article_plots/spatial_effects.png",
#   plot = spatial_maps_grid,
#   width = 7.5,
#   height = 4.2,
#   units = "in",
#   dpi = 300
# )
