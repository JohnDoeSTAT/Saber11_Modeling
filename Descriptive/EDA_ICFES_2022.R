library(tidyverse)
icfes_2022 <- readRDS("Data/icfes_2022_cleaned.Rds")

# Check the available variables
names(icfes_2022)

# Response variable: PUNT_MATEMATICAS
# HISTOGRAM OF THE RESPONSE VARIABLE
math_hist <- ggplot(icfes_2022, aes(x = PUNT_MATEMATICAS)) +
  geom_histogram(fill = "lightgreen", col = "black") +
  labs(x = "Score", y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5))

math_hist

# SAVE THE HISTOGRAM
# ggsave(
#   filename = "article_plots/Math_Score_Histogram.png",
#   plot = math_hist,
#   width = 7.5,
#   height = 4.2,
#   units = "in",
#   dpi = 300
# )

################################################################################
# 1. Comparison among the score and school features
################################################################################
# COLE_AREAUBICACION, COLE_CALENDARIO, COLE_GENERO, COLE_NATURALEZA, COLE_BILINGUE, COLE_CARACTER, 
# COLE_DEPTO_UBICACION, COLE_JORNADA

# COLE_AREA_UBICACION
icfes_2022 %>%
  select(COLE_AREA_UBICACION) %>%
  group_by(COLE_AREA_UBICACION) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))
#
ggplot(icfes_2022, aes(x = COLE_AREA_UBICACION, y = PUNT_MATEMATICAS)) +
  geom_boxplot(aes(fill = COLE_AREA_UBICACION), notch = T) +
  labs(title = "Boxplot for distribution of math score for school calendar type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5))
# No difference. URBANO slightly above.





# COLE_CALENDARIO: Type of the calendar used by the school
# A: From february to november
# B: From september to june
# Otro: Any other type of calendar used by the particular institution
icfes_2022 %>%
  select(COLE_CALENDARIO) %>%
  group_by(COLE_CALENDARIO) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))

# Props: 
# A -> 98.2%
# B -> 1.53%
# OTHER -> 0.24%
# HIGHLY UNBALANCED CATEGORY -> DROP


# COLE_GENERO: Type of the gener present inside the school
icfes_2022 %>%
  select(COLE_GENERO) %>%
  group_by(COLE_GENERO) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))
# FEMALE (Only women): 2.86%
# MALE (Only men): 0.94%
# MIXED (Both geners): 96.2%
# HIGHLY UNBALANCED CATEGORY -> DROP



# COLE_NATURALEZA
icfes_2022 %>%
  select(COLE_NATURALEZA) %>%
  group_by(COLE_NATURALEZA) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))

# UNOFFICIAL: 22.6%
# OFICIAL: 77.4%
schools_nature <- ggplot(icfes_2022, aes(x = COLE_NATURALEZA, y = PUNT_MATEMATICAS)) +
  geom_boxplot(aes(fill = COLE_NATURALEZA), notch = T) +
  guides(fill = "none") +
  labs(title = "Math score by \ninstitution type", y = "Math score", x = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5))

schools_nature
# The comparison among private and public schools is always there. This variable
# is of interest.

# COLE_BILINGUE: Whether or not, the students receive lectures in english as well
icfes_2022 %>%
  select(COLE_BILINGUE) %>%
  group_by(COLE_BILINGUE) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))

# N: 98.4%
# S: 1.63%
# HIGHLY UNBALANCED CATEGORY -> DROP


# COLE_CARACTER: Specialty of the institution. Academic, industrial or both.
icfes_2022 %>%
  select(COLE_CARACTER_IMPUTED) %>%
  group_by(COLE_CARACTER_IMPUTED) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))

# ACADEMIC: 50.8%
# DOES NOT APPLY: 0.15%
# TECHNICAL: 11.8%
# TECHNICAL/ACADEMIC: 37.2%
ggplot(icfes_2022, aes(x = COLE_CARACTER_IMPUTED, y = PUNT_MATEMATICAS)) +
  geom_boxplot(aes(fill = COLE_CARACTER_IMPUTED), notch = T) +
  labs(title = "Boxplot for distribution of math for school speciality") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5))
# OVERLAPPING



# COLE_JORNADA: The classes are on the morning, afternoon or weekends
freq_academic_day <- icfes_2022 %>%
  select(COLE_JORNADA_IMPUTED) %>%
  group_by(COLE_JORNADA_IMPUTED) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  mutate(Proportion = Frequency / sum(Frequency)) %>%
  ggplot(aes(COLE_JORNADA_IMPUTED, Frequency)) +
  geom_col(aes(fill = COLE_JORNADA_IMPUTED), col = "black") +
  geom_text(aes(label = scales::percent(Proportion)), vjust = -0.35) +
  labs(y = "Frequency",
       x = "",
       title = "",
       fill = "Academic day") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank())

freq_academic_day

# COMPLETE: 43.99%
# MORNINGS: 38.6%
# NIGHTS: 4.05%
# SATURDAYS: 4.06%
# AFTERNOONS: 9.3%

# MATH SCORE ON EACH LEVEL
academic_day <- ggplot(icfes_2022, aes(x = COLE_JORNADA_IMPUTED, y = PUNT_MATEMATICAS)) +
  geom_boxplot(aes(fill = COLE_JORNADA_IMPUTED), notch = T) +
  labs(y = "Math score", x = "", fill = "Academic day") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5), axis.text.x = element_blank())

academic_day

# GRID WITH BOTH PLOTS
ggpubr::ggarrange(freq_academic_day, academic_day,
                  common.legend = T, legend = "bottom") %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("School's type of academic day"))

# THE VARIABLE IS INCLUDED

################################################################################
# 2. Features of the students
################################################################################
# ESTU_GENERO
ggplot(icfes_2022, aes(x = ESTU_GENERO, y = PUNT_MATEMATICAS)) +
  geom_boxplot(aes(fill = ESTU_GENERO), notch = T) +
  labs(title = "", fill = "Gender", y = "Score") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = .5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
# OVERLAP

################################################################################
# 3. Family features
################################################################################
# FAMI_ESTRATOVIVIENDA_IMPUTED,  FAMI_PERSONASHOGAR, FAMI_CUARTOSHOGAR, 
# FAMI_TIENEAUTOMOVIL, FAMI_TIENECOMPUTADOR, FAMI_TIENEINTERNET, 
# FAMI_TIENELAVADORA, FAMI_EDUCACIONMADRE and PADRE.

# SOCIO-ECONOMIC STRATUM
icfes_2022 %>%
  select(FAMI_ESTRATOVIVIENDA_IMPUTED) %>%
  group_by(FAMI_ESTRATOVIVIENDA_IMPUTED) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))
# NON UNBALANCED PROPORTIONS

ggplot(
  icfes_2022 %>%
    mutate(
      FAMI_ESTRATOVIVIENDA_IMPUTED = as.factor(FAMI_ESTRATOVIVIENDA_IMPUTED)
    ),
  aes(x = FAMI_ESTRATOVIVIENDA_IMPUTED, y = PUNT_MATEMATICAS)
) +
  scale_x_discrete() +
  geom_boxplot(aes(fill = FAMI_ESTRATOVIVIENDA_IMPUTED), notch = T) +
  labs(title = "Boxplot for distribution of math score per socioeconomic level") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90))
# APPARENT INCREASING TREND


# FAMI_PERSONAS_HOGAR
icfes_2022 %>%
  select(FAMI_PERSONASHOGAR) %>%
  group_by(FAMI_PERSONASHOGAR) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))
# UNBALANCED CATEGORY


# FAMI_CUARTOS_HOGAR: Number of bedrooms inside the house
icfes_2022 %>%
  select(FAMI_CUARTOSHOGAR) %>%
  group_by(FAMI_CUARTOSHOGAR) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))

# NON UNBALANCED PROPORTIONS
ggplot(icfes_2022, aes(x = as.factor(FAMI_CUARTOSHOGAR), y = PUNT_MATEMATICAS)) +
  geom_boxplot(aes(fill = as.factor(FAMI_CUARTOSHOGAR)), notch = T) +
  labs(title = "Boxplot for distribution of math score per number of bedrooms in the home", fill = "FAMI_CUARTOSHOGAR") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90))
# OVERLAPPING


# FAMI_TIENEAUTOMOVIL: Whether or not the family has a car
icfes_2022 %>%
  select(FAMI_TIENEAUTOMOVIL) %>%
  group_by(FAMI_TIENEAUTOMOVIL) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))

ggplot(icfes_2022, aes(x = FAMI_TIENEAUTOMOVIL, y = PUNT_MATEMATICAS)) +
  geom_boxplot(aes(fill = FAMI_TIENEAUTOMOVIL), notch = T) +
  labs(title = "Boxplot for distribution of math score if family has car") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90))
# OVERLAP, BUT IT IS NOT A CRAZY IDEA TO INCLUDE IT.


# FAMI_TIENECOMPUTADOR: Whether or not the family has a computer
icfes_2022 %>%
  select(FAMI_TIENECOMPUTADOR) %>%
  group_by(FAMI_TIENECOMPUTADOR) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))
# BALANCED PROPORTIONS

has_computer <- ggplot(icfes_2022, aes(x = FAMI_TIENECOMPUTADOR, y = PUNT_MATEMATICAS)) +
  geom_boxplot(aes(fill = FAMI_TIENECOMPUTADOR), notch = T) +
  labs(title = "Math score if family \nhave computer", y = "Math score", x = "") +
  guides(fill = "none") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5))

has_computer

# VARIABLE TO INCLUDE


# FAMI_TIENEINTERNET: Whether or not the family has internet
icfes_2022 %>%
  select(FAMI_TIENEINTERNET) %>%
  group_by(FAMI_TIENEINTERNET) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))
# ACCEPTABLE PROPORTION

ggplot(icfes_2022, aes(x = FAMI_TIENEINTERNET, y = PUNT_MATEMATICAS)) +
  geom_boxplot(aes(fill = FAMI_TIENEINTERNET), notch = T) +
  labs(title = "Boxplot for distribution of math score if family has internet") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90))
# IT COULD BE INCLUDED


# FAMI_TIENELAVADORA: Whether or not the family has a washing machine
icfes_2022 %>%
  select(FAMI_TIENELAVADORA) %>%
  group_by(FAMI_TIENELAVADORA) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(prop = count / sum(count))

ggplot(icfes_2022, aes(x = FAMI_TIENELAVADORA, y = PUNT_MATEMATICAS)) +
  geom_boxplot(aes(fill = FAMI_TIENELAVADORA), notch = T) +
  labs(title = "Boxplot for distribution of math score if family has washing machine") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90))
# OVERLAPPING

# CAR, INTERNET, WASHING MACHINE shows a similar trend as COMPUTADOR. 
# Include only computer?

# ------------------------------------------------------------------------------
# PARENTS EDUCATION LEVEL SUB-BLOCK
# ------------------------------------------------------------------------------
# FAMI_EDUCACIONMADRE
academic_order <- c("Primary incomplete", "Primary", "High school",
                    "Technical", "Graduate", "Postgraduate")
mom_academic_lvl <- ggplot(
  icfes_2022 %>%
    mutate(
      FAMI_EDUCACIONMADRE_IMPUTED =
        factor(FAMI_EDUCACIONMADRE_IMPUTED, levels = academic_order)
    ),
  aes(x = FAMI_EDUCACIONMADRE_IMPUTED, y = PUNT_MATEMATICAS)
) +
  geom_boxplot(aes(fill = FAMI_EDUCACIONMADRE_IMPUTED), notch = T) +
  labs(title = "",
       y = "Math score",
       x = "",
       fill = "Academic level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5), axis.text.x = element_blank())

# Barplot for the frequencies
options(scipen = 9999)

mom_academic_freq <- icfes_2022 %>%
  select(FAMI_EDUCACIONMADRE_IMPUTED) %>%
  group_by(FAMI_EDUCACIONMADRE_IMPUTED) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  mutate(Proportion = Frequency / sum(Frequency)) %>%
  ggplot(aes(FAMI_EDUCACIONMADRE_IMPUTED, Frequency)) +
  geom_col(aes(fill = FAMI_EDUCACIONMADRE_IMPUTED), col = "black") +
  geom_text(aes(label = scales::percent(Proportion)), vjust = -0.35) +
  labs(y = "Frequency",
       x = "",
       title = "",
       fill = "Academic level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank())

# GRID FOR THE PLOTS
ggpubr::ggarrange(mom_academic_freq, mom_academic_lvl,
                  common.legend = T, legend = "bottom") %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("Mom's academic level"))

# INCREASING TREND FOR THE ACADEMIC LEVEL OF THE MOTHER




# FAMI_EDUCACIONPADRE
# COMPLETELY ANALOGOUS PROCESS
dad_academic_lvl <- ggplot(
  icfes_2022 %>%
    mutate(
      FAMI_EDUCACIONPADRE_IMPUTED =
        factor(FAMI_EDUCACIONPADRE_IMPUTED, levels = academic_order)
    ),
  aes(x = FAMI_EDUCACIONPADRE_IMPUTED, y = PUNT_MATEMATICAS)
) +
  geom_boxplot(aes(fill = FAMI_EDUCACIONPADRE_IMPUTED), notch = T) +
  labs(x = "", y = "Math score", fill = "Academic level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5), axis.text.x = element_blank())

options(scipen = 9999)

dad_academic_freq <- icfes_2022 %>% 
  select(FAMI_EDUCACIONPADRE_IMPUTED) %>% 
  group_by(FAMI_EDUCACIONPADRE_IMPUTED) %>% 
  summarise(Frequency = n()) %>% 
  ungroup() %>% 
  mutate(Proportion = Frequency/sum(Frequency)) %>% 
  ggplot(aes(FAMI_EDUCACIONPADRE_IMPUTED, Frequency)) +
  geom_col(aes(fill = FAMI_EDUCACIONPADRE_IMPUTED), col = "black") +
  geom_text(aes(label = scales::percent(Proportion)), vjust = -0.35) +
  labs(y = "Frequency", x = "", fill = "Academic level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank())

# GRID WITH THE PLOTS
ggpubr::ggarrange(dad_academic_freq, dad_academic_lvl,
                  common.legend = T, legend = "bottom") %>% 
  ggpubr::annotate_figure(ggpubr::text_grob("Dad's academic level"))
# ANALOGOUS TREND AS THE ACADEMIC LEVEL OF THE MOTHER


# GRID WITH THE BOXPLOTS OF EACH ACADEMIC LEVEL
ggpubr::ggarrange(mom_academic_lvl + labs(x = "Mom's academic level"),
                  dad_academic_lvl + labs(x = "Dad's academic level"),
                  common.legend = T, legend = "bottom") %>% 
  ggpubr::annotate_figure(ggpubr::text_grob("Parent's academic level"))
# REDUNDANT TO KEEP BOTH ACADEMIC LEVELS?
# IDEA: KEEP ONLY THE HIGHEST ACADEMIC LEVEL INSTEAD OF BOTH
#-------------------------------------------------------------------------------
# END OF ACADEMIC LEVEL SUB-BLOCK
# ------------------------------------------------------------------------------

################################################################################
# 4. CATEGORICAL VARIABLE RELATIONSHIPS
################################################################################
#-------------------------------------------------------------------------------
# COLE NATURALEZA (INSTITUTION TYPE) AND STRATUM
# Freqs and percentages
stratum_nature_freqs <- icfes_2022 %>%
  select(FAMI_ESTRATOVIVIENDA_IMPUTED, COLE_NATURALEZA) %>%
  group_by(FAMI_ESTRATOVIVIENDA_IMPUTED, COLE_NATURALEZA) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  mutate(Proportion = Frequency / sum(Frequency)) %>%
  ggplot(aes(FAMI_ESTRATOVIVIENDA_IMPUTED, Frequency, fill = COLE_NATURALEZA)) +
  geom_col(col = "black", position = position_dodge()) +
  lims(y = c(0, 300000)) +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 0.01)),
            vjust = -0.5,
            position = position_dodge(width = 1)) +
  labs(y = "",
       x = "",
       title = "",
       fill = "Institution type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Interaction plot
schools_nature_and_stratum <- ggplot(
  icfes_2022 %>%
    mutate(
      FAMI_ESTRATOVIVIENDA_IMPUTED = factor(FAMI_ESTRATOVIVIENDA_IMPUTED, levels = 1:6)
    ),
  aes(x = FAMI_ESTRATOVIVIENDA_IMPUTED, y = PUNT_MATEMATICAS)
) +
  geom_boxplot(aes(fill = COLE_NATURALEZA), notch = T) +
  labs(y = "", x = "", fill = "Institution type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5), axis.text.x = element_blank())

#GRID WITH THE PLOTS
ggpubr::ggarrange(schools_natur_and_estrato, schools_nature_and_stratum, nrow = 2,
                  common.legend = T, legend = "bottom") %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("Math score per stratum level and institution type"))

# THIS MOTIVATES THE INTERACTION FAMI_ESTRATO_VIVIENDA * COLE_NATURALEZA



# ------------------------------------------------------------------------------
# HAVING A COMPUTER AND STRATUM
# Freqs and percentages
stratum_computer_freq <- icfes_2022 %>% 
  select(FAMI_ESTRATOVIVIENDA_IMPUTED, FAMI_TIENECOMPUTADOR) %>% 
  group_by(FAMI_ESTRATOVIVIENDA_IMPUTED, FAMI_TIENECOMPUTADOR) %>% 
  summarise(Frequency = n()) %>% 
  ungroup() %>% 
  mutate(Proportion = Frequency/sum(Frequency)) %>% 
  ggplot(aes(FAMI_ESTRATOVIVIENDA_IMPUTED, Frequency, fill = FAMI_TIENECOMPUTADOR)) +
  geom_col(col = "black",
           position = position_dodge()) +
  lims(y = c(0, 215000)) +
  geom_text(aes(label = scales::percent(round(Proportion, 4))),
            vjust = -0.5, position = position_dodge(width = 1)) +
  labs(y = "", x = "", title = "", fill = "Have a computer?") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Interaction plot
stratum_and_computer <- ggplot(
  icfes_2022 %>%
    mutate(
      FAMI_ESTRATOVIVIENDA_IMPUTED = factor(FAMI_ESTRATOVIVIENDA_IMPUTED, levels = 1:6)
    ),
  aes(x = FAMI_ESTRATOVIVIENDA_IMPUTED, y = PUNT_MATEMATICAS)
) +
  geom_boxplot(aes(fill = factor(FAMI_TIENECOMPUTADOR)), notch = T) +
  labs(y = "", x = "", fill = "Have a computer?") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5), axis.text.x = element_blank())

# GRID WITH THE PLOTS
int_plot_comp_str <- ggpubr::ggarrange(stratum_and_computer, stratum_computer_freq, nrow = 2,
                                       common.legend = T, legend = "bottom") %>% 
  ggpubr::annotate_figure(top = ggpubr::text_grob("Math score per stratum level and whether the family have a computer"))

int_plot_comp_str

# INCLUDE THE INTERACTION FAMI_TIENE_COMPUTADOR * FAMI_ESTRATTO_VIVIENDA


# SAVE THE INTERACTION PLOT OF COMPUTER AND STRATUM
# ggsave(
#   filename = "article_plots/stratum_computer.png",
#   plot = int_plot_comp_str,
#   width = 7.5,
#   height = 4.2,
#   units = "in",
#   dpi = 300
# )

################################################################################
# SUMMARY
################################################################################
# WE SELECT THE FOLLOWING VARIABLES
# COLE_JORNADA
# COLE_NATURALEZA
# FAMI_ESTRATOVIVIENDA
# FAMI_TIENECOMPUTADOR
# ACADEMIC LEVEL
# The inclusion of the academic level is revised through sensitivity analyzes

