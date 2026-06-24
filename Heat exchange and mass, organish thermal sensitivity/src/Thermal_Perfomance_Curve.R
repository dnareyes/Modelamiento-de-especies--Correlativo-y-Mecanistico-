# working directory
setwd("C:/.../curso/Dia 3 - Junio 17/03_pract") # set your own directory


# Thermal Performance Curves
library(tidyverse)
library(mgcv)
library(rTPC)
library(nls.multstart)
library(purrr)
library(tidyr)




# load data
df_perf <- read.delim('data/jump_perf/Leptodactylus_perf.txt')
df_perf <- df_perf %>% 
  filter(!is.na(JUMP_DISTANCE), !is.na(BODY_TEMPERATURE)) %>%
  mutate(ID = as.factor(ID),
         BODY_TEMPERATURE = as.numeric(BODY_TEMPERATURE),
         JUMP_DISTANCE = as.numeric(JUMP_DISTANCE))


# TPC with GAMM (Generalized Additive Mixed Models)
m_gam <- gam(JUMP_DISTANCE ~ s(BODY_TEMPERATURE, ID, bs = "fs", k = 5),
    data = df_perf, method = "REML")

# create a finer curve
temp_grid <- seq(min(df_perf$BODY_TEMPERATURE), max(df_perf$BODY_TEMPERATURE), length.out = 500)
gam_preds <- expand.grid(ID = unique(df_perf$ID), BODY_TEMPERATURE = temp_grid)

# Predict performance
gam_preds$pred <- predict(m_gam, newdata = gam_preds)

# calculate performance traits
gam_params <- gam_preds %>%
  group_by(ID) %>%
  summarise(
    Pmax = max(pred),
    Topt = BODY_TEMPERATURE[which.max(pred)],
    B80_lower = min(BODY_TEMPERATURE[pred >= 0.8 * Pmax]),
    B80_upper = max(BODY_TEMPERATURE[pred >= 0.8 * Pmax])
  )


# check TPCs
df_perf <- df_perf %>%
  mutate(PointType = case_when(
    TEMPERATURE == "ctmin" ~ "CTmin",
    TEMPERATURE == "ctmax" ~ "CTmax",
    TRUE ~ "Jumping trial"
  ))

ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_line(data = gam_preds %>% filter(pred >= 0), 
            aes(x = BODY_TEMPERATURE, y = pred), 
            color = "gray40", linewidth = 1) +
  geom_point(data = df_perf, aes(x = BODY_TEMPERATURE, y = JUMP_DISTANCE, color = PointType), 
             size = 2.5, alpha = 0.8) +
  scale_color_manual(values = c("CTmin" = "blue", 
                                "CTmax" = "red", 
                                "Jumping trial" = "gray20"),
                     name = "Observation") +
  facet_wrap(~ ID, ncol = 5) + 
  labs(x = "Body Temperature (°C)",
       y = "Jump Distance (cm)",
       title = "Thermal Performance Curves (GAMM)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
        legend.position = "bottom")












# TPC using rTPC package

# fit gaussian TPC
fits <- df_perf %>%
  group_by(ID) %>%
  nest() %>%
  mutate(model = map(data, ~nls_multstart(JUMP_DISTANCE ~ gaussian_1987(temp = BODY_TEMPERATURE, rmax, topt, a),
                                          data = .x,
                                          iter = 500,
                                          # Wide starting bounds to ensure it catches the curve
                                          start_lower = c(rmax = 50, topt = 20, a = 2),
                                          start_upper = c(rmax = 200, topt = 35, a = 15),
                                          supp_errors = "Y")))

# create a finer curve
temp_grid <- seq(min(df_perf$BODY_TEMPERATURE), max(df_perf$BODY_TEMPERATURE), length.out = 500)

# get predictions
gaussian_preds <- fits %>%
  mutate(predictions = map(model, ~data.frame(
    BODY_TEMPERATURE = temp_grid,
    pred = predict(.x, newdata = data.frame(BODY_TEMPERATURE = temp_grid))
  ))) %>%
  select(ID, predictions) %>%
  unnest(predictions)

# calculate performance traits
gaussian_params <- gaussian_preds %>%
  group_by(ID) %>%
  summarise(
    Pmax = max(pred),
    Topt = BODY_TEMPERATURE[which.max(pred)],
    B80_lower = min(BODY_TEMPERATURE[pred >= 0.8 * Pmax]),
    B80_upper = max(BODY_TEMPERATURE[pred >= 0.8 * Pmax])
  )

# check TPCs
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_line(data = gaussian_preds %>% filter(pred >= 0), 
            aes(x = BODY_TEMPERATURE, y = pred), 
            color = "gray40", linewidth = 1) +
  geom_point(data = df_perf, 
             aes(x = BODY_TEMPERATURE, y = JUMP_DISTANCE, color = PointType), 
             size = 2.5, alpha = 0.8) +
  scale_color_manual(values = c("CTmin" = "blue", 
                                "CTmax" = "red", 
                                "Jumping trial" = "gray20"),
                     name = "Observation") +
  facet_wrap(~ ID, ncol = 5) + 
  labs(x = "Body Temperature (°C)",
       y = "Jump Distance (cm)",
       title = "Gaussian Thermal Performance Curves (rTPC)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
        legend.position = "bottom")


# 
gam_df <- gam_params %>% mutate(Method = "GAMM")
gauss_df <- gaussian_params %>% mutate(Method = "Gaussian (rTPC)")
combined_params <- bind_rows(gam_df, gauss_df)

# wide to long format
long_params <- combined_params %>%
  pivot_longer(cols = c(Pmax, Topt, B80_lower, B80_upper),
               names_to = "Trait", values_to = "Value") %>%
  mutate(Trait = factor(Trait, levels = c("Topt", "Pmax", "B80_lower", "B80_upper")))

# plot
ggplot(long_params, aes(x = Method, y = Value, fill = Method)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5) +
  geom_point(aes(fill = Method), shape = 21, size = 2.5, color = "black", alpha = 0.8) +
  facet_wrap(~ Trait, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("GAMM" = "#3B9AB2", "Gaussian (rTPC)" = "#E1AF00")) +
  labs(title = "Comparison of TPC Parameters by Modeling Method",
       x = NULL, y = "Estimated Value") +
  theme_bw()

#











# calculating body temperature (Tb) and evaporative water loss (EWL)

# load the function for calculating tb and ewl
source("data/extra.R")


# define animal parameters
M_frog  <- 35                                       # body mass (g)
A_frog  <- 8.6856 * (M_frog^0.6652) / 10000         # Surface area (m2) (for Leptodactylidae)
#A_frog  <- 9.1411 * (M_frog^0.7735) / 10000          # Surface area (m2) (for Hylidae)
#A_frog  <- 7.9560 * (M_frog^0.6772) / 10000         # Surface area (m2) (for Bufonidae)
#A_frog  <- 9.9632 * (M_frog^0.8936) / 10000         # Surface area (m2) (for Craugastoridae)
#A_frog  <- 9.8537 * (M_frog^0.6745) / 10000         # Surface area (m2) (for Anura)
# check Klein et al 2016 for more anuran families (http://dx.doi.org/10.1016/j.jcz.2016.04.007)
Rs_frog <- 2.46 * 100

# define an hypothetical ambient conditions
Ta_example <- 25    # Air temperature (°C)
Tg_example <- 26    # Ground temperature (°C)
S_example  <- 300   # Solar radiation (W m-2)
v_example  <- 0.5   # Wind speed (m s-1)
RH_example <- 75    # Relative Humidity (%)

# 3. Run the function JUST ONCE
result <- theatmodel(
  Tb = Ta_example,  
  A  = A_frog, 
  M  = M_frog, 
  Ta = Ta_example, 
  Tg = Tg_example, 
  S  = S_example, 
  v  = v_example, 
  HR = RH_example, 
  skin_humidity = 1, 
  r  = Rs_frog, 
  posture = 1, 
  vent = 1/3, 
  delta = 3600,     
  C = 3.6
)

cat("Final Body Temperature (Tb):", round(result[1], 2), "°C\n")
cat("Evaporative Water Loss (EWL):", round(result[2] * 3600, 4), "g/hour\n")

# end---