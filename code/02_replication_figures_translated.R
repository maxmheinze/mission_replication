

# Header ------------------------------------------------------------------

# Load necessary packages
pacman::p_load(
  haven,
  dplyr,
  ggplot2
)

# Set data path
data_path <- "./data/source/Figures"

# Replication Package Translated ------------------------------------------

# **************************************
# Replication files for "The Mission"
# Felipe Valencia Caicedo (2018)
# TABLES
# translated to R
# **************************************

### Figure II

# Load data
data_figure_ii <- read_dta(file.path(data_path, "Figure II.dta"))

# Filter data where distmiss < 225
data_filtered_ii <- data_figure_ii %>% filter(distmiss < 225)

# Figure IIa
ggplot(data_filtered_ii, aes(x = distmiss, y = literacy)) +
  geom_point(aes(color = as.factor(mission))) +
  geom_smooth(method = "lm", se = TRUE, color = "black", aes(group = 1)) +
  scale_color_manual(values = c("blue", "red"), labels = c("Mission == 0", "Mission == 1")) +
  labs(x = "Distance to Mission", y = "Literacy", color = "Mission") +
  theme_minimal()

# Figure IIb
ggplot(data_filtered_ii, aes(x = distmiss, y = litresfecen)) +
  geom_point(aes(color = as.factor(mission))) +
  geom_smooth(method = "lm", se = TRUE, color = "black", aes(group = 1)) +
  scale_color_manual(values = c("blue", "red"), labels = c("Mission == 0", "Mission == 1")) +
  labs(x = "Distance to Mission", y = "Literacy (Demeaned and Centered)", color = "Mission") +
  theme_minimal()

# Figure IIc
ggplot(data_filtered_ii, aes(x = distmiss, y = literacy)) +
  stat_summary_bin(fun = "mean", bins = 20, geom = "point") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Distance to Mission", y = "Literacy") +
  theme_minimal()

### Figures III and IV

# Load data
data_figures_iii_iv <- read_dta(file.path(data_path, "Figures IIII IV.dta"))

# Filter data where distmiss < 225
data_filtered_iii_iv <- data_figures_iii_iv %>% filter(distmiss < 225)

# Figure IIIa
ggplot(data_filtered_iii_iv, aes(x = distmiss, y = lnincome)) +
  geom_point(aes(color = as.factor(mission))) +
  geom_smooth(method = "lm", se = TRUE, color = "black", aes(group = 1)) +
  scale_color_manual(values = c("blue", "red"), labels = c("Mission == 0", "Mission == 1")) +
  labs(x = "Distance to Mission", y = "Log Income", color = "Mission") +
  theme_minimal()

# Figure IIIb
ggplot(data_filtered_iii_iv, aes(x = distmiss, y = linresfecen)) +
  geom_point(aes(color = as.factor(mission))) +
  geom_smooth(method = "lm", se = TRUE, color = "black", aes(group = 1)) +
  scale_color_manual(values = c("blue", "red"), labels = c("Mission == 0", "Mission == 1")) +
  labs(x = "Distance to Mission", y = "Log Income (Demeaned and Centered)", color = "Mission") +
  theme_minimal()

# Figure IIIc
ggplot(data_filtered_iii_iv, aes(x = distmiss, y = lnincome)) +
  stat_summary_bin(fun = "mean", bins = 20, geom = "point") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Distance to Mission", y = "Log Income") +
  theme_minimal()

# Figure IV
ggplot(data_filtered_iii_iv, aes(x = distmiss, y = pca)) +
  stat_summary_bin(fun = "mean", bins = 20, geom = "point") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Distance to Mission", y = "PCA") +
  theme_minimal()

### Figure V

# Load data
data_figure_v <- read_dta(file.path(data_path, "Figure V.dta"))

# Filter data where distmiss < 245
data_filtered_v <- data_figure_v %>% filter(distmiss < 245)

# Figure V
ggplot(data_filtered_v, aes(x = distmiss, y = gsoy_TA)) +
  stat_summary_bin(fun = "mean", bins = 20, geom = "point") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Distance to Mission", y = "gsoy_TA") +
  theme_minimal()

