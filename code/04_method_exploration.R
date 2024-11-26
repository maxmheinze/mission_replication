
# Header ------------------------------------------------------------------

pacman::p_load(
  dplyr,
  magrittr,
  haven,
  fixest,
  BMS,
  BAS
)

rm(list=ls())

data_path <- "./data/source/Tables"





# Read in Data ------------------------------------------------------------

literacy_data <- read_dta(file.path(data_path, "Literacy Argentina Brazil Paraguay.dta"))

literacy_data <- literacy_data %>%
  mutate(mesorregi = as.factor(mesorregi))


# Create Matrix -----------------------------------------------------------

literacy_matrix <- cbind(
  literacy_data$illiteracy,
  literacy_data$distmiss,
  literacy_data$lati,
  literacy_data$longi,
  literacy_data$corr,
  literacy_data$ita,
  literacy_data$mis,
  literacy_data$mis1,
  literacy_data$area,
  literacy_data$tempe,
  literacy_data$alti,
  literacy_data$preci,
  literacy_data$rugg,
  literacy_data$river,
  literacy_data$coast,
  literacy_data$distfran
  #literacy_data$popd
  #exp(literacy_data$distmiss * -1),
  #exp(literacy_data$distfran * -1)
)

colnames(literacy_matrix) <- c(
  "illiteracy",
  "distmiss",
  "lati",
  "longi",
  "corr",
  "ita",
  "mis",
  "mis1",
  "area",
  "tempe",
  "alti",
  "preci",
  "rugg",
  "river",
  "coast",
  "distfran"
  #"popd"
  #"exp_distmiss",
  #"exp_distfran"
)

bmalit = bms(
  X.data = literacy_matrix,
  burn = 10000,
  iter = 100000,
  nmodel = 100, 
  mprior="fixed", 
  g="UIP",  
  mprior.size=7, 
  user.int=T
)


# summary(bmalit)

# coef(bmalit)

# beta.draws.bma(bmalit[1:3])

pdf("/home/max/research/mission_replication/image_without_popd.pdf", width = 8, height = 5)
image(bmalit[1:100],F) 
dev.off()

# plotModelsize(bmalit,exact=TRUE) 

# plot(bmalit)

pdf("/home/max/research/mission_replication/density_distmiss_without_popd.pdf", width = 8, height = 5)
density(bmalit)
dev.off()

# Include popd ------------------------------------------------------------

literacy_matrix <- cbind(
  literacy_data$illiteracy,
  literacy_data$distmiss,
  literacy_data$lati,
  literacy_data$longi,
  literacy_data$corr,
  literacy_data$ita,
  literacy_data$mis,
  literacy_data$mis1,
  literacy_data$area,
  literacy_data$tempe,
  literacy_data$alti,
  literacy_data$preci,
  literacy_data$rugg,
  literacy_data$river,
  literacy_data$coast,
  literacy_data$distfran,
  exp(literacy_data$distmiss * -1),
  exp(literacy_data$distfran * -1)
)

colnames(literacy_matrix) <- c(
  "illiteracy",
  "distmiss",
  "lati",
  "longi",
  "corr",
  "ita",
  "mis",
  "mis1",
  "area",
  "tempe",
  "alti",
  "preci",
  "rugg",
  "river",
  "coast",
  "distfran",
  "exp_distmiss",
  "exp_distfran"
)

literacy_df <- as.data.frame(literacy_matrix)



# Model -------------------------------------------------------------------

# Exclude the dependent variable
predictor_vars <- setdiff(names(literacy_df), "illiteracy")

# Create a mapping from variable names to beta indices
beta_indices <- setNames(seq_along(predictor_vars), predictor_vars)

# Get indices for the variables
index_beta_distmiss <- beta_indices["distmiss"]
index_beta_exp_distmiss <- beta_indices["exp_distmiss"]
index_beta_distfran <- beta_indices["distfran"]
index_beta_exp_distfran <- beta_indices["exp_distfran"]

custom_model_prior <- function(beta, n.vars, ...) {
  # beta is a binary vector of length n.vars
  
  # Get inclusion status for each variable
  beta_distmiss <- beta[index_beta_distmiss]
  beta_exp_distmiss <- beta[index_beta_exp_distmiss]
  beta_distfran <- beta[index_beta_distfran]
  beta_exp_distfran <- beta[index_beta_exp_distfran]
  
  # Exclude models with both distmiss and exp_distmiss
  if (beta_distmiss == 1 && beta_exp_distmiss == 1) {
    return(0)
  }
  
  # Exclude models with both distfran and exp_distfran
  if (beta_distfran == 1 && beta_exp_distfran == 1) {
    return(0)
  }
  
  # Assign equal prior probability to other models
  return(1)
}

# Load the BAS package
library(BAS)

# Construct the formula
formula_str <- paste("illiteracy ~", paste(predictor_vars, collapse = " + "))
formula <- as.formula(formula_str)

# Run Bayesian Model Averaging with the custom prior
bas_fit <- bas.lm(
  formula = formula,
  data = literacy_df,
  modelprior = custom_model_prior,
  method = "BAS"
)

uniform()

