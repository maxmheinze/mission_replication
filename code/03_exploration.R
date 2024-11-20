
# Header ------------------------------------------------------------------

pacman::p_load(
  dplyr,
  magrittr,
  fixest,
  BMS
)

data_path <- "./data/source/Tables"


# Read in Data ------------------------------------------------------------

literacy_data <- read_dta(file.path(data_path, "Literacy Argentina Brazil Paraguay.dta"))

literacy_data <- literacy_data %>%
  mutate(mesorregi = as.factor(mesorregi))



# Re-Run the Regression from the Paper ------------------------------------

# Full sample
model_full_1 <- feols(illiteracy ~ distmiss + lati + longi + corr + ita + mis + mis1, data = literacy_data)
model_full_2 <- feols(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr + ita + mis + mis1, data = literacy_data)

# Brazil
brazil_data <- literacy_data %>% filter(country == "BRA")
model_brazil_1 <- feols(illiteracy ~ distmiss + lati + longi + mesorregi, data = brazil_data)
model_brazil_2 <- feols(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + mesorregi, data = brazil_data)

# Argentina
argentina_data <- literacy_data %>% filter(country == "Argentina")
model_argentina_1 <- feols(illiteracy ~ distmiss + lati + longi + corr, data = argentina_data)
model_argentina_2 <- feols(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr, data = argentina_data)

# Paraguay
paraguay_data <- literacy_data %>% filter(country == "Paraguay")
model_paraguay_1 <- feols(illiteracy ~ distmiss + ita, data = paraguay_data)
model_paraguay_2 <- feols(illiteracy ~ distmiss + area + tempe + alti + preci + rugg + river + coast + ita, data = paraguay_data)


# Display outputs
etable(
  model_full_1, 
  model_full_2, 
  model_brazil_1, 
  model_brazil_2, 
  model_argentina_1, 
  model_argentina_2, 
  model_paraguay_1, 
  model_paraguay_2,
  se = "stata"
)

# note that brazil coefficients can be exactly replicated if mesorregi is treated as dbl,
# they are slightly different when treated as fct




# Slap a BMA on it --------------------------------------------------------

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
  literacy_data$popd
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
  "distfran",
  "popd"
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


summary(bmalit)

coef(bmalit)

beta.draws.bma(bmalit[1:3])

image(bmalit[1:100],FALSE) 

plotModelsize(bmalit,exact=TRUE) 

plot(bmalit)

density(bmalit)


