

# Header ------------------------------------------------------------------

# Load necessary libraries
pacman::p_load(
  tidyverse,
  fixest,
  haven,
  modelsummary
)


# Preliminaries -----------------------------------------------------------

# Set data path
data_path <- "./data/source/Tables/"

# Function for tabular summaries
tabular_summary <- function(data, vars, by_var) {
  data %>%
    group_by(!!sym(by_var)) %>%
    summarise(across(all_of(vars),
                     list(
                       n = function(x) sum(!is.na(x)),
                       mean = function(x) mean(x, na.rm = TRUE),
                       median = function(x) median(x, na.rm = TRUE),
                       sd = function(x) sd(x, na.rm = TRUE),
                       min = function(x) min(x, na.rm = TRUE),
                       max = function(x) max(x, na.rm = TRUE)
                     ),
                     .names = "{col}_{fn}"
    ))
}


# Replication Package Translated ------------------------------------------

# **************************************
# Replication files for "The Mission"
# Felipe Valencia Caicedo (2018)
# TABLES
# translated to R
# **************************************

# Table I

# Literacy
literacy_data <- read_dta(file.path(data_path, "Literacy Argentina Brazil Paraguay.dta"))
literacy_summary <- tabular_summary(literacy_data, vars = c("literacy", "illiteracy"), by_var = "distmiss50")
print(literacy_summary)

# Median Years of Schooling
schooling_data <- read_dta(file.path(data_path, "Median Years of Schooling Brazil.dta"))
schooling_summary <- tabular_summary(schooling_data, vars = c("edumedyr"), by_var = "distmiss50")
print(schooling_summary)

# Income
income_data <- read_dta(file.path(data_path, "Income Brazil Paraguay.dta"))
income_summary <- tabular_summary(income_data, vars = c("lnincome"), by_var = "distmiss50")
print(income_summary)

# Poverty
poverty_data <- read_dta(file.path(data_path, "Poverty Argentina Paraguay.dta"))
poverty_summary <- tabular_summary(poverty_data, vars = c("pnbiper"), by_var = "missionary")
print(poverty_summary)

# Mission / Geographic Controls
geo_data <- read_dta(file.path(data_path, "Literacy Argentina Brazil Paraguay.dta"))
geo_summary <- geo_data %>%
  filter(!is.na(lati)) %>%
  tabular_summary(
    vars = c("distmiss", "lati", "longi", "area", "tempe", "alti", "preci", "rugg", "slope", "river", "coast", "landlocked"),
    by_var = "distmiss50"
  )
print(geo_summary)

# Table II

# Literacy Data
literacy_data <- read_dta(file.path(data_path, "Literacy Argentina Brazil Paraguay.dta"))

# Full Sample Regressions
model_full_1 <- feols(illiteracy ~ distmiss + lati + longi + corr + ita + mis + mis1, data = literacy_data)
model_full_2 <- feols(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr + ita + mis + mis1, data = literacy_data)

# Display outputs
etable(model_full_1, model_full_2)

# Brazil-specific Regressions
brazil_data <- literacy_data %>% filter(country == "BRA")
model_brazil_1 <- feols(illiteracy ~ distmiss + lati + longi + mesorregi, data = brazil_data)
model_brazil_2 <- feols(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + mesorregi, data = brazil_data)

etable(model_brazil_1, model_brazil_2)

# Argentina-specific Regressions
argentina_data <- literacy_data %>% filter(country == "Argentina")
model_argentina_1 <- feols(illiteracy ~ distmiss + lati + longi + corr, data = argentina_data)
model_argentina_2 <- feols(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr, data = argentina_data)

etable(model_argentina_1, model_argentina_2)

# Paraguay-specific Regressions
paraguay_data <- literacy_data %>% filter(country == "Paraguay")
model_paraguay_1 <- feols(illiteracy ~ distmiss + ita, data = paraguay_data)
model_paraguay_2 <- feols(illiteracy ~ distmiss + area + tempe + alti + preci + rugg + river + coast + ita, data = paraguay_data)

etable(model_paraguay_1, model_paraguay_2)

# Table III

# Median Years of Schooling
schooling_data <- read_dta(file.path(data_path, "Median Years of Schooling Brazil.dta"))
model_schooling_1 <- feols(edumedyr ~ distmiss + mesorregi + lat1 + long1, data = schooling_data)
model_schooling_2 <- feols(edumedyr ~ distmiss + lat1 + long1 + area + river + slope + rugg + alt + tempe + rain + coast + mesorregi, data = schooling_data)

etable(model_schooling_1, model_schooling_2)

# Ln Income
income_data <- read_dta(file.path(data_path, "Income Brazil Paraguay.dta"))
model_income_1 <- feols(lnincome ~ distmiss + lati + longi + par, data = income_data)
model_income_2 <- feols(lnincome ~ distmiss + lati + longi + area + river + slope + rugg + alti + tempe + preci + landlocked + coast + par, data = income_data)

etable(model_income_1, model_income_2)

# Poverty
poverty_data <- read_dta(file.path(data_path, "Poverty Argentina Paraguay.dta"))
model_poverty_1 <- feols(pnbiper ~ distmiss + lati + longi + par, data = poverty_data)
model_poverty_2 <- feols(pnbiper ~ distmiss + coast + river + alti + tempe + area + preci + lati + longi + par, data = poverty_data)

etable(model_poverty_1, model_poverty_2)

# Table IV

# Literacy Placebo Tests
placebo_data <- read_dta(file.path(data_path, "Placebo Literacy Income.dta"))

model_placebo_1 <- feols(illiteracy ~ distpar + distfran + lati + longi + area + alti + tempe + preci + river + rugg + coast + landlocked + ita + mis1 + corr + mis, data = placebo_data)
model_placebo_2 <- feols(illiteracy ~ distgua + distfran + lati + longi + area + alti + tempe + preci + river + rugg + coast + landlocked + ita + mis1 + corr + mis, data = placebo_data)
model_placebo_3 <- feols(illiteracy ~ distita + distfran + lati + longi + area + alti + tempe + preci + river + rugg + coast + landlocked + ita + mis1 + corr + mis, data = placebo_data)
model_placebo_4 <- feols(illiteracy ~ distpar + distgua + distita + lati + longi + par + arg, data = placebo_data)
model_placebo_5 <- feols(illiteracy ~ distpar + distgua + distita + distfran + lati + longi + area + alti + tempe + preci + river + rugg + coast + landlocked + ita + mis1 + corr + mis, data = placebo_data)
model_placebo_6 <- feols(illiteracy ~ distpar + distgua + distita + distmiss + lati + longi + mis1 + mis + corr + ita, data = placebo_data)
model_placebo_7 <- feols(illiteracy ~ distpar + distgua + distita + distfran + distmiss + lati + longi + area + alti + tempe + preci + river + rugg + coast + landlocked + ita + mis1 + corr + mis, data = placebo_data)

etable(model_placebo_1, model_placebo_2, model_placebo_3, model_placebo_4, model_placebo_5, model_placebo_6, model_placebo_7)

# Table V

# Median Years of Schooling Placebo Tests
schooling_data <- read_dta(file.path(data_path, "Median Years of Schooling Brazil.dta"))
model_schooling_placebo_1 <- feols(edumedyr ~ distita + distpar + distgua + distfran + long1 + area + temp + alt + rain + tordist1 + rugg + river + coast + mesorregi, data = schooling_data)
model_schooling_placebo_2 <- feols(edumedyr ~ distita + distpar + distgua + distfran + distmiss + long1 + area + temp + alt + rain + tordist1 + rugg + river + coast + mesorregi, data = schooling_data)

etable(model_schooling_placebo_1, model_schooling_placebo_2)

# Ln Income Placebo Tests
income_data <- read_dta(file.path(data_path, "Placebo Literacy Income.dta"))
model_income_placebo_1 <- feols(lnincome ~ distita + distpar + distgua + distfran + lati + longi + area + alti + tempe + preci + river + rugg + coast + slope + par, data = income_data)
model_income_placebo_2 <- feols(lnincome ~ distita + distpar + distgua + distmiss + distfran + lati + longi + area + alti + tempe + preci + river + rugg + coast + slope + par, data = income_data)

etable(model_income_placebo_1, model_income_placebo_2)

# Poverty Placebo Tests
poverty_data <- read_dta(file.path(data_path, "Poverty Argentina Paraguay.dta"))
model_poverty_placebo_1 <- feols(pnbiper ~ distpar + distgua + distita + distfran + coast + river + alti + tempe + area + preci + rugg + distord1 + longi + par, data = poverty_data)
model_poverty_placebo_2 <- feols(pnbiper ~ distpar + distgua + distita + distfran + distmiss + coast + river + alti + tempe + area + preci + rugg + distord1 + longi + par, data = poverty_data)

etable(model_poverty_placebo_1, model_poverty_placebo_2)

# Table VI

# Literacy Alternative Explanations
literacy_data <- read_dta(file.path(data_path, "Literacy Argentina Brazil Paraguay.dta"))

model_alt_1 <- feols(illiteracy ~ distfran + lati + longi + arg + par, data = literacy_data)
model_alt_2 <- feols(illiteracy ~ distfran + lati + longi + area + river + slope + rugg + alti + tempe + preci + landlocked + coast + par + arg, data = literacy_data)
model_alt_3 <- feols(illiteracy ~ distfran + distmiss + lati + longi + arg + par, data = literacy_data)
model_alt_4 <- feols(illiteracy ~ distfran + distmiss + lati + longi + area + river + slope + rugg + alti + tempe + preci + landlocked + coast + par + arg, data = literacy_data)

# Country-specific Regressions
# Brazil
brazil_data <- literacy_data %>% filter(country == "BRA")
model_brazil_alt <- feols(illiteracy ~ distfran + distmiss + lati + longi + area + tempe + alti + preci + rugg + river + slope + landlocked + coast + mesorregi, data = brazil_data)

# Argentina
argentina_data <- literacy_data %>% filter(country == "Argentina")
model_argentina_alt <- feols(illiteracy ~ distmiss + distfran + lati + longi + area + tempe + alti + preci + rugg + river + slope + coast + corr, data = argentina_data)

# Paraguay
paraguay_data <- literacy_data %>% filter(country == "Paraguay")
model_paraguay_alt <- feols(illiteracy ~ distfran + distmiss + area + river + slope + rugg + alti + tempe + preci + coast + ita, data = paraguay_data)

etable(model_alt_1, model_alt_2, model_alt_3, model_alt_4, model_brazil_alt, model_argentina_alt, model_paraguay_alt)

# Table VII

# Median Years of Schooling Alternative Explanations
schooling_data <- read_dta(file.path(data_path, "Median Years of Schooling Brazil.dta"))
model_schooling_alt_1 <- feols(edumedyr ~ distfran + lat1 + long1 + area + river + slope + rugg + alt + tempe + rain + coast + mesorregi, data = schooling_data)
model_schooling_alt_2 <- feols(edumedyr ~ distfran + distmiss + lat1 + long1 + area + river + slope + rugg + alt + tempe + rain + coast + mesorregi, data = schooling_data)

etable(model_schooling_alt_1, model_schooling_alt_2)

# Ln Income Alternative Explanations
income_data <- read_dta(file.path(data_path, "Income Brazil Paraguay.dta"))
model_income_alt_1 <- feols(lnincome ~ distfran + lati + longi + area + river + slope + rugg + alti + tempe + preci + landlocked + coast + par, data = income_data)
model_income_alt_2 <- feols(lnincome ~ distfran + distmiss + lati + longi + area + river + slope + rugg + alti + tempe + preci + landlocked + coast + par, data = income_data)

etable(model_income_alt_1, model_income_alt_2)

# Poverty Alternative Explanations
poverty_data <- read_dta(file.path(data_path, "Poverty Argentina Paraguay.dta"))
model_poverty_alt_1 <- feols(pnbiper ~ distfran + coast + river + alti + tempe + area + preci + rugg + lati + longi + ita + mis + corr, data = poverty_data)
model_poverty_alt_2 <- feols(pnbiper ~ distfran + distmiss + coast + river + alti + tempe + area + preci + rugg + lati + longi + ita + mis + corr, data = poverty_data)

etable(model_poverty_alt_1, model_poverty_alt_2)

# Table VIII: Historical Data Analysis

# Panel A: Argentina

# Argentina 1895 data
argentina_1895_data <- read_dta(file.path(data_path, "Argentina 1895.dta"))

model_arg1895_1 <- feols(argiliteracy ~ distmiss + distfran + lati + alti + tempe + area + river, data = argentina_1895_data)
model_arg1895_2 <- feols(argmailiteracy ~ distmiss + distfran + lati + alti + tempe + area + river, data = argentina_1895_data)
model_arg1895_3 <- feols(argfeiliteracy ~ distmiss + distfran + lati + alti + tempe + area + river, data = argentina_1895_data)
model_arg1895_4 <- feols(foriliteracy ~ distmiss + distfran + lati + alti + tempe + area + river, data = argentina_1895_data)

# IPUMS Argentina data
ipums_argentina <- read_dta(file.path(data_path, "IPUMS Argentina.dta"))

# Probit models for years 1970, 1980, 1991
ipums_argentina <- ipums_argentina %>% mutate(muni = factor(muni))

model_ipums_1970 <- feglm(illiterate ~ distmiss + distfran + lati + longi + area + coast + river + slope + rugg + alti + tempe + preci + corr,
                          family = binomial(link = "probit"), cluster = ~muni,
                          data = ipums_argentina %>% filter(year == 1970))

model_ipums_1980 <- feglm(illiterate ~ distmiss + distfran + lati + longi + area + coast + river + slope + rugg + alti + tempe + preci + corr,
                          family = binomial(link = "probit"), cluster = ~muni,
                          data = ipums_argentina %>% filter(year == 1980))

model_ipums_1991 <- feglm(illiterate ~ distmiss + distfran + lati + longi + area + coast + river + slope + rugg + alti + tempe + preci + corr,
                          family = binomial(link = "probit"), cluster = ~muni,
                          data = ipums_argentina %>% filter(year == 1991))

# Display outputs for Table VIII Panel A
etable(model_arg1895_1, model_arg1895_2, model_arg1895_3, model_arg1895_4,
       model_ipums_1970, model_ipums_1980, model_ipums_1991)

# Panel B: Brazil

# Brazil 1920 data
brazil_1920_data <- read_dta(file.path(data_path, "Brazil 1920.dta"))

model_brazil1920_1 <- feols(illit1920 ~ distmiss + distfran + lat1 + alt + temp + rain + area + long1, data = brazil_1920_data)
model_brazil1920_2 <- feols(braillit ~ distmiss + distfran + lat1 + alt + temp + rain + area + long1, data = brazil_1920_data)
model_brazil1920_3 <- feols(forillit ~ distmiss + distfran + lat1 + alt + temp + rain + area + long1, data = brazil_1920_data)

# IPUMS Brazil data
ipums_brazil <- read_dta(file.path(data_path, "IPUMS Brazil.dta"))

model_ipums_brazil_1980 <- feols(yrschool ~ distmiss + distfran + pa + lat1 + long1 + area + coast + river + slope + rugg + alti + tempe + preci + mesorregi,
                                 data = ipums_brazil %>% filter(year == 1980, yrschool < 90),
                                 cluster = ~muni)

model_ipums_brazil_1991 <- feols(yrschool ~ distmiss + distfran + pa + lat1 + long1 + area + coast + river + slope + rugg + alti + tempe + preci + mesorregi,
                                 data = ipums_brazil %>% filter(year == 1991, yrschool < 90),
                                 cluster = ~muni)

# Display outputs for Table VIII Panel B
etable(model_brazil1920_1, model_brazil1920_2, model_brazil1920_3,
       model_ipums_brazil_1980, model_ipums_brazil_1991)

# Panel C: Paraguay

# Paraguay 1950 data
paraguay_1950_data <- read_dta(file.path(data_path, "Paraguay 1950.dta"))

model_paraguay1950 <- feols(illitper1950 ~ distmiss + distfran + ita + area + coast + river + slope + rugg + alti + preci + lati + tempe, data = paraguay_1950_data)

# IPUMS Paraguay data
ipums_paraguay <- read_dta(file.path(data_path, "IPUMS Paraguay.dta"))

model_ipums_paraguay_1962 <- feglm(illiterate ~ distmiss + distfran + lati + longi + coast + river + slope + rugg + alti + tempe + preci,
                                   family = binomial(link = "probit"),
                                   data = ipums_paraguay %>% filter(year == 1962))

model_ipums_paraguay_1982 <- feglm(illiterate ~ distmiss + distfran + lati + longi + coast + river + slope + rugg + alti + tempe + preci,
                                   family = binomial(link = "probit"),
                                   data = ipums_paraguay %>% filter(year == 1982))

# Display outputs for Table VIII Panel C
etable(model_paraguay1950, model_ipums_paraguay_1962, model_ipums_paraguay_1982)

# Table IX: Structural Transformation

# Panel A: Simple Regressions

# Brazil data
structural_brazil <- read_dta(file.path(data_path, "Structural Transformation Brazil.dta"))

model_brazil_agro <- feols(agroper ~ distmiss + lat1 + long1 + mesorregi, data = structural_brazil)
model_brazil_indu <- feols(induper ~ distmiss + lat1 + long1 + mesorregi, data = structural_brazil)
model_brazil_comm <- feols(commper ~ distmiss + lat1 + long1 + mesorregi, data = structural_brazil)

# Argentina data
structural_argentina <- read_dta(file.path(data_path, "Structural Transformation Argentina.dta"))

model_argentina_agro <- feglm(agriculture ~ distmiss + lati + longi + corr,
                              family = binomial(link = "probit"), cluster = ~geolev2,
                              data = structural_argentina %>% filter(indgen != 0))

model_argentina_manufacturing <- feglm(manufacturing ~ distmiss + lati + longi + corr,
                                       family = binomial(link = "probit"), cluster = ~geolev2,
                                       data = structural_argentina %>% filter(indgen != 0))

model_argentina_commerce <- feglm(commerce ~ distmiss + lati + longi + corr,
                                  family = binomial(link = "probit"), cluster = ~geolev2,
                                  data = structural_argentina %>% filter(indgen != 0))

# Paraguay data
structural_paraguay <- read_dta(file.path(data_path, "Structural Transformation Paraguay.dta"))

model_paraguay_agro <- feglm(agro ~ distmiss + itapua,
                             family = binomial(link = "probit"), cluster = ~district,
                             data = structural_paraguay)
model_paraguay_manu <- feglm(manu ~ distmiss + itapua,
                             family = binomial(link = "probit"), cluster = ~district,
                             data = structural_paraguay)
model_paraguay_comm <- feglm(comm ~ distmiss + itapua,
                             family = binomial(link = "probit"), cluster = ~district,
                             data = structural_paraguay)

# Display outputs for Table IX Panel A
etable(model_brazil_agro, model_brazil_indu, model_brazil_comm,
       model_argentina_agro, model_argentina_manufacturing, model_argentina_commerce,
       model_paraguay_agro, model_paraguay_manu, model_paraguay_comm)

# Panel B: Full Regressions

# Brazil data with additional controls
model_brazil_agro_full <- feols(agroper ~ distmiss + distfran + lat1 + long1 + area + temp + alt + rain +
                                  rugg + river + coast + slope + mesorregi, data = structural_brazil)
model_brazil_indu_full <- feols(induper ~ distmiss + distfran + lat1 + long1 + area + temp + alt + rain +
                                  rugg + river + coast + slope + mesorregi, data = structural_brazil)
model_brazil_comm_full <- feols(commper ~ distmiss + distfran + lat1 + long1 + area + temp + alt + rain +
                                  rugg + river + coast + slope + mesorregi, data = structural_brazil)

# Argentina data with additional controls
model_argentina_agro_full <- feglm(agriculture ~ distmiss + distfran + lati + longi + area + tempe +
                                     preci + coast + river + slope + alti + rugg + corr,
                                   family = binomial(link = "probit"), cluster = ~geolev2,
                                   data = structural_argentina %>% filter(indgen != 0))

model_argentina_manufacturing_full <- feglm(manufacturing ~ distmiss + distfran + lati + longi + area +
                                              tempe + preci + coast + river + slope + alti + rugg + corr,
                                            family = binomial(link = "probit"), cluster = ~geolev2,
                                            data = structural_argentina %>% filter(indgen != 0))

model_argentina_commerce_full <- feglm(commerce ~ distmiss + distfran + lati + longi + area + tempe +
                                         preci + coast + river + slope + alti + rugg + corr,
                                       family = binomial(link = "probit"), cluster = ~geolev2,
                                       data = structural_argentina %>% filter(indgen != 0))

# Paraguay data with additional controls
model_paraguay_agro_full <- feglm(agro ~ distmiss + distfran + itapua + area + coast + river + slope +
                                    rugg + alti + tempe + preci,
                                  family = binomial(link = "probit"), cluster = ~district,
                                  data = structural_paraguay)

model_paraguay_manu_full <- feglm(manu ~ distmiss + distfran + itapua + area + coast + river + slope +
                                    rugg + alti + tempe + preci,
                                  family = binomial(link = "probit"), cluster = ~district,
                                  data = structural_paraguay)

model_paraguay_comm_full <- feglm(comm ~ distmiss + distfran + itapua + area + coast + river + slope +
                                    rugg + alti + tempe + preci,
                                  family = binomial(link = "probit"), cluster = ~district,
                                  data = structural_paraguay)

# Display outputs for Table IX Panel B
etable(model_brazil_agro_full, model_brazil_indu_full, model_brazil_comm_full,
       model_argentina_agro_full, model_argentina_manufacturing_full, model_argentina_commerce_full,
       model_paraguay_agro_full, model_paraguay_manu_full, model_paraguay_comm_full)

# Table X: Industry Analysis in Brazil

# Industries Brazil data
industries_brazil <- read_dta(file.path(data_path, "Industries Brazil.dta"))

industries_brazil <- industries_brazil %>% filter(occ != 9999, year == 2010)

industry_vars <- c("foundries", "tobacco", "nfmetals", "metalp", "plastic", "beverages",
                   "transport", "machinee", "chemicals", "chemicalo")

models_industries <- lapply(industry_vars, function(industry) {
  feglm(as.formula(paste0(industry, " ~ distmiss + distfran + lat1 + long1 + pa + area + temp + rain + coast + river + slope + alti + rugg + meso")),
        family = binomial(link = "probit"), data = industries_brazil)
})

# Display outputs for Table X
etable(models_industries)

# Table XI: Soy Production in Brazil

# Soy Brazil data
soy_brazil <- read_dta(file.path(data_path, "Soy Brazil.dta"))

models_soy <- list(
  feols(soy_TA ~ distmiss + distfran + time + latitude + longitude + micro + dA_soy, data = soy_brazil),
  feols(dgsoy_TA ~ distmiss + distfran + dA_soy + latitude + longitude + micro, data = soy_brazil),
  feols(dnsoy_TA ~ distmiss + distfran + dA_soy + latitude + longitude + micro, data = soy_brazil),
  feols(log_PQ_LA ~ distmiss + distfran + dA_soy + latitude + longitude + micro, data = soy_brazil),
  feols(La_L ~ distmiss + distfran + time + latitude + longitude + micro, data = soy_brazil),
  feols(Lm_L ~ distmiss + distfran + time + latitude + longitude + micro, data = soy_brazil),
  feols(Ls_L ~ distmiss + distfran + time + latitude + longitude + micro, data = soy_brazil)
)

# Display outputs for Table XI
etable(models_soy)

# Table XII: Population Density and Cultural Outcomes

# Panel A: Population Density and Roads

# Load data
literacy_data <- read_dta(file.path(data_path, "Literacy Argentina Brazil Paraguay.dta"))

model_popd <- feols(popd ~ distmiss + distfran + area + river + slope + rugg + alti +
                      tempe + preci + landlocked + coast + arg + par + lati + longi, data = literacy_data)

# Pre-Colonial Population Density data
precolonial_data <- read_dta(file.path(data_path, "Pre-Colonial Population Density.dta"))

model_precolonial <- feols(popd ~ mission + temp_avg + rainfall + alti + landlocked +
                             altisq + tempsq + rainsq + factor(countrynum), data = precolonial_data)

# Roads and Railroads
model_roads <- feols(roads ~ distmiss + distfran + lati + longi + area + tempe + alti +
                       preci + rugg + slope + river + coast + corr + ita + mis + mis1, data = literacy_data)
model_rail <- feols(rail ~ distmiss + distfran + lati + longi + area + tempe + alti +
                      preci + rugg + slope + river + coast + corr + ita + mis + mis1, data = literacy_data)

# Display outputs for Table XII Panel A
etable(model_popd, model_precolonial, model_roads, model_rail)

# Panel B: Cultural Outcomes

# IFDM Index
schooling_data <- read_dta(file.path(data_path, "Median Years of Schooling Brazil.dta"))
model_ifdm <- feols(ifdmhea100 ~ distmiss + distfran + area + river + slope + rugg + alti +
                      tempe + preci + landlocked + mesorregi + coast + lat1 + long1, data = schooling_data)

# Tourism in Brazil
culture_brazil <- read_dta(file.path(data_path, "Culture Brazil.dta"))
model_tourism <- feols(turismo ~ distmiss + distfran + alt + area + temp + rain + mesorregi +
                         lat1 + long1, data = culture_brazil)

# Culture in Paraguay
culture_paraguay <- read_dta(file.path(data_path, "Culture Paraguay.dta"))

model_museum <- feols(museum ~ distmiss + distfran + itapua + area + coast + river +
                        slope + rugg + alti + tempe + preci, data = culture_paraguay)
model_visited <- feols(visited ~ distmiss + distfran + itapua + area + coast + river +
                         slope + rugg + alti + tempe + preci + lati + longi, data = culture_paraguay)

# Resident vs. Non-Resident in Brazil
model_resident <- feols(edumedyr ~ distmiss + area + river + slope + rugg + alti + tempe +
                          preci + landlocked + coast + mesorregi, data = schooling_data %>% filter(residente == 1))
model_nonresident <- feols(edumedyr ~ distmiss + area + river + slope + rugg + alti + tempe +
                             preci + landlocked + coast + mesorregi, data = schooling_data %>% filter(residente != 1))

# Display outputs for Table XII Panel B
etable(model_ifdm, model_tourism, model_museum, model_visited, model_resident, model_nonresident)

