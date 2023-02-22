# === drake plan ----------------------------------------------------------
# Alec L. Robitaille



# Packages ----------------------------------------------------------------
source('packages.R')



# Variables ---------------------------------------------------------------
source('variables.R')



# "Functions" -------------------------------------------------------------
source('functions/get_sri.R')
source('functions/hr_network.R')
source('functions/diff_dyad.R')


# This is a shim to use drake with numbered scripts... 
# In the future, use targets. 
prep_locs <- code_to_function('scripts/01-Prep-Locs.R') 
make_habitat <- code_to_function('scripts/02-Habitat.R') 
calc_sri <- code_to_function('scripts/03-SRI.R') 
calc_nn <- code_to_function('scripts/04-NN.R') 
calc_hro <- code_to_function('scripts/05-Home-Range-Overlap.R') 
calc_body <- code_to_function('scripts/06-Body-Size.R') 
calc_dyads <- code_to_function('scripts/07-Dyads.R') 
calc_survival <- code_to_function('scripts/08-Survival.R') 
merge_data <- code_to_function('scripts/09-Merge.R')
model_glmm <- code_to_function('scripts/10-GLMM.R') 
model_cox <- code_to_function('scripts/11-Cox-model.R') 


# Check if data exists
check_exists <- function(path) stopifnot(file.exists(path))


# Data --------------------------------------------------------------------
check_exists(fogo_path)

check_exists(lc_path)
check_exists(legend_path)

check_exists(body_path)



# Plan --------------------------------------------------------------------
plan <- drake_plan(
  locs = prep_locs(), 
  habitat = make_habitat(locs),
  sri = calc_sri(locs),
  nn = calc_nn(sri),
  hro = calc_hro(locs),
  body = calc_body(),
  dyads = calc_dyads(nn, habitat),
  survival = calc_survival(dyads),
  merge = merge_data(sri, hro, body, dyads),
  glmm = model_glmm(merge),
  cox = model_cox(merge, survival)
)


drake_config(plan)