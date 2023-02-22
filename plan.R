# === drake plan ----------------------------------------------------------
# Alec L. Robitaille



# Packages ----------------------------------------------------------------
source('packages.R')



# "Functions" -------------------------------------------------------------
# This is a shim to use drake with numbered scripts... 
# In the future, use targets. 
prep_locs <- code_to_function('scripts/01-Prep-Locs.R') 
make_habitat <- code_to_function('scripts/02-Habitat.R') 
calc_sri <- code_to_function('scripts/03-SRI.R') 
calc_nn <- code_to_function('scripts/04-NN.R') 
calc_hr <- code_to_function('scripts/05-Home-Range-Overlap.R') 
calc_body <- code_to_function('scripts/06-Body-Size.R') 
calc_dyads <- code_to_function('scripts/07-Dyads.R') 
calc_survival <- code_to_function('scripts/08-Survival.R') 
merge_data <- code_to_function('scripts/09-Merge.R')
model_glmm <- code_to_function('scripts/10-GLMM.R') 
model_cox <- code_to_function('scripts/11-Cox-model.R') 
model_a_glmm <- code_to_function('scripts/A - GLMM.R') 
model_b_cox <- code_to_function('scripts/B- Cox model.R')


# Check if data exists
check_exists <- function(path) stopifnot(file.exists(path))



# Data --------------------------------------------------------------------
fogo_path <- '../prepare-locs/output/NL-Fogo-Caribou-Telemetry.csv'
check_exists(fogo_path)

lc_path <- '../nl-landcover/output/fogo_lc.tif'
legend_path <- '../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv'
check_exists(lc_path)
check_exists(legend_path)

body_path <- 'input/body.csv'
check_exists(body_path)



# Plan --------------------------------------------------------------------
plan <- drake_plan(
  locs = prep_locs(), 
  hab = make_habitat(),
  sri = calc_sri(),
  nn = calc_nn(),
  hr = calc_hr(),
  body = calc_body(),
  dyad = calc_dyads(),
  survival = calc_survival(),
  merge = merge_data(),
  glmm = model_glmm(),
  cox = model_cox(),
  a_glmm = model_a_glmm(),
  b_cox = model_b_cox()
)

