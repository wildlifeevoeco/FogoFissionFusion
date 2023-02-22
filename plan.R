# === drake plan ----------------------------------------------------------
# Alec L. Robitaille



# Packages ----------------------------------------------------------------
source('packages.R')



# Data --------------------------------------------------------------------
# TODO: check if exists




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

