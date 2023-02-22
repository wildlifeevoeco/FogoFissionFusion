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
habitat <- code_to_function('scripts/02-Habitat.R') 
sri <- code_to_function('scripts/03-SRI.R') 
nn <- code_to_function('scripts/04-NN.R') 
hr <- code_to_function('scripts/05-Home-Range-Overlap.R') 
body <- code_to_function('scripts/06-Body-Size.R') 
dyad <- code_to_function('scripts/07-Dyads.R') 
survival <- code_to_function('scripts/08-Survival.R') 
merge <- code_to_function('scripts/09-Merge.R')
glmm <- code_to_function('scripts/10-GLMM.R') 
cox <- code_to_function('scripts/11-Cox-model.R') 
a_glmm <- code_to_function('scripts/A - GLMM.R') 
b_cox <- code_to_function('scripts/B- Cox model.R')


