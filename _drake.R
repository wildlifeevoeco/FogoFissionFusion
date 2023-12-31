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

write_r_to_rmd <- function(r_file) {
  rmd_file <- gsub('scripts', 'md', with_ext(r_file, 'Rmd'))
  file.copy(r_file, rmd_file, overwrite = TRUE)
  
  writeLines(
    c(
      paste0('# ', Sys.Date()),
      '',
      '```{r}',
      'knitr::opts_chunk$set(dev = "png")',
      'knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())',
      '```', '',
      '```{r}',
      readLines(rmd_file),
      '```'),
    rmd_file
  )
}


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
  sri = calc_sri(habitat),
  nn = calc_nn(sri),
  hro = calc_hro(locs),
  body = calc_body(),
  dyads = calc_dyads(nn, habitat),
  survival = calc_survival(dyads),
  merge = merge_data(sri, hro, body, dyads),
  glmm = model_glmm(merge),
  cox = model_cox(merge, survival),
  glmm_render = {
    glmm;
    write_r_to_rmd('scripts/10-GLMM.R'); 
    render('md/10-GLMM.Rmd', 'pdf_document', clean = FALSE)
    unlink('md/10-GLMM.html')
  },
  cox_render = {
    cox;
    write_r_to_rmd('scripts/11-Cox-model.R'); 
    render('md/11-Cox-model.Rmd', 'pdf_document', clean = FALSE)
    unlink('md/11-Cox-model.html')
  }
  
)


drake_config(plan)