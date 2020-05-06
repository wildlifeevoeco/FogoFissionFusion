

library(data.table)

sri <- readRDS('output/4-sri.RDS')
hr <- readRDS('output/5-hro.Rds')
body <- fread('input/body.csv')

body2 <- body[,c("date") := NULL][, lapply(.SD, mean), by=ANIMAL_ID]
body2

