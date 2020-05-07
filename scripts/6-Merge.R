### Combine outputs ====


### Packages ----
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)


### Input ----
sri <- readRDS('output/4-sri.RDS')
hro <- readRDS('output/5-hro.Rds')
body <- fread('input/body.csv')


### Body size ----
body2 <- body[, lapply(.SD, mean, na.rm = T), 
              by = ANIMAL_ID, .SDcols = -"date"]


# Calculate difference in size between all individuals
d <- as.matrix(dist(body2$total_length))
d[lower.tri(d, diag = TRUE)] <- NA
dimnames(d) <- list(body2$ANIMAL_ID, body2$ANIMAL_ID)

lendiff <- data.table(
  ID1 = rep(colnames(d), each = nrow(d)),
  ID2 = rep(rownames(d), ncol(d)),
  diff = as.vector(d)
)[!is.na(diff)]

dyad_id(lendiff, 'ID1', 'ID2')

# Check
lendiff[sample(1), diff] == body2[ANIMAL_ID %in% c(lendiff[1, ID1], lendiff[1, ID2]), dist(total_length)]


### Merge
lsDTs <- c(sri, hro, lendiff)
Reduce(function(x, y) merge(x, y, by = 'dyadID'), lsDTs)
sri_hro <- cbind(sri, hro[,c("Year", "ID1", "ID2", "dyad") := NULL])

DT <- merge(sri_hro, mat[,c("ID1", "ID2") := NULL], by = "dyad")

saveRDS(DT, "output/6-all-dyad-data.RDS")
fwrite(DT, "output/6-all-dyad-data.csv")

ggplot(DT) +
  geom_jitter(aes(sri,udoi, color = factor(Year))) 
