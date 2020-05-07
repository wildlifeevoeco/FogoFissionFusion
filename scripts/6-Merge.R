### Combine outputs ====


### Packages ----
libs <- c('data.table')


### Input ----
sri <- readRDS('output/4-sri.RDS')
hro <- readRDS('output/5-hro.Rds')
body <- fread('input/body.csv')


### Body size ----
body2 <- body[, lapply(.SD, mean, na.rm = T), 
              by = ANIMAL_ID, .SDcols = -"date"]


# Generate delta length
d <- as.matrix(dist(body2$total_length))
d[lower.tri(d, diag = TRUE)] <- NA
dimnames(d) <- list(body2$ANIMAL_ID, body2$ANIMAL_ID)

lendiff <- data.table(
  ID1 = rep(colnames(d), each = nrow(d)),
  ID2 = rep(rownames(d), ncol(d)),
  diff = as.vector(d)
)

dyad_id(lendiff, 'ID1', 'ID2')

mymat_len <- matrix(data = len_matrix, 
                    nrow = length(len_matrix), 
                    ncol = length(len_matrix))


# calculate difference in size between all individuals
for(i in 1:31){
  for(j in 1:31){
    mymat_len[i,j] <- len_matrix[i] - len_matrix[j]
  }}
spatsoc::group_pts

body_matrix <- as.matrix(abs(mymat_len))
row.names(body_matrix) <- body2$ANIMAL_ID
colnames(body_matrix) <- body2$ANIMAL_ID

mat <- data.table(data.table::melt(body_matrix))
colnames(mat) <- c("ID1", "ID2", "delta_length")

## delete rows comparing individuals to themselves
mat[, diff := (ID1==ID2)]
mat <- mat[diff != TRUE][,c("diff") := NULL]

mat$dyad <- as.factor(paste(mat$ID1, mat$ID2, sep = "_"))

### merge 
sri_hro <- cbind(sri, hro[,c("Year", "ID1", "ID2", "dyad") := NULL])

DT <- merge(sri_hro, mat[,c("ID1", "ID2") := NULL], by = "dyad")

saveRDS(DT, "output/6-all-dyad-data.RDS")
fwrite(DT, "output/6-all-dyad-data.csv")

ggplot(DT) +
  geom_jitter(aes(sri,udoi, color = factor(Year))) 
