

library(data.table)

sri <- readRDS('output/4-sri.RDS')
hr <- readRDS('output/5-hro.Rds')
body <- fread('input/body.csv')

body2 <- body[,c("date") := NULL][, lapply(.SD, mean, na.rm = T), by=ANIMAL_ID]


## generate delta length
len_matrix <- c(body2$total_length)
mymat_len <- matrix(data= len_matrix,nrow=31, ncol=31) 


# calculate difference in size between all individuals
for(i in 1:31){
  for(j in 1:31){
    mymat_len[i,j] <- len_matrix[i] - len_matrix[j]
  }}

body_matrix <- as.matrix(abs(mymat_len))
row.names(body_matrix) <- body2$ANIMAL_ID
colnames(body_matrix) <- body2$ANIMAL_ID

mat <- data.table(data.table::melt(body_matrix))
colnames(mat) <- c("ID1", "ID2", "delta_length")

## delete rows comparing individuals to themselves
mat[, diff := (ID1==ID2)]
mat <- mat[diff != TRUE][,c("diff") := NULL]




