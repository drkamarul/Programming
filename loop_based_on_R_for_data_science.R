data1 <- mtcars
typeof(data1)
data1[1]
data1[[1]]
data1[c(1,2)]
data1[[c(1,2)]]

#calculate means for all columns

output <- vector('double', ncol(data1))
output
output[1]
output[[1]]

for (i in seq_along(data1)) {
  output[[i]] <- mean(data1[[i]]) 
}

output

# improve the codes

output2 <- vector('list', ncol(data1))
output2
output2[1]
output2[[1]]

names(output2) <- names(data1)
output2

for (i in seq_along(data1)) {
  output2[[i]] <- mean(data1[[i]]) 
}

output2

# lets make plots

output3 <- vector('list', ncol(data1))
output3
output3[1]
output3[[1]]

names(output3) <- names(data1)
output3

for (i in seq_along(data1)) {
  output3[[i]] <- hist(data1[[i]] , main = paste0(names(data1)[i])) 
}

output3
output3[3]
output3[[3]]

