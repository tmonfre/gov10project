random_values <- vector()

for (i in c(1:50)) {
  choice = sample(0:1)
  random_values = c(random_values, choice)
}

write.csv(random_values, file = "~/Desktop/output.csv")