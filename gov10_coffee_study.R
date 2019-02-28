# Gov 10 Coffee Study
# Thomas Monfre, Karla Rosas, Josh Calianos, Jack Richardson

dta = read.csv('~/Documents/School/Dartmouth/Sophomore Year/19W Courses/Gov 10/project/data.csv', header=TRUE)
alpha = 0.05
z_star = abs(qnorm(alpha/2))

# assess total correctness
num_correct = length(dta$Correct_Guess[dta$Correct_Guess == 1])
total = length(dta$Subject_Number)
group_accuracy = num_correct / total   # OVERALL PERCENT CORRECT

# separate kaf first vs kaf last
kaf_first = dta[dta$KAF_First == 1, ]
kaf_last = dta[dta$KAF_First == 0, ]

# define sample sizes and counts
sample_sizes = c(length(kaf_first$Subject_Number), length(kaf_last$Subject_Number))
counts = c(length(kaf_first$Correct_Guess[kaf_first$Correct_Guess == 1]), length(kaf_last$Correct_Guess[kaf_last$Correct_Guess == 1]))

# get accuracy of each group and average cups per day
accuracies = c(counts[1]/sample_sizes[1], counts[2]/sample_sizes[2])
avg_cpds = c(mean(kaf_first$Avg_Cups_Per_Day), mean(kaf_last$Avg_Cups_Per_Day))

# get distribution of class years for each group
for (group in unique(kaf_first$Class_Year)) {
    print(group)
    print(paste("KAF First: ", length(kaf_first$Class_Year[kaf_first$Class_Year == group])))
    print(paste("KAF Last: ", length(kaf_last$Class_Year[kaf_last$Class_Year == group])))
    print("--------------------")
}

# get distribution of preferred coffee location for each group
for (group in unique(kaf_first$Preferred_Location_Summarized)) {
  print(group)
  print(paste("KAF First: ", length(kaf_first$Preferred_Location_Summarized[kaf_first$Preferred_Location_Summarized == group])))
  print(paste("KAF Last: ", length(kaf_last$Preferred_Location_Summarized[kaf_last$Preferred_Location_Summarized == group])))
  print("--------------------")
}


# PERFORM A TWO SAMPLE PROPORTION TEST

# compute confidence interval 
point_estimate = accuracies[1] - accuracies[2]
margin_of_error = z_star * sqrt( ((accuracies[1]*(1-accuracies[1]))/sample_sizes[1]) + ((accuracies[2]*(1-accuracies[2]))/sample_sizes[2]) )
paste(point_estimate, "±", margin_of_error)

# compute test-statistic and p-value
pnm = (counts[1] + counts[2])/(sample_sizes[1] + sample_sizes[2])
test_statistic = (accuracies[1] - accuracies[2]) / sqrt(pnm*(1-pnm)*((1/sample_sizes[1])+(1/sample_sizes[2])))
p_value = 2*pnorm(-1*test_statistic)

# plot test statistic on standard normal curve
curve(dnorm(x), xlim=c(-5,5), main='Sampling Distribution of Test Statistic', xlab="", ylab="", xaxt = "n") 
axis(side = 1, at = c(round(-1*z_star,digits=2), round(z_star,digits=2)), labels=c(round(-1*z_star,digits=2), round(z_star,digits=2)))

# add rejection region to left
cord.x <- c(-5,seq(-5,-1*z_star,0.01),-1*z_star) 
cord.y <- c(0,dnorm(seq(-5,-1*z_star,0.01)),0) 
polygon(cord.x,cord.y,col='lightgrey')

# add rejection region to right
cord.x <- c(z_star,seq(z_star,5,0.01),5) 
cord.y <- c(0,dnorm(seq(z_star,5,0.01)),0) 
polygon(cord.x,cord.y,col='lightgrey')

# add legend and vertical line for test statistic
abline(v = test_statistic, col="red", lwd=3, lty=2)
legend(-5.1,0.4, legend=c("test statistic", "rejection region"), col=c("red", "grey"), lwd=3, lty=2:1)

