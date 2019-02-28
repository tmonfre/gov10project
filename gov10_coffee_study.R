# Gov 10 Coffee Study
# Thomas Monfre, Karla Rosas, Josh Calianos, Jack Richardson

dta = read.csv('~/Documents/School/Dartmouth/Sophomore Year/19W Courses/Gov 10/project/data.csv', header=TRUE)
alpha = 0.05
z_star = abs(qnorm(alpha/2))

# assess total correctness
num_correct = length(dta$Correct_Guess[dta$Correct_Guess == 1])
total = length(dta$Subject_Number)
num_correct / total   # OVERALL PERCENT CORRECT

# separate kaf first vs kaf last
kaf_first = dta[dta$KAF_First == 1, ]
kaf_last = dta[dta$KAF_First == 0, ]

kaf_first_correct = kaf_first[kaf_first$Correct_Guess == 1, ]
kaf_first_incorrect = kaf_first[kaf_first$Correct_Guess == 0, ]

kaf_last_correct = kaf_last[kaf_last$Correct_Guess == 1, ]
kaf_last_incorrect = kaf_last[kaf_last$Correct_Guess == 0, ]

# define n and m as the number of subjects in each group
n = length(kaf_first$Subject_Number)
m = length(kaf_last$Subject_Number)

# get accuracy of each group
kaf_first_accuracy = length(kaf_first$Correct_Guess[kaf_first$Correct_Guess == 1]) / n
kaf_last_accuracy = length(kaf_last$Correct_Guess[kaf_last$Correct_Guess == 1]) / m

# get average cups per day
kaf_first_avg_cpd = mean(kaf_first$Avg_Cups_Per_Day)
kaf_last_avg_cpd = mean(kaf_last$Avg_Cups_Per_Day)

# get average class year for each group
for (group in unique(kaf_first$Class_Year)) {
    print(group)
    print(paste("KAF First: ", length(kaf_first$Class_Year[kaf_first$Class_Year == group])))
    print(paste("KAF Last: ", length(kaf_last$Class_Year[kaf_last$Class_Year == group])))
    print("--------------------")
}

# get average class year for each group
for (group in unique(kaf_first$Preferred_Location_Summarized)) {
  print(group)
  print(paste("KAF First: ", length(kaf_first$Preferred_Location_Summarized[kaf_first$Preferred_Location_Summarized == group])))
  print(paste("KAF Last: ", length(kaf_last$Preferred_Location_Summarized[kaf_last$Preferred_Location_Summarized == group])))
  print("--------------------")
}

# get margins of error individually (in a one sample test) -- not sure if we need this
kaf_first_moe = z_star * sqrt((kaf_first_accuracy*(1 - kaf_first_accuracy)/n))
kaf_last_moe = z_star * sqrt((kaf_last_accuracy*(1 - kaf_last_accuracy)/m))


# TWO SAMPLE PROPORTION TEST

# compute confidence interval 
point_estimate = kaf_first_accuracy - kaf_last_accuracy
margin_of_error = z_star * sqrt( ((kaf_first_accuracy*(1-kaf_first_accuracy))/length(kaf_first$Subject_Number)) + ((kaf_last_accuracy*(1-kaf_last_accuracy))/length(kaf_last$Subject_Number)) )
paste(point_estimate, "±", margin_of_error)

# compute test-statistic and p-value
pnm = (length(kaf_first$Correct_Guess[kaf_first$Correct_Guess == 1]) + length(kaf_last$Correct_Guess[kaf_last$Correct_Guess == 1]))/(n + m)
test_statistic = (kaf_first_accuracy - kaf_last_accuracy - 0) / sqrt(pnm*(1-pnm)*((1/n)+(1/m)))
p_value = 2*pnorm(-1*test_statistic)

# plot test statistic on standard normal curve
curve(dnorm(x), xlim=c(-5,5), main='Sampling Distribution of Test Statistic', xlab="", ylab="", xaxt = "n") 
axis(side = 1, at = c(-1.96, 1.96), labels=c(-1.96, 1.96))

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

