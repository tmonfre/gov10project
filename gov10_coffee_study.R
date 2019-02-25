# Gov 10 Coffee Study
# Thomas Monfre, Karla Rosas, Josh Calianos, Jack Richardson

dta = read.csv('~/Documents/School/Dartmouth/Sophomore Year/19W Courses/Gov 10/project/data.csv', header=TRUE)

alpha = 0.05

# assess total correctness
num_correct = length(dta$Correct_Guess[dta$Correct_Guess == 1])
total = length(dta$Subject_Number)
percent_correct = num_correct / total

# separate kaf first vs kaf last
kaf_first = dta[dta$KAF_First == 1, ]
kaf_last = dta[dta$KAF_First == 0, ]

kaf_first_correct = kaf_first[kaf_first$Correct_Guess == 1, ]
kaf_first_incorrect = kaf_first[kaf_first$Correct_Guess == 0, ]

kaf_last_correct = kaf_last[kaf_last$Correct_Guess == 1, ]
kaf_last_incorrect = kaf_last[kaf_last$Correct_Guess == 0, ]

# get accuracy of each group
kaf_first_accuracy = length(kaf_first$Correct_Guess[kaf_first$Correct_Guess == 1]) / length(kaf_first$Subject_Number)
kaf_last_accuracy = length(kaf_last$Correct_Guess[kaf_last$Correct_Guess == 1]) / length(kaf_last$Subject_Number)

