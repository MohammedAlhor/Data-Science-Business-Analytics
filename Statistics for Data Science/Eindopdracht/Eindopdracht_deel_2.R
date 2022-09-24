# 3 (a) Voer eerst een test uit voor de hypothese dat het aantal aanmeldingen een
# normale verdeling volgt. Wat is je conclusie? Is deze conclusie van belang voor
# het verder modelleren van deze variabele?
summary(college_statistics$Apps)
hist(college_statistics$Apps)
qqnorm(college_statistics$Apps)
# Both the histogram and qq plot indicate that this variable is not normally distributed (Positive skewness in histogram and )
# Lets put this to the test with a Shapiro-Wilk test for normality
# H0: Apps is normally distributed Ha: Apps is not normally distributed
shapiro.test(college_statistics$Apps)

# The p-value is less than 0.05, which means the null hypothesis can be rejected and the data is not normally distributed.