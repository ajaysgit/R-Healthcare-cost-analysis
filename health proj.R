# Importing & Reading the file
library(readxl)
hlth <- read_xlsx(file.choose())
View(hlth)
head(hlth)

# finding the summary of imported  file
summary(hlth)

#lets do this by 1st insight

hist(hlth$AGE,
     main = 'Histogram', border = 'red',
     col = 'Orange',
     xlab = 'AGE', 
     freq = TRUE)

#checking the class (obvi numeric but just checking)
class(hlth$AGE)

#Converting it into factor type

a <- as.factor(hlth$AGE)
class(a)
summary(a)

#lets check the discharge costs according to age by adding "TOTCHG"

tapply(hlth$TOTCHG, hlth$AGE,sum)

#now view the maximum among them
which.max(tapply(hlth$TOTCHG, hlth$AGE,sum))

#hence maximum expenditure incurred by hospital is on infant .i.e., of 678118

# Lets get the 2nd insight .i.e., "APRDRG"

class(hlth$APRDRG)

# same steps lets convert numeric into factor

b <- as.factor(hlth$APRDRG)
class(b)
summary(b)

#lets check the max of them

which.max(summary(b))

# lets add and check both expenditure and diag groups for accuracy

tapply(hlth$TOTCHG,b,sum)

# max in them

which.max(tapply(hlth$TOTCHG,b,sum))

#hence, reflects the same

#now lets find the exact accurate value which is max in them

max(tapply(hlth$TOTCHG,b,sum))

#hence, we can conclude from above measures is that highest cost is "437978" 
#i.e., category "640"

#as per my conclusion race doesn't have any relation but lets check it out

class(hlth$RACE)
c <- as.factor(hlth$RACE)
class(c)
summary(c)

#we found there is an "NA" value inside the data, lets remove it off
# and check with highest expenditure
hltha <- na.omit(hlth)

x <- aov(hlth$TOTCHG~hlth$RACE)
summary(x)

#we can see that pvalue reflects the highest i.e., 68% so we can go forward
#and ignore hypothesis test, so we can finally conclude that there is no 
#relation between race and cost


#lets solve it by comparing and finding the pvalues by all other different
#possible outcomes & availabilities 


y <- lm(hlth$TOTCHG~hlth$AGE+hlth$FEMALE)
summary(y)

#Comparatively we can see that even gender have lesser pvalue

#lets compare the final attribute .i.e., "LOS" along with addition to age, gender $ race

z <- lm(hlth$LOS~hlth$AGE+hlth$FEMALE+hlth$RACE)
summary(z)

# here we got very high pvalue so it means no relation of linear among attribute.
# Conclusion: we cant predict the "LOS" with just age, gender & race

ab <- lm(TOTCHG~ .,data=hltha)
summary(ab)

#by now we can say that "APRDRG" is also  affected,
#Final conclusion: Cost is affected mainly by AGE and LOS.
