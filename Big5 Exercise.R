###### Psychometric Exercises ######


 ##### Big5 Data prep and score https://www.youtube.com/watch?v=iFFW5sK3Bhk psymetricExer data
# Uses psymetric_exer.csv
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr, psych, Hmisc, lavaan, lm.beta)

head(psymetricExer)
headTail() #Another option
View() #opens data in new tab
na.omit() #remove N/A's if needed
str(psymetricExer) #Data structure
dim(psymetricExer) #Data dimentions
names(psymetricExer) #Returns column names
dput(names(psymetricExer)) #Useful trick that prints column names in a format that can be copy and pasted into later code
psymetricExer$gender #Gender is 1's and 2's next steps convert to male and female
?factor
psymetricExer$genderf <- factor(psymetricExer$gender, c(1,2), c("male", "female")) #Converts 1's and 2's to male/female and saves into dataframe
table(psymetricExer$gender, psymetricExer$genderf) #puts into crosstabulation table to visually confirm
psymetricExer$educationf <- factor(psymetricExer$education, c(1,2,3,4,5), c("HS", "finished HS", "some college", "college graduate", "graduate degree")) #converts education like above
table(psymetricExer$education, psymetricExer$educationf) #puts into crosstabulation table to visually confirm
range(psymetricExer$a1, na.rm = TRUE) #individual way confirm data is within exected range, this data should be 1-6
describe(psymetricExer[,c("a1", "a2", "a3")]) #multiple way to confirm data is within ecpected range, this data should be 1-6
#Below is another useful trick to store items and/or factors into lists that can be easily and cleanly called in other code or analysis NOTE: the "v" is arbitrary can be any name
v <- list()
v$allitems <- c("a1", "a2", "a3", "a4", "a5", "c1", "c2", "c3", "c4", "c5", 
                "e1", "e2", "e3", "e4", "e5", "n1", "n2", "n3", "n4", "n5", "o1", 
                "o2", "o3", "o4", "o5")
v$a <- c("a1", "a2", "a3", "a4", "a5")
v$c <- c("c1", "c2", "c3", "c4", "c5")
v$e <- c("e1", "e2", "e3", "e4", "e5")
v$n <- c("n1", "n2", "n3", "n4", "n5")
v$o <- c("o1", "o2", "o3", "o4", "o5")
describe(psymetricExer[,c(v$n)]) #Simple example of use of above trick
factanal(na.omit(psymetricExer[,v$allitems]), 5) # Another example of above trick by performing a factor analysis fit to 5 factors 
#Dealing with missing data
is.na(psymetricExer$a1) #Returns which datapoints are missing or NA
sum(is.na(psymetricExer$a1)) #If you take the sum of a TRUE/FAlSE expression, such as is.na, it returns the sum of what is TRUE
?sapply
sapply(psymetricExer, function(x) sum(is.na(x))) #sapply enables you to apply a function to every column therefore this will return the number of missing datapoints in each column
sapply(psymetricExer[,v$allitems], function(x) mean(x, na.rm = TRUE)) #Another sapply example like above but this will return the mean of each column
?apply
apply(psymetricExer, 2, function(x) sum(is.na(x))) #Alternative to above sapply but the '2' tells to look at columns, '1' would look at rows
apply(psymetricExer, 1, function(x) sum(is.na(x))) #apply is similar to sapply but allows you to perform the function on the rows, therefore, this will return the number of missing data points by case or row. The '1' designates using rows
psymetricExer$miss <- apply(psymetricExer, 1, function(x) sum(is.na(x))) #Adds a missing count column to each person
table(psymetricExer$miss) #Confirms above and returns a table of number of cases/people missing particular amounts of data
psymetricExer$retain <- psymetricExer$miss < 4 #Retain cases/people missing less than 4
table(psymetricExer$retain) #Confirms and returns table of number of cases/people that will be dropped and retained
cleandata <- psymetricExer [ psymetricExer$retain, ] #Create a 'cleaned' dataframe that excludes the cases/people above the missing data cutoff
dim(psymetricExer) 
dim(cleandata) #Confirm and compare new data dimentions to prior not cleaned data dimentions above
#### Scoring #####
?scoreItems #In psych package
keys.list <- list(agreeableness=c("-a1","a2","a3","a4","a5"),
                  conscientiousness=c("c1","c2","c3","-c4","-c5"),extraversion=c("-e1","-e2","e3","e4","e5"),
                  neuroticism=c("n1","n2","n3","n4","n5"), openness = c("o1","-o2","o3","o4","-o5")) #Creating keys list
scoreItems(keys.list, cleandata[, v$allitems])
scores <- scoreItems(keys.list, cleandata[, v$allitems])
summary(scores)
describe(scores$scores) #The scores themselves are available in the scores$scores object. Type "scores$" and scroll through the options; includes item.col, alphas, cor, missing, etc
round(scores$cor, 2) #Scale/construct correlation table rounded to 3 digits
round(scores$item.cor, 2) #Item correlation table rounded to 2 digits
round(scores$alpha, 2) #Cronbach's Alpha rounded to 2 digits
cleandata[,colnames(scores$scores)] <-scores$scores #This will add the scores to the datatable 
describe(cleandata$agreeableness)
describe(cleandata[,v$bigfive]) #Since we added the scores to the cleandata this is the same as running describe(scores$scores)
round(cor(cleandata[,v$bigfive]), 2) #Since you added the scores to the cleandata you can do things like this which is the same as doing round(scores$cor, 2) above
# Correlations with significance test below; uses Hmisc package
?rcorr
rcorr(as.matrix(cleandata[,v$bigfive]), type="pearson") # type can be pearson or spearman; in the Hmisc package
hist(cleandata[,v$bigfive])
?t.test
t.test(agreeableness ~ genderf, cleandata) #Running a t-test predicting agreeableness from gender; common in r for left is predicted by right 
lm(agreeableness ~ genderf + educationf, cleandata) #Multiple regression looking at gender and education; another example of r doing left is predicted by right like t.test above
ggplot(cleandata, aes(x = age, y = agreeableness)) + geom_point() + geom_smooth()



git config --global user.email "sean@seantopham.com"
git config --global user.name "seantopham"