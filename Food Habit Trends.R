######intro######
setwd() #set your working directory here
habits<-read.csv(paste(as.character(getwd()),"/Food Habit Tracker.csv",sep=""), header = TRUE)

#assumption: only three meals per day
#because some days could have overlapping fish + dairy or eggs + dairy combos we can't calculate vegan meals per day but we can on totaly vegan days or on days where only one of the indicators is checked

######data wrangling######
#narrowing data set to only days which are filled in
habits<-habits[is.na(habits$Meals.w..Dairy)==FALSE,]
i<-0
veganMeals<-c()
for (i in 1:length(habits$Date)) {
  if (is.na(habits$Meals.w..Dairy[i])) {
    vmeals<-NA
  } else if(habits$Meals.w..Dairy[i]<=3 & habits$Meals.w..Eggs[i]==0 & habits$Meals.w..Seafood[i]==0)
  {
    vmeals<-3 - habits$Meals.w..Dairy[i]
  } else if (habits$Meals.w..Eggs[i]<=3 & habits$Meals.w..Dairy[i]==0 & habits$Meals.w..Seafood[i] ==0) {
    vmeals<-3 - habits$Meals.w..Eggs[i]
  } else if (habits$Meals.w..Seafood[i]<=3 & habits$Meals.w..Dairy[i]==0 & habits$Meals.w..Eggs[i]==0 ) {
    vmeals<- 3 - habits$Meals.w..Seafood[i]
  } else if (habits$Meals.w..Dairy[i]==0 & habits$Meals.w..Eggs[i]==0 & habits$Meals.w..Seafood[i]==0) {
    vmeals<-3
  } else {
    vmeals <- NA
  }
  
  veganMeals<-c(veganMeals,vmeals)
  i<-i+1
}

habits<-cbind(habits,veganMeals)

#####trend analysis####

#test one, does the amount of single use plastic used vary with meals including dairy

#start with visualizing the data
plot(habits$Meals.w..Dairy, habits$Meals.requiring.opening.single.use..non.recyclable.plastic..total.count.of.items.)
dairyModel <- lm(habits$Meals.requiring.opening.single.use..non.recyclable.plastic..total.count.of.items.~habits$Meals.w..Dairy)
summary(dairyModel)
#rsquared is low and p value is high

#test two, does vegan meals correlate with the number of single use pastic used
veganHabits<-habits[is.na(habits$veganMeals)==FALSE,]

plot(veganHabits$veganMeals,veganHabits$Meals.requiring.opening.single.use..non.recyclable.plastic..total.count.of.items.)

veganModel<- lm(veganHabits$Meals.requiring.opening.single.use..non.recyclable.plastic..total.count.of.items.~veganHabits$veganMeals)
summary(veganModel)
#def a trend up but with p value is low and rsquared isn't high 

