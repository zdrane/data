##### Play with a suicide dataset for fun   #####

setwd("/home/zelda/Downloads/project")
df <- read.csv("master.csv", header = TRUE) ## Pulled from https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016
year <- sort(as.character(unique(df$year)))

## Checking GDP as a predictor of suicide rates
gdp <- aggregate(df[,c("gdp_per_capita....","suicides.100k.pop")], list(df$country), FUN = "mean")
gdp <- gdp[order(gdp$gdp_per_capita....),]
gdp

#pdf("sui-by-gdp.pdf")
plot(gdp$suicides.100k.pop ~ gdp$gdp_per_capita...., main = "Scatter plot of GDP per capita against suicide rates.",
     xlab = "GDP per capita", ylab = "Suicides per 100k", las = 1)
gdp_model <- lm(gdp$suicides.100k.pop ~ gdp$gdp_per_capita....)
abline(gdp_model)
#dev.off()
summary(gdp_model)

## Before plotting the data we need to calculate the suicides rates per country.
sui_rates <- function(country, data = df){
  abbreviation <- data[data$country==country,]

  ## Initialize the vectors
  abbreviation_sui <- NULL
  abbreviation_pop <- NULL
  for(i in year){
    abbreviation_sui[year] <- 0
    abbreviation_pop[year] <- 0
    }
  ## Condense the data to isolate the suicides rates per year in a given location.
  for(i in year){
  for(j in 1:length(abbreviation$year)){
    if(i == abbreviation$year[j]){
      abbreviation_sui[i] <- abbreviation_sui[i] + abbreviation$suicides_no[j]
      abbreviation_pop[i] <- abbreviation_pop[i] + abbreviation$population[j]
      }
    }
    }
  ((abbreviation_sui/abbreviation_pop)*100000)
}
## There doesn't seem to be a strong relation to gdp and suicide rates.

## Now lets explore the relationship between suicide rates, sex, and age.

amab <- df[df$sex=="male",]
sum(amab$suicides_no)/sum(amab$population)*100000
afab <- df[df$sex=="female",]
sum(afab$suicides_no)/sum(afab$population)*100000

amab_country <- NULL
afab_country <- NULL
for(country in unique(df$country)){
  amab_country[country] <- mean(sui_rates(country, data = amab), na.rm = TRUE) 
  afab_country[country] <- mean(sui_rates(country, data = afab), na.rm = TRUE) 
}

#pdf("sui-by-sex.pdf")
pie(c(mean(amab_country),mean(afab_country)), labels = c(round(mean(amab_country), 2),
                                                         round(mean(afab_country),2)), col = c("Blue","Red"),
    main = "Global proportion of suicides by birth sex.")
legend("topright",pch=15, col = c("Blue","Red"), c("Male","Female"))
#dev.off()
sd(amab_country)
sd(afab_country)

t.test(amab_country, conf.level = 0.95)
t.test(afab_country, conf.level = 0.95)
t.test(amab_country, y = afab_country, alternative = "g", conf.level = 0.95)


#pdf("sui-by-sex-by-country.pdf", width = 19, height = 15)
barplot(c(amab_country), beside = FALSE, las = 2, col = "Blue",
        main = "Plot suicide proportion by sex by country", ylab = "suicides per 100k")
barplot(afab_country, add = TRUE, las = 2, col = "Red")
legend("topright", c("Male","Female"),pch = 15, col = c("Blue","Red"))
#dev.off()

age_sex <- aggregate(df[,c("suicides.100k.pop")], list(df$age, df$sex), FUN = "mean")
age_sex$Group.1 <- substring(age_sex$Group.1, 1,5)
age_sex$Group.1[age_sex$Group.1=="75+ y"] <- "75+"
age_sex$Group.1[age_sex$Group.1=="5-14 "] <- "05-14"
age_sex <- age_sex[order(age_sex$Group.1),]

## Run an ANOVA test to verify statistical significance.
anova <- aov(x ~ Group.1, data = age_sex)
summary(anova)


#pdf("sui-by-age-by-sex.pdf")
barplot(age_sex[age_sex$Group.2=="male","x"], names.arg = age_sex[age_sex$Group.2=="male","Group.1"], las = 2, col = "Blue", xlab = "Age Group",
        ylab = "Suicides per 100k population", main = "Plot suicide rates by age and sex.")
barplot(age_sex[age_sex$Group.2=="female","x"],beside = FALSE, add = TRUE, col = "red", axes = FALSE)
legend("topleft", c("Male","Female"),pch = 15, col = c("Blue", "Red"))
#dev.off()