library(readxl)
data <- read_excel("~/Downloads/ultrarunning.xlsx")
#Remove all rows with at least one NA
df <- na.omit(data)
summary(df)
head(df)

#Categorical plots to understand distribution
#1 Potential outlier in female (lower) 
boxplot(df$teique_sf ~ df$sex, names=c("1 = Male", "2 = Female"), main="Figure 1")

#1 potential outlier in trail category (lower)
boxplot(df$teique_sf ~ df$pb_surface, names=c("1 = Trail", "2 = Track", "3 = Road", 
                                              "4=Mix of 3"), main="Figure 2")
barplot(table(df$sex),xlab="Sex", ylab="Frequency", main="Figure 3")
barplot(table(df$pb_surface),xlab="Surfaces", ylab="Frequency", names=c("1 = Trail", "2 = Track", "3 = Road", 
                                                                        "4 = Mix of 3"),main="Figure 4")

#Numerical plots to understand distribution
#Potential outlier in age (upper)
hist(df$age, main="Figure 5")
boxplot(df$age, xlab="Age", main="Figure 6")

#Fastest time looks normally distributed 
hist(df$pb100k_dec, main="Figure 7")
boxplot(df$pb100k_dec, xlab="Fastest time to complete a 100km ultra marathon",
        main="Figure 8")

#Total elevation change is skewed to the right with potetial outliers (upper)
hist(df$pb_elev, main="Figure 9")
boxplot(df$pb_elev, xlab="Elevation", main="Figure 10")

#Average km run per week looks normally distributed with a couple outliers 
#in both percentiles
hist(df$avg_km, main="Figure 11")
boxplot(df$avg_km, xlab="Average distance run per week", main="Figure 12")

#Scatterplots to asses linearity

#Data looks moderately linear
#No transformation presents improvements
plot(df$age, df$teique_sf, main="Figure 13")
plot(log(df$age), df$teique_sf, main="Logarithmic transformation")

summary(df$age)

#Not much linearity is present, may be due to the presence of outliers
#No transformation improves it
plot(df$pb_elev, df$teique_sf, main="Figure 14")
plot(sqrt(df$pb_elev), df$teique_sf, main="Square root transformation")
hist(sqrt(df$pb_elev), main="Square root transformation on pb_elev")
boxplot(sqrt(df$pb_elev), main="Square root transformation on pb_elev", xlab="Elevation")


#Data looks linear for the most part 
plot(df$pb100k_dec, df$teique_sf, main="Figure 15")
plot(sqrt(df$pb100k_dec), df$teique_sf, main="Square root transformation on pb_elev")
plot(log(df$pb100k_dec), df$teique_sf, main="Logarithmic transformation on pb_elev")


summary(df$pb100k_dec)

#Data looks moderately linear for the most part with some spread  and potential outliers
plot(df$avg_km, df$teique_sf, main="Figure 16")
plot(sqrt(df$avg_km), df$teique_sf, main="Square root transformation on avg_km")
plot(log(df$avg_km), df$teique_sf, main="Logarithmic transformation on avg_km")


summary(df$avg_km)

#Partial regression plots
df$sex <- factor(df$sex, levels = c(1,2), 
                 labels = c("Male", "Female"))
df$pb_surface <- factor(df$pb_surface,
                        levels = c(1, 2, 3, 4),
                        labels = c("trail", "track", "road", "mix"))

full_model <- lm(teique_sf ~ age + pb_elev + pb100k_dec + avg_km + pb_surface + sex, 
                 data=df)
transformed_model <- lm(teique_sf ~ age + I(sqrt(pb_elev)) + pb100k_dec + avg_km + pb_surface + sex, 
                 data=df)

#Numerical values are linear, age variable is slightly pulled by a potential outlier
crPlots(full_model)
crPlots(transformed_model)


#Correlation between independent variables
#No correlation is high
cor(df[, -c(2, 3, 5, 8,9, 10)])

df_transformed <- df
#Transform pb_elev
df_transformed$sqrt_pb_elev <- sqrt(df_transformed$pb_elev)

num_vars <- df_transformed[, c("age", "sqrt_pb_elev", "pb100k_dec", "avg_km")]

# Compute the correlation matrix using complete observations
cor_transformed <- cor(num_vars)
#Transformed pb_elev correlation between independent variables
print(cor_transformed)
