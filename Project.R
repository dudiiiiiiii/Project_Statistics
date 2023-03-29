install.packages("tidyverse")
install.packages("Hmisc")
library(tidyverse)
library(Hmisc)


dim(ATP_main)
colnames(ATP_main)
View(ATP_main)
#summary(ATP_main) #statystyki dla kolumn

# sprawdzam czy dane nie zawieraj¹ null / pustych pól

colSums(is.na(ATP_main))

#

n <- as.integer(count(ATP_main[1]))

age <- sapply(ATP_main[2], mean) #sredni wiek
PAge <- sapply(ATP_main[10], mean) # sredni najlepszy wiek
Pvar <- sapply(ATP_main[10], var) # wariancja najlepszego wieku
Psd <- sapply(ATP_main[10], sd) # odchylenie stand. najlepszego wieku


x <- (ATP_main %>%
  drop_na(Peak.Age) %>%
  ggplot(aes(x = Peak.Age))+
  geom_bar(fill = "#2ae14b")+
  theme_bw()+
  labs(x = "Peak Age", y = NULL, title = "Age for best results"))

Pmin <- sapply(ATP_main[10], min)
Pmax <- sapply(ATP_main[10], max)

tablica <- table(ATP_main[10])
tablica_s <- names(sort(tablica, decreasing=T))
dominant <- tablica_s[1]
dominant

# Przedzia³y ufnoœci

# t-studenta (parametr wart. oczekiw.)
range <- function(lvl) {
  alfa <- 1 - lvl
  return (round(PAge+c(-1,1)*Psd/sqrt(n)*qt(1-alfa/2, n-1),2))
}

range(0.9)

# rozk³ad x^2 (paramtr wariancja)

varRange <- function(lvl) {
  alfa <- 1 - lvl
  return (round(Psd*n/qchisq(c(1-alfa/2,alfa/2), n-1),2))
}

varRange(0.9)

########################################
# Peak.age to grass.court.rating
ggplot(ATP_main, 
       mapping = aes(x=Peak.Age, y = grass.court.elo.rating)) +
  geom_point()

# Peak.age to clay.court.rating
ggplot(ATP_main, 
       mapping = aes(x=Peak.Age, y = clay.court.elo.rating)) +
  geom_point()

# Peak.age to hard.court.rating
ggplot(ATP_main, 
       mapping = aes(x=Peak.Age, y = hard.court.elo.rating)) +
  geom_point()

#################################

df <- data.frame(ATP_main[10])
hist.data.frame(df)


for( i in df){
  i <- round(i,1)
}
i
fivenum(i)
hist(i)
lines(i)
qqnorm(i)
qqline(i)
boxplot(i) #boxplot


hist(qqq)

##########################
#perform kolmogorov-smirnov test
ks.test(i, "pnorm")

shapiro.test(i)
# Shapiro-Wilk normality test
lapply(df, shapiro.test)

t.test(i)



