# Seminar 3 

library(tidyverse)

rm(list = ls())

t <- seq(0, 100, by = 1) 
df <- data.frame(t)

y0 <- 600
alpha <- 0.2
beta <- 0.6
gamma <- 0.2
ga0 <- 0
j0 <- 0
m0 <- 0
h0 <- 0
ga1 <- 0.01
j1 <- 0.01
m1 <- 0.01
h1 <- 0.01
n <- 0.02
u <- 0.0001

gy0 <- ((1/(1-alpha))*(ga0+alpha*j0+beta*m0 +gamma*h0))-(gamma/(1-alpha))*(u+n)
gy1 <- ((1/(1-alpha))*(ga1+alpha*j1+beta*m1 +gamma*h1))-(gamma/(1-alpha))*(u+n)

g0 <- ((1/(1-alpha))*(ga0+alpha*j0+beta*m0 +gamma*h0))
g1 <- ((1/(1-alpha))*(ga1+alpha*j1+beta*m1 +gamma*h1))

growthdrag <- (gamma/(1-alpha))*(u+n)

print(gy0)
print(gy1)
print(g0)
print(g1)
print(growthdrag)

yt_0 <-function(t) (y0*exp(gy0*t))
yt_1 <- function(t) (y0*exp(gy1*t))



axes_1 <- ggplot(df, aes(t))+
  labs(x="Tid", y="BNP per innbygger") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 10),
        panel.background = element_blank(), # hvit bakgrunn
        axis.line = element_line(colour = "black"), # sett inn akselinjer
        axis.title.x = element_text(hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 1, hjust=1))+
  coord_fixed(ratio = 1)+ # lik skala for x og y aksen
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0), breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
  scale_y_continuous(limits = c(0, 4500), expand = c(0, 0), breaks=c(0,250,500,750,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000,4250,4500))+ # begrense aksene
  coord_fixed(ratio = (100 - 0) / (4500 - 0)) 
axes_1


figur_2 <- axes_1 +
  stat_function(df,
                fun=yt_0,
                mapping = aes() # vekst i situasjon 0
  )+
  stat_function(df,
                fun=yt_1,
                mapping = aes(),
                color = "blue"# vekst i situasjon 1
  )+
  geom_segment(aes(x = 0, xend = 95, y = 600, yend = 600), linetype = "dashed", color = "red") + 
  annotate("text",
           x = 96,
           y = 600, 
           label=expression(y[~0])) +
  annotate("text",
           x = 96,
           y = 4250, 
           label=expression(g[~y1]))+
  annotate("text",
           x = 96,
           y = 425, 
           label=expression(g[~y0]))

figur_2


