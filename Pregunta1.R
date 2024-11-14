#Distribució poblacional:
#Pasos:

#1. Identificar model, tenim:

#     Binomial, binomial negativa, hipergeometrica, Poisson, Uniforme, exp i normal

#Ex binomial: Si tenim una enquesta: P(X=a) --> dbinom(a, n, p)

#Ex binomial negtiva (repetim l'experiment fins que passi el que estudiem(error) passi n vegades): dnbinom(a, r, p)
# on r és el numero de fallos

#Ex hiperg.: Agafem x boles dins una urna amb boles de diferent color dhiper(a, N, n, k)

#Ex Poisson: contar el nº d'eventos en un espai de temps: dpois(a, λ)

#Ex Exp: temps fins el 1r evento en un exp. de Poisson --> pexp(a, λ)

#Ex Normal: mirar l'alçada d'un grup de persones --> pnorm(a, pi, sigma)

#2. Indentificar paràmetres

# Exercici:

mu <- 95.3
sigma <- 5.7

#N -> N(mu, sigma^2)

curve(dnorm(x, mean=mu, sd=sigma), xlim = c(80,120), col = "red")

Y <- function(i){sum(rnorm(4, mean = mu, sd = sigma))}
Y(1)

# Y = sum(xi) des de i=1 fins a 4 --> Suma Muestral (estadístico)

y100000 <- sapply(1:100000, Y)
mean(y100000) #E(Y)
 
hist(y100000, freq = FALSE)
#E(Y) = n*mu   V(Y)=n*sigma^2
curve(dnorm(x, 4*mu, 2*sigma), col = "red", add = TRUE) #mu de y = n*mu    sigma de y = sqrt(n)*sigma

#Apartat a)
4*mu

#Apartat b)
Y <- function(i){sum(rnorm(100, mean = mu, sd = sigma))}
y100000 <- sapply(1:100000, Y)
var(y100000)
100*sigma^2

#Apartat c)
#Població: X~N(mu, sigma^2)
#En demana: P(X>103) = 1 - pnorm(103, mu, sigma)
1 - pnorm(103, mu, sigma)

#Apartat d)

xbar <- function(i){mean(rnorm(4, mean = mu, sd = sigma))}
xbar100000 <- sapply(1:100000, xbar)
hist(xbar100000, freq = FALSE)
curve(dnorm(x,mu,sigma/sqrt(4)), col="red", add=TRUE)
mean(xbar100000<98)


#Apartat e)
#P(S^2 > 32)? On S^2 és la varianza muestral
ssq <- function(i){var(rnorm(100, mean = mu, sd = sigma))}
ssq100000 <- sapply(1:100000, ssq)
hist(ssq100000, freq = FALSE)

mean(ssq100000>32) #Tots

hist(ssq100000*(100-1)/sigma^2, freq = FALSE)
curve(dchisq(x, 100-1), add = TRUE, col = "red")

W <- 32*(100-1)/sigma^2
W
1- pchisq(W,100-1)
mean(ssq100000>32)
