rm(list=ls())
require(deSolve)
#----- The differential equations -----
sir <- function(t, x, p) {
  with(as.list(c(x,p)),{
    
    N = S + I + R
    dS <- mu*N - k*S*log(1+beta*I/k)- mu*S
    dI <- k*S*log(1+beta*I/k) - gamma*I - mu*I
    dR <- gamma*I - mu*R
    
    list(c(dS, dI, dR))
  })
}
mu = 1/(5*365)
beta <- 0.00001 # infection rate
gamma <- 0.1 # recovery rate
k = 100

S_0 <- 50000.0 # Initial conditions
I_0 <- 5.0 # initial conditions
R_0 <- 0.0 # initial conditions

times <- seq(0.0, 100.0, 0.1) # Time sequence
parms <- c(mu=mu, beta=beta, gamma=gamma,k=k)

xstart <- c(S=S_0, I=I_0, R= R_0) # Initial conditions
my.atol <- c(1e-16,1e-16,1e-16); # Abs. accuracy - remember to add a term for each equation
my.rtol <- 1e-12 # Rel. accuracy
out <- as.data.frame(lsoda(xstart, times,
                           sir, parms, my.rtol, my.atol)) # Solve the eqns.
#----- Plot the output -------
plot(out$time, out$I, col="black", type="l", lwd=5,
     ylim=c(0,50000), xlab="Time", ylab="Number")
lines(out$time, out$S, col="red", lwd=5)
lines(out$time, out$R, col="blue", lwd=5)
grid(NULL, NULL, lty=1,lwd=1)

# plot(out$time[20000:100000], out$I[20000:100000], col="black", type="l", lwd=5,
#       xlab="Time", ylab="Number")
#N_star <- m/b
#print(N_star) # Print out equilibria values
#print((r/a)*(1-m/(b*K)))

