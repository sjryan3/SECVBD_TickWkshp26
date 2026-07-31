library(popbio)

f1 <- 0
f2 <- 0
f3 <- 0.4
f4 <- 25

p1 <- 0.5
p2 <- 0.2
p3 <- 0.2

########################################################
# you shouldn't have to change anything below this line#
########################################################

x<- matrix(c(f1,f2,f3,f4,p1,0, 0,0,0,p2,0,0,0,0,p3,0),4)
#the following transposes x, which is useful for eigenvectors, but doesn't affect the dom eig'
x<- t(x)

n<- c(10, 3, 2, 1)
print(c("Dominant Eigenvalue =", lambda(x)), quote = FALSE)
p<-pop.projection(x,n,500)
names(p)
plot(p$pop.sizes)
stage.vector.plot(p$stage.vectors)

print("Eigenvector ", quote = FALSE)
print(stable.stage(x))

print("Sensitivities ", quote = FALSE)
print(sensitivity(x,zero=FALSE))

print("Elasticities ",quote = FALSE)
print(elasticity(x))
