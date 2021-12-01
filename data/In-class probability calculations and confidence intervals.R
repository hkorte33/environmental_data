#poisson-distribued pop prop density or mass ex.
dpois(x=7,lambda = 10.4)
#Q1- n=6, p=2/3
#Q2- Binomial propability 1
dbinom(x=4, size= 6, p=2/3)
#Q3- Binomial propability 2
dbinom(x=0,size=6,p=2/3)
#poisson-distribued pop of observing a range of events.
ppois(q=7,lambda=10.4)
#Law of Total Probability and Complementary Events
1-ppois(q=7,lambda=10.4)
#Q4- Binomial propability 3
pbinom(4,6,2/3)
#or 
dbinom(1,6,2/3)+
dbinom(2,6,2/3)+
dbinom(3,6,2/3)+
dbinom(4,6,2/3)
#Q5- Binomial propability 4
1-pbinom(3,6,2/3)
#0r 
dbinom(4,6,2/3)+
dbinom(5,6,2/3)+
dbinom(6,6,2/3)
