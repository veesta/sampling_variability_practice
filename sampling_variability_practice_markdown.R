library(learnSampling)

#simulate obtaining a sample based on N= 30 from a population where the correlation is .35 
#get_cor_samples lest you create a population distribution around the rho 

get_cor_samples(rho = .35, n=30, number.of.samples = 1)

#the r value you get from the above may be different because it is an estimate from a large population 

#hypothetical: 1000 replications of the same study are done - we can model this with the below eqn 

my.samples <- get_cor_samples(rho = .35, n=30, number.of.samples = 1000)

#View(my.samples)
#print(my.samples)

#to make it easier to view, sort the correlations from big to small 

my.samples.sorted <- sort_samples_by_r(my.samples)

#View(my.samples.sorted)

#now you can look at the range of your values since their sorted - head vs. tail 
head(my.samples.sorted)
tail(my.samples.sorted)

#look at output and see that it ranges from -.15 to .7
#even though the population correlation is .35, the values can range from -.15 to .7 
#this is huge variability - conclusion you draw from a single study isn't meaningful 

#lets look at the variability in the studies with a histogram 
library(ggplot2)
plot1 <- qplot(r,data=my.samples.sorted,binwidth=.05)
plot1 <- plot1 + coord_cartesian(xlim = c(-.40,.75),ylim=c(0,300))
plot1 <- plot1 + ggtitle("Replications n = 30")

#print(plot1)
#sample correlations close to .35 are more likely than those farther from the population mean 
#individual studies are generally wrong, but on average they are correct

#if we take the mean of all these samples, we will have valuable information - see output 
mean(my.samples.sorted$r)

#look at the max and min of the sample with n=30
max(my.samples.sorted$r)
min(my.samples.sorted$r)
#the above sample mean is .34 which is very close to the population mean - this suggests that there is very little variability
#only reason sample and population mean varies was due to random sampling

#how to calculate the assumption that it is random sampling (compare expected variability to observed variability)
#see handout - calculate expected by hand (1-p^2)^2 / N-1

#calculate observed variability: 
var(my.samples.sorted$r)

#expected = .026, observed = .026 --> pretty similar = only source of variability is random sampling 
#since all variability is random, there is no replication crisis in this sample - don't always need to find a reason for differences in the same sample - may just be random sampling 
#the more meta analysis studies you include, the more accurate your average mean of the studies will be to the true population 

#BREAK 

#SCENARIO 2 is the same but with a LARGER sample size (n=75) 
library(learnSampling)
get_cor_samples(rho = .35, n=75, number.of.samples = 1000)
my.samples.75 <- get_cor_samples(rho = .35, n=75, number.of.samples = 1000)
my.samples.75.sorted <- sort_samples_by_r(my.samples.75)
head(my.samples.sorted)
tail(my.samples.sorted)

mean(my.samples.75.sorted$r)
max(my.samples.75.sorted$r)
min(my.samples.75.sorted$r)

plot2 <- qplot(r,data=my.samples.75.sorted,binwidth=.05)
plot2 <- plot2 + coord_cartesian(xlim = c(-.40,.75),ylim=c(0,300))
plot2 <- plot2 + ggtitle("Replication n=75")
print(plot2)


#scenario 3 - even larger sample size (n=150)
library(learnSampling)
get_cor_samples(rho = .35, n=150, number.of.samples = 1000)
my.samples.150 <- get_cor_samples(rho = .35, n=150, number.of.samples = 1000)
my.samples.150.sorted <- sort_samples_by_r(my.samples.150)
head(my.samples.150.sorted)
tail(my.samples.150.sorted)

mean(my.samples.150.sorted$r)
max(my.samples.150.sorted$r)
min(my.samples.150.sorted$r)

library(ggplot2)
plot3 <- qplot(r,data=my.samples.150.sorted,binwidth=.05)
plot3 <- plot3 + coord_cartesian(xlim = c(-.40,.75),ylim=c(0,300))
plot3 <- plot3 + ggtitle("Replications n = 150")


print(plot3)

#look at all 3 graphs together with different sample sizes 

library(gridExtra)
grid.arrange(plot1,plot2,plot3,nrow=2)

#in real life all meta analysis samples are different - exp. 50 to 1000

my.samples.vary <- get_cor_samples(rho = .35, n.min=50,n.max=1000, number.of.samples = 1000)

#look at the range of values with:
#View(my.samples.vary)

#make a scatter plot where x axis is correlation size and y axis is sample size 
library(ggplot2)
plot4 <- qplot(r,n,data=my.samples.vary)
plot4 <- plot4 + ggtitle("Observed Correlation by Sample Size")
print(plot4)

#use weighted mean to calculate the meta analytic correlation - in output 
#ensures that samples based on large sample size contribute more to the mean than a small sample size

weighted.mean(my.samples.vary$r,w=my.samples.vary$n)

#each sample correlation (effect size) gives an estimate of the population correlation 
