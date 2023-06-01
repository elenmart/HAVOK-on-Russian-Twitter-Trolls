# install.packages("devtools")
# devtools::install_github("RobertGM111/havok")

library(havok)


######################################################################################
### Russian Twitter trolls ### Interindividual time series governed by Fourier cycles

load("results.all.trolls.Rdata")

right <- results.all$avg.all.date[results.all$avg.all.date$Type=="Right",] # only right-wing trolls

xdat <- right$Topic2 + right$Topic5 + right$Topic11 # tweets on 3 Trump supporting topics
dt <- 0.25   # 4 measurements per day, so one time unit = 1/4 of a day

plot(xdat,type="l")

# Model c

stackmax <- 56   # 56/4 = 9 day long kernels
rset <- 6           
lambda <- 0.006   
hav <- havok(xdat = xdat, dt = dt, stackmax = stackmax, lambda = lambda,rmax=NA,rset=rset,
             polyOrder = 1, useSine = FALSE,devMethod = "FOCD",
             gllaEmbed = NA, alignSVD = TRUE)
(R2 <- cor(hav$Vr_aligned[,1],hav$havokSS$y[1,])^2)

plot(hav)

hav$sys 


# Model a

stackmax <- 14  # 14/4 = 3.5 day long kernels
rset <- 4
lambda <- 0.002
hav <- havok(xdat = xdat, dt = dt, stackmax = stackmax, lambda = lambda,rmax=NA,rset=rset,
             polyOrder = 1, useSine = FALSE,devMethod = "FOCD",
             gllaEmbed = NA, alignSVD = TRUE)
(R2 <- cor(hav$Vr_aligned[,1],hav$havokSS$y[1,])^2)

plot(hav)

hav$sys 


# Model b

stackmax <- 36   # 36/4 = 9 day long kernels
rset <- 10
lambda <- 0.1242  
hav <- havok(xdat = xdat, dt = dt, stackmax = stackmax, lambda = lambda,rmax=NA,rset=rset,
             polyOrder = 1, useSine = FALSE,devMethod = "FOCD",
             gllaEmbed = NA, alignSVD = TRUE)
(R2 <- cor(hav$Vr_aligned[,1],hav$havokSS$y[1,])^2)

plot(hav)

hav$sys 


# Model d

stackmax <- 81  #  81/4 = 20.25 day long kernels
rset <- 6            
lambda <- 0.04 
hav <- havok(xdat = xdat, dt = dt, stackmax = stackmax, lambda = lambda,rmax=NA,rset=rset,
             polyOrder = 1, useSine = FALSE,devMethod = "FOCD",
             gllaEmbed = NA, alignSVD = TRUE)
(R2 <- cor(hav$Vr_aligned[,1],hav$havokSS$y[1,])^2)

plot(hav)

hav$sys 
