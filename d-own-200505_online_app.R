# d-own-200505.R Figure 3 & 5 for d-own-36.tex (JEMS accepted)
# d-own-181104.R Figure 4 & 5 for d-own-8.tex
library(ggplot2)

#########
# Figure 3 in Section 4 starts here:
phi2 = 0.05 #
phi1 = 0.10 # 
phi0 = 0.75 # 
gamma = 64

mu = seq(0.5, 1, 0.005) #Horizontal axis
length(mu)# 101 points
summary(mu)

# from equation (10)
pia1_2 = 0 # profit firm 1 at state 2
pia2_2 = 0 # profit firm 2 at state 2
pia1_1 = mu*gamma/4 # profit at state 1
pia2_1 = (1-mu)*gamma/4
pia1_0 = mu*gamma*mu/((2*mu+1)^2) # profit at state 0
pia2_0 = (1-mu)*gamma*mu/((2*mu+1)^2)

# expected profit from A1 and A2 (partial ownership of firm 1 and 2, equation (11))
epia1 = phi2*pia1_2 + phi1*pia1_1 + phi0*pia1_0 # exp profit from partially (mu) owning firm 1
epia2 = phi2*pia2_2 + phi1*pia2_1 + phi0*pia2_0 # exp profit from partially (1-mu) owning firm 2

# the corresponding variances, equation (12)
var_pia1 = phi2*(pia1_2-epia1)^2 + phi1*(pia1_1-epia1)^2 + phi1*(0-epia1)^2 + phi0*(pia1_0-epia1)^2
var_pia2 = phi2*(pia2_2-epia2)^2 + phi1*(pia2_1-epia2)^2 + phi1*(0-epia2)^2 + phi0*(pia2_0-epia2)^2

# Equation (13)
cov_pia = phi2*(pia1_2-epia1)*(pia2_2-epia2) + phi1*(pia1_1-epia1)*(0-epia2) + phi1*(0-epia1)*(pia2_1-epia2) + phi0*(pia1_0-epia1)*(pia2_0-epia2)

# computing portfolio weights for investor A with respec to asset 1, equation (14)
s = epia1/(epia1+epia2)

# computing investor A's portfolio variance, equation (15)
var_pia = s^2*var_pia1 + (1-s)^2*var_pia2 + 2*s*(1-s)*cov_pia

# plotting A's portfolio's variance as function of mu. Plotting Figure 3:
var_df = data.frame(mu, var_pia1, var_pia2, var_pia) # defining the data frame

var_plot = ggplot(var_df, aes(x = mu, y = var_pia))
var_plot = var_plot + theme_bw()
(var_plot = var_plot + geom_line(aes(y=var_pia), size = 1.01))
(var_plot = var_plot + geom_line(aes(y=var_pia1), linetype = "dashed", size = 1.01))
(var_plot = var_plot + geom_line(aes(y=var_pia2), linetype = "dotted", size = 1.01))
(var_plot = var_plot + scale_x_continuous(breaks = seq(0.5, 1, 0.05))) 
(var_plot = var_plot + theme(axis.text.x = element_text(color = "black", size = 10))) 
(var_plot = var_plot + theme(axis.text.y = element_text(color = "black", size = 10))) 
(var_plot = var_plot + labs(x = expression("Ownership share of main investor ("*mu*")"), y = "Investor's portfolio variances"))
(var_plot = var_plot + annotate("text", size = 10, x = 0.88, y = 7, parse = T, label = as.character(expression("Var["*pi[A]*"]"))))
(var_plot = var_plot + annotate("text", size = 10, x = 0.6, y = 7, parse = T, label = as.character(expression("Var["*pi[A*1]*"]"))))
(var_plot = var_plot + annotate("text", size = 10, x = 0.9, y = 1, parse = T, label = as.character(expression("Var["*pi[A*2]*"]"))))
(var_plot = var_plot + theme(axis.title.x = element_text(size = 30)))
(var_plot = var_plot + theme(axis.title.y = element_text(size = 30)))
(var_plot = var_plot + theme(axis.text.x = element_text(color = "black", size = 25)))
(var_plot = var_plot + theme(axis.text.y = element_text(color = "black", size = 25)))

###############
# Figure 5 Section 5 starts

mu = seq(0.5, 1, 0.005) #Horizontal axis
length(mu)# 101 points
summary(mu)

gamma = 64
omega = 0.5 # equal weights in social welfare

phi2 = 0.25 # original JEMS submission
phi1 = 0.25 # original JEMS submission
phi0 = 0.25 # original JEMS submission

theta_low = 0.4
theta_mid = 0.6
theta_hi = 0.8

theta = theta_hi #initial assignment for the highest graph
ew_hi = omega*( 2*phi1*(gamma/8)^theta + phi0*(2*gamma*mu^2/((2*mu+1)^2))^theta) + (1-omega)* (2*phi1*((mu*gamma/4)^theta + ((1-mu)*gamma/4)^theta)+ phi0*( (gamma*mu/((2*mu+1)^2))^theta + (gamma*mu/((2*mu+1)^2))^theta) )
theta = theta_mid #mid graph
ew_mid = omega*( 2*phi1*(gamma/8)^theta + phi0*(2*gamma*mu^2/((2*mu+1)^2))^theta) + (1-omega)* (2*phi1*((mu*gamma/4)^theta + ((1-mu)*gamma/4)^theta)+ phi0*( (gamma*mu/((2*mu+1)^2))^theta + (gamma*mu/((2*mu+1)^2))^theta) )
theta = theta_low # low graph
ew_low = omega*( 2*phi1*(gamma/8)^theta + phi0*(2*gamma*mu^2/((2*mu+1)^2))^theta) + (1-omega)* (2*phi1*((mu*gamma/4)^theta + ((1-mu)*gamma/4)^theta)+ phi0*( (gamma*mu/((2*mu+1)^2))^theta + (gamma*mu/((2*mu+1)^2))^theta) )

ew_df = data.frame(mu, ew_hi, ew_mid, ew_low) #creating data frame

# extracting maxima coordinates for the 3 graphs
(mu_hi = ew_df$mu[which.max(ew_df$ew_hi)])
(y_hi = ew_df$ew_hi[which.max(ew_df$ew_hi)])
(mu_mid = ew_df$mu[which.max(ew_df$ew_mid)])
(y_mid = ew_df$ew_mid[which.max(ew_df$ew_mid)])
(mu_low = ew_df$mu[which.max(ew_df$ew_low)])
(y_low = ew_df$ew_low[which.max(ew_df$ew_low)])

# Plotting Figure 5:
ew_plot = ggplot(ew_df, aes(mu))
ew_plot = ew_plot + theme_bw()
(ew_plot = ew_plot + geom_line(aes(y = ew_hi), size = 1.05))
(ew_plot = ew_plot + geom_line(aes(y = ew_mid), linetype = "solid", size = 1.05))
(ew_plot = ew_plot + geom_line(aes(y = ew_low), linetype = "solid", size = 1.05))
(ew_plot = ew_plot + geom_vline(xintercept = mu_hi, linetype = "dotted")) # max high curve
(ew_plot = ew_plot + geom_vline(xintercept = mu_mid, linetype = "dotted")) # max mid curve
(ew_plot = ew_plot + geom_vline(xintercept = mu_low, linetype = "dotted")) # max low curve
(ew_plot = ew_plot + annotate("point", x = mu_hi, y = y_hi, size = 4))
(ew_plot = ew_plot + annotate("point", x = mu_mid, y = y_mid, size = 4))
(ew_plot = ew_plot + annotate("point", x = mu_low, y = y_low, size = 4))
(ew_plot = ew_plot + annotate("text", size = 10, x = mu_hi, y = y_hi+0.2, parse = T, label = as.character(expression("Maximum EW for"~theta~"= 0.8"))))
(ew_plot = ew_plot + annotate("text", size = 10, x = mu_mid, y = y_mid+0.2, parse = T, label = as.character(expression("Maximum EW for"~theta~"= 0.6"))))
(ew_plot = ew_plot + annotate("text", size = 10, x = mu_low, y = y_low+0.2, parse = T, label = as.character(expression("Maximum EW for"~theta~"= 0.4"))))
(ew_plot = ew_plot + labs(x = expression("Ownership share of main investor ("*mu*")"), y = "Total welfare (EW)"))
(ew_plot = ew_plot + theme(axis.title.x = element_text(size = 30)))
(ew_plot = ew_plot + theme(axis.title.y = element_text(size = 30)))
(ew_plot = ew_plot + scale_x_continuous(breaks = seq(0.5, 1, 0.05))) # adding ticks
(ew_plot = ew_plot + theme(axis.text.x = element_text(color = "black", size = 25)))
(ew_plot = ew_plot + theme(axis.text.y = element_text(color = "black", size = 25)))
