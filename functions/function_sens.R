## (2) Threshold analysis under diagnostic uncertainty #######################
## One-way sensitivity analysis

# The probability of PE for the range 0.00 to 0.10
p.PE_range <- seq(0.00, 0.10, by=0.01)
p.PE_range
# Generate matrix of inputs for decision tree
input_matrix <- cbind(p.PE = p.PE_range, input[-1])
input_matrix

# Run model and generate a matrix of outputs
outputs <- apply(input_matrix, 1, getEV)
outputs

outputs_AC <- unlist(outputs)[attr(unlist(outputs),"names")=="AC"]
outputs_noAC <- unlist(outputs)[attr(unlist(outputs),"names")=="noAC"]

# Generate 1-way sensitivity table
tbl_sens_1way <- data.frame(
  p.PE = p.PE_range,
  AC = outputs_AC,
  noAC = outputs_noAC
)

tbl_sens_1way

# Find optimal strategy with highest EV
tbl_sens_1way$maxEV = pmax(outputs_AC, outputs_noAC)
tbl_sens_1way$strategy = factor(max.col(cbind(outputs_AC, outputs_noAC)), labels = c("AC", "noAC"))
tbl_sens_1way

# Generate treatment threshold plot with 1-way sensitivity
plot(p.PE_range, outputs_AC, type = "l",
     xlim = c(0, 0.1), ylim=c(min(c(outputs_AC, outputs_noAC)), 1),
     xlab = "p.PE", ylab = "Expected Value")
lines(p.PE_range, outputs_noAC, col = "red")
legend("bottomleft", c("AC", "noAC"), col = 1:2, lty = c(1, 1), bty = "n")
title(main = "One-way Sensitivity Analysis", 
      font.main = 4)

## (3) Threshold analysis under diagnostic + treatment uncertainty ##########
## Two-way sensitivity analysis

## Generate 100 combinations between two different parameters
p.PE_range <- seq(0, 1, length.out = 100)
p.h_range <- seq(0, 0.24, length.out = 100)
params <- expand.grid(p.PE = p.PE_range,
                      p.h = p.h_range)
params

# Generate matrix of inputs for decision tree
input_matrix <- cbind(params, input[-c(1,4)])
head(input_matrix)

# Run model and generate a matrix of outputs
outputs <- apply(input_matrix, 1, getEV)
outputs_AC <- unlist(outputs)[attr(unlist(outputs),"names")=="AC"]
outputs_noAC <- unlist(outputs)[attr(unlist(outputs),"names")=="noAC"]

# Generate 2-way sensitivity table
tbl_sens_2way <- data.frame(
  p.PE <- params$p.PE,
  p.h <- params$p.h,
  AC <- outputs_AC,
  noAC <- outputs_noAC
)

tbl_sens_2way

# Find optimal strategy with highest EV
tbl_sens_2way$maxEV = pmax(outputs_AC, outputs_noAC)
tbl_sens_2way$strategy = factor(max.col(cbind(outputs_AC, outputs_noAC)), 
                                labels = c("AC", "noAC"))
tbl_sens_2way

strategy <- max.col(cbind(outputs_AC, outputs_noAC))

# Generate treatment threshold plot with 2-way sensitivity
image(p.PE_range, p.h_range, matrix(strategy, ncol=100), 
      xlab = "p.PE", ylab = "p.h",
      col=c(3,2))
legend(grconvertX(0.5, "device"), grconvertY(1, "device"), 
       legend=c("AC", "noAC"), fill=c(3,2), 
       xpd=TRUE, horiz=TRUE, bty="n")
title(main = "Two-way Sensitivity Analysis", 
      font.main = 4)

## (4) Multiple sensitivity analyses #########################################
## Tornado diagrams

## Define parameter ranges (base case, min, max)
p.PE_range <- c(0.19, 0, 1)
p.h_range <- c(0.008, 0, 0.24)

## Parameter names
paramNames <-  c( "p.PE",
                  "p.h")

## List of inputs
l.tor.in <- vector("list", 2)
names(l.tor.in) <- paramNames
l.tor.in$p.PE <- cbind(p.PE = p.PE_range, input[-1])
l.tor.in$p.h <- cbind(p.h = p.h_range, input[-4])

## List of outputs
l.tor.out <- vector("list", 2)
names(l.tor.out) <- paramNames
## Run model on different parameters
l.tor.out$p.PE <- apply(l.tor.in$p.PE, 1, getEV)
l.tor.out$p.h <- apply(l.tor.in$p.h, 1, getEV)

AC_p.PE <- unlist(l.tor.out$p.PE)[attr(unlist(l.tor.out$p.PE),"names")=="AC"]
AC_p.h <- unlist(l.tor.out$p.h)[attr(unlist(l.tor.out$p.h),"names")=="AC"]

## Data structure: ymean	ymin	ymax
m.tor <- matrix(rbind(AC_p.PE, AC_p.h), nrow=2,
                dimnames =list(paramNames,c("basecase", "low", "high"	)) )
## Plot tornado
## Function for tornado plot
TornadoPlot <-function(Parms, Outcomes, titleName, outcomeName){
  library(ggplot2)
  library(reshape2)
  library(scales)
  
  # Grouped Bar Plot
  # Determine the overall optimal strategy
  paramNames2 <- Parms
  
  # Combine the parameter list with the data
  ymean <- Outcomes[1,1]
  
  yMin <- Outcomes[,2] - ymean
  yMax <- Outcomes[,3] - ymean
  ySize <- abs(yMax - yMin)  #High value - Low value
  
  
  rankY<- order(ySize)
  nParams <- length(paramNames2)
  
  Tor <- data.frame(
    Parameter=c(paramNames2[rankY],paramNames2[rankY]),  
    Level=c(rep("Low",nParams),rep("High",nParams)),
    value=ymean+c(yMin[rankY],yMax[rankY]),
    sort=seq(1,nParams)
  )
  
  #re-order the levels in the order of appearance in the data.frame
  Tor$Parameter2 <- ordered(Tor$Parameter, Tor$Parameter[1:(length(Tor$Parameter)/2)])
  # Tor$Parameter2 <- factor(Tor$Parameter, as.character(Tor$Parameter))
  #Define offset as a new axis transformation. Source: http://blog.ggplot2.org/post/25938265813/defining-a-new-transformation-for-ggplot2-scales  
  offset_trans <- function(offset=0) {
    trans_new(paste0("offset-", format(offset)), function(x) x-offset, function(x) x+offset)
  }
  #Plot the Tornado diagram.
  txtsize<-12
  print(
    ggplot(Tor[Tor$Level=="Low",], aes(x=Parameter2,y=value, fill=level)) +
      geom_bar(stat="identity", fill="blue") +
      ggtitle("Tornado Plot", subtitle = outcomeName) +
      scale_fill_discrete("Parameter Level: ", l=50)+
      scale_y_continuous(name="Probability of survival", trans=offset_trans(offset=ymean)) +
      scale_x_discrete(name="Parameter") +
      geom_bar(data=Tor[Tor$Level=="High",], aes(x=Parameter2,y=value, fill=level), stat="identity", fill="red", alpha=0.5) +
      geom_hline(yintercept = ymean, linetype = "dotted", size=0.5) +
      theme_bw(base_size = 14) +
      coord_flip() +
      theme(legend.position="bottom",
            legend.title=element_text(size = txtsize,angle = 0, hjust = 1),
            legend.key = element_rect(colour = "black"),
            legend.text = element_text(size = txtsize),
            title = element_text(face="bold", size=15),
            axis.title.x = element_text(face="bold", size=txtsize),
            axis.title.y = element_text(face="bold", size=txtsize),
            axis.text.y = element_text(size=txtsize),
            axis.text.x = element_text(size=txtsize),
            axis.ticks.y = element_blank())
  )
  # ggsave(paste("results/", titleName,".png"))
}

TornadoPlot(Parms = paramNames, Outcomes = m.tor, 
            titleName = "Tornado Plot", outcomeName = "AC treatment")