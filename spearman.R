# Spearman rank stuff
library(plotly)
library(dplyr)
library(ggplot2)
library(magrittr)

# some faffing around with pipe operators
iris %>% head
iris$Sepal.Length %<>% sqrt


static <- mtcars %>%
  ggplot(aes(x = qsec, y = disp, color = factor(gear))) +
  geom_point()

ggplotly(static)

 

# Plan: Create a plot that shows how correlation and p-values change with changing data.
# sine wave example from https://plot.ly/r/sliders/

x <- seq(0,10, length.out = 1000)

# create data
aval <- list()
for(step in 1:11){
  aval[[step]] <-list(visible = FALSE,
                      name = paste0('v = ', step),
                      x=x,
                      y=sin(step*x))
}
aval[3][[1]]$visible = TRUE

# create steps and plot all traces
steps <- list()
p <- plot_ly()
for (i in 1:11) {
  p <- add_lines(p,x=aval[i][[1]]$x,  y=aval[i][[1]]$y, visible = aval[i][[1]]$visible, 
                 name = aval[i][[1]]$name, type = 'scatter', mode = 'lines', hoverinfo = 'name', 
                 line=list(color='00CED1'), showlegend = FALSE)
  
  step <- list(args = list('visible', rep(FALSE, length(aval))),
               method = 'restyle')
  step$args[[2]][i] = TRUE  
  steps[[i]] = step 
}  

# add slider control to plot
p <- p %>%
  layout(sliders = list(list(active = 3,
                             currentvalue = list(prefix = "Frequency: "),
                             steps = steps)))

##########



# Idea for spearman: create slider to change your data type, e.g. the degree variance in the data
# Create data sets


set.seed(123)
x <- runif(100, min = 0, max = 1)
y <- x 
dat <- data.frame(x,y)

cor.test(dat$x, dat$y, method="s")

set.seed(123)
x <- runif(100, min = 0, max = 1)

# create data
aval <- list()
for(step in 1:90){
  scramble <- sample(c(1:100), step*0.01*length(x), replace = FALSE)
  temp <- data.frame(x=x, y=x)
  temp$y[scramble] %<>% runif
  aval[[step]] <-list(visible = FALSE,
                      name = paste0('v = ', step),
                      x=x,
                      y=temp$y,
                      rho=cor.test(x,temp$y,method="s")$estimate[[1]],
                      p=cor.test(x,temp$y,method="s")$p.[[1]]
                      )
}
aval[3][[1]]$visible = TRUE

# create steps and plot all traces
steps <- list()
p <- plot_ly(color = I("black"))
for (i in 1:9) {
    p <- add_markers(p, x=aval[i][[1]]$x,  y=aval[i][[1]]$y, name=aval[i][[1]]$name,
                     showlegend=FALSE, visible = aval[i][[1]]$visible)
    p <- add_annotations(p, text = round(aval[i][[1]]$rho,2), visible = aval[i][[1]]$visible)
  # p <- add_lines(p,x=aval[i][[1]]$x,  y=aval[i][[1]]$y, visible = aval[i][[1]]$visible, 
  #                name = aval[i][[1]]$name, type = 'scatter', mode = 'points', hoverinfo = 'name', 
  #                line=list(color='00CED1'), showlegend = FALSE)
  
  step <- list(args = list('visible', rep(FALSE, length(aval))),
               method = 'restyle')
  step$args[[2]][i] = TRUE  
  steps[[i]] = step 
}  

# add slider control to plot
p <- p %>%
  layout(sliders = list(list(active = 3,
                             currentvalue = list(prefix = "Frequency: "),
                             steps = steps)))
p







###############################
### manual ####
x <- runif(100, min = 0, max = 1)
start <- 10
end <- 95

# create data
aval <- list()
for(step in start:end){
  scramble <- sample(c(1:100), step*0.01*length(x), replace = FALSE)
  temp <- data.frame(x=x, y=x)
  temp$y[scramble] %<>% runif
  aval[[step]] <-list(visible = FALSE,
                      name = paste0('v = ', step),
                      x=x,
                      y=temp$y,
                      rho=cor.test(x,temp$y,method="s")$estimate[[1]],
                      p=cor.test(x,temp$y,method="s")$p.[[1]]
  )
}
# sample size=100

groups <- length(seq(start, end))

dat <- data.frame(x=rep(NA, groups*100), y=rep(NA, groups*100), deg=rep(NA, groups*100))
for(i in start:end){
  if(i==start){
    dat <- data.frame(x=aval[i][[1]]$x, y=aval[i][[1]]$y, random=rep(i, 100))
  }else{
    temp <- data.frame(x=aval[i][[1]]$x, y=aval[i][[1]]$y, random=rep(i, 100))
    dat <- rbind(dat, temp)
  }
}

rho <- c()
p <- c()
scrambling <- c()
for(i in start:end){
  rho <- c(rho, cor.test(dat$x[dat$random==i],dat$y[dat$random==i],
                         method="s")$estimate[[1]])
  p <- c(p, cor.test(dat$x[dat$random==i],dat$y[dat$random==i],
                     method="s")$p.[[1]])
  scrambling <- c(scrambling, i)
}
plot(rho, p, col=scrambling)


## smaller sample sizes
#samped_data <- dat %>% group_by(random) %>% sample_vals(size = 10) %>% ungroup()
samples_per_group <- rev(seq(start, end, 5))

samplesize <- c()
rho <- c()
p.value <- c()
scrambling <- c()
for(j in 1:length(samples_per_group)){
  sub <- dat %>%
    group_by(random) %>%
    slice(sample(n(), min(samples_per_group[j], n()))) %>%
    ungroup()
  rho.sub <- c()
  p.sub <- c()
  for(i in start:end){
    rho <- c(rho, cor.test(sub$x[sub$random==i],sub$y[sub$random==i],
                                   method="s")$estimate[[1]])
    p.value <- c(p.value, cor.test(sub$x[sub$random==i],sub$y[sub$random==i],
                               method="s")$p.[[1]])
    scrambling <- c(scrambling, i)
    samplesize <- c(samplesize, samples_per_group[j])
  }  
}
all <- data.frame(rho=rho, p.value=p.value, randomization=scrambling, samplesize=samplesize)

theme_set(theme_bw())

allplot <- all %>%
  ggplot(aes(x=rho, y=p.value, col=randomization))+
  geom_point()+
  geom_hline(yintercept=0.05, col="red")+
  geom_vline(xintercept=0, col="black")+
  facet_wrap(~samplesize, ncol=6)
ggplotly(allplot)




# ggplot(all, aes(x=rho, y=p.value, col=randomization))+
#   geom_point()+
#   geom_hline(yintercept=0.05, col="red")+
#   geom_vline(xintercept=0, col="black")

save.image("spearman.RData")


