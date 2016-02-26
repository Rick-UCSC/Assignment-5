install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(foreign)

RickShemanski_Assignment3 <- list(
  first =  "Rick",
  last = "Shemanski",
  email = "rshemans@ucsc.edu",
  studentid = 1504018
)


print(RickShemanski_Assignment3)



###     1
diamond_df <- diamonds

diamond_df <- diamond_df%>%
          mutate(
          volume = x * y *z
  
          )


p <- ggplot(diamond_df, 
       aes(x = volume, y = price, colour = clarity)
       
       )

p_point <- p + geom_point(aes(size = carat))
p_point <- p_point + scale_x_log10()
p_point <- p_point + scale_y_log10()

p_point

###
###2
summary(diamond_df$carat)

p2 <- ggplot(diamond_df,aes(x = carat))

p2 <- p2 + geom_histogram(binwidth = 0.2 ,aes(y = ..density.., fill = clarity))  

p2 <- p2 + facet_grid(cut ~.)

p2


###c

p3 <- ggplot(diamond_df, aes( x = cut,  y = price )) + geom_violin(size = 1)
p3 <- p3 + geom_jitter(alpha= 1/50, size = 1)
p3


###################################################
#### input data set here

#org_df1 <- read.dta("C:/Users/Rick/Documents/Econ 217 Metrics Alan/example data/org_example.dta")
#org_df <- sample_n(org_df, 50000)
org_df1 <- read.dta("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/org_example.dta")

org_df1 <- group_by(org_df1, year, month)%>%
  mutate(
    medianinc = median(rw, na.rm = TRUE),
    first = quantile(rw, probs = c(0.25), na.rm=T),
    third = quantile(rw, probs = c(0.75), na.rm=T),
    tenth = quantile(rw, probs = c(0.10), na.rm=T),
    ninety = quantile(rw, probs = c(0.90), na.rm=T),
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format= "%Y-%m-%d")
  )



p4 <- ggplot(org_df1, aes(x = date, y= medianinc))+ 
      geom_line() + lims(y = c(0,50)) +
      geom_ribbon(aes(ymin = first, ymax = third), alpha = 0.35) +
      geom_ribbon(aes(ymin = tenth, ymax = ninety), alpha = 0.1)

p4


print(length(org_df1$medianinc))
print(length(org_df1$first))

  

###############################################


org_educ <- group_by(org_df1, educ, date)%>%
  summarise(
    rw.median = median(rw, na.rm= T)
    
  )

p5 <- ggplot(org_educ, aes(x= date, y = rw.median)) +
      geom_line(aes(colour= educ))
p5


