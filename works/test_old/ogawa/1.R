dir()

d<-read.csv("Matsuotoge2016.csv")

edit(d)  #


names(d) 


table(d$sp)


plot(d$x,d$y)
text(d$x,d$y,d$sp)