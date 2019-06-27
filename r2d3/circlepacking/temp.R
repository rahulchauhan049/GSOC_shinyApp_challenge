a<- data.frame("a")
b<- data.frame(c("a","b","c","d"))
c<- data.frame(c("a","b","c","d","e","f","g","h"))
d<-c()
for(i in 1:nrow(b)){
  for(j in 1:nrow(c)){
    d<-append(d,paste(b[i,],c[j,],sep = "."))
  }
}
d<-as.data.frame(d)
d<-na.omit(d)
