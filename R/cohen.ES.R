"cohen.ES" <-
function(test=c("p","t","r","anov","chisq","f2"),size=c("small","medium","large")){
test <- match.arg(test)
size <- match.arg(size)
ntest <- switch(test, p = 1, t = 2,r=3,anov=4,chisq=5,f2=6)
if(ntest==1){
ES<-switch(size,small=0.2,medium=0.5,large=0.8)
}
if(ntest==2){
ES<-switch(size,small=0.2,medium=0.5,large=0.8)
}
if(ntest==3){
ES<-switch(size,small=0.1,medium=0.3,large=0.5)
}
if(ntest==4){
ES<-switch(size,small=0.1,medium=0.25,large=0.4)
}
if(ntest==5){
ES<-switch(size,small=0.1,medium=0.3,large=0.5)
}
if(ntest==6){
ES<-switch(size,small=0.02,medium=0.15,large=0.35)
}

METHOD <- "Conventional effect size from Cohen (1982)"
structure(list(test = test,size=size,effect.size=ES,
method = METHOD), class = "power.htest")
}

