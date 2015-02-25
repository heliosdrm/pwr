"ES.w2" <-
function(P){
pi<-apply(P,1,sum)
pj<-apply(P,2,sum)
P0<-pi%*%t(pj)
sqrt(sum((P-P0)^2/P0))
}

