a <- 1
invoice <- 871 * 1.21
invoice
length(invoice)
is.numeric(invoice)
dado<-c(1,2,3,4,5,6)
dado<-1:6
length(dado)
is.vector(dado)
is.numeric(dado)

dado+2
dado*dado
dado%*%dado #matrix operation, no es elemento a elemento.
#Para operaciones matriciales siempre se pone %, si es una operación vectorial
#no se pone % y se hace elemento a elemento
dado+dado
dado%o%dado

int<-c(-1L,2L,4L)
int
typeof(int)
int<-c(-1,2,4)
int
typeof(int)
sqrt(2)
sqrt(2)^2-2
text<-c("hello","bye")
text
typeof(text)
c(TRUE,FALSE)
logic<-c(1>2,1<2)
logic
raw(3)

mano<-c("oros","bastos","espadas","copas")
typeof(mano)
mano[1]
mano2<-c("oros2","bastos2","espadas2","copas2")
matrix<-cbind(mano,mano2)
matrix
matrix2<-rbind(mano,mano2)
matrix2
dim(matrix)
matrix[1,2]
matrix[,1]
matrix[1,]
matrix[c(1,3),1]
matrix[which(matrix[,1]=="bastos"),1]
matrix[,1]=="bastos"
matrix[(matrix[,1]=="bastos"),1]
moneda<-c(0,1)

attributes(moneda)<-list("cara"=0,"cruz"=1)
attributes(moneda)

ar<-array(c(11:14,21:24,31:34),dim=c(2,2,3)) #3 dimensions
ar

gender<-factor(c(rep("M",6),rep("F",6)))
gender
gender2<-c(rep("M",6),rep("F",6))
gender2
as.numeric(gender)
as.character(gender)

p<-cbind(c("sota","caballo","rey"),c(10,11,12))
p
sum(p[,2])
p<-data.frame(c("sota","caballo","rey"),c(10,11,12))
p
sum(p[,2])
p<-data.frame(carta=c("sota","caballo","rey"),puntos=c(10,11,12))
p

p$carta
p$puntos
str(p)

list1<-list(a=100:130,b="R",c=list(TRUE,FALSE))
list1

   
baraja<-data.frame(carta=rep(c("as",as.character(seq(2,7)),"sota","caballo","rey"),4),
                   palo=rep(c("bastos","copas","espadas","oros"),each=10),
                   puntos=rep(c(11,0,10,0,0,0,0,2,3,4),4),stringsAsFactors = F)
    

?rep           
sum(baraja$puntos)

throw<-function(x){return(x[1])}
throw<-function(x){return(sample(x,1))}
a<-c(2,3,4)
throw(a)

throw<-function(x) {
  a<-sample(x,1)
  b<-2*a
  return(b)
}
throw(dado)

min(dado)
max(dado)

deal<-function(x){
  return(x[1,])
}
deal(baraja)

shuffle<-function(x){
  new_order<-sample(1:40,40)
  return(x[new_order,])
}
baraja2<-shuffle(baraja)
baraja2

shuffle2<-function(x){
  new_order<-sample(1:40,40)
  x2<-x[new_order,]
  return(x2[1,])
}
baraja3<-shuffle2(baraja)
baraja3

baraja[1,-1] #first line, everything except for the first column
baraja[1:10,]
baraja[1,c(T,F,T)]

baraja$carta[1]

dado
dado[1]<-2
dado
dado[1]<-"hola"
dado
baraja$puntos2<-1:40
head(baraja)
baraja$puntos2<-NULL
head(baraja)
baraja2<-baraja
baraja2$puntos[baraja2$carta=="As"]<-0
head(baraja2)
baraja2$puntos[c(1,11,21,31)]<-0
baraja2

"a" %in% c("a","b","c")
which(c("a","b","c")=="a")
which(baraja$carta=="as")
sum(baraja$carta=="as")
length(which(baraja$carta=="as"))

a<-c(1,2,3)
b<-c(1,2,3)
c<-c(1,2,4)
a==b
b==c
a==b & b==c

baraja$carta=="as" & baraja$palo=="oros"
which(baraja$carta=="as" & baraja$palo=="oros")

w <- c(-1, 0, 1)
x <- c(5, 15)
y <- "February"
z <- c("Monday", "Tuesday", "Friday")
w>0
which(w>0)
which(w>0 & w<200)
y=="February"
week<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
z %in% week
z == week

1+NA
NA==1
x<-c(1,2,3,NA)
x
is.na(x)
which(is.na(x))
mean(x)
mean(x, na.rm=TRUE)
?mean


