library(pracma)
library(Matrix)

#Se limpian los elementos creados con anterioridad
rm(list=ls())
#Se limpia la consola para una mejor visualización 
cat("\014")

#Taller realizado por: Juan Francisco Hamón, Diego Mauricio Bulla y Juan Diego Campos

#---------------------------------------------------------------------------------------------------
#Matriz del libro (pag 142)

M = matrix(c(8,9,2,2,7,2,2,8,6), nrow = 3, ncol = 3, byrow = TRUE)
B = c(69,47,68)


#Matriz diagonal inferior
L = tril(M,k=-1,diag = FALSE)

#Matriz diagonal superior
U = triu(M,k=1,diag = FALSE)

#Matriz de la diagonal de M
D = diag(diag(M))

#Matriz identidad
I = diag(1,nrow = nrow(M))

#Matriz inversa de la diagonal de M
D1 = solve(D,I)

#Solucion normal
Normal = solve(M,B)
cat("Normal","\n")
print(Normal)

#Solucion por Jacobi
cat("\n","Jacobi","\n")
Jacob = itersolve(M,B, method = "Jacobi")
print(Jacob)

#Encontrar la matriz de transicion para Jacobi

Tja = (-D1)%*%(L+U)

normaJa = norm(Tja,type=c("I"))

if(normaJa > 1){
  cat("no converge con Jacobi","\n")
}else if(normaJa < 1){
  cat("El metodo de Jacobi converge","\n")
}


#Solucion por Gauss-Seidel
cat("\n","Gauss-Seidel","\n")
GS = itersolve(M,B, method = "Gauss-Seidel")
print(GS)

#Encontrar la matriz de transiscion para Gauss-Seidel

TGs = (D1%*%U) + (I + (L%*%D1))

normaGs = norm(TGs,type=c("I"))

if(normaGs > 1){
  cat("no converge con Gauss-Seidel","\n")
}else if(normaGs < 1){
  cat("El metodo de Gauss-Seidel converge","\n")
}

#---------------------------------------------------------------------------------------------------
#Matriz con valores aleatorios del 1 al 20

cat("\n")

n=5;
A = matrix(c(0) ,nrow = n, ncol = n, byrow = TRUE)
c = 0

while(c <= 1000){
  for(i in 1:n){
    for(j in 1:n){
      A[i,j] = sample(0:20, 1, replace = F)
    }
    c= 1/rcond(A)
  }
}

B = c(1,2,3,4,5)

#Matriz diagonal inferior
L = tril(A,k=-1,diag = FALSE)

#Matriz diagonal superior
U = triu(A,k=1,diag = FALSE)

#Matriz de la diagonal de A
D = diag(diag(A))

#Matriz identidad
I = diag(1,nrow = nrow(A))

#Matriz inversa de la diagonal de A
D1 = solve(D,I)

#Solucion normal
Normal = solve(A,B)
cat("Normal","\n")
print(Normal)

#Solucion por Jacobi
cat("\n","Jacobi","\n")
Jacob = itersolve(A,B, method = "Jacobi")
print(Jacob)

#Encontrar la matriz de transicion para Jacobi

Tja = (-D1)%*%(L+U)

normaJa = norm(Tja,type=c("I"))

if(normaJa > 1){
  cat("no converge con Jacobi","\n")
}else if(normaJa < 1){
  cat("El metodo de Jacobi converge","\n")
}


#Solucion por Gauss-Seidel
cat("\n","Gauss-Seidel","\n")
GS = itersolve(A,B, method = "Gauss-Seidel")
print(GS)

#Encontrar la matriz de transiscion para Gauss-Seidel

TGs = (D1%*%U) + (I + (L%*%D1))

normaGs = norm(TGs,type=c("I"))

if(normaGs > 1){
  cat("no converge con Gauss-Seidel","\n")
}else if(normaGs < 1){
  cat("El metodo de Gauss-Seidel converge","\n")
}

#-----------------------------------------------------------------------------------------------------

#Modificacion de la matriz de numeros aleatorios

cat("\n")

A[1,3] = A[1,3] + 0.01

print(A)

cat("\n")

#Solucion normal
Normal = solve(A,B)
cat("Normal","\n")
print(Normal)

#Solucion por Jacobi
cat("\n","Jacobi","\n")
Jacob = itersolve(A,B, method = "Jacobi")
print(Jacob)

#Encontrar la matriz de transicion para Jacobi

Tja = (-D1)%*%(L+U)

normaJa = norm(Tja,type=c("I"))

if(normaJa > 1){
  cat("no converge con Jacobi","\n")
}else if(normaJa < 1){
  cat("El metodo de Jacobi converge","\n")
}

#Solucion por Gauss-Seidel
cat("\n","Gauss-Seidel","\n")
GS = itersolve(A,B, method = "Gauss-Seidel")
print(GS)


#Encontrar la matriz de transiscion para Gauss-Seidel

TGs = (D1%*%U) + (I + (L%*%D1))

normaGs = norm(TGs,type=c("I"))

if(normaGs > 1){
  cat("no converge con Gauss-Seidel","\n")
}else if(normaGs < 1){
  cat("El metodo de Gauss-Seidel converge","\n")
}
