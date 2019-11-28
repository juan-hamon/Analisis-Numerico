#Remueve los elementos que estan guardados en memoria
rm(list=ls())
#Limpia la consola para una mejor visualización
cat("\014")

Horner = function(x0, coeficientes){
  
  resultado = 0
  
  for(i in 1:length(coeficientes)){
    
    resultado = resultado * x0 + coeficientes[i]
    
  }
  
  cat("Respuesta aproximada -> ", resultado, "\n")
  
}

#La función recibe el valor que se quiere evaluar y los coeficientes que hay en la función a evaluar
Horner(8, c(4,7,3,6,2))