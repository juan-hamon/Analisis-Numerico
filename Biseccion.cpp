#include <iostream>

//Trabajo realizado por: Juan Francisco Hamón, Diego Mauricio Bulla y Juan Diego Campos

double funcion(double x);
double biseccion(double izquierda, double derecha, double error, int iteraciones);


int main(){
	int iteraciones = 0;
	double xIzquierda, xDerecha, valorAprox, error;
	
	std::cout<<"Ingrese el punto izquierdo del intervalo"<<std::endl;
	std::cin>>xIzquierda;
	std::cout<<"Ingrese el punto derecho del intervalo"<<std::endl;
	std::cin>>xDerecha;
	std::cout<<"Ingrese el error"<<std::endl;
	std::cin>>error;
	
	
	valorAprox = biseccion(xIzquierda, xDerecha, error, iteraciones);
	if(valorAprox != 0){
		std::cout<<"Valor aproximado de la raiz: "<<valorAprox<<std::endl;
	}else{
		std::cout<<"No se pudo encontrar la raiz"<<std::endl;
	}
	
	
	return 0;
	
}

double funcion(double x){
	return ((x*x) - 2);
}

double biseccion(double izquierda, double derecha, double error, int iteraciones){
	double valor_actual = 0;
	while((derecha - izquierda) > error){
		
		iteraciones++;
		if(funcion(izquierda)*funcion(derecha) < 0){
			valor_actual = (derecha + izquierda)/2;
			std::cout<<std::endl;
			std::cout<<"Valor actual "<<valor_actual<<std::endl;
			std::cout<<"Iteracion "<<iteraciones<<std::endl;
			std::cout<<std::endl;
		
			if(funcion(valor_actual) == 0){
				return 0;
			}else{
				if(funcion(valor_actual)*funcion(izquierda) > 0){
					izquierda = valor_actual;
				}else{
					derecha = valor_actual;
				}
			}
		}
		else{
			std::cout<<"Intervalo no valido"<<std::endl;
			return 0;
		}
		
	}
	return valor_actual;
}

