#include <iostream>
#include <cmath>

//Trabajo realizado por: Juan Francisco Hamón, Diego Mauricio Bulla y Juan Diego Campos

double funcion(double n, double E, double x, double y);

int main(){
	double n = 0;
	double x = -1;
	double E, y;
	while(n <= 0){
		std::cout<<"Ingrese el dato"<<std::endl;
		std::cin>>n;
		if(n < 0){
			std::cout<<"Por favor ingrese un numero positivo"<<std::endl;
		}
	}
	std::cout<<"Ingrese el error"<<std::endl;
	std::cin>>E;
	while(x < 0){
		std::cout<<"Ingrese el valor inicial"<<std::endl;
		std::cin>>x;
		if(x == 0){
			std::cout<<"Ingrese un valor distinto de 0"<<std::endl;
		}
		else if(x < 0){
			std::cout<<"Ingrese un valor positivo"<<std::endl;
		}
	}
	y = (0.5) * (x + (n/x));
	y = funcion(n,E,x,y);
	std::cout<<"Valor encontrado: "<<y<<std::endl;
	std::cout<<"Validacion "<<y*y<<std::endl;
}

double funcion(double n, double E, double x, double y){
	while( abs(x - y) > E){
		x = y;
		y = (0.5) * (x + (n/x));
	}
	return y;
}
