#include <iostream>
#include <cmath>

//Trabajo realizado por: Juan Francisco Hamón, Diego Mauricio Bulla y Juan Diego Campos

double conversion1(double numero, int& conti);
double separacion(double numero, int posibles_cifras);

int main(){
	
	double numero = 0;
	double convertido = 0;
	double separado = 0;
	int posibles_cifras = 0;
	int cont = 0;
	
	std::cout<<"Ingrese el numero"<<std::endl;
	std::cin>>numero;
	
	std::cout<<"Ingrese las cifras que acepta el dispositivo"<<std::endl;
	std::cin>>posibles_cifras;
	
	convertido = conversion1(numero, cont);
	separado = separacion(convertido, posibles_cifras);
	
	double final = separado * pow(10,(cont-posibles_cifras));
	std::cout<<"Error de redondeo "<<final<<std::endl;
	
}

double conversion1(double numero, int& conti){
	int i = numero;
	int cont=0;
	while((i)!=0){
		i = (i/10);
		cont++;
	}
	numero = numero * pow(10,-cont);
	conti = cont;
	return numero;
}

double separacion(double numero, int posibles_cifras){
	int cont = 0;
	double final = numero;
	while(cont != posibles_cifras){
		final = final*10;
		cont++;
	}
	int i = final;
	final = final - i;
	return final;
}
