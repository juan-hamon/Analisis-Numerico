#include <iostream>

//Trabajo realizado por: Juan Francisco Hamón, Diego Mauricio Bulla y Juan Diego Campos

double errorRelativo(double v, double et, double t, double ev);
double errorAbsoluto(double v, double et, double t, double ev);

int main(){

    double v = 0;
    double ev = 0;
    double et = 0;
    double t = 0;
    
    double rel = 0;
    double ed = 0;
    
    std::cout<<"Ingrese el valor de la velocidad"<<std::endl;
    std::cin>>v;
    std::cout<<"Ingrese el error de la medida"<<std::endl;
    std::cin>>ev;
    std::cout<<"Ingrese el valor del tiempo"<<std::endl;
    std::cin>>t;
    std::cout<<"Ingrese el error de la medida"<<std::endl;
    std::cin>>et;
    
    double d = (v*t);
    rel = errorRelativo(v,et,t,ev);
    ed = errorAbsoluto(v,et,t,ev);

	std::cout<<v<<" * "<<t<<" = "<<d<<std::endl;
    std::cout<<"error relativo = "<<rel<<"%"<<std::endl;
    std::cout<<"error absoluto = "<<ed<<std::endl;
}

double errorAbsoluto(double v, double et, double t, double ev){

    double ed;
    ed = (v*et)+(t*ev);

    return ed;
}

double errorRelativo(double v, double et, double t, double ev){

  double rel;
  rel = (ev/v)+(et/t);
  rel = rel * 100;

  return rel;
}
