#para poner un valor por defecto, basta poner = x.

salario <- function(costehora, horas = 40, retenciones = 0.02){
  if (horas > 45){
    horasextras = horas - 45
    costeextra = costehora * 1.5
    bruto = 45 * costehora + horasextras * costeextra
    
  } else {
    bruto = horas * costehora
  }
  
  neto = bruto * (1- retenciones)
  
  return(list("Salário Bruto" = bruto, "Salário Neto" = neto))
  
}

salario(20)

salario(20, 50, 0.20)


#### ejercicio 2

media <- function(x){
  resultado <- 0
  for(i in 1:length(x)){
    resultado <- resultado + x[i]
  }
  resultado / length(x)
}

media(1:11)


#while

media <- function(x){
  resultado <- 0
  i <- 1
  while(i <= length(x)){
    resultado <- resultado + x[i]
    i <- i + 1
  }
  resultado / length(x)
}

media(1:11)

##vectorización de operaciones

media <- function(x){
  sum(x) / length(x)
}

media(20:30)

