
###################################################################################
################ DEFINICIÓN DE LOS ELEMENTOS PRINCIPALES DEL JUEGO ################
###################################################################################

###### 1) PARÁMETROS Y ESTRATEGIAS:

# Datos del defensor:
cte_medidas <- 10 # Coste medidas ciberseguridad en millones de euros
cteTBcomp <- 2 # Coste por TB comprometido en millones de euros

d1_values <- c(0,1) # Vectores de estrategias para cada decisión
d2_values <- c(0,1)

# Datos del atacante:
cte_ataque <- 15 # Coste de lanzar un ataque en millones de euros
bfoTBcomp <- 2 # Beneficio por TB comprometido en millones de euros 

a1_values <- c(0,0.5,1) # Vectores de estrategias para cada decisión
a2_values <- c(0,0.5,1)


###### 2) MODELADO DE LAS VARIBLES ALEATORIAS S1 Y S2:

# Determinar parámetros de las distribuciónes de S1 y S2 según las decisiones de
# los jugadores.

sdlog <- 0.5

meanlogS1 <- function(d1,a1){
  if (d1 == d1_values[1] && a1 == a1_values[1]) {
    return(0)
  } else if (d1 == d1_values[2] && a1 == a1_values[1]) {
    return(0)
  } else if (d1 == d1_values[1] && a1 == a1_values[2]) {
    return(log(20))
  } else if (d1 == d1_values[1] && a1 == a1_values[3]) {
    return(log(25))
  } else if (d1 == d1_values[2] && a1 == a1_values[2]) {
    return(log(15))
  } else if (d1 == d1_values[2] && a1 == a1_values[3]) {
    return(log(5))
  }
}

meanlogS2 <- function(a1,d2,a2){ # Depende de la primera decisión del atacante
  if (a1 == a1_values[1]) {
    return(meanlogS1(d2,a2)) # Esta manera de definir la media de S2 supone que d1_values = d2_values
  }
  else {
    return(log(exp(meanlogS1(d2,a2))*0.5))
  }
}


###### 3) FUNCIONES DE UTILIDAD:

# Interceptos calculados con el percentil 80
intD2 <- -170 
intA2 <- -30

# Coeficientes de aversión al riesgo
roD <- 0.01  
roA <- 0.015 

# Funciones de utilidad
uD <- function(d1,d2,s1,s2){
  utility = 1 - exp(-roD * (-cte_medidas * (d1 + d2) - cteTBcomp * (s1 + s2)
                            - (intD2)))
  return(utility)
}

uA <- function(a1,a2,s1,s2){
  utility = 1 - exp(-roA * (-cte_ataque * (a1 + a2) + bfoTBcomp * (s1 + s2)
                            - (intA2)))
  return(utility)
}


###################################################################################
###################### ALGORITMO DE RESOLUCIÓN MEDIANTE TTJJ ######################
###################################################################################


#### 1) Cálculo de d1 óptima (resolución de {D1})
#### NOTA: Antes de ejecutar este paso es necesario ejecutar los pasos 2) a 6)

n <- 300 # Tamaño de Montecarlo para las muestras de S1 y S2

# Pagos de D en S1 según el valor de d1
pago_d1_0 <- pago_S1(d1_values[1], opt_a1(d1_values[1], n), n)[1]
pago_d1_1 <- pago_S1(d1_values[2], opt_a1(d1_values[2], n), n)[1]

# Cálculo de d1 óptima
if (pago_d1_0 >= pago_d1_1) {
  opt_d1 <- d1_values[1]
} else {
  opt_d1 <- d1_values[2]
}
print(paste("d1 óptima:", opt_d1))

#### 2) Función para calcular a1 óptima (resolución de {A1})
opt_a1 <- function(d1,n){
  pagos <- sapply(a1_values, function(a1) pago_S1(d1, a1, n)[2])
  opt_a1 <- a1_values[which.max(pagos)]
  return(opt_a1)
}

#### 3) Función para calcular los pagos de D y A en S1
#### Se calcula mediante simulación de Montecarlo
pago_S1 <- function(d1,a1,n){
  pagoD <- 0
  pagoA <- 0
  muestraS1 <- rlnorm(n, meanlog = meanlogS1(d1,a1), sdlog)
  for (i in 1:n){
    opt_d2_a2_result <- opt_d2_a2(muestraS1[i],d1,a1,n)
    pagoD <- pagoD + opt_d2_a2_result[[2]]
    pagoA <- pagoA + opt_d2_a2_result[[4]]
  }
  pagoD <- pagoD/n
  pagoA <- pagoA/n
  return(c(pagoD,pagoA))
}

#### 4) Función para calcular los pagos de D y A en S2
#### Se calcula mediante muestreo de Montecarlo
pago_s2 <- function(s1,d1,a1,d2,a2,n){
  pagoD <- 0
  pagoA <- 0
  muestraS2 <- rlnorm(n, meanlog = meanlogS2(a1,d2,a2), sdlog)
  for (i in 1:n){
    pagoD <- pagoD + uD(d1,d2,s1,muestraS2[i])
    pagoA <- pagoA + uA(a1,a2,s1,muestraS2[i])
  }
  pagoD <- pagoD/n
  pagoA <- pagoA/n
  return(c(pagoD,pagoA))
}

#### 5) Función para calcular d2 y a2 óptimas (resolución de {D2} y {A2})

opt_d2_a2 <- function(s1,d1,a1,n){
  
  # 5.1) Cálculo de las matrices de pagos
  MD <- matrix(0, nrow = length(d1_values), ncol = length(a1_values))
  MA <- matrix(0, nrow = length(d1_values), ncol = length(a1_values))
  for (i in 1:length(d2_values)){
    for (j in 1:length(a2_values)){
      pagos <- pago_s2(s1,d1,a1,d2_values[i],a2_values[j],n)
      MD[i,j] <- pagos[1]
      MA[i,j] <- pagos[2]
    }
  }
  MP <- list(MD = MD, MA = MA)
  
  # 5.2) Cálculo de los equilibrios de Nash
  equilibrios_nash <- eqNash(MP)
  d2_probs <- rep(0,length(d2_values))
  a2_probs <- rep(0,length(a2_values))
  pagoD <- 0
  pagoA <- 0
  
  # Mostrar matrices de pagos
  # print("Matrices de pagos:")
  # print(MP)
  
  if (length(equilibrios_nash$Pure) > 0){
    # 5.3.1) Cálculo de vectores de probabilidad y pagos en caso de que exista
    # un equilibrio en estrategias puras.
    
    # VECTORES DE PROBABILIDAD:
    d2_index <- match(equilibrios_nash$Pure[[1]][1], d2_values)
    a2_index <- match(equilibrios_nash$Pure[[1]][2], a2_values)
    d2_probs[d2_index] <- 1
    a2_probs[a2_index] <- 1
    
    # PAGOS:
    pagoD <- MD[d2_index,a2_index]
    pagoA <- MA[d2_index,a2_index]
    
    # Mostrar índices y pagos en caso de equilibrio puro
    # print("Equilibrio puro encontrado:")
    # print(paste("d2_index:", d2_index, "a2_index:", a2_index))
    # print(paste("pagoD:", pagoD, "pagoA:", pagoA))
    
  } else {
    # 5.3.2) Cálculo de vectores de probabilidad y pagos en caso de que exista
    # un equilibrio en estrategias mixtas.
    
    # VECTORES:
    estrategiasD <- equilibrios_nash$Mixed[[1]]$estrategiasD
    pD <- equilibrios_nash$Mixed[[1]]$pD
    estrategiasA <- equilibrios_nash$Mixed[[1]]$estrategiasA
    pA <- equilibrios_nash$Mixed[[1]]$pA
    
    # Mostrar estrategias y probabilidades en caso de equilibrio mixto
    # print("Estrategias mixtas encontradas:")
    # print(paste("estrategiasD:", estrategiasD))
    # print(paste("pD:", pD))
    # print(paste("estrategiasA:", estrategiasA))
    # print(paste("pA:", pA))
    
    # PAGOS: 
    # 1. Asignación de probabilidades a las estrategias:
    for (i in 1:length(estrategiasD)) {
      d2_index <- match(estrategiasD[i], d2_values)
      d2_probs[d2_index] <- pD[i]
    }
    
    for (i in 1:length(estrategiasA)) {
      a2_index <- match(estrategiasA[i], a2_values)
      a2_probs[a2_index] <- pA[i]
    }
    
    # 2. Cálculo de pagos esperados:
    for (i in 1:length(d2_values)){
      for (j in 1:length(a2_values)){
        pagoD <- pagoD + d2_probs[i]*a2_probs[j]*MD[i,j]
        pagoA <- pagoA + d2_probs[i]*a2_probs[j]*MA[i,j]
      }
    }
    
    # Mostrar pagos esperados en caso de equilibrio mixto
    # print(paste("pagoD esperado:", pagoD))
    # print(paste("pagoA esperado:", pagoA))
    
  }
  return(list(d2_probs, pagoD, a2_probs, pagoA))
}

#### 6) Funciones para el cálculo de equilibrios de Nash
######## 6.1) Dominación: función para eliminar filas y columnas dominadas

mat_sin_dom <- function(MP){
  
  # 1) Función para eliminar filas dominadas de una matriz.
  elim_filas_dominadas <- function(M){
    dominadas <- rep(FALSE,nrow(M))
    for (i in 1:nrow(M)){
      for (j in 1:nrow(M)){
        if (i!=j && all(M[i,] <= M[j,])){
          dominadas[i] <- TRUE
        }
      }
    }
    M <- M[!dominadas, , drop=FALSE]
    return(list(M=M, dominadas=dominadas))
  }
  
  # 2) Eliminación de estrategias dominadas en las matrices de pagos
  dom <- TRUE
  
  while(dom){ # Si se elimina alguna fila o alguna columna dom = TRUE y 
    # hay que comprobar que no se han generado nuevas dominaciones 
    dom <- FALSE
    numD <- nrow(MP[[1]])
    numA <- ncol(MP[[2]])
    rownamesD <- rownames(MP[[1]])
    colnamesA <- colnames(MP[[2]])
    
    # Eliminación en la matriz de pagos del defensor
    resultado_MD <- elim_filas_dominadas(MP[[1]])
    filas_eliminadas <- resultado_MD$dominadas
    MP[[1]] <- resultado_MD$M
    new_numD <- nrow(MP[[1]])
    rownames(MP[[1]]) <- rownamesD[!filas_eliminadas]
    
    # Eliminación en la matriz de pagos del atacante
    resultado_MA <- elim_filas_dominadas(t(MP[[2]][!filas_eliminadas, ,drop=FALSE]))
    columnas_eliminadas <- resultado_MA$dominadas
    MP[[2]] <- t(resultado_MA$M)
    new_numA <- ncol(MP[[2]])
    MP[[1]] <- MP[[1]][ ,!columnas_eliminadas, drop=FALSE]
    colnames(MP[[2]]) <- colnamesA[!columnas_eliminadas]
    
    if (new_numD < numD || new_numA < numA){
      if (dim(MP[[1]])[1] > 1 && dim(MP[[1]])[2] > 1 && dim(MP[[2]])[1] > 1 && dim(MP[[2]])[2] > 1){
        dom <- TRUE
      }
    }
  }
  
  return(MP)
}

######## 6.2) Equilibrios de Nash: función para encontrar equilibrios de Nash en estrategias puras y mixtas

library(gtools)

eqNash <- function(MP){
  rownames(MP[[1]]) <- d1_values
  colnames(MP[[2]]) <- a1_values
  MP <- mat_sin_dom(MP)
  numD <- nrow(MP[[1]])
  numA <- ncol(MP[[2]])
  
  #### Equilibrios de Nash en estrategias puras
  eqP <- list()
  numEq <- 0
  for (i in 1:numD){
    for (j in 1:numA){
      if (max(MP[[1]][,j]) <= MP[[1]][i,j] & max(MP[[2]][i,]) <= MP[[2]][i,j]){
        numEq <- numEq + 1
        eqP[[numEq]] <- c(rownames(MP[[1]])[i],colnames(MP[[2]])[j])
      }
    }
  }
  
  #### Equilibrios de Nash en estrategias mixtas
  eqM <- list()
  numEq <- 0
  
  if (length(eqP) != 1){ # Solo se calculan si no hay equilibrios puros
    
    ppo_indiferencia <- function(M,num){
      # Para la distribución del jugador 1 los pagos esperados del jugador
      # 2 son iguales.
      eq <- rep(1, num)
      for (j in 2:num){
        eq <- rbind(eq, M[,1]-M[,j])
      }
      ind <- c(1, rep(0,num-1))
      p <- solve(eq, ind)
      return(p)
    }
    
    comprobar_eqM <- function(p, M, num, var){
      for (j in (1:num)[!(1:num %in% var)]){ # Vector de índices que no están en var
        if (p%*%M[,j] > p%*%M[,var[1]]){ # Comprobar si hay alternativas mejores
          return(FALSE)
        }
      }
      return(TRUE)
    }
    
    if (numD < numA){ # A tiene más alternativas
      varA <- combinations(numA,numD) # Combinaciones de elementos de numA tomados de numD en numD
      for (i in 1:choose(numA,numD)){ # Iteramos sobre todas esas combinaciones
        # choose(numD,numA) es el número de combinaciones posibles
        redMPD <- MP[[1]][,varA[i,]] # Selecciona las columnas de MP$MD correspondientes a la combinación i
        redMPA <- MP[[2]][,varA[i,]] # Selecciona las columnas de MP$MA correspondientes a la combinación i
        redMP <- list(redMPD, redMPA)
        
        # En este momento hay que comprobar si eliminar columnas genera dominaciones nuevas en las estrategias de D
        red_sin_dom <- mat_sin_dom(redMP)
        redD <- red_sin_dom[[1]]
        redA <- red_sin_dom[[2]]
        if (nrow(redD) < nrow(redMPD) || ncol(redA) < ncol(redMPA)){
          next
        }      
        
        pD <- ppo_indiferencia(redMPA,numD)
        
        if (comprobar_eqM(pD, MP[[2]], numA, varA[i,])){ # Comprobar no hay alternativas mejores de A
          pA <- ppo_indiferencia(t(redMPD), numD) # Transponemos redMPD porque ppo_indiferencia opera por columnas
          # y para D queremos operar por filas
          numEq <- numEq + 1
          eqM[[numEq]] <- list()
          eqM[[numEq]]$estrategiasD <- rownames(MP[[1]])
          eqM[[numEq]]$pD <- pD
          eqM[[numEq]]$estrategiasA <- colnames(MP[[2]])[varA[i,]]
          eqM[[numEq]]$pA <- pA
        }
      }
    } else if (numD > numA){ # D tiene más alternativas
      varD <- combinations(numD,numA) # Genera todas las combinaciones de elementos de numD tomados de numA en numA
      for (i in 1:choose(numD,numA)){ # Iteramos sobre todas esas combinaciones
        # choose(numD,numA) es el número de combinaciones posibles
        redMPD <- MP[[1]][varD[i,],] # Selecciona las filas de MP$MD correspondientes a la combinación i
        redMPA <- MP[[2]][varD[i,],] # Selecciona las filas de MP$MA correspondientes a la combinación i
        redMP <- list(redMPD, redMPA)
        
        # En este momento hay que comprobar si eliminar columnas genera dominaciones nuevas en las estrategias de D
        red_sin_dom <- mat_sin_dom(redMP)
        redD <- red_sin_dom[[1]]
        redA <- red_sin_dom[[2]]
        if (nrow(redD) < nrow(redMPD) || ncol(redA) < ncol(redMPA)){
          next
        }
        
        pA <- ppo_indiferencia(t(redMPD),numA) # Transponemos redMPD porque ppo_indiferencia opera por columnas
        # y para D queremos operar por filas
        
        if (comprobar_eqM(pA, t(MP[[1]]), numD, varD[i,])){ # Comprobar no hay alternativas mejores de D
          pD <- ppo_indiferencia(redMPA,numA)
          numEq <- numEq + 1
          eqM[[numEq]] <- list()
          eqM[[numEq]]$estrategiasD <- rownames(MP[[1]])[varD[i,]]
          eqM[[numEq]]$pD <- pD
          eqM[[numEq]]$estrategiasA <- colnames(MP[[2]])
          eqM[[numEq]]$pA <- pA
        }
      }
      
    } else { # numD = numA
      pD <- ppo_indiferencia(MP[[2]], numD)
      pA <- ppo_indiferencia(t(MP[[1]]), numA)
      
      numEq <- numEq + 1
      eqM[[numEq]] <- list()
      eqM[[numEq]]$estrategiasD <- rownames(MP[[1]])
      eqM[[numEq]]$pD <- pD
      eqM[[numEq]]$estrategiasA <- colnames(MP[[2]])
      eqM[[numEq]]$pA <- pA
    } 
  }
  
  eqN <- list(Pure = eqP, Mixed = eqM)
  
  return(eqN) 
}


###################################################################################
################### COMPROBACIÓN DE LA ESTABILIDAD DEL PROBLEMA ###################
###################################################################################

# Crear un data.frame vacío para almacenar los resultados
resultados <- data.frame(semilla = integer(), d1_optimo = numeric(), a1_optimo_d1_0 = numeric(), a1_optimo_d1_1 = numeric())

# Resolver el juego para 50 semillas diferentes
for (i in 1:20) { 
  set.seed(i)
  n <- 300
  a1_opt_d1_0 <- opt_a1(d1_values[1], n)
  a1_opt_d1_1 <- opt_a1(d1_values[2], n)
  pago_d1_0 <- pago_S1(d1_values[1], a1_opt_d1_0, n)[1]
  pago_d1_1 <- pago_S1(d1_values[2], a1_opt_d1_1, n)[1]
  
  if (pago_d1_0 >= pago_d1_1) {
    opt_d1 <- d1_values[1]
  } else {
    opt_d1 <- d1_values[2]
  }
  
  # Guardar los resultados en el data.frame
  resultados <- rbind(resultados, data.frame(
    semilla = i,
    d1_optimo = opt_d1,
    a1_optimo_d1_0 = a1_opt_d1_0,
    a1_optimo_d1_1 = a1_opt_d1_1
  ))
}

# Frecuencia de óptimos:
table(resultados$d1_optimo)
table(resultados$a1_optimo_d1_0)
table(resultados$a1_optimo_d1_1)


###################################################################################
##################### RESOLUCIÓN DE UN EJEMPLO REPRESENTATIVO #####################
###################################################################################

# Decisiones óptimas de d1 y d2 obtenidas en la sección anterior
d1 <- 1
a1 <- 0.5

# La media de una log-normal es: e^{\mu + \frac{\sigma^2}{2}} 
s1 <- exp(meanlogS1(d1,a1) + (sdlog^2)/2)

# Cálculo de d2 óptima y a2 óptima -> antes de ejecutarlo hay que quitar los # que hay
# delante de los "print" de esta función para ver los resultados
opt_d2_a2(s1,d1,a1,n)

