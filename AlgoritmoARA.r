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
bfoTBcomp <- 2 # Beneficio por TB comprometido en millones de euros (antes 4)

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
###################### ALGORITMO DE RESOLUCIÓN MEDIANTE ARA #######################
###################################################################################

############################## PROBLEMA DEL DEFENSOR ##############################

#### D1) Cálculo de d1 óptima (decisión D1)
#### NOTA: Antes de ejecutar este paso es necesario ejecutar los pasos D2) a D6) y A1) a A5)

M <- 100 # Número de veces que D resuelve el problema de A
n <- 200 # Tamaño de las muestras de S1 y S2
pago_d1_0 <- pago_d1(d1_values[1],M,n)
pago_d1_1 <- pago_d1(d1_values[2],M,n)

if (pago_d1_0 >= pago_d1_1) {
  opt_d1 <- d1_values[1]
} else {
  opt_d1 <- d1_values[2]
}
# print(paste("d1 óptima:", opt_d1))

#### D2) Función para calcular el pago en d1 (azar A1)
#### Pago esperado según el vector de probabilidades pD_a1
pago_d1 <- function (d1,M,n){
  pago <- 0
  pDa1 <- pD_a1(d1,M,n) # Probabilidades de a1 (calculado en A1.0)
  for (i in 1:length(a1_values)){
    pago <- pago + pDa1[i]*pagoD_S1(d1,a1_values[i],M,n)
  }
  return(pago)  
}

#### D3) Función para calcular el pago en S1 (azar S1) 
#### Se calcula mediante simulación de Montecarlo
pagoD_S1 <- function(d1,a1,M,n){
  pagoD <- 0
  muestraS1 <- rlnorm(n, meanlog = meanlogS1(d1,a1), sdlog)
  for (i in 1:n){
    pagoD <- pagoD + opt_d2(d1,a1,muestraS1[i],M,n)$pago
  }
  pagoD <- pagoD/n
  return(pagoD)
}

#### D4) Cálculo de d2 óptimo (decisión D2)
opt_d2 <- function(d1,a1,s1,M,n){
  pago_d2_0 <- pago_d2(d1,a1,s1,d2_values[1],M,n)
  pago_d2_1 <- pago_d2(d1,a1,s1,d2_values[2],M,n)
  
  opt <- list()
  if (pago_d2_0 >= pago_d2_1) {
    opt$d2 <- d2_values[1]
    opt$pago <- pago_d2_0
  } else {
    opt$d2 <- d2_values[2]
    opt$pago <- pago_d2_1
  }
  # print(paste("d2 óptima:", opt$d2, "con pago:", opt$pago))
  return(opt)
}

#### D5) Función para calcular el pago en d2 (azar A2)
#### Pago esperado según el vector de probabilidades pD_a2
pago_d2 <- function (d1,a1,s1,d2,M,n){
  pago <- 0
  pDa2 <- pD_a2(d1,a1,s1,M,n)
  for (i in 1:length(a2_values)){
    pago <- pago + pDa2[i]*pagoD_S2(d1,a1,s1,d2,a2_values[i],n)
  }
  return(pago)  
}

#### D6) Función para calcular el pago en S2 (azar S2)
#### Se calcula mediante simulación de Montecarlo
pagoD_S2 <- function(d1,a1,s1,d2,a2,n){
  pagoD <- 0
  muestraS2 <- rlnorm(n, meanlog = meanlogS2(a1,d2,a2), sdlog) 
  for (i in 1:n){
    pagoD <- pagoD + uD(d1,d2,s1,muestraS2[i])
  }
  pagoD <- pagoD/n
  return(pagoD)
}


############################## PROBLEMA DEL ATACANTE ##############################

#### A1.0) Función para calcular la distribución de probabilidad de A1 (vector pD_a1)
#### El defensor resuelve el problema de A M veces y asigna a cada estrategia a1 una
#### probabilidad igual a su frecuencia relativa.
pD_a1 <- function(d1,M,n){
  pD_a1 <- rep(0, length(a1_values))
  for (i in 1:M){
    opt_a1 <- opt_a1(d1,n)
    index <- match(opt_a1, a1_values)
    pD_a1[index] <- pD_a1[index] + 1
  }
  pD_a1 <- pD_a1/M
  # cat("pD_a1 con d1 = ", d1, " es: ", pD_a1, "\n")
  return(pD_a1)
}


### A1.1) Función para calcular opt_a1(d1) (decisión A1)
opt_a1 <- function(d1,n){
  pagos <- sapply(a1_values, function(a1) pagoA_S1(d1, a1, n))
  opt_a1 <- a1_values[which.max(pagos)]
  return(opt_a1)
}

#### A2) Función para calcular el pago en S1 (azar S1)
#### Puede haber incertidumbre de D (variando para cada M) => se introducen variaciones
#### en la media de S1 según una uniforme [-0.1, 0.1].
pagoA_S1 <- function(d1,a1,n){
  pagoA <- 0
  meanlogS1_var <- meanlogS1(d1,a1) + runif(1, -0.1, 0.1)
  muestraS1 <- rlnorm(n, meanlog = meanlogS1_var, sdlog) 
  for (i in 1:n){
    pagoA <- pagoA + opt_a2(d1,a1,muestraS1[i],n)$pago
  }
  pagoA <- pagoA/n
  return(pagoA)
}

#### A3.0) Función para calcular la distribución de probabilidad de A2 (vector pD_a2)
#### El defensor resuelve el problema de A M veces y asigna a cada estrategia a2 una
#### probabilidad igual a su frecuencia relativa.
pD_a2 <- function(d1,a1,s1,M,n){
  pD_a2 <- rep(0, length(a2_values))
  for (i in 1:M){
    opt_a2_resultado <- opt_a2(d1,a1,s1,n)
    opt_a2 <- opt_a2_resultado$a2
    index <- match(opt_a2, a2_values)
    pD_a2[index] <- pD_a2[index] + 1
  }
  pD_a2 <- pD_a2/M
  # cat("pD_a2 con d1 = ", d1, " y a1 = ",a1, " es: ", pD_a2, "\n")
  return(pD_a2)
}

#### A3) Cálculo de a2 óptimo (decisión A2)
opt_a2 <- function(d1,a1,s1,n){
  pagos <- sapply(a2_values, function(a2) pago_a2(d1, a1, s1, a2, n))
  opt <- list()
  opt$a2 <- a2_values[which.max(pagos)]
  opt$pago <- max(pagos)
  return(opt)
}

###############################################################################
################### Función que calcula d2 óptimo en TTJJ #####################

# Para calcular el pago en a2 (paso A4) el atacante debe conocer d2. Para no entrar en
# una recursividad infinita de resolución del problema del oponente se asume
# que el defensor elige d2 óptimo en TTJJ.

opt_d2_a2 <- function(s1,d1,a1,n){

  # 1) Cálculo de las matrices de pagos
  MD <- matrix(0, nrow = length(d1_values), ncol = length(a1_values))
  MA <- matrix(0, nrow = length(d1_values), ncol = length(a1_values))
  for (i in 1:length(d2_values)){
    for (j in 1:length(a2_values)){
      pagosD <- pagoD_S2(d1,a1,s1,d2_values[i],a2_values[j],n)
      pagosA <- pagoA_S2(d1,a1,s1,d2_values[i],a2_values[j],n)
      MD[i,j] <- pagosD
      MA[i,j] <- pagosA
    }
  }
  MP <- list(MD = MD, MA = MA) 
  
  # 2) Cálculo de los equilibrios de Nash
  equilibrios_nash <- eqNash(MP)
  d2_probs <- rep(0,length(d2_values))
  a2_probs <- rep(0,length(a2_values))
  pagoD <- 0
  pagoA <- 0
  
  if (length(equilibrios_nash$Pure) > 0){
    # 2.1) Cálculo de vectores de probabilidad y pagos en caso de que exista
    # un equilibrio en estrategias puras.

     # VECTORES DE PROBABILIDAD:
    d2_index <- match(equilibrios_nash$Pure[[1]][1], d2_values)
    a2_index <- match(equilibrios_nash$Pure[[1]][2], a2_values)
    d2_probs[d2_index] <- 1
    a2_probs[a2_index] <- 1

    # PAGOS:
    pagoD <- MD[d2_index,a2_index]
    pagoA <- MA[d2_index,a2_index]
    
  } else {
    # 2.2) Cálculo de vectores de probabilidad y pagos en caso de que exista
    # un equilibrio en estrategias mixtas.

    # VECTORES:
    estrategiasD <- equilibrios_nash$Mixed[[1]]$estrategiasD
    pD <- equilibrios_nash$Mixed[[1]]$pD
    estrategiasA <- equilibrios_nash$Mixed[[1]]$estrategiasA
    pA <- equilibrios_nash$Mixed[[1]]$pA
    
    # PAGOS:
    # 2.2.1) Asignación de probabilidades a las estrategias:
    for (i in 1:length(estrategiasD)) {
      d2_index <- match(estrategiasD[i], d2_values)
      d2_probs[d2_index] <- pD[i]
    }
    
    for (i in 1:length(estrategiasA)) {
      a2_index <- match(estrategiasA[i], a2_values)
      a2_probs[a2_index] <- pA[i]
    }

    # 2.2.2) Cálculo de pagos esperados:
    for (i in 1:length(d2_values)){
      for (j in 1:length(a2_values)){
        pagoD <- pagoD + d2_probs[i]*a2_probs[j]*MD[i,j]
        pagoA <- pagoA + d2_probs[i]*a2_probs[j]*MA[i,j]
      }
    }
  }
  return(list(d2_probs, pagoD, a2_probs, pagoA))
}


#### Función para el cálculo de equilibrios de Nash
######## Dominación: función para eliminar filas y columnas dominadas

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

######## Equilibrios de Nash: función para encontrar equilibrios de Nash en estrategias puras y mixtas

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
  
  if (length(eqP) != 1){ 
    
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

################################################################################
################################################################################

#### A4) Función para calcular el pago en a2 (azar D2)
pago_a2 <- function (d1,a1,s1,a2,n){
  pago <- 0
  pDd2 <- opt_d2_a2(s1,d1,a1,n)[[1]] # Es la función que calcula d2 óptimo en TTJJ
  for (i in 1:length(d2_values)){
    pago <- pago + pDd2[i]*pagoA_S2(d1,a1,s1,d2_values[i],a2,n)
  }
  return(pago)  
}

#### A5) Función para calcular el pago en S2 (azar S2)
#### #### Puede haber incertidumbre de D (variando para cada M) => se introducen variaciones
#### en la media de S2 y en los parámetros de la función de utilidad según una uniforme [-0.1, 0.1].
pagoA_S2 <- function(d1,a1,s1,d2,a2,n){
  pagoA <- 0
  meanlogS2_var <- meanlogS2(a1,d2,a2) + runif(1, -0.1, 0.1) # Variación media S2
  muestraS2 <- rlnorm(n, meanlog = meanlogS2_var, sdlog) 
  cte_ataque <- cte_ataque + runif(1, -0.1, 0.1) # Variación en los parámetros de A
  bfoTBcomp <- bfoTBcomp + runif(1, -0.1, 0.1) # Variación en los parámetros de A
  for (i in 1:n){
    pagoA <- pagoA + uA(a1,a2,s1,muestraS2[i])
  }
  pagoA <- pagoA/n
  return(pagoA)
}

###################################################################################
################### COMPROBACIÓN DE LA ESTABILIDAD DEL PROBLEMA ###################
###################################################################################

# Crear un data.frame vacío para almacenar los resultados
resultados <- data.frame(semilla = integer(), d1_optimo = numeric(), pDa1_d10 = I(list()), pDa1_d11 = I(list()))

for (i in 1:10){
  set.seed(i)
  print (paste("Resolviendo para la semilla ", i))
  n <- 200 
  M <- 100 
  pago_d1_0 <- pago_d1(d1_values[1], M, n) 
  pago_d1_1 <- pago_d1(d1_values[2],M,n)
  pDa1_d1_0 <- pD_a1(d1_values[1],M,n)
  pDa1_d1_1 <- pD_a1(d1_values[2],M,n)
  
  if (pago_d1_0 >= pago_d1_1) {
    opt_d1 <- d1_values[1]
  } else {
    opt_d1 <- d1_values[2]
  }
  
  # Almacenar los resultados en el data.frame
  resultados <- rbind(resultados, data.frame(
    semilla = i,
    d1_optimo = opt_d1,
    pDa1_d10 = I(list(pDa1_d1_0)),
    pDa1_d11 = I(list(pDa1_d1_1))
  ))
}

print(resultados)

# Frecuencia de d1 óptimo:
table(resultados$d1_optimo)

########## Estudio estadístico de pDa1

#### Para d1=0
# Para d1=1 no es necesario, ya que en ese caso, pDa1 = (0, 1, 0) siempre.
# 1) Extraer los valores de cada componente del vector pDa1_d10 en listas separadas
a1_0_vals   <- sapply(resultados$pDa1_d10, function(x) x[1])
a1_05_vals  <- sapply(resultados$pDa1_d10, function(x) x[2])
a1_1_vals   <- sapply(resultados$pDa1_d10, function(x) x[3])

# 2) Función para calcular la moda
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 3) Calcular la desviación estándar para cada valor de a1
sd_a1_0  <- sd(a1_0_vals)
sd_a1_05 <- sd(a1_05_vals)
sd_a1_1  <- sd(a1_1_vals)

# 4) Calcular estadísticos para cada valor de a1
cat("a1 = 0:\n")
cat("  Mínimo:", min(a1_0_vals), "\n")
cat("  Máximo:", max(a1_0_vals), "\n")
cat("  Media :", mean(a1_0_vals), "\n")
cat("  Moda  :", moda(a1_0_vals), "\n")
cat("Desviación estándar para a1 = 0   :", sd_a1_0, "\n\n")

cat("a1 = 0.5:\n")
cat("  Mínimo:", min(a1_05_vals), "\n")
cat("  Máximo:", max(a1_05_vals), "\n")
cat("  Media :", mean(a1_05_vals), "\n")
cat("  Moda  :", moda(a1_05_vals), "\n")
cat("Desviación estándar para a1 = 0.5 :", sd_a1_05, "\n\n")

cat("a1 = 1:\n")
cat("  Mínimo:", min(a1_1_vals), "\n")
cat("  Máximo:", max(a1_1_vals), "\n")
cat("  Media :", mean(a1_1_vals), "\n")
cat("  Moda  :", moda(a1_1_vals), "\n")
cat("Desviación estándar para a1 = 1   :", sd_a1_1, "\n\n")

# 5) # Calcular percentiles (0%, 25%, 50%, 75%, 100%)
percentiles <- c(0, 0.25, 0.5, 0.75, 1)

cat("Percentiles para a1 = 0:\n")
print(quantile(a1_0_vals, probs = percentiles))
 
cat("\nPercentiles para a1 = 0.5:\n")
print(quantile(a1_05_vals, probs = percentiles))

cat("\nPercentiles para a1 = 1:\n")
print(quantile(a1_1_vals, probs = percentiles))



###################################################################################
##################### RESOLUCIÓN DE UN EJEMPLO REPRESENTATIVO #####################
###################################################################################

d1 <- 1 # Se va variando para analizar los diferentes escenarios
a1 <- 0.5 # Se va variando para analizar los diferentes escenarios
n <- 200
M <- 100

s1 <- exp(meanlogS1(d1,a1) + (sdlog^2)/2)

#### Cálculo de d2 óptima y pDa2 

# Crear un data.frame vacío para almacenar los resultados
resultados_d2 <- data.frame(
  semilla = integer(),
  d2_optimo = numeric(),
  pago_d2_optimo = numeric(),
  pago_d2_no_optimo = numeric(),
  pDa2 = I(list())
)

for (i in 1:10) {
  print (paste("Resolviendo para la semilla ", i))
  set.seed(i)
  opt_d2_res <- opt_d2(d1, a1, s1, M, n)
  pDa2_vec <- pD_a2(d1, a1, s1, M, n)
  # Calcular el pago para el valor de d2 que NO es óptimo
  d2_no_optimo <- d2_values[d2_values != opt_d2_res$d2]
  pago_d2_no_optimo <- pago_d2(d1, a1, s1, d2_no_optimo, M, n)
  resultados_d2 <- rbind(resultados_d2, data.frame(
    semilla = i,
    d2_optimo = opt_d2_res$d2,
    pago_d2_optimo = opt_d2_res$pago,
    pago_d2_no_optimo = pago_d2_no_optimo,
    pDa2 = I(list(pDa2_vec))
  ))
}

print(resultados_d2)

# Estudio estadístico de pDa2
# 1) Extraer los valores de cada componente del vector pDa2 en listas separadas
a2_05_vals  <- sapply(resultados_d2$pDa2, function(x) x[2])
a2_1_vals   <- sapply(resultados_d2$pDa2, function(x) x[3])

# 2) Calcular estadísticos para cada valor de a2
percentiles <- c(0, 0.25, 0.5, 0.75, 1)

cat("a2 = 0.5:\n")
cat("  Media:", mean(a2_05_vals), "\n")
cat("  Moda:", moda(a2_05_vals), "\n")
print(quantile(a2_05_vals, probs = percentiles))

cat("a2 = 1:\n")
cat("  Media:", mean(a2_1_vals), "\n")
cat("  Moda:", moda(a2_1_vals), "\n")
print(quantile(a2_1_vals, probs = percentiles))

# Media de los pagos en d2
cat("Media pagos d2 optimo: ", mean(resultados_d2$pago_d2_optimo))
cat("Media pagos d2 no optimo: ", mean(resultados_d2$pago_d2_no_optimo))
