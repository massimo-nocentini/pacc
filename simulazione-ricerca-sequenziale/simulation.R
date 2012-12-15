
simula <- function(numdimensioni, intervallo, prove){

  # vettore per i risultati della simulazione
  vettore <- rep(0, numdimensioni*intervallo)
  # vettore delle dimensioni
  dimensioni <- rep(0, numdimensioni)
  # vettore delle medie simulate
  medie <- rep(0, numdimensioni)
  # vettore delle varianze simulate
  varianze <- rep(0, numdimensioni)
   # vettore delle varianze delle varianze simulate
  varianzevarianze <- rep(0, numdimensioni)
 # vettore delle medie simulate
  medie.theo <- rep(0, numdimensioni)
  # vettore delle varianze simulate
  varianze.theo <- rep(0, numdimensioni)
   # vettore delle varianze delle varianze simulate
  varianzevarianze.theo <- rep(0, numdimensioni)  
  # vettore per la verifica dei valori simulati
  testmedie <- rep(0, numdimensioni)  
  testvarianze <- rep(0, numdimensioni)
  
  for (n1 in 1:numdimensioni){
    n <- n1*intervallo #dimensione corrente del vettore

    # contiene il parametro da valutare (confronti nel caso della
    # ricerca sequenziale)
    tot <- 0

    # contiene la somma dei quadrati del parametro da valutare
    tot2 <- 0 

    tot4 <- 0;

    # calcolo della media teorica
    mediaTeorica <- valoreTeoricoMedia(n)

    # calcolo della varianza teorica
    varianzaTeorica <- valoreTeoricoVarianza(n)

    # calcolo della varianzavarianza teorica
    varianzavarianzaTeorica <- valoreTeoricoVarianzaVarianza(n);

 # si decide di fare su tale permutazione un numero di ricerche
      # proporzionale ad n
    ripetizioni <- 2*n
    checks <- rep(0, ripetizioni*prove)
    index <- 1
     # p1 conta il numero di prove fatte
    for (p1 in 1:prove){

      # si genera una permutazione casuale di lunghezza n
      vettore <- sample(x=1:n, size=n)     
      
      for (i in 1:ripetizioni){
	# si memorizza in "comp" il risultato dell'esecuzione
	# dell'algoritmo
        comp <- sequenziale(n, vettore)
        checks[index] <- comp
        index <- index + 1
        
        tot <- tot+ comp;
        tot2 <- tot2+comp^2;
        tot4 <- tot4+(comp-mediaTeorica)^4;
      }
    }
    print(paste("Mean: ",
                mean(checks),
                " Var: ",
                var(checks),
                sep=""))
    media <- tot/(ripetizioni*prove);
    varianza <- (tot2/(ripetizioni*prove))-media^2;
    varianzavarianza <- (tot4/(ripetizioni*prove))-varianzaTeorica^2;

    ## print(paste(n,
    ##             " -- \nMedia teorica = ",
    ##             mediaTeorica,
    ##             " \nMedia empirica = ",
    ##             media,
    ##             sep=""))
    
    ## print(paste(n,
    ##             " -- \nVarianza teorica = ",
    ##             varianzaTeorica,
    ##             " \nVarianza empirica = ",
    ##             varianza,
    ##             sep=""))
          
    ## print(paste(n,
    ##             " -- \nVarianzavarianza teorica = ",
    ##             varianzavarianzaTeorica,
    ##             " \nVarianzavarianza empirica = ",
    ##             varianzavarianza,
    ##             sep=""))

    # si memorizza la dimensione del vettore corrente
    dimensioni[n1] <- n
      
    # si memorizza la media simulata
    medie[n1] <- media

    # si memorizza la varianza simulata
    varianze[n1] <- varianza;

    varianzevarianze[n1] <- varianzavarianza

    # si memorizza la media simulata
    medie.theo[n1] <- mediaTeorica

    # si memorizza la varianza simulata
    varianze.theo[n1] <- varianzaTeorica

    varianzevarianze.theo[n1] <- varianzavarianzaTeorica
    
    testmedie[n1] <- (media-mediaTeorica)/sqrt(varianzaTeorica)*
      sqrt(prove*ripetizioni)
    
    testvarianze[n1] <- (varianza-varianzaTeorica)/sqrt(varianzavarianzaTeorica)*
        sqrt(prove*ripetizioni);
  }
  
  ## [sort([seq(testmedie[i],i=1..numdimensioni)]),sort([seq(testvarianze[i],i=1..numdimensioni)])];
 #[seq(medie[i],i=1..numdimensioni)];
  
  data.frame(dimensioni,
             medie.theo,
             medie,
             varianze.theo,
             varianze,
             varianzevarianze.theo,
             varianzevarianze,
             testmedie,
             testvarianze)
}

valoreTeoricoMedia <- function(n){
 # Ricerca sequenziale: numero medio confronti
  (n+1)/2
}


valoreTeoricoVarianza <- function(n){
# Ricerca sequenziale: varianza numero confronti
  ((n^2)-1)/12
}


valoreTeoricoVarianzaVarianza <- function(n){
# Ricerca sequenziale: "varianzavarianza" numero confronti
  (n-2)*(n-1)*(n+1)*(n+2)/180;
}

#
# La seguente funzione restituisce il numero di confronti
# fatti durante una ricerca sequenziale.
#

sequenziale <- function(nelem, vettore){

                                        # Ricerca sequenziale
  k <- sample(x=1:nelem, 1)  
  comp <- 1;
  j <- 1
  while (k != vettore[j]){
    comp <- comp+1;
    j <- j+1
  }
  comp
}
