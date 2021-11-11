NajdiSetridene <- function(vstup) {
  delka = length(vstup)
  for (i in 1 : delka){
    if (vstup[i]!=(i-1)){
      chyba= i
      return(chyba)
    }
    
  }
}
  
Vzestupne <- function(vstup){
  delka = length(vstup)
  pomocny <- c()
  pomocny = rep(0, delka)
  pomocny[1]=1
  pomocny[delka]=1
  for (i in 2:(delka - 1)){
    if (vstup[i-1]+1 ==vstup[i]){
      pomocny[i] = 1
      pomocny[i-1] = 1
    }
  }
  return(pomocny)
}

BreakPointSort <- function(vstup){
  delka = length(vstup) +2
  zaver = max(vstup) + 1
  novy_vektor = c(0,vstup, zaver)
  vzestup_sestup = Vzestupne(novy_vektor)
  vektor_minim <- c()
  for (i in 1:length(vzestup_sestup)){
    if (vzestup_sestup[i] == 0){
      vektor_minim = c(vektor_minim,novy_vektor[i])
    }
  }
  nejmensi_sestupna_pozice = which.min(vektor_minim)
  setrizeny_index = NajdiSetridene(novy_vektor)
  vektor_co_budu_otacet = novy_vektor[setrizeny_index:nejmensi_sestupna_pozice]
  otoceny = rev(vektor_co_budu_otacet)
  novy_vektor = c(novy_vektor[1:(setrizeny_index-1)],otoceny,novy_vektor[(setrizeny_index+1):delka])
 return(novy_vektor)
}
  
  
  
  
  
  
  
  
  