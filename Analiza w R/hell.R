ao.hellwig <- function(NumOfVar, VarSet, CorrMethod) {
  
  WybraneZmienne <- array();
  
  tmpH <- 0;
  maxH <- 0;
  
  TablicaKorelacji <- cor(VarSet, method=CorrMethod);
  
  Lista <- list();
  
  for( i in 1: (NumOfVar-1) ) {
    
    Lista[[i]] <- combinations( (NumOfVar-1), i, 2:NumOfVar, repeats=FALSE);
    
  }
  
  for(KtoryRzadKombinacji in 1:length(Lista)) {
    
    H<-array();
    
    for( KtoraKombinacja in 1: length(Lista[[ KtoryRzadKombinacji ]][, 1]) ) {
      
      h<-array();
      
      for( KtoryIndexZmiennej in 1:length( Lista[[ KtoryRzadKombinacji ]][ KtoraKombinacja, ])) {
        
        Zmienna <- Lista[[ KtoryRzadKombinacji ]][ KtoraKombinacja, KtoryIndexZmiennej ];
        
        RXY <- ( TablicaKorelacji[ Zmienna, 1] )^2;
        
        Zmienne <- Lista[[ KtoryRzadKombinacji ]][ KtoraKombinacja, ];
        
        SumaKorelacji <- 0;
        
        for(k in Zmienne) {
          
          SumaKorelacji <- SumaKorelacji + abs( TablicaKorelacji[k, Zmienna] );
        }
        
        h[ KtoryIndexZmiennej ] <- RXY / SumaKorelacji;
      }
      
      tmpH <- sum(h);
      
      if(tmpH > maxH) {
        maxH = tmpH;
        WybraneZmienne <- Zmienne;
      }
    }
  }
  
  return(WybraneZmienne);
}