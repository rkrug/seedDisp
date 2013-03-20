#include "windDispCpp.h"

SEXP windDispCpp( SEXP DX2, SEXP DY2, SEXP SD2D, SEXP SEEDS, SEXP MASK ){
  using namespace Rcpp;

  // The input parameter  
  int dx2 = as<int>(DX2); // by reference or value?
  int dy2 = as<int>(DY2);
  NumericVector sd2D (SD2D); // by reference!
  IntegerMatrix seeds (SEEDS);
  IntegerMatrix mask (MASK);

  // result vector
  IntegerMatrix dispSeeds = clone<IntegerMatrix>(mask);

  // internal variables
  IntegerVector s (sd2D.size());
  RNGScope scope;                 // N.B. Needed when calling random number generators

  int res; 
  int nc = dispSeeds.ncol();
  int nr = dispSeeds.nrow();
  
  // BEGIN loop over seeds grid ("moving")
  for( int y=0; y < nc; y++ ){
    for( int x=0; x < nr; x++ ){
      // BEGIN loop over sd2D ("window")
      if ( dispSeeds(x, y) >= 0 ) {
        int indS = 0;
        for( int xS=x; xS <= x + dx2; xS++ )
          for( int yS=y; yS <= y + dy2; yS++, indS++ )
            s[indS]=seeds(xS, yS);
        res = 0;
        for( int i=0; i<s.size(); i++ ){
          if (s[i]>0 && sd2D[i]>0) {
            res += (int) ::Rf_rbinom((double)(s[i]), sd2D[i]);
          }
        }
        dispSeeds(x, y) = res;
      }
    }
  }
  // END loop over seeds

  return wrap( dispSeeds );
}
