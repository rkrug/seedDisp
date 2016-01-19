// [[file:../seedDisp.org::*windDispCpp.cpp][windDispCpp\.cpp:1]]
#include "windDispCpp.h"
  
SEXP windDispCpp( 
                 SEXP BWX, SEXP BWY, // width of buffering in x and y direction
                 SEXP SD2D,          // seed dispersal kernel
                 SEXP SEEDSin,       // buffered seed input matrix
                 SEXP MASKin,        // buffered input mask
                 SEXP MASKOut        // MASK for cells to return and template for return matrix
 ){
  using namespace Rcpp;
  
  // The input parameter  
  int bwx = Rcpp::as<int>(BWX); // by reference or value?
  int bwy = Rcpp::as<int>(BWY);
  Rcpp::NumericVector sd2D     = Rcpp::clone<Rcpp::NumericVector>(SD2D); // by reference!
  Rcpp::NumericMatrix seedsIn = Rcpp::clone<Rcpp::NumericMatrix>(SEEDSin);
  Rcpp::NumericMatrix maskIn  = Rcpp::clone<Rcpp::NumericMatrix>(MASKin);
  Rcpp::NumericMatrix maskOut     = Rcpp::clone<Rcpp::NumericMatrix>(MASKOut);
  
  // result matrix
  Rcpp::NumericMatrix output = Rcpp::clone<Rcpp::NumericMatrix>(MASKOut);
  std::fill(output.begin(), output.end(), 0); // just to make sure set all to 0
  
  // internal variables
  Rcpp::IntegerVector s(sd2D.size());

  int res; 

  RNGScope scope;                 // N.B. Needed when calling random number generators

  // BEGIN loop over output seeds grid ("moving")
  for( int y=0; y < output.ncol(); y++ ){
    for( int x=0; x < output.nrow(); x++ ){
      // if dispBEGIN loop over sd2D ("window")
      // #### begin if MASKOut <> NA
      if ( maskOut(x, y) >= 0 ) { 
        int indS = 0;
        // loop over 2d2D and copy values into s
        for( int xS=x; xS <= x + bwx; xS++ ){
          for( int yS=y; yS <= y + bwy; yS++, indS++) {
            if ( maskIn(xS, yS) >= 0){ 
              s[indS]=seedsIn(xS, yS);
            } else {
              s[indS]=-1;
            }
          }
        }
        res = 0;
        // for each element in s draw binom and sum up
        for( int i=0; i<s.size(); i++ ){
          if (s[i]>0 && sd2D[i]>0) {
            res += (int) ::Rf_rbinom((double)(s[i]), sd2D[i]);
          }
        }
        // copy resulting number of seds into output(x,y)
        output(x, y) = res;
      }
      // #### end if MASKOut <> NA
    }
  }
  // END loop over seeds
  
  return( Rcpp::wrap( output ) );
}
// windDispCpp\.cpp:1 ends here
