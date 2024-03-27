#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame edgelist( NumericMatrix x ) {
  
  int R = x.nrow();
  int C = x.ncol();
  
  int col;
  int row;
  NumericVector vals(R * C);
  for ( int i = 0 ; i < ( R * C ); i++ ) {
    row = floor(i / C);
    col = i  - (row * C);
    vals[i] = x( row , col );
  }
  
  NumericVector cell(R * C * 8);
  for ( int i = 0 ; i < cell.size(); i++ ) {
    cell[i] = floor(i / 8);
  }
  
  NumericVector neigh(R * C * 8);
  for ( int i = 0 ; i < ( R * C ); i++ ) {
    // top-left
    if ( (i % C == 0) | (i < C) ) {
      neigh[i * 8] = R_NaN;
    } else {
      neigh[i * 8] = i - C - 1;
    }
    // top
    if (i < C) {
      neigh[i * 8 + 1] = R_NaN;
    }
    else {
      neigh[i * 8 + 1] = i - C;  
    }
    // top-right
    if ( ((i + 1) % C == 0) | (i < C) ) {
      neigh[i * 8 + 2] = R_NaN;
    } else {
      neigh[i * 8 + 2] = i - C + 1;
    }
    // left
    if ( (i % C) == 0) {
      neigh[i * 8 + 3] = R_NaN;
    } else {
      neigh[i * 8 + 3] = i - 1;
    }
    // right 
    if ( ((i + 1) % C) == 0) {
      neigh[i * 8 + 4] = R_NaN;
    } else {
      neigh[i * 8 + 4] = i + 1;
    }
    // bottom-left
    if ( (i % C == 0) | (i >= ((R-1) * C)) ) {
      neigh[i * 8 + 5] = R_NaN;
    } else {
      neigh[i * 8 + 5] = i + C - 1;
    }
    // bottom
    if (i >= ((R-1) * C) ) {
      neigh[i * 8 + 6] = R_NaN;
    } else {
      neigh[i * 8 + 6] = i + C;
    }
    //bottom-right
    if ( (((i + 1) % C) == 0) | (i >= ((R-1) * C)) ) {
      neigh[i * 8 + 7] = R_NaN;
    } else {
      neigh[i * 8 + 7] = i + C + 1;
    }
    
  }
  
  NumericVector weight(R * C * 8);
  for ( int i = 0 ; i < weight.size(); i++ ) {
    weight[i] = vals[neigh[i]];
  }
  
  DataFrame ans = DataFrame::create( 
    Named("from") = cell + 1,
    _["to"] = neigh + 1,
    _["weight"] = weight
  );
  return ans ;
}
// 