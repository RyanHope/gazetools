#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector uidvec(LogicalVector x) {
  int n = x.size();
  NumericVector out(n);
  bool last = 0;
  int id = 0;
  for(int i = 0; i < n; ++i) {
    if (x[i]==last) {
      out[i] = (last==0) ? 0 : id;
    } else {
      if (last==0) {
        id += 1;
        last = 1;
        out[i] = id;
      } else {
        last = 0;
        out[i] = 0;
      }
    }
  }
  return out;
}
