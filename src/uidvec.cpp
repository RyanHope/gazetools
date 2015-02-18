#include "gazetools.h"

//' Logical-based (non-continuous) unique ID runs {C++}
//'
//' For a vector of logicals, assigns each continuous run of TRUE values a unique ID.
//'
//' @param x a vector of class \code{"logical"}
//'
//' @return a vector of \code{"numeric"}
//'
//' @examples
//' uidvec(c(FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,FALSE,TRUE))
//'
//' @export
// [[Rcpp::export]]
std::vector<int> uidvec(std::vector<bool> x, int start=0) {
  int n = x.size();
  std::vector<int> out(n);
  bool last = 0;
  int id = start;
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
