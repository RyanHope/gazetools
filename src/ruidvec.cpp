#include "gazetools.h"

//' @export
// [[Rcpp::export]]
std::vector<int> ruidvec(std::vector<std::string> x) {
  int n = x.size();
  std::vector<int> out(n);
  std::string last = "";
  int id = 0;
  for(int i = 0; i < n; ++i) {
    if (x[i]==last) {
      out[i] = id;
    } else {
      id += 1;
      last = x[i];
      out[i] = id;
    }
  }
  return out;
}
