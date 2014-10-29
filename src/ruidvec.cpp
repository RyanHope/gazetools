#include "gazetools.h"

//' String-based (continuous) unique ID runs {C++}
//'
//' For a vector strings, assigns each continuous run of particular string a unique ID.
//'
//' @param x a vector of class \code{"character"}
//'
//' @return a vector of \code{"numeric"}
//'
//' @examples
//' ruidvec(c("One","One","Two","Three","Three","Two","Three","Three","One"))
//'
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
