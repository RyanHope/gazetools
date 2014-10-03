#include "gazetools.h"

//' @export
// [[Rcpp::export]]
std::vector<int> classify(std::vector<double> x, std::vector<double> e, double vt = 100, double sigma = 6) {

  double st;
  int i, j, n;

  vt = sigthresh(x, e, vt, sigma);
  st = sigthresh(x, e, vt, sigma/2);

  n = x.size();
  std::vector<int> out(n);
  for(i = 0; i < n; ++i) {
    if (x[i]>vt)
      out[i] = 1;
    else
      out[i] = 0;
  }
  for(i = 1; i < n; ++i) {
    j = 1;
    if (e[i]==true) {
      out[i] = -1;
    } else if ((i-j>=0) && out[i]==1 && out[i-j]==0) {
      while ((i-j>=0) && x[i-j] < x[i-j+1]) {
        out[i-j] = 1;
        j += 1;
      }
    } else if ((i+1<n) && out[i]==1 && out[i+j]==0) {
      while ((i+j<n) && x[i+j] > st) {
        out[i+j] = 1;
        j += 1;
      }
    }
  }

  return out;
}
