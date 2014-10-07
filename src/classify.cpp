#include "gazetools.h"

//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector classify(std::vector<double> v, std::vector<double> e, double vt, double sigma) {

  double st;
  int i, j, n;

  st = vt;

  if (sigma>0) {
    vt = sigthresh(v, e, vt, sigma);
    st = sigthresh(v, e, vt, sigma/2);
  }

  n = v.size();
  Rcpp::IntegerVector out(n);
  for(i = 0; i < n; ++i) {
    if (v[i]>vt)
      out[i] = 1;
    else
      out[i] = 0;
  }
  for(i = 1; i < n; ++i) {
    j = 1;
    if (e[i]==true) { // This sample is a blink
      out[i] = -1;
    } else if ((i-j>=0) && out[i]==1 && out[i-j]==0) { // Find saccade onset
      while ((i-j>=0) && v[i-j] < v[i-j+1]) { // Saccade onset is first local minima under saccade onset threshold
        out[i-j] = 1;
        j += 1;
      }
    } else if ((i+1<n) && out[i]==1 && out[i+j]==0) { // Find saccade offset
      if (sigma==0) // If sigma is 0 we wont be looking for a glissade
        st = v[i+j+1]; // so we use first local minima under saccade onset threshold
      while ((i+j<n) && v[i+j] > st) { // otherwise we use the first local minima under the saccade offset threshold
        out[i+j] = 1;
        j += 1;
      }
    }
  }

  Rcpp::IntegerVector levels(3);
  levels[0] = -1;
  levels[1] = 0;
  levels[2] = 1;
  Rcpp::CharacterVector labels(3);
  labels[0] = "BLINK";
  labels[1] = "FIXATION";
  labels[2] = "SACCADE";
  Rcpp::IntegerVector c = match(out, levels);
  c.attr("levels") = labels;
  c.attr("class") = "factor";

  return c;
}
