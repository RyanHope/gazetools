#include "gazetools.h"

//' Sigma Threshold {C++}
//'
//' Iteratively converges on a threshold for which the mean of the values below the threshold
//' is \emph{sigma} standard deviations away from the threshold
//'
//' @param x a vector of values that a sigma threshold needs to be determined for
//' @param e a vector of class \code{"logical"} indicating samples that should be excluded from any calculations
//' @param threshold the starting threshold for the iterative algorithm
//' @param sigma the number of standard deviations of separation between the threshold and the mean of the values below the threhold
//'
//' @examples
//' d <- with(highspeed, pva(x,y,1250,1024,768,.38,.30,.67))
//' sigthresh(d$v,d$blinks,80,5)
//'
//' @export
//'
// [[Rcpp::export]]
double sigthresh(std::vector<double> x, std::vector<bool> e, double threshold, double sigma) {
  double temp_threshold, sum, mean, sd;
  int n, count;
  n = x.size();
  while (true) {
    count = 0;
    sum = 0;
    for(int i = 0; i < n; ++i) {
      if (x[i]<threshold && !e[i]) {
        sum += x[i];
        count += 1;
      }
    }
    mean = sum/count;
    sum = 0;
    for(int i = 0; i < n; ++i) {
      if (x[i]<threshold && !e[i])
        sum += (x[i]-mean)*(x[i]-mean);
    }
    sd = sqrt(sum/(double)count);
    temp_threshold = mean + (sigma * sd);
    if (abs(temp_threshold-threshold)<1)
      break;
    threshold = temp_threshold;
  }
  return threshold;
}
