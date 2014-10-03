#include "gazetools.h"

//while (T) {
//  f <- vv[vv$v < vt,]
//  vtn <- mean(f$v) + sigma * sd(f$v)
//  if (abs(vtn-vt)<1)
//    break
//  vt <- vtn
//}


//' @export
// [[Rcpp::export]]
double sigthresh(std::vector<double> x, std::vector<double> e, double threshold, double sigma) {
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
