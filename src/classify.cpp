#include "gazetools.h"

//' Classify Gaze
//'
//' Velocity based classification of raw gaze samples to discrete events such as saccades and fixations.
//'
//' @param v a vector of the instantaneous velocities for a set raw gaze samples
//' @param e a vector indicating blinks or bad data in the velocity vector
//' @param vt saccade onset velocity threshold
//' @param sigma when greater than 0, the saccade onset velocity threshold is iteratively adjusted such that
//'        the saccade onset threshold is sigma standard deviations higher than the mean of all velocity samples
//'        lower than the saccade onset threshold, when glissade detection is enabled sigma/2 is used as the
//'        saccade offset velocity threshold
//' @param minsacdur the minimum saccade duration in terms of number of samples (to obtain divide desired duration by samplerate)
//'
//'
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector classify(std::vector<double> v, std::vector<double> e, double vt, double sigma, int minsacdur) {

  double st = vt;
  int i=0, j=0, n=0, fb=0, fe=0;

  if (sigma>0) {
    vt = sigthresh(v, e, vt, sigma); // Iteratively find saccade onset threshold
    st = sigthresh(v, e, vt, sigma/2); // Iteratively find saccade offset threshold
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
      fb = i-j; // Save the start of the saccade
    } else if ((i+1<n) && out[i]==1 && out[i+j]==0) { // Find saccade offset
      if (sigma==0) // If sigma is 0 we wont be looking for a glissade
        st = v[i+j+1]; // so we use first local minima under saccade onset threshold
      while ((i+j<n) && v[i+j] > st) { // otherwise we use the first local minima under the saccade offset threshold
        out[i+j] = 1;
        j += 1;
      }
      fe = i+j; // Save the end of the saccade
      if (fe-fb < minsacdur) { // If the saccade is too short, wipe it out
        for (i=fe; i<=fb; i++)
          out[i] = 0;
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
