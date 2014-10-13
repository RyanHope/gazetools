#include "gazetools.h"

//' Classify Raw Gaze Data
//'
//' Velocity based classification of raw gaze samples to discrete events such as saccades and fixations.
//' With this classifier, fixations are what ever samples are left after identifying all other events.
//' In other words, there is no concept of minium fixation duration.
//'
//' @param v a vector of the instantaneous velocities for a set raw gaze samples
//' @param e a vector indicating blinks or otherwise bad data in the velocity vector
//' @param samplerate the number of samples taken in one second of time
//' @param vt saccade onset velocity threshold
//' @param sigma when greater than 0, the saccade onset velocity threshold is iteratively adjusted such that
//'        the saccade onset threshold is \emph{sigma} standard deviations higher than the mean of all velocity samples
//'        lower than the saccade onset threshold, when glissade detection is enabled \emph{sigma}/2 is used as the
//'        saccade offset velocity threshold
//' @param minsac the minimum saccade duration in seconds
//' @param glswin the duration (in seconds) of the window post-saccade to look for glissades in, setting to 0 disables glissade detection
//' @param alpha the weight (from 0 to 1) of the saccade onset threshold component of the saccade offset threshold,
//'        \emph{1-alpha} is used as the weight for the noise threshold component the saccade offset threshold
//'
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector classify(std::vector<double> v, std::vector<bool> e, int samplerate, double vt=100, double sigma=4.5, double minsac=.02, double glswin=.04, double alpha=.7) {

  // ******************************************************************************
  // TODO:
  //  * Catch saccade->blink & fixation->blink transitions and vise versa
  //
  // ******************************************************************************

  double ts = 1.0 / samplerate;
  double st = vt;
  double beta = 1.0 - alpha;
  int i=0, j=0, n=0, sb=0, se=0;

  int gls = 0;
  if (glswin>0) gls = std::ceil(glswin/ts);

  if (sigma>0) {
    vt = sigthresh(v, e, vt, sigma); // Iteratively find saccade peak threshold
    st = sigthresh(v, e, vt, sigma/2); // Iteratively find saccade onset threshold
  }

  n = v.size();
  Rcpp::IntegerVector out(n);
  for(i = 0; i < n; ++i) {
    if (v[i]>vt)
      out[i] = SACCADE;
    else
      out[i] = FIXATION;
  }
  for(i = 0; i < n;) {
    j = 1;
    if (e[i]==true) { // This sample is a blink
      out[i] = NOISE;
    } else if ((i-j>=0) && out[i]==SACCADE && out[i-j]==FIXATION) { // Find saccade onset
      while ((i-j>=0) && (v[i-j] > st || v[i-j] < v[i-j+1])) { // Saccade onset is first local minima under saccade onset threshold
        out[i-j] = SACCADE;
        ++j;
      }
      sb = i-j; // Save saccade begin index
    } else if ((i+1<n) && out[i]==SACCADE && out[i+j]==FIXATION) { // Find saccade offset
      double tmpsum=0, tmpmean=0, tmpsd=0;
      for (int k=sb; k>=std::max(sb-gls,0); --k) {
        tmpsum += v[k];
      }
      tmpmean = tmpsum/gls;
      tmpsum = 0;
      for (int k=sb; k>=std::max(sb-gls,0); --k) {
        tmpsum += (v[k]-tmpmean)*(v[k]-tmpmean);
      }
      tmpsd = sqrt(tmpsum/(double)gls);
      double nt = alpha*st + beta*(tmpmean + (sigma/2 * tmpsd));
      while ((i+j<n) && (v[i+j] > nt || v[i+j] < v[i+j-1]))  {// Saccade offset is first local minima under saccade offset threshold
        out[i+j] = SACCADE;
        ++j;
      }
      se = i+j; // Save saccade end index
      if ((se-sb+1) < minsac/ts) { // If the saccade is too short, wipe it out
        for (j=sb; j<=se; ++j)
          out[j] = FIXATION;
      }
      i = se; // Move index to end of saccade
      if (out[i-1]==SACCADE && gls>0 && (i+1)<n) {// Now look for glissades
        bool glissade = false;
        int glissade_type = 0;
        int gt = 0;
        int gmax = std::min(i+gls,n-1);
        for (j=i;j<=gmax;++j) {
          if (glissade_type==0) {
            if (v[j]>nt) {
              glissade_type=GLISSADE_SLOW;
              if (v[j]>vt) {
                glissade_type=GLISSADE_FAST;
              }
            }
          } else {
            if (glissade_type==GLISSADE_SLOW && v[j]<=nt) {
              gt = j;
              glissade = true;
              break;
            }
            else if (glissade_type==GLISSADE_FAST && v[j]<=st) {
              gt = j;
              glissade = true;
              break;
            }
          }
        }
        if (glissade) {
          for (i=i;i<=gt;++i)
            out[i]=glissade_type;
          while (((i+1)<n) && v[i+1]<v[i]) {
            out[i]=glissade_type;
            ++i;
          }
        }
      }
    }
    ++i;
  }

  Rcpp::IntegerVector levels(5);
  levels[0] = NOISE;
  levels[1] = FIXATION;
  levels[2] = SACCADE;
  levels[3] = GLISSADE_FAST;
  levels[4] = GLISSADE_SLOW;
  Rcpp::CharacterVector labels(5);
  labels[0] = "Noise";
  labels[1] = "Fixation";
  labels[2] = "Saccade";
  labels[3] = "Glissade-fast";
  labels[4] = "Glissade-slow";
  Rcpp::IntegerVector c = match(out, levels);
  c.attr("levels") = labels;
  c.attr("class") = "factor";
  c.attr("saccade-peak-threshold") = vt;
  c.attr("saccade-onset-threshold") = st;

  return c;
}
