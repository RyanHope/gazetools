#include <Rcpp.h>

enum GazeClass {
  NOISE,
  FIXATION,
  SACCADE,
  GLISSADE_FAST,
  GLISSADE_SLOW
};

Rcpp::IntegerVector classify(std::vector<double> v, std::vector<bool> e, int samplerate, double vt, double sigma, double minsac, double glswin);

double sigthresh(std::vector<double> x, std::vector<bool> e, double threshold, double sigma);

std::vector<int> uidvec(std::vector<bool> x);
std::vector<int> ruidvec(std::vector<std::string> x);

std::vector<double> distance_2_point(std::vector<double> x, std::vector<double> y, double rx, double ry, double sw, double sh, Rcpp::NumericVector ez, Rcpp::NumericVector ex, Rcpp::NumericVector ey);
std::vector<double> subtended_angle(std::vector<double> x1, std::vector<double> y1, std::vector<double> x2, std::vector<double> y2, double rx, double ry, double sw, double sh, Rcpp::NumericVector ez, Rcpp::NumericVector ex, Rcpp::NumericVector ey);
