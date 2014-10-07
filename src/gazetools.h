#include <Rcpp.h>

Rcpp::IntegerVector classify(std::vector<double> v, std::vector<double> e, double vt, double sigma);

double sigthresh(std::vector<double> x, std::vector<double> e, double threshold, double sigma);

std::vector<int> uidvec(std::vector<bool> x);
std::vector<int> ruidvec(std::vector<std::string> x);

std::vector<double> distance_2_point(std::vector<double> x, std::vector<double> y, double rx, double ry, double sw, double sh, std::vector<double> ez, std::vector<double> ex, std::vector<double> ey);
std::vector<double> subtended_angle(std::vector<double> x1, std::vector<double> y1, std::vector<double> x2, std::vector<double> y2, double rx, double ry, double sw, double sh, std::vector<double> ez, std::vector<double> ex, std::vector<double> ey);
