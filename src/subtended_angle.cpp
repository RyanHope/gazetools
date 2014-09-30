#define _USE_MATH_DEFINES
#include <cmath>

#include "Rcpp.h"

using namespace std;
using namespace Rcpp;

//' Distance to Point
//'
//' Takes an x and y screen coordinate and returns the physical distance
//' from the observer to that point on the screen.
//' 
//' @template 1p
//' @template eye
//'
//' @export
//' 
//' @examples
//' distance_2_point(840, 525, 1680, 1050, 473.76, 296.1, 750)
//' 
// [[Rcpp::export]]
std::vector<double> distance_2_point(std::vector<double> x, std::vector<double> y, double rx, double ry, double sw, double sh, std::vector<double> ez, std::vector<double> ex, std::vector<double> ey) {
  int n = x.size();
  std::vector<double> out(n);
  double dx;
  double dy;
  for(int i = 0; i < n; ++i) {
    dx = x[i] / rx * sw - sw / 2 + ex[i];
    dy = y[i] / ry * sh - sh / 2 - ey[i];
    out[i] = sqrt(ez[i]*ez[i] + dx*dx + dy*dy);
  }
  return out;
}

//' Subtended Angle
//'
//' Takes two screen coordinates and returns the angle (in degrees)
//' subtended by those two points.
//' 
//' @template 2p
//' @template eye
//' 
//' @return degrees of visual angle
//'
//' @export
//' 
//' @examples
//' subtended_angle(835, 525, 845, 525, 1680, 1050, 473.76, 296.1, 750)
//'
// [[Rcpp::export]]
std::vector<double> subtended_angle(std::vector<double> x1, std::vector<double> y1, std::vector<double> x2, std::vector<double> y2, double rx, double ry, double sw, double sh, std::vector<double> ez, std::vector<double> ex, std::vector<double> ey) {
  int n = x1.size();
  std::vector<double> out(n);
  std::vector<double> d1 = distance_2_point(x1, y1, rx, ry, sw, sh, ez, ex, ey);
  std::vector<double> d2 = distance_2_point(x2, y2, rx, ry, sw, sh, ez, ex, ey);
  double dX, dY, dS, w1, w2;
  for(int i = 0; i < n; ++i) {
    dX = sw * (x2[i] - x1[i]) / rx;
    dY = sh * (y2[i] - y1[i]) / ry;
    dS = sqrt(dX*dX + dY*dY);
    w1 = d1[i]*d1[i] + d2[i]*d2[i] - dS*dS;
    w2 = 2 * d1[i] * d2[i];
    out[i] = acos(min(max((w1 / w2), -1.0), 1.0)) * 180 / M_PI;
  }
  return out;
}