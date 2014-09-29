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
double distance_2_point(double x, double y, double rx, double ry, double sw, double sh, double ez, double ex = 0, double ey = 0) {
  double dx = x / rx * sw - sw / 2 + ex;
  double dy = y / ry * sh - sh / 2 - ey;
  return sqrt(ez*ez + dx*dx + dy*dy);
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
double subtended_angle(double x1, double y1, double x2, double y2, double rx, double ry, double sw, double sh, double ez, double ex = 0, double ey = 0) {
  double d1 = distance_2_point(x1, y1, rx, ry, sw, sh, ez, ex, ey);
  double d2 = distance_2_point(x2, y2, rx, ry, sw, sh, ez, ex, ey);
  double dX = sw * (x2 - x1) / rx;
  double dY = sh * (y2 - y1) / ry;
  double dS = sqrt(dX*dX + dY*dY);
  double w1 = d1*d1 + d2*d2 - dS*dS;
  double w2 = 2 * d1 * d2;
  return acos(min(max((w1 / w2), -1.0), 1.0)) * 180 / M_PI;
}