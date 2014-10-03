#include "gazetools.h"

//' Subtended Angle
//'
//' Takes two screen coordinates and returns the angle (in degrees)
//' subtended by those two points.
//'
//' @param x1 the horizontal coordinate of the first point on a screen (pixels)
//' @param y1 the vertical coordinate of the first point on a screen (pixels)
//' @param x2 the horizontal coordinate of the second point on a screen (pixels)
//' @param y2 the vertical coordinate of the second point on a screen (pixels)
//' @param rx the horizontal resolution of the screen (pixels)
//' @param ry the vertical resolution of the screen (pixels)
//' @param sw the physical screen width (mm)
//' @param sh the physical screen height (mm)
//' @param ez the perpendicular distance from the viewer to the screen (mm)
//' @param ex the horizontal offset of the viewer from screen center (mm)
//' @param ey the vertical offset of the viewer from screen center (mm)
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
    out[i] = std::acos(std::min(std::max(w1/w2, -1.0), 1.0)) * 180.0 / M_PI;
  }
  return out;
}
