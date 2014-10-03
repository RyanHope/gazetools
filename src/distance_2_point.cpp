#include "gazetools.h"

//' Distance to Point
//'
//' Takes an x and y screen coordinate and returns the physical distance
//' from the observer to that point on the screen.
//'
//' @param x the horizontal coordinate of a point on a screen (pixels)
//' @param y the vertical coordinate of a point on a screen (pixels)
//' @param rx the horizontal resolution of the screen (pixels)
//' @param ry the vertical resolution of the screen (pixels)
//' @param sw the physical screen width (mm)
//' @param sh the physical screen height (mm)
//' @param ez the perpendicular distance from the viewer to the screen (mm)
//' @param ex the horizontal offset of the viewer from screen center (mm)
//' @param ey the vertical offset of the viewer from screen center (mm)
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
