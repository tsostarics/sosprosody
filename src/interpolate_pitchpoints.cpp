#include <Rcpp.h>
using namespace Rcpp;


//' Interpolate pitch points
//'
//' Given a vector of numeric timepoints `new_times` that lie within the range
//' of `old_times` and `pitch_vals` located at `old_times`, determine which
//' two pitch points are adjacent to each new timepoint, then calculate the
//' weighted mean based on how far the time point is from each pitch point to
//' interpolate between the two.
//'
//' @param new_times Sorted numeric vector with range within `old_times`
//' @param old_times Sorted numeric vector
//' @param pitch_vals Pitch values, of same length as `old_times`
//'
//' @return Numeric vector of interpolated pitch points to correspond to the
//' timepoints at `new_times`
// [[Rcpp::export]]
NumericVector interpolate_pitchpoints(NumericVector new_times, NumericVector old_times, NumericVector pitch_vals) {
  int ot_size = old_times.size();
  int pt_size = pitch_vals.size();
  int nt_size = new_times.size();

  if (ot_size != pt_size)
    stop("old_times and pitch_vals must have equal length");

  if (!((new_times[0] >= old_times[0]) & (new_times[nt_size-1] <= old_times[ot_size-1])))
    stop("range of new_times must be within inclusive range of old_times");

  int n = new_times.size();
  NumericVector left_times(n, NA_REAL), right_times(n, NA_REAL);
  NumericVector left_points(n, NA_REAL), right_points(n, NA_REAL);
  NumericVector interpolated_values(n, NA_REAL);

  int j = 0;
  for (int i = 0; i < n; i++) {
    double x = new_times[i];
    double difference = 1;

    while (difference > 0) {
      j++;
      difference = x - old_times[j];
    }
    left_times[i] = old_times[j - 1];
    right_times[i] = old_times[j];
    left_points[i] = pitch_vals[j - 1];
    right_points[i] = pitch_vals[j];
    j--;
  }

  NumericVector left_weights = abs(new_times - right_times);
  NumericVector right_weights = abs(new_times - left_times);

  for (int i = 0; i < n; i++) {
    NumericVector values = NumericVector::create(left_points[i], right_points[i]);
    NumericVector weights = NumericVector::create(left_weights[i], right_weights[i]);
    double wmean = sum(values * weights) / sum(weights);
    interpolated_values[i] = wmean;
  }

  return interpolated_values;
}
