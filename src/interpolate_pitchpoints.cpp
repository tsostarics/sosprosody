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
NumericVector interpolate_pitchpoints(const NumericVector& new_times,
                                      const NumericVector& old_times,
                                      const NumericVector& pitch_vals) {
  const int ot_size = old_times.size();
  const int pt_size = pitch_vals.size();
  const int nt_size = new_times.size();

  if (ot_size != pt_size)
    stop("old_times and pitch_vals must have equal length");

  if ((new_times[0] < old_times[0]) or (new_times[nt_size-1] > old_times[ot_size-1]))
    stop("range of new_times must be within inclusive range of old_times");

  NumericVector interpolated_values(nt_size, NA_REAL);

  int j = 0;
  for (int i = 0; i < nt_size; i++) {
    double difference = 1;

    while (difference > 0) {
      j++;
      difference = new_times[i] - old_times[j];
    }
    double left_time   = old_times[j - 1];
    double right_time  = old_times[j];
    double left_point  = pitch_vals[j - 1];
    double right_point = pitch_vals[j];

    double left_weight  = right_time   - new_times[i];
    double right_weight = new_times[i] - left_time;

    interpolated_values[i] = (left_point * left_weight + right_point * right_weight) / (left_weight + right_weight);
    j--;
  }

  return interpolated_values;
}
