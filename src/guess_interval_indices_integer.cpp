#include <Rcpp.h>
using namespace Rcpp;

//' Make section indices
 //'
 //' Given a vector, assign contiguous groups based on where the values change.
 //'
 //' @param section_column Vector of values
 //' @title Integer section indices
 //' @return Integer vector of indices for each contiguous group of values
 // [[Rcpp::export]]
IntegerVector guess_interval_indices_integer(IntegerVector section_column) {
  int n = section_column.size();
  IntegerVector indices(n);
  int interval_idx = 1;
  int i = 0;
  int previous_section = section_column[0];

  while (i < n) {
    int current_section = section_column[i];

    if (current_section != previous_section) {
      interval_idx++;
      previous_section = current_section;
    }

    indices[i] = interval_idx;
    i++;
  }

  return indices;
}