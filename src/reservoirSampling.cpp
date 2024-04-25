#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
IntegerVector reservoirSampling(IntegerVector& indexs, int k)
{
  IntegerVector results(k);
  int N = indexs.length();
  
  for (int i=0; i<k; ++i) {
    results[i] = indexs[i];
  }
  for (int i=k; i<N; ++i) {
    //int random = rand()%(i+1);
    // int random = R::runif(0, i + 1);
    int random = floor(R::unif_rand() * (i + 1));
    if (random<k) {
      results[random] = indexs[i];
    }
  }
  return results;
}
