#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

double winner_update (double r1, double r2){
  return (1.0 / (1 + pow(10, (r2-r1)/400)));
}

NumericVector elo_update(NumericVector& x, int k) {
  double P_winner = winner_update(x[0], x[1]);
  double P_loser = winner_update(x[1], x[0]);
  double output_rating_winner = x[0] + (k * (1 - P_winner));
  double output_rating_loser = x[1] + (k * (0 - P_loser));
  NumericVector vout = {output_rating_winner,output_rating_loser};
  return vout;
}

// [[Rcpp::export]]
NumericVector eloCPP(IntegerMatrix& pwc, NumericVector& scores, int num) {
  NumericVector s = scores;
  for (int i=0; i<num; i++) {
    const int winner = pwc(i,0);
    const int loser = pwc(i,1);
    const double winner_rating = s[winner-1];
    const double loser_rating = s[loser-1];
    NumericVector m = {winner_rating, loser_rating};
    const NumericVector updates = elo_update(m, 100);
    s[winner-1] = updates[0];
    s[loser-1] = updates[1];
  }
  return s;
}

