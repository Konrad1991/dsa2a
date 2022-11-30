// [[Rcpp::depends(ast2ast, RcppArmadillo)]]
// [[Rcpp::plugins(cpp17)]]

#include "etr.hpp"
using namespace Rcpp;
using namespace etr;

static double parms[3];
#define a parms[0]
#define b parms[1]
#define c parms[2]


extern "C" {
  void initmod_a2a(void (* odeparms)(int *, double *));
}

extern "C" {
  void derivs_a2a (int *neq, double *t, double *y, double *ydot,
                   double *yout, int *ip);
}

void initmod_a2a(void (* odeparms)(int *, double *)) {
  int N = 3;
  odeparms(&N, parms);
}

// this could be created by ast2ast based on the R function of the user
void user_fct(double t, sexp& y, sexp& ydot) {
  at(ydot, 1) = a * at(y, 1) + at(y, 2)*at(y, 3);
  at(ydot, 2) = b * (at(y, 2) - at(y, 3));
  at(ydot, 3) = - at(y, 1)*at(y, 2) + c * at(y, 2) - at(y, 3);
}

/* Derivatives */
void derivs_a2a (int *neq, double *t, double *y, double *ydot,
             double *yout, int *ip) {
  sexp y_(*neq, y, 2); // 2 --> only borrow memory
  sexp ydot_(*neq, ydot, 2);
  double t_ = *t;

  user_fct(t_, y_, ydot_);
}

