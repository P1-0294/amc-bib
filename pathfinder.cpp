#include <Rcpp.h>
#include <stdio.h>
#include <queue>
using namespace Rcpp;
using namespace std;

#define EPS 1e-10

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


struct Qrecord {
  int node;
  double d;
  int l;
};


void target_form(IntegerVector &from, IntegerVector &to, NumericVector &weight, int n,
                 IntegerVector &start , IntegerVector &end, IntegerVector &ntarget, NumericVector &nweight) {
  IntegerVector count(n);
  int len = from.size();
  int f, t;
  
  for(int i = 0; i < len; i++) {
    f = from[i] - 1; t = to[i] - 1;
    count[f]++; count[t]++;
  }
  
  int cumsum = 0;
  for(int i = 0; i < n; i++) {
    start[i] = cumsum;
    cumsum += count[i];
    end[i] = cumsum;
  }
  
  // IntegerVector ntarget(2*len);
  // NumericVector nweight(2*len);
  IntegerVector pos = clone(start);
  
  
  // printf("YY: %ld %ld\n", from.size(), to.size());
  for(int i = 0; i < len; i++) {
    int f = from[i] - 1;
    int t = to[i] - 1;
    // printf("%d %d -- %d %d\n", f, t, pos[f], pos[t]);
    double w = weight[i];
    ntarget[pos[f]] = t;
    nweight[pos[f]] = w;
    pos[f]++;
    ntarget[pos[t]] = f;
    nweight[pos[t]] = w;
    pos[t]++;
  }

  // printf("%d", to.size());
  // List lst = List::create(
  //   Named("start") = start,
  //   Named("end") = end,
  //   Named("target") = ntarget,
  //   Named("weight") = nweight
  // );
  
  // return lst;
  // DataFrame df = DataFrame::create( 
  //   Named("from") = from, 
  //   Named("to") = to
  // ); 
  // return df;
}

void calculate_dmax(IntegerVector &start, IntegerVector &end, IntegerVector &target, NumericVector &weight, NumericVector &dmax) {
  int n = start.size();
  for(int i = 0; i < n; i++) {
    dmax[i] = 0;
    for(int j = start[i]; j < end[i]; j++) {
      double d = weight[j];
      if(d > dmax[i]) {
        dmax[i] = d;
      }
    }
  }    
}

void print_network(IntegerVector &start, IntegerVector &end, IntegerVector &target, NumericVector &weight) {
  int n = start.size();
  printf("========================\n");
  for(int i = 0; i < n; i++) {
    for(int j = start[i]; j < end[i]; j++) {
      int t = target[j];
      double w = weight[j];
      printf("%d -> %d: %.2f\n", i + 1, t + 1, w);
    }
  }
  printf("========================\n");
}


// [[Rcpp::export]]
DataFrame pathfinder_sparse_cpp(IntegerVector from, IntegerVector to, NumericVector weight, int n, double r, int q) {
  if(q > n - 1) {
    q = n - 1;
  }
  int len = from.size();
  IntegerVector start(n);
  IntegerVector end(n);
  IntegerVector ntarget(2*len);
  NumericVector nweight(2*len);
  // printf("XX: %ld %ld\n", from.size(), to.size());
  
  
  target_form(from, to, weight, n, start, end, ntarget, nweight);
  // print_network(start, end, ntarget, nweight);
  NumericVector dmax(n);
  calculate_dmax(start, end, ntarget, nweight, dmax);
  NumericVector dist(n);
  vector<vector<int> > new_targets;
  LogicalVector keep(2*len);
  int keepcount = 0;
  
  queue<Qrecord> Q;
  
  for(int v = 0; v < n; v++) {
    Qrecord rec = {v, 0, 0};
    Q.push(rec);
    for(int i = 0; i < n; i++) {
      dist[i] = R_PosInf;
    }
    dist[v] = 0;
    while(!Q.empty()) {
      rec = Q.front();
      Q.pop();
      // printf("Q: %d %.3f %d\n", rec.node, rec.d, rec.l);
        
      int l = rec.l + 1;
      int u = rec.node;
      double d = rec.d;
      for(int j = start[u]; j < end[u]; j++) {
        int t = ntarget[j];
        double w = nweight[j];
        double new_dist = 0;
        if(r == R_PosInf) {   // max
          new_dist = d;
          if(d < w) {
            new_dist = w;
          }
        } else {
          new_dist = pow(pow(d, r) + pow(w, r), 1/r);
        }
        if(new_dist <= dmax[v] && new_dist < dist[t]) {
          dist[t] = new_dist;
          if(l < q) {
            Qrecord rec = {t, new_dist, l};
            Q.push(rec);
          }
        }
      }
    }
    
    for(int j = start[v]; j < end[v]; j++) {
      int u = ntarget[j];
      // printf("X:%d -> %d: %.2f %.2f %.2f %.2f\n", v + 1, u + 1, nweight[j], dist[u], fabs(nweight[j] - dist[u]), EPS);
      if(fabs(nweight[j] - dist[u]) < EPS) {
        keep[j] = TRUE;
        keepcount++;
      }
    }
  }

  int keepSize = keepcount/2;
  IntegerVector outFrom(keepSize);
  IntegerVector outTo(keepSize);
  NumericVector outWeight(keepSize);

  // printf("KS: %d\n", keepSize);
  
  int keepPos = 0;
  for(int i = 0; i < n; i++) {
    int f = i + 1;
    for(int j = start[i]; j < end[i]; j++) {
      int t = ntarget[j] + 1;
      double w = nweight[j];
      // printf("X[%d]: %d->%d: %f: K: %d\n",j, f, t, w, keep[j]);
      if(keep[j] && f < t) {
        outFrom[keepPos] = f;
        outTo[keepPos] = t;
        outWeight[keepPos] = w;
        keepPos++;
      }
    }
  }
  DataFrame df = DataFrame::create(
    Named("from") = outFrom,
    Named("to") = outTo,
    Named("weight") = outWeight
  );
  return df;
}


// /*** R
// pathfinder_sparse_cpp(1:200, c(2:200, 1) , seq(0.01, 2, 0.01), 200, Inf, 200)
// */
