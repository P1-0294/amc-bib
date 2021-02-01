#include <Rcpp.h>
#include <stdio.h>
#include <queue>
#include "network.h"
using namespace Rcpp;
using namespace std;

#define EPS 1e-10

struct Qrecord {
  int node;
  double d;
  int l;
};

// IntegerVector nodesV, IntegerVector fromV, IntegerVector toV, NumericVector weightV, double r, int q, bool directed
// [[Rcpp::export]]
DataFrame pathfinder_sparse_cpp(XPtr<Network> networkPtr, double r, double q) {
  Network &network = *(networkPtr.get());
    // 
    // *net(nodesV, fromV, toV, weightV, directed);
  // network.print_network();
  if(q > network.V - 1) {
    q = network.V - 1;
  }

  NumericVector dmax(network.V);
  for(int i = 0; i < network.V; i++) {
    dmax[i] = 0;
    for(int j = network.start[i]; j < network.end[i]; j++) {
      double d = network.weight[j];
      if(d > dmax[i]) {
        dmax[i] = d;
      }
    }
  }    

  NumericVector dist(network.V);
  LogicalVector keep(network.nArcs);

  queue<Qrecord> Q;
  
  
  for(int v = 0; v < network.V; v++) {
    Qrecord rec = {v, 0, 0};
    Q.push(rec);
    for(int i = 0; i < network.V; i++) {
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
      for(int j = network.start[u]; j < network.end[u]; j++) {
        int t = network.target[j];
        double w = network.weight[j];
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
    for(int j = network.start[v]; j < network.end[v]; j++) {
      int u = network.target[j];
      // printf("X:%d -> %d: %.2f %.2f %.2f %.2f\n", v + 1, u + 1, nweight[j], dist[u], fabs(nweight[j] - dist[u]), EPS);
      if(fabs(network.weight[j] - dist[u]) < EPS) {
        keep[j] = TRUE;
      }
    }
  }
  
  // for(int i = 0; i < network.nArcs; i++) {
  //   printf("KEEP: %s\n", keep[i] ? "true": "false");
  // }
  // return NULL;
  return network.export_edges(keep);
}


// /*** R
// pathfinder_sparse_cpp(1:200, c(2:200, 1) , seq(0.01, 2, 0.01), 200, Inf, 200)
// */
