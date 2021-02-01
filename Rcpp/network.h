#ifndef MIN_INDEX_PQ_H
#define MIN_INDEX_PQ_H

#include <Rcpp.h>
#include <stdio.h>
#include <map>

using namespace Rcpp;
using namespace std;

class Network {
public:  
  IntegerVector nodes;
  std::map<int, int> to_index;
  int *from, *to, *start, *end, *target, *indeg, *outdeg;
  bool isDirected;
  double *weight;
  int V, E, nArcs;

  // Create an empty MinIndexedPQ which can contain atmost NMAX elements
  Network(IntegerVector nodes, IntegerVector from, IntegerVector to, NumericVector weight, bool directed);
  ~Network();
  DataFrame export_edges(LogicalVector &keep);
  void print_network();
  bool removeLink(int i, int j);
};

Network::Network(IntegerVector nodesV, IntegerVector from, IntegerVector to, NumericVector weightV, bool directed) {
  nodes = nodesV;
  isDirected = directed;
  V = nodes.size();
  E = from.size();
  nArcs = directed ? E : 2*E;
  for(int i = 0; i < V; i++) {
    to_index[nodes[i]] = i;
  }
  
  // for(std::map<int,int>::iterator iter = to_index.begin(); iter != to_index.end(); ++iter)
  // {
  //   int k =  iter->first;
  //   int v = iter->second;
  //   printf("MAP: %d -> %d, %d\n", k, v, to_index[k]);
  // }

  // printf("VEA: %d, %d, %d\n", V, E, nArcs);
  start = new int[V];
  end = new int[V];
  indeg = new int[V];
  outdeg = new int[V];
  
  std::fill(start, start+V, 0);
  std::fill(end, end+V, 0);
  std::fill(indeg, indeg+V, 0);
  std::fill(outdeg, outdeg+V, 0);
  
  target = new int[nArcs];
  weight = new double[nArcs];
  int f, t;
  
  if(!isDirected) {
    for(int i = 0; i < E; i++) {
      f = to_index[from[i]]; 
      t = to_index[to[i]];
      outdeg[f]++; outdeg[t]++;
      indeg[f]++; indeg[t]++;
    }
  } else {
    for(int i = 0; i < E; i++) {
      f = to_index[from[i]]; 
      t = to_index[to[i]];
      // printf("FF: %d(%d) -> %d(%d)\n", from[i], f, to[i], t);
      outdeg[f]++; indeg[t]++;
    }
  }

  int cumsum = 0;
  for(int i = 0; i < V; i++) {
    start[i] = cumsum;
    cumsum += outdeg[i];
    end[i] = cumsum;
  }
  
  // printf("CUMSUM: %d\n", cumsum);
  int *pos = new int[V];
  for(int i = 0; i < V; i++) {
    pos[i] = start[i];
  }
  
  // printf("YY: %ld %ld\n", from.size(), to.size());
  for(int i = 0; i < E; i++) {
    int f = to_index[from[i]];
    int t = to_index[to[i]];
    // printf("%d %d -- %d %d\n", f, t, pos[f], pos[t]);
    double w = weightV[i];
    target[pos[f]] = t;
    weight[pos[f]] = w;
    pos[f]++;
    if(!isDirected) {
      target[pos[t]] = f;
      weight[pos[t]] = w;
      pos[t]++;
    }
  }

}

Network::~Network() {
  delete [] start;
  delete [] end;
  delete [] indeg;
  delete []outdeg;
  delete [] target;
  delete [] weight;
}


DataFrame Network::export_edges(LogicalVector &keep) {
  int keepcount = 0;
  for(int i = 0; i < nArcs; i++) {
    if(keep[i]) {
      keepcount++;
    }
  }
  int keepSize = isDirected ? keepcount : keepcount/2;
  IntegerVector outFrom(keepSize);
  IntegerVector outTo(keepSize);
  NumericVector outWeight(keepSize);
  
  int keepPos = 0;
  for(int i = 0; i < V; i++) {
    int f = i;
    for(int j = start[i]; j < end[i]; j++) {
      int t = target[j];
      double w = weight[j];
      // printf("X[%d]: %d->%d: %f: K: %d\n",j, f, t, w, keep[j]);
      if(keep[j]) {
        if(isDirected || nodes[f] < nodes[t]) {
          outFrom[keepPos] = nodes[f];
          outTo[keepPos] = nodes[t];
          outWeight[keepPos] = w;
          keepPos++;
        } 
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

void Network::print_network() {
  int n = nodes.size();
  printf("========================\n");
  for(int i = 0; i < n; i++) {
    for(int j = start[i]; j < end[i]; j++) {
      int t = target[j];
      double w = weight[j];
      printf("%d(%d) -> %d(%d): %.2f\n", i, nodes[i], t, nodes[t], w);
    }
  }
  printf("========================\n");
}

// [[Rcpp::export]]
XPtr<Network> make_network(IntegerVector nodesV, IntegerVector from, IntegerVector to, NumericVector weightV, bool directed) {
  Network* pd = new Network(nodesV, from, to, weightV, directed);
  XPtr<Network> ptr(pd);
  return ptr;
}

// [[Rcpp::export]]
void print_network(XPtr<Network> network) {
  return network.get()->print_network();
}

// https://stackoverflow.com/questions/59384221/proper-way-to-return-a-pointer-to-a-new-object-from-an-rcpp-function
#endif