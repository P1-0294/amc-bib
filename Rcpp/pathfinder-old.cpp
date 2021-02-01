#ifndef MIN_INDEX_PQ_H
#define MIN_INDEX_PQ_H

#include <map>


class Network {
public:  
  IntegerVector nodes;
  std::map<int, int> to_index;
  int *from, *to, *start, *end, *target, *inded, *outdeg;
  bool isDirected = false;
  double *weight;
  int V, E, nArcs;

  // Create an empty MinIndexedPQ which can contain atmost NMAX elements
  Network(IntegerVector nodes, IntegerVector from, IntegerVector to, NumericVector weight);
  ~Network();
  DataFrame export_edges(LogicalVector &keep);
  void print_network();
  bool removeLink(int i, int j);
};

Network::Network(IntegerVector nodesV, IntegerVector from, IntegerVector to, NumericVector weightV, bool directed=false) {
  nodes = nodesV;
  isDirected = directed;
  // weight = weightV;
  V = nodes.size();
  E = from.size();
  nArcs = undirected ? 2*E : E;
  for(int i = 0; i < E; i++) {
    to_index[nodes[i]] = i;
  }
  start = new int[V];
  end = new int[V];
  indeg = new int[V]
  outdeg = new int[V]
  
  int len = from.size();
  
  target = new int[nArcs];
  weight = new T[nArcs];
  int f, t;
  
  IntegerVector count(n);
  if(!isDirected) {
    for(int i = 0; i < len; i++) {
      f = to_index[from[i]]; t = to_index[to[i]];
      outdeg[f]++; outdeg[t]++;
      indeg[f]++; indeg[t]++;
    }
  } else {
    for(int i = 0; i < len; i++) {
      f = to_index[from[i]]; 
      outdeg[f]++; indeg[t]++;
    }
  }
  
  int cumsum = 0;
  for(int i = 0; i < n; i++) {
    start[i] = cumsum;
    cumsum += count[i];
    end[i] = cumsum;
  }
  
  IntegerVector pos = clone(start);
  
  // printf("YY: %ld %ld\n", from.size(), to.size());
  for(int i = 0; i < len; i++) {
    int f = to_index[from[i]];
    int t = to_index[to[i]];
    // printf("%d %d -- %d %d\n", f, t, pos[f], pos[t]);
    double w = weightV[i];
    target[pos[f]] = t;
    weight[pos[f]] = w;
    pos[f]++;
    target[pos[t]] = f;
    weight[pos[t]] = w;
    pos[t]++;
  }
}

Network::~Network() {
  delete start;
  delete end;
  delete indeg;
  delete outdeg;
  delete target;
  delete weight;
}


DataFrame Network::export_edges(LogicalVector &keep) {
  int keepcount = 0;
  for(i = 0; i < nArcs; i++) {
    if(keep[i]) {
      keepcount++;
    }
  }
  
  int keepSize = isDirected ? keepcount/2 : keepcount;
  IntegerVector outFrom(keepSize);
  IntegerVector outTo(keepSize);
  NumericVector outWeight(keepSize);
  
  int keepPos = 0;
  for(int i = 0; i < n; i++) {
    int f = i;
    for(int j = start[i]; j < end[i]; j++) {
      int t = target[j];
      double w = weight[j];
      // printf("X[%d]: %d->%d: %f: K: %d\n",j, f, t, w, keep[j]);
      if(keep[j]) {
        if(isDirected || f < t) {
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
  int n = nodes.size()
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

#endif