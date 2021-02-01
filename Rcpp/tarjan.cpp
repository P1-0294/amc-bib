#include <Rcpp.h>
#include <stdio.h>
#include <stack>
#include "network.h"
using namespace Rcpp;
using namespace std;

// https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
struct Srecord {
  int node;
  int parent;
  int target_index;
  int state;  
  // 0 start of call of STRONGCONNECT
  // 1 for loop entry processing
  // 2 after recursive call STRONGCONNECT(node), where node is successor of parent
  // and node target of parent at target[target_index]
  // 3 end of successor traversing
};

void printSrecord(Srecord rec) {
  printf("(%d, %d, %d, %d)\n", rec.node, rec.parent, rec.target_index, rec.state);
}

// [[Rcpp::export]]
IntegerVector strongly_connected_components(XPtr<Network> networkPtr) {
  Network &network = *(networkPtr.get()); 
  // network.print_network();
  IntegerVector index(network.V);
  IntegerVector lowlink(network.V);
  LogicalVector onStack(network.V);
  IntegerVector component(network.V);
  for(int i = 0; i < network.V; i++) {
    index[i] = -1;
    lowlink[i] = -1;
    onStack[i] = false;
    component[i] = -1;
  }
  int componentCnt = -1;
  stack<int> S;
  int tmpIndex = 0;
  stack<Srecord> DFS;
  for(int vv = 0; vv < network.V; vv++) {
    if(index[vv] < 0) {
      Srecord rec = {vv, -1, -1, 0}; // strongconnect(vv)
      DFS.push(rec);
      
      while(!DFS.empty()) {   // strongconnect without recursion
        Srecord tmpRec = DFS.top(); DFS.pop();
        if(tmpRec.state == 0) {  //#0 (v,-,-,0)
          int v = tmpRec.node; 
          index[v] = tmpIndex;
          lowlink[v] = tmpIndex;
          tmpIndex++;
          S.push(v);
          onStack[v] = true;
          
          
          int target = network.start[v];
          if(target == network.end[v]) {  // -> #3 (v, -, -, 3) // no successors -> no for loop
            Srecord rec = {v, -1, -1, 3}; DFS.push(rec);
          } else {  // -> #1 (w, v, t, 1) // start the for loop
            int w = network.target[target];
            Srecord rec = {w , v, target, 1}; DFS.push(rec);
          }
        } else if(tmpRec.state == 1) {  // #1 (w, v, t, 1)
          int w = tmpRec.node;
          int v = tmpRec.parent;
          int target = tmpRec.target_index;
          if(index[w] == -1) {
            // -> #2 (w, v, t, 2) // return position
            // -> #0 (w, -, -, 0) // recursive call of strong connect
            Srecord rec1 = {w, v, target, 2}; DFS.push(rec1);
            Srecord rec2 = {w, -1, -1, 0}; DFS.push(rec2);
            continue;
          } else if(onStack[w]) {
            if(index[w] < lowlink[v]) {
              lowlink[v] = index[w];
            }
          } 
          // next run of for loop
          target++;
          if(target == network.end[v]) {  // -> #3 (v, -, -, 3) // end of for loop
            Srecord rec = {v, -1, -1, 3}; DFS.push(rec);
          } else {  // -> #1 (w, v, t, 1)
            int w = network.target[target];
            Srecord rec = {w , v, target, 1}; DFS.push(rec);
          }
        } else if(tmpRec.state == 2) { // #2 (w, v, t, 2) // coming back from recursion call
          int w = tmpRec.node;
          int v = tmpRec.parent;
          int target = tmpRec.target_index;
          if(lowlink[w] < lowlink[v]) {
            lowlink[v] = lowlink[w];
          }
          // prepare next for loop or finish for loop
          target++;
          if(target == network.end[v]) {  // -> #3 (v, -, -, 3) 
            Srecord rec = {v, -1, -1, 3}; DFS.push(rec);
          } else {  // -> #1 (w, v, t, 1)
            int w = network.target[target];
            Srecord rec = {w , v, target, 1}; DFS.push(rec);
          }
        } else {// tmpRec.state == 3  // for loop finished, process component if v is root
          printSrecord(tmpRec);
          int v = tmpRec.node;
          if(lowlink[v] == index[v]) {
            componentCnt++;  // next component index
            while(true) {
              int w = S.top(); S.pop();
              onStack[w] = false;
              component[w] = componentCnt;
              if(v == w) break; 
            }
          }
        } // end ifelse #0 - #3
      } // end of toplevel strongconnect recursion call
    } // end if processing vv
  } // for int vv
  return component;  
}

// https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
// algorithm tarjan is
//     input: graph G = (V, E)
//     output: set of strongly connected components (sets of vertices)
//    
//     index := 0
//     S := empty stack
//     for each v in V do
//         if v.index is undefined then
// -> 0 (v, -, -, 0)
//             strongconnect(v)   			
//         end if
//     end for
//    
//     function strongconnect(v)
// #0 (v,-,-,0)
//         v.index := index
//         v.lowlink := index
//         index := index + 1
//         S.push(v)
//         v.onStack := true
// if no succ: 
// -> #3 (v, -, -, 3)
// else
// -> #1 (w_st, v, t, 1)     
//         for each (v, w) in E do
// 
// #1 (w, v, t, 1)
//             if w.index is undefined then
// -> #2 (w, v, t, 2) // return
// -> #0 (w, -, -, 0) 
//                 strongconnect(w)   
// #2 (w, v, t, 2)
//                 v.lowlink := min(v.lowlink, w.lowlink)
// if next(w)
// -> #1 (next(w), v, t+1, 1)
// else
// -> #3 (v, -, -, 3)
//             else if w.onStack then
//                 v.lowlink := min(v.lowlink, w.index)
//             end if
// if w+1
// -> #1 (w+1, v, t+1, 1)
// else
// -> #3 (v, -, -, 3)
//         end for
// #3 (v, -, -, 3)      
//         if v.lowlink = v.index then
//             start a new strongly connected component
//             repeat
//                 w := S.pop()
//                 w.onStack := false
//                 add w to current strongly connected component
//             while w ≠ v
//             output the current strongly connected component
//         end if
//     end function