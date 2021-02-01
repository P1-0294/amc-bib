#ifndef MIN_INDEX_PQ_H
#define MIN_INDEX_PQ_H

template <class T>
class MinIndexedPQ {
  int NMAX, N, *heap, *index;
  T *keys;
  
  void swap(int i, int j);
  void bubbleUp(int k);
  void bubbleDown(int k);
  
public:
  // Create an empty MinIndexedPQ which can contain atmost NMAX elements
  MinIndexedPQ(int NMAX);
  ~MinIndexedPQ();
  
  bool isEmpty(); // check if the PQ is empty
  bool contains(int i); // check if i is an index on the PQ
  int size(); // return the number of elements in the PQ
  void insert(int i, T key); // associate key with index i; 0 < i < NMAX
  int minIndex(); // returns the index associated with the minimal key
  int minKey(); // returns the minimal key
  
  // delete the minimal key and return its associated index
  // Warning: Don't try to read from this index after calling this function
  int deleteMin();
  T keyOf(int i); // returns the key associated with index i
  void changeKey(int i, T key); // change the key associated with index i to the specified value
  void decreaseKey(int i, T key); // decrease the key associated with index i to the specified value
  void increaseKey(int i, T key);// increase the key associated with index i to the specified value
  void deleteKey(int i); // delete the key associated with index i
};

//////////////////////////////////////////////
template<typename T>
void MinIndexedPQ<T>::swap(int i, int j) {
  int t = heap[i]; heap[i] = heap[j]; heap[j] = t;
  index[heap[i]] = i; index[heap[j]] = j;
}

template<typename T>
void MinIndexedPQ<T>::bubbleUp(int k)    {
  while(k > 1 && keys[heap[k/2]] > keys[heap[k]])   {
    swap(k, k/2);
    k = k/2;
  }
}

template<typename T>
void MinIndexedPQ<T>::bubbleDown(int k)  {
  int j;
  while(2*k <= N) {
    j = 2*k;
    if(j < N && keys[heap[j]] > keys[heap[j+1]])
      j++;
    if(keys[heap[k]] <= keys[heap[j]])
      break;
    swap(k, j);
    k = j;
  }
}


// Create an empty MinIndexedPQ which can contain atmost NMAX elements
template<typename T>   
MinIndexedPQ<T>::MinIndexedPQ(int NMAX)  {
  this->NMAX = NMAX;
  N = 0;
  keys = new T[NMAX + 1];
  heap = new int[NMAX + 1];
  index = new int[NMAX + 1];
  for(int i = 0; i <= NMAX; i++)
    index[i] = -1;
}

template<typename T>
MinIndexedPQ<T>::~MinIndexedPQ() {
  delete [] keys;
  delete [] heap;
  delete [] index;
}

// check if the PQ is empty
template<typename T>
bool MinIndexedPQ<T>::isEmpty()  {
  return N == 0;
}

// check if i is an index on the PQ
template<typename T>
bool MinIndexedPQ<T>::contains(int i)    {
  return index[i] != -1;
}

// return the number of elements in the PQ
template<typename T>
int MinIndexedPQ<T>::size()  {
  return N;
}

// associate key with index i; 0 < i < NMAX
template<typename T>
void MinIndexedPQ<T>::insert(int i, T key) {
  N++;
  index[i] = N;
  heap[N] = i;
  keys[i] = key;
  bubbleUp(N);
}

// returns the index associated with the minimal key
template<typename T>
int MinIndexedPQ<T>::minIndex()  {
  return heap[1];
}

// returns the minimal key
template<typename T>
int MinIndexedPQ<T>::minKey()    {
  return keys[heap[1]];
}

// delete the minimal key and return its associated index
// Warning: Don't try to read from this index after calling this function
template<typename T>
int MinIndexedPQ<T>::deleteMin() {
  int min = heap[1];
  swap(1, N--);
  bubbleDown(1);
  index[min] = -1;
  heap[N+1] = -1;
  return min;
}

// returns the key associated with index i
template<typename T>
T MinIndexedPQ<T>::keyOf(int i)    {
  return keys[i];
}

// change the key associated with index i to the specified value
template<typename T>
void MinIndexedPQ<T>::changeKey(int i, T key)  {
  keys[i] = key;
  bubbleUp(index[i]);
  bubbleDown(index[i]);
}

// decrease the key associated with index i to the specified value
template<typename T>
void MinIndexedPQ<T>::decreaseKey(int i, T key)    {
  keys[i] = key;
  bubbleUp(index[i]);
}

// increase the key associated with index i to the specified value
template<typename T>
void MinIndexedPQ<T>::increaseKey(int i, T key)    {
  keys[i] = key;
  bubbleDown(index[i]);
}
  
// delete the key associated with index i
template<typename T>
void MinIndexedPQ<T>::deleteKey(int i)   {
  int ind = index[i];
  swap(ind, N--);
  bubbleUp(ind);
  bubbleDown(ind);
  index[i] = -1;
}
  
#endif