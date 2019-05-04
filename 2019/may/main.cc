#include <cstdlib>
#include <cstdio>
#include <ctime>
#include <utility>
#include <queue>
#include <vector>

using namespace std;

typedef unsigned char byte;

const int TMAX = 10000;

vector<byte> next(byte n) {
  vector<byte> m;
  for(byte i = n; i != 0; i &= i - 1) {
    m.push_back(n ^ (i & -i));
    for(byte j = i & (i - 1); j != 0; j &= j - 1) {
      m.push_back(n ^ (i & -i) ^ (j & -j));
    }  
  }
  return m;
}

int find_best(const vector<bool>& forbidden) {
  vector<vector<bool>> ok = vector<vector<bool>>(256, vector<bool>(2, true));
  for(byte f = 0; f < forbidden.size(); ++f) {
    if(forbidden[f]) {
      ok[f][0] = false;
      ok[f][1] = false;
      ok[(byte)(~f)][0] = false;
      ok[(byte)(~f)][1] = false;
    }
  }
  queue<pair<byte, byte>> q;
  q.emplace(0, 0);
  ok[0][0] = false;
  while(!q.empty()) {
    pair<byte, byte> p = q.front();
    q.pop();
    byte n = p.first;
    byte d = p.second;
    if(n == 255) {
      return d;
    }
    if((d & 1) == 0) {
      n = ~n;
    }
    for(byte m : next(n)) {
      if((d & 1) == 0) {
        m = ~m;
      }
      if(ok[m][d & 1]) {
        ok[m][d & 1] = false;
        q.emplace(m, d + 1);
      }
    }
  }
  return 0;
}

void print(const vector<bool>& forbidden) {
  for(int i = 0; i < 32; ++i) {
    byte c = 0;
    for(int j = 0; j < 4; ++j) {
      c = c << 1;
      if(forbidden[4 * i + j]) {
        ++c;
      }
    }
    printf("%x", c);
  }
}

vector<int> random_order(int n) {
  vector<int> order = vector<int>(n);
  for(int i = 0; i < n; ++i) {
    order[i] = i;
  }
  for(int i = 0; i < n; ++i) {
    swap(order[i], order[i + rand() % (n - i)]);
  }
  return order;
}

int main() {
  srand(time(0));
  vector<bool> forbidden = vector<bool>(128, false);
  vector<vector<bool>> best;
  best.push_back(forbidden);
  int record = 0;
  int b = 13;
  int c = 13;
  while(true) {
    for(int t = 0; t < TMAX; ++t) {
      int r;
      while(r = rand() % 127, forbidden[r]);
      forbidden[r] = true;
      c = find_best(forbidden);
      if(c < b) {
	forbidden = best.back();
      } else if(c > b) {
	b = c;
	for(int i : random_order(127)) {
	  if(forbidden[i]) {
	    forbidden[i] = false;
	    c = find_best(forbidden);
	    if(c < b) {
	      forbidden[i] = true;
	    }
	  }
	}
	best.push_back(forbidden);
        if(b > record) {
          record = b;
	  printf("%d: ", b);
	  print(best.back());
          printf("\n");
          fflush(stdout);
        }
        t = 0;
      }
    }
    while(best.size() > 1 && rand() % 2 == 0) {
      best.pop_back();
    }
    forbidden = best.back();
    b = find_best(forbidden);
  }
  return 0;
}
