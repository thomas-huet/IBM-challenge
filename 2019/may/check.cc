#include <cstdio>
#include <utility>
#include <queue>
#include <vector>

using namespace std;

#define BITS 8
#define N (1 << BITS)
#define MASK (N - 1)

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
  vector<vector<bool>> ok = vector<vector<bool>>(N, vector<bool>(2, true));
  for(byte f = 0; f < forbidden.size(); ++f) {
    printf("%d", forbidden[f] ? 1 : 0);
    if(forbidden[f]) {
      byte g = f | (N >> 1);
      ok[g][0] = false;
      ok[g][1] = false;
      ok[(byte)(~g & MASK)][0] = false;
      ok[(byte)(~g & MASK)][1] = false;
    }
  }
  printf("\n");
  queue<pair<byte, byte>> q;
  q.emplace(0, 0);
  ok[0][0] = false;
  while(!q.empty()) {
    pair<byte, byte> p = q.front();
    q.pop();
    byte n = p.first;
    byte d = p.second;
    if(n == MASK) {
      return d;
    }
    if((d & 1) == 0) {
      n = ~n & MASK;
    }
    for(byte m : next(n)) {
      if((d & 1) == 0) {
        m = ~m & MASK;
      }
      if(ok[m][d & 1]) {
        ok[m][d & 1] = false;
        q.emplace(m, d + 1);
      }
    }
  }
  return 0;
}

vector<bool> scan() {
  vector<bool> ans = vector<bool>(N / 2);
  for(int i = 0; i < N / 8; ++i) {
    char c;
    scanf("%c", &c);
    if(c < 'a') {
      c -= '0';
    } else {
      c = c - 'a' + 10;
    }
    for(int j = 0; j < 4; ++j) {
      ans[4 * (N / 8 - 1 - i) + j] = ((c & 1) == 1);
      c = c >> 1;
    }
  }
  return ans;
}

int main() {
  printf("%d bits, %d sets\n", BITS, N);
  vector<bool> forbidden = scan();
  printf("%d\n", find_best(forbidden));
  return 0;
}
