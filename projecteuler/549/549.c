#include "citset.h"
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <unistd.h>

void M(void)
{
  static int i = 0;
  static clock_t t = 0;
  if (i == 0) {
    i = 1;
    t = clock();
  } else if (i != 0) {
    i = 0;
    printf("\n%u \n", clock() - t);
  } else ;
}

int n_and_p(int n, int p)
{
  int p_now = p;
  int tot = 0;
  for (; p_now <= n; p_now *= p) {
    tot += n / p_now;
  }
  return tot;
}
int now_and_p(int n, int p)
{
  if (n % p != 0) {
    return 0;
  }
  int tot = 0;
  while (n % p == 0) {
    n = n / p;
    tot = tot + 1;
  }
  return tot;
}

int main(void)
{
  M();
  const int range = 100000000;
  const int MAX = range + 1;
  cit prime;
  citInit(prime, range, ONE);
  citSetAll(prime, 0, 1, ZERO);
  for (int i = 0; i < range; i++) {
    if (citGet(prime, i) == ONE) {
      for (int j = i + i; j < range; j += i) {
	citSet(prime, j, ZERO);
      }
    }
  }
  int num_of_prime = 0;
  for (int i = 0; i < range; i++) {
    if (citGet(prime, i) == ONE) {
      num_of_prime += 1;
    }
  }
  int *p = (int*)malloc(sizeof(int) * num_of_prime);
  for (int i = 0, k = 0; i < range; i++) {
    if (citGet(prime, i) == ONE) {
      p[k] = i;
      k = k + 1;
    }
  }
  
  int * per = (int*)malloc(sizeof(int) * MAX);
  for (int i = 0; i < MAX; i++) {
    per[i] = 0;
  }

  for (int i = 2; i <= MAX; i++) {
    if (per[i] == 0) {
      per[i] = i;
    }
    for (int j = 0; j < num_of_prime && i * p[j] < MAX; j = j + 1) {
      int n = per[i];
      int need = now_and_p(i * p[j], p[j]);
      while(n_and_p(n, p[j]) < need) {
	n = n + 1;
      }
      per[i * p[j]] = n;
      if (i % p[j] == 0) {
	break;
      }
    }
  }
  long long int sum = 0;
  for (int i = 2; i < MAX; i = i + 1) {
    sum += per[i];
  }
  printf("%lld\n", sum);
  
  citClean(prime), free(p), free(per);
  
  M();
}

/*
time : 13577 ms
value : 476001479068717
*/
