#include <stdio.h>
#include <time.h>

int main(void)
{
  int i;
  int sum = 0;
  for (i = 0; i <= 1 << 30; i = i + 1) {
    if (!(i ^ (2 * i) ^ (3 * i))) {
      sum = sum + 1;
    }
  }
  printf("%d\n", sum);
  return 0;
}
    
// 2624 ms
// 2178310
