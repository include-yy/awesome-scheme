#include <map>
#include <string>
#include <vector>
#include <math.h>
#include <set>
#include <algorithm>
#include <list>
#include <iostream>
    
using namespace std;

int A[40][40];
int dp[40][1<<20];
int m,n;
int findit(int row,int mask)
{
    if(row==n)
        return 0;
    int retval=0;
    if(dp[row][mask]!=-1)
        return dp[row][mask];
    for(int j=0;j<m;j++)
        if((mask>>j)&1)
        {
            retval=max(retval,A[row][j]+findit(row+1,mask^(1<<j)));
        }
    dp[row][mask]=retval;
    return retval;
}
int main()
{
    scanf("%d%d",&n,&m);
    for(int i=0;i<n;i++)
        for(int j=0;j<m;j++)
            scanf("%d",&A[i][j]);
    for(int j=0;j<40;j++)
        for(int m=0;m<(1<<20);m++)
            dp[j][m]=-1;
    M();
    printf("%d\n",findit(0,(1<<m)-1));
    M();
    
}
