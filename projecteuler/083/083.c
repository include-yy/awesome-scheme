#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>

#define RANK 80

int (*matrix)[RANK] = NULL;

void Init(void);
void Destroy(void);
void Createmaplist(void);

int FindReasonMinIndex(int *, int *);
struct maplist
{
    struct
    {
        int x, y;
    } pos;
    int ielements;
    struct
    {
        int x, y;
    } OutPos[4];
};

struct maplist * pmaplist;

int main(void)
{
    Init();
    Createmaplist();

    int * pdists = malloc(sizeof (int) * RANK * RANK);
    int * pvisited = malloc(sizeof (int) * RANK * RANK);

    for (int i = 0; i < RANK * RANK; i++)
    {
        pdists[i] = INT_MAX;
        pvisited[i] = 0;
    }
    pdists[0] = 0;
    pvisited[0] = 1;
    int remained = RANK * RANK - 1;

    for (int i = 0; i < pmaplist[0].ielements; i++)
    {
        int index = pmaplist[0].OutPos[i].x * RANK + pmaplist[0].OutPos[i].y;
        pdists[index] = matrix[pmaplist[0].OutPos[i].x][pmaplist[0].OutPos[i].y];
    }

    while (remained > 0)
    {
        int minIndex = FindReasonMinIndex(pdists, pvisited);
        if (minIndex == 0)
        {
            printf("unconcerned condition\n");
        }
        pvisited[minIndex] = 1;
        remained -= 1;
        int nowIndex = minIndex;

        for (int i = 0; i < pmaplist[nowIndex].ielements; i++)
        {
            int index = pmaplist[nowIndex].OutPos[i].x * RANK + pmaplist[nowIndex].OutPos[i].y;
            if (pdists[index] == INT_MAX)
            {
                pdists[index] = matrix[pmaplist[nowIndex].OutPos[i].x][pmaplist[nowIndex].OutPos[i].y] + pdists[nowIndex];
            }
            else
            {
                if (pdists[index] >
                    matrix[pmaplist[nowIndex].OutPos[i].x][pmaplist[nowIndex].OutPos[i].y] +
                    pdists[nowIndex])
                {
                    pdists[index] = matrix[pmaplist[nowIndex].OutPos[i].x][pmaplist[nowIndex].OutPos[i].y] + pdists[nowIndex];
                }
            }
        }
    }

    printf("%d\n", matrix[0][0] + pdists[RANK * RANK - 1]);
    free(pdists);
    free(pvisited);
    Destroy();
    return 0;
}

void Init(void)
{
    matrix = malloc(sizeof (int) * RANK * RANK);
    pmaplist = malloc(sizeof (struct maplist) * RANK * RANK);

    FILE * fp = NULL;
    if ((fp = fopen("p083_matrix.txt", "r")) == NULL)
    {
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < RANK; i++)
    {
        for (int j = 0; j < RANK; j++)
        {
            fscanf(fp, "%d", matrix[i] + j);
            fgetc(fp);
        }
    }
    fclose(fp);
    fp = NULL;
}

void Destroy(void)
{
    free(pmaplist);
    free(matrix);
}

void Createmaplist(void)
{
    struct maplist * p = NULL;
    for (int i = 0; i < RANK; i++)
    {
        for (int j = 0; j < RANK; j++)
        {
            int EleCount = 0;
            p = pmaplist + RANK * i + j;
            p->pos.x = i;
            p->pos.y = j;
            if (i - 1 >= 0)
            {
                p->OutPos[EleCount].x = i - 1;
                p->OutPos[EleCount].y = j;
                EleCount += 1;
            }
            if (j + 1 < RANK)
            {
                p->OutPos[EleCount].x = i;
                p->OutPos[EleCount].y = j + 1;
                EleCount += 1;
            }
            if (i + 1 < RANK)
            {
                p->OutPos[EleCount].x = i + 1;
                p->OutPos[EleCount].y = j;
                EleCount += 1;
            }
            if (j - 1 >= 0)
            {
                p->OutPos[EleCount].x = i;
                p->OutPos[EleCount].y = j - 1;
                EleCount += 1;
            }
            p->ielements = EleCount;
        }
    }
}

int FindReasonMinIndex(int * distance, int * visited)
{
    int nowindex = 0;
    int nowbig = INT_MAX;
    int i = 0;
    for (i = 1; i < RANK * RANK; i++)
    {
        if (visited[i])
        continue;
        if (nowbig > distance[i])
        {
            nowindex = i;
            nowbig = distance[i];
        }
    }

    if (nowindex == 0)
    {
        return 0;
    }
    else
    {
        return nowindex;
    }
}