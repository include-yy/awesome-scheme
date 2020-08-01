#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void Me(void)
{
    static int i;
    static clock_t t;
    if (i == 0)
    {
        t = clock();
        i = 1;
    }
    else
    {
        printf("\n%u\n", clock() - t);
        i = 0;
    }
}
#define RANK 9
#define NUM 50
int (*matrix)[RANK][RANK];

struct fixpoint
{
    struct {int x; int y;} pos;
    int CurrentChoice;
    int usefulpoints[10];
};

void ReadSuDoku(void);
int SumMatrixZeros(int (*)[RANK]);
void MakeSurePos(int id, struct fixpoint * array, int iZeros);
void FindPossibleSolutions(int id, struct fixpoint * p);

int main(void)
{
    ReadSuDoku();
    int total = 0;
    Me();
    for (int items = 0; items < NUM; items++)
    {
        int iZeros = SumMatrixZeros(matrix[items]);
        struct fixpoint * pfpoints = malloc(sizeof (struct fixpoint) * iZeros);
        MakeSurePos(items, pfpoints, iZeros);

        int CurrPos = 0;
        while (CurrPos < iZeros)
        {
            if (CurrPos < 0)
            {
                printf("Unexpected error!\n");
                exit(EXIT_FAILURE);
            }

            if (pfpoints[CurrPos].CurrentChoice == 0)
            {
                FindPossibleSolutions(items, pfpoints + CurrPos);
                int tempi = 0;
                for (tempi = pfpoints[CurrPos].CurrentChoice; tempi < 10; tempi++)
                {
                    if (pfpoints[CurrPos].usefulpoints[tempi] != 0)
                    {
                        break;
                    }
                }
                if (tempi == 10)
                {
                    pfpoints[CurrPos].CurrentChoice = 0;
                    CurrPos -= 1;
                    continue;
                }
                else
                {
                    pfpoints[CurrPos].CurrentChoice = tempi;
                    matrix[items][pfpoints[CurrPos].pos.x][pfpoints[CurrPos].pos.y] = tempi;
                    pfpoints[CurrPos].usefulpoints[tempi] = 0;
                    CurrPos += 1;
                }
            }
            else
            {
                int tempi = 0;
                for (tempi = pfpoints[CurrPos].CurrentChoice; tempi < 10; tempi ++)
                {
                    if (pfpoints[CurrPos].usefulpoints[tempi] != 0)
                    {
                        break;
                    }
                }
                if (tempi == 10)
                {
                    pfpoints[CurrPos].CurrentChoice = 0;
                    matrix[items][pfpoints[CurrPos].pos.x][pfpoints[CurrPos].pos.y] = 0;
                    CurrPos -= 1;
                    continue;
                }
                else
                {
                    pfpoints[CurrPos].CurrentChoice = tempi;
                    matrix[items][pfpoints[CurrPos].pos.x][pfpoints[CurrPos].pos.y] = tempi;
                    pfpoints[CurrPos].usefulpoints[tempi] = 0;
                    CurrPos += 1;
                }
            }
        }
        free(pfpoints);
        total += matrix[items][0][0] * 100 +
            matrix[items][0][1] * 10 +
            matrix[items][0][2] * 1;
        //*
        for (int i = 0; i < RANK; i++)
        {
            for (int j = 0; j < RANK; j++)
            {
                printf("%d", matrix[items][i][j]);
            }
            putchar('\n');
        }
        putchar('\n');
        //*/
    }
    printf("%d\n", total);
    Me();
    free(matrix);
    return 0;
}


void ReadSuDoku(void)
{
    if ((matrix = malloc(sizeof(int) * RANK * RANK * NUM)) == NULL)
    {
        printf("memory alloc error!\n");
        exit(EXIT_FAILURE);
    }
    FILE * fp = NULL;
    char buffer[20];
    if ((fp = fopen("p096_sudoku.txt", "r")) == NULL)
    {
        printf("open file error occur!\n");
        exit(EXIT_FAILURE);
    }
    for (int i = 0; i < 50; i++)
    {
        fgets(buffer, sizeof (buffer), fp);
        for (int j = 0; j < RANK; j++)
        {
            for (int k = 0; k < RANK; k++)
            {
                matrix[i][j][k] = fgetc(fp) - '0';
            }
            fgetc(fp);
        }
    }
    fclose(fp);
}

int SumMatrixZeros(int (*mat)[RANK])
{
    int sum = 0;
    for (int i = 0; i < RANK; i++)
    {
        for (int j = 0; j < RANK; j++)
        {
            if (mat[i][j] == 0)
            {
                sum = sum + 1;
            }
        }
    }

    return sum;
}

void MakeSurePos(int id, struct fixpoint * fp, int iZeros)
{
    int iCounts = 0;
    for (int i = 0; i < RANK; i++)
    {
        for (int j = 0; j < RANK; j++)
        {
            if (matrix[id][i][j] == 0)
            {
                fp[iCounts].pos.x = i;
                fp[iCounts].pos.y = j;
                fp[iCounts].CurrentChoice = 0;
                for (int i = 0; i < 10; i++)
                {
                    fp[iCounts].usefulpoints[i] = 0;
                }
                iCounts += 1;
                if (iCounts > iZeros)
                {
                    printf("array up");
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
}

void FindPossibleSolutions(int id, struct fixpoint * p)
{
    p->usefulpoints[0] = 0;
    for (int i = 1; i < 10; i++)
    {
        p->usefulpoints[i] = 1;
    }
    p->CurrentChoice = 0;

    for (int i = 0; i < RANK; i++)
    {
        p->usefulpoints[matrix[id][i][p->pos.y]] = 0;
    }
    for (int j = 0; j < RANK; j++)
    {
        p->usefulpoints[matrix[id][p->pos.x][j]] = 0;
    }

    int xbound = p->pos.x / 3;
    int ybound = p->pos.y / 3;

    for (int i = xbound * 3; i < xbound * 3 + 3; i++)
    {
        for (int j = ybound * 3; j < ybound * 3 + 3; j++)
        {
            p->usefulpoints[matrix[id][i][j]] = 0;
        }
    }

}

//time : 190ms