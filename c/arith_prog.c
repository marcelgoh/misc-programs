#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

double uniform01() {
    return ((double) rand())/RAND_MAX;
}


int poisson(double lambda) {
    double l = exp(-lambda);
    double p = 1.0;
    int k = 0;
    double u;
    do {
        k += 1;
        u = uniform01();
        p *= u;
    } while (p > l);
    return k-1;
}

int geometric(double p) {
    double u = uniform01();
    return ceil(log(u) / log(1.0-p)) - 1.;
}

int binomial(int n, double p) {
    int sum = 0;
    double u;
    for (int i=0; i<n; ++i) {
        u = uniform01();
        sum += (p<=u);
    }
    return sum;
}


/* runs in O(n^3) time */
void fill_dp(int n, int *nums, int **dp) {
    for (int i=0; i<n; ++i) {
        for (int j=0; j<n; ++j) {
            dp[i][j] = (j>i) ? 2 : 0;
        }
    }
    /* consider every element as a possible second element of an AP */
    for (int j=n-2; j>=1; --j) {
        for (int k=j+1; k<=n-1; ++k) {
            for (int i=j-1; i>=0; --i) {
                if (nums[i] + nums[k] == 2*nums[j] && nums[i] - nums[j] != 0) {
                    /* i, j, k form an arithmetic progression */
                    if (dp[j][k] + 1 > dp[i][j]) {
                        dp[i][j] = dp[j][k] + 1;
                    }
                } else {
                }
            }
        }
    }
}

int print_ap(int n, int *nums, int **dp, int verbose) {
    fill_dp(n, nums, dp);
    int max_i = 0;
    int max_j = 1;
    int max_len = 2;
    for (long int i=0; i<n; ++i) {
        for (long int j=0; j<n; ++j) {
            //printf("%d ", dp[i][j]);
            if (dp[i][j] > max_len) {
                max_i = i;
                max_j = j;
                max_len = dp[i][j];
            }
        }
        //printf("\n");
    }

    int start = nums[max_i];
    int diff = nums[max_j]-start;
    if (verbose) printf("Max length: %d\tStep size: %d\n", max_len, diff);
    for (int i=0; i<max_len; ++i) {
        if (verbose) printf("%d%s", start+i*diff, i == max_len-1 ? ".\n" : ", ");
    }
    return max_len;
}

int iid(int n, int *nums, int **dp, int verbose) {
    /* initialise nums array */
    nums[0] = geometric(0.1);
    int max_num=nums[0];
    int min_num=nums[0];
    for (int i=1; i<n; ++i) {
        //nums[i] = geometric(0.5);
        int r = geometric(0.5);
        nums[i] = r;
        if (nums[i] > max_num) {
            max_num=nums[i];
        }
        if (nums[i] < min_num) {
            min_num=nums[i];
        }
    }
    for (int i=0; i<n; ++i) {
        if (verbose) printf("%d%s", nums[i], i==n-1 ? ".\n" : ", ");
    }
    if (verbose) printf("Min value:%d\tMax value: %d\t", min_num, max_num);
    return print_ap(n, nums, dp, verbose);
}

int sums(int n, int *nums, int **dp, int verbose) {
    /* initialise nums array */
    nums[0] = 0;
    for (int i=1; i<n; ++i) {
        //int u = rand() % 2;
        //nums[i] = (u == 0) ? nums[i-1]-1 : nums[i-1]+1;
        nums[i] = nums[i-1] + geometric(0.5);
    }
    for (int i=0; i<n; ++i) {
        if (verbose) printf("%d%s", nums[i], i==n-1 ? ".\n" : ", ");
    }
    return print_ap(n, nums, dp, verbose);
}

int galton_watson(int n, int *nums, int **dp, int verbose) {
    int max_num;
    do {
        /* initialise nums array */
        max_num=1;
        nums[0] = 1;
        for (int i=1; i<n; ++i) {
            int kids = 0;
            for (int j=0; j<nums[i-1]; ++j) {
                if (verbose) printf("here: %d, %d\n", i, nums[i-1]);
                //kids += poisson(1.0);
                kids += binomial(2, 0.5);
                //kids += geometric(0.5);
            }
            nums[i] = kids;
        }
    } while (nums[n-1] == 0);
    for (int i=0; i<n; ++i) {
        if (verbose) printf("%d%s", nums[i], i==n-1 ? ".\n" : ", ");
    }
    if (verbose) printf("Max value: %d\t", max_num);
    return print_ap(n, nums, dp, verbose);
}

int sequence(int n, int seq[n], int **dp, int verbose) {
    int max_num=0;
    for (int i=0; i<n; ++i) {
        if (verbose) printf("%d%s", seq[i], i==n-1 ? ".\n" : ", ");
    }
    if (verbose) printf("Max value: %d\t", max_num);
    return print_ap(n, seq, dp, verbose);
}


int main() {
    int n=1000;
    int **dp = malloc(n*sizeof(int*));
    for (int i=0; i<n; ++i) {
        dp[i] = malloc(n*sizeof(int));
    }
    int *nums = malloc(n*sizeof(int));
    time_t t;
    srand((unsigned) time(&t));
    for (int i=0; i<10; ++i) {
        rand();
    }
    int seq[] = {1,4,7,2,5,8,3,6,9,16,19,22,17,20,23,18,21,24};
    //sequence(n,seq,dp,1);
    iid(n, nums, dp, 1);
    //sums(n, nums, dp, 1);
    //FILE *fp = fopen("galton_watson.txt", "w");
    //for (int k=1; k<n; ++k) {
    //    fprintf(fp, "%d\t%d\n", k, galton_watson(k, nums, dp, 1));
    //}
    //fclose(fp);

    for (int i=0; i<n; ++i) {
        free(dp[i]);
    }
    free(nums);
    free(dp);

    return 0;
}
