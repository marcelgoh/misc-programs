#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int longest(int n, int *nums, int max_val, int verbose, int mod) {
    int ans = 1;
    int max_i = 0;
    int max_j = 0;
    int **dp = malloc(n*sizeof(int *));
    /* second coordinate is the common difference, + max_val to keep it positive */
    for (int i=0; i<n; ++i) {
        dp[i] = malloc((2*max_val+1)*sizeof(int));
        for (int j=0; j<2*max_val+1; ++j) {
            dp[i][j] = 1;
        }
    }
    int *indices = malloc(n*sizeof(int));

    int diff, curr, old, bigger;
    for (int i=1; i<n; ++i) {
        for (int j=0; j<i; ++j) {
            diff = nums[i] - nums[j];
            if (mod) diff = (diff > 0) ? diff : diff + n;
            //if (diff > 0) {
                curr = dp[i][max_val + diff];
                old = dp[j][max_val + diff] + 1;
                bigger = (curr > old) ? curr : old;
                dp[i][max_val + diff] = bigger;
                if (bigger > ans) {
                    ans = bigger;
                    max_i = i;
                    max_j = j;
                }
            //}
        }
    }

    if (verbose&&n==8&&ans==2) {
        for (int i=0; i<n; ++i) {
            printf("%d%s", nums[i], (i==n-1) ? ".\n" : ", ");
        }
    }
    if (verbose&&n==8&&ans==2) printf("max_i: %d\tmax_j: %d\tans: %d\tdiff: %d\n", max_i, max_j, ans, nums[max_i]-nums[max_j]);

    free(indices);
    for (int i=0; i<n; ++i) {
        free(dp[i]);
    }
    free(dp);

    return ans;
}

int factorial(int n) {
    if (n == 0) return 1;
    return n*factorial(n-1);
}

/* runs through all permutations (Knuth Vol. 4A, p. 322) */
void distribution(int max_n, int mod) {
    long int **table = malloc((max_n+1)*sizeof(int *));
    for (int n=0; n<=max_n; ++n) {
        table[n] = malloc((max_n+1)*sizeof(int *));
        for (int k=1; k<=max_n; ++k) {
            table[n][k] = 0;
        }
    }

    for (int n=1; n<max_n; ++n) {
        int *a = malloc((n+1)*sizeof(int));
        int *c = malloc((n+1)*sizeof(int));
        int *o = malloc((n+1)*sizeof(int));
        for (int i=0; i<=n; ++i) {
            a[i] = i;
            c[i] = 0;
            o[i] = 1;
        }
        int partial_sum = 0;
        while (1) {
            int l = longest(n, a, n-1, 1, mod);
            partial_sum += l;
            ++table[n][l];
            int j=n;
            int s = 0;
            while (1) {
                int q = c[j] + o[j];  /* P4 */
                if (q >= 0 && q != j) {  /* P5 */
                    int temp = a[j-c[j]+s-1];
                    a[j-c[j]+s-1] = a[j-q+s-1];
                    a[j-q+s-1] = temp;
                    c[j] = q;
                    break;  /* goto P2 */
                }
                if (q == j) {
                    if (j == 1) {
                        goto exit_loop;  /* terminate */
                    }
                    s = s + 1;
                }
                o[j] = -o[j];
                j = j-1;  /* goto P4 */
            }
        }
exit_loop:
        free(a);
        free(c);
        free(o);
        printf("n=%d: average=%f\n", n, partial_sum*1.0 / factorial(n));
    }
    printf("\t");
    for (int k=1; k<max_n; ++k) {
        printf("k=%d:\t", k);
    }
    printf("\n");
    for (int n=1; n<max_n; ++n) {
        printf("n=%d:\t",n);
        for (int k=1; k<=n; ++k) {
            printf("%ld\t", table[n][k]);
        }
        printf("\n");
    }
    for (int n=0; n<=max_n; ++n) {
        free(table[n]);
    }
    free(table);
}

void shuffle(int n, int *nums) {
    int rand_index, temp;
    for (int i=0; i<n-1; ++i) {
        rand_index = i + rand() % (n-i);
        temp=nums[rand_index];
        nums[rand_index] = nums[i];
        nums[i] = temp;
    }
}

double average(int n, int tries, int mod) {
    int *nums = malloc((n+1)*sizeof(int));
    for (int i=0; i<=n; ++i) nums[i] = i;
    int sum = 0;
    for (int i=0; i<tries; ++i) {
        shuffle(n, nums);
        sum += longest(n,nums,n-1,0,mod);
    }
    free(nums);
    return ((double) sum)/((double)tries);
}

int valid_combs(int t, int n, int mod) {
    int j,s;
    int *d = malloc((t+2)*sizeof(int));
    for (int i=0; i<t; ++i) {
        d[i] = i;
    }
    d[t] = n;
    d[t+1] = 0;
    int count = 0;
    int *a = malloc((t+1)*sizeof(int));
    int *c = malloc((t+1)*sizeof(int));
    int *o = malloc((t+1)*sizeof(int));
    while (1) {  /* Knuth Vol. 4A p. 358 */
        for (int i=0; i<=t; ++i) {
            a[i] = d[i];
            c[i] = 0;
            o[i] = 1;
        }
        while (1) {
            //for (int i=0; i<t; ++i) printf("%d%s", a[i], (i==t-1) ? ".\n" : ", ");
            int found_bad = 0;
            int diff = a[1]-a[0];
            int test_diff;
            if (mod) diff = (diff >= 0) ? diff : diff + n;
            for (int i=1; i<t-1; ++i) {
                test_diff = a[i+1] - a[i];
                if (mod) test_diff = (test_diff >= 0) ? test_diff : test_diff + n;
                if (test_diff != diff) {
                    found_bad = 1;
                    break;
                }
            }
            if (!found_bad) {
                //for (int i=0; i<t; ++i) printf("%d%s", a[i], (i==t-1) ? ".\n" : ", ");
                ++count;
            }
            j=t;
            s = 0;
            while (1) {
                int q = c[j] + o[j];  /* P4 */
                if (q >= 0 && q != j) {  /* P5 */
                    int temp = a[j-c[j]+s-1];
                    a[j-c[j]+s-1] = a[j-q+s-1];
                    a[j-q+s-1] = temp;
                    c[j] = q;
                    break;  /* goto P2 */
                }
                if (q == j) {
                    if (j == 1) {
                        goto exit_loop2;  /* terminate */
                    }
                    s = s + 1;
                }
                o[j] = -o[j];
                j = j-1;  /* goto P4 */
            }
        }
exit_loop2:
        for (j=1; d[j-1]+1 == d[j]; ++j) d[j-1] = j-1;
        if (j>t) break;
        d[j-1] = d[j-1]+1;
    }

    free(a);
    free(c);
    free(o);
    free(d);

    return count;

}

void run_sampling_test(int mod) {
    srand(time(0));
    for (int i=0; i<20; ++i) rand();
    printf("%f\n", log(M_E));
    for (int n=1; n<2500; n+=10) {
        double threshold = 2*log(n)/(log(log(n)));
        double actual = average(n, 10, mod);
        printf("n: %d\t threshold: %.4g\tmeasured: %.6g\tdifference: %.6g\n",
                n, threshold, actual, actual-threshold);
    }
}

void print_combinations_table() {
    int phi[21] = {0,1,1,2,2,4,2,6,4,6,4,10,4,12,6,8,8,16,6,18,8};
    for (int n=1; n<21; ++n) {
        for (int k=1; k<=n; ++k) {
            if (k==1) {
                printf("%d\t", n);
            } else {
                int sum=0;
                for (int r=2; r<k; ++r) {
                    if (n % r == 0) {
                        sum += phi[r];
                    }
                }
                printf("%d\t", n*(n-1 - sum));
                //double computed = (1.0*(n-k+2)*ceil(1.0*(n-1)/(k-1)));
                //printf("%.1f\t", computed);
            }
        }
        printf("\n");
    }
    int row_sum;
    for (int n=1; n<13; ++n) {
        row_sum=0;
        for (int k=1; k<=n; ++k) {
            int x = valid_combs(k,n,1);
            printf("%d\t", x/n);
            row_sum += x;
        }
        printf("\t total: %d\n", row_sum);
    }
}

int main() {
    //run_sampling_test(0);
    distribution(9,1);

    return 0;
}
