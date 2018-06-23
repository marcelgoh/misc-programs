/* Written by Marcel Goh on 16 May 2018
 * r/dailyprogrammer Challenge #361 [Easy]: Tally Program
 * Takes in a single string as input
 * Outputs score per letter (abcde): +1 for lowercase, -1 for uppercase
 */
#include <stdio.h>

int main(int argc, char *argv[]) {
    char name[5] = {'a', 'b', 'c', 'd', 'e'};
    int score[5] = {0, 0, 0, 0, 0};
    for (int i=0; argv[1][i] != '\0'; ++i) {
        if (argv[1][i] < 'F') {
            --score[argv[1][i]-'A'];
        } else {
            ++score[argv[1][i]-'a'];
        }
    }
    int largest_index, temp_int;
    char temp_char;
    for (int i=0; i<4; ++i) {
        largest_index = i;
        for (int j=i+1; j<5; ++j) {
            if (score[j] > score[largest_index]) {
                largest_index = j;
            }
        }
        if (largest_index != i) {
            temp_int = score[largest_index];
            score[largest_index] = score[i];
            score[i] = temp_int;
            temp_char = name[largest_index];
            name[largest_index] = name[i];
            name[i] = temp_char;
        }
    }
    for (int i=0; i<5; ++i) {
        printf("%c:%d", name[i], score[i]);
        if (i == 4) {
            printf("\n");
        } else {
            printf(", ");
        }
    }
    return 0;
}
