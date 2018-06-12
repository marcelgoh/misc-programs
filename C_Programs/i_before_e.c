#include <stdio.h>

/* Written by Marcel Goh on 12 June 2018. 
 * Based on r/dailyprogrammer Challenge #363 [Easy]
 * The program checks whether words follow the "I before E except after C" rule.
 */

/* Takes word as input and returns 0 if rule is followed, -1 otherwise */
int check(const char *word) {
    int followed = 0;
    for (int i=0; word[i] != '\0'; ++i) {
        if (word[i] == 'e') {
            if (i == 0) {
                if (word[1] == 'i') {
                    followed = -1;
                }
            } else if (word[i+1] == 'i' && word[i-1] != 'c') {
                followed = -1;
            }
        }
        if (word[i] == 'i' && i != 0) {
            if (word[i+1] == 'e' && word[i-1] = 'c') {
                followed = -1;
            }
        }
    }
    return followed;
}

/* Gets word from user then tests word using check() */
int check_word() {
    char

int check_file() {
    return 0;
}


int main() {
    int option = 0;
    int running = 1;

    /* Program loop: provides user with options until program is exited */
    while(running) {
        printf("Choose one of the following options:\n
                1) Check a single word\n
                2) Import file\n
                3) Quit program\n
                >> ");
        scanf("%d", option);
        switch(option) {
            case 1:
                check_word();
                break;
            case 2:
                check_file();
                break;
            case 3: 
                printf("Exiting the program.\n");
                running = 0;
                break;
            default: 
                printf("Invalid input.\n");
        }
    }

    return 0;
}
    


