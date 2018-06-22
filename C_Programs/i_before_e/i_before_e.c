#include <stdio.h>

/* Written by Marcel Goh on 12 June 2018.
 * Last updated on 22 June 2018.
 * Based on r/dailyprogrammer Challenge #363 [Easy]
 * The program checks whether words follow the "I before E except after C" rule.
 */

/* Takes word as input and returns 1 if rule is followed, 0 otherwise */
int check(const char *word) {
    for (int i=0; word[i] != '\0'; ++i) {
        if (word[i] == 'e' && word[i+1] == 'i') {
            if (i == 0) {
                return 0;
            } else if (word[i-1] != 'c') {
                return 0;
            }
        }
        if (word[i] == 'i' && word[i+1] == 'e') {
            if (i != 0) {
                if (word[i-1] == 'c') {
                    return 0;
                }
            }
        }   
    }
    return 1;
}

/* Gets word from user then tests word using check() */
int check_word() {
    char word[65];
    
    printf("Enter a word to check (max. 64 characters): ");
    scanf("%s", word);
    if (check(word)) {
        printf("True\n\n");
    } else {
        printf("False\n\n");
    }
    
    return 0;
}   

/* Gets filename from user. Reads file line-by-line and returns number
 * of exceptions to spelling rule.
 */
int check_file() {
    char filename[65];

    printf("Enter name of a file to read from (max. 64 characters): ");
    scanf("%s", filename);
    FILE *file = fopen(filename, "r");
    if (!file) {
        printf("Error opening file.\n\n");
        return -1;
    }

    char buffer;
    char word[65];
    int word_length = 0;
    int exceptions = 0;

    fread(&buffer, sizeof(char), 1, file);
    while (!feof(file)) {
        if (buffer == '\n') {
            word[word_length++] = '\0';
            if (!check(word)) {
                ++exceptions;
            }
            word[0] = '\0';
            word_length = 0;
        } else {
            word[word_length++] = buffer;
        }
        fread(&buffer, sizeof(char), 1, file);
    }
    if (!check(word)) {
        ++exceptions;
    }
    fclose(file);
    
    printf("The number of exceptions is: %d\n\n", exceptions);
    return 0;
}

int main() {
    int option = 0;
    int running = 1;

    /* Program loop: provides user with options until program is exited */
    while(running) {
        printf("Choose an option:\n1) Check a single word\n2) Import file\n3) Quit program\n>> ");
        scanf("%d", &option);
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
    


