#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* converts scale to integer representation, returns -1 if invalid */
int scaleToInt(char *note) {
    if (strcmp(note, "C") == 0) {
        return 0;
    } else if (strcmp(note, "C#") == 0 || strcmp(note, "Db") == 0) {
        return 1;
    } else if (strcmp(note, "D") == 0) {
        return 2;
    } else if (strcmp(note, "D#") == 0 || strcmp(note, "Eb") == 0) {
        return 3;
    } else if (strcmp(note, "E") == 0) {
        return 4;
    } else if (strcmp(note, "F") == 0) {
        return 5;
    } else if (strcmp(note, "F#") == 0 || strcmp(note, "Gb") == 0) {
        return 6;
    } else if (strcmp(note, "G") == 0) {
        return 7;
    } else if (strcmp(note, "G#") == 0 || strcmp(note, "Ab") == 0) {
        return 8;
    } else if (strcmp(note, "A") == 0) {
        return 9;
    } else if (strcmp(note, "A#") == 0 || strcmp(note, "Bb") == 0) {
        return 10;
    } else if (strcmp(note, "B") == 0) {
        return 11;
    } else {
        return -1;
    }
}

/* returns integer to add to tonic, -1 if invalid */
int solfegeToInt(char *solfege) {
    if (strcmp(solfege, "Do") == 0) {
        return 0;
    } else if (strcmp(solfege, "Re") == 0) {
        return 2;
    } else if (strcmp(solfege, "Mi") == 0) {
        return 4;
    } else if (strcmp(solfege, "Fa") == 0) {
        return 5;
    } else if (strcmp(solfege, "So") == 0) {
        return 7;
    } else if (strcmp(solfege, "La") == 0) {
        return 9;
    } else if (strcmp(solfege, "Ti") == 0) {
        return 11;
    } else {
        return -1;
    }
}

/*
 * converts integer representation back to note and stores it in note,
 * using sharps or flats according to char sf; returns -1 if invalid
 */
int intToNote(char *note, int scale, char sf) {
     if (sf == 's') {
         if (scale == 0) {
             strcpy(note, "C");
         } else if (scale == 1) {
             strcpy(note, "C#");
         } else if (scale == 2) {
             strcpy(note, "D");
         } else if (scale == 3) {
             strcpy(note, "D#");
         } else if (scale == 4) {
             strcpy(note, "E");
         } else if (scale == 5) {
             strcpy(note, "F");
         } else if (scale == 6) {
             strcpy(note, "F#");
         } else if (scale == 7) {
             strcpy(note, "G");
         } else if (scale == 8) {
             strcpy(note, "G#");
         } else if (scale == 9) {
             strcpy(note, "A");
         } else if (scale == 10) {
             strcpy(note, "A#");
         } else if (scale == 11) {
             strcpy(note, "B");
         } else {
             printf("Invalid input.\n");
             return -1;
         }
    } else if (sf == 'f') {
         if (scale == 0) {
             strcpy(note, "C");
         } else if (scale == 1) {
             strcpy(note, "Db");
         } else if (scale == 2) {
             strcpy(note, "D");
         } else if (scale == 3) {
             strcpy(note, "Eb");
         } else if (scale == 4) {
             strcpy(note, "E");
         } else if (scale == 5) {
             strcpy(note, "F");
         } else if (scale == 6) {
             strcpy(note, "Gb");
         } else if (scale == 7) {
             strcpy(note, "G");
         } else if (scale == 8) {
             strcpy(note, "Ab");
         } else if (scale == 9) {
             strcpy(note, "A");
         } else if (scale == 10) {
             strcpy(note, "Bb");
         } else if (scale == 11) {
             strcpy(note, "b");
         } else {
             printf("Invalid input.\n");
             return -1;
         }
     } else {
         printf("Invalid input.\n");
         return -1;
     }
     return 0;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Usage: %s <scale> <solfege>\n", argv[0]);
        return EXIT_FAILURE;
    }

    int scale = scaleToInt(argv[1]);
    int solfege = solfegeToInt(argv[2]);
    if (scale == -1 || solfege == -1) {
        printf("Invalid input.\n");
        return EXIT_FAILURE;
    }
   
    /* determine whether sharps or flats are used */
    char sf = 0;
    argv[1]++;
    if (*argv[1] != '\0') {
        if (*argv[1] == '#') {
            sf = 's';
        } else if (*argv[1] == 'b') {
            sf = 'f';
        }
    } else {
        switch (scale) {
            case 0: case 2: case 4: case 7: case 9: case 11:
                sf = 's';
                break;
            default:
                sf = 'f';
                break;
        }
    }

    scale = (scale + solfege) % 12;   
    char *note = (char *) malloc(2*sizeof(char));
    intToNote(note, scale, sf);
    printf("%s\n", note);
    free(note);
    return EXIT_SUCCESS;
}
