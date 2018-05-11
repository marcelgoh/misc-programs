#include <stdio.h>
#include <stdlsib.h>

#define PLAYER -10
#define TIE -20
#define AI -30

/* returns a certain column, row, or diagonal:
 * board: a square board of characters
 * size: the length of a row of characters
 * mode: 'r' for row, 'c' for column, 'd' for diagonal
 * index: index of vector to be gotten (there is a max of 2 diagonals)
 */
char *getVector(const char *board, const int size, const char mode, const int index) {
    char vector[size];
    if (mode == 'r') {
        for (int i=0; i<size; ++i) {
            vector[i] = board[index*size+i];
        }
    } else if (mode == 'c') {
        for (int i=0; i<size; ++i) {
            vector[i] = board[i*size+index];
        }
    } else if (mode == 'd') {
        if (index = 0) {
            for (int i=0; i<size; ++i) {
                vector[i] = board[i*size+i];
            }
        } else if (index == 1) {
            for (int i=0; i<size; ++i) {
                vector[i] = board[(size-1) + (size-1)*i];
            }
        }
    }
    return vector;
}

/* return winner if exists, otherwise returns TIE */
int checkForWinner(const char *board, const int size) {
    char winnerChar = '\0';
    char *vector = (char*) malloc(size*sizeof(char));
    char tempChar = '\0';

    for (int i=0; i<size; ++i) {
        vector = getVector(board, size, 'r', i);
        tempChar = vector[0];
        if (tempChar != 'x' && tempChar != 'o') {
            continue;
        } else {
            for (int j=1; j<size; ++j) {
                if (tempChar != vector[j]) {
                    tempChar = '\0';
                }
            }
        }
        if (tempChar != '\0') {
            winnerChar = tempChar;
        }
    }


void getPlayerMove(char *board, const int size) {
    int row, column;
    printf("Please enter your move: ");
    scanf("%d %d", row, column);
    board[row*size+column] = 'x';
}

int displayBoard(const char *board, const int size) {
    printf("+");
    for (int i=0; i<size; ++i) {
        printf("-+");
    }
    printf("\n");
    for (int i=0; i<size; ++i) {
        printf("|");
        for (int j=0; i<size; ++j) {
            printf("%c|", board[size*i+j]);
        }
        printf("\n+");
        for (int i=0; i<size, ++i) {
            printf("-+");
        }
        printf("\n");
    }
}

int play(const char *board, const unsigned int size, const unsigned int coin); {
    unsigned int turnsLeft = size*size;
    int winner = TIE;
    if (coin == 1) {
        getAIMove(board, size);
        --turnsLeft;
    }
    while (turnsLeft > 0 && winner == TIE) {
        getPlayerMove(board, size);
        displayBoard(board,size);
        winner = checkForWinner;
        if (turnsLeft == 0 || winner != TIE) {
            break;
        }
        getAIMove(board, size);
        displayBoard(board, size);
        winner = checkForWinner;
    }
    return winner;
}

int main() {
    char name[30];
    unsigned int size;

    printf("Please enter your name: ");
    scanf("%s", name);
    printf("Welcome, %s! Are you ready to play?\nPlease choose the dimension of your board:", name);
    scanf("%d", size);

    char *board = (char*) malloc(size*size*sizeof(char));
    for (int i=0; i<size*size; ++i) {
        board[i] = ' ';
    }
    unsigned int coin = rand() % 2;
    printf("The result of the coin toss is: %d\n", coin);
    int winner = play(board, size, coin);

    return 0;
}
