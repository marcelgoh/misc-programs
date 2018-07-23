#include <stdio.h>
#include <stdlib.h>

/* sudoku_solver: Reads a sudoku puzzle from file and prints the solution.
 * The program accepts plain text files up to 2048 bytes, simply reading the first 81 numbers it 
 * finds as the puzzle to solve.
 * Written by Marcel Goh from 20-22 June 2018
 */

/* utility function to print 1x81 vector to screen as a 9x9 grid */
int print_grid(int *grid) {
    for (int i=0; i<81; ++i) {
        printf("%d ", grid[i]);
        if ((i+1)%9 == 0) {
            printf("\n");
        }
    }
    return 0;
}

/* finds a portion of grid at given index and stores it in part
 * mode: 'r' for row, 'c' for column, 'b' for box
 * part must be able to store at least 9 ints
 * returns 0 if successful, -1 otherwise
 */
int part(int *part, const int *grid, int index, char mode) {
    switch(mode) {
    case 'r':
        for (int i=0; i<9; ++i) {
            part[i] = grid[i + index*9];
        }
        break;
    case 'c':
        for (int i=0; i<9; ++i) {
            part[i] = grid[i*9 + index];
        }
        break;
    case 'b':
        for (int i=0; i<9; ++i) {
            part[i] = grid[i%3 + i/3*9 + index%3*3 + index/3*27];
        }
        break;
    default:
        printf("Error: Invalid mode.");
        return -1;
    }
    return 0;
}

/* returns the row, column, or box the cell belongs to
 * if an error is encountered, returns -1
 */
int which(int cell, char mode) {
    if (cell < 0 || cell > 80) {
        printf("Error: Invalid cell number.");
        return -1;
    }
    switch(mode) {
    case 'r':
        return cell/9;
    case 'c':
        return cell%9;
    case 'b':
        return (cell%9)/3 + (cell/27)*3;
    default:
        printf("Error: Invalid mode.");
        return -1;
    }
}

/* returns 1 if in number in array, else returns 0 */
int check(const int *arr, int num) {
    for (int i=0; i<9; ++i) {
        if (arr[i] == num) {
            return 1;
        }
    }
    return 0;
}

/* returns 1 if number in cell is valid, 0 otherwise */
int safe(int cell, int num, const int *grid) {
    if (num == 0) {
        return 0;
    }
    int ret_val = 1;
    int *checker = (int*) malloc(9*sizeof(int));
    part(checker, grid, which(cell, 'r'), 'r');
    if (check(checker, num)) {
        ret_val = 0;
    }
    part(checker, grid, which(cell, 'c'), 'c');
    if (check(checker, num)) {
        ret_val = 0;
    }
    part(checker, grid, which(cell, 'b'), 'b');
    if (check(checker, num)) {
        ret_val = 0;
    }
    free(checker);
    return ret_val;
}

/* recursive backtrack function: left indicates the number of blanks left to fill
 * the algorithm progresses by guessing a safe number and decreasing left
 * if a dead end is reached, the algorithm adds 1 to left (backtracks)
 * when there are no blanks left and all cells have valid entries, the grid is solved.
 */
int* try(int *grid, const int *blanks, int num_blanks, int left) {
    if (left == 0) {
        return grid;
    } else if (left > num_blanks) {
        printf("A solution was not found.\n");
        return NULL;
    } else {
        int curr = num_blanks - left;
        int cell = blanks[curr];
        for (int num = grid[cell]+1; num<=9; ++num) {
            if (safe(cell, num, grid)) {
                grid[cell] = num;
                return try(grid, blanks, num_blanks, left-1);
            }
        }
        grid[cell] = 0;
        return try(grid, blanks, num_blanks, left+1);
    }

}

/* prepares list of cells to fill then starts the recursive function
 * returns 0 if successful, -1 otherwise
 */
int solve(int *grid) {
    /* indices of all blanks in original grid */
    int *blanks = (int*) malloc(81*sizeof(int));
    int num_blanks = 0;
    for (int i=0; i<81; ++i) {
        if (grid[i] == 0) {
            blanks[num_blanks++] = i;
        }
    }
    grid = try(grid, blanks, num_blanks, num_blanks);
    if (grid == NULL) {
        return -1;
    }

    return 0;
}

int main() {
    char *filename = (char*) malloc(128*sizeof(char));
    int *grid = (int*) malloc(81*sizeof(int));
    char *contents;
    int filesize = 0;

    /* program loop */
    while(1) {
        printf("Enter name of file to load (max 2kB) or 0 to exit: ");
        scanf("%s", filename);
        if (filename[0] == '0') {
            break;
        }

        /* store contents of file in a string */
        FILE *f = fopen(filename, "rb");
        if (!f) {
            printf("Error. File not found.\n");
            continue;
        }
        fseek(f, 0, SEEK_END);
        filesize = ftell(f);
        rewind(f);
        contents = (char*) malloc((filesize + 1)*sizeof(char));
        fread(contents, sizeof(int), filesize, f);
        fclose(f);
        contents[filesize] = '\0';

        /* load first 81 numbers of string into grid vector */
        int num_loaded = 0;
        for (int i=0; contents[i] != '\0' && num_loaded < 81; ++i) {
            if (contents[i] >= '0' && contents[i] <= '9') {
                grid[num_loaded++] = contents[i] - '0';
            }
        }
        free(contents);

        /* print problem and solution grids */
        printf("Problem grid:\n");
        print_grid(grid);
        if (solve(grid) == 0) {
            printf("Solution grid:\n");
            print_grid(grid);
        }
    }
    free(filename);
    free(grid);

    return 0;
}
