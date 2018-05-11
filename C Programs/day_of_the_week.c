#include <stdio.h>

/* Written by Marcel Goh on 12 January 2018. Adapted from a COMP 202 assignment at McGill University
 * (Originally assigned in Java). The program asks for a date as input from the user and prints the day of
 * the week that day falls on.
 */

int main() {
    int y, m, d;
    printf("Enter date as three integers (Y M D): ");
    scanf("%d %d %d", &y, &m, &d);
    int y0, x, m0, d0;
    y0 = y - (14 - m) / 12;
    x = y0 + y0/4 - y0/100 + y0/400;
    m0 = m + (12*((14-m)/12)) - 2;
    d0 = (d + x + (31*m0/12)) % 7;
    if (d0 == 0) {
        printf("SUNDAY\n");
    } else if (d0 == 1) {
        printf("MONDAY\n");
    } else if (d0 == 2) {
        printf("TUESDAY\n");
    } else if (d0 == 3) {
        printf("WEDNESDAY\n");
    } else if (d0 == 4) {
        printf("THURSDAY\n");
    } else if (d0 == 5) {
        printf("FRIDAY\n");
    } else if (d0 == 6) {
        printf("SATURDAY\n");
    }
    return 0;
}
