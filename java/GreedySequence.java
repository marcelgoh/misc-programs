/* Greedily create a subsequence Kattis problem
 * Written by Marcel Goh on 11 October 2018 */

import java.util.ArrayList;
import java.util.Scanner;

public class Compete {

    public static void main(String args[]) {
        Scanner s = new Scanner(System.in);

        int length = s.nextInt();
        s.nextLine();

        ArrayList<Integer> list = new ArrayList<>();
        int max = 0;
        int curr = 0;
        for (int i=0; i<length; ++i) {
            curr = s.nextInt();
            if (curr >= max) {
                max = curr;
                list.add(curr);
                if (curr == length) {
                    break;
                }
            }
        }


        System.out.println(list.size());
        for (int i=0; i<list.size(); ++i) {
            System.out.print(list.get(i) + " ");
        }
        System.out.println();



    }
}

