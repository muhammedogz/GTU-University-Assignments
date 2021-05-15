package CSE222_hw06.src_oguz;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;


/**
 * Company
 */
@SuppressWarnings("unused")
public class Company {
    private int a = 0;

    public Company() {
        this.a = 0;
    }

    public void read_file(String file) throws FileNotFoundException  {
        String path = getClass().getResource("").getPath() + file + "domates" ;
        System.out.println(path);
        File fp = new File("e-commerce-samples.csv");
        try {
            System.out.println(fp.getCanonicalPath());
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.out.println("::(((");
        Scanner scanner = new Scanner(fp);
        scanner.useDelimiter("\n");
        int i = 0;
        while(scanner.hasNext())
        {
            if (i == 0 || i == 1) 
            {
                i++;
                continue;
            }
            String str = scanner.next();
            ArrayList<String> arr = new ArrayList<String>(Arrays.asList(str.split(";")));
            System.out.println("ID: " + arr.get(0));
            System.out.println("product_name: " + arr.get(1));
            ArrayList<String>sa = new ArrayList<String>(Arrays.asList(arr.get(2).substring(4, arr.get(2).length()-4).split(">>")));
            for (int j = 0; j < sa.size(); j++) System.out.println("product_category_tree: " + sa.get(j));


            
            

            System.out.println("-------------------------------");

        }
        scanner.close();
        
    }
    public static void main(String[] args) {
        Company co = new Company();
        try {
            co.read_file("makefilee");
        } catch (FileNotFoundException e) {
            
            e.printStackTrace();
        }
    }
}