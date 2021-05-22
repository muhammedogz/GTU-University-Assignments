package CSE222_hw06.src_oguz;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;

import java.util.Scanner;


public class Trader {

    /** Data Fields */
    private String name;
    private String pass;
    private ArrayList<Product> products;

    public Trader(String name, String pass) {
        System.out.println("Name = " + name);
        this.name = name;
        this.pass = pass;
        this.products = new ArrayList<Product>();
        loadProducts();
    }



    private void loadProducts() {
        File fp = new File("Temp/products.csv");
        boolean flag = false;

        Scanner scanner = null;
        try {
            scanner = new Scanner(fp);
        } catch (FileNotFoundException e1) {
            e1.printStackTrace();
        }

        scanner.useDelimiter("\n");
        while (scanner.hasNext())
        {
            String str = scanner.next();

            ArrayList<String> info = new ArrayList<String>(Arrays.asList(str.split(";")));
            if (!info.get(0).equals(name))
            {
                if (flag) break;
                continue;
            } 
            else flag = true;

            // System.out.println(categories);

            Product temp = new Product(info.get(2), info.get(1) ,info.get(4), info.get(5), info.get(6), info.get(0));
            temp.setCategory(new ArrayList<String>(Arrays.asList(info.get(3).split(">>"))));
            products.add(temp);
        }
        scanner.close();
    }

    public ArrayList<Product> getProducts() {
        return products;
    }

}
