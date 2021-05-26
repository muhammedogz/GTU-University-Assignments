package CSE222_hw06.src_oguz;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Scanner;


public class Trader {

    /** Data Fields */
    private String name;
    private String pass;
    private ArrayList<Product> products;
    boolean showDescription = false, showCategory = false;

    public Trader(String name, String pass) {
        System.out.println("Name = " + name);
        this.name = name;
        this.pass = pass;
        this.products = new ArrayList<Product>();
        // load traders product
        loadProducts();
    }


    public void setPass(String pass) {
        this.pass = pass;
    }
    public void setShowCategory(boolean showCategory) {
        this.showCategory = showCategory;
    }
    public void setShowDescription(boolean showDescription) {
        this.showDescription = showDescription;
    }

    public ArrayList<Product> getProducts() {
        return products;
    }

    public void printProducts() {
        Iterator<Product> it = getProducts().iterator();
        StringBuilder str = new StringBuilder();
        str.append("Name\t\tPrice\tDiscount");
        if (showDescription) str.append("\tDescription");
        if (showCategory) str.append("\tCategory");

        System.out.println(str.toString());
        while (it.hasNext()) {
            Product temp = it.next();
            if (showDescription) temp.setShowDescription(true);
            if (showCategory) temp.setShowCategory(true);

            System.out.println(temp);
        }
    }
    
    public void addProduct(Product product) {
        products.add(product);
    }




    /**
     * Call in constructor to load product
     */
    private void loadProducts() {
        File fp = new File("Temp/products.csv");

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

            // if name not match with this trader, jump other product
            if (!info.get(0).equals(name))
                continue;



            // System.out.println(categories);

            Product temp = new Product(info.get(2), info.get(1) ,info.get(4), info.get(5), info.get(6), info.get(0));
            temp.setCategory(new ArrayList<String>(Arrays.asList(info.get(3).split(">>"))));
            products.add(temp);
        }
        scanner.close();
    }

}
