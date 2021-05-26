package CSE222_hw06.src_oguz;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
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

    public Trader(String name, String pass, boolean first) {
        System.out.println("Name = " + name);
        this.name = name;
        this.pass = pass;
        this.products = new ArrayList<Product>();
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
        str.append("ID - Name - Price - Discount");
        if (showDescription) str.append(" - Description");
        if (showCategory) str.append(" - Category");

        int i = 0;
        System.out.println(str.toString());
        while (it.hasNext()) {
            System.out.println("\nProduct ----" + (i+1)+"----" );
            Product temp = it.next();
            if (showDescription) temp.setShowDescription(true);
            if (showCategory) temp.setShowCategory(true);
            System.out.println(temp);
            i++;
        }
    }
    
    public void addProduct(Product product) {
        products.add(product);

        // add products file to store products
        StringBuilder temp = new StringBuilder();

        temp.append(product.getTrader()+";"+product.getName()+";"+product.getId()+";");
        Iterator<String> it = product.getCategory().iterator();
        while (it.hasNext())
        {
            temp.append(it.next()+" >> ");
        }
        temp.deleteCharAt(temp.length() - 1);
        temp.deleteCharAt(temp.length() - 1);
        temp.deleteCharAt(temp.length() - 1);
        temp.deleteCharAt(temp.length() - 1);
        temp.append(";"+product.getPrice()+";"+product.getDiscount()+";"+product.getDescription());

        File products = new File("Temp/products.csv");

        try (FileWriter products_Writer = new FileWriter(products, true);) {
            products_Writer.append(temp);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    public String getName() {
        return name;
    }
    public String getPass() {
        return pass;
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
