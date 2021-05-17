package CSE222_hw06.src_oguz;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

public class Trader {

    private String name;
    private ArrayList<Product> products;

    public Trader(String name) {
        System.out.println("Name = " + name);
        this.name = name;
        this.products = new ArrayList<Product>();
        loadProducts();
    }


    private void loadProducts() {
        File fp = new File("Temp/Traders/"+ this.name + ".csv");
        System.out.println("Temp/Traders/"+this.name + ".csv");
        if (!fp.exists())
        {
            System.err.println("There is no trader with this name " + name );
            System.err.println("Path = " + fp.getAbsolutePath().toString() );
            System.exit(1);
        }

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

            Product temp = new Product(info.get(1), info.get(0), info.get(3), info.get(4), info.get(5), info.get(6));
            
            products.add(temp);
        }
        scanner.close();
    }

    public ArrayList<Product> getProducts() {
        return products;
    }

}
