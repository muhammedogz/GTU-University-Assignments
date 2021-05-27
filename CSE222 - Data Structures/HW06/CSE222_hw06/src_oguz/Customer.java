package CSE222_hw06.src_oguz;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.PriorityQueue;
import java.util.Scanner;


public class Customer {
    private PriorityQueue<Product> orders = new PriorityQueue<Product>();
    private ArrayList<Product> search = new ArrayList<>();
    private String name;
    private String pass;

    public Customer(String name, String pass)
    {
        this.name = name;
        this.pass = pass;
    }

    public boolean searchProducts(String search_val) {
        File fp = new File("Temp/products.csv");
        boolean flag = false;

        Scanner scanner = null;
        try {
            scanner = new Scanner(fp);
        } catch (Exception e1) {
            e1.printStackTrace();
        }

        scanner.useDelimiter("\n");
        int i = 0;

        while (scanner.hasNext())
        {
            String str = scanner.next();

            ArrayList<String> info = new ArrayList<String>(Arrays.asList(str.split(";")));

            if (str.contains(search_val))
            {
                System.out.println("Matched Product ------ " + i+1 + " ------");
                Product temp = new Product(info.get(2), info.get(1) ,info.get(4), info.get(5), info.get(6), info.get(0));
                temp.setCategory(new ArrayList<String>(Arrays.asList(info.get(3).split(">>"))));
                System.out.println(temp.toString());
                this.search.add(temp);
                flag = true;
            }
            
        }

        return flag;
    }

    public void sortByName(boolean type) {
        int size = search.size();
        if (size == 0)
        {
            System.out.println("There is no search value");
            System.out.println("First search products");
            return;
        }    

        if (type == true)
        {
            for (int i = 0; i < size; i++)
            {
                for (int j = i + 1; j < size; j++)
                {
                    if (search.get(i).getName().compareTo(search.get(j).getName()) > 0)
                    {
                        swap(search.get(i), search.get(j));
                    }
                }
            }
        }
        else
        {
            for (int i = 0; i < size; i++)
            {
                for (int j = i + 1; j < size; j++)
                {
                    if (search.get(i).getName().compareTo(search.get(j).getName()) < 0)
                    {
                        swap(search.get(i), search.get(j));
                    }
                }
            }
        }
    
    }

    public void SearchResult() {
        Iterator<Product> it = search.iterator();

        while(it.hasNext())
            System.out.println(it.next());
    }

    private void swap(Product one, Product two) {
        Product temp = new Product(one.getId(), one.getName(), one.getPrice(), one.getDescription(), one.getDescription(), one.getTrader());
        temp.setCategory(one.getCategory());
        one.setId(two.getId());
        one.setName(two.getName());
        one.setPrice(two.getPrice());
        one.setDiscount(two.getDiscount());
        one.setDescription(two.getDescription());
        one.setTrader(two.getTrader());
        one.setCategory(two.getCategory());

        two.setId(temp.getId());
        two.setName(temp.getName());
        two.setPrice(temp.getPrice());
        two.setDiscount(temp.getDiscount());
        two.setDescription(temp.getDescription());
        two.setTrader(temp.getTrader());
        two.setCategory(temp.getCategory());

        
    }

    public void setName(String name) {
        this.name = name;
    }
    public void setPass(String pass) {
        this.pass = pass;
    }

    public PriorityQueue<Product> getOrders() {
        return orders;
    }

    public String getName() {
        return name;
    }
    public String getPass() {
        return pass;
    }

    
}
