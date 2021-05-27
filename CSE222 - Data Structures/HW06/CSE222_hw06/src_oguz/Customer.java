package CSE222_hw06.src_oguz;

import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.PriorityQueue;
import java.util.Scanner;


public class Customer {
    private PriorityQueue<String> orders = new PriorityQueue<String>();
    private ArrayList<Product> search = new ArrayList<>();
    private String name;
    private String pass;

    public Customer(String name, String pass)
    {
        this.name = name;
        this.pass = pass;
        
    }

    /**
     * search products
     * @param search_val
     * @return
     */
    public boolean searchProducts(String search_val) {
        File fp = new File("Temp/products.csv");
        boolean flag = false;

        search.clear();

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
                System.out.println("Matched Product ------ " + ++i + " ------");
                Product temp = new Product(info.get(2), info.get(1) ,info.get(4), info.get(5), info.get(6), info.get(0));
                temp.setCategory(new ArrayList<String>(Arrays.asList(info.get(3).split(">>"))));
                System.out.println(temp.toString());
                this.search.add(temp);
                flag = true;
            }
            
        }

        return flag;
    }

    /**
     * Sort by name
     * @param type
     */
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

    /**
     * Sort by price
     * @param type
     */
    public void sortByPrice(boolean type) {
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
                    if (search.get(i).getPrice().compareTo(search.get(j).getPrice()) > 0)
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
                    if (search.get(i).getPrice().compareTo(search.get(j).getPrice()) < 0)
                    {
                        swap(search.get(i), search.get(j));
                    }
                }
            }
        }
    
    }

    /**
     * Sort by discount
     * @param type
     */
    public void sortByDiscount(boolean type) {
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
                    if (search.get(i).getDiscount().compareTo(search.get(j).getDiscount()) > 0)
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
                    if (search.get(i).getDiscount().compareTo(search.get(j).getDiscount()) < 0)
                    {
                        swap(search.get(i), search.get(j));
                    }
                }
            }
        }
    
    }

    /**
     * Sort  by trader name
     * @param name
     * @return
     */
    public boolean searchTraderProducts(String name) {
        File fp = new File("Temp/products.csv");
        boolean flag = false;

        search.clear();

        Scanner scanner = null;
        try {
            scanner = new Scanner(fp);
        } catch (Exception e1) {
            e1.printStackTrace();
        }

        scanner.useDelimiter("\n");

        while (scanner.hasNext())
        {
            String str = scanner.next();

            ArrayList<String> info = new ArrayList<String>(Arrays.asList(str.split(";")));

            if (info.get(0).equals(name))
            {
                Product temp = new Product(info.get(2), info.get(1) ,info.get(4), info.get(5), info.get(6), info.get(0));
                temp.setCategory(new ArrayList<String>(Arrays.asList(info.get(3).split(">>"))));
                System.out.println(temp.toString());
                this.search.add(temp);
                flag = true;
            }
            
        }

        return flag;
    }

    /**
     * print search res
     * @param description
     * @param category
     */
    public void SearchResult(boolean description, boolean category) {
        Iterator<Product> it = search.iterator();
        Integer i  = 1;
        while(it.hasNext())
        {

            Product temp = it.next();
            temp.setShowCategory(category);
            temp.setShowDescription(description);
            System.out.println((i++).toString() + "- " + temp);
        }
            
    }

    private void swap(Product one, Product two) {
        Product temp = new Product(one.getId(), one.getName(), one.getPrice(), one.getDiscount(), one.getDescription(), one.getTrader());
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

    
    public void orderProduct(int index) {
        Product temp = search.get(index - 1);

        File orders_File = new File("Temp/orders.csv");

        // Append to file to login next time.
        try (FileWriter orders_Writer = new FileWriter(orders_File, true);) {
            orders_Writer.append(temp.getId() + ";" + this.name + ";" + temp.getTrader() + "\n");
        } catch (Exception e) {
            e.printStackTrace();
        }

    }


    public void setName(String name) {
        this.name = name;
    }
    public void setPass(String pass) {
        this.pass = pass;
    }

    public PriorityQueue<String> getOrders() {
        return orders;
    }

    public String getName() {
        return name;
    }
    public String getPass() {
        return pass;
    }

    

    
}
