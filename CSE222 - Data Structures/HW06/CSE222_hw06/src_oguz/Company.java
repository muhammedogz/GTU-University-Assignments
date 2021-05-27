/**
 * @author Muhammed Oğuz
 * 
 * This class implements ICompany interface.
 */

package CSE222_hw06.src_oguz;

import CSE222_hw06.interface_oguz.ICompany;

import java.text.Collator;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;


public class Company implements ICompany {
    private ArrayList<Trader> cTraders;
    private ArrayList<Customer> cCustomers;

    public Company() {
        cTraders = new ArrayList<>();
        cCustomers = new ArrayList<>();
    }

    @Override
    public void readFile(String filename) {
        File fp = new File(filename);

        // Create Temp Folder
        File tempFolder = new File("Temp/");
        tempFolder.mkdirs();
        
        // this will hold all data and will sorted.
        // lastly write file sorted
        LinkedList<String> sort = new LinkedList<>();
        HashMap<String, Integer> traders = new HashMap<>();
        Integer trader_pass = 1234;

        Scanner scanner = null;
        try {
            scanner = new Scanner(fp);
        } catch (FileNotFoundException e1) {
            e1.printStackTrace();
        }

        scanner.useDelimiter("\n");
        
        scanner.next(); // skip first line.

        while(scanner.hasNext())
        {
            // Move scanner to next position
            String str = scanner.next();

            // Split data from string
            ArrayList<String> info = new ArrayList<String>(Arrays.asList(str.split(";")));
            String id = info.get(0);
            String product_name = info.get(1);
            String product_category = info.get(2).substring(4, info.get(2).length()-4);
            String price = info.get(3);
            String discount = info.get(4);
            String description = info.get(5);
            String trader = info.get(6).strip();

            // Sum up all data.
            StringBuilder all_Info = new StringBuilder(trader + ";" + product_name + ";" + id + ";" + product_category + ";" + price +
                            ";" + discount + ";" + description + "\n");

            // add linked list for sorting.
            sort.add(all_Info.toString());

            if (traders.get(trader) == null)
            {
                traders.put(trader, trader_pass);
            }
                     
        }
        
        // close scanner
        scanner.close();

        writeProductsFile(sort);
        writeTradersFile(traders);

    }

    public boolean signUpTrader(Trader trader) {
        cTraders.add(trader);

        File trader_File = new File("Temp/traders.csv");

        // Append to file to login next time.
        try (FileWriter traders_Writer = new FileWriter(trader_File, true);) {
                traders_Writer.append(trader.getName() + ";" + trader.getPass() + "\n");
        } catch (Exception e) {
            e.printStackTrace();
        }

        return true;
    }

    /**
     * Login as a trader
     * @param trader
     * @return
     */
    public boolean loginTrader(Trader trader)
    {
        File fp = new File("Temp/traders.csv");

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

            if (info.get(0).equals(trader.getName()) && info.get(1).equals(trader.getPass()))
            {
                cTraders.add(new Trader(trader.getName(),trader.getPass()));
                return true;
            }
            
        }

        return false;
    }

    /**
     * Sgin up cusomter
     * @param customer
     * @return
     */
    public boolean signUpCustomer(Customer customer) {
        cCustomers.add(customer);

        File customer_File = new File("Temp/customer.csv");

        // Append to file to login next time.
        try (FileWriter customer_Writer = new FileWriter(customer_File, true);) {
            customer_Writer.append(customer.getName() + ";" + customer.getPass() + "\n");
        } catch (Exception e) {
            e.printStackTrace();
        }

        return true;
    }

    /**
     * Login as a customer
     * @param customer
     * @return
     */
    public boolean loginCustomer(Customer customer)
    {
        File fp = new File("Temp/customer.csv");

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

            if (info.get(0).equals(customer.getName()) && info.get(1).equals(customer.getPass()))
            {
                cTraders.add(new Trader(customer.getName(),customer.getPass()));
                return true;
            }
            
        }

        return false;
    }



    /**
     * Update products file due to givens
     * @param old_str
     * @param new_str
     * @throws IOException
     */
    public void updateProducts(String old_str, String new_str) throws IOException {
        Path path = Paths.get("Temp/products.csv");

        List<String> fileContent = new ArrayList<>(Files.readAllLines(path));
        
        for (int i = 0; i < fileContent.size(); i++) {
            if (fileContent.get(i).equals(old_str)) {
                fileContent.set(i, new_str);
                break;
            }
        }

        Files.write(path, fileContent);
    }

    /**
     * Delete from products file
     * @param product
     * @throws IOException
     */
    public void deleteProducts(Product product) throws IOException {
        Path path = Paths.get("Temp/products.csv");

        List<String> fileContent = new ArrayList<>(Files.readAllLines(path));
        
        for (int i = 0; i < fileContent.size(); i++) {
            if (fileContent.get(i).equals(product.getStringFormat())) {
                fileContent.remove(i);
                break;
            }
        }

        Files.write(path, fileContent);
    }

    public ArrayList<Trader> getcTraders() {
        return cTraders;
    }

    /**
     * Helper function for writing product infos to file as sorted
     * @param sort Sorted list
     */
    private void writeProductsFile(LinkedList<String> sort)
    {
        // File to write
        File products = new File("Temp/products.csv");

        // sort with new comparator. 
        // It allows to ignore upper/lowercase
        sort.sort(new Comparator<String>(){
            @Override
                public int compare(String o1,String o2){
                    return Collator.getInstance().compare(o1,o2);
                }
            });

        // create iterator for taking all elements of list
        ListIterator<String> it = sort.listIterator();

        // Open with try-catch
        try (FileWriter products_Writer = new FileWriter(products);) {
            while(it.hasNext())
            {
                products_Writer.append(it.next());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Helper function for creating traders.csv file with passwords
     * @param traders traders going to added
     */
    private void writeTradersFile(HashMap<String, Integer> traders)
    {
        File trader_File = new File("Temp/traders.csv");

        Iterator<Map.Entry<String, Integer>> it = traders.entrySet().iterator();

        try (FileWriter traders_Writer = new FileWriter(trader_File);) {
            while(it.hasNext())
            {
                Map.Entry<String, Integer> entry = it.next();
                traders_Writer.append(entry.getKey() + ";" + entry.getValue() + "\n");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }


    }

}