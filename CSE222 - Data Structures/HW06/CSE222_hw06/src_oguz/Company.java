/**
 * @author Muhammed Oğuz
 * 
 * This class implements ICompany interface.
 */

package CSE222_hw06.src_oguz;

import CSE222_hw06.interface_oguz.ICompany;

import java.text.Collator;
import java.io.*;
import java.util.*;


public class Company implements ICompany {
    private ArrayList<Trader> cTraders;


    public Company() {
        cTraders = new ArrayList<>();
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


    public boolean loginTrader(String name, String pass)
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

            if (info.get(0).equals(name) && info.get(1).equals(pass))
            {
                cTraders.add(new Trader(name,pass));
                return true;
            }
            
        }

        return false;
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