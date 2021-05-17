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


    public Company() {

    }

    @Override
    public void readFile(String filename) {
        File fp = new File(filename);

        // Create Temp Folder
        File traderFolder = new File("Temp/Traders");
        traderFolder.mkdirs();
        
        // Create Company Folder
        // traderFolder = new File("Temp/Category");
        // traderFolder.mkdirs();

        LinkedList<String> sort = new LinkedList<>();

        Scanner scanner = null;
        try {
            scanner = new Scanner(fp);
        } catch (FileNotFoundException e1) {
            e1.printStackTrace();
        }

        scanner.useDelimiter("\n");
        
        scanner.next(); // skip first line.

        // File to write
        File products = new File("Temp/products.csv");

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
            String trader = info.get(6);

            // Sum up all data.
            StringBuilder all_Info = new StringBuilder(product_name + ";" + id + ";" + product_category + ";" + price +
                            ";" + discount + ";" + description + ";" + trader + "\n");

            // add linked list for sorting.
            sort.add(all_Info.toString());
            
            // TRADERS FOLDER
            File trader_File = new File("Temp/Traders/"+ trader.strip()+".csv");
            
            try (FileWriter trader_Writer = new FileWriter(trader_File, true);
            ) {
                if (!trader_File.exists()) trader_File.createNewFile();
                trader_Writer.append(all_Info);
            } 
            catch (Exception e) {
                e.printStackTrace();
            }
                    
        }
        
        // close scanner
        scanner.close();

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


    
}