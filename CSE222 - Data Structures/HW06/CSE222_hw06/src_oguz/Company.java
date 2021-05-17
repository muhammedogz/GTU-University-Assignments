package CSE222_hw06.src_oguz;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.ListIterator;
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

    public void read_file(String file) {
        File fp = new File("Data/e-commerce-samples.csv");

        File traderFolder = new File("Temp/Traders");
        traderFolder.mkdirs();
        traderFolder = new File("Temp/Category");
        traderFolder.mkdirs();

        LinkedList<String> sort = new LinkedList<>();

        Scanner scanner = null;
        try {
            scanner = new Scanner(fp);
        } catch (FileNotFoundException e1) {
            e1.printStackTrace();
        }
        scanner.useDelimiter("\n");
        scanner.next(); // skip first line.
        ArrayList<String> dummy = new ArrayList<String>();
        int i = 0;
        File products = new File("Temp/products.csv");

        while(scanner.hasNext())
        {
            String str = scanner.next();
            ArrayList<String> info = new ArrayList<String>(Arrays.asList(str.split(";")));
            String id = info.get(0);
            String product_name = info.get(1);
            String product_category = info.get(2).substring(4, info.get(2).length()-4);
            String price = info.get(3);
            String discount = info.get(4);
            String description = info.get(5);
            String trader = info.get(6);
            StringBuilder all_Info = new StringBuilder(product_name + "," + id + "," + product_category + "," + price +
                            "," + discount + "," + description + "," + trader + "\n");


            sort.add(all_Info.toString());
            // CATEGORY FOLDER
            // int temp_len = product_category.indexOf(">>");
            // if (temp_len == -1) temp_len = product_category.length() - 4;
            // product_category = product_category.substring(4, temp_len).strip();
            // File category_File = new File("Temp/Category/"+product_category +".csv");
            // if (!category_File.exists()) category_File.createNewFile();


            // try (FileWriter category_Writer = new FileWriter(category_File, true);
            // ) {
            //     category_Writer.append(all_Info);
            // } 
            // catch (Exception e) {
            //     e.printStackTrace();
            // }


            // TRADERS FOLDER
            // File trader_File = new File("Temp/Traders/"+ trader+".csv");
            // if (!trader_File.exists()) trader_File.createNewFile();

            // try (FileWriter trader_Writer = new FileWriter(trader_File, true);
            // ) {
            //     trader_Writer.append(all_Info);
            // } 
            // catch (Exception e) {
            //     e.printStackTrace();
            // }
            
            
                
        } // while
        
        sort.sort(new Comparator<String>(){
            @Override
                public int compare(String o1,String o2){
                    return Collator.getInstance().compare(o1,o2);
                }
            });
        scanner.close();
        ListIterator<String> it = sort.listIterator();
        try (FileWriter products_Writer = new FileWriter(products);) {
            while(it.hasNext())
            {
                products_Writer.append(it.next());
            }
            
        } // try
        catch (Exception e) {
            e.printStackTrace();
        }
            
    }
    
}