/**
 * @author Muhammed Oğuz
 * 
 * This class Test all methods and provides a menu for testing CSE222_hw06 classes.
 */

import CSE222_hw06.src_oguz.*;

import java.util.*;


public class Test {

    public static Integer id_generator = 1000001;
    public static void main(String[] args) {
        Company co = new Company();
        co.readFile("Data/e-commerce-samples.csv");

        while (true) {
            try {
            System.out.println("1-Sign Up Trader");
            System.out.println("2-Login Trader (Hint for test, All Passwords are 1234, Try @home and 1234");
            System.out.println("3-Sign Up Customer");
            System.out.println("4-Exit");

            int choice = getInt("CHoice:");

            switch (choice) {
                case 1:
                    Trader trader = getTrader();
                    co.signUpTrader(trader);
                    traderMenu(co, trader);
                    break;
            
                case 2:
                    Trader temp = getTrader();
                    System.out.println("Login State + " + co.loginTrader(temp));
                    if (!co.loginTrader(temp))
                        break;
                    traderMenu(co, temp);
                    break;

                case 3:
                    Customer customer = new Customer(getStr("Name:"), getStr("Pass:"));
                    co.signUpCustomer(customer);
                    CustomerMenu(customer);
                    break;
                
                case 4:
                System.out.println("Goodbye !!");
                    return;

                default:
                    System.out.println("Invalid");
                    break;
            }
        } catch (Exception e) {
            System.out.println("Exception Handled");
            System.out.println(e.toString());
        }
        }
        
        
    }

    public static void traderMenu(Company company, Trader trader) {
        System.out.println("Welcome to board " + trader.getName());
        while (true) {
        try {
            System.out.println("\nChose the following");
            System.out.println("1-See all your Products");
            System.out.println("2-Add Products");
            System.out.println("3-Edit Products");
            System.out.println("4-Remove Products");
            System.out.println("5-See Order List");
            System.out.println("6-Exit Trader Panel");
            System.out.println("7-Show Description info for products");
            System.out.println("8-Show Category info for products");

            int choice = getInt("Choice:");

            switch (choice) {
                case 1:
                    trader.printProducts();
                    break;
                
                case 2:
                    Product product = getProduct(trader);
                    trader.addProduct(product);
                    break;

                case 3:
                    System.out.println("Enter desired product number for editing");
                    trader.printProducts();
                    Product edit = null;
                    int edit_choice = getInt("Choice:");
                    try {
                        edit = trader.getProducts().get(edit_choice - 1);
                    } catch (Exception e) {
                        System.err.println("Invalid input");
                        System.err.println(e.toString());
                    }
                    if (edit == null) continue;
                    String old_str = edit.getStringFormat();
                    edit = editProduct(edit, trader);
                    company.updateProducts(old_str, edit.getStringFormat());
                    break;
                
                case 4:
                    System.out.println("Enter desired product number for removing");
                    trader.printProducts();
                    Product remove = null;
                    int remove_choice = getInt("Choice:");
                    try {
                        remove = trader.getProducts().get(remove_choice - 1);
                    } catch (Exception e) {
                        System.err.println("Invalid input");
                        System.err.println(e.toString());
                    }
                    if (remove == null) continue;
                    trader.deleteProduct(remove_choice);
                    company.deleteProducts(remove);
                    break;

                case 5:
                    System.out.println("See order list is being constructed! Try Again Later");
                    break;

                case 6:
                    System.out.println("Exiting trader panel!");
                    return;

                case 7:
                    System.out.println("Showing Description option opened");
                    trader.setShowDescription(true);
                    break;

                case 8:
                    System.out.println("Showing category option opened");
                    trader.setShowCategory(true);
                    break;
                
                default:
                    System.out.println("Invalid Option");
                    break;
            }
        } // try block
        catch (Exception e) {
            System.err.println("Error handled");
            System.err.println(e.toString());
        }
        }

    }

    public static Product editProduct(Product edit, Trader trader) {
        
        while (edit != null) {
        
            System.out.println("Which values do you want to edit?");
            System.out.println("1-Change name\n2-Change description\n3-Change price\n4-Change discount\n5-Change category\n6-Finish Editing");

            int choice = getInt("Choice:");

            switch (choice) {
                case 1:
                    edit.setName(getStr("Name:"));
                    break;
            
                case 2:
                    edit.setDescription(getStr("Description:"));
                    break;
                
                case 3:
                    edit.setPrice(getStr("Price"));
                    break;

                case 4:
                    edit.setDiscount(getStr("Discount:"));
                    break;
                    
                case 5:
                    ArrayList<String> category = new ArrayList<String>();
                    String str = "new String()";
                    int i = 1;
                    System.out.println("When finish entering category levels, type -1");
                    while(!str.equals("-1"))
                    {
                        str = getStr("Category level :" + i++ + ">> ");
                        if (str.equals("-1")) break;
                        category.add(str);
                    }
                    
                    edit.setCategory(category);
                    break;

                case 6:
                    return edit;

                    default:
                    System.out.println("Invalid operation");
                    break;
            }
        }

        return null;
        

    }

    public static void CustomerMenu(Customer customer) {
        System.out.println("Welcome board + " + customer.getName());
        boolean showC = false, showD = false;
        while (true)
        {
        try
        {
            System.out.println("0-Search products by name and description");
            System.out.println("1-See Search Results (You should use after all search actions to see result)");
            System.out.println("2-Sort search results by name ascending");
            System.out.println("3-Sort search results by name descending");
            System.out.println("4-Sort search results by price ascending");
            System.out.println("5-Sort search results by price descending");
            System.out.println("6-Sort search results by discount percentage ascending");
            System.out.println("7-Sort search results by discount percentage descending");
            System.out.println("8-See a trader's products");
            System.out.println("9-Order Product from select search results");
            System.out.println("10-Open Category info");
            System.out.println("11-Open Description info");
            System.out.println("12-See orders");
            System.out.println("13-Exit to upper menu");
            int choice = getInt("Choice:");

            switch (choice) {
                case 0:
                    customer.searchProducts(getStr("Enter a word for search:"));
                    break;

                case 1:
                    customer.SearchResult(showD, showC);
                    break;

            
                case 2:
                    customer.sortByName(true);
                    break;

                case 3:
                    customer.sortByName(false);
                    break;

                case 4:
                    customer.sortByPrice(true);
                    break;

                case 5:
                    customer.sortByPrice(false);
                    break;

                case 6:
                    customer.sortByDiscount(true);
                    break;

                case 7:
                    customer.sortByDiscount(false);

                case 8:
                    customer.searchTraderProducts(getStr("Enter trader name:"));
                    break;
                    
                case 9:
                    customer.orderProduct(getInt("Enter index:"));
                    break;

                case 10:
                    showC = true;
                    break;

                case 11:
                    showD = true;
                    break;

                case 12:
                    Iterator<String> it = customer.getOrders().iterator();
                    while(it.hasNext())
                    {
                        customer.searchProducts(it.next());
                        customer.SearchResult(false, false);
                    }
                    break;

                case 13:
                    return;

                default:
                    System.out.println("Invalid operation");
                    break;
            }
        } catch (Exception e) {
            System.err.println("Error handled");
            System.out.println(e.toString());
        }
        }
    

    }

    /**
     * Helper function for getting integer input easily
     * 
     * @param str Desired text for showing.
     * @return Scanned integer
     */
    @SuppressWarnings("resource")
    public static int getInt(String str) {
        System.out.print(str);
        Scanner scanner = new Scanner(System.in);

        int temp = -1;
        while (temp == -1) {
            try {
                temp = scanner.nextInt();
            } catch (InputMismatchException e) {
                System.out.println("Wrong input. Enter an int. Try Again");
                scanner.nextLine();
            }
        }

        // scanner.close();
        return temp;
    }

    public static Trader getTrader() {
        return new Trader(getStr("Trader Name:"), getStr("Trader Password:"));
    }

    public static Product getProduct(Trader trader) {
        Product temp = new Product((id_generator++).toString(), getStr("Product Name:"), getStr("Product Price:"), getStr("Product Discount:"), getStr("Product Description:"), trader.getName());
        ArrayList<String> category = new ArrayList<String>();
        String str = "new String()";
        int i = 1;
        System.out.println("When finish entering category levels, type -1");
        while(!str.equals("-1"))
        { 
            str = getStr("Category level :" + i++ + " >> ");
            if (str.equals("-1")) break;
            category.add(str);
        }
        
        
        temp.setCategory(category);
        return temp;
    }

    /**
     * Helper function for getting string input easily
     * 
     * @param str Desired text for showing.
     * @return Scanned String
     */
    @SuppressWarnings("resource")
    public static String getStr(String str) {
        System.out.print(str);
        Scanner scanner = new Scanner(System.in);
        String temp = scanner.nextLine();
        // scanner.close();
        return temp;
    }
}
