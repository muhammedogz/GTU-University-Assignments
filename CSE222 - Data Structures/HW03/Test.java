import java.util.InputMismatchException;
import java.util.Scanner;

import CSE222_hw03.interface_oguz.Person;
import CSE222_hw03.src_oguz.*;

public class Test {

    private static Company company = new Company(null);
    private static boolean quickControl = false;

    public static void main(String[] args) {

        System.out.println("-------------------- Welcome ---------------------"
                + "\nTHIS IS --> Perfect Company Automation System  <--"
                + "\n------------------- MANAGER.IO -------------------\n");
        while (true) {
            System.out.println(
                    "\n-Menu-\n" + "1-Login with Example Company\n" + "2-Create Your Own Company from Scratch!\n"
                            + "3-Disable login panels (Disables to ask login information each time   when using menu)\n"+"4-Use quick test all functions\n" + "5-Quit\n");
            int choice = getInt("Choice:");
            if (choice == 1)
                company = useExampleCompany();
            else if (choice == 2)
                company = new Company(getStr("Enter your Company Name:"));
            else if (choice == 3) {
                System.out.println("This flag disables login information.");
                System.out.println("With this, Testing functions would be faster");
                System.out.println("It prevents Entering names every login in Menu's");
                quickControl = (quickControl == false) ? true : false;
                System.out.println("Now flag state is: " + quickControl);
            }
            else if (choice == 4) {
                quickCheckAllFunctions();
            }
            else if (choice == 5) {
                System.out.println("Goodbye My Friend <3");
                return;
            } else
                System.out.println("Wrong input");

            if (choice == 1 || choice == 2)
                companyMenu();
        }

    }

    public static void companyMenu() {
        while (true) {
            System.out.println("\n\nCompany Menu\n" + "1-See Company Information\n" + "2-Add an admin\n"
                    + "3-Login as a Admin\n" + "4-Login as a Employee\n" + "5-Login as a Customer\n"
                    + "6-See Product List\n" + "7-Close MANAGER.IO\n");
            int choice = getInt("Choice:");
            switch (choice) {
                case 1:
                    System.out.println(company);
                    break;
                case 2:
                    if (company.addAdmin(new Administrator(company, getStr("Name:"), getStr("Surname:"),
                            getStr("Password:"))))
                        System.out.println("Admin Added Successfully");
                    else
                        System.out.println("Admin adding failed. Same admin already exist");
                    break;
                case 3:
                    adminMenu();
                    break;
                case 4:
                    employeeMenu();
                    break;
                case 5:
                    customerMenu();
                    break;
                case 6:
                    System.out.println(company.allProducts());
                    break;
                case 7:
                    System.out.println("Thanks for Using Our Software <3");
                    return;
                default:
                    System.out.println("Invalid Input");
                    break;
            }

        }
    }

    public static void adminMenu() {
        System.out.println("Admin Login Panel");
        Person admin = null;
        if (quickControl)
            admin = company.getAdmins().get(0);
        else
            admin = company.loginAdmin(
                    new Administrator(company, getStr("Name:"), getStr("Surname:"), getStr("Password:")));

        if (admin == null) {
            System.out.println("Name Surname or Password is invalid. Or There is No Admin in Company");
            return;
        }

        System.out.println("Login Success");
        while (true) {
            System.out.println("\nAdmin Menu\n" + "0-My Profile\n" + "1-Add a Branch\n" + "2-Add an Employee\n"
                    + "3-Remove a Branch\n" + "4-Remove an Employee\n" + "5-See Branch List\n" + "6-See Employee List\n"
                    + "7-Log Out\n");
            int choice = getInt("Choice:");
            try {
                switch (choice) {
                    case 0:
                        System.out.println("Admin of " + company.getCompanyName() + "\n" + admin);
                        break;
                    case 1:
                        ((Administrator) admin).addBranch(new Branch(getInt("Branch Id:")));
                        System.out.println("Branch successfully added.");
                        break;
                    case 2:
                        ((Administrator) admin)
                                .addBranchEmployee(new Employee(company, getStr("Name:"), getStr("Surname:"),
                                        getStr("Password:"), company.getBranches().get(new Branch(getInt("Branch Id:")))));
                        System.out.println("Employee Successfully Added.");
                        break;
                    case 3:
                        System.out.println("Branch List\n" + company.getBranches());
                        ((Administrator) admin).removeBranch(new Branch(getInt("Enter Branch Id:")));
                        System.out.println("Branch, its employees and its products are successfully removed");
                        break;
                    case 4:
                        System.out.println("Employee List\n" + company.getEmployees());
                        ((Administrator) admin).removeBranchEmployee(
                                new Employee(company, getStr("Name:"), getStr("Surname:"), null, null));
                        System.out.println("Remove Employee Successful");
                        break;
                    case 5:
                        System.out.println("Branch List\n" + company.getBranches());
                        break;
                    case 6:
                        System.out.println("Employee List\n" + company.getEmployees());
                        break;
                    case 7:
                        return;
                    default:
                        System.out.println("Wrong Input, Try Again");
                        break;
                }
                
            }catch (NullPointerException e){
                // it is only happens when employee addition
                System.out.println("Employee adding failed");
            } 
            catch (Exception e) {
                System.out.println(e);
            } 
        }

    }

    public static void employeeMenu() {
        System.out.println("Employee Panel");
        Employee employee = null;
        if (quickControl)
            employee = company.getEmployees().get(0);
        else
            employee = company.loginEmployee(
                    new Employee(company, getStr("Name:"), getStr("Surname:"), getStr("Password:"), null));

        if (employee == null) {
            System.out.println("Login failed. Wrong name, surname or password. Or There is no Employee in Company");
            return;
        }
        System.out.println("Login Success");
        while (true) {
        try
        {
            System.out.println("\nEmployee Menu\n" + "0-My Profile\n" + "1-Add Product\n" + "2-Add Customer\n"
                    + "3-Remove Product\n" + "4-Remove Customer\n" + "5-See a Customer's Purchase History\n"
                    + "6-Branch Product List\n" + "7-See All Customers\n" + "8-Log Out\n");

            int choice = getInt("Choice:");
            switch (choice) {
            case 0:
                System.out.println("Employee of " + company.getCompanyName() + "\n" + employee);
                break;
            case 1:
                employee.addProduct(getProduct());
                System.out.println("Product Added Successfully");
                break;
            case 2:
                employee.addCustomer(getCustomer());
                System.out.println("Adding Customer Successful");
                break;
            case 3:
                System.out.println("Employee's Branch Product List\n" + employee.getBranch().getProducts());
                employee.removeProduct(getProduct());
                System.out.println("Removing Product stock Successful");
                break;
            case 4:
                System.out.println("Customers List\n" + company.getCustomers());
                employee.removeCustomer(new Customer(getStr("Name:"), getStr("Surname:"), null, null));
                System.out.println("Remove Customer Successful");
                break;
            case 5:
                System.out.println("Customers List\n" + company.getCustomers());
                int temp = getInt("Customer ID:");
                employee.customerProducts(temp);
                System.out.println(employee.customerProducts(temp));
                break;
            case 6:
                System.out.println("Employee's Branch Product List\n" + employee.getBranch().getProducts());
                break;
            case 7:
                System.out.println("Customers List\n" + company.getCustomers());
                break;
            case 8:
                return;
            default:
                System.out.println("Wrong input");
                break;
            }
            }
            catch (Exception e)
            {
                System.out.println(e.getMessage());
            }
        }

    }

    public static void customerMenu() {
        System.out.println("Customer Panel");
        Customer customer = null;
        String first = getStr("First time customer? (Y),Type Y for Yes. Other types considered No.\n");
        if (first.equals("Y"))
        {
            System.out.println("Enter your Info");
            customer = getCustomer();
            company.getCustomers().add(customer);
        }
        else if (quickControl)
            customer = company.getCustomers().get(0);
        else
        {
            System.out.println("Enter your info for login to System");
            customer = company.loginCustomer(getCustomer());
        }
        

        if (customer == null) {
            System.out.println(
                    "Login Failed. Wrong name, surname, password or mail. Or There is No Customer in Company ");
            return;
        }
        System.out.println("Login Success");
        while (true) {

            System.out
                    .println("\nCustomer Menu\n" + "0-My Profile\n" + "1-Buy Online\n" + "2-Buy Offline (from Branch)\n"
                            + "3-See Purchase History\n" + "4-See All Products\n" + "5-Log Out\n");
            int choice = getInt("Choice:");
            switch (choice) {
                case 0:
                    System.out.println("A loyal customer of " + company.getCompanyName() + "\n" + customer);
                    break;
                case 1:
                    if (customer.getPhone() == null && customer.getAddress() == null) {
                        System.out.println("First Time Online Buyers Should Provide Address and Phone");
                        customer.setAddress(getStr("Address:"));
                        customer.setPhone(getStr("Phone:"));
                    }
                    System.out.println("Enter Desired Product Information");
                    if (customer.buyOnline(company, getProduct()))
                        System.out.println("Purchase Successful");
                    else
                        System.out.println("Purchase Failed. Wrong Product Info");
                    break;
                case 2:
                    if (customer.buyOffline(company, new Branch(getInt("Branch Id:")), getProduct()))
                        System.out.println("Purchase Success");
                    else
                        System.out.println("Purchase Failed");
                    break;
                case 3:
                    System.out.println(customer.getProducts());
                    break;
                case 4:
                    System.out.println(company.allProducts());
                    break;
                case 5:
                    return;
                default:
                    System.out.println("Wrong Input");
                    break;
            }
        }
    }

    public static Product getProduct() {
        return new Product(getStr("Name:"), getInt("Type:"), getInt("Color(0 for colorless):"), getInt("Stock:"));
    }

    public static Customer getCustomer() {
        return new Customer(getStr("Name:"), getStr("Surname:"), getStr("Password:"), getStr("Mail:"));
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

    public static Company useExampleCompany() {
        Company company = new Company("GTU Office Solutions");
        System.out.println("GTU Office Solutions Company CREATED.\n" + "Information about Company\n"
                + "Admin     -> Name: Erdogan Surname:Hoca Password:123\n"
                + "Employee1 -> Name:Burak Surname:Hoca Password:123\n"
                + "Employee2 -> Name:Basak Surname:Hoca Password:123\n"
                + "Employee3 -> Name:Ilhan Surname:Hoca Password:123\n"
                + "Employee4 -> Name:Muhammed Surname:Student Password:123\n"
                + "Customer1 -> Name:Siftah Surname:Para Password:123 Mail:123\n"
                + "Customer2 -> Name:Foo Surname:Bar Password:123 Mail:123\n" + "There is 4 Branches and 20 Products.");
        Administrator admin = new Administrator(company, "Erdogan", "Hoca", "123");
        Branch Branch1 = new Branch(1);
        Branch Branch2 = new Branch(2);
        Branch Branch3 = new Branch(3);
        Branch Branch4 = new Branch(4);
        Employee employee1 = new Employee(company, "Burak", "Hoca", "123", Branch1);
        Employee employee2 = new Employee(company, "Basak", "Hoca", "123", Branch2);
        Employee employee3 = new Employee(company, "Ilhan", "Hoca", "123", Branch3);
        Employee employee4 = new Employee(company, "Muhammed", "Student", "123", Branch4);
        Customer customer1 = new Customer("Siftah", "Para", "123", "123");
        Customer customer2 = new Customer("Foo", "Bar", "123", "123");
        company.addAdmin(admin);
        try {
            admin.addBranch(Branch1); admin.addBranch(Branch2);     
            admin.addBranch(Branch3); admin.addBranch(Branch4);
            admin.addBranchEmployee(employee1); admin.addBranchEmployee(employee2);
            admin.addBranchEmployee(employee3); admin.addBranchEmployee(employee4);

            /* --------------------------------------------- */
        employee1.addProduct(new Product("Chair", 1, 1, 5));
        employee1.addProduct(new Product("Desk", 3, 4, 5));
        employee1.addProduct(new Product("Table", 2, 5, 5));
        employee1.addProduct(new Product("Bookcase", 1, 4, 5));
        employee1.addProduct(new Product("Cabinet", 4, 0, 5)); 
        /* --------------------------------------------- */
        employee2.addProduct(new Product("Chair", 2, 1, 5));
        employee2.addProduct(new Product("Desk", 3, 2, 5));
        employee2.addProduct(new Product("Table", 4, 3, 5));
        employee2.addProduct(new Product("Bookcase", 5, 4, 5));
        employee2.addProduct(new Product("Cabinet", 2, 0, 5));
        /* --------------------------------------------- */
        employee3.addProduct(new Product("Chair", 7, 1, 5));
        employee3.addProduct(new Product("Desk", 1, 1, 5));
        employee3.addProduct(new Product("Table", 4, 1, 5));
        employee3.addProduct(new Product("Bookcase", 2, 1, 5));
        employee3.addProduct(new Product("Cabinet", 2, 0, 5));
        /* --------------------------------------------- */
        employee4.addProduct(new Product("Chair", 6, 1, 5));
        employee4.addProduct(new Product("Desk", 5, 1, 5));
        employee4.addProduct(new Product("Table", 4, 1, 5));
        employee4.addProduct(new Product("Bookcase", 3, 1, 5));
        employee4.addProduct(new Product("Cabinet", 2, 0, 5));
        /* --------------------------------------------- */
        employee1.addCustomer(customer1); employee1.addCustomer(customer2);
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        //System.out.println("All Product List\n" + company.allProducts());

        
        return company;
    }

    public static void quickCheckAllFunctions(){
        try
        {
        System.out.println("Load Example Company");
        company = useExampleCompany();
        System.out.println("See company Info before test functions");
        System.out.println(company);
        System.out.println("Add a new admin to company.");
        System.out.println("Admin: Admin 123");
        Administrator admin = new Administrator(company, "Admin", "123", "123");
        company.addAdmin(admin);
        System.out.println("Add new Branch to company with id 10");
        Branch Branch = new Branch(10);
        admin.addBranch(Branch);
        System.out.println("Add new employee");
        System.out.println("Employee: Employee 123");
        Employee employee = new Employee(company, "Employee", "123", "123", Branch);
        admin.addBranchEmployee(employee);
        System.out.println("Add new customer and product");
        System.out.println("Customer: Customer 123");
        Customer customer = new Customer("Customer", "123", "123", "123");
        employee.addCustomer(customer);
        System.out.println("Product: Test 1 1 500");
        Product product = new Product("Test", 1, 1, 500);
        employee.addProduct(product);
        System.out.println("See all products in this company as a customer");
        System.out.println(customer.allProducts(company));
        System.out.println("Buy Test product with stock 100 with Customer");
        customer.buyOnline(company, new Product("Test", 1, 1, 100));
        System.out.println("See customer purchase history");
        System.out.println(employee.customerProducts(customer.getId()));
        System.out.println("See all info about company for changes");
        System.out.println(company);

        System.out.println("End of the test :)\nIf you want to test other functions as well, you can use menu\n Have a good day hocam\n" );
  
        }
        catch (Exception e)
        {
            System.out.println(e.getMessage());
        }
    }

}
