/**
 * @author Muhammed Oğuz
 * 
 * This interface represents of E-Commerce Company.
 * Contains company methods 
 */


package CSE222_hw06.interface_oguz;

import CSE222_hw06.src_oguz.Customer;
import CSE222_hw06.src_oguz.Trader;

public interface ICompany {
 
    /**
     * Read given file and create new sorted one
     * @param filename The filename going to read
     */
    void readFile(String filename);

    boolean signUpTrader(Trader trader);

    boolean loginTrader(Trader trader);

    boolean signUpCustomer(Customer customer);

    boolean loginCustomer(Customer customer);

}
