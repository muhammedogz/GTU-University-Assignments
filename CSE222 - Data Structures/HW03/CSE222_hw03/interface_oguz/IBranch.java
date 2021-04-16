/**
 * @author Muhammed Oguz
 * @version 1.0
 */
package CSE222_hw03.interface_oguz;

import CSE222_hw03.src_oguz.HybridList;
import CSE222_hw03.src_oguz.KWArrayList;

public interface IBranch {

    KWArrayList<IEmployee> getEmployees();

    HybridList<IProduct> getProducts();
    
    
}
