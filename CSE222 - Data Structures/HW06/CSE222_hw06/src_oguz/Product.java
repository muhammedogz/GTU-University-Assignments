package CSE222_hw06.src_oguz;


import java.util.ArrayList;
import java.util.Iterator;



public class Product {
    
    static Integer id_generate = 10000001;
    private String id, name, price, discount, description, trader;
    private ArrayList<String> category =  new ArrayList<>();
    boolean showDescription = false, showCategory = false;


    public Product() {
        this.id = null;
        this.name = null;
        this.price = null;
        this.discount = null;
        this.description = null;
        this.trader = null;
    }

    public Product(String id, String name, String price, String discount, String description, String trader){
        this.id = id;
        this.name = name;
        this.price = price;
        this.discount = discount;
        this.description = description;
        this.trader = trader;
        this.category = null;
    }
    
    public void setCategory(ArrayList<String> category) {
        this.category = category;
    }
    public void setDescription(String description) {
        this.description = description;
    }
    public void setDiscount(String discount) {
        this.discount = discount;
    }
    public void setId(String id) {
        this.id = id;
    }
    public void setName(String name) {
        this.name = name;
    }
    public void setPrice(String price) {
        this.price = price;
    }
    public void setTrader(String trader) {
        this.trader = trader;
    }
    public void setShowDescription(boolean showDescription) {
        this.showDescription = showDescription;
    }
    public void setShowCategory(boolean showCategory) {
        this.showCategory = showCategory;
    }
    public ArrayList<String> getCategory() {
        return category;
    }
    public String getDescription() {
        return description;
    }
    public String getDiscount() {
        return discount;
    }
    public String getId() {
        return id;
    }
    public String getName() {
        return name;
    }
    public String getPrice() {
        return price;
    }
    public String getTrader() {
        return trader;
    }

    /**
     * Return string whole product
     * @return
     */
    public String getStringFormat() {
        StringBuilder temp = new StringBuilder();

        temp.append(getTrader()+";"+getName()+";"+getId()+";");
        Iterator<String> it = getCategory().iterator();
        while (it.hasNext())
        {
            temp.append(it.next()+" >> ");
        }
        temp.deleteCharAt(temp.length() - 1);
        temp.deleteCharAt(temp.length() - 1);
        temp.deleteCharAt(temp.length() - 1);
        temp.deleteCharAt(temp.length() - 1);
        temp.append(";"+getPrice()+";"+getDiscount()+";"+getDescription());

        return temp.toString();
    }

    @Override
    public String toString() {
        StringBuilder str = new StringBuilder();
        str.append(id + "\t" + name + "\t" + price + "\t" + discount + "\t");
        if (showDescription)
            str.append("\n" + description + "\n");

        if (showCategory && category != null)
        {
            for (String ct : category)
            {
                str.append(ct + " >> ");
            }
            // delete last three char after inserting those >>
            str.deleteCharAt(str.length() - 1);
            str.deleteCharAt(str.length() - 1);
            str.deleteCharAt(str.length() - 1);
            
        }
        
        return str.toString();
    }
}
