package CSE222_hw06.src_oguz;

import java.util.ArrayList;


public class Product {
    
    private String id;
    private String name;
    private ArrayList<String> category;
    private String price;
    private String discount;
    private String description;
    private String trader;

    public Product() {
        this.id = null;
        this.name = null;
        this.category = null;
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

    @Override
    public String toString() {
        return name + "\t" + price + "\t" + trader;
    }
}
