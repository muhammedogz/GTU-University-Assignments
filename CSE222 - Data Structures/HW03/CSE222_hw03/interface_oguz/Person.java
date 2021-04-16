/**
 * This class is upper class but not an interface. Still it is in interface folder for code readability purpose
 */

package CSE222_hw03.interface_oguz;


public class Person {
    private String name;
    private String surname;
    private String password;

    /**
     * Default Constructor
     * @param name give name of the person
     * @param surname give surname of the person
     * @param password give password of the person
     */
    public Person(String name, String surname, String password)
    {
        this.name = name;
        this.surname = surname;
        this.password = password;
    }

    public void setName(String name) {
        this.name = name;
    }
    public void setSurname(String surname) {
        this.surname = surname;
    }
    public void setPassword(String password) {
        this.password = password;
    }

    public String getName() {
        return name;
    }
    public String getPassword() {
        return password;
    }
    public String getSurname() {
        return surname;
    }

    @Override
    public boolean equals(Object obj) {
        if (this.name.equals(((Person) obj).getName()) && this.surname.equals(((Person) obj).getSurname()))
            return true;
        return false;
    }
    
}
