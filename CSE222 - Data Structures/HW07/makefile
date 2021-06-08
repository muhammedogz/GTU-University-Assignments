# Compiler
JC = javac
# Run
R = java
# Book implementations folder
BOOK = CSE222_hw07/book_implementation/
# My implementations source folder
SUPER = CSE222_hw07/interface_oguz/
# My implementaions interface folder
SUB = CSE222_hw07/src_oguz/
# Driver class name
DRIVER = Test
# Flags
JFlags =  


muo:
	$(JC) $(JFlags) $(SUPER)*.java $(SUB)*.java $(BOOK)*.java $(DRIVER).java  -d classfiles
	$(R) -cp classfiles $(DRIVER)

# create javadoc files
javadoc:
	javadoc $(SUPER)*.java $(SUB)*.java -d javadoc

# clean javadoc and classfiles folder
clean:
	rm classfiles/ javadoc/ -r 
	

