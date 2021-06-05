JC = javac
R = java
SUPER = CSE222_hw07/interface_oguz/
SUB = CSE222_hw07/src_oguz/
DRIVER = Test
JFlags = 


muo:
	$(JC) $(JFlags) $(SUPER)*.java $(SUB)*.java $(DRIVER).java  -d classfiles
	$(R) -cp classfiles $(DRIVER)
#	make clean

javadoc:
	javadoc $(SUPER)*.java $(SUB)*.java -d javadoc
clean:
	rm classfiles/ Temp/ -r javadoc/ 
	

