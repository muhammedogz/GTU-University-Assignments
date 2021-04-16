JC = javac
R = java
SUPER = CSE222_hw02/interface_oguz/
SUB = CSE222_hw02/src_oguz/
DRIVER = Test
JFlags = 


muo:
	$(JC) $(JFlags) $(SUPER)*.java $(SUB)*.java  -d classfiles
#	$(R) $(DRIVER)

javadoc:
	javadoc $(SUPER)*.java $(SUB)*.java -d javadoc

clean:
	rm javadoc/ classfiles/ -r


	