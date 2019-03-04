all: program clean

program:
	alex -g Lexer.x
	happy -gca Parser.y
	ghc --make Esegui.hs -o Esegui
	
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f *.o *.hi

test: testDichiarazioni testFunzioni testCicliECondizioni

testDichiarazioni: clean
	@echo
	@echo test1: Dichiarazioni e assegnamenti corretti
	@echo
	./Esegui < test/test1.txt
	@echo
	@echo test2: Errori di dichiarazione e assegnamento
	@echo
	./Esegui < test/test2.txt
	@echo
	@echo test3: Definizioni di procedure e funzioni corrette
	@echo
	./Esegui < test/test3.txt
	@echo

testFunzioni:
	@echo test4: Errori nella dichiarazione di funzioni/procedure \(corpo non analizzato\)
	@echo
	./Esegui < test/test4.txt
	@echo
	@echo test5: Errori nel corpo delle funzioni/procedure
	@echo
	./Esegui < test/test5.txt
	@echo
	
testCicliECondizioni:	
	@echo test6: Dichiarazione di If e IfElse con varianti di break e continue
	@echo
	./Esegui < test/test6.txt
	@echo
	@echo test7: Errori nella condizione degli If e IfElse
	@echo
	./Esegui < test/test7.txt
	@echo
	@echo test8: Dichiarazione nel corpo degli If e IfElse
	@echo
	./Esegui < test/test8.txt
	@echo
	@echo test9: Dichiarazioni di cicli \(for e while\) con varianti di break e continue
	@echo
	./Esegui < test/test9.txt
	@echo
	@echo test10: Errori nella condizione dei cicli
	@echo
	./Esegui < test/test10.txt
	@echo

