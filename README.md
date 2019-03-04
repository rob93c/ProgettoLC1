# Progetto di Linguaggi e Compilatori 1

Si consideri un linguaggio costruito a partire dalla stessa sintassi concreta di Pascal.

In tale linguaggio si hanno procedure e funzioni. Si hanno le operazioni aritmetiche, booleane e
relazionali standard ed inoltre 8 procedure predefinite (`write | read`) (`Int | Real | Char | String`).

Le `read...` hanno argomento per riferimento.

Non è necessario avere tipi di dato definiti dall’utente.

I tipi ammessi siano interi, booleani, float, caratteri, stringhe, array e puntatori (con i costruttori
e/o le operazioni di selezione relativi).

Il linguaggio deve implementare—con la sintassi concreta di Pascal qualora sia prevista—le modalità
di passaggio dei parametri value e reference (le altre a piacere), con i relativi vincoli di semantica
statica.

Il linguaggio deve ammettere le istruzioni break e continue (dentro il corpo dei cicli), con la sintassi
concreta di Pascal, qualora sia prevista.

Per detto linguaggio (non necessariamente in ordine sequenziale):

- Si progetti una opportuna sintassi astratta.
- Si progetti un type-system (definendo le regole di tipo rispetto alla sintassi astratta generata dal
parser, eventualmente semplificandone la rappresentazione senza però snaturarne il contenuto
semantico).
- Si implementi un lexer con Alex, un parser con Happy ed il corrispondente pretty printer
(si suggerisce di utilizzare BNFC per costruire un primo prototipo iniziale da raffinare poi
manualmente, ma non è obbligatorio).
- Si implementi il type-checker e tutti gli altri opportuni controlli di semantica statica (ad esempio
il rilevamento di utilizzo di r-expr illegali nel passaggio dei parametri). Si cerchi di fornire
messaggi di errore che aiutino il più possibile a capire in cosa consiste l’errore, quali entità
coinvolge e dove queste si collochino nel sorgente.

Si predispongano dei test case significativi per una funzione che, dato un nome di file, esegua il
parsing del contenuto, l’analisi di semantica statica e il pretty-print.

Si tenga presente che del linguaggio Pascal, da cui si trae ispirazione per la sintassi concreta, non
si richiede di capire la semantica di funzionamento (irrilevante ai fini della soluzione) ma solo di
utilizzare le stesse scelte di sintassi concreta relativamente ai costrutti (canonici) previsti dal testo.

# Avviare il programma

Usare il comando `make` per eseguire il programma e `make test` per eseguire alcuni test prestabiliti.

# License

See the [LICENSE](https://github.com/rob93c/ProgettoLC1/blob/master/LICENSE.md) file for license rights and limitations (MIT).
