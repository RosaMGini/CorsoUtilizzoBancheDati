Questo folder contiene in i_input una piccola istanza di dati sintetici che simulano una sorgente di dati amministrativi italiani, ristretta a una popolazione di utilizzatori di due farmaci anticoagulanti, il rivaroxaban e l'apixaban. 

Inoltre, nel folder si trova un programma in R che esegue un semplice protocollo. Il protocollo è la parte introduttiva di uno studio pre-post volto a comprendere l'impatto su alcuni outcome (a) dell'introduzione di un antidoto all'azione di due farmaci anticoagulanti, rivaroxaban e apixaban, e (b) dell'introduzione di linee guida cliniche per la somministrazione di tale antidoto. Lo studio parte nel 2018 e il periodo di studio è suddiviso in 1: prima dell'introduzione dell'antidoto (fino al 31/8/2021), 2: dopo l'introduzione dell'antidoto (1/9/2021-31/7/2023), 3: dopo l'introduzione delle linee guida (1/8/2023 - 31/12/2023). Nel primo periodo si distinguono separatamente i periodi prima, durante e dopo le restrizioni dovute alla pandemia da Covid 19 (1a, 1b, 1c). Il protcollo completo è più complesso ed è implementato in [questo repository](https://github.com/ARS-toscana/emorragie_gravi/tree/main).

Nel programma in questo folder ci si limita alla costruzione della popolazione di studio. Ad alto livello, il programma esegue queste due azioni:

	1) individua nella popolazione dell'istanza la coorte degli utilizzatori di rivaroxaban e apixaban, che può includere più volte la stessa persona; questa coorte viene definita *popolazione sorgente*
	2) individua all'interno della popolazione sorgente i casi di sanguinamento grave. Questa è definita *popolazione in studio*, e ogni persona della popolazione sorgente può contribuire a più di una unità di questa popolazione. Lo script caratterizza la popolazione di studio con le seguenti variabili
	    -) periodo di studio in cui avviene il sanguinamento
	    -) genere
	    -) classe di età al momento del sanguinamento
	    -) numero di precedenti sanguinamenti
	    -) giorni dal più recente sanguinamento (se ce n'è più d'uno)
	   
Il programma è strutturato secondo la pipeline sviluppata nello studio IMI-ConcePTION e riassunta in [questo deliverable](https://zenodo.org/records/5829464#.YfraFurMK3A) e [in questo wiki](https://github.com/IMI-ConcePTION/ConceptionTools/wiki). A differenza di quanto descritto in queste risorse, però, l'istanza di dati non è strutturata secondo il Common Data Model di ConcePTION, ma con un adattamento del [Common Data Model di TheShinISS](https://www.epicentro.iss.it/ben/2020/4/theshiniss).

Seguendo le raccomandazioni della pipeline, il programma è suddiviso in step, e il programma viene eseguito lanciando il file to_run.R, che invoca dei file di parametri e poi ciascuno step. Accanto ai commenti al codice, il programma è documentato nel subfolder i_codebook, in cui il [file indice](https://github.com/RosaMGini/CorsoUtilizzoBancheDati/blob/main/esercitazione_02/i_codebooks/00_index_esercitazione_02.xlsx) contiene una riga per ogni dataset intermedio generato, e ciascuno di questi ultimi è rappresentato dal proprio codebook, in un file con il suo nome, nel folder stesso.
