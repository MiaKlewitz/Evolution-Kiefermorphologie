# Evolution-Kiefermorphologie
Der Code zu meiner Bachelorarbeit "Evolution der Unterkiefermorphologie bei Säugetieren im frühen Känozoikum" (2024)

Der gesamte Code besteht aus mehreren Dateien, die jeweils ihre eigene Funktion haben (bsp. Datensätze zusammenfügen oder die PCA durchführen).

Folgede Dateien gibt es:
1. readland.gem.tps function
    
Das ist die benötigte TPS Funktion für den nächsten Code.
   
2. Measure Landmalrs EG for Mia
   
Hier werde die Eigenschaften der Kiefer aus den Orientierungspukten (gesetzt mit TPSdig) gemessen.

3. Combining multilpe data
   
Hier werden die Datensätze von Dr. Gemma Benevento, Wilman et al. (2014) und mir auf die nötigen Informationen reduziert und zusammengefügt.

4. Feeding Ecology Code
   
Hier werden die Ernährungseinteilungen berechnet und zugeteilt.

5. Reduce Fossil Data
   
Hier wird der Datensatz von Dr. Gemma Benevento von den ausgestorbenen Säugetieren auf die nötigen Informationen reduziert.
  
6. Combining Fossil and Modern Data
   
Hier werden die ausgestorbenen Säugetiere zum Datensatz der modernen Säugetiere hinzugefügt.

7. Multiple plots
    
Hier wird die PCA für Multituberculata, Primaten und Nagetiere für PC1-2 durchgeführt und nebeneinander geplottet. Der Code beinhaltet zwei Analysen, einmal mit und einmal ohne die Primaten.

8. Primates TIME
    
Hier wird die PCA mit den Primaten für PC1-4 durchgeführt.

9. Nagetiere TIME
    
Hier wird die PCA mit ohne Primaten für PC1-4 durchgeführt.

10. Multiple plots PC5-6
    
Hier wird die PCA mit und ohne Primaten (zwei Analysen) für PC5-6 durchgeführt.
