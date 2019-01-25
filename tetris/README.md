# Project Tetris

Tetris is een eenvoudige 2-dimensionale puzzel. Bovenaan een rooster
verschijnen willekeurige tetromino's, die in functie van de tijd naar beneden
vallen. Door de tetromino's te draaien en naar links/rechts te duwen probeert
de speler onderaan het rooster rijen volledig te vullen, waarop deze
verdwijnen. De speler probeert de opstapeling van blokjes onderaan zo lang
mogelijk onder een zekere hoogte te houden.


## Praktisch

Vul de [gegeven skeletcode] aan, eventueel met extra bestanden, tot je code aan
onderstaande vereisten voldoet. Test je code zorgvuldig. Dien in door je
project gezipt op te laden naar [indianio]. Wegens gebrek aan ondersteuning
voor Haskell, zal Indianio **niet** controleren of jouw code compileert, dit
kan je zelf doen door met een stack project te werken (dien je aangepaste
metafiles mee in!).

Om ons werk te sparen tijdens het verbeteren, zorgen jullie er zelf voor dat
jullie projecten compileren. **Projecten die niet compileren krijgen
automatisch een nul.** Om een "maar op mijn machine werkte het wel..." te
vermijden, kan je altijd je project eens compileren in de standaard Haskell
docker. Loop onderstaand commando met je project als huidige map.

    docker run -it -v $(pwd):/mnt --workdir=/mnt --rm haskell:8 bash -c 'apt-get update; yes | apt-get install freeglut3-dev; stack build --allow-different-user'

Problemen kan je altijd (voor de deadline) melden.


## Functionele vereisten

* Blokjes moeten automatisch naar beneden vallen.
* Het volgende blokje moeten (semi-) random gekozen worden.
* De linker- en rechterpijltoetsen moeten het blokje respectievelijk naar links
  en rechts bewegen.
* De "r"-toets moet het blokje roteren.
* Volledige lijnen moeten opgekuist worden. 
* Als de gebruiker een blokje plaats boven een bepaalde hoogte moet het spel
  stoppen. 


## Stilistische vereisten

- Je grafische interface moet geimplementeerd worden met gloss.
- Geen hardgecodeerde constanten in je code.
- Vermijd het gebruik van if-testen, maak gebruik van pattern matching en
  guards. 
- Maak gebruik van where clauses voor locale functies.
- Probeer gebruik te maken van de reeds bestaande functies.
- Pas hlint toe op je code.

Voldoet je code niet aan de verwachte code stijl zal je hiervoor punten
verliezen.


## Dependencies

In dit project maken we gebruik opnieuw gebruik van [gloss], en deze werd al
toegevoegd aan het gegeven cabal bestand. Gebruik je andere bibliotheken, dan
voeg je die ook daar toe (en je dient deze mee in). Vermijd het gebruik van
ongeziene bibliotheken, bij twijfel vraag je even of je jouw favoriete
bibliotheek mag gebruiken.


[gegeven skeletcode]: src/Main.hs
[indianio]: https://indianio.ugent.be/?action=handinform&id=11751
[gloss]: http://hackage.haskell.org/package/gloss

