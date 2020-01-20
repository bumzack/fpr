# Todo

## wäre gut
- [ ] Ausgabe, wo hat der Computer probiert: das eigene Brett anpassen
- [ ] es gibt kein "Schiff 1 versenkt" - sondern nur, wenn alle FElder mit Schiffen getroffen -> Spiel aus

## wäre nett
- [ ] doppelte eingaben abfange
- [ ] Schiffe auf benachbarten Feldern verhindern

## erledigt
- [x] Bug: am Spielfeld rand wird ein Schiff nicht gesetzt?\
    wenn ein invalides schiff gesetzt wird, dann wird der Game Status auf den nächsten Index gesetzt, dass sollte nicht
    sein. siehe screenshot: bei Eingabe ```Set A1 2 W``` sollte eine Fehlermeldung erscheinen und der GameStatus nicht upgedated werden, da das schiff nicht auf das Board passt.

- [x] remove SetNew and rename to Set
- [x] Spiel beenden
- [x] Schiffformen  Computer Random generiert anpassen (2er Schiffe, 3 Stk)
- [x] Schiffformen (2er Schiffe, 3 Stk)
- [x] der Code in ```setShip``` macht nicht das was er soll ```match shipOnBoard (s, game.Size) with``` und man kann große Schiffe setzen

## Bug:

![alt text](bug.png)
