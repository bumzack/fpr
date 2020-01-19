# Todo

## wäre gut
- [ ] Bug: am Spielfeld rand wird ein Schiff nicht gesetzt?\
    wenn ein invalides schiff gesetzt wird, dann wird der Game Status auf den nächsten Index gesetzt, dass sollte nicht
    sein. siehe screenshot: bei Eingabe ```Set A1 2 W``` sollte eine Fehlermeldung erscheinen und der GameStatus nicht upgedated werden, da das schiff nicht auf das Board passt.
- [ ] Ausgabe, wo hat der Computer probiert: das eigene Brett anpassen
- [ ] der Code in ```setShip``` macht nicht das was er soll ```match shipOnBoard (s, game.Size) with``` und man kann große Schiffe setzen

## wäre nett
- [ ] doppelte eingaben abfange
- [ ] Schiffe auf benachbarten Feldern verhindern

## erledigt
- [x] remove SetNew and rename to Set
- [x] Spiel beenden
- [x] Schiffformen  Computer Random generiert anpassen (2er Schiffe, 3 Stk)
- [x] Schiffformen (2er Schiffe, 3 Stk)

## Bug:

![alt text](bug.png)
