# Domainmodell

man braucht 2 boards? jeder spieler: wo sind meine schiffe, wo habe ich beim gegner probiert bzw getroffen oder wasser

## variante 1
- Board mit 5x5(?) Feldern
    - liste mit 25 feldern
    - width
    - height
    - oder nur size
    - cnt_ships   Schiffe pro Spieler

- Game:
    - P1Ships:   board
    - P1Attempts: board
    - P2Ships:    board
    - P2Attempts:   board
    - status:  SPielStatus


- Feld: kann 2 Zustände haben:
    - wurde noch nicht vom Gegner  ausgewählt
        - schiff of int   (zb 2 schiffe, int .. Index des schiffs)
        - wasser
    - vom gegner ausgewählt (gleicher subtyp w.o.)
        - wasser
        - schiff of int    (schiff index)

- 2 boards: für jeden Spieler eines

- SpielStatus:
    - new (?!)
    - finished  ?!
        - player 1 won/lost
        - player 2 won/lost
    - turn:
        - player 1
        - player 2

- Type: coord - check auf valide inputs (zb 5x5 feld -> e5 ist maximum )



## variante 2  - leichter festzustellen ob und wieviele schiffe versenkt, aber redundante daten, schiweriger zu "zeichnen"
Ein Board besteht aus einer Größe und einer Liste von Schiffen
- Board:
    - list ship (länge 2)
    - list field

    - size: 5    (5x5)
    - ships_destroyed   int
    - ships_total   int   (länge der *list ship*)

 - point_status
    - not_attempted
    - attempted
        - water     (redundant mit Liste der Ships)
        - hit

 - ship
    - list points
    - status
        - alive
        - destroyed
 - point
    - coord: coord   struct (x, y)
    - status
        - not hit
        - hit



## offene Fragen
- wie Schiff(e) auf Board platzieren? so, dass felder zusammenhängen
- wer spielt gg wen?
    -   2 menschliche Gegner od.
    - Mensch gg. maschine (nur Random Züge, keine sophisticated Züge, wenn es einen treffer gab)

## Regeln
-  jeder spieler 2 Schiffe, 2 Einheiten lang
- abwechselnd versucht Schiffe zu treffen
- bei Treffer darf der Spieler ein weiteres Mal probieren

- ein startpunkt eingeben und dann dfragt mich das programm welche der max 4. Punkt ich als 2. punkt verwenden will

## Methoden (wie mit immutable daten strukturen?)
- game_finished() -> boolean: alle schiffe versenkt?
- ship_destroyed(old_board, new_board) -> boolean    : mit letzten zug ein schiff versenkt?

- print_board(board):  auf console anzeigen

- new_hit(coord) -> Game

- add_ship(board:, list of  coords)




# Befehle
- "new":   neues spiel
- zB "a3"     buchstabe + Ziffer -> Koordinaten für "Bombe"\
wird in Typ koordinate konvertiert ()
- "quit"
- "help"
- print


