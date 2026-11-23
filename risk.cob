       identification division.
           program-id. risk.
           author. Christian.
       
       data division.
           working-storage section.
      * Lauf Variable
           01 I pic S99.
      * Anzahl der Spieler
           01 PlayerCount pic 9.

      * Namen als Listen     
           01 Name pic A(10) occurs 6 times.
      * Anzahl der Punkte als Listen    
           01 Punkte pic 99 occurs 6 times.
      * Punkte Änderung     
           01 PunkteDiff pic S99.
      * Ergebnis
           01 Ergebnis pic 99.

       procedure division.
      * Spieler Anzahl erfassen     
           PERFORM UNTIL PlayerCount < 7 AND PlayerCount > 1
               display "Anzahl der Spieler eingeben"
               accept PlayerCount
           END-PERFORM.
      * Namen eingeben
           PERFORM VARYING I from 1 by 1 until I>PlayerCount
               display "Name eingeben"
               accept Name(I)
           END-PERFORM.
      * Punkte erste Runde     
           PERFORM VARYING I from 1 by 1 until I>PlayerCount
               display Name(I) "Punkte eingeben"
               accept Punkte(I)
           END-PERFORM.
      * Beschreibung für Program Ende
           display "Zum Beenden 99 eingeben"    
      * Punkte neu Berechnen     
           PERFORM UNTIL PunkteDiff = 99
               PERFORM VARYING I from 1 by 1 until I>PlayerCount
               display Name(I) "Punkte Differenz eingeben"
               accept PunkteDiff
               IF PunkteDiff=99
               STOP RUN
               END-IF
               add PunkteDiff to Punkte(I)
               divide Punkte(I) by 3 giving Ergebnis
               display "Einheiten:" Ergebnis
               END-PERFORM
           END-PERFORM.

           STOP RUN.
           

     