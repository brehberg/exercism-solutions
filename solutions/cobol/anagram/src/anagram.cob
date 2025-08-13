       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANAGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Subject word to be compared
       01 WS-SUBJECT PIC X(20).
      * Count of candidate words
       01 WS-CANDIDATES-COUNT PIC 9.
      * Table of candidate words
       01 WS-CANDIDATES-TABLE.
           02 WS-CANDIDATES OCCURS 1 TO 20
                            DEPENDING ON WS-CANDIDATES-COUNT.
              05 WS-CANDIDATE PIC X(20).
      * Result list to store matching anagrams
       01 WS-RESULT-LIST PIC X(48).
       01 WS-RESULT-INDEX PIC 99 VALUE 1.
      * Index for iterating through candidates
       01 WS-CANDIDATE-INDEX PIC 99.
      * Temporary storage for sorted and lowercase strings
       01 WS-CANDIDATE-SORTED PIC X(20).
       01 WS-CANDIDATE-LOWERCASE PIC X(20).
       01 WS-SUBJECT-SORTED PIC X(20).
       01 WS-SUBJECT-LOWERCASE PIC X(20).
      * Temporary variables for sorting procedure
       01 WS-SORT-STRING PIC X(20).
       01 WS-SORT-STRING-TEMP-CHAR PIC X.
       01 WS-SORT-STRING-INDEX-A PIC 99.
       01 WS-SORT-STRING-INDEX-B PIC 99.

       PROCEDURE DIVISION.

       FIND-ANAGRAMS.
      *    Initialize result list and index
           MOVE 1 TO WS-RESULT-INDEX
           MOVE SPACES TO WS-RESULT-LIST

      *    Convert subject to lowercase
           MOVE WS-SUBJECT TO WS-SUBJECT-LOWERCASE
           INSPECT WS-SUBJECT-LOWERCASE
            CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            TO "abcdefghijklmnopqrstuvwxyz"

      *    Sort the subject
           MOVE WS-SUBJECT-LOWERCASE TO WS-SORT-STRING
           PERFORM SORT-STRING
           MOVE WS-SORT-STRING TO WS-SUBJECT-SORTED

      *    Loop through the candidates
           PERFORM VARYING WS-CANDIDATE-INDEX
            FROM 1 BY 1
            UNTIL WS-CANDIDATE-INDEX > WS-CANDIDATES-COUNT

      *        Convert candidate to lowercase
               MOVE WS-CANDIDATES(WS-CANDIDATE-INDEX)
                TO WS-CANDIDATE-LOWERCASE
               INSPECT WS-CANDIDATE-LOWERCASE
                CONVERTING "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                TO "abcdefghijklmnopqrstuvwxyz"
      *        Sort the candidate
               MOVE WS-CANDIDATE-LOWERCASE TO WS-SORT-STRING
               PERFORM SORT-STRING
               MOVE WS-SORT-STRING TO WS-CANDIDATE-SORTED

      *        Does the sorted candidate match the sorted subject and
      *        does the lowercase candidate not match the lowercase
      *        subject?
               IF WS-CANDIDATE-SORTED = WS-SUBJECT-SORTED AND
                  WS-CANDIDATE-LOWERCASE NOT = WS-SUBJECT-LOWERCASE
      *            Add a comma if there are allready results
                   IF WS-RESULT-INDEX > 1
                       STRING WS-RESULT-LIST DELIMITED BY SPACE
                              "," DELIMITED BY SIZE
                              INTO WS-RESULT-LIST
                   END-IF
      *            Append the matching canidate to the result list
                   STRING WS-RESULT-LIST DELIMITED BY SPACE
                          WS-CANDIDATES(WS-CANDIDATE-INDEX)
                          DELIMITED BY SPACE
                          INTO WS-RESULT-LIST
      *            Update the number of words in the result
                   ADD 1 TO WS-RESULT-INDEX
               END-IF
           END-PERFORM
           EXIT.

      * A simple buble-sort used to sort a string.
      * Move the string to be sorted into WS-SORT-STRING,
      * Perform a SORT-STRING,
      * Move the sorted string in WS-SORT-STRING into another variable
       SORT-STRING.
           PERFORM VARYING WS-SORT-STRING-INDEX-A FROM 1 BY 1
            UNTIL WS-SORT-STRING-INDEX-A >= 20
      *        Get the next character to compare
               MOVE WS-SORT-STRING-INDEX-A TO WS-SORT-STRING-INDEX-B
               ADD 1 TO WS-SORT-STRING-INDEX-B

               PERFORM VARYING WS-SORT-STRING-INDEX-B
                FROM WS-SORT-STRING-INDEX-B BY 1
                UNTIL WS-SORT-STRING-INDEX-B > 20
      *            Swap characters if they are out of order
                   IF WS-SORT-STRING(WS-SORT-STRING-INDEX-A:1) >
                      WS-SORT-STRING(WS-SORT-STRING-INDEX-B:1)
                       MOVE WS-SORT-STRING(WS-SORT-STRING-INDEX-A:1)
                        TO WS-SORT-STRING-TEMP-CHAR
                       MOVE WS-SORT-STRING(WS-SORT-STRING-INDEX-B:1)
                        TO WS-SORT-STRING(WS-SORT-STRING-INDEX-A:1)
                       MOVE WS-SORT-STRING-TEMP-CHAR
                        TO WS-SORT-STRING(WS-SORT-STRING-INDEX-B:1)
                   END-IF
               END-PERFORM
           END-PERFORM
           EXIT.

       END PROGRAM ANAGRAM.