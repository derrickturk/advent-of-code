: >= 2DUP = -ROT > OR ; \ (x y -- x >= y)
: <= 2DUP = -ROT < OR ; \ (x y -- x <= y)
: 0= 0 = ; \ (x -- x = 0)
: 0<> 0 <> ; \ (x -- x <> 0)
: 0< 0 < ; \ (x -- x < 0)
: 0> 0 > ; \ (x -- x > 0)
: 0<= 0 <= ; \ (x -- x <= 0)
: 0>= 0 >= ; \ (x -- x >= 0)

: /MOD 2DUP MOD -ROT / ; \ (x y -- rem quo)

: NEGATE 0 SWAP - ; \ (x -- -x)
: TRUE 1 ;
: FALSE 0 ;

: LITERAL IMMEDIATE ' LIT , , ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: '\n' 10 ;
: BL 32 ;

: CR '\n' EMIT ;

: IF IMMEDIATE ' 0BRANCH , HERE @ 0 , ;
: THEN IMMEDIATE DUP HERE @ SWAP - SWAP ! ;
: ELSE IMMEDIATE ' BRANCH , HERE @ 0 , SWAP DUP HERE @ SWAP - SWAP ! ;

: [COMPILE] IMMEDIATE WORD FIND >CFA , ;
: RECURSE IMMEDIATE LATEST @ >CFA , ;

: BEGIN IMMEDIATE HERE @ ;
: UNTIL IMMEDIATE ' 0BRANCH , HERE @ - , ;
: AGAIN IMMEDIATE ' BRANCH , HERE @ - , ;
: WHILE IMMEDIATE ' 0BRANCH , HERE @ 0 , ;
: REPEAT IMMEDIATE
         ' BRANCH ,
         SWAP
         HERE @ - ,
         DUP
         HERE @ SWAP -
         SWAP !
;

: UNLESS IMMEDIATE ' NOT , [COMPILE] IF ;

: ( IMMEDIATE
    1
    BEGIN
      KEY
      DUP '(' = IF DROP 1+
                ELSE ')' = IF 1- THEN
                THEN
    DUP 0= UNTIL
    DROP
;

( now we have comments! )

: SPACE ( -- ) BL EMIT ;
: SPACES ( n -- ) BEGIN DUP 0> WHILE SPACE 1- REPEAT DROP ;

: U. ( u -- )
  BASE @ /MOD
  ?DUP IF RECURSE THEN
  DUP 10 < IF '0'
           ELSE 10 -
                'A'
           THEN
  +
  EMIT
;

: UWIDTH ( u -- width )
  BASE @ /
  ?DUP IF RECURSE 1+ ELSE 1 THEN
;

: U.R ( u width -- )
  SWAP
  DUP
  UWIDTH
  ROT
  SWAP -
  SPACES
  U.
;

: .R ( n width -- )
  SWAP
  DUP 0< IF NEGATE
            1
            SWAP
            ROT
            1-
         ELSE 0
              SWAP
              ROT
         THEN
  SWAP
  DUP
  UWIDTH
  ROT
  SWAP -
  SPACES
  SWAP
  IF 45 EMIT THEN
  U.
;

: . 0 .R SPACE ;
: U. U. SPACE ;
: ? ( addr -- ) @ . ;

: DECIMAL 10 BASE ! ; ( -- )
: HEX 16 BASE ! ; ( -- )

( strings? )

: C, ( "bytes" are 64-bit )
  HERE @ !
  1 HERE +!
;

: S" IMMEDIATE
     STATE @ IF ' LITSTRING ,
                HERE @
                0 ,
                BEGIN
                  KEY
                  DUP '"' <>
                WHILE
                  C,
                REPEAT
                DROP
                DUP
                HERE @ SWAP -
                1-
                SWAP !
             ELSE
                HERE @
                BEGIN
                  KEY
                  DUP '"' <>
                WHILE
                  OVER !
                  1+
                REPEAT
                DROP
                HERE @ -
                HERE @
                SWAP
             THEN
;

: ." IMMEDIATE
     STATE @ IF [COMPILE] S" ' TELL ,
             ELSE BEGIN KEY DUP '"' = IF DROP EXIT THEN EMIT AGAIN
             THEN
;

: CONSTANT WORD CREATE ( const -- )
           DOCOL ,
           ' LIT ,
           ,
           ' EXIT ,
;

( a crappy malloc )
: ALLOT ( words -- ptr )
    HERE @ SWAP
    HERE +!
;

: VARIABLE ( -- )
    1 ALLOT
    WORD CREATE
    DOCOL ,
    ' LIT ,
    ,
    ' EXIT ,
;

( reflection and such )

: ID. ( link-ptr -- )
    3 + ( &length )
    DUP ( &length &length )
    @ ( &length length )
    SWAP 1+ SWAP ( ptr length )
    TELL
;

: ?HIDDEN ( link-ptr -- bool ) 1+ @ ;
: ?IMMEDIATE ( link-ptr -- bool ) 2 + @ ;

: WORDS ( -- )
    LATEST @
    BEGIN ?DUP
    WHILE
      DUP ?HIDDEN UNLESS DUP ID. CR THEN @
    REPEAT
;

( case statements )

: CASE IMMEDIATE 0 ;

: OF IMMEDIATE
     ' OVER ,
     ' = ,
     [COMPILE] IF
     ' DROP ,
;

: ENDOF IMMEDIATE
    [COMPILE] ELSE
;

: ENDCASE IMMEDIATE
    ' DROP ,
    BEGIN ?DUP
    WHILE
      [COMPILE] THEN
    REPEAT
;

: GREET
."   ____  ___   _ _____ ___ ____ ______ _____ __  __" CR
."  /_ _/ / _ \ ///_  _// _//__ // /_/ //_  _// /_/ /" CR
."  _//_ / / \ v/  / / / _///_/// __  /  / / / __  / " CR
." /___//_/   \/  /_/ /_/ /___//_/  \_\ /_/ /_/ /_/  " CR
CR
;

GREET
