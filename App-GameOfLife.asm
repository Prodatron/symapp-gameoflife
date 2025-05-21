;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                 C o n w a y ' s   G a m e   o f   L i f e                  @
;@                                                                            @
;@             (c) 2012-2025 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

relocate_start

;Bugs
;- still crashes sometime, e.g. with special rules sometimes

;Todo
;- use system libraries
;- 16col optimization


;### PRGPRZ -> Programm-Prozess
;### PRGEND -> End Program
;### PRGINF -> open info window
;### PRGCFG -> Generates datafile path

;--- CONTROL-ROUTINES ---------------------------------------------------------
;### CTLSHW -> Shows field and counters
;### CTLRUN -> Runs/stops cell generation
;### CTLEDT -> Edits cell field
;### CTLFIG -> Adds figure to cell field
;### CTLFGC -> Figure has been choosed
;### CTLRND -> Generates random cell field
;### CTLRNV -> Sets random value
;### CTLCLR -> Clears cellfield and resets generation counter

;--- FILE-ROUTINES ------------------------------------------------------------
;### FILFIG -> Gets figure (from disc or buffer)
;### FILLOD -> Loads cell field
;### FILSAV -> Saves cell field
;### FILPRE -> Loads preset

;--- SUB-ROUTINES -------------------------------------------------------------
;### MSGGET -> Receives Message
;### MSGDSK -> Receives Message from Desktop-Process
;### MSGSND -> Sends Message to Desktop-Process
;### CLCDEZ -> Converts Byte into two Decimaldigits
;### CLCNUM -> Converts 16bit Number into sting (0-terminated)
;### CLCM16 -> Multipliziert zwei Werte (16bit)
;### CLCMUL -> Multipliziert zwei Werte (24bit)
;### CLCDIV -> Dividiert zwei Werte (24bit)
;### FLO_RANDOMIZE0 -> RND seek to 0
;### FLO_RANDOMIZE -> RND seek to (HL)
;### FLO_RND -> Gets next RND value
;### FLO_LAST_RND -> Gets current RND value
;### FLO_SGN -> Tests the sign of (HL)
;### FLO_VZW -> Changes the sign of (HL)

;--- CELL-ROUTINES ------------------------------------------------------------
;### CELPRZ -> Cell calculation process
;### CELMIR -> Mirrors cell-field borders for torus-mode
;### CELCNT -> Checks cell situation
;### CELCLC -> Calculates new cell field
;### CELSHW -> Builds cell field

;### CELGEN -> calculates next cell generation
;### CELPLT -> plots cell field
;### CELTRS -> mirrows cells at the border, if "torus" is activated
;### CELTRC -> clears cells at the border, if "torus" is activated
;### CELREC -> recalculate neighbours and alive count (torus ignored)

;---

macro cell_dead
        dec (ix-65)     ;6
        dec (ix-64)     ;6
        dec (ix-63)     ;6
        dec (ix-1)      ;6
        dec (ix+1)      ;6
        dec (ix+63)     ;6
        dec (ix+64)     ;6
        dec (ix+65)     ;6
        res 4,(ix+0)    ;7
        dec bc          ;2
mend

macro cell_alive
        inc (ix-65)
        inc (ix-64)
        inc (ix-63)
        inc (ix-1)
        inc (ix+1)
        inc (ix+63)
        inc (ix+64)
        inc (ix+65)
        set 4,(ix+0)
        inc bc
mend


;==============================================================================
;### C O D E   A R E A #######################################################
;==============================================================================

;### APPLICATION HEADER #######################################################

;Definition (before the initialization phase)
prgdatcod       equ 0           ;Length of the code area (includes this header)
prgdatdat       equ 2           ;Length of the data area
prgdattra       equ 4           ;Length of the transfer area
prgdatorg       equ 6           ;Original origin of the assembler code
prgdatrel       equ 8           ;Number of entries in the relocator table
prgdatstk       equ 10          ;Length of the stack in bytes
prgdatrs1       equ 12          ;*reserved* (3 bytes)
prgdatnam       equ 15          ;program name (24+1[0] chars)
prgdatflg       equ 40          ;flags (+1=16colour icon available)
prgdat16i       equ 41          ;file offset of 16colour icon
prgdatrs2       equ 43          ;*reserved* (5 bytes)
prgdatidn       equ 48          ;"SymExe10" SymbOS executable file identification
prgdatcex       equ 56          ;Length of additional reserved code area memory
prgdatdex       equ 58          ;Length of additional reserved data area memory
prgdattex       equ 60          ;Length of additional reserved transfer area memory
prgdatres       equ 62          ;*RESERVED* (26 bytes)
prgdatver       equ 88          ;required OS version (minor, major)
prgdatism       equ 90          ;Application icon (small version), 8x8 pixel, SymbOS graphic format
prgdatibg       equ 109         ;Application icon (big version), 24x24 pixel, SymbOS graphic format
prgdatlen       equ 256         ;Total length of the header

;Definition (after the initialization phase)
prgpstdat       equ 6           ;Address of the data area
prgpsttra       equ 8           ;Address of the transfer area
prgpstspz       equ 10          ;Additional sub process IDs; 4 process IDs can be registered here
prgpstbnk       equ 14          ;Ram bank number (1-8), where the application is located
prgpstmem       equ 48          ;Additional memory areas; 8 memory areas can be registered here,
                                ;each entry consists of 5 bytes:
                                ;00  1B  Ram bank number (1-8; if 0, the entry will be ignored)
                                ;01  1W  Address
                                ;03  1W  Length
prgpstnum       equ 88          ;Application ID
prgpstprz       equ 89          ;Main process ID

;------------------------------------------------------------------------------
;Header data for this applications
prgcodbeg   dw prgdatbeg-prgcodbeg  ;Länge Code-Teil
            dw prgtrnbeg-prgdatbeg  ;Länge Daten-Teil
            dw prgtrnend-prgtrnbeg  ;Länge Transfer-Teil
prgdatadr   dw #1000                ;Original-Origin                    POST Adresse Daten-Teil
prgtrnadr   dw relocate_count       ;Anzahl Einträge Relocator-Tabelle  POST Adresse Transfer-Teil
prgprztab   dw prgstk-prgtrnbeg     ;Länge Stack                        POST Tabelle Prozesse
            dw 0                    ;*reserved*
prgbnknum   db 0                    ;*reserved*                         POST bank number
            db "Conway's Game of Life":ds 3:db 0 ;Name
            db 1                    ;flags (+1=16c icon)
            dw prgicn16c-prgcodbeg  ;16 colour icon offset
            ds 5                    ;*reserved*
prgmemtab   db "SymExe10"           ;SymbOS-EXE-Kennung                 POST Tabelle Speicherbereiche
            dw celxln*celyln*2+512+2    ;zusätzlicher Code-Speicher
            dw celxln*celyln            ;zusätzlicher Data-Speicher
            dw 0                        ;zusätzlicher Transfer-Speicher
            ds 26                   ;*reserviert*
            db 0,2                  ;required OS version (2.0)
prgicnsml   db 2,8,8,#F0,#F0,#82,#78,#87,#78,#92,#92,#96,#96,#92,#F0,#96,#F0,#F0,#F0
prgicnbig   db 6,24,24
            db #FE,#FE,#FE,#FE,#FE,#FE,#F8,#F8,#F8,#F8,#F8,#F8,#F8,#F8,#F8,#F8,#F8,#F8,#F0,#F0,#F0,#F0,#F0,#F0,#FE,#01,#01,#FE,#FE,#FE,#F8,#07,#07,#F8,#F8,#F8,#F8,#07,#07,#F8,#F8,#F8,#F0,#0F,#0F,#F0,#F0,#F0
            db #FE,#01,#FE,#01,#FE,#FE,#F8,#07,#F8,#07,#F8,#F8,#F8,#07,#F8,#07,#F8,#F8,#F0,#0F,#F0,#0F,#F0,#F0,#FE,#01,#FE,#FE,#FE,#01,#F8,#07,#F8,#F8,#F8,#07,#F8,#07,#F8,#F8,#F8,#07,#F0,#0F,#F0,#F0,#F0,#0F
            db #FE,#FE,#FE,#01,#01,#FE,#F8,#F8,#F8,#07,#07,#F8,#F8,#F8,#F8,#07,#07,#F8,#F0,#F0,#F0,#0F,#0F,#F0,#FE,#FE,#FE,#FE,#FE,#FE,#F8,#F8,#F8,#F8,#F8,#F8,#F8,#F8,#F8,#F8,#F8,#F8,#F0,#F0,#F0,#F0,#F0,#F0


;### PRGPRZ -> Programm-Prozess
dskprzn     db 2
sysprzn     db 3
windatprz   equ 3   ;Prozeßnummer
windatsup   equ 51  ;Nummer des Superfensters+1 oder 0
prgwin      db 0    ;Nummer des Haupt-Fensters
diawin      db 0    ;Nummer des Dialog-Fensters

prgprz  ld a,(prgprzn)
        ld (prgwindat+windatprz),a
        call SySystem_HLPINI

        ld hl,celgen_wasdead
        ld a,h
        ld (celgen0+1),a

        ld hl,celstk
        ld a,(prgbnknum)
        ld e,6                  ;low priority
        call SyKernel_MTADDP    ;add new process
        jp c,prgend             ;too many processes -> quit
        ld a,(celprzn)
        ld (prgcodbeg+prgpstspz+0),a

        ld hl,celfld
        ld bc,celxln*celyln
        add hl,bc
        ld (celawr),hl
        add hl,bc
        ld (celabf),hl

        call celrul0
        call ctlclr0
        call ctlshw0
        call ctlrnv0
        call prgcfg

        ld c,MSC_DSK_WINOPN
        ld a,(prgbnknum)
        ld b,a
        ld de,prgwindat
        call msgsnd             ;Fenster aufbauen
prgprz1 call msgdsk             ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        cp MSR_DSK_WOPNER
        jp z,prgend             ;kein Speicher für Fenster -> Prozeß beenden
        cp MSR_DSK_WOPNOK
        jr nz,prgprz1           ;andere Message als "Fenster geöffnet" -> ignorieren
        ld a,(prgmsgb+4)
        ld (prgwin),a           ;Fenster wurde geöffnet -> Nummer merken

prgprz0 call msgget
        jr nc,prgprz0
        cp MSR_DSK_WCLICK       ;*** Fenster-Aktion wurde geklickt
        jr nz,prgprz0
        ld e,(iy+1)
        ld a,(prgwin)
        cp e
        jr nz,prgprz0
        ld a,(iy+2)             ;*** HAUPT-FENSTER
        cp DSK_ACT_CLOSE        ;*** Close wurde geklickt
        jp z,prgend
        cp DSK_ACT_MENU         ;*** a menu entry has been clicked
        jr z,prgprz4
        cp DSK_ACT_CONTENT      ;*** Inhalt wurde geklickt
        jr nz,prgprz0
prgprz4 ld l,(iy+8)
        ld h,(iy+9)
        ld a,l
        or h
        jr z,prgprz0
        ld a,(iy+3)             ;A=Klick-Typ (0/1/2=Maus links/rechts/doppelt, 7=Tastatur)
        jp (hl)

;### PRGEND -> End Program
prgend  ld a,(prgprzn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        ld iy,prgmsgb
        ld (iy+0),MSC_SYS_PRGEND
        ld a,(prgcodbeg+prgpstnum)
        ld (iy+1),a
        rst #10
prgend0 rst #30
        jr prgend0

;### PRGINF -> open info window
prginf  ld hl,prgmsginf         ;*** Info-Window
        ld b,1+128
        call prginf0
        jp prgprz0
prginf0 ld (prgmsgb+1),hl
        ld a,(prgbnknum)
        ld c,a
        ld (prgmsgb+3),bc
        ld a,MSC_SYS_SYSWRN
        ld (prgmsgb),a
        ld a,(prgprzn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        ld iy,prgmsgb
        rst #10
        ret

;### PRGCFG -> Generates datafile path
cfgnam  db "gamelife.dat",0:cfgnam0
cfgpth  ds 128

prgcfg  ld hl,(prgcodbeg)
        ld de,prgcodbeg
        dec h
        add hl,de           ;HL = CodeEnd = path
        ld de,cfgpth
        push de
        ld bc,128
        ldir
        pop hl
        ld e,l
        ld d,h              ;DE=HL
        ld b,127
prgcfg1 ld a,(hl)           ;search end of path
        or a
        jr z,prgcfg2
        inc hl
        djnz prgcfg1
        jr prgcfg4
        ld a,127
        sub b
        jr z,prgcfg4
        ld b,a
prgcfg2 dec hl              ;search start of filename
        ld a,(hl)
        cp "/"
        jr z,prgcfg3
        cp "\"
        jr z,prgcfg3
        cp ":"
        jr z,prgcfg3
        djnz prgcfg2
        jr prgcfg4
prgcfg3 inc hl
        ex de,hl
prgcfg4 ld hl,cfgnam        ;replace application filename with config filename
        ld bc,cfgnam0-cfgnam
        ldir
        ret


;==============================================================================
;### CONTROL-ROUTINES #########################################################
;==============================================================================

ctlmod  db 0    ;0=stop, 1=run cell generator
ctlmod0 db 0    ;temp

;### CTLSHW -> Shows field and counters
ctlshw  call ctlshw0
        ld e,3
        call msgsnd0
        ld de,7*256+256-3
        jp msgsnd0
ctlshw0 call celplt             ;updates controls
        ld hl,(celclv)
        push hl
        ex de,hl
        ld bc,100
        call clcmul
        ld c,l
        ld b,h
        ld de,62*62
        call clcdiv
        pop ix
        ld iy,txtcl1val
        ld a,l
        push af
        ld e,4
        call clcnum
        pop af
        push af
        call ctlshw1
        ld hl,62*62
        ld bc,(celclv)
        or a
        sbc hl,bc
        ld a,l:db #dd:ld l,a
        ld a,h:db #dd:ld h,a
        ld iy,txtcl0val
        ld e,4
        call clcnum
        pop bc
        ld a,100
        sub b
        call ctlshw1
        ld ix,(celcgn)
        ld e,5
        ld iy,txtgenval
        jp clcnum
;hl=number, de=destination
ctlshw1 ld (iy+1)," "
        ld (iy+2),"("
        cp 100
        jr c,ctlshw2
        sub 100
        ld (iy+3),"1"
        inc iy
ctlshw2 call clcdez
        ld (iy+3),l
        ld (iy+4),h
        ld (iy+5),"%"
        ld (iy+6),")"
        ld (iy+7),0
        ret

;### CTLRUN -> Runs/stops cell generation
ctlrun  ld hl,ctlmod
        ld a,(hl)
        xor 1
        call ctlrun2
        jp prgprz0
ctlrun0 ld hl,ctlmod
        xor a
ctlrun2 ld (hl),a
        ld hl,txtbutrun
        jr z,ctlrun1
        ld a,(celprzn)
        call msgsnd1        ;wake up cell calculator process
        ld hl,txtbutstp
ctlrun1 ld (prgwinobj1),hl
        ld e,19
        jp msgsnd0

;### CTLPOS -> get cell position
;### Input      IY=message  buffer
;### Output     CF=1 ok, E,D=X,Ypos
ctlpos  ld a,(iy+4)
        sub 4:srl a:cp 62
        ret nc
        inc a
        ld e,a              ;e=xpos (1-62)
        ld a,(iy+6)
        sub 4:srl a:cp 62
        ret nc
        inc a
        ld d,a              ;d=ypos (1-62)
        ret

;### CTLADR -> get cell address
;### Input      E,D=X,Ypos
;### Output     HL=address
ctladr  ld c,e
        ld a,d
        ld de,celxln
        ld b,d
        call clcm16
        add hl,bc
        ld bc,(celard)
        add hl,bc
        ret

;### CTLPAU -> pauses cell calculation for edit action
ctlpau  ld a,(ctlmod)
        ld (ctlmod0),a
        xor a
        ld (ctlmod),a
        ret

;### CTLCON -> continues cell calculation, if active before
ctlcon  ld a,(ctlmod0)
        or a
        ret z
        ld (ctlmod),a
        ld (celprzr),a
        ld a,(celprzn)
        jp msgsnd1

;### CTLEDT -> Edits cell field
ctledt  or a
        jp nz,prgprz0       ;react only on left mouse clicks
        call ctlpos         ;e,d=x,y
        jp nc,prgprz0
        ld a,(ctlsetflg)
        or a
        jr nz,ctlfig
        call ctlpau
        call ctladr
        push hl:pop ix
        ld bc,(celclv)
        bit 4,(ix+0)
        jr nz,ctledt1
        cell_alive
        jr ctledt2
ctledt1 cell_dead
ctledt2 ld (celclv),bc
ctledt3 call ctlcon
        call ctlshw
        jp prgprz0

;### CTLFIG -> Adds figure to cell field
ctlfig  push de
        ld de,(ctlfigobj+12)
        call filfig
        pop de              ;e=xpos, d=ypos
        jp c,prgprz0

ctlfig0 call ctlpau
        ld c,(hl)           ;c=xlen
        inc hl
        ld b,(hl)           ;b=ylen
        inc hl              ;hl=bitmapdata
        call ctlfiga
        call celrec
        jr ctledt3

;c=xlen,b=ylen,hl=bitmap,e=xpos,d=ypos
ctlfiga db #dd:ld h,e       ;ixh=xpos
ctlfig1 push bc             ;** line loop
        push hl
        db #dd:ld l,c
        ld b,1
        db #dd:ld e,h
ctlfig2 dec b               ;** column loop
        jr nz,ctlfig3
        ld c,(hl)
        inc hl
        ld b,8
ctlfig3 push hl
        push de
        push bc
        call ctladr
        ld a,16
        pop bc
        rl c
        jr c,ctlfig4
        cpl
        and (hl)
        jr ctlfig5
ctlfig4 or (hl)
ctlfig5 ld (hl),a
        ld a,c
        pop de
        pop hl
        inc e
        ld a,e              ;don't cross right border
ctlfigx cp 63
        jr z,ctlfig6
        db #dd:dec l
        jr nz,ctlfig2
ctlfig6 inc d
        pop hl
        pop bc
        ld a,d              ;don't cross lower border
ctlfigy cp 63
        jr z,ctlfig7
        ld a,c
        add 7
        and #f8
        rra:rra:rra         ;a=figure width in bytes
        add l
        ld l,a
        adc h
        sub l
        ld h,a              ;hl=next line
        djnz ctlfig1
ctlfig7 ret

;### CTLFGC -> Figure has been choosed
ctlfgc  ld a,1
        ld (ctlsetflg),a
        ld de,256*12+256-2
        call msgsnd0
        jp prgprz0

;### CTLRNV -> Sets random value
ctlrnv  call ctlrnv0
        ld e,16
        call msgsnd0
        jp prgprz0
ctlrnv0 ld a,(ctlobjdat1+2)
        ld hl,txtrndval
        cp 100
        jr c,ctlrnv1
        sub 100
        ld (hl),"1"
        inc hl
ctlrnv1 ex de,hl:call clcdez:ex de,hl
        ld (hl),e:inc hl
        ld (hl),d:inc hl
        ld (hl),"%":inc hl:ld (hl),0
        ret

;### CTLRND -> Generates random cell field
ctlrnd  call ctlrun0
        ld a,(ctlobjdat1+2) ;a=0-100
        ld b,a
        xor a
        ld c,a
        ld de,100*16
        call clcdiv
        ld a,l              ;a=0-16
        ld (ctlrnd3+1),a
        ld a,r
        and 127
        ld hl,rndval
        push hl
        ld de,rndval+1
        ld (hl),a
        ld bc,4
        ldir
        pop hl
        call FLO_RANDOMIZE
        ld iy,(celard)
        ld bc,celxln+1
        add iy,bc
        ld c,31
ctlrnd1 ld b,31             ;double row loop
ctlrnd2 push bc             ;double column loop
        ld hl,rndval
        call FLO_RND
ctlrnd3 ld bc,256*2
ctlrnd4 inc hl              ;2x2 cell loop
        rld:and 15:cp c
        jr c,ctlrnd5
        res 4,(iy+0)
        jr ctlrnd6
ctlrnd5 set 4,(iy+0)
ctlrnd6 rld:and 15:cp c
        jr c,ctlrnd7
        res 4,(iy+64)
        jr ctlrnd8
ctlrnd7 set 4,(iy+64)
ctlrnd8 inc iy
        djnz ctlrnd4
        pop bc
        djnz ctlrnd2
        ld de,2+64
        add iy,de
        dec c
        jr nz,ctlrnd1
        call celrec
        call ctlclr1        ;reset gen counter
        call ctlshw
        jp prgprz0

;### CTLCLR -> Clears cellfield and resets generation counter
ctlclr  call ctlrun0
        call ctlclr0
        call ctlshw
        jp prgprz0
ctlclr0 ld hl,(celard)
        ld e,l:ld d,h
        inc de
        ld (hl),0
        ld bc,celxln*celyln-1
        ldir
        ld (celclv),bc
ctlclr1 ld hl,0
        ld (celcgn),hl
        ret


;==============================================================================
;### FILE-ROUTINES ############################################################
;==============================================================================

filhnd  db 0

;### FILFIG -> Gets figure (from disc or buffer)
;### Input      DE=figure number
;### Output     HL=figure data, CF=1 error, no data
filfign dw -1
filfiga dw 0

filfig  ld hl,(filfign)
        or a
        sbc hl,de
        jr nz,filfig1
        ld hl,(celabf)
        ret
filfig1 push de
        ld ix,(prgbnknum-1)         ;ixh=bank number
        ld hl,cfgpth
        call SySystem_CallFunction  ;open file
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOPN
        pop ix
        jr c,filfig5                ;** error **
        push ix
        ld (filhnd),a
        inc ix                      ;skip preset-address
        add ix,ix
        ld iy,0
        ld c,0
        call SySystem_CallFunction  ;set pointer to figure-address
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILPOI
        jr c,filfig4
        ld a,(filhnd)
        ld hl,filfiga
        ld bc,2
        ld de,(prgbnknum)
        call SySystem_CallFunction  ;get figure-address
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILINP
        jr c,filfig4
        ld ix,(filfiga)
        ld iy,0
        ld c,0
        ld a,(filhnd)
        call SySystem_CallFunction  ;set pointer to figure-data
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILPOI
        jr c,filfig4
        ld a,(filhnd)
        ld hl,(celabf)
        push hl
        ld bc,512+2
        ld de,(prgbnknum)
        call SySystem_CallFunction  ;get figure-data
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILINP
        pop hl
        jr c,filfig4
        pop de
        ld (filfign),de
filfig3 push hl
        ld a,(filhnd)
        call SySystem_CallFunction  ;close file
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILCLO
        pop hl
        or a
        ret
filfig4 pop hl
        call filfig3
        scf
filfig5 ld hl,-1
        ld (filfign),hl
        ret

;### FILLOD -> Loads cell field
fillod  ld c,0
        call filsav0
        jp nz,prgprz0
        call ctlrun0
        call ctlclr1
        ld ix,(prgbnknum-1)         ;ixh=bank number
        ld hl,filpth
        call SySystem_CallFunction  ;open file
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOPN
        jp c,prgprz0                ;** error **
        ld (filhnd),a
fillod0 ld hl,celcfgbeg
        ld bc,celcfgend-celcfgbeg
        ld de,(prgbnknum)
        push de
        call SySystem_CallFunction  ;read field config
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILINP
        push af
        call celrul0
        pop af
        pop de
        jr c,fillod1
        jr nz,fillod1
        ld a,(filhnd)
        ld hl,(celabf)
        ld bc,64*64/8
        call SySystem_CallFunction  ;read cells
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILINP
        jr c,fillod1
        ld bc,celxln*256+celyln
        ld hl,(celabf)
        ld de,0
        ld a,64
        ld (ctlfigx+1),a
        ld (ctlfigy+1),a
        call ctlfiga
        ld a,63
        ld (ctlfigx+1),a
        ld (ctlfigy+1),a
        call celrec
        or a
fillod1 push af
        ld a,(filhnd)               ;close file
        call SySystem_CallFunction
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILCLO
        pop af
        ;...error message
        call ctlshw
        ld e,10
        call msgsnd0
        ld de,256*32+256-9
        call msgsnd0
        ld de,256*41+256-9
        call msgsnd0
        jp prgprz0

;### FILSAV -> Saves cell field
filsav  ld c,64
        call filsav0
        jp nz,prgprz0
        xor a
        ld ix,(prgbnknum-1)         ;ixh=bank number
        ld hl,filpth
        call SySystem_CallFunction  ;open file
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILNEW
        jp c,prgprz0                ;** error **
        ld (filhnd),a
        ld hl,celcfgbeg
        ld bc,celcfgend-celcfgbeg
        ld de,(prgbnknum)
        push de
        call SySystem_CallFunction  ;write field config
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOUT
        pop de
        jr c,filsav1
        or a
        jr nz,filsav1
        push de
        call celbin                 ;convert current field to bin
        pop de
        ld a,(filhnd)
        ld hl,(celabf)
        ld bc,64*64/8
        call SySystem_CallFunction  ;write cells
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOUT
filsav1 ld a,(filhnd)               ;close file
        call SySystem_CallFunction
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILCLO
        jp prgprz0
filsav0 ld hl,filmsk                ;select filename
        ld a,(prgbnknum)
        or c
        ld c,8
        ld ix,200
        ld iy,4000
        ld de,prgwindat
        call SySystem_SELOPN
        or a
        ret

;### FILPRE -> Loads preset
;### Input      A=number (0-6)
filpreo dw 0

filpsa  xor a
        jr filpre
filpsb  ld a,1
        jr filpre
filpsc  ld a,2
        jr filpre
filpsd  ld a,3
        jr filpre
filpse  ld a,4
        jr filpre
filpsf  ld a,5
        jr filpre
filpsg  ld a,6

filpre  ld h,a
        ld l,0
        ld b,l
        add hl,hl                       ;*512
        add a:add a:add a:add a:add a   ;*32
        ld c,a
        add hl,bc                       ;*544
        push hl
        call ctlrun0
        call ctlclr1
        ld ix,(prgbnknum-1)         ;ixh=bank number
        ld hl,cfgpth
        call SySystem_CallFunction  ;open file
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILOPN
        pop hl
        jp c,prgprz0                ;** error **
        push hl
        ld (filhnd),a
        ld hl,filpreo
        ld bc,2
        ld de,(prgbnknum)
        call SySystem_CallFunction  ;read preset-address
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILINP
        pop ix
        jp c,fillod1
        ld de,(filpreo)
        add ix,de
        ld iy,0
        ld c,0
        ld a,(filhnd)
        call SySystem_CallFunction  ;set pointer to presetdata
        db MSC_SYS_SYSFIL
        db FNC_FIL_FILPOI
        jp c,fillod1
        ld a,(filhnd)
        jp fillod0


SySystem_SELOPN
;******************************************************************************
;*** Name           Dialogue_FileSelector_Command
;*** Input          HL = File mask, path and name address (#C000-#FFFF)
;***                     00  3B  File extension filter (e.g. "*  ")
;***                     03  1B  0
;***                     04 256B path and filename
;***                A  = [Bit0-3] File mask, path and name ram bank (0-15)
;***                     [Bit6  ] Flag, if "open" (0) or "save" (1) dialogue
;***                     [Bit7  ] Flag, if file (0) or directory (1) selection
;***                C  = Attribute filter
;***                     Bit0 = 1 -> don't show read only files
;***                     Bit1 = 1 -> don't show hidden files
;***                     Bit2 = 1 -> don't show system files
;***                     Bit3 = 1 -> don't show volume ID entries
;***                     Bit4 = 1 -> don't show directories
;***                     Bit5 = 1 -> don't show archive files
;***                IX = Maximum number of directory entries
;***                IY = Maximum size of directory data buffer
;***                DE = Data record of the caller window; the file selector
;***                     window will be a super window of it, during its open)
;*** Output         A  = Success status
;***                     0 -> The user choosed a file and closed the dialogue
;***                          with "OK". The complete file path and name can be
;***                          found in the filepath buffer of the application.
;***                     1 -> The user aborted the file selection. The content
;***                          of the applications filepath buffer is unchanged.
;***                     2 -> The file selection dialogue is currently used by
;***                          another application. It can only be opened once
;***                          at the same time. The user should close the
;***                          dialogue first before it can be opened again by
;***                          the application.
;***                     3 -> Memory full. There was not enough memory
;***                          available for the directory buffer and/or the
;***                          list data structure.
;***                     4 -> No window available. The desktop manager couldn't
;***                          open a new window for the dialogue, as the
;***                          maximum number of windows (32) has already been
;***                          reached.
;*** Destroyed      F,BC,DE,HL,IX,IY
;*** Description    Opens the file selection dialogue. In this dialogue the user
;***                can move through the directory structure, change the drive and
;***                search and select a file or a directory for opening or saving.
;***                If you specify a path, the dialogue will start directly in the
;***                directory. If you append a filename, too, it will be used as
;***                the preselected file.
;***                You can filter the entries of the directory by attributes and
;***                filename extension. We recommend always to set Bit3 of the
;***                attribute filter byte.
;***                The File mask/path/name string (260 bytes) must always be
;***                placed in the transfer ram area (#C000-#FFFF). For more
;***                information about this memory types see the "applications"
;***                chapter.
;***                Please note, that the system will reserve memory to store the
;***                listed directory entries and the data structure of the list.
;***                With IX and IY you can choose, how much memory should be used.
;***                We recommend to set the number of entries between 100 and 200
;***                (Amsdos supports a maximum amount of 64 entries) and to set the
;***                data buffer between 5000 and 10000.
;******************************************************************************
        ld (SySSOpW),de
        push iy
        ld iy,prgmsgb
        ld (iy+6),a
        ld (iy+7),c
        ld (iy+8),l
        ld (iy+9),h
        db #dd:ld a,l
        ld (iy+10),a
        db #dd:ld a,h
        ld (iy+11),a
        pop de
        ld (iy+12),e
        ld (iy+13),d
        ld c,MSC_SYS_SELOPN
        call SySystem_SendMessage
SySSOp1 call SySystem_WaitMessage
        cp MSR_SYS_SELOPN
        jr nz,SySSOp1
        ld a,(iy+1)
        cp -1
        ret nz
        ld ix,(SySSOpW)
        ld a,(iy+2)
        ld (ix+51),a
        jr SySSOp1
SySSOpW dw 0

SySystem_SendMessage
;******************************************************************************
;*** Input          C       = Command
;***                HL,A,DE = additional Parameters
;*** Output         -
;*** Destroyed      AF,BC,DE,HL,IX,IY
;*** Description    Sends a message to the system manager
;******************************************************************************
        ld iy,prgmsgb
        ld (iy+0),c
        ld (iy+1),l
        ld (iy+2),h
        ld (iy+3),a
        ld (iy+4),e
        ld (iy+5),d
        db #dd:ld h,3       ;3 is the number of the system manager process
        ld a,(prgprzn)
        db #dd:ld l,a
        rst #10
        ret

SySystem_WaitMessage
;******************************************************************************
;*** Input          -
;*** Output         IY = message buffer
;***                A  = first byte in the Message buffer (IY+0)
;*** Destroyed      AF,BC,DE,HL,IX,IY
;*** Description    Sends a message to the desktop manager, which includes the
;***                window ID and additional parameters
;******************************************************************************
        ld iy,prgmsgb
SySWMs1 db #dd:ld h,3       ;3 is the number of the system manager process
        ld a,(prgprzn)
        db #dd:ld l,a
        rst #08             ;wait for a system manager message
        db #dd:dec l
        jr nz,SySWMs1
        ld a,(iy+0)
        ret

SySystem_CallFunction
;******************************************************************************
;*** Name           System_CallFunction
;*** Input          ((SP+0)) = System manager command
;***                ((SP+1)) = Function ID
;***                AF,BC,DE,HL,IX,IY = Input for the function
;*** Output         AF,BC,DE,HL,IX,IY = Output from the function
;*** Destroyed      -
;*** Description    Calls a function via the system manager. This function is
;***                needed to have access to the file manager.
;******************************************************************************
        ld (prgmsgb+04),bc      ;copy registers into the message buffer
        ld (prgmsgb+06),de
        ld (prgmsgb+08),hl
        ld (prgmsgb+10),ix
        ld (prgmsgb+12),iy
        push af
        pop hl
        ld (prgmsgb+02),hl
        pop hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        push hl
        ld (prgmsgb+00),de      ;module und funktion number
        ld a,e
        ld (SyCallN),a
        ld iy,prgmsgb
        ld a,(prgprzn)
        db #dd:ld l,a
        ld a,3
        db #dd:ld h,a
        rst #10                 ;send message
SyCall1 rst #30
        ld iy,prgmsgb
        ld a,(prgprzn)
        db #dd:ld l,a
        ld a,3
        db #dd:ld h,a
        rst #18                 ;wait for answer
        db #dd:dec l
        jr nz,SyCall1
        ld a,(prgmsgb)
        sub 128
        ld e,a
        ld a,(SyCallN)
        cp e
        jr nz,SyCall1
        ld hl,(prgmsgb+02)      ;get registers out of the message buffer
        push hl
        pop af
        ld bc,(prgmsgb+04)
        ld de,(prgmsgb+06)
        ld hl,(prgmsgb+08)
        ld ix,(prgmsgb+10)
        ld iy,(prgmsgb+12)
        ret
SyCallN db 0

SyKernel_MTADDP
;******************************************************************************
;*** Name           Multitasking_Add_Process_Command
;*** Input          HL = Stack address (see notes below)
;***                A  = Ram bank (0-15)
;***                E  = Priority (1=highest, 7=lowest)
;*** Output         A  = Process ID
;***                CF = Success status
;***                     0 = OK
;***                     1 = the process couldn't been added, as the maximum
;***                         number of processes (32) has been reached
;*** Destroyed      BC,DE,HL,IX,IY
;*** Description    Adds a new process with a given priority and starts it
;***                immediately.
;***                Application processes usually will be started with priority 4.
;***                Note, that the desktop manager process runs with priority 1,
;***                the system manager process with 1. If you start a process,
;***                which should do some long and intensive calculation, you should
;***                choose a priority greater than 4, so that other applications
;***                will not be disturbed.
;***                The stack must always be placed between #C000 and #FFFF
;***                (transfer ram area). It must contain the start address of the
;***                process (or timer) routine at offset 12 and may contain the
;***                initial values of the registers. You can choose the size of the
;***                stack buffer by yourself, just be sure, that it is large
;***                enough.
;***                At offset 13 there must be a free byte. In this byte the kernel
;***                will write the ID of the process (or timer) after it has been
;***                started.
;*** Example(stack)                ds 128              ;Stack buffer
;***                stack_pointer: dw 0                ;initial value for IY
;***                               dw 0                ;initial value for IX
;***                               dw 0                ;initial value for HL
;***                               dw 0                ;initial value for DE
;***                               dw 0                ;initial value for BC
;***                               dw 0                ;initial value for AF
;***                               dw process_start    ;process start address
;***                process_id:    db 0                ;kernel writes the ID here
;******************************************************************************
        ld c,MSC_KRL_MTADDP
        call SyKernel_Message
        xor a
        cp l
        ld a,h
        ret

SyKernel_Message
;******************************************************************************
;*** Input          C        = Command
;***                HL,E,A,B = Additional parameters
;*** Output         HL       = returned parameters
;*** Destroyed      AF,BC,DE,IX,IY
;*** Description    Sends a message to the kernel, waits for the answer and
;***                returns the result
;******************************************************************************
        ld iy,prgmsgb
        ld (iy+0),c
        ld (iy+1),l
        ld (iy+2),h
        ld (iy+3),e
        ld (iy+4),a
        ld (iy+5),b
        ld a,c
        add 128
        ld (SyKMsgN),a
        db #dd:ld h,1       ;1 is the number of the kernel process
        ld a,(prgprzn)
        db #dd:ld l,a
        rst #10
SyKMsg1 db #dd:ld h,1       ;1 is the number of the kernel process
        ld a,(prgprzn)
        db #dd:ld l,a
        rst #08             ;wait for a kernel message
        db #dd:dec l
        jr nz,SyKMsg1
        ld a,(SyKMsgN)
        cp (iy+0)
        jr nz,SyKMsg1
        ld l,(iy+1)
        ld h,(iy+2)
        ret
SyKMsgN db 0


;==============================================================================
;### SUB-ROUTINES #############################################################
;==============================================================================

;### MSGGET -> Receives Message
;### Output     CF=0 -> no message available, CF=1 -> IXH=sender, (recmsgb)=message, A=(recmsgb+0), IY=recmsgb
;### Destroyed
msgget  ld a,(prgprzn)
        db #dd:ld l,a
        db #dd:ld h,-1
        ld iy,prgmsgb
        rst #08
msgget1 or a
        db #dd:dec l
        ret nz
        ld iy,prgmsgb
        ld a,(iy+0)
        or a
        jp z,prgend
        scf
        ret

;### MSGDSK -> Receives Message from Desktop-Process
;### Output     CF=0 -> no message available, CF=1 -> IXH=sender, (recmsgb)=message, A=(recmsgb+0), IY=recmsgb
;### Destroyed
msgdsk  call msgget
        jr nc,msgdsk            ;keine Message
        ld a,(dskprzn)
        db #dd:cp h
        jr nz,msgdsk            ;Message von anderem als Desktop-Prozeß -> ignorieren
        ld a,(prgmsgb)
        ret

;### MSGSND -> Sends Message to Desktop-Process
;### Input      C=Command, B/E/D/L/H=Parameter1/2/3/4/5
msgsnd0 ld c,MSC_DSK_WINDIN
        ld a,(prgwin)
        ld b,a
msgsnd  ld a,(dskprzn)
msgsnd1 db #dd:ld h,a
        ld a,(prgprzn)
        db #dd:ld l,a
        ld iy,prgmsgb
        ld (iy+0),c
        ld (iy+1),b
        ld (iy+2),e
        ld (iy+3),d
        ld (iy+4),l
        ld (iy+5),h
        rst #10
        ret

;### CLCDEZ -> Converts Byte into two Decimaldigits
;### Input      A=value
;### Output     L=10.digit, H=1.digit
;### Destroyed  AF
clcdez  ld l,0
clcdez1 sub 10
        jr c,clcdez2
        inc l
        jr clcdez1
clcdez2 add "0"+10
        ld h,a
        ld a,"0"
        add l
        ld l,a
        ret

;### CLCNUM -> Converts 16bit Number into sting (0-terminated)
;### Input      IX=value, IY=address, E=max numbers of digits
;### Output     (IY)=last digit
;### Destroyed  AF,BC,DE,HL,IX,IY
clcnumt dw 1,10,100,1000,10000
clcnum  ld d,0
        ld b,e
        dec e
        push ix
        pop hl
        ld ix,clcnumt
        add ix,de
        add ix,de               ;IX=first divider
        dec b
        jr z,clcnum4
        ld c,0
clcnum1 ld e,(ix)
        ld d,(ix+1)
        dec ix
        dec ix
        ld a,"0"
        or a
clcnum2 sbc hl,de
        jr c,clcnum5
        inc c
        inc a
        jr clcnum2
clcnum5 add hl,de
        inc c
        dec c
        jr z,clcnum3
        ld (iy+0),a
        inc iy
clcnum3 djnz clcnum1
clcnum4 ld a,"0"
        add l
        ld (iy+0),a
        ld (iy+1),0
        ret

;### CLCM16 -> Multipliziert zwei Werte (16bit)
;### Eingabe    A=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1*Wert2 (16bit)
;### Veraendert AF,DE
clcm16  ld hl,0
clcm161 or a
        ret z
        rra
        jr nc,clcm162
        add hl,de
clcm162 sla e
        rl d
        jr clcm161

;### CLCMUL -> Multipliziert zwei Werte (24bit)
;### Eingabe    BC=Wert1, DE=Wert2
;### Ausgabe    A,HL=Wert1*Wert2 (24bit)
;### Veraendert F,BC,DE,IX
clcmul  ld ix,0
        ld hl,0
clcmul1 ld a,c
        or b
        jr z,clcmul3
        srl b
        rr c
        jr nc,clcmul2
        add ix,de
        ld a,h
        adc l
        ld h,a
clcmul2 sla e
        rl d
        rl l
        jr clcmul1
clcmul3 ld a,h
        db #dd:ld e,l
        db #dd:ld d,h
        ex de,hl
        ret

;### CLCDIV -> Dividiert zwei Werte (24bit)
;### Eingabe    A,BC=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1/Wert2, DE=Wert1 MOD Wert2
;### Veraendert AF,BC,DE,IX,IYL
clcdiv  db #dd:ld l,e
        db #dd:ld h,d   ;IX=Wert2(Nenner)
        ld e,a          ;E,BC=Wert1(Zaehler)
        ld hl,0
        db #dd:ld a,l
        or d
        ret z
        ld d,l          ;D,HL=RechenVar
        db #fd:ld l,24  ;IYL=Counter
clcdiv1 rl c
        rl b
        rl e
        adc hl,hl
        rl d
        ld a,l
        db #dd:sub l
        ld l,a
        ld a,h
        db #dd:sbc h
        ld h,a
        ld a,d
        sbc 0
        ld d,a          ;D,HL=D,HL-IX
        jr nc,clcdiv2
        ld a,l
        db #dd:add l
        ld l,a
        ld a,h
        db #dd:adc h
        ld h,a
        ld a,d
        adc 0
        ld d,a
        scf
clcdiv2 db #fd:dec l
        jr nz,clcdiv1
        ex de,hl        ;DE=Wert1 MOD Wert2
        ld a,c
        rla
        cpl
        ld l,a
        ld a,b
        rla
        cpl
        ld h,a          ;HL=Wert1 DIV Wert2
        ret

FLO_VALUE_RND       ds 4
rndval  ds 5

;### FLO_RANDOMIZE0 -> RND seek to 0
;### Unchanged  AF,BC,DE,IX,IY
.FLO_RANDOMIZE0
        ld hl,#8965
        ld (FLO_VALUE_RND+2),hl
        ld hl,#6c07
        ld (FLO_VALUE_RND),hl
        ret

;### FLO_RANDOMIZE -> RND seek to (HL)
;### Input      (HL)=value
;### Unchanged  C,IY,FLO(HL)
.FLO_RANDOMIZE
        ex de,hl
        call FLO_RANDOMIZE0
        ex de,hl
        call FLO_SGN
        ret z
        ld de,FLO_VALUE_RND
        ld b,#04
.l3151
        ld a,(de)
        xor (hl)
        ld (de),a
        inc de
        inc hl
        djnz l3151
        ret

;### FLO_RND -> Gets next RND value
;### Input      (HL)=Destination
;### Output     (HL)=new RND
;### Unchanged  HL,IY
.FLO_RND
        push hl
        ld hl,(FLO_VALUE_RND+2)
        ld bc,#6c07
        call l319c
        push hl
        ld hl,(FLO_VALUE_RND)
        ld bc,#8965
        call l319c
        push de
        push hl
        ld hl,(FLO_VALUE_RND+2)
        call l319c
        ex (sp),hl
        add hl,bc
        ld (FLO_VALUE_RND),hl
        pop hl
        ld bc,#6c07
        adc hl,bc
        pop bc
        add hl,bc
        pop bc
        add hl,bc
        ld (FLO_VALUE_RND+2),hl
        pop hl

;### FLO_LAST_RND -> Gets current RND value
;### Input      (HL)=Destination
;### Output     (HL)=current RND
;### Unchanged  HL,IY
.FLO_LAST_RND
        push hl
        pop ix
        ld hl,(FLO_VALUE_RND)
        ld de,(FLO_VALUE_RND+2)
        ld bc,#0000
        ld (ix+#04),#80
        jp l37ac
.l319c
        ex de,hl
        ld hl,#0000
        ld a,#11
.l31a2
        dec a
        ret z
        add hl,hl
        rl e
        rl d
        jr nc,l31a2
        add hl,bc
        jr nc,l31a2
        inc de
        jr l31a2

;### FLO_SGN -> Tests the sign of (HL)
;### Input      (HL)=value
;### Output     A=sign [-1 -> (HL)<0, 0 -> (HL)=0, 1 -> (HL)>0]
;###            ZF=1 -> (HL)=0, CF=1 -> (HL)<0
;### Unchanged  BC,DE,HL,IY,FLO(HL)
.FLO_SGN
        push hl
        pop ix
        ld a,(ix+#04)
        or a
        ret z
        ld a,(ix+#03)
        add a
        sbc a
        ret c
        inc a
        ret

;### FLO_VZW -> Changes the sign of (HL)
;### Input      (HL)=value
;### Output     (HL)=-(HL)
;### Unchanged  BC,DE,HL,IY
.FLO_VZW
        push hl
        pop ix
.l3734
        ld a,(ix+#03)
        xor #80
        ld (ix+#03),a
        ret
.l373d
        cp #21
        jr c,l3743
        ld a,#21
.l3743
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ld c,(hl)
        inc hl
        ld h,(hl)
        ld l,c
        ex de,hl
        set 7,d
        ld bc,#0000
        jr l375e
.l3753
        ld c,a
        ld a,b
        or l
        ld b,a
        ld a,c
        ld c,l
        ld l,h
        ld h,e
        ld e,d
        ld d,#00
.l375e
        sub #08
        jr nc,l3753
        add #08
        ret z
.l3765
        srl d
        rr e
        rr h
        rr l
        rr c
        dec a
        jr nz,l3765
        ret
.l3773
        jr nz,l378c
        ld d,a
        ld a,e
        or h
        or l
        or c
        ret z
        ld a,d
.l377c
        sub #08
        jr c,l379a
        ret z
        ld d,e
        ld e,h
        ld h,l
        ld l,c
        ld c,#00
        inc d
        dec d
        jr z,l377c
        ret m
.l378c
        dec a
        ret z
        sla c
        adc hl,hl
        rl e
        rl d
        jp p,l378c
        ret
.l379a
        xor a
        ret
.l379c
        push hl
        pop ix
        ld (ix+#04),b
        ld b,a
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ex de,hl
.l37ac
        ld a,(ix+#04)
        dec d
        inc d
        call p,l3773
        ld (ix+#04),a
.l37b7
        sla c
        jr nc,l37cd
        inc l
        jr nz,l37cd
        inc h
        jr nz,l37cd
        inc de
        ld a,d
        or e
        jr nz,l37cd
        inc (ix+#04)
        jr z,l37ea
        ld d,#80
.l37cd
        ld a,b
        or #7f
        and d
        ld (ix+#03),a
        ld (ix+#02),e
        ld (ix+#01),h
        ld (ix+#00),l
.l37dd
        push ix
        pop hl
        scf
        ret
.l37e2
        xor a
        ld (ix+#04),a
        jr l37dd
.l37e8
        ld b,#00
.l37ea
        push ix
        pop hl
        ld a,b
        or #7f
        ld (ix+#03),a
        or #ff
        ld (ix+#04),a
        ld (hl),a
        ld (ix+#01),a
        ld (ix+#02),a
        ret

SySystem_HLPFLG db 0    ;flag, if HLP-path is valid
SySystem_HLPPTH db "%help.exe "
SySystem_HLPPTH1 ds 128
SySHInX db ".HLP",0

SySystem_HLPINI
        ld hl,(prgcodbeg)
        ld de,prgcodbeg
        dec h
        add hl,de                   ;HL = CodeEnd = Command line
        ld de,SySystem_HLPPTH1
        ld bc,0
        db #dd:ld l,128
SySHIn1 ld a,(hl)
        or a
        jr z,SySHIn3
        cp " "
        jr z,SySHIn3
        cp "."
        jr nz,SySHIn2
        ld c,e
        ld b,d
SySHIn2 ld (de),a
        inc hl
        inc de
        db #dd:dec l
        ret z
        jr SySHIn1
SySHIn3 ld a,c
        or b
        ret z
        ld e,c
        ld d,b
        ld hl,SySHInX
        ld bc,5
        ldir
        ld a,1
        ld (SySystem_HLPFLG),a
        ret

hlpopn  ld a,(SySystem_HLPFLG)
        or a
        jp z,prgprz0
        ld a,(prgbnknum)
        ld d,a
        ld a,PRC_ID_SYSTEM
        ld c,MSC_SYS_PRGRUN
        ld hl,SySystem_HLPPTH
        ld b,l
        ld e,h
        call msgsnd1
        jp prgprz0


;==============================================================================
;### CELL-ROUTINES ############################################################
;==============================================================================

;figtab

cellln  equ 8
celard  dw celfld       ;read offset
celawr  dw 0            ;write offset
celabf  dw 0            ;buffer offset

celclv  dw 0            ;number of alive cells
celcgn  dw 0            ;number of generations

;### CELPRZ -> Cell calculation process
celprzr db 0    ;1=discard last update

celprz  ei
        ld ix,(celprzn)
        db #dd:ld h,-1
        ld iy,celmsgb
        rst #08
celprz1 ld a,(ctlmod)
        or a
        jr z,celprz
celprz2 ei
        ld bc,256*62+62
        call celgen
        di
        ld a,(ctlmod)
        or a
        jr z,celprz
        ld hl,celprzr
        dec (hl)
        ld (hl),0
        jr z,celprz2
        ld (celclv),bc
        ld hl,(celawr)
        ld de,(celard)
        ld (celard),hl
        ld (celawr),de
        ei
        ld hl,(celcgn)
        inc hl
        ld (celcgn),hl
        call ctlshw0
        ld e,3
        call celprz3
        ld de,7*256+256-3
        call celprz3
        jr celprz1
celprz3 ld c,MSC_DSK_WINDIN
        ld a,(prgwin)
        ld b,a
        ld a,(dskprzn)
        db #dd:ld h,a
        ld a,(celprzn)
        db #dd:ld l,a
        ld iy,celmsgb
        ld (iy+0),c
        ld (iy+1),b
        ld (iy+2),e
        ld (iy+3),d
        ld (iy+4),l
        ld (iy+5),h
        rst #10
        ret

;### CELRUL -> generates code from cell rules
;### Input      (celold+0-8),(celnew+0-8)=rules
;### Output     celgen_wasdead,celgen_wasalive modified
celrul  call celrul0
        jp prgprz0
celrul0 ld ix,celold
        ld hl,celgen_wasalive+1
        ld de,celgen_nowdead-celgen_wasalive-2*256+celgen_keep-celgen_wasalive-2
        call celrul1
        inc ix
        ld hl,celgen_wasdead+1
        ld de,celgen_keep-celgen_wasdead-2*256+celgen_nowalive-celgen_wasdead-2
celrul1 ld b,9
celrul2 bit 0,(ix+0)
        ld (hl),d
        jr z,celrul3
        ld (hl),e
celrul3 inc ix
        inc hl:inc hl
        dec e:dec e
        dec d:dec d
        djnz celrul2
        ret

;### CELGEN -> calculates next cell generation
;### Input      (celard)=old cell field [000SNNNN], celxln,celyln=size with border, (celclv)=old number of alive cells, celgen_wasdead/celgen_wasalive=rules as jumptable
;### Output     (celawr)=new cell field, HL=new number of alive cells
ds -$ mod 256
celgen_wasdead
        jr celgen_keep          ;3
        jr celgen_keep
        jr celgen_keep
        jr celgen_nowalive
        jr celgen_keep
        jr celgen_keep
        jr celgen_keep
        jr celgen_keep
        jr celgen_keep
        ds 7*2              ;N/A
celgen_wasalive
        jr celgen_nowdead
        jr celgen_nowdead
        jr celgen_keep
        jr celgen_keep
        jr celgen_nowdead
        jr celgen_nowdead
        jr celgen_nowdead
        jr celgen_nowdead
        jr celgen_nowdead
celgen_nowdead
        cell_dead
        jr celgen_keep  ;1,5 58,5
celgen_nowalive
        cell_alive
celgen_keep
        inc de          ;2
        inc ix          ;3
        dec iyh         ;2
        jr nz,celgen2   ;3 18 -> 69192=3,46fr / 294066=14,7fr; 9fr for 50% changes
        inc de:inc de
        inc ix:inc ix
        dec iyl
        jr nz,celgen1
        push bc
        ld a,(celtor)
        or a
        call nz,celtrc          ;torus -> clear border
        pop bc
        ret

celgen  ld a,(celtor)
        or a
        call nz,celtrs          ;torus -> mirror border
        ld hl,(celard)
        ld de,(celawr)
        push hl
        push de
        ld bc,celxln*celyln
        ldir
        pop ix
        pop hl
        ld bc,celxln+1
        add hl,bc
        ex de,hl
        add ix,bc
        ld bc,(celclv)
celgen0 ld h,0                  ;celgen_wasdead/256
        ld iyl,celyln-2
celgen1 ld iyh,celxln-2
celgen2 ld a,(de)               ;2
        add a                   ;1
        ld l,a                  ;1
        jp (hl)                 ;1

;### CELPLT -> plots cell field
;### Input      (celard)=field, (celgfx)=bitmap, celxln,celyln=size with border
celplta db #00,#05, #30,#35, #c0,#c5, #f0,#f5   ;cpc encoded
celpltb db #00,#22, #05,#27, #50,#72, #55,#77   ;msx encoded

celplt  ld a,(celgfx)
        rla
        ld bc,celplta
        jr nc,celplt1
        ld bc,celpltb
celplt1 ld ix,celgfx1
        ld de,celxln
        ;add ix,de
        ld hl,(celard)
        ld e,celxln
        ;add hl,de
        ex de,hl
        ld iyl,celyln;-2
celplt2 ld iyh,celxln/2
celplt3 ld a,(de):inc de        ;4
        and 16                  ;2
        ld l,a                  ;1
        ld a,(de):inc de        ;4
        and 16                  ;2
        rrca                    ;1
        or l                    ;1
        rrca:rrca               ;2
        ld l,a                  ;1
        ld h,0                  ;2
        add hl,bc               ;3
        ld a,(hl):ld (ix+0),a   ;7
        inc hl                  ;2
        ld a,(hl):ld (ix+32),a  ;7
        inc ix                  ;3
        dec iyh                 ;2
        jr nz,celplt3           ;3 47 -> 93248=4,6fr
        ex de,hl
        ld de,celxln/2
        add ix,de
        ex de,hl
        dec iyl
        jr nz,celplt2
        ret

;### CELTRS -> mirrows cells at the border, if "torus" is activated
;### Input      (celard)=field
;### Output     (celard)=updated field
celtrs  ld ix,(celard)      ;** vertical
        ld de,celxln+32
        add ix,de           ;ix=middle of line (+32)
        ld e,celxln
        ld b,celyln-2
celtrs1 bit 4,(ix-32+1)     ;cell on left?
        jr z,celtrs2
        set 4,(ix+32-1)     ;add new cell right
        inc (ix+32-66)
        inc (ix+32-02)
        inc (ix+32+62)
celtrs2 bit 4,(ix+32-2)     ;cell on right?
        jr z,celtrs3
        set 4,(ix-32+0)     ;add new cell left
        inc (ix-32-63)
        inc (ix-32+01)
        inc (ix-32+65)
celtrs3 add ix,de
        djnz celtrs1

        ld ix,(celard)      ;** horizontal
        ld iy,(celard)
        ld de,celxln*celyln-celxln
        add iy,de           ;ix=top border, iy=bottom border
        push iy
        ld b,celxln-2
celtrs4 bit 4,(ix+65)       ;cell on top?
        jr z,celtrs5
        set 4,(iy+1)        ;add new bottom cell
        inc (iy-64)
        inc (iy-63)
        inc (iy-62)
celtrs5 bit 4,(iy-63)       ;cell on bottom?
        jr z,celtrs6
        set 4,(ix+1)        ;add new top cell
        inc (ix+64)
        inc (ix+65)
        inc (ix+66)
celtrs6 inc ix
        inc iy
        djnz celtrs4

        ld ix,(celard)      ;** edges
        pop iy
        bit 4,(ix+64+1)     ;up-left
        jr z,celtrs7
        set 4,(iy+63)       ;   -> add down-right
        inc (iy-1-1)
celtrs7 bit 4,(ix+64+62)    ;up-right
        jr z,celtrs8
        set 4,(iy+0)        ;   -> add down-left
        inc (iy-1-62)
celtrs8 bit 4,(iy-1-1)      ;down-right
        jr z,celtrs9
        set 4,(ix+0)        ;   -> add up-left
        inc (ix+64+1)
celtrs9 bit 4,(iy-1-62)     ;down-left
        ret z
        set 4,(ix+63)       ;   -> add up-right
        inc (ix+64+62)
        ret

;### CELTRC -> clears cells at the border, if "torus" is activated
;### Input      (celawr)=field
;### Output     (celawr)=updated field
celtrc  ld ix,(celawr)      ;** vertical
        ld de,celxln+32
        add ix,de           ;ix=middle of line (+32)
        ld e,celxln
        ld b,celyln-2
celtrc1 bit 4,(ix+32-1)     ;cell on right?
        jr z,celtrc2
        res 4,(ix+32-1)     ;remove
        dec (ix+32-66)
        dec (ix+32-02)
        dec (ix+32+62)
celtrc2 bit 4,(ix-32+0)     ;cell on left?
        jr z,celtrc3
        res 4,(ix-32+0)     ;remove
        dec (ix-32-63)
        dec (ix-32+01)
        dec (ix-32+65)
celtrc3 add ix,de
        djnz celtrc1

        ld ix,(celawr)      ;** horizontal
        ld iy,(celawr)
        ld de,celxln*celyln-celxln
        add iy,de           ;ix=top border, iy=bottom border
        push iy
        ld b,celxln-2
celtrc4 bit 4,(iy+1)        ;cell on bottom?
        jr z,celtrc5
        res 4,(iy+1)        ;remove
        dec (iy-64)
        dec (iy-63)
        dec (iy-62)
celtrc5 bit 4,(ix+1)        ;cell on top?
        jr z,celtrc6
        res 4,(ix+1)        ;remove
        dec (ix+64)
        dec (ix+65)
        dec (ix+66)
celtrc6 inc ix
        inc iy
        djnz celtrc4

        ld ix,(celawr)      ;** edges
        pop iy
        bit 4,(iy+63)       ;down-right
        jr z,celtrc7
        res 4,(iy+63)       ;   -> remove
        dec (iy-1-1)
celtrc7 bit 4,(iy+0)        ;down-left
        jr z,celtrc8
        res 4,(iy+0)        ;   -> remove 
        dec (iy-1-62)
celtrc8 bit 4,(ix+0)        ;up-left
        jr z,celtrc9
        res 4,(ix+0)        ;   -> remove
        dec (ix+64+1)
celtrc9 bit 4,(ix+63)       ;up-right
        ret z
        res 4,(ix+63)       ;   -> remove
        dec (ix+64+62)
        ret

;### CELBIN -> converts cell field into binary data
;### Input      (celard)=field
;### Output     (celabf)=binary
celbin  ld hl,(celard)
        ld ix,(celabf)
        ld de,celxln*celyln/8
celbin1 ld b,8
celbin2 bit 4,(hl)
        inc hl
        scf
        jr nz,celbin3
        ccf
celbin3 rla
        djnz celbin2
        ld (ix+0),a
        inc ix
        dec de
        ld a,e:or d
        jr nz,celbin1
        ret

;### CELREC -> recalculate neighbours and alive count (torus ignored)
;### Input      (celard)=field
;### Output     (celard)=updated field, (celclv)=number of alive cells
celrec  ld h,16
        ld ix,(celard)

        ld a,(ix+65)                ;** edge up-left
        and h:rrca:rrca:rrca:rrca
        ld (ix+0),a
        ld a,(ix+126)               ;** edge up-right
        and h:rrca:rrca:rrca:rrca
        ld (ix+63),a

        ld de,celxln                ;** center lines
        add ix,de
        ld e,0
        ld bc,celxln*celyln-celxln-celxln
celrec1 ld a,(ix-65):and h:      ld l,a     ;7
        ld a,(ix-64):and h:add l:ld l,a     ;8
        ld a,(ix-63):and h:add l:ld l,a     ;8
        ld a,(ix- 1):and h:add l:ld l,a     ;8
        ld a,(ix+ 1):and h:add l:ld l,a     ;8
        ld a,(ix+63):and h:add l:ld l,a     ;8
        ld a,(ix+64):and h:add l:ld l,a     ;8
        ld a,(ix+65):and h:add l            ;7
        rrca:rrca:rrca:rrca     :ld l,a     ;5
        ld a,(ix+0)                         ;5
        and h                               ;1
        jr z,celrec2                        ;3/2
        inc de                              ;0/2
celrec2 add l                               ;1
        ld (ix+0),a                         ;5
        inc ix                              ;3
        dec bc
        ld a,c:or b
        jr nz,celrec1
        ld (celclv),de

        ld a,(ix-63)                ;** edge down-left
        and h:rrca:rrca:rrca:rrca
        ld (ix+0),a
        ld a,(ix-2)                 ;** edge down-right
        and h:rrca:rrca:rrca:rrca
        ld (ix+63),a

        ld a,-65                    ;** last and first line
        call celrec3
        ld ix,(celard)
        ld a,63

celrec3 inc ix
        ld (celreca+2),a:inc a
        ld (celrecb+2),a:inc a
        ld (celrecc+2),a
        ld b,celxln-2
celreca ld a,(ix+63):and h:      ld l,a
celrecb ld a,(ix+64):and h:add l:ld l,a
celrecc ld a,(ix+65):and h:add l
        rrca:rrca:rrca:rrca
        ld (ix+0),a
        inc ix
        djnz celreca
        ret


;==============================================================================
;### CELL FIELD ###############################################################
;==============================================================================

celxln equ 64
celyln equ 64

celfld  db 0    ;buffer for temporary field
;!!!last label!!!

;==============================================================================
;### D A T A   A R E A ########################################################
;==============================================================================

prgdatbeg

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #FF,#F3,#FF,#F3,#FF,#F3,#FF,#F3,#FF,#F3,#FF,#F3,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#33,#33,#33,#33,#33,#33,#33,#33,#33,#33,#33,#33
db #FF,#F3,#AA,#A9,#AA,#A9,#FF,#F3,#FF,#F3,#FF,#F3,#F3,#33,#A9,#99,#A9,#99,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#A9,#99,#A9,#99,#F3,#33,#F3,#33,#F3,#33,#33,#33,#99,#99,#99,#99,#33,#33,#33,#33,#33,#33
db #FF,#F3,#AA,#A9,#FF,#F3,#AA,#A9,#FF,#F3,#FF,#F3,#F3,#33,#A9,#99,#F3,#33,#A9,#99,#F3,#33,#F3,#33,#F3,#33,#A9,#99,#F3,#33,#A9,#99,#F3,#33,#F3,#33,#33,#33,#99,#99,#33,#33,#99,#99,#33,#33,#33,#33
db #FF,#F3,#AA,#A9,#FF,#F3,#FF,#F3,#FF,#F3,#AA,#A9,#F3,#33,#A9,#99,#F3,#33,#F3,#33,#F3,#33,#A9,#99,#F3,#33,#A9,#99,#F3,#33,#F3,#33,#F3,#33,#A9,#99,#33,#33,#99,#99,#33,#33,#33,#33,#33,#33,#99,#99
db #FF,#F3,#FF,#F3,#FF,#F3,#AA,#A9,#AA,#A9,#FF,#F3,#F3,#33,#F3,#33,#F3,#33,#A9,#99,#A9,#99,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#A9,#99,#A9,#99,#F3,#33,#33,#33,#33,#33,#33,#33,#99,#99,#99,#99,#33,#33
db #FF,#F3,#FF,#F3,#FF,#F3,#FF,#F3,#FF,#F3,#FF,#F3,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#F3,#33,#33,#33,#33,#33,#33,#33,#33,#33,#33,#33,#33,#33

;### Menu-Texts
prgwinmentx1    db "File",0
prgwinmen1tx1   db "Load cell field",0
prgwinmen1tx2   db "Save cell field",0
prgwinmen1tx3   db "Quit",0

prgwinmentx2    db "Presets",0
prgwinmen2tx1   db "Oscillators (1)",0
prgwinmen2tx2   db "Oscillators (2)",0
prgwinmen2tx3   db "Glider Guns",0
prgwinmen2tx4   db "Spaceships (1)",0
prgwinmen2tx5   db "Spaceships (2)",0
prgwinmen2tx6   db "Spaceships (3)",0
prgwinmen2tx7   db "1357 Copy World",0

prgwinmentx3    db "?",0
prgwinmen3tx1   db "Index",0
prgwinmen3tx2   db "About Game of Life",0


;### Form-Texts
prgwintit   db "Conway's Game of Life",0

txtbutrun   db "Start!",0
txtbutstp   db "Stop",0
txtbutrnd   db "Random",0
txtbutclr   db "Clear",0

txtrndval   db "xxxx",0
txtgentit   db "Generation",0
txtgenval   db "00000",0
txtcl1tit   db "Alive",0
txtcl1val   db "9999 (999%)",0
txtcl0tit   db "Dead",0
txtcl0val   db "9999 (999%)",0

txttorflg   db "Torus",0
txtsetflg   db "Field edit mode",0
txtst0flg   db "set/reset cells",0
txtst1flg   db "insert figure",0

txtrultit   db "Neighbours",0
txtrulkep   db "stay alive",0
txtrulnew   db "new cell",0
txtrulnn0   db "0",0
txtrulnn1   db "1",0
txtrulnn2   db "2",0
txtrulnn3   db "3",0
txtrulnn4   db "4",0
txtrulnn5   db "5",0
txtrulnn6   db "6",0
txtrulnn7   db "7",0
txtrulnn8   db "8"
txtrulnnx   db 0

;### Figures
;read "App-GameOfLife.txt"
txtctlfig000 db "101",0
txtctlfig001 db "4-8-12 diamond",0
txtctlfig002 db "Achim's p144",0
txtctlfig003 db "Achim's p16",0
txtctlfig004 db "Achim's p4",0
txtctlfig005 db "A for all",0
txtctlfig006 db "airforce",0
txtctlfig007 db "almosymmetric",0
txtctlfig008 db "ants",0
txtctlfig009 db "aVerage",0
txtctlfig010 db "B29",0
txtctlfig011 db "B-52 bomber",0
txtctlfig012 db "backrake",0
txtctlfig013 db "baker",0
txtctlfig014 db "baker's dozen",0
txtctlfig015 db "beacon maker",0
txtctlfig016 db "bent keys",0
txtctlfig017 db "big glider",0
txtctlfig018 db "bi-loaf",0
txtctlfig019 db "bi-loaf",0
txtctlfig020 db "bipole",0
txtctlfig021 db "biting off more than they can chew",0
txtctlfig022 db "blinker puffer1",0
txtctlfig023 db "blinker puffer2",0
txtctlfig024 db "blinker ship",0
txtctlfig025 db "boat maker",0
txtctlfig026 db "boojum reflector",0
txtctlfig027 db "boss",0
txtctlfig028 db "bottle",0
txtctlfig029 db "brain",0
txtctlfig030 db "buckaroo",0
txtctlfig031 db "bullet heptomino",0
txtctlfig032 db "bun",0
txtctlfig033 db "butterfly",0
txtctlfig034 db "Canada goose",0
txtctlfig035 db "candlefrobra",0
txtctlfig036 db "candlefrobra",0
txtctlfig037 db "cap",0
txtctlfig038 db "carnival shuttle",0
txtctlfig039 db "Caterpillar",0
txtctlfig040 db "cauldron",0
txtctlfig041 db "centinal",0
txtctlfig042 db "chemist",0
txtctlfig043 db "chicken wire",0
txtctlfig044 db "chicken wire",0
txtctlfig045 db "clock II",0
txtctlfig046 db "Coe ship",0
txtctlfig047 db "Coe's p8",0
txtctlfig048 db "confused eaters",0
txtctlfig049 db "converter",0
txtctlfig050 db "cow",0
txtctlfig051 db "crane",0
txtctlfig052 db "cross",0
txtctlfig053 db "cross",0
txtctlfig054 db "crown",0
txtctlfig055 db "cuphook",0
txtctlfig056 db "cuphook",0
txtctlfig057 db "dart",0
txtctlfig058 db "diamond ring",0
txtctlfig059 db "diehard",0
txtctlfig060 db "dinner table",0
txtctlfig061 db "diuresis",0
txtctlfig062 db "do-see-do",0
txtctlfig063 db "double caterer",0
txtctlfig064 db "double ewe",0
txtctlfig065 db "dragon",0
txtctlfig066 db "eater",0
txtctlfig067 db "eater2",0
txtctlfig068 db "eater2",0
txtctlfig069 db "eater3",0
txtctlfig070 db "eater4",0
txtctlfig071 db "ecologist",0
txtctlfig072 db "edge-repair spaceship",0
txtctlfig073 db "edge-repair spaceship",0
txtctlfig074 db "edge-repair spaceship",0
txtctlfig075 db "edge shooter",0
txtctlfig076 db "electric fence",0
txtctlfig077 db "en retard",0
txtctlfig078 db "Enterprise",0
txtctlfig079 db "factory",0
txtctlfig080 db "Fast Forward Force Field",0
txtctlfig081 db "figure-8",0
txtctlfig082 db "filter",0
txtctlfig083 db "flotilla",0
txtctlfig084 db "fly",0
txtctlfig085 db "fountain",0
txtctlfig086 db "frothing puffer",0
txtctlfig087 db "fumarole",0
txtctlfig088 db "Gabriel's p138",0
txtctlfig089 db "glasses",0
txtctlfig090 db "glider",0
txtctlfig091 db "glider-block cycle",0
txtctlfig092 db "glider duplicator",0
txtctlfig093 db "gliderless",0
txtctlfig094 db "glider pusher",0
txtctlfig095 db "gliders by the dozen",0
txtctlfig096 db "glider synthesis",0
txtctlfig097 db "glider turner",0
txtctlfig098 db "Gosper glider gun",0
txtctlfig099 db "gourmet",0
txtctlfig100 db "grow-by-one object",0
txtctlfig101 db "hammer",0
txtctlfig102 db "hammerhead",0
txtctlfig103 db "handshake",0
txtctlfig104 db "harbor",0
txtctlfig105 db "harvester",0
txtctlfig106 db "hebdarole",0
txtctlfig107 db "hectic",0
txtctlfig108 db "helix",0
txtctlfig109 db "hivenudger",0
txtctlfig110 db "hivenudger",0
txtctlfig111 db "house",0
txtctlfig112 db "HW emulator",0
txtctlfig113 db "HWSS",0
txtctlfig114 db "HW volcano",0
txtctlfig115 db "infinite growth",0
txtctlfig116 db "infinite growth",0
txtctlfig117 db "infinite growth",0
txtctlfig118 db "inline inverter",0
txtctlfig119 db "jack",0
txtctlfig120 db "Jolson",0
txtctlfig121 db "kickback reaction",0
txtctlfig122 db "killer toads",0
txtctlfig123 db "Kok's galaxy",0
txtctlfig124 db "light bulb",0
txtctlfig125 db "light bulb",0
txtctlfig126 db "lightspeed wire",0
txtctlfig127 db "loaflipflop",0
txtctlfig128 db "LWSS",0
txtctlfig129 db "metamorphosis",0
txtctlfig130 db "metamorphosis II",0
txtctlfig131 db "MWSS",0
txtctlfig132 db "MW volcano",0
txtctlfig133 db "new gun",0
txtctlfig134 db "non-monotonic",0
txtctlfig135 db "octagon II",0
txtctlfig136 db "octagon IV",0
txtctlfig137 db "Orion",0
txtctlfig138 db "Orion",0
txtctlfig139 db "p54 shuttle",0
txtctlfig140 db "p6 shuttle",0
txtctlfig141 db "p6 shuttle",0
txtctlfig142 db "phase change",0
txtctlfig143 db "phoenix",0
txtctlfig144 db "pi-heptomino",0
txtctlfig145 db "pinwheel",0
txtctlfig146 db "pi orbital",0
txtctlfig147 db "pi portraitor",0
txtctlfig148 db "popover",0
txtctlfig149 db "pre-pulsar",0
txtctlfig150 db "pseudo-barberpole",0
txtctlfig151 db "puffer",0
txtctlfig152 db "puff suppressor",0
txtctlfig153 db "pulsar",0
txtctlfig154 db "pulsar quadrant",0
txtctlfig155 db "pulse divider",0
txtctlfig156 db "pulshuttle V",0
txtctlfig157 db "pure glider generator",0
txtctlfig158 db "pushalong",0
txtctlfig159 db "quarter",0
txtctlfig160 db "quasar",0
txtctlfig161 db "queen bee shuttle",0
txtctlfig162 db "reflector",0
txtctlfig163 db "relay",0
txtctlfig164 db "rephaser",0
txtctlfig165 db "ring of fire",0
txtctlfig166 db "roteightor",0
txtctlfig167 db "rumbling river",0
txtctlfig168 db "sailboat",0
txtctlfig169 db "Schick engine",0
txtctlfig170 db "seal",0
txtctlfig171 db "ship in a bottle",0
txtctlfig172 db "short keys",0
txtctlfig173 db "sidecar",0
txtctlfig174 db "sixty-nine",0
txtctlfig175 db "skewed traffic light",0
txtctlfig176 db "smiley",0
txtctlfig177 db "snacker",0
txtctlfig178 db "snacker",0
txtctlfig179 db "snail",0
txtctlfig180 db "spacefiller",0
txtctlfig181 db "space rake",0
txtctlfig182 db "spaceship",0
txtctlfig183 db "Spaceship C5 diagonal",0
txtctlfig184 db "sparky",0
txtctlfig185 db "spider",0
txtctlfig186 db "stairstep hexomino",0
txtctlfig187 db "star",0
txtctlfig188 db "still life tagalong",0
txtctlfig189 db "superfountain",0
txtctlfig190 db "superstring",0
txtctlfig191 db "swan",0
txtctlfig192 db "thunderbird",0
txtctlfig193 db "T-nosed p4",0
txtctlfig194 db "T-nosed p6",0
txtctlfig195 db "toad-sucker",0
txtctlfig196 db "toaster",0
txtctlfig197 db "total aperiodic",0
txtctlfig198 db "tractor beam",0
txtctlfig199 db "traffic circle",0
txtctlfig200 db "triple caterer",0
txtctlfig201 db "true",0
txtctlfig202 db "tubeater",0
txtctlfig203 db "tubstretcher",0
txtctlfig204 db "tumbler",0
txtctlfig205 db "tumbling T-tetson",0
txtctlfig206 db "turning toads",0
txtctlfig207 db "turtle",0
txtctlfig208 db "twin bees shuttle",0
txtctlfig209 db "twirling T-tetsons II",0
txtctlfig210 db "washerwoman",0
txtctlfig211 db "wasp",0
txtctlfig212 db "weekender",0
txtctlfig213 db "wickstretcher",0
txtctlfig214 db "windmill",0
txtctlfig215 db "worker bee",0
txtctlfig216 db "x66",0
txtctlfig217 db "Z-pentomino",0

;### About
prgmsginf1 db "Conway's Game of Life",0
prgmsginf2 db " Version 1.2 (Build "
read "..\..\..\SRC-Main\build.asm"
            db "pdt)",0
prgmsginf3 db " Copyright <c> 2025 SymbiosiS",0


celgfx  db celxln/2, celxln*2, celyln*2
celgfx1 db 0
;!!!last label!!!


;==============================================================================
;### T R A N S F E R   A R E A ################################################
;==============================================================================

prgtrnbeg

;### PROCESS STRUCTURES #######################################################

;### Main process
        ds 128
prgstk  ds 6*2
        dw prgprz
prgprzn db 0
prgmsgb ds 14

;### Cell calculation process
        ds 128
celstk  ds 6*2
        dw celprz
celprzn db 0
celmsgb ds 14

;config
celcfgbeg
celold  db 0,0,1,1,0,0,0,0,0,0
celnew  db 0,0,0,1,0,0,0,0,0,0
celtor  db 1
celcfgend

;### File handling
filmsk  db "gol",0
filpth  ds 256

;### INFO WINDOW ##############################################################

prgmsginf  dw prgmsginf1,4*1+2,prgmsginf2,4*1+2,prgmsginf3,4*1+2,prgicnbig

;### MAIN WINDOW ##############################################################

prgwindat dw #3501,0,101,04,celxln*2+77,celyln*2+29,0,0,celxln*2+77,celyln*2+29,1,1,1000,1000,prgicnsml,prgwintit,0,prgwinmen,prgwingrp,0,0:ds 136+14

prgwinmen  dw 3, 1+4,prgwinmentx1,prgwinmen1,0, 1+4,prgwinmentx2,prgwinmen2,0, 1+4,prgwinmentx3,prgwinmen3,0
prgwinmen1 dw 4, 1,prgwinmen1tx1,fillod,0, 1,prgwinmen1tx2,filsav,0, 1+8,0,0,0, 1,prgwinmen1tx3,prgend,0
prgwinmen2 dw 10, 1,prgwinmen2tx1,filpsa,0, 1,prgwinmen2tx2,filpsb,0, 1+8,0,0,0, 1,prgwinmen2tx3,filpsc,0, 1+8,0,0,0, 1,prgwinmen2tx4,filpsd,0, 1,prgwinmen2tx5,filpse,0, 1,prgwinmen2tx6,filpsf,0, 1+8,0,0,0, 1,prgwinmen2tx7,filpsg,0
prgwinmen3 dw 3, 1,prgwinmen3tx1,hlpopn,0, 1+8,0,0,0, 1,prgwinmen3tx2,prginf,0

prgwingrp db 50,0:dw prgwinobj,0,0,256*0+0,0,0,0

prgwinobj
dw     00,255*256+00,2         ,0,0,10000,10000,0               ;00=background
dw     00,255*256+02,3*4+3     ,1,1,celxln*2+2,celyln*2+2,0     ;01=cell field border
dw     00,255*256+00,1         ,celxln*2+4,0,1,10000,0          ;02=seperator line
dw ctledt,255*256+08,celgfx    ,2,2,celxln*2,celyln*2,0         ;03=cell field

dw     00,255*256+1 ,ctlobjdat3,celxln*2+6,0,70, 8,0            ;04=Generation text
dw     00,255*256+1 ,ctlobjdat5,celxln*2+6,16,70, 8,0           ;05=Alive      text
dw     00,255*256+1 ,ctlobjdat7,celxln*2+6,32,70, 8,0           ;06=Dead       text
dw     00,255*256+1 ,ctlobjdat4,celxln*2+16,8,60, 8,0           ;07=Generation value
dw     00,255*256+1 ,ctlobjdat6,celxln*2+16,24,60, 8,0          ;08=Alive      value
dw     00,255*256+1 ,ctlobjdat8,celxln*2+16,40,60, 8,0          ;09=Dead       value

dw     00,255*256+17,ctlobjdatc,celxln*2+6,53,70,8,0            ;10=torus
dw     00,255*256+1 ,ctlobjdatf,celxln*2+6,65,70,8,0            ;11=Edit mode
dw     00,255*256+18,ctlobjdatd,celxln*2+6,73,70,8,0            ;12=cell add/remove
dw     00,255*256+18,ctlobjdate,celxln*2+6,81,70,8,0            ;13=figure add
dw ctlfgc,255*256+42,ctlfigobj,celxln*2+6,89,70,10,0            ;14=figure selection

dw ctlrnv,255*256+24,ctlobjdat1,celxln*2+24,105,52,8,0          ;15=Slider Random
dw     00,255*256+1 ,ctlobjdat2,celxln*2+6,105,17, 8,0          ;16=value  Random
dw ctlrnd,255*256+16,txtbutrnd ,celxln*2+6,114,70,12,0          ;17=Button "Random"
dw ctlclr,255*256+16,txtbutclr ,celxln*2+6,129,70,12,0          ;18=Button "Clear"
dw ctlrun,255*256+16
prgwinobj1        dw txtbutrun ,celxln*2+6,144,70,12,0          ;19=Button "Start/Pause"

dw     00,255*256+1 ,ctlobjdat9,1,celyln*2+4,47,8, 0            ;20=Neighbours
dw     00,255*256+1 ,ctlobjdata,1,celyln*2+12,47,8, 0           ;21=Keep alive
dw     00,255*256+1 ,ctlobjdatb,1,celyln*2+20,47,8, 0           ;22=New cell

dw     00,255*256+1 ,ctlobjdri0,53,celyln*2+4,8,8, 0            ;23="0"
dw     00,255*256+1 ,ctlobjdri1,62,celyln*2+4,8,8, 0            ;24="1"
dw     00,255*256+1 ,ctlobjdri2,71,celyln*2+4,8,8, 0            ;25="2"
dw     00,255*256+1 ,ctlobjdri3,80,celyln*2+4,8,8, 0            ;26="3"
dw     00,255*256+1 ,ctlobjdri4,89,celyln*2+4,8,8, 0            ;27="4"
dw     00,255*256+1 ,ctlobjdri5,98,celyln*2+4,8,8, 0            ;28="5"
dw     00,255*256+1 ,ctlobjdri6,107,celyln*2+4,8,8, 0           ;29="6"
dw     00,255*256+1 ,ctlobjdri7,116,celyln*2+4,8,8, 0           ;30="7"
dw     00,255*256+1 ,ctlobjdri8,125,celyln*2+4,8,8, 0           ;31="8"

dw celrul,255*256+17,ctlobjdrk0,52,celyln*2+12,8,8,0            ;32=keep 0
dw celrul,255*256+17,ctlobjdrk1,61,celyln*2+12,8,8,0            ;33=keep 1
dw celrul,255*256+17,ctlobjdrk2,70,celyln*2+12,8,8,0            ;34=keep 2
dw celrul,255*256+17,ctlobjdrk3,79,celyln*2+12,8,8,0            ;35=keep 3
dw celrul,255*256+17,ctlobjdrk4,88,celyln*2+12,8,8,0            ;36=keep 4
dw celrul,255*256+17,ctlobjdrk5,97,celyln*2+12,8,8,0            ;37=keep 5
dw celrul,255*256+17,ctlobjdrk6,106,celyln*2+12,8,8,0           ;38=keep 6
dw celrul,255*256+17,ctlobjdrk7,115,celyln*2+12,8,8,0           ;39=keep 7
dw celrul,255*256+17,ctlobjdrk8,124,celyln*2+12,8,8,0           ;40=keep 8

dw celrul,255*256+17,ctlobjdrn0,52,celyln*2+20,8,8,0            ;41=new 0
dw celrul,255*256+17,ctlobjdrn1,61,celyln*2+20,8,8,0            ;42=new 1
dw celrul,255*256+17,ctlobjdrn2,70,celyln*2+20,8,8,0            ;43=new 2
dw celrul,255*256+17,ctlobjdrn3,79,celyln*2+20,8,8,0            ;44=new 3
dw celrul,255*256+17,ctlobjdrn4,88,celyln*2+20,8,8,0            ;45=new 4
dw celrul,255*256+17,ctlobjdrn5,97,celyln*2+20,8,8,0            ;46=new 5
dw celrul,255*256+17,ctlobjdrn6,106,celyln*2+20,8,8,0           ;47=new 6
dw celrul,255*256+17,ctlobjdrn7,115,celyln*2+20,8,8,0           ;48=new 7
dw celrul,255*256+17,ctlobjdrn8,124,celyln*2+20,8,8,0           ;49=new 8

ctlobjdat1 dw 1, 50, 100, 256*255+1
ctlobjdat2 dw txtrndval:db 3+0+128,2
ctlobjdat3 dw txtgentit:db 2+4,0
ctlobjdat4 dw txtgenval:db 3+0+128,1
ctlobjdat5 dw txtcl1tit:db 2+4,0
ctlobjdat6 dw txtcl1val:db 3+0+128,1
ctlobjdat7 dw txtcl0tit:db 2+4,0
ctlobjdat8 dw txtcl0val:db 3+0+128,1
ctlobjdat9 dw txtrultit:db 2+4,1
ctlobjdata dw txtrulkep:db 2+4,1
ctlobjdatb dw txtrulnew:db 2+4,1
ctlobjdatc dw celtor,txttorflg,2+4
ctlobjdatd dw ctlsetflg,txtst0flg,2+4+000,ctlsetcoo
ctlobjdate dw ctlsetflg,txtst1flg,2+4+256,ctlsetcoo
ctlobjdatf dw txtsetflg:db 2+4,0

ctlsetcoo   ds 4
ctlsetflg   db 0

ctlfigobj   dw 218,00,ctlfiglst,0,1,ctlfigrow,0,1
ctlfigrow   dw 0,70,0,0
ctlfiglst
;read "App-GameOfLife.obj"
dw 000,txtctlfig000,001,txtctlfig001,002,txtctlfig002,003,txtctlfig003,004,txtctlfig004,005,txtctlfig005,006,txtctlfig006,007,txtctlfig007
dw 008,txtctlfig008,009,txtctlfig009,010,txtctlfig010,011,txtctlfig011,012,txtctlfig012,013,txtctlfig013,014,txtctlfig014,015,txtctlfig015
dw 016,txtctlfig016,017,txtctlfig017,018,txtctlfig018,019,txtctlfig019,020,txtctlfig020,021,txtctlfig021,022,txtctlfig022,023,txtctlfig023
dw 024,txtctlfig024,025,txtctlfig025,026,txtctlfig026,027,txtctlfig027,028,txtctlfig028,029,txtctlfig029,030,txtctlfig030,031,txtctlfig031
dw 032,txtctlfig032,033,txtctlfig033,034,txtctlfig034,035,txtctlfig035,036,txtctlfig036,037,txtctlfig037,038,txtctlfig038,039,txtctlfig039
dw 040,txtctlfig040,041,txtctlfig041,042,txtctlfig042,043,txtctlfig043,044,txtctlfig044,045,txtctlfig045,046,txtctlfig046,047,txtctlfig047
dw 048,txtctlfig048,049,txtctlfig049,050,txtctlfig050,051,txtctlfig051,052,txtctlfig052,053,txtctlfig053,054,txtctlfig054,055,txtctlfig055
dw 056,txtctlfig056,057,txtctlfig057,058,txtctlfig058,059,txtctlfig059,060,txtctlfig060,061,txtctlfig061,062,txtctlfig062,063,txtctlfig063
dw 064,txtctlfig064,065,txtctlfig065,066,txtctlfig066,067,txtctlfig067,068,txtctlfig068,069,txtctlfig069,070,txtctlfig070,071,txtctlfig071
dw 072,txtctlfig072,073,txtctlfig073,074,txtctlfig074,075,txtctlfig075,076,txtctlfig076,077,txtctlfig077,078,txtctlfig078,079,txtctlfig079
dw 080,txtctlfig080,081,txtctlfig081,082,txtctlfig082,083,txtctlfig083,084,txtctlfig084,085,txtctlfig085,086,txtctlfig086,087,txtctlfig087
dw 088,txtctlfig088,089,txtctlfig089,090,txtctlfig090,091,txtctlfig091,092,txtctlfig092,093,txtctlfig093,094,txtctlfig094,095,txtctlfig095
dw 096,txtctlfig096,097,txtctlfig097,098,txtctlfig098,099,txtctlfig099,100,txtctlfig100,101,txtctlfig101,102,txtctlfig102,103,txtctlfig103
dw 104,txtctlfig104,105,txtctlfig105,106,txtctlfig106,107,txtctlfig107,108,txtctlfig108,109,txtctlfig109,110,txtctlfig110,111,txtctlfig111
dw 112,txtctlfig112,113,txtctlfig113,114,txtctlfig114,115,txtctlfig115,116,txtctlfig116,117,txtctlfig117,118,txtctlfig118,119,txtctlfig119
dw 120,txtctlfig120,121,txtctlfig121,122,txtctlfig122,123,txtctlfig123,124,txtctlfig124,125,txtctlfig125,126,txtctlfig126,127,txtctlfig127
dw 128,txtctlfig128,129,txtctlfig129,130,txtctlfig130,131,txtctlfig131,132,txtctlfig132,133,txtctlfig133,134,txtctlfig134,135,txtctlfig135
dw 136,txtctlfig136,137,txtctlfig137,138,txtctlfig138,139,txtctlfig139,140,txtctlfig140,141,txtctlfig141,142,txtctlfig142,143,txtctlfig143
dw 144,txtctlfig144,145,txtctlfig145,146,txtctlfig146,147,txtctlfig147,148,txtctlfig148,149,txtctlfig149,150,txtctlfig150,151,txtctlfig151
dw 152,txtctlfig152,153,txtctlfig153,154,txtctlfig154,155,txtctlfig155,156,txtctlfig156,157,txtctlfig157,158,txtctlfig158,159,txtctlfig159
dw 160,txtctlfig160,161,txtctlfig161,162,txtctlfig162,163,txtctlfig163,164,txtctlfig164,165,txtctlfig165,166,txtctlfig166,167,txtctlfig167
dw 168,txtctlfig168,169,txtctlfig169,170,txtctlfig170,171,txtctlfig171,172,txtctlfig172,173,txtctlfig173,174,txtctlfig174,175,txtctlfig175
dw 176,txtctlfig176,177,txtctlfig177,178,txtctlfig178,179,txtctlfig179,180,txtctlfig180,181,txtctlfig181,182,txtctlfig182,183,txtctlfig183
dw 184,txtctlfig184,185,txtctlfig185,186,txtctlfig186,187,txtctlfig187,188,txtctlfig188,189,txtctlfig189,190,txtctlfig190,191,txtctlfig191
dw 192,txtctlfig192,193,txtctlfig193,194,txtctlfig194,195,txtctlfig195,196,txtctlfig196,197,txtctlfig197,198,txtctlfig198,199,txtctlfig199
dw 200,txtctlfig200,201,txtctlfig201,202,txtctlfig202,203,txtctlfig203,204,txtctlfig204,205,txtctlfig205,206,txtctlfig206,207,txtctlfig207
dw 208,txtctlfig208,209,txtctlfig209,210,txtctlfig210,211,txtctlfig211,212,txtctlfig212,213,txtctlfig213,214,txtctlfig214,215,txtctlfig215
dw 216,txtctlfig216,217,txtctlfig217

ctlobjdri0 dw txtrulnn0:db 2+4,0
ctlobjdri1 dw txtrulnn1:db 2+4,0
ctlobjdri2 dw txtrulnn2:db 2+4,0
ctlobjdri3 dw txtrulnn3:db 2+4,0
ctlobjdri4 dw txtrulnn4:db 2+4,0
ctlobjdri5 dw txtrulnn5:db 2+4,0
ctlobjdri6 dw txtrulnn6:db 2+4,0
ctlobjdri7 dw txtrulnn7:db 2+4,0
ctlobjdri8 dw txtrulnn8:db 2+4,0

ctlobjdrk0 dw celold+0,txtrulnnx,2+4
ctlobjdrk1 dw celold+1,txtrulnnx,2+4
ctlobjdrk2 dw celold+2,txtrulnnx,2+4
ctlobjdrk3 dw celold+3,txtrulnnx,2+4
ctlobjdrk4 dw celold+4,txtrulnnx,2+4
ctlobjdrk5 dw celold+5,txtrulnnx,2+4
ctlobjdrk6 dw celold+6,txtrulnnx,2+4
ctlobjdrk7 dw celold+7,txtrulnnx,2+4
ctlobjdrk8 dw celold+8,txtrulnnx,2+4

ctlobjdrn0 dw celnew+0,txtrulnnx,2+4
ctlobjdrn1 dw celnew+1,txtrulnnx,2+4
ctlobjdrn2 dw celnew+2,txtrulnnx,2+4
ctlobjdrn3 dw celnew+3,txtrulnnx,2+4
ctlobjdrn4 dw celnew+4,txtrulnnx,2+4
ctlobjdrn5 dw celnew+5,txtrulnnx,2+4
ctlobjdrn6 dw celnew+6,txtrulnnx,2+4
ctlobjdrn7 dw celnew+7,txtrulnnx,2+4
ctlobjdrn8 dw celnew+8,txtrulnnx,2+4

prgtrnend

relocate_table
relocate_end
