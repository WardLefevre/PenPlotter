;Pen Plotter, geschreven door Ward Lefevre.
    
    
;INITIALISATIES
        org     00000h          ;startadres programma
        mov     sp,#07fh        ;stackpointer klaar zetten voor gebruik
        lcall   initsio         ;seriële poort klaar zetten
        lcall	initlcd         ;lcd scherm klaar zetten
        lcall   initleds        ;leds klaar zetten
        lcall   initftoetsen    ;functietoetsen klaar zetten
        lcall   initdipswitch   ;dipswitch klaar zetten
        mov     a,#03h          ;cursor niet zichtbaar maken
        lcall   lcdoutchar      ;cursor niet zichtbaar maken
        mov     a,#80h          ;zet de cursor vooraan de eerste lijn
        lcall   lcdoutchar      ;zet de cursor vooraan de eerste lijn
        ljmp    man             ;spring naar manuele modus


;STURING MOTOREN
xpos:   mov     b,#4            ;gebruik b als lusteller
        mov     dptr,#xpostab   ;startadres tabel in datapointer laden
xposlus:mov     a,#000h         ;nodig voor correct gebruik volgende instructie
        movc    a,@a+dptr       ;lees waarde uit tabel
        mov    	p3_data,a       ;schrijf naar de motoren
        lcall   delay2ms        ;vertraging nodig voor goede werking motoren
        lcall   delay1ms        ;vertraging nodig voor goede werking motoren
        inc     dptr            ;volgend adres
        djnz	b,xposlus       ;herhaal 8 keer
		ret                     ;ga terug

xneg:   mov     b,#4           
        mov     dptr,#xnegtab  
xneglus:mov     a,#000h       
        movc    a,@a+dptr     
        mov    	p3_data,a       
        lcall   delay2ms       
        lcall   delay1ms        
        inc     dptr            
        djnz	b,xneglus
		ret

ypos:   mov     b,#4
        mov     dptr,#ypostab
yposlus:mov     a,#000h
        movc    a,@a+dptr
        mov    	p3_data,a
        lcall   delay2ms       
        lcall   delay1ms     
        inc     dptr
        djnz	b,yposlus
		ret

yneg:   mov     b,#4
        mov     dptr,#ynegtab
yneglus:mov     a,#000h
        movc    a,@a+dptr
        mov    	p3_data,a
        lcall   delay2ms      
        lcall   delay1ms     
        inc     dptr
        djnz	b,yneglus
		ret


;MANUELE MODUS
man:    mov     dptr,#textman   ;startadres tabel in datapointer laden
        lcall   lcdoutmsga      ;start afdrukken bij adres in dptr, stoppen met 00h
manlus: jnb     p4_data.0,aut   ;dipswitch p4.0 =0?, dan naar automatische modus
        jb      p2_data.3,manxneg;functietoets p2.3 =0?, dan p2.2 testen
        acall   xpos            ;anders xpos uitvoeren
manxneg:jb      p2_data.2,manypos;functietoets p2.2 =0?, dan p2.1 testen
        acall   xneg            ;anders xneg uitvoeren
manypos:jb      p2_data.1,manyneg;functietoets p2.1 =0?, dan p2.0 testen
        acall   ypos            ;anders ypos uitvoeren
manyneg:jb      p2_data.0,manlus;functietoets p2.0 =0?, dan oneindige lus
        acall   yneg            ;anders yneg uitvoeren
        ljmp    manlus          ;oneindige lus


;AUTOMATISCHE MODUS
aut:    mov     dptr,#textaut   ;startadres tabel in datapointer laden
        lcall   lcdoutmsga      ;start afdrukken bij adres in dptr, stoppen met 00h

  ;we moeten eerst beginwaarden inlezen vooraleer we in de lus springen
eerst:  clr     psw.3           ;we gebruiken bank 0 voor de X waarden
        acall   testx           ;test voor 'STOP'(=ga terug naar manuele modus) of 'X'(=return)
        acall   lees            ;lees de X waarde in
        setb    psw.3           ;we gebruiken bank 1 voor de Y waarden
        acall   testy           ;test voor 'STOP'(=ga terug naar manuele modus) of 'Y'(=return)
        acall   lees            ;lees de Y waarde in

  ;lus die herhaald wordt tot de functie testx/testy een 'S' detecteert
lus:    clr     psw.3           ;we gebruiken bank 0 voor de x waarden
        acall   testx           ;test voor 'STOP'(=ga naar functie stop) of 'X'(=return)
        acall   lees            ;lees de x waarde in
        acall   schrijfx        ;stuur de motor aan
        setb    psw.3           ;we gebruiken bank 1 voor de y waarden
        acall   testy           ;test voor 'STOP'(=ga naar functie stop) of 'Y'(=return)
        acall   lees            ;lees de y waarde in
        acall   schrijfy        ;stuur de motor aan
        ljmp    lus             ;herhalen tot 'S' karakter

  ;functie die naar een 'S' of 'X' zoekt
testx:  lcall   sioinchar       ;karakter inlezen
        mov     r0,a            ;a backuppen, want wordt gebruikt voor volgende instructie
        xrl     a,#01010011b    ;exor ingelezen karakter met ASCII waarde 'S' (van 'STOP')
        jz      man             ;jump naar man als a=00000000b
        cjne    r0,#01011000b,testx;als waarde geen 'X' is dan opnieuw inlezen
        ret                     ;ga terug

  ;functie die naar een 'S' of 'Y' zoekt
testy:  lcall   sioinchar       
        mov     r0,a
        xrl     a,#01010011b    
        jz      man             
        cjne    r0,#01011001b,testy
        ret

  ;functie die getal inleest tot de komma en omzet in 2 complements voorstelling
lees:   clr     c               ;resetten voor later gebruik 
        clr     20h             ;resetten voor later gebruik
        mov     a,r1            ;plaats het vorige getal in r0
        mov     r0,a            ;plaats het vorige getal in r0
        lcall   sioinchar       ;lees eerste cijfer van nieuw getal in
        cjne    a,#00101101b,mask;als dit een '-' is, dan:
        setb    c               ;onthouden dat het een negatief getal is
        setb    20h             ;onthouden dat het een negatief getal is
        lcall   sioinchar       ;lees eerste cijfer van nieuw getal in
mask:   anl     a,#0fh          ;mask hoogste nibble (de 3 verwijderen, a=0000xxxx)
        mov     b,a             ;a backuppen, want wordt gebruikt voor volgende instructie
        lcall   sioinchar       ;volgend cijfer inlezen
        cjne    a,#00101110b,tiental;als dit geen '.' is, dan is het vorige cijfer een tiental
        mov     a,b             ;indien wel een '.', ééntal terug in a steken
        mov     c,20h           ;als het getal negatief is dan:
        jc      negatief        ;moet het 2 complement worden
        mov     r1,a            ;plaats het nieuwe getal in r1
        ret                     ;ga terug
tiental:anl     a,#0fh          ;mask hoogste nibble (de 3 verwijderen, a=0000xxxx)
        mov     r2,a            ;a backuppen, want wordt gebruikt voor volgende instructie
        mov     a,#00001010b    ;binaire tien in a steken
        mul     ab              ;tien maal b want b is een tiental
        add     a,r2            ;ééntal bij tiental optellen
        mov     c,20h           ;als het getal negatief is dan:
        jc      negatief        ;moet het 2 complement worden
        mov     r1,a            ;plaats het nieuwe getal in r1
        ret                     ;ga terug
negatief:cpl    a               ;eerst inverteren,
        add     a,#00000001b    ;dan plus 1 geeft ons 2 complement
        mov     r1,a            ;plaats het nieuwe getal in r1
        ret                     ;ga terug

  ;functie die het correct aantal y stappen zet
schrijfy:mov    a,r1            ;nieuw getal in accu
        subb    a,r0            ;nieuw getal - vorige getal
        mov     r3,a            ;slaag het verschil op in r3
        cjne    a,#00000000b,schrijfy1;als verschil 0 is, dan niet gaan schrijven
        ret                     ;ga terug
schrijfy1:anl   a,#10000000b    ;als de msb 1 is, is het verschil negatieef
        cjne    a,#10000000b,schrijfyp;positief schrijven als msb 0 is
schrijfyn:acall yneg            ;motor 2 keer aansturen, want de verhouding
        acall   yneg            ;yas-snelheid/xas-snelheid is 2/3
        inc     r3              ;het verschil (in 2 complement) incrementeren
        cjne    r3,#00000000b,schrijfyn;schrijven tot verschil 0 is
        ret                     ;ga terug
schrijfyp:acall ypos            ;motor 2 keer aansturen, want de verhouding
        acall   ypos            ;yas-snelheid/xas-snelheid is 2/3
        dec     r3              ;het verschil decrementeren
        cjne    r3,#00000000b,schrijfyp;schrijven tot verschil 0 is
        ret                     ;ga terug

  ;functie die het correct aantal x stappen zet
schrijfx:mov    a,r1
        subb    a,r0
        mov     r3,a
        cjne    a,#00000000b,schrijfx1
        ret
schrijfx1:anl   a,#10000000b
        cjne    a,#10000000b,schrijfxp
schrijfxn:acall xneg            
        acall   xneg            
        acall   xneg
        inc     r3
        cjne    r3,#00000000b,schrijfxn
        ret
schrijfxp:acall xpos
        acall   xpos
        acall   xpos
        dec     r3
        cjne    r3,#00000000b,schrijfxp
        ret

;TABELLEN
  ;text voor de interface op het lcd scherm
textman:db      84h,"PEN PLOTTER"
        db      94h,"   Manuele Modus    "
        db      d4h,"Xpos Xneg Ypos Yneg ",00h
textaut:db      84h,"PEN PLOTTER"
        db      94h," Automatische Modus "
        db      d4h," Lees uw G-Code in. ",00h

  ;aansturing motoren
xpostab:db	00010000b
       	db	00100000b
	    db	01000000b
        db  10000000b
xnegtab:db	10000000b
       	db	01000000b
	    db	00100000b
        db  00010000b
ypostab:db	00001000b
       	db	00000100b
	    db	00000010b
        db  00000001b
ynegtab:db	00000001b
	    db	00000010b
        db  00000100b
        db  00001000b

#include	"c:\xcez1.inc"
