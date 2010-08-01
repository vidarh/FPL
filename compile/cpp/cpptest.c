#define MAC_PARM    0x7F        /* Macro formals start here */
#define PAR_MAC 12
#if PAR_MAC >= 33
    assertion fails -- PAR_MAC isn't less than 33
#endif
#define LASTPARM    (PAR_MAC - 1)

