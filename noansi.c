#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/types.h>

/* noansi: rough translator from cp437+dosansi -> ascii+mirc colors
 * cstone@pobox.com
 *
 * run noansi -h for usage
 *
 * R.I.P. BANTOWN
 *
 * references used here: ctlseqs.ms (xorg source); ms-dos 6.22 help (ansi.sys);
 * ecma-48; (http://www.ecma-international.org/publications/standards/Ecma-048.htm);
 * NANSI source (4.0, earlier); various comp.terminals searches; dosbox
 *
 * (it should be noted that none of these perfectly explains why sequences like SGR 55
 * or SGR 48 (esp. in constructions like in CSI 1 ; 48 m, which makes no sense
 * and doesn't seem to do anything) end up in ansis from the early 1990s &
 * intended for visual display only.  SGR 48, 53, 55 are always silently ignored.)
 *
 * also unknown: CSI 0x4b 0x4d
 */
#define NCOLS 80
#define NROWS 1024
#define MAXSEQLEN 64
enum { 
    /* mirc color codes; from http://www.mirc.co.uk/help/color.txt */
        mWHITE = 0,   mBLACK = 1,   mBLUE = 2,   mGREEN = 3,
        mRED = 4,     mBROWN = 5,   mPURPLE = 6, mORANGE = 7,
        mYELLOW = 8,  mLTGREEN = 9, mTEAL = 10,  mCYAN = 11,
        mLTBLUE = 12, mPINK = 13,   mGREY = 14,  mLTGREY = 15,

    /* iso/ansi color pattern. +30 for fg, +40 for bg */
        aBLACK = 0, aRED = 1,     aGREEN = 2, aYELLOW = 3,
        aBLUE = 4,  aMAGENTA = 5, aCYAN = 6,  aWHITE = 7,

    /* SGR flags (that we support here); see below */
        ACF_BOLD =    0x10000, ACF_UNDERLINE =  0x20000,
        ACF_BLINK =   0x40000, ACF_INVERSE =    0x80000,
        ACF_BGBOLD = 0x100000, 
        
    /* special "unchanged" flag used in default_char */
        ACF_UNCHANGED = 0x200000,
};
const int sgr_to_mirc[] = {
    mBLACK,  mRED, mGREEN,   mYELLOW, mBLUE,   mPURPLE, mCYAN, mLTGREY,
    mGREY,  mPINK, mLTGREEN, mYELLOW, mLTBLUE, mPINK,   mTEAL, mWHITE
};

/* bits 0-7 of achar_t are the character; bits 8-11 are the fg color; bits
 * 12-15 are the bg color; the rest are flags
 */
typedef u_int32_t achar_t;
#define AC(chr,fg,bg,flags) ((chr & 0xff) | ((fg & 0xf) << 8) | ((bg & 0xf) << 12) | (flags & 0xffff0000))
#define ACCHAR(x) ((unsigned char)(x & 0xff))
#define ACREST(x) (x & 0xffffff00)
#define ACFG(x) ((x >> 8) & 0xf)
#define ACBG(x) ((x >> 12) & 0xf)
#define ACFLAGS(x) (x & 0xffff0000)

#ifndef MAX
#define MAX(x,y) ((x) > (y) ? (x) : (y))
#endif
#ifndef MIN
#define MIN(x,y) ((x) > (y) ? (y) : (x))
#endif

static const unsigned int default_fg = aWHITE, default_bg = aBLACK;
static const unsigned char default_ch = ' ';
static const achar_t default_char = AC(' ', aWHITE, aBLACK, ACF_UNCHANGED);

static int expandtab = 0;
static int includez = 0;

static int errors = 0;

static void doerror(char const *fmt, ...) {
    va_list va;
    va_start(va, fmt);
    vfprintf(stderr, fmt, va);
    va_end(va);
    if(errors == 0) {
        abort();
    }
}
static void handle_sgr(int sgrcode, unsigned int *curfg, unsigned int *curbg,
        unsigned int *curflags) {
    switch(sgrcode) {
        case 0:
            *curflags = 0;
            *curfg = default_fg;
            *curbg = default_bg;
            break;
        case 1:   /* bold */
            *curflags = ACF_BOLD;
            break;
        case 4:   /* underlined */
            *curflags = ACF_UNDERLINE;
            break;
        case 5:   /* blink */
            *curflags = ACF_BLINK;
            break;
        case 7:   /* inverse */
            *curflags = ACF_INVERSE;
            break;
        case 30: case 31: case 32: case 33:
        case 34: case 35: case 36: case 37:
            *curfg = sgrcode-30;
            break;
        case 39:   /* ctlseqs.ms: "Set foreground color to default (original)" */
            *curfg = default_fg;
            break;
        case 40: case 41: case 42: case 43:
        case 44: case 45: case 46: case 47:
            *curbg = sgrcode-40;
            break;

        case 8:    /* invisible */
        case 48:   /* i'm not sure what this is supposed to do.  ECMA-48
                    * indicates that this is supposed to be used in cases such
                    * as CSI [ 48 ; 5 ; Ps m, which is supposed to set the bg
                    * color to Ps.  but this construction doesn't seem to
                    * work:  Ps is treated as a normal SGR code.  ctlseqs.ms
                    * says this is only for newer xterm/rxvts, anyway..
                    */
        case 53:   /* enable "overline mode" (an apparent misnomer in many cases) */
        case 55:   /* disable overline mode */
            break;
        default:
            fprintf(stderr, "warning: invalid SGR code %d detected, ignoring\n", 
                    sgrcode);
            break;
    }
}
static void clear_screen(achar_t screen[NROWS][NCOLS]) {
    int i, j;
    for(i = 0; i < NROWS; i++) {
        for(j = 0; j < NCOLS; j++) {
            screen[i][j] = default_char;
        }
    }
}
static void read_ansi(FILE *f, achar_t screen[NROWS][NCOLS]) {
    unsigned char y = 0, savedx = 255, savedy = 255, delta;
    int c, d;
    unsigned int x = 0;
    unsigned int curbg = aBLACK, curfg = aWHITE, curflags = 0, wrapping = 1;
    while((c = fgetc(f)) != EOF) {
        int params[3] = { -1, -1, -1 }, np = 0, bidx = 0, i, quesflag = 0,
            semicount = 0;
        unsigned int curseqlen = 0;
        char nbuf[5];
        if(c != 27) {
            if(c == 0xa) {
                x = MIN(NROWS-1, x+1);
                y = 0;
                continue;
            } else if(c == 0xd) {
                y = 0;
                continue;
            } else if(c == 0x9 && expandtab == 1) {
                y = MIN(NCOLS-1, ((y + 8) & 0xf8));
                continue;
            } else if(c == 0x1a && includez == 0) {
                return;
            }
            screen[x][y] = AC(c, curfg, curbg, curflags);
            /* last-line wrapping behavior may need to change */
            if(wrapping) {
                y += 1;
                x = MIN(NROWS-1, x+(y/NCOLS));
                y %= NCOLS;
            } else {
                y = MIN(NCOLS-1,(y+1));
            }
            continue;
        }
        d = fgetc(f);
        if(d == EOF) {
            doerror("EOF reached after ESC, aborting\n");
        } else if(d != '[') {
            doerror("unknown sequence EOF 0x%d at pos %ld, aborting\n",
                    d, ftell(f)-1);
        } else if(d == 0x1a && includez == 0) {
            return;
        }
        while((d = fgetc(f)) != EOF) {
            curseqlen++;
            if(curseqlen == MAXSEQLEN) {
                doerror("reached max sequence length %u at position %ld, aborting\n",
                        curseqlen, ftell(f)-1);
            }
            if(d == 0x1a && includez == 0) {
                return;
            }
            if(isdigit(d)) {
                if(bidx == sizeof(nbuf)-1) {
                    doerror("error at pos %ld: number too large, aborting\n", ftell(f)-1);
                }
                nbuf[bidx++] = (char)d;
                continue;
            }
            if(bidx > 0 && np < sizeof(params)/sizeof(int)) {
                nbuf[bidx] = 0;
                params[np++] = atoi(nbuf);
                bidx = 0;
            }
            switch(d) {
                case '?':
                    if(np != 0) {
                        doerror("invalid sequence CSI ... ; ? at pos %ld\n",
                                ftell(f)-1);
                    }
                    quesflag = 1;
                    continue;
                case ';':
                    semicount++;
                    continue;
                case 'm':   /* set graphics (SGR) attributes */
                    if(quesflag) {
                        doerror("invalid CSI ? ... m at pos %ld\n",
                                ftell(f)-1);
                    }
                    if(np == 0) {
                        handle_sgr(0, &curfg, &curbg, &curflags);
                    } else {
                        for(i = 0; i < np; i++) {
                            handle_sgr(params[i], &curfg, &curbg, &curflags);
                        }
                    }
                    break;
                case 'J':  /* erase parts of the display.  only CSI 2 J handled here */
                    if(quesflag) {
                        doerror("invalid CSI ? ... m at pos %ld\n",
                                ftell(f)-1);
                    }
                    if(np != 1) {
                        doerror("expected 1 param for CSI ... J, got %d\n", 
                                np);
                    }
                    if(params[0] != 2) {
                        doerror("unknown parameter p = %d for CSI p J\n", np);
                    }
                    clear_screen(screen);
                    x = 0;
                    y = 0;
                    break;
                case 'h':   /* only handling CSI ? 7 h (enable wrapping) */
                    if(quesflag) {
                        if(np != 1 || params[0] != 7) {
                            doerror("expected CSI ? 7 h at position %ld\n",
                                    ftell(f)-1);
                        }
                        wrapping = 1;
                    } else {
                        doerror("unknown sequence: CSI %d %d %d h at position %ld\n", 
                            params[0], params[1], params[2], ftell(f)-1);
                    }
                    break;
                case 'H':  /* CUP (CSI row ; col H): set position */
                    if(quesflag) {
                        doerror("invalid CSI ? ... H at pos %ld\n",
                                ftell(f)-1);
                    }
                    if(np == 0) {
                        x = y = 0;
                    } else if(np == 1) {
                        if(semicount == 0) {
                            x = MAX(0, MIN(params[0]-1, NROWS-1));
                            y = 0;
                        } else {
                            x = 0;
                            y = MAX(0, MIN(params[0]-1, NCOLS-1));
                        }
                    } else if(np == 2) {
                        x = MAX(0, MIN(params[0]-1, NROWS-1));
                        y = MAX(0, MIN(params[1]-1, NCOLS-1));
                    } 
                    break;
                case 's':   /* save cursor position */
                    if(quesflag || np != 0) {
                        doerror("invalid CSI s form at pos %ld\n",
                                ftell(f)-1);
                    }
                    savedx = x;
                    savedy = y;
                    break;
                case 'u':  /* restore cursor position */
                    if(quesflag || np != 0) {
                        doerror("invalid CSI s form at pos %ld\n",
                                ftell(f)-1);
                    }
                    if(savedx == 255 || savedy == 255) {
                        doerror("CSI u before a CSI s at pos %ld\n",
                                ftell(f)-1);
                    }
                    x = savedx;
                    y = savedy;
                    break;
                case 'A':   /* move up <p> rows */
                    if(quesflag) {
                        doerror("invalid CSI ? ... A at pos %ld\n",
                                ftell(f)-1);
                    }
                    if(np > 1) {
                        doerror("expected 0-1 parameters, got %d for CSI ... A at pos %ld\n",
                                np, ftell(f)-1);
                    } else if(np == 1) {
                        delta = params[0];
                    } else {
                        delta = 1;
                    }
                    if(delta > x) {
                        x = 0;
                    } else {
                        x -= delta;
                    }
                    break;
                case 'B':   /* move down <p> rows */
                    if(quesflag) {
                        doerror("invalid CSI ? ... B at pos %ld\n",
                                ftell(f)-1);
                    }
                    if(np > 1) {
                        doerror("expected 0-1 parameters, got %d for CSI ... B at pos %ld\n",
                                np, ftell(f)-1);
                    } else if(np == 1) {
                        delta = params[0];
                    } else {
                        delta = 1;
                    }
                    x = MIN(NROWS-1, (x+delta));
                    break;
                case 'C':   /* move forward <p> columns */
                    if(quesflag) {
                        doerror("invalid CSI ? ... C at pos %ld\n",
                                ftell(f)-1);
                    }
                    if(np > 1) {
                        doerror("expected 0-1 parameters, got %d for CSI ... C at pos %ld\n",
                                np, ftell(f)-1);
                    } if(np == 1) {
                        delta = params[0];
                    } else {
                        delta = 1;
                    }
                    y = MIN(NCOLS-1, (y+delta));
                    break;
                case 'D':
                    if(quesflag) {
                        doerror("invalid CSI ? ... D at pos %ld\n",
                                ftell(f)-1);
                    }
                    if(np > 1) {
                        doerror("expected 0-1 parameters, got %d for CSI ... D at pos %ld\n",
                                np, ftell(f)-1);
                    } if(np == 1) {
                        delta = params[0];
                    } else {
                        delta = 1;
                    }
                    if(delta > y) {
                        y = 0;
                    } else {
                        y -= delta;
                    }
                    break;
                default:
                    doerror("unknown sequence CSI <params> 0x%x at pos %ld, aborting\n", 
                            d, ftell(f)-1);
                    break;
            }
            break;
        }
    }
}

/* a fairly simple algorithm; straight replacement except for some special
 * cases where we tweak the attributes.
 *
 * it's clear there were a lot of compromises made here:  the drawing characters
 * are all demoted to roguelike-style boxes like this:   +---+-----+
 * but this ends up more or less okay.  the real hurt    |   |     |
 * came in deciding what to do for the shading colors    +---+-----+
 * (0xb0-b2, 0xdb-df).  
 * db-df are the trickiest, from my point of view; the approach taken here is 
 * to reverse and use a space instead.  this is fine until we run into the
 * problem of bold colors: there's no way to do bold backgrounds.  for some
 * colors this doesn't matter; for others, we flip the ACF_BGBOLD flag.
 */
static const unsigned char cp437_to_ascii_map[] = {
    /* 00 */  ' ', '@', '@', '*',   'x', 'A', '*', '*',
    /* 08 */  '*', 'o', '*', '6',   'Q', 'f', 'M', '*',
    /* 10 */  '>', '<', '$', '!',   'P', 'S', '_', '$',
    /* 18 */  '^', 'v', '>', '<',   '_', '-', 'A', 'v',
    /* 20 */  ' ', '!', '"', '#',   '$', '%', '&', '\'',
    /* 28 */  '(', ')', '*', '+',   ',', '-', '.', '/',
    /* 30 */  '0', '1', '2', '3',   '4', '5', '6', '7',
    /* 38 */  '8', '9', ':', ';',   '<', '=', '>', '?',
    /* 40 */  '@', 'A', 'B', 'C',   'D', 'E', 'F', 'G',
    /* 48 */  'H', 'I', 'J', 'K',   'L', 'M', 'N', 'O',
    /* 50 */  'P', 'Q', 'R', 'S',   'T', 'U', 'V', 'W',
    /* 58 */  'X', 'Y', 'Z', '[',   '\\', ']', '^', '_',
    /* 60 */  '`', 'a', 'b', 'c',   'd', 'e', 'f', 'g',
    /* 68 */  'h', 'i', 'j', 'k',   'l', 'm', 'n', 'o',
    /* 70 */  'p', 'q', 'r', 's',   't', 'u', 'v', 'w',
    /* 78 */  'x', 'y', 'z', '{',   '|', '}', '~', '^',

    /* 80 */  'C', 'u', 'e', 'a',   'a', 'a', 'a', 'c',
    /* 88 */  'e', 'e', 'e', 'i',   'i', 'i', 'A', 'A',
    /* 90 */  'E', '%', 'A', 'o',   'o', 'o', 'u', 'u',
    /* 98 */  'y', 'O', 'U', 'c',   'L', 'Y', 'P', 'f',
    /* a0 */  'a', 'i', 'o', 'u',   'n', 'N', '~', '^',
    /* a8 */  '?', '+', '+', 'X',   'K', '!', '<', '>',
    /* b0 */  '#', '@', '#', '|',   '+', '+', '+', '+',
    /* b8 */  '+', '+', '|', '+',   '+', '+', '+', '+',
    /* c0 */  '+', '+', '+', '+',   '-', '+', '+', '+',
    /* c8 */  '+', '+', '+', '+',   '+', '=', '+', '+',
    /* d0 */  '+', '+', '+', '+',   '+', '+', '+', '+',
    /* d8 */  '+', '+', '+', ' ',   'm', '|', '|', '"',
    /* e0 */  'a', 'B', 'r', 'n',   'E', 'q', 'u', 'r',
    /* e8 */  'I', '0', '*', 'o',   '*', '0', 'E', 'n',
    /* f0 */  '=', '+', '>', '<',   'l', 'j', '%', '=',
    /* f8 */  '*', '.', '.', 'j',   'n', '2', '#', ' ',
};
static void cp437_to_ascii(achar_t screen[NROWS][NCOLS]) {
    int i, j;
    for(i = 0; i < NROWS; i++) {
        for(j = 0; j < NCOLS; j++) {
            achar_t c = screen[i][j], rest = ACREST(c);
            unsigned char oldchar = ACCHAR(c);
            if(oldchar == 0x02 || oldchar == 0xb2 || oldchar == 0xdb) {
                rest ^= ACF_BGBOLD;
                rest ^= ACF_INVERSE;
            } else if(oldchar == 0xb1) {
                rest ^= ACF_BOLD;
            }
            screen[i][j] = rest | cp437_to_ascii_map[oldchar];
        }
    }
}
/* interpret and remove all attributes. */
static void normalize(achar_t screen[NROWS][NCOLS]) {
    int i, j;
    for(i = 0; i < NROWS; i++) {
        for(j = 0; j < NCOLS; j++) {
            achar_t c = screen[i][j];
            if(c == default_char) {
                continue;
            }
            achar_t flags = ACFLAGS(c);
            unsigned char ch = ACCHAR(c);
            unsigned int bgcolor = ACBG(c), fgcolor = ACFG(c), x;
            if(flags & ACF_BOLD) {
                fgcolor |= 8;
            }
            if(flags & ACF_BGBOLD) {
                bgcolor |= 8;
            }
            if(flags & ACF_UNDERLINE) {
                /* nothing right now */
            }
            if(flags & ACF_BLINK) {
                /* nothing right now */
            } 
            if(flags & ACF_INVERSE) {
                x = bgcolor;
                bgcolor = fgcolor;
                fgcolor = x;
            }
            screen[i][j] = AC(ch, fgcolor, bgcolor, 0);
        }
    }
}
static void output_mirc(achar_t screen[NROWS][NCOLS], unsigned short colstart, unsigned short colend) {
    int i, j, lasti = 0;
    for(i = NROWS-1; i >= 0 && lasti == 0; i--) {
        for(j = 0; j < NCOLS; j++) {
            if(screen[i][j] != default_char) {
                lasti = i+1;
                break;
            }
        }
    }
    lasti = MIN(colend, i+2);
    for(i = colstart; i < lasti; i++) {
        unsigned int curfg = 65535, curbg = 65535; 
        int stopidx;
        for(j = NCOLS-1; j >= 0; j--) {
            achar_t ac = screen[i][j];
            if(ac != default_char) {
                break;
            }
        }
        stopidx = j;
        for(j = 0; j <= stopidx; j++) {
            achar_t c = screen[i][j];
            unsigned char ch = ACCHAR(c);
            unsigned int bgcolor, fgcolor, fgchange = 0, bgchange = 0;
            if(c == default_char) {
                bgcolor = sgr_to_mirc[default_bg];
                fgcolor = sgr_to_mirc[default_fg];
            } else {
                bgcolor = sgr_to_mirc[ACBG(c)];
                fgcolor = sgr_to_mirc[ACFG(c)];
            }
            if(curfg != fgcolor) {
                curfg = fgcolor;
                fgchange = 1;
            }
            if(curbg != bgcolor) {
                curbg = bgcolor;
                bgchange = 1;
            }
            if(fgchange || bgchange) {
                if(bgchange) {
                    printf("%c%02d,%02d", 0x3, curfg, curbg);
                } else {
                    printf("%c%02d", 0x3, curfg);
                }
            }
            if(fputc(ch, stdout) == EOF) {
                fprintf(stderr, "eof at output; exiting\n");
            }
        }
        if(fputc('\n', stdout) == EOF) {
            fprintf(stderr, "eof at output; exiting\n");
        }
    }
}
static void check(achar_t screen[NROWS][NCOLS]) {
    int i, j;
    for(i = 0; i < NROWS; i++) {
        for(j = 0; j < NCOLS; j++) {
            if(screen[i][j] == 0) {
                doerror("stop %d %d\n", i, j);
            }
        }
    }
}

void usage(void) {
    fprintf(stderr, "args: [-tzh] [START-END]\n");
    fprintf(stderr, "      -t: expand tabs to 8 spaces like DOS does\n");
    fprintf(stderr, "      -z: don't stop reading when an EOF (^Z, 0x1a) is encountered");
    fprintf(stderr, "      -h: show this text\n");
    fprintf(stderr, "\n      START and END are the lines to display; START is inclusive and END is exclusive\n");
}

extern int optind;
int main(int argc, char *argv[]) {
    unsigned short colstart = 0, colend = NCOLS;
    int ch;
    while((ch = getopt(argc, argv, "thz")) != -1) {
        switch (ch) {
            case 't':
                expandtab = 1;
                break;
            case 'z':
                includez = 1;
                break;
            case 'h':
             default:
                 usage();
                 exit(0);
        }
    }
    argc -= optind;
    argv += optind;
    if(argc > 0) {
        unsigned short nc, ne;
        if(sscanf(argv[0], "%hu-%hu", &nc, &ne) != 2) {
            usage();
            doerror("invalid length, expected START-END (0-indexed, START inclusive, END exclusive)\n");
        } else {
            colstart = nc;
            colend = ne;
        }
    }
    achar_t screen[NROWS][NCOLS];
    clear_screen(screen);
    read_ansi(stdin, screen);
    cp437_to_ascii(screen);
    normalize(screen);
    output_mirc(screen, colstart, colend);
    return 0;
}
