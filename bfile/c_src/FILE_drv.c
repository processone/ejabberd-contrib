/* Interface to stdio buffered FILE io                              */
/* author: klacke@kaja.klacke.net                                   */
/* Created : 22 Nov 1999 by Claes Wikstrom <klacke@kaja.klacke.net> */

#include <string.h>

#define USE_STDIO

#ifdef WIN32
#define USE_STDIO
#include <windows.h>

#else

#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>
#endif

#include "erl_driver.h"
#ifndef ERL_DRV_NIL
#include "erl_driver_compat.h"
#endif

#ifdef USE_STDIO
#  include <stdio.h>
#else

#  define malloc(s)     driver_alloc(s)
#  define realloc(p, s) driver_realloc(p, s)
#  define free(p)       driver_free(p)

#  define BINTERFACE static
#  include "bbio.c"
#  define FILE    bFILE
#  define clearerr bclearerr
#  define fclose  bfclose
#  define feof    bfeof
#  define ferror  bferror
#  define fflush  bfflush
#  define fgets   bfgets
#  define fileno  bfileno
#  define fopen   bfopen
#  define fread   bfread
#  define fseek   bfseek
#  define ftell   bftell
#  define fwrite  bfwrite
#  define getc    bgetc
#  define ungetc  bungetc
#endif


#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define put_int32(i, s) {((char*)(s))[0] = (char)((i) >> 24) & 0xff; \
                        ((char*)(s))[1] = (char)((i) >> 16) & 0xff; \
                        ((char*)(s))[2] = (char)((i) >> 8)  & 0xff; \
                        ((char*)(s))[3] = (char)((i)        & 0xff);}
/* op codes */

#define XX_OPEN             'o'
#define XX_CLOSE            'c'
#define XX_READ             'r'
#define XX_WRITE            'w'
#define XX_SEEK             's'
#define XX_TELL             't'
#define XX_TRUNCATE         'T'
#define XX_FLUSH            'f'
#define XX_OEOF             'e'
#define XX_ERROR            'E'
#define XX_GETC             'g'
#define XX_GETS             'G'
#define XX_GETS2            '2'
#define XX_SET_LINEBUF_SIZE 'S'
#define XX_UNGETC           'u'



/* return codes */
#define XX_VALUE   'v'
#define XX_FLINE   'L'
#define XX_OK      'o'
#define XX_I32     'O'
#define XX_NOLINE  'N'
#define XX_FERROR  'E'
#define XX_REOF    'x'


#ifdef WIN32
#define XX_EINVAL WSAEINVAL


#else
#define XX_EINVAL EINVAL

#endif


static ErlDrvData FILE_start(ErlDrvPort port, char *buf);
static void FILE_stop(ErlDrvData drv_data);

static ErlDrvEntry FILE_driver_entry;

typedef struct _desc {
    ErlDrvPort port;
    FILE *fp;
    int linebuf_size;
} Desc;


static ErlDrvData FILE_start(ErlDrvPort port, char *buf)
{
    Desc *d = (Desc*) driver_alloc(sizeof (Desc));

    if (d == NULL) 
	return (ErlDrvData) -1;
    d->fp = NULL;
    d->port = port;
    d->linebuf_size = 255;   /* default line size */

    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

    return (ErlDrvData) d;
}


static void FILE_stop(ErlDrvData drv_data)
{
    Desc *d = (Desc*) drv_data;
    if (d->fp)
	fclose(d->fp);
    driver_free(d);
}


static char *driver_error(ErlDrvPort port, int err)
{
    char response[256];		/* Response buffer. */
    char* s;
    char* t;
    ErlDrvBinary* bin;

    bin = driver_alloc_binary(1);
    bin->orig_bytes[0] = XX_FERROR;

    response[0] = XX_FERROR;
    for (s = erl_errno_id(err), t = bin->orig_bytes + 1; *s; s++, t++)
	*t = tolower(*s);
    return (char *)bin;
}

static char *driver_ret32(ErlDrvPort port, unsigned int r)
{
    char ch = XX_I32;
    ErlDrvBinary* bin;

    bin = driver_alloc_binary(1);
    bin->orig_bytes[0] = ch;
    put_int32(r, bin->orig_bytes + 1);
    return (char *)bin;
}

static char *driver_ok(ErlDrvPort port)
{
    char ch = XX_OK;
    ErlDrvBinary* bin;
    bin = driver_alloc_binary(1);
    bin->orig_bytes[0] = ch;
    return (char *)bin;
}

static char *driver_eof(ErlDrvPort port)
{
    char ch = XX_REOF;
    ErlDrvBinary* bin;
    bin = driver_alloc_binary(1);
    bin->orig_bytes[0] = ch;
    return (char *)bin;
}


static int FILE_control(ErlDrvData drv_data,
			unsigned int command,
			char *buf, int len,
			char **rbuf, int rlen)
{
    Desc *desc = (Desc*) drv_data;
    ErlDrvBinary* bin;

    switch (command) {

    case XX_OPEN: {
	char file[BUFSIZ];  /* should be FILENAME_MAX */
	char flags[4];       /* at most someething like rb+ */
	char* src;
	char* dst;
	char* src_end;
	char* dst_end;

	if (desc->fp != NULL) {
	    *rbuf = driver_error(desc->port, XX_EINVAL);
	    return 1;
	}

	/* play it safe ? */
	src = buf;
	src_end = buf + len;

	/* get file name */
	dst = file;
	dst_end = dst + BUFSIZ;  /* make room for a '\0' */
	while((src < src_end) && (dst < dst_end) && (*src != '\0'))
	    *dst++ = *src++;
	if ((src == src_end) || (dst == dst_end)) {
	    driver_error(desc->port, XX_EINVAL);
	}
	*dst = *src++;
	/* get flags */
	dst = flags;
	dst_end = dst + 4;
	while((src < src_end) && (dst < dst_end) && (*src != '\0'))
	    *dst++ = *src++;	
	if (dst == dst_end) {
	    *rbuf = driver_error(desc->port, XX_EINVAL);
	    return 1;
	}
	*dst = '\0';

	if (src + 1 != src_end) {
	    *rbuf = driver_error(desc->port, XX_EINVAL);
	    return 1;
	}

	if ((desc->fp = fopen(file, flags))==NULL) {
	    *rbuf = driver_error(desc->port, errno);
	    return 1;
	}
	*rbuf = driver_ok(desc->port);
	return 1;
	break;
    }

    case XX_WRITE: {
        if (fwrite(buf, 1, len, desc->fp) != len) {
	    *rbuf = driver_error(desc->port, errno);
	    return 1;
	}
	*rbuf = driver_ok(desc->port);
	return 1;
	break;
    }

    case XX_READ: {
	char ch = XX_VALUE;
	int rval;
	int sz = get_int32(buf);

	if ((bin = driver_alloc_binary(sz + 1)) == NULL) {
	    *rbuf = driver_error(desc->port, -1);
	    return 1;
	}

	bin->orig_bytes[0] = ch;
	if ((rval = fread(bin->orig_bytes + 1, 1, sz, desc->fp)) != sz) {
	    if (feof(desc->fp)) {
		if (rval == 0) {
		    driver_free_binary(bin);
		    *rbuf = driver_eof(desc->port);
		    return 1;
		}
		bin = driver_realloc_binary(bin, rval + 1);
		*rbuf = (char *)bin;
		return 1;
	    }
	    driver_free_binary(bin);
	    *rbuf = driver_error(desc->port, errno);
	    return 1;
	}
	*rbuf = (char *)bin;
	return 1;
    }

    case XX_SEEK: {
	int offs = get_int32(buf);
	int w = (int) buf[4];
	int whence;
	switch (w) {
	case 1: whence = SEEK_SET; break;
	case 2: whence = SEEK_CUR; break;
	case 3: whence = SEEK_END; break;
	}
	if ((w = fseek(desc->fp, offs, whence)) != 0) {
	    *rbuf = driver_error(desc->port, errno);
	    return 1;
	}
	*rbuf = driver_ok(desc->port);
	return 1;
    }

    case XX_TELL: {
	int offs;
	if ((offs = ftell(desc->fp)) == -1) {
	    *rbuf = driver_error(desc->port, errno);
	    return 1;
	}
	*rbuf = driver_ret32(desc->port, offs);
	return 1;
	break;
    }

    case XX_TRUNCATE: {
        int fno;
        int offs;
	/* is this really safe? */
        if (fflush(desc->fp) != 0) {
	    *rbuf = driver_error(desc->port, errno);
	    return 1;
	}
	if ((offs = ftell(desc->fp)) == -1) {
	    *rbuf = driver_error(desc->port, errno);
	    return 1;
	}
        fno = fileno(desc->fp);
#ifdef WIN32
	if (SetEndOfFile((HANDLE)fno) !=  0) {
	    *rbuf = driver_error(desc->port, GetLastError());
	    return 1;
	}
#else
        if (ftruncate(fno, offs) == -1) {
	    *rbuf = driver_error(desc->port, errno);
	    return 1;
	}
#endif
	*rbuf = driver_ok(desc->port);
	return 1;
    }

    case XX_FLUSH:
	if (fflush(desc->fp) != 0)
	    *rbuf = driver_error(desc->port, errno);
	else
	    *rbuf = driver_ok(desc->port);
	return 1;
	break;

    case XX_OEOF:
	if (feof(desc->fp))
	    *rbuf = driver_ret32(desc->port, 1);
	else
	    *rbuf = driver_ret32(desc->port, 0);
	return 1;
	break;

    case XX_ERROR:
	if (ferror(desc->fp))
	    *rbuf = driver_ret32(desc->port, 1);
	else
	    *rbuf = driver_ret32(desc->port,0);
	return 1;
	break;

    case XX_GETC: {
	int ch;
	if ((ch = getc(desc->fp)) == EOF) {
	    if (feof(desc->fp)) {
		*rbuf = driver_eof(desc->port);
		return 1;
	    }
	    *rbuf = driver_error(desc->port, errno);
	    return 1;
	}
	*rbuf = driver_ret32(desc->port, ch);
	return 1;
	break;
    }

    case XX_SET_LINEBUF_SIZE: {
	int sz = get_int32(buf);
	desc->linebuf_size = sz;
	*rbuf = driver_ok(desc->port);
	return 1;
	break;
    }

    case XX_GETS:
    case XX_GETS2: {
	int rval;
	long cpos1, cpos2;
	char header;
	
	if ((bin = driver_alloc_binary(desc->linebuf_size + 1)) == NULL) {
	    *rbuf = driver_error(desc->port, -1);
	    return 1;
	}

	if ((cpos1 = ftell(desc->fp)) == -1) {
	    driver_free_binary(bin);
	    *rbuf = driver_error(desc->port, errno);
	    return 1;
	}

	if ((fgets(bin->orig_bytes + 1, desc->linebuf_size,
		   desc->fp)) == NULL) {
	    driver_free_binary(bin);
	    if (feof(desc->fp)) {
		*rbuf = driver_eof(desc->port);
		return 1;
	    }
	    *rbuf = driver_error(desc->port, errno);
	    return 1;
	}
	if ((cpos2 = ftell(desc->fp)) == -1) {
	    driver_free_binary(bin);
	    *rbuf = driver_error(desc->port, errno);
	    return 1;
	}
	rval = cpos2 - cpos1;

	if (bin->orig_bytes[rval] == '\n' &&
	    bin->orig_bytes[rval + 1] == 0) {
	    header = XX_FLINE;
	    /* GETS keep newline, GETS2 remove newline */
	    rval = rval - (command == XX_GETS ? 0 : 1);
	}
	else
	    header = XX_NOLINE;
	bin->orig_bytes[0] = header;
	bin = driver_realloc_binary(bin, rval + 1);
	*rbuf = (char *)bin;
	return 1;
    }

    case XX_UNGETC: {
	int ch = buf[0];
	if (ungetc(ch, desc->fp) == EOF)
	    *rbuf = driver_error(desc->port, errno);
	else
	    *rbuf = driver_ok(desc->port);
	return 1;
	break;
    }
    
    default:
#ifdef DEBUG
	fprintf(stderr, "Unknown opcode %c\n\r", command);
#endif
	*rbuf = driver_error(desc->port, XX_EINVAL);
	return 1;
	break;
    }
	
	
}

static void FILE_finish()
{
#ifndef USE_STDIO
    /*
     * Make sure any remaining buffers are flushed (this is done on exit() by
     * the normal stdio).
     */
    bbio_cleanup();
#endif
}


/*
 * Initialize and return a driver entry struct
 */

DRIVER_INIT(FILE_drv)
{
    FILE_driver_entry.init         = NULL;   /* Not used */
    FILE_driver_entry.start        = FILE_start;
    FILE_driver_entry.stop         = FILE_stop;
    FILE_driver_entry.output       = NULL;
    FILE_driver_entry.ready_input  = NULL;
    FILE_driver_entry.ready_output = NULL;
    FILE_driver_entry.driver_name  = "FILE_drv";
    FILE_driver_entry.finish       = FILE_finish;
    FILE_driver_entry.outputv      = NULL;
    FILE_driver_entry.control      = FILE_control;
    return &FILE_driver_entry;
}

