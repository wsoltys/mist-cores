/***********************************************************************
 *
 * Apple ][ .dsk file to .nib file format converter
 *
 * Stephen A. Edwards, sedwards@cs.columbia.edu
 *
 * Adapted from the "dsk2pdb" program supplied with the PalmApple/Appalm ][
 *
 ***********************************************************************
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef	unsigned char BYTE;

/*
 * GCR encoding/decoding utility
 */

#define RAW_TRACK_BYTES 0x1A00
#define DOS_TRACK_BYTES 4096

static  FILE	*diskimage;
static	int	write_protect;
static	BYTE	nibble[RAW_TRACK_BYTES];
static	BYTE	dos_track[4096];
static int		physical_track_no;

static BYTE	GCR_encoding_table[64] = {
  0x96, 0x97, 0x9A, 0x9B, 0x9D, 0x9E, 0x9F, 0xA6,
  0xA7, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, 0xB2, 0xB3,
  0xB4, 0xB5, 0xB6, 0xB7, 0xB9, 0xBA, 0xBB, 0xBC,
  0xBD, 0xBE, 0xBF, 0xCB, 0xCD, 0xCE, 0xCF, 0xD3,
  0xD6, 0xD7, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE,
  0xDF, 0xE5, 0xE6, 0xE7, 0xE9, 0xEA, 0xEB, 0xEC,
  0xED, 0xEE, 0xEF, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6,
  0xF7, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF };

static BYTE	GCR_decoding_table[256];
static	int	Swap_Bit[4] = { 0, 2, 1, 3 }; /* swap lower 2 bits */
static BYTE	GCR_buffer[256];
static BYTE	GCR_buffer2[86];

static int	Position = 0;
static BYTE	*Track_Nibble;

/* physical sector no. to DOS 3.3 logical sector no. table */
static int	Logical_Sector[16] = {
  0x0, 0x7, 0xE, 0x6, 0xD, 0x5, 0xC, 0x4,
  0xB, 0x3, 0xA, 0x2, 0x9, 0x1, 0x8, 0xF };

static int	Physical_Sector[16];

/* static function prototypes */

static void init_GCR_table(void);
static void gcr_write_nibble( BYTE );
static void encode62( BYTE* );
static void FM_encode( BYTE );
static void write_sync( int );
static void write_address_field( int, int, int );
static void write_data_field(void);

static void init_GCR_table(void)
{
  static int	initialized = 0;
  int		i;

  if ( !initialized ) {
    for ( i = 0; i < 64; i++ )
      GCR_decoding_table[GCR_encoding_table[i]] = i;
    for ( i = 0; i < 16; i++ )
      Physical_Sector[Logical_Sector[i]] = i;
    initialized = 1;
  }
}

static void gcr_write_nibble( BYTE data )
{
  Track_Nibble[Position++] = data;
  if ( Position >= RAW_TRACK_BYTES )
    Position = 0;
}

static void encode62( BYTE *page )
{
  int	i, j;

  /* 86 * 3 = 258, so the first two byte are encoded twice */
  GCR_buffer2[0] = Swap_Bit[page[1]&0x03];
  GCR_buffer2[1] = Swap_Bit[page[0]&0x03];

  /* save higher 6 bits in GCR_buffer and lower 2 bits in GCR_buffer2 */
  for( i = 255, j = 2; i >= 0; i--, j = j == 85? 0: j + 1 ) {
    GCR_buffer2[j] = (GCR_buffer2[j] << 2) | Swap_Bit[page[i]&0x03];
    GCR_buffer[i] = page[i] >> 2;
  }

  /* clear off higher 2 bits of GCR_buffer2 set in the last call */
  for( i = 0; i < 86; i++ )
    GCR_buffer2[i] &= 0x3f;
}

/*
 * write an FM encoded value, used in writing address fields
 */
static void FM_encode( BYTE data )
{
  gcr_write_nibble( (data >> 1) | 0xAA );
  gcr_write_nibble( data | 0xAA );
}

static void write_sync( int length )
{
  while( length-- )
    gcr_write_nibble( 0xFF );
}

static void write_address_field( int volume, int track, int sector )
{
  /*
   * write address mark
   */
  gcr_write_nibble( 0xD5 );
  gcr_write_nibble( 0xAA );
  gcr_write_nibble( 0x96 );

  /*
   * write Volume, Track, Sector & Check-sum
   */
  FM_encode( volume );
  FM_encode( track );
  FM_encode( sector );
  FM_encode( volume ^ track ^ sector );

  /*
   * write epilogue
   */
  gcr_write_nibble( 0xDE );
  gcr_write_nibble( 0xAA );
  gcr_write_nibble( 0xEB );
}

static void write_data_field(void)
{
  int	i;
  BYTE	last, checksum;

  /* write prologue */
  gcr_write_nibble( 0xD5 );
  gcr_write_nibble( 0xAA );
  gcr_write_nibble( 0xAD );

  /* write GCR encode data */
  for( i = 0x55, last = 0; i >= 0; i-- ) {
    checksum = last^ GCR_buffer2[i];
    gcr_write_nibble( GCR_encoding_table[checksum] );
    last = GCR_buffer2[i];
  }
  for( i = 0; i < 256; i++ ) {
    checksum = last ^ GCR_buffer[i];
    gcr_write_nibble( GCR_encoding_table[checksum] );
    last = GCR_buffer[i];
  }

  /* write checksum and epilogue */
  gcr_write_nibble( GCR_encoding_table[last] );
  gcr_write_nibble( 0xDE );
  gcr_write_nibble( 0xAA );
  gcr_write_nibble( 0xEB );
}

void SectorsToNibbles( BYTE *sectors, BYTE *nibbles, int volume, int track )
{
  int i;

  init_GCR_table();
  Track_Nibble = nibbles;
  Position = 0;

  for( i = 0; i < 16; i ++ ) {
    encode62( sectors + Logical_Sector[i] * 0x100 );
    write_sync( 16 );
    write_address_field( volume, track, i );
    write_sync( 8 );
    write_data_field();
  }
}

void load_track_buffer(void)
{
  int logical_track;

  if (!diskimage) return;

  if ( physical_track_no & 0x3 ) {
    fprintf( stderr, "Cannot read half track %g!\n",
	     physical_track_no * 0.25 );
  }

  logical_track = (physical_track_no+1)>>2;
  fseek( diskimage, logical_track * DOS_TRACK_BYTES, 0L );
  fread( dos_track, 1, DOS_TRACK_BYTES, diskimage );
  SectorsToNibbles( dos_track, nibble, 254, logical_track );
}

int mount_disk( char *filename)
{
  if ( diskimage ) unmount_disk();
  write_protect = 0;
  diskimage = fopen( filename, "rb+" );
  if ( !diskimage ) {
    write_protect = 1;
    diskimage = fopen( filename, "rb" );
  }
  if (!diskimage) {
    fprintf( stderr, "Fail to mount disk %s\n", filename );
    return -1;
  }
  else {
    load_track_buffer();
  }
  return 0;
}

int unmount_disk()
{
  if ( diskimage ) {
    fclose( diskimage );
    diskimage = NULL;
  }
  return 0;
}

int main(int argc, char **argv)
{
  FILE *nib;
  int tracks, sectors, file_size, header_size, track_size, i, j, cvt2raw, argfilename;
  char nibfile[64], *p;

  int now;

  if (argc >= 2) {
    argfilename = 1;
    cvt2raw = 0;
    if (argv[1][0] == '-') {
      argfilename = 2;
      if (argv[1][1] == 'r')
	cvt2raw = 1;
    }
    if (mount_disk(argv[argfilename])) {
      fprintf(stderr, "Unable to mount disk file %s.\n", argv[1]);
      exit(1);
    }
    tracks  = 35;
    sectors = 16;
    fseek(diskimage, 0, SEEK_END);
    file_size = ftell(diskimage);
    if (argc > argfilename + 1) {
      strcpy(nibfile, argv[argfilename + 1]);
    } else {
      /* Strip leading pathname from DSK name. */
      for (p = argv[argfilename]; *p; p++) {
	if (*p == '/' || *p == '\\')
	  argv[argfilename] = p + 1;
      }
      strcpy(nibfile, argv[argfilename]);
      strcat(nibfile, ".nib");
    }
    if (!(nib = fopen(nibfile, "wb"))) {
      fprintf(stderr, "Unable to create NIB file %s.\n", argv[2]);
      exit(1);
    }
    track_size  = RAW_TRACK_BYTES;
    for (physical_track_no = 0 ;
	 physical_track_no < tracks * 4 ;
	 physical_track_no += 4) {
      load_track_buffer();
      fwrite(nibble, RAW_TRACK_BYTES, 1, nib);
    }
    unmount_disk();
    fclose(nib);
  } else
    fprintf(stderr, "Usage: %s <DSK file> [NIB file]\n", argv[0]);    

  return 0;
}

