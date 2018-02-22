/* C subroutine usage. This routine will print usage text to the screen when
 * itdlp is invoked with no command line options. The routine accepts the values
 * ND1, ND2, ND3, ND2X3, and ND5 to print to screen. */

#include <stdio.h>

void usage_(int *nd1, int *nd2, int *nd3, int *nd2x3, int *nd5)
{
    // Define the version number
    const char* itdlp_version="v1.3.1";

    // Define the build date
    const char* build_date=__DATE__;

    // Print the usage to screen
    printf("\n");
    printf("itdlp %s - %s - NOAA/NWS/MDL\n",itdlp_version,build_date);
    printf("\n");
    printf("Usage: itdlp [Input TDLPACK file] | [options] (order is not important)\n");
    printf("\n");
    printf("Options:\n");
    printf(" -append        -- Append TDLPACK records to and existing sequential or random access file)\n");
    printf(" -bin X         -- Output binary file X (NOTE: can only process 1 TDLPACK record at a time)\n");
    printf(" -change-dd X Y -- Output TDLPACK Sequental file with new DD X and filename Y\n");
    printf(" -date X        -- Dump TDLPACK records by date X in YYYYMMDDHH format\n");
    printf(" -debug         -- Generates log file with write statements from mos2k routines\n");
    printf(" -ftype         -- Print information about the input file. (NOTE: file will not be inventoried).\n");
    printf(" -gis X         -- Output ArcGIS ASCII grid file X (NOTE: can only process 1 TDLPACK record at a time)\n");
    printf(" -grid          -- Output grid specifications for use in superImageGen mapdef file\n");
    printf(" -help          -- Print itdlp help\n");
    printf(" -i             -- Read itdlp inventory from standard input\n");
    printf(" -id X          -- Dump TDLPACK record X by the 4-word MOS-2000 Id. Only 1 -id allowed\n");
    printf(" -invect X      -- Print inventory of station CALL letters and data values with X columns. X=1-5; default is 5\n"); 
    printf(" -is0           -- Print TDLPACK Identification Section 0\n");
    printf(" -is1           -- Print TDLPACK Identification Section 1 (NOTE: Only first 39 bytes)\n");
    printf(" -is2           -- Print TDLPACK Identification Section 2 (NOTE: Will only output for grids)\n");
    printf(" -is4           -- Print TDLPACK Identification Section 4 (NOTE: Only first 16 bytes)\n");
    printf(" -probe X Y,Z   -- Probe a grid where X is probe type 'ij' (grid) or 'll' (lat/lon); Y is the I-coord or Lat value; Z is the J-coord or Lon value\n");
    printf(" -rasize X      -- Create new RA file with a template size X where X = 'small' (DEFAULT) or 'large'\n");
    printf(" -rec X         -- Dump TDLPACK record X. Only 1 -rec allowed\n");
    printf(" -recsize       -- Print TDLPACK record size in bytes (appended to simple inventory output)\n");
    printf(" -simple        -- Simple inventory (i.e. data are not unpacked) (DEFAULT)\n");
    printf(" -tdlp X        -- Output TDLPACK records to new sequential file X\n");
    printf(" -tdlpra X      -- Output TDLPACK records to new random access file X (NOTE: Only 1 date allowed)\n");
    printf(" -text X        -- Output ASCII text file X (NOTE: can only process 1 TDLPACK Gridded record at a time)\n");
    printf(" -v             -- Verbose inventory (i.e. data are unpacked), statistics printed to screen (min, max, mean)\n");
    printf(" -vt            -- Print valid time (intialization time + forecast projection)\n");
    printf("\n");
    printf("Dimensions:\n");
    printf(" ND1   = %7d  -- Maximum number of stations\n",*nd1);
    printf(" ND2   = %7d  -- Maximum number of gridpoints in x-direction\n",*nd2);
    printf(" ND3   = %7d  -- Maximum number of gridpoints in y-direction\n",*nd3);
    printf(" ND2X3 = %7d  -- Maximum number of total gridpoints\n",*nd2x3);
    printf(" ND5   = %7d  -- Maximum size of a TDLPACK record in 4-byte words\n",*nd5);
    printf("\n");

    // Exit gracefully
    exit(0); 
}
