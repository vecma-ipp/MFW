/*
Name:  
	PSEUDO 

Purpose: 
	
	Retrieve time history DIII-D data from pseudo-pointnames through MDSplus

Calling Sequence:

	Y = MDSVALUE (' PSEUDO(  IN _pointname, 
      	                         OPTIONAL IN      _shot, 
                                 OPTIONAL OUT     _error  ) ')

Input Parameters: 

	POINTNAME 	:  string	- pointname requested from the pseudo-pointname FORTRAN library.
	SHOT		:  long		- shot number from which to retrieve pseudo-pointname.  
                    			  if not specified, defaults to current DIII-D shot.

Outputs:

	Y	: data from the requested pseudo-pointname.
	ERROR   : long -  PTDATA error code.

Restrictions:

	PSEUDO retrieves pseudo-pointname data from the FORTRAN shared library libidl_ga.  This TDI routine 
	is currently only supported on the DIII-D MDSplus server, atlas.gat.com.  Users must mdsconnect 
	to atlas.gat.com to retrieve pseudo-pointname data through TDI.  

Procedure:

	PSEUDO is a TDI function used to retrieve DIII-D pseudo-pointnames.  Users need to specify the 
	pointname and shot number.  The routine call the FORTRAN shared library libidl_ga to calculate 
	the pseudo-pointname.

Retrieving time dimension from requested PTDATA pointname: 

	PSEUDO returns the data from DIII-D pseuo-pointanames by default.  To retrieve the time dimension 
	from the pseudo-pointname, an additional call needs to be made after calling PSEUDO.  

	For example:

		Y = MDSVALUE(' PSEUDO(MSEP13,111203)   ')
		X = MDSVALUE(' DIM_OF(__pseduo_signal) ')

	The results from the PSEUDO call are stored in a public TDI variable (denoted by the double underscore 
	before the variable name __pseudo_signal).  The routine builds an MDSplus signal out of the data 
	retrieved from the pseudo-pointname.  So, a typical MDSplus call for the first dimension will return 
	the desired time array.

Required Software: Currently, only supported by the DIII-D MDSPlus server, atlas.gat.com. 

Author:  Sean Flanagan

Updated: 2005-10-10
*/
FUN PUBLIC PSEUDO(IN _pointname,IN _shot,OPTIONAL OUT _err) {
   
        _err = 0;
        _execute = 0;
        _pointname = UPCASE(_pointname//"          ");

        /* Check for PTDATA first, return if not pseudo-pointname. */
        _idfi = PTHEAD2(_pointname,_shot)[30];   
        if ( NE(_idfi,0) ) { return([0]); }

        /* If not in PTDATA, then try getallbig_camac for pseduo-pointname data */
        /* Some pseudo-pointnames are special cases                             */

        _npts      = 4000000;           /* max number of points    */
        _x         = ZERO(_npts,0.0);   /* x return array          */
        _y         = ZERO(_npts,0.0);   /* y return array          */
        _xmin      = -1e30;             /* -1e30 default in gadat3 */
        _xmax      =  1e30;             /*  1e30 default in gadat3 */
        _tmin      = _xmin / 1000;      /* always xmin/1000        */
        _tmax      = _xmax / 1000;      /* always xmax/1000        */
        _idebug    = 0;                 /* debug off by default    */

        if ("MSET" == EXTRACT(0,4,_pointname))  {  
           _nchan = EXTRACT(4,2,_pointname);
           if ("  " == _nchan) { write(*,"Error - nchan not 1 through 45"); return([0]); }
           EXECUTE( "_nchan = "//EXTRACT(4,2,_pointname) );
           _ibksub = 0;
           _msefitfun = 1;
           _label = ARRAY(11,1b);
           _status = BUILD_CALL(0, '/c/linux/lib/libidl_ga.so', 'mset_multi_c_f_',
                                   _shot, _msefitfun, REF(_label), REF(_xmin), REF(_xmax),
                                   REF(_x), REF(_y), REF(_npts), _nchan, REF(_err), _npts,
                                  _ibksub);
           _x = _x[0.._npts-1];
           _y = _y[0.._npts-1];
           PUBLIC __pseudo_signal=MAKE_SIGNAL(_y, *, _x);
           return(__pseudo_signal); 
        }
        

        if ("MSEP" == EXTRACT(0,4,_pointname))  { 
           _nchan = EXTRACT(4,2,_pointname); 
           if ("  " == _nchan) { write(*,"Error - nchan not 1 through 45"); return([0]); } 
           EXECUTE( "_nchan = "//EXTRACT(4,2,_pointname) );
           _ibksub = 0;
           _msefitfun = 1;
           _label = ARRAY(11,1b);
           _status = BUILD_CALL(0, '/c/linux/lib/libidl_ga.so', 'msep_multi_c_f_',
                                   _shot, _msefitfun, REF(_label), REF(_xmin), REF(_xmax),
                                   REF(_x), REF(_y), REF(_npts), _nchan, REF(_err), _npts,
                                   _ibksub);
           _x = _x[0.._npts-1];
           _y = _y[0.._npts-1];
           PUBLIC __pseudo_signal=MAKE_SIGNAL(_y, *, _x);
           return(__pseudo_signal); 
        }

        if ("SPRED" == EXTRACT(0,5,_pointname))  {  
           _nwrite    = 10;                /* always 10 in gadat      */
           _ulab      = ARRAY(16,1b);    
           _label     = ARRAY(27,1b);  
           _status = BUILD_CALL(0, '/c/linux/lib/libidl_ga.so', "spred_data_f_c_",
                                   _shot, _pointname, REF(_x), REF(_y), REF(_npts), REF(_ulab), REF(_label), 
                                   REF(_TMIN), REF(_TMAX), REF(_XMIN), REF(_XMAX),REF(_err), REF(_nwrite) );
           _x = _x[0.._npts-1];
           _y = _y[0.._npts-1];
           PUBLIC __pseudo_signal=MAKE_SIGNAL(_y, *, _x);
           return(__pseudo_signal); 
        }

        _ulab      = "               "; /* char 15                 */
        _label     = "            ";    /* char 12                 */
        _ical      = 1;                 /* always 1                */
        _iwait     = 0;                 /* defunct                 */
        _nwrite    = 10;                /* always 10 in gadat      */
        _iavtype   = 1;                 /* always 1  in gadat      */
        _ibranch   = 0;                 /* defunct                 */
        _icrate    = 0;                 /* defunct                 */
        _islot     = 0;                 /* defunct                 */
        _ivtype    = 0;                 /* ignored                 */
        _idouble   = 0;                 /* no double precision     */
        _time64    = ZERO(_npts+2,0d0);           /* defunct                 */

        _status = BUILD_CALL(0, '/c/linux/lib/libidl_ga.so', "getallbig_camac_", 
                                _shot, _pointname, REF(_err), REF(_label), REF(_ULAB), _ical,
                                REF(_x), REF(_y), REF(_npts), REF(_TMIN), REF(_TMAX), REF(_XMIN),
                                REF(_XMAX), REF(_IWAIT), REF(_NWRITE), REF(_IAVTYPE),
                                REF(_ibranch), REF(_icrate), REF(_islot), REF(_TIME64),
                                REF(_idouble), REF(_ivtype), REF(_idebug) ); 

        IF ( EQ(_npts,2) ) {
           _x = [0.0];
           _y = [0.0];
           _time64 = [0.0];
           return([0.0]);
        }      
 
        _x = _x[0.._npts-1];
        _y = _y[0.._npts-1];   
        _time64 = [0d0,0d0];

        PUBLIC __pseudo_signal=MAKE_SIGNAL(MAKE_WITH_UNITS(_y, _ulab), *, MAKE_WITH_UNITS(_x, "ms"));
	return(__pseudo_signal);

}

