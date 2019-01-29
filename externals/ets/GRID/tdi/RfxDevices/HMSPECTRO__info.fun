public fun HMSPECTRO__info(as_is _nid, optional _method)
{
    private _K_CONG_NODES = 16;
    private _N_HEAD = 0;
    private _N_NAME = 1;
    private _N_SW_MODE = 2;
    private _N_IP_ADDR = 3;
    private _N_COMMENT = 4;
    private _N_TYPE = 5;
    private _N_TRIG_MODE = 6;
    private _N_TRIG_EDGE = 7;
    private _N_TRIG_SOURCE = 8;
    private _N_NUM_SCAN = 9;
    private _N_GAIN = 10;
    private _N_INTEG_TIME = 11;
    private _N_CALIBRATION = 12;
    private _N_DATA = 13;
 
    private _INVALID = -1 ;
	
    private _HMSPECTRO_SUCCESS = 0;
	private _HMSPECTRO_INVALID_BUFFER_ALLOCATION = 9004;
    private _HMSPECTRO_NOT_ACQUIRED_ALL_FRAME = 9005;

write(*, "HMSPECTRO__info");

	_DevicesType = ['C9404MC', 'C9405MC', 'C9404GC', 'C9913GC', 'C9914GB', 'C10082MD', 'C10083MD', 'C9404CA', 'C9404CAH', 'C9405CA', 'C10082CA', 'C10083CA', 'C10083CA', 'C10082CAH', 'C10083CAH' ]; 
	_DevicesCode = [ 0x2905,    0x2905,    0x2905,    0x2907,    0x2907,    0x2908,     0x2908,     0x290D,    0x290D,     0x290D,    0x2909,     0x2909,     0x2909,      0x2909,     0x2909];
	_DevicesPixel= [ 512,       512,       512,       512,       256,       1024,       1024,       1024,      1024,       1024,      2048,       2048,       2048,        2048,       2048];
	_GainSwitchDevice = ['C9405MC', 'C9404CA', 'C9404CAH', 'C9405CA', 'C10082CA', 'C10083CA', 'C10083CA', 'C10082CAH', 'C10082CAH' ];

    _dev_name = if_error(data(DevNodeRef(_nid, _N_NAME)), "");
    if( _dev_name == "" )
    {
    	DevLogErr(_nid, "Invalid deice name");
 		abort();
    }

write(*, "1 _dev_name ", _dev_name);

    DevNodeCvt(_nid, _N_SW_MODE, ['LOCAL', 'REMOTE'], [0,1], _remote = 0);
	if(_remote != 0)
	{
		_ip_addr = if_error(data(DevNodeRef(_nid, _N_IP_ADDR)), "");
		if(_ip_addr == "")
		{
    	    DevLogErr(_nid, "Invalid Crate IP specification");
 		    abort();
		}
	}

	if(_remote != 0)
	{
		_cmd = 'MdsConnect("'//_ip_addr//'")';
		execute(_cmd);
	    _info = MdsValue('HMSpectroInfo( $1 )', _dev_name);


		_status = _HMSPECTRO_SUCCESS;
		if(len( _info ) == 4)
			_status = _info[0];

		if( _status != _HMSPECTRO_SUCCESS )
		{
			_msg = MdsValue('HMSPECTROGetMsg( $1 )', _status );
		}
		MdsDisconnect();
	}
	else
	{
		_info = repeat(' ', 4096);
		_status = HMSPECTRO->HMSpectroInfo( _dev_name, ref( _info ), val( 4096 ) );

		if( _status != _HMSPECTRO_SUCCESS )
		{
			_msg = repeat(' ', 200);
			HMSPECTRO->HMSpectroGetMsg( val(_status), ref( _msg ) );
		}
	}


	if( _status != _HMSPECTRO_SUCCESS )
	{
    	DevLogErr(_nid, _msg );	
		abort();
	}

	write(*,  _info  );

	return (1);

}
