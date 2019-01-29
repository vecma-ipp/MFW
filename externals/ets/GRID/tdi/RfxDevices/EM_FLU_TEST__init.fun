public fun EM_FLU_TEST__init(as_is _nid, optional _method)
{

    private _K_CONG_NODES = 66;
    private _N_HEAD = 0;
    private _N_COMMENT = 1;

    private _N_POLOIDAL = 2;
    private _N_POL_DECODER_1 = 3;
    private _N_POL_DECODER_2 = 4;
    private _N_POL_CARD_01 = 7;

    private _N_TOROIDAL = 43;
    private _N_TOR_DECODER_1 = 44;
    private _N_TOR_DECODER_2 = 45;
    private _N_TOR_CARD_01 = 48;

    private _K_NUM_CARD = 12;
    private _K_NODES_PER_CARD = 3;
    private _N_CARD_GAIN  = 1;
    private _N_CARD_ADC  = 2;

	
	if( DevIsOn(DevNodeRef(_nid, _N_POLOIDAL)) )
	{
		_pol_dec_1 = if_error(data(DevNodeRef(_nid, _N_POL_DECODER_1)), "");
		if(_pol_dec_1 == "")
		{
			DevLogErr(_nid, "Missing poloidal Decoder 1 path reference"); 
			abort();
		};
	
		_pol_dec_2 = if_error(data(DevNodeRef(_nid, _N_POL_DECODER_2)), "");;
		if(_pol_dec_2 == "")
		{
			DevLogErr(_nid, "Missing poloidal Decoder 2 path reference"); 
			abort();
		};
	}
	
	if( DevIsOn(DevNodeRef(_nid, _N_TOROIDAL)) )
	{
		_tor_dec_1 = if_error(data(DevNodeRef(_nid, _N_TOR_DECODER_1)), "");
		if(_tor_dec_1 == "")
		{
			DevLogErr(_nid, "Missing toroidal Decoder 1 path reference"); 
			abort();
		};
	
		_tor_dec_2 = if_error(data(DevNodeRef(_nid, _N_TOR_DECODER_2)), "");;
		if(_tor_dec_2 == "")
		{
			DevLogErr(_nid, "Missing toroidal Decoder 2 path reference"); 
			abort();
		};
	}	

	if( DevIsOn(DevNodeRef(_nid, _N_POLOIDAL)) )
	{

		_status = tcl("do/method \\POL_AUTOZERO init");
		_status = tcl("do/method \\POL_TRIG init");
		_status = tcl("do/method \\POL_CLOCK init");
		_status = tcl("do/method \\POL_TEST_SIG init");


		for(_i = 0; _i < _K_NUM_CARD; _i++)
		{
			_head_channel = _N_POL_CARD_01 + (_i *  _K_NODES_PER_CARD);

			if( DevIsOn(DevNodeRef(_nid, _head_channel)) )
			{ 

				_adc = if_error(data(DevNodeRef(_nid, _head_channel + _N_CARD_ADC)), "");
				

				if( _adc != "" )
				{
					write(*,"do/method \\"//_adc//" init"); 
					_status = tcl("do/method \\"//_adc//" init");
				}

				_gain = if_error(data(DevNodeRef(_nid, _head_channel + _N_CARD_GAIN)), "");
				
				
				if(_gain != "")
				{
					write(*,"do/method \\"//_gain//" init"); 
					_status = tcl("do/method \\"//_gain//" init");
				}
			}
		}
	}

write(*, "OK");

	if( DevIsOn(DevNodeRef(_nid, _N_TOROIDAL)) )
	{

write(*, "OK1");

		_status = tcl("do/method \\TOR_AUTOZERO init");
		_status = tcl("do/method \\TOR_TRIG init");
		_status = tcl("do/method \\TOR_CLOCK init");
		_status = tcl("do/method \\TOR_TEST_SIG init");


		for(_i = 0; _i < _K_NUM_CARD; _i++)
		{
			_head_channel = _N_TOR_CARD_01 + (_i *  _K_NODES_PER_CARD);

			if( DevIsOn(DevNodeRef(_nid, _head_channel)) )
			{ 

				_adc = if_error(data(DevNodeRef(_nid, _head_channel + _N_CARD_ADC)), "");
				
				if( _adc != "" )
				{
					write(*,"do/method \\"//_adc//" init"); 
					_status = tcl("do/method \\"//_adc//" init");
				}

				_gain = if_error(data(DevNodeRef(_nid, _head_channel + _N_CARD_GAIN)), "");
				
				
				if(_gain != "")
				{
					write(*,"do/method \\"//_gain//" init"); 
					_status = tcl("do/method \\"//_gain//" init");
				}
			}
		}
	}

write(*, "OK3");


	if( DevIsOn(DevNodeRef(_nid, _N_POLOIDAL)) )
	{

write(*, "OK4");

		write(*,"do/method \\"//_pol_dec_1//" init"); 
		_status = tcl("do/method \\"//_pol_dec_1//" init");
		write(*,"do/method \\"//_pol_dec_2//" init"); 
		_status = tcl("do/method \\"//_pol_dec_2//" init");
	}

	if( DevIsOn(DevNodeRef(_nid, _N_TOROIDAL)) )
	{
		write(*,"do/method \\"//_tor_dec_1//" init"); 
		_status = tcl("do/method \\"//_tor_dec_1//" init");
		write(*,"do/method \\"//_tor_dec_2//" init"); 
		_status = tcl("do/method \\"//_tor_dec_2//" init");
	}


	wait(4.0);


	if( DevIsOn(DevNodeRef(_nid, _N_POLOIDAL)) )
	{

		for(_i = 0; _i < _K_NUM_CARD; _i++)
		{
			_head_channel = _N_POL_CARD_01 + (_i *  _K_NODES_PER_CARD);

			if( DevIsOn(DevNodeRef(_nid, _head_channel)) )
			{ 

				_adc = if_error(data(DevNodeRef(_nid, _head_channel + _N_CARD_ADC)), "");
				
				if( _adc != "" )
				{
					write(*,"do/method \\"//_adc//" store"); 
					_status = tcl("do/method \\"//_adc//" store");
				}
			}
		}
	}


	if( DevIsOn(DevNodeRef(_nid, _N_TOROIDAL)) )
	{

		for(_i = 0; _i < _K_NUM_CARD; _i++)
		{
			_head_channel = _N_TOR_CARD_01 + (_i *  _K_NODES_PER_CARD);

			if( DevIsOn(DevNodeRef(_nid, _head_channel)) )
			{ 

				_adc = if_error(data(DevNodeRef(_nid, _head_channel + _N_CARD_ADC)), "");
				
				if( _adc != "" )
				{
					write(*,"do/method \\"//_adc//" store"); 
					_status = tcl("do/method \\"//_adc//" store");
				}
			}
		}
	}

	return (1);
}
