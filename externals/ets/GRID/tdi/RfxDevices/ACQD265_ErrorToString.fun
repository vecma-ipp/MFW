public fun ACQD265_ErrorToString(in _status)
{

	switch(_status)
	{
		case(0) return ("SUCCESS");
		case(1) return ("Multi instrument not defined");
		case(2) return ("Invalid trigger chan specification");
		case(3) return ("Multi instruments not triggered");
		case(-1074116608)  return ("ACQRSD1_ERROR_FILE_NOT_FOUND");
		case(-1074116607)  return ("ACQRSD1_ERROR_PATH_NOT_FOUND");
		case(-1074116605)  return ("ACQRSD1_ERROR_INVALID_HANDLE");
		case(-1074116603)  return ("ACQRSD1_ERROR_NOT_SUPPORTED");
		case(-1074116602)  return ("ACQRSD1_ERROR_INVALID_WINDOWS_PARAM");
		case(-1074116601)  return ("ACQRSD1_ERROR_NO_DATA");
		case(-1074116600)  return ("ACQRSD1_ERROR_NO_ACCESS");
		case(-1074116599)  return ("ACQRSD1_ERROR_BUFFER_OVERFLOW");
		case(-1074116544)  return ("ACQRSD1_ERROR_ALREADY_OPEN");
		case(-1074116480)  return ("ACQRSD1_ERROR_SETUP_NOT_AVAILABLE");
		case(-1074116416)  return ("ACQRSD1_ERROR_INTERNAL_DEVICENO_INVALID");
		case(-1074116415)  return ("ACQRSD1_ERROR_TOO_MANY_DEVICES");
		case(-1074116414)  return ("ACQRSD1_ERROR_EEPROM_DATA_INVALID");
		case(-1074116413)  return ("ACQRSD1_ERROR_INIT_STRING_INVALID");
		case(-1074116412)  return ("ACQRSD1_ERROR_INSTRUMENT_NOT_FOUND");
		case(-1074116411)  return ("ACQRSD1_ERROR_INSTRUMENT_RUNNING");
		case(-1074116410)  return ("ACQRSD1_ERROR_INSTRUMENT_STOPPED");
		case(-1074116409)  return ("ACQRSD1_ERROR_MODULES_NOT_ON_SAME_BUS");
		case(-1074116408)  return ("ACQRSD1_ERROR_NOT_ENOUGH_DEVICES");
		case(-1074116407)  return ("ACQRSD1_ERROR_NO_MASTER_DEVICE");
		case(-1074116352)  return ("ACQRSD1_ERROR_TIMEOUT");
		case(-1074116351)  return ("ACQRSD1_ERROR_OVERLOAD");
		case(-1074115319)  return ("ACQRSD1_ERROR_PARAMETER9");
		case(-1074115318)  return ("ACQRSD1_ERROR_PARAMETER10");
		case(-1074115317)  return ("ACQRSD1_ERROR_PARAMETER11");
		case(-1074115316)  return ("ACQRSD1_ERROR_PARAMETER12");
		case(-1074115315)  return ("ACQRSD1_ERROR_PARAMETER13");
		case(-1074115314)  return ("ACQRSD1_ERROR_PARAMETER14");
		case(-1074115313)  return ("ACQRSD1_ERROR_PARAMETER15");
		case(-1074115584)  return ("ACQRSD1_ERROR_OTHER_WINDOWS_ERROR");
		case(-1074115583)  return ("ACQRSD1_ERROR_UNKNOWN_ERROR");
		case(1073368576)   return ("ACQRSD1_WARN_SETUP_ADAPTED");
		case(-1074003967)  return ("VI_ERROR_PARAMETER1");
		case(-1074003966)  return ("VI_ERROR_PARAMETER2");
		case(-1074003965)  return ("VI_ERROR_PARAMETER3");
		case(-1074003964)  return ("VI_ERROR_PARAMETER4");
		case(-1074003963)  return ("VI_ERROR_PARAMETER5");
		case(-1074003962)  return ("VI_ERROR_PARAMETER6");
		case(-1074003961)  return ("VI_ERROR_PARAMETER7");
		case(-1074003960)  return ("VI_ERROR_PARAMETER8");
		case(-1074003951)  return ("VI_ERROR_FAIL_ID_QUERY");
		case(-1074003950)  return ("VI_ERROR_INV_RESPONSE");
	}

	return ("UNKNOWN ERROR CODE");
}
