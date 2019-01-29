from . import ual_low_level_wrapper as ull

INTERPOLATION   = 3
CLOSEST_SAMPLE  = 1
PREVIOUS_SAMPLE = 2
EMPTY_INT       = -999999999
EMPTY_FLOAT     = -9.0E40
EMPTY_DOUBLE    = -9.0E40
EMPTY_COMPLEX    = complex(EMPTY_DOUBLE, EMPTY_DOUBLE)
# new defines for struct_array management
NON_TIMED       = 0
TIMED           = 1
TIMED_CLEAR     = 2
# printing level defines, can be changed at runtime
PRINT_DEBUG     = 0
VERBOSE_DEBUG   = 0
DEVEL_DEBUG     = 0

def check_status(status):
	if PRINT_DEBUG:
		if status:
			print (ull.euitm_last_errmsg())

def verb():
	return VERBOSE_DEBUG

def dev():
	return DEVEL_DEBUG
