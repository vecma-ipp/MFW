/*
 * DecodeITMpar.cpp
 *
 *  Created on: Jul 4, 2011
 *      Author: Edmondo Giovannozzi
 *     Company: ENEA
 */
#include "DecodeITMpar.h"

namespace DecITM {
const int LINLEN = 132;
DecodeITMpar::DecodeITMpar(char ** par){
	parameter.clear();
	if (par == 0) {
		return;
	}
	int i = 0;
	int len = 0;
	bool last = false;
	while (1) {
		if (par[i] == 0) break; // but it shouldn't actually happen.
		last = lenpar(par[i], len);
		parameter.append(par[i], len);
		if (last) break;
		i++;
	}
}
void DecodeITMpar::encode(char ** & par){
	const char * str = parameter.c_str();
	int len = parameter.length();
	int nstr = len/LINLEN;
	// One more element is allocated and set to NULL so that it will be easy to deallocate the full
	// array
	if (len % LINLEN > 0) {
		par = new char*[nstr+2];
		par[nstr+1] = 0; // null terminated list for an easy deallocation
	} else {
		par = new char* [nstr+1];
		par[nstr] = 0; // null terminated list for an easy deallocation
	}
	for (int i=0; i<nstr; i++) {
		par[i] = new char[LINLEN];
		strncpy(par[i], &str[i*LINLEN], LINLEN);
	}
	if (len % LINLEN > 0) {
		par[nstr] = new char[LINLEN]; // don't bother, get just one line
		strncpy(par[nstr], &str[nstr*LINLEN], LINLEN);
	}
}
bool DecodeITMpar::lenpar(const char * par, int & len) {
	int i = 0;
	bool flag = false;
	for (i=0; i<LINLEN; i++) {
		if (par[i] == '\0') {
			flag = true;
			len = i + 1;
			break;
		}
	}
	if (! flag) len = LINLEN;
	return flag;
}
void deallocateITMpar(char **par){
	if (par == 0) return;

	for (int i=0; par[i]!=0; i++) {
		delete [] par[i];
	}
	delete [] par;
}
void deallocateITMpar(std::string & par) {
	par.clear();
}


}

