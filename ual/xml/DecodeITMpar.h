/*
 * DecodeITMpar.h
 *
 *  Created on: Jul 4, 2011
 *      Author: Edmondo Giovannozzi
 *     Company: ENEA
 */
//! @file It defines a class which interprets a codeparam_t field and transform it into a std::string

#ifndef DECODEITMPAR_H_
#define DECODEITMPAR_H_
#include <string>
#include <string.h>

namespace DecITM {
//! @brief Decode a codeparm_t field.
/*!
 * A codeparm_t field is defined as char** in the current 4.09a version. It provide
 * a way to tranform it in a std::string.
 * Overloaded creators will permits the use of existing codes without rewriting it.
 *
 * use:
 *
 * void myactor(....., ItmNs::codeparam_t &codeparam) { // you receive the codeparam_t structure
 *
 * DecITM::DecodeITMpar params(codeparam.parameters); // you call the constructor with a field of codeparm_t
 * 
 *.... = params.get() // you get the parameter as a std::string that you can pass to your preferred xml parser
 *
 * You can use it also if in your test routine you want to provide the actor with a correctly codeparam_t structure:
 *
 * ItmNs::codeparam_t codeparam;
 * std::string parameters;
 *
 *  ... fill the parameters string in some way with you XML paramaters
 *
 * DecITM::DecodeITMpar itmpar(parameters); // pass the string to the constructor
 *
 *   itmpar.encode(codeparam.parameters); // encode it correctly
 *   myactor(....., codeparam);      // use your codeparm structure
 *   deallocateITMpar(codeparam.parameters); // deallocate the codeparam_t structure (only because you used itmpar.encode to allocate it)
 *
 */
class DecodeITMpar {
	std::string parameter; 
public:
	//! @brief Basic constructor from a string
	DecodeITMpar(const std::string & parameter_u): parameter(parameter_u){}

	//! @brief Basic constructor from a C string
	DecodeITMpar(const char * parameter_u): parameter(parameter_u){}

	//! @brief Constructor from a 4.08b codeparam char**
	DecodeITMpar(char ** paremeter_u);

	//! @brief Encode into a 4.08b codeparam char **
	/*!
	 * A null pointer is allocated at the end of the list so that it will easier to deallocate
	 * the full list. Use deallocateITMpar(char **par) to deallocate it.
	 * Don't use on char **par allocated by Kepler (but actually you don't need to)
	 */
	void encode(char ** & par);

	//! @brief Standard encoder into a string
	void encode(std::string & par){par = parameter;}

	//! @brief Getter function
	std::string get() const { return parameter;}
private:
	bool lenpar(const char * par, int & len);
};

//! @brief helper function which deallocate a codeparam_t field (char **). Only if allocated by the DecodeITMpar::encode function
void deallocateITMpar(char **par);

//! @brief helper function which deallocate a codeparam_t field (Actually just clear the string)
void deallocateITMpar(std::string & par);

}

#endif /* DECODEITMPAR_H_ */
