/* $Id: DevPutValue.fun,v 1.2 2004/08/09 17:07:27 twf Exp $ */

public fun DevPutValue(in _nid, in _value, optional in _units )

/*
 *  TDI function to store a value (with optional units) into a node
 *
 *  _nid must be the node id to hold the value
 *	(use DevHead() to get value)
 */

{

  if ( not present(_units) ) 
    _val_build = _value ;
  else
    _val_build = compile( 'build_with_units( `_value, `_units )' );
  
/*
 * The is_vms() function seems to no longer be in the Linux distribution
 *	so this is commented out for Linux.
 *
  if (is_vms())
    return( TreeShr->Tree$Put_Record( _nid, xd(_val_build) ) );
  else
 */

  return( TreeShr->TreePutRecord(val(_nid),xd(_val_build),val(0)) );

}
