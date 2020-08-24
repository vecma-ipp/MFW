import turb_coeff as tc

def assign_turb_parameters(code_parameters):
    """
    calls the XML parser for the code parameters and assign the
    resulting values to the corresponding variables
    """

    #TODO check if replaceble with xml.etree
    if IMAS == True:
        import imas_xml_parser
    else:
        import euitm_xml_parser

    #TODO check what difference doe it make
    if IMAS == True:
        code_parameters = {}
        return_status = 0
        i, nparm, n_values = 0
    else:
        code_parameters = {}
        return_status = 0
        i, nparm, n_values = 0

    parameter_list = {}

    # initialization
    nparm = 0
    n_values = 0
    return_status = 0

    # parse xml-string code_parameters%parameters using W3C XML schema in code_parameters%schema

    if IMAS == True:
        imas_xml_parse(code_parameters, nparm, parameter_list)
    else:
        euitm_xml_parse(code_parameters, nparm, parameter_list)

    # assign variables

    current = parameter_list.first

    cname = current.cname   # necessary for AIX
    # parameters overall
    if cname == "parameters":
        current = current.child
    # parameter classes
    elif cname == "flags":
        current = current.child
    elif cname == "physical":
        current = current.child
    elif cname == "grid":
        current = current.child
    # individual parameters
    elif cname == "write_cpos":
        if current.cvalue != None:
            write_cpos = current.cvalue
    elif cname == "write_diags":
        if current.cvalue != None:
            write_diags = current.cvalue
    elif cname == "hmode":
        if current.cvalue != None:
            hmode = current.cvalue
    elif cname == "q_choice":
        if current.cvalue != None:
         q_choice = current.cvalue
    elif cname == "thresh":
        if current.cvalue != None:
         thresh = current.cvalue
    elif cname == "beta_reduction":
        if current.cvalue != None:
         beta_reduction = current.cvalue
    elif cname == "etae_pinch":
        if current.cvalue != None:
            etae_pinch = current.cvalue
    elif cname == "chi_d":
        if current.cvalue != None:
            chi_d = current.cvalue
    elif cname == "chiratio_phi":
        if current.cvalue != None:
         chiratio_phi = current.cvalue
    elif cname == "nrho_transp":
        if current.cvalue != None:
            nrho_tranps = current.cvalue
    elif cname == "ra0":
        if current.cvalue != None:
          ra0 = current.cvalue
    elif cname == "nion":
        if current.cvalue != None:
            nion = current.cvalue
    else:
        print('ERROR: invalid parameter {}'.format(cname))
        return_status = 1

#    if associated(current.sibling)) then
#        current => current%sibling
#        exit
#    if (associated(current%parent, parameter_list%first )) &
#        exit outer
#    if (associated(current%parent)) then
#        current => current%parent
#    else
#        write(*, *) 'ERROR: broken list.'
#        return

    return return_status
