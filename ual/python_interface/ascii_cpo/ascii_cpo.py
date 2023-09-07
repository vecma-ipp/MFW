import ual
import re
import sys
import copy
import numpy
from functools import reduce


ASCII_CPO_VERB = False

if (ASCII_CPO_VERB):
    print('welcome in hacked ascii_cpo module')


#
def field2path(field):
    stack = field.split('%')
    stack.__delitem__(0)
    name = stack.pop()
    stack.reverse()
    return stack,name


#
def relpath2path(relpath):
    stack = relpath.split('%')
    name = stack.pop()
    stack.reverse()
    return stack,name


#
def cpo2field(obj,stack):
    while stack!=[]:
        subobj = getattr(obj,stack.pop())
        obj = subobj
    return obj


#
def obj2field(obj,stack):
    while stack!=[]:
        subobj = getattr(obj,stack.pop())
        obj = subobj
    return obj


#
def getfieldtype(obj,name):
    field = getattr(obj,name)
    return field.__class__



#
def parseScalarField(f, parentobj, name):
    next_field = None
    val = (f.readline()).strip()
    obj = getattr(parentobj,name)
    objtype = obj.__class__

    if objtype == str:
        if (ASCII_CPO_VERB):
            print(('   string val = '+val))
        setattr(parentobj,name,val)
    else:
        if objtype == int:
            if (ASCII_CPO_VERB):
                print(('   int val = '+val))
            setattr(parentobj,name,int(val))
        elif objtype == float:
            if (ASCII_CPO_VERB):
                print(('   float val = '+val))
            setattr(parentobj,name,float(val))
        elif objtype == complex:
            if (ASCII_CPO_VERB):
                print(('   complex val = '+val))
            cplxstr = val.replace('(','').replace(')','').split(',')
            setattr(parentobj,name,complex(float(cplxstr[0]),float(cplxstr[1])))
        else:
            print('Scalar type error')
            sys.exit()

#
def parseVectorField(f, parentobj, name, fullpath):
    next_field = None
    ssize = ((f.readline()).strip()).split()
    shape = ()
    for s in ssize:
        shape = shape.__add__((int(s),))
    if len(shape)!=0:
        size = reduce(numpy.multiply,shape)
    else:
        size = 0

    if (ASCII_CPO_VERB):
        print(('   size = '+str(size)))

    obj = getattr(parentobj,name)
    objtype = obj.__class__

    if objtype == str:
        val = []
        for s in range(size):
            val.append((f.readline()).strip())
        if (ASCII_CPO_VERB):
            print(('   string val = '+str(val)))
        setattr(parentobj,name,'\n'.join(val))

    elif objtype == list: # special case for vecstring
        val = []
        for s in range(size):
            val.append((f.readline()).strip())
        if (ASCII_CPO_VERB):
            print(('   string val = '+str(val)))
        setattr(parentobj,name,val)

    elif objtype == numpy.ndarray:
        if obj.dtype == numpy.int32:
            val = numpy.fromfile(f,dtype=numpy.int32,count=size,sep=' ')
        elif obj.dtype == numpy.float64:
            val = numpy.fromfile(f,dtype=numpy.float64,count=size,sep=' ')
        ### reading complex vector is not fast in current implementation! ###
        elif obj.dtype == numpy.complex128:
            val = numpy.ndarray((size), numpy.complex128)
            i=0
            while i<size:
                cstrl = f.readline().strip().split(' ')
                for cstr in cstrl:
                    val[i] = complex(cstr.strip("()").replace(",","+").replace("+-","-")+"j")
                    i=i+1
            #val = numpy.loadtxt(f, dtype=numpy.complex128,count=size,sep='\n',converters={0: lambda x: complex(x.strip("()").replace(",","+").replace("+-","-")+"j")}) ### <== this solution lacks a 'count' parameter
        else:
            print('Numpy Vector error type')
            sys.exit()

        try:
            val = val.reshape(shape,order='F')
        except ValueError:
            sys.exit('Error: during reshape of field '+field.string+
                     ' of value '+ val +
                     ' with shape '+ str(shape))

        if (ASCII_CPO_VERB):
            print(('   ndarray val = '+str(val)))
        setattr(parentobj,name,val)


    #here should lies the struct_arrays cases
    else:
        if (str(objtype).find('ual')==-1):
            print('ERROR: though it should be a struct_array here...')
            print(('objtype = '+str(objtype)))
            sys.exit()
        else:
            if (ASCII_CPO_VERB):
                print('   struct_array')
            obj.resize(size)

        if (ASCII_CPO_VERB):
            print(("   sa_pattern based on name="+fullpath))
        sa_pattern = re.compile(fullpath+"%.+")

        sa_cpt = 0
        sa_first = None
        sa_end = False

        line = (f.readline()).strip()
        field = sa_pattern.match(line)
        if field == None:
            if (ASCII_CPO_VERB):
                print('ERROR: first field does not match struct_array pattern')
                sys.exit()

        # keep in mind the beginning of the structure
        sa_first = field.string
        sa_elt = obj.array[sa_cpt]

        #for s in range(size):
        while not sa_end:
            next_field = None

            #do the dimension parsing
            line = (f.readline()).strip()
            dim = int(line)

            ### EMPTY FIELD ###
            if dim == -1:
                if (ASCII_CPO_VERB):
                    print('     | empty')

            ### EXISTING FIELD ###
            else:
                relpath = field.string.replace(fullpath+'%','')
                if (ASCII_CPO_VERB):
                    print(('     | relpath = '+relpath))
                path,name = relpath2path(relpath)
                if (ASCII_CPO_VERB):
                    print(('     | path='+str(path)+' and name='+name))

                ### SCALAR FIELD ###
                if dim == 0:
                    if (ASCII_CPO_VERB):
                        print('     | scalar')
                    parentobj = obj2field(sa_elt,path)
                    if (ASCII_CPO_VERB):
                        if len(relpath)==1:
                            if (parentobj != sa_elt):
                                sys.exit(str(parentobj)+" != "+str(sa_elt))
                    parseScalarField(f,parentobj,name)

                ### VECTOR FIELD ###
                else:
                    if (ASCII_CPO_VERB):
                        print('     | vector')
                    relpath = field.string.replace(fullpath+'%','')
                    parentobj = obj2field(sa_elt,path)
                    if (ASCII_CPO_VERB):
                        if len(path)==1:
                            if (parentobj != sa_elt):
                                sys.exit(str(parentobj)+" != "+str(sa_elt))
                    next_field = parseVectorField(f,parentobj,name,field.string)


            # prepare next field...
            if (next_field==None):
                line = (f.readline()).strip()
            else:
                line = next_field

            field = sa_pattern.match(line)

            if field == None:
                if (ASCII_CPO_VERB):
                    print(('     |> Field (='+line+') does not match current struct_array pattern (='+fullpath+'%'+'): end of struct_array + save field for next parsing'))
                next_field = line
                sa_end = True

            else:
                if sa_first == field.string:
                    sa_cpt += 1
                    if sa_cpt < size:
                        sa_elt = obj.array[sa_cpt]
                        if (ASCII_CPO_VERB):
                            print('     |> new struct elt')
                    else:
                        sa_end = True
                        if (ASCII_CPO_VERB):
                            print('     |> end of struct_array')
                        next_field = None


        if (ASCII_CPO_VERB):
            print('     |> end of struct_array loop')
        return next_field


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# Implementation of struct_array lacks some beauty at the moment #
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#


##################################################################
# READ ###########################################################
def read(filename,cponame,outcpo=None):

    if outcpo==None:
        itmobj = ual.itm()
        try:
            glob_cpo = getattr(itmobj,cponame)
        except AttributeError:
            sys.exit("Error: no CPO named "+cponame)

        cpo = copy.deepcopy(glob_cpo)
    else:
        if (ASCII_CPO_VERB):
            print('Populates given output cpo')
        cpo = outcpo

    if (ASCII_CPO_VERB):
        print(('Read file '+filename))
    f = open(filename,'r',errors='replace')

    version = f.readline()
    if (ASCII_CPO_VERB):
        print(('Read file ' + filename + ', ' + version))

    cpo = read_fstream(f, cpo, cponame)


def read_fstream(f, cpo, cponame='coreprof'):
    """
    Reads a file stream f into a Python CPO object cpo
    """

    pattern = re.compile(cponame+"%.+")

    end = False

    next_field = None

    while not end:
        if next_field == None:
            if (ASCII_CPO_VERB):
                print('Normal process of next field')
            line = (f.readline()).strip()
        else:
            if (ASCII_CPO_VERB):
                print('Process already parsed next_field')
            line = next_field
            next_field = None

        field = pattern.match(line)

        ### PARSE A CPO FIELD ###
        if field != None:
            if (ASCII_CPO_VERB):
                print(('+ field '+field.string))

            line = (f.readline()).strip()
            dim = int(line)

            ### EMPTY FIELD ###
            if dim == -1:
                if (ASCII_CPO_VERB):
                    print('   empty')

            ### EXISTING FIELD ###
            else:

                path,name = field2path(field.string)
                parentobj = cpo2field(cpo,path)

                ### SCALAR FIELD ###
                if dim == 0:
                    if (ASCII_CPO_VERB):
                        print('   scalar')
                    parseScalarField(f,parentobj,name)

                ### VECTOR FIELD ###
                else:
                    if (ASCII_CPO_VERB):
                        print('   vector')
                    next_field = parseVectorField(f,parentobj,name,field.string)

        ### NOT A VALID CPO FIELD ###
        else:
            if line == cponame:
                print(('Time slice for '+cponame+' CPO'))
                line = (f.readline()).strip()
                if (ASCII_CPO_VERB):
                    print((' - size = '+line))
                line = (f.readline()).strip()
                if (ASCII_CPO_VERB):
                    print((' - shape = '+line))
            else:
                if (ASCII_CPO_VERB):
                    print('End of the CPO')
                end = True

    return cpo;






def writefield(outfile,field):

    if type(field)==str:
        if field=='': # no allocated but empty strings as in Fortran
            outfile.write("\t {:> d} \n".format(-1))
        else:
            outfile.write("\t {:> d} \n".format(1)) # strings are always arrays of 132 char strings (because of Fortran)
            #strlines = (len(field)//133)+1
            splitted = field.split('\n')
            strlines = len(splitted)
            outfile.write("\t {:> d} \n".format(strlines))
            for l in splitted:
                outfile.write("{:s}\n".format(l))
            #for i in range(strlines):
            #    chunk = i*132
            #    outfile.write("{:s}\n".format(field[chunk:chunk+132]))

    elif type(field)==list: # special case of 1D array of strings, implemented through list of strings
        if field==['']: # no allocated but empty strings as in Fortran
            outfile.write("\t {:> d} \n".format(-1))
        else:
            outfile.write("\t {:> d} \n".format(1)) # strings are always arrays of 132 char strings (because of Fortran)
            lstlen = len(field)
            outfile.write("\t {:> d} \n".format(lstlen))
            for i in range(lstlen):
                strlines = (len(field[i])//133)+1
                for j in range(strlines):
                    chunk = j*132
                    outfile.write("{:s}\n".format(field[i][chunk:chunk+132]))


    elif type(field)==int:
        if field==ual.EMPTY_INT:
            outfile.write("\t {:> d} \n".format(-1))
        else:
            outfile.write("\t {:> d} \n".format(0)) # scalar
            outfile.write(" {:> d} \n".format(field))

    elif type(field)==float:
        if field==ual.EMPTY_FLOAT:
            outfile.write("\t {:> d} \n".format(-1))
        else:
            outfile.write("\t {:> d} \n".format(0)) # scalar
            outfile.write(" {:> 19.15E} \n".format(field))

    elif type(field)==numpy.ndarray:
        size = field.size
        if size==0:
            outfile.write("\t {:> d} \n".format(-1))
        else:
            shape = field.shape
            outfile.write("\t {:> d} \n".format(len(shape))) # dim
            for s in shape:
                outfile.write("\t {:< d} ".format(s)) # shape
            printarrays(outfile,field)
            outfile.write("\n")




def printarrays(outfile,arr):
    farr = arr.reshape(arr.size, order='F')
    if arr.dtype==numpy.dtype('int32'):
        formatstr = " {:> d} "
    elif arr.dtype==numpy.dtype('float64'):
        formatstr = " {:> 19.15E} "
    else: # complex?
        print("complex to be implemented")
        sys.exit()

    for i in range(arr.size):
        if not i%3:
            outfile.write("\n")
        outfile.write(formatstr.format(farr[i]))




def explore(outfile,path,field):

    if (not hasattr(field,'base_path')):
        #print("Writing "+path+" field to file")
        outfile.write(" "+path+"\n")
        writefield(outfile,field)

    elif (hasattr(field,'array')):
        #print("Exploring array of structure "+field.base_path)
        outfile.write(" "+path+"\n")
        size = len(field.array)
        if size>0:
            outfile.write("\t {:> d} \n".format(1))   # array of structure are always 1D
            outfile.write("\t {:> d} \n".format(size))
            for elt in field.array:
                explore(outfile,path,elt)
        else:
            outfile.write("\t {:> d} \n".format(-1))

    else:
        #print("Exploring structure "+field.base_path)
        for (name,obj) in field.items():
            if name not in ['base_path','cpoTime','idx','maxOccurrences']:
                explore(outfile,path+'%'+name,obj)




##################################################################
# WRITE ##########################################################
def write(incpo,filename,cponame=None):

    outfile = open(filename,'w')

    outfile.write(" used schema version 4.10b.10\n") #hard-coded as version is not supposed to evolve

    if cponame==None:
        cponame = incpo.base_path

    explore(outfile,cponame,incpo)

    return 0;
