import ual
import ascii_cpo
import os
import re




def get_cpo_from_dir(path, codename, cponame, time_pattern):
    """
    Read CPO files corresponding to pattern in order to build and return a time serie.
    \nParameters
    ----------
    path : str
        Path to the directory containing CPO files
    codename : str
        Name of the code which produced the CPO
    cponame : str
        Name of the CPO
    time_pattern : str
        Pattern to match the time slice part of the cpo file 
        e.g: 'codename_cponame_timeslice\.cpo' where timeslice='0...' for slices between 0 and 999
    \nReturns
    -------
    cpo_list : 
        CPO array
    """
    db = ual.itm()

    prevdir = os.getcwd()
    os.chdir(path)
    ll = os.listdir('.')
    ll.sort()

    pattern = re.compile(codename+'_'+cponame+'_'+time_pattern+'\.cpo')
    lcpo = pattern.findall(ll.__str__())
    lcpolen = len(lcpo)

    cpoobj = getattr(db,cponame+'Array')
    cpoobj.resize(lcpolen)
    cpoarr = getattr(cpoobj,'array')      
    for i in range(lcpolen):
        cpoarr[i] = ascii_cpo.read(lcpo[i],cponame)

    os.chdir(prevdir)
    return cpoobj




def build_time_trace(paths, cpo_producer=[('ets','coreprof'),('chease','equilibrium'),('imp4dv','coretransp')], db=None):
    """
    Build the whole time trace of a simulation by reading single CPO slices possibly \n
    scattered in different directories.
    \nParameters
    ----------
    db : ual.itm instance, optional 
        Instance of ual.itm which will be storing all time slices for all CPOs.
        If None, the returned db will not be link to any opened database.
    paths : list(str)
        List of directories in which the data are stored. Order matters! (most recent last)
    cpo_producer : list((str,str)), optional
        List tuples of (codename,cponame) to specify which code produced which CPO.
    \nReturns
    -------
    db 
        ual.itm instance filled with all CPOs
    """
    if db==None:
        db = ual.itm()

    for cc in cpo_producer:
        cpoobj = getattr(db,cc[1]+'Array')
        cpoarr = getattr(cpoobj,'array')
        for p in paths:
            cpo = get_cpo_from_dir(p, codename=cc[0], cponame=cc[1], time_pattern='....')
            for cposlice in cpo:
                cpoarr.append(cposlice)

        if db.isConnected():
            getattr(cpoobj,'setExpIdx')(db.expIdx)

    return db




if __name__ == "__main__":
    import argparse
    print("Not usable as a script (yet?)")
    


