#!/usr/bin/env python2
#py2 because of ual dependency
import ual
import argparse
import os
import yaml
import logging
import re
from ascii_cpo import read
from datetime import datetime
from io import open




#main
argp = argparse.ArgumentParser(prog="save-to-itmdb.py",
                               description="This script save all CPO data and meaningful description from a simulation into the ITMDB")
argp.add_argument('yml',type=argparse.FileType('r'),nargs='+',
                  help="Yaml description of the simulation to be saved")
#argp.add_argument('-D','--dataDirectory',default=os.getcwd(),
#                  help="Specifies directory containing all simulation data to be saved (default: %(default)s)")
argp.add_argument('-v','--verbosity',default='INFO',
                  help="Specifies the level of verbosity of the logs: critical, error, warning, info or debug (default: %(default)s)")
#argp.add_argument('-l','--logfile',action='store_true',
#                  help="Stores logs into a file instead of printing in console")
args = argp.parse_args()

loglevel = getattr(logging,args.verbosity.upper(),None)
if not isinstance(loglevel, int):
    raise ValueError('Invalid verbosity: %s' % args.verbosity)
logger = logging.getLogger('save-to-itmdb')
logger.setLevel(loglevel)

# create handler
#if args.logfile:
#    ch = logging.FileHandler(args.workDir+'.log',mode='w')
#else:
ch = logging.StreamHandler()
ch.setLevel(loglevel)

# create formatter
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
ch.setFormatter(formatter)
logger.addHandler(ch)



for yml in args.yml:
    fname = yml.name
    if fname=="TEMPLATE.yml":
        continue

    try:
        simulation = yaml.load(yml)

        ######################### CREATE ENTRY IN DATABASE ##########################
        logger.critical("Creating new database entry")
        entry = simulation.get('ENTRY')
        shot = entry.get('SHOT')
        run = entry.get('RUN')
        usr = os.environ['USER']
        tok = entry.get('TOKAMAK')
        ver = entry.get('DATAVERSION')
        # do some checks

        db = ual.itm(shot,run)
        db.create_env(usr,tok,ver)
        # do some checks

        
        ######################## AGGREGATE AND PUT THE CPOS #########################
        logger.critical("Aggregating CPO slices and saving them into the database")
        source = simulation.get('SOURCE')
        datadir = source.get('DIR')
        codes = source.get('CODE')
        
        prevdir = os.getcwd()
        os.chdir(datadir)
        ll = os.listdir('.')
        ll.sort()
        # do some checks

        for code in codes:
            codename = code.get('NAME')
            cponame = code.get('CPO')
            pattern = re.compile(codename+'_'+cponame+'_....\.cpo')

            lcpo = pattern.findall(ll.__str__())
            lcpolen = len(lcpo)

            cpoobj = getattr(db,cponame+'Array')
            cpoobj.resize(lcpolen)
            cpoarr = getattr(cpoobj,'array')

            logger.info("Reading CPO %s from %s",cponame,codename)
            for i in range(lcpolen):
                logger.info("slice #%i",i)
                cpoarr[i] = read(lcpo[i],cponame)

            logger.info("Writing CPO into database")
            getattr(cpoobj,"setExpIdx")(db.expIdx)
            cpoobj.put()


        ########################## STORE PROVENANCE DATA ############################
        logger.critical("Aggregating CPO slices and saving them into the database")
        workflow = simulation.get('WORKFLOW')
        configfile = open(workflow.get('CONFIGURATION'),'r')

        # todo: get real name instead of username
        db.topinfo.dataprovider = usr
        db.topinfo.description = entry.get("DESCRIPTION")
        db.topinfo.firstputdate = datetime.now().strftime('%Y-%m-%d (%Hh%Mm%Ss)')
        db.topinfo.dataversion = ver
        db.topinfo.comment = workflow.get("COMMENT")
        db.topinfo.workflow = configfile.read()

        logger.info("Writing 'topinfo' into database")
        db.topinfo.put()

        db.close()


    except yaml.YAMLError as exc:
        logger.critical(exc)


