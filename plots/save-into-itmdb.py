import ual
import argparse
import os
import yaml
import logging






#main
argp = argparse.ArgumentParser(prog="save-to-itmdb.py",
                               description="This script save all CPO data and meaningful description from a simulation into the ITMDB")
argp.add_argument('yml',type=argparse.FileType('r'),nargs='+',
                  help="Yaml description of the simulation to be saved")
argp.add_argument('-D','--dataDirectory',default=os.getcwd(),
                  help="Specifies directory containing all simulation data to be saved (default: %(default)s)")
argp.add_argument('-v','--verbosity',default='ERROR',
                  help="Specifies the level of verbosity of the logs: critical, error, warning, info or debug (default: %(default)s)")
argp.add_argument('-l','--logfile',action='store_true',
                  help="Stores logs into a file instead of printing in console")
args = argp.parse_args()

loglevel = getattr(logging,args.verbosity.upper(),None)
if not isinstance(loglevel, int):
    raise ValueError('Invalid verbosity: %s' % args.verbosity)
logger = logging.getLogger('save-to-itmdb')
logger.setLevel(loglevel)

# create handler
if args.logfile:
    ch = logging.FileHandler(args.workDir+'.log',mode='w')
else:
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
        desc = yaml.load(yml)

    except yaml.YAMLError as exc:
        logger.critical(exc)


