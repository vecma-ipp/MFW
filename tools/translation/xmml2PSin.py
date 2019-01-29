#!/usr/bin/python
################################################################################
## This script is part of the ComPat Pattern Services                         ##
## It translates an XMML description of a multiscale model into a set of      ##
## skeleton input files for the Pattern Performance Service                   ##
## @author: olivier.hoenen@ipp.mpg.de                                         ##
################################################################################
from lxml import etree
import argparse

argp = argparse.ArgumentParser(prog="xmml2PSin",
                               description="This program generates inputs skeleton for the Pattern Service of ComPat, given XMML description of the targeted multiscale model.")
argp.add_argument('xmmlfile',type=argparse.FileType('r'),
                  help='XMML file')
argp.add_argument('-b','--benchmark',action='store_true',
                  help="Add placeholder for benchmark section")
argp.add_argument('-u','--usecase',action='store_true',
                  help="Add placeholder for use-case specific section")
argp.add_argument('-m','--muscle',action='store_true',
                  help="Add placeholder for a MUSCLE2 use-case section")
argp.add_argument('-matrix',default='matrix.xml',metavar=('FILE'),
                  help="Specifies output file for describing single scale submodels (default: %(default)s)")
argp.add_argument('-multiscale',default='multiscale.xml',metavar=('FILE'),
                  help="Specifies output file for describing multiscale application (default: %(default)s)")
args = argp.parse_args()



# preamble: get rid of namespace and parse the xmml ############################
file = args.xmmlfile
xmmlns = file.read()
xmml = xmmlns.replace('xmlns="http://www.mapper-project.eu/xmml"','')

parser = etree.XMLParser(remove_blank_text=True)
root = etree.XML(xmml,parser)

mappers = root.findall(".//mapper")
submodels = root.findall(".//submodel")
instances = root.findall("./topology/instance")
couplings = root.findall("./topology/coupling")
subm2inst = { i.values()[1]:i.values()[0] for i in instances}
inst2subm = { i.values()[0]:i.values()[1] for i in instances}



# matrix tree section ###########################################################
mat = etree.Element("matrix")
mod = etree.SubElement(mat,"model",attrib={
    'id':root.attrib['id'],
    'name':root.attrib['name'],
    'xmml_version':root.attrib['xmml_version']})

for s in submodels:
    [s.remove(c) for c in s.getchildren()]
    inst = etree.SubElement(s,"instance",attrib={'id':subm2inst[s.attrib['id']]})
    restr = etree.SubElement(inst,"restrictions")
    cpu = etree.SubElement(restr,"cpu")
    etree.SubElement(cpu,"number").text=''
    etree.SubElement(cpu,"min_cores").text=''
    etree.SubElement(cpu,"max_cores").text=''
    resrc = etree.SubElement(inst,"resources",attrib={'name':''})
    etree.SubElement(resrc,"nodeType").text=''
    etree.SubElement(resrc,"numberOfCores").text=''
    etree.SubElement(resrc,"wallClockTime").text=''
    if args.benchmark:
        bench = etree.SubElement(inst,"benchmark")
        etree.SubElement(bench,"iterations").text=''
        etree.SubElement(bench,"scalability_formula").text=''
    mod.append(s)

for m in mappers:
    [m.remove(c) for c in m.getchildren()]
    mod.append(m)

matrix = etree.ElementTree(mat)
matrix.write(args.matrix,pretty_print=True,xml_declaration=True)



# multiscale tree section #######################################################
mult = etree.Element("multiscale")

info = etree.SubElement(mult,"info")
info.append(etree.Comment(text='Values for "computing" tag is either: ES, HMC or RC'))
etree.SubElement(info,"computing").text='' #should be named pattern?
job = etree.SubElement(info,"job",attrib={'appID':root.attrib['id'],
                                          'project':'compat'})
task = etree.SubElement(job,"task",attrib={'persistent':'true','taskID':'test'})
cores = etree.SubElement(task,"numberofcores")
etree.SubElement(cores,"max").text=''
etree.SubElement(cores,"min").text=''

topo = etree.SubElement(mult,"topology")
for c in couplings:
    c.attrib['to'] = inst2subm[c.attrib['to'].split('.')[0]]
    c.attrib['from'] = inst2subm[c.attrib['from'].split('.')[0]]
    topo.append(c)

inst = etree.SubElement(mult,"instances")
for i in instances:
    inst.append(i)

if args.usecase or args.muscle:
    exe = etree.SubElement(mult,"execution")
    if args.muscle:
        app = etree.SubElement(exe,"application",attrib={'name':'muscle2'})
    else:
        app = etree.SubElement(exe,"application",attrib={'name':''})
    arg = etree.SubElement(exe,"arguments")
    etree.SubElement(arg,"value").text=''
    stage = etree.SubElement(exe,"stageInOut")
    stage.append(etree.Comment(text='Can contain <file> or <directory> tags'))
    env = etree.SubElement(exe,"environment")
    etree.SubElement(env,"module").text=''
    etree.SubElement(exe,"preprocessing").text=''
    etree.SubElement(exe,"postprocessing").text=''
    
multiscale = etree.ElementTree(mult)
multiscale.write(args.multiscale,pretty_print=True,xml_declaration=True,
                 with_comments=True)

