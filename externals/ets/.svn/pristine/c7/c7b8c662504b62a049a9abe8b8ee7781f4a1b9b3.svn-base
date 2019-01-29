#! /usr/bin/python
from xml.dom.minidom import parse
import sys
xsd = parse( sys.argv[1] )
print "\\tablefirsthead{\\hline {\\bf Flag} & {\\bf Description} \\\\ \\hline}"
print "\\tablehead{\\hline \\multicolumn{2}{|l|}{\\sl continued} \\\\ \\hline {\\bf Flag} & {\\bf Description} \\\\ \\hline}"
print "\\tabletail{\\hline \\multicolumn{2}{|r|}{\\sl continued} \\\\ \\hline}"
print "\\tablelasttail{\\hline}"
print "\\begin{supertabular}{|l|p{0.7\\columnwidth}|}"
for node in xsd.getElementsByTagName('xs:documentation'):
    for c in node.childNodes:
        print node.parentNode.parentNode.attributes.getNamedItem('name').value,'&',c.wholeText.strip(),'\\\\ \\hline'
print "\\end{supertabular}"
