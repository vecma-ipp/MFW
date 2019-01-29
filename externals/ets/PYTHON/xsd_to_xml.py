#! /usr/bin/python
from xml.dom.minidom import parse
import sys
f = sys.argv[1]
xsd = parse( f )
l = f.rfind('/')
if l > 1:
  f=f[l+1:]
n = f.replace('.','_')
print '<section class="topic" id="imp3_'+n+'">'
print '<title> ',f,' </title>'
print '<para> Documentation for ',f,'</para>'
print '<informaltable frame="all">'
print '  <tgroup cols="2">'
print '    <colspec colnum="1" colname="c1" colwidth="20mm"/>'
print '    <colspec colnum="2" colname="c2" colwidth="120mm"/>'
print '    <thead>'
print '      <row>'
print '        <entry valign="middle">     Flag                 </entry>'
print '        <entry valign="middle">     Description          </entry>'
print '      </row>'
print '    </thead>'
print '    <tbody>'
for node in xsd.getElementsByTagName('xs:documentation'):
  for c in node.childNodes:
    print '      <row>'
    print '         <entry> ',node.parentNode.parentNode.attributes.getNamedItem('name').value,'</entry>'
    print '         <entry> ',c.wholeText.strip(),'</entry>'
    print '      </row>'

print '</tbody></tgroup>'
print '</informaltable>'
print '</section>'
