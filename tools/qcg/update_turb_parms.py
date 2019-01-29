#!/usr/bin/python
import os
import xml.etree.ElementTree as ET

turb_PEs = int(os.environ["QCG_KERNEL_turb"])

tree = ET.parse("gem.xml")
root = tree.getroot()

npesx = root.find(".//npesx")
npess = root.find(".//npess")
nftubes = int(root.find(".//nftubes").text)
nx00 = int(root.find(".//nx00").text)
ns00 = int(root.find(".//ns00").text)

nss = min(turb_PEs/int(nftubes),ns00)
nsx = turb_PEs/(int(nftubes)*nss)

npess.text = str(nss)
npesx.text = str(nsx)

tree.write("gem.xml")
