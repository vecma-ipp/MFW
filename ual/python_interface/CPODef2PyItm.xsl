<?xml version="1.0" encoding="UTF-8"?>
<?modxslt-stylesheet type="text/xsl" media="fuffa, screen and $GET[stylesheet]" href="./%24GET%5Bstylesheet%5D" alternate="no" title="Translation using provided stylesheet" charset="ISO-8859-1" ?>
<?modxslt-stylesheet type="text/xsl" media="screen" alternate="no" title="Show raw source of the XML file" charset="ISO-8859-1" ?>
<!-- Matthieu Haefele, IRMA, 2009, Generating  python code calls from CPODef.xml file -->
<!-- Jalal Lakhlili, IPP, 2018, import and print updates for python 3 compatibility -->
<!-- -->


<xsl:stylesheet xmlns:yaslt="http://www.mod-xslt2.com/ns/1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema" version="1.0"
  xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
	xmlns:exsl="http://exslt.org/common"
                extension-element-prefixes="yaslt exsl">

<xsl:output method="text" version="1.0" encoding="UTF-8" indent="yes"/>


<!-- Code generation for the ITM class in itm.py and global definitions in ualdef.py -->
<xsl:template match = "/CPOs">
  <exsl:document href="ual.py" method="text">
    <xsl:text>from . import ual_low_level_wrapper as ull&#xA;</xsl:text>
    <xsl:apply-templates select="CPO" mode="ITM_IMPORT"/>
    <xsl:text>&#xA;class itm:&#xA;</xsl:text>
    <xsl:text>&#009;def __init__(self, s=-1, r=-1, rs=-1, rr=-1):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.shot = s&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.refShot = rs&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.run = r&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.refRun = rr&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.treeName = 'euitm'&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.connected = False&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.expIdx = -1&#xA;</xsl:text>
    <xsl:apply-templates select="CPO" mode="ITM_DECLARE"/>
    
    <xsl:text>&#xA;&#009;def __str__(self, depth=0):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;space = ''&#xA;</xsl:text>
    <xsl:text>&#009;&#009;for i in range(depth):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;space = space + '\t'&#xA;</xsl:text>
    <xsl:text>&#xA;&#009;&#009;ret = space + 'class itm\n'&#xA;</xsl:text>
    <xsl:text>&#009;&#009;ret = ret + space + 'Shot=%d, Run=%d, RefShot%d RefRun=%d\n' % (self.shot, self.run, self.refShot, self.refRun)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;ret = ret + space + 'treeName=%s, connected=%d, expIdx=%d\n' % (self.treeName, self.connected, self.expIdx)&#xA;</xsl:text>
    <xsl:apply-templates select="CPO" mode="ITM_PRINT"/>
    <xsl:text>&#009;&#009;return ret&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def __del__(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if self.expIdx != -1:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;ull.euitm_close(self.expIdx)&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def setShot(self, inShot):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.shot = inShot&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def setRun(self, inRun):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.run = inRun&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def setRefShot(self, inRefShot):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.refShot = inRefShot&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def setRefNum(self, inRefRun):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.refRun = inRefRun&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def setTreeName(self, inTreeName):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.treeName = inTreeName&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def getShot(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;return self.shot&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def getRun(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;return self.run&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def getRefShot(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;return self.refShot&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def getRefRun(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;return self.refRun&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def getTreeName(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;return self.treeName&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def isConnected(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;return self.connected&#xA;</xsl:text>
    
    
    <xsl:text>&#xA;&#009;def create(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;status, idx = ull.euitm_create(self.treeName, self.shot, self.run, </xsl:text>
    <xsl:text>self.refShot, self.refRun)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.__update_cpo(status, idx)&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def create_env(self, user, tokamak, version):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;status, idx = ull.euitm_create_env(self.treeName, self.shot, self.run, </xsl:text>
    <xsl:text>self.refShot, self.refRun, user, tokamak, version)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.__update_cpo(status, idx)&#xA;</xsl:text>
    
    <xsl:text>&#xA;&#009;def create_hdf5(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;status, idx = ull.euitm_create_hdf5(self.treeName, self.shot, self.run, </xsl:text>
    <xsl:text>self.refShot, self.refRun)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.__update_cpo(status, idx)&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def open(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;status, idx = ull.euitm_open(self.treeName, self.shot, self.run)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.__update_cpo(status, idx)&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def open_env(self, user, tokamak, version):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;status, idx = ull.euitm_open_env(self.treeName, self.shot, self.run, </xsl:text>
    <xsl:text>user, tokamak, version)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.__update_cpo(status, idx)&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def open_hdf5(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;status, idx = ull.euitm_open_hdf5(self.treeName, self.shot, self.run)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;self.__update_cpo(status, idx)&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def __update_cpo(self, status, idx):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if status != 0:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;print ('Error opening euitm shot ' + str(self.shot) + ' run ' + str(self.run) + ': ' + ull.euitm_last_errmsg())&#xA;</xsl:text>
    <xsl:text>&#009;&#009;else:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;self.expIdx = idx&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;self.connected = True&#xA;</xsl:text>
    <xsl:apply-templates select="CPO" mode="ITM_SET_IDX"/>
    
    <xsl:text>&#xA;&#009;def close(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if (self.expIdx != -1):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;ull.euitm_close(self.expIdx)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;self.connected = False&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;self.expIdx = -1&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def enableMemCache(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if (self.expIdx != -1):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;ull.euitm_enable_mem_cache(self.expIdx)&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def disableMemCache(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if (self.expIdx != -1):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;ull.euitm_disable_mem_cache(self.expIdx)&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def discardMemCache(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if (self.expIdx != -1):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;ull.euitm_discard_mem_cache(self.expIdx)&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def flushMemCache(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if (self.expIdx != -1):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;ull.euitm_flush_mem_cache(self.expIdx)&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def getTimes(self, path):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if not self.connected:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;return -1,None&#xA;</xsl:text>
    <xsl:text>&#009;&#009;timeList = []&#xA;</xsl:text>
    <xsl:text>&#009;&#009;status, nbslice = ull.beginCPOGet(self.expIdx, path, True)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if status:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;return status,timeList&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if nbslice > 0:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;status,timeList = ull.getVect1DDouble(self.expIdx, path, 'time')&#xA;</xsl:text>
    <xsl:text>&#009;&#009;status2 = ull.endCPOGet(self.expIdx, path)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;return status,timeList&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def getNbSlices(self, path):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;status, timeList = self.getTimes(path)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if not status:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;return len(timeList)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;else:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;return status&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def isCPOExisting(self, path):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;size = self.getNbSlices(path)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if size>0:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;return True&#xA;</xsl:text>
    <xsl:text>&#009;&#009;else:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;return False&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def getAllCPONames(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;itmlist = dir(self)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;cpolist = []&#xA;</xsl:text>
    <xsl:text>&#009;&#009;for l in itmlist:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;t = type(getattr(self, l))&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;if t==types.InstanceType and l.find("Array")==-1:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;&#009;cpolist.append(l)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;return cpolist&#xA;</xsl:text>

    <xsl:text>&#xA;&#009;def getExistingCPONames(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;cpolist = self.getAllCPONames()&#xA;</xsl:text>
    <xsl:text>&#009;&#009;cpoexistlist = []&#xA;</xsl:text>
    <xsl:text>&#009;&#009;for cponame in cpolist:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;cpoinst = getattr(self, cponame)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;maxocc = cpoinst.getMaxOccurrences()&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;if self.isCPOExisting(cponame):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;&#009;cpoexistlist.append(cponame)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;for occ in range(maxocc):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;&#009;cpofullname = cponame+"/"+str(occ+1)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;&#009;if self.isCPOExisting(cpofullname):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;&#009;&#009;cpoexistlist.append(cpofullname)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;return cpoexistlist&#xA;</xsl:text>
    
        
  </exsl:document>
  
  <exsl:document href="ualdef.py" method="text">
    <xsl:text>from . import ual_low_level_wrapper as ull&#xA;</xsl:text>
    <xsl:text>from collections import OrderedDict&#xA;&#xA;</xsl:text>
    <xsl:text>class KeepInOrder(object):&#xA;</xsl:text>
    <xsl:text>&#009;def __new__(cls, *args, **kwargs):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;instance = object.__new__(cls)&#xA;</xsl:text>
    <xsl:text>&#009;&#009;instance.__odict__ = OrderedDict()&#xA;</xsl:text>
    <xsl:text>&#009;&#009;return instance&#xA;&#xA;</xsl:text>
    <xsl:text>&#009;def __setattr__(self, key, value):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if key != '__odict__':&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;self.__odict__[key] = value&#xA;</xsl:text>
    <xsl:text>&#009;&#009;object.__setattr__(self, key, value)&#xA;&#xA;</xsl:text>
    <xsl:text>&#009;def keys(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;return self.__odict__.keys()&#xA;&#xA;</xsl:text>
    <xsl:text>&#009;def iteritems(self):&#xA;</xsl:text>
    <xsl:text>&#009;&#009;return self.__odict__.iteritems()&#xA;&#xA;</xsl:text>
    <xsl:text>&#xA;INTERPOLATION   = 3&#xA;</xsl:text>
    <xsl:text>CLOSEST_SAMPLE  = 1&#xA;</xsl:text>
    <xsl:text>PREVIOUS_SAMPLE = 2&#xA;</xsl:text>    
    <xsl:text>EMPTY_INT       = -999999999&#xA;</xsl:text>
    <xsl:text>EMPTY_FLOAT     = -9.0E40&#xA;</xsl:text>
    <xsl:text>EMPTY_DOUBLE    = -9.0E40&#xA;</xsl:text>
    <xsl:text>EMPTY_COMPLEX    = complex(EMPTY_DOUBLE, EMPTY_DOUBLE)&#xA;</xsl:text>
    <xsl:text># new defines for struct_array management&#xA;</xsl:text>
    <xsl:text>NON_TIMED       = 0&#xA;</xsl:text>
    <xsl:text>TIMED           = 1&#xA;</xsl:text>
    <xsl:text>TIMED_CLEAR     = 2&#xA;</xsl:text>
    <xsl:text># printing level defines, can be changed at runtime&#xA;</xsl:text>
    <xsl:text>PRINT_DEBUG     = 0&#xA;</xsl:text>
    <xsl:text>VERBOSE_DEBUG   = 0&#xA;</xsl:text>
    <xsl:text>DEVEL_DEBUG     = 0&#xA;</xsl:text>
    <xsl:text>&#xA;def check_status(status):&#xA;</xsl:text>
    <xsl:text>&#009;if PRINT_DEBUG:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;if status:&#xA;</xsl:text>
    <xsl:text>&#009;&#009;&#009;print (ull.euitm_last_errmsg())&#xA;</xsl:text>
    <xsl:text>&#xA;def verb():&#xA;</xsl:text>
    <xsl:text>&#009;return VERBOSE_DEBUG&#xA;</xsl:text>
    <xsl:text>&#xA;def dev():&#xA;</xsl:text>
    <xsl:text>&#009;return DEVEL_DEBUG&#xA;</xsl:text>
  </exsl:document>
  
  <exsl:document href="setup.py" method="text">
    <xsl:text>#! /usr/bin/env python&#xA;</xsl:text>
    <xsl:text># System imports&#xA;</xsl:text>
    <xsl:text>from numpy.distutils.core import setup&#xA;&#xA;</xsl:text>
    <xsl:text>all_py_modules = ['ual', 'ualdef', 'ual_low_level_wrapper', '__init__', </xsl:text>
    <xsl:apply-templates select="CPO" mode="DECLARE_LIST"/>
    <xsl:text>]&#xA;&#xA;</xsl:text>
    <xsl:text># ual low level setup&#xA;</xsl:text>
    <xsl:text>setup(name        = "ual",&#xA;</xsl:text>
    <xsl:text>      description = "High level interface for ual with dummy back-end",&#xA;</xsl:text>
    <xsl:text>      author      = "Matthieu Haefele, Olivier Hoenen",&#xA;</xsl:text>
    <xsl:text>      py_modules  = all_py_modules&#xA;</xsl:text>
    <xsl:text>      )&#xA;</xsl:text>
  </exsl:document>
  
  <exsl:document href="__init__.py" method="text">
    <xsl:text>from .ual import *&#xA;</xsl:text>
    <xsl:text>from .ualdef import *&#xA;</xsl:text>
  </exsl:document>
  
</xsl:template>

<xsl:template match = "CPO" mode = "ITM_IMPORT">
  <xsl:text>from . import </xsl:text><xsl:value-of select="@type"/><xsl:text>&#xA;</xsl:text>
</xsl:template>

<xsl:template match = "CPO" mode = "DECLARE_LIST">
  <xsl:text>'</xsl:text><xsl:value-of select="@type"/><xsl:text>',</xsl:text>
</xsl:template>


<xsl:template match = "CPO" mode = "ITM_DECLARE">
  <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@type"/><xsl:text> = </xsl:text>
  <xsl:value-of select="@type"/><xsl:text>.</xsl:text><xsl:value-of select="@type"/><xsl:text>()&#xA;</xsl:text>
  <xsl:choose>
    <xsl:when test = "@timed = 'yes'">
      <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@type"/><xsl:text>Array = </xsl:text>
      <xsl:value-of select="@type"/><xsl:text>.</xsl:text><xsl:value-of select="@type"/><xsl:text>Array()&#xA;</xsl:text>
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match = "CPO" mode = "ITM_PRINT">
  <xsl:text>&#009;&#009;ret = ret + space + 'Attribute </xsl:text><xsl:value-of select="@type"/>
  <xsl:text>\n' + self.</xsl:text><xsl:value-of select="@type"/><xsl:text>.__str__(depth+1)&#xA;</xsl:text>
  <xsl:choose>
    <xsl:when test = "@timed = 'yes'">
      <xsl:text>&#009;&#009;ret = ret + space + 'Attribute </xsl:text><xsl:value-of select="@type"/>
      <xsl:text>Array\n' + self.</xsl:text><xsl:value-of select="@type"/><xsl:text>Array.__str__(depth+1)&#xA;</xsl:text>
    </xsl:when>
  </xsl:choose>
</xsl:template>


<xsl:template match = "CPO" mode = "ITM_SET_IDX">
  <xsl:text>&#009;&#009;&#009;self.</xsl:text><xsl:value-of select="@type"/><xsl:text>.setExpIdx(idx)&#xA;</xsl:text>
  <xsl:choose>
    <xsl:when test = "@timed = 'yes'">
      <xsl:text>&#009;&#009;&#009;self.</xsl:text><xsl:value-of select="@type"/><xsl:text>Array.setExpIdx(idx)&#xA;</xsl:text>
    </xsl:when>
  </xsl:choose>
</xsl:template>


</xsl:stylesheet>
