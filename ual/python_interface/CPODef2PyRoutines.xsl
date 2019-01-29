<?xml version="1.0" encoding="UTF-8"?>
<?modxslt-stylesheet type="text/xsl" media="fuffa, screen and $GET[stylesheet]" href="./%24GET%5Bstylesheet%5D" alternate="no" title="Translation using provided stylesheet" charset="ISO-8859-1" ?>
<?modxslt-stylesheet type="text/xsl" media="screen" alternate="no" title="Show raw source of the XML file" charset="ISO-8859-1" ?>
<!-- Matthieu Haefele, IRMA, 2009, Generating  python code for the ual high
level interface from CPODef.xml file -->
<!-- Olivier Hoenen, IRMA, 2010, several updates and add of array of structures
support                          -->
<!-- Tomasz Żok, PSNC, 2012, moved inner structures from external file into the
nested class -->
<!-- Jalal Lakhlili, IPP, 2018, import and print updates for python 3 compatibility -->
<!-- -->


<xsl:stylesheet xmlns:yaslt="http://www.mod-xslt2.com/ns/1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" version="1.0"
    xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
    xmlns:exsl="http://exslt.org/common"
    xmlns:func="http://exslt.org/functions"
    xmlns:my="http://localhost.localdomain/localns"
    exclude-result-prefixes="my"
    extension-element-prefixes="yaslt exsl func">

    <xsl:output method="text" version="1.0" encoding="UTF-8" indent="yes"/>


    <!-- For each CPO : 1 file is generated named cponame.py -->
    <!-- The cponame.py file contains the cpo class and the cpoArray class and
    all the class (structures) used as field by the cpo  -->

    <!-- Main template which calls the two sub-templates. Each sub-template
    generate a file -->
    <xsl:template match = "/CPOs">
        <xsl:apply-templates select="CPO"/>
    </xsl:template>


    <!-- Template which generate cponame.py file -->
    <xsl:template match="CPO">
        <exsl:document href="{@type}.py" method="text">
            <xsl:text># -*- coding: utf-8 -*-&#xA;</xsl:text>
            <xsl:text>from .ualdef import *&#xA;</xsl:text>
            <xsl:text>from . import ual_low_level_wrapper as ull&#xA;</xsl:text>
            <xsl:text>import numpy&#xA;&#xA;</xsl:text>

            <xsl:text>class </xsl:text><xsl:value-of select="@type"/><xsl:text>:&#xA;</xsl:text>
            <xsl:text>&#009;'''&#xA;</xsl:text>
            <xsl:text>&#009;class </xsl:text><xsl:value-of select="@type"/><xsl:text>&#xA;</xsl:text>
            <xsl:text>&#009;</xsl:text><xsl:value-of select="@documentation"/><xsl:text>&#xA;&#xA;</xsl:text>
            <xsl:text>&#009;Attributes:&#xA;</xsl:text>
            <xsl:apply-templates select="field" mode="DOC"/>
            <xsl:text>&#009;'''&#xA;</xsl:text>

            <xsl:text>&#xA;&#009;def __init__(self):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;self.base_path = '</xsl:text><xsl:value-of select="@type"/><xsl:text>'&#xA;</xsl:text>
            <xsl:text>&#009;&#009;self.idx = EMPTY_INT&#xA;</xsl:text>
	    <xsl:text>&#009;&#009;self.cpoTime = EMPTY_DOUBLE&#xA;</xsl:text>
	    <xsl:text>&#009;&#009;self.maxOccurrences = </xsl:text><xsl:value-of select="@maxoccur"/><xsl:text>&#xA;</xsl:text>
            <xsl:apply-templates select="field" mode="DECLARE">
                <xsl:with-param name="processing" select="'main'"/>
            </xsl:apply-templates>

            <xsl:text>&#xA;&#009;def __str__(self, depth=0):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;space = depth*'\t'&#xA;</xsl:text>
            <xsl:text>&#009;&#009;ret = space + 'class </xsl:text><xsl:value-of select="@type"/><xsl:text>\n'&#xA;</xsl:text>
            <xsl:apply-templates select="field" mode="PRINT"/>
            <xsl:text>&#009;&#009;return ret&#xA;</xsl:text>

            <xsl:text>&#xA;&#009;def setExpIdx(self, idx):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;self.idx = idx&#xA;</xsl:text>
            <xsl:apply-templates select="field[@type='structure' or @type='struct_array']" mode="SETEXPIDX"/>

            <xsl:text>&#xA;&#009;def getMaxOccurrences(self):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;return self.maxOccurrences&#xA;</xsl:text>
	    
            <xsl:text>&#xA;&#009;def getCPOName(self):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;return self.base_path&#xA;</xsl:text>

            <!-- If the CPO is not timed, only the get and put methods are
            available -->
            <xsl:choose>
                <xsl:when test = "@timed = 'no'">
                    <xsl:text>&#xA;&#009;def put(self, occurrence=0):&#xA;</xsl:text>
                    <xsl:call-template name="COMPUTE_PATH">
                        <xsl:with-param name="processing" select="'main'"/>
                    </xsl:call-template>
                    <xsl:text>&#009;&#009;status = ull.beginCPOPut(self.idx, path)&#xA;</xsl:text>
                    <xsl:apply-templates select="field" mode="PUT"/>
                    <xsl:text>&#009;&#009;status = ull.endCPOPut(self.idx, path)&#xA;&#xA;</xsl:text>

                    <xsl:text>&#xA;&#009;def get(self, occurrence=0):&#xA;</xsl:text>
                    <xsl:call-template name="COMPUTE_PATH">
                        <xsl:with-param name="processing" select="'main'"/>
                    </xsl:call-template>
                    <xsl:text>&#009;&#009;status, nbslice = ull.beginCPOGet(self.idx, path, NON_TIMED)&#xA;</xsl:text>
                    <xsl:apply-templates select="field" mode="GET"/>
                    <xsl:text>&#009;&#009;status = ull.endCPOGet(self.idx, path)&#xA;&#xA;</xsl:text>
                </xsl:when>

                <!-- If the CPO is timed, get and put methods are not available
                but putSlice, putNonTimed, and getSlice are available-->
                <xsl:otherwise>
                    <xsl:text>&#xA;&#009;def putSlice(self, occurrence=0):&#xA;</xsl:text>
                    <xsl:call-template name="COMPUTE_PATH">
                        <xsl:with-param name="processing" select="'main'"/>
                    </xsl:call-template>
		    <xsl:text>&#009;&#009;self.cpoTime = self.time&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;status = ull.beginCPOPutSlice(self.idx, path)&#xA;</xsl:text>
                    <xsl:apply-templates select="field" mode="PUT_TIME">
                        <xsl:with-param name="action" select="'put'"/>
                        <xsl:with-param name="method" select="'slice'"/>
                    </xsl:apply-templates> 
                    <xsl:text>&#009;&#009;status = ull.endCPOPutSlice(self.idx, path)&#xA;</xsl:text>

                    <xsl:text>&#xA;&#009;def replaceLastSlice(self, occurrence=0):&#xA;</xsl:text>
                    <xsl:call-template name="COMPUTE_PATH">
                        <xsl:with-param name="processing" select="'main'"/>
                    </xsl:call-template>
		        <xsl:text>&#009;&#009;self.cpoTime = self.time&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;status = ull.beginCPOReplaceLastSlice(self.idx, path)&#xA;</xsl:text>
                    <xsl:apply-templates select="field" mode="PUT_TIME">
                        <xsl:with-param name="action" select="'replaceLast'"/>
                        <xsl:with-param name="method" select="'slice'"/>
                    </xsl:apply-templates>
                    <xsl:text>&#009;&#009;status = ull.endCPOReplaceLastSlice(self.idx, path)&#xA;</xsl:text>		

                    <xsl:text>&#xA;&#009;def putNonTimed(self, occurrence=0):&#xA;</xsl:text>
                    <xsl:call-template name="COMPUTE_PATH">
                        <xsl:with-param name="processing" select="'main'"/>
                    </xsl:call-template>
                    <xsl:text>&#009;&#009;self.deleteData(occurrence)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;status = ull.beginCPOPutNonTimed(self.idx, path)&#xA;</xsl:text>
                    <xsl:apply-templates select="field" mode="PUT_TIME">
                        <xsl:with-param name="action" select="'put'"/>
                        <xsl:with-param name="method" select="'non-timed'"/>
                    </xsl:apply-templates> 
                    <xsl:text>&#009;&#009;status = ull.endCPOPutNonTimed(self.idx, path)&#xA;</xsl:text>

                    <xsl:text>&#xA;&#009;def getSlice(self, inTime, interpolMode, occurrence=0):&#xA;</xsl:text>
                    <xsl:call-template name="COMPUTE_PATH">
                        <xsl:with-param name="processing" select="'main'"/>
                    </xsl:call-template>
		        <xsl:text>&#009;&#009;self.cpoTime = self.time&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;status = ull.beginCPOGetSlice(self.idx, path, inTime)&#xA;</xsl:text>
                    <xsl:apply-templates select="field" mode="GET_SLICE"/>
                    <xsl:text>&#009;&#009;status = ull.endCPOGetSlice(self.idx, path)&#xA;</xsl:text>

                    <!-- build_non_resampled_data method is not part of the API
                    of the CPO normally. But it is this method who buids the
                    array of CPO needed by cpoArray::get class -->
                    <xsl:text>&#xA;&#009;def build_non_resampled_data(self, occurrence=0):&#xA;</xsl:text>
                    <xsl:call-template name="COMPUTE_PATH">
                        <xsl:with-param name="processing" select="'main'"/>
                    </xsl:call-template>
                    <xsl:text>&#009;&#009;status, nbslice = ull.beginCPOGet(self.idx, path, True)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;array=None&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;if nbslice > 0:&#xA;</xsl:text>
                    <xsl:apply-templates select="field" mode="GET_BUILD">
                        <xsl:with-param name="processing" select="''"/>
                    </xsl:apply-templates>
                    <xsl:text>&#009;&#009;&#009;array = []&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;for i in range(nbslice):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;slice = </xsl:text><xsl:value-of select="@type"/><xsl:text>()&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;slice.setExpIdx(self.idx)&#xA;</xsl:text>
                    <xsl:apply-templates select="field" mode="GET_AFFECT"/>
                    <xsl:text>&#009;&#009;&#009;&#009;array.append(slice)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;else:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;print ('error, nbslice must be > 0 and got:' + str(nbslice))&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;status = ull.endCPOGet(self.idx, path)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;return array&#xA;</xsl:text>

                    <xsl:text>&#xA;&#009;def deleteData(self, occurrence=0):&#xA;</xsl:text>
                    <xsl:call-template name="COMPUTE_PATH">
                        <xsl:with-param name="processing" select="'main'"/>
                    </xsl:call-template>
                    <xsl:apply-templates select="field" mode="DELETE"/>

                    <!-- Class cpoArray definition -->
                    <xsl:text>&#xA;&#xA;class </xsl:text><xsl:value-of select="@type"/><xsl:text>Array:&#xA;</xsl:text>
                    <xsl:text>&#009;'''&#xA;</xsl:text>
                    <xsl:text>&#009;class </xsl:text><xsl:value-of select="@type"/><xsl:text>Array&#xA;</xsl:text>
                    <xsl:text>&#009;</xsl:text><xsl:value-of select="@documentation"/><xsl:text>&#xA;&#xA;</xsl:text>
                    <xsl:text>&#009;Attributes:&#xA;</xsl:text>
                    <xsl:text>&#009;- array : list of </xsl:text><xsl:value-of select="@type"/><xsl:text>&#xA;</xsl:text>
                    <xsl:text>&#009;   Each list element correspond to one time slice.&#xA;</xsl:text>
                    <xsl:text>&#009;'''&#xA;&#xA;</xsl:text>

                    <xsl:text>&#009;def __init__(self):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;self.array = []&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;self.idx = EMPTY_INT&#xA;</xsl:text>

                    <xsl:text>&#009;def __getitem__(self, key):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;return self.array[key]&#xA;</xsl:text>

                    <xsl:text>&#009;def __len__(self):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;return len(self.array)&#xA;</xsl:text>

                    <xsl:text>&#009;def __iter__(self):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;return self.array.__iter__()&#xA;</xsl:text>

                    <xsl:text>&#xA;&#009;def __str__(self, depth=0):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;space = depth*'\t'&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;ret = space + 'class </xsl:text><xsl:value-of select="@type"/>
                    <xsl:text>Array nb_cpos=%d\n' %(len(self.array))&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;for i in range(len(self.array)):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;ret = ret + space + '</xsl:text><xsl:value-of select="@type"/>
                    <xsl:text> cpos=%d\n' %(i) + self.array[i].__str__(depth+1)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;return ret&#xA;</xsl:text>

                    <xsl:text>&#xA;&#009;def setExpIdx(self, idx):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;self.idx = idx&#xA;</xsl:text>
		    <xsl:text>&#009;&#009;for i in range(len(self.array)):&#xA;</xsl:text>
        	    <xsl:text>&#009;&#009;&#009;self.array[i].setExpIdx(idx)&#xA;</xsl:text>

                    <xsl:text>&#xA;&#009;def put(self, occurrence=0):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;if (len(self.array)>0):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;self.array[0].putNonTimed(occurrence)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;for i in self.array:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;i.putSlice(occurrence)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;else:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;print('no time slice to be put')&#xA;</xsl:text>

                    <xsl:text>&#xA;&#009;def get(self, occurrence=0):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;cpo = </xsl:text><xsl:value-of select="@type"/><xsl:text>()&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;cpo.setExpIdx(self.idx)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;self.array = cpo.build_non_resampled_data(occurrence)&#xA;</xsl:text>

                    <xsl:text>&#xA;&#009;def resize(self, nb_cpos):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;self.array = []&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;for i in range(nb_cpos):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;self.array.append(</xsl:text><xsl:value-of select="@type"/><xsl:text>())&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;self.array[i].setExpIdx(self.idx)&#xA;</xsl:text>

                    <xsl:text>&#xA;&#009;def deleteAll(self, occurrence=0):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;cpo = </xsl:text><xsl:value-of select="@type"/><xsl:text>()&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;cpo.setExpIdx(self.idx)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;cpo.deleteData(occurrence)&#xA;</xsl:text>
                </xsl:otherwise>
            </xsl:choose>	 

            <xsl:apply-templates select=".//field[@type='structure' or @type='struct_array']"/>
        </exsl:document>
    </xsl:template>


    <xsl:template match="field[@type='structure' or @type='struct_array']">
        <xsl:variable name="this-name" select="@name"/>
        <xsl:variable name="this-type" select="@type"/>
        <xsl:variable name="this-type-name" select="@type-name"/>
        <xsl:variable name="this-cpo-type" select="ancestor::CPO/@type"/>

        <xsl:if test="not (preceding::field[@name=$this-name and @type=$this-type and @type-name=$this-type-name and ancestor::CPO/@type=$this-cpo-type])">
            <xsl:apply-templates select="." mode="CLASS"/>
        </xsl:if>
    </xsl:template>


    <!-- Class generation for struct_array case:
    'type-name'Obj class is set for the structure definition, with specific profile 
    'type-name' class is set for the array of 'type-name'Obj, with classic profile -->
    <!-- DOIT simple mode stands for not already defined class, and nested parameter is 
    set to 'yes' if the struct_array is defined inside an other struct_array       -->
    <xsl:template match="field[@type='struct_array']" mode="CLASS">
        <!-- array class part -->
        <xsl:text>&#xA;&#xA;class </xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>:&#xA;</xsl:text>
        <xsl:text>&#009;'''&#xA;</xsl:text>
        <xsl:text>&#009;class </xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>&#xA;</xsl:text>
        <xsl:text>&#009;</xsl:text><xsl:value-of select="@documentation"/><xsl:text>&#xA;&#xA;</xsl:text>
        <xsl:text>&#009;Attributes:&#xA;</xsl:text>
        <xsl:text>&#009;- array : list of </xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>Obj &#xA;</xsl:text>
        <xsl:text>&#009;'''&#xA;&#xA;</xsl:text>

        <xsl:text>&#009;def __init__(self, base_path_in='</xsl:text><xsl:value-of select="@name"/><xsl:text>'):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.base_path = base_path_in&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.idx = EMPTY_INT&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.cpoTime = EMPTY_DOUBLE&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.array = []&#xA;</xsl:text>

        <xsl:text>&#009;def __getitem__(self, key):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;return self.array[key]&#xA;</xsl:text>

        <xsl:text>&#009;def __len__(self):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;return len(self.array)&#xA;</xsl:text>

        <xsl:text>&#009;def __iter__(self):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;return self.array.__iter__()&#xA;</xsl:text>

        <xsl:text>&#xA;&#009;def __str__(self, depth=0):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;space = depth*'\t'&#xA;</xsl:text>
        <xsl:text>&#009;&#009;ret = space + 'class </xsl:text><xsl:value-of select = "@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>\n'&#xA;</xsl:text>
        <xsl:text>&#009;&#009;for i in range(len(self.array)):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;ret = ret + space + '</xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/>
        <xsl:text>[%d] = \n' %(i) + self.array[i].__str__(depth+1)&#xA;</xsl:text>
        <xsl:text>&#009;&#009;return ret&#xA;</xsl:text>		

        <xsl:text>&#xA;&#009;def setExpIdx(self, idx):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.idx = idx&#xA;</xsl:text>
        <xsl:text>&#009;&#009;for i in range(len(self.array)):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;self.array[i].setExpIdx(idx)&#xA;</xsl:text>

        <xsl:text>&#xA;&#009;def resize(self, nbelt):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.array = []&#xA;</xsl:text>
        <xsl:text>&#009;&#009;for i in range(nbelt):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;self.array.append(</xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>Obj(self.base_path))&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;self.array[i].setExpIdx(self.idx)&#xA;</xsl:text>

        <!-- methods for non-timed CPOs case -->
        <xsl:if test="ancestor::CPO/@timed='no'">
            <xsl:text>&#xA;&#009;def put(self, path, cpopath):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'put'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'object'"/>
            </xsl:call-template>
            <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')&#xA;</xsl:text>
            <xsl:text>&#009;&#009;obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;for i in range(len(self.array)):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')&#xA;</xsl:text>
            <xsl:text>&#009;&#009;status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>

            <xsl:text>&#xA;&#009;def get(self, path, cpopath):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'get'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'object'"/>
            </xsl:call-template>
            <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')&#xA;</xsl:text>
            <xsl:text>&#009;&#009;status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;if status:&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;return&#xA;</xsl:text>
            <xsl:text>&#009;&#009;obj_size = ull.getObjectDim(self.idx, obj)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;self.resize(obj_size)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;for i in range(obj_size):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;self.array[i].getNonTimedElt(path, self.base_path, i, obj)</xsl:text>
        </xsl:if>

        <!-- methods for timed CPOs case -->
        <xsl:if test="ancestor::CPO/@timed='yes'">
            <xsl:text>&#xA;&#009;def putSlice(self, path, cpopath):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'putSlice'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'object'"/>
            </xsl:call-template>
            <xsl:if test="@timed='yes'">
                <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', TIMED)')&#xA;</xsl:text>
                <xsl:text>&#009;&#009;obj_time = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, TIMED)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;obj = ull.beginObject(self.idx, obj_time, 0, 'ALLTIMES', TIMED)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;for i in range(len(self.array)):&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;obj = self.array[i].putTimedElt(path, self.base_path, i, obj)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;obj_time = ull.putObjectInObject(self.idx, obj_time, 'ALLTIMES', 0, obj)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;print ('putObjectSlice('+path+', '+cpopath+self.base_path+', '+str(self.cpoTime)+', '+str(obj_time)+')')&#xA;</xsl:text>
                <xsl:text>&#009;&#009;status = ull.putObjectSlice(self.idx, path, cpopath + self.base_path, self.cpoTime, obj_time)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>
            </xsl:if>

            <xsl:text>&#xA;&#009;def replaceLastSlice(self, path, cpopath):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'replaceLastSlice'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'object'"/>
            </xsl:call-template>
            <xsl:if test="@timed='yes'">
                <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', TIMED)')&#xA;</xsl:text>
                <xsl:text>&#009;&#009;obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, TIMED)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;for i in range(len(self.array)):&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;obj = self.array[i].putTimedElt(path, self.base_path, i, obj)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;print ('replaceLastObjectSlice('+path+', '+cpopath+self.base_path+', '+str(obj)+')')&#xA;</xsl:text>
                <xsl:text>&#009;&#009;status = ull.replaceLastObjectSlice(self.idx, path, cpopath + self.base_path, obj)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>      
            </xsl:if>

            <xsl:text>&#xA;&#009;def putNonTimed(self, path, cpopath):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'putNonTimed'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'object'"/>
            </xsl:call-template>
            <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')&#xA;</xsl:text>
            <xsl:text>&#009;&#009;obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;for i in range(len(self.array)):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')&#xA;</xsl:text>
            <xsl:text>&#009;&#009;status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>

            <xsl:text>&#xA;&#009;def getSlice(self, path, cpopath, inTime, interpolMode):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'getSlice'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'object'"/>
            </xsl:call-template>
            <!-- timed part -->
            <xsl:choose>
                <xsl:when test="@timed='yes'">
                    <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;print ('getObjectSlice('+path+', '+cpopath+self.base_path+', '+str(inTime)+')')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;status, obj_time = ull.getObjectSlice(self.idx, path, cpopath + self.base_path, inTime)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;if status:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;print ('Failed to get slice: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ')')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;return&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', 0)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;if status:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;print ('No data found for slice: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;return&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;obj_size = ull.getObjectDim(self.idx, obj)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;self.resize(obj_size)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;for i in range(obj_size):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;self.array[i].getTimedElt(path, self.base_path, i, obj)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;ull.releaseObject(self.idx, obj_time)&#xA;</xsl:text>        
                    <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;if status:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;return&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;obj_size = ull.getObjectDim(self.idx, obj)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;if (obj_size != 0):&#xA;</xsl:text>
		    <xsl:text>&#009;&#009;&#009;if (len(self.array) == 0):&#xA;</xsl:text>
		    <xsl:text>&#009;&#009;&#009;&#009;self.resize(obj_size)&#xA;</xsl:text>
		    <xsl:text>&#009;&#009;&#009;if (obj_size != len(self.array)):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;print ('error in getSlice: wrong size of object '+ path + '/' + cpopath +self.base_path + ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;else:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;for i in range(obj_size):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;self.array[i].getNonTimedElt(path, self.base_path, i, obj)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;ull.releaseObject(self.idx, obj)&#xA;</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;if status:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;return&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;obj_size = ull.getObjectDim(self.idx, obj)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;self.resize(obj_size)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;for i in range(obj_size):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;self.array[i].getNonTimedElt(path, self.base_path, i, obj)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;ull.releaseObject(self.idx, obj)&#xA;</xsl:text>
                </xsl:otherwise>
            </xsl:choose>

            <xsl:text>&#xA;&#009;def build_non_resampled_data(self, path, cpopath, nbslice):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'build_non_resampled_data'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'object'"/>
            </xsl:call-template>
            <xsl:text>&#009;&#009;list=[]&#xA;</xsl:text>
            <xsl:text>&#009;&#009;if nbslice > 0:&#xA;</xsl:text>
            <!-- timed part -->
            <xsl:choose>
                <xsl:when test="@timed='yes'">
                    <xsl:text>&#009;&#009;&#009;if (dev()):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;if status:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;for i in range(nbslice):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;list.append(</xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>(self.base_path))&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;return list&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;for n in range(nbslice):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;if status:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;list.append(</xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>(self.base_path))&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;continue&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;obj_size = ull.getObjectDim(self.idx, obj)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;slice = </xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>(self.base_path)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;slice.setExpIdx(self.idx)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;slice.resize(obj_size)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;for i in range(obj_size):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;slice.array[i].getTimedElt(path, self.base_path, i, obj)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;list.append(slice)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;ull.releaseObject(self.idx, obj_time)&#xA;</xsl:text>
                    <!-- non-timed part -->
                    <xsl:text>&#009;&#009;&#009;if (dev()):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;if status:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;return list&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;obj_size = ull.getObjectDim(self.idx, obj)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;for n in range(nbslice):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;if (obj_size != 0):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;if (len(list[n].array) == 0):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;&#009;list[n].resize(obj_size)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;if (obj_size != len(list[n].array)):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;&#009;print ('error in get: wrong size of object at '+path+'/'+cpopath+self.base_path + ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(list[n].array)) +')')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;else:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;&#009;for i in range(obj_size):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;&#009;&#009;if list[n]:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;&#009;&#009;&#009;list[n].array[i].getNonTimedElt(path, self.base_path, i, obj)&#xA;</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <!-- non-timed part -->
                    <xsl:text>&#009;&#009;&#009;if (dev()):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;if status:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;for i in range(nbslice):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;list.append(</xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>(self.base_path))&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;return list&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;obj_size = ull.getObjectDim(self.idx, obj)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;for n in range(nbslice):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;slice = </xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>(self.base_path)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;slice.setExpIdx(self.idx)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;slice.resize(obj_size)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;for i in range(obj_size):&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;&#009;slice.array[i].getNonTimedElt(path, self.base_path, i, obj)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;&#009;list.append(slice)&#xA;</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
            <xsl:text>&#009;&#009;&#009;ull.releaseObject(self.idx, obj)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;else:&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('error, nbslice must be > 0 and got:' + str(nbslice))&#xA;</xsl:text>
            <xsl:text>&#009;&#009;return list&#xA;</xsl:text>
        </xsl:if>

        <xsl:if test="@timed='yes'">
            <xsl:text>&#xA;&#009;def putTimedElt(self, path, cpopath, i, obj):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'putTimedElt'"/>
            </xsl:call-template>
            <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('beginObject idx=%d' %(self.idx)) &#xA;</xsl:text>
            <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', TIMED)')&#xA;</xsl:text>
            <xsl:text>&#009;&#009;obj2 = ull.beginObject(self.idx, obj, i, cpopath, TIMED)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('obj = ' + str(obj)) &#xA;</xsl:text>
            <xsl:text>&#009;&#009;for j in range(len(self.array)):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;if (dev()):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;&#009;print ('struct_array loop elt %d' %(j)) &#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;obj2 = self.array[j].putTimedElt(path, self.base_path, j, obj2)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')&#xA;</xsl:text>
            <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('putObjectInObject('+str(obj)+', '+cpopath+', '+str(i)+', '+str(obj2)+')')&#xA;</xsl:text>
            <xsl:text>&#009;&#009;obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;return obj&#xA;</xsl:text>

            <xsl:text>&#xA;&#009;def getTimedElt(self, path, cpopath, i, obj):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'getTimedElt'"/>
            </xsl:call-template>
            <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')&#xA;</xsl:text>
            <xsl:text>&#009;&#009;status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;if status:&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('No data found for timed array of structures: ' + path + '/' + cpopath)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;return&#xA;</xsl:text>
            <xsl:text>&#009;&#009;obj_size = ull.getObjectDim(self.idx, obj2)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;if (len(self.array)>0):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;&#009;print ('error in getTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (current part size =  ' + str(obj_size) + ') != (existing part size = ' + str(len(self.array)) +')')&#xA;</xsl:text>
            <xsl:text>&#009;&#009;else:&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;self.resize(obj_size)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;for j in range(obj_size):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;self.array[j].getTimedElt(path, self.base_path, j, obj2)&#xA;</xsl:text>
        </xsl:if>


        <xsl:text>&#xA;&#009;def putNonTimedElt(self, path, cpopath, i, obj):&#xA;</xsl:text>
        <xsl:call-template name="VERB_DEBUG">
            <xsl:with-param name="function" select="'putNonTimedElt'"/>
        </xsl:call-template>
        <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;print ('beginObject idx=%d' %(self.idx)) &#xA;</xsl:text>
        <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')&#xA;</xsl:text>
        <xsl:text>&#009;&#009;obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)&#xA;</xsl:text>
        <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;print ('obj = ' + str(obj)) &#xA;</xsl:text>
        <xsl:text>&#009;&#009;for j in range(len(self.array)):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;if (dev()):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;&#009;print ('struct_array loop elt %d' %(j) )&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)&#xA;</xsl:text>
        <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+</xsl:text>
        <xsl:text>','+str(i)+','+str(obj2)+')')&#xA;</xsl:text>
        <xsl:text>&#009;&#009;obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)&#xA;</xsl:text>
        <xsl:text>&#009;&#009;return obj&#xA;</xsl:text>    

        <xsl:text>&#xA;&#009;def getNonTimedElt(self, path, cpopath, i, obj):&#xA;</xsl:text>
        <xsl:call-template name="VERB_DEBUG">
            <xsl:with-param name="function" select="'getNonTimedElt'"/>
        </xsl:call-template>
        <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')&#xA;</xsl:text>
        <xsl:text>&#009;&#009;status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)&#xA;</xsl:text>
        <xsl:text>&#009;&#009;if status:&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;return&#xA;</xsl:text>
        <xsl:text>&#009;&#009;obj_size = ull.getObjectDim(self.idx, obj2)&#xA;</xsl:text>
        <xsl:text>&#009;&#009;if (len(self.array)>0):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;&#009;print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')&#xA;</xsl:text>
        <xsl:text>&#009;&#009;else:&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;self.resize(obj_size)&#xA;</xsl:text>
        <xsl:text>&#009;&#009;for j in range(obj_size):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;self.array[j].getNonTimedElt(path, self.base_path, j, obj2)&#xA;</xsl:text>

        <!-- class for elements of a struct_array: same name + 'Obj' -->
        <xsl:text>&#xA;&#xA;class </xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>Obj:&#xA;</xsl:text>
        <xsl:text>&#009;'''&#xA;</xsl:text>
        <xsl:text>&#009;class </xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>Obj&#xA;</xsl:text>
        <xsl:text>&#009;</xsl:text><xsl:value-of select="@documentation"/><xsl:text>&#xA;&#xA;</xsl:text>
        <xsl:text>&#009;Attributes:&#xA;</xsl:text>
        <xsl:apply-templates select="field" mode="DOC"/>
        <xsl:text>&#009;'''&#xA;&#xA;</xsl:text>

        <xsl:text>&#009;def __init__(self, base_path_in='</xsl:text><xsl:value-of select="@name"/><xsl:text>'):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.base_path = base_path_in&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.idx = EMPTY_INT&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.cpoTime = EMPTY_DOUBLE&#xA;</xsl:text>
        <xsl:apply-templates select="field" mode="DECLARE">
            <xsl:with-param name="processing" select="'type'"/>
        </xsl:apply-templates>

        <xsl:text>&#xA;&#009;def __str__(self, depth=0):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;space = depth*'\t'&#xA;</xsl:text>
        <xsl:text>&#009;&#009;ret = space + 'class </xsl:text><xsl:value-of select = "@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>Obj\n'&#xA;</xsl:text>
        <xsl:apply-templates select="field" mode="PRINT"/>
        <xsl:text>&#009;&#009;return ret&#xA;</xsl:text>

        <xsl:text>&#xA;&#009;def setExpIdx(self, idx):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.idx = idx&#xA;</xsl:text>
        <xsl:apply-templates select="field[@type='structure' or @type='struct_array']" mode="SETEXPIDX"/>

        <xsl:if test="@timed='yes'">
            <xsl:text>&#xA;&#009;def putTimedElt(self, path, cpopath, i, obj):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG_OBJ">
                <xsl:with-param name="function" select="'putTimedElt'"/>
            </xsl:call-template>
            <xsl:text>&#009;&#009;cpopath = cpopath + '/'&#xA;</xsl:text>
            <xsl:for-each select="field"><xsl:if test="descendant-or-self::field[@timed='yes']"><xsl:apply-templates select="." mode="PUTOBJ_TIMED"/></xsl:if></xsl:for-each>
            <xsl:text>&#009;&#009;return obj&#xA;</xsl:text>

            <xsl:text>&#xA;&#009;def getTimedElt(self, path, cpopath, i, obj):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG_OBJ">
                <xsl:with-param name="function" select="'getTimedElt'"/>
            </xsl:call-template>
            <xsl:text>&#009;&#009;cpopath = cpopath + '/'&#xA;</xsl:text>
            <xsl:for-each select="field"><xsl:if test="descendant-or-self::field[@timed='yes']"><xsl:apply-templates select="." mode="GETOBJ_TIMED"/></xsl:if></xsl:for-each>
        </xsl:if>

        <xsl:text>&#xA;&#009;def putNonTimedElt(self, path, cpopath, i, obj):&#xA;</xsl:text>
        <xsl:call-template name="VERB_DEBUG_OBJ">
            <xsl:with-param name="function" select="'putNonTimedElt'"/>
        </xsl:call-template>
        <xsl:text>&#009;&#009;cpopath = cpopath + '/'&#xA;</xsl:text>
        <xsl:apply-templates select="field" mode="PUTOBJ_NONTIMED"/>
        <xsl:text>&#009;&#009;return obj&#xA;</xsl:text>

        <xsl:text>&#xA;&#009;def getNonTimedElt(self, path, cpopath, i, obj):&#xA;</xsl:text>
        <xsl:call-template name="VERB_DEBUG_OBJ">
            <xsl:with-param name="function" select="'getNonTimedElt'"/>
        </xsl:call-template>
        <xsl:text>&#009;&#009;cpopath = cpopath + '/'&#xA;</xsl:text>
        <xsl:apply-templates select="field" mode="GETOBJ_NONTIMED"/>
    </xsl:template>


    <!-- Class generation for structure case: DOIT simple mode stands for not already 
    defined class, and nested parameter is set to 'yes' if the structue is defined 
    inside a struct_array field -->
    <xsl:template match="field[@type='structure']" mode="CLASS">
        <xsl:text>&#xA;&#xA;class </xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>:&#xA;</xsl:text>
        <xsl:text>&#009;'''&#xA;</xsl:text>
        <xsl:text>&#009;class </xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>&#xA;</xsl:text>
        <xsl:text>&#009;</xsl:text><xsl:value-of select="@documentation"/><xsl:text>&#xA;&#xA;</xsl:text>
        <xsl:text>&#009;Attributes:&#xA;</xsl:text>
        <xsl:apply-templates select="field" mode="DOC"/>
        <xsl:text>&#009;'''&#xA;&#xA;</xsl:text>

        <xsl:text>&#009;def __init__(self, base_path_in='</xsl:text><xsl:value-of select="@name"/><xsl:text>'):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.base_path = base_path_in&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.idx = EMPTY_INT&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.cpoTime = EMPTY_DOUBLE&#xA;</xsl:text>
        <xsl:apply-templates select="field" mode="DECLARE">
            <xsl:with-param name="processing" select="'type'"/>
        </xsl:apply-templates>

        <xsl:text>&#xA;&#009;def __str__(self, depth=0):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;space = depth*'\t'&#xA;</xsl:text>
        <xsl:text>&#009;&#009;ret = space + 'class </xsl:text><xsl:value-of select = "@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>\n'&#xA;</xsl:text>
        <xsl:apply-templates select="field" mode="PRINT"/>
        <xsl:text>&#009;&#009;return ret&#xA;</xsl:text>

        <xsl:text>&#xA;&#009;def setExpIdx(self, idx):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;self.idx = idx&#xA;</xsl:text>
        <xsl:apply-templates select="field[@type='structure' or @type='struct_array']" mode="SETEXPIDX"/>

        <!-- methods for non-timed CPOs case -->
        <xsl:if test="ancestor::CPO/@timed='no'">
            <xsl:text>&#xA;&#009;def put(self, path, cpopath):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'put'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'type'"/>
            </xsl:call-template>
            <xsl:apply-templates select="field" mode="PUT"/>

            <xsl:text>&#xA;&#009;def get(self, path, cpopath):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'get'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'type'"/>
            </xsl:call-template>
            <xsl:apply-templates select="field" mode="GET"/>
        </xsl:if>

        <!-- methods for timed CPOs case -->
        <xsl:if test="ancestor::CPO/@timed='yes'">
            <xsl:text>&#xA;&#009;def putSlice(self, path, cpopath):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'putSlice'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'type'"/>
            </xsl:call-template>
            <xsl:apply-templates select="field" mode="PUT_TIME">
                <xsl:with-param name="action" select="'put'"/>
                <xsl:with-param name="method" select="'slice'"/>
            </xsl:apply-templates> 

            <xsl:text>&#xA;&#009;def replaceLastSlice(self, path, cpopath):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'replaceLastSlice'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'type'"/>
            </xsl:call-template>
            <xsl:apply-templates select="field" mode="PUT_TIME">
                <xsl:with-param name="action" select="'replaceLast'"/>
                <xsl:with-param name="method" select="'slice'"/>
            </xsl:apply-templates>

            <xsl:text>&#xA;&#009;def putNonTimed(self, path, cpopath):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'putNonTimed'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'type'"/>
            </xsl:call-template>
            <xsl:apply-templates select="field" mode="PUT_TIME">
                <xsl:with-param name="action" select="'put'"/>
                <xsl:with-param name="method" select="'non-timed'"/>
            </xsl:apply-templates> 

            <xsl:text>&#xA;&#009;def getSlice(self, path, cpopath, inTime, interpolMode):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'getSlice'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'type'"/>
            </xsl:call-template>
            <xsl:apply-templates select="field" mode="GET_SLICE"/>

            <xsl:text>&#xA;&#009;def build_non_resampled_data(self, path, cpopath, nbslice):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG">
                <xsl:with-param name="function" select="'build_non_resampled_data'"/>
            </xsl:call-template>
            <xsl:call-template name="COMPUTE_PATH">
                <xsl:with-param name="processing" select="'type'"/>
            </xsl:call-template>
            <xsl:text>&#009;&#009;list=[]&#xA;</xsl:text>
            <xsl:text>&#009;&#009;if nbslice > 0:&#xA;</xsl:text>
            <xsl:apply-templates select="field" mode="GET_BUILD">
                <xsl:with-param name="processing" select="''"/>
            </xsl:apply-templates>
            <xsl:text>&#009;&#009;&#009;for i in range(nbslice):&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;&#009;slice = </xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>(self.base_path)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;&#009;slice.setExpIdx(self.idx)&#xA;</xsl:text>
            <xsl:apply-templates select="field" mode="GET_AFFECT"/>
            <xsl:text>&#009;&#009;&#009;&#009;list.append(slice)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;else:&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;print ('error, nbslice must be > 0 and got:' + str(nbslice))&#xA;</xsl:text>  
            <xsl:text>&#009;&#009;return list&#xA;</xsl:text>

            <xsl:text>&#xA;&#009;def putTimedElt(self, path, cpopath, i, obj):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG_OBJ">
                <xsl:with-param name="function" select="'putTimedElt'"/>
            </xsl:call-template>
            <xsl:text>&#009;&#009;cpopath = cpopath + '/' &#xA;</xsl:text>
            <xsl:for-each select="field"><xsl:if test="descendant-or-self::field[@timed='yes']"><xsl:apply-templates select="." mode="PUTOBJ_TIMED"/></xsl:if></xsl:for-each>
            <xsl:text>&#009;&#009;return obj&#xA;</xsl:text>

            <xsl:text>&#xA;&#009;def getTimedElt(self, path, cpopath, i, obj):&#xA;</xsl:text>
            <xsl:call-template name="VERB_DEBUG_OBJ">
                <xsl:with-param name="function" select="'getTimedElt'"/>
            </xsl:call-template>
            <xsl:text>&#009;&#009;cpopath = cpopath + '/' &#xA;</xsl:text>
            <xsl:for-each select="field"><xsl:if test="descendant-or-self::field[@timed='yes']"><xsl:apply-templates select="." mode="GETOBJ_TIMED"/></xsl:if></xsl:for-each>
        </xsl:if>

        <xsl:text>&#xA;&#009;def putNonTimedElt(self, path, cpopath, i, obj):&#xA;</xsl:text>
        <xsl:call-template name="VERB_DEBUG_OBJ">
            <xsl:with-param name="function" select="'putNonTimedElt'"/>
        </xsl:call-template>
        <xsl:text>&#009;&#009;cpopath = cpopath + '/' &#xA;</xsl:text>
        <xsl:apply-templates select="field" mode="PUTOBJ_NONTIMED"/>
        <xsl:text>&#009;&#009;return obj&#xA;</xsl:text>

        <xsl:text>&#xA;&#009;def getNonTimedElt(self, path, cpopath, i, obj):&#xA;</xsl:text>
        <xsl:call-template name="VERB_DEBUG_OBJ">
            <xsl:with-param name="function" select="'getNonTimedElt'"/>
        </xsl:call-template>
        <xsl:text>&#009;&#009;cpopath = cpopath + '/' &#xA;</xsl:text>
        <xsl:apply-templates select="field" mode="GETOBJ_NONTIMED"/>    

        <xsl:text>&#xA;&#009;def deleteData(self, path, cpopath):&#xA;</xsl:text>
        <xsl:call-template name="COMPUTE_PATH">
            <xsl:with-param name="processing" select="'type'"/>
        </xsl:call-template>
        <xsl:apply-templates select="field" mode="DELETE"/>
    </xsl:template>


    <!--Template used for debug prints-->
    <xsl:template name="VERB_DEBUG">
        <xsl:param name="function"/>
        <xsl:text>&#009;&#009;if (verb()):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;print ('field '</xsl:text>
        <xsl:text>+self.base_path+' of type </xsl:text>
        <xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>, run function </xsl:text>
        <xsl:value-of select="$function"/><xsl:text>') &#xA;</xsl:text>
    </xsl:template>
    <xsl:template name="VERB_DEBUG_OBJ">
        <xsl:param name="function"/>
        <xsl:text>&#009;&#009;if (verb()):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;print ('object of type </xsl:text>
        <xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>Obj, run function </xsl:text>
        <xsl:value-of select="$function"/><xsl:text>') &#xA;</xsl:text>
    </xsl:template>
    <xsl:template name="DEVEL_DEBUG">
        <xsl:param name="function"/>
        <xsl:param name="attr"/>
        <xsl:text>&#009;&#009;if (dev()):&#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;print ('</xsl:text>
        <xsl:value-of select="$function"/><xsl:text> : ' + cpopath + '</xsl:text>
        <xsl:value-of select="$attr"/><xsl:text>') &#xA;</xsl:text>
        <xsl:text>&#009;&#009;&#009;print ('obj = ' + str(obj))&#xA;</xsl:text>
    </xsl:template>


    <!--Template extensively used. It computes the path or cpopath string according to the occurrence value-->
    <xsl:template name="COMPUTE_PATH">
        <xsl:param name="processing"/>
        <xsl:choose>
            <xsl:when test="$processing='main'">
                <xsl:text>&#009;&#009;if occurrence==0:&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;path = self.base_path&#xA;</xsl:text>
                <xsl:text>&#009;&#009;else:&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;path = self.base_path + '/' + str(occurrence)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;cpopath = '' &#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="$processing='type'">
                <xsl:text>&#009;&#009;if cpopath=='':&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;cpopath = self.base_path + '/'&#xA;</xsl:text>
                <xsl:text>&#009;&#009;else:&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;cpopath = cpopath + self.base_path + '/' &#xA;</xsl:text>
                <xsl:text>&#009;&#009;if (verb()):&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;print ('path = ' + path + '  and cpopath = ' + cpopath) &#xA;</xsl:text>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <!--Documentation for a single field-->
    <xsl:template match = "field" mode = "DOC">
        <xsl:text>&#009;- </xsl:text><xsl:value-of select = "@name"/><xsl:text> : </xsl:text>
        <xsl:choose>
            <xsl:when test="@type='xs:string'"><xsl:text>str&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='xs:integer'"><xsl:text>int&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='xs:boolean'"><xsl:text>bool&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='xs:double' or @type='xs:float'"><xsl:text>float&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='cplx_type'"><xsl:text>complex&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='vecflt_type'or @type='vecdbl_type'"><xsl:text>numpy.ndarray 1D with float&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='vecint_type'"><xsl:text>numpy.ndarray 1D with int)&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='veccplx_type'"><xsl:text>numpy.ndarray 1D with  complex numbers&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='vecstring_type'"><xsl:text>list of str&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='matflt_type' or @type='matdbl_type'"><xsl:text>numpy.ndarray 2D with float&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='matint_type'"><xsl:text>numpy.ndarray 2D with int&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='matcplx_type'"><xsl:text>numpy.ndarray 2D with  complex numbers&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array3dflt_type' or @type='array3ddbl_type'"><xsl:text>numpy.ndarray 3D with float&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array3dint_type'"><xsl:text>numpy.ndarray 3D with int&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='array3dcplx_type'"><xsl:text>numpy.ndarray 3D with  complex numbers&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array4dflt_type' or @type='array4ddbl_type'"><xsl:text>numpy.ndarray 4D with float&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array4dint_type'"><xsl:text>numpy.ndarray 4D with int&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='array4dcplx_type'"><xsl:text>numpy.ndarray 4D with  complex numbers&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array5dflt_type' or @type='array5ddbl_type'"><xsl:text>numpy.ndarray 5D with float&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array5dint_type'"><xsl:text>numpy.ndarray 5D with int&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='array5dcplx_type'"><xsl:text>numpy.ndarray 5D with  complex numbers&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array6dflt_type' or @type='array6ddbl_type'"><xsl:text>numpy.ndarray 6D with float&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array6dint_type'"><xsl:text>numpy.ndarray 6D with int&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='array6dcplx_type'"><xsl:text>numpy.ndarray 6D with  complex numbers&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='structure'"><xsl:text>class </xsl:text><xsl:value-of select = "@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='struct_array'"><xsl:text>class </xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>: array of </xsl:text><xsl:value-of select="@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>Obj objects&#xA;</xsl:text></xsl:when>
        </xsl:choose>
        <xsl:text>&#009;   </xsl:text><xsl:value-of select = "@documentation"/><xsl:text>&#xA;</xsl:text>
    </xsl:template>

    <!--Declaration of a single field. Used only in __init__ functions-->   
    <xsl:template match = "field" mode = "DECLARE">
        <xsl:param name="processing"/>
        <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select = "@name"/>
        <xsl:choose>
            <xsl:when test="@type='xs:string'"><xsl:text> = ''&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='xs:integer'"><xsl:text> = EMPTY_INT&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='xs:boolean'"><xsl:text> = False&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='xs:double'"><xsl:text> = EMPTY_DOUBLE&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='xs:float'"><xsl:text> = EMPTY_DOUBLE&#xA;</xsl:text></xsl:when> 
	    <xsl:when test="@type='cplx_type'"><xsl:text> = EMPTY_COMPLEX&#xA;</xsl:text></xsl:when> 
            <xsl:when test="@type='vecflt_type' or @type='vecdbl_type'"><xsl:text> = numpy.zeros(0, numpy.float64, order='C')&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='veccplx_type'"><xsl:text> = numpy.zeros(0, numpy.complex128, order='C')&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='vecint_type'"><xsl:text> = numpy.zeros(0, numpy.int32, order='C')&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='vecstring_type'"><xsl:text> = ['']&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='matflt_type' or @type='matdbl_type'"><xsl:text> = numpy.zeros((0,0), numpy.float64, order='C')&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='matcplx_type'"><xsl:text> = numpy.zeros((0,0), numpy.complex128, order='C')&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='matint_type'"><xsl:text> = numpy.zeros((0,0), numpy.int32, order='C')&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array3dflt_type' or @type='array3ddbl_type'"><xsl:text> = numpy.zeros((0,0,0), numpy.float64, order='C')&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='array3dcplx_type'"><xsl:text> = numpy.zeros((0,0,0), numpy.complex128, order='C')&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array3dint_type'"><xsl:text> = numpy.zeros((0,0,0), numpy.int32, order='C')&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array4dflt_type' or @type='array4ddbl_type'"><xsl:text> = numpy.zeros((0,0,0,0), numpy.float64, order='C')&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array4dint_type'"><xsl:text> = numpy.zeros((0,0,0,0), numpy.int32, order='C')&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='array4dcplx_type'"><xsl:text> = numpy.zeros((0,0,0,0), numpy.complex128, order='C')&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array5dflt_type' or @type='array5ddbl_type'"><xsl:text> = numpy.zeros((0,0,0,0,0), numpy.float64, order='C')&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array5dint_type'"><xsl:text> = numpy.zeros((0,0,0,0,0), numpy.int32, order='C')&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='array5dcplx_type'"><xsl:text> = numpy.zeros((0,0,0,0,0), numpy.complex128, order='C')&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array6dflt_type' or @type='array6ddbl_type'"><xsl:text> = numpy.zeros((0,0,0,0,0,0), numpy.float64, order='C')&#xA;</xsl:text></xsl:when>
            <xsl:when test="@type='array6dint_type'"><xsl:text> = numpy.zeros((0,0,0,0,0,0), numpy.int32, order='C')&#xA;</xsl:text></xsl:when>
	    <xsl:when test="@type='array6dcplx_type'"><xsl:text> = numpy.zeros((0,0,0,0,0,0), numpy.complex128, order='C')&#xA;</xsl:text></xsl:when> 
            <xsl:when test="@type='structure' or @type='struct_array'"><xsl:text> = </xsl:text><xsl:value-of select = "@name"/><xsl:value-of select="@type"/><xsl:value-of select="@type-name"/><xsl:text>('</xsl:text><xsl:value-of select = "@name"/><xsl:text>')&#xA;</xsl:text></xsl:when>
            <xsl:otherwise>
		<xsl:message terminate="no">    ERROR! Cannot declare a field of unknown type: <xsl:value-of select="@type"/>!  </xsl:message>
	    </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!--Calling setExpIdx on all the structure in this class.-->
    <xsl:template match = "field" mode = "SETEXPIDX">
        <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select = "@name"/><xsl:text>.setExpIdx(idx)&#xA;</xsl:text>
    </xsl:template>



    <!--Display of a single field. Used only in __str__ functions-->   
    <xsl:template match = "field" mode = "PRINT">
        <xsl:choose>
            <xsl:when test="@type='xs:string' or @type='xs:integer' or @type='xs:boolean' or @type='xs:double' or @type='xs:float' or @type='cplx_type'">
                <xsl:text>&#009;&#009;ret = ret + space + 'Attribute </xsl:text><xsl:value-of select = "@name"/>
                <xsl:text>: ' + str(self.</xsl:text><xsl:value-of select = "@name"/><xsl:text>) + '\n'&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="@type='vecflt_type' or @type='vecdbl_type' or @type='vecint_type' or @type='vecstring_type' or @type='veccplx_type' or 
                @type='matflt_type' or @type='matdbl_type' or @type='matint_type' or @type='matcplx_type' or 
                @type='array3dflt_type' or @type='array3dint_type' or @type='array3ddbl_type' or @type='array3dcplx_type' or 
                @type='array4dflt_type' or @type='array4dint_type' or @type='array4ddbl_type' or @type='array4dcplx_type' or
                @type='array5dflt_type' or @type='array5dint_type' or @type='array5ddbl_type' or @type='array5dcplx_type' or
                @type='array6dflt_type' or @type='array6dint_type' or @type='array6ddbl_type' or @type='array6dcplx_type'" >
                <xsl:text>&#009;&#009;s = self.</xsl:text><xsl:value-of select = "@name"/><xsl:text>.__str__()&#xA;</xsl:text>
                <xsl:text>&#009;&#009;ret = ret + space + 'Attribute </xsl:text><xsl:value-of select = "@name"/>
                <xsl:text>\n' + space + s.replace('\n', '\n'+space) + '\n'&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="@type='structure' or @type='struct_array'">
                <xsl:text>&#009;&#009;ret = ret + space + 'Attribute </xsl:text><xsl:value-of select = "@name"/>
                <xsl:text>\n ' + self.</xsl:text><xsl:value-of select = "@name"/><xsl:text>.__str__(depth+1)&#xA;</xsl:text>
            </xsl:when>
	    <xsl:otherwise>
		<xsl:message terminate="no">    ERROR! Cannot print a field of unknown type: <xsl:value-of select="@type"/>!   </xsl:message>
	    </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <!--Little function used in the following template to make it more readable-->
    <func:function name="my:gen-get">
        <xsl:param name="prefix" />
        <xsl:param name="type" />
        <func:result>
            <xsl:text>&#009;&#009;status, ret_</xsl:text><xsl:value-of select = "@name"/><xsl:text> = ull.</xsl:text><xsl:value-of select="$prefix"/><xsl:value-of select="$type"/><xsl:text>(self.idx, path, cpopath + '</xsl:text><xsl:value-of select="@name"/><xsl:text>')&#xA;</xsl:text>
            <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>
            <xsl:text>&#009;&#009;if not status:&#xA;</xsl:text>
            <xsl:text>&#009;&#009;&#009;self.</xsl:text><xsl:value-of select = "@name"/><xsl:text> = ret_</xsl:text><xsl:value-of select = "@name"/><xsl:text>&#xA;</xsl:text>
        </func:result>
    </func:function>

    <!--Generation of the UAL low level call for a field within the get function-->
    <xsl:template match = "field" mode = "GET">
        <xsl:choose>
            <xsl:when test="@type='xs:string'"><xsl:value-of select="my:gen-get('get','String')"/></xsl:when>
            <xsl:when test="@type='xs:integer'"><xsl:value-of select="my:gen-get('get','Int')"/></xsl:when>
            <xsl:when test="@type='xs:boolean'"><xsl:value-of select="my:gen-get('get','Int')"/></xsl:when>
            <xsl:when test="@type='xs:double'"><xsl:value-of select="my:gen-get('get','Double')"/></xsl:when>
            <xsl:when test="@type='xs:float'"><xsl:value-of select="my:gen-get('get','Double')"/></xsl:when>
	    <xsl:when test="@type='cplx_type'"><xsl:value-of select="my:gen-get('get','Complex')"/></xsl:when>
            <xsl:when test="@type='vecstring_type'"><xsl:value-of select="my:gen-get('get','Vect1DString')"/></xsl:when>
            <xsl:when test="@type='vecflt_type'"><xsl:value-of select="my:gen-get('get','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecdbl_type'"><xsl:value-of select="my:gen-get('get','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecint_type'"><xsl:value-of select="my:gen-get('get','Vect1DInt')"/></xsl:when>
	    <xsl:when test="@type='veccplx_type'"><xsl:value-of select="my:gen-get('get','Vect1DComplex')"/></xsl:when>
            <xsl:when test="@type='matflt_type'"><xsl:value-of select="my:gen-get('get','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matdbl_type'"><xsl:value-of select="my:gen-get('get','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matint_type'"><xsl:value-of select="my:gen-get('get','Vect2DInt')"/></xsl:when>
	    <xsl:when test="@type='matcplx_type'"><xsl:value-of select="my:gen-get('get','Vect2DComplex')"/></xsl:when>
            <xsl:when test="@type='array3dflt_type'"><xsl:value-of select="my:gen-get('get','Vect3DDouble')"/></xsl:when>
            <xsl:when test="@type='array3dint_type'"><xsl:value-of select="my:gen-get('get','Vect3DInt')"/></xsl:when>
            <xsl:when test="@type='array3ddbl_type'"><xsl:value-of select="my:gen-get('get','Vect3DDouble')"/></xsl:when>
	    <xsl:when test="@type='array3dcplx_type'"><xsl:value-of select="my:gen-get('get','Vect3DComplex')"/></xsl:when>
            <xsl:when test="@type='array4dflt_type'"><xsl:value-of select="my:gen-get('get','Vect4DDouble')"/></xsl:when>
            <xsl:when test="@type='array4dint_type'"><xsl:value-of select="my:gen-get('get','Vect4DInt')"/></xsl:when>
            <xsl:when test="@type='array4ddbl_type'"><xsl:value-of select="my:gen-get('get','Vect4DDouble')"/></xsl:when>
	    <xsl:when test="@type='array4dcplx_type'"><xsl:value-of select="my:gen-get('get','Vect4DComplex')"/></xsl:when>
            <xsl:when test="@type='array5dflt_type'"><xsl:value-of select="my:gen-get('get','Vect5DDouble')"/></xsl:when>
            <xsl:when test="@type='array5dint_type'"><xsl:value-of select="my:gen-get('get','Vect5DInt')"/></xsl:when>
            <xsl:when test="@type='array5ddbl_type'"><xsl:value-of select="my:gen-get('get','Vect5DDouble')"/></xsl:when>
	    <xsl:when test="@type='array5dcplx_type'"><xsl:value-of select="my:gen-get('get','Vect5DComplex')"/></xsl:when>
            <xsl:when test="@type='array6dflt_type'"><xsl:value-of select="my:gen-get('get','Vect6DDouble')"/></xsl:when>
            <xsl:when test="@type='array6dint_type'"><xsl:value-of select="my:gen-get('get','Vect6DInt')"/></xsl:when>
            <xsl:when test="@type='array6ddbl_type'"><xsl:value-of select="my:gen-get('get','Vect6DDouble')"/></xsl:when>
	    <xsl:when test="@type='array6dcplx_type'"><xsl:value-of select="my:gen-get('get','Vect6DComplex')"/></xsl:when>
            <xsl:when test="@type='structure' or @type='struct_array'"><xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@name"/><xsl:text>.get(path, cpopath)&#xA;</xsl:text></xsl:when>
	    <xsl:otherwise>
		<xsl:message terminate="no">    ERROR! Cannot define GET method for unknown type: <xsl:value-of select="@type"/>!  </xsl:message>
	    </xsl:otherwise>
        </xsl:choose>
    </xsl:template>



    <!--Little function used in the following template to make it more readable-->
    <!--If the field is timed, the ual low level call is a getsomethingSlice, otherwise it is getsomething like in the get method.-->
    <func:function name="my:gen-getSlice">
        <xsl:param name="prefix" />
        <xsl:param name="type" />
        <func:result>
            <xsl:choose>
                <xsl:when test="@timed='yes'">
                    <xsl:text>&#009;&#009;status, ret_</xsl:text><xsl:value-of select = "@name"/><xsl:text>, retTime</xsl:text><xsl:text> = ull.</xsl:text><xsl:value-of select="$prefix"/><xsl:value-of select="$type"/><xsl:text>Slice(self.idx, path, cpopath + '</xsl:text><xsl:value-of select="@name"/><xsl:text>', inTime, interpolMode)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;if not status:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;self.</xsl:text><xsl:value-of select = "@name"/><xsl:text> = ret_</xsl:text><xsl:value-of select = "@name"/><xsl:text>&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;self.cpoTime = retTime&#xA;</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>&#009;&#009;status, ret_</xsl:text><xsl:value-of select = "@name"/><xsl:text> = ull.</xsl:text><xsl:value-of select="$prefix"/><xsl:value-of select="$type"/><xsl:text>(self.idx, path, cpopath + '</xsl:text><xsl:value-of select="@name"/><xsl:text>')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;if not status:&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;self.</xsl:text><xsl:value-of select = "@name"/><xsl:text> = ret_</xsl:text><xsl:value-of select = "@name"/><xsl:text>&#xA;</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </func:result>
    </func:function>

    <!-- Generation of the UAL low level call for a field within the getSlice function -->
    <xsl:template match = "field" mode = "GET_SLICE">
        <xsl:choose>
            <xsl:when test="@type='xs:string'"><xsl:value-of select="my:gen-getSlice('get','String')"/></xsl:when>
            <xsl:when test="@type='xs:integer'"><xsl:value-of select="my:gen-getSlice('get','Int')"/></xsl:when>
            <xsl:when test="@type='xs:boolean'"><xsl:value-of select="my:gen-getSlice('get','Int')"/></xsl:when>
            <xsl:when test="@type='xs:double'"><xsl:value-of select="my:gen-getSlice('get','Double')"/></xsl:when>
            <xsl:when test="@type='xs:float'"><xsl:value-of select="my:gen-getSlice('get','Double')"/></xsl:when>
	    <xsl:when test="@type='cplx_type'"><xsl:value-of select="my:gen-getSlice('get','Complex')"/></xsl:when>
            <xsl:when test="@type='vecstring_type'"><xsl:value-of select="my:gen-getSlice('get','Vect1DString')"/></xsl:when>
            <xsl:when test="@type='vecflt_type'"><xsl:value-of select="my:gen-getSlice('get','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecdbl_type'"><xsl:value-of select="my:gen-getSlice('get','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecint_type'"><xsl:value-of select="my:gen-getSlice('get','Vect1DInt')"/></xsl:when>
	    <xsl:when test="@type='veccplx_type'"><xsl:value-of select="my:gen-getSlice('get','Vect1DComplex')"/></xsl:when>
            <xsl:when test="@type='matflt_type'"><xsl:value-of select="my:gen-getSlice('get','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matdbl_type'"><xsl:value-of select="my:gen-getSlice('get','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matint_type'"><xsl:value-of select="my:gen-getSlice('get','Vect2DInt')"/></xsl:when>
	    <xsl:when test="@type='matcplx_type'"><xsl:value-of select="my:gen-getSlice('get','Vect2DComplex')"/></xsl:when>
            <xsl:when test="@type='array3dflt_type'"><xsl:value-of select="my:gen-getSlice('get','Vect3DDouble')"/></xsl:when>
            <xsl:when test="@type='array3dint_type'"><xsl:value-of select="my:gen-getSlice('get','Vect3DInt')"/></xsl:when>
            <xsl:when test="@type='array3ddbl_type'"><xsl:value-of select="my:gen-getSlice('get','Vect3DDouble')"/></xsl:when>
	    <xsl:when test="@type='array3dcplx_type'"><xsl:value-of select="my:gen-getSlice('get','Vect3DComplex')"/></xsl:when>
            <xsl:when test="@type='array4dflt_type'"><xsl:value-of select="my:gen-getSlice('get','Vect4DDouble')"/></xsl:when>
            <xsl:when test="@type='array4dint_type'"><xsl:value-of select="my:gen-getSlice('get','Vect4DInt')"/></xsl:when>
            <xsl:when test="@type='array4ddbl_type'"><xsl:value-of select="my:gen-getSlice('get','Vect4DDouble')"/></xsl:when>
	    <xsl:when test="@type='array4dcplx_type'"><xsl:value-of select="my:gen-getSlice('get','Vect4DComplex')"/></xsl:when>
            <xsl:when test="@type='array5dflt_type'"><xsl:value-of select="my:gen-getSlice('get','Vect5DDouble')"/></xsl:when>
            <xsl:when test="@type='array5dint_type'"><xsl:value-of select="my:gen-getSlice('get','Vect5DInt')"/></xsl:when>
            <xsl:when test="@type='array5ddbl_type'"><xsl:value-of select="my:gen-getSlice('get','Vect5DDouble')"/></xsl:when>
	    <xsl:when test="@type='array5dcplx_type'"><xsl:value-of select="my:gen-getSlice('get','Vect5DComplex')"/></xsl:when>
            <xsl:when test="@type='array6dflt_type'"><xsl:value-of select="my:gen-getSlice('get','Vect6DDouble')"/></xsl:when>
            <xsl:when test="@type='array6dint_type'"><xsl:value-of select="my:gen-getSlice('get','Vect6DInt')"/></xsl:when>
            <xsl:when test="@type='array6ddbl_type'"><xsl:value-of select="my:gen-getSlice('get','Vect6DDouble')"/></xsl:when>
	    <xsl:when test="@type='array6dcplx_type'"><xsl:value-of select="my:gen-getSlice('get','Vect6DComplex')"/></xsl:when>	    
            <xsl:when test="@type='structure' or @type='struct_array'"><xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@name"/><xsl:text>.getSlice(path, cpopath, inTime, interpolMode)&#xA;</xsl:text></xsl:when>
	    <xsl:otherwise>
		<xsl:message terminate="no">    ERROR! Cannot define GET_SLICE method for unknown type: <xsl:value-of select="@type"/>!  </xsl:message>
	    </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <!--Function used in the following template to make it more readable-->
    <!--If the field is not timed, the ual low level call is a getsomething call like in the get function.
    otherwise the ual call returns all the time slices in a single array, that's why the dimension of the vector 
    is increased by one. The ual call is not exactly the same if we are treating the resampled case or not. 
    Finally, if the field is empty in the database, no memory is allocated by the ual and the empty time slices 
    have to be built by the high level interface.-->
    <func:function name="my:gen-getBuild">
        <xsl:param name="prefix" />
        <xsl:param name="type-no-time" />
        <xsl:param name="type-with-time" />
        <xsl:param name="resize" />
        <func:result>
            <xsl:choose>
                <xsl:when test="@timed='yes'">
                    <xsl:text>&#009;&#009;&#009;status, </xsl:text><xsl:value-of select="@name"/><xsl:text>List = ull.</xsl:text><xsl:value-of select="$prefix"/><xsl:value-of select="$type-with-time"/><xsl:text>(self.idx, path, cpopath + '</xsl:text><xsl:value-of select="@name"/><xsl:text>')&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;&#009;if len(</xsl:text><xsl:value-of select="@name"/><xsl:text>List) == 0:&#xA;</xsl:text>
                    <xsl:choose>
                        <xsl:when test="contains(string($type-with-time), 'String')">
                            <xsl:text>&#009;&#009;&#009;&#009;for i in range(nbslice):&#xA;</xsl:text>
                            <xsl:text>&#009;&#009;&#009;&#009;&#009;</xsl:text><xsl:value-of select="@name"/>
                            <xsl:text>List.append('')&#xA;</xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:text>&#009;&#009;&#009;&#009;</xsl:text><xsl:value-of select="@name"/><xsl:text>List = </xsl:text>
                            <xsl:text>numpy.resize(</xsl:text><xsl:value-of select="@name"/><xsl:text>List, (</xsl:text>
                            <xsl:value-of select="$resize"/><xsl:text>nbslice))&#xA;</xsl:text>            
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>&#009;&#009;&#009;status, </xsl:text><xsl:value-of select = "@name"/><xsl:text>Val = ull.</xsl:text>
                    <xsl:value-of select="$prefix"/><xsl:value-of select="$type-no-time"/>
                    <xsl:text>(self.idx, path, cpopath + '</xsl:text><xsl:value-of select="@name"/><xsl:text>')&#xA;</xsl:text>
                </xsl:otherwise>
            </xsl:choose> 
            <xsl:text>&#009;&#009;&#009;check_status(status)&#xA;</xsl:text>
        </func:result>
    </func:function>


    <!-- Generation of the UAL low level call for a field within the build_non_resampled_data function (build_resampled_data is DEPRECATED) -->
    <xsl:template match = "field" mode = "GET_BUILD">
        <!-- DEPRECATED <xsl:param name="processing"/> param used to be passed last in my:gen-getBuild func-->
        <xsl:choose>
            <xsl:when test="@type='xs:string'"><xsl:value-of select="my:gen-getBuild('get','String','Vect1DString','')"/></xsl:when>
            <xsl:when test="@type='xs:integer'"><xsl:value-of select="my:gen-getBuild('get','Int','Vect1DInt','')"/></xsl:when>
            <xsl:when test="@type='xs:boolean'"><xsl:value-of select="my:gen-getBuild('get','Int','Vect1DInt','')"/></xsl:when>
            <xsl:when test="@type='xs:double'"><xsl:value-of select="my:gen-getBuild('get','Double','Vect1DDouble','')"/></xsl:when>
            <xsl:when test="@type='xs:float'"><xsl:value-of select="my:gen-getBuild('get','Double','Vect1DDouble','')"/></xsl:when>
	    <xsl:when test="@type='cplx_type'"><xsl:value-of select="my:gen-getBuild('get','Complex','Vect1DComplex','')"/></xsl:when>
            <!-- should never go in timed case for vecstring_type -->
            <xsl:when test="@type='vecstring_type'"><xsl:value-of select="my:gen-getBuild('get','Vect1DString','Vect2DString_error','0,')"/></xsl:when>
            <xsl:when test="@type='vecflt_type'"><xsl:value-of select="my:gen-getBuild('get','Vect1DDouble','Vect2DDouble','0,')"/></xsl:when>
            <xsl:when test="@type='vecdbl_type'"><xsl:value-of select="my:gen-getBuild('get','Vect1DDouble','Vect2DDouble','0,')"/></xsl:when>
            <xsl:when test="@type='vecint_type'"><xsl:value-of select="my:gen-getBuild('get','Vect1DInt','Vect2DInt','0,')"/></xsl:when>
	    <xsl:when test="@type='veccplx_type'"><xsl:value-of select="my:gen-getBuild('get','Vect1DComplex','Vect2DComplex','0,')"/></xsl:when>
            <xsl:when test="@type='matflt_type'"><xsl:value-of select="my:gen-getBuild('get','Vect2DDouble','Vect3DDouble','0,0,')"/></xsl:when>
            <xsl:when test="@type='matdbl_type'"><xsl:value-of select="my:gen-getBuild('get','Vect2DDouble','Vect3DDouble','0,0,')"/></xsl:when>
            <xsl:when test="@type='matint_type'"><xsl:value-of select="my:gen-getBuild('get','Vect2DInt','Vect3DInt','0,0,')"/></xsl:when>
	    <xsl:when test="@type='matcplx_type'"><xsl:value-of select="my:gen-getBuild('get','Vect2DComplex','Vect3DComplex','0,0,')"/></xsl:when>
            <xsl:when test="@type='array3dflt_type'"><xsl:value-of select="my:gen-getBuild('get','Vect3DDouble','Vect4DDouble','0,0,0,')"/></xsl:when>
            <xsl:when test="@type='array3dint_type'"><xsl:value-of select="my:gen-getBuild('get','Vect3DInt','Vect4DInt','0,0,0,')"/></xsl:when>
            <xsl:when test="@type='array3ddbl_type'"><xsl:value-of select="my:gen-getBuild('get','Vect3DDouble','Vect4DDouble','0,0,0,')"/></xsl:when>
	    <xsl:when test="@type='array3dcplx_type'"><xsl:value-of select="my:gen-getBuild('get','Vect3DComplex','Vect4DComplex','0,0,0,')"/></xsl:when>
            <xsl:when test="@type='array4dflt_type'"><xsl:value-of select="my:gen-getBuild('get','Vect4DDouble','Vect5DDouble','0,0,0,0,')"/></xsl:when>
            <xsl:when test="@type='array4dint_type'"><xsl:value-of select="my:gen-getBuild('get','Vect4DInt','Vect5DInt','0,0,0,0,')"/></xsl:when>
            <xsl:when test="@type='array4ddbl_type'"><xsl:value-of select="my:gen-getBuild('get','Vect4DDouble','Vect5DDouble','0,0,0,0,')"/></xsl:when>
	    <xsl:when test="@type='array4dcplx_type'"><xsl:value-of select="my:gen-getBuild('get','Vect4DComplex','Vect5DComplex','0,0,0,0,')"/></xsl:when>
            <xsl:when test="@type='array5dflt_type'"><xsl:value-of select="my:gen-getBuild('get','Vect5DDouble','Vect6DDouble','0,0,0,0,0,')"/></xsl:when>
            <xsl:when test="@type='array5dint_type'"><xsl:value-of select="my:gen-getBuild('get','Vect5DInt','Vect6DInt','0,0,0,0,0,')"/></xsl:when>
            <xsl:when test="@type='array5ddbl_type'"><xsl:value-of select="my:gen-getBuild('get','Vect5DDouble','Vect6DDouble','0,0,0,0,0,')"/></xsl:when>
	    <xsl:when test="@type='array5dcplx_type'"><xsl:value-of select="my:gen-getBuild('get','Vect5DComplex','Vect6DComplex','0,0,0,0,0,')"/></xsl:when>
            <xsl:when test="@type='array6dflt_type'"><xsl:value-of select="my:gen-getBuild('get','Vect6DDouble','Vect7DDouble','0,0,0,0,0,0,')"/></xsl:when>
            <xsl:when test="@type='array6dint_type'"><xsl:value-of select="my:gen-getBuild('get','Vect6DInt','Vect7DInt','0,0,0,0,0,0,')"/></xsl:when>
            <xsl:when test="@type='array6ddbl_type'"><xsl:value-of select="my:gen-getBuild('get','Vect6DDouble','Vect7DDouble','0,0,0,0,0,0,')"/></xsl:when>
	    <xsl:when test="@type='array6dcplx_type'"><xsl:value-of select="my:gen-getBuild('get','Vect6DComplex','Vect7DComplex','0,0,0,0,0,0,')"/></xsl:when>
            <xsl:when test="@type='structure' or @type='struct_array'"><xsl:text>&#009;&#009;&#009;</xsl:text><xsl:value-of select="@name"/><xsl:text>List = self.</xsl:text><xsl:value-of select="@name"/><xsl:text>.build_non_resampled_data(path, cpopath, nbslice)&#xA;</xsl:text></xsl:when>
   	    <xsl:otherwise>
		<xsl:message terminate="no">    ERROR! Cannot define GET_BUILD method for unknown type: <xsl:value-of select="@type"/>!  </xsl:message>
	    </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <!--Little function used in the following template to make it more readable-->
    <!--If the field is not timed, the same value is affected to all the time slices, 
    otherwise each d-1 slice of the array returned by the ual to the right time slice
    in the array.-->
    <func:function name="my:gen-getAffect">
        <xsl:param name="array_access" />
        <func:result>
            <xsl:choose>
                <xsl:when test="@timed='no'">
                    <xsl:text>&#009;&#009;&#009;&#009;slice.</xsl:text><xsl:value-of select="@name"/><xsl:text> = </xsl:text><xsl:value-of select="@name"/><xsl:text>Val&#xA;</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>&#009;&#009;&#009;&#009;slice.</xsl:text><xsl:value-of select="@name"/><xsl:text> = </xsl:text>
                    <xsl:choose>
                        <xsl:when test="contains(string(@type),'xs:integer')">
                            <xsl:text>int(</xsl:text>
                        </xsl:when>
                    </xsl:choose>
                    <xsl:value-of select="@name"/><xsl:text>List</xsl:text><xsl:value-of select="$array_access"/>

                    <xsl:choose>
                        <xsl:when test="contains(string(@type),'xs:integer')">
                            <xsl:text>.copy())</xsl:text>
                        </xsl:when>
                        <xsl:when test="contains(string(@type),'xs:float') or contains(string(@type),'xs:double')">
                            <xsl:text>.copy().astype(float)</xsl:text>
                        </xsl:when>
	                <xsl:when test="contains(string(@type),'cplx_type')">
                            <xsl:text>.copy().astype(complex)</xsl:text>
                        </xsl:when>
                    </xsl:choose>
                    <!--
                    <xsl:if test="not(contains(string(@type), 'string'))">
                        <xsl:text>.copy()</xsl:text>
                    </xsl:if>
                    -->
                    <xsl:text>&#xA;</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </func:result>
    </func:function>

    <!-- Slice affectations for a field within the build_non_resampled_data function (build_resampled_data is DEPRECATED) -->
    <xsl:template match = "field" mode = "GET_AFFECT">
        <xsl:choose>
            <xsl:when test="@type='xs:string' or @type='xs:integer' or @type='xs:boolean' or @type='xs:double' or @type='xs:float' or @type='cplx_type'">
                <xsl:value-of select="my:gen-getAffect('[i]')"/>
            </xsl:when>
            <xsl:when test="@type='vecflt_type' or @type='vecdbl_type' or @type='vecint_type' or @type='vecstring_type' or @type='veccplx_type' ">
                <xsl:value-of select="my:gen-getAffect('[:,i]')"/>
            </xsl:when>
            <xsl:when test="@type='matflt_type' or @type='matdbl_type' or @type='matint_type' or @type='matcplx_type'">
                <xsl:value-of select="my:gen-getAffect('[:,:,i]')"/>
            </xsl:when>
            <xsl:when test="@type='array3dflt_type' or @type='array3dint_type' or @type='array3ddbl_type' or @type='array3dcplx_type'">
                <xsl:value-of select="my:gen-getAffect('[:,:,:,i]')"/>
            </xsl:when>
            <xsl:when test="@type='array4dflt_type' or @type='array4dint_type' or @type='array4ddbl_type' or @type='array4dcplx_type'">
                <xsl:value-of select="my:gen-getAffect('[:,:,:,:,i]')"/>
            </xsl:when>
            <xsl:when test="@type='array5dflt_type' or @type='array5dint_type' or @type='array5ddbl_type' or @type='array5dcplx_type'">
                <xsl:value-of select="my:gen-getAffect('[:,:,:,:,:,i]')"/>
            </xsl:when>
            <xsl:when test="@type='array6dflt_type' or @type='array6dint_type' or @type='array6ddbl_type' or @type='array6dcplx_type'">
                <xsl:value-of select="my:gen-getAffect('[:,:,:,:,:,:,i]')"/>
            </xsl:when>
            <xsl:when test="@type='structure' or @type='struct_array'">
                <xsl:text>&#009;&#009;&#009;&#009;slice.</xsl:text><xsl:value-of select="@name"/><xsl:text> = </xsl:text>
                <xsl:value-of select="@name"/><xsl:text>List[i]&#xA;</xsl:text>
            </xsl:when>
	    <xsl:otherwise>
		<xsl:message terminate="no">    ERROR! Cannot define GET_AFFECT method for unknown type: <xsl:value-of select="@type"/>!  </xsl:message>
	    </xsl:otherwise>
        </xsl:choose>
    </xsl:template>




    <!-- Little function used in the following template to make it more readable-->
    <!-- Generate the right ual call for each field. There is a subtility in the ual 
    API, for vectors it must be specified if the field is timed or not but not 
    for non vector types-->
    <func:function name="my:gen-put">
        <xsl:param name="prefix" />
        <xsl:param name="type" />
        <func:result>
            <xsl:text>&#009;&#009;status = ull.</xsl:text>
            <xsl:value-of select="$prefix"/><xsl:value-of select="$type"/>
            <xsl:text>(self.idx, path, cpopath + '</xsl:text><xsl:value-of select="@name"/><xsl:text>', </xsl:text>
            <xsl:choose>
                <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Int')">
                    <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                    <xsl:text>).astype(numpy.int32)</xsl:text>
                    <xsl:text>, False)&#xA;</xsl:text>
                </xsl:when>
                <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Double')">
                    <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                    <xsl:text>).astype(numpy.float64)</xsl:text>
                    <xsl:text>, False)&#xA;</xsl:text>
                </xsl:when>
		<xsl:when test="contains(string($type),'Vect') and contains(string($type),'Complex')">
                    <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                    <xsl:text>).astype(numpy.complex128)</xsl:text>
                    <xsl:text>, False)&#xA;</xsl:text>
                </xsl:when>
                <xsl:when test="contains(string($type),'Vect') and contains(string($type),'String')">
                    <xsl:text>self.</xsl:text><xsl:value-of select = "@name"/>
                    <xsl:text>, False)&#xA;</xsl:text>
                </xsl:when>
                <xsl:when test="string($type)='String'">
                    <xsl:text>self.</xsl:text><xsl:value-of select = "@name"/>
                    <xsl:text>)&#xA;</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:text>self.</xsl:text><xsl:value-of select = "@name"/>
                    <xsl:text>)&#xA;</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
            <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>
        </func:result>
    </func:function>

    <!--Generation of the UAL low level call for a field within the put function-->
    <xsl:template match = "field" mode = "PUT">
        <xsl:choose>
            <xsl:when test="@type='xs:string'"><xsl:value-of select="my:gen-put('put','String')"/></xsl:when>
            <xsl:when test="@type='xs:integer'"><xsl:value-of select="my:gen-put('put','Int')"/></xsl:when>
            <xsl:when test="@type='xs:boolean'"><xsl:value-of select="my:gen-put('put','Int')"/></xsl:when>
            <xsl:when test="@type='xs:double'"><xsl:value-of select="my:gen-put('put','Double')"/></xsl:when>
            <xsl:when test="@type='xs:float'"><xsl:value-of select="my:gen-put('put','Double')"/></xsl:when>
	    <xsl:when test="@type='cplx_type'"><xsl:value-of select="my:gen-put('put','Complex')"/></xsl:when>
            <xsl:when test="@type='vecstring_type'"><xsl:value-of select="my:gen-put('put','Vect1DString')"/></xsl:when>
            <xsl:when test="@type='vecflt_type'"><xsl:value-of select="my:gen-put('put','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecdbl_type'"><xsl:value-of select="my:gen-put('put','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecint_type'"><xsl:value-of select="my:gen-put('put','Vect1DInt')"/></xsl:when>
	    <xsl:when test="@type='veccplx_type'"><xsl:value-of select="my:gen-put('put','Vect1DComplex')"/></xsl:when>
            <xsl:when test="@type='matflt_type'"><xsl:value-of select="my:gen-put('put','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matdbl_type'"><xsl:value-of select="my:gen-put('put','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matint_type'"><xsl:value-of select="my:gen-put('put','Vect2DInt')"/></xsl:when>
	    <xsl:when test="@type='matcplx_type'"><xsl:value-of select="my:gen-put('put','Vect2DComplex')"/></xsl:when>
            <xsl:when test="@type='array3dflt_type'"><xsl:value-of select="my:gen-put('put','Vect3DDouble')"/></xsl:when>
            <xsl:when test="@type='array3dint_type'"><xsl:value-of select="my:gen-put('put','Vect3DInt')"/></xsl:when>
            <xsl:when test="@type='array3ddbl_type'"><xsl:value-of select="my:gen-put('put','Vect3DDouble')"/></xsl:when>
	    <xsl:when test="@type='array3dcplx_type'"><xsl:value-of select="my:gen-put('put','Vect3DComplex')"/></xsl:when>
            <xsl:when test="@type='array4dflt_type'"><xsl:value-of select="my:gen-put('put','Vect4DDouble')"/></xsl:when>
            <xsl:when test="@type='array4dint_type'"><xsl:value-of select="my:gen-put('put','Vect4DInt')"/></xsl:when>
            <xsl:when test="@type='array4ddbl_type'"><xsl:value-of select="my:gen-put('put','Vect4DDouble')"/></xsl:when>
	    <xsl:when test="@type='array4dcplx_type'"><xsl:value-of select="my:gen-put('put','Vect4DComplex')"/></xsl:when>
            <xsl:when test="@type='array5dflt_type'"><xsl:value-of select="my:gen-put('put','Vect5DDouble')"/></xsl:when>
            <xsl:when test="@type='array5dint_type'"><xsl:value-of select="my:gen-put('put','Vect5DInt')"/></xsl:when>
            <xsl:when test="@type='array5ddbl_type'"><xsl:value-of select="my:gen-put('put','Vect5DDouble')"/></xsl:when>
	    <xsl:when test="@type='array5dcplx_type'"><xsl:value-of select="my:gen-put('put','Vect5DComplex')"/></xsl:when>
            <xsl:when test="@type='array6dflt_type'"><xsl:value-of select="my:gen-put('put','Vect6DDouble')"/></xsl:when>
            <xsl:when test="@type='array6dint_type'"><xsl:value-of select="my:gen-put('put','Vect6DInt')"/></xsl:when>
            <xsl:when test="@type='array6ddbl_type'"><xsl:value-of select="my:gen-put('put','Vect6DDouble')"/></xsl:when>
	    <xsl:when test="@type='array6dcplx_type'"><xsl:value-of select="my:gen-put('put','Vect6DComplex')"/></xsl:when>
            <xsl:when test="@type='structure' or 'struct_array'">
                <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@name"/><xsl:text>.put(path, cpopath)&#xA;</xsl:text>
            </xsl:when>
	    <xsl:otherwise>
		<xsl:message terminate="no">    ERROR! Cannot define PUT method for unknown type: <xsl:value-of select="@type"/>!  </xsl:message>
	    </xsl:otherwise>
        </xsl:choose>
    </xsl:template>




    <!--Little function used in the following template to make it more readable-->
    <func:function name="my:gen-putObjTimed">
        <xsl:param name="prefix" />
        <xsl:param name="type" />
        <func:result>
            <xsl:if test="@timed='yes'">
                <xsl:call-template name="DEVEL_DEBUG">
                    <xsl:with-param name="function" select="concat(concat('put',$type),'InObject')"/>
                    <xsl:with-param name="attr" select="@name"/>          
                </xsl:call-template>
                <xsl:text>&#009;&#009;obj = ull.</xsl:text>
                <xsl:value-of select="$prefix"/><xsl:value-of select="$type"/>
                <xsl:text>InObject(self.idx, obj, cpopath + '</xsl:text><xsl:value-of select="@name"/>
                <xsl:text>', i, </xsl:text>

                <xsl:choose>
                    <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Int')">
                        <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                        <xsl:text>).astype(numpy.int32)</xsl:text>
                    </xsl:when>
                    <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Double')">
                        <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                        <xsl:text>).astype(numpy.float64)</xsl:text>
                    </xsl:when>
		     <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Complex')">
                        <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                        <xsl:text>).astype(numpy.complex128)</xsl:text>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:text>self.</xsl:text><xsl:value-of select = "@name"/>
                    </xsl:otherwise>
                </xsl:choose>
                <xsl:text>)&#xA;</xsl:text>          
            </xsl:if>
        </func:result>
    </func:function>

    <!--Generation of the UAL low level call for a field within the putTimedElt function-->
    <xsl:template match = "field" mode = "PUTOBJ_TIMED">
        <xsl:choose>
            <xsl:when test="@type='xs:string'"><xsl:value-of select="my:gen-putObjTimed('put','String')"/></xsl:when>
            <xsl:when test="@type='xs:integer'"><xsl:value-of select="my:gen-putObjTimed('put','Int')"/></xsl:when>
            <xsl:when test="@type='xs:boolean'"><xsl:value-of select="my:gen-putObjTimed('put','Int')"/></xsl:when>
            <xsl:when test="@type='xs:double'"><xsl:value-of select="my:gen-putObjTimed('put','Double')"/></xsl:when>
            <xsl:when test="@type='xs:float'"><xsl:value-of select="my:gen-putObjTimed('put','Double')"/></xsl:when>
	    <xsl:when test="@type='cplx_type'"><xsl:value-of select="my:gen-putObjTimed('put','Complex')"/></xsl:when>
            <xsl:when test="@type='vecstring_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect1DString')"/></xsl:when>
            <xsl:when test="@type='vecflt_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecdbl_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecint_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect1DInt')"/></xsl:when>
	    <xsl:when test="@type='veccplx_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect1DComplex')"/></xsl:when>
            <xsl:when test="@type='matflt_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matdbl_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matint_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect2DInt')"/></xsl:when>
	    <xsl:when test="@type='matcplx_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect2DComplex')"/></xsl:when>
            <xsl:when test="@type='array3dflt_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect3DDouble')"/></xsl:when>
            <xsl:when test="@type='array3dint_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect3DInt')"/></xsl:when>
            <xsl:when test="@type='array3ddbl_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect3DDouble')"/></xsl:when>
	    <xsl:when test="@type='array3dcplx_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect3DComplex')"/></xsl:when>
            <xsl:when test="@type='array4dflt_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect4DDouble')"/></xsl:when>
            <xsl:when test="@type='array4dint_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect4DInt')"/></xsl:when>
            <xsl:when test="@type='array4ddbl_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect4DDouble')"/></xsl:when>
   	    <xsl:when test="@type='array4dcplx_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect4DComplex')"/></xsl:when>
            <xsl:when test="@type='array5dflt_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect5DDouble')"/></xsl:when>
            <xsl:when test="@type='array5dint_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect5DInt')"/></xsl:when>
            <xsl:when test="@type='array5ddbl_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect5DDouble')"/></xsl:when>
	    <xsl:when test="@type='array5dcplx_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect5DComplex')"/></xsl:when>
            <xsl:when test="@type='array6dflt_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect6DDouble')"/></xsl:when>
            <xsl:when test="@type='array6dint_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect6DInt')"/></xsl:when>
            <xsl:when test="@type='array6ddbl_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect6DDouble')"/></xsl:when>
	    <xsl:when test="@type='array6dcplx_type'"><xsl:value-of select="my:gen-putObjTimed('put','Vect6DComplex')"/></xsl:when>
            <xsl:when test="@type='structure' or @type='struct_array'">
                <xsl:text>&#009;&#009;obj = self.</xsl:text><xsl:value-of select="@name"/><xsl:text>.putTimedElt(path, cpopath + '</xsl:text>
                <xsl:value-of select="@name"/><xsl:text>', i, obj)&#xA;</xsl:text>
            </xsl:when>
	    <xsl:otherwise>
		<xsl:message terminate="no">    ERROR! Cannot define PUTOBJ_TIMED method for unknown type: <xsl:value-of select="@type"/>!  </xsl:message>
	    </xsl:otherwise>
        </xsl:choose>
    </xsl:template>




    <!--Little function used in the following template to make it more readable-->
    <func:function name="my:gen-putObjNonTimed">
        <xsl:param name="prefix" />
        <xsl:param name="type" />
        <func:result>
            <xsl:if test="@timed='no'">
                <xsl:call-template name="DEVEL_DEBUG">
                    <xsl:with-param name="function" select="concat(concat('put',$type),'InObject')"/>
                    <xsl:with-param name="attr" select="@name"/>   
                </xsl:call-template>
                <xsl:text>&#009;&#009;obj = ull.</xsl:text>
                <xsl:value-of select="$prefix"/><xsl:value-of select="$type"/>
                <xsl:text>InObject(self.idx, obj, cpopath + '</xsl:text><xsl:value-of select="@name"/>
                <xsl:text>', i, </xsl:text>

                <xsl:choose>
                    <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Int')">
                        <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                        <xsl:text>).astype(numpy.int32)</xsl:text>
                    </xsl:when>
                    <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Double')">
                        <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                        <xsl:text>).astype(numpy.float64)</xsl:text>
                    </xsl:when>
	            <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Complex')">
                        <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                        <xsl:text>).astype(numpy.complex128)</xsl:text>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:text>self.</xsl:text><xsl:value-of select = "@name"/>
                    </xsl:otherwise>
                </xsl:choose>
                <xsl:text>)&#xA;</xsl:text>
            </xsl:if>
        </func:result>
    </func:function>

    <!--Generation of the UAL low level call for a field within the putTimedElt function-->
    <xsl:template match = "field" mode = "PUTOBJ_NONTIMED">
        <xsl:choose>
            <xsl:when test="@type='xs:string'"><xsl:value-of select="my:gen-putObjNonTimed('put','String')"/></xsl:when>
            <xsl:when test="@type='xs:integer'"><xsl:value-of select="my:gen-putObjNonTimed('put','Int')"/></xsl:when>
            <xsl:when test="@type='xs:boolean'"><xsl:value-of select="my:gen-putObjNonTimed('put','Int')"/></xsl:when>
            <xsl:when test="@type='xs:double'"><xsl:value-of select="my:gen-putObjNonTimed('put','Double')"/></xsl:when>
            <xsl:when test="@type='xs:float'"><xsl:value-of select="my:gen-putObjNonTimed('put','Double')"/></xsl:when>
	    <xsl:when test="@type='cplx_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Complex')"/></xsl:when>
            <xsl:when test="@type='vecstring_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect1DString')"/></xsl:when>
            <xsl:when test="@type='vecflt_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecdbl_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecint_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect1DInt')"/></xsl:when>
	    <xsl:when test="@type='veccplx_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect1DComplex')"/></xsl:when>
            <xsl:when test="@type='matflt_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matdbl_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matint_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect2DInt')"/></xsl:when>
	    <xsl:when test="@type='matcplx_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect2DComplex')"/></xsl:when>
            <xsl:when test="@type='array3dflt_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect3DDouble')"/></xsl:when>
            <xsl:when test="@type='array3dint_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect3DInt')"/></xsl:when>
            <xsl:when test="@type='array3ddbl_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect3DDouble')"/></xsl:when>
	    <xsl:when test="@type='array3dcplx_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect3DComplex')"/></xsl:when>
            <xsl:when test="@type='array4dflt_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect4DDouble')"/></xsl:when>
            <xsl:when test="@type='array4dint_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect4DInt')"/></xsl:when>
            <xsl:when test="@type='array4ddbl_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect4DDouble')"/></xsl:when>
	    <xsl:when test="@type='array4dcplx_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect4DComplex')"/></xsl:when>
            <xsl:when test="@type='array5dflt_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect5DDouble')"/></xsl:when>
            <xsl:when test="@type='array5dint_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect5DInt')"/></xsl:when>
            <xsl:when test="@type='array5ddbl_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect5DDouble')"/></xsl:when>
	    <xsl:when test="@type='array5dcplx_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect5DComplex')"/></xsl:when>
            <xsl:when test="@type='array6dflt_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect6DDouble')"/></xsl:when>
            <xsl:when test="@type='array6dint_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect6DInt')"/></xsl:when>
            <xsl:when test="@type='array6ddbl_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect6DDouble')"/></xsl:when>
	    <xsl:when test="@type='array6dcplx_type'"><xsl:value-of select="my:gen-putObjNonTimed('put','Vect6DComplex')"/></xsl:when>
            <xsl:when test="@type='structure' or @type='struct_array'">
                <xsl:text>&#009;&#009;obj = self.</xsl:text><xsl:value-of select="@name"/><xsl:text>.putNonTimedElt(path, cpopath + '</xsl:text>
                <xsl:value-of select="@name"/><xsl:text>', i, obj)&#xA;</xsl:text>
            </xsl:when>
	    <xsl:otherwise>
		<xsl:message terminate="no">    ERROR! Cannot define PUTOBJ_NONTIMED method for unknown type: <xsl:value-of select="@type"/>!  </xsl:message>
	    </xsl:otherwise>
        </xsl:choose>
    </xsl:template>




    <!--Little function used in the following template to make it more readable-->
    <func:function name="my:gen-getObjTimed">
        <xsl:param name="prefix" />
        <xsl:param name="type" />
        <func:result>
            <xsl:if test="@timed='yes'">
                <xsl:call-template name="DEVEL_DEBUG">
                    <xsl:with-param name="function" select="concat(concat('get',$type),'InObject')"/>
                    <xsl:with-param name="attr" select="@name"/>   
                </xsl:call-template>
                <xsl:text>&#009;&#009;status, ret_</xsl:text><xsl:value-of select = "@name"/><xsl:text> = ull.</xsl:text>
                <xsl:value-of select="$prefix"/><xsl:value-of select="$type"/>
                <xsl:text>FromObject(self.idx, obj, cpopath + '</xsl:text><xsl:value-of select="@name"/>
                <xsl:text>', i)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;if not status:&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;self.</xsl:text><xsl:value-of select = "@name"/>
                <xsl:text> = ret_</xsl:text><xsl:value-of select = "@name"/><xsl:text>&#xA;</xsl:text>
            </xsl:if>
        </func:result>
    </func:function>

    <!-- Generation of the UAL low level call for a field within the getTimedElt function-->
    <xsl:template match = "field" mode = "GETOBJ_TIMED">
        <xsl:choose>
            <xsl:when test="@type='xs:string'"><xsl:value-of select="my:gen-getObjTimed('get','String')"/></xsl:when>
            <xsl:when test="@type='xs:integer'"><xsl:value-of select="my:gen-getObjTimed('get','Int')"/></xsl:when>
            <xsl:when test="@type='xs:boolean'"><xsl:value-of select="my:gen-getObjTimed('get','Int')"/></xsl:when>
            <xsl:when test="@type='xs:double'"><xsl:value-of select="my:gen-getObjTimed('get','Double')"/></xsl:when>
            <xsl:when test="@type='xs:float'"><xsl:value-of select="my:gen-getObjTimed('get','Double')"/></xsl:when>
	    <xsl:when test="@type='cplx_type'"><xsl:value-of select="my:gen-getObjTimed('get','Complex')"/></xsl:when>
            <xsl:when test="@type='vecstring_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect1DString')"/></xsl:when>
            <xsl:when test="@type='vecflt_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecdbl_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecint_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect1DInt')"/></xsl:when>
	    <xsl:when test="@type='veccplx_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect1DComplex')"/></xsl:when>
            <xsl:when test="@type='matflt_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matdbl_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matint_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect2DInt')"/></xsl:when>
	    <xsl:when test="@type='matcplx_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect2DComplex')"/></xsl:when>
            <xsl:when test="@type='array3dflt_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect3DDouble')"/></xsl:when>
            <xsl:when test="@type='array3dint_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect3DInt')"/></xsl:when>
            <xsl:when test="@type='array3ddbl_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect3DDouble')"/></xsl:when>
	    <xsl:when test="@type='array3dcplx_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect3DComplex')"/></xsl:when>
            <xsl:when test="@type='array4dflt_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect4DDouble')"/></xsl:when>
            <xsl:when test="@type='array4dint_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect4DInt')"/></xsl:when>
            <xsl:when test="@type='array4ddbl_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect4DDouble')"/></xsl:when>
	    <xsl:when test="@type='array4dcplx_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect4DComplex')"/></xsl:when>
            <xsl:when test="@type='array5dflt_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect5DDouble')"/></xsl:when>
            <xsl:when test="@type='array5dint_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect5DInt')"/></xsl:when>
            <xsl:when test="@type='array5ddbl_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect5DDouble')"/></xsl:when>
	    <xsl:when test="@type='array5dcplx_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect5DComplex')"/></xsl:when>
            <xsl:when test="@type='array6dflt_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect6DDouble')"/></xsl:when>
            <xsl:when test="@type='array6dint_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect6DInt')"/></xsl:when>
            <xsl:when test="@type='array6ddbl_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect6DDouble')"/></xsl:when>
	    <xsl:when test="@type='array6dcplx_type'"><xsl:value-of select="my:gen-getObjTimed('get','Vect6DComplex')"/></xsl:when>
            <xsl:when test="@type='structure' or @type='struct_array'">
                <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@name"/><xsl:text>.getTimedElt(path, cpopath + '</xsl:text>
                <xsl:value-of select="@name"/><xsl:text>', i, obj)&#xA;</xsl:text>
            </xsl:when>
	     <xsl:otherwise>
		<xsl:message terminate="no">    ERROR! Cannot define GETOBJ_TIMED method for unknown type: <xsl:value-of select="@type"/>!  </xsl:message>
	    </xsl:otherwise>
        </xsl:choose>
    </xsl:template>



    <!--Little function used in the following template to make it more readable-->
    <func:function name="my:gen-getObjNonTimed">
        <xsl:param name="prefix" />
        <xsl:param name="type" />
        <func:result>
	    <xsl:choose>
            <xsl:when test="@timed='no'">
                <xsl:call-template name="DEVEL_DEBUG">
                    <xsl:with-param name="function" select="concat(concat('get',$type),'InObject')"/>
                    <xsl:with-param name="attr" select="@name"/>   
                </xsl:call-template>
                <xsl:text>&#009;&#009;status, ret_</xsl:text><xsl:value-of select = "@name"/><xsl:text> = ull.</xsl:text>
                <xsl:value-of select="$prefix"/><xsl:value-of select="$type"/>
                <xsl:text>FromObject(self.idx, obj, cpopath + '</xsl:text><xsl:value-of select="@name"/>
                <xsl:text>', i)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>
                <xsl:text>&#009;&#009;if not status:&#xA;</xsl:text>
                <xsl:text>&#009;&#009;&#009;self.</xsl:text><xsl:value-of select = "@name"/>
                <xsl:text> = ret_</xsl:text><xsl:value-of select = "@name"/><xsl:text>&#xA;</xsl:text>
            </xsl:when>   
	     <xsl:otherwise>
		<!-- <xsl:message terminate="no">    ERROR! Data inconsistency: my:gen-getObjNonTimed called for timed != NO! </xsl:message>  -->
	    </xsl:otherwise>
	    </xsl:choose>
        </func:result>
    </func:function>

    <!-- Generation of the UAL low level call for a field within the getNonTimedElt function-->
    <xsl:template match = "field" mode = "GETOBJ_NONTIMED">
        <xsl:choose>
            <xsl:when test="@type='xs:string'"><xsl:value-of select="my:gen-getObjNonTimed('get','String')"/></xsl:when>
            <xsl:when test="@type='xs:integer'"><xsl:value-of select="my:gen-getObjNonTimed('get','Int')"/></xsl:when>
            <xsl:when test="@type='xs:boolean'"><xsl:value-of select="my:gen-getObjNonTimed('get','Int')"/></xsl:when>
            <xsl:when test="@type='xs:double'"><xsl:value-of select="my:gen-getObjNonTimed('get','Double')"/></xsl:when>
            <xsl:when test="@type='xs:float'"><xsl:value-of select="my:gen-getObjNonTimed('get','Double')"/></xsl:when>
	    <xsl:when test="@type='cplx_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Complex')"/></xsl:when>
            <xsl:when test="@type='vecstring_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect1DString')"/></xsl:when>
            <xsl:when test="@type='vecflt_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecdbl_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect1DDouble')"/></xsl:when>
            <xsl:when test="@type='vecint_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect1DInt')"/></xsl:when>
	    <xsl:when test="@type='veccplx_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect1DComplex')"/></xsl:when>
            <xsl:when test="@type='matflt_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matdbl_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect2DDouble')"/></xsl:when>
            <xsl:when test="@type='matint_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect2DInt')"/></xsl:when>
	    <xsl:when test="@type='matcplx_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect2DComplex')"/></xsl:when>
            <xsl:when test="@type='array3dflt_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect3DDouble')"/></xsl:when>
            <xsl:when test="@type='array3dint_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect3DInt')"/></xsl:when>
            <xsl:when test="@type='array3ddbl_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect3DDouble')"/></xsl:when>
	    <xsl:when test="@type='array3dcplx_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect3DComplex')"/></xsl:when>
            <xsl:when test="@type='array4dflt_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect4DDouble')"/></xsl:when>
            <xsl:when test="@type='array4dint_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect4DInt')"/></xsl:when>
            <xsl:when test="@type='array4ddbl_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect4DDouble')"/></xsl:when>
	    <xsl:when test="@type='array4dcplx_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect4DComplex')"/></xsl:when>
            <xsl:when test="@type='array5dflt_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect5DDouble')"/></xsl:when>
            <xsl:when test="@type='array5dint_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect5DInt')"/></xsl:when>
            <xsl:when test="@type='array5ddbl_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect5DDouble')"/></xsl:when>
	    <xsl:when test="@type='array5dcplx_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect5DComplex')"/></xsl:when>
            <xsl:when test="@type='array6dflt_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect6DDouble')"/></xsl:when>
            <xsl:when test="@type='array6dint_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect6DInt')"/></xsl:when>
            <xsl:when test="@type='array6ddbl_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect6DDouble')"/></xsl:when>
	    <xsl:when test="@type='array6dcplx_type'"><xsl:value-of select="my:gen-getObjNonTimed('get','Vect6DComplex')"/></xsl:when>
            <xsl:when test="@type='structure' or @type='struct_array'">
                <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@name"/><xsl:text>.getNonTimedElt(path, cpopath + '</xsl:text>
                <xsl:value-of select="@name"/><xsl:text>', i, obj)&#xA;</xsl:text>
            </xsl:when>
	    <xsl:otherwise>
		<xsl:message terminate="no">    ERROR! Cannot define GETOBJ_NONTIMED method for unknown type: <xsl:value-of select="@type"/>!  </xsl:message>
	    </xsl:otherwise>
        </xsl:choose>
    </xsl:template>




    <!--Little function used in the following template to make it more readable-->
    <!-- Three cases : two for timed field, they are treated in the putSlice method
    or in the replaceLastSlice method, and one for non timed field, they are 
    treated in the putNonTimed method. The treatment of the non timed field is
    exactly the same as in the put method
    -->
    <func:function name="my:gen-putSlice">
        <xsl:param name="prefix" />
        <xsl:param name="type" />
        <xsl:param name="method" />
        <func:result>

            <xsl:choose>
                <xsl:when test="@timed='yes' and $method='slice'">
                    <xsl:text>&#009;&#009;status = ull.</xsl:text><xsl:value-of select="$prefix"/><xsl:value-of select="$type"/>
                    <xsl:text>Slice(self.idx, path, cpopath + '</xsl:text>
                    <xsl:value-of select="@name"/><xsl:text>', </xsl:text>

                    <xsl:choose>
                        <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Int')">
                            <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                            <xsl:text>).astype(numpy.int32)</xsl:text>
                        </xsl:when>
                        <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Double')">
                            <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                            <xsl:text>).astype(numpy.float64)</xsl:text>
                        </xsl:when>
			<xsl:when test="contains(string($type),'Vect') and contains(string($type),'Complex')">
                            <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                            <xsl:text>).astype(numpy.complex128)</xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:text>self.</xsl:text><xsl:value-of select = "@name"/>
                        </xsl:otherwise>
                    </xsl:choose>
                    <xsl:if test="$prefix='put'">
                        <xsl:text>, self.cpoTime</xsl:text>
                    </xsl:if>
                    <xsl:text>)&#xA;</xsl:text>
                    <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>
                </xsl:when>

                <xsl:when test="@timed='no' and $method='non-timed'">
                    <xsl:text>&#009;&#009;status = ull.</xsl:text>
                    <xsl:value-of select="$prefix"/><xsl:value-of select="$type"/>
                    <xsl:text>(self.idx, path, cpopath + '</xsl:text><xsl:value-of select="@name"/><xsl:text>', </xsl:text>

                    <xsl:choose>
                        <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Int')">
                            <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                            <xsl:text>).astype(numpy.int32)</xsl:text>
                            <xsl:text>, False)&#xA;</xsl:text>
                        </xsl:when>
                        <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Double')">
                            <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                            <xsl:text>).astype(numpy.float64)</xsl:text>
                            <xsl:text>, False)&#xA;</xsl:text>
                        </xsl:when>
		        <xsl:when test="contains(string($type),'Vect') and contains(string($type),'Complex')">
                            <xsl:text>numpy.array(self.</xsl:text><xsl:value-of select = "@name"/>
                            <xsl:text>).astype(numpy.complex128)</xsl:text>
                            <xsl:text>, False)&#xA;</xsl:text>
                        </xsl:when>
                        <xsl:when test="contains(string($type),'Vect') and contains(string($type),'String')">
                            <xsl:text>self.</xsl:text><xsl:value-of select = "@name"/>
                            <xsl:text>, False)&#xA;</xsl:text>
                        </xsl:when>
                        <xsl:when test="string($type)='String'">
                            <xsl:text>self.</xsl:text><xsl:value-of select = "@name"/>
                         <!--   <xsl:text>, len(self.</xsl:text><xsl:value-of select = "@name"/> -->
                            <xsl:text>)&#xA;</xsl:text>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:text>self.</xsl:text><xsl:value-of select = "@name"/>
                            <xsl:text>)&#xA;</xsl:text>
                        </xsl:otherwise>
                    </xsl:choose>
                    <xsl:text>&#009;&#009;check_status(status)&#xA;</xsl:text>
                </xsl:when>
	     <xsl:otherwise>
	<!--	<xsl:message terminate="no">    ERROR! Datastructure inconsistency :  <xsl:value-of select="@path"/>!  <xsl:value-of select="@prefix"/>!  <xsl:value-of select="@timed"/>! <xsl:value-of select="@method"/>!   </xsl:message> -->
	    </xsl:otherwise>
            </xsl:choose>    
        </func:result>
    </func:function>

    <!--Generation of the UAL low level call for a field within the putSlice and putNonTimed functions-->
    <xsl:template match = "field" mode = "PUT_TIME">
        <xsl:param name="action"/>
        <xsl:param name="method"/>

        <xsl:choose>
            <xsl:when test="@type='xs:string'"><xsl:value-of select="my:gen-putSlice($action,'String',$method)"/></xsl:when>
            <xsl:when test="@type='xs:integer'"><xsl:value-of select="my:gen-putSlice($action,'Int',$method)"/></xsl:when>
            <xsl:when test="@type='xs:boolean'"><xsl:value-of select="my:gen-putSlice($action,'Int',$method)"/></xsl:when>
            <xsl:when test="@type='xs:double'"><xsl:value-of select="my:gen-putSlice($action,'Double',$method)"/></xsl:when>
            <xsl:when test="@type='xs:float'"><xsl:value-of select="my:gen-putSlice($action,'Double',$method)"/></xsl:when>
	    <xsl:when test="@type='cplx_type'"><xsl:value-of select="my:gen-putSlice($action,'Complex',$method)"/></xsl:when>
            <xsl:when test="@type='vecstring_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect1DString',$method)"/></xsl:when>
            <xsl:when test="@type='vecflt_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect1DDouble',$method)"/></xsl:when>
            <xsl:when test="@type='vecdbl_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect1DDouble',$method)"/></xsl:when>
            <xsl:when test="@type='vecint_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect1DInt',$method)"/></xsl:when>
	    <xsl:when test="@type='veccplx_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect1DComplex',$method)"/></xsl:when>
            <xsl:when test="@type='matflt_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect2DDouble',$method)"/></xsl:when>
            <xsl:when test="@type='matdbl_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect2DDouble',$method)"/></xsl:when>
            <xsl:when test="@type='matint_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect2DInt',$method)"/></xsl:when>
	    <xsl:when test="@type='matcplx_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect2DComplex',$method)"/></xsl:when>
            <xsl:when test="@type='array3dflt_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect3DDouble',$method)"/></xsl:when>
            <xsl:when test="@type='array3dint_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect3DInt',$method)"/></xsl:when>
            <xsl:when test="@type='array3ddbl_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect3DDouble',$method)"/></xsl:when>
	    <xsl:when test="@type='array3dcplx_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect3DComplex',$method)"/></xsl:when>
            <xsl:when test="@type='array4dflt_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect4DDouble',$method)"/></xsl:when>
            <xsl:when test="@type='array4dint_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect4DInt',$method)"/></xsl:when>
            <xsl:when test="@type='array4ddbl_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect4DDouble',$method)"/></xsl:when>
	    <xsl:when test="@type='array4dcplx_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect4DComplex',$method)"/></xsl:when>
            <xsl:when test="@type='array5dflt_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect5DDouble',$method)"/></xsl:when>
            <xsl:when test="@type='array5dint_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect5DInt',$method)"/></xsl:when>
            <xsl:when test="@type='array5ddbl_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect5DDouble',$method)"/></xsl:when>
	    <xsl:when test="@type='array5dcplx_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect5DComplex',$method)"/></xsl:when>
            <xsl:when test="@type='array6dflt_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect6DDouble',$method)"/></xsl:when>
            <xsl:when test="@type='array6dint_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect6DInt',$method)"/></xsl:when>
            <xsl:when test="@type='array6ddbl_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect6DDouble',$method)"/></xsl:when>
	    <xsl:when test="@type='array6dcplx_type'"><xsl:value-of select="my:gen-putSlice($action,'Vect6DComplex',$method)"/></xsl:when>
            <xsl:when test="@type='structure' or @type='struct_array'">
                <xsl:choose>
                    <xsl:when test="$method='slice'">
                        <!-- propagate time -->
                        <xsl:if test="$action='put'">
                            <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@name"/><xsl:text>.cpoTime = self.cpoTime&#xA;</xsl:text>
                        </xsl:if>
                        <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@name"/>
                        <xsl:text>.</xsl:text><xsl:value-of select="$action"/><xsl:text>Slice(path, cpopath)&#xA;</xsl:text>
                    </xsl:when>
                    <xsl:when test="$method='non-timed'">
                        <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@name"/><xsl:text>.putNonTimed(path, cpopath)&#xA;</xsl:text>
                    </xsl:when>
                </xsl:choose>
            </xsl:when>
	    <xsl:otherwise>
		<xsl:message terminate="no">    ERROR! Cannot define PUT_TIME method for unknown type: <xsl:value-of select="@type"/>!  </xsl:message>
	    </xsl:otherwise>
        </xsl:choose>  
    </xsl:template>


    <!--Generation of the UAL low level call for a field within the flushCache function-->
    <xsl:template match = "field" mode = "FLUSH">
        <xsl:choose>
            <xsl:when test="@type='structure'">
                <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@name"/>
                <xsl:text>.flushCache(path, cpopath)&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="@type='struct_array'">
                <xsl:text>&#009;&#009;ull.euitm_flush(self.idx, path, cpopath + '</xsl:text>
                <xsl:value-of select="@name"/><xsl:text>')&#xA;</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>&#009;&#009;ull.euitm_flush(self.idx, path, cpopath + '</xsl:text>
                <xsl:value-of select="@name"/><xsl:text>')&#xA;</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <!--Generation of the UAL low level call for a field within the discardMemCache function-->
    <xsl:template match = "field" mode = "DISCARD">
        <xsl:choose>
            <xsl:when test="@type='structure'">
                <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@name"/>
                <xsl:text>.discardMemCache(path, cpopath)&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="@type='struct_array'">
                <xsl:text>&#009;&#009;ull.euitm_discard_mem(self.idx, path, cpopath + '</xsl:text>
                <xsl:value-of select="@name"/><xsl:text>')&#xA;</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>&#009;&#009;ull.euitm_discard_mem(self.idx, path, cpopath + '</xsl:text>
                <xsl:value-of select="@name"/><xsl:text>')&#xA;</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <!--Generation of the UAL low level call for a field within the discardMemCache function-->
    <xsl:template match = "field" mode = "DELETE">
        <xsl:choose>
            <xsl:when test="@type='structure'">
                <xsl:text>&#009;&#009;self.</xsl:text><xsl:value-of select="@name"/>
                <xsl:text>.deleteData(path, cpopath)&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="@type='struct_array'">
                <xsl:text>&#009;&#009;ull.deleteData(self.idx, path, cpopath + '</xsl:text>
                <xsl:value-of select="@name"/><xsl:text>')&#xA;</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>&#009;&#009;ull.deleteData(self.idx, path, cpopath + '</xsl:text>
                <xsl:value-of select="@name"/><xsl:text>')&#xA;</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


</xsl:stylesheet>
