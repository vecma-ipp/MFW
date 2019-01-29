# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class bb_shield:
	'''
	class bb_shield
	Breeding blanket and relevant shield. CPO. Time-dependent CPO.

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- type : str
	   Type of breeding blanket (HCLL, DCLL, HCPB, ...). String
	- limits : class limitsstructurelimits
	   Limits
	- li6_enrich : float
	   Lithium 6 enrichement (at%).
	- geom : class geomstructuregeom
	   Geometry between components
	- neut_results : class neut_resultsstructureneut_results
	   Neutronic results
	- shield : class shieldstructureshield
	   Shield
	- bb : class bbstructurebb
	   Breeding blanket
	- hcll : class hcllstructurehcll
	   Data specific to HCLL blanket concept
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar
	'''

	def __init__(self):
		self.base_path = 'bb_shield'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 3
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.type = ''
		self.limits = limitsstructurelimits('limits')
		self.li6_enrich = EMPTY_DOUBLE
		self.geom = geomstructuregeom('geom')
		self.neut_results = neut_resultsstructureneut_results('neut_results')
		self.shield = shieldstructureshield('shield')
		self.bb = bbstructurebb('bb')
		self.hcll = hcllstructurehcll('hcll')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class bb_shield\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute type: ' + str(self.type) + '\n'
		ret = ret + space + 'Attribute limits\n ' + self.limits.__str__(depth+1)
		ret = ret + space + 'Attribute li6_enrich: ' + str(self.li6_enrich) + '\n'
		ret = ret + space + 'Attribute geom\n ' + self.geom.__str__(depth+1)
		ret = ret + space + 'Attribute neut_results\n ' + self.neut_results.__str__(depth+1)
		ret = ret + space + 'Attribute shield\n ' + self.shield.__str__(depth+1)
		ret = ret + space + 'Attribute bb\n ' + self.bb.__str__(depth+1)
		ret = ret + space + 'Attribute hcll\n ' + self.hcll.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.limits.setExpIdx(idx)
		self.geom.setExpIdx(idx)
		self.neut_results.setExpIdx(idx)
		self.shield.setExpIdx(idx)
		self.bb.setExpIdx(idx)
		self.hcll.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def getMaxOccurrences(self):
		return self.maxOccurrences

	def getCPOName(self):
		return self.base_path

	def putSlice(self, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		self.cpoTime = self.time
		status = ull.beginCPOPutSlice(self.idx, path)
		self.datainfo.cpoTime = self.cpoTime
		self.datainfo.putSlice(path, cpopath)
		self.limits.cpoTime = self.cpoTime
		self.limits.putSlice(path, cpopath)
		self.geom.cpoTime = self.cpoTime
		self.geom.putSlice(path, cpopath)
		self.neut_results.cpoTime = self.cpoTime
		self.neut_results.putSlice(path, cpopath)
		self.shield.cpoTime = self.cpoTime
		self.shield.putSlice(path, cpopath)
		self.bb.cpoTime = self.cpoTime
		self.bb.putSlice(path, cpopath)
		self.hcll.cpoTime = self.cpoTime
		self.hcll.putSlice(path, cpopath)
		self.codeparam.cpoTime = self.cpoTime
		self.codeparam.putSlice(path, cpopath)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.endCPOPutSlice(self.idx, path)

	def replaceLastSlice(self, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		self.cpoTime = self.time
		status = ull.beginCPOReplaceLastSlice(self.idx, path)
		self.datainfo.replaceLastSlice(path, cpopath)
		self.limits.replaceLastSlice(path, cpopath)
		self.geom.replaceLastSlice(path, cpopath)
		self.neut_results.replaceLastSlice(path, cpopath)
		self.shield.replaceLastSlice(path, cpopath)
		self.bb.replaceLastSlice(path, cpopath)
		self.hcll.replaceLastSlice(path, cpopath)
		self.codeparam.replaceLastSlice(path, cpopath)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.endCPOReplaceLastSlice(self.idx, path)

	def putNonTimed(self, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		self.deleteData(occurrence)
		status = ull.beginCPOPutNonTimed(self.idx, path)
		self.datainfo.putNonTimed(path, cpopath)
		status = ull.putString(self.idx, path, cpopath + 'type', self.type)
		check_status(status)
		self.limits.putNonTimed(path, cpopath)
		status = ull.putDouble(self.idx, path, cpopath + 'li6_enrich', self.li6_enrich)
		check_status(status)
		self.geom.putNonTimed(path, cpopath)
		self.neut_results.putNonTimed(path, cpopath)
		self.shield.putNonTimed(path, cpopath)
		self.bb.putNonTimed(path, cpopath)
		self.hcll.putNonTimed(path, cpopath)
		self.codeparam.putNonTimed(path, cpopath)
		status = ull.endCPOPutNonTimed(self.idx, path)

	def getSlice(self, inTime, interpolMode, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		self.cpoTime = self.time
		status = ull.beginCPOGetSlice(self.idx, path, inTime)
		self.datainfo.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_type = ull.getString(self.idx, path, cpopath + 'type')
		check_status(status)
		if not status:
			self.type = ret_type
		self.limits.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_li6_enrich = ull.getDouble(self.idx, path, cpopath + 'li6_enrich')
		check_status(status)
		if not status:
			self.li6_enrich = ret_li6_enrich
		self.geom.getSlice(path, cpopath, inTime, interpolMode)
		self.neut_results.getSlice(path, cpopath, inTime, interpolMode)
		self.shield.getSlice(path, cpopath, inTime, interpolMode)
		self.bb.getSlice(path, cpopath, inTime, interpolMode)
		self.hcll.getSlice(path, cpopath, inTime, interpolMode)
		self.codeparam.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status = ull.endCPOGetSlice(self.idx, path)

	def build_non_resampled_data(self, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		status, nbslice = ull.beginCPOGet(self.idx, path, True)
		check_status(status)
		array=None
		if nbslice > 0:
			datainfoList = self.datainfo.build_non_resampled_data(path, cpopath, nbslice)
			status, typeVal = ull.getString(self.idx, path, cpopath + 'type')
			check_status(status)
			limitsList = self.limits.build_non_resampled_data(path, cpopath, nbslice)
			status, li6_enrichVal = ull.getDouble(self.idx, path, cpopath + 'li6_enrich')
			check_status(status)
			geomList = self.geom.build_non_resampled_data(path, cpopath, nbslice)
			neut_resultsList = self.neut_results.build_non_resampled_data(path, cpopath, nbslice)
			shieldList = self.shield.build_non_resampled_data(path, cpopath, nbslice)
			bbList = self.bb.build_non_resampled_data(path, cpopath, nbslice)
			hcllList = self.hcll.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = bb_shield()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.type = typeVal
				slice.limits = limitsList[i]
				slice.li6_enrich = li6_enrichVal
				slice.geom = geomList[i]
				slice.neut_results = neut_resultsList[i]
				slice.shield = shieldList[i]
				slice.bb = bbList[i]
				slice.hcll = hcllList[i]
				slice.codeparam = codeparamList[i]
				slice.time = timeList[i].copy().astype(float)
				array.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		status = ull.endCPOGet(self.idx, path)
		return array

	def deleteData(self, occurrence=0):
		if occurrence==0:
			path = self.base_path
		else:
			path = self.base_path + '/' + str(occurrence)
		cpopath = '' 
		self.datainfo.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'type')
		self.limits.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'li6_enrich')
		self.geom.deleteData(path, cpopath)
		self.neut_results.deleteData(path, cpopath)
		self.shield.deleteData(path, cpopath)
		self.bb.deleteData(path, cpopath)
		self.hcll.deleteData(path, cpopath)
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class bb_shieldArray:
	'''
	class bb_shieldArray
	Breeding blanket and relevant shield. CPO. Time-dependent CPO.

	Attributes:
	- array : list of bb_shield
	   Each list element correspond to one time slice.
	'''

	def __init__(self):
		self.array = []
		self.idx = EMPTY_INT
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class bb_shieldArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'bb_shield cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def put(self, occurrence=0):
		if (len(self.array)>0):
			self.array[0].putNonTimed(occurrence)
			for i in self.array:
				i.putSlice(occurrence)
		else:
			print('no time slice to be put')

	def get(self, occurrence=0):
		cpo = bb_shield()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(bb_shield())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = bb_shield()
		cpo.setExpIdx(self.idx)
		cpo.deleteData(occurrence)


class datainfostructuredatainfo:
	'''
	class datainfostructuredatainfo
	Generic information on a data item

	Attributes:
	- dataprovider : str
	   Name of the actual data provider (the person who filled the data)
	- putdate : str
	   Date at which the data has been put in the DB
	- source : str
	   Exact reference of the data source (e.g. original reference in the native machine data base)
	- comment : str
	   Any additional comment
	- cocos : int
	   COordinates COnventionS followed by this CPO
	- id : int
	   CPO id for checking its provenance in the workflow
	- isref : int
	   1 if the data can be found in the present data base entry; 2 if the data can be found in a parent data base entry; 0 if no data consistent with the present entry can be found.
	- whatref : class whatrefstructurewhatref
	   Structure defining a database entry and the CPO occurrence
	- putinfo : class putinfostructureputinfo
	   Level 2 information describing how to retrieve the actual data for the UAL. Not to be filled/used by the ITM user !
	'''

	def __init__(self, base_path_in='datainfo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dataprovider = ''
		self.putdate = ''
		self.source = ''
		self.comment = ''
		self.cocos = EMPTY_INT
		self.id = EMPTY_INT
		self.isref = EMPTY_INT
		self.whatref = whatrefstructurewhatref('whatref')
		self.putinfo = putinfostructureputinfo('putinfo')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class datainfostructuredatainfo\n'
		ret = ret + space + 'Attribute dataprovider: ' + str(self.dataprovider) + '\n'
		ret = ret + space + 'Attribute putdate: ' + str(self.putdate) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute comment: ' + str(self.comment) + '\n'
		ret = ret + space + 'Attribute cocos: ' + str(self.cocos) + '\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute isref: ' + str(self.isref) + '\n'
		ret = ret + space + 'Attribute whatref\n ' + self.whatref.__str__(depth+1)
		ret = ret + space + 'Attribute putinfo\n ' + self.putinfo.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.whatref.setExpIdx(idx)
		self.putinfo.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type datainfostructuredatainfo, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.whatref.cpoTime = self.cpoTime
		self.whatref.putSlice(path, cpopath)
		self.putinfo.cpoTime = self.cpoTime
		self.putinfo.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type datainfostructuredatainfo, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.whatref.replaceLastSlice(path, cpopath)
		self.putinfo.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type datainfostructuredatainfo, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'dataprovider', self.dataprovider)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'putdate', self.putdate)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'comment', self.comment)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'cocos', self.cocos)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'id', self.id)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'isref', self.isref)
		check_status(status)
		self.whatref.putNonTimed(path, cpopath)
		self.putinfo.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type datainfostructuredatainfo, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dataprovider = ull.getString(self.idx, path, cpopath + 'dataprovider')
		check_status(status)
		if not status:
			self.dataprovider = ret_dataprovider
		status, ret_putdate = ull.getString(self.idx, path, cpopath + 'putdate')
		check_status(status)
		if not status:
			self.putdate = ret_putdate
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_comment = ull.getString(self.idx, path, cpopath + 'comment')
		check_status(status)
		if not status:
			self.comment = ret_comment
		status, ret_cocos = ull.getInt(self.idx, path, cpopath + 'cocos')
		check_status(status)
		if not status:
			self.cocos = ret_cocos
		status, ret_id = ull.getInt(self.idx, path, cpopath + 'id')
		check_status(status)
		if not status:
			self.id = ret_id
		status, ret_isref = ull.getInt(self.idx, path, cpopath + 'isref')
		check_status(status)
		if not status:
			self.isref = ret_isref
		self.whatref.getSlice(path, cpopath, inTime, interpolMode)
		self.putinfo.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type datainfostructuredatainfo, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, dataproviderVal = ull.getString(self.idx, path, cpopath + 'dataprovider')
			check_status(status)
			status, putdateVal = ull.getString(self.idx, path, cpopath + 'putdate')
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, commentVal = ull.getString(self.idx, path, cpopath + 'comment')
			check_status(status)
			status, cocosVal = ull.getInt(self.idx, path, cpopath + 'cocos')
			check_status(status)
			status, idVal = ull.getInt(self.idx, path, cpopath + 'id')
			check_status(status)
			status, isrefVal = ull.getInt(self.idx, path, cpopath + 'isref')
			check_status(status)
			whatrefList = self.whatref.build_non_resampled_data(path, cpopath, nbslice)
			putinfoList = self.putinfo.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = datainfostructuredatainfo(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dataprovider = dataproviderVal
				slice.putdate = putdateVal
				slice.source = sourceVal
				slice.comment = commentVal
				slice.cocos = cocosVal
				slice.id = idVal
				slice.isref = isrefVal
				slice.whatref = whatrefList[i]
				slice.putinfo = putinfoList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type datainfostructuredatainfoObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type datainfostructuredatainfoObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type datainfostructuredatainfoObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'dataprovider') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'dataprovider', i, self.dataprovider)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'putdate') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'putdate', i, self.putdate)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'comment') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'comment', i, self.comment)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'cocos') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'cocos', i, self.cocos)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'id', i, self.id)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'isref') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'isref', i, self.isref)
		obj = self.whatref.putNonTimedElt(path, cpopath + 'whatref', i, obj)
		obj = self.putinfo.putNonTimedElt(path, cpopath + 'putinfo', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type datainfostructuredatainfoObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'dataprovider') 
			print ('obj = ' + str(obj))
		status, ret_dataprovider = ull.getStringFromObject(self.idx, obj, cpopath + 'dataprovider', i)
		check_status(status)
		if not status:
			self.dataprovider = ret_dataprovider
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'putdate') 
			print ('obj = ' + str(obj))
		status, ret_putdate = ull.getStringFromObject(self.idx, obj, cpopath + 'putdate', i)
		check_status(status)
		if not status:
			self.putdate = ret_putdate
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'comment') 
			print ('obj = ' + str(obj))
		status, ret_comment = ull.getStringFromObject(self.idx, obj, cpopath + 'comment', i)
		check_status(status)
		if not status:
			self.comment = ret_comment
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'cocos') 
			print ('obj = ' + str(obj))
		status, ret_cocos = ull.getIntFromObject(self.idx, obj, cpopath + 'cocos', i)
		check_status(status)
		if not status:
			self.cocos = ret_cocos
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getIntFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'isref') 
			print ('obj = ' + str(obj))
		status, ret_isref = ull.getIntFromObject(self.idx, obj, cpopath + 'isref', i)
		check_status(status)
		if not status:
			self.isref = ret_isref
		self.whatref.getNonTimedElt(path, cpopath + 'whatref', i, obj)
		self.putinfo.getNonTimedElt(path, cpopath + 'putinfo', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'dataprovider')
		ull.deleteData(self.idx, path, cpopath + 'putdate')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'comment')
		ull.deleteData(self.idx, path, cpopath + 'cocos')
		ull.deleteData(self.idx, path, cpopath + 'id')
		ull.deleteData(self.idx, path, cpopath + 'isref')
		self.whatref.deleteData(path, cpopath)
		self.putinfo.deleteData(path, cpopath)


class whatrefstructurewhatref:
	'''
	class whatrefstructurewhatref
	Structure defining a database entry and the CPO occurrence

	Attributes:
	- user : str
	   Name of the user if private data, public if public ITM database.
	- machine : str
	   Name of the device
	- shot : int
	   Shot number
	- run : int
	   Run number
	- occurrence : int
	   Occurrence number of the CPO in the reference entry
	'''

	def __init__(self, base_path_in='whatref'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.user = ''
		self.machine = ''
		self.shot = EMPTY_INT
		self.run = EMPTY_INT
		self.occurrence = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class whatrefstructurewhatref\n'
		ret = ret + space + 'Attribute user: ' + str(self.user) + '\n'
		ret = ret + space + 'Attribute machine: ' + str(self.machine) + '\n'
		ret = ret + space + 'Attribute shot: ' + str(self.shot) + '\n'
		ret = ret + space + 'Attribute run: ' + str(self.run) + '\n'
		ret = ret + space + 'Attribute occurrence: ' + str(self.occurrence) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type whatrefstructurewhatref, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type whatrefstructurewhatref, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type whatrefstructurewhatref, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'user', self.user)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'machine', self.machine)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'shot', self.shot)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'run', self.run)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'occurrence', self.occurrence)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type whatrefstructurewhatref, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_user = ull.getString(self.idx, path, cpopath + 'user')
		check_status(status)
		if not status:
			self.user = ret_user
		status, ret_machine = ull.getString(self.idx, path, cpopath + 'machine')
		check_status(status)
		if not status:
			self.machine = ret_machine
		status, ret_shot = ull.getInt(self.idx, path, cpopath + 'shot')
		check_status(status)
		if not status:
			self.shot = ret_shot
		status, ret_run = ull.getInt(self.idx, path, cpopath + 'run')
		check_status(status)
		if not status:
			self.run = ret_run
		status, ret_occurrence = ull.getInt(self.idx, path, cpopath + 'occurrence')
		check_status(status)
		if not status:
			self.occurrence = ret_occurrence

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type whatrefstructurewhatref, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, userVal = ull.getString(self.idx, path, cpopath + 'user')
			check_status(status)
			status, machineVal = ull.getString(self.idx, path, cpopath + 'machine')
			check_status(status)
			status, shotVal = ull.getInt(self.idx, path, cpopath + 'shot')
			check_status(status)
			status, runVal = ull.getInt(self.idx, path, cpopath + 'run')
			check_status(status)
			status, occurrenceVal = ull.getInt(self.idx, path, cpopath + 'occurrence')
			check_status(status)
			for i in range(nbslice):
				slice = whatrefstructurewhatref(self.base_path)
				slice.setExpIdx(self.idx)
				slice.user = userVal
				slice.machine = machineVal
				slice.shot = shotVal
				slice.run = runVal
				slice.occurrence = occurrenceVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type whatrefstructurewhatrefObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type whatrefstructurewhatrefObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type whatrefstructurewhatrefObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'user') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'user', i, self.user)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'machine') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'machine', i, self.machine)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'shot') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'shot', i, self.shot)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'run') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'run', i, self.run)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'occurrence') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'occurrence', i, self.occurrence)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type whatrefstructurewhatrefObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'user') 
			print ('obj = ' + str(obj))
		status, ret_user = ull.getStringFromObject(self.idx, obj, cpopath + 'user', i)
		check_status(status)
		if not status:
			self.user = ret_user
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'machine') 
			print ('obj = ' + str(obj))
		status, ret_machine = ull.getStringFromObject(self.idx, obj, cpopath + 'machine', i)
		check_status(status)
		if not status:
			self.machine = ret_machine
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'shot') 
			print ('obj = ' + str(obj))
		status, ret_shot = ull.getIntFromObject(self.idx, obj, cpopath + 'shot', i)
		check_status(status)
		if not status:
			self.shot = ret_shot
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'run') 
			print ('obj = ' + str(obj))
		status, ret_run = ull.getIntFromObject(self.idx, obj, cpopath + 'run', i)
		check_status(status)
		if not status:
			self.run = ret_run
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'occurrence') 
			print ('obj = ' + str(obj))
		status, ret_occurrence = ull.getIntFromObject(self.idx, obj, cpopath + 'occurrence', i)
		check_status(status)
		if not status:
			self.occurrence = ret_occurrence

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'user')
		ull.deleteData(self.idx, path, cpopath + 'machine')
		ull.deleteData(self.idx, path, cpopath + 'shot')
		ull.deleteData(self.idx, path, cpopath + 'run')
		ull.deleteData(self.idx, path, cpopath + 'occurrence')


class putinfostructureputinfo:
	'''
	class putinfostructureputinfo
	Level 2 information describing how to retrieve the actual data for the UAL. Not to be filled/used by the ITM user !

	Attributes:
	- putmethod : str
	   Storage method for this data
	- putaccess : str
	   Instructions to access the data using this method
	- putlocation : str
	   Name of this data under this method
	- rights : str
	   Access rights to this data
	'''

	def __init__(self, base_path_in='putinfo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.putmethod = ''
		self.putaccess = ''
		self.putlocation = ''
		self.rights = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class putinfostructureputinfo\n'
		ret = ret + space + 'Attribute putmethod: ' + str(self.putmethod) + '\n'
		ret = ret + space + 'Attribute putaccess: ' + str(self.putaccess) + '\n'
		ret = ret + space + 'Attribute putlocation: ' + str(self.putlocation) + '\n'
		ret = ret + space + 'Attribute rights: ' + str(self.rights) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type putinfostructureputinfo, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type putinfostructureputinfo, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type putinfostructureputinfo, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'putmethod', self.putmethod)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'putaccess', self.putaccess)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'putlocation', self.putlocation)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'rights', self.rights)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type putinfostructureputinfo, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_putmethod = ull.getString(self.idx, path, cpopath + 'putmethod')
		check_status(status)
		if not status:
			self.putmethod = ret_putmethod
		status, ret_putaccess = ull.getString(self.idx, path, cpopath + 'putaccess')
		check_status(status)
		if not status:
			self.putaccess = ret_putaccess
		status, ret_putlocation = ull.getString(self.idx, path, cpopath + 'putlocation')
		check_status(status)
		if not status:
			self.putlocation = ret_putlocation
		status, ret_rights = ull.getString(self.idx, path, cpopath + 'rights')
		check_status(status)
		if not status:
			self.rights = ret_rights

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type putinfostructureputinfo, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, putmethodVal = ull.getString(self.idx, path, cpopath + 'putmethod')
			check_status(status)
			status, putaccessVal = ull.getString(self.idx, path, cpopath + 'putaccess')
			check_status(status)
			status, putlocationVal = ull.getString(self.idx, path, cpopath + 'putlocation')
			check_status(status)
			status, rightsVal = ull.getString(self.idx, path, cpopath + 'rights')
			check_status(status)
			for i in range(nbslice):
				slice = putinfostructureputinfo(self.base_path)
				slice.setExpIdx(self.idx)
				slice.putmethod = putmethodVal
				slice.putaccess = putaccessVal
				slice.putlocation = putlocationVal
				slice.rights = rightsVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type putinfostructureputinfoObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type putinfostructureputinfoObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type putinfostructureputinfoObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'putmethod') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'putmethod', i, self.putmethod)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'putaccess') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'putaccess', i, self.putaccess)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'putlocation') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'putlocation', i, self.putlocation)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'rights') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'rights', i, self.rights)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type putinfostructureputinfoObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'putmethod') 
			print ('obj = ' + str(obj))
		status, ret_putmethod = ull.getStringFromObject(self.idx, obj, cpopath + 'putmethod', i)
		check_status(status)
		if not status:
			self.putmethod = ret_putmethod
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'putaccess') 
			print ('obj = ' + str(obj))
		status, ret_putaccess = ull.getStringFromObject(self.idx, obj, cpopath + 'putaccess', i)
		check_status(status)
		if not status:
			self.putaccess = ret_putaccess
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'putlocation') 
			print ('obj = ' + str(obj))
		status, ret_putlocation = ull.getStringFromObject(self.idx, obj, cpopath + 'putlocation', i)
		check_status(status)
		if not status:
			self.putlocation = ret_putlocation
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'rights') 
			print ('obj = ' + str(obj))
		status, ret_rights = ull.getStringFromObject(self.idx, obj, cpopath + 'rights', i)
		check_status(status)
		if not status:
			self.rights = ret_rights

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'putmethod')
		ull.deleteData(self.idx, path, cpopath + 'putaccess')
		ull.deleteData(self.idx, path, cpopath + 'putlocation')
		ull.deleteData(self.idx, path, cpopath + 'rights')


class limitsstructurelimits:
	'''
	class limitsstructurelimits
	Limits

	Attributes:
	- fw_dpa : float
	   max allowable displacement per atom on FW [dpa]; Scalar.
	- he_appm : float
	   He concentration limit in re-welding areas [appm]; Scalar
	- ins_dose : float
	   Integral radiation dose in insulator (Epoxy) [Gy] [J*Kg^-1]; Scalar
	- fn_flu : float
	   Peak fast neutron fluence (E>0.1 MeV) to the Nb3Sn superconductor  [m^-2]; Scalar
	- dpa_cu : float
	   Peak displacement damage to copper stabilizer [dpa]; Scalar
	- wp_nh : float
	   Peak nuclear eating in winding pack [W*m^-3]; Scalar
	'''

	def __init__(self, base_path_in='limits'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.fw_dpa = EMPTY_DOUBLE
		self.he_appm = EMPTY_DOUBLE
		self.ins_dose = EMPTY_DOUBLE
		self.fn_flu = EMPTY_DOUBLE
		self.dpa_cu = EMPTY_DOUBLE
		self.wp_nh = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class limitsstructurelimits\n'
		ret = ret + space + 'Attribute fw_dpa: ' + str(self.fw_dpa) + '\n'
		ret = ret + space + 'Attribute he_appm: ' + str(self.he_appm) + '\n'
		ret = ret + space + 'Attribute ins_dose: ' + str(self.ins_dose) + '\n'
		ret = ret + space + 'Attribute fn_flu: ' + str(self.fn_flu) + '\n'
		ret = ret + space + 'Attribute dpa_cu: ' + str(self.dpa_cu) + '\n'
		ret = ret + space + 'Attribute wp_nh: ' + str(self.wp_nh) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type limitsstructurelimits, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type limitsstructurelimits, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type limitsstructurelimits, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'fw_dpa', self.fw_dpa)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_appm', self.he_appm)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'ins_dose', self.ins_dose)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'fn_flu', self.fn_flu)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dpa_cu', self.dpa_cu)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'wp_nh', self.wp_nh)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type limitsstructurelimits, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_fw_dpa = ull.getDouble(self.idx, path, cpopath + 'fw_dpa')
		check_status(status)
		if not status:
			self.fw_dpa = ret_fw_dpa
		status, ret_he_appm = ull.getDouble(self.idx, path, cpopath + 'he_appm')
		check_status(status)
		if not status:
			self.he_appm = ret_he_appm
		status, ret_ins_dose = ull.getDouble(self.idx, path, cpopath + 'ins_dose')
		check_status(status)
		if not status:
			self.ins_dose = ret_ins_dose
		status, ret_fn_flu = ull.getDouble(self.idx, path, cpopath + 'fn_flu')
		check_status(status)
		if not status:
			self.fn_flu = ret_fn_flu
		status, ret_dpa_cu = ull.getDouble(self.idx, path, cpopath + 'dpa_cu')
		check_status(status)
		if not status:
			self.dpa_cu = ret_dpa_cu
		status, ret_wp_nh = ull.getDouble(self.idx, path, cpopath + 'wp_nh')
		check_status(status)
		if not status:
			self.wp_nh = ret_wp_nh

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type limitsstructurelimits, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, fw_dpaVal = ull.getDouble(self.idx, path, cpopath + 'fw_dpa')
			check_status(status)
			status, he_appmVal = ull.getDouble(self.idx, path, cpopath + 'he_appm')
			check_status(status)
			status, ins_doseVal = ull.getDouble(self.idx, path, cpopath + 'ins_dose')
			check_status(status)
			status, fn_fluVal = ull.getDouble(self.idx, path, cpopath + 'fn_flu')
			check_status(status)
			status, dpa_cuVal = ull.getDouble(self.idx, path, cpopath + 'dpa_cu')
			check_status(status)
			status, wp_nhVal = ull.getDouble(self.idx, path, cpopath + 'wp_nh')
			check_status(status)
			for i in range(nbslice):
				slice = limitsstructurelimits(self.base_path)
				slice.setExpIdx(self.idx)
				slice.fw_dpa = fw_dpaVal
				slice.he_appm = he_appmVal
				slice.ins_dose = ins_doseVal
				slice.fn_flu = fn_fluVal
				slice.dpa_cu = dpa_cuVal
				slice.wp_nh = wp_nhVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limitsstructurelimitsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limitsstructurelimitsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limitsstructurelimitsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fw_dpa') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fw_dpa', i, self.fw_dpa)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_appm') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_appm', i, self.he_appm)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ins_dose') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ins_dose', i, self.ins_dose)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fn_flu') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fn_flu', i, self.fn_flu)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dpa_cu') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dpa_cu', i, self.dpa_cu)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'wp_nh') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'wp_nh', i, self.wp_nh)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type limitsstructurelimitsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fw_dpa') 
			print ('obj = ' + str(obj))
		status, ret_fw_dpa = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fw_dpa', i)
		check_status(status)
		if not status:
			self.fw_dpa = ret_fw_dpa
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_appm') 
			print ('obj = ' + str(obj))
		status, ret_he_appm = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_appm', i)
		check_status(status)
		if not status:
			self.he_appm = ret_he_appm
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ins_dose') 
			print ('obj = ' + str(obj))
		status, ret_ins_dose = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ins_dose', i)
		check_status(status)
		if not status:
			self.ins_dose = ret_ins_dose
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fn_flu') 
			print ('obj = ' + str(obj))
		status, ret_fn_flu = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fn_flu', i)
		check_status(status)
		if not status:
			self.fn_flu = ret_fn_flu
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dpa_cu') 
			print ('obj = ' + str(obj))
		status, ret_dpa_cu = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dpa_cu', i)
		check_status(status)
		if not status:
			self.dpa_cu = ret_dpa_cu
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'wp_nh') 
			print ('obj = ' + str(obj))
		status, ret_wp_nh = ull.getDoubleFromObject(self.idx, obj, cpopath + 'wp_nh', i)
		check_status(status)
		if not status:
			self.wp_nh = ret_wp_nh

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'fw_dpa')
		ull.deleteData(self.idx, path, cpopath + 'he_appm')
		ull.deleteData(self.idx, path, cpopath + 'ins_dose')
		ull.deleteData(self.idx, path, cpopath + 'fn_flu')
		ull.deleteData(self.idx, path, cpopath + 'dpa_cu')
		ull.deleteData(self.idx, path, cpopath + 'wp_nh')


class geomstructuregeom:
	'''
	class geomstructuregeom
	Geometry between components

	Attributes:
	- dr_bb_sh_ib : float
	   Gap between the breeding blanket module and the shield (inboard) in the equatorial section [m]; Scalar
	- dr_sh_vv_ib : float
	   Gap between the shield and the vacuum vessel (inboard) in the equatorial section [m]; Scalar
	- dr_bb_sh_ob : float
	   Gap between the breeding blanket module and the shield (outboard) in the equatorial section [m]; Scalar
	- dr_sh_vv_ob : float
	   Gap between the shield and the vacuum vessel (outboard) in the equatorial section [m]; Scalar
	- dr_bb__sh_ib : float
	   Overal radial dimension of the ensemble BB plus shield (inboard) [m]; Scalar
	- dr_bb__sh_ob : float
	   Overal radial dimension of the ensemble BB plus shield (outboard) [m]; Scalar
	- delta_int : float
	   Distance between the inner plasma surface and the plasma facing side of the superconducting winding of the toroidal field coil [m]; Scalar
	'''

	def __init__(self, base_path_in='geom'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dr_bb_sh_ib = EMPTY_DOUBLE
		self.dr_sh_vv_ib = EMPTY_DOUBLE
		self.dr_bb_sh_ob = EMPTY_DOUBLE
		self.dr_sh_vv_ob = EMPTY_DOUBLE
		self.dr_bb__sh_ib = EMPTY_DOUBLE
		self.dr_bb__sh_ob = EMPTY_DOUBLE
		self.delta_int = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class geomstructuregeom\n'
		ret = ret + space + 'Attribute dr_bb_sh_ib: ' + str(self.dr_bb_sh_ib) + '\n'
		ret = ret + space + 'Attribute dr_sh_vv_ib: ' + str(self.dr_sh_vv_ib) + '\n'
		ret = ret + space + 'Attribute dr_bb_sh_ob: ' + str(self.dr_bb_sh_ob) + '\n'
		ret = ret + space + 'Attribute dr_sh_vv_ob: ' + str(self.dr_sh_vv_ob) + '\n'
		ret = ret + space + 'Attribute dr_bb__sh_ib: ' + str(self.dr_bb__sh_ib) + '\n'
		ret = ret + space + 'Attribute dr_bb__sh_ob: ' + str(self.dr_bb__sh_ob) + '\n'
		ret = ret + space + 'Attribute delta_int: ' + str(self.delta_int) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geomstructuregeom, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geomstructuregeom, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geomstructuregeom, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'dr_bb_sh_ib', self.dr_bb_sh_ib)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dr_sh_vv_ib', self.dr_sh_vv_ib)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dr_bb_sh_ob', self.dr_bb_sh_ob)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dr_sh_vv_ob', self.dr_sh_vv_ob)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dr_bb__sh_ib', self.dr_bb__sh_ib)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dr_bb__sh_ob', self.dr_bb__sh_ob)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'delta_int', self.delta_int)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type geomstructuregeom, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dr_bb_sh_ib = ull.getDouble(self.idx, path, cpopath + 'dr_bb_sh_ib')
		check_status(status)
		if not status:
			self.dr_bb_sh_ib = ret_dr_bb_sh_ib
		status, ret_dr_sh_vv_ib = ull.getDouble(self.idx, path, cpopath + 'dr_sh_vv_ib')
		check_status(status)
		if not status:
			self.dr_sh_vv_ib = ret_dr_sh_vv_ib
		status, ret_dr_bb_sh_ob = ull.getDouble(self.idx, path, cpopath + 'dr_bb_sh_ob')
		check_status(status)
		if not status:
			self.dr_bb_sh_ob = ret_dr_bb_sh_ob
		status, ret_dr_sh_vv_ob = ull.getDouble(self.idx, path, cpopath + 'dr_sh_vv_ob')
		check_status(status)
		if not status:
			self.dr_sh_vv_ob = ret_dr_sh_vv_ob
		status, ret_dr_bb__sh_ib = ull.getDouble(self.idx, path, cpopath + 'dr_bb__sh_ib')
		check_status(status)
		if not status:
			self.dr_bb__sh_ib = ret_dr_bb__sh_ib
		status, ret_dr_bb__sh_ob = ull.getDouble(self.idx, path, cpopath + 'dr_bb__sh_ob')
		check_status(status)
		if not status:
			self.dr_bb__sh_ob = ret_dr_bb__sh_ob
		status, ret_delta_int = ull.getDouble(self.idx, path, cpopath + 'delta_int')
		check_status(status)
		if not status:
			self.delta_int = ret_delta_int

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type geomstructuregeom, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, dr_bb_sh_ibVal = ull.getDouble(self.idx, path, cpopath + 'dr_bb_sh_ib')
			check_status(status)
			status, dr_sh_vv_ibVal = ull.getDouble(self.idx, path, cpopath + 'dr_sh_vv_ib')
			check_status(status)
			status, dr_bb_sh_obVal = ull.getDouble(self.idx, path, cpopath + 'dr_bb_sh_ob')
			check_status(status)
			status, dr_sh_vv_obVal = ull.getDouble(self.idx, path, cpopath + 'dr_sh_vv_ob')
			check_status(status)
			status, dr_bb__sh_ibVal = ull.getDouble(self.idx, path, cpopath + 'dr_bb__sh_ib')
			check_status(status)
			status, dr_bb__sh_obVal = ull.getDouble(self.idx, path, cpopath + 'dr_bb__sh_ob')
			check_status(status)
			status, delta_intVal = ull.getDouble(self.idx, path, cpopath + 'delta_int')
			check_status(status)
			for i in range(nbslice):
				slice = geomstructuregeom(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dr_bb_sh_ib = dr_bb_sh_ibVal
				slice.dr_sh_vv_ib = dr_sh_vv_ibVal
				slice.dr_bb_sh_ob = dr_bb_sh_obVal
				slice.dr_sh_vv_ob = dr_sh_vv_obVal
				slice.dr_bb__sh_ib = dr_bb__sh_ibVal
				slice.dr_bb__sh_ob = dr_bb__sh_obVal
				slice.delta_int = delta_intVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geomstructuregeomObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geomstructuregeomObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geomstructuregeomObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dr_bb_sh_ib') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dr_bb_sh_ib', i, self.dr_bb_sh_ib)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dr_sh_vv_ib') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dr_sh_vv_ib', i, self.dr_sh_vv_ib)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dr_bb_sh_ob') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dr_bb_sh_ob', i, self.dr_bb_sh_ob)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dr_sh_vv_ob') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dr_sh_vv_ob', i, self.dr_sh_vv_ob)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dr_bb__sh_ib') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dr_bb__sh_ib', i, self.dr_bb__sh_ib)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dr_bb__sh_ob') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dr_bb__sh_ob', i, self.dr_bb__sh_ob)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'delta_int') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'delta_int', i, self.delta_int)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geomstructuregeomObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dr_bb_sh_ib') 
			print ('obj = ' + str(obj))
		status, ret_dr_bb_sh_ib = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dr_bb_sh_ib', i)
		check_status(status)
		if not status:
			self.dr_bb_sh_ib = ret_dr_bb_sh_ib
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dr_sh_vv_ib') 
			print ('obj = ' + str(obj))
		status, ret_dr_sh_vv_ib = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dr_sh_vv_ib', i)
		check_status(status)
		if not status:
			self.dr_sh_vv_ib = ret_dr_sh_vv_ib
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dr_bb_sh_ob') 
			print ('obj = ' + str(obj))
		status, ret_dr_bb_sh_ob = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dr_bb_sh_ob', i)
		check_status(status)
		if not status:
			self.dr_bb_sh_ob = ret_dr_bb_sh_ob
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dr_sh_vv_ob') 
			print ('obj = ' + str(obj))
		status, ret_dr_sh_vv_ob = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dr_sh_vv_ob', i)
		check_status(status)
		if not status:
			self.dr_sh_vv_ob = ret_dr_sh_vv_ob
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dr_bb__sh_ib') 
			print ('obj = ' + str(obj))
		status, ret_dr_bb__sh_ib = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dr_bb__sh_ib', i)
		check_status(status)
		if not status:
			self.dr_bb__sh_ib = ret_dr_bb__sh_ib
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dr_bb__sh_ob') 
			print ('obj = ' + str(obj))
		status, ret_dr_bb__sh_ob = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dr_bb__sh_ob', i)
		check_status(status)
		if not status:
			self.dr_bb__sh_ob = ret_dr_bb__sh_ob
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'delta_int') 
			print ('obj = ' + str(obj))
		status, ret_delta_int = ull.getDoubleFromObject(self.idx, obj, cpopath + 'delta_int', i)
		check_status(status)
		if not status:
			self.delta_int = ret_delta_int

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'dr_bb_sh_ib')
		ull.deleteData(self.idx, path, cpopath + 'dr_sh_vv_ib')
		ull.deleteData(self.idx, path, cpopath + 'dr_bb_sh_ob')
		ull.deleteData(self.idx, path, cpopath + 'dr_sh_vv_ob')
		ull.deleteData(self.idx, path, cpopath + 'dr_bb__sh_ib')
		ull.deleteData(self.idx, path, cpopath + 'dr_bb__sh_ob')
		ull.deleteData(self.idx, path, cpopath + 'delta_int')


class neut_resultsstructureneut_results:
	'''
	class neut_resultsstructureneut_results
	Neutronic results

	Attributes:
	- tbr_bk : float
	   Resulting global breeding blanket tritium breeding ratio; Scalar
	- tbr_bk_inb : float
	   Resulting inboard breeding blanket Tritium Breeding Ratio [-]; Scalar
	- tbr_bk_outb : float
	   Resulting outboard breeding blanket Tritium Breeding Ratio [-]; Scalar
	- me_bk : float
	   Energy multiplication factor in breeding blanket; Scalar
	- me_shield : float
	   Energy multiplication factor in shield; Scalar
	- he_appm_res : float
	   He production in areas needing to be rewelded; Scalar
	- ins_dose_max : float
	   Integral radiation dose in insulator (Epoxy) [J*Kg^-1]; Scalar
	- fn_flu_max : float
	   Peak fast neutron fluence (E>0.1 MeV) to the Nb3Sn superconductor [m^-2]; Scalar
	- dpa_cu_max : float
	   Peak displacement damage to copper stabilizer [dpa]; Scalar
	- fn_flux_bz : float
	   Fast neutron flux in breeding zone inboard [m^2.s^-1]; Scalar
	- fn_flux_bp : float
	   Fast neutron flux in backplate inboard [m^2.s^-1]; Scalar
	- fn_flux_man : float
	   Fast neutron flux in manifold inboard [m^2.s^-1]; Scalar
	- fn_flux_sh : float
	   Fast neutron flux in shield inboard [m^2.s^-1]; Scalar
	- p_nh_bk : float
	   Total nuclear heating in blanket [W]; Scalar
	- p_nh_sh : float
	   Total nuclear heating in shield [W]; Scalar
	'''

	def __init__(self, base_path_in='neut_results'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.tbr_bk = EMPTY_DOUBLE
		self.tbr_bk_inb = EMPTY_DOUBLE
		self.tbr_bk_outb = EMPTY_DOUBLE
		self.me_bk = EMPTY_DOUBLE
		self.me_shield = EMPTY_DOUBLE
		self.he_appm_res = EMPTY_DOUBLE
		self.ins_dose_max = EMPTY_DOUBLE
		self.fn_flu_max = EMPTY_DOUBLE
		self.dpa_cu_max = EMPTY_DOUBLE
		self.fn_flux_bz = EMPTY_DOUBLE
		self.fn_flux_bp = EMPTY_DOUBLE
		self.fn_flux_man = EMPTY_DOUBLE
		self.fn_flux_sh = EMPTY_DOUBLE
		self.p_nh_bk = EMPTY_DOUBLE
		self.p_nh_sh = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class neut_resultsstructureneut_results\n'
		ret = ret + space + 'Attribute tbr_bk: ' + str(self.tbr_bk) + '\n'
		ret = ret + space + 'Attribute tbr_bk_inb: ' + str(self.tbr_bk_inb) + '\n'
		ret = ret + space + 'Attribute tbr_bk_outb: ' + str(self.tbr_bk_outb) + '\n'
		ret = ret + space + 'Attribute me_bk: ' + str(self.me_bk) + '\n'
		ret = ret + space + 'Attribute me_shield: ' + str(self.me_shield) + '\n'
		ret = ret + space + 'Attribute he_appm_res: ' + str(self.he_appm_res) + '\n'
		ret = ret + space + 'Attribute ins_dose_max: ' + str(self.ins_dose_max) + '\n'
		ret = ret + space + 'Attribute fn_flu_max: ' + str(self.fn_flu_max) + '\n'
		ret = ret + space + 'Attribute dpa_cu_max: ' + str(self.dpa_cu_max) + '\n'
		ret = ret + space + 'Attribute fn_flux_bz: ' + str(self.fn_flux_bz) + '\n'
		ret = ret + space + 'Attribute fn_flux_bp: ' + str(self.fn_flux_bp) + '\n'
		ret = ret + space + 'Attribute fn_flux_man: ' + str(self.fn_flux_man) + '\n'
		ret = ret + space + 'Attribute fn_flux_sh: ' + str(self.fn_flux_sh) + '\n'
		ret = ret + space + 'Attribute p_nh_bk: ' + str(self.p_nh_bk) + '\n'
		ret = ret + space + 'Attribute p_nh_sh: ' + str(self.p_nh_sh) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neut_resultsstructureneut_results, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neut_resultsstructureneut_results, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neut_resultsstructureneut_results, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'tbr_bk', self.tbr_bk)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'tbr_bk_inb', self.tbr_bk_inb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'tbr_bk_outb', self.tbr_bk_outb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'me_bk', self.me_bk)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'me_shield', self.me_shield)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_appm_res', self.he_appm_res)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'ins_dose_max', self.ins_dose_max)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'fn_flu_max', self.fn_flu_max)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dpa_cu_max', self.dpa_cu_max)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'fn_flux_bz', self.fn_flux_bz)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'fn_flux_bp', self.fn_flux_bp)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'fn_flux_man', self.fn_flux_man)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'fn_flux_sh', self.fn_flux_sh)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'p_nh_bk', self.p_nh_bk)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'p_nh_sh', self.p_nh_sh)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type neut_resultsstructureneut_results, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_tbr_bk = ull.getDouble(self.idx, path, cpopath + 'tbr_bk')
		check_status(status)
		if not status:
			self.tbr_bk = ret_tbr_bk
		status, ret_tbr_bk_inb = ull.getDouble(self.idx, path, cpopath + 'tbr_bk_inb')
		check_status(status)
		if not status:
			self.tbr_bk_inb = ret_tbr_bk_inb
		status, ret_tbr_bk_outb = ull.getDouble(self.idx, path, cpopath + 'tbr_bk_outb')
		check_status(status)
		if not status:
			self.tbr_bk_outb = ret_tbr_bk_outb
		status, ret_me_bk = ull.getDouble(self.idx, path, cpopath + 'me_bk')
		check_status(status)
		if not status:
			self.me_bk = ret_me_bk
		status, ret_me_shield = ull.getDouble(self.idx, path, cpopath + 'me_shield')
		check_status(status)
		if not status:
			self.me_shield = ret_me_shield
		status, ret_he_appm_res = ull.getDouble(self.idx, path, cpopath + 'he_appm_res')
		check_status(status)
		if not status:
			self.he_appm_res = ret_he_appm_res
		status, ret_ins_dose_max = ull.getDouble(self.idx, path, cpopath + 'ins_dose_max')
		check_status(status)
		if not status:
			self.ins_dose_max = ret_ins_dose_max
		status, ret_fn_flu_max = ull.getDouble(self.idx, path, cpopath + 'fn_flu_max')
		check_status(status)
		if not status:
			self.fn_flu_max = ret_fn_flu_max
		status, ret_dpa_cu_max = ull.getDouble(self.idx, path, cpopath + 'dpa_cu_max')
		check_status(status)
		if not status:
			self.dpa_cu_max = ret_dpa_cu_max
		status, ret_fn_flux_bz = ull.getDouble(self.idx, path, cpopath + 'fn_flux_bz')
		check_status(status)
		if not status:
			self.fn_flux_bz = ret_fn_flux_bz
		status, ret_fn_flux_bp = ull.getDouble(self.idx, path, cpopath + 'fn_flux_bp')
		check_status(status)
		if not status:
			self.fn_flux_bp = ret_fn_flux_bp
		status, ret_fn_flux_man = ull.getDouble(self.idx, path, cpopath + 'fn_flux_man')
		check_status(status)
		if not status:
			self.fn_flux_man = ret_fn_flux_man
		status, ret_fn_flux_sh = ull.getDouble(self.idx, path, cpopath + 'fn_flux_sh')
		check_status(status)
		if not status:
			self.fn_flux_sh = ret_fn_flux_sh
		status, ret_p_nh_bk = ull.getDouble(self.idx, path, cpopath + 'p_nh_bk')
		check_status(status)
		if not status:
			self.p_nh_bk = ret_p_nh_bk
		status, ret_p_nh_sh = ull.getDouble(self.idx, path, cpopath + 'p_nh_sh')
		check_status(status)
		if not status:
			self.p_nh_sh = ret_p_nh_sh

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type neut_resultsstructureneut_results, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, tbr_bkVal = ull.getDouble(self.idx, path, cpopath + 'tbr_bk')
			check_status(status)
			status, tbr_bk_inbVal = ull.getDouble(self.idx, path, cpopath + 'tbr_bk_inb')
			check_status(status)
			status, tbr_bk_outbVal = ull.getDouble(self.idx, path, cpopath + 'tbr_bk_outb')
			check_status(status)
			status, me_bkVal = ull.getDouble(self.idx, path, cpopath + 'me_bk')
			check_status(status)
			status, me_shieldVal = ull.getDouble(self.idx, path, cpopath + 'me_shield')
			check_status(status)
			status, he_appm_resVal = ull.getDouble(self.idx, path, cpopath + 'he_appm_res')
			check_status(status)
			status, ins_dose_maxVal = ull.getDouble(self.idx, path, cpopath + 'ins_dose_max')
			check_status(status)
			status, fn_flu_maxVal = ull.getDouble(self.idx, path, cpopath + 'fn_flu_max')
			check_status(status)
			status, dpa_cu_maxVal = ull.getDouble(self.idx, path, cpopath + 'dpa_cu_max')
			check_status(status)
			status, fn_flux_bzVal = ull.getDouble(self.idx, path, cpopath + 'fn_flux_bz')
			check_status(status)
			status, fn_flux_bpVal = ull.getDouble(self.idx, path, cpopath + 'fn_flux_bp')
			check_status(status)
			status, fn_flux_manVal = ull.getDouble(self.idx, path, cpopath + 'fn_flux_man')
			check_status(status)
			status, fn_flux_shVal = ull.getDouble(self.idx, path, cpopath + 'fn_flux_sh')
			check_status(status)
			status, p_nh_bkVal = ull.getDouble(self.idx, path, cpopath + 'p_nh_bk')
			check_status(status)
			status, p_nh_shVal = ull.getDouble(self.idx, path, cpopath + 'p_nh_sh')
			check_status(status)
			for i in range(nbslice):
				slice = neut_resultsstructureneut_results(self.base_path)
				slice.setExpIdx(self.idx)
				slice.tbr_bk = tbr_bkVal
				slice.tbr_bk_inb = tbr_bk_inbVal
				slice.tbr_bk_outb = tbr_bk_outbVal
				slice.me_bk = me_bkVal
				slice.me_shield = me_shieldVal
				slice.he_appm_res = he_appm_resVal
				slice.ins_dose_max = ins_dose_maxVal
				slice.fn_flu_max = fn_flu_maxVal
				slice.dpa_cu_max = dpa_cu_maxVal
				slice.fn_flux_bz = fn_flux_bzVal
				slice.fn_flux_bp = fn_flux_bpVal
				slice.fn_flux_man = fn_flux_manVal
				slice.fn_flux_sh = fn_flux_shVal
				slice.p_nh_bk = p_nh_bkVal
				slice.p_nh_sh = p_nh_shVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neut_resultsstructureneut_resultsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neut_resultsstructureneut_resultsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neut_resultsstructureneut_resultsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'tbr_bk') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'tbr_bk', i, self.tbr_bk)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'tbr_bk_inb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'tbr_bk_inb', i, self.tbr_bk_inb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'tbr_bk_outb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'tbr_bk_outb', i, self.tbr_bk_outb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'me_bk') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'me_bk', i, self.me_bk)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'me_shield') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'me_shield', i, self.me_shield)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_appm_res') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_appm_res', i, self.he_appm_res)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ins_dose_max') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ins_dose_max', i, self.ins_dose_max)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fn_flu_max') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fn_flu_max', i, self.fn_flu_max)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dpa_cu_max') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dpa_cu_max', i, self.dpa_cu_max)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fn_flux_bz') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fn_flux_bz', i, self.fn_flux_bz)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fn_flux_bp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fn_flux_bp', i, self.fn_flux_bp)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fn_flux_man') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fn_flux_man', i, self.fn_flux_man)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fn_flux_sh') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fn_flux_sh', i, self.fn_flux_sh)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'p_nh_bk') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'p_nh_bk', i, self.p_nh_bk)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'p_nh_sh') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'p_nh_sh', i, self.p_nh_sh)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neut_resultsstructureneut_resultsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'tbr_bk') 
			print ('obj = ' + str(obj))
		status, ret_tbr_bk = ull.getDoubleFromObject(self.idx, obj, cpopath + 'tbr_bk', i)
		check_status(status)
		if not status:
			self.tbr_bk = ret_tbr_bk
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'tbr_bk_inb') 
			print ('obj = ' + str(obj))
		status, ret_tbr_bk_inb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'tbr_bk_inb', i)
		check_status(status)
		if not status:
			self.tbr_bk_inb = ret_tbr_bk_inb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'tbr_bk_outb') 
			print ('obj = ' + str(obj))
		status, ret_tbr_bk_outb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'tbr_bk_outb', i)
		check_status(status)
		if not status:
			self.tbr_bk_outb = ret_tbr_bk_outb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'me_bk') 
			print ('obj = ' + str(obj))
		status, ret_me_bk = ull.getDoubleFromObject(self.idx, obj, cpopath + 'me_bk', i)
		check_status(status)
		if not status:
			self.me_bk = ret_me_bk
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'me_shield') 
			print ('obj = ' + str(obj))
		status, ret_me_shield = ull.getDoubleFromObject(self.idx, obj, cpopath + 'me_shield', i)
		check_status(status)
		if not status:
			self.me_shield = ret_me_shield
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_appm_res') 
			print ('obj = ' + str(obj))
		status, ret_he_appm_res = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_appm_res', i)
		check_status(status)
		if not status:
			self.he_appm_res = ret_he_appm_res
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ins_dose_max') 
			print ('obj = ' + str(obj))
		status, ret_ins_dose_max = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ins_dose_max', i)
		check_status(status)
		if not status:
			self.ins_dose_max = ret_ins_dose_max
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fn_flu_max') 
			print ('obj = ' + str(obj))
		status, ret_fn_flu_max = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fn_flu_max', i)
		check_status(status)
		if not status:
			self.fn_flu_max = ret_fn_flu_max
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dpa_cu_max') 
			print ('obj = ' + str(obj))
		status, ret_dpa_cu_max = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dpa_cu_max', i)
		check_status(status)
		if not status:
			self.dpa_cu_max = ret_dpa_cu_max
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fn_flux_bz') 
			print ('obj = ' + str(obj))
		status, ret_fn_flux_bz = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fn_flux_bz', i)
		check_status(status)
		if not status:
			self.fn_flux_bz = ret_fn_flux_bz
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fn_flux_bp') 
			print ('obj = ' + str(obj))
		status, ret_fn_flux_bp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fn_flux_bp', i)
		check_status(status)
		if not status:
			self.fn_flux_bp = ret_fn_flux_bp
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fn_flux_man') 
			print ('obj = ' + str(obj))
		status, ret_fn_flux_man = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fn_flux_man', i)
		check_status(status)
		if not status:
			self.fn_flux_man = ret_fn_flux_man
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fn_flux_sh') 
			print ('obj = ' + str(obj))
		status, ret_fn_flux_sh = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fn_flux_sh', i)
		check_status(status)
		if not status:
			self.fn_flux_sh = ret_fn_flux_sh
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'p_nh_bk') 
			print ('obj = ' + str(obj))
		status, ret_p_nh_bk = ull.getDoubleFromObject(self.idx, obj, cpopath + 'p_nh_bk', i)
		check_status(status)
		if not status:
			self.p_nh_bk = ret_p_nh_bk
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'p_nh_sh') 
			print ('obj = ' + str(obj))
		status, ret_p_nh_sh = ull.getDoubleFromObject(self.idx, obj, cpopath + 'p_nh_sh', i)
		check_status(status)
		if not status:
			self.p_nh_sh = ret_p_nh_sh

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'tbr_bk')
		ull.deleteData(self.idx, path, cpopath + 'tbr_bk_inb')
		ull.deleteData(self.idx, path, cpopath + 'tbr_bk_outb')
		ull.deleteData(self.idx, path, cpopath + 'me_bk')
		ull.deleteData(self.idx, path, cpopath + 'me_shield')
		ull.deleteData(self.idx, path, cpopath + 'he_appm_res')
		ull.deleteData(self.idx, path, cpopath + 'ins_dose_max')
		ull.deleteData(self.idx, path, cpopath + 'fn_flu_max')
		ull.deleteData(self.idx, path, cpopath + 'dpa_cu_max')
		ull.deleteData(self.idx, path, cpopath + 'fn_flux_bz')
		ull.deleteData(self.idx, path, cpopath + 'fn_flux_bp')
		ull.deleteData(self.idx, path, cpopath + 'fn_flux_man')
		ull.deleteData(self.idx, path, cpopath + 'fn_flux_sh')
		ull.deleteData(self.idx, path, cpopath + 'p_nh_bk')
		ull.deleteData(self.idx, path, cpopath + 'p_nh_sh')


class shieldstructureshield:
	'''
	class shieldstructureshield
	Shield

	Attributes:
	- inboard : class inboardstructureshield_specs
	   Inboard
	- outboard : class outboardstructureshield_specs
	   Outboard
	'''

	def __init__(self, base_path_in='shield'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.inboard = inboardstructureshield_specs('inboard')
		self.outboard = outboardstructureshield_specs('outboard')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class shieldstructureshield\n'
		ret = ret + space + 'Attribute inboard\n ' + self.inboard.__str__(depth+1)
		ret = ret + space + 'Attribute outboard\n ' + self.outboard.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.inboard.setExpIdx(idx)
		self.outboard.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type shieldstructureshield, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.inboard.cpoTime = self.cpoTime
		self.inboard.putSlice(path, cpopath)
		self.outboard.cpoTime = self.cpoTime
		self.outboard.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type shieldstructureshield, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.inboard.replaceLastSlice(path, cpopath)
		self.outboard.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type shieldstructureshield, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.inboard.putNonTimed(path, cpopath)
		self.outboard.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type shieldstructureshield, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.inboard.getSlice(path, cpopath, inTime, interpolMode)
		self.outboard.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type shieldstructureshield, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			inboardList = self.inboard.build_non_resampled_data(path, cpopath, nbslice)
			outboardList = self.outboard.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = shieldstructureshield(self.base_path)
				slice.setExpIdx(self.idx)
				slice.inboard = inboardList[i]
				slice.outboard = outboardList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type shieldstructureshieldObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type shieldstructureshieldObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type shieldstructureshieldObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.inboard.putNonTimedElt(path, cpopath + 'inboard', i, obj)
		obj = self.outboard.putNonTimedElt(path, cpopath + 'outboard', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type shieldstructureshieldObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.inboard.getNonTimedElt(path, cpopath + 'inboard', i, obj)
		self.outboard.getNonTimedElt(path, cpopath + 'outboard', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.inboard.deleteData(path, cpopath)
		self.outboard.deleteData(path, cpopath)


class inboardstructureshield_specs:
	'''
	class inboardstructureshield_specs
	Inboard

	Attributes:
	- nmat : int
	   Number of materials; Scalar
	- composition : numpy.ndarray 1D with float
	   Inboard or outboard shield  radial build the percentage of each material respectively (Meaning of the material index 1: Eurofer, 2: Pb-15.7Li, 3: He, 4: Water, 5: Tungsten Carbide, 6: Boron, 7: Tungsten, 8: Stainless Steel 316) in %vol; Vector(nmat).
	- r1 : float
	   Inner radius (nearest to the plasma), in the global tokamak coordinate system of the inboard or outboard shield located at the equatorial plane [m]; Scalar
	- r2 : float
	   Outer radius (farest to the plasma), in the global tokamak coordinate system of the inboard or outboard shield located at the equatorial plane [m]; Scalar
	- mass : float
	   Mass of inboard or outboard shield [Kg]; Scalar
	'''

	def __init__(self, base_path_in='inboard'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nmat = EMPTY_INT
		self.composition = numpy.zeros(0, numpy.float64, order='C')
		self.r1 = EMPTY_DOUBLE
		self.r2 = EMPTY_DOUBLE
		self.mass = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class inboardstructureshield_specs\n'
		ret = ret + space + 'Attribute nmat: ' + str(self.nmat) + '\n'
		s = self.composition.__str__()
		ret = ret + space + 'Attribute composition\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute r1: ' + str(self.r1) + '\n'
		ret = ret + space + 'Attribute r2: ' + str(self.r2) + '\n'
		ret = ret + space + 'Attribute mass: ' + str(self.mass) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructureshield_specs, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructureshield_specs, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructureshield_specs, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'nmat', self.nmat)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'composition', numpy.array(self.composition).astype(numpy.float64), False)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'r1', self.r1)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'r2', self.r2)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'mass', self.mass)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructureshield_specs, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_nmat = ull.getInt(self.idx, path, cpopath + 'nmat')
		check_status(status)
		if not status:
			self.nmat = ret_nmat
		status, ret_composition = ull.getVect1DDouble(self.idx, path, cpopath + 'composition')
		check_status(status)
		if not status:
			self.composition = ret_composition
		status, ret_r1 = ull.getDouble(self.idx, path, cpopath + 'r1')
		check_status(status)
		if not status:
			self.r1 = ret_r1
		status, ret_r2 = ull.getDouble(self.idx, path, cpopath + 'r2')
		check_status(status)
		if not status:
			self.r2 = ret_r2
		status, ret_mass = ull.getDouble(self.idx, path, cpopath + 'mass')
		check_status(status)
		if not status:
			self.mass = ret_mass

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructureshield_specs, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nmatVal = ull.getInt(self.idx, path, cpopath + 'nmat')
			check_status(status)
			status, compositionVal = ull.getVect1DDouble(self.idx, path, cpopath + 'composition')
			check_status(status)
			status, r1Val = ull.getDouble(self.idx, path, cpopath + 'r1')
			check_status(status)
			status, r2Val = ull.getDouble(self.idx, path, cpopath + 'r2')
			check_status(status)
			status, massVal = ull.getDouble(self.idx, path, cpopath + 'mass')
			check_status(status)
			for i in range(nbslice):
				slice = inboardstructureshield_specs(self.base_path)
				slice.setExpIdx(self.idx)
				slice.nmat = nmatVal
				slice.composition = compositionVal
				slice.r1 = r1Val
				slice.r2 = r2Val
				slice.mass = massVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructureshield_specsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructureshield_specsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructureshield_specsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nmat') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nmat', i, self.nmat)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'composition') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'composition', i, numpy.array(self.composition).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r1') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r1', i, self.r1)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r2') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r2', i, self.r2)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'mass') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'mass', i, self.mass)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructureshield_specsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nmat') 
			print ('obj = ' + str(obj))
		status, ret_nmat = ull.getIntFromObject(self.idx, obj, cpopath + 'nmat', i)
		check_status(status)
		if not status:
			self.nmat = ret_nmat
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'composition') 
			print ('obj = ' + str(obj))
		status, ret_composition = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'composition', i)
		check_status(status)
		if not status:
			self.composition = ret_composition
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r1') 
			print ('obj = ' + str(obj))
		status, ret_r1 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r1', i)
		check_status(status)
		if not status:
			self.r1 = ret_r1
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r2') 
			print ('obj = ' + str(obj))
		status, ret_r2 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r2', i)
		check_status(status)
		if not status:
			self.r2 = ret_r2
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'mass') 
			print ('obj = ' + str(obj))
		status, ret_mass = ull.getDoubleFromObject(self.idx, obj, cpopath + 'mass', i)
		check_status(status)
		if not status:
			self.mass = ret_mass

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'nmat')
		ull.deleteData(self.idx, path, cpopath + 'composition')
		ull.deleteData(self.idx, path, cpopath + 'r1')
		ull.deleteData(self.idx, path, cpopath + 'r2')
		ull.deleteData(self.idx, path, cpopath + 'mass')


class outboardstructureshield_specs:
	'''
	class outboardstructureshield_specs
	Outboard

	Attributes:
	- nmat : int
	   Number of materials; Scalar
	- composition : numpy.ndarray 1D with float
	   Inboard or outboard shield  radial build the percentage of each material respectively (Meaning of the material index 1: Eurofer, 2: Pb-15.7Li, 3: He, 4: Water, 5: Tungsten Carbide, 6: Boron, 7: Tungsten, 8: Stainless Steel 316) in %vol; Vector(nmat).
	- r1 : float
	   Inner radius (nearest to the plasma), in the global tokamak coordinate system of the inboard or outboard shield located at the equatorial plane [m]; Scalar
	- r2 : float
	   Outer radius (farest to the plasma), in the global tokamak coordinate system of the inboard or outboard shield located at the equatorial plane [m]; Scalar
	- mass : float
	   Mass of inboard or outboard shield [Kg]; Scalar
	'''

	def __init__(self, base_path_in='outboard'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nmat = EMPTY_INT
		self.composition = numpy.zeros(0, numpy.float64, order='C')
		self.r1 = EMPTY_DOUBLE
		self.r2 = EMPTY_DOUBLE
		self.mass = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class outboardstructureshield_specs\n'
		ret = ret + space + 'Attribute nmat: ' + str(self.nmat) + '\n'
		s = self.composition.__str__()
		ret = ret + space + 'Attribute composition\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute r1: ' + str(self.r1) + '\n'
		ret = ret + space + 'Attribute r2: ' + str(self.r2) + '\n'
		ret = ret + space + 'Attribute mass: ' + str(self.mass) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructureshield_specs, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructureshield_specs, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructureshield_specs, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'nmat', self.nmat)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'composition', numpy.array(self.composition).astype(numpy.float64), False)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'r1', self.r1)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'r2', self.r2)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'mass', self.mass)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructureshield_specs, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_nmat = ull.getInt(self.idx, path, cpopath + 'nmat')
		check_status(status)
		if not status:
			self.nmat = ret_nmat
		status, ret_composition = ull.getVect1DDouble(self.idx, path, cpopath + 'composition')
		check_status(status)
		if not status:
			self.composition = ret_composition
		status, ret_r1 = ull.getDouble(self.idx, path, cpopath + 'r1')
		check_status(status)
		if not status:
			self.r1 = ret_r1
		status, ret_r2 = ull.getDouble(self.idx, path, cpopath + 'r2')
		check_status(status)
		if not status:
			self.r2 = ret_r2
		status, ret_mass = ull.getDouble(self.idx, path, cpopath + 'mass')
		check_status(status)
		if not status:
			self.mass = ret_mass

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructureshield_specs, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nmatVal = ull.getInt(self.idx, path, cpopath + 'nmat')
			check_status(status)
			status, compositionVal = ull.getVect1DDouble(self.idx, path, cpopath + 'composition')
			check_status(status)
			status, r1Val = ull.getDouble(self.idx, path, cpopath + 'r1')
			check_status(status)
			status, r2Val = ull.getDouble(self.idx, path, cpopath + 'r2')
			check_status(status)
			status, massVal = ull.getDouble(self.idx, path, cpopath + 'mass')
			check_status(status)
			for i in range(nbslice):
				slice = outboardstructureshield_specs(self.base_path)
				slice.setExpIdx(self.idx)
				slice.nmat = nmatVal
				slice.composition = compositionVal
				slice.r1 = r1Val
				slice.r2 = r2Val
				slice.mass = massVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructureshield_specsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructureshield_specsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructureshield_specsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nmat') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nmat', i, self.nmat)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'composition') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'composition', i, numpy.array(self.composition).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r1') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r1', i, self.r1)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r2') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r2', i, self.r2)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'mass') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'mass', i, self.mass)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructureshield_specsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nmat') 
			print ('obj = ' + str(obj))
		status, ret_nmat = ull.getIntFromObject(self.idx, obj, cpopath + 'nmat', i)
		check_status(status)
		if not status:
			self.nmat = ret_nmat
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'composition') 
			print ('obj = ' + str(obj))
		status, ret_composition = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'composition', i)
		check_status(status)
		if not status:
			self.composition = ret_composition
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r1') 
			print ('obj = ' + str(obj))
		status, ret_r1 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r1', i)
		check_status(status)
		if not status:
			self.r1 = ret_r1
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r2') 
			print ('obj = ' + str(obj))
		status, ret_r2 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r2', i)
		check_status(status)
		if not status:
			self.r2 = ret_r2
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'mass') 
			print ('obj = ' + str(obj))
		status, ret_mass = ull.getDoubleFromObject(self.idx, obj, cpopath + 'mass', i)
		check_status(status)
		if not status:
			self.mass = ret_mass

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'nmat')
		ull.deleteData(self.idx, path, cpopath + 'composition')
		ull.deleteData(self.idx, path, cpopath + 'r1')
		ull.deleteData(self.idx, path, cpopath + 'r2')
		ull.deleteData(self.idx, path, cpopath + 'mass')


class bbstructurebb:
	'''
	class bbstructurebb
	Breeding blanket

	Attributes:
	- nb_bb : float
	   Total (in the reactor) number of breeding blanket module; Scalar
	- nb_bb_polcut : float
	   Number of bb modules on a poloidal cut; Scalar
	- teta_bb : float
	   Angle (0 for equatorial outboard, then in anti-clokwise direction) of bb module; [deg]
	- tbr : float
	   Tritium breeding ratio of the blanket [-]; Scalar
	- neutro_resul : class neutro_resulstructureneutro_resul
	   Neutronic results
	- inboard : class inboardstructurebb_specs
	   Inboard
	- outboard : class outboardstructurebb_specs
	   Outboard
	'''

	def __init__(self, base_path_in='bb'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nb_bb = EMPTY_DOUBLE
		self.nb_bb_polcut = EMPTY_DOUBLE
		self.teta_bb = EMPTY_DOUBLE
		self.tbr = EMPTY_DOUBLE
		self.neutro_resul = neutro_resulstructureneutro_resul('neutro_resul')
		self.inboard = inboardstructurebb_specs('inboard')
		self.outboard = outboardstructurebb_specs('outboard')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class bbstructurebb\n'
		ret = ret + space + 'Attribute nb_bb: ' + str(self.nb_bb) + '\n'
		ret = ret + space + 'Attribute nb_bb_polcut: ' + str(self.nb_bb_polcut) + '\n'
		ret = ret + space + 'Attribute teta_bb: ' + str(self.teta_bb) + '\n'
		ret = ret + space + 'Attribute tbr: ' + str(self.tbr) + '\n'
		ret = ret + space + 'Attribute neutro_resul\n ' + self.neutro_resul.__str__(depth+1)
		ret = ret + space + 'Attribute inboard\n ' + self.inboard.__str__(depth+1)
		ret = ret + space + 'Attribute outboard\n ' + self.outboard.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.neutro_resul.setExpIdx(idx)
		self.inboard.setExpIdx(idx)
		self.outboard.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bbstructurebb, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.neutro_resul.cpoTime = self.cpoTime
		self.neutro_resul.putSlice(path, cpopath)
		self.inboard.cpoTime = self.cpoTime
		self.inboard.putSlice(path, cpopath)
		self.outboard.cpoTime = self.cpoTime
		self.outboard.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bbstructurebb, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.neutro_resul.replaceLastSlice(path, cpopath)
		self.inboard.replaceLastSlice(path, cpopath)
		self.outboard.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bbstructurebb, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'nb_bb', self.nb_bb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'nb_bb_polcut', self.nb_bb_polcut)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'teta_bb', self.teta_bb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'tbr', self.tbr)
		check_status(status)
		self.neutro_resul.putNonTimed(path, cpopath)
		self.inboard.putNonTimed(path, cpopath)
		self.outboard.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type bbstructurebb, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_nb_bb = ull.getDouble(self.idx, path, cpopath + 'nb_bb')
		check_status(status)
		if not status:
			self.nb_bb = ret_nb_bb
		status, ret_nb_bb_polcut = ull.getDouble(self.idx, path, cpopath + 'nb_bb_polcut')
		check_status(status)
		if not status:
			self.nb_bb_polcut = ret_nb_bb_polcut
		status, ret_teta_bb = ull.getDouble(self.idx, path, cpopath + 'teta_bb')
		check_status(status)
		if not status:
			self.teta_bb = ret_teta_bb
		status, ret_tbr = ull.getDouble(self.idx, path, cpopath + 'tbr')
		check_status(status)
		if not status:
			self.tbr = ret_tbr
		self.neutro_resul.getSlice(path, cpopath, inTime, interpolMode)
		self.inboard.getSlice(path, cpopath, inTime, interpolMode)
		self.outboard.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type bbstructurebb, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nb_bbVal = ull.getDouble(self.idx, path, cpopath + 'nb_bb')
			check_status(status)
			status, nb_bb_polcutVal = ull.getDouble(self.idx, path, cpopath + 'nb_bb_polcut')
			check_status(status)
			status, teta_bbVal = ull.getDouble(self.idx, path, cpopath + 'teta_bb')
			check_status(status)
			status, tbrVal = ull.getDouble(self.idx, path, cpopath + 'tbr')
			check_status(status)
			neutro_resulList = self.neutro_resul.build_non_resampled_data(path, cpopath, nbslice)
			inboardList = self.inboard.build_non_resampled_data(path, cpopath, nbslice)
			outboardList = self.outboard.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = bbstructurebb(self.base_path)
				slice.setExpIdx(self.idx)
				slice.nb_bb = nb_bbVal
				slice.nb_bb_polcut = nb_bb_polcutVal
				slice.teta_bb = teta_bbVal
				slice.tbr = tbrVal
				slice.neutro_resul = neutro_resulList[i]
				slice.inboard = inboardList[i]
				slice.outboard = outboardList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bbstructurebbObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bbstructurebbObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bbstructurebbObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'nb_bb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'nb_bb', i, self.nb_bb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'nb_bb_polcut') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'nb_bb_polcut', i, self.nb_bb_polcut)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'teta_bb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'teta_bb', i, self.teta_bb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'tbr') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'tbr', i, self.tbr)
		obj = self.neutro_resul.putNonTimedElt(path, cpopath + 'neutro_resul', i, obj)
		obj = self.inboard.putNonTimedElt(path, cpopath + 'inboard', i, obj)
		obj = self.outboard.putNonTimedElt(path, cpopath + 'outboard', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bbstructurebbObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'nb_bb') 
			print ('obj = ' + str(obj))
		status, ret_nb_bb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'nb_bb', i)
		check_status(status)
		if not status:
			self.nb_bb = ret_nb_bb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'nb_bb_polcut') 
			print ('obj = ' + str(obj))
		status, ret_nb_bb_polcut = ull.getDoubleFromObject(self.idx, obj, cpopath + 'nb_bb_polcut', i)
		check_status(status)
		if not status:
			self.nb_bb_polcut = ret_nb_bb_polcut
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'teta_bb') 
			print ('obj = ' + str(obj))
		status, ret_teta_bb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'teta_bb', i)
		check_status(status)
		if not status:
			self.teta_bb = ret_teta_bb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'tbr') 
			print ('obj = ' + str(obj))
		status, ret_tbr = ull.getDoubleFromObject(self.idx, obj, cpopath + 'tbr', i)
		check_status(status)
		if not status:
			self.tbr = ret_tbr
		self.neutro_resul.getNonTimedElt(path, cpopath + 'neutro_resul', i, obj)
		self.inboard.getNonTimedElt(path, cpopath + 'inboard', i, obj)
		self.outboard.getNonTimedElt(path, cpopath + 'outboard', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'nb_bb')
		ull.deleteData(self.idx, path, cpopath + 'nb_bb_polcut')
		ull.deleteData(self.idx, path, cpopath + 'teta_bb')
		ull.deleteData(self.idx, path, cpopath + 'tbr')
		self.neutro_resul.deleteData(path, cpopath)
		self.inboard.deleteData(path, cpopath)
		self.outboard.deleteData(path, cpopath)


class neutro_resulstructureneutro_resul:
	'''
	class neutro_resulstructureneutro_resul
	Neutronic results

	Attributes:
	- nwl_max : float
	   Maximum neutron wall load (on equatorial outboard module) [W*m^-2]; Scalar
	- nwl_pol_prof : numpy.ndarray 1D with float
	   NWL scaling factor coefficient for each bb module; Vector(nmodules)
	'''

	def __init__(self, base_path_in='neutro_resul'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nwl_max = EMPTY_DOUBLE
		self.nwl_pol_prof = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class neutro_resulstructureneutro_resul\n'
		ret = ret + space + 'Attribute nwl_max: ' + str(self.nwl_max) + '\n'
		s = self.nwl_pol_prof.__str__()
		ret = ret + space + 'Attribute nwl_pol_prof\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutro_resulstructureneutro_resul, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutro_resulstructureneutro_resul, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutro_resulstructureneutro_resul, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'nwl_max', self.nwl_max)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'nwl_pol_prof', numpy.array(self.nwl_pol_prof).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type neutro_resulstructureneutro_resul, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_nwl_max = ull.getDouble(self.idx, path, cpopath + 'nwl_max')
		check_status(status)
		if not status:
			self.nwl_max = ret_nwl_max
		status, ret_nwl_pol_prof = ull.getVect1DDouble(self.idx, path, cpopath + 'nwl_pol_prof')
		check_status(status)
		if not status:
			self.nwl_pol_prof = ret_nwl_pol_prof

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type neutro_resulstructureneutro_resul, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nwl_maxVal = ull.getDouble(self.idx, path, cpopath + 'nwl_max')
			check_status(status)
			status, nwl_pol_profVal = ull.getVect1DDouble(self.idx, path, cpopath + 'nwl_pol_prof')
			check_status(status)
			for i in range(nbslice):
				slice = neutro_resulstructureneutro_resul(self.base_path)
				slice.setExpIdx(self.idx)
				slice.nwl_max = nwl_maxVal
				slice.nwl_pol_prof = nwl_pol_profVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neutro_resulstructureneutro_resulObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neutro_resulstructureneutro_resulObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neutro_resulstructureneutro_resulObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'nwl_max') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'nwl_max', i, self.nwl_max)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'nwl_pol_prof') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'nwl_pol_prof', i, numpy.array(self.nwl_pol_prof).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neutro_resulstructureneutro_resulObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'nwl_max') 
			print ('obj = ' + str(obj))
		status, ret_nwl_max = ull.getDoubleFromObject(self.idx, obj, cpopath + 'nwl_max', i)
		check_status(status)
		if not status:
			self.nwl_max = ret_nwl_max
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'nwl_pol_prof') 
			print ('obj = ' + str(obj))
		status, ret_nwl_pol_prof = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'nwl_pol_prof', i)
		check_status(status)
		if not status:
			self.nwl_pol_prof = ret_nwl_pol_prof

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'nwl_max')
		ull.deleteData(self.idx, path, cpopath + 'nwl_pol_prof')


class inboardstructurebb_specs:
	'''
	class inboardstructurebb_specs
	Inboard

	Attributes:
	- nbb : float
	   Number of inboard or outboard bb modules (in a poloidal cut), Scalar
	- r1 : float
	   Inner radius (nearest to the plasma), in the global tokamak coordinate system of the inboard or outboard bb located at the equatorial plane [m]; Scalar
	- r2 : float
	   Outer radius (farest to the plasma), in the global tokamak coordinate system of the inboard or outboard bb located at the equatorial plane [m]; Scalar
	- dimension : class dimensionstructurebb_dimension
	   dimension of the various modules
	'''

	def __init__(self, base_path_in='inboard'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nbb = EMPTY_DOUBLE
		self.r1 = EMPTY_DOUBLE
		self.r2 = EMPTY_DOUBLE
		self.dimension = dimensionstructurebb_dimension('dimension')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class inboardstructurebb_specs\n'
		ret = ret + space + 'Attribute nbb: ' + str(self.nbb) + '\n'
		ret = ret + space + 'Attribute r1: ' + str(self.r1) + '\n'
		ret = ret + space + 'Attribute r2: ' + str(self.r2) + '\n'
		ret = ret + space + 'Attribute dimension\n ' + self.dimension.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.dimension.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructurebb_specs, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.dimension.cpoTime = self.cpoTime
		self.dimension.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructurebb_specs, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.dimension.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructurebb_specs, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'nbb', self.nbb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'r1', self.r1)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'r2', self.r2)
		check_status(status)
		self.dimension.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructurebb_specs, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_nbb = ull.getDouble(self.idx, path, cpopath + 'nbb')
		check_status(status)
		if not status:
			self.nbb = ret_nbb
		status, ret_r1 = ull.getDouble(self.idx, path, cpopath + 'r1')
		check_status(status)
		if not status:
			self.r1 = ret_r1
		status, ret_r2 = ull.getDouble(self.idx, path, cpopath + 'r2')
		check_status(status)
		if not status:
			self.r2 = ret_r2
		self.dimension.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructurebb_specs, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nbbVal = ull.getDouble(self.idx, path, cpopath + 'nbb')
			check_status(status)
			status, r1Val = ull.getDouble(self.idx, path, cpopath + 'r1')
			check_status(status)
			status, r2Val = ull.getDouble(self.idx, path, cpopath + 'r2')
			check_status(status)
			dimensionList = self.dimension.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = inboardstructurebb_specs(self.base_path)
				slice.setExpIdx(self.idx)
				slice.nbb = nbbVal
				slice.r1 = r1Val
				slice.r2 = r2Val
				slice.dimension = dimensionList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructurebb_specsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructurebb_specsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructurebb_specsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'nbb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'nbb', i, self.nbb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r1') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r1', i, self.r1)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r2') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r2', i, self.r2)
		obj = self.dimension.putNonTimedElt(path, cpopath + 'dimension', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructurebb_specsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'nbb') 
			print ('obj = ' + str(obj))
		status, ret_nbb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'nbb', i)
		check_status(status)
		if not status:
			self.nbb = ret_nbb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r1') 
			print ('obj = ' + str(obj))
		status, ret_r1 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r1', i)
		check_status(status)
		if not status:
			self.r1 = ret_r1
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r2') 
			print ('obj = ' + str(obj))
		status, ret_r2 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r2', i)
		check_status(status)
		if not status:
			self.r2 = ret_r2
		self.dimension.getNonTimedElt(path, cpopath + 'dimension', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'nbb')
		ull.deleteData(self.idx, path, cpopath + 'r1')
		ull.deleteData(self.idx, path, cpopath + 'r2')
		self.dimension.deleteData(path, cpopath)


class dimensionstructurebb_dimension:
	'''
	class dimensionstructurebb_dimension
	dimension of the various modules

	Attributes:
	- radial : numpy.ndarray 1D with float
	   Radial dimension [m]. Vector(nmodules) 
	- toroidal : numpy.ndarray 1D with float
	   Toroidal dimension [m]. Vector(nmodules) 
	- poloidal : numpy.ndarray 1D with float
	   Poloidal dimension [m]. Vector(nmodules) 
	'''

	def __init__(self, base_path_in='dimension'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.radial = numpy.zeros(0, numpy.float64, order='C')
		self.toroidal = numpy.zeros(0, numpy.float64, order='C')
		self.poloidal = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class dimensionstructurebb_dimension\n'
		s = self.radial.__str__()
		ret = ret + space + 'Attribute radial\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.toroidal.__str__()
		ret = ret + space + 'Attribute toroidal\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.poloidal.__str__()
		ret = ret + space + 'Attribute poloidal\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dimensionstructurebb_dimension, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dimensionstructurebb_dimension, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dimensionstructurebb_dimension, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'radial', numpy.array(self.radial).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'toroidal', numpy.array(self.toroidal).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'poloidal', numpy.array(self.poloidal).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type dimensionstructurebb_dimension, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_radial = ull.getVect1DDouble(self.idx, path, cpopath + 'radial')
		check_status(status)
		if not status:
			self.radial = ret_radial
		status, ret_toroidal = ull.getVect1DDouble(self.idx, path, cpopath + 'toroidal')
		check_status(status)
		if not status:
			self.toroidal = ret_toroidal
		status, ret_poloidal = ull.getVect1DDouble(self.idx, path, cpopath + 'poloidal')
		check_status(status)
		if not status:
			self.poloidal = ret_poloidal

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type dimensionstructurebb_dimension, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, radialVal = ull.getVect1DDouble(self.idx, path, cpopath + 'radial')
			check_status(status)
			status, toroidalVal = ull.getVect1DDouble(self.idx, path, cpopath + 'toroidal')
			check_status(status)
			status, poloidalVal = ull.getVect1DDouble(self.idx, path, cpopath + 'poloidal')
			check_status(status)
			for i in range(nbslice):
				slice = dimensionstructurebb_dimension(self.base_path)
				slice.setExpIdx(self.idx)
				slice.radial = radialVal
				slice.toroidal = toroidalVal
				slice.poloidal = poloidalVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dimensionstructurebb_dimensionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dimensionstructurebb_dimensionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dimensionstructurebb_dimensionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'radial') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'radial', i, numpy.array(self.radial).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'toroidal') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'toroidal', i, numpy.array(self.toroidal).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'poloidal') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'poloidal', i, numpy.array(self.poloidal).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dimensionstructurebb_dimensionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'radial') 
			print ('obj = ' + str(obj))
		status, ret_radial = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'radial', i)
		check_status(status)
		if not status:
			self.radial = ret_radial
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'toroidal') 
			print ('obj = ' + str(obj))
		status, ret_toroidal = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'toroidal', i)
		check_status(status)
		if not status:
			self.toroidal = ret_toroidal
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'poloidal') 
			print ('obj = ' + str(obj))
		status, ret_poloidal = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'poloidal', i)
		check_status(status)
		if not status:
			self.poloidal = ret_poloidal

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'radial')
		ull.deleteData(self.idx, path, cpopath + 'toroidal')
		ull.deleteData(self.idx, path, cpopath + 'poloidal')


class outboardstructurebb_specs:
	'''
	class outboardstructurebb_specs
	Outboard

	Attributes:
	- nbb : float
	   Number of inboard or outboard bb modules (in a poloidal cut), Scalar
	- r1 : float
	   Inner radius (nearest to the plasma), in the global tokamak coordinate system of the inboard or outboard bb located at the equatorial plane [m]; Scalar
	- r2 : float
	   Outer radius (farest to the plasma), in the global tokamak coordinate system of the inboard or outboard bb located at the equatorial plane [m]; Scalar
	- dimension : class dimensionstructurebb_dimension
	   dimension of the various modules
	'''

	def __init__(self, base_path_in='outboard'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nbb = EMPTY_DOUBLE
		self.r1 = EMPTY_DOUBLE
		self.r2 = EMPTY_DOUBLE
		self.dimension = dimensionstructurebb_dimension('dimension')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class outboardstructurebb_specs\n'
		ret = ret + space + 'Attribute nbb: ' + str(self.nbb) + '\n'
		ret = ret + space + 'Attribute r1: ' + str(self.r1) + '\n'
		ret = ret + space + 'Attribute r2: ' + str(self.r2) + '\n'
		ret = ret + space + 'Attribute dimension\n ' + self.dimension.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.dimension.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructurebb_specs, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.dimension.cpoTime = self.cpoTime
		self.dimension.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructurebb_specs, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.dimension.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructurebb_specs, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'nbb', self.nbb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'r1', self.r1)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'r2', self.r2)
		check_status(status)
		self.dimension.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructurebb_specs, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_nbb = ull.getDouble(self.idx, path, cpopath + 'nbb')
		check_status(status)
		if not status:
			self.nbb = ret_nbb
		status, ret_r1 = ull.getDouble(self.idx, path, cpopath + 'r1')
		check_status(status)
		if not status:
			self.r1 = ret_r1
		status, ret_r2 = ull.getDouble(self.idx, path, cpopath + 'r2')
		check_status(status)
		if not status:
			self.r2 = ret_r2
		self.dimension.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructurebb_specs, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nbbVal = ull.getDouble(self.idx, path, cpopath + 'nbb')
			check_status(status)
			status, r1Val = ull.getDouble(self.idx, path, cpopath + 'r1')
			check_status(status)
			status, r2Val = ull.getDouble(self.idx, path, cpopath + 'r2')
			check_status(status)
			dimensionList = self.dimension.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = outboardstructurebb_specs(self.base_path)
				slice.setExpIdx(self.idx)
				slice.nbb = nbbVal
				slice.r1 = r1Val
				slice.r2 = r2Val
				slice.dimension = dimensionList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructurebb_specsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructurebb_specsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructurebb_specsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'nbb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'nbb', i, self.nbb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r1') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r1', i, self.r1)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r2') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r2', i, self.r2)
		obj = self.dimension.putNonTimedElt(path, cpopath + 'dimension', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructurebb_specsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'nbb') 
			print ('obj = ' + str(obj))
		status, ret_nbb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'nbb', i)
		check_status(status)
		if not status:
			self.nbb = ret_nbb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r1') 
			print ('obj = ' + str(obj))
		status, ret_r1 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r1', i)
		check_status(status)
		if not status:
			self.r1 = ret_r1
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r2') 
			print ('obj = ' + str(obj))
		status, ret_r2 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r2', i)
		check_status(status)
		if not status:
			self.r2 = ret_r2
		self.dimension.getNonTimedElt(path, cpopath + 'dimension', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'nbb')
		ull.deleteData(self.idx, path, cpopath + 'r1')
		ull.deleteData(self.idx, path, cpopath + 'r2')
		self.dimension.deleteData(path, cpopath)


class hcllstructurehcll:
	'''
	class hcllstructurehcll
	Data specific to HCLL blanket concept

	Attributes:
	- mat_lim : class mat_limstructuremat_lim
	   Material limits specific to HCLL breeding blanket
	- hcll_bb : class hcll_bbstructurehcll_bb
	   HCLL breeding blanket. Radially, the blanket is divided in 4 layers: 1: First Wall, 2 : breeder zone, 3 : back plates, 4 : manifolds 
	'''

	def __init__(self, base_path_in='hcll'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.mat_lim = mat_limstructuremat_lim('mat_lim')
		self.hcll_bb = hcll_bbstructurehcll_bb('hcll_bb')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class hcllstructurehcll\n'
		ret = ret + space + 'Attribute mat_lim\n ' + self.mat_lim.__str__(depth+1)
		ret = ret + space + 'Attribute hcll_bb\n ' + self.hcll_bb.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.mat_lim.setExpIdx(idx)
		self.hcll_bb.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type hcllstructurehcll, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mat_lim.cpoTime = self.cpoTime
		self.mat_lim.putSlice(path, cpopath)
		self.hcll_bb.cpoTime = self.cpoTime
		self.hcll_bb.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type hcllstructurehcll, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mat_lim.replaceLastSlice(path, cpopath)
		self.hcll_bb.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type hcllstructurehcll, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mat_lim.putNonTimed(path, cpopath)
		self.hcll_bb.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type hcllstructurehcll, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mat_lim.getSlice(path, cpopath, inTime, interpolMode)
		self.hcll_bb.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type hcllstructurehcll, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			mat_limList = self.mat_lim.build_non_resampled_data(path, cpopath, nbslice)
			hcll_bbList = self.hcll_bb.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = hcllstructurehcll(self.base_path)
				slice.setExpIdx(self.idx)
				slice.mat_lim = mat_limList[i]
				slice.hcll_bb = hcll_bbList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type hcllstructurehcllObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type hcllstructurehcllObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type hcllstructurehcllObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.mat_lim.putNonTimedElt(path, cpopath + 'mat_lim', i, obj)
		obj = self.hcll_bb.putNonTimedElt(path, cpopath + 'hcll_bb', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type hcllstructurehcllObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.mat_lim.getNonTimedElt(path, cpopath + 'mat_lim', i, obj)
		self.hcll_bb.getNonTimedElt(path, cpopath + 'hcll_bb', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mat_lim.deleteData(path, cpopath)
		self.hcll_bb.deleteData(path, cpopath)


class mat_limstructuremat_lim:
	'''
	class mat_limstructuremat_lim
	Material limits specific to HCLL breeding blanket

	Attributes:
	- cool_t_lim : float
	   Min, max allowable He temperature [K]; 
	- steel_t_lim : float
	   Min, max allowable steel temperature [K]; 
	- lipb_t_lim : float
	   Min, max allowable LiPb temperature [K]; 
	'''

	def __init__(self, base_path_in='mat_lim'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.cool_t_lim = EMPTY_DOUBLE
		self.steel_t_lim = EMPTY_DOUBLE
		self.lipb_t_lim = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mat_limstructuremat_lim\n'
		ret = ret + space + 'Attribute cool_t_lim: ' + str(self.cool_t_lim) + '\n'
		ret = ret + space + 'Attribute steel_t_lim: ' + str(self.steel_t_lim) + '\n'
		ret = ret + space + 'Attribute lipb_t_lim: ' + str(self.lipb_t_lim) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mat_limstructuremat_lim, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mat_limstructuremat_lim, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mat_limstructuremat_lim, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'cool_t_lim', self.cool_t_lim)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'steel_t_lim', self.steel_t_lim)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'lipb_t_lim', self.lipb_t_lim)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mat_limstructuremat_lim, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_cool_t_lim = ull.getDouble(self.idx, path, cpopath + 'cool_t_lim')
		check_status(status)
		if not status:
			self.cool_t_lim = ret_cool_t_lim
		status, ret_steel_t_lim = ull.getDouble(self.idx, path, cpopath + 'steel_t_lim')
		check_status(status)
		if not status:
			self.steel_t_lim = ret_steel_t_lim
		status, ret_lipb_t_lim = ull.getDouble(self.idx, path, cpopath + 'lipb_t_lim')
		check_status(status)
		if not status:
			self.lipb_t_lim = ret_lipb_t_lim

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mat_limstructuremat_lim, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, cool_t_limVal = ull.getDouble(self.idx, path, cpopath + 'cool_t_lim')
			check_status(status)
			status, steel_t_limVal = ull.getDouble(self.idx, path, cpopath + 'steel_t_lim')
			check_status(status)
			status, lipb_t_limVal = ull.getDouble(self.idx, path, cpopath + 'lipb_t_lim')
			check_status(status)
			for i in range(nbslice):
				slice = mat_limstructuremat_lim(self.base_path)
				slice.setExpIdx(self.idx)
				slice.cool_t_lim = cool_t_limVal
				slice.steel_t_lim = steel_t_limVal
				slice.lipb_t_lim = lipb_t_limVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mat_limstructuremat_limObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mat_limstructuremat_limObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mat_limstructuremat_limObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'cool_t_lim') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'cool_t_lim', i, self.cool_t_lim)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'steel_t_lim') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'steel_t_lim', i, self.steel_t_lim)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'lipb_t_lim') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'lipb_t_lim', i, self.lipb_t_lim)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mat_limstructuremat_limObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'cool_t_lim') 
			print ('obj = ' + str(obj))
		status, ret_cool_t_lim = ull.getDoubleFromObject(self.idx, obj, cpopath + 'cool_t_lim', i)
		check_status(status)
		if not status:
			self.cool_t_lim = ret_cool_t_lim
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'steel_t_lim') 
			print ('obj = ' + str(obj))
		status, ret_steel_t_lim = ull.getDoubleFromObject(self.idx, obj, cpopath + 'steel_t_lim', i)
		check_status(status)
		if not status:
			self.steel_t_lim = ret_steel_t_lim
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'lipb_t_lim') 
			print ('obj = ' + str(obj))
		status, ret_lipb_t_lim = ull.getDoubleFromObject(self.idx, obj, cpopath + 'lipb_t_lim', i)
		check_status(status)
		if not status:
			self.lipb_t_lim = ret_lipb_t_lim

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'cool_t_lim')
		ull.deleteData(self.idx, path, cpopath + 'steel_t_lim')
		ull.deleteData(self.idx, path, cpopath + 'lipb_t_lim')


class hcll_bbstructurehcll_bb:
	'''
	class hcll_bbstructurehcll_bb
	HCLL breeding blanket. Radially, the blanket is divided in 4 layers: 1: First Wall, 2 : breeder zone, 3 : back plates, 4 : manifolds 

	Attributes:
	- bb_lifetime : float
	   Breeding blanket lifetime [years]; Scalar
	- he_inl_t : float
	   Inlet temperature (to the bb module) [K]; Scalar
	- he_fr : float
	   Coolant mass flow rate in "the" reference bb module (or in each module) [Kg/s];
	- he_inl_p : float
	   Helium inlet pressure [Pa]; Scalar
	- loca_des_p : float
	   Box design pressure (coincident He circuit design pressure) [Pa]; Scalar
	- he_dp : float
	   Coolant pressure drops in the breeding blankets [Pa]; Scalar
	- lipb_dp : float
	   Pb-15.7Li pressure drops in the bb [Pa]; Scalar
	- react : class reactstructurereact
	   In the reactor region
	- inboard : class inboardstructurehcllbb_specs
	   Inboard
	- outboard : class outboardstructurehcllbb_specs
	   Outboard
	'''

	def __init__(self, base_path_in='hcll_bb'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.bb_lifetime = EMPTY_DOUBLE
		self.he_inl_t = EMPTY_DOUBLE
		self.he_fr = EMPTY_DOUBLE
		self.he_inl_p = EMPTY_DOUBLE
		self.loca_des_p = EMPTY_DOUBLE
		self.he_dp = EMPTY_DOUBLE
		self.lipb_dp = EMPTY_DOUBLE
		self.react = reactstructurereact('react')
		self.inboard = inboardstructurehcllbb_specs('inboard')
		self.outboard = outboardstructurehcllbb_specs('outboard')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class hcll_bbstructurehcll_bb\n'
		ret = ret + space + 'Attribute bb_lifetime: ' + str(self.bb_lifetime) + '\n'
		ret = ret + space + 'Attribute he_inl_t: ' + str(self.he_inl_t) + '\n'
		ret = ret + space + 'Attribute he_fr: ' + str(self.he_fr) + '\n'
		ret = ret + space + 'Attribute he_inl_p: ' + str(self.he_inl_p) + '\n'
		ret = ret + space + 'Attribute loca_des_p: ' + str(self.loca_des_p) + '\n'
		ret = ret + space + 'Attribute he_dp: ' + str(self.he_dp) + '\n'
		ret = ret + space + 'Attribute lipb_dp: ' + str(self.lipb_dp) + '\n'
		ret = ret + space + 'Attribute react\n ' + self.react.__str__(depth+1)
		ret = ret + space + 'Attribute inboard\n ' + self.inboard.__str__(depth+1)
		ret = ret + space + 'Attribute outboard\n ' + self.outboard.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.react.setExpIdx(idx)
		self.inboard.setExpIdx(idx)
		self.outboard.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type hcll_bbstructurehcll_bb, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.react.cpoTime = self.cpoTime
		self.react.putSlice(path, cpopath)
		self.inboard.cpoTime = self.cpoTime
		self.inboard.putSlice(path, cpopath)
		self.outboard.cpoTime = self.cpoTime
		self.outboard.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type hcll_bbstructurehcll_bb, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.react.replaceLastSlice(path, cpopath)
		self.inboard.replaceLastSlice(path, cpopath)
		self.outboard.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type hcll_bbstructurehcll_bb, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'bb_lifetime', self.bb_lifetime)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_inl_t', self.he_inl_t)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_fr', self.he_fr)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_inl_p', self.he_inl_p)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'loca_des_p', self.loca_des_p)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_dp', self.he_dp)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'lipb_dp', self.lipb_dp)
		check_status(status)
		self.react.putNonTimed(path, cpopath)
		self.inboard.putNonTimed(path, cpopath)
		self.outboard.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type hcll_bbstructurehcll_bb, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_bb_lifetime = ull.getDouble(self.idx, path, cpopath + 'bb_lifetime')
		check_status(status)
		if not status:
			self.bb_lifetime = ret_bb_lifetime
		status, ret_he_inl_t = ull.getDouble(self.idx, path, cpopath + 'he_inl_t')
		check_status(status)
		if not status:
			self.he_inl_t = ret_he_inl_t
		status, ret_he_fr = ull.getDouble(self.idx, path, cpopath + 'he_fr')
		check_status(status)
		if not status:
			self.he_fr = ret_he_fr
		status, ret_he_inl_p = ull.getDouble(self.idx, path, cpopath + 'he_inl_p')
		check_status(status)
		if not status:
			self.he_inl_p = ret_he_inl_p
		status, ret_loca_des_p = ull.getDouble(self.idx, path, cpopath + 'loca_des_p')
		check_status(status)
		if not status:
			self.loca_des_p = ret_loca_des_p
		status, ret_he_dp = ull.getDouble(self.idx, path, cpopath + 'he_dp')
		check_status(status)
		if not status:
			self.he_dp = ret_he_dp
		status, ret_lipb_dp = ull.getDouble(self.idx, path, cpopath + 'lipb_dp')
		check_status(status)
		if not status:
			self.lipb_dp = ret_lipb_dp
		self.react.getSlice(path, cpopath, inTime, interpolMode)
		self.inboard.getSlice(path, cpopath, inTime, interpolMode)
		self.outboard.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type hcll_bbstructurehcll_bb, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, bb_lifetimeVal = ull.getDouble(self.idx, path, cpopath + 'bb_lifetime')
			check_status(status)
			status, he_inl_tVal = ull.getDouble(self.idx, path, cpopath + 'he_inl_t')
			check_status(status)
			status, he_frVal = ull.getDouble(self.idx, path, cpopath + 'he_fr')
			check_status(status)
			status, he_inl_pVal = ull.getDouble(self.idx, path, cpopath + 'he_inl_p')
			check_status(status)
			status, loca_des_pVal = ull.getDouble(self.idx, path, cpopath + 'loca_des_p')
			check_status(status)
			status, he_dpVal = ull.getDouble(self.idx, path, cpopath + 'he_dp')
			check_status(status)
			status, lipb_dpVal = ull.getDouble(self.idx, path, cpopath + 'lipb_dp')
			check_status(status)
			reactList = self.react.build_non_resampled_data(path, cpopath, nbslice)
			inboardList = self.inboard.build_non_resampled_data(path, cpopath, nbslice)
			outboardList = self.outboard.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = hcll_bbstructurehcll_bb(self.base_path)
				slice.setExpIdx(self.idx)
				slice.bb_lifetime = bb_lifetimeVal
				slice.he_inl_t = he_inl_tVal
				slice.he_fr = he_frVal
				slice.he_inl_p = he_inl_pVal
				slice.loca_des_p = loca_des_pVal
				slice.he_dp = he_dpVal
				slice.lipb_dp = lipb_dpVal
				slice.react = reactList[i]
				slice.inboard = inboardList[i]
				slice.outboard = outboardList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type hcll_bbstructurehcll_bbObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type hcll_bbstructurehcll_bbObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type hcll_bbstructurehcll_bbObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'bb_lifetime') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'bb_lifetime', i, self.bb_lifetime)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_inl_t') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_inl_t', i, self.he_inl_t)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_fr') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_fr', i, self.he_fr)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_inl_p') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_inl_p', i, self.he_inl_p)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'loca_des_p') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'loca_des_p', i, self.loca_des_p)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_dp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_dp', i, self.he_dp)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'lipb_dp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'lipb_dp', i, self.lipb_dp)
		obj = self.react.putNonTimedElt(path, cpopath + 'react', i, obj)
		obj = self.inboard.putNonTimedElt(path, cpopath + 'inboard', i, obj)
		obj = self.outboard.putNonTimedElt(path, cpopath + 'outboard', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type hcll_bbstructurehcll_bbObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'bb_lifetime') 
			print ('obj = ' + str(obj))
		status, ret_bb_lifetime = ull.getDoubleFromObject(self.idx, obj, cpopath + 'bb_lifetime', i)
		check_status(status)
		if not status:
			self.bb_lifetime = ret_bb_lifetime
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_inl_t') 
			print ('obj = ' + str(obj))
		status, ret_he_inl_t = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_inl_t', i)
		check_status(status)
		if not status:
			self.he_inl_t = ret_he_inl_t
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_fr') 
			print ('obj = ' + str(obj))
		status, ret_he_fr = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_fr', i)
		check_status(status)
		if not status:
			self.he_fr = ret_he_fr
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_inl_p') 
			print ('obj = ' + str(obj))
		status, ret_he_inl_p = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_inl_p', i)
		check_status(status)
		if not status:
			self.he_inl_p = ret_he_inl_p
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'loca_des_p') 
			print ('obj = ' + str(obj))
		status, ret_loca_des_p = ull.getDoubleFromObject(self.idx, obj, cpopath + 'loca_des_p', i)
		check_status(status)
		if not status:
			self.loca_des_p = ret_loca_des_p
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_dp') 
			print ('obj = ' + str(obj))
		status, ret_he_dp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_dp', i)
		check_status(status)
		if not status:
			self.he_dp = ret_he_dp
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'lipb_dp') 
			print ('obj = ' + str(obj))
		status, ret_lipb_dp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'lipb_dp', i)
		check_status(status)
		if not status:
			self.lipb_dp = ret_lipb_dp
		self.react.getNonTimedElt(path, cpopath + 'react', i, obj)
		self.inboard.getNonTimedElt(path, cpopath + 'inboard', i, obj)
		self.outboard.getNonTimedElt(path, cpopath + 'outboard', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'bb_lifetime')
		ull.deleteData(self.idx, path, cpopath + 'he_inl_t')
		ull.deleteData(self.idx, path, cpopath + 'he_fr')
		ull.deleteData(self.idx, path, cpopath + 'he_inl_p')
		ull.deleteData(self.idx, path, cpopath + 'loca_des_p')
		ull.deleteData(self.idx, path, cpopath + 'he_dp')
		ull.deleteData(self.idx, path, cpopath + 'lipb_dp')
		self.react.deleteData(path, cpopath)
		self.inboard.deleteData(path, cpopath)
		self.outboard.deleteData(path, cpopath)


class reactstructurereact:
	'''
	class reactstructurereact
	In the reactor region

	Attributes:
	- he_fr : float
	   Coolant mass flow rate in the whole reactor [Kg/s]; Scalar
	- lp_fr : float
	   Pb-15.7Li mass flow rate in  the whole reactor [Kg/s]; Scalar
	- he_dp : float
	   Coolant pressure drops in the reactor (compressing pipelines) [Pa]; Scalar
	- lipb_dp : float
	   Pb-15.7Li pressure drops in the reactor [Pa]; Scalar
	'''

	def __init__(self, base_path_in='react'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.he_fr = EMPTY_DOUBLE
		self.lp_fr = EMPTY_DOUBLE
		self.he_dp = EMPTY_DOUBLE
		self.lipb_dp = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class reactstructurereact\n'
		ret = ret + space + 'Attribute he_fr: ' + str(self.he_fr) + '\n'
		ret = ret + space + 'Attribute lp_fr: ' + str(self.lp_fr) + '\n'
		ret = ret + space + 'Attribute he_dp: ' + str(self.he_dp) + '\n'
		ret = ret + space + 'Attribute lipb_dp: ' + str(self.lipb_dp) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type reactstructurereact, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type reactstructurereact, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type reactstructurereact, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'he_fr', self.he_fr)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'lp_fr', self.lp_fr)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_dp', self.he_dp)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'lipb_dp', self.lipb_dp)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type reactstructurereact, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_he_fr = ull.getDouble(self.idx, path, cpopath + 'he_fr')
		check_status(status)
		if not status:
			self.he_fr = ret_he_fr
		status, ret_lp_fr = ull.getDouble(self.idx, path, cpopath + 'lp_fr')
		check_status(status)
		if not status:
			self.lp_fr = ret_lp_fr
		status, ret_he_dp = ull.getDouble(self.idx, path, cpopath + 'he_dp')
		check_status(status)
		if not status:
			self.he_dp = ret_he_dp
		status, ret_lipb_dp = ull.getDouble(self.idx, path, cpopath + 'lipb_dp')
		check_status(status)
		if not status:
			self.lipb_dp = ret_lipb_dp

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type reactstructurereact, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, he_frVal = ull.getDouble(self.idx, path, cpopath + 'he_fr')
			check_status(status)
			status, lp_frVal = ull.getDouble(self.idx, path, cpopath + 'lp_fr')
			check_status(status)
			status, he_dpVal = ull.getDouble(self.idx, path, cpopath + 'he_dp')
			check_status(status)
			status, lipb_dpVal = ull.getDouble(self.idx, path, cpopath + 'lipb_dp')
			check_status(status)
			for i in range(nbslice):
				slice = reactstructurereact(self.base_path)
				slice.setExpIdx(self.idx)
				slice.he_fr = he_frVal
				slice.lp_fr = lp_frVal
				slice.he_dp = he_dpVal
				slice.lipb_dp = lipb_dpVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type reactstructurereactObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type reactstructurereactObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type reactstructurereactObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_fr') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_fr', i, self.he_fr)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'lp_fr') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'lp_fr', i, self.lp_fr)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_dp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_dp', i, self.he_dp)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'lipb_dp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'lipb_dp', i, self.lipb_dp)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type reactstructurereactObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_fr') 
			print ('obj = ' + str(obj))
		status, ret_he_fr = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_fr', i)
		check_status(status)
		if not status:
			self.he_fr = ret_he_fr
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'lp_fr') 
			print ('obj = ' + str(obj))
		status, ret_lp_fr = ull.getDoubleFromObject(self.idx, obj, cpopath + 'lp_fr', i)
		check_status(status)
		if not status:
			self.lp_fr = ret_lp_fr
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_dp') 
			print ('obj = ' + str(obj))
		status, ret_he_dp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_dp', i)
		check_status(status)
		if not status:
			self.he_dp = ret_he_dp
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'lipb_dp') 
			print ('obj = ' + str(obj))
		status, ret_lipb_dp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'lipb_dp', i)
		check_status(status)
		if not status:
			self.lipb_dp = ret_lipb_dp

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'he_fr')
		ull.deleteData(self.idx, path, cpopath + 'lp_fr')
		ull.deleteData(self.idx, path, cpopath + 'he_dp')
		ull.deleteData(self.idx, path, cpopath + 'lipb_dp')


class inboardstructurehcllbb_specs:
	'''
	class inboardstructurehcllbb_specs
	Inboard

	Attributes:
	- mass : numpy.ndarray 1D with float
	   Mass of inboard or outboard breeding blanket modules (located at equatorial midplane if only one considered) [Kg]; Vector(nmodules)
	- dr : numpy.ndarray 1D with float
	   Inboard or outboard breeding blanket radial build giving the thickness of each layer [m]; Vector(nlayers)
	- mat : numpy.ndarray 1D with float
	   Inboard or outboard breeding blanket materials; Vector(nlayers)
	- composition : numpy.ndarray 2D with float
	   Inboard or outboard breeding blanket radial build giving for each layer (1: First Wall protective layer, 2: First Wall, 3 : breeder zone, 4 : back plates, 5 : manifolds), the percentage of each material respectively (Meaning of the material index 1: Eurofer, 2: Pb-15.7Li, 3: He, 4: Water, 5: Tungsten Carbide, 6: Boron, 7: Tungsten, 8: Stainless Steel 316) in %vol; Matrix(nlayers(=5), max_nmaterials)
	- mod_geom : class mod_geomstructurebb_geometry
	   Geometrical parameters of "the" reference region blanket module
	- mod_neutr : class mod_neutrstructuremode_neutr
	   Neutrons "effects"
	- mod_therm : class mod_thermstructuremode_therm
	   Thermical parameters
	- mod_th_hyd : class mod_th_hydstructuremode_th_hyd
	   hydrodynamics parameters
	- mod_mech : class mod_mechstructuremode_mech
	   Mechanical parameters
	- mod_lipb : class mod_lipbstructuremode_lipb
	   Pb-15.7Li "effects"
	- mod_tritium : class mod_tritiumstructuremode_tritium
	   Tritium parameters
	'''

	def __init__(self, base_path_in='inboard'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.mass = numpy.zeros(0, numpy.float64, order='C')
		self.dr = numpy.zeros(0, numpy.float64, order='C')
		self.mat = numpy.zeros(0, numpy.float64, order='C')
		self.composition = numpy.zeros((0,0), numpy.float64, order='C')
		self.mod_geom = mod_geomstructurebb_geometry('mod_geom')
		self.mod_neutr = mod_neutrstructuremode_neutr('mod_neutr')
		self.mod_therm = mod_thermstructuremode_therm('mod_therm')
		self.mod_th_hyd = mod_th_hydstructuremode_th_hyd('mod_th_hyd')
		self.mod_mech = mod_mechstructuremode_mech('mod_mech')
		self.mod_lipb = mod_lipbstructuremode_lipb('mod_lipb')
		self.mod_tritium = mod_tritiumstructuremode_tritium('mod_tritium')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class inboardstructurehcllbb_specs\n'
		s = self.mass.__str__()
		ret = ret + space + 'Attribute mass\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dr.__str__()
		ret = ret + space + 'Attribute dr\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.mat.__str__()
		ret = ret + space + 'Attribute mat\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.composition.__str__()
		ret = ret + space + 'Attribute composition\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute mod_geom\n ' + self.mod_geom.__str__(depth+1)
		ret = ret + space + 'Attribute mod_neutr\n ' + self.mod_neutr.__str__(depth+1)
		ret = ret + space + 'Attribute mod_therm\n ' + self.mod_therm.__str__(depth+1)
		ret = ret + space + 'Attribute mod_th_hyd\n ' + self.mod_th_hyd.__str__(depth+1)
		ret = ret + space + 'Attribute mod_mech\n ' + self.mod_mech.__str__(depth+1)
		ret = ret + space + 'Attribute mod_lipb\n ' + self.mod_lipb.__str__(depth+1)
		ret = ret + space + 'Attribute mod_tritium\n ' + self.mod_tritium.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.mod_geom.setExpIdx(idx)
		self.mod_neutr.setExpIdx(idx)
		self.mod_therm.setExpIdx(idx)
		self.mod_th_hyd.setExpIdx(idx)
		self.mod_mech.setExpIdx(idx)
		self.mod_lipb.setExpIdx(idx)
		self.mod_tritium.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructurehcllbb_specs, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mod_geom.cpoTime = self.cpoTime
		self.mod_geom.putSlice(path, cpopath)
		self.mod_neutr.cpoTime = self.cpoTime
		self.mod_neutr.putSlice(path, cpopath)
		self.mod_therm.cpoTime = self.cpoTime
		self.mod_therm.putSlice(path, cpopath)
		self.mod_th_hyd.cpoTime = self.cpoTime
		self.mod_th_hyd.putSlice(path, cpopath)
		self.mod_mech.cpoTime = self.cpoTime
		self.mod_mech.putSlice(path, cpopath)
		self.mod_lipb.cpoTime = self.cpoTime
		self.mod_lipb.putSlice(path, cpopath)
		self.mod_tritium.cpoTime = self.cpoTime
		self.mod_tritium.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructurehcllbb_specs, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mod_geom.replaceLastSlice(path, cpopath)
		self.mod_neutr.replaceLastSlice(path, cpopath)
		self.mod_therm.replaceLastSlice(path, cpopath)
		self.mod_th_hyd.replaceLastSlice(path, cpopath)
		self.mod_mech.replaceLastSlice(path, cpopath)
		self.mod_lipb.replaceLastSlice(path, cpopath)
		self.mod_tritium.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructurehcllbb_specs, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'mass', numpy.array(self.mass).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'dr', numpy.array(self.dr).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'mat', numpy.array(self.mat).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'composition', numpy.array(self.composition).astype(numpy.float64), False)
		check_status(status)
		self.mod_geom.putNonTimed(path, cpopath)
		self.mod_neutr.putNonTimed(path, cpopath)
		self.mod_therm.putNonTimed(path, cpopath)
		self.mod_th_hyd.putNonTimed(path, cpopath)
		self.mod_mech.putNonTimed(path, cpopath)
		self.mod_lipb.putNonTimed(path, cpopath)
		self.mod_tritium.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructurehcllbb_specs, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_mass = ull.getVect1DDouble(self.idx, path, cpopath + 'mass')
		check_status(status)
		if not status:
			self.mass = ret_mass
		status, ret_dr = ull.getVect1DDouble(self.idx, path, cpopath + 'dr')
		check_status(status)
		if not status:
			self.dr = ret_dr
		status, ret_mat = ull.getVect1DDouble(self.idx, path, cpopath + 'mat')
		check_status(status)
		if not status:
			self.mat = ret_mat
		status, ret_composition = ull.getVect2DDouble(self.idx, path, cpopath + 'composition')
		check_status(status)
		if not status:
			self.composition = ret_composition
		self.mod_geom.getSlice(path, cpopath, inTime, interpolMode)
		self.mod_neutr.getSlice(path, cpopath, inTime, interpolMode)
		self.mod_therm.getSlice(path, cpopath, inTime, interpolMode)
		self.mod_th_hyd.getSlice(path, cpopath, inTime, interpolMode)
		self.mod_mech.getSlice(path, cpopath, inTime, interpolMode)
		self.mod_lipb.getSlice(path, cpopath, inTime, interpolMode)
		self.mod_tritium.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type inboardstructurehcllbb_specs, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, massVal = ull.getVect1DDouble(self.idx, path, cpopath + 'mass')
			check_status(status)
			status, drVal = ull.getVect1DDouble(self.idx, path, cpopath + 'dr')
			check_status(status)
			status, matVal = ull.getVect1DDouble(self.idx, path, cpopath + 'mat')
			check_status(status)
			status, compositionVal = ull.getVect2DDouble(self.idx, path, cpopath + 'composition')
			check_status(status)
			mod_geomList = self.mod_geom.build_non_resampled_data(path, cpopath, nbslice)
			mod_neutrList = self.mod_neutr.build_non_resampled_data(path, cpopath, nbslice)
			mod_thermList = self.mod_therm.build_non_resampled_data(path, cpopath, nbslice)
			mod_th_hydList = self.mod_th_hyd.build_non_resampled_data(path, cpopath, nbslice)
			mod_mechList = self.mod_mech.build_non_resampled_data(path, cpopath, nbslice)
			mod_lipbList = self.mod_lipb.build_non_resampled_data(path, cpopath, nbslice)
			mod_tritiumList = self.mod_tritium.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = inboardstructurehcllbb_specs(self.base_path)
				slice.setExpIdx(self.idx)
				slice.mass = massVal
				slice.dr = drVal
				slice.mat = matVal
				slice.composition = compositionVal
				slice.mod_geom = mod_geomList[i]
				slice.mod_neutr = mod_neutrList[i]
				slice.mod_therm = mod_thermList[i]
				slice.mod_th_hyd = mod_th_hydList[i]
				slice.mod_mech = mod_mechList[i]
				slice.mod_lipb = mod_lipbList[i]
				slice.mod_tritium = mod_tritiumList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructurehcllbb_specsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructurehcllbb_specsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructurehcllbb_specsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'mass') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'mass', i, numpy.array(self.mass).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dr') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dr', i, numpy.array(self.dr).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'mat') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'mat', i, numpy.array(self.mat).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'composition') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'composition', i, numpy.array(self.composition).astype(numpy.float64))
		obj = self.mod_geom.putNonTimedElt(path, cpopath + 'mod_geom', i, obj)
		obj = self.mod_neutr.putNonTimedElt(path, cpopath + 'mod_neutr', i, obj)
		obj = self.mod_therm.putNonTimedElt(path, cpopath + 'mod_therm', i, obj)
		obj = self.mod_th_hyd.putNonTimedElt(path, cpopath + 'mod_th_hyd', i, obj)
		obj = self.mod_mech.putNonTimedElt(path, cpopath + 'mod_mech', i, obj)
		obj = self.mod_lipb.putNonTimedElt(path, cpopath + 'mod_lipb', i, obj)
		obj = self.mod_tritium.putNonTimedElt(path, cpopath + 'mod_tritium', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inboardstructurehcllbb_specsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'mass') 
			print ('obj = ' + str(obj))
		status, ret_mass = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'mass', i)
		check_status(status)
		if not status:
			self.mass = ret_mass
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dr') 
			print ('obj = ' + str(obj))
		status, ret_dr = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dr', i)
		check_status(status)
		if not status:
			self.dr = ret_dr
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'mat') 
			print ('obj = ' + str(obj))
		status, ret_mat = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'mat', i)
		check_status(status)
		if not status:
			self.mat = ret_mat
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'composition') 
			print ('obj = ' + str(obj))
		status, ret_composition = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'composition', i)
		check_status(status)
		if not status:
			self.composition = ret_composition
		self.mod_geom.getNonTimedElt(path, cpopath + 'mod_geom', i, obj)
		self.mod_neutr.getNonTimedElt(path, cpopath + 'mod_neutr', i, obj)
		self.mod_therm.getNonTimedElt(path, cpopath + 'mod_therm', i, obj)
		self.mod_th_hyd.getNonTimedElt(path, cpopath + 'mod_th_hyd', i, obj)
		self.mod_mech.getNonTimedElt(path, cpopath + 'mod_mech', i, obj)
		self.mod_lipb.getNonTimedElt(path, cpopath + 'mod_lipb', i, obj)
		self.mod_tritium.getNonTimedElt(path, cpopath + 'mod_tritium', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'mass')
		ull.deleteData(self.idx, path, cpopath + 'dr')
		ull.deleteData(self.idx, path, cpopath + 'mat')
		ull.deleteData(self.idx, path, cpopath + 'composition')
		self.mod_geom.deleteData(path, cpopath)
		self.mod_neutr.deleteData(path, cpopath)
		self.mod_therm.deleteData(path, cpopath)
		self.mod_th_hyd.deleteData(path, cpopath)
		self.mod_mech.deleteData(path, cpopath)
		self.mod_lipb.deleteData(path, cpopath)
		self.mod_tritium.deleteData(path, cpopath)


class mod_geomstructurebb_geometry:
	'''
	class mod_geomstructurebb_geometry
	Geometrical parameters of "the" reference region blanket module

	Attributes:
	- dr_fw : float
	   Radial thickness of the FW [m]; Scalar
	- dr_bz : float
	   Radial thickness of the BZ (between the FW and the 1st back plate wall) [m]; Scalar
	- dr_bp : float
	   Radial thickness of the BPs integrated to the module [m]; Scalar
	- dr_bp_plates : numpy.ndarray 1D with float
	   Radial thickness of every BP integrated to the module [m]; Vector(nplates)
	- dr_bp_he : numpy.ndarray 1D with float
	   Radial thickness of Helium layers [m]; Vector(nplates)
	- dr_man : float
	   Radial thickness of the banana manifold common to all modules [m]; Scalar
	- dt_sw : float
	   Toroidal thickness of side walls (or covers) [m]; Scalar
	- dt_bz : float
	   Toroidal dimension of the BZ (between the two side walls [m]; Scalar
	- dp_bz : float
	   Poloidal dimension of the Breeder zone [m]; Scalar
	- top_cap_dim : class top_cap_dimstructurebb_dimension
	   Top cap dimension of bb modules
	- bot_cap_dim : class bot_cap_dimstructurebb_dimension
	   Bottom cap dimension of bb modules
	- a_fw_ch : float
	   First wall channel radial dimension [m]; Scalar
	- b_fw_ch : float
	   First wall channel toroidal dimension [m]; Scalar
	- td_tc_ch : float
	   Top cap channel toroidal dimension [m]; Scalar
	- rd_tc_ch : float
	   Top cap channel radial dimension [m]; Scalar
	- td_bc_ch : float
	   Bottom cap channel toroidal dimension [m]; Scalar
	- rd_bc_ch : float
	   Bottom cap channel radial dimension [m]; Scalar
	- n_fw_ch : float
	   Number of first wall channels; Scalar
	- n_fw_circ : float
	   Number of circulation in channel first wall channels; Scalar
	- a_sg_ch : float
	   Stiffening grid channel dimension 1 [m]; Scalar
	- b_sg_ch : float
	   Stiffening grid channel dimension 2 [m]; Scalar
	- n_sg_ch : float
	   Number of channels per stiffening plate [m]; Scalar
	- sg_thick : float
	   Stiffening grid thickness [m]; Scalar
	- sg_weld : float
	   Stiffening grid required dimension for welding [m]; Scalar
	- sg_in_out : float
	   Stiffening grid input/output geometry length [m]; Scalar
	- r_sg_cp : float
	   Percentage of the cooling plate length [-]; Scalar
	- cp_tor_gap : float
	   Gap between cooling plates and toroidal breeder [m]; Scalar
	- a_cp_ch : float
	   Cooling plates channel dimension 1 [m]; Scalar
	- b_cp_ch : float
	   Cooling plates channel dimension 2 [m]; Scalar
	- n_cp_ch : float
	   Number of channels per cooling plates [m]; Scalar
	- cp_thick : float
	   Cooling plates thickness [m]; Scalar
	- n_pol_bu : float
	   Number of poloidal breeder units; Scalar
	- n_tor_bu : float
	   Number of toroidal breeder units; Scalar
	- n_cp_bu : float
	   Number of cooling plates per breeder unit; Scalar
	- cp_in_out : float
	   Cooling plate input/output geometry length [m]; Scalar
	- he_man_tck : float
	   Helium stage manifold thickness [m]; Scalar
	- man_tck : float
	   Manifold zone thickness [m]; Scalar
	- pbli_bptb_od : float
	   Output diameter of pbli tube [m]; Scalar
	- pbli_bptb_id : float
	   Input diameter of pbli tube [m]; Scalar
	- he_bptb_od : float
	   Output diameter of He inlet tube [m]; Scalar
	- he_bptb_id : float
	   Input diameter of He inlet tube [m]; Scalar
	- dr_max_fw : float
	   First wall frontmost thickness [m]; Scalar
	- dr_fwpl : float
	   Radial thickness of fisrt protective layer [m]; Scalar
	'''

	def __init__(self, base_path_in='mod_geom'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dr_fw = EMPTY_DOUBLE
		self.dr_bz = EMPTY_DOUBLE
		self.dr_bp = EMPTY_DOUBLE
		self.dr_bp_plates = numpy.zeros(0, numpy.float64, order='C')
		self.dr_bp_he = numpy.zeros(0, numpy.float64, order='C')
		self.dr_man = EMPTY_DOUBLE
		self.dt_sw = EMPTY_DOUBLE
		self.dt_bz = EMPTY_DOUBLE
		self.dp_bz = EMPTY_DOUBLE
		self.top_cap_dim = top_cap_dimstructurebb_dimension('top_cap_dim')
		self.bot_cap_dim = bot_cap_dimstructurebb_dimension('bot_cap_dim')
		self.a_fw_ch = EMPTY_DOUBLE
		self.b_fw_ch = EMPTY_DOUBLE
		self.td_tc_ch = EMPTY_DOUBLE
		self.rd_tc_ch = EMPTY_DOUBLE
		self.td_bc_ch = EMPTY_DOUBLE
		self.rd_bc_ch = EMPTY_DOUBLE
		self.n_fw_ch = EMPTY_DOUBLE
		self.n_fw_circ = EMPTY_DOUBLE
		self.a_sg_ch = EMPTY_DOUBLE
		self.b_sg_ch = EMPTY_DOUBLE
		self.n_sg_ch = EMPTY_DOUBLE
		self.sg_thick = EMPTY_DOUBLE
		self.sg_weld = EMPTY_DOUBLE
		self.sg_in_out = EMPTY_DOUBLE
		self.r_sg_cp = EMPTY_DOUBLE
		self.cp_tor_gap = EMPTY_DOUBLE
		self.a_cp_ch = EMPTY_DOUBLE
		self.b_cp_ch = EMPTY_DOUBLE
		self.n_cp_ch = EMPTY_DOUBLE
		self.cp_thick = EMPTY_DOUBLE
		self.n_pol_bu = EMPTY_DOUBLE
		self.n_tor_bu = EMPTY_DOUBLE
		self.n_cp_bu = EMPTY_DOUBLE
		self.cp_in_out = EMPTY_DOUBLE
		self.he_man_tck = EMPTY_DOUBLE
		self.man_tck = EMPTY_DOUBLE
		self.pbli_bptb_od = EMPTY_DOUBLE
		self.pbli_bptb_id = EMPTY_DOUBLE
		self.he_bptb_od = EMPTY_DOUBLE
		self.he_bptb_id = EMPTY_DOUBLE
		self.dr_max_fw = EMPTY_DOUBLE
		self.dr_fwpl = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mod_geomstructurebb_geometry\n'
		ret = ret + space + 'Attribute dr_fw: ' + str(self.dr_fw) + '\n'
		ret = ret + space + 'Attribute dr_bz: ' + str(self.dr_bz) + '\n'
		ret = ret + space + 'Attribute dr_bp: ' + str(self.dr_bp) + '\n'
		s = self.dr_bp_plates.__str__()
		ret = ret + space + 'Attribute dr_bp_plates\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dr_bp_he.__str__()
		ret = ret + space + 'Attribute dr_bp_he\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute dr_man: ' + str(self.dr_man) + '\n'
		ret = ret + space + 'Attribute dt_sw: ' + str(self.dt_sw) + '\n'
		ret = ret + space + 'Attribute dt_bz: ' + str(self.dt_bz) + '\n'
		ret = ret + space + 'Attribute dp_bz: ' + str(self.dp_bz) + '\n'
		ret = ret + space + 'Attribute top_cap_dim\n ' + self.top_cap_dim.__str__(depth+1)
		ret = ret + space + 'Attribute bot_cap_dim\n ' + self.bot_cap_dim.__str__(depth+1)
		ret = ret + space + 'Attribute a_fw_ch: ' + str(self.a_fw_ch) + '\n'
		ret = ret + space + 'Attribute b_fw_ch: ' + str(self.b_fw_ch) + '\n'
		ret = ret + space + 'Attribute td_tc_ch: ' + str(self.td_tc_ch) + '\n'
		ret = ret + space + 'Attribute rd_tc_ch: ' + str(self.rd_tc_ch) + '\n'
		ret = ret + space + 'Attribute td_bc_ch: ' + str(self.td_bc_ch) + '\n'
		ret = ret + space + 'Attribute rd_bc_ch: ' + str(self.rd_bc_ch) + '\n'
		ret = ret + space + 'Attribute n_fw_ch: ' + str(self.n_fw_ch) + '\n'
		ret = ret + space + 'Attribute n_fw_circ: ' + str(self.n_fw_circ) + '\n'
		ret = ret + space + 'Attribute a_sg_ch: ' + str(self.a_sg_ch) + '\n'
		ret = ret + space + 'Attribute b_sg_ch: ' + str(self.b_sg_ch) + '\n'
		ret = ret + space + 'Attribute n_sg_ch: ' + str(self.n_sg_ch) + '\n'
		ret = ret + space + 'Attribute sg_thick: ' + str(self.sg_thick) + '\n'
		ret = ret + space + 'Attribute sg_weld: ' + str(self.sg_weld) + '\n'
		ret = ret + space + 'Attribute sg_in_out: ' + str(self.sg_in_out) + '\n'
		ret = ret + space + 'Attribute r_sg_cp: ' + str(self.r_sg_cp) + '\n'
		ret = ret + space + 'Attribute cp_tor_gap: ' + str(self.cp_tor_gap) + '\n'
		ret = ret + space + 'Attribute a_cp_ch: ' + str(self.a_cp_ch) + '\n'
		ret = ret + space + 'Attribute b_cp_ch: ' + str(self.b_cp_ch) + '\n'
		ret = ret + space + 'Attribute n_cp_ch: ' + str(self.n_cp_ch) + '\n'
		ret = ret + space + 'Attribute cp_thick: ' + str(self.cp_thick) + '\n'
		ret = ret + space + 'Attribute n_pol_bu: ' + str(self.n_pol_bu) + '\n'
		ret = ret + space + 'Attribute n_tor_bu: ' + str(self.n_tor_bu) + '\n'
		ret = ret + space + 'Attribute n_cp_bu: ' + str(self.n_cp_bu) + '\n'
		ret = ret + space + 'Attribute cp_in_out: ' + str(self.cp_in_out) + '\n'
		ret = ret + space + 'Attribute he_man_tck: ' + str(self.he_man_tck) + '\n'
		ret = ret + space + 'Attribute man_tck: ' + str(self.man_tck) + '\n'
		ret = ret + space + 'Attribute pbli_bptb_od: ' + str(self.pbli_bptb_od) + '\n'
		ret = ret + space + 'Attribute pbli_bptb_id: ' + str(self.pbli_bptb_id) + '\n'
		ret = ret + space + 'Attribute he_bptb_od: ' + str(self.he_bptb_od) + '\n'
		ret = ret + space + 'Attribute he_bptb_id: ' + str(self.he_bptb_id) + '\n'
		ret = ret + space + 'Attribute dr_max_fw: ' + str(self.dr_max_fw) + '\n'
		ret = ret + space + 'Attribute dr_fwpl: ' + str(self.dr_fwpl) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.top_cap_dim.setExpIdx(idx)
		self.bot_cap_dim.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_geomstructurebb_geometry, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.top_cap_dim.cpoTime = self.cpoTime
		self.top_cap_dim.putSlice(path, cpopath)
		self.bot_cap_dim.cpoTime = self.cpoTime
		self.bot_cap_dim.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_geomstructurebb_geometry, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.top_cap_dim.replaceLastSlice(path, cpopath)
		self.bot_cap_dim.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_geomstructurebb_geometry, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'dr_fw', self.dr_fw)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dr_bz', self.dr_bz)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dr_bp', self.dr_bp)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'dr_bp_plates', numpy.array(self.dr_bp_plates).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'dr_bp_he', numpy.array(self.dr_bp_he).astype(numpy.float64), False)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dr_man', self.dr_man)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dt_sw', self.dt_sw)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dt_bz', self.dt_bz)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dp_bz', self.dp_bz)
		check_status(status)
		self.top_cap_dim.putNonTimed(path, cpopath)
		self.bot_cap_dim.putNonTimed(path, cpopath)
		status = ull.putDouble(self.idx, path, cpopath + 'a_fw_ch', self.a_fw_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'b_fw_ch', self.b_fw_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'td_tc_ch', self.td_tc_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'rd_tc_ch', self.rd_tc_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'td_bc_ch', self.td_bc_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'rd_bc_ch', self.rd_bc_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'n_fw_ch', self.n_fw_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'n_fw_circ', self.n_fw_circ)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'a_sg_ch', self.a_sg_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'b_sg_ch', self.b_sg_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'n_sg_ch', self.n_sg_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sg_thick', self.sg_thick)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sg_weld', self.sg_weld)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sg_in_out', self.sg_in_out)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'r_sg_cp', self.r_sg_cp)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'cp_tor_gap', self.cp_tor_gap)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'a_cp_ch', self.a_cp_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'b_cp_ch', self.b_cp_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'n_cp_ch', self.n_cp_ch)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'cp_thick', self.cp_thick)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'n_pol_bu', self.n_pol_bu)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'n_tor_bu', self.n_tor_bu)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'n_cp_bu', self.n_cp_bu)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'cp_in_out', self.cp_in_out)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_man_tck', self.he_man_tck)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'man_tck', self.man_tck)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'pbli_bptb_od', self.pbli_bptb_od)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'pbli_bptb_id', self.pbli_bptb_id)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_bptb_od', self.he_bptb_od)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_bptb_id', self.he_bptb_id)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dr_max_fw', self.dr_max_fw)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dr_fwpl', self.dr_fwpl)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mod_geomstructurebb_geometry, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dr_fw = ull.getDouble(self.idx, path, cpopath + 'dr_fw')
		check_status(status)
		if not status:
			self.dr_fw = ret_dr_fw
		status, ret_dr_bz = ull.getDouble(self.idx, path, cpopath + 'dr_bz')
		check_status(status)
		if not status:
			self.dr_bz = ret_dr_bz
		status, ret_dr_bp = ull.getDouble(self.idx, path, cpopath + 'dr_bp')
		check_status(status)
		if not status:
			self.dr_bp = ret_dr_bp
		status, ret_dr_bp_plates = ull.getVect1DDouble(self.idx, path, cpopath + 'dr_bp_plates')
		check_status(status)
		if not status:
			self.dr_bp_plates = ret_dr_bp_plates
		status, ret_dr_bp_he = ull.getVect1DDouble(self.idx, path, cpopath + 'dr_bp_he')
		check_status(status)
		if not status:
			self.dr_bp_he = ret_dr_bp_he
		status, ret_dr_man = ull.getDouble(self.idx, path, cpopath + 'dr_man')
		check_status(status)
		if not status:
			self.dr_man = ret_dr_man
		status, ret_dt_sw = ull.getDouble(self.idx, path, cpopath + 'dt_sw')
		check_status(status)
		if not status:
			self.dt_sw = ret_dt_sw
		status, ret_dt_bz = ull.getDouble(self.idx, path, cpopath + 'dt_bz')
		check_status(status)
		if not status:
			self.dt_bz = ret_dt_bz
		status, ret_dp_bz = ull.getDouble(self.idx, path, cpopath + 'dp_bz')
		check_status(status)
		if not status:
			self.dp_bz = ret_dp_bz
		self.top_cap_dim.getSlice(path, cpopath, inTime, interpolMode)
		self.bot_cap_dim.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_a_fw_ch = ull.getDouble(self.idx, path, cpopath + 'a_fw_ch')
		check_status(status)
		if not status:
			self.a_fw_ch = ret_a_fw_ch
		status, ret_b_fw_ch = ull.getDouble(self.idx, path, cpopath + 'b_fw_ch')
		check_status(status)
		if not status:
			self.b_fw_ch = ret_b_fw_ch
		status, ret_td_tc_ch = ull.getDouble(self.idx, path, cpopath + 'td_tc_ch')
		check_status(status)
		if not status:
			self.td_tc_ch = ret_td_tc_ch
		status, ret_rd_tc_ch = ull.getDouble(self.idx, path, cpopath + 'rd_tc_ch')
		check_status(status)
		if not status:
			self.rd_tc_ch = ret_rd_tc_ch
		status, ret_td_bc_ch = ull.getDouble(self.idx, path, cpopath + 'td_bc_ch')
		check_status(status)
		if not status:
			self.td_bc_ch = ret_td_bc_ch
		status, ret_rd_bc_ch = ull.getDouble(self.idx, path, cpopath + 'rd_bc_ch')
		check_status(status)
		if not status:
			self.rd_bc_ch = ret_rd_bc_ch
		status, ret_n_fw_ch = ull.getDouble(self.idx, path, cpopath + 'n_fw_ch')
		check_status(status)
		if not status:
			self.n_fw_ch = ret_n_fw_ch
		status, ret_n_fw_circ = ull.getDouble(self.idx, path, cpopath + 'n_fw_circ')
		check_status(status)
		if not status:
			self.n_fw_circ = ret_n_fw_circ
		status, ret_a_sg_ch = ull.getDouble(self.idx, path, cpopath + 'a_sg_ch')
		check_status(status)
		if not status:
			self.a_sg_ch = ret_a_sg_ch
		status, ret_b_sg_ch = ull.getDouble(self.idx, path, cpopath + 'b_sg_ch')
		check_status(status)
		if not status:
			self.b_sg_ch = ret_b_sg_ch
		status, ret_n_sg_ch = ull.getDouble(self.idx, path, cpopath + 'n_sg_ch')
		check_status(status)
		if not status:
			self.n_sg_ch = ret_n_sg_ch
		status, ret_sg_thick = ull.getDouble(self.idx, path, cpopath + 'sg_thick')
		check_status(status)
		if not status:
			self.sg_thick = ret_sg_thick
		status, ret_sg_weld = ull.getDouble(self.idx, path, cpopath + 'sg_weld')
		check_status(status)
		if not status:
			self.sg_weld = ret_sg_weld
		status, ret_sg_in_out = ull.getDouble(self.idx, path, cpopath + 'sg_in_out')
		check_status(status)
		if not status:
			self.sg_in_out = ret_sg_in_out
		status, ret_r_sg_cp = ull.getDouble(self.idx, path, cpopath + 'r_sg_cp')
		check_status(status)
		if not status:
			self.r_sg_cp = ret_r_sg_cp
		status, ret_cp_tor_gap = ull.getDouble(self.idx, path, cpopath + 'cp_tor_gap')
		check_status(status)
		if not status:
			self.cp_tor_gap = ret_cp_tor_gap
		status, ret_a_cp_ch = ull.getDouble(self.idx, path, cpopath + 'a_cp_ch')
		check_status(status)
		if not status:
			self.a_cp_ch = ret_a_cp_ch
		status, ret_b_cp_ch = ull.getDouble(self.idx, path, cpopath + 'b_cp_ch')
		check_status(status)
		if not status:
			self.b_cp_ch = ret_b_cp_ch
		status, ret_n_cp_ch = ull.getDouble(self.idx, path, cpopath + 'n_cp_ch')
		check_status(status)
		if not status:
			self.n_cp_ch = ret_n_cp_ch
		status, ret_cp_thick = ull.getDouble(self.idx, path, cpopath + 'cp_thick')
		check_status(status)
		if not status:
			self.cp_thick = ret_cp_thick
		status, ret_n_pol_bu = ull.getDouble(self.idx, path, cpopath + 'n_pol_bu')
		check_status(status)
		if not status:
			self.n_pol_bu = ret_n_pol_bu
		status, ret_n_tor_bu = ull.getDouble(self.idx, path, cpopath + 'n_tor_bu')
		check_status(status)
		if not status:
			self.n_tor_bu = ret_n_tor_bu
		status, ret_n_cp_bu = ull.getDouble(self.idx, path, cpopath + 'n_cp_bu')
		check_status(status)
		if not status:
			self.n_cp_bu = ret_n_cp_bu
		status, ret_cp_in_out = ull.getDouble(self.idx, path, cpopath + 'cp_in_out')
		check_status(status)
		if not status:
			self.cp_in_out = ret_cp_in_out
		status, ret_he_man_tck = ull.getDouble(self.idx, path, cpopath + 'he_man_tck')
		check_status(status)
		if not status:
			self.he_man_tck = ret_he_man_tck
		status, ret_man_tck = ull.getDouble(self.idx, path, cpopath + 'man_tck')
		check_status(status)
		if not status:
			self.man_tck = ret_man_tck
		status, ret_pbli_bptb_od = ull.getDouble(self.idx, path, cpopath + 'pbli_bptb_od')
		check_status(status)
		if not status:
			self.pbli_bptb_od = ret_pbli_bptb_od
		status, ret_pbli_bptb_id = ull.getDouble(self.idx, path, cpopath + 'pbli_bptb_id')
		check_status(status)
		if not status:
			self.pbli_bptb_id = ret_pbli_bptb_id
		status, ret_he_bptb_od = ull.getDouble(self.idx, path, cpopath + 'he_bptb_od')
		check_status(status)
		if not status:
			self.he_bptb_od = ret_he_bptb_od
		status, ret_he_bptb_id = ull.getDouble(self.idx, path, cpopath + 'he_bptb_id')
		check_status(status)
		if not status:
			self.he_bptb_id = ret_he_bptb_id
		status, ret_dr_max_fw = ull.getDouble(self.idx, path, cpopath + 'dr_max_fw')
		check_status(status)
		if not status:
			self.dr_max_fw = ret_dr_max_fw
		status, ret_dr_fwpl = ull.getDouble(self.idx, path, cpopath + 'dr_fwpl')
		check_status(status)
		if not status:
			self.dr_fwpl = ret_dr_fwpl

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mod_geomstructurebb_geometry, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, dr_fwVal = ull.getDouble(self.idx, path, cpopath + 'dr_fw')
			check_status(status)
			status, dr_bzVal = ull.getDouble(self.idx, path, cpopath + 'dr_bz')
			check_status(status)
			status, dr_bpVal = ull.getDouble(self.idx, path, cpopath + 'dr_bp')
			check_status(status)
			status, dr_bp_platesVal = ull.getVect1DDouble(self.idx, path, cpopath + 'dr_bp_plates')
			check_status(status)
			status, dr_bp_heVal = ull.getVect1DDouble(self.idx, path, cpopath + 'dr_bp_he')
			check_status(status)
			status, dr_manVal = ull.getDouble(self.idx, path, cpopath + 'dr_man')
			check_status(status)
			status, dt_swVal = ull.getDouble(self.idx, path, cpopath + 'dt_sw')
			check_status(status)
			status, dt_bzVal = ull.getDouble(self.idx, path, cpopath + 'dt_bz')
			check_status(status)
			status, dp_bzVal = ull.getDouble(self.idx, path, cpopath + 'dp_bz')
			check_status(status)
			top_cap_dimList = self.top_cap_dim.build_non_resampled_data(path, cpopath, nbslice)
			bot_cap_dimList = self.bot_cap_dim.build_non_resampled_data(path, cpopath, nbslice)
			status, a_fw_chVal = ull.getDouble(self.idx, path, cpopath + 'a_fw_ch')
			check_status(status)
			status, b_fw_chVal = ull.getDouble(self.idx, path, cpopath + 'b_fw_ch')
			check_status(status)
			status, td_tc_chVal = ull.getDouble(self.idx, path, cpopath + 'td_tc_ch')
			check_status(status)
			status, rd_tc_chVal = ull.getDouble(self.idx, path, cpopath + 'rd_tc_ch')
			check_status(status)
			status, td_bc_chVal = ull.getDouble(self.idx, path, cpopath + 'td_bc_ch')
			check_status(status)
			status, rd_bc_chVal = ull.getDouble(self.idx, path, cpopath + 'rd_bc_ch')
			check_status(status)
			status, n_fw_chVal = ull.getDouble(self.idx, path, cpopath + 'n_fw_ch')
			check_status(status)
			status, n_fw_circVal = ull.getDouble(self.idx, path, cpopath + 'n_fw_circ')
			check_status(status)
			status, a_sg_chVal = ull.getDouble(self.idx, path, cpopath + 'a_sg_ch')
			check_status(status)
			status, b_sg_chVal = ull.getDouble(self.idx, path, cpopath + 'b_sg_ch')
			check_status(status)
			status, n_sg_chVal = ull.getDouble(self.idx, path, cpopath + 'n_sg_ch')
			check_status(status)
			status, sg_thickVal = ull.getDouble(self.idx, path, cpopath + 'sg_thick')
			check_status(status)
			status, sg_weldVal = ull.getDouble(self.idx, path, cpopath + 'sg_weld')
			check_status(status)
			status, sg_in_outVal = ull.getDouble(self.idx, path, cpopath + 'sg_in_out')
			check_status(status)
			status, r_sg_cpVal = ull.getDouble(self.idx, path, cpopath + 'r_sg_cp')
			check_status(status)
			status, cp_tor_gapVal = ull.getDouble(self.idx, path, cpopath + 'cp_tor_gap')
			check_status(status)
			status, a_cp_chVal = ull.getDouble(self.idx, path, cpopath + 'a_cp_ch')
			check_status(status)
			status, b_cp_chVal = ull.getDouble(self.idx, path, cpopath + 'b_cp_ch')
			check_status(status)
			status, n_cp_chVal = ull.getDouble(self.idx, path, cpopath + 'n_cp_ch')
			check_status(status)
			status, cp_thickVal = ull.getDouble(self.idx, path, cpopath + 'cp_thick')
			check_status(status)
			status, n_pol_buVal = ull.getDouble(self.idx, path, cpopath + 'n_pol_bu')
			check_status(status)
			status, n_tor_buVal = ull.getDouble(self.idx, path, cpopath + 'n_tor_bu')
			check_status(status)
			status, n_cp_buVal = ull.getDouble(self.idx, path, cpopath + 'n_cp_bu')
			check_status(status)
			status, cp_in_outVal = ull.getDouble(self.idx, path, cpopath + 'cp_in_out')
			check_status(status)
			status, he_man_tckVal = ull.getDouble(self.idx, path, cpopath + 'he_man_tck')
			check_status(status)
			status, man_tckVal = ull.getDouble(self.idx, path, cpopath + 'man_tck')
			check_status(status)
			status, pbli_bptb_odVal = ull.getDouble(self.idx, path, cpopath + 'pbli_bptb_od')
			check_status(status)
			status, pbli_bptb_idVal = ull.getDouble(self.idx, path, cpopath + 'pbli_bptb_id')
			check_status(status)
			status, he_bptb_odVal = ull.getDouble(self.idx, path, cpopath + 'he_bptb_od')
			check_status(status)
			status, he_bptb_idVal = ull.getDouble(self.idx, path, cpopath + 'he_bptb_id')
			check_status(status)
			status, dr_max_fwVal = ull.getDouble(self.idx, path, cpopath + 'dr_max_fw')
			check_status(status)
			status, dr_fwplVal = ull.getDouble(self.idx, path, cpopath + 'dr_fwpl')
			check_status(status)
			for i in range(nbslice):
				slice = mod_geomstructurebb_geometry(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dr_fw = dr_fwVal
				slice.dr_bz = dr_bzVal
				slice.dr_bp = dr_bpVal
				slice.dr_bp_plates = dr_bp_platesVal
				slice.dr_bp_he = dr_bp_heVal
				slice.dr_man = dr_manVal
				slice.dt_sw = dt_swVal
				slice.dt_bz = dt_bzVal
				slice.dp_bz = dp_bzVal
				slice.top_cap_dim = top_cap_dimList[i]
				slice.bot_cap_dim = bot_cap_dimList[i]
				slice.a_fw_ch = a_fw_chVal
				slice.b_fw_ch = b_fw_chVal
				slice.td_tc_ch = td_tc_chVal
				slice.rd_tc_ch = rd_tc_chVal
				slice.td_bc_ch = td_bc_chVal
				slice.rd_bc_ch = rd_bc_chVal
				slice.n_fw_ch = n_fw_chVal
				slice.n_fw_circ = n_fw_circVal
				slice.a_sg_ch = a_sg_chVal
				slice.b_sg_ch = b_sg_chVal
				slice.n_sg_ch = n_sg_chVal
				slice.sg_thick = sg_thickVal
				slice.sg_weld = sg_weldVal
				slice.sg_in_out = sg_in_outVal
				slice.r_sg_cp = r_sg_cpVal
				slice.cp_tor_gap = cp_tor_gapVal
				slice.a_cp_ch = a_cp_chVal
				slice.b_cp_ch = b_cp_chVal
				slice.n_cp_ch = n_cp_chVal
				slice.cp_thick = cp_thickVal
				slice.n_pol_bu = n_pol_buVal
				slice.n_tor_bu = n_tor_buVal
				slice.n_cp_bu = n_cp_buVal
				slice.cp_in_out = cp_in_outVal
				slice.he_man_tck = he_man_tckVal
				slice.man_tck = man_tckVal
				slice.pbli_bptb_od = pbli_bptb_odVal
				slice.pbli_bptb_id = pbli_bptb_idVal
				slice.he_bptb_od = he_bptb_odVal
				slice.he_bptb_id = he_bptb_idVal
				slice.dr_max_fw = dr_max_fwVal
				slice.dr_fwpl = dr_fwplVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_geomstructurebb_geometryObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_geomstructurebb_geometryObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_geomstructurebb_geometryObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dr_fw') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dr_fw', i, self.dr_fw)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dr_bz') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dr_bz', i, self.dr_bz)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dr_bp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dr_bp', i, self.dr_bp)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dr_bp_plates') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dr_bp_plates', i, numpy.array(self.dr_bp_plates).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dr_bp_he') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dr_bp_he', i, numpy.array(self.dr_bp_he).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dr_man') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dr_man', i, self.dr_man)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dt_sw') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dt_sw', i, self.dt_sw)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dt_bz') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dt_bz', i, self.dt_bz)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dp_bz') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dp_bz', i, self.dp_bz)
		obj = self.top_cap_dim.putNonTimedElt(path, cpopath + 'top_cap_dim', i, obj)
		obj = self.bot_cap_dim.putNonTimedElt(path, cpopath + 'bot_cap_dim', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'a_fw_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'a_fw_ch', i, self.a_fw_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'b_fw_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'b_fw_ch', i, self.b_fw_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'td_tc_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'td_tc_ch', i, self.td_tc_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'rd_tc_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'rd_tc_ch', i, self.rd_tc_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'td_bc_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'td_bc_ch', i, self.td_bc_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'rd_bc_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'rd_bc_ch', i, self.rd_bc_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'n_fw_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'n_fw_ch', i, self.n_fw_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'n_fw_circ') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'n_fw_circ', i, self.n_fw_circ)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'a_sg_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'a_sg_ch', i, self.a_sg_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'b_sg_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'b_sg_ch', i, self.b_sg_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'n_sg_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'n_sg_ch', i, self.n_sg_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sg_thick') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sg_thick', i, self.sg_thick)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sg_weld') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sg_weld', i, self.sg_weld)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sg_in_out') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sg_in_out', i, self.sg_in_out)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r_sg_cp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r_sg_cp', i, self.r_sg_cp)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'cp_tor_gap') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'cp_tor_gap', i, self.cp_tor_gap)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'a_cp_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'a_cp_ch', i, self.a_cp_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'b_cp_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'b_cp_ch', i, self.b_cp_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'n_cp_ch') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'n_cp_ch', i, self.n_cp_ch)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'cp_thick') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'cp_thick', i, self.cp_thick)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'n_pol_bu') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'n_pol_bu', i, self.n_pol_bu)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'n_tor_bu') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'n_tor_bu', i, self.n_tor_bu)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'n_cp_bu') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'n_cp_bu', i, self.n_cp_bu)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'cp_in_out') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'cp_in_out', i, self.cp_in_out)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_man_tck') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_man_tck', i, self.he_man_tck)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'man_tck') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'man_tck', i, self.man_tck)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pbli_bptb_od') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pbli_bptb_od', i, self.pbli_bptb_od)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pbli_bptb_id') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pbli_bptb_id', i, self.pbli_bptb_id)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_bptb_od') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_bptb_od', i, self.he_bptb_od)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_bptb_id') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_bptb_id', i, self.he_bptb_id)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dr_max_fw') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dr_max_fw', i, self.dr_max_fw)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dr_fwpl') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dr_fwpl', i, self.dr_fwpl)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_geomstructurebb_geometryObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dr_fw') 
			print ('obj = ' + str(obj))
		status, ret_dr_fw = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dr_fw', i)
		check_status(status)
		if not status:
			self.dr_fw = ret_dr_fw
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dr_bz') 
			print ('obj = ' + str(obj))
		status, ret_dr_bz = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dr_bz', i)
		check_status(status)
		if not status:
			self.dr_bz = ret_dr_bz
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dr_bp') 
			print ('obj = ' + str(obj))
		status, ret_dr_bp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dr_bp', i)
		check_status(status)
		if not status:
			self.dr_bp = ret_dr_bp
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dr_bp_plates') 
			print ('obj = ' + str(obj))
		status, ret_dr_bp_plates = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dr_bp_plates', i)
		check_status(status)
		if not status:
			self.dr_bp_plates = ret_dr_bp_plates
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dr_bp_he') 
			print ('obj = ' + str(obj))
		status, ret_dr_bp_he = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dr_bp_he', i)
		check_status(status)
		if not status:
			self.dr_bp_he = ret_dr_bp_he
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dr_man') 
			print ('obj = ' + str(obj))
		status, ret_dr_man = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dr_man', i)
		check_status(status)
		if not status:
			self.dr_man = ret_dr_man
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dt_sw') 
			print ('obj = ' + str(obj))
		status, ret_dt_sw = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dt_sw', i)
		check_status(status)
		if not status:
			self.dt_sw = ret_dt_sw
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dt_bz') 
			print ('obj = ' + str(obj))
		status, ret_dt_bz = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dt_bz', i)
		check_status(status)
		if not status:
			self.dt_bz = ret_dt_bz
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dp_bz') 
			print ('obj = ' + str(obj))
		status, ret_dp_bz = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dp_bz', i)
		check_status(status)
		if not status:
			self.dp_bz = ret_dp_bz
		self.top_cap_dim.getNonTimedElt(path, cpopath + 'top_cap_dim', i, obj)
		self.bot_cap_dim.getNonTimedElt(path, cpopath + 'bot_cap_dim', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'a_fw_ch') 
			print ('obj = ' + str(obj))
		status, ret_a_fw_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'a_fw_ch', i)
		check_status(status)
		if not status:
			self.a_fw_ch = ret_a_fw_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'b_fw_ch') 
			print ('obj = ' + str(obj))
		status, ret_b_fw_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'b_fw_ch', i)
		check_status(status)
		if not status:
			self.b_fw_ch = ret_b_fw_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'td_tc_ch') 
			print ('obj = ' + str(obj))
		status, ret_td_tc_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'td_tc_ch', i)
		check_status(status)
		if not status:
			self.td_tc_ch = ret_td_tc_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'rd_tc_ch') 
			print ('obj = ' + str(obj))
		status, ret_rd_tc_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'rd_tc_ch', i)
		check_status(status)
		if not status:
			self.rd_tc_ch = ret_rd_tc_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'td_bc_ch') 
			print ('obj = ' + str(obj))
		status, ret_td_bc_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'td_bc_ch', i)
		check_status(status)
		if not status:
			self.td_bc_ch = ret_td_bc_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'rd_bc_ch') 
			print ('obj = ' + str(obj))
		status, ret_rd_bc_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'rd_bc_ch', i)
		check_status(status)
		if not status:
			self.rd_bc_ch = ret_rd_bc_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'n_fw_ch') 
			print ('obj = ' + str(obj))
		status, ret_n_fw_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'n_fw_ch', i)
		check_status(status)
		if not status:
			self.n_fw_ch = ret_n_fw_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'n_fw_circ') 
			print ('obj = ' + str(obj))
		status, ret_n_fw_circ = ull.getDoubleFromObject(self.idx, obj, cpopath + 'n_fw_circ', i)
		check_status(status)
		if not status:
			self.n_fw_circ = ret_n_fw_circ
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'a_sg_ch') 
			print ('obj = ' + str(obj))
		status, ret_a_sg_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'a_sg_ch', i)
		check_status(status)
		if not status:
			self.a_sg_ch = ret_a_sg_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'b_sg_ch') 
			print ('obj = ' + str(obj))
		status, ret_b_sg_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'b_sg_ch', i)
		check_status(status)
		if not status:
			self.b_sg_ch = ret_b_sg_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'n_sg_ch') 
			print ('obj = ' + str(obj))
		status, ret_n_sg_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'n_sg_ch', i)
		check_status(status)
		if not status:
			self.n_sg_ch = ret_n_sg_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sg_thick') 
			print ('obj = ' + str(obj))
		status, ret_sg_thick = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sg_thick', i)
		check_status(status)
		if not status:
			self.sg_thick = ret_sg_thick
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sg_weld') 
			print ('obj = ' + str(obj))
		status, ret_sg_weld = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sg_weld', i)
		check_status(status)
		if not status:
			self.sg_weld = ret_sg_weld
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sg_in_out') 
			print ('obj = ' + str(obj))
		status, ret_sg_in_out = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sg_in_out', i)
		check_status(status)
		if not status:
			self.sg_in_out = ret_sg_in_out
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r_sg_cp') 
			print ('obj = ' + str(obj))
		status, ret_r_sg_cp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r_sg_cp', i)
		check_status(status)
		if not status:
			self.r_sg_cp = ret_r_sg_cp
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'cp_tor_gap') 
			print ('obj = ' + str(obj))
		status, ret_cp_tor_gap = ull.getDoubleFromObject(self.idx, obj, cpopath + 'cp_tor_gap', i)
		check_status(status)
		if not status:
			self.cp_tor_gap = ret_cp_tor_gap
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'a_cp_ch') 
			print ('obj = ' + str(obj))
		status, ret_a_cp_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'a_cp_ch', i)
		check_status(status)
		if not status:
			self.a_cp_ch = ret_a_cp_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'b_cp_ch') 
			print ('obj = ' + str(obj))
		status, ret_b_cp_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'b_cp_ch', i)
		check_status(status)
		if not status:
			self.b_cp_ch = ret_b_cp_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'n_cp_ch') 
			print ('obj = ' + str(obj))
		status, ret_n_cp_ch = ull.getDoubleFromObject(self.idx, obj, cpopath + 'n_cp_ch', i)
		check_status(status)
		if not status:
			self.n_cp_ch = ret_n_cp_ch
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'cp_thick') 
			print ('obj = ' + str(obj))
		status, ret_cp_thick = ull.getDoubleFromObject(self.idx, obj, cpopath + 'cp_thick', i)
		check_status(status)
		if not status:
			self.cp_thick = ret_cp_thick
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'n_pol_bu') 
			print ('obj = ' + str(obj))
		status, ret_n_pol_bu = ull.getDoubleFromObject(self.idx, obj, cpopath + 'n_pol_bu', i)
		check_status(status)
		if not status:
			self.n_pol_bu = ret_n_pol_bu
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'n_tor_bu') 
			print ('obj = ' + str(obj))
		status, ret_n_tor_bu = ull.getDoubleFromObject(self.idx, obj, cpopath + 'n_tor_bu', i)
		check_status(status)
		if not status:
			self.n_tor_bu = ret_n_tor_bu
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'n_cp_bu') 
			print ('obj = ' + str(obj))
		status, ret_n_cp_bu = ull.getDoubleFromObject(self.idx, obj, cpopath + 'n_cp_bu', i)
		check_status(status)
		if not status:
			self.n_cp_bu = ret_n_cp_bu
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'cp_in_out') 
			print ('obj = ' + str(obj))
		status, ret_cp_in_out = ull.getDoubleFromObject(self.idx, obj, cpopath + 'cp_in_out', i)
		check_status(status)
		if not status:
			self.cp_in_out = ret_cp_in_out
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_man_tck') 
			print ('obj = ' + str(obj))
		status, ret_he_man_tck = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_man_tck', i)
		check_status(status)
		if not status:
			self.he_man_tck = ret_he_man_tck
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'man_tck') 
			print ('obj = ' + str(obj))
		status, ret_man_tck = ull.getDoubleFromObject(self.idx, obj, cpopath + 'man_tck', i)
		check_status(status)
		if not status:
			self.man_tck = ret_man_tck
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pbli_bptb_od') 
			print ('obj = ' + str(obj))
		status, ret_pbli_bptb_od = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pbli_bptb_od', i)
		check_status(status)
		if not status:
			self.pbli_bptb_od = ret_pbli_bptb_od
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pbli_bptb_id') 
			print ('obj = ' + str(obj))
		status, ret_pbli_bptb_id = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pbli_bptb_id', i)
		check_status(status)
		if not status:
			self.pbli_bptb_id = ret_pbli_bptb_id
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_bptb_od') 
			print ('obj = ' + str(obj))
		status, ret_he_bptb_od = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_bptb_od', i)
		check_status(status)
		if not status:
			self.he_bptb_od = ret_he_bptb_od
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_bptb_id') 
			print ('obj = ' + str(obj))
		status, ret_he_bptb_id = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_bptb_id', i)
		check_status(status)
		if not status:
			self.he_bptb_id = ret_he_bptb_id
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dr_max_fw') 
			print ('obj = ' + str(obj))
		status, ret_dr_max_fw = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dr_max_fw', i)
		check_status(status)
		if not status:
			self.dr_max_fw = ret_dr_max_fw
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dr_fwpl') 
			print ('obj = ' + str(obj))
		status, ret_dr_fwpl = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dr_fwpl', i)
		check_status(status)
		if not status:
			self.dr_fwpl = ret_dr_fwpl

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'dr_fw')
		ull.deleteData(self.idx, path, cpopath + 'dr_bz')
		ull.deleteData(self.idx, path, cpopath + 'dr_bp')
		ull.deleteData(self.idx, path, cpopath + 'dr_bp_plates')
		ull.deleteData(self.idx, path, cpopath + 'dr_bp_he')
		ull.deleteData(self.idx, path, cpopath + 'dr_man')
		ull.deleteData(self.idx, path, cpopath + 'dt_sw')
		ull.deleteData(self.idx, path, cpopath + 'dt_bz')
		ull.deleteData(self.idx, path, cpopath + 'dp_bz')
		self.top_cap_dim.deleteData(path, cpopath)
		self.bot_cap_dim.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'a_fw_ch')
		ull.deleteData(self.idx, path, cpopath + 'b_fw_ch')
		ull.deleteData(self.idx, path, cpopath + 'td_tc_ch')
		ull.deleteData(self.idx, path, cpopath + 'rd_tc_ch')
		ull.deleteData(self.idx, path, cpopath + 'td_bc_ch')
		ull.deleteData(self.idx, path, cpopath + 'rd_bc_ch')
		ull.deleteData(self.idx, path, cpopath + 'n_fw_ch')
		ull.deleteData(self.idx, path, cpopath + 'n_fw_circ')
		ull.deleteData(self.idx, path, cpopath + 'a_sg_ch')
		ull.deleteData(self.idx, path, cpopath + 'b_sg_ch')
		ull.deleteData(self.idx, path, cpopath + 'n_sg_ch')
		ull.deleteData(self.idx, path, cpopath + 'sg_thick')
		ull.deleteData(self.idx, path, cpopath + 'sg_weld')
		ull.deleteData(self.idx, path, cpopath + 'sg_in_out')
		ull.deleteData(self.idx, path, cpopath + 'r_sg_cp')
		ull.deleteData(self.idx, path, cpopath + 'cp_tor_gap')
		ull.deleteData(self.idx, path, cpopath + 'a_cp_ch')
		ull.deleteData(self.idx, path, cpopath + 'b_cp_ch')
		ull.deleteData(self.idx, path, cpopath + 'n_cp_ch')
		ull.deleteData(self.idx, path, cpopath + 'cp_thick')
		ull.deleteData(self.idx, path, cpopath + 'n_pol_bu')
		ull.deleteData(self.idx, path, cpopath + 'n_tor_bu')
		ull.deleteData(self.idx, path, cpopath + 'n_cp_bu')
		ull.deleteData(self.idx, path, cpopath + 'cp_in_out')
		ull.deleteData(self.idx, path, cpopath + 'he_man_tck')
		ull.deleteData(self.idx, path, cpopath + 'man_tck')
		ull.deleteData(self.idx, path, cpopath + 'pbli_bptb_od')
		ull.deleteData(self.idx, path, cpopath + 'pbli_bptb_id')
		ull.deleteData(self.idx, path, cpopath + 'he_bptb_od')
		ull.deleteData(self.idx, path, cpopath + 'he_bptb_id')
		ull.deleteData(self.idx, path, cpopath + 'dr_max_fw')
		ull.deleteData(self.idx, path, cpopath + 'dr_fwpl')


class top_cap_dimstructurebb_dimension:
	'''
	class top_cap_dimstructurebb_dimension
	Top cap dimension of bb modules

	Attributes:
	- radial : numpy.ndarray 1D with float
	   Radial dimension [m]. Vector(nmodules) 
	- toroidal : numpy.ndarray 1D with float
	   Toroidal dimension [m]. Vector(nmodules) 
	- poloidal : numpy.ndarray 1D with float
	   Poloidal dimension [m]. Vector(nmodules) 
	'''

	def __init__(self, base_path_in='top_cap_dim'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.radial = numpy.zeros(0, numpy.float64, order='C')
		self.toroidal = numpy.zeros(0, numpy.float64, order='C')
		self.poloidal = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class top_cap_dimstructurebb_dimension\n'
		s = self.radial.__str__()
		ret = ret + space + 'Attribute radial\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.toroidal.__str__()
		ret = ret + space + 'Attribute toroidal\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.poloidal.__str__()
		ret = ret + space + 'Attribute poloidal\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type top_cap_dimstructurebb_dimension, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type top_cap_dimstructurebb_dimension, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type top_cap_dimstructurebb_dimension, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'radial', numpy.array(self.radial).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'toroidal', numpy.array(self.toroidal).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'poloidal', numpy.array(self.poloidal).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type top_cap_dimstructurebb_dimension, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_radial = ull.getVect1DDouble(self.idx, path, cpopath + 'radial')
		check_status(status)
		if not status:
			self.radial = ret_radial
		status, ret_toroidal = ull.getVect1DDouble(self.idx, path, cpopath + 'toroidal')
		check_status(status)
		if not status:
			self.toroidal = ret_toroidal
		status, ret_poloidal = ull.getVect1DDouble(self.idx, path, cpopath + 'poloidal')
		check_status(status)
		if not status:
			self.poloidal = ret_poloidal

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type top_cap_dimstructurebb_dimension, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, radialVal = ull.getVect1DDouble(self.idx, path, cpopath + 'radial')
			check_status(status)
			status, toroidalVal = ull.getVect1DDouble(self.idx, path, cpopath + 'toroidal')
			check_status(status)
			status, poloidalVal = ull.getVect1DDouble(self.idx, path, cpopath + 'poloidal')
			check_status(status)
			for i in range(nbslice):
				slice = top_cap_dimstructurebb_dimension(self.base_path)
				slice.setExpIdx(self.idx)
				slice.radial = radialVal
				slice.toroidal = toroidalVal
				slice.poloidal = poloidalVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type top_cap_dimstructurebb_dimensionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type top_cap_dimstructurebb_dimensionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type top_cap_dimstructurebb_dimensionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'radial') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'radial', i, numpy.array(self.radial).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'toroidal') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'toroidal', i, numpy.array(self.toroidal).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'poloidal') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'poloidal', i, numpy.array(self.poloidal).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type top_cap_dimstructurebb_dimensionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'radial') 
			print ('obj = ' + str(obj))
		status, ret_radial = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'radial', i)
		check_status(status)
		if not status:
			self.radial = ret_radial
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'toroidal') 
			print ('obj = ' + str(obj))
		status, ret_toroidal = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'toroidal', i)
		check_status(status)
		if not status:
			self.toroidal = ret_toroidal
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'poloidal') 
			print ('obj = ' + str(obj))
		status, ret_poloidal = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'poloidal', i)
		check_status(status)
		if not status:
			self.poloidal = ret_poloidal

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'radial')
		ull.deleteData(self.idx, path, cpopath + 'toroidal')
		ull.deleteData(self.idx, path, cpopath + 'poloidal')


class bot_cap_dimstructurebb_dimension:
	'''
	class bot_cap_dimstructurebb_dimension
	Bottom cap dimension of bb modules

	Attributes:
	- radial : numpy.ndarray 1D with float
	   Radial dimension [m]. Vector(nmodules) 
	- toroidal : numpy.ndarray 1D with float
	   Toroidal dimension [m]. Vector(nmodules) 
	- poloidal : numpy.ndarray 1D with float
	   Poloidal dimension [m]. Vector(nmodules) 
	'''

	def __init__(self, base_path_in='bot_cap_dim'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.radial = numpy.zeros(0, numpy.float64, order='C')
		self.toroidal = numpy.zeros(0, numpy.float64, order='C')
		self.poloidal = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class bot_cap_dimstructurebb_dimension\n'
		s = self.radial.__str__()
		ret = ret + space + 'Attribute radial\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.toroidal.__str__()
		ret = ret + space + 'Attribute toroidal\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.poloidal.__str__()
		ret = ret + space + 'Attribute poloidal\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bot_cap_dimstructurebb_dimension, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bot_cap_dimstructurebb_dimension, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bot_cap_dimstructurebb_dimension, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'radial', numpy.array(self.radial).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'toroidal', numpy.array(self.toroidal).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'poloidal', numpy.array(self.poloidal).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type bot_cap_dimstructurebb_dimension, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_radial = ull.getVect1DDouble(self.idx, path, cpopath + 'radial')
		check_status(status)
		if not status:
			self.radial = ret_radial
		status, ret_toroidal = ull.getVect1DDouble(self.idx, path, cpopath + 'toroidal')
		check_status(status)
		if not status:
			self.toroidal = ret_toroidal
		status, ret_poloidal = ull.getVect1DDouble(self.idx, path, cpopath + 'poloidal')
		check_status(status)
		if not status:
			self.poloidal = ret_poloidal

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type bot_cap_dimstructurebb_dimension, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, radialVal = ull.getVect1DDouble(self.idx, path, cpopath + 'radial')
			check_status(status)
			status, toroidalVal = ull.getVect1DDouble(self.idx, path, cpopath + 'toroidal')
			check_status(status)
			status, poloidalVal = ull.getVect1DDouble(self.idx, path, cpopath + 'poloidal')
			check_status(status)
			for i in range(nbslice):
				slice = bot_cap_dimstructurebb_dimension(self.base_path)
				slice.setExpIdx(self.idx)
				slice.radial = radialVal
				slice.toroidal = toroidalVal
				slice.poloidal = poloidalVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bot_cap_dimstructurebb_dimensionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bot_cap_dimstructurebb_dimensionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bot_cap_dimstructurebb_dimensionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'radial') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'radial', i, numpy.array(self.radial).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'toroidal') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'toroidal', i, numpy.array(self.toroidal).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'poloidal') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'poloidal', i, numpy.array(self.poloidal).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bot_cap_dimstructurebb_dimensionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'radial') 
			print ('obj = ' + str(obj))
		status, ret_radial = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'radial', i)
		check_status(status)
		if not status:
			self.radial = ret_radial
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'toroidal') 
			print ('obj = ' + str(obj))
		status, ret_toroidal = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'toroidal', i)
		check_status(status)
		if not status:
			self.toroidal = ret_toroidal
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'poloidal') 
			print ('obj = ' + str(obj))
		status, ret_poloidal = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'poloidal', i)
		check_status(status)
		if not status:
			self.poloidal = ret_poloidal

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'radial')
		ull.deleteData(self.idx, path, cpopath + 'toroidal')
		ull.deleteData(self.idx, path, cpopath + 'poloidal')


class mod_neutrstructuremode_neutr:
	'''
	class mod_neutrstructuremode_neutr
	Neutrons "effects"

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius position at wich power density is calculated [m]; Vector(nr)
	- pd_rad : numpy.ndarray 1D with float
	   Power density distribution in radial direction [W/m^3]; Vector(nr)
	- lipb_coef_pd : numpy.ndarray 1D with float
	   Pb-15.7Li power density distribution in radial direction: coefficients of bi-exponential law if this one is used [W/m^-3,W/m^-3,m^-1,m^-1]; Matrix
	- steel_coef_pd : numpy.ndarray 1D with float
	   Eurofer power density distribution in radial direction: coefficients of bi-exponential law if this one is used
	- pow_exchange : class pow_exchangestructurepower_exchange
	   
	'''

	def __init__(self, base_path_in='mod_neutr'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.pd_rad = numpy.zeros(0, numpy.float64, order='C')
		self.lipb_coef_pd = numpy.zeros(0, numpy.float64, order='C')
		self.steel_coef_pd = numpy.zeros(0, numpy.float64, order='C')
		self.pow_exchange = pow_exchangestructurepower_exchange('pow_exchange')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mod_neutrstructuremode_neutr\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pd_rad.__str__()
		ret = ret + space + 'Attribute pd_rad\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.lipb_coef_pd.__str__()
		ret = ret + space + 'Attribute lipb_coef_pd\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.steel_coef_pd.__str__()
		ret = ret + space + 'Attribute steel_coef_pd\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute pow_exchange\n ' + self.pow_exchange.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.pow_exchange.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_neutrstructuremode_neutr, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.pow_exchange.cpoTime = self.cpoTime
		self.pow_exchange.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_neutrstructuremode_neutr, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.pow_exchange.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_neutrstructuremode_neutr, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'pd_rad', numpy.array(self.pd_rad).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'lipb_coef_pd', numpy.array(self.lipb_coef_pd).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'steel_coef_pd', numpy.array(self.steel_coef_pd).astype(numpy.float64), False)
		check_status(status)
		self.pow_exchange.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mod_neutrstructuremode_neutr, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r = ull.getVect1DDouble(self.idx, path, cpopath + 'r')
		check_status(status)
		if not status:
			self.r = ret_r
		status, ret_pd_rad = ull.getVect1DDouble(self.idx, path, cpopath + 'pd_rad')
		check_status(status)
		if not status:
			self.pd_rad = ret_pd_rad
		status, ret_lipb_coef_pd = ull.getVect1DDouble(self.idx, path, cpopath + 'lipb_coef_pd')
		check_status(status)
		if not status:
			self.lipb_coef_pd = ret_lipb_coef_pd
		status, ret_steel_coef_pd = ull.getVect1DDouble(self.idx, path, cpopath + 'steel_coef_pd')
		check_status(status)
		if not status:
			self.steel_coef_pd = ret_steel_coef_pd
		self.pow_exchange.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mod_neutrstructuremode_neutr, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rVal = ull.getVect1DDouble(self.idx, path, cpopath + 'r')
			check_status(status)
			status, pd_radVal = ull.getVect1DDouble(self.idx, path, cpopath + 'pd_rad')
			check_status(status)
			status, lipb_coef_pdVal = ull.getVect1DDouble(self.idx, path, cpopath + 'lipb_coef_pd')
			check_status(status)
			status, steel_coef_pdVal = ull.getVect1DDouble(self.idx, path, cpopath + 'steel_coef_pd')
			check_status(status)
			pow_exchangeList = self.pow_exchange.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = mod_neutrstructuremode_neutr(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.pd_rad = pd_radVal
				slice.lipb_coef_pd = lipb_coef_pdVal
				slice.steel_coef_pd = steel_coef_pdVal
				slice.pow_exchange = pow_exchangeList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_neutrstructuremode_neutrObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_neutrstructuremode_neutrObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_neutrstructuremode_neutrObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pd_rad') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pd_rad', i, numpy.array(self.pd_rad).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'lipb_coef_pd') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'lipb_coef_pd', i, numpy.array(self.lipb_coef_pd).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'steel_coef_pd') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'steel_coef_pd', i, numpy.array(self.steel_coef_pd).astype(numpy.float64))
		obj = self.pow_exchange.putNonTimedElt(path, cpopath + 'pow_exchange', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_neutrstructuremode_neutrObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		status, ret_r = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'r', i)
		check_status(status)
		if not status:
			self.r = ret_r
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pd_rad') 
			print ('obj = ' + str(obj))
		status, ret_pd_rad = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pd_rad', i)
		check_status(status)
		if not status:
			self.pd_rad = ret_pd_rad
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'lipb_coef_pd') 
			print ('obj = ' + str(obj))
		status, ret_lipb_coef_pd = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'lipb_coef_pd', i)
		check_status(status)
		if not status:
			self.lipb_coef_pd = ret_lipb_coef_pd
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'steel_coef_pd') 
			print ('obj = ' + str(obj))
		status, ret_steel_coef_pd = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'steel_coef_pd', i)
		check_status(status)
		if not status:
			self.steel_coef_pd = ret_steel_coef_pd
		self.pow_exchange.getNonTimedElt(path, cpopath + 'pow_exchange', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'pd_rad')
		ull.deleteData(self.idx, path, cpopath + 'lipb_coef_pd')
		ull.deleteData(self.idx, path, cpopath + 'steel_coef_pd')
		self.pow_exchange.deleteData(path, cpopath)


class pow_exchangestructurepower_exchange:
	'''
	class pow_exchangestructurepower_exchange
	

	Attributes:
	- dep_pow : numpy.ndarray 1D with float
	   Power deposited in each bb module (the reference outboard module if only value is given) [W]; Vector(nmodules)
	- dep_fw : float
	   Power deposited in the first wall (heat flux + neutrons) [W]; Scalar
	- dep_sg : float
	   Power deposited in the stiffening grid (neutrons) [W]; Scalar
	- dep_cp : float
	   Power deposited in the cooling plates (neutrons) [W]; Scalar
	- dep_lp : float
	   Power deposited in the Pb-15.7Li (neutrons) [W]; Scalar
	- dep_man : float
	   Power deposited in the manifolds (neutrons) [W]; Scalar
	- dep_pl : float
	   Power deposited in the protect layer (made of tungsten) (neutrons) [W]; Scalar
	- rec_fw : float
	   Power recovered from He in first wall channels [W]; Scalar
	- rec_sg : float
	   Power recovered from He in stiffening grid channels [W]; Scalar
	- rec_cp : float
	   Power recovered from He in cooling plates channels [W]; Scalar
	- pow_dens_fw : float
	   Peak energy depostion in first wall [W.m^-3]; Scalar
	- pow_dens_bz : float
	   Peak energy depostion in breeding zone [W.m^-3]; Scalar
	- pow_dens_bz10 : float
	   Peak energy depostion in breeding zone (first ten centimers) [W.m^-3]; Scalar
	- pow_dens_bp : float
	   Peak energy depostion in back plate [W.m^-3]; Scalar
	- pow_dens_man : float
	   Peak energy depostion in manifold [W.m^-3]; Scalar
	- pow_dens_sh : float
	   Peak energy depostion in shield [W.m^-3]; Scalar
	'''

	def __init__(self, base_path_in='pow_exchange'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dep_pow = numpy.zeros(0, numpy.float64, order='C')
		self.dep_fw = EMPTY_DOUBLE
		self.dep_sg = EMPTY_DOUBLE
		self.dep_cp = EMPTY_DOUBLE
		self.dep_lp = EMPTY_DOUBLE
		self.dep_man = EMPTY_DOUBLE
		self.dep_pl = EMPTY_DOUBLE
		self.rec_fw = EMPTY_DOUBLE
		self.rec_sg = EMPTY_DOUBLE
		self.rec_cp = EMPTY_DOUBLE
		self.pow_dens_fw = EMPTY_DOUBLE
		self.pow_dens_bz = EMPTY_DOUBLE
		self.pow_dens_bz10 = EMPTY_DOUBLE
		self.pow_dens_bp = EMPTY_DOUBLE
		self.pow_dens_man = EMPTY_DOUBLE
		self.pow_dens_sh = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pow_exchangestructurepower_exchange\n'
		s = self.dep_pow.__str__()
		ret = ret + space + 'Attribute dep_pow\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute dep_fw: ' + str(self.dep_fw) + '\n'
		ret = ret + space + 'Attribute dep_sg: ' + str(self.dep_sg) + '\n'
		ret = ret + space + 'Attribute dep_cp: ' + str(self.dep_cp) + '\n'
		ret = ret + space + 'Attribute dep_lp: ' + str(self.dep_lp) + '\n'
		ret = ret + space + 'Attribute dep_man: ' + str(self.dep_man) + '\n'
		ret = ret + space + 'Attribute dep_pl: ' + str(self.dep_pl) + '\n'
		ret = ret + space + 'Attribute rec_fw: ' + str(self.rec_fw) + '\n'
		ret = ret + space + 'Attribute rec_sg: ' + str(self.rec_sg) + '\n'
		ret = ret + space + 'Attribute rec_cp: ' + str(self.rec_cp) + '\n'
		ret = ret + space + 'Attribute pow_dens_fw: ' + str(self.pow_dens_fw) + '\n'
		ret = ret + space + 'Attribute pow_dens_bz: ' + str(self.pow_dens_bz) + '\n'
		ret = ret + space + 'Attribute pow_dens_bz10: ' + str(self.pow_dens_bz10) + '\n'
		ret = ret + space + 'Attribute pow_dens_bp: ' + str(self.pow_dens_bp) + '\n'
		ret = ret + space + 'Attribute pow_dens_man: ' + str(self.pow_dens_man) + '\n'
		ret = ret + space + 'Attribute pow_dens_sh: ' + str(self.pow_dens_sh) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pow_exchangestructurepower_exchange, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pow_exchangestructurepower_exchange, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pow_exchangestructurepower_exchange, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'dep_pow', numpy.array(self.dep_pow).astype(numpy.float64), False)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dep_fw', self.dep_fw)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dep_sg', self.dep_sg)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dep_cp', self.dep_cp)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dep_lp', self.dep_lp)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dep_man', self.dep_man)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'dep_pl', self.dep_pl)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'rec_fw', self.rec_fw)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'rec_sg', self.rec_sg)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'rec_cp', self.rec_cp)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'pow_dens_fw', self.pow_dens_fw)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'pow_dens_bz', self.pow_dens_bz)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'pow_dens_bz10', self.pow_dens_bz10)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'pow_dens_bp', self.pow_dens_bp)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'pow_dens_man', self.pow_dens_man)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'pow_dens_sh', self.pow_dens_sh)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pow_exchangestructurepower_exchange, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dep_pow = ull.getVect1DDouble(self.idx, path, cpopath + 'dep_pow')
		check_status(status)
		if not status:
			self.dep_pow = ret_dep_pow
		status, ret_dep_fw = ull.getDouble(self.idx, path, cpopath + 'dep_fw')
		check_status(status)
		if not status:
			self.dep_fw = ret_dep_fw
		status, ret_dep_sg = ull.getDouble(self.idx, path, cpopath + 'dep_sg')
		check_status(status)
		if not status:
			self.dep_sg = ret_dep_sg
		status, ret_dep_cp = ull.getDouble(self.idx, path, cpopath + 'dep_cp')
		check_status(status)
		if not status:
			self.dep_cp = ret_dep_cp
		status, ret_dep_lp = ull.getDouble(self.idx, path, cpopath + 'dep_lp')
		check_status(status)
		if not status:
			self.dep_lp = ret_dep_lp
		status, ret_dep_man = ull.getDouble(self.idx, path, cpopath + 'dep_man')
		check_status(status)
		if not status:
			self.dep_man = ret_dep_man
		status, ret_dep_pl = ull.getDouble(self.idx, path, cpopath + 'dep_pl')
		check_status(status)
		if not status:
			self.dep_pl = ret_dep_pl
		status, ret_rec_fw = ull.getDouble(self.idx, path, cpopath + 'rec_fw')
		check_status(status)
		if not status:
			self.rec_fw = ret_rec_fw
		status, ret_rec_sg = ull.getDouble(self.idx, path, cpopath + 'rec_sg')
		check_status(status)
		if not status:
			self.rec_sg = ret_rec_sg
		status, ret_rec_cp = ull.getDouble(self.idx, path, cpopath + 'rec_cp')
		check_status(status)
		if not status:
			self.rec_cp = ret_rec_cp
		status, ret_pow_dens_fw = ull.getDouble(self.idx, path, cpopath + 'pow_dens_fw')
		check_status(status)
		if not status:
			self.pow_dens_fw = ret_pow_dens_fw
		status, ret_pow_dens_bz = ull.getDouble(self.idx, path, cpopath + 'pow_dens_bz')
		check_status(status)
		if not status:
			self.pow_dens_bz = ret_pow_dens_bz
		status, ret_pow_dens_bz10 = ull.getDouble(self.idx, path, cpopath + 'pow_dens_bz10')
		check_status(status)
		if not status:
			self.pow_dens_bz10 = ret_pow_dens_bz10
		status, ret_pow_dens_bp = ull.getDouble(self.idx, path, cpopath + 'pow_dens_bp')
		check_status(status)
		if not status:
			self.pow_dens_bp = ret_pow_dens_bp
		status, ret_pow_dens_man = ull.getDouble(self.idx, path, cpopath + 'pow_dens_man')
		check_status(status)
		if not status:
			self.pow_dens_man = ret_pow_dens_man
		status, ret_pow_dens_sh = ull.getDouble(self.idx, path, cpopath + 'pow_dens_sh')
		check_status(status)
		if not status:
			self.pow_dens_sh = ret_pow_dens_sh

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pow_exchangestructurepower_exchange, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, dep_powVal = ull.getVect1DDouble(self.idx, path, cpopath + 'dep_pow')
			check_status(status)
			status, dep_fwVal = ull.getDouble(self.idx, path, cpopath + 'dep_fw')
			check_status(status)
			status, dep_sgVal = ull.getDouble(self.idx, path, cpopath + 'dep_sg')
			check_status(status)
			status, dep_cpVal = ull.getDouble(self.idx, path, cpopath + 'dep_cp')
			check_status(status)
			status, dep_lpVal = ull.getDouble(self.idx, path, cpopath + 'dep_lp')
			check_status(status)
			status, dep_manVal = ull.getDouble(self.idx, path, cpopath + 'dep_man')
			check_status(status)
			status, dep_plVal = ull.getDouble(self.idx, path, cpopath + 'dep_pl')
			check_status(status)
			status, rec_fwVal = ull.getDouble(self.idx, path, cpopath + 'rec_fw')
			check_status(status)
			status, rec_sgVal = ull.getDouble(self.idx, path, cpopath + 'rec_sg')
			check_status(status)
			status, rec_cpVal = ull.getDouble(self.idx, path, cpopath + 'rec_cp')
			check_status(status)
			status, pow_dens_fwVal = ull.getDouble(self.idx, path, cpopath + 'pow_dens_fw')
			check_status(status)
			status, pow_dens_bzVal = ull.getDouble(self.idx, path, cpopath + 'pow_dens_bz')
			check_status(status)
			status, pow_dens_bz10Val = ull.getDouble(self.idx, path, cpopath + 'pow_dens_bz10')
			check_status(status)
			status, pow_dens_bpVal = ull.getDouble(self.idx, path, cpopath + 'pow_dens_bp')
			check_status(status)
			status, pow_dens_manVal = ull.getDouble(self.idx, path, cpopath + 'pow_dens_man')
			check_status(status)
			status, pow_dens_shVal = ull.getDouble(self.idx, path, cpopath + 'pow_dens_sh')
			check_status(status)
			for i in range(nbslice):
				slice = pow_exchangestructurepower_exchange(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dep_pow = dep_powVal
				slice.dep_fw = dep_fwVal
				slice.dep_sg = dep_sgVal
				slice.dep_cp = dep_cpVal
				slice.dep_lp = dep_lpVal
				slice.dep_man = dep_manVal
				slice.dep_pl = dep_plVal
				slice.rec_fw = rec_fwVal
				slice.rec_sg = rec_sgVal
				slice.rec_cp = rec_cpVal
				slice.pow_dens_fw = pow_dens_fwVal
				slice.pow_dens_bz = pow_dens_bzVal
				slice.pow_dens_bz10 = pow_dens_bz10Val
				slice.pow_dens_bp = pow_dens_bpVal
				slice.pow_dens_man = pow_dens_manVal
				slice.pow_dens_sh = pow_dens_shVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_exchangestructurepower_exchangeObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_exchangestructurepower_exchangeObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_exchangestructurepower_exchangeObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dep_pow') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dep_pow', i, numpy.array(self.dep_pow).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dep_fw') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dep_fw', i, self.dep_fw)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dep_sg') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dep_sg', i, self.dep_sg)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dep_cp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dep_cp', i, self.dep_cp)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dep_lp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dep_lp', i, self.dep_lp)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dep_man') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dep_man', i, self.dep_man)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dep_pl') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dep_pl', i, self.dep_pl)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'rec_fw') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'rec_fw', i, self.rec_fw)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'rec_sg') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'rec_sg', i, self.rec_sg)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'rec_cp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'rec_cp', i, self.rec_cp)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pow_dens_fw') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pow_dens_fw', i, self.pow_dens_fw)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pow_dens_bz') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pow_dens_bz', i, self.pow_dens_bz)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pow_dens_bz10') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pow_dens_bz10', i, self.pow_dens_bz10)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pow_dens_bp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pow_dens_bp', i, self.pow_dens_bp)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pow_dens_man') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pow_dens_man', i, self.pow_dens_man)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pow_dens_sh') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pow_dens_sh', i, self.pow_dens_sh)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_exchangestructurepower_exchangeObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dep_pow') 
			print ('obj = ' + str(obj))
		status, ret_dep_pow = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dep_pow', i)
		check_status(status)
		if not status:
			self.dep_pow = ret_dep_pow
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dep_fw') 
			print ('obj = ' + str(obj))
		status, ret_dep_fw = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dep_fw', i)
		check_status(status)
		if not status:
			self.dep_fw = ret_dep_fw
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dep_sg') 
			print ('obj = ' + str(obj))
		status, ret_dep_sg = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dep_sg', i)
		check_status(status)
		if not status:
			self.dep_sg = ret_dep_sg
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dep_cp') 
			print ('obj = ' + str(obj))
		status, ret_dep_cp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dep_cp', i)
		check_status(status)
		if not status:
			self.dep_cp = ret_dep_cp
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dep_lp') 
			print ('obj = ' + str(obj))
		status, ret_dep_lp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dep_lp', i)
		check_status(status)
		if not status:
			self.dep_lp = ret_dep_lp
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dep_man') 
			print ('obj = ' + str(obj))
		status, ret_dep_man = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dep_man', i)
		check_status(status)
		if not status:
			self.dep_man = ret_dep_man
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dep_pl') 
			print ('obj = ' + str(obj))
		status, ret_dep_pl = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dep_pl', i)
		check_status(status)
		if not status:
			self.dep_pl = ret_dep_pl
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'rec_fw') 
			print ('obj = ' + str(obj))
		status, ret_rec_fw = ull.getDoubleFromObject(self.idx, obj, cpopath + 'rec_fw', i)
		check_status(status)
		if not status:
			self.rec_fw = ret_rec_fw
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'rec_sg') 
			print ('obj = ' + str(obj))
		status, ret_rec_sg = ull.getDoubleFromObject(self.idx, obj, cpopath + 'rec_sg', i)
		check_status(status)
		if not status:
			self.rec_sg = ret_rec_sg
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'rec_cp') 
			print ('obj = ' + str(obj))
		status, ret_rec_cp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'rec_cp', i)
		check_status(status)
		if not status:
			self.rec_cp = ret_rec_cp
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pow_dens_fw') 
			print ('obj = ' + str(obj))
		status, ret_pow_dens_fw = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pow_dens_fw', i)
		check_status(status)
		if not status:
			self.pow_dens_fw = ret_pow_dens_fw
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pow_dens_bz') 
			print ('obj = ' + str(obj))
		status, ret_pow_dens_bz = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pow_dens_bz', i)
		check_status(status)
		if not status:
			self.pow_dens_bz = ret_pow_dens_bz
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pow_dens_bz10') 
			print ('obj = ' + str(obj))
		status, ret_pow_dens_bz10 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pow_dens_bz10', i)
		check_status(status)
		if not status:
			self.pow_dens_bz10 = ret_pow_dens_bz10
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pow_dens_bp') 
			print ('obj = ' + str(obj))
		status, ret_pow_dens_bp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pow_dens_bp', i)
		check_status(status)
		if not status:
			self.pow_dens_bp = ret_pow_dens_bp
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pow_dens_man') 
			print ('obj = ' + str(obj))
		status, ret_pow_dens_man = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pow_dens_man', i)
		check_status(status)
		if not status:
			self.pow_dens_man = ret_pow_dens_man
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pow_dens_sh') 
			print ('obj = ' + str(obj))
		status, ret_pow_dens_sh = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pow_dens_sh', i)
		check_status(status)
		if not status:
			self.pow_dens_sh = ret_pow_dens_sh

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'dep_pow')
		ull.deleteData(self.idx, path, cpopath + 'dep_fw')
		ull.deleteData(self.idx, path, cpopath + 'dep_sg')
		ull.deleteData(self.idx, path, cpopath + 'dep_cp')
		ull.deleteData(self.idx, path, cpopath + 'dep_lp')
		ull.deleteData(self.idx, path, cpopath + 'dep_man')
		ull.deleteData(self.idx, path, cpopath + 'dep_pl')
		ull.deleteData(self.idx, path, cpopath + 'rec_fw')
		ull.deleteData(self.idx, path, cpopath + 'rec_sg')
		ull.deleteData(self.idx, path, cpopath + 'rec_cp')
		ull.deleteData(self.idx, path, cpopath + 'pow_dens_fw')
		ull.deleteData(self.idx, path, cpopath + 'pow_dens_bz')
		ull.deleteData(self.idx, path, cpopath + 'pow_dens_bz10')
		ull.deleteData(self.idx, path, cpopath + 'pow_dens_bp')
		ull.deleteData(self.idx, path, cpopath + 'pow_dens_man')
		ull.deleteData(self.idx, path, cpopath + 'pow_dens_sh')


class mod_thermstructuremode_therm:
	'''
	class mod_thermstructuremode_therm
	Thermical parameters

	Attributes:
	- he_fr : float
	   Coolant mass flow rate in "the" reference bb (inboard or outboard) module [Kg/s]; Scalar
	- perc_bp_he : float
	   % of Helium going through the bypass (set to 0 if not otherwise specified)
	- he_out_t : float
	   Outlet temperature (from the bb module) [K]; Scalar
	- fw_he_out_t : float
	   First wall outlet temperature [K]; Scalar
	- sg_he_out_t : float
	   Stiffening grid outlet temperature [K]; Scalar
	- cp_he_out_t : float
	   Cooling plates outlet temperature [K]; Scalar
	- fw_st_max_t : float
	   First wall eurofer maximum temperature [K]; Scalar
	- sg_st_max_t : float
	   Stiffening grid eurofer maximum temperature [K]; Scalar
	- cp_st_max_t : float
	   Cooling plates eurofer maximum temperature [K]; Scalar
	'''

	def __init__(self, base_path_in='mod_therm'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.he_fr = EMPTY_DOUBLE
		self.perc_bp_he = EMPTY_DOUBLE
		self.he_out_t = EMPTY_DOUBLE
		self.fw_he_out_t = EMPTY_DOUBLE
		self.sg_he_out_t = EMPTY_DOUBLE
		self.cp_he_out_t = EMPTY_DOUBLE
		self.fw_st_max_t = EMPTY_DOUBLE
		self.sg_st_max_t = EMPTY_DOUBLE
		self.cp_st_max_t = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mod_thermstructuremode_therm\n'
		ret = ret + space + 'Attribute he_fr: ' + str(self.he_fr) + '\n'
		ret = ret + space + 'Attribute perc_bp_he: ' + str(self.perc_bp_he) + '\n'
		ret = ret + space + 'Attribute he_out_t: ' + str(self.he_out_t) + '\n'
		ret = ret + space + 'Attribute fw_he_out_t: ' + str(self.fw_he_out_t) + '\n'
		ret = ret + space + 'Attribute sg_he_out_t: ' + str(self.sg_he_out_t) + '\n'
		ret = ret + space + 'Attribute cp_he_out_t: ' + str(self.cp_he_out_t) + '\n'
		ret = ret + space + 'Attribute fw_st_max_t: ' + str(self.fw_st_max_t) + '\n'
		ret = ret + space + 'Attribute sg_st_max_t: ' + str(self.sg_st_max_t) + '\n'
		ret = ret + space + 'Attribute cp_st_max_t: ' + str(self.cp_st_max_t) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_thermstructuremode_therm, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_thermstructuremode_therm, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_thermstructuremode_therm, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'he_fr', self.he_fr)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'perc_bp_he', self.perc_bp_he)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'he_out_t', self.he_out_t)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'fw_he_out_t', self.fw_he_out_t)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sg_he_out_t', self.sg_he_out_t)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'cp_he_out_t', self.cp_he_out_t)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'fw_st_max_t', self.fw_st_max_t)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sg_st_max_t', self.sg_st_max_t)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'cp_st_max_t', self.cp_st_max_t)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mod_thermstructuremode_therm, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_he_fr = ull.getDouble(self.idx, path, cpopath + 'he_fr')
		check_status(status)
		if not status:
			self.he_fr = ret_he_fr
		status, ret_perc_bp_he = ull.getDouble(self.idx, path, cpopath + 'perc_bp_he')
		check_status(status)
		if not status:
			self.perc_bp_he = ret_perc_bp_he
		status, ret_he_out_t = ull.getDouble(self.idx, path, cpopath + 'he_out_t')
		check_status(status)
		if not status:
			self.he_out_t = ret_he_out_t
		status, ret_fw_he_out_t = ull.getDouble(self.idx, path, cpopath + 'fw_he_out_t')
		check_status(status)
		if not status:
			self.fw_he_out_t = ret_fw_he_out_t
		status, ret_sg_he_out_t = ull.getDouble(self.idx, path, cpopath + 'sg_he_out_t')
		check_status(status)
		if not status:
			self.sg_he_out_t = ret_sg_he_out_t
		status, ret_cp_he_out_t = ull.getDouble(self.idx, path, cpopath + 'cp_he_out_t')
		check_status(status)
		if not status:
			self.cp_he_out_t = ret_cp_he_out_t
		status, ret_fw_st_max_t = ull.getDouble(self.idx, path, cpopath + 'fw_st_max_t')
		check_status(status)
		if not status:
			self.fw_st_max_t = ret_fw_st_max_t
		status, ret_sg_st_max_t = ull.getDouble(self.idx, path, cpopath + 'sg_st_max_t')
		check_status(status)
		if not status:
			self.sg_st_max_t = ret_sg_st_max_t
		status, ret_cp_st_max_t = ull.getDouble(self.idx, path, cpopath + 'cp_st_max_t')
		check_status(status)
		if not status:
			self.cp_st_max_t = ret_cp_st_max_t

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mod_thermstructuremode_therm, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, he_frVal = ull.getDouble(self.idx, path, cpopath + 'he_fr')
			check_status(status)
			status, perc_bp_heVal = ull.getDouble(self.idx, path, cpopath + 'perc_bp_he')
			check_status(status)
			status, he_out_tVal = ull.getDouble(self.idx, path, cpopath + 'he_out_t')
			check_status(status)
			status, fw_he_out_tVal = ull.getDouble(self.idx, path, cpopath + 'fw_he_out_t')
			check_status(status)
			status, sg_he_out_tVal = ull.getDouble(self.idx, path, cpopath + 'sg_he_out_t')
			check_status(status)
			status, cp_he_out_tVal = ull.getDouble(self.idx, path, cpopath + 'cp_he_out_t')
			check_status(status)
			status, fw_st_max_tVal = ull.getDouble(self.idx, path, cpopath + 'fw_st_max_t')
			check_status(status)
			status, sg_st_max_tVal = ull.getDouble(self.idx, path, cpopath + 'sg_st_max_t')
			check_status(status)
			status, cp_st_max_tVal = ull.getDouble(self.idx, path, cpopath + 'cp_st_max_t')
			check_status(status)
			for i in range(nbslice):
				slice = mod_thermstructuremode_therm(self.base_path)
				slice.setExpIdx(self.idx)
				slice.he_fr = he_frVal
				slice.perc_bp_he = perc_bp_heVal
				slice.he_out_t = he_out_tVal
				slice.fw_he_out_t = fw_he_out_tVal
				slice.sg_he_out_t = sg_he_out_tVal
				slice.cp_he_out_t = cp_he_out_tVal
				slice.fw_st_max_t = fw_st_max_tVal
				slice.sg_st_max_t = sg_st_max_tVal
				slice.cp_st_max_t = cp_st_max_tVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_thermstructuremode_thermObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_thermstructuremode_thermObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_thermstructuremode_thermObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_fr') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_fr', i, self.he_fr)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'perc_bp_he') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'perc_bp_he', i, self.perc_bp_he)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'he_out_t') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'he_out_t', i, self.he_out_t)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fw_he_out_t') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fw_he_out_t', i, self.fw_he_out_t)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sg_he_out_t') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sg_he_out_t', i, self.sg_he_out_t)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'cp_he_out_t') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'cp_he_out_t', i, self.cp_he_out_t)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fw_st_max_t') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fw_st_max_t', i, self.fw_st_max_t)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sg_st_max_t') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sg_st_max_t', i, self.sg_st_max_t)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'cp_st_max_t') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'cp_st_max_t', i, self.cp_st_max_t)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_thermstructuremode_thermObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_fr') 
			print ('obj = ' + str(obj))
		status, ret_he_fr = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_fr', i)
		check_status(status)
		if not status:
			self.he_fr = ret_he_fr
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'perc_bp_he') 
			print ('obj = ' + str(obj))
		status, ret_perc_bp_he = ull.getDoubleFromObject(self.idx, obj, cpopath + 'perc_bp_he', i)
		check_status(status)
		if not status:
			self.perc_bp_he = ret_perc_bp_he
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'he_out_t') 
			print ('obj = ' + str(obj))
		status, ret_he_out_t = ull.getDoubleFromObject(self.idx, obj, cpopath + 'he_out_t', i)
		check_status(status)
		if not status:
			self.he_out_t = ret_he_out_t
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fw_he_out_t') 
			print ('obj = ' + str(obj))
		status, ret_fw_he_out_t = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fw_he_out_t', i)
		check_status(status)
		if not status:
			self.fw_he_out_t = ret_fw_he_out_t
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sg_he_out_t') 
			print ('obj = ' + str(obj))
		status, ret_sg_he_out_t = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sg_he_out_t', i)
		check_status(status)
		if not status:
			self.sg_he_out_t = ret_sg_he_out_t
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'cp_he_out_t') 
			print ('obj = ' + str(obj))
		status, ret_cp_he_out_t = ull.getDoubleFromObject(self.idx, obj, cpopath + 'cp_he_out_t', i)
		check_status(status)
		if not status:
			self.cp_he_out_t = ret_cp_he_out_t
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fw_st_max_t') 
			print ('obj = ' + str(obj))
		status, ret_fw_st_max_t = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fw_st_max_t', i)
		check_status(status)
		if not status:
			self.fw_st_max_t = ret_fw_st_max_t
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sg_st_max_t') 
			print ('obj = ' + str(obj))
		status, ret_sg_st_max_t = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sg_st_max_t', i)
		check_status(status)
		if not status:
			self.sg_st_max_t = ret_sg_st_max_t
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'cp_st_max_t') 
			print ('obj = ' + str(obj))
		status, ret_cp_st_max_t = ull.getDoubleFromObject(self.idx, obj, cpopath + 'cp_st_max_t', i)
		check_status(status)
		if not status:
			self.cp_st_max_t = ret_cp_st_max_t

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'he_fr')
		ull.deleteData(self.idx, path, cpopath + 'perc_bp_he')
		ull.deleteData(self.idx, path, cpopath + 'he_out_t')
		ull.deleteData(self.idx, path, cpopath + 'fw_he_out_t')
		ull.deleteData(self.idx, path, cpopath + 'sg_he_out_t')
		ull.deleteData(self.idx, path, cpopath + 'cp_he_out_t')
		ull.deleteData(self.idx, path, cpopath + 'fw_st_max_t')
		ull.deleteData(self.idx, path, cpopath + 'sg_st_max_t')
		ull.deleteData(self.idx, path, cpopath + 'cp_st_max_t')


class mod_th_hydstructuremode_th_hyd:
	'''
	class mod_th_hydstructuremode_th_hyd
	hydrodynamics parameters

	Attributes:
	- fw_dp_he : float
	   Pressure drops in the first wall [Pa]; Scalar
	- sg_dp_he : float
	   Pressure drops in the stiffening grid [Pa]; Scalar
	- cp_dp_he : float
	   Pressure drops in the cooling plates [Pa]; Scalar
	- man_dp_he : float
	   Pressure drops in the manifolds [Pa]; Scalar
	- tot_dp_he : float
	   Total pressure drops in bb module [Pa]; Scalar
	- bp_dp_he : float
	   Total pressure drops in the by pass (if any) [Pa]; ScalarScalar
	- circ_dp_he : float
	   Pressure drops in one He circuit [Pa]; Scalar
	'''

	def __init__(self, base_path_in='mod_th_hyd'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.fw_dp_he = EMPTY_DOUBLE
		self.sg_dp_he = EMPTY_DOUBLE
		self.cp_dp_he = EMPTY_DOUBLE
		self.man_dp_he = EMPTY_DOUBLE
		self.tot_dp_he = EMPTY_DOUBLE
		self.bp_dp_he = EMPTY_DOUBLE
		self.circ_dp_he = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mod_th_hydstructuremode_th_hyd\n'
		ret = ret + space + 'Attribute fw_dp_he: ' + str(self.fw_dp_he) + '\n'
		ret = ret + space + 'Attribute sg_dp_he: ' + str(self.sg_dp_he) + '\n'
		ret = ret + space + 'Attribute cp_dp_he: ' + str(self.cp_dp_he) + '\n'
		ret = ret + space + 'Attribute man_dp_he: ' + str(self.man_dp_he) + '\n'
		ret = ret + space + 'Attribute tot_dp_he: ' + str(self.tot_dp_he) + '\n'
		ret = ret + space + 'Attribute bp_dp_he: ' + str(self.bp_dp_he) + '\n'
		ret = ret + space + 'Attribute circ_dp_he: ' + str(self.circ_dp_he) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_th_hydstructuremode_th_hyd, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_th_hydstructuremode_th_hyd, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_th_hydstructuremode_th_hyd, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'fw_dp_he', self.fw_dp_he)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sg_dp_he', self.sg_dp_he)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'cp_dp_he', self.cp_dp_he)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'man_dp_he', self.man_dp_he)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'tot_dp_he', self.tot_dp_he)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'bp_dp_he', self.bp_dp_he)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'circ_dp_he', self.circ_dp_he)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mod_th_hydstructuremode_th_hyd, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_fw_dp_he = ull.getDouble(self.idx, path, cpopath + 'fw_dp_he')
		check_status(status)
		if not status:
			self.fw_dp_he = ret_fw_dp_he
		status, ret_sg_dp_he = ull.getDouble(self.idx, path, cpopath + 'sg_dp_he')
		check_status(status)
		if not status:
			self.sg_dp_he = ret_sg_dp_he
		status, ret_cp_dp_he = ull.getDouble(self.idx, path, cpopath + 'cp_dp_he')
		check_status(status)
		if not status:
			self.cp_dp_he = ret_cp_dp_he
		status, ret_man_dp_he = ull.getDouble(self.idx, path, cpopath + 'man_dp_he')
		check_status(status)
		if not status:
			self.man_dp_he = ret_man_dp_he
		status, ret_tot_dp_he = ull.getDouble(self.idx, path, cpopath + 'tot_dp_he')
		check_status(status)
		if not status:
			self.tot_dp_he = ret_tot_dp_he
		status, ret_bp_dp_he = ull.getDouble(self.idx, path, cpopath + 'bp_dp_he')
		check_status(status)
		if not status:
			self.bp_dp_he = ret_bp_dp_he
		status, ret_circ_dp_he = ull.getDouble(self.idx, path, cpopath + 'circ_dp_he')
		check_status(status)
		if not status:
			self.circ_dp_he = ret_circ_dp_he

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mod_th_hydstructuremode_th_hyd, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, fw_dp_heVal = ull.getDouble(self.idx, path, cpopath + 'fw_dp_he')
			check_status(status)
			status, sg_dp_heVal = ull.getDouble(self.idx, path, cpopath + 'sg_dp_he')
			check_status(status)
			status, cp_dp_heVal = ull.getDouble(self.idx, path, cpopath + 'cp_dp_he')
			check_status(status)
			status, man_dp_heVal = ull.getDouble(self.idx, path, cpopath + 'man_dp_he')
			check_status(status)
			status, tot_dp_heVal = ull.getDouble(self.idx, path, cpopath + 'tot_dp_he')
			check_status(status)
			status, bp_dp_heVal = ull.getDouble(self.idx, path, cpopath + 'bp_dp_he')
			check_status(status)
			status, circ_dp_heVal = ull.getDouble(self.idx, path, cpopath + 'circ_dp_he')
			check_status(status)
			for i in range(nbslice):
				slice = mod_th_hydstructuremode_th_hyd(self.base_path)
				slice.setExpIdx(self.idx)
				slice.fw_dp_he = fw_dp_heVal
				slice.sg_dp_he = sg_dp_heVal
				slice.cp_dp_he = cp_dp_heVal
				slice.man_dp_he = man_dp_heVal
				slice.tot_dp_he = tot_dp_heVal
				slice.bp_dp_he = bp_dp_heVal
				slice.circ_dp_he = circ_dp_heVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_th_hydstructuremode_th_hydObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_th_hydstructuremode_th_hydObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_th_hydstructuremode_th_hydObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fw_dp_he') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fw_dp_he', i, self.fw_dp_he)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sg_dp_he') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sg_dp_he', i, self.sg_dp_he)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'cp_dp_he') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'cp_dp_he', i, self.cp_dp_he)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'man_dp_he') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'man_dp_he', i, self.man_dp_he)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'tot_dp_he') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'tot_dp_he', i, self.tot_dp_he)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'bp_dp_he') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'bp_dp_he', i, self.bp_dp_he)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'circ_dp_he') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'circ_dp_he', i, self.circ_dp_he)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_th_hydstructuremode_th_hydObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fw_dp_he') 
			print ('obj = ' + str(obj))
		status, ret_fw_dp_he = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fw_dp_he', i)
		check_status(status)
		if not status:
			self.fw_dp_he = ret_fw_dp_he
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sg_dp_he') 
			print ('obj = ' + str(obj))
		status, ret_sg_dp_he = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sg_dp_he', i)
		check_status(status)
		if not status:
			self.sg_dp_he = ret_sg_dp_he
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'cp_dp_he') 
			print ('obj = ' + str(obj))
		status, ret_cp_dp_he = ull.getDoubleFromObject(self.idx, obj, cpopath + 'cp_dp_he', i)
		check_status(status)
		if not status:
			self.cp_dp_he = ret_cp_dp_he
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'man_dp_he') 
			print ('obj = ' + str(obj))
		status, ret_man_dp_he = ull.getDoubleFromObject(self.idx, obj, cpopath + 'man_dp_he', i)
		check_status(status)
		if not status:
			self.man_dp_he = ret_man_dp_he
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'tot_dp_he') 
			print ('obj = ' + str(obj))
		status, ret_tot_dp_he = ull.getDoubleFromObject(self.idx, obj, cpopath + 'tot_dp_he', i)
		check_status(status)
		if not status:
			self.tot_dp_he = ret_tot_dp_he
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'bp_dp_he') 
			print ('obj = ' + str(obj))
		status, ret_bp_dp_he = ull.getDoubleFromObject(self.idx, obj, cpopath + 'bp_dp_he', i)
		check_status(status)
		if not status:
			self.bp_dp_he = ret_bp_dp_he
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'circ_dp_he') 
			print ('obj = ' + str(obj))
		status, ret_circ_dp_he = ull.getDoubleFromObject(self.idx, obj, cpopath + 'circ_dp_he', i)
		check_status(status)
		if not status:
			self.circ_dp_he = ret_circ_dp_he

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'fw_dp_he')
		ull.deleteData(self.idx, path, cpopath + 'sg_dp_he')
		ull.deleteData(self.idx, path, cpopath + 'cp_dp_he')
		ull.deleteData(self.idx, path, cpopath + 'man_dp_he')
		ull.deleteData(self.idx, path, cpopath + 'tot_dp_he')
		ull.deleteData(self.idx, path, cpopath + 'bp_dp_he')
		ull.deleteData(self.idx, path, cpopath + 'circ_dp_he')


class mod_mechstructuremode_mech:
	'''
	class mod_mechstructuremode_mech
	Mechanical parameters

	Attributes:
	- fw_min_ts_mg : float
	   Min margin to tensile stress limit in the first wall; Scalar
	- fw_min_bd_mg : float
	   Min margin to banding stress limit in the first wall; Scalar
	- sg_min_ts_mg : float
	   Min margin to tensile stress limit in the stiffening grid; Scalar
	- sg_min_bd_mg : float
	   Min margin to bending stress limit in the stiffening grid; Scalar
	- cp_min_ts_mg : float
	   Min margin to tensile stress limit in the cooling plate; Scalar
	- cp_min_bd_mg : float
	   Min margin to bending stress limit in the cooling plate; Scalar
	- min_ts_mg_ac : float
	   Min tensile margin in accidental conditions; Scalar
	- min_bd_mg_ac : float
	   Min bending margin in accidental conditions; Scalar
	'''

	def __init__(self, base_path_in='mod_mech'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.fw_min_ts_mg = EMPTY_DOUBLE
		self.fw_min_bd_mg = EMPTY_DOUBLE
		self.sg_min_ts_mg = EMPTY_DOUBLE
		self.sg_min_bd_mg = EMPTY_DOUBLE
		self.cp_min_ts_mg = EMPTY_DOUBLE
		self.cp_min_bd_mg = EMPTY_DOUBLE
		self.min_ts_mg_ac = EMPTY_DOUBLE
		self.min_bd_mg_ac = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mod_mechstructuremode_mech\n'
		ret = ret + space + 'Attribute fw_min_ts_mg: ' + str(self.fw_min_ts_mg) + '\n'
		ret = ret + space + 'Attribute fw_min_bd_mg: ' + str(self.fw_min_bd_mg) + '\n'
		ret = ret + space + 'Attribute sg_min_ts_mg: ' + str(self.sg_min_ts_mg) + '\n'
		ret = ret + space + 'Attribute sg_min_bd_mg: ' + str(self.sg_min_bd_mg) + '\n'
		ret = ret + space + 'Attribute cp_min_ts_mg: ' + str(self.cp_min_ts_mg) + '\n'
		ret = ret + space + 'Attribute cp_min_bd_mg: ' + str(self.cp_min_bd_mg) + '\n'
		ret = ret + space + 'Attribute min_ts_mg_ac: ' + str(self.min_ts_mg_ac) + '\n'
		ret = ret + space + 'Attribute min_bd_mg_ac: ' + str(self.min_bd_mg_ac) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_mechstructuremode_mech, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_mechstructuremode_mech, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_mechstructuremode_mech, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'fw_min_ts_mg', self.fw_min_ts_mg)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'fw_min_bd_mg', self.fw_min_bd_mg)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sg_min_ts_mg', self.sg_min_ts_mg)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sg_min_bd_mg', self.sg_min_bd_mg)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'cp_min_ts_mg', self.cp_min_ts_mg)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'cp_min_bd_mg', self.cp_min_bd_mg)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'min_ts_mg_ac', self.min_ts_mg_ac)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'min_bd_mg_ac', self.min_bd_mg_ac)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mod_mechstructuremode_mech, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_fw_min_ts_mg = ull.getDouble(self.idx, path, cpopath + 'fw_min_ts_mg')
		check_status(status)
		if not status:
			self.fw_min_ts_mg = ret_fw_min_ts_mg
		status, ret_fw_min_bd_mg = ull.getDouble(self.idx, path, cpopath + 'fw_min_bd_mg')
		check_status(status)
		if not status:
			self.fw_min_bd_mg = ret_fw_min_bd_mg
		status, ret_sg_min_ts_mg = ull.getDouble(self.idx, path, cpopath + 'sg_min_ts_mg')
		check_status(status)
		if not status:
			self.sg_min_ts_mg = ret_sg_min_ts_mg
		status, ret_sg_min_bd_mg = ull.getDouble(self.idx, path, cpopath + 'sg_min_bd_mg')
		check_status(status)
		if not status:
			self.sg_min_bd_mg = ret_sg_min_bd_mg
		status, ret_cp_min_ts_mg = ull.getDouble(self.idx, path, cpopath + 'cp_min_ts_mg')
		check_status(status)
		if not status:
			self.cp_min_ts_mg = ret_cp_min_ts_mg
		status, ret_cp_min_bd_mg = ull.getDouble(self.idx, path, cpopath + 'cp_min_bd_mg')
		check_status(status)
		if not status:
			self.cp_min_bd_mg = ret_cp_min_bd_mg
		status, ret_min_ts_mg_ac = ull.getDouble(self.idx, path, cpopath + 'min_ts_mg_ac')
		check_status(status)
		if not status:
			self.min_ts_mg_ac = ret_min_ts_mg_ac
		status, ret_min_bd_mg_ac = ull.getDouble(self.idx, path, cpopath + 'min_bd_mg_ac')
		check_status(status)
		if not status:
			self.min_bd_mg_ac = ret_min_bd_mg_ac

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mod_mechstructuremode_mech, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, fw_min_ts_mgVal = ull.getDouble(self.idx, path, cpopath + 'fw_min_ts_mg')
			check_status(status)
			status, fw_min_bd_mgVal = ull.getDouble(self.idx, path, cpopath + 'fw_min_bd_mg')
			check_status(status)
			status, sg_min_ts_mgVal = ull.getDouble(self.idx, path, cpopath + 'sg_min_ts_mg')
			check_status(status)
			status, sg_min_bd_mgVal = ull.getDouble(self.idx, path, cpopath + 'sg_min_bd_mg')
			check_status(status)
			status, cp_min_ts_mgVal = ull.getDouble(self.idx, path, cpopath + 'cp_min_ts_mg')
			check_status(status)
			status, cp_min_bd_mgVal = ull.getDouble(self.idx, path, cpopath + 'cp_min_bd_mg')
			check_status(status)
			status, min_ts_mg_acVal = ull.getDouble(self.idx, path, cpopath + 'min_ts_mg_ac')
			check_status(status)
			status, min_bd_mg_acVal = ull.getDouble(self.idx, path, cpopath + 'min_bd_mg_ac')
			check_status(status)
			for i in range(nbslice):
				slice = mod_mechstructuremode_mech(self.base_path)
				slice.setExpIdx(self.idx)
				slice.fw_min_ts_mg = fw_min_ts_mgVal
				slice.fw_min_bd_mg = fw_min_bd_mgVal
				slice.sg_min_ts_mg = sg_min_ts_mgVal
				slice.sg_min_bd_mg = sg_min_bd_mgVal
				slice.cp_min_ts_mg = cp_min_ts_mgVal
				slice.cp_min_bd_mg = cp_min_bd_mgVal
				slice.min_ts_mg_ac = min_ts_mg_acVal
				slice.min_bd_mg_ac = min_bd_mg_acVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_mechstructuremode_mechObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_mechstructuremode_mechObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_mechstructuremode_mechObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fw_min_ts_mg') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fw_min_ts_mg', i, self.fw_min_ts_mg)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'fw_min_bd_mg') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'fw_min_bd_mg', i, self.fw_min_bd_mg)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sg_min_ts_mg') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sg_min_ts_mg', i, self.sg_min_ts_mg)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sg_min_bd_mg') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sg_min_bd_mg', i, self.sg_min_bd_mg)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'cp_min_ts_mg') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'cp_min_ts_mg', i, self.cp_min_ts_mg)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'cp_min_bd_mg') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'cp_min_bd_mg', i, self.cp_min_bd_mg)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'min_ts_mg_ac') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'min_ts_mg_ac', i, self.min_ts_mg_ac)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'min_bd_mg_ac') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'min_bd_mg_ac', i, self.min_bd_mg_ac)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_mechstructuremode_mechObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fw_min_ts_mg') 
			print ('obj = ' + str(obj))
		status, ret_fw_min_ts_mg = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fw_min_ts_mg', i)
		check_status(status)
		if not status:
			self.fw_min_ts_mg = ret_fw_min_ts_mg
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'fw_min_bd_mg') 
			print ('obj = ' + str(obj))
		status, ret_fw_min_bd_mg = ull.getDoubleFromObject(self.idx, obj, cpopath + 'fw_min_bd_mg', i)
		check_status(status)
		if not status:
			self.fw_min_bd_mg = ret_fw_min_bd_mg
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sg_min_ts_mg') 
			print ('obj = ' + str(obj))
		status, ret_sg_min_ts_mg = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sg_min_ts_mg', i)
		check_status(status)
		if not status:
			self.sg_min_ts_mg = ret_sg_min_ts_mg
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sg_min_bd_mg') 
			print ('obj = ' + str(obj))
		status, ret_sg_min_bd_mg = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sg_min_bd_mg', i)
		check_status(status)
		if not status:
			self.sg_min_bd_mg = ret_sg_min_bd_mg
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'cp_min_ts_mg') 
			print ('obj = ' + str(obj))
		status, ret_cp_min_ts_mg = ull.getDoubleFromObject(self.idx, obj, cpopath + 'cp_min_ts_mg', i)
		check_status(status)
		if not status:
			self.cp_min_ts_mg = ret_cp_min_ts_mg
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'cp_min_bd_mg') 
			print ('obj = ' + str(obj))
		status, ret_cp_min_bd_mg = ull.getDoubleFromObject(self.idx, obj, cpopath + 'cp_min_bd_mg', i)
		check_status(status)
		if not status:
			self.cp_min_bd_mg = ret_cp_min_bd_mg
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'min_ts_mg_ac') 
			print ('obj = ' + str(obj))
		status, ret_min_ts_mg_ac = ull.getDoubleFromObject(self.idx, obj, cpopath + 'min_ts_mg_ac', i)
		check_status(status)
		if not status:
			self.min_ts_mg_ac = ret_min_ts_mg_ac
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'min_bd_mg_ac') 
			print ('obj = ' + str(obj))
		status, ret_min_bd_mg_ac = ull.getDoubleFromObject(self.idx, obj, cpopath + 'min_bd_mg_ac', i)
		check_status(status)
		if not status:
			self.min_bd_mg_ac = ret_min_bd_mg_ac

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'fw_min_ts_mg')
		ull.deleteData(self.idx, path, cpopath + 'fw_min_bd_mg')
		ull.deleteData(self.idx, path, cpopath + 'sg_min_ts_mg')
		ull.deleteData(self.idx, path, cpopath + 'sg_min_bd_mg')
		ull.deleteData(self.idx, path, cpopath + 'cp_min_ts_mg')
		ull.deleteData(self.idx, path, cpopath + 'cp_min_bd_mg')
		ull.deleteData(self.idx, path, cpopath + 'min_ts_mg_ac')
		ull.deleteData(self.idx, path, cpopath + 'min_bd_mg_ac')


class mod_lipbstructuremode_lipb:
	'''
	class mod_lipbstructuremode_lipb
	Pb-15.7Li "effects"

	Attributes:
	- lp_rec_day : float
	   nb of Pb-15.7Li recirculation per day [Pa]; Scalar
	- bb_lp_fr : numpy.ndarray 1D with float
	   Pb-15.7Li mass flow rate in "the" bb module (or in each bb module) [Kg/s]; Vector(nmodules)
	- lp_inl_p : float
	   Pb-15.7Li inlet pressure [Pa]; Scalar
	- bu_dp_lp : float
	   Pb-15.7Li pressure drops in the breeder unit [Pa]; Scalar
	- man_dp_lp : float
	   Pb-15.7Li pressure drops in the bb manifolds [Pa]; Scalar
	- tot_dp_lp : float
	   Pb-15.7Li total pressure drops [Pa]; Scalar
	- bu_lp_ave_t : float
	   Pb-15.7Li average temperature in a breeder unit [K]; Scalar
	- bu_lp_max_t : float
	   Pb-15.7Li max temperature in a breeder unit [K]; Scalar
	'''

	def __init__(self, base_path_in='mod_lipb'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.lp_rec_day = EMPTY_DOUBLE
		self.bb_lp_fr = numpy.zeros(0, numpy.float64, order='C')
		self.lp_inl_p = EMPTY_DOUBLE
		self.bu_dp_lp = EMPTY_DOUBLE
		self.man_dp_lp = EMPTY_DOUBLE
		self.tot_dp_lp = EMPTY_DOUBLE
		self.bu_lp_ave_t = EMPTY_DOUBLE
		self.bu_lp_max_t = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mod_lipbstructuremode_lipb\n'
		ret = ret + space + 'Attribute lp_rec_day: ' + str(self.lp_rec_day) + '\n'
		s = self.bb_lp_fr.__str__()
		ret = ret + space + 'Attribute bb_lp_fr\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute lp_inl_p: ' + str(self.lp_inl_p) + '\n'
		ret = ret + space + 'Attribute bu_dp_lp: ' + str(self.bu_dp_lp) + '\n'
		ret = ret + space + 'Attribute man_dp_lp: ' + str(self.man_dp_lp) + '\n'
		ret = ret + space + 'Attribute tot_dp_lp: ' + str(self.tot_dp_lp) + '\n'
		ret = ret + space + 'Attribute bu_lp_ave_t: ' + str(self.bu_lp_ave_t) + '\n'
		ret = ret + space + 'Attribute bu_lp_max_t: ' + str(self.bu_lp_max_t) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_lipbstructuremode_lipb, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_lipbstructuremode_lipb, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_lipbstructuremode_lipb, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'lp_rec_day', self.lp_rec_day)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'bb_lp_fr', numpy.array(self.bb_lp_fr).astype(numpy.float64), False)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'lp_inl_p', self.lp_inl_p)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'bu_dp_lp', self.bu_dp_lp)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'man_dp_lp', self.man_dp_lp)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'tot_dp_lp', self.tot_dp_lp)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'bu_lp_ave_t', self.bu_lp_ave_t)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'bu_lp_max_t', self.bu_lp_max_t)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mod_lipbstructuremode_lipb, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_lp_rec_day = ull.getDouble(self.idx, path, cpopath + 'lp_rec_day')
		check_status(status)
		if not status:
			self.lp_rec_day = ret_lp_rec_day
		status, ret_bb_lp_fr = ull.getVect1DDouble(self.idx, path, cpopath + 'bb_lp_fr')
		check_status(status)
		if not status:
			self.bb_lp_fr = ret_bb_lp_fr
		status, ret_lp_inl_p = ull.getDouble(self.idx, path, cpopath + 'lp_inl_p')
		check_status(status)
		if not status:
			self.lp_inl_p = ret_lp_inl_p
		status, ret_bu_dp_lp = ull.getDouble(self.idx, path, cpopath + 'bu_dp_lp')
		check_status(status)
		if not status:
			self.bu_dp_lp = ret_bu_dp_lp
		status, ret_man_dp_lp = ull.getDouble(self.idx, path, cpopath + 'man_dp_lp')
		check_status(status)
		if not status:
			self.man_dp_lp = ret_man_dp_lp
		status, ret_tot_dp_lp = ull.getDouble(self.idx, path, cpopath + 'tot_dp_lp')
		check_status(status)
		if not status:
			self.tot_dp_lp = ret_tot_dp_lp
		status, ret_bu_lp_ave_t = ull.getDouble(self.idx, path, cpopath + 'bu_lp_ave_t')
		check_status(status)
		if not status:
			self.bu_lp_ave_t = ret_bu_lp_ave_t
		status, ret_bu_lp_max_t = ull.getDouble(self.idx, path, cpopath + 'bu_lp_max_t')
		check_status(status)
		if not status:
			self.bu_lp_max_t = ret_bu_lp_max_t

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mod_lipbstructuremode_lipb, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, lp_rec_dayVal = ull.getDouble(self.idx, path, cpopath + 'lp_rec_day')
			check_status(status)
			status, bb_lp_frVal = ull.getVect1DDouble(self.idx, path, cpopath + 'bb_lp_fr')
			check_status(status)
			status, lp_inl_pVal = ull.getDouble(self.idx, path, cpopath + 'lp_inl_p')
			check_status(status)
			status, bu_dp_lpVal = ull.getDouble(self.idx, path, cpopath + 'bu_dp_lp')
			check_status(status)
			status, man_dp_lpVal = ull.getDouble(self.idx, path, cpopath + 'man_dp_lp')
			check_status(status)
			status, tot_dp_lpVal = ull.getDouble(self.idx, path, cpopath + 'tot_dp_lp')
			check_status(status)
			status, bu_lp_ave_tVal = ull.getDouble(self.idx, path, cpopath + 'bu_lp_ave_t')
			check_status(status)
			status, bu_lp_max_tVal = ull.getDouble(self.idx, path, cpopath + 'bu_lp_max_t')
			check_status(status)
			for i in range(nbslice):
				slice = mod_lipbstructuremode_lipb(self.base_path)
				slice.setExpIdx(self.idx)
				slice.lp_rec_day = lp_rec_dayVal
				slice.bb_lp_fr = bb_lp_frVal
				slice.lp_inl_p = lp_inl_pVal
				slice.bu_dp_lp = bu_dp_lpVal
				slice.man_dp_lp = man_dp_lpVal
				slice.tot_dp_lp = tot_dp_lpVal
				slice.bu_lp_ave_t = bu_lp_ave_tVal
				slice.bu_lp_max_t = bu_lp_max_tVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_lipbstructuremode_lipbObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_lipbstructuremode_lipbObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_lipbstructuremode_lipbObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'lp_rec_day') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'lp_rec_day', i, self.lp_rec_day)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'bb_lp_fr') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'bb_lp_fr', i, numpy.array(self.bb_lp_fr).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'lp_inl_p') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'lp_inl_p', i, self.lp_inl_p)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'bu_dp_lp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'bu_dp_lp', i, self.bu_dp_lp)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'man_dp_lp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'man_dp_lp', i, self.man_dp_lp)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'tot_dp_lp') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'tot_dp_lp', i, self.tot_dp_lp)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'bu_lp_ave_t') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'bu_lp_ave_t', i, self.bu_lp_ave_t)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'bu_lp_max_t') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'bu_lp_max_t', i, self.bu_lp_max_t)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_lipbstructuremode_lipbObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'lp_rec_day') 
			print ('obj = ' + str(obj))
		status, ret_lp_rec_day = ull.getDoubleFromObject(self.idx, obj, cpopath + 'lp_rec_day', i)
		check_status(status)
		if not status:
			self.lp_rec_day = ret_lp_rec_day
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'bb_lp_fr') 
			print ('obj = ' + str(obj))
		status, ret_bb_lp_fr = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'bb_lp_fr', i)
		check_status(status)
		if not status:
			self.bb_lp_fr = ret_bb_lp_fr
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'lp_inl_p') 
			print ('obj = ' + str(obj))
		status, ret_lp_inl_p = ull.getDoubleFromObject(self.idx, obj, cpopath + 'lp_inl_p', i)
		check_status(status)
		if not status:
			self.lp_inl_p = ret_lp_inl_p
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'bu_dp_lp') 
			print ('obj = ' + str(obj))
		status, ret_bu_dp_lp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'bu_dp_lp', i)
		check_status(status)
		if not status:
			self.bu_dp_lp = ret_bu_dp_lp
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'man_dp_lp') 
			print ('obj = ' + str(obj))
		status, ret_man_dp_lp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'man_dp_lp', i)
		check_status(status)
		if not status:
			self.man_dp_lp = ret_man_dp_lp
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'tot_dp_lp') 
			print ('obj = ' + str(obj))
		status, ret_tot_dp_lp = ull.getDoubleFromObject(self.idx, obj, cpopath + 'tot_dp_lp', i)
		check_status(status)
		if not status:
			self.tot_dp_lp = ret_tot_dp_lp
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'bu_lp_ave_t') 
			print ('obj = ' + str(obj))
		status, ret_bu_lp_ave_t = ull.getDoubleFromObject(self.idx, obj, cpopath + 'bu_lp_ave_t', i)
		check_status(status)
		if not status:
			self.bu_lp_ave_t = ret_bu_lp_ave_t
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'bu_lp_max_t') 
			print ('obj = ' + str(obj))
		status, ret_bu_lp_max_t = ull.getDoubleFromObject(self.idx, obj, cpopath + 'bu_lp_max_t', i)
		check_status(status)
		if not status:
			self.bu_lp_max_t = ret_bu_lp_max_t

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'lp_rec_day')
		ull.deleteData(self.idx, path, cpopath + 'bb_lp_fr')
		ull.deleteData(self.idx, path, cpopath + 'lp_inl_p')
		ull.deleteData(self.idx, path, cpopath + 'bu_dp_lp')
		ull.deleteData(self.idx, path, cpopath + 'man_dp_lp')
		ull.deleteData(self.idx, path, cpopath + 'tot_dp_lp')
		ull.deleteData(self.idx, path, cpopath + 'bu_lp_ave_t')
		ull.deleteData(self.idx, path, cpopath + 'bu_lp_max_t')


class mod_tritiumstructuremode_tritium:
	'''
	class mod_tritiumstructuremode_tritium
	Tritium parameters

	Attributes:
	- t_conc_lipb : float
	   Tritium concentration in Pb-15.7Li; Scalar
	- t_conc_he : float
	   Tritium concentration in He; Scalar
	'''

	def __init__(self, base_path_in='mod_tritium'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.t_conc_lipb = EMPTY_DOUBLE
		self.t_conc_he = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mod_tritiumstructuremode_tritium\n'
		ret = ret + space + 'Attribute t_conc_lipb: ' + str(self.t_conc_lipb) + '\n'
		ret = ret + space + 'Attribute t_conc_he: ' + str(self.t_conc_he) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_tritiumstructuremode_tritium, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_tritiumstructuremode_tritium, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mod_tritiumstructuremode_tritium, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 't_conc_lipb', self.t_conc_lipb)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 't_conc_he', self.t_conc_he)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mod_tritiumstructuremode_tritium, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_t_conc_lipb = ull.getDouble(self.idx, path, cpopath + 't_conc_lipb')
		check_status(status)
		if not status:
			self.t_conc_lipb = ret_t_conc_lipb
		status, ret_t_conc_he = ull.getDouble(self.idx, path, cpopath + 't_conc_he')
		check_status(status)
		if not status:
			self.t_conc_he = ret_t_conc_he

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mod_tritiumstructuremode_tritium, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, t_conc_lipbVal = ull.getDouble(self.idx, path, cpopath + 't_conc_lipb')
			check_status(status)
			status, t_conc_heVal = ull.getDouble(self.idx, path, cpopath + 't_conc_he')
			check_status(status)
			for i in range(nbslice):
				slice = mod_tritiumstructuremode_tritium(self.base_path)
				slice.setExpIdx(self.idx)
				slice.t_conc_lipb = t_conc_lipbVal
				slice.t_conc_he = t_conc_heVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_tritiumstructuremode_tritiumObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_tritiumstructuremode_tritiumObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_tritiumstructuremode_tritiumObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 't_conc_lipb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 't_conc_lipb', i, self.t_conc_lipb)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 't_conc_he') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 't_conc_he', i, self.t_conc_he)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mod_tritiumstructuremode_tritiumObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 't_conc_lipb') 
			print ('obj = ' + str(obj))
		status, ret_t_conc_lipb = ull.getDoubleFromObject(self.idx, obj, cpopath + 't_conc_lipb', i)
		check_status(status)
		if not status:
			self.t_conc_lipb = ret_t_conc_lipb
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 't_conc_he') 
			print ('obj = ' + str(obj))
		status, ret_t_conc_he = ull.getDoubleFromObject(self.idx, obj, cpopath + 't_conc_he', i)
		check_status(status)
		if not status:
			self.t_conc_he = ret_t_conc_he

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 't_conc_lipb')
		ull.deleteData(self.idx, path, cpopath + 't_conc_he')


class outboardstructurehcllbb_specs:
	'''
	class outboardstructurehcllbb_specs
	Outboard

	Attributes:
	- mass : numpy.ndarray 1D with float
	   Mass of inboard or outboard breeding blanket modules (located at equatorial midplane if only one considered) [Kg]; Vector(nmodules)
	- dr : numpy.ndarray 1D with float
	   Inboard or outboard breeding blanket radial build giving the thickness of each layer [m]; Vector(nlayers)
	- mat : numpy.ndarray 1D with float
	   Inboard or outboard breeding blanket materials; Vector(nlayers)
	- composition : numpy.ndarray 2D with float
	   Inboard or outboard breeding blanket radial build giving for each layer (1: First Wall protective layer, 2: First Wall, 3 : breeder zone, 4 : back plates, 5 : manifolds), the percentage of each material respectively (Meaning of the material index 1: Eurofer, 2: Pb-15.7Li, 3: He, 4: Water, 5: Tungsten Carbide, 6: Boron, 7: Tungsten, 8: Stainless Steel 316) in %vol; Matrix(nlayers(=5), max_nmaterials)
	- mod_geom : class mod_geomstructurebb_geometry
	   Geometrical parameters of "the" reference region blanket module
	- mod_neutr : class mod_neutrstructuremode_neutr
	   Neutrons "effects"
	- mod_therm : class mod_thermstructuremode_therm
	   Thermical parameters
	- mod_th_hyd : class mod_th_hydstructuremode_th_hyd
	   hydrodynamics parameters
	- mod_mech : class mod_mechstructuremode_mech
	   Mechanical parameters
	- mod_lipb : class mod_lipbstructuremode_lipb
	   Pb-15.7Li "effects"
	- mod_tritium : class mod_tritiumstructuremode_tritium
	   Tritium parameters
	'''

	def __init__(self, base_path_in='outboard'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.mass = numpy.zeros(0, numpy.float64, order='C')
		self.dr = numpy.zeros(0, numpy.float64, order='C')
		self.mat = numpy.zeros(0, numpy.float64, order='C')
		self.composition = numpy.zeros((0,0), numpy.float64, order='C')
		self.mod_geom = mod_geomstructurebb_geometry('mod_geom')
		self.mod_neutr = mod_neutrstructuremode_neutr('mod_neutr')
		self.mod_therm = mod_thermstructuremode_therm('mod_therm')
		self.mod_th_hyd = mod_th_hydstructuremode_th_hyd('mod_th_hyd')
		self.mod_mech = mod_mechstructuremode_mech('mod_mech')
		self.mod_lipb = mod_lipbstructuremode_lipb('mod_lipb')
		self.mod_tritium = mod_tritiumstructuremode_tritium('mod_tritium')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class outboardstructurehcllbb_specs\n'
		s = self.mass.__str__()
		ret = ret + space + 'Attribute mass\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dr.__str__()
		ret = ret + space + 'Attribute dr\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.mat.__str__()
		ret = ret + space + 'Attribute mat\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.composition.__str__()
		ret = ret + space + 'Attribute composition\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute mod_geom\n ' + self.mod_geom.__str__(depth+1)
		ret = ret + space + 'Attribute mod_neutr\n ' + self.mod_neutr.__str__(depth+1)
		ret = ret + space + 'Attribute mod_therm\n ' + self.mod_therm.__str__(depth+1)
		ret = ret + space + 'Attribute mod_th_hyd\n ' + self.mod_th_hyd.__str__(depth+1)
		ret = ret + space + 'Attribute mod_mech\n ' + self.mod_mech.__str__(depth+1)
		ret = ret + space + 'Attribute mod_lipb\n ' + self.mod_lipb.__str__(depth+1)
		ret = ret + space + 'Attribute mod_tritium\n ' + self.mod_tritium.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.mod_geom.setExpIdx(idx)
		self.mod_neutr.setExpIdx(idx)
		self.mod_therm.setExpIdx(idx)
		self.mod_th_hyd.setExpIdx(idx)
		self.mod_mech.setExpIdx(idx)
		self.mod_lipb.setExpIdx(idx)
		self.mod_tritium.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructurehcllbb_specs, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mod_geom.cpoTime = self.cpoTime
		self.mod_geom.putSlice(path, cpopath)
		self.mod_neutr.cpoTime = self.cpoTime
		self.mod_neutr.putSlice(path, cpopath)
		self.mod_therm.cpoTime = self.cpoTime
		self.mod_therm.putSlice(path, cpopath)
		self.mod_th_hyd.cpoTime = self.cpoTime
		self.mod_th_hyd.putSlice(path, cpopath)
		self.mod_mech.cpoTime = self.cpoTime
		self.mod_mech.putSlice(path, cpopath)
		self.mod_lipb.cpoTime = self.cpoTime
		self.mod_lipb.putSlice(path, cpopath)
		self.mod_tritium.cpoTime = self.cpoTime
		self.mod_tritium.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructurehcllbb_specs, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mod_geom.replaceLastSlice(path, cpopath)
		self.mod_neutr.replaceLastSlice(path, cpopath)
		self.mod_therm.replaceLastSlice(path, cpopath)
		self.mod_th_hyd.replaceLastSlice(path, cpopath)
		self.mod_mech.replaceLastSlice(path, cpopath)
		self.mod_lipb.replaceLastSlice(path, cpopath)
		self.mod_tritium.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructurehcllbb_specs, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'mass', numpy.array(self.mass).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'dr', numpy.array(self.dr).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'mat', numpy.array(self.mat).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'composition', numpy.array(self.composition).astype(numpy.float64), False)
		check_status(status)
		self.mod_geom.putNonTimed(path, cpopath)
		self.mod_neutr.putNonTimed(path, cpopath)
		self.mod_therm.putNonTimed(path, cpopath)
		self.mod_th_hyd.putNonTimed(path, cpopath)
		self.mod_mech.putNonTimed(path, cpopath)
		self.mod_lipb.putNonTimed(path, cpopath)
		self.mod_tritium.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructurehcllbb_specs, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_mass = ull.getVect1DDouble(self.idx, path, cpopath + 'mass')
		check_status(status)
		if not status:
			self.mass = ret_mass
		status, ret_dr = ull.getVect1DDouble(self.idx, path, cpopath + 'dr')
		check_status(status)
		if not status:
			self.dr = ret_dr
		status, ret_mat = ull.getVect1DDouble(self.idx, path, cpopath + 'mat')
		check_status(status)
		if not status:
			self.mat = ret_mat
		status, ret_composition = ull.getVect2DDouble(self.idx, path, cpopath + 'composition')
		check_status(status)
		if not status:
			self.composition = ret_composition
		self.mod_geom.getSlice(path, cpopath, inTime, interpolMode)
		self.mod_neutr.getSlice(path, cpopath, inTime, interpolMode)
		self.mod_therm.getSlice(path, cpopath, inTime, interpolMode)
		self.mod_th_hyd.getSlice(path, cpopath, inTime, interpolMode)
		self.mod_mech.getSlice(path, cpopath, inTime, interpolMode)
		self.mod_lipb.getSlice(path, cpopath, inTime, interpolMode)
		self.mod_tritium.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type outboardstructurehcllbb_specs, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, massVal = ull.getVect1DDouble(self.idx, path, cpopath + 'mass')
			check_status(status)
			status, drVal = ull.getVect1DDouble(self.idx, path, cpopath + 'dr')
			check_status(status)
			status, matVal = ull.getVect1DDouble(self.idx, path, cpopath + 'mat')
			check_status(status)
			status, compositionVal = ull.getVect2DDouble(self.idx, path, cpopath + 'composition')
			check_status(status)
			mod_geomList = self.mod_geom.build_non_resampled_data(path, cpopath, nbslice)
			mod_neutrList = self.mod_neutr.build_non_resampled_data(path, cpopath, nbslice)
			mod_thermList = self.mod_therm.build_non_resampled_data(path, cpopath, nbslice)
			mod_th_hydList = self.mod_th_hyd.build_non_resampled_data(path, cpopath, nbslice)
			mod_mechList = self.mod_mech.build_non_resampled_data(path, cpopath, nbslice)
			mod_lipbList = self.mod_lipb.build_non_resampled_data(path, cpopath, nbslice)
			mod_tritiumList = self.mod_tritium.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = outboardstructurehcllbb_specs(self.base_path)
				slice.setExpIdx(self.idx)
				slice.mass = massVal
				slice.dr = drVal
				slice.mat = matVal
				slice.composition = compositionVal
				slice.mod_geom = mod_geomList[i]
				slice.mod_neutr = mod_neutrList[i]
				slice.mod_therm = mod_thermList[i]
				slice.mod_th_hyd = mod_th_hydList[i]
				slice.mod_mech = mod_mechList[i]
				slice.mod_lipb = mod_lipbList[i]
				slice.mod_tritium = mod_tritiumList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructurehcllbb_specsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructurehcllbb_specsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructurehcllbb_specsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'mass') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'mass', i, numpy.array(self.mass).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dr') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dr', i, numpy.array(self.dr).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'mat') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'mat', i, numpy.array(self.mat).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'composition') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'composition', i, numpy.array(self.composition).astype(numpy.float64))
		obj = self.mod_geom.putNonTimedElt(path, cpopath + 'mod_geom', i, obj)
		obj = self.mod_neutr.putNonTimedElt(path, cpopath + 'mod_neutr', i, obj)
		obj = self.mod_therm.putNonTimedElt(path, cpopath + 'mod_therm', i, obj)
		obj = self.mod_th_hyd.putNonTimedElt(path, cpopath + 'mod_th_hyd', i, obj)
		obj = self.mod_mech.putNonTimedElt(path, cpopath + 'mod_mech', i, obj)
		obj = self.mod_lipb.putNonTimedElt(path, cpopath + 'mod_lipb', i, obj)
		obj = self.mod_tritium.putNonTimedElt(path, cpopath + 'mod_tritium', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outboardstructurehcllbb_specsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'mass') 
			print ('obj = ' + str(obj))
		status, ret_mass = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'mass', i)
		check_status(status)
		if not status:
			self.mass = ret_mass
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dr') 
			print ('obj = ' + str(obj))
		status, ret_dr = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dr', i)
		check_status(status)
		if not status:
			self.dr = ret_dr
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'mat') 
			print ('obj = ' + str(obj))
		status, ret_mat = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'mat', i)
		check_status(status)
		if not status:
			self.mat = ret_mat
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'composition') 
			print ('obj = ' + str(obj))
		status, ret_composition = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'composition', i)
		check_status(status)
		if not status:
			self.composition = ret_composition
		self.mod_geom.getNonTimedElt(path, cpopath + 'mod_geom', i, obj)
		self.mod_neutr.getNonTimedElt(path, cpopath + 'mod_neutr', i, obj)
		self.mod_therm.getNonTimedElt(path, cpopath + 'mod_therm', i, obj)
		self.mod_th_hyd.getNonTimedElt(path, cpopath + 'mod_th_hyd', i, obj)
		self.mod_mech.getNonTimedElt(path, cpopath + 'mod_mech', i, obj)
		self.mod_lipb.getNonTimedElt(path, cpopath + 'mod_lipb', i, obj)
		self.mod_tritium.getNonTimedElt(path, cpopath + 'mod_tritium', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'mass')
		ull.deleteData(self.idx, path, cpopath + 'dr')
		ull.deleteData(self.idx, path, cpopath + 'mat')
		ull.deleteData(self.idx, path, cpopath + 'composition')
		self.mod_geom.deleteData(path, cpopath)
		self.mod_neutr.deleteData(path, cpopath)
		self.mod_therm.deleteData(path, cpopath)
		self.mod_th_hyd.deleteData(path, cpopath)
		self.mod_mech.deleteData(path, cpopath)
		self.mod_lipb.deleteData(path, cpopath)
		self.mod_tritium.deleteData(path, cpopath)


class codeparamstructurecodeparam:
	'''
	class codeparamstructurecodeparam
	Code parameters

	Attributes:
	- codename : str
	   Name of the code
	- codeversion : str
	   Version of the code (as in the ITM repository)
	- parameters : str
	   List of the code specific parameters, string expected to be in XML format.
	- output_diag : str
	   List of the code specific diagnostic/output, string expected to be in XML format.
	- output_flag : int
	   Output flag : 0 means the run is successful, other values meaning some difficulty has been encountered, the exact meaning is then code specific. Negative values mean the result shall not be used. Exact rules could discussed and implemented in the module wrapper. Time-dependent.
	'''

	def __init__(self, base_path_in='codeparam'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.codename = ''
		self.codeversion = ''
		self.parameters = ''
		self.output_diag = ''
		self.output_flag = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class codeparamstructurecodeparam\n'
		ret = ret + space + 'Attribute codename: ' + str(self.codename) + '\n'
		ret = ret + space + 'Attribute codeversion: ' + str(self.codeversion) + '\n'
		ret = ret + space + 'Attribute parameters: ' + str(self.parameters) + '\n'
		ret = ret + space + 'Attribute output_diag: ' + str(self.output_diag) + '\n'
		ret = ret + space + 'Attribute output_flag: ' + str(self.output_flag) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type codeparamstructurecodeparam, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'output_flag', self.output_flag, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type codeparamstructurecodeparam, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'output_flag', self.output_flag)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type codeparamstructurecodeparam, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'codename', self.codename)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'codeversion', self.codeversion)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'parameters', self.parameters)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'output_diag', self.output_diag)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type codeparamstructurecodeparam, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_codename = ull.getString(self.idx, path, cpopath + 'codename')
		check_status(status)
		if not status:
			self.codename = ret_codename
		status, ret_codeversion = ull.getString(self.idx, path, cpopath + 'codeversion')
		check_status(status)
		if not status:
			self.codeversion = ret_codeversion
		status, ret_parameters = ull.getString(self.idx, path, cpopath + 'parameters')
		check_status(status)
		if not status:
			self.parameters = ret_parameters
		status, ret_output_diag = ull.getString(self.idx, path, cpopath + 'output_diag')
		check_status(status)
		if not status:
			self.output_diag = ret_output_diag
		status, ret_output_flag, retTime = ull.getIntSlice(self.idx, path, cpopath + 'output_flag', inTime, interpolMode)
		check_status(status)
		if not status:
			self.output_flag = ret_output_flag
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type codeparamstructurecodeparam, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, codenameVal = ull.getString(self.idx, path, cpopath + 'codename')
			check_status(status)
			status, codeversionVal = ull.getString(self.idx, path, cpopath + 'codeversion')
			check_status(status)
			status, parametersVal = ull.getString(self.idx, path, cpopath + 'parameters')
			check_status(status)
			status, output_diagVal = ull.getString(self.idx, path, cpopath + 'output_diag')
			check_status(status)
			status, output_flagList = ull.getVect1DInt(self.idx, path, cpopath + 'output_flag')
			if len(output_flagList) == 0:
				output_flagList = numpy.resize(output_flagList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = codeparamstructurecodeparam(self.base_path)
				slice.setExpIdx(self.idx)
				slice.codename = codenameVal
				slice.codeversion = codeversionVal
				slice.parameters = parametersVal
				slice.output_diag = output_diagVal
				slice.output_flag = int(output_flagList[i].copy())
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type codeparamstructurecodeparamObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'output_flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'output_flag', i, self.output_flag)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type codeparamstructurecodeparamObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'output_flag') 
			print ('obj = ' + str(obj))
		status, ret_output_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'output_flag', i)
		check_status(status)
		if not status:
			self.output_flag = ret_output_flag

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type codeparamstructurecodeparamObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'codename') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'codename', i, self.codename)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'codeversion') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'codeversion', i, self.codeversion)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'parameters') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'parameters', i, self.parameters)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'output_diag') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'output_diag', i, self.output_diag)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type codeparamstructurecodeparamObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'codename') 
			print ('obj = ' + str(obj))
		status, ret_codename = ull.getStringFromObject(self.idx, obj, cpopath + 'codename', i)
		check_status(status)
		if not status:
			self.codename = ret_codename
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'codeversion') 
			print ('obj = ' + str(obj))
		status, ret_codeversion = ull.getStringFromObject(self.idx, obj, cpopath + 'codeversion', i)
		check_status(status)
		if not status:
			self.codeversion = ret_codeversion
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'parameters') 
			print ('obj = ' + str(obj))
		status, ret_parameters = ull.getStringFromObject(self.idx, obj, cpopath + 'parameters', i)
		check_status(status)
		if not status:
			self.parameters = ret_parameters
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'output_diag') 
			print ('obj = ' + str(obj))
		status, ret_output_diag = ull.getStringFromObject(self.idx, obj, cpopath + 'output_diag', i)
		check_status(status)
		if not status:
			self.output_diag = ret_output_diag

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'codename')
		ull.deleteData(self.idx, path, cpopath + 'codeversion')
		ull.deleteData(self.idx, path, cpopath + 'parameters')
		ull.deleteData(self.idx, path, cpopath + 'output_diag')
		ull.deleteData(self.idx, path, cpopath + 'output_flag')
