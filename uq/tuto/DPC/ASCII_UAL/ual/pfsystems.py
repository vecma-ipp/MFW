# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class pfsystems(KeepInOrder):
	'''
	class pfsystems
	Description of the active poloidal coils, passive conductors, currents flowing in those and mutual electromagnetic effects of the device; Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- pfcoils : class pfcoilsstructurepfcoils
	   Active poloidal field coils
	- pfpassive : class pfpassivestructurepfpassive
	   Passive axisymmetric conductor description
	- pfcircuits : class pfcircuitsstructurepfcircuits
	   Circuits, connected to multiple coils and to multiple supplies, defining the current and voltage relationships in the system
	- pfsupplies : class pfsuppliesstructurepfsupplies
	   PF power supplies
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar
	'''

	def __init__(self):
		self.base_path = 'pfsystems'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 3
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.pfcoils = pfcoilsstructurepfcoils('pfcoils')
		self.pfpassive = pfpassivestructurepfpassive('pfpassive')
		self.pfcircuits = pfcircuitsstructurepfcircuits('pfcircuits')
		self.pfsupplies = pfsuppliesstructurepfsupplies('pfsupplies')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pfsystems\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute pfcoils\n ' + self.pfcoils.__str__(depth+1)
		ret = ret + space + 'Attribute pfpassive\n ' + self.pfpassive.__str__(depth+1)
		ret = ret + space + 'Attribute pfcircuits\n ' + self.pfcircuits.__str__(depth+1)
		ret = ret + space + 'Attribute pfsupplies\n ' + self.pfsupplies.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.pfcoils.setExpIdx(idx)
		self.pfpassive.setExpIdx(idx)
		self.pfcircuits.setExpIdx(idx)
		self.pfsupplies.setExpIdx(idx)
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
		self.pfcoils.cpoTime = self.cpoTime
		self.pfcoils.putSlice(path, cpopath)
		self.pfpassive.cpoTime = self.cpoTime
		self.pfpassive.putSlice(path, cpopath)
		self.pfcircuits.cpoTime = self.cpoTime
		self.pfcircuits.putSlice(path, cpopath)
		self.pfsupplies.cpoTime = self.cpoTime
		self.pfsupplies.putSlice(path, cpopath)
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
		self.pfcoils.replaceLastSlice(path, cpopath)
		self.pfpassive.replaceLastSlice(path, cpopath)
		self.pfcircuits.replaceLastSlice(path, cpopath)
		self.pfsupplies.replaceLastSlice(path, cpopath)
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
		self.pfcoils.putNonTimed(path, cpopath)
		self.pfpassive.putNonTimed(path, cpopath)
		self.pfcircuits.putNonTimed(path, cpopath)
		self.pfsupplies.putNonTimed(path, cpopath)
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
		self.pfcoils.getSlice(path, cpopath, inTime, interpolMode)
		self.pfpassive.getSlice(path, cpopath, inTime, interpolMode)
		self.pfcircuits.getSlice(path, cpopath, inTime, interpolMode)
		self.pfsupplies.getSlice(path, cpopath, inTime, interpolMode)
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
			pfcoilsList = self.pfcoils.build_non_resampled_data(path, cpopath, nbslice)
			pfpassiveList = self.pfpassive.build_non_resampled_data(path, cpopath, nbslice)
			pfcircuitsList = self.pfcircuits.build_non_resampled_data(path, cpopath, nbslice)
			pfsuppliesList = self.pfsupplies.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = pfsystems()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.pfcoils = pfcoilsList[i]
				slice.pfpassive = pfpassiveList[i]
				slice.pfcircuits = pfcircuitsList[i]
				slice.pfsupplies = pfsuppliesList[i]
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
		self.pfcoils.deleteData(path, cpopath)
		self.pfpassive.deleteData(path, cpopath)
		self.pfcircuits.deleteData(path, cpopath)
		self.pfsupplies.deleteData(path, cpopath)
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class pfsystemsArray:
	'''
	class pfsystemsArray
	Description of the active poloidal coils, passive conductors, currents flowing in those and mutual electromagnetic effects of the device; Time-dependent CPO

	Attributes:
	- array : list of pfsystems
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
		ret = space + 'class pfsystemsArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'pfsystems cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = pfsystems()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(pfsystems())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = pfsystems()
		cpo.setExpIdx(self.idx)
		cpo.deleteData(occurrence)


class datainfostructuredatainfo(KeepInOrder):
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


class whatrefstructurewhatref(KeepInOrder):
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


class putinfostructureputinfo(KeepInOrder):
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


class pfcoilsstructurepfcoils(KeepInOrder):
	'''
	class pfcoilsstructurepfcoils
	Active poloidal field coils

	Attributes:
	- desc_pfcoils : class desc_pfcoilsstructuredesc_pfcoils
	   Description of the coils
	- coilcurrent : class coilcurrentstructureexp1D
	   Circuit feed current in the coil, defined positive if it flows from point 1 to point 2 of the component in the pfcircuit description [A]; Time-dependent; Vector (ncoils)
	- coilvoltage : class coilvoltagestructureexp1D
	   Voltage on the full coil [V]; Time-dependent; Vector (ncoils)
	- p_cryo : float
	   Total electric power consumed by the cryoplant system [W]; Time-dependent. Scalar.
	- p_nh : numpy.ndarray 1D with float
	   Nuclear heating on the poloidal field coils [W]; Time-dependent. Vector(ncoils)
	'''

	def __init__(self, base_path_in='pfcoils'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.desc_pfcoils = desc_pfcoilsstructuredesc_pfcoils('desc_pfcoils')
		self.coilcurrent = coilcurrentstructureexp1D('coilcurrent')
		self.coilvoltage = coilvoltagestructureexp1D('coilvoltage')
		self.p_cryo = EMPTY_DOUBLE
		self.p_nh = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pfcoilsstructurepfcoils\n'
		ret = ret + space + 'Attribute desc_pfcoils\n ' + self.desc_pfcoils.__str__(depth+1)
		ret = ret + space + 'Attribute coilcurrent\n ' + self.coilcurrent.__str__(depth+1)
		ret = ret + space + 'Attribute coilvoltage\n ' + self.coilvoltage.__str__(depth+1)
		ret = ret + space + 'Attribute p_cryo: ' + str(self.p_cryo) + '\n'
		s = self.p_nh.__str__()
		ret = ret + space + 'Attribute p_nh\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.desc_pfcoils.setExpIdx(idx)
		self.coilcurrent.setExpIdx(idx)
		self.coilvoltage.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfcoilsstructurepfcoils, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.desc_pfcoils.cpoTime = self.cpoTime
		self.desc_pfcoils.putSlice(path, cpopath)
		self.coilcurrent.cpoTime = self.cpoTime
		self.coilcurrent.putSlice(path, cpopath)
		self.coilvoltage.cpoTime = self.cpoTime
		self.coilvoltage.putSlice(path, cpopath)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'p_cryo', self.p_cryo, self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'p_nh', numpy.array(self.p_nh).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfcoilsstructurepfcoils, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.desc_pfcoils.replaceLastSlice(path, cpopath)
		self.coilcurrent.replaceLastSlice(path, cpopath)
		self.coilvoltage.replaceLastSlice(path, cpopath)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'p_cryo', self.p_cryo)
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'p_nh', numpy.array(self.p_nh).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfcoilsstructurepfcoils, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.desc_pfcoils.putNonTimed(path, cpopath)
		self.coilcurrent.putNonTimed(path, cpopath)
		self.coilvoltage.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pfcoilsstructurepfcoils, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.desc_pfcoils.getSlice(path, cpopath, inTime, interpolMode)
		self.coilcurrent.getSlice(path, cpopath, inTime, interpolMode)
		self.coilvoltage.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_p_cryo, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'p_cryo', inTime, interpolMode)
		check_status(status)
		if not status:
			self.p_cryo = ret_p_cryo
			self.cpoTime = retTime
		status, ret_p_nh, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'p_nh', inTime, interpolMode)
		check_status(status)
		if not status:
			self.p_nh = ret_p_nh
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pfcoilsstructurepfcoils, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			desc_pfcoilsList = self.desc_pfcoils.build_non_resampled_data(path, cpopath, nbslice)
			coilcurrentList = self.coilcurrent.build_non_resampled_data(path, cpopath, nbslice)
			coilvoltageList = self.coilvoltage.build_non_resampled_data(path, cpopath, nbslice)
			status, p_cryoList = ull.getVect1DDouble(self.idx, path, cpopath + 'p_cryo')
			if len(p_cryoList) == 0:
				p_cryoList = numpy.resize(p_cryoList, (nbslice))
			check_status(status)
			status, p_nhList = ull.getVect2DDouble(self.idx, path, cpopath + 'p_nh')
			if len(p_nhList) == 0:
				p_nhList = numpy.resize(p_nhList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = pfcoilsstructurepfcoils(self.base_path)
				slice.setExpIdx(self.idx)
				slice.desc_pfcoils = desc_pfcoilsList[i]
				slice.coilcurrent = coilcurrentList[i]
				slice.coilvoltage = coilvoltageList[i]
				slice.p_cryo = p_cryoList[i].copy().astype(float)
				slice.p_nh = p_nhList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfcoilsstructurepfcoilsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.coilcurrent.putTimedElt(path, cpopath + 'coilcurrent', i, obj)
		obj = self.coilvoltage.putTimedElt(path, cpopath + 'coilvoltage', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'p_cryo') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'p_cryo', i, self.p_cryo)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'p_nh') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'p_nh', i, numpy.array(self.p_nh).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfcoilsstructurepfcoilsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.coilcurrent.getTimedElt(path, cpopath + 'coilcurrent', i, obj)
		self.coilvoltage.getTimedElt(path, cpopath + 'coilvoltage', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'p_cryo') 
			print ('obj = ' + str(obj))
		status, ret_p_cryo = ull.getDoubleFromObject(self.idx, obj, cpopath + 'p_cryo', i)
		check_status(status)
		if not status:
			self.p_cryo = ret_p_cryo
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'p_nh') 
			print ('obj = ' + str(obj))
		status, ret_p_nh = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'p_nh', i)
		check_status(status)
		if not status:
			self.p_nh = ret_p_nh

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfcoilsstructurepfcoilsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.desc_pfcoils.putNonTimedElt(path, cpopath + 'desc_pfcoils', i, obj)
		obj = self.coilcurrent.putNonTimedElt(path, cpopath + 'coilcurrent', i, obj)
		obj = self.coilvoltage.putNonTimedElt(path, cpopath + 'coilvoltage', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfcoilsstructurepfcoilsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.desc_pfcoils.getNonTimedElt(path, cpopath + 'desc_pfcoils', i, obj)
		self.coilcurrent.getNonTimedElt(path, cpopath + 'coilcurrent', i, obj)
		self.coilvoltage.getNonTimedElt(path, cpopath + 'coilvoltage', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.desc_pfcoils.deleteData(path, cpopath)
		self.coilcurrent.deleteData(path, cpopath)
		self.coilvoltage.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'p_cryo')
		ull.deleteData(self.idx, path, cpopath + 'p_nh')


class desc_pfcoilsstructuredesc_pfcoils(KeepInOrder):
	'''
	class desc_pfcoilsstructuredesc_pfcoils
	Description of the coils

	Attributes:
	- name : list of str
	   Name of coil. Array of strings (ncoils)
	- id : list of str
	   ID of coil. Array of strings (ncoils)
	- res : numpy.ndarray 1D with float
	   Coil resistance [Ohm]; Vector (ncoils)
	- emax : numpy.ndarray 1D with float
	   Maximum Energy to be dissipated in coils [J]; Vector (ncoils)
	- structure_cs : class structure_csstructurestructure_cs
	   Detailed description of the coil structure, for coils that are part of the central solenoid.
	- pol_flux_cs : float
	   Maximum poloidal flux available in the Central Solenoid for a plasma pulse [Wb].
	- nelement : numpy.ndarray 1D with int)
	   Number of elements used to describe a coil; Vector (ncoils)
	- pfelement : class pfelementstructurepfelement
	   Axisymmetric conductor description
	'''

	def __init__(self, base_path_in='desc_pfcoils'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ['']
		self.id = ['']
		self.res = numpy.zeros(0, numpy.float64, order='C')
		self.emax = numpy.zeros(0, numpy.float64, order='C')
		self.structure_cs = structure_csstructurestructure_cs('structure_cs')
		self.pol_flux_cs = EMPTY_DOUBLE
		self.nelement = numpy.zeros(0, numpy.int32, order='C')
		self.pfelement = pfelementstructurepfelement('pfelement')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class desc_pfcoilsstructuredesc_pfcoils\n'
		s = self.name.__str__()
		ret = ret + space + 'Attribute name\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.id.__str__()
		ret = ret + space + 'Attribute id\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.res.__str__()
		ret = ret + space + 'Attribute res\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.emax.__str__()
		ret = ret + space + 'Attribute emax\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute structure_cs\n ' + self.structure_cs.__str__(depth+1)
		ret = ret + space + 'Attribute pol_flux_cs: ' + str(self.pol_flux_cs) + '\n'
		s = self.nelement.__str__()
		ret = ret + space + 'Attribute nelement\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute pfelement\n ' + self.pfelement.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.structure_cs.setExpIdx(idx)
		self.pfelement.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type desc_pfcoilsstructuredesc_pfcoils, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.structure_cs.cpoTime = self.cpoTime
		self.structure_cs.putSlice(path, cpopath)
		self.pfelement.cpoTime = self.cpoTime
		self.pfelement.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type desc_pfcoilsstructuredesc_pfcoils, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.structure_cs.replaceLastSlice(path, cpopath)
		self.pfelement.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type desc_pfcoilsstructuredesc_pfcoils, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'name', self.name, False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'id', self.id, False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'res', numpy.array(self.res).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'emax', numpy.array(self.emax).astype(numpy.float64), False)
		check_status(status)
		self.structure_cs.putNonTimed(path, cpopath)
		status = ull.putDouble(self.idx, path, cpopath + 'pol_flux_cs', self.pol_flux_cs)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'nelement', numpy.array(self.nelement).astype(numpy.int32), False)
		check_status(status)
		self.pfelement.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type desc_pfcoilsstructuredesc_pfcoils, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_name = ull.getVect1DString(self.idx, path, cpopath + 'name')
		check_status(status)
		if not status:
			self.name = ret_name
		status, ret_id = ull.getVect1DString(self.idx, path, cpopath + 'id')
		check_status(status)
		if not status:
			self.id = ret_id
		status, ret_res = ull.getVect1DDouble(self.idx, path, cpopath + 'res')
		check_status(status)
		if not status:
			self.res = ret_res
		status, ret_emax = ull.getVect1DDouble(self.idx, path, cpopath + 'emax')
		check_status(status)
		if not status:
			self.emax = ret_emax
		self.structure_cs.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_pol_flux_cs = ull.getDouble(self.idx, path, cpopath + 'pol_flux_cs')
		check_status(status)
		if not status:
			self.pol_flux_cs = ret_pol_flux_cs
		status, ret_nelement = ull.getVect1DInt(self.idx, path, cpopath + 'nelement')
		check_status(status)
		if not status:
			self.nelement = ret_nelement
		self.pfelement.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type desc_pfcoilsstructuredesc_pfcoils, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nameVal = ull.getVect1DString(self.idx, path, cpopath + 'name')
			check_status(status)
			status, idVal = ull.getVect1DString(self.idx, path, cpopath + 'id')
			check_status(status)
			status, resVal = ull.getVect1DDouble(self.idx, path, cpopath + 'res')
			check_status(status)
			status, emaxVal = ull.getVect1DDouble(self.idx, path, cpopath + 'emax')
			check_status(status)
			structure_csList = self.structure_cs.build_non_resampled_data(path, cpopath, nbslice)
			status, pol_flux_csVal = ull.getDouble(self.idx, path, cpopath + 'pol_flux_cs')
			check_status(status)
			status, nelementVal = ull.getVect1DInt(self.idx, path, cpopath + 'nelement')
			check_status(status)
			pfelementList = self.pfelement.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = desc_pfcoilsstructuredesc_pfcoils(self.base_path)
				slice.setExpIdx(self.idx)
				slice.name = nameVal
				slice.id = idVal
				slice.res = resVal
				slice.emax = emaxVal
				slice.structure_cs = structure_csList[i]
				slice.pol_flux_cs = pol_flux_csVal
				slice.nelement = nelementVal
				slice.pfelement = pfelementList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_pfcoilsstructuredesc_pfcoilsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_pfcoilsstructuredesc_pfcoilsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_pfcoilsstructuredesc_pfcoilsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'res') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'res', i, numpy.array(self.res).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'emax') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'emax', i, numpy.array(self.emax).astype(numpy.float64))
		obj = self.structure_cs.putNonTimedElt(path, cpopath + 'structure_cs', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pol_flux_cs') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pol_flux_cs', i, self.pol_flux_cs)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'nelement') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'nelement', i, numpy.array(self.nelement).astype(numpy.int32))
		obj = self.pfelement.putNonTimedElt(path, cpopath + 'pfelement', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_pfcoilsstructuredesc_pfcoilsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'res') 
			print ('obj = ' + str(obj))
		status, ret_res = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'res', i)
		check_status(status)
		if not status:
			self.res = ret_res
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'emax') 
			print ('obj = ' + str(obj))
		status, ret_emax = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'emax', i)
		check_status(status)
		if not status:
			self.emax = ret_emax
		self.structure_cs.getNonTimedElt(path, cpopath + 'structure_cs', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pol_flux_cs') 
			print ('obj = ' + str(obj))
		status, ret_pol_flux_cs = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pol_flux_cs', i)
		check_status(status)
		if not status:
			self.pol_flux_cs = ret_pol_flux_cs
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'nelement') 
			print ('obj = ' + str(obj))
		status, ret_nelement = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'nelement', i)
		check_status(status)
		if not status:
			self.nelement = ret_nelement
		self.pfelement.getNonTimedElt(path, cpopath + 'pfelement', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'name')
		ull.deleteData(self.idx, path, cpopath + 'id')
		ull.deleteData(self.idx, path, cpopath + 'res')
		ull.deleteData(self.idx, path, cpopath + 'emax')
		self.structure_cs.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'pol_flux_cs')
		ull.deleteData(self.idx, path, cpopath + 'nelement')
		self.pfelement.deleteData(path, cpopath)


class structure_csstructurestructure_cs(KeepInOrder):
	'''
	class structure_csstructurestructure_cs
	Detailed description of the coil structure, for coils that are part of the central solenoid.

	Attributes:
	- gaptf : float
	   gap between CS external radius and TF internal vault radius [m]; Scalar
	- ri : float
	   CS internal radius [m]; Scalar
	- re : float
	   CS external radius [m]; Scalar
	- jcable : float
	   Maximum allowable CS Cable In Conduit current density [A/m^2]; Scalar
	- current_nom : float
	   Nominal current in the CS conductor [A]; Scalar
	- sigma : float
	   Maximum allowable stress in the CS [Pa]; Scalar
	- tiso : float
	   Insulation thickness of CS conductor [m]; Scalar
	- nlay : float
	   Number of conductor layers in the Central Solenoid; Scalar
	'''

	def __init__(self, base_path_in='structure_cs'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.gaptf = EMPTY_DOUBLE
		self.ri = EMPTY_DOUBLE
		self.re = EMPTY_DOUBLE
		self.jcable = EMPTY_DOUBLE
		self.current_nom = EMPTY_DOUBLE
		self.sigma = EMPTY_DOUBLE
		self.tiso = EMPTY_DOUBLE
		self.nlay = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class structure_csstructurestructure_cs\n'
		ret = ret + space + 'Attribute gaptf: ' + str(self.gaptf) + '\n'
		ret = ret + space + 'Attribute ri: ' + str(self.ri) + '\n'
		ret = ret + space + 'Attribute re: ' + str(self.re) + '\n'
		ret = ret + space + 'Attribute jcable: ' + str(self.jcable) + '\n'
		ret = ret + space + 'Attribute current_nom: ' + str(self.current_nom) + '\n'
		ret = ret + space + 'Attribute sigma: ' + str(self.sigma) + '\n'
		ret = ret + space + 'Attribute tiso: ' + str(self.tiso) + '\n'
		ret = ret + space + 'Attribute nlay: ' + str(self.nlay) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type structure_csstructurestructure_cs, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type structure_csstructurestructure_cs, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type structure_csstructurestructure_cs, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'gaptf', self.gaptf)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'ri', self.ri)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 're', self.re)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'jcable', self.jcable)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'current_nom', self.current_nom)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'sigma', self.sigma)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'tiso', self.tiso)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'nlay', self.nlay)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type structure_csstructurestructure_cs, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_gaptf = ull.getDouble(self.idx, path, cpopath + 'gaptf')
		check_status(status)
		if not status:
			self.gaptf = ret_gaptf
		status, ret_ri = ull.getDouble(self.idx, path, cpopath + 'ri')
		check_status(status)
		if not status:
			self.ri = ret_ri
		status, ret_re = ull.getDouble(self.idx, path, cpopath + 're')
		check_status(status)
		if not status:
			self.re = ret_re
		status, ret_jcable = ull.getDouble(self.idx, path, cpopath + 'jcable')
		check_status(status)
		if not status:
			self.jcable = ret_jcable
		status, ret_current_nom = ull.getDouble(self.idx, path, cpopath + 'current_nom')
		check_status(status)
		if not status:
			self.current_nom = ret_current_nom
		status, ret_sigma = ull.getDouble(self.idx, path, cpopath + 'sigma')
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		status, ret_tiso = ull.getDouble(self.idx, path, cpopath + 'tiso')
		check_status(status)
		if not status:
			self.tiso = ret_tiso
		status, ret_nlay = ull.getDouble(self.idx, path, cpopath + 'nlay')
		check_status(status)
		if not status:
			self.nlay = ret_nlay

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type structure_csstructurestructure_cs, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, gaptfVal = ull.getDouble(self.idx, path, cpopath + 'gaptf')
			check_status(status)
			status, riVal = ull.getDouble(self.idx, path, cpopath + 'ri')
			check_status(status)
			status, reVal = ull.getDouble(self.idx, path, cpopath + 're')
			check_status(status)
			status, jcableVal = ull.getDouble(self.idx, path, cpopath + 'jcable')
			check_status(status)
			status, current_nomVal = ull.getDouble(self.idx, path, cpopath + 'current_nom')
			check_status(status)
			status, sigmaVal = ull.getDouble(self.idx, path, cpopath + 'sigma')
			check_status(status)
			status, tisoVal = ull.getDouble(self.idx, path, cpopath + 'tiso')
			check_status(status)
			status, nlayVal = ull.getDouble(self.idx, path, cpopath + 'nlay')
			check_status(status)
			for i in range(nbslice):
				slice = structure_csstructurestructure_cs(self.base_path)
				slice.setExpIdx(self.idx)
				slice.gaptf = gaptfVal
				slice.ri = riVal
				slice.re = reVal
				slice.jcable = jcableVal
				slice.current_nom = current_nomVal
				slice.sigma = sigmaVal
				slice.tiso = tisoVal
				slice.nlay = nlayVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type structure_csstructurestructure_csObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type structure_csstructurestructure_csObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type structure_csstructurestructure_csObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'gaptf') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'gaptf', i, self.gaptf)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ri') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ri', i, self.ri)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 're') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 're', i, self.re)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'jcable') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'jcable', i, self.jcable)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'current_nom') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'current_nom', i, self.current_nom)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sigma', i, self.sigma)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'tiso') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'tiso', i, self.tiso)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'nlay') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'nlay', i, self.nlay)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type structure_csstructurestructure_csObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'gaptf') 
			print ('obj = ' + str(obj))
		status, ret_gaptf = ull.getDoubleFromObject(self.idx, obj, cpopath + 'gaptf', i)
		check_status(status)
		if not status:
			self.gaptf = ret_gaptf
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ri') 
			print ('obj = ' + str(obj))
		status, ret_ri = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ri', i)
		check_status(status)
		if not status:
			self.ri = ret_ri
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 're') 
			print ('obj = ' + str(obj))
		status, ret_re = ull.getDoubleFromObject(self.idx, obj, cpopath + 're', i)
		check_status(status)
		if not status:
			self.re = ret_re
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'jcable') 
			print ('obj = ' + str(obj))
		status, ret_jcable = ull.getDoubleFromObject(self.idx, obj, cpopath + 'jcable', i)
		check_status(status)
		if not status:
			self.jcable = ret_jcable
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'current_nom') 
			print ('obj = ' + str(obj))
		status, ret_current_nom = ull.getDoubleFromObject(self.idx, obj, cpopath + 'current_nom', i)
		check_status(status)
		if not status:
			self.current_nom = ret_current_nom
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'tiso') 
			print ('obj = ' + str(obj))
		status, ret_tiso = ull.getDoubleFromObject(self.idx, obj, cpopath + 'tiso', i)
		check_status(status)
		if not status:
			self.tiso = ret_tiso
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'nlay') 
			print ('obj = ' + str(obj))
		status, ret_nlay = ull.getDoubleFromObject(self.idx, obj, cpopath + 'nlay', i)
		check_status(status)
		if not status:
			self.nlay = ret_nlay

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'gaptf')
		ull.deleteData(self.idx, path, cpopath + 'ri')
		ull.deleteData(self.idx, path, cpopath + 're')
		ull.deleteData(self.idx, path, cpopath + 'jcable')
		ull.deleteData(self.idx, path, cpopath + 'current_nom')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'tiso')
		ull.deleteData(self.idx, path, cpopath + 'nlay')


class pfelementstructurepfelement(KeepInOrder):
	'''
	class pfelementstructurepfelement
	Axisymmetric conductor description

	Attributes:
	- name : list of str
	   Name of this element. Should be a matrix of strings (ncoils,max_nelements), but not supported by the UAL yet.
	- id : list of str
	   ID of this element. Should be a matrix of strings (ncoils,max_nelements), but not supported by the UAL yet.
	- turnsign : numpy.ndarray 2D with float
	   Sign of turn and fraction of a turn for calculating magnetic field of the Element; Matrix (ncoils,max_nelements)
	- area : numpy.ndarray 2D with float
	   Surface area of this element [m^2]; Matrix (ncoils,max_nelements)
	- pfgeometry : class pfgeometrystructurepfgeometry
	   Shape of a PF Coil Element
	'''

	def __init__(self, base_path_in='pfelement'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ['']
		self.id = ['']
		self.turnsign = numpy.zeros((0,0), numpy.float64, order='C')
		self.area = numpy.zeros((0,0), numpy.float64, order='C')
		self.pfgeometry = pfgeometrystructurepfgeometry('pfgeometry')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pfelementstructurepfelement\n'
		s = self.name.__str__()
		ret = ret + space + 'Attribute name\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.id.__str__()
		ret = ret + space + 'Attribute id\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.turnsign.__str__()
		ret = ret + space + 'Attribute turnsign\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.area.__str__()
		ret = ret + space + 'Attribute area\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute pfgeometry\n ' + self.pfgeometry.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.pfgeometry.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfelementstructurepfelement, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.pfgeometry.cpoTime = self.cpoTime
		self.pfgeometry.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfelementstructurepfelement, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.pfgeometry.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfelementstructurepfelement, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'name', self.name, False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'id', self.id, False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'turnsign', numpy.array(self.turnsign).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'area', numpy.array(self.area).astype(numpy.float64), False)
		check_status(status)
		self.pfgeometry.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pfelementstructurepfelement, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_name = ull.getVect1DString(self.idx, path, cpopath + 'name')
		check_status(status)
		if not status:
			self.name = ret_name
		status, ret_id = ull.getVect1DString(self.idx, path, cpopath + 'id')
		check_status(status)
		if not status:
			self.id = ret_id
		status, ret_turnsign = ull.getVect2DDouble(self.idx, path, cpopath + 'turnsign')
		check_status(status)
		if not status:
			self.turnsign = ret_turnsign
		status, ret_area = ull.getVect2DDouble(self.idx, path, cpopath + 'area')
		check_status(status)
		if not status:
			self.area = ret_area
		self.pfgeometry.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pfelementstructurepfelement, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nameVal = ull.getVect1DString(self.idx, path, cpopath + 'name')
			check_status(status)
			status, idVal = ull.getVect1DString(self.idx, path, cpopath + 'id')
			check_status(status)
			status, turnsignVal = ull.getVect2DDouble(self.idx, path, cpopath + 'turnsign')
			check_status(status)
			status, areaVal = ull.getVect2DDouble(self.idx, path, cpopath + 'area')
			check_status(status)
			pfgeometryList = self.pfgeometry.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = pfelementstructurepfelement(self.base_path)
				slice.setExpIdx(self.idx)
				slice.name = nameVal
				slice.id = idVal
				slice.turnsign = turnsignVal
				slice.area = areaVal
				slice.pfgeometry = pfgeometryList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfelementstructurepfelementObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfelementstructurepfelementObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfelementstructurepfelementObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'turnsign') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'turnsign', i, numpy.array(self.turnsign).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'area') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'area', i, numpy.array(self.area).astype(numpy.float64))
		obj = self.pfgeometry.putNonTimedElt(path, cpopath + 'pfgeometry', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfelementstructurepfelementObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'turnsign') 
			print ('obj = ' + str(obj))
		status, ret_turnsign = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'turnsign', i)
		check_status(status)
		if not status:
			self.turnsign = ret_turnsign
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'area') 
			print ('obj = ' + str(obj))
		status, ret_area = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'area', i)
		check_status(status)
		if not status:
			self.area = ret_area
		self.pfgeometry.getNonTimedElt(path, cpopath + 'pfgeometry', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'name')
		ull.deleteData(self.idx, path, cpopath + 'id')
		ull.deleteData(self.idx, path, cpopath + 'turnsign')
		ull.deleteData(self.idx, path, cpopath + 'area')
		self.pfgeometry.deleteData(path, cpopath)


class pfgeometrystructurepfgeometry(KeepInOrder):
	'''
	class pfgeometrystructurepfgeometry
	Shape of a PF Coil Element

	Attributes:
	- type : numpy.ndarray 2D with int
	   Type used to describe a coil shape (0 for 'rzcoordinates' or 1 for 'rzdrdz'); Matrix of integers (ncoils,max_nelements)
	- npoints : numpy.ndarray 2D with int
	   Number of points describing an element (irregular outline rzcoordinates); Matrix (ncoils,max_nelements)
	- rzcoordinate : class rzcoordinatestructurerz3D
	   Irregular outline [m]; 3D arrays (ncoils,max_nelements,max_npoints)
	- rzdrdz : numpy.ndarray 3D with float
	   4-vector defining Centre R,Z and full extents dR, dZ [m]; 3D Array (ncoils,max_nelements,4)
	'''

	def __init__(self, base_path_in='pfgeometry'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.type = numpy.zeros((0,0), numpy.int32, order='C')
		self.npoints = numpy.zeros((0,0), numpy.int32, order='C')
		self.rzcoordinate = rzcoordinatestructurerz3D('rzcoordinate')
		self.rzdrdz = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pfgeometrystructurepfgeometry\n'
		s = self.type.__str__()
		ret = ret + space + 'Attribute type\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.npoints.__str__()
		ret = ret + space + 'Attribute npoints\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute rzcoordinate\n ' + self.rzcoordinate.__str__(depth+1)
		s = self.rzdrdz.__str__()
		ret = ret + space + 'Attribute rzdrdz\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.rzcoordinate.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfgeometrystructurepfgeometry, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.rzcoordinate.cpoTime = self.cpoTime
		self.rzcoordinate.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfgeometrystructurepfgeometry, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.rzcoordinate.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfgeometrystructurepfgeometry, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DInt(self.idx, path, cpopath + 'type', numpy.array(self.type).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect2DInt(self.idx, path, cpopath + 'npoints', numpy.array(self.npoints).astype(numpy.int32), False)
		check_status(status)
		self.rzcoordinate.putNonTimed(path, cpopath)
		status = ull.putVect3DDouble(self.idx, path, cpopath + 'rzdrdz', numpy.array(self.rzdrdz).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pfgeometrystructurepfgeometry, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_type = ull.getVect2DInt(self.idx, path, cpopath + 'type')
		check_status(status)
		if not status:
			self.type = ret_type
		status, ret_npoints = ull.getVect2DInt(self.idx, path, cpopath + 'npoints')
		check_status(status)
		if not status:
			self.npoints = ret_npoints
		self.rzcoordinate.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_rzdrdz = ull.getVect3DDouble(self.idx, path, cpopath + 'rzdrdz')
		check_status(status)
		if not status:
			self.rzdrdz = ret_rzdrdz

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pfgeometrystructurepfgeometry, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, typeVal = ull.getVect2DInt(self.idx, path, cpopath + 'type')
			check_status(status)
			status, npointsVal = ull.getVect2DInt(self.idx, path, cpopath + 'npoints')
			check_status(status)
			rzcoordinateList = self.rzcoordinate.build_non_resampled_data(path, cpopath, nbslice)
			status, rzdrdzVal = ull.getVect3DDouble(self.idx, path, cpopath + 'rzdrdz')
			check_status(status)
			for i in range(nbslice):
				slice = pfgeometrystructurepfgeometry(self.base_path)
				slice.setExpIdx(self.idx)
				slice.type = typeVal
				slice.npoints = npointsVal
				slice.rzcoordinate = rzcoordinateList[i]
				slice.rzdrdz = rzdrdzVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfgeometrystructurepfgeometryObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfgeometrystructurepfgeometryObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfgeometrystructurepfgeometryObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'type', i, numpy.array(self.type).astype(numpy.int32))
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'npoints') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'npoints', i, numpy.array(self.npoints).astype(numpy.int32))
		obj = self.rzcoordinate.putNonTimedElt(path, cpopath + 'rzcoordinate', i, obj)
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'rzdrdz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'rzdrdz', i, numpy.array(self.rzdrdz).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfgeometrystructurepfgeometryObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		status, ret_type = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'type', i)
		check_status(status)
		if not status:
			self.type = ret_type
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'npoints') 
			print ('obj = ' + str(obj))
		status, ret_npoints = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'npoints', i)
		check_status(status)
		if not status:
			self.npoints = ret_npoints
		self.rzcoordinate.getNonTimedElt(path, cpopath + 'rzcoordinate', i, obj)
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'rzdrdz') 
			print ('obj = ' + str(obj))
		status, ret_rzdrdz = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'rzdrdz', i)
		check_status(status)
		if not status:
			self.rzdrdz = ret_rzdrdz

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'type')
		ull.deleteData(self.idx, path, cpopath + 'npoints')
		self.rzcoordinate.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'rzdrdz')


class rzcoordinatestructurerz3D(KeepInOrder):
	'''
	class rzcoordinatestructurerz3D
	Irregular outline [m]; 3D arrays (ncoils,max_nelements,max_npoints)

	Attributes:
	- r : numpy.ndarray 3D with float
	   Major radius [m]
	- z : numpy.ndarray 3D with float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='rzcoordinate'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.z = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class rzcoordinatestructurerz3D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rzcoordinatestructurerz3D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rzcoordinatestructurerz3D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rzcoordinatestructurerz3D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect3DDouble(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect3DDouble(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type rzcoordinatestructurerz3D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r = ull.getVect3DDouble(self.idx, path, cpopath + 'r')
		check_status(status)
		if not status:
			self.r = ret_r
		status, ret_z = ull.getVect3DDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type rzcoordinatestructurerz3D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rVal = ull.getVect3DDouble(self.idx, path, cpopath + 'r')
			check_status(status)
			status, zVal = ull.getVect3DDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = rzcoordinatestructurerz3D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rzcoordinatestructurerz3DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rzcoordinatestructurerz3DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rzcoordinatestructurerz3DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rzcoordinatestructurerz3DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		status, ret_r = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'r', i)
		check_status(status)
		if not status:
			self.r = ret_r
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'z', i)
		check_status(status)
		if not status:
			self.z = ret_z

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'z')


class coilcurrentstructureexp1D(KeepInOrder):
	'''
	class coilcurrentstructureexp1D
	Circuit feed current in the coil, defined positive if it flows from point 1 to point 2 of the component in the pfcircuit description [A]; Time-dependent; Vector (ncoils)

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='coilcurrent'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class coilcurrentstructureexp1D\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.abserror.__str__()
		ret = ret + space + 'Attribute abserror\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.relerror.__str__()
		ret = ret + space + 'Attribute relerror\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coilcurrentstructureexp1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coilcurrentstructureexp1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coilcurrentstructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type coilcurrentstructureexp1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_abserror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type coilcurrentstructureexp1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, abserrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (0,nbslice))
			check_status(status)
			status, relerrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = coilcurrentstructureexp1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.abserror = abserrorList[:,i]
				slice.relerror = relerrorList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coilcurrentstructureexp1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'abserror', i, numpy.array(self.abserror).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'relerror', i, numpy.array(self.relerror).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coilcurrentstructureexp1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coilcurrentstructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coilcurrentstructureexp1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'abserror')
		ull.deleteData(self.idx, path, cpopath + 'relerror')


class coilvoltagestructureexp1D(KeepInOrder):
	'''
	class coilvoltagestructureexp1D
	Voltage on the full coil [V]; Time-dependent; Vector (ncoils)

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='coilvoltage'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class coilvoltagestructureexp1D\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.abserror.__str__()
		ret = ret + space + 'Attribute abserror\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.relerror.__str__()
		ret = ret + space + 'Attribute relerror\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coilvoltagestructureexp1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coilvoltagestructureexp1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coilvoltagestructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type coilvoltagestructureexp1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_abserror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type coilvoltagestructureexp1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, abserrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (0,nbslice))
			check_status(status)
			status, relerrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = coilvoltagestructureexp1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.abserror = abserrorList[:,i]
				slice.relerror = relerrorList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coilvoltagestructureexp1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'abserror', i, numpy.array(self.abserror).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'relerror', i, numpy.array(self.relerror).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coilvoltagestructureexp1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coilvoltagestructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coilvoltagestructureexp1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'abserror')
		ull.deleteData(self.idx, path, cpopath + 'relerror')


class pfpassivestructurepfpassive(KeepInOrder):
	'''
	class pfpassivestructurepfpassive
	Passive axisymmetric conductor description

	Attributes:
	- name : list of str
	   Name of coil. Array of strings (nelements)
	- area : numpy.ndarray 1D with float
	   Surface area of this passive element [m^2]; Vector (nelements)
	- res : numpy.ndarray 1D with float
	   Passive element resistance [Ohm]; Vector (nelements)
	- eta : numpy.ndarray 1D with float
	   Passive element resistivity [Ohm.m]; Vector (nelements)
	- current : class currentstructurepfpassive_current
	   Current induced in passive structures.
	- pfpageometry : class pfpageometrystructurepfpageometry
	   Geometry of the passive elements
	'''

	def __init__(self, base_path_in='pfpassive'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ['']
		self.area = numpy.zeros(0, numpy.float64, order='C')
		self.res = numpy.zeros(0, numpy.float64, order='C')
		self.eta = numpy.zeros(0, numpy.float64, order='C')
		self.current = currentstructurepfpassive_current('current')
		self.pfpageometry = pfpageometrystructurepfpageometry('pfpageometry')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pfpassivestructurepfpassive\n'
		s = self.name.__str__()
		ret = ret + space + 'Attribute name\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.area.__str__()
		ret = ret + space + 'Attribute area\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.res.__str__()
		ret = ret + space + 'Attribute res\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.eta.__str__()
		ret = ret + space + 'Attribute eta\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute current\n ' + self.current.__str__(depth+1)
		ret = ret + space + 'Attribute pfpageometry\n ' + self.pfpageometry.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.current.setExpIdx(idx)
		self.pfpageometry.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfpassivestructurepfpassive, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.current.cpoTime = self.cpoTime
		self.current.putSlice(path, cpopath)
		self.pfpageometry.cpoTime = self.cpoTime
		self.pfpageometry.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfpassivestructurepfpassive, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.current.replaceLastSlice(path, cpopath)
		self.pfpageometry.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfpassivestructurepfpassive, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'name', self.name, False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'area', numpy.array(self.area).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'res', numpy.array(self.res).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'eta', numpy.array(self.eta).astype(numpy.float64), False)
		check_status(status)
		self.current.putNonTimed(path, cpopath)
		self.pfpageometry.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pfpassivestructurepfpassive, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_name = ull.getVect1DString(self.idx, path, cpopath + 'name')
		check_status(status)
		if not status:
			self.name = ret_name
		status, ret_area = ull.getVect1DDouble(self.idx, path, cpopath + 'area')
		check_status(status)
		if not status:
			self.area = ret_area
		status, ret_res = ull.getVect1DDouble(self.idx, path, cpopath + 'res')
		check_status(status)
		if not status:
			self.res = ret_res
		status, ret_eta = ull.getVect1DDouble(self.idx, path, cpopath + 'eta')
		check_status(status)
		if not status:
			self.eta = ret_eta
		self.current.getSlice(path, cpopath, inTime, interpolMode)
		self.pfpageometry.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pfpassivestructurepfpassive, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nameVal = ull.getVect1DString(self.idx, path, cpopath + 'name')
			check_status(status)
			status, areaVal = ull.getVect1DDouble(self.idx, path, cpopath + 'area')
			check_status(status)
			status, resVal = ull.getVect1DDouble(self.idx, path, cpopath + 'res')
			check_status(status)
			status, etaVal = ull.getVect1DDouble(self.idx, path, cpopath + 'eta')
			check_status(status)
			currentList = self.current.build_non_resampled_data(path, cpopath, nbslice)
			pfpageometryList = self.pfpageometry.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = pfpassivestructurepfpassive(self.base_path)
				slice.setExpIdx(self.idx)
				slice.name = nameVal
				slice.area = areaVal
				slice.res = resVal
				slice.eta = etaVal
				slice.current = currentList[i]
				slice.pfpageometry = pfpageometryList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfpassivestructurepfpassiveObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.current.putTimedElt(path, cpopath + 'current', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfpassivestructurepfpassiveObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.current.getTimedElt(path, cpopath + 'current', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfpassivestructurepfpassiveObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'area') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'area', i, numpy.array(self.area).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'res') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'res', i, numpy.array(self.res).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'eta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'eta', i, numpy.array(self.eta).astype(numpy.float64))
		obj = self.current.putNonTimedElt(path, cpopath + 'current', i, obj)
		obj = self.pfpageometry.putNonTimedElt(path, cpopath + 'pfpageometry', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfpassivestructurepfpassiveObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'area') 
			print ('obj = ' + str(obj))
		status, ret_area = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'area', i)
		check_status(status)
		if not status:
			self.area = ret_area
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'res') 
			print ('obj = ' + str(obj))
		status, ret_res = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'res', i)
		check_status(status)
		if not status:
			self.res = ret_res
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'eta') 
			print ('obj = ' + str(obj))
		status, ret_eta = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'eta', i)
		check_status(status)
		if not status:
			self.eta = ret_eta
		self.current.getNonTimedElt(path, cpopath + 'current', i, obj)
		self.pfpageometry.getNonTimedElt(path, cpopath + 'pfpageometry', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'name')
		ull.deleteData(self.idx, path, cpopath + 'area')
		ull.deleteData(self.idx, path, cpopath + 'res')
		ull.deleteData(self.idx, path, cpopath + 'eta')
		self.current.deleteData(path, cpopath)
		self.pfpageometry.deleteData(path, cpopath)


class currentstructurepfpassive_current(KeepInOrder):
	'''
	class currentstructurepfpassive_current
	Current induced in passive structures.

	Attributes:
	- toroidal : class toroidalstructureexp1D
	   Toroidal current induced in passive structures [A]. Vector (nelements); Time-dependent
	- poloidal : class poloidalstructureexp1D
	   Poloidal current induced in passive structures [A]. Vector (nelements); Time-dependent
	'''

	def __init__(self, base_path_in='current'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.toroidal = toroidalstructureexp1D('toroidal')
		self.poloidal = poloidalstructureexp1D('poloidal')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class currentstructurepfpassive_current\n'
		ret = ret + space + 'Attribute toroidal\n ' + self.toroidal.__str__(depth+1)
		ret = ret + space + 'Attribute poloidal\n ' + self.poloidal.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.toroidal.setExpIdx(idx)
		self.poloidal.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructurepfpassive_current, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.toroidal.cpoTime = self.cpoTime
		self.toroidal.putSlice(path, cpopath)
		self.poloidal.cpoTime = self.cpoTime
		self.poloidal.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructurepfpassive_current, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.toroidal.replaceLastSlice(path, cpopath)
		self.poloidal.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructurepfpassive_current, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.toroidal.putNonTimed(path, cpopath)
		self.poloidal.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructurepfpassive_current, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.toroidal.getSlice(path, cpopath, inTime, interpolMode)
		self.poloidal.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructurepfpassive_current, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			toroidalList = self.toroidal.build_non_resampled_data(path, cpopath, nbslice)
			poloidalList = self.poloidal.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = currentstructurepfpassive_current(self.base_path)
				slice.setExpIdx(self.idx)
				slice.toroidal = toroidalList[i]
				slice.poloidal = poloidalList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructurepfpassive_currentObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.toroidal.putTimedElt(path, cpopath + 'toroidal', i, obj)
		obj = self.poloidal.putTimedElt(path, cpopath + 'poloidal', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructurepfpassive_currentObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.toroidal.getTimedElt(path, cpopath + 'toroidal', i, obj)
		self.poloidal.getTimedElt(path, cpopath + 'poloidal', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructurepfpassive_currentObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.toroidal.putNonTimedElt(path, cpopath + 'toroidal', i, obj)
		obj = self.poloidal.putNonTimedElt(path, cpopath + 'poloidal', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructurepfpassive_currentObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.toroidal.getNonTimedElt(path, cpopath + 'toroidal', i, obj)
		self.poloidal.getNonTimedElt(path, cpopath + 'poloidal', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.toroidal.deleteData(path, cpopath)
		self.poloidal.deleteData(path, cpopath)


class toroidalstructureexp1D(KeepInOrder):
	'''
	class toroidalstructureexp1D
	Toroidal current induced in passive structures [A]. Vector (nelements); Time-dependent

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='toroidal'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class toroidalstructureexp1D\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.abserror.__str__()
		ret = ret + space + 'Attribute abserror\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.relerror.__str__()
		ret = ret + space + 'Attribute relerror\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type toroidalstructureexp1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type toroidalstructureexp1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type toroidalstructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type toroidalstructureexp1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_abserror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type toroidalstructureexp1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, abserrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (0,nbslice))
			check_status(status)
			status, relerrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = toroidalstructureexp1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.abserror = abserrorList[:,i]
				slice.relerror = relerrorList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroidalstructureexp1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'abserror', i, numpy.array(self.abserror).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'relerror', i, numpy.array(self.relerror).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroidalstructureexp1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroidalstructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroidalstructureexp1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'abserror')
		ull.deleteData(self.idx, path, cpopath + 'relerror')


class poloidalstructureexp1D(KeepInOrder):
	'''
	class poloidalstructureexp1D
	Poloidal current induced in passive structures [A]. Vector (nelements); Time-dependent

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='poloidal'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class poloidalstructureexp1D\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.abserror.__str__()
		ret = ret + space + 'Attribute abserror\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.relerror.__str__()
		ret = ret + space + 'Attribute relerror\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type poloidalstructureexp1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type poloidalstructureexp1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type poloidalstructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type poloidalstructureexp1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_abserror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type poloidalstructureexp1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, abserrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (0,nbslice))
			check_status(status)
			status, relerrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = poloidalstructureexp1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.abserror = abserrorList[:,i]
				slice.relerror = relerrorList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type poloidalstructureexp1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'abserror', i, numpy.array(self.abserror).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'relerror', i, numpy.array(self.relerror).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type poloidalstructureexp1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type poloidalstructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type poloidalstructureexp1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'abserror')
		ull.deleteData(self.idx, path, cpopath + 'relerror')


class pfpageometrystructurepfpageometry(KeepInOrder):
	'''
	class pfpageometrystructurepfpageometry
	Geometry of the passive elements

	Attributes:
	- type : numpy.ndarray 1D with int)
	   Type used to describe the shape (0 for 'rzcoordinates' or 1 for 'rzdrdz'); Vector of integers (nelements)
	- npoints : numpy.ndarray 1D with int)
	   Number of points describing an element (irregular outline rzcoordinates); Vector of integers (nelements)
	- rzcoordinate : class rzcoordinatestructurerz2D
	   Irregular outline [m]; Matrix (nelements,max_npoints)
	- rzdrdz : numpy.ndarray 2D with float
	   4-vector defining Centre R,Z and full extents dR, dZ [m]; Matrix (nelements,4)
	'''

	def __init__(self, base_path_in='pfpageometry'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.type = numpy.zeros(0, numpy.int32, order='C')
		self.npoints = numpy.zeros(0, numpy.int32, order='C')
		self.rzcoordinate = rzcoordinatestructurerz2D('rzcoordinate')
		self.rzdrdz = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pfpageometrystructurepfpageometry\n'
		s = self.type.__str__()
		ret = ret + space + 'Attribute type\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.npoints.__str__()
		ret = ret + space + 'Attribute npoints\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute rzcoordinate\n ' + self.rzcoordinate.__str__(depth+1)
		s = self.rzdrdz.__str__()
		ret = ret + space + 'Attribute rzdrdz\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.rzcoordinate.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfpageometrystructurepfpageometry, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.rzcoordinate.cpoTime = self.cpoTime
		self.rzcoordinate.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfpageometrystructurepfpageometry, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.rzcoordinate.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfpageometrystructurepfpageometry, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DInt(self.idx, path, cpopath + 'type', numpy.array(self.type).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'npoints', numpy.array(self.npoints).astype(numpy.int32), False)
		check_status(status)
		self.rzcoordinate.putNonTimed(path, cpopath)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'rzdrdz', numpy.array(self.rzdrdz).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pfpageometrystructurepfpageometry, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_type = ull.getVect1DInt(self.idx, path, cpopath + 'type')
		check_status(status)
		if not status:
			self.type = ret_type
		status, ret_npoints = ull.getVect1DInt(self.idx, path, cpopath + 'npoints')
		check_status(status)
		if not status:
			self.npoints = ret_npoints
		self.rzcoordinate.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_rzdrdz = ull.getVect2DDouble(self.idx, path, cpopath + 'rzdrdz')
		check_status(status)
		if not status:
			self.rzdrdz = ret_rzdrdz

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pfpageometrystructurepfpageometry, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, typeVal = ull.getVect1DInt(self.idx, path, cpopath + 'type')
			check_status(status)
			status, npointsVal = ull.getVect1DInt(self.idx, path, cpopath + 'npoints')
			check_status(status)
			rzcoordinateList = self.rzcoordinate.build_non_resampled_data(path, cpopath, nbslice)
			status, rzdrdzVal = ull.getVect2DDouble(self.idx, path, cpopath + 'rzdrdz')
			check_status(status)
			for i in range(nbslice):
				slice = pfpageometrystructurepfpageometry(self.base_path)
				slice.setExpIdx(self.idx)
				slice.type = typeVal
				slice.npoints = npointsVal
				slice.rzcoordinate = rzcoordinateList[i]
				slice.rzdrdz = rzdrdzVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfpageometrystructurepfpageometryObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfpageometrystructurepfpageometryObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfpageometrystructurepfpageometryObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'type', i, numpy.array(self.type).astype(numpy.int32))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'npoints') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'npoints', i, numpy.array(self.npoints).astype(numpy.int32))
		obj = self.rzcoordinate.putNonTimedElt(path, cpopath + 'rzcoordinate', i, obj)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'rzdrdz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'rzdrdz', i, numpy.array(self.rzdrdz).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfpageometrystructurepfpageometryObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		status, ret_type = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'type', i)
		check_status(status)
		if not status:
			self.type = ret_type
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'npoints') 
			print ('obj = ' + str(obj))
		status, ret_npoints = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'npoints', i)
		check_status(status)
		if not status:
			self.npoints = ret_npoints
		self.rzcoordinate.getNonTimedElt(path, cpopath + 'rzcoordinate', i, obj)
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'rzdrdz') 
			print ('obj = ' + str(obj))
		status, ret_rzdrdz = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'rzdrdz', i)
		check_status(status)
		if not status:
			self.rzdrdz = ret_rzdrdz

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'type')
		ull.deleteData(self.idx, path, cpopath + 'npoints')
		self.rzcoordinate.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'rzdrdz')


class rzcoordinatestructurerz2D(KeepInOrder):
	'''
	class rzcoordinatestructurerz2D
	Irregular outline [m]; Matrix (nelements,max_npoints)

	Attributes:
	- r : numpy.ndarray 2D with float
	   Major radius [m]
	- z : numpy.ndarray 2D with float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='rzcoordinate'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros((0,0), numpy.float64, order='C')
		self.z = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class rzcoordinatestructurerz2D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rzcoordinatestructurerz2D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rzcoordinatestructurerz2D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rzcoordinatestructurerz2D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type rzcoordinatestructurerz2D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r = ull.getVect2DDouble(self.idx, path, cpopath + 'r')
		check_status(status)
		if not status:
			self.r = ret_r
		status, ret_z = ull.getVect2DDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type rzcoordinatestructurerz2D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rVal = ull.getVect2DDouble(self.idx, path, cpopath + 'r')
			check_status(status)
			status, zVal = ull.getVect2DDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = rzcoordinatestructurerz2D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rzcoordinatestructurerz2DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rzcoordinatestructurerz2DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rzcoordinatestructurerz2DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rzcoordinatestructurerz2DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		status, ret_r = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'r', i)
		check_status(status)
		if not status:
			self.r = ret_r
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'z', i)
		check_status(status)
		if not status:
			self.z = ret_z

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'z')


class pfcircuitsstructurepfcircuits(KeepInOrder):
	'''
	class pfcircuitsstructurepfcircuits
	Circuits, connected to multiple coils and to multiple supplies, defining the current and voltage relationships in the system

	Attributes:
	- name : list of str
	   Name of circuit, array of strings (ncircuits)
	- id : list of str
	   ID of circuit, array of strings (ncircuits)
	- type : list of str
	   Type of circuit, array of strings (ncircuits)
	- nnodes : numpy.ndarray 1D with int)
	   Number of nodes used to describe a circuit. Vector (ncircuits)
	- connections : numpy.ndarray 3D with int
	   Description of the supplies and coils connections (nodes) across each circuit. Array 3D (ncircuits,max_nnodes,2*ncomponents), describing for each node which component are connected to it (1 if connected, 0 otherwise). There are 2 sides at each component, thus 2*ncomponents as the size of the third dimension, listing first all supplies, then all coils (in the same order as listed in PFSUPPLIES and PFCOILS). An example can be found in the data structure documentation PFconnections.pdf
	'''

	def __init__(self, base_path_in='pfcircuits'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ['']
		self.id = ['']
		self.type = ['']
		self.nnodes = numpy.zeros(0, numpy.int32, order='C')
		self.connections = numpy.zeros((0,0,0), numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pfcircuitsstructurepfcircuits\n'
		s = self.name.__str__()
		ret = ret + space + 'Attribute name\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.id.__str__()
		ret = ret + space + 'Attribute id\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.type.__str__()
		ret = ret + space + 'Attribute type\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.nnodes.__str__()
		ret = ret + space + 'Attribute nnodes\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.connections.__str__()
		ret = ret + space + 'Attribute connections\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfcircuitsstructurepfcircuits, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfcircuitsstructurepfcircuits, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfcircuitsstructurepfcircuits, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'name', self.name, False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'id', self.id, False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'type', self.type, False)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'nnodes', numpy.array(self.nnodes).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect3DInt(self.idx, path, cpopath + 'connections', numpy.array(self.connections).astype(numpy.int32), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pfcircuitsstructurepfcircuits, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_name = ull.getVect1DString(self.idx, path, cpopath + 'name')
		check_status(status)
		if not status:
			self.name = ret_name
		status, ret_id = ull.getVect1DString(self.idx, path, cpopath + 'id')
		check_status(status)
		if not status:
			self.id = ret_id
		status, ret_type = ull.getVect1DString(self.idx, path, cpopath + 'type')
		check_status(status)
		if not status:
			self.type = ret_type
		status, ret_nnodes = ull.getVect1DInt(self.idx, path, cpopath + 'nnodes')
		check_status(status)
		if not status:
			self.nnodes = ret_nnodes
		status, ret_connections = ull.getVect3DInt(self.idx, path, cpopath + 'connections')
		check_status(status)
		if not status:
			self.connections = ret_connections

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pfcircuitsstructurepfcircuits, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nameVal = ull.getVect1DString(self.idx, path, cpopath + 'name')
			check_status(status)
			status, idVal = ull.getVect1DString(self.idx, path, cpopath + 'id')
			check_status(status)
			status, typeVal = ull.getVect1DString(self.idx, path, cpopath + 'type')
			check_status(status)
			status, nnodesVal = ull.getVect1DInt(self.idx, path, cpopath + 'nnodes')
			check_status(status)
			status, connectionsVal = ull.getVect3DInt(self.idx, path, cpopath + 'connections')
			check_status(status)
			for i in range(nbslice):
				slice = pfcircuitsstructurepfcircuits(self.base_path)
				slice.setExpIdx(self.idx)
				slice.name = nameVal
				slice.id = idVal
				slice.type = typeVal
				slice.nnodes = nnodesVal
				slice.connections = connectionsVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfcircuitsstructurepfcircuitsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfcircuitsstructurepfcircuitsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfcircuitsstructurepfcircuitsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'type', i, self.type)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'nnodes') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'nnodes', i, numpy.array(self.nnodes).astype(numpy.int32))
		if (dev()):
			print ('putVect3DIntInObject : ' + cpopath + 'connections') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DIntInObject(self.idx, obj, cpopath + 'connections', i, numpy.array(self.connections).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfcircuitsstructurepfcircuitsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		status, ret_type = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'type', i)
		check_status(status)
		if not status:
			self.type = ret_type
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'nnodes') 
			print ('obj = ' + str(obj))
		status, ret_nnodes = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'nnodes', i)
		check_status(status)
		if not status:
			self.nnodes = ret_nnodes
		if (dev()):
			print ('getVect3DIntInObject : ' + cpopath + 'connections') 
			print ('obj = ' + str(obj))
		status, ret_connections = ull.getVect3DIntFromObject(self.idx, obj, cpopath + 'connections', i)
		check_status(status)
		if not status:
			self.connections = ret_connections

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'name')
		ull.deleteData(self.idx, path, cpopath + 'id')
		ull.deleteData(self.idx, path, cpopath + 'type')
		ull.deleteData(self.idx, path, cpopath + 'nnodes')
		ull.deleteData(self.idx, path, cpopath + 'connections')


class pfsuppliesstructurepfsupplies(KeepInOrder):
	'''
	class pfsuppliesstructurepfsupplies
	PF power supplies

	Attributes:
	- desc_supply : class desc_supplystructuredesc_supply
	   Description of the power supplies
	- voltage : class voltagestructureexp1D
	   Voltage at the supply output [V]; Time-dependent; Vector  (nsupplies)
	- current : class currentstructureexp1D
	   Current at the supply output, defined positive if it flows from point 1 to point 2 of the component in the pfcircuit description [A]; Time-dependent; Vector (nsupplies) 
	'''

	def __init__(self, base_path_in='pfsupplies'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.desc_supply = desc_supplystructuredesc_supply('desc_supply')
		self.voltage = voltagestructureexp1D('voltage')
		self.current = currentstructureexp1D('current')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pfsuppliesstructurepfsupplies\n'
		ret = ret + space + 'Attribute desc_supply\n ' + self.desc_supply.__str__(depth+1)
		ret = ret + space + 'Attribute voltage\n ' + self.voltage.__str__(depth+1)
		ret = ret + space + 'Attribute current\n ' + self.current.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.desc_supply.setExpIdx(idx)
		self.voltage.setExpIdx(idx)
		self.current.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfsuppliesstructurepfsupplies, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.desc_supply.cpoTime = self.cpoTime
		self.desc_supply.putSlice(path, cpopath)
		self.voltage.cpoTime = self.cpoTime
		self.voltage.putSlice(path, cpopath)
		self.current.cpoTime = self.cpoTime
		self.current.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfsuppliesstructurepfsupplies, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.desc_supply.replaceLastSlice(path, cpopath)
		self.voltage.replaceLastSlice(path, cpopath)
		self.current.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfsuppliesstructurepfsupplies, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.desc_supply.putNonTimed(path, cpopath)
		self.voltage.putNonTimed(path, cpopath)
		self.current.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pfsuppliesstructurepfsupplies, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.desc_supply.getSlice(path, cpopath, inTime, interpolMode)
		self.voltage.getSlice(path, cpopath, inTime, interpolMode)
		self.current.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pfsuppliesstructurepfsupplies, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			desc_supplyList = self.desc_supply.build_non_resampled_data(path, cpopath, nbslice)
			voltageList = self.voltage.build_non_resampled_data(path, cpopath, nbslice)
			currentList = self.current.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = pfsuppliesstructurepfsupplies(self.base_path)
				slice.setExpIdx(self.idx)
				slice.desc_supply = desc_supplyList[i]
				slice.voltage = voltageList[i]
				slice.current = currentList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfsuppliesstructurepfsuppliesObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.voltage.putTimedElt(path, cpopath + 'voltage', i, obj)
		obj = self.current.putTimedElt(path, cpopath + 'current', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfsuppliesstructurepfsuppliesObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.voltage.getTimedElt(path, cpopath + 'voltage', i, obj)
		self.current.getTimedElt(path, cpopath + 'current', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfsuppliesstructurepfsuppliesObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.desc_supply.putNonTimedElt(path, cpopath + 'desc_supply', i, obj)
		obj = self.voltage.putNonTimedElt(path, cpopath + 'voltage', i, obj)
		obj = self.current.putNonTimedElt(path, cpopath + 'current', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfsuppliesstructurepfsuppliesObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.desc_supply.getNonTimedElt(path, cpopath + 'desc_supply', i, obj)
		self.voltage.getNonTimedElt(path, cpopath + 'voltage', i, obj)
		self.current.getNonTimedElt(path, cpopath + 'current', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.desc_supply.deleteData(path, cpopath)
		self.voltage.deleteData(path, cpopath)
		self.current.deleteData(path, cpopath)


class desc_supplystructuredesc_supply(KeepInOrder):
	'''
	class desc_supplystructuredesc_supply
	Description of the power supplies

	Attributes:
	- name : list of str
	   Name of the supply; Array of strings (nsupplies)
	- id : list of str
	   ID of the supply; Array of strings (nsupplies)
	- type : list of str
	   Type of supply; Array of strings (nsupplies)
	- delay : numpy.ndarray 1D with float
	   Pure delay in the supply [s]; Vector (nsupplies)
	- filter : class filterstructurefilter
	   Laplace proper filter 
	- imin : numpy.ndarray 1D with float
	   Minimum current [A]; Vector (nsupplies)
	- imax : numpy.ndarray 1D with float
	   Maximum current [A]; Vector (nsupplies)
	- res : numpy.ndarray 1D with float
	   Supply internal resistance [Ohm]; Vector (nsupplies)
	- umin : numpy.ndarray 1D with float
	   Minimum voltage [V]; Vector (nsupplies)
	- umax : numpy.ndarray 1D with float
	   Maximum voltage [V]; Vector (nsupplies)
	- emax : numpy.ndarray 1D with float
	   Maximum Energy to be dissipated in supply [J]; Vector (nsupplies)
	'''

	def __init__(self, base_path_in='desc_supply'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ['']
		self.id = ['']
		self.type = ['']
		self.delay = numpy.zeros(0, numpy.float64, order='C')
		self.filter = filterstructurefilter('filter')
		self.imin = numpy.zeros(0, numpy.float64, order='C')
		self.imax = numpy.zeros(0, numpy.float64, order='C')
		self.res = numpy.zeros(0, numpy.float64, order='C')
		self.umin = numpy.zeros(0, numpy.float64, order='C')
		self.umax = numpy.zeros(0, numpy.float64, order='C')
		self.emax = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class desc_supplystructuredesc_supply\n'
		s = self.name.__str__()
		ret = ret + space + 'Attribute name\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.id.__str__()
		ret = ret + space + 'Attribute id\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.type.__str__()
		ret = ret + space + 'Attribute type\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.delay.__str__()
		ret = ret + space + 'Attribute delay\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute filter\n ' + self.filter.__str__(depth+1)
		s = self.imin.__str__()
		ret = ret + space + 'Attribute imin\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.imax.__str__()
		ret = ret + space + 'Attribute imax\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.res.__str__()
		ret = ret + space + 'Attribute res\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.umin.__str__()
		ret = ret + space + 'Attribute umin\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.umax.__str__()
		ret = ret + space + 'Attribute umax\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.emax.__str__()
		ret = ret + space + 'Attribute emax\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.filter.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type desc_supplystructuredesc_supply, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.filter.cpoTime = self.cpoTime
		self.filter.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type desc_supplystructuredesc_supply, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.filter.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type desc_supplystructuredesc_supply, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DString(self.idx, path, cpopath + 'name', self.name, False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'id', self.id, False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'type', self.type, False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'delay', numpy.array(self.delay).astype(numpy.float64), False)
		check_status(status)
		self.filter.putNonTimed(path, cpopath)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'imin', numpy.array(self.imin).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'imax', numpy.array(self.imax).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'res', numpy.array(self.res).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'umin', numpy.array(self.umin).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'umax', numpy.array(self.umax).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'emax', numpy.array(self.emax).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type desc_supplystructuredesc_supply, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_name = ull.getVect1DString(self.idx, path, cpopath + 'name')
		check_status(status)
		if not status:
			self.name = ret_name
		status, ret_id = ull.getVect1DString(self.idx, path, cpopath + 'id')
		check_status(status)
		if not status:
			self.id = ret_id
		status, ret_type = ull.getVect1DString(self.idx, path, cpopath + 'type')
		check_status(status)
		if not status:
			self.type = ret_type
		status, ret_delay = ull.getVect1DDouble(self.idx, path, cpopath + 'delay')
		check_status(status)
		if not status:
			self.delay = ret_delay
		self.filter.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_imin = ull.getVect1DDouble(self.idx, path, cpopath + 'imin')
		check_status(status)
		if not status:
			self.imin = ret_imin
		status, ret_imax = ull.getVect1DDouble(self.idx, path, cpopath + 'imax')
		check_status(status)
		if not status:
			self.imax = ret_imax
		status, ret_res = ull.getVect1DDouble(self.idx, path, cpopath + 'res')
		check_status(status)
		if not status:
			self.res = ret_res
		status, ret_umin = ull.getVect1DDouble(self.idx, path, cpopath + 'umin')
		check_status(status)
		if not status:
			self.umin = ret_umin
		status, ret_umax = ull.getVect1DDouble(self.idx, path, cpopath + 'umax')
		check_status(status)
		if not status:
			self.umax = ret_umax
		status, ret_emax = ull.getVect1DDouble(self.idx, path, cpopath + 'emax')
		check_status(status)
		if not status:
			self.emax = ret_emax

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type desc_supplystructuredesc_supply, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nameVal = ull.getVect1DString(self.idx, path, cpopath + 'name')
			check_status(status)
			status, idVal = ull.getVect1DString(self.idx, path, cpopath + 'id')
			check_status(status)
			status, typeVal = ull.getVect1DString(self.idx, path, cpopath + 'type')
			check_status(status)
			status, delayVal = ull.getVect1DDouble(self.idx, path, cpopath + 'delay')
			check_status(status)
			filterList = self.filter.build_non_resampled_data(path, cpopath, nbslice)
			status, iminVal = ull.getVect1DDouble(self.idx, path, cpopath + 'imin')
			check_status(status)
			status, imaxVal = ull.getVect1DDouble(self.idx, path, cpopath + 'imax')
			check_status(status)
			status, resVal = ull.getVect1DDouble(self.idx, path, cpopath + 'res')
			check_status(status)
			status, uminVal = ull.getVect1DDouble(self.idx, path, cpopath + 'umin')
			check_status(status)
			status, umaxVal = ull.getVect1DDouble(self.idx, path, cpopath + 'umax')
			check_status(status)
			status, emaxVal = ull.getVect1DDouble(self.idx, path, cpopath + 'emax')
			check_status(status)
			for i in range(nbslice):
				slice = desc_supplystructuredesc_supply(self.base_path)
				slice.setExpIdx(self.idx)
				slice.name = nameVal
				slice.id = idVal
				slice.type = typeVal
				slice.delay = delayVal
				slice.filter = filterList[i]
				slice.imin = iminVal
				slice.imax = imaxVal
				slice.res = resVal
				slice.umin = uminVal
				slice.umax = umaxVal
				slice.emax = emaxVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_supplystructuredesc_supplyObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_supplystructuredesc_supplyObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_supplystructuredesc_supplyObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'type', i, self.type)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'delay') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'delay', i, numpy.array(self.delay).astype(numpy.float64))
		obj = self.filter.putNonTimedElt(path, cpopath + 'filter', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'imin') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'imin', i, numpy.array(self.imin).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'imax') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'imax', i, numpy.array(self.imax).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'res') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'res', i, numpy.array(self.res).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'umin') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'umin', i, numpy.array(self.umin).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'umax') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'umax', i, numpy.array(self.umax).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'emax') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'emax', i, numpy.array(self.emax).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type desc_supplystructuredesc_supplyObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		status, ret_type = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'type', i)
		check_status(status)
		if not status:
			self.type = ret_type
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'delay') 
			print ('obj = ' + str(obj))
		status, ret_delay = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'delay', i)
		check_status(status)
		if not status:
			self.delay = ret_delay
		self.filter.getNonTimedElt(path, cpopath + 'filter', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'imin') 
			print ('obj = ' + str(obj))
		status, ret_imin = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'imin', i)
		check_status(status)
		if not status:
			self.imin = ret_imin
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'imax') 
			print ('obj = ' + str(obj))
		status, ret_imax = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'imax', i)
		check_status(status)
		if not status:
			self.imax = ret_imax
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'res') 
			print ('obj = ' + str(obj))
		status, ret_res = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'res', i)
		check_status(status)
		if not status:
			self.res = ret_res
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'umin') 
			print ('obj = ' + str(obj))
		status, ret_umin = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'umin', i)
		check_status(status)
		if not status:
			self.umin = ret_umin
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'umax') 
			print ('obj = ' + str(obj))
		status, ret_umax = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'umax', i)
		check_status(status)
		if not status:
			self.umax = ret_umax
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'emax') 
			print ('obj = ' + str(obj))
		status, ret_emax = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'emax', i)
		check_status(status)
		if not status:
			self.emax = ret_emax

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'name')
		ull.deleteData(self.idx, path, cpopath + 'id')
		ull.deleteData(self.idx, path, cpopath + 'type')
		ull.deleteData(self.idx, path, cpopath + 'delay')
		self.filter.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'imin')
		ull.deleteData(self.idx, path, cpopath + 'imax')
		ull.deleteData(self.idx, path, cpopath + 'res')
		ull.deleteData(self.idx, path, cpopath + 'umin')
		ull.deleteData(self.idx, path, cpopath + 'umax')
		ull.deleteData(self.idx, path, cpopath + 'emax')


class filterstructurefilter(KeepInOrder):
	'''
	class filterstructurefilter
	Laplace proper filter 

	Attributes:
	- num : numpy.ndarray 2D with float
	   Coefficients of the numerator, in increasing order : a0 + a1*s + ... + an*s^n; Matrix (nsupplies,n)
	- den : numpy.ndarray 2D with float
	   Coefficients of the denominator, in increasing order : b0 + b1*s + ... + bm*s^m; Matrix (nsupplies,m)
	'''

	def __init__(self, base_path_in='filter'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.num = numpy.zeros((0,0), numpy.float64, order='C')
		self.den = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class filterstructurefilter\n'
		s = self.num.__str__()
		ret = ret + space + 'Attribute num\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.den.__str__()
		ret = ret + space + 'Attribute den\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type filterstructurefilter, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type filterstructurefilter, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type filterstructurefilter, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'num', numpy.array(self.num).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect2DDouble(self.idx, path, cpopath + 'den', numpy.array(self.den).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type filterstructurefilter, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_num = ull.getVect2DDouble(self.idx, path, cpopath + 'num')
		check_status(status)
		if not status:
			self.num = ret_num
		status, ret_den = ull.getVect2DDouble(self.idx, path, cpopath + 'den')
		check_status(status)
		if not status:
			self.den = ret_den

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type filterstructurefilter, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, numVal = ull.getVect2DDouble(self.idx, path, cpopath + 'num')
			check_status(status)
			status, denVal = ull.getVect2DDouble(self.idx, path, cpopath + 'den')
			check_status(status)
			for i in range(nbslice):
				slice = filterstructurefilter(self.base_path)
				slice.setExpIdx(self.idx)
				slice.num = numVal
				slice.den = denVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type filterstructurefilterObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type filterstructurefilterObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type filterstructurefilterObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'num') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'num', i, numpy.array(self.num).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'den') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'den', i, numpy.array(self.den).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type filterstructurefilterObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'num') 
			print ('obj = ' + str(obj))
		status, ret_num = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'num', i)
		check_status(status)
		if not status:
			self.num = ret_num
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'den') 
			print ('obj = ' + str(obj))
		status, ret_den = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'den', i)
		check_status(status)
		if not status:
			self.den = ret_den

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'num')
		ull.deleteData(self.idx, path, cpopath + 'den')


class voltagestructureexp1D(KeepInOrder):
	'''
	class voltagestructureexp1D
	Voltage at the supply output [V]; Time-dependent; Vector  (nsupplies)

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='voltage'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class voltagestructureexp1D\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.abserror.__str__()
		ret = ret + space + 'Attribute abserror\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.relerror.__str__()
		ret = ret + space + 'Attribute relerror\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type voltagestructureexp1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type voltagestructureexp1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type voltagestructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type voltagestructureexp1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_abserror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type voltagestructureexp1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, abserrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (0,nbslice))
			check_status(status)
			status, relerrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = voltagestructureexp1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.abserror = abserrorList[:,i]
				slice.relerror = relerrorList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type voltagestructureexp1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'abserror', i, numpy.array(self.abserror).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'relerror', i, numpy.array(self.relerror).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type voltagestructureexp1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type voltagestructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type voltagestructureexp1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'abserror')
		ull.deleteData(self.idx, path, cpopath + 'relerror')


class currentstructureexp1D(KeepInOrder):
	'''
	class currentstructureexp1D
	Current at the supply output, defined positive if it flows from point 1 to point 2 of the component in the pfcircuit description [A]; Time-dependent; Vector (nsupplies) 

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='current'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class currentstructureexp1D\n'
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.abserror.__str__()
		ret = ret + space + 'Attribute abserror\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.relerror.__str__()
		ret = ret + space + 'Attribute relerror\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructureexp1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructureexp1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructureexp1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_abserror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type currentstructureexp1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,nbslice))
			check_status(status)
			status, abserrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (0,nbslice))
			check_status(status)
			status, relerrorList = ull.getVect2DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = currentstructureexp1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,i]
				slice.abserror = abserrorList[:,i]
				slice.relerror = relerrorList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructureexp1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'abserror', i, numpy.array(self.abserror).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'relerror', i, numpy.array(self.relerror).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructureexp1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type currentstructureexp1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'abserror')
		ull.deleteData(self.idx, path, cpopath + 'relerror')


class codeparamstructurecodeparam(KeepInOrder):
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
