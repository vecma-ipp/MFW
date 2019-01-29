# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class nbi:
	'''
	class nbi
	Neutral Beam Injection. Input to NBI source codes; describes the neutrals that are about to be launched into the torus; Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- nbi_unit : class nbi_unitstruct_arraynbi_unit: array of nbi_unitstruct_arraynbi_unitObj objects
	   Vector of Neutral Beam Injector units. The NBI system should be separated in to the individually power strucutres. Structure array(nunits). Time-dependent
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar
	'''

	def __init__(self):
		self.base_path = 'nbi'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 1
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.nbi_unit = nbi_unitstruct_arraynbi_unit('nbi_unit')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nbi\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute nbi_unit\n ' + self.nbi_unit.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.nbi_unit.setExpIdx(idx)
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
		self.nbi_unit.cpoTime = self.cpoTime
		self.nbi_unit.putSlice(path, cpopath)
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
		self.nbi_unit.replaceLastSlice(path, cpopath)
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
		self.nbi_unit.putNonTimed(path, cpopath)
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
		self.nbi_unit.getSlice(path, cpopath, inTime, interpolMode)
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
			nbi_unitList = self.nbi_unit.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = nbi()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.nbi_unit = nbi_unitList[i]
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
		ull.deleteData(self.idx, path, cpopath + 'nbi_unit')
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class nbiArray:
	'''
	class nbiArray
	Neutral Beam Injection. Input to NBI source codes; describes the neutrals that are about to be launched into the torus; Time-dependent CPO

	Attributes:
	- array : list of nbi
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
		ret = space + 'class nbiArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'nbi cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = nbi()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(nbi())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = nbi()
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


class nbi_unitstruct_arraynbi_unit:
	'''
	class nbi_unitstruct_arraynbi_unit
	Vector of Neutral Beam Injector units. The NBI system should be separated in to the individually power strucutres. Structure array(nunits). Time-dependent

	Attributes:
	- array : list of nbi_unitstruct_arraynbi_unitObj 
	'''

	def __init__(self, base_path_in='nbi_unit'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nbi_unitstruct_arraynbi_unit\n'
		for i in range(len(self.array)):
			ret = ret + space + 'nbi_unitstruct_arraynbi_unit[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(nbi_unitstruct_arraynbi_unitObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nbi_unitstruct_arraynbi_unit, run function putSlice') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', TIMED)')
		obj_time = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, TIMED)
		obj = ull.beginObject(self.idx, obj_time, 0, 'ALLTIMES', TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putTimedElt(path, self.base_path, i, obj)
		obj_time = ull.putObjectInObject(self.idx, obj_time, 'ALLTIMES', 0, obj)
		if (dev()):
			print ('putObjectSlice('+path+', '+cpopath+self.base_path+', '+str(self.cpoTime)+', '+str(obj_time)+')')
		status = ull.putObjectSlice(self.idx, path, cpopath + self.base_path, self.cpoTime, obj_time)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nbi_unitstruct_arraynbi_unit, run function replaceLastSlice') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('replaceLastObjectSlice('+path+', '+cpopath+self.base_path+', '+str(obj)+')')
		status = ull.replaceLastObjectSlice(self.idx, path, cpopath + self.base_path, obj)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nbi_unitstruct_arraynbi_unit, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type nbi_unitstruct_arraynbi_unit, run function getSlice') 
		if (dev()):
			print ('getObjectSlice('+path+', '+cpopath+self.base_path+', '+str(inTime)+')')
		status, obj_time = ull.getObjectSlice(self.idx, path, cpopath + self.base_path, inTime)
		if status:
			print ('Failed to get slice: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ')')
			return
		status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', 0)
		if status:
			print ('No data found for slice: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj_time)
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		if (obj_size != 0):
			if (len(self.array) == 0):
				self.resize(obj_size)
			if (obj_size != len(self.array)):
				print ('error in getSlice: wrong size of object '+ path + '/' + cpopath +self.base_path + ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
			else:
				for i in range(obj_size):
					self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type nbi_unitstruct_arraynbi_unit, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(nbi_unitstruct_arraynbi_unit(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(nbi_unitstruct_arraynbi_unit(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = nbi_unitstruct_arraynbi_unit(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj_time)
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				if (obj_size != 0):
					if (len(list[n].array) == 0):
						list[n].resize(obj_size)
					if (obj_size != len(list[n].array)):
						print ('error in get: wrong size of object at '+path+'/'+cpopath+self.base_path + ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(list[n].array)) +')')
					else:
						for i in range(obj_size):
							if list[n]:
								list[n].array[i].getNonTimedElt(path, self.base_path, i, obj)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type nbi_unitstruct_arraynbi_unit, run function putTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j)) 
			obj2 = self.array[j].putTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		if (dev()):
			print ('putObjectInObject('+str(obj)+', '+cpopath+', '+str(i)+', '+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type nbi_unitstruct_arraynbi_unit, run function getTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (current part size =  ' + str(obj_size) + ') != (existing part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getTimedElt(path, self.base_path, j, obj2)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type nbi_unitstruct_arraynbi_unit, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type nbi_unitstruct_arraynbi_unit, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class nbi_unitstruct_arraynbi_unitObj:
	'''
	class nbi_unitstruct_arraynbi_unitObj
	Vector of Neutral Beam Injector units. The NBI system should be separated in to the individually power strucutres. Structure array(nunits). Time-dependent

	Attributes:
	- name : str
	   Name of the neutral beam injector
	- inj_spec : class inj_specstructureinj_spec
	   Injected species
	- pow_unit : class pow_unitstructureexp0D
	   Power delivered by an NBI unit [W]; Time-dependent
	- inj_eng_unit : class inj_eng_unitstructureexp0D
	   Full injection energy of a unit [ev]; Time-dependent
	- beamcurrfrac : class beamcurrfracstructureexp1D
	   Beam current fractions; beamcurrfrac(j) is the fraction of the beam current from beam neutrals with the j:th harmonic energy, inj_eng_unit. Vector(3); Time-dependent
	- beampowrfrac : class beampowrfracstructureexp1D
	   Beam power fractions; beampowrfrac(j) is the fraction of the beam power from beam neutrals with the j:th harmonic energy, inj_eng_unit;. Vector(3); Time-dependent
	- beamletgroup : class beamletgroupstruct_arraybeamletgroup: array of beamletgroupstruct_arraybeamletgroupObj objects
	   Group of beamlets with common vertical and horizontal focal point. If there are no common focal points, then select small groups of beamlets such that a focal point description of the beamlet-group provides a fair description.
	- wall : class wallstructurenbi_nbi_unit_wall
	   Description of the wall components in the NBI system that limits the beam spatial width of the beam. The wall is here described a superposition of surface segments and collimating holes.
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='nbi_unit'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ''
		self.inj_spec = inj_specstructureinj_spec('inj_spec')
		self.pow_unit = pow_unitstructureexp0D('pow_unit')
		self.inj_eng_unit = inj_eng_unitstructureexp0D('inj_eng_unit')
		self.beamcurrfrac = beamcurrfracstructureexp1D('beamcurrfrac')
		self.beampowrfrac = beampowrfracstructureexp1D('beampowrfrac')
		self.beamletgroup = beamletgroupstruct_arraybeamletgroup('beamletgroup')
		self.wall = wallstructurenbi_nbi_unit_wall('wall')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nbi_unitstruct_arraynbi_unitObj\n'
		ret = ret + space + 'Attribute name: ' + str(self.name) + '\n'
		ret = ret + space + 'Attribute inj_spec\n ' + self.inj_spec.__str__(depth+1)
		ret = ret + space + 'Attribute pow_unit\n ' + self.pow_unit.__str__(depth+1)
		ret = ret + space + 'Attribute inj_eng_unit\n ' + self.inj_eng_unit.__str__(depth+1)
		ret = ret + space + 'Attribute beamcurrfrac\n ' + self.beamcurrfrac.__str__(depth+1)
		ret = ret + space + 'Attribute beampowrfrac\n ' + self.beampowrfrac.__str__(depth+1)
		ret = ret + space + 'Attribute beamletgroup\n ' + self.beamletgroup.__str__(depth+1)
		ret = ret + space + 'Attribute wall\n ' + self.wall.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.inj_spec.setExpIdx(idx)
		self.pow_unit.setExpIdx(idx)
		self.inj_eng_unit.setExpIdx(idx)
		self.beamcurrfrac.setExpIdx(idx)
		self.beampowrfrac.setExpIdx(idx)
		self.beamletgroup.setExpIdx(idx)
		self.wall.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nbi_unitstruct_arraynbi_unitObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.pow_unit.putTimedElt(path, cpopath + 'pow_unit', i, obj)
		obj = self.inj_eng_unit.putTimedElt(path, cpopath + 'inj_eng_unit', i, obj)
		obj = self.beamcurrfrac.putTimedElt(path, cpopath + 'beamcurrfrac', i, obj)
		obj = self.beampowrfrac.putTimedElt(path, cpopath + 'beampowrfrac', i, obj)
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nbi_unitstruct_arraynbi_unitObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.pow_unit.getTimedElt(path, cpopath + 'pow_unit', i, obj)
		self.inj_eng_unit.getTimedElt(path, cpopath + 'inj_eng_unit', i, obj)
		self.beamcurrfrac.getTimedElt(path, cpopath + 'beamcurrfrac', i, obj)
		self.beampowrfrac.getTimedElt(path, cpopath + 'beampowrfrac', i, obj)
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nbi_unitstruct_arraynbi_unitObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		obj = self.inj_spec.putNonTimedElt(path, cpopath + 'inj_spec', i, obj)
		obj = self.pow_unit.putNonTimedElt(path, cpopath + 'pow_unit', i, obj)
		obj = self.inj_eng_unit.putNonTimedElt(path, cpopath + 'inj_eng_unit', i, obj)
		obj = self.beamcurrfrac.putNonTimedElt(path, cpopath + 'beamcurrfrac', i, obj)
		obj = self.beampowrfrac.putNonTimedElt(path, cpopath + 'beampowrfrac', i, obj)
		obj = self.beamletgroup.putNonTimedElt(path, cpopath + 'beamletgroup', i, obj)
		obj = self.wall.putNonTimedElt(path, cpopath + 'wall', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nbi_unitstruct_arraynbi_unitObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		self.inj_spec.getNonTimedElt(path, cpopath + 'inj_spec', i, obj)
		self.pow_unit.getNonTimedElt(path, cpopath + 'pow_unit', i, obj)
		self.inj_eng_unit.getNonTimedElt(path, cpopath + 'inj_eng_unit', i, obj)
		self.beamcurrfrac.getNonTimedElt(path, cpopath + 'beamcurrfrac', i, obj)
		self.beampowrfrac.getNonTimedElt(path, cpopath + 'beampowrfrac', i, obj)
		self.beamletgroup.getNonTimedElt(path, cpopath + 'beamletgroup', i, obj)
		self.wall.getNonTimedElt(path, cpopath + 'wall', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)


class inj_specstructureinj_spec:
	'''
	class inj_specstructureinj_spec
	Injected species

	Attributes:
	- amn : float
	   Atomic mass number
	- zn : float
	   Nuclear charge
	'''

	def __init__(self, base_path_in='inj_spec'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.amn = EMPTY_DOUBLE
		self.zn = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class inj_specstructureinj_spec\n'
		ret = ret + space + 'Attribute amn: ' + str(self.amn) + '\n'
		ret = ret + space + 'Attribute zn: ' + str(self.zn) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inj_specstructureinj_spec, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inj_specstructureinj_spec, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inj_specstructureinj_spec, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'amn', self.amn)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'zn', self.zn)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type inj_specstructureinj_spec, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_amn = ull.getDouble(self.idx, path, cpopath + 'amn')
		check_status(status)
		if not status:
			self.amn = ret_amn
		status, ret_zn = ull.getDouble(self.idx, path, cpopath + 'zn')
		check_status(status)
		if not status:
			self.zn = ret_zn

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type inj_specstructureinj_spec, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, amnVal = ull.getDouble(self.idx, path, cpopath + 'amn')
			check_status(status)
			status, znVal = ull.getDouble(self.idx, path, cpopath + 'zn')
			check_status(status)
			for i in range(nbslice):
				slice = inj_specstructureinj_spec(self.base_path)
				slice.setExpIdx(self.idx)
				slice.amn = amnVal
				slice.zn = znVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inj_specstructureinj_specObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inj_specstructureinj_specObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inj_specstructureinj_specObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'amn', i, self.amn)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zn', i, self.zn)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inj_specstructureinj_specObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		status, ret_amn = ull.getDoubleFromObject(self.idx, obj, cpopath + 'amn', i)
		check_status(status)
		if not status:
			self.amn = ret_amn
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		status, ret_zn = ull.getDoubleFromObject(self.idx, obj, cpopath + 'zn', i)
		check_status(status)
		if not status:
			self.zn = ret_zn

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'amn')
		ull.deleteData(self.idx, path, cpopath + 'zn')


class pow_unitstructureexp0D:
	'''
	class pow_unitstructureexp0D
	Power delivered by an NBI unit [W]; Time-dependent

	Attributes:
	- value : float
	   Signal value; Time-dependent; Scalar
	- abserror : float
	   Absolute error on signal; Time-dependent; Scalar
	- relerror : float
	   Relative error on signal (normalised to signal value); Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='pow_unit'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = EMPTY_DOUBLE
		self.abserror = EMPTY_DOUBLE
		self.relerror = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pow_unitstructureexp0D\n'
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		ret = ret + space + 'Attribute abserror: ' + str(self.abserror) + '\n'
		ret = ret + space + 'Attribute relerror: ' + str(self.relerror) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pow_unitstructureexp0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'value', self.value, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'abserror', self.abserror, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'relerror', self.relerror, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pow_unitstructureexp0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'value', self.value)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'abserror', self.abserror)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'relerror', self.relerror)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pow_unitstructureexp0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pow_unitstructureexp0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_abserror, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pow_unitstructureexp0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect1DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (nbslice))
			check_status(status)
			status, abserrorList = ull.getVect1DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (nbslice))
			check_status(status)
			status, relerrorList = ull.getVect1DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = pow_unitstructureexp0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[i].copy().astype(float)
				slice.abserror = abserrorList[i].copy().astype(float)
				slice.relerror = relerrorList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_unitstructureexp0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'value', i, self.value)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'abserror', i, self.abserror)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'relerror', i, self.relerror)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_unitstructureexp0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_unitstructureexp0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_unitstructureexp0DObj, run function getNonTimedElt') 
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


class inj_eng_unitstructureexp0D:
	'''
	class inj_eng_unitstructureexp0D
	Full injection energy of a unit [ev]; Time-dependent

	Attributes:
	- value : float
	   Signal value; Time-dependent; Scalar
	- abserror : float
	   Absolute error on signal; Time-dependent; Scalar
	- relerror : float
	   Relative error on signal (normalised to signal value); Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='inj_eng_unit'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = EMPTY_DOUBLE
		self.abserror = EMPTY_DOUBLE
		self.relerror = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class inj_eng_unitstructureexp0D\n'
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		ret = ret + space + 'Attribute abserror: ' + str(self.abserror) + '\n'
		ret = ret + space + 'Attribute relerror: ' + str(self.relerror) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inj_eng_unitstructureexp0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'value', self.value, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'abserror', self.abserror, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'relerror', self.relerror, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inj_eng_unitstructureexp0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'value', self.value)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'abserror', self.abserror)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'relerror', self.relerror)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type inj_eng_unitstructureexp0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type inj_eng_unitstructureexp0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_abserror, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type inj_eng_unitstructureexp0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect1DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (nbslice))
			check_status(status)
			status, abserrorList = ull.getVect1DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (nbslice))
			check_status(status)
			status, relerrorList = ull.getVect1DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = inj_eng_unitstructureexp0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[i].copy().astype(float)
				slice.abserror = abserrorList[i].copy().astype(float)
				slice.relerror = relerrorList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inj_eng_unitstructureexp0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'value', i, self.value)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'abserror', i, self.abserror)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'relerror', i, self.relerror)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inj_eng_unitstructureexp0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inj_eng_unitstructureexp0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type inj_eng_unitstructureexp0DObj, run function getNonTimedElt') 
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


class beamcurrfracstructureexp1D:
	'''
	class beamcurrfracstructureexp1D
	Beam current fractions; beamcurrfrac(j) is the fraction of the beam current from beam neutrals with the j:th harmonic energy, inj_eng_unit. Vector(3); Time-dependent

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='beamcurrfrac'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class beamcurrfracstructureexp1D\n'
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
			print ('field '+self.base_path+' of type beamcurrfracstructureexp1D, run function putSlice') 
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
			print ('field '+self.base_path+' of type beamcurrfracstructureexp1D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type beamcurrfracstructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type beamcurrfracstructureexp1D, run function getSlice') 
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
			print ('field '+self.base_path+' of type beamcurrfracstructureexp1D, run function build_non_resampled_data') 
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
				slice = beamcurrfracstructureexp1D(self.base_path)
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
			print ('object of type beamcurrfracstructureexp1DObj, run function putTimedElt') 
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
			print ('object of type beamcurrfracstructureexp1DObj, run function getTimedElt') 
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
			print ('object of type beamcurrfracstructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamcurrfracstructureexp1DObj, run function getNonTimedElt') 
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


class beampowrfracstructureexp1D:
	'''
	class beampowrfracstructureexp1D
	Beam power fractions; beampowrfrac(j) is the fraction of the beam power from beam neutrals with the j:th harmonic energy, inj_eng_unit;. Vector(3); Time-dependent

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='beampowrfrac'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class beampowrfracstructureexp1D\n'
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
			print ('field '+self.base_path+' of type beampowrfracstructureexp1D, run function putSlice') 
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
			print ('field '+self.base_path+' of type beampowrfracstructureexp1D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type beampowrfracstructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type beampowrfracstructureexp1D, run function getSlice') 
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
			print ('field '+self.base_path+' of type beampowrfracstructureexp1D, run function build_non_resampled_data') 
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
				slice = beampowrfracstructureexp1D(self.base_path)
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
			print ('object of type beampowrfracstructureexp1DObj, run function putTimedElt') 
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
			print ('object of type beampowrfracstructureexp1DObj, run function getTimedElt') 
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
			print ('object of type beampowrfracstructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beampowrfracstructureexp1DObj, run function getNonTimedElt') 
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


class beamletgroupstruct_arraybeamletgroup:
	'''
	class beamletgroupstruct_arraybeamletgroup
	Group of beamlets with common vertical and horizontal focal point. If there are no common focal points, then select small groups of beamlets such that a focal point description of the beamlet-group provides a fair description.

	Attributes:
	- array : list of beamletgroupstruct_arraybeamletgroupObj 
	'''

	def __init__(self, base_path_in='beamletgroup'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class beamletgroupstruct_arraybeamletgroup\n'
		for i in range(len(self.array)):
			ret = ret + space + 'beamletgroupstruct_arraybeamletgroup[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(beamletgroupstruct_arraybeamletgroupObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type beamletgroupstruct_arraybeamletgroup, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type beamletgroupstruct_arraybeamletgroup, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type beamletgroupstruct_arraybeamletgroup, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type beamletgroupstruct_arraybeamletgroup, run function getSlice') 
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type beamletgroupstruct_arraybeamletgroup, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(beamletgroupstruct_arraybeamletgroup(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = beamletgroupstruct_arraybeamletgroup(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getNonTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type beamletgroupstruct_arraybeamletgroup, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type beamletgroupstruct_arraybeamletgroup, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class beamletgroupstruct_arraybeamletgroupObj:
	'''
	class beamletgroupstruct_arraybeamletgroupObj
	Group of beamlets with common vertical and horizontal focal point. If there are no common focal points, then select small groups of beamlets such that a focal point description of the beamlet-group provides a fair description.

	Attributes:
	- position : class positionstructurerzphi0D
	   Position of centre of injection unit surface (or grounded grid).
	- tang_rad : float
	   Tangency radius (major radius where the central line of a NBI unit is tangent to a circle around the torus) [m]
	- angle : float
	   Angle of inclination between a line at the centre of the injection unit surface and the horiontal plane [rad]
	- direction : int
	   Direction of the beam seen from above the torus: -1 = clockwise; 1 = counter clockwise
	- width_horiz : float
	   Horizontal width of the beam group at the injection unit surface (or grounded grid) [m]
	- width_vert : float
	   Vertical width of the beam group at the injection unit surface (or grounded grid) [m]
	- focussing : class focussingstructurefocussing
	   Describes how the beam is focussed.
	- divergence : class divergencestructuredivergence
	   Detailed information on beamlet divergence. Divergens is described as a super position of Gaussian profiles with amplitide "frac_divcomp" and vertical/horizontal divergence "div_vert"/"div_horiz". Note that for positive ion NBI the divergence is well described by a single Gaussian. 
	- beamlets : class beamletsstructurebeamlets
	   Detailed information on beamlets.
	'''

	def __init__(self, base_path_in='beamletgroup'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.position = positionstructurerzphi0D('position')
		self.tang_rad = EMPTY_DOUBLE
		self.angle = EMPTY_DOUBLE
		self.direction = EMPTY_INT
		self.width_horiz = EMPTY_DOUBLE
		self.width_vert = EMPTY_DOUBLE
		self.focussing = focussingstructurefocussing('focussing')
		self.divergence = divergencestructuredivergence('divergence')
		self.beamlets = beamletsstructurebeamlets('beamlets')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class beamletgroupstruct_arraybeamletgroupObj\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		ret = ret + space + 'Attribute tang_rad: ' + str(self.tang_rad) + '\n'
		ret = ret + space + 'Attribute angle: ' + str(self.angle) + '\n'
		ret = ret + space + 'Attribute direction: ' + str(self.direction) + '\n'
		ret = ret + space + 'Attribute width_horiz: ' + str(self.width_horiz) + '\n'
		ret = ret + space + 'Attribute width_vert: ' + str(self.width_vert) + '\n'
		ret = ret + space + 'Attribute focussing\n ' + self.focussing.__str__(depth+1)
		ret = ret + space + 'Attribute divergence\n ' + self.divergence.__str__(depth+1)
		ret = ret + space + 'Attribute beamlets\n ' + self.beamlets.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.position.setExpIdx(idx)
		self.focussing.setExpIdx(idx)
		self.divergence.setExpIdx(idx)
		self.beamlets.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamletgroupstruct_arraybeamletgroupObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'tang_rad') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'tang_rad', i, self.tang_rad)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'angle') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'angle', i, self.angle)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'direction') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'direction', i, self.direction)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'width_horiz') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'width_horiz', i, self.width_horiz)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'width_vert') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'width_vert', i, self.width_vert)
		obj = self.focussing.putNonTimedElt(path, cpopath + 'focussing', i, obj)
		obj = self.divergence.putNonTimedElt(path, cpopath + 'divergence', i, obj)
		obj = self.beamlets.putNonTimedElt(path, cpopath + 'beamlets', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamletgroupstruct_arraybeamletgroupObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'tang_rad') 
			print ('obj = ' + str(obj))
		status, ret_tang_rad = ull.getDoubleFromObject(self.idx, obj, cpopath + 'tang_rad', i)
		check_status(status)
		if not status:
			self.tang_rad = ret_tang_rad
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'angle') 
			print ('obj = ' + str(obj))
		status, ret_angle = ull.getDoubleFromObject(self.idx, obj, cpopath + 'angle', i)
		check_status(status)
		if not status:
			self.angle = ret_angle
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'direction') 
			print ('obj = ' + str(obj))
		status, ret_direction = ull.getIntFromObject(self.idx, obj, cpopath + 'direction', i)
		check_status(status)
		if not status:
			self.direction = ret_direction
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'width_horiz') 
			print ('obj = ' + str(obj))
		status, ret_width_horiz = ull.getDoubleFromObject(self.idx, obj, cpopath + 'width_horiz', i)
		check_status(status)
		if not status:
			self.width_horiz = ret_width_horiz
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'width_vert') 
			print ('obj = ' + str(obj))
		status, ret_width_vert = ull.getDoubleFromObject(self.idx, obj, cpopath + 'width_vert', i)
		check_status(status)
		if not status:
			self.width_vert = ret_width_vert
		self.focussing.getNonTimedElt(path, cpopath + 'focussing', i, obj)
		self.divergence.getNonTimedElt(path, cpopath + 'divergence', i, obj)
		self.beamlets.getNonTimedElt(path, cpopath + 'beamlets', i, obj)


class positionstructurerzphi0D:
	'''
	class positionstructurerzphi0D
	Position of centre of injection unit surface (or grounded grid).

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	- phi : float
	   Toroidal angle [rad]
	'''

	def __init__(self, base_path_in='position'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE
		self.phi = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class positionstructurerzphi0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		ret = ret + space + 'Attribute phi: ' + str(self.phi) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'r', self.r)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'z', self.z)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'phi', self.phi)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r = ull.getDouble(self.idx, path, cpopath + 'r')
		check_status(status)
		if not status:
			self.r = ret_r
		status, ret_z = ull.getDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z
		status, ret_phi = ull.getDouble(self.idx, path, cpopath + 'phi')
		check_status(status)
		if not status:
			self.phi = ret_phi

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rVal = ull.getDouble(self.idx, path, cpopath + 'r')
			check_status(status)
			status, zVal = ull.getDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			status, phiVal = ull.getDouble(self.idx, path, cpopath + 'phi')
			check_status(status)
			for i in range(nbslice):
				slice = positionstructurerzphi0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.z = zVal
				slice.phi = phiVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r', i, self.r)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'phi', i, self.phi)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		status, ret_r = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r', i)
		check_status(status)
		if not status:
			self.r = ret_r
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getDoubleFromObject(self.idx, obj, cpopath + 'z', i)
		check_status(status)
		if not status:
			self.z = ret_z
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'z')
		ull.deleteData(self.idx, path, cpopath + 'phi')


class focussingstructurefocussing:
	'''
	class focussingstructurefocussing
	Describes how the beam is focussed.

	Attributes:
	- focal_len_hz : float
	   Horizontal focal length along the beam line, i.e. the point along the centre of the beamlet-group where the beamlet-group has its minimum horizontal width [m]. Scalar
	- focal_len_vc : float
	   Vertical focal length along the beam line, i.e. the point along the centre of the beamlet-group where the beamlet-group has its minimum vertical width [m]. Scalar
	- width_min_hz : float
	   The horizontal width of the beamlet-group at the at the horizontal focal point [m]. Scalar
	- width_min_vc : float
	   The vertical width of the beamlet-group at the at the vertical focal point [m]. Scalar
	'''

	def __init__(self, base_path_in='focussing'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.focal_len_hz = EMPTY_DOUBLE
		self.focal_len_vc = EMPTY_DOUBLE
		self.width_min_hz = EMPTY_DOUBLE
		self.width_min_vc = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class focussingstructurefocussing\n'
		ret = ret + space + 'Attribute focal_len_hz: ' + str(self.focal_len_hz) + '\n'
		ret = ret + space + 'Attribute focal_len_vc: ' + str(self.focal_len_vc) + '\n'
		ret = ret + space + 'Attribute width_min_hz: ' + str(self.width_min_hz) + '\n'
		ret = ret + space + 'Attribute width_min_vc: ' + str(self.width_min_vc) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type focussingstructurefocussing, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type focussingstructurefocussing, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type focussingstructurefocussing, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'focal_len_hz', self.focal_len_hz)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'focal_len_vc', self.focal_len_vc)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'width_min_hz', self.width_min_hz)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'width_min_vc', self.width_min_vc)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type focussingstructurefocussing, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_focal_len_hz = ull.getDouble(self.idx, path, cpopath + 'focal_len_hz')
		check_status(status)
		if not status:
			self.focal_len_hz = ret_focal_len_hz
		status, ret_focal_len_vc = ull.getDouble(self.idx, path, cpopath + 'focal_len_vc')
		check_status(status)
		if not status:
			self.focal_len_vc = ret_focal_len_vc
		status, ret_width_min_hz = ull.getDouble(self.idx, path, cpopath + 'width_min_hz')
		check_status(status)
		if not status:
			self.width_min_hz = ret_width_min_hz
		status, ret_width_min_vc = ull.getDouble(self.idx, path, cpopath + 'width_min_vc')
		check_status(status)
		if not status:
			self.width_min_vc = ret_width_min_vc

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type focussingstructurefocussing, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, focal_len_hzVal = ull.getDouble(self.idx, path, cpopath + 'focal_len_hz')
			check_status(status)
			status, focal_len_vcVal = ull.getDouble(self.idx, path, cpopath + 'focal_len_vc')
			check_status(status)
			status, width_min_hzVal = ull.getDouble(self.idx, path, cpopath + 'width_min_hz')
			check_status(status)
			status, width_min_vcVal = ull.getDouble(self.idx, path, cpopath + 'width_min_vc')
			check_status(status)
			for i in range(nbslice):
				slice = focussingstructurefocussing(self.base_path)
				slice.setExpIdx(self.idx)
				slice.focal_len_hz = focal_len_hzVal
				slice.focal_len_vc = focal_len_vcVal
				slice.width_min_hz = width_min_hzVal
				slice.width_min_vc = width_min_vcVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type focussingstructurefocussingObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type focussingstructurefocussingObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type focussingstructurefocussingObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'focal_len_hz') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'focal_len_hz', i, self.focal_len_hz)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'focal_len_vc') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'focal_len_vc', i, self.focal_len_vc)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'width_min_hz') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'width_min_hz', i, self.width_min_hz)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'width_min_vc') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'width_min_vc', i, self.width_min_vc)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type focussingstructurefocussingObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'focal_len_hz') 
			print ('obj = ' + str(obj))
		status, ret_focal_len_hz = ull.getDoubleFromObject(self.idx, obj, cpopath + 'focal_len_hz', i)
		check_status(status)
		if not status:
			self.focal_len_hz = ret_focal_len_hz
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'focal_len_vc') 
			print ('obj = ' + str(obj))
		status, ret_focal_len_vc = ull.getDoubleFromObject(self.idx, obj, cpopath + 'focal_len_vc', i)
		check_status(status)
		if not status:
			self.focal_len_vc = ret_focal_len_vc
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'width_min_hz') 
			print ('obj = ' + str(obj))
		status, ret_width_min_hz = ull.getDoubleFromObject(self.idx, obj, cpopath + 'width_min_hz', i)
		check_status(status)
		if not status:
			self.width_min_hz = ret_width_min_hz
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'width_min_vc') 
			print ('obj = ' + str(obj))
		status, ret_width_min_vc = ull.getDoubleFromObject(self.idx, obj, cpopath + 'width_min_vc', i)
		check_status(status)
		if not status:
			self.width_min_vc = ret_width_min_vc

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'focal_len_hz')
		ull.deleteData(self.idx, path, cpopath + 'focal_len_vc')
		ull.deleteData(self.idx, path, cpopath + 'width_min_hz')
		ull.deleteData(self.idx, path, cpopath + 'width_min_vc')


class divergencestructuredivergence:
	'''
	class divergencestructuredivergence
	Detailed information on beamlet divergence. Divergens is described as a super position of Gaussian profiles with amplitide "frac_divcomp" and vertical/horizontal divergence "div_vert"/"div_horiz". Note that for positive ion NBI the divergence is well described by a single Gaussian. 

	Attributes:
	- frac_divcomp : numpy.ndarray 1D with float
	   Fraction of injected particles. Vector(ndiv_comp)
	- div_vert : numpy.ndarray 1D with float
	   The vertical beamlet divergence [rad]. Here the divergence is defined for Gaussian beams as the angel where the beam density is reduced by a factor 1/e compared to the maximum density. For non-Gaussian beams the divergence is sqrt(2)*mean((x-mean(x))**2), where x is the angle and the mean should be performed over the beam density, P(x): mean(y)=int(y*P(x)*dx). Vector(ndiv_comp)
	- div_horiz : numpy.ndarray 1D with float
	   The horizontal beamlet divergence [rad]. Here the divergence is defined for Gaussian beams as the angel where the beam density is reduced by a factor 1/e compared to the maximum density. For non-Gaussian beams the divergence is sqrt(2)*mean((x-mean(x))**2), where x is the angle and the mean should be performed over the beam density, P(x): mean(y)=int(y*P(x)*dx). Vector(ndiv_comp)
	'''

	def __init__(self, base_path_in='divergence'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.frac_divcomp = numpy.zeros(0, numpy.float64, order='C')
		self.div_vert = numpy.zeros(0, numpy.float64, order='C')
		self.div_horiz = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class divergencestructuredivergence\n'
		s = self.frac_divcomp.__str__()
		ret = ret + space + 'Attribute frac_divcomp\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.div_vert.__str__()
		ret = ret + space + 'Attribute div_vert\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.div_horiz.__str__()
		ret = ret + space + 'Attribute div_horiz\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type divergencestructuredivergence, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type divergencestructuredivergence, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type divergencestructuredivergence, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'frac_divcomp', numpy.array(self.frac_divcomp).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'div_vert', numpy.array(self.div_vert).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'div_horiz', numpy.array(self.div_horiz).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type divergencestructuredivergence, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_frac_divcomp = ull.getVect1DDouble(self.idx, path, cpopath + 'frac_divcomp')
		check_status(status)
		if not status:
			self.frac_divcomp = ret_frac_divcomp
		status, ret_div_vert = ull.getVect1DDouble(self.idx, path, cpopath + 'div_vert')
		check_status(status)
		if not status:
			self.div_vert = ret_div_vert
		status, ret_div_horiz = ull.getVect1DDouble(self.idx, path, cpopath + 'div_horiz')
		check_status(status)
		if not status:
			self.div_horiz = ret_div_horiz

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type divergencestructuredivergence, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, frac_divcompVal = ull.getVect1DDouble(self.idx, path, cpopath + 'frac_divcomp')
			check_status(status)
			status, div_vertVal = ull.getVect1DDouble(self.idx, path, cpopath + 'div_vert')
			check_status(status)
			status, div_horizVal = ull.getVect1DDouble(self.idx, path, cpopath + 'div_horiz')
			check_status(status)
			for i in range(nbslice):
				slice = divergencestructuredivergence(self.base_path)
				slice.setExpIdx(self.idx)
				slice.frac_divcomp = frac_divcompVal
				slice.div_vert = div_vertVal
				slice.div_horiz = div_horizVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type divergencestructuredivergenceObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type divergencestructuredivergenceObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type divergencestructuredivergenceObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'frac_divcomp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'frac_divcomp', i, numpy.array(self.frac_divcomp).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'div_vert') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'div_vert', i, numpy.array(self.div_vert).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'div_horiz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'div_horiz', i, numpy.array(self.div_horiz).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type divergencestructuredivergenceObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'frac_divcomp') 
			print ('obj = ' + str(obj))
		status, ret_frac_divcomp = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'frac_divcomp', i)
		check_status(status)
		if not status:
			self.frac_divcomp = ret_frac_divcomp
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'div_vert') 
			print ('obj = ' + str(obj))
		status, ret_div_vert = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'div_vert', i)
		check_status(status)
		if not status:
			self.div_vert = ret_div_vert
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'div_horiz') 
			print ('obj = ' + str(obj))
		status, ret_div_horiz = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'div_horiz', i)
		check_status(status)
		if not status:
			self.div_horiz = ret_div_horiz

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'frac_divcomp')
		ull.deleteData(self.idx, path, cpopath + 'div_vert')
		ull.deleteData(self.idx, path, cpopath + 'div_horiz')


class beamletsstructurebeamlets:
	'''
	class beamletsstructurebeamlets
	Detailed information on beamlets.

	Attributes:
	- position : class positionstructurerzphi1D
	   Position of beamlets. Vector rzphi1D (nbeamlets)
	- tang_rad_blt : numpy.ndarray 1D with float
	   Tangency radius (major radius where the central line of a beamlet is tangent to a circle around the torus) [m]; Vector(nbeamlets)
	- angle_blt : numpy.ndarray 1D with float
	   Angle of inclination between a line at the centre of a beamlet and the horiontal plane [rad]; Vector(nbeamlets)
	- pow_frc_blt : numpy.ndarray 1D with float
	   Fraction of power of a unit injected by a beamlet; Vector(nbeamlets)
	'''

	def __init__(self, base_path_in='beamlets'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.position = positionstructurerzphi1D('position')
		self.tang_rad_blt = numpy.zeros(0, numpy.float64, order='C')
		self.angle_blt = numpy.zeros(0, numpy.float64, order='C')
		self.pow_frc_blt = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class beamletsstructurebeamlets\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		s = self.tang_rad_blt.__str__()
		ret = ret + space + 'Attribute tang_rad_blt\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.angle_blt.__str__()
		ret = ret + space + 'Attribute angle_blt\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_frc_blt.__str__()
		ret = ret + space + 'Attribute pow_frc_blt\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.position.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type beamletsstructurebeamlets, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.cpoTime = self.cpoTime
		self.position.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type beamletsstructurebeamlets, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type beamletsstructurebeamlets, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.putNonTimed(path, cpopath)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'tang_rad_blt', numpy.array(self.tang_rad_blt).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'angle_blt', numpy.array(self.angle_blt).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'pow_frc_blt', numpy.array(self.pow_frc_blt).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type beamletsstructurebeamlets, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_tang_rad_blt = ull.getVect1DDouble(self.idx, path, cpopath + 'tang_rad_blt')
		check_status(status)
		if not status:
			self.tang_rad_blt = ret_tang_rad_blt
		status, ret_angle_blt = ull.getVect1DDouble(self.idx, path, cpopath + 'angle_blt')
		check_status(status)
		if not status:
			self.angle_blt = ret_angle_blt
		status, ret_pow_frc_blt = ull.getVect1DDouble(self.idx, path, cpopath + 'pow_frc_blt')
		check_status(status)
		if not status:
			self.pow_frc_blt = ret_pow_frc_blt

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type beamletsstructurebeamlets, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			positionList = self.position.build_non_resampled_data(path, cpopath, nbslice)
			status, tang_rad_bltVal = ull.getVect1DDouble(self.idx, path, cpopath + 'tang_rad_blt')
			check_status(status)
			status, angle_bltVal = ull.getVect1DDouble(self.idx, path, cpopath + 'angle_blt')
			check_status(status)
			status, pow_frc_bltVal = ull.getVect1DDouble(self.idx, path, cpopath + 'pow_frc_blt')
			check_status(status)
			for i in range(nbslice):
				slice = beamletsstructurebeamlets(self.base_path)
				slice.setExpIdx(self.idx)
				slice.position = positionList[i]
				slice.tang_rad_blt = tang_rad_bltVal
				slice.angle_blt = angle_bltVal
				slice.pow_frc_blt = pow_frc_bltVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamletsstructurebeamletsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamletsstructurebeamletsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamletsstructurebeamletsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'tang_rad_blt') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'tang_rad_blt', i, numpy.array(self.tang_rad_blt).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'angle_blt') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'angle_blt', i, numpy.array(self.angle_blt).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pow_frc_blt') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pow_frc_blt', i, numpy.array(self.pow_frc_blt).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamletsstructurebeamletsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'tang_rad_blt') 
			print ('obj = ' + str(obj))
		status, ret_tang_rad_blt = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'tang_rad_blt', i)
		check_status(status)
		if not status:
			self.tang_rad_blt = ret_tang_rad_blt
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'angle_blt') 
			print ('obj = ' + str(obj))
		status, ret_angle_blt = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'angle_blt', i)
		check_status(status)
		if not status:
			self.angle_blt = ret_angle_blt
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pow_frc_blt') 
			print ('obj = ' + str(obj))
		status, ret_pow_frc_blt = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pow_frc_blt', i)
		check_status(status)
		if not status:
			self.pow_frc_blt = ret_pow_frc_blt

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'tang_rad_blt')
		ull.deleteData(self.idx, path, cpopath + 'angle_blt')
		ull.deleteData(self.idx, path, cpopath + 'pow_frc_blt')


class positionstructurerzphi1D:
	'''
	class positionstructurerzphi1D
	Position of beamlets. Vector rzphi1D (nbeamlets)

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]
	- z : numpy.ndarray 1D with float
	   Altitude [m]
	- phi : numpy.ndarray 1D with float
	   Toroidal angle [rad]
	'''

	def __init__(self, base_path_in='position'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class positionstructurerzphi1D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi1D, run function getSlice') 
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
		status, ret_z = ull.getVect1DDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z
		status, ret_phi = ull.getVect1DDouble(self.idx, path, cpopath + 'phi')
		check_status(status)
		if not status:
			self.phi = ret_phi

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerzphi1D, run function build_non_resampled_data') 
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
			status, zVal = ull.getVect1DDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			status, phiVal = ull.getVect1DDouble(self.idx, path, cpopath + 'phi')
			check_status(status)
			for i in range(nbslice):
				slice = positionstructurerzphi1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.z = zVal
				slice.phi = phiVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerzphi1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		status, ret_r = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'r', i)
		check_status(status)
		if not status:
			self.r = ret_r
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'z', i)
		check_status(status)
		if not status:
			self.z = ret_z
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'z')
		ull.deleteData(self.idx, path, cpopath + 'phi')


class wallstructurenbi_nbi_unit_wall:
	'''
	class wallstructurenbi_nbi_unit_wall
	Description of the wall components in the NBI system that limits the beam spatial width of the beam. The wall is here described a superposition of surface segments and collimating holes.

	Attributes:
	- surface : class surfacestructurenbi_nbi_unit_wall_surface
	   A collimating solid surface described by a polygon; no particle can pass through this surface  
	- collimator : class collimatorstruct_arrayflat_polygon: array of collimatorstruct_arrayflat_polygonObj objects
	   Vector of collimating holes (openings). Each hole has to be flat, i.e. it lies on a surface. Particles can only cross this surface by passing through the hole. To describe the hole we first construct a coordinate system on the surface by defining the original and two basis vectors in (x,y,z) space. The polyon is then represented as the origin, plus a linear combination of the two basis vectors using coord1 and coord2. As an example, a rectangle with two of the corners given by "origin+basis1" and "origin+basis2" can be described using coord1=[1,0,-1,0] and coord2=[0,1,0,-1].
	'''

	def __init__(self, base_path_in='wall'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.surface = surfacestructurenbi_nbi_unit_wall_surface('surface')
		self.collimator = collimatorstruct_arrayflat_polygon('collimator')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class wallstructurenbi_nbi_unit_wall\n'
		ret = ret + space + 'Attribute surface\n ' + self.surface.__str__(depth+1)
		ret = ret + space + 'Attribute collimator\n ' + self.collimator.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.surface.setExpIdx(idx)
		self.collimator.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wallstructurenbi_nbi_unit_wall, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.surface.cpoTime = self.cpoTime
		self.surface.putSlice(path, cpopath)
		self.collimator.cpoTime = self.cpoTime
		self.collimator.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wallstructurenbi_nbi_unit_wall, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.surface.replaceLastSlice(path, cpopath)
		self.collimator.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wallstructurenbi_nbi_unit_wall, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.surface.putNonTimed(path, cpopath)
		self.collimator.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type wallstructurenbi_nbi_unit_wall, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.surface.getSlice(path, cpopath, inTime, interpolMode)
		self.collimator.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type wallstructurenbi_nbi_unit_wall, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			surfaceList = self.surface.build_non_resampled_data(path, cpopath, nbslice)
			collimatorList = self.collimator.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = wallstructurenbi_nbi_unit_wall(self.base_path)
				slice.setExpIdx(self.idx)
				slice.surface = surfaceList[i]
				slice.collimator = collimatorList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wallstructurenbi_nbi_unit_wallObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wallstructurenbi_nbi_unit_wallObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wallstructurenbi_nbi_unit_wallObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.surface.putNonTimedElt(path, cpopath + 'surface', i, obj)
		obj = self.collimator.putNonTimedElt(path, cpopath + 'collimator', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wallstructurenbi_nbi_unit_wallObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.surface.getNonTimedElt(path, cpopath + 'surface', i, obj)
		self.collimator.getNonTimedElt(path, cpopath + 'collimator', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.surface.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'collimator')


class surfacestructurenbi_nbi_unit_wall_surface:
	'''
	class surfacestructurenbi_nbi_unit_wall_surface
	A collimating solid surface described by a polygon; no particle can pass through this surface  

	Attributes:
	- triangle : class trianglestruct_arraytrianglexyz: array of trianglestruct_arraytrianglexyzObj objects
	   Triangular wall surface described by its three corners: point1, point2, and point3. Vector(n_triangles)
	- rectangle : class rectanglestruct_arrayrectanglexyz: array of rectanglestruct_arrayrectanglexyzObj objects
	   Rectangular wall surface described by its four corners. These form an ordered sequence: point00, point01, point11, point10. Here the first point should be calculated from the other three as point00=point01+poin10-point11. Vector(n_rectangles)
	'''

	def __init__(self, base_path_in='surface'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.triangle = trianglestruct_arraytrianglexyz('triangle')
		self.rectangle = rectanglestruct_arrayrectanglexyz('rectangle')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class surfacestructurenbi_nbi_unit_wall_surface\n'
		ret = ret + space + 'Attribute triangle\n ' + self.triangle.__str__(depth+1)
		ret = ret + space + 'Attribute rectangle\n ' + self.rectangle.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.triangle.setExpIdx(idx)
		self.rectangle.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type surfacestructurenbi_nbi_unit_wall_surface, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.triangle.cpoTime = self.cpoTime
		self.triangle.putSlice(path, cpopath)
		self.rectangle.cpoTime = self.cpoTime
		self.rectangle.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type surfacestructurenbi_nbi_unit_wall_surface, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.triangle.replaceLastSlice(path, cpopath)
		self.rectangle.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type surfacestructurenbi_nbi_unit_wall_surface, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.triangle.putNonTimed(path, cpopath)
		self.rectangle.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type surfacestructurenbi_nbi_unit_wall_surface, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.triangle.getSlice(path, cpopath, inTime, interpolMode)
		self.rectangle.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type surfacestructurenbi_nbi_unit_wall_surface, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			triangleList = self.triangle.build_non_resampled_data(path, cpopath, nbslice)
			rectangleList = self.rectangle.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = surfacestructurenbi_nbi_unit_wall_surface(self.base_path)
				slice.setExpIdx(self.idx)
				slice.triangle = triangleList[i]
				slice.rectangle = rectangleList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type surfacestructurenbi_nbi_unit_wall_surfaceObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type surfacestructurenbi_nbi_unit_wall_surfaceObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type surfacestructurenbi_nbi_unit_wall_surfaceObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.triangle.putNonTimedElt(path, cpopath + 'triangle', i, obj)
		obj = self.rectangle.putNonTimedElt(path, cpopath + 'rectangle', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type surfacestructurenbi_nbi_unit_wall_surfaceObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.triangle.getNonTimedElt(path, cpopath + 'triangle', i, obj)
		self.rectangle.getNonTimedElt(path, cpopath + 'rectangle', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'triangle')
		ull.deleteData(self.idx, path, cpopath + 'rectangle')


class trianglestruct_arraytrianglexyz:
	'''
	class trianglestruct_arraytrianglexyz
	Triangular wall surface described by its three corners: point1, point2, and point3. Vector(n_triangles)

	Attributes:
	- array : list of trianglestruct_arraytrianglexyzObj 
	'''

	def __init__(self, base_path_in='triangle'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class trianglestruct_arraytrianglexyz\n'
		for i in range(len(self.array)):
			ret = ret + space + 'trianglestruct_arraytrianglexyz[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(trianglestruct_arraytrianglexyzObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trianglestruct_arraytrianglexyz, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trianglestruct_arraytrianglexyz, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trianglestruct_arraytrianglexyz, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type trianglestruct_arraytrianglexyz, run function getSlice') 
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type trianglestruct_arraytrianglexyz, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(trianglestruct_arraytrianglexyz(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = trianglestruct_arraytrianglexyz(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getNonTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type trianglestruct_arraytrianglexyz, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type trianglestruct_arraytrianglexyz, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class trianglestruct_arraytrianglexyzObj:
	'''
	class trianglestruct_arraytrianglexyzObj
	Triangular wall surface described by its three corners: point1, point2, and point3. Vector(n_triangles)

	Attributes:
	- point1 : class point1structurexyz0D
	   Point 1 on the triangle
	- point2 : class point2structurexyz0D
	   Point 2 on the triangle
	- point3 : class point3structurexyz0D
	   Point 3 on the triangle
	'''

	def __init__(self, base_path_in='triangle'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.point1 = point1structurexyz0D('point1')
		self.point2 = point2structurexyz0D('point2')
		self.point3 = point3structurexyz0D('point3')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class trianglestruct_arraytrianglexyzObj\n'
		ret = ret + space + 'Attribute point1\n ' + self.point1.__str__(depth+1)
		ret = ret + space + 'Attribute point2\n ' + self.point2.__str__(depth+1)
		ret = ret + space + 'Attribute point3\n ' + self.point3.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.point1.setExpIdx(idx)
		self.point2.setExpIdx(idx)
		self.point3.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trianglestruct_arraytrianglexyzObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.point1.putNonTimedElt(path, cpopath + 'point1', i, obj)
		obj = self.point2.putNonTimedElt(path, cpopath + 'point2', i, obj)
		obj = self.point3.putNonTimedElt(path, cpopath + 'point3', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trianglestruct_arraytrianglexyzObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.point1.getNonTimedElt(path, cpopath + 'point1', i, obj)
		self.point2.getNonTimedElt(path, cpopath + 'point2', i, obj)
		self.point3.getNonTimedElt(path, cpopath + 'point3', i, obj)


class point1structurexyz0D:
	'''
	class point1structurexyz0D
	Point 1 on the triangle

	Attributes:
	- x : float
	   Spatial coordinate x [m]
	- y : float
	   Spatial coordinate y [m]
	- z : float
	   Spatial coordinate z [m]
	'''

	def __init__(self, base_path_in='point1'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.x = EMPTY_DOUBLE
		self.y = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class point1structurexyz0D\n'
		ret = ret + space + 'Attribute x: ' + str(self.x) + '\n'
		ret = ret + space + 'Attribute y: ' + str(self.y) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point1structurexyz0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point1structurexyz0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point1structurexyz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'x', self.x)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'y', self.y)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'z', self.z)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type point1structurexyz0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_x = ull.getDouble(self.idx, path, cpopath + 'x')
		check_status(status)
		if not status:
			self.x = ret_x
		status, ret_y = ull.getDouble(self.idx, path, cpopath + 'y')
		check_status(status)
		if not status:
			self.y = ret_y
		status, ret_z = ull.getDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type point1structurexyz0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, xVal = ull.getDouble(self.idx, path, cpopath + 'x')
			check_status(status)
			status, yVal = ull.getDouble(self.idx, path, cpopath + 'y')
			check_status(status)
			status, zVal = ull.getDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = point1structurexyz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.x = xVal
				slice.y = yVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point1structurexyz0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point1structurexyz0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point1structurexyz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'x', i, self.x)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'y', i, self.y)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point1structurexyz0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		status, ret_x = ull.getDoubleFromObject(self.idx, obj, cpopath + 'x', i)
		check_status(status)
		if not status:
			self.x = ret_x
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		status, ret_y = ull.getDoubleFromObject(self.idx, obj, cpopath + 'y', i)
		check_status(status)
		if not status:
			self.y = ret_y
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getDoubleFromObject(self.idx, obj, cpopath + 'z', i)
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
		ull.deleteData(self.idx, path, cpopath + 'x')
		ull.deleteData(self.idx, path, cpopath + 'y')
		ull.deleteData(self.idx, path, cpopath + 'z')


class point2structurexyz0D:
	'''
	class point2structurexyz0D
	Point 2 on the triangle

	Attributes:
	- x : float
	   Spatial coordinate x [m]
	- y : float
	   Spatial coordinate y [m]
	- z : float
	   Spatial coordinate z [m]
	'''

	def __init__(self, base_path_in='point2'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.x = EMPTY_DOUBLE
		self.y = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class point2structurexyz0D\n'
		ret = ret + space + 'Attribute x: ' + str(self.x) + '\n'
		ret = ret + space + 'Attribute y: ' + str(self.y) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point2structurexyz0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point2structurexyz0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point2structurexyz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'x', self.x)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'y', self.y)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'z', self.z)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type point2structurexyz0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_x = ull.getDouble(self.idx, path, cpopath + 'x')
		check_status(status)
		if not status:
			self.x = ret_x
		status, ret_y = ull.getDouble(self.idx, path, cpopath + 'y')
		check_status(status)
		if not status:
			self.y = ret_y
		status, ret_z = ull.getDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type point2structurexyz0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, xVal = ull.getDouble(self.idx, path, cpopath + 'x')
			check_status(status)
			status, yVal = ull.getDouble(self.idx, path, cpopath + 'y')
			check_status(status)
			status, zVal = ull.getDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = point2structurexyz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.x = xVal
				slice.y = yVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point2structurexyz0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point2structurexyz0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point2structurexyz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'x', i, self.x)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'y', i, self.y)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point2structurexyz0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		status, ret_x = ull.getDoubleFromObject(self.idx, obj, cpopath + 'x', i)
		check_status(status)
		if not status:
			self.x = ret_x
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		status, ret_y = ull.getDoubleFromObject(self.idx, obj, cpopath + 'y', i)
		check_status(status)
		if not status:
			self.y = ret_y
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getDoubleFromObject(self.idx, obj, cpopath + 'z', i)
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
		ull.deleteData(self.idx, path, cpopath + 'x')
		ull.deleteData(self.idx, path, cpopath + 'y')
		ull.deleteData(self.idx, path, cpopath + 'z')


class point3structurexyz0D:
	'''
	class point3structurexyz0D
	Point 3 on the triangle

	Attributes:
	- x : float
	   Spatial coordinate x [m]
	- y : float
	   Spatial coordinate y [m]
	- z : float
	   Spatial coordinate z [m]
	'''

	def __init__(self, base_path_in='point3'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.x = EMPTY_DOUBLE
		self.y = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class point3structurexyz0D\n'
		ret = ret + space + 'Attribute x: ' + str(self.x) + '\n'
		ret = ret + space + 'Attribute y: ' + str(self.y) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point3structurexyz0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point3structurexyz0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point3structurexyz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'x', self.x)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'y', self.y)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'z', self.z)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type point3structurexyz0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_x = ull.getDouble(self.idx, path, cpopath + 'x')
		check_status(status)
		if not status:
			self.x = ret_x
		status, ret_y = ull.getDouble(self.idx, path, cpopath + 'y')
		check_status(status)
		if not status:
			self.y = ret_y
		status, ret_z = ull.getDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type point3structurexyz0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, xVal = ull.getDouble(self.idx, path, cpopath + 'x')
			check_status(status)
			status, yVal = ull.getDouble(self.idx, path, cpopath + 'y')
			check_status(status)
			status, zVal = ull.getDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = point3structurexyz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.x = xVal
				slice.y = yVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point3structurexyz0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point3structurexyz0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point3structurexyz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'x', i, self.x)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'y', i, self.y)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point3structurexyz0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		status, ret_x = ull.getDoubleFromObject(self.idx, obj, cpopath + 'x', i)
		check_status(status)
		if not status:
			self.x = ret_x
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		status, ret_y = ull.getDoubleFromObject(self.idx, obj, cpopath + 'y', i)
		check_status(status)
		if not status:
			self.y = ret_y
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getDoubleFromObject(self.idx, obj, cpopath + 'z', i)
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
		ull.deleteData(self.idx, path, cpopath + 'x')
		ull.deleteData(self.idx, path, cpopath + 'y')
		ull.deleteData(self.idx, path, cpopath + 'z')


class rectanglestruct_arrayrectanglexyz:
	'''
	class rectanglestruct_arrayrectanglexyz
	Rectangular wall surface described by its four corners. These form an ordered sequence: point00, point01, point11, point10. Here the first point should be calculated from the other three as point00=point01+poin10-point11. Vector(n_rectangles)

	Attributes:
	- array : list of rectanglestruct_arrayrectanglexyzObj 
	'''

	def __init__(self, base_path_in='rectangle'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class rectanglestruct_arrayrectanglexyz\n'
		for i in range(len(self.array)):
			ret = ret + space + 'rectanglestruct_arrayrectanglexyz[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(rectanglestruct_arrayrectanglexyzObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rectanglestruct_arrayrectanglexyz, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rectanglestruct_arrayrectanglexyz, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rectanglestruct_arrayrectanglexyz, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type rectanglestruct_arrayrectanglexyz, run function getSlice') 
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type rectanglestruct_arrayrectanglexyz, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(rectanglestruct_arrayrectanglexyz(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = rectanglestruct_arrayrectanglexyz(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getNonTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type rectanglestruct_arrayrectanglexyz, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type rectanglestruct_arrayrectanglexyz, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class rectanglestruct_arrayrectanglexyzObj:
	'''
	class rectanglestruct_arrayrectanglexyzObj
	Rectangular wall surface described by its four corners. These form an ordered sequence: point00, point01, point11, point10. Here the first point should be calculated from the other three as point00=point01+poin10-point11. Vector(n_rectangles)

	Attributes:
	- point01 : class point01structurexyz0D
	   Point 01 on the rectangle
	- point11 : class point11structurexyz0D
	   Point 11 on the rectangle
	- point10 : class point10structurexyz0D
	   Point 10 on the rectangle
	'''

	def __init__(self, base_path_in='rectangle'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.point01 = point01structurexyz0D('point01')
		self.point11 = point11structurexyz0D('point11')
		self.point10 = point10structurexyz0D('point10')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class rectanglestruct_arrayrectanglexyzObj\n'
		ret = ret + space + 'Attribute point01\n ' + self.point01.__str__(depth+1)
		ret = ret + space + 'Attribute point11\n ' + self.point11.__str__(depth+1)
		ret = ret + space + 'Attribute point10\n ' + self.point10.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.point01.setExpIdx(idx)
		self.point11.setExpIdx(idx)
		self.point10.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rectanglestruct_arrayrectanglexyzObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.point01.putNonTimedElt(path, cpopath + 'point01', i, obj)
		obj = self.point11.putNonTimedElt(path, cpopath + 'point11', i, obj)
		obj = self.point10.putNonTimedElt(path, cpopath + 'point10', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rectanglestruct_arrayrectanglexyzObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.point01.getNonTimedElt(path, cpopath + 'point01', i, obj)
		self.point11.getNonTimedElt(path, cpopath + 'point11', i, obj)
		self.point10.getNonTimedElt(path, cpopath + 'point10', i, obj)


class point01structurexyz0D:
	'''
	class point01structurexyz0D
	Point 01 on the rectangle

	Attributes:
	- x : float
	   Spatial coordinate x [m]
	- y : float
	   Spatial coordinate y [m]
	- z : float
	   Spatial coordinate z [m]
	'''

	def __init__(self, base_path_in='point01'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.x = EMPTY_DOUBLE
		self.y = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class point01structurexyz0D\n'
		ret = ret + space + 'Attribute x: ' + str(self.x) + '\n'
		ret = ret + space + 'Attribute y: ' + str(self.y) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point01structurexyz0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point01structurexyz0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point01structurexyz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'x', self.x)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'y', self.y)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'z', self.z)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type point01structurexyz0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_x = ull.getDouble(self.idx, path, cpopath + 'x')
		check_status(status)
		if not status:
			self.x = ret_x
		status, ret_y = ull.getDouble(self.idx, path, cpopath + 'y')
		check_status(status)
		if not status:
			self.y = ret_y
		status, ret_z = ull.getDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type point01structurexyz0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, xVal = ull.getDouble(self.idx, path, cpopath + 'x')
			check_status(status)
			status, yVal = ull.getDouble(self.idx, path, cpopath + 'y')
			check_status(status)
			status, zVal = ull.getDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = point01structurexyz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.x = xVal
				slice.y = yVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point01structurexyz0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point01structurexyz0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point01structurexyz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'x', i, self.x)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'y', i, self.y)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point01structurexyz0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		status, ret_x = ull.getDoubleFromObject(self.idx, obj, cpopath + 'x', i)
		check_status(status)
		if not status:
			self.x = ret_x
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		status, ret_y = ull.getDoubleFromObject(self.idx, obj, cpopath + 'y', i)
		check_status(status)
		if not status:
			self.y = ret_y
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getDoubleFromObject(self.idx, obj, cpopath + 'z', i)
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
		ull.deleteData(self.idx, path, cpopath + 'x')
		ull.deleteData(self.idx, path, cpopath + 'y')
		ull.deleteData(self.idx, path, cpopath + 'z')


class point11structurexyz0D:
	'''
	class point11structurexyz0D
	Point 11 on the rectangle

	Attributes:
	- x : float
	   Spatial coordinate x [m]
	- y : float
	   Spatial coordinate y [m]
	- z : float
	   Spatial coordinate z [m]
	'''

	def __init__(self, base_path_in='point11'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.x = EMPTY_DOUBLE
		self.y = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class point11structurexyz0D\n'
		ret = ret + space + 'Attribute x: ' + str(self.x) + '\n'
		ret = ret + space + 'Attribute y: ' + str(self.y) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point11structurexyz0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point11structurexyz0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point11structurexyz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'x', self.x)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'y', self.y)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'z', self.z)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type point11structurexyz0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_x = ull.getDouble(self.idx, path, cpopath + 'x')
		check_status(status)
		if not status:
			self.x = ret_x
		status, ret_y = ull.getDouble(self.idx, path, cpopath + 'y')
		check_status(status)
		if not status:
			self.y = ret_y
		status, ret_z = ull.getDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type point11structurexyz0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, xVal = ull.getDouble(self.idx, path, cpopath + 'x')
			check_status(status)
			status, yVal = ull.getDouble(self.idx, path, cpopath + 'y')
			check_status(status)
			status, zVal = ull.getDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = point11structurexyz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.x = xVal
				slice.y = yVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point11structurexyz0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point11structurexyz0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point11structurexyz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'x', i, self.x)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'y', i, self.y)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point11structurexyz0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		status, ret_x = ull.getDoubleFromObject(self.idx, obj, cpopath + 'x', i)
		check_status(status)
		if not status:
			self.x = ret_x
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		status, ret_y = ull.getDoubleFromObject(self.idx, obj, cpopath + 'y', i)
		check_status(status)
		if not status:
			self.y = ret_y
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getDoubleFromObject(self.idx, obj, cpopath + 'z', i)
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
		ull.deleteData(self.idx, path, cpopath + 'x')
		ull.deleteData(self.idx, path, cpopath + 'y')
		ull.deleteData(self.idx, path, cpopath + 'z')


class point10structurexyz0D:
	'''
	class point10structurexyz0D
	Point 10 on the rectangle

	Attributes:
	- x : float
	   Spatial coordinate x [m]
	- y : float
	   Spatial coordinate y [m]
	- z : float
	   Spatial coordinate z [m]
	'''

	def __init__(self, base_path_in='point10'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.x = EMPTY_DOUBLE
		self.y = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class point10structurexyz0D\n'
		ret = ret + space + 'Attribute x: ' + str(self.x) + '\n'
		ret = ret + space + 'Attribute y: ' + str(self.y) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point10structurexyz0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point10structurexyz0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type point10structurexyz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'x', self.x)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'y', self.y)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'z', self.z)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type point10structurexyz0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_x = ull.getDouble(self.idx, path, cpopath + 'x')
		check_status(status)
		if not status:
			self.x = ret_x
		status, ret_y = ull.getDouble(self.idx, path, cpopath + 'y')
		check_status(status)
		if not status:
			self.y = ret_y
		status, ret_z = ull.getDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type point10structurexyz0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, xVal = ull.getDouble(self.idx, path, cpopath + 'x')
			check_status(status)
			status, yVal = ull.getDouble(self.idx, path, cpopath + 'y')
			check_status(status)
			status, zVal = ull.getDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = point10structurexyz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.x = xVal
				slice.y = yVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point10structurexyz0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point10structurexyz0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point10structurexyz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'x', i, self.x)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'y', i, self.y)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type point10structurexyz0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		status, ret_x = ull.getDoubleFromObject(self.idx, obj, cpopath + 'x', i)
		check_status(status)
		if not status:
			self.x = ret_x
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		status, ret_y = ull.getDoubleFromObject(self.idx, obj, cpopath + 'y', i)
		check_status(status)
		if not status:
			self.y = ret_y
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getDoubleFromObject(self.idx, obj, cpopath + 'z', i)
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
		ull.deleteData(self.idx, path, cpopath + 'x')
		ull.deleteData(self.idx, path, cpopath + 'y')
		ull.deleteData(self.idx, path, cpopath + 'z')


class collimatorstruct_arrayflat_polygon:
	'''
	class collimatorstruct_arrayflat_polygon
	Vector of collimating holes (openings). Each hole has to be flat, i.e. it lies on a surface. Particles can only cross this surface by passing through the hole. To describe the hole we first construct a coordinate system on the surface by defining the original and two basis vectors in (x,y,z) space. The polyon is then represented as the origin, plus a linear combination of the two basis vectors using coord1 and coord2. As an example, a rectangle with two of the corners given by "origin+basis1" and "origin+basis2" can be described using coord1=[1,0,-1,0] and coord2=[0,1,0,-1].

	Attributes:
	- array : list of collimatorstruct_arrayflat_polygonObj 
	'''

	def __init__(self, base_path_in='collimator'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.array = []
	def __getitem__(self, key):
		return self.array[key]
	def __len__(self):
		return len(self.array)
	def __iter__(self):
		return self.array.__iter__()

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class collimatorstruct_arrayflat_polygon\n'
		for i in range(len(self.array)):
			ret = ret + space + 'collimatorstruct_arrayflat_polygon[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(collimatorstruct_arrayflat_polygonObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collimatorstruct_arrayflat_polygon, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collimatorstruct_arrayflat_polygon, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collimatorstruct_arrayflat_polygon, run function putNonTimed') 
		if (dev()):
			print ('beginObject(None, 0, '+path+'/'+cpopath+self.base_path+', NON_TIMED)')
		obj = ull.beginObject(self.idx, None, 0, path + '/' + cpopath + self.base_path, NON_TIMED)
		for i in range(len(self.array)):
			obj = self.array[i].putNonTimedElt(path, self.base_path, i, obj)
		if (dev()):
			print ('putObject('+path+', '+cpopath+self.base_path+', '+str(obj)+', NON_TIMED)')
		status = ull.putObject(self.idx, path, cpopath + self.base_path, obj, NON_TIMED)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type collimatorstruct_arrayflat_polygon, run function getSlice') 
		if (dev()):
			print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
		status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
		if status:
			print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
			return
		obj_size = ull.getObjectDim(self.idx, obj)
		self.resize(obj_size)
		for i in range(obj_size):
			self.array[i].getNonTimedElt(path, self.base_path, i, obj)
		ull.releaseObject(self.idx, obj)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type collimatorstruct_arrayflat_polygon, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(collimatorstruct_arrayflat_polygon(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = collimatorstruct_arrayflat_polygon(self.base_path)
				slice.setExpIdx(self.idx)
				slice.resize(obj_size)
				for i in range(obj_size):
					slice.array[i].getNonTimedElt(path, self.base_path, i, obj)
				list.append(slice)
			ull.releaseObject(self.idx, obj)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type collimatorstruct_arrayflat_polygon, run function putNonTimedElt') 
		if (dev()):
			print ('beginObject idx=%d' %(self.idx)) 
		if (dev()):
			print ('beginObject('+str(obj)+', '+str(i)+', '+path+'/'+cpopath+', NON_TIMED)')
		obj2 = ull.beginObject(self.idx, obj, i, cpopath, NON_TIMED)
		if (dev()):
			print ('obj = ' + str(obj)) 
		for j in range(len(self.array)):
			if (dev()):
				print ('struct_array loop elt %d' %(j) )
			obj2 = self.array[j].putNonTimedElt(path, self.base_path, j, obj2)
		if (dev()):
			print ('putObjectInObject('+str(self.idx)+','+str(obj)+','+cpopath+','+str(i)+','+str(obj2)+')')
		obj = ull.putObjectInObject(self.idx, obj, cpopath, i, obj2)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('field '+self.base_path+' of type collimatorstruct_arrayflat_polygon, run function getNonTimedElt') 
		if (dev()):
			print ('getObjectFromObject('+str(obj)+', '+cpopath+', '+str(i)+')')
		status, obj2 = ull.getObjectFromObject(self.idx, obj, cpopath, i)
		if status:
			print ('No data found for non-timed array of structures: ' + path + '/' + cpopath)
			return
		obj_size = ull.getObjectDim(self.idx, obj2)
		if (len(self.array)>0):
			if (obj_size != 0) and (len(self.array) != obj_size) and (len(self.array) != 0):
				print ('error in getNonTimedElt: wrong size of object at '+path+'/'+cpopath+ ', (NON-TIMED part size =  ' + str(obj_size) + ') != (TIMED part size = ' + str(len(self.array)) +')')
		else:
			self.resize(obj_size)
		for j in range(obj_size):
			self.array[j].getNonTimedElt(path, self.base_path, j, obj2)


class collimatorstruct_arrayflat_polygonObj:
	'''
	class collimatorstruct_arrayflat_polygonObj
	Vector of collimating holes (openings). Each hole has to be flat, i.e. it lies on a surface. Particles can only cross this surface by passing through the hole. To describe the hole we first construct a coordinate system on the surface by defining the original and two basis vectors in (x,y,z) space. The polyon is then represented as the origin, plus a linear combination of the two basis vectors using coord1 and coord2. As an example, a rectangle with two of the corners given by "origin+basis1" and "origin+basis2" can be described using coord1=[1,0,-1,0] and coord2=[0,1,0,-1].

	Attributes:
	- origin : class originstructurexyz0D
	   Origin of the surface coordinate system.
	- basis1 : class basis1structurexyz0D
	   First basis vector on the surface.
	- basis2 : class basis2structurexyz0D
	   First basis vector on the surface.
	- coord1 : numpy.ndarray 1D with float
	   First coordinate of the polygon points, conjugate to basis1.
	- coord2 : numpy.ndarray 1D with float
	   Second coordinate of the polygon points, conjugate to basis2.
	'''

	def __init__(self, base_path_in='collimator'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.origin = originstructurexyz0D('origin')
		self.basis1 = basis1structurexyz0D('basis1')
		self.basis2 = basis2structurexyz0D('basis2')
		self.coord1 = numpy.zeros(0, numpy.float64, order='C')
		self.coord2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class collimatorstruct_arrayflat_polygonObj\n'
		ret = ret + space + 'Attribute origin\n ' + self.origin.__str__(depth+1)
		ret = ret + space + 'Attribute basis1\n ' + self.basis1.__str__(depth+1)
		ret = ret + space + 'Attribute basis2\n ' + self.basis2.__str__(depth+1)
		s = self.coord1.__str__()
		ret = ret + space + 'Attribute coord1\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.coord2.__str__()
		ret = ret + space + 'Attribute coord2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.origin.setExpIdx(idx)
		self.basis1.setExpIdx(idx)
		self.basis2.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collimatorstruct_arrayflat_polygonObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.origin.putNonTimedElt(path, cpopath + 'origin', i, obj)
		obj = self.basis1.putNonTimedElt(path, cpopath + 'basis1', i, obj)
		obj = self.basis2.putNonTimedElt(path, cpopath + 'basis2', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'coord1') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'coord1', i, numpy.array(self.coord1).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'coord2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'coord2', i, numpy.array(self.coord2).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collimatorstruct_arrayflat_polygonObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.origin.getNonTimedElt(path, cpopath + 'origin', i, obj)
		self.basis1.getNonTimedElt(path, cpopath + 'basis1', i, obj)
		self.basis2.getNonTimedElt(path, cpopath + 'basis2', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'coord1') 
			print ('obj = ' + str(obj))
		status, ret_coord1 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'coord1', i)
		check_status(status)
		if not status:
			self.coord1 = ret_coord1
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'coord2') 
			print ('obj = ' + str(obj))
		status, ret_coord2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'coord2', i)
		check_status(status)
		if not status:
			self.coord2 = ret_coord2


class originstructurexyz0D:
	'''
	class originstructurexyz0D
	Origin of the surface coordinate system.

	Attributes:
	- x : float
	   Spatial coordinate x [m]
	- y : float
	   Spatial coordinate y [m]
	- z : float
	   Spatial coordinate z [m]
	'''

	def __init__(self, base_path_in='origin'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.x = EMPTY_DOUBLE
		self.y = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class originstructurexyz0D\n'
		ret = ret + space + 'Attribute x: ' + str(self.x) + '\n'
		ret = ret + space + 'Attribute y: ' + str(self.y) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type originstructurexyz0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type originstructurexyz0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type originstructurexyz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'x', self.x)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'y', self.y)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'z', self.z)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type originstructurexyz0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_x = ull.getDouble(self.idx, path, cpopath + 'x')
		check_status(status)
		if not status:
			self.x = ret_x
		status, ret_y = ull.getDouble(self.idx, path, cpopath + 'y')
		check_status(status)
		if not status:
			self.y = ret_y
		status, ret_z = ull.getDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type originstructurexyz0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, xVal = ull.getDouble(self.idx, path, cpopath + 'x')
			check_status(status)
			status, yVal = ull.getDouble(self.idx, path, cpopath + 'y')
			check_status(status)
			status, zVal = ull.getDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = originstructurexyz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.x = xVal
				slice.y = yVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type originstructurexyz0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type originstructurexyz0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type originstructurexyz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'x', i, self.x)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'y', i, self.y)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type originstructurexyz0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		status, ret_x = ull.getDoubleFromObject(self.idx, obj, cpopath + 'x', i)
		check_status(status)
		if not status:
			self.x = ret_x
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		status, ret_y = ull.getDoubleFromObject(self.idx, obj, cpopath + 'y', i)
		check_status(status)
		if not status:
			self.y = ret_y
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getDoubleFromObject(self.idx, obj, cpopath + 'z', i)
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
		ull.deleteData(self.idx, path, cpopath + 'x')
		ull.deleteData(self.idx, path, cpopath + 'y')
		ull.deleteData(self.idx, path, cpopath + 'z')


class basis1structurexyz0D:
	'''
	class basis1structurexyz0D
	First basis vector on the surface.

	Attributes:
	- x : float
	   Spatial coordinate x [m]
	- y : float
	   Spatial coordinate y [m]
	- z : float
	   Spatial coordinate z [m]
	'''

	def __init__(self, base_path_in='basis1'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.x = EMPTY_DOUBLE
		self.y = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class basis1structurexyz0D\n'
		ret = ret + space + 'Attribute x: ' + str(self.x) + '\n'
		ret = ret + space + 'Attribute y: ' + str(self.y) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type basis1structurexyz0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type basis1structurexyz0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type basis1structurexyz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'x', self.x)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'y', self.y)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'z', self.z)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type basis1structurexyz0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_x = ull.getDouble(self.idx, path, cpopath + 'x')
		check_status(status)
		if not status:
			self.x = ret_x
		status, ret_y = ull.getDouble(self.idx, path, cpopath + 'y')
		check_status(status)
		if not status:
			self.y = ret_y
		status, ret_z = ull.getDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type basis1structurexyz0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, xVal = ull.getDouble(self.idx, path, cpopath + 'x')
			check_status(status)
			status, yVal = ull.getDouble(self.idx, path, cpopath + 'y')
			check_status(status)
			status, zVal = ull.getDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = basis1structurexyz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.x = xVal
				slice.y = yVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basis1structurexyz0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basis1structurexyz0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basis1structurexyz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'x', i, self.x)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'y', i, self.y)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basis1structurexyz0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		status, ret_x = ull.getDoubleFromObject(self.idx, obj, cpopath + 'x', i)
		check_status(status)
		if not status:
			self.x = ret_x
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		status, ret_y = ull.getDoubleFromObject(self.idx, obj, cpopath + 'y', i)
		check_status(status)
		if not status:
			self.y = ret_y
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getDoubleFromObject(self.idx, obj, cpopath + 'z', i)
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
		ull.deleteData(self.idx, path, cpopath + 'x')
		ull.deleteData(self.idx, path, cpopath + 'y')
		ull.deleteData(self.idx, path, cpopath + 'z')


class basis2structurexyz0D:
	'''
	class basis2structurexyz0D
	First basis vector on the surface.

	Attributes:
	- x : float
	   Spatial coordinate x [m]
	- y : float
	   Spatial coordinate y [m]
	- z : float
	   Spatial coordinate z [m]
	'''

	def __init__(self, base_path_in='basis2'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.x = EMPTY_DOUBLE
		self.y = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class basis2structurexyz0D\n'
		ret = ret + space + 'Attribute x: ' + str(self.x) + '\n'
		ret = ret + space + 'Attribute y: ' + str(self.y) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type basis2structurexyz0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type basis2structurexyz0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type basis2structurexyz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'x', self.x)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'y', self.y)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'z', self.z)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type basis2structurexyz0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_x = ull.getDouble(self.idx, path, cpopath + 'x')
		check_status(status)
		if not status:
			self.x = ret_x
		status, ret_y = ull.getDouble(self.idx, path, cpopath + 'y')
		check_status(status)
		if not status:
			self.y = ret_y
		status, ret_z = ull.getDouble(self.idx, path, cpopath + 'z')
		check_status(status)
		if not status:
			self.z = ret_z

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type basis2structurexyz0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, xVal = ull.getDouble(self.idx, path, cpopath + 'x')
			check_status(status)
			status, yVal = ull.getDouble(self.idx, path, cpopath + 'y')
			check_status(status)
			status, zVal = ull.getDouble(self.idx, path, cpopath + 'z')
			check_status(status)
			for i in range(nbslice):
				slice = basis2structurexyz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.x = xVal
				slice.y = yVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basis2structurexyz0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basis2structurexyz0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basis2structurexyz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'x', i, self.x)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'y', i, self.y)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basis2structurexyz0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'x') 
			print ('obj = ' + str(obj))
		status, ret_x = ull.getDoubleFromObject(self.idx, obj, cpopath + 'x', i)
		check_status(status)
		if not status:
			self.x = ret_x
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'y') 
			print ('obj = ' + str(obj))
		status, ret_y = ull.getDoubleFromObject(self.idx, obj, cpopath + 'y', i)
		check_status(status)
		if not status:
			self.y = ret_y
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		status, ret_z = ull.getDoubleFromObject(self.idx, obj, cpopath + 'z', i)
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
		ull.deleteData(self.idx, path, cpopath + 'x')
		ull.deleteData(self.idx, path, cpopath + 'y')
		ull.deleteData(self.idx, path, cpopath + 'z')


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
