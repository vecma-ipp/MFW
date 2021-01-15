# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class temporary(KeepInOrder):
	'''
	class temporary
	Storage of undeclared data model components; Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- non_timed : class non_timedstructuretemporary_nt
	   Time-independent quantities (parameters)
	- timed : class timedstructuretemporary_t
	   Time-dependent quantities
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar
	'''

	def __init__(self):
		self.base_path = 'temporary'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 50
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.non_timed = non_timedstructuretemporary_nt('non_timed')
		self.timed = timedstructuretemporary_t('timed')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class temporary\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute non_timed\n ' + self.non_timed.__str__(depth+1)
		ret = ret + space + 'Attribute timed\n ' + self.timed.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.non_timed.setExpIdx(idx)
		self.timed.setExpIdx(idx)
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
		self.non_timed.cpoTime = self.cpoTime
		self.non_timed.putSlice(path, cpopath)
		self.timed.cpoTime = self.cpoTime
		self.timed.putSlice(path, cpopath)
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
		self.non_timed.replaceLastSlice(path, cpopath)
		self.timed.replaceLastSlice(path, cpopath)
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
		self.non_timed.putNonTimed(path, cpopath)
		self.timed.putNonTimed(path, cpopath)
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
		self.non_timed.getSlice(path, cpopath, inTime, interpolMode)
		self.timed.getSlice(path, cpopath, inTime, interpolMode)
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
			non_timedList = self.non_timed.build_non_resampled_data(path, cpopath, nbslice)
			timedList = self.timed.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = temporary()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.non_timed = non_timedList[i]
				slice.timed = timedList[i]
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
		self.non_timed.deleteData(path, cpopath)
		self.timed.deleteData(path, cpopath)
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class temporaryArray:
	'''
	class temporaryArray
	Storage of undeclared data model components; Time-dependent CPO

	Attributes:
	- array : list of temporary
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
		ret = space + 'class temporaryArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'temporary cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = temporary()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(temporary())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = temporary()
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


class non_timedstructuretemporary_nt(KeepInOrder):
	'''
	class non_timedstructuretemporary_nt
	Time-independent quantities (parameters)

	Attributes:
	- float0d : class float0dstruct_arraytemporary_nt_0dr: array of float0dstruct_arraytemporary_nt_0drObj objects
	   Constant 0D float
	- integer0d : class integer0dstruct_arraytemporary_nt_0di: array of integer0dstruct_arraytemporary_nt_0diObj objects
	   Constant 0D integer
	- complex0d : class complex0dstruct_arraytemporary_nt_0dc: array of complex0dstruct_arraytemporary_nt_0dcObj objects
	   Constant 0D complex
	- string0d : class string0dstruct_arraytemporary_nt_0ds: array of string0dstruct_arraytemporary_nt_0dsObj objects
	   Constant 0D string
	- float1d : class float1dstruct_arraytemporary_nt_1dr: array of float1dstruct_arraytemporary_nt_1drObj objects
	   Constant 1D float
	- integer1d : class integer1dstruct_arraytemporary_nt_1di: array of integer1dstruct_arraytemporary_nt_1diObj objects
	   Constant 1D integer
	- string1d : class string1dstruct_arraytemporary_nt_1dr: array of string1dstruct_arraytemporary_nt_1drObj objects
	   Constant 1D string
	- complex1d : class complex1dstruct_arraytemporary_nt_1dc: array of complex1dstruct_arraytemporary_nt_1dcObj objects
	   Constant 1D complex
	- float2d : class float2dstruct_arraytemporary_nt_2dr: array of float2dstruct_arraytemporary_nt_2drObj objects
	   Constant 2D float
	- integer2d : class integer2dstruct_arraytemporary_nt_2di: array of integer2dstruct_arraytemporary_nt_2diObj objects
	   Constant 2D integer
	- complex2d : class complex2dstruct_arraytemporary_nt_2dc: array of complex2dstruct_arraytemporary_nt_2dcObj objects
	   Constant 2D complex
	- float3d : class float3dstruct_arraytemporary_nt_3dr: array of float3dstruct_arraytemporary_nt_3drObj objects
	   Constant 3D float
	- integer3d : class integer3dstruct_arraytemporary_nt_3di: array of integer3dstruct_arraytemporary_nt_3diObj objects
	   Constant 3D integer
	- complex3d : class complex3dstruct_arraytemporary_nt_3dc: array of complex3dstruct_arraytemporary_nt_3dcObj objects
	   Constant 3D complex
	- float4d : class float4dstruct_arraytemporary_nt_4dr: array of float4dstruct_arraytemporary_nt_4drObj objects
	   Constant 4D float
	'''

	def __init__(self, base_path_in='non_timed'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.float0d = float0dstruct_arraytemporary_nt_0dr('float0d')
		self.integer0d = integer0dstruct_arraytemporary_nt_0di('integer0d')
		self.complex0d = complex0dstruct_arraytemporary_nt_0dc('complex0d')
		self.string0d = string0dstruct_arraytemporary_nt_0ds('string0d')
		self.float1d = float1dstruct_arraytemporary_nt_1dr('float1d')
		self.integer1d = integer1dstruct_arraytemporary_nt_1di('integer1d')
		self.string1d = string1dstruct_arraytemporary_nt_1dr('string1d')
		self.complex1d = complex1dstruct_arraytemporary_nt_1dc('complex1d')
		self.float2d = float2dstruct_arraytemporary_nt_2dr('float2d')
		self.integer2d = integer2dstruct_arraytemporary_nt_2di('integer2d')
		self.complex2d = complex2dstruct_arraytemporary_nt_2dc('complex2d')
		self.float3d = float3dstruct_arraytemporary_nt_3dr('float3d')
		self.integer3d = integer3dstruct_arraytemporary_nt_3di('integer3d')
		self.complex3d = complex3dstruct_arraytemporary_nt_3dc('complex3d')
		self.float4d = float4dstruct_arraytemporary_nt_4dr('float4d')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class non_timedstructuretemporary_nt\n'
		ret = ret + space + 'Attribute float0d\n ' + self.float0d.__str__(depth+1)
		ret = ret + space + 'Attribute integer0d\n ' + self.integer0d.__str__(depth+1)
		ret = ret + space + 'Attribute complex0d\n ' + self.complex0d.__str__(depth+1)
		ret = ret + space + 'Attribute string0d\n ' + self.string0d.__str__(depth+1)
		ret = ret + space + 'Attribute float1d\n ' + self.float1d.__str__(depth+1)
		ret = ret + space + 'Attribute integer1d\n ' + self.integer1d.__str__(depth+1)
		ret = ret + space + 'Attribute string1d\n ' + self.string1d.__str__(depth+1)
		ret = ret + space + 'Attribute complex1d\n ' + self.complex1d.__str__(depth+1)
		ret = ret + space + 'Attribute float2d\n ' + self.float2d.__str__(depth+1)
		ret = ret + space + 'Attribute integer2d\n ' + self.integer2d.__str__(depth+1)
		ret = ret + space + 'Attribute complex2d\n ' + self.complex2d.__str__(depth+1)
		ret = ret + space + 'Attribute float3d\n ' + self.float3d.__str__(depth+1)
		ret = ret + space + 'Attribute integer3d\n ' + self.integer3d.__str__(depth+1)
		ret = ret + space + 'Attribute complex3d\n ' + self.complex3d.__str__(depth+1)
		ret = ret + space + 'Attribute float4d\n ' + self.float4d.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.float0d.setExpIdx(idx)
		self.integer0d.setExpIdx(idx)
		self.complex0d.setExpIdx(idx)
		self.string0d.setExpIdx(idx)
		self.float1d.setExpIdx(idx)
		self.integer1d.setExpIdx(idx)
		self.string1d.setExpIdx(idx)
		self.complex1d.setExpIdx(idx)
		self.float2d.setExpIdx(idx)
		self.integer2d.setExpIdx(idx)
		self.complex2d.setExpIdx(idx)
		self.float3d.setExpIdx(idx)
		self.integer3d.setExpIdx(idx)
		self.complex3d.setExpIdx(idx)
		self.float4d.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type non_timedstructuretemporary_nt, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.float0d.cpoTime = self.cpoTime
		self.float0d.putSlice(path, cpopath)
		self.integer0d.cpoTime = self.cpoTime
		self.integer0d.putSlice(path, cpopath)
		self.complex0d.cpoTime = self.cpoTime
		self.complex0d.putSlice(path, cpopath)
		self.string0d.cpoTime = self.cpoTime
		self.string0d.putSlice(path, cpopath)
		self.float1d.cpoTime = self.cpoTime
		self.float1d.putSlice(path, cpopath)
		self.integer1d.cpoTime = self.cpoTime
		self.integer1d.putSlice(path, cpopath)
		self.string1d.cpoTime = self.cpoTime
		self.string1d.putSlice(path, cpopath)
		self.complex1d.cpoTime = self.cpoTime
		self.complex1d.putSlice(path, cpopath)
		self.float2d.cpoTime = self.cpoTime
		self.float2d.putSlice(path, cpopath)
		self.integer2d.cpoTime = self.cpoTime
		self.integer2d.putSlice(path, cpopath)
		self.complex2d.cpoTime = self.cpoTime
		self.complex2d.putSlice(path, cpopath)
		self.float3d.cpoTime = self.cpoTime
		self.float3d.putSlice(path, cpopath)
		self.integer3d.cpoTime = self.cpoTime
		self.integer3d.putSlice(path, cpopath)
		self.complex3d.cpoTime = self.cpoTime
		self.complex3d.putSlice(path, cpopath)
		self.float4d.cpoTime = self.cpoTime
		self.float4d.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type non_timedstructuretemporary_nt, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.float0d.replaceLastSlice(path, cpopath)
		self.integer0d.replaceLastSlice(path, cpopath)
		self.complex0d.replaceLastSlice(path, cpopath)
		self.string0d.replaceLastSlice(path, cpopath)
		self.float1d.replaceLastSlice(path, cpopath)
		self.integer1d.replaceLastSlice(path, cpopath)
		self.string1d.replaceLastSlice(path, cpopath)
		self.complex1d.replaceLastSlice(path, cpopath)
		self.float2d.replaceLastSlice(path, cpopath)
		self.integer2d.replaceLastSlice(path, cpopath)
		self.complex2d.replaceLastSlice(path, cpopath)
		self.float3d.replaceLastSlice(path, cpopath)
		self.integer3d.replaceLastSlice(path, cpopath)
		self.complex3d.replaceLastSlice(path, cpopath)
		self.float4d.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type non_timedstructuretemporary_nt, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.float0d.putNonTimed(path, cpopath)
		self.integer0d.putNonTimed(path, cpopath)
		self.complex0d.putNonTimed(path, cpopath)
		self.string0d.putNonTimed(path, cpopath)
		self.float1d.putNonTimed(path, cpopath)
		self.integer1d.putNonTimed(path, cpopath)
		self.string1d.putNonTimed(path, cpopath)
		self.complex1d.putNonTimed(path, cpopath)
		self.float2d.putNonTimed(path, cpopath)
		self.integer2d.putNonTimed(path, cpopath)
		self.complex2d.putNonTimed(path, cpopath)
		self.float3d.putNonTimed(path, cpopath)
		self.integer3d.putNonTimed(path, cpopath)
		self.complex3d.putNonTimed(path, cpopath)
		self.float4d.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type non_timedstructuretemporary_nt, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.float0d.getSlice(path, cpopath, inTime, interpolMode)
		self.integer0d.getSlice(path, cpopath, inTime, interpolMode)
		self.complex0d.getSlice(path, cpopath, inTime, interpolMode)
		self.string0d.getSlice(path, cpopath, inTime, interpolMode)
		self.float1d.getSlice(path, cpopath, inTime, interpolMode)
		self.integer1d.getSlice(path, cpopath, inTime, interpolMode)
		self.string1d.getSlice(path, cpopath, inTime, interpolMode)
		self.complex1d.getSlice(path, cpopath, inTime, interpolMode)
		self.float2d.getSlice(path, cpopath, inTime, interpolMode)
		self.integer2d.getSlice(path, cpopath, inTime, interpolMode)
		self.complex2d.getSlice(path, cpopath, inTime, interpolMode)
		self.float3d.getSlice(path, cpopath, inTime, interpolMode)
		self.integer3d.getSlice(path, cpopath, inTime, interpolMode)
		self.complex3d.getSlice(path, cpopath, inTime, interpolMode)
		self.float4d.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type non_timedstructuretemporary_nt, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			float0dList = self.float0d.build_non_resampled_data(path, cpopath, nbslice)
			integer0dList = self.integer0d.build_non_resampled_data(path, cpopath, nbslice)
			complex0dList = self.complex0d.build_non_resampled_data(path, cpopath, nbslice)
			string0dList = self.string0d.build_non_resampled_data(path, cpopath, nbslice)
			float1dList = self.float1d.build_non_resampled_data(path, cpopath, nbslice)
			integer1dList = self.integer1d.build_non_resampled_data(path, cpopath, nbslice)
			string1dList = self.string1d.build_non_resampled_data(path, cpopath, nbslice)
			complex1dList = self.complex1d.build_non_resampled_data(path, cpopath, nbslice)
			float2dList = self.float2d.build_non_resampled_data(path, cpopath, nbslice)
			integer2dList = self.integer2d.build_non_resampled_data(path, cpopath, nbslice)
			complex2dList = self.complex2d.build_non_resampled_data(path, cpopath, nbslice)
			float3dList = self.float3d.build_non_resampled_data(path, cpopath, nbslice)
			integer3dList = self.integer3d.build_non_resampled_data(path, cpopath, nbslice)
			complex3dList = self.complex3d.build_non_resampled_data(path, cpopath, nbslice)
			float4dList = self.float4d.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = non_timedstructuretemporary_nt(self.base_path)
				slice.setExpIdx(self.idx)
				slice.float0d = float0dList[i]
				slice.integer0d = integer0dList[i]
				slice.complex0d = complex0dList[i]
				slice.string0d = string0dList[i]
				slice.float1d = float1dList[i]
				slice.integer1d = integer1dList[i]
				slice.string1d = string1dList[i]
				slice.complex1d = complex1dList[i]
				slice.float2d = float2dList[i]
				slice.integer2d = integer2dList[i]
				slice.complex2d = complex2dList[i]
				slice.float3d = float3dList[i]
				slice.integer3d = integer3dList[i]
				slice.complex3d = complex3dList[i]
				slice.float4d = float4dList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type non_timedstructuretemporary_ntObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type non_timedstructuretemporary_ntObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type non_timedstructuretemporary_ntObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.float0d.putNonTimedElt(path, cpopath + 'float0d', i, obj)
		obj = self.integer0d.putNonTimedElt(path, cpopath + 'integer0d', i, obj)
		obj = self.complex0d.putNonTimedElt(path, cpopath + 'complex0d', i, obj)
		obj = self.string0d.putNonTimedElt(path, cpopath + 'string0d', i, obj)
		obj = self.float1d.putNonTimedElt(path, cpopath + 'float1d', i, obj)
		obj = self.integer1d.putNonTimedElt(path, cpopath + 'integer1d', i, obj)
		obj = self.string1d.putNonTimedElt(path, cpopath + 'string1d', i, obj)
		obj = self.complex1d.putNonTimedElt(path, cpopath + 'complex1d', i, obj)
		obj = self.float2d.putNonTimedElt(path, cpopath + 'float2d', i, obj)
		obj = self.integer2d.putNonTimedElt(path, cpopath + 'integer2d', i, obj)
		obj = self.complex2d.putNonTimedElt(path, cpopath + 'complex2d', i, obj)
		obj = self.float3d.putNonTimedElt(path, cpopath + 'float3d', i, obj)
		obj = self.integer3d.putNonTimedElt(path, cpopath + 'integer3d', i, obj)
		obj = self.complex3d.putNonTimedElt(path, cpopath + 'complex3d', i, obj)
		obj = self.float4d.putNonTimedElt(path, cpopath + 'float4d', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type non_timedstructuretemporary_ntObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.float0d.getNonTimedElt(path, cpopath + 'float0d', i, obj)
		self.integer0d.getNonTimedElt(path, cpopath + 'integer0d', i, obj)
		self.complex0d.getNonTimedElt(path, cpopath + 'complex0d', i, obj)
		self.string0d.getNonTimedElt(path, cpopath + 'string0d', i, obj)
		self.float1d.getNonTimedElt(path, cpopath + 'float1d', i, obj)
		self.integer1d.getNonTimedElt(path, cpopath + 'integer1d', i, obj)
		self.string1d.getNonTimedElt(path, cpopath + 'string1d', i, obj)
		self.complex1d.getNonTimedElt(path, cpopath + 'complex1d', i, obj)
		self.float2d.getNonTimedElt(path, cpopath + 'float2d', i, obj)
		self.integer2d.getNonTimedElt(path, cpopath + 'integer2d', i, obj)
		self.complex2d.getNonTimedElt(path, cpopath + 'complex2d', i, obj)
		self.float3d.getNonTimedElt(path, cpopath + 'float3d', i, obj)
		self.integer3d.getNonTimedElt(path, cpopath + 'integer3d', i, obj)
		self.complex3d.getNonTimedElt(path, cpopath + 'complex3d', i, obj)
		self.float4d.getNonTimedElt(path, cpopath + 'float4d', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'float0d')
		ull.deleteData(self.idx, path, cpopath + 'integer0d')
		ull.deleteData(self.idx, path, cpopath + 'complex0d')
		ull.deleteData(self.idx, path, cpopath + 'string0d')
		ull.deleteData(self.idx, path, cpopath + 'float1d')
		ull.deleteData(self.idx, path, cpopath + 'integer1d')
		ull.deleteData(self.idx, path, cpopath + 'string1d')
		ull.deleteData(self.idx, path, cpopath + 'complex1d')
		ull.deleteData(self.idx, path, cpopath + 'float2d')
		ull.deleteData(self.idx, path, cpopath + 'integer2d')
		ull.deleteData(self.idx, path, cpopath + 'complex2d')
		ull.deleteData(self.idx, path, cpopath + 'float3d')
		ull.deleteData(self.idx, path, cpopath + 'integer3d')
		ull.deleteData(self.idx, path, cpopath + 'complex3d')
		ull.deleteData(self.idx, path, cpopath + 'float4d')


class float0dstruct_arraytemporary_nt_0dr:
	'''
	class float0dstruct_arraytemporary_nt_0dr
	Constant 0D float

	Attributes:
	- array : list of float0dstruct_arraytemporary_nt_0drObj 
	'''

	def __init__(self, base_path_in='float0d'):
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
		ret = space + 'class float0dstruct_arraytemporary_nt_0dr\n'
		for i in range(len(self.array)):
			ret = ret + space + 'float0dstruct_arraytemporary_nt_0dr[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(float0dstruct_arraytemporary_nt_0drObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_nt_0dr, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_nt_0dr, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_nt_0dr, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_nt_0dr, run function getSlice') 
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
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_nt_0dr, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(float0dstruct_arraytemporary_nt_0dr(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = float0dstruct_arraytemporary_nt_0dr(self.base_path)
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
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_nt_0dr, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_nt_0dr, run function getNonTimedElt') 
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


class float0dstruct_arraytemporary_nt_0drObj(KeepInOrder):
	'''
	class float0dstruct_arraytemporary_nt_0drObj
	Constant 0D float

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : float
	   Value. Real scalar.
	'''

	def __init__(self, base_path_in='float0d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class float0dstruct_arraytemporary_nt_0drObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float0dstruct_arraytemporary_nt_0drObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'value', i, self.value)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float0dstruct_arraytemporary_nt_0drObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class identifierstructureidentifier(KeepInOrder):
	'''
	class identifierstructureidentifier
	Identifier.

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='identifier'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class identifierstructureidentifier\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type identifierstructureidentifier, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type identifierstructureidentifier, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type identifierstructureidentifier, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'id', self.id)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'flag', self.flag)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'description', self.description)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type identifierstructureidentifier, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_id = ull.getString(self.idx, path, cpopath + 'id')
		check_status(status)
		if not status:
			self.id = ret_id
		status, ret_flag = ull.getInt(self.idx, path, cpopath + 'flag')
		check_status(status)
		if not status:
			self.flag = ret_flag
		status, ret_description = ull.getString(self.idx, path, cpopath + 'description')
		check_status(status)
		if not status:
			self.description = ret_description

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type identifierstructureidentifier, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, idVal = ull.getString(self.idx, path, cpopath + 'id')
			check_status(status)
			status, flagVal = ull.getInt(self.idx, path, cpopath + 'flag')
			check_status(status)
			status, descriptionVal = ull.getString(self.idx, path, cpopath + 'description')
			check_status(status)
			for i in range(nbslice):
				slice = identifierstructureidentifier(self.base_path)
				slice.setExpIdx(self.idx)
				slice.id = idVal
				slice.flag = flagVal
				slice.description = descriptionVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type identifierstructureidentifierObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type identifierstructureidentifierObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type identifierstructureidentifierObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'flag', i, self.flag)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'description') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'description', i, self.description)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type identifierstructureidentifierObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'flag') 
			print ('obj = ' + str(obj))
		status, ret_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'flag', i)
		check_status(status)
		if not status:
			self.flag = ret_flag
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'description') 
			print ('obj = ' + str(obj))
		status, ret_description = ull.getStringFromObject(self.idx, obj, cpopath + 'description', i)
		check_status(status)
		if not status:
			self.description = ret_description

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'id')
		ull.deleteData(self.idx, path, cpopath + 'flag')
		ull.deleteData(self.idx, path, cpopath + 'description')


class integer0dstruct_arraytemporary_nt_0di:
	'''
	class integer0dstruct_arraytemporary_nt_0di
	Constant 0D integer

	Attributes:
	- array : list of integer0dstruct_arraytemporary_nt_0diObj 
	'''

	def __init__(self, base_path_in='integer0d'):
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
		ret = space + 'class integer0dstruct_arraytemporary_nt_0di\n'
		for i in range(len(self.array)):
			ret = ret + space + 'integer0dstruct_arraytemporary_nt_0di[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(integer0dstruct_arraytemporary_nt_0diObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_nt_0di, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_nt_0di, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_nt_0di, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_nt_0di, run function getSlice') 
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
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_nt_0di, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(integer0dstruct_arraytemporary_nt_0di(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = integer0dstruct_arraytemporary_nt_0di(self.base_path)
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
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_nt_0di, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_nt_0di, run function getNonTimedElt') 
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


class integer0dstruct_arraytemporary_nt_0diObj(KeepInOrder):
	'''
	class integer0dstruct_arraytemporary_nt_0diObj
	Constant 0D integer

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : int
	   Value. integer scalar
	'''

	def __init__(self, base_path_in='integer0d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class integer0dstruct_arraytemporary_nt_0diObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer0dstruct_arraytemporary_nt_0diObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'value', i, self.value)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer0dstruct_arraytemporary_nt_0diObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getIntFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class complex0dstruct_arraytemporary_nt_0dc:
	'''
	class complex0dstruct_arraytemporary_nt_0dc
	Constant 0D complex

	Attributes:
	- array : list of complex0dstruct_arraytemporary_nt_0dcObj 
	'''

	def __init__(self, base_path_in='complex0d'):
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
		ret = space + 'class complex0dstruct_arraytemporary_nt_0dc\n'
		for i in range(len(self.array)):
			ret = ret + space + 'complex0dstruct_arraytemporary_nt_0dc[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(complex0dstruct_arraytemporary_nt_0dcObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_nt_0dc, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_nt_0dc, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_nt_0dc, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_nt_0dc, run function getSlice') 
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
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_nt_0dc, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(complex0dstruct_arraytemporary_nt_0dc(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = complex0dstruct_arraytemporary_nt_0dc(self.base_path)
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
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_nt_0dc, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_nt_0dc, run function getNonTimedElt') 
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


class complex0dstruct_arraytemporary_nt_0dcObj(KeepInOrder):
	'''
	class complex0dstruct_arraytemporary_nt_0dcObj
	Constant 0D complex

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : complex
	   Value. Complex scalar.
	'''

	def __init__(self, base_path_in='complex0d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = EMPTY_COMPLEX

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class complex0dstruct_arraytemporary_nt_0dcObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex0dstruct_arraytemporary_nt_0dcObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putComplexInObject(self.idx, obj, cpopath + 'value', i, self.value)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex0dstruct_arraytemporary_nt_0dcObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getComplexFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class string0dstruct_arraytemporary_nt_0ds:
	'''
	class string0dstruct_arraytemporary_nt_0ds
	Constant 0D string

	Attributes:
	- array : list of string0dstruct_arraytemporary_nt_0dsObj 
	'''

	def __init__(self, base_path_in='string0d'):
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
		ret = space + 'class string0dstruct_arraytemporary_nt_0ds\n'
		for i in range(len(self.array)):
			ret = ret + space + 'string0dstruct_arraytemporary_nt_0ds[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(string0dstruct_arraytemporary_nt_0dsObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_nt_0ds, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_nt_0ds, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_nt_0ds, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_nt_0ds, run function getSlice') 
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
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_nt_0ds, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(string0dstruct_arraytemporary_nt_0ds(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = string0dstruct_arraytemporary_nt_0ds(self.base_path)
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
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_nt_0ds, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_nt_0ds, run function getNonTimedElt') 
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


class string0dstruct_arraytemporary_nt_0dsObj(KeepInOrder):
	'''
	class string0dstruct_arraytemporary_nt_0dsObj
	Constant 0D string

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : str
	   Value. String.
	'''

	def __init__(self, base_path_in='string0d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class string0dstruct_arraytemporary_nt_0dsObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type string0dstruct_arraytemporary_nt_0dsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'value', i, self.value)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type string0dstruct_arraytemporary_nt_0dsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getStringFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class float1dstruct_arraytemporary_nt_1dr:
	'''
	class float1dstruct_arraytemporary_nt_1dr
	Constant 1D float

	Attributes:
	- array : list of float1dstruct_arraytemporary_nt_1drObj 
	'''

	def __init__(self, base_path_in='float1d'):
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
		ret = space + 'class float1dstruct_arraytemporary_nt_1dr\n'
		for i in range(len(self.array)):
			ret = ret + space + 'float1dstruct_arraytemporary_nt_1dr[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(float1dstruct_arraytemporary_nt_1drObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_nt_1dr, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_nt_1dr, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_nt_1dr, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_nt_1dr, run function getSlice') 
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
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_nt_1dr, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(float1dstruct_arraytemporary_nt_1dr(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = float1dstruct_arraytemporary_nt_1dr(self.base_path)
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
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_nt_1dr, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_nt_1dr, run function getNonTimedElt') 
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


class float1dstruct_arraytemporary_nt_1drObj(KeepInOrder):
	'''
	class float1dstruct_arraytemporary_nt_1drObj
	Constant 1D float

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 1D with float
	   Value. Vector of float.
	'''

	def __init__(self, base_path_in='float1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class float1dstruct_arraytemporary_nt_1drObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float1dstruct_arraytemporary_nt_1drObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float1dstruct_arraytemporary_nt_1drObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class integer1dstruct_arraytemporary_nt_1di:
	'''
	class integer1dstruct_arraytemporary_nt_1di
	Constant 1D integer

	Attributes:
	- array : list of integer1dstruct_arraytemporary_nt_1diObj 
	'''

	def __init__(self, base_path_in='integer1d'):
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
		ret = space + 'class integer1dstruct_arraytemporary_nt_1di\n'
		for i in range(len(self.array)):
			ret = ret + space + 'integer1dstruct_arraytemporary_nt_1di[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(integer1dstruct_arraytemporary_nt_1diObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_nt_1di, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_nt_1di, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_nt_1di, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_nt_1di, run function getSlice') 
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
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_nt_1di, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(integer1dstruct_arraytemporary_nt_1di(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = integer1dstruct_arraytemporary_nt_1di(self.base_path)
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
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_nt_1di, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_nt_1di, run function getNonTimedElt') 
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


class integer1dstruct_arraytemporary_nt_1diObj(KeepInOrder):
	'''
	class integer1dstruct_arraytemporary_nt_1diObj
	Constant 1D integer

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 1D with int)
	   Value. Vector of integers
	'''

	def __init__(self, base_path_in='integer1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros(0, numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class integer1dstruct_arraytemporary_nt_1diObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer1dstruct_arraytemporary_nt_1diObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer1dstruct_arraytemporary_nt_1diObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class string1dstruct_arraytemporary_nt_1dr:
	'''
	class string1dstruct_arraytemporary_nt_1dr
	Constant 1D string

	Attributes:
	- array : list of string1dstruct_arraytemporary_nt_1drObj 
	'''

	def __init__(self, base_path_in='string1d'):
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
		ret = space + 'class string1dstruct_arraytemporary_nt_1dr\n'
		for i in range(len(self.array)):
			ret = ret + space + 'string1dstruct_arraytemporary_nt_1dr[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(string1dstruct_arraytemporary_nt_1drObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type string1dstruct_arraytemporary_nt_1dr, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type string1dstruct_arraytemporary_nt_1dr, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type string1dstruct_arraytemporary_nt_1dr, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type string1dstruct_arraytemporary_nt_1dr, run function getSlice') 
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
			print ('field '+self.base_path+' of type string1dstruct_arraytemporary_nt_1dr, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(string1dstruct_arraytemporary_nt_1dr(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = string1dstruct_arraytemporary_nt_1dr(self.base_path)
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
			print ('field '+self.base_path+' of type string1dstruct_arraytemporary_nt_1dr, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type string1dstruct_arraytemporary_nt_1dr, run function getNonTimedElt') 
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


class string1dstruct_arraytemporary_nt_1drObj(KeepInOrder):
	'''
	class string1dstruct_arraytemporary_nt_1drObj
	Constant 1D string

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 1D with float
	   Value. Vector of float.
	'''

	def __init__(self, base_path_in='string1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class string1dstruct_arraytemporary_nt_1drObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type string1dstruct_arraytemporary_nt_1drObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type string1dstruct_arraytemporary_nt_1drObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class complex1dstruct_arraytemporary_nt_1dc:
	'''
	class complex1dstruct_arraytemporary_nt_1dc
	Constant 1D complex

	Attributes:
	- array : list of complex1dstruct_arraytemporary_nt_1dcObj 
	'''

	def __init__(self, base_path_in='complex1d'):
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
		ret = space + 'class complex1dstruct_arraytemporary_nt_1dc\n'
		for i in range(len(self.array)):
			ret = ret + space + 'complex1dstruct_arraytemporary_nt_1dc[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(complex1dstruct_arraytemporary_nt_1dcObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_nt_1dc, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_nt_1dc, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_nt_1dc, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_nt_1dc, run function getSlice') 
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
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_nt_1dc, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(complex1dstruct_arraytemporary_nt_1dc(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = complex1dstruct_arraytemporary_nt_1dc(self.base_path)
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
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_nt_1dc, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_nt_1dc, run function getNonTimedElt') 
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


class complex1dstruct_arraytemporary_nt_1dcObj(KeepInOrder):
	'''
	class complex1dstruct_arraytemporary_nt_1dcObj
	Constant 1D complex

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 1D with  complex numbers
	   Value. Vector of complex numbers
	'''

	def __init__(self, base_path_in='complex1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros(0, numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class complex1dstruct_arraytemporary_nt_1dcObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex1dstruct_arraytemporary_nt_1dcObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putVect1DComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DComplexInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.complex128))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex1dstruct_arraytemporary_nt_1dcObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getVect1DComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DComplexFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class float2dstruct_arraytemporary_nt_2dr:
	'''
	class float2dstruct_arraytemporary_nt_2dr
	Constant 2D float

	Attributes:
	- array : list of float2dstruct_arraytemporary_nt_2drObj 
	'''

	def __init__(self, base_path_in='float2d'):
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
		ret = space + 'class float2dstruct_arraytemporary_nt_2dr\n'
		for i in range(len(self.array)):
			ret = ret + space + 'float2dstruct_arraytemporary_nt_2dr[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(float2dstruct_arraytemporary_nt_2drObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_nt_2dr, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_nt_2dr, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_nt_2dr, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_nt_2dr, run function getSlice') 
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
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_nt_2dr, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(float2dstruct_arraytemporary_nt_2dr(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = float2dstruct_arraytemporary_nt_2dr(self.base_path)
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
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_nt_2dr, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_nt_2dr, run function getNonTimedElt') 
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


class float2dstruct_arraytemporary_nt_2drObj(KeepInOrder):
	'''
	class float2dstruct_arraytemporary_nt_2drObj
	Constant 2D float

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 2D with float
	   Value. Matrix of float.
	'''

	def __init__(self, base_path_in='float2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class float2dstruct_arraytemporary_nt_2drObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float2dstruct_arraytemporary_nt_2drObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float2dstruct_arraytemporary_nt_2drObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class integer2dstruct_arraytemporary_nt_2di:
	'''
	class integer2dstruct_arraytemporary_nt_2di
	Constant 2D integer

	Attributes:
	- array : list of integer2dstruct_arraytemporary_nt_2diObj 
	'''

	def __init__(self, base_path_in='integer2d'):
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
		ret = space + 'class integer2dstruct_arraytemporary_nt_2di\n'
		for i in range(len(self.array)):
			ret = ret + space + 'integer2dstruct_arraytemporary_nt_2di[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(integer2dstruct_arraytemporary_nt_2diObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_nt_2di, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_nt_2di, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_nt_2di, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_nt_2di, run function getSlice') 
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
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_nt_2di, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(integer2dstruct_arraytemporary_nt_2di(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = integer2dstruct_arraytemporary_nt_2di(self.base_path)
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
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_nt_2di, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_nt_2di, run function getNonTimedElt') 
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


class integer2dstruct_arraytemporary_nt_2diObj(KeepInOrder):
	'''
	class integer2dstruct_arraytemporary_nt_2diObj
	Constant 2D integer

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 2D with int
	   Value. Matrix of integers
	'''

	def __init__(self, base_path_in='integer2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0), numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class integer2dstruct_arraytemporary_nt_2diObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer2dstruct_arraytemporary_nt_2diObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer2dstruct_arraytemporary_nt_2diObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class complex2dstruct_arraytemporary_nt_2dc:
	'''
	class complex2dstruct_arraytemporary_nt_2dc
	Constant 2D complex

	Attributes:
	- array : list of complex2dstruct_arraytemporary_nt_2dcObj 
	'''

	def __init__(self, base_path_in='complex2d'):
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
		ret = space + 'class complex2dstruct_arraytemporary_nt_2dc\n'
		for i in range(len(self.array)):
			ret = ret + space + 'complex2dstruct_arraytemporary_nt_2dc[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(complex2dstruct_arraytemporary_nt_2dcObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_nt_2dc, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_nt_2dc, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_nt_2dc, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_nt_2dc, run function getSlice') 
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
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_nt_2dc, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(complex2dstruct_arraytemporary_nt_2dc(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = complex2dstruct_arraytemporary_nt_2dc(self.base_path)
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
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_nt_2dc, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_nt_2dc, run function getNonTimedElt') 
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


class complex2dstruct_arraytemporary_nt_2dcObj(KeepInOrder):
	'''
	class complex2dstruct_arraytemporary_nt_2dcObj
	Constant 2D complex

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 2D with  complex numbers
	   Value. Matrix of complex numbers
	'''

	def __init__(self, base_path_in='complex2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class complex2dstruct_arraytemporary_nt_2dcObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex2dstruct_arraytemporary_nt_2dcObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.complex128))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex2dstruct_arraytemporary_nt_2dcObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class float3dstruct_arraytemporary_nt_3dr:
	'''
	class float3dstruct_arraytemporary_nt_3dr
	Constant 3D float

	Attributes:
	- array : list of float3dstruct_arraytemporary_nt_3drObj 
	'''

	def __init__(self, base_path_in='float3d'):
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
		ret = space + 'class float3dstruct_arraytemporary_nt_3dr\n'
		for i in range(len(self.array)):
			ret = ret + space + 'float3dstruct_arraytemporary_nt_3dr[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(float3dstruct_arraytemporary_nt_3drObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_nt_3dr, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_nt_3dr, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_nt_3dr, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_nt_3dr, run function getSlice') 
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
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_nt_3dr, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(float3dstruct_arraytemporary_nt_3dr(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = float3dstruct_arraytemporary_nt_3dr(self.base_path)
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
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_nt_3dr, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_nt_3dr, run function getNonTimedElt') 
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


class float3dstruct_arraytemporary_nt_3drObj(KeepInOrder):
	'''
	class float3dstruct_arraytemporary_nt_3drObj
	Constant 3D float

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 3D with float
	   Value. array 3D of floats
	'''

	def __init__(self, base_path_in='float3d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class float3dstruct_arraytemporary_nt_3drObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float3dstruct_arraytemporary_nt_3drObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float3dstruct_arraytemporary_nt_3drObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class integer3dstruct_arraytemporary_nt_3di:
	'''
	class integer3dstruct_arraytemporary_nt_3di
	Constant 3D integer

	Attributes:
	- array : list of integer3dstruct_arraytemporary_nt_3diObj 
	'''

	def __init__(self, base_path_in='integer3d'):
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
		ret = space + 'class integer3dstruct_arraytemporary_nt_3di\n'
		for i in range(len(self.array)):
			ret = ret + space + 'integer3dstruct_arraytemporary_nt_3di[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(integer3dstruct_arraytemporary_nt_3diObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_nt_3di, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_nt_3di, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_nt_3di, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_nt_3di, run function getSlice') 
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
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_nt_3di, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(integer3dstruct_arraytemporary_nt_3di(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = integer3dstruct_arraytemporary_nt_3di(self.base_path)
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
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_nt_3di, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_nt_3di, run function getNonTimedElt') 
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


class integer3dstruct_arraytemporary_nt_3diObj(KeepInOrder):
	'''
	class integer3dstruct_arraytemporary_nt_3diObj
	Constant 3D integer

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 3D with int
	   Value. array 3D of integers
	'''

	def __init__(self, base_path_in='integer3d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0,0), numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class integer3dstruct_arraytemporary_nt_3diObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer3dstruct_arraytemporary_nt_3diObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putVect3DIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DIntInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer3dstruct_arraytemporary_nt_3diObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getVect3DIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect3DIntFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class complex3dstruct_arraytemporary_nt_3dc:
	'''
	class complex3dstruct_arraytemporary_nt_3dc
	Constant 3D complex

	Attributes:
	- array : list of complex3dstruct_arraytemporary_nt_3dcObj 
	'''

	def __init__(self, base_path_in='complex3d'):
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
		ret = space + 'class complex3dstruct_arraytemporary_nt_3dc\n'
		for i in range(len(self.array)):
			ret = ret + space + 'complex3dstruct_arraytemporary_nt_3dc[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(complex3dstruct_arraytemporary_nt_3dcObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_nt_3dc, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_nt_3dc, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_nt_3dc, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_nt_3dc, run function getSlice') 
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
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_nt_3dc, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(complex3dstruct_arraytemporary_nt_3dc(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = complex3dstruct_arraytemporary_nt_3dc(self.base_path)
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
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_nt_3dc, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_nt_3dc, run function getNonTimedElt') 
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


class complex3dstruct_arraytemporary_nt_3dcObj(KeepInOrder):
	'''
	class complex3dstruct_arraytemporary_nt_3dcObj
	Constant 3D complex

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 3D with  complex numbers
	   Value. array 3D of complex numbers
	'''

	def __init__(self, base_path_in='complex3d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class complex3dstruct_arraytemporary_nt_3dcObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex3dstruct_arraytemporary_nt_3dcObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putVect3DComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DComplexInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.complex128))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex3dstruct_arraytemporary_nt_3dcObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getVect3DComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect3DComplexFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class float4dstruct_arraytemporary_nt_4dr:
	'''
	class float4dstruct_arraytemporary_nt_4dr
	Constant 4D float

	Attributes:
	- array : list of float4dstruct_arraytemporary_nt_4drObj 
	'''

	def __init__(self, base_path_in='float4d'):
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
		ret = space + 'class float4dstruct_arraytemporary_nt_4dr\n'
		for i in range(len(self.array)):
			ret = ret + space + 'float4dstruct_arraytemporary_nt_4dr[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(float4dstruct_arraytemporary_nt_4drObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_nt_4dr, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_nt_4dr, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_nt_4dr, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_nt_4dr, run function getSlice') 
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
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_nt_4dr, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(float4dstruct_arraytemporary_nt_4dr(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = float4dstruct_arraytemporary_nt_4dr(self.base_path)
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
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_nt_4dr, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_nt_4dr, run function getNonTimedElt') 
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


class float4dstruct_arraytemporary_nt_4drObj(KeepInOrder):
	'''
	class float4dstruct_arraytemporary_nt_4drObj
	Constant 4D float

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 4D with float
	   Value. array 4D of floats
	'''

	def __init__(self, base_path_in='float4d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class float4dstruct_arraytemporary_nt_4drObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float4dstruct_arraytemporary_nt_4drObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float4dstruct_arraytemporary_nt_4drObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value


class timedstructuretemporary_t(KeepInOrder):
	'''
	class timedstructuretemporary_t
	Time-dependent quantities

	Attributes:
	- float0d : class float0dstruct_arraytemporary_t_0dr: array of float0dstruct_arraytemporary_t_0drObj objects
	   Time-dependent 0D float
	- integer0d : class integer0dstruct_arraytemporary_t_0di: array of integer0dstruct_arraytemporary_t_0diObj objects
	   Time-dependent 0D integer. 
	- complex0d : class complex0dstruct_arraytemporary_t_0dc: array of complex0dstruct_arraytemporary_t_0dcObj objects
	   Time-dependent 0D complex.
	- string0d : class string0dstruct_arraytemporary_t_0ds: array of string0dstruct_arraytemporary_t_0dsObj objects
	   Time-dependent 0D string.
	- float1d : class float1dstruct_arraytemporary_t_1dr: array of float1dstruct_arraytemporary_t_1drObj objects
	   Time-dependent 1D float.
	- integer1d : class integer1dstruct_arraytemporary_t_1di: array of integer1dstruct_arraytemporary_t_1diObj objects
	   Time-dependent 1D integer.
	- complex1d : class complex1dstruct_arraytemporary_t_1dc: array of complex1dstruct_arraytemporary_t_1dcObj objects
	   Time-dependent 1D complex
	- float2d : class float2dstruct_arraytemporary_t_2dr: array of float2dstruct_arraytemporary_t_2drObj objects
	   Time-dependent 2D float
	- integer2d : class integer2dstruct_arraytemporary_t_2di: array of integer2dstruct_arraytemporary_t_2diObj objects
	   Time-dependent 2D integer
	- complex2d : class complex2dstruct_arraytemporary_t_2dc: array of complex2dstruct_arraytemporary_t_2dcObj objects
	   Time-dependent 2D complex
	- float3d : class float3dstruct_arraytemporary_t_3dr: array of float3dstruct_arraytemporary_t_3drObj objects
	   Time-dependent 3D float
	- integer3d : class integer3dstruct_arraytemporary_t_3di: array of integer3dstruct_arraytemporary_t_3diObj objects
	   Time-dependent 3D integer
	- complex3d : class complex3dstruct_arraytemporary_t_3dc: array of complex3dstruct_arraytemporary_t_3dcObj objects
	   Time-dependent 3D complex
	- float4d : class float4dstruct_arraytemporary_t_4dr: array of float4dstruct_arraytemporary_t_4drObj objects
	   Time-dependent 4D float
	'''

	def __init__(self, base_path_in='timed'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.float0d = float0dstruct_arraytemporary_t_0dr('float0d')
		self.integer0d = integer0dstruct_arraytemporary_t_0di('integer0d')
		self.complex0d = complex0dstruct_arraytemporary_t_0dc('complex0d')
		self.string0d = string0dstruct_arraytemporary_t_0ds('string0d')
		self.float1d = float1dstruct_arraytemporary_t_1dr('float1d')
		self.integer1d = integer1dstruct_arraytemporary_t_1di('integer1d')
		self.complex1d = complex1dstruct_arraytemporary_t_1dc('complex1d')
		self.float2d = float2dstruct_arraytemporary_t_2dr('float2d')
		self.integer2d = integer2dstruct_arraytemporary_t_2di('integer2d')
		self.complex2d = complex2dstruct_arraytemporary_t_2dc('complex2d')
		self.float3d = float3dstruct_arraytemporary_t_3dr('float3d')
		self.integer3d = integer3dstruct_arraytemporary_t_3di('integer3d')
		self.complex3d = complex3dstruct_arraytemporary_t_3dc('complex3d')
		self.float4d = float4dstruct_arraytemporary_t_4dr('float4d')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class timedstructuretemporary_t\n'
		ret = ret + space + 'Attribute float0d\n ' + self.float0d.__str__(depth+1)
		ret = ret + space + 'Attribute integer0d\n ' + self.integer0d.__str__(depth+1)
		ret = ret + space + 'Attribute complex0d\n ' + self.complex0d.__str__(depth+1)
		ret = ret + space + 'Attribute string0d\n ' + self.string0d.__str__(depth+1)
		ret = ret + space + 'Attribute float1d\n ' + self.float1d.__str__(depth+1)
		ret = ret + space + 'Attribute integer1d\n ' + self.integer1d.__str__(depth+1)
		ret = ret + space + 'Attribute complex1d\n ' + self.complex1d.__str__(depth+1)
		ret = ret + space + 'Attribute float2d\n ' + self.float2d.__str__(depth+1)
		ret = ret + space + 'Attribute integer2d\n ' + self.integer2d.__str__(depth+1)
		ret = ret + space + 'Attribute complex2d\n ' + self.complex2d.__str__(depth+1)
		ret = ret + space + 'Attribute float3d\n ' + self.float3d.__str__(depth+1)
		ret = ret + space + 'Attribute integer3d\n ' + self.integer3d.__str__(depth+1)
		ret = ret + space + 'Attribute complex3d\n ' + self.complex3d.__str__(depth+1)
		ret = ret + space + 'Attribute float4d\n ' + self.float4d.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.float0d.setExpIdx(idx)
		self.integer0d.setExpIdx(idx)
		self.complex0d.setExpIdx(idx)
		self.string0d.setExpIdx(idx)
		self.float1d.setExpIdx(idx)
		self.integer1d.setExpIdx(idx)
		self.complex1d.setExpIdx(idx)
		self.float2d.setExpIdx(idx)
		self.integer2d.setExpIdx(idx)
		self.complex2d.setExpIdx(idx)
		self.float3d.setExpIdx(idx)
		self.integer3d.setExpIdx(idx)
		self.complex3d.setExpIdx(idx)
		self.float4d.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type timedstructuretemporary_t, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.float0d.cpoTime = self.cpoTime
		self.float0d.putSlice(path, cpopath)
		self.integer0d.cpoTime = self.cpoTime
		self.integer0d.putSlice(path, cpopath)
		self.complex0d.cpoTime = self.cpoTime
		self.complex0d.putSlice(path, cpopath)
		self.string0d.cpoTime = self.cpoTime
		self.string0d.putSlice(path, cpopath)
		self.float1d.cpoTime = self.cpoTime
		self.float1d.putSlice(path, cpopath)
		self.integer1d.cpoTime = self.cpoTime
		self.integer1d.putSlice(path, cpopath)
		self.complex1d.cpoTime = self.cpoTime
		self.complex1d.putSlice(path, cpopath)
		self.float2d.cpoTime = self.cpoTime
		self.float2d.putSlice(path, cpopath)
		self.integer2d.cpoTime = self.cpoTime
		self.integer2d.putSlice(path, cpopath)
		self.complex2d.cpoTime = self.cpoTime
		self.complex2d.putSlice(path, cpopath)
		self.float3d.cpoTime = self.cpoTime
		self.float3d.putSlice(path, cpopath)
		self.integer3d.cpoTime = self.cpoTime
		self.integer3d.putSlice(path, cpopath)
		self.complex3d.cpoTime = self.cpoTime
		self.complex3d.putSlice(path, cpopath)
		self.float4d.cpoTime = self.cpoTime
		self.float4d.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type timedstructuretemporary_t, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.float0d.replaceLastSlice(path, cpopath)
		self.integer0d.replaceLastSlice(path, cpopath)
		self.complex0d.replaceLastSlice(path, cpopath)
		self.string0d.replaceLastSlice(path, cpopath)
		self.float1d.replaceLastSlice(path, cpopath)
		self.integer1d.replaceLastSlice(path, cpopath)
		self.complex1d.replaceLastSlice(path, cpopath)
		self.float2d.replaceLastSlice(path, cpopath)
		self.integer2d.replaceLastSlice(path, cpopath)
		self.complex2d.replaceLastSlice(path, cpopath)
		self.float3d.replaceLastSlice(path, cpopath)
		self.integer3d.replaceLastSlice(path, cpopath)
		self.complex3d.replaceLastSlice(path, cpopath)
		self.float4d.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type timedstructuretemporary_t, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.float0d.putNonTimed(path, cpopath)
		self.integer0d.putNonTimed(path, cpopath)
		self.complex0d.putNonTimed(path, cpopath)
		self.string0d.putNonTimed(path, cpopath)
		self.float1d.putNonTimed(path, cpopath)
		self.integer1d.putNonTimed(path, cpopath)
		self.complex1d.putNonTimed(path, cpopath)
		self.float2d.putNonTimed(path, cpopath)
		self.integer2d.putNonTimed(path, cpopath)
		self.complex2d.putNonTimed(path, cpopath)
		self.float3d.putNonTimed(path, cpopath)
		self.integer3d.putNonTimed(path, cpopath)
		self.complex3d.putNonTimed(path, cpopath)
		self.float4d.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type timedstructuretemporary_t, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.float0d.getSlice(path, cpopath, inTime, interpolMode)
		self.integer0d.getSlice(path, cpopath, inTime, interpolMode)
		self.complex0d.getSlice(path, cpopath, inTime, interpolMode)
		self.string0d.getSlice(path, cpopath, inTime, interpolMode)
		self.float1d.getSlice(path, cpopath, inTime, interpolMode)
		self.integer1d.getSlice(path, cpopath, inTime, interpolMode)
		self.complex1d.getSlice(path, cpopath, inTime, interpolMode)
		self.float2d.getSlice(path, cpopath, inTime, interpolMode)
		self.integer2d.getSlice(path, cpopath, inTime, interpolMode)
		self.complex2d.getSlice(path, cpopath, inTime, interpolMode)
		self.float3d.getSlice(path, cpopath, inTime, interpolMode)
		self.integer3d.getSlice(path, cpopath, inTime, interpolMode)
		self.complex3d.getSlice(path, cpopath, inTime, interpolMode)
		self.float4d.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type timedstructuretemporary_t, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			float0dList = self.float0d.build_non_resampled_data(path, cpopath, nbslice)
			integer0dList = self.integer0d.build_non_resampled_data(path, cpopath, nbslice)
			complex0dList = self.complex0d.build_non_resampled_data(path, cpopath, nbslice)
			string0dList = self.string0d.build_non_resampled_data(path, cpopath, nbslice)
			float1dList = self.float1d.build_non_resampled_data(path, cpopath, nbslice)
			integer1dList = self.integer1d.build_non_resampled_data(path, cpopath, nbslice)
			complex1dList = self.complex1d.build_non_resampled_data(path, cpopath, nbslice)
			float2dList = self.float2d.build_non_resampled_data(path, cpopath, nbslice)
			integer2dList = self.integer2d.build_non_resampled_data(path, cpopath, nbslice)
			complex2dList = self.complex2d.build_non_resampled_data(path, cpopath, nbslice)
			float3dList = self.float3d.build_non_resampled_data(path, cpopath, nbslice)
			integer3dList = self.integer3d.build_non_resampled_data(path, cpopath, nbslice)
			complex3dList = self.complex3d.build_non_resampled_data(path, cpopath, nbslice)
			float4dList = self.float4d.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = timedstructuretemporary_t(self.base_path)
				slice.setExpIdx(self.idx)
				slice.float0d = float0dList[i]
				slice.integer0d = integer0dList[i]
				slice.complex0d = complex0dList[i]
				slice.string0d = string0dList[i]
				slice.float1d = float1dList[i]
				slice.integer1d = integer1dList[i]
				slice.complex1d = complex1dList[i]
				slice.float2d = float2dList[i]
				slice.integer2d = integer2dList[i]
				slice.complex2d = complex2dList[i]
				slice.float3d = float3dList[i]
				slice.integer3d = integer3dList[i]
				slice.complex3d = complex3dList[i]
				slice.float4d = float4dList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type timedstructuretemporary_tObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.float0d.putTimedElt(path, cpopath + 'float0d', i, obj)
		obj = self.integer0d.putTimedElt(path, cpopath + 'integer0d', i, obj)
		obj = self.complex0d.putTimedElt(path, cpopath + 'complex0d', i, obj)
		obj = self.string0d.putTimedElt(path, cpopath + 'string0d', i, obj)
		obj = self.float1d.putTimedElt(path, cpopath + 'float1d', i, obj)
		obj = self.integer1d.putTimedElt(path, cpopath + 'integer1d', i, obj)
		obj = self.complex1d.putTimedElt(path, cpopath + 'complex1d', i, obj)
		obj = self.float2d.putTimedElt(path, cpopath + 'float2d', i, obj)
		obj = self.integer2d.putTimedElt(path, cpopath + 'integer2d', i, obj)
		obj = self.complex2d.putTimedElt(path, cpopath + 'complex2d', i, obj)
		obj = self.float3d.putTimedElt(path, cpopath + 'float3d', i, obj)
		obj = self.integer3d.putTimedElt(path, cpopath + 'integer3d', i, obj)
		obj = self.complex3d.putTimedElt(path, cpopath + 'complex3d', i, obj)
		obj = self.float4d.putTimedElt(path, cpopath + 'float4d', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type timedstructuretemporary_tObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.float0d.getTimedElt(path, cpopath + 'float0d', i, obj)
		self.integer0d.getTimedElt(path, cpopath + 'integer0d', i, obj)
		self.complex0d.getTimedElt(path, cpopath + 'complex0d', i, obj)
		self.string0d.getTimedElt(path, cpopath + 'string0d', i, obj)
		self.float1d.getTimedElt(path, cpopath + 'float1d', i, obj)
		self.integer1d.getTimedElt(path, cpopath + 'integer1d', i, obj)
		self.complex1d.getTimedElt(path, cpopath + 'complex1d', i, obj)
		self.float2d.getTimedElt(path, cpopath + 'float2d', i, obj)
		self.integer2d.getTimedElt(path, cpopath + 'integer2d', i, obj)
		self.complex2d.getTimedElt(path, cpopath + 'complex2d', i, obj)
		self.float3d.getTimedElt(path, cpopath + 'float3d', i, obj)
		self.integer3d.getTimedElt(path, cpopath + 'integer3d', i, obj)
		self.complex3d.getTimedElt(path, cpopath + 'complex3d', i, obj)
		self.float4d.getTimedElt(path, cpopath + 'float4d', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type timedstructuretemporary_tObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.float0d.putNonTimedElt(path, cpopath + 'float0d', i, obj)
		obj = self.integer0d.putNonTimedElt(path, cpopath + 'integer0d', i, obj)
		obj = self.complex0d.putNonTimedElt(path, cpopath + 'complex0d', i, obj)
		obj = self.string0d.putNonTimedElt(path, cpopath + 'string0d', i, obj)
		obj = self.float1d.putNonTimedElt(path, cpopath + 'float1d', i, obj)
		obj = self.integer1d.putNonTimedElt(path, cpopath + 'integer1d', i, obj)
		obj = self.complex1d.putNonTimedElt(path, cpopath + 'complex1d', i, obj)
		obj = self.float2d.putNonTimedElt(path, cpopath + 'float2d', i, obj)
		obj = self.integer2d.putNonTimedElt(path, cpopath + 'integer2d', i, obj)
		obj = self.complex2d.putNonTimedElt(path, cpopath + 'complex2d', i, obj)
		obj = self.float3d.putNonTimedElt(path, cpopath + 'float3d', i, obj)
		obj = self.integer3d.putNonTimedElt(path, cpopath + 'integer3d', i, obj)
		obj = self.complex3d.putNonTimedElt(path, cpopath + 'complex3d', i, obj)
		obj = self.float4d.putNonTimedElt(path, cpopath + 'float4d', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type timedstructuretemporary_tObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.float0d.getNonTimedElt(path, cpopath + 'float0d', i, obj)
		self.integer0d.getNonTimedElt(path, cpopath + 'integer0d', i, obj)
		self.complex0d.getNonTimedElt(path, cpopath + 'complex0d', i, obj)
		self.string0d.getNonTimedElt(path, cpopath + 'string0d', i, obj)
		self.float1d.getNonTimedElt(path, cpopath + 'float1d', i, obj)
		self.integer1d.getNonTimedElt(path, cpopath + 'integer1d', i, obj)
		self.complex1d.getNonTimedElt(path, cpopath + 'complex1d', i, obj)
		self.float2d.getNonTimedElt(path, cpopath + 'float2d', i, obj)
		self.integer2d.getNonTimedElt(path, cpopath + 'integer2d', i, obj)
		self.complex2d.getNonTimedElt(path, cpopath + 'complex2d', i, obj)
		self.float3d.getNonTimedElt(path, cpopath + 'float3d', i, obj)
		self.integer3d.getNonTimedElt(path, cpopath + 'integer3d', i, obj)
		self.complex3d.getNonTimedElt(path, cpopath + 'complex3d', i, obj)
		self.float4d.getNonTimedElt(path, cpopath + 'float4d', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'float0d')
		ull.deleteData(self.idx, path, cpopath + 'integer0d')
		ull.deleteData(self.idx, path, cpopath + 'complex0d')
		ull.deleteData(self.idx, path, cpopath + 'string0d')
		ull.deleteData(self.idx, path, cpopath + 'float1d')
		ull.deleteData(self.idx, path, cpopath + 'integer1d')
		ull.deleteData(self.idx, path, cpopath + 'complex1d')
		ull.deleteData(self.idx, path, cpopath + 'float2d')
		ull.deleteData(self.idx, path, cpopath + 'integer2d')
		ull.deleteData(self.idx, path, cpopath + 'complex2d')
		ull.deleteData(self.idx, path, cpopath + 'float3d')
		ull.deleteData(self.idx, path, cpopath + 'integer3d')
		ull.deleteData(self.idx, path, cpopath + 'complex3d')
		ull.deleteData(self.idx, path, cpopath + 'float4d')


class float0dstruct_arraytemporary_t_0dr:
	'''
	class float0dstruct_arraytemporary_t_0dr
	Time-dependent 0D float

	Attributes:
	- array : list of float0dstruct_arraytemporary_t_0drObj 
	'''

	def __init__(self, base_path_in='float0d'):
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
		ret = space + 'class float0dstruct_arraytemporary_t_0dr\n'
		for i in range(len(self.array)):
			ret = ret + space + 'float0dstruct_arraytemporary_t_0dr[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(float0dstruct_arraytemporary_t_0drObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_t_0dr, run function putSlice') 
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
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_t_0dr, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_t_0dr, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_t_0dr, run function getSlice') 
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
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_t_0dr, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(float0dstruct_arraytemporary_t_0dr(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(float0dstruct_arraytemporary_t_0dr(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = float0dstruct_arraytemporary_t_0dr(self.base_path)
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
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_t_0dr, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_t_0dr, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_t_0dr, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type float0dstruct_arraytemporary_t_0dr, run function getNonTimedElt') 
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


class float0dstruct_arraytemporary_t_0drObj(KeepInOrder):
	'''
	class float0dstruct_arraytemporary_t_0drObj
	Time-dependent 0D float

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : float
	   Value. Time-dependent. Real scalar.
	'''

	def __init__(self, base_path_in='float0d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class float0dstruct_arraytemporary_t_0drObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float0dstruct_arraytemporary_t_0drObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'value', i, self.value)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float0dstruct_arraytemporary_t_0drObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float0dstruct_arraytemporary_t_0drObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float0dstruct_arraytemporary_t_0drObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class integer0dstruct_arraytemporary_t_0di:
	'''
	class integer0dstruct_arraytemporary_t_0di
	Time-dependent 0D integer. 

	Attributes:
	- array : list of integer0dstruct_arraytemporary_t_0diObj 
	'''

	def __init__(self, base_path_in='integer0d'):
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
		ret = space + 'class integer0dstruct_arraytemporary_t_0di\n'
		for i in range(len(self.array)):
			ret = ret + space + 'integer0dstruct_arraytemporary_t_0di[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(integer0dstruct_arraytemporary_t_0diObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_t_0di, run function putSlice') 
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
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_t_0di, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_t_0di, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_t_0di, run function getSlice') 
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
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_t_0di, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(integer0dstruct_arraytemporary_t_0di(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(integer0dstruct_arraytemporary_t_0di(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = integer0dstruct_arraytemporary_t_0di(self.base_path)
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
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_t_0di, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_t_0di, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_t_0di, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type integer0dstruct_arraytemporary_t_0di, run function getNonTimedElt') 
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


class integer0dstruct_arraytemporary_t_0diObj(KeepInOrder):
	'''
	class integer0dstruct_arraytemporary_t_0diObj
	Time-dependent 0D integer. 

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : int
	   Value. Time-dependent. integer scalar
	'''

	def __init__(self, base_path_in='integer0d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class integer0dstruct_arraytemporary_t_0diObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer0dstruct_arraytemporary_t_0diObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'value', i, self.value)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer0dstruct_arraytemporary_t_0diObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getIntFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer0dstruct_arraytemporary_t_0diObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer0dstruct_arraytemporary_t_0diObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class complex0dstruct_arraytemporary_t_0dc:
	'''
	class complex0dstruct_arraytemporary_t_0dc
	Time-dependent 0D complex.

	Attributes:
	- array : list of complex0dstruct_arraytemporary_t_0dcObj 
	'''

	def __init__(self, base_path_in='complex0d'):
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
		ret = space + 'class complex0dstruct_arraytemporary_t_0dc\n'
		for i in range(len(self.array)):
			ret = ret + space + 'complex0dstruct_arraytemporary_t_0dc[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(complex0dstruct_arraytemporary_t_0dcObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_t_0dc, run function putSlice') 
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
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_t_0dc, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_t_0dc, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_t_0dc, run function getSlice') 
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
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_t_0dc, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(complex0dstruct_arraytemporary_t_0dc(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(complex0dstruct_arraytemporary_t_0dc(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = complex0dstruct_arraytemporary_t_0dc(self.base_path)
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
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_t_0dc, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_t_0dc, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_t_0dc, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type complex0dstruct_arraytemporary_t_0dc, run function getNonTimedElt') 
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


class complex0dstruct_arraytemporary_t_0dcObj(KeepInOrder):
	'''
	class complex0dstruct_arraytemporary_t_0dcObj
	Time-dependent 0D complex.

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : complex
	   Value. Time-dependent. Complex scalar.
	'''

	def __init__(self, base_path_in='complex0d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = EMPTY_COMPLEX

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class complex0dstruct_arraytemporary_t_0dcObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex0dstruct_arraytemporary_t_0dcObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putComplexInObject(self.idx, obj, cpopath + 'value', i, self.value)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex0dstruct_arraytemporary_t_0dcObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getComplexFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex0dstruct_arraytemporary_t_0dcObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex0dstruct_arraytemporary_t_0dcObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class string0dstruct_arraytemporary_t_0ds:
	'''
	class string0dstruct_arraytemporary_t_0ds
	Time-dependent 0D string.

	Attributes:
	- array : list of string0dstruct_arraytemporary_t_0dsObj 
	'''

	def __init__(self, base_path_in='string0d'):
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
		ret = space + 'class string0dstruct_arraytemporary_t_0ds\n'
		for i in range(len(self.array)):
			ret = ret + space + 'string0dstruct_arraytemporary_t_0ds[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(string0dstruct_arraytemporary_t_0dsObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_t_0ds, run function putSlice') 
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
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_t_0ds, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_t_0ds, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_t_0ds, run function getSlice') 
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
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_t_0ds, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(string0dstruct_arraytemporary_t_0ds(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(string0dstruct_arraytemporary_t_0ds(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = string0dstruct_arraytemporary_t_0ds(self.base_path)
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
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_t_0ds, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_t_0ds, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_t_0ds, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type string0dstruct_arraytemporary_t_0ds, run function getNonTimedElt') 
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


class string0dstruct_arraytemporary_t_0dsObj(KeepInOrder):
	'''
	class string0dstruct_arraytemporary_t_0dsObj
	Time-dependent 0D string.

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : str
	   Value. Time-dependent. String.
	'''

	def __init__(self, base_path_in='string0d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class string0dstruct_arraytemporary_t_0dsObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type string0dstruct_arraytemporary_t_0dsObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'value', i, self.value)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type string0dstruct_arraytemporary_t_0dsObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getStringFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type string0dstruct_arraytemporary_t_0dsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type string0dstruct_arraytemporary_t_0dsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class float1dstruct_arraytemporary_t_1dr:
	'''
	class float1dstruct_arraytemporary_t_1dr
	Time-dependent 1D float.

	Attributes:
	- array : list of float1dstruct_arraytemporary_t_1drObj 
	'''

	def __init__(self, base_path_in='float1d'):
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
		ret = space + 'class float1dstruct_arraytemporary_t_1dr\n'
		for i in range(len(self.array)):
			ret = ret + space + 'float1dstruct_arraytemporary_t_1dr[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(float1dstruct_arraytemporary_t_1drObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_t_1dr, run function putSlice') 
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
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_t_1dr, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_t_1dr, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_t_1dr, run function getSlice') 
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
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_t_1dr, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(float1dstruct_arraytemporary_t_1dr(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(float1dstruct_arraytemporary_t_1dr(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = float1dstruct_arraytemporary_t_1dr(self.base_path)
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
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_t_1dr, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_t_1dr, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_t_1dr, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type float1dstruct_arraytemporary_t_1dr, run function getNonTimedElt') 
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


class float1dstruct_arraytemporary_t_1drObj(KeepInOrder):
	'''
	class float1dstruct_arraytemporary_t_1drObj
	Time-dependent 1D float.

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 1D with float
	   Value. Time-dependent. Vector of float.
	'''

	def __init__(self, base_path_in='float1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class float1dstruct_arraytemporary_t_1drObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float1dstruct_arraytemporary_t_1drObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float1dstruct_arraytemporary_t_1drObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float1dstruct_arraytemporary_t_1drObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float1dstruct_arraytemporary_t_1drObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class integer1dstruct_arraytemporary_t_1di:
	'''
	class integer1dstruct_arraytemporary_t_1di
	Time-dependent 1D integer.

	Attributes:
	- array : list of integer1dstruct_arraytemporary_t_1diObj 
	'''

	def __init__(self, base_path_in='integer1d'):
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
		ret = space + 'class integer1dstruct_arraytemporary_t_1di\n'
		for i in range(len(self.array)):
			ret = ret + space + 'integer1dstruct_arraytemporary_t_1di[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(integer1dstruct_arraytemporary_t_1diObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_t_1di, run function putSlice') 
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
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_t_1di, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_t_1di, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_t_1di, run function getSlice') 
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
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_t_1di, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(integer1dstruct_arraytemporary_t_1di(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(integer1dstruct_arraytemporary_t_1di(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = integer1dstruct_arraytemporary_t_1di(self.base_path)
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
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_t_1di, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_t_1di, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_t_1di, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type integer1dstruct_arraytemporary_t_1di, run function getNonTimedElt') 
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


class integer1dstruct_arraytemporary_t_1diObj(KeepInOrder):
	'''
	class integer1dstruct_arraytemporary_t_1diObj
	Time-dependent 1D integer.

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 1D with int)
	   Value. Time-dependent. Vector of integers
	'''

	def __init__(self, base_path_in='integer1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros(0, numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class integer1dstruct_arraytemporary_t_1diObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer1dstruct_arraytemporary_t_1diObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.int32))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer1dstruct_arraytemporary_t_1diObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer1dstruct_arraytemporary_t_1diObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer1dstruct_arraytemporary_t_1diObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class complex1dstruct_arraytemporary_t_1dc:
	'''
	class complex1dstruct_arraytemporary_t_1dc
	Time-dependent 1D complex

	Attributes:
	- array : list of complex1dstruct_arraytemporary_t_1dcObj 
	'''

	def __init__(self, base_path_in='complex1d'):
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
		ret = space + 'class complex1dstruct_arraytemporary_t_1dc\n'
		for i in range(len(self.array)):
			ret = ret + space + 'complex1dstruct_arraytemporary_t_1dc[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(complex1dstruct_arraytemporary_t_1dcObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_t_1dc, run function putSlice') 
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
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_t_1dc, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_t_1dc, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_t_1dc, run function getSlice') 
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
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_t_1dc, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(complex1dstruct_arraytemporary_t_1dc(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(complex1dstruct_arraytemporary_t_1dc(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = complex1dstruct_arraytemporary_t_1dc(self.base_path)
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
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_t_1dc, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_t_1dc, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_t_1dc, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type complex1dstruct_arraytemporary_t_1dc, run function getNonTimedElt') 
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


class complex1dstruct_arraytemporary_t_1dcObj(KeepInOrder):
	'''
	class complex1dstruct_arraytemporary_t_1dcObj
	Time-dependent 1D complex

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 1D with  complex numbers
	   Value. Time-dependent. Vector of complex numbers
	'''

	def __init__(self, base_path_in='complex1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros(0, numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class complex1dstruct_arraytemporary_t_1dcObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex1dstruct_arraytemporary_t_1dcObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DComplexInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex1dstruct_arraytemporary_t_1dcObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect1DComplexFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex1dstruct_arraytemporary_t_1dcObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex1dstruct_arraytemporary_t_1dcObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class float2dstruct_arraytemporary_t_2dr:
	'''
	class float2dstruct_arraytemporary_t_2dr
	Time-dependent 2D float

	Attributes:
	- array : list of float2dstruct_arraytemporary_t_2drObj 
	'''

	def __init__(self, base_path_in='float2d'):
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
		ret = space + 'class float2dstruct_arraytemporary_t_2dr\n'
		for i in range(len(self.array)):
			ret = ret + space + 'float2dstruct_arraytemporary_t_2dr[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(float2dstruct_arraytemporary_t_2drObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_t_2dr, run function putSlice') 
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
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_t_2dr, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_t_2dr, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_t_2dr, run function getSlice') 
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
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_t_2dr, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(float2dstruct_arraytemporary_t_2dr(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(float2dstruct_arraytemporary_t_2dr(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = float2dstruct_arraytemporary_t_2dr(self.base_path)
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
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_t_2dr, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_t_2dr, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_t_2dr, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type float2dstruct_arraytemporary_t_2dr, run function getNonTimedElt') 
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


class float2dstruct_arraytemporary_t_2drObj(KeepInOrder):
	'''
	class float2dstruct_arraytemporary_t_2drObj
	Time-dependent 2D float

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 2D with float
	   Value. Time-dependent. Matrix of float.
	'''

	def __init__(self, base_path_in='float2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class float2dstruct_arraytemporary_t_2drObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float2dstruct_arraytemporary_t_2drObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float2dstruct_arraytemporary_t_2drObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float2dstruct_arraytemporary_t_2drObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float2dstruct_arraytemporary_t_2drObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class integer2dstruct_arraytemporary_t_2di:
	'''
	class integer2dstruct_arraytemporary_t_2di
	Time-dependent 2D integer

	Attributes:
	- array : list of integer2dstruct_arraytemporary_t_2diObj 
	'''

	def __init__(self, base_path_in='integer2d'):
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
		ret = space + 'class integer2dstruct_arraytemporary_t_2di\n'
		for i in range(len(self.array)):
			ret = ret + space + 'integer2dstruct_arraytemporary_t_2di[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(integer2dstruct_arraytemporary_t_2diObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_t_2di, run function putSlice') 
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
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_t_2di, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_t_2di, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_t_2di, run function getSlice') 
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
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_t_2di, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(integer2dstruct_arraytemporary_t_2di(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(integer2dstruct_arraytemporary_t_2di(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = integer2dstruct_arraytemporary_t_2di(self.base_path)
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
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_t_2di, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_t_2di, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_t_2di, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type integer2dstruct_arraytemporary_t_2di, run function getNonTimedElt') 
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


class integer2dstruct_arraytemporary_t_2diObj(KeepInOrder):
	'''
	class integer2dstruct_arraytemporary_t_2diObj
	Time-dependent 2D integer

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 2D with int
	   Value. Time-dependent. Matrix of integers
	'''

	def __init__(self, base_path_in='integer2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0), numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class integer2dstruct_arraytemporary_t_2diObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer2dstruct_arraytemporary_t_2diObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.int32))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer2dstruct_arraytemporary_t_2diObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer2dstruct_arraytemporary_t_2diObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer2dstruct_arraytemporary_t_2diObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class complex2dstruct_arraytemporary_t_2dc:
	'''
	class complex2dstruct_arraytemporary_t_2dc
	Time-dependent 2D complex

	Attributes:
	- array : list of complex2dstruct_arraytemporary_t_2dcObj 
	'''

	def __init__(self, base_path_in='complex2d'):
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
		ret = space + 'class complex2dstruct_arraytemporary_t_2dc\n'
		for i in range(len(self.array)):
			ret = ret + space + 'complex2dstruct_arraytemporary_t_2dc[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(complex2dstruct_arraytemporary_t_2dcObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_t_2dc, run function putSlice') 
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
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_t_2dc, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_t_2dc, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_t_2dc, run function getSlice') 
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
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_t_2dc, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(complex2dstruct_arraytemporary_t_2dc(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(complex2dstruct_arraytemporary_t_2dc(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = complex2dstruct_arraytemporary_t_2dc(self.base_path)
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
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_t_2dc, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_t_2dc, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_t_2dc, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type complex2dstruct_arraytemporary_t_2dc, run function getNonTimedElt') 
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


class complex2dstruct_arraytemporary_t_2dcObj(KeepInOrder):
	'''
	class complex2dstruct_arraytemporary_t_2dcObj
	Time-dependent 2D complex

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 2D with  complex numbers
	   Value. Time-dependent. Matrix of complex numbers
	'''

	def __init__(self, base_path_in='complex2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class complex2dstruct_arraytemporary_t_2dcObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex2dstruct_arraytemporary_t_2dcObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex2dstruct_arraytemporary_t_2dcObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex2dstruct_arraytemporary_t_2dcObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex2dstruct_arraytemporary_t_2dcObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class float3dstruct_arraytemporary_t_3dr:
	'''
	class float3dstruct_arraytemporary_t_3dr
	Time-dependent 3D float

	Attributes:
	- array : list of float3dstruct_arraytemporary_t_3drObj 
	'''

	def __init__(self, base_path_in='float3d'):
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
		ret = space + 'class float3dstruct_arraytemporary_t_3dr\n'
		for i in range(len(self.array)):
			ret = ret + space + 'float3dstruct_arraytemporary_t_3dr[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(float3dstruct_arraytemporary_t_3drObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_t_3dr, run function putSlice') 
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
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_t_3dr, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_t_3dr, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_t_3dr, run function getSlice') 
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
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_t_3dr, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(float3dstruct_arraytemporary_t_3dr(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(float3dstruct_arraytemporary_t_3dr(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = float3dstruct_arraytemporary_t_3dr(self.base_path)
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
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_t_3dr, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_t_3dr, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_t_3dr, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type float3dstruct_arraytemporary_t_3dr, run function getNonTimedElt') 
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


class float3dstruct_arraytemporary_t_3drObj(KeepInOrder):
	'''
	class float3dstruct_arraytemporary_t_3drObj
	Time-dependent 3D float

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 3D with float
	   Value. Time-dependent. array 3D of floats
	'''

	def __init__(self, base_path_in='float3d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class float3dstruct_arraytemporary_t_3drObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float3dstruct_arraytemporary_t_3drObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float3dstruct_arraytemporary_t_3drObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float3dstruct_arraytemporary_t_3drObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float3dstruct_arraytemporary_t_3drObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class integer3dstruct_arraytemporary_t_3di:
	'''
	class integer3dstruct_arraytemporary_t_3di
	Time-dependent 3D integer

	Attributes:
	- array : list of integer3dstruct_arraytemporary_t_3diObj 
	'''

	def __init__(self, base_path_in='integer3d'):
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
		ret = space + 'class integer3dstruct_arraytemporary_t_3di\n'
		for i in range(len(self.array)):
			ret = ret + space + 'integer3dstruct_arraytemporary_t_3di[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(integer3dstruct_arraytemporary_t_3diObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_t_3di, run function putSlice') 
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
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_t_3di, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_t_3di, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_t_3di, run function getSlice') 
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
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_t_3di, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(integer3dstruct_arraytemporary_t_3di(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(integer3dstruct_arraytemporary_t_3di(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = integer3dstruct_arraytemporary_t_3di(self.base_path)
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
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_t_3di, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_t_3di, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_t_3di, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type integer3dstruct_arraytemporary_t_3di, run function getNonTimedElt') 
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


class integer3dstruct_arraytemporary_t_3diObj(KeepInOrder):
	'''
	class integer3dstruct_arraytemporary_t_3diObj
	Time-dependent 3D integer

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 3D with int
	   Value. Time-dependent. array 3D of integers
	'''

	def __init__(self, base_path_in='integer3d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0,0), numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class integer3dstruct_arraytemporary_t_3diObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer3dstruct_arraytemporary_t_3diObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect3DIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DIntInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.int32))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer3dstruct_arraytemporary_t_3diObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect3DIntInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect3DIntFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer3dstruct_arraytemporary_t_3diObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type integer3dstruct_arraytemporary_t_3diObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class complex3dstruct_arraytemporary_t_3dc:
	'''
	class complex3dstruct_arraytemporary_t_3dc
	Time-dependent 3D complex

	Attributes:
	- array : list of complex3dstruct_arraytemporary_t_3dcObj 
	'''

	def __init__(self, base_path_in='complex3d'):
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
		ret = space + 'class complex3dstruct_arraytemporary_t_3dc\n'
		for i in range(len(self.array)):
			ret = ret + space + 'complex3dstruct_arraytemporary_t_3dc[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(complex3dstruct_arraytemporary_t_3dcObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_t_3dc, run function putSlice') 
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
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_t_3dc, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_t_3dc, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_t_3dc, run function getSlice') 
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
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_t_3dc, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(complex3dstruct_arraytemporary_t_3dc(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(complex3dstruct_arraytemporary_t_3dc(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = complex3dstruct_arraytemporary_t_3dc(self.base_path)
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
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_t_3dc, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_t_3dc, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_t_3dc, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type complex3dstruct_arraytemporary_t_3dc, run function getNonTimedElt') 
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


class complex3dstruct_arraytemporary_t_3dcObj(KeepInOrder):
	'''
	class complex3dstruct_arraytemporary_t_3dcObj
	Time-dependent 3D complex

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 3D with  complex numbers
	   Value. Time-dependent. array 3D of complex numbers
	'''

	def __init__(self, base_path_in='complex3d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class complex3dstruct_arraytemporary_t_3dcObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex3dstruct_arraytemporary_t_3dcObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect3DComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DComplexInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex3dstruct_arraytemporary_t_3dcObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect3DComplexInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect3DComplexFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex3dstruct_arraytemporary_t_3dcObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type complex3dstruct_arraytemporary_t_3dcObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


class float4dstruct_arraytemporary_t_4dr:
	'''
	class float4dstruct_arraytemporary_t_4dr
	Time-dependent 4D float

	Attributes:
	- array : list of float4dstruct_arraytemporary_t_4drObj 
	'''

	def __init__(self, base_path_in='float4d'):
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
		ret = space + 'class float4dstruct_arraytemporary_t_4dr\n'
		for i in range(len(self.array)):
			ret = ret + space + 'float4dstruct_arraytemporary_t_4dr[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(float4dstruct_arraytemporary_t_4drObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_t_4dr, run function putSlice') 
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
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_t_4dr, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_t_4dr, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_t_4dr, run function getSlice') 
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
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_t_4dr, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(float4dstruct_arraytemporary_t_4dr(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(float4dstruct_arraytemporary_t_4dr(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = float4dstruct_arraytemporary_t_4dr(self.base_path)
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
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_t_4dr, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_t_4dr, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_t_4dr, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type float4dstruct_arraytemporary_t_4dr, run function getNonTimedElt') 
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


class float4dstruct_arraytemporary_t_4drObj(KeepInOrder):
	'''
	class float4dstruct_arraytemporary_t_4drObj
	Time-dependent 4D float

	Attributes:
	- identifier : class identifierstructureidentifier
	   Identifier.
	- value : numpy.ndarray 4D with float
	   Value. Time-dependent. array 4D of floats
	'''

	def __init__(self, base_path_in='float4d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.identifier = identifierstructureidentifier('identifier')
		self.value = numpy.zeros((0,0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class float4dstruct_arraytemporary_t_4drObj\n'
		ret = ret + space + 'Attribute identifier\n ' + self.identifier.__str__(depth+1)
		s = self.value.__str__()
		ret = ret + space + 'Attribute value\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.identifier.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float4dstruct_arraytemporary_t_4drObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float4dstruct_arraytemporary_t_4drObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float4dstruct_arraytemporary_t_4drObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.identifier.putNonTimedElt(path, cpopath + 'identifier', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type float4dstruct_arraytemporary_t_4drObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.identifier.getNonTimedElt(path, cpopath + 'identifier', i, obj)


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
