# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class waves(KeepInOrder):
	'''
	class waves
	RF wave propagation and deposition. Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- coherentwave : class coherentwavestruct_arraycoherentwave: array of coherentwavestruct_arraycoherentwaveObj objects
	   Wave description for each frequency. Time-dependent. Structure array(nfreq)
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar
	'''

	def __init__(self):
		self.base_path = 'waves'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 8
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.coherentwave = coherentwavestruct_arraycoherentwave('coherentwave')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class waves\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute coherentwave\n ' + self.coherentwave.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.coherentwave.setExpIdx(idx)
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
		self.coherentwave.cpoTime = self.cpoTime
		self.coherentwave.putSlice(path, cpopath)
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
		self.coherentwave.replaceLastSlice(path, cpopath)
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
		self.coherentwave.putNonTimed(path, cpopath)
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
		self.coherentwave.getSlice(path, cpopath, inTime, interpolMode)
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
			coherentwaveList = self.coherentwave.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = waves()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.coherentwave = coherentwaveList[i]
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
		ull.deleteData(self.idx, path, cpopath + 'coherentwave')
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class wavesArray:
	'''
	class wavesArray
	RF wave propagation and deposition. Time-dependent CPO

	Attributes:
	- array : list of waves
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
		ret = space + 'class wavesArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'waves cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = waves()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(waves())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = waves()
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


class coherentwavestruct_arraycoherentwave:
	'''
	class coherentwavestruct_arraycoherentwave
	Wave description for each frequency. Time-dependent. Structure array(nfreq)

	Attributes:
	- array : list of coherentwavestruct_arraycoherentwaveObj 
	'''

	def __init__(self, base_path_in='coherentwave'):
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
		ret = space + 'class coherentwavestruct_arraycoherentwave\n'
		for i in range(len(self.array)):
			ret = ret + space + 'coherentwavestruct_arraycoherentwave[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(coherentwavestruct_arraycoherentwaveObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coherentwavestruct_arraycoherentwave, run function putSlice') 
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
			print ('field '+self.base_path+' of type coherentwavestruct_arraycoherentwave, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type coherentwavestruct_arraycoherentwave, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type coherentwavestruct_arraycoherentwave, run function getSlice') 
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
			print ('field '+self.base_path+' of type coherentwavestruct_arraycoherentwave, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(coherentwavestruct_arraycoherentwave(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(coherentwavestruct_arraycoherentwave(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = coherentwavestruct_arraycoherentwave(self.base_path)
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
			print ('field '+self.base_path+' of type coherentwavestruct_arraycoherentwave, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type coherentwavestruct_arraycoherentwave, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type coherentwavestruct_arraycoherentwave, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type coherentwavestruct_arraycoherentwave, run function getNonTimedElt') 
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


class coherentwavestruct_arraycoherentwaveObj(KeepInOrder):
	'''
	class coherentwavestruct_arraycoherentwaveObj
	Wave description for each frequency. Time-dependent. Structure array(nfreq)

	Attributes:
	- wave_id : class wave_idstructureenum_instance
	   List of identifiers for the coherent-wave, in terms of the type and name of the antenna driving the wave and an index separating waves driven by the same antenna. Possible types: EC/LH/IC (see waves_types in the Documentation website under Conventions/Enumerated_datatypes); the field name should include the name of the antenna as specified in either antennas(*)%ec_antenna%name, antennas(*)%ic_antenna%name, or antennas(*)%lh_antenna%name; the field index should separate different waves generated from a single antenna.
	- composition : class compositionstructurecomposition
	   Plasma composition (description of ion species). OBSOLESCENT.
	- compositions : class compositionsstructurecompositions_type
	   Contains detailed information on the plasma composition (main ions, impurities, neutrals, edge species).
	- global_param : class global_paramstructurewaves_global_param
	   Global wave deposition parameters
	- grid_1d : class grid_1dstructurewaves_grid_1d
	   Grid points for 1D profiles.
	- grid_2d : class grid_2dstructurewaves_grid_2d
	   Grid points for 2D profiles and for full wave solutions.
	- profiles_1d : class profiles_1dstructurewaves_profiles_1d
	   1D radial profiles
	- profiles_2d : class profiles_2dstructurewaves_profiles_2d
	   2D profiles in poloidal cross-section
	- beamtracing : class beamtracingstruct_arraybeamtracing: array of beamtracingstruct_arraybeamtracingObj objects
	   Beam-tracing or ray-tracing solver. Vector(nbeams). Time-dependent
	- fullwave : class fullwavestructurefullwave
	   Solution by full wave code
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='coherentwave'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.wave_id = wave_idstructureenum_instance('wave_id')
		self.composition = compositionstructurecomposition('composition')
		self.compositions = compositionsstructurecompositions_type('compositions')
		self.global_param = global_paramstructurewaves_global_param('global_param')
		self.grid_1d = grid_1dstructurewaves_grid_1d('grid_1d')
		self.grid_2d = grid_2dstructurewaves_grid_2d('grid_2d')
		self.profiles_1d = profiles_1dstructurewaves_profiles_1d('profiles_1d')
		self.profiles_2d = profiles_2dstructurewaves_profiles_2d('profiles_2d')
		self.beamtracing = beamtracingstruct_arraybeamtracing('beamtracing')
		self.fullwave = fullwavestructurefullwave('fullwave')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class coherentwavestruct_arraycoherentwaveObj\n'
		ret = ret + space + 'Attribute wave_id\n ' + self.wave_id.__str__(depth+1)
		ret = ret + space + 'Attribute composition\n ' + self.composition.__str__(depth+1)
		ret = ret + space + 'Attribute compositions\n ' + self.compositions.__str__(depth+1)
		ret = ret + space + 'Attribute global_param\n ' + self.global_param.__str__(depth+1)
		ret = ret + space + 'Attribute grid_1d\n ' + self.grid_1d.__str__(depth+1)
		ret = ret + space + 'Attribute grid_2d\n ' + self.grid_2d.__str__(depth+1)
		ret = ret + space + 'Attribute profiles_1d\n ' + self.profiles_1d.__str__(depth+1)
		ret = ret + space + 'Attribute profiles_2d\n ' + self.profiles_2d.__str__(depth+1)
		ret = ret + space + 'Attribute beamtracing\n ' + self.beamtracing.__str__(depth+1)
		ret = ret + space + 'Attribute fullwave\n ' + self.fullwave.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.wave_id.setExpIdx(idx)
		self.composition.setExpIdx(idx)
		self.compositions.setExpIdx(idx)
		self.global_param.setExpIdx(idx)
		self.grid_1d.setExpIdx(idx)
		self.grid_2d.setExpIdx(idx)
		self.profiles_1d.setExpIdx(idx)
		self.profiles_2d.setExpIdx(idx)
		self.beamtracing.setExpIdx(idx)
		self.fullwave.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coherentwavestruct_arraycoherentwaveObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.global_param.putTimedElt(path, cpopath + 'global_param', i, obj)
		obj = self.grid_1d.putTimedElt(path, cpopath + 'grid_1d', i, obj)
		obj = self.grid_2d.putTimedElt(path, cpopath + 'grid_2d', i, obj)
		obj = self.profiles_1d.putTimedElt(path, cpopath + 'profiles_1d', i, obj)
		obj = self.profiles_2d.putTimedElt(path, cpopath + 'profiles_2d', i, obj)
		obj = self.beamtracing.putTimedElt(path, cpopath + 'beamtracing', i, obj)
		obj = self.fullwave.putTimedElt(path, cpopath + 'fullwave', i, obj)
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coherentwavestruct_arraycoherentwaveObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.global_param.getTimedElt(path, cpopath + 'global_param', i, obj)
		self.grid_1d.getTimedElt(path, cpopath + 'grid_1d', i, obj)
		self.grid_2d.getTimedElt(path, cpopath + 'grid_2d', i, obj)
		self.profiles_1d.getTimedElt(path, cpopath + 'profiles_1d', i, obj)
		self.profiles_2d.getTimedElt(path, cpopath + 'profiles_2d', i, obj)
		self.beamtracing.getTimedElt(path, cpopath + 'beamtracing', i, obj)
		self.fullwave.getTimedElt(path, cpopath + 'fullwave', i, obj)
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coherentwavestruct_arraycoherentwaveObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.wave_id.putNonTimedElt(path, cpopath + 'wave_id', i, obj)
		obj = self.composition.putNonTimedElt(path, cpopath + 'composition', i, obj)
		obj = self.compositions.putNonTimedElt(path, cpopath + 'compositions', i, obj)
		obj = self.global_param.putNonTimedElt(path, cpopath + 'global_param', i, obj)
		obj = self.grid_1d.putNonTimedElt(path, cpopath + 'grid_1d', i, obj)
		obj = self.grid_2d.putNonTimedElt(path, cpopath + 'grid_2d', i, obj)
		obj = self.profiles_1d.putNonTimedElt(path, cpopath + 'profiles_1d', i, obj)
		obj = self.profiles_2d.putNonTimedElt(path, cpopath + 'profiles_2d', i, obj)
		obj = self.beamtracing.putNonTimedElt(path, cpopath + 'beamtracing', i, obj)
		obj = self.fullwave.putNonTimedElt(path, cpopath + 'fullwave', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coherentwavestruct_arraycoherentwaveObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.wave_id.getNonTimedElt(path, cpopath + 'wave_id', i, obj)
		self.composition.getNonTimedElt(path, cpopath + 'composition', i, obj)
		self.compositions.getNonTimedElt(path, cpopath + 'compositions', i, obj)
		self.global_param.getNonTimedElt(path, cpopath + 'global_param', i, obj)
		self.grid_1d.getNonTimedElt(path, cpopath + 'grid_1d', i, obj)
		self.grid_2d.getNonTimedElt(path, cpopath + 'grid_2d', i, obj)
		self.profiles_1d.getNonTimedElt(path, cpopath + 'profiles_1d', i, obj)
		self.profiles_2d.getNonTimedElt(path, cpopath + 'profiles_2d', i, obj)
		self.beamtracing.getNonTimedElt(path, cpopath + 'beamtracing', i, obj)
		self.fullwave.getNonTimedElt(path, cpopath + 'fullwave', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)


class wave_idstructureenum_instance(KeepInOrder):
	'''
	class wave_idstructureenum_instance
	List of identifiers for the coherent-wave, in terms of the type and name of the antenna driving the wave and an index separating waves driven by the same antenna. Possible types: EC/LH/IC (see waves_types in the Documentation website under Conventions/Enumerated_datatypes); the field name should include the name of the antenna as specified in either antennas(*)%ec_antenna%name, antennas(*)%ic_antenna%name, or antennas(*)%lh_antenna%name; the field index should separate different waves generated from a single antenna.

	Attributes:
	- type : class typestructureidentifier
	   Identify the type of the object or process.
	- name : str
	   The name of the object or process. Here the object should be an instans of the type specified in the field type.
	- index : int
	   Index the separating objects or processes with the same name.
	'''

	def __init__(self, base_path_in='wave_id'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.type = typestructureidentifier('type')
		self.name = ''
		self.index = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class wave_idstructureenum_instance\n'
		ret = ret + space + 'Attribute type\n ' + self.type.__str__(depth+1)
		ret = ret + space + 'Attribute name: ' + str(self.name) + '\n'
		ret = ret + space + 'Attribute index: ' + str(self.index) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.type.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wave_idstructureenum_instance, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.cpoTime = self.cpoTime
		self.type.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wave_idstructureenum_instance, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wave_idstructureenum_instance, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.putNonTimed(path, cpopath)
		status = ull.putString(self.idx, path, cpopath + 'name', self.name)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'index', self.index)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type wave_idstructureenum_instance, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_name = ull.getString(self.idx, path, cpopath + 'name')
		check_status(status)
		if not status:
			self.name = ret_name
		status, ret_index = ull.getInt(self.idx, path, cpopath + 'index')
		check_status(status)
		if not status:
			self.index = ret_index

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type wave_idstructureenum_instance, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			typeList = self.type.build_non_resampled_data(path, cpopath, nbslice)
			status, nameVal = ull.getString(self.idx, path, cpopath + 'name')
			check_status(status)
			status, indexVal = ull.getInt(self.idx, path, cpopath + 'index')
			check_status(status)
			for i in range(nbslice):
				slice = wave_idstructureenum_instance(self.base_path)
				slice.setExpIdx(self.idx)
				slice.type = typeList[i]
				slice.name = nameVal
				slice.index = indexVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wave_idstructureenum_instanceObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wave_idstructureenum_instanceObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wave_idstructureenum_instanceObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.type.putNonTimedElt(path, cpopath + 'type', i, obj)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'index') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'index', i, self.index)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wave_idstructureenum_instanceObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.type.getNonTimedElt(path, cpopath + 'type', i, obj)
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'index') 
			print ('obj = ' + str(obj))
		status, ret_index = ull.getIntFromObject(self.idx, obj, cpopath + 'index', i)
		check_status(status)
		if not status:
			self.index = ret_index

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'name')
		ull.deleteData(self.idx, path, cpopath + 'index')


class typestructureidentifier(KeepInOrder):
	'''
	class typestructureidentifier
	Identify the type of the object or process.

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='type'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class typestructureidentifier\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type typestructureidentifier, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type typestructureidentifier, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type typestructureidentifier, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type typestructureidentifier, run function getSlice') 
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
			print ('field '+self.base_path+' of type typestructureidentifier, run function build_non_resampled_data') 
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
				slice = typestructureidentifier(self.base_path)
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
			print ('object of type typestructureidentifierObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type typestructureidentifierObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type typestructureidentifierObj, run function putNonTimedElt') 
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
			print ('object of type typestructureidentifierObj, run function getNonTimedElt') 
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


class compositionstructurecomposition(KeepInOrder):
	'''
	class compositionstructurecomposition
	Plasma composition (description of ion species). OBSOLESCENT.

	Attributes:
	- amn : numpy.ndarray 1D with float
	   Atomic mass number (lumped ions are allowed); Vector (nion)
	- zn : numpy.ndarray 1D with float
	   Nuclear charge (lumped ions are allowed); Vector (nion)
	- zion : numpy.ndarray 1D with float
	   Ion charge (of the dominant ionisation state; lumped ions are allowed); Vector (nion)
	- imp_flag : numpy.ndarray 1D with int)
	   Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge state are considered and are described in impurity CPO; Vector (nion)
	- label : list of str
	   Label for the ions - note the charge state is not included; String Vector (nion)
	'''

	def __init__(self, base_path_in='composition'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.amn = numpy.zeros(0, numpy.float64, order='C')
		self.zn = numpy.zeros(0, numpy.float64, order='C')
		self.zion = numpy.zeros(0, numpy.float64, order='C')
		self.imp_flag = numpy.zeros(0, numpy.int32, order='C')
		self.label = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class compositionstructurecomposition\n'
		s = self.amn.__str__()
		ret = ret + space + 'Attribute amn\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.zn.__str__()
		ret = ret + space + 'Attribute zn\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.zion.__str__()
		ret = ret + space + 'Attribute zion\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.imp_flag.__str__()
		ret = ret + space + 'Attribute imp_flag\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.label.__str__()
		ret = ret + space + 'Attribute label\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructurecomposition, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructurecomposition, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructurecomposition, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'amn', numpy.array(self.amn).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'zn', numpy.array(self.zn).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'zion', numpy.array(self.zion).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'imp_flag', numpy.array(self.imp_flag).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'label', self.label, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructurecomposition, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_amn = ull.getVect1DDouble(self.idx, path, cpopath + 'amn')
		check_status(status)
		if not status:
			self.amn = ret_amn
		status, ret_zn = ull.getVect1DDouble(self.idx, path, cpopath + 'zn')
		check_status(status)
		if not status:
			self.zn = ret_zn
		status, ret_zion = ull.getVect1DDouble(self.idx, path, cpopath + 'zion')
		check_status(status)
		if not status:
			self.zion = ret_zion
		status, ret_imp_flag = ull.getVect1DInt(self.idx, path, cpopath + 'imp_flag')
		check_status(status)
		if not status:
			self.imp_flag = ret_imp_flag
		status, ret_label = ull.getVect1DString(self.idx, path, cpopath + 'label')
		check_status(status)
		if not status:
			self.label = ret_label

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type compositionstructurecomposition, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, amnVal = ull.getVect1DDouble(self.idx, path, cpopath + 'amn')
			check_status(status)
			status, znVal = ull.getVect1DDouble(self.idx, path, cpopath + 'zn')
			check_status(status)
			status, zionVal = ull.getVect1DDouble(self.idx, path, cpopath + 'zion')
			check_status(status)
			status, imp_flagVal = ull.getVect1DInt(self.idx, path, cpopath + 'imp_flag')
			check_status(status)
			status, labelVal = ull.getVect1DString(self.idx, path, cpopath + 'label')
			check_status(status)
			for i in range(nbslice):
				slice = compositionstructurecomposition(self.base_path)
				slice.setExpIdx(self.idx)
				slice.amn = amnVal
				slice.zn = znVal
				slice.zion = zionVal
				slice.imp_flag = imp_flagVal
				slice.label = labelVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionstructurecompositionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionstructurecompositionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionstructurecompositionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'amn', i, numpy.array(self.amn).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'zn', i, numpy.array(self.zn).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'zion') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'zion', i, numpy.array(self.zion).astype(numpy.float64))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'imp_flag') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'imp_flag', i, numpy.array(self.imp_flag).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionstructurecompositionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		status, ret_amn = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'amn', i)
		check_status(status)
		if not status:
			self.amn = ret_amn
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		status, ret_zn = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'zn', i)
		check_status(status)
		if not status:
			self.zn = ret_zn
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'zion') 
			print ('obj = ' + str(obj))
		status, ret_zion = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'zion', i)
		check_status(status)
		if not status:
			self.zion = ret_zion
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'imp_flag') 
			print ('obj = ' + str(obj))
		status, ret_imp_flag = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'imp_flag', i)
		check_status(status)
		if not status:
			self.imp_flag = ret_imp_flag
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'amn')
		ull.deleteData(self.idx, path, cpopath + 'zn')
		ull.deleteData(self.idx, path, cpopath + 'zion')
		ull.deleteData(self.idx, path, cpopath + 'imp_flag')
		ull.deleteData(self.idx, path, cpopath + 'label')


class compositionsstructurecompositions_type(KeepInOrder):
	'''
	class compositionsstructurecompositions_type
	Contains detailed information on the plasma composition (main ions, impurities, neutrals, edge species).

	Attributes:
	- nuclei : class nucleistruct_arraynuclei: array of nucleistruct_arraynucleiObj objects
	   Array of nuclei considered.
	- ions : class ionsstruct_arrayions: array of ionsstruct_arrayionsObj objects
	   Array of main plasma ions.
	- impurities : class impuritiesstruct_arrayimpurities: array of impuritiesstruct_arrayimpuritiesObj objects
	   Array of impurities.
	- neutralscomp : class neutralscompstruct_arraycomposition_neutralscomp: array of neutralscompstruct_arraycomposition_neutralscompObj objects
	   Array of neutrals.
	- edgespecies : class edgespeciesstruct_arrayedgespecies: array of edgespeciesstruct_arrayedgespeciesObj objects
	   Array of edge species.
	- signature : class signaturestructureidentifier
	   Identifier for species choices. The goal of this is to uniquely capture the species blocks so that if the signatures are the same then the species blocks will also be the same.
	'''

	def __init__(self, base_path_in='compositions'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nuclei = nucleistruct_arraynuclei('nuclei')
		self.ions = ionsstruct_arrayions('ions')
		self.impurities = impuritiesstruct_arrayimpurities('impurities')
		self.neutralscomp = neutralscompstruct_arraycomposition_neutralscomp('neutralscomp')
		self.edgespecies = edgespeciesstruct_arrayedgespecies('edgespecies')
		self.signature = signaturestructureidentifier('signature')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class compositionsstructurecompositions_type\n'
		ret = ret + space + 'Attribute nuclei\n ' + self.nuclei.__str__(depth+1)
		ret = ret + space + 'Attribute ions\n ' + self.ions.__str__(depth+1)
		ret = ret + space + 'Attribute impurities\n ' + self.impurities.__str__(depth+1)
		ret = ret + space + 'Attribute neutralscomp\n ' + self.neutralscomp.__str__(depth+1)
		ret = ret + space + 'Attribute edgespecies\n ' + self.edgespecies.__str__(depth+1)
		ret = ret + space + 'Attribute signature\n ' + self.signature.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.nuclei.setExpIdx(idx)
		self.ions.setExpIdx(idx)
		self.impurities.setExpIdx(idx)
		self.neutralscomp.setExpIdx(idx)
		self.edgespecies.setExpIdx(idx)
		self.signature.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionsstructurecompositions_type, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.nuclei.cpoTime = self.cpoTime
		self.nuclei.putSlice(path, cpopath)
		self.ions.cpoTime = self.cpoTime
		self.ions.putSlice(path, cpopath)
		self.impurities.cpoTime = self.cpoTime
		self.impurities.putSlice(path, cpopath)
		self.neutralscomp.cpoTime = self.cpoTime
		self.neutralscomp.putSlice(path, cpopath)
		self.edgespecies.cpoTime = self.cpoTime
		self.edgespecies.putSlice(path, cpopath)
		self.signature.cpoTime = self.cpoTime
		self.signature.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionsstructurecompositions_type, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.nuclei.replaceLastSlice(path, cpopath)
		self.ions.replaceLastSlice(path, cpopath)
		self.impurities.replaceLastSlice(path, cpopath)
		self.neutralscomp.replaceLastSlice(path, cpopath)
		self.edgespecies.replaceLastSlice(path, cpopath)
		self.signature.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compositionsstructurecompositions_type, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.nuclei.putNonTimed(path, cpopath)
		self.ions.putNonTimed(path, cpopath)
		self.impurities.putNonTimed(path, cpopath)
		self.neutralscomp.putNonTimed(path, cpopath)
		self.edgespecies.putNonTimed(path, cpopath)
		self.signature.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type compositionsstructurecompositions_type, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.nuclei.getSlice(path, cpopath, inTime, interpolMode)
		self.ions.getSlice(path, cpopath, inTime, interpolMode)
		self.impurities.getSlice(path, cpopath, inTime, interpolMode)
		self.neutralscomp.getSlice(path, cpopath, inTime, interpolMode)
		self.edgespecies.getSlice(path, cpopath, inTime, interpolMode)
		self.signature.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type compositionsstructurecompositions_type, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			nucleiList = self.nuclei.build_non_resampled_data(path, cpopath, nbslice)
			ionsList = self.ions.build_non_resampled_data(path, cpopath, nbslice)
			impuritiesList = self.impurities.build_non_resampled_data(path, cpopath, nbslice)
			neutralscompList = self.neutralscomp.build_non_resampled_data(path, cpopath, nbslice)
			edgespeciesList = self.edgespecies.build_non_resampled_data(path, cpopath, nbslice)
			signatureList = self.signature.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = compositionsstructurecompositions_type(self.base_path)
				slice.setExpIdx(self.idx)
				slice.nuclei = nucleiList[i]
				slice.ions = ionsList[i]
				slice.impurities = impuritiesList[i]
				slice.neutralscomp = neutralscompList[i]
				slice.edgespecies = edgespeciesList[i]
				slice.signature = signatureList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionsstructurecompositions_typeObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionsstructurecompositions_typeObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionsstructurecompositions_typeObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.nuclei.putNonTimedElt(path, cpopath + 'nuclei', i, obj)
		obj = self.ions.putNonTimedElt(path, cpopath + 'ions', i, obj)
		obj = self.impurities.putNonTimedElt(path, cpopath + 'impurities', i, obj)
		obj = self.neutralscomp.putNonTimedElt(path, cpopath + 'neutralscomp', i, obj)
		obj = self.edgespecies.putNonTimedElt(path, cpopath + 'edgespecies', i, obj)
		obj = self.signature.putNonTimedElt(path, cpopath + 'signature', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compositionsstructurecompositions_typeObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.nuclei.getNonTimedElt(path, cpopath + 'nuclei', i, obj)
		self.ions.getNonTimedElt(path, cpopath + 'ions', i, obj)
		self.impurities.getNonTimedElt(path, cpopath + 'impurities', i, obj)
		self.neutralscomp.getNonTimedElt(path, cpopath + 'neutralscomp', i, obj)
		self.edgespecies.getNonTimedElt(path, cpopath + 'edgespecies', i, obj)
		self.signature.getNonTimedElt(path, cpopath + 'signature', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'nuclei')
		ull.deleteData(self.idx, path, cpopath + 'ions')
		ull.deleteData(self.idx, path, cpopath + 'impurities')
		ull.deleteData(self.idx, path, cpopath + 'neutralscomp')
		ull.deleteData(self.idx, path, cpopath + 'edgespecies')
		self.signature.deleteData(path, cpopath)


class nucleistruct_arraynuclei:
	'''
	class nucleistruct_arraynuclei
	Array of nuclei considered.

	Attributes:
	- array : list of nucleistruct_arraynucleiObj 
	'''

	def __init__(self, base_path_in='nuclei'):
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
		ret = space + 'class nucleistruct_arraynuclei\n'
		for i in range(len(self.array)):
			ret = ret + space + 'nucleistruct_arraynuclei[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(nucleistruct_arraynucleiObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function getSlice') 
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
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(nucleistruct_arraynuclei(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = nucleistruct_arraynuclei(self.base_path)
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
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type nucleistruct_arraynuclei, run function getNonTimedElt') 
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


class nucleistruct_arraynucleiObj(KeepInOrder):
	'''
	class nucleistruct_arraynucleiObj
	Array of nuclei considered.

	Attributes:
	- zn : float
	   Nuclear charge [units of elementary charge];
	- amn : float
	   Mass of atom [amu]
	- label : str
	   String identifying element (e.g. H, D, T, He, C, ...)
	'''

	def __init__(self, base_path_in='nuclei'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.zn = EMPTY_DOUBLE
		self.amn = EMPTY_DOUBLE
		self.label = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nucleistruct_arraynucleiObj\n'
		ret = ret + space + 'Attribute zn: ' + str(self.zn) + '\n'
		ret = ret + space + 'Attribute amn: ' + str(self.amn) + '\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nucleistruct_arraynucleiObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zn', i, self.zn)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'amn', i, self.amn)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nucleistruct_arraynucleiObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		status, ret_zn = ull.getDoubleFromObject(self.idx, obj, cpopath + 'zn', i)
		check_status(status)
		if not status:
			self.zn = ret_zn
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		status, ret_amn = ull.getDoubleFromObject(self.idx, obj, cpopath + 'amn', i)
		check_status(status)
		if not status:
			self.amn = ret_amn
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label


class ionsstruct_arrayions:
	'''
	class ionsstruct_arrayions
	Array of main plasma ions.

	Attributes:
	- array : list of ionsstruct_arrayionsObj 
	'''

	def __init__(self, base_path_in='ions'):
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
		ret = space + 'class ionsstruct_arrayions\n'
		for i in range(len(self.array)):
			ret = ret + space + 'ionsstruct_arrayions[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(ionsstruct_arrayionsObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function getSlice') 
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
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(ionsstruct_arrayions(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = ionsstruct_arrayions(self.base_path)
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
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type ionsstruct_arrayions, run function getNonTimedElt') 
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


class ionsstruct_arrayionsObj(KeepInOrder):
	'''
	class ionsstruct_arrayionsObj
	Array of main plasma ions.

	Attributes:
	- nucindex : int
	   Index into list of nuclei; int
	- zion : float
	   Ion charge (of the dominant ionisation state; lumped ions are allowed); Vector (nion)
	- imp_flag : int
	   Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge state are considered and are described in impurity CPO; Vector (nion)
	- label : str
	   String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
	'''

	def __init__(self, base_path_in='ions'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nucindex = EMPTY_INT
		self.zion = EMPTY_DOUBLE
		self.imp_flag = EMPTY_INT
		self.label = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class ionsstruct_arrayionsObj\n'
		ret = ret + space + 'Attribute nucindex: ' + str(self.nucindex) + '\n'
		ret = ret + space + 'Attribute zion: ' + str(self.zion) + '\n'
		ret = ret + space + 'Attribute imp_flag: ' + str(self.imp_flag) + '\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ionsstruct_arrayionsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nucindex', i, self.nucindex)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zion') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zion', i, self.zion)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'imp_flag') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'imp_flag', i, self.imp_flag)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ionsstruct_arrayionsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		status, ret_nucindex = ull.getIntFromObject(self.idx, obj, cpopath + 'nucindex', i)
		check_status(status)
		if not status:
			self.nucindex = ret_nucindex
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'zion') 
			print ('obj = ' + str(obj))
		status, ret_zion = ull.getDoubleFromObject(self.idx, obj, cpopath + 'zion', i)
		check_status(status)
		if not status:
			self.zion = ret_zion
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'imp_flag') 
			print ('obj = ' + str(obj))
		status, ret_imp_flag = ull.getIntFromObject(self.idx, obj, cpopath + 'imp_flag', i)
		check_status(status)
		if not status:
			self.imp_flag = ret_imp_flag
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label


class impuritiesstruct_arrayimpurities:
	'''
	class impuritiesstruct_arrayimpurities
	Array of impurities.

	Attributes:
	- array : list of impuritiesstruct_arrayimpuritiesObj 
	'''

	def __init__(self, base_path_in='impurities'):
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
		ret = space + 'class impuritiesstruct_arrayimpurities\n'
		for i in range(len(self.array)):
			ret = ret + space + 'impuritiesstruct_arrayimpurities[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(impuritiesstruct_arrayimpuritiesObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function getSlice') 
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
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(impuritiesstruct_arrayimpurities(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = impuritiesstruct_arrayimpurities(self.base_path)
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
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type impuritiesstruct_arrayimpurities, run function getNonTimedElt') 
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


class impuritiesstruct_arrayimpuritiesObj(KeepInOrder):
	'''
	class impuritiesstruct_arrayimpuritiesObj
	Array of impurities.

	Attributes:
	- nucindex : int
	   Index into list of nuclei; int
	- i_ion : int
	   Index of the impurity species in the ions array of structures. Vector (nimp)
	- nzimp : int
	   Number of charge states (or bundles) considered for this impurity species.
	- zmin : numpy.ndarray 1D with float
	   Minimum Z of impurity ionisation state bundle. Vector (nzimp)
	- zmax : numpy.ndarray 1D with float
	   Maximum Z of impurity ionisation state bundle. If no bundle, zmax=zmin. Vector (nzimp)
	- label : list of str
	   String array (nzimp) identifying impurities (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
	'''

	def __init__(self, base_path_in='impurities'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nucindex = EMPTY_INT
		self.i_ion = EMPTY_INT
		self.nzimp = EMPTY_INT
		self.zmin = numpy.zeros(0, numpy.float64, order='C')
		self.zmax = numpy.zeros(0, numpy.float64, order='C')
		self.label = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class impuritiesstruct_arrayimpuritiesObj\n'
		ret = ret + space + 'Attribute nucindex: ' + str(self.nucindex) + '\n'
		ret = ret + space + 'Attribute i_ion: ' + str(self.i_ion) + '\n'
		ret = ret + space + 'Attribute nzimp: ' + str(self.nzimp) + '\n'
		s = self.zmin.__str__()
		ret = ret + space + 'Attribute zmin\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.zmax.__str__()
		ret = ret + space + 'Attribute zmax\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.label.__str__()
		ret = ret + space + 'Attribute label\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type impuritiesstruct_arrayimpuritiesObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nucindex', i, self.nucindex)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'i_ion') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'i_ion', i, self.i_ion)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nzimp') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nzimp', i, self.nzimp)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'zmin') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'zmin', i, numpy.array(self.zmin).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'zmax') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'zmax', i, numpy.array(self.zmax).astype(numpy.float64))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type impuritiesstruct_arrayimpuritiesObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		status, ret_nucindex = ull.getIntFromObject(self.idx, obj, cpopath + 'nucindex', i)
		check_status(status)
		if not status:
			self.nucindex = ret_nucindex
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'i_ion') 
			print ('obj = ' + str(obj))
		status, ret_i_ion = ull.getIntFromObject(self.idx, obj, cpopath + 'i_ion', i)
		check_status(status)
		if not status:
			self.i_ion = ret_i_ion
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nzimp') 
			print ('obj = ' + str(obj))
		status, ret_nzimp = ull.getIntFromObject(self.idx, obj, cpopath + 'nzimp', i)
		check_status(status)
		if not status:
			self.nzimp = ret_nzimp
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'zmin') 
			print ('obj = ' + str(obj))
		status, ret_zmin = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'zmin', i)
		check_status(status)
		if not status:
			self.zmin = ret_zmin
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'zmax') 
			print ('obj = ' + str(obj))
		status, ret_zmax = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'zmax', i)
		check_status(status)
		if not status:
			self.zmax = ret_zmax
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label


class neutralscompstruct_arraycomposition_neutralscomp:
	'''
	class neutralscompstruct_arraycomposition_neutralscomp
	Array of neutrals.

	Attributes:
	- array : list of neutralscompstruct_arraycomposition_neutralscompObj 
	'''

	def __init__(self, base_path_in='neutralscomp'):
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
		ret = space + 'class neutralscompstruct_arraycomposition_neutralscomp\n'
		for i in range(len(self.array)):
			ret = ret + space + 'neutralscompstruct_arraycomposition_neutralscomp[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(neutralscompstruct_arraycomposition_neutralscompObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function getSlice') 
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
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(neutralscompstruct_arraycomposition_neutralscomp(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = neutralscompstruct_arraycomposition_neutralscomp(self.base_path)
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
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type neutralscompstruct_arraycomposition_neutralscomp, run function getNonTimedElt') 
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


class neutralscompstruct_arraycomposition_neutralscompObj(KeepInOrder):
	'''
	class neutralscompstruct_arraycomposition_neutralscompObj
	Array of neutrals.

	Attributes:
	- neutcomp : class neutcompstruct_arraycomposition_neutrals_neutcomp: array of neutcompstruct_arraycomposition_neutrals_neutcompObj objects
	   Array of components to the atom or molecule. Vector (ncomp)
	- type : class typestruct_arrayidentifier: array of typestruct_arrayidentifierObj objects
	   Type of neutral, in terms of energy : 0=cold, 1=thermal, 2= fast, 3=NBI. Vector (ntype) of identifiers
	- label : str
	   String identifying the atom or molecule (e.g. D2, DT, CD4, ...)
	'''

	def __init__(self, base_path_in='neutralscomp'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.neutcomp = neutcompstruct_arraycomposition_neutrals_neutcomp('neutcomp')
		self.type = typestruct_arrayidentifier('type')
		self.label = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class neutralscompstruct_arraycomposition_neutralscompObj\n'
		ret = ret + space + 'Attribute neutcomp\n ' + self.neutcomp.__str__(depth+1)
		ret = ret + space + 'Attribute type\n ' + self.type.__str__(depth+1)
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.neutcomp.setExpIdx(idx)
		self.type.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neutralscompstruct_arraycomposition_neutralscompObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.neutcomp.putNonTimedElt(path, cpopath + 'neutcomp', i, obj)
		obj = self.type.putNonTimedElt(path, cpopath + 'type', i, obj)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neutralscompstruct_arraycomposition_neutralscompObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.neutcomp.getNonTimedElt(path, cpopath + 'neutcomp', i, obj)
		self.type.getNonTimedElt(path, cpopath + 'type', i, obj)
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label


class neutcompstruct_arraycomposition_neutrals_neutcomp:
	'''
	class neutcompstruct_arraycomposition_neutrals_neutcomp
	Array of components to the atom or molecule. Vector (ncomp)

	Attributes:
	- array : list of neutcompstruct_arraycomposition_neutrals_neutcompObj 
	'''

	def __init__(self, base_path_in='neutcomp'):
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
		ret = space + 'class neutcompstruct_arraycomposition_neutrals_neutcomp\n'
		for i in range(len(self.array)):
			ret = ret + space + 'neutcompstruct_arraycomposition_neutrals_neutcomp[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(neutcompstruct_arraycomposition_neutrals_neutcompObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function getSlice') 
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
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(neutcompstruct_arraycomposition_neutrals_neutcomp(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = neutcompstruct_arraycomposition_neutrals_neutcomp(self.base_path)
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
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type neutcompstruct_arraycomposition_neutrals_neutcomp, run function getNonTimedElt') 
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


class neutcompstruct_arraycomposition_neutrals_neutcompObj(KeepInOrder):
	'''
	class neutcompstruct_arraycomposition_neutrals_neutcompObj
	Array of components to the atom or molecule. Vector (ncomp)

	Attributes:
	- nucindex : int
	   Index into list of nuclei; int
	- multiplicity : int
	   Multiplicity of the atom; int
	'''

	def __init__(self, base_path_in='neutcomp'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nucindex = EMPTY_INT
		self.multiplicity = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class neutcompstruct_arraycomposition_neutrals_neutcompObj\n'
		ret = ret + space + 'Attribute nucindex: ' + str(self.nucindex) + '\n'
		ret = ret + space + 'Attribute multiplicity: ' + str(self.multiplicity) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neutcompstruct_arraycomposition_neutrals_neutcompObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nucindex', i, self.nucindex)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'multiplicity') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'multiplicity', i, self.multiplicity)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type neutcompstruct_arraycomposition_neutrals_neutcompObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		status, ret_nucindex = ull.getIntFromObject(self.idx, obj, cpopath + 'nucindex', i)
		check_status(status)
		if not status:
			self.nucindex = ret_nucindex
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'multiplicity') 
			print ('obj = ' + str(obj))
		status, ret_multiplicity = ull.getIntFromObject(self.idx, obj, cpopath + 'multiplicity', i)
		check_status(status)
		if not status:
			self.multiplicity = ret_multiplicity


class typestruct_arrayidentifier:
	'''
	class typestruct_arrayidentifier
	Type of neutral, in terms of energy : 0=cold, 1=thermal, 2= fast, 3=NBI. Vector (ntype) of identifiers

	Attributes:
	- array : list of typestruct_arrayidentifierObj 
	'''

	def __init__(self, base_path_in='type'):
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
		ret = space + 'class typestruct_arrayidentifier\n'
		for i in range(len(self.array)):
			ret = ret + space + 'typestruct_arrayidentifier[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(typestruct_arrayidentifierObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function getSlice') 
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
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(typestruct_arrayidentifier(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = typestruct_arrayidentifier(self.base_path)
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
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type typestruct_arrayidentifier, run function getNonTimedElt') 
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


class typestruct_arrayidentifierObj(KeepInOrder):
	'''
	class typestruct_arrayidentifierObj
	Type of neutral, in terms of energy : 0=cold, 1=thermal, 2= fast, 3=NBI. Vector (ntype) of identifiers

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='type'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class typestruct_arrayidentifierObj\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type typestruct_arrayidentifierObj, run function putNonTimedElt') 
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
			print ('object of type typestruct_arrayidentifierObj, run function getNonTimedElt') 
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


class edgespeciesstruct_arrayedgespecies:
	'''
	class edgespeciesstruct_arrayedgespecies
	Array of edge species.

	Attributes:
	- array : list of edgespeciesstruct_arrayedgespeciesObj 
	'''

	def __init__(self, base_path_in='edgespecies'):
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
		ret = space + 'class edgespeciesstruct_arrayedgespecies\n'
		for i in range(len(self.array)):
			ret = ret + space + 'edgespeciesstruct_arrayedgespecies[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(edgespeciesstruct_arrayedgespeciesObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function getSlice') 
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
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(edgespeciesstruct_arrayedgespecies(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = edgespeciesstruct_arrayedgespecies(self.base_path)
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
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type edgespeciesstruct_arrayedgespecies, run function getNonTimedElt') 
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


class edgespeciesstruct_arrayedgespeciesObj(KeepInOrder):
	'''
	class edgespeciesstruct_arrayedgespeciesObj
	Array of edge species.

	Attributes:
	- nucindex : int
	   Index into list of nuclei; int
	- zmin : float
	   Minimum Z of species charge state bundle
	- zmax : float
	   Maximum Z of species charge state bundle
	- label : str
	   String identifying the species (e.g. D0, D+, C0, C+, C+2, ...)
	'''

	def __init__(self, base_path_in='edgespecies'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.nucindex = EMPTY_INT
		self.zmin = EMPTY_DOUBLE
		self.zmax = EMPTY_DOUBLE
		self.label = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class edgespeciesstruct_arrayedgespeciesObj\n'
		ret = ret + space + 'Attribute nucindex: ' + str(self.nucindex) + '\n'
		ret = ret + space + 'Attribute zmin: ' + str(self.zmin) + '\n'
		ret = ret + space + 'Attribute zmax: ' + str(self.zmax) + '\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type edgespeciesstruct_arrayedgespeciesObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'nucindex', i, self.nucindex)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zmin') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zmin', i, self.zmin)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zmax') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zmax', i, self.zmax)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type edgespeciesstruct_arrayedgespeciesObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'nucindex') 
			print ('obj = ' + str(obj))
		status, ret_nucindex = ull.getIntFromObject(self.idx, obj, cpopath + 'nucindex', i)
		check_status(status)
		if not status:
			self.nucindex = ret_nucindex
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'zmin') 
			print ('obj = ' + str(obj))
		status, ret_zmin = ull.getDoubleFromObject(self.idx, obj, cpopath + 'zmin', i)
		check_status(status)
		if not status:
			self.zmin = ret_zmin
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'zmax') 
			print ('obj = ' + str(obj))
		status, ret_zmax = ull.getDoubleFromObject(self.idx, obj, cpopath + 'zmax', i)
		check_status(status)
		if not status:
			self.zmax = ret_zmax
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label


class signaturestructureidentifier(KeepInOrder):
	'''
	class signaturestructureidentifier
	Identifier for species choices. The goal of this is to uniquely capture the species blocks so that if the signatures are the same then the species blocks will also be the same.

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='signature'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class signaturestructureidentifier\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type signaturestructureidentifier, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type signaturestructureidentifier, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type signaturestructureidentifier, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type signaturestructureidentifier, run function getSlice') 
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
			print ('field '+self.base_path+' of type signaturestructureidentifier, run function build_non_resampled_data') 
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
				slice = signaturestructureidentifier(self.base_path)
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
			print ('object of type signaturestructureidentifierObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type signaturestructureidentifierObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type signaturestructureidentifierObj, run function putNonTimedElt') 
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
			print ('object of type signaturestructureidentifierObj, run function getNonTimedElt') 
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


class global_paramstructurewaves_global_param(KeepInOrder):
	'''
	class global_paramstructurewaves_global_param
	Global wave deposition parameters

	Attributes:
	- name : str
	   Antenna name, String
	- type : str
	   Wave type (LH, EC, IC, ...), String
	- f_assumption : numpy.ndarray 1D with int)
	   Assumption on the functions distribution used by the wave solver to calculate the power deposition : 0 = Maxwellian (linear absorption); 1 = quasi-linear (F given by a distribution function CPO). Integer vector (nion+1). The first value corresponds to the electrons, then to the other ion species. Time-dependent.
	- code_type : int
	   Type of wave deposition code for a given frequency: 1=beam/ray tracing; 2=full wave; Integer
	- frequency : float
	   Wave frequency [Hz]; Time-dependent, floating
	- ntor : numpy.ndarray 1D with int)
	   Toroidal mode numbers; Time-dependent; Vector (ntor)
	- power_tot : float
	   Total absorbed wave power [W]; Time-dependent
	- p_frac_ntor : numpy.ndarray 1D with float
	   Fraction of wave power per toroidal mode number; Time-dependent; Vector (ntor)
	- pow_e : float
	   Wave power absorbed by the thermal electrons [W]; Time-dependent; Float
	- pow_i : numpy.ndarray 1D with float
	   Wave power absorbed by the thermal ion species [W]; Time-dependent; Vector (nion)
	- pow_z : numpy.ndarray 2D with float
	   Wave power absorbed by the thermal impurity species [W]; Time-dependent; Vector (nimpur, nzimp)
	- pow_fe : float
	   Wave power absorbed by the fast electrons [W]; Time-dependent; Float
	- pow_fi : numpy.ndarray 1D with float
	   Wave power absorbed by the fast ion species [W]; Time-dependent; Vector (nion)
	- pow_fz : numpy.ndarray 2D with float
	   Wave power absorbed by the fast impurity species [W]; Time-dependent; Vector (nimpur, nzimp)
	- pow_ntor_e : numpy.ndarray 1D with float
	   Wave power absorbed by the thermal electrons for each toroidal mode [W]; Time-dependent; Vector (ntor)
	- pow_ntor_i : numpy.ndarray 2D with float
	   Wave power absorbed by an the thermal ion species for each toroidal mode [W]; Time-dependent; Matrix (ntor, nion)
	- pow_ntor_z : numpy.ndarray 3D with float
	   Wave power absorbed by an the thermal impurity species for each toroidal mode [W]; Time-dependent; Matrix (ntor, nimpur, nzimp)
	- pow_ntor_fe : numpy.ndarray 1D with float
	   Wave power absorbed by the fast electrons for each toroidal mode [W]; Time-dependent; Vector (ntor)
	- pow_ntor_fi : numpy.ndarray 2D with float
	   Wave power absorbed by an the fast ion species for each toroidal mode [W]; Time-dependent; Matrix (ntor, nion)
	- pow_ntor_fz : numpy.ndarray 3D with float
	   Wave power absorbed by an the fast impurity species for each toroidal mode [W]; Time-dependent; Matrix (ntor, nimpur, nzimp)
	- cur_tor : float
	   Wave driven toroidal current from a stand alone calculation (not consistent with other sources) [A]; Time-dependent, Float
	- cur_tor_ntor : numpy.ndarray 1D with float
	   Wave driven toroidal current for each toroidal mode number from a stand alone calculation (not consistent with other sources)  [A]; Time-dependent; Vector (ntor)
	- mag_axis : class mag_axisstructurerz0D
	   Position of the magnetic axis. Time-dependent; Scalar
	- toroid_field : class toroid_fieldstructureb0r0
	   Characteristics of the vacuum toroidal field (used to define the rho_tor coordinate and the normalisation of parallel current densities).
	'''

	def __init__(self, base_path_in='global_param'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.name = ''
		self.type = ''
		self.f_assumption = numpy.zeros(0, numpy.int32, order='C')
		self.code_type = EMPTY_INT
		self.frequency = EMPTY_DOUBLE
		self.ntor = numpy.zeros(0, numpy.int32, order='C')
		self.power_tot = EMPTY_DOUBLE
		self.p_frac_ntor = numpy.zeros(0, numpy.float64, order='C')
		self.pow_e = EMPTY_DOUBLE
		self.pow_i = numpy.zeros(0, numpy.float64, order='C')
		self.pow_z = numpy.zeros((0,0), numpy.float64, order='C')
		self.pow_fe = EMPTY_DOUBLE
		self.pow_fi = numpy.zeros(0, numpy.float64, order='C')
		self.pow_fz = numpy.zeros((0,0), numpy.float64, order='C')
		self.pow_ntor_e = numpy.zeros(0, numpy.float64, order='C')
		self.pow_ntor_i = numpy.zeros((0,0), numpy.float64, order='C')
		self.pow_ntor_z = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.pow_ntor_fe = numpy.zeros(0, numpy.float64, order='C')
		self.pow_ntor_fi = numpy.zeros((0,0), numpy.float64, order='C')
		self.pow_ntor_fz = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.cur_tor = EMPTY_DOUBLE
		self.cur_tor_ntor = numpy.zeros(0, numpy.float64, order='C')
		self.mag_axis = mag_axisstructurerz0D('mag_axis')
		self.toroid_field = toroid_fieldstructureb0r0('toroid_field')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class global_paramstructurewaves_global_param\n'
		ret = ret + space + 'Attribute name: ' + str(self.name) + '\n'
		ret = ret + space + 'Attribute type: ' + str(self.type) + '\n'
		s = self.f_assumption.__str__()
		ret = ret + space + 'Attribute f_assumption\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute code_type: ' + str(self.code_type) + '\n'
		ret = ret + space + 'Attribute frequency: ' + str(self.frequency) + '\n'
		s = self.ntor.__str__()
		ret = ret + space + 'Attribute ntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute power_tot: ' + str(self.power_tot) + '\n'
		s = self.p_frac_ntor.__str__()
		ret = ret + space + 'Attribute p_frac_ntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute pow_e: ' + str(self.pow_e) + '\n'
		s = self.pow_i.__str__()
		ret = ret + space + 'Attribute pow_i\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_z.__str__()
		ret = ret + space + 'Attribute pow_z\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute pow_fe: ' + str(self.pow_fe) + '\n'
		s = self.pow_fi.__str__()
		ret = ret + space + 'Attribute pow_fi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_fz.__str__()
		ret = ret + space + 'Attribute pow_fz\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor_e.__str__()
		ret = ret + space + 'Attribute pow_ntor_e\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor_i.__str__()
		ret = ret + space + 'Attribute pow_ntor_i\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor_z.__str__()
		ret = ret + space + 'Attribute pow_ntor_z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor_fe.__str__()
		ret = ret + space + 'Attribute pow_ntor_fe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor_fi.__str__()
		ret = ret + space + 'Attribute pow_ntor_fi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor_fz.__str__()
		ret = ret + space + 'Attribute pow_ntor_fz\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute cur_tor: ' + str(self.cur_tor) + '\n'
		s = self.cur_tor_ntor.__str__()
		ret = ret + space + 'Attribute cur_tor_ntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute mag_axis\n ' + self.mag_axis.__str__(depth+1)
		ret = ret + space + 'Attribute toroid_field\n ' + self.toroid_field.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.mag_axis.setExpIdx(idx)
		self.toroid_field.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructurewaves_global_param, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'f_assumption', numpy.array(self.f_assumption).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'frequency', self.frequency, self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'ntor', numpy.array(self.ntor).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'power_tot', self.power_tot, self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'p_frac_ntor', numpy.array(self.p_frac_ntor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'pow_e', self.pow_e, self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pow_i', numpy.array(self.pow_i).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'pow_z', numpy.array(self.pow_z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'pow_fe', self.pow_fe, self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pow_fi', numpy.array(self.pow_fi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'pow_fz', numpy.array(self.pow_fz).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_e', numpy.array(self.pow_ntor_e).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_i', numpy.array(self.pow_ntor_i).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_z', numpy.array(self.pow_ntor_z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fe', numpy.array(self.pow_ntor_fe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fi', numpy.array(self.pow_ntor_fi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fz', numpy.array(self.pow_ntor_fz).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'cur_tor', self.cur_tor, self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'cur_tor_ntor', numpy.array(self.cur_tor_ntor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.mag_axis.cpoTime = self.cpoTime
		self.mag_axis.putSlice(path, cpopath)
		self.toroid_field.cpoTime = self.cpoTime
		self.toroid_field.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructurewaves_global_param, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'f_assumption', numpy.array(self.f_assumption).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'frequency', self.frequency)
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'ntor', numpy.array(self.ntor).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'power_tot', self.power_tot)
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'p_frac_ntor', numpy.array(self.p_frac_ntor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'pow_e', self.pow_e)
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pow_i', numpy.array(self.pow_i).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'pow_z', numpy.array(self.pow_z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'pow_fe', self.pow_fe)
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pow_fi', numpy.array(self.pow_fi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'pow_fz', numpy.array(self.pow_fz).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_e', numpy.array(self.pow_ntor_e).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_i', numpy.array(self.pow_ntor_i).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_z', numpy.array(self.pow_ntor_z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fe', numpy.array(self.pow_ntor_fe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fi', numpy.array(self.pow_ntor_fi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fz', numpy.array(self.pow_ntor_fz).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'cur_tor', self.cur_tor)
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'cur_tor_ntor', numpy.array(self.cur_tor_ntor).astype(numpy.float64))
		check_status(status)
		self.mag_axis.replaceLastSlice(path, cpopath)
		self.toroid_field.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructurewaves_global_param, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'name', self.name)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'type', self.type)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'code_type', self.code_type)
		check_status(status)
		self.mag_axis.putNonTimed(path, cpopath)
		self.toroid_field.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructurewaves_global_param, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_name = ull.getString(self.idx, path, cpopath + 'name')
		check_status(status)
		if not status:
			self.name = ret_name
		status, ret_type = ull.getString(self.idx, path, cpopath + 'type')
		check_status(status)
		if not status:
			self.type = ret_type
		status, ret_f_assumption, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'f_assumption', inTime, interpolMode)
		check_status(status)
		if not status:
			self.f_assumption = ret_f_assumption
			self.cpoTime = retTime
		status, ret_code_type = ull.getInt(self.idx, path, cpopath + 'code_type')
		check_status(status)
		if not status:
			self.code_type = ret_code_type
		status, ret_frequency, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'frequency', inTime, interpolMode)
		check_status(status)
		if not status:
			self.frequency = ret_frequency
			self.cpoTime = retTime
		status, ret_ntor, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'ntor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ntor = ret_ntor
			self.cpoTime = retTime
		status, ret_power_tot, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'power_tot', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power_tot = ret_power_tot
			self.cpoTime = retTime
		status, ret_p_frac_ntor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'p_frac_ntor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.p_frac_ntor = ret_p_frac_ntor
			self.cpoTime = retTime
		status, ret_pow_e, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'pow_e', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_e = ret_pow_e
			self.cpoTime = retTime
		status, ret_pow_i, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pow_i', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_i = ret_pow_i
			self.cpoTime = retTime
		status, ret_pow_z, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'pow_z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_z = ret_pow_z
			self.cpoTime = retTime
		status, ret_pow_fe, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'pow_fe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_fe = ret_pow_fe
			self.cpoTime = retTime
		status, ret_pow_fi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pow_fi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_fi = ret_pow_fi
			self.cpoTime = retTime
		status, ret_pow_fz, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'pow_fz', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_fz = ret_pow_fz
			self.cpoTime = retTime
		status, ret_pow_ntor_e, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_e', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor_e = ret_pow_ntor_e
			self.cpoTime = retTime
		status, ret_pow_ntor_i, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_i', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor_i = ret_pow_ntor_i
			self.cpoTime = retTime
		status, ret_pow_ntor_z, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor_z = ret_pow_ntor_z
			self.cpoTime = retTime
		status, ret_pow_ntor_fe, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor_fe = ret_pow_ntor_fe
			self.cpoTime = retTime
		status, ret_pow_ntor_fi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor_fi = ret_pow_ntor_fi
			self.cpoTime = retTime
		status, ret_pow_ntor_fz, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fz', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor_fz = ret_pow_ntor_fz
			self.cpoTime = retTime
		status, ret_cur_tor, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'cur_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.cur_tor = ret_cur_tor
			self.cpoTime = retTime
		status, ret_cur_tor_ntor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'cur_tor_ntor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.cur_tor_ntor = ret_cur_tor_ntor
			self.cpoTime = retTime
		self.mag_axis.getSlice(path, cpopath, inTime, interpolMode)
		self.toroid_field.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructurewaves_global_param, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, nameVal = ull.getString(self.idx, path, cpopath + 'name')
			check_status(status)
			status, typeVal = ull.getString(self.idx, path, cpopath + 'type')
			check_status(status)
			status, f_assumptionList = ull.getVect2DInt(self.idx, path, cpopath + 'f_assumption')
			if len(f_assumptionList) == 0:
				f_assumptionList = numpy.resize(f_assumptionList, (0,nbslice))
			check_status(status)
			status, code_typeVal = ull.getInt(self.idx, path, cpopath + 'code_type')
			check_status(status)
			status, frequencyList = ull.getVect1DDouble(self.idx, path, cpopath + 'frequency')
			if len(frequencyList) == 0:
				frequencyList = numpy.resize(frequencyList, (nbslice))
			check_status(status)
			status, ntorList = ull.getVect2DInt(self.idx, path, cpopath + 'ntor')
			if len(ntorList) == 0:
				ntorList = numpy.resize(ntorList, (0,nbslice))
			check_status(status)
			status, power_totList = ull.getVect1DDouble(self.idx, path, cpopath + 'power_tot')
			if len(power_totList) == 0:
				power_totList = numpy.resize(power_totList, (nbslice))
			check_status(status)
			status, p_frac_ntorList = ull.getVect2DDouble(self.idx, path, cpopath + 'p_frac_ntor')
			if len(p_frac_ntorList) == 0:
				p_frac_ntorList = numpy.resize(p_frac_ntorList, (0,nbslice))
			check_status(status)
			status, pow_eList = ull.getVect1DDouble(self.idx, path, cpopath + 'pow_e')
			if len(pow_eList) == 0:
				pow_eList = numpy.resize(pow_eList, (nbslice))
			check_status(status)
			status, pow_iList = ull.getVect2DDouble(self.idx, path, cpopath + 'pow_i')
			if len(pow_iList) == 0:
				pow_iList = numpy.resize(pow_iList, (0,nbslice))
			check_status(status)
			status, pow_zList = ull.getVect3DDouble(self.idx, path, cpopath + 'pow_z')
			if len(pow_zList) == 0:
				pow_zList = numpy.resize(pow_zList, (0,0,nbslice))
			check_status(status)
			status, pow_feList = ull.getVect1DDouble(self.idx, path, cpopath + 'pow_fe')
			if len(pow_feList) == 0:
				pow_feList = numpy.resize(pow_feList, (nbslice))
			check_status(status)
			status, pow_fiList = ull.getVect2DDouble(self.idx, path, cpopath + 'pow_fi')
			if len(pow_fiList) == 0:
				pow_fiList = numpy.resize(pow_fiList, (0,nbslice))
			check_status(status)
			status, pow_fzList = ull.getVect3DDouble(self.idx, path, cpopath + 'pow_fz')
			if len(pow_fzList) == 0:
				pow_fzList = numpy.resize(pow_fzList, (0,0,nbslice))
			check_status(status)
			status, pow_ntor_eList = ull.getVect2DDouble(self.idx, path, cpopath + 'pow_ntor_e')
			if len(pow_ntor_eList) == 0:
				pow_ntor_eList = numpy.resize(pow_ntor_eList, (0,nbslice))
			check_status(status)
			status, pow_ntor_iList = ull.getVect3DDouble(self.idx, path, cpopath + 'pow_ntor_i')
			if len(pow_ntor_iList) == 0:
				pow_ntor_iList = numpy.resize(pow_ntor_iList, (0,0,nbslice))
			check_status(status)
			status, pow_ntor_zList = ull.getVect4DDouble(self.idx, path, cpopath + 'pow_ntor_z')
			if len(pow_ntor_zList) == 0:
				pow_ntor_zList = numpy.resize(pow_ntor_zList, (0,0,0,nbslice))
			check_status(status)
			status, pow_ntor_feList = ull.getVect2DDouble(self.idx, path, cpopath + 'pow_ntor_fe')
			if len(pow_ntor_feList) == 0:
				pow_ntor_feList = numpy.resize(pow_ntor_feList, (0,nbslice))
			check_status(status)
			status, pow_ntor_fiList = ull.getVect3DDouble(self.idx, path, cpopath + 'pow_ntor_fi')
			if len(pow_ntor_fiList) == 0:
				pow_ntor_fiList = numpy.resize(pow_ntor_fiList, (0,0,nbslice))
			check_status(status)
			status, pow_ntor_fzList = ull.getVect4DDouble(self.idx, path, cpopath + 'pow_ntor_fz')
			if len(pow_ntor_fzList) == 0:
				pow_ntor_fzList = numpy.resize(pow_ntor_fzList, (0,0,0,nbslice))
			check_status(status)
			status, cur_torList = ull.getVect1DDouble(self.idx, path, cpopath + 'cur_tor')
			if len(cur_torList) == 0:
				cur_torList = numpy.resize(cur_torList, (nbslice))
			check_status(status)
			status, cur_tor_ntorList = ull.getVect2DDouble(self.idx, path, cpopath + 'cur_tor_ntor')
			if len(cur_tor_ntorList) == 0:
				cur_tor_ntorList = numpy.resize(cur_tor_ntorList, (0,nbslice))
			check_status(status)
			mag_axisList = self.mag_axis.build_non_resampled_data(path, cpopath, nbslice)
			toroid_fieldList = self.toroid_field.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = global_paramstructurewaves_global_param(self.base_path)
				slice.setExpIdx(self.idx)
				slice.name = nameVal
				slice.type = typeVal
				slice.f_assumption = f_assumptionList[:,i]
				slice.code_type = code_typeVal
				slice.frequency = frequencyList[i].copy().astype(float)
				slice.ntor = ntorList[:,i]
				slice.power_tot = power_totList[i].copy().astype(float)
				slice.p_frac_ntor = p_frac_ntorList[:,i]
				slice.pow_e = pow_eList[i].copy().astype(float)
				slice.pow_i = pow_iList[:,i]
				slice.pow_z = pow_zList[:,:,i]
				slice.pow_fe = pow_feList[i].copy().astype(float)
				slice.pow_fi = pow_fiList[:,i]
				slice.pow_fz = pow_fzList[:,:,i]
				slice.pow_ntor_e = pow_ntor_eList[:,i]
				slice.pow_ntor_i = pow_ntor_iList[:,:,i]
				slice.pow_ntor_z = pow_ntor_zList[:,:,:,i]
				slice.pow_ntor_fe = pow_ntor_feList[:,i]
				slice.pow_ntor_fi = pow_ntor_fiList[:,:,i]
				slice.pow_ntor_fz = pow_ntor_fzList[:,:,:,i]
				slice.cur_tor = cur_torList[i].copy().astype(float)
				slice.cur_tor_ntor = cur_tor_ntorList[:,i]
				slice.mag_axis = mag_axisList[i]
				slice.toroid_field = toroid_fieldList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructurewaves_global_paramObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'f_assumption') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'f_assumption', i, numpy.array(self.f_assumption).astype(numpy.int32))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'frequency') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'frequency', i, self.frequency)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'ntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'ntor', i, numpy.array(self.ntor).astype(numpy.int32))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'power_tot') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'power_tot', i, self.power_tot)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'p_frac_ntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'p_frac_ntor', i, numpy.array(self.p_frac_ntor).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pow_e') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pow_e', i, self.pow_e)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pow_i') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pow_i', i, numpy.array(self.pow_i).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pow_z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pow_z', i, numpy.array(self.pow_z).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'pow_fe') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'pow_fe', i, self.pow_fe)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pow_fi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pow_fi', i, numpy.array(self.pow_fi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pow_fz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pow_fz', i, numpy.array(self.pow_fz).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pow_ntor_e') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor_e', i, numpy.array(self.pow_ntor_e).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pow_ntor_i') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor_i', i, numpy.array(self.pow_ntor_i).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'pow_ntor_z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor_z', i, numpy.array(self.pow_ntor_z).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pow_ntor_fe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor_fe', i, numpy.array(self.pow_ntor_fe).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pow_ntor_fi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor_fi', i, numpy.array(self.pow_ntor_fi).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'pow_ntor_fz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor_fz', i, numpy.array(self.pow_ntor_fz).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'cur_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'cur_tor', i, self.cur_tor)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'cur_tor_ntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'cur_tor_ntor', i, numpy.array(self.cur_tor_ntor).astype(numpy.float64))
		obj = self.mag_axis.putTimedElt(path, cpopath + 'mag_axis', i, obj)
		obj = self.toroid_field.putTimedElt(path, cpopath + 'toroid_field', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructurewaves_global_paramObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'f_assumption') 
			print ('obj = ' + str(obj))
		status, ret_f_assumption = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'f_assumption', i)
		check_status(status)
		if not status:
			self.f_assumption = ret_f_assumption
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'frequency') 
			print ('obj = ' + str(obj))
		status, ret_frequency = ull.getDoubleFromObject(self.idx, obj, cpopath + 'frequency', i)
		check_status(status)
		if not status:
			self.frequency = ret_frequency
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'ntor') 
			print ('obj = ' + str(obj))
		status, ret_ntor = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'ntor', i)
		check_status(status)
		if not status:
			self.ntor = ret_ntor
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'power_tot') 
			print ('obj = ' + str(obj))
		status, ret_power_tot = ull.getDoubleFromObject(self.idx, obj, cpopath + 'power_tot', i)
		check_status(status)
		if not status:
			self.power_tot = ret_power_tot
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'p_frac_ntor') 
			print ('obj = ' + str(obj))
		status, ret_p_frac_ntor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'p_frac_ntor', i)
		check_status(status)
		if not status:
			self.p_frac_ntor = ret_p_frac_ntor
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pow_e') 
			print ('obj = ' + str(obj))
		status, ret_pow_e = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pow_e', i)
		check_status(status)
		if not status:
			self.pow_e = ret_pow_e
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pow_i') 
			print ('obj = ' + str(obj))
		status, ret_pow_i = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pow_i', i)
		check_status(status)
		if not status:
			self.pow_i = ret_pow_i
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pow_z') 
			print ('obj = ' + str(obj))
		status, ret_pow_z = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pow_z', i)
		check_status(status)
		if not status:
			self.pow_z = ret_pow_z
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'pow_fe') 
			print ('obj = ' + str(obj))
		status, ret_pow_fe = ull.getDoubleFromObject(self.idx, obj, cpopath + 'pow_fe', i)
		check_status(status)
		if not status:
			self.pow_fe = ret_pow_fe
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pow_fi') 
			print ('obj = ' + str(obj))
		status, ret_pow_fi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pow_fi', i)
		check_status(status)
		if not status:
			self.pow_fi = ret_pow_fi
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pow_fz') 
			print ('obj = ' + str(obj))
		status, ret_pow_fz = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pow_fz', i)
		check_status(status)
		if not status:
			self.pow_fz = ret_pow_fz
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pow_ntor_e') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor_e = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor_e', i)
		check_status(status)
		if not status:
			self.pow_ntor_e = ret_pow_ntor_e
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pow_ntor_i') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor_i = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor_i', i)
		check_status(status)
		if not status:
			self.pow_ntor_i = ret_pow_ntor_i
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'pow_ntor_z') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor_z = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor_z', i)
		check_status(status)
		if not status:
			self.pow_ntor_z = ret_pow_ntor_z
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pow_ntor_fe') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor_fe = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor_fe', i)
		check_status(status)
		if not status:
			self.pow_ntor_fe = ret_pow_ntor_fe
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pow_ntor_fi') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor_fi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor_fi', i)
		check_status(status)
		if not status:
			self.pow_ntor_fi = ret_pow_ntor_fi
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'pow_ntor_fz') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor_fz = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor_fz', i)
		check_status(status)
		if not status:
			self.pow_ntor_fz = ret_pow_ntor_fz
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'cur_tor') 
			print ('obj = ' + str(obj))
		status, ret_cur_tor = ull.getDoubleFromObject(self.idx, obj, cpopath + 'cur_tor', i)
		check_status(status)
		if not status:
			self.cur_tor = ret_cur_tor
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'cur_tor_ntor') 
			print ('obj = ' + str(obj))
		status, ret_cur_tor_ntor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'cur_tor_ntor', i)
		check_status(status)
		if not status:
			self.cur_tor_ntor = ret_cur_tor_ntor
		self.mag_axis.getTimedElt(path, cpopath + 'mag_axis', i, obj)
		self.toroid_field.getTimedElt(path, cpopath + 'toroid_field', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructurewaves_global_paramObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'name', i, self.name)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'type', i, self.type)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'code_type') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'code_type', i, self.code_type)
		obj = self.mag_axis.putNonTimedElt(path, cpopath + 'mag_axis', i, obj)
		obj = self.toroid_field.putNonTimedElt(path, cpopath + 'toroid_field', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructurewaves_global_paramObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'name') 
			print ('obj = ' + str(obj))
		status, ret_name = ull.getStringFromObject(self.idx, obj, cpopath + 'name', i)
		check_status(status)
		if not status:
			self.name = ret_name
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'type') 
			print ('obj = ' + str(obj))
		status, ret_type = ull.getStringFromObject(self.idx, obj, cpopath + 'type', i)
		check_status(status)
		if not status:
			self.type = ret_type
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'code_type') 
			print ('obj = ' + str(obj))
		status, ret_code_type = ull.getIntFromObject(self.idx, obj, cpopath + 'code_type', i)
		check_status(status)
		if not status:
			self.code_type = ret_code_type
		self.mag_axis.getNonTimedElt(path, cpopath + 'mag_axis', i, obj)
		self.toroid_field.getNonTimedElt(path, cpopath + 'toroid_field', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'name')
		ull.deleteData(self.idx, path, cpopath + 'type')
		ull.deleteData(self.idx, path, cpopath + 'f_assumption')
		ull.deleteData(self.idx, path, cpopath + 'code_type')
		ull.deleteData(self.idx, path, cpopath + 'frequency')
		ull.deleteData(self.idx, path, cpopath + 'ntor')
		ull.deleteData(self.idx, path, cpopath + 'power_tot')
		ull.deleteData(self.idx, path, cpopath + 'p_frac_ntor')
		ull.deleteData(self.idx, path, cpopath + 'pow_e')
		ull.deleteData(self.idx, path, cpopath + 'pow_i')
		ull.deleteData(self.idx, path, cpopath + 'pow_z')
		ull.deleteData(self.idx, path, cpopath + 'pow_fe')
		ull.deleteData(self.idx, path, cpopath + 'pow_fi')
		ull.deleteData(self.idx, path, cpopath + 'pow_fz')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor_e')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor_i')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor_z')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor_fe')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor_fi')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor_fz')
		ull.deleteData(self.idx, path, cpopath + 'cur_tor')
		ull.deleteData(self.idx, path, cpopath + 'cur_tor_ntor')
		self.mag_axis.deleteData(path, cpopath)
		self.toroid_field.deleteData(path, cpopath)


class mag_axisstructurerz0D(KeepInOrder):
	'''
	class mag_axisstructurerz0D
	Position of the magnetic axis. Time-dependent; Scalar

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='mag_axis'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mag_axisstructurerz0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mag_axisstructurerz0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'r', self.r, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'z', self.z, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mag_axisstructurerz0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'r', self.r)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'z', self.z)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mag_axisstructurerz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mag_axisstructurerz0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'r', inTime, interpolMode)
		check_status(status)
		if not status:
			self.r = ret_r
			self.cpoTime = retTime
		status, ret_z, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.z = ret_z
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mag_axisstructurerz0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rList = ull.getVect1DDouble(self.idx, path, cpopath + 'r')
			if len(rList) == 0:
				rList = numpy.resize(rList, (nbslice))
			check_status(status)
			status, zList = ull.getVect1DDouble(self.idx, path, cpopath + 'z')
			if len(zList) == 0:
				zList = numpy.resize(zList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = mag_axisstructurerz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[i].copy().astype(float)
				slice.z = zList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mag_axisstructurerz0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r', i, self.r)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'z', i, self.z)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mag_axisstructurerz0DObj, run function getTimedElt') 
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

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mag_axisstructurerz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mag_axisstructurerz0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'z')


class toroid_fieldstructureb0r0(KeepInOrder):
	'''
	class toroid_fieldstructureb0r0
	Characteristics of the vacuum toroidal field (used to define the rho_tor coordinate and the normalisation of parallel current densities).

	Attributes:
	- r0 : float
	   Characteristic major radius of the device (used in publications, usually middle of the vessel at the equatorial midplane) [m]. Scalar. 
	- b0 : float
	   Vacuum field at r0 [T]; Positive sign means anti-clockwise when viewed from above. Scalar. Time-dependent. 
	'''

	def __init__(self, base_path_in='toroid_field'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r0 = EMPTY_DOUBLE
		self.b0 = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class toroid_fieldstructureb0r0\n'
		ret = ret + space + 'Attribute r0: ' + str(self.r0) + '\n'
		ret = ret + space + 'Attribute b0: ' + str(self.b0) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructureb0r0, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'b0', self.b0, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructureb0r0, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'b0', self.b0)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructureb0r0, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'r0', self.r0)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructureb0r0, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r0 = ull.getDouble(self.idx, path, cpopath + 'r0')
		check_status(status)
		if not status:
			self.r0 = ret_r0
		status, ret_b0, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'b0', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b0 = ret_b0
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type toroid_fieldstructureb0r0, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, r0Val = ull.getDouble(self.idx, path, cpopath + 'r0')
			check_status(status)
			status, b0List = ull.getVect1DDouble(self.idx, path, cpopath + 'b0')
			if len(b0List) == 0:
				b0List = numpy.resize(b0List, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = toroid_fieldstructureb0r0(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r0 = r0Val
				slice.b0 = b0List[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroid_fieldstructureb0r0Obj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'b0') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'b0', i, self.b0)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroid_fieldstructureb0r0Obj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'b0') 
			print ('obj = ' + str(obj))
		status, ret_b0 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'b0', i)
		check_status(status)
		if not status:
			self.b0 = ret_b0

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroid_fieldstructureb0r0Obj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'r0') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'r0', i, self.r0)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type toroid_fieldstructureb0r0Obj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'r0') 
			print ('obj = ' + str(obj))
		status, ret_r0 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'r0', i)
		check_status(status)
		if not status:
			self.r0 = ret_r0

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'r0')
		ull.deleteData(self.idx, path, cpopath + 'b0')


class grid_1dstructurewaves_grid_1d(KeepInOrder):
	'''
	class grid_1dstructurewaves_grid_1d
	Grid points for 1D profiles.

	Attributes:
	- rho_tor : numpy.ndarray 1D with float
	   Toroidal flux coordinate [m]. Defined as sqrt((phi-phi_axis)/pi/B0), where B0=../global_param/toroid_field/b0, phi is the toroidal flux and phi_axis is the toroidal flux at the magnetic axis. Time-dependent; Vector (npsi)
	- rho_tor_norm : numpy.ndarray 1D with float
	   The toroidal flux coordinate normalised to be zero at the axis and unity at the last closed flux surface, or last available fluxsurface if the last closed flux surface is not defined. Time-dependent; Vector (npsi)
	- psi : numpy.ndarray 1D with float
	   Poloidal flux function [Wb], evaluated without 1/2pi, such that Bp=|grad psi| /R/2/pi. Time-dependent; Vector (npsi)
	- volume : numpy.ndarray 1D with float
	   Volume enclosed by the flux surface [m^3]. Time-dependent; Vector (npsi)
	- area : numpy.ndarray 1D with float
	   Cross-sectional area of the flux surface [m^2]. Time-dependent; Vector (npsi)
	'''

	def __init__(self, base_path_in='grid_1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.rho_tor = numpy.zeros(0, numpy.float64, order='C')
		self.rho_tor_norm = numpy.zeros(0, numpy.float64, order='C')
		self.psi = numpy.zeros(0, numpy.float64, order='C')
		self.volume = numpy.zeros(0, numpy.float64, order='C')
		self.area = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class grid_1dstructurewaves_grid_1d\n'
		s = self.rho_tor.__str__()
		ret = ret + space + 'Attribute rho_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_tor_norm.__str__()
		ret = ret + space + 'Attribute rho_tor_norm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.volume.__str__()
		ret = ret + space + 'Attribute volume\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.area.__str__()
		ret = ret + space + 'Attribute area\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type grid_1dstructurewaves_grid_1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor_norm', numpy.array(self.rho_tor_norm).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'volume', numpy.array(self.volume).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'area', numpy.array(self.area).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type grid_1dstructurewaves_grid_1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor_norm', numpy.array(self.rho_tor_norm).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'volume', numpy.array(self.volume).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'area', numpy.array(self.area).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type grid_1dstructurewaves_grid_1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type grid_1dstructurewaves_grid_1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_rho_tor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
			self.cpoTime = retTime
		status, ret_rho_tor_norm, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor_norm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor_norm = ret_rho_tor_norm
			self.cpoTime = retTime
		status, ret_psi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'psi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.psi = ret_psi
			self.cpoTime = retTime
		status, ret_volume, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'volume', inTime, interpolMode)
		check_status(status)
		if not status:
			self.volume = ret_volume
			self.cpoTime = retTime
		status, ret_area, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'area', inTime, interpolMode)
		check_status(status)
		if not status:
			self.area = ret_area
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type grid_1dstructurewaves_grid_1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rho_torList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_tor')
			if len(rho_torList) == 0:
				rho_torList = numpy.resize(rho_torList, (0,nbslice))
			check_status(status)
			status, rho_tor_normList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_tor_norm')
			if len(rho_tor_normList) == 0:
				rho_tor_normList = numpy.resize(rho_tor_normList, (0,nbslice))
			check_status(status)
			status, psiList = ull.getVect2DDouble(self.idx, path, cpopath + 'psi')
			if len(psiList) == 0:
				psiList = numpy.resize(psiList, (0,nbslice))
			check_status(status)
			status, volumeList = ull.getVect2DDouble(self.idx, path, cpopath + 'volume')
			if len(volumeList) == 0:
				volumeList = numpy.resize(volumeList, (0,nbslice))
			check_status(status)
			status, areaList = ull.getVect2DDouble(self.idx, path, cpopath + 'area')
			if len(areaList) == 0:
				areaList = numpy.resize(areaList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = grid_1dstructurewaves_grid_1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.rho_tor = rho_torList[:,i]
				slice.rho_tor_norm = rho_tor_normList[:,i]
				slice.psi = psiList[:,i]
				slice.volume = volumeList[:,i]
				slice.area = areaList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type grid_1dstructurewaves_grid_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'rho_tor', i, numpy.array(self.rho_tor).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'rho_tor_norm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'rho_tor_norm', i, numpy.array(self.rho_tor_norm).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'psi', i, numpy.array(self.psi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'volume') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'volume', i, numpy.array(self.volume).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'area') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'area', i, numpy.array(self.area).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type grid_1dstructurewaves_grid_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'rho_tor', i)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'rho_tor_norm') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor_norm = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'rho_tor_norm', i)
		check_status(status)
		if not status:
			self.rho_tor_norm = ret_rho_tor_norm
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		status, ret_psi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'psi', i)
		check_status(status)
		if not status:
			self.psi = ret_psi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'volume') 
			print ('obj = ' + str(obj))
		status, ret_volume = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'volume', i)
		check_status(status)
		if not status:
			self.volume = ret_volume
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'area') 
			print ('obj = ' + str(obj))
		status, ret_area = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'area', i)
		check_status(status)
		if not status:
			self.area = ret_area

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type grid_1dstructurewaves_grid_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type grid_1dstructurewaves_grid_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'rho_tor')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor_norm')
		ull.deleteData(self.idx, path, cpopath + 'psi')
		ull.deleteData(self.idx, path, cpopath + 'volume')
		ull.deleteData(self.idx, path, cpopath + 'area')


class grid_2dstructurewaves_grid_2d(KeepInOrder):
	'''
	class grid_2dstructurewaves_grid_2d
	Grid points for 2D profiles and for full wave solutions.

	Attributes:
	- grid_type : int
	   Grid type. 1: rectangular grid in (R,Z). 2: rectangular grid in (psi, theta). 3: unstructured grid. Integer.
	- rho_tor_norm : numpy.ndarray 2D with float
	   The toroidal flux coordinate normalised to be zero at the axis and unity at the last closed flux surface (or last available fluxsurface from a fix boundary equilibrium code). Time-dependent; Matrix (ndim1, ndim2)
	- rho_tor : numpy.ndarray 2D with float
	   Toroidal flux coordinate [m]. Defined as sqrt((phi-phi_axis)/pi/B0), where B0=../global_param/toroid_field/b0, phi is the toroidal flux and phi_axis is the toroidal flux at the magnetic axis. Time-dependent; Matrix (ndim1, ndim2)
	- psi : numpy.ndarray 2D with float
	   Grid points in poloidal flux function [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent; Matrix (ndim1, ndim2)
	- theta : numpy.ndarray 2D with float
	   Poloidal angle at the grid points (see theta_info for detailed definition); Time-dependent; Matrix (ndim1, ndim2)
	- r : numpy.ndarray 2D with float
	   R (major radius) of grid points; Time-dependent; Matrix(ndim1, ndim2)
	- z : numpy.ndarray 2D with float
	   Z (altitude) of grid points; Time-dependent; Matrix (ndim1, ndim2)
	- theta_info : class theta_infostructuretheta_info
	   Information on the poloidal angle theta.
	'''

	def __init__(self, base_path_in='grid_2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.grid_type = EMPTY_INT
		self.rho_tor_norm = numpy.zeros((0,0), numpy.float64, order='C')
		self.rho_tor = numpy.zeros((0,0), numpy.float64, order='C')
		self.psi = numpy.zeros((0,0), numpy.float64, order='C')
		self.theta = numpy.zeros((0,0), numpy.float64, order='C')
		self.r = numpy.zeros((0,0), numpy.float64, order='C')
		self.z = numpy.zeros((0,0), numpy.float64, order='C')
		self.theta_info = theta_infostructuretheta_info('theta_info')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class grid_2dstructurewaves_grid_2d\n'
		ret = ret + space + 'Attribute grid_type: ' + str(self.grid_type) + '\n'
		s = self.rho_tor_norm.__str__()
		ret = ret + space + 'Attribute rho_tor_norm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_tor.__str__()
		ret = ret + space + 'Attribute rho_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta.__str__()
		ret = ret + space + 'Attribute theta\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute theta_info\n ' + self.theta_info.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.theta_info.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type grid_2dstructurewaves_grid_2d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'rho_tor_norm', numpy.array(self.rho_tor_norm).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'theta', numpy.array(self.theta).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.theta_info.cpoTime = self.cpoTime
		self.theta_info.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type grid_2dstructurewaves_grid_2d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'rho_tor_norm', numpy.array(self.rho_tor_norm).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'theta', numpy.array(self.theta).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64))
		check_status(status)
		self.theta_info.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type grid_2dstructurewaves_grid_2d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'grid_type', self.grid_type)
		check_status(status)
		self.theta_info.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type grid_2dstructurewaves_grid_2d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_grid_type = ull.getInt(self.idx, path, cpopath + 'grid_type')
		check_status(status)
		if not status:
			self.grid_type = ret_grid_type
		status, ret_rho_tor_norm, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'rho_tor_norm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor_norm = ret_rho_tor_norm
			self.cpoTime = retTime
		status, ret_rho_tor, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'rho_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
			self.cpoTime = retTime
		status, ret_psi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'psi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.psi = ret_psi
			self.cpoTime = retTime
		status, ret_theta, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'theta', inTime, interpolMode)
		check_status(status)
		if not status:
			self.theta = ret_theta
			self.cpoTime = retTime
		status, ret_r, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'r', inTime, interpolMode)
		check_status(status)
		if not status:
			self.r = ret_r
			self.cpoTime = retTime
		status, ret_z, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.z = ret_z
			self.cpoTime = retTime
		self.theta_info.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type grid_2dstructurewaves_grid_2d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, grid_typeVal = ull.getInt(self.idx, path, cpopath + 'grid_type')
			check_status(status)
			status, rho_tor_normList = ull.getVect3DDouble(self.idx, path, cpopath + 'rho_tor_norm')
			if len(rho_tor_normList) == 0:
				rho_tor_normList = numpy.resize(rho_tor_normList, (0,0,nbslice))
			check_status(status)
			status, rho_torList = ull.getVect3DDouble(self.idx, path, cpopath + 'rho_tor')
			if len(rho_torList) == 0:
				rho_torList = numpy.resize(rho_torList, (0,0,nbslice))
			check_status(status)
			status, psiList = ull.getVect3DDouble(self.idx, path, cpopath + 'psi')
			if len(psiList) == 0:
				psiList = numpy.resize(psiList, (0,0,nbslice))
			check_status(status)
			status, thetaList = ull.getVect3DDouble(self.idx, path, cpopath + 'theta')
			if len(thetaList) == 0:
				thetaList = numpy.resize(thetaList, (0,0,nbslice))
			check_status(status)
			status, rList = ull.getVect3DDouble(self.idx, path, cpopath + 'r')
			if len(rList) == 0:
				rList = numpy.resize(rList, (0,0,nbslice))
			check_status(status)
			status, zList = ull.getVect3DDouble(self.idx, path, cpopath + 'z')
			if len(zList) == 0:
				zList = numpy.resize(zList, (0,0,nbslice))
			check_status(status)
			theta_infoList = self.theta_info.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = grid_2dstructurewaves_grid_2d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.grid_type = grid_typeVal
				slice.rho_tor_norm = rho_tor_normList[:,:,i]
				slice.rho_tor = rho_torList[:,:,i]
				slice.psi = psiList[:,:,i]
				slice.theta = thetaList[:,:,i]
				slice.r = rList[:,:,i]
				slice.z = zList[:,:,i]
				slice.theta_info = theta_infoList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type grid_2dstructurewaves_grid_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'rho_tor_norm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'rho_tor_norm', i, numpy.array(self.rho_tor_norm).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'rho_tor', i, numpy.array(self.rho_tor).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'psi', i, numpy.array(self.psi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'theta', i, numpy.array(self.theta).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		obj = self.theta_info.putTimedElt(path, cpopath + 'theta_info', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type grid_2dstructurewaves_grid_2dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'rho_tor_norm') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor_norm = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'rho_tor_norm', i)
		check_status(status)
		if not status:
			self.rho_tor_norm = ret_rho_tor_norm
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'rho_tor', i)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		status, ret_psi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'psi', i)
		check_status(status)
		if not status:
			self.psi = ret_psi
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		status, ret_theta = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'theta', i)
		check_status(status)
		if not status:
			self.theta = ret_theta
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
		self.theta_info.getTimedElt(path, cpopath + 'theta_info', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type grid_2dstructurewaves_grid_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'grid_type') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'grid_type', i, self.grid_type)
		obj = self.theta_info.putNonTimedElt(path, cpopath + 'theta_info', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type grid_2dstructurewaves_grid_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'grid_type') 
			print ('obj = ' + str(obj))
		status, ret_grid_type = ull.getIntFromObject(self.idx, obj, cpopath + 'grid_type', i)
		check_status(status)
		if not status:
			self.grid_type = ret_grid_type
		self.theta_info.getNonTimedElt(path, cpopath + 'theta_info', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'grid_type')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor_norm')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor')
		ull.deleteData(self.idx, path, cpopath + 'psi')
		ull.deleteData(self.idx, path, cpopath + 'theta')
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'z')
		self.theta_info.deleteData(path, cpopath)


class theta_infostructuretheta_info(KeepInOrder):
	'''
	class theta_infostructuretheta_info
	Information on the poloidal angle theta.

	Attributes:
	- angl_type : int
	   Type of poloidal angle: 1 : same as the poloidal angle in the equlibrium cpo; 2 : geometrical polar angle, tan(theta) = Z/(R-R_0); 3 : other. If option 3, a transformation to the geometrical poloidal angle is provided in th2th_pol.
	- th2th_pol : numpy.ndarray 2D with float
	   Geometrical poloidal angle at grid points in theta, i.e. the transformation from theta to the polar poloidal angle; used only if angl_type=3; Time-dependent; Matrix (ndim1, ndim2)
	'''

	def __init__(self, base_path_in='theta_info'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.angl_type = EMPTY_INT
		self.th2th_pol = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class theta_infostructuretheta_info\n'
		ret = ret + space + 'Attribute angl_type: ' + str(self.angl_type) + '\n'
		s = self.th2th_pol.__str__()
		ret = ret + space + 'Attribute th2th_pol\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type theta_infostructuretheta_info, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'th2th_pol', numpy.array(self.th2th_pol).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type theta_infostructuretheta_info, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'th2th_pol', numpy.array(self.th2th_pol).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type theta_infostructuretheta_info, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'angl_type', self.angl_type)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type theta_infostructuretheta_info, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_angl_type = ull.getInt(self.idx, path, cpopath + 'angl_type')
		check_status(status)
		if not status:
			self.angl_type = ret_angl_type
		status, ret_th2th_pol, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'th2th_pol', inTime, interpolMode)
		check_status(status)
		if not status:
			self.th2th_pol = ret_th2th_pol
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type theta_infostructuretheta_info, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, angl_typeVal = ull.getInt(self.idx, path, cpopath + 'angl_type')
			check_status(status)
			status, th2th_polList = ull.getVect3DDouble(self.idx, path, cpopath + 'th2th_pol')
			if len(th2th_polList) == 0:
				th2th_polList = numpy.resize(th2th_polList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = theta_infostructuretheta_info(self.base_path)
				slice.setExpIdx(self.idx)
				slice.angl_type = angl_typeVal
				slice.th2th_pol = th2th_polList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type theta_infostructuretheta_infoObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'th2th_pol') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'th2th_pol', i, numpy.array(self.th2th_pol).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type theta_infostructuretheta_infoObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'th2th_pol') 
			print ('obj = ' + str(obj))
		status, ret_th2th_pol = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'th2th_pol', i)
		check_status(status)
		if not status:
			self.th2th_pol = ret_th2th_pol

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type theta_infostructuretheta_infoObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'angl_type') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'angl_type', i, self.angl_type)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type theta_infostructuretheta_infoObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'angl_type') 
			print ('obj = ' + str(obj))
		status, ret_angl_type = ull.getIntFromObject(self.idx, obj, cpopath + 'angl_type', i)
		check_status(status)
		if not status:
			self.angl_type = ret_angl_type

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'angl_type')
		ull.deleteData(self.idx, path, cpopath + 'th2th_pol')


class profiles_1dstructurewaves_profiles_1d(KeepInOrder):
	'''
	class profiles_1dstructurewaves_profiles_1d
	1D radial profiles

	Attributes:
	- powd_tot : numpy.ndarray 1D with float
	   Total flux surface averaged wave power density [W/m^3]; Time-dependent; Vector (npsi)
	- powd_e : numpy.ndarray 1D with float
	   Flux surface averaged absorbed wave power density on the thermal electrons [W/m^3]; Time-dependent; Vector (npsi)
	- powd_i : numpy.ndarray 2D with float
	   Flux surface averaged absorbed wave power density on the thermal ion species [W/m^3]; Time-dependent; Matrix (npsi, nion)
	- powd_z : numpy.ndarray 3D with float
	   Flux surface averaged absorbed wave power density on the thermal impurities species [W/m^3]; Time-dependent; Matrix (npsi, nimpur, nzimp)
	- powd_fe : numpy.ndarray 1D with float
	   Flux surface averaged absorbed wave power density on the fast electrons [W/m^3]; Time-dependent; Vector (npsi)
	- powd_fi : numpy.ndarray 2D with float
	   Flux surface averaged absorbed wave power density on the fast ion species [W/m^3]; Time-dependent; Matrix (npsi, nion)
	- powd_fz : numpy.ndarray 3D with float
	   Flux surface averaged absorbed wave power density on the fast impurities species [W/m^3]; Time-dependent; Matrix (npsi, nimpur, nzimp)
	- powd_ntor : numpy.ndarray 2D with float
	   Flux surface averaged power density for each toroidal mode number [W/m^3]; Time-dependent; Matrix(npsi, ntor)
	- powd_ntor_e : numpy.ndarray 2D with float
	   Flux surface averaged power density absorbed for each toroidal mode number on the thermal electrons [W/m^3]; Time-dependent; Matrix (npsi, ntor)
	- powd_ntor_i : numpy.ndarray 3D with float
	   Flux surface averaged power density absorbed for each toroidal mode number on each thermal ions species [W/m^3]; Time-dependent; Array3D (npsi, ntor, nion)
	- powd_ntor_z : numpy.ndarray 4D with float
	   Flux surface averaged power density absorbed for each toroidal mode number on each thermal impurity species [W/m^3]; Time-dependent; Array3D (npsi, ntor, nimpur, nzimp)
	- powd_ntor_fe : numpy.ndarray 2D with float
	   Flux surface averaged power density absorbed for each toroidal mode number on the fast electrons [W/m^3]; Time-dependent; Matrix (npsi, ntor)
	- powd_ntor_fi : numpy.ndarray 3D with float
	   Flux surface averaged power density absorbed for each toroidal mode number on each fast ions species [W/m^3]; Time-dependent; Array3D (npsi, ntor, nion)
	- powd_ntor_fz : numpy.ndarray 4D with float
	   Flux surface averaged power density absorbed for each toroidal mode number on each fast impurity species [W/m^3]; Time-dependent; Array3D (npsi, ntor, nimpur, nzimp)
	- curd_tor : numpy.ndarray 1D with float
	   Flux surface averaged wave driven toroidal current density = average(jphi/R) / average(1/R) [A/m^2]; Time-dependent; Vector (npsi)
	- curd_torntor : numpy.ndarray 2D with float
	   Flux surface averaged wave driven toroidal current density for each toroidal mode number = average(jphi/R) / average(1/R) [A/m^2]; Time-dependent; Matrix (npsi, ntor)
	- pow_tot : numpy.ndarray 1D with float
	   Cumulative volume integral of the absorbed wave power density [W]; Time-dependent; Vector (npsi)
	- pow_e : numpy.ndarray 1D with float
	   Cumulative volume integral of the absorbed wave power on the thermal electrons [W]; Time-dependent; Vector (npsi)
	- pow_i : numpy.ndarray 2D with float
	   Cumulative volume integral of the absorbed wave power on the thermal ion species [W]; Time-dependent; Matrix (npsi, nion)
	- pow_z : numpy.ndarray 3D with float
	   Cumulative volume integral of the absorbed wave power on the thermal impurities species [W]; Time-dependent; Matrix (npsi, nimpur, nzimp)
	- pow_fe : numpy.ndarray 1D with float
	   Cumulative volume integral of the absorbed wave power on the fast electrons [W]; Time-dependent; Vector (npsi)
	- pow_fi : numpy.ndarray 2D with float
	   Cumulative volume integral of the absorbed wave power on the fast ion species [W]; Time-dependent; Matrix (npsi, nion)
	- pow_fz : numpy.ndarray 3D with float
	   Cumulative volume integral of the absorbed wave power on the fast impurities species [W]; Time-dependent; Matrix (npsi, nimpur, nzimp)
	- pow_ntor : numpy.ndarray 2D with float
	   Cumulative volume integral of the absorbed wave power for each toroidal mode number [W]; Time-dependent; Matrix (npsi, ntor)
	- pow_ntor_e : numpy.ndarray 2D with float
	   Cumulative volume integral of the absorbed wave power for each toroidal mode number on the thermal electrons  [W]; Time-dependent; Matrix (npsi, ntor)
	- pow_ntor_i : numpy.ndarray 3D with float
	   Cumulative volume integral of the absorbed wave power for each toroidal mode number on each thermal ions species [W]; Time-dependent; Array3D (npsi, ntor, nion)
	- pow_ntor_z : numpy.ndarray 3D with float
	   Cumulative volume integral of the absorbed wave power for each toroidal mode number on each thermal impurity species [W]; Time-dependent; Array3D (npsi, ntor, nimpur, nzimp)
	- pow_ntor_fe : numpy.ndarray 2D with float
	   Cumulative volume integral of the absorbed wave power for each toroidal mode number on the fast electrons  [W]; Time-dependent; Matrix (npsi, ntor)
	- pow_ntor_fi : numpy.ndarray 3D with float
	   Cumulative volume integral of the absorbed wave power for each toroidal mode number on each fast ions species [W]; Time-dependent; Array3D (npsi, ntor, nion)
	- pow_ntor_fz : numpy.ndarray 3D with float
	   Cumulative volume integral of the absorbed wave power for each toroidal mode number on each fast impurity species [W]; Time-dependent; Array3D (npsi, ntor, nimpur, nzimp)
	- curd_par : numpy.ndarray 1D with float
	   Flux surface averaged wave driven parallel current density = average(j.B) / B0, where B0 = global_param/toroid_field/b0; [A/m^2]; Time-dependent; Vector (npsi)
	- curd_parntor : numpy.ndarray 2D with float
	   Flux surface averaged wave driven parallel current density for each toroidal mode number = average(j.B) / B0, where B0 = global_param/toroid_field/b0; [A/m^2]; Time-dependent; Matrix (npsi, ntor)
	- cur_tor : numpy.ndarray 1D with float
	   Wave driven toroidal current inside a flux surface [A]; Time-dependent; Vector (npsi)
	- cur_tor_ntor : numpy.ndarray 2D with float
	   Wave driven toroidal current inside a flux surface for each toroidal mode number [A]; Time-dependent; Matrix (npsi, ntor)
	- e_plus_ave : numpy.ndarray 2D with float
	   The left hand polarised electric field component, E_plus [V/m], averaged over the flux surface, where the averaged is weighted with the power depotition, P, such that e_plus_ave = ave( E_plus P ) / ave( P ), where ave(*) is the flux surface average operator; Time-dependent; Matrix (npsi, ntor)
	- e_minus_ave : numpy.ndarray 2D with float
	   The right hand polarised electric field component, E_minus [V/m], averaged over the flux surface, where the averaged is weighted with the power depotition, P, such that e_minus_ave = ave( E_minus P ) / ave( P ), where (*) is the flux surface average operator; Time-dependent; Matrix (npsi, ntor)
	- e_para_ave : numpy.ndarray 2D with float
	   The parallel electric field component, E_para [V/m], averaged over the flux surface, where the averaged is weighted with the power depotition, P, such that e_para_ave = ave( E_para P ) / ave( P ), where ave(*) is the flux surface average operator; Time-dependent; Matrix (npsi, ntor)
	- k_perp_ave : numpy.ndarray 2D with float
	   The perpendicular wave number, k_perp [1/m], averaged over the flux surface, where the averaged is weighted with the power depotition, P, such that k_perp_ave = ave( k_perp P ) / ( P ), where ave(*) is the flux surface average operator; Time-dependent; Matrix (npsi, ntor)
	'''

	def __init__(self, base_path_in='profiles_1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.powd_tot = numpy.zeros(0, numpy.float64, order='C')
		self.powd_e = numpy.zeros(0, numpy.float64, order='C')
		self.powd_i = numpy.zeros((0,0), numpy.float64, order='C')
		self.powd_z = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.powd_fe = numpy.zeros(0, numpy.float64, order='C')
		self.powd_fi = numpy.zeros((0,0), numpy.float64, order='C')
		self.powd_fz = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.powd_ntor = numpy.zeros((0,0), numpy.float64, order='C')
		self.powd_ntor_e = numpy.zeros((0,0), numpy.float64, order='C')
		self.powd_ntor_i = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.powd_ntor_z = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.powd_ntor_fe = numpy.zeros((0,0), numpy.float64, order='C')
		self.powd_ntor_fi = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.powd_ntor_fz = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.curd_tor = numpy.zeros(0, numpy.float64, order='C')
		self.curd_torntor = numpy.zeros((0,0), numpy.float64, order='C')
		self.pow_tot = numpy.zeros(0, numpy.float64, order='C')
		self.pow_e = numpy.zeros(0, numpy.float64, order='C')
		self.pow_i = numpy.zeros((0,0), numpy.float64, order='C')
		self.pow_z = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.pow_fe = numpy.zeros(0, numpy.float64, order='C')
		self.pow_fi = numpy.zeros((0,0), numpy.float64, order='C')
		self.pow_fz = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.pow_ntor = numpy.zeros((0,0), numpy.float64, order='C')
		self.pow_ntor_e = numpy.zeros((0,0), numpy.float64, order='C')
		self.pow_ntor_i = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.pow_ntor_z = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.pow_ntor_fe = numpy.zeros((0,0), numpy.float64, order='C')
		self.pow_ntor_fi = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.pow_ntor_fz = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.curd_par = numpy.zeros(0, numpy.float64, order='C')
		self.curd_parntor = numpy.zeros((0,0), numpy.float64, order='C')
		self.cur_tor = numpy.zeros(0, numpy.float64, order='C')
		self.cur_tor_ntor = numpy.zeros((0,0), numpy.float64, order='C')
		self.e_plus_ave = numpy.zeros((0,0), numpy.float64, order='C')
		self.e_minus_ave = numpy.zeros((0,0), numpy.float64, order='C')
		self.e_para_ave = numpy.zeros((0,0), numpy.float64, order='C')
		self.k_perp_ave = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class profiles_1dstructurewaves_profiles_1d\n'
		s = self.powd_tot.__str__()
		ret = ret + space + 'Attribute powd_tot\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_e.__str__()
		ret = ret + space + 'Attribute powd_e\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_i.__str__()
		ret = ret + space + 'Attribute powd_i\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_z.__str__()
		ret = ret + space + 'Attribute powd_z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_fe.__str__()
		ret = ret + space + 'Attribute powd_fe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_fi.__str__()
		ret = ret + space + 'Attribute powd_fi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_fz.__str__()
		ret = ret + space + 'Attribute powd_fz\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor.__str__()
		ret = ret + space + 'Attribute powd_ntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor_e.__str__()
		ret = ret + space + 'Attribute powd_ntor_e\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor_i.__str__()
		ret = ret + space + 'Attribute powd_ntor_i\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor_z.__str__()
		ret = ret + space + 'Attribute powd_ntor_z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor_fe.__str__()
		ret = ret + space + 'Attribute powd_ntor_fe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor_fi.__str__()
		ret = ret + space + 'Attribute powd_ntor_fi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor_fz.__str__()
		ret = ret + space + 'Attribute powd_ntor_fz\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.curd_tor.__str__()
		ret = ret + space + 'Attribute curd_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.curd_torntor.__str__()
		ret = ret + space + 'Attribute curd_torntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_tot.__str__()
		ret = ret + space + 'Attribute pow_tot\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_e.__str__()
		ret = ret + space + 'Attribute pow_e\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_i.__str__()
		ret = ret + space + 'Attribute pow_i\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_z.__str__()
		ret = ret + space + 'Attribute pow_z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_fe.__str__()
		ret = ret + space + 'Attribute pow_fe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_fi.__str__()
		ret = ret + space + 'Attribute pow_fi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_fz.__str__()
		ret = ret + space + 'Attribute pow_fz\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor.__str__()
		ret = ret + space + 'Attribute pow_ntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor_e.__str__()
		ret = ret + space + 'Attribute pow_ntor_e\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor_i.__str__()
		ret = ret + space + 'Attribute pow_ntor_i\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor_z.__str__()
		ret = ret + space + 'Attribute pow_ntor_z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor_fe.__str__()
		ret = ret + space + 'Attribute pow_ntor_fe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor_fi.__str__()
		ret = ret + space + 'Attribute pow_ntor_fi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pow_ntor_fz.__str__()
		ret = ret + space + 'Attribute pow_ntor_fz\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.curd_par.__str__()
		ret = ret + space + 'Attribute curd_par\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.curd_parntor.__str__()
		ret = ret + space + 'Attribute curd_parntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.cur_tor.__str__()
		ret = ret + space + 'Attribute cur_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.cur_tor_ntor.__str__()
		ret = ret + space + 'Attribute cur_tor_ntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_plus_ave.__str__()
		ret = ret + space + 'Attribute e_plus_ave\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_minus_ave.__str__()
		ret = ret + space + 'Attribute e_minus_ave\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_para_ave.__str__()
		ret = ret + space + 'Attribute e_para_ave\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.k_perp_ave.__str__()
		ret = ret + space + 'Attribute k_perp_ave\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructurewaves_profiles_1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'powd_tot', numpy.array(self.powd_tot).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'powd_e', numpy.array(self.powd_e).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'powd_i', numpy.array(self.powd_i).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'powd_z', numpy.array(self.powd_z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'powd_fe', numpy.array(self.powd_fe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'powd_fi', numpy.array(self.powd_fi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'powd_fz', numpy.array(self.powd_fz).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'powd_ntor', numpy.array(self.powd_ntor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_e', numpy.array(self.powd_ntor_e).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_i', numpy.array(self.powd_ntor_i).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect4DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_z', numpy.array(self.powd_ntor_z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fe', numpy.array(self.powd_ntor_fe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fi', numpy.array(self.powd_ntor_fi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect4DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fz', numpy.array(self.powd_ntor_fz).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'curd_tor', numpy.array(self.curd_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'curd_torntor', numpy.array(self.curd_torntor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pow_tot', numpy.array(self.pow_tot).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pow_e', numpy.array(self.pow_e).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'pow_i', numpy.array(self.pow_i).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'pow_z', numpy.array(self.pow_z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pow_fe', numpy.array(self.pow_fe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'pow_fi', numpy.array(self.pow_fi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'pow_fz', numpy.array(self.pow_fz).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor', numpy.array(self.pow_ntor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_e', numpy.array(self.pow_ntor_e).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_i', numpy.array(self.pow_ntor_i).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_z', numpy.array(self.pow_ntor_z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fe', numpy.array(self.pow_ntor_fe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fi', numpy.array(self.pow_ntor_fi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fz', numpy.array(self.pow_ntor_fz).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'curd_par', numpy.array(self.curd_par).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'curd_parntor', numpy.array(self.curd_parntor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'cur_tor', numpy.array(self.cur_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'cur_tor_ntor', numpy.array(self.cur_tor_ntor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'e_plus_ave', numpy.array(self.e_plus_ave).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'e_minus_ave', numpy.array(self.e_minus_ave).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'e_para_ave', numpy.array(self.e_para_ave).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'k_perp_ave', numpy.array(self.k_perp_ave).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructurewaves_profiles_1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'powd_tot', numpy.array(self.powd_tot).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'powd_e', numpy.array(self.powd_e).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'powd_i', numpy.array(self.powd_i).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'powd_z', numpy.array(self.powd_z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'powd_fe', numpy.array(self.powd_fe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'powd_fi', numpy.array(self.powd_fi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'powd_fz', numpy.array(self.powd_fz).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'powd_ntor', numpy.array(self.powd_ntor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_e', numpy.array(self.powd_ntor_e).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_i', numpy.array(self.powd_ntor_i).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect4DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_z', numpy.array(self.powd_ntor_z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fe', numpy.array(self.powd_ntor_fe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fi', numpy.array(self.powd_ntor_fi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect4DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fz', numpy.array(self.powd_ntor_fz).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'curd_tor', numpy.array(self.curd_tor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'curd_torntor', numpy.array(self.curd_torntor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pow_tot', numpy.array(self.pow_tot).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pow_e', numpy.array(self.pow_e).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'pow_i', numpy.array(self.pow_i).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'pow_z', numpy.array(self.pow_z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pow_fe', numpy.array(self.pow_fe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'pow_fi', numpy.array(self.pow_fi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'pow_fz', numpy.array(self.pow_fz).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor', numpy.array(self.pow_ntor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_e', numpy.array(self.pow_ntor_e).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_i', numpy.array(self.pow_ntor_i).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_z', numpy.array(self.pow_ntor_z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fe', numpy.array(self.pow_ntor_fe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fi', numpy.array(self.pow_ntor_fi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fz', numpy.array(self.pow_ntor_fz).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'curd_par', numpy.array(self.curd_par).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'curd_parntor', numpy.array(self.curd_parntor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'cur_tor', numpy.array(self.cur_tor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'cur_tor_ntor', numpy.array(self.cur_tor_ntor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'e_plus_ave', numpy.array(self.e_plus_ave).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'e_minus_ave', numpy.array(self.e_minus_ave).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'e_para_ave', numpy.array(self.e_para_ave).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'k_perp_ave', numpy.array(self.k_perp_ave).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructurewaves_profiles_1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructurewaves_profiles_1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_powd_tot, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'powd_tot', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_tot = ret_powd_tot
			self.cpoTime = retTime
		status, ret_powd_e, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'powd_e', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_e = ret_powd_e
			self.cpoTime = retTime
		status, ret_powd_i, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'powd_i', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_i = ret_powd_i
			self.cpoTime = retTime
		status, ret_powd_z, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'powd_z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_z = ret_powd_z
			self.cpoTime = retTime
		status, ret_powd_fe, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'powd_fe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_fe = ret_powd_fe
			self.cpoTime = retTime
		status, ret_powd_fi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'powd_fi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_fi = ret_powd_fi
			self.cpoTime = retTime
		status, ret_powd_fz, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'powd_fz', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_fz = ret_powd_fz
			self.cpoTime = retTime
		status, ret_powd_ntor, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'powd_ntor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor = ret_powd_ntor
			self.cpoTime = retTime
		status, ret_powd_ntor_e, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_e', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor_e = ret_powd_ntor_e
			self.cpoTime = retTime
		status, ret_powd_ntor_i, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_i', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor_i = ret_powd_ntor_i
			self.cpoTime = retTime
		status, ret_powd_ntor_z, retTime = ull.getVect4DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor_z = ret_powd_ntor_z
			self.cpoTime = retTime
		status, ret_powd_ntor_fe, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor_fe = ret_powd_ntor_fe
			self.cpoTime = retTime
		status, ret_powd_ntor_fi, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor_fi = ret_powd_ntor_fi
			self.cpoTime = retTime
		status, ret_powd_ntor_fz, retTime = ull.getVect4DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fz', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor_fz = ret_powd_ntor_fz
			self.cpoTime = retTime
		status, ret_curd_tor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'curd_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.curd_tor = ret_curd_tor
			self.cpoTime = retTime
		status, ret_curd_torntor, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'curd_torntor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.curd_torntor = ret_curd_torntor
			self.cpoTime = retTime
		status, ret_pow_tot, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pow_tot', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_tot = ret_pow_tot
			self.cpoTime = retTime
		status, ret_pow_e, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pow_e', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_e = ret_pow_e
			self.cpoTime = retTime
		status, ret_pow_i, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'pow_i', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_i = ret_pow_i
			self.cpoTime = retTime
		status, ret_pow_z, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'pow_z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_z = ret_pow_z
			self.cpoTime = retTime
		status, ret_pow_fe, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pow_fe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_fe = ret_pow_fe
			self.cpoTime = retTime
		status, ret_pow_fi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'pow_fi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_fi = ret_pow_fi
			self.cpoTime = retTime
		status, ret_pow_fz, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'pow_fz', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_fz = ret_pow_fz
			self.cpoTime = retTime
		status, ret_pow_ntor, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor = ret_pow_ntor
			self.cpoTime = retTime
		status, ret_pow_ntor_e, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_e', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor_e = ret_pow_ntor_e
			self.cpoTime = retTime
		status, ret_pow_ntor_i, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_i', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor_i = ret_pow_ntor_i
			self.cpoTime = retTime
		status, ret_pow_ntor_z, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor_z = ret_pow_ntor_z
			self.cpoTime = retTime
		status, ret_pow_ntor_fe, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor_fe = ret_pow_ntor_fe
			self.cpoTime = retTime
		status, ret_pow_ntor_fi, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor_fi = ret_pow_ntor_fi
			self.cpoTime = retTime
		status, ret_pow_ntor_fz, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'pow_ntor_fz', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pow_ntor_fz = ret_pow_ntor_fz
			self.cpoTime = retTime
		status, ret_curd_par, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'curd_par', inTime, interpolMode)
		check_status(status)
		if not status:
			self.curd_par = ret_curd_par
			self.cpoTime = retTime
		status, ret_curd_parntor, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'curd_parntor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.curd_parntor = ret_curd_parntor
			self.cpoTime = retTime
		status, ret_cur_tor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'cur_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.cur_tor = ret_cur_tor
			self.cpoTime = retTime
		status, ret_cur_tor_ntor, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'cur_tor_ntor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.cur_tor_ntor = ret_cur_tor_ntor
			self.cpoTime = retTime
		status, ret_e_plus_ave, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'e_plus_ave', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_plus_ave = ret_e_plus_ave
			self.cpoTime = retTime
		status, ret_e_minus_ave, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'e_minus_ave', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_minus_ave = ret_e_minus_ave
			self.cpoTime = retTime
		status, ret_e_para_ave, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'e_para_ave', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_para_ave = ret_e_para_ave
			self.cpoTime = retTime
		status, ret_k_perp_ave, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'k_perp_ave', inTime, interpolMode)
		check_status(status)
		if not status:
			self.k_perp_ave = ret_k_perp_ave
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructurewaves_profiles_1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, powd_totList = ull.getVect2DDouble(self.idx, path, cpopath + 'powd_tot')
			if len(powd_totList) == 0:
				powd_totList = numpy.resize(powd_totList, (0,nbslice))
			check_status(status)
			status, powd_eList = ull.getVect2DDouble(self.idx, path, cpopath + 'powd_e')
			if len(powd_eList) == 0:
				powd_eList = numpy.resize(powd_eList, (0,nbslice))
			check_status(status)
			status, powd_iList = ull.getVect3DDouble(self.idx, path, cpopath + 'powd_i')
			if len(powd_iList) == 0:
				powd_iList = numpy.resize(powd_iList, (0,0,nbslice))
			check_status(status)
			status, powd_zList = ull.getVect4DDouble(self.idx, path, cpopath + 'powd_z')
			if len(powd_zList) == 0:
				powd_zList = numpy.resize(powd_zList, (0,0,0,nbslice))
			check_status(status)
			status, powd_feList = ull.getVect2DDouble(self.idx, path, cpopath + 'powd_fe')
			if len(powd_feList) == 0:
				powd_feList = numpy.resize(powd_feList, (0,nbslice))
			check_status(status)
			status, powd_fiList = ull.getVect3DDouble(self.idx, path, cpopath + 'powd_fi')
			if len(powd_fiList) == 0:
				powd_fiList = numpy.resize(powd_fiList, (0,0,nbslice))
			check_status(status)
			status, powd_fzList = ull.getVect4DDouble(self.idx, path, cpopath + 'powd_fz')
			if len(powd_fzList) == 0:
				powd_fzList = numpy.resize(powd_fzList, (0,0,0,nbslice))
			check_status(status)
			status, powd_ntorList = ull.getVect3DDouble(self.idx, path, cpopath + 'powd_ntor')
			if len(powd_ntorList) == 0:
				powd_ntorList = numpy.resize(powd_ntorList, (0,0,nbslice))
			check_status(status)
			status, powd_ntor_eList = ull.getVect3DDouble(self.idx, path, cpopath + 'powd_ntor_e')
			if len(powd_ntor_eList) == 0:
				powd_ntor_eList = numpy.resize(powd_ntor_eList, (0,0,nbslice))
			check_status(status)
			status, powd_ntor_iList = ull.getVect4DDouble(self.idx, path, cpopath + 'powd_ntor_i')
			if len(powd_ntor_iList) == 0:
				powd_ntor_iList = numpy.resize(powd_ntor_iList, (0,0,0,nbslice))
			check_status(status)
			status, powd_ntor_zList = ull.getVect5DDouble(self.idx, path, cpopath + 'powd_ntor_z')
			if len(powd_ntor_zList) == 0:
				powd_ntor_zList = numpy.resize(powd_ntor_zList, (0,0,0,0,nbslice))
			check_status(status)
			status, powd_ntor_feList = ull.getVect3DDouble(self.idx, path, cpopath + 'powd_ntor_fe')
			if len(powd_ntor_feList) == 0:
				powd_ntor_feList = numpy.resize(powd_ntor_feList, (0,0,nbslice))
			check_status(status)
			status, powd_ntor_fiList = ull.getVect4DDouble(self.idx, path, cpopath + 'powd_ntor_fi')
			if len(powd_ntor_fiList) == 0:
				powd_ntor_fiList = numpy.resize(powd_ntor_fiList, (0,0,0,nbslice))
			check_status(status)
			status, powd_ntor_fzList = ull.getVect5DDouble(self.idx, path, cpopath + 'powd_ntor_fz')
			if len(powd_ntor_fzList) == 0:
				powd_ntor_fzList = numpy.resize(powd_ntor_fzList, (0,0,0,0,nbslice))
			check_status(status)
			status, curd_torList = ull.getVect2DDouble(self.idx, path, cpopath + 'curd_tor')
			if len(curd_torList) == 0:
				curd_torList = numpy.resize(curd_torList, (0,nbslice))
			check_status(status)
			status, curd_torntorList = ull.getVect3DDouble(self.idx, path, cpopath + 'curd_torntor')
			if len(curd_torntorList) == 0:
				curd_torntorList = numpy.resize(curd_torntorList, (0,0,nbslice))
			check_status(status)
			status, pow_totList = ull.getVect2DDouble(self.idx, path, cpopath + 'pow_tot')
			if len(pow_totList) == 0:
				pow_totList = numpy.resize(pow_totList, (0,nbslice))
			check_status(status)
			status, pow_eList = ull.getVect2DDouble(self.idx, path, cpopath + 'pow_e')
			if len(pow_eList) == 0:
				pow_eList = numpy.resize(pow_eList, (0,nbslice))
			check_status(status)
			status, pow_iList = ull.getVect3DDouble(self.idx, path, cpopath + 'pow_i')
			if len(pow_iList) == 0:
				pow_iList = numpy.resize(pow_iList, (0,0,nbslice))
			check_status(status)
			status, pow_zList = ull.getVect4DDouble(self.idx, path, cpopath + 'pow_z')
			if len(pow_zList) == 0:
				pow_zList = numpy.resize(pow_zList, (0,0,0,nbslice))
			check_status(status)
			status, pow_feList = ull.getVect2DDouble(self.idx, path, cpopath + 'pow_fe')
			if len(pow_feList) == 0:
				pow_feList = numpy.resize(pow_feList, (0,nbslice))
			check_status(status)
			status, pow_fiList = ull.getVect3DDouble(self.idx, path, cpopath + 'pow_fi')
			if len(pow_fiList) == 0:
				pow_fiList = numpy.resize(pow_fiList, (0,0,nbslice))
			check_status(status)
			status, pow_fzList = ull.getVect4DDouble(self.idx, path, cpopath + 'pow_fz')
			if len(pow_fzList) == 0:
				pow_fzList = numpy.resize(pow_fzList, (0,0,0,nbslice))
			check_status(status)
			status, pow_ntorList = ull.getVect3DDouble(self.idx, path, cpopath + 'pow_ntor')
			if len(pow_ntorList) == 0:
				pow_ntorList = numpy.resize(pow_ntorList, (0,0,nbslice))
			check_status(status)
			status, pow_ntor_eList = ull.getVect3DDouble(self.idx, path, cpopath + 'pow_ntor_e')
			if len(pow_ntor_eList) == 0:
				pow_ntor_eList = numpy.resize(pow_ntor_eList, (0,0,nbslice))
			check_status(status)
			status, pow_ntor_iList = ull.getVect4DDouble(self.idx, path, cpopath + 'pow_ntor_i')
			if len(pow_ntor_iList) == 0:
				pow_ntor_iList = numpy.resize(pow_ntor_iList, (0,0,0,nbslice))
			check_status(status)
			status, pow_ntor_zList = ull.getVect4DDouble(self.idx, path, cpopath + 'pow_ntor_z')
			if len(pow_ntor_zList) == 0:
				pow_ntor_zList = numpy.resize(pow_ntor_zList, (0,0,0,nbslice))
			check_status(status)
			status, pow_ntor_feList = ull.getVect3DDouble(self.idx, path, cpopath + 'pow_ntor_fe')
			if len(pow_ntor_feList) == 0:
				pow_ntor_feList = numpy.resize(pow_ntor_feList, (0,0,nbslice))
			check_status(status)
			status, pow_ntor_fiList = ull.getVect4DDouble(self.idx, path, cpopath + 'pow_ntor_fi')
			if len(pow_ntor_fiList) == 0:
				pow_ntor_fiList = numpy.resize(pow_ntor_fiList, (0,0,0,nbslice))
			check_status(status)
			status, pow_ntor_fzList = ull.getVect4DDouble(self.idx, path, cpopath + 'pow_ntor_fz')
			if len(pow_ntor_fzList) == 0:
				pow_ntor_fzList = numpy.resize(pow_ntor_fzList, (0,0,0,nbslice))
			check_status(status)
			status, curd_parList = ull.getVect2DDouble(self.idx, path, cpopath + 'curd_par')
			if len(curd_parList) == 0:
				curd_parList = numpy.resize(curd_parList, (0,nbslice))
			check_status(status)
			status, curd_parntorList = ull.getVect3DDouble(self.idx, path, cpopath + 'curd_parntor')
			if len(curd_parntorList) == 0:
				curd_parntorList = numpy.resize(curd_parntorList, (0,0,nbslice))
			check_status(status)
			status, cur_torList = ull.getVect2DDouble(self.idx, path, cpopath + 'cur_tor')
			if len(cur_torList) == 0:
				cur_torList = numpy.resize(cur_torList, (0,nbslice))
			check_status(status)
			status, cur_tor_ntorList = ull.getVect3DDouble(self.idx, path, cpopath + 'cur_tor_ntor')
			if len(cur_tor_ntorList) == 0:
				cur_tor_ntorList = numpy.resize(cur_tor_ntorList, (0,0,nbslice))
			check_status(status)
			status, e_plus_aveList = ull.getVect3DDouble(self.idx, path, cpopath + 'e_plus_ave')
			if len(e_plus_aveList) == 0:
				e_plus_aveList = numpy.resize(e_plus_aveList, (0,0,nbslice))
			check_status(status)
			status, e_minus_aveList = ull.getVect3DDouble(self.idx, path, cpopath + 'e_minus_ave')
			if len(e_minus_aveList) == 0:
				e_minus_aveList = numpy.resize(e_minus_aveList, (0,0,nbslice))
			check_status(status)
			status, e_para_aveList = ull.getVect3DDouble(self.idx, path, cpopath + 'e_para_ave')
			if len(e_para_aveList) == 0:
				e_para_aveList = numpy.resize(e_para_aveList, (0,0,nbslice))
			check_status(status)
			status, k_perp_aveList = ull.getVect3DDouble(self.idx, path, cpopath + 'k_perp_ave')
			if len(k_perp_aveList) == 0:
				k_perp_aveList = numpy.resize(k_perp_aveList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = profiles_1dstructurewaves_profiles_1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.powd_tot = powd_totList[:,i]
				slice.powd_e = powd_eList[:,i]
				slice.powd_i = powd_iList[:,:,i]
				slice.powd_z = powd_zList[:,:,:,i]
				slice.powd_fe = powd_feList[:,i]
				slice.powd_fi = powd_fiList[:,:,i]
				slice.powd_fz = powd_fzList[:,:,:,i]
				slice.powd_ntor = powd_ntorList[:,:,i]
				slice.powd_ntor_e = powd_ntor_eList[:,:,i]
				slice.powd_ntor_i = powd_ntor_iList[:,:,:,i]
				slice.powd_ntor_z = powd_ntor_zList[:,:,:,:,i]
				slice.powd_ntor_fe = powd_ntor_feList[:,:,i]
				slice.powd_ntor_fi = powd_ntor_fiList[:,:,:,i]
				slice.powd_ntor_fz = powd_ntor_fzList[:,:,:,:,i]
				slice.curd_tor = curd_torList[:,i]
				slice.curd_torntor = curd_torntorList[:,:,i]
				slice.pow_tot = pow_totList[:,i]
				slice.pow_e = pow_eList[:,i]
				slice.pow_i = pow_iList[:,:,i]
				slice.pow_z = pow_zList[:,:,:,i]
				slice.pow_fe = pow_feList[:,i]
				slice.pow_fi = pow_fiList[:,:,i]
				slice.pow_fz = pow_fzList[:,:,:,i]
				slice.pow_ntor = pow_ntorList[:,:,i]
				slice.pow_ntor_e = pow_ntor_eList[:,:,i]
				slice.pow_ntor_i = pow_ntor_iList[:,:,:,i]
				slice.pow_ntor_z = pow_ntor_zList[:,:,:,i]
				slice.pow_ntor_fe = pow_ntor_feList[:,:,i]
				slice.pow_ntor_fi = pow_ntor_fiList[:,:,:,i]
				slice.pow_ntor_fz = pow_ntor_fzList[:,:,:,i]
				slice.curd_par = curd_parList[:,i]
				slice.curd_parntor = curd_parntorList[:,:,i]
				slice.cur_tor = cur_torList[:,i]
				slice.cur_tor_ntor = cur_tor_ntorList[:,:,i]
				slice.e_plus_ave = e_plus_aveList[:,:,i]
				slice.e_minus_ave = e_minus_aveList[:,:,i]
				slice.e_para_ave = e_para_aveList[:,:,i]
				slice.k_perp_ave = k_perp_aveList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructurewaves_profiles_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'powd_tot') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'powd_tot', i, numpy.array(self.powd_tot).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'powd_e') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'powd_e', i, numpy.array(self.powd_e).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'powd_i') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'powd_i', i, numpy.array(self.powd_i).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'powd_z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'powd_z', i, numpy.array(self.powd_z).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'powd_fe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'powd_fe', i, numpy.array(self.powd_fe).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'powd_fi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'powd_fi', i, numpy.array(self.powd_fi).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'powd_fz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'powd_fz', i, numpy.array(self.powd_fz).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'powd_ntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor', i, numpy.array(self.powd_ntor).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'powd_ntor_e') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor_e', i, numpy.array(self.powd_ntor_e).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'powd_ntor_i') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor_i', i, numpy.array(self.powd_ntor_i).astype(numpy.float64))
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'powd_ntor_z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor_z', i, numpy.array(self.powd_ntor_z).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'powd_ntor_fe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor_fe', i, numpy.array(self.powd_ntor_fe).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'powd_ntor_fi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor_fi', i, numpy.array(self.powd_ntor_fi).astype(numpy.float64))
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'powd_ntor_fz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor_fz', i, numpy.array(self.powd_ntor_fz).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'curd_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'curd_tor', i, numpy.array(self.curd_tor).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'curd_torntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'curd_torntor', i, numpy.array(self.curd_torntor).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pow_tot') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pow_tot', i, numpy.array(self.pow_tot).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pow_e') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pow_e', i, numpy.array(self.pow_e).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pow_i') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pow_i', i, numpy.array(self.pow_i).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'pow_z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'pow_z', i, numpy.array(self.pow_z).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pow_fe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pow_fe', i, numpy.array(self.pow_fe).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pow_fi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pow_fi', i, numpy.array(self.pow_fi).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'pow_fz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'pow_fz', i, numpy.array(self.pow_fz).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pow_ntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor', i, numpy.array(self.pow_ntor).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pow_ntor_e') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor_e', i, numpy.array(self.pow_ntor_e).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'pow_ntor_i') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor_i', i, numpy.array(self.pow_ntor_i).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'pow_ntor_z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor_z', i, numpy.array(self.pow_ntor_z).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pow_ntor_fe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor_fe', i, numpy.array(self.pow_ntor_fe).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'pow_ntor_fi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor_fi', i, numpy.array(self.pow_ntor_fi).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'pow_ntor_fz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'pow_ntor_fz', i, numpy.array(self.pow_ntor_fz).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'curd_par') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'curd_par', i, numpy.array(self.curd_par).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'curd_parntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'curd_parntor', i, numpy.array(self.curd_parntor).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'cur_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'cur_tor', i, numpy.array(self.cur_tor).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'cur_tor_ntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'cur_tor_ntor', i, numpy.array(self.cur_tor_ntor).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'e_plus_ave') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'e_plus_ave', i, numpy.array(self.e_plus_ave).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'e_minus_ave') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'e_minus_ave', i, numpy.array(self.e_minus_ave).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'e_para_ave') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'e_para_ave', i, numpy.array(self.e_para_ave).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'k_perp_ave') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'k_perp_ave', i, numpy.array(self.k_perp_ave).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructurewaves_profiles_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'powd_tot') 
			print ('obj = ' + str(obj))
		status, ret_powd_tot = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'powd_tot', i)
		check_status(status)
		if not status:
			self.powd_tot = ret_powd_tot
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'powd_e') 
			print ('obj = ' + str(obj))
		status, ret_powd_e = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'powd_e', i)
		check_status(status)
		if not status:
			self.powd_e = ret_powd_e
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'powd_i') 
			print ('obj = ' + str(obj))
		status, ret_powd_i = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'powd_i', i)
		check_status(status)
		if not status:
			self.powd_i = ret_powd_i
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'powd_z') 
			print ('obj = ' + str(obj))
		status, ret_powd_z = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'powd_z', i)
		check_status(status)
		if not status:
			self.powd_z = ret_powd_z
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'powd_fe') 
			print ('obj = ' + str(obj))
		status, ret_powd_fe = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'powd_fe', i)
		check_status(status)
		if not status:
			self.powd_fe = ret_powd_fe
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'powd_fi') 
			print ('obj = ' + str(obj))
		status, ret_powd_fi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'powd_fi', i)
		check_status(status)
		if not status:
			self.powd_fi = ret_powd_fi
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'powd_fz') 
			print ('obj = ' + str(obj))
		status, ret_powd_fz = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'powd_fz', i)
		check_status(status)
		if not status:
			self.powd_fz = ret_powd_fz
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'powd_ntor') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor', i)
		check_status(status)
		if not status:
			self.powd_ntor = ret_powd_ntor
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'powd_ntor_e') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor_e = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor_e', i)
		check_status(status)
		if not status:
			self.powd_ntor_e = ret_powd_ntor_e
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'powd_ntor_i') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor_i = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor_i', i)
		check_status(status)
		if not status:
			self.powd_ntor_i = ret_powd_ntor_i
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'powd_ntor_z') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor_z = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor_z', i)
		check_status(status)
		if not status:
			self.powd_ntor_z = ret_powd_ntor_z
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'powd_ntor_fe') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor_fe = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor_fe', i)
		check_status(status)
		if not status:
			self.powd_ntor_fe = ret_powd_ntor_fe
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'powd_ntor_fi') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor_fi = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor_fi', i)
		check_status(status)
		if not status:
			self.powd_ntor_fi = ret_powd_ntor_fi
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'powd_ntor_fz') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor_fz = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor_fz', i)
		check_status(status)
		if not status:
			self.powd_ntor_fz = ret_powd_ntor_fz
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'curd_tor') 
			print ('obj = ' + str(obj))
		status, ret_curd_tor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'curd_tor', i)
		check_status(status)
		if not status:
			self.curd_tor = ret_curd_tor
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'curd_torntor') 
			print ('obj = ' + str(obj))
		status, ret_curd_torntor = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'curd_torntor', i)
		check_status(status)
		if not status:
			self.curd_torntor = ret_curd_torntor
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pow_tot') 
			print ('obj = ' + str(obj))
		status, ret_pow_tot = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pow_tot', i)
		check_status(status)
		if not status:
			self.pow_tot = ret_pow_tot
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pow_e') 
			print ('obj = ' + str(obj))
		status, ret_pow_e = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pow_e', i)
		check_status(status)
		if not status:
			self.pow_e = ret_pow_e
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pow_i') 
			print ('obj = ' + str(obj))
		status, ret_pow_i = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pow_i', i)
		check_status(status)
		if not status:
			self.pow_i = ret_pow_i
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'pow_z') 
			print ('obj = ' + str(obj))
		status, ret_pow_z = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'pow_z', i)
		check_status(status)
		if not status:
			self.pow_z = ret_pow_z
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pow_fe') 
			print ('obj = ' + str(obj))
		status, ret_pow_fe = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pow_fe', i)
		check_status(status)
		if not status:
			self.pow_fe = ret_pow_fe
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pow_fi') 
			print ('obj = ' + str(obj))
		status, ret_pow_fi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pow_fi', i)
		check_status(status)
		if not status:
			self.pow_fi = ret_pow_fi
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'pow_fz') 
			print ('obj = ' + str(obj))
		status, ret_pow_fz = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'pow_fz', i)
		check_status(status)
		if not status:
			self.pow_fz = ret_pow_fz
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pow_ntor') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor', i)
		check_status(status)
		if not status:
			self.pow_ntor = ret_pow_ntor
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pow_ntor_e') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor_e = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor_e', i)
		check_status(status)
		if not status:
			self.pow_ntor_e = ret_pow_ntor_e
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'pow_ntor_i') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor_i = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor_i', i)
		check_status(status)
		if not status:
			self.pow_ntor_i = ret_pow_ntor_i
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'pow_ntor_z') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor_z = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor_z', i)
		check_status(status)
		if not status:
			self.pow_ntor_z = ret_pow_ntor_z
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pow_ntor_fe') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor_fe = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor_fe', i)
		check_status(status)
		if not status:
			self.pow_ntor_fe = ret_pow_ntor_fe
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'pow_ntor_fi') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor_fi = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor_fi', i)
		check_status(status)
		if not status:
			self.pow_ntor_fi = ret_pow_ntor_fi
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'pow_ntor_fz') 
			print ('obj = ' + str(obj))
		status, ret_pow_ntor_fz = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'pow_ntor_fz', i)
		check_status(status)
		if not status:
			self.pow_ntor_fz = ret_pow_ntor_fz
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'curd_par') 
			print ('obj = ' + str(obj))
		status, ret_curd_par = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'curd_par', i)
		check_status(status)
		if not status:
			self.curd_par = ret_curd_par
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'curd_parntor') 
			print ('obj = ' + str(obj))
		status, ret_curd_parntor = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'curd_parntor', i)
		check_status(status)
		if not status:
			self.curd_parntor = ret_curd_parntor
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'cur_tor') 
			print ('obj = ' + str(obj))
		status, ret_cur_tor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'cur_tor', i)
		check_status(status)
		if not status:
			self.cur_tor = ret_cur_tor
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'cur_tor_ntor') 
			print ('obj = ' + str(obj))
		status, ret_cur_tor_ntor = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'cur_tor_ntor', i)
		check_status(status)
		if not status:
			self.cur_tor_ntor = ret_cur_tor_ntor
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'e_plus_ave') 
			print ('obj = ' + str(obj))
		status, ret_e_plus_ave = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'e_plus_ave', i)
		check_status(status)
		if not status:
			self.e_plus_ave = ret_e_plus_ave
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'e_minus_ave') 
			print ('obj = ' + str(obj))
		status, ret_e_minus_ave = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'e_minus_ave', i)
		check_status(status)
		if not status:
			self.e_minus_ave = ret_e_minus_ave
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'e_para_ave') 
			print ('obj = ' + str(obj))
		status, ret_e_para_ave = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'e_para_ave', i)
		check_status(status)
		if not status:
			self.e_para_ave = ret_e_para_ave
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'k_perp_ave') 
			print ('obj = ' + str(obj))
		status, ret_k_perp_ave = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'k_perp_ave', i)
		check_status(status)
		if not status:
			self.k_perp_ave = ret_k_perp_ave

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructurewaves_profiles_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructurewaves_profiles_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'powd_tot')
		ull.deleteData(self.idx, path, cpopath + 'powd_e')
		ull.deleteData(self.idx, path, cpopath + 'powd_i')
		ull.deleteData(self.idx, path, cpopath + 'powd_z')
		ull.deleteData(self.idx, path, cpopath + 'powd_fe')
		ull.deleteData(self.idx, path, cpopath + 'powd_fi')
		ull.deleteData(self.idx, path, cpopath + 'powd_fz')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor_e')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor_i')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor_z')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor_fe')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor_fi')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor_fz')
		ull.deleteData(self.idx, path, cpopath + 'curd_tor')
		ull.deleteData(self.idx, path, cpopath + 'curd_torntor')
		ull.deleteData(self.idx, path, cpopath + 'pow_tot')
		ull.deleteData(self.idx, path, cpopath + 'pow_e')
		ull.deleteData(self.idx, path, cpopath + 'pow_i')
		ull.deleteData(self.idx, path, cpopath + 'pow_z')
		ull.deleteData(self.idx, path, cpopath + 'pow_fe')
		ull.deleteData(self.idx, path, cpopath + 'pow_fi')
		ull.deleteData(self.idx, path, cpopath + 'pow_fz')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor_e')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor_i')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor_z')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor_fe')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor_fi')
		ull.deleteData(self.idx, path, cpopath + 'pow_ntor_fz')
		ull.deleteData(self.idx, path, cpopath + 'curd_par')
		ull.deleteData(self.idx, path, cpopath + 'curd_parntor')
		ull.deleteData(self.idx, path, cpopath + 'cur_tor')
		ull.deleteData(self.idx, path, cpopath + 'cur_tor_ntor')
		ull.deleteData(self.idx, path, cpopath + 'e_plus_ave')
		ull.deleteData(self.idx, path, cpopath + 'e_minus_ave')
		ull.deleteData(self.idx, path, cpopath + 'e_para_ave')
		ull.deleteData(self.idx, path, cpopath + 'k_perp_ave')


class profiles_2dstructurewaves_profiles_2d(KeepInOrder):
	'''
	class profiles_2dstructurewaves_profiles_2d
	2D profiles in poloidal cross-section

	Attributes:
	- powd_tot : numpy.ndarray 2D with float
	   Total wave power density; Time-dependent [W/m^3]; Matrix (ndim1, ndim2)
	- powd_e : numpy.ndarray 2D with float
	   Absorbed wave power density on the thermal electrons [W/m^3]; Time-dependent; Matrix (ndim1, ndim2)
	- powd_i : numpy.ndarray 3D with float
	   Absorbed wave power density on each thermal ion species [W/m^3]; Time-dependent; Array3D (ndim1, ndim2, nion)
	- powd_z : numpy.ndarray 4D with float
	   Absorbed wave power density on each thermal impurity species [W/m^3]; Time-dependent; Array3D (ndim1, ndim2, nimpur, nzimp)
	- powd_fe : numpy.ndarray 2D with float
	   Absorbed wave power density on the fast electrons [W/m^3]; Time-dependent; Matrix (ndim1, ndim2)
	- powd_fi : numpy.ndarray 3D with float
	   Absorbed wave power density on each fast ion species [W/m^3]; Time-dependent; Array3D (ndim1, ndim2, nion)
	- powd_fz : numpy.ndarray 4D with float
	   Absorbed wave power density on each fast impurity species [W/m^3]; Time-dependent; Array3D (ndim1, ndim2, nimpur, nzimp)
	- powd_ntor : numpy.ndarray 3D with float
	   Absorbed power density for each toroidal mode number [W/m^3]; Time-dependent; Array 3D (ndim1, ndim2, ntor)
	- powd_ntor_e : numpy.ndarray 3D with float
	   Absorbed power density for each toroidal mode number on the thermal electrons [W/m^3]; Time-dependent; Array 3D (ndim1, ndim2, ntor)
	- powd_ntor_i : numpy.ndarray 4D with float
	   Absorbed power density for each toroidal mode number on each thermal ions species [W/m^3]; Time-dependent; Array4D (ndim1, ndim2, ntor, nion)
	- powd_ntor_z : numpy.ndarray 5D with float
	   Absorbed power density for each toroidal mode number on each thermal impurity species [W/m^3]; Time-dependent; Array4D (ndim1, ndim2, ntor, nimpur, nzimp)
	- powd_ntor_fe : numpy.ndarray 3D with float
	   Absorbed power density for each toroidal mode number on the fast electrons [W/m^3]; Time-dependent; Array 3D (ndim1, ndim2, ntor)
	- powd_ntor_fi : numpy.ndarray 4D with float
	   Absorbed power density for each toroidal mode number on each fast ions species [W/m^3]; Time-dependent; Array4D (ndim1, ndim2, ntor, nion)
	- powd_ntor_fz : numpy.ndarray 5D with float
	   Absorbed power density for each toroidal mode number on each fast impurity species [W/m^3]; Time-dependent; Array4D (ndim1, ndim2, ntor, nimpur, nzimp)
	- powd_iharm : numpy.ndarray 5D with float
	   Power density absorbed by an ion species for each toroidal mode numer at a given harmonic cyclotron resonance ; Time-dependent (W/m^3); Array5D (ndim1, ndim2, ntor, nion, nharm)
	'''

	def __init__(self, base_path_in='profiles_2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.powd_tot = numpy.zeros((0,0), numpy.float64, order='C')
		self.powd_e = numpy.zeros((0,0), numpy.float64, order='C')
		self.powd_i = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.powd_z = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.powd_fe = numpy.zeros((0,0), numpy.float64, order='C')
		self.powd_fi = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.powd_fz = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.powd_ntor = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.powd_ntor_e = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.powd_ntor_i = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.powd_ntor_z = numpy.zeros((0,0,0,0,0), numpy.float64, order='C')
		self.powd_ntor_fe = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.powd_ntor_fi = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.powd_ntor_fz = numpy.zeros((0,0,0,0,0), numpy.float64, order='C')
		self.powd_iharm = numpy.zeros((0,0,0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class profiles_2dstructurewaves_profiles_2d\n'
		s = self.powd_tot.__str__()
		ret = ret + space + 'Attribute powd_tot\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_e.__str__()
		ret = ret + space + 'Attribute powd_e\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_i.__str__()
		ret = ret + space + 'Attribute powd_i\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_z.__str__()
		ret = ret + space + 'Attribute powd_z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_fe.__str__()
		ret = ret + space + 'Attribute powd_fe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_fi.__str__()
		ret = ret + space + 'Attribute powd_fi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_fz.__str__()
		ret = ret + space + 'Attribute powd_fz\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor.__str__()
		ret = ret + space + 'Attribute powd_ntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor_e.__str__()
		ret = ret + space + 'Attribute powd_ntor_e\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor_i.__str__()
		ret = ret + space + 'Attribute powd_ntor_i\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor_z.__str__()
		ret = ret + space + 'Attribute powd_ntor_z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor_fe.__str__()
		ret = ret + space + 'Attribute powd_ntor_fe\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor_fi.__str__()
		ret = ret + space + 'Attribute powd_ntor_fi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_ntor_fz.__str__()
		ret = ret + space + 'Attribute powd_ntor_fz\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.powd_iharm.__str__()
		ret = ret + space + 'Attribute powd_iharm\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructurewaves_profiles_2d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'powd_tot', numpy.array(self.powd_tot).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'powd_e', numpy.array(self.powd_e).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'powd_i', numpy.array(self.powd_i).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect4DDoubleSlice(self.idx, path, cpopath + 'powd_z', numpy.array(self.powd_z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'powd_fe', numpy.array(self.powd_fe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'powd_fi', numpy.array(self.powd_fi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect4DDoubleSlice(self.idx, path, cpopath + 'powd_fz', numpy.array(self.powd_fz).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor', numpy.array(self.powd_ntor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_e', numpy.array(self.powd_ntor_e).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect4DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_i', numpy.array(self.powd_ntor_i).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect5DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_z', numpy.array(self.powd_ntor_z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fe', numpy.array(self.powd_ntor_fe).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect4DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fi', numpy.array(self.powd_ntor_fi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect5DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fz', numpy.array(self.powd_ntor_fz).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect5DDoubleSlice(self.idx, path, cpopath + 'powd_iharm', numpy.array(self.powd_iharm).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructurewaves_profiles_2d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'powd_tot', numpy.array(self.powd_tot).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'powd_e', numpy.array(self.powd_e).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'powd_i', numpy.array(self.powd_i).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect4DDoubleSlice(self.idx, path, cpopath + 'powd_z', numpy.array(self.powd_z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'powd_fe', numpy.array(self.powd_fe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'powd_fi', numpy.array(self.powd_fi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect4DDoubleSlice(self.idx, path, cpopath + 'powd_fz', numpy.array(self.powd_fz).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor', numpy.array(self.powd_ntor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_e', numpy.array(self.powd_ntor_e).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect4DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_i', numpy.array(self.powd_ntor_i).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect5DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_z', numpy.array(self.powd_ntor_z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fe', numpy.array(self.powd_ntor_fe).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect4DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fi', numpy.array(self.powd_ntor_fi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect5DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fz', numpy.array(self.powd_ntor_fz).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect5DDoubleSlice(self.idx, path, cpopath + 'powd_iharm', numpy.array(self.powd_iharm).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructurewaves_profiles_2d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructurewaves_profiles_2d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_powd_tot, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'powd_tot', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_tot = ret_powd_tot
			self.cpoTime = retTime
		status, ret_powd_e, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'powd_e', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_e = ret_powd_e
			self.cpoTime = retTime
		status, ret_powd_i, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'powd_i', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_i = ret_powd_i
			self.cpoTime = retTime
		status, ret_powd_z, retTime = ull.getVect4DDoubleSlice(self.idx, path, cpopath + 'powd_z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_z = ret_powd_z
			self.cpoTime = retTime
		status, ret_powd_fe, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'powd_fe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_fe = ret_powd_fe
			self.cpoTime = retTime
		status, ret_powd_fi, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'powd_fi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_fi = ret_powd_fi
			self.cpoTime = retTime
		status, ret_powd_fz, retTime = ull.getVect4DDoubleSlice(self.idx, path, cpopath + 'powd_fz', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_fz = ret_powd_fz
			self.cpoTime = retTime
		status, ret_powd_ntor, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor = ret_powd_ntor
			self.cpoTime = retTime
		status, ret_powd_ntor_e, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_e', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor_e = ret_powd_ntor_e
			self.cpoTime = retTime
		status, ret_powd_ntor_i, retTime = ull.getVect4DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_i', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor_i = ret_powd_ntor_i
			self.cpoTime = retTime
		status, ret_powd_ntor_z, retTime = ull.getVect5DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor_z = ret_powd_ntor_z
			self.cpoTime = retTime
		status, ret_powd_ntor_fe, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fe', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor_fe = ret_powd_ntor_fe
			self.cpoTime = retTime
		status, ret_powd_ntor_fi, retTime = ull.getVect4DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor_fi = ret_powd_ntor_fi
			self.cpoTime = retTime
		status, ret_powd_ntor_fz, retTime = ull.getVect5DDoubleSlice(self.idx, path, cpopath + 'powd_ntor_fz', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_ntor_fz = ret_powd_ntor_fz
			self.cpoTime = retTime
		status, ret_powd_iharm, retTime = ull.getVect5DDoubleSlice(self.idx, path, cpopath + 'powd_iharm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.powd_iharm = ret_powd_iharm
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructurewaves_profiles_2d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, powd_totList = ull.getVect3DDouble(self.idx, path, cpopath + 'powd_tot')
			if len(powd_totList) == 0:
				powd_totList = numpy.resize(powd_totList, (0,0,nbslice))
			check_status(status)
			status, powd_eList = ull.getVect3DDouble(self.idx, path, cpopath + 'powd_e')
			if len(powd_eList) == 0:
				powd_eList = numpy.resize(powd_eList, (0,0,nbslice))
			check_status(status)
			status, powd_iList = ull.getVect4DDouble(self.idx, path, cpopath + 'powd_i')
			if len(powd_iList) == 0:
				powd_iList = numpy.resize(powd_iList, (0,0,0,nbslice))
			check_status(status)
			status, powd_zList = ull.getVect5DDouble(self.idx, path, cpopath + 'powd_z')
			if len(powd_zList) == 0:
				powd_zList = numpy.resize(powd_zList, (0,0,0,0,nbslice))
			check_status(status)
			status, powd_feList = ull.getVect3DDouble(self.idx, path, cpopath + 'powd_fe')
			if len(powd_feList) == 0:
				powd_feList = numpy.resize(powd_feList, (0,0,nbslice))
			check_status(status)
			status, powd_fiList = ull.getVect4DDouble(self.idx, path, cpopath + 'powd_fi')
			if len(powd_fiList) == 0:
				powd_fiList = numpy.resize(powd_fiList, (0,0,0,nbslice))
			check_status(status)
			status, powd_fzList = ull.getVect5DDouble(self.idx, path, cpopath + 'powd_fz')
			if len(powd_fzList) == 0:
				powd_fzList = numpy.resize(powd_fzList, (0,0,0,0,nbslice))
			check_status(status)
			status, powd_ntorList = ull.getVect4DDouble(self.idx, path, cpopath + 'powd_ntor')
			if len(powd_ntorList) == 0:
				powd_ntorList = numpy.resize(powd_ntorList, (0,0,0,nbslice))
			check_status(status)
			status, powd_ntor_eList = ull.getVect4DDouble(self.idx, path, cpopath + 'powd_ntor_e')
			if len(powd_ntor_eList) == 0:
				powd_ntor_eList = numpy.resize(powd_ntor_eList, (0,0,0,nbslice))
			check_status(status)
			status, powd_ntor_iList = ull.getVect5DDouble(self.idx, path, cpopath + 'powd_ntor_i')
			if len(powd_ntor_iList) == 0:
				powd_ntor_iList = numpy.resize(powd_ntor_iList, (0,0,0,0,nbslice))
			check_status(status)
			status, powd_ntor_zList = ull.getVect6DDouble(self.idx, path, cpopath + 'powd_ntor_z')
			if len(powd_ntor_zList) == 0:
				powd_ntor_zList = numpy.resize(powd_ntor_zList, (0,0,0,0,0,nbslice))
			check_status(status)
			status, powd_ntor_feList = ull.getVect4DDouble(self.idx, path, cpopath + 'powd_ntor_fe')
			if len(powd_ntor_feList) == 0:
				powd_ntor_feList = numpy.resize(powd_ntor_feList, (0,0,0,nbslice))
			check_status(status)
			status, powd_ntor_fiList = ull.getVect5DDouble(self.idx, path, cpopath + 'powd_ntor_fi')
			if len(powd_ntor_fiList) == 0:
				powd_ntor_fiList = numpy.resize(powd_ntor_fiList, (0,0,0,0,nbslice))
			check_status(status)
			status, powd_ntor_fzList = ull.getVect6DDouble(self.idx, path, cpopath + 'powd_ntor_fz')
			if len(powd_ntor_fzList) == 0:
				powd_ntor_fzList = numpy.resize(powd_ntor_fzList, (0,0,0,0,0,nbslice))
			check_status(status)
			status, powd_iharmList = ull.getVect6DDouble(self.idx, path, cpopath + 'powd_iharm')
			if len(powd_iharmList) == 0:
				powd_iharmList = numpy.resize(powd_iharmList, (0,0,0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = profiles_2dstructurewaves_profiles_2d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.powd_tot = powd_totList[:,:,i]
				slice.powd_e = powd_eList[:,:,i]
				slice.powd_i = powd_iList[:,:,:,i]
				slice.powd_z = powd_zList[:,:,:,:,i]
				slice.powd_fe = powd_feList[:,:,i]
				slice.powd_fi = powd_fiList[:,:,:,i]
				slice.powd_fz = powd_fzList[:,:,:,:,i]
				slice.powd_ntor = powd_ntorList[:,:,:,i]
				slice.powd_ntor_e = powd_ntor_eList[:,:,:,i]
				slice.powd_ntor_i = powd_ntor_iList[:,:,:,:,i]
				slice.powd_ntor_z = powd_ntor_zList[:,:,:,:,:,i]
				slice.powd_ntor_fe = powd_ntor_feList[:,:,:,i]
				slice.powd_ntor_fi = powd_ntor_fiList[:,:,:,:,i]
				slice.powd_ntor_fz = powd_ntor_fzList[:,:,:,:,:,i]
				slice.powd_iharm = powd_iharmList[:,:,:,:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstructurewaves_profiles_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'powd_tot') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'powd_tot', i, numpy.array(self.powd_tot).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'powd_e') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'powd_e', i, numpy.array(self.powd_e).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'powd_i') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'powd_i', i, numpy.array(self.powd_i).astype(numpy.float64))
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'powd_z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'powd_z', i, numpy.array(self.powd_z).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'powd_fe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'powd_fe', i, numpy.array(self.powd_fe).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'powd_fi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'powd_fi', i, numpy.array(self.powd_fi).astype(numpy.float64))
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'powd_fz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'powd_fz', i, numpy.array(self.powd_fz).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'powd_ntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor', i, numpy.array(self.powd_ntor).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'powd_ntor_e') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor_e', i, numpy.array(self.powd_ntor_e).astype(numpy.float64))
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'powd_ntor_i') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor_i', i, numpy.array(self.powd_ntor_i).astype(numpy.float64))
		if (dev()):
			print ('putVect5DDoubleInObject : ' + cpopath + 'powd_ntor_z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect5DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor_z', i, numpy.array(self.powd_ntor_z).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'powd_ntor_fe') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor_fe', i, numpy.array(self.powd_ntor_fe).astype(numpy.float64))
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'powd_ntor_fi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor_fi', i, numpy.array(self.powd_ntor_fi).astype(numpy.float64))
		if (dev()):
			print ('putVect5DDoubleInObject : ' + cpopath + 'powd_ntor_fz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect5DDoubleInObject(self.idx, obj, cpopath + 'powd_ntor_fz', i, numpy.array(self.powd_ntor_fz).astype(numpy.float64))
		if (dev()):
			print ('putVect5DDoubleInObject : ' + cpopath + 'powd_iharm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect5DDoubleInObject(self.idx, obj, cpopath + 'powd_iharm', i, numpy.array(self.powd_iharm).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstructurewaves_profiles_2dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'powd_tot') 
			print ('obj = ' + str(obj))
		status, ret_powd_tot = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'powd_tot', i)
		check_status(status)
		if not status:
			self.powd_tot = ret_powd_tot
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'powd_e') 
			print ('obj = ' + str(obj))
		status, ret_powd_e = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'powd_e', i)
		check_status(status)
		if not status:
			self.powd_e = ret_powd_e
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'powd_i') 
			print ('obj = ' + str(obj))
		status, ret_powd_i = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'powd_i', i)
		check_status(status)
		if not status:
			self.powd_i = ret_powd_i
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'powd_z') 
			print ('obj = ' + str(obj))
		status, ret_powd_z = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'powd_z', i)
		check_status(status)
		if not status:
			self.powd_z = ret_powd_z
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'powd_fe') 
			print ('obj = ' + str(obj))
		status, ret_powd_fe = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'powd_fe', i)
		check_status(status)
		if not status:
			self.powd_fe = ret_powd_fe
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'powd_fi') 
			print ('obj = ' + str(obj))
		status, ret_powd_fi = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'powd_fi', i)
		check_status(status)
		if not status:
			self.powd_fi = ret_powd_fi
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'powd_fz') 
			print ('obj = ' + str(obj))
		status, ret_powd_fz = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'powd_fz', i)
		check_status(status)
		if not status:
			self.powd_fz = ret_powd_fz
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'powd_ntor') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor', i)
		check_status(status)
		if not status:
			self.powd_ntor = ret_powd_ntor
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'powd_ntor_e') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor_e = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor_e', i)
		check_status(status)
		if not status:
			self.powd_ntor_e = ret_powd_ntor_e
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'powd_ntor_i') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor_i = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor_i', i)
		check_status(status)
		if not status:
			self.powd_ntor_i = ret_powd_ntor_i
		if (dev()):
			print ('getVect5DDoubleInObject : ' + cpopath + 'powd_ntor_z') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor_z = ull.getVect5DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor_z', i)
		check_status(status)
		if not status:
			self.powd_ntor_z = ret_powd_ntor_z
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'powd_ntor_fe') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor_fe = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor_fe', i)
		check_status(status)
		if not status:
			self.powd_ntor_fe = ret_powd_ntor_fe
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'powd_ntor_fi') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor_fi = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor_fi', i)
		check_status(status)
		if not status:
			self.powd_ntor_fi = ret_powd_ntor_fi
		if (dev()):
			print ('getVect5DDoubleInObject : ' + cpopath + 'powd_ntor_fz') 
			print ('obj = ' + str(obj))
		status, ret_powd_ntor_fz = ull.getVect5DDoubleFromObject(self.idx, obj, cpopath + 'powd_ntor_fz', i)
		check_status(status)
		if not status:
			self.powd_ntor_fz = ret_powd_ntor_fz
		if (dev()):
			print ('getVect5DDoubleInObject : ' + cpopath + 'powd_iharm') 
			print ('obj = ' + str(obj))
		status, ret_powd_iharm = ull.getVect5DDoubleFromObject(self.idx, obj, cpopath + 'powd_iharm', i)
		check_status(status)
		if not status:
			self.powd_iharm = ret_powd_iharm

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstructurewaves_profiles_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstructurewaves_profiles_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'powd_tot')
		ull.deleteData(self.idx, path, cpopath + 'powd_e')
		ull.deleteData(self.idx, path, cpopath + 'powd_i')
		ull.deleteData(self.idx, path, cpopath + 'powd_z')
		ull.deleteData(self.idx, path, cpopath + 'powd_fe')
		ull.deleteData(self.idx, path, cpopath + 'powd_fi')
		ull.deleteData(self.idx, path, cpopath + 'powd_fz')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor_e')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor_i')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor_z')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor_fe')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor_fi')
		ull.deleteData(self.idx, path, cpopath + 'powd_ntor_fz')
		ull.deleteData(self.idx, path, cpopath + 'powd_iharm')


class beamtracingstruct_arraybeamtracing:
	'''
	class beamtracingstruct_arraybeamtracing
	Beam-tracing or ray-tracing solver. Vector(nbeams). Time-dependent

	Attributes:
	- array : list of beamtracingstruct_arraybeamtracingObj 
	'''

	def __init__(self, base_path_in='beamtracing'):
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
		ret = space + 'class beamtracingstruct_arraybeamtracing\n'
		for i in range(len(self.array)):
			ret = ret + space + 'beamtracingstruct_arraybeamtracing[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(beamtracingstruct_arraybeamtracingObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type beamtracingstruct_arraybeamtracing, run function putSlice') 
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
			print ('field '+self.base_path+' of type beamtracingstruct_arraybeamtracing, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type beamtracingstruct_arraybeamtracing, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type beamtracingstruct_arraybeamtracing, run function getSlice') 
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
			print ('field '+self.base_path+' of type beamtracingstruct_arraybeamtracing, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(beamtracingstruct_arraybeamtracing(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(beamtracingstruct_arraybeamtracing(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = beamtracingstruct_arraybeamtracing(self.base_path)
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
			print ('field '+self.base_path+' of type beamtracingstruct_arraybeamtracing, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type beamtracingstruct_arraybeamtracing, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type beamtracingstruct_arraybeamtracing, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type beamtracingstruct_arraybeamtracing, run function getNonTimedElt') 
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


class beamtracingstruct_arraybeamtracingObj(KeepInOrder):
	'''
	class beamtracingstruct_arraybeamtracingObj
	Beam-tracing or ray-tracing solver. Vector(nbeams). Time-dependent

	Attributes:
	- npoints : int
	   Number of points along each ray/beam. Integer
	- power : float
	   Initial power in each ray/beam [W]. Float. Time-dependent
	- dnpar : numpy.ndarray 1D with float
	   Spectral width in refractive index associated with each ray/beam, Vector (npoints). Time-dependent
	- length : numpy.ndarray 1D with float
	   Ray/beam curvilinear length [m], Vector (npoints). Time-dependent
	- position : class positionstructurewaves_rtposition
	   Ray/beam position
	- wavevector : class wavevectorstructurewaves_rtwavevector
	   Ray/beam wave vector.
	- polarization : class polarizationstructurepolarization
	   Wave field polarization along the ray/beam.
	- powerflow : class powerflowstructurepowerflow
	   Power flow along the ray/beam.
	'''

	def __init__(self, base_path_in='beamtracing'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.npoints = EMPTY_INT
		self.power = EMPTY_DOUBLE
		self.dnpar = numpy.zeros(0, numpy.float64, order='C')
		self.length = numpy.zeros(0, numpy.float64, order='C')
		self.position = positionstructurewaves_rtposition('position')
		self.wavevector = wavevectorstructurewaves_rtwavevector('wavevector')
		self.polarization = polarizationstructurepolarization('polarization')
		self.powerflow = powerflowstructurepowerflow('powerflow')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class beamtracingstruct_arraybeamtracingObj\n'
		ret = ret + space + 'Attribute npoints: ' + str(self.npoints) + '\n'
		ret = ret + space + 'Attribute power: ' + str(self.power) + '\n'
		s = self.dnpar.__str__()
		ret = ret + space + 'Attribute dnpar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.length.__str__()
		ret = ret + space + 'Attribute length\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		ret = ret + space + 'Attribute wavevector\n ' + self.wavevector.__str__(depth+1)
		ret = ret + space + 'Attribute polarization\n ' + self.polarization.__str__(depth+1)
		ret = ret + space + 'Attribute powerflow\n ' + self.powerflow.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.position.setExpIdx(idx)
		self.wavevector.setExpIdx(idx)
		self.polarization.setExpIdx(idx)
		self.powerflow.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamtracingstruct_arraybeamtracingObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'power') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'power', i, self.power)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dnpar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dnpar', i, numpy.array(self.dnpar).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'length') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'length', i, numpy.array(self.length).astype(numpy.float64))
		obj = self.position.putTimedElt(path, cpopath + 'position', i, obj)
		obj = self.wavevector.putTimedElt(path, cpopath + 'wavevector', i, obj)
		obj = self.polarization.putTimedElt(path, cpopath + 'polarization', i, obj)
		obj = self.powerflow.putTimedElt(path, cpopath + 'powerflow', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamtracingstruct_arraybeamtracingObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'power') 
			print ('obj = ' + str(obj))
		status, ret_power = ull.getDoubleFromObject(self.idx, obj, cpopath + 'power', i)
		check_status(status)
		if not status:
			self.power = ret_power
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dnpar') 
			print ('obj = ' + str(obj))
		status, ret_dnpar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dnpar', i)
		check_status(status)
		if not status:
			self.dnpar = ret_dnpar
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'length') 
			print ('obj = ' + str(obj))
		status, ret_length = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'length', i)
		check_status(status)
		if not status:
			self.length = ret_length
		self.position.getTimedElt(path, cpopath + 'position', i, obj)
		self.wavevector.getTimedElt(path, cpopath + 'wavevector', i, obj)
		self.polarization.getTimedElt(path, cpopath + 'polarization', i, obj)
		self.powerflow.getTimedElt(path, cpopath + 'powerflow', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamtracingstruct_arraybeamtracingObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'npoints') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'npoints', i, self.npoints)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		obj = self.wavevector.putNonTimedElt(path, cpopath + 'wavevector', i, obj)
		obj = self.polarization.putNonTimedElt(path, cpopath + 'polarization', i, obj)
		obj = self.powerflow.putNonTimedElt(path, cpopath + 'powerflow', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type beamtracingstruct_arraybeamtracingObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'npoints') 
			print ('obj = ' + str(obj))
		status, ret_npoints = ull.getIntFromObject(self.idx, obj, cpopath + 'npoints', i)
		check_status(status)
		if not status:
			self.npoints = ret_npoints
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		self.wavevector.getNonTimedElt(path, cpopath + 'wavevector', i, obj)
		self.polarization.getNonTimedElt(path, cpopath + 'polarization', i, obj)
		self.powerflow.getNonTimedElt(path, cpopath + 'powerflow', i, obj)


class positionstructurewaves_rtposition(KeepInOrder):
	'''
	class positionstructurewaves_rtposition
	Ray/beam position

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius location [m]; Time-dependent; Vector (npoints)
	- z : numpy.ndarray 1D with float
	   Vertical location [m]; Time-dependent; Vector (npoints)
	- phi : numpy.ndarray 1D with float
	   Toroidal angle location [rad]; Time-dependent; Vector (npoints)
	- psi : numpy.ndarray 1D with float
	   Poloidal magnetic flux coordinate [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi; Time-dependent; Vector (npoints)
	- theta : numpy.ndarray 1D with float
	   Poloidal angle location [rad]; Time-dependent; Vector (npoints). PRECISE THE DEFINITION OF THE POLOIDAL ANGLE, SEE WAVES/COHERENTWAVE(:)/GRID_2D.
	'''

	def __init__(self, base_path_in='position'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')
		self.psi = numpy.zeros(0, numpy.float64, order='C')
		self.theta = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class positionstructurewaves_rtposition\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta.__str__()
		ret = ret + space + 'Attribute theta\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurewaves_rtposition, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'theta', numpy.array(self.theta).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurewaves_rtposition, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'theta', numpy.array(self.theta).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurewaves_rtposition, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurewaves_rtposition, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_r, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'r', inTime, interpolMode)
		check_status(status)
		if not status:
			self.r = ret_r
			self.cpoTime = retTime
		status, ret_z, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'z', inTime, interpolMode)
		check_status(status)
		if not status:
			self.z = ret_z
			self.cpoTime = retTime
		status, ret_phi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi = ret_phi
			self.cpoTime = retTime
		status, ret_psi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'psi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.psi = ret_psi
			self.cpoTime = retTime
		status, ret_theta, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'theta', inTime, interpolMode)
		check_status(status)
		if not status:
			self.theta = ret_theta
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurewaves_rtposition, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rList = ull.getVect2DDouble(self.idx, path, cpopath + 'r')
			if len(rList) == 0:
				rList = numpy.resize(rList, (0,nbslice))
			check_status(status)
			status, zList = ull.getVect2DDouble(self.idx, path, cpopath + 'z')
			if len(zList) == 0:
				zList = numpy.resize(zList, (0,nbslice))
			check_status(status)
			status, phiList = ull.getVect2DDouble(self.idx, path, cpopath + 'phi')
			if len(phiList) == 0:
				phiList = numpy.resize(phiList, (0,nbslice))
			check_status(status)
			status, psiList = ull.getVect2DDouble(self.idx, path, cpopath + 'psi')
			if len(psiList) == 0:
				psiList = numpy.resize(psiList, (0,nbslice))
			check_status(status)
			status, thetaList = ull.getVect2DDouble(self.idx, path, cpopath + 'theta')
			if len(thetaList) == 0:
				thetaList = numpy.resize(thetaList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = positionstructurewaves_rtposition(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[:,i]
				slice.z = zList[:,i]
				slice.phi = phiList[:,i]
				slice.psi = psiList[:,i]
				slice.theta = thetaList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurewaves_rtpositionObj, run function putTimedElt') 
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
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'psi', i, numpy.array(self.psi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'theta', i, numpy.array(self.theta).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurewaves_rtpositionObj, run function getTimedElt') 
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
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		status, ret_psi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'psi', i)
		check_status(status)
		if not status:
			self.psi = ret_psi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		status, ret_theta = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'theta', i)
		check_status(status)
		if not status:
			self.theta = ret_theta

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurewaves_rtpositionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurewaves_rtpositionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

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
		ull.deleteData(self.idx, path, cpopath + 'psi')
		ull.deleteData(self.idx, path, cpopath + 'theta')


class wavevectorstructurewaves_rtwavevector(KeepInOrder):
	'''
	class wavevectorstructurewaves_rtwavevector
	Ray/beam wave vector.

	Attributes:
	- kr : numpy.ndarray 1D with float
	   Wave vector in the major radius direction [m**-1], Vector (npoints). Time-dependent
	- kz : numpy.ndarray 1D with float
	   Wave vector in the vertical direction [m**-1], Vector (npoints). Time-dependent
	- kphi : numpy.ndarray 1D with float
	   Wave vector in the toroidal direction [m**-1], Vector (npoints). Time-dependent
	- npar : numpy.ndarray 1D with float
	   Parallel refractive index, Vector (npoints). Time-dependent
	- nperp : numpy.ndarray 1D with float
	   Perpendicular refractive index, Vector (npoints). Time-dependent
	- ntor : numpy.ndarray 1D with float
	   Toroidal wave number, Vector (npoints/1). If var_ntor=0, ntor is constant along the ray path and the last dimension is of size 1 in order to avoid useless repetition of ntor constant value. Time-dependent
	- var_ntor : int
	   Flag telling whether ntor is constant along the ray path (0) or varying (1). Integer
	'''

	def __init__(self, base_path_in='wavevector'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.kr = numpy.zeros(0, numpy.float64, order='C')
		self.kz = numpy.zeros(0, numpy.float64, order='C')
		self.kphi = numpy.zeros(0, numpy.float64, order='C')
		self.npar = numpy.zeros(0, numpy.float64, order='C')
		self.nperp = numpy.zeros(0, numpy.float64, order='C')
		self.ntor = numpy.zeros(0, numpy.float64, order='C')
		self.var_ntor = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class wavevectorstructurewaves_rtwavevector\n'
		s = self.kr.__str__()
		ret = ret + space + 'Attribute kr\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.kz.__str__()
		ret = ret + space + 'Attribute kz\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.kphi.__str__()
		ret = ret + space + 'Attribute kphi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.npar.__str__()
		ret = ret + space + 'Attribute npar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.nperp.__str__()
		ret = ret + space + 'Attribute nperp\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ntor.__str__()
		ret = ret + space + 'Attribute ntor\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute var_ntor: ' + str(self.var_ntor) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wavevectorstructurewaves_rtwavevector, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'kr', numpy.array(self.kr).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'kz', numpy.array(self.kz).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'kphi', numpy.array(self.kphi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'npar', numpy.array(self.npar).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'nperp', numpy.array(self.nperp).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ntor', numpy.array(self.ntor).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wavevectorstructurewaves_rtwavevector, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'kr', numpy.array(self.kr).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'kz', numpy.array(self.kz).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'kphi', numpy.array(self.kphi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'npar', numpy.array(self.npar).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'nperp', numpy.array(self.nperp).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ntor', numpy.array(self.ntor).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wavevectorstructurewaves_rtwavevector, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'var_ntor', self.var_ntor)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type wavevectorstructurewaves_rtwavevector, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_kr, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'kr', inTime, interpolMode)
		check_status(status)
		if not status:
			self.kr = ret_kr
			self.cpoTime = retTime
		status, ret_kz, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'kz', inTime, interpolMode)
		check_status(status)
		if not status:
			self.kz = ret_kz
			self.cpoTime = retTime
		status, ret_kphi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'kphi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.kphi = ret_kphi
			self.cpoTime = retTime
		status, ret_npar, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'npar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.npar = ret_npar
			self.cpoTime = retTime
		status, ret_nperp, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'nperp', inTime, interpolMode)
		check_status(status)
		if not status:
			self.nperp = ret_nperp
			self.cpoTime = retTime
		status, ret_ntor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ntor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ntor = ret_ntor
			self.cpoTime = retTime
		status, ret_var_ntor = ull.getInt(self.idx, path, cpopath + 'var_ntor')
		check_status(status)
		if not status:
			self.var_ntor = ret_var_ntor

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type wavevectorstructurewaves_rtwavevector, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, krList = ull.getVect2DDouble(self.idx, path, cpopath + 'kr')
			if len(krList) == 0:
				krList = numpy.resize(krList, (0,nbslice))
			check_status(status)
			status, kzList = ull.getVect2DDouble(self.idx, path, cpopath + 'kz')
			if len(kzList) == 0:
				kzList = numpy.resize(kzList, (0,nbslice))
			check_status(status)
			status, kphiList = ull.getVect2DDouble(self.idx, path, cpopath + 'kphi')
			if len(kphiList) == 0:
				kphiList = numpy.resize(kphiList, (0,nbslice))
			check_status(status)
			status, nparList = ull.getVect2DDouble(self.idx, path, cpopath + 'npar')
			if len(nparList) == 0:
				nparList = numpy.resize(nparList, (0,nbslice))
			check_status(status)
			status, nperpList = ull.getVect2DDouble(self.idx, path, cpopath + 'nperp')
			if len(nperpList) == 0:
				nperpList = numpy.resize(nperpList, (0,nbslice))
			check_status(status)
			status, ntorList = ull.getVect2DDouble(self.idx, path, cpopath + 'ntor')
			if len(ntorList) == 0:
				ntorList = numpy.resize(ntorList, (0,nbslice))
			check_status(status)
			status, var_ntorVal = ull.getInt(self.idx, path, cpopath + 'var_ntor')
			check_status(status)
			for i in range(nbslice):
				slice = wavevectorstructurewaves_rtwavevector(self.base_path)
				slice.setExpIdx(self.idx)
				slice.kr = krList[:,i]
				slice.kz = kzList[:,i]
				slice.kphi = kphiList[:,i]
				slice.npar = nparList[:,i]
				slice.nperp = nperpList[:,i]
				slice.ntor = ntorList[:,i]
				slice.var_ntor = var_ntorVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wavevectorstructurewaves_rtwavevectorObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'kr') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'kr', i, numpy.array(self.kr).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'kz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'kz', i, numpy.array(self.kz).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'kphi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'kphi', i, numpy.array(self.kphi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'npar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'npar', i, numpy.array(self.npar).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'nperp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'nperp', i, numpy.array(self.nperp).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ntor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ntor', i, numpy.array(self.ntor).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wavevectorstructurewaves_rtwavevectorObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'kr') 
			print ('obj = ' + str(obj))
		status, ret_kr = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'kr', i)
		check_status(status)
		if not status:
			self.kr = ret_kr
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'kz') 
			print ('obj = ' + str(obj))
		status, ret_kz = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'kz', i)
		check_status(status)
		if not status:
			self.kz = ret_kz
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'kphi') 
			print ('obj = ' + str(obj))
		status, ret_kphi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'kphi', i)
		check_status(status)
		if not status:
			self.kphi = ret_kphi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'npar') 
			print ('obj = ' + str(obj))
		status, ret_npar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'npar', i)
		check_status(status)
		if not status:
			self.npar = ret_npar
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'nperp') 
			print ('obj = ' + str(obj))
		status, ret_nperp = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'nperp', i)
		check_status(status)
		if not status:
			self.nperp = ret_nperp
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ntor') 
			print ('obj = ' + str(obj))
		status, ret_ntor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ntor', i)
		check_status(status)
		if not status:
			self.ntor = ret_ntor

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wavevectorstructurewaves_rtwavevectorObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'var_ntor') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'var_ntor', i, self.var_ntor)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wavevectorstructurewaves_rtwavevectorObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'var_ntor') 
			print ('obj = ' + str(obj))
		status, ret_var_ntor = ull.getIntFromObject(self.idx, obj, cpopath + 'var_ntor', i)
		check_status(status)
		if not status:
			self.var_ntor = ret_var_ntor

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'kr')
		ull.deleteData(self.idx, path, cpopath + 'kz')
		ull.deleteData(self.idx, path, cpopath + 'kphi')
		ull.deleteData(self.idx, path, cpopath + 'npar')
		ull.deleteData(self.idx, path, cpopath + 'nperp')
		ull.deleteData(self.idx, path, cpopath + 'ntor')
		ull.deleteData(self.idx, path, cpopath + 'var_ntor')


class polarizationstructurepolarization(KeepInOrder):
	'''
	class polarizationstructurepolarization
	Wave field polarization along the ray/beam.

	Attributes:
	- epol_p_re : numpy.ndarray 1D with float
	   Real part of the left hand polarized electric field (rotating with the ions), Vector (npoints). Time-dependent
	- epol_p_im : numpy.ndarray 1D with float
	   Imaginary part of the left hand polarized electric field (rotating with the ions), Vector (npoints). Time-dependent
	- epol_m_re : numpy.ndarray 1D with float
	   Real part of the right hand polarized electric field (rotating with the electrons), Vector (npoints). Time-dependent
	- epol_m_im : numpy.ndarray 1D with float
	   Real part of the right hand polarized electric field (rotating with the electrons), Vector (npoints). Time-dependent
	- epol_par_re : numpy.ndarray 1D with float
	   Real part of the electric field polarization vector in the magnetic field direction, Vector (npoints). Time-dependent
	- epol_par_im : numpy.ndarray 1D with float
	   Imaginary part of the electric field polarization vector in the magnetic field direction, Vector (npoints). Time-dependent
	'''

	def __init__(self, base_path_in='polarization'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.epol_p_re = numpy.zeros(0, numpy.float64, order='C')
		self.epol_p_im = numpy.zeros(0, numpy.float64, order='C')
		self.epol_m_re = numpy.zeros(0, numpy.float64, order='C')
		self.epol_m_im = numpy.zeros(0, numpy.float64, order='C')
		self.epol_par_re = numpy.zeros(0, numpy.float64, order='C')
		self.epol_par_im = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class polarizationstructurepolarization\n'
		s = self.epol_p_re.__str__()
		ret = ret + space + 'Attribute epol_p_re\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.epol_p_im.__str__()
		ret = ret + space + 'Attribute epol_p_im\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.epol_m_re.__str__()
		ret = ret + space + 'Attribute epol_m_re\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.epol_m_im.__str__()
		ret = ret + space + 'Attribute epol_m_im\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.epol_par_re.__str__()
		ret = ret + space + 'Attribute epol_par_re\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.epol_par_im.__str__()
		ret = ret + space + 'Attribute epol_par_im\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type polarizationstructurepolarization, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'epol_p_re', numpy.array(self.epol_p_re).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'epol_p_im', numpy.array(self.epol_p_im).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'epol_m_re', numpy.array(self.epol_m_re).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'epol_m_im', numpy.array(self.epol_m_im).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'epol_par_re', numpy.array(self.epol_par_re).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'epol_par_im', numpy.array(self.epol_par_im).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type polarizationstructurepolarization, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'epol_p_re', numpy.array(self.epol_p_re).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'epol_p_im', numpy.array(self.epol_p_im).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'epol_m_re', numpy.array(self.epol_m_re).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'epol_m_im', numpy.array(self.epol_m_im).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'epol_par_re', numpy.array(self.epol_par_re).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'epol_par_im', numpy.array(self.epol_par_im).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type polarizationstructurepolarization, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type polarizationstructurepolarization, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_epol_p_re, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'epol_p_re', inTime, interpolMode)
		check_status(status)
		if not status:
			self.epol_p_re = ret_epol_p_re
			self.cpoTime = retTime
		status, ret_epol_p_im, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'epol_p_im', inTime, interpolMode)
		check_status(status)
		if not status:
			self.epol_p_im = ret_epol_p_im
			self.cpoTime = retTime
		status, ret_epol_m_re, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'epol_m_re', inTime, interpolMode)
		check_status(status)
		if not status:
			self.epol_m_re = ret_epol_m_re
			self.cpoTime = retTime
		status, ret_epol_m_im, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'epol_m_im', inTime, interpolMode)
		check_status(status)
		if not status:
			self.epol_m_im = ret_epol_m_im
			self.cpoTime = retTime
		status, ret_epol_par_re, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'epol_par_re', inTime, interpolMode)
		check_status(status)
		if not status:
			self.epol_par_re = ret_epol_par_re
			self.cpoTime = retTime
		status, ret_epol_par_im, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'epol_par_im', inTime, interpolMode)
		check_status(status)
		if not status:
			self.epol_par_im = ret_epol_par_im
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type polarizationstructurepolarization, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, epol_p_reList = ull.getVect2DDouble(self.idx, path, cpopath + 'epol_p_re')
			if len(epol_p_reList) == 0:
				epol_p_reList = numpy.resize(epol_p_reList, (0,nbslice))
			check_status(status)
			status, epol_p_imList = ull.getVect2DDouble(self.idx, path, cpopath + 'epol_p_im')
			if len(epol_p_imList) == 0:
				epol_p_imList = numpy.resize(epol_p_imList, (0,nbslice))
			check_status(status)
			status, epol_m_reList = ull.getVect2DDouble(self.idx, path, cpopath + 'epol_m_re')
			if len(epol_m_reList) == 0:
				epol_m_reList = numpy.resize(epol_m_reList, (0,nbslice))
			check_status(status)
			status, epol_m_imList = ull.getVect2DDouble(self.idx, path, cpopath + 'epol_m_im')
			if len(epol_m_imList) == 0:
				epol_m_imList = numpy.resize(epol_m_imList, (0,nbslice))
			check_status(status)
			status, epol_par_reList = ull.getVect2DDouble(self.idx, path, cpopath + 'epol_par_re')
			if len(epol_par_reList) == 0:
				epol_par_reList = numpy.resize(epol_par_reList, (0,nbslice))
			check_status(status)
			status, epol_par_imList = ull.getVect2DDouble(self.idx, path, cpopath + 'epol_par_im')
			if len(epol_par_imList) == 0:
				epol_par_imList = numpy.resize(epol_par_imList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = polarizationstructurepolarization(self.base_path)
				slice.setExpIdx(self.idx)
				slice.epol_p_re = epol_p_reList[:,i]
				slice.epol_p_im = epol_p_imList[:,i]
				slice.epol_m_re = epol_m_reList[:,i]
				slice.epol_m_im = epol_m_imList[:,i]
				slice.epol_par_re = epol_par_reList[:,i]
				slice.epol_par_im = epol_par_imList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type polarizationstructurepolarizationObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'epol_p_re') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'epol_p_re', i, numpy.array(self.epol_p_re).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'epol_p_im') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'epol_p_im', i, numpy.array(self.epol_p_im).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'epol_m_re') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'epol_m_re', i, numpy.array(self.epol_m_re).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'epol_m_im') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'epol_m_im', i, numpy.array(self.epol_m_im).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'epol_par_re') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'epol_par_re', i, numpy.array(self.epol_par_re).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'epol_par_im') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'epol_par_im', i, numpy.array(self.epol_par_im).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type polarizationstructurepolarizationObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'epol_p_re') 
			print ('obj = ' + str(obj))
		status, ret_epol_p_re = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'epol_p_re', i)
		check_status(status)
		if not status:
			self.epol_p_re = ret_epol_p_re
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'epol_p_im') 
			print ('obj = ' + str(obj))
		status, ret_epol_p_im = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'epol_p_im', i)
		check_status(status)
		if not status:
			self.epol_p_im = ret_epol_p_im
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'epol_m_re') 
			print ('obj = ' + str(obj))
		status, ret_epol_m_re = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'epol_m_re', i)
		check_status(status)
		if not status:
			self.epol_m_re = ret_epol_m_re
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'epol_m_im') 
			print ('obj = ' + str(obj))
		status, ret_epol_m_im = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'epol_m_im', i)
		check_status(status)
		if not status:
			self.epol_m_im = ret_epol_m_im
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'epol_par_re') 
			print ('obj = ' + str(obj))
		status, ret_epol_par_re = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'epol_par_re', i)
		check_status(status)
		if not status:
			self.epol_par_re = ret_epol_par_re
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'epol_par_im') 
			print ('obj = ' + str(obj))
		status, ret_epol_par_im = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'epol_par_im', i)
		check_status(status)
		if not status:
			self.epol_par_im = ret_epol_par_im

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type polarizationstructurepolarizationObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type polarizationstructurepolarizationObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'epol_p_re')
		ull.deleteData(self.idx, path, cpopath + 'epol_p_im')
		ull.deleteData(self.idx, path, cpopath + 'epol_m_re')
		ull.deleteData(self.idx, path, cpopath + 'epol_m_im')
		ull.deleteData(self.idx, path, cpopath + 'epol_par_re')
		ull.deleteData(self.idx, path, cpopath + 'epol_par_im')


class powerflowstructurepowerflow(KeepInOrder):
	'''
	class powerflowstructurepowerflow
	Power flow along the ray/beam.

	Attributes:
	- phi_perp : numpy.ndarray 1D with float
	   Normalized power flow in the direction perpendicular to the magnetic field; Vector (npoints). Time-dependent
	- phi_par : numpy.ndarray 1D with float
	   Normalized power flow in the direction parallel to the magnetic field; Vector (npoints). Time-dependent
	- power_e : numpy.ndarray 1D with float
	   Power absorbed along the beam by electrons [W]; Vector (npoints). Time-dependent
	- power_i : numpy.ndarray 2D with float
	   Power absorbed along the beam by an ion species [W]; Matrix (npoints, nion). Time-dependent
	'''

	def __init__(self, base_path_in='powerflow'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.phi_perp = numpy.zeros(0, numpy.float64, order='C')
		self.phi_par = numpy.zeros(0, numpy.float64, order='C')
		self.power_e = numpy.zeros(0, numpy.float64, order='C')
		self.power_i = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class powerflowstructurepowerflow\n'
		s = self.phi_perp.__str__()
		ret = ret + space + 'Attribute phi_perp\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi_par.__str__()
		ret = ret + space + 'Attribute phi_par\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.power_e.__str__()
		ret = ret + space + 'Attribute power_e\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.power_i.__str__()
		ret = ret + space + 'Attribute power_i\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type powerflowstructurepowerflow, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'phi_perp', numpy.array(self.phi_perp).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'phi_par', numpy.array(self.phi_par).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'power_e', numpy.array(self.power_e).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'power_i', numpy.array(self.power_i).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type powerflowstructurepowerflow, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'phi_perp', numpy.array(self.phi_perp).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'phi_par', numpy.array(self.phi_par).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'power_e', numpy.array(self.power_e).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'power_i', numpy.array(self.power_i).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type powerflowstructurepowerflow, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type powerflowstructurepowerflow, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_phi_perp, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'phi_perp', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi_perp = ret_phi_perp
			self.cpoTime = retTime
		status, ret_phi_par, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'phi_par', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi_par = ret_phi_par
			self.cpoTime = retTime
		status, ret_power_e, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'power_e', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power_e = ret_power_e
			self.cpoTime = retTime
		status, ret_power_i, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'power_i', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power_i = ret_power_i
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type powerflowstructurepowerflow, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, phi_perpList = ull.getVect2DDouble(self.idx, path, cpopath + 'phi_perp')
			if len(phi_perpList) == 0:
				phi_perpList = numpy.resize(phi_perpList, (0,nbslice))
			check_status(status)
			status, phi_parList = ull.getVect2DDouble(self.idx, path, cpopath + 'phi_par')
			if len(phi_parList) == 0:
				phi_parList = numpy.resize(phi_parList, (0,nbslice))
			check_status(status)
			status, power_eList = ull.getVect2DDouble(self.idx, path, cpopath + 'power_e')
			if len(power_eList) == 0:
				power_eList = numpy.resize(power_eList, (0,nbslice))
			check_status(status)
			status, power_iList = ull.getVect3DDouble(self.idx, path, cpopath + 'power_i')
			if len(power_iList) == 0:
				power_iList = numpy.resize(power_iList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = powerflowstructurepowerflow(self.base_path)
				slice.setExpIdx(self.idx)
				slice.phi_perp = phi_perpList[:,i]
				slice.phi_par = phi_parList[:,i]
				slice.power_e = power_eList[:,i]
				slice.power_i = power_iList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type powerflowstructurepowerflowObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'phi_perp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'phi_perp', i, numpy.array(self.phi_perp).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'phi_par') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'phi_par', i, numpy.array(self.phi_par).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'power_e') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'power_e', i, numpy.array(self.power_e).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'power_i') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'power_i', i, numpy.array(self.power_i).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type powerflowstructurepowerflowObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'phi_perp') 
			print ('obj = ' + str(obj))
		status, ret_phi_perp = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'phi_perp', i)
		check_status(status)
		if not status:
			self.phi_perp = ret_phi_perp
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'phi_par') 
			print ('obj = ' + str(obj))
		status, ret_phi_par = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'phi_par', i)
		check_status(status)
		if not status:
			self.phi_par = ret_phi_par
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'power_e') 
			print ('obj = ' + str(obj))
		status, ret_power_e = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'power_e', i)
		check_status(status)
		if not status:
			self.power_e = ret_power_e
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'power_i') 
			print ('obj = ' + str(obj))
		status, ret_power_i = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'power_i', i)
		check_status(status)
		if not status:
			self.power_i = ret_power_i

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type powerflowstructurepowerflowObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type powerflowstructurepowerflowObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'phi_perp')
		ull.deleteData(self.idx, path, cpopath + 'phi_par')
		ull.deleteData(self.idx, path, cpopath + 'power_e')
		ull.deleteData(self.idx, path, cpopath + 'power_i')


class fullwavestructurefullwave(KeepInOrder):
	'''
	class fullwavestructurefullwave
	Solution by full wave code

	Attributes:
	- grid : class gridstructurecomplexgrid
	   Grid for storing the components of the wave field; Time-dependent
	- e_components : class e_componentsstructuree_components
	   E-field representation in terms of the parallel and circularly polarised components
	- pol_decomp : class pol_decompstructurepol_decomp
	   TO BE REMOVED, being replaced by e_components and grid. Kept only to make smooth transition between data-type versions. [Poloidal decomposition of the wave fields. Uses the flux surface grid in grid_1d.]
	- local : class localstructurelocal
	   TO BE REMOVED, being replaced by e_components and grid. Kept only to make smooth transition between data-type versions. [Local description of the wave fields. Uses the grid in grid_2d].
	'''

	def __init__(self, base_path_in='fullwave'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.grid = gridstructurecomplexgrid('grid')
		self.e_components = e_componentsstructuree_components('e_components')
		self.pol_decomp = pol_decompstructurepol_decomp('pol_decomp')
		self.local = localstructurelocal('local')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class fullwavestructurefullwave\n'
		ret = ret + space + 'Attribute grid\n ' + self.grid.__str__(depth+1)
		ret = ret + space + 'Attribute e_components\n ' + self.e_components.__str__(depth+1)
		ret = ret + space + 'Attribute pol_decomp\n ' + self.pol_decomp.__str__(depth+1)
		ret = ret + space + 'Attribute local\n ' + self.local.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.grid.setExpIdx(idx)
		self.e_components.setExpIdx(idx)
		self.pol_decomp.setExpIdx(idx)
		self.local.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fullwavestructurefullwave, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.cpoTime = self.cpoTime
		self.grid.putSlice(path, cpopath)
		self.e_components.cpoTime = self.cpoTime
		self.e_components.putSlice(path, cpopath)
		self.pol_decomp.cpoTime = self.cpoTime
		self.pol_decomp.putSlice(path, cpopath)
		self.local.cpoTime = self.cpoTime
		self.local.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fullwavestructurefullwave, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.replaceLastSlice(path, cpopath)
		self.e_components.replaceLastSlice(path, cpopath)
		self.pol_decomp.replaceLastSlice(path, cpopath)
		self.local.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fullwavestructurefullwave, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.putNonTimed(path, cpopath)
		self.e_components.putNonTimed(path, cpopath)
		self.pol_decomp.putNonTimed(path, cpopath)
		self.local.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type fullwavestructurefullwave, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.getSlice(path, cpopath, inTime, interpolMode)
		self.e_components.getSlice(path, cpopath, inTime, interpolMode)
		self.pol_decomp.getSlice(path, cpopath, inTime, interpolMode)
		self.local.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type fullwavestructurefullwave, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			gridList = self.grid.build_non_resampled_data(path, cpopath, nbslice)
			e_componentsList = self.e_components.build_non_resampled_data(path, cpopath, nbslice)
			pol_decompList = self.pol_decomp.build_non_resampled_data(path, cpopath, nbslice)
			localList = self.local.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = fullwavestructurefullwave(self.base_path)
				slice.setExpIdx(self.idx)
				slice.grid = gridList[i]
				slice.e_components = e_componentsList[i]
				slice.pol_decomp = pol_decompList[i]
				slice.local = localList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fullwavestructurefullwaveObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.grid.putTimedElt(path, cpopath + 'grid', i, obj)
		obj = self.e_components.putTimedElt(path, cpopath + 'e_components', i, obj)
		obj = self.pol_decomp.putTimedElt(path, cpopath + 'pol_decomp', i, obj)
		obj = self.local.putTimedElt(path, cpopath + 'local', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fullwavestructurefullwaveObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.grid.getTimedElt(path, cpopath + 'grid', i, obj)
		self.e_components.getTimedElt(path, cpopath + 'e_components', i, obj)
		self.pol_decomp.getTimedElt(path, cpopath + 'pol_decomp', i, obj)
		self.local.getTimedElt(path, cpopath + 'local', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fullwavestructurefullwaveObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.grid.putNonTimedElt(path, cpopath + 'grid', i, obj)
		obj = self.e_components.putNonTimedElt(path, cpopath + 'e_components', i, obj)
		obj = self.pol_decomp.putNonTimedElt(path, cpopath + 'pol_decomp', i, obj)
		obj = self.local.putNonTimedElt(path, cpopath + 'local', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fullwavestructurefullwaveObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.grid.getNonTimedElt(path, cpopath + 'grid', i, obj)
		self.e_components.getNonTimedElt(path, cpopath + 'e_components', i, obj)
		self.pol_decomp.getNonTimedElt(path, cpopath + 'pol_decomp', i, obj)
		self.local.getNonTimedElt(path, cpopath + 'local', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.deleteData(path, cpopath)
		self.e_components.deleteData(path, cpopath)
		self.pol_decomp.deleteData(path, cpopath)
		self.local.deleteData(path, cpopath)


class gridstructurecomplexgrid(KeepInOrder):
	'''
	class gridstructurecomplexgrid
	Grid for storing the components of the wave field; Time-dependent

	Attributes:
	- uid : int
	   Unique index of this grid. Used for handling multiple grids
	- id : str
	   Name / identifier string for this grid
	- spaces : class spacesstruct_arraycomplexgrid_space: array of spacesstruct_arraycomplexgrid_spaceObj objects
	   Definitions of grid spaces. Array of structures (number of spaces)
	- subgrids : class subgridsstruct_arraycomplexgrid_subgrid: array of subgridsstruct_arraycomplexgrid_subgridObj objects
	   Definitions of subgrids. Array of structures (number of subgrids)
	- metric : class metricstructurecomplexgrid_metric
	   Metric coefficients
	- geo : class geostruct_arraycomplexgrid_geo_global: array of geostruct_arraycomplexgrid_geo_globalObj objects
	   Geometry data for implicit objects
	- bases : class basesstruct_arraycomplexgrid_vector: array of basesstruct_arraycomplexgrid_vectorObj objects
	   Vector bases. Used for aligned vector representation. Time-dependent (added systematically for the COMP child inheritance of that property). Array of structures (number of bases)
	'''

	def __init__(self, base_path_in='grid'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.uid = EMPTY_INT
		self.id = ''
		self.spaces = spacesstruct_arraycomplexgrid_space('spaces')
		self.subgrids = subgridsstruct_arraycomplexgrid_subgrid('subgrids')
		self.metric = metricstructurecomplexgrid_metric('metric')
		self.geo = geostruct_arraycomplexgrid_geo_global('geo')
		self.bases = basesstruct_arraycomplexgrid_vector('bases')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class gridstructurecomplexgrid\n'
		ret = ret + space + 'Attribute uid: ' + str(self.uid) + '\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute spaces\n ' + self.spaces.__str__(depth+1)
		ret = ret + space + 'Attribute subgrids\n ' + self.subgrids.__str__(depth+1)
		ret = ret + space + 'Attribute metric\n ' + self.metric.__str__(depth+1)
		ret = ret + space + 'Attribute geo\n ' + self.geo.__str__(depth+1)
		ret = ret + space + 'Attribute bases\n ' + self.bases.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.spaces.setExpIdx(idx)
		self.subgrids.setExpIdx(idx)
		self.metric.setExpIdx(idx)
		self.geo.setExpIdx(idx)
		self.bases.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurecomplexgrid, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.spaces.cpoTime = self.cpoTime
		self.spaces.putSlice(path, cpopath)
		self.subgrids.cpoTime = self.cpoTime
		self.subgrids.putSlice(path, cpopath)
		self.metric.cpoTime = self.cpoTime
		self.metric.putSlice(path, cpopath)
		self.geo.cpoTime = self.cpoTime
		self.geo.putSlice(path, cpopath)
		self.bases.cpoTime = self.cpoTime
		self.bases.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurecomplexgrid, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.spaces.replaceLastSlice(path, cpopath)
		self.subgrids.replaceLastSlice(path, cpopath)
		self.metric.replaceLastSlice(path, cpopath)
		self.geo.replaceLastSlice(path, cpopath)
		self.bases.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurecomplexgrid, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'uid', self.uid)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'id', self.id)
		check_status(status)
		self.spaces.putNonTimed(path, cpopath)
		self.subgrids.putNonTimed(path, cpopath)
		self.metric.putNonTimed(path, cpopath)
		self.geo.putNonTimed(path, cpopath)
		self.bases.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurecomplexgrid, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_uid = ull.getInt(self.idx, path, cpopath + 'uid')
		check_status(status)
		if not status:
			self.uid = ret_uid
		status, ret_id = ull.getString(self.idx, path, cpopath + 'id')
		check_status(status)
		if not status:
			self.id = ret_id
		self.spaces.getSlice(path, cpopath, inTime, interpolMode)
		self.subgrids.getSlice(path, cpopath, inTime, interpolMode)
		self.metric.getSlice(path, cpopath, inTime, interpolMode)
		self.geo.getSlice(path, cpopath, inTime, interpolMode)
		self.bases.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurecomplexgrid, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, uidVal = ull.getInt(self.idx, path, cpopath + 'uid')
			check_status(status)
			status, idVal = ull.getString(self.idx, path, cpopath + 'id')
			check_status(status)
			spacesList = self.spaces.build_non_resampled_data(path, cpopath, nbslice)
			subgridsList = self.subgrids.build_non_resampled_data(path, cpopath, nbslice)
			metricList = self.metric.build_non_resampled_data(path, cpopath, nbslice)
			geoList = self.geo.build_non_resampled_data(path, cpopath, nbslice)
			basesList = self.bases.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = gridstructurecomplexgrid(self.base_path)
				slice.setExpIdx(self.idx)
				slice.uid = uidVal
				slice.id = idVal
				slice.spaces = spacesList[i]
				slice.subgrids = subgridsList[i]
				slice.metric = metricList[i]
				slice.geo = geoList[i]
				slice.bases = basesList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurecomplexgridObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.bases.putTimedElt(path, cpopath + 'bases', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurecomplexgridObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.bases.getTimedElt(path, cpopath + 'bases', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurecomplexgridObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'uid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'uid', i, self.uid)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		obj = self.spaces.putNonTimedElt(path, cpopath + 'spaces', i, obj)
		obj = self.subgrids.putNonTimedElt(path, cpopath + 'subgrids', i, obj)
		obj = self.metric.putNonTimedElt(path, cpopath + 'metric', i, obj)
		obj = self.geo.putNonTimedElt(path, cpopath + 'geo', i, obj)
		obj = self.bases.putNonTimedElt(path, cpopath + 'bases', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurecomplexgridObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'uid') 
			print ('obj = ' + str(obj))
		status, ret_uid = ull.getIntFromObject(self.idx, obj, cpopath + 'uid', i)
		check_status(status)
		if not status:
			self.uid = ret_uid
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		self.spaces.getNonTimedElt(path, cpopath + 'spaces', i, obj)
		self.subgrids.getNonTimedElt(path, cpopath + 'subgrids', i, obj)
		self.metric.getNonTimedElt(path, cpopath + 'metric', i, obj)
		self.geo.getNonTimedElt(path, cpopath + 'geo', i, obj)
		self.bases.getNonTimedElt(path, cpopath + 'bases', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'uid')
		ull.deleteData(self.idx, path, cpopath + 'id')
		ull.deleteData(self.idx, path, cpopath + 'spaces')
		ull.deleteData(self.idx, path, cpopath + 'subgrids')
		self.metric.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'geo')
		ull.deleteData(self.idx, path, cpopath + 'bases')


class spacesstruct_arraycomplexgrid_space:
	'''
	class spacesstruct_arraycomplexgrid_space
	Definitions of grid spaces. Array of structures (number of spaces)

	Attributes:
	- array : list of spacesstruct_arraycomplexgrid_spaceObj 
	'''

	def __init__(self, base_path_in='spaces'):
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
		ret = space + 'class spacesstruct_arraycomplexgrid_space\n'
		for i in range(len(self.array)):
			ret = ret + space + 'spacesstruct_arraycomplexgrid_space[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(spacesstruct_arraycomplexgrid_spaceObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function getSlice') 
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
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(spacesstruct_arraycomplexgrid_space(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = spacesstruct_arraycomplexgrid_space(self.base_path)
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
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type spacesstruct_arraycomplexgrid_space, run function getNonTimedElt') 
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


class spacesstruct_arraycomplexgrid_spaceObj(KeepInOrder):
	'''
	class spacesstruct_arraycomplexgrid_spaceObj
	Definitions of grid spaces. Array of structures (number of spaces)

	Attributes:
	- geotype : numpy.ndarray 1D with int)
	   Type of space geometry (id flags). Flags defining how the geometry (objects.geo) fields associated with; space objects are to be interpreted. Array (number of geometries defined for  this space),; first dimension: geometry index. A flag value of GRID_UNDEFINED=0 indicates the standard interpretation for; the given coordinates.
	- geotypeid : list of str
	   Type of space geometries (id string). See geotype.
	- coordtype : numpy.ndarray 2D with int
	   Type of coordinates describing the physical space. Vector (number of space dimensions); The size of coordtype defines the dimension of the space.; For predefined integer constants for standard coordinates see; the documentation of the grid service library.
	- objects : class objectsstruct_arrayobjects: array of objectsstruct_arrayobjectsObj objects
	   Definition of the space objects.; Array of structures (dimension of highest-dimensional objects).; First dimension: dimension of the objects (1=nodes, 2=edges, 3=faces, 4=cells/volumes, ...)
	- xpoints : numpy.ndarray 1D with int)
	   List of indices of all nodes which are x-points. Vector (number of x-points)
	'''

	def __init__(self, base_path_in='spaces'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.geotype = numpy.zeros(0, numpy.int32, order='C')
		self.geotypeid = ['']
		self.coordtype = numpy.zeros((0,0), numpy.int32, order='C')
		self.objects = objectsstruct_arrayobjects('objects')
		self.xpoints = numpy.zeros(0, numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class spacesstruct_arraycomplexgrid_spaceObj\n'
		s = self.geotype.__str__()
		ret = ret + space + 'Attribute geotype\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.geotypeid.__str__()
		ret = ret + space + 'Attribute geotypeid\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.coordtype.__str__()
		ret = ret + space + 'Attribute coordtype\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute objects\n ' + self.objects.__str__(depth+1)
		s = self.xpoints.__str__()
		ret = ret + space + 'Attribute xpoints\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.objects.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spacesstruct_arraycomplexgrid_spaceObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'geotype') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'geotype', i, numpy.array(self.geotype).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'geotypeid') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'geotypeid', i, self.geotypeid)
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'coordtype') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'coordtype', i, numpy.array(self.coordtype).astype(numpy.int32))
		obj = self.objects.putNonTimedElt(path, cpopath + 'objects', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'xpoints') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'xpoints', i, numpy.array(self.xpoints).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type spacesstruct_arraycomplexgrid_spaceObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'geotype') 
			print ('obj = ' + str(obj))
		status, ret_geotype = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'geotype', i)
		check_status(status)
		if not status:
			self.geotype = ret_geotype
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'geotypeid') 
			print ('obj = ' + str(obj))
		status, ret_geotypeid = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'geotypeid', i)
		check_status(status)
		if not status:
			self.geotypeid = ret_geotypeid
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'coordtype') 
			print ('obj = ' + str(obj))
		status, ret_coordtype = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'coordtype', i)
		check_status(status)
		if not status:
			self.coordtype = ret_coordtype
		self.objects.getNonTimedElt(path, cpopath + 'objects', i, obj)
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'xpoints') 
			print ('obj = ' + str(obj))
		status, ret_xpoints = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'xpoints', i)
		check_status(status)
		if not status:
			self.xpoints = ret_xpoints


class objectsstruct_arrayobjects:
	'''
	class objectsstruct_arrayobjects
	Definition of the space objects.; Array of structures (dimension of highest-dimensional objects).; First dimension: dimension of the objects (1=nodes, 2=edges, 3=faces, 4=cells/volumes, ...)

	Attributes:
	- array : list of objectsstruct_arrayobjectsObj 
	'''

	def __init__(self, base_path_in='objects'):
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
		ret = space + 'class objectsstruct_arrayobjects\n'
		for i in range(len(self.array)):
			ret = ret + space + 'objectsstruct_arrayobjects[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(objectsstruct_arrayobjectsObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function getSlice') 
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
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(objectsstruct_arrayobjects(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = objectsstruct_arrayobjects(self.base_path)
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
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type objectsstruct_arrayobjects, run function getNonTimedElt') 
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


class objectsstruct_arrayobjectsObj(KeepInOrder):
	'''
	class objectsstruct_arrayobjectsObj
	Definition of the space objects.; Array of structures (dimension of highest-dimensional objects).; First dimension: dimension of the objects (1=nodes, 2=edges, 3=faces, 4=cells/volumes, ...)

	Attributes:
	- boundary : numpy.ndarray 2D with int
	   Lists of (n-1)-dimensional space objects defining the boundary of an n-dimensional space object.; Matrix(number of objects of dimension n, maximum number of boundary objects).; First dimension: object index, second dimension: boundary object index
	- neighbour : numpy.ndarray 3D with int
	   Connectivity information. Array (number of objects, maximum number of boundaries per object, maximum number of neighbours per boundary).; Stores the indices of the n-dimensional objects adjacent to the given n-dimensional object.;An object can possibly have multiple neighbours on every boundary.; First dimension: object index, second dimension: boundary index, third dimension: neighbour index on the boundary.
	- geo : numpy.ndarray 4D with float
	   Geometry data matrix associated with every object. Float array (number of objects, number of geometry coeff. 1, number of geometry coeff. 2, number of geometries).; The exact definition depends on the geometry type of the space (complexgrid_space.geotype).; First dimension: object index, second+third dimension: geometry coefficient matrix row+column, third dimension: geometry index (for definition of multiple geometries).
	- measure : numpy.ndarray 2D with float
	   Measure of space objects, i.e. physical size (length for 1d, area for 2d, volume for 3d objects,...). [m^dim].; First dimension: object index, second dimension: geometry index
	'''

	def __init__(self, base_path_in='objects'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.boundary = numpy.zeros((0,0), numpy.int32, order='C')
		self.neighbour = numpy.zeros((0,0,0), numpy.int32, order='C')
		self.geo = numpy.zeros((0,0,0,0), numpy.float64, order='C')
		self.measure = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class objectsstruct_arrayobjectsObj\n'
		s = self.boundary.__str__()
		ret = ret + space + 'Attribute boundary\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.neighbour.__str__()
		ret = ret + space + 'Attribute neighbour\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.geo.__str__()
		ret = ret + space + 'Attribute geo\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.measure.__str__()
		ret = ret + space + 'Attribute measure\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type objectsstruct_arrayobjectsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'boundary') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'boundary', i, numpy.array(self.boundary).astype(numpy.int32))
		if (dev()):
			print ('putVect3DIntInObject : ' + cpopath + 'neighbour') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DIntInObject(self.idx, obj, cpopath + 'neighbour', i, numpy.array(self.neighbour).astype(numpy.int32))
		if (dev()):
			print ('putVect4DDoubleInObject : ' + cpopath + 'geo') 
			print ('obj = ' + str(obj))
		obj = ull.putVect4DDoubleInObject(self.idx, obj, cpopath + 'geo', i, numpy.array(self.geo).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'measure') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'measure', i, numpy.array(self.measure).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type objectsstruct_arrayobjectsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'boundary') 
			print ('obj = ' + str(obj))
		status, ret_boundary = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'boundary', i)
		check_status(status)
		if not status:
			self.boundary = ret_boundary
		if (dev()):
			print ('getVect3DIntInObject : ' + cpopath + 'neighbour') 
			print ('obj = ' + str(obj))
		status, ret_neighbour = ull.getVect3DIntFromObject(self.idx, obj, cpopath + 'neighbour', i)
		check_status(status)
		if not status:
			self.neighbour = ret_neighbour
		if (dev()):
			print ('getVect4DDoubleInObject : ' + cpopath + 'geo') 
			print ('obj = ' + str(obj))
		status, ret_geo = ull.getVect4DDoubleFromObject(self.idx, obj, cpopath + 'geo', i)
		check_status(status)
		if not status:
			self.geo = ret_geo
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'measure') 
			print ('obj = ' + str(obj))
		status, ret_measure = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'measure', i)
		check_status(status)
		if not status:
			self.measure = ret_measure


class subgridsstruct_arraycomplexgrid_subgrid:
	'''
	class subgridsstruct_arraycomplexgrid_subgrid
	Definitions of subgrids. Array of structures (number of subgrids)

	Attributes:
	- array : list of subgridsstruct_arraycomplexgrid_subgridObj 
	'''

	def __init__(self, base_path_in='subgrids'):
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
		ret = space + 'class subgridsstruct_arraycomplexgrid_subgrid\n'
		for i in range(len(self.array)):
			ret = ret + space + 'subgridsstruct_arraycomplexgrid_subgrid[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(subgridsstruct_arraycomplexgrid_subgridObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function getSlice') 
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
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(subgridsstruct_arraycomplexgrid_subgrid(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = subgridsstruct_arraycomplexgrid_subgrid(self.base_path)
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
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type subgridsstruct_arraycomplexgrid_subgrid, run function getNonTimedElt') 
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


class subgridsstruct_arraycomplexgrid_subgridObj(KeepInOrder):
	'''
	class subgridsstruct_arraycomplexgrid_subgridObj
	Definitions of subgrids. Array of structures (number of subgrids)

	Attributes:
	- id : str
	   ID string (name) of the subgrid.
	- list : class liststruct_arraycomplexgrid_objectlist: array of liststruct_arraycomplexgrid_objectlistObj objects
	   List of object lists. Array of structures (number of object lists).
	'''

	def __init__(self, base_path_in='subgrids'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.list = liststruct_arraycomplexgrid_objectlist('list')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class subgridsstruct_arraycomplexgrid_subgridObj\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute list\n ' + self.list.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.list.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type subgridsstruct_arraycomplexgrid_subgridObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'id', i, self.id)
		obj = self.list.putNonTimedElt(path, cpopath + 'list', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type subgridsstruct_arraycomplexgrid_subgridObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'id') 
			print ('obj = ' + str(obj))
		status, ret_id = ull.getStringFromObject(self.idx, obj, cpopath + 'id', i)
		check_status(status)
		if not status:
			self.id = ret_id
		self.list.getNonTimedElt(path, cpopath + 'list', i, obj)


class liststruct_arraycomplexgrid_objectlist:
	'''
	class liststruct_arraycomplexgrid_objectlist
	List of object lists. Array of structures (number of object lists).

	Attributes:
	- array : list of liststruct_arraycomplexgrid_objectlistObj 
	'''

	def __init__(self, base_path_in='list'):
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
		ret = space + 'class liststruct_arraycomplexgrid_objectlist\n'
		for i in range(len(self.array)):
			ret = ret + space + 'liststruct_arraycomplexgrid_objectlist[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(liststruct_arraycomplexgrid_objectlistObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function getSlice') 
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
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(liststruct_arraycomplexgrid_objectlist(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = liststruct_arraycomplexgrid_objectlist(self.base_path)
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
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type liststruct_arraycomplexgrid_objectlist, run function getNonTimedElt') 
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


class liststruct_arraycomplexgrid_objectlistObj(KeepInOrder):
	'''
	class liststruct_arraycomplexgrid_objectlistObj
	List of object lists. Array of structures (number of object lists).

	Attributes:
	- cls : numpy.ndarray 1D with int)
	   Class tuple of the grid objects in this object list. Vector (number of grid spaces)
	- indset : class indsetstruct_arraycomplexgrid_indexlist: array of indsetstruct_arraycomplexgrid_indexlistObj objects
	   Implicit list of the object indices.;  Array of structures (number of grid spaces = length of index tuple). Every index of the index tuple is described by an index set, which defines either a list of index values or a range of index values.
	- ind : numpy.ndarray 2D with int
	   Explicit list of index tuples. Matrix (number of objects, number of spaces in grid).; First dimension: object index, second dimension: index tuple/space index.; If this field is defined and has nonzero size, the object list is understood to be explicit.
	'''

	def __init__(self, base_path_in='list'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.cls = numpy.zeros(0, numpy.int32, order='C')
		self.indset = indsetstruct_arraycomplexgrid_indexlist('indset')
		self.ind = numpy.zeros((0,0), numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class liststruct_arraycomplexgrid_objectlistObj\n'
		s = self.cls.__str__()
		ret = ret + space + 'Attribute cls\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute indset\n ' + self.indset.__str__(depth+1)
		s = self.ind.__str__()
		ret = ret + space + 'Attribute ind\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.indset.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type liststruct_arraycomplexgrid_objectlistObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'cls') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'cls', i, numpy.array(self.cls).astype(numpy.int32))
		obj = self.indset.putNonTimedElt(path, cpopath + 'indset', i, obj)
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'ind') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'ind', i, numpy.array(self.ind).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type liststruct_arraycomplexgrid_objectlistObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'cls') 
			print ('obj = ' + str(obj))
		status, ret_cls = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'cls', i)
		check_status(status)
		if not status:
			self.cls = ret_cls
		self.indset.getNonTimedElt(path, cpopath + 'indset', i, obj)
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'ind') 
			print ('obj = ' + str(obj))
		status, ret_ind = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'ind', i)
		check_status(status)
		if not status:
			self.ind = ret_ind


class indsetstruct_arraycomplexgrid_indexlist:
	'''
	class indsetstruct_arraycomplexgrid_indexlist
	Implicit list of the object indices.;  Array of structures (number of grid spaces = length of index tuple). Every index of the index tuple is described by an index set, which defines either a list of index values or a range of index values.

	Attributes:
	- array : list of indsetstruct_arraycomplexgrid_indexlistObj 
	'''

	def __init__(self, base_path_in='indset'):
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
		ret = space + 'class indsetstruct_arraycomplexgrid_indexlist\n'
		for i in range(len(self.array)):
			ret = ret + space + 'indsetstruct_arraycomplexgrid_indexlist[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(indsetstruct_arraycomplexgrid_indexlistObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function getSlice') 
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
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(indsetstruct_arraycomplexgrid_indexlist(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = indsetstruct_arraycomplexgrid_indexlist(self.base_path)
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
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type indsetstruct_arraycomplexgrid_indexlist, run function getNonTimedElt') 
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


class indsetstruct_arraycomplexgrid_indexlistObj(KeepInOrder):
	'''
	class indsetstruct_arraycomplexgrid_indexlistObj
	Implicit list of the object indices.;  Array of structures (number of grid spaces = length of index tuple). Every index of the index tuple is described by an index set, which defines either a list of index values or a range of index values.

	Attributes:
	- range : numpy.ndarray 1D with int)
	   Defines an index range enumerating from range[1] to range[2] (with both range[1] and range[2] included). If additionally a third value range(3) is given, it is used as a stride. If it is omitted, a stride of 1 is assumed. Vector(3)
	- ind : numpy.ndarray 1D with int)
	   An explicit list of indices. If this member is defined and has nonzero size, the list is assumed to be explicit. Vector(length of explicit index list)
	'''

	def __init__(self, base_path_in='indset'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.range = numpy.zeros(0, numpy.int32, order='C')
		self.ind = numpy.zeros(0, numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class indsetstruct_arraycomplexgrid_indexlistObj\n'
		s = self.range.__str__()
		ret = ret + space + 'Attribute range\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ind.__str__()
		ret = ret + space + 'Attribute ind\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type indsetstruct_arraycomplexgrid_indexlistObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'range') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'range', i, numpy.array(self.range).astype(numpy.int32))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'ind') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'ind', i, numpy.array(self.ind).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type indsetstruct_arraycomplexgrid_indexlistObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'range') 
			print ('obj = ' + str(obj))
		status, ret_range = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'range', i)
		check_status(status)
		if not status:
			self.range = ret_range
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'ind') 
			print ('obj = ' + str(obj))
		status, ret_ind = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'ind', i)
		check_status(status)
		if not status:
			self.ind = ret_ind


class metricstructurecomplexgrid_metric(KeepInOrder):
	'''
	class metricstructurecomplexgrid_metric
	Metric coefficients

	Attributes:
	- measure : class measurestruct_arraycomplexgrid_scalar: array of measurestruct_arraycomplexgrid_scalarObj objects
	   Measure of object, i.e. physical size (length for 1d, area for 2d, volume for 3d objects). [m^dim].; Use this field to store measures of implicitly defined grid objects.; Array of structures (number of subgrids this information is stored on)
	- g11 : class g11struct_arraycomplexgrid_scalar: array of g11struct_arraycomplexgrid_scalarObj objects
	   Metric coefficients g11. Array of structures (number of subgrids this information is stored on)
	- g12 : class g12struct_arraycomplexgrid_scalar: array of g12struct_arraycomplexgrid_scalarObj objects
	   Metric coefficients g12. Array of structures (number of subgrids this information is stored on)
	- g13 : class g13struct_arraycomplexgrid_scalar: array of g13struct_arraycomplexgrid_scalarObj objects
	   Metric coefficients g13. Array of structures (number of subgrids this information is stored on)
	- g22 : class g22struct_arraycomplexgrid_scalar: array of g22struct_arraycomplexgrid_scalarObj objects
	   Metric coefficients g22. Array of structures (number of subgrids this information is stored on)
	- g23 : class g23struct_arraycomplexgrid_scalar: array of g23struct_arraycomplexgrid_scalarObj objects
	   Metric coefficients g23. Array of structures (number of subgrids this information is stored on)
	- g33 : class g33struct_arraycomplexgrid_scalar: array of g33struct_arraycomplexgrid_scalarObj objects
	   Metric coefficients g33. Array of structures (number of subgrids this information is stored on)
	- jacobian : class jacobianstruct_arraycomplexgrid_scalar: array of jacobianstruct_arraycomplexgrid_scalarObj objects
	   Jacobian. Array of structures (number of subgrids this information is stored on)
	'''

	def __init__(self, base_path_in='metric'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measure = measurestruct_arraycomplexgrid_scalar('measure')
		self.g11 = g11struct_arraycomplexgrid_scalar('g11')
		self.g12 = g12struct_arraycomplexgrid_scalar('g12')
		self.g13 = g13struct_arraycomplexgrid_scalar('g13')
		self.g22 = g22struct_arraycomplexgrid_scalar('g22')
		self.g23 = g23struct_arraycomplexgrid_scalar('g23')
		self.g33 = g33struct_arraycomplexgrid_scalar('g33')
		self.jacobian = jacobianstruct_arraycomplexgrid_scalar('jacobian')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class metricstructurecomplexgrid_metric\n'
		ret = ret + space + 'Attribute measure\n ' + self.measure.__str__(depth+1)
		ret = ret + space + 'Attribute g11\n ' + self.g11.__str__(depth+1)
		ret = ret + space + 'Attribute g12\n ' + self.g12.__str__(depth+1)
		ret = ret + space + 'Attribute g13\n ' + self.g13.__str__(depth+1)
		ret = ret + space + 'Attribute g22\n ' + self.g22.__str__(depth+1)
		ret = ret + space + 'Attribute g23\n ' + self.g23.__str__(depth+1)
		ret = ret + space + 'Attribute g33\n ' + self.g33.__str__(depth+1)
		ret = ret + space + 'Attribute jacobian\n ' + self.jacobian.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.measure.setExpIdx(idx)
		self.g11.setExpIdx(idx)
		self.g12.setExpIdx(idx)
		self.g13.setExpIdx(idx)
		self.g22.setExpIdx(idx)
		self.g23.setExpIdx(idx)
		self.g33.setExpIdx(idx)
		self.jacobian.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type metricstructurecomplexgrid_metric, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.measure.cpoTime = self.cpoTime
		self.measure.putSlice(path, cpopath)
		self.g11.cpoTime = self.cpoTime
		self.g11.putSlice(path, cpopath)
		self.g12.cpoTime = self.cpoTime
		self.g12.putSlice(path, cpopath)
		self.g13.cpoTime = self.cpoTime
		self.g13.putSlice(path, cpopath)
		self.g22.cpoTime = self.cpoTime
		self.g22.putSlice(path, cpopath)
		self.g23.cpoTime = self.cpoTime
		self.g23.putSlice(path, cpopath)
		self.g33.cpoTime = self.cpoTime
		self.g33.putSlice(path, cpopath)
		self.jacobian.cpoTime = self.cpoTime
		self.jacobian.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type metricstructurecomplexgrid_metric, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.measure.replaceLastSlice(path, cpopath)
		self.g11.replaceLastSlice(path, cpopath)
		self.g12.replaceLastSlice(path, cpopath)
		self.g13.replaceLastSlice(path, cpopath)
		self.g22.replaceLastSlice(path, cpopath)
		self.g23.replaceLastSlice(path, cpopath)
		self.g33.replaceLastSlice(path, cpopath)
		self.jacobian.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type metricstructurecomplexgrid_metric, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.measure.putNonTimed(path, cpopath)
		self.g11.putNonTimed(path, cpopath)
		self.g12.putNonTimed(path, cpopath)
		self.g13.putNonTimed(path, cpopath)
		self.g22.putNonTimed(path, cpopath)
		self.g23.putNonTimed(path, cpopath)
		self.g33.putNonTimed(path, cpopath)
		self.jacobian.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type metricstructurecomplexgrid_metric, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.measure.getSlice(path, cpopath, inTime, interpolMode)
		self.g11.getSlice(path, cpopath, inTime, interpolMode)
		self.g12.getSlice(path, cpopath, inTime, interpolMode)
		self.g13.getSlice(path, cpopath, inTime, interpolMode)
		self.g22.getSlice(path, cpopath, inTime, interpolMode)
		self.g23.getSlice(path, cpopath, inTime, interpolMode)
		self.g33.getSlice(path, cpopath, inTime, interpolMode)
		self.jacobian.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type metricstructurecomplexgrid_metric, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			measureList = self.measure.build_non_resampled_data(path, cpopath, nbslice)
			g11List = self.g11.build_non_resampled_data(path, cpopath, nbslice)
			g12List = self.g12.build_non_resampled_data(path, cpopath, nbslice)
			g13List = self.g13.build_non_resampled_data(path, cpopath, nbslice)
			g22List = self.g22.build_non_resampled_data(path, cpopath, nbslice)
			g23List = self.g23.build_non_resampled_data(path, cpopath, nbslice)
			g33List = self.g33.build_non_resampled_data(path, cpopath, nbslice)
			jacobianList = self.jacobian.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = metricstructurecomplexgrid_metric(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measure = measureList[i]
				slice.g11 = g11List[i]
				slice.g12 = g12List[i]
				slice.g13 = g13List[i]
				slice.g22 = g22List[i]
				slice.g23 = g23List[i]
				slice.g33 = g33List[i]
				slice.jacobian = jacobianList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type metricstructurecomplexgrid_metricObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type metricstructurecomplexgrid_metricObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type metricstructurecomplexgrid_metricObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.measure.putNonTimedElt(path, cpopath + 'measure', i, obj)
		obj = self.g11.putNonTimedElt(path, cpopath + 'g11', i, obj)
		obj = self.g12.putNonTimedElt(path, cpopath + 'g12', i, obj)
		obj = self.g13.putNonTimedElt(path, cpopath + 'g13', i, obj)
		obj = self.g22.putNonTimedElt(path, cpopath + 'g22', i, obj)
		obj = self.g23.putNonTimedElt(path, cpopath + 'g23', i, obj)
		obj = self.g33.putNonTimedElt(path, cpopath + 'g33', i, obj)
		obj = self.jacobian.putNonTimedElt(path, cpopath + 'jacobian', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type metricstructurecomplexgrid_metricObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.measure.getNonTimedElt(path, cpopath + 'measure', i, obj)
		self.g11.getNonTimedElt(path, cpopath + 'g11', i, obj)
		self.g12.getNonTimedElt(path, cpopath + 'g12', i, obj)
		self.g13.getNonTimedElt(path, cpopath + 'g13', i, obj)
		self.g22.getNonTimedElt(path, cpopath + 'g22', i, obj)
		self.g23.getNonTimedElt(path, cpopath + 'g23', i, obj)
		self.g33.getNonTimedElt(path, cpopath + 'g33', i, obj)
		self.jacobian.getNonTimedElt(path, cpopath + 'jacobian', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measure')
		ull.deleteData(self.idx, path, cpopath + 'g11')
		ull.deleteData(self.idx, path, cpopath + 'g12')
		ull.deleteData(self.idx, path, cpopath + 'g13')
		ull.deleteData(self.idx, path, cpopath + 'g22')
		ull.deleteData(self.idx, path, cpopath + 'g23')
		ull.deleteData(self.idx, path, cpopath + 'g33')
		ull.deleteData(self.idx, path, cpopath + 'jacobian')


class measurestruct_arraycomplexgrid_scalar:
	'''
	class measurestruct_arraycomplexgrid_scalar
	Measure of object, i.e. physical size (length for 1d, area for 2d, volume for 3d objects). [m^dim].; Use this field to store measures of implicitly defined grid objects.; Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of measurestruct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='measure'):
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
		ret = space + 'class measurestruct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'measurestruct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(measurestruct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(measurestruct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = measurestruct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type measurestruct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class measurestruct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class measurestruct_arraycomplexgrid_scalarObj
	Measure of object, i.e. physical size (length for 1d, area for 2d, volume for 3d objects). [m^dim].; Use this field to store measures of implicitly defined grid objects.; Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='measure'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class measurestruct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type measurestruct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type measurestruct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class g11struct_arraycomplexgrid_scalar:
	'''
	class g11struct_arraycomplexgrid_scalar
	Metric coefficients g11. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of g11struct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='g11'):
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
		ret = space + 'class g11struct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'g11struct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(g11struct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(g11struct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = g11struct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type g11struct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class g11struct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class g11struct_arraycomplexgrid_scalarObj
	Metric coefficients g11. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='g11'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class g11struct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g11struct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g11struct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class g12struct_arraycomplexgrid_scalar:
	'''
	class g12struct_arraycomplexgrid_scalar
	Metric coefficients g12. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of g12struct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='g12'):
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
		ret = space + 'class g12struct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'g12struct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(g12struct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(g12struct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = g12struct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type g12struct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class g12struct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class g12struct_arraycomplexgrid_scalarObj
	Metric coefficients g12. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='g12'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class g12struct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g12struct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g12struct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class g13struct_arraycomplexgrid_scalar:
	'''
	class g13struct_arraycomplexgrid_scalar
	Metric coefficients g13. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of g13struct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='g13'):
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
		ret = space + 'class g13struct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'g13struct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(g13struct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(g13struct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = g13struct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type g13struct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class g13struct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class g13struct_arraycomplexgrid_scalarObj
	Metric coefficients g13. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='g13'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class g13struct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g13struct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g13struct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class g22struct_arraycomplexgrid_scalar:
	'''
	class g22struct_arraycomplexgrid_scalar
	Metric coefficients g22. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of g22struct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='g22'):
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
		ret = space + 'class g22struct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'g22struct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(g22struct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(g22struct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = g22struct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type g22struct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class g22struct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class g22struct_arraycomplexgrid_scalarObj
	Metric coefficients g22. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='g22'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class g22struct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g22struct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g22struct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class g23struct_arraycomplexgrid_scalar:
	'''
	class g23struct_arraycomplexgrid_scalar
	Metric coefficients g23. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of g23struct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='g23'):
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
		ret = space + 'class g23struct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'g23struct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(g23struct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(g23struct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = g23struct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type g23struct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class g23struct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class g23struct_arraycomplexgrid_scalarObj
	Metric coefficients g23. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='g23'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class g23struct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g23struct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g23struct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class g33struct_arraycomplexgrid_scalar:
	'''
	class g33struct_arraycomplexgrid_scalar
	Metric coefficients g33. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of g33struct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='g33'):
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
		ret = space + 'class g33struct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'g33struct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(g33struct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(g33struct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = g33struct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type g33struct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class g33struct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class g33struct_arraycomplexgrid_scalarObj
	Metric coefficients g33. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='g33'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class g33struct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g33struct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type g33struct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class jacobianstruct_arraycomplexgrid_scalar:
	'''
	class jacobianstruct_arraycomplexgrid_scalar
	Jacobian. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- array : list of jacobianstruct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='jacobian'):
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
		ret = space + 'class jacobianstruct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'jacobianstruct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(jacobianstruct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(jacobianstruct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = jacobianstruct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type jacobianstruct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class jacobianstruct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class jacobianstruct_arraycomplexgrid_scalarObj
	Jacobian. Array of structures (number of subgrids this information is stored on)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='jacobian'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class jacobianstruct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jacobianstruct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jacobianstruct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class geostruct_arraycomplexgrid_geo_global:
	'''
	class geostruct_arraycomplexgrid_geo_global
	Geometry data for implicit objects

	Attributes:
	- array : list of geostruct_arraycomplexgrid_geo_globalObj 
	'''

	def __init__(self, base_path_in='geo'):
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
		ret = space + 'class geostruct_arraycomplexgrid_geo_global\n'
		for i in range(len(self.array)):
			ret = ret + space + 'geostruct_arraycomplexgrid_geo_global[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(geostruct_arraycomplexgrid_geo_globalObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function getSlice') 
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
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(geostruct_arraycomplexgrid_geo_global(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = geostruct_arraycomplexgrid_geo_global(self.base_path)
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
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type geostruct_arraycomplexgrid_geo_global, run function getNonTimedElt') 
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


class geostruct_arraycomplexgrid_geo_globalObj(KeepInOrder):
	'''
	class geostruct_arraycomplexgrid_geo_globalObj
	Geometry data for implicit objects

	Attributes:
	- geotype : int
	   Type of geometry (id flag). A flag defining how the geometry data associated with grid objects is to be interpreted. If the field is undefined (0=GRID_UNDEFINED), the standard interpretation for; the given coordinate types is assumed.
	- geotypeid : str
	   Type of geometry (id string). 
	- coordtype : numpy.ndarray 1D with int)
	   Type of coordinates describing the physical space. Vector (number of space dimensions); The size of coordtype defines the dimension of the space.; For predefined integer constants for standard coordinates see; the documentation of the grid service library.
	- geo_matrix : class geo_matrixstruct_arraycomplexgrid_scalar: array of geo_matrixstruct_arraycomplexgrid_scalarObj objects
	   Geometry data matrix associated with implicit objects. Array of structures (number of subgrids this information is stored on); The exact definition of the stored values depends on the geometry type of the geometry complexgrid_geo_global.geotype; 
	- measure : class measurestruct_arraycomplexgrid_scalar: array of measurestruct_arraycomplexgrid_scalarObj objects
	   Measure of object, i.e. physical size (length for 1d, area for 2d, volume for 3d objects) in this geometry. [m^dim].; Use this field to store measures of implicitly defined grid objects.; Array of structures (number of subgrids this information is stored on)
	'''

	def __init__(self, base_path_in='geo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.geotype = EMPTY_INT
		self.geotypeid = ''
		self.coordtype = numpy.zeros(0, numpy.int32, order='C')
		self.geo_matrix = geo_matrixstruct_arraycomplexgrid_scalar('geo_matrix')
		self.measure = measurestruct_arraycomplexgrid_scalar('measure')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class geostruct_arraycomplexgrid_geo_globalObj\n'
		ret = ret + space + 'Attribute geotype: ' + str(self.geotype) + '\n'
		ret = ret + space + 'Attribute geotypeid: ' + str(self.geotypeid) + '\n'
		s = self.coordtype.__str__()
		ret = ret + space + 'Attribute coordtype\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute geo_matrix\n ' + self.geo_matrix.__str__(depth+1)
		ret = ret + space + 'Attribute measure\n ' + self.measure.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.geo_matrix.setExpIdx(idx)
		self.measure.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geostruct_arraycomplexgrid_geo_globalObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'geotype') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'geotype', i, self.geotype)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'geotypeid') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'geotypeid', i, self.geotypeid)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'coordtype') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'coordtype', i, numpy.array(self.coordtype).astype(numpy.int32))
		obj = self.geo_matrix.putNonTimedElt(path, cpopath + 'geo_matrix', i, obj)
		obj = self.measure.putNonTimedElt(path, cpopath + 'measure', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geostruct_arraycomplexgrid_geo_globalObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'geotype') 
			print ('obj = ' + str(obj))
		status, ret_geotype = ull.getIntFromObject(self.idx, obj, cpopath + 'geotype', i)
		check_status(status)
		if not status:
			self.geotype = ret_geotype
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'geotypeid') 
			print ('obj = ' + str(obj))
		status, ret_geotypeid = ull.getStringFromObject(self.idx, obj, cpopath + 'geotypeid', i)
		check_status(status)
		if not status:
			self.geotypeid = ret_geotypeid
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'coordtype') 
			print ('obj = ' + str(obj))
		status, ret_coordtype = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'coordtype', i)
		check_status(status)
		if not status:
			self.coordtype = ret_coordtype
		self.geo_matrix.getNonTimedElt(path, cpopath + 'geo_matrix', i, obj)
		self.measure.getNonTimedElt(path, cpopath + 'measure', i, obj)


class geo_matrixstruct_arraycomplexgrid_scalar:
	'''
	class geo_matrixstruct_arraycomplexgrid_scalar
	Geometry data matrix associated with implicit objects. Array of structures (number of subgrids this information is stored on); The exact definition of the stored values depends on the geometry type of the geometry complexgrid_geo_global.geotype; 

	Attributes:
	- array : list of geo_matrixstruct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='geo_matrix'):
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
		ret = space + 'class geo_matrixstruct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'geo_matrixstruct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(geo_matrixstruct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(geo_matrixstruct_arraycomplexgrid_scalar(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = geo_matrixstruct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type geo_matrixstruct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class geo_matrixstruct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class geo_matrixstruct_arraycomplexgrid_scalarObj
	Geometry data matrix associated with implicit objects. Array of structures (number of subgrids this information is stored on); The exact definition of the stored values depends on the geometry type of the geometry complexgrid_geo_global.geotype; 

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='geo_matrix'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class geo_matrixstruct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geo_matrixstruct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geo_matrixstruct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix


class basesstruct_arraycomplexgrid_vector:
	'''
	class basesstruct_arraycomplexgrid_vector
	Vector bases. Used for aligned vector representation. Time-dependent (added systematically for the COMP child inheritance of that property). Array of structures (number of bases)

	Attributes:
	- array : list of basesstruct_arraycomplexgrid_vectorObj 
	'''

	def __init__(self, base_path_in='bases'):
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
		ret = space + 'class basesstruct_arraycomplexgrid_vector\n'
		for i in range(len(self.array)):
			ret = ret + space + 'basesstruct_arraycomplexgrid_vector[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(basesstruct_arraycomplexgrid_vectorObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function putSlice') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function getSlice') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(basesstruct_arraycomplexgrid_vector(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(basesstruct_arraycomplexgrid_vector(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = basesstruct_arraycomplexgrid_vector(self.base_path)
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type basesstruct_arraycomplexgrid_vector, run function getNonTimedElt') 
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


class basesstruct_arraycomplexgrid_vectorObj(KeepInOrder):
	'''
	class basesstruct_arraycomplexgrid_vectorObj
	Vector bases. Used for aligned vector representation. Time-dependent (added systematically for the COMP child inheritance of that property). Array of structures (number of bases)

	Attributes:
	- griduid : int
	   Unique identifier of the grid this vector quantity is associated with.
	- label : str
	   Label describing the data
	- comp : class compstruct_arraycomplexgrid_scalar: array of compstruct_arraycomplexgrid_scalarObj objects
	   Components of the vector. Array of structures (number of vector components). Time-dependent; FIXME: inherit time-dependence for this element
	- align : numpy.ndarray 1D with int)
	   Alignment flag for vector components. Integer vector (number of vector components).
	- alignid : list of str
	   Alignment id for vector components. String vector (number of vector components).
	- basis : int
	   Index of basis (defined in associated grid) this vector is aligned to; If set to GRID_UNDEFINED=0, the canonical basis of the default coordinates of the grid assumed.
	'''

	def __init__(self, base_path_in='bases'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.label = ''
		self.comp = compstruct_arraycomplexgrid_scalar('comp')
		self.align = numpy.zeros(0, numpy.int32, order='C')
		self.alignid = ['']
		self.basis = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class basesstruct_arraycomplexgrid_vectorObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		ret = ret + space + 'Attribute comp\n ' + self.comp.__str__(depth+1)
		s = self.align.__str__()
		ret = ret + space + 'Attribute align\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.alignid.__str__()
		ret = ret + space + 'Attribute alignid\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute basis: ' + str(self.basis) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.comp.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basesstruct_arraycomplexgrid_vectorObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.comp.putTimedElt(path, cpopath + 'comp', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basesstruct_arraycomplexgrid_vectorObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.comp.getTimedElt(path, cpopath + 'comp', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basesstruct_arraycomplexgrid_vectorObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		obj = self.comp.putNonTimedElt(path, cpopath + 'comp', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'align') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'align', i, numpy.array(self.align).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'alignid') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'alignid', i, self.alignid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'basis', i, self.basis)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type basesstruct_arraycomplexgrid_vectorObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label
		self.comp.getNonTimedElt(path, cpopath + 'comp', i, obj)
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'align') 
			print ('obj = ' + str(obj))
		status, ret_align = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'align', i)
		check_status(status)
		if not status:
			self.align = ret_align
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'alignid') 
			print ('obj = ' + str(obj))
		status, ret_alignid = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'alignid', i)
		check_status(status)
		if not status:
			self.alignid = ret_alignid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		status, ret_basis = ull.getIntFromObject(self.idx, obj, cpopath + 'basis', i)
		check_status(status)
		if not status:
			self.basis = ret_basis


class compstruct_arraycomplexgrid_scalar:
	'''
	class compstruct_arraycomplexgrid_scalar
	Components of the vector. Array of structures (number of vector components). Time-dependent; FIXME: inherit time-dependence for this element

	Attributes:
	- array : list of compstruct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='comp'):
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
		ret = space + 'class compstruct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'compstruct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(compstruct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function putSlice') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(compstruct_arraycomplexgrid_scalar(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(compstruct_arraycomplexgrid_scalar(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = compstruct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type compstruct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class compstruct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class compstruct_arraycomplexgrid_scalarObj
	Components of the vector. Array of structures (number of vector components). Time-dependent; FIXME: inherit time-dependence for this element

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with float
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Float Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with float
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Float matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with float
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d float array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='comp'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.float64, order='C')
		self.vector = numpy.zeros((0,0), numpy.float64, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class compstruct_arraycomplexgrid_scalarObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compstruct_arraycomplexgrid_scalarObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compstruct_arraycomplexgrid_scalarObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compstruct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compstruct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class e_componentsstructuree_components(KeepInOrder):
	'''
	class e_componentsstructuree_components
	E-field representation in terms of the parallel and circularly polarised components

	Attributes:
	- e_plus : class e_plusstructurecomplexgrid_scalar_cplx
	   Left hand circularly polarised component of the perpendicular (to the static magnetic field) electric field [V/m]. Time-dependent; Complexgrid_scalar
	- e_minus : class e_minusstructurecomplexgrid_scalar_cplx
	   Right hand circularly polarised component of the perpendicular (to the static magnetic field) electric field [V/m]. Time-dependent; Complexgrid_scalar
	- e_para : class e_parastructurecomplexgrid_scalar_cplx
	   Parallel (to the static magnetic field) component of electric field [V/m]. Time-dependent; Complexgrid_scalar
	- e_norm : class e_normstructurecomplexgrid_scalar_cplx
	   Magnitude of wave electric field normal to a flux surface [V/m]; Time-dependent; Complexgrid_scalar
	- e_binorm : class e_binormstructurecomplexgrid_scalar_cplx
	   Magnitude of perpendicular (to the static magnetic field) wave electric field tangent to a flux surface [V/m]; Time-dependent; Complexgrid_scalar
	- b_norm : class b_normstructurecomplexgrid_scalar_cplx
	   Magnitude of perpendicular (to the static magnetic field) wave magnetic field normal to a flux surface [T]; Time-dependent; Complexgrid_scalar
	- b_binorm : class b_binormstructurecomplexgrid_scalar_cplx
	   Magnitude of wave magnetic field tangent to a flux surface [T]; Time-dependent; Complexgrid_scalar
	- b_para : class b_parastructurecomplexgrid_scalar_cplx
	   Magnitude of wave magnetic field parallel to the equilibrium magnetic field [T]; Time-dependent; Complexgrid_scalar
	- k_perp : class k_perpstructurecomplexgrid_scalar_cplx
	   Perpendicular wave number [1/m]; Time-dependent; Complexgrid_scalar
	'''

	def __init__(self, base_path_in='e_components'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.e_plus = e_plusstructurecomplexgrid_scalar_cplx('e_plus')
		self.e_minus = e_minusstructurecomplexgrid_scalar_cplx('e_minus')
		self.e_para = e_parastructurecomplexgrid_scalar_cplx('e_para')
		self.e_norm = e_normstructurecomplexgrid_scalar_cplx('e_norm')
		self.e_binorm = e_binormstructurecomplexgrid_scalar_cplx('e_binorm')
		self.b_norm = b_normstructurecomplexgrid_scalar_cplx('b_norm')
		self.b_binorm = b_binormstructurecomplexgrid_scalar_cplx('b_binorm')
		self.b_para = b_parastructurecomplexgrid_scalar_cplx('b_para')
		self.k_perp = k_perpstructurecomplexgrid_scalar_cplx('k_perp')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class e_componentsstructuree_components\n'
		ret = ret + space + 'Attribute e_plus\n ' + self.e_plus.__str__(depth+1)
		ret = ret + space + 'Attribute e_minus\n ' + self.e_minus.__str__(depth+1)
		ret = ret + space + 'Attribute e_para\n ' + self.e_para.__str__(depth+1)
		ret = ret + space + 'Attribute e_norm\n ' + self.e_norm.__str__(depth+1)
		ret = ret + space + 'Attribute e_binorm\n ' + self.e_binorm.__str__(depth+1)
		ret = ret + space + 'Attribute b_norm\n ' + self.b_norm.__str__(depth+1)
		ret = ret + space + 'Attribute b_binorm\n ' + self.b_binorm.__str__(depth+1)
		ret = ret + space + 'Attribute b_para\n ' + self.b_para.__str__(depth+1)
		ret = ret + space + 'Attribute k_perp\n ' + self.k_perp.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.e_plus.setExpIdx(idx)
		self.e_minus.setExpIdx(idx)
		self.e_para.setExpIdx(idx)
		self.e_norm.setExpIdx(idx)
		self.e_binorm.setExpIdx(idx)
		self.b_norm.setExpIdx(idx)
		self.b_binorm.setExpIdx(idx)
		self.b_para.setExpIdx(idx)
		self.k_perp.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_componentsstructuree_components, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.e_plus.cpoTime = self.cpoTime
		self.e_plus.putSlice(path, cpopath)
		self.e_minus.cpoTime = self.cpoTime
		self.e_minus.putSlice(path, cpopath)
		self.e_para.cpoTime = self.cpoTime
		self.e_para.putSlice(path, cpopath)
		self.e_norm.cpoTime = self.cpoTime
		self.e_norm.putSlice(path, cpopath)
		self.e_binorm.cpoTime = self.cpoTime
		self.e_binorm.putSlice(path, cpopath)
		self.b_norm.cpoTime = self.cpoTime
		self.b_norm.putSlice(path, cpopath)
		self.b_binorm.cpoTime = self.cpoTime
		self.b_binorm.putSlice(path, cpopath)
		self.b_para.cpoTime = self.cpoTime
		self.b_para.putSlice(path, cpopath)
		self.k_perp.cpoTime = self.cpoTime
		self.k_perp.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_componentsstructuree_components, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.e_plus.replaceLastSlice(path, cpopath)
		self.e_minus.replaceLastSlice(path, cpopath)
		self.e_para.replaceLastSlice(path, cpopath)
		self.e_norm.replaceLastSlice(path, cpopath)
		self.e_binorm.replaceLastSlice(path, cpopath)
		self.b_norm.replaceLastSlice(path, cpopath)
		self.b_binorm.replaceLastSlice(path, cpopath)
		self.b_para.replaceLastSlice(path, cpopath)
		self.k_perp.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_componentsstructuree_components, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.e_plus.putNonTimed(path, cpopath)
		self.e_minus.putNonTimed(path, cpopath)
		self.e_para.putNonTimed(path, cpopath)
		self.e_norm.putNonTimed(path, cpopath)
		self.e_binorm.putNonTimed(path, cpopath)
		self.b_norm.putNonTimed(path, cpopath)
		self.b_binorm.putNonTimed(path, cpopath)
		self.b_para.putNonTimed(path, cpopath)
		self.k_perp.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type e_componentsstructuree_components, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.e_plus.getSlice(path, cpopath, inTime, interpolMode)
		self.e_minus.getSlice(path, cpopath, inTime, interpolMode)
		self.e_para.getSlice(path, cpopath, inTime, interpolMode)
		self.e_norm.getSlice(path, cpopath, inTime, interpolMode)
		self.e_binorm.getSlice(path, cpopath, inTime, interpolMode)
		self.b_norm.getSlice(path, cpopath, inTime, interpolMode)
		self.b_binorm.getSlice(path, cpopath, inTime, interpolMode)
		self.b_para.getSlice(path, cpopath, inTime, interpolMode)
		self.k_perp.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type e_componentsstructuree_components, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			e_plusList = self.e_plus.build_non_resampled_data(path, cpopath, nbslice)
			e_minusList = self.e_minus.build_non_resampled_data(path, cpopath, nbslice)
			e_paraList = self.e_para.build_non_resampled_data(path, cpopath, nbslice)
			e_normList = self.e_norm.build_non_resampled_data(path, cpopath, nbslice)
			e_binormList = self.e_binorm.build_non_resampled_data(path, cpopath, nbslice)
			b_normList = self.b_norm.build_non_resampled_data(path, cpopath, nbslice)
			b_binormList = self.b_binorm.build_non_resampled_data(path, cpopath, nbslice)
			b_paraList = self.b_para.build_non_resampled_data(path, cpopath, nbslice)
			k_perpList = self.k_perp.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = e_componentsstructuree_components(self.base_path)
				slice.setExpIdx(self.idx)
				slice.e_plus = e_plusList[i]
				slice.e_minus = e_minusList[i]
				slice.e_para = e_paraList[i]
				slice.e_norm = e_normList[i]
				slice.e_binorm = e_binormList[i]
				slice.b_norm = b_normList[i]
				slice.b_binorm = b_binormList[i]
				slice.b_para = b_paraList[i]
				slice.k_perp = k_perpList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_componentsstructuree_componentsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.e_plus.putTimedElt(path, cpopath + 'e_plus', i, obj)
		obj = self.e_minus.putTimedElt(path, cpopath + 'e_minus', i, obj)
		obj = self.e_para.putTimedElt(path, cpopath + 'e_para', i, obj)
		obj = self.e_norm.putTimedElt(path, cpopath + 'e_norm', i, obj)
		obj = self.e_binorm.putTimedElt(path, cpopath + 'e_binorm', i, obj)
		obj = self.b_norm.putTimedElt(path, cpopath + 'b_norm', i, obj)
		obj = self.b_binorm.putTimedElt(path, cpopath + 'b_binorm', i, obj)
		obj = self.b_para.putTimedElt(path, cpopath + 'b_para', i, obj)
		obj = self.k_perp.putTimedElt(path, cpopath + 'k_perp', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_componentsstructuree_componentsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.e_plus.getTimedElt(path, cpopath + 'e_plus', i, obj)
		self.e_minus.getTimedElt(path, cpopath + 'e_minus', i, obj)
		self.e_para.getTimedElt(path, cpopath + 'e_para', i, obj)
		self.e_norm.getTimedElt(path, cpopath + 'e_norm', i, obj)
		self.e_binorm.getTimedElt(path, cpopath + 'e_binorm', i, obj)
		self.b_norm.getTimedElt(path, cpopath + 'b_norm', i, obj)
		self.b_binorm.getTimedElt(path, cpopath + 'b_binorm', i, obj)
		self.b_para.getTimedElt(path, cpopath + 'b_para', i, obj)
		self.k_perp.getTimedElt(path, cpopath + 'k_perp', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_componentsstructuree_componentsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.e_plus.putNonTimedElt(path, cpopath + 'e_plus', i, obj)
		obj = self.e_minus.putNonTimedElt(path, cpopath + 'e_minus', i, obj)
		obj = self.e_para.putNonTimedElt(path, cpopath + 'e_para', i, obj)
		obj = self.e_norm.putNonTimedElt(path, cpopath + 'e_norm', i, obj)
		obj = self.e_binorm.putNonTimedElt(path, cpopath + 'e_binorm', i, obj)
		obj = self.b_norm.putNonTimedElt(path, cpopath + 'b_norm', i, obj)
		obj = self.b_binorm.putNonTimedElt(path, cpopath + 'b_binorm', i, obj)
		obj = self.b_para.putNonTimedElt(path, cpopath + 'b_para', i, obj)
		obj = self.k_perp.putNonTimedElt(path, cpopath + 'k_perp', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_componentsstructuree_componentsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.e_plus.getNonTimedElt(path, cpopath + 'e_plus', i, obj)
		self.e_minus.getNonTimedElt(path, cpopath + 'e_minus', i, obj)
		self.e_para.getNonTimedElt(path, cpopath + 'e_para', i, obj)
		self.e_norm.getNonTimedElt(path, cpopath + 'e_norm', i, obj)
		self.e_binorm.getNonTimedElt(path, cpopath + 'e_binorm', i, obj)
		self.b_norm.getNonTimedElt(path, cpopath + 'b_norm', i, obj)
		self.b_binorm.getNonTimedElt(path, cpopath + 'b_binorm', i, obj)
		self.b_para.getNonTimedElt(path, cpopath + 'b_para', i, obj)
		self.k_perp.getNonTimedElt(path, cpopath + 'k_perp', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.e_plus.deleteData(path, cpopath)
		self.e_minus.deleteData(path, cpopath)
		self.e_para.deleteData(path, cpopath)
		self.e_norm.deleteData(path, cpopath)
		self.e_binorm.deleteData(path, cpopath)
		self.b_norm.deleteData(path, cpopath)
		self.b_binorm.deleteData(path, cpopath)
		self.b_para.deleteData(path, cpopath)
		self.k_perp.deleteData(path, cpopath)


class e_plusstructurecomplexgrid_scalar_cplx(KeepInOrder):
	'''
	class e_plusstructurecomplexgrid_scalar_cplx
	Left hand circularly polarised component of the perpendicular (to the static magnetic field) electric field [V/m]. Time-dependent; Complexgrid_scalar

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with  complex numbers
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Complex Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with  complex numbers
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Complex matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with  complex numbers
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d complex array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='e_plus'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.complex128, order='C')
		self.vector = numpy.zeros((0,0), numpy.complex128, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class e_plusstructurecomplexgrid_scalar_cplx\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_plusstructurecomplexgrid_scalar_cplx, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'griduid', self.griduid, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid, self.cpoTime)
		check_status(status)
		status = ull.putVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_plusstructurecomplexgrid_scalar_cplx, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid)
		check_status(status)
		status = ull.replaceLastVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_plusstructurecomplexgrid_scalar_cplx, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type e_plusstructurecomplexgrid_scalar_cplx, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'griduid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
			self.cpoTime = retTime
		status, ret_subgrid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'subgrid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
			self.cpoTime = retTime
		status, ret_scalar, retTime = ull.getVect1DComplexSlice(self.idx, path, cpopath + 'scalar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
			self.cpoTime = retTime
		status, ret_vector, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'vector', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vector = ret_vector
			self.cpoTime = retTime
		status, ret_matrix, retTime = ull.getVect3DComplexSlice(self.idx, path, cpopath + 'matrix', inTime, interpolMode)
		check_status(status)
		if not status:
			self.matrix = ret_matrix
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type e_plusstructurecomplexgrid_scalar_cplx, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidList = ull.getVect1DInt(self.idx, path, cpopath + 'griduid')
			if len(griduidList) == 0:
				griduidList = numpy.resize(griduidList, (nbslice))
			check_status(status)
			status, subgridList = ull.getVect1DInt(self.idx, path, cpopath + 'subgrid')
			if len(subgridList) == 0:
				subgridList = numpy.resize(subgridList, (nbslice))
			check_status(status)
			status, scalarList = ull.getVect2DComplex(self.idx, path, cpopath + 'scalar')
			if len(scalarList) == 0:
				scalarList = numpy.resize(scalarList, (0,nbslice))
			check_status(status)
			status, vectorList = ull.getVect3DComplex(self.idx, path, cpopath + 'vector')
			if len(vectorList) == 0:
				vectorList = numpy.resize(vectorList, (0,0,nbslice))
			check_status(status)
			status, matrixList = ull.getVect4DComplex(self.idx, path, cpopath + 'matrix')
			if len(matrixList) == 0:
				matrixList = numpy.resize(matrixList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = e_plusstructurecomplexgrid_scalar_cplx(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = int(griduidList[i].copy())
				slice.subgrid = int(subgridList[i].copy())
				slice.scalar = scalarList[:,i].copy().astype(complex)
				slice.vector = vectorList[:,:,i].copy().astype(complex)
				slice.matrix = matrixList[:,:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_plusstructurecomplexgrid_scalar_cplxObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DComplexInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.complex128))
		if (dev()):
			print ('putVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DComplexInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_plusstructurecomplexgrid_scalar_cplxObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DComplexFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DComplexFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_plusstructurecomplexgrid_scalar_cplxObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_plusstructurecomplexgrid_scalar_cplxObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'subgrid')
		ull.deleteData(self.idx, path, cpopath + 'scalar')
		ull.deleteData(self.idx, path, cpopath + 'vector')
		ull.deleteData(self.idx, path, cpopath + 'matrix')


class e_minusstructurecomplexgrid_scalar_cplx(KeepInOrder):
	'''
	class e_minusstructurecomplexgrid_scalar_cplx
	Right hand circularly polarised component of the perpendicular (to the static magnetic field) electric field [V/m]. Time-dependent; Complexgrid_scalar

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with  complex numbers
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Complex Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with  complex numbers
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Complex matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with  complex numbers
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d complex array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='e_minus'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.complex128, order='C')
		self.vector = numpy.zeros((0,0), numpy.complex128, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class e_minusstructurecomplexgrid_scalar_cplx\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_minusstructurecomplexgrid_scalar_cplx, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'griduid', self.griduid, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid, self.cpoTime)
		check_status(status)
		status = ull.putVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_minusstructurecomplexgrid_scalar_cplx, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid)
		check_status(status)
		status = ull.replaceLastVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_minusstructurecomplexgrid_scalar_cplx, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type e_minusstructurecomplexgrid_scalar_cplx, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'griduid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
			self.cpoTime = retTime
		status, ret_subgrid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'subgrid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
			self.cpoTime = retTime
		status, ret_scalar, retTime = ull.getVect1DComplexSlice(self.idx, path, cpopath + 'scalar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
			self.cpoTime = retTime
		status, ret_vector, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'vector', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vector = ret_vector
			self.cpoTime = retTime
		status, ret_matrix, retTime = ull.getVect3DComplexSlice(self.idx, path, cpopath + 'matrix', inTime, interpolMode)
		check_status(status)
		if not status:
			self.matrix = ret_matrix
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type e_minusstructurecomplexgrid_scalar_cplx, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidList = ull.getVect1DInt(self.idx, path, cpopath + 'griduid')
			if len(griduidList) == 0:
				griduidList = numpy.resize(griduidList, (nbslice))
			check_status(status)
			status, subgridList = ull.getVect1DInt(self.idx, path, cpopath + 'subgrid')
			if len(subgridList) == 0:
				subgridList = numpy.resize(subgridList, (nbslice))
			check_status(status)
			status, scalarList = ull.getVect2DComplex(self.idx, path, cpopath + 'scalar')
			if len(scalarList) == 0:
				scalarList = numpy.resize(scalarList, (0,nbslice))
			check_status(status)
			status, vectorList = ull.getVect3DComplex(self.idx, path, cpopath + 'vector')
			if len(vectorList) == 0:
				vectorList = numpy.resize(vectorList, (0,0,nbslice))
			check_status(status)
			status, matrixList = ull.getVect4DComplex(self.idx, path, cpopath + 'matrix')
			if len(matrixList) == 0:
				matrixList = numpy.resize(matrixList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = e_minusstructurecomplexgrid_scalar_cplx(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = int(griduidList[i].copy())
				slice.subgrid = int(subgridList[i].copy())
				slice.scalar = scalarList[:,i].copy().astype(complex)
				slice.vector = vectorList[:,:,i].copy().astype(complex)
				slice.matrix = matrixList[:,:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_minusstructurecomplexgrid_scalar_cplxObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DComplexInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.complex128))
		if (dev()):
			print ('putVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DComplexInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_minusstructurecomplexgrid_scalar_cplxObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DComplexFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DComplexFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_minusstructurecomplexgrid_scalar_cplxObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_minusstructurecomplexgrid_scalar_cplxObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'subgrid')
		ull.deleteData(self.idx, path, cpopath + 'scalar')
		ull.deleteData(self.idx, path, cpopath + 'vector')
		ull.deleteData(self.idx, path, cpopath + 'matrix')


class e_parastructurecomplexgrid_scalar_cplx(KeepInOrder):
	'''
	class e_parastructurecomplexgrid_scalar_cplx
	Parallel (to the static magnetic field) component of electric field [V/m]. Time-dependent; Complexgrid_scalar

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with  complex numbers
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Complex Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with  complex numbers
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Complex matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with  complex numbers
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d complex array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='e_para'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.complex128, order='C')
		self.vector = numpy.zeros((0,0), numpy.complex128, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class e_parastructurecomplexgrid_scalar_cplx\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_parastructurecomplexgrid_scalar_cplx, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'griduid', self.griduid, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid, self.cpoTime)
		check_status(status)
		status = ull.putVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_parastructurecomplexgrid_scalar_cplx, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid)
		check_status(status)
		status = ull.replaceLastVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_parastructurecomplexgrid_scalar_cplx, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type e_parastructurecomplexgrid_scalar_cplx, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'griduid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
			self.cpoTime = retTime
		status, ret_subgrid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'subgrid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
			self.cpoTime = retTime
		status, ret_scalar, retTime = ull.getVect1DComplexSlice(self.idx, path, cpopath + 'scalar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
			self.cpoTime = retTime
		status, ret_vector, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'vector', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vector = ret_vector
			self.cpoTime = retTime
		status, ret_matrix, retTime = ull.getVect3DComplexSlice(self.idx, path, cpopath + 'matrix', inTime, interpolMode)
		check_status(status)
		if not status:
			self.matrix = ret_matrix
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type e_parastructurecomplexgrid_scalar_cplx, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidList = ull.getVect1DInt(self.idx, path, cpopath + 'griduid')
			if len(griduidList) == 0:
				griduidList = numpy.resize(griduidList, (nbslice))
			check_status(status)
			status, subgridList = ull.getVect1DInt(self.idx, path, cpopath + 'subgrid')
			if len(subgridList) == 0:
				subgridList = numpy.resize(subgridList, (nbslice))
			check_status(status)
			status, scalarList = ull.getVect2DComplex(self.idx, path, cpopath + 'scalar')
			if len(scalarList) == 0:
				scalarList = numpy.resize(scalarList, (0,nbslice))
			check_status(status)
			status, vectorList = ull.getVect3DComplex(self.idx, path, cpopath + 'vector')
			if len(vectorList) == 0:
				vectorList = numpy.resize(vectorList, (0,0,nbslice))
			check_status(status)
			status, matrixList = ull.getVect4DComplex(self.idx, path, cpopath + 'matrix')
			if len(matrixList) == 0:
				matrixList = numpy.resize(matrixList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = e_parastructurecomplexgrid_scalar_cplx(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = int(griduidList[i].copy())
				slice.subgrid = int(subgridList[i].copy())
				slice.scalar = scalarList[:,i].copy().astype(complex)
				slice.vector = vectorList[:,:,i].copy().astype(complex)
				slice.matrix = matrixList[:,:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_parastructurecomplexgrid_scalar_cplxObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DComplexInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.complex128))
		if (dev()):
			print ('putVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DComplexInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_parastructurecomplexgrid_scalar_cplxObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DComplexFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DComplexFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_parastructurecomplexgrid_scalar_cplxObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_parastructurecomplexgrid_scalar_cplxObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'subgrid')
		ull.deleteData(self.idx, path, cpopath + 'scalar')
		ull.deleteData(self.idx, path, cpopath + 'vector')
		ull.deleteData(self.idx, path, cpopath + 'matrix')


class e_normstructurecomplexgrid_scalar_cplx(KeepInOrder):
	'''
	class e_normstructurecomplexgrid_scalar_cplx
	Magnitude of wave electric field normal to a flux surface [V/m]; Time-dependent; Complexgrid_scalar

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with  complex numbers
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Complex Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with  complex numbers
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Complex matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with  complex numbers
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d complex array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='e_norm'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.complex128, order='C')
		self.vector = numpy.zeros((0,0), numpy.complex128, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class e_normstructurecomplexgrid_scalar_cplx\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_normstructurecomplexgrid_scalar_cplx, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'griduid', self.griduid, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid, self.cpoTime)
		check_status(status)
		status = ull.putVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_normstructurecomplexgrid_scalar_cplx, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid)
		check_status(status)
		status = ull.replaceLastVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_normstructurecomplexgrid_scalar_cplx, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type e_normstructurecomplexgrid_scalar_cplx, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'griduid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
			self.cpoTime = retTime
		status, ret_subgrid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'subgrid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
			self.cpoTime = retTime
		status, ret_scalar, retTime = ull.getVect1DComplexSlice(self.idx, path, cpopath + 'scalar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
			self.cpoTime = retTime
		status, ret_vector, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'vector', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vector = ret_vector
			self.cpoTime = retTime
		status, ret_matrix, retTime = ull.getVect3DComplexSlice(self.idx, path, cpopath + 'matrix', inTime, interpolMode)
		check_status(status)
		if not status:
			self.matrix = ret_matrix
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type e_normstructurecomplexgrid_scalar_cplx, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidList = ull.getVect1DInt(self.idx, path, cpopath + 'griduid')
			if len(griduidList) == 0:
				griduidList = numpy.resize(griduidList, (nbslice))
			check_status(status)
			status, subgridList = ull.getVect1DInt(self.idx, path, cpopath + 'subgrid')
			if len(subgridList) == 0:
				subgridList = numpy.resize(subgridList, (nbslice))
			check_status(status)
			status, scalarList = ull.getVect2DComplex(self.idx, path, cpopath + 'scalar')
			if len(scalarList) == 0:
				scalarList = numpy.resize(scalarList, (0,nbslice))
			check_status(status)
			status, vectorList = ull.getVect3DComplex(self.idx, path, cpopath + 'vector')
			if len(vectorList) == 0:
				vectorList = numpy.resize(vectorList, (0,0,nbslice))
			check_status(status)
			status, matrixList = ull.getVect4DComplex(self.idx, path, cpopath + 'matrix')
			if len(matrixList) == 0:
				matrixList = numpy.resize(matrixList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = e_normstructurecomplexgrid_scalar_cplx(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = int(griduidList[i].copy())
				slice.subgrid = int(subgridList[i].copy())
				slice.scalar = scalarList[:,i].copy().astype(complex)
				slice.vector = vectorList[:,:,i].copy().astype(complex)
				slice.matrix = matrixList[:,:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_normstructurecomplexgrid_scalar_cplxObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DComplexInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.complex128))
		if (dev()):
			print ('putVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DComplexInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_normstructurecomplexgrid_scalar_cplxObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DComplexFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DComplexFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_normstructurecomplexgrid_scalar_cplxObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_normstructurecomplexgrid_scalar_cplxObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'subgrid')
		ull.deleteData(self.idx, path, cpopath + 'scalar')
		ull.deleteData(self.idx, path, cpopath + 'vector')
		ull.deleteData(self.idx, path, cpopath + 'matrix')


class e_binormstructurecomplexgrid_scalar_cplx(KeepInOrder):
	'''
	class e_binormstructurecomplexgrid_scalar_cplx
	Magnitude of perpendicular (to the static magnetic field) wave electric field tangent to a flux surface [V/m]; Time-dependent; Complexgrid_scalar

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with  complex numbers
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Complex Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with  complex numbers
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Complex matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with  complex numbers
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d complex array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='e_binorm'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.complex128, order='C')
		self.vector = numpy.zeros((0,0), numpy.complex128, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class e_binormstructurecomplexgrid_scalar_cplx\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_binormstructurecomplexgrid_scalar_cplx, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'griduid', self.griduid, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid, self.cpoTime)
		check_status(status)
		status = ull.putVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_binormstructurecomplexgrid_scalar_cplx, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid)
		check_status(status)
		status = ull.replaceLastVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type e_binormstructurecomplexgrid_scalar_cplx, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type e_binormstructurecomplexgrid_scalar_cplx, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'griduid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
			self.cpoTime = retTime
		status, ret_subgrid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'subgrid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
			self.cpoTime = retTime
		status, ret_scalar, retTime = ull.getVect1DComplexSlice(self.idx, path, cpopath + 'scalar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
			self.cpoTime = retTime
		status, ret_vector, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'vector', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vector = ret_vector
			self.cpoTime = retTime
		status, ret_matrix, retTime = ull.getVect3DComplexSlice(self.idx, path, cpopath + 'matrix', inTime, interpolMode)
		check_status(status)
		if not status:
			self.matrix = ret_matrix
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type e_binormstructurecomplexgrid_scalar_cplx, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidList = ull.getVect1DInt(self.idx, path, cpopath + 'griduid')
			if len(griduidList) == 0:
				griduidList = numpy.resize(griduidList, (nbslice))
			check_status(status)
			status, subgridList = ull.getVect1DInt(self.idx, path, cpopath + 'subgrid')
			if len(subgridList) == 0:
				subgridList = numpy.resize(subgridList, (nbslice))
			check_status(status)
			status, scalarList = ull.getVect2DComplex(self.idx, path, cpopath + 'scalar')
			if len(scalarList) == 0:
				scalarList = numpy.resize(scalarList, (0,nbslice))
			check_status(status)
			status, vectorList = ull.getVect3DComplex(self.idx, path, cpopath + 'vector')
			if len(vectorList) == 0:
				vectorList = numpy.resize(vectorList, (0,0,nbslice))
			check_status(status)
			status, matrixList = ull.getVect4DComplex(self.idx, path, cpopath + 'matrix')
			if len(matrixList) == 0:
				matrixList = numpy.resize(matrixList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = e_binormstructurecomplexgrid_scalar_cplx(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = int(griduidList[i].copy())
				slice.subgrid = int(subgridList[i].copy())
				slice.scalar = scalarList[:,i].copy().astype(complex)
				slice.vector = vectorList[:,:,i].copy().astype(complex)
				slice.matrix = matrixList[:,:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_binormstructurecomplexgrid_scalar_cplxObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DComplexInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.complex128))
		if (dev()):
			print ('putVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DComplexInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_binormstructurecomplexgrid_scalar_cplxObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DComplexFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DComplexFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_binormstructurecomplexgrid_scalar_cplxObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type e_binormstructurecomplexgrid_scalar_cplxObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'subgrid')
		ull.deleteData(self.idx, path, cpopath + 'scalar')
		ull.deleteData(self.idx, path, cpopath + 'vector')
		ull.deleteData(self.idx, path, cpopath + 'matrix')


class b_normstructurecomplexgrid_scalar_cplx(KeepInOrder):
	'''
	class b_normstructurecomplexgrid_scalar_cplx
	Magnitude of perpendicular (to the static magnetic field) wave magnetic field normal to a flux surface [T]; Time-dependent; Complexgrid_scalar

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with  complex numbers
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Complex Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with  complex numbers
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Complex matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with  complex numbers
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d complex array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='b_norm'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.complex128, order='C')
		self.vector = numpy.zeros((0,0), numpy.complex128, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class b_normstructurecomplexgrid_scalar_cplx\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type b_normstructurecomplexgrid_scalar_cplx, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'griduid', self.griduid, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid, self.cpoTime)
		check_status(status)
		status = ull.putVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type b_normstructurecomplexgrid_scalar_cplx, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid)
		check_status(status)
		status = ull.replaceLastVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type b_normstructurecomplexgrid_scalar_cplx, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type b_normstructurecomplexgrid_scalar_cplx, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'griduid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
			self.cpoTime = retTime
		status, ret_subgrid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'subgrid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
			self.cpoTime = retTime
		status, ret_scalar, retTime = ull.getVect1DComplexSlice(self.idx, path, cpopath + 'scalar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
			self.cpoTime = retTime
		status, ret_vector, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'vector', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vector = ret_vector
			self.cpoTime = retTime
		status, ret_matrix, retTime = ull.getVect3DComplexSlice(self.idx, path, cpopath + 'matrix', inTime, interpolMode)
		check_status(status)
		if not status:
			self.matrix = ret_matrix
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type b_normstructurecomplexgrid_scalar_cplx, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidList = ull.getVect1DInt(self.idx, path, cpopath + 'griduid')
			if len(griduidList) == 0:
				griduidList = numpy.resize(griduidList, (nbslice))
			check_status(status)
			status, subgridList = ull.getVect1DInt(self.idx, path, cpopath + 'subgrid')
			if len(subgridList) == 0:
				subgridList = numpy.resize(subgridList, (nbslice))
			check_status(status)
			status, scalarList = ull.getVect2DComplex(self.idx, path, cpopath + 'scalar')
			if len(scalarList) == 0:
				scalarList = numpy.resize(scalarList, (0,nbslice))
			check_status(status)
			status, vectorList = ull.getVect3DComplex(self.idx, path, cpopath + 'vector')
			if len(vectorList) == 0:
				vectorList = numpy.resize(vectorList, (0,0,nbslice))
			check_status(status)
			status, matrixList = ull.getVect4DComplex(self.idx, path, cpopath + 'matrix')
			if len(matrixList) == 0:
				matrixList = numpy.resize(matrixList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = b_normstructurecomplexgrid_scalar_cplx(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = int(griduidList[i].copy())
				slice.subgrid = int(subgridList[i].copy())
				slice.scalar = scalarList[:,i].copy().astype(complex)
				slice.vector = vectorList[:,:,i].copy().astype(complex)
				slice.matrix = matrixList[:,:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_normstructurecomplexgrid_scalar_cplxObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DComplexInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.complex128))
		if (dev()):
			print ('putVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DComplexInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_normstructurecomplexgrid_scalar_cplxObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DComplexFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DComplexFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_normstructurecomplexgrid_scalar_cplxObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_normstructurecomplexgrid_scalar_cplxObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'subgrid')
		ull.deleteData(self.idx, path, cpopath + 'scalar')
		ull.deleteData(self.idx, path, cpopath + 'vector')
		ull.deleteData(self.idx, path, cpopath + 'matrix')


class b_binormstructurecomplexgrid_scalar_cplx(KeepInOrder):
	'''
	class b_binormstructurecomplexgrid_scalar_cplx
	Magnitude of wave magnetic field tangent to a flux surface [T]; Time-dependent; Complexgrid_scalar

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with  complex numbers
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Complex Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with  complex numbers
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Complex matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with  complex numbers
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d complex array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='b_binorm'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.complex128, order='C')
		self.vector = numpy.zeros((0,0), numpy.complex128, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class b_binormstructurecomplexgrid_scalar_cplx\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type b_binormstructurecomplexgrid_scalar_cplx, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'griduid', self.griduid, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid, self.cpoTime)
		check_status(status)
		status = ull.putVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type b_binormstructurecomplexgrid_scalar_cplx, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid)
		check_status(status)
		status = ull.replaceLastVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type b_binormstructurecomplexgrid_scalar_cplx, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type b_binormstructurecomplexgrid_scalar_cplx, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'griduid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
			self.cpoTime = retTime
		status, ret_subgrid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'subgrid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
			self.cpoTime = retTime
		status, ret_scalar, retTime = ull.getVect1DComplexSlice(self.idx, path, cpopath + 'scalar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
			self.cpoTime = retTime
		status, ret_vector, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'vector', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vector = ret_vector
			self.cpoTime = retTime
		status, ret_matrix, retTime = ull.getVect3DComplexSlice(self.idx, path, cpopath + 'matrix', inTime, interpolMode)
		check_status(status)
		if not status:
			self.matrix = ret_matrix
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type b_binormstructurecomplexgrid_scalar_cplx, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidList = ull.getVect1DInt(self.idx, path, cpopath + 'griduid')
			if len(griduidList) == 0:
				griduidList = numpy.resize(griduidList, (nbslice))
			check_status(status)
			status, subgridList = ull.getVect1DInt(self.idx, path, cpopath + 'subgrid')
			if len(subgridList) == 0:
				subgridList = numpy.resize(subgridList, (nbslice))
			check_status(status)
			status, scalarList = ull.getVect2DComplex(self.idx, path, cpopath + 'scalar')
			if len(scalarList) == 0:
				scalarList = numpy.resize(scalarList, (0,nbslice))
			check_status(status)
			status, vectorList = ull.getVect3DComplex(self.idx, path, cpopath + 'vector')
			if len(vectorList) == 0:
				vectorList = numpy.resize(vectorList, (0,0,nbslice))
			check_status(status)
			status, matrixList = ull.getVect4DComplex(self.idx, path, cpopath + 'matrix')
			if len(matrixList) == 0:
				matrixList = numpy.resize(matrixList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = b_binormstructurecomplexgrid_scalar_cplx(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = int(griduidList[i].copy())
				slice.subgrid = int(subgridList[i].copy())
				slice.scalar = scalarList[:,i].copy().astype(complex)
				slice.vector = vectorList[:,:,i].copy().astype(complex)
				slice.matrix = matrixList[:,:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_binormstructurecomplexgrid_scalar_cplxObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DComplexInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.complex128))
		if (dev()):
			print ('putVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DComplexInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_binormstructurecomplexgrid_scalar_cplxObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DComplexFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DComplexFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_binormstructurecomplexgrid_scalar_cplxObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_binormstructurecomplexgrid_scalar_cplxObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'subgrid')
		ull.deleteData(self.idx, path, cpopath + 'scalar')
		ull.deleteData(self.idx, path, cpopath + 'vector')
		ull.deleteData(self.idx, path, cpopath + 'matrix')


class b_parastructurecomplexgrid_scalar_cplx(KeepInOrder):
	'''
	class b_parastructurecomplexgrid_scalar_cplx
	Magnitude of wave magnetic field parallel to the equilibrium magnetic field [T]; Time-dependent; Complexgrid_scalar

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with  complex numbers
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Complex Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with  complex numbers
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Complex matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with  complex numbers
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d complex array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='b_para'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.complex128, order='C')
		self.vector = numpy.zeros((0,0), numpy.complex128, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class b_parastructurecomplexgrid_scalar_cplx\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type b_parastructurecomplexgrid_scalar_cplx, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'griduid', self.griduid, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid, self.cpoTime)
		check_status(status)
		status = ull.putVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type b_parastructurecomplexgrid_scalar_cplx, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid)
		check_status(status)
		status = ull.replaceLastVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type b_parastructurecomplexgrid_scalar_cplx, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type b_parastructurecomplexgrid_scalar_cplx, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'griduid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
			self.cpoTime = retTime
		status, ret_subgrid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'subgrid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
			self.cpoTime = retTime
		status, ret_scalar, retTime = ull.getVect1DComplexSlice(self.idx, path, cpopath + 'scalar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
			self.cpoTime = retTime
		status, ret_vector, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'vector', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vector = ret_vector
			self.cpoTime = retTime
		status, ret_matrix, retTime = ull.getVect3DComplexSlice(self.idx, path, cpopath + 'matrix', inTime, interpolMode)
		check_status(status)
		if not status:
			self.matrix = ret_matrix
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type b_parastructurecomplexgrid_scalar_cplx, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidList = ull.getVect1DInt(self.idx, path, cpopath + 'griduid')
			if len(griduidList) == 0:
				griduidList = numpy.resize(griduidList, (nbslice))
			check_status(status)
			status, subgridList = ull.getVect1DInt(self.idx, path, cpopath + 'subgrid')
			if len(subgridList) == 0:
				subgridList = numpy.resize(subgridList, (nbslice))
			check_status(status)
			status, scalarList = ull.getVect2DComplex(self.idx, path, cpopath + 'scalar')
			if len(scalarList) == 0:
				scalarList = numpy.resize(scalarList, (0,nbslice))
			check_status(status)
			status, vectorList = ull.getVect3DComplex(self.idx, path, cpopath + 'vector')
			if len(vectorList) == 0:
				vectorList = numpy.resize(vectorList, (0,0,nbslice))
			check_status(status)
			status, matrixList = ull.getVect4DComplex(self.idx, path, cpopath + 'matrix')
			if len(matrixList) == 0:
				matrixList = numpy.resize(matrixList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = b_parastructurecomplexgrid_scalar_cplx(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = int(griduidList[i].copy())
				slice.subgrid = int(subgridList[i].copy())
				slice.scalar = scalarList[:,i].copy().astype(complex)
				slice.vector = vectorList[:,:,i].copy().astype(complex)
				slice.matrix = matrixList[:,:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_parastructurecomplexgrid_scalar_cplxObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DComplexInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.complex128))
		if (dev()):
			print ('putVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DComplexInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_parastructurecomplexgrid_scalar_cplxObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DComplexFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DComplexFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_parastructurecomplexgrid_scalar_cplxObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type b_parastructurecomplexgrid_scalar_cplxObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'subgrid')
		ull.deleteData(self.idx, path, cpopath + 'scalar')
		ull.deleteData(self.idx, path, cpopath + 'vector')
		ull.deleteData(self.idx, path, cpopath + 'matrix')


class k_perpstructurecomplexgrid_scalar_cplx(KeepInOrder):
	'''
	class k_perpstructurecomplexgrid_scalar_cplx
	Perpendicular wave number [1/m]; Time-dependent; Complexgrid_scalar

	Attributes:
	- griduid : int
	   Unique identifier of the grid this scalar quantity is associated with.
	- subgrid : int
	   Index of the subgrid (as stored in grid.subgrids) the data is stored on.
	- scalar : numpy.ndarray 1D with  complex numbers
	   Scalar representation of data. One scalar entry is stored per object in the subgrid.; The order is implicitly defined by the subgrid.; Complex Vector(nobjects_subgrid). First dimension: object index.
	- vector : numpy.ndarray 2D with  complex numbers
	   Vector representation of data. One vector is stored per object in the subgrid. The order is implicitly defined by the subgrid.; Complex matrix(nobjects_subgrid, ndata).First dimension: object index, second dimension: index of data vector.
	- matrix : numpy.ndarray 3D with  complex numbers
	   Matrix representation of data. One matrix is stored per object in the subgrid. The order is implicitly defined by the subgrid.; 3d complex array(nobjects_subgrid,ndata1,ndata2). First dimension: object index, second dimension: matrix row, third dimension: matrix column.
	'''

	def __init__(self, base_path_in='k_perp'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.subgrid = EMPTY_INT
		self.scalar = numpy.zeros(0, numpy.complex128, order='C')
		self.vector = numpy.zeros((0,0), numpy.complex128, order='C')
		self.matrix = numpy.zeros((0,0,0), numpy.complex128, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class k_perpstructurecomplexgrid_scalar_cplx\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute subgrid: ' + str(self.subgrid) + '\n'
		s = self.scalar.__str__()
		ret = ret + space + 'Attribute scalar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vector.__str__()
		ret = ret + space + 'Attribute vector\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.matrix.__str__()
		ret = ret + space + 'Attribute matrix\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type k_perpstructurecomplexgrid_scalar_cplx, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'griduid', self.griduid, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid, self.cpoTime)
		check_status(status)
		status = ull.putVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128), self.cpoTime)
		check_status(status)
		status = ull.putVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type k_perpstructurecomplexgrid_scalar_cplx, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'subgrid', self.subgrid)
		check_status(status)
		status = ull.replaceLastVect1DComplexSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect2DComplexSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.complex128))
		check_status(status)
		status = ull.replaceLastVect3DComplexSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.complex128))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type k_perpstructurecomplexgrid_scalar_cplx, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type k_perpstructurecomplexgrid_scalar_cplx, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'griduid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
			self.cpoTime = retTime
		status, ret_subgrid, retTime = ull.getIntSlice(self.idx, path, cpopath + 'subgrid', inTime, interpolMode)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
			self.cpoTime = retTime
		status, ret_scalar, retTime = ull.getVect1DComplexSlice(self.idx, path, cpopath + 'scalar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
			self.cpoTime = retTime
		status, ret_vector, retTime = ull.getVect2DComplexSlice(self.idx, path, cpopath + 'vector', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vector = ret_vector
			self.cpoTime = retTime
		status, ret_matrix, retTime = ull.getVect3DComplexSlice(self.idx, path, cpopath + 'matrix', inTime, interpolMode)
		check_status(status)
		if not status:
			self.matrix = ret_matrix
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type k_perpstructurecomplexgrid_scalar_cplx, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidList = ull.getVect1DInt(self.idx, path, cpopath + 'griduid')
			if len(griduidList) == 0:
				griduidList = numpy.resize(griduidList, (nbslice))
			check_status(status)
			status, subgridList = ull.getVect1DInt(self.idx, path, cpopath + 'subgrid')
			if len(subgridList) == 0:
				subgridList = numpy.resize(subgridList, (nbslice))
			check_status(status)
			status, scalarList = ull.getVect2DComplex(self.idx, path, cpopath + 'scalar')
			if len(scalarList) == 0:
				scalarList = numpy.resize(scalarList, (0,nbslice))
			check_status(status)
			status, vectorList = ull.getVect3DComplex(self.idx, path, cpopath + 'vector')
			if len(vectorList) == 0:
				vectorList = numpy.resize(vectorList, (0,0,nbslice))
			check_status(status)
			status, matrixList = ull.getVect4DComplex(self.idx, path, cpopath + 'matrix')
			if len(matrixList) == 0:
				matrixList = numpy.resize(matrixList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = k_perpstructurecomplexgrid_scalar_cplx(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = int(griduidList[i].copy())
				slice.subgrid = int(subgridList[i].copy())
				slice.scalar = scalarList[:,i].copy().astype(complex)
				slice.vector = vectorList[:,:,i].copy().astype(complex)
				slice.matrix = matrixList[:,:,:,i].copy().astype(complex)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type k_perpstructurecomplexgrid_scalar_cplxObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'subgrid', i, self.subgrid)
		if (dev()):
			print ('putVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DComplexInObject(self.idx, obj, cpopath + 'scalar', i, numpy.array(self.scalar).astype(numpy.complex128))
		if (dev()):
			print ('putVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DComplexInObject(self.idx, obj, cpopath + 'vector', i, numpy.array(self.vector).astype(numpy.complex128))
		if (dev()):
			print ('putVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DComplexInObject(self.idx, obj, cpopath + 'matrix', i, numpy.array(self.matrix).astype(numpy.complex128))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type k_perpstructurecomplexgrid_scalar_cplxObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'subgrid') 
			print ('obj = ' + str(obj))
		status, ret_subgrid = ull.getIntFromObject(self.idx, obj, cpopath + 'subgrid', i)
		check_status(status)
		if not status:
			self.subgrid = ret_subgrid
		if (dev()):
			print ('getVect1DComplexInObject : ' + cpopath + 'scalar') 
			print ('obj = ' + str(obj))
		status, ret_scalar = ull.getVect1DComplexFromObject(self.idx, obj, cpopath + 'scalar', i)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
		if (dev()):
			print ('getVect2DComplexInObject : ' + cpopath + 'vector') 
			print ('obj = ' + str(obj))
		status, ret_vector = ull.getVect2DComplexFromObject(self.idx, obj, cpopath + 'vector', i)
		check_status(status)
		if not status:
			self.vector = ret_vector
		if (dev()):
			print ('getVect3DComplexInObject : ' + cpopath + 'matrix') 
			print ('obj = ' + str(obj))
		status, ret_matrix = ull.getVect3DComplexFromObject(self.idx, obj, cpopath + 'matrix', i)
		check_status(status)
		if not status:
			self.matrix = ret_matrix

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type k_perpstructurecomplexgrid_scalar_cplxObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type k_perpstructurecomplexgrid_scalar_cplxObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'subgrid')
		ull.deleteData(self.idx, path, cpopath + 'scalar')
		ull.deleteData(self.idx, path, cpopath + 'vector')
		ull.deleteData(self.idx, path, cpopath + 'matrix')


class pol_decompstructurepol_decomp(KeepInOrder):
	'''
	class pol_decompstructurepol_decomp
	TO BE REMOVED, being replaced by e_components and grid. Kept only to make smooth transition between data-type versions. [Poloidal decomposition of the wave fields. Uses the flux surface grid in grid_1d.]

	Attributes:
	- mpol : numpy.ndarray 1D with int)
	   Poloidal mode numbers; Vector (nmpol)
	- e_plus : numpy.ndarray 3D with float
	   Magnitude of poloidal Fourier decomposition of left hand polarised component of the wave electric field [V/m]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- e_plus_ph : numpy.ndarray 3D with float
	   Phase of poloidal Fourier decomposition of left hand polarised component of the wave electric field [rad]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- e_minus : numpy.ndarray 3D with float
	   Magnitude of poloidal Fourier decomposition of right hand polarised component of the wave electric field; Time-dependent (V/m); Array 3D (ntor, npsi, nmpol)
	- e_minus_ph : numpy.ndarray 3D with float
	   Phase of poloidal Fourier decomposition of right hand polarised component of the wave electric field [rad]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- e_norm : numpy.ndarray 3D with float
	   Magnitude of poloidal Fourier decomposition of  wave electric field normal to a flux surface [V/m]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- e_norm_ph : numpy.ndarray 3D with float
	   Phase of poloidal Fourier decomposition of  wave electric field normal to a flux surface [rad]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- e_binorm : numpy.ndarray 3D with float
	   Magnitude of poloidal Fourier decomposition of wave electric field tangent to a flux surface [V/m]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- e_binorm_ph : numpy.ndarray 3D with float
	   Phase of poloidal Fourier decomposition of wave electric field tangent to a flux surface [rad]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- e_para : numpy.ndarray 3D with float
	   Magnitude of poloidal Fourier decomposition of parallel wave electric field [V/m]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- e_para_ph : numpy.ndarray 3D with float
	   Phase of poloidal Fourier decomposition of parallel wave electric field [rad]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- b_norm : numpy.ndarray 3D with float
	   Magnitude of poloidal Fourier decomposition of wave magnetic field normal to a flux surface [T]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- b_norm_ph : numpy.ndarray 3D with float
	   Phase of poloidal Fourier decomposition of normal wave magnetic field [rad]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- b_binorm : numpy.ndarray 3D with float
	   Magnitude of poloidal Fourier decomposition of wave magnetic field tangent to a flux surface [T]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- b_binorm_ph : numpy.ndarray 3D with float
	   Phase of poloidal Fourier decomposition of wave magnetic field tangent to a flux surface [rad]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- b_para : numpy.ndarray 3D with float
	   Magnitude of Fourier decomposition of wave magnetic field parallel to the equilibrium magnetic field [T]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- b_para_ph : numpy.ndarray 3D with float
	   Phase of Fourier decomposition of wave magnetic field parallel to the equilibrium magnetic field [T]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	- k_perp : numpy.ndarray 3D with float
	   Perpendicular wave number [T]; Time-dependent; Array 3D (ntor, npsi, nmpol)
	'''

	def __init__(self, base_path_in='pol_decomp'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.mpol = numpy.zeros(0, numpy.int32, order='C')
		self.e_plus = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_plus_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_minus = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_minus_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_norm = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_norm_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_binorm = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_binorm_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_para = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_para_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.b_norm = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.b_norm_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.b_binorm = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.b_binorm_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.b_para = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.b_para_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.k_perp = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pol_decompstructurepol_decomp\n'
		s = self.mpol.__str__()
		ret = ret + space + 'Attribute mpol\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_plus.__str__()
		ret = ret + space + 'Attribute e_plus\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_plus_ph.__str__()
		ret = ret + space + 'Attribute e_plus_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_minus.__str__()
		ret = ret + space + 'Attribute e_minus\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_minus_ph.__str__()
		ret = ret + space + 'Attribute e_minus_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_norm.__str__()
		ret = ret + space + 'Attribute e_norm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_norm_ph.__str__()
		ret = ret + space + 'Attribute e_norm_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_binorm.__str__()
		ret = ret + space + 'Attribute e_binorm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_binorm_ph.__str__()
		ret = ret + space + 'Attribute e_binorm_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_para.__str__()
		ret = ret + space + 'Attribute e_para\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_para_ph.__str__()
		ret = ret + space + 'Attribute e_para_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_norm.__str__()
		ret = ret + space + 'Attribute b_norm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_norm_ph.__str__()
		ret = ret + space + 'Attribute b_norm_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_binorm.__str__()
		ret = ret + space + 'Attribute b_binorm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_binorm_ph.__str__()
		ret = ret + space + 'Attribute b_binorm_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_para.__str__()
		ret = ret + space + 'Attribute b_para\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_para_ph.__str__()
		ret = ret + space + 'Attribute b_para_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.k_perp.__str__()
		ret = ret + space + 'Attribute k_perp\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pol_decompstructurepol_decomp, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_plus', numpy.array(self.e_plus).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_plus_ph', numpy.array(self.e_plus_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_minus', numpy.array(self.e_minus).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_minus_ph', numpy.array(self.e_minus_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_norm', numpy.array(self.e_norm).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_norm_ph', numpy.array(self.e_norm_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_binorm', numpy.array(self.e_binorm).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_binorm_ph', numpy.array(self.e_binorm_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_para', numpy.array(self.e_para).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_para_ph', numpy.array(self.e_para_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'b_norm', numpy.array(self.b_norm).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'b_norm_ph', numpy.array(self.b_norm_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'b_binorm', numpy.array(self.b_binorm).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'b_binorm_ph', numpy.array(self.b_binorm_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'b_para', numpy.array(self.b_para).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'b_para_ph', numpy.array(self.b_para_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'k_perp', numpy.array(self.k_perp).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pol_decompstructurepol_decomp, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_plus', numpy.array(self.e_plus).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_plus_ph', numpy.array(self.e_plus_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_minus', numpy.array(self.e_minus).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_minus_ph', numpy.array(self.e_minus_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_norm', numpy.array(self.e_norm).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_norm_ph', numpy.array(self.e_norm_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_binorm', numpy.array(self.e_binorm).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_binorm_ph', numpy.array(self.e_binorm_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_para', numpy.array(self.e_para).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_para_ph', numpy.array(self.e_para_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'b_norm', numpy.array(self.b_norm).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'b_norm_ph', numpy.array(self.b_norm_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'b_binorm', numpy.array(self.b_binorm).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'b_binorm_ph', numpy.array(self.b_binorm_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'b_para', numpy.array(self.b_para).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'b_para_ph', numpy.array(self.b_para_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'k_perp', numpy.array(self.k_perp).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pol_decompstructurepol_decomp, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DInt(self.idx, path, cpopath + 'mpol', numpy.array(self.mpol).astype(numpy.int32), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pol_decompstructurepol_decomp, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_mpol = ull.getVect1DInt(self.idx, path, cpopath + 'mpol')
		check_status(status)
		if not status:
			self.mpol = ret_mpol
		status, ret_e_plus, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_plus', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_plus = ret_e_plus
			self.cpoTime = retTime
		status, ret_e_plus_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_plus_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_plus_ph = ret_e_plus_ph
			self.cpoTime = retTime
		status, ret_e_minus, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_minus', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_minus = ret_e_minus
			self.cpoTime = retTime
		status, ret_e_minus_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_minus_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_minus_ph = ret_e_minus_ph
			self.cpoTime = retTime
		status, ret_e_norm, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_norm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_norm = ret_e_norm
			self.cpoTime = retTime
		status, ret_e_norm_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_norm_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_norm_ph = ret_e_norm_ph
			self.cpoTime = retTime
		status, ret_e_binorm, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_binorm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_binorm = ret_e_binorm
			self.cpoTime = retTime
		status, ret_e_binorm_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_binorm_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_binorm_ph = ret_e_binorm_ph
			self.cpoTime = retTime
		status, ret_e_para, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_para', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_para = ret_e_para
			self.cpoTime = retTime
		status, ret_e_para_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_para_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_para_ph = ret_e_para_ph
			self.cpoTime = retTime
		status, ret_b_norm, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'b_norm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_norm = ret_b_norm
			self.cpoTime = retTime
		status, ret_b_norm_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'b_norm_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_norm_ph = ret_b_norm_ph
			self.cpoTime = retTime
		status, ret_b_binorm, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'b_binorm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_binorm = ret_b_binorm
			self.cpoTime = retTime
		status, ret_b_binorm_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'b_binorm_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_binorm_ph = ret_b_binorm_ph
			self.cpoTime = retTime
		status, ret_b_para, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'b_para', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_para = ret_b_para
			self.cpoTime = retTime
		status, ret_b_para_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'b_para_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_para_ph = ret_b_para_ph
			self.cpoTime = retTime
		status, ret_k_perp, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'k_perp', inTime, interpolMode)
		check_status(status)
		if not status:
			self.k_perp = ret_k_perp
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pol_decompstructurepol_decomp, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, mpolVal = ull.getVect1DInt(self.idx, path, cpopath + 'mpol')
			check_status(status)
			status, e_plusList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_plus')
			if len(e_plusList) == 0:
				e_plusList = numpy.resize(e_plusList, (0,0,0,nbslice))
			check_status(status)
			status, e_plus_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_plus_ph')
			if len(e_plus_phList) == 0:
				e_plus_phList = numpy.resize(e_plus_phList, (0,0,0,nbslice))
			check_status(status)
			status, e_minusList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_minus')
			if len(e_minusList) == 0:
				e_minusList = numpy.resize(e_minusList, (0,0,0,nbslice))
			check_status(status)
			status, e_minus_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_minus_ph')
			if len(e_minus_phList) == 0:
				e_minus_phList = numpy.resize(e_minus_phList, (0,0,0,nbslice))
			check_status(status)
			status, e_normList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_norm')
			if len(e_normList) == 0:
				e_normList = numpy.resize(e_normList, (0,0,0,nbslice))
			check_status(status)
			status, e_norm_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_norm_ph')
			if len(e_norm_phList) == 0:
				e_norm_phList = numpy.resize(e_norm_phList, (0,0,0,nbslice))
			check_status(status)
			status, e_binormList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_binorm')
			if len(e_binormList) == 0:
				e_binormList = numpy.resize(e_binormList, (0,0,0,nbslice))
			check_status(status)
			status, e_binorm_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_binorm_ph')
			if len(e_binorm_phList) == 0:
				e_binorm_phList = numpy.resize(e_binorm_phList, (0,0,0,nbslice))
			check_status(status)
			status, e_paraList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_para')
			if len(e_paraList) == 0:
				e_paraList = numpy.resize(e_paraList, (0,0,0,nbslice))
			check_status(status)
			status, e_para_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_para_ph')
			if len(e_para_phList) == 0:
				e_para_phList = numpy.resize(e_para_phList, (0,0,0,nbslice))
			check_status(status)
			status, b_normList = ull.getVect4DDouble(self.idx, path, cpopath + 'b_norm')
			if len(b_normList) == 0:
				b_normList = numpy.resize(b_normList, (0,0,0,nbslice))
			check_status(status)
			status, b_norm_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'b_norm_ph')
			if len(b_norm_phList) == 0:
				b_norm_phList = numpy.resize(b_norm_phList, (0,0,0,nbslice))
			check_status(status)
			status, b_binormList = ull.getVect4DDouble(self.idx, path, cpopath + 'b_binorm')
			if len(b_binormList) == 0:
				b_binormList = numpy.resize(b_binormList, (0,0,0,nbslice))
			check_status(status)
			status, b_binorm_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'b_binorm_ph')
			if len(b_binorm_phList) == 0:
				b_binorm_phList = numpy.resize(b_binorm_phList, (0,0,0,nbslice))
			check_status(status)
			status, b_paraList = ull.getVect4DDouble(self.idx, path, cpopath + 'b_para')
			if len(b_paraList) == 0:
				b_paraList = numpy.resize(b_paraList, (0,0,0,nbslice))
			check_status(status)
			status, b_para_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'b_para_ph')
			if len(b_para_phList) == 0:
				b_para_phList = numpy.resize(b_para_phList, (0,0,0,nbslice))
			check_status(status)
			status, k_perpList = ull.getVect4DDouble(self.idx, path, cpopath + 'k_perp')
			if len(k_perpList) == 0:
				k_perpList = numpy.resize(k_perpList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = pol_decompstructurepol_decomp(self.base_path)
				slice.setExpIdx(self.idx)
				slice.mpol = mpolVal
				slice.e_plus = e_plusList[:,:,:,i]
				slice.e_plus_ph = e_plus_phList[:,:,:,i]
				slice.e_minus = e_minusList[:,:,:,i]
				slice.e_minus_ph = e_minus_phList[:,:,:,i]
				slice.e_norm = e_normList[:,:,:,i]
				slice.e_norm_ph = e_norm_phList[:,:,:,i]
				slice.e_binorm = e_binormList[:,:,:,i]
				slice.e_binorm_ph = e_binorm_phList[:,:,:,i]
				slice.e_para = e_paraList[:,:,:,i]
				slice.e_para_ph = e_para_phList[:,:,:,i]
				slice.b_norm = b_normList[:,:,:,i]
				slice.b_norm_ph = b_norm_phList[:,:,:,i]
				slice.b_binorm = b_binormList[:,:,:,i]
				slice.b_binorm_ph = b_binorm_phList[:,:,:,i]
				slice.b_para = b_paraList[:,:,:,i]
				slice.b_para_ph = b_para_phList[:,:,:,i]
				slice.k_perp = k_perpList[:,:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pol_decompstructurepol_decompObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_plus') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_plus', i, numpy.array(self.e_plus).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_plus_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_plus_ph', i, numpy.array(self.e_plus_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_minus') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_minus', i, numpy.array(self.e_minus).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_minus_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_minus_ph', i, numpy.array(self.e_minus_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_norm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_norm', i, numpy.array(self.e_norm).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_norm_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_norm_ph', i, numpy.array(self.e_norm_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_binorm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_binorm', i, numpy.array(self.e_binorm).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_binorm_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_binorm_ph', i, numpy.array(self.e_binorm_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_para') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_para', i, numpy.array(self.e_para).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_para_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_para_ph', i, numpy.array(self.e_para_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'b_norm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'b_norm', i, numpy.array(self.b_norm).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'b_norm_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'b_norm_ph', i, numpy.array(self.b_norm_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'b_binorm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'b_binorm', i, numpy.array(self.b_binorm).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'b_binorm_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'b_binorm_ph', i, numpy.array(self.b_binorm_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'b_para') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'b_para', i, numpy.array(self.b_para).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'b_para_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'b_para_ph', i, numpy.array(self.b_para_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'k_perp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'k_perp', i, numpy.array(self.k_perp).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pol_decompstructurepol_decompObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_plus') 
			print ('obj = ' + str(obj))
		status, ret_e_plus = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_plus', i)
		check_status(status)
		if not status:
			self.e_plus = ret_e_plus
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_plus_ph') 
			print ('obj = ' + str(obj))
		status, ret_e_plus_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_plus_ph', i)
		check_status(status)
		if not status:
			self.e_plus_ph = ret_e_plus_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_minus') 
			print ('obj = ' + str(obj))
		status, ret_e_minus = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_minus', i)
		check_status(status)
		if not status:
			self.e_minus = ret_e_minus
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_minus_ph') 
			print ('obj = ' + str(obj))
		status, ret_e_minus_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_minus_ph', i)
		check_status(status)
		if not status:
			self.e_minus_ph = ret_e_minus_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_norm') 
			print ('obj = ' + str(obj))
		status, ret_e_norm = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_norm', i)
		check_status(status)
		if not status:
			self.e_norm = ret_e_norm
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_norm_ph') 
			print ('obj = ' + str(obj))
		status, ret_e_norm_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_norm_ph', i)
		check_status(status)
		if not status:
			self.e_norm_ph = ret_e_norm_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_binorm') 
			print ('obj = ' + str(obj))
		status, ret_e_binorm = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_binorm', i)
		check_status(status)
		if not status:
			self.e_binorm = ret_e_binorm
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_binorm_ph') 
			print ('obj = ' + str(obj))
		status, ret_e_binorm_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_binorm_ph', i)
		check_status(status)
		if not status:
			self.e_binorm_ph = ret_e_binorm_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_para') 
			print ('obj = ' + str(obj))
		status, ret_e_para = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_para', i)
		check_status(status)
		if not status:
			self.e_para = ret_e_para
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_para_ph') 
			print ('obj = ' + str(obj))
		status, ret_e_para_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_para_ph', i)
		check_status(status)
		if not status:
			self.e_para_ph = ret_e_para_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'b_norm') 
			print ('obj = ' + str(obj))
		status, ret_b_norm = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'b_norm', i)
		check_status(status)
		if not status:
			self.b_norm = ret_b_norm
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'b_norm_ph') 
			print ('obj = ' + str(obj))
		status, ret_b_norm_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'b_norm_ph', i)
		check_status(status)
		if not status:
			self.b_norm_ph = ret_b_norm_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'b_binorm') 
			print ('obj = ' + str(obj))
		status, ret_b_binorm = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'b_binorm', i)
		check_status(status)
		if not status:
			self.b_binorm = ret_b_binorm
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'b_binorm_ph') 
			print ('obj = ' + str(obj))
		status, ret_b_binorm_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'b_binorm_ph', i)
		check_status(status)
		if not status:
			self.b_binorm_ph = ret_b_binorm_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'b_para') 
			print ('obj = ' + str(obj))
		status, ret_b_para = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'b_para', i)
		check_status(status)
		if not status:
			self.b_para = ret_b_para
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'b_para_ph') 
			print ('obj = ' + str(obj))
		status, ret_b_para_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'b_para_ph', i)
		check_status(status)
		if not status:
			self.b_para_ph = ret_b_para_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'k_perp') 
			print ('obj = ' + str(obj))
		status, ret_k_perp = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'k_perp', i)
		check_status(status)
		if not status:
			self.k_perp = ret_k_perp

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pol_decompstructurepol_decompObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'mpol') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'mpol', i, numpy.array(self.mpol).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pol_decompstructurepol_decompObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'mpol') 
			print ('obj = ' + str(obj))
		status, ret_mpol = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'mpol', i)
		check_status(status)
		if not status:
			self.mpol = ret_mpol

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'mpol')
		ull.deleteData(self.idx, path, cpopath + 'e_plus')
		ull.deleteData(self.idx, path, cpopath + 'e_plus_ph')
		ull.deleteData(self.idx, path, cpopath + 'e_minus')
		ull.deleteData(self.idx, path, cpopath + 'e_minus_ph')
		ull.deleteData(self.idx, path, cpopath + 'e_norm')
		ull.deleteData(self.idx, path, cpopath + 'e_norm_ph')
		ull.deleteData(self.idx, path, cpopath + 'e_binorm')
		ull.deleteData(self.idx, path, cpopath + 'e_binorm_ph')
		ull.deleteData(self.idx, path, cpopath + 'e_para')
		ull.deleteData(self.idx, path, cpopath + 'e_para_ph')
		ull.deleteData(self.idx, path, cpopath + 'b_norm')
		ull.deleteData(self.idx, path, cpopath + 'b_norm_ph')
		ull.deleteData(self.idx, path, cpopath + 'b_binorm')
		ull.deleteData(self.idx, path, cpopath + 'b_binorm_ph')
		ull.deleteData(self.idx, path, cpopath + 'b_para')
		ull.deleteData(self.idx, path, cpopath + 'b_para_ph')
		ull.deleteData(self.idx, path, cpopath + 'k_perp')


class localstructurelocal(KeepInOrder):
	'''
	class localstructurelocal
	TO BE REMOVED, being replaced by e_components and grid. Kept only to make smooth transition between data-type versions. [Local description of the wave fields. Uses the grid in grid_2d].

	Attributes:
	- e_plus : numpy.ndarray 3D with float
	   Magnitude of left hand polarised component of the wave electric field [V/m]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	- e_plus_ph : numpy.ndarray 3D with float
	   Phase of left hand polarised component of the wave electric field [rad]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	- e_minus : numpy.ndarray 3D with float
	   Magnitude of right hand polarised component of the wave electric field [v/m]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	- e_minus_ph : numpy.ndarray 3D with float
	   Phase of right hand polarised component of the wave electric field [rad]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	- e_norm : numpy.ndarray 3D with int
	   Magnitude of wave electric field normal to a flux surface [V/m]; Time-dependent; 3D (ntor, ndim1, ndim2)
	- enorm_ph : numpy.ndarray 3D with float
	   Phase of wave electric field normal to a flux surface [rad]; Time-dependent; 3D (ntor, ndim1, ndim2)
	- e_binorm : numpy.ndarray 3D with float
	   Magnitude of wave electric field tangent to a flux surface [V/m]; Time-dependent; 3D (ntor, ndim1, ndim2)
	- e_binorm_ph : numpy.ndarray 3D with float
	   Phase of wave electric field tangent to a flux surface [rad]; Time-dependent; 3D (ntor, ndim1, ndim2)
	- e_para : numpy.ndarray 3D with float
	   Magnitude of parallel wave electric field [V/m]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	- e_para_ph : numpy.ndarray 3D with float
	   Phase of parallel wave electric field [rad]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	- b_norm : numpy.ndarray 3D with float
	   Magnitude of wave magnetic field normal to a flux surface [T]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	- b_norm_ph : numpy.ndarray 3D with float
	   Phase of wave magnetic field normal to a flux surface [rad]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	- b_binorm : numpy.ndarray 3D with float
	   Magnitude of wave magnetic field tangent to a flux surface [T]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	- b_binorm_ph : numpy.ndarray 3D with float
	   Phase of wave magnetic field tangent to a flux surface [rad]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	- b_para : numpy.ndarray 3D with float
	   Magnitude of wave magnetic field parallel to the equilibrium magnetic field [T]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	- b_para_ph : numpy.ndarray 3D with float
	   Phase of wave magnetic field parallel to the equilibrium magnetic field [rad]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	- k_perp : numpy.ndarray 3D with float
	   Perpendicular wave number [T]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
	'''

	def __init__(self, base_path_in='local'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.e_plus = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_plus_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_minus = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_minus_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_norm = numpy.zeros((0,0,0), numpy.int32, order='C')
		self.enorm_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_binorm = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_binorm_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_para = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.e_para_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.b_norm = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.b_norm_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.b_binorm = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.b_binorm_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.b_para = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.b_para_ph = numpy.zeros((0,0,0), numpy.float64, order='C')
		self.k_perp = numpy.zeros((0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class localstructurelocal\n'
		s = self.e_plus.__str__()
		ret = ret + space + 'Attribute e_plus\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_plus_ph.__str__()
		ret = ret + space + 'Attribute e_plus_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_minus.__str__()
		ret = ret + space + 'Attribute e_minus\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_minus_ph.__str__()
		ret = ret + space + 'Attribute e_minus_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_norm.__str__()
		ret = ret + space + 'Attribute e_norm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.enorm_ph.__str__()
		ret = ret + space + 'Attribute enorm_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_binorm.__str__()
		ret = ret + space + 'Attribute e_binorm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_binorm_ph.__str__()
		ret = ret + space + 'Attribute e_binorm_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_para.__str__()
		ret = ret + space + 'Attribute e_para\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.e_para_ph.__str__()
		ret = ret + space + 'Attribute e_para_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_norm.__str__()
		ret = ret + space + 'Attribute b_norm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_norm_ph.__str__()
		ret = ret + space + 'Attribute b_norm_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_binorm.__str__()
		ret = ret + space + 'Attribute b_binorm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_binorm_ph.__str__()
		ret = ret + space + 'Attribute b_binorm_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_para.__str__()
		ret = ret + space + 'Attribute b_para\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_para_ph.__str__()
		ret = ret + space + 'Attribute b_para_ph\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.k_perp.__str__()
		ret = ret + space + 'Attribute k_perp\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type localstructurelocal, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_plus', numpy.array(self.e_plus).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_plus_ph', numpy.array(self.e_plus_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_minus', numpy.array(self.e_minus).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_minus_ph', numpy.array(self.e_minus_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DIntSlice(self.idx, path, cpopath + 'e_norm', numpy.array(self.e_norm).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'enorm_ph', numpy.array(self.enorm_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_binorm', numpy.array(self.e_binorm).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_binorm_ph', numpy.array(self.e_binorm_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_para', numpy.array(self.e_para).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'e_para_ph', numpy.array(self.e_para_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'b_norm', numpy.array(self.b_norm).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'b_norm_ph', numpy.array(self.b_norm_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'b_binorm', numpy.array(self.b_binorm).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'b_binorm_ph', numpy.array(self.b_binorm_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'b_para', numpy.array(self.b_para).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'b_para_ph', numpy.array(self.b_para_ph).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'k_perp', numpy.array(self.k_perp).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type localstructurelocal, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_plus', numpy.array(self.e_plus).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_plus_ph', numpy.array(self.e_plus_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_minus', numpy.array(self.e_minus).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_minus_ph', numpy.array(self.e_minus_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DIntSlice(self.idx, path, cpopath + 'e_norm', numpy.array(self.e_norm).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'enorm_ph', numpy.array(self.enorm_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_binorm', numpy.array(self.e_binorm).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_binorm_ph', numpy.array(self.e_binorm_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_para', numpy.array(self.e_para).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'e_para_ph', numpy.array(self.e_para_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'b_norm', numpy.array(self.b_norm).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'b_norm_ph', numpy.array(self.b_norm_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'b_binorm', numpy.array(self.b_binorm).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'b_binorm_ph', numpy.array(self.b_binorm_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'b_para', numpy.array(self.b_para).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'b_para_ph', numpy.array(self.b_para_ph).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'k_perp', numpy.array(self.k_perp).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type localstructurelocal, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type localstructurelocal, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_e_plus, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_plus', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_plus = ret_e_plus
			self.cpoTime = retTime
		status, ret_e_plus_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_plus_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_plus_ph = ret_e_plus_ph
			self.cpoTime = retTime
		status, ret_e_minus, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_minus', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_minus = ret_e_minus
			self.cpoTime = retTime
		status, ret_e_minus_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_minus_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_minus_ph = ret_e_minus_ph
			self.cpoTime = retTime
		status, ret_e_norm, retTime = ull.getVect3DIntSlice(self.idx, path, cpopath + 'e_norm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_norm = ret_e_norm
			self.cpoTime = retTime
		status, ret_enorm_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'enorm_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.enorm_ph = ret_enorm_ph
			self.cpoTime = retTime
		status, ret_e_binorm, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_binorm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_binorm = ret_e_binorm
			self.cpoTime = retTime
		status, ret_e_binorm_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_binorm_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_binorm_ph = ret_e_binorm_ph
			self.cpoTime = retTime
		status, ret_e_para, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_para', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_para = ret_e_para
			self.cpoTime = retTime
		status, ret_e_para_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'e_para_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.e_para_ph = ret_e_para_ph
			self.cpoTime = retTime
		status, ret_b_norm, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'b_norm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_norm = ret_b_norm
			self.cpoTime = retTime
		status, ret_b_norm_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'b_norm_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_norm_ph = ret_b_norm_ph
			self.cpoTime = retTime
		status, ret_b_binorm, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'b_binorm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_binorm = ret_b_binorm
			self.cpoTime = retTime
		status, ret_b_binorm_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'b_binorm_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_binorm_ph = ret_b_binorm_ph
			self.cpoTime = retTime
		status, ret_b_para, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'b_para', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_para = ret_b_para
			self.cpoTime = retTime
		status, ret_b_para_ph, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'b_para_ph', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_para_ph = ret_b_para_ph
			self.cpoTime = retTime
		status, ret_k_perp, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'k_perp', inTime, interpolMode)
		check_status(status)
		if not status:
			self.k_perp = ret_k_perp
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type localstructurelocal, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, e_plusList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_plus')
			if len(e_plusList) == 0:
				e_plusList = numpy.resize(e_plusList, (0,0,0,nbslice))
			check_status(status)
			status, e_plus_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_plus_ph')
			if len(e_plus_phList) == 0:
				e_plus_phList = numpy.resize(e_plus_phList, (0,0,0,nbslice))
			check_status(status)
			status, e_minusList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_minus')
			if len(e_minusList) == 0:
				e_minusList = numpy.resize(e_minusList, (0,0,0,nbslice))
			check_status(status)
			status, e_minus_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_minus_ph')
			if len(e_minus_phList) == 0:
				e_minus_phList = numpy.resize(e_minus_phList, (0,0,0,nbslice))
			check_status(status)
			status, e_normList = ull.getVect4DInt(self.idx, path, cpopath + 'e_norm')
			if len(e_normList) == 0:
				e_normList = numpy.resize(e_normList, (0,0,0,nbslice))
			check_status(status)
			status, enorm_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'enorm_ph')
			if len(enorm_phList) == 0:
				enorm_phList = numpy.resize(enorm_phList, (0,0,0,nbslice))
			check_status(status)
			status, e_binormList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_binorm')
			if len(e_binormList) == 0:
				e_binormList = numpy.resize(e_binormList, (0,0,0,nbslice))
			check_status(status)
			status, e_binorm_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_binorm_ph')
			if len(e_binorm_phList) == 0:
				e_binorm_phList = numpy.resize(e_binorm_phList, (0,0,0,nbslice))
			check_status(status)
			status, e_paraList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_para')
			if len(e_paraList) == 0:
				e_paraList = numpy.resize(e_paraList, (0,0,0,nbslice))
			check_status(status)
			status, e_para_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'e_para_ph')
			if len(e_para_phList) == 0:
				e_para_phList = numpy.resize(e_para_phList, (0,0,0,nbslice))
			check_status(status)
			status, b_normList = ull.getVect4DDouble(self.idx, path, cpopath + 'b_norm')
			if len(b_normList) == 0:
				b_normList = numpy.resize(b_normList, (0,0,0,nbslice))
			check_status(status)
			status, b_norm_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'b_norm_ph')
			if len(b_norm_phList) == 0:
				b_norm_phList = numpy.resize(b_norm_phList, (0,0,0,nbslice))
			check_status(status)
			status, b_binormList = ull.getVect4DDouble(self.idx, path, cpopath + 'b_binorm')
			if len(b_binormList) == 0:
				b_binormList = numpy.resize(b_binormList, (0,0,0,nbslice))
			check_status(status)
			status, b_binorm_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'b_binorm_ph')
			if len(b_binorm_phList) == 0:
				b_binorm_phList = numpy.resize(b_binorm_phList, (0,0,0,nbslice))
			check_status(status)
			status, b_paraList = ull.getVect4DDouble(self.idx, path, cpopath + 'b_para')
			if len(b_paraList) == 0:
				b_paraList = numpy.resize(b_paraList, (0,0,0,nbslice))
			check_status(status)
			status, b_para_phList = ull.getVect4DDouble(self.idx, path, cpopath + 'b_para_ph')
			if len(b_para_phList) == 0:
				b_para_phList = numpy.resize(b_para_phList, (0,0,0,nbslice))
			check_status(status)
			status, k_perpList = ull.getVect4DDouble(self.idx, path, cpopath + 'k_perp')
			if len(k_perpList) == 0:
				k_perpList = numpy.resize(k_perpList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = localstructurelocal(self.base_path)
				slice.setExpIdx(self.idx)
				slice.e_plus = e_plusList[:,:,:,i]
				slice.e_plus_ph = e_plus_phList[:,:,:,i]
				slice.e_minus = e_minusList[:,:,:,i]
				slice.e_minus_ph = e_minus_phList[:,:,:,i]
				slice.e_norm = e_normList[:,:,:,i]
				slice.enorm_ph = enorm_phList[:,:,:,i]
				slice.e_binorm = e_binormList[:,:,:,i]
				slice.e_binorm_ph = e_binorm_phList[:,:,:,i]
				slice.e_para = e_paraList[:,:,:,i]
				slice.e_para_ph = e_para_phList[:,:,:,i]
				slice.b_norm = b_normList[:,:,:,i]
				slice.b_norm_ph = b_norm_phList[:,:,:,i]
				slice.b_binorm = b_binormList[:,:,:,i]
				slice.b_binorm_ph = b_binorm_phList[:,:,:,i]
				slice.b_para = b_paraList[:,:,:,i]
				slice.b_para_ph = b_para_phList[:,:,:,i]
				slice.k_perp = k_perpList[:,:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type localstructurelocalObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_plus') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_plus', i, numpy.array(self.e_plus).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_plus_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_plus_ph', i, numpy.array(self.e_plus_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_minus') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_minus', i, numpy.array(self.e_minus).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_minus_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_minus_ph', i, numpy.array(self.e_minus_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DIntInObject : ' + cpopath + 'e_norm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DIntInObject(self.idx, obj, cpopath + 'e_norm', i, numpy.array(self.e_norm).astype(numpy.int32))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'enorm_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'enorm_ph', i, numpy.array(self.enorm_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_binorm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_binorm', i, numpy.array(self.e_binorm).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_binorm_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_binorm_ph', i, numpy.array(self.e_binorm_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_para') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_para', i, numpy.array(self.e_para).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'e_para_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'e_para_ph', i, numpy.array(self.e_para_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'b_norm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'b_norm', i, numpy.array(self.b_norm).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'b_norm_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'b_norm_ph', i, numpy.array(self.b_norm_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'b_binorm') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'b_binorm', i, numpy.array(self.b_binorm).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'b_binorm_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'b_binorm_ph', i, numpy.array(self.b_binorm_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'b_para') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'b_para', i, numpy.array(self.b_para).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'b_para_ph') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'b_para_ph', i, numpy.array(self.b_para_ph).astype(numpy.float64))
		if (dev()):
			print ('putVect3DDoubleInObject : ' + cpopath + 'k_perp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect3DDoubleInObject(self.idx, obj, cpopath + 'k_perp', i, numpy.array(self.k_perp).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type localstructurelocalObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_plus') 
			print ('obj = ' + str(obj))
		status, ret_e_plus = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_plus', i)
		check_status(status)
		if not status:
			self.e_plus = ret_e_plus
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_plus_ph') 
			print ('obj = ' + str(obj))
		status, ret_e_plus_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_plus_ph', i)
		check_status(status)
		if not status:
			self.e_plus_ph = ret_e_plus_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_minus') 
			print ('obj = ' + str(obj))
		status, ret_e_minus = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_minus', i)
		check_status(status)
		if not status:
			self.e_minus = ret_e_minus
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_minus_ph') 
			print ('obj = ' + str(obj))
		status, ret_e_minus_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_minus_ph', i)
		check_status(status)
		if not status:
			self.e_minus_ph = ret_e_minus_ph
		if (dev()):
			print ('getVect3DIntInObject : ' + cpopath + 'e_norm') 
			print ('obj = ' + str(obj))
		status, ret_e_norm = ull.getVect3DIntFromObject(self.idx, obj, cpopath + 'e_norm', i)
		check_status(status)
		if not status:
			self.e_norm = ret_e_norm
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'enorm_ph') 
			print ('obj = ' + str(obj))
		status, ret_enorm_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'enorm_ph', i)
		check_status(status)
		if not status:
			self.enorm_ph = ret_enorm_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_binorm') 
			print ('obj = ' + str(obj))
		status, ret_e_binorm = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_binorm', i)
		check_status(status)
		if not status:
			self.e_binorm = ret_e_binorm
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_binorm_ph') 
			print ('obj = ' + str(obj))
		status, ret_e_binorm_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_binorm_ph', i)
		check_status(status)
		if not status:
			self.e_binorm_ph = ret_e_binorm_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_para') 
			print ('obj = ' + str(obj))
		status, ret_e_para = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_para', i)
		check_status(status)
		if not status:
			self.e_para = ret_e_para
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'e_para_ph') 
			print ('obj = ' + str(obj))
		status, ret_e_para_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'e_para_ph', i)
		check_status(status)
		if not status:
			self.e_para_ph = ret_e_para_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'b_norm') 
			print ('obj = ' + str(obj))
		status, ret_b_norm = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'b_norm', i)
		check_status(status)
		if not status:
			self.b_norm = ret_b_norm
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'b_norm_ph') 
			print ('obj = ' + str(obj))
		status, ret_b_norm_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'b_norm_ph', i)
		check_status(status)
		if not status:
			self.b_norm_ph = ret_b_norm_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'b_binorm') 
			print ('obj = ' + str(obj))
		status, ret_b_binorm = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'b_binorm', i)
		check_status(status)
		if not status:
			self.b_binorm = ret_b_binorm
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'b_binorm_ph') 
			print ('obj = ' + str(obj))
		status, ret_b_binorm_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'b_binorm_ph', i)
		check_status(status)
		if not status:
			self.b_binorm_ph = ret_b_binorm_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'b_para') 
			print ('obj = ' + str(obj))
		status, ret_b_para = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'b_para', i)
		check_status(status)
		if not status:
			self.b_para = ret_b_para
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'b_para_ph') 
			print ('obj = ' + str(obj))
		status, ret_b_para_ph = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'b_para_ph', i)
		check_status(status)
		if not status:
			self.b_para_ph = ret_b_para_ph
		if (dev()):
			print ('getVect3DDoubleInObject : ' + cpopath + 'k_perp') 
			print ('obj = ' + str(obj))
		status, ret_k_perp = ull.getVect3DDoubleFromObject(self.idx, obj, cpopath + 'k_perp', i)
		check_status(status)
		if not status:
			self.k_perp = ret_k_perp

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type localstructurelocalObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type localstructurelocalObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'e_plus')
		ull.deleteData(self.idx, path, cpopath + 'e_plus_ph')
		ull.deleteData(self.idx, path, cpopath + 'e_minus')
		ull.deleteData(self.idx, path, cpopath + 'e_minus_ph')
		ull.deleteData(self.idx, path, cpopath + 'e_norm')
		ull.deleteData(self.idx, path, cpopath + 'enorm_ph')
		ull.deleteData(self.idx, path, cpopath + 'e_binorm')
		ull.deleteData(self.idx, path, cpopath + 'e_binorm_ph')
		ull.deleteData(self.idx, path, cpopath + 'e_para')
		ull.deleteData(self.idx, path, cpopath + 'e_para_ph')
		ull.deleteData(self.idx, path, cpopath + 'b_norm')
		ull.deleteData(self.idx, path, cpopath + 'b_norm_ph')
		ull.deleteData(self.idx, path, cpopath + 'b_binorm')
		ull.deleteData(self.idx, path, cpopath + 'b_binorm_ph')
		ull.deleteData(self.idx, path, cpopath + 'b_para')
		ull.deleteData(self.idx, path, cpopath + 'b_para_ph')
		ull.deleteData(self.idx, path, cpopath + 'k_perp')


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
