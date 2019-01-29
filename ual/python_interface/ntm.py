# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class ntm:
	'''
	class ntm
	Description of a Neoclassical Tearing Mode and its evolution.Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- mode : class modestruct_arrayntm_mode: array of modestruct_arrayntm_modeObj objects
	   List of the various NTM modes appearing during the simulation. If a mode appears several times, use several indices in this arra of structure with the same m,n values. All descendant nodes are marked as Time-dependent for technical reasons, to allow the size of the mode AoS to vary.
	- time : float
	   Time [s]; Time-dependent; Scalar.
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self):
		self.base_path = 'ntm'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 1
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.mode = modestruct_arrayntm_mode('mode')
		self.time = EMPTY_DOUBLE
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class ntm\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute mode\n ' + self.mode.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.mode.setExpIdx(idx)
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
		self.mode.cpoTime = self.cpoTime
		self.mode.putSlice(path, cpopath)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		self.codeparam.cpoTime = self.cpoTime
		self.codeparam.putSlice(path, cpopath)
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
		self.mode.replaceLastSlice(path, cpopath)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		self.codeparam.replaceLastSlice(path, cpopath)
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
		self.mode.putNonTimed(path, cpopath)
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
		self.mode.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		self.codeparam.getSlice(path, cpopath, inTime, interpolMode)
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
			modeList = self.mode.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			array = []
			for i in range(nbslice):
				slice = ntm()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.mode = modeList[i]
				slice.time = timeList[i].copy().astype(float)
				slice.codeparam = codeparamList[i]
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
		ull.deleteData(self.idx, path, cpopath + 'mode')
		ull.deleteData(self.idx, path, cpopath + 'time')
		self.codeparam.deleteData(path, cpopath)


class ntmArray:
	'''
	class ntmArray
	Description of a Neoclassical Tearing Mode and its evolution.Time-dependent CPO

	Attributes:
	- array : list of ntm
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
		ret = space + 'class ntmArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'ntm cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = ntm()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(ntm())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = ntm()
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


class modestruct_arrayntm_mode:
	'''
	class modestruct_arrayntm_mode
	List of the various NTM modes appearing during the simulation. If a mode appears several times, use several indices in this arra of structure with the same m,n values. All descendant nodes are marked as Time-dependent for technical reasons, to allow the size of the mode AoS to vary.

	Attributes:
	- array : list of modestruct_arrayntm_modeObj 
	'''

	def __init__(self, base_path_in='mode'):
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
		ret = space + 'class modestruct_arrayntm_mode\n'
		for i in range(len(self.array)):
			ret = ret + space + 'modestruct_arrayntm_mode[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(modestruct_arrayntm_modeObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type modestruct_arrayntm_mode, run function putSlice') 
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
			print ('field '+self.base_path+' of type modestruct_arrayntm_mode, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type modestruct_arrayntm_mode, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type modestruct_arrayntm_mode, run function getSlice') 
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
			print ('field '+self.base_path+' of type modestruct_arrayntm_mode, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(modestruct_arrayntm_mode(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(modestruct_arrayntm_mode(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = modestruct_arrayntm_mode(self.base_path)
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
			print ('field '+self.base_path+' of type modestruct_arrayntm_mode, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type modestruct_arrayntm_mode, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type modestruct_arrayntm_mode, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type modestruct_arrayntm_mode, run function getNonTimedElt') 
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


class modestruct_arrayntm_modeObj:
	'''
	class modestruct_arrayntm_modeObj
	List of the various NTM modes appearing during the simulation. If a mode appears several times, use several indices in this arra of structure with the same m,n values. All descendant nodes are marked as Time-dependent for technical reasons, to allow the size of the mode AoS to vary.

	Attributes:
	- onset : class onsetstructurentm_mode_onset
	   NTM onset characteristics. Time-dependent
	- full_evol : class full_evolstructurentm_mode_full_evol
	   Detailed NTM evolution on a finer timebase than the CPO timebase. Time-dependent.
	- evolution : class evolutionstructurentm_mode_evolution
	   NTM evolution corresponding to the CPO timebase. Time-dependent.
	'''

	def __init__(self, base_path_in='mode'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.onset = onsetstructurentm_mode_onset('onset')
		self.full_evol = full_evolstructurentm_mode_full_evol('full_evol')
		self.evolution = evolutionstructurentm_mode_evolution('evolution')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class modestruct_arrayntm_modeObj\n'
		ret = ret + space + 'Attribute onset\n ' + self.onset.__str__(depth+1)
		ret = ret + space + 'Attribute full_evol\n ' + self.full_evol.__str__(depth+1)
		ret = ret + space + 'Attribute evolution\n ' + self.evolution.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.onset.setExpIdx(idx)
		self.full_evol.setExpIdx(idx)
		self.evolution.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type modestruct_arrayntm_modeObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.onset.putTimedElt(path, cpopath + 'onset', i, obj)
		obj = self.full_evol.putTimedElt(path, cpopath + 'full_evol', i, obj)
		obj = self.evolution.putTimedElt(path, cpopath + 'evolution', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type modestruct_arrayntm_modeObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.onset.getTimedElt(path, cpopath + 'onset', i, obj)
		self.full_evol.getTimedElt(path, cpopath + 'full_evol', i, obj)
		self.evolution.getTimedElt(path, cpopath + 'evolution', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type modestruct_arrayntm_modeObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.onset.putNonTimedElt(path, cpopath + 'onset', i, obj)
		obj = self.full_evol.putNonTimedElt(path, cpopath + 'full_evol', i, obj)
		obj = self.evolution.putNonTimedElt(path, cpopath + 'evolution', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type modestruct_arrayntm_modeObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.onset.getNonTimedElt(path, cpopath + 'onset', i, obj)
		self.full_evol.getNonTimedElt(path, cpopath + 'full_evol', i, obj)
		self.evolution.getNonTimedElt(path, cpopath + 'evolution', i, obj)


class onsetstructurentm_mode_onset:
	'''
	class onsetstructurentm_mode_onset
	NTM onset characteristics. Time-dependent

	Attributes:
	- w : float
	   Seed island full width [m]. Time-dependent.
	- time_onset : float
	   Onset time [s]. Time-dependent.
	- time_offset : float
	   Offset time [s] (when a mode disappears). If the mode reappears later in the simulation, use another index of the mode array of structure. Time-dependent.
	- phase : float
	   Phase of the mode at onset [rad]. Time-dependent.
	- n : int
	   Toroidal mode number. Time-dependent.
	- m : int
	   Poloidal mode number. Time-dependent.
	- description : str
	   Cause of the mode onset. Time-dependent.
	'''

	def __init__(self, base_path_in='onset'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.w = EMPTY_DOUBLE
		self.time_onset = EMPTY_DOUBLE
		self.time_offset = EMPTY_DOUBLE
		self.phase = EMPTY_DOUBLE
		self.n = EMPTY_INT
		self.m = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class onsetstructurentm_mode_onset\n'
		ret = ret + space + 'Attribute w: ' + str(self.w) + '\n'
		ret = ret + space + 'Attribute time_onset: ' + str(self.time_onset) + '\n'
		ret = ret + space + 'Attribute time_offset: ' + str(self.time_offset) + '\n'
		ret = ret + space + 'Attribute phase: ' + str(self.phase) + '\n'
		ret = ret + space + 'Attribute n: ' + str(self.n) + '\n'
		ret = ret + space + 'Attribute m: ' + str(self.m) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type onsetstructurentm_mode_onset, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'w', self.w, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time_onset', self.time_onset, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time_offset', self.time_offset, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'phase', self.phase, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'n', self.n, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'm', self.m, self.cpoTime)
		check_status(status)
		status = ull.putStringSlice(self.idx, path, cpopath + 'description', self.description, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type onsetstructurentm_mode_onset, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'w', self.w)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time_onset', self.time_onset)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time_offset', self.time_offset)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'phase', self.phase)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'n', self.n)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'm', self.m)
		check_status(status)
		status = ull.replaceLastStringSlice(self.idx, path, cpopath + 'description', self.description)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type onsetstructurentm_mode_onset, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type onsetstructurentm_mode_onset, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_w, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'w', inTime, interpolMode)
		check_status(status)
		if not status:
			self.w = ret_w
			self.cpoTime = retTime
		status, ret_time_onset, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time_onset', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time_onset = ret_time_onset
			self.cpoTime = retTime
		status, ret_time_offset, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time_offset', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time_offset = ret_time_offset
			self.cpoTime = retTime
		status, ret_phase, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'phase', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phase = ret_phase
			self.cpoTime = retTime
		status, ret_n, retTime = ull.getIntSlice(self.idx, path, cpopath + 'n', inTime, interpolMode)
		check_status(status)
		if not status:
			self.n = ret_n
			self.cpoTime = retTime
		status, ret_m, retTime = ull.getIntSlice(self.idx, path, cpopath + 'm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.m = ret_m
			self.cpoTime = retTime
		status, ret_description, retTime = ull.getStringSlice(self.idx, path, cpopath + 'description', inTime, interpolMode)
		check_status(status)
		if not status:
			self.description = ret_description
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type onsetstructurentm_mode_onset, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, wList = ull.getVect1DDouble(self.idx, path, cpopath + 'w')
			if len(wList) == 0:
				wList = numpy.resize(wList, (nbslice))
			check_status(status)
			status, time_onsetList = ull.getVect1DDouble(self.idx, path, cpopath + 'time_onset')
			if len(time_onsetList) == 0:
				time_onsetList = numpy.resize(time_onsetList, (nbslice))
			check_status(status)
			status, time_offsetList = ull.getVect1DDouble(self.idx, path, cpopath + 'time_offset')
			if len(time_offsetList) == 0:
				time_offsetList = numpy.resize(time_offsetList, (nbslice))
			check_status(status)
			status, phaseList = ull.getVect1DDouble(self.idx, path, cpopath + 'phase')
			if len(phaseList) == 0:
				phaseList = numpy.resize(phaseList, (nbslice))
			check_status(status)
			status, nList = ull.getVect1DInt(self.idx, path, cpopath + 'n')
			if len(nList) == 0:
				nList = numpy.resize(nList, (nbslice))
			check_status(status)
			status, mList = ull.getVect1DInt(self.idx, path, cpopath + 'm')
			if len(mList) == 0:
				mList = numpy.resize(mList, (nbslice))
			check_status(status)
			status, descriptionList = ull.getVect1DString(self.idx, path, cpopath + 'description')
			if len(descriptionList) == 0:
				for i in range(nbslice):
					descriptionList.append('')
			check_status(status)
			for i in range(nbslice):
				slice = onsetstructurentm_mode_onset(self.base_path)
				slice.setExpIdx(self.idx)
				slice.w = wList[i].copy().astype(float)
				slice.time_onset = time_onsetList[i].copy().astype(float)
				slice.time_offset = time_offsetList[i].copy().astype(float)
				slice.phase = phaseList[i].copy().astype(float)
				slice.n = int(nList[i].copy())
				slice.m = int(mList[i].copy())
				slice.description = descriptionList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type onsetstructurentm_mode_onsetObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'w') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'w', i, self.w)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time_onset') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time_onset', i, self.time_onset)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time_offset') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time_offset', i, self.time_offset)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'phase') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'phase', i, self.phase)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'n') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'n', i, self.n)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'm') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'm', i, self.m)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'description') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'description', i, self.description)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type onsetstructurentm_mode_onsetObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'w') 
			print ('obj = ' + str(obj))
		status, ret_w = ull.getDoubleFromObject(self.idx, obj, cpopath + 'w', i)
		check_status(status)
		if not status:
			self.w = ret_w
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time_onset') 
			print ('obj = ' + str(obj))
		status, ret_time_onset = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time_onset', i)
		check_status(status)
		if not status:
			self.time_onset = ret_time_onset
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time_offset') 
			print ('obj = ' + str(obj))
		status, ret_time_offset = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time_offset', i)
		check_status(status)
		if not status:
			self.time_offset = ret_time_offset
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'phase') 
			print ('obj = ' + str(obj))
		status, ret_phase = ull.getDoubleFromObject(self.idx, obj, cpopath + 'phase', i)
		check_status(status)
		if not status:
			self.phase = ret_phase
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'n') 
			print ('obj = ' + str(obj))
		status, ret_n = ull.getIntFromObject(self.idx, obj, cpopath + 'n', i)
		check_status(status)
		if not status:
			self.n = ret_n
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'm') 
			print ('obj = ' + str(obj))
		status, ret_m = ull.getIntFromObject(self.idx, obj, cpopath + 'm', i)
		check_status(status)
		if not status:
			self.m = ret_m
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'description') 
			print ('obj = ' + str(obj))
		status, ret_description = ull.getStringFromObject(self.idx, obj, cpopath + 'description', i)
		check_status(status)
		if not status:
			self.description = ret_description

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type onsetstructurentm_mode_onsetObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type onsetstructurentm_mode_onsetObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'w')
		ull.deleteData(self.idx, path, cpopath + 'time_onset')
		ull.deleteData(self.idx, path, cpopath + 'time_offset')
		ull.deleteData(self.idx, path, cpopath + 'phase')
		ull.deleteData(self.idx, path, cpopath + 'n')
		ull.deleteData(self.idx, path, cpopath + 'm')
		ull.deleteData(self.idx, path, cpopath + 'description')


class full_evolstructurentm_mode_full_evol:
	'''
	class full_evolstructurentm_mode_full_evol
	Detailed NTM evolution on a finer timebase than the CPO timebase. Time-dependent.

	Attributes:
	- time_evol : numpy.ndarray 1D with float
	   Time array used to describe the detailed mode evolution which can be different from the CPO timebase [s]. Vector(ntime_evol). Time-dependent.
	- w : numpy.ndarray 1D with float
	   Full width of the mode [m]. Vector(ntime_evol). Time-dependent.
	- dwdt : numpy.ndarray 1D with float
	   Time derivative of the full width of the mode [m/s]. Vector(ntime_evol). Time-dependent.
	- phase : numpy.ndarray 1D with float
	   Phase of the mode [rad]. Vector(ntime_evol). Time-dependent.
	- dphasedt : numpy.ndarray 1D with float
	   Time-derivative of the phase of the mode [rad]. Vector(ntime_evol). Time-dependent.
	- frequency : numpy.ndarray 1D with float
	   Frequency of the mode [Hz]. Vector(ntime_evol). Time-dependent.
	- dfrequencydt : numpy.ndarray 1D with float
	   time derivative of the frequency of the mode [Hz]. Vector(ntime_evol). Time-dependent.
	- island : class islandstructurentm_mode_full_evol_island
	   Island description
	- n : int
	   Toroidal mode number. Time-dependent.
	- m : int
	   Poloidal mode number. Time-dependent.
	- deltaw_value : numpy.ndarray 2D with float
	   Matrix(ntype, ntime_evol). Time-dependent.
	- deltaw_name : list of str
	   Name of the deltaw contribution. String vector (ntype). Time-dependent.
	- torque_value : numpy.ndarray 2D with float
	   Matrix(ntype_torque, ntime_evol). Time-dependent.
	- torque_name : list of str
	   Name of the torque contribution. String vector (ntype_torque). Time-dependent.
	- delta_diff : numpy.ndarray 2D with float
	   Extra diffusion coefficient for Te, ne, Ti equation. Matrix(nequation, ntime_evol). Time-dependent.
	- description : str
	   How the mode evolution is calculated. Time-dependent.
	- rho_tor : numpy.ndarray 1D with float
	   [m]. Vector(ntime_evol) Time-dependent.
	'''

	def __init__(self, base_path_in='full_evol'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.time_evol = numpy.zeros(0, numpy.float64, order='C')
		self.w = numpy.zeros(0, numpy.float64, order='C')
		self.dwdt = numpy.zeros(0, numpy.float64, order='C')
		self.phase = numpy.zeros(0, numpy.float64, order='C')
		self.dphasedt = numpy.zeros(0, numpy.float64, order='C')
		self.frequency = numpy.zeros(0, numpy.float64, order='C')
		self.dfrequencydt = numpy.zeros(0, numpy.float64, order='C')
		self.island = islandstructurentm_mode_full_evol_island('island')
		self.n = EMPTY_INT
		self.m = EMPTY_INT
		self.deltaw_value = numpy.zeros((0,0), numpy.float64, order='C')
		self.deltaw_name = ['']
		self.torque_value = numpy.zeros((0,0), numpy.float64, order='C')
		self.torque_name = ['']
		self.delta_diff = numpy.zeros((0,0), numpy.float64, order='C')
		self.description = ''
		self.rho_tor = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class full_evolstructurentm_mode_full_evol\n'
		s = self.time_evol.__str__()
		ret = ret + space + 'Attribute time_evol\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.w.__str__()
		ret = ret + space + 'Attribute w\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dwdt.__str__()
		ret = ret + space + 'Attribute dwdt\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phase.__str__()
		ret = ret + space + 'Attribute phase\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dphasedt.__str__()
		ret = ret + space + 'Attribute dphasedt\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.frequency.__str__()
		ret = ret + space + 'Attribute frequency\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dfrequencydt.__str__()
		ret = ret + space + 'Attribute dfrequencydt\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute island\n ' + self.island.__str__(depth+1)
		ret = ret + space + 'Attribute n: ' + str(self.n) + '\n'
		ret = ret + space + 'Attribute m: ' + str(self.m) + '\n'
		s = self.deltaw_value.__str__()
		ret = ret + space + 'Attribute deltaw_value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.deltaw_name.__str__()
		ret = ret + space + 'Attribute deltaw_name\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_value.__str__()
		ret = ret + space + 'Attribute torque_value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_name.__str__()
		ret = ret + space + 'Attribute torque_name\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.delta_diff.__str__()
		ret = ret + space + 'Attribute delta_diff\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		s = self.rho_tor.__str__()
		ret = ret + space + 'Attribute rho_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.island.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type full_evolstructurentm_mode_full_evol, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'time_evol', numpy.array(self.time_evol).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'w', numpy.array(self.w).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dwdt', numpy.array(self.dwdt).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'phase', numpy.array(self.phase).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dphasedt', numpy.array(self.dphasedt).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'frequency', numpy.array(self.frequency).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dfrequencydt', numpy.array(self.dfrequencydt).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.island.cpoTime = self.cpoTime
		self.island.putSlice(path, cpopath)
		status = ull.putIntSlice(self.idx, path, cpopath + 'n', self.n, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'm', self.m, self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'deltaw_value', numpy.array(self.deltaw_value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DStringSlice(self.idx, path, cpopath + 'deltaw_name', self.deltaw_name, self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'torque_value', numpy.array(self.torque_value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DStringSlice(self.idx, path, cpopath + 'torque_name', self.torque_name, self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'delta_diff', numpy.array(self.delta_diff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putStringSlice(self.idx, path, cpopath + 'description', self.description, self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type full_evolstructurentm_mode_full_evol, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'time_evol', numpy.array(self.time_evol).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'w', numpy.array(self.w).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dwdt', numpy.array(self.dwdt).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'phase', numpy.array(self.phase).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dphasedt', numpy.array(self.dphasedt).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'frequency', numpy.array(self.frequency).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dfrequencydt', numpy.array(self.dfrequencydt).astype(numpy.float64))
		check_status(status)
		self.island.replaceLastSlice(path, cpopath)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'n', self.n)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'm', self.m)
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'deltaw_value', numpy.array(self.deltaw_value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DStringSlice(self.idx, path, cpopath + 'deltaw_name', self.deltaw_name)
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'torque_value', numpy.array(self.torque_value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DStringSlice(self.idx, path, cpopath + 'torque_name', self.torque_name)
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'delta_diff', numpy.array(self.delta_diff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastStringSlice(self.idx, path, cpopath + 'description', self.description)
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type full_evolstructurentm_mode_full_evol, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.island.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type full_evolstructurentm_mode_full_evol, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_time_evol, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'time_evol', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time_evol = ret_time_evol
			self.cpoTime = retTime
		status, ret_w, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'w', inTime, interpolMode)
		check_status(status)
		if not status:
			self.w = ret_w
			self.cpoTime = retTime
		status, ret_dwdt, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dwdt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dwdt = ret_dwdt
			self.cpoTime = retTime
		status, ret_phase, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'phase', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phase = ret_phase
			self.cpoTime = retTime
		status, ret_dphasedt, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dphasedt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dphasedt = ret_dphasedt
			self.cpoTime = retTime
		status, ret_frequency, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'frequency', inTime, interpolMode)
		check_status(status)
		if not status:
			self.frequency = ret_frequency
			self.cpoTime = retTime
		status, ret_dfrequencydt, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dfrequencydt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dfrequencydt = ret_dfrequencydt
			self.cpoTime = retTime
		self.island.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_n, retTime = ull.getIntSlice(self.idx, path, cpopath + 'n', inTime, interpolMode)
		check_status(status)
		if not status:
			self.n = ret_n
			self.cpoTime = retTime
		status, ret_m, retTime = ull.getIntSlice(self.idx, path, cpopath + 'm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.m = ret_m
			self.cpoTime = retTime
		status, ret_deltaw_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'deltaw_value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.deltaw_value = ret_deltaw_value
			self.cpoTime = retTime
		status, ret_deltaw_name, retTime = ull.getVect1DStringSlice(self.idx, path, cpopath + 'deltaw_name', inTime, interpolMode)
		check_status(status)
		if not status:
			self.deltaw_name = ret_deltaw_name
			self.cpoTime = retTime
		status, ret_torque_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'torque_value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_value = ret_torque_value
			self.cpoTime = retTime
		status, ret_torque_name, retTime = ull.getVect1DStringSlice(self.idx, path, cpopath + 'torque_name', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_name = ret_torque_name
			self.cpoTime = retTime
		status, ret_delta_diff, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'delta_diff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.delta_diff = ret_delta_diff
			self.cpoTime = retTime
		status, ret_description, retTime = ull.getStringSlice(self.idx, path, cpopath + 'description', inTime, interpolMode)
		check_status(status)
		if not status:
			self.description = ret_description
			self.cpoTime = retTime
		status, ret_rho_tor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type full_evolstructurentm_mode_full_evol, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, time_evolList = ull.getVect2DDouble(self.idx, path, cpopath + 'time_evol')
			if len(time_evolList) == 0:
				time_evolList = numpy.resize(time_evolList, (0,nbslice))
			check_status(status)
			status, wList = ull.getVect2DDouble(self.idx, path, cpopath + 'w')
			if len(wList) == 0:
				wList = numpy.resize(wList, (0,nbslice))
			check_status(status)
			status, dwdtList = ull.getVect2DDouble(self.idx, path, cpopath + 'dwdt')
			if len(dwdtList) == 0:
				dwdtList = numpy.resize(dwdtList, (0,nbslice))
			check_status(status)
			status, phaseList = ull.getVect2DDouble(self.idx, path, cpopath + 'phase')
			if len(phaseList) == 0:
				phaseList = numpy.resize(phaseList, (0,nbslice))
			check_status(status)
			status, dphasedtList = ull.getVect2DDouble(self.idx, path, cpopath + 'dphasedt')
			if len(dphasedtList) == 0:
				dphasedtList = numpy.resize(dphasedtList, (0,nbslice))
			check_status(status)
			status, frequencyList = ull.getVect2DDouble(self.idx, path, cpopath + 'frequency')
			if len(frequencyList) == 0:
				frequencyList = numpy.resize(frequencyList, (0,nbslice))
			check_status(status)
			status, dfrequencydtList = ull.getVect2DDouble(self.idx, path, cpopath + 'dfrequencydt')
			if len(dfrequencydtList) == 0:
				dfrequencydtList = numpy.resize(dfrequencydtList, (0,nbslice))
			check_status(status)
			islandList = self.island.build_non_resampled_data(path, cpopath, nbslice)
			status, nList = ull.getVect1DInt(self.idx, path, cpopath + 'n')
			if len(nList) == 0:
				nList = numpy.resize(nList, (nbslice))
			check_status(status)
			status, mList = ull.getVect1DInt(self.idx, path, cpopath + 'm')
			if len(mList) == 0:
				mList = numpy.resize(mList, (nbslice))
			check_status(status)
			status, deltaw_valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'deltaw_value')
			if len(deltaw_valueList) == 0:
				deltaw_valueList = numpy.resize(deltaw_valueList, (0,0,nbslice))
			check_status(status)
			status, deltaw_nameList = ull.getVect2DString_error(self.idx, path, cpopath + 'deltaw_name')
			if len(deltaw_nameList) == 0:
				for i in range(nbslice):
					deltaw_nameList.append('')
			check_status(status)
			status, torque_valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'torque_value')
			if len(torque_valueList) == 0:
				torque_valueList = numpy.resize(torque_valueList, (0,0,nbslice))
			check_status(status)
			status, torque_nameList = ull.getVect2DString_error(self.idx, path, cpopath + 'torque_name')
			if len(torque_nameList) == 0:
				for i in range(nbslice):
					torque_nameList.append('')
			check_status(status)
			status, delta_diffList = ull.getVect3DDouble(self.idx, path, cpopath + 'delta_diff')
			if len(delta_diffList) == 0:
				delta_diffList = numpy.resize(delta_diffList, (0,0,nbslice))
			check_status(status)
			status, descriptionList = ull.getVect1DString(self.idx, path, cpopath + 'description')
			if len(descriptionList) == 0:
				for i in range(nbslice):
					descriptionList.append('')
			check_status(status)
			status, rho_torList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_tor')
			if len(rho_torList) == 0:
				rho_torList = numpy.resize(rho_torList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = full_evolstructurentm_mode_full_evol(self.base_path)
				slice.setExpIdx(self.idx)
				slice.time_evol = time_evolList[:,i]
				slice.w = wList[:,i]
				slice.dwdt = dwdtList[:,i]
				slice.phase = phaseList[:,i]
				slice.dphasedt = dphasedtList[:,i]
				slice.frequency = frequencyList[:,i]
				slice.dfrequencydt = dfrequencydtList[:,i]
				slice.island = islandList[i]
				slice.n = int(nList[i].copy())
				slice.m = int(mList[i].copy())
				slice.deltaw_value = deltaw_valueList[:,:,i]
				slice.deltaw_name = deltaw_nameList[:,i]
				slice.torque_value = torque_valueList[:,:,i]
				slice.torque_name = torque_nameList[:,i]
				slice.delta_diff = delta_diffList[:,:,i]
				slice.description = descriptionList[i]
				slice.rho_tor = rho_torList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type full_evolstructurentm_mode_full_evolObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'time_evol') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'time_evol', i, numpy.array(self.time_evol).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'w') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'w', i, numpy.array(self.w).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dwdt') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dwdt', i, numpy.array(self.dwdt).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'phase') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'phase', i, numpy.array(self.phase).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dphasedt') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dphasedt', i, numpy.array(self.dphasedt).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'frequency') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'frequency', i, numpy.array(self.frequency).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dfrequencydt') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dfrequencydt', i, numpy.array(self.dfrequencydt).astype(numpy.float64))
		obj = self.island.putTimedElt(path, cpopath + 'island', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'n') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'n', i, self.n)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'm') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'm', i, self.m)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'deltaw_value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'deltaw_value', i, numpy.array(self.deltaw_value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'deltaw_name') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'deltaw_name', i, self.deltaw_name)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'torque_value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'torque_value', i, numpy.array(self.torque_value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'torque_name') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'torque_name', i, self.torque_name)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'delta_diff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'delta_diff', i, numpy.array(self.delta_diff).astype(numpy.float64))
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'description') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'description', i, self.description)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'rho_tor', i, numpy.array(self.rho_tor).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type full_evolstructurentm_mode_full_evolObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'time_evol') 
			print ('obj = ' + str(obj))
		status, ret_time_evol = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'time_evol', i)
		check_status(status)
		if not status:
			self.time_evol = ret_time_evol
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'w') 
			print ('obj = ' + str(obj))
		status, ret_w = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'w', i)
		check_status(status)
		if not status:
			self.w = ret_w
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dwdt') 
			print ('obj = ' + str(obj))
		status, ret_dwdt = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dwdt', i)
		check_status(status)
		if not status:
			self.dwdt = ret_dwdt
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'phase') 
			print ('obj = ' + str(obj))
		status, ret_phase = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'phase', i)
		check_status(status)
		if not status:
			self.phase = ret_phase
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dphasedt') 
			print ('obj = ' + str(obj))
		status, ret_dphasedt = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dphasedt', i)
		check_status(status)
		if not status:
			self.dphasedt = ret_dphasedt
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'frequency') 
			print ('obj = ' + str(obj))
		status, ret_frequency = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'frequency', i)
		check_status(status)
		if not status:
			self.frequency = ret_frequency
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dfrequencydt') 
			print ('obj = ' + str(obj))
		status, ret_dfrequencydt = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dfrequencydt', i)
		check_status(status)
		if not status:
			self.dfrequencydt = ret_dfrequencydt
		self.island.getTimedElt(path, cpopath + 'island', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'n') 
			print ('obj = ' + str(obj))
		status, ret_n = ull.getIntFromObject(self.idx, obj, cpopath + 'n', i)
		check_status(status)
		if not status:
			self.n = ret_n
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'm') 
			print ('obj = ' + str(obj))
		status, ret_m = ull.getIntFromObject(self.idx, obj, cpopath + 'm', i)
		check_status(status)
		if not status:
			self.m = ret_m
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'deltaw_value') 
			print ('obj = ' + str(obj))
		status, ret_deltaw_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'deltaw_value', i)
		check_status(status)
		if not status:
			self.deltaw_value = ret_deltaw_value
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'deltaw_name') 
			print ('obj = ' + str(obj))
		status, ret_deltaw_name = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'deltaw_name', i)
		check_status(status)
		if not status:
			self.deltaw_name = ret_deltaw_name
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'torque_value') 
			print ('obj = ' + str(obj))
		status, ret_torque_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'torque_value', i)
		check_status(status)
		if not status:
			self.torque_value = ret_torque_value
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'torque_name') 
			print ('obj = ' + str(obj))
		status, ret_torque_name = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'torque_name', i)
		check_status(status)
		if not status:
			self.torque_name = ret_torque_name
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'delta_diff') 
			print ('obj = ' + str(obj))
		status, ret_delta_diff = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'delta_diff', i)
		check_status(status)
		if not status:
			self.delta_diff = ret_delta_diff
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'description') 
			print ('obj = ' + str(obj))
		status, ret_description = ull.getStringFromObject(self.idx, obj, cpopath + 'description', i)
		check_status(status)
		if not status:
			self.description = ret_description
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'rho_tor', i)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type full_evolstructurentm_mode_full_evolObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.island.putNonTimedElt(path, cpopath + 'island', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type full_evolstructurentm_mode_full_evolObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.island.getNonTimedElt(path, cpopath + 'island', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'time_evol')
		ull.deleteData(self.idx, path, cpopath + 'w')
		ull.deleteData(self.idx, path, cpopath + 'dwdt')
		ull.deleteData(self.idx, path, cpopath + 'phase')
		ull.deleteData(self.idx, path, cpopath + 'dphasedt')
		ull.deleteData(self.idx, path, cpopath + 'frequency')
		ull.deleteData(self.idx, path, cpopath + 'dfrequencydt')
		self.island.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'n')
		ull.deleteData(self.idx, path, cpopath + 'm')
		ull.deleteData(self.idx, path, cpopath + 'deltaw_value')
		ull.deleteData(self.idx, path, cpopath + 'deltaw_name')
		ull.deleteData(self.idx, path, cpopath + 'torque_value')
		ull.deleteData(self.idx, path, cpopath + 'torque_name')
		ull.deleteData(self.idx, path, cpopath + 'delta_diff')
		ull.deleteData(self.idx, path, cpopath + 'description')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor')


class islandstructurentm_mode_full_evol_island:
	'''
	class islandstructurentm_mode_full_evol_island
	Island description

	Attributes:
	- geometry : numpy.ndarray 2D with float
	   Description of island geometry [?]. Matrix(nradial, ntime_evol). Time-dependent.
	- coord_values : numpy.ndarray 2D with float
	   Radial coordinate values [?]. Matrix(nradial, ntime_evol). Time-dependent.
	- coord_desc : str
	   Description of flux label, use the same for all islands. Time-dependent.
	'''

	def __init__(self, base_path_in='island'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.geometry = numpy.zeros((0,0), numpy.float64, order='C')
		self.coord_values = numpy.zeros((0,0), numpy.float64, order='C')
		self.coord_desc = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class islandstructurentm_mode_full_evol_island\n'
		s = self.geometry.__str__()
		ret = ret + space + 'Attribute geometry\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.coord_values.__str__()
		ret = ret + space + 'Attribute coord_values\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute coord_desc: ' + str(self.coord_desc) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type islandstructurentm_mode_full_evol_island, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'geometry', numpy.array(self.geometry).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'coord_values', numpy.array(self.coord_values).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putStringSlice(self.idx, path, cpopath + 'coord_desc', self.coord_desc, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type islandstructurentm_mode_full_evol_island, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'geometry', numpy.array(self.geometry).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'coord_values', numpy.array(self.coord_values).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastStringSlice(self.idx, path, cpopath + 'coord_desc', self.coord_desc)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type islandstructurentm_mode_full_evol_island, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type islandstructurentm_mode_full_evol_island, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_geometry, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'geometry', inTime, interpolMode)
		check_status(status)
		if not status:
			self.geometry = ret_geometry
			self.cpoTime = retTime
		status, ret_coord_values, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'coord_values', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord_values = ret_coord_values
			self.cpoTime = retTime
		status, ret_coord_desc, retTime = ull.getStringSlice(self.idx, path, cpopath + 'coord_desc', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord_desc = ret_coord_desc
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type islandstructurentm_mode_full_evol_island, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, geometryList = ull.getVect3DDouble(self.idx, path, cpopath + 'geometry')
			if len(geometryList) == 0:
				geometryList = numpy.resize(geometryList, (0,0,nbslice))
			check_status(status)
			status, coord_valuesList = ull.getVect3DDouble(self.idx, path, cpopath + 'coord_values')
			if len(coord_valuesList) == 0:
				coord_valuesList = numpy.resize(coord_valuesList, (0,0,nbslice))
			check_status(status)
			status, coord_descList = ull.getVect1DString(self.idx, path, cpopath + 'coord_desc')
			if len(coord_descList) == 0:
				for i in range(nbslice):
					coord_descList.append('')
			check_status(status)
			for i in range(nbslice):
				slice = islandstructurentm_mode_full_evol_island(self.base_path)
				slice.setExpIdx(self.idx)
				slice.geometry = geometryList[:,:,i]
				slice.coord_values = coord_valuesList[:,:,i]
				slice.coord_desc = coord_descList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type islandstructurentm_mode_full_evol_islandObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'geometry') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'geometry', i, numpy.array(self.geometry).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'coord_values') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'coord_values', i, numpy.array(self.coord_values).astype(numpy.float64))
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'coord_desc') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'coord_desc', i, self.coord_desc)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type islandstructurentm_mode_full_evol_islandObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'geometry') 
			print ('obj = ' + str(obj))
		status, ret_geometry = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'geometry', i)
		check_status(status)
		if not status:
			self.geometry = ret_geometry
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'coord_values') 
			print ('obj = ' + str(obj))
		status, ret_coord_values = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'coord_values', i)
		check_status(status)
		if not status:
			self.coord_values = ret_coord_values
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'coord_desc') 
			print ('obj = ' + str(obj))
		status, ret_coord_desc = ull.getStringFromObject(self.idx, obj, cpopath + 'coord_desc', i)
		check_status(status)
		if not status:
			self.coord_desc = ret_coord_desc

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type islandstructurentm_mode_full_evol_islandObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type islandstructurentm_mode_full_evol_islandObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'geometry')
		ull.deleteData(self.idx, path, cpopath + 'coord_values')
		ull.deleteData(self.idx, path, cpopath + 'coord_desc')


class evolutionstructurentm_mode_evolution:
	'''
	class evolutionstructurentm_mode_evolution
	NTM evolution corresponding to the CPO timebase. Time-dependent.

	Attributes:
	- w : float
	   Full width of the mode [m]. Time-dependent.
	- dwdt : float
	   Time derivative of the full width of the mode [m/s].  Time-dependent.
	- phase : float
	   Phase of the mode [rad].  Time-dependent.
	- dphasedt : float
	   Time-derivative of the phase of the mode [rad]. Time-dependent.
	- frequency : float
	   Frequency of the mode [Hz]. Time-dependent.
	- dfrequencydt : float
	   Time derivative of the frequency of the mode [Hz].  Time-dependent.
	- island : class islandstructurentm_mode_evolution_island
	   Island description
	- n : int
	   Toroidal mode number. Time-dependent.
	- m : int
	   Poloidal mode number. Time-dependent.
	- deltaw_value : numpy.ndarray 1D with float
	   Vector(ntype). Time-dependent.
	- deltaw_name : list of str
	   Name of the deltaw contribution. String vector (ntype). Time-dependent.
	- torque_value : numpy.ndarray 1D with float
	   Vector(ntype_torque). Time-dependent.
	- torque_name : list of str
	   Name of the torque contribution. String vector (ntype). Time-dependent.
	- delta_diff : numpy.ndarray 1D with float
	   Extra diffusion coefficient for Te, ne, Ti equation. Vector(nequation). Time-dependent.
	- description : str
	   How the mode evolution is calculated. Time-dependent.
	- rho_tor : float
	   [m]. Time-dependent.
	'''

	def __init__(self, base_path_in='evolution'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.w = EMPTY_DOUBLE
		self.dwdt = EMPTY_DOUBLE
		self.phase = EMPTY_DOUBLE
		self.dphasedt = EMPTY_DOUBLE
		self.frequency = EMPTY_DOUBLE
		self.dfrequencydt = EMPTY_DOUBLE
		self.island = islandstructurentm_mode_evolution_island('island')
		self.n = EMPTY_INT
		self.m = EMPTY_INT
		self.deltaw_value = numpy.zeros(0, numpy.float64, order='C')
		self.deltaw_name = ['']
		self.torque_value = numpy.zeros(0, numpy.float64, order='C')
		self.torque_name = ['']
		self.delta_diff = numpy.zeros(0, numpy.float64, order='C')
		self.description = ''
		self.rho_tor = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class evolutionstructurentm_mode_evolution\n'
		ret = ret + space + 'Attribute w: ' + str(self.w) + '\n'
		ret = ret + space + 'Attribute dwdt: ' + str(self.dwdt) + '\n'
		ret = ret + space + 'Attribute phase: ' + str(self.phase) + '\n'
		ret = ret + space + 'Attribute dphasedt: ' + str(self.dphasedt) + '\n'
		ret = ret + space + 'Attribute frequency: ' + str(self.frequency) + '\n'
		ret = ret + space + 'Attribute dfrequencydt: ' + str(self.dfrequencydt) + '\n'
		ret = ret + space + 'Attribute island\n ' + self.island.__str__(depth+1)
		ret = ret + space + 'Attribute n: ' + str(self.n) + '\n'
		ret = ret + space + 'Attribute m: ' + str(self.m) + '\n'
		s = self.deltaw_value.__str__()
		ret = ret + space + 'Attribute deltaw_value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.deltaw_name.__str__()
		ret = ret + space + 'Attribute deltaw_name\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_value.__str__()
		ret = ret + space + 'Attribute torque_value\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_name.__str__()
		ret = ret + space + 'Attribute torque_name\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.delta_diff.__str__()
		ret = ret + space + 'Attribute delta_diff\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		ret = ret + space + 'Attribute rho_tor: ' + str(self.rho_tor) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.island.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type evolutionstructurentm_mode_evolution, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'w', self.w, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'dwdt', self.dwdt, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'phase', self.phase, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'dphasedt', self.dphasedt, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'frequency', self.frequency, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'dfrequencydt', self.dfrequencydt, self.cpoTime)
		check_status(status)
		self.island.cpoTime = self.cpoTime
		self.island.putSlice(path, cpopath)
		status = ull.putIntSlice(self.idx, path, cpopath + 'n', self.n, self.cpoTime)
		check_status(status)
		status = ull.putIntSlice(self.idx, path, cpopath + 'm', self.m, self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'deltaw_value', numpy.array(self.deltaw_value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DStringSlice(self.idx, path, cpopath + 'deltaw_name', self.deltaw_name, self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'torque_value', numpy.array(self.torque_value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DStringSlice(self.idx, path, cpopath + 'torque_name', self.torque_name, self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'delta_diff', numpy.array(self.delta_diff).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putStringSlice(self.idx, path, cpopath + 'description', self.description, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'rho_tor', self.rho_tor, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type evolutionstructurentm_mode_evolution, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'w', self.w)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'dwdt', self.dwdt)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'phase', self.phase)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'dphasedt', self.dphasedt)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'frequency', self.frequency)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'dfrequencydt', self.dfrequencydt)
		check_status(status)
		self.island.replaceLastSlice(path, cpopath)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'n', self.n)
		check_status(status)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'm', self.m)
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'deltaw_value', numpy.array(self.deltaw_value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DStringSlice(self.idx, path, cpopath + 'deltaw_name', self.deltaw_name)
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'torque_value', numpy.array(self.torque_value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DStringSlice(self.idx, path, cpopath + 'torque_name', self.torque_name)
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'delta_diff', numpy.array(self.delta_diff).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastStringSlice(self.idx, path, cpopath + 'description', self.description)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'rho_tor', self.rho_tor)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type evolutionstructurentm_mode_evolution, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.island.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type evolutionstructurentm_mode_evolution, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_w, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'w', inTime, interpolMode)
		check_status(status)
		if not status:
			self.w = ret_w
			self.cpoTime = retTime
		status, ret_dwdt, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'dwdt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dwdt = ret_dwdt
			self.cpoTime = retTime
		status, ret_phase, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'phase', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phase = ret_phase
			self.cpoTime = retTime
		status, ret_dphasedt, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'dphasedt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dphasedt = ret_dphasedt
			self.cpoTime = retTime
		status, ret_frequency, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'frequency', inTime, interpolMode)
		check_status(status)
		if not status:
			self.frequency = ret_frequency
			self.cpoTime = retTime
		status, ret_dfrequencydt, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'dfrequencydt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dfrequencydt = ret_dfrequencydt
			self.cpoTime = retTime
		self.island.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_n, retTime = ull.getIntSlice(self.idx, path, cpopath + 'n', inTime, interpolMode)
		check_status(status)
		if not status:
			self.n = ret_n
			self.cpoTime = retTime
		status, ret_m, retTime = ull.getIntSlice(self.idx, path, cpopath + 'm', inTime, interpolMode)
		check_status(status)
		if not status:
			self.m = ret_m
			self.cpoTime = retTime
		status, ret_deltaw_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'deltaw_value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.deltaw_value = ret_deltaw_value
			self.cpoTime = retTime
		status, ret_deltaw_name, retTime = ull.getVect1DStringSlice(self.idx, path, cpopath + 'deltaw_name', inTime, interpolMode)
		check_status(status)
		if not status:
			self.deltaw_name = ret_deltaw_name
			self.cpoTime = retTime
		status, ret_torque_value, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'torque_value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_value = ret_torque_value
			self.cpoTime = retTime
		status, ret_torque_name, retTime = ull.getVect1DStringSlice(self.idx, path, cpopath + 'torque_name', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_name = ret_torque_name
			self.cpoTime = retTime
		status, ret_delta_diff, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'delta_diff', inTime, interpolMode)
		check_status(status)
		if not status:
			self.delta_diff = ret_delta_diff
			self.cpoTime = retTime
		status, ret_description, retTime = ull.getStringSlice(self.idx, path, cpopath + 'description', inTime, interpolMode)
		check_status(status)
		if not status:
			self.description = ret_description
			self.cpoTime = retTime
		status, ret_rho_tor, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'rho_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type evolutionstructurentm_mode_evolution, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, wList = ull.getVect1DDouble(self.idx, path, cpopath + 'w')
			if len(wList) == 0:
				wList = numpy.resize(wList, (nbslice))
			check_status(status)
			status, dwdtList = ull.getVect1DDouble(self.idx, path, cpopath + 'dwdt')
			if len(dwdtList) == 0:
				dwdtList = numpy.resize(dwdtList, (nbslice))
			check_status(status)
			status, phaseList = ull.getVect1DDouble(self.idx, path, cpopath + 'phase')
			if len(phaseList) == 0:
				phaseList = numpy.resize(phaseList, (nbslice))
			check_status(status)
			status, dphasedtList = ull.getVect1DDouble(self.idx, path, cpopath + 'dphasedt')
			if len(dphasedtList) == 0:
				dphasedtList = numpy.resize(dphasedtList, (nbslice))
			check_status(status)
			status, frequencyList = ull.getVect1DDouble(self.idx, path, cpopath + 'frequency')
			if len(frequencyList) == 0:
				frequencyList = numpy.resize(frequencyList, (nbslice))
			check_status(status)
			status, dfrequencydtList = ull.getVect1DDouble(self.idx, path, cpopath + 'dfrequencydt')
			if len(dfrequencydtList) == 0:
				dfrequencydtList = numpy.resize(dfrequencydtList, (nbslice))
			check_status(status)
			islandList = self.island.build_non_resampled_data(path, cpopath, nbslice)
			status, nList = ull.getVect1DInt(self.idx, path, cpopath + 'n')
			if len(nList) == 0:
				nList = numpy.resize(nList, (nbslice))
			check_status(status)
			status, mList = ull.getVect1DInt(self.idx, path, cpopath + 'm')
			if len(mList) == 0:
				mList = numpy.resize(mList, (nbslice))
			check_status(status)
			status, deltaw_valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'deltaw_value')
			if len(deltaw_valueList) == 0:
				deltaw_valueList = numpy.resize(deltaw_valueList, (0,nbslice))
			check_status(status)
			status, deltaw_nameList = ull.getVect2DString_error(self.idx, path, cpopath + 'deltaw_name')
			if len(deltaw_nameList) == 0:
				for i in range(nbslice):
					deltaw_nameList.append('')
			check_status(status)
			status, torque_valueList = ull.getVect2DDouble(self.idx, path, cpopath + 'torque_value')
			if len(torque_valueList) == 0:
				torque_valueList = numpy.resize(torque_valueList, (0,nbslice))
			check_status(status)
			status, torque_nameList = ull.getVect2DString_error(self.idx, path, cpopath + 'torque_name')
			if len(torque_nameList) == 0:
				for i in range(nbslice):
					torque_nameList.append('')
			check_status(status)
			status, delta_diffList = ull.getVect2DDouble(self.idx, path, cpopath + 'delta_diff')
			if len(delta_diffList) == 0:
				delta_diffList = numpy.resize(delta_diffList, (0,nbslice))
			check_status(status)
			status, descriptionList = ull.getVect1DString(self.idx, path, cpopath + 'description')
			if len(descriptionList) == 0:
				for i in range(nbslice):
					descriptionList.append('')
			check_status(status)
			status, rho_torList = ull.getVect1DDouble(self.idx, path, cpopath + 'rho_tor')
			if len(rho_torList) == 0:
				rho_torList = numpy.resize(rho_torList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = evolutionstructurentm_mode_evolution(self.base_path)
				slice.setExpIdx(self.idx)
				slice.w = wList[i].copy().astype(float)
				slice.dwdt = dwdtList[i].copy().astype(float)
				slice.phase = phaseList[i].copy().astype(float)
				slice.dphasedt = dphasedtList[i].copy().astype(float)
				slice.frequency = frequencyList[i].copy().astype(float)
				slice.dfrequencydt = dfrequencydtList[i].copy().astype(float)
				slice.island = islandList[i]
				slice.n = int(nList[i].copy())
				slice.m = int(mList[i].copy())
				slice.deltaw_value = deltaw_valueList[:,i]
				slice.deltaw_name = deltaw_nameList[:,i]
				slice.torque_value = torque_valueList[:,i]
				slice.torque_name = torque_nameList[:,i]
				slice.delta_diff = delta_diffList[:,i]
				slice.description = descriptionList[i]
				slice.rho_tor = rho_torList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type evolutionstructurentm_mode_evolutionObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'w') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'w', i, self.w)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dwdt') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dwdt', i, self.dwdt)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'phase') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'phase', i, self.phase)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dphasedt') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dphasedt', i, self.dphasedt)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'frequency') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'frequency', i, self.frequency)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'dfrequencydt') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'dfrequencydt', i, self.dfrequencydt)
		obj = self.island.putTimedElt(path, cpopath + 'island', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'n') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'n', i, self.n)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'm') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'm', i, self.m)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'deltaw_value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'deltaw_value', i, numpy.array(self.deltaw_value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'deltaw_name') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'deltaw_name', i, self.deltaw_name)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'torque_value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'torque_value', i, numpy.array(self.torque_value).astype(numpy.float64))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'torque_name') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'torque_name', i, self.torque_name)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'delta_diff') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'delta_diff', i, numpy.array(self.delta_diff).astype(numpy.float64))
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'description') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'description', i, self.description)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'rho_tor', i, self.rho_tor)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type evolutionstructurentm_mode_evolutionObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'w') 
			print ('obj = ' + str(obj))
		status, ret_w = ull.getDoubleFromObject(self.idx, obj, cpopath + 'w', i)
		check_status(status)
		if not status:
			self.w = ret_w
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dwdt') 
			print ('obj = ' + str(obj))
		status, ret_dwdt = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dwdt', i)
		check_status(status)
		if not status:
			self.dwdt = ret_dwdt
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'phase') 
			print ('obj = ' + str(obj))
		status, ret_phase = ull.getDoubleFromObject(self.idx, obj, cpopath + 'phase', i)
		check_status(status)
		if not status:
			self.phase = ret_phase
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dphasedt') 
			print ('obj = ' + str(obj))
		status, ret_dphasedt = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dphasedt', i)
		check_status(status)
		if not status:
			self.dphasedt = ret_dphasedt
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'frequency') 
			print ('obj = ' + str(obj))
		status, ret_frequency = ull.getDoubleFromObject(self.idx, obj, cpopath + 'frequency', i)
		check_status(status)
		if not status:
			self.frequency = ret_frequency
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'dfrequencydt') 
			print ('obj = ' + str(obj))
		status, ret_dfrequencydt = ull.getDoubleFromObject(self.idx, obj, cpopath + 'dfrequencydt', i)
		check_status(status)
		if not status:
			self.dfrequencydt = ret_dfrequencydt
		self.island.getTimedElt(path, cpopath + 'island', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'n') 
			print ('obj = ' + str(obj))
		status, ret_n = ull.getIntFromObject(self.idx, obj, cpopath + 'n', i)
		check_status(status)
		if not status:
			self.n = ret_n
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'm') 
			print ('obj = ' + str(obj))
		status, ret_m = ull.getIntFromObject(self.idx, obj, cpopath + 'm', i)
		check_status(status)
		if not status:
			self.m = ret_m
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'deltaw_value') 
			print ('obj = ' + str(obj))
		status, ret_deltaw_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'deltaw_value', i)
		check_status(status)
		if not status:
			self.deltaw_value = ret_deltaw_value
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'deltaw_name') 
			print ('obj = ' + str(obj))
		status, ret_deltaw_name = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'deltaw_name', i)
		check_status(status)
		if not status:
			self.deltaw_name = ret_deltaw_name
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'torque_value') 
			print ('obj = ' + str(obj))
		status, ret_torque_value = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'torque_value', i)
		check_status(status)
		if not status:
			self.torque_value = ret_torque_value
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'torque_name') 
			print ('obj = ' + str(obj))
		status, ret_torque_name = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'torque_name', i)
		check_status(status)
		if not status:
			self.torque_name = ret_torque_name
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'delta_diff') 
			print ('obj = ' + str(obj))
		status, ret_delta_diff = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'delta_diff', i)
		check_status(status)
		if not status:
			self.delta_diff = ret_delta_diff
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'description') 
			print ('obj = ' + str(obj))
		status, ret_description = ull.getStringFromObject(self.idx, obj, cpopath + 'description', i)
		check_status(status)
		if not status:
			self.description = ret_description
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor = ull.getDoubleFromObject(self.idx, obj, cpopath + 'rho_tor', i)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type evolutionstructurentm_mode_evolutionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.island.putNonTimedElt(path, cpopath + 'island', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type evolutionstructurentm_mode_evolutionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.island.getNonTimedElt(path, cpopath + 'island', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'w')
		ull.deleteData(self.idx, path, cpopath + 'dwdt')
		ull.deleteData(self.idx, path, cpopath + 'phase')
		ull.deleteData(self.idx, path, cpopath + 'dphasedt')
		ull.deleteData(self.idx, path, cpopath + 'frequency')
		ull.deleteData(self.idx, path, cpopath + 'dfrequencydt')
		self.island.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'n')
		ull.deleteData(self.idx, path, cpopath + 'm')
		ull.deleteData(self.idx, path, cpopath + 'deltaw_value')
		ull.deleteData(self.idx, path, cpopath + 'deltaw_name')
		ull.deleteData(self.idx, path, cpopath + 'torque_value')
		ull.deleteData(self.idx, path, cpopath + 'torque_name')
		ull.deleteData(self.idx, path, cpopath + 'delta_diff')
		ull.deleteData(self.idx, path, cpopath + 'description')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor')


class islandstructurentm_mode_evolution_island:
	'''
	class islandstructurentm_mode_evolution_island
	Island description

	Attributes:
	- geometry : numpy.ndarray 1D with float
	   Description of island geometry [?]. Vector(nradial). Time-dependent.
	- coord_values : numpy.ndarray 1D with float
	   Radial coordinate values [?]. Vector(nradial). Time-dependent.
	- coord_desc : str
	   Description of flux label, use the same for all islands. Time-dependent.
	'''

	def __init__(self, base_path_in='island'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.geometry = numpy.zeros(0, numpy.float64, order='C')
		self.coord_values = numpy.zeros(0, numpy.float64, order='C')
		self.coord_desc = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class islandstructurentm_mode_evolution_island\n'
		s = self.geometry.__str__()
		ret = ret + space + 'Attribute geometry\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.coord_values.__str__()
		ret = ret + space + 'Attribute coord_values\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute coord_desc: ' + str(self.coord_desc) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type islandstructurentm_mode_evolution_island, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'geometry', numpy.array(self.geometry).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'coord_values', numpy.array(self.coord_values).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putStringSlice(self.idx, path, cpopath + 'coord_desc', self.coord_desc, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type islandstructurentm_mode_evolution_island, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'geometry', numpy.array(self.geometry).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'coord_values', numpy.array(self.coord_values).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastStringSlice(self.idx, path, cpopath + 'coord_desc', self.coord_desc)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type islandstructurentm_mode_evolution_island, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type islandstructurentm_mode_evolution_island, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_geometry, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'geometry', inTime, interpolMode)
		check_status(status)
		if not status:
			self.geometry = ret_geometry
			self.cpoTime = retTime
		status, ret_coord_values, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'coord_values', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord_values = ret_coord_values
			self.cpoTime = retTime
		status, ret_coord_desc, retTime = ull.getStringSlice(self.idx, path, cpopath + 'coord_desc', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord_desc = ret_coord_desc
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type islandstructurentm_mode_evolution_island, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, geometryList = ull.getVect2DDouble(self.idx, path, cpopath + 'geometry')
			if len(geometryList) == 0:
				geometryList = numpy.resize(geometryList, (0,nbslice))
			check_status(status)
			status, coord_valuesList = ull.getVect2DDouble(self.idx, path, cpopath + 'coord_values')
			if len(coord_valuesList) == 0:
				coord_valuesList = numpy.resize(coord_valuesList, (0,nbslice))
			check_status(status)
			status, coord_descList = ull.getVect1DString(self.idx, path, cpopath + 'coord_desc')
			if len(coord_descList) == 0:
				for i in range(nbslice):
					coord_descList.append('')
			check_status(status)
			for i in range(nbslice):
				slice = islandstructurentm_mode_evolution_island(self.base_path)
				slice.setExpIdx(self.idx)
				slice.geometry = geometryList[:,i]
				slice.coord_values = coord_valuesList[:,i]
				slice.coord_desc = coord_descList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type islandstructurentm_mode_evolution_islandObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'geometry') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'geometry', i, numpy.array(self.geometry).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'coord_values') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'coord_values', i, numpy.array(self.coord_values).astype(numpy.float64))
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'coord_desc') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'coord_desc', i, self.coord_desc)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type islandstructurentm_mode_evolution_islandObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'geometry') 
			print ('obj = ' + str(obj))
		status, ret_geometry = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'geometry', i)
		check_status(status)
		if not status:
			self.geometry = ret_geometry
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'coord_values') 
			print ('obj = ' + str(obj))
		status, ret_coord_values = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'coord_values', i)
		check_status(status)
		if not status:
			self.coord_values = ret_coord_values
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'coord_desc') 
			print ('obj = ' + str(obj))
		status, ret_coord_desc = ull.getStringFromObject(self.idx, obj, cpopath + 'coord_desc', i)
		check_status(status)
		if not status:
			self.coord_desc = ret_coord_desc

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type islandstructurentm_mode_evolution_islandObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type islandstructurentm_mode_evolution_islandObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'geometry')
		ull.deleteData(self.idx, path, cpopath + 'coord_values')
		ull.deleteData(self.idx, path, cpopath + 'coord_desc')


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
