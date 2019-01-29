# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class orbit:
	'''
	class orbit
	Orbits for a set of particles. Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- com : class comstructurecom
	   COM (Constants Of Motion) parameters identifying an orbit
	- trace : class tracestructuretrace
	   Position of particle in 5D space (3D in real and 2D in velocity).
	- global_param : class global_paramstructureorbit_global_param
	   Global quantities associated with an orbit.
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar
	'''

	def __init__(self):
		self.base_path = 'orbit'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 5
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.com = comstructurecom('com')
		self.trace = tracestructuretrace('trace')
		self.global_param = global_paramstructureorbit_global_param('global_param')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class orbit\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute com\n ' + self.com.__str__(depth+1)
		ret = ret + space + 'Attribute trace\n ' + self.trace.__str__(depth+1)
		ret = ret + space + 'Attribute global_param\n ' + self.global_param.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.com.setExpIdx(idx)
		self.trace.setExpIdx(idx)
		self.global_param.setExpIdx(idx)
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
		self.com.cpoTime = self.cpoTime
		self.com.putSlice(path, cpopath)
		self.trace.cpoTime = self.cpoTime
		self.trace.putSlice(path, cpopath)
		self.global_param.cpoTime = self.cpoTime
		self.global_param.putSlice(path, cpopath)
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
		self.com.replaceLastSlice(path, cpopath)
		self.trace.replaceLastSlice(path, cpopath)
		self.global_param.replaceLastSlice(path, cpopath)
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
		self.com.putNonTimed(path, cpopath)
		self.trace.putNonTimed(path, cpopath)
		self.global_param.putNonTimed(path, cpopath)
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
		self.com.getSlice(path, cpopath, inTime, interpolMode)
		self.trace.getSlice(path, cpopath, inTime, interpolMode)
		self.global_param.getSlice(path, cpopath, inTime, interpolMode)
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
			comList = self.com.build_non_resampled_data(path, cpopath, nbslice)
			traceList = self.trace.build_non_resampled_data(path, cpopath, nbslice)
			global_paramList = self.global_param.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = orbit()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.com = comList[i]
				slice.trace = traceList[i]
				slice.global_param = global_paramList[i]
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
		self.com.deleteData(path, cpopath)
		self.trace.deleteData(path, cpopath)
		self.global_param.deleteData(path, cpopath)
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class orbitArray:
	'''
	class orbitArray
	Orbits for a set of particles. Time-dependent CPO

	Attributes:
	- array : list of orbit
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
		ret = space + 'class orbitArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'orbit cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = orbit()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(orbit())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = orbit()
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


class comstructurecom:
	'''
	class comstructurecom
	COM (Constants Of Motion) parameters identifying an orbit

	Attributes:
	- amn : float
	   Atomic mass of the particle; Scalar
	- zion : float
	   Atomic charge of the particle; Scalar
	- energy : numpy.ndarray 1D with float
	   Energy of the particle [keV]; Time-dependent; Vector (norbits).
	- magn_mom : numpy.ndarray 1D with float
	   Magnetic momentum [kg m^2 / s^2 / T]; Time-dependent, Vector(norbits).
	- p_phi : numpy.ndarray 1D with float
	   toroidal angular momentum [kg m^2 / s]; Time-dependent; Vector(norbits);
	- sigma : numpy.ndarray 1D with int)
	   Sign of parallel velocity at psi=psi_max along the orbit; Time-dependent; Vector(norbits)
	'''

	def __init__(self, base_path_in='com'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.amn = EMPTY_DOUBLE
		self.zion = EMPTY_DOUBLE
		self.energy = numpy.zeros(0, numpy.float64, order='C')
		self.magn_mom = numpy.zeros(0, numpy.float64, order='C')
		self.p_phi = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class comstructurecom\n'
		ret = ret + space + 'Attribute amn: ' + str(self.amn) + '\n'
		ret = ret + space + 'Attribute zion: ' + str(self.zion) + '\n'
		s = self.energy.__str__()
		ret = ret + space + 'Attribute energy\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.magn_mom.__str__()
		ret = ret + space + 'Attribute magn_mom\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.p_phi.__str__()
		ret = ret + space + 'Attribute p_phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type comstructurecom, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'energy', numpy.array(self.energy).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'magn_mom', numpy.array(self.magn_mom).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'p_phi', numpy.array(self.p_phi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.int32), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type comstructurecom, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'energy', numpy.array(self.energy).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'magn_mom', numpy.array(self.magn_mom).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'p_phi', numpy.array(self.p_phi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.int32))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type comstructurecom, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDouble(self.idx, path, cpopath + 'amn', self.amn)
		check_status(status)
		status = ull.putDouble(self.idx, path, cpopath + 'zion', self.zion)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type comstructurecom, run function getSlice') 
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
		status, ret_zion = ull.getDouble(self.idx, path, cpopath + 'zion')
		check_status(status)
		if not status:
			self.zion = ret_zion
		status, ret_energy, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'energy', inTime, interpolMode)
		check_status(status)
		if not status:
			self.energy = ret_energy
			self.cpoTime = retTime
		status, ret_magn_mom, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'magn_mom', inTime, interpolMode)
		check_status(status)
		if not status:
			self.magn_mom = ret_magn_mom
			self.cpoTime = retTime
		status, ret_p_phi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'p_phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.p_phi = ret_p_phi
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type comstructurecom, run function build_non_resampled_data') 
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
			status, zionVal = ull.getDouble(self.idx, path, cpopath + 'zion')
			check_status(status)
			status, energyList = ull.getVect2DDouble(self.idx, path, cpopath + 'energy')
			if len(energyList) == 0:
				energyList = numpy.resize(energyList, (0,nbslice))
			check_status(status)
			status, magn_momList = ull.getVect2DDouble(self.idx, path, cpopath + 'magn_mom')
			if len(magn_momList) == 0:
				magn_momList = numpy.resize(magn_momList, (0,nbslice))
			check_status(status)
			status, p_phiList = ull.getVect2DDouble(self.idx, path, cpopath + 'p_phi')
			if len(p_phiList) == 0:
				p_phiList = numpy.resize(p_phiList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DInt(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = comstructurecom(self.base_path)
				slice.setExpIdx(self.idx)
				slice.amn = amnVal
				slice.zion = zionVal
				slice.energy = energyList[:,i]
				slice.magn_mom = magn_momList[:,i]
				slice.p_phi = p_phiList[:,i]
				slice.sigma = sigmaList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type comstructurecomObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'energy', i, numpy.array(self.energy).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'magn_mom') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'magn_mom', i, numpy.array(self.magn_mom).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'p_phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'p_phi', i, numpy.array(self.p_phi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.int32))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type comstructurecomObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		status, ret_energy = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'energy', i)
		check_status(status)
		if not status:
			self.energy = ret_energy
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'magn_mom') 
			print ('obj = ' + str(obj))
		status, ret_magn_mom = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'magn_mom', i)
		check_status(status)
		if not status:
			self.magn_mom = ret_magn_mom
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'p_phi') 
			print ('obj = ' + str(obj))
		status, ret_p_phi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'p_phi', i)
		check_status(status)
		if not status:
			self.p_phi = ret_p_phi
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type comstructurecomObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'amn', i, self.amn)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zion') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zion', i, self.zion)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type comstructurecomObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		status, ret_amn = ull.getDoubleFromObject(self.idx, obj, cpopath + 'amn', i)
		check_status(status)
		if not status:
			self.amn = ret_amn
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'zion') 
			print ('obj = ' + str(obj))
		status, ret_zion = ull.getDoubleFromObject(self.idx, obj, cpopath + 'zion', i)
		check_status(status)
		if not status:
			self.zion = ret_zion

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'amn')
		ull.deleteData(self.idx, path, cpopath + 'zion')
		ull.deleteData(self.idx, path, cpopath + 'energy')
		ull.deleteData(self.idx, path, cpopath + 'magn_mom')
		ull.deleteData(self.idx, path, cpopath + 'p_phi')
		ull.deleteData(self.idx, path, cpopath + 'sigma')


class tracestructuretrace:
	'''
	class tracestructuretrace
	Position of particle in 5D space (3D in real and 2D in velocity).

	Attributes:
	- time_orb : numpy.ndarray 2D with float
	   Time along the orbit  [s]; Time-dependent; Matrix (norbits, max_ntorb)
	- ntorb : numpy.ndarray 1D with int)
	   Number of time slices along the orbit, for each orbit. Time-dependent; Vector (norbits)
	- r : numpy.ndarray 2D with float
	   Major radius of the guiding centre [m], Major radius; Time-dependent; Matrix (norbits, max_ntorb). 
	- z : numpy.ndarray 2D with float
	   Altitude of the guiding centre [m]; Time-dependent; Matrix (norbits, max_ntorb).
	- phi : numpy.ndarray 2D with float
	   Toroidal angle of the guiding centre [rad]; Time-dependent; Matrix (norbits, max_ntorb).
	- psi : numpy.ndarray 2D with float
	   Guiding centre position in psi [normalised poloidal flux]; Time-dependent; Matrix (norbits, max_ntorb)).
	- theta_b : numpy.ndarray 2D with float
	   Position of the guiding centre in poloidal Boozer angle [rad]; Time-dependent; Matrix (norbits, max_ntorb). 
	- v_parallel : numpy.ndarray 2D with float
	   Parallel velocity along the orbit [m/s]; Time-dependent; Matrix (norbits, max_ntorb).
	- v_perp : numpy.ndarray 2D with float
	   Perpendicular velocity along the orbit [m/s]; Time-dependent; Matrix (norbits, max_ntorb).
	'''

	def __init__(self, base_path_in='trace'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.time_orb = numpy.zeros((0,0), numpy.float64, order='C')
		self.ntorb = numpy.zeros(0, numpy.int32, order='C')
		self.r = numpy.zeros((0,0), numpy.float64, order='C')
		self.z = numpy.zeros((0,0), numpy.float64, order='C')
		self.phi = numpy.zeros((0,0), numpy.float64, order='C')
		self.psi = numpy.zeros((0,0), numpy.float64, order='C')
		self.theta_b = numpy.zeros((0,0), numpy.float64, order='C')
		self.v_parallel = numpy.zeros((0,0), numpy.float64, order='C')
		self.v_perp = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class tracestructuretrace\n'
		s = self.time_orb.__str__()
		ret = ret + space + 'Attribute time_orb\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ntorb.__str__()
		ret = ret + space + 'Attribute ntorb\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta_b.__str__()
		ret = ret + space + 'Attribute theta_b\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.v_parallel.__str__()
		ret = ret + space + 'Attribute v_parallel\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.v_perp.__str__()
		ret = ret + space + 'Attribute v_perp\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type tracestructuretrace, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'time_orb', numpy.array(self.time_orb).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'ntorb', numpy.array(self.ntorb).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'theta_b', numpy.array(self.theta_b).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'v_parallel', numpy.array(self.v_parallel).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'v_perp', numpy.array(self.v_perp).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type tracestructuretrace, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'time_orb', numpy.array(self.time_orb).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'ntorb', numpy.array(self.ntorb).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'theta_b', numpy.array(self.theta_b).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'v_parallel', numpy.array(self.v_parallel).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'v_perp', numpy.array(self.v_perp).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type tracestructuretrace, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type tracestructuretrace, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_time_orb, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'time_orb', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time_orb = ret_time_orb
			self.cpoTime = retTime
		status, ret_ntorb, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'ntorb', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ntorb = ret_ntorb
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
		status, ret_phi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi = ret_phi
			self.cpoTime = retTime
		status, ret_psi, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'psi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.psi = ret_psi
			self.cpoTime = retTime
		status, ret_theta_b, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'theta_b', inTime, interpolMode)
		check_status(status)
		if not status:
			self.theta_b = ret_theta_b
			self.cpoTime = retTime
		status, ret_v_parallel, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'v_parallel', inTime, interpolMode)
		check_status(status)
		if not status:
			self.v_parallel = ret_v_parallel
			self.cpoTime = retTime
		status, ret_v_perp, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'v_perp', inTime, interpolMode)
		check_status(status)
		if not status:
			self.v_perp = ret_v_perp
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type tracestructuretrace, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, time_orbList = ull.getVect3DDouble(self.idx, path, cpopath + 'time_orb')
			if len(time_orbList) == 0:
				time_orbList = numpy.resize(time_orbList, (0,0,nbslice))
			check_status(status)
			status, ntorbList = ull.getVect2DInt(self.idx, path, cpopath + 'ntorb')
			if len(ntorbList) == 0:
				ntorbList = numpy.resize(ntorbList, (0,nbslice))
			check_status(status)
			status, rList = ull.getVect3DDouble(self.idx, path, cpopath + 'r')
			if len(rList) == 0:
				rList = numpy.resize(rList, (0,0,nbslice))
			check_status(status)
			status, zList = ull.getVect3DDouble(self.idx, path, cpopath + 'z')
			if len(zList) == 0:
				zList = numpy.resize(zList, (0,0,nbslice))
			check_status(status)
			status, phiList = ull.getVect3DDouble(self.idx, path, cpopath + 'phi')
			if len(phiList) == 0:
				phiList = numpy.resize(phiList, (0,0,nbslice))
			check_status(status)
			status, psiList = ull.getVect3DDouble(self.idx, path, cpopath + 'psi')
			if len(psiList) == 0:
				psiList = numpy.resize(psiList, (0,0,nbslice))
			check_status(status)
			status, theta_bList = ull.getVect3DDouble(self.idx, path, cpopath + 'theta_b')
			if len(theta_bList) == 0:
				theta_bList = numpy.resize(theta_bList, (0,0,nbslice))
			check_status(status)
			status, v_parallelList = ull.getVect3DDouble(self.idx, path, cpopath + 'v_parallel')
			if len(v_parallelList) == 0:
				v_parallelList = numpy.resize(v_parallelList, (0,0,nbslice))
			check_status(status)
			status, v_perpList = ull.getVect3DDouble(self.idx, path, cpopath + 'v_perp')
			if len(v_perpList) == 0:
				v_perpList = numpy.resize(v_perpList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = tracestructuretrace(self.base_path)
				slice.setExpIdx(self.idx)
				slice.time_orb = time_orbList[:,:,i]
				slice.ntorb = ntorbList[:,i]
				slice.r = rList[:,:,i]
				slice.z = zList[:,:,i]
				slice.phi = phiList[:,:,i]
				slice.psi = psiList[:,:,i]
				slice.theta_b = theta_bList[:,:,i]
				slice.v_parallel = v_parallelList[:,:,i]
				slice.v_perp = v_perpList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tracestructuretraceObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'time_orb') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'time_orb', i, numpy.array(self.time_orb).astype(numpy.float64))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'ntorb') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'ntorb', i, numpy.array(self.ntorb).astype(numpy.int32))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'psi', i, numpy.array(self.psi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'theta_b') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'theta_b', i, numpy.array(self.theta_b).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'v_parallel') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'v_parallel', i, numpy.array(self.v_parallel).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'v_perp') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'v_perp', i, numpy.array(self.v_perp).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tracestructuretraceObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'time_orb') 
			print ('obj = ' + str(obj))
		status, ret_time_orb = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'time_orb', i)
		check_status(status)
		if not status:
			self.time_orb = ret_time_orb
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'ntorb') 
			print ('obj = ' + str(obj))
		status, ret_ntorb = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'ntorb', i)
		check_status(status)
		if not status:
			self.ntorb = ret_ntorb
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
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		status, ret_psi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'psi', i)
		check_status(status)
		if not status:
			self.psi = ret_psi
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'theta_b') 
			print ('obj = ' + str(obj))
		status, ret_theta_b = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'theta_b', i)
		check_status(status)
		if not status:
			self.theta_b = ret_theta_b
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'v_parallel') 
			print ('obj = ' + str(obj))
		status, ret_v_parallel = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'v_parallel', i)
		check_status(status)
		if not status:
			self.v_parallel = ret_v_parallel
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'v_perp') 
			print ('obj = ' + str(obj))
		status, ret_v_perp = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'v_perp', i)
		check_status(status)
		if not status:
			self.v_perp = ret_v_perp

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tracestructuretraceObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tracestructuretraceObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'time_orb')
		ull.deleteData(self.idx, path, cpopath + 'ntorb')
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'z')
		ull.deleteData(self.idx, path, cpopath + 'phi')
		ull.deleteData(self.idx, path, cpopath + 'psi')
		ull.deleteData(self.idx, path, cpopath + 'theta_b')
		ull.deleteData(self.idx, path, cpopath + 'v_parallel')
		ull.deleteData(self.idx, path, cpopath + 'v_perp')


class global_paramstructureorbit_global_param:
	'''
	class global_paramstructureorbit_global_param
	Global quantities associated with an orbit.

	Attributes:
	- orbit_type : numpy.ndarray 1D with int)
	   Identifier of orbit type: 0 trapped, -1 co-passing, + 1 counter-passing ; Time-dependent; Vector (norbits)
	- omega_b : numpy.ndarray 1D with float
	   Bounce angular frequency rad/s; Time-dependent; Vector (norbits)
	- omega_phi : numpy.ndarray 1D with float
	   Toroidal angular precession frequency [rad/s]; Time-dependent; Vector (norbits).
	- omega_c_av : numpy.ndarray 1D with float
	   Orbit averaged cyclotron frequency [rad/a]; Time-dependent; Vector(norbits).
	- special_pos : class special_posstructureorbit_special_pos
	   Special positions along an orbit (like turning points).
	'''

	def __init__(self, base_path_in='global_param'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.orbit_type = numpy.zeros(0, numpy.int32, order='C')
		self.omega_b = numpy.zeros(0, numpy.float64, order='C')
		self.omega_phi = numpy.zeros(0, numpy.float64, order='C')
		self.omega_c_av = numpy.zeros(0, numpy.float64, order='C')
		self.special_pos = special_posstructureorbit_special_pos('special_pos')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class global_paramstructureorbit_global_param\n'
		s = self.orbit_type.__str__()
		ret = ret + space + 'Attribute orbit_type\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.omega_b.__str__()
		ret = ret + space + 'Attribute omega_b\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.omega_phi.__str__()
		ret = ret + space + 'Attribute omega_phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.omega_c_av.__str__()
		ret = ret + space + 'Attribute omega_c_av\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute special_pos\n ' + self.special_pos.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.special_pos.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructureorbit_global_param, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'orbit_type', numpy.array(self.orbit_type).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'omega_b', numpy.array(self.omega_b).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'omega_phi', numpy.array(self.omega_phi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'omega_c_av', numpy.array(self.omega_c_av).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.special_pos.cpoTime = self.cpoTime
		self.special_pos.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructureorbit_global_param, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'orbit_type', numpy.array(self.orbit_type).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'omega_b', numpy.array(self.omega_b).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'omega_phi', numpy.array(self.omega_phi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'omega_c_av', numpy.array(self.omega_c_av).astype(numpy.float64))
		check_status(status)
		self.special_pos.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructureorbit_global_param, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.special_pos.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructureorbit_global_param, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_orbit_type, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'orbit_type', inTime, interpolMode)
		check_status(status)
		if not status:
			self.orbit_type = ret_orbit_type
			self.cpoTime = retTime
		status, ret_omega_b, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'omega_b', inTime, interpolMode)
		check_status(status)
		if not status:
			self.omega_b = ret_omega_b
			self.cpoTime = retTime
		status, ret_omega_phi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'omega_phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.omega_phi = ret_omega_phi
			self.cpoTime = retTime
		status, ret_omega_c_av, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'omega_c_av', inTime, interpolMode)
		check_status(status)
		if not status:
			self.omega_c_av = ret_omega_c_av
			self.cpoTime = retTime
		self.special_pos.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructureorbit_global_param, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, orbit_typeList = ull.getVect2DInt(self.idx, path, cpopath + 'orbit_type')
			if len(orbit_typeList) == 0:
				orbit_typeList = numpy.resize(orbit_typeList, (0,nbslice))
			check_status(status)
			status, omega_bList = ull.getVect2DDouble(self.idx, path, cpopath + 'omega_b')
			if len(omega_bList) == 0:
				omega_bList = numpy.resize(omega_bList, (0,nbslice))
			check_status(status)
			status, omega_phiList = ull.getVect2DDouble(self.idx, path, cpopath + 'omega_phi')
			if len(omega_phiList) == 0:
				omega_phiList = numpy.resize(omega_phiList, (0,nbslice))
			check_status(status)
			status, omega_c_avList = ull.getVect2DDouble(self.idx, path, cpopath + 'omega_c_av')
			if len(omega_c_avList) == 0:
				omega_c_avList = numpy.resize(omega_c_avList, (0,nbslice))
			check_status(status)
			special_posList = self.special_pos.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = global_paramstructureorbit_global_param(self.base_path)
				slice.setExpIdx(self.idx)
				slice.orbit_type = orbit_typeList[:,i]
				slice.omega_b = omega_bList[:,i]
				slice.omega_phi = omega_phiList[:,i]
				slice.omega_c_av = omega_c_avList[:,i]
				slice.special_pos = special_posList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructureorbit_global_paramObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'orbit_type') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'orbit_type', i, numpy.array(self.orbit_type).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'omega_b') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'omega_b', i, numpy.array(self.omega_b).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'omega_phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'omega_phi', i, numpy.array(self.omega_phi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'omega_c_av') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'omega_c_av', i, numpy.array(self.omega_c_av).astype(numpy.float64))
		obj = self.special_pos.putTimedElt(path, cpopath + 'special_pos', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructureorbit_global_paramObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'orbit_type') 
			print ('obj = ' + str(obj))
		status, ret_orbit_type = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'orbit_type', i)
		check_status(status)
		if not status:
			self.orbit_type = ret_orbit_type
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'omega_b') 
			print ('obj = ' + str(obj))
		status, ret_omega_b = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'omega_b', i)
		check_status(status)
		if not status:
			self.omega_b = ret_omega_b
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'omega_phi') 
			print ('obj = ' + str(obj))
		status, ret_omega_phi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'omega_phi', i)
		check_status(status)
		if not status:
			self.omega_phi = ret_omega_phi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'omega_c_av') 
			print ('obj = ' + str(obj))
		status, ret_omega_c_av = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'omega_c_av', i)
		check_status(status)
		if not status:
			self.omega_c_av = ret_omega_c_av
		self.special_pos.getTimedElt(path, cpopath + 'special_pos', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructureorbit_global_paramObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.special_pos.putNonTimedElt(path, cpopath + 'special_pos', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructureorbit_global_paramObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.special_pos.getNonTimedElt(path, cpopath + 'special_pos', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'orbit_type')
		ull.deleteData(self.idx, path, cpopath + 'omega_b')
		ull.deleteData(self.idx, path, cpopath + 'omega_phi')
		ull.deleteData(self.idx, path, cpopath + 'omega_c_av')
		self.special_pos.deleteData(path, cpopath)


class special_posstructureorbit_special_pos:
	'''
	class special_posstructureorbit_special_pos
	Special positions along an orbit (like turning points).

	Attributes:
	- midplane : class midplanestructureorbit_midplane
	   Intersections with the midplane
	- turning_pts : class turning_ptsstructureorbit_turning_pts
	   Location of turning points
	'''

	def __init__(self, base_path_in='special_pos'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.midplane = midplanestructureorbit_midplane('midplane')
		self.turning_pts = turning_ptsstructureorbit_turning_pts('turning_pts')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class special_posstructureorbit_special_pos\n'
		ret = ret + space + 'Attribute midplane\n ' + self.midplane.__str__(depth+1)
		ret = ret + space + 'Attribute turning_pts\n ' + self.turning_pts.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.midplane.setExpIdx(idx)
		self.turning_pts.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type special_posstructureorbit_special_pos, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.midplane.cpoTime = self.cpoTime
		self.midplane.putSlice(path, cpopath)
		self.turning_pts.cpoTime = self.cpoTime
		self.turning_pts.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type special_posstructureorbit_special_pos, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.midplane.replaceLastSlice(path, cpopath)
		self.turning_pts.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type special_posstructureorbit_special_pos, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.midplane.putNonTimed(path, cpopath)
		self.turning_pts.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type special_posstructureorbit_special_pos, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.midplane.getSlice(path, cpopath, inTime, interpolMode)
		self.turning_pts.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type special_posstructureorbit_special_pos, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			midplaneList = self.midplane.build_non_resampled_data(path, cpopath, nbslice)
			turning_ptsList = self.turning_pts.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = special_posstructureorbit_special_pos(self.base_path)
				slice.setExpIdx(self.idx)
				slice.midplane = midplaneList[i]
				slice.turning_pts = turning_ptsList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type special_posstructureorbit_special_posObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.midplane.putTimedElt(path, cpopath + 'midplane', i, obj)
		obj = self.turning_pts.putTimedElt(path, cpopath + 'turning_pts', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type special_posstructureorbit_special_posObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.midplane.getTimedElt(path, cpopath + 'midplane', i, obj)
		self.turning_pts.getTimedElt(path, cpopath + 'turning_pts', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type special_posstructureorbit_special_posObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.midplane.putNonTimedElt(path, cpopath + 'midplane', i, obj)
		obj = self.turning_pts.putNonTimedElt(path, cpopath + 'turning_pts', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type special_posstructureorbit_special_posObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.midplane.getNonTimedElt(path, cpopath + 'midplane', i, obj)
		self.turning_pts.getNonTimedElt(path, cpopath + 'turning_pts', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.midplane.deleteData(path, cpopath)
		self.turning_pts.deleteData(path, cpopath)


class midplanestructureorbit_midplane:
	'''
	class midplanestructureorbit_midplane
	Intersections with the midplane

	Attributes:
	- outer : class outerstructureorbit_pos
	   Position at outer mid-plane
	- inner : class innerstructureorbit_pos
	   Position at inner mid-plane
	'''

	def __init__(self, base_path_in='midplane'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.outer = outerstructureorbit_pos('outer')
		self.inner = innerstructureorbit_pos('inner')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class midplanestructureorbit_midplane\n'
		ret = ret + space + 'Attribute outer\n ' + self.outer.__str__(depth+1)
		ret = ret + space + 'Attribute inner\n ' + self.inner.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.outer.setExpIdx(idx)
		self.inner.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type midplanestructureorbit_midplane, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.outer.cpoTime = self.cpoTime
		self.outer.putSlice(path, cpopath)
		self.inner.cpoTime = self.cpoTime
		self.inner.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type midplanestructureorbit_midplane, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.outer.replaceLastSlice(path, cpopath)
		self.inner.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type midplanestructureorbit_midplane, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.outer.putNonTimed(path, cpopath)
		self.inner.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type midplanestructureorbit_midplane, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.outer.getSlice(path, cpopath, inTime, interpolMode)
		self.inner.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type midplanestructureorbit_midplane, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			outerList = self.outer.build_non_resampled_data(path, cpopath, nbslice)
			innerList = self.inner.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = midplanestructureorbit_midplane(self.base_path)
				slice.setExpIdx(self.idx)
				slice.outer = outerList[i]
				slice.inner = innerList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type midplanestructureorbit_midplaneObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.outer.putTimedElt(path, cpopath + 'outer', i, obj)
		obj = self.inner.putTimedElt(path, cpopath + 'inner', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type midplanestructureorbit_midplaneObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.outer.getTimedElt(path, cpopath + 'outer', i, obj)
		self.inner.getTimedElt(path, cpopath + 'inner', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type midplanestructureorbit_midplaneObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.outer.putNonTimedElt(path, cpopath + 'outer', i, obj)
		obj = self.inner.putNonTimedElt(path, cpopath + 'inner', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type midplanestructureorbit_midplaneObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.outer.getNonTimedElt(path, cpopath + 'outer', i, obj)
		self.inner.getNonTimedElt(path, cpopath + 'inner', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.outer.deleteData(path, cpopath)
		self.inner.deleteData(path, cpopath)


class outerstructureorbit_pos:
	'''
	class outerstructureorbit_pos
	Position at outer mid-plane

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]; Time-dependent; Vector (norbits). 
	- z : numpy.ndarray 1D with float
	   Altitude [m]; Time-dependent; Vector (norbits).
	- phi : numpy.ndarray 1D with float
	   Toroidal angle [rad]; Time-dependent; Vector (norbits).
	- psi : numpy.ndarray 1D with float
	   Position in psi [normalised poloidal flux]; Time-dependent; Vector (norbits).
	- theta_b : numpy.ndarray 1D with float
	   Poloidal Boozer angle [rad]; Time-dependent; Vector (norbits). 
	'''

	def __init__(self, base_path_in='outer'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')
		self.psi = numpy.zeros(0, numpy.float64, order='C')
		self.theta_b = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class outerstructureorbit_pos\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta_b.__str__()
		ret = ret + space + 'Attribute theta_b\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outerstructureorbit_pos, run function putSlice') 
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
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'theta_b', numpy.array(self.theta_b).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outerstructureorbit_pos, run function replaceLastSlice') 
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
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'theta_b', numpy.array(self.theta_b).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type outerstructureorbit_pos, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type outerstructureorbit_pos, run function getSlice') 
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
		status, ret_theta_b, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'theta_b', inTime, interpolMode)
		check_status(status)
		if not status:
			self.theta_b = ret_theta_b
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type outerstructureorbit_pos, run function build_non_resampled_data') 
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
			status, theta_bList = ull.getVect2DDouble(self.idx, path, cpopath + 'theta_b')
			if len(theta_bList) == 0:
				theta_bList = numpy.resize(theta_bList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = outerstructureorbit_pos(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[:,i]
				slice.z = zList[:,i]
				slice.phi = phiList[:,i]
				slice.psi = psiList[:,i]
				slice.theta_b = theta_bList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outerstructureorbit_posObj, run function putTimedElt') 
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
			print ('putVect1DDoubleInObject : ' + cpopath + 'theta_b') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'theta_b', i, numpy.array(self.theta_b).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outerstructureorbit_posObj, run function getTimedElt') 
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
			print ('getVect1DDoubleInObject : ' + cpopath + 'theta_b') 
			print ('obj = ' + str(obj))
		status, ret_theta_b = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'theta_b', i)
		check_status(status)
		if not status:
			self.theta_b = ret_theta_b

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outerstructureorbit_posObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type outerstructureorbit_posObj, run function getNonTimedElt') 
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
		ull.deleteData(self.idx, path, cpopath + 'theta_b')


class innerstructureorbit_pos:
	'''
	class innerstructureorbit_pos
	Position at inner mid-plane

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]; Time-dependent; Vector (norbits). 
	- z : numpy.ndarray 1D with float
	   Altitude [m]; Time-dependent; Vector (norbits).
	- phi : numpy.ndarray 1D with float
	   Toroidal angle [rad]; Time-dependent; Vector (norbits).
	- psi : numpy.ndarray 1D with float
	   Position in psi [normalised poloidal flux]; Time-dependent; Vector (norbits).
	- theta_b : numpy.ndarray 1D with float
	   Poloidal Boozer angle [rad]; Time-dependent; Vector (norbits). 
	'''

	def __init__(self, base_path_in='inner'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')
		self.psi = numpy.zeros(0, numpy.float64, order='C')
		self.theta_b = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class innerstructureorbit_pos\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta_b.__str__()
		ret = ret + space + 'Attribute theta_b\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type innerstructureorbit_pos, run function putSlice') 
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
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'theta_b', numpy.array(self.theta_b).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type innerstructureorbit_pos, run function replaceLastSlice') 
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
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'theta_b', numpy.array(self.theta_b).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type innerstructureorbit_pos, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type innerstructureorbit_pos, run function getSlice') 
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
		status, ret_theta_b, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'theta_b', inTime, interpolMode)
		check_status(status)
		if not status:
			self.theta_b = ret_theta_b
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type innerstructureorbit_pos, run function build_non_resampled_data') 
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
			status, theta_bList = ull.getVect2DDouble(self.idx, path, cpopath + 'theta_b')
			if len(theta_bList) == 0:
				theta_bList = numpy.resize(theta_bList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = innerstructureorbit_pos(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[:,i]
				slice.z = zList[:,i]
				slice.phi = phiList[:,i]
				slice.psi = psiList[:,i]
				slice.theta_b = theta_bList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type innerstructureorbit_posObj, run function putTimedElt') 
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
			print ('putVect1DDoubleInObject : ' + cpopath + 'theta_b') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'theta_b', i, numpy.array(self.theta_b).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type innerstructureorbit_posObj, run function getTimedElt') 
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
			print ('getVect1DDoubleInObject : ' + cpopath + 'theta_b') 
			print ('obj = ' + str(obj))
		status, ret_theta_b = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'theta_b', i)
		check_status(status)
		if not status:
			self.theta_b = ret_theta_b

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type innerstructureorbit_posObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type innerstructureorbit_posObj, run function getNonTimedElt') 
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
		ull.deleteData(self.idx, path, cpopath + 'theta_b')


class turning_ptsstructureorbit_turning_pts:
	'''
	class turning_ptsstructureorbit_turning_pts
	Location of turning points

	Attributes:
	- upper : class upperstructureorbit_pos
	   Position at upper turning point
	- lower : class lowerstructureorbit_pos
	   Position at lower turning point
	'''

	def __init__(self, base_path_in='turning_pts'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.upper = upperstructureorbit_pos('upper')
		self.lower = lowerstructureorbit_pos('lower')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class turning_ptsstructureorbit_turning_pts\n'
		ret = ret + space + 'Attribute upper\n ' + self.upper.__str__(depth+1)
		ret = ret + space + 'Attribute lower\n ' + self.lower.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.upper.setExpIdx(idx)
		self.lower.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type turning_ptsstructureorbit_turning_pts, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.upper.cpoTime = self.cpoTime
		self.upper.putSlice(path, cpopath)
		self.lower.cpoTime = self.cpoTime
		self.lower.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type turning_ptsstructureorbit_turning_pts, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.upper.replaceLastSlice(path, cpopath)
		self.lower.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type turning_ptsstructureorbit_turning_pts, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.upper.putNonTimed(path, cpopath)
		self.lower.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type turning_ptsstructureorbit_turning_pts, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.upper.getSlice(path, cpopath, inTime, interpolMode)
		self.lower.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type turning_ptsstructureorbit_turning_pts, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			upperList = self.upper.build_non_resampled_data(path, cpopath, nbslice)
			lowerList = self.lower.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = turning_ptsstructureorbit_turning_pts(self.base_path)
				slice.setExpIdx(self.idx)
				slice.upper = upperList[i]
				slice.lower = lowerList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type turning_ptsstructureorbit_turning_ptsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.upper.putTimedElt(path, cpopath + 'upper', i, obj)
		obj = self.lower.putTimedElt(path, cpopath + 'lower', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type turning_ptsstructureorbit_turning_ptsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.upper.getTimedElt(path, cpopath + 'upper', i, obj)
		self.lower.getTimedElt(path, cpopath + 'lower', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type turning_ptsstructureorbit_turning_ptsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.upper.putNonTimedElt(path, cpopath + 'upper', i, obj)
		obj = self.lower.putNonTimedElt(path, cpopath + 'lower', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type turning_ptsstructureorbit_turning_ptsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.upper.getNonTimedElt(path, cpopath + 'upper', i, obj)
		self.lower.getNonTimedElt(path, cpopath + 'lower', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.upper.deleteData(path, cpopath)
		self.lower.deleteData(path, cpopath)


class upperstructureorbit_pos:
	'''
	class upperstructureorbit_pos
	Position at upper turning point

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]; Time-dependent; Vector (norbits). 
	- z : numpy.ndarray 1D with float
	   Altitude [m]; Time-dependent; Vector (norbits).
	- phi : numpy.ndarray 1D with float
	   Toroidal angle [rad]; Time-dependent; Vector (norbits).
	- psi : numpy.ndarray 1D with float
	   Position in psi [normalised poloidal flux]; Time-dependent; Vector (norbits).
	- theta_b : numpy.ndarray 1D with float
	   Poloidal Boozer angle [rad]; Time-dependent; Vector (norbits). 
	'''

	def __init__(self, base_path_in='upper'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')
		self.psi = numpy.zeros(0, numpy.float64, order='C')
		self.theta_b = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class upperstructureorbit_pos\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta_b.__str__()
		ret = ret + space + 'Attribute theta_b\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type upperstructureorbit_pos, run function putSlice') 
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
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'theta_b', numpy.array(self.theta_b).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type upperstructureorbit_pos, run function replaceLastSlice') 
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
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'theta_b', numpy.array(self.theta_b).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type upperstructureorbit_pos, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type upperstructureorbit_pos, run function getSlice') 
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
		status, ret_theta_b, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'theta_b', inTime, interpolMode)
		check_status(status)
		if not status:
			self.theta_b = ret_theta_b
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type upperstructureorbit_pos, run function build_non_resampled_data') 
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
			status, theta_bList = ull.getVect2DDouble(self.idx, path, cpopath + 'theta_b')
			if len(theta_bList) == 0:
				theta_bList = numpy.resize(theta_bList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = upperstructureorbit_pos(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[:,i]
				slice.z = zList[:,i]
				slice.phi = phiList[:,i]
				slice.psi = psiList[:,i]
				slice.theta_b = theta_bList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type upperstructureorbit_posObj, run function putTimedElt') 
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
			print ('putVect1DDoubleInObject : ' + cpopath + 'theta_b') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'theta_b', i, numpy.array(self.theta_b).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type upperstructureorbit_posObj, run function getTimedElt') 
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
			print ('getVect1DDoubleInObject : ' + cpopath + 'theta_b') 
			print ('obj = ' + str(obj))
		status, ret_theta_b = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'theta_b', i)
		check_status(status)
		if not status:
			self.theta_b = ret_theta_b

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type upperstructureorbit_posObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type upperstructureorbit_posObj, run function getNonTimedElt') 
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
		ull.deleteData(self.idx, path, cpopath + 'theta_b')


class lowerstructureorbit_pos:
	'''
	class lowerstructureorbit_pos
	Position at lower turning point

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]; Time-dependent; Vector (norbits). 
	- z : numpy.ndarray 1D with float
	   Altitude [m]; Time-dependent; Vector (norbits).
	- phi : numpy.ndarray 1D with float
	   Toroidal angle [rad]; Time-dependent; Vector (norbits).
	- psi : numpy.ndarray 1D with float
	   Position in psi [normalised poloidal flux]; Time-dependent; Vector (norbits).
	- theta_b : numpy.ndarray 1D with float
	   Poloidal Boozer angle [rad]; Time-dependent; Vector (norbits). 
	'''

	def __init__(self, base_path_in='lower'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')
		self.psi = numpy.zeros(0, numpy.float64, order='C')
		self.theta_b = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class lowerstructureorbit_pos\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta_b.__str__()
		ret = ret + space + 'Attribute theta_b\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type lowerstructureorbit_pos, run function putSlice') 
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
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'theta_b', numpy.array(self.theta_b).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type lowerstructureorbit_pos, run function replaceLastSlice') 
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
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'theta_b', numpy.array(self.theta_b).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type lowerstructureorbit_pos, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type lowerstructureorbit_pos, run function getSlice') 
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
		status, ret_theta_b, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'theta_b', inTime, interpolMode)
		check_status(status)
		if not status:
			self.theta_b = ret_theta_b
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type lowerstructureorbit_pos, run function build_non_resampled_data') 
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
			status, theta_bList = ull.getVect2DDouble(self.idx, path, cpopath + 'theta_b')
			if len(theta_bList) == 0:
				theta_bList = numpy.resize(theta_bList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = lowerstructureorbit_pos(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[:,i]
				slice.z = zList[:,i]
				slice.phi = phiList[:,i]
				slice.psi = psiList[:,i]
				slice.theta_b = theta_bList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type lowerstructureorbit_posObj, run function putTimedElt') 
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
			print ('putVect1DDoubleInObject : ' + cpopath + 'theta_b') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'theta_b', i, numpy.array(self.theta_b).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type lowerstructureorbit_posObj, run function getTimedElt') 
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
			print ('getVect1DDoubleInObject : ' + cpopath + 'theta_b') 
			print ('obj = ' + str(obj))
		status, ret_theta_b = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'theta_b', i)
		check_status(status)
		if not status:
			self.theta_b = ret_theta_b

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type lowerstructureorbit_posObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type lowerstructureorbit_posObj, run function getNonTimedElt') 
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
		ull.deleteData(self.idx, path, cpopath + 'theta_b')


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
