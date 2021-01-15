# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class distsource(KeepInOrder):
	'''
	class distsource
	Sources of particles for input to kinetic equations, e.g. Fokker-Planck calculation. The sources could originate from e.g. NBI or fusion reactions. Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- composition : class compositionstructurecomposition
	   Plasma composition (description of ion species). OBSOLESCENT.
	- compositions : class compositionsstructurecompositions_type
	   Contains detailed information on the plasma composition (main ions, impurities, neutrals, edge species).
	- source : class sourcestruct_arraydistsource_source: array of sourcestruct_arraydistsource_sourceObj objects
	   Source. Time-dependent. Structure array(nsrc_spec)
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; scalar
	'''

	def __init__(self):
		self.base_path = 'distsource'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 4
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.composition = compositionstructurecomposition('composition')
		self.compositions = compositionsstructurecompositions_type('compositions')
		self.source = sourcestruct_arraydistsource_source('source')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class distsource\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute composition\n ' + self.composition.__str__(depth+1)
		ret = ret + space + 'Attribute compositions\n ' + self.compositions.__str__(depth+1)
		ret = ret + space + 'Attribute source\n ' + self.source.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.composition.setExpIdx(idx)
		self.compositions.setExpIdx(idx)
		self.source.setExpIdx(idx)
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
		self.composition.cpoTime = self.cpoTime
		self.composition.putSlice(path, cpopath)
		self.compositions.cpoTime = self.cpoTime
		self.compositions.putSlice(path, cpopath)
		self.source.cpoTime = self.cpoTime
		self.source.putSlice(path, cpopath)
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
		self.composition.replaceLastSlice(path, cpopath)
		self.compositions.replaceLastSlice(path, cpopath)
		self.source.replaceLastSlice(path, cpopath)
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
		self.composition.putNonTimed(path, cpopath)
		self.compositions.putNonTimed(path, cpopath)
		self.source.putNonTimed(path, cpopath)
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
		self.composition.getSlice(path, cpopath, inTime, interpolMode)
		self.compositions.getSlice(path, cpopath, inTime, interpolMode)
		self.source.getSlice(path, cpopath, inTime, interpolMode)
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
			compositionList = self.composition.build_non_resampled_data(path, cpopath, nbslice)
			compositionsList = self.compositions.build_non_resampled_data(path, cpopath, nbslice)
			sourceList = self.source.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = distsource()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.composition = compositionList[i]
				slice.compositions = compositionsList[i]
				slice.source = sourceList[i]
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
		self.composition.deleteData(path, cpopath)
		self.compositions.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'source')
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class distsourceArray:
	'''
	class distsourceArray
	Sources of particles for input to kinetic equations, e.g. Fokker-Planck calculation. The sources could originate from e.g. NBI or fusion reactions. Time-dependent CPO

	Attributes:
	- array : list of distsource
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
		ret = space + 'class distsourceArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'distsource cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = distsource()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(distsource())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = distsource()
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


class sourcestruct_arraydistsource_source:
	'''
	class sourcestruct_arraydistsource_source
	Source. Time-dependent. Structure array(nsrc_spec)

	Attributes:
	- array : list of sourcestruct_arraydistsource_sourceObj 
	'''

	def __init__(self, base_path_in='source'):
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
		ret = space + 'class sourcestruct_arraydistsource_source\n'
		for i in range(len(self.array)):
			ret = ret + space + 'sourcestruct_arraydistsource_source[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(sourcestruct_arraydistsource_sourceObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type sourcestruct_arraydistsource_source, run function putSlice') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraydistsource_source, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraydistsource_source, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraydistsource_source, run function getSlice') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraydistsource_source, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(sourcestruct_arraydistsource_source(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(sourcestruct_arraydistsource_source(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = sourcestruct_arraydistsource_source(self.base_path)
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
			print ('field '+self.base_path+' of type sourcestruct_arraydistsource_source, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraydistsource_source, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraydistsource_source, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraydistsource_source, run function getNonTimedElt') 
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


class sourcestruct_arraydistsource_sourceObj(KeepInOrder):
	'''
	class sourcestruct_arraydistsource_sourceObj
	Source. Time-dependent. Structure array(nsrc_spec)

	Attributes:
	- source_id : class source_idstruct_arrayenum_instance: array of source_idstruct_arrayenum_instanceObj objects
	   List of identifiers for the source, in term the type and name of the injectors and reactions that provide the source, along with an index separating sources with the same name and type. Possible content for type: NBI or reaction names (see distsource_types in the Documentation website under Conventions/Enumerated_datatypes); the field name should either be taken from nbi(*)%nbi_unit(*)%name, or describe the populations involved in the reaction, e.g. fast-thermal; the field index should separate different sources generated from a single injector or reaction. Vector(n_injectors_and_reactions)
	- species : class speciesstructurespecies_reference
	   Defines the source species represented in this element of the vector /distsource/source. Time-dependent
	- gyro_type : int
	   Defines how to interpret the spatial coordinates: 1 = given at the actual particle birth point;  2 =given at the gyro centre of the birth point.
	- global_param : class global_paramstructuredistsource_global_param
	   Global parameters.
	- profiles_1d : class profiles_1dstructuredistsource_profiles_1d
	   1D radial profiles
	- profiles_2d : class profiles_2dstructuredistsource_profiles_2d
	   2D source profiles in terms of two phase space coordinates
	- line_srcprof : class line_srcprofstruct_arraydistsource_line_src_prof: array of line_srcprofstruct_arraydistsource_line_src_profObj objects
	   1D profiles representation of a line source. Time-dependent
	- source_rate : class source_ratestructuresource_rate
	   Source density of particles in phase space (real space, velocity space, spin state).
	- markers : class markersstructureweighted_markers
	   Source given as a set of markers (test particles) born per second.
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='source'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.source_id = source_idstruct_arrayenum_instance('source_id')
		self.species = speciesstructurespecies_reference('species')
		self.gyro_type = EMPTY_INT
		self.global_param = global_paramstructuredistsource_global_param('global_param')
		self.profiles_1d = profiles_1dstructuredistsource_profiles_1d('profiles_1d')
		self.profiles_2d = profiles_2dstructuredistsource_profiles_2d('profiles_2d')
		self.line_srcprof = line_srcprofstruct_arraydistsource_line_src_prof('line_srcprof')
		self.source_rate = source_ratestructuresource_rate('source_rate')
		self.markers = markersstructureweighted_markers('markers')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class sourcestruct_arraydistsource_sourceObj\n'
		ret = ret + space + 'Attribute source_id\n ' + self.source_id.__str__(depth+1)
		ret = ret + space + 'Attribute species\n ' + self.species.__str__(depth+1)
		ret = ret + space + 'Attribute gyro_type: ' + str(self.gyro_type) + '\n'
		ret = ret + space + 'Attribute global_param\n ' + self.global_param.__str__(depth+1)
		ret = ret + space + 'Attribute profiles_1d\n ' + self.profiles_1d.__str__(depth+1)
		ret = ret + space + 'Attribute profiles_2d\n ' + self.profiles_2d.__str__(depth+1)
		ret = ret + space + 'Attribute line_srcprof\n ' + self.line_srcprof.__str__(depth+1)
		ret = ret + space + 'Attribute source_rate\n ' + self.source_rate.__str__(depth+1)
		ret = ret + space + 'Attribute markers\n ' + self.markers.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.source_id.setExpIdx(idx)
		self.species.setExpIdx(idx)
		self.global_param.setExpIdx(idx)
		self.profiles_1d.setExpIdx(idx)
		self.profiles_2d.setExpIdx(idx)
		self.line_srcprof.setExpIdx(idx)
		self.source_rate.setExpIdx(idx)
		self.markers.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcestruct_arraydistsource_sourceObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.species.putTimedElt(path, cpopath + 'species', i, obj)
		obj = self.global_param.putTimedElt(path, cpopath + 'global_param', i, obj)
		obj = self.profiles_1d.putTimedElt(path, cpopath + 'profiles_1d', i, obj)
		obj = self.profiles_2d.putTimedElt(path, cpopath + 'profiles_2d', i, obj)
		obj = self.line_srcprof.putTimedElt(path, cpopath + 'line_srcprof', i, obj)
		obj = self.source_rate.putTimedElt(path, cpopath + 'source_rate', i, obj)
		obj = self.markers.putTimedElt(path, cpopath + 'markers', i, obj)
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcestruct_arraydistsource_sourceObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.species.getTimedElt(path, cpopath + 'species', i, obj)
		self.global_param.getTimedElt(path, cpopath + 'global_param', i, obj)
		self.profiles_1d.getTimedElt(path, cpopath + 'profiles_1d', i, obj)
		self.profiles_2d.getTimedElt(path, cpopath + 'profiles_2d', i, obj)
		self.line_srcprof.getTimedElt(path, cpopath + 'line_srcprof', i, obj)
		self.source_rate.getTimedElt(path, cpopath + 'source_rate', i, obj)
		self.markers.getTimedElt(path, cpopath + 'markers', i, obj)
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcestruct_arraydistsource_sourceObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.source_id.putNonTimedElt(path, cpopath + 'source_id', i, obj)
		obj = self.species.putNonTimedElt(path, cpopath + 'species', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'gyro_type') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'gyro_type', i, self.gyro_type)
		obj = self.global_param.putNonTimedElt(path, cpopath + 'global_param', i, obj)
		obj = self.profiles_1d.putNonTimedElt(path, cpopath + 'profiles_1d', i, obj)
		obj = self.profiles_2d.putNonTimedElt(path, cpopath + 'profiles_2d', i, obj)
		obj = self.line_srcprof.putNonTimedElt(path, cpopath + 'line_srcprof', i, obj)
		obj = self.source_rate.putNonTimedElt(path, cpopath + 'source_rate', i, obj)
		obj = self.markers.putNonTimedElt(path, cpopath + 'markers', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcestruct_arraydistsource_sourceObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.source_id.getNonTimedElt(path, cpopath + 'source_id', i, obj)
		self.species.getNonTimedElt(path, cpopath + 'species', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'gyro_type') 
			print ('obj = ' + str(obj))
		status, ret_gyro_type = ull.getIntFromObject(self.idx, obj, cpopath + 'gyro_type', i)
		check_status(status)
		if not status:
			self.gyro_type = ret_gyro_type
		self.global_param.getNonTimedElt(path, cpopath + 'global_param', i, obj)
		self.profiles_1d.getNonTimedElt(path, cpopath + 'profiles_1d', i, obj)
		self.profiles_2d.getNonTimedElt(path, cpopath + 'profiles_2d', i, obj)
		self.line_srcprof.getNonTimedElt(path, cpopath + 'line_srcprof', i, obj)
		self.source_rate.getNonTimedElt(path, cpopath + 'source_rate', i, obj)
		self.markers.getNonTimedElt(path, cpopath + 'markers', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)


class source_idstruct_arrayenum_instance:
	'''
	class source_idstruct_arrayenum_instance
	List of identifiers for the source, in term the type and name of the injectors and reactions that provide the source, along with an index separating sources with the same name and type. Possible content for type: NBI or reaction names (see distsource_types in the Documentation website under Conventions/Enumerated_datatypes); the field name should either be taken from nbi(*)%nbi_unit(*)%name, or describe the populations involved in the reaction, e.g. fast-thermal; the field index should separate different sources generated from a single injector or reaction. Vector(n_injectors_and_reactions)

	Attributes:
	- array : list of source_idstruct_arrayenum_instanceObj 
	'''

	def __init__(self, base_path_in='source_id'):
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
		ret = space + 'class source_idstruct_arrayenum_instance\n'
		for i in range(len(self.array)):
			ret = ret + space + 'source_idstruct_arrayenum_instance[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(source_idstruct_arrayenum_instanceObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_idstruct_arrayenum_instance, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_idstruct_arrayenum_instance, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_idstruct_arrayenum_instance, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type source_idstruct_arrayenum_instance, run function getSlice') 
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
			print ('field '+self.base_path+' of type source_idstruct_arrayenum_instance, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(source_idstruct_arrayenum_instance(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = source_idstruct_arrayenum_instance(self.base_path)
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
			print ('field '+self.base_path+' of type source_idstruct_arrayenum_instance, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type source_idstruct_arrayenum_instance, run function getNonTimedElt') 
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


class source_idstruct_arrayenum_instanceObj(KeepInOrder):
	'''
	class source_idstruct_arrayenum_instanceObj
	List of identifiers for the source, in term the type and name of the injectors and reactions that provide the source, along with an index separating sources with the same name and type. Possible content for type: NBI or reaction names (see distsource_types in the Documentation website under Conventions/Enumerated_datatypes); the field name should either be taken from nbi(*)%nbi_unit(*)%name, or describe the populations involved in the reaction, e.g. fast-thermal; the field index should separate different sources generated from a single injector or reaction. Vector(n_injectors_and_reactions)

	Attributes:
	- type : class typestructureidentifier
	   Identify the type of the object or process.
	- name : str
	   The name of the object or process. Here the object should be an instans of the type specified in the field type.
	- index : int
	   Index the separating objects or processes with the same name.
	'''

	def __init__(self, base_path_in='source_id'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.type = typestructureidentifier('type')
		self.name = ''
		self.index = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class source_idstruct_arrayenum_instanceObj\n'
		ret = ret + space + 'Attribute type\n ' + self.type.__str__(depth+1)
		ret = ret + space + 'Attribute name: ' + str(self.name) + '\n'
		ret = ret + space + 'Attribute index: ' + str(self.index) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.type.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_idstruct_arrayenum_instanceObj, run function putNonTimedElt') 
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
			print ('object of type source_idstruct_arrayenum_instanceObj, run function getNonTimedElt') 
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


class speciesstructurespecies_reference(KeepInOrder):
	'''
	class speciesstructurespecies_reference
	Defines the source species represented in this element of the vector /distsource/source. Time-dependent

	Attributes:
	- type : class typestructureidentifier
	   The type species: type.flag=1 for electron source; type.flag=2 for ion source taken from compositions/ions; type.flag=3 for impurity source taken from compositions/impur; 4=neutron source; 4=photon source etc (see species_reference_identifier_definition in the Documentation website under Conventions/Enumerated_datatypes).
	- index : int
	   Index of the species. This definition of index depends on the value of type; if the species is an ion (type.flag=1) or an impurity (type.flag=2) then the index refers to distribution/compositions/ions, or distribution/compositions/impur, respectively. This field has no meaning for other species, e.g. like electrons, neutrons or photons. The indexing follows the Fortran/Matlab convention where the first element in an array has index 1. 
	'''

	def __init__(self, base_path_in='species'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.type = typestructureidentifier('type')
		self.index = EMPTY_INT

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class speciesstructurespecies_reference\n'
		ret = ret + space + 'Attribute type\n ' + self.type.__str__(depth+1)
		ret = ret + space + 'Attribute index: ' + str(self.index) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.type.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type speciesstructurespecies_reference, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.cpoTime = self.cpoTime
		self.type.putSlice(path, cpopath)
		status = ull.putIntSlice(self.idx, path, cpopath + 'index', self.index, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type speciesstructurespecies_reference, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.replaceLastSlice(path, cpopath)
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'index', self.index)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type speciesstructurespecies_reference, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type speciesstructurespecies_reference, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_index, retTime = ull.getIntSlice(self.idx, path, cpopath + 'index', inTime, interpolMode)
		check_status(status)
		if not status:
			self.index = ret_index
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type speciesstructurespecies_reference, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			typeList = self.type.build_non_resampled_data(path, cpopath, nbslice)
			status, indexList = ull.getVect1DInt(self.idx, path, cpopath + 'index')
			if len(indexList) == 0:
				indexList = numpy.resize(indexList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = speciesstructurespecies_reference(self.base_path)
				slice.setExpIdx(self.idx)
				slice.type = typeList[i]
				slice.index = int(indexList[i].copy())
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type speciesstructurespecies_referenceObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'index') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'index', i, self.index)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type speciesstructurespecies_referenceObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'index') 
			print ('obj = ' + str(obj))
		status, ret_index = ull.getIntFromObject(self.idx, obj, cpopath + 'index', i)
		check_status(status)
		if not status:
			self.index = ret_index

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type speciesstructurespecies_referenceObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.type.putNonTimedElt(path, cpopath + 'type', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type speciesstructurespecies_referenceObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.type.getNonTimedElt(path, cpopath + 'type', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'index')


class global_paramstructuredistsource_global_param(KeepInOrder):
	'''
	class global_paramstructuredistsource_global_param
	Global parameters.

	Attributes:
	- src_pow : class src_powstructureexp0D
	   Total power source [W]; Time-dependent.
	- src_rate : class src_ratestructureexp0D
	   Particle source rate [1/s]; Time-dependent.
	- mag_axis : class mag_axisstructurerz0D
	   Position of the magnetic axis. Time-dependent; Scalar
	- toroid_field : class toroid_fieldstructureb0r0
	   Characteristics of the vacuum toroidal field. Used to define the radial coordiante rho_tor. Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='global_param'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.src_pow = src_powstructureexp0D('src_pow')
		self.src_rate = src_ratestructureexp0D('src_rate')
		self.mag_axis = mag_axisstructurerz0D('mag_axis')
		self.toroid_field = toroid_fieldstructureb0r0('toroid_field')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class global_paramstructuredistsource_global_param\n'
		ret = ret + space + 'Attribute src_pow\n ' + self.src_pow.__str__(depth+1)
		ret = ret + space + 'Attribute src_rate\n ' + self.src_rate.__str__(depth+1)
		ret = ret + space + 'Attribute mag_axis\n ' + self.mag_axis.__str__(depth+1)
		ret = ret + space + 'Attribute toroid_field\n ' + self.toroid_field.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.src_pow.setExpIdx(idx)
		self.src_rate.setExpIdx(idx)
		self.mag_axis.setExpIdx(idx)
		self.toroid_field.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructuredistsource_global_param, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.src_pow.cpoTime = self.cpoTime
		self.src_pow.putSlice(path, cpopath)
		self.src_rate.cpoTime = self.cpoTime
		self.src_rate.putSlice(path, cpopath)
		self.mag_axis.cpoTime = self.cpoTime
		self.mag_axis.putSlice(path, cpopath)
		self.toroid_field.cpoTime = self.cpoTime
		self.toroid_field.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructuredistsource_global_param, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.src_pow.replaceLastSlice(path, cpopath)
		self.src_rate.replaceLastSlice(path, cpopath)
		self.mag_axis.replaceLastSlice(path, cpopath)
		self.toroid_field.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructuredistsource_global_param, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.src_pow.putNonTimed(path, cpopath)
		self.src_rate.putNonTimed(path, cpopath)
		self.mag_axis.putNonTimed(path, cpopath)
		self.toroid_field.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructuredistsource_global_param, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.src_pow.getSlice(path, cpopath, inTime, interpolMode)
		self.src_rate.getSlice(path, cpopath, inTime, interpolMode)
		self.mag_axis.getSlice(path, cpopath, inTime, interpolMode)
		self.toroid_field.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructuredistsource_global_param, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			src_powList = self.src_pow.build_non_resampled_data(path, cpopath, nbslice)
			src_rateList = self.src_rate.build_non_resampled_data(path, cpopath, nbslice)
			mag_axisList = self.mag_axis.build_non_resampled_data(path, cpopath, nbslice)
			toroid_fieldList = self.toroid_field.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = global_paramstructuredistsource_global_param(self.base_path)
				slice.setExpIdx(self.idx)
				slice.src_pow = src_powList[i]
				slice.src_rate = src_rateList[i]
				slice.mag_axis = mag_axisList[i]
				slice.toroid_field = toroid_fieldList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructuredistsource_global_paramObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.src_pow.putTimedElt(path, cpopath + 'src_pow', i, obj)
		obj = self.src_rate.putTimedElt(path, cpopath + 'src_rate', i, obj)
		obj = self.mag_axis.putTimedElt(path, cpopath + 'mag_axis', i, obj)
		obj = self.toroid_field.putTimedElt(path, cpopath + 'toroid_field', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructuredistsource_global_paramObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.src_pow.getTimedElt(path, cpopath + 'src_pow', i, obj)
		self.src_rate.getTimedElt(path, cpopath + 'src_rate', i, obj)
		self.mag_axis.getTimedElt(path, cpopath + 'mag_axis', i, obj)
		self.toroid_field.getTimedElt(path, cpopath + 'toroid_field', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructuredistsource_global_paramObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.src_pow.putNonTimedElt(path, cpopath + 'src_pow', i, obj)
		obj = self.src_rate.putNonTimedElt(path, cpopath + 'src_rate', i, obj)
		obj = self.mag_axis.putNonTimedElt(path, cpopath + 'mag_axis', i, obj)
		obj = self.toroid_field.putNonTimedElt(path, cpopath + 'toroid_field', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructuredistsource_global_paramObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.src_pow.getNonTimedElt(path, cpopath + 'src_pow', i, obj)
		self.src_rate.getNonTimedElt(path, cpopath + 'src_rate', i, obj)
		self.mag_axis.getNonTimedElt(path, cpopath + 'mag_axis', i, obj)
		self.toroid_field.getNonTimedElt(path, cpopath + 'toroid_field', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.src_pow.deleteData(path, cpopath)
		self.src_rate.deleteData(path, cpopath)
		self.mag_axis.deleteData(path, cpopath)
		self.toroid_field.deleteData(path, cpopath)


class src_powstructureexp0D(KeepInOrder):
	'''
	class src_powstructureexp0D
	Total power source [W]; Time-dependent.

	Attributes:
	- value : float
	   Signal value; Time-dependent; Scalar
	- abserror : float
	   Absolute error on signal; Time-dependent; Scalar
	- relerror : float
	   Relative error on signal (normalised to signal value); Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='src_pow'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = EMPTY_DOUBLE
		self.abserror = EMPTY_DOUBLE
		self.relerror = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class src_powstructureexp0D\n'
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		ret = ret + space + 'Attribute abserror: ' + str(self.abserror) + '\n'
		ret = ret + space + 'Attribute relerror: ' + str(self.relerror) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type src_powstructureexp0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type src_powstructureexp0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type src_powstructureexp0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type src_powstructureexp0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type src_powstructureexp0D, run function build_non_resampled_data') 
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
				slice = src_powstructureexp0D(self.base_path)
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
			print ('object of type src_powstructureexp0DObj, run function putTimedElt') 
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
			print ('object of type src_powstructureexp0DObj, run function getTimedElt') 
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
			print ('object of type src_powstructureexp0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type src_powstructureexp0DObj, run function getNonTimedElt') 
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


class src_ratestructureexp0D(KeepInOrder):
	'''
	class src_ratestructureexp0D
	Particle source rate [1/s]; Time-dependent.

	Attributes:
	- value : float
	   Signal value; Time-dependent; Scalar
	- abserror : float
	   Absolute error on signal; Time-dependent; Scalar
	- relerror : float
	   Relative error on signal (normalised to signal value); Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='src_rate'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = EMPTY_DOUBLE
		self.abserror = EMPTY_DOUBLE
		self.relerror = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class src_ratestructureexp0D\n'
		ret = ret + space + 'Attribute value: ' + str(self.value) + '\n'
		ret = ret + space + 'Attribute abserror: ' + str(self.abserror) + '\n'
		ret = ret + space + 'Attribute relerror: ' + str(self.relerror) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type src_ratestructureexp0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type src_ratestructureexp0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type src_ratestructureexp0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type src_ratestructureexp0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type src_ratestructureexp0D, run function build_non_resampled_data') 
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
				slice = src_ratestructureexp0D(self.base_path)
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
			print ('object of type src_ratestructureexp0DObj, run function putTimedElt') 
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
			print ('object of type src_ratestructureexp0DObj, run function getTimedElt') 
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
			print ('object of type src_ratestructureexp0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type src_ratestructureexp0DObj, run function getNonTimedElt') 
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
	Characteristics of the vacuum toroidal field. Used to define the radial coordiante rho_tor. Time-dependent; Scalar

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


class profiles_1dstructuredistsource_profiles_1d(KeepInOrder):
	'''
	class profiles_1dstructuredistsource_profiles_1d
	1D radial profiles

	Attributes:
	- rho_tor : numpy.ndarray 1D with float
	   Toroidal flux coordinate [m]. Defined as sqrt((phi-phi_axis)/pi/B0), where B0=../global_param/toroid_field/b0, phi is the toroidal flux and phi_axis is the toroidal flux at the magnetic axis. Time-dependent; Vector (npsi)
	- rho_tor_norm : numpy.ndarray 1D with float
	   The toroidal flux coordinate normalised to be zero at the axis and unity at the last closed flux surface, or last available fluxsurface if the last closed flux surface is not defined. Time-dependent; Vector (npsi)
	- psi : numpy.ndarray 1D with float
	   Poloidal flux [Wb], evaluated without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent; Vector (npsi)
	- volume : numpy.ndarray 1D with float
	   Volume enclosed by the flux surface [m^3]. Time-dependent; Vector (npsi)
	- area : numpy.ndarray 1D with float
	   Cross-sectional area of the flux surface [m^2]. Time-dependent; Vector (npsi)
	- pow_den : class pow_denstructureexp1D
	   Flux surface averaged power density [W/m^3]; Time-dependent; Vector (npsi)
	- trq_den : class trq_denstructureexp1D
	   Flux surface averaged toroidal torque density [N/m^2]; Time-dependent; Vector (npsi)
	- src_rate : class src_ratestructureexp1D
	   Flux surface averaged total source density of particles [m^-3 s^-1]; Time-dependent; Vector (npsi)
	'''

	def __init__(self, base_path_in='profiles_1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.rho_tor = numpy.zeros(0, numpy.float64, order='C')
		self.rho_tor_norm = numpy.zeros(0, numpy.float64, order='C')
		self.psi = numpy.zeros(0, numpy.float64, order='C')
		self.volume = numpy.zeros(0, numpy.float64, order='C')
		self.area = numpy.zeros(0, numpy.float64, order='C')
		self.pow_den = pow_denstructureexp1D('pow_den')
		self.trq_den = trq_denstructureexp1D('trq_den')
		self.src_rate = src_ratestructureexp1D('src_rate')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class profiles_1dstructuredistsource_profiles_1d\n'
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
		ret = ret + space + 'Attribute pow_den\n ' + self.pow_den.__str__(depth+1)
		ret = ret + space + 'Attribute trq_den\n ' + self.trq_den.__str__(depth+1)
		ret = ret + space + 'Attribute src_rate\n ' + self.src_rate.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.pow_den.setExpIdx(idx)
		self.trq_den.setExpIdx(idx)
		self.src_rate.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructuredistsource_profiles_1d, run function putSlice') 
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
		self.pow_den.cpoTime = self.cpoTime
		self.pow_den.putSlice(path, cpopath)
		self.trq_den.cpoTime = self.cpoTime
		self.trq_den.putSlice(path, cpopath)
		self.src_rate.cpoTime = self.cpoTime
		self.src_rate.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructuredistsource_profiles_1d, run function replaceLastSlice') 
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
		self.pow_den.replaceLastSlice(path, cpopath)
		self.trq_den.replaceLastSlice(path, cpopath)
		self.src_rate.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructuredistsource_profiles_1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.pow_den.putNonTimed(path, cpopath)
		self.trq_den.putNonTimed(path, cpopath)
		self.src_rate.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructuredistsource_profiles_1d, run function getSlice') 
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
		self.pow_den.getSlice(path, cpopath, inTime, interpolMode)
		self.trq_den.getSlice(path, cpopath, inTime, interpolMode)
		self.src_rate.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructuredistsource_profiles_1d, run function build_non_resampled_data') 
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
			pow_denList = self.pow_den.build_non_resampled_data(path, cpopath, nbslice)
			trq_denList = self.trq_den.build_non_resampled_data(path, cpopath, nbslice)
			src_rateList = self.src_rate.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = profiles_1dstructuredistsource_profiles_1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.rho_tor = rho_torList[:,i]
				slice.rho_tor_norm = rho_tor_normList[:,i]
				slice.psi = psiList[:,i]
				slice.volume = volumeList[:,i]
				slice.area = areaList[:,i]
				slice.pow_den = pow_denList[i]
				slice.trq_den = trq_denList[i]
				slice.src_rate = src_rateList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructuredistsource_profiles_1dObj, run function putTimedElt') 
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
		obj = self.pow_den.putTimedElt(path, cpopath + 'pow_den', i, obj)
		obj = self.trq_den.putTimedElt(path, cpopath + 'trq_den', i, obj)
		obj = self.src_rate.putTimedElt(path, cpopath + 'src_rate', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructuredistsource_profiles_1dObj, run function getTimedElt') 
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
		self.pow_den.getTimedElt(path, cpopath + 'pow_den', i, obj)
		self.trq_den.getTimedElt(path, cpopath + 'trq_den', i, obj)
		self.src_rate.getTimedElt(path, cpopath + 'src_rate', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructuredistsource_profiles_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.pow_den.putNonTimedElt(path, cpopath + 'pow_den', i, obj)
		obj = self.trq_den.putNonTimedElt(path, cpopath + 'trq_den', i, obj)
		obj = self.src_rate.putNonTimedElt(path, cpopath + 'src_rate', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructuredistsource_profiles_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.pow_den.getNonTimedElt(path, cpopath + 'pow_den', i, obj)
		self.trq_den.getNonTimedElt(path, cpopath + 'trq_den', i, obj)
		self.src_rate.getNonTimedElt(path, cpopath + 'src_rate', i, obj)

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
		self.pow_den.deleteData(path, cpopath)
		self.trq_den.deleteData(path, cpopath)
		self.src_rate.deleteData(path, cpopath)


class pow_denstructureexp1D(KeepInOrder):
	'''
	class pow_denstructureexp1D
	Flux surface averaged power density [W/m^3]; Time-dependent; Vector (npsi)

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='pow_den'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pow_denstructureexp1D\n'
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
			print ('field '+self.base_path+' of type pow_denstructureexp1D, run function putSlice') 
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
			print ('field '+self.base_path+' of type pow_denstructureexp1D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type pow_denstructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pow_denstructureexp1D, run function getSlice') 
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
			print ('field '+self.base_path+' of type pow_denstructureexp1D, run function build_non_resampled_data') 
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
				slice = pow_denstructureexp1D(self.base_path)
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
			print ('object of type pow_denstructureexp1DObj, run function putTimedElt') 
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
			print ('object of type pow_denstructureexp1DObj, run function getTimedElt') 
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
			print ('object of type pow_denstructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_denstructureexp1DObj, run function getNonTimedElt') 
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


class trq_denstructureexp1D(KeepInOrder):
	'''
	class trq_denstructureexp1D
	Flux surface averaged toroidal torque density [N/m^2]; Time-dependent; Vector (npsi)

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='trq_den'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class trq_denstructureexp1D\n'
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
			print ('field '+self.base_path+' of type trq_denstructureexp1D, run function putSlice') 
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
			print ('field '+self.base_path+' of type trq_denstructureexp1D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type trq_denstructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type trq_denstructureexp1D, run function getSlice') 
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
			print ('field '+self.base_path+' of type trq_denstructureexp1D, run function build_non_resampled_data') 
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
				slice = trq_denstructureexp1D(self.base_path)
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
			print ('object of type trq_denstructureexp1DObj, run function putTimedElt') 
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
			print ('object of type trq_denstructureexp1DObj, run function getTimedElt') 
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
			print ('object of type trq_denstructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trq_denstructureexp1DObj, run function getNonTimedElt') 
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


class src_ratestructureexp1D(KeepInOrder):
	'''
	class src_ratestructureexp1D
	Flux surface averaged total source density of particles [m^-3 s^-1]; Time-dependent; Vector (npsi)

	Attributes:
	- value : numpy.ndarray 1D with float
	   Signal value; Time-dependent; Vector
	- abserror : numpy.ndarray 1D with float
	   Absolute error on signal; Time-dependent; Vector
	- relerror : numpy.ndarray 1D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Vector
	'''

	def __init__(self, base_path_in='src_rate'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros(0, numpy.float64, order='C')
		self.abserror = numpy.zeros(0, numpy.float64, order='C')
		self.relerror = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class src_ratestructureexp1D\n'
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
			print ('field '+self.base_path+' of type src_ratestructureexp1D, run function putSlice') 
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
			print ('field '+self.base_path+' of type src_ratestructureexp1D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type src_ratestructureexp1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type src_ratestructureexp1D, run function getSlice') 
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
			print ('field '+self.base_path+' of type src_ratestructureexp1D, run function build_non_resampled_data') 
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
				slice = src_ratestructureexp1D(self.base_path)
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
			print ('object of type src_ratestructureexp1DObj, run function putTimedElt') 
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
			print ('object of type src_ratestructureexp1DObj, run function getTimedElt') 
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
			print ('object of type src_ratestructureexp1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type src_ratestructureexp1DObj, run function getNonTimedElt') 
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


class profiles_2dstructuredistsource_profiles_2d(KeepInOrder):
	'''
	class profiles_2dstructuredistsource_profiles_2d
	2D source profiles in terms of two phase space coordinates

	Attributes:
	- grid_coord : numpy.ndarray 1D with int)
	   Identifies the coordinates specifies in dim1 and dim2. grid_coord(1) and grid_coord(2) describe the coordinate represented in dim1 and dim2. The possible coordinates are: 1=R, Major radius [m]; 2=Z, Vertical position [m]; 3=X, first cartesian coordinate in the horizontal plane [m]; 4=Y, second cartesian coordinate in the horizontal plane (grad(X) x grad(Y) = grad(Z)) [m]; 5=phi, toroidal angle [rad]; 6=psi, poloidal magnetic flux [T*m^2]; 7=rhotor, the square root of the toroidal flux; 8=theta, geometrical poloidal angle [rad]; 9=theta_b, Boozer poloidal angle [rad]; 10=vx, velocity in the x-direction [m/s]; 11=vy, velocity in the y-direction [m/s]; 12=vz, velocity in the z-direction [m/s]; 13=vel, total velocity [m/s]; 14=vphi, velocity in the phi-direction [m/s]; 15=vpar, velocity in the parallel direction [m/s]; 16=vperp, velocity in the perpendicular direction [m/s]; 17=E, Hamiltonian energy [J]; 18=Pphi, canonical toroidal angular momentum [kg m^2/s]; 19=mu, magnetic moment [J/T]; 20=Lambda=mu/E [1/T]. Vector (2)
	- dim1 : numpy.ndarray 2D with float
	   First coordinate of 2D grid. Time-dependent; Vector (ndim1,ndim2)
	- dim2 : numpy.ndarray 2D with float
	   Second coordinate of 2D grid. Time-dependent; Vector (ndim1,ndim2)
	- g11 : numpy.ndarray 2D with float
	   11 component of the covariant metric tensor in the (dim1, dim2) coordiante system. Time-dependent; Vector (ndim1,ndim2)
	- g12 : numpy.ndarray 2D with float
	   12 component of the covariant metric tensor in the (dim1, dim2) coordiante system. Time-dependent; Vector (ndim1,ndim2)
	- g21 : numpy.ndarray 2D with float
	   21 component of the covariant metric tensor in the (dim1, dim2) coordiante system. Time-dependent; Vector (ndim1,ndim2)
	- g22 : numpy.ndarray 2D with float
	   22 component of the covariant metric tensor in the (dim1, dim2) coordiante system. Time-dependent; Vector (ndim1,ndim2)
	- pow_den : class pow_denstructureexp2D
	   Source power density. Here sum(M,N=1,2; pow_den*gNM*dimN*dimM) have unit [W]. Time-dependent; Vector (ndim1,ndim2)
	- src_rate : class src_ratestructureexp2D
	   Source density of particles.Here sum(M,N=1,2; src_rate*gNM*dimN*dimM) have unit [1/s]. Time-dependent; Vector (ndim1,ndim2)
	'''

	def __init__(self, base_path_in='profiles_2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.grid_coord = numpy.zeros(0, numpy.int32, order='C')
		self.dim1 = numpy.zeros((0,0), numpy.float64, order='C')
		self.dim2 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g11 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g12 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g21 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g22 = numpy.zeros((0,0), numpy.float64, order='C')
		self.pow_den = pow_denstructureexp2D('pow_den')
		self.src_rate = src_ratestructureexp2D('src_rate')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class profiles_2dstructuredistsource_profiles_2d\n'
		s = self.grid_coord.__str__()
		ret = ret + space + 'Attribute grid_coord\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim1.__str__()
		ret = ret + space + 'Attribute dim1\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim2.__str__()
		ret = ret + space + 'Attribute dim2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g11.__str__()
		ret = ret + space + 'Attribute g11\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g12.__str__()
		ret = ret + space + 'Attribute g12\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g21.__str__()
		ret = ret + space + 'Attribute g21\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g22.__str__()
		ret = ret + space + 'Attribute g22\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute pow_den\n ' + self.pow_den.__str__(depth+1)
		ret = ret + space + 'Attribute src_rate\n ' + self.src_rate.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.pow_den.setExpIdx(idx)
		self.src_rate.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructuredistsource_profiles_2d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'dim1', numpy.array(self.dim1).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'dim2', numpy.array(self.dim2).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g11', numpy.array(self.g11).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g12', numpy.array(self.g12).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g21', numpy.array(self.g21).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g22', numpy.array(self.g22).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.pow_den.cpoTime = self.cpoTime
		self.pow_den.putSlice(path, cpopath)
		self.src_rate.cpoTime = self.cpoTime
		self.src_rate.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructuredistsource_profiles_2d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'dim1', numpy.array(self.dim1).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'dim2', numpy.array(self.dim2).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g11', numpy.array(self.g11).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g12', numpy.array(self.g12).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g21', numpy.array(self.g21).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g22', numpy.array(self.g22).astype(numpy.float64))
		check_status(status)
		self.pow_den.replaceLastSlice(path, cpopath)
		self.src_rate.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructuredistsource_profiles_2d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DInt(self.idx, path, cpopath + 'grid_coord', numpy.array(self.grid_coord).astype(numpy.int32), False)
		check_status(status)
		self.pow_den.putNonTimed(path, cpopath)
		self.src_rate.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructuredistsource_profiles_2d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_grid_coord = ull.getVect1DInt(self.idx, path, cpopath + 'grid_coord')
		check_status(status)
		if not status:
			self.grid_coord = ret_grid_coord
		status, ret_dim1, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'dim1', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dim1 = ret_dim1
			self.cpoTime = retTime
		status, ret_dim2, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'dim2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dim2 = ret_dim2
			self.cpoTime = retTime
		status, ret_g11, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g11', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g11 = ret_g11
			self.cpoTime = retTime
		status, ret_g12, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g12', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g12 = ret_g12
			self.cpoTime = retTime
		status, ret_g21, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g21', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g21 = ret_g21
			self.cpoTime = retTime
		status, ret_g22, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g22', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g22 = ret_g22
			self.cpoTime = retTime
		self.pow_den.getSlice(path, cpopath, inTime, interpolMode)
		self.src_rate.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructuredistsource_profiles_2d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, grid_coordVal = ull.getVect1DInt(self.idx, path, cpopath + 'grid_coord')
			check_status(status)
			status, dim1List = ull.getVect3DDouble(self.idx, path, cpopath + 'dim1')
			if len(dim1List) == 0:
				dim1List = numpy.resize(dim1List, (0,0,nbslice))
			check_status(status)
			status, dim2List = ull.getVect3DDouble(self.idx, path, cpopath + 'dim2')
			if len(dim2List) == 0:
				dim2List = numpy.resize(dim2List, (0,0,nbslice))
			check_status(status)
			status, g11List = ull.getVect3DDouble(self.idx, path, cpopath + 'g11')
			if len(g11List) == 0:
				g11List = numpy.resize(g11List, (0,0,nbslice))
			check_status(status)
			status, g12List = ull.getVect3DDouble(self.idx, path, cpopath + 'g12')
			if len(g12List) == 0:
				g12List = numpy.resize(g12List, (0,0,nbslice))
			check_status(status)
			status, g21List = ull.getVect3DDouble(self.idx, path, cpopath + 'g21')
			if len(g21List) == 0:
				g21List = numpy.resize(g21List, (0,0,nbslice))
			check_status(status)
			status, g22List = ull.getVect3DDouble(self.idx, path, cpopath + 'g22')
			if len(g22List) == 0:
				g22List = numpy.resize(g22List, (0,0,nbslice))
			check_status(status)
			pow_denList = self.pow_den.build_non_resampled_data(path, cpopath, nbslice)
			src_rateList = self.src_rate.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = profiles_2dstructuredistsource_profiles_2d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.grid_coord = grid_coordVal
				slice.dim1 = dim1List[:,:,i]
				slice.dim2 = dim2List[:,:,i]
				slice.g11 = g11List[:,:,i]
				slice.g12 = g12List[:,:,i]
				slice.g21 = g21List[:,:,i]
				slice.g22 = g22List[:,:,i]
				slice.pow_den = pow_denList[i]
				slice.src_rate = src_rateList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstructuredistsource_profiles_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'dim1') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'dim1', i, numpy.array(self.dim1).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'dim2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'dim2', i, numpy.array(self.dim2).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g11') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g11', i, numpy.array(self.g11).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g12') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g12', i, numpy.array(self.g12).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g21') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g21', i, numpy.array(self.g21).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g22') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g22', i, numpy.array(self.g22).astype(numpy.float64))
		obj = self.pow_den.putTimedElt(path, cpopath + 'pow_den', i, obj)
		obj = self.src_rate.putTimedElt(path, cpopath + 'src_rate', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstructuredistsource_profiles_2dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'dim1') 
			print ('obj = ' + str(obj))
		status, ret_dim1 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'dim1', i)
		check_status(status)
		if not status:
			self.dim1 = ret_dim1
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'dim2') 
			print ('obj = ' + str(obj))
		status, ret_dim2 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'dim2', i)
		check_status(status)
		if not status:
			self.dim2 = ret_dim2
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g11') 
			print ('obj = ' + str(obj))
		status, ret_g11 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g11', i)
		check_status(status)
		if not status:
			self.g11 = ret_g11
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g12') 
			print ('obj = ' + str(obj))
		status, ret_g12 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g12', i)
		check_status(status)
		if not status:
			self.g12 = ret_g12
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g21') 
			print ('obj = ' + str(obj))
		status, ret_g21 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g21', i)
		check_status(status)
		if not status:
			self.g21 = ret_g21
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g22') 
			print ('obj = ' + str(obj))
		status, ret_g22 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g22', i)
		check_status(status)
		if not status:
			self.g22 = ret_g22
		self.pow_den.getTimedElt(path, cpopath + 'pow_den', i, obj)
		self.src_rate.getTimedElt(path, cpopath + 'src_rate', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstructuredistsource_profiles_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'grid_coord') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'grid_coord', i, numpy.array(self.grid_coord).astype(numpy.int32))
		obj = self.pow_den.putNonTimedElt(path, cpopath + 'pow_den', i, obj)
		obj = self.src_rate.putNonTimedElt(path, cpopath + 'src_rate', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstructuredistsource_profiles_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'grid_coord') 
			print ('obj = ' + str(obj))
		status, ret_grid_coord = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'grid_coord', i)
		check_status(status)
		if not status:
			self.grid_coord = ret_grid_coord
		self.pow_den.getNonTimedElt(path, cpopath + 'pow_den', i, obj)
		self.src_rate.getNonTimedElt(path, cpopath + 'src_rate', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'grid_coord')
		ull.deleteData(self.idx, path, cpopath + 'dim1')
		ull.deleteData(self.idx, path, cpopath + 'dim2')
		ull.deleteData(self.idx, path, cpopath + 'g11')
		ull.deleteData(self.idx, path, cpopath + 'g12')
		ull.deleteData(self.idx, path, cpopath + 'g21')
		ull.deleteData(self.idx, path, cpopath + 'g22')
		self.pow_den.deleteData(path, cpopath)
		self.src_rate.deleteData(path, cpopath)


class pow_denstructureexp2D(KeepInOrder):
	'''
	class pow_denstructureexp2D
	Source power density. Here sum(M,N=1,2; pow_den*gNM*dimN*dimM) have unit [W]. Time-dependent; Vector (ndim1,ndim2)

	Attributes:
	- value : numpy.ndarray 2D with float
	   Signal value; Time-dependent; Matrix
	- abserror : numpy.ndarray 2D with float
	   Absolute error on signal; Time-dependent; Matrix
	- relerror : numpy.ndarray 2D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Matrix
	'''

	def __init__(self, base_path_in='pow_den'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros((0,0), numpy.float64, order='C')
		self.abserror = numpy.zeros((0,0), numpy.float64, order='C')
		self.relerror = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pow_denstructureexp2D\n'
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
			print ('field '+self.base_path+' of type pow_denstructureexp2D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pow_denstructureexp2D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pow_denstructureexp2D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pow_denstructureexp2D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_abserror, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pow_denstructureexp2D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,0,nbslice))
			check_status(status)
			status, abserrorList = ull.getVect3DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (0,0,nbslice))
			check_status(status)
			status, relerrorList = ull.getVect3DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = pow_denstructureexp2D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,:,i]
				slice.abserror = abserrorList[:,:,i]
				slice.relerror = relerrorList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_denstructureexp2DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'abserror', i, numpy.array(self.abserror).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'relerror', i, numpy.array(self.relerror).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_denstructureexp2DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_denstructureexp2DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pow_denstructureexp2DObj, run function getNonTimedElt') 
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


class src_ratestructureexp2D(KeepInOrder):
	'''
	class src_ratestructureexp2D
	Source density of particles.Here sum(M,N=1,2; src_rate*gNM*dimN*dimM) have unit [1/s]. Time-dependent; Vector (ndim1,ndim2)

	Attributes:
	- value : numpy.ndarray 2D with float
	   Signal value; Time-dependent; Matrix
	- abserror : numpy.ndarray 2D with float
	   Absolute error on signal; Time-dependent; Matrix
	- relerror : numpy.ndarray 2D with float
	   Relative error on signal (normalised to signal value); Time-dependent; Matrix
	'''

	def __init__(self, base_path_in='src_rate'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = numpy.zeros((0,0), numpy.float64, order='C')
		self.abserror = numpy.zeros((0,0), numpy.float64, order='C')
		self.relerror = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class src_ratestructureexp2D\n'
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
			print ('field '+self.base_path+' of type src_ratestructureexp2D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type src_ratestructureexp2D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'value', numpy.array(self.value).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'abserror', numpy.array(self.abserror).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'relerror', numpy.array(self.relerror).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type src_ratestructureexp2D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type src_ratestructureexp2D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_value, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'value', inTime, interpolMode)
		check_status(status)
		if not status:
			self.value = ret_value
			self.cpoTime = retTime
		status, ret_abserror, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'abserror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
			self.cpoTime = retTime
		status, ret_relerror, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'relerror', inTime, interpolMode)
		check_status(status)
		if not status:
			self.relerror = ret_relerror
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type src_ratestructureexp2D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, valueList = ull.getVect3DDouble(self.idx, path, cpopath + 'value')
			if len(valueList) == 0:
				valueList = numpy.resize(valueList, (0,0,nbslice))
			check_status(status)
			status, abserrorList = ull.getVect3DDouble(self.idx, path, cpopath + 'abserror')
			if len(abserrorList) == 0:
				abserrorList = numpy.resize(abserrorList, (0,0,nbslice))
			check_status(status)
			status, relerrorList = ull.getVect3DDouble(self.idx, path, cpopath + 'relerror')
			if len(relerrorList) == 0:
				relerrorList = numpy.resize(relerrorList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = src_ratestructureexp2D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[:,:,i]
				slice.abserror = abserrorList[:,:,i]
				slice.relerror = relerrorList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type src_ratestructureexp2DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'value', i, numpy.array(self.value).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'abserror', i, numpy.array(self.abserror).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'relerror', i, numpy.array(self.relerror).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type src_ratestructureexp2DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'value') 
			print ('obj = ' + str(obj))
		status, ret_value = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'value', i)
		check_status(status)
		if not status:
			self.value = ret_value
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'abserror') 
			print ('obj = ' + str(obj))
		status, ret_abserror = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'abserror', i)
		check_status(status)
		if not status:
			self.abserror = ret_abserror
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'relerror') 
			print ('obj = ' + str(obj))
		status, ret_relerror = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'relerror', i)
		check_status(status)
		if not status:
			self.relerror = ret_relerror

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type src_ratestructureexp2DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type src_ratestructureexp2DObj, run function getNonTimedElt') 
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


class line_srcprofstruct_arraydistsource_line_src_prof:
	'''
	class line_srcprofstruct_arraydistsource_line_src_prof
	1D profiles representation of a line source. Time-dependent

	Attributes:
	- array : list of line_srcprofstruct_arraydistsource_line_src_profObj 
	'''

	def __init__(self, base_path_in='line_srcprof'):
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
		ret = space + 'class line_srcprofstruct_arraydistsource_line_src_prof\n'
		for i in range(len(self.array)):
			ret = ret + space + 'line_srcprofstruct_arraydistsource_line_src_prof[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(line_srcprofstruct_arraydistsource_line_src_profObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type line_srcprofstruct_arraydistsource_line_src_prof, run function putSlice') 
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
			print ('field '+self.base_path+' of type line_srcprofstruct_arraydistsource_line_src_prof, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type line_srcprofstruct_arraydistsource_line_src_prof, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type line_srcprofstruct_arraydistsource_line_src_prof, run function getSlice') 
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
			print ('field '+self.base_path+' of type line_srcprofstruct_arraydistsource_line_src_prof, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(line_srcprofstruct_arraydistsource_line_src_prof(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(line_srcprofstruct_arraydistsource_line_src_prof(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = line_srcprofstruct_arraydistsource_line_src_prof(self.base_path)
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
			print ('field '+self.base_path+' of type line_srcprofstruct_arraydistsource_line_src_prof, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type line_srcprofstruct_arraydistsource_line_src_prof, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type line_srcprofstruct_arraydistsource_line_src_prof, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type line_srcprofstruct_arraydistsource_line_src_prof, run function getNonTimedElt') 
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


class line_srcprofstruct_arraydistsource_line_src_profObj(KeepInOrder):
	'''
	class line_srcprofstruct_arraydistsource_line_src_profObj
	1D profiles representation of a line source. Time-dependent

	Attributes:
	- rho_tor : numpy.ndarray 1D with float
	   Toroidal flux coordinate at the grid points for 1D profiles [m]. Defined as sqrt(phi/pi/B0), where B0 = equilibrium/global_param/toroid_field/b0. Time-dependent; Vector (npsi)
	- rho_tor_norm : numpy.ndarray 1D with float
	   Normalised toroidal flux coordinate at the grid points for 1D profiles; Time-dependent; Vector (npsi)
	- psi : numpy.ndarray 1D with float
	   Poloidal flux at the grid points for 1D profiles [Wb], without 1/2pi and such that Bp = |grad psi| / (R 2 pi). Time-dependent; Vector (npsi)
	- R : numpy.ndarray 1D with float
	   Major radius at the line source. Time-dependent; Vector (npsi)
	- Z : numpy.ndarray 1D with float
	   Vertical position of the line source. Time-dependent; Vector (npsi)
	- theta : numpy.ndarray 1D with float
	   Polidal angle [rad]. Time-dependent; Vector (npsi)
	- theta_id : numpy.ndarray 1D with float
	   Type of poloidal angle: 1 : same as the poloidal angle in the equlibrium cpo; 2 : geometrical polar angle, tan(theta) = Z/(R-R_0); 3 : other. If option 3, a transformation to the geometrical poloidal angle is provided in th2th_pol.
	- th2th_pol : numpy.ndarray 2D with float
	   Geometrical poloidal angle at grid points in theta, i.e. the transformation from theta to the polar poloidal angle; used only if angl_type=3; Time-dependent; Matrix (ndim1, ndim2)
	- pitch : numpy.ndarray 1D with float
	   Pitch (i.e. v_parallel/v) of source particles. Time-dependent; Vector (npsi)
	- energy : numpy.ndarray 1D with float
	   Kinetic energy of source particles [eV]. Time-dependent; Vector (npsi)
	- ang_momentum : numpy.ndarray 1D with float
	   Kinetic angular momentum of a single source particles, R m v_phi [Nms]. Time-dependent; Vector (npsi)
	- src_rate : numpy.ndarray 1D with float
	   Source density of particles [1/m^3/s]. Time-dependent; Vector (npsi)
	'''

	def __init__(self, base_path_in='line_srcprof'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.rho_tor = numpy.zeros(0, numpy.float64, order='C')
		self.rho_tor_norm = numpy.zeros(0, numpy.float64, order='C')
		self.psi = numpy.zeros(0, numpy.float64, order='C')
		self.R = numpy.zeros(0, numpy.float64, order='C')
		self.Z = numpy.zeros(0, numpy.float64, order='C')
		self.theta = numpy.zeros(0, numpy.float64, order='C')
		self.theta_id = numpy.zeros(0, numpy.float64, order='C')
		self.th2th_pol = numpy.zeros((0,0), numpy.float64, order='C')
		self.pitch = numpy.zeros(0, numpy.float64, order='C')
		self.energy = numpy.zeros(0, numpy.float64, order='C')
		self.ang_momentum = numpy.zeros(0, numpy.float64, order='C')
		self.src_rate = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class line_srcprofstruct_arraydistsource_line_src_profObj\n'
		s = self.rho_tor.__str__()
		ret = ret + space + 'Attribute rho_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_tor_norm.__str__()
		ret = ret + space + 'Attribute rho_tor_norm\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.R.__str__()
		ret = ret + space + 'Attribute R\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.Z.__str__()
		ret = ret + space + 'Attribute Z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta.__str__()
		ret = ret + space + 'Attribute theta\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta_id.__str__()
		ret = ret + space + 'Attribute theta_id\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.th2th_pol.__str__()
		ret = ret + space + 'Attribute th2th_pol\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pitch.__str__()
		ret = ret + space + 'Attribute pitch\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.energy.__str__()
		ret = ret + space + 'Attribute energy\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ang_momentum.__str__()
		ret = ret + space + 'Attribute ang_momentum\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.src_rate.__str__()
		ret = ret + space + 'Attribute src_rate\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type line_srcprofstruct_arraydistsource_line_src_profObj, run function putTimedElt') 
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
			print ('putVect1DDoubleInObject : ' + cpopath + 'R') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'R', i, numpy.array(self.R).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'Z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'Z', i, numpy.array(self.Z).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'theta', i, numpy.array(self.theta).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'th2th_pol') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'th2th_pol', i, numpy.array(self.th2th_pol).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pitch') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pitch', i, numpy.array(self.pitch).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'energy', i, numpy.array(self.energy).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ang_momentum') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ang_momentum', i, numpy.array(self.ang_momentum).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'src_rate') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'src_rate', i, numpy.array(self.src_rate).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type line_srcprofstruct_arraydistsource_line_src_profObj, run function getTimedElt') 
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
			print ('getVect1DDoubleInObject : ' + cpopath + 'R') 
			print ('obj = ' + str(obj))
		status, ret_R = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'R', i)
		check_status(status)
		if not status:
			self.R = ret_R
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'Z') 
			print ('obj = ' + str(obj))
		status, ret_Z = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'Z', i)
		check_status(status)
		if not status:
			self.Z = ret_Z
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		status, ret_theta = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'theta', i)
		check_status(status)
		if not status:
			self.theta = ret_theta
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'th2th_pol') 
			print ('obj = ' + str(obj))
		status, ret_th2th_pol = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'th2th_pol', i)
		check_status(status)
		if not status:
			self.th2th_pol = ret_th2th_pol
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pitch') 
			print ('obj = ' + str(obj))
		status, ret_pitch = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pitch', i)
		check_status(status)
		if not status:
			self.pitch = ret_pitch
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		status, ret_energy = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'energy', i)
		check_status(status)
		if not status:
			self.energy = ret_energy
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ang_momentum') 
			print ('obj = ' + str(obj))
		status, ret_ang_momentum = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ang_momentum', i)
		check_status(status)
		if not status:
			self.ang_momentum = ret_ang_momentum
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'src_rate') 
			print ('obj = ' + str(obj))
		status, ret_src_rate = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'src_rate', i)
		check_status(status)
		if not status:
			self.src_rate = ret_src_rate

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type line_srcprofstruct_arraydistsource_line_src_profObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'theta_id') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'theta_id', i, numpy.array(self.theta_id).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type line_srcprofstruct_arraydistsource_line_src_profObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'theta_id') 
			print ('obj = ' + str(obj))
		status, ret_theta_id = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'theta_id', i)
		check_status(status)
		if not status:
			self.theta_id = ret_theta_id


class source_ratestructuresource_rate(KeepInOrder):
	'''
	class source_ratestructuresource_rate
	Source density of particles in phase space (real space, velocity space, spin state).

	Attributes:
	- grid : class gridstructurecomplexgrid
	   Grid for storing the source-rate. Time-dependent; Complexgrid
	- value : class valuestructurecomplexgrid_scalar
	   The source-rate of particles in phase space; given on grid [(m/s)^-3 m^-3 s^-1]. Time-dependent; Complexgrid_scalar
	- discrete : numpy.ndarray 1D with int)
	   List of indexes for the dimensions (coordinates) of grid for which the source is discretely distributed. For example consider a source of 3.5 MeV alpha particles provided on a grid  with two coordinates; rho_tor and energy. To specify that the source is given at energies exactly equal to 3.5 MeV, let discret have length 1 and set discrete=(1)=2 since energy is dimension number 2. The source is then proportional to delta( 1 - energy / 3.5MeV ), where delta is the Direct delta distribution. Discrete dimensions can only be used when the grid is rectangular. Time-dependent; Vector(n_discrete_dimensions)
	- parameters : class parametersstructureparameters
	   Parameters used to defined the grid coordiantes. Time-dependent
	'''

	def __init__(self, base_path_in='source_rate'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.grid = gridstructurecomplexgrid('grid')
		self.value = valuestructurecomplexgrid_scalar('value')
		self.discrete = numpy.zeros(0, numpy.int32, order='C')
		self.parameters = parametersstructureparameters('parameters')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class source_ratestructuresource_rate\n'
		ret = ret + space + 'Attribute grid\n ' + self.grid.__str__(depth+1)
		ret = ret + space + 'Attribute value\n ' + self.value.__str__(depth+1)
		s = self.discrete.__str__()
		ret = ret + space + 'Attribute discrete\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute parameters\n ' + self.parameters.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.grid.setExpIdx(idx)
		self.value.setExpIdx(idx)
		self.parameters.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_ratestructuresource_rate, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.cpoTime = self.cpoTime
		self.grid.putSlice(path, cpopath)
		self.value.cpoTime = self.cpoTime
		self.value.putSlice(path, cpopath)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'discrete', numpy.array(self.discrete).astype(numpy.int32), self.cpoTime)
		check_status(status)
		self.parameters.cpoTime = self.cpoTime
		self.parameters.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_ratestructuresource_rate, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.replaceLastSlice(path, cpopath)
		self.value.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'discrete', numpy.array(self.discrete).astype(numpy.int32))
		check_status(status)
		self.parameters.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_ratestructuresource_rate, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.putNonTimed(path, cpopath)
		self.value.putNonTimed(path, cpopath)
		self.parameters.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type source_ratestructuresource_rate, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.getSlice(path, cpopath, inTime, interpolMode)
		self.value.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_discrete, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'discrete', inTime, interpolMode)
		check_status(status)
		if not status:
			self.discrete = ret_discrete
			self.cpoTime = retTime
		self.parameters.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type source_ratestructuresource_rate, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			gridList = self.grid.build_non_resampled_data(path, cpopath, nbslice)
			valueList = self.value.build_non_resampled_data(path, cpopath, nbslice)
			status, discreteList = ull.getVect2DInt(self.idx, path, cpopath + 'discrete')
			if len(discreteList) == 0:
				discreteList = numpy.resize(discreteList, (0,nbslice))
			check_status(status)
			parametersList = self.parameters.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = source_ratestructuresource_rate(self.base_path)
				slice.setExpIdx(self.idx)
				slice.grid = gridList[i]
				slice.value = valueList[i]
				slice.discrete = discreteList[:,i]
				slice.parameters = parametersList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_ratestructuresource_rateObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.grid.putTimedElt(path, cpopath + 'grid', i, obj)
		obj = self.value.putTimedElt(path, cpopath + 'value', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'discrete') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'discrete', i, numpy.array(self.discrete).astype(numpy.int32))
		obj = self.parameters.putTimedElt(path, cpopath + 'parameters', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_ratestructuresource_rateObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.grid.getTimedElt(path, cpopath + 'grid', i, obj)
		self.value.getTimedElt(path, cpopath + 'value', i, obj)
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'discrete') 
			print ('obj = ' + str(obj))
		status, ret_discrete = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'discrete', i)
		check_status(status)
		if not status:
			self.discrete = ret_discrete
		self.parameters.getTimedElt(path, cpopath + 'parameters', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_ratestructuresource_rateObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.grid.putNonTimedElt(path, cpopath + 'grid', i, obj)
		obj = self.value.putNonTimedElt(path, cpopath + 'value', i, obj)
		obj = self.parameters.putNonTimedElt(path, cpopath + 'parameters', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_ratestructuresource_rateObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.grid.getNonTimedElt(path, cpopath + 'grid', i, obj)
		self.value.getNonTimedElt(path, cpopath + 'value', i, obj)
		self.parameters.getNonTimedElt(path, cpopath + 'parameters', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.deleteData(path, cpopath)
		self.value.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'discrete')
		self.parameters.deleteData(path, cpopath)


class gridstructurecomplexgrid(KeepInOrder):
	'''
	class gridstructurecomplexgrid
	Grid for storing the source-rate. Time-dependent; Complexgrid

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


class valuestructurecomplexgrid_scalar(KeepInOrder):
	'''
	class valuestructurecomplexgrid_scalar
	The source-rate of particles in phase space; given on grid [(m/s)^-3 m^-3 s^-1]. Time-dependent; Complexgrid_scalar

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

	def __init__(self, base_path_in='value'):
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
		ret = space + 'class valuestructurecomplexgrid_scalar\n'
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
			print ('field '+self.base_path+' of type valuestructurecomplexgrid_scalar, run function putSlice') 
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
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect3DDoubleSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type valuestructurecomplexgrid_scalar, run function replaceLastSlice') 
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
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'scalar', numpy.array(self.scalar).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'vector', numpy.array(self.vector).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect3DDoubleSlice(self.idx, path, cpopath + 'matrix', numpy.array(self.matrix).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type valuestructurecomplexgrid_scalar, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type valuestructurecomplexgrid_scalar, run function getSlice') 
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
		status, ret_scalar, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'scalar', inTime, interpolMode)
		check_status(status)
		if not status:
			self.scalar = ret_scalar
			self.cpoTime = retTime
		status, ret_vector, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'vector', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vector = ret_vector
			self.cpoTime = retTime
		status, ret_matrix, retTime = ull.getVect3DDoubleSlice(self.idx, path, cpopath + 'matrix', inTime, interpolMode)
		check_status(status)
		if not status:
			self.matrix = ret_matrix
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type valuestructurecomplexgrid_scalar, run function build_non_resampled_data') 
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
			status, scalarList = ull.getVect2DDouble(self.idx, path, cpopath + 'scalar')
			if len(scalarList) == 0:
				scalarList = numpy.resize(scalarList, (0,nbslice))
			check_status(status)
			status, vectorList = ull.getVect3DDouble(self.idx, path, cpopath + 'vector')
			if len(vectorList) == 0:
				vectorList = numpy.resize(vectorList, (0,0,nbslice))
			check_status(status)
			status, matrixList = ull.getVect4DDouble(self.idx, path, cpopath + 'matrix')
			if len(matrixList) == 0:
				matrixList = numpy.resize(matrixList, (0,0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = valuestructurecomplexgrid_scalar(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = int(griduidList[i].copy())
				slice.subgrid = int(subgridList[i].copy())
				slice.scalar = scalarList[:,i]
				slice.vector = vectorList[:,:,i]
				slice.matrix = matrixList[:,:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type valuestructurecomplexgrid_scalarObj, run function putTimedElt') 
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
			print ('object of type valuestructurecomplexgrid_scalarObj, run function getTimedElt') 
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
			print ('object of type valuestructurecomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type valuestructurecomplexgrid_scalarObj, run function getNonTimedElt') 
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


class parametersstructureparameters(KeepInOrder):
	'''
	class parametersstructureparameters
	Parameters used to defined the grid coordiantes. Time-dependent

	Attributes:
	- equatorial : class equatorialstructureequatorial_plane
	   Description of the equatorial plane or any other omnigeuous surfaces. Time-dependent
	'''

	def __init__(self, base_path_in='parameters'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.equatorial = equatorialstructureequatorial_plane('equatorial')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class parametersstructureparameters\n'
		ret = ret + space + 'Attribute equatorial\n ' + self.equatorial.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.equatorial.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type parametersstructureparameters, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.equatorial.cpoTime = self.cpoTime
		self.equatorial.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type parametersstructureparameters, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.equatorial.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type parametersstructureparameters, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.equatorial.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type parametersstructureparameters, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.equatorial.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type parametersstructureparameters, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			equatorialList = self.equatorial.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = parametersstructureparameters(self.base_path)
				slice.setExpIdx(self.idx)
				slice.equatorial = equatorialList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parametersstructureparametersObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.equatorial.putTimedElt(path, cpopath + 'equatorial', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parametersstructureparametersObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.equatorial.getTimedElt(path, cpopath + 'equatorial', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parametersstructureparametersObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.equatorial.putNonTimedElt(path, cpopath + 'equatorial', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parametersstructureparametersObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.equatorial.getNonTimedElt(path, cpopath + 'equatorial', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.equatorial.deleteData(path, cpopath)


class equatorialstructureequatorial_plane(KeepInOrder):
	'''
	class equatorialstructureequatorial_plane
	Description of the equatorial plane or any other omnigeuous surfaces. Time-dependent

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius coordinate of the equitorial plane (m). Time-dependent; Vector(n_equitorial_grid)
	- z : numpy.ndarray 1D with float
	   Major radius coordinate of the equitorial plane (m). Time-dependent; Vector(n_equitorial_grid)
	- s : numpy.ndarray 1D with float
	   Distance along the poloidal projection of the equitorial plane (m). Here s=0 should be at the magnetic axis, s>0 on the low field side and s<0 on the high field side. For example, in up-down symmetric fields s=R-R0, where R is the major radius and R0 the major radius at the magnetic axis. Time-dependent; Vector(n_equatorial_grid)
	- rho_tor : numpy.ndarray 1D with float
	   Toroidal flux coordinate [m]. Defined as sqrt((phi-phi_axis)/pi/B0), where B0 is the reference magnetic field, phi is the toroidal flux and phi_axis is the toroidal flux at the magnetic axis. Time-dependent; Vector (n_equitorial_grid)
	- psi : numpy.ndarray 1D with float
	   Poloidal flux [Wb], evaluated without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent; Vector (n_equitorial_grid)
	- b_mod : numpy.ndarray 1D with float
	   The modulous of the magnetic field along the equitorial plane, or more generally of the omnigeuous surfaces [T]. Time-dependent; Vector (n_equatorial_grid)
	'''

	def __init__(self, base_path_in='equatorial'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')
		self.s = numpy.zeros(0, numpy.float64, order='C')
		self.rho_tor = numpy.zeros(0, numpy.float64, order='C')
		self.psi = numpy.zeros(0, numpy.float64, order='C')
		self.b_mod = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class equatorialstructureequatorial_plane\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.s.__str__()
		ret = ret + space + 'Attribute s\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_tor.__str__()
		ret = ret + space + 'Attribute rho_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_mod.__str__()
		ret = ret + space + 'Attribute b_mod\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type equatorialstructureequatorial_plane, run function putSlice') 
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
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 's', numpy.array(self.s).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'b_mod', numpy.array(self.b_mod).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type equatorialstructureequatorial_plane, run function replaceLastSlice') 
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
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 's', numpy.array(self.s).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'b_mod', numpy.array(self.b_mod).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type equatorialstructureequatorial_plane, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type equatorialstructureequatorial_plane, run function getSlice') 
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
		status, ret_s, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 's', inTime, interpolMode)
		check_status(status)
		if not status:
			self.s = ret_s
			self.cpoTime = retTime
		status, ret_rho_tor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
			self.cpoTime = retTime
		status, ret_psi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'psi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.psi = ret_psi
			self.cpoTime = retTime
		status, ret_b_mod, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'b_mod', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_mod = ret_b_mod
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type equatorialstructureequatorial_plane, run function build_non_resampled_data') 
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
			status, sList = ull.getVect2DDouble(self.idx, path, cpopath + 's')
			if len(sList) == 0:
				sList = numpy.resize(sList, (0,nbslice))
			check_status(status)
			status, rho_torList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_tor')
			if len(rho_torList) == 0:
				rho_torList = numpy.resize(rho_torList, (0,nbslice))
			check_status(status)
			status, psiList = ull.getVect2DDouble(self.idx, path, cpopath + 'psi')
			if len(psiList) == 0:
				psiList = numpy.resize(psiList, (0,nbslice))
			check_status(status)
			status, b_modList = ull.getVect2DDouble(self.idx, path, cpopath + 'b_mod')
			if len(b_modList) == 0:
				b_modList = numpy.resize(b_modList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = equatorialstructureequatorial_plane(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[:,i]
				slice.z = zList[:,i]
				slice.s = sList[:,i]
				slice.rho_tor = rho_torList[:,i]
				slice.psi = psiList[:,i]
				slice.b_mod = b_modList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type equatorialstructureequatorial_planeObj, run function putTimedElt') 
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
			print ('putVect1DDoubleInObject : ' + cpopath + 's') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 's', i, numpy.array(self.s).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'rho_tor', i, numpy.array(self.rho_tor).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'psi', i, numpy.array(self.psi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'b_mod') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'b_mod', i, numpy.array(self.b_mod).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type equatorialstructureequatorial_planeObj, run function getTimedElt') 
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
			print ('getVect1DDoubleInObject : ' + cpopath + 's') 
			print ('obj = ' + str(obj))
		status, ret_s = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 's', i)
		check_status(status)
		if not status:
			self.s = ret_s
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'rho_tor', i)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		status, ret_psi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'psi', i)
		check_status(status)
		if not status:
			self.psi = ret_psi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'b_mod') 
			print ('obj = ' + str(obj))
		status, ret_b_mod = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'b_mod', i)
		check_status(status)
		if not status:
			self.b_mod = ret_b_mod

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type equatorialstructureequatorial_planeObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type equatorialstructureequatorial_planeObj, run function getNonTimedElt') 
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
		ull.deleteData(self.idx, path, cpopath + 's')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor')
		ull.deleteData(self.idx, path, cpopath + 'psi')
		ull.deleteData(self.idx, path, cpopath + 'b_mod')


class markersstructureweighted_markers(KeepInOrder):
	'''
	class markersstructureweighted_markers
	Source given as a set of markers (test particles) born per second.

	Attributes:
	- variable_ids : class variable_idsstruct_arrayidentifier: array of variable_idsstruct_arrayidentifierObj objects
	   Identifier for the variable_ids stored in the coord matrix (see coordinate_identifier_definitions in the Documentation website under Conventions/Enumerated_datatypes). Vector(NDIM)
	- coord : numpy.ndarray 2D with float
	   Coordinates of the markers. The coordinates used is specified in variable_ids. Time-dependent; Float(NMARK,NDIM)
	- weight : numpy.ndarray 1D with float
	   Weight of the marker; number of real particles represented by the marker. Time-dependent; Float(NMARK)
	'''

	def __init__(self, base_path_in='markers'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.variable_ids = variable_idsstruct_arrayidentifier('variable_ids')
		self.coord = numpy.zeros((0,0), numpy.float64, order='C')
		self.weight = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class markersstructureweighted_markers\n'
		ret = ret + space + 'Attribute variable_ids\n ' + self.variable_ids.__str__(depth+1)
		s = self.coord.__str__()
		ret = ret + space + 'Attribute coord\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.variable_ids.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type markersstructureweighted_markers, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.variable_ids.cpoTime = self.cpoTime
		self.variable_ids.putSlice(path, cpopath)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'coord', numpy.array(self.coord).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type markersstructureweighted_markers, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.variable_ids.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'coord', numpy.array(self.coord).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type markersstructureweighted_markers, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.variable_ids.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type markersstructureweighted_markers, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.variable_ids.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_coord, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'coord', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coord = ret_coord
			self.cpoTime = retTime
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type markersstructureweighted_markers, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			variable_idsList = self.variable_ids.build_non_resampled_data(path, cpopath, nbslice)
			status, coordList = ull.getVect3DDouble(self.idx, path, cpopath + 'coord')
			if len(coordList) == 0:
				coordList = numpy.resize(coordList, (0,0,nbslice))
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = markersstructureweighted_markers(self.base_path)
				slice.setExpIdx(self.idx)
				slice.variable_ids = variable_idsList[i]
				slice.coord = coordList[:,:,i]
				slice.weight = weightList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type markersstructureweighted_markersObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'coord') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'coord', i, numpy.array(self.coord).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type markersstructureweighted_markersObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'coord') 
			print ('obj = ' + str(obj))
		status, ret_coord = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'coord', i)
		check_status(status)
		if not status:
			self.coord = ret_coord
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type markersstructureweighted_markersObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.variable_ids.putNonTimedElt(path, cpopath + 'variable_ids', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type markersstructureweighted_markersObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.variable_ids.getNonTimedElt(path, cpopath + 'variable_ids', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'variable_ids')
		ull.deleteData(self.idx, path, cpopath + 'coord')
		ull.deleteData(self.idx, path, cpopath + 'weight')


class variable_idsstruct_arrayidentifier:
	'''
	class variable_idsstruct_arrayidentifier
	Identifier for the variable_ids stored in the coord matrix (see coordinate_identifier_definitions in the Documentation website under Conventions/Enumerated_datatypes). Vector(NDIM)

	Attributes:
	- array : list of variable_idsstruct_arrayidentifierObj 
	'''

	def __init__(self, base_path_in='variable_ids'):
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
		ret = space + 'class variable_idsstruct_arrayidentifier\n'
		for i in range(len(self.array)):
			ret = ret + space + 'variable_idsstruct_arrayidentifier[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(variable_idsstruct_arrayidentifierObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type variable_idsstruct_arrayidentifier, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type variable_idsstruct_arrayidentifier, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type variable_idsstruct_arrayidentifier, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type variable_idsstruct_arrayidentifier, run function getSlice') 
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
			print ('field '+self.base_path+' of type variable_idsstruct_arrayidentifier, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(variable_idsstruct_arrayidentifier(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = variable_idsstruct_arrayidentifier(self.base_path)
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
			print ('field '+self.base_path+' of type variable_idsstruct_arrayidentifier, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type variable_idsstruct_arrayidentifier, run function getNonTimedElt') 
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


class variable_idsstruct_arrayidentifierObj(KeepInOrder):
	'''
	class variable_idsstruct_arrayidentifierObj
	Identifier for the variable_ids stored in the coord matrix (see coordinate_identifier_definitions in the Documentation website under Conventions/Enumerated_datatypes). Vector(NDIM)

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='variable_ids'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class variable_idsstruct_arrayidentifierObj\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type variable_idsstruct_arrayidentifierObj, run function putNonTimedElt') 
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
			print ('object of type variable_idsstruct_arrayidentifierObj, run function getNonTimedElt') 
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
