# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class edge(KeepInOrder):
	'''
	class edge
	CPO for edge/SOL plasma  description. Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- grid : class gridstructurecomplexgrid
	   Grid description
	- species : class speciesstruct_arrayspecies_desc: array of speciesstruct_arrayspecies_descObj objects
	   Description of ion species. Array of structures(nspecies)
	- compositions : class compositionsstructurecompositions_type
	   Contains all the composition information for the simulation (main ions, impurities, neutrals, edge species).
	- fluid : class fluidstructureedge_fluid
	   Fluid description of edge plasma. Time-dependent.
	- kinetic : class kineticstructureedge_kinetic
	   Kinetic description of edge plasma. Time-dependent.
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar
	'''

	def __init__(self):
		self.base_path = 'edge'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 1
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.grid = gridstructurecomplexgrid('grid')
		self.species = speciesstruct_arrayspecies_desc('species')
		self.compositions = compositionsstructurecompositions_type('compositions')
		self.fluid = fluidstructureedge_fluid('fluid')
		self.kinetic = kineticstructureedge_kinetic('kinetic')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class edge\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute grid\n ' + self.grid.__str__(depth+1)
		ret = ret + space + 'Attribute species\n ' + self.species.__str__(depth+1)
		ret = ret + space + 'Attribute compositions\n ' + self.compositions.__str__(depth+1)
		ret = ret + space + 'Attribute fluid\n ' + self.fluid.__str__(depth+1)
		ret = ret + space + 'Attribute kinetic\n ' + self.kinetic.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.grid.setExpIdx(idx)
		self.species.setExpIdx(idx)
		self.compositions.setExpIdx(idx)
		self.fluid.setExpIdx(idx)
		self.kinetic.setExpIdx(idx)
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
		self.grid.cpoTime = self.cpoTime
		self.grid.putSlice(path, cpopath)
		self.species.cpoTime = self.cpoTime
		self.species.putSlice(path, cpopath)
		self.compositions.cpoTime = self.cpoTime
		self.compositions.putSlice(path, cpopath)
		self.fluid.cpoTime = self.cpoTime
		self.fluid.putSlice(path, cpopath)
		self.kinetic.cpoTime = self.cpoTime
		self.kinetic.putSlice(path, cpopath)
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
		self.grid.replaceLastSlice(path, cpopath)
		self.species.replaceLastSlice(path, cpopath)
		self.compositions.replaceLastSlice(path, cpopath)
		self.fluid.replaceLastSlice(path, cpopath)
		self.kinetic.replaceLastSlice(path, cpopath)
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
		self.grid.putNonTimed(path, cpopath)
		self.species.putNonTimed(path, cpopath)
		self.compositions.putNonTimed(path, cpopath)
		self.fluid.putNonTimed(path, cpopath)
		self.kinetic.putNonTimed(path, cpopath)
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
		self.grid.getSlice(path, cpopath, inTime, interpolMode)
		self.species.getSlice(path, cpopath, inTime, interpolMode)
		self.compositions.getSlice(path, cpopath, inTime, interpolMode)
		self.fluid.getSlice(path, cpopath, inTime, interpolMode)
		self.kinetic.getSlice(path, cpopath, inTime, interpolMode)
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
			gridList = self.grid.build_non_resampled_data(path, cpopath, nbslice)
			speciesList = self.species.build_non_resampled_data(path, cpopath, nbslice)
			compositionsList = self.compositions.build_non_resampled_data(path, cpopath, nbslice)
			fluidList = self.fluid.build_non_resampled_data(path, cpopath, nbslice)
			kineticList = self.kinetic.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = edge()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.grid = gridList[i]
				slice.species = speciesList[i]
				slice.compositions = compositionsList[i]
				slice.fluid = fluidList[i]
				slice.kinetic = kineticList[i]
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
		self.grid.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'species')
		self.compositions.deleteData(path, cpopath)
		self.fluid.deleteData(path, cpopath)
		self.kinetic.deleteData(path, cpopath)
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class edgeArray:
	'''
	class edgeArray
	CPO for edge/SOL plasma  description. Time-dependent CPO

	Attributes:
	- array : list of edge
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
		ret = space + 'class edgeArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'edge cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = edge()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(edge())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = edge()
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


class gridstructurecomplexgrid(KeepInOrder):
	'''
	class gridstructurecomplexgrid
	Grid description

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


class speciesstruct_arrayspecies_desc:
	'''
	class speciesstruct_arrayspecies_desc
	Description of ion species. Array of structures(nspecies)

	Attributes:
	- array : list of speciesstruct_arrayspecies_descObj 
	'''

	def __init__(self, base_path_in='species'):
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
		ret = space + 'class speciesstruct_arrayspecies_desc\n'
		for i in range(len(self.array)):
			ret = ret + space + 'speciesstruct_arrayspecies_desc[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(speciesstruct_arrayspecies_descObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type speciesstruct_arrayspecies_desc, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type speciesstruct_arrayspecies_desc, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type speciesstruct_arrayspecies_desc, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type speciesstruct_arrayspecies_desc, run function getSlice') 
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
			print ('field '+self.base_path+' of type speciesstruct_arrayspecies_desc, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(speciesstruct_arrayspecies_desc(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = speciesstruct_arrayspecies_desc(self.base_path)
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
			print ('field '+self.base_path+' of type speciesstruct_arrayspecies_desc, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type speciesstruct_arrayspecies_desc, run function getNonTimedElt') 
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


class speciesstruct_arrayspecies_descObj(KeepInOrder):
	'''
	class speciesstruct_arrayspecies_descObj
	Description of ion species. Array of structures(nspecies)

	Attributes:
	- label : str
	   Name of species
	- amn : float
	   Atomic mass number of the species
	- zn : float
	   Nuclear charge of the impurity
	- zmin : float
	   Minimum Z of species charge state bundle
	- zmax : float
	   Maximum Z of species charge state bundle
	'''

	def __init__(self, base_path_in='species'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.label = ''
		self.amn = EMPTY_DOUBLE
		self.zn = EMPTY_DOUBLE
		self.zmin = EMPTY_DOUBLE
		self.zmax = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class speciesstruct_arrayspecies_descObj\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		ret = ret + space + 'Attribute amn: ' + str(self.amn) + '\n'
		ret = ret + space + 'Attribute zn: ' + str(self.zn) + '\n'
		ret = ret + space + 'Attribute zmin: ' + str(self.zmin) + '\n'
		ret = ret + space + 'Attribute zmax: ' + str(self.zmax) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type speciesstruct_arrayspecies_descObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'label', i, self.label)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'amn') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'amn', i, self.amn)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zn') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zn', i, self.zn)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zmin') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zmin', i, self.zmin)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'zmax') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'zmax', i, self.zmax)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type speciesstruct_arrayspecies_descObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'label') 
			print ('obj = ' + str(obj))
		status, ret_label = ull.getStringFromObject(self.idx, obj, cpopath + 'label', i)
		check_status(status)
		if not status:
			self.label = ret_label
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


class compositionsstructurecompositions_type(KeepInOrder):
	'''
	class compositionsstructurecompositions_type
	Contains all the composition information for the simulation (main ions, impurities, neutrals, edge species).

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


class fluidstructureedge_fluid(KeepInOrder):
	'''
	class fluidstructureedge_fluid
	Fluid description of edge plasma. Time-dependent.

	Attributes:
	- ne : class nestructureedge_fluid_scalar_simplestruct
	   Electron density [1/m^3]; Time-dependent;
	- ni : class nistruct_arrayedge_fluid_scalar: array of nistruct_arrayedge_fluid_scalarObj objects
	   Ion density [1/m^3] (per species). Array of structures(nspecies); Time-dependent;
	- ve : class vestructureedge_fluid_vector_simplestruct
	   Electron velocity [m/s]; Time-dependent;
	- vi : class vistruct_arrayedge_fluid_vector: array of vistruct_arrayedge_fluid_vectorObj objects
	   Ion velocity [m/s] (per species).Array of structures(nspecies); Time-dependent;
	- te : class testructureedge_fluid_scalar_simplestruct
	   Electron temperature [eV]; Time-dependent;
	- ti : class tistruct_arrayedge_fluid_scalar: array of tistruct_arrayedge_fluid_scalarObj objects
	   Ion temperature [eV] (per species). Array of structures(nspecies).; Time-dependent;
	- te_aniso : class te_anisostructureedge_fluid_vector_simplestruct
	   Anisotropic electron temperature [eV]; Time-dependent;
	- ti_aniso : class ti_anisostruct_arrayedge_fluid_vector: array of ti_anisostruct_arrayedge_fluid_vectorObj objects
	   Anisotropic ion temperature [eV] (per species). Array of structures(nspecies); Time-dependent;
	- po : class postructureedge_fluid_scalar_simplestruct
	   Electric potential [V]; Time-dependent;
	- j : class jstructureedge_fluid_vector_simplestruct
	   Electric current [A]; Time-dependent;
	- b : class bstruct_arraycomplexgrid_vector: array of bstruct_arraycomplexgrid_vectorObj objects
	   Magnetic field vector [T]; Time-dependent;
	'''

	def __init__(self, base_path_in='fluid'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.ne = nestructureedge_fluid_scalar_simplestruct('ne')
		self.ni = nistruct_arrayedge_fluid_scalar('ni')
		self.ve = vestructureedge_fluid_vector_simplestruct('ve')
		self.vi = vistruct_arrayedge_fluid_vector('vi')
		self.te = testructureedge_fluid_scalar_simplestruct('te')
		self.ti = tistruct_arrayedge_fluid_scalar('ti')
		self.te_aniso = te_anisostructureedge_fluid_vector_simplestruct('te_aniso')
		self.ti_aniso = ti_anisostruct_arrayedge_fluid_vector('ti_aniso')
		self.po = postructureedge_fluid_scalar_simplestruct('po')
		self.j = jstructureedge_fluid_vector_simplestruct('j')
		self.b = bstruct_arraycomplexgrid_vector('b')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class fluidstructureedge_fluid\n'
		ret = ret + space + 'Attribute ne\n ' + self.ne.__str__(depth+1)
		ret = ret + space + 'Attribute ni\n ' + self.ni.__str__(depth+1)
		ret = ret + space + 'Attribute ve\n ' + self.ve.__str__(depth+1)
		ret = ret + space + 'Attribute vi\n ' + self.vi.__str__(depth+1)
		ret = ret + space + 'Attribute te\n ' + self.te.__str__(depth+1)
		ret = ret + space + 'Attribute ti\n ' + self.ti.__str__(depth+1)
		ret = ret + space + 'Attribute te_aniso\n ' + self.te_aniso.__str__(depth+1)
		ret = ret + space + 'Attribute ti_aniso\n ' + self.ti_aniso.__str__(depth+1)
		ret = ret + space + 'Attribute po\n ' + self.po.__str__(depth+1)
		ret = ret + space + 'Attribute j\n ' + self.j.__str__(depth+1)
		ret = ret + space + 'Attribute b\n ' + self.b.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.ne.setExpIdx(idx)
		self.ni.setExpIdx(idx)
		self.ve.setExpIdx(idx)
		self.vi.setExpIdx(idx)
		self.te.setExpIdx(idx)
		self.ti.setExpIdx(idx)
		self.te_aniso.setExpIdx(idx)
		self.ti_aniso.setExpIdx(idx)
		self.po.setExpIdx(idx)
		self.j.setExpIdx(idx)
		self.b.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluidstructureedge_fluid, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.ne.cpoTime = self.cpoTime
		self.ne.putSlice(path, cpopath)
		self.ni.cpoTime = self.cpoTime
		self.ni.putSlice(path, cpopath)
		self.ve.cpoTime = self.cpoTime
		self.ve.putSlice(path, cpopath)
		self.vi.cpoTime = self.cpoTime
		self.vi.putSlice(path, cpopath)
		self.te.cpoTime = self.cpoTime
		self.te.putSlice(path, cpopath)
		self.ti.cpoTime = self.cpoTime
		self.ti.putSlice(path, cpopath)
		self.te_aniso.cpoTime = self.cpoTime
		self.te_aniso.putSlice(path, cpopath)
		self.ti_aniso.cpoTime = self.cpoTime
		self.ti_aniso.putSlice(path, cpopath)
		self.po.cpoTime = self.cpoTime
		self.po.putSlice(path, cpopath)
		self.j.cpoTime = self.cpoTime
		self.j.putSlice(path, cpopath)
		self.b.cpoTime = self.cpoTime
		self.b.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluidstructureedge_fluid, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.ne.replaceLastSlice(path, cpopath)
		self.ni.replaceLastSlice(path, cpopath)
		self.ve.replaceLastSlice(path, cpopath)
		self.vi.replaceLastSlice(path, cpopath)
		self.te.replaceLastSlice(path, cpopath)
		self.ti.replaceLastSlice(path, cpopath)
		self.te_aniso.replaceLastSlice(path, cpopath)
		self.ti_aniso.replaceLastSlice(path, cpopath)
		self.po.replaceLastSlice(path, cpopath)
		self.j.replaceLastSlice(path, cpopath)
		self.b.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluidstructureedge_fluid, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.ne.putNonTimed(path, cpopath)
		self.ni.putNonTimed(path, cpopath)
		self.ve.putNonTimed(path, cpopath)
		self.vi.putNonTimed(path, cpopath)
		self.te.putNonTimed(path, cpopath)
		self.ti.putNonTimed(path, cpopath)
		self.te_aniso.putNonTimed(path, cpopath)
		self.ti_aniso.putNonTimed(path, cpopath)
		self.po.putNonTimed(path, cpopath)
		self.j.putNonTimed(path, cpopath)
		self.b.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type fluidstructureedge_fluid, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.ne.getSlice(path, cpopath, inTime, interpolMode)
		self.ni.getSlice(path, cpopath, inTime, interpolMode)
		self.ve.getSlice(path, cpopath, inTime, interpolMode)
		self.vi.getSlice(path, cpopath, inTime, interpolMode)
		self.te.getSlice(path, cpopath, inTime, interpolMode)
		self.ti.getSlice(path, cpopath, inTime, interpolMode)
		self.te_aniso.getSlice(path, cpopath, inTime, interpolMode)
		self.ti_aniso.getSlice(path, cpopath, inTime, interpolMode)
		self.po.getSlice(path, cpopath, inTime, interpolMode)
		self.j.getSlice(path, cpopath, inTime, interpolMode)
		self.b.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type fluidstructureedge_fluid, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			neList = self.ne.build_non_resampled_data(path, cpopath, nbslice)
			niList = self.ni.build_non_resampled_data(path, cpopath, nbslice)
			veList = self.ve.build_non_resampled_data(path, cpopath, nbslice)
			viList = self.vi.build_non_resampled_data(path, cpopath, nbslice)
			teList = self.te.build_non_resampled_data(path, cpopath, nbslice)
			tiList = self.ti.build_non_resampled_data(path, cpopath, nbslice)
			te_anisoList = self.te_aniso.build_non_resampled_data(path, cpopath, nbslice)
			ti_anisoList = self.ti_aniso.build_non_resampled_data(path, cpopath, nbslice)
			poList = self.po.build_non_resampled_data(path, cpopath, nbslice)
			jList = self.j.build_non_resampled_data(path, cpopath, nbslice)
			bList = self.b.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = fluidstructureedge_fluid(self.base_path)
				slice.setExpIdx(self.idx)
				slice.ne = neList[i]
				slice.ni = niList[i]
				slice.ve = veList[i]
				slice.vi = viList[i]
				slice.te = teList[i]
				slice.ti = tiList[i]
				slice.te_aniso = te_anisoList[i]
				slice.ti_aniso = ti_anisoList[i]
				slice.po = poList[i]
				slice.j = jList[i]
				slice.b = bList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluidstructureedge_fluidObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.ne.putTimedElt(path, cpopath + 'ne', i, obj)
		obj = self.ni.putTimedElt(path, cpopath + 'ni', i, obj)
		obj = self.ve.putTimedElt(path, cpopath + 've', i, obj)
		obj = self.vi.putTimedElt(path, cpopath + 'vi', i, obj)
		obj = self.te.putTimedElt(path, cpopath + 'te', i, obj)
		obj = self.ti.putTimedElt(path, cpopath + 'ti', i, obj)
		obj = self.te_aniso.putTimedElt(path, cpopath + 'te_aniso', i, obj)
		obj = self.ti_aniso.putTimedElt(path, cpopath + 'ti_aniso', i, obj)
		obj = self.po.putTimedElt(path, cpopath + 'po', i, obj)
		obj = self.j.putTimedElt(path, cpopath + 'j', i, obj)
		obj = self.b.putTimedElt(path, cpopath + 'b', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluidstructureedge_fluidObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.ne.getTimedElt(path, cpopath + 'ne', i, obj)
		self.ni.getTimedElt(path, cpopath + 'ni', i, obj)
		self.ve.getTimedElt(path, cpopath + 've', i, obj)
		self.vi.getTimedElt(path, cpopath + 'vi', i, obj)
		self.te.getTimedElt(path, cpopath + 'te', i, obj)
		self.ti.getTimedElt(path, cpopath + 'ti', i, obj)
		self.te_aniso.getTimedElt(path, cpopath + 'te_aniso', i, obj)
		self.ti_aniso.getTimedElt(path, cpopath + 'ti_aniso', i, obj)
		self.po.getTimedElt(path, cpopath + 'po', i, obj)
		self.j.getTimedElt(path, cpopath + 'j', i, obj)
		self.b.getTimedElt(path, cpopath + 'b', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluidstructureedge_fluidObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.ne.putNonTimedElt(path, cpopath + 'ne', i, obj)
		obj = self.ni.putNonTimedElt(path, cpopath + 'ni', i, obj)
		obj = self.ve.putNonTimedElt(path, cpopath + 've', i, obj)
		obj = self.vi.putNonTimedElt(path, cpopath + 'vi', i, obj)
		obj = self.te.putNonTimedElt(path, cpopath + 'te', i, obj)
		obj = self.ti.putNonTimedElt(path, cpopath + 'ti', i, obj)
		obj = self.te_aniso.putNonTimedElt(path, cpopath + 'te_aniso', i, obj)
		obj = self.ti_aniso.putNonTimedElt(path, cpopath + 'ti_aniso', i, obj)
		obj = self.po.putNonTimedElt(path, cpopath + 'po', i, obj)
		obj = self.j.putNonTimedElt(path, cpopath + 'j', i, obj)
		obj = self.b.putNonTimedElt(path, cpopath + 'b', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluidstructureedge_fluidObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.ne.getNonTimedElt(path, cpopath + 'ne', i, obj)
		self.ni.getNonTimedElt(path, cpopath + 'ni', i, obj)
		self.ve.getNonTimedElt(path, cpopath + 've', i, obj)
		self.vi.getNonTimedElt(path, cpopath + 'vi', i, obj)
		self.te.getNonTimedElt(path, cpopath + 'te', i, obj)
		self.ti.getNonTimedElt(path, cpopath + 'ti', i, obj)
		self.te_aniso.getNonTimedElt(path, cpopath + 'te_aniso', i, obj)
		self.ti_aniso.getNonTimedElt(path, cpopath + 'ti_aniso', i, obj)
		self.po.getNonTimedElt(path, cpopath + 'po', i, obj)
		self.j.getNonTimedElt(path, cpopath + 'j', i, obj)
		self.b.getNonTimedElt(path, cpopath + 'b', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.ne.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'ni')
		self.ve.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'vi')
		self.te.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'ti')
		self.te_aniso.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'ti_aniso')
		self.po.deleteData(path, cpopath)
		self.j.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'b')


class nestructureedge_fluid_scalar_simplestruct(KeepInOrder):
	'''
	class nestructureedge_fluid_scalar_simplestruct
	Electron density [1/m^3]; Time-dependent;

	Attributes:
	- value : class valuestruct_arraycomplexgrid_scalar: array of valuestruct_arraycomplexgrid_scalarObj objects
	   Value of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- bndvalue : class bndvaluestruct_arraycomplexgrid_scalar: array of bndvaluestruct_arraycomplexgrid_scalarObj objects
	   Boundary values of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- flux : class fluxstruct_arraycomplexgrid_vector: array of fluxstruct_arraycomplexgrid_vectorObj objects
	   Flux of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- bndflux : class bndfluxstruct_arraycomplexgrid_vector: array of bndfluxstruct_arraycomplexgrid_vectorObj objects
	   Flux of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- transpcoeff : class transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff: array of transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj objects
	   Transport coefficients; Time-dependent; Array of structures (nsubgrid_quantity)
	- source : class sourcestruct_arraycomplexgrid_scalar: array of sourcestruct_arraycomplexgrid_scalarObj objects
	   Source; Time-dependent; Array of structures (nsubgrid_quantity)
	'''

	def __init__(self, base_path_in='ne'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = valuestruct_arraycomplexgrid_scalar('value')
		self.bndvalue = bndvaluestruct_arraycomplexgrid_scalar('bndvalue')
		self.flux = fluxstruct_arraycomplexgrid_vector('flux')
		self.bndflux = bndfluxstruct_arraycomplexgrid_vector('bndflux')
		self.transpcoeff = transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff('transpcoeff')
		self.source = sourcestruct_arraycomplexgrid_scalar('source')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nestructureedge_fluid_scalar_simplestruct\n'
		ret = ret + space + 'Attribute value\n ' + self.value.__str__(depth+1)
		ret = ret + space + 'Attribute bndvalue\n ' + self.bndvalue.__str__(depth+1)
		ret = ret + space + 'Attribute flux\n ' + self.flux.__str__(depth+1)
		ret = ret + space + 'Attribute bndflux\n ' + self.bndflux.__str__(depth+1)
		ret = ret + space + 'Attribute transpcoeff\n ' + self.transpcoeff.__str__(depth+1)
		ret = ret + space + 'Attribute source\n ' + self.source.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.value.setExpIdx(idx)
		self.bndvalue.setExpIdx(idx)
		self.flux.setExpIdx(idx)
		self.bndflux.setExpIdx(idx)
		self.transpcoeff.setExpIdx(idx)
		self.source.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nestructureedge_fluid_scalar_simplestruct, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.value.cpoTime = self.cpoTime
		self.value.putSlice(path, cpopath)
		self.bndvalue.cpoTime = self.cpoTime
		self.bndvalue.putSlice(path, cpopath)
		self.flux.cpoTime = self.cpoTime
		self.flux.putSlice(path, cpopath)
		self.bndflux.cpoTime = self.cpoTime
		self.bndflux.putSlice(path, cpopath)
		self.transpcoeff.cpoTime = self.cpoTime
		self.transpcoeff.putSlice(path, cpopath)
		self.source.cpoTime = self.cpoTime
		self.source.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nestructureedge_fluid_scalar_simplestruct, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.value.replaceLastSlice(path, cpopath)
		self.bndvalue.replaceLastSlice(path, cpopath)
		self.flux.replaceLastSlice(path, cpopath)
		self.bndflux.replaceLastSlice(path, cpopath)
		self.transpcoeff.replaceLastSlice(path, cpopath)
		self.source.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nestructureedge_fluid_scalar_simplestruct, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.value.putNonTimed(path, cpopath)
		self.bndvalue.putNonTimed(path, cpopath)
		self.flux.putNonTimed(path, cpopath)
		self.bndflux.putNonTimed(path, cpopath)
		self.transpcoeff.putNonTimed(path, cpopath)
		self.source.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type nestructureedge_fluid_scalar_simplestruct, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.value.getSlice(path, cpopath, inTime, interpolMode)
		self.bndvalue.getSlice(path, cpopath, inTime, interpolMode)
		self.flux.getSlice(path, cpopath, inTime, interpolMode)
		self.bndflux.getSlice(path, cpopath, inTime, interpolMode)
		self.transpcoeff.getSlice(path, cpopath, inTime, interpolMode)
		self.source.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type nestructureedge_fluid_scalar_simplestruct, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			valueList = self.value.build_non_resampled_data(path, cpopath, nbslice)
			bndvalueList = self.bndvalue.build_non_resampled_data(path, cpopath, nbslice)
			fluxList = self.flux.build_non_resampled_data(path, cpopath, nbslice)
			bndfluxList = self.bndflux.build_non_resampled_data(path, cpopath, nbslice)
			transpcoeffList = self.transpcoeff.build_non_resampled_data(path, cpopath, nbslice)
			sourceList = self.source.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = nestructureedge_fluid_scalar_simplestruct(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[i]
				slice.bndvalue = bndvalueList[i]
				slice.flux = fluxList[i]
				slice.bndflux = bndfluxList[i]
				slice.transpcoeff = transpcoeffList[i]
				slice.source = sourceList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nestructureedge_fluid_scalar_simplestructObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.value.putTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.flux.putTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.bndflux.putTimedElt(path, cpopath + 'bndflux', i, obj)
		obj = self.transpcoeff.putTimedElt(path, cpopath + 'transpcoeff', i, obj)
		obj = self.source.putTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nestructureedge_fluid_scalar_simplestructObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.value.getTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.flux.getTimedElt(path, cpopath + 'flux', i, obj)
		self.bndflux.getTimedElt(path, cpopath + 'bndflux', i, obj)
		self.transpcoeff.getTimedElt(path, cpopath + 'transpcoeff', i, obj)
		self.source.getTimedElt(path, cpopath + 'source', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nestructureedge_fluid_scalar_simplestructObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.value.putNonTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.flux.putNonTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.bndflux.putNonTimedElt(path, cpopath + 'bndflux', i, obj)
		obj = self.transpcoeff.putNonTimedElt(path, cpopath + 'transpcoeff', i, obj)
		obj = self.source.putNonTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nestructureedge_fluid_scalar_simplestructObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.value.getNonTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.flux.getNonTimedElt(path, cpopath + 'flux', i, obj)
		self.bndflux.getNonTimedElt(path, cpopath + 'bndflux', i, obj)
		self.transpcoeff.getNonTimedElt(path, cpopath + 'transpcoeff', i, obj)
		self.source.getNonTimedElt(path, cpopath + 'source', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'bndvalue')
		ull.deleteData(self.idx, path, cpopath + 'flux')
		ull.deleteData(self.idx, path, cpopath + 'bndflux')
		ull.deleteData(self.idx, path, cpopath + 'transpcoeff')
		ull.deleteData(self.idx, path, cpopath + 'source')


class valuestruct_arraycomplexgrid_scalar:
	'''
	class valuestruct_arraycomplexgrid_scalar
	Value of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)

	Attributes:
	- array : list of valuestruct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='value'):
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
		ret = space + 'class valuestruct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'valuestruct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(valuestruct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type valuestruct_arraycomplexgrid_scalar, run function putSlice') 
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
			print ('field '+self.base_path+' of type valuestruct_arraycomplexgrid_scalar, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type valuestruct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type valuestruct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type valuestruct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(valuestruct_arraycomplexgrid_scalar(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(valuestruct_arraycomplexgrid_scalar(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = valuestruct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type valuestruct_arraycomplexgrid_scalar, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type valuestruct_arraycomplexgrid_scalar, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type valuestruct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type valuestruct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class valuestruct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class valuestruct_arraycomplexgrid_scalarObj
	Value of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)

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
		ret = space + 'class valuestruct_arraycomplexgrid_scalarObj\n'
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
			print ('object of type valuestruct_arraycomplexgrid_scalarObj, run function putTimedElt') 
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
			print ('object of type valuestruct_arraycomplexgrid_scalarObj, run function getTimedElt') 
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
			print ('object of type valuestruct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type valuestruct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class bndvaluestruct_arraycomplexgrid_scalar:
	'''
	class bndvaluestruct_arraycomplexgrid_scalar
	Boundary values of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)

	Attributes:
	- array : list of bndvaluestruct_arraycomplexgrid_scalarObj 
	'''

	def __init__(self, base_path_in='bndvalue'):
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
		ret = space + 'class bndvaluestruct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'bndvaluestruct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(bndvaluestruct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bndvaluestruct_arraycomplexgrid_scalar, run function putSlice') 
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
			print ('field '+self.base_path+' of type bndvaluestruct_arraycomplexgrid_scalar, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type bndvaluestruct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type bndvaluestruct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type bndvaluestruct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(bndvaluestruct_arraycomplexgrid_scalar(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(bndvaluestruct_arraycomplexgrid_scalar(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = bndvaluestruct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type bndvaluestruct_arraycomplexgrid_scalar, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type bndvaluestruct_arraycomplexgrid_scalar, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type bndvaluestruct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type bndvaluestruct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class bndvaluestruct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class bndvaluestruct_arraycomplexgrid_scalarObj
	Boundary values of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)

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

	def __init__(self, base_path_in='bndvalue'):
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
		ret = space + 'class bndvaluestruct_arraycomplexgrid_scalarObj\n'
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
			print ('object of type bndvaluestruct_arraycomplexgrid_scalarObj, run function putTimedElt') 
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
			print ('object of type bndvaluestruct_arraycomplexgrid_scalarObj, run function getTimedElt') 
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
			print ('object of type bndvaluestruct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bndvaluestruct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class fluxstruct_arraycomplexgrid_vector:
	'''
	class fluxstruct_arraycomplexgrid_vector
	Flux of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)

	Attributes:
	- array : list of fluxstruct_arraycomplexgrid_vectorObj 
	'''

	def __init__(self, base_path_in='flux'):
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
		ret = space + 'class fluxstruct_arraycomplexgrid_vector\n'
		for i in range(len(self.array)):
			ret = ret + space + 'fluxstruct_arraycomplexgrid_vector[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(fluxstruct_arraycomplexgrid_vectorObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstruct_arraycomplexgrid_vector, run function putSlice') 
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
			print ('field '+self.base_path+' of type fluxstruct_arraycomplexgrid_vector, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type fluxstruct_arraycomplexgrid_vector, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type fluxstruct_arraycomplexgrid_vector, run function getSlice') 
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
			print ('field '+self.base_path+' of type fluxstruct_arraycomplexgrid_vector, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(fluxstruct_arraycomplexgrid_vector(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(fluxstruct_arraycomplexgrid_vector(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = fluxstruct_arraycomplexgrid_vector(self.base_path)
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
			print ('field '+self.base_path+' of type fluxstruct_arraycomplexgrid_vector, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type fluxstruct_arraycomplexgrid_vector, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type fluxstruct_arraycomplexgrid_vector, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type fluxstruct_arraycomplexgrid_vector, run function getNonTimedElt') 
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


class fluxstruct_arraycomplexgrid_vectorObj(KeepInOrder):
	'''
	class fluxstruct_arraycomplexgrid_vectorObj
	Flux of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)

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

	def __init__(self, base_path_in='flux'):
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
		ret = space + 'class fluxstruct_arraycomplexgrid_vectorObj\n'
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
			print ('object of type fluxstruct_arraycomplexgrid_vectorObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.comp.putTimedElt(path, cpopath + 'comp', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstruct_arraycomplexgrid_vectorObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.comp.getTimedElt(path, cpopath + 'comp', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstruct_arraycomplexgrid_vectorObj, run function putNonTimedElt') 
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
			print ('object of type fluxstruct_arraycomplexgrid_vectorObj, run function getNonTimedElt') 
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


class bndfluxstruct_arraycomplexgrid_vector:
	'''
	class bndfluxstruct_arraycomplexgrid_vector
	Flux of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)

	Attributes:
	- array : list of bndfluxstruct_arraycomplexgrid_vectorObj 
	'''

	def __init__(self, base_path_in='bndflux'):
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
		ret = space + 'class bndfluxstruct_arraycomplexgrid_vector\n'
		for i in range(len(self.array)):
			ret = ret + space + 'bndfluxstruct_arraycomplexgrid_vector[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(bndfluxstruct_arraycomplexgrid_vectorObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bndfluxstruct_arraycomplexgrid_vector, run function putSlice') 
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
			print ('field '+self.base_path+' of type bndfluxstruct_arraycomplexgrid_vector, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type bndfluxstruct_arraycomplexgrid_vector, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type bndfluxstruct_arraycomplexgrid_vector, run function getSlice') 
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
			print ('field '+self.base_path+' of type bndfluxstruct_arraycomplexgrid_vector, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(bndfluxstruct_arraycomplexgrid_vector(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(bndfluxstruct_arraycomplexgrid_vector(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = bndfluxstruct_arraycomplexgrid_vector(self.base_path)
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
			print ('field '+self.base_path+' of type bndfluxstruct_arraycomplexgrid_vector, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type bndfluxstruct_arraycomplexgrid_vector, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type bndfluxstruct_arraycomplexgrid_vector, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type bndfluxstruct_arraycomplexgrid_vector, run function getNonTimedElt') 
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


class bndfluxstruct_arraycomplexgrid_vectorObj(KeepInOrder):
	'''
	class bndfluxstruct_arraycomplexgrid_vectorObj
	Flux of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)

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

	def __init__(self, base_path_in='bndflux'):
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
		ret = space + 'class bndfluxstruct_arraycomplexgrid_vectorObj\n'
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
			print ('object of type bndfluxstruct_arraycomplexgrid_vectorObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.comp.putTimedElt(path, cpopath + 'comp', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bndfluxstruct_arraycomplexgrid_vectorObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.comp.getTimedElt(path, cpopath + 'comp', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bndfluxstruct_arraycomplexgrid_vectorObj, run function putNonTimedElt') 
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
			print ('object of type bndfluxstruct_arraycomplexgrid_vectorObj, run function getNonTimedElt') 
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


class transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff:
	'''
	class transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff
	Transport coefficients; Time-dependent; Array of structures (nsubgrid_quantity)

	Attributes:
	- array : list of transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj 
	'''

	def __init__(self, base_path_in='transpcoeff'):
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
		ret = space + 'class transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff\n'
		for i in range(len(self.array)):
			ret = ret + space + 'transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff, run function putSlice') 
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
			print ('field '+self.base_path+' of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff, run function getSlice') 
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
			print ('field '+self.base_path+' of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff(self.base_path)
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
			print ('field '+self.base_path+' of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff, run function getNonTimedElt') 
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


class transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj(KeepInOrder):
	'''
	class transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj
	Transport coefficients; Time-dependent; Array of structures (nsubgrid_quantity)

	Attributes:
	- d : class dstructurecomplexgrid_vector_simplestruct
	   Diffusivity [m^2/s]; Time-dependent;
	- v : class vstructurecomplexgrid_vector_simplestruct
	   Velocity [m/s]; Time-dependent;
	'''

	def __init__(self, base_path_in='transpcoeff'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.d = dstructurecomplexgrid_vector_simplestruct('d')
		self.v = vstructurecomplexgrid_vector_simplestruct('v')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj\n'
		ret = ret + space + 'Attribute d\n ' + self.d.__str__(depth+1)
		ret = ret + space + 'Attribute v\n ' + self.v.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.d.setExpIdx(idx)
		self.v.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.d.putTimedElt(path, cpopath + 'd', i, obj)
		obj = self.v.putTimedElt(path, cpopath + 'v', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.d.getTimedElt(path, cpopath + 'd', i, obj)
		self.v.getTimedElt(path, cpopath + 'v', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.d.putNonTimedElt(path, cpopath + 'd', i, obj)
		obj = self.v.putNonTimedElt(path, cpopath + 'v', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.d.getNonTimedElt(path, cpopath + 'd', i, obj)
		self.v.getNonTimedElt(path, cpopath + 'v', i, obj)


class dstructurecomplexgrid_vector_simplestruct(KeepInOrder):
	'''
	class dstructurecomplexgrid_vector_simplestruct
	Diffusivity [m^2/s]; Time-dependent;

	Attributes:
	- label : str
	   Label describing the data
	- comp : class compstruct_arraycomplexgrid_scalar: array of compstruct_arraycomplexgrid_scalarObj objects
	   Components of the vector. Vector of griddata(ndim). Time-dependent; FIXME: inherit time-dependence for this element
	- align : numpy.ndarray 1D with int)
	   Alignment of vector components, numerical flag. Int vector(ndim)
	- alignid : list of str
	   Alignment of vector components, string description. String vector(ndim)
	'''

	def __init__(self, base_path_in='d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.label = ''
		self.comp = compstruct_arraycomplexgrid_scalar('comp')
		self.align = numpy.zeros(0, numpy.int32, order='C')
		self.alignid = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class dstructurecomplexgrid_vector_simplestruct\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		ret = ret + space + 'Attribute comp\n ' + self.comp.__str__(depth+1)
		s = self.align.__str__()
		ret = ret + space + 'Attribute align\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.alignid.__str__()
		ret = ret + space + 'Attribute alignid\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.comp.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dstructurecomplexgrid_vector_simplestruct, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.comp.cpoTime = self.cpoTime
		self.comp.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dstructurecomplexgrid_vector_simplestruct, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.comp.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dstructurecomplexgrid_vector_simplestruct, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'label', self.label)
		check_status(status)
		self.comp.putNonTimed(path, cpopath)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'align', numpy.array(self.align).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'alignid', self.alignid, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type dstructurecomplexgrid_vector_simplestruct, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_label = ull.getString(self.idx, path, cpopath + 'label')
		check_status(status)
		if not status:
			self.label = ret_label
		self.comp.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_align = ull.getVect1DInt(self.idx, path, cpopath + 'align')
		check_status(status)
		if not status:
			self.align = ret_align
		status, ret_alignid = ull.getVect1DString(self.idx, path, cpopath + 'alignid')
		check_status(status)
		if not status:
			self.alignid = ret_alignid

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type dstructurecomplexgrid_vector_simplestruct, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, labelVal = ull.getString(self.idx, path, cpopath + 'label')
			check_status(status)
			compList = self.comp.build_non_resampled_data(path, cpopath, nbslice)
			status, alignVal = ull.getVect1DInt(self.idx, path, cpopath + 'align')
			check_status(status)
			status, alignidVal = ull.getVect1DString(self.idx, path, cpopath + 'alignid')
			check_status(status)
			for i in range(nbslice):
				slice = dstructurecomplexgrid_vector_simplestruct(self.base_path)
				slice.setExpIdx(self.idx)
				slice.label = labelVal
				slice.comp = compList[i]
				slice.align = alignVal
				slice.alignid = alignidVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dstructurecomplexgrid_vector_simplestructObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.comp.putTimedElt(path, cpopath + 'comp', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dstructurecomplexgrid_vector_simplestructObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.comp.getTimedElt(path, cpopath + 'comp', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dstructurecomplexgrid_vector_simplestructObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
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
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dstructurecomplexgrid_vector_simplestructObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
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

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'label')
		ull.deleteData(self.idx, path, cpopath + 'comp')
		ull.deleteData(self.idx, path, cpopath + 'align')
		ull.deleteData(self.idx, path, cpopath + 'alignid')


class vstructurecomplexgrid_vector_simplestruct(KeepInOrder):
	'''
	class vstructurecomplexgrid_vector_simplestruct
	Velocity [m/s]; Time-dependent;

	Attributes:
	- label : str
	   Label describing the data
	- comp : class compstruct_arraycomplexgrid_scalar: array of compstruct_arraycomplexgrid_scalarObj objects
	   Components of the vector. Vector of griddata(ndim). Time-dependent; FIXME: inherit time-dependence for this element
	- align : numpy.ndarray 1D with int)
	   Alignment of vector components, numerical flag. Int vector(ndim)
	- alignid : list of str
	   Alignment of vector components, string description. String vector(ndim)
	'''

	def __init__(self, base_path_in='v'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.label = ''
		self.comp = compstruct_arraycomplexgrid_scalar('comp')
		self.align = numpy.zeros(0, numpy.int32, order='C')
		self.alignid = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class vstructurecomplexgrid_vector_simplestruct\n'
		ret = ret + space + 'Attribute label: ' + str(self.label) + '\n'
		ret = ret + space + 'Attribute comp\n ' + self.comp.__str__(depth+1)
		s = self.align.__str__()
		ret = ret + space + 'Attribute align\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.alignid.__str__()
		ret = ret + space + 'Attribute alignid\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.comp.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vstructurecomplexgrid_vector_simplestruct, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.comp.cpoTime = self.cpoTime
		self.comp.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vstructurecomplexgrid_vector_simplestruct, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.comp.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vstructurecomplexgrid_vector_simplestruct, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'label', self.label)
		check_status(status)
		self.comp.putNonTimed(path, cpopath)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'align', numpy.array(self.align).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'alignid', self.alignid, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type vstructurecomplexgrid_vector_simplestruct, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_label = ull.getString(self.idx, path, cpopath + 'label')
		check_status(status)
		if not status:
			self.label = ret_label
		self.comp.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_align = ull.getVect1DInt(self.idx, path, cpopath + 'align')
		check_status(status)
		if not status:
			self.align = ret_align
		status, ret_alignid = ull.getVect1DString(self.idx, path, cpopath + 'alignid')
		check_status(status)
		if not status:
			self.alignid = ret_alignid

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type vstructurecomplexgrid_vector_simplestruct, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, labelVal = ull.getString(self.idx, path, cpopath + 'label')
			check_status(status)
			compList = self.comp.build_non_resampled_data(path, cpopath, nbslice)
			status, alignVal = ull.getVect1DInt(self.idx, path, cpopath + 'align')
			check_status(status)
			status, alignidVal = ull.getVect1DString(self.idx, path, cpopath + 'alignid')
			check_status(status)
			for i in range(nbslice):
				slice = vstructurecomplexgrid_vector_simplestruct(self.base_path)
				slice.setExpIdx(self.idx)
				slice.label = labelVal
				slice.comp = compList[i]
				slice.align = alignVal
				slice.alignid = alignidVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vstructurecomplexgrid_vector_simplestructObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.comp.putTimedElt(path, cpopath + 'comp', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vstructurecomplexgrid_vector_simplestructObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.comp.getTimedElt(path, cpopath + 'comp', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vstructurecomplexgrid_vector_simplestructObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
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
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vstructurecomplexgrid_vector_simplestructObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
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

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'label')
		ull.deleteData(self.idx, path, cpopath + 'comp')
		ull.deleteData(self.idx, path, cpopath + 'align')
		ull.deleteData(self.idx, path, cpopath + 'alignid')


class sourcestruct_arraycomplexgrid_scalar:
	'''
	class sourcestruct_arraycomplexgrid_scalar
	Source; Time-dependent; Array of structures (nsubgrid_quantity)

	Attributes:
	- array : list of sourcestruct_arraycomplexgrid_scalarObj 
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
		ret = space + 'class sourcestruct_arraycomplexgrid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'sourcestruct_arraycomplexgrid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(sourcestruct_arraycomplexgrid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type sourcestruct_arraycomplexgrid_scalar, run function putSlice') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraycomplexgrid_scalar, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraycomplexgrid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraycomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraycomplexgrid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(sourcestruct_arraycomplexgrid_scalar(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(sourcestruct_arraycomplexgrid_scalar(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = sourcestruct_arraycomplexgrid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type sourcestruct_arraycomplexgrid_scalar, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraycomplexgrid_scalar, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraycomplexgrid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type sourcestruct_arraycomplexgrid_scalar, run function getNonTimedElt') 
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


class sourcestruct_arraycomplexgrid_scalarObj(KeepInOrder):
	'''
	class sourcestruct_arraycomplexgrid_scalarObj
	Source; Time-dependent; Array of structures (nsubgrid_quantity)

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

	def __init__(self, base_path_in='source'):
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
		ret = space + 'class sourcestruct_arraycomplexgrid_scalarObj\n'
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
			print ('object of type sourcestruct_arraycomplexgrid_scalarObj, run function putTimedElt') 
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
			print ('object of type sourcestruct_arraycomplexgrid_scalarObj, run function getTimedElt') 
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
			print ('object of type sourcestruct_arraycomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcestruct_arraycomplexgrid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class nistruct_arrayedge_fluid_scalar:
	'''
	class nistruct_arrayedge_fluid_scalar
	Ion density [1/m^3] (per species). Array of structures(nspecies); Time-dependent;

	Attributes:
	- array : list of nistruct_arrayedge_fluid_scalarObj 
	'''

	def __init__(self, base_path_in='ni'):
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
		ret = space + 'class nistruct_arrayedge_fluid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'nistruct_arrayedge_fluid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(nistruct_arrayedge_fluid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nistruct_arrayedge_fluid_scalar, run function putSlice') 
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
			print ('field '+self.base_path+' of type nistruct_arrayedge_fluid_scalar, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type nistruct_arrayedge_fluid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type nistruct_arrayedge_fluid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type nistruct_arrayedge_fluid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(nistruct_arrayedge_fluid_scalar(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(nistruct_arrayedge_fluid_scalar(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = nistruct_arrayedge_fluid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type nistruct_arrayedge_fluid_scalar, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type nistruct_arrayedge_fluid_scalar, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type nistruct_arrayedge_fluid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type nistruct_arrayedge_fluid_scalar, run function getNonTimedElt') 
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


class nistruct_arrayedge_fluid_scalarObj(KeepInOrder):
	'''
	class nistruct_arrayedge_fluid_scalarObj
	Ion density [1/m^3] (per species). Array of structures(nspecies); Time-dependent;

	Attributes:
	- value : class valuestruct_arraycomplexgrid_scalar: array of valuestruct_arraycomplexgrid_scalarObj objects
	   Value of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- bndvalue : class bndvaluestruct_arraycomplexgrid_scalar: array of bndvaluestruct_arraycomplexgrid_scalarObj objects
	   Boundary values of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- flux : class fluxstruct_arraycomplexgrid_vector: array of fluxstruct_arraycomplexgrid_vectorObj objects
	   Flux of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- bndflux : class bndfluxstruct_arraycomplexgrid_vector: array of bndfluxstruct_arraycomplexgrid_vectorObj objects
	   Flux of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- transpcoeff : class transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff: array of transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj objects
	   Transport coefficients; Time-dependent; Array of structures (nsubgrid_quantity)
	- source : class sourcestruct_arraycomplexgrid_scalar: array of sourcestruct_arraycomplexgrid_scalarObj objects
	   Source; Time-dependent; Array of structures (nsubgrid_quantity)
	'''

	def __init__(self, base_path_in='ni'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = valuestruct_arraycomplexgrid_scalar('value')
		self.bndvalue = bndvaluestruct_arraycomplexgrid_scalar('bndvalue')
		self.flux = fluxstruct_arraycomplexgrid_vector('flux')
		self.bndflux = bndfluxstruct_arraycomplexgrid_vector('bndflux')
		self.transpcoeff = transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff('transpcoeff')
		self.source = sourcestruct_arraycomplexgrid_scalar('source')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nistruct_arrayedge_fluid_scalarObj\n'
		ret = ret + space + 'Attribute value\n ' + self.value.__str__(depth+1)
		ret = ret + space + 'Attribute bndvalue\n ' + self.bndvalue.__str__(depth+1)
		ret = ret + space + 'Attribute flux\n ' + self.flux.__str__(depth+1)
		ret = ret + space + 'Attribute bndflux\n ' + self.bndflux.__str__(depth+1)
		ret = ret + space + 'Attribute transpcoeff\n ' + self.transpcoeff.__str__(depth+1)
		ret = ret + space + 'Attribute source\n ' + self.source.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.value.setExpIdx(idx)
		self.bndvalue.setExpIdx(idx)
		self.flux.setExpIdx(idx)
		self.bndflux.setExpIdx(idx)
		self.transpcoeff.setExpIdx(idx)
		self.source.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nistruct_arrayedge_fluid_scalarObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.value.putTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.flux.putTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.bndflux.putTimedElt(path, cpopath + 'bndflux', i, obj)
		obj = self.transpcoeff.putTimedElt(path, cpopath + 'transpcoeff', i, obj)
		obj = self.source.putTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nistruct_arrayedge_fluid_scalarObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.value.getTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.flux.getTimedElt(path, cpopath + 'flux', i, obj)
		self.bndflux.getTimedElt(path, cpopath + 'bndflux', i, obj)
		self.transpcoeff.getTimedElt(path, cpopath + 'transpcoeff', i, obj)
		self.source.getTimedElt(path, cpopath + 'source', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nistruct_arrayedge_fluid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.value.putNonTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.flux.putNonTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.bndflux.putNonTimedElt(path, cpopath + 'bndflux', i, obj)
		obj = self.transpcoeff.putNonTimedElt(path, cpopath + 'transpcoeff', i, obj)
		obj = self.source.putNonTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nistruct_arrayedge_fluid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.value.getNonTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.flux.getNonTimedElt(path, cpopath + 'flux', i, obj)
		self.bndflux.getNonTimedElt(path, cpopath + 'bndflux', i, obj)
		self.transpcoeff.getNonTimedElt(path, cpopath + 'transpcoeff', i, obj)
		self.source.getNonTimedElt(path, cpopath + 'source', i, obj)


class vestructureedge_fluid_vector_simplestruct(KeepInOrder):
	'''
	class vestructureedge_fluid_vector_simplestruct
	Electron velocity [m/s]; Time-dependent;

	Attributes:
	- griduid : int
	   Unique identifier of the grid this vector quantity is associated with.
	- basis : int
	   Index of basis (defined in associated grid) this vector is aligned to; If set to GRID_UNDEFINED=0, the canonical basis of the default coordinates of the grid assumed.
	- comps : class compsstruct_arrayedge_fluid_scalar: array of compsstruct_arrayedge_fluid_scalarObj objects
	   Components of the vector. Array of structures(ndim); Time-dependent;
	- align : numpy.ndarray 1D with int)
	   Alignment of vector components, numerical flag. Int vector(ndim);
	- alignid : list of str
	   Alignment of vector components, string description. String vector(ndim);
	'''

	def __init__(self, base_path_in='ve'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.basis = EMPTY_INT
		self.comps = compsstruct_arrayedge_fluid_scalar('comps')
		self.align = numpy.zeros(0, numpy.int32, order='C')
		self.alignid = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class vestructureedge_fluid_vector_simplestruct\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute basis: ' + str(self.basis) + '\n'
		ret = ret + space + 'Attribute comps\n ' + self.comps.__str__(depth+1)
		s = self.align.__str__()
		ret = ret + space + 'Attribute align\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.alignid.__str__()
		ret = ret + space + 'Attribute alignid\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.comps.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vestructureedge_fluid_vector_simplestruct, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.comps.cpoTime = self.cpoTime
		self.comps.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vestructureedge_fluid_vector_simplestruct, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.comps.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vestructureedge_fluid_vector_simplestruct, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'basis', self.basis)
		check_status(status)
		self.comps.putNonTimed(path, cpopath)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'align', numpy.array(self.align).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'alignid', self.alignid, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type vestructureedge_fluid_vector_simplestruct, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid = ull.getInt(self.idx, path, cpopath + 'griduid')
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		status, ret_basis = ull.getInt(self.idx, path, cpopath + 'basis')
		check_status(status)
		if not status:
			self.basis = ret_basis
		self.comps.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_align = ull.getVect1DInt(self.idx, path, cpopath + 'align')
		check_status(status)
		if not status:
			self.align = ret_align
		status, ret_alignid = ull.getVect1DString(self.idx, path, cpopath + 'alignid')
		check_status(status)
		if not status:
			self.alignid = ret_alignid

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type vestructureedge_fluid_vector_simplestruct, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidVal = ull.getInt(self.idx, path, cpopath + 'griduid')
			check_status(status)
			status, basisVal = ull.getInt(self.idx, path, cpopath + 'basis')
			check_status(status)
			compsList = self.comps.build_non_resampled_data(path, cpopath, nbslice)
			status, alignVal = ull.getVect1DInt(self.idx, path, cpopath + 'align')
			check_status(status)
			status, alignidVal = ull.getVect1DString(self.idx, path, cpopath + 'alignid')
			check_status(status)
			for i in range(nbslice):
				slice = vestructureedge_fluid_vector_simplestruct(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = griduidVal
				slice.basis = basisVal
				slice.comps = compsList[i]
				slice.align = alignVal
				slice.alignid = alignidVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vestructureedge_fluid_vector_simplestructObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.comps.putTimedElt(path, cpopath + 'comps', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vestructureedge_fluid_vector_simplestructObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.comps.getTimedElt(path, cpopath + 'comps', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vestructureedge_fluid_vector_simplestructObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'basis', i, self.basis)
		obj = self.comps.putNonTimedElt(path, cpopath + 'comps', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'align') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'align', i, numpy.array(self.align).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'alignid') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'alignid', i, self.alignid)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vestructureedge_fluid_vector_simplestructObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		status, ret_basis = ull.getIntFromObject(self.idx, obj, cpopath + 'basis', i)
		check_status(status)
		if not status:
			self.basis = ret_basis
		self.comps.getNonTimedElt(path, cpopath + 'comps', i, obj)
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

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'basis')
		ull.deleteData(self.idx, path, cpopath + 'comps')
		ull.deleteData(self.idx, path, cpopath + 'align')
		ull.deleteData(self.idx, path, cpopath + 'alignid')


class compsstruct_arrayedge_fluid_scalar:
	'''
	class compsstruct_arrayedge_fluid_scalar
	Components of the vector. Array of structures(ndim); Time-dependent;

	Attributes:
	- array : list of compsstruct_arrayedge_fluid_scalarObj 
	'''

	def __init__(self, base_path_in='comps'):
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
		ret = space + 'class compsstruct_arrayedge_fluid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'compsstruct_arrayedge_fluid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(compsstruct_arrayedge_fluid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type compsstruct_arrayedge_fluid_scalar, run function putSlice') 
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
			print ('field '+self.base_path+' of type compsstruct_arrayedge_fluid_scalar, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type compsstruct_arrayedge_fluid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type compsstruct_arrayedge_fluid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type compsstruct_arrayedge_fluid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(compsstruct_arrayedge_fluid_scalar(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(compsstruct_arrayedge_fluid_scalar(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = compsstruct_arrayedge_fluid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type compsstruct_arrayedge_fluid_scalar, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type compsstruct_arrayedge_fluid_scalar, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type compsstruct_arrayedge_fluid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type compsstruct_arrayedge_fluid_scalar, run function getNonTimedElt') 
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


class compsstruct_arrayedge_fluid_scalarObj(KeepInOrder):
	'''
	class compsstruct_arrayedge_fluid_scalarObj
	Components of the vector. Array of structures(ndim); Time-dependent;

	Attributes:
	- value : class valuestruct_arraycomplexgrid_scalar: array of valuestruct_arraycomplexgrid_scalarObj objects
	   Value of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- bndvalue : class bndvaluestruct_arraycomplexgrid_scalar: array of bndvaluestruct_arraycomplexgrid_scalarObj objects
	   Boundary values of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- flux : class fluxstruct_arraycomplexgrid_vector: array of fluxstruct_arraycomplexgrid_vectorObj objects
	   Flux of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- bndflux : class bndfluxstruct_arraycomplexgrid_vector: array of bndfluxstruct_arraycomplexgrid_vectorObj objects
	   Flux of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- transpcoeff : class transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff: array of transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj objects
	   Transport coefficients; Time-dependent; Array of structures (nsubgrid_quantity)
	- source : class sourcestruct_arraycomplexgrid_scalar: array of sourcestruct_arraycomplexgrid_scalarObj objects
	   Source; Time-dependent; Array of structures (nsubgrid_quantity)
	'''

	def __init__(self, base_path_in='comps'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = valuestruct_arraycomplexgrid_scalar('value')
		self.bndvalue = bndvaluestruct_arraycomplexgrid_scalar('bndvalue')
		self.flux = fluxstruct_arraycomplexgrid_vector('flux')
		self.bndflux = bndfluxstruct_arraycomplexgrid_vector('bndflux')
		self.transpcoeff = transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff('transpcoeff')
		self.source = sourcestruct_arraycomplexgrid_scalar('source')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class compsstruct_arrayedge_fluid_scalarObj\n'
		ret = ret + space + 'Attribute value\n ' + self.value.__str__(depth+1)
		ret = ret + space + 'Attribute bndvalue\n ' + self.bndvalue.__str__(depth+1)
		ret = ret + space + 'Attribute flux\n ' + self.flux.__str__(depth+1)
		ret = ret + space + 'Attribute bndflux\n ' + self.bndflux.__str__(depth+1)
		ret = ret + space + 'Attribute transpcoeff\n ' + self.transpcoeff.__str__(depth+1)
		ret = ret + space + 'Attribute source\n ' + self.source.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.value.setExpIdx(idx)
		self.bndvalue.setExpIdx(idx)
		self.flux.setExpIdx(idx)
		self.bndflux.setExpIdx(idx)
		self.transpcoeff.setExpIdx(idx)
		self.source.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compsstruct_arrayedge_fluid_scalarObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.value.putTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.flux.putTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.bndflux.putTimedElt(path, cpopath + 'bndflux', i, obj)
		obj = self.transpcoeff.putTimedElt(path, cpopath + 'transpcoeff', i, obj)
		obj = self.source.putTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compsstruct_arrayedge_fluid_scalarObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.value.getTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.flux.getTimedElt(path, cpopath + 'flux', i, obj)
		self.bndflux.getTimedElt(path, cpopath + 'bndflux', i, obj)
		self.transpcoeff.getTimedElt(path, cpopath + 'transpcoeff', i, obj)
		self.source.getTimedElt(path, cpopath + 'source', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compsstruct_arrayedge_fluid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.value.putNonTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.flux.putNonTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.bndflux.putNonTimedElt(path, cpopath + 'bndflux', i, obj)
		obj = self.transpcoeff.putNonTimedElt(path, cpopath + 'transpcoeff', i, obj)
		obj = self.source.putNonTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type compsstruct_arrayedge_fluid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.value.getNonTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.flux.getNonTimedElt(path, cpopath + 'flux', i, obj)
		self.bndflux.getNonTimedElt(path, cpopath + 'bndflux', i, obj)
		self.transpcoeff.getNonTimedElt(path, cpopath + 'transpcoeff', i, obj)
		self.source.getNonTimedElt(path, cpopath + 'source', i, obj)


class vistruct_arrayedge_fluid_vector:
	'''
	class vistruct_arrayedge_fluid_vector
	Ion velocity [m/s] (per species).Array of structures(nspecies); Time-dependent;

	Attributes:
	- array : list of vistruct_arrayedge_fluid_vectorObj 
	'''

	def __init__(self, base_path_in='vi'):
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
		ret = space + 'class vistruct_arrayedge_fluid_vector\n'
		for i in range(len(self.array)):
			ret = ret + space + 'vistruct_arrayedge_fluid_vector[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(vistruct_arrayedge_fluid_vectorObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type vistruct_arrayedge_fluid_vector, run function putSlice') 
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
			print ('field '+self.base_path+' of type vistruct_arrayedge_fluid_vector, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type vistruct_arrayedge_fluid_vector, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type vistruct_arrayedge_fluid_vector, run function getSlice') 
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
			print ('field '+self.base_path+' of type vistruct_arrayedge_fluid_vector, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(vistruct_arrayedge_fluid_vector(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(vistruct_arrayedge_fluid_vector(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = vistruct_arrayedge_fluid_vector(self.base_path)
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
			print ('field '+self.base_path+' of type vistruct_arrayedge_fluid_vector, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type vistruct_arrayedge_fluid_vector, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type vistruct_arrayedge_fluid_vector, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type vistruct_arrayedge_fluid_vector, run function getNonTimedElt') 
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


class vistruct_arrayedge_fluid_vectorObj(KeepInOrder):
	'''
	class vistruct_arrayedge_fluid_vectorObj
	Ion velocity [m/s] (per species).Array of structures(nspecies); Time-dependent;

	Attributes:
	- griduid : int
	   Unique identifier of the grid this vector quantity is associated with.
	- basis : int
	   Index of basis (defined in associated grid) this vector is aligned to; If set to GRID_UNDEFINED=0, the canonical basis of the default coordinates of the grid assumed.
	- align : numpy.ndarray 1D with int)
	   Alignment of vector components, numerical flag. Int vector (number of vector components);
	- alignid : list of str
	   Alignment of vector components, string description. String vector (number of vector components);
	- comps : class compsstruct_arrayedge_fluid_scalar: array of compsstruct_arrayedge_fluid_scalarObj objects
	   Components of the vector. Array of structures (number of vector components); Time-dependent;
	'''

	def __init__(self, base_path_in='vi'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.basis = EMPTY_INT
		self.align = numpy.zeros(0, numpy.int32, order='C')
		self.alignid = ['']
		self.comps = compsstruct_arrayedge_fluid_scalar('comps')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class vistruct_arrayedge_fluid_vectorObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute basis: ' + str(self.basis) + '\n'
		s = self.align.__str__()
		ret = ret + space + 'Attribute align\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.alignid.__str__()
		ret = ret + space + 'Attribute alignid\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute comps\n ' + self.comps.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.comps.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vistruct_arrayedge_fluid_vectorObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.comps.putTimedElt(path, cpopath + 'comps', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vistruct_arrayedge_fluid_vectorObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.comps.getTimedElt(path, cpopath + 'comps', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vistruct_arrayedge_fluid_vectorObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'basis', i, self.basis)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'align') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'align', i, numpy.array(self.align).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'alignid') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'alignid', i, self.alignid)
		obj = self.comps.putNonTimedElt(path, cpopath + 'comps', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type vistruct_arrayedge_fluid_vectorObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		status, ret_basis = ull.getIntFromObject(self.idx, obj, cpopath + 'basis', i)
		check_status(status)
		if not status:
			self.basis = ret_basis
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
		self.comps.getNonTimedElt(path, cpopath + 'comps', i, obj)


class testructureedge_fluid_scalar_simplestruct(KeepInOrder):
	'''
	class testructureedge_fluid_scalar_simplestruct
	Electron temperature [eV]; Time-dependent;

	Attributes:
	- value : class valuestruct_arraycomplexgrid_scalar: array of valuestruct_arraycomplexgrid_scalarObj objects
	   Value of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- bndvalue : class bndvaluestruct_arraycomplexgrid_scalar: array of bndvaluestruct_arraycomplexgrid_scalarObj objects
	   Boundary values of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- flux : class fluxstruct_arraycomplexgrid_vector: array of fluxstruct_arraycomplexgrid_vectorObj objects
	   Flux of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- bndflux : class bndfluxstruct_arraycomplexgrid_vector: array of bndfluxstruct_arraycomplexgrid_vectorObj objects
	   Flux of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- transpcoeff : class transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff: array of transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj objects
	   Transport coefficients; Time-dependent; Array of structures (nsubgrid_quantity)
	- source : class sourcestruct_arraycomplexgrid_scalar: array of sourcestruct_arraycomplexgrid_scalarObj objects
	   Source; Time-dependent; Array of structures (nsubgrid_quantity)
	'''

	def __init__(self, base_path_in='te'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = valuestruct_arraycomplexgrid_scalar('value')
		self.bndvalue = bndvaluestruct_arraycomplexgrid_scalar('bndvalue')
		self.flux = fluxstruct_arraycomplexgrid_vector('flux')
		self.bndflux = bndfluxstruct_arraycomplexgrid_vector('bndflux')
		self.transpcoeff = transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff('transpcoeff')
		self.source = sourcestruct_arraycomplexgrid_scalar('source')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class testructureedge_fluid_scalar_simplestruct\n'
		ret = ret + space + 'Attribute value\n ' + self.value.__str__(depth+1)
		ret = ret + space + 'Attribute bndvalue\n ' + self.bndvalue.__str__(depth+1)
		ret = ret + space + 'Attribute flux\n ' + self.flux.__str__(depth+1)
		ret = ret + space + 'Attribute bndflux\n ' + self.bndflux.__str__(depth+1)
		ret = ret + space + 'Attribute transpcoeff\n ' + self.transpcoeff.__str__(depth+1)
		ret = ret + space + 'Attribute source\n ' + self.source.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.value.setExpIdx(idx)
		self.bndvalue.setExpIdx(idx)
		self.flux.setExpIdx(idx)
		self.bndflux.setExpIdx(idx)
		self.transpcoeff.setExpIdx(idx)
		self.source.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type testructureedge_fluid_scalar_simplestruct, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.value.cpoTime = self.cpoTime
		self.value.putSlice(path, cpopath)
		self.bndvalue.cpoTime = self.cpoTime
		self.bndvalue.putSlice(path, cpopath)
		self.flux.cpoTime = self.cpoTime
		self.flux.putSlice(path, cpopath)
		self.bndflux.cpoTime = self.cpoTime
		self.bndflux.putSlice(path, cpopath)
		self.transpcoeff.cpoTime = self.cpoTime
		self.transpcoeff.putSlice(path, cpopath)
		self.source.cpoTime = self.cpoTime
		self.source.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type testructureedge_fluid_scalar_simplestruct, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.value.replaceLastSlice(path, cpopath)
		self.bndvalue.replaceLastSlice(path, cpopath)
		self.flux.replaceLastSlice(path, cpopath)
		self.bndflux.replaceLastSlice(path, cpopath)
		self.transpcoeff.replaceLastSlice(path, cpopath)
		self.source.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type testructureedge_fluid_scalar_simplestruct, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.value.putNonTimed(path, cpopath)
		self.bndvalue.putNonTimed(path, cpopath)
		self.flux.putNonTimed(path, cpopath)
		self.bndflux.putNonTimed(path, cpopath)
		self.transpcoeff.putNonTimed(path, cpopath)
		self.source.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type testructureedge_fluid_scalar_simplestruct, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.value.getSlice(path, cpopath, inTime, interpolMode)
		self.bndvalue.getSlice(path, cpopath, inTime, interpolMode)
		self.flux.getSlice(path, cpopath, inTime, interpolMode)
		self.bndflux.getSlice(path, cpopath, inTime, interpolMode)
		self.transpcoeff.getSlice(path, cpopath, inTime, interpolMode)
		self.source.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type testructureedge_fluid_scalar_simplestruct, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			valueList = self.value.build_non_resampled_data(path, cpopath, nbslice)
			bndvalueList = self.bndvalue.build_non_resampled_data(path, cpopath, nbslice)
			fluxList = self.flux.build_non_resampled_data(path, cpopath, nbslice)
			bndfluxList = self.bndflux.build_non_resampled_data(path, cpopath, nbslice)
			transpcoeffList = self.transpcoeff.build_non_resampled_data(path, cpopath, nbslice)
			sourceList = self.source.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = testructureedge_fluid_scalar_simplestruct(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[i]
				slice.bndvalue = bndvalueList[i]
				slice.flux = fluxList[i]
				slice.bndflux = bndfluxList[i]
				slice.transpcoeff = transpcoeffList[i]
				slice.source = sourceList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type testructureedge_fluid_scalar_simplestructObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.value.putTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.flux.putTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.bndflux.putTimedElt(path, cpopath + 'bndflux', i, obj)
		obj = self.transpcoeff.putTimedElt(path, cpopath + 'transpcoeff', i, obj)
		obj = self.source.putTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type testructureedge_fluid_scalar_simplestructObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.value.getTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.flux.getTimedElt(path, cpopath + 'flux', i, obj)
		self.bndflux.getTimedElt(path, cpopath + 'bndflux', i, obj)
		self.transpcoeff.getTimedElt(path, cpopath + 'transpcoeff', i, obj)
		self.source.getTimedElt(path, cpopath + 'source', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type testructureedge_fluid_scalar_simplestructObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.value.putNonTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.flux.putNonTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.bndflux.putNonTimedElt(path, cpopath + 'bndflux', i, obj)
		obj = self.transpcoeff.putNonTimedElt(path, cpopath + 'transpcoeff', i, obj)
		obj = self.source.putNonTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type testructureedge_fluid_scalar_simplestructObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.value.getNonTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.flux.getNonTimedElt(path, cpopath + 'flux', i, obj)
		self.bndflux.getNonTimedElt(path, cpopath + 'bndflux', i, obj)
		self.transpcoeff.getNonTimedElt(path, cpopath + 'transpcoeff', i, obj)
		self.source.getNonTimedElt(path, cpopath + 'source', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'bndvalue')
		ull.deleteData(self.idx, path, cpopath + 'flux')
		ull.deleteData(self.idx, path, cpopath + 'bndflux')
		ull.deleteData(self.idx, path, cpopath + 'transpcoeff')
		ull.deleteData(self.idx, path, cpopath + 'source')


class tistruct_arrayedge_fluid_scalar:
	'''
	class tistruct_arrayedge_fluid_scalar
	Ion temperature [eV] (per species). Array of structures(nspecies).; Time-dependent;

	Attributes:
	- array : list of tistruct_arrayedge_fluid_scalarObj 
	'''

	def __init__(self, base_path_in='ti'):
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
		ret = space + 'class tistruct_arrayedge_fluid_scalar\n'
		for i in range(len(self.array)):
			ret = ret + space + 'tistruct_arrayedge_fluid_scalar[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(tistruct_arrayedge_fluid_scalarObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type tistruct_arrayedge_fluid_scalar, run function putSlice') 
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
			print ('field '+self.base_path+' of type tistruct_arrayedge_fluid_scalar, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type tistruct_arrayedge_fluid_scalar, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type tistruct_arrayedge_fluid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type tistruct_arrayedge_fluid_scalar, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(tistruct_arrayedge_fluid_scalar(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(tistruct_arrayedge_fluid_scalar(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = tistruct_arrayedge_fluid_scalar(self.base_path)
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
			print ('field '+self.base_path+' of type tistruct_arrayedge_fluid_scalar, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type tistruct_arrayedge_fluid_scalar, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type tistruct_arrayedge_fluid_scalar, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type tistruct_arrayedge_fluid_scalar, run function getNonTimedElt') 
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


class tistruct_arrayedge_fluid_scalarObj(KeepInOrder):
	'''
	class tistruct_arrayedge_fluid_scalarObj
	Ion temperature [eV] (per species). Array of structures(nspecies).; Time-dependent;

	Attributes:
	- value : class valuestruct_arraycomplexgrid_scalar: array of valuestruct_arraycomplexgrid_scalarObj objects
	   Value of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- bndvalue : class bndvaluestruct_arraycomplexgrid_scalar: array of bndvaluestruct_arraycomplexgrid_scalarObj objects
	   Boundary values of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- flux : class fluxstruct_arraycomplexgrid_vector: array of fluxstruct_arraycomplexgrid_vectorObj objects
	   Flux of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- bndflux : class bndfluxstruct_arraycomplexgrid_vector: array of bndfluxstruct_arraycomplexgrid_vectorObj objects
	   Flux of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- transpcoeff : class transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff: array of transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj objects
	   Transport coefficients; Time-dependent; Array of structures (nsubgrid_quantity)
	- source : class sourcestruct_arraycomplexgrid_scalar: array of sourcestruct_arraycomplexgrid_scalarObj objects
	   Source; Time-dependent; Array of structures (nsubgrid_quantity)
	'''

	def __init__(self, base_path_in='ti'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = valuestruct_arraycomplexgrid_scalar('value')
		self.bndvalue = bndvaluestruct_arraycomplexgrid_scalar('bndvalue')
		self.flux = fluxstruct_arraycomplexgrid_vector('flux')
		self.bndflux = bndfluxstruct_arraycomplexgrid_vector('bndflux')
		self.transpcoeff = transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff('transpcoeff')
		self.source = sourcestruct_arraycomplexgrid_scalar('source')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class tistruct_arrayedge_fluid_scalarObj\n'
		ret = ret + space + 'Attribute value\n ' + self.value.__str__(depth+1)
		ret = ret + space + 'Attribute bndvalue\n ' + self.bndvalue.__str__(depth+1)
		ret = ret + space + 'Attribute flux\n ' + self.flux.__str__(depth+1)
		ret = ret + space + 'Attribute bndflux\n ' + self.bndflux.__str__(depth+1)
		ret = ret + space + 'Attribute transpcoeff\n ' + self.transpcoeff.__str__(depth+1)
		ret = ret + space + 'Attribute source\n ' + self.source.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.value.setExpIdx(idx)
		self.bndvalue.setExpIdx(idx)
		self.flux.setExpIdx(idx)
		self.bndflux.setExpIdx(idx)
		self.transpcoeff.setExpIdx(idx)
		self.source.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tistruct_arrayedge_fluid_scalarObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.value.putTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.flux.putTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.bndflux.putTimedElt(path, cpopath + 'bndflux', i, obj)
		obj = self.transpcoeff.putTimedElt(path, cpopath + 'transpcoeff', i, obj)
		obj = self.source.putTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tistruct_arrayedge_fluid_scalarObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.value.getTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.flux.getTimedElt(path, cpopath + 'flux', i, obj)
		self.bndflux.getTimedElt(path, cpopath + 'bndflux', i, obj)
		self.transpcoeff.getTimedElt(path, cpopath + 'transpcoeff', i, obj)
		self.source.getTimedElt(path, cpopath + 'source', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tistruct_arrayedge_fluid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.value.putNonTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.flux.putNonTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.bndflux.putNonTimedElt(path, cpopath + 'bndflux', i, obj)
		obj = self.transpcoeff.putNonTimedElt(path, cpopath + 'transpcoeff', i, obj)
		obj = self.source.putNonTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type tistruct_arrayedge_fluid_scalarObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.value.getNonTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.flux.getNonTimedElt(path, cpopath + 'flux', i, obj)
		self.bndflux.getNonTimedElt(path, cpopath + 'bndflux', i, obj)
		self.transpcoeff.getNonTimedElt(path, cpopath + 'transpcoeff', i, obj)
		self.source.getNonTimedElt(path, cpopath + 'source', i, obj)


class te_anisostructureedge_fluid_vector_simplestruct(KeepInOrder):
	'''
	class te_anisostructureedge_fluid_vector_simplestruct
	Anisotropic electron temperature [eV]; Time-dependent;

	Attributes:
	- griduid : int
	   Unique identifier of the grid this vector quantity is associated with.
	- basis : int
	   Index of basis (defined in associated grid) this vector is aligned to; If set to GRID_UNDEFINED=0, the canonical basis of the default coordinates of the grid assumed.
	- comps : class compsstruct_arrayedge_fluid_scalar: array of compsstruct_arrayedge_fluid_scalarObj objects
	   Components of the vector. Array of structures(ndim); Time-dependent;
	- align : numpy.ndarray 1D with int)
	   Alignment of vector components, numerical flag. Int vector(ndim);
	- alignid : list of str
	   Alignment of vector components, string description. String vector(ndim);
	'''

	def __init__(self, base_path_in='te_aniso'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.basis = EMPTY_INT
		self.comps = compsstruct_arrayedge_fluid_scalar('comps')
		self.align = numpy.zeros(0, numpy.int32, order='C')
		self.alignid = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class te_anisostructureedge_fluid_vector_simplestruct\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute basis: ' + str(self.basis) + '\n'
		ret = ret + space + 'Attribute comps\n ' + self.comps.__str__(depth+1)
		s = self.align.__str__()
		ret = ret + space + 'Attribute align\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.alignid.__str__()
		ret = ret + space + 'Attribute alignid\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.comps.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type te_anisostructureedge_fluid_vector_simplestruct, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.comps.cpoTime = self.cpoTime
		self.comps.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type te_anisostructureedge_fluid_vector_simplestruct, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.comps.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type te_anisostructureedge_fluid_vector_simplestruct, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'basis', self.basis)
		check_status(status)
		self.comps.putNonTimed(path, cpopath)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'align', numpy.array(self.align).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'alignid', self.alignid, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type te_anisostructureedge_fluid_vector_simplestruct, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid = ull.getInt(self.idx, path, cpopath + 'griduid')
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		status, ret_basis = ull.getInt(self.idx, path, cpopath + 'basis')
		check_status(status)
		if not status:
			self.basis = ret_basis
		self.comps.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_align = ull.getVect1DInt(self.idx, path, cpopath + 'align')
		check_status(status)
		if not status:
			self.align = ret_align
		status, ret_alignid = ull.getVect1DString(self.idx, path, cpopath + 'alignid')
		check_status(status)
		if not status:
			self.alignid = ret_alignid

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type te_anisostructureedge_fluid_vector_simplestruct, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidVal = ull.getInt(self.idx, path, cpopath + 'griduid')
			check_status(status)
			status, basisVal = ull.getInt(self.idx, path, cpopath + 'basis')
			check_status(status)
			compsList = self.comps.build_non_resampled_data(path, cpopath, nbslice)
			status, alignVal = ull.getVect1DInt(self.idx, path, cpopath + 'align')
			check_status(status)
			status, alignidVal = ull.getVect1DString(self.idx, path, cpopath + 'alignid')
			check_status(status)
			for i in range(nbslice):
				slice = te_anisostructureedge_fluid_vector_simplestruct(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = griduidVal
				slice.basis = basisVal
				slice.comps = compsList[i]
				slice.align = alignVal
				slice.alignid = alignidVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type te_anisostructureedge_fluid_vector_simplestructObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.comps.putTimedElt(path, cpopath + 'comps', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type te_anisostructureedge_fluid_vector_simplestructObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.comps.getTimedElt(path, cpopath + 'comps', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type te_anisostructureedge_fluid_vector_simplestructObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'basis', i, self.basis)
		obj = self.comps.putNonTimedElt(path, cpopath + 'comps', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'align') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'align', i, numpy.array(self.align).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'alignid') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'alignid', i, self.alignid)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type te_anisostructureedge_fluid_vector_simplestructObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		status, ret_basis = ull.getIntFromObject(self.idx, obj, cpopath + 'basis', i)
		check_status(status)
		if not status:
			self.basis = ret_basis
		self.comps.getNonTimedElt(path, cpopath + 'comps', i, obj)
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

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'basis')
		ull.deleteData(self.idx, path, cpopath + 'comps')
		ull.deleteData(self.idx, path, cpopath + 'align')
		ull.deleteData(self.idx, path, cpopath + 'alignid')


class ti_anisostruct_arrayedge_fluid_vector:
	'''
	class ti_anisostruct_arrayedge_fluid_vector
	Anisotropic ion temperature [eV] (per species). Array of structures(nspecies); Time-dependent;

	Attributes:
	- array : list of ti_anisostruct_arrayedge_fluid_vectorObj 
	'''

	def __init__(self, base_path_in='ti_aniso'):
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
		ret = space + 'class ti_anisostruct_arrayedge_fluid_vector\n'
		for i in range(len(self.array)):
			ret = ret + space + 'ti_anisostruct_arrayedge_fluid_vector[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(ti_anisostruct_arrayedge_fluid_vectorObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type ti_anisostruct_arrayedge_fluid_vector, run function putSlice') 
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
			print ('field '+self.base_path+' of type ti_anisostruct_arrayedge_fluid_vector, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type ti_anisostruct_arrayedge_fluid_vector, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type ti_anisostruct_arrayedge_fluid_vector, run function getSlice') 
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
			print ('field '+self.base_path+' of type ti_anisostruct_arrayedge_fluid_vector, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(ti_anisostruct_arrayedge_fluid_vector(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(ti_anisostruct_arrayedge_fluid_vector(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = ti_anisostruct_arrayedge_fluid_vector(self.base_path)
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
			print ('field '+self.base_path+' of type ti_anisostruct_arrayedge_fluid_vector, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type ti_anisostruct_arrayedge_fluid_vector, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type ti_anisostruct_arrayedge_fluid_vector, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type ti_anisostruct_arrayedge_fluid_vector, run function getNonTimedElt') 
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


class ti_anisostruct_arrayedge_fluid_vectorObj(KeepInOrder):
	'''
	class ti_anisostruct_arrayedge_fluid_vectorObj
	Anisotropic ion temperature [eV] (per species). Array of structures(nspecies); Time-dependent;

	Attributes:
	- griduid : int
	   Unique identifier of the grid this vector quantity is associated with.
	- basis : int
	   Index of basis (defined in associated grid) this vector is aligned to; If set to GRID_UNDEFINED=0, the canonical basis of the default coordinates of the grid assumed.
	- align : numpy.ndarray 1D with int)
	   Alignment of vector components, numerical flag. Int vector (number of vector components);
	- alignid : list of str
	   Alignment of vector components, string description. String vector (number of vector components);
	- comps : class compsstruct_arrayedge_fluid_scalar: array of compsstruct_arrayedge_fluid_scalarObj objects
	   Components of the vector. Array of structures (number of vector components); Time-dependent;
	'''

	def __init__(self, base_path_in='ti_aniso'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.basis = EMPTY_INT
		self.align = numpy.zeros(0, numpy.int32, order='C')
		self.alignid = ['']
		self.comps = compsstruct_arrayedge_fluid_scalar('comps')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class ti_anisostruct_arrayedge_fluid_vectorObj\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute basis: ' + str(self.basis) + '\n'
		s = self.align.__str__()
		ret = ret + space + 'Attribute align\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.alignid.__str__()
		ret = ret + space + 'Attribute alignid\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute comps\n ' + self.comps.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.comps.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ti_anisostruct_arrayedge_fluid_vectorObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.comps.putTimedElt(path, cpopath + 'comps', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ti_anisostruct_arrayedge_fluid_vectorObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.comps.getTimedElt(path, cpopath + 'comps', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ti_anisostruct_arrayedge_fluid_vectorObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'basis', i, self.basis)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'align') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'align', i, numpy.array(self.align).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'alignid') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'alignid', i, self.alignid)
		obj = self.comps.putNonTimedElt(path, cpopath + 'comps', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type ti_anisostruct_arrayedge_fluid_vectorObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		status, ret_basis = ull.getIntFromObject(self.idx, obj, cpopath + 'basis', i)
		check_status(status)
		if not status:
			self.basis = ret_basis
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
		self.comps.getNonTimedElt(path, cpopath + 'comps', i, obj)


class postructureedge_fluid_scalar_simplestruct(KeepInOrder):
	'''
	class postructureedge_fluid_scalar_simplestruct
	Electric potential [V]; Time-dependent;

	Attributes:
	- value : class valuestruct_arraycomplexgrid_scalar: array of valuestruct_arraycomplexgrid_scalarObj objects
	   Value of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- bndvalue : class bndvaluestruct_arraycomplexgrid_scalar: array of bndvaluestruct_arraycomplexgrid_scalarObj objects
	   Boundary values of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- flux : class fluxstruct_arraycomplexgrid_vector: array of fluxstruct_arraycomplexgrid_vectorObj objects
	   Flux of the quantity. Possibly stored on multiple subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- bndflux : class bndfluxstruct_arraycomplexgrid_vector: array of bndfluxstruct_arraycomplexgrid_vectorObj objects
	   Flux of the quantity. Possibly stored on multiple (boundary) subgrids.; Time-dependent; Array of structures (nsubgrid_quantity)
	- transpcoeff : class transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff: array of transpcoeffstruct_arrayedge_fluid_scalar_transpcoeffObj objects
	   Transport coefficients; Time-dependent; Array of structures (nsubgrid_quantity)
	- source : class sourcestruct_arraycomplexgrid_scalar: array of sourcestruct_arraycomplexgrid_scalarObj objects
	   Source; Time-dependent; Array of structures (nsubgrid_quantity)
	'''

	def __init__(self, base_path_in='po'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = valuestruct_arraycomplexgrid_scalar('value')
		self.bndvalue = bndvaluestruct_arraycomplexgrid_scalar('bndvalue')
		self.flux = fluxstruct_arraycomplexgrid_vector('flux')
		self.bndflux = bndfluxstruct_arraycomplexgrid_vector('bndflux')
		self.transpcoeff = transpcoeffstruct_arrayedge_fluid_scalar_transpcoeff('transpcoeff')
		self.source = sourcestruct_arraycomplexgrid_scalar('source')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class postructureedge_fluid_scalar_simplestruct\n'
		ret = ret + space + 'Attribute value\n ' + self.value.__str__(depth+1)
		ret = ret + space + 'Attribute bndvalue\n ' + self.bndvalue.__str__(depth+1)
		ret = ret + space + 'Attribute flux\n ' + self.flux.__str__(depth+1)
		ret = ret + space + 'Attribute bndflux\n ' + self.bndflux.__str__(depth+1)
		ret = ret + space + 'Attribute transpcoeff\n ' + self.transpcoeff.__str__(depth+1)
		ret = ret + space + 'Attribute source\n ' + self.source.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.value.setExpIdx(idx)
		self.bndvalue.setExpIdx(idx)
		self.flux.setExpIdx(idx)
		self.bndflux.setExpIdx(idx)
		self.transpcoeff.setExpIdx(idx)
		self.source.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type postructureedge_fluid_scalar_simplestruct, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.value.cpoTime = self.cpoTime
		self.value.putSlice(path, cpopath)
		self.bndvalue.cpoTime = self.cpoTime
		self.bndvalue.putSlice(path, cpopath)
		self.flux.cpoTime = self.cpoTime
		self.flux.putSlice(path, cpopath)
		self.bndflux.cpoTime = self.cpoTime
		self.bndflux.putSlice(path, cpopath)
		self.transpcoeff.cpoTime = self.cpoTime
		self.transpcoeff.putSlice(path, cpopath)
		self.source.cpoTime = self.cpoTime
		self.source.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type postructureedge_fluid_scalar_simplestruct, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.value.replaceLastSlice(path, cpopath)
		self.bndvalue.replaceLastSlice(path, cpopath)
		self.flux.replaceLastSlice(path, cpopath)
		self.bndflux.replaceLastSlice(path, cpopath)
		self.transpcoeff.replaceLastSlice(path, cpopath)
		self.source.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type postructureedge_fluid_scalar_simplestruct, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.value.putNonTimed(path, cpopath)
		self.bndvalue.putNonTimed(path, cpopath)
		self.flux.putNonTimed(path, cpopath)
		self.bndflux.putNonTimed(path, cpopath)
		self.transpcoeff.putNonTimed(path, cpopath)
		self.source.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type postructureedge_fluid_scalar_simplestruct, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.value.getSlice(path, cpopath, inTime, interpolMode)
		self.bndvalue.getSlice(path, cpopath, inTime, interpolMode)
		self.flux.getSlice(path, cpopath, inTime, interpolMode)
		self.bndflux.getSlice(path, cpopath, inTime, interpolMode)
		self.transpcoeff.getSlice(path, cpopath, inTime, interpolMode)
		self.source.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type postructureedge_fluid_scalar_simplestruct, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			valueList = self.value.build_non_resampled_data(path, cpopath, nbslice)
			bndvalueList = self.bndvalue.build_non_resampled_data(path, cpopath, nbslice)
			fluxList = self.flux.build_non_resampled_data(path, cpopath, nbslice)
			bndfluxList = self.bndflux.build_non_resampled_data(path, cpopath, nbslice)
			transpcoeffList = self.transpcoeff.build_non_resampled_data(path, cpopath, nbslice)
			sourceList = self.source.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = postructureedge_fluid_scalar_simplestruct(self.base_path)
				slice.setExpIdx(self.idx)
				slice.value = valueList[i]
				slice.bndvalue = bndvalueList[i]
				slice.flux = fluxList[i]
				slice.bndflux = bndfluxList[i]
				slice.transpcoeff = transpcoeffList[i]
				slice.source = sourceList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type postructureedge_fluid_scalar_simplestructObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.value.putTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.flux.putTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.bndflux.putTimedElt(path, cpopath + 'bndflux', i, obj)
		obj = self.transpcoeff.putTimedElt(path, cpopath + 'transpcoeff', i, obj)
		obj = self.source.putTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type postructureedge_fluid_scalar_simplestructObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.value.getTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.flux.getTimedElt(path, cpopath + 'flux', i, obj)
		self.bndflux.getTimedElt(path, cpopath + 'bndflux', i, obj)
		self.transpcoeff.getTimedElt(path, cpopath + 'transpcoeff', i, obj)
		self.source.getTimedElt(path, cpopath + 'source', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type postructureedge_fluid_scalar_simplestructObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.value.putNonTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.flux.putNonTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.bndflux.putNonTimedElt(path, cpopath + 'bndflux', i, obj)
		obj = self.transpcoeff.putNonTimedElt(path, cpopath + 'transpcoeff', i, obj)
		obj = self.source.putNonTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type postructureedge_fluid_scalar_simplestructObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.value.getNonTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.flux.getNonTimedElt(path, cpopath + 'flux', i, obj)
		self.bndflux.getNonTimedElt(path, cpopath + 'bndflux', i, obj)
		self.transpcoeff.getNonTimedElt(path, cpopath + 'transpcoeff', i, obj)
		self.source.getNonTimedElt(path, cpopath + 'source', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'value')
		ull.deleteData(self.idx, path, cpopath + 'bndvalue')
		ull.deleteData(self.idx, path, cpopath + 'flux')
		ull.deleteData(self.idx, path, cpopath + 'bndflux')
		ull.deleteData(self.idx, path, cpopath + 'transpcoeff')
		ull.deleteData(self.idx, path, cpopath + 'source')


class jstructureedge_fluid_vector_simplestruct(KeepInOrder):
	'''
	class jstructureedge_fluid_vector_simplestruct
	Electric current [A]; Time-dependent;

	Attributes:
	- griduid : int
	   Unique identifier of the grid this vector quantity is associated with.
	- basis : int
	   Index of basis (defined in associated grid) this vector is aligned to; If set to GRID_UNDEFINED=0, the canonical basis of the default coordinates of the grid assumed.
	- comps : class compsstruct_arrayedge_fluid_scalar: array of compsstruct_arrayedge_fluid_scalarObj objects
	   Components of the vector. Array of structures(ndim); Time-dependent;
	- align : numpy.ndarray 1D with int)
	   Alignment of vector components, numerical flag. Int vector(ndim);
	- alignid : list of str
	   Alignment of vector components, string description. String vector(ndim);
	'''

	def __init__(self, base_path_in='j'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.griduid = EMPTY_INT
		self.basis = EMPTY_INT
		self.comps = compsstruct_arrayedge_fluid_scalar('comps')
		self.align = numpy.zeros(0, numpy.int32, order='C')
		self.alignid = ['']

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class jstructureedge_fluid_vector_simplestruct\n'
		ret = ret + space + 'Attribute griduid: ' + str(self.griduid) + '\n'
		ret = ret + space + 'Attribute basis: ' + str(self.basis) + '\n'
		ret = ret + space + 'Attribute comps\n ' + self.comps.__str__(depth+1)
		s = self.align.__str__()
		ret = ret + space + 'Attribute align\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.alignid.__str__()
		ret = ret + space + 'Attribute alignid\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.comps.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jstructureedge_fluid_vector_simplestruct, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.comps.cpoTime = self.cpoTime
		self.comps.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jstructureedge_fluid_vector_simplestruct, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.comps.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jstructureedge_fluid_vector_simplestruct, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'griduid', self.griduid)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'basis', self.basis)
		check_status(status)
		self.comps.putNonTimed(path, cpopath)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'align', numpy.array(self.align).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DString(self.idx, path, cpopath + 'alignid', self.alignid, False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type jstructureedge_fluid_vector_simplestruct, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_griduid = ull.getInt(self.idx, path, cpopath + 'griduid')
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		status, ret_basis = ull.getInt(self.idx, path, cpopath + 'basis')
		check_status(status)
		if not status:
			self.basis = ret_basis
		self.comps.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_align = ull.getVect1DInt(self.idx, path, cpopath + 'align')
		check_status(status)
		if not status:
			self.align = ret_align
		status, ret_alignid = ull.getVect1DString(self.idx, path, cpopath + 'alignid')
		check_status(status)
		if not status:
			self.alignid = ret_alignid

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type jstructureedge_fluid_vector_simplestruct, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, griduidVal = ull.getInt(self.idx, path, cpopath + 'griduid')
			check_status(status)
			status, basisVal = ull.getInt(self.idx, path, cpopath + 'basis')
			check_status(status)
			compsList = self.comps.build_non_resampled_data(path, cpopath, nbslice)
			status, alignVal = ull.getVect1DInt(self.idx, path, cpopath + 'align')
			check_status(status)
			status, alignidVal = ull.getVect1DString(self.idx, path, cpopath + 'alignid')
			check_status(status)
			for i in range(nbslice):
				slice = jstructureedge_fluid_vector_simplestruct(self.base_path)
				slice.setExpIdx(self.idx)
				slice.griduid = griduidVal
				slice.basis = basisVal
				slice.comps = compsList[i]
				slice.align = alignVal
				slice.alignid = alignidVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jstructureedge_fluid_vector_simplestructObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.comps.putTimedElt(path, cpopath + 'comps', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jstructureedge_fluid_vector_simplestructObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.comps.getTimedElt(path, cpopath + 'comps', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jstructureedge_fluid_vector_simplestructObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'griduid', i, self.griduid)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'basis', i, self.basis)
		obj = self.comps.putNonTimedElt(path, cpopath + 'comps', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'align') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'align', i, numpy.array(self.align).astype(numpy.int32))
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'alignid') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'alignid', i, self.alignid)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jstructureedge_fluid_vector_simplestructObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'griduid') 
			print ('obj = ' + str(obj))
		status, ret_griduid = ull.getIntFromObject(self.idx, obj, cpopath + 'griduid', i)
		check_status(status)
		if not status:
			self.griduid = ret_griduid
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'basis') 
			print ('obj = ' + str(obj))
		status, ret_basis = ull.getIntFromObject(self.idx, obj, cpopath + 'basis', i)
		check_status(status)
		if not status:
			self.basis = ret_basis
		self.comps.getNonTimedElt(path, cpopath + 'comps', i, obj)
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

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'griduid')
		ull.deleteData(self.idx, path, cpopath + 'basis')
		ull.deleteData(self.idx, path, cpopath + 'comps')
		ull.deleteData(self.idx, path, cpopath + 'align')
		ull.deleteData(self.idx, path, cpopath + 'alignid')


class bstruct_arraycomplexgrid_vector:
	'''
	class bstruct_arraycomplexgrid_vector
	Magnetic field vector [T]; Time-dependent;

	Attributes:
	- array : list of bstruct_arraycomplexgrid_vectorObj 
	'''

	def __init__(self, base_path_in='b'):
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
		ret = space + 'class bstruct_arraycomplexgrid_vector\n'
		for i in range(len(self.array)):
			ret = ret + space + 'bstruct_arraycomplexgrid_vector[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(bstruct_arraycomplexgrid_vectorObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bstruct_arraycomplexgrid_vector, run function putSlice') 
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
			print ('field '+self.base_path+' of type bstruct_arraycomplexgrid_vector, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type bstruct_arraycomplexgrid_vector, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type bstruct_arraycomplexgrid_vector, run function getSlice') 
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
			print ('field '+self.base_path+' of type bstruct_arraycomplexgrid_vector, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(bstruct_arraycomplexgrid_vector(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(bstruct_arraycomplexgrid_vector(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = bstruct_arraycomplexgrid_vector(self.base_path)
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
			print ('field '+self.base_path+' of type bstruct_arraycomplexgrid_vector, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type bstruct_arraycomplexgrid_vector, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type bstruct_arraycomplexgrid_vector, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type bstruct_arraycomplexgrid_vector, run function getNonTimedElt') 
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


class bstruct_arraycomplexgrid_vectorObj(KeepInOrder):
	'''
	class bstruct_arraycomplexgrid_vectorObj
	Magnetic field vector [T]; Time-dependent;

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

	def __init__(self, base_path_in='b'):
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
		ret = space + 'class bstruct_arraycomplexgrid_vectorObj\n'
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
			print ('object of type bstruct_arraycomplexgrid_vectorObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.comp.putTimedElt(path, cpopath + 'comp', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bstruct_arraycomplexgrid_vectorObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.comp.getTimedElt(path, cpopath + 'comp', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bstruct_arraycomplexgrid_vectorObj, run function putNonTimedElt') 
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
			print ('object of type bstruct_arraycomplexgrid_vectorObj, run function getNonTimedElt') 
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


class kineticstructureedge_kinetic(KeepInOrder):
	'''
	class kineticstructureedge_kinetic
	Kinetic description of edge plasma. Time-dependent.

	Attributes:
	- f : class fstruct_arrayedge_kinetic_distribution: array of fstruct_arrayedge_kinetic_distributionObj objects
	   Distribution function [1/m^3 (m/s)^-3]. Array of structuresr(nspecies); Time-dependent;
	'''

	def __init__(self, base_path_in='kinetic'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.f = fstruct_arrayedge_kinetic_distribution('f')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class kineticstructureedge_kinetic\n'
		ret = ret + space + 'Attribute f\n ' + self.f.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.f.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type kineticstructureedge_kinetic, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.f.cpoTime = self.cpoTime
		self.f.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type kineticstructureedge_kinetic, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.f.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type kineticstructureedge_kinetic, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.f.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type kineticstructureedge_kinetic, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.f.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type kineticstructureedge_kinetic, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			fList = self.f.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = kineticstructureedge_kinetic(self.base_path)
				slice.setExpIdx(self.idx)
				slice.f = fList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type kineticstructureedge_kineticObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.f.putTimedElt(path, cpopath + 'f', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type kineticstructureedge_kineticObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.f.getTimedElt(path, cpopath + 'f', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type kineticstructureedge_kineticObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.f.putNonTimedElt(path, cpopath + 'f', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type kineticstructureedge_kineticObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.f.getNonTimedElt(path, cpopath + 'f', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'f')


class fstruct_arrayedge_kinetic_distribution:
	'''
	class fstruct_arrayedge_kinetic_distribution
	Distribution function [1/m^3 (m/s)^-3]. Array of structuresr(nspecies); Time-dependent;

	Attributes:
	- array : list of fstruct_arrayedge_kinetic_distributionObj 
	'''

	def __init__(self, base_path_in='f'):
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
		ret = space + 'class fstruct_arrayedge_kinetic_distribution\n'
		for i in range(len(self.array)):
			ret = ret + space + 'fstruct_arrayedge_kinetic_distribution[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(fstruct_arrayedge_kinetic_distributionObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fstruct_arrayedge_kinetic_distribution, run function putSlice') 
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
			print ('field '+self.base_path+' of type fstruct_arrayedge_kinetic_distribution, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type fstruct_arrayedge_kinetic_distribution, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type fstruct_arrayedge_kinetic_distribution, run function getSlice') 
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
			print ('field '+self.base_path+' of type fstruct_arrayedge_kinetic_distribution, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(fstruct_arrayedge_kinetic_distribution(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(fstruct_arrayedge_kinetic_distribution(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = fstruct_arrayedge_kinetic_distribution(self.base_path)
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
			print ('field '+self.base_path+' of type fstruct_arrayedge_kinetic_distribution, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type fstruct_arrayedge_kinetic_distribution, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type fstruct_arrayedge_kinetic_distribution, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type fstruct_arrayedge_kinetic_distribution, run function getNonTimedElt') 
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


class fstruct_arrayedge_kinetic_distributionObj(KeepInOrder):
	'''
	class fstruct_arrayedge_kinetic_distributionObj
	Distribution function [1/m^3 (m/s)^-3]. Array of structuresr(nspecies); Time-dependent;

	Attributes:
	- value : class valuestruct_arraycomplexgrid_scalar: array of valuestruct_arraycomplexgrid_scalarObj objects
	   Value of distribution function. Possibly stored on multiple subgrids.; Vector (nsubgrid_quantity). Time-dependent;
	- bndvalue : class bndvaluestruct_arraycomplexgrid_scalar: array of bndvaluestruct_arraycomplexgrid_scalarObj objects
	   Boundary value of distribution function. Possibly stored on multiple subgrids.; Vector (nsubgrid_quantity). Time-dependent;
	- fluxes : class fluxesstruct_arraycomplexgrid_vector: array of fluxesstruct_arraycomplexgrid_vectorObj objects
	   Fluxes in phase space. Possibly stored on multiple subgrids.; Vector (nsubgrid_quantity). Time-dependent;
	- source : class sourcestruct_arraycomplexgrid_scalar: array of sourcestruct_arraycomplexgrid_scalarObj objects
	   Sources in phase space. Possibly stored on multiple subgrids.; Vector (nsubgrid_quantity). Time-dependent;
	'''

	def __init__(self, base_path_in='f'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.value = valuestruct_arraycomplexgrid_scalar('value')
		self.bndvalue = bndvaluestruct_arraycomplexgrid_scalar('bndvalue')
		self.fluxes = fluxesstruct_arraycomplexgrid_vector('fluxes')
		self.source = sourcestruct_arraycomplexgrid_scalar('source')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class fstruct_arrayedge_kinetic_distributionObj\n'
		ret = ret + space + 'Attribute value\n ' + self.value.__str__(depth+1)
		ret = ret + space + 'Attribute bndvalue\n ' + self.bndvalue.__str__(depth+1)
		ret = ret + space + 'Attribute fluxes\n ' + self.fluxes.__str__(depth+1)
		ret = ret + space + 'Attribute source\n ' + self.source.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.value.setExpIdx(idx)
		self.bndvalue.setExpIdx(idx)
		self.fluxes.setExpIdx(idx)
		self.source.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fstruct_arrayedge_kinetic_distributionObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.value.putTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.fluxes.putTimedElt(path, cpopath + 'fluxes', i, obj)
		obj = self.source.putTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fstruct_arrayedge_kinetic_distributionObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.value.getTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.fluxes.getTimedElt(path, cpopath + 'fluxes', i, obj)
		self.source.getTimedElt(path, cpopath + 'source', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fstruct_arrayedge_kinetic_distributionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.value.putNonTimedElt(path, cpopath + 'value', i, obj)
		obj = self.bndvalue.putNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		obj = self.fluxes.putNonTimedElt(path, cpopath + 'fluxes', i, obj)
		obj = self.source.putNonTimedElt(path, cpopath + 'source', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fstruct_arrayedge_kinetic_distributionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.value.getNonTimedElt(path, cpopath + 'value', i, obj)
		self.bndvalue.getNonTimedElt(path, cpopath + 'bndvalue', i, obj)
		self.fluxes.getNonTimedElt(path, cpopath + 'fluxes', i, obj)
		self.source.getNonTimedElt(path, cpopath + 'source', i, obj)


class fluxesstruct_arraycomplexgrid_vector:
	'''
	class fluxesstruct_arraycomplexgrid_vector
	Fluxes in phase space. Possibly stored on multiple subgrids.; Vector (nsubgrid_quantity). Time-dependent;

	Attributes:
	- array : list of fluxesstruct_arraycomplexgrid_vectorObj 
	'''

	def __init__(self, base_path_in='fluxes'):
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
		ret = space + 'class fluxesstruct_arraycomplexgrid_vector\n'
		for i in range(len(self.array)):
			ret = ret + space + 'fluxesstruct_arraycomplexgrid_vector[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(fluxesstruct_arraycomplexgrid_vectorObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluxesstruct_arraycomplexgrid_vector, run function putSlice') 
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
			print ('field '+self.base_path+' of type fluxesstruct_arraycomplexgrid_vector, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type fluxesstruct_arraycomplexgrid_vector, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type fluxesstruct_arraycomplexgrid_vector, run function getSlice') 
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
			print ('field '+self.base_path+' of type fluxesstruct_arraycomplexgrid_vector, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(fluxesstruct_arraycomplexgrid_vector(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(fluxesstruct_arraycomplexgrid_vector(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = fluxesstruct_arraycomplexgrid_vector(self.base_path)
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
			print ('field '+self.base_path+' of type fluxesstruct_arraycomplexgrid_vector, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type fluxesstruct_arraycomplexgrid_vector, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type fluxesstruct_arraycomplexgrid_vector, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type fluxesstruct_arraycomplexgrid_vector, run function getNonTimedElt') 
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


class fluxesstruct_arraycomplexgrid_vectorObj(KeepInOrder):
	'''
	class fluxesstruct_arraycomplexgrid_vectorObj
	Fluxes in phase space. Possibly stored on multiple subgrids.; Vector (nsubgrid_quantity). Time-dependent;

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

	def __init__(self, base_path_in='fluxes'):
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
		ret = space + 'class fluxesstruct_arraycomplexgrid_vectorObj\n'
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
			print ('object of type fluxesstruct_arraycomplexgrid_vectorObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.comp.putTimedElt(path, cpopath + 'comp', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxesstruct_arraycomplexgrid_vectorObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.comp.getTimedElt(path, cpopath + 'comp', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxesstruct_arraycomplexgrid_vectorObj, run function putNonTimedElt') 
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
			print ('object of type fluxesstruct_arraycomplexgrid_vectorObj, run function getNonTimedElt') 
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
