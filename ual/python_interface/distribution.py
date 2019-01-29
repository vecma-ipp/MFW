# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class distribution:
	'''
	class distribution
	Datastructure for representing data associated with a distribution function one or many particle species. This structure is specifically designed to handle non-Maxwellian distribution function generated during heating and current drive, typically solved using a Fokker-Planck calculation perturbed by a heating scheme (e.g. IC, EC, LH, NBI, or alpha heating) and then relaxed by Coloumb collisions. Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- composition : class compositionstructurecomposition
	   Plasma composition (description of ion species). OBSOLESCENT.
	- compositions : class compositionsstructurecompositions_type
	   Contains all the composition information for the simulation (main ions, impurities, neutrals, edge species).
	- distri_vec : class distri_vecstruct_arraydistri_vec: array of distri_vecstruct_arraydistri_vecObj objects
	   Vector over all distribution functions. Every distribution function has to be associated with only one particle species, specifiec in distri_vec/species/, but there could be multiple distribution function for each species. In this case, the fast particle populations should be superposed. Time-dependent. Structure array(ndistri_vec)
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	- time : float
	   Time [s]; Time-dependent; Scalar
	'''

	def __init__(self):
		self.base_path = 'distribution'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 8
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.composition = compositionstructurecomposition('composition')
		self.compositions = compositionsstructurecompositions_type('compositions')
		self.distri_vec = distri_vecstruct_arraydistri_vec('distri_vec')
		self.codeparam = codeparamstructurecodeparam('codeparam')
		self.time = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class distribution\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute composition\n ' + self.composition.__str__(depth+1)
		ret = ret + space + 'Attribute compositions\n ' + self.compositions.__str__(depth+1)
		ret = ret + space + 'Attribute distri_vec\n ' + self.distri_vec.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.composition.setExpIdx(idx)
		self.compositions.setExpIdx(idx)
		self.distri_vec.setExpIdx(idx)
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
		self.distri_vec.cpoTime = self.cpoTime
		self.distri_vec.putSlice(path, cpopath)
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
		self.distri_vec.replaceLastSlice(path, cpopath)
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
		self.distri_vec.putNonTimed(path, cpopath)
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
		self.distri_vec.getSlice(path, cpopath, inTime, interpolMode)
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
			distri_vecList = self.distri_vec.build_non_resampled_data(path, cpopath, nbslice)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			array = []
			for i in range(nbslice):
				slice = distribution()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.composition = compositionList[i]
				slice.compositions = compositionsList[i]
				slice.distri_vec = distri_vecList[i]
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
		ull.deleteData(self.idx, path, cpopath + 'distri_vec')
		self.codeparam.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')


class distributionArray:
	'''
	class distributionArray
	Datastructure for representing data associated with a distribution function one or many particle species. This structure is specifically designed to handle non-Maxwellian distribution function generated during heating and current drive, typically solved using a Fokker-Planck calculation perturbed by a heating scheme (e.g. IC, EC, LH, NBI, or alpha heating) and then relaxed by Coloumb collisions. Time-dependent CPO

	Attributes:
	- array : list of distribution
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
		ret = space + 'class distributionArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'distribution cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = distribution()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(distribution())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = distribution()
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


class compositionstructurecomposition:
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


class compositionsstructurecompositions_type:
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


class nucleistruct_arraynucleiObj:
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


class ionsstruct_arrayionsObj:
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


class impuritiesstruct_arrayimpuritiesObj:
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


class neutralscompstruct_arraycomposition_neutralscompObj:
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


class neutcompstruct_arraycomposition_neutrals_neutcompObj:
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


class typestruct_arrayidentifierObj:
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


class edgespeciesstruct_arrayedgespeciesObj:
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


class signaturestructureidentifier:
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


class distri_vecstruct_arraydistri_vec:
	'''
	class distri_vecstruct_arraydistri_vec
	Vector over all distribution functions. Every distribution function has to be associated with only one particle species, specifiec in distri_vec/species/, but there could be multiple distribution function for each species. In this case, the fast particle populations should be superposed. Time-dependent. Structure array(ndistri_vec)

	Attributes:
	- array : list of distri_vecstruct_arraydistri_vecObj 
	'''

	def __init__(self, base_path_in='distri_vec'):
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
		ret = space + 'class distri_vecstruct_arraydistri_vec\n'
		for i in range(len(self.array)):
			ret = ret + space + 'distri_vecstruct_arraydistri_vec[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(distri_vecstruct_arraydistri_vecObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type distri_vecstruct_arraydistri_vec, run function putSlice') 
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
			print ('field '+self.base_path+' of type distri_vecstruct_arraydistri_vec, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type distri_vecstruct_arraydistri_vec, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type distri_vecstruct_arraydistri_vec, run function getSlice') 
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
			print ('field '+self.base_path+' of type distri_vecstruct_arraydistri_vec, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(distri_vecstruct_arraydistri_vec(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(distri_vecstruct_arraydistri_vec(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = distri_vecstruct_arraydistri_vec(self.base_path)
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
			print ('field '+self.base_path+' of type distri_vecstruct_arraydistri_vec, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type distri_vecstruct_arraydistri_vec, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type distri_vecstruct_arraydistri_vec, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type distri_vecstruct_arraydistri_vec, run function getNonTimedElt') 
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


class distri_vecstruct_arraydistri_vecObj:
	'''
	class distri_vecstruct_arraydistri_vecObj
	Vector over all distribution functions. Every distribution function has to be associated with only one particle species, specifiec in distri_vec/species/, but there could be multiple distribution function for each species. In this case, the fast particle populations should be superposed. Time-dependent. Structure array(ndistri_vec)

	Attributes:
	- wave_id : class wave_idstruct_arrayenum_instance: array of wave_idstruct_arrayenum_instanceObj objects
	   List all waves affecting the distribution, as specified in waves/coherentwave/wave_id (see waves_types in the Documentation website under Conventions/Enumerated_datatypes). Vector(n_antennas)
	- source_id : class source_idstruct_arrayenum_instance: array of source_idstruct_arrayenum_instanceObj objects
	   List all neutral beam injectors and reactions contributing to the source, as specified in distsource/source/source_id (see distsource_types in the Documentation website under Conventions/Enumerated_datatypes). Vector(n_injectors_and_reactions)
	- species : class speciesstructurespecies_reference
	   Defines the distribution function species represented in this element of distri_vec. Time-dependent
	- gyro_type : int
	   Defines how to interpret the spatial coordinates: 1 = given at the actual particle position;  2 = given at the gyro centre of the particle position. Time-dependent
	- fast_filter : class fast_filterstructurefast_thermal_separation_filter
	   Description of how the fast and the thermal particle populations, used in global_param and profiles_1d, were separated. 
	- global_param : class global_paramstructuredist_global_param
	   Global parameters (in most cases volume integrated and surface averaged quanatities). Time-dependent
	- profiles_1d : class profiles_1dstructuredist_profiles_1d
	   Flux surface averaged profiles.
	- profiles_2d : class profiles_2dstructuredist_profiles_2d
	   2D profiles in the poloidal plane
	- dist_func : class dist_funcstructuredist_func
	   Distribution functions. The total distribution total distribution can either be given by the a set of markers/test particles (in markers), or by a gridded function (dist_expand). Note that the gridded distribution can be written as sum of successive approximations, where each term is given by an element in the vector dist_expand. Finally, the distribution can be written as a sum of a marker distribution and a gridded distribution, e.g. for delta-f Monte Carlo solution. Time-dependent
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='distri_vec'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.wave_id = wave_idstruct_arrayenum_instance('wave_id')
		self.source_id = source_idstruct_arrayenum_instance('source_id')
		self.species = speciesstructurespecies_reference('species')
		self.gyro_type = EMPTY_INT
		self.fast_filter = fast_filterstructurefast_thermal_separation_filter('fast_filter')
		self.global_param = global_paramstructuredist_global_param('global_param')
		self.profiles_1d = profiles_1dstructuredist_profiles_1d('profiles_1d')
		self.profiles_2d = profiles_2dstructuredist_profiles_2d('profiles_2d')
		self.dist_func = dist_funcstructuredist_func('dist_func')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class distri_vecstruct_arraydistri_vecObj\n'
		ret = ret + space + 'Attribute wave_id\n ' + self.wave_id.__str__(depth+1)
		ret = ret + space + 'Attribute source_id\n ' + self.source_id.__str__(depth+1)
		ret = ret + space + 'Attribute species\n ' + self.species.__str__(depth+1)
		ret = ret + space + 'Attribute gyro_type: ' + str(self.gyro_type) + '\n'
		ret = ret + space + 'Attribute fast_filter\n ' + self.fast_filter.__str__(depth+1)
		ret = ret + space + 'Attribute global_param\n ' + self.global_param.__str__(depth+1)
		ret = ret + space + 'Attribute profiles_1d\n ' + self.profiles_1d.__str__(depth+1)
		ret = ret + space + 'Attribute profiles_2d\n ' + self.profiles_2d.__str__(depth+1)
		ret = ret + space + 'Attribute dist_func\n ' + self.dist_func.__str__(depth+1)
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.wave_id.setExpIdx(idx)
		self.source_id.setExpIdx(idx)
		self.species.setExpIdx(idx)
		self.fast_filter.setExpIdx(idx)
		self.global_param.setExpIdx(idx)
		self.profiles_1d.setExpIdx(idx)
		self.profiles_2d.setExpIdx(idx)
		self.dist_func.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type distri_vecstruct_arraydistri_vecObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.species.putTimedElt(path, cpopath + 'species', i, obj)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'gyro_type') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'gyro_type', i, self.gyro_type)
		obj = self.fast_filter.putTimedElt(path, cpopath + 'fast_filter', i, obj)
		obj = self.global_param.putTimedElt(path, cpopath + 'global_param', i, obj)
		obj = self.profiles_1d.putTimedElt(path, cpopath + 'profiles_1d', i, obj)
		obj = self.profiles_2d.putTimedElt(path, cpopath + 'profiles_2d', i, obj)
		obj = self.dist_func.putTimedElt(path, cpopath + 'dist_func', i, obj)
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type distri_vecstruct_arraydistri_vecObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.species.getTimedElt(path, cpopath + 'species', i, obj)
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'gyro_type') 
			print ('obj = ' + str(obj))
		status, ret_gyro_type = ull.getIntFromObject(self.idx, obj, cpopath + 'gyro_type', i)
		check_status(status)
		if not status:
			self.gyro_type = ret_gyro_type
		self.fast_filter.getTimedElt(path, cpopath + 'fast_filter', i, obj)
		self.global_param.getTimedElt(path, cpopath + 'global_param', i, obj)
		self.profiles_1d.getTimedElt(path, cpopath + 'profiles_1d', i, obj)
		self.profiles_2d.getTimedElt(path, cpopath + 'profiles_2d', i, obj)
		self.dist_func.getTimedElt(path, cpopath + 'dist_func', i, obj)
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type distri_vecstruct_arraydistri_vecObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.wave_id.putNonTimedElt(path, cpopath + 'wave_id', i, obj)
		obj = self.source_id.putNonTimedElt(path, cpopath + 'source_id', i, obj)
		obj = self.species.putNonTimedElt(path, cpopath + 'species', i, obj)
		obj = self.fast_filter.putNonTimedElt(path, cpopath + 'fast_filter', i, obj)
		obj = self.global_param.putNonTimedElt(path, cpopath + 'global_param', i, obj)
		obj = self.profiles_1d.putNonTimedElt(path, cpopath + 'profiles_1d', i, obj)
		obj = self.profiles_2d.putNonTimedElt(path, cpopath + 'profiles_2d', i, obj)
		obj = self.dist_func.putNonTimedElt(path, cpopath + 'dist_func', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type distri_vecstruct_arraydistri_vecObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.wave_id.getNonTimedElt(path, cpopath + 'wave_id', i, obj)
		self.source_id.getNonTimedElt(path, cpopath + 'source_id', i, obj)
		self.species.getNonTimedElt(path, cpopath + 'species', i, obj)
		self.fast_filter.getNonTimedElt(path, cpopath + 'fast_filter', i, obj)
		self.global_param.getNonTimedElt(path, cpopath + 'global_param', i, obj)
		self.profiles_1d.getNonTimedElt(path, cpopath + 'profiles_1d', i, obj)
		self.profiles_2d.getNonTimedElt(path, cpopath + 'profiles_2d', i, obj)
		self.dist_func.getNonTimedElt(path, cpopath + 'dist_func', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)


class wave_idstruct_arrayenum_instance:
	'''
	class wave_idstruct_arrayenum_instance
	List all waves affecting the distribution, as specified in waves/coherentwave/wave_id (see waves_types in the Documentation website under Conventions/Enumerated_datatypes). Vector(n_antennas)

	Attributes:
	- array : list of wave_idstruct_arrayenum_instanceObj 
	'''

	def __init__(self, base_path_in='wave_id'):
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
		ret = space + 'class wave_idstruct_arrayenum_instance\n'
		for i in range(len(self.array)):
			ret = ret + space + 'wave_idstruct_arrayenum_instance[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(wave_idstruct_arrayenum_instanceObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wave_idstruct_arrayenum_instance, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wave_idstruct_arrayenum_instance, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type wave_idstruct_arrayenum_instance, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type wave_idstruct_arrayenum_instance, run function getSlice') 
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
			print ('field '+self.base_path+' of type wave_idstruct_arrayenum_instance, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(wave_idstruct_arrayenum_instance(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = wave_idstruct_arrayenum_instance(self.base_path)
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
			print ('field '+self.base_path+' of type wave_idstruct_arrayenum_instance, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type wave_idstruct_arrayenum_instance, run function getNonTimedElt') 
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


class wave_idstruct_arrayenum_instanceObj:
	'''
	class wave_idstruct_arrayenum_instanceObj
	List all waves affecting the distribution, as specified in waves/coherentwave/wave_id (see waves_types in the Documentation website under Conventions/Enumerated_datatypes). Vector(n_antennas)

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
		ret = space + 'class wave_idstruct_arrayenum_instanceObj\n'
		ret = ret + space + 'Attribute type\n ' + self.type.__str__(depth+1)
		ret = ret + space + 'Attribute name: ' + str(self.name) + '\n'
		ret = ret + space + 'Attribute index: ' + str(self.index) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.type.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type wave_idstruct_arrayenum_instanceObj, run function putNonTimedElt') 
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
			print ('object of type wave_idstruct_arrayenum_instanceObj, run function getNonTimedElt') 
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


class typestructureidentifier:
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


class source_idstruct_arrayenum_instance:
	'''
	class source_idstruct_arrayenum_instance
	List all neutral beam injectors and reactions contributing to the source, as specified in distsource/source/source_id (see distsource_types in the Documentation website under Conventions/Enumerated_datatypes). Vector(n_injectors_and_reactions)

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


class source_idstruct_arrayenum_instanceObj:
	'''
	class source_idstruct_arrayenum_instanceObj
	List all neutral beam injectors and reactions contributing to the source, as specified in distsource/source/source_id (see distsource_types in the Documentation website under Conventions/Enumerated_datatypes). Vector(n_injectors_and_reactions)

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


class speciesstructurespecies_reference:
	'''
	class speciesstructurespecies_reference
	Defines the distribution function species represented in this element of distri_vec. Time-dependent

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


class fast_filterstructurefast_thermal_separation_filter:
	'''
	class fast_filterstructurefast_thermal_separation_filter
	Description of how the fast and the thermal particle populations, used in global_param and profiles_1d, were separated. 

	Attributes:
	- method : class methodstructureidentifier
	   Identifier describing the method used to separate the fast and thermal particle population (see fast_thermal_separation_filter_identifier_definition in the Documentation website under Conventions/Enumerated_datatypes)
	- energy_sep : numpy.ndarray 1D with float
	   Energy at which the fast and thermal particle populations were separated [eV]. Vector (nrho). Time-dependent. 
	'''

	def __init__(self, base_path_in='fast_filter'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.method = methodstructureidentifier('method')
		self.energy_sep = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class fast_filterstructurefast_thermal_separation_filter\n'
		ret = ret + space + 'Attribute method\n ' + self.method.__str__(depth+1)
		s = self.energy_sep.__str__()
		ret = ret + space + 'Attribute energy_sep\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.method.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fast_filterstructurefast_thermal_separation_filter, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.method.cpoTime = self.cpoTime
		self.method.putSlice(path, cpopath)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'energy_sep', numpy.array(self.energy_sep).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fast_filterstructurefast_thermal_separation_filter, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.method.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'energy_sep', numpy.array(self.energy_sep).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fast_filterstructurefast_thermal_separation_filter, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.method.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type fast_filterstructurefast_thermal_separation_filter, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.method.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_energy_sep, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'energy_sep', inTime, interpolMode)
		check_status(status)
		if not status:
			self.energy_sep = ret_energy_sep
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type fast_filterstructurefast_thermal_separation_filter, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			methodList = self.method.build_non_resampled_data(path, cpopath, nbslice)
			status, energy_sepList = ull.getVect2DDouble(self.idx, path, cpopath + 'energy_sep')
			if len(energy_sepList) == 0:
				energy_sepList = numpy.resize(energy_sepList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = fast_filterstructurefast_thermal_separation_filter(self.base_path)
				slice.setExpIdx(self.idx)
				slice.method = methodList[i]
				slice.energy_sep = energy_sepList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fast_filterstructurefast_thermal_separation_filterObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'energy_sep') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'energy_sep', i, numpy.array(self.energy_sep).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fast_filterstructurefast_thermal_separation_filterObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'energy_sep') 
			print ('obj = ' + str(obj))
		status, ret_energy_sep = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'energy_sep', i)
		check_status(status)
		if not status:
			self.energy_sep = ret_energy_sep

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fast_filterstructurefast_thermal_separation_filterObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.method.putNonTimedElt(path, cpopath + 'method', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fast_filterstructurefast_thermal_separation_filterObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.method.getNonTimedElt(path, cpopath + 'method', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.method.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'energy_sep')


class methodstructureidentifier:
	'''
	class methodstructureidentifier
	Identifier describing the method used to separate the fast and thermal particle population (see fast_thermal_separation_filter_identifier_definition in the Documentation website under Conventions/Enumerated_datatypes)

	Attributes:
	- id : str
	   Short string identifier
	- flag : int
	   Integer identifier
	- description : str
	   Verbose description of identifier
	'''

	def __init__(self, base_path_in='method'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.id = ''
		self.flag = EMPTY_INT
		self.description = ''

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class methodstructureidentifier\n'
		ret = ret + space + 'Attribute id: ' + str(self.id) + '\n'
		ret = ret + space + 'Attribute flag: ' + str(self.flag) + '\n'
		ret = ret + space + 'Attribute description: ' + str(self.description) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type methodstructureidentifier, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type methodstructureidentifier, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type methodstructureidentifier, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type methodstructureidentifier, run function getSlice') 
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
			print ('field '+self.base_path+' of type methodstructureidentifier, run function build_non_resampled_data') 
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
				slice = methodstructureidentifier(self.base_path)
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
			print ('object of type methodstructureidentifierObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type methodstructureidentifierObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type methodstructureidentifierObj, run function putNonTimedElt') 
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
			print ('object of type methodstructureidentifierObj, run function getNonTimedElt') 
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


class global_paramstructuredist_global_param:
	'''
	class global_paramstructuredist_global_param
	Global parameters (in most cases volume integrated and surface averaged quanatities). Time-dependent

	Attributes:
	- geometry : class geometrystructuredist_geometry_0d
	   Geometrical constants
	- state : class statestructuredist_state_0d
	   Algebraic moments of the distribution function integrated over the plasma volume, e.g. total number of particles, energy etc. Time-dependent
	- collisions_e : class collisions_estructuredist_collisional_transfer_0d
	   Collisional exchange with the electrons. Time-dependent
	- collisions_i : class collisions_istruct_arraydist_collisional_transfer_0d: array of collisions_istruct_arraydist_collisional_transfer_0dObj objects
	   Collisional exchange with each ion species. The ion indexing should match the one in /distribution/compositions/ions.Time-dependent; Vector(nion)
	- collisions_z : class collisions_zstruct_arraydist_global_param_collisions_z: array of collisions_zstruct_arraydist_global_param_collisions_zObj objects
	   Collisional exchange with each impurity species. The ion indexing should match the one in /distribution/compositions/impurities. Time-dependent; Vector(nimpur)
	- sources : class sourcesstruct_arraydist_sources_0d: array of sourcesstruct_arraydist_sources_0dObj objects
	   Vector of volume integrated sources and sinks of particles, momentum and power included in the Fokker-Planck modelling. The physical meaning of each source term is specified through the identifier ./sources/type. Note that it is possible to store multiple source terms with the same value for ./source/type. Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='global_param'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.geometry = geometrystructuredist_geometry_0d('geometry')
		self.state = statestructuredist_state_0d('state')
		self.collisions_e = collisions_estructuredist_collisional_transfer_0d('collisions_e')
		self.collisions_i = collisions_istruct_arraydist_collisional_transfer_0d('collisions_i')
		self.collisions_z = collisions_zstruct_arraydist_global_param_collisions_z('collisions_z')
		self.sources = sourcesstruct_arraydist_sources_0d('sources')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class global_paramstructuredist_global_param\n'
		ret = ret + space + 'Attribute geometry\n ' + self.geometry.__str__(depth+1)
		ret = ret + space + 'Attribute state\n ' + self.state.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_e\n ' + self.collisions_e.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_i\n ' + self.collisions_i.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_z\n ' + self.collisions_z.__str__(depth+1)
		ret = ret + space + 'Attribute sources\n ' + self.sources.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.geometry.setExpIdx(idx)
		self.state.setExpIdx(idx)
		self.collisions_e.setExpIdx(idx)
		self.collisions_i.setExpIdx(idx)
		self.collisions_z.setExpIdx(idx)
		self.sources.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructuredist_global_param, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.cpoTime = self.cpoTime
		self.geometry.putSlice(path, cpopath)
		self.state.cpoTime = self.cpoTime
		self.state.putSlice(path, cpopath)
		self.collisions_e.cpoTime = self.cpoTime
		self.collisions_e.putSlice(path, cpopath)
		self.collisions_i.cpoTime = self.cpoTime
		self.collisions_i.putSlice(path, cpopath)
		self.collisions_z.cpoTime = self.cpoTime
		self.collisions_z.putSlice(path, cpopath)
		self.sources.cpoTime = self.cpoTime
		self.sources.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructuredist_global_param, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.replaceLastSlice(path, cpopath)
		self.state.replaceLastSlice(path, cpopath)
		self.collisions_e.replaceLastSlice(path, cpopath)
		self.collisions_i.replaceLastSlice(path, cpopath)
		self.collisions_z.replaceLastSlice(path, cpopath)
		self.sources.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructuredist_global_param, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.putNonTimed(path, cpopath)
		self.state.putNonTimed(path, cpopath)
		self.collisions_e.putNonTimed(path, cpopath)
		self.collisions_i.putNonTimed(path, cpopath)
		self.collisions_z.putNonTimed(path, cpopath)
		self.sources.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructuredist_global_param, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.getSlice(path, cpopath, inTime, interpolMode)
		self.state.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_e.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_i.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_z.getSlice(path, cpopath, inTime, interpolMode)
		self.sources.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructuredist_global_param, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			geometryList = self.geometry.build_non_resampled_data(path, cpopath, nbslice)
			stateList = self.state.build_non_resampled_data(path, cpopath, nbslice)
			collisions_eList = self.collisions_e.build_non_resampled_data(path, cpopath, nbslice)
			collisions_iList = self.collisions_i.build_non_resampled_data(path, cpopath, nbslice)
			collisions_zList = self.collisions_z.build_non_resampled_data(path, cpopath, nbslice)
			sourcesList = self.sources.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = global_paramstructuredist_global_param(self.base_path)
				slice.setExpIdx(self.idx)
				slice.geometry = geometryList[i]
				slice.state = stateList[i]
				slice.collisions_e = collisions_eList[i]
				slice.collisions_i = collisions_iList[i]
				slice.collisions_z = collisions_zList[i]
				slice.sources = sourcesList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructuredist_global_paramObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.geometry.putTimedElt(path, cpopath + 'geometry', i, obj)
		obj = self.state.putTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putTimedElt(path, cpopath + 'collisions_z', i, obj)
		obj = self.sources.putTimedElt(path, cpopath + 'sources', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructuredist_global_paramObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.geometry.getTimedElt(path, cpopath + 'geometry', i, obj)
		self.state.getTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getTimedElt(path, cpopath + 'collisions_z', i, obj)
		self.sources.getTimedElt(path, cpopath + 'sources', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructuredist_global_paramObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.geometry.putNonTimedElt(path, cpopath + 'geometry', i, obj)
		obj = self.state.putNonTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		obj = self.sources.putNonTimedElt(path, cpopath + 'sources', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructuredist_global_paramObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.geometry.getNonTimedElt(path, cpopath + 'geometry', i, obj)
		self.state.getNonTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		self.sources.getNonTimedElt(path, cpopath + 'sources', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.deleteData(path, cpopath)
		self.state.deleteData(path, cpopath)
		self.collisions_e.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'collisions_i')
		ull.deleteData(self.idx, path, cpopath + 'collisions_z')
		ull.deleteData(self.idx, path, cpopath + 'sources')


class geometrystructuredist_geometry_0d:
	'''
	class geometrystructuredist_geometry_0d
	Geometrical constants

	Attributes:
	- mag_axis : class mag_axisstructurerz0D
	   Position of the magnetic axis [m]. Time-dependent; Scalar
	- toroid_field : class toroid_fieldstructureb0r0
	   Characteristics of the vacuum toroidal field. Used to define the radial coordiante rho_tor and to measure the current drive. Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='geometry'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.mag_axis = mag_axisstructurerz0D('mag_axis')
		self.toroid_field = toroid_fieldstructureb0r0('toroid_field')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class geometrystructuredist_geometry_0d\n'
		ret = ret + space + 'Attribute mag_axis\n ' + self.mag_axis.__str__(depth+1)
		ret = ret + space + 'Attribute toroid_field\n ' + self.toroid_field.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.mag_axis.setExpIdx(idx)
		self.toroid_field.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_0d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mag_axis.cpoTime = self.cpoTime
		self.mag_axis.putSlice(path, cpopath)
		self.toroid_field.cpoTime = self.cpoTime
		self.toroid_field.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_0d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mag_axis.replaceLastSlice(path, cpopath)
		self.toroid_field.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_0d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mag_axis.putNonTimed(path, cpopath)
		self.toroid_field.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_0d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mag_axis.getSlice(path, cpopath, inTime, interpolMode)
		self.toroid_field.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_0d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			mag_axisList = self.mag_axis.build_non_resampled_data(path, cpopath, nbslice)
			toroid_fieldList = self.toroid_field.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = geometrystructuredist_geometry_0d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.mag_axis = mag_axisList[i]
				slice.toroid_field = toroid_fieldList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geometrystructuredist_geometry_0dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.mag_axis.putTimedElt(path, cpopath + 'mag_axis', i, obj)
		obj = self.toroid_field.putTimedElt(path, cpopath + 'toroid_field', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geometrystructuredist_geometry_0dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.mag_axis.getTimedElt(path, cpopath + 'mag_axis', i, obj)
		self.toroid_field.getTimedElt(path, cpopath + 'toroid_field', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geometrystructuredist_geometry_0dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.mag_axis.putNonTimedElt(path, cpopath + 'mag_axis', i, obj)
		obj = self.toroid_field.putNonTimedElt(path, cpopath + 'toroid_field', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geometrystructuredist_geometry_0dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.mag_axis.getNonTimedElt(path, cpopath + 'mag_axis', i, obj)
		self.toroid_field.getNonTimedElt(path, cpopath + 'toroid_field', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mag_axis.deleteData(path, cpopath)
		self.toroid_field.deleteData(path, cpopath)


class mag_axisstructurerz0D:
	'''
	class mag_axisstructurerz0D
	Position of the magnetic axis [m]. Time-dependent; Scalar

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


class toroid_fieldstructureb0r0:
	'''
	class toroid_fieldstructureb0r0
	Characteristics of the vacuum toroidal field. Used to define the radial coordiante rho_tor and to measure the current drive. Time-dependent; Scalar

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


class statestructuredist_state_0d:
	'''
	class statestructuredist_state_0d
	Algebraic moments of the distribution function integrated over the plasma volume, e.g. total number of particles, energy etc. Time-dependent

	Attributes:
	- n_particles : float
	   Number of particles in the distribution; the volume integral of the density (note: this is the number of real particles and not markers); Time-dependent
	- n_part_fast : float
	   Number of fast particles in the distribution; the volume integral of the fast particle density (note: this is the number of real particles and not markers); Time-dependent
	- enrg : float
	   Total energy distribution [J]; Time-dependent
	- enrg_fast : float
	   Total energy of the fast particle distribution [J]; Time-dependent
	- enrg_fast_pa : float
	   Parallel energy of the fast particle distribution [J]; Time-dependent
	- momentm_fast : float
	   Kinetic toroidal angular momentum of the fast ions [Nms]; Time-dependent; Vector (npsi)
	- current_dr : float
	   Toroidal non-inductive current drive [A]; Time-dependent.
	- torque_jrxb : float
	   Toroidal torque due to radial currents [N.m]; Time-dependent.
	'''

	def __init__(self, base_path_in='state'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.n_particles = EMPTY_DOUBLE
		self.n_part_fast = EMPTY_DOUBLE
		self.enrg = EMPTY_DOUBLE
		self.enrg_fast = EMPTY_DOUBLE
		self.enrg_fast_pa = EMPTY_DOUBLE
		self.momentm_fast = EMPTY_DOUBLE
		self.current_dr = EMPTY_DOUBLE
		self.torque_jrxb = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class statestructuredist_state_0d\n'
		ret = ret + space + 'Attribute n_particles: ' + str(self.n_particles) + '\n'
		ret = ret + space + 'Attribute n_part_fast: ' + str(self.n_part_fast) + '\n'
		ret = ret + space + 'Attribute enrg: ' + str(self.enrg) + '\n'
		ret = ret + space + 'Attribute enrg_fast: ' + str(self.enrg_fast) + '\n'
		ret = ret + space + 'Attribute enrg_fast_pa: ' + str(self.enrg_fast_pa) + '\n'
		ret = ret + space + 'Attribute momentm_fast: ' + str(self.momentm_fast) + '\n'
		ret = ret + space + 'Attribute current_dr: ' + str(self.current_dr) + '\n'
		ret = ret + space + 'Attribute torque_jrxb: ' + str(self.torque_jrxb) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_0d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'n_particles', self.n_particles, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'n_part_fast', self.n_part_fast, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'enrg', self.enrg, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'enrg_fast', self.enrg_fast, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'enrg_fast_pa', self.enrg_fast_pa, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'momentm_fast', self.momentm_fast, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'current_dr', self.current_dr, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'torque_jrxb', self.torque_jrxb, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_0d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'n_particles', self.n_particles)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'n_part_fast', self.n_part_fast)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'enrg', self.enrg)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'enrg_fast', self.enrg_fast)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'enrg_fast_pa', self.enrg_fast_pa)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'momentm_fast', self.momentm_fast)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'current_dr', self.current_dr)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'torque_jrxb', self.torque_jrxb)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_0d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_0d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_n_particles, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'n_particles', inTime, interpolMode)
		check_status(status)
		if not status:
			self.n_particles = ret_n_particles
			self.cpoTime = retTime
		status, ret_n_part_fast, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'n_part_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.n_part_fast = ret_n_part_fast
			self.cpoTime = retTime
		status, ret_enrg, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'enrg', inTime, interpolMode)
		check_status(status)
		if not status:
			self.enrg = ret_enrg
			self.cpoTime = retTime
		status, ret_enrg_fast, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'enrg_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.enrg_fast = ret_enrg_fast
			self.cpoTime = retTime
		status, ret_enrg_fast_pa, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'enrg_fast_pa', inTime, interpolMode)
		check_status(status)
		if not status:
			self.enrg_fast_pa = ret_enrg_fast_pa
			self.cpoTime = retTime
		status, ret_momentm_fast, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'momentm_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.momentm_fast = ret_momentm_fast
			self.cpoTime = retTime
		status, ret_current_dr, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'current_dr', inTime, interpolMode)
		check_status(status)
		if not status:
			self.current_dr = ret_current_dr
			self.cpoTime = retTime
		status, ret_torque_jrxb, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'torque_jrxb', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_jrxb = ret_torque_jrxb
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_0d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, n_particlesList = ull.getVect1DDouble(self.idx, path, cpopath + 'n_particles')
			if len(n_particlesList) == 0:
				n_particlesList = numpy.resize(n_particlesList, (nbslice))
			check_status(status)
			status, n_part_fastList = ull.getVect1DDouble(self.idx, path, cpopath + 'n_part_fast')
			if len(n_part_fastList) == 0:
				n_part_fastList = numpy.resize(n_part_fastList, (nbslice))
			check_status(status)
			status, enrgList = ull.getVect1DDouble(self.idx, path, cpopath + 'enrg')
			if len(enrgList) == 0:
				enrgList = numpy.resize(enrgList, (nbslice))
			check_status(status)
			status, enrg_fastList = ull.getVect1DDouble(self.idx, path, cpopath + 'enrg_fast')
			if len(enrg_fastList) == 0:
				enrg_fastList = numpy.resize(enrg_fastList, (nbslice))
			check_status(status)
			status, enrg_fast_paList = ull.getVect1DDouble(self.idx, path, cpopath + 'enrg_fast_pa')
			if len(enrg_fast_paList) == 0:
				enrg_fast_paList = numpy.resize(enrg_fast_paList, (nbslice))
			check_status(status)
			status, momentm_fastList = ull.getVect1DDouble(self.idx, path, cpopath + 'momentm_fast')
			if len(momentm_fastList) == 0:
				momentm_fastList = numpy.resize(momentm_fastList, (nbslice))
			check_status(status)
			status, current_drList = ull.getVect1DDouble(self.idx, path, cpopath + 'current_dr')
			if len(current_drList) == 0:
				current_drList = numpy.resize(current_drList, (nbslice))
			check_status(status)
			status, torque_jrxbList = ull.getVect1DDouble(self.idx, path, cpopath + 'torque_jrxb')
			if len(torque_jrxbList) == 0:
				torque_jrxbList = numpy.resize(torque_jrxbList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = statestructuredist_state_0d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.n_particles = n_particlesList[i].copy().astype(float)
				slice.n_part_fast = n_part_fastList[i].copy().astype(float)
				slice.enrg = enrgList[i].copy().astype(float)
				slice.enrg_fast = enrg_fastList[i].copy().astype(float)
				slice.enrg_fast_pa = enrg_fast_paList[i].copy().astype(float)
				slice.momentm_fast = momentm_fastList[i].copy().astype(float)
				slice.current_dr = current_drList[i].copy().astype(float)
				slice.torque_jrxb = torque_jrxbList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type statestructuredist_state_0dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'n_particles') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'n_particles', i, self.n_particles)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'n_part_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'n_part_fast', i, self.n_part_fast)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'enrg') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'enrg', i, self.enrg)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'enrg_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'enrg_fast', i, self.enrg_fast)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'enrg_fast_pa') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'enrg_fast_pa', i, self.enrg_fast_pa)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'momentm_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'momentm_fast', i, self.momentm_fast)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'current_dr') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'current_dr', i, self.current_dr)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'torque_jrxb') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'torque_jrxb', i, self.torque_jrxb)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type statestructuredist_state_0dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'n_particles') 
			print ('obj = ' + str(obj))
		status, ret_n_particles = ull.getDoubleFromObject(self.idx, obj, cpopath + 'n_particles', i)
		check_status(status)
		if not status:
			self.n_particles = ret_n_particles
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'n_part_fast') 
			print ('obj = ' + str(obj))
		status, ret_n_part_fast = ull.getDoubleFromObject(self.idx, obj, cpopath + 'n_part_fast', i)
		check_status(status)
		if not status:
			self.n_part_fast = ret_n_part_fast
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'enrg') 
			print ('obj = ' + str(obj))
		status, ret_enrg = ull.getDoubleFromObject(self.idx, obj, cpopath + 'enrg', i)
		check_status(status)
		if not status:
			self.enrg = ret_enrg
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'enrg_fast') 
			print ('obj = ' + str(obj))
		status, ret_enrg_fast = ull.getDoubleFromObject(self.idx, obj, cpopath + 'enrg_fast', i)
		check_status(status)
		if not status:
			self.enrg_fast = ret_enrg_fast
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'enrg_fast_pa') 
			print ('obj = ' + str(obj))
		status, ret_enrg_fast_pa = ull.getDoubleFromObject(self.idx, obj, cpopath + 'enrg_fast_pa', i)
		check_status(status)
		if not status:
			self.enrg_fast_pa = ret_enrg_fast_pa
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'momentm_fast') 
			print ('obj = ' + str(obj))
		status, ret_momentm_fast = ull.getDoubleFromObject(self.idx, obj, cpopath + 'momentm_fast', i)
		check_status(status)
		if not status:
			self.momentm_fast = ret_momentm_fast
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'current_dr') 
			print ('obj = ' + str(obj))
		status, ret_current_dr = ull.getDoubleFromObject(self.idx, obj, cpopath + 'current_dr', i)
		check_status(status)
		if not status:
			self.current_dr = ret_current_dr
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'torque_jrxb') 
			print ('obj = ' + str(obj))
		status, ret_torque_jrxb = ull.getDoubleFromObject(self.idx, obj, cpopath + 'torque_jrxb', i)
		check_status(status)
		if not status:
			self.torque_jrxb = ret_torque_jrxb

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type statestructuredist_state_0dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type statestructuredist_state_0dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'n_particles')
		ull.deleteData(self.idx, path, cpopath + 'n_part_fast')
		ull.deleteData(self.idx, path, cpopath + 'enrg')
		ull.deleteData(self.idx, path, cpopath + 'enrg_fast')
		ull.deleteData(self.idx, path, cpopath + 'enrg_fast_pa')
		ull.deleteData(self.idx, path, cpopath + 'momentm_fast')
		ull.deleteData(self.idx, path, cpopath + 'current_dr')
		ull.deleteData(self.idx, path, cpopath + 'torque_jrxb')


class collisions_estructuredist_collisional_transfer_0d:
	'''
	class collisions_estructuredist_collisional_transfer_0d
	Collisional exchange with the electrons. Time-dependent

	Attributes:
	- power_th : float
	   Collisional power to the thermal particle population [W]; Time-dependent; Scalar
	- power_fast : float
	   Collisional power to the fast particle population [W]; Time-dependent; Scalar
	- torque_th : float
	   Collisional toroidal torque to the thermal particle population [N.m]; Time-dependent; Scalar
	- torque_fast : float
	   Collisional toroidal torque to the fast particle population [N.m]; Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='collisions_e'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.power_th = EMPTY_DOUBLE
		self.power_fast = EMPTY_DOUBLE
		self.torque_th = EMPTY_DOUBLE
		self.torque_fast = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class collisions_estructuredist_collisional_transfer_0d\n'
		ret = ret + space + 'Attribute power_th: ' + str(self.power_th) + '\n'
		ret = ret + space + 'Attribute power_fast: ' + str(self.power_fast) + '\n'
		ret = ret + space + 'Attribute torque_th: ' + str(self.torque_th) + '\n'
		ret = ret + space + 'Attribute torque_fast: ' + str(self.torque_fast) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_0d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'power_th', self.power_th, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'power_fast', self.power_fast, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'torque_th', self.torque_th, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'torque_fast', self.torque_fast, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_0d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'power_th', self.power_th)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'power_fast', self.power_fast)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'torque_th', self.torque_th)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'torque_fast', self.torque_fast)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_0d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_0d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_power_th, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'power_th', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power_th = ret_power_th
			self.cpoTime = retTime
		status, ret_power_fast, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'power_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power_fast = ret_power_fast
			self.cpoTime = retTime
		status, ret_torque_th, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'torque_th', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_th = ret_torque_th
			self.cpoTime = retTime
		status, ret_torque_fast, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'torque_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_fast = ret_torque_fast
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_0d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, power_thList = ull.getVect1DDouble(self.idx, path, cpopath + 'power_th')
			if len(power_thList) == 0:
				power_thList = numpy.resize(power_thList, (nbslice))
			check_status(status)
			status, power_fastList = ull.getVect1DDouble(self.idx, path, cpopath + 'power_fast')
			if len(power_fastList) == 0:
				power_fastList = numpy.resize(power_fastList, (nbslice))
			check_status(status)
			status, torque_thList = ull.getVect1DDouble(self.idx, path, cpopath + 'torque_th')
			if len(torque_thList) == 0:
				torque_thList = numpy.resize(torque_thList, (nbslice))
			check_status(status)
			status, torque_fastList = ull.getVect1DDouble(self.idx, path, cpopath + 'torque_fast')
			if len(torque_fastList) == 0:
				torque_fastList = numpy.resize(torque_fastList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = collisions_estructuredist_collisional_transfer_0d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.power_th = power_thList[i].copy().astype(float)
				slice.power_fast = power_fastList[i].copy().astype(float)
				slice.torque_th = torque_thList[i].copy().astype(float)
				slice.torque_fast = torque_fastList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_estructuredist_collisional_transfer_0dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'power_th', i, self.power_th)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'power_fast', i, self.power_fast)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'torque_th', i, self.torque_th)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'torque_fast', i, self.torque_fast)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_estructuredist_collisional_transfer_0dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		status, ret_power_th = ull.getDoubleFromObject(self.idx, obj, cpopath + 'power_th', i)
		check_status(status)
		if not status:
			self.power_th = ret_power_th
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		status, ret_power_fast = ull.getDoubleFromObject(self.idx, obj, cpopath + 'power_fast', i)
		check_status(status)
		if not status:
			self.power_fast = ret_power_fast
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		status, ret_torque_th = ull.getDoubleFromObject(self.idx, obj, cpopath + 'torque_th', i)
		check_status(status)
		if not status:
			self.torque_th = ret_torque_th
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		status, ret_torque_fast = ull.getDoubleFromObject(self.idx, obj, cpopath + 'torque_fast', i)
		check_status(status)
		if not status:
			self.torque_fast = ret_torque_fast

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_estructuredist_collisional_transfer_0dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_estructuredist_collisional_transfer_0dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'power_th')
		ull.deleteData(self.idx, path, cpopath + 'power_fast')
		ull.deleteData(self.idx, path, cpopath + 'torque_th')
		ull.deleteData(self.idx, path, cpopath + 'torque_fast')


class collisions_istruct_arraydist_collisional_transfer_0d:
	'''
	class collisions_istruct_arraydist_collisional_transfer_0d
	Collisional exchange with each ion species. The ion indexing should match the one in /distribution/compositions/ions.Time-dependent; Vector(nion)

	Attributes:
	- array : list of collisions_istruct_arraydist_collisional_transfer_0dObj 
	'''

	def __init__(self, base_path_in='collisions_i'):
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
		ret = space + 'class collisions_istruct_arraydist_collisional_transfer_0d\n'
		for i in range(len(self.array)):
			ret = ret + space + 'collisions_istruct_arraydist_collisional_transfer_0d[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(collisions_istruct_arraydist_collisional_transfer_0dObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_0d, run function putSlice') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_0d, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_0d, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_0d, run function getSlice') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_0d, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(collisions_istruct_arraydist_collisional_transfer_0d(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(collisions_istruct_arraydist_collisional_transfer_0d(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = collisions_istruct_arraydist_collisional_transfer_0d(self.base_path)
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_0d, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_0d, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_0d, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_0d, run function getNonTimedElt') 
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


class collisions_istruct_arraydist_collisional_transfer_0dObj:
	'''
	class collisions_istruct_arraydist_collisional_transfer_0dObj
	Collisional exchange with each ion species. The ion indexing should match the one in /distribution/compositions/ions.Time-dependent; Vector(nion)

	Attributes:
	- power_th : float
	   Collisional power to the thermal particle population [W]; Time-dependent; Scalar
	- power_fast : float
	   Collisional power to the fast particle population [W]; Time-dependent; Scalar
	- torque_th : float
	   Collisional toroidal torque to the thermal particle population [N.m]; Time-dependent; Scalar
	- torque_fast : float
	   Collisional toroidal torque to the fast particle population [N.m]; Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='collisions_i'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.power_th = EMPTY_DOUBLE
		self.power_fast = EMPTY_DOUBLE
		self.torque_th = EMPTY_DOUBLE
		self.torque_fast = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class collisions_istruct_arraydist_collisional_transfer_0dObj\n'
		ret = ret + space + 'Attribute power_th: ' + str(self.power_th) + '\n'
		ret = ret + space + 'Attribute power_fast: ' + str(self.power_fast) + '\n'
		ret = ret + space + 'Attribute torque_th: ' + str(self.torque_th) + '\n'
		ret = ret + space + 'Attribute torque_fast: ' + str(self.torque_fast) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_istruct_arraydist_collisional_transfer_0dObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'power_th', i, self.power_th)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'power_fast', i, self.power_fast)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'torque_th', i, self.torque_th)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'torque_fast', i, self.torque_fast)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_istruct_arraydist_collisional_transfer_0dObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		status, ret_power_th = ull.getDoubleFromObject(self.idx, obj, cpopath + 'power_th', i)
		check_status(status)
		if not status:
			self.power_th = ret_power_th
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		status, ret_power_fast = ull.getDoubleFromObject(self.idx, obj, cpopath + 'power_fast', i)
		check_status(status)
		if not status:
			self.power_fast = ret_power_fast
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		status, ret_torque_th = ull.getDoubleFromObject(self.idx, obj, cpopath + 'torque_th', i)
		check_status(status)
		if not status:
			self.torque_th = ret_torque_th
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		status, ret_torque_fast = ull.getDoubleFromObject(self.idx, obj, cpopath + 'torque_fast', i)
		check_status(status)
		if not status:
			self.torque_fast = ret_torque_fast

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_istruct_arraydist_collisional_transfer_0dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_istruct_arraydist_collisional_transfer_0dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class collisions_zstruct_arraydist_global_param_collisions_z:
	'''
	class collisions_zstruct_arraydist_global_param_collisions_z
	Collisional exchange with each impurity species. The ion indexing should match the one in /distribution/compositions/impurities. Time-dependent; Vector(nimpur)

	Attributes:
	- array : list of collisions_zstruct_arraydist_global_param_collisions_zObj 
	'''

	def __init__(self, base_path_in='collisions_z'):
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
		ret = space + 'class collisions_zstruct_arraydist_global_param_collisions_z\n'
		for i in range(len(self.array)):
			ret = ret + space + 'collisions_zstruct_arraydist_global_param_collisions_z[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(collisions_zstruct_arraydist_global_param_collisions_zObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_global_param_collisions_z, run function putSlice') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_global_param_collisions_z, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_global_param_collisions_z, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_global_param_collisions_z, run function getSlice') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_global_param_collisions_z, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(collisions_zstruct_arraydist_global_param_collisions_z(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(collisions_zstruct_arraydist_global_param_collisions_z(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = collisions_zstruct_arraydist_global_param_collisions_z(self.base_path)
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_global_param_collisions_z, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_global_param_collisions_z, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_global_param_collisions_z, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_global_param_collisions_z, run function getNonTimedElt') 
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


class collisions_zstruct_arraydist_global_param_collisions_zObj:
	'''
	class collisions_zstruct_arraydist_global_param_collisions_zObj
	Collisional exchange with each impurity species. The ion indexing should match the one in /distribution/compositions/impurities. Time-dependent; Vector(nimpur)

	Attributes:
	- charge_state : class charge_statestruct_arraydist_collisional_transfer_0d: array of charge_statestruct_arraydist_collisional_transfer_0dObj objects
	   Collisional exchange with the impurities. The ion indexing should match the one in distribution/compositions/impurities/zmin. Time-dependent; Vector(nzimp)
	'''

	def __init__(self, base_path_in='collisions_z'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.charge_state = charge_statestruct_arraydist_collisional_transfer_0d('charge_state')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class collisions_zstruct_arraydist_global_param_collisions_zObj\n'
		ret = ret + space + 'Attribute charge_state\n ' + self.charge_state.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.charge_state.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_zstruct_arraydist_global_param_collisions_zObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.charge_state.putTimedElt(path, cpopath + 'charge_state', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_zstruct_arraydist_global_param_collisions_zObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.charge_state.getTimedElt(path, cpopath + 'charge_state', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_zstruct_arraydist_global_param_collisions_zObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.charge_state.putNonTimedElt(path, cpopath + 'charge_state', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_zstruct_arraydist_global_param_collisions_zObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.charge_state.getNonTimedElt(path, cpopath + 'charge_state', i, obj)


class charge_statestruct_arraydist_collisional_transfer_0d:
	'''
	class charge_statestruct_arraydist_collisional_transfer_0d
	Collisional exchange with the impurities. The ion indexing should match the one in distribution/compositions/impurities/zmin. Time-dependent; Vector(nzimp)

	Attributes:
	- array : list of charge_statestruct_arraydist_collisional_transfer_0dObj 
	'''

	def __init__(self, base_path_in='charge_state'):
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
		ret = space + 'class charge_statestruct_arraydist_collisional_transfer_0d\n'
		for i in range(len(self.array)):
			ret = ret + space + 'charge_statestruct_arraydist_collisional_transfer_0d[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(charge_statestruct_arraydist_collisional_transfer_0dObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_0d, run function putSlice') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_0d, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_0d, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_0d, run function getSlice') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_0d, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(charge_statestruct_arraydist_collisional_transfer_0d(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(charge_statestruct_arraydist_collisional_transfer_0d(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = charge_statestruct_arraydist_collisional_transfer_0d(self.base_path)
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_0d, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_0d, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_0d, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_0d, run function getNonTimedElt') 
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


class charge_statestruct_arraydist_collisional_transfer_0dObj:
	'''
	class charge_statestruct_arraydist_collisional_transfer_0dObj
	Collisional exchange with the impurities. The ion indexing should match the one in distribution/compositions/impurities/zmin. Time-dependent; Vector(nzimp)

	Attributes:
	- power_th : float
	   Collisional power to the thermal particle population [W]; Time-dependent; Scalar
	- power_fast : float
	   Collisional power to the fast particle population [W]; Time-dependent; Scalar
	- torque_th : float
	   Collisional toroidal torque to the thermal particle population [N.m]; Time-dependent; Scalar
	- torque_fast : float
	   Collisional toroidal torque to the fast particle population [N.m]; Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='charge_state'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.power_th = EMPTY_DOUBLE
		self.power_fast = EMPTY_DOUBLE
		self.torque_th = EMPTY_DOUBLE
		self.torque_fast = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class charge_statestruct_arraydist_collisional_transfer_0dObj\n'
		ret = ret + space + 'Attribute power_th: ' + str(self.power_th) + '\n'
		ret = ret + space + 'Attribute power_fast: ' + str(self.power_fast) + '\n'
		ret = ret + space + 'Attribute torque_th: ' + str(self.torque_th) + '\n'
		ret = ret + space + 'Attribute torque_fast: ' + str(self.torque_fast) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type charge_statestruct_arraydist_collisional_transfer_0dObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'power_th', i, self.power_th)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'power_fast', i, self.power_fast)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'torque_th', i, self.torque_th)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'torque_fast', i, self.torque_fast)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type charge_statestruct_arraydist_collisional_transfer_0dObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		status, ret_power_th = ull.getDoubleFromObject(self.idx, obj, cpopath + 'power_th', i)
		check_status(status)
		if not status:
			self.power_th = ret_power_th
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		status, ret_power_fast = ull.getDoubleFromObject(self.idx, obj, cpopath + 'power_fast', i)
		check_status(status)
		if not status:
			self.power_fast = ret_power_fast
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		status, ret_torque_th = ull.getDoubleFromObject(self.idx, obj, cpopath + 'torque_th', i)
		check_status(status)
		if not status:
			self.torque_th = ret_torque_th
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		status, ret_torque_fast = ull.getDoubleFromObject(self.idx, obj, cpopath + 'torque_fast', i)
		check_status(status)
		if not status:
			self.torque_fast = ret_torque_fast

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type charge_statestruct_arraydist_collisional_transfer_0dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type charge_statestruct_arraydist_collisional_transfer_0dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class sourcesstruct_arraydist_sources_0d:
	'''
	class sourcesstruct_arraydist_sources_0d
	Vector of volume integrated sources and sinks of particles, momentum and power included in the Fokker-Planck modelling. The physical meaning of each source term is specified through the identifier ./sources/type. Note that it is possible to store multiple source terms with the same value for ./source/type. Time-dependent; Scalar

	Attributes:
	- array : list of sourcesstruct_arraydist_sources_0dObj 
	'''

	def __init__(self, base_path_in='sources'):
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
		ret = space + 'class sourcesstruct_arraydist_sources_0d\n'
		for i in range(len(self.array)):
			ret = ret + space + 'sourcesstruct_arraydist_sources_0d[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(sourcesstruct_arraydist_sources_0dObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_0d, run function putSlice') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_0d, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_0d, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_0d, run function getSlice') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_0d, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(sourcesstruct_arraydist_sources_0d(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(sourcesstruct_arraydist_sources_0d(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = sourcesstruct_arraydist_sources_0d(self.base_path)
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_0d, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_0d, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_0d, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_0d, run function getNonTimedElt') 
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


class sourcesstruct_arraydist_sources_0dObj:
	'''
	class sourcesstruct_arraydist_sources_0dObj
	Vector of volume integrated sources and sinks of particles, momentum and power included in the Fokker-Planck modelling. The physical meaning of each source term is specified through the identifier ./sources/type. Note that it is possible to store multiple source terms with the same value for ./source/type. Time-dependent; Scalar

	Attributes:
	- source_ref : class source_refstructuredist_sources_reference
	   Reference identifying the origin and type of source; Time-dependedent
	- particle : float
	   Source (or sink) rate of particles [1/s]; Time-dependedent; Scalar
	- momentum : float
	   Source (or sink) rate of toroidal angular momentum [Nm/s]; Time-dependedent; Scalar
	- energy : float
	   Source (or sink) rate of energy [J/s]; Time-dependedent; Scalar
	'''

	def __init__(self, base_path_in='sources'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.source_ref = source_refstructuredist_sources_reference('source_ref')
		self.particle = EMPTY_DOUBLE
		self.momentum = EMPTY_DOUBLE
		self.energy = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class sourcesstruct_arraydist_sources_0dObj\n'
		ret = ret + space + 'Attribute source_ref\n ' + self.source_ref.__str__(depth+1)
		ret = ret + space + 'Attribute particle: ' + str(self.particle) + '\n'
		ret = ret + space + 'Attribute momentum: ' + str(self.momentum) + '\n'
		ret = ret + space + 'Attribute energy: ' + str(self.energy) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.source_ref.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcesstruct_arraydist_sources_0dObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcesstruct_arraydist_sources_0dObj, run function getTimedElt') 
		cpopath = cpopath + '/'

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcesstruct_arraydist_sources_0dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.source_ref.putNonTimedElt(path, cpopath + 'source_ref', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'particle') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'particle', i, self.particle)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'momentum') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'momentum', i, self.momentum)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'energy', i, self.energy)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcesstruct_arraydist_sources_0dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.source_ref.getNonTimedElt(path, cpopath + 'source_ref', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'particle') 
			print ('obj = ' + str(obj))
		status, ret_particle = ull.getDoubleFromObject(self.idx, obj, cpopath + 'particle', i)
		check_status(status)
		if not status:
			self.particle = ret_particle
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'momentum') 
			print ('obj = ' + str(obj))
		status, ret_momentum = ull.getDoubleFromObject(self.idx, obj, cpopath + 'momentum', i)
		check_status(status)
		if not status:
			self.momentum = ret_momentum
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		status, ret_energy = ull.getDoubleFromObject(self.idx, obj, cpopath + 'energy', i)
		check_status(status)
		if not status:
			self.energy = ret_energy


class source_refstructuredist_sources_reference:
	'''
	class source_refstructuredist_sources_reference
	Reference identifying the origin and type of source; Time-dependedent

	Attributes:
	- type : class typestructureidentifier
	   Identifier for sources and sinks in Fokker-Planck solver; type.flag=1 for wave source, type.flag=2 for particle source, etc (see fokker_planck_source_identifier_definition in the Documentation website under Conventions/Enumerated_datatypes); Time-dependedent
	- index_waveid : numpy.ndarray 1D with int)
	   Index pointing to /distribution/distri_vec/wave_id[index_waveid] from which the source is taken. Time-dependedent; Vector (npsi)
	- index_srcid : numpy.ndarray 1D with int)
	   Index pointing to /distribution/distri_vec/source_id[index_waveid] from which the source is taken. Time-dependedent; Vector (npsi)
	'''

	def __init__(self, base_path_in='source_ref'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.type = typestructureidentifier('type')
		self.index_waveid = numpy.zeros(0, numpy.int32, order='C')
		self.index_srcid = numpy.zeros(0, numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class source_refstructuredist_sources_reference\n'
		ret = ret + space + 'Attribute type\n ' + self.type.__str__(depth+1)
		s = self.index_waveid.__str__()
		ret = ret + space + 'Attribute index_waveid\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.index_srcid.__str__()
		ret = ret + space + 'Attribute index_srcid\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.type.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_refstructuredist_sources_reference, run function putSlice') 
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
			print ('field '+self.base_path+' of type source_refstructuredist_sources_reference, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type source_refstructuredist_sources_reference, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.putNonTimed(path, cpopath)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'index_waveid', numpy.array(self.index_waveid).astype(numpy.int32), False)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'index_srcid', numpy.array(self.index_srcid).astype(numpy.int32), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type source_refstructuredist_sources_reference, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_index_waveid = ull.getVect1DInt(self.idx, path, cpopath + 'index_waveid')
		check_status(status)
		if not status:
			self.index_waveid = ret_index_waveid
		status, ret_index_srcid = ull.getVect1DInt(self.idx, path, cpopath + 'index_srcid')
		check_status(status)
		if not status:
			self.index_srcid = ret_index_srcid

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type source_refstructuredist_sources_reference, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			typeList = self.type.build_non_resampled_data(path, cpopath, nbslice)
			status, index_waveidVal = ull.getVect1DInt(self.idx, path, cpopath + 'index_waveid')
			check_status(status)
			status, index_srcidVal = ull.getVect1DInt(self.idx, path, cpopath + 'index_srcid')
			check_status(status)
			for i in range(nbslice):
				slice = source_refstructuredist_sources_reference(self.base_path)
				slice.setExpIdx(self.idx)
				slice.type = typeList[i]
				slice.index_waveid = index_waveidVal
				slice.index_srcid = index_srcidVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_refstructuredist_sources_referenceObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_refstructuredist_sources_referenceObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_refstructuredist_sources_referenceObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.type.putNonTimedElt(path, cpopath + 'type', i, obj)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'index_waveid') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'index_waveid', i, numpy.array(self.index_waveid).astype(numpy.int32))
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'index_srcid') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'index_srcid', i, numpy.array(self.index_srcid).astype(numpy.int32))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type source_refstructuredist_sources_referenceObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.type.getNonTimedElt(path, cpopath + 'type', i, obj)
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'index_waveid') 
			print ('obj = ' + str(obj))
		status, ret_index_waveid = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'index_waveid', i)
		check_status(status)
		if not status:
			self.index_waveid = ret_index_waveid
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'index_srcid') 
			print ('obj = ' + str(obj))
		status, ret_index_srcid = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'index_srcid', i)
		check_status(status)
		if not status:
			self.index_srcid = ret_index_srcid

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.type.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'index_waveid')
		ull.deleteData(self.idx, path, cpopath + 'index_srcid')


class profiles_1dstructuredist_profiles_1d:
	'''
	class profiles_1dstructuredist_profiles_1d
	Flux surface averaged profiles.

	Attributes:
	- geometry : class geometrystructuredist_geometry_1d
	   Grids and metric information; including rho_tor, psi, area and volume. Time-dependent
	- state : class statestructuredist_state_1d
	   Fluid moments describing the state of the distribution; calculated from the distribution. Time-dependent
	- collisions_e : class collisions_estructuredist_collisional_transfer_1d
	   Collisional exchange from the background electrons to the distribution function. Time-dependent
	- collisions_i : class collisions_istruct_arraydist_collisional_transfer_1d: array of collisions_istruct_arraydist_collisional_transfer_1dObj objects
	   Collisional exchange from each background ion speices to the distribution function. Time-dependent; Vector (nions)
	- collisions_z : class collisions_zstruct_arraydist_profiles_1d_collisions_z: array of collisions_zstruct_arraydist_profiles_1d_collisions_zObj objects
	   Collisional exchange from each background impurities species to the distribution function. Time-dependent; Vector (nimpur)
	- thermalised : class thermalisedstructuredist_thermalised_1d
	   Representation of the flux surface averaged source of thermal particles, momentum and energy due to thermalisation. Here thermalisation refers to non-thermal particles, sufficiently assimilated to the thermal background to be re-categorised as thermal particles. Note that this source may also be negative if thermal particles are being accelerated such that they form a distinct non-thermal contribution, e.g. due run-away of RF interactions.
	- sources : class sourcesstruct_arraydist_sources_1d: array of sourcesstruct_arraydist_sources_1dObj objects
	   Vector of flux surface averaged sources and sinks of particles, momentum and power included in the Fokker-Planck modelling. The physical meaning of each source term is specified through the identifier ./sources/type. Note that it is possible to store multiple source terms with the same value for source/type. Time-dependent; Vector(n_source_terms)
	- trapped : class trappedstructuredist_profile_values_1d
	   Flux surface averaged profile evaluated using the trapped particle part of the distribution.
	- co_passing : class co_passingstructuredist_profile_values_1d
	   Flux surface averaged profile evaluated using the co-current passing particle part of the distribution.
	- cntr_passing : class cntr_passingstructuredist_profile_values_1d
	   Flux surface averaged profile evaluated using the counter-current passing particle part of the distribution.
	'''

	def __init__(self, base_path_in='profiles_1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.geometry = geometrystructuredist_geometry_1d('geometry')
		self.state = statestructuredist_state_1d('state')
		self.collisions_e = collisions_estructuredist_collisional_transfer_1d('collisions_e')
		self.collisions_i = collisions_istruct_arraydist_collisional_transfer_1d('collisions_i')
		self.collisions_z = collisions_zstruct_arraydist_profiles_1d_collisions_z('collisions_z')
		self.thermalised = thermalisedstructuredist_thermalised_1d('thermalised')
		self.sources = sourcesstruct_arraydist_sources_1d('sources')
		self.trapped = trappedstructuredist_profile_values_1d('trapped')
		self.co_passing = co_passingstructuredist_profile_values_1d('co_passing')
		self.cntr_passing = cntr_passingstructuredist_profile_values_1d('cntr_passing')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class profiles_1dstructuredist_profiles_1d\n'
		ret = ret + space + 'Attribute geometry\n ' + self.geometry.__str__(depth+1)
		ret = ret + space + 'Attribute state\n ' + self.state.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_e\n ' + self.collisions_e.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_i\n ' + self.collisions_i.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_z\n ' + self.collisions_z.__str__(depth+1)
		ret = ret + space + 'Attribute thermalised\n ' + self.thermalised.__str__(depth+1)
		ret = ret + space + 'Attribute sources\n ' + self.sources.__str__(depth+1)
		ret = ret + space + 'Attribute trapped\n ' + self.trapped.__str__(depth+1)
		ret = ret + space + 'Attribute co_passing\n ' + self.co_passing.__str__(depth+1)
		ret = ret + space + 'Attribute cntr_passing\n ' + self.cntr_passing.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.geometry.setExpIdx(idx)
		self.state.setExpIdx(idx)
		self.collisions_e.setExpIdx(idx)
		self.collisions_i.setExpIdx(idx)
		self.collisions_z.setExpIdx(idx)
		self.thermalised.setExpIdx(idx)
		self.sources.setExpIdx(idx)
		self.trapped.setExpIdx(idx)
		self.co_passing.setExpIdx(idx)
		self.cntr_passing.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructuredist_profiles_1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.cpoTime = self.cpoTime
		self.geometry.putSlice(path, cpopath)
		self.state.cpoTime = self.cpoTime
		self.state.putSlice(path, cpopath)
		self.collisions_e.cpoTime = self.cpoTime
		self.collisions_e.putSlice(path, cpopath)
		self.collisions_i.cpoTime = self.cpoTime
		self.collisions_i.putSlice(path, cpopath)
		self.collisions_z.cpoTime = self.cpoTime
		self.collisions_z.putSlice(path, cpopath)
		self.thermalised.cpoTime = self.cpoTime
		self.thermalised.putSlice(path, cpopath)
		self.sources.cpoTime = self.cpoTime
		self.sources.putSlice(path, cpopath)
		self.trapped.cpoTime = self.cpoTime
		self.trapped.putSlice(path, cpopath)
		self.co_passing.cpoTime = self.cpoTime
		self.co_passing.putSlice(path, cpopath)
		self.cntr_passing.cpoTime = self.cpoTime
		self.cntr_passing.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructuredist_profiles_1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.replaceLastSlice(path, cpopath)
		self.state.replaceLastSlice(path, cpopath)
		self.collisions_e.replaceLastSlice(path, cpopath)
		self.collisions_i.replaceLastSlice(path, cpopath)
		self.collisions_z.replaceLastSlice(path, cpopath)
		self.thermalised.replaceLastSlice(path, cpopath)
		self.sources.replaceLastSlice(path, cpopath)
		self.trapped.replaceLastSlice(path, cpopath)
		self.co_passing.replaceLastSlice(path, cpopath)
		self.cntr_passing.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructuredist_profiles_1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.putNonTimed(path, cpopath)
		self.state.putNonTimed(path, cpopath)
		self.collisions_e.putNonTimed(path, cpopath)
		self.collisions_i.putNonTimed(path, cpopath)
		self.collisions_z.putNonTimed(path, cpopath)
		self.thermalised.putNonTimed(path, cpopath)
		self.sources.putNonTimed(path, cpopath)
		self.trapped.putNonTimed(path, cpopath)
		self.co_passing.putNonTimed(path, cpopath)
		self.cntr_passing.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructuredist_profiles_1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.getSlice(path, cpopath, inTime, interpolMode)
		self.state.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_e.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_i.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_z.getSlice(path, cpopath, inTime, interpolMode)
		self.thermalised.getSlice(path, cpopath, inTime, interpolMode)
		self.sources.getSlice(path, cpopath, inTime, interpolMode)
		self.trapped.getSlice(path, cpopath, inTime, interpolMode)
		self.co_passing.getSlice(path, cpopath, inTime, interpolMode)
		self.cntr_passing.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructuredist_profiles_1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			geometryList = self.geometry.build_non_resampled_data(path, cpopath, nbslice)
			stateList = self.state.build_non_resampled_data(path, cpopath, nbslice)
			collisions_eList = self.collisions_e.build_non_resampled_data(path, cpopath, nbslice)
			collisions_iList = self.collisions_i.build_non_resampled_data(path, cpopath, nbslice)
			collisions_zList = self.collisions_z.build_non_resampled_data(path, cpopath, nbslice)
			thermalisedList = self.thermalised.build_non_resampled_data(path, cpopath, nbslice)
			sourcesList = self.sources.build_non_resampled_data(path, cpopath, nbslice)
			trappedList = self.trapped.build_non_resampled_data(path, cpopath, nbslice)
			co_passingList = self.co_passing.build_non_resampled_data(path, cpopath, nbslice)
			cntr_passingList = self.cntr_passing.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = profiles_1dstructuredist_profiles_1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.geometry = geometryList[i]
				slice.state = stateList[i]
				slice.collisions_e = collisions_eList[i]
				slice.collisions_i = collisions_iList[i]
				slice.collisions_z = collisions_zList[i]
				slice.thermalised = thermalisedList[i]
				slice.sources = sourcesList[i]
				slice.trapped = trappedList[i]
				slice.co_passing = co_passingList[i]
				slice.cntr_passing = cntr_passingList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructuredist_profiles_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.geometry.putTimedElt(path, cpopath + 'geometry', i, obj)
		obj = self.state.putTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putTimedElt(path, cpopath + 'collisions_z', i, obj)
		obj = self.sources.putTimedElt(path, cpopath + 'sources', i, obj)
		obj = self.trapped.putTimedElt(path, cpopath + 'trapped', i, obj)
		obj = self.co_passing.putTimedElt(path, cpopath + 'co_passing', i, obj)
		obj = self.cntr_passing.putTimedElt(path, cpopath + 'cntr_passing', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructuredist_profiles_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.geometry.getTimedElt(path, cpopath + 'geometry', i, obj)
		self.state.getTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getTimedElt(path, cpopath + 'collisions_z', i, obj)
		self.sources.getTimedElt(path, cpopath + 'sources', i, obj)
		self.trapped.getTimedElt(path, cpopath + 'trapped', i, obj)
		self.co_passing.getTimedElt(path, cpopath + 'co_passing', i, obj)
		self.cntr_passing.getTimedElt(path, cpopath + 'cntr_passing', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructuredist_profiles_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.geometry.putNonTimedElt(path, cpopath + 'geometry', i, obj)
		obj = self.state.putNonTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		obj = self.thermalised.putNonTimedElt(path, cpopath + 'thermalised', i, obj)
		obj = self.sources.putNonTimedElt(path, cpopath + 'sources', i, obj)
		obj = self.trapped.putNonTimedElt(path, cpopath + 'trapped', i, obj)
		obj = self.co_passing.putNonTimedElt(path, cpopath + 'co_passing', i, obj)
		obj = self.cntr_passing.putNonTimedElt(path, cpopath + 'cntr_passing', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructuredist_profiles_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.geometry.getNonTimedElt(path, cpopath + 'geometry', i, obj)
		self.state.getNonTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		self.thermalised.getNonTimedElt(path, cpopath + 'thermalised', i, obj)
		self.sources.getNonTimedElt(path, cpopath + 'sources', i, obj)
		self.trapped.getNonTimedElt(path, cpopath + 'trapped', i, obj)
		self.co_passing.getNonTimedElt(path, cpopath + 'co_passing', i, obj)
		self.cntr_passing.getNonTimedElt(path, cpopath + 'cntr_passing', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.deleteData(path, cpopath)
		self.state.deleteData(path, cpopath)
		self.collisions_e.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'collisions_i')
		ull.deleteData(self.idx, path, cpopath + 'collisions_z')
		self.thermalised.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'sources')
		self.trapped.deleteData(path, cpopath)
		self.co_passing.deleteData(path, cpopath)
		self.cntr_passing.deleteData(path, cpopath)


class geometrystructuredist_geometry_1d:
	'''
	class geometrystructuredist_geometry_1d
	Grids and metric information; including rho_tor, psi, area and volume. Time-dependent

	Attributes:
	- rho_tor : numpy.ndarray 1D with float
	   Toroidal flux coordinate [m]. Defined as sqrt((phi-phi_axis)/pi/B0), where B0=../global_param/toroid_field/b0, phi is the toroidal flux and phi_axis is the toroidal flux at the magnetic axis. Time-dependent; Vector (npsi)
	- rho_tor_norm : numpy.ndarray 1D with float
	   The toroidal flux coordinate normalised to be zero at the axis and unity at the last closed flux surface, or last available fluxsurface if the last closed flux surface is not defined. Time-dependent; Vector (npsi)
	- psi : numpy.ndarray 1D with float
	   Poloidal flux at the grid points for 1D profiles [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent; Vector (npsi)
	- volume : numpy.ndarray 1D with float
	   Volume enclosed by the flux surface [m^3]; Time-dependent; Vector (npsi)
	- area : numpy.ndarray 1D with float
	   Cross-sectional area of the flux surface [m^2]; Time-dependent; Vector (npsi)
	'''

	def __init__(self, base_path_in='geometry'):
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
		ret = space + 'class geometrystructuredist_geometry_1d\n'
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
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_1d, run function putSlice') 
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
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_1d, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_1d, run function getSlice') 
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
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_1d, run function build_non_resampled_data') 
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
				slice = geometrystructuredist_geometry_1d(self.base_path)
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
			print ('object of type geometrystructuredist_geometry_1dObj, run function putTimedElt') 
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
			print ('object of type geometrystructuredist_geometry_1dObj, run function getTimedElt') 
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
			print ('object of type geometrystructuredist_geometry_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geometrystructuredist_geometry_1dObj, run function getNonTimedElt') 
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


class statestructuredist_state_1d:
	'''
	class statestructuredist_state_1d
	Fluid moments describing the state of the distribution; calculated from the distribution. Time-dependent

	Attributes:
	- dens : numpy.ndarray 1D with float
	   Flux surface averaged particle density (including both thermal and fast particles) [1/m^3]; Time-dependent; Vector (npsi)
	- dens_fast : numpy.ndarray 1D with float
	   Flux surface averaged fast particle density [1/m^3]; Time-dependent; Vector (npsi)
	- pres : numpy.ndarray 1D with float
	   Scalar pressure (including both thermal and fast particles) [J/m^3]. Related to the energy content, W, according to: pres=2*W/3. Time-dependent; Vector (npsi)
	- pres_fast : numpy.ndarray 1D with float
	   Scalar pressure of the fast particles [J/m^3]. Related to the fast particle energy content, Wf, according to: pres_fast=2*Wf/3. Time-dependent; Vector (npsi)
	- pres_fast_pa : numpy.ndarray 1D with float
	   Parallel pressure of the fast particles [J/m^3]. Related to the fast particle parallel energy content, Wfpar, according to: pres_fast_pa=2*Wfpar. Time-dependent; Vector (npsi)
	- momentm_fast : numpy.ndarray 1D with float
	   Kinetic toroidal angular momentum density of the fast ions [Ns/m^2]; Time-dependent; Vector (npsi)
	- current : numpy.ndarray 1D with float
	   Total toroidal driven current density (including electron and thermal ion back-current, or drag-current) [A/m^3]; Time-dependent; Vector (npsi)
	- current_fast : numpy.ndarray 1D with float
	   Flux surface averaged toroidal current density of fast (non-thermal) particles (excluding electron and thermal ion back-current, or drag-current) [A.m^-2]; Time-dependent; Vector (npsi).
	- torque_jrxb : numpy.ndarray 1D with float
	   Toroidal torque density due to radial currents, excluding radial current due to neoclassical effect [N/m^2]; Time-dependent; Vector (npsi)
	'''

	def __init__(self, base_path_in='state'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dens = numpy.zeros(0, numpy.float64, order='C')
		self.dens_fast = numpy.zeros(0, numpy.float64, order='C')
		self.pres = numpy.zeros(0, numpy.float64, order='C')
		self.pres_fast = numpy.zeros(0, numpy.float64, order='C')
		self.pres_fast_pa = numpy.zeros(0, numpy.float64, order='C')
		self.momentm_fast = numpy.zeros(0, numpy.float64, order='C')
		self.current = numpy.zeros(0, numpy.float64, order='C')
		self.current_fast = numpy.zeros(0, numpy.float64, order='C')
		self.torque_jrxb = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class statestructuredist_state_1d\n'
		s = self.dens.__str__()
		ret = ret + space + 'Attribute dens\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dens_fast.__str__()
		ret = ret + space + 'Attribute dens_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pres.__str__()
		ret = ret + space + 'Attribute pres\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pres_fast.__str__()
		ret = ret + space + 'Attribute pres_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pres_fast_pa.__str__()
		ret = ret + space + 'Attribute pres_fast_pa\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.momentm_fast.__str__()
		ret = ret + space + 'Attribute momentm_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.current.__str__()
		ret = ret + space + 'Attribute current\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.current_fast.__str__()
		ret = ret + space + 'Attribute current_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_jrxb.__str__()
		ret = ret + space + 'Attribute torque_jrxb\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dens', numpy.array(self.dens).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dens_fast', numpy.array(self.dens_fast).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pres', numpy.array(self.pres).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pres_fast', numpy.array(self.pres_fast).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pres_fast_pa', numpy.array(self.pres_fast_pa).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'momentm_fast', numpy.array(self.momentm_fast).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'current', numpy.array(self.current).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'current_fast', numpy.array(self.current_fast).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'torque_jrxb', numpy.array(self.torque_jrxb).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dens', numpy.array(self.dens).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dens_fast', numpy.array(self.dens_fast).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pres', numpy.array(self.pres).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pres_fast', numpy.array(self.pres_fast).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pres_fast_pa', numpy.array(self.pres_fast_pa).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'momentm_fast', numpy.array(self.momentm_fast).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'current', numpy.array(self.current).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'current_fast', numpy.array(self.current_fast).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'torque_jrxb', numpy.array(self.torque_jrxb).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dens, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dens', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dens = ret_dens
			self.cpoTime = retTime
		status, ret_dens_fast, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dens_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dens_fast = ret_dens_fast
			self.cpoTime = retTime
		status, ret_pres, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pres', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pres = ret_pres
			self.cpoTime = retTime
		status, ret_pres_fast, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pres_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pres_fast = ret_pres_fast
			self.cpoTime = retTime
		status, ret_pres_fast_pa, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pres_fast_pa', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pres_fast_pa = ret_pres_fast_pa
			self.cpoTime = retTime
		status, ret_momentm_fast, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'momentm_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.momentm_fast = ret_momentm_fast
			self.cpoTime = retTime
		status, ret_current, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'current', inTime, interpolMode)
		check_status(status)
		if not status:
			self.current = ret_current
			self.cpoTime = retTime
		status, ret_current_fast, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'current_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.current_fast = ret_current_fast
			self.cpoTime = retTime
		status, ret_torque_jrxb, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'torque_jrxb', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_jrxb = ret_torque_jrxb
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, densList = ull.getVect2DDouble(self.idx, path, cpopath + 'dens')
			if len(densList) == 0:
				densList = numpy.resize(densList, (0,nbslice))
			check_status(status)
			status, dens_fastList = ull.getVect2DDouble(self.idx, path, cpopath + 'dens_fast')
			if len(dens_fastList) == 0:
				dens_fastList = numpy.resize(dens_fastList, (0,nbslice))
			check_status(status)
			status, presList = ull.getVect2DDouble(self.idx, path, cpopath + 'pres')
			if len(presList) == 0:
				presList = numpy.resize(presList, (0,nbslice))
			check_status(status)
			status, pres_fastList = ull.getVect2DDouble(self.idx, path, cpopath + 'pres_fast')
			if len(pres_fastList) == 0:
				pres_fastList = numpy.resize(pres_fastList, (0,nbslice))
			check_status(status)
			status, pres_fast_paList = ull.getVect2DDouble(self.idx, path, cpopath + 'pres_fast_pa')
			if len(pres_fast_paList) == 0:
				pres_fast_paList = numpy.resize(pres_fast_paList, (0,nbslice))
			check_status(status)
			status, momentm_fastList = ull.getVect2DDouble(self.idx, path, cpopath + 'momentm_fast')
			if len(momentm_fastList) == 0:
				momentm_fastList = numpy.resize(momentm_fastList, (0,nbslice))
			check_status(status)
			status, currentList = ull.getVect2DDouble(self.idx, path, cpopath + 'current')
			if len(currentList) == 0:
				currentList = numpy.resize(currentList, (0,nbslice))
			check_status(status)
			status, current_fastList = ull.getVect2DDouble(self.idx, path, cpopath + 'current_fast')
			if len(current_fastList) == 0:
				current_fastList = numpy.resize(current_fastList, (0,nbslice))
			check_status(status)
			status, torque_jrxbList = ull.getVect2DDouble(self.idx, path, cpopath + 'torque_jrxb')
			if len(torque_jrxbList) == 0:
				torque_jrxbList = numpy.resize(torque_jrxbList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = statestructuredist_state_1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dens = densList[:,i]
				slice.dens_fast = dens_fastList[:,i]
				slice.pres = presList[:,i]
				slice.pres_fast = pres_fastList[:,i]
				slice.pres_fast_pa = pres_fast_paList[:,i]
				slice.momentm_fast = momentm_fastList[:,i]
				slice.current = currentList[:,i]
				slice.current_fast = current_fastList[:,i]
				slice.torque_jrxb = torque_jrxbList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type statestructuredist_state_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dens') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dens', i, numpy.array(self.dens).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dens_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dens_fast', i, numpy.array(self.dens_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pres') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pres', i, numpy.array(self.pres).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pres_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pres_fast', i, numpy.array(self.pres_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pres_fast_pa') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pres_fast_pa', i, numpy.array(self.pres_fast_pa).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'momentm_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'momentm_fast', i, numpy.array(self.momentm_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'current') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'current', i, numpy.array(self.current).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'current_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'current_fast', i, numpy.array(self.current_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'torque_jrxb') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'torque_jrxb', i, numpy.array(self.torque_jrxb).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type statestructuredist_state_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dens') 
			print ('obj = ' + str(obj))
		status, ret_dens = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dens', i)
		check_status(status)
		if not status:
			self.dens = ret_dens
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dens_fast') 
			print ('obj = ' + str(obj))
		status, ret_dens_fast = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dens_fast', i)
		check_status(status)
		if not status:
			self.dens_fast = ret_dens_fast
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pres') 
			print ('obj = ' + str(obj))
		status, ret_pres = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pres', i)
		check_status(status)
		if not status:
			self.pres = ret_pres
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pres_fast') 
			print ('obj = ' + str(obj))
		status, ret_pres_fast = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pres_fast', i)
		check_status(status)
		if not status:
			self.pres_fast = ret_pres_fast
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pres_fast_pa') 
			print ('obj = ' + str(obj))
		status, ret_pres_fast_pa = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pres_fast_pa', i)
		check_status(status)
		if not status:
			self.pres_fast_pa = ret_pres_fast_pa
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'momentm_fast') 
			print ('obj = ' + str(obj))
		status, ret_momentm_fast = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'momentm_fast', i)
		check_status(status)
		if not status:
			self.momentm_fast = ret_momentm_fast
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'current') 
			print ('obj = ' + str(obj))
		status, ret_current = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'current', i)
		check_status(status)
		if not status:
			self.current = ret_current
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'current_fast') 
			print ('obj = ' + str(obj))
		status, ret_current_fast = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'current_fast', i)
		check_status(status)
		if not status:
			self.current_fast = ret_current_fast
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'torque_jrxb') 
			print ('obj = ' + str(obj))
		status, ret_torque_jrxb = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'torque_jrxb', i)
		check_status(status)
		if not status:
			self.torque_jrxb = ret_torque_jrxb

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type statestructuredist_state_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type statestructuredist_state_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'dens')
		ull.deleteData(self.idx, path, cpopath + 'dens_fast')
		ull.deleteData(self.idx, path, cpopath + 'pres')
		ull.deleteData(self.idx, path, cpopath + 'pres_fast')
		ull.deleteData(self.idx, path, cpopath + 'pres_fast_pa')
		ull.deleteData(self.idx, path, cpopath + 'momentm_fast')
		ull.deleteData(self.idx, path, cpopath + 'current')
		ull.deleteData(self.idx, path, cpopath + 'current_fast')
		ull.deleteData(self.idx, path, cpopath + 'torque_jrxb')


class collisions_estructuredist_collisional_transfer_1d:
	'''
	class collisions_estructuredist_collisional_transfer_1d
	Collisional exchange from the background electrons to the distribution function. Time-dependent

	Attributes:
	- power_th : numpy.ndarray 1D with float
	   Flux surface averaged collisional power density to the thermal particle population [W.m^-3]; Time-dependent; Vector(npsi)
	- power_fast : numpy.ndarray 1D with float
	   Flux surface averaged collisional power density to the fast particle population [W.m^-3]; Time-dependent; Vector(npsi)
	- torque_th : numpy.ndarray 1D with float
	   Flux surface averaged collisional toroidal torque density to the thermal particle population [N.m^-2]; Time-dependent; Vector(npsi)
	- torque_fast : numpy.ndarray 1D with float
	   Flux surface averaged collisional toroidal torque density to the fast particle population [N.m^-2]; Time-dependent; Vector(npsi)
	'''

	def __init__(self, base_path_in='collisions_e'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.power_th = numpy.zeros(0, numpy.float64, order='C')
		self.power_fast = numpy.zeros(0, numpy.float64, order='C')
		self.torque_th = numpy.zeros(0, numpy.float64, order='C')
		self.torque_fast = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class collisions_estructuredist_collisional_transfer_1d\n'
		s = self.power_th.__str__()
		ret = ret + space + 'Attribute power_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.power_fast.__str__()
		ret = ret + space + 'Attribute power_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_th.__str__()
		ret = ret + space + 'Attribute torque_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_fast.__str__()
		ret = ret + space + 'Attribute torque_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'power_th', numpy.array(self.power_th).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'power_fast', numpy.array(self.power_fast).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'torque_th', numpy.array(self.torque_th).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'torque_fast', numpy.array(self.torque_fast).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'power_th', numpy.array(self.power_th).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'power_fast', numpy.array(self.power_fast).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'torque_th', numpy.array(self.torque_th).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'torque_fast', numpy.array(self.torque_fast).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_power_th, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'power_th', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power_th = ret_power_th
			self.cpoTime = retTime
		status, ret_power_fast, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'power_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power_fast = ret_power_fast
			self.cpoTime = retTime
		status, ret_torque_th, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'torque_th', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_th = ret_torque_th
			self.cpoTime = retTime
		status, ret_torque_fast, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'torque_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_fast = ret_torque_fast
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, power_thList = ull.getVect2DDouble(self.idx, path, cpopath + 'power_th')
			if len(power_thList) == 0:
				power_thList = numpy.resize(power_thList, (0,nbslice))
			check_status(status)
			status, power_fastList = ull.getVect2DDouble(self.idx, path, cpopath + 'power_fast')
			if len(power_fastList) == 0:
				power_fastList = numpy.resize(power_fastList, (0,nbslice))
			check_status(status)
			status, torque_thList = ull.getVect2DDouble(self.idx, path, cpopath + 'torque_th')
			if len(torque_thList) == 0:
				torque_thList = numpy.resize(torque_thList, (0,nbslice))
			check_status(status)
			status, torque_fastList = ull.getVect2DDouble(self.idx, path, cpopath + 'torque_fast')
			if len(torque_fastList) == 0:
				torque_fastList = numpy.resize(torque_fastList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = collisions_estructuredist_collisional_transfer_1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.power_th = power_thList[:,i]
				slice.power_fast = power_fastList[:,i]
				slice.torque_th = torque_thList[:,i]
				slice.torque_fast = torque_fastList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_estructuredist_collisional_transfer_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'power_th', i, numpy.array(self.power_th).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'power_fast', i, numpy.array(self.power_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'torque_th', i, numpy.array(self.torque_th).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'torque_fast', i, numpy.array(self.torque_fast).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_estructuredist_collisional_transfer_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		status, ret_power_th = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'power_th', i)
		check_status(status)
		if not status:
			self.power_th = ret_power_th
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		status, ret_power_fast = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'power_fast', i)
		check_status(status)
		if not status:
			self.power_fast = ret_power_fast
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		status, ret_torque_th = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'torque_th', i)
		check_status(status)
		if not status:
			self.torque_th = ret_torque_th
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		status, ret_torque_fast = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'torque_fast', i)
		check_status(status)
		if not status:
			self.torque_fast = ret_torque_fast

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_estructuredist_collisional_transfer_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_estructuredist_collisional_transfer_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'power_th')
		ull.deleteData(self.idx, path, cpopath + 'power_fast')
		ull.deleteData(self.idx, path, cpopath + 'torque_th')
		ull.deleteData(self.idx, path, cpopath + 'torque_fast')


class collisions_istruct_arraydist_collisional_transfer_1d:
	'''
	class collisions_istruct_arraydist_collisional_transfer_1d
	Collisional exchange from each background ion speices to the distribution function. Time-dependent; Vector (nions)

	Attributes:
	- array : list of collisions_istruct_arraydist_collisional_transfer_1dObj 
	'''

	def __init__(self, base_path_in='collisions_i'):
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
		ret = space + 'class collisions_istruct_arraydist_collisional_transfer_1d\n'
		for i in range(len(self.array)):
			ret = ret + space + 'collisions_istruct_arraydist_collisional_transfer_1d[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(collisions_istruct_arraydist_collisional_transfer_1dObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_1d, run function putSlice') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_1d, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_1d, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_1d, run function getSlice') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_1d, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(collisions_istruct_arraydist_collisional_transfer_1d(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(collisions_istruct_arraydist_collisional_transfer_1d(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = collisions_istruct_arraydist_collisional_transfer_1d(self.base_path)
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_1d, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_1d, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_1d, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_1d, run function getNonTimedElt') 
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


class collisions_istruct_arraydist_collisional_transfer_1dObj:
	'''
	class collisions_istruct_arraydist_collisional_transfer_1dObj
	Collisional exchange from each background ion speices to the distribution function. Time-dependent; Vector (nions)

	Attributes:
	- power_th : numpy.ndarray 1D with float
	   Flux surface averaged collisional power density to the thermal particle population [W.m^-3]; Time-dependent; Vector(npsi)
	- power_fast : numpy.ndarray 1D with float
	   Flux surface averaged collisional power density to the fast particle population [W.m^-3]; Time-dependent; Vector(npsi)
	- torque_th : numpy.ndarray 1D with float
	   Flux surface averaged collisional toroidal torque density to the thermal particle population [N.m^-2]; Time-dependent; Vector(npsi)
	- torque_fast : numpy.ndarray 1D with float
	   Flux surface averaged collisional toroidal torque density to the fast particle population [N.m^-2]; Time-dependent; Vector(npsi)
	'''

	def __init__(self, base_path_in='collisions_i'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.power_th = numpy.zeros(0, numpy.float64, order='C')
		self.power_fast = numpy.zeros(0, numpy.float64, order='C')
		self.torque_th = numpy.zeros(0, numpy.float64, order='C')
		self.torque_fast = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class collisions_istruct_arraydist_collisional_transfer_1dObj\n'
		s = self.power_th.__str__()
		ret = ret + space + 'Attribute power_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.power_fast.__str__()
		ret = ret + space + 'Attribute power_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_th.__str__()
		ret = ret + space + 'Attribute torque_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_fast.__str__()
		ret = ret + space + 'Attribute torque_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_istruct_arraydist_collisional_transfer_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'power_th', i, numpy.array(self.power_th).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'power_fast', i, numpy.array(self.power_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'torque_th', i, numpy.array(self.torque_th).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'torque_fast', i, numpy.array(self.torque_fast).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_istruct_arraydist_collisional_transfer_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		status, ret_power_th = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'power_th', i)
		check_status(status)
		if not status:
			self.power_th = ret_power_th
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		status, ret_power_fast = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'power_fast', i)
		check_status(status)
		if not status:
			self.power_fast = ret_power_fast
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		status, ret_torque_th = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'torque_th', i)
		check_status(status)
		if not status:
			self.torque_th = ret_torque_th
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		status, ret_torque_fast = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'torque_fast', i)
		check_status(status)
		if not status:
			self.torque_fast = ret_torque_fast

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_istruct_arraydist_collisional_transfer_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_istruct_arraydist_collisional_transfer_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class collisions_zstruct_arraydist_profiles_1d_collisions_z:
	'''
	class collisions_zstruct_arraydist_profiles_1d_collisions_z
	Collisional exchange from each background impurities species to the distribution function. Time-dependent; Vector (nimpur)

	Attributes:
	- array : list of collisions_zstruct_arraydist_profiles_1d_collisions_zObj 
	'''

	def __init__(self, base_path_in='collisions_z'):
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
		ret = space + 'class collisions_zstruct_arraydist_profiles_1d_collisions_z\n'
		for i in range(len(self.array)):
			ret = ret + space + 'collisions_zstruct_arraydist_profiles_1d_collisions_z[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(collisions_zstruct_arraydist_profiles_1d_collisions_zObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles_1d_collisions_z, run function putSlice') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles_1d_collisions_z, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles_1d_collisions_z, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles_1d_collisions_z, run function getSlice') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles_1d_collisions_z, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(collisions_zstruct_arraydist_profiles_1d_collisions_z(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(collisions_zstruct_arraydist_profiles_1d_collisions_z(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = collisions_zstruct_arraydist_profiles_1d_collisions_z(self.base_path)
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles_1d_collisions_z, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles_1d_collisions_z, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles_1d_collisions_z, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles_1d_collisions_z, run function getNonTimedElt') 
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


class collisions_zstruct_arraydist_profiles_1d_collisions_zObj:
	'''
	class collisions_zstruct_arraydist_profiles_1d_collisions_zObj
	Collisional exchange from each background impurities species to the distribution function. Time-dependent; Vector (nimpur)

	Attributes:
	- charge_state : class charge_statestruct_arraydist_collisional_transfer_1d: array of charge_statestruct_arraydist_collisional_transfer_1dObj objects
	   Collisional exchange from each charge state (or bundled charge state) to the distribution function. Time-dependent; Vector (nzimp)
	'''

	def __init__(self, base_path_in='collisions_z'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.charge_state = charge_statestruct_arraydist_collisional_transfer_1d('charge_state')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class collisions_zstruct_arraydist_profiles_1d_collisions_zObj\n'
		ret = ret + space + 'Attribute charge_state\n ' + self.charge_state.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.charge_state.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_zstruct_arraydist_profiles_1d_collisions_zObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.charge_state.putTimedElt(path, cpopath + 'charge_state', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_zstruct_arraydist_profiles_1d_collisions_zObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.charge_state.getTimedElt(path, cpopath + 'charge_state', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_zstruct_arraydist_profiles_1d_collisions_zObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.charge_state.putNonTimedElt(path, cpopath + 'charge_state', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_zstruct_arraydist_profiles_1d_collisions_zObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.charge_state.getNonTimedElt(path, cpopath + 'charge_state', i, obj)


class charge_statestruct_arraydist_collisional_transfer_1d:
	'''
	class charge_statestruct_arraydist_collisional_transfer_1d
	Collisional exchange from each charge state (or bundled charge state) to the distribution function. Time-dependent; Vector (nzimp)

	Attributes:
	- array : list of charge_statestruct_arraydist_collisional_transfer_1dObj 
	'''

	def __init__(self, base_path_in='charge_state'):
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
		ret = space + 'class charge_statestruct_arraydist_collisional_transfer_1d\n'
		for i in range(len(self.array)):
			ret = ret + space + 'charge_statestruct_arraydist_collisional_transfer_1d[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(charge_statestruct_arraydist_collisional_transfer_1dObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_1d, run function putSlice') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_1d, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_1d, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_1d, run function getSlice') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_1d, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(charge_statestruct_arraydist_collisional_transfer_1d(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(charge_statestruct_arraydist_collisional_transfer_1d(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = charge_statestruct_arraydist_collisional_transfer_1d(self.base_path)
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_1d, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_1d, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_1d, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_1d, run function getNonTimedElt') 
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


class charge_statestruct_arraydist_collisional_transfer_1dObj:
	'''
	class charge_statestruct_arraydist_collisional_transfer_1dObj
	Collisional exchange from each charge state (or bundled charge state) to the distribution function. Time-dependent; Vector (nzimp)

	Attributes:
	- power_th : numpy.ndarray 1D with float
	   Flux surface averaged collisional power density to the thermal particle population [W.m^-3]; Time-dependent; Vector(npsi)
	- power_fast : numpy.ndarray 1D with float
	   Flux surface averaged collisional power density to the fast particle population [W.m^-3]; Time-dependent; Vector(npsi)
	- torque_th : numpy.ndarray 1D with float
	   Flux surface averaged collisional toroidal torque density to the thermal particle population [N.m^-2]; Time-dependent; Vector(npsi)
	- torque_fast : numpy.ndarray 1D with float
	   Flux surface averaged collisional toroidal torque density to the fast particle population [N.m^-2]; Time-dependent; Vector(npsi)
	'''

	def __init__(self, base_path_in='charge_state'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.power_th = numpy.zeros(0, numpy.float64, order='C')
		self.power_fast = numpy.zeros(0, numpy.float64, order='C')
		self.torque_th = numpy.zeros(0, numpy.float64, order='C')
		self.torque_fast = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class charge_statestruct_arraydist_collisional_transfer_1dObj\n'
		s = self.power_th.__str__()
		ret = ret + space + 'Attribute power_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.power_fast.__str__()
		ret = ret + space + 'Attribute power_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_th.__str__()
		ret = ret + space + 'Attribute torque_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_fast.__str__()
		ret = ret + space + 'Attribute torque_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type charge_statestruct_arraydist_collisional_transfer_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'power_th', i, numpy.array(self.power_th).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'power_fast', i, numpy.array(self.power_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'torque_th', i, numpy.array(self.torque_th).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'torque_fast', i, numpy.array(self.torque_fast).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type charge_statestruct_arraydist_collisional_transfer_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		status, ret_power_th = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'power_th', i)
		check_status(status)
		if not status:
			self.power_th = ret_power_th
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		status, ret_power_fast = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'power_fast', i)
		check_status(status)
		if not status:
			self.power_fast = ret_power_fast
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		status, ret_torque_th = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'torque_th', i)
		check_status(status)
		if not status:
			self.torque_th = ret_torque_th
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		status, ret_torque_fast = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'torque_fast', i)
		check_status(status)
		if not status:
			self.torque_fast = ret_torque_fast

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type charge_statestruct_arraydist_collisional_transfer_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type charge_statestruct_arraydist_collisional_transfer_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class thermalisedstructuredist_thermalised_1d:
	'''
	class thermalisedstructuredist_thermalised_1d
	Representation of the flux surface averaged source of thermal particles, momentum and energy due to thermalisation. Here thermalisation refers to non-thermal particles, sufficiently assimilated to the thermal background to be re-categorised as thermal particles. Note that this source may also be negative if thermal particles are being accelerated such that they form a distinct non-thermal contribution, e.g. due run-away of RF interactions.

	Attributes:
	- particle : numpy.ndarray 1D with float
	   Source rate for the thermal particle density due to the thermalisation of fast (non-thermal) particles [1/s/m**3]; Time-dependedent; Vector (npsi)
	- momentum : numpy.ndarray 1D with float
	   Source rate for the toroidal angular momentum density within the thermal particle population due to the thermalisation of fast (non-thermal) particles [N/m**2]; Time-dependedent; Vector (npsi)
	- energy : numpy.ndarray 1D with float
	   Source rate for the energy density within the thermal particle population due to the thermalisation of fast (non-thermal) particles [W/m**3]; Time-dependedent; Vector (npsi)
	'''

	def __init__(self, base_path_in='thermalised'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.particle = numpy.zeros(0, numpy.float64, order='C')
		self.momentum = numpy.zeros(0, numpy.float64, order='C')
		self.energy = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class thermalisedstructuredist_thermalised_1d\n'
		s = self.particle.__str__()
		ret = ret + space + 'Attribute particle\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.momentum.__str__()
		ret = ret + space + 'Attribute momentum\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.energy.__str__()
		ret = ret + space + 'Attribute energy\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type thermalisedstructuredist_thermalised_1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type thermalisedstructuredist_thermalised_1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type thermalisedstructuredist_thermalised_1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'particle', numpy.array(self.particle).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'momentum', numpy.array(self.momentum).astype(numpy.float64), False)
		check_status(status)
		status = ull.putVect1DDouble(self.idx, path, cpopath + 'energy', numpy.array(self.energy).astype(numpy.float64), False)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type thermalisedstructuredist_thermalised_1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_particle = ull.getVect1DDouble(self.idx, path, cpopath + 'particle')
		check_status(status)
		if not status:
			self.particle = ret_particle
		status, ret_momentum = ull.getVect1DDouble(self.idx, path, cpopath + 'momentum')
		check_status(status)
		if not status:
			self.momentum = ret_momentum
		status, ret_energy = ull.getVect1DDouble(self.idx, path, cpopath + 'energy')
		check_status(status)
		if not status:
			self.energy = ret_energy

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type thermalisedstructuredist_thermalised_1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, particleVal = ull.getVect1DDouble(self.idx, path, cpopath + 'particle')
			check_status(status)
			status, momentumVal = ull.getVect1DDouble(self.idx, path, cpopath + 'momentum')
			check_status(status)
			status, energyVal = ull.getVect1DDouble(self.idx, path, cpopath + 'energy')
			check_status(status)
			for i in range(nbslice):
				slice = thermalisedstructuredist_thermalised_1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.particle = particleVal
				slice.momentum = momentumVal
				slice.energy = energyVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type thermalisedstructuredist_thermalised_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type thermalisedstructuredist_thermalised_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type thermalisedstructuredist_thermalised_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'particle') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'particle', i, numpy.array(self.particle).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'momentum') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'momentum', i, numpy.array(self.momentum).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'energy', i, numpy.array(self.energy).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type thermalisedstructuredist_thermalised_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'particle') 
			print ('obj = ' + str(obj))
		status, ret_particle = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'particle', i)
		check_status(status)
		if not status:
			self.particle = ret_particle
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'momentum') 
			print ('obj = ' + str(obj))
		status, ret_momentum = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'momentum', i)
		check_status(status)
		if not status:
			self.momentum = ret_momentum
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		status, ret_energy = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'energy', i)
		check_status(status)
		if not status:
			self.energy = ret_energy

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'particle')
		ull.deleteData(self.idx, path, cpopath + 'momentum')
		ull.deleteData(self.idx, path, cpopath + 'energy')


class sourcesstruct_arraydist_sources_1d:
	'''
	class sourcesstruct_arraydist_sources_1d
	Vector of flux surface averaged sources and sinks of particles, momentum and power included in the Fokker-Planck modelling. The physical meaning of each source term is specified through the identifier ./sources/type. Note that it is possible to store multiple source terms with the same value for source/type. Time-dependent; Vector(n_source_terms)

	Attributes:
	- array : list of sourcesstruct_arraydist_sources_1dObj 
	'''

	def __init__(self, base_path_in='sources'):
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
		ret = space + 'class sourcesstruct_arraydist_sources_1d\n'
		for i in range(len(self.array)):
			ret = ret + space + 'sourcesstruct_arraydist_sources_1d[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(sourcesstruct_arraydist_sources_1dObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_1d, run function putSlice') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_1d, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_1d, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_1d, run function getSlice') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_1d, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(sourcesstruct_arraydist_sources_1d(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(sourcesstruct_arraydist_sources_1d(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = sourcesstruct_arraydist_sources_1d(self.base_path)
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_1d, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_1d, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_1d, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type sourcesstruct_arraydist_sources_1d, run function getNonTimedElt') 
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


class sourcesstruct_arraydist_sources_1dObj:
	'''
	class sourcesstruct_arraydist_sources_1dObj
	Vector of flux surface averaged sources and sinks of particles, momentum and power included in the Fokker-Planck modelling. The physical meaning of each source term is specified through the identifier ./sources/type. Note that it is possible to store multiple source terms with the same value for source/type. Time-dependent; Vector(n_source_terms)

	Attributes:
	- source_ref : class source_refstructuredist_sources_reference
	   Reference identifying the origin and type of source; Time-dependedent
	- particle : numpy.ndarray 1D with float
	   Source (or sink) rate of particles density [1/s/m**3]; Time-dependedent; Vector (npsi)
	- momentum : numpy.ndarray 1D with float
	   Source (or sink) rate of toroidal angular momentum density [Nm/s/m**3]; Time-dependedent; Vector (npsi)
	- energy : numpy.ndarray 1D with float
	   Source (or sink) rate of energy density [J/s/m**3]; Time-dependedent; Vector (npsi)
	'''

	def __init__(self, base_path_in='sources'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.source_ref = source_refstructuredist_sources_reference('source_ref')
		self.particle = numpy.zeros(0, numpy.float64, order='C')
		self.momentum = numpy.zeros(0, numpy.float64, order='C')
		self.energy = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class sourcesstruct_arraydist_sources_1dObj\n'
		ret = ret + space + 'Attribute source_ref\n ' + self.source_ref.__str__(depth+1)
		s = self.particle.__str__()
		ret = ret + space + 'Attribute particle\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.momentum.__str__()
		ret = ret + space + 'Attribute momentum\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.energy.__str__()
		ret = ret + space + 'Attribute energy\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.source_ref.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcesstruct_arraydist_sources_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcesstruct_arraydist_sources_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/'

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcesstruct_arraydist_sources_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.source_ref.putNonTimedElt(path, cpopath + 'source_ref', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'particle') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'particle', i, numpy.array(self.particle).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'momentum') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'momentum', i, numpy.array(self.momentum).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'energy', i, numpy.array(self.energy).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type sourcesstruct_arraydist_sources_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.source_ref.getNonTimedElt(path, cpopath + 'source_ref', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'particle') 
			print ('obj = ' + str(obj))
		status, ret_particle = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'particle', i)
		check_status(status)
		if not status:
			self.particle = ret_particle
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'momentum') 
			print ('obj = ' + str(obj))
		status, ret_momentum = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'momentum', i)
		check_status(status)
		if not status:
			self.momentum = ret_momentum
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'energy') 
			print ('obj = ' + str(obj))
		status, ret_energy = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'energy', i)
		check_status(status)
		if not status:
			self.energy = ret_energy


class trappedstructuredist_profile_values_1d:
	'''
	class trappedstructuredist_profile_values_1d
	Flux surface averaged profile evaluated using the trapped particle part of the distribution.

	Attributes:
	- state : class statestructuredist_state_1d
	   Fluid moments describing the state of the distribution; calculated from the distribution. Time-dependent
	- collisions_e : class collisions_estructuredist_collisional_transfer_1d
	   Collisional exchange from the background electrons to the distribution function. Time-dependent
	- collisions_i : class collisions_istruct_arraydist_collisional_transfer_1d: array of collisions_istruct_arraydist_collisional_transfer_1dObj objects
	   Collisional exchange from each background ion speices to the distribution function. Time-dependent; Vector (nions)
	- collisions_z : class collisions_zstruct_arraydist_profiles_1d_collisions_z: array of collisions_zstruct_arraydist_profiles_1d_collisions_zObj objects
	   Collisional exchange from each background impurities species to the distribution function. Time-dependent; Vector (nimpur)
	- sources : class sourcesstruct_arraydist_sources_1d: array of sourcesstruct_arraydist_sources_1dObj objects
	   Vector of flux surface averaged sources and sinks of particles, momentum and power included in the Fokker-Planck modelling. The physical meaning of each source term is specified through the identifier ./sources/type. Note that it is possible to store multiple source terms with the same value for source/type. Time-dependent; Vector(n_source_terms)
	'''

	def __init__(self, base_path_in='trapped'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.state = statestructuredist_state_1d('state')
		self.collisions_e = collisions_estructuredist_collisional_transfer_1d('collisions_e')
		self.collisions_i = collisions_istruct_arraydist_collisional_transfer_1d('collisions_i')
		self.collisions_z = collisions_zstruct_arraydist_profiles_1d_collisions_z('collisions_z')
		self.sources = sourcesstruct_arraydist_sources_1d('sources')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class trappedstructuredist_profile_values_1d\n'
		ret = ret + space + 'Attribute state\n ' + self.state.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_e\n ' + self.collisions_e.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_i\n ' + self.collisions_i.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_z\n ' + self.collisions_z.__str__(depth+1)
		ret = ret + space + 'Attribute sources\n ' + self.sources.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.state.setExpIdx(idx)
		self.collisions_e.setExpIdx(idx)
		self.collisions_i.setExpIdx(idx)
		self.collisions_z.setExpIdx(idx)
		self.sources.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trappedstructuredist_profile_values_1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.cpoTime = self.cpoTime
		self.state.putSlice(path, cpopath)
		self.collisions_e.cpoTime = self.cpoTime
		self.collisions_e.putSlice(path, cpopath)
		self.collisions_i.cpoTime = self.cpoTime
		self.collisions_i.putSlice(path, cpopath)
		self.collisions_z.cpoTime = self.cpoTime
		self.collisions_z.putSlice(path, cpopath)
		self.sources.cpoTime = self.cpoTime
		self.sources.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trappedstructuredist_profile_values_1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.replaceLastSlice(path, cpopath)
		self.collisions_e.replaceLastSlice(path, cpopath)
		self.collisions_i.replaceLastSlice(path, cpopath)
		self.collisions_z.replaceLastSlice(path, cpopath)
		self.sources.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trappedstructuredist_profile_values_1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.putNonTimed(path, cpopath)
		self.collisions_e.putNonTimed(path, cpopath)
		self.collisions_i.putNonTimed(path, cpopath)
		self.collisions_z.putNonTimed(path, cpopath)
		self.sources.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type trappedstructuredist_profile_values_1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_e.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_i.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_z.getSlice(path, cpopath, inTime, interpolMode)
		self.sources.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type trappedstructuredist_profile_values_1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			stateList = self.state.build_non_resampled_data(path, cpopath, nbslice)
			collisions_eList = self.collisions_e.build_non_resampled_data(path, cpopath, nbslice)
			collisions_iList = self.collisions_i.build_non_resampled_data(path, cpopath, nbslice)
			collisions_zList = self.collisions_z.build_non_resampled_data(path, cpopath, nbslice)
			sourcesList = self.sources.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = trappedstructuredist_profile_values_1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.state = stateList[i]
				slice.collisions_e = collisions_eList[i]
				slice.collisions_i = collisions_iList[i]
				slice.collisions_z = collisions_zList[i]
				slice.sources = sourcesList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trappedstructuredist_profile_values_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.state.putTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putTimedElt(path, cpopath + 'collisions_z', i, obj)
		obj = self.sources.putTimedElt(path, cpopath + 'sources', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trappedstructuredist_profile_values_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.state.getTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getTimedElt(path, cpopath + 'collisions_z', i, obj)
		self.sources.getTimedElt(path, cpopath + 'sources', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trappedstructuredist_profile_values_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.state.putNonTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		obj = self.sources.putNonTimedElt(path, cpopath + 'sources', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trappedstructuredist_profile_values_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.state.getNonTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		self.sources.getNonTimedElt(path, cpopath + 'sources', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.deleteData(path, cpopath)
		self.collisions_e.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'collisions_i')
		ull.deleteData(self.idx, path, cpopath + 'collisions_z')
		ull.deleteData(self.idx, path, cpopath + 'sources')


class co_passingstructuredist_profile_values_1d:
	'''
	class co_passingstructuredist_profile_values_1d
	Flux surface averaged profile evaluated using the co-current passing particle part of the distribution.

	Attributes:
	- state : class statestructuredist_state_1d
	   Fluid moments describing the state of the distribution; calculated from the distribution. Time-dependent
	- collisions_e : class collisions_estructuredist_collisional_transfer_1d
	   Collisional exchange from the background electrons to the distribution function. Time-dependent
	- collisions_i : class collisions_istruct_arraydist_collisional_transfer_1d: array of collisions_istruct_arraydist_collisional_transfer_1dObj objects
	   Collisional exchange from each background ion speices to the distribution function. Time-dependent; Vector (nions)
	- collisions_z : class collisions_zstruct_arraydist_profiles_1d_collisions_z: array of collisions_zstruct_arraydist_profiles_1d_collisions_zObj objects
	   Collisional exchange from each background impurities species to the distribution function. Time-dependent; Vector (nimpur)
	- sources : class sourcesstruct_arraydist_sources_1d: array of sourcesstruct_arraydist_sources_1dObj objects
	   Vector of flux surface averaged sources and sinks of particles, momentum and power included in the Fokker-Planck modelling. The physical meaning of each source term is specified through the identifier ./sources/type. Note that it is possible to store multiple source terms with the same value for source/type. Time-dependent; Vector(n_source_terms)
	'''

	def __init__(self, base_path_in='co_passing'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.state = statestructuredist_state_1d('state')
		self.collisions_e = collisions_estructuredist_collisional_transfer_1d('collisions_e')
		self.collisions_i = collisions_istruct_arraydist_collisional_transfer_1d('collisions_i')
		self.collisions_z = collisions_zstruct_arraydist_profiles_1d_collisions_z('collisions_z')
		self.sources = sourcesstruct_arraydist_sources_1d('sources')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class co_passingstructuredist_profile_values_1d\n'
		ret = ret + space + 'Attribute state\n ' + self.state.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_e\n ' + self.collisions_e.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_i\n ' + self.collisions_i.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_z\n ' + self.collisions_z.__str__(depth+1)
		ret = ret + space + 'Attribute sources\n ' + self.sources.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.state.setExpIdx(idx)
		self.collisions_e.setExpIdx(idx)
		self.collisions_i.setExpIdx(idx)
		self.collisions_z.setExpIdx(idx)
		self.sources.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type co_passingstructuredist_profile_values_1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.cpoTime = self.cpoTime
		self.state.putSlice(path, cpopath)
		self.collisions_e.cpoTime = self.cpoTime
		self.collisions_e.putSlice(path, cpopath)
		self.collisions_i.cpoTime = self.cpoTime
		self.collisions_i.putSlice(path, cpopath)
		self.collisions_z.cpoTime = self.cpoTime
		self.collisions_z.putSlice(path, cpopath)
		self.sources.cpoTime = self.cpoTime
		self.sources.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type co_passingstructuredist_profile_values_1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.replaceLastSlice(path, cpopath)
		self.collisions_e.replaceLastSlice(path, cpopath)
		self.collisions_i.replaceLastSlice(path, cpopath)
		self.collisions_z.replaceLastSlice(path, cpopath)
		self.sources.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type co_passingstructuredist_profile_values_1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.putNonTimed(path, cpopath)
		self.collisions_e.putNonTimed(path, cpopath)
		self.collisions_i.putNonTimed(path, cpopath)
		self.collisions_z.putNonTimed(path, cpopath)
		self.sources.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type co_passingstructuredist_profile_values_1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_e.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_i.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_z.getSlice(path, cpopath, inTime, interpolMode)
		self.sources.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type co_passingstructuredist_profile_values_1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			stateList = self.state.build_non_resampled_data(path, cpopath, nbslice)
			collisions_eList = self.collisions_e.build_non_resampled_data(path, cpopath, nbslice)
			collisions_iList = self.collisions_i.build_non_resampled_data(path, cpopath, nbslice)
			collisions_zList = self.collisions_z.build_non_resampled_data(path, cpopath, nbslice)
			sourcesList = self.sources.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = co_passingstructuredist_profile_values_1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.state = stateList[i]
				slice.collisions_e = collisions_eList[i]
				slice.collisions_i = collisions_iList[i]
				slice.collisions_z = collisions_zList[i]
				slice.sources = sourcesList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type co_passingstructuredist_profile_values_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.state.putTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putTimedElt(path, cpopath + 'collisions_z', i, obj)
		obj = self.sources.putTimedElt(path, cpopath + 'sources', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type co_passingstructuredist_profile_values_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.state.getTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getTimedElt(path, cpopath + 'collisions_z', i, obj)
		self.sources.getTimedElt(path, cpopath + 'sources', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type co_passingstructuredist_profile_values_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.state.putNonTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		obj = self.sources.putNonTimedElt(path, cpopath + 'sources', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type co_passingstructuredist_profile_values_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.state.getNonTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		self.sources.getNonTimedElt(path, cpopath + 'sources', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.deleteData(path, cpopath)
		self.collisions_e.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'collisions_i')
		ull.deleteData(self.idx, path, cpopath + 'collisions_z')
		ull.deleteData(self.idx, path, cpopath + 'sources')


class cntr_passingstructuredist_profile_values_1d:
	'''
	class cntr_passingstructuredist_profile_values_1d
	Flux surface averaged profile evaluated using the counter-current passing particle part of the distribution.

	Attributes:
	- state : class statestructuredist_state_1d
	   Fluid moments describing the state of the distribution; calculated from the distribution. Time-dependent
	- collisions_e : class collisions_estructuredist_collisional_transfer_1d
	   Collisional exchange from the background electrons to the distribution function. Time-dependent
	- collisions_i : class collisions_istruct_arraydist_collisional_transfer_1d: array of collisions_istruct_arraydist_collisional_transfer_1dObj objects
	   Collisional exchange from each background ion speices to the distribution function. Time-dependent; Vector (nions)
	- collisions_z : class collisions_zstruct_arraydist_profiles_1d_collisions_z: array of collisions_zstruct_arraydist_profiles_1d_collisions_zObj objects
	   Collisional exchange from each background impurities species to the distribution function. Time-dependent; Vector (nimpur)
	- sources : class sourcesstruct_arraydist_sources_1d: array of sourcesstruct_arraydist_sources_1dObj objects
	   Vector of flux surface averaged sources and sinks of particles, momentum and power included in the Fokker-Planck modelling. The physical meaning of each source term is specified through the identifier ./sources/type. Note that it is possible to store multiple source terms with the same value for source/type. Time-dependent; Vector(n_source_terms)
	'''

	def __init__(self, base_path_in='cntr_passing'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.state = statestructuredist_state_1d('state')
		self.collisions_e = collisions_estructuredist_collisional_transfer_1d('collisions_e')
		self.collisions_i = collisions_istruct_arraydist_collisional_transfer_1d('collisions_i')
		self.collisions_z = collisions_zstruct_arraydist_profiles_1d_collisions_z('collisions_z')
		self.sources = sourcesstruct_arraydist_sources_1d('sources')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class cntr_passingstructuredist_profile_values_1d\n'
		ret = ret + space + 'Attribute state\n ' + self.state.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_e\n ' + self.collisions_e.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_i\n ' + self.collisions_i.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_z\n ' + self.collisions_z.__str__(depth+1)
		ret = ret + space + 'Attribute sources\n ' + self.sources.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.state.setExpIdx(idx)
		self.collisions_e.setExpIdx(idx)
		self.collisions_i.setExpIdx(idx)
		self.collisions_z.setExpIdx(idx)
		self.sources.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type cntr_passingstructuredist_profile_values_1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.cpoTime = self.cpoTime
		self.state.putSlice(path, cpopath)
		self.collisions_e.cpoTime = self.cpoTime
		self.collisions_e.putSlice(path, cpopath)
		self.collisions_i.cpoTime = self.cpoTime
		self.collisions_i.putSlice(path, cpopath)
		self.collisions_z.cpoTime = self.cpoTime
		self.collisions_z.putSlice(path, cpopath)
		self.sources.cpoTime = self.cpoTime
		self.sources.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type cntr_passingstructuredist_profile_values_1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.replaceLastSlice(path, cpopath)
		self.collisions_e.replaceLastSlice(path, cpopath)
		self.collisions_i.replaceLastSlice(path, cpopath)
		self.collisions_z.replaceLastSlice(path, cpopath)
		self.sources.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type cntr_passingstructuredist_profile_values_1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.putNonTimed(path, cpopath)
		self.collisions_e.putNonTimed(path, cpopath)
		self.collisions_i.putNonTimed(path, cpopath)
		self.collisions_z.putNonTimed(path, cpopath)
		self.sources.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type cntr_passingstructuredist_profile_values_1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_e.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_i.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_z.getSlice(path, cpopath, inTime, interpolMode)
		self.sources.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type cntr_passingstructuredist_profile_values_1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			stateList = self.state.build_non_resampled_data(path, cpopath, nbslice)
			collisions_eList = self.collisions_e.build_non_resampled_data(path, cpopath, nbslice)
			collisions_iList = self.collisions_i.build_non_resampled_data(path, cpopath, nbslice)
			collisions_zList = self.collisions_z.build_non_resampled_data(path, cpopath, nbslice)
			sourcesList = self.sources.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = cntr_passingstructuredist_profile_values_1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.state = stateList[i]
				slice.collisions_e = collisions_eList[i]
				slice.collisions_i = collisions_iList[i]
				slice.collisions_z = collisions_zList[i]
				slice.sources = sourcesList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type cntr_passingstructuredist_profile_values_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.state.putTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putTimedElt(path, cpopath + 'collisions_z', i, obj)
		obj = self.sources.putTimedElt(path, cpopath + 'sources', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type cntr_passingstructuredist_profile_values_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.state.getTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getTimedElt(path, cpopath + 'collisions_z', i, obj)
		self.sources.getTimedElt(path, cpopath + 'sources', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type cntr_passingstructuredist_profile_values_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.state.putNonTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		obj = self.sources.putNonTimedElt(path, cpopath + 'sources', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type cntr_passingstructuredist_profile_values_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.state.getNonTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		self.sources.getNonTimedElt(path, cpopath + 'sources', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.deleteData(path, cpopath)
		self.collisions_e.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'collisions_i')
		ull.deleteData(self.idx, path, cpopath + 'collisions_z')
		ull.deleteData(self.idx, path, cpopath + 'sources')


class profiles_2dstructuredist_profiles_2d:
	'''
	class profiles_2dstructuredist_profiles_2d
	2D profiles in the poloidal plane

	Attributes:
	- geometry : class geometrystructuredist_geometry_2d
	   Grids and metric information; including R, Z, rho_tor, psi, theta_geom and theta_strt. The grid has to be rectangular in a pair of these coordinates; this is specified in coord_type. Time-dependent
	- state : class statestructuredist_state_2d
	   Fluid moments describing the state of the distribution; calculated from the distribution. Time-dependent
	- collisions_e : class collisions_estructuredist_collisional_transfer_2d
	   Collisional exchange from the background electrons to the distribution function. Time-dependent
	- collisions_i : class collisions_istruct_arraydist_collisional_transfer_2d: array of collisions_istruct_arraydist_collisional_transfer_2dObj objects
	   Collisional exchange from each background ion speices to the distribution function. Time-dependent; Vector (nions)
	- collisions_z : class collisions_zstruct_arraydist_profiles2d_collisions_z: array of collisions_zstruct_arraydist_profiles2d_collisions_zObj objects
	   Collisional exchange from each background impurities species to the distribution function. Time-dependent; Vector (nimpur)
	- trapped : class trappedstructuredist_profile_values_2d
	   2D profiles evaluated using the trapped particle part of the distribution.
	- co_passing : class co_passingstructuredist_profile_values_2d
	   2D profiles evaluated using the co-current passing particle part of the distribution.
	- cntr_passing : class cntr_passingstructuredist_profile_values_2d
	   2D profiles evaluated using the counter-current passing particle part of the distribution.
	'''

	def __init__(self, base_path_in='profiles_2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.geometry = geometrystructuredist_geometry_2d('geometry')
		self.state = statestructuredist_state_2d('state')
		self.collisions_e = collisions_estructuredist_collisional_transfer_2d('collisions_e')
		self.collisions_i = collisions_istruct_arraydist_collisional_transfer_2d('collisions_i')
		self.collisions_z = collisions_zstruct_arraydist_profiles2d_collisions_z('collisions_z')
		self.trapped = trappedstructuredist_profile_values_2d('trapped')
		self.co_passing = co_passingstructuredist_profile_values_2d('co_passing')
		self.cntr_passing = cntr_passingstructuredist_profile_values_2d('cntr_passing')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class profiles_2dstructuredist_profiles_2d\n'
		ret = ret + space + 'Attribute geometry\n ' + self.geometry.__str__(depth+1)
		ret = ret + space + 'Attribute state\n ' + self.state.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_e\n ' + self.collisions_e.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_i\n ' + self.collisions_i.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_z\n ' + self.collisions_z.__str__(depth+1)
		ret = ret + space + 'Attribute trapped\n ' + self.trapped.__str__(depth+1)
		ret = ret + space + 'Attribute co_passing\n ' + self.co_passing.__str__(depth+1)
		ret = ret + space + 'Attribute cntr_passing\n ' + self.cntr_passing.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.geometry.setExpIdx(idx)
		self.state.setExpIdx(idx)
		self.collisions_e.setExpIdx(idx)
		self.collisions_i.setExpIdx(idx)
		self.collisions_z.setExpIdx(idx)
		self.trapped.setExpIdx(idx)
		self.co_passing.setExpIdx(idx)
		self.cntr_passing.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructuredist_profiles_2d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.cpoTime = self.cpoTime
		self.geometry.putSlice(path, cpopath)
		self.state.cpoTime = self.cpoTime
		self.state.putSlice(path, cpopath)
		self.collisions_e.cpoTime = self.cpoTime
		self.collisions_e.putSlice(path, cpopath)
		self.collisions_i.cpoTime = self.cpoTime
		self.collisions_i.putSlice(path, cpopath)
		self.collisions_z.cpoTime = self.cpoTime
		self.collisions_z.putSlice(path, cpopath)
		self.trapped.cpoTime = self.cpoTime
		self.trapped.putSlice(path, cpopath)
		self.co_passing.cpoTime = self.cpoTime
		self.co_passing.putSlice(path, cpopath)
		self.cntr_passing.cpoTime = self.cpoTime
		self.cntr_passing.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructuredist_profiles_2d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.replaceLastSlice(path, cpopath)
		self.state.replaceLastSlice(path, cpopath)
		self.collisions_e.replaceLastSlice(path, cpopath)
		self.collisions_i.replaceLastSlice(path, cpopath)
		self.collisions_z.replaceLastSlice(path, cpopath)
		self.trapped.replaceLastSlice(path, cpopath)
		self.co_passing.replaceLastSlice(path, cpopath)
		self.cntr_passing.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructuredist_profiles_2d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.putNonTimed(path, cpopath)
		self.state.putNonTimed(path, cpopath)
		self.collisions_e.putNonTimed(path, cpopath)
		self.collisions_i.putNonTimed(path, cpopath)
		self.collisions_z.putNonTimed(path, cpopath)
		self.trapped.putNonTimed(path, cpopath)
		self.co_passing.putNonTimed(path, cpopath)
		self.cntr_passing.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructuredist_profiles_2d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.getSlice(path, cpopath, inTime, interpolMode)
		self.state.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_e.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_i.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_z.getSlice(path, cpopath, inTime, interpolMode)
		self.trapped.getSlice(path, cpopath, inTime, interpolMode)
		self.co_passing.getSlice(path, cpopath, inTime, interpolMode)
		self.cntr_passing.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstructuredist_profiles_2d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			geometryList = self.geometry.build_non_resampled_data(path, cpopath, nbslice)
			stateList = self.state.build_non_resampled_data(path, cpopath, nbslice)
			collisions_eList = self.collisions_e.build_non_resampled_data(path, cpopath, nbslice)
			collisions_iList = self.collisions_i.build_non_resampled_data(path, cpopath, nbslice)
			collisions_zList = self.collisions_z.build_non_resampled_data(path, cpopath, nbslice)
			trappedList = self.trapped.build_non_resampled_data(path, cpopath, nbslice)
			co_passingList = self.co_passing.build_non_resampled_data(path, cpopath, nbslice)
			cntr_passingList = self.cntr_passing.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = profiles_2dstructuredist_profiles_2d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.geometry = geometryList[i]
				slice.state = stateList[i]
				slice.collisions_e = collisions_eList[i]
				slice.collisions_i = collisions_iList[i]
				slice.collisions_z = collisions_zList[i]
				slice.trapped = trappedList[i]
				slice.co_passing = co_passingList[i]
				slice.cntr_passing = cntr_passingList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstructuredist_profiles_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.geometry.putTimedElt(path, cpopath + 'geometry', i, obj)
		obj = self.state.putTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putTimedElt(path, cpopath + 'collisions_z', i, obj)
		obj = self.trapped.putTimedElt(path, cpopath + 'trapped', i, obj)
		obj = self.co_passing.putTimedElt(path, cpopath + 'co_passing', i, obj)
		obj = self.cntr_passing.putTimedElt(path, cpopath + 'cntr_passing', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstructuredist_profiles_2dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.geometry.getTimedElt(path, cpopath + 'geometry', i, obj)
		self.state.getTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getTimedElt(path, cpopath + 'collisions_z', i, obj)
		self.trapped.getTimedElt(path, cpopath + 'trapped', i, obj)
		self.co_passing.getTimedElt(path, cpopath + 'co_passing', i, obj)
		self.cntr_passing.getTimedElt(path, cpopath + 'cntr_passing', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstructuredist_profiles_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.geometry.putNonTimedElt(path, cpopath + 'geometry', i, obj)
		obj = self.state.putNonTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		obj = self.trapped.putNonTimedElt(path, cpopath + 'trapped', i, obj)
		obj = self.co_passing.putNonTimedElt(path, cpopath + 'co_passing', i, obj)
		obj = self.cntr_passing.putNonTimedElt(path, cpopath + 'cntr_passing', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstructuredist_profiles_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.geometry.getNonTimedElt(path, cpopath + 'geometry', i, obj)
		self.state.getNonTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		self.trapped.getNonTimedElt(path, cpopath + 'trapped', i, obj)
		self.co_passing.getNonTimedElt(path, cpopath + 'co_passing', i, obj)
		self.cntr_passing.getNonTimedElt(path, cpopath + 'cntr_passing', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.geometry.deleteData(path, cpopath)
		self.state.deleteData(path, cpopath)
		self.collisions_e.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'collisions_i')
		ull.deleteData(self.idx, path, cpopath + 'collisions_z')
		self.trapped.deleteData(path, cpopath)
		self.co_passing.deleteData(path, cpopath)
		self.cntr_passing.deleteData(path, cpopath)


class geometrystructuredist_geometry_2d:
	'''
	class geometrystructuredist_geometry_2d
	Grids and metric information; including R, Z, rho_tor, psi, theta_geom and theta_strt. The grid has to be rectangular in a pair of these coordinates; this is specified in coord_type. Time-dependent

	Attributes:
	- coord_type : int
	   0: Rectangular grid in the (R,Z) coordinates; 1: Rectangular grid in the (rho_tor,theta_geom) coordinates; 2: Rectangular grid in the (rho_tor,theta_straight) coordinates. 
	- r : numpy.ndarray 2D with float
	   Major radius coordinate [m]; Time-dependent; Matrix (n_coord1,n_coord2)
	- z : numpy.ndarray 2D with float
	   Vertical coordinate [m]; Time-dependent; Matrix (n_coord1,n_coord2)
	- rho_tor : numpy.ndarray 2D with float
	   Toroidal flux coordinate [m]. Defined as sqrt((phi-phi_axis)/pi/B0), where B0=../global_param/toroid_field/b0, phi is the toroidal flux and phi_axis is the toroidal flux at the magnetic axis. Time-dependent; Matrix (n_coord1,n_coord2)
	- psi : numpy.ndarray 2D with float
	   Poloidal flux at the grid points for 1D profiles [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent; Matrix (n_coord1,n_coord2)
	- theta_geom : numpy.ndarray 2D with float
	   Geometrical poloidal angle [rad]; Time-dependent; Matrix (n_coord1,n_coord2)
	- theta_strt : numpy.ndarray 2D with float
	   Straight field line poloidal angle [rad]; Time-dependent; Matrix (n_coord1,n_coord2)
	'''

	def __init__(self, base_path_in='geometry'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.coord_type = EMPTY_INT
		self.r = numpy.zeros((0,0), numpy.float64, order='C')
		self.z = numpy.zeros((0,0), numpy.float64, order='C')
		self.rho_tor = numpy.zeros((0,0), numpy.float64, order='C')
		self.psi = numpy.zeros((0,0), numpy.float64, order='C')
		self.theta_geom = numpy.zeros((0,0), numpy.float64, order='C')
		self.theta_strt = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class geometrystructuredist_geometry_2d\n'
		ret = ret + space + 'Attribute coord_type: ' + str(self.coord_type) + '\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_tor.__str__()
		ret = ret + space + 'Attribute rho_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta_geom.__str__()
		ret = ret + space + 'Attribute theta_geom\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta_strt.__str__()
		ret = ret + space + 'Attribute theta_strt\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_2d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'theta_geom', numpy.array(self.theta_geom).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'theta_strt', numpy.array(self.theta_strt).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_2d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'r', numpy.array(self.r).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'z', numpy.array(self.z).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'theta_geom', numpy.array(self.theta_geom).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'theta_strt', numpy.array(self.theta_strt).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_2d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'coord_type', self.coord_type)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_2d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_coord_type = ull.getInt(self.idx, path, cpopath + 'coord_type')
		check_status(status)
		if not status:
			self.coord_type = ret_coord_type
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
		status, ret_theta_geom, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'theta_geom', inTime, interpolMode)
		check_status(status)
		if not status:
			self.theta_geom = ret_theta_geom
			self.cpoTime = retTime
		status, ret_theta_strt, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'theta_strt', inTime, interpolMode)
		check_status(status)
		if not status:
			self.theta_strt = ret_theta_strt
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type geometrystructuredist_geometry_2d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, coord_typeVal = ull.getInt(self.idx, path, cpopath + 'coord_type')
			check_status(status)
			status, rList = ull.getVect3DDouble(self.idx, path, cpopath + 'r')
			if len(rList) == 0:
				rList = numpy.resize(rList, (0,0,nbslice))
			check_status(status)
			status, zList = ull.getVect3DDouble(self.idx, path, cpopath + 'z')
			if len(zList) == 0:
				zList = numpy.resize(zList, (0,0,nbslice))
			check_status(status)
			status, rho_torList = ull.getVect3DDouble(self.idx, path, cpopath + 'rho_tor')
			if len(rho_torList) == 0:
				rho_torList = numpy.resize(rho_torList, (0,0,nbslice))
			check_status(status)
			status, psiList = ull.getVect3DDouble(self.idx, path, cpopath + 'psi')
			if len(psiList) == 0:
				psiList = numpy.resize(psiList, (0,0,nbslice))
			check_status(status)
			status, theta_geomList = ull.getVect3DDouble(self.idx, path, cpopath + 'theta_geom')
			if len(theta_geomList) == 0:
				theta_geomList = numpy.resize(theta_geomList, (0,0,nbslice))
			check_status(status)
			status, theta_strtList = ull.getVect3DDouble(self.idx, path, cpopath + 'theta_strt')
			if len(theta_strtList) == 0:
				theta_strtList = numpy.resize(theta_strtList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = geometrystructuredist_geometry_2d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.coord_type = coord_typeVal
				slice.r = rList[:,:,i]
				slice.z = zList[:,:,i]
				slice.rho_tor = rho_torList[:,:,i]
				slice.psi = psiList[:,:,i]
				slice.theta_geom = theta_geomList[:,:,i]
				slice.theta_strt = theta_strtList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geometrystructuredist_geometry_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'rho_tor', i, numpy.array(self.rho_tor).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'psi', i, numpy.array(self.psi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'theta_geom') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'theta_geom', i, numpy.array(self.theta_geom).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'theta_strt') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'theta_strt', i, numpy.array(self.theta_strt).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geometrystructuredist_geometry_2dObj, run function getTimedElt') 
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
			print ('getVect2DDoubleInObject : ' + cpopath + 'theta_geom') 
			print ('obj = ' + str(obj))
		status, ret_theta_geom = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'theta_geom', i)
		check_status(status)
		if not status:
			self.theta_geom = ret_theta_geom
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'theta_strt') 
			print ('obj = ' + str(obj))
		status, ret_theta_strt = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'theta_strt', i)
		check_status(status)
		if not status:
			self.theta_strt = ret_theta_strt

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geometrystructuredist_geometry_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'coord_type') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'coord_type', i, self.coord_type)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geometrystructuredist_geometry_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'coord_type') 
			print ('obj = ' + str(obj))
		status, ret_coord_type = ull.getIntFromObject(self.idx, obj, cpopath + 'coord_type', i)
		check_status(status)
		if not status:
			self.coord_type = ret_coord_type

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'coord_type')
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'z')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor')
		ull.deleteData(self.idx, path, cpopath + 'psi')
		ull.deleteData(self.idx, path, cpopath + 'theta_geom')
		ull.deleteData(self.idx, path, cpopath + 'theta_strt')


class statestructuredist_state_2d:
	'''
	class statestructuredist_state_2d
	Fluid moments describing the state of the distribution; calculated from the distribution. Time-dependent

	Attributes:
	- dens : numpy.ndarray 2D with float
	   Particle density (including both thermal and fast particles) [1/m^3]; Time-dependent; Matrix (n_coord1, n_coord2)
	- dens_fast : numpy.ndarray 2D with float
	   Fast particle density [1/m^3]; Time-dependent; Matrix (n_coord1, n_coord2)
	- pres : numpy.ndarray 2D with float
	   Scalar pressure (including both thermal and fast particles) [J/m^3]. Related to the energy content, W, according to: pres=2*W/3. Time-dependent; Matrix (n_coord1, n_coord2)
	- pres_fast : numpy.ndarray 2D with float
	   Scalar pressure of the fast particles [J/m^3]. Related to the fast particle energy content, Wf, according to: pres_fast=2*Wf/3. Time-dependent; Matrix (n_coord1, n_coord2)
	- pres_fast_pa : numpy.ndarray 2D with float
	   Parallel pressure of the fast particles [J/m^3]. Related to the fast particle parallel energy content, Wfpar, according to: pres_fast_pa=2*Wfpar. Time-dependent; Matrix (n_coord1, n_coord2)
	- momentm_fast : numpy.ndarray 2D with float
	   Kinetic toroidal angular momentum density of the fast ions [Ns/m^2]; Time-dependent; Matrix (n_coord1, n_coord2)
	- current : numpy.ndarray 2D with float
	   Total toroidal driven current density (including electron and thermal ion back-current, or drag-current) [A/m^3]; Time-dependent; Matrix (n_coord1, n_coord2)
	- current_fast : numpy.ndarray 2D with float
	   Toroidal current density of fast (non-thermal) particles of the distribution species (excluding electron and thermal ion back-current, or drag-current) [A.m^-2]; Time-dependent; Matrix (n_coord1, n_coord2).
	- torque_jrxb : numpy.ndarray 2D with float
	   Toroidal torque density due to radial currents, excluding radial current due to neoclassical effect [N/m^2]; Time-dependent; Matrix (n_coord1, n_coord2)
	'''

	def __init__(self, base_path_in='state'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dens = numpy.zeros((0,0), numpy.float64, order='C')
		self.dens_fast = numpy.zeros((0,0), numpy.float64, order='C')
		self.pres = numpy.zeros((0,0), numpy.float64, order='C')
		self.pres_fast = numpy.zeros((0,0), numpy.float64, order='C')
		self.pres_fast_pa = numpy.zeros((0,0), numpy.float64, order='C')
		self.momentm_fast = numpy.zeros((0,0), numpy.float64, order='C')
		self.current = numpy.zeros((0,0), numpy.float64, order='C')
		self.current_fast = numpy.zeros((0,0), numpy.float64, order='C')
		self.torque_jrxb = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class statestructuredist_state_2d\n'
		s = self.dens.__str__()
		ret = ret + space + 'Attribute dens\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dens_fast.__str__()
		ret = ret + space + 'Attribute dens_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pres.__str__()
		ret = ret + space + 'Attribute pres\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pres_fast.__str__()
		ret = ret + space + 'Attribute pres_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pres_fast_pa.__str__()
		ret = ret + space + 'Attribute pres_fast_pa\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.momentm_fast.__str__()
		ret = ret + space + 'Attribute momentm_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.current.__str__()
		ret = ret + space + 'Attribute current\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.current_fast.__str__()
		ret = ret + space + 'Attribute current_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_jrxb.__str__()
		ret = ret + space + 'Attribute torque_jrxb\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_2d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'dens', numpy.array(self.dens).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'dens_fast', numpy.array(self.dens_fast).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'pres', numpy.array(self.pres).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'pres_fast', numpy.array(self.pres_fast).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'pres_fast_pa', numpy.array(self.pres_fast_pa).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'momentm_fast', numpy.array(self.momentm_fast).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'current', numpy.array(self.current).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'current_fast', numpy.array(self.current_fast).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'torque_jrxb', numpy.array(self.torque_jrxb).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_2d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'dens', numpy.array(self.dens).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'dens_fast', numpy.array(self.dens_fast).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'pres', numpy.array(self.pres).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'pres_fast', numpy.array(self.pres_fast).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'pres_fast_pa', numpy.array(self.pres_fast_pa).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'momentm_fast', numpy.array(self.momentm_fast).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'current', numpy.array(self.current).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'current_fast', numpy.array(self.current_fast).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'torque_jrxb', numpy.array(self.torque_jrxb).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_2d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_2d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dens, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'dens', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dens = ret_dens
			self.cpoTime = retTime
		status, ret_dens_fast, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'dens_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dens_fast = ret_dens_fast
			self.cpoTime = retTime
		status, ret_pres, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'pres', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pres = ret_pres
			self.cpoTime = retTime
		status, ret_pres_fast, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'pres_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pres_fast = ret_pres_fast
			self.cpoTime = retTime
		status, ret_pres_fast_pa, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'pres_fast_pa', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pres_fast_pa = ret_pres_fast_pa
			self.cpoTime = retTime
		status, ret_momentm_fast, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'momentm_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.momentm_fast = ret_momentm_fast
			self.cpoTime = retTime
		status, ret_current, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'current', inTime, interpolMode)
		check_status(status)
		if not status:
			self.current = ret_current
			self.cpoTime = retTime
		status, ret_current_fast, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'current_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.current_fast = ret_current_fast
			self.cpoTime = retTime
		status, ret_torque_jrxb, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'torque_jrxb', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_jrxb = ret_torque_jrxb
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type statestructuredist_state_2d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, densList = ull.getVect3DDouble(self.idx, path, cpopath + 'dens')
			if len(densList) == 0:
				densList = numpy.resize(densList, (0,0,nbslice))
			check_status(status)
			status, dens_fastList = ull.getVect3DDouble(self.idx, path, cpopath + 'dens_fast')
			if len(dens_fastList) == 0:
				dens_fastList = numpy.resize(dens_fastList, (0,0,nbslice))
			check_status(status)
			status, presList = ull.getVect3DDouble(self.idx, path, cpopath + 'pres')
			if len(presList) == 0:
				presList = numpy.resize(presList, (0,0,nbslice))
			check_status(status)
			status, pres_fastList = ull.getVect3DDouble(self.idx, path, cpopath + 'pres_fast')
			if len(pres_fastList) == 0:
				pres_fastList = numpy.resize(pres_fastList, (0,0,nbslice))
			check_status(status)
			status, pres_fast_paList = ull.getVect3DDouble(self.idx, path, cpopath + 'pres_fast_pa')
			if len(pres_fast_paList) == 0:
				pres_fast_paList = numpy.resize(pres_fast_paList, (0,0,nbslice))
			check_status(status)
			status, momentm_fastList = ull.getVect3DDouble(self.idx, path, cpopath + 'momentm_fast')
			if len(momentm_fastList) == 0:
				momentm_fastList = numpy.resize(momentm_fastList, (0,0,nbslice))
			check_status(status)
			status, currentList = ull.getVect3DDouble(self.idx, path, cpopath + 'current')
			if len(currentList) == 0:
				currentList = numpy.resize(currentList, (0,0,nbslice))
			check_status(status)
			status, current_fastList = ull.getVect3DDouble(self.idx, path, cpopath + 'current_fast')
			if len(current_fastList) == 0:
				current_fastList = numpy.resize(current_fastList, (0,0,nbslice))
			check_status(status)
			status, torque_jrxbList = ull.getVect3DDouble(self.idx, path, cpopath + 'torque_jrxb')
			if len(torque_jrxbList) == 0:
				torque_jrxbList = numpy.resize(torque_jrxbList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = statestructuredist_state_2d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dens = densList[:,:,i]
				slice.dens_fast = dens_fastList[:,:,i]
				slice.pres = presList[:,:,i]
				slice.pres_fast = pres_fastList[:,:,i]
				slice.pres_fast_pa = pres_fast_paList[:,:,i]
				slice.momentm_fast = momentm_fastList[:,:,i]
				slice.current = currentList[:,:,i]
				slice.current_fast = current_fastList[:,:,i]
				slice.torque_jrxb = torque_jrxbList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type statestructuredist_state_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'dens') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'dens', i, numpy.array(self.dens).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'dens_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'dens_fast', i, numpy.array(self.dens_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pres') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pres', i, numpy.array(self.pres).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pres_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pres_fast', i, numpy.array(self.pres_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pres_fast_pa') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pres_fast_pa', i, numpy.array(self.pres_fast_pa).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'momentm_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'momentm_fast', i, numpy.array(self.momentm_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'current') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'current', i, numpy.array(self.current).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'current_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'current_fast', i, numpy.array(self.current_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'torque_jrxb') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'torque_jrxb', i, numpy.array(self.torque_jrxb).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type statestructuredist_state_2dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'dens') 
			print ('obj = ' + str(obj))
		status, ret_dens = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'dens', i)
		check_status(status)
		if not status:
			self.dens = ret_dens
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'dens_fast') 
			print ('obj = ' + str(obj))
		status, ret_dens_fast = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'dens_fast', i)
		check_status(status)
		if not status:
			self.dens_fast = ret_dens_fast
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pres') 
			print ('obj = ' + str(obj))
		status, ret_pres = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pres', i)
		check_status(status)
		if not status:
			self.pres = ret_pres
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pres_fast') 
			print ('obj = ' + str(obj))
		status, ret_pres_fast = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pres_fast', i)
		check_status(status)
		if not status:
			self.pres_fast = ret_pres_fast
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pres_fast_pa') 
			print ('obj = ' + str(obj))
		status, ret_pres_fast_pa = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pres_fast_pa', i)
		check_status(status)
		if not status:
			self.pres_fast_pa = ret_pres_fast_pa
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'momentm_fast') 
			print ('obj = ' + str(obj))
		status, ret_momentm_fast = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'momentm_fast', i)
		check_status(status)
		if not status:
			self.momentm_fast = ret_momentm_fast
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'current') 
			print ('obj = ' + str(obj))
		status, ret_current = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'current', i)
		check_status(status)
		if not status:
			self.current = ret_current
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'current_fast') 
			print ('obj = ' + str(obj))
		status, ret_current_fast = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'current_fast', i)
		check_status(status)
		if not status:
			self.current_fast = ret_current_fast
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'torque_jrxb') 
			print ('obj = ' + str(obj))
		status, ret_torque_jrxb = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'torque_jrxb', i)
		check_status(status)
		if not status:
			self.torque_jrxb = ret_torque_jrxb

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type statestructuredist_state_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type statestructuredist_state_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'dens')
		ull.deleteData(self.idx, path, cpopath + 'dens_fast')
		ull.deleteData(self.idx, path, cpopath + 'pres')
		ull.deleteData(self.idx, path, cpopath + 'pres_fast')
		ull.deleteData(self.idx, path, cpopath + 'pres_fast_pa')
		ull.deleteData(self.idx, path, cpopath + 'momentm_fast')
		ull.deleteData(self.idx, path, cpopath + 'current')
		ull.deleteData(self.idx, path, cpopath + 'current_fast')
		ull.deleteData(self.idx, path, cpopath + 'torque_jrxb')


class collisions_estructuredist_collisional_transfer_2d:
	'''
	class collisions_estructuredist_collisional_transfer_2d
	Collisional exchange from the background electrons to the distribution function. Time-dependent

	Attributes:
	- power_th : numpy.ndarray 2D with float
	   Collisional power density to the thermal particle population [W.m^-3]; Time-dependent; Matrix(n_coord1,n_coord2)
	- power_fast : numpy.ndarray 2D with float
	   Collisional power density to the fast particle population [W.m^-3]; Time-dependent; Matrix(n_coord1,n_coord2)
	- torque_th : numpy.ndarray 2D with float
	   Collisional toroidal torque density to the thermal particle population [N.m^-2]; Time-dependent; Matrix(n_coord1,n_coord2)
	- torque_fast : numpy.ndarray 2D with float
	   Collisional toroidal torque density to the fast particle population [N.m^-2]; Time-dependent; Matrix(n_coord1,n_coord2)
	'''

	def __init__(self, base_path_in='collisions_e'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.power_th = numpy.zeros((0,0), numpy.float64, order='C')
		self.power_fast = numpy.zeros((0,0), numpy.float64, order='C')
		self.torque_th = numpy.zeros((0,0), numpy.float64, order='C')
		self.torque_fast = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class collisions_estructuredist_collisional_transfer_2d\n'
		s = self.power_th.__str__()
		ret = ret + space + 'Attribute power_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.power_fast.__str__()
		ret = ret + space + 'Attribute power_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_th.__str__()
		ret = ret + space + 'Attribute torque_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_fast.__str__()
		ret = ret + space + 'Attribute torque_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_2d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'power_th', numpy.array(self.power_th).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'power_fast', numpy.array(self.power_fast).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'torque_th', numpy.array(self.torque_th).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'torque_fast', numpy.array(self.torque_fast).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_2d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'power_th', numpy.array(self.power_th).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'power_fast', numpy.array(self.power_fast).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'torque_th', numpy.array(self.torque_th).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'torque_fast', numpy.array(self.torque_fast).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_2d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_2d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_power_th, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'power_th', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power_th = ret_power_th
			self.cpoTime = retTime
		status, ret_power_fast, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'power_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.power_fast = ret_power_fast
			self.cpoTime = retTime
		status, ret_torque_th, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'torque_th', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_th = ret_torque_th
			self.cpoTime = retTime
		status, ret_torque_fast, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'torque_fast', inTime, interpolMode)
		check_status(status)
		if not status:
			self.torque_fast = ret_torque_fast
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_estructuredist_collisional_transfer_2d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, power_thList = ull.getVect3DDouble(self.idx, path, cpopath + 'power_th')
			if len(power_thList) == 0:
				power_thList = numpy.resize(power_thList, (0,0,nbslice))
			check_status(status)
			status, power_fastList = ull.getVect3DDouble(self.idx, path, cpopath + 'power_fast')
			if len(power_fastList) == 0:
				power_fastList = numpy.resize(power_fastList, (0,0,nbslice))
			check_status(status)
			status, torque_thList = ull.getVect3DDouble(self.idx, path, cpopath + 'torque_th')
			if len(torque_thList) == 0:
				torque_thList = numpy.resize(torque_thList, (0,0,nbslice))
			check_status(status)
			status, torque_fastList = ull.getVect3DDouble(self.idx, path, cpopath + 'torque_fast')
			if len(torque_fastList) == 0:
				torque_fastList = numpy.resize(torque_fastList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = collisions_estructuredist_collisional_transfer_2d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.power_th = power_thList[:,:,i]
				slice.power_fast = power_fastList[:,:,i]
				slice.torque_th = torque_thList[:,:,i]
				slice.torque_fast = torque_fastList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_estructuredist_collisional_transfer_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'power_th', i, numpy.array(self.power_th).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'power_fast', i, numpy.array(self.power_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'torque_th', i, numpy.array(self.torque_th).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'torque_fast', i, numpy.array(self.torque_fast).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_estructuredist_collisional_transfer_2dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		status, ret_power_th = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'power_th', i)
		check_status(status)
		if not status:
			self.power_th = ret_power_th
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		status, ret_power_fast = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'power_fast', i)
		check_status(status)
		if not status:
			self.power_fast = ret_power_fast
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		status, ret_torque_th = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'torque_th', i)
		check_status(status)
		if not status:
			self.torque_th = ret_torque_th
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		status, ret_torque_fast = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'torque_fast', i)
		check_status(status)
		if not status:
			self.torque_fast = ret_torque_fast

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_estructuredist_collisional_transfer_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_estructuredist_collisional_transfer_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'power_th')
		ull.deleteData(self.idx, path, cpopath + 'power_fast')
		ull.deleteData(self.idx, path, cpopath + 'torque_th')
		ull.deleteData(self.idx, path, cpopath + 'torque_fast')


class collisions_istruct_arraydist_collisional_transfer_2d:
	'''
	class collisions_istruct_arraydist_collisional_transfer_2d
	Collisional exchange from each background ion speices to the distribution function. Time-dependent; Vector (nions)

	Attributes:
	- array : list of collisions_istruct_arraydist_collisional_transfer_2dObj 
	'''

	def __init__(self, base_path_in='collisions_i'):
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
		ret = space + 'class collisions_istruct_arraydist_collisional_transfer_2d\n'
		for i in range(len(self.array)):
			ret = ret + space + 'collisions_istruct_arraydist_collisional_transfer_2d[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(collisions_istruct_arraydist_collisional_transfer_2dObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_2d, run function putSlice') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_2d, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_2d, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_2d, run function getSlice') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_2d, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(collisions_istruct_arraydist_collisional_transfer_2d(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(collisions_istruct_arraydist_collisional_transfer_2d(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = collisions_istruct_arraydist_collisional_transfer_2d(self.base_path)
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_2d, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_2d, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_2d, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_istruct_arraydist_collisional_transfer_2d, run function getNonTimedElt') 
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


class collisions_istruct_arraydist_collisional_transfer_2dObj:
	'''
	class collisions_istruct_arraydist_collisional_transfer_2dObj
	Collisional exchange from each background ion speices to the distribution function. Time-dependent; Vector (nions)

	Attributes:
	- power_th : numpy.ndarray 2D with float
	   Collisional power density to the thermal particle population [W.m^-3]; Time-dependent; Matrix(n_coord1,n_coord2)
	- power_fast : numpy.ndarray 2D with float
	   Collisional power density to the fast particle population [W.m^-3]; Time-dependent; Matrix(n_coord1,n_coord2)
	- torque_th : numpy.ndarray 2D with float
	   Collisional toroidal torque density to the thermal particle population [N.m^-2]; Time-dependent; Matrix(n_coord1,n_coord2)
	- torque_fast : numpy.ndarray 2D with float
	   Collisional toroidal torque density to the fast particle population [N.m^-2]; Time-dependent; Matrix(n_coord1,n_coord2)
	'''

	def __init__(self, base_path_in='collisions_i'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.power_th = numpy.zeros((0,0), numpy.float64, order='C')
		self.power_fast = numpy.zeros((0,0), numpy.float64, order='C')
		self.torque_th = numpy.zeros((0,0), numpy.float64, order='C')
		self.torque_fast = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class collisions_istruct_arraydist_collisional_transfer_2dObj\n'
		s = self.power_th.__str__()
		ret = ret + space + 'Attribute power_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.power_fast.__str__()
		ret = ret + space + 'Attribute power_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_th.__str__()
		ret = ret + space + 'Attribute torque_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_fast.__str__()
		ret = ret + space + 'Attribute torque_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_istruct_arraydist_collisional_transfer_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'power_th', i, numpy.array(self.power_th).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'power_fast', i, numpy.array(self.power_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'torque_th', i, numpy.array(self.torque_th).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'torque_fast', i, numpy.array(self.torque_fast).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_istruct_arraydist_collisional_transfer_2dObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		status, ret_power_th = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'power_th', i)
		check_status(status)
		if not status:
			self.power_th = ret_power_th
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		status, ret_power_fast = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'power_fast', i)
		check_status(status)
		if not status:
			self.power_fast = ret_power_fast
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		status, ret_torque_th = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'torque_th', i)
		check_status(status)
		if not status:
			self.torque_th = ret_torque_th
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		status, ret_torque_fast = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'torque_fast', i)
		check_status(status)
		if not status:
			self.torque_fast = ret_torque_fast

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_istruct_arraydist_collisional_transfer_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_istruct_arraydist_collisional_transfer_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class collisions_zstruct_arraydist_profiles2d_collisions_z:
	'''
	class collisions_zstruct_arraydist_profiles2d_collisions_z
	Collisional exchange from each background impurities species to the distribution function. Time-dependent; Vector (nimpur)

	Attributes:
	- array : list of collisions_zstruct_arraydist_profiles2d_collisions_zObj 
	'''

	def __init__(self, base_path_in='collisions_z'):
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
		ret = space + 'class collisions_zstruct_arraydist_profiles2d_collisions_z\n'
		for i in range(len(self.array)):
			ret = ret + space + 'collisions_zstruct_arraydist_profiles2d_collisions_z[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(collisions_zstruct_arraydist_profiles2d_collisions_zObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles2d_collisions_z, run function putSlice') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles2d_collisions_z, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles2d_collisions_z, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles2d_collisions_z, run function getSlice') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles2d_collisions_z, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(collisions_zstruct_arraydist_profiles2d_collisions_z(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(collisions_zstruct_arraydist_profiles2d_collisions_z(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = collisions_zstruct_arraydist_profiles2d_collisions_z(self.base_path)
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles2d_collisions_z, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles2d_collisions_z, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles2d_collisions_z, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type collisions_zstruct_arraydist_profiles2d_collisions_z, run function getNonTimedElt') 
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


class collisions_zstruct_arraydist_profiles2d_collisions_zObj:
	'''
	class collisions_zstruct_arraydist_profiles2d_collisions_zObj
	Collisional exchange from each background impurities species to the distribution function. Time-dependent; Vector (nimpur)

	Attributes:
	- charge_state : class charge_statestruct_arraydist_collisional_transfer_2d: array of charge_statestruct_arraydist_collisional_transfer_2dObj objects
	   Collisional exchange from each charge state (or bundled charge state) to the distribution function. Time-dependent; Vector (nzimp)
	'''

	def __init__(self, base_path_in='collisions_z'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.charge_state = charge_statestruct_arraydist_collisional_transfer_2d('charge_state')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class collisions_zstruct_arraydist_profiles2d_collisions_zObj\n'
		ret = ret + space + 'Attribute charge_state\n ' + self.charge_state.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.charge_state.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_zstruct_arraydist_profiles2d_collisions_zObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.charge_state.putTimedElt(path, cpopath + 'charge_state', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_zstruct_arraydist_profiles2d_collisions_zObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.charge_state.getTimedElt(path, cpopath + 'charge_state', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_zstruct_arraydist_profiles2d_collisions_zObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.charge_state.putNonTimedElt(path, cpopath + 'charge_state', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type collisions_zstruct_arraydist_profiles2d_collisions_zObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.charge_state.getNonTimedElt(path, cpopath + 'charge_state', i, obj)


class charge_statestruct_arraydist_collisional_transfer_2d:
	'''
	class charge_statestruct_arraydist_collisional_transfer_2d
	Collisional exchange from each charge state (or bundled charge state) to the distribution function. Time-dependent; Vector (nzimp)

	Attributes:
	- array : list of charge_statestruct_arraydist_collisional_transfer_2dObj 
	'''

	def __init__(self, base_path_in='charge_state'):
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
		ret = space + 'class charge_statestruct_arraydist_collisional_transfer_2d\n'
		for i in range(len(self.array)):
			ret = ret + space + 'charge_statestruct_arraydist_collisional_transfer_2d[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(charge_statestruct_arraydist_collisional_transfer_2dObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_2d, run function putSlice') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_2d, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_2d, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_2d, run function getSlice') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_2d, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(charge_statestruct_arraydist_collisional_transfer_2d(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(charge_statestruct_arraydist_collisional_transfer_2d(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = charge_statestruct_arraydist_collisional_transfer_2d(self.base_path)
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_2d, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_2d, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_2d, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type charge_statestruct_arraydist_collisional_transfer_2d, run function getNonTimedElt') 
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


class charge_statestruct_arraydist_collisional_transfer_2dObj:
	'''
	class charge_statestruct_arraydist_collisional_transfer_2dObj
	Collisional exchange from each charge state (or bundled charge state) to the distribution function. Time-dependent; Vector (nzimp)

	Attributes:
	- power_th : numpy.ndarray 2D with float
	   Collisional power density to the thermal particle population [W.m^-3]; Time-dependent; Matrix(n_coord1,n_coord2)
	- power_fast : numpy.ndarray 2D with float
	   Collisional power density to the fast particle population [W.m^-3]; Time-dependent; Matrix(n_coord1,n_coord2)
	- torque_th : numpy.ndarray 2D with float
	   Collisional toroidal torque density to the thermal particle population [N.m^-2]; Time-dependent; Matrix(n_coord1,n_coord2)
	- torque_fast : numpy.ndarray 2D with float
	   Collisional toroidal torque density to the fast particle population [N.m^-2]; Time-dependent; Matrix(n_coord1,n_coord2)
	'''

	def __init__(self, base_path_in='charge_state'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.power_th = numpy.zeros((0,0), numpy.float64, order='C')
		self.power_fast = numpy.zeros((0,0), numpy.float64, order='C')
		self.torque_th = numpy.zeros((0,0), numpy.float64, order='C')
		self.torque_fast = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class charge_statestruct_arraydist_collisional_transfer_2dObj\n'
		s = self.power_th.__str__()
		ret = ret + space + 'Attribute power_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.power_fast.__str__()
		ret = ret + space + 'Attribute power_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_th.__str__()
		ret = ret + space + 'Attribute torque_th\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.torque_fast.__str__()
		ret = ret + space + 'Attribute torque_fast\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type charge_statestruct_arraydist_collisional_transfer_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'power_th', i, numpy.array(self.power_th).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'power_fast', i, numpy.array(self.power_fast).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'torque_th', i, numpy.array(self.torque_th).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'torque_fast', i, numpy.array(self.torque_fast).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type charge_statestruct_arraydist_collisional_transfer_2dObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'power_th') 
			print ('obj = ' + str(obj))
		status, ret_power_th = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'power_th', i)
		check_status(status)
		if not status:
			self.power_th = ret_power_th
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'power_fast') 
			print ('obj = ' + str(obj))
		status, ret_power_fast = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'power_fast', i)
		check_status(status)
		if not status:
			self.power_fast = ret_power_fast
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'torque_th') 
			print ('obj = ' + str(obj))
		status, ret_torque_th = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'torque_th', i)
		check_status(status)
		if not status:
			self.torque_th = ret_torque_th
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'torque_fast') 
			print ('obj = ' + str(obj))
		status, ret_torque_fast = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'torque_fast', i)
		check_status(status)
		if not status:
			self.torque_fast = ret_torque_fast

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type charge_statestruct_arraydist_collisional_transfer_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type charge_statestruct_arraydist_collisional_transfer_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class trappedstructuredist_profile_values_2d:
	'''
	class trappedstructuredist_profile_values_2d
	2D profiles evaluated using the trapped particle part of the distribution.

	Attributes:
	- state : class statestructuredist_state_2d
	   Fluid moments describing the state of the distribution; calculated from the distribution. Time-dependent
	- collisions_e : class collisions_estructuredist_collisional_transfer_2d
	   Collisional exchange from the background electrons to the distribution function. Time-dependent
	- collisions_i : class collisions_istruct_arraydist_collisional_transfer_2d: array of collisions_istruct_arraydist_collisional_transfer_2dObj objects
	   Collisional exchange from each background ion speices to the distribution function. Time-dependent; Vector (nions)
	- collisions_z : class collisions_zstruct_arraydist_profiles2d_collisions_z: array of collisions_zstruct_arraydist_profiles2d_collisions_zObj objects
	   Collisional exchange from each background impurities species to the distribution function. Time-dependent; Vector (nimpur)
	'''

	def __init__(self, base_path_in='trapped'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.state = statestructuredist_state_2d('state')
		self.collisions_e = collisions_estructuredist_collisional_transfer_2d('collisions_e')
		self.collisions_i = collisions_istruct_arraydist_collisional_transfer_2d('collisions_i')
		self.collisions_z = collisions_zstruct_arraydist_profiles2d_collisions_z('collisions_z')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class trappedstructuredist_profile_values_2d\n'
		ret = ret + space + 'Attribute state\n ' + self.state.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_e\n ' + self.collisions_e.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_i\n ' + self.collisions_i.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_z\n ' + self.collisions_z.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.state.setExpIdx(idx)
		self.collisions_e.setExpIdx(idx)
		self.collisions_i.setExpIdx(idx)
		self.collisions_z.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trappedstructuredist_profile_values_2d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.cpoTime = self.cpoTime
		self.state.putSlice(path, cpopath)
		self.collisions_e.cpoTime = self.cpoTime
		self.collisions_e.putSlice(path, cpopath)
		self.collisions_i.cpoTime = self.cpoTime
		self.collisions_i.putSlice(path, cpopath)
		self.collisions_z.cpoTime = self.cpoTime
		self.collisions_z.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trappedstructuredist_profile_values_2d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.replaceLastSlice(path, cpopath)
		self.collisions_e.replaceLastSlice(path, cpopath)
		self.collisions_i.replaceLastSlice(path, cpopath)
		self.collisions_z.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type trappedstructuredist_profile_values_2d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.putNonTimed(path, cpopath)
		self.collisions_e.putNonTimed(path, cpopath)
		self.collisions_i.putNonTimed(path, cpopath)
		self.collisions_z.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type trappedstructuredist_profile_values_2d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_e.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_i.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_z.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type trappedstructuredist_profile_values_2d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			stateList = self.state.build_non_resampled_data(path, cpopath, nbslice)
			collisions_eList = self.collisions_e.build_non_resampled_data(path, cpopath, nbslice)
			collisions_iList = self.collisions_i.build_non_resampled_data(path, cpopath, nbslice)
			collisions_zList = self.collisions_z.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = trappedstructuredist_profile_values_2d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.state = stateList[i]
				slice.collisions_e = collisions_eList[i]
				slice.collisions_i = collisions_iList[i]
				slice.collisions_z = collisions_zList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trappedstructuredist_profile_values_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.state.putTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putTimedElt(path, cpopath + 'collisions_z', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trappedstructuredist_profile_values_2dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.state.getTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getTimedElt(path, cpopath + 'collisions_z', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trappedstructuredist_profile_values_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.state.putNonTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type trappedstructuredist_profile_values_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.state.getNonTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getNonTimedElt(path, cpopath + 'collisions_z', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.deleteData(path, cpopath)
		self.collisions_e.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'collisions_i')
		ull.deleteData(self.idx, path, cpopath + 'collisions_z')


class co_passingstructuredist_profile_values_2d:
	'''
	class co_passingstructuredist_profile_values_2d
	2D profiles evaluated using the co-current passing particle part of the distribution.

	Attributes:
	- state : class statestructuredist_state_2d
	   Fluid moments describing the state of the distribution; calculated from the distribution. Time-dependent
	- collisions_e : class collisions_estructuredist_collisional_transfer_2d
	   Collisional exchange from the background electrons to the distribution function. Time-dependent
	- collisions_i : class collisions_istruct_arraydist_collisional_transfer_2d: array of collisions_istruct_arraydist_collisional_transfer_2dObj objects
	   Collisional exchange from each background ion speices to the distribution function. Time-dependent; Vector (nions)
	- collisions_z : class collisions_zstruct_arraydist_profiles2d_collisions_z: array of collisions_zstruct_arraydist_profiles2d_collisions_zObj objects
	   Collisional exchange from each background impurities species to the distribution function. Time-dependent; Vector (nimpur)
	'''

	def __init__(self, base_path_in='co_passing'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.state = statestructuredist_state_2d('state')
		self.collisions_e = collisions_estructuredist_collisional_transfer_2d('collisions_e')
		self.collisions_i = collisions_istruct_arraydist_collisional_transfer_2d('collisions_i')
		self.collisions_z = collisions_zstruct_arraydist_profiles2d_collisions_z('collisions_z')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class co_passingstructuredist_profile_values_2d\n'
		ret = ret + space + 'Attribute state\n ' + self.state.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_e\n ' + self.collisions_e.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_i\n ' + self.collisions_i.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_z\n ' + self.collisions_z.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.state.setExpIdx(idx)
		self.collisions_e.setExpIdx(idx)
		self.collisions_i.setExpIdx(idx)
		self.collisions_z.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type co_passingstructuredist_profile_values_2d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.cpoTime = self.cpoTime
		self.state.putSlice(path, cpopath)
		self.collisions_e.cpoTime = self.cpoTime
		self.collisions_e.putSlice(path, cpopath)
		self.collisions_i.cpoTime = self.cpoTime
		self.collisions_i.putSlice(path, cpopath)
		self.collisions_z.cpoTime = self.cpoTime
		self.collisions_z.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type co_passingstructuredist_profile_values_2d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.replaceLastSlice(path, cpopath)
		self.collisions_e.replaceLastSlice(path, cpopath)
		self.collisions_i.replaceLastSlice(path, cpopath)
		self.collisions_z.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type co_passingstructuredist_profile_values_2d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.putNonTimed(path, cpopath)
		self.collisions_e.putNonTimed(path, cpopath)
		self.collisions_i.putNonTimed(path, cpopath)
		self.collisions_z.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type co_passingstructuredist_profile_values_2d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_e.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_i.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_z.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type co_passingstructuredist_profile_values_2d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			stateList = self.state.build_non_resampled_data(path, cpopath, nbslice)
			collisions_eList = self.collisions_e.build_non_resampled_data(path, cpopath, nbslice)
			collisions_iList = self.collisions_i.build_non_resampled_data(path, cpopath, nbslice)
			collisions_zList = self.collisions_z.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = co_passingstructuredist_profile_values_2d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.state = stateList[i]
				slice.collisions_e = collisions_eList[i]
				slice.collisions_i = collisions_iList[i]
				slice.collisions_z = collisions_zList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type co_passingstructuredist_profile_values_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.state.putTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putTimedElt(path, cpopath + 'collisions_z', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type co_passingstructuredist_profile_values_2dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.state.getTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getTimedElt(path, cpopath + 'collisions_z', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type co_passingstructuredist_profile_values_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.state.putNonTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type co_passingstructuredist_profile_values_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.state.getNonTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getNonTimedElt(path, cpopath + 'collisions_z', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.deleteData(path, cpopath)
		self.collisions_e.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'collisions_i')
		ull.deleteData(self.idx, path, cpopath + 'collisions_z')


class cntr_passingstructuredist_profile_values_2d:
	'''
	class cntr_passingstructuredist_profile_values_2d
	2D profiles evaluated using the counter-current passing particle part of the distribution.

	Attributes:
	- state : class statestructuredist_state_2d
	   Fluid moments describing the state of the distribution; calculated from the distribution. Time-dependent
	- collisions_e : class collisions_estructuredist_collisional_transfer_2d
	   Collisional exchange from the background electrons to the distribution function. Time-dependent
	- collisions_i : class collisions_istruct_arraydist_collisional_transfer_2d: array of collisions_istruct_arraydist_collisional_transfer_2dObj objects
	   Collisional exchange from each background ion speices to the distribution function. Time-dependent; Vector (nions)
	- collisions_z : class collisions_zstruct_arraydist_profiles2d_collisions_z: array of collisions_zstruct_arraydist_profiles2d_collisions_zObj objects
	   Collisional exchange from each background impurities species to the distribution function. Time-dependent; Vector (nimpur)
	'''

	def __init__(self, base_path_in='cntr_passing'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.state = statestructuredist_state_2d('state')
		self.collisions_e = collisions_estructuredist_collisional_transfer_2d('collisions_e')
		self.collisions_i = collisions_istruct_arraydist_collisional_transfer_2d('collisions_i')
		self.collisions_z = collisions_zstruct_arraydist_profiles2d_collisions_z('collisions_z')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class cntr_passingstructuredist_profile_values_2d\n'
		ret = ret + space + 'Attribute state\n ' + self.state.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_e\n ' + self.collisions_e.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_i\n ' + self.collisions_i.__str__(depth+1)
		ret = ret + space + 'Attribute collisions_z\n ' + self.collisions_z.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.state.setExpIdx(idx)
		self.collisions_e.setExpIdx(idx)
		self.collisions_i.setExpIdx(idx)
		self.collisions_z.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type cntr_passingstructuredist_profile_values_2d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.cpoTime = self.cpoTime
		self.state.putSlice(path, cpopath)
		self.collisions_e.cpoTime = self.cpoTime
		self.collisions_e.putSlice(path, cpopath)
		self.collisions_i.cpoTime = self.cpoTime
		self.collisions_i.putSlice(path, cpopath)
		self.collisions_z.cpoTime = self.cpoTime
		self.collisions_z.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type cntr_passingstructuredist_profile_values_2d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.replaceLastSlice(path, cpopath)
		self.collisions_e.replaceLastSlice(path, cpopath)
		self.collisions_i.replaceLastSlice(path, cpopath)
		self.collisions_z.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type cntr_passingstructuredist_profile_values_2d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.putNonTimed(path, cpopath)
		self.collisions_e.putNonTimed(path, cpopath)
		self.collisions_i.putNonTimed(path, cpopath)
		self.collisions_z.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type cntr_passingstructuredist_profile_values_2d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_e.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_i.getSlice(path, cpopath, inTime, interpolMode)
		self.collisions_z.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type cntr_passingstructuredist_profile_values_2d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			stateList = self.state.build_non_resampled_data(path, cpopath, nbslice)
			collisions_eList = self.collisions_e.build_non_resampled_data(path, cpopath, nbslice)
			collisions_iList = self.collisions_i.build_non_resampled_data(path, cpopath, nbslice)
			collisions_zList = self.collisions_z.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = cntr_passingstructuredist_profile_values_2d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.state = stateList[i]
				slice.collisions_e = collisions_eList[i]
				slice.collisions_i = collisions_iList[i]
				slice.collisions_z = collisions_zList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type cntr_passingstructuredist_profile_values_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.state.putTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putTimedElt(path, cpopath + 'collisions_z', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type cntr_passingstructuredist_profile_values_2dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.state.getTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getTimedElt(path, cpopath + 'collisions_z', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type cntr_passingstructuredist_profile_values_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.state.putNonTimedElt(path, cpopath + 'state', i, obj)
		obj = self.collisions_e.putNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		obj = self.collisions_i.putNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		obj = self.collisions_z.putNonTimedElt(path, cpopath + 'collisions_z', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type cntr_passingstructuredist_profile_values_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.state.getNonTimedElt(path, cpopath + 'state', i, obj)
		self.collisions_e.getNonTimedElt(path, cpopath + 'collisions_e', i, obj)
		self.collisions_i.getNonTimedElt(path, cpopath + 'collisions_i', i, obj)
		self.collisions_z.getNonTimedElt(path, cpopath + 'collisions_z', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.state.deleteData(path, cpopath)
		self.collisions_e.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'collisions_i')
		ull.deleteData(self.idx, path, cpopath + 'collisions_z')


class dist_funcstructuredist_func:
	'''
	class dist_funcstructuredist_func
	Distribution functions. The total distribution total distribution can either be given by the a set of markers/test particles (in markers), or by a gridded function (dist_expand). Note that the gridded distribution can be written as sum of successive approximations, where each term is given by an element in the vector dist_expand. Finally, the distribution can be written as a sum of a marker distribution and a gridded distribution, e.g. for delta-f Monte Carlo solution. Time-dependent

	Attributes:
	- is_delta_f : int
	   If is_delta_f=1, then the distribution represents the deviation from a Maxwellian; is_delta_f=0, then the distribution represents all particles, i.e. the full-f solution. Time-dependent
	- markers : class markersstructureweighted_markers
	   Distribution represented by a set of markers (test particles). Time-dependent
	- f_expan_topo : class f_expan_topostruct_arraydist_ff: array of f_expan_topostruct_arraydist_ffObj objects
	   TO BE REMOVED. KEPT TEMPORARILY AS AN ALTERNATIVE TO f_expansion. [Distribution function, f, expanded into a vector of successive approximations (topology-based formulation, without the grid-cpo). The first element in the vector (f_expansion(1)) is the zeroth order distribution function, while the K:th elemnet in the vector (f_expansion(K)) is the K:th correction, such that the total distribution function is a sum over all elements in the f_expansion vector. Time-dependent. Structure array(Nf_expansion)]. Time-dependent
	- f_expansion : class f_expansionstruct_arrayf_expansion: array of f_expansionstruct_arrayf_expansionObj objects
	   Distribution function, f, expanded into a vector of successive approximations. The first element in the vector (f_expansion(1)) is the zeroth order distribution function, while the K:th element in the vector (f_expansion(K)) is the K:th correction, such that the total distribution function is a sum over all elements in the f_expansion vector. Time-dependent. Structure array(Nf_expansion) 
	'''

	def __init__(self, base_path_in='dist_func'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.is_delta_f = EMPTY_INT
		self.markers = markersstructureweighted_markers('markers')
		self.f_expan_topo = f_expan_topostruct_arraydist_ff('f_expan_topo')
		self.f_expansion = f_expansionstruct_arrayf_expansion('f_expansion')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class dist_funcstructuredist_func\n'
		ret = ret + space + 'Attribute is_delta_f: ' + str(self.is_delta_f) + '\n'
		ret = ret + space + 'Attribute markers\n ' + self.markers.__str__(depth+1)
		ret = ret + space + 'Attribute f_expan_topo\n ' + self.f_expan_topo.__str__(depth+1)
		ret = ret + space + 'Attribute f_expansion\n ' + self.f_expansion.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.markers.setExpIdx(idx)
		self.f_expan_topo.setExpIdx(idx)
		self.f_expansion.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dist_funcstructuredist_func, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'is_delta_f', self.is_delta_f, self.cpoTime)
		check_status(status)
		self.markers.cpoTime = self.cpoTime
		self.markers.putSlice(path, cpopath)
		self.f_expan_topo.cpoTime = self.cpoTime
		self.f_expan_topo.putSlice(path, cpopath)
		self.f_expansion.cpoTime = self.cpoTime
		self.f_expansion.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dist_funcstructuredist_func, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'is_delta_f', self.is_delta_f)
		check_status(status)
		self.markers.replaceLastSlice(path, cpopath)
		self.f_expan_topo.replaceLastSlice(path, cpopath)
		self.f_expansion.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type dist_funcstructuredist_func, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.markers.putNonTimed(path, cpopath)
		self.f_expan_topo.putNonTimed(path, cpopath)
		self.f_expansion.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type dist_funcstructuredist_func, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_is_delta_f, retTime = ull.getIntSlice(self.idx, path, cpopath + 'is_delta_f', inTime, interpolMode)
		check_status(status)
		if not status:
			self.is_delta_f = ret_is_delta_f
			self.cpoTime = retTime
		self.markers.getSlice(path, cpopath, inTime, interpolMode)
		self.f_expan_topo.getSlice(path, cpopath, inTime, interpolMode)
		self.f_expansion.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type dist_funcstructuredist_func, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, is_delta_fList = ull.getVect1DInt(self.idx, path, cpopath + 'is_delta_f')
			if len(is_delta_fList) == 0:
				is_delta_fList = numpy.resize(is_delta_fList, (nbslice))
			check_status(status)
			markersList = self.markers.build_non_resampled_data(path, cpopath, nbslice)
			f_expan_topoList = self.f_expan_topo.build_non_resampled_data(path, cpopath, nbslice)
			f_expansionList = self.f_expansion.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = dist_funcstructuredist_func(self.base_path)
				slice.setExpIdx(self.idx)
				slice.is_delta_f = int(is_delta_fList[i].copy())
				slice.markers = markersList[i]
				slice.f_expan_topo = f_expan_topoList[i]
				slice.f_expansion = f_expansionList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dist_funcstructuredist_funcObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'is_delta_f') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'is_delta_f', i, self.is_delta_f)
		obj = self.markers.putTimedElt(path, cpopath + 'markers', i, obj)
		obj = self.f_expan_topo.putTimedElt(path, cpopath + 'f_expan_topo', i, obj)
		obj = self.f_expansion.putTimedElt(path, cpopath + 'f_expansion', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dist_funcstructuredist_funcObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'is_delta_f') 
			print ('obj = ' + str(obj))
		status, ret_is_delta_f = ull.getIntFromObject(self.idx, obj, cpopath + 'is_delta_f', i)
		check_status(status)
		if not status:
			self.is_delta_f = ret_is_delta_f
		self.markers.getTimedElt(path, cpopath + 'markers', i, obj)
		self.f_expan_topo.getTimedElt(path, cpopath + 'f_expan_topo', i, obj)
		self.f_expansion.getTimedElt(path, cpopath + 'f_expansion', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dist_funcstructuredist_funcObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.markers.putNonTimedElt(path, cpopath + 'markers', i, obj)
		obj = self.f_expan_topo.putNonTimedElt(path, cpopath + 'f_expan_topo', i, obj)
		obj = self.f_expansion.putNonTimedElt(path, cpopath + 'f_expansion', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type dist_funcstructuredist_funcObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.markers.getNonTimedElt(path, cpopath + 'markers', i, obj)
		self.f_expan_topo.getNonTimedElt(path, cpopath + 'f_expan_topo', i, obj)
		self.f_expansion.getNonTimedElt(path, cpopath + 'f_expansion', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'is_delta_f')
		self.markers.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'f_expan_topo')
		ull.deleteData(self.idx, path, cpopath + 'f_expansion')


class markersstructureweighted_markers:
	'''
	class markersstructureweighted_markers
	Distribution represented by a set of markers (test particles). Time-dependent

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


class variable_idsstruct_arrayidentifierObj:
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


class f_expan_topostruct_arraydist_ff:
	'''
	class f_expan_topostruct_arraydist_ff
	TO BE REMOVED. KEPT TEMPORARILY AS AN ALTERNATIVE TO f_expansion. [Distribution function, f, expanded into a vector of successive approximations (topology-based formulation, without the grid-cpo). The first element in the vector (f_expansion(1)) is the zeroth order distribution function, while the K:th elemnet in the vector (f_expansion(K)) is the K:th correction, such that the total distribution function is a sum over all elements in the f_expansion vector. Time-dependent. Structure array(Nf_expansion)]. Time-dependent

	Attributes:
	- array : list of f_expan_topostruct_arraydist_ffObj 
	'''

	def __init__(self, base_path_in='f_expan_topo'):
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
		ret = space + 'class f_expan_topostruct_arraydist_ff\n'
		for i in range(len(self.array)):
			ret = ret + space + 'f_expan_topostruct_arraydist_ff[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(f_expan_topostruct_arraydist_ffObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type f_expan_topostruct_arraydist_ff, run function putSlice') 
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
			print ('field '+self.base_path+' of type f_expan_topostruct_arraydist_ff, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type f_expan_topostruct_arraydist_ff, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type f_expan_topostruct_arraydist_ff, run function getSlice') 
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
			print ('field '+self.base_path+' of type f_expan_topostruct_arraydist_ff, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(f_expan_topostruct_arraydist_ff(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(f_expan_topostruct_arraydist_ff(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = f_expan_topostruct_arraydist_ff(self.base_path)
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
			print ('field '+self.base_path+' of type f_expan_topostruct_arraydist_ff, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type f_expan_topostruct_arraydist_ff, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type f_expan_topostruct_arraydist_ff, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type f_expan_topostruct_arraydist_ff, run function getNonTimedElt') 
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


class f_expan_topostruct_arraydist_ffObj:
	'''
	class f_expan_topostruct_arraydist_ffObj
	TO BE REMOVED. KEPT TEMPORARILY AS AN ALTERNATIVE TO f_expansion. [Distribution function, f, expanded into a vector of successive approximations (topology-based formulation, without the grid-cpo). The first element in the vector (f_expansion(1)) is the zeroth order distribution function, while the K:th elemnet in the vector (f_expansion(K)) is the K:th correction, such that the total distribution function is a sum over all elements in the f_expansion vector. Time-dependent. Structure array(Nf_expansion)]. Time-dependent

	Attributes:
	- grid_info : class grid_infostructuredist_grid_info
	   Specification of grids used in topo_regions. Grid coordinates could either be invariants of motion, or information at single point along orbit, e.g. xi and s for grid_coord=3. This point should always be on a so-called omnigenous surface (a generalised equitorial plane); grad(psi) x grad(B) = 0. All closed orbits cross omnigenous surfaces at least two times. The omnigenous surfaces are described in omnigen_surf.
	- topo_regions : class topo_regionsstruct_arraytopo_regions: array of topo_regionsstruct_arraytopo_regionsObj objects
	   List with distribution function in each topological region; Time-dependent. Structure array(nregion_topo)
	'''

	def __init__(self, base_path_in='f_expan_topo'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.grid_info = grid_infostructuredist_grid_info('grid_info')
		self.topo_regions = topo_regionsstruct_arraytopo_regions('topo_regions')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class f_expan_topostruct_arraydist_ffObj\n'
		ret = ret + space + 'Attribute grid_info\n ' + self.grid_info.__str__(depth+1)
		ret = ret + space + 'Attribute topo_regions\n ' + self.topo_regions.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.grid_info.setExpIdx(idx)
		self.topo_regions.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type f_expan_topostruct_arraydist_ffObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.topo_regions.putTimedElt(path, cpopath + 'topo_regions', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type f_expan_topostruct_arraydist_ffObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.topo_regions.getTimedElt(path, cpopath + 'topo_regions', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type f_expan_topostruct_arraydist_ffObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.grid_info.putNonTimedElt(path, cpopath + 'grid_info', i, obj)
		obj = self.topo_regions.putNonTimedElt(path, cpopath + 'topo_regions', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type f_expan_topostruct_arraydist_ffObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.grid_info.getNonTimedElt(path, cpopath + 'grid_info', i, obj)
		self.topo_regions.getNonTimedElt(path, cpopath + 'topo_regions', i, obj)


class grid_infostructuredist_grid_info:
	'''
	class grid_infostructuredist_grid_info
	Specification of grids used in topo_regions. Grid coordinates could either be invariants of motion, or information at single point along orbit, e.g. xi and s for grid_coord=3. This point should always be on a so-called omnigenous surface (a generalised equitorial plane); grad(psi) x grad(B) = 0. All closed orbits cross omnigenous surfaces at least two times. The omnigenous surfaces are described in omnigen_surf.

	Attributes:
	- grid_type : int
	   Type of grid: 1=unstructured grid; 2=structured non-rectangular grid, here ndim11=ndim12=ndim13, ndim21=ndim22=ndim23, ndim31=ndim32=ndim33; 3=rectangular grid, where grid coordinates are stored in the vectors dim1(1:ndim1,1,1), dim2(1,1:ndim2,1), dim3(1,1,1:ndim3)
	- ngriddim : int
	   Number of grid dimension. For ngriddim=2 the grid is specified by dim1 and dim2 only, while dim3, dim4, dim5, dim6 can be ignored (should not be allocated). For ngriddim=3 also dim3 is used to describe the grid etc. E.g. if your distribution is given by the three variables the poloidal flux, perpendicular and parallel velocities, then ngriddim=3 and grid_coord(1)=15, grid_coord(1)=16, grid_coord(3)=6.
	- grid_coord : numpy.ndarray 1D with int)
	   Identifies the coordinates specifies in dim1, dim2, dim3, dim4, dim5, and dim6. grid_coord(K) describes the coordinate representaed in dimK, for K=1,2...6. The possible coordinates are: 1=R, Major radius [m]; 2=Z, Vertical position [m]; 3=X, first cartesian coordinate in the horizontal plane [m]; 4=Y, second cartesian coordinate in the horizontal plane (grad(X) x grad(Y) = grad(Z)) [m]; 5=phi, toroidal angle [rad]; 6=psi, poloidal magnetic flux [T*m^2]; 7=rhotor, the square root of the toroidal flux; 8=theta, geometrical poloidal angle [rad]; 9=theta_b, Boozer poloidal angle [rad]; 10=vx, velocity in the x-direction [m/s]; 11=vy, velocity in the y-direction [m/s]; 12=vz, velocity in the z-direction [m/s]; 13=vel, total velocity [m/s]; 14=vphi, velocity in the phi-direction [m/s]; 15=vpar, velocity in the parallel direction [m/s]; 16=vperp, velocity in the perpendicular direction [m/s]; 17=E, Hamiltonian energy [J]; 18=Pphi, canonical toroidal angular momentum [kg m^2/s]; 19=mu, magnetic moment [J/T]; 20=Lambda=mu/E [1/T]; 21=pitch=vpar/v [-]; 22=s, the position of the omnigenous plane (generalised equitorial plane) as described by the fields omnigen_surf%s and omnigen_surf%rz; 23=particle spin; 24=n_Legendre, the index of the Legendre polynomial of the pitch, e.g. if the k:th component of dim3(1,1,k,1,1,1)=5 then this refer to the 5:th Legendre polynomial P_5(xi). Vector (6)
	- thin_orbits : int
	   Specifies if guiding centre orbits are thin. Note: only used for orbit averaged distribution functions. For thin_orbits=1 the orbit are considered thin, i.e. each orbit is bound to follow a single flux surface; for thin_orbits=0 the orbits are asumed to follow guiding centre trajectories. E.g. thin_orbits=0 using constants of motion as given in a generalised equitorial plane, then the orbit outside the equitorial plane are described by the guiding centre equations of motion.
	- topology : str
	   Description of the topology of the grid. NOTE: only used for nregion_topo>2.
	- omnigen_surf : class omnigen_surfstruct_arrayomnigen_surf: array of omnigen_surfstruct_arrayomnigen_surfObj objects
	   List of omnigeuous magnetic surfaces to which the s-coordinates in grid_coord refer. NOTE: only used for gridcoord=3. NOTE: all guiding centre orbits intersect at least one omnigeuous (or stagnation) surfaces, i.e. the omnigeuous generalised the equitorial plane (the midplane). nsurfs=Number of omnigenous surfaces. Structure array(nregion_topo)
	'''

	def __init__(self, base_path_in='grid_info'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.grid_type = EMPTY_INT
		self.ngriddim = EMPTY_INT
		self.grid_coord = numpy.zeros(0, numpy.int32, order='C')
		self.thin_orbits = EMPTY_INT
		self.topology = ''
		self.omnigen_surf = omnigen_surfstruct_arrayomnigen_surf('omnigen_surf')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class grid_infostructuredist_grid_info\n'
		ret = ret + space + 'Attribute grid_type: ' + str(self.grid_type) + '\n'
		ret = ret + space + 'Attribute ngriddim: ' + str(self.ngriddim) + '\n'
		s = self.grid_coord.__str__()
		ret = ret + space + 'Attribute grid_coord\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute thin_orbits: ' + str(self.thin_orbits) + '\n'
		ret = ret + space + 'Attribute topology: ' + str(self.topology) + '\n'
		ret = ret + space + 'Attribute omnigen_surf\n ' + self.omnigen_surf.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.omnigen_surf.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type grid_infostructuredist_grid_info, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.omnigen_surf.cpoTime = self.cpoTime
		self.omnigen_surf.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type grid_infostructuredist_grid_info, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.omnigen_surf.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type grid_infostructuredist_grid_info, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putInt(self.idx, path, cpopath + 'grid_type', self.grid_type)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'ngriddim', self.ngriddim)
		check_status(status)
		status = ull.putVect1DInt(self.idx, path, cpopath + 'grid_coord', numpy.array(self.grid_coord).astype(numpy.int32), False)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'thin_orbits', self.thin_orbits)
		check_status(status)
		status = ull.putString(self.idx, path, cpopath + 'topology', self.topology)
		check_status(status)
		self.omnigen_surf.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type grid_infostructuredist_grid_info, run function getSlice') 
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
		status, ret_ngriddim = ull.getInt(self.idx, path, cpopath + 'ngriddim')
		check_status(status)
		if not status:
			self.ngriddim = ret_ngriddim
		status, ret_grid_coord = ull.getVect1DInt(self.idx, path, cpopath + 'grid_coord')
		check_status(status)
		if not status:
			self.grid_coord = ret_grid_coord
		status, ret_thin_orbits = ull.getInt(self.idx, path, cpopath + 'thin_orbits')
		check_status(status)
		if not status:
			self.thin_orbits = ret_thin_orbits
		status, ret_topology = ull.getString(self.idx, path, cpopath + 'topology')
		check_status(status)
		if not status:
			self.topology = ret_topology
		self.omnigen_surf.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type grid_infostructuredist_grid_info, run function build_non_resampled_data') 
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
			status, ngriddimVal = ull.getInt(self.idx, path, cpopath + 'ngriddim')
			check_status(status)
			status, grid_coordVal = ull.getVect1DInt(self.idx, path, cpopath + 'grid_coord')
			check_status(status)
			status, thin_orbitsVal = ull.getInt(self.idx, path, cpopath + 'thin_orbits')
			check_status(status)
			status, topologyVal = ull.getString(self.idx, path, cpopath + 'topology')
			check_status(status)
			omnigen_surfList = self.omnigen_surf.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = grid_infostructuredist_grid_info(self.base_path)
				slice.setExpIdx(self.idx)
				slice.grid_type = grid_typeVal
				slice.ngriddim = ngriddimVal
				slice.grid_coord = grid_coordVal
				slice.thin_orbits = thin_orbitsVal
				slice.topology = topologyVal
				slice.omnigen_surf = omnigen_surfList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type grid_infostructuredist_grid_infoObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type grid_infostructuredist_grid_infoObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type grid_infostructuredist_grid_infoObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'grid_type') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'grid_type', i, self.grid_type)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'ngriddim') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'ngriddim', i, self.ngriddim)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'grid_coord') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'grid_coord', i, numpy.array(self.grid_coord).astype(numpy.int32))
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'thin_orbits') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'thin_orbits', i, self.thin_orbits)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'topology') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'topology', i, self.topology)
		obj = self.omnigen_surf.putNonTimedElt(path, cpopath + 'omnigen_surf', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type grid_infostructuredist_grid_infoObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'grid_type') 
			print ('obj = ' + str(obj))
		status, ret_grid_type = ull.getIntFromObject(self.idx, obj, cpopath + 'grid_type', i)
		check_status(status)
		if not status:
			self.grid_type = ret_grid_type
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'ngriddim') 
			print ('obj = ' + str(obj))
		status, ret_ngriddim = ull.getIntFromObject(self.idx, obj, cpopath + 'ngriddim', i)
		check_status(status)
		if not status:
			self.ngriddim = ret_ngriddim
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'grid_coord') 
			print ('obj = ' + str(obj))
		status, ret_grid_coord = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'grid_coord', i)
		check_status(status)
		if not status:
			self.grid_coord = ret_grid_coord
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'thin_orbits') 
			print ('obj = ' + str(obj))
		status, ret_thin_orbits = ull.getIntFromObject(self.idx, obj, cpopath + 'thin_orbits', i)
		check_status(status)
		if not status:
			self.thin_orbits = ret_thin_orbits
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'topology') 
			print ('obj = ' + str(obj))
		status, ret_topology = ull.getStringFromObject(self.idx, obj, cpopath + 'topology', i)
		check_status(status)
		if not status:
			self.topology = ret_topology
		self.omnigen_surf.getNonTimedElt(path, cpopath + 'omnigen_surf', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'grid_type')
		ull.deleteData(self.idx, path, cpopath + 'ngriddim')
		ull.deleteData(self.idx, path, cpopath + 'grid_coord')
		ull.deleteData(self.idx, path, cpopath + 'thin_orbits')
		ull.deleteData(self.idx, path, cpopath + 'topology')
		ull.deleteData(self.idx, path, cpopath + 'omnigen_surf')


class omnigen_surfstruct_arrayomnigen_surf:
	'''
	class omnigen_surfstruct_arrayomnigen_surf
	List of omnigeuous magnetic surfaces to which the s-coordinates in grid_coord refer. NOTE: only used for gridcoord=3. NOTE: all guiding centre orbits intersect at least one omnigeuous (or stagnation) surfaces, i.e. the omnigeuous generalised the equitorial plane (the midplane). nsurfs=Number of omnigenous surfaces. Structure array(nregion_topo)

	Attributes:
	- array : list of omnigen_surfstruct_arrayomnigen_surfObj 
	'''

	def __init__(self, base_path_in='omnigen_surf'):
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
		ret = space + 'class omnigen_surfstruct_arrayomnigen_surf\n'
		for i in range(len(self.array)):
			ret = ret + space + 'omnigen_surfstruct_arrayomnigen_surf[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(omnigen_surfstruct_arrayomnigen_surfObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type omnigen_surfstruct_arrayomnigen_surf, run function putSlice') 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type omnigen_surfstruct_arrayomnigen_surf, run function replaceLastSlice') 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type omnigen_surfstruct_arrayomnigen_surf, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type omnigen_surfstruct_arrayomnigen_surf, run function getSlice') 
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
			print ('field '+self.base_path+' of type omnigen_surfstruct_arrayomnigen_surf, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', NON_TIMED)')
			status, obj = ull.getObject(self.idx, path, cpopath + self.base_path, NON_TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (NON_TIMED)')
				for i in range(nbslice):
					list.append(omnigen_surfstruct_arrayomnigen_surf(self.base_path))
				return list
			obj_size = ull.getObjectDim(self.idx, obj)
			for n in range(nbslice):
				slice = omnigen_surfstruct_arrayomnigen_surf(self.base_path)
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
			print ('field '+self.base_path+' of type omnigen_surfstruct_arrayomnigen_surf, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type omnigen_surfstruct_arrayomnigen_surf, run function getNonTimedElt') 
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


class omnigen_surfstruct_arrayomnigen_surfObj:
	'''
	class omnigen_surfstruct_arrayomnigen_surfObj
	List of omnigeuous magnetic surfaces to which the s-coordinates in grid_coord refer. NOTE: only used for gridcoord=3. NOTE: all guiding centre orbits intersect at least one omnigeuous (or stagnation) surfaces, i.e. the omnigeuous generalised the equitorial plane (the midplane). nsurfs=Number of omnigenous surfaces. Structure array(nregion_topo)

	Attributes:
	- rz : class rzstructurerz1D
	   (R,z) coordinates of the omnigeuous magnetic surfaces (generalised equitorial plane). NOTE: only used for gridcoord=3. Vector rz1d (nsurfs)
	- s : numpy.ndarray 1D with float
	   Coordinates which uniquely maps the omnigeuous magnetic surfaces (generalised equitorial plane). NOTE: only used for gridcoord=3. Vector (nsurfs)
	'''

	def __init__(self, base_path_in='omnigen_surf'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.rz = rzstructurerz1D('rz')
		self.s = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class omnigen_surfstruct_arrayomnigen_surfObj\n'
		ret = ret + space + 'Attribute rz\n ' + self.rz.__str__(depth+1)
		s = self.s.__str__()
		ret = ret + space + 'Attribute s\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.rz.setExpIdx(idx)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type omnigen_surfstruct_arrayomnigen_surfObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.rz.putNonTimedElt(path, cpopath + 'rz', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 's') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 's', i, numpy.array(self.s).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type omnigen_surfstruct_arrayomnigen_surfObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.rz.getNonTimedElt(path, cpopath + 'rz', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 's') 
			print ('obj = ' + str(obj))
		status, ret_s = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 's', i)
		check_status(status)
		if not status:
			self.s = ret_s


class rzstructurerz1D:
	'''
	class rzstructurerz1D
	(R,z) coordinates of the omnigeuous magnetic surfaces (generalised equitorial plane). NOTE: only used for gridcoord=3. Vector rz1d (nsurfs)

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]
	- z : numpy.ndarray 1D with float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='rz'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class rzstructurerz1D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rzstructurerz1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rzstructurerz1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type rzstructurerz1D, run function putNonTimed') 
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

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type rzstructurerz1D, run function getSlice') 
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

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type rzstructurerz1D, run function build_non_resampled_data') 
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
			for i in range(nbslice):
				slice = rzstructurerz1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rVal
				slice.z = zVal
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rzstructurerz1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rzstructurerz1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rzstructurerz1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type rzstructurerz1DObj, run function getNonTimedElt') 
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

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'r')
		ull.deleteData(self.idx, path, cpopath + 'z')


class topo_regionsstruct_arraytopo_regions:
	'''
	class topo_regionsstruct_arraytopo_regions
	List with distribution function in each topological region; Time-dependent. Structure array(nregion_topo)

	Attributes:
	- array : list of topo_regionsstruct_arraytopo_regionsObj 
	'''

	def __init__(self, base_path_in='topo_regions'):
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
		ret = space + 'class topo_regionsstruct_arraytopo_regions\n'
		for i in range(len(self.array)):
			ret = ret + space + 'topo_regionsstruct_arraytopo_regions[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(topo_regionsstruct_arraytopo_regionsObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type topo_regionsstruct_arraytopo_regions, run function putSlice') 
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
			print ('field '+self.base_path+' of type topo_regionsstruct_arraytopo_regions, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type topo_regionsstruct_arraytopo_regions, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type topo_regionsstruct_arraytopo_regions, run function getSlice') 
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
			print ('field '+self.base_path+' of type topo_regionsstruct_arraytopo_regions, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(topo_regionsstruct_arraytopo_regions(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(topo_regionsstruct_arraytopo_regions(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = topo_regionsstruct_arraytopo_regions(self.base_path)
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
			print ('field '+self.base_path+' of type topo_regionsstruct_arraytopo_regions, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type topo_regionsstruct_arraytopo_regions, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type topo_regionsstruct_arraytopo_regions, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type topo_regionsstruct_arraytopo_regions, run function getNonTimedElt') 
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


class topo_regionsstruct_arraytopo_regionsObj:
	'''
	class topo_regionsstruct_arraytopo_regionsObj
	List with distribution function in each topological region; Time-dependent. Structure array(nregion_topo)

	Attributes:
	- ind_omnigen : int
	   Index of the omnigeuous magnetic surfaces (generalised equitorial plane) to which the s-coordinates refer. NOTE: only used for gridcoord=3.
	- dim1 : numpy.ndarray 6D with float
	   First dimension in phase space; Time-dependent; Array6d(ndim11, ndim21, ndim31, ndim41, ndim51, ndim61).
	- dim2 : numpy.ndarray 6D with float
	   Second dimension in phase space; Time-dependent; Array6d(ndim12, ndim22, ndim32, ndim42, ndim52, ndim62).
	- dim3 : numpy.ndarray 6D with float
	   Third dimension in phase space; Time-dependent; Array6d(ndim13, ndim23, ndim33, ndim43, ndim53, ndim63).
	- dim4 : numpy.ndarray 6D with float
	   Fourth dimension in phase space; Time-dependent; Array6d(ndim14, ndim24, ndim34, ndim44, ndim54, ndim64).
	- dim5 : numpy.ndarray 6D with float
	   Fifth dimension in phase space; Time-dependent; Array6d(ndim15, ndim25, ndim35, ndim45, ndim55, ndim65).
	- dim6 : numpy.ndarray 6D with float
	   Sixth dimension in phase space; Time-dependent; Array6d(ndim16, ndim26, ndim36, ndim46, ndim56, ndim66).
	- jacobian : numpy.ndarray 6D with float
	   Jacobian of the transformation of the phase space grid variables; Time-dependent; Array6d(ndim11, ndim22, ndim33, ndim44, ndim55, ndim66).
	- distfunc : numpy.ndarray 6D with float
	   Orbit (or bounce) averaged distribution function given on a grid [1/m^3 (m/s)^-3]; Time-dependent; Array6d(ndim11, ndim22, ndim33, ndim44, ndim55, ndim66).
	'''

	def __init__(self, base_path_in='topo_regions'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.ind_omnigen = EMPTY_INT
		self.dim1 = numpy.zeros((0,0,0,0,0,0), numpy.float64, order='C')
		self.dim2 = numpy.zeros((0,0,0,0,0,0), numpy.float64, order='C')
		self.dim3 = numpy.zeros((0,0,0,0,0,0), numpy.float64, order='C')
		self.dim4 = numpy.zeros((0,0,0,0,0,0), numpy.float64, order='C')
		self.dim5 = numpy.zeros((0,0,0,0,0,0), numpy.float64, order='C')
		self.dim6 = numpy.zeros((0,0,0,0,0,0), numpy.float64, order='C')
		self.jacobian = numpy.zeros((0,0,0,0,0,0), numpy.float64, order='C')
		self.distfunc = numpy.zeros((0,0,0,0,0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class topo_regionsstruct_arraytopo_regionsObj\n'
		ret = ret + space + 'Attribute ind_omnigen: ' + str(self.ind_omnigen) + '\n'
		s = self.dim1.__str__()
		ret = ret + space + 'Attribute dim1\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim2.__str__()
		ret = ret + space + 'Attribute dim2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim3.__str__()
		ret = ret + space + 'Attribute dim3\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim4.__str__()
		ret = ret + space + 'Attribute dim4\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim5.__str__()
		ret = ret + space + 'Attribute dim5\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim6.__str__()
		ret = ret + space + 'Attribute dim6\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.jacobian.__str__()
		ret = ret + space + 'Attribute jacobian\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.distfunc.__str__()
		ret = ret + space + 'Attribute distfunc\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type topo_regionsstruct_arraytopo_regionsObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect6DDoubleInObject : ' + cpopath + 'dim1') 
			print ('obj = ' + str(obj))
		obj = ull.putVect6DDoubleInObject(self.idx, obj, cpopath + 'dim1', i, numpy.array(self.dim1).astype(numpy.float64))
		if (dev()):
			print ('putVect6DDoubleInObject : ' + cpopath + 'dim2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect6DDoubleInObject(self.idx, obj, cpopath + 'dim2', i, numpy.array(self.dim2).astype(numpy.float64))
		if (dev()):
			print ('putVect6DDoubleInObject : ' + cpopath + 'dim3') 
			print ('obj = ' + str(obj))
		obj = ull.putVect6DDoubleInObject(self.idx, obj, cpopath + 'dim3', i, numpy.array(self.dim3).astype(numpy.float64))
		if (dev()):
			print ('putVect6DDoubleInObject : ' + cpopath + 'dim4') 
			print ('obj = ' + str(obj))
		obj = ull.putVect6DDoubleInObject(self.idx, obj, cpopath + 'dim4', i, numpy.array(self.dim4).astype(numpy.float64))
		if (dev()):
			print ('putVect6DDoubleInObject : ' + cpopath + 'dim5') 
			print ('obj = ' + str(obj))
		obj = ull.putVect6DDoubleInObject(self.idx, obj, cpopath + 'dim5', i, numpy.array(self.dim5).astype(numpy.float64))
		if (dev()):
			print ('putVect6DDoubleInObject : ' + cpopath + 'dim6') 
			print ('obj = ' + str(obj))
		obj = ull.putVect6DDoubleInObject(self.idx, obj, cpopath + 'dim6', i, numpy.array(self.dim6).astype(numpy.float64))
		if (dev()):
			print ('putVect6DDoubleInObject : ' + cpopath + 'jacobian') 
			print ('obj = ' + str(obj))
		obj = ull.putVect6DDoubleInObject(self.idx, obj, cpopath + 'jacobian', i, numpy.array(self.jacobian).astype(numpy.float64))
		if (dev()):
			print ('putVect6DDoubleInObject : ' + cpopath + 'distfunc') 
			print ('obj = ' + str(obj))
		obj = ull.putVect6DDoubleInObject(self.idx, obj, cpopath + 'distfunc', i, numpy.array(self.distfunc).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type topo_regionsstruct_arraytopo_regionsObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect6DDoubleInObject : ' + cpopath + 'dim1') 
			print ('obj = ' + str(obj))
		status, ret_dim1 = ull.getVect6DDoubleFromObject(self.idx, obj, cpopath + 'dim1', i)
		check_status(status)
		if not status:
			self.dim1 = ret_dim1
		if (dev()):
			print ('getVect6DDoubleInObject : ' + cpopath + 'dim2') 
			print ('obj = ' + str(obj))
		status, ret_dim2 = ull.getVect6DDoubleFromObject(self.idx, obj, cpopath + 'dim2', i)
		check_status(status)
		if not status:
			self.dim2 = ret_dim2
		if (dev()):
			print ('getVect6DDoubleInObject : ' + cpopath + 'dim3') 
			print ('obj = ' + str(obj))
		status, ret_dim3 = ull.getVect6DDoubleFromObject(self.idx, obj, cpopath + 'dim3', i)
		check_status(status)
		if not status:
			self.dim3 = ret_dim3
		if (dev()):
			print ('getVect6DDoubleInObject : ' + cpopath + 'dim4') 
			print ('obj = ' + str(obj))
		status, ret_dim4 = ull.getVect6DDoubleFromObject(self.idx, obj, cpopath + 'dim4', i)
		check_status(status)
		if not status:
			self.dim4 = ret_dim4
		if (dev()):
			print ('getVect6DDoubleInObject : ' + cpopath + 'dim5') 
			print ('obj = ' + str(obj))
		status, ret_dim5 = ull.getVect6DDoubleFromObject(self.idx, obj, cpopath + 'dim5', i)
		check_status(status)
		if not status:
			self.dim5 = ret_dim5
		if (dev()):
			print ('getVect6DDoubleInObject : ' + cpopath + 'dim6') 
			print ('obj = ' + str(obj))
		status, ret_dim6 = ull.getVect6DDoubleFromObject(self.idx, obj, cpopath + 'dim6', i)
		check_status(status)
		if not status:
			self.dim6 = ret_dim6
		if (dev()):
			print ('getVect6DDoubleInObject : ' + cpopath + 'jacobian') 
			print ('obj = ' + str(obj))
		status, ret_jacobian = ull.getVect6DDoubleFromObject(self.idx, obj, cpopath + 'jacobian', i)
		check_status(status)
		if not status:
			self.jacobian = ret_jacobian
		if (dev()):
			print ('getVect6DDoubleInObject : ' + cpopath + 'distfunc') 
			print ('obj = ' + str(obj))
		status, ret_distfunc = ull.getVect6DDoubleFromObject(self.idx, obj, cpopath + 'distfunc', i)
		check_status(status)
		if not status:
			self.distfunc = ret_distfunc

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type topo_regionsstruct_arraytopo_regionsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'ind_omnigen') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'ind_omnigen', i, self.ind_omnigen)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type topo_regionsstruct_arraytopo_regionsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'ind_omnigen') 
			print ('obj = ' + str(obj))
		status, ret_ind_omnigen = ull.getIntFromObject(self.idx, obj, cpopath + 'ind_omnigen', i)
		check_status(status)
		if not status:
			self.ind_omnigen = ret_ind_omnigen


class f_expansionstruct_arrayf_expansion:
	'''
	class f_expansionstruct_arrayf_expansion
	Distribution function, f, expanded into a vector of successive approximations. The first element in the vector (f_expansion(1)) is the zeroth order distribution function, while the K:th element in the vector (f_expansion(K)) is the K:th correction, such that the total distribution function is a sum over all elements in the f_expansion vector. Time-dependent. Structure array(Nf_expansion) 

	Attributes:
	- array : list of f_expansionstruct_arrayf_expansionObj 
	'''

	def __init__(self, base_path_in='f_expansion'):
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
		ret = space + 'class f_expansionstruct_arrayf_expansion\n'
		for i in range(len(self.array)):
			ret = ret + space + 'f_expansionstruct_arrayf_expansion[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(f_expansionstruct_arrayf_expansionObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type f_expansionstruct_arrayf_expansion, run function putSlice') 
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
			print ('field '+self.base_path+' of type f_expansionstruct_arrayf_expansion, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type f_expansionstruct_arrayf_expansion, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type f_expansionstruct_arrayf_expansion, run function getSlice') 
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
			print ('field '+self.base_path+' of type f_expansionstruct_arrayf_expansion, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(f_expansionstruct_arrayf_expansion(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(f_expansionstruct_arrayf_expansion(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = f_expansionstruct_arrayf_expansion(self.base_path)
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
			print ('field '+self.base_path+' of type f_expansionstruct_arrayf_expansion, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type f_expansionstruct_arrayf_expansion, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type f_expansionstruct_arrayf_expansion, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type f_expansionstruct_arrayf_expansion, run function getNonTimedElt') 
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


class f_expansionstruct_arrayf_expansionObj:
	'''
	class f_expansionstruct_arrayf_expansionObj
	Distribution function, f, expanded into a vector of successive approximations. The first element in the vector (f_expansion(1)) is the zeroth order distribution function, while the K:th element in the vector (f_expansion(K)) is the K:th correction, such that the total distribution function is a sum over all elements in the f_expansion vector. Time-dependent. Structure array(Nf_expansion) 

	Attributes:
	- grid : class gridstructurecomplexgrid
	   Grid for storing the distribution function. Time-dependent; Complexgrid
	- values : class valuesstructurecomplexgrid_scalar
	   Values of the distribution function [m^-3 (m/s)^-3]. Time-dependent; Complexgrid_scalar.
	- parameters : class parametersstructuredist_distrivec_distfunc_fexp_param
	   Parameters used to defined the grid coordinates. Time-dependent
	'''

	def __init__(self, base_path_in='f_expansion'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.grid = gridstructurecomplexgrid('grid')
		self.values = valuesstructurecomplexgrid_scalar('values')
		self.parameters = parametersstructuredist_distrivec_distfunc_fexp_param('parameters')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class f_expansionstruct_arrayf_expansionObj\n'
		ret = ret + space + 'Attribute grid\n ' + self.grid.__str__(depth+1)
		ret = ret + space + 'Attribute values\n ' + self.values.__str__(depth+1)
		ret = ret + space + 'Attribute parameters\n ' + self.parameters.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.grid.setExpIdx(idx)
		self.values.setExpIdx(idx)
		self.parameters.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type f_expansionstruct_arrayf_expansionObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.grid.putTimedElt(path, cpopath + 'grid', i, obj)
		obj = self.values.putTimedElt(path, cpopath + 'values', i, obj)
		obj = self.parameters.putTimedElt(path, cpopath + 'parameters', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type f_expansionstruct_arrayf_expansionObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.grid.getTimedElt(path, cpopath + 'grid', i, obj)
		self.values.getTimedElt(path, cpopath + 'values', i, obj)
		self.parameters.getTimedElt(path, cpopath + 'parameters', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type f_expansionstruct_arrayf_expansionObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		obj = self.grid.putNonTimedElt(path, cpopath + 'grid', i, obj)
		obj = self.values.putNonTimedElt(path, cpopath + 'values', i, obj)
		obj = self.parameters.putNonTimedElt(path, cpopath + 'parameters', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type f_expansionstruct_arrayf_expansionObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		self.grid.getNonTimedElt(path, cpopath + 'grid', i, obj)
		self.values.getNonTimedElt(path, cpopath + 'values', i, obj)
		self.parameters.getNonTimedElt(path, cpopath + 'parameters', i, obj)


class gridstructurecomplexgrid:
	'''
	class gridstructurecomplexgrid
	Grid for storing the distribution function. Time-dependent; Complexgrid

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


class spacesstruct_arraycomplexgrid_spaceObj:
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


class objectsstruct_arrayobjectsObj:
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


class subgridsstruct_arraycomplexgrid_subgridObj:
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


class liststruct_arraycomplexgrid_objectlistObj:
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


class indsetstruct_arraycomplexgrid_indexlistObj:
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


class metricstructurecomplexgrid_metric:
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


class measurestruct_arraycomplexgrid_scalarObj:
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


class g11struct_arraycomplexgrid_scalarObj:
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


class g12struct_arraycomplexgrid_scalarObj:
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


class g13struct_arraycomplexgrid_scalarObj:
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


class g22struct_arraycomplexgrid_scalarObj:
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


class g23struct_arraycomplexgrid_scalarObj:
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


class g33struct_arraycomplexgrid_scalarObj:
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


class jacobianstruct_arraycomplexgrid_scalarObj:
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


class geostruct_arraycomplexgrid_geo_globalObj:
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


class geo_matrixstruct_arraycomplexgrid_scalarObj:
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


class basesstruct_arraycomplexgrid_vectorObj:
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


class compstruct_arraycomplexgrid_scalarObj:
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


class valuesstructurecomplexgrid_scalar:
	'''
	class valuesstructurecomplexgrid_scalar
	Values of the distribution function [m^-3 (m/s)^-3]. Time-dependent; Complexgrid_scalar.

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

	def __init__(self, base_path_in='values'):
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
		ret = space + 'class valuesstructurecomplexgrid_scalar\n'
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
			print ('field '+self.base_path+' of type valuesstructurecomplexgrid_scalar, run function putSlice') 
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
			print ('field '+self.base_path+' of type valuesstructurecomplexgrid_scalar, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type valuesstructurecomplexgrid_scalar, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type valuesstructurecomplexgrid_scalar, run function getSlice') 
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
			print ('field '+self.base_path+' of type valuesstructurecomplexgrid_scalar, run function build_non_resampled_data') 
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
				slice = valuesstructurecomplexgrid_scalar(self.base_path)
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
			print ('object of type valuesstructurecomplexgrid_scalarObj, run function putTimedElt') 
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
			print ('object of type valuesstructurecomplexgrid_scalarObj, run function getTimedElt') 
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
			print ('object of type valuesstructurecomplexgrid_scalarObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type valuesstructurecomplexgrid_scalarObj, run function getNonTimedElt') 
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


class parametersstructuredist_distrivec_distfunc_fexp_param:
	'''
	class parametersstructuredist_distrivec_distfunc_fexp_param
	Parameters used to defined the grid coordinates. Time-dependent

	Attributes:
	- equatorial : class equatorialstructureequatorial_plane
	   Description of the equatorial plane or any other omnigeuous surfaces. Time-dependent
	- temperature : numpy.ndarray 1D with float
	   Reference temperature profile (eV); on the grid in /distsource/source/profiles_1d/rho_tor. Used to define the local thermal energy and the thermal velocity. Time-dependent; Vector(npsi)
	'''

	def __init__(self, base_path_in='parameters'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.equatorial = equatorialstructureequatorial_plane('equatorial')
		self.temperature = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class parametersstructuredist_distrivec_distfunc_fexp_param\n'
		ret = ret + space + 'Attribute equatorial\n ' + self.equatorial.__str__(depth+1)
		s = self.temperature.__str__()
		ret = ret + space + 'Attribute temperature\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.equatorial.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type parametersstructuredist_distrivec_distfunc_fexp_param, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.equatorial.cpoTime = self.cpoTime
		self.equatorial.putSlice(path, cpopath)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'temperature', numpy.array(self.temperature).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type parametersstructuredist_distrivec_distfunc_fexp_param, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.equatorial.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'temperature', numpy.array(self.temperature).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type parametersstructuredist_distrivec_distfunc_fexp_param, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.equatorial.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type parametersstructuredist_distrivec_distfunc_fexp_param, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.equatorial.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_temperature, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'temperature', inTime, interpolMode)
		check_status(status)
		if not status:
			self.temperature = ret_temperature
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type parametersstructuredist_distrivec_distfunc_fexp_param, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			equatorialList = self.equatorial.build_non_resampled_data(path, cpopath, nbslice)
			status, temperatureList = ull.getVect2DDouble(self.idx, path, cpopath + 'temperature')
			if len(temperatureList) == 0:
				temperatureList = numpy.resize(temperatureList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = parametersstructuredist_distrivec_distfunc_fexp_param(self.base_path)
				slice.setExpIdx(self.idx)
				slice.equatorial = equatorialList[i]
				slice.temperature = temperatureList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parametersstructuredist_distrivec_distfunc_fexp_paramObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.equatorial.putTimedElt(path, cpopath + 'equatorial', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'temperature') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'temperature', i, numpy.array(self.temperature).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parametersstructuredist_distrivec_distfunc_fexp_paramObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.equatorial.getTimedElt(path, cpopath + 'equatorial', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'temperature') 
			print ('obj = ' + str(obj))
		status, ret_temperature = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'temperature', i)
		check_status(status)
		if not status:
			self.temperature = ret_temperature

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parametersstructuredist_distrivec_distfunc_fexp_paramObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.equatorial.putNonTimedElt(path, cpopath + 'equatorial', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type parametersstructuredist_distrivec_distfunc_fexp_paramObj, run function getNonTimedElt') 
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
		ull.deleteData(self.idx, path, cpopath + 'temperature')


class equatorialstructureequatorial_plane:
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
