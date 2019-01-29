# -*- coding: utf-8 -*-
from .ualdef import *
from . import ual_low_level_wrapper as ull
import numpy

class equilibrium:
	'''
	class equilibrium
	Description of a 2D, axi-symmetric, tokamak equilibrium; result of an equilibrium code. Time-dependent CPO

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- eqconstraint : class eqconstraintstructureeqconstraint
	   measurements to constrain the equilibrium, output values and accuracy of the fit
	- eqgeometry : class eqgeometrystructureeqgeometry
	   Geometry of the plasma boundary
	- flush : class flushstructureflush
	   FLUSH package coefficients for the mapping of the equlibrium. The time grid of this structure is the same as the equilibrium structure above.
	- global_param : class global_paramstructureglobal_param
	   0d output parameters
	- profiles_1d : class profiles_1dstructureprofiles_1d
	   output profiles as a function of the poloidal flux
	- profiles_2d : class profiles_2dstruct_arrayequilibrium_profiles_2d: array of profiles_2dstruct_arrayequilibrium_profiles_2dObj objects
	   Output profiles in the poloidal plane. Time-dependent
	- coord_sys : class coord_sysstructurecoord_sys
	   flux surface coordinate system on a square grid of flux and angle
	- time : float
	   Time [s]; Time-dependent; Scalar
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self):
		self.base_path = 'equilibrium'
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.maxOccurrences = 5
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.eqconstraint = eqconstraintstructureeqconstraint('eqconstraint')
		self.eqgeometry = eqgeometrystructureeqgeometry('eqgeometry')
		self.flush = flushstructureflush('flush')
		self.global_param = global_paramstructureglobal_param('global_param')
		self.profiles_1d = profiles_1dstructureprofiles_1d('profiles_1d')
		self.profiles_2d = profiles_2dstruct_arrayequilibrium_profiles_2d('profiles_2d')
		self.coord_sys = coord_sysstructurecoord_sys('coord_sys')
		self.time = EMPTY_DOUBLE
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class equilibrium\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute eqconstraint\n ' + self.eqconstraint.__str__(depth+1)
		ret = ret + space + 'Attribute eqgeometry\n ' + self.eqgeometry.__str__(depth+1)
		ret = ret + space + 'Attribute flush\n ' + self.flush.__str__(depth+1)
		ret = ret + space + 'Attribute global_param\n ' + self.global_param.__str__(depth+1)
		ret = ret + space + 'Attribute profiles_1d\n ' + self.profiles_1d.__str__(depth+1)
		ret = ret + space + 'Attribute profiles_2d\n ' + self.profiles_2d.__str__(depth+1)
		ret = ret + space + 'Attribute coord_sys\n ' + self.coord_sys.__str__(depth+1)
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.eqconstraint.setExpIdx(idx)
		self.eqgeometry.setExpIdx(idx)
		self.flush.setExpIdx(idx)
		self.global_param.setExpIdx(idx)
		self.profiles_1d.setExpIdx(idx)
		self.profiles_2d.setExpIdx(idx)
		self.coord_sys.setExpIdx(idx)
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
		self.eqconstraint.cpoTime = self.cpoTime
		self.eqconstraint.putSlice(path, cpopath)
		self.eqgeometry.cpoTime = self.cpoTime
		self.eqgeometry.putSlice(path, cpopath)
		self.flush.cpoTime = self.cpoTime
		self.flush.putSlice(path, cpopath)
		self.global_param.cpoTime = self.cpoTime
		self.global_param.putSlice(path, cpopath)
		self.profiles_1d.cpoTime = self.cpoTime
		self.profiles_1d.putSlice(path, cpopath)
		self.profiles_2d.cpoTime = self.cpoTime
		self.profiles_2d.putSlice(path, cpopath)
		self.coord_sys.cpoTime = self.cpoTime
		self.coord_sys.putSlice(path, cpopath)
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
		self.eqconstraint.replaceLastSlice(path, cpopath)
		self.eqgeometry.replaceLastSlice(path, cpopath)
		self.flush.replaceLastSlice(path, cpopath)
		self.global_param.replaceLastSlice(path, cpopath)
		self.profiles_1d.replaceLastSlice(path, cpopath)
		self.profiles_2d.replaceLastSlice(path, cpopath)
		self.coord_sys.replaceLastSlice(path, cpopath)
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
		self.eqconstraint.putNonTimed(path, cpopath)
		self.eqgeometry.putNonTimed(path, cpopath)
		self.flush.putNonTimed(path, cpopath)
		self.global_param.putNonTimed(path, cpopath)
		self.profiles_1d.putNonTimed(path, cpopath)
		self.profiles_2d.putNonTimed(path, cpopath)
		self.coord_sys.putNonTimed(path, cpopath)
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
		self.eqconstraint.getSlice(path, cpopath, inTime, interpolMode)
		self.eqgeometry.getSlice(path, cpopath, inTime, interpolMode)
		self.flush.getSlice(path, cpopath, inTime, interpolMode)
		self.global_param.getSlice(path, cpopath, inTime, interpolMode)
		self.profiles_1d.getSlice(path, cpopath, inTime, interpolMode)
		self.profiles_2d.getSlice(path, cpopath, inTime, interpolMode)
		self.coord_sys.getSlice(path, cpopath, inTime, interpolMode)
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
			eqconstraintList = self.eqconstraint.build_non_resampled_data(path, cpopath, nbslice)
			eqgeometryList = self.eqgeometry.build_non_resampled_data(path, cpopath, nbslice)
			flushList = self.flush.build_non_resampled_data(path, cpopath, nbslice)
			global_paramList = self.global_param.build_non_resampled_data(path, cpopath, nbslice)
			profiles_1dList = self.profiles_1d.build_non_resampled_data(path, cpopath, nbslice)
			profiles_2dList = self.profiles_2d.build_non_resampled_data(path, cpopath, nbslice)
			coord_sysList = self.coord_sys.build_non_resampled_data(path, cpopath, nbslice)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			array = []
			for i in range(nbslice):
				slice = equilibrium()
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.eqconstraint = eqconstraintList[i]
				slice.eqgeometry = eqgeometryList[i]
				slice.flush = flushList[i]
				slice.global_param = global_paramList[i]
				slice.profiles_1d = profiles_1dList[i]
				slice.profiles_2d = profiles_2dList[i]
				slice.coord_sys = coord_sysList[i]
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
		self.eqconstraint.deleteData(path, cpopath)
		self.eqgeometry.deleteData(path, cpopath)
		self.flush.deleteData(path, cpopath)
		self.global_param.deleteData(path, cpopath)
		self.profiles_1d.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'profiles_2d')
		self.coord_sys.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'time')
		self.codeparam.deleteData(path, cpopath)


class equilibriumArray:
	'''
	class equilibriumArray
	Description of a 2D, axi-symmetric, tokamak equilibrium; result of an equilibrium code. Time-dependent CPO

	Attributes:
	- array : list of equilibrium
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
		ret = space + 'class equilibriumArray nb_cpos=%d\n' %(len(self.array))
		for i in range(len(self.array)):
			ret = ret + space + 'equilibrium cpos=%d\n' %(i) + self.array[i].__str__(depth+1)
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
		cpo = equilibrium()
		cpo.setExpIdx(self.idx)
		self.array = cpo.build_non_resampled_data(occurrence)

	def resize(self, nb_cpos):
		self.array = []
		for i in range(nb_cpos):
			self.array.append(equilibrium())
			self.array[i].setExpIdx(self.idx)

	def deleteAll(self, occurrence=0):
		cpo = equilibrium()
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


class eqconstraintstructureeqconstraint:
	'''
	class eqconstraintstructureeqconstraint
	measurements to constrain the equilibrium, output values and accuracy of the fit

	Attributes:
	- bpol : class bpolstructureeqmes1D
	   poloidal pickup coils [T]
	- bvac_r : class bvac_rstructureeqmes0D
	   Vacuum field times radius in the toroidal field magnet [T.m];
	- diamagflux : class diamagfluxstructureeqmes0D
	   Diamagnetic flux [Wb], defined as integral (Btor - Btor,vac) dS where the integral is over the poloidal cross section of the plasma. It is measured by a single wire loop around the cross section of the torus (e.g. Wesson, Tokamaks, 1997, p.473). It gives information about the separation of the two source profiles p' and FF' of the Grad-Shafranov equation.
	- faraday : class faradaystructureeqmes1D
	   Faraday rotation angles [rad]
	- flux : class fluxstructureeqmes1D
	   Poloidal flux loops [Wb]
	- i_plasma : class i_plasmastructureeqmes0D
	   Plasma current [A];
	- isoflux : class isofluxstructureisoflux
	   Point series at which the flux is considered the same
	- jsurf : class jsurfstructureeqmes1D
	   Average of current density on the flux surface [A/m^2]
	- magnet_iron : class magnet_ironstructuremagnet_iron
	   Magnetisation in iron segments [T]
	- mse : class msestructureeqmes1D
	   MSE angles [rad]
	- ne : class nestructureeqmes1D
	   Electron density [m^-3 for local measurement, m^-2 if line integrated]
	- pfcurrent : class pfcurrentstructureeqmes1D
	   Current in poloidal field coils [A]
	- pressure : class pressurestructureeqmes1D
	   Total pressure [Pa]
	- q : class qstructureq
	   Safety factor
	- xpts : class xptsstructurexpts
	   Position of the X-point(s)
	'''

	def __init__(self, base_path_in='eqconstraint'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.bpol = bpolstructureeqmes1D('bpol')
		self.bvac_r = bvac_rstructureeqmes0D('bvac_r')
		self.diamagflux = diamagfluxstructureeqmes0D('diamagflux')
		self.faraday = faradaystructureeqmes1D('faraday')
		self.flux = fluxstructureeqmes1D('flux')
		self.i_plasma = i_plasmastructureeqmes0D('i_plasma')
		self.isoflux = isofluxstructureisoflux('isoflux')
		self.jsurf = jsurfstructureeqmes1D('jsurf')
		self.magnet_iron = magnet_ironstructuremagnet_iron('magnet_iron')
		self.mse = msestructureeqmes1D('mse')
		self.ne = nestructureeqmes1D('ne')
		self.pfcurrent = pfcurrentstructureeqmes1D('pfcurrent')
		self.pressure = pressurestructureeqmes1D('pressure')
		self.q = qstructureq('q')
		self.xpts = xptsstructurexpts('xpts')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class eqconstraintstructureeqconstraint\n'
		ret = ret + space + 'Attribute bpol\n ' + self.bpol.__str__(depth+1)
		ret = ret + space + 'Attribute bvac_r\n ' + self.bvac_r.__str__(depth+1)
		ret = ret + space + 'Attribute diamagflux\n ' + self.diamagflux.__str__(depth+1)
		ret = ret + space + 'Attribute faraday\n ' + self.faraday.__str__(depth+1)
		ret = ret + space + 'Attribute flux\n ' + self.flux.__str__(depth+1)
		ret = ret + space + 'Attribute i_plasma\n ' + self.i_plasma.__str__(depth+1)
		ret = ret + space + 'Attribute isoflux\n ' + self.isoflux.__str__(depth+1)
		ret = ret + space + 'Attribute jsurf\n ' + self.jsurf.__str__(depth+1)
		ret = ret + space + 'Attribute magnet_iron\n ' + self.magnet_iron.__str__(depth+1)
		ret = ret + space + 'Attribute mse\n ' + self.mse.__str__(depth+1)
		ret = ret + space + 'Attribute ne\n ' + self.ne.__str__(depth+1)
		ret = ret + space + 'Attribute pfcurrent\n ' + self.pfcurrent.__str__(depth+1)
		ret = ret + space + 'Attribute pressure\n ' + self.pressure.__str__(depth+1)
		ret = ret + space + 'Attribute q\n ' + self.q.__str__(depth+1)
		ret = ret + space + 'Attribute xpts\n ' + self.xpts.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.bpol.setExpIdx(idx)
		self.bvac_r.setExpIdx(idx)
		self.diamagflux.setExpIdx(idx)
		self.faraday.setExpIdx(idx)
		self.flux.setExpIdx(idx)
		self.i_plasma.setExpIdx(idx)
		self.isoflux.setExpIdx(idx)
		self.jsurf.setExpIdx(idx)
		self.magnet_iron.setExpIdx(idx)
		self.mse.setExpIdx(idx)
		self.ne.setExpIdx(idx)
		self.pfcurrent.setExpIdx(idx)
		self.pressure.setExpIdx(idx)
		self.q.setExpIdx(idx)
		self.xpts.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type eqconstraintstructureeqconstraint, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.bpol.cpoTime = self.cpoTime
		self.bpol.putSlice(path, cpopath)
		self.bvac_r.cpoTime = self.cpoTime
		self.bvac_r.putSlice(path, cpopath)
		self.diamagflux.cpoTime = self.cpoTime
		self.diamagflux.putSlice(path, cpopath)
		self.faraday.cpoTime = self.cpoTime
		self.faraday.putSlice(path, cpopath)
		self.flux.cpoTime = self.cpoTime
		self.flux.putSlice(path, cpopath)
		self.i_plasma.cpoTime = self.cpoTime
		self.i_plasma.putSlice(path, cpopath)
		self.isoflux.cpoTime = self.cpoTime
		self.isoflux.putSlice(path, cpopath)
		self.jsurf.cpoTime = self.cpoTime
		self.jsurf.putSlice(path, cpopath)
		self.magnet_iron.cpoTime = self.cpoTime
		self.magnet_iron.putSlice(path, cpopath)
		self.mse.cpoTime = self.cpoTime
		self.mse.putSlice(path, cpopath)
		self.ne.cpoTime = self.cpoTime
		self.ne.putSlice(path, cpopath)
		self.pfcurrent.cpoTime = self.cpoTime
		self.pfcurrent.putSlice(path, cpopath)
		self.pressure.cpoTime = self.cpoTime
		self.pressure.putSlice(path, cpopath)
		self.q.cpoTime = self.cpoTime
		self.q.putSlice(path, cpopath)
		self.xpts.cpoTime = self.cpoTime
		self.xpts.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type eqconstraintstructureeqconstraint, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.bpol.replaceLastSlice(path, cpopath)
		self.bvac_r.replaceLastSlice(path, cpopath)
		self.diamagflux.replaceLastSlice(path, cpopath)
		self.faraday.replaceLastSlice(path, cpopath)
		self.flux.replaceLastSlice(path, cpopath)
		self.i_plasma.replaceLastSlice(path, cpopath)
		self.isoflux.replaceLastSlice(path, cpopath)
		self.jsurf.replaceLastSlice(path, cpopath)
		self.magnet_iron.replaceLastSlice(path, cpopath)
		self.mse.replaceLastSlice(path, cpopath)
		self.ne.replaceLastSlice(path, cpopath)
		self.pfcurrent.replaceLastSlice(path, cpopath)
		self.pressure.replaceLastSlice(path, cpopath)
		self.q.replaceLastSlice(path, cpopath)
		self.xpts.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type eqconstraintstructureeqconstraint, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.bpol.putNonTimed(path, cpopath)
		self.bvac_r.putNonTimed(path, cpopath)
		self.diamagflux.putNonTimed(path, cpopath)
		self.faraday.putNonTimed(path, cpopath)
		self.flux.putNonTimed(path, cpopath)
		self.i_plasma.putNonTimed(path, cpopath)
		self.isoflux.putNonTimed(path, cpopath)
		self.jsurf.putNonTimed(path, cpopath)
		self.magnet_iron.putNonTimed(path, cpopath)
		self.mse.putNonTimed(path, cpopath)
		self.ne.putNonTimed(path, cpopath)
		self.pfcurrent.putNonTimed(path, cpopath)
		self.pressure.putNonTimed(path, cpopath)
		self.q.putNonTimed(path, cpopath)
		self.xpts.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type eqconstraintstructureeqconstraint, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.bpol.getSlice(path, cpopath, inTime, interpolMode)
		self.bvac_r.getSlice(path, cpopath, inTime, interpolMode)
		self.diamagflux.getSlice(path, cpopath, inTime, interpolMode)
		self.faraday.getSlice(path, cpopath, inTime, interpolMode)
		self.flux.getSlice(path, cpopath, inTime, interpolMode)
		self.i_plasma.getSlice(path, cpopath, inTime, interpolMode)
		self.isoflux.getSlice(path, cpopath, inTime, interpolMode)
		self.jsurf.getSlice(path, cpopath, inTime, interpolMode)
		self.magnet_iron.getSlice(path, cpopath, inTime, interpolMode)
		self.mse.getSlice(path, cpopath, inTime, interpolMode)
		self.ne.getSlice(path, cpopath, inTime, interpolMode)
		self.pfcurrent.getSlice(path, cpopath, inTime, interpolMode)
		self.pressure.getSlice(path, cpopath, inTime, interpolMode)
		self.q.getSlice(path, cpopath, inTime, interpolMode)
		self.xpts.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type eqconstraintstructureeqconstraint, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			bpolList = self.bpol.build_non_resampled_data(path, cpopath, nbslice)
			bvac_rList = self.bvac_r.build_non_resampled_data(path, cpopath, nbslice)
			diamagfluxList = self.diamagflux.build_non_resampled_data(path, cpopath, nbslice)
			faradayList = self.faraday.build_non_resampled_data(path, cpopath, nbslice)
			fluxList = self.flux.build_non_resampled_data(path, cpopath, nbslice)
			i_plasmaList = self.i_plasma.build_non_resampled_data(path, cpopath, nbslice)
			isofluxList = self.isoflux.build_non_resampled_data(path, cpopath, nbslice)
			jsurfList = self.jsurf.build_non_resampled_data(path, cpopath, nbslice)
			magnet_ironList = self.magnet_iron.build_non_resampled_data(path, cpopath, nbslice)
			mseList = self.mse.build_non_resampled_data(path, cpopath, nbslice)
			neList = self.ne.build_non_resampled_data(path, cpopath, nbslice)
			pfcurrentList = self.pfcurrent.build_non_resampled_data(path, cpopath, nbslice)
			pressureList = self.pressure.build_non_resampled_data(path, cpopath, nbslice)
			qList = self.q.build_non_resampled_data(path, cpopath, nbslice)
			xptsList = self.xpts.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = eqconstraintstructureeqconstraint(self.base_path)
				slice.setExpIdx(self.idx)
				slice.bpol = bpolList[i]
				slice.bvac_r = bvac_rList[i]
				slice.diamagflux = diamagfluxList[i]
				slice.faraday = faradayList[i]
				slice.flux = fluxList[i]
				slice.i_plasma = i_plasmaList[i]
				slice.isoflux = isofluxList[i]
				slice.jsurf = jsurfList[i]
				slice.magnet_iron = magnet_ironList[i]
				slice.mse = mseList[i]
				slice.ne = neList[i]
				slice.pfcurrent = pfcurrentList[i]
				slice.pressure = pressureList[i]
				slice.q = qList[i]
				slice.xpts = xptsList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type eqconstraintstructureeqconstraintObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.bpol.putTimedElt(path, cpopath + 'bpol', i, obj)
		obj = self.bvac_r.putTimedElt(path, cpopath + 'bvac_r', i, obj)
		obj = self.diamagflux.putTimedElt(path, cpopath + 'diamagflux', i, obj)
		obj = self.faraday.putTimedElt(path, cpopath + 'faraday', i, obj)
		obj = self.flux.putTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.i_plasma.putTimedElt(path, cpopath + 'i_plasma', i, obj)
		obj = self.isoflux.putTimedElt(path, cpopath + 'isoflux', i, obj)
		obj = self.jsurf.putTimedElt(path, cpopath + 'jsurf', i, obj)
		obj = self.magnet_iron.putTimedElt(path, cpopath + 'magnet_iron', i, obj)
		obj = self.mse.putTimedElt(path, cpopath + 'mse', i, obj)
		obj = self.ne.putTimedElt(path, cpopath + 'ne', i, obj)
		obj = self.pfcurrent.putTimedElt(path, cpopath + 'pfcurrent', i, obj)
		obj = self.pressure.putTimedElt(path, cpopath + 'pressure', i, obj)
		obj = self.q.putTimedElt(path, cpopath + 'q', i, obj)
		obj = self.xpts.putTimedElt(path, cpopath + 'xpts', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type eqconstraintstructureeqconstraintObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.bpol.getTimedElt(path, cpopath + 'bpol', i, obj)
		self.bvac_r.getTimedElt(path, cpopath + 'bvac_r', i, obj)
		self.diamagflux.getTimedElt(path, cpopath + 'diamagflux', i, obj)
		self.faraday.getTimedElt(path, cpopath + 'faraday', i, obj)
		self.flux.getTimedElt(path, cpopath + 'flux', i, obj)
		self.i_plasma.getTimedElt(path, cpopath + 'i_plasma', i, obj)
		self.isoflux.getTimedElt(path, cpopath + 'isoflux', i, obj)
		self.jsurf.getTimedElt(path, cpopath + 'jsurf', i, obj)
		self.magnet_iron.getTimedElt(path, cpopath + 'magnet_iron', i, obj)
		self.mse.getTimedElt(path, cpopath + 'mse', i, obj)
		self.ne.getTimedElt(path, cpopath + 'ne', i, obj)
		self.pfcurrent.getTimedElt(path, cpopath + 'pfcurrent', i, obj)
		self.pressure.getTimedElt(path, cpopath + 'pressure', i, obj)
		self.q.getTimedElt(path, cpopath + 'q', i, obj)
		self.xpts.getTimedElt(path, cpopath + 'xpts', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type eqconstraintstructureeqconstraintObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.bpol.putNonTimedElt(path, cpopath + 'bpol', i, obj)
		obj = self.bvac_r.putNonTimedElt(path, cpopath + 'bvac_r', i, obj)
		obj = self.diamagflux.putNonTimedElt(path, cpopath + 'diamagflux', i, obj)
		obj = self.faraday.putNonTimedElt(path, cpopath + 'faraday', i, obj)
		obj = self.flux.putNonTimedElt(path, cpopath + 'flux', i, obj)
		obj = self.i_plasma.putNonTimedElt(path, cpopath + 'i_plasma', i, obj)
		obj = self.isoflux.putNonTimedElt(path, cpopath + 'isoflux', i, obj)
		obj = self.jsurf.putNonTimedElt(path, cpopath + 'jsurf', i, obj)
		obj = self.magnet_iron.putNonTimedElt(path, cpopath + 'magnet_iron', i, obj)
		obj = self.mse.putNonTimedElt(path, cpopath + 'mse', i, obj)
		obj = self.ne.putNonTimedElt(path, cpopath + 'ne', i, obj)
		obj = self.pfcurrent.putNonTimedElt(path, cpopath + 'pfcurrent', i, obj)
		obj = self.pressure.putNonTimedElt(path, cpopath + 'pressure', i, obj)
		obj = self.q.putNonTimedElt(path, cpopath + 'q', i, obj)
		obj = self.xpts.putNonTimedElt(path, cpopath + 'xpts', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type eqconstraintstructureeqconstraintObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.bpol.getNonTimedElt(path, cpopath + 'bpol', i, obj)
		self.bvac_r.getNonTimedElt(path, cpopath + 'bvac_r', i, obj)
		self.diamagflux.getNonTimedElt(path, cpopath + 'diamagflux', i, obj)
		self.faraday.getNonTimedElt(path, cpopath + 'faraday', i, obj)
		self.flux.getNonTimedElt(path, cpopath + 'flux', i, obj)
		self.i_plasma.getNonTimedElt(path, cpopath + 'i_plasma', i, obj)
		self.isoflux.getNonTimedElt(path, cpopath + 'isoflux', i, obj)
		self.jsurf.getNonTimedElt(path, cpopath + 'jsurf', i, obj)
		self.magnet_iron.getNonTimedElt(path, cpopath + 'magnet_iron', i, obj)
		self.mse.getNonTimedElt(path, cpopath + 'mse', i, obj)
		self.ne.getNonTimedElt(path, cpopath + 'ne', i, obj)
		self.pfcurrent.getNonTimedElt(path, cpopath + 'pfcurrent', i, obj)
		self.pressure.getNonTimedElt(path, cpopath + 'pressure', i, obj)
		self.q.getNonTimedElt(path, cpopath + 'q', i, obj)
		self.xpts.getNonTimedElt(path, cpopath + 'xpts', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.bpol.deleteData(path, cpopath)
		self.bvac_r.deleteData(path, cpopath)
		self.diamagflux.deleteData(path, cpopath)
		self.faraday.deleteData(path, cpopath)
		self.flux.deleteData(path, cpopath)
		self.i_plasma.deleteData(path, cpopath)
		self.isoflux.deleteData(path, cpopath)
		self.jsurf.deleteData(path, cpopath)
		self.magnet_iron.deleteData(path, cpopath)
		self.mse.deleteData(path, cpopath)
		self.ne.deleteData(path, cpopath)
		self.pfcurrent.deleteData(path, cpopath)
		self.pressure.deleteData(path, cpopath)
		self.q.deleteData(path, cpopath)
		self.xpts.deleteData(path, cpopath)


class bpolstructureeqmes1D:
	'''
	class bpolstructureeqmes1D
	poloidal pickup coils [T]

	Attributes:
	- measured : numpy.ndarray 1D with float
	   Measured value of the signal; Time-dependent; Array(nmeas)
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- time : float
	   Exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used);Time-dependent; Scalar
	- exact : numpy.ndarray 1D with int)
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; Time-dependent; Array(nmeas)
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); Time-dependent; Array(nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Array(nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Array(nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Array(nmeas)
	'''

	def __init__(self, base_path_in='bpol'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = numpy.zeros(0, numpy.int32, order='C')
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class bpolstructureeqmes1D\n'
		s = self.measured.__str__()
		ret = ret + space + 'Attribute measured\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		s = self.exact.__str__()
		ret = ret + space + 'Attribute exact\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bpolstructureeqmes1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bpolstructureeqmes1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bpolstructureeqmes1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type bpolstructureeqmes1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'exact', inTime, interpolMode)
		check_status(status)
		if not status:
			self.exact = ret_exact
			self.cpoTime = retTime
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type bpolstructureeqmes1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect2DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactList = ull.getVect2DInt(self.idx, path, cpopath + 'exact')
			if len(exactList) == 0:
				exactList = numpy.resize(exactList, (0,nbslice))
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = bpolstructureeqmes1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[:,i]
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactList[:,i]
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bpolstructureeqmes1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'measured', i, numpy.array(self.measured).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'exact', i, numpy.array(self.exact).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bpolstructureeqmes1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bpolstructureeqmes1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bpolstructureeqmes1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class bvac_rstructureeqmes0D:
	'''
	class bvac_rstructureeqmes0D
	Vacuum field times radius in the toroidal field magnet [T.m];

	Attributes:
	- measured : float
	   Measured value of the signal; Time-dependent; Scalar.
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal); String
	- time : float
	   Time (exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used); Time-dependent; Scalar.
	- exact : int
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; scalar integer
	- weight : float
	   weight given to the measurement (>= 0); Time-dependent; Scalar.
	- sigma : float
	   standard deviation of the measurement; Time-dependent; Scalar.
	- calculated : float
	   Signal as recalculated by the equilibrium code; Time-dependent; Scalar.
	- chi2 : float
	   chi^2 of (calculated-measured); Time-dependent; Scalar.
	'''

	def __init__(self, base_path_in='bvac_r'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = EMPTY_DOUBLE
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = EMPTY_INT
		self.weight = EMPTY_DOUBLE
		self.sigma = EMPTY_DOUBLE
		self.calculated = EMPTY_DOUBLE
		self.chi2 = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class bvac_rstructureeqmes0D\n'
		ret = ret + space + 'Attribute measured: ' + str(self.measured) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		ret = ret + space + 'Attribute exact: ' + str(self.exact) + '\n'
		ret = ret + space + 'Attribute weight: ' + str(self.weight) + '\n'
		ret = ret + space + 'Attribute sigma: ' + str(self.sigma) + '\n'
		ret = ret + space + 'Attribute calculated: ' + str(self.calculated) + '\n'
		ret = ret + space + 'Attribute chi2: ' + str(self.chi2) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bvac_rstructureeqmes0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'measured', self.measured, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'weight', self.weight, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'sigma', self.sigma, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'calculated', self.calculated, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'chi2', self.chi2, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bvac_rstructureeqmes0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'measured', self.measured)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'weight', self.weight)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'sigma', self.sigma)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'calculated', self.calculated)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'chi2', self.chi2)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type bvac_rstructureeqmes0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'exact', self.exact)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type bvac_rstructureeqmes0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact = ull.getInt(self.idx, path, cpopath + 'exact')
		check_status(status)
		if not status:
			self.exact = ret_exact
		status, ret_weight, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type bvac_rstructureeqmes0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect1DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactVal = ull.getInt(self.idx, path, cpopath + 'exact')
			check_status(status)
			status, weightList = ull.getVect1DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (nbslice))
			check_status(status)
			status, sigmaList = ull.getVect1DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (nbslice))
			check_status(status)
			status, calculatedList = ull.getVect1DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (nbslice))
			check_status(status)
			status, chi2List = ull.getVect1DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = bvac_rstructureeqmes0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[i].copy().astype(float)
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactVal
				slice.weight = weightList[i].copy().astype(float)
				slice.sigma = sigmaList[i].copy().astype(float)
				slice.calculated = calculatedList[i].copy().astype(float)
				slice.chi2 = chi2List[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bvac_rstructureeqmes0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'measured', i, self.measured)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'weight', i, self.weight)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sigma', i, self.sigma)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'calculated', i, self.calculated)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'chi2', i, self.chi2)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bvac_rstructureeqmes0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bvac_rstructureeqmes0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'exact', i, self.exact)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type bvac_rstructureeqmes0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class diamagfluxstructureeqmes0D:
	'''
	class diamagfluxstructureeqmes0D
	Diamagnetic flux [Wb], defined as integral (Btor - Btor,vac) dS where the integral is over the poloidal cross section of the plasma. It is measured by a single wire loop around the cross section of the torus (e.g. Wesson, Tokamaks, 1997, p.473). It gives information about the separation of the two source profiles p' and FF' of the Grad-Shafranov equation.

	Attributes:
	- measured : float
	   Measured value of the signal; Time-dependent; Scalar.
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal); String
	- time : float
	   Time (exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used); Time-dependent; Scalar.
	- exact : int
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; scalar integer
	- weight : float
	   weight given to the measurement (>= 0); Time-dependent; Scalar.
	- sigma : float
	   standard deviation of the measurement; Time-dependent; Scalar.
	- calculated : float
	   Signal as recalculated by the equilibrium code; Time-dependent; Scalar.
	- chi2 : float
	   chi^2 of (calculated-measured); Time-dependent; Scalar.
	'''

	def __init__(self, base_path_in='diamagflux'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = EMPTY_DOUBLE
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = EMPTY_INT
		self.weight = EMPTY_DOUBLE
		self.sigma = EMPTY_DOUBLE
		self.calculated = EMPTY_DOUBLE
		self.chi2 = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class diamagfluxstructureeqmes0D\n'
		ret = ret + space + 'Attribute measured: ' + str(self.measured) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		ret = ret + space + 'Attribute exact: ' + str(self.exact) + '\n'
		ret = ret + space + 'Attribute weight: ' + str(self.weight) + '\n'
		ret = ret + space + 'Attribute sigma: ' + str(self.sigma) + '\n'
		ret = ret + space + 'Attribute calculated: ' + str(self.calculated) + '\n'
		ret = ret + space + 'Attribute chi2: ' + str(self.chi2) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type diamagfluxstructureeqmes0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'measured', self.measured, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'weight', self.weight, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'sigma', self.sigma, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'calculated', self.calculated, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'chi2', self.chi2, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type diamagfluxstructureeqmes0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'measured', self.measured)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'weight', self.weight)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'sigma', self.sigma)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'calculated', self.calculated)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'chi2', self.chi2)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type diamagfluxstructureeqmes0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'exact', self.exact)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type diamagfluxstructureeqmes0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact = ull.getInt(self.idx, path, cpopath + 'exact')
		check_status(status)
		if not status:
			self.exact = ret_exact
		status, ret_weight, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type diamagfluxstructureeqmes0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect1DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactVal = ull.getInt(self.idx, path, cpopath + 'exact')
			check_status(status)
			status, weightList = ull.getVect1DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (nbslice))
			check_status(status)
			status, sigmaList = ull.getVect1DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (nbslice))
			check_status(status)
			status, calculatedList = ull.getVect1DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (nbslice))
			check_status(status)
			status, chi2List = ull.getVect1DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = diamagfluxstructureeqmes0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[i].copy().astype(float)
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactVal
				slice.weight = weightList[i].copy().astype(float)
				slice.sigma = sigmaList[i].copy().astype(float)
				slice.calculated = calculatedList[i].copy().astype(float)
				slice.chi2 = chi2List[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type diamagfluxstructureeqmes0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'measured', i, self.measured)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'weight', i, self.weight)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sigma', i, self.sigma)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'calculated', i, self.calculated)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'chi2', i, self.chi2)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type diamagfluxstructureeqmes0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type diamagfluxstructureeqmes0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'exact', i, self.exact)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type diamagfluxstructureeqmes0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class faradaystructureeqmes1D:
	'''
	class faradaystructureeqmes1D
	Faraday rotation angles [rad]

	Attributes:
	- measured : numpy.ndarray 1D with float
	   Measured value of the signal; Time-dependent; Array(nmeas)
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- time : float
	   Exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used);Time-dependent; Scalar
	- exact : numpy.ndarray 1D with int)
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; Time-dependent; Array(nmeas)
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); Time-dependent; Array(nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Array(nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Array(nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Array(nmeas)
	'''

	def __init__(self, base_path_in='faraday'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = numpy.zeros(0, numpy.int32, order='C')
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class faradaystructureeqmes1D\n'
		s = self.measured.__str__()
		ret = ret + space + 'Attribute measured\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		s = self.exact.__str__()
		ret = ret + space + 'Attribute exact\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type faradaystructureeqmes1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type faradaystructureeqmes1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type faradaystructureeqmes1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type faradaystructureeqmes1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'exact', inTime, interpolMode)
		check_status(status)
		if not status:
			self.exact = ret_exact
			self.cpoTime = retTime
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type faradaystructureeqmes1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect2DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactList = ull.getVect2DInt(self.idx, path, cpopath + 'exact')
			if len(exactList) == 0:
				exactList = numpy.resize(exactList, (0,nbslice))
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = faradaystructureeqmes1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[:,i]
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactList[:,i]
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type faradaystructureeqmes1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'measured', i, numpy.array(self.measured).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'exact', i, numpy.array(self.exact).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type faradaystructureeqmes1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type faradaystructureeqmes1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type faradaystructureeqmes1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class fluxstructureeqmes1D:
	'''
	class fluxstructureeqmes1D
	Poloidal flux loops [Wb]

	Attributes:
	- measured : numpy.ndarray 1D with float
	   Measured value of the signal; Time-dependent; Array(nmeas)
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- time : float
	   Exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used);Time-dependent; Scalar
	- exact : numpy.ndarray 1D with int)
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; Time-dependent; Array(nmeas)
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); Time-dependent; Array(nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Array(nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Array(nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Array(nmeas)
	'''

	def __init__(self, base_path_in='flux'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = numpy.zeros(0, numpy.int32, order='C')
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class fluxstructureeqmes1D\n'
		s = self.measured.__str__()
		ret = ret + space + 'Attribute measured\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		s = self.exact.__str__()
		ret = ret + space + 'Attribute exact\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructureeqmes1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructureeqmes1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructureeqmes1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructureeqmes1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'exact', inTime, interpolMode)
		check_status(status)
		if not status:
			self.exact = ret_exact
			self.cpoTime = retTime
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type fluxstructureeqmes1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect2DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactList = ull.getVect2DInt(self.idx, path, cpopath + 'exact')
			if len(exactList) == 0:
				exactList = numpy.resize(exactList, (0,nbslice))
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = fluxstructureeqmes1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[:,i]
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactList[:,i]
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstructureeqmes1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'measured', i, numpy.array(self.measured).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'exact', i, numpy.array(self.exact).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstructureeqmes1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstructureeqmes1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type fluxstructureeqmes1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class i_plasmastructureeqmes0D:
	'''
	class i_plasmastructureeqmes0D
	Plasma current [A];

	Attributes:
	- measured : float
	   Measured value of the signal; Time-dependent; Scalar.
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal); String
	- time : float
	   Time (exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used); Time-dependent; Scalar.
	- exact : int
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; scalar integer
	- weight : float
	   weight given to the measurement (>= 0); Time-dependent; Scalar.
	- sigma : float
	   standard deviation of the measurement; Time-dependent; Scalar.
	- calculated : float
	   Signal as recalculated by the equilibrium code; Time-dependent; Scalar.
	- chi2 : float
	   chi^2 of (calculated-measured); Time-dependent; Scalar.
	'''

	def __init__(self, base_path_in='i_plasma'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = EMPTY_DOUBLE
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = EMPTY_INT
		self.weight = EMPTY_DOUBLE
		self.sigma = EMPTY_DOUBLE
		self.calculated = EMPTY_DOUBLE
		self.chi2 = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class i_plasmastructureeqmes0D\n'
		ret = ret + space + 'Attribute measured: ' + str(self.measured) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		ret = ret + space + 'Attribute exact: ' + str(self.exact) + '\n'
		ret = ret + space + 'Attribute weight: ' + str(self.weight) + '\n'
		ret = ret + space + 'Attribute sigma: ' + str(self.sigma) + '\n'
		ret = ret + space + 'Attribute calculated: ' + str(self.calculated) + '\n'
		ret = ret + space + 'Attribute chi2: ' + str(self.chi2) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type i_plasmastructureeqmes0D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'measured', self.measured, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'weight', self.weight, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'sigma', self.sigma, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'calculated', self.calculated, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'chi2', self.chi2, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type i_plasmastructureeqmes0D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'measured', self.measured)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'weight', self.weight)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'sigma', self.sigma)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'calculated', self.calculated)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'chi2', self.chi2)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type i_plasmastructureeqmes0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'exact', self.exact)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type i_plasmastructureeqmes0D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact = ull.getInt(self.idx, path, cpopath + 'exact')
		check_status(status)
		if not status:
			self.exact = ret_exact
		status, ret_weight, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type i_plasmastructureeqmes0D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect1DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactVal = ull.getInt(self.idx, path, cpopath + 'exact')
			check_status(status)
			status, weightList = ull.getVect1DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (nbslice))
			check_status(status)
			status, sigmaList = ull.getVect1DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (nbslice))
			check_status(status)
			status, calculatedList = ull.getVect1DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (nbslice))
			check_status(status)
			status, chi2List = ull.getVect1DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = i_plasmastructureeqmes0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[i].copy().astype(float)
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactVal
				slice.weight = weightList[i].copy().astype(float)
				slice.sigma = sigmaList[i].copy().astype(float)
				slice.calculated = calculatedList[i].copy().astype(float)
				slice.chi2 = chi2List[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type i_plasmastructureeqmes0DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'measured', i, self.measured)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'weight', i, self.weight)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'sigma', i, self.sigma)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'calculated', i, self.calculated)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'chi2', i, self.chi2)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type i_plasmastructureeqmes0DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type i_plasmastructureeqmes0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'exact', i, self.exact)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type i_plasmastructureeqmes0DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class isofluxstructureisoflux:
	'''
	class isofluxstructureisoflux
	Point series at which the flux is considered the same

	Attributes:
	- position : class positionstructurerz1D
	   Position of the points at which the flux is considered the same; Time-dependent; Vector (nmeas)
	- source : str
	   Description or path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); -1 if exact data; Time-dependent; Vector (nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Vector (nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Vector (nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Vector (nmeas)
	'''

	def __init__(self, base_path_in='isoflux'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.position = positionstructurerz1D('position')
		self.source = ''
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class isofluxstructureisoflux\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.position.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type isofluxstructureisoflux, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.cpoTime = self.cpoTime
		self.position.putSlice(path, cpopath)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type isofluxstructureisoflux, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type isofluxstructureisoflux, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.putNonTimed(path, cpopath)
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type isofluxstructureisoflux, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type isofluxstructureisoflux, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			positionList = self.position.build_non_resampled_data(path, cpopath, nbslice)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = isofluxstructureisoflux(self.base_path)
				slice.setExpIdx(self.idx)
				slice.position = positionList[i]
				slice.source = sourceVal
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type isofluxstructureisofluxObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.position.putTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type isofluxstructureisofluxObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.position.getTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type isofluxstructureisofluxObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type isofluxstructureisofluxObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class positionstructurerz1D:
	'''
	class positionstructurerz1D
	Position of the points at which the flux is considered the same; Time-dependent; Vector (nmeas)

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]
	- z : numpy.ndarray 1D with float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='position'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class positionstructurerz1D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz1D, run function putSlice') 
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

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz1D, run function replaceLastSlice') 
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

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz1D, run function getSlice') 
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

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz1D, run function build_non_resampled_data') 
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
			for i in range(nbslice):
				slice = positionstructurerz1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[:,i]
				slice.z = zList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz1DObj, run function putTimedElt') 
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

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz1DObj, run function getTimedElt') 
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

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz1DObj, run function getNonTimedElt') 
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


class jsurfstructureeqmes1D:
	'''
	class jsurfstructureeqmes1D
	Average of current density on the flux surface [A/m^2]

	Attributes:
	- measured : numpy.ndarray 1D with float
	   Measured value of the signal; Time-dependent; Array(nmeas)
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- time : float
	   Exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used);Time-dependent; Scalar
	- exact : numpy.ndarray 1D with int)
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; Time-dependent; Array(nmeas)
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); Time-dependent; Array(nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Array(nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Array(nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Array(nmeas)
	'''

	def __init__(self, base_path_in='jsurf'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = numpy.zeros(0, numpy.int32, order='C')
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class jsurfstructureeqmes1D\n'
		s = self.measured.__str__()
		ret = ret + space + 'Attribute measured\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		s = self.exact.__str__()
		ret = ret + space + 'Attribute exact\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jsurfstructureeqmes1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jsurfstructureeqmes1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type jsurfstructureeqmes1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type jsurfstructureeqmes1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'exact', inTime, interpolMode)
		check_status(status)
		if not status:
			self.exact = ret_exact
			self.cpoTime = retTime
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type jsurfstructureeqmes1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect2DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactList = ull.getVect2DInt(self.idx, path, cpopath + 'exact')
			if len(exactList) == 0:
				exactList = numpy.resize(exactList, (0,nbslice))
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = jsurfstructureeqmes1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[:,i]
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactList[:,i]
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jsurfstructureeqmes1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'measured', i, numpy.array(self.measured).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'exact', i, numpy.array(self.exact).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jsurfstructureeqmes1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jsurfstructureeqmes1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type jsurfstructureeqmes1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class magnet_ironstructuremagnet_iron:
	'''
	class magnet_ironstructuremagnet_iron
	Magnetisation in iron segments [T]

	Attributes:
	- mr : class mrstructureeqmes1D
	   Magnetisation along the R axis [T];
	- mz : class mzstructureeqmes1D
	   Magnetisation along the Z axis [T];
	'''

	def __init__(self, base_path_in='magnet_iron'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.mr = mrstructureeqmes1D('mr')
		self.mz = mzstructureeqmes1D('mz')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class magnet_ironstructuremagnet_iron\n'
		ret = ret + space + 'Attribute mr\n ' + self.mr.__str__(depth+1)
		ret = ret + space + 'Attribute mz\n ' + self.mz.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.mr.setExpIdx(idx)
		self.mz.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type magnet_ironstructuremagnet_iron, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mr.cpoTime = self.cpoTime
		self.mr.putSlice(path, cpopath)
		self.mz.cpoTime = self.cpoTime
		self.mz.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type magnet_ironstructuremagnet_iron, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mr.replaceLastSlice(path, cpopath)
		self.mz.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type magnet_ironstructuremagnet_iron, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mr.putNonTimed(path, cpopath)
		self.mz.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type magnet_ironstructuremagnet_iron, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mr.getSlice(path, cpopath, inTime, interpolMode)
		self.mz.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type magnet_ironstructuremagnet_iron, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			mrList = self.mr.build_non_resampled_data(path, cpopath, nbslice)
			mzList = self.mz.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = magnet_ironstructuremagnet_iron(self.base_path)
				slice.setExpIdx(self.idx)
				slice.mr = mrList[i]
				slice.mz = mzList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type magnet_ironstructuremagnet_ironObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.mr.putTimedElt(path, cpopath + 'mr', i, obj)
		obj = self.mz.putTimedElt(path, cpopath + 'mz', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type magnet_ironstructuremagnet_ironObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.mr.getTimedElt(path, cpopath + 'mr', i, obj)
		self.mz.getTimedElt(path, cpopath + 'mz', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type magnet_ironstructuremagnet_ironObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.mr.putNonTimedElt(path, cpopath + 'mr', i, obj)
		obj = self.mz.putNonTimedElt(path, cpopath + 'mz', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type magnet_ironstructuremagnet_ironObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.mr.getNonTimedElt(path, cpopath + 'mr', i, obj)
		self.mz.getNonTimedElt(path, cpopath + 'mz', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.mr.deleteData(path, cpopath)
		self.mz.deleteData(path, cpopath)


class mrstructureeqmes1D:
	'''
	class mrstructureeqmes1D
	Magnetisation along the R axis [T];

	Attributes:
	- measured : numpy.ndarray 1D with float
	   Measured value of the signal; Time-dependent; Array(nmeas)
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- time : float
	   Exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used);Time-dependent; Scalar
	- exact : numpy.ndarray 1D with int)
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; Time-dependent; Array(nmeas)
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); Time-dependent; Array(nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Array(nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Array(nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Array(nmeas)
	'''

	def __init__(self, base_path_in='mr'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = numpy.zeros(0, numpy.int32, order='C')
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mrstructureeqmes1D\n'
		s = self.measured.__str__()
		ret = ret + space + 'Attribute measured\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		s = self.exact.__str__()
		ret = ret + space + 'Attribute exact\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mrstructureeqmes1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mrstructureeqmes1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mrstructureeqmes1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mrstructureeqmes1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'exact', inTime, interpolMode)
		check_status(status)
		if not status:
			self.exact = ret_exact
			self.cpoTime = retTime
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mrstructureeqmes1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect2DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactList = ull.getVect2DInt(self.idx, path, cpopath + 'exact')
			if len(exactList) == 0:
				exactList = numpy.resize(exactList, (0,nbslice))
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = mrstructureeqmes1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[:,i]
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactList[:,i]
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mrstructureeqmes1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'measured', i, numpy.array(self.measured).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'exact', i, numpy.array(self.exact).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mrstructureeqmes1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mrstructureeqmes1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mrstructureeqmes1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class mzstructureeqmes1D:
	'''
	class mzstructureeqmes1D
	Magnetisation along the Z axis [T];

	Attributes:
	- measured : numpy.ndarray 1D with float
	   Measured value of the signal; Time-dependent; Array(nmeas)
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- time : float
	   Exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used);Time-dependent; Scalar
	- exact : numpy.ndarray 1D with int)
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; Time-dependent; Array(nmeas)
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); Time-dependent; Array(nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Array(nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Array(nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Array(nmeas)
	'''

	def __init__(self, base_path_in='mz'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = numpy.zeros(0, numpy.int32, order='C')
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mzstructureeqmes1D\n'
		s = self.measured.__str__()
		ret = ret + space + 'Attribute measured\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		s = self.exact.__str__()
		ret = ret + space + 'Attribute exact\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mzstructureeqmes1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mzstructureeqmes1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mzstructureeqmes1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mzstructureeqmes1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'exact', inTime, interpolMode)
		check_status(status)
		if not status:
			self.exact = ret_exact
			self.cpoTime = retTime
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mzstructureeqmes1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect2DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactList = ull.getVect2DInt(self.idx, path, cpopath + 'exact')
			if len(exactList) == 0:
				exactList = numpy.resize(exactList, (0,nbslice))
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = mzstructureeqmes1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[:,i]
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactList[:,i]
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mzstructureeqmes1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'measured', i, numpy.array(self.measured).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'exact', i, numpy.array(self.exact).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mzstructureeqmes1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mzstructureeqmes1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mzstructureeqmes1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class msestructureeqmes1D:
	'''
	class msestructureeqmes1D
	MSE angles [rad]

	Attributes:
	- measured : numpy.ndarray 1D with float
	   Measured value of the signal; Time-dependent; Array(nmeas)
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- time : float
	   Exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used);Time-dependent; Scalar
	- exact : numpy.ndarray 1D with int)
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; Time-dependent; Array(nmeas)
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); Time-dependent; Array(nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Array(nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Array(nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Array(nmeas)
	'''

	def __init__(self, base_path_in='mse'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = numpy.zeros(0, numpy.int32, order='C')
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class msestructureeqmes1D\n'
		s = self.measured.__str__()
		ret = ret + space + 'Attribute measured\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		s = self.exact.__str__()
		ret = ret + space + 'Attribute exact\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type msestructureeqmes1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type msestructureeqmes1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type msestructureeqmes1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type msestructureeqmes1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'exact', inTime, interpolMode)
		check_status(status)
		if not status:
			self.exact = ret_exact
			self.cpoTime = retTime
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type msestructureeqmes1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect2DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactList = ull.getVect2DInt(self.idx, path, cpopath + 'exact')
			if len(exactList) == 0:
				exactList = numpy.resize(exactList, (0,nbslice))
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = msestructureeqmes1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[:,i]
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactList[:,i]
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type msestructureeqmes1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'measured', i, numpy.array(self.measured).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'exact', i, numpy.array(self.exact).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type msestructureeqmes1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type msestructureeqmes1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type msestructureeqmes1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class nestructureeqmes1D:
	'''
	class nestructureeqmes1D
	Electron density [m^-3 for local measurement, m^-2 if line integrated]

	Attributes:
	- measured : numpy.ndarray 1D with float
	   Measured value of the signal; Time-dependent; Array(nmeas)
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- time : float
	   Exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used);Time-dependent; Scalar
	- exact : numpy.ndarray 1D with int)
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; Time-dependent; Array(nmeas)
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); Time-dependent; Array(nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Array(nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Array(nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Array(nmeas)
	'''

	def __init__(self, base_path_in='ne'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = numpy.zeros(0, numpy.int32, order='C')
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class nestructureeqmes1D\n'
		s = self.measured.__str__()
		ret = ret + space + 'Attribute measured\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		s = self.exact.__str__()
		ret = ret + space + 'Attribute exact\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nestructureeqmes1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nestructureeqmes1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type nestructureeqmes1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type nestructureeqmes1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'exact', inTime, interpolMode)
		check_status(status)
		if not status:
			self.exact = ret_exact
			self.cpoTime = retTime
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type nestructureeqmes1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect2DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactList = ull.getVect2DInt(self.idx, path, cpopath + 'exact')
			if len(exactList) == 0:
				exactList = numpy.resize(exactList, (0,nbslice))
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = nestructureeqmes1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[:,i]
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactList[:,i]
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nestructureeqmes1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'measured', i, numpy.array(self.measured).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'exact', i, numpy.array(self.exact).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nestructureeqmes1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nestructureeqmes1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type nestructureeqmes1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class pfcurrentstructureeqmes1D:
	'''
	class pfcurrentstructureeqmes1D
	Current in poloidal field coils [A]

	Attributes:
	- measured : numpy.ndarray 1D with float
	   Measured value of the signal; Time-dependent; Array(nmeas)
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- time : float
	   Exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used);Time-dependent; Scalar
	- exact : numpy.ndarray 1D with int)
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; Time-dependent; Array(nmeas)
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); Time-dependent; Array(nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Array(nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Array(nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Array(nmeas)
	'''

	def __init__(self, base_path_in='pfcurrent'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = numpy.zeros(0, numpy.int32, order='C')
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pfcurrentstructureeqmes1D\n'
		s = self.measured.__str__()
		ret = ret + space + 'Attribute measured\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		s = self.exact.__str__()
		ret = ret + space + 'Attribute exact\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfcurrentstructureeqmes1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfcurrentstructureeqmes1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pfcurrentstructureeqmes1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pfcurrentstructureeqmes1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'exact', inTime, interpolMode)
		check_status(status)
		if not status:
			self.exact = ret_exact
			self.cpoTime = retTime
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pfcurrentstructureeqmes1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect2DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactList = ull.getVect2DInt(self.idx, path, cpopath + 'exact')
			if len(exactList) == 0:
				exactList = numpy.resize(exactList, (0,nbslice))
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = pfcurrentstructureeqmes1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[:,i]
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactList[:,i]
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfcurrentstructureeqmes1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'measured', i, numpy.array(self.measured).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'exact', i, numpy.array(self.exact).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfcurrentstructureeqmes1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfcurrentstructureeqmes1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pfcurrentstructureeqmes1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class pressurestructureeqmes1D:
	'''
	class pressurestructureeqmes1D
	Total pressure [Pa]

	Attributes:
	- measured : numpy.ndarray 1D with float
	   Measured value of the signal; Time-dependent; Array(nmeas)
	- source : str
	   Path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- time : float
	   Exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source signal, it means linear interpolation has been used);Time-dependent; Scalar
	- exact : numpy.ndarray 1D with int)
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; Time-dependent; Array(nmeas)
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); Time-dependent; Array(nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Array(nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Array(nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Array(nmeas)
	'''

	def __init__(self, base_path_in='pressure'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.measured = numpy.zeros(0, numpy.float64, order='C')
		self.source = ''
		self.time = EMPTY_DOUBLE
		self.exact = numpy.zeros(0, numpy.int32, order='C')
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class pressurestructureeqmes1D\n'
		s = self.measured.__str__()
		ret = ret + space + 'Attribute measured\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute time: ' + str(self.time) + '\n'
		s = self.exact.__str__()
		ret = ret + space + 'Attribute exact\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pressurestructureeqmes1D, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'time', self.time, self.cpoTime)
		check_status(status)
		status = ull.putVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pressurestructureeqmes1D, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'measured', numpy.array(self.measured).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'time', self.time)
		check_status(status)
		status = ull.replaceLastVect1DIntSlice(self.idx, path, cpopath + 'exact', numpy.array(self.exact).astype(numpy.int32))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type pressurestructureeqmes1D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type pressurestructureeqmes1D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_measured, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'measured', inTime, interpolMode)
		check_status(status)
		if not status:
			self.measured = ret_measured
			self.cpoTime = retTime
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_time, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'time', inTime, interpolMode)
		check_status(status)
		if not status:
			self.time = ret_time
			self.cpoTime = retTime
		status, ret_exact, retTime = ull.getVect1DIntSlice(self.idx, path, cpopath + 'exact', inTime, interpolMode)
		check_status(status)
		if not status:
			self.exact = ret_exact
			self.cpoTime = retTime
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type pressurestructureeqmes1D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, measuredList = ull.getVect2DDouble(self.idx, path, cpopath + 'measured')
			if len(measuredList) == 0:
				measuredList = numpy.resize(measuredList, (0,nbslice))
			check_status(status)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, timeList = ull.getVect1DDouble(self.idx, path, cpopath + 'time')
			if len(timeList) == 0:
				timeList = numpy.resize(timeList, (nbslice))
			check_status(status)
			status, exactList = ull.getVect2DInt(self.idx, path, cpopath + 'exact')
			if len(exactList) == 0:
				exactList = numpy.resize(exactList, (0,nbslice))
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = pressurestructureeqmes1D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.measured = measuredList[:,i]
				slice.source = sourceVal
				slice.time = timeList[i].copy().astype(float)
				slice.exact = exactList[:,i]
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pressurestructureeqmes1DObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'measured', i, numpy.array(self.measured).astype(numpy.float64))
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'time', i, self.time)
		if (dev()):
			print ('putVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DIntInObject(self.idx, obj, cpopath + 'exact', i, numpy.array(self.exact).astype(numpy.int32))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pressurestructureeqmes1DObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'measured') 
			print ('obj = ' + str(obj))
		status, ret_measured = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'measured', i)
		check_status(status)
		if not status:
			self.measured = ret_measured
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'time') 
			print ('obj = ' + str(obj))
		status, ret_time = ull.getDoubleFromObject(self.idx, obj, cpopath + 'time', i)
		check_status(status)
		if not status:
			self.time = ret_time
		if (dev()):
			print ('getVect1DIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getVect1DIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pressurestructureeqmes1DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type pressurestructureeqmes1DObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'measured')
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'time')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class qstructureq:
	'''
	class qstructureq
	Safety factor

	Attributes:
	- qvalue : numpy.ndarray 1D with float
	   Safety factor values; Time-dependent; Vector (nmeas)
	- position : class positionstructurerz1D
	   Major radius of the given safety factor values [m]; Time-dependent; Vector (nmeas)
	- source : str
	   Description or path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- exact : int
	   1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; scalar integer
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); Time-dependent; Vector (nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Vector (nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Vector (nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Vector (nmeas)
	'''

	def __init__(self, base_path_in='q'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.qvalue = numpy.zeros(0, numpy.float64, order='C')
		self.position = positionstructurerz1D('position')
		self.source = ''
		self.exact = EMPTY_INT
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class qstructureq\n'
		s = self.qvalue.__str__()
		ret = ret + space + 'Attribute qvalue\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute exact: ' + str(self.exact) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.position.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type qstructureq, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'qvalue', numpy.array(self.qvalue).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.position.cpoTime = self.cpoTime
		self.position.putSlice(path, cpopath)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type qstructureq, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'qvalue', numpy.array(self.qvalue).astype(numpy.float64))
		check_status(status)
		self.position.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type qstructureq, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.putNonTimed(path, cpopath)
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)
		status = ull.putInt(self.idx, path, cpopath + 'exact', self.exact)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type qstructureq, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_qvalue, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'qvalue', inTime, interpolMode)
		check_status(status)
		if not status:
			self.qvalue = ret_qvalue
			self.cpoTime = retTime
		self.position.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_exact = ull.getInt(self.idx, path, cpopath + 'exact')
		check_status(status)
		if not status:
			self.exact = ret_exact
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type qstructureq, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, qvalueList = ull.getVect2DDouble(self.idx, path, cpopath + 'qvalue')
			if len(qvalueList) == 0:
				qvalueList = numpy.resize(qvalueList, (0,nbslice))
			check_status(status)
			positionList = self.position.build_non_resampled_data(path, cpopath, nbslice)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, exactVal = ull.getInt(self.idx, path, cpopath + 'exact')
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = qstructureq(self.base_path)
				slice.setExpIdx(self.idx)
				slice.qvalue = qvalueList[:,i]
				slice.position = positionList[i]
				slice.source = sourceVal
				slice.exact = exactVal
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qstructureqObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'qvalue') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'qvalue', i, numpy.array(self.qvalue).astype(numpy.float64))
		obj = self.position.putTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qstructureqObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'qvalue') 
			print ('obj = ' + str(obj))
		status, ret_qvalue = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'qvalue', i)
		check_status(status)
		if not status:
			self.qvalue = ret_qvalue
		self.position.getTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qstructureqObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'exact', i, self.exact)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type qstructureqObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'exact') 
			print ('obj = ' + str(obj))
		status, ret_exact = ull.getIntFromObject(self.idx, obj, cpopath + 'exact', i)
		check_status(status)
		if not status:
			self.exact = ret_exact

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'qvalue')
		self.position.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'exact')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class xptsstructurexpts:
	'''
	class xptsstructurexpts
	Position of the X-point(s)

	Attributes:
	- position : class positionstructurerz1D
	   Position of the X-point(s); Time-dependent; Vector (nmeas)
	- source : str
	   Description or path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/value'. String
	- weight : numpy.ndarray 1D with float
	   weight given to the measurement (>= 0); -1 if exact data; Time-dependent; Vector (nmeas)
	- sigma : numpy.ndarray 1D with float
	   standard deviation of the measurement; Time-dependent; Vector (nmeas)
	- calculated : numpy.ndarray 1D with float
	   Signal as recalculated by the equilibrium code; Time-dependent; Vector (nmeas)
	- chi2 : numpy.ndarray 1D with float
	   chi^2 of (calculated-measured); Time-dependent; Vector (nmeas)
	'''

	def __init__(self, base_path_in='xpts'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.position = positionstructurerz1D('position')
		self.source = ''
		self.weight = numpy.zeros(0, numpy.float64, order='C')
		self.sigma = numpy.zeros(0, numpy.float64, order='C')
		self.calculated = numpy.zeros(0, numpy.float64, order='C')
		self.chi2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class xptsstructurexpts\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		s = self.weight.__str__()
		ret = ret + space + 'Attribute weight\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.sigma.__str__()
		ret = ret + space + 'Attribute sigma\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.calculated.__str__()
		ret = ret + space + 'Attribute calculated\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.chi2.__str__()
		ret = ret + space + 'Attribute chi2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.position.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type xptsstructurexpts, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.cpoTime = self.cpoTime
		self.position.putSlice(path, cpopath)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type xptsstructurexpts, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'weight', numpy.array(self.weight).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', numpy.array(self.sigma).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', numpy.array(self.calculated).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', numpy.array(self.chi2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type xptsstructurexpts, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.putNonTimed(path, cpopath)
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type xptsstructurexpts, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_weight, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'weight', inTime, interpolMode)
		check_status(status)
		if not status:
			self.weight = ret_weight
			self.cpoTime = retTime
		status, ret_sigma, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'sigma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
			self.cpoTime = retTime
		status, ret_calculated, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'calculated', inTime, interpolMode)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
			self.cpoTime = retTime
		status, ret_chi2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'chi2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type xptsstructurexpts, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			positionList = self.position.build_non_resampled_data(path, cpopath, nbslice)
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, weightList = ull.getVect2DDouble(self.idx, path, cpopath + 'weight')
			if len(weightList) == 0:
				weightList = numpy.resize(weightList, (0,nbslice))
			check_status(status)
			status, sigmaList = ull.getVect2DDouble(self.idx, path, cpopath + 'sigma')
			if len(sigmaList) == 0:
				sigmaList = numpy.resize(sigmaList, (0,nbslice))
			check_status(status)
			status, calculatedList = ull.getVect2DDouble(self.idx, path, cpopath + 'calculated')
			if len(calculatedList) == 0:
				calculatedList = numpy.resize(calculatedList, (0,nbslice))
			check_status(status)
			status, chi2List = ull.getVect2DDouble(self.idx, path, cpopath + 'chi2')
			if len(chi2List) == 0:
				chi2List = numpy.resize(chi2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = xptsstructurexpts(self.base_path)
				slice.setExpIdx(self.idx)
				slice.position = positionList[i]
				slice.source = sourceVal
				slice.weight = weightList[:,i]
				slice.sigma = sigmaList[:,i]
				slice.calculated = calculatedList[:,i]
				slice.chi2 = chi2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type xptsstructurexptsObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.position.putTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'weight', i, numpy.array(self.weight).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'sigma', i, numpy.array(self.sigma).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'calculated', i, numpy.array(self.calculated).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'chi2', i, numpy.array(self.chi2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type xptsstructurexptsObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.position.getTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'weight') 
			print ('obj = ' + str(obj))
		status, ret_weight = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'weight', i)
		check_status(status)
		if not status:
			self.weight = ret_weight
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'sigma') 
			print ('obj = ' + str(obj))
		status, ret_sigma = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'sigma', i)
		check_status(status)
		if not status:
			self.sigma = ret_sigma
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'calculated') 
			print ('obj = ' + str(obj))
		status, ret_calculated = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'calculated', i)
		check_status(status)
		if not status:
			self.calculated = ret_calculated
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'chi2') 
			print ('obj = ' + str(obj))
		status, ret_chi2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'chi2', i)
		check_status(status)
		if not status:
			self.chi2 = ret_chi2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type xptsstructurexptsObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type xptsstructurexptsObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'weight')
		ull.deleteData(self.idx, path, cpopath + 'sigma')
		ull.deleteData(self.idx, path, cpopath + 'calculated')
		ull.deleteData(self.idx, path, cpopath + 'chi2')


class eqgeometrystructureeqgeometry:
	'''
	class eqgeometrystructureeqgeometry
	Geometry of the plasma boundary

	Attributes:
	- source : str
	   Comment describing the origin of the eqgeometry data; String
	- boundarytype : int
	   0 (limiter) or 1 (separatrix); Integer; Time-dependent
	- boundary : class boundarystruct_arrayrz1Dexp: array of boundarystruct_arrayrz1DexpObj objects
	   RZ description of the plasma boundary; This is formally declared as an array of structure to allow for time-dependent size of the R and Z vectors in the sub-structure below. However, boundary must be allocated to size 1. Time-dependent;
	- geom_axis : class geom_axisstructurerz0D
	   RZ position of the geometric axis (defined as (Rmin+Rmax) / 2 and (Zmin+Zmax) / 2 of the boundary) [m]; Time-dependent; Scalar
	- a_minor : float
	   Minor radius of the plasma boundary [m]; Time-dependent; Scalar
	- elongation : float
	   Elongation of the plasma boundary; Time-dependent; Scalar
	- elong_upper : float
	   Elongation upper of the plasma boundary; Time-dependent; Scalar
	- elong_lower : float
	   Elongation lower of the plasma boundary; Time-dependent; Scalar
	- tria_upper : float
	   Upper triangularity of the plasma boundary; Time-dependent; Scalar
	- tria_lower : float
	   Lower triangularity of the plasma boundary; Time-dependent; Scalar
	- xpts : class xptsstruct_arrayrz1Dexp: array of xptsstruct_arrayrz1DexpObj objects
	   Position of the Xpoints, first is the active xpoint if diverted [m]; This is formally declared as an array of structure to allow for time-dependent size of the R and Z vectors in the sub-structure below. However, xpts must be allocated to size 1. Time-dependent;
	- left_low_st : class left_low_ststructurerz0D
	   Position of the lower left strike point [m]; Time-dependent; Scalar
	- right_low_st : class right_low_ststructurerz0D
	   Position of the lower right strike point [m]; Time-dependent; Scalar
	- left_up_st : class left_up_ststructurerz0D
	   Position of the upper left strike point [m]; Time-dependent; Scalar
	- right_up_st : class right_up_ststructurerz0D
	   Position of the upper right strike point [m]; Time-dependent; Scalar
	- active_limit : class active_limitstructurerz0D
	   Position of the active limiter point (point of the plasma boundary in contact with the limiter) [m]; Set R = 0 for X-point plasma; Time-dependent; Scalar
	- ang_lcms_upo : float
	   Angle at the LMCS X point upper outer; Time-dependent; Scalar
	- ang_lcms_upi : float
	   Angle at the LMCS X point upper inner; Time-dependent; Scalar
	- ang_lcms_lwo : float
	   Angle at the LMCS X point lower outer; Time-dependent; Scalar 
	- ang_lcms_lwi : float
	   Angle at the LMCS X point lower inner; Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='eqgeometry'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.source = ''
		self.boundarytype = EMPTY_INT
		self.boundary = boundarystruct_arrayrz1Dexp('boundary')
		self.geom_axis = geom_axisstructurerz0D('geom_axis')
		self.a_minor = EMPTY_DOUBLE
		self.elongation = EMPTY_DOUBLE
		self.elong_upper = EMPTY_DOUBLE
		self.elong_lower = EMPTY_DOUBLE
		self.tria_upper = EMPTY_DOUBLE
		self.tria_lower = EMPTY_DOUBLE
		self.xpts = xptsstruct_arrayrz1Dexp('xpts')
		self.left_low_st = left_low_ststructurerz0D('left_low_st')
		self.right_low_st = right_low_ststructurerz0D('right_low_st')
		self.left_up_st = left_up_ststructurerz0D('left_up_st')
		self.right_up_st = right_up_ststructurerz0D('right_up_st')
		self.active_limit = active_limitstructurerz0D('active_limit')
		self.ang_lcms_upo = EMPTY_DOUBLE
		self.ang_lcms_upi = EMPTY_DOUBLE
		self.ang_lcms_lwo = EMPTY_DOUBLE
		self.ang_lcms_lwi = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class eqgeometrystructureeqgeometry\n'
		ret = ret + space + 'Attribute source: ' + str(self.source) + '\n'
		ret = ret + space + 'Attribute boundarytype: ' + str(self.boundarytype) + '\n'
		ret = ret + space + 'Attribute boundary\n ' + self.boundary.__str__(depth+1)
		ret = ret + space + 'Attribute geom_axis\n ' + self.geom_axis.__str__(depth+1)
		ret = ret + space + 'Attribute a_minor: ' + str(self.a_minor) + '\n'
		ret = ret + space + 'Attribute elongation: ' + str(self.elongation) + '\n'
		ret = ret + space + 'Attribute elong_upper: ' + str(self.elong_upper) + '\n'
		ret = ret + space + 'Attribute elong_lower: ' + str(self.elong_lower) + '\n'
		ret = ret + space + 'Attribute tria_upper: ' + str(self.tria_upper) + '\n'
		ret = ret + space + 'Attribute tria_lower: ' + str(self.tria_lower) + '\n'
		ret = ret + space + 'Attribute xpts\n ' + self.xpts.__str__(depth+1)
		ret = ret + space + 'Attribute left_low_st\n ' + self.left_low_st.__str__(depth+1)
		ret = ret + space + 'Attribute right_low_st\n ' + self.right_low_st.__str__(depth+1)
		ret = ret + space + 'Attribute left_up_st\n ' + self.left_up_st.__str__(depth+1)
		ret = ret + space + 'Attribute right_up_st\n ' + self.right_up_st.__str__(depth+1)
		ret = ret + space + 'Attribute active_limit\n ' + self.active_limit.__str__(depth+1)
		ret = ret + space + 'Attribute ang_lcms_upo: ' + str(self.ang_lcms_upo) + '\n'
		ret = ret + space + 'Attribute ang_lcms_upi: ' + str(self.ang_lcms_upi) + '\n'
		ret = ret + space + 'Attribute ang_lcms_lwo: ' + str(self.ang_lcms_lwo) + '\n'
		ret = ret + space + 'Attribute ang_lcms_lwi: ' + str(self.ang_lcms_lwi) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.boundary.setExpIdx(idx)
		self.geom_axis.setExpIdx(idx)
		self.xpts.setExpIdx(idx)
		self.left_low_st.setExpIdx(idx)
		self.right_low_st.setExpIdx(idx)
		self.left_up_st.setExpIdx(idx)
		self.right_up_st.setExpIdx(idx)
		self.active_limit.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type eqgeometrystructureeqgeometry, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putIntSlice(self.idx, path, cpopath + 'boundarytype', self.boundarytype, self.cpoTime)
		check_status(status)
		self.boundary.cpoTime = self.cpoTime
		self.boundary.putSlice(path, cpopath)
		self.geom_axis.cpoTime = self.cpoTime
		self.geom_axis.putSlice(path, cpopath)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'a_minor', self.a_minor, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'elongation', self.elongation, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'elong_upper', self.elong_upper, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'elong_lower', self.elong_lower, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'tria_upper', self.tria_upper, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'tria_lower', self.tria_lower, self.cpoTime)
		check_status(status)
		self.xpts.cpoTime = self.cpoTime
		self.xpts.putSlice(path, cpopath)
		self.left_low_st.cpoTime = self.cpoTime
		self.left_low_st.putSlice(path, cpopath)
		self.right_low_st.cpoTime = self.cpoTime
		self.right_low_st.putSlice(path, cpopath)
		self.left_up_st.cpoTime = self.cpoTime
		self.left_up_st.putSlice(path, cpopath)
		self.right_up_st.cpoTime = self.cpoTime
		self.right_up_st.putSlice(path, cpopath)
		self.active_limit.cpoTime = self.cpoTime
		self.active_limit.putSlice(path, cpopath)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'ang_lcms_upo', self.ang_lcms_upo, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'ang_lcms_upi', self.ang_lcms_upi, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'ang_lcms_lwo', self.ang_lcms_lwo, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'ang_lcms_lwi', self.ang_lcms_lwi, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type eqgeometrystructureeqgeometry, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastIntSlice(self.idx, path, cpopath + 'boundarytype', self.boundarytype)
		check_status(status)
		self.boundary.replaceLastSlice(path, cpopath)
		self.geom_axis.replaceLastSlice(path, cpopath)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'a_minor', self.a_minor)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'elongation', self.elongation)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'elong_upper', self.elong_upper)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'elong_lower', self.elong_lower)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'tria_upper', self.tria_upper)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'tria_lower', self.tria_lower)
		check_status(status)
		self.xpts.replaceLastSlice(path, cpopath)
		self.left_low_st.replaceLastSlice(path, cpopath)
		self.right_low_st.replaceLastSlice(path, cpopath)
		self.left_up_st.replaceLastSlice(path, cpopath)
		self.right_up_st.replaceLastSlice(path, cpopath)
		self.active_limit.replaceLastSlice(path, cpopath)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'ang_lcms_upo', self.ang_lcms_upo)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'ang_lcms_upi', self.ang_lcms_upi)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'ang_lcms_lwo', self.ang_lcms_lwo)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'ang_lcms_lwi', self.ang_lcms_lwi)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type eqgeometrystructureeqgeometry, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'source', self.source)
		check_status(status)
		self.boundary.putNonTimed(path, cpopath)
		self.geom_axis.putNonTimed(path, cpopath)
		self.xpts.putNonTimed(path, cpopath)
		self.left_low_st.putNonTimed(path, cpopath)
		self.right_low_st.putNonTimed(path, cpopath)
		self.left_up_st.putNonTimed(path, cpopath)
		self.right_up_st.putNonTimed(path, cpopath)
		self.active_limit.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type eqgeometrystructureeqgeometry, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_source = ull.getString(self.idx, path, cpopath + 'source')
		check_status(status)
		if not status:
			self.source = ret_source
		status, ret_boundarytype, retTime = ull.getIntSlice(self.idx, path, cpopath + 'boundarytype', inTime, interpolMode)
		check_status(status)
		if not status:
			self.boundarytype = ret_boundarytype
			self.cpoTime = retTime
		self.boundary.getSlice(path, cpopath, inTime, interpolMode)
		self.geom_axis.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_a_minor, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'a_minor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.a_minor = ret_a_minor
			self.cpoTime = retTime
		status, ret_elongation, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'elongation', inTime, interpolMode)
		check_status(status)
		if not status:
			self.elongation = ret_elongation
			self.cpoTime = retTime
		status, ret_elong_upper, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'elong_upper', inTime, interpolMode)
		check_status(status)
		if not status:
			self.elong_upper = ret_elong_upper
			self.cpoTime = retTime
		status, ret_elong_lower, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'elong_lower', inTime, interpolMode)
		check_status(status)
		if not status:
			self.elong_lower = ret_elong_lower
			self.cpoTime = retTime
		status, ret_tria_upper, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'tria_upper', inTime, interpolMode)
		check_status(status)
		if not status:
			self.tria_upper = ret_tria_upper
			self.cpoTime = retTime
		status, ret_tria_lower, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'tria_lower', inTime, interpolMode)
		check_status(status)
		if not status:
			self.tria_lower = ret_tria_lower
			self.cpoTime = retTime
		self.xpts.getSlice(path, cpopath, inTime, interpolMode)
		self.left_low_st.getSlice(path, cpopath, inTime, interpolMode)
		self.right_low_st.getSlice(path, cpopath, inTime, interpolMode)
		self.left_up_st.getSlice(path, cpopath, inTime, interpolMode)
		self.right_up_st.getSlice(path, cpopath, inTime, interpolMode)
		self.active_limit.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_ang_lcms_upo, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'ang_lcms_upo', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ang_lcms_upo = ret_ang_lcms_upo
			self.cpoTime = retTime
		status, ret_ang_lcms_upi, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'ang_lcms_upi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ang_lcms_upi = ret_ang_lcms_upi
			self.cpoTime = retTime
		status, ret_ang_lcms_lwo, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'ang_lcms_lwo', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ang_lcms_lwo = ret_ang_lcms_lwo
			self.cpoTime = retTime
		status, ret_ang_lcms_lwi, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'ang_lcms_lwi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ang_lcms_lwi = ret_ang_lcms_lwi
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type eqgeometrystructureeqgeometry, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, sourceVal = ull.getString(self.idx, path, cpopath + 'source')
			check_status(status)
			status, boundarytypeList = ull.getVect1DInt(self.idx, path, cpopath + 'boundarytype')
			if len(boundarytypeList) == 0:
				boundarytypeList = numpy.resize(boundarytypeList, (nbslice))
			check_status(status)
			boundaryList = self.boundary.build_non_resampled_data(path, cpopath, nbslice)
			geom_axisList = self.geom_axis.build_non_resampled_data(path, cpopath, nbslice)
			status, a_minorList = ull.getVect1DDouble(self.idx, path, cpopath + 'a_minor')
			if len(a_minorList) == 0:
				a_minorList = numpy.resize(a_minorList, (nbslice))
			check_status(status)
			status, elongationList = ull.getVect1DDouble(self.idx, path, cpopath + 'elongation')
			if len(elongationList) == 0:
				elongationList = numpy.resize(elongationList, (nbslice))
			check_status(status)
			status, elong_upperList = ull.getVect1DDouble(self.idx, path, cpopath + 'elong_upper')
			if len(elong_upperList) == 0:
				elong_upperList = numpy.resize(elong_upperList, (nbslice))
			check_status(status)
			status, elong_lowerList = ull.getVect1DDouble(self.idx, path, cpopath + 'elong_lower')
			if len(elong_lowerList) == 0:
				elong_lowerList = numpy.resize(elong_lowerList, (nbslice))
			check_status(status)
			status, tria_upperList = ull.getVect1DDouble(self.idx, path, cpopath + 'tria_upper')
			if len(tria_upperList) == 0:
				tria_upperList = numpy.resize(tria_upperList, (nbslice))
			check_status(status)
			status, tria_lowerList = ull.getVect1DDouble(self.idx, path, cpopath + 'tria_lower')
			if len(tria_lowerList) == 0:
				tria_lowerList = numpy.resize(tria_lowerList, (nbslice))
			check_status(status)
			xptsList = self.xpts.build_non_resampled_data(path, cpopath, nbslice)
			left_low_stList = self.left_low_st.build_non_resampled_data(path, cpopath, nbslice)
			right_low_stList = self.right_low_st.build_non_resampled_data(path, cpopath, nbslice)
			left_up_stList = self.left_up_st.build_non_resampled_data(path, cpopath, nbslice)
			right_up_stList = self.right_up_st.build_non_resampled_data(path, cpopath, nbslice)
			active_limitList = self.active_limit.build_non_resampled_data(path, cpopath, nbslice)
			status, ang_lcms_upoList = ull.getVect1DDouble(self.idx, path, cpopath + 'ang_lcms_upo')
			if len(ang_lcms_upoList) == 0:
				ang_lcms_upoList = numpy.resize(ang_lcms_upoList, (nbslice))
			check_status(status)
			status, ang_lcms_upiList = ull.getVect1DDouble(self.idx, path, cpopath + 'ang_lcms_upi')
			if len(ang_lcms_upiList) == 0:
				ang_lcms_upiList = numpy.resize(ang_lcms_upiList, (nbslice))
			check_status(status)
			status, ang_lcms_lwoList = ull.getVect1DDouble(self.idx, path, cpopath + 'ang_lcms_lwo')
			if len(ang_lcms_lwoList) == 0:
				ang_lcms_lwoList = numpy.resize(ang_lcms_lwoList, (nbslice))
			check_status(status)
			status, ang_lcms_lwiList = ull.getVect1DDouble(self.idx, path, cpopath + 'ang_lcms_lwi')
			if len(ang_lcms_lwiList) == 0:
				ang_lcms_lwiList = numpy.resize(ang_lcms_lwiList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = eqgeometrystructureeqgeometry(self.base_path)
				slice.setExpIdx(self.idx)
				slice.source = sourceVal
				slice.boundarytype = int(boundarytypeList[i].copy())
				slice.boundary = boundaryList[i]
				slice.geom_axis = geom_axisList[i]
				slice.a_minor = a_minorList[i].copy().astype(float)
				slice.elongation = elongationList[i].copy().astype(float)
				slice.elong_upper = elong_upperList[i].copy().astype(float)
				slice.elong_lower = elong_lowerList[i].copy().astype(float)
				slice.tria_upper = tria_upperList[i].copy().astype(float)
				slice.tria_lower = tria_lowerList[i].copy().astype(float)
				slice.xpts = xptsList[i]
				slice.left_low_st = left_low_stList[i]
				slice.right_low_st = right_low_stList[i]
				slice.left_up_st = left_up_stList[i]
				slice.right_up_st = right_up_stList[i]
				slice.active_limit = active_limitList[i]
				slice.ang_lcms_upo = ang_lcms_upoList[i].copy().astype(float)
				slice.ang_lcms_upi = ang_lcms_upiList[i].copy().astype(float)
				slice.ang_lcms_lwo = ang_lcms_lwoList[i].copy().astype(float)
				slice.ang_lcms_lwi = ang_lcms_lwiList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type eqgeometrystructureeqgeometryObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putIntInObject : ' + cpopath + 'boundarytype') 
			print ('obj = ' + str(obj))
		obj = ull.putIntInObject(self.idx, obj, cpopath + 'boundarytype', i, self.boundarytype)
		obj = self.boundary.putTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.geom_axis.putTimedElt(path, cpopath + 'geom_axis', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'a_minor') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'a_minor', i, self.a_minor)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'elongation') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'elongation', i, self.elongation)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'elong_upper') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'elong_upper', i, self.elong_upper)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'elong_lower') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'elong_lower', i, self.elong_lower)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'tria_upper') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'tria_upper', i, self.tria_upper)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'tria_lower') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'tria_lower', i, self.tria_lower)
		obj = self.xpts.putTimedElt(path, cpopath + 'xpts', i, obj)
		obj = self.left_low_st.putTimedElt(path, cpopath + 'left_low_st', i, obj)
		obj = self.right_low_st.putTimedElt(path, cpopath + 'right_low_st', i, obj)
		obj = self.left_up_st.putTimedElt(path, cpopath + 'left_up_st', i, obj)
		obj = self.right_up_st.putTimedElt(path, cpopath + 'right_up_st', i, obj)
		obj = self.active_limit.putTimedElt(path, cpopath + 'active_limit', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ang_lcms_upo') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ang_lcms_upo', i, self.ang_lcms_upo)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ang_lcms_upi') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ang_lcms_upi', i, self.ang_lcms_upi)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ang_lcms_lwo') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ang_lcms_lwo', i, self.ang_lcms_lwo)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'ang_lcms_lwi') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'ang_lcms_lwi', i, self.ang_lcms_lwi)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type eqgeometrystructureeqgeometryObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getIntInObject : ' + cpopath + 'boundarytype') 
			print ('obj = ' + str(obj))
		status, ret_boundarytype = ull.getIntFromObject(self.idx, obj, cpopath + 'boundarytype', i)
		check_status(status)
		if not status:
			self.boundarytype = ret_boundarytype
		self.boundary.getTimedElt(path, cpopath + 'boundary', i, obj)
		self.geom_axis.getTimedElt(path, cpopath + 'geom_axis', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'a_minor') 
			print ('obj = ' + str(obj))
		status, ret_a_minor = ull.getDoubleFromObject(self.idx, obj, cpopath + 'a_minor', i)
		check_status(status)
		if not status:
			self.a_minor = ret_a_minor
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'elongation') 
			print ('obj = ' + str(obj))
		status, ret_elongation = ull.getDoubleFromObject(self.idx, obj, cpopath + 'elongation', i)
		check_status(status)
		if not status:
			self.elongation = ret_elongation
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'elong_upper') 
			print ('obj = ' + str(obj))
		status, ret_elong_upper = ull.getDoubleFromObject(self.idx, obj, cpopath + 'elong_upper', i)
		check_status(status)
		if not status:
			self.elong_upper = ret_elong_upper
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'elong_lower') 
			print ('obj = ' + str(obj))
		status, ret_elong_lower = ull.getDoubleFromObject(self.idx, obj, cpopath + 'elong_lower', i)
		check_status(status)
		if not status:
			self.elong_lower = ret_elong_lower
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'tria_upper') 
			print ('obj = ' + str(obj))
		status, ret_tria_upper = ull.getDoubleFromObject(self.idx, obj, cpopath + 'tria_upper', i)
		check_status(status)
		if not status:
			self.tria_upper = ret_tria_upper
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'tria_lower') 
			print ('obj = ' + str(obj))
		status, ret_tria_lower = ull.getDoubleFromObject(self.idx, obj, cpopath + 'tria_lower', i)
		check_status(status)
		if not status:
			self.tria_lower = ret_tria_lower
		self.xpts.getTimedElt(path, cpopath + 'xpts', i, obj)
		self.left_low_st.getTimedElt(path, cpopath + 'left_low_st', i, obj)
		self.right_low_st.getTimedElt(path, cpopath + 'right_low_st', i, obj)
		self.left_up_st.getTimedElt(path, cpopath + 'left_up_st', i, obj)
		self.right_up_st.getTimedElt(path, cpopath + 'right_up_st', i, obj)
		self.active_limit.getTimedElt(path, cpopath + 'active_limit', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ang_lcms_upo') 
			print ('obj = ' + str(obj))
		status, ret_ang_lcms_upo = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ang_lcms_upo', i)
		check_status(status)
		if not status:
			self.ang_lcms_upo = ret_ang_lcms_upo
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ang_lcms_upi') 
			print ('obj = ' + str(obj))
		status, ret_ang_lcms_upi = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ang_lcms_upi', i)
		check_status(status)
		if not status:
			self.ang_lcms_upi = ret_ang_lcms_upi
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ang_lcms_lwo') 
			print ('obj = ' + str(obj))
		status, ret_ang_lcms_lwo = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ang_lcms_lwo', i)
		check_status(status)
		if not status:
			self.ang_lcms_lwo = ret_ang_lcms_lwo
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'ang_lcms_lwi') 
			print ('obj = ' + str(obj))
		status, ret_ang_lcms_lwi = ull.getDoubleFromObject(self.idx, obj, cpopath + 'ang_lcms_lwi', i)
		check_status(status)
		if not status:
			self.ang_lcms_lwi = ret_ang_lcms_lwi

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type eqgeometrystructureeqgeometryObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'source', i, self.source)
		obj = self.boundary.putNonTimedElt(path, cpopath + 'boundary', i, obj)
		obj = self.geom_axis.putNonTimedElt(path, cpopath + 'geom_axis', i, obj)
		obj = self.xpts.putNonTimedElt(path, cpopath + 'xpts', i, obj)
		obj = self.left_low_st.putNonTimedElt(path, cpopath + 'left_low_st', i, obj)
		obj = self.right_low_st.putNonTimedElt(path, cpopath + 'right_low_st', i, obj)
		obj = self.left_up_st.putNonTimedElt(path, cpopath + 'left_up_st', i, obj)
		obj = self.right_up_st.putNonTimedElt(path, cpopath + 'right_up_st', i, obj)
		obj = self.active_limit.putNonTimedElt(path, cpopath + 'active_limit', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type eqgeometrystructureeqgeometryObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'source') 
			print ('obj = ' + str(obj))
		status, ret_source = ull.getStringFromObject(self.idx, obj, cpopath + 'source', i)
		check_status(status)
		if not status:
			self.source = ret_source
		self.boundary.getNonTimedElt(path, cpopath + 'boundary', i, obj)
		self.geom_axis.getNonTimedElt(path, cpopath + 'geom_axis', i, obj)
		self.xpts.getNonTimedElt(path, cpopath + 'xpts', i, obj)
		self.left_low_st.getNonTimedElt(path, cpopath + 'left_low_st', i, obj)
		self.right_low_st.getNonTimedElt(path, cpopath + 'right_low_st', i, obj)
		self.left_up_st.getNonTimedElt(path, cpopath + 'left_up_st', i, obj)
		self.right_up_st.getNonTimedElt(path, cpopath + 'right_up_st', i, obj)
		self.active_limit.getNonTimedElt(path, cpopath + 'active_limit', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'source')
		ull.deleteData(self.idx, path, cpopath + 'boundarytype')
		ull.deleteData(self.idx, path, cpopath + 'boundary')
		self.geom_axis.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'a_minor')
		ull.deleteData(self.idx, path, cpopath + 'elongation')
		ull.deleteData(self.idx, path, cpopath + 'elong_upper')
		ull.deleteData(self.idx, path, cpopath + 'elong_lower')
		ull.deleteData(self.idx, path, cpopath + 'tria_upper')
		ull.deleteData(self.idx, path, cpopath + 'tria_lower')
		ull.deleteData(self.idx, path, cpopath + 'xpts')
		self.left_low_st.deleteData(path, cpopath)
		self.right_low_st.deleteData(path, cpopath)
		self.left_up_st.deleteData(path, cpopath)
		self.right_up_st.deleteData(path, cpopath)
		self.active_limit.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'ang_lcms_upo')
		ull.deleteData(self.idx, path, cpopath + 'ang_lcms_upi')
		ull.deleteData(self.idx, path, cpopath + 'ang_lcms_lwo')
		ull.deleteData(self.idx, path, cpopath + 'ang_lcms_lwi')


class boundarystruct_arrayrz1Dexp:
	'''
	class boundarystruct_arrayrz1Dexp
	RZ description of the plasma boundary; This is formally declared as an array of structure to allow for time-dependent size of the R and Z vectors in the sub-structure below. However, boundary must be allocated to size 1. Time-dependent;

	Attributes:
	- array : list of boundarystruct_arrayrz1DexpObj 
	'''

	def __init__(self, base_path_in='boundary'):
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
		ret = space + 'class boundarystruct_arrayrz1Dexp\n'
		for i in range(len(self.array)):
			ret = ret + space + 'boundarystruct_arrayrz1Dexp[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(boundarystruct_arrayrz1DexpObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type boundarystruct_arrayrz1Dexp, run function putSlice') 
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
			print ('field '+self.base_path+' of type boundarystruct_arrayrz1Dexp, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type boundarystruct_arrayrz1Dexp, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type boundarystruct_arrayrz1Dexp, run function getSlice') 
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
			print ('field '+self.base_path+' of type boundarystruct_arrayrz1Dexp, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(boundarystruct_arrayrz1Dexp(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(boundarystruct_arrayrz1Dexp(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = boundarystruct_arrayrz1Dexp(self.base_path)
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
			print ('field '+self.base_path+' of type boundarystruct_arrayrz1Dexp, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type boundarystruct_arrayrz1Dexp, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type boundarystruct_arrayrz1Dexp, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type boundarystruct_arrayrz1Dexp, run function getNonTimedElt') 
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


class boundarystruct_arrayrz1DexpObj:
	'''
	class boundarystruct_arrayrz1DexpObj
	RZ description of the plasma boundary; This is formally declared as an array of structure to allow for time-dependent size of the R and Z vectors in the sub-structure below. However, boundary must be allocated to size 1. Time-dependent;

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]. Vector(npoints). Time-dependent
	- z : numpy.ndarray 1D with float
	   Altitude [m]. Vector(npoints). Time-dependent
	'''

	def __init__(self, base_path_in='boundary'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class boundarystruct_arrayrz1DexpObj\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystruct_arrayrz1DexpObj, run function putTimedElt') 
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

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystruct_arrayrz1DexpObj, run function getTimedElt') 
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

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystruct_arrayrz1DexpObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type boundarystruct_arrayrz1DexpObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class geom_axisstructurerz0D:
	'''
	class geom_axisstructurerz0D
	RZ position of the geometric axis (defined as (Rmin+Rmax) / 2 and (Zmin+Zmax) / 2 of the boundary) [m]; Time-dependent; Scalar

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='geom_axis'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class geom_axisstructurerz0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type geom_axisstructurerz0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type geom_axisstructurerz0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type geom_axisstructurerz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type geom_axisstructurerz0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type geom_axisstructurerz0D, run function build_non_resampled_data') 
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
				slice = geom_axisstructurerz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[i].copy().astype(float)
				slice.z = zList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geom_axisstructurerz0DObj, run function putTimedElt') 
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
			print ('object of type geom_axisstructurerz0DObj, run function getTimedElt') 
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
			print ('object of type geom_axisstructurerz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type geom_axisstructurerz0DObj, run function getNonTimedElt') 
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


class xptsstruct_arrayrz1Dexp:
	'''
	class xptsstruct_arrayrz1Dexp
	Position of the Xpoints, first is the active xpoint if diverted [m]; This is formally declared as an array of structure to allow for time-dependent size of the R and Z vectors in the sub-structure below. However, xpts must be allocated to size 1. Time-dependent;

	Attributes:
	- array : list of xptsstruct_arrayrz1DexpObj 
	'''

	def __init__(self, base_path_in='xpts'):
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
		ret = space + 'class xptsstruct_arrayrz1Dexp\n'
		for i in range(len(self.array)):
			ret = ret + space + 'xptsstruct_arrayrz1Dexp[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(xptsstruct_arrayrz1DexpObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type xptsstruct_arrayrz1Dexp, run function putSlice') 
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
			print ('field '+self.base_path+' of type xptsstruct_arrayrz1Dexp, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type xptsstruct_arrayrz1Dexp, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type xptsstruct_arrayrz1Dexp, run function getSlice') 
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
			print ('field '+self.base_path+' of type xptsstruct_arrayrz1Dexp, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(xptsstruct_arrayrz1Dexp(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(xptsstruct_arrayrz1Dexp(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = xptsstruct_arrayrz1Dexp(self.base_path)
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
			print ('field '+self.base_path+' of type xptsstruct_arrayrz1Dexp, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type xptsstruct_arrayrz1Dexp, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type xptsstruct_arrayrz1Dexp, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type xptsstruct_arrayrz1Dexp, run function getNonTimedElt') 
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


class xptsstruct_arrayrz1DexpObj:
	'''
	class xptsstruct_arrayrz1DexpObj
	Position of the Xpoints, first is the active xpoint if diverted [m]; This is formally declared as an array of structure to allow for time-dependent size of the R and Z vectors in the sub-structure below. However, xpts must be allocated to size 1. Time-dependent;

	Attributes:
	- r : numpy.ndarray 1D with float
	   Major radius [m]. Vector(npoints). Time-dependent
	- z : numpy.ndarray 1D with float
	   Altitude [m]. Vector(npoints). Time-dependent
	'''

	def __init__(self, base_path_in='xpts'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros(0, numpy.float64, order='C')
		self.z = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class xptsstruct_arrayrz1DexpObj\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type xptsstruct_arrayrz1DexpObj, run function putTimedElt') 
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

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type xptsstruct_arrayrz1DexpObj, run function getTimedElt') 
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

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type xptsstruct_arrayrz1DexpObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type xptsstruct_arrayrz1DexpObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'


class left_low_ststructurerz0D:
	'''
	class left_low_ststructurerz0D
	Position of the lower left strike point [m]; Time-dependent; Scalar

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='left_low_st'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class left_low_ststructurerz0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type left_low_ststructurerz0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type left_low_ststructurerz0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type left_low_ststructurerz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type left_low_ststructurerz0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type left_low_ststructurerz0D, run function build_non_resampled_data') 
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
				slice = left_low_ststructurerz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[i].copy().astype(float)
				slice.z = zList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type left_low_ststructurerz0DObj, run function putTimedElt') 
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
			print ('object of type left_low_ststructurerz0DObj, run function getTimedElt') 
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
			print ('object of type left_low_ststructurerz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type left_low_ststructurerz0DObj, run function getNonTimedElt') 
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


class right_low_ststructurerz0D:
	'''
	class right_low_ststructurerz0D
	Position of the lower right strike point [m]; Time-dependent; Scalar

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='right_low_st'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class right_low_ststructurerz0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type right_low_ststructurerz0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type right_low_ststructurerz0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type right_low_ststructurerz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type right_low_ststructurerz0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type right_low_ststructurerz0D, run function build_non_resampled_data') 
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
				slice = right_low_ststructurerz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[i].copy().astype(float)
				slice.z = zList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type right_low_ststructurerz0DObj, run function putTimedElt') 
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
			print ('object of type right_low_ststructurerz0DObj, run function getTimedElt') 
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
			print ('object of type right_low_ststructurerz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type right_low_ststructurerz0DObj, run function getNonTimedElt') 
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


class left_up_ststructurerz0D:
	'''
	class left_up_ststructurerz0D
	Position of the upper left strike point [m]; Time-dependent; Scalar

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='left_up_st'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class left_up_ststructurerz0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type left_up_ststructurerz0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type left_up_ststructurerz0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type left_up_ststructurerz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type left_up_ststructurerz0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type left_up_ststructurerz0D, run function build_non_resampled_data') 
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
				slice = left_up_ststructurerz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[i].copy().astype(float)
				slice.z = zList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type left_up_ststructurerz0DObj, run function putTimedElt') 
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
			print ('object of type left_up_ststructurerz0DObj, run function getTimedElt') 
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
			print ('object of type left_up_ststructurerz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type left_up_ststructurerz0DObj, run function getNonTimedElt') 
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


class right_up_ststructurerz0D:
	'''
	class right_up_ststructurerz0D
	Position of the upper right strike point [m]; Time-dependent; Scalar

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='right_up_st'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class right_up_ststructurerz0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type right_up_ststructurerz0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type right_up_ststructurerz0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type right_up_ststructurerz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type right_up_ststructurerz0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type right_up_ststructurerz0D, run function build_non_resampled_data') 
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
				slice = right_up_ststructurerz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[i].copy().astype(float)
				slice.z = zList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type right_up_ststructurerz0DObj, run function putTimedElt') 
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
			print ('object of type right_up_ststructurerz0DObj, run function getTimedElt') 
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
			print ('object of type right_up_ststructurerz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type right_up_ststructurerz0DObj, run function getNonTimedElt') 
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


class active_limitstructurerz0D:
	'''
	class active_limitstructurerz0D
	Position of the active limiter point (point of the plasma boundary in contact with the limiter) [m]; Set R = 0 for X-point plasma; Time-dependent; Scalar

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='active_limit'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class active_limitstructurerz0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type active_limitstructurerz0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type active_limitstructurerz0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type active_limitstructurerz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type active_limitstructurerz0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type active_limitstructurerz0D, run function build_non_resampled_data') 
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
				slice = active_limitstructurerz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[i].copy().astype(float)
				slice.z = zList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type active_limitstructurerz0DObj, run function putTimedElt') 
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
			print ('object of type active_limitstructurerz0DObj, run function getTimedElt') 
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
			print ('object of type active_limitstructurerz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type active_limitstructurerz0DObj, run function getNonTimedElt') 
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


class flushstructureflush:
	'''
	class flushstructureflush
	FLUSH package coefficients for the mapping of the equlibrium. The time grid of this structure is the same as the equilibrium structure above.

	Attributes:
	- datainfo : class datainfostructuredatainfo
	   Generic information on a data item
	- position : class positionstructurerz1D
	   Major radius and altitude of the FLUSH grid [m]; Time-dependent; Vectors resp. (nR) and (nZ)
	- coef : numpy.ndarray 2D with float
	   Coefficients of the fit; Time-dependent; Matrix 2D (nR,nZ)
	- codeparam : class codeparamstructurecodeparam
	   Code parameters
	'''

	def __init__(self, base_path_in='flush'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.datainfo = datainfostructuredatainfo('datainfo')
		self.position = positionstructurerz1D('position')
		self.coef = numpy.zeros((0,0), numpy.float64, order='C')
		self.codeparam = codeparamstructurecodeparam('codeparam')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class flushstructureflush\n'
		ret = ret + space + 'Attribute datainfo\n ' + self.datainfo.__str__(depth+1)
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		s = self.coef.__str__()
		ret = ret + space + 'Attribute coef\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute codeparam\n ' + self.codeparam.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.datainfo.setExpIdx(idx)
		self.position.setExpIdx(idx)
		self.codeparam.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type flushstructureflush, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.datainfo.cpoTime = self.cpoTime
		self.datainfo.putSlice(path, cpopath)
		self.position.cpoTime = self.cpoTime
		self.position.putSlice(path, cpopath)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'coef', numpy.array(self.coef).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.codeparam.cpoTime = self.cpoTime
		self.codeparam.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type flushstructureflush, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.datainfo.replaceLastSlice(path, cpopath)
		self.position.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'coef', numpy.array(self.coef).astype(numpy.float64))
		check_status(status)
		self.codeparam.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type flushstructureflush, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.datainfo.putNonTimed(path, cpopath)
		self.position.putNonTimed(path, cpopath)
		self.codeparam.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type flushstructureflush, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.datainfo.getSlice(path, cpopath, inTime, interpolMode)
		self.position.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_coef, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'coef', inTime, interpolMode)
		check_status(status)
		if not status:
			self.coef = ret_coef
			self.cpoTime = retTime
		self.codeparam.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type flushstructureflush, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			datainfoList = self.datainfo.build_non_resampled_data(path, cpopath, nbslice)
			positionList = self.position.build_non_resampled_data(path, cpopath, nbslice)
			status, coefList = ull.getVect3DDouble(self.idx, path, cpopath + 'coef')
			if len(coefList) == 0:
				coefList = numpy.resize(coefList, (0,0,nbslice))
			check_status(status)
			codeparamList = self.codeparam.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = flushstructureflush(self.base_path)
				slice.setExpIdx(self.idx)
				slice.datainfo = datainfoList[i]
				slice.position = positionList[i]
				slice.coef = coefList[:,:,i]
				slice.codeparam = codeparamList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type flushstructureflushObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.position.putTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'coef') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'coef', i, numpy.array(self.coef).astype(numpy.float64))
		obj = self.codeparam.putTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type flushstructureflushObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.position.getTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'coef') 
			print ('obj = ' + str(obj))
		status, ret_coef = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'coef', i)
		check_status(status)
		if not status:
			self.coef = ret_coef
		self.codeparam.getTimedElt(path, cpopath + 'codeparam', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type flushstructureflushObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.datainfo.putNonTimedElt(path, cpopath + 'datainfo', i, obj)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		obj = self.codeparam.putNonTimedElt(path, cpopath + 'codeparam', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type flushstructureflushObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.datainfo.getNonTimedElt(path, cpopath + 'datainfo', i, obj)
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)
		self.codeparam.getNonTimedElt(path, cpopath + 'codeparam', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.datainfo.deleteData(path, cpopath)
		self.position.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'coef')
		self.codeparam.deleteData(path, cpopath)


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


class global_paramstructureglobal_param:
	'''
	class global_paramstructureglobal_param
	0d output parameters

	Attributes:
	- beta_pol : float
	   poloidal beta; Time-dependent; Scalar
	- beta_tor : float
	   toroidal beta; Time-dependent; Scalar
	- beta_normal : float
	   normalised beta; Time-dependent; Scalar
	- i_plasma : float
	   total toroidal plasma current [A]; Positive sign means anti-clockwise when viewed from above. Time-dependent; Scalar
	- li : float
	   internal inductance; Time-dependent; Scalar
	- volume : float
	   total plasma volume [m^3]; Time-dependent; Scalar
	- area : float
	   area poloidal cross section [m^2]; Time-dependent; Scalar
	- psi_ax : float
	   poloidal flux at the magnetic axis [Wb]; Time-dependent; Scalar
	- psi_bound : float
	   poloidal flux at the selected plasma boundary (separatrix for a free boundary code; fixed boundary for fixed boundary code) [Wb]; Time-dependent; Scalar
	- mag_axis : class mag_axisstructuremag_axis
	   Magnetic axis values
	- q_95 : float
	   q at the 95% poloidal flux surface; Time-dependent; Scalar
	- q_min : float
	   minimum q value in the plasma; Time-dependent; Scalar
	- toroid_field : class toroid_fieldstructureb0r0
	   Characteristics of the vacuum toroidal field, redundant with the toroidfield CPO, to be used by the ETS
	- w_mhd : float
	   Plasma energy content = 3/2 * int(p,dV) with p being the total pressure (thermal + fast particles) [J]. Time-dependent; Scalar
	- gamma : float
	   Adiabatic index. Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='global_param'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.beta_pol = EMPTY_DOUBLE
		self.beta_tor = EMPTY_DOUBLE
		self.beta_normal = EMPTY_DOUBLE
		self.i_plasma = EMPTY_DOUBLE
		self.li = EMPTY_DOUBLE
		self.volume = EMPTY_DOUBLE
		self.area = EMPTY_DOUBLE
		self.psi_ax = EMPTY_DOUBLE
		self.psi_bound = EMPTY_DOUBLE
		self.mag_axis = mag_axisstructuremag_axis('mag_axis')
		self.q_95 = EMPTY_DOUBLE
		self.q_min = EMPTY_DOUBLE
		self.toroid_field = toroid_fieldstructureb0r0('toroid_field')
		self.w_mhd = EMPTY_DOUBLE
		self.gamma = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class global_paramstructureglobal_param\n'
		ret = ret + space + 'Attribute beta_pol: ' + str(self.beta_pol) + '\n'
		ret = ret + space + 'Attribute beta_tor: ' + str(self.beta_tor) + '\n'
		ret = ret + space + 'Attribute beta_normal: ' + str(self.beta_normal) + '\n'
		ret = ret + space + 'Attribute i_plasma: ' + str(self.i_plasma) + '\n'
		ret = ret + space + 'Attribute li: ' + str(self.li) + '\n'
		ret = ret + space + 'Attribute volume: ' + str(self.volume) + '\n'
		ret = ret + space + 'Attribute area: ' + str(self.area) + '\n'
		ret = ret + space + 'Attribute psi_ax: ' + str(self.psi_ax) + '\n'
		ret = ret + space + 'Attribute psi_bound: ' + str(self.psi_bound) + '\n'
		ret = ret + space + 'Attribute mag_axis\n ' + self.mag_axis.__str__(depth+1)
		ret = ret + space + 'Attribute q_95: ' + str(self.q_95) + '\n'
		ret = ret + space + 'Attribute q_min: ' + str(self.q_min) + '\n'
		ret = ret + space + 'Attribute toroid_field\n ' + self.toroid_field.__str__(depth+1)
		ret = ret + space + 'Attribute w_mhd: ' + str(self.w_mhd) + '\n'
		ret = ret + space + 'Attribute gamma: ' + str(self.gamma) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.mag_axis.setExpIdx(idx)
		self.toroid_field.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructureglobal_param, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'beta_pol', self.beta_pol, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'beta_tor', self.beta_tor, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'beta_normal', self.beta_normal, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'i_plasma', self.i_plasma, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'li', self.li, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'volume', self.volume, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'area', self.area, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'psi_ax', self.psi_ax, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'psi_bound', self.psi_bound, self.cpoTime)
		check_status(status)
		self.mag_axis.cpoTime = self.cpoTime
		self.mag_axis.putSlice(path, cpopath)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'q_95', self.q_95, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'q_min', self.q_min, self.cpoTime)
		check_status(status)
		self.toroid_field.cpoTime = self.cpoTime
		self.toroid_field.putSlice(path, cpopath)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'w_mhd', self.w_mhd, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'gamma', self.gamma, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructureglobal_param, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'beta_pol', self.beta_pol)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'beta_tor', self.beta_tor)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'beta_normal', self.beta_normal)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'i_plasma', self.i_plasma)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'li', self.li)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'volume', self.volume)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'area', self.area)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'psi_ax', self.psi_ax)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'psi_bound', self.psi_bound)
		check_status(status)
		self.mag_axis.replaceLastSlice(path, cpopath)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'q_95', self.q_95)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'q_min', self.q_min)
		check_status(status)
		self.toroid_field.replaceLastSlice(path, cpopath)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'w_mhd', self.w_mhd)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'gamma', self.gamma)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructureglobal_param, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type global_paramstructureglobal_param, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_beta_pol, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'beta_pol', inTime, interpolMode)
		check_status(status)
		if not status:
			self.beta_pol = ret_beta_pol
			self.cpoTime = retTime
		status, ret_beta_tor, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'beta_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.beta_tor = ret_beta_tor
			self.cpoTime = retTime
		status, ret_beta_normal, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'beta_normal', inTime, interpolMode)
		check_status(status)
		if not status:
			self.beta_normal = ret_beta_normal
			self.cpoTime = retTime
		status, ret_i_plasma, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'i_plasma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.i_plasma = ret_i_plasma
			self.cpoTime = retTime
		status, ret_li, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'li', inTime, interpolMode)
		check_status(status)
		if not status:
			self.li = ret_li
			self.cpoTime = retTime
		status, ret_volume, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'volume', inTime, interpolMode)
		check_status(status)
		if not status:
			self.volume = ret_volume
			self.cpoTime = retTime
		status, ret_area, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'area', inTime, interpolMode)
		check_status(status)
		if not status:
			self.area = ret_area
			self.cpoTime = retTime
		status, ret_psi_ax, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'psi_ax', inTime, interpolMode)
		check_status(status)
		if not status:
			self.psi_ax = ret_psi_ax
			self.cpoTime = retTime
		status, ret_psi_bound, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'psi_bound', inTime, interpolMode)
		check_status(status)
		if not status:
			self.psi_bound = ret_psi_bound
			self.cpoTime = retTime
		self.mag_axis.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_q_95, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'q_95', inTime, interpolMode)
		check_status(status)
		if not status:
			self.q_95 = ret_q_95
			self.cpoTime = retTime
		status, ret_q_min, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'q_min', inTime, interpolMode)
		check_status(status)
		if not status:
			self.q_min = ret_q_min
			self.cpoTime = retTime
		self.toroid_field.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_w_mhd, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'w_mhd', inTime, interpolMode)
		check_status(status)
		if not status:
			self.w_mhd = ret_w_mhd
			self.cpoTime = retTime
		status, ret_gamma, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'gamma', inTime, interpolMode)
		check_status(status)
		if not status:
			self.gamma = ret_gamma
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type global_paramstructureglobal_param, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, beta_polList = ull.getVect1DDouble(self.idx, path, cpopath + 'beta_pol')
			if len(beta_polList) == 0:
				beta_polList = numpy.resize(beta_polList, (nbslice))
			check_status(status)
			status, beta_torList = ull.getVect1DDouble(self.idx, path, cpopath + 'beta_tor')
			if len(beta_torList) == 0:
				beta_torList = numpy.resize(beta_torList, (nbslice))
			check_status(status)
			status, beta_normalList = ull.getVect1DDouble(self.idx, path, cpopath + 'beta_normal')
			if len(beta_normalList) == 0:
				beta_normalList = numpy.resize(beta_normalList, (nbslice))
			check_status(status)
			status, i_plasmaList = ull.getVect1DDouble(self.idx, path, cpopath + 'i_plasma')
			if len(i_plasmaList) == 0:
				i_plasmaList = numpy.resize(i_plasmaList, (nbslice))
			check_status(status)
			status, liList = ull.getVect1DDouble(self.idx, path, cpopath + 'li')
			if len(liList) == 0:
				liList = numpy.resize(liList, (nbslice))
			check_status(status)
			status, volumeList = ull.getVect1DDouble(self.idx, path, cpopath + 'volume')
			if len(volumeList) == 0:
				volumeList = numpy.resize(volumeList, (nbslice))
			check_status(status)
			status, areaList = ull.getVect1DDouble(self.idx, path, cpopath + 'area')
			if len(areaList) == 0:
				areaList = numpy.resize(areaList, (nbslice))
			check_status(status)
			status, psi_axList = ull.getVect1DDouble(self.idx, path, cpopath + 'psi_ax')
			if len(psi_axList) == 0:
				psi_axList = numpy.resize(psi_axList, (nbslice))
			check_status(status)
			status, psi_boundList = ull.getVect1DDouble(self.idx, path, cpopath + 'psi_bound')
			if len(psi_boundList) == 0:
				psi_boundList = numpy.resize(psi_boundList, (nbslice))
			check_status(status)
			mag_axisList = self.mag_axis.build_non_resampled_data(path, cpopath, nbslice)
			status, q_95List = ull.getVect1DDouble(self.idx, path, cpopath + 'q_95')
			if len(q_95List) == 0:
				q_95List = numpy.resize(q_95List, (nbslice))
			check_status(status)
			status, q_minList = ull.getVect1DDouble(self.idx, path, cpopath + 'q_min')
			if len(q_minList) == 0:
				q_minList = numpy.resize(q_minList, (nbslice))
			check_status(status)
			toroid_fieldList = self.toroid_field.build_non_resampled_data(path, cpopath, nbslice)
			status, w_mhdList = ull.getVect1DDouble(self.idx, path, cpopath + 'w_mhd')
			if len(w_mhdList) == 0:
				w_mhdList = numpy.resize(w_mhdList, (nbslice))
			check_status(status)
			status, gammaList = ull.getVect1DDouble(self.idx, path, cpopath + 'gamma')
			if len(gammaList) == 0:
				gammaList = numpy.resize(gammaList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = global_paramstructureglobal_param(self.base_path)
				slice.setExpIdx(self.idx)
				slice.beta_pol = beta_polList[i].copy().astype(float)
				slice.beta_tor = beta_torList[i].copy().astype(float)
				slice.beta_normal = beta_normalList[i].copy().astype(float)
				slice.i_plasma = i_plasmaList[i].copy().astype(float)
				slice.li = liList[i].copy().astype(float)
				slice.volume = volumeList[i].copy().astype(float)
				slice.area = areaList[i].copy().astype(float)
				slice.psi_ax = psi_axList[i].copy().astype(float)
				slice.psi_bound = psi_boundList[i].copy().astype(float)
				slice.mag_axis = mag_axisList[i]
				slice.q_95 = q_95List[i].copy().astype(float)
				slice.q_min = q_minList[i].copy().astype(float)
				slice.toroid_field = toroid_fieldList[i]
				slice.w_mhd = w_mhdList[i].copy().astype(float)
				slice.gamma = gammaList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructureglobal_paramObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'beta_pol') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'beta_pol', i, self.beta_pol)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'beta_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'beta_tor', i, self.beta_tor)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'beta_normal') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'beta_normal', i, self.beta_normal)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'i_plasma') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'i_plasma', i, self.i_plasma)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'li') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'li', i, self.li)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'volume') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'volume', i, self.volume)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'area') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'area', i, self.area)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'psi_ax') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'psi_ax', i, self.psi_ax)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'psi_bound') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'psi_bound', i, self.psi_bound)
		obj = self.mag_axis.putTimedElt(path, cpopath + 'mag_axis', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'q_95') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'q_95', i, self.q_95)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'q_min') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'q_min', i, self.q_min)
		obj = self.toroid_field.putTimedElt(path, cpopath + 'toroid_field', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'w_mhd') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'w_mhd', i, self.w_mhd)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'gamma') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'gamma', i, self.gamma)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructureglobal_paramObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'beta_pol') 
			print ('obj = ' + str(obj))
		status, ret_beta_pol = ull.getDoubleFromObject(self.idx, obj, cpopath + 'beta_pol', i)
		check_status(status)
		if not status:
			self.beta_pol = ret_beta_pol
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'beta_tor') 
			print ('obj = ' + str(obj))
		status, ret_beta_tor = ull.getDoubleFromObject(self.idx, obj, cpopath + 'beta_tor', i)
		check_status(status)
		if not status:
			self.beta_tor = ret_beta_tor
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'beta_normal') 
			print ('obj = ' + str(obj))
		status, ret_beta_normal = ull.getDoubleFromObject(self.idx, obj, cpopath + 'beta_normal', i)
		check_status(status)
		if not status:
			self.beta_normal = ret_beta_normal
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'i_plasma') 
			print ('obj = ' + str(obj))
		status, ret_i_plasma = ull.getDoubleFromObject(self.idx, obj, cpopath + 'i_plasma', i)
		check_status(status)
		if not status:
			self.i_plasma = ret_i_plasma
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'li') 
			print ('obj = ' + str(obj))
		status, ret_li = ull.getDoubleFromObject(self.idx, obj, cpopath + 'li', i)
		check_status(status)
		if not status:
			self.li = ret_li
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'volume') 
			print ('obj = ' + str(obj))
		status, ret_volume = ull.getDoubleFromObject(self.idx, obj, cpopath + 'volume', i)
		check_status(status)
		if not status:
			self.volume = ret_volume
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'area') 
			print ('obj = ' + str(obj))
		status, ret_area = ull.getDoubleFromObject(self.idx, obj, cpopath + 'area', i)
		check_status(status)
		if not status:
			self.area = ret_area
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'psi_ax') 
			print ('obj = ' + str(obj))
		status, ret_psi_ax = ull.getDoubleFromObject(self.idx, obj, cpopath + 'psi_ax', i)
		check_status(status)
		if not status:
			self.psi_ax = ret_psi_ax
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'psi_bound') 
			print ('obj = ' + str(obj))
		status, ret_psi_bound = ull.getDoubleFromObject(self.idx, obj, cpopath + 'psi_bound', i)
		check_status(status)
		if not status:
			self.psi_bound = ret_psi_bound
		self.mag_axis.getTimedElt(path, cpopath + 'mag_axis', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'q_95') 
			print ('obj = ' + str(obj))
		status, ret_q_95 = ull.getDoubleFromObject(self.idx, obj, cpopath + 'q_95', i)
		check_status(status)
		if not status:
			self.q_95 = ret_q_95
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'q_min') 
			print ('obj = ' + str(obj))
		status, ret_q_min = ull.getDoubleFromObject(self.idx, obj, cpopath + 'q_min', i)
		check_status(status)
		if not status:
			self.q_min = ret_q_min
		self.toroid_field.getTimedElt(path, cpopath + 'toroid_field', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'w_mhd') 
			print ('obj = ' + str(obj))
		status, ret_w_mhd = ull.getDoubleFromObject(self.idx, obj, cpopath + 'w_mhd', i)
		check_status(status)
		if not status:
			self.w_mhd = ret_w_mhd
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'gamma') 
			print ('obj = ' + str(obj))
		status, ret_gamma = ull.getDoubleFromObject(self.idx, obj, cpopath + 'gamma', i)
		check_status(status)
		if not status:
			self.gamma = ret_gamma

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructureglobal_paramObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.mag_axis.putNonTimedElt(path, cpopath + 'mag_axis', i, obj)
		obj = self.toroid_field.putNonTimedElt(path, cpopath + 'toroid_field', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type global_paramstructureglobal_paramObj, run function getNonTimedElt') 
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
		ull.deleteData(self.idx, path, cpopath + 'beta_pol')
		ull.deleteData(self.idx, path, cpopath + 'beta_tor')
		ull.deleteData(self.idx, path, cpopath + 'beta_normal')
		ull.deleteData(self.idx, path, cpopath + 'i_plasma')
		ull.deleteData(self.idx, path, cpopath + 'li')
		ull.deleteData(self.idx, path, cpopath + 'volume')
		ull.deleteData(self.idx, path, cpopath + 'area')
		ull.deleteData(self.idx, path, cpopath + 'psi_ax')
		ull.deleteData(self.idx, path, cpopath + 'psi_bound')
		self.mag_axis.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'q_95')
		ull.deleteData(self.idx, path, cpopath + 'q_min')
		self.toroid_field.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'w_mhd')
		ull.deleteData(self.idx, path, cpopath + 'gamma')


class mag_axisstructuremag_axis:
	'''
	class mag_axisstructuremag_axis
	Magnetic axis values

	Attributes:
	- position : class positionstructurerz0D
	   Position of the magnetic axis [m]; Time-dependent; Scalar; 
	- bphi : float
	   Total toroidal magnetic field at the magnetic axis [T]; Time-dependent; Scalar
	- q : float
	   q at the magnetic axis; Time-dependent; Scalar
	'''

	def __init__(self, base_path_in='mag_axis'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.position = positionstructurerz0D('position')
		self.bphi = EMPTY_DOUBLE
		self.q = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class mag_axisstructuremag_axis\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		ret = ret + space + 'Attribute bphi: ' + str(self.bphi) + '\n'
		ret = ret + space + 'Attribute q: ' + str(self.q) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.position.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mag_axisstructuremag_axis, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.cpoTime = self.cpoTime
		self.position.putSlice(path, cpopath)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'bphi', self.bphi, self.cpoTime)
		check_status(status)
		status = ull.putDoubleSlice(self.idx, path, cpopath + 'q', self.q, self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mag_axisstructuremag_axis, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.replaceLastSlice(path, cpopath)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'bphi', self.bphi)
		check_status(status)
		status = ull.replaceLastDoubleSlice(self.idx, path, cpopath + 'q', self.q)
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type mag_axisstructuremag_axis, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type mag_axisstructuremag_axis, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_bphi, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'bphi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.bphi = ret_bphi
			self.cpoTime = retTime
		status, ret_q, retTime = ull.getDoubleSlice(self.idx, path, cpopath + 'q', inTime, interpolMode)
		check_status(status)
		if not status:
			self.q = ret_q
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type mag_axisstructuremag_axis, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			positionList = self.position.build_non_resampled_data(path, cpopath, nbslice)
			status, bphiList = ull.getVect1DDouble(self.idx, path, cpopath + 'bphi')
			if len(bphiList) == 0:
				bphiList = numpy.resize(bphiList, (nbslice))
			check_status(status)
			status, qList = ull.getVect1DDouble(self.idx, path, cpopath + 'q')
			if len(qList) == 0:
				qList = numpy.resize(qList, (nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = mag_axisstructuremag_axis(self.base_path)
				slice.setExpIdx(self.idx)
				slice.position = positionList[i]
				slice.bphi = bphiList[i].copy().astype(float)
				slice.q = qList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mag_axisstructuremag_axisObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.position.putTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'bphi') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'bphi', i, self.bphi)
		if (dev()):
			print ('putDoubleInObject : ' + cpopath + 'q') 
			print ('obj = ' + str(obj))
		obj = ull.putDoubleInObject(self.idx, obj, cpopath + 'q', i, self.q)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mag_axisstructuremag_axisObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.position.getTimedElt(path, cpopath + 'position', i, obj)
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'bphi') 
			print ('obj = ' + str(obj))
		status, ret_bphi = ull.getDoubleFromObject(self.idx, obj, cpopath + 'bphi', i)
		check_status(status)
		if not status:
			self.bphi = ret_bphi
		if (dev()):
			print ('getDoubleInObject : ' + cpopath + 'q') 
			print ('obj = ' + str(obj))
		status, ret_q = ull.getDoubleFromObject(self.idx, obj, cpopath + 'q', i)
		check_status(status)
		if not status:
			self.q = ret_q

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mag_axisstructuremag_axisObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type mag_axisstructuremag_axisObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.position.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'bphi')
		ull.deleteData(self.idx, path, cpopath + 'q')


class positionstructurerz0D:
	'''
	class positionstructurerz0D
	Position of the magnetic axis [m]; Time-dependent; Scalar; 

	Attributes:
	- r : float
	   Major radius [m]
	- z : float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='position'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = EMPTY_DOUBLE
		self.z = EMPTY_DOUBLE

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class positionstructurerz0D\n'
		ret = ret + space + 'Attribute r: ' + str(self.r) + '\n'
		ret = ret + space + 'Attribute z: ' + str(self.z) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz0D, run function putSlice') 
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
			print ('field '+self.base_path+' of type positionstructurerz0D, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type positionstructurerz0D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz0D, run function getSlice') 
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
			print ('field '+self.base_path+' of type positionstructurerz0D, run function build_non_resampled_data') 
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
				slice = positionstructurerz0D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[i].copy().astype(float)
				slice.z = zList[i].copy().astype(float)
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz0DObj, run function putTimedElt') 
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
			print ('object of type positionstructurerz0DObj, run function getTimedElt') 
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
			print ('object of type positionstructurerz0DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz0DObj, run function getNonTimedElt') 
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
	Characteristics of the vacuum toroidal field, redundant with the toroidfield CPO, to be used by the ETS

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


class profiles_1dstructureprofiles_1d:
	'''
	class profiles_1dstructureprofiles_1d
	output profiles as a function of the poloidal flux

	Attributes:
	- psi : numpy.ndarray 1D with float
	   Poloidal flux [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent; Vector (npsi)
	- phi : numpy.ndarray 1D with float
	   toroidal flux [Wb]; Time-dependent; Vector (npsi)
	- pressure : numpy.ndarray 1D with float
	   pressure profile as a function of the poloidal flux [Pa]; Time-dependent; Vector (npsi)
	- F_dia : numpy.ndarray 1D with float
	   diamagnetic profile (R B_phi) [T m]; Time-dependent; Vector (npsi)
	- pprime : numpy.ndarray 1D with float
	   psi derivative of the pressure profile [Pa/Wb]; Time-dependent; Vector (npsi)
	- ffprime : numpy.ndarray 1D with float
	   psi derivative of F_dia multiplied with F_dia [T^2 m^2/Wb]; Time-dependent; Vector (npsi)
	- jphi : numpy.ndarray 1D with float
	   flux surface averaged toroidal current density = average(jphi/R) / average(1/R) [A/m^2]; Time-dependent; Vector (npsi)
	- jparallel : numpy.ndarray 1D with float
	   flux surface averaged parallel current density = average(j.B) / B0, where B0 = equilibrium/global_param/toroid_field/b0 ; [A/m^2]; Time-dependent; Vector (npsi)
	- q : numpy.ndarray 1D with float
	   Safety factor = dphi/dpsi [-]; Time-dependent; Vector (npsi)
	- shear : numpy.ndarray 1D with float
	   Magnetic shear, defined as rho_tor/q*dq/drho_tor [-]; Time-dependent; Vector (npsi)
	- r_inboard : numpy.ndarray 1D with float
	   radial coordinate (major radius) at the height and on the left of the magnetic axis [m]; Time-dependent; Vector (npsi)
	- r_outboard : numpy.ndarray 1D with float
	   radial coordinate (major radius) at the height and on the right of the magnetic axis [m]; Time-dependent; Vector (npsi)
	- rho_tor : numpy.ndarray 1D with float
	   Toroidal flux coordinate [m], to be used by the ETS and in many CPOs (coreprof, ...). Defined as sqrt(phi/pi/B0), where B0 = equilibrium/global_param/toroid_field/b0. Time-dependent; Vector (npsi)
	- dpsidrho_tor : numpy.ndarray 1D with float
	   dpsi/drho_tor [Wb/m]; Time-dependent; Vector (npsi)
	- rho_vol : numpy.ndarray 1D with float
	   Normalised radial coordinate related to the plasma volume. Defined as sqrt(volume / volume[LCFS]). Time-dependent; Vector (npsi)
	- beta_pol : numpy.ndarray 1D with float
	   poloidal beta (inside the magnetic surface); Time-dependent; Vector (npsi)
	- li : numpy.ndarray 1D with float
	   internal inductance (inside the magnetic surface); Time-dependent; Vector (npsi)
	- elongation : numpy.ndarray 1D with float
	   Elongation; Time-dependent; Vector (npsi)
	- tria_upper : numpy.ndarray 1D with float
	   Upper triangularity profile; Time-dependent; Vector (npsi)
	- tria_lower : numpy.ndarray 1D with float
	   Lower triangularity profile; Time-dependent; Vector (npsi)
	- volume : numpy.ndarray 1D with float
	   Volume enclosed in the flux surface [m^3]; Time-dependent; Vector (npsi)
	- vprime : numpy.ndarray 1D with float
	   Radial derivative of the volume enclosed in the flux surface with respect to psi, i.e. dV/dpsi [m^3/Wb]; Time-dependent; Vector (npsi)
	- dvdrho : numpy.ndarray 1D with float
	   Radial derivative of the volume enclosed in the flux surface with respect to rho_tor, i.e. dV/drho_tor [m^2]; Time-dependent; Vector (npsi)
	- area : numpy.ndarray 1D with float
	   Cross-sectional area of the flux surface [m^2]; Time-dependent; Vector (npsi)
	- aprime : numpy.ndarray 1D with float
	   Radial derivative of the cross-sectional area of the flux surface with respect to psi, i.e. darea/dpsi [m^2/Wb]; Time-dependent; Vector (npsi)
	- surface : numpy.ndarray 1D with float
	   Surface area of the flux surface [m^2]; Time-dependent; Vector (npsi)
	- ftrap : numpy.ndarray 1D with float
	   Trapped particle fraction; Time-dependent; Vector (npsi)
	- gm1 : numpy.ndarray 1D with float
	   average(1/R^2); Time-dependent; Vector (npsi)
	- gm2 : numpy.ndarray 1D with float
	   average(grad_rho^2/R^2); Time-dependent; Vector (npsi)
	- gm3 : numpy.ndarray 1D with float
	   average(grad_rho^2); Time-dependent; Vector (npsi)
	- gm4 : numpy.ndarray 1D with float
	   average(1/B^2) [T^-2]; Time-dependent; Vector (npsi)
	- gm5 : numpy.ndarray 1D with float
	   average(B^2) [T^2]; Time-dependent; Vector (npsi)
	- gm6 : numpy.ndarray 1D with float
	   average(grad_rho^2/B^2)  [T^-2]; Time-dependent; Vector (npsi)
	- gm7 : numpy.ndarray 1D with float
	   average(grad_rho); Time-dependent; Vector (npsi)
	- gm8 : numpy.ndarray 1D with float
	   average(R); Time-dependent; Vector (npsi)
	- gm9 : numpy.ndarray 1D with float
	   average(1/R); Time-dependent; Vector (npsi)
	- b_av : numpy.ndarray 1D with float
	   average(B); Time-dependent; Vector (npsi)
	- b_min : numpy.ndarray 1D with float
	   minimum(B) on the flux surface; Time-dependent; Vector (npsi)
	- b_max : numpy.ndarray 1D with float
	   maximum(B) on the flux surface; Time-dependent; Vector (npsi)
	- omega : numpy.ndarray 1D with float
	   Toroidal rotation angular frequency (assumed constant on the flux surface)  [rad/s]; Time-dependent; Vector (npsi)
	- omegaprime : numpy.ndarray 1D with float
	   Psi derivative of the toroidal rotation angular frequency (assumed constant on the flux surface)  [rad/(s.Wb)]; Time-dependent; Vector (npsi)
	- mach_a : numpy.ndarray 1D with float
	   Alfvenic Mach number; Time-dependent; Vector (npsi)
	- phi_flow : numpy.ndarray 1D with float
	   Poloidal flow function phi_flow = rho*v_pol/B_pol[kg/(V.s^2)] where rho is mass density; Time-dependent; Vector (npsi)
	- s_flow : numpy.ndarray 1D with float
	   Flux function in the closure equation p=S(psi).rho^(gamma); Entropy (gamma=5/3) or Temperature (gamma=1); Time-dependent; Vector (npsi)
	- h_flow : numpy.ndarray 1D with float
	   flow function h_flow = gamma/(gamma-1)*s_flow*rho^(gamma-1) + 0.5*(phi_flow*B/rho)^2 - 0.5*(R*omega)^2 [m^2/s^2]; Time-dependent; Vector (npsi)
	- rho_mass : numpy.ndarray 1D with float
	   Mass density [kg/m^3]; Time-dependent; Vector (npsi)
	'''

	def __init__(self, base_path_in='profiles_1d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.psi = numpy.zeros(0, numpy.float64, order='C')
		self.phi = numpy.zeros(0, numpy.float64, order='C')
		self.pressure = numpy.zeros(0, numpy.float64, order='C')
		self.F_dia = numpy.zeros(0, numpy.float64, order='C')
		self.pprime = numpy.zeros(0, numpy.float64, order='C')
		self.ffprime = numpy.zeros(0, numpy.float64, order='C')
		self.jphi = numpy.zeros(0, numpy.float64, order='C')
		self.jparallel = numpy.zeros(0, numpy.float64, order='C')
		self.q = numpy.zeros(0, numpy.float64, order='C')
		self.shear = numpy.zeros(0, numpy.float64, order='C')
		self.r_inboard = numpy.zeros(0, numpy.float64, order='C')
		self.r_outboard = numpy.zeros(0, numpy.float64, order='C')
		self.rho_tor = numpy.zeros(0, numpy.float64, order='C')
		self.dpsidrho_tor = numpy.zeros(0, numpy.float64, order='C')
		self.rho_vol = numpy.zeros(0, numpy.float64, order='C')
		self.beta_pol = numpy.zeros(0, numpy.float64, order='C')
		self.li = numpy.zeros(0, numpy.float64, order='C')
		self.elongation = numpy.zeros(0, numpy.float64, order='C')
		self.tria_upper = numpy.zeros(0, numpy.float64, order='C')
		self.tria_lower = numpy.zeros(0, numpy.float64, order='C')
		self.volume = numpy.zeros(0, numpy.float64, order='C')
		self.vprime = numpy.zeros(0, numpy.float64, order='C')
		self.dvdrho = numpy.zeros(0, numpy.float64, order='C')
		self.area = numpy.zeros(0, numpy.float64, order='C')
		self.aprime = numpy.zeros(0, numpy.float64, order='C')
		self.surface = numpy.zeros(0, numpy.float64, order='C')
		self.ftrap = numpy.zeros(0, numpy.float64, order='C')
		self.gm1 = numpy.zeros(0, numpy.float64, order='C')
		self.gm2 = numpy.zeros(0, numpy.float64, order='C')
		self.gm3 = numpy.zeros(0, numpy.float64, order='C')
		self.gm4 = numpy.zeros(0, numpy.float64, order='C')
		self.gm5 = numpy.zeros(0, numpy.float64, order='C')
		self.gm6 = numpy.zeros(0, numpy.float64, order='C')
		self.gm7 = numpy.zeros(0, numpy.float64, order='C')
		self.gm8 = numpy.zeros(0, numpy.float64, order='C')
		self.gm9 = numpy.zeros(0, numpy.float64, order='C')
		self.b_av = numpy.zeros(0, numpy.float64, order='C')
		self.b_min = numpy.zeros(0, numpy.float64, order='C')
		self.b_max = numpy.zeros(0, numpy.float64, order='C')
		self.omega = numpy.zeros(0, numpy.float64, order='C')
		self.omegaprime = numpy.zeros(0, numpy.float64, order='C')
		self.mach_a = numpy.zeros(0, numpy.float64, order='C')
		self.phi_flow = numpy.zeros(0, numpy.float64, order='C')
		self.s_flow = numpy.zeros(0, numpy.float64, order='C')
		self.h_flow = numpy.zeros(0, numpy.float64, order='C')
		self.rho_mass = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class profiles_1dstructureprofiles_1d\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pressure.__str__()
		ret = ret + space + 'Attribute pressure\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.F_dia.__str__()
		ret = ret + space + 'Attribute F_dia\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pprime.__str__()
		ret = ret + space + 'Attribute pprime\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ffprime.__str__()
		ret = ret + space + 'Attribute ffprime\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.jphi.__str__()
		ret = ret + space + 'Attribute jphi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.jparallel.__str__()
		ret = ret + space + 'Attribute jparallel\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.q.__str__()
		ret = ret + space + 'Attribute q\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.shear.__str__()
		ret = ret + space + 'Attribute shear\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.r_inboard.__str__()
		ret = ret + space + 'Attribute r_inboard\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.r_outboard.__str__()
		ret = ret + space + 'Attribute r_outboard\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_tor.__str__()
		ret = ret + space + 'Attribute rho_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dpsidrho_tor.__str__()
		ret = ret + space + 'Attribute dpsidrho_tor\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_vol.__str__()
		ret = ret + space + 'Attribute rho_vol\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.beta_pol.__str__()
		ret = ret + space + 'Attribute beta_pol\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.li.__str__()
		ret = ret + space + 'Attribute li\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.elongation.__str__()
		ret = ret + space + 'Attribute elongation\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.tria_upper.__str__()
		ret = ret + space + 'Attribute tria_upper\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.tria_lower.__str__()
		ret = ret + space + 'Attribute tria_lower\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.volume.__str__()
		ret = ret + space + 'Attribute volume\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vprime.__str__()
		ret = ret + space + 'Attribute vprime\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dvdrho.__str__()
		ret = ret + space + 'Attribute dvdrho\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.area.__str__()
		ret = ret + space + 'Attribute area\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.aprime.__str__()
		ret = ret + space + 'Attribute aprime\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.surface.__str__()
		ret = ret + space + 'Attribute surface\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.ftrap.__str__()
		ret = ret + space + 'Attribute ftrap\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.gm1.__str__()
		ret = ret + space + 'Attribute gm1\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.gm2.__str__()
		ret = ret + space + 'Attribute gm2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.gm3.__str__()
		ret = ret + space + 'Attribute gm3\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.gm4.__str__()
		ret = ret + space + 'Attribute gm4\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.gm5.__str__()
		ret = ret + space + 'Attribute gm5\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.gm6.__str__()
		ret = ret + space + 'Attribute gm6\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.gm7.__str__()
		ret = ret + space + 'Attribute gm7\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.gm8.__str__()
		ret = ret + space + 'Attribute gm8\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.gm9.__str__()
		ret = ret + space + 'Attribute gm9\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_av.__str__()
		ret = ret + space + 'Attribute b_av\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_min.__str__()
		ret = ret + space + 'Attribute b_min\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.b_max.__str__()
		ret = ret + space + 'Attribute b_max\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.omega.__str__()
		ret = ret + space + 'Attribute omega\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.omegaprime.__str__()
		ret = ret + space + 'Attribute omegaprime\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.mach_a.__str__()
		ret = ret + space + 'Attribute mach_a\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi_flow.__str__()
		ret = ret + space + 'Attribute phi_flow\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.s_flow.__str__()
		ret = ret + space + 'Attribute s_flow\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.h_flow.__str__()
		ret = ret + space + 'Attribute h_flow\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_mass.__str__()
		ret = ret + space + 'Attribute rho_mass\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructureprofiles_1d, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pressure', numpy.array(self.pressure).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'F_dia', numpy.array(self.F_dia).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'pprime', numpy.array(self.pprime).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ffprime', numpy.array(self.ffprime).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'jphi', numpy.array(self.jphi).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'jparallel', numpy.array(self.jparallel).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'q', numpy.array(self.q).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'shear', numpy.array(self.shear).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'r_inboard', numpy.array(self.r_inboard).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'r_outboard', numpy.array(self.r_outboard).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dpsidrho_tor', numpy.array(self.dpsidrho_tor).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_vol', numpy.array(self.rho_vol).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'beta_pol', numpy.array(self.beta_pol).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'li', numpy.array(self.li).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'elongation', numpy.array(self.elongation).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'tria_upper', numpy.array(self.tria_upper).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'tria_lower', numpy.array(self.tria_lower).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'volume', numpy.array(self.volume).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'vprime', numpy.array(self.vprime).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dvdrho', numpy.array(self.dvdrho).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'area', numpy.array(self.area).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'aprime', numpy.array(self.aprime).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'surface', numpy.array(self.surface).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'ftrap', numpy.array(self.ftrap).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'gm1', numpy.array(self.gm1).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'gm2', numpy.array(self.gm2).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'gm3', numpy.array(self.gm3).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'gm4', numpy.array(self.gm4).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'gm5', numpy.array(self.gm5).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'gm6', numpy.array(self.gm6).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'gm7', numpy.array(self.gm7).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'gm8', numpy.array(self.gm8).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'gm9', numpy.array(self.gm9).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'b_av', numpy.array(self.b_av).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'b_min', numpy.array(self.b_min).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'b_max', numpy.array(self.b_max).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'omega', numpy.array(self.omega).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'omegaprime', numpy.array(self.omegaprime).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'mach_a', numpy.array(self.mach_a).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'phi_flow', numpy.array(self.phi_flow).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 's_flow', numpy.array(self.s_flow).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'h_flow', numpy.array(self.h_flow).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'rho_mass', numpy.array(self.rho_mass).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructureprofiles_1d, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'psi', numpy.array(self.psi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'phi', numpy.array(self.phi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pressure', numpy.array(self.pressure).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'F_dia', numpy.array(self.F_dia).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'pprime', numpy.array(self.pprime).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ffprime', numpy.array(self.ffprime).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'jphi', numpy.array(self.jphi).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'jparallel', numpy.array(self.jparallel).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'q', numpy.array(self.q).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'shear', numpy.array(self.shear).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'r_inboard', numpy.array(self.r_inboard).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'r_outboard', numpy.array(self.r_outboard).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', numpy.array(self.rho_tor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dpsidrho_tor', numpy.array(self.dpsidrho_tor).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_vol', numpy.array(self.rho_vol).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'beta_pol', numpy.array(self.beta_pol).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'li', numpy.array(self.li).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'elongation', numpy.array(self.elongation).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'tria_upper', numpy.array(self.tria_upper).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'tria_lower', numpy.array(self.tria_lower).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'volume', numpy.array(self.volume).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'vprime', numpy.array(self.vprime).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dvdrho', numpy.array(self.dvdrho).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'area', numpy.array(self.area).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'aprime', numpy.array(self.aprime).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'surface', numpy.array(self.surface).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'ftrap', numpy.array(self.ftrap).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'gm1', numpy.array(self.gm1).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'gm2', numpy.array(self.gm2).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'gm3', numpy.array(self.gm3).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'gm4', numpy.array(self.gm4).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'gm5', numpy.array(self.gm5).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'gm6', numpy.array(self.gm6).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'gm7', numpy.array(self.gm7).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'gm8', numpy.array(self.gm8).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'gm9', numpy.array(self.gm9).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'b_av', numpy.array(self.b_av).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'b_min', numpy.array(self.b_min).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'b_max', numpy.array(self.b_max).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'omega', numpy.array(self.omega).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'omegaprime', numpy.array(self.omegaprime).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'mach_a', numpy.array(self.mach_a).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'phi_flow', numpy.array(self.phi_flow).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 's_flow', numpy.array(self.s_flow).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'h_flow', numpy.array(self.h_flow).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'rho_mass', numpy.array(self.rho_mass).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructureprofiles_1d, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructureprofiles_1d, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_psi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'psi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.psi = ret_psi
			self.cpoTime = retTime
		status, ret_phi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'phi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi = ret_phi
			self.cpoTime = retTime
		status, ret_pressure, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pressure', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pressure = ret_pressure
			self.cpoTime = retTime
		status, ret_F_dia, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'F_dia', inTime, interpolMode)
		check_status(status)
		if not status:
			self.F_dia = ret_F_dia
			self.cpoTime = retTime
		status, ret_pprime, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'pprime', inTime, interpolMode)
		check_status(status)
		if not status:
			self.pprime = ret_pprime
			self.cpoTime = retTime
		status, ret_ffprime, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ffprime', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ffprime = ret_ffprime
			self.cpoTime = retTime
		status, ret_jphi, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'jphi', inTime, interpolMode)
		check_status(status)
		if not status:
			self.jphi = ret_jphi
			self.cpoTime = retTime
		status, ret_jparallel, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'jparallel', inTime, interpolMode)
		check_status(status)
		if not status:
			self.jparallel = ret_jparallel
			self.cpoTime = retTime
		status, ret_q, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'q', inTime, interpolMode)
		check_status(status)
		if not status:
			self.q = ret_q
			self.cpoTime = retTime
		status, ret_shear, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'shear', inTime, interpolMode)
		check_status(status)
		if not status:
			self.shear = ret_shear
			self.cpoTime = retTime
		status, ret_r_inboard, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'r_inboard', inTime, interpolMode)
		check_status(status)
		if not status:
			self.r_inboard = ret_r_inboard
			self.cpoTime = retTime
		status, ret_r_outboard, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'r_outboard', inTime, interpolMode)
		check_status(status)
		if not status:
			self.r_outboard = ret_r_outboard
			self.cpoTime = retTime
		status, ret_rho_tor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'rho_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
			self.cpoTime = retTime
		status, ret_dpsidrho_tor, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dpsidrho_tor', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dpsidrho_tor = ret_dpsidrho_tor
			self.cpoTime = retTime
		status, ret_rho_vol, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'rho_vol', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_vol = ret_rho_vol
			self.cpoTime = retTime
		status, ret_beta_pol, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'beta_pol', inTime, interpolMode)
		check_status(status)
		if not status:
			self.beta_pol = ret_beta_pol
			self.cpoTime = retTime
		status, ret_li, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'li', inTime, interpolMode)
		check_status(status)
		if not status:
			self.li = ret_li
			self.cpoTime = retTime
		status, ret_elongation, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'elongation', inTime, interpolMode)
		check_status(status)
		if not status:
			self.elongation = ret_elongation
			self.cpoTime = retTime
		status, ret_tria_upper, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'tria_upper', inTime, interpolMode)
		check_status(status)
		if not status:
			self.tria_upper = ret_tria_upper
			self.cpoTime = retTime
		status, ret_tria_lower, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'tria_lower', inTime, interpolMode)
		check_status(status)
		if not status:
			self.tria_lower = ret_tria_lower
			self.cpoTime = retTime
		status, ret_volume, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'volume', inTime, interpolMode)
		check_status(status)
		if not status:
			self.volume = ret_volume
			self.cpoTime = retTime
		status, ret_vprime, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'vprime', inTime, interpolMode)
		check_status(status)
		if not status:
			self.vprime = ret_vprime
			self.cpoTime = retTime
		status, ret_dvdrho, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dvdrho', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dvdrho = ret_dvdrho
			self.cpoTime = retTime
		status, ret_area, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'area', inTime, interpolMode)
		check_status(status)
		if not status:
			self.area = ret_area
			self.cpoTime = retTime
		status, ret_aprime, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'aprime', inTime, interpolMode)
		check_status(status)
		if not status:
			self.aprime = ret_aprime
			self.cpoTime = retTime
		status, ret_surface, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'surface', inTime, interpolMode)
		check_status(status)
		if not status:
			self.surface = ret_surface
			self.cpoTime = retTime
		status, ret_ftrap, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'ftrap', inTime, interpolMode)
		check_status(status)
		if not status:
			self.ftrap = ret_ftrap
			self.cpoTime = retTime
		status, ret_gm1, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'gm1', inTime, interpolMode)
		check_status(status)
		if not status:
			self.gm1 = ret_gm1
			self.cpoTime = retTime
		status, ret_gm2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'gm2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.gm2 = ret_gm2
			self.cpoTime = retTime
		status, ret_gm3, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'gm3', inTime, interpolMode)
		check_status(status)
		if not status:
			self.gm3 = ret_gm3
			self.cpoTime = retTime
		status, ret_gm4, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'gm4', inTime, interpolMode)
		check_status(status)
		if not status:
			self.gm4 = ret_gm4
			self.cpoTime = retTime
		status, ret_gm5, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'gm5', inTime, interpolMode)
		check_status(status)
		if not status:
			self.gm5 = ret_gm5
			self.cpoTime = retTime
		status, ret_gm6, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'gm6', inTime, interpolMode)
		check_status(status)
		if not status:
			self.gm6 = ret_gm6
			self.cpoTime = retTime
		status, ret_gm7, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'gm7', inTime, interpolMode)
		check_status(status)
		if not status:
			self.gm7 = ret_gm7
			self.cpoTime = retTime
		status, ret_gm8, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'gm8', inTime, interpolMode)
		check_status(status)
		if not status:
			self.gm8 = ret_gm8
			self.cpoTime = retTime
		status, ret_gm9, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'gm9', inTime, interpolMode)
		check_status(status)
		if not status:
			self.gm9 = ret_gm9
			self.cpoTime = retTime
		status, ret_b_av, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'b_av', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_av = ret_b_av
			self.cpoTime = retTime
		status, ret_b_min, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'b_min', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_min = ret_b_min
			self.cpoTime = retTime
		status, ret_b_max, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'b_max', inTime, interpolMode)
		check_status(status)
		if not status:
			self.b_max = ret_b_max
			self.cpoTime = retTime
		status, ret_omega, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'omega', inTime, interpolMode)
		check_status(status)
		if not status:
			self.omega = ret_omega
			self.cpoTime = retTime
		status, ret_omegaprime, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'omegaprime', inTime, interpolMode)
		check_status(status)
		if not status:
			self.omegaprime = ret_omegaprime
			self.cpoTime = retTime
		status, ret_mach_a, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'mach_a', inTime, interpolMode)
		check_status(status)
		if not status:
			self.mach_a = ret_mach_a
			self.cpoTime = retTime
		status, ret_phi_flow, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'phi_flow', inTime, interpolMode)
		check_status(status)
		if not status:
			self.phi_flow = ret_phi_flow
			self.cpoTime = retTime
		status, ret_s_flow, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 's_flow', inTime, interpolMode)
		check_status(status)
		if not status:
			self.s_flow = ret_s_flow
			self.cpoTime = retTime
		status, ret_h_flow, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'h_flow', inTime, interpolMode)
		check_status(status)
		if not status:
			self.h_flow = ret_h_flow
			self.cpoTime = retTime
		status, ret_rho_mass, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'rho_mass', inTime, interpolMode)
		check_status(status)
		if not status:
			self.rho_mass = ret_rho_mass
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_1dstructureprofiles_1d, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, psiList = ull.getVect2DDouble(self.idx, path, cpopath + 'psi')
			if len(psiList) == 0:
				psiList = numpy.resize(psiList, (0,nbslice))
			check_status(status)
			status, phiList = ull.getVect2DDouble(self.idx, path, cpopath + 'phi')
			if len(phiList) == 0:
				phiList = numpy.resize(phiList, (0,nbslice))
			check_status(status)
			status, pressureList = ull.getVect2DDouble(self.idx, path, cpopath + 'pressure')
			if len(pressureList) == 0:
				pressureList = numpy.resize(pressureList, (0,nbslice))
			check_status(status)
			status, F_diaList = ull.getVect2DDouble(self.idx, path, cpopath + 'F_dia')
			if len(F_diaList) == 0:
				F_diaList = numpy.resize(F_diaList, (0,nbslice))
			check_status(status)
			status, pprimeList = ull.getVect2DDouble(self.idx, path, cpopath + 'pprime')
			if len(pprimeList) == 0:
				pprimeList = numpy.resize(pprimeList, (0,nbslice))
			check_status(status)
			status, ffprimeList = ull.getVect2DDouble(self.idx, path, cpopath + 'ffprime')
			if len(ffprimeList) == 0:
				ffprimeList = numpy.resize(ffprimeList, (0,nbslice))
			check_status(status)
			status, jphiList = ull.getVect2DDouble(self.idx, path, cpopath + 'jphi')
			if len(jphiList) == 0:
				jphiList = numpy.resize(jphiList, (0,nbslice))
			check_status(status)
			status, jparallelList = ull.getVect2DDouble(self.idx, path, cpopath + 'jparallel')
			if len(jparallelList) == 0:
				jparallelList = numpy.resize(jparallelList, (0,nbslice))
			check_status(status)
			status, qList = ull.getVect2DDouble(self.idx, path, cpopath + 'q')
			if len(qList) == 0:
				qList = numpy.resize(qList, (0,nbslice))
			check_status(status)
			status, shearList = ull.getVect2DDouble(self.idx, path, cpopath + 'shear')
			if len(shearList) == 0:
				shearList = numpy.resize(shearList, (0,nbslice))
			check_status(status)
			status, r_inboardList = ull.getVect2DDouble(self.idx, path, cpopath + 'r_inboard')
			if len(r_inboardList) == 0:
				r_inboardList = numpy.resize(r_inboardList, (0,nbslice))
			check_status(status)
			status, r_outboardList = ull.getVect2DDouble(self.idx, path, cpopath + 'r_outboard')
			if len(r_outboardList) == 0:
				r_outboardList = numpy.resize(r_outboardList, (0,nbslice))
			check_status(status)
			status, rho_torList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_tor')
			if len(rho_torList) == 0:
				rho_torList = numpy.resize(rho_torList, (0,nbslice))
			check_status(status)
			status, dpsidrho_torList = ull.getVect2DDouble(self.idx, path, cpopath + 'dpsidrho_tor')
			if len(dpsidrho_torList) == 0:
				dpsidrho_torList = numpy.resize(dpsidrho_torList, (0,nbslice))
			check_status(status)
			status, rho_volList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_vol')
			if len(rho_volList) == 0:
				rho_volList = numpy.resize(rho_volList, (0,nbslice))
			check_status(status)
			status, beta_polList = ull.getVect2DDouble(self.idx, path, cpopath + 'beta_pol')
			if len(beta_polList) == 0:
				beta_polList = numpy.resize(beta_polList, (0,nbslice))
			check_status(status)
			status, liList = ull.getVect2DDouble(self.idx, path, cpopath + 'li')
			if len(liList) == 0:
				liList = numpy.resize(liList, (0,nbslice))
			check_status(status)
			status, elongationList = ull.getVect2DDouble(self.idx, path, cpopath + 'elongation')
			if len(elongationList) == 0:
				elongationList = numpy.resize(elongationList, (0,nbslice))
			check_status(status)
			status, tria_upperList = ull.getVect2DDouble(self.idx, path, cpopath + 'tria_upper')
			if len(tria_upperList) == 0:
				tria_upperList = numpy.resize(tria_upperList, (0,nbslice))
			check_status(status)
			status, tria_lowerList = ull.getVect2DDouble(self.idx, path, cpopath + 'tria_lower')
			if len(tria_lowerList) == 0:
				tria_lowerList = numpy.resize(tria_lowerList, (0,nbslice))
			check_status(status)
			status, volumeList = ull.getVect2DDouble(self.idx, path, cpopath + 'volume')
			if len(volumeList) == 0:
				volumeList = numpy.resize(volumeList, (0,nbslice))
			check_status(status)
			status, vprimeList = ull.getVect2DDouble(self.idx, path, cpopath + 'vprime')
			if len(vprimeList) == 0:
				vprimeList = numpy.resize(vprimeList, (0,nbslice))
			check_status(status)
			status, dvdrhoList = ull.getVect2DDouble(self.idx, path, cpopath + 'dvdrho')
			if len(dvdrhoList) == 0:
				dvdrhoList = numpy.resize(dvdrhoList, (0,nbslice))
			check_status(status)
			status, areaList = ull.getVect2DDouble(self.idx, path, cpopath + 'area')
			if len(areaList) == 0:
				areaList = numpy.resize(areaList, (0,nbslice))
			check_status(status)
			status, aprimeList = ull.getVect2DDouble(self.idx, path, cpopath + 'aprime')
			if len(aprimeList) == 0:
				aprimeList = numpy.resize(aprimeList, (0,nbslice))
			check_status(status)
			status, surfaceList = ull.getVect2DDouble(self.idx, path, cpopath + 'surface')
			if len(surfaceList) == 0:
				surfaceList = numpy.resize(surfaceList, (0,nbslice))
			check_status(status)
			status, ftrapList = ull.getVect2DDouble(self.idx, path, cpopath + 'ftrap')
			if len(ftrapList) == 0:
				ftrapList = numpy.resize(ftrapList, (0,nbslice))
			check_status(status)
			status, gm1List = ull.getVect2DDouble(self.idx, path, cpopath + 'gm1')
			if len(gm1List) == 0:
				gm1List = numpy.resize(gm1List, (0,nbslice))
			check_status(status)
			status, gm2List = ull.getVect2DDouble(self.idx, path, cpopath + 'gm2')
			if len(gm2List) == 0:
				gm2List = numpy.resize(gm2List, (0,nbslice))
			check_status(status)
			status, gm3List = ull.getVect2DDouble(self.idx, path, cpopath + 'gm3')
			if len(gm3List) == 0:
				gm3List = numpy.resize(gm3List, (0,nbslice))
			check_status(status)
			status, gm4List = ull.getVect2DDouble(self.idx, path, cpopath + 'gm4')
			if len(gm4List) == 0:
				gm4List = numpy.resize(gm4List, (0,nbslice))
			check_status(status)
			status, gm5List = ull.getVect2DDouble(self.idx, path, cpopath + 'gm5')
			if len(gm5List) == 0:
				gm5List = numpy.resize(gm5List, (0,nbslice))
			check_status(status)
			status, gm6List = ull.getVect2DDouble(self.idx, path, cpopath + 'gm6')
			if len(gm6List) == 0:
				gm6List = numpy.resize(gm6List, (0,nbslice))
			check_status(status)
			status, gm7List = ull.getVect2DDouble(self.idx, path, cpopath + 'gm7')
			if len(gm7List) == 0:
				gm7List = numpy.resize(gm7List, (0,nbslice))
			check_status(status)
			status, gm8List = ull.getVect2DDouble(self.idx, path, cpopath + 'gm8')
			if len(gm8List) == 0:
				gm8List = numpy.resize(gm8List, (0,nbslice))
			check_status(status)
			status, gm9List = ull.getVect2DDouble(self.idx, path, cpopath + 'gm9')
			if len(gm9List) == 0:
				gm9List = numpy.resize(gm9List, (0,nbslice))
			check_status(status)
			status, b_avList = ull.getVect2DDouble(self.idx, path, cpopath + 'b_av')
			if len(b_avList) == 0:
				b_avList = numpy.resize(b_avList, (0,nbslice))
			check_status(status)
			status, b_minList = ull.getVect2DDouble(self.idx, path, cpopath + 'b_min')
			if len(b_minList) == 0:
				b_minList = numpy.resize(b_minList, (0,nbslice))
			check_status(status)
			status, b_maxList = ull.getVect2DDouble(self.idx, path, cpopath + 'b_max')
			if len(b_maxList) == 0:
				b_maxList = numpy.resize(b_maxList, (0,nbslice))
			check_status(status)
			status, omegaList = ull.getVect2DDouble(self.idx, path, cpopath + 'omega')
			if len(omegaList) == 0:
				omegaList = numpy.resize(omegaList, (0,nbslice))
			check_status(status)
			status, omegaprimeList = ull.getVect2DDouble(self.idx, path, cpopath + 'omegaprime')
			if len(omegaprimeList) == 0:
				omegaprimeList = numpy.resize(omegaprimeList, (0,nbslice))
			check_status(status)
			status, mach_aList = ull.getVect2DDouble(self.idx, path, cpopath + 'mach_a')
			if len(mach_aList) == 0:
				mach_aList = numpy.resize(mach_aList, (0,nbslice))
			check_status(status)
			status, phi_flowList = ull.getVect2DDouble(self.idx, path, cpopath + 'phi_flow')
			if len(phi_flowList) == 0:
				phi_flowList = numpy.resize(phi_flowList, (0,nbslice))
			check_status(status)
			status, s_flowList = ull.getVect2DDouble(self.idx, path, cpopath + 's_flow')
			if len(s_flowList) == 0:
				s_flowList = numpy.resize(s_flowList, (0,nbslice))
			check_status(status)
			status, h_flowList = ull.getVect2DDouble(self.idx, path, cpopath + 'h_flow')
			if len(h_flowList) == 0:
				h_flowList = numpy.resize(h_flowList, (0,nbslice))
			check_status(status)
			status, rho_massList = ull.getVect2DDouble(self.idx, path, cpopath + 'rho_mass')
			if len(rho_massList) == 0:
				rho_massList = numpy.resize(rho_massList, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = profiles_1dstructureprofiles_1d(self.base_path)
				slice.setExpIdx(self.idx)
				slice.psi = psiList[:,i]
				slice.phi = phiList[:,i]
				slice.pressure = pressureList[:,i]
				slice.F_dia = F_diaList[:,i]
				slice.pprime = pprimeList[:,i]
				slice.ffprime = ffprimeList[:,i]
				slice.jphi = jphiList[:,i]
				slice.jparallel = jparallelList[:,i]
				slice.q = qList[:,i]
				slice.shear = shearList[:,i]
				slice.r_inboard = r_inboardList[:,i]
				slice.r_outboard = r_outboardList[:,i]
				slice.rho_tor = rho_torList[:,i]
				slice.dpsidrho_tor = dpsidrho_torList[:,i]
				slice.rho_vol = rho_volList[:,i]
				slice.beta_pol = beta_polList[:,i]
				slice.li = liList[:,i]
				slice.elongation = elongationList[:,i]
				slice.tria_upper = tria_upperList[:,i]
				slice.tria_lower = tria_lowerList[:,i]
				slice.volume = volumeList[:,i]
				slice.vprime = vprimeList[:,i]
				slice.dvdrho = dvdrhoList[:,i]
				slice.area = areaList[:,i]
				slice.aprime = aprimeList[:,i]
				slice.surface = surfaceList[:,i]
				slice.ftrap = ftrapList[:,i]
				slice.gm1 = gm1List[:,i]
				slice.gm2 = gm2List[:,i]
				slice.gm3 = gm3List[:,i]
				slice.gm4 = gm4List[:,i]
				slice.gm5 = gm5List[:,i]
				slice.gm6 = gm6List[:,i]
				slice.gm7 = gm7List[:,i]
				slice.gm8 = gm8List[:,i]
				slice.gm9 = gm9List[:,i]
				slice.b_av = b_avList[:,i]
				slice.b_min = b_minList[:,i]
				slice.b_max = b_maxList[:,i]
				slice.omega = omegaList[:,i]
				slice.omegaprime = omegaprimeList[:,i]
				slice.mach_a = mach_aList[:,i]
				slice.phi_flow = phi_flowList[:,i]
				slice.s_flow = s_flowList[:,i]
				slice.h_flow = h_flowList[:,i]
				slice.rho_mass = rho_massList[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructureprofiles_1dObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'psi', i, numpy.array(self.psi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pressure') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pressure', i, numpy.array(self.pressure).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'F_dia') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'F_dia', i, numpy.array(self.F_dia).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'pprime') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'pprime', i, numpy.array(self.pprime).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ffprime') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ffprime', i, numpy.array(self.ffprime).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'jphi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'jphi', i, numpy.array(self.jphi).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'jparallel') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'jparallel', i, numpy.array(self.jparallel).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'q') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'q', i, numpy.array(self.q).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'shear') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'shear', i, numpy.array(self.shear).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'r_inboard') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'r_inboard', i, numpy.array(self.r_inboard).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'r_outboard') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'r_outboard', i, numpy.array(self.r_outboard).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'rho_tor', i, numpy.array(self.rho_tor).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dpsidrho_tor') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dpsidrho_tor', i, numpy.array(self.dpsidrho_tor).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'rho_vol') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'rho_vol', i, numpy.array(self.rho_vol).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'beta_pol') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'beta_pol', i, numpy.array(self.beta_pol).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'li') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'li', i, numpy.array(self.li).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'elongation') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'elongation', i, numpy.array(self.elongation).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'tria_upper') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'tria_upper', i, numpy.array(self.tria_upper).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'tria_lower') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'tria_lower', i, numpy.array(self.tria_lower).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'volume') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'volume', i, numpy.array(self.volume).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'vprime') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'vprime', i, numpy.array(self.vprime).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dvdrho') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dvdrho', i, numpy.array(self.dvdrho).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'area') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'area', i, numpy.array(self.area).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'aprime') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'aprime', i, numpy.array(self.aprime).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'surface') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'surface', i, numpy.array(self.surface).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'ftrap') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'ftrap', i, numpy.array(self.ftrap).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'gm1') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'gm1', i, numpy.array(self.gm1).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'gm2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'gm2', i, numpy.array(self.gm2).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'gm3') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'gm3', i, numpy.array(self.gm3).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'gm4') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'gm4', i, numpy.array(self.gm4).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'gm5') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'gm5', i, numpy.array(self.gm5).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'gm6') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'gm6', i, numpy.array(self.gm6).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'gm7') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'gm7', i, numpy.array(self.gm7).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'gm8') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'gm8', i, numpy.array(self.gm8).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'gm9') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'gm9', i, numpy.array(self.gm9).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'b_av') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'b_av', i, numpy.array(self.b_av).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'b_min') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'b_min', i, numpy.array(self.b_min).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'b_max') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'b_max', i, numpy.array(self.b_max).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'omega') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'omega', i, numpy.array(self.omega).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'omegaprime') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'omegaprime', i, numpy.array(self.omegaprime).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'mach_a') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'mach_a', i, numpy.array(self.mach_a).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'phi_flow') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'phi_flow', i, numpy.array(self.phi_flow).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 's_flow') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 's_flow', i, numpy.array(self.s_flow).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'h_flow') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'h_flow', i, numpy.array(self.h_flow).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'rho_mass') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'rho_mass', i, numpy.array(self.rho_mass).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructureprofiles_1dObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		status, ret_psi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'psi', i)
		check_status(status)
		if not status:
			self.psi = ret_psi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pressure') 
			print ('obj = ' + str(obj))
		status, ret_pressure = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pressure', i)
		check_status(status)
		if not status:
			self.pressure = ret_pressure
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'F_dia') 
			print ('obj = ' + str(obj))
		status, ret_F_dia = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'F_dia', i)
		check_status(status)
		if not status:
			self.F_dia = ret_F_dia
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'pprime') 
			print ('obj = ' + str(obj))
		status, ret_pprime = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'pprime', i)
		check_status(status)
		if not status:
			self.pprime = ret_pprime
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ffprime') 
			print ('obj = ' + str(obj))
		status, ret_ffprime = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ffprime', i)
		check_status(status)
		if not status:
			self.ffprime = ret_ffprime
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'jphi') 
			print ('obj = ' + str(obj))
		status, ret_jphi = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'jphi', i)
		check_status(status)
		if not status:
			self.jphi = ret_jphi
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'jparallel') 
			print ('obj = ' + str(obj))
		status, ret_jparallel = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'jparallel', i)
		check_status(status)
		if not status:
			self.jparallel = ret_jparallel
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'q') 
			print ('obj = ' + str(obj))
		status, ret_q = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'q', i)
		check_status(status)
		if not status:
			self.q = ret_q
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'shear') 
			print ('obj = ' + str(obj))
		status, ret_shear = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'shear', i)
		check_status(status)
		if not status:
			self.shear = ret_shear
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'r_inboard') 
			print ('obj = ' + str(obj))
		status, ret_r_inboard = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'r_inboard', i)
		check_status(status)
		if not status:
			self.r_inboard = ret_r_inboard
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'r_outboard') 
			print ('obj = ' + str(obj))
		status, ret_r_outboard = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'r_outboard', i)
		check_status(status)
		if not status:
			self.r_outboard = ret_r_outboard
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'rho_tor') 
			print ('obj = ' + str(obj))
		status, ret_rho_tor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'rho_tor', i)
		check_status(status)
		if not status:
			self.rho_tor = ret_rho_tor
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dpsidrho_tor') 
			print ('obj = ' + str(obj))
		status, ret_dpsidrho_tor = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dpsidrho_tor', i)
		check_status(status)
		if not status:
			self.dpsidrho_tor = ret_dpsidrho_tor
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'rho_vol') 
			print ('obj = ' + str(obj))
		status, ret_rho_vol = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'rho_vol', i)
		check_status(status)
		if not status:
			self.rho_vol = ret_rho_vol
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'beta_pol') 
			print ('obj = ' + str(obj))
		status, ret_beta_pol = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'beta_pol', i)
		check_status(status)
		if not status:
			self.beta_pol = ret_beta_pol
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'li') 
			print ('obj = ' + str(obj))
		status, ret_li = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'li', i)
		check_status(status)
		if not status:
			self.li = ret_li
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'elongation') 
			print ('obj = ' + str(obj))
		status, ret_elongation = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'elongation', i)
		check_status(status)
		if not status:
			self.elongation = ret_elongation
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'tria_upper') 
			print ('obj = ' + str(obj))
		status, ret_tria_upper = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'tria_upper', i)
		check_status(status)
		if not status:
			self.tria_upper = ret_tria_upper
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'tria_lower') 
			print ('obj = ' + str(obj))
		status, ret_tria_lower = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'tria_lower', i)
		check_status(status)
		if not status:
			self.tria_lower = ret_tria_lower
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'volume') 
			print ('obj = ' + str(obj))
		status, ret_volume = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'volume', i)
		check_status(status)
		if not status:
			self.volume = ret_volume
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'vprime') 
			print ('obj = ' + str(obj))
		status, ret_vprime = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'vprime', i)
		check_status(status)
		if not status:
			self.vprime = ret_vprime
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dvdrho') 
			print ('obj = ' + str(obj))
		status, ret_dvdrho = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dvdrho', i)
		check_status(status)
		if not status:
			self.dvdrho = ret_dvdrho
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'area') 
			print ('obj = ' + str(obj))
		status, ret_area = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'area', i)
		check_status(status)
		if not status:
			self.area = ret_area
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'aprime') 
			print ('obj = ' + str(obj))
		status, ret_aprime = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'aprime', i)
		check_status(status)
		if not status:
			self.aprime = ret_aprime
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'surface') 
			print ('obj = ' + str(obj))
		status, ret_surface = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'surface', i)
		check_status(status)
		if not status:
			self.surface = ret_surface
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'ftrap') 
			print ('obj = ' + str(obj))
		status, ret_ftrap = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'ftrap', i)
		check_status(status)
		if not status:
			self.ftrap = ret_ftrap
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'gm1') 
			print ('obj = ' + str(obj))
		status, ret_gm1 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'gm1', i)
		check_status(status)
		if not status:
			self.gm1 = ret_gm1
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'gm2') 
			print ('obj = ' + str(obj))
		status, ret_gm2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'gm2', i)
		check_status(status)
		if not status:
			self.gm2 = ret_gm2
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'gm3') 
			print ('obj = ' + str(obj))
		status, ret_gm3 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'gm3', i)
		check_status(status)
		if not status:
			self.gm3 = ret_gm3
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'gm4') 
			print ('obj = ' + str(obj))
		status, ret_gm4 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'gm4', i)
		check_status(status)
		if not status:
			self.gm4 = ret_gm4
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'gm5') 
			print ('obj = ' + str(obj))
		status, ret_gm5 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'gm5', i)
		check_status(status)
		if not status:
			self.gm5 = ret_gm5
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'gm6') 
			print ('obj = ' + str(obj))
		status, ret_gm6 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'gm6', i)
		check_status(status)
		if not status:
			self.gm6 = ret_gm6
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'gm7') 
			print ('obj = ' + str(obj))
		status, ret_gm7 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'gm7', i)
		check_status(status)
		if not status:
			self.gm7 = ret_gm7
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'gm8') 
			print ('obj = ' + str(obj))
		status, ret_gm8 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'gm8', i)
		check_status(status)
		if not status:
			self.gm8 = ret_gm8
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'gm9') 
			print ('obj = ' + str(obj))
		status, ret_gm9 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'gm9', i)
		check_status(status)
		if not status:
			self.gm9 = ret_gm9
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'b_av') 
			print ('obj = ' + str(obj))
		status, ret_b_av = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'b_av', i)
		check_status(status)
		if not status:
			self.b_av = ret_b_av
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'b_min') 
			print ('obj = ' + str(obj))
		status, ret_b_min = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'b_min', i)
		check_status(status)
		if not status:
			self.b_min = ret_b_min
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'b_max') 
			print ('obj = ' + str(obj))
		status, ret_b_max = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'b_max', i)
		check_status(status)
		if not status:
			self.b_max = ret_b_max
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'omega') 
			print ('obj = ' + str(obj))
		status, ret_omega = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'omega', i)
		check_status(status)
		if not status:
			self.omega = ret_omega
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'omegaprime') 
			print ('obj = ' + str(obj))
		status, ret_omegaprime = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'omegaprime', i)
		check_status(status)
		if not status:
			self.omegaprime = ret_omegaprime
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'mach_a') 
			print ('obj = ' + str(obj))
		status, ret_mach_a = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'mach_a', i)
		check_status(status)
		if not status:
			self.mach_a = ret_mach_a
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'phi_flow') 
			print ('obj = ' + str(obj))
		status, ret_phi_flow = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'phi_flow', i)
		check_status(status)
		if not status:
			self.phi_flow = ret_phi_flow
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 's_flow') 
			print ('obj = ' + str(obj))
		status, ret_s_flow = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 's_flow', i)
		check_status(status)
		if not status:
			self.s_flow = ret_s_flow
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'h_flow') 
			print ('obj = ' + str(obj))
		status, ret_h_flow = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'h_flow', i)
		check_status(status)
		if not status:
			self.h_flow = ret_h_flow
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'rho_mass') 
			print ('obj = ' + str(obj))
		status, ret_rho_mass = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'rho_mass', i)
		check_status(status)
		if not status:
			self.rho_mass = ret_rho_mass

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructureprofiles_1dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_1dstructureprofiles_1dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'psi')
		ull.deleteData(self.idx, path, cpopath + 'phi')
		ull.deleteData(self.idx, path, cpopath + 'pressure')
		ull.deleteData(self.idx, path, cpopath + 'F_dia')
		ull.deleteData(self.idx, path, cpopath + 'pprime')
		ull.deleteData(self.idx, path, cpopath + 'ffprime')
		ull.deleteData(self.idx, path, cpopath + 'jphi')
		ull.deleteData(self.idx, path, cpopath + 'jparallel')
		ull.deleteData(self.idx, path, cpopath + 'q')
		ull.deleteData(self.idx, path, cpopath + 'shear')
		ull.deleteData(self.idx, path, cpopath + 'r_inboard')
		ull.deleteData(self.idx, path, cpopath + 'r_outboard')
		ull.deleteData(self.idx, path, cpopath + 'rho_tor')
		ull.deleteData(self.idx, path, cpopath + 'dpsidrho_tor')
		ull.deleteData(self.idx, path, cpopath + 'rho_vol')
		ull.deleteData(self.idx, path, cpopath + 'beta_pol')
		ull.deleteData(self.idx, path, cpopath + 'li')
		ull.deleteData(self.idx, path, cpopath + 'elongation')
		ull.deleteData(self.idx, path, cpopath + 'tria_upper')
		ull.deleteData(self.idx, path, cpopath + 'tria_lower')
		ull.deleteData(self.idx, path, cpopath + 'volume')
		ull.deleteData(self.idx, path, cpopath + 'vprime')
		ull.deleteData(self.idx, path, cpopath + 'dvdrho')
		ull.deleteData(self.idx, path, cpopath + 'area')
		ull.deleteData(self.idx, path, cpopath + 'aprime')
		ull.deleteData(self.idx, path, cpopath + 'surface')
		ull.deleteData(self.idx, path, cpopath + 'ftrap')
		ull.deleteData(self.idx, path, cpopath + 'gm1')
		ull.deleteData(self.idx, path, cpopath + 'gm2')
		ull.deleteData(self.idx, path, cpopath + 'gm3')
		ull.deleteData(self.idx, path, cpopath + 'gm4')
		ull.deleteData(self.idx, path, cpopath + 'gm5')
		ull.deleteData(self.idx, path, cpopath + 'gm6')
		ull.deleteData(self.idx, path, cpopath + 'gm7')
		ull.deleteData(self.idx, path, cpopath + 'gm8')
		ull.deleteData(self.idx, path, cpopath + 'gm9')
		ull.deleteData(self.idx, path, cpopath + 'b_av')
		ull.deleteData(self.idx, path, cpopath + 'b_min')
		ull.deleteData(self.idx, path, cpopath + 'b_max')
		ull.deleteData(self.idx, path, cpopath + 'omega')
		ull.deleteData(self.idx, path, cpopath + 'omegaprime')
		ull.deleteData(self.idx, path, cpopath + 'mach_a')
		ull.deleteData(self.idx, path, cpopath + 'phi_flow')
		ull.deleteData(self.idx, path, cpopath + 's_flow')
		ull.deleteData(self.idx, path, cpopath + 'h_flow')
		ull.deleteData(self.idx, path, cpopath + 'rho_mass')


class profiles_2dstruct_arrayequilibrium_profiles_2d:
	'''
	class profiles_2dstruct_arrayequilibrium_profiles_2d
	Output profiles in the poloidal plane. Time-dependent

	Attributes:
	- array : list of profiles_2dstruct_arrayequilibrium_profiles_2dObj 
	'''

	def __init__(self, base_path_in='profiles_2d'):
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
		ret = space + 'class profiles_2dstruct_arrayequilibrium_profiles_2d\n'
		for i in range(len(self.array)):
			ret = ret + space + 'profiles_2dstruct_arrayequilibrium_profiles_2d[%d] = \n' %(i) + self.array[i].__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		for i in range(len(self.array)):
			self.array[i].setExpIdx(idx)

	def resize(self, nbelt):
		self.array = []
		for i in range(nbelt):
			self.array.append(profiles_2dstruct_arrayequilibrium_profiles_2dObj(self.base_path))
			self.array[i].setExpIdx(self.idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type profiles_2dstruct_arrayequilibrium_profiles_2d, run function putSlice') 
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
			print ('field '+self.base_path+' of type profiles_2dstruct_arrayequilibrium_profiles_2d, run function replaceLastSlice') 
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
			print ('field '+self.base_path+' of type profiles_2dstruct_arrayequilibrium_profiles_2d, run function putNonTimed') 
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
			print ('field '+self.base_path+' of type profiles_2dstruct_arrayequilibrium_profiles_2d, run function getSlice') 
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
			print ('field '+self.base_path+' of type profiles_2dstruct_arrayequilibrium_profiles_2d, run function build_non_resampled_data') 
		list=[]
		if nbslice > 0:
			if (dev()):
				print ('getObject('+path+', '+cpopath+self.base_path+', TIMED)')
			status, obj_time = ull.getObject(self.idx, path, cpopath + self.base_path, TIMED)
			if status:
				print ('Failed to get data: ' + path + '/' + cpopath + self.base_path + ' (TIMED)')
				for i in range(nbslice):
					list.append(profiles_2dstruct_arrayequilibrium_profiles_2d(self.base_path))
				return list
			for n in range(nbslice):
				status, obj = ull.getObjectFromObject(self.idx, obj_time, 'ALLTIMES', n)
				if status:
					print ('Failed to get object[' + n + '] from timed array of structures: ' + path + '/' + cpopath + self.base_path + ' (time = ' + str(inTime) + ', object = ALLTIMES)')
					list.append(profiles_2dstruct_arrayequilibrium_profiles_2d(self.base_path))
					continue
				obj_size = ull.getObjectDim(self.idx, obj)
				slice = profiles_2dstruct_arrayequilibrium_profiles_2d(self.base_path)
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
			print ('field '+self.base_path+' of type profiles_2dstruct_arrayequilibrium_profiles_2d, run function putTimedElt') 
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
			print ('field '+self.base_path+' of type profiles_2dstruct_arrayequilibrium_profiles_2d, run function getTimedElt') 
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
			print ('field '+self.base_path+' of type profiles_2dstruct_arrayequilibrium_profiles_2d, run function putNonTimedElt') 
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
			print ('field '+self.base_path+' of type profiles_2dstruct_arrayequilibrium_profiles_2d, run function getNonTimedElt') 
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


class profiles_2dstruct_arrayequilibrium_profiles_2dObj:
	'''
	class profiles_2dstruct_arrayequilibrium_profiles_2dObj
	Output profiles in the poloidal plane. Time-dependent

	Attributes:
	- grid_type : list of str
	   Selection of one of a set of grid types. 1-rectangular (R,Z) grid, in this case the position arrays should not be filled since they are redundant with grid/dim1 and dim2.
	- grid : class gridstructureequilibrium_profiles2d_grid
	   definition of the 2D grid
	- r : numpy.ndarray 2D with float
	   values of the major radius on the grid [m]; Time-dependent; Matrix (ndim1, ndim2)
	- z : numpy.ndarray 2D with float
	   values of the altitude on the grid [m]; Time-dependent; Matrix (ndim1, ndim2)
	- psi : numpy.ndarray 2D with float
	   values of the poloidal flux at the grid in the poloidal plane [Wb]; Time-dependent; Matrix (ndim1, ndim2)
	- theta : numpy.ndarray 2D with float
	   values of the poloidal angle on the grid [rad]; Time-dependent; Matrix (ndim1, ndim2)
	- phi : numpy.ndarray 2D with float
	   Toroidal flux [Wb]. Time-dependent; Matrix (ndim1, ndim2)
	- jphi : numpy.ndarray 2D with float
	   toroidal plasma current density [A m-2]; Time-dependent; Matrix (ndim1, ndim2)
	- jpar : numpy.ndarray 2D with float
	   parallel (to magnetic field) plasma current density [A m-2]; Time-dependent; Matrix (ndim1, ndim2)
	- br : numpy.ndarray 2D with float
	   R component of the poloidal magnetic field at the specified grid [T]; Time-dependent; Matrix (ndim1, ndim2)
	- bz : numpy.ndarray 2D with float
	   Z component of the poloidal magnetic field at the specified grid [T]; Time-dependent; Matrix (ndim1, ndim2)
	- bphi : numpy.ndarray 2D with float
	   toroidal component of the magnetic field at the specified grid [T]; Time-dependent; Matrix (ndim1, ndim2)
	- vphi : numpy.ndarray 2D with float
	   toroidal flow velocity [m/s]; Time-dependent; Matrix (ndim1, ndim2)
	- vtheta : numpy.ndarray 2D with float
	   Poloidal flow velocity [m/s]; Time-dependent; Matrix (ndim1, ndim2)
	- rho_mass : numpy.ndarray 2D with float
	   Mass density [kg/m^3]; Time-dependent; Matrix (ndim1, ndim2)
	- pressure : numpy.ndarray 2D with float
	   Pressure [Pa]; Time-dependent; Matrix (ndim1, ndim2)
	- temperature : numpy.ndarray 2D with float
	   Temperature [eV]; Time-dependent; Matrix (ndim1, ndim2)
	'''

	def __init__(self, base_path_in='profiles_2d'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.grid_type = ['']
		self.grid = gridstructureequilibrium_profiles2d_grid('grid')
		self.r = numpy.zeros((0,0), numpy.float64, order='C')
		self.z = numpy.zeros((0,0), numpy.float64, order='C')
		self.psi = numpy.zeros((0,0), numpy.float64, order='C')
		self.theta = numpy.zeros((0,0), numpy.float64, order='C')
		self.phi = numpy.zeros((0,0), numpy.float64, order='C')
		self.jphi = numpy.zeros((0,0), numpy.float64, order='C')
		self.jpar = numpy.zeros((0,0), numpy.float64, order='C')
		self.br = numpy.zeros((0,0), numpy.float64, order='C')
		self.bz = numpy.zeros((0,0), numpy.float64, order='C')
		self.bphi = numpy.zeros((0,0), numpy.float64, order='C')
		self.vphi = numpy.zeros((0,0), numpy.float64, order='C')
		self.vtheta = numpy.zeros((0,0), numpy.float64, order='C')
		self.rho_mass = numpy.zeros((0,0), numpy.float64, order='C')
		self.pressure = numpy.zeros((0,0), numpy.float64, order='C')
		self.temperature = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class profiles_2dstruct_arrayequilibrium_profiles_2dObj\n'
		s = self.grid_type.__str__()
		ret = ret + space + 'Attribute grid_type\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute grid\n ' + self.grid.__str__(depth+1)
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.psi.__str__()
		ret = ret + space + 'Attribute psi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.theta.__str__()
		ret = ret + space + 'Attribute theta\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.phi.__str__()
		ret = ret + space + 'Attribute phi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.jphi.__str__()
		ret = ret + space + 'Attribute jphi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.jpar.__str__()
		ret = ret + space + 'Attribute jpar\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.br.__str__()
		ret = ret + space + 'Attribute br\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.bz.__str__()
		ret = ret + space + 'Attribute bz\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.bphi.__str__()
		ret = ret + space + 'Attribute bphi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vphi.__str__()
		ret = ret + space + 'Attribute vphi\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.vtheta.__str__()
		ret = ret + space + 'Attribute vtheta\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.rho_mass.__str__()
		ret = ret + space + 'Attribute rho_mass\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.pressure.__str__()
		ret = ret + space + 'Attribute pressure\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.temperature.__str__()
		ret = ret + space + 'Attribute temperature\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.grid.setExpIdx(idx)

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstruct_arrayequilibrium_profiles_2dObj, run function putTimedElt') 
		cpopath = cpopath + '/'
		obj = self.grid.putTimedElt(path, cpopath + 'grid', i, obj)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'r') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'r', i, numpy.array(self.r).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'z') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'z', i, numpy.array(self.z).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'psi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'psi', i, numpy.array(self.psi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'theta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'theta', i, numpy.array(self.theta).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'phi', i, numpy.array(self.phi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'jphi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'jphi', i, numpy.array(self.jphi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'jpar') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'jpar', i, numpy.array(self.jpar).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'br') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'br', i, numpy.array(self.br).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'bz') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'bz', i, numpy.array(self.bz).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'bphi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'bphi', i, numpy.array(self.bphi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vphi') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vphi', i, numpy.array(self.vphi).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'vtheta') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'vtheta', i, numpy.array(self.vtheta).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'rho_mass') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'rho_mass', i, numpy.array(self.rho_mass).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'pressure') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'pressure', i, numpy.array(self.pressure).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'temperature') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'temperature', i, numpy.array(self.temperature).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstruct_arrayequilibrium_profiles_2dObj, run function getTimedElt') 
		cpopath = cpopath + '/'
		self.grid.getTimedElt(path, cpopath + 'grid', i, obj)
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
			print ('getVect2DDoubleInObject : ' + cpopath + 'phi') 
			print ('obj = ' + str(obj))
		status, ret_phi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'phi', i)
		check_status(status)
		if not status:
			self.phi = ret_phi
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'jphi') 
			print ('obj = ' + str(obj))
		status, ret_jphi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'jphi', i)
		check_status(status)
		if not status:
			self.jphi = ret_jphi
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'jpar') 
			print ('obj = ' + str(obj))
		status, ret_jpar = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'jpar', i)
		check_status(status)
		if not status:
			self.jpar = ret_jpar
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'br') 
			print ('obj = ' + str(obj))
		status, ret_br = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'br', i)
		check_status(status)
		if not status:
			self.br = ret_br
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'bz') 
			print ('obj = ' + str(obj))
		status, ret_bz = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'bz', i)
		check_status(status)
		if not status:
			self.bz = ret_bz
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'bphi') 
			print ('obj = ' + str(obj))
		status, ret_bphi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'bphi', i)
		check_status(status)
		if not status:
			self.bphi = ret_bphi
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vphi') 
			print ('obj = ' + str(obj))
		status, ret_vphi = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vphi', i)
		check_status(status)
		if not status:
			self.vphi = ret_vphi
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'vtheta') 
			print ('obj = ' + str(obj))
		status, ret_vtheta = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'vtheta', i)
		check_status(status)
		if not status:
			self.vtheta = ret_vtheta
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'rho_mass') 
			print ('obj = ' + str(obj))
		status, ret_rho_mass = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'rho_mass', i)
		check_status(status)
		if not status:
			self.rho_mass = ret_rho_mass
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'pressure') 
			print ('obj = ' + str(obj))
		status, ret_pressure = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'pressure', i)
		check_status(status)
		if not status:
			self.pressure = ret_pressure
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'temperature') 
			print ('obj = ' + str(obj))
		status, ret_temperature = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'temperature', i)
		check_status(status)
		if not status:
			self.temperature = ret_temperature

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstruct_arrayequilibrium_profiles_2dObj, run function putNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('putVect1DStringInObject : ' + cpopath + 'grid_type') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DStringInObject(self.idx, obj, cpopath + 'grid_type', i, self.grid_type)
		obj = self.grid.putNonTimedElt(path, cpopath + 'grid', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type profiles_2dstruct_arrayequilibrium_profiles_2dObj, run function getNonTimedElt') 
		cpopath = cpopath + '/'
		if (dev()):
			print ('getVect1DStringInObject : ' + cpopath + 'grid_type') 
			print ('obj = ' + str(obj))
		status, ret_grid_type = ull.getVect1DStringFromObject(self.idx, obj, cpopath + 'grid_type', i)
		check_status(status)
		if not status:
			self.grid_type = ret_grid_type
		self.grid.getNonTimedElt(path, cpopath + 'grid', i, obj)


class gridstructureequilibrium_profiles2d_grid:
	'''
	class gridstructureequilibrium_profiles2d_grid
	definition of the 2D grid

	Attributes:
	- dim1 : numpy.ndarray 1D with float
	   First dimension values; Time-dependent; Vector (ndim1) 
	- dim2 : numpy.ndarray 1D with float
	   Second dimension values; Time-dependent; Vector (ndim2) 
	- connect : numpy.ndarray 2D with int
	   In case of a finite elemnt representation, lists the points (3 for triangles, 4 for quadrangles) which define a finite element. In this case, ndim1=ndim2 and the value of grid_connect represents the index of the points in the list 1:ndim. E.g. : grid_connect(i,1:4) is a list of four integers [k1 k2 k3 k4] meaning that finite element #i is defined by the points (dim1(k1),dim2(k1)),(dim1(k2),dim2(k2)),(dim1(k3),dim2(k3)) and (dim1(k4),dim2(k4)); Time-dependent; Matrix of integers (nelement,4)
	'''

	def __init__(self, base_path_in='grid'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dim1 = numpy.zeros(0, numpy.float64, order='C')
		self.dim2 = numpy.zeros(0, numpy.float64, order='C')
		self.connect = numpy.zeros((0,0), numpy.int32, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class gridstructureequilibrium_profiles2d_grid\n'
		s = self.dim1.__str__()
		ret = ret + space + 'Attribute dim1\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim2.__str__()
		ret = ret + space + 'Attribute dim2\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.connect.__str__()
		ret = ret + space + 'Attribute connect\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructureequilibrium_profiles2d_grid, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dim1', numpy.array(self.dim1).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dim2', numpy.array(self.dim2).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DIntSlice(self.idx, path, cpopath + 'connect', numpy.array(self.connect).astype(numpy.int32), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructureequilibrium_profiles2d_grid, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dim1', numpy.array(self.dim1).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dim2', numpy.array(self.dim2).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DIntSlice(self.idx, path, cpopath + 'connect', numpy.array(self.connect).astype(numpy.int32))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructureequilibrium_profiles2d_grid, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructureequilibrium_profiles2d_grid, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dim1, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dim1', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dim1 = ret_dim1
			self.cpoTime = retTime
		status, ret_dim2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dim2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dim2 = ret_dim2
			self.cpoTime = retTime
		status, ret_connect, retTime = ull.getVect2DIntSlice(self.idx, path, cpopath + 'connect', inTime, interpolMode)
		check_status(status)
		if not status:
			self.connect = ret_connect
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructureequilibrium_profiles2d_grid, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, dim1List = ull.getVect2DDouble(self.idx, path, cpopath + 'dim1')
			if len(dim1List) == 0:
				dim1List = numpy.resize(dim1List, (0,nbslice))
			check_status(status)
			status, dim2List = ull.getVect2DDouble(self.idx, path, cpopath + 'dim2')
			if len(dim2List) == 0:
				dim2List = numpy.resize(dim2List, (0,nbslice))
			check_status(status)
			status, connectList = ull.getVect3DInt(self.idx, path, cpopath + 'connect')
			if len(connectList) == 0:
				connectList = numpy.resize(connectList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = gridstructureequilibrium_profiles2d_grid(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dim1 = dim1List[:,i]
				slice.dim2 = dim2List[:,i]
				slice.connect = connectList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructureequilibrium_profiles2d_gridObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dim1') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dim1', i, numpy.array(self.dim1).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dim2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dim2', i, numpy.array(self.dim2).astype(numpy.float64))
		if (dev()):
			print ('putVect2DIntInObject : ' + cpopath + 'connect') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DIntInObject(self.idx, obj, cpopath + 'connect', i, numpy.array(self.connect).astype(numpy.int32))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructureequilibrium_profiles2d_gridObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dim1') 
			print ('obj = ' + str(obj))
		status, ret_dim1 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dim1', i)
		check_status(status)
		if not status:
			self.dim1 = ret_dim1
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dim2') 
			print ('obj = ' + str(obj))
		status, ret_dim2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dim2', i)
		check_status(status)
		if not status:
			self.dim2 = ret_dim2
		if (dev()):
			print ('getVect2DIntInObject : ' + cpopath + 'connect') 
			print ('obj = ' + str(obj))
		status, ret_connect = ull.getVect2DIntFromObject(self.idx, obj, cpopath + 'connect', i)
		check_status(status)
		if not status:
			self.connect = ret_connect

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructureequilibrium_profiles2d_gridObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructureequilibrium_profiles2d_gridObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'dim1')
		ull.deleteData(self.idx, path, cpopath + 'dim2')
		ull.deleteData(self.idx, path, cpopath + 'connect')


class coord_sysstructurecoord_sys:
	'''
	class coord_sysstructurecoord_sys
	flux surface coordinate system on a square grid of flux and angle

	Attributes:
	- grid_type : str
	   Type of coordinate system
	- grid : class gridstructurereggrid
	   Regular grid definition; Time-dependent
	- jacobian : numpy.ndarray 2D with float
	   Jacobian of the coordinate system; Time-dependent; Matrix (ndim1, ndim2)
	- g_11 : numpy.ndarray 2D with float
	   metric coefficients g_11; g_ij=g^ij are contravariant metric tensor for the grid described by grid_type. Time-dependent; Matrix (ndim1, ndim2)
	- g_12 : numpy.ndarray 2D with float
	   metric coefficients g_12; g_ij=g^ij are contravariant metric tensor for the grid described by grid_type. Time-dependent; Matrix (ndim1, ndim2)
	- g_13 : numpy.ndarray 2D with float
	   metric coefficients g_13; g_ij=g^ij are contravariant metric tensor for the grid described by grid_type. Time-dependent; Matrix (ndim1, ndim2)
	- g_22 : numpy.ndarray 2D with float
	   metric coefficients g_22; g_ij=g^ij are contravariant metric tensor for the grid described by grid_type. Time-dependent; Matrix (ndim1, ndim2)
	- g_23 : numpy.ndarray 2D with float
	   metric coefficients g_23; g_ij=g^ij are contravariant metric tensor for the grid described by grid_type. Time-dependent; Matrix (ndim1, ndim2)
	- g_33 : numpy.ndarray 2D with float
	   metric coefficients g_33; g_ij=g^ij are contravariant metric tensor for the grid described by grid_type. Time-dependent; Matrix (ndim1, ndim2)
	- position : class positionstructurerz2D
	   R and Z position of grid points; Time-dependent; Matrix (ndim1, ndim2)
	'''

	def __init__(self, base_path_in='coord_sys'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.grid_type = ''
		self.grid = gridstructurereggrid('grid')
		self.jacobian = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_11 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_12 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_13 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_22 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_23 = numpy.zeros((0,0), numpy.float64, order='C')
		self.g_33 = numpy.zeros((0,0), numpy.float64, order='C')
		self.position = positionstructurerz2D('position')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class coord_sysstructurecoord_sys\n'
		ret = ret + space + 'Attribute grid_type: ' + str(self.grid_type) + '\n'
		ret = ret + space + 'Attribute grid\n ' + self.grid.__str__(depth+1)
		s = self.jacobian.__str__()
		ret = ret + space + 'Attribute jacobian\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g_11.__str__()
		ret = ret + space + 'Attribute g_11\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g_12.__str__()
		ret = ret + space + 'Attribute g_12\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g_13.__str__()
		ret = ret + space + 'Attribute g_13\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g_22.__str__()
		ret = ret + space + 'Attribute g_22\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g_23.__str__()
		ret = ret + space + 'Attribute g_23\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.g_33.__str__()
		ret = ret + space + 'Attribute g_33\n' + space + s.replace('\n', '\n'+space) + '\n'
		ret = ret + space + 'Attribute position\n ' + self.position.__str__(depth+1)
		return ret

	def setExpIdx(self, idx):
		self.idx = idx
		self.grid.setExpIdx(idx)
		self.position.setExpIdx(idx)

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coord_sysstructurecoord_sys, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.cpoTime = self.cpoTime
		self.grid.putSlice(path, cpopath)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'jacobian', numpy.array(self.jacobian).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g_11', numpy.array(self.g_11).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g_12', numpy.array(self.g_12).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g_13', numpy.array(self.g_13).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g_22', numpy.array(self.g_22).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g_23', numpy.array(self.g_23).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect2DDoubleSlice(self.idx, path, cpopath + 'g_33', numpy.array(self.g_33).astype(numpy.float64), self.cpoTime)
		check_status(status)
		self.position.cpoTime = self.cpoTime
		self.position.putSlice(path, cpopath)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coord_sysstructurecoord_sys, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		self.grid.replaceLastSlice(path, cpopath)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'jacobian', numpy.array(self.jacobian).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g_11', numpy.array(self.g_11).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g_12', numpy.array(self.g_12).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g_13', numpy.array(self.g_13).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g_22', numpy.array(self.g_22).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g_23', numpy.array(self.g_23).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect2DDoubleSlice(self.idx, path, cpopath + 'g_33', numpy.array(self.g_33).astype(numpy.float64))
		check_status(status)
		self.position.replaceLastSlice(path, cpopath)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type coord_sysstructurecoord_sys, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putString(self.idx, path, cpopath + 'grid_type', self.grid_type)
		check_status(status)
		self.grid.putNonTimed(path, cpopath)
		self.position.putNonTimed(path, cpopath)

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type coord_sysstructurecoord_sys, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_grid_type = ull.getString(self.idx, path, cpopath + 'grid_type')
		check_status(status)
		if not status:
			self.grid_type = ret_grid_type
		self.grid.getSlice(path, cpopath, inTime, interpolMode)
		status, ret_jacobian, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'jacobian', inTime, interpolMode)
		check_status(status)
		if not status:
			self.jacobian = ret_jacobian
			self.cpoTime = retTime
		status, ret_g_11, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g_11', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g_11 = ret_g_11
			self.cpoTime = retTime
		status, ret_g_12, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g_12', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g_12 = ret_g_12
			self.cpoTime = retTime
		status, ret_g_13, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g_13', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g_13 = ret_g_13
			self.cpoTime = retTime
		status, ret_g_22, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g_22', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g_22 = ret_g_22
			self.cpoTime = retTime
		status, ret_g_23, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g_23', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g_23 = ret_g_23
			self.cpoTime = retTime
		status, ret_g_33, retTime = ull.getVect2DDoubleSlice(self.idx, path, cpopath + 'g_33', inTime, interpolMode)
		check_status(status)
		if not status:
			self.g_33 = ret_g_33
			self.cpoTime = retTime
		self.position.getSlice(path, cpopath, inTime, interpolMode)

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type coord_sysstructurecoord_sys, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, grid_typeVal = ull.getString(self.idx, path, cpopath + 'grid_type')
			check_status(status)
			gridList = self.grid.build_non_resampled_data(path, cpopath, nbslice)
			status, jacobianList = ull.getVect3DDouble(self.idx, path, cpopath + 'jacobian')
			if len(jacobianList) == 0:
				jacobianList = numpy.resize(jacobianList, (0,0,nbslice))
			check_status(status)
			status, g_11List = ull.getVect3DDouble(self.idx, path, cpopath + 'g_11')
			if len(g_11List) == 0:
				g_11List = numpy.resize(g_11List, (0,0,nbslice))
			check_status(status)
			status, g_12List = ull.getVect3DDouble(self.idx, path, cpopath + 'g_12')
			if len(g_12List) == 0:
				g_12List = numpy.resize(g_12List, (0,0,nbslice))
			check_status(status)
			status, g_13List = ull.getVect3DDouble(self.idx, path, cpopath + 'g_13')
			if len(g_13List) == 0:
				g_13List = numpy.resize(g_13List, (0,0,nbslice))
			check_status(status)
			status, g_22List = ull.getVect3DDouble(self.idx, path, cpopath + 'g_22')
			if len(g_22List) == 0:
				g_22List = numpy.resize(g_22List, (0,0,nbslice))
			check_status(status)
			status, g_23List = ull.getVect3DDouble(self.idx, path, cpopath + 'g_23')
			if len(g_23List) == 0:
				g_23List = numpy.resize(g_23List, (0,0,nbslice))
			check_status(status)
			status, g_33List = ull.getVect3DDouble(self.idx, path, cpopath + 'g_33')
			if len(g_33List) == 0:
				g_33List = numpy.resize(g_33List, (0,0,nbslice))
			check_status(status)
			positionList = self.position.build_non_resampled_data(path, cpopath, nbslice)
			for i in range(nbslice):
				slice = coord_sysstructurecoord_sys(self.base_path)
				slice.setExpIdx(self.idx)
				slice.grid_type = grid_typeVal
				slice.grid = gridList[i]
				slice.jacobian = jacobianList[:,:,i]
				slice.g_11 = g_11List[:,:,i]
				slice.g_12 = g_12List[:,:,i]
				slice.g_13 = g_13List[:,:,i]
				slice.g_22 = g_22List[:,:,i]
				slice.g_23 = g_23List[:,:,i]
				slice.g_33 = g_33List[:,:,i]
				slice.position = positionList[i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coord_sysstructurecoord_sysObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		obj = self.grid.putTimedElt(path, cpopath + 'grid', i, obj)
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'jacobian') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'jacobian', i, numpy.array(self.jacobian).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g_11') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g_11', i, numpy.array(self.g_11).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g_12') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g_12', i, numpy.array(self.g_12).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g_13') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g_13', i, numpy.array(self.g_13).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g_22') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g_22', i, numpy.array(self.g_22).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g_23') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g_23', i, numpy.array(self.g_23).astype(numpy.float64))
		if (dev()):
			print ('putVect2DDoubleInObject : ' + cpopath + 'g_33') 
			print ('obj = ' + str(obj))
		obj = ull.putVect2DDoubleInObject(self.idx, obj, cpopath + 'g_33', i, numpy.array(self.g_33).astype(numpy.float64))
		obj = self.position.putTimedElt(path, cpopath + 'position', i, obj)
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coord_sysstructurecoord_sysObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		self.grid.getTimedElt(path, cpopath + 'grid', i, obj)
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'jacobian') 
			print ('obj = ' + str(obj))
		status, ret_jacobian = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'jacobian', i)
		check_status(status)
		if not status:
			self.jacobian = ret_jacobian
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g_11') 
			print ('obj = ' + str(obj))
		status, ret_g_11 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g_11', i)
		check_status(status)
		if not status:
			self.g_11 = ret_g_11
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g_12') 
			print ('obj = ' + str(obj))
		status, ret_g_12 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g_12', i)
		check_status(status)
		if not status:
			self.g_12 = ret_g_12
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g_13') 
			print ('obj = ' + str(obj))
		status, ret_g_13 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g_13', i)
		check_status(status)
		if not status:
			self.g_13 = ret_g_13
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g_22') 
			print ('obj = ' + str(obj))
		status, ret_g_22 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g_22', i)
		check_status(status)
		if not status:
			self.g_22 = ret_g_22
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g_23') 
			print ('obj = ' + str(obj))
		status, ret_g_23 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g_23', i)
		check_status(status)
		if not status:
			self.g_23 = ret_g_23
		if (dev()):
			print ('getVect2DDoubleInObject : ' + cpopath + 'g_33') 
			print ('obj = ' + str(obj))
		status, ret_g_33 = ull.getVect2DDoubleFromObject(self.idx, obj, cpopath + 'g_33', i)
		check_status(status)
		if not status:
			self.g_33 = ret_g_33
		self.position.getTimedElt(path, cpopath + 'position', i, obj)

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coord_sysstructurecoord_sysObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putStringInObject : ' + cpopath + 'grid_type') 
			print ('obj = ' + str(obj))
		obj = ull.putStringInObject(self.idx, obj, cpopath + 'grid_type', i, self.grid_type)
		obj = self.grid.putNonTimedElt(path, cpopath + 'grid', i, obj)
		obj = self.position.putNonTimedElt(path, cpopath + 'position', i, obj)
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type coord_sysstructurecoord_sysObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getStringInObject : ' + cpopath + 'grid_type') 
			print ('obj = ' + str(obj))
		status, ret_grid_type = ull.getStringFromObject(self.idx, obj, cpopath + 'grid_type', i)
		check_status(status)
		if not status:
			self.grid_type = ret_grid_type
		self.grid.getNonTimedElt(path, cpopath + 'grid', i, obj)
		self.position.getNonTimedElt(path, cpopath + 'position', i, obj)

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'grid_type')
		self.grid.deleteData(path, cpopath)
		ull.deleteData(self.idx, path, cpopath + 'jacobian')
		ull.deleteData(self.idx, path, cpopath + 'g_11')
		ull.deleteData(self.idx, path, cpopath + 'g_12')
		ull.deleteData(self.idx, path, cpopath + 'g_13')
		ull.deleteData(self.idx, path, cpopath + 'g_22')
		ull.deleteData(self.idx, path, cpopath + 'g_23')
		ull.deleteData(self.idx, path, cpopath + 'g_33')
		self.position.deleteData(path, cpopath)


class gridstructurereggrid:
	'''
	class gridstructurereggrid
	Regular grid definition; Time-dependent

	Attributes:
	- dim1 : numpy.ndarray 1D with float
	   First dimension values; Vector (ndim1) 
	- dim2 : numpy.ndarray 1D with float
	   Second dimension values; Vector (ndim2) 
	'''

	def __init__(self, base_path_in='grid'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.dim1 = numpy.zeros(0, numpy.float64, order='C')
		self.dim2 = numpy.zeros(0, numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class gridstructurereggrid\n'
		s = self.dim1.__str__()
		ret = ret + space + 'Attribute dim1\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.dim2.__str__()
		ret = ret + space + 'Attribute dim2\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurereggrid, run function putSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dim1', numpy.array(self.dim1).astype(numpy.float64), self.cpoTime)
		check_status(status)
		status = ull.putVect1DDoubleSlice(self.idx, path, cpopath + 'dim2', numpy.array(self.dim2).astype(numpy.float64), self.cpoTime)
		check_status(status)

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurereggrid, run function replaceLastSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dim1', numpy.array(self.dim1).astype(numpy.float64))
		check_status(status)
		status = ull.replaceLastVect1DDoubleSlice(self.idx, path, cpopath + 'dim2', numpy.array(self.dim2).astype(numpy.float64))
		check_status(status)

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurereggrid, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurereggrid, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		status, ret_dim1, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dim1', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dim1 = ret_dim1
			self.cpoTime = retTime
		status, ret_dim2, retTime = ull.getVect1DDoubleSlice(self.idx, path, cpopath + 'dim2', inTime, interpolMode)
		check_status(status)
		if not status:
			self.dim2 = ret_dim2
			self.cpoTime = retTime

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type gridstructurereggrid, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, dim1List = ull.getVect2DDouble(self.idx, path, cpopath + 'dim1')
			if len(dim1List) == 0:
				dim1List = numpy.resize(dim1List, (0,nbslice))
			check_status(status)
			status, dim2List = ull.getVect2DDouble(self.idx, path, cpopath + 'dim2')
			if len(dim2List) == 0:
				dim2List = numpy.resize(dim2List, (0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = gridstructurereggrid(self.base_path)
				slice.setExpIdx(self.idx)
				slice.dim1 = dim1List[:,i]
				slice.dim2 = dim2List[:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurereggridObj, run function putTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dim1') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dim1', i, numpy.array(self.dim1).astype(numpy.float64))
		if (dev()):
			print ('putVect1DDoubleInObject : ' + cpopath + 'dim2') 
			print ('obj = ' + str(obj))
		obj = ull.putVect1DDoubleInObject(self.idx, obj, cpopath + 'dim2', i, numpy.array(self.dim2).astype(numpy.float64))
		return obj

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurereggridObj, run function getTimedElt') 
		cpopath = cpopath + '/' 
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dim1') 
			print ('obj = ' + str(obj))
		status, ret_dim1 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dim1', i)
		check_status(status)
		if not status:
			self.dim1 = ret_dim1
		if (dev()):
			print ('getVect1DDoubleInObject : ' + cpopath + 'dim2') 
			print ('obj = ' + str(obj))
		status, ret_dim2 = ull.getVect1DDoubleFromObject(self.idx, obj, cpopath + 'dim2', i)
		check_status(status)
		if not status:
			self.dim2 = ret_dim2

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurereggridObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type gridstructurereggridObj, run function getNonTimedElt') 
		cpopath = cpopath + '/' 

	def deleteData(self, path, cpopath):
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		ull.deleteData(self.idx, path, cpopath + 'dim1')
		ull.deleteData(self.idx, path, cpopath + 'dim2')


class positionstructurerz2D:
	'''
	class positionstructurerz2D
	R and Z position of grid points; Time-dependent; Matrix (ndim1, ndim2)

	Attributes:
	- r : numpy.ndarray 2D with float
	   Major radius [m]
	- z : numpy.ndarray 2D with float
	   Altitude [m]
	'''

	def __init__(self, base_path_in='position'):
		self.base_path = base_path_in
		self.idx = EMPTY_INT
		self.cpoTime = EMPTY_DOUBLE
		self.r = numpy.zeros((0,0), numpy.float64, order='C')
		self.z = numpy.zeros((0,0), numpy.float64, order='C')

	def __str__(self, depth=0):
		space = depth*'\t'
		ret = space + 'class positionstructurerz2D\n'
		s = self.r.__str__()
		ret = ret + space + 'Attribute r\n' + space + s.replace('\n', '\n'+space) + '\n'
		s = self.z.__str__()
		ret = ret + space + 'Attribute z\n' + space + s.replace('\n', '\n'+space) + '\n'
		return ret

	def setExpIdx(self, idx):
		self.idx = idx

	def putSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz2D, run function putSlice') 
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

	def replaceLastSlice(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz2D, run function replaceLastSlice') 
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

	def putNonTimed(self, path, cpopath):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz2D, run function putNonTimed') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 

	def getSlice(self, path, cpopath, inTime, interpolMode):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz2D, run function getSlice') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
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

	def build_non_resampled_data(self, path, cpopath, nbslice):
		if (verb()):
			print ('field '+self.base_path+' of type positionstructurerz2D, run function build_non_resampled_data') 
		if cpopath=='':
			cpopath = self.base_path + '/'
		else:
			cpopath = cpopath + self.base_path + '/' 
		if (verb()):
			print ('path = ' + path + '  and cpopath = ' + cpopath) 
		list=[]
		if nbslice > 0:
			status, rList = ull.getVect3DDouble(self.idx, path, cpopath + 'r')
			if len(rList) == 0:
				rList = numpy.resize(rList, (0,0,nbslice))
			check_status(status)
			status, zList = ull.getVect3DDouble(self.idx, path, cpopath + 'z')
			if len(zList) == 0:
				zList = numpy.resize(zList, (0,0,nbslice))
			check_status(status)
			for i in range(nbslice):
				slice = positionstructurerz2D(self.base_path)
				slice.setExpIdx(self.idx)
				slice.r = rList[:,:,i]
				slice.z = zList[:,:,i]
				list.append(slice)
		else:
			print ('error, nbslice must be > 0 and got:' + str(nbslice))
		return list

	def putTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz2DObj, run function putTimedElt') 
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

	def getTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz2DObj, run function getTimedElt') 
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

	def putNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz2DObj, run function putNonTimedElt') 
		cpopath = cpopath + '/' 
		return obj

	def getNonTimedElt(self, path, cpopath, i, obj):
		if (verb()):
			print ('object of type positionstructurerz2DObj, run function getNonTimedElt') 
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
