from . import ual_low_level_wrapper as ull
from . import topinfo
from . import amns
from . import antennas
from . import bb_shield
from . import compositionc
from . import coredelta
from . import corefast
from . import coreneutrals
from . import coreimpur
from . import coreprof
from . import coresource
from . import coretransp
from . import cxdiag
from . import distribution
from . import distsource
from . import ecediag
from . import edge
from . import efcc
from . import equilibrium
from . import fusiondiag
from . import halphadiag
from . import heat_sources
from . import interfdiag
from . import ironmodel
from . import langmuirdiag
from . import launchs
from . import lithiumdiag
from . import mhd
from . import magdiag
from . import msediag
from . import nbi
from . import ntm
from . import neoclassic
from . import orbit
from . import pellets
from . import pfsystems
from . import polardiag
from . import power_conv
from . import reflectomet
from . import rfadiag
from . import sawteeth
from . import scenario
from . import solcurdiag
from . import temporary
from . import toroidfield
from . import tsdiag
from . import turbulence
from . import wall
from . import waves

class itm:
	def __init__(self, s=-1, r=-1, rs=-1, rr=-1):
		self.shot = s
		self.refShot = rs
		self.run = r
		self.refRun = rr
		self.treeName = 'euitm'
		self.connected = False
		self.expIdx = -1
		self.topinfo = topinfo.topinfo()
		self.amns = amns.amns()
		self.amnsArray = amns.amnsArray()
		self.antennas = antennas.antennas()
		self.antennasArray = antennas.antennasArray()
		self.bb_shield = bb_shield.bb_shield()
		self.bb_shieldArray = bb_shield.bb_shieldArray()
		self.compositionc = compositionc.compositionc()
		self.compositioncArray = compositionc.compositioncArray()
		self.coredelta = coredelta.coredelta()
		self.coredeltaArray = coredelta.coredeltaArray()
		self.corefast = corefast.corefast()
		self.corefastArray = corefast.corefastArray()
		self.coreneutrals = coreneutrals.coreneutrals()
		self.coreneutralsArray = coreneutrals.coreneutralsArray()
		self.coreimpur = coreimpur.coreimpur()
		self.coreimpurArray = coreimpur.coreimpurArray()
		self.coreprof = coreprof.coreprof()
		self.coreprofArray = coreprof.coreprofArray()
		self.coresource = coresource.coresource()
		self.coresourceArray = coresource.coresourceArray()
		self.coretransp = coretransp.coretransp()
		self.coretranspArray = coretransp.coretranspArray()
		self.cxdiag = cxdiag.cxdiag()
		self.cxdiagArray = cxdiag.cxdiagArray()
		self.distribution = distribution.distribution()
		self.distributionArray = distribution.distributionArray()
		self.distsource = distsource.distsource()
		self.distsourceArray = distsource.distsourceArray()
		self.ecediag = ecediag.ecediag()
		self.ecediagArray = ecediag.ecediagArray()
		self.edge = edge.edge()
		self.edgeArray = edge.edgeArray()
		self.efcc = efcc.efcc()
		self.efccArray = efcc.efccArray()
		self.equilibrium = equilibrium.equilibrium()
		self.equilibriumArray = equilibrium.equilibriumArray()
		self.fusiondiag = fusiondiag.fusiondiag()
		self.fusiondiagArray = fusiondiag.fusiondiagArray()
		self.halphadiag = halphadiag.halphadiag()
		self.halphadiagArray = halphadiag.halphadiagArray()
		self.heat_sources = heat_sources.heat_sources()
		self.heat_sourcesArray = heat_sources.heat_sourcesArray()
		self.interfdiag = interfdiag.interfdiag()
		self.interfdiagArray = interfdiag.interfdiagArray()
		self.ironmodel = ironmodel.ironmodel()
		self.ironmodelArray = ironmodel.ironmodelArray()
		self.langmuirdiag = langmuirdiag.langmuirdiag()
		self.langmuirdiagArray = langmuirdiag.langmuirdiagArray()
		self.launchs = launchs.launchs()
		self.launchsArray = launchs.launchsArray()
		self.lithiumdiag = lithiumdiag.lithiumdiag()
		self.lithiumdiagArray = lithiumdiag.lithiumdiagArray()
		self.mhd = mhd.mhd()
		self.mhdArray = mhd.mhdArray()
		self.magdiag = magdiag.magdiag()
		self.magdiagArray = magdiag.magdiagArray()
		self.msediag = msediag.msediag()
		self.msediagArray = msediag.msediagArray()
		self.nbi = nbi.nbi()
		self.nbiArray = nbi.nbiArray()
		self.ntm = ntm.ntm()
		self.ntmArray = ntm.ntmArray()
		self.neoclassic = neoclassic.neoclassic()
		self.neoclassicArray = neoclassic.neoclassicArray()
		self.orbit = orbit.orbit()
		self.orbitArray = orbit.orbitArray()
		self.pellets = pellets.pellets()
		self.pelletsArray = pellets.pelletsArray()
		self.pfsystems = pfsystems.pfsystems()
		self.pfsystemsArray = pfsystems.pfsystemsArray()
		self.polardiag = polardiag.polardiag()
		self.polardiagArray = polardiag.polardiagArray()
		self.power_conv = power_conv.power_conv()
		self.power_convArray = power_conv.power_convArray()
		self.reflectomet = reflectomet.reflectomet()
		self.reflectometArray = reflectomet.reflectometArray()
		self.rfadiag = rfadiag.rfadiag()
		self.rfadiagArray = rfadiag.rfadiagArray()
		self.sawteeth = sawteeth.sawteeth()
		self.sawteethArray = sawteeth.sawteethArray()
		self.scenario = scenario.scenario()
		self.scenarioArray = scenario.scenarioArray()
		self.solcurdiag = solcurdiag.solcurdiag()
		self.solcurdiagArray = solcurdiag.solcurdiagArray()
		self.temporary = temporary.temporary()
		self.temporaryArray = temporary.temporaryArray()
		self.toroidfield = toroidfield.toroidfield()
		self.toroidfieldArray = toroidfield.toroidfieldArray()
		self.tsdiag = tsdiag.tsdiag()
		self.tsdiagArray = tsdiag.tsdiagArray()
		self.turbulence = turbulence.turbulence()
		self.turbulenceArray = turbulence.turbulenceArray()
		self.wall = wall.wall()
		self.wallArray = wall.wallArray()
		self.waves = waves.waves()
		self.wavesArray = waves.wavesArray()

	def __str__(self, depth=0):
		space = ''
		for i in range(depth):
			space = space + '\t'

		ret = space + 'class itm\n'
		ret = ret + space + 'Shot=%d, Run=%d, RefShot%d RefRun=%d\n' % (self.shot, self.run, self.refShot, self.refRun)
		ret = ret + space + 'treeName=%s, connected=%d, expIdx=%d\n' % (self.treeName, self.connected, self.expIdx)
		ret = ret + space + 'Attribute topinfo\n' + self.topinfo.__str__(depth+1)
		ret = ret + space + 'Attribute amns\n' + self.amns.__str__(depth+1)
		ret = ret + space + 'Attribute amnsArray\n' + self.amnsArray.__str__(depth+1)
		ret = ret + space + 'Attribute antennas\n' + self.antennas.__str__(depth+1)
		ret = ret + space + 'Attribute antennasArray\n' + self.antennasArray.__str__(depth+1)
		ret = ret + space + 'Attribute bb_shield\n' + self.bb_shield.__str__(depth+1)
		ret = ret + space + 'Attribute bb_shieldArray\n' + self.bb_shieldArray.__str__(depth+1)
		ret = ret + space + 'Attribute compositionc\n' + self.compositionc.__str__(depth+1)
		ret = ret + space + 'Attribute compositioncArray\n' + self.compositioncArray.__str__(depth+1)
		ret = ret + space + 'Attribute coredelta\n' + self.coredelta.__str__(depth+1)
		ret = ret + space + 'Attribute coredeltaArray\n' + self.coredeltaArray.__str__(depth+1)
		ret = ret + space + 'Attribute corefast\n' + self.corefast.__str__(depth+1)
		ret = ret + space + 'Attribute corefastArray\n' + self.corefastArray.__str__(depth+1)
		ret = ret + space + 'Attribute coreneutrals\n' + self.coreneutrals.__str__(depth+1)
		ret = ret + space + 'Attribute coreneutralsArray\n' + self.coreneutralsArray.__str__(depth+1)
		ret = ret + space + 'Attribute coreimpur\n' + self.coreimpur.__str__(depth+1)
		ret = ret + space + 'Attribute coreimpurArray\n' + self.coreimpurArray.__str__(depth+1)
		ret = ret + space + 'Attribute coreprof\n' + self.coreprof.__str__(depth+1)
		ret = ret + space + 'Attribute coreprofArray\n' + self.coreprofArray.__str__(depth+1)
		ret = ret + space + 'Attribute coresource\n' + self.coresource.__str__(depth+1)
		ret = ret + space + 'Attribute coresourceArray\n' + self.coresourceArray.__str__(depth+1)
		ret = ret + space + 'Attribute coretransp\n' + self.coretransp.__str__(depth+1)
		ret = ret + space + 'Attribute coretranspArray\n' + self.coretranspArray.__str__(depth+1)
		ret = ret + space + 'Attribute cxdiag\n' + self.cxdiag.__str__(depth+1)
		ret = ret + space + 'Attribute cxdiagArray\n' + self.cxdiagArray.__str__(depth+1)
		ret = ret + space + 'Attribute distribution\n' + self.distribution.__str__(depth+1)
		ret = ret + space + 'Attribute distributionArray\n' + self.distributionArray.__str__(depth+1)
		ret = ret + space + 'Attribute distsource\n' + self.distsource.__str__(depth+1)
		ret = ret + space + 'Attribute distsourceArray\n' + self.distsourceArray.__str__(depth+1)
		ret = ret + space + 'Attribute ecediag\n' + self.ecediag.__str__(depth+1)
		ret = ret + space + 'Attribute ecediagArray\n' + self.ecediagArray.__str__(depth+1)
		ret = ret + space + 'Attribute edge\n' + self.edge.__str__(depth+1)
		ret = ret + space + 'Attribute edgeArray\n' + self.edgeArray.__str__(depth+1)
		ret = ret + space + 'Attribute efcc\n' + self.efcc.__str__(depth+1)
		ret = ret + space + 'Attribute efccArray\n' + self.efccArray.__str__(depth+1)
		ret = ret + space + 'Attribute equilibrium\n' + self.equilibrium.__str__(depth+1)
		ret = ret + space + 'Attribute equilibriumArray\n' + self.equilibriumArray.__str__(depth+1)
		ret = ret + space + 'Attribute fusiondiag\n' + self.fusiondiag.__str__(depth+1)
		ret = ret + space + 'Attribute fusiondiagArray\n' + self.fusiondiagArray.__str__(depth+1)
		ret = ret + space + 'Attribute halphadiag\n' + self.halphadiag.__str__(depth+1)
		ret = ret + space + 'Attribute halphadiagArray\n' + self.halphadiagArray.__str__(depth+1)
		ret = ret + space + 'Attribute heat_sources\n' + self.heat_sources.__str__(depth+1)
		ret = ret + space + 'Attribute heat_sourcesArray\n' + self.heat_sourcesArray.__str__(depth+1)
		ret = ret + space + 'Attribute interfdiag\n' + self.interfdiag.__str__(depth+1)
		ret = ret + space + 'Attribute interfdiagArray\n' + self.interfdiagArray.__str__(depth+1)
		ret = ret + space + 'Attribute ironmodel\n' + self.ironmodel.__str__(depth+1)
		ret = ret + space + 'Attribute ironmodelArray\n' + self.ironmodelArray.__str__(depth+1)
		ret = ret + space + 'Attribute langmuirdiag\n' + self.langmuirdiag.__str__(depth+1)
		ret = ret + space + 'Attribute langmuirdiagArray\n' + self.langmuirdiagArray.__str__(depth+1)
		ret = ret + space + 'Attribute launchs\n' + self.launchs.__str__(depth+1)
		ret = ret + space + 'Attribute launchsArray\n' + self.launchsArray.__str__(depth+1)
		ret = ret + space + 'Attribute lithiumdiag\n' + self.lithiumdiag.__str__(depth+1)
		ret = ret + space + 'Attribute lithiumdiagArray\n' + self.lithiumdiagArray.__str__(depth+1)
		ret = ret + space + 'Attribute mhd\n' + self.mhd.__str__(depth+1)
		ret = ret + space + 'Attribute mhdArray\n' + self.mhdArray.__str__(depth+1)
		ret = ret + space + 'Attribute magdiag\n' + self.magdiag.__str__(depth+1)
		ret = ret + space + 'Attribute magdiagArray\n' + self.magdiagArray.__str__(depth+1)
		ret = ret + space + 'Attribute msediag\n' + self.msediag.__str__(depth+1)
		ret = ret + space + 'Attribute msediagArray\n' + self.msediagArray.__str__(depth+1)
		ret = ret + space + 'Attribute nbi\n' + self.nbi.__str__(depth+1)
		ret = ret + space + 'Attribute nbiArray\n' + self.nbiArray.__str__(depth+1)
		ret = ret + space + 'Attribute ntm\n' + self.ntm.__str__(depth+1)
		ret = ret + space + 'Attribute ntmArray\n' + self.ntmArray.__str__(depth+1)
		ret = ret + space + 'Attribute neoclassic\n' + self.neoclassic.__str__(depth+1)
		ret = ret + space + 'Attribute neoclassicArray\n' + self.neoclassicArray.__str__(depth+1)
		ret = ret + space + 'Attribute orbit\n' + self.orbit.__str__(depth+1)
		ret = ret + space + 'Attribute orbitArray\n' + self.orbitArray.__str__(depth+1)
		ret = ret + space + 'Attribute pellets\n' + self.pellets.__str__(depth+1)
		ret = ret + space + 'Attribute pelletsArray\n' + self.pelletsArray.__str__(depth+1)
		ret = ret + space + 'Attribute pfsystems\n' + self.pfsystems.__str__(depth+1)
		ret = ret + space + 'Attribute pfsystemsArray\n' + self.pfsystemsArray.__str__(depth+1)
		ret = ret + space + 'Attribute polardiag\n' + self.polardiag.__str__(depth+1)
		ret = ret + space + 'Attribute polardiagArray\n' + self.polardiagArray.__str__(depth+1)
		ret = ret + space + 'Attribute power_conv\n' + self.power_conv.__str__(depth+1)
		ret = ret + space + 'Attribute power_convArray\n' + self.power_convArray.__str__(depth+1)
		ret = ret + space + 'Attribute reflectomet\n' + self.reflectomet.__str__(depth+1)
		ret = ret + space + 'Attribute reflectometArray\n' + self.reflectometArray.__str__(depth+1)
		ret = ret + space + 'Attribute rfadiag\n' + self.rfadiag.__str__(depth+1)
		ret = ret + space + 'Attribute rfadiagArray\n' + self.rfadiagArray.__str__(depth+1)
		ret = ret + space + 'Attribute sawteeth\n' + self.sawteeth.__str__(depth+1)
		ret = ret + space + 'Attribute sawteethArray\n' + self.sawteethArray.__str__(depth+1)
		ret = ret + space + 'Attribute scenario\n' + self.scenario.__str__(depth+1)
		ret = ret + space + 'Attribute scenarioArray\n' + self.scenarioArray.__str__(depth+1)
		ret = ret + space + 'Attribute solcurdiag\n' + self.solcurdiag.__str__(depth+1)
		ret = ret + space + 'Attribute solcurdiagArray\n' + self.solcurdiagArray.__str__(depth+1)
		ret = ret + space + 'Attribute temporary\n' + self.temporary.__str__(depth+1)
		ret = ret + space + 'Attribute temporaryArray\n' + self.temporaryArray.__str__(depth+1)
		ret = ret + space + 'Attribute toroidfield\n' + self.toroidfield.__str__(depth+1)
		ret = ret + space + 'Attribute toroidfieldArray\n' + self.toroidfieldArray.__str__(depth+1)
		ret = ret + space + 'Attribute tsdiag\n' + self.tsdiag.__str__(depth+1)
		ret = ret + space + 'Attribute tsdiagArray\n' + self.tsdiagArray.__str__(depth+1)
		ret = ret + space + 'Attribute turbulence\n' + self.turbulence.__str__(depth+1)
		ret = ret + space + 'Attribute turbulenceArray\n' + self.turbulenceArray.__str__(depth+1)
		ret = ret + space + 'Attribute wall\n' + self.wall.__str__(depth+1)
		ret = ret + space + 'Attribute wallArray\n' + self.wallArray.__str__(depth+1)
		ret = ret + space + 'Attribute waves\n' + self.waves.__str__(depth+1)
		ret = ret + space + 'Attribute wavesArray\n' + self.wavesArray.__str__(depth+1)
		return ret

	def __del__(self):
		if self.expIdx != -1:
			ull.euitm_close(self.expIdx)

	def setShot(self, inShot):
		self.shot = inShot

	def setRun(self, inRun):
		self.run = inRun

	def setRefShot(self, inRefShot):
		self.refShot = inRefShot

	def setRefNum(self, inRefRun):
		self.refRun = inRefRun

	def setTreeName(self, inTreeName):
		self.treeName = inTreeName

	def getShot(self):
		return self.shot

	def getRun(self):
		return self.run

	def getRefShot(self):
		return self.refShot

	def getRefRun(self):
		return self.refRun

	def getTreeName(self):
		return self.treeName

	def isConnected(self):
		return self.connected

	def create(self):
		status, idx = ull.euitm_create(self.treeName, self.shot, self.run, self.refShot, self.refRun)
		self.__update_cpo(status, idx)

	def create_env(self, user, tokamak, version):
		status, idx = ull.euitm_create_env(self.treeName, self.shot, self.run, self.refShot, self.refRun, user, tokamak, version)
		self.__update_cpo(status, idx)

	def create_hdf5(self):
		status, idx = ull.euitm_create_hdf5(self.treeName, self.shot, self.run, self.refShot, self.refRun)
		self.__update_cpo(status, idx)

	def open(self):
		status, idx = ull.euitm_open(self.treeName, self.shot, self.run)
		self.__update_cpo(status, idx)

	def open_env(self, user, tokamak, version):
		status, idx = ull.euitm_open_env(self.treeName, self.shot, self.run, user, tokamak, version)
		self.__update_cpo(status, idx)

	def open_hdf5(self):
		status, idx = ull.euitm_open_hdf5(self.treeName, self.shot, self.run)
		self.__update_cpo(status, idx)

	def __update_cpo(self, status, idx):
		if status != 0:
			print ('Error opening euitm shot ' + str(self.shot) + ' run ' + str(self.run) + ': ' + ull.euitm_last_errmsg())
		else:
			self.expIdx = idx
			self.connected = True
			self.topinfo.setExpIdx(idx)
			self.amns.setExpIdx(idx)
			self.amnsArray.setExpIdx(idx)
			self.antennas.setExpIdx(idx)
			self.antennasArray.setExpIdx(idx)
			self.bb_shield.setExpIdx(idx)
			self.bb_shieldArray.setExpIdx(idx)
			self.compositionc.setExpIdx(idx)
			self.compositioncArray.setExpIdx(idx)
			self.coredelta.setExpIdx(idx)
			self.coredeltaArray.setExpIdx(idx)
			self.corefast.setExpIdx(idx)
			self.corefastArray.setExpIdx(idx)
			self.coreneutrals.setExpIdx(idx)
			self.coreneutralsArray.setExpIdx(idx)
			self.coreimpur.setExpIdx(idx)
			self.coreimpurArray.setExpIdx(idx)
			self.coreprof.setExpIdx(idx)
			self.coreprofArray.setExpIdx(idx)
			self.coresource.setExpIdx(idx)
			self.coresourceArray.setExpIdx(idx)
			self.coretransp.setExpIdx(idx)
			self.coretranspArray.setExpIdx(idx)
			self.cxdiag.setExpIdx(idx)
			self.cxdiagArray.setExpIdx(idx)
			self.distribution.setExpIdx(idx)
			self.distributionArray.setExpIdx(idx)
			self.distsource.setExpIdx(idx)
			self.distsourceArray.setExpIdx(idx)
			self.ecediag.setExpIdx(idx)
			self.ecediagArray.setExpIdx(idx)
			self.edge.setExpIdx(idx)
			self.edgeArray.setExpIdx(idx)
			self.efcc.setExpIdx(idx)
			self.efccArray.setExpIdx(idx)
			self.equilibrium.setExpIdx(idx)
			self.equilibriumArray.setExpIdx(idx)
			self.fusiondiag.setExpIdx(idx)
			self.fusiondiagArray.setExpIdx(idx)
			self.halphadiag.setExpIdx(idx)
			self.halphadiagArray.setExpIdx(idx)
			self.heat_sources.setExpIdx(idx)
			self.heat_sourcesArray.setExpIdx(idx)
			self.interfdiag.setExpIdx(idx)
			self.interfdiagArray.setExpIdx(idx)
			self.ironmodel.setExpIdx(idx)
			self.ironmodelArray.setExpIdx(idx)
			self.langmuirdiag.setExpIdx(idx)
			self.langmuirdiagArray.setExpIdx(idx)
			self.launchs.setExpIdx(idx)
			self.launchsArray.setExpIdx(idx)
			self.lithiumdiag.setExpIdx(idx)
			self.lithiumdiagArray.setExpIdx(idx)
			self.mhd.setExpIdx(idx)
			self.mhdArray.setExpIdx(idx)
			self.magdiag.setExpIdx(idx)
			self.magdiagArray.setExpIdx(idx)
			self.msediag.setExpIdx(idx)
			self.msediagArray.setExpIdx(idx)
			self.nbi.setExpIdx(idx)
			self.nbiArray.setExpIdx(idx)
			self.ntm.setExpIdx(idx)
			self.ntmArray.setExpIdx(idx)
			self.neoclassic.setExpIdx(idx)
			self.neoclassicArray.setExpIdx(idx)
			self.orbit.setExpIdx(idx)
			self.orbitArray.setExpIdx(idx)
			self.pellets.setExpIdx(idx)
			self.pelletsArray.setExpIdx(idx)
			self.pfsystems.setExpIdx(idx)
			self.pfsystemsArray.setExpIdx(idx)
			self.polardiag.setExpIdx(idx)
			self.polardiagArray.setExpIdx(idx)
			self.power_conv.setExpIdx(idx)
			self.power_convArray.setExpIdx(idx)
			self.reflectomet.setExpIdx(idx)
			self.reflectometArray.setExpIdx(idx)
			self.rfadiag.setExpIdx(idx)
			self.rfadiagArray.setExpIdx(idx)
			self.sawteeth.setExpIdx(idx)
			self.sawteethArray.setExpIdx(idx)
			self.scenario.setExpIdx(idx)
			self.scenarioArray.setExpIdx(idx)
			self.solcurdiag.setExpIdx(idx)
			self.solcurdiagArray.setExpIdx(idx)
			self.temporary.setExpIdx(idx)
			self.temporaryArray.setExpIdx(idx)
			self.toroidfield.setExpIdx(idx)
			self.toroidfieldArray.setExpIdx(idx)
			self.tsdiag.setExpIdx(idx)
			self.tsdiagArray.setExpIdx(idx)
			self.turbulence.setExpIdx(idx)
			self.turbulenceArray.setExpIdx(idx)
			self.wall.setExpIdx(idx)
			self.wallArray.setExpIdx(idx)
			self.waves.setExpIdx(idx)
			self.wavesArray.setExpIdx(idx)

	def close(self):
		if (self.expIdx != -1):
			ull.euitm_close(self.expIdx)
			self.connected = False
			self.expIdx = -1

	def enableMemCache(self):
		if (self.expIdx != -1):
			ull.euitm_enable_mem_cache(self.expIdx)

	def disableMemCache(self):
		if (self.expIdx != -1):
			ull.euitm_disable_mem_cache(self.expIdx)

	def discardMemCache(self):
		if (self.expIdx != -1):
			ull.euitm_discard_mem_cache(self.expIdx)

	def flushMemCache(self):
		if (self.expIdx != -1):
			ull.euitm_flush_mem_cache(self.expIdx)

	def getTimes(self, path):
		if not self.connected:
			return -1,None
		timeList = []
		status, nbslice = ull.beginCPOGet(self.expIdx, path, True)
		if status:
			return status,timeList
		if nbslice > 0:
			status,timeList = ull.getVect1DDouble(self.expIdx, path, 'time')
		status2 = ull.endCPOGet(self.expIdx, path)
		return status,timeList

	def getNbSlices(self, path):
		status, timeList = self.getTimes(path)
		if not status:
			return len(timeList)
		else:
			return status

	def isCPOExisting(self, path):
		size = self.getNbSlices(path)
		if size>0:
			return True
		else:
			return False

	def getAllCPONames(self):
		itmlist = dir(self)
		cpolist = []
		for l in itmlist:
			t = type(getattr(self, l))
			if t==types.InstanceType and l.find("Array")==-1:
				cpolist.append(l)
		return cpolist

	def getExistingCPONames(self):
		cpolist = self.getAllCPONames()
		cpoexistlist = []
		for cponame in cpolist:
			cpoinst = getattr(self, cponame)
			maxocc = cpoinst.getMaxOccurrences()
			if self.isCPOExisting(cponame):
				cpoexistlist.append(cponame)
			for occ in range(maxocc):
				cpofullname = cponame+"/"+str(occ+1)
				if self.isCPOExisting(cpofullname):
					cpoexistlist.append(cpofullname)
		return cpoexistlist
