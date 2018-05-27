package breeze.numerics.constants

import scala.collection.immutable.HashMap
import scala.util.matching.Regex

/**Object encapsulating CODATA2010 constants.
  *
  * @author ktakagaki
  * @date 3/12/14.
  */
object Database {

  /** Return a vector containing all database keys.
    */
  def list() = databaseHM.keys.toVector

  /** Look up the value of a specific entry.
    * Name must be an exact match with the entry.
    */
  def value(string: String) = databaseHM(string)._1

  /** Look up the value of entries with name matching the given regex expression.
    *
    * @param regex enter a regex object (i.e. """regex expression""".r)
    * @return returns a vector of (String, Double) objects, which contain the full entry name (String) and the value (Double)
    */
  def value(regex: Regex) = databaseHM.collect {
    case (key: String, (value: Double, _, _))
        if regex.findFirstIn(key).nonEmpty =>
      (key, value)
  }

  /** Look up the uncertainty of a specific entry.
    * Name must be an exact match with the entry.
    */
  def uncertainty(string: String) = databaseHM(string)._2

  /** Look up the uncertainty of entries with name matching the given regex expression.
    *
    * @param regex enter a regex object (i.e. """regex expression""".r)
    * @return returns a vector of (String, Double) objects, which contain the full entry name (String) and the uncertainty (Double)
    */
  def uncertainty(regex: Regex) = databaseHM.collect {
    case (key: String, (_, uncert: Double, _))
        if regex.findFirstIn(key).nonEmpty =>
      (key, uncert)
  }

  /** Look up the unit of a specific entry.
    * Name must be an exact match with the entry.
    */
  def unit(string: String) = databaseHM(string)._3

  /** Look up the unit of entries with name matching the given regex expression.
    *
    * @param regex enter a regex object (i.e. """regex expression""".r)
    * @return returns a vector of (String, Double) objects, which contain the full entry name (String) and the uncertainty (Double)
    */
  def unit(regex: Regex) = databaseHM.collect {
    case (key: String, (_, _, unit: String))
        if regex.findFirstIn(key).nonEmpty =>
      (key, unit)
  }

  // <editor-fold defaultstate="collapsed" desc=" CODATA2010 Database HashMap ">

  private lazy val databaseHM = HashMap[String, (Double, Double, String)](
    (
      """{220} lattice spacing of silicon""",
      (1.920155714e-10, 3.2e-18, """m""")),
    ("""alpha particle-electron mass ratio""", (7294.29953610, 2.9e-6, """""")),
    ("""alpha particle mass""", (6.64465675e-27, 2.9e-34, """kg""")),
    (
      """alpha particle mass energy equivalent""",
      (5.97191967e-10, 2.6e-17, """J""")),
    (
      """alpha particle mass energy equivalent in MeV""",
      (3727.379240, 0.000082, """MeV""")),
    ("""alpha particle mass in u""", (4.0015061791250, 6.2e-11, """u""")),
    (
      """alpha particle molar mass""",
      (0.0040015061791250, 6.2e-14, """kg mol^-1""")),
    ("""alpha particle-proton mass ratio""", (3.97259968933, 3.6e-10, """""")),
    ("""Angstrom star""", (1.00001495e-10, 9.0e-17, """m""")),
    ("""atomic mass constant""", (1.660538921e-27, 7.3e-35, """kg""")),
    (
      """atomic mass constant energy equivalent""",
      (1.492417954e-10, 6.6e-18, """J""")),
    (
      """atomic mass constant energy equivalent in MeV""",
      (931.4940610, 0.000021, """MeV""")),
    (
      """atomic mass unit-electron volt relationship""",
      (9.31494061e8, 21.0, """eV""")),
    (
      """atomic mass unit-hartree relationship""",
      (3.4231776845e7, 0.024, """E_h""")),
    (
      """atomic mass unit-hertz relationship""",
      (2.2523427168e23, 1.6e14, """Hz""")),
    (
      """atomic mass unit-inverse meter relationship""",
      (7.5130066042e14, 530000.0, """m^-1""")),
    (
      """atomic mass unit-joule relationship""",
      (1.492417954e-10, 6.6e-18, """J""")),
    (
      """atomic mass unit-kelvin relationship""",
      (1.08095408e13, 9.8e6, """K""")),
    (
      """atomic mass unit-kilogram relationship""",
      (1.660538921e-27, 7.3e-35, """kg""")),
    (
      """atomic unit of 1st hyperpolarizability""",
      (3.206361449e-53, 7.1e-61, """C^3 m^3 J^-2""")),
    (
      """atomic unit of 2nd hyperpolarizability""",
      (6.23538054e-65, 2.8e-72, """C^4 m^4 J^-3""")),
    ("""atomic unit of action""", (1.054571726e-34, 4.7e-42, """J s""")),
    ("""atomic unit of charge""", (1.602176565e-19, 3.5e-27, """C""")),
    (
      """atomic unit of charge density""",
      (1.081202338e12, 24000.0, """C m^-3""")),
    ("""atomic unit of current""", (0.006623617950, 1.5e-10, """A""")),
    (
      """atomic unit of electric dipole mom.""",
      (8.47835326e-30, 1.9e-37, """C m""")),
    (
      """atomic unit of electric field""",
      (5.14220652e11, 11000.0, """V m^-1""")),
    (
      """atomic unit of electric field gradient""",
      (9.717362e21, 2.1e14, """V m^-2""")),
    (
      """atomic unit of electric polarizability""",
      (1.6487772754e-41, 1.6e-50, """C^2 m^2 J^-1""")),
    ("""atomic unit of electric potential""", (27.21138505, 6.0e-7, """V""")),
    (
      """atomic unit of electric quadrupole mom.""",
      (4.486551331e-40, 9.9e-48, """C m^2""")),
    ("""atomic unit of energy""", (4.35974434e-18, 1.9e-25, """J""")),
    ("""atomic unit of force""", (8.23872278e-8, 3.6e-15, """N""")),
    ("""atomic unit of length""", (5.2917721092e-11, 1.7e-20, """m""")),
    (
      """atomic unit of mag. dipole mom.""",
      (1.854801936e-23, 4.1e-31, """J T^-1""")),
    ("""atomic unit of mag. flux density""", (235051.7464, 0.0052, """T""")),
    (
      """atomic unit of magnetizability""",
      (7.891036607e-29, 1.3e-37, """J T^-2""")),
    ("""atomic unit of mass""", (9.10938291e-31, 4.0e-38, """kg""")),
    ("""atomic unit of mom.um""", (1.99285174e-24, 8.8e-32, """kg m s^-1""")),
    ("""atomic unit of permittivity""", (1.112650056e-10, 0d, """F m^-1""")),
    ("""atomic unit of time""", (2.418884326502e-17, 1.2e-28, """s""")),
    ("""atomic unit of velocity""", (2.18769126379e6, 0.00071, """m s^-1""")),
    ("""Avogadro constant""", (6.02214129e23, 2.7e16, """mol^-1""")),
    ("""Bohr magneton""", (9.27400968e-24, 2.0e-31, """J T^-1""")),
    ("""Bohr magneton in eV/T""", (0.0000578838180660, 3.8e-14, """eV T^-1""")),
    ("""Bohr magneton in Hz/T""", (1.399624555e10, 310.0, """Hz T^-1""")),
    (
      """Bohr magneton in inverse meters per tesla""",
      (46.68644980, 1.0e-6, """m^-1 T^-1""")),
    ("""Bohr magneton in K/T""", (0.67171388, 6.1e-7, """K T^-1""")),
    ("""Bohr radius""", (5.2917721092e-11, 1.7e-20, """m""")),
    ("""Boltzmann constant""", (1.3806488e-23, 1.3e-29, """J K^-1""")),
    (
      """Boltzmann constant in eV/K""",
      (0.000086173324, 7.8e-11, """eV K^-1""")),
    ("""Boltzmann constant in Hz/K""", (2.0836618e10, 19000.0, """Hz K^-1""")),
    (
      """Boltzmann constant in inverse meters per kelvin""",
      (69.503476, 0.000063, """m^-1 K^-1""")),
    ("""characteristic impedance of vacuum""", (376.730313461, 0d, """ohm""")),
    ("""classical electron radius""", (2.8179403267e-15, 2.7e-24, """m""")),
    ("""Compton wavelength""", (2.4263102389e-12, 1.6e-21, """m""")),
    ("""Compton wavelength over 2 pi""", (3.86159268e-13, 2.5e-22, """m""")),
    ("""conductance quantum""", (0.0000774809173460, 2.5e-14, """S""")),
    (
      """conventional value of Josephson constant""",
      (4.835979e14, 0d, """Hz V^-1""")),
    (
      """conventional value of von Klitzing constant""",
      (25812.807, 0d, """ohm""")),
    ("""Cu x unit""", (1.00207697e-13, 2.8e-20, """m""")),
    (
      """deuteron-electron mag. mom. ratio""",
      (-0.0004664345537, 3.9e-12, """""")),
    ("""deuteron-electron mass ratio""", (3670.48296520, 1.5e-6, """""")),
    ("""deuteron g factor""", (0.8574382308, 7.2e-9, """""")),
    ("""deuteron mag. mom.""", (4.33073489e-27, 1.0e-34, """J T^-1""")),
    (
      """deuteron mag. mom. to Bohr magneton ratio""",
      (0.0004669754556, 3.9e-12, """""")),
    (
      """deuteron mag. mom. to nuclear magneton ratio""",
      (0.8574382308, 7.2e-9, """""")),
    ("""deuteron mass""", (3.34358348e-27, 1.5e-34, """kg""")),
    ("""deuteron mass energy equivalent""", (3.00506297e-10, 1.3e-17, """J""")),
    (
      """deuteron mass energy equivalent in MeV""",
      (1875.612859, 0.000041, """MeV""")),
    ("""deuteron mass in u""", (2.0135532127120, 7.7e-11, """u""")),
    ("""deuteron molar mass""", (0.0020135532127120, 7.7e-14, """kg mol^-1""")),
    ("""deuteron-neutron mag. mom. ratio""", (-0.44820652, 1.1e-7, """""")),
    ("""deuteron-proton mag. mom. ratio""", (0.3070122070, 2.4e-9, """""")),
    ("""deuteron-proton mass ratio""", (1.99900750097, 1.8e-10, """""")),
    ("""deuteron rms charge radius""", (2.1424e-15, 2.1e-18, """m""")),
    ("""electric constant""", (8.854187817e-12, 0d, """F m^-1""")),
    (
      """electron charge to mass quotient""",
      (-1.758820088e11, 3900.0, """C kg^-1""")),
    ("""electron-deuteron mag. mom. ratio""", (-2143.923498, 0.000018, """""")),
    ("""electron-deuteron mass ratio""", (0.000272443710950, 1.1e-13, """""")),
    ("""electron g factor""", (-2.002319304361530, 5.3e-13, """""")),
    ("""electron gyromag. ratio""", (1.760859708e11, 3900.0, """s^-1 T^-1""")),
    (
      """electron gyromag. ratio over 2 pi""",
      (28024.95266, 0.00062, """MHz T^-1""")),
    ("""electron-helion mass ratio""", (0.000181954307610, 1.7e-13, """""")),
    ("""electron mag. mom.""", (-9.2847643e-24, 2.1e-31, """J T^-1""")),
    ("""electron mag. mom. anomaly""", (0.00115965218076, 2.7e-13, """""")),
    (
      """electron mag. mom. to Bohr magneton ratio""",
      (-1.001159652180760, 2.7e-13, """""")),
    (
      """electron mag. mom. to nuclear magneton ratio""",
      (-1838.28197090, 7.5e-7, """""")),
    ("""electron mass""", (9.10938291e-31, 4.0e-38, """kg""")),
    ("""electron mass energy equivalent""", (8.18710506e-14, 3.6e-21, """J""")),
    (
      """electron mass energy equivalent in MeV""",
      (0.5109989280, 1.1e-8, """MeV""")),
    ("""electron mass in u""", (0.000548579909460, 2.2e-13, """u""")),
    ("""electron molar mass""", (5.4857990946e-7, 2.2e-16, """kg mol^-1""")),
    ("""electron-muon mag. mom. ratio""", (206.7669896, 5.2e-6, """""")),
    ("""electron-muon mass ratio""", (0.004836331660, 1.2e-10, """""")),
    ("""electron-neutron mag. mom. ratio""", (960.92050, 0.00023, """""")),
    ("""electron-neutron mass ratio""", (0.000543867344610, 3.2e-13, """""")),
    ("""electron-proton mag. mom. ratio""", (-658.2106848, 5.4e-6, """""")),
    ("""electron-proton mass ratio""", (0.000544617021780, 2.2e-13, """""")),
    ("""electron-tau mass ratio""", (0.000287592, 2.6e-8, """""")),
    (
      """electron to alpha particle mass ratio""",
      (0.000137093355578, 5.5e-14, """""")),
    (
      """electron to shielded helion mag. mom. ratio""",
      (864.0582570, 1.0e-5, """""")),
    (
      """electron to shielded proton mag. mom. ratio""",
      (-658.2275971, 7.2e-6, """""")),
    ("""electron-triton mass ratio""", (0.000181920006530, 1.7e-13, """""")),
    ("""electron volt""", (1.602176565e-19, 3.5e-27, """J""")),
    (
      """electron volt-atomic mass unit relationship""",
      (1.07354415e-9, 2.4e-17, """u""")),
    (
      """electron volt-hartree relationship""",
      (0.03674932379, 8.1e-10, """E_h""")),
    ("""electron volt-hertz relationship""", (2.417989348e14, 5.3e6, """Hz""")),
    (
      """electron volt-inverse meter relationship""",
      (806554.4290, 0.018, """m^-1""")),
    (
      """electron volt-joule relationship""",
      (1.602176565e-19, 3.5e-27, """J""")),
    ("""electron volt-kelvin relationship""", (11604.519, 0.011, """K""")),
    (
      """electron volt-kilogram relationship""",
      (1.782661845e-36, 3.9e-44, """kg""")),
    ("""elementary charge""", (1.602176565e-19, 3.5e-27, """C""")),
    ("""elementary charge over h""", (2.417989348e14, 5.3e6, """A J^-1""")),
    ("""Faraday constant""", (96485.33650, 0.0021, """C mol^-1""")),
    (
      """Faraday constant for conventional electric current""",
      (96485.33210, 0.0043, """C_90 mol^-1""")),
    ("""Fermi coupling constant""", (0.000011663640, 5.0e-11, """GeV^-2""")),
    ("""fine-structure constant""", (0.00729735256980, 2.4e-12, """""")),
    ("""first radiation constant""", (3.74177153e-16, 1.7e-23, """W m^2""")),
    (
      """first radiation constant for spectral radiance""",
      (1.191042869e-16, 5.3e-24, """W m^2 sr^-1""")),
    (
      """hartree-atomic mass unit relationship""",
      (2.9212623246e-8, 2.1e-17, """u""")),
    ("""hartree-electron volt relationship""", (27.21138505, 6.0e-7, """eV""")),
    ("""Hartree energy""", (4.35974434e-18, 1.9e-25, """J""")),
    ("""Hartree energy in eV""", (27.21138505, 6.0e-7, """eV""")),
    ("""hartree-hertz relationship""", (6.579683920729e15, 33000.0, """Hz""")),
    (
      """hartree-inverse meter relationship""",
      (2.194746313708e7, 0.00011, """m^-1""")),
    ("""hartree-joule relationship""", (4.35974434e-18, 1.9e-25, """J""")),
    ("""hartree-kelvin relationship""", (315775.04, 0.29, """K""")),
    ("""hartree-kilogram relationship""", (4.85086979e-35, 2.1e-42, """kg""")),
    ("""helion-electron mass ratio""", (5495.88527540, 5.0e-6, """""")),
    ("""helion g factor""", (-4.255250613, 5.0e-8, """""")),
    ("""helion mag. mom.""", (-1.074617486e-26, 2.7e-34, """J T^-1""")),
    (
      """helion mag. mom. to Bohr magneton ratio""",
      (-0.001158740958, 1.4e-11, """""")),
    (
      """helion mag. mom. to nuclear magneton ratio""",
      (-2.127625306, 2.5e-8, """""")),
    ("""helion mass""", (5.00641234e-27, 2.2e-34, """kg""")),
    ("""helion mass energy equivalent""", (4.49953902e-10, 2.0e-17, """J""")),
    (
      """helion mass energy equivalent in MeV""",
      (2808.391482, 0.000062, """MeV""")),
    ("""helion mass in u""", (3.01493224680, 2.5e-9, """u""")),
    ("""helion molar mass""", (0.00301493224680, 2.5e-12, """kg mol^-1""")),
    ("""helion-proton mass ratio""", (2.99315267070, 2.5e-9, """""")),
    (
      """hertz-atomic mass unit relationship""",
      (4.4398216689e-24, 3.1e-33, """u""")),
    (
      """hertz-electron volt relationship""",
      (4.135667516e-15, 9.1e-23, """eV""")),
    (
      """hertz-hartree relationship""",
      (1.5198298460045e-16, 7.6e-28, """E_h""")),
    ("""hertz-inverse meter relationship""", (3.335640951e-9, 0d, """m^-1""")),
    ("""hertz-joule relationship""", (6.62606957e-34, 2.9e-41, """J""")),
    ("""hertz-kelvin relationship""", (4.7992434e-11, 4.4e-17, """K""")),
    ("""hertz-kilogram relationship""", (7.37249668e-51, 3.3e-58, """kg""")),
    ("""inverse fine-structure constant""", (137.035999074, 4.4e-8, """""")),
    (
      """inverse meter-atomic mass unit relationship""",
      (1.3310250512e-15, 9.4e-25, """u""")),
    (
      """inverse meter-electron volt relationship""",
      (1.23984193e-6, 2.7e-14, """eV""")),
    (
      """inverse meter-hartree relationship""",
      (4.556335252755e-8, 2.3e-19, """E_h""")),
    ("""inverse meter-hertz relationship""", (2.99792458e8, 0d, """Hz""")),
    (
      """inverse meter-joule relationship""",
      (1.986445684e-25, 8.8e-33, """J""")),
    ("""inverse meter-kelvin relationship""", (0.014387770, 1.3e-8, """K""")),
    (
      """inverse meter-kilogram relationship""",
      (2.210218902e-42, 9.8e-50, """kg""")),
    ("""inverse of conductance quantum""", (12906.4037217, 4.2e-6, """ohm""")),
    ("""Josephson constant""", (4.8359787e14, 1.1e7, """Hz V^-1""")),
    ("""joule-atomic mass unit relationship""", (6.70053585e9, 300.0, """u""")),
    ("""joule-electron volt relationship""", (6.24150934e18, 1.4e11, """eV""")),
    ("""joule-hartree relationship""", (2.29371248e17, 1.0e10, """E_h""")),
    ("""joule-hertz relationship""", (1.509190311e33, 6.7e25, """Hz""")),
    (
      """joule-inverse meter relationship""",
      (5.03411701e24, 2.2e17, """m^-1""")),
    ("""joule-kelvin relationship""", (7.2429716e22, 6.6e16, """K""")),
    ("""joule-kilogram relationship""", (1.112650056e-17, 0d, """kg""")),
    (
      """kelvin-atomic mass unit relationship""",
      (9.2510868e-14, 8.4e-20, """u""")),
    (
      """kelvin-electron volt relationship""",
      (0.000086173324, 7.8e-11, """eV""")),
    ("""kelvin-hartree relationship""", (3.1668114e-6, 2.9e-12, """E_h""")),
    ("""kelvin-hertz relationship""", (2.0836618e10, 19000.0, """Hz""")),
    (
      """kelvin-inverse meter relationship""",
      (69.503476, 0.000063, """m^-1""")),
    ("""kelvin-joule relationship""", (1.3806488e-23, 1.3e-29, """J""")),
    ("""kelvin-kilogram relationship""", (1.536179e-40, 1.4e-46, """kg""")),
    (
      """kilogram-atomic mass unit relationship""",
      (6.02214129e26, 2.7e19, """u""")),
    (
      """kilogram-electron volt relationship""",
      (5.60958885e35, 1.2e28, """eV""")),
    ("""kilogram-hartree relationship""", (2.061485968e34, 9.1e26, """E_h""")),
    ("""kilogram-hertz relationship""", (1.356392608e50, 6.0e42, """Hz""")),
    (
      """kilogram-inverse meter relationship""",
      (4.52443873e41, 2.0e34, """m^-1""")),
    ("""kilogram-joule relationship""", (8.987551787e16, 0d, """J""")),
    ("""kilogram-kelvin relationship""", (6.5096582e39, 5.9e33, """K""")),
    ("""lattice parameter of silicon""", (5.431020504e-10, 8.9e-18, """m""")),
    (
      """Loschmidt constant (273.15 K, 100 kPa)""",
      (2.6516462e25, 2.4e19, """m^-3""")),
    (
      """Loschmidt constant (273.15 K, 101.325 kPa)""",
      (2.6867805e25, 2.4e19, """m^-3""")),
    ("""mag. constant""", (1.2566370614e-6, 0d, """N A^-2""")),
    ("""mag. flux quantum""", (2.067833758e-15, 4.6e-23, """Wb""")),
    ("""molar gas constant""", (8.3144621, 7.5e-6, """J mol^-1 K^-1""")),
    ("""molar mass constant""", (0.0010, 0d, """kg mol^-1""")),
    ("""molar mass of carbon-12""", (0.012, 0d, """kg mol^-1""")),
    (
      """molar Planck constant""",
      (3.9903127176e-10, 2.8e-19, """J s mol^-1""")),
    (
      """molar Planck constant times c""",
      (0.119626565779, 8.4e-11, """J m mol^-1""")),
    (
      """molar volume of ideal gas (273.15 K, 100 kPa)""",
      (0.022710953, 2.1e-8, """m^3 mol^-1""")),
    (
      """molar volume of ideal gas (273.15 K, 101.325 kPa)""",
      (0.022413968, 2.0e-8, """m^3 mol^-1""")),
    (
      """molar volume of silicon""",
      (0.00001205883301, 8.0e-13, """m^3 mol^-1""")),
    ("""Mo x unit""", (1.00209952e-13, 5.3e-20, """m""")),
    ("""muon Compton wavelength""", (1.173444103e-14, 3.0e-22, """m""")),
    (
      """muon Compton wavelength over 2 pi""",
      (1.867594294e-15, 4.7e-23, """m""")),
    ("""muon-electron mass ratio""", (206.7682843, 5.2e-6, """""")),
    ("""muon g factor""", (-2.00233184180, 1.3e-9, """""")),
    ("""muon mag. mom.""", (-4.49044807e-26, 1.5e-33, """J T^-1""")),
    ("""muon mag. mom. anomaly""", (0.001165920910, 6.3e-10, """""")),
    (
      """muon mag. mom. to Bohr magneton ratio""",
      (-0.004841970440, 1.2e-10, """""")),
    (
      """muon mag. mom. to nuclear magneton ratio""",
      (-8.890596970, 2.2e-7, """""")),
    ("""muon mass""", (1.883531475e-28, 9.6e-36, """kg""")),
    ("""muon mass energy equivalent""", (1.692833667e-11, 8.6e-19, """J""")),
    (
      """muon mass energy equivalent in MeV""",
      (105.6583715, 3.5e-6, """MeV""")),
    ("""muon mass in u""", (0.1134289267, 2.9e-9, """u""")),
    ("""muon molar mass""", (0.0001134289267, 2.9e-12, """kg mol^-1""")),
    ("""muon-neutron mass ratio""", (0.1124545177, 2.8e-9, """""")),
    ("""muon-proton mag. mom. ratio""", (-3.183345107, 8.4e-8, """""")),
    ("""muon-proton mass ratio""", (0.1126095272, 2.8e-9, """""")),
    ("""muon-tau mass ratio""", (0.0594649, 5.4e-6, """""")),
    ("""natural unit of action""", (1.054571726e-34, 4.7e-42, """J s""")),
    (
      """natural unit of action in eV s""",
      (6.58211928e-16, 1.5e-23, """eV s""")),
    ("""natural unit of energy""", (8.18710506e-14, 3.6e-21, """J""")),
    ("""natural unit of energy in MeV""", (0.5109989280, 1.1e-8, """MeV""")),
    ("""natural unit of length""", (3.86159268e-13, 2.5e-22, """m""")),
    ("""natural unit of mass""", (9.10938291e-31, 4.0e-38, """kg""")),
    ("""natural unit of mom.um""", (2.73092429e-22, 1.2e-29, """kg m s^-1""")),
    (
      """natural unit of mom.um in MeV/c""",
      (0.5109989280, 1.1e-8, """MeV/c""")),
    ("""natural unit of time""", (1.28808866833e-21, 8.3e-31, """s""")),
    ("""natural unit of velocity""", (2.99792458e8, 0d, """m s^-1""")),
    ("""neutron Compton wavelength""", (1.3195909068e-15, 1.1e-24, """m""")),
    (
      """neutron Compton wavelength over 2 pi""",
      (2.1001941568e-16, 1.7e-25, """m""")),
    ("""neutron-electron mag. mom. ratio""", (0.001040668820, 2.5e-10, """""")),
    ("""neutron-electron mass ratio""", (1838.68366050, 1.1e-6, """""")),
    ("""neutron g factor""", (-3.826085450, 9.0e-7, """""")),
    ("""neutron gyromag. ratio""", (1.83247179e8, 43.0, """s^-1 T^-1""")),
    (
      """neutron gyromag. ratio over 2 pi""",
      (29.16469430, 6.9e-6, """MHz T^-1""")),
    ("""neutron mag. mom.""", (-9.6623647e-27, 2.3e-33, """J T^-1""")),
    (
      """neutron mag. mom. to Bohr magneton ratio""",
      (-0.001041875630, 2.5e-10, """""")),
    (
      """neutron mag. mom. to nuclear magneton ratio""",
      (-1.913042720, 4.5e-7, """""")),
    ("""neutron mass""", (1.674927351e-27, 7.4e-35, """kg""")),
    ("""neutron mass energy equivalent""", (1.505349631e-10, 6.6e-18, """J""")),
    (
      """neutron mass energy equivalent in MeV""",
      (939.5653790, 0.000021, """MeV""")),
    ("""neutron mass in u""", (1.008664916, 4.3e-10, """u""")),
    ("""neutron molar mass""", (0.001008664916, 4.3e-13, """kg mol^-1""")),
    ("""neutron-muon mass ratio""", (8.8924840, 2.2e-7, """""")),
    ("""neutron-proton mag. mom. ratio""", (-0.68497934, 1.6e-7, """""")),
    ("""neutron-proton mass difference""", (2.30557392e-30, 7.6e-37, """""")),
    (
      """neutron-proton mass difference energy equivalent""",
      (2.0721465e-13, 6.8e-20, """""")),
    (
      """neutron-proton mass difference energy equivalent in MeV""",
      (1.293332170, 4.2e-7, """""")),
    (
      """neutron-proton mass difference in u""",
      (0.001388449190, 4.5e-10, """""")),
    ("""neutron-proton mass ratio""", (1.00137841917, 4.5e-10, """""")),
    ("""neutron-tau mass ratio""", (0.528790, 0.000048, """""")),
    (
      """neutron to shielded proton mag. mom. ratio""",
      (-0.68499694, 1.6e-7, """""")),
    (
      """Newtonian constant of gravitation""",
      (6.67384e-11, 8.0e-15, """m^3 kg^-1 s^-2""")),
    (
      """Newtonian constant of gravitation over h-bar c""",
      (6.70837e-39, 8.0e-43, """(GeV/c^2)^-2""")),
    ("""nuclear magneton""", (5.05078353e-27, 1.1e-34, """J T^-1""")),
    ("""nuclear magneton in eV/T""", (3.1524512605e-8, 2.2e-17, """eV T^-1""")),
    (
      """nuclear magneton in inverse meters per tesla""",
      (0.02542623527, 5.6e-10, """m^-1 T^-1""")),
    ("""nuclear magneton in K/T""", (0.00036582682, 3.3e-10, """K T^-1""")),
    ("""nuclear magneton in MHz/T""", (7.622593570, 1.7e-7, """MHz T^-1""")),
    ("""Planck constant""", (6.62606957e-34, 2.9e-41, """J s""")),
    ("""Planck constant in eV s""", (4.135667516e-15, 9.1e-23, """eV s""")),
    ("""Planck constant over 2 pi""", (1.054571726e-34, 4.7e-42, """J s""")),
    (
      """Planck constant over 2 pi in eV s""",
      (6.58211928e-16, 1.5e-23, """eV s""")),
    (
      """Planck constant over 2 pi times c in MeV fm""",
      (197.3269718, 4.4e-6, """MeV fm""")),
    ("""Planck length""", (1.616199e-35, 9.7e-40, """m""")),
    ("""Planck mass""", (2.17651e-8, 1.3e-12, """kg""")),
    (
      """Planck mass energy equivalent in GeV""",
      (1.220932e19, 7.3e14, """GeV""")),
    ("""Planck temperature""", (1.416833e32, 8.5e27, """K""")),
    ("""Planck time""", (5.39106e-44, 3.2e-48, """s""")),
    ("""proton charge to mass quotient""", (9.57883358e7, 2.1, """C kg^-1""")),
    ("""proton Compton wavelength""", (1.32140985623e-15, 9.4e-25, """m""")),
    (
      """proton Compton wavelength over 2 pi""",
      (2.1030891047e-16, 1.5e-25, """m""")),
    ("""proton-electron mass ratio""", (1836.15267245, 7.5e-7, """""")),
    ("""proton g factor""", (5.585694713, 4.6e-8, """""")),
    ("""proton gyromag. ratio""", (2.675222005e8, 6.3, """s^-1 T^-1""")),
    (
      """proton gyromag. ratio over 2 pi""",
      (42.57748060, 1.0e-6, """MHz T^-1""")),
    ("""proton mag. mom.""", (1.410606743e-26, 3.3e-34, """J T^-1""")),
    (
      """proton mag. mom. to Bohr magneton ratio""",
      (0.001521032210, 1.2e-11, """""")),
    (
      """proton mag. mom. to nuclear magneton ratio""",
      (2.792847356, 2.3e-8, """""")),
    ("""proton mag. shielding correction""", (0.0000256940, 1.4e-8, """""")),
    ("""proton mass""", (1.672621777e-27, 7.4e-35, """kg""")),
    ("""proton mass energy equivalent""", (1.503277484e-10, 6.6e-18, """J""")),
    (
      """proton mass energy equivalent in MeV""",
      (938.2720460, 0.000021, """MeV""")),
    ("""proton mass in u""", (1.0072764668120, 9.0e-11, """u""")),
    ("""proton molar mass""", (0.0010072764668120, 9.0e-14, """kg mol^-1""")),
    ("""proton-muon mass ratio""", (8.880243310, 2.2e-7, """""")),
    ("""proton-neutron mag. mom. ratio""", (-1.459898060, 3.4e-7, """""")),
    ("""proton-neutron mass ratio""", (0.998623478260, 4.5e-10, """""")),
    ("""proton rms charge radius""", (8.7750e-16, 5.1e-18, """m""")),
    ("""proton-tau mass ratio""", (0.528063, 0.000048, """""")),
    ("""quantum of circulation""", (0.0003636947552, 2.4e-13, """m^2 s^-1""")),
    (
      """quantum of circulation times 2""",
      (0.0007273895104, 4.7e-13, """m^2 s^-1""")),
    ("""Rydberg constant""", (1.0973731568539e7, 0.000055, """m^-1""")),
    (
      """Rydberg constant times c in Hz""",
      (3.289841960364e15, 17000.0, """Hz""")),
    ("""Rydberg constant times hc in eV""", (13.60569253, 3.0e-7, """eV""")),
    ("""Rydberg constant times hc in J""", (2.179872171e-18, 9.6e-26, """J""")),
    (
      """Sackur-Tetrode constant (1 K, 100 kPa)""",
      (-1.1517078, 2.3e-6, """""")),
    (
      """Sackur-Tetrode constant (1 K, 101.325 kPa)""",
      (-1.1648708, 2.3e-6, """""")),
    ("""second radiation constant""", (0.014387770, 1.3e-8, """m K""")),
    (
      """shielded helion gyromag. ratio""",
      (2.037894659e8, 5.1, """s^-1 T^-1""")),
    (
      """shielded helion gyromag. ratio over 2 pi""",
      (32.43410084, 8.1e-7, """MHz T^-1""")),
    (
      """shielded helion mag. mom.""",
      (-1.074553044e-26, 2.7e-34, """J T^-1""")),
    (
      """shielded helion mag. mom. to Bohr magneton ratio""",
      (-0.001158671471, 1.4e-11, """""")),
    (
      """shielded helion mag. mom. to nuclear magneton ratio""",
      (-2.127497718, 2.5e-8, """""")),
    (
      """shielded helion to proton mag. mom. ratio""",
      (-0.7617665580, 1.1e-8, """""")),
    (
      """shielded helion to shielded proton mag. mom. ratio""",
      (-0.7617861313, 3.3e-9, """""")),
    (
      """shielded proton gyromag. ratio""",
      (2.675153268e8, 6.6, """s^-1 T^-1""")),
    (
      """shielded proton gyromag. ratio over 2 pi""",
      (42.57638660, 1.0e-6, """MHz T^-1""")),
    ("""shielded proton mag. mom.""", (1.410570499e-26, 3.5e-34, """J T^-1""")),
    (
      """shielded proton mag. mom. to Bohr magneton ratio""",
      (0.001520993128, 1.7e-11, """""")),
    (
      """shielded proton mag. mom. to nuclear magneton ratio""",
      (2.792775598, 3.0e-8, """""")),
    ("""speed of light in vacuum""", (2.99792458e8, 0d, """m s^-1""")),
    ("""standard acceleration of gravity""", (9.80665, 0d, """m s^-2""")),
    ("""standard atmosphere""", (101325.0, 0d, """Pa""")),
    ("""standard-state pressure""", (100000.0, 0d, """Pa""")),
    (
      """Stefan-Boltzmann constant""",
      (5.670373e-8, 2.1e-13, """W m^-2 K^-4""")),
    ("""tau Compton wavelength""", (6.97787e-16, 6.3e-20, """m""")),
    ("""tau Compton wavelength over 2 pi""", (1.11056e-16, 1.0e-20, """m""")),
    ("""tau-electron mass ratio""", (3477.15, 0.31, """""")),
    ("""tau mass""", (3.16747e-27, 2.9e-31, """kg""")),
    ("""tau mass energy equivalent""", (2.84678e-10, 2.6e-14, """J""")),
    ("""tau mass energy equivalent in MeV""", (1776.82, 0.16, """MeV""")),
    ("""tau mass in u""", (1.90749, 0.00017, """u""")),
    ("""tau molar mass""", (0.00190749, 1.7e-7, """kg mol^-1""")),
    ("""tau-muon mass ratio""", (16.8167, 0.0015, """""")),
    ("""tau-neutron mass ratio""", (1.89111, 0.00017, """""")),
    ("""tau-proton mass ratio""", (1.89372, 0.00017, """""")),
    ("""Thomson cross section""", (6.652458734e-29, 1.3e-37, """m^2""")),
    ("""triton-electron mass ratio""", (5496.92152670, 5.0e-6, """""")),
    ("""triton g factor""", (5.957924896, 7.6e-8, """""")),
    ("""triton mag. mom.""", (1.504609447e-26, 3.8e-34, """J T^-1""")),
    (
      """triton mag. mom. to Bohr magneton ratio""",
      (0.001622393657, 2.1e-11, """""")),
    (
      """triton mag. mom. to nuclear magneton ratio""",
      (2.978962448, 3.8e-8, """""")),
    ("""triton mass""", (5.0073563e-27, 2.2e-34, """kg""")),
    ("""triton mass energy equivalent""", (4.50038741e-10, 2.0e-17, """J""")),
    (
      """triton mass energy equivalent in MeV""",
      (2808.921005, 0.000062, """MeV""")),
    ("""triton mass in u""", (3.01550071340, 2.5e-9, """u""")),
    ("""triton molar mass""", (0.00301550071340, 2.5e-12, """kg mol^-1""")),
    ("""triton-proton mass ratio""", (2.99371703080, 2.5e-9, """""")),
    ("""unified atomic mass unit""", (1.660538921e-27, 7.3e-35, """kg""")),
    ("""von Klitzing constant""", (25812.8074434, 8.4e-6, """ohm""")),
    ("""weak mixing angle""", (0.2223, 0.0021, """""")),
    (
      """Wien frequency displacement law constant""",
      (5.8789254e10, 53000.0, """Hz K^-1""")),
    (
      """Wien wavelength displacement law constant""",
      (0.0028977721, 2.6e-9, """m K"""))
  )

  // </editor-fold>

}
