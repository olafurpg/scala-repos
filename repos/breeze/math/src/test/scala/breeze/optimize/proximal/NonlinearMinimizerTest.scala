package breeze.optimize.proximal

import breeze.linalg.{sum, DenseMatrix, norm, DenseVector}
import breeze.optimize.proximal.Constraint._
import breeze.optimize.OptimizeTestBase
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import breeze.numerics._

import org.scalatest.Matchers

@RunWith(classOf[JUnitRunner])
class NonlinearMinimizerTest extends OptimizeTestBase with Matchers {
  val n = 5
  val H = new DenseMatrix(
    n,
    n,
    Array(1.8984250861699135, 0.5955576666769438, -1.484430453342902,
      -1.0434994471390804, -3.675310432634351, 0.5955576666769438,
      0.9090751938470876, -2.146380947361661, -0.13037609428980368,
      -0.40639564652095117, -1.484430453342902, -2.146380947361661,
      10.262733520770384, -6.097698907163584, 2.29625304115155,
      -1.0434994471390804, -0.13037609428980368, -6.097698907163584,
      27.775920405610677, -5.574220233644466, -3.675310432634351,
      -0.40639564652095117, 2.29625304115155, -5.574220233644466,
      12.21329172136971)
  )
  val f = DenseVector(
    -1.2320199653150048,
    -0.14220655875869606,
    0.38477404739124765,
    -0.3480575854151014,
    -0.4729810900829228)

  val cost = QuadraticMinimizer.Cost(H, f :* (-1.0))
  val init = DenseVector.zeros[Double](n)

  test("Nonlinear Minimization with Identity constraint compared to BFGS") {
    init := 0.0
    val x = H \ f

    init := 0.0
    val nlResult = NonlinearMinimizer(n, IDENTITY, 0.0).minimize(cost, init)
    assert(norm(x - nlResult, inf) < 1e-4)
  }

  test("Nonlinear Minimization with positivity constraint compared to Octave") {
    val ata = new DenseMatrix[Double](
      5,
      5,
      Array(4.377, -3.531, -1.306, -0.139, 3.418, -3.531, 4.344, 0.934, 0.305,
        -2.140, -1.306, 0.934, 2.644, -0.203, -0.170, -0.139, 0.305, -0.203,
        5.883, 1.428, 3.418, -2.140, -0.170, 1.428, 4.684)
    )

    val atb = DenseVector(-1.632, 2.115, 1.094, -1.025, -0.636)
    val goodx = DenseVector(0.13025, 0.54506, 0.2874, 0.0, 0.028628)
    val nlResult = NonlinearMinimizer(n, POSITIVE, 0.0).minimizeAndReturnState(
      QuadraticMinimizer.Cost(ata, atb :* (-1.0)),
      DenseVector.zeros[Double](n))
    println(s"Positivity projection iter ${nlResult.iter}")
    assert(norm(nlResult.x - goodx, 2) < 1e-3)
  }

  test("Nonlinear Minimization with positivity proximal compared to Octave") {
    val ata = new DenseMatrix[Double](
      5,
      5,
      Array(4.377, -3.531, -1.306, -0.139, 3.418, -3.531, 4.344, 0.934, 0.305,
        -2.140, -1.306, 0.934, 2.644, -0.203, -0.170, -0.139, 0.305, -0.203,
        5.883, 1.428, 3.418, -2.140, -0.170, 1.428, 4.684)
    )

    val atb = DenseVector(-1.632, 2.115, 1.094, -1.025, -0.636)
    val goodx = DenseVector(0.13025, 0.54506, 0.2874, 0.0, 0.028628)
    val nlResult = new NonlinearMinimizer(ProjectPos()).minimizeAndReturnState(
      QuadraticMinimizer.Cost(ata, atb :* (-1.0)),
      DenseVector.zeros[Double](n))
    println(s"Positivity proximal iter ${nlResult.iter}")
    assert(norm(nlResult.z - goodx, 2) < 1e-3)
  }

  test(
    "Nonlinear Minimization with bounds constraint compared to QuadraticMinimizer") {
    init := 0.0
    val gold = QuadraticMinimizer(n, BOX).minimize(H, f :* (-1.0))
    val nlResult =
      NonlinearMinimizer(n, BOX, 0.0).minimizeAndReturnState(cost, init)
    println(s"Bounds projection iter ${nlResult.iter}")
    assert(norm(nlResult.x - gold) < 1e-4)
  }

  test(
    "Nonlinear Minimization with bounds proximal compared to QuadraticMinimizer") {
    init := 0.0
    val gold = QuadraticMinimizer(n, BOX).minimize(H, f :* (-1.0))
    val lb = DenseVector.zeros[Double](n)
    val ub = DenseVector.ones[Double](n)
    val nlResult = new NonlinearMinimizer(ProjectBox(lb, ub))
      .minimizeAndReturnState(cost, init)
    println(s"Bounds proximal iter ${nlResult.iter}")
    assert(norm(nlResult.z - gold) < 1e-4)
  }

  test("Nonlinear Minimization with probability simplex compared to Octave") {
    val Hml = new DenseMatrix(
      25,
      25,
      Array(112.647378, 44.962984, 49.127829, 43.708389, 43.333008, 46.345827,
        49.581542, 42.991226, 43.999341, 41.336724, 42.879992, 46.896465,
        41.778920, 46.333559, 51.168782, 44.800998, 43.735417, 42.672057,
        40.024492, 48.793499, 48.696170, 45.870016, 46.398093, 44.305791,
        41.863013, 44.962984, 107.202825, 44.218178, 38.585858, 36.606830,
        41.783275, 44.631314, 40.883821, 37.948817, 34.908843, 38.356328,
        43.642467, 36.213124, 38.760314, 43.317775, 36.803445, 41.905953,
        40.238334, 42.325769, 45.853665, 46.601722, 40.668861, 49.084078,
        39.292553, 35.781804, 49.127829, 44.218178, 118.264304, 40.139032,
        43.741591, 49.387932, 45.558785, 40.793703, 46.136010, 41.839393,
        39.544248, 43.161644, 43.361811, 43.832852, 50.572459, 42.036961,
        47.251940, 45.273068, 42.842437, 49.323737, 52.125739, 45.831747,
        49.466716, 44.762183, 41.930313, 43.708389, 38.585858, 40.139032,
        94.937989, 36.562570, 41.628404, 38.604965, 39.080500, 37.267530,
        34.291272, 34.891704, 39.216238, 35.970491, 40.733288, 41.872521,
        35.825264, 38.144457, 41.368293, 40.751910, 41.123673, 41.930358,
        41.002915, 43.099168, 36.018699, 33.646602, 43.333008, 36.606830,
        43.741591, 36.562570, 105.764912, 42.799031, 38.215171, 42.193565,
        38.708056, 39.448031, 37.882184, 40.172339, 40.625192, 39.015338,
        36.433413, 40.848178, 36.480813, 41.439981, 40.797598, 40.325652,
        38.599119, 42.727171, 39.382845, 41.535989, 41.518779, 46.345827,
        41.783275, 49.387932, 41.628404, 42.799031, 114.691992, 43.015599,
        42.688570, 42.722905, 38.675192, 38.377970, 44.656183, 39.087805,
        45.443516, 50.585268, 40.949970, 41.920556, 43.711898, 41.463472,
        51.248836, 46.869144, 45.178199, 45.709593, 42.402465, 44.097412,
        49.581542, 44.631314, 45.558785, 38.604965, 38.215171, 43.015599,
        114.667896, 40.966284, 37.748084, 39.496813, 40.534741, 42.770125,
        40.628678, 41.194251, 47.837969, 44.596875, 43.448257, 43.291878,
        39.005953, 50.493111, 46.296591, 43.449036, 48.798961, 42.877859,
        37.055014, 42.991226, 40.883821, 40.793703, 39.080500, 42.193565,
        42.688570, 40.966284, 106.632656, 37.640927, 37.181799, 40.065085,
        38.978761, 36.014753, 38.071494, 41.081064, 37.981693, 41.821252,
        42.773603, 39.293957, 38.600491, 43.761301, 42.294750, 42.410289,
        40.266469, 39.909538, 43.999341, 37.948817, 46.136010, 37.267530,
        38.708056, 42.722905, 37.748084, 37.640927, 102.747189, 34.574727,
        36.525241, 39.839891, 36.297838, 42.756496, 44.673874, 38.350523,
        40.330611, 42.288511, 39.472844, 45.617102, 44.692618, 41.194977,
        41.284030, 39.580938, 42.382268, 41.336724, 34.908843, 41.839393,
        34.291272, 39.448031, 38.675192, 39.496813, 37.181799, 34.574727,
        94.205550, 37.583319, 38.504211, 36.376976, 34.239351, 39.060978,
        37.515228, 37.079566, 37.317791, 38.046576, 36.112222, 39.406838,
        39.258432, 36.347136, 38.927619, 41.604838, 42.879992, 38.356328,
        39.544248, 34.891704, 37.882184, 38.377970, 40.534741, 40.065085,
        36.525241, 37.583319, 98.109622, 39.428284, 37.518381, 39.659011,
        38.477483, 40.547021, 42.678061, 42.279104, 41.515782, 43.478416,
        45.003800, 42.433639, 42.757336, 35.814356, 39.017848, 46.896465,
        43.642467, 43.161644, 39.216238, 40.172339, 44.656183, 42.770125,
        38.978761, 39.839891, 38.504211, 39.428284, 103.478446, 39.984358,
        40.587958, 44.490750, 40.600474, 40.698368, 42.296794, 41.567854,
        47.002908, 43.922434, 43.479144, 44.291425, 43.352951, 42.613649,
        41.778920, 36.213124, 43.361811, 35.970491, 40.625192, 39.087805,
        40.628678, 36.014753, 36.297838, 36.376976, 37.518381, 39.984358,
        99.799628, 38.027891, 44.268308, 36.202204, 39.921811, 38.668774,
        36.832286, 45.833218, 43.228963, 36.833273, 44.787401, 38.176476,
        39.062471, 46.333559, 38.760314, 43.832852, 40.733288, 39.015338,
        45.443516, 41.194251, 38.071494, 42.756496, 34.239351, 39.659011,
        40.587958, 38.027891, 114.304283, 46.958354, 39.636801, 40.927870,
        49.118094, 43.093642, 50.196436, 45.535041, 43.087415, 48.540036,
        35.942528, 37.962886, 51.168782, 43.317775, 50.572459, 41.872521,
        36.433413, 50.585268, 47.837969, 41.081064, 44.673874, 39.060978,
        38.477483, 44.490750, 44.268308, 46.958354, 122.935323, 39.948695,
        46.801841, 44.455283, 40.160668, 54.193098, 49.678271, 41.834745,
        47.227606, 42.214571, 42.598524, 44.800998, 36.803445, 42.036961,
        35.825264, 40.848178, 40.949970, 44.596875, 37.981693, 38.350523,
        37.515228, 40.547021, 40.600474, 36.202204, 39.636801, 39.948695,
        97.365126, 40.163209, 39.177628, 38.935283, 41.465246, 40.962743,
        40.533287, 43.367907, 38.723316, 36.312733, 43.735417, 41.905953,
        47.251940, 38.144457, 36.480813, 41.920556, 43.448257, 41.821252,
        40.330611, 37.079566, 42.678061, 40.698368, 39.921811, 40.927870,
        46.801841, 40.163209, 110.416786, 46.843429, 41.834126, 46.788801,
        46.983780, 43.511429, 47.291825, 40.023523, 40.581819, 42.672057,
        40.238334, 45.273068, 41.368293, 41.439981, 43.711898, 43.291878,
        42.773603, 42.288511, 37.317791, 42.279104, 42.296794, 38.668774,
        49.118094, 44.455283, 39.177628, 46.843429, 107.474576, 44.590023,
        48.333476, 44.059916, 42.653703, 44.171623, 39.363181, 41.716539,
        40.024492, 42.325769, 42.842437, 40.751910, 40.797598, 41.463472,
        39.005953, 39.293957, 39.472844, 38.046576, 41.515782, 41.567854,
        36.832286, 43.093642, 40.160668, 38.935283, 41.834126, 44.590023,
        105.140579, 43.149105, 41.516560, 43.494333, 45.664210, 36.466241,
        37.477898, 48.793499, 45.853665, 49.323737, 41.123673, 40.325652,
        51.248836, 50.493111, 38.600491, 45.617102, 36.112222, 43.478416,
        47.002908, 45.833218, 50.196436, 54.193098, 41.465246, 46.788801,
        48.333476, 43.149105, 123.746816, 53.234332, 44.633908, 53.537592,
        43.196327, 42.747181, 48.696170, 46.601722, 52.125739, 41.930358,
        38.599119, 46.869144, 46.296591, 43.761301, 44.692618, 39.406838,
        45.003800, 43.922434, 43.228963, 45.535041, 49.678271, 40.962743,
        46.983780, 44.059916, 41.516560, 53.234332, 125.202062, 43.967875,
        52.416619, 39.937196, 39.775405, 45.870016, 40.668861, 45.831747,
        41.002915, 42.727171, 45.178199, 43.449036, 42.294750, 41.194977,
        39.258432, 42.433639, 43.479144, 36.833273, 43.087415, 41.834745,
        40.533287, 43.511429, 42.653703, 43.494333, 44.633908, 43.967875,
        107.336922, 44.396001, 39.819884, 38.676633, 46.398093, 49.084078,
        49.466716, 43.099168, 39.382845, 45.709593, 48.798961, 42.410289,
        41.284030, 36.347136, 42.757336, 44.291425, 44.787401, 48.540036,
        47.227606, 43.367907, 47.291825, 44.171623, 45.664210, 53.537592,
        52.416619, 44.396001, 114.651847, 40.826050, 37.634130, 44.305791,
        39.292553, 44.762183, 36.018699, 41.535989, 42.402465, 42.877859,
        40.266469, 39.580938, 38.927619, 35.814356, 43.352951, 38.176476,
        35.942528, 42.214571, 38.723316, 40.023523, 39.363181, 36.466241,
        43.196327, 39.937196, 39.819884, 40.826050, 96.128345, 40.788606,
        41.863013, 35.781804, 41.930313, 33.646602, 41.518779, 44.097412,
        37.055014, 39.909538, 42.382268, 41.604838, 39.017848, 42.613649,
        39.062471, 37.962886, 42.598524, 36.312733, 40.581819, 41.716539,
        37.477898, 42.747181, 39.775405, 38.676633, 37.634130, 40.788606,
        97.605849)
    )
    val fml = DenseVector(-1219.296604, -1126.029219, -1202.257728,
      -1022.064083, -1047.414836, -1155.507387, -1169.502847, -1091.655366,
      -1063.832607, -1015.829142, -1075.864072, -1101.427162, -1058.907539,
      -1115.171116, -1205.015211, -1090.627084, -1143.206126, -1140.107801,
      -1100.285642, -1198.992795, -1246.276120, -1159.678276, -1194.177391,
      -1056.458015, -1058.791892)
    val cost = QuadraticMinimizer.Cost(Hml, fml)

    val nlResult = NonlinearMinimizer(25, PROBABILITYSIMPLEX, 1.0)
      .minimizeAndReturnState(cost, DenseVector.zeros[Double](25))

    val golden = DenseVector(0.3131862265452959, 0.0, 0.01129486116330884, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.060642310566736704,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.6151756449091074, 0.0, 0.0, 0.0, 0.0)

    println(s"Probability Simplex projection iter ${nlResult.iter}")

    assert(norm(nlResult.x - golden) < 1e-3)
    assert(abs(sum(nlResult.x) - 1.0) < 1e-4)
  }

  test("Nonlinear Minimization with L1 projection compared to Octave") {
    val Hl1 = new DenseMatrix(
      25,
      25,
      Array(253.535098, 236.477785, 234.421906, 223.374867, 241.007512,
        204.695511, 226.465507, 223.351032, 249.179386, 221.411909, 238.679352,
        203.579010, 217.564498, 243.852681, 266.607649, 213.994496, 241.620759,
        223.602907, 220.038678, 264.704959, 240.341716, 223.672378, 244.323303,
        223.216217, 226.074990, 236.477785, 278.862035, 245.756639, 237.489890,
        252.783139, 214.077652, 241.816953, 238.790633, 260.536460, 228.121417,
        255.103936, 216.608405, 237.150426, 258.933231, 281.958112, 228.971242,
        252.508513, 234.242638, 240.308477, 285.416390, 254.792243, 240.176223,
        259.048267, 235.566855, 236.277617, 234.421906, 245.756639, 269.162882,
        231.416867, 251.321527, 208.134322, 236.567647, 236.558029, 255.805108,
        226.535825, 251.514713, 212.770208, 228.565362, 261.748652, 273.946966,
        227.411615, 252.767900, 232.823977, 233.084574, 278.315614, 250.872786,
        235.227909, 255.104263, 238.931093, 235.402356, 223.374867, 237.489890,
        231.416867, 254.771963, 241.703229, 209.028084, 231.517998, 228.768510,
        250.805315, 216.548935, 245.473869, 207.687875, 222.036114, 250.906955,
        263.018181, 216.128966, 244.445283, 227.436840, 231.369510, 270.721492,
        242.475130, 226.471530, 248.130112, 225.826557, 228.266719, 241.007512,
        252.783139, 251.321527, 241.703229, 285.702320, 219.051868, 249.442308,
        240.400187, 264.970407, 232.503138, 258.819837, 220.160683, 235.621356,
        267.743972, 285.795029, 229.667231, 260.870105, 240.751687, 247.183922,
        289.044453, 260.715749, 244.210258, 267.159502, 242.992822, 244.070245,
        204.695511, 214.077652, 208.134322, 209.028084, 219.051868, 210.164224,
        208.151908, 201.539036, 226.373834, 192.056565, 219.950686, 191.459568,
        195.982460, 226.739575, 240.677519, 196.116652, 217.352348, 203.533069,
        204.581690, 243.603643, 217.785986, 204.205559, 223.747953, 203.586842,
        200.165867, 226.465507, 241.816953, 236.567647, 231.517998, 249.442308,
        208.151908, 264.007925, 227.080718, 253.174653, 220.322823, 248.619983,
        210.100242, 223.279198, 254.807401, 269.896959, 222.927882, 247.017507,
        230.484479, 233.358639, 274.935489, 249.237737, 235.229584, 253.029955,
        228.601700, 230.512885, 223.351032, 238.790633, 236.558029, 228.768510,
        240.400187, 201.539036, 227.080718, 258.773479, 249.471480, 215.664539,
        243.078577, 202.337063, 221.020998, 249.979759, 263.356244, 213.470569,
        246.182278, 225.727773, 229.873732, 266.295057, 242.954024, 225.510760,
        249.370268, 227.813265, 232.141964, 249.179386, 260.536460, 255.805108,
        250.805315, 264.970407, 226.373834, 253.174653, 249.471480, 302.360150,
        237.902729, 265.769812, 224.947876, 243.088105, 273.690377, 291.076027,
        241.089661, 267.772651, 248.459822, 249.662698, 295.935799, 267.460908,
        255.668926, 275.902272, 248.495606, 246.827505, 221.411909, 228.121417,
        226.535825, 216.548935, 232.503138, 192.056565, 220.322823, 215.664539,
        237.902729, 245.154567, 234.956316, 199.557862, 214.774631, 240.339217,
        255.161923, 209.328714, 232.277540, 216.298768, 220.296241, 253.817633,
        237.638235, 220.785141, 239.098500, 220.583355, 218.962732, 238.679352,
        255.103936, 251.514713, 245.473869, 258.819837, 219.950686, 248.619983,
        243.078577, 265.769812, 234.956316, 288.133073, 225.087852, 239.810430,
        268.406605, 283.289840, 233.858455, 258.306589, 240.263617, 246.844456,
        290.492875, 267.212598, 243.218596, 265.681905, 244.615890, 242.543363,
        203.579010, 216.608405, 212.770208, 207.687875, 220.160683, 191.459568,
        210.100242, 202.337063, 224.947876, 199.557862, 225.087852, 217.501685,
        197.897572, 229.825316, 242.175607, 201.123644, 219.820165, 202.894307,
        211.468055, 246.048907, 225.135194, 210.076305, 226.806762, 212.014431,
        205.123267, 217.564498, 237.150426, 228.565362, 222.036114, 235.621356,
        195.982460, 223.279198, 221.020998, 243.088105, 214.774631, 239.810430,
        197.897572, 244.439113, 241.621129, 260.400953, 216.482178, 236.805076,
        216.680343, 223.816297, 263.188711, 236.311810, 222.950152, 244.636356,
        219.121372, 219.911078, 243.852681, 258.933231, 261.748652, 250.906955,
        267.743972, 226.739575, 254.807401, 249.979759, 273.690377, 240.339217,
        268.406605, 229.825316, 241.621129, 302.928261, 288.344398, 238.549018,
        267.239982, 248.073140, 254.230916, 296.789984, 267.158551, 252.226496,
        271.170860, 248.325354, 253.694013, 266.607649, 281.958112, 273.946966,
        263.018181, 285.795029, 240.677519, 269.896959, 263.356244, 291.076027,
        255.161923, 283.289840, 242.175607, 260.400953, 288.344398, 343.457361,
        257.368309, 284.795470, 263.122266, 271.239770, 320.209823, 283.933299,
        264.416752, 292.035194, 268.764031, 265.345807, 213.994496, 228.971242,
        227.411615, 216.128966, 229.667231, 196.116652, 222.927882, 213.470569,
        241.089661, 209.328714, 233.858455, 201.123644, 216.482178, 238.549018,
        257.368309, 239.295031, 234.913508, 218.066855, 219.648997, 257.969951,
        231.243624, 224.657569, 238.158714, 217.174368, 215.933866, 241.620759,
        252.508513, 252.767900, 244.445283, 260.870105, 217.352348, 247.017507,
        246.182278, 267.772651, 232.277540, 258.306589, 219.820165, 236.805076,
        267.239982, 284.795470, 234.913508, 289.709239, 241.312315, 247.249491,
        286.702147, 264.252852, 245.151647, 264.582984, 240.842689, 245.837476,
        223.602907, 234.242638, 232.823977, 227.436840, 240.751687, 203.533069,
        230.484479, 225.727773, 248.459822, 216.298768, 240.263617, 202.894307,
        216.680343, 248.073140, 263.122266, 218.066855, 241.312315, 255.363057,
        230.209787, 271.091482, 239.220241, 225.387834, 247.486715, 226.052431,
        224.119935, 220.038678, 240.308477, 233.084574, 231.369510, 247.183922,
        204.581690, 233.358639, 229.873732, 249.662698, 220.296241, 246.844456,
        211.468055, 223.816297, 254.230916, 271.239770, 219.648997, 247.249491,
        230.209787, 264.014907, 271.938970, 246.664305, 227.889045, 249.908085,
        232.035369, 229.010298, 264.704959, 285.416390, 278.315614, 270.721492,
        289.044453, 243.603643, 274.935489, 266.295057, 295.935799, 253.817633,
        290.492875, 246.048907, 263.188711, 296.789984, 320.209823, 257.969951,
        286.702147, 271.091482, 271.938970, 352.825726, 286.200221, 267.716897,
        297.182554, 269.776351, 266.721561, 240.341716, 254.792243, 250.872786,
        242.475130, 260.715749, 217.785986, 249.237737, 242.954024, 267.460908,
        237.638235, 267.212598, 225.135194, 236.311810, 267.158551, 283.933299,
        231.243624, 264.252852, 239.220241, 246.664305, 286.200221, 294.042749,
        246.504021, 269.570596, 243.980697, 242.690997, 223.672378, 240.176223,
        235.227909, 226.471530, 244.210258, 204.205559, 235.229584, 225.510760,
        255.668926, 220.785141, 243.218596, 210.076305, 222.950152, 252.226496,
        264.416752, 224.657569, 245.151647, 225.387834, 227.889045, 267.716897,
        246.504021, 259.897656, 251.730847, 229.335712, 229.759185, 244.323303,
        259.048267, 255.104263, 248.130112, 267.159502, 223.747953, 253.029955,
        249.370268, 275.902272, 239.098500, 265.681905, 226.806762, 244.636356,
        271.170860, 292.035194, 238.158714, 264.582984, 247.486715, 249.908085,
        297.182554, 269.570596, 251.730847, 303.872223, 251.585636, 247.878402,
        223.216217, 235.566855, 238.931093, 225.826557, 242.992822, 203.586842,
        228.601700, 227.813265, 248.495606, 220.583355, 244.615890, 212.014431,
        219.121372, 248.325354, 268.764031, 217.174368, 240.842689, 226.052431,
        232.035369, 269.776351, 243.980697, 229.335712, 251.585636, 257.544914,
        228.810942, 226.074990, 236.277617, 235.402356, 228.266719, 244.070245,
        200.165867, 230.512885, 232.141964, 246.827505, 218.962732, 242.543363,
        205.123267, 219.911078, 253.694013, 265.345807, 215.933866, 245.837476,
        224.119935, 229.010298, 266.721561, 242.690997, 229.759185, 247.878402,
        228.810942, 253.353769)
    )
    val fl1 = DenseVector(-892.842851, -934.071560, -932.936015, -888.124343,
      -961.050207, -791.191087, -923.711397, -904.289301, -988.384984,
      -883.909133, -959.465030, -798.551172, -871.622303, -997.463289,
      -1043.912620, -863.013719, -976.975712, -897.033693, -898.694786,
      -1069.245497, -963.491924, -901.263474, -983.768031, -899.865392,
      -902.283567)
    val cost = QuadraticMinimizer.Cost(Hl1, fl1)

    val octaveL1 = DenseVector(0.18611, 0.00000, 0.06317, -0.10417, 0.11262,
      -0.20495, 0.52668, 0.32790, 0.19421, 0.72180, 0.06309, -0.41326, -0.00000,
      0.52078, -0.00000, 0.18040, 0.62915, 0.16329, -0.06424, 0.37539, 0.01659,
      0.00000, 0.11215, 0.24778, 0.04082)

    val s = octaveL1.foldLeft(0.0) { case (agg, entry) => agg + abs(entry) }
    val nlResult = NonlinearMinimizer(25, SPARSE, s)
      .minimizeAndReturnState(cost, DenseVector.zeros[Double](25))

    println(s"L1 projection iter ${nlResult.iter}")
    assert(norm(nlResult.x - octaveL1, 2) < 1e-4)
  }

  test("Nonlinear Minimization with L1 proximal compared to Octave") {
    val Hl1 = new DenseMatrix(
      25,
      25,
      Array(253.535098, 236.477785, 234.421906, 223.374867, 241.007512,
        204.695511, 226.465507, 223.351032, 249.179386, 221.411909, 238.679352,
        203.579010, 217.564498, 243.852681, 266.607649, 213.994496, 241.620759,
        223.602907, 220.038678, 264.704959, 240.341716, 223.672378, 244.323303,
        223.216217, 226.074990, 236.477785, 278.862035, 245.756639, 237.489890,
        252.783139, 214.077652, 241.816953, 238.790633, 260.536460, 228.121417,
        255.103936, 216.608405, 237.150426, 258.933231, 281.958112, 228.971242,
        252.508513, 234.242638, 240.308477, 285.416390, 254.792243, 240.176223,
        259.048267, 235.566855, 236.277617, 234.421906, 245.756639, 269.162882,
        231.416867, 251.321527, 208.134322, 236.567647, 236.558029, 255.805108,
        226.535825, 251.514713, 212.770208, 228.565362, 261.748652, 273.946966,
        227.411615, 252.767900, 232.823977, 233.084574, 278.315614, 250.872786,
        235.227909, 255.104263, 238.931093, 235.402356, 223.374867, 237.489890,
        231.416867, 254.771963, 241.703229, 209.028084, 231.517998, 228.768510,
        250.805315, 216.548935, 245.473869, 207.687875, 222.036114, 250.906955,
        263.018181, 216.128966, 244.445283, 227.436840, 231.369510, 270.721492,
        242.475130, 226.471530, 248.130112, 225.826557, 228.266719, 241.007512,
        252.783139, 251.321527, 241.703229, 285.702320, 219.051868, 249.442308,
        240.400187, 264.970407, 232.503138, 258.819837, 220.160683, 235.621356,
        267.743972, 285.795029, 229.667231, 260.870105, 240.751687, 247.183922,
        289.044453, 260.715749, 244.210258, 267.159502, 242.992822, 244.070245,
        204.695511, 214.077652, 208.134322, 209.028084, 219.051868, 210.164224,
        208.151908, 201.539036, 226.373834, 192.056565, 219.950686, 191.459568,
        195.982460, 226.739575, 240.677519, 196.116652, 217.352348, 203.533069,
        204.581690, 243.603643, 217.785986, 204.205559, 223.747953, 203.586842,
        200.165867, 226.465507, 241.816953, 236.567647, 231.517998, 249.442308,
        208.151908, 264.007925, 227.080718, 253.174653, 220.322823, 248.619983,
        210.100242, 223.279198, 254.807401, 269.896959, 222.927882, 247.017507,
        230.484479, 233.358639, 274.935489, 249.237737, 235.229584, 253.029955,
        228.601700, 230.512885, 223.351032, 238.790633, 236.558029, 228.768510,
        240.400187, 201.539036, 227.080718, 258.773479, 249.471480, 215.664539,
        243.078577, 202.337063, 221.020998, 249.979759, 263.356244, 213.470569,
        246.182278, 225.727773, 229.873732, 266.295057, 242.954024, 225.510760,
        249.370268, 227.813265, 232.141964, 249.179386, 260.536460, 255.805108,
        250.805315, 264.970407, 226.373834, 253.174653, 249.471480, 302.360150,
        237.902729, 265.769812, 224.947876, 243.088105, 273.690377, 291.076027,
        241.089661, 267.772651, 248.459822, 249.662698, 295.935799, 267.460908,
        255.668926, 275.902272, 248.495606, 246.827505, 221.411909, 228.121417,
        226.535825, 216.548935, 232.503138, 192.056565, 220.322823, 215.664539,
        237.902729, 245.154567, 234.956316, 199.557862, 214.774631, 240.339217,
        255.161923, 209.328714, 232.277540, 216.298768, 220.296241, 253.817633,
        237.638235, 220.785141, 239.098500, 220.583355, 218.962732, 238.679352,
        255.103936, 251.514713, 245.473869, 258.819837, 219.950686, 248.619983,
        243.078577, 265.769812, 234.956316, 288.133073, 225.087852, 239.810430,
        268.406605, 283.289840, 233.858455, 258.306589, 240.263617, 246.844456,
        290.492875, 267.212598, 243.218596, 265.681905, 244.615890, 242.543363,
        203.579010, 216.608405, 212.770208, 207.687875, 220.160683, 191.459568,
        210.100242, 202.337063, 224.947876, 199.557862, 225.087852, 217.501685,
        197.897572, 229.825316, 242.175607, 201.123644, 219.820165, 202.894307,
        211.468055, 246.048907, 225.135194, 210.076305, 226.806762, 212.014431,
        205.123267, 217.564498, 237.150426, 228.565362, 222.036114, 235.621356,
        195.982460, 223.279198, 221.020998, 243.088105, 214.774631, 239.810430,
        197.897572, 244.439113, 241.621129, 260.400953, 216.482178, 236.805076,
        216.680343, 223.816297, 263.188711, 236.311810, 222.950152, 244.636356,
        219.121372, 219.911078, 243.852681, 258.933231, 261.748652, 250.906955,
        267.743972, 226.739575, 254.807401, 249.979759, 273.690377, 240.339217,
        268.406605, 229.825316, 241.621129, 302.928261, 288.344398, 238.549018,
        267.239982, 248.073140, 254.230916, 296.789984, 267.158551, 252.226496,
        271.170860, 248.325354, 253.694013, 266.607649, 281.958112, 273.946966,
        263.018181, 285.795029, 240.677519, 269.896959, 263.356244, 291.076027,
        255.161923, 283.289840, 242.175607, 260.400953, 288.344398, 343.457361,
        257.368309, 284.795470, 263.122266, 271.239770, 320.209823, 283.933299,
        264.416752, 292.035194, 268.764031, 265.345807, 213.994496, 228.971242,
        227.411615, 216.128966, 229.667231, 196.116652, 222.927882, 213.470569,
        241.089661, 209.328714, 233.858455, 201.123644, 216.482178, 238.549018,
        257.368309, 239.295031, 234.913508, 218.066855, 219.648997, 257.969951,
        231.243624, 224.657569, 238.158714, 217.174368, 215.933866, 241.620759,
        252.508513, 252.767900, 244.445283, 260.870105, 217.352348, 247.017507,
        246.182278, 267.772651, 232.277540, 258.306589, 219.820165, 236.805076,
        267.239982, 284.795470, 234.913508, 289.709239, 241.312315, 247.249491,
        286.702147, 264.252852, 245.151647, 264.582984, 240.842689, 245.837476,
        223.602907, 234.242638, 232.823977, 227.436840, 240.751687, 203.533069,
        230.484479, 225.727773, 248.459822, 216.298768, 240.263617, 202.894307,
        216.680343, 248.073140, 263.122266, 218.066855, 241.312315, 255.363057,
        230.209787, 271.091482, 239.220241, 225.387834, 247.486715, 226.052431,
        224.119935, 220.038678, 240.308477, 233.084574, 231.369510, 247.183922,
        204.581690, 233.358639, 229.873732, 249.662698, 220.296241, 246.844456,
        211.468055, 223.816297, 254.230916, 271.239770, 219.648997, 247.249491,
        230.209787, 264.014907, 271.938970, 246.664305, 227.889045, 249.908085,
        232.035369, 229.010298, 264.704959, 285.416390, 278.315614, 270.721492,
        289.044453, 243.603643, 274.935489, 266.295057, 295.935799, 253.817633,
        290.492875, 246.048907, 263.188711, 296.789984, 320.209823, 257.969951,
        286.702147, 271.091482, 271.938970, 352.825726, 286.200221, 267.716897,
        297.182554, 269.776351, 266.721561, 240.341716, 254.792243, 250.872786,
        242.475130, 260.715749, 217.785986, 249.237737, 242.954024, 267.460908,
        237.638235, 267.212598, 225.135194, 236.311810, 267.158551, 283.933299,
        231.243624, 264.252852, 239.220241, 246.664305, 286.200221, 294.042749,
        246.504021, 269.570596, 243.980697, 242.690997, 223.672378, 240.176223,
        235.227909, 226.471530, 244.210258, 204.205559, 235.229584, 225.510760,
        255.668926, 220.785141, 243.218596, 210.076305, 222.950152, 252.226496,
        264.416752, 224.657569, 245.151647, 225.387834, 227.889045, 267.716897,
        246.504021, 259.897656, 251.730847, 229.335712, 229.759185, 244.323303,
        259.048267, 255.104263, 248.130112, 267.159502, 223.747953, 253.029955,
        249.370268, 275.902272, 239.098500, 265.681905, 226.806762, 244.636356,
        271.170860, 292.035194, 238.158714, 264.582984, 247.486715, 249.908085,
        297.182554, 269.570596, 251.730847, 303.872223, 251.585636, 247.878402,
        223.216217, 235.566855, 238.931093, 225.826557, 242.992822, 203.586842,
        228.601700, 227.813265, 248.495606, 220.583355, 244.615890, 212.014431,
        219.121372, 248.325354, 268.764031, 217.174368, 240.842689, 226.052431,
        232.035369, 269.776351, 243.980697, 229.335712, 251.585636, 257.544914,
        228.810942, 226.074990, 236.277617, 235.402356, 228.266719, 244.070245,
        200.165867, 230.512885, 232.141964, 246.827505, 218.962732, 242.543363,
        205.123267, 219.911078, 253.694013, 265.345807, 215.933866, 245.837476,
        224.119935, 229.010298, 266.721561, 242.690997, 229.759185, 247.878402,
        228.810942, 253.353769)
    )
    val fl1 = DenseVector(-892.842851, -934.071560, -932.936015, -888.124343,
      -961.050207, -791.191087, -923.711397, -904.289301, -988.384984,
      -883.909133, -959.465030, -798.551172, -871.622303, -997.463289,
      -1043.912620, -863.013719, -976.975712, -897.033693, -898.694786,
      -1069.245497, -963.491924, -901.263474, -983.768031, -899.865392,
      -902.283567)
    val cost = QuadraticMinimizer.Cost(Hl1, fl1)

    val octaveL1 = DenseVector(0.18611, 0.00000, 0.06317, -0.10417, 0.11262,
      -0.20495, 0.52668, 0.32790, 0.19421, 0.72180, 0.06309, -0.41326, -0.00000,
      0.52078, -0.00000, 0.18040, 0.62915, 0.16329, -0.06424, 0.37539, 0.01659,
      0.00000, 0.11215, 0.24778, 0.04082)
    val proximalL1 = ProximalL1().setLambda(2.0)
    val nl = new NonlinearMinimizer(proximalL1)
    val nlResult =
      nl.minimizeAndReturnState(cost, DenseVector.zeros[Double](25))

    println(s"L1 Proximal iter ${nlResult.iter}")
    assert(norm(nlResult.z - octaveL1, Inf) < 1e-4)
  }
}
