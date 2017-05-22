/************************************************************************\
  ** Project                                                              **
  **       ______  ______   __    ______    ____                          **
  **      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
  **     / /__   / /_/ /  / /   / /_/ /   / /_                            **
  **    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
  **   ____/ / / /      / /   / / | |   / /__                             **
  **  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
  **                                                                      **
  **      Redistribution and use permitted under the MIT license.         **
  **                                                                      **
\************************************************************************/
package spire
package random
package rng

/**
  * This is an Index Cache for the Well19937a and Well19937c implementations.
  *
  * <p>The acronym WELL stands for Well Equidistributed Long-period Linear.
  *
  * <p><b>Reference: </b>
  * François Panneton, Pierre L'Ecuyer and Makoto Matsumoto:
  * "Improved Long-Period Generators Based on Linear Recurrences Modulo 2",
  * <i>ACM Transactions on Mathematical Software,</i> Vol. 32, No. 1, January 2006, pp 1--16.
  *
  * @see <a href="http://www.iro.umontreal.ca/~panneton/well/WELL19937a.c">WELL19937a.c</a>
  * @see <a href="http://www.iro.umontreal.ca/~panneton/WELLRNG.html">Well PRNG Home Page</a>
  * @see <a href="http://en.wikipedia.org/wiki/Well_Equidistributed_Long-period_Linear">WELL @ Wikipedia</a>
  * @author <a href="mailto:dusan.kysel@gmail.com">Dušan Kysel</a>
  */
private[random] object Well19937acIndexCache

  // Number of bits in the pool.
  @inline private final val K: Int = 19937

  // Length of the pool in ints.
  @inline private final val R: Int = (K + 31) / 32

  // Length of the pool in ints -1.
  @inline private final val R_1: Int = R - 1

  // Length of the pool in ints -2.
  @inline private final val R_2: Int = R - 2

  // First parameter of the algorithm.
  @inline private final val M1: Int = 70

  // Second parameter of the algorithm.
  @inline private final val M2: Int = 179

  // Third parameter of the algorithm.
  @inline private final val M3: Int = 449

  val vm1 = Array.tabulate(R)(i => (i + M1) % R)
  val vm2 = Array.tabulate(R)(i => (i + M2) % R)
  val vm3 = Array.tabulate(R)(i => (i + M3) % R)
  val vrm1 = Array.tabulate(R)(i => (i + R_1) % R)
  val vrm2 = Array.tabulate(R)(i => (i + R_2) % R)
