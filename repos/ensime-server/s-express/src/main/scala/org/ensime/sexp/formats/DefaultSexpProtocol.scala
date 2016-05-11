// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp.formats

trait DefaultSexpProtocol
  extends BasicFormats
  with StandardFormats
  with CollectionFormats
  with ProductFormats

object DefaultSexpProtocol extends DefaultSexpProtocol
