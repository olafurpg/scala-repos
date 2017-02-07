/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb
package mapper

import scala.collection.mutable._
import java.lang.reflect.Method
import scala.xml._
import java.util.Date
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.S._
import net.liftweb.http.js._
import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.util._
import scala.reflect.runtime.universe._

/**
  * This is the supertrait of all traits that can be mixed into a MappedField.
  * All methods should be abstract.  They will be made concrete in implementations.
  */
trait MixableMappedField extends BaseField {

  /**
    * Will be set to the type of the owner of the field
    */
  type TheOwnerType <: Mapper[TheOwnerType]

  /**
    * Return the field name and field value, delimited by an '='
    */
  def asString: String

  def dbColumnCount: Int

  def dbIndexed_? : Boolean

  def dbNotNull_? : Boolean

  def dbPrimaryKey_? : Boolean

  /**
    * Is the field a foreign key reference
    */
  def dbForeignKey_? : Boolean

  def asHtml: NodeSeq
}

/**
  * The base (not Typed) trait that defines a field that is mapped to a column or more than 1 column
  * (e.g., MappedPassword) in the database
  */
trait BaseMappedField
    extends SelectableField
    with Bindable
    with MixableMappedField
    with Serializable {

  def dbDisplay_? = true

  def dbIncludeInForm_? = dbDisplay_?

  def asJsonField: Box[JsonAST.JField] =
    asJsonValue.map(v => JsonAST.JField(name, v))

  def asJsonValue: Box[JsonAST.JValue]

  /**
    *  Get a JDBC friendly representation of the named field (this is used for MappedFields that correspond to more than
    * 1 column in the database.)
    * @param field -- the name of the field being mapped to
    */
  def jdbcFriendly(field: String): AnyRef

  /**
    * Get a JDBC friendly object for the part of this field that maps to the first
    * column in the database
    */
  def jdbcFriendly: AnyRef

  /**
    * Get the JDBC SQL Type for this field
    */
  def targetSQLType(field: String): Int

  /**
    * Do we ignore the targetSQLType for setObject
    */
  def dbIgnoreSQLType_? : Boolean = false

  /**
    * Get the JDBC SQL Type for this field
    */
  def targetSQLType: Int

  /**
    * Given the driver type, return the string required to create the column in the database
    */
  def fieldCreatorString(dbType: DriverType, colName: String): String

  /**
    * Given the driver type, return a list of statements to create the columns in the database
    */
  def fieldCreatorString(dbType: DriverType): List[String]

  /**
    * Convert the field to its name/value pair (e.g., name=David)
    */
  def asString: String

  /**
    * The number of database columns that this field represents
    */
  def dbColumnCount: Int

  def dbColumnNames(in: String): List[String]

  def dbColumnName: String

  /**
    * The forced lower case column names
    */
  final def _dbColumnNameLC = {
    val name = dbColumnName

    val conn = DB.currentConnection
    conn
      .map { c =>
        if (c.metaData.storesMixedCaseIdentifiers) name
        else name.toLowerCase
      }
      .openOr(name)
  }

  /**
    *  Should the field be indexed?
    */
  def dbIndexed_? : Boolean

  /**
    * Set to true if the field should be created as NOT NULL
    */
  def dbNotNull_? : Boolean = false

  /**
    * Is the field the table's primary key
    */
  def dbPrimaryKey_? : Boolean

  /**
    * Is the primary key autogenerated
    */
  def dbAutogenerated_? : Boolean = dbPrimaryKey_?

  /**
    * Is the field a foreign key reference
    */
  def dbForeignKey_? : Boolean

  /**
    * Called when a column has been added to the database via Schemifier
    */
  def dbAddedColumn: Box[() => Unit]

  /**
    * Called when a column has indexed via Schemifier
    */
  def dbAddedIndex: Box[() => Unit]

  def asHtml: NodeSeq

  /**
    * Called after the field is saved to the database
    */
  protected[mapper] def doneWithSave()

  def asJsExp: JsExp

  def asJs: List[(String, JsExp)] = List((name, asJsExp))

  /**
    * What form elements are we going to add to this field?
    */
  def formElemAttrs: scala.Seq[SHtml.ElemAttr] = Nil

  def renderJs_? = true

  /**
    * This is where the instance creates its "toForm" stuff.
    * The actual toForm method wraps the information based on
    * mode.
    */
  def _toForm: Box[NodeSeq]
}

/**
  * Mix this trait into a BaseMappedField and it will be indexed
  */
trait DBIndexed extends BaseMappedField {
  override def dbIndexed_? = true
}

trait BaseOwnedMappedField[OwnerType <: Mapper[OwnerType]]
    extends BaseMappedField

trait TypedField[FieldType] {

  /**
    * The default value for the field
    */
  def defaultValue: FieldType

  /**
    * What is the real class that corresponds to FieldType
    */
  def dbFieldClass: Class[FieldType]
}

/**
  * A Mapped field that is Nullable in the database.  Will return Empty box for NULL values and Full for non-null values
  */
trait MappedNullableField[
    NullableFieldType <: Any, OwnerType <: Mapper[OwnerType]]
    extends MappedField[Box[NullableFieldType], OwnerType] {

  /**
    * All fields of this type are NULLable
    */
  override final def dbNotNull_? : Boolean = false

  override def toString = get.map(_.toString).openOr("")

  /**
    * Create an input field for the item
    */
  override def _toForm: Box[NodeSeq] =
    S.fmapFunc({ s: List[String] =>
      this.setFromAny(s)
    }) { funcName =>
      Full(appendFieldId(<input type={formInputType}
                       name={funcName}
                       value={get match {
                         case null => ""
                         case Full(null) => ""
                         case Full(s) => s.toString
                         case _ => ""
                       }}/>))
    }
}

/**
  * The strongly typed field that's mapped to a column (or many columns) in the database.
  * FieldType is the type of the field and OwnerType is the Owner of the field
  */
trait MappedField[FieldType <: Any, OwnerType <: Mapper[OwnerType]]
    extends TypedField[FieldType]
    with BaseOwnedMappedField[OwnerType]
    with FieldIdentifier
    with PSettableValueHolder[FieldType]
    with scala.Equals {

  /**
    * Will be set to the type of the field
    */
  override type ValueType = FieldType

  /**
    * Will be set to the type of the owner of the field
    */
  type TheOwnerType = OwnerType

  /**
    * Should the field be ignored by the OR Mapper?
    */
  def ignoreField_? = false

  def manifest: TypeTag[FieldType]

  /**
    * Get the source field metadata for the field
    * @return the source field metadata for the field
    */
  def sourceInfoMetadata(): SourceFieldMetadata { type ST = FieldType }

  def sourceFieldInfo(): SourceFieldInfo { type T = FieldType } =
    SourceFieldInfoRep(get, sourceInfoMetadata())

  /**
    * Get the field that this prototypical field represents
    *
    * @param actual the object to find the field on
    */
  def actualField(actual: OwnerType): MappedField[FieldType, OwnerType] =
    actual.getSingleton.getActualField(actual, this)

  /**
    * Given the driver type, return the string required to create the column in the database
    */
  def fieldCreatorString(dbType: DriverType, colName: String): String

  /**
    * Given the driver type, return a list of SQL creation strings for the columns represented by this field
    */
  def fieldCreatorString(dbType: DriverType): List[String] =
    dbColumnNames(name).map { c =>
      fieldCreatorString(dbType, c)
    }

  def notNullAppender() = if (dbNotNull_?) " NOT NULL " else ""

  /**
    * Is the field dirty
    */
  private var _dirty_? = false

  /**
    * Is the field dirty (has it been changed since the record was loaded from the database
    */
  def dirty_? = !dbPrimaryKey_? && _dirty_?

  /**
    * Make the field dirty
    */
  protected def dirty_?(b: Boolean) = _dirty_? = b

  /**
    * Called when a column has been added to the database via Schemifier
    */
  def dbAddedColumn: Box[() => Unit] = Empty

  /**
    * Called when a column has indexed via Schemifier
    */
  def dbAddedIndex: Box[() => Unit] = Empty

  /**
    * override this method in indexed fields to indicate that the field has been saved
    */
  def dbIndexFieldIndicatesSaved_? = false;

  /**
    * Return the owner of this field
    */
  def fieldOwner: OwnerType

  /**
    * Are we in "safe" mode (i.e., the value of the field can be read or written without any security checks.)
    */
  final def safe_? : Boolean = fieldOwner.safe_?

  /**
    * Given the current execution state, can the field be written?
    */
  def writePermission_? = false

  /**
    * Given the current execution state, can the field be read?
    */
  def readPermission_? = false

  /**
    * Assignment from the underlying type.  It's ugly, but:<br />
    * field() = new_value <br />
    * field set new_value <br />
    * field.set(new_value) <br />
    * are all the same
    */
  def update[Q <% FieldType](v: Q) {
    this.set(v)
  }

  def apply[Q <% FieldType](v: Q): OwnerType = {
    this.set(v)
    fieldOwner
  }

  def apply(v: FieldType): OwnerType = {
    // issue 154
    this.set(v)
    fieldOwner
  }

  /**
    * The unique field id is the field name and the mapper name
    */
  override def uniqueFieldId: Box[String] =
    Full(fieldOwner.getSingleton.dbTableName + "_" + name)

  /**
    * Set the field to the value
    */
  def set(value: FieldType): FieldType = {
    if (safe_? || writePermission_?) i_set_!(value)
    else throw new Exception("Do not have permissions to set this field")
  }

  def :=[Q <% FieldType](v: Q): FieldType = {
    set(v)
  }

  def :=(v: FieldType): FieldType = {
    set(v)
  }

  private var _name: String = null

  /**
    * The internal name of this field.  Use name
    */
  private[mapper] final def i_name_! = _name

  /**
    * The name of this field
    */
  final def name = synchronized {
    if (_name eq null) {
      fieldOwner.checkNames
    }
    _name
  }

  /**
    * Set the name of this field
    */
  private[mapper] final def setName_!(newName: String): String = {
    if (safe_?) _name = newName
    _name
  }

  /**
    * The display name of this field (e.g., "First Name")
    */
  override def displayName: String =
    MapperRules.displayNameCalculator.vend(fieldOwner, S.locale, name)

  def resetDirty {
    if (safe_?) dirty_?(false)
  }

  /**
    *  Attempt to figure out what the incoming value is and set the field to that value.  Return true if
    * the value could be assigned
    */
  def setFromAny(value: Any): FieldType

  def toFormAppendedAttributes: MetaData =
    if (Props.mode == Props.RunModes.Test)
      new PrefixedAttribute("lift", "field_name", Text(calcFieldName), Null)
    else Null

  def calcFieldName: String =
    fieldOwner.getSingleton.internal_dbTableName + ":" + name

  def toForm: Box[NodeSeq] = {
    def mf(in: scala.xml.Node): NodeSeq = in match {
      case g: Group => g.nodes.flatMap(mf)
      case e: Elem => e % toFormAppendedAttributes
      case other => other
    }

    _toForm
      .map(_.flatMap(mf))
      .map(SHtml.ElemAttr.applyToAllElems(_, formElemAttrs))
  }

  /**
    * Create an input field for the item
    */
  override def _toForm: Box[NodeSeq] =
    S.fmapFunc({ s: List[String] =>
      this.setFromAny(s)
    }) { funcName =>
      Full(appendFieldId(<input type={formInputType}
                       name={funcName}
                       value={get match {case null => "" case s => s.toString}}/>))
    }

  /**
    * When building the form field, what's the input element's
    * type attribute.  Defaults to 'text', but change to 'email'
    * or other HTML5 values.
    */
  protected def formInputType = "text"

  /**
    * If the field has a defined fieldId, append it
    */
  protected def appendFieldId(in: Elem): Elem = fieldId match {
    case Some(i) => {
      import util.Helpers._
      in % ("id" -> i)
    }
    case _ => in
  }

  /**
    * Set the field to the Box value if the Box is Full
    */
  def set_?(value: Box[FieldType]): Box[FieldType] = {
    value.foreach(v => this.set(v))
    value
  }

  /**
    * A list of functions that transform the value before it is set.  The transformations
    * are also applied before the value is used in a query.  Typical applications
    * of this are trimming and/or toLowerCase-ing strings
    */
  def setFilter: List[FieldType => FieldType] = Nil

  protected final def i_set_!(value: FieldType): FieldType = {
    real_i_set_!(runFilters(value, setFilter))
  }

  def runFilters(in: FieldType,
                 filter: List[FieldType => FieldType]): FieldType =
    filter match {
      case Nil => in
      case x :: xs => runFilters(x(in), xs)
    }

  /**
    * Must be implemented to store the value of the field
    */
  protected def real_i_set_!(value: FieldType): FieldType

  def buildSetActualValue(accessor: Method,
                          inst: AnyRef,
                          columnName: String): (OwnerType, AnyRef) => Unit
  def buildSetLongValue(accessor: Method,
                        columnName: String): (OwnerType, Long, Boolean) => Unit
  def buildSetStringValue(accessor: Method,
                          columnName: String): (OwnerType, String) => Unit
  def buildSetDateValue(accessor: Method,
                        columnName: String): (OwnerType, Date) => Unit
  def buildSetBooleanValue(
      accessor: Method,
      columnName: String): (OwnerType, Boolean, Boolean) => Unit
  protected def getField(inst: OwnerType, meth: Method) =
    meth.invoke(inst).asInstanceOf[MappedField[FieldType, OwnerType]];
  protected def doField(
      inst: OwnerType,
      meth: Method,
      func: PartialFunction[MappedField[FieldType, OwnerType], Unit]) {
    val f = getField(inst, meth)
    if (func.isDefinedAt(f)) func(f)
  }

  /**
    * Convert the field to its "context free" type (e.g., String, Int, Long, etc.)
    * If there are no read permissions, the value will be obscured
    */
  def get: FieldType = {
    if (safe_? || readPermission_?) i_is_!
    else i_obscure_!(i_is_!)
  }

  /**
    * What value was the field's value when it was pulled from the DB?
    */
  def was: FieldType = {
    if (safe_? || readPermission_?) i_was_!
    else i_obscure_!(i_was_!)
  }

  /**
    * The actual value of the field
    */
  protected def i_is_! : FieldType

  /**
    * The value of the field when it was pulled from the DB
    */
  protected def i_was_! : FieldType

  /**
    * Obscure the incoming value to a "safe" value (e.g., if there are
    * not enough rights to view the entire social security number 123-45-5678, this
    * method might return ***-**-*678
    */
  protected def i_obscure_!(in: FieldType): FieldType

  /**
    * Return the field name and field value, delimited by an '='
    */
  def asString = name + "=" + toString

  def dbColumnCount = 1

  def dbColumnNames(in: String) =
    if (dbColumnCount == 1) List(_dbColumnNameLC) else List(in.toLowerCase)

  def dbColumnName = {
    val columnName =
      MapperRules.columnName(fieldOwner.connectionIdentifier, name)
    if (DB.reservedWords.contains(columnName.toLowerCase)) columnName + "_c"
    else columnName
  }

  def dbSelectString =
    fieldOwner.getSingleton._dbTableNameLC + "." + _dbColumnNameLC

  def dbIndexed_? : Boolean = false

  /**
    * Set to true if the field should be created as NOT NULL
    */
  override def dbNotNull_? : Boolean = false

  def dbPrimaryKey_? : Boolean = false

  /**
    * Is the field a foreign key reference
    */
  def dbForeignKey_? : Boolean = false

  def jdbcFriendly(field: String): Object

  def jdbcFriendly: Object = jdbcFriendly(_dbColumnNameLC)

  /**
    * Get the JDBC SQL Type for this field
    */
  def targetSQLType(field: String): Int = targetSQLType

  /**
    * Get the JDBC SQL Type for this field
    */
  def targetSQLType: Int

  override def toString: String =
    get match {
      case null => ""
      case v => v.toString
    }

  def validations: List[FieldType => List[FieldError]] = Nil

  def validate: List[FieldError] = {
    val cv = get
    val errorRet: ListBuffer[FieldError] = new ListBuffer

    /*
     validations.flatMap{
     case pf: PartialFunction[FieldType, List[FieldError]] =>
     if (pf.isDefinedAt(cv)) pf(cv)
     else Nil
     case f => f(cv)
     }
     */

    def runValidations(validators: List[FieldType => List[FieldError]]) {
      validators match {
        case Nil => ()
        case x :: rest =>
          val errors = x match {
            case pf: PartialFunction[FieldType, List[FieldError]] =>
              if (pf.isDefinedAt(cv)) pf(cv)
              else Nil
            case f => f(cv)
          }

          (errors, x) match {
            case (Nil, _) => runValidations(rest)
            case (errors, e: StopValidationOnError[FieldType]) =>
              errorRet.appendAll(errors)
            case (errors, _) =>
              errorRet.appendAll(errors)
              runValidations(rest)
          }
      }
    }
    runValidations(validations)
    errorRet.toList
  }

  final def convertToJDBCFriendly(value: FieldType): Object =
    real_convertToJDBCFriendly(runFilters(value, setFilter))

  protected def real_convertToJDBCFriendly(value: FieldType): Object

  override def hashCode(): Int = i_is_! match {
    case null => 0
    case x => x.hashCode
  }

  /**
    * Does the "right thing" comparing mapped fields
    */
  override def equals(other: Any): Boolean = {
    (other match {
      case e: scala.Equals => e.canEqual(this)
      case _ => true
    }) &&
    (other match {
      case mapped: MappedField[_, _] => this.i_is_! == mapped.i_is_!
      case ov: AnyRef
          if (ov ne null) && dbFieldClass.isAssignableFrom(ov.getClass) =>
        this.get == runFilters(ov.asInstanceOf[FieldType], setFilter)
      case ov => this.get == ov
    })
  }

  def canEqual(that: Any) = that match {
    case ar: AnyRef => ar.getClass == this.getClass
    case _ => false
  }

  override def asHtml: scala.xml.Node = Text(toString)
}

trait IndexedField[O] extends BaseIndexedField {
  def convertKey(in: String): Box[O]
  def convertKey(in: Int): Box[O]
  def convertKey(in: Long): Box[O]
  def convertKey(in: AnyRef): Box[O]
  def makeKeyJDBCFriendly(in: O): AnyRef
  override def dbDisplay_? = false
}

trait BaseIndexedField extends BaseMappedField {}

trait LifecycleCallbacks {
  def beforeValidation {}
  def beforeValidationOnCreate {}
  def beforeValidationOnUpdate {}
  def afterValidation {}
  def afterValidationOnCreate {}
  def afterValidationOnUpdate {}

  def beforeSave {}
  def beforeCreate {}
  def beforeUpdate {}

  def afterSave {}
  def afterCreate {}
  def afterUpdate {}

  def beforeDelete {}
  def afterDelete {}
}
