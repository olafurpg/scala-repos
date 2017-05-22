package scalafx.scene.paint

import javafx.scene.{image => jfxsi, paint => jfxsp}

import scala.language.implicitConversions
import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.delegate.SFXDelegate
import scalafx.scene.image.Image

object PhongMaterial
  implicit def sfxPhongMaterial2jfx(c: PhongMaterial): jfxsp.PhongMaterial =
    if (c != null) c.delegate else null

/** The PhongMaterial class provides definitions of properties that represent a form of Phong shaded material. */
class PhongMaterial(
    override val delegate: jfxsp.PhongMaterial = new jfxsp.PhongMaterial())
    extends Material(delegate) with SFXDelegate[jfxsp.PhongMaterial]

  def this(diffuseColor: Color) = this(new jfxsp.PhongMaterial(diffuseColor))

  def this(diffuseColor: Color,
           diffuseMap: Image,
           specularMap: Image,
           bumpMap: Image,
           selfIlluminationMap: Image) =
    this(
        new jfxsp.PhongMaterial(diffuseColor,
                                diffuseMap,
                                specularMap,
                                bumpMap,
                                selfIlluminationMap))

  /** The bump map of this `PhongMaterial`. */
  def bumpMap: ObjectProperty[jfxsi.Image] = delegate.bumpMapProperty
  def bumpMap_=(v: jfxsi.Image)
    bumpMap() = v

  /** Specifies the diffuse color of this Material. */
  def diffuseColor: ObjectProperty[jfxsp.Color] = delegate.diffuseColorProperty
  def diffuseColor_=(v: jfxsp.Color)
    diffuseColor() = v

  /** The diffuse map of this `PhongMaterial`. */
  def diffuseMap: ObjectProperty[jfxsi.Image] = delegate.diffuseMapProperty
  def diffuseMap_=(v: jfxsi.Image)
    diffuseMap() = v

  /** The self illumination map of this `PhongMaterial`. */
  def selfIlluminationMap: ObjectProperty[jfxsi.Image] =
    delegate.selfIlluminationMapProperty
  def selfIlluminationMap_=(v: jfxsi.Image)
    selfIlluminationMap() = v

  /** Specifies the specular color of this Material. */
  def specularColor: ObjectProperty[jfxsp.Color] =
    delegate.specularColorProperty
  def specularColor_=(v: jfxsp.Color)
    specularColor() = v

  /** The specular map of this `PhongMaterial`. */
  def specularMap: ObjectProperty[jfxsi.Image] = delegate.specularMapProperty
  def specularMap_=(v: jfxsi.Image)
    specularMap() = v

  /** Defines the specular power of this Material. */
  def specularPower: DoubleProperty = delegate.specularPowerProperty
  def specularPower_=(v: Double)
    specularPower() = v
