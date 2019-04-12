/**
  * Copyright 2014 Dropbox, Inc.
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *    http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */

package djinni

import djinni.ast.TypeDef
import scala.collection.immutable

package object meta {

case class MExpr(base: Meta, args: Seq[MExpr])

abstract sealed class Meta
{
  val numParams: Int
}

case class MParam(name: String) extends Meta { val numParams = 0 }
case class MDef(name: String, override val numParams: Int, defType: DefType, body: TypeDef) extends Meta
case class MExtern(name: String, override val numParams: Int, defType: DefType, body: TypeDef, cpp: MExtern.Cpp, objc: MExtern.Objc, objcpp: MExtern.Objcpp, java: MExtern.Java, jni: MExtern.Jni) extends Meta
object MExtern {
  // These hold the information marshals need to interface with existing types correctly
  // All include paths are complete including quotation marks "a/b/c" or angle brackets <a/b/c>.
  // All typenames are fully qualified in their respective language.
  // TODO: names of enum values and record fields as written in code for use in constants (do not use IdentStyle)
  case class Cpp(
    typename: String,
    header: String,
    byValue: Boolean // Whether to pass struct by value in C++ (e.g. std::chrono::duration). Only used for "record" types.
  )
  case class Objc(
    typename: String,
    header: String,
    boxed: String, // Fully qualified Objective-C typename, must be an object. Only used for "record" types.
    pointer: Boolean, // True to construct pointer types and make it eligible for "nonnull" qualifier. Only used for "record" types.
    hash: String // A well-formed expression to get the hash value. Must be a format string with a single "%s" placeholder. Only used for "record" types with "eq" deriving when needed.
  )
  case class Objcpp(
    translator: String, // C++ typename containing toCpp/fromCpp methods
    header: String // Where to find the translator class
  )
  case class Java(
    typename: String,
    boxed: String, // Java typename used if boxing is required, must be an object.
    reference: Boolean, // True if the unboxed type is an object reference and qualifies for any kind of "nonnull" annotation in Java. Only used for "record" types.
    generic: Boolean, // Set to false to exclude type arguments from the Java class. This is should be true by default. Useful if template arguments are only used in C++.
    hash: String, // A well-formed expression to get the hash value. Must be a format string with a single "%s" placeholder. Only used for "record" types types with "eq" deriving when needed.
    writeToParcel: String, // A well-formed expression to write value into android.os.Parcel. Must be a format string with a single "%s" placeholder. Only used for "record" types types
    readFromParcel: String // A well-formed expression to read value from android.os.Parcel. Must be a format string with a single "%s" placeholder. Only used for "record" types types
  )
  case class Jni(
    translator: String, // C++ typename containing toCpp/fromCpp methods
    header: String, // Where to find the translator class
    typename: String, // The JNI type to use (e.g. jobject, jstring)
    typeSignature: String // The mangled Java type signature (e.g. "Ljava/lang/String;")
  )
}

abstract sealed class MOpaque extends Meta { val idlName: String }

abstract sealed class DefType
case object DEnum extends DefType
case object DInterface extends DefType
case object DRecord extends DefType

case class MPrimitive(_idlName: String, jName: String, jniName: String, cName: String, jBoxed: String, jSig: String, objcName: String, objcBoxed: String, swiftName: String, nodeJSName: String, jsName: String) extends MOpaque { val numParams = 0; val idlName = _idlName }
case object MString extends MOpaque { val numParams = 0; val idlName = "string" }
case object MDate extends MOpaque { val numParams = 0; val idlName = "date" }
case object MBinary extends MOpaque { val numParams = 0; val idlName = "binary" }
case object MOptional extends MOpaque { val numParams = 1; val idlName = "optional" }
case object MList extends MOpaque { val numParams = 1; val idlName = "list" }
case object MSet extends MOpaque { val numParams = 1; val idlName = "set" }
case object MMap extends MOpaque { val numParams = 2; val idlName = "map" }
case object MCallback1 extends MOpaque { val numParams = 1; val idlName = "callback1" }
case object MCallback2 extends MOpaque { val numParams = 2; val idlName = "callback2" }
case object MCallback3 extends MOpaque { val numParams = 3; val idlName = "callback3" }
case object MCallback4 extends MOpaque { val numParams = 4; val idlName = "callback4" }
case object MCallback5 extends MOpaque { val numParams = 5; val idlName = "callback5" }
case object MCallback6 extends MOpaque { val numParams = 6; val idlName = "callback6" }
case object MCallback7 extends MOpaque { val numParams = 7; val idlName = "callback7" }
case object MCallback8 extends MOpaque { val numParams = 8; val idlName = "callback8" }
case object MCallback9 extends MOpaque { val numParams = 9; val idlName = "callback9" }
case object MCallback10 extends MOpaque { val numParams = 10; val idlName = "callback10" }
case object MCallback11 extends MOpaque { val numParams = 11; val idlName = "callback11" }
case object MCallback12 extends MOpaque { val numParams = 12; val idlName = "callback12" }
case object MCallback13 extends MOpaque { val numParams = 13; val idlName = "callback13" }
case object MCallback14 extends MOpaque { val numParams = 14; val idlName = "callback14" }
case object MCallback15 extends MOpaque { val numParams = 15; val idlName = "callback15" }

val defaults: Map[String,MOpaque] = immutable.HashMap(
  ("i8",   MPrimitive("i8",   "byte",    "jbyte",    "int8_t",  "Byte",    "B", "int8_t",  "NSNumber", "Int8", "Int32", "number")),
  ("i16",  MPrimitive("i16",  "short",   "jshort",   "int16_t", "Short",   "S", "int16_t", "NSNumber", "Int16", "Int32", "number")),
  ("i32",  MPrimitive("i32",  "int",     "jint",     "int32_t", "Integer", "I", "int32_t", "NSNumber", "Int32", "Int32", "number")),
  ("i64",  MPrimitive("i64",  "long",    "jlong",    "int64_t", "Long",    "J", "int64_t", "NSNumber", "Int64", "Number", "number")),
  ("f32",  MPrimitive("f32",  "float",   "jfloat",   "float",   "Float",   "F", "float",   "NSNumber", "Float32", "Number", "number")),
  ("f64",  MPrimitive("f64",  "double",  "jdouble",  "double",  "Double",  "D", "double",  "NSNumber", "Float64", "Number", "number")),
  ("bool", MPrimitive("bool", "boolean", "jboolean", "bool",    "Boolean", "Z", "BOOL",    "NSNumber", "Bool", "Boolean", "boolean")),
  ("string", MString),
  ("binary", MBinary),
  ("optional", MOptional),
  ("date", MDate),
  ("list", MList),
  ("set", MSet),
  ("map", MMap),
  ("callback1", MCallback1)
  ("callback2", MCallback2)
  ("callback3", MCallback3)
  ("callback4", MCallback4)
  ("callback5", MCallback5)
  ("callback6", MCallback6)
  ("callback7", MCallback7)
  ("callback8", MCallback8)
  ("callback9", MCallback9)
  ("callback10", MCallback10)
  ("callback11", MCallback11)
  ("callback12", MCallback12)
  ("callback13", MCallback13)
  ("callback14", MCallback14)
  ("callback15", MCallback15)
)

def isInterface(ty: MExpr): Boolean = {
  ty.base match {
    case d: MDef => d.defType == DInterface
    case _ => false
  }
}

def isRecord(ty: MExpr): Boolean = {
  ty.base match {
  case d: MDef => d.defType == DRecord
  case _ => false
  }
}

def isEnum(ty: MExpr): Boolean = {
  ty.base match {
    case d: MDef => d.defType == DEnum
    case _ => false
  }
}

def isOptionalInterface(ty: MExpr): Boolean = {
  ty.base == MOptional && ty.args.length == 1 && isInterface(ty.args.head)
}
}
