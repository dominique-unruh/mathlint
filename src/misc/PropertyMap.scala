package misc

import scala.collection.generic.Subtractable
import scala.collection.mutable.MapBuilder
import scala.collection.{GenIterable, GenMap, GenMapLike, GenSet, IterableLike, Map, mutable}
import scala.language.existentials

trait Property[T] {
  def unapply(map: PropertyMap): Some[T] = Some(map(this))
  val name : String
  val default : T
}

class PropertyMap private (private val map : Map[Property[_],Any]) 
  extends IterableLike[(Property[T], T) forSome {type T}, PropertyMap]
    with Iterable[(Property[T], T) forSome {type T}]
    with Subtractable[Property[_], PropertyMap] {
  def +[A](kv: (Property[A], A)): PropertyMap = new PropertyMap(map + kv)

  def empty: PropertyMap = new PropertyMap(Map.empty)

  def get[T](key: Property[T]): Option[T] = map.get(key).asInstanceOf[Option[T]]
//  def getOrElse[T](key: Property[T], default: => T): Option[T] = map.getOrElse(key,default).asInstanceOf[Option[T]]

  override def iterator: Iterator[(Property[T], T) forSome {type T}] =
    map.iterator.asInstanceOf[Iterator[(Property[T], T) forSome {type T}]]

  def -(key: Property[_]): PropertyMap = new PropertyMap(map - key)

  override protected[this] def newBuilder: mutable.Builder[(Property[T], T) forSome {type T}, PropertyMap] =
    new mutable.Builder[(Property[T], T) forSome {type T}, PropertyMap] {
      private val builder = new mutable.MapBuilder[Property[_], Any, Map[Property[_],Any]](Map.empty)
      override def +=(elem: (Property[T], T) forSome {type T}): this.type = {
        builder += elem
        this
      }

      override def clear(): Unit = builder.clear()

      override def result(): PropertyMap = new PropertyMap(builder.result)
    }

  def apply[T](v1: Property[T]): T = map.getOrElse(v1,v1.default).asInstanceOf[T]
}
