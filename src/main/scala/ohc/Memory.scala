package ohc

import java.lang.invoke.MethodHandles
import java.nio.{ Buffer, ByteBuffer }

/**
 * Abstraction over an arbitrary chunk of memory.
 */
trait Memory extends Any {
  def size: Long
  
  def getByte(pointer: Long): Byte
  def getShort(pointer: Long): Short
  def getInt(pointer: Long): Int
  def getLong(pointer: Long): Long
  def getFloat(pointer: Long): Float
  def getDouble(pointer: Long): Double
  def getBytes(pointer: Long, bytes: ByteBuffer)
  def getBytes(pointer: Long, bytes: Array[Byte])

  def setByte(pointer: Long, value: Byte): Unit
  def setShort(pointer: Long, value: Short): Unit
  def setInt(pointer: Long, value: Int): Unit
  def setLong(pointer: Long, value: Long): Unit
  def setFloat(pointer: Long, value: Float): Unit
  def setDouble(pointer: Long, value: Double): Unit
  def setBytes(pointer: Long, bytes: ByteBuffer): Unit
  def setBytes(pointer: Long, bytes: Array[Byte]): Unit

  /**
   * sets to 0 the specified memory.
   */
  def clear(offset: Long, bytes: Long): Unit

  /**
   * General purpose copy method useful for moving big chunks of bytes in a possibly optimized way.
   * This method is intended for copying bytes to `other` memory, starting at `from`, copying `length` bytes to `dest`.
   * @param other The memory bank to where the bytes are going to be copied.
   * @param from Starting pointer in this memory bank.
   * @param length Amount of bytes to copy.
   * @param dest starting pointer in the destination memory bank.
   */
  def copyTo(other: Memory, from: Long, length: Long, dest: Long)
}

/**
 * Implementation of Memory using a ByteBuffer.
 */
final class ByteBufferMemory(val byteBuffer: ByteBuffer) extends AnyVal with Memory {
  @inline def size: Long = byteBuffer.capacity

  @inline def getByte(pointer) = byteBuffer.get(pointer.toInt)
  @inline def getShort(pointer) = byteBuffer.getShort(pointer.toInt)
  @inline def getInt(pointer) = byteBuffer.getInt(pointer.toInt)
  @inline def getLong(pointer) = byteBuffer.getLong(pointer.toInt)
  @inline def getFloat(pointer) = byteBuffer.getFloat(pointer.toInt)
  @inline def getDouble(pointer) = byteBuffer.getDouble(pointer.toInt)
  @inline def getBytes(pointer, bytes: Array[Byte]) = byteBuffer.get(bytes, pointer.toInt, bytes.length)
  @inline def getBytes(pointer, bytes: ByteBuffer) = {
    byteBuffer.position(pointer.toInt).limit(bytes.remaining)
    bytes.put(byteBuffer)
    byteBuffer.reset()
  }

  @inline def setByte(pointer, value) = byteBuffer.put(pointer.toInt, value)
  @inline def setShort(pointer, value) = byteBuffer.putShort(pointer.toInt, value)
  @inline def setInt(pointer, value) = byteBuffer.putInt(pointer.toInt, value)
  @inline def setLong(pointer, value) = byteBuffer.putLong(pointer.toInt, value)
  @inline def setFloat(pointer, value) = byteBuffer.putFloat(pointer.toInt, value)
  @inline def setDouble(pointer, value) = byteBuffer.putDouble(pointer.toInt, value)
  @inline def setBytes(pointer, bytes: Array[Byte]) = byteBuffer.put(bytes, pointer.toInt, bytes.length)
  @inline def setBytes(pointer, bytes: ByteBuffer) = {
    byteBuffer.position(pointer.toInt)
    byteBuffer.put(bytes)
    byteBuffer.reset()
  }

  @inline def clear(offset, bytes) = {
    var i = -1
    while ({i += 1; i < bytes}) byteBuffer.put(offset.toInt + i, 0.toByte)
  }

  @inline def copyTo(other, from, length, dest): Unit = {
    byteBuffer.position(from.toInt).limit(length.toInt)
    val view = byteBuffer.slice()
    other.setBytes(dest, view)
    byteBuffer.reset()
  }
}

/**
 * Implementation of Memory using a directly mmaped memory.
 */
final class DirectMemory(val size: Long) extends Memory {
  private[this] val unsafe = scala.concurrent.util.Unsafe.instance
  val memAddr = unsafe.allocateMemory(size)

  //helper direct byte buffer for bulk operations
  private[this] val directBuffer = new ByteBufferMemory(DirectMemory.forMemory(memAddr, size.toInt))

  @inline def getByte(pointer) = unsafe.getByte(memAddr + pointer)
  @inline def getShort(pointer) = unsafe.getShort(memAddr + pointer)
  @inline def getInt(pointer) = unsafe.getInt(memAddr + pointer)
  @inline def getLong(pointer) = unsafe.getLong(memAddr + pointer)
  @inline def getFloat(pointer) = unsafe.getFloat(memAddr + pointer)
  @inline def getDouble(pointer) = unsafe.getDouble(memAddr + pointer)
  @inline def getBytes(pointer, bytes: Array[Byte]) = directBuffer.getBytes(pointer, bytes)
  @inline def getBytes(pointer, bytes: ByteBuffer) = directBuffer.getBytes(pointer, bytes)

  @inline def setByte(pointer, value) = unsafe.putByte(memAddr + pointer, value)
  @inline def setShort(pointer, value) = unsafe.putShort(memAddr + pointer, value)
  @inline def setInt(pointer, value) = unsafe.putInt(memAddr + pointer, value)
  @inline def setLong(pointer, value) = unsafe.putLong(memAddr + pointer, value)
  @inline def setFloat(pointer, value) = unsafe.putFloat(memAddr + pointer, value)
  @inline def setDouble(pointer, value) = unsafe.putDouble(memAddr + pointer, value)
  @inline def setBytes(pointer, bytes: Array[Byte]) = directBuffer.setBytes(pointer, bytes)
  @inline def setBytes(pointer, bytes: ByteBuffer) = directBuffer.setBytes(pointer, bytes)

  @inline def clear(offset, bytes) = unsafe.setMemory(memAddr + offset, bytes, 0)

  @inline def copyTo(other, from, length, dest) = {
    other match {
      case other: DirectMemory => unsafe.copyMemory(memAddr + from, other.memAddr + dest, length)
      case other => directBuffer.copyTo(other, from, length, dest)
    }
  }
}
object DirectMemory {
  private[DirectMemory] val directByteBufferInstantiator = {
    val cons = Class.forName("java.nio.DirectByteBuffer").getDeclaredConstructor(java.lang.Long.TYPE, java.lang.Integer.TYPE, classOf[Object])
    cons.setAccessible(true)
    MethodHandles.lookup.unreflectConstructor(cons)
  }
  private[DirectMemory] def forMemory(address: Long, cap: Int): ByteBuffer = {
    directByteBufferInstantiator.invoke(address: java.lang.Long, cap: java.lang.Integer, null).asInstanceOf[ByteBuffer]
  }
}