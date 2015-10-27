package ohc

import java.nio.ByteBuffer

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

  def setByte(pointer: Long, value: Byte): Unit
  def setShort(pointer: Long, value: Short): Unit
  def setInt(pointer: Long, value: Int): Unit
  def setLong(pointer: Long, value: Long): Unit
  def setFloat(pointer: Long, value: Float): Unit
  def setDouble(pointer: Long, value: Double): Unit

  /**
   * sets to 0 the specified memory.
   */
  def clear(offset: Long, bytes: Long): Unit
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

  @inline def setByte(pointer, value) = byteBuffer.put(pointer.toInt, value)
  @inline def setShort(pointer, value) = byteBuffer.putShort(pointer.toInt, value)
  @inline def setInt(pointer, value) = byteBuffer.putInt(pointer.toInt, value)
  @inline def setLong(pointer, value) = byteBuffer.putLong(pointer.toInt, value)
  @inline def setFloat(pointer, value) = byteBuffer.putFloat(pointer.toInt, value)
  @inline def setDouble(pointer, value) = byteBuffer.putDouble(pointer.toInt, value)

  @inline def clear(offset, bytes) = {
    var i = -1
    while ({i += 1; i < bytes}) byteBuffer.put(offset.toInt + i, 0.toByte)
  }
}

/**
 * Implementation of Memory using a directly mmaped memory.
 */
final class DirectMemory(val size: Long) extends Memory {
  private[this] val unsafe = scala.concurrent.util.Unsafe.instance
  private[this] val memAddr = unsafe.allocateMemory(size)

  @inline def getByte(pointer) = unsafe.getByte(memAddr + pointer)
  @inline def getShort(pointer) = unsafe.getShort(memAddr + pointer)
  @inline def getInt(pointer) = unsafe.getInt(memAddr + pointer)
  @inline def getLong(pointer) = unsafe.getLong(memAddr + pointer)
  @inline def getFloat(pointer) = unsafe.getFloat(memAddr + pointer)
  @inline def getDouble(pointer) = unsafe.getDouble(memAddr + pointer)

  @inline def setByte(pointer, value) = unsafe.putByte(memAddr + pointer, value)
  @inline def setShort(pointer, value) = unsafe.putShort(memAddr + pointer, value)
  @inline def setInt(pointer, value) = unsafe.putInt(memAddr + pointer, value)
  @inline def setLong(pointer, value) = unsafe.putLong(memAddr + pointer, value)
  @inline def setFloat(pointer, value) = unsafe.putFloat(memAddr + pointer, value)
  @inline def setDouble(pointer, value) = unsafe.putDouble(memAddr + pointer, value)

  @inline def clear(offset, bytes) = unsafe.setMemory(memAddr + offset, bytes, 0)
}