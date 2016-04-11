package roc.postgresql.transport

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

@Fork(2)
@State(Scope.Thread)
class BufferWriterBenchmarks extends BufferTestData {

  var buf = BufferWriter(Array.empty[Byte])

  @Setup(Level.Invocation)
  def allocateBuffer(): Unit = {
    buf = BufferWriter(new Array[Byte](4))
    ()
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def measureBufAllocationAndIntWrite(): BufferWriter = {
    buf.writeInt(TestInt)
  }
}

@Fork(2)
@State(Scope.Thread)
class BufferWriterCStringBenchmark extends BufferTestData {
  var buf = BufferWriter(Array.empty[Byte])

  @Setup(Level.Invocation)
  def allocateBuffer(): Unit = {
    buf = BufferWriter(new Array[Byte](TestStringByteLength))
    ()
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def measureCStyleStringWrite(): BufferWriter = {
    buf.writeNullTerminatedString(TestString)
  }
}
