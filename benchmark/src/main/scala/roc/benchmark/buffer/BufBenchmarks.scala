package roc
package postgresql
package transport

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

@Fork(2)
@State(Scope.Thread)
class BufBenchmarks extends BufferTestData {

  var buf: Buf = Buf(0)

  @Setup(Level.Invocation)
  def instantiateBuf(): Unit = {
    buf = Buf(4)
    ()
  }

  @TearDown(Level.Invocation)
  def releaseBuf(): Unit = buf.release()

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def measureBufAllocationAndIntWrite(): Unit = {
    val u = buf.writeInt(TestInt)
    u
  }
}

@Fork(2)
@State(Scope.Thread)
class BufWriteCStringBenchmark extends BufferTestData {

  var buf: Buf = Buf(0)

  @Setup(Level.Invocation)
  def allocateBuf(): Unit = {
    buf = Buf(TestStringByteLength)
    ()
  }

  @TearDown(Level.Invocation)
  def releaseBuf(): Unit = buf.release()

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def measureBufCStringWrite(): Unit = {
    val u = buf.writeCStyleString(TestString)
    u
  }
}
