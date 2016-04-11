package roc
package postgresql
package transport

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

@Fork(1)
@State(Scope.Thread)
class BufIntReaderBenchmark extends BufferTestData {

  var buf = Buf(0)

  @Setup(Level.Invocation)
  def instantiateBuf(): Unit = {
    buf = Buf(4)
    buf.writeInt(TestInt)
    ()
  }

  @TearDown(Level.Invocation)
  def releaseBuf(): Unit = buf.release()

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def measureBufReadInt(): Int = {
    buf.readInt
  }

}
