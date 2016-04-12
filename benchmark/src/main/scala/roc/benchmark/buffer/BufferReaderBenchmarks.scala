package roc
package postgresql
package transport

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

@Fork(2)
@State(Scope.Thread)
class BufferReaderReadIntBenchmarks extends BufferTestData {

  var br = BufferReader(Array.empty[Byte])

  @Setup(Level.Invocation)
  def allocateBuffer(): Unit = {
    val bw = BufferWriter(new Array[Byte](4))
    bw.writeInt(TestInt)
    br = BufferReader(bw.toBytes)
    ()
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def measureBufferReaderReadInt(): Int = {
    br.readInt
  }
}
