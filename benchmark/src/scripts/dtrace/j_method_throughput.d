#!/usr/sbin/dtrace -Zs

/**
 *  Provides a simple sanity check against a JMH benchmark. It should be strongly
 *      emphasized that enabling ExtendedDTraceProbes prvodes a massive performance 
 *      hit, on the order of 1 magnitude, and actually running this script further
 *      degrades performances by 2 and a half orders of magnitude.
 *
 *  NOTE: Bash will eat any "$" contained within a Scala class name, so be sure
 *      to escape it out.
 *  Exampe: sudo ./j_method_throughput.d roc/postgresql/transport/PacketDecoderImplicits\$\$anonfun\$readErrorNoticePacket\$1.apply 
 */

#pragma D option quiet
#pragma D option bufsize=8m

dtrace:::BEGIN
{
  class_name = $$1;
  printf("Tracing on %s ... Hit Ctrl-C to end.\n", class_name);
}

hotspot*:::method-entry,
hotspot*:::method-return
{
  this->class = strjoin(strjoin(copyinstr(arg1), "."), copyinstr(arg3));
}

hotspot*:::method-entry
/ this->class == class_name /
{
  self->ts = timestamp;
}

hotspot*:::method-return
/ this->class == class_name /
{
  this->elapsed = (timestamp - self->ts) / 1000000;
  @method_count = count();
  @min = min(this->elapsed);
  @max = max(this->elapsed);
  @std = stddev(this->elapsed);
  @avg = avg(this->elapsed);
  @quant = quantize(this->elapsed);
}

dtrace:::END
{
  printf("\nElapsed times in milliseconds (ms),\n");
  printf("   %-95s %-10s %-10s %-10s %-10s %-10s\n", "METHOD", "EXECUTED", "AVG", "MIN", "MAX", "STD DEV");
  printf("   %-96s", class_name);
  printa("%-11@d", @method_count);
  printa("%-11@d", @avg);
  printa("%-11@d", @min);
  printa("%-11@d", @max);
  printa("%-11@d\n", @std);
  printa(@quant);
}
