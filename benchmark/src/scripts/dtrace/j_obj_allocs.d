#!/usr/sbin/dtrace -Zs

/**
 *  Provides a "ballpark estimate" of allocations while executing a JMH test.
 *      It should be emphasized that enabling ExtendedDTraceProbes prvodes a massive 
 *      performance hit, on the order of 1 magnitude, and actually running this script 
 *      further degrades performances by 2 and a half orders of magnitude.
 *
 *  This script will only track allocations while the given method is being executed. This
 *      does not prevent JMH allocations from making it in, but it does dramatically reduce
 *      their number and gives a very solid initial estimate.
 *
 *  NOTE: This script IS NOT THREAD SAFE.
 *  NOTE: Bash will eat any "$" contained within a Scala class name, so be sure
 *      to escape it out.
 *  Exampe: sudo ./j_obj_allocs.d roc/postgresql/transport/PacketDecoderImplicits\$\$anonfun\$readErrorNoticePacket\$1.apply 
 */

#pragma D option quiet
#pragma D option bufsize=32m

dtrace:::BEGIN
{
  class_name = $$1;
  top = 20; 
  self->tracing = 1;
  printf("Tracing on %s ... Hit Ctrl-C to end.\n", class_name);
}

hotspot*:::method-entry,
hotspot*:::method-return
{
  this->class = strjoin(strjoin(copyinstr(arg1), "."), copyinstr(arg3))
}

hotspot*:::method-entry
/ this->class == class_name /
{
  self->tracing = 0 ;
}

hotspot*:::method-return
/ this->class == class_name /
{
  self->tracing = 1;
  @method_count = count();
}

hotspot*:::object-alloc
/ self->tracing == 0 /
{
  this->class = copyinstr(arg1);
  @alloc_count[this->class] = count();
  @alloc_size[this->class] = sum(arg3);
  @total_allocs = count();
  this->alloc_size = arg3;
  @total_size = sum(this->alloc_size);
}

dtrace:::END
{
  normalize(@total_size, 1048576);
  printf("\nSizes in megabytes (mb),\n");
  printf("   %-95s %-8s %-14s %-14s\n", "METHOD", "EXECUTED", "TOTAL ALLOCS", "TOTAL SIZE");
  printf("   %-96s", class_name);
  printa("%-9@d", @method_count);
  printa("%-15@d", @total_allocs);
  printa("%-15@u\n", @total_size);

  trunc(@alloc_count, top);
  trunc(@alloc_size, top);
  normalize(@alloc_size, 1024);
  printf("\nTop %d allocations by class,\n", top);
  printf("%65s %-15s\n", "CLASS", "COUNT");
  printa("%65s %-15@d\n", @alloc_count);
  printf("\nTop %d allocations by class (size), in kilobytes,\n", top);
  printf("%65s %-15s\n", "CLASS", "KILOBYTES");
  printa("%65s %-15@u\n", @alloc_size);
}
