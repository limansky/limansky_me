---
title: Fixing nvidia-drivers-340.76 build with kernel 4.0
tags: Gentoo, NVidia
---

After upgrade to gentoo-sources-4.0.5 I've found what I cannot install drivers
for my good old GTS-250.  This card is pretty old, so it supports only in 340.x
branch of NVidia drivers.  Unfortunately NVidia is not really fast in fixing of
such kind of problems (https://devtalk.nvidia.com/default/topic/827836/-bug-nvidia-340-76-build-on-linux-64-fails-after-upgrade-of-kernel-v3-19-4-gt-v4-0-0/).
They just provide a link to the patch in Ubuntu.  But, AFAIK Gentoo maintainers
doesn't take such patches as well (I suppose it is break older kernels compatibility).

The cool thing I've never used before called epatch_user allows you to add custom
patches to any ebuild supporting this feature.  And actually this is the way,
proposed to solve such issues.  So, all you need to do is to create folder
`/etc/portage/patches/x11-drivers/nvidia-drivers-340.76` and place there following
patch:

```diff
--- kernel/nv-pat.c.orig    2015-08-17 21:46:15.541979210 +0300
+++ kernel/nv-pat.c 2015-08-17 21:47:17.180978210 +0300
@@ -35,8 +35,8 @@
     unsigned long cr0 = read_cr0();
     write_cr0(((cr0 & (0xdfffffff)) | 0x40000000));
     wbinvd();
-    *cr4 = read_cr4();
-    if (*cr4 & 0x80) write_cr4(*cr4 & ~0x80);
+    *cr4 = __read_cr4();
+    if (*cr4 & 0x80) __write_cr4(*cr4 & ~0x80);
     __flush_tlb();
 }
 
@@ -46,7 +46,7 @@
     wbinvd();
     __flush_tlb();
     write_cr0((cr0 & 0x9fffffff));
-    if (cr4 & 0x80) write_cr4(cr4);
+    if (cr4 & 0x80) __write_cr4(cr4);
 }
 
 static int nv_determine_pat_mode(void)
```
