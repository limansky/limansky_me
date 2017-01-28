---
title: Firefox ESR and XMonad
tags: XMonad, Gentoo
---

If you are XMonad and Firefox ESR user, just like me, you might know that there
is a problem with full screen videos playback.  A video hangs and you cannot close
it.  All you can do is just to kill Firefox.  It's already fixed in the main line,
but it looks like they [don't want](https://bugzilla.mozilla.org/show_bug.cgi?id=1189622)
to port the patch to ESR version.

Fortunately the patch is not too big and I adopted it for the ESR version. Here is
it:

<!--more-->

```diff
diff --git a/dom/base/nsGlobalWindow.h b/dom/base/nsGlobalWindow.h
--- a/dom/base/nsGlobalWindow.h
+++ b/dom/base/nsGlobalWindow.h
@@ -473,7 +473,7 @@
     FullscreenReason aReason, bool aIsFullscreen,
     mozilla::gfx::VRHMDInfo *aHMD = nullptr) override final;
   virtual void FinishFullscreenChange(bool aIsFullscreen) override final;
-  void SetWidgetFullscreen(FullscreenReason aReason, bool aIsFullscreen,
+  bool SetWidgetFullscreen(FullscreenReason aReason, bool aIsFullscreen,
                            nsIWidget* aWidget, nsIScreen* aScreen);
   bool FullScreen() const;

diff --git a/dom/base/nsGlobalWindow.cpp b/dom/base/nsGlobalWindow.cpp
--- a/dom/base/nsGlobalWindow.cpp.orig
+++ b/dom/base/nsGlobalWindow.cpp	
@@ -5905,8 +5905,12 @@
       mWindow->mFullScreen = mFullscreen;
     }
     // Toggle the fullscreen state on the widget
-    mWindow->SetWidgetFullscreen(nsPIDOMWindow::eForFullscreenAPI,
-                                 mFullscreen, mWidget, mScreen);
+    if (!mWindow->SetWidgetFullscreen(nsPIDOMWindow::eForFullscreenAPI,
+                                      mFullscreen, mWidget, mScreen)) {
+      // Fail to setup the widget, call FinishFullscreenChange to
+      // complete fullscreen change directly.
+      mWindow->FinishFullscreenChange(mFullscreen);
+    }
     // Set observer for the next content paint.
     nsCOMPtr<nsIObserver> observer = new Observer(this);
     nsCOMPtr<nsIObserverService> obs = mozilla::services::GetObserverService();
@@ -5985,14 +5989,14 @@
   }
   nsCOMPtr<nsIScreen> screen = aHMD ? aHMD->GetScreen() : nullptr;
   if (!performTransition) {
-    aWindow->SetWidgetFullscreen(aReason, aFullscreen, widget, screen);
+    return aWindow->SetWidgetFullscreen(aReason, aFullscreen, widget, screen);
   } else {
     nsCOMPtr<nsIRunnable> task =
       new FullscreenTransitionTask(duration, aWindow, aFullscreen,
                                    widget, screen, transitionData);
     task->Run();
+    return true;
   }
-  return true;
 }
 
 nsresult
@@ -6096,7 +6100,7 @@
   return NS_OK;
 }
 
-void
+bool
 nsGlobalWindow::SetWidgetFullscreen(FullscreenReason aReason, bool aIsFullscreen,
                                     nsIWidget* aWidget, nsIScreen* aScreen)
 {
@@ -6108,13 +6112,12 @@
   if (nsCOMPtr<nsIPresShell> presShell = mDocShell->GetPresShell()) {
     presShell->SetIsInFullscreenChange(true);
   }
-  if (aReason == nsPIDOMWindow::eForFullscreenMode) {
+  nsresult rv = aReason == nsPIDOMWindow::eForFullscreenMode ?
     // If we enter fullscreen for fullscreen mode, we want
     // the native system behavior.
-    aWidget->MakeFullScreenWithNativeTransition(aIsFullscreen, aScreen);
-  } else {
+    aWidget->MakeFullScreenWithNativeTransition(aIsFullscreen, aScreen) :
     aWidget->MakeFullScreen(aIsFullscreen, aScreen);
-  }
+  return NS_SUCCEEDED(rv);
 }
 
 /* virtual */ void
diff --git a/widget/nsIWidget.h b/widget/nsIWidget.h
--- a/widget/nsIWidget.h
+++ b/widget/nsIWidget.h
@@ -1137,16 +1137,20 @@ class nsIWidget : public nsISupports {
                                              nsIRunnable* aCallback) = 0;
 
     /**
      * Put the toplevel window into or out of fullscreen mode.
      * If aTargetScreen is given, attempt to go fullscreen on that screen,
      * if possible.  (If not, it behaves as if aTargetScreen is null.)
      * If !aFullScreen, aTargetScreen is ignored.
      * aTargetScreen support is currently only implemented on Windows.
+     *
+     * @return NS_OK if the widget is setup properly for fullscreen and
+     * FullscreenChanged callback has been or will be called. If other
+     * value is returned, the caller should continue the change itself.
      */
     NS_IMETHOD MakeFullScreen(bool aFullScreen, nsIScreen* aTargetScreen = nullptr) = 0;
 
     /**
      * Same as MakeFullScreen, except that, on systems which natively
      * support fullscreen transition, calling this method explicitly
      * requests that behavior.
      * It is currently only supported on OS X 10.7+.
diff --git a/widget/gtk/mozgtk/mozgtk.c b/widget/gtk/mozgtk/mozgtk.c
--- a/widget/gtk/mozgtk/mozgtk.c
+++ b/widget/gtk/mozgtk/mozgtk.c
@@ -122,16 +122,17 @@ STUB(gdk_x11_display_get_user_time)
 STUB(gdk_x11_display_get_xdisplay)
 STUB(gdk_x11_get_default_root_xwindow)
 STUB(gdk_x11_get_default_xdisplay)
 STUB(gdk_x11_get_server_time)
 STUB(gdk_x11_get_xatom_by_name)
 STUB(gdk_x11_get_xatom_by_name_for_display)
 STUB(gdk_x11_lookup_xdisplay)
 STUB(gdk_x11_screen_get_xscreen)
+STUB(gdk_x11_screen_supports_net_wm_hint)
 STUB(gdk_x11_visual_get_xvisual)
 STUB(gdk_x11_window_foreign_new_for_display)
 STUB(gdk_x11_window_lookup_for_display)
 STUB(gdk_x11_window_set_user_time)
 STUB(gdk_x11_xatom_to_atom)
 STUB(gtk_accel_label_new)
 STUB(gtk_alignment_get_type)
 STUB(gtk_alignment_new)
diff --git a/widget/gtk/nsWindow.cpp b/widget/gtk/nsWindow.cpp
--- a/widget/gtk/nsWindow.cpp
+++ b/widget/gtk/nsWindow.cpp
@@ -4972,22 +4972,39 @@ nsWindow::PerformFullscreenTransition(Fu
     auto transitionData = new FullscreenTransitionData(aStage, aDuration,
                                                        aCallback, data);
     g_timeout_add_full(G_PRIORITY_HIGH,
                        FullscreenTransitionData::sInterval,
                        FullscreenTransitionData::TimeoutCallback,
                        transitionData, nullptr);
 }
 
+static bool
+IsFullscreenSupported(GtkWidget* aShell)
+{
+#ifdef MOZ_X11
+    GdkScreen* screen = gtk_widget_get_screen(aShell);
+    GdkAtom atom = gdk_atom_intern("_NET_WM_STATE_FULLSCREEN", FALSE);
+    if (!gdk_x11_screen_supports_net_wm_hint(screen, atom)) {
+        return false;
+    }
+#endif
+    return true;
+}
+
 NS_IMETHODIMP
 nsWindow::MakeFullScreen(bool aFullScreen, nsIScreen* aTargetScreen)
 {
     LOG(("nsWindow::MakeFullScreen [%p] aFullScreen %d\n",
          (void *)this, aFullScreen));
 
+    if (!IsFullscreenSupported(mShell)) {
+        return NS_ERROR_NOT_AVAILABLE;
+    }
+
     if (aFullScreen) {
         if (mSizeMode != nsSizeMode_Fullscreen)
             mLastSizeMode = mSizeMode;
 
         mSizeMode = nsSizeMode_Fullscreen;
         gtk_window_fullscreen(GTK_WINDOW(mShell));
     }
     else {

```

If you use Gentoo you can just put the patch in
`/etc/portage/patches/www-client/firefox/firefox-45.7.0/patch.diff` file and
reemerge Firefox.
