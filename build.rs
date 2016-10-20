// [build] cargo test -vv

use std::env;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

// TODO: Should probably switch to https://github.com/dlo9/vtd-xml-c.

fn cmd (dir: &Path, cmd: &str) {
  println! ("<cmd cmd=\"{}\" dir=\"{}\">", cmd, dir.to_string_lossy());
  let status = Command::new ("sh") .arg ("-c") .arg (cmd) .current_dir (dir) .stdout (Stdio::inherit()) .stderr (Stdio::inherit())
    .status() .expect ("!sh");
  if !status.success() {panic! ("cmd] exit code {:?}", status.code())}
  println! ("</cmd>");}

fn main() {
  let out_dir = env::var ("OUT_DIR") .expect ("!OUT_DIR");
  let out_dir = Path::new (&out_dir);
  if !out_dir.join ("ximpleware-2.12-c.zip") .exists() {
    cmd (out_dir, "wget \
      'http://downloads.sourceforge.net/project/vtd-xml/vtd-xml/ximpleware_2.12/VTD-XML%20Standard%20Edition/ximpleware-2.12-c.zip'")}

  let unpacked = out_dir.join ("ximpleware-2.12-c");
  if !unpacked.exists() {
    // pacman -S msys/unzip
    cmd (out_dir, "unzip ximpleware-2.12-c.zip");}

  let sources = unpacked.join ("vtd-xml");
  assert! (sources.exists());

  let target = env::var ("TARGET") .expect ("!TARGET");
  println! ("target: {}", target);

  // TODO: See if we're in a Release cargo build and use -O3 there.
  cmd (&sources, "perl -i.tmp -pe 's/CFLAGS= -c -O3/CFLAGS= -c -ggdb -fPIC -Og/' makefile");

  if target.ends_with ("-windows-gnu") {
    cmd (&sources, "perl -i.tmp -pe 's/-lm//' makefile");}

  cmd (&sources, "make -j 4");

  { let mut shims = std::fs::File::create (sources.join ("shims.c")) .expect ("!create");
    shims.write (b"
      #include \"customTypes.h\"
      _thread struct exception_context the_exception_context[1];

      int vtd_try_catch_shim (void (*rust_cb) (void*, void*), void* closure_pp, void* panic_p, exception* ex_out) {
        exception e; int exception_raised = 0;
        Try {rust_cb (closure_pp, panic_p);} Catch (e) {exception_raised = 1; if (ex_out) *ex_out = e;}
        return exception_raised;}

      #include \"vtdNav.h\"
      VTDNav* cloneNav_shim (VTDNav *vn) {return vn->__cloneNav(vn);}
      VTDNav* duplicateNav_shim (VTDNav *vn) {return vn->__duplicateNav (vn);}
      void recoverNode_shim (VTDNav *vn, int index) {vn->__recoverNode (vn, index);}
      void freeVTDNav_shim (VTDNav *vn) {vn->__freeVTDNav(vn);};
      Boolean toElement_shim (VTDNav *vn, navDir direction) {return vn->__toElement (vn, direction);}
      Boolean toElement2_shim (VTDNav *vn, navDir direction, UCSChar *en) {return vn->__toElement2 (vn, direction, en);}
      int getCurrentIndex_shim (VTDNav *vn) {return getCurrentIndex (vn);}

      // We need the shims because the MSYS2 version of iconv uses the 'lib' prefix, e.g. 'libiconv_open' instead of 'iconv_open'.
      #include <iconv.h>
      iconv_t iconv_open_shim (const char *tocode, const char *fromcode) {return iconv_open (tocode, fromcode);}
      size_t iconv_shim (iconv_t cd, char** inbuf, size_t* inbytesleft, char** outbuf, size_t* outbytesleft) {
        return iconv (cd, inbuf, inbytesleft, outbuf, outbytesleft);}
      int iconv_close_shim (iconv_t cd) {return iconv_close (cd);}
      #include <errno.h>
      int is_errno_einval() {return errno == EINVAL;}
      int is_errno_e2big() {return errno == E2BIG;}
    ") .expect ("!write"); }
  cmd (&sources, "gcc -ggdb -fPIC \
    -Og -fomit-frame-pointer -fforce-addr -march=core2 \
    -c shims.c -o shims.o");

  // NB: We should always repack the library in order to include any shim updates.
  let lib = sources.join ("libvtdxml.a");
  let _ = std::fs::remove_file (&lib);
  cmd (&sources, "ar rcs libvtdxml.a shims.o \
    arrayList.o fastIntBuffer.o fastLongBuffer.o contextBuffer.o vtdNav.o vtdGen.o autoPilot.o XMLChar.o helper.o lex.yy.o l8.tab.o \
    literalExpr.o numberExpr.o pathExpr.o filterExpr.o binaryExpr.o unaryExpr.o funcExpr.o locationPathExpr.o intHash.o unionExpr.o \
    decoder.o XMLModifier.o nodeRecorder.o indexHandler.o bookMark.o elementFragmentNs.o transcoder.o textIter.o variableExpr.o cachedExpr.o");

  println! ("cargo:rustc-link-lib=static=vtdxml");
  if target.ends_with ("-windows-gnu") {
    println! ("cargo:rustc-link-lib=iconv");}
  println! ("cargo:rustc-link-search=native={}", sources.to_string_lossy());}
