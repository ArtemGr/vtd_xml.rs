// [build] cargo test -vv

use std::env;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

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

  cmd (&sources, "perl -i.tmp -pe 's/CFLAGS= -c/CFLAGS= -c -fpic/' makefile");

  if target.ends_with ("-windows-gnu") {
    cmd (&sources, "perl -i.tmp -pe 's/-lm//' makefile");}

  cmd (&sources, "make -j 4");

  { let mut shims = std::fs::File::create (sources.join ("shims.c")) .expect ("!create");
    shims.write (b"
      #include \"customTypes.h\"
      _thread struct exception_context the_exception_context[1];
      #include \"vtdNav.h\"
      void freeVTDNav_shim (VTDNav *vn) {vn->__freeVTDNav(vn);};
    ") .expect ("!write"); }
  cmd (&sources, "gcc \
    -O3 -fomit-frame-pointer -fforce-addr -frerun-cse-after-loop -fexpensive-optimizations -fregmove -frerun-loop-opt -march=core2 \
    -c shims.c -o shims.o");

  let lib = sources.join ("libvtdxml.a");
  if !lib.exists() {
    cmd (&sources, "ar rcs libvtdxml.a shims.o \
      arrayList.o fastIntBuffer.o fastLongBuffer.o contextBuffer.o vtdNav.o vtdGen.o autoPilot.o XMLChar.o helper.o lex.yy.o l8.tab.o \
      literalExpr.o numberExpr.o pathExpr.o filterExpr.o binaryExpr.o unaryExpr.o funcExpr.o locationPathExpr.o intHash.o unionExpr.o \
      decoder.o XMLModifier.o nodeRecorder.o indexHandler.o bookMark.o elementFragmentNs.o transcoder.o textIter.o variableExpr.o cachedExpr.o");}

  println! ("cargo:rustc-link-lib=static=vtdxml");
  println! ("cargo:rustc-link-search=native={}", sources.to_string_lossy());}
